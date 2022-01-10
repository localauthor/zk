;;; zk.el --- Functions to deal with link-connected notes, with no backend -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Grant Rosson

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This set of functions aims to implement many (but not all) of the features
;; of the package 'Zetteldeft', while circumventing and eliminating any
;; dependency on 'Deft', or any other external packages for that matter. It
;; does not use any backend cache or database, but instead queries a
;; directory of notes directly, treating and utilizing that directory as a
;; sufficient database unto itself.

;; To that end, these functions rely, at the lowest level, on simple calls to
;; 'grep', which returns lists of files, links, and tags to
;; 'completing-read', from which files can be opened and links and tags can
;; be inserted into an open buffer.

;; The primary connector between notes is the simple link, which takes the
;; form of an ID number enclosed in double-brackets, eg, [[202012091130]]. A
;; note's ID number, by default, is a twelve-digit string corresponding to
;; the date and time the note was originally created. For example, a note
;; created on December 9th, 2020 at 11:30 will have the ID "202012091130".
;; Linking to such a note involves nothing more than placing the string
;; [[202012091130]] into another note in the directory.

;; There are several ways to follow links. The most basic way, which works in
;; any mode, is to simply call the function 'zk-follow-link-at-point' with
;; the point on an ID. This function could be bound to a convenient key.
;; Other ways of following links rely on external packages. If notes are in
;; 'org-mode', load the file 'zk-org.el' to enable click-to-follow links. If
;; 'Embark' (https://github.com/oantolin/embark) is installed, load
;; 'zk-embark.el' to enable 'embark-act' to target links at point as well as
;; filenames in a completion interface. If 'link-hint.el'
;; (https://github.com/noctuid/link-hint.el) is installed, load
;; 'zk-link-hint.el' to allow 'link-hint.el' to find visible IDs in a buffer.

;; A note's filename is constructed as follows: the ID number followed by the
;; title of the note followed by the file extension, e.g. "202012091130 On
;; the origin of species.txt". A key consequence of this ID/linking scheme is
;; that a note's title can change without any existing links to the note
;; being broken, wherever they might be in the directory.

;; The directory is a single folder containing all notes.

;; The structural simplicity of this set of functions is---one hopes, at
;; least---in line with the structural simplicity of the so-called
;; "Zettelkasten method," of which much can be read in many places, including
;; at https://www.zettelkasten.de.

;;; Code:

(require 'grep)

;;; Variables

(defvar zk-directory nil)
(defvar zk-file-extension nil)
(defvar zk-id-regexp "[0-9]\\{12\\}")
(defvar zk-id-time-string-format "%Y%m%d%H%M")
(defvar zk-new-note-header-function #'zk-new-note-header)
(defvar zk-search-function #'zk-grep)
(defvar zk-tag-regexp "[#][[:alnum:]_-]+")
(defvar zk-tag-search-function #'zk-grep)

(defvar zk-link-format "[[%s]]"
  "Set format for inserting link.
Used in conjunction with 'format', the string '%s' will be
replaced by a note's ID.")

(defvar zk-link-and-title nil
  "If non-nil, 'zk-insert-link' inserts both link and title.
If set to 'ask, 'zk-insert-link' asks each time whether to
include a title. In both cases, calling 'zk-insert-link' with a
prefix-argument reverts to default behavior and inserts only a
link. The format in which link and title are inserted can be
configured by setting the variable
'zk-link-and-title-format'.")

(defvar zk-link-and-title-format "[%t] [[%i]]"
  "Set format for inserting link and title togethers.
Used with 'format-spec', the string '%t' will be replaced by the
note's title and '%i' will be replaced by its ID.")

(defvar zk-default-backlink nil)
(defvar zk-current-notes-function nil)
(defvar zk-history nil)

;;; Low-Level Functions

(defun zk--generate-id ()
  "Generate and return a note ID.
The ID is created using `zk-id-time-string-format'."
  (let ((id (format-time-string zk-id-time-string-format)))
    (while (zk--id-unavailable-p id)
      (setq id (1+ (string-to-number id)))
      (setq id (number-to-string id)))
    id))

(defun zk--id-list ()
  "Return a list of IDs for all notes in 'zk-directory'."
  (let* ((files (directory-files zk-directory t zk-id-regexp))
         (all-ids))
    (dolist (file files)
      (progn
        (string-match zk-id-regexp file)
        (push (match-string 0 file) all-ids)))
    all-ids))

(defun zk--id-unavailable-p (str)
  "Return t if provided string STR is already in use as an id."
  (let ((all-ids (zk--id-list)))
    (member str all-ids)))

(defun zk--current-id ()
  "Return id of current note."
  (if (not (string=
            default-directory
            (expand-file-name (concat zk-directory "/"))))
      (user-error "Not a zk file")
    (string-match zk-id-regexp buffer-file-name))
  (match-string 0 buffer-file-name))

(defun zk--grep-file-list (str)
  "Return a list of files containing STR."
  (let* ((files (shell-command-to-string (concat
                                          "grep -lir -e "
                                          (regexp-quote str)
                                          " "
                                          zk-directory
                                          " 2>/dev/null")))
         (list (split-string files "\n" t)))
    (if (null list)
        (message (format "No results for \"%s\"" str))
      (mapcar 'abbreviate-file-name list))))

(defun zk--grep-tag-list ()
  "Return list of tags from all notes in zk directory."
  (let* ((files (shell-command-to-string (concat
                                          "grep -ohir -e '#[a-z0-9]\\+' "
                                          zk-directory " 2>/dev/null")))
         (list (split-string files "\n" t)))
    (delete-dups list)))

(defun zk--select-file (&optional list)
  "Wrapper around `completing-read' to select zk-file from LIST."
  (let* ((list (if list list
                 (directory-files zk-directory t zk-id-regexp)))
         (files (mapcar 'abbreviate-file-name list)))
    (completing-read
     "Select File: "
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (category . zk-file))
         (complete-with-action action files string predicate)))
     nil t nil 'zk-history)))

(defun zk--parse-id (target id)
  "Return TARGET, ie, 'file-path, 'file-name, or 'title, from file with ID."
  (let ((file (car (directory-files zk-directory nil id)))
        (return (pcase target
                  ('file-path '0)
                  ('file-name '0)
                  ('title '2))))
    (if file
        (progn
          (string-match (concat "\\(?1:"
                              zk-id-regexp
                              "\\).\\(?2:.*?\\)\\."
                              zk-file-extension
                              ".*")
                        file)
          (if (eq target 'file-path)
              (concat zk-directory "/" (match-string return file))
            (match-string return file)))
      (user-error (format "No file associated with %s" id)))))

(defun zk--parse-file (target file)
  "Return TARGET, either 'id or 'title, from FILE.

A note's title is understood to be the portion of its filename
following the ID, in the format 'zk-id-regexp', and preceding the
file extension."
  (let ((return (pcase target
                  ('id '1)
                  ('title '2))))
            (string-match (concat "\\(?1:"
                              zk-id-regexp
                              "\\).\\(?2:.*?\\)\\."
                              zk-file-extension
                              ".*")
                  file)
    (match-string return file)))

;;; Note Functions

;;;###autoload
(defun zk-new-note ()
  "Create a new note, insert link at point of creation."
  (interactive)
  (let* ((new-id (zk--generate-id))
         (orig-id (ignore-errors (zk--current-id)))
         (text (when (use-region-p)
                 (buffer-substring
                  (region-beginning)
                  (region-end))))
         (title (if (use-region-p)
                  (with-temp-buffer
                    (insert text)
                    (goto-char (point-min))
                    (buffer-substring
                     (point)
                     (line-end-position)))
                  (read-string "Note title: ")))
         (body (when (use-region-p)
                 (with-temp-buffer
                   (insert text)
                   (goto-char (point-min))
                   (forward-line 2)
                   (buffer-substring
                    (point)
                    (point-max))))))
    (unless orig-id
      (setq orig-id zk-default-backlink))
    (when (use-region-p)
      (kill-region (region-beginning) (region-end)))
    (zk-insert-link-and-title new-id title)
    (find-file (concat (format "%s/%s %s.%s"
                               zk-directory
                               new-id
                               title
                               zk-file-extension)))
    (funcall zk-new-note-header-function title new-id orig-id)
    (when body (insert body))
    (save-buffer)))

(defun zk-new-note-header (title new-id &optional orig-id)
  "Insert header in new notes."
  (insert (format "# [[%s]] %s \n===\ntags: \n" new-id title))
  (when orig-id
    (progn
      (insert "===\n<- ")
      (zk-insert-link-and-title orig-id (zk--parse-id 'title orig-id))
      (newline)))
  (insert "===\n\n"))

;;;###autoload
(defun zk-rename-note ()
  "Rename current note and replace title in header.
When header title does not match file title, ask to accept header
title as new title. If no, prompt for new title and replace
header title in buffer. If yes, file name changed to header
title."
  (interactive)
  (let* ((id (zk--current-id))
         (file-title (zk--parse-id 'title id))
         (header-title (progn
                      (save-excursion
                        (goto-char (point-min))
                        (re-search-forward id)
                        (re-search-forward " ")
                        (buffer-substring-no-properties
                         (point)
                         (line-end-position)))))
         (new-title))
    (if (not (string= file-title header-title))
      (if (y-or-n-p (format "Change from \"%s\" to \"%s\"?" file-title header-title))
          (setq new-title header-title)
        (setq new-title (read-string "New title: " file-title)))
      (setq new-title (read-string "New title: " file-title)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward id)
      (re-search-forward " ")
      (delete-region (point) (line-end-position))
      (insert new-title))
    (let ((new-file (concat
                     zk-directory "/"
                     id " "
                     new-title
                     "." zk-file-extension)))
      (rename-file buffer-file-name new-file t)
      (set-visited-file-name new-file t t))))

;;; Find File

;;;###autoload
(defun zk-find-file ()
  "Search and open file in 'zk-directory'."
  (interactive)
  (find-file (zk--select-file)))

;;;###autoload
(defun zk-find-file-by-id (id)
  "Open file associated with ID."
  (find-file (zk--parse-id 'file-path id)))

;;;###autoload
(defun zk-find-file-by-full-text-search (str)
  "Search for and open file containing STR."
  (interactive
   (list (read-string "Search string: ")))
  (let ((choice
         (completing-read
          (format "Files containing \"%s\": " str)
          (zk--grep-file-list str) nil t)))
    (find-file choice)))

;;;###autoload
(defun zk-current-notes ()
  "Select from list of currently open notes.
Can call a custom function, set to the variable
'zk-current-notes-function'. An alternative,
'zk-consult-current-notes', is provided in 'zk-consult.el'."
  (interactive)
  (if zk-current-notes-function
      (funcall zk-current-notes-function)
    (read-buffer
     "Current Notes: " nil t
     (lambda (x)
       (and (string-match zk-id-regexp (car x))
            (member (match-string 0 (car x))
                    (zk--id-list)))))))

;;; Follow Links

;;;###autoload
(defun zk-follow-link-at-point ()
  "Open note that corresponds with the zk-id at point."
  (interactive)
  (when (thing-at-point-looking-at zk-id-regexp)
    (find-file (zk--parse-id 'file-path (match-string-no-properties 0)))))

;;;###autoload
(defun zk-links-in-note ()
  "Select from list of notes linked to in the current note."
  (interactive)
  (let (files)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward zk-id-regexp nil t)
          (let ((note
                 (condition-case nil
                     (zk--parse-id 'file-path (match-string-no-properties 0))
                   (error "no-file"))))
            (when (file-exists-p note)
              (push note files))))))
        (find-file (zk--select-file (delete-dups files)))))

    
;;; List Backlinks

;;;###autoload
(defun zk-backlinks ()
  "Select from list of all notes that link to the current note."
  (interactive)
  (let* ((id (zk--current-id))
         (files (zk--grep-file-list
                 (regexp-quote (format zk-link-format id))))
         (choice (if (length= files 1) ;; presumes self-link in header
                     (user-error "No backlinks - no notes link to this note")
                   (zk--select-file
                    (remove (zk--parse-id 'file-path id) files)))))
    (find-file choice)))

;;; Insert Link

;;;###autoload
(defun zk-insert-link (id)
  "Insert link to note, with 'completing-read'.
By default, only a link is inserted. With prefix-argument, both
link and title are inserted. See variable 'zk-link-and-title'
for additional configurations."
  (interactive (list (zk--parse-file 'id (zk--select-file))))
  (let* ((pref-arg current-prefix-arg)
         (title (zk--parse-id 'title id)))
    (cond
     ((or (and (not pref-arg) (eq 't zk-link-and-title))
          (and pref-arg (eq 'nil zk-link-and-title)))
      (zk-insert-link-and-title id title))
     ((and (not pref-arg) (eq 'ask zk-link-and-title))
      (if (y-or-n-p "Include title? ")
          (zk-insert-link-and-title id title)
        (insert (format zk-link-format id))))
     ((or t
          (and pref-arg (eq 't zk-link-and-title)))
      (insert (format zk-link-format id))))))

(defun zk-insert-link-and-title (id title)
  (insert (format-spec zk-link-and-title-format
                       `((?i . ,id)(?t . ,title)))))

(defun zk-completion-at-point ()
  (let ((case-fold-search t)
        (beg (save-excursion
               (re-search-backward "\\[\\[" nil t)))
        (end (point)))
    (list beg end (zk--completion-at-point-list)
          :exclusive 'no)))

(defvar zk--completion-at-point-format "[[%i]] %t")

(defun zk--completion-at-point-list ()
  "Return a list of filenames for all notes in 'zk-directory'."
  (let* ((files (directory-files zk-directory zk-id-regexp))
         (output))
    (dolist (file files)
      (progn
        (string-match (concat "\\(?1:"
                              zk-id-regexp
                              "\\).\\(?2:.*?\\)\\."
                              zk-file-extension
                              ".*")
                      file)
        (let ((id (match-string 1 file))
              (title (match-string 2 file)))
          (when id
            (push (format-spec zk--completion-at-point-format
                               `((?i . ,id)(?t . ,title)))
                  output)))))
    output))

;; (add-to-list 'completion-at-point-functions #'zk-completion-at-point)

;;; Search

;;;###autoload
(defun zk-search (string)
  "Search for STRING using function set in 'zk-search-function'.
Defaults to 'zk-grep.'"
  (interactive "sSearch: ")
  (funcall zk-search-function string))

(defun zk-grep (regexp)
  "Wrapper around 'lgrep' to search for REGEXP in all notes.
Opens search results in a grep buffer."
  (grep-compute-defaults)
  (lgrep regexp (concat "*." zk-file-extension) zk-directory nil))

;;; Tag Functions

;;;###autoload
(defun zk-tag-search (tag)
  "Open grep buffer containing results of search for TAG.
Select TAG, with completion, from list of all tags in zk notes.
Defaults to 'zk-grep'."
  (interactive (list (completing-read "Tag: " (zk--grep-tag-list))))
  (funcall zk-tag-search-function tag))

;;;###autoload
(defun zk-tag-insert (tag)
  "Insert TAG at point.
Select TAG, with completion, from list of all tags in zk notes."
  (interactive (list (completing-read "Tag: " (zk--grep-tag-list))))
  (insert tag))

(provide 'zk)

;;; zk.el ends here
