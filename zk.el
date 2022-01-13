;;; zk.el --- Functions for dealing with link-connected notes -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 4, 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "24.3"))

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
;; created on December 9th, 2020 at 11:30 will have the zk ID "202012091130".
;; Linking to such a note involves nothing more than placing the string
;; [[202012091130]] into another note in the directory.

;; There are several ways to follow links. The most basic way, which works in
;; any mode, is to simply call the function 'zk-follow-link-at-point' with
;; the point on a zk ID. This function could be bound to a convenient key.
;; Other ways of following links rely on external packages. If notes are in
;; 'org-mode', load the file 'zk-org.el' to enable click-to-follow links. If
;; 'Embark' (https://github.com/oantolin/embark) is installed, load
;; 'zk-embark.el' to enable 'embark-act' to target links at point as well as
;; filenames in a completion interface. If 'link-hint.el'
;; (https://github.com/noctuid/link-hint.el) is installed, load
;; 'zk-link-hint.el' to allow 'link-hint.el' to find visible zk IDs in a buffer.

;; A note's filename is constructed as follows: the zk ID number followed by the
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
(require 'thingatpt)

;;; Variables

(defgroup zk nil
  "A zettelkasten on top of deft."
  :group 'text
  :group 'files
  :prefix "zk-")

(defcustom zk-directory nil
    "Main zettelkasten directory."
  :type 'string
  :group 'zk)

(defcustom zk-file-extension nil
  "The extension for zk files."
  :type 'string
  :group 'zk)

(defcustom zk-id-time-string-format "%Y%m%d%H%M"
  "Format for new zk IDs.
For supported options, please consult `format-time-string'.
Note: the regexp to find zk IDs is set separately.
If you change this value, set `zk-id-regexp' so that
the zk IDs can be found."
  :type 'string
  :group 'zk)

(defcustom zk-id-regexp "[0-9]\\{12\\}"
  "The regular expression used to search for zk IDs.
Set it so that it matches strings generated with
`zetteldeft-id-format'."
  :type 'regexp
  :group 'zk)

(defcustom zk-new-note-header-function #'zk-new-note-header
  "Function called by 'zk-new-note' to insert header in a new note.
A user-defined function should use 'insert' to insert a string or
strings. The arguments NEW-ID, TITLE, and ORIG-ID can be used to
those corresponding values from 'zk-new-note' available for
insertion. See 'zk-new-note-header' for an example."
   :type 'function
   :group 'zk)

(defcustom zk-new-note-link-insert t
  "Should 'zk-new-note' insert link to new note at point?

Options:
1. t - Always insert a link
2. 'zk - Ansert link only inside an existing note
3. 'ask - Ask user, yes or no
4. nil - Never insert a link

Calling 'zk-new-note' with a prefix-argument inserts a link
regardless of how 'zk-new-note-link-insert' is set."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Ask" 'ask)
                 (const :tag "Only in zk notes" 'zk)
                 (const :tag "Never" nil))
  :group 'zk)


(defcustom zk-search-function #'zk-grep
  "Function used by 'zk-search'.
Must take a single STRING argument."
  :type 'function
  :group 'zk)

(defcustom zk-tag-search-function #'zk-grep
  "Function used by 'zk-tag-search'.
Must take a single STRING argument."
  :type 'function
  :group 'zk)

(defcustom zk-link-format "[[%s]]"
  "Format for inserted links.
Used in conjunction with 'format', the string '%s' will be
replaced by a note's ID."
    :type 'string
    :group 'zk)

(defcustom zk-link-and-title nil
  "Should 'zk-insert-link' insert both link and title?

Options:
1. t - Always inserts link and title; with 'prefix-arg', only link
2. 'ask - Ask user, yes or no; with 'prefix-arg', only link
3. nil - Only insert link, not title; 'with prefix-arg', include title

The format in which link and title are inserted can be configured
by setting the variable 'zk-link-and-title-format'."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Ask" 'ask)
                 (const :tag "Never" nil))
  :group 'zk)

(defcustom zk-link-and-title-format "[%t] [[%i]]"
  "Format for link and title when inserted to together.

The string '%t' will be replaced by the note's title and '%i'
will be replaced by its ID."
  :type 'string
  :group 'zk)

(defcustom zk-default-backlink nil
  "When non-nil, should be a single zk ID.
See 'zk-new-note' for details."
  :type 'string
  :group 'zk)

(defcustom zk-current-notes-function nil
  "User-defined function for listing currently open notes.
See 'zk-current-notes' for details."
  :type 'function
  :group 'zk)

(defcustom zk-completion-at-point-format "[[%i]] %t"
  "Format for completion table used by 'zk-completion-at-point'.

The string '%t' will be replaced by the note's title and '%i'
will be replaced by its ID."
  :type 'string
  :group 'zk)

(defvar zk-history nil)

;;; Low-Level Functions

(defun zk--generate-id ()
  "Generate and return a zk ID.
The ID is created using `zk-id-time-string-format'."
  (let ((id (format-time-string zk-id-time-string-format)))
    (while (zk--id-unavailable-p id)
      (setq id (1+ (string-to-number id)))
      (setq id (number-to-string id)))
    id))

(defun zk--id-list ()
  "Return a list of zk IDs for all notes in 'zk-directory'."
  (let* ((files (zk--directory-files t))
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
            (concat (expand-file-name zk-directory) "/")))
      (user-error "Not a zk file")
    (string-match zk-id-regexp buffer-file-name))
  (match-string 0 buffer-file-name))

(defun zk--directory-files (&optional full regexp)
  "Return list of notes' filenames in 'zk-directory' .
Excludes lockfiles, autosave files, and backupfiles. When FULL is
non-nil, return full file-paths. If REGEXP is non-nil, it must be
a regexp to replace the default, 'zk-id-regexp'."
  (let* ((regexp (if regexp regexp
                  zk-id-regexp))
         (list (directory-files zk-directory full regexp))
         (files (remq nil (mapcar
                           (lambda (x)
                             (unless (string-match-p "^[.]\\|[#|~]$"
                                                     (file-name-nondirectory x))
                               (abbreviate-file-name x)))
                           list))))
    files))

(defun zk--grep-file-list (str)
  "Return a list of files containing STR."
  (let* ((files (shell-command-to-string (concat
                                          "grep -lir --include \\*."
                                          zk-file-extension
                                          " -e "
                                          (regexp-quote str)
                                          " "
                                          zk-directory
                                          " 2>/dev/null")))
         (list (split-string files "\n" t)))
    (when list
      (mapcar #'abbreviate-file-name list))))

(defun zk--grep-tag-list ()
  "Return list of tags from all notes in zk directory."
  (let* ((files (shell-command-to-string (concat
                                          "grep -ohir -e '#[a-zA-Z0-9]\\+' "
                                          zk-directory " 2>/dev/null")))
         (list (split-string files "\n" t)))
    (delete-dups list)))

(defun zk--select-file (&optional list)
  "Wrapper around `completing-read' to select zk-file.
Optional argument LIST."
  (let* ((files (if list list
                  (zk--directory-files t))))
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
  (let ((file (car (zk--directory-files nil id)))
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
      (user-error "No file associated with %s" id))))

(defun zk--parse-file (target file)
  "Return TARGET, either 'id or 'title, from FILE.
A note's title is understood to be the portion of its filename
following the zk ID, in the format 'zk-id-regexp', and preceding the
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
  (let* ((pref-arg current-prefix-arg)
         (new-id (zk--generate-id))
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
    (when (or pref-arg
              (eq zk-new-note-link-insert 't)
              (and (eq zk-new-note-link-insert 'zk)
                   (string= default-directory
                            (concat (expand-file-name zk-directory)
                                    "/")))
              (and (eq zk-new-note-link-insert 'ask)
                   (y-or-n-p "Insert link at point? ")))
      (zk-insert-link-and-title new-id title))
    (find-file (concat (format "%s/%s %s.%s"
                               zk-directory
                               new-id
                               title
                               zk-file-extension)))
    (funcall zk-new-note-header-function title new-id orig-id)
    (when body (insert body))
    (save-buffer)))

(defun zk-new-note-header (title new-id &optional orig-id)
  "Insert header in new notes with args TITLE and NEW-ID.
Optionally use ORIG-ID for backlink."
  (insert (format "# %s %s \n===\ntags: \n" new-id title))
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
      (if (y-or-n-p (format "Change from \"%s\" to \"%s\"? " file-title header-title))
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
  (let ((files (zk--grep-file-list str)))
    (if files
        (find-file (completing-read
                    (format "Files containing \"%s\": " str)
                    files nil t))
      (user-error "No results for \"%s\"" str))))

;;;###autoload
(defun zk-current-notes ()
  "Select from list of currently open notes.
Optionally call a custom function by setting the variable
'zk-current-notes-function' to a function name. One such
function, 'zk-consult-current-notes', is provided in
'zk-consult.el'."
  (interactive)
  (if zk-current-notes-function
      (funcall zk-current-notes-function)
    (switch-to-buffer
     (read-buffer
      "Current Notes: " nil t
      (lambda (x)
        (and (string-match zk-id-regexp (car x))
             (member (match-string 0 (car x))
                     (zk--id-list))))))))

;;; Follow Links

;;;###autoload
(defun zk-follow-link-at-point ()
  "Open note that corresponds with the zk ID at point."
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
                   (error "No file"))))
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
                 (regexp-quote (format zk-link-format id)))))
    (if files
        (find-file (zk--select-file files))
      (user-error "No backlinks found"))))

;;; Insert Link

;;;###autoload
(defun zk-insert-link (id)
  "Insert link to note with ID.
By default, only a link is inserted. With prefix-argument, both
link and title are inserted. See variable 'zk-link-and-title'
for additional configurations."
  (interactive (list (zk--parse-file 'id (zk--select-file))))
  (let* ((pref-arg current-prefix-arg)
         (title (zk--parse-id 'title id)))
    (cond
     ((or (and (not pref-arg) (eq 't zk-link-and-title))
          (and pref-arg (not zk-link-and-title)))
      (zk-insert-link-and-title id title))
     ((and (not pref-arg) (eq 'ask zk-link-and-title))
      (if (y-or-n-p "Include title? ")
          (zk-insert-link-and-title id title)
        (insert (format zk-link-format id))))
     ((or t
          (and pref-arg (eq 't zk-link-and-title)))
      (insert (format zk-link-format id))))))

(defun zk-insert-link-and-title (id title)
  "Insert zk ID and TITLE according to 'zk-link-and-title-format'."
  (insert (format-spec zk-link-and-title-format
                       `((?i . ,id)(?t . ,title)))))

(defun zk-completion-at-point ()
  "Completion-at-point function for zk-links.
Whent added to 'completion-at-point-functions', typing two
brackets \"[[\" initiates completion."
  (let ((case-fold-search t)
        (pt (point)))
    (save-excursion
      (when (re-search-backward "\\[\\[" nil t)
        (list (match-beginning 0)
              pt
              (zk--completion-at-point-list)
              :exclusive 'no)))))

(defun zk--completion-at-point-list ()
  "Return a list of candidates for 'zk-completion-at-point'."
  (let* ((files (zk--directory-files))
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
            (push (format-spec zk-completion-at-point-format
                               `((?i . ,id)(?t . ,title)))
                  output)))))
    output))

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
