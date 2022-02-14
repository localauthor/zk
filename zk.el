;;; zk.el --- Functions for working with Zettelkasten-style linked notes -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 4, 2022
;; License: GPL-3.0-or-later
;; Version: 0.2
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "24.4"))

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

;; A note's filename is constructed as follows: the zk ID number followed by
;; the title of the note followed by the file extension, e.g. "202012091130
;; On the origin of species.txt". A key consequence of this ID/linking scheme
;; is that a note's title can change without any existing links to the note
;; being broken, wherever they might be in the directory.

;; The directory is a single folder containing all notes.

;; The structural simplicity of this set of functions is---one hopes, at
;; least---in line with the structural simplicity of the so-called
;; "Zettelkasten method," of which much can be read in many places, including
;; at https://www.zettelkasten.de.

;;; Code:

(require 'thingatpt)
(require 'format-spec)

;;; Variable Declarations

(defvar embark-keymap-alist)
(defvar embark-target-finders)
(defvar embark-general-map)
(defvar embark-file-map)

;;; Variables

(defgroup zk nil
  "A Zettelkasten implementation for Emacs."
  :group 'text
  :group 'files
  :prefix "zk-")

(defcustom zk-directory nil
  "Main zk directory."
  :type 'string
  :group 'zk)

(defcustom zk-file-extension nil
  "The extension for zk files."
  :type 'string
  :group 'zk)

(defcustom zk-enable-link-buttons t
  "When non-nil, valid zk-id links will be clickable buttons.
Allows 'zk-make-link-buttons' to be added to 'find-file-hook', so
buttons will be automatically created when a note is opened."
  :type 'boolean
  :group 'zk)

(defcustom zk-id-time-string-format "%Y%m%d%H%M"
  "Format for new zk IDs.
For supported options, please consult `format-time-string'.
Note: the regexp to find zk IDs is set separately.
If you change this value, set `zk-id-regexp' so that
the zk IDs can be found."
  :type 'string
  :group 'zk)

(defcustom zk-id-regexp "\\([0-9]\\{12\\}\\)"
  "The regular expression used to search for zk IDs.
Set it so that it matches strings generated with
`zk-id-format'."
  :type 'regexp
  :group 'zk)

(defcustom zk-tag-regexp "#[a-zA-Z0-9]\\+"
  "The regular expression used to search for tags."
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

(defcustom zk-new-note-link-insert 'ask
  "Should 'zk-new-note' insert link to new note at point?

Options:
1. t - Always insert a link
2. 'zk - Insert link only inside an existing note
3. 'ask - Ask user, yes or no
4. nil - Never insert a link

Calling 'zk-new-note' with a prefix-argument inserts a link
regardless of how 'zk-new-note-link-insert' is set."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Ask" 'ask)
                 (const :tag "Only in zk notes" 'zk)
                 (const :tag "Never" nil))
  :group 'zk)

(defcustom zk-grep-function #'zk-grep
  "Function used by 'zk-search'.
Must take a single STRING argument."
  :type 'function
  :group 'zk)

(defcustom zk-tag-grep-function #'zk-grep
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

(defcustom zk-link-and-title t
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

(defcustom zk-link-and-title-format "%t [[%i]]"
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

(defvar zk-link-regexp (format (regexp-quote zk-link-format) zk-id-regexp))
(defvar zk-history nil)

;;; Embark Integration

(defvar zk-id-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zk-follow-link-at-point)
    (define-key map (kbd "k") #'zk-copy-link-and-title)
    (define-key map (kbd "s") #'zk-search)
    map)
  "Keymap for Embark zk-id at-point actions.")

(defvar zk-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'zk-insert-link)
    (define-key map (kbd "f") #'zk-find-file)
    (define-key map (kbd "k") #'zk-copy-link-and-title)
    map)
  "Keymap for Embark zk-file minibuffer actions.")

;;;###autoload
(defun zk-embark-target-zk-id-at-point ()
  "Target zk-id at point."
  (when (thing-at-point-looking-at zk-id-regexp)
    (let ((zk-id (match-string-no-properties 0)))
      `(zk-id ,zk-id . ,(bounds-of-thing-at-point 'symbol)))))

;;;###autoload
(defun zk-setup-embark ()
  "Setup Embark integration for zk.
Adds zk-id as an Embark target, and adds zk-id-map and
zk-file-map to embark-keymap-alist."
  (with-eval-after-load 'embark
    (add-to-list 'embark-target-finders 'zk-embark-target-zk-id-at-point)
    (add-to-list 'embark-keymap-alist '(zk-id . zk-id-map))
    (add-to-list 'embark-keymap-alist '(zk-file . zk-file-map))
    (set-keymap-parent zk-id-map embark-general-map)
    (set-keymap-parent zk-file-map embark-file-map)))

;;; Low-Level Functions

(defun zk-file-p (&optional file)
  "Return t if 'current-buffer' is a zk-file.
With optional argument FILE."
  (let ((dir (if file file
               default-directory))
        (file-name (if file file
                     buffer-file-name)))
    (when (and (file-in-directory-p dir zk-directory)
               (string-match-p zk-id-regexp file-name))
      t)))

(defun zk--generate-id ()
  "Generate and return a zk ID.
The ID is created using `zk-id-time-string-format'."
  (let ((id (format-time-string zk-id-time-string-format)))
    (while (zk--id-unavailable-p id)
      (setq id (1+ (string-to-number id)))
      (setq id (number-to-string id)))
    id))

(defun zk--id-list (&optional str)
  "Return a list of zk IDs for notes in 'zk-directory'.
Optional search for regexp STR in note title."
  (let* ((files (if str (zk--directory-files t str)
                 (zk--directory-files t)))
	(id-list (zk--parse-file 'id files)))
    (if (listp id-list) id-list
      (list id-list))))

(defun zk--id-unavailable-p (str)
  "Return t if provided string STR is already in use as an id."
  (let ((all-ids (zk--id-list)))
    (member str all-ids)))

(defun zk--current-id ()
  "Return id of current note."
  (unless (zk-file-p)
    (user-error "Not a zk file"))
  (string-match zk-id-regexp buffer-file-name)
  (match-string 0 buffer-file-name))

(defun zk--directory-files (&optional full regexp)
  "Return list of notes' filenames in 'zk-directory' .
Excludes lockfiles, autosave files, and backup files. When FULL is
non-nil, return full file-paths. If REGEXP is non-nil, it must be
a regexp to replace the default, 'zk-id-regexp'."
  (let* ((regexp (if regexp regexp
                   zk-id-regexp))
         (list (directory-files zk-directory full regexp))
         (files (remq nil (mapcar
                           (lambda (x)
                             (unless (string-match-p
                                      "^[.]\\|[#|~]$"
                                      (file-name-nondirectory x))
                               x))
                           list))))
    files))

(defun zk--grep-file-list (str)
  "Return a list of files containing regexp STR."
  (let* ((files (shell-command-to-string (concat
                                          "grep -lir --include \\*."
                                          zk-file-extension
                                          " -e "
                                          (shell-quote-argument
                                           str)
                                          " "
                                          zk-directory
                                          " 2>/dev/null"))))
    (split-string files "\n" t)))

(defun zk--grep-id-list (str)
  "Return a list of IDs for files containing STR."
  (zk--parse-file 'id (zk--grep-file-list str)))

(defun zk--grep-tag-list ()
  "Return list of tags from all notes in zk directory."
  (let* ((files (shell-command-to-string (concat
                                          "grep -ohir -e "
                                          (shell-quote-argument
                                           zk-tag-regexp)
                                          " "
                                          zk-directory " 2>/dev/null")))
         (list (split-string files "\n" t)))
    (delete-dups list)))

(defun zk--select-file (&optional prompt list)
  "Wrapper around `completing-read' to select zk-file.
Offers candidates from 'zk--directory-files', or from LIST when
supplied. Can take a PROMPT argument."
  (let* ((files (if list list
                  (zk--directory-files t))))
    (completing-read
     (if prompt prompt
       "Select File: ")
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (group-function . zk--group-function)
             (category . zk-file))
         (complete-with-action action files string predicate)))
     nil t nil 'zk-history)))

(defun zk--group-function (cand transform)
  "TRANSFORM completion CAND to remove 'zk-directory' path."
  (if transform
      (file-name-nondirectory cand)
    "zk"))

(defun zk--id-at-point ()
  "Return ID at point."
  (when (thing-at-point-looking-at zk-id-regexp)
    (match-string-no-properties 0)))

(defun zk--alist ()
  "Return an alist ID, title, and file-path triples."
  (mapcar
   (lambda (file)
     (progn
       (string-match (concat "\\(?1:"
                             zk-id-regexp
                             "\\).\\(?2:.*?\\)\\."
                             zk-file-extension
                             ".*")
                     file)
       `(,(match-string-no-properties 1 file) ,(match-string-no-properties 2 file) ,file)))
   (zk--directory-files t)))

(defun zk--parse-id (target ids)
  "Return TARGET, either 'file-path or 'title, from files with IDS.
Takes a single ID, as a string, or a list of IDs."
  (let* ((zk-alist (zk--alist))
         (return
          (cond ((eq target 'file-path)
                 (cond ((stringp ids)
                        (if (member ids (zk--id-list))
                            (cddr (assoc ids zk-alist))
                          (user-error "No file associated with %s" ids)))
                       ((listp ids)
                        (mapcar
                         (lambda (x)
                           (caddr (assoc x zk-alist)))
                         ids))))
                ((eq target 'title)
                 (cond ((stringp ids)
                        (if (member ids (zk--id-list))
                            (cadr (assoc ids zk-alist))
                          (user-error "No file associated with %s" ids)))
                       ((listp ids)
                        (mapcar
                         (lambda (x)
                           (cadr (assoc x zk-alist)))
                         ids)))))))
    (if (eq 1 (length return))
        (car return)
      return)))

(defun zk--parse-file (target files)
  "Return TARGET, either 'id or 'title, from FILES.
Takes a single file-path, as a string, or a list of file-paths.
A note's title is understood to be the portion of its filename
following the zk ID, in the format 'zk-id-regexp', and preceding the
file extension."
  (let* ((target (pcase target
                   ('id '1)
                   ('title '2)))
         (files (if (listp files)
                    files
                  (list files)))
         (return
          (mapcar
           (lambda (file)
             (string-match (concat "\\(?1:"
                                   zk-id-regexp
                                   "\\).\\(?2:.*?\\)\\."
                                   zk-file-extension
                                   ".*")
                           file)
             (match-string target file))
           files)))
    (if (eq 1 (length return))
        (car return)
      return)))

;;; Buttons

(defun zk-setup-auto-link-buttons ()
  "Enable automatic link creation when zk-file is opened.
Adds 'zk-make-link-buttons' to 'find-file-hook.'"
  (setq zk-enable-link-buttons t)
  (add-hook 'find-file-hook #'zk-make-link-buttons))

(eval-and-compile
  (define-button-type 'zk-link
    'action 'zk-follow-link-at-point
    'follow-link t
    'help-echo (lambda (_win _obj pos)
                 (format
                  "%s"
                  (zk--parse-id
                   'title
                   (button-label
                    (button-at pos)))))))

(defun zk-make-link-buttons ()
  "Make zk-link-regexps in current buffer into zk-link buttons."
  (interactive)
  (when (and (zk-file-p)
             zk-enable-link-buttons)
    (let ((ids (zk--id-list)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward zk-link-regexp nil t)
          (let ((beg (match-beginning 1))
                (end (match-end 1))
                (id (match-string-no-properties 1)))
            (when (member id ids)
              (make-button beg end 'type 'zk-link))))))))

(defun zk-make-button-before-point ()
  "Find 'zk-link-regexp' before point and make it a zk-link button."
  (interactive)
  (save-excursion
    (re-search-backward zk-link-regexp (line-beginning-position))
    (make-button (match-beginning 1) (match-end 1)
                 'type 'zk-link)))

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
                   (zk-file-p))
              (and (eq zk-new-note-link-insert 'ask)
                   (y-or-n-p "Insert link at point? ")))
      (zk-insert-link new-id title))
    (save-buffer)
    (find-file (concat (format "%s/%s %s.%s"
                               zk-directory
                               new-id
                               title
                               zk-file-extension)))
    (funcall zk-new-note-header-function title new-id orig-id)
    (when body (insert body))
    (when zk-enable-link-buttons (zk-make-link-buttons))
    (save-buffer)))

(defun zk-new-note-header (title new-id &optional orig-id)
  "Insert header in new notes with args TITLE and NEW-ID.
Optionally use ORIG-ID for backlink."
  (insert (format "# %s %s\n===\ntags: \n" new-id title))
  (when (ignore-errors (zk--parse-id 'title orig-id)) ;; check for file
    (progn
      (insert "===\n<- ")
      (zk--insert-link-and-title orig-id (zk--parse-id 'title orig-id))
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
      (set-visited-file-name new-file t t)
      (save-buffer))))

;;; Find File

;;;###autoload
(defun zk-find-file ()
  "Find file in 'zk-directory'."
  (interactive)
  (find-file (zk--select-file "Find file: ")))

;;;###autoload
(defun zk-find-file-by-id (id)
  "Find file associated with ID."
  (find-file (zk--parse-id 'file-path id)))

;;;###autoload
(defun zk-find-file-by-full-text-search (str)
  "Find files containing regexp STR."
  (interactive
   (list (read-string "Search string: ")))
  (let ((files (zk--grep-file-list str)))
    (if files
        (find-file (zk--select-file
                    (format "Files containing \"%s\": " str) files))
      (user-error "No results for \"%s\"" str))))

;;;###autoload
(defun zk-current-notes ()
  "Select from list of currently open notes.
Optionally call a custom function by setting the variable
'zk-current-notes-function' to a function name."
  (interactive)
  (if zk-current-notes-function
      (funcall zk-current-notes-function)
    (switch-to-buffer
     (zk--select-file
      "Current Notes:"
      (remq nil
            (mapcar
             (lambda (x)
               (when (and (buffer-file-name x)
                          (zk-file-p (buffer-file-name x)))
                 (buffer-name x)))
             (buffer-list)))))))

;;; Follow Links

;;;###autoload
(defun zk-follow-link-at-point (&optional _)
  "Open note that corresponds with the zk ID at point."
  (interactive)
  (find-file (zk--parse-id 'file-path (zk--id-at-point))))

(defun zk--links-in-note-list (id)
  "Return list of links in note with ID."
  (let (files)
    (save-buffer)
    (with-temp-buffer
      (insert-file-contents (zk--parse-id 'file-path id))
      ;; skip id in header
      (goto-char (point-min))
      (forward-line 2)
      (save-match-data
        (while (re-search-forward zk-id-regexp nil t)
          (let ((note
                 (condition-case nil
                     (zk--parse-id 'file-path (match-string-no-properties 0))
                   (error "No file"))))
            (when (file-exists-p note)
              (push note files)))))
      files)))

;;;###autoload
(defun zk-links-in-note ()
  "Select from list of notes linked to in the current note."
  (interactive)
  (let* ((id (zk--current-id))
         (files (zk--links-in-note-list id)))
    (if files
        (find-file (zk--select-file "Links: " (delete-dups files)))
      (user-error "No links found"))))

;;; Insert Link

;;;###autoload
(defun zk-insert-link (id &optional title)
  "Insert link to note with ID and optional TITLE.
By default, only a link is inserted. With prefix-argument, both
link and title are inserted. See variable 'zk-link-and-title'
for additional configurations."
  (interactive (list (zk--parse-file 'id (zk--select-file "Insert link: "))))
  (let* ((pref-arg current-prefix-arg)
         (title (if title title
                  (zk--parse-id 'title id))))
    (cond
     ((or (and (not pref-arg) (eq 't zk-link-and-title))
          (and pref-arg (not zk-link-and-title)))
      (zk--insert-link-and-title id title))
     ((and (not pref-arg) (eq 'ask zk-link-and-title))
      (if (y-or-n-p "Include title? ")
          (zk--insert-link-and-title id title)
        (zk--insert-link id)))
     ((or t
          (and pref-arg (eq 't zk-link-and-title)))
      (zk--insert-link id)))))

(defun zk--insert-link (id)
  "Insert link to note with ID, with button optional."
  (insert (format zk-link-format id))
  (when zk-enable-link-buttons
    (zk-make-button-before-point)))

(defun zk--insert-link-and-title (id title)
  "Insert zk ID and TITLE according to 'zk-link-and-title-format'."
  (insert (format-spec zk-link-and-title-format
                       `((?i . ,id)(?t . ,title))))
  (when zk-enable-link-buttons
    (zk-make-button-before-point)))

;;; Completion at Point

(defun zk--format-candidates (&optional files format)
  "Return a list of FILES as formatted candidates, following FORMAT.

FORMAT must be a 'format-spec' template, wherein '%i' is replaced
by the ID and '%t' by the title. It can be a string, such as \"%t
[[%i]]\", or a variable whose value is a string. If nil,
'zk-completion-at-point-format' will be used by default.

FILES must be a list of filepaths. If nil, all files in
'zk-directory' will be returned as formatted candidates."
  (let* ((format (if format format
                   zk-completion-at-point-format))
         (list (if files files
                 (zk--directory-files)))
         (output))
    (dolist (file list)
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
            (push (format-spec format
                               `((?i . ,id)(?t . ,title)))
                  output)))))
    output))

(defun zk-completion-at-point ()
  "Completion-at-point function for zk-links.
When added to 'completion-at-point-functions', typing two
brackets \"[[\" initiates completion."
  (let ((case-fold-search t)
        (pt (point)))
    (save-excursion
      (when (re-search-backward "\\[\\[" nil t)
        (list (match-beginning 0)
              pt
              (zk--format-candidates)
              :exclusive 'no)))))

;; TODO add post completion hook, to optionally make button before point?

;;; Copy Link and Title

;;;###autoload
(defun zk-copy-link-and-title (&optional id)
  "Copy link and title for ID at point."
  (interactive (list (zk--parse-file 'id (zk--select-file "Copy link: "))))
  (let* ((id (if id id (zk--id-at-point)))
         (title (zk--parse-id 'title id)))
    (kill-new (format-spec zk-completion-at-point-format
                           `((?i . ,id)(?t . ,title))))))

;;; List Backlinks

(defun zk--backlinks-list (id)
  "Return list of notes that link to note with ID."
  (zk--grep-file-list (regexp-quote (format zk-link-format id))))

;;;###autoload
(defun zk-backlinks ()
  "Select from list of all notes that link to the current note."
  (interactive)
  (let* ((id (zk--current-id))
         (files (zk--backlinks-list id)))
    (if files
        (find-file (zk--select-file "Backlinks: " files))
      (user-error "No backlinks found"))))

;;; Search

;;;###autoload
(defun zk-search (string)
  "Search for STRING using function set in 'zk-grep-function'.
Defaults to 'zk-grep.'"
  (interactive "sSearch: ")
  (funcall zk-grep-function string))

(defun zk-grep (regexp)
  "Wrapper around 'rgrep' to search for REGEXP in all notes.
Opens search results in a grep buffer."
  (grep-compute-defaults)
  (rgrep regexp (concat "*." zk-file-extension) zk-directory nil))

;;; Tag Functions

;;;###autoload
(defun zk-tag-search (tag)
  "Open grep buffer containing results of search for TAG.
Select TAG, with completion, from list of all tags in zk notes.
Defaults to 'zk-grep'."
  (interactive (list (completing-read "Tag: " (zk--grep-tag-list))))
  (funcall zk-tag-grep-function tag))

;;;###autoload
(defun zk-tag-insert (tag)
  "Insert TAG at point.
Select TAG, with completion, from list of all tags in zk notes."
  (interactive (list (completing-read "Tag: " (zk--grep-tag-list))))
  (insert tag))

;;; Find Dead Links and Unlinked Notes

(defun zk--grep-link-id-list ()
  "Return list of all ids that appear as links in zk directory."
  (let* ((files (shell-command-to-string (concat
                                          "grep -ohir -e "
                                          (shell-quote-argument
                                           zk-link-regexp)
                                          " "
                                          zk-directory " 2>/dev/null")))
         (list (split-string files "\n" t))
         (ids (mapcar
               (lambda (x)
                 (string-match zk-id-regexp x)
                 (match-string 0 x))
               list)))
    (delete-dups ids)))

(defun zk--dead-link-id-list ()
  "Return list of all links with no corresponding note."
  (let* ((all-link-ids (zk--grep-link-id-list))
         (all-ids (zk--id-list)))
    (delete-dups (remq nil (mapcar
                            (lambda (x)
                              (string-match zk-id-regexp x)
                              (when (not (member (match-string-no-properties 0 x) all-ids))
                                x))
                            all-link-ids)))))

;;;###autoload
(defun zk-grep-dead-links ()
  "Search for dead links using 'zk-search-function'."
  (interactive)
  (let* ((dead-link-ids (zk--dead-link-id-list)))
    (if dead-link-ids
        (funcall zk-grep-function (mapconcat
                                   #'identity
                                   dead-link-ids
                                   "\\|"))
      (user-error "No dead links found"))))

(defun zk--unlinked-notes-list ()
  "Return list of IDs for notes that no notes link to."
  (let* ((all-link-ids (zk--grep-link-id-list))
         (all-ids (zk--id-list)))
    (remq nil (mapcar
               (lambda (x)
                 (when (not (member x all-link-ids))
                   x))
               all-ids))))

;;;###autoload
(defun zk-unlinked-notes ()
  "Find unlinked notes."
  (interactive)
  (let* ((ids (zk--unlinked-notes-list))
         (notes (zk--parse-id 'file-path ids)))
    (if notes
        (find-file (zk--select-file "Unlinked notes: " notes))
      (user-error "No unlinked notes found"))))

(provide 'zk)

;;; zk.el ends here
