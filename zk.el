;;; zk.el --- Functions for working with Zettelkasten-style linked notes -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 4, 2022
;; License: GPL-3.0-or-later
;; Version: 0.5
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "25.1"))

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

;; By default (see `zk-file-name-title-optional'), note's filename is
;; constructed as follows: the zk ID number followed by the title of the note
;; followed by the file extension, e.g. "202012091130 On the origin of
;; species.txt". A key consequence of this ID/linking scheme is that a note's
;; title can change without any existing links to the note being broken,
;; wherever they might be in the directory.

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
  :type 'string)

;; Borrowed from Deft by Jason R. Blevins <jblevins@xbeta.org>
(defcustom zk-directory-recursive nil
  "Recursively search for files in subdirectories of `zk-directory'."
  :type 'boolean)

(defcustom zk-directory-recursive-ignore-dir-regexp
  "\\(?:\\.\\|\\.\\.\\)$"
  "Regexp for subdirs to be ignored when ‘zk-directory-recursive’ is non-nil."
  :type 'string)

(defcustom zk-file-extension nil
  "The extension for zk files."
  :type 'string)

(defcustom zk-file-name-separator " "
  "Character(s), as a string, to separate elements of filename.

Useful for keeping spaces out of file-names. When set to \"-\",
for example, the file-name will be in the form
\"202012341234-Title-of-note.ext\". In notes, the title will be
rendered with spaces."
  :type 'string)

(defcustom zk-file-name-title-optional nil
  "If non-nil, file names can consist of IDs only, like \"202012341234\".
Note: If you change this value, also set `zk--parse-file-function' to a
function that can return the title for a given file."
  :type 'boolean)

(defcustom zk--parse-file-function #'zk--parse-file-name
  "A function taking two arguments, TARGET and FILE, which returns either 'id
or 'title (as specified by TARGET) for the given FILE."
  :type 'function)

(defcustom zk-enable-link-buttons t
  "When non-nil, valid zk-id links will be clickable buttons.
Allows `zk-make-link-buttons' to be added to `find-file-hook', so
buttons will be automatically created when a note is opened."
  :type 'boolean)

(defcustom zk-id-time-string-format "%Y%m%d%H%M"
  "Format for new zk IDs.
For supported options, please consult `format-time-string'.
Note: the regexp to find zk IDs is set separately.
If you change this value, set `zk-id-regexp' so that
the zk IDs can be found."
  :type 'string)

(defcustom zk-id-regexp "\\([0-9]\\{12\\}\\)"
  "The regular expression used to search for zk IDs.
Set it so that it matches strings generated with
`zk-id-format'."
  :type 'regexp)

(defcustom zk-tag-regexp "\\s#[a-zA-Z0-9]\\+"
  "The regular expression used to search for tags."
  :type 'regexp)

(defcustom zk-new-note-header-function #'zk-new-note-header
  "Function called by `zk-new-note' to insert header in a new note.
A user-defined function should use `insert' to insert a string or
strings. The arguments NEW-ID, TITLE, and ORIG-ID can be used to
those corresponding values from `zk-new-note' available for
insertion. See `zk-new-note-header' for an example."
  :type 'function)

(defcustom zk-new-note-link-insert 'ask
  "Should `zk-new-note' insert link to new note at point?

Options:
1. t - Always insert a link
2. `zk - Insert link only inside an existing note
3. `ask - Ask user, yes or no
4. nil - Never insert a link

Calling `zk-new-note' with a prefix-argument inserts a link
regardless of how `zk-new-note-link-insert' is set."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Ask" ask)
                 (const :tag "Only in zk notes" zk)
                 (const :tag "Never" nil)))

(defcustom zk-select-file-function #'zk--select-file
  "Function for performing completing read.
Must take an optional prompt and a list of files"
  :type 'function)

(defcustom zk-search-function #'zk-grep
  "Function used by `zk-search'.
Must take a single STRING argument."
  :type 'function)

(make-obsolete-variable 'zk-grep-function "The use of the
  'zk-grep-function' variable is deprecated.
 'zk-search-function' should be used instead"
                        "0.5")

(defcustom zk-tag-search-function #'zk-grep
  "Function used by `zk-tag-search'.
Must take a single STRING argument."
  :type 'function)

(make-obsolete-variable 'zk-tag-grep-function "The use of the
  'zk-tag-grep-function' variable is deprecated.
 'zk-tag-search-function' should be used instead"
                        "0.5")

(defcustom zk-link-format "[[%s]]"
  "Format for inserted links.
Used in conjunction with `format', the string `%s' will be
replaced by a note's ID."
  :type 'string)

(defcustom zk-link-and-title t
  "Should `zk-insert-link' insert both link and title?

Options:
1. t - Always inserts link and title; with `prefix-arg', only link
2. `ask - Ask user, yes or no; with `prefix-arg', only link
3. nil - Only insert link, not title; with `prefix-arg', include title

The format in which link and title are inserted can be configured
by setting the variable `zk-link-and-title-format'."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Ask" ask)
                 (const :tag "Never" nil)))

(defcustom zk-link-and-title-format "%t [[%i]]"
  "Format for link and title when inserted to together.

The string `%t' will be replaced by the note's title and `%i'
will be replaced by its ID."
  :type 'string)

(defcustom zk-default-backlink nil
  "When non-nil, should be a single zk ID.
See `zk-new-note' for details."
  :type 'string)

(defcustom zk-current-notes-function nil
  "User-defined function for listing currently open notes.
See `zk-current-notes' for details."
  :type 'function)

(defcustom zk-completion-at-point-format "[[%i]] %t"
  "Format for completion table used by `zk-completion-at-point'.

The string `%t' will be replaced by the note's title and `%i'
will be replaced by its ID."
  :type 'string)

(defvar zk-link-regexp (format (regexp-quote zk-link-format) zk-id-regexp))

(defvar zk-file-history nil)
(defvar zk-search-history nil)

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
Adds zk-id as an Embark target, and adds `zk-id-map' and
`zk-file-map' to `embark-keymap-alist'."
  (with-eval-after-load 'embark
    (add-to-list 'embark-target-finders 'zk-embark-target-zk-id-at-point)
    (add-to-list 'embark-keymap-alist '(zk-id . zk-id-map))
    (add-to-list 'embark-keymap-alist '(zk-file . zk-file-map))
    (set-keymap-parent zk-id-map embark-general-map)
    (set-keymap-parent zk-file-map embark-file-map)))

;;; Low-Level Functions

(defun zk-file-p (&optional file strict)
  "Return t if FILE is a zk-file.
If FILE is not given, get it from variable `buffer-file-name'. If
STRICT is non-nil, make sure the file is in `zk-directory',
otherwise just match against `zk-id-regexp'."
  (let ((file (cond ((stringp file) file)
                    ((null file) buffer-file-name)
                    ((listp file) (car file))
                    (t
                     (signal 'wrong-type-argument '(file))))))
    (and file
         (file-exists-p file)
         (string-match-p zk-id-regexp file)
         (or (not strict)
             (file-in-directory-p file zk-directory)))))

(defun zk--generate-id ()
  "Generate and return a zk ID.
The ID is created using `zk-id-time-string-format'."
  (let ((id (format-time-string zk-id-time-string-format)))
    (while (zk--id-unavailable-p id)
      (setq id (1+ (string-to-number id)))
      (setq id (number-to-string id)))
    id))

(defun zk--id-list (&optional str zk-alist)
  "Return a list of zk IDs for notes in `zk-directory'.
Optional search for regexp STR in note title, case-insenstive.
Takes an optional ZK-ALIST, for efficiency if `zk--id-list' is
called in an internal loop."
  (let ((zk-alist (or zk-alist (zk--alist)))
        (case-fold-search t)
        (ids))
    (dolist (item zk-alist)
      (if str
          (when (string-match str (cadr item))
            (push (car item) ids))
        (push (car item) ids)))
    ids))

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
  "Return list of zk-files in `zk-directory'.
Excludes lockfiles, autosave files, and backup files. When FULL is
non-nil, return full file-paths. If REGEXP is non-nil, it must be
a regexp to replace the default, `zk-id-regexp'.

When `zk-directory-recursive' is non-nil, searches recursively in
subdirectories of `zk-directory' (except those matching
`zk-directory-recursive-ignore-dir-regexp') and returns full
file-paths."
  (let* ((regexp (or regexp zk-id-regexp))
         (list
          (if (not zk-directory-recursive)
              (directory-files zk-directory full regexp)
            (directory-files-recursively
             zk-directory regexp nil
             (lambda (dir)
               (not (string-match
                     zk-directory-recursive-ignore-dir-regexp
                     dir)))))))
    (remq nil (mapcar
               (lambda (x)
                 (when (and (zk-file-p x)
                            (not (string-match-p
                                  "^[.]\\|[#|~]$"
                                  (file-name-nondirectory x))))
                   x))
               list))))

(defun zk--current-notes-list ()
  "Return list of files for currently open notes."
  (remq nil
        (mapcar
         (lambda (x)
           (when (and (buffer-file-name x)
                      (zk-file-p (buffer-file-name x)))
             (buffer-file-name x)))
         (buffer-list))))

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
  (let ((ids (zk--parse-file 'id (zk--grep-file-list str))))
    (if (stringp ids)
        (list ids)
      ids)))

(defun zk--grep-tag-list ()
  "Return list of tags from all notes in zk directory."
  (let* ((files (shell-command-to-string (concat
                                          "grep -ohir --include \\*."
                                          zk-file-extension
                                          " -e "
                                          (shell-quote-argument
                                           zk-tag-regexp)
                                          " "
                                          zk-directory " 2>/dev/null")))
         (list (split-string files "\n" t)))
    (delete-dups list)))

(defun zk--select-file (&optional prompt list)
  "Wrapper around `completing-read' to select zk-file.
Offers candidates from `zk--directory-files', or from LIST when
supplied. Can take a PROMPT argument."
  (let* ((files (or list
                    (zk--directory-files t))))
    (completing-read
     (or prompt
         "Select File: ")
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (group-function . zk--group-function)
             (category . zk-file))
         (complete-with-action action files string predicate)))
     nil t nil 'zk-file-history)))

(defun zk--group-function (file transform)
  "TRANSFORM completion candidate FILE to note title."
  (if transform
      (zk--parse-file 'title file)
    "zk"))

(defun zk--id-at-point ()
  "Return ID at point."
  (cond ((thing-at-point-looking-at zk-id-regexp)
         (match-string-no-properties 0))
        ((thing-at-point-looking-at zk-link-regexp)
         (match-string-no-properties 1))))

(defun zk--alist ()
  "Return an alist ID, title, and file-path triples."
  (mapcar (lambda (file)
            (when (zk-file-p file)
              (list (zk--parse-file 'id file)
                    (zk--parse-file 'title file)
                    file)))
          (zk--directory-files t)))

(defun zk--parse-id (target ids &optional zk-alist)
  "Return TARGET, either `file-path or `title, from files with IDS.
Takes a single ID, as a string, or a list of IDs. Takes an
optional ZK-ALIST, for efficiency if `zk--parse-id' is called
in an internal loop."
  (let* ((zk-alist (or zk-alist
                       (zk--alist)))
         (zk-id-list (zk--id-list nil zk-alist))
         (return
          (cond ((eq target 'file-path)
                 (cond ((stringp ids)
                        (if (member ids zk-id-list)
                            (cddr (assoc ids zk-alist))
                          (user-error "No file associated with %s" ids)))
                       ((listp ids)
                        (mapcar
                         (lambda (x)
                           (caddr (assoc x zk-alist)))
                         ids))))
                ((eq target 'title)
                 (cond ((stringp ids)
                        (if (member ids zk-id-list)
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

(defun zk--parse-file (target file-or-files)
  "Return TARGET, either `id or `title, from FILE-OR-FILES.
Takes a single file-path, as a string, or a list of file-paths.
On each file, call `zk--parse-file-function' and collect the results."
  (let ((result
         (mapcar (lambda (file)
                   (funcall zk--parse-file-function target file))
                 (if (listp file-or-files)
                     file-or-files
                   (list file-or-files)))))
    (if (null (cdr result))             ; list with single element
        (car result)
      result)))

(defun zk--parse-file-name (target file)
  "Return TARGET, either 'id or 'title, from the given FILE, a single
file-path, as a string.A note's title is understood to be the portion of its
filename following the zk ID, in the format `zk-id-regexp', and preceding the
file extension. This is the default value of `zk--parse-file-function'."
  (when (string-match (concat "\\(?1:"
                              zk-id-regexp
                              "\\).\\(?2:.*?\\)\\."
                              zk-file-extension)
                      file)
    (replace-regexp-in-string zk-file-name-separator " "
                              (match-string (pcase target
                                              ('id    1)
                                              ('title 2))
                                            file))))

;;; Buttons

(defun zk-setup-auto-link-buttons ()
  "Enable automatic link creation when zk-file is opened.
Adds `zk-make-link-buttons' to `find-file-hook.'"
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
  "Find `zk-link-regexp' before point and make it a zk-link button."
  (interactive)
  (save-excursion
    (re-search-backward zk-link-regexp (line-beginning-position))
    (make-button (match-beginning 1) (match-end 1)
                 'type 'zk-link)))

;;; Note Functions

;;;###autoload
(defun zk-new-note (&optional title)
  "Create a new note, insert link at point of creation.
Optional TITLE argument."
  (interactive)
  (let* ((pref-arg current-prefix-arg)
         (new-id (zk--generate-id))
         (orig-id (ignore-errors (zk--current-id)))
         (text (when (use-region-p)
                 (buffer-substring
                  (region-beginning)
                  (region-end))))
         (title (cond (title title)
                      ((use-region-p)
                       (with-temp-buffer
                         (insert text)
                         (goto-char (point-min))
                         (buffer-substring
                          (point)
                          (line-end-position))))
                      (t (read-string "Note title: "))))
         (body (when (use-region-p)
                 (with-temp-buffer
                   (insert text)
                   (goto-char (point-min))
                   (forward-line 2)
                   (buffer-substring
                    (point)
                    (point-max)))))
         (file-name (replace-regexp-in-string " "
                                              zk-file-name-separator
                                              (concat
                                               (format "%s/%s%s%s.%s"
                                                       zk-directory
                                                       new-id
                                                       zk-file-name-separator
                                                       title
                                                       zk-file-extension)))))
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
      (unless buffer-read-only
        (zk-insert-link new-id title)))
    (when buffer-file-name
      (save-buffer))
    (find-file file-name)
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
  (read-only-mode -1)
  (let* ((id (zk--current-id))
         (file-title (zk--parse-id 'title id))
         (header-title (progn
                         (save-excursion
                           (goto-char (point-min))
                           (re-search-forward (concat id "."))
                           (buffer-substring-no-properties
                            (point)
                            (line-end-position)))))
         (new-title))
    (if (not (string= file-title header-title))
        (if (y-or-n-p (format "Change from \"%s\" to \"%s\"? " file-title header-title))
            (setq new-title header-title)
          (setq new-title (read-string "New title: " file-title)))
      (setq new-title (read-string "New title: " file-title)))
    (when (string-match "\n" new-title)
      (setq new-title (replace-regexp-in-string "\n" "" new-title)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward id)
      (re-search-forward " ")
      (delete-region (point) (line-end-position))
      (insert new-title))
    (let ((new-file (concat
                     zk-directory "/"
                     id
                     zk-file-name-separator
                     (replace-regexp-in-string
                      " "
                      zk-file-name-separator
                      new-title)
                     "." zk-file-extension)))
      (rename-file buffer-file-name new-file t)
      (set-visited-file-name new-file t t)
      (save-buffer))))

;;; Find File

;;;###autoload
(defun zk-find-file ()
  "Find file in `zk-directory'."
  (interactive)
  (find-file (funcall zk-select-file-function "Find file: ")))

;;;###autoload
(defun zk-find-file-by-id (id)
  "Find file associated with ID."
  (find-file (zk--parse-id 'file-path id)))

;;;###autoload
(defun zk-find-file-by-full-text-search (str)
  "Find files containing regexp STR."
  (interactive
   (list (read-string "Search string: "
                      nil 'zk-search-history)))
  (let ((files (zk--grep-file-list str)))
    (if files
        (find-file (funcall zk-select-file-function
                    (format "Files containing \"%s\": " str) files))
      (user-error "No results for \"%s\"" str))))

;;;###autoload
(defun zk-current-notes ()
  "Select from list of currently open notes.
Optionally call a custom function by setting the variable
`zk-current-notes-function' to a function name."
  (interactive)
  (if zk-current-notes-function
      (funcall zk-current-notes-function)
    (find-file
     (funcall zk-select-file-function
      "Current Notes:"
      (zk--current-notes-list)))))

;;; Follow Links

;;;###autoload
(defun zk-follow-link-at-point (&optional id)
  "Open note that corresponds with the zk ID at point."
  (interactive)
  (let ((id (or (zk--id-at-point)
                id)))
    (if id
        (find-file (zk--parse-id 'file-path id))
      (error "No zk-link at point"))))

(defun zk--links-in-note-list ()
  "Return list of links in note with ID."
  (let ((id-list)
        (zk-ids (zk--id-list)))
    (save-buffer)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward zk-link-regexp nil t)
        (if (member (match-string-no-properties 1) zk-ids)
            (push (match-string-no-properties 1) id-list))))
    (cond ((null id-list)
           (error "No zk-links in note"))
          ((eq 1 (length id-list))
           (list (zk--parse-id 'file-path id-list)))
          (t
           (zk--parse-id 'file-path (delete-dups id-list))))))

;;;###autoload
(defun zk-links-in-note ()
  "Select from list of notes linked to in the current note."
  (interactive)
  (let* ((files (zk--links-in-note-list)))
    (if files
        (find-file (funcall zk-select-file-function "Links: " files))
      (user-error "No links found"))))

;;; Insert Link

;;;###autoload
(defun zk-insert-link (id &optional title)
  "Insert link to note with ID and optional TITLE.
By default, only a link is inserted. With prefix-argument, both
link and title are inserted. See variable `zk-link-and-title'
for additional configurations."
  (interactive (list (zk--parse-file 'id (funcall zk-select-file-function "Insert link: "))))
  (let* ((pref-arg current-prefix-arg)
         (title (or title
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
  "Insert zk ID and TITLE according to `zk-link-and-title-format'."
  (insert (format-spec zk-link-and-title-format
                       `((?i . ,id)(?t . ,title))))
  (when zk-enable-link-buttons
    (zk-make-button-before-point)))

;;; Completion at Point

(defun zk--format-candidates (&optional files format)
  "Return a list of FILES as formatted candidates, following FORMAT.

FORMAT must be a `format-spec' template, wherein `%i' is replaced
by the ID and `%t' by the title. It can be a string, such as \"%t
[[%i]]\", or a variable whose value is a string. If nil,
`zk-completion-at-point-format' will be used by default.

FILES must be a list of filepaths. If nil, all files in
`zk-directory' will be returned as formatted candidates."
  (let* ((format (or format
                     zk-completion-at-point-format))
         (list (or files
                   (zk--directory-files)))
         (output))
    (dolist (file list output)
      (let ((id (zk--parse-file 'id file))
            (title (zk--parse-file 'id file)))
        (when id
          (push (format-spec format
                             `((?i . ,id) (?t . ,title)))
                output))))))

(defun zk-completion-at-point ()
  "Completion-at-point function for zk-links.
When added to `completion-at-point-functions', typing two
brackets \"[[\" initiates completion."
  (let ((case-fold-search t)
        (origin (point)))
    (save-excursion
      (when (and (re-search-backward "\\[\\["
                                     (line-beginning-position)
                                     t)
                 (save-excursion
                   (not (search-forward "]]" origin t))))
        (list (match-end 0)
              origin
              (completion-table-dynamic
               (lambda (_)
                 (zk--format-candidates)))
              :exit-function
              (lambda (str _status)
                (delete-char (- -2 (length str)))
                (insert str)
                (when zk-enable-link-buttons
                  (zk-make-button-before-point))))))))

;;; Copy Link and Title

;;;###autoload
(defun zk-copy-link-and-title (&optional arg)
  "Copy link and title for id or file ARG at point."
  (interactive (list (funcall zk-select-file-function "Copy link: ")))
  (let* ((zk-id-list (zk--id-list))
         (id (cond ((member arg zk-id-list)
                    arg)
                   ((member (car arg) zk-id-list)
                    (car arg))
                   ((zk-file-p arg)
                    (zk--parse-file 'id arg))
                   (t (zk--id-at-point))))
         (title (zk--parse-id 'title id)))
    (if id
        (progn
          (kill-new (format-spec zk-link-and-title-format
                                 `((?i . ,id)(?t . ,title))))
          (message "Link and title copied: %s" title))
      (error "No valid zk-id"))))

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
        (find-file (funcall zk-select-file-function "Backlinks: " files))
      (user-error "No backlinks found"))))

;;; Search

;;;###autoload
(defun zk-search (string)
  "Search for STRING using function set in `zk-search-function'.
Defaults to `zk-grep.'"
  (interactive
   (list (read-string "Search: "
                      nil 'zk-search-history)))
  (funcall zk-search-function string))

(defun zk-grep (regexp)
  "Wrapper around `rgrep' to search for REGEXP in all notes.
Opens search results in a grep buffer."
  (interactive
   (list (read-string "zk-grep: "
                      nil 'zk-search-history)))
  (grep-compute-defaults)
  (rgrep regexp (concat "*." zk-file-extension) zk-directory nil))

;;; Tag Functions

;;;###autoload
(defun zk-tag-search (tag)
  "Open grep buffer containing results of search for TAG.
Select TAG, with completion, from list of all tags in zk notes.
Defaults to `zk-grep'."
  (interactive (list (completing-read "Tag: " (zk--grep-tag-list))))
  (funcall zk-tag-search-function tag))

;;;###autoload
(defun zk-tag-insert (tag)
  "Insert TAG at point.
Select TAG, with completion, from list of all tags in zk notes."
  (interactive (list (completing-read "Tag: " (zk--grep-tag-list))))
  (insert tag))

;;; Find Dead Links and Unlinked Notes

(defun zk--grep-link-id-list ()
  "Return list of all ids that appear as links in `zk-directory' files."
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
  "Search for dead links using `zk-search-function'."
  (interactive)
  (let* ((dead-link-ids (zk--dead-link-id-list)))
    (if dead-link-ids
        (funcall zk-search-function (mapconcat
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
        (find-file (funcall zk-select-file-function "Unlinked notes: " notes))
      (user-error "No unlinked notes found"))))


;;; zk-network - Backlinks and Forward Links Together

(defun zk-network ()
  "Find `zk-backlinks' and `zk-links-in-note' for current or selected note.
Backlinks and Links-in-Note are grouped separately."
  (interactive)
  (unless (zk-file-p)
    (user-error "Not a zk file"))
  (let* ((id (ignore-errors (zk--current-id)))
         (backlinks (ignore-errors (zk--backlinks-list id)))
         (links-in-note (ignore-errors (zk--links-in-note-list)))
         (resources))
    (if (or backlinks links-in-note)
        (progn
          (dolist (file backlinks)
            (push (propertize file 'type 'backlink) resources))
          (dolist (file links-in-note)
            ;; abbreviate-file-name allows a file to be in both groups
            (push (propertize (abbreviate-file-name file) 'type 'link) resources))
          (find-file
           (completing-read
            "Links: "
            (lambda (string predicate action)
              (if (eq action 'metadata)
                  `(metadata
                    (group-function . zk--network-group-function)
                    (display-sort-function . zk--network-sort-function)
                    (category . zk-file))
                (complete-with-action action resources string predicate))))))
      (user-error "No links found"))))

(defun zk--network-group-function (file transform)
  "Group FILE by type and TRANSFORM."
  (if transform
      (zk--parse-file 'title file)
    (cond
     ((eq 'backlink (get-text-property 0 'type file)) "Backlinks")
     ((eq 'link (get-text-property 0 'type file)) "Links-in-Note"))))

(defun zk--network-sort-function (list)
  "Sort LIST of links so Backlinks group is first."
  (sort list
        (lambda (a _b)
          (when (eq 'backlink (get-text-property 0 'type a))
              t))))

(provide 'zk)

;;; zk.el ends here
