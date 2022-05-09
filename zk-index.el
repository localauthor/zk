;;; zk-index.el --- Index and Desktop for zk   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 25, 2022
;; License: GPL-3.0-or-later
;; Version: 0.4
;; Homepage: https://github.com/localauthor/zk

;; Package-Requires: ((emacs "26.1")(zk "0.3"))

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

;; Two interfaces for zk:

;; ZK-Index: A sortable, searchable, narrowable, semi-persistent selection of
;; notes, in the form of clickable links.

;; ZK-Desktop: A place (or places) for collecting, grouping, arranging, and
;; saving curated selections of notes (also in the form of clickable links).

;; To enable integration with Embark, include '(zk-index-setup-embark)' in
;; your init config.

;;; Code:

(require 'zk)
(require 'view)

;;; Custom Variables

(defgroup zk-index nil
  "Index and Desktop interfaces for zk."
  :group 'text
  :group 'files
  :prefix "zk-index")

(defcustom zk-index-format-function 'zk-index--format-candidates
  "Default formatting function for ZK-Index candidates."
  :type 'function)

(defcustom zk-index-invisible-ids t
  "If non-nil, IDs will not be visible in the index."
  :type 'boolean)

(defcustom zk-index-format "%t [[%i]]"
  "Default format for candidates in the index."
    :type 'string)

(defcustom zk-index-prefix "-> "
  "String to prepend to note names in index and desktop."
    :type 'string)

(defcustom zk-index-auto-scroll t
  "Enable automatically showing note at point in ZK-Index."
  :type 'boolean)

(defcustom zk-index-desktop-directory nil
  "Directory for saved ZK-Desktops."
  :type 'directory)

(defcustom zk-index-desktop-basename "*ZK-Desktop:"
  "Basename for ZK-Desktops.
The names of all ZK-Desktops should begin with this string."
  :type 'string)

(defcustom zk-index-desktop-add-pos 'append
  "Behavior for placement of notes in ZK-Desktop via 'zk-index-send-to-desktop'.

Options:
1. 'append - Place notes at end of current ZK-Desktop
2. 'prepend - Place notes at beginning of current ZK-Desktop
3. 'at-point - Place notes at current point of current ZK-Desktop

To quickly change this setting, call 'zk-index-desktop-add-toggle'."
  :type '(choice (const :tag "Append" 'append)
                 (const :tag "Prepend" 'prepend)
                 (const :tag "At point" 'at-point)))

;;; ZK-Index Minor Mode Settings

(defvar-local zk-index-mode nil
  "Toggle 'zk-index-mode'.")

(defvar zk-index-mode-line '(:eval (zk-index-mode-line-text)))
(defvar zk-index-query-mode-line nil)
(defvar zk-index-last-query nil)
(defvar zk-index-last-focus-terms nil)
(defvar zk-index-last-search-terms nil)

(defun zk-index-mode (&optional ARG)
  "Toggle 'zk-index-mode'.
If called from Lisp, ARG should be 'toggle."
  (interactive (list 'toggle))
  (setq zk-index-mode
        (if (eq ARG 'toggle)
            (not zk-index-mode)
          (> ARG 0))))

(defun zk-index-mode-line-text ()
  "Set modeline for 'zk-index-mode'."
  zk-index-query-mode-line)

(defvar zk-index-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'zk-index-next-line)
    (define-key map (kbd "p") #'zk-index-previous-line)
    (define-key map (kbd "v") #'zk-index-view-note)
    (define-key map (kbd "o") #'other-window)
    (define-key map (kbd "f") #'zk-index-focus)
    (define-key map (kbd "s") #'zk-index-search)
    (define-key map (kbd "d") #'zk-index-send-to-desktop)
    (define-key map (kbd "D") #'zk-index-switch-to-desktop)
    (define-key map (kbd "S") #'zk-index-desktop-select)
    (define-key map (kbd "c") #'zk-index-current-notes)
    (define-key map (kbd "i") #'zk-index-refresh)
    (define-key map (kbd "M") #'zk-index-sort-modified)
    (define-key map (kbd "C") #'zk-index-sort-created)
    (define-key map (kbd "q") #'delete-window)
    (make-composed-keymap map tabulated-list-mode-map))
  "Keymap for ZK-Index buffer.")

(add-to-list 'minor-mode-map-alist (cons 'zk-index-mode zk-index-map))
(add-to-list 'minor-mode-alist `(zk-index-mode ,zk-index-mode-line))


;;; ZK-Desktop Minor Mode Settings

(defvar-local zk-index-desktop-mode nil
  "Toggle 'zk-index-desktop-mode'.")

(defvar zk-index-desktop-mode-line '(:eval (zk-index-desktop-mode-line-text)))

(defun zk-index-desktop-mode (&optional ARG)
  "Toggle 'zk-index-desktop-mode'.
If called from Lisp, ARG should be 'toggle."
  (interactive (list 'toggle))
  (setq zk-index-desktop-mode
        (if (eq ARG 'toggle)
            (not zk-index-desktop-mode)
          (> ARG 0))))

(defvar zk-index-desktop-mode-line-text nil)

(defun zk-index-desktop-mode-line-text ()
  "Set modeline for 'zk-index-desktop-mode'."
  zk-index-desktop-mode-line-text)

(defvar zk-index-desktop-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<up>") #'zk-index-move-line-up)
    (define-key map (kbd "C-<down>") #'zk-index-move-line-down)
    (define-key map (kbd "n") #'zk-index-next-line)
    (define-key map (kbd "p") #'zk-index-previous-line)
    (define-key map (kbd "v") #'zk-index-view-note)
    (define-key map (kbd "I") #'zk-index-switch-to-index)
    (define-key map (kbd "C-+") #'zk-index-desktop-edit-mode)
    (define-key map [remap newline] #'zk-index-desktop-newline)
    (define-key map (kbd "C-j") #'zk-index-desktop-newline)
    (define-key map (kbd "C-d") #'zk-index-desktop-delete-line)
    (define-key map [remap yank]  #'zk-index-desktop-yank)
    (define-key map [remap kill-line] #'zk-index-desktop-kill-line)
    (define-key map [remap kill-region] #'zk-index-desktop-kill-region)
    (define-key map [remap undo] #'zk-index-desktop-undo)
    (define-key map (kbd "v") #'zk-index-view-note)
    (define-key map (kbd "S") #'zk-index-desktop-select)
    (define-key map (kbd "o") #'other-window)
    (define-key map (kbd "q") #'delete-window)
    map)
  "Keymap for ZK-Desktop buffers.")

(add-to-list 'minor-mode-map-alist
             (cons 'zk-index-desktop-mode zk-index-desktop-map))
(add-to-list 'minor-mode-alist
             `(zk-index-desktop-mode ,zk-index-desktop-mode-line))


;;; Declarations

(defvar zk-index-last-sort-function nil)
(defvar zk-index-last-format-function nil)
(defvar zk-index-desktop-current nil)
(defvar zk-index-query-history nil)

(declare-function zk-file-p zk)
(declare-function zk--grep-id-list zk)


;;; Embark Integration

(defvar embark-multitarget-actions)
(defvar embark-target-finders)

(defun zk-index-setup-embark ()
  "Setup Embark integration for zk.
Adds zk-id as an Embark target, and adds 'zk-id-map' and
'zk-file-map' to embark-keymap-alist."
  (with-eval-after-load 'embark
    (add-to-list 'embark-multitarget-actions 'zk-index)
    (add-to-list 'embark-multitarget-actions 'zk-index-send-to-desktop)
    (add-to-list 'embark-multitarget-actions 'zk-copy-link-and-title)
    (add-to-list 'embark-multitarget-actions 'zk-follow-link-at-point)
    (add-to-list 'embark-target-finders 'zk-index-embark-target)
    (define-key zk-file-map (kbd "d") #'zk-index-send-to-desktop)
    (define-key zk-id-map (kbd "d") #'zk-index-send-to-desktop)))

(defun zk-index-embark-target ()
  "Target zk-id of button at point in ZK-Index and ZK-Desktop."
  (when (zk-index--button-at-point-p)
    (save-excursion
      (beginning-of-line)
      (re-search-forward zk-id-regexp (line-end-position)))
    (let ((zk-id (match-string-no-properties 1)))
      `(zk-id ,zk-id . ,(cons (line-beginning-position) (line-end-position))))))

;;; Formatting

(defun zk-index--format-candidates (&optional files format)
  "Return a list of FILES as formatted candidates, following FORMAT.

FORMAT must be a 'format-spec' template, wherein '%i' is replaced
by the ID and '%t' by the title. It can be a string, such as \"%t
[[%i]]\", or a variable whose value is a string. If nil,
'zk-completion-at-point-format' will be used by default.

FILES must be a list of filepaths. If nil, all files in
'zk-directory' will be returned as formatted candidates."
  (let* ((zk-index-format (if zk-index-invisible-ids "%t %i"
                            zk-index-format))
         (format (if format format
                   zk-index-format))
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
        (let ((id (if zk-index-invisible-ids
                      (propertize (match-string 1 file) 'invisible t)
                    (match-string 1 file)))
              (title (replace-regexp-in-string
                      zk-file-name-separator
                      " "
                      (match-string 2 file))))
          (when id
            (push (concat zk-index-prefix
                          (format-spec format
                                       `((?i . ,id)(?t . ,title))))
                  output)))))
    output))

;;; Main Stack

;;;###autoload
(defun zk-index (&optional files format-fn sort-fn)
  "Open ZK-Index, with optional FILES, FORMAT-FN, and SORT-FN."
  (interactive)
  (setq zk-index-last-format-function format-fn)
  (setq zk-index-last-sort-function sort-fn)
  (let ((inhibit-message t)
        (buffer "*ZK-Index*")
        (list (if files files
                (zk--directory-files t))))
    (unless (get-buffer buffer)
      (progn
        (when zk-default-backlink
          (unless (zk-file-p)
            (zk-find-file-by-id zk-default-backlink)))
        (generate-new-buffer buffer)
        (with-current-buffer buffer
          (zk-index--sort list format-fn sort-fn)
          (setq zk-index-mode t)
          (read-only-mode 1)
          (toggle-truncate-lines)
          (goto-char (point-min)))))
    (when files
      (zk-index-refresh files format-fn sort-fn))
      (pop-to-buffer buffer
                     '(display-buffer-at-bottom))))

(defun zk-index-refresh (&optional files format-fn sort-fn)
  "Refresh the index.
Optionally refresh with FILES, using FORMAT-FN and SORT-FN."
  (interactive)
  (let ((inhibit-message t)
        (files (if files files
                 (zk--directory-files t)))
        (sort-fn (if sort-fn sort-fn
                   (setq zk-index-last-sort-function nil)))
        (line))
    (with-current-buffer "*ZK-Index*"
      (setq line (line-number-at-pos))
      (read-only-mode -1)
      (erase-buffer)
      (zk-index--sort files format-fn sort-fn)
      (goto-char (point-min))
      (setq zk-index-mode t)
      (toggle-truncate-lines 1)
      (unless (zk-index-narrowed-p)
        (progn
          (zk-index--clear-query-mode-line)
          (forward-line line)))
      (read-only-mode))))

(defun zk-index--sort (files &optional format-fn sort-fn)
  "Sort FILES, with option FORMAT-FN and SORT-FN."
  (let* ((sort-fn (if sort-fn sort-fn
                    'zk-index--sort-modified))
         (files (if (eq 1 (length files))
                    files
                  (nreverse (funcall sort-fn files)))))
    (funcall #'zk-index--format files format-fn)))

(defun zk-index--format (files &optional format-fn)
  "Format FILES with optional custom FORMAT-FN."
  (let* ((format-fn (if format-fn format-fn
                      zk-index-format-function))
         (candidates (funcall format-fn files)))
    (zk-index--insert candidates)))

(defun zk-index--insert (candidates)
  "Insert CANDIDATES into ZK-Index."
  (dolist (file candidates)
    (string-match zk-id-regexp file)
    (let ((id (match-string 0 file)))
      (insert-text-button file
                          'type 'zk-index
                          'follow-link t
                          'face 'default
                          'action
                          (lambda (_)
                            (find-file-other-window
                             (zk--parse-id 'file-path
                                           id)))
                          'help-echo (lambda (_win _obj _pos)
                                       (format
                                        "%s"
                                        (zk--parse-id
                                         'title
                                         id)))))
    (newline))
  (message "Notes: %s" (length candidates)))

(eval-and-compile
  (define-button-type 'zk-index
    'follow-link t
    'face 'default))

(defun zk-index-narrowed-p ()
  "Return t when index is narrowed."
  (with-current-buffer "*ZK-Index*"
    (if (< (count-lines (point-min) (point-max))
           (length (zk--id-list)))
        t nil)))

;;; Index Search and Focus Functions

;;;; Index Search
;; narrow index based on search of notes' full text

(defun zk-index-search ()
  "Narrow index based on regexp search of note contents."
  (interactive)
  (zk-index-refresh
   (zk-index-query-files)
   zk-index-last-format-function
   zk-index-last-sort-function))

;;;; Index Focus
;; narrow index based on search of note titles (case sensitive)
;; an alternative to consult-focus-lines

(defun zk-index-focus ()
  "Narrow index based on regexp search of note titles."
  (interactive)
  (zk-index-refresh
   (zk-index-query-files)
   zk-index-last-format-function
   zk-index-last-sort-function))

;;;; Low-level Query Functions

(defun zk-index-query-files ()
  "Return narrowed list of notes, based on focus or search query."
  (let* ((command this-command)
         (scope (if (zk-index-narrowed-p)
                    (zk-index--current-id-list)
                  (progn
                    (setq zk-index-last-query nil)
                    (zk--id-list))))
         (string (read-string
                  (cond
                   ((eq command 'zk-index-focus)
                    "Focus: ")
                   ((eq command 'zk-index-search)
                    "Search: "))
                  nil 'zk-index-query-history))
         (query (cond
                 ((eq command 'zk-index-focus)
                  (zk--id-list string))
                 ((eq command 'zk-index-search)
                  (zk--grep-id-list string))))
         (mode-line
          (cond ((eq command 'zk-index-focus)
                 (zk-index-focus-mode-line string))
                ((eq command 'zk-index-search)
                 (zk-index-search-mode-line string))))
         (ids
          (mapcar
           (lambda (x)
             (when (member x scope)
               x))
           query))
         (files (zk--parse-id 'file-path (remq nil ids))))
    (add-to-history 'zk-index-query-history string)
    (when files
      (setq zk-index-query-mode-line mode-line))
    (when (stringp files)
      (setq files (list files)))
    (if files files
      (error "No matches for \"%s\"" string))))

(defun zk-index-focus-mode-line (string)
  "Add STRING to modeline for 'zk-index-focus'."
  (cond
   ;;same
   ((eq zk-index-last-query 'focus)
    ;;outcome
    (setq zk-index-last-focus-terms
          (if zk-index-last-focus-terms
              (concat zk-index-last-focus-terms "\" + \"" string)
            string))
    (concat " [ZK-Focus: \"" zk-index-last-focus-terms "\"]"))
   ;;mix
   ((eq zk-index-last-query 'search)
    ;;outcome
    (setq zk-index-last-query 'focus)
    (setq zk-index-last-focus-terms
          (if zk-index-last-focus-terms
              (concat zk-index-last-focus-terms "\" + \"" string)
            string))
    (concat " [ZK-Search: \"" zk-index-last-search-terms "\" |"
            " ZK-Focus: \"" zk-index-last-focus-terms "\"]"))
   ;;neither
   ((not zk-index-last-query)
    ;; outcome
    (setq zk-index-last-query 'focus)
    (setq zk-index-last-focus-terms string)
    (concat " [ZK-Focus: \"" string "\"]"))))

(defun zk-index-search-mode-line (string)
  "Add STRING to modeline for 'zk-index-search'."
  (cond
   ;;same
   ((eq zk-index-last-query 'search)
    ;;outcome
    (setq zk-index-last-search-terms
          (if zk-index-last-search-terms
              (concat zk-index-last-search-terms "\" + \"" string)
            string))
    (concat " [ZK-Search: \"" zk-index-last-search-terms "\"]"))
   ;;mix
   ((eq zk-index-last-query 'focus)
    ;;outcome
    (setq zk-index-last-query 'search)
    (setq zk-index-last-search-terms
          (if zk-index-last-search-terms
              (concat zk-index-last-search-terms "\" + \"" string)
            string))
    (concat " [ZK-Focus: \"" zk-index-last-focus-terms "\" |"
            " ZK-Search: \"" zk-index-last-search-terms "\"]"))
   ;;neither
   ((not zk-index-last-query)
    ;; outcome
    (setq zk-index-last-query 'search)
    (setq zk-index-last-search-terms string)
    (concat " [ZK-Search: \"" string "\"]"))))

(defun zk-index--clear-query-mode-line ()
  "Clear query modeline."
  (setq zk-index-query-mode-line nil
        zk-index-last-focus-terms nil
        zk-index-last-search-terms nil))

(defun zk-index--current-id-list ()
  "Return list of IDs for current index, as filepaths."
  (let (ids)
    (with-current-buffer "*ZK-Index*"
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (while (re-search-forward zk-id-regexp nil t)
            (push (match-string-no-properties 0) ids)))
        ids))))

;;; Index Sort Functions

(defun zk-index-sort-modified ()
  "Sort index by last modified."
  (interactive)
  (zk-index-refresh (zk-index--current-file-list)
                    zk-index-last-format-function
                    #'zk-index--sort-modified))

(defun zk-index-sort-created ()
  "Sort index by date created."
  (interactive)
  (zk-index-refresh (zk-index--current-file-list)
                    zk-index-last-format-function
                    #'zk-index--sort-created))

(defun zk-index--current-file-list ()
  "Return list files in current index."
  (let* ((ids (zk-index--current-id-list))
         (files (zk--parse-id 'file-path ids)))
    (when files
      files)))

(defun zk-index--sort-created (list)
  "Sort LIST for latest created."
  (let ((ht (make-hash-table :test #'equal :size 5000)))
    (dolist (x list)
      (puthash x (zk--parse-file 'id x) ht))
    (sort list
          (lambda (a b)
            (let ((one
                   (gethash a ht))
                  (two
                   (gethash b ht)))
              (string< two one))))))

(defun zk-index--sort-modified (list)
  "Sort LIST for latest modification."
  (let ((ht (make-hash-table :test #'equal :size 5000)))
    (dolist (x list)
      (puthash x (file-attribute-modification-time (file-attributes x)) ht))
    (sort list
          (lambda (a b)
            (let ((one
                   (gethash a ht))
                  (two
                   (gethash b ht)))
              (time-less-p two one))))))

(defun zk-index--sort-size (list)
  "Sort LIST for latest modification."
  (sort list
        (lambda (a b)
          (let ((one
                 (file-attribute-size (file-attributes a)))
                (two
                 (file-attribute-size (file-attributes b))))
            (time-less-p two one)))))

;;; ZK-Index Keymap Commands

(defun zk-index-open-note ()
  "Open note."
  (interactive)
  (let ((buffer (current-buffer)))
    (push-button nil t)
    (select-window (get-buffer-window buffer))))

(defun zk-index-view-note ()
  "View note in view mode."
  (interactive)
  (let ((buffer (current-buffer)))
    (push-button nil t)
    (view-mode)
    (select-window (get-buffer-window buffer))))

(defun zk-index-current-notes ()
  "Open zk-index listing currently open notes."
  (interactive)
  (zk-index
   (zk--current-notes-list)
   zk-index-last-format-function
   zk-index-last-sort-function))

(defun zk-index--button-at-point-p ()
  "Return t when `zk-index' button is at point."
  (let ((button (button-at (point))))
    (when (and button
               (eq (button-type button) 'zk-index))
      t)))

(defun zk-index-next-line ()
  "Move to next line.
If 'zk-index-auto-scroll' is non-nil, show note in other window."
  (interactive)
  (let ((buffer (current-buffer))
        (split-width-threshold nil))
    (if zk-index-auto-scroll
        (progn
          (other-window 1)
          (when (and (zk-file-p)
                     view-mode
                     (not (buffer-modified-p)))
            (kill-buffer))
          (if (get-buffer-window buffer)
              (other-window -1)
            (select-window (get-buffer-window buffer)))
          (forward-button 1)
          (unless (looking-at-p "[[:space:]]*$")
            (zk-index-view-note)))
      (forward-button 1))))

(defun zk-index-previous-line ()
  "Move to previous line.
If 'zk-index-auto-scroll' is non-nil, show note in other window."
  (interactive)
  (let ((buffer (current-buffer))
        (split-width-threshold nil))
    (if zk-index-auto-scroll
        (progn
          (other-window 1)
          (when (and (zk-file-p)
                     view-mode
                     (not (buffer-modified-p)))
            (kill-buffer))
          (if (get-buffer-window buffer)
              (other-window -1)
            (select-window (get-buffer-window buffer)))
          (forward-button -1)
          (unless (looking-at-p "[[:space:]]*$")
            (zk-index-view-note)))
      (forward-button -1))))

(defun zk-index-move-line-down ()
  "Move line at point down in 'read-only-mode'."
  (interactive)
  (read-only-mode -1)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (read-only-mode))

(defun zk-index-move-line-up ()
  "Move line at point up in 'read-only-mode'."
  (interactive)
  (read-only-mode -1)
  (transpose-lines 1)
  (forward-line -2)
  (read-only-mode))

;;; ZK-Desktop
;; index's more flexible, savable cousin; a place to collect and order notes
;; in the form of links

;;;###autoload
(defun zk-index-desktop ()
  "Open ZK-Desktop."
  (interactive)
  (let ((buffer (if (and zk-index-desktop-current
                         (buffer-live-p (get-buffer zk-index-desktop-current)))
                    zk-index-desktop-current
                  (zk-index-desktop-select)))
        (choice (read-char "Choice: \[s\]witch or \[p\]op-up?")))
    (pcase choice
      ('?s (switch-to-buffer buffer))
      ('?p (pop-to-buffer buffer
                          '(display-buffer-at-bottom)))
      (_ (zk-index-desktop)))))

;;;###autoload
(defun zk-index-desktop-select ()
  "Select a ZK-Desktop to work with."
  (interactive)
  (let* ((last-command last-command)
         (desktop
          (completing-read "Select or Create ZK-Desktop: "
                           (directory-files
                            zk-index-desktop-directory
                            nil
                            (concat
                             zk-index-desktop-basename
                             ".*"))
                           nil nil zk-index-desktop-basename))
         (file (concat zk-index-desktop-directory "/" desktop)))
    (if (file-exists-p (expand-file-name file))
        (setq zk-index-desktop-current
              (find-file-noselect file))
      (progn
        (generate-new-buffer desktop)
        (setq zk-index-desktop-current desktop)))
    (with-current-buffer zk-index-desktop-current
      (setq require-final-newline 'visit-save)
      (zk-index-desktop-make-buttons)
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))
      (set-visited-file-name file t t)
      (setq zk-index-desktop-mode t)
      (save-buffer))
    (if (and (not (eq last-command 'zk-index-desktop))
             (y-or-n-p (format "Visit %s? " zk-index-desktop-current)))
        (progn
          (switch-to-buffer zk-index-desktop-current)
          (setq zk-index-desktop-mode t))
      (message "Desktop set to: %s" zk-index-desktop-current)))
  zk-index-desktop-current)

;;;###autoload
(defun zk-index-desktop-make-buttons ()
  "Re-make buttons in ZK-Desktop."
  (interactive)
  (when (and (string-match-p zk-index-desktop-basename (buffer-name))
             (file-in-directory-p default-directory zk-index-desktop-directory))
    (let ((ids (zk--id-list))
          (zk-alist (zk--alist)))
      (save-excursion
        (read-only-mode -1)
        (goto-char (point-min))
        (while (re-search-forward zk-id-regexp nil t)
          (let* ((beg (line-beginning-position))
                 (end (line-end-position))
                 (id (match-string-no-properties 1))
                 (title (string-trim-left
                         (buffer-substring-no-properties beg (match-beginning 0))
                         zk-index-prefix))
                 (new-title (concat (zk--parse-id 'title id zk-alist) " ")))
            (when (member id ids)
              (beginning-of-line)
              (unless (string= title new-title)
                (progn
                  (search-forward title end)
                  (replace-match new-title)
                  (setq end (line-end-position))))
              (when zk-index-invisible-ids
                ;; find zk-links and plain zk-ids
                (if (re-search-forward zk-link-regexp (line-end-position) t)
                    (replace-match
                     (propertize (match-string 0) 'invisible t) nil t)
                  (progn
                    (re-search-forward id)
                    (replace-match
                     (propertize id 'invisible t)))))
              (make-text-button beg end 'type 'zk-index
                                'action (lambda (_)
                                          (find-file-other-window
                                           (zk--parse-id 'file-path
                                                         id)))
                                'help-echo (lambda (_win _obj _pos)
                                             (format
                                              "%s"
                                              (zk--parse-id
                                               'title
                                               id))))
              (goto-char (match-end 0)))))
        (read-only-mode)))))

;;;###autoload
(defun zk-index-send-to-desktop (&optional files)
  "Send notes from ZK-Index to ZK-Desktop.
In ZK-Index, works on note at point or notes in active region.
Also works on FILES or group of files in minibuffer, and on zk-id
at point."
  (interactive)
  (unless zk-index-desktop-directory
    (error "Please set 'zk-index-desktop-directory'"))
  (let ((buffer) (items))
    (cond ((eq 1 (length files))
           (unless
               (ignore-errors
                 (setq items (car (funcall zk-index-format-function files))))
             (setq items
                 (car
                  (funcall
                   zk-index-format-function
                   (list (zk--parse-id 'file-path files)))))))
          ((and files
                (< 1 (length files)))
           (setq items
                 (mapconcat
                  #'identity
                  (funcall zk-index-format-function files) "\n")))
          ((string= (buffer-name) "*ZK-Index*")
           (progn
             (read-only-mode -1)
             (setq items (if (use-region-p)
                             (buffer-substring
                              (save-excursion
                                (goto-char (region-beginning))
                                (line-beginning-position))
                              (save-excursion
                                (goto-char (region-end))
                                (line-end-position)))
                           (buffer-substring
                            (line-beginning-position)
                            (line-end-position))))
             (read-only-mode)))
          ((zk-file-p)
           (setq items
                 (car
                  (funcall
                   zk-index-format-function
                   (list (zk--parse-id 'file-path (zk--current-id))))))))
    (if (and zk-index-desktop-current
             (buffer-live-p (get-buffer zk-index-desktop-current)))
        (setq buffer zk-index-desktop-current)
      (setq buffer (zk-index-desktop-select)))
    (unless (get-buffer buffer)
      (generate-new-buffer buffer))
    (with-current-buffer buffer
      (read-only-mode -1)
      (setq zk-index-desktop-mode t)
      (setq require-final-newline 'visit-save)
      (pcase zk-index-desktop-add-pos
        ('append (goto-char (point-max)))
        ('prepend (progn
                    (goto-char (point-min))))
        ('at-point (goto-char (point))))
      (insert "\n" items "\n")
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))
      (pcase zk-index-desktop-add-pos
        ('append (progn
                   (goto-char (point-max))
                   (beginning-of-line)))
        ('prepend (goto-char (point-min)))
        ('at-point (goto-char (point))))
      (zk-index-desktop-make-buttons)
      (read-only-mode))
    (if (string= (buffer-name) "*ZK-Index*")
        (message "Sent to %s - press D to switch" buffer)
      (message "Sent to %s" buffer))))

(defun zk-index-desktop-add-toggle ()
  "Set 'zk-index-desktop-add-pos' interactively."
  (interactive)
   (let ((choice (read-char "Choice: \[a\]ppend; \[p\]repend; at-\[P\]oint")))
     (pcase choice
      ('?a (setq zk-index-desktop-add-pos 'append))
      ('?p (setq zk-index-desktop-add-pos 'prepend))
      ('?P (setq zk-index-desktop-add-pos 'at-point)))))

;;;###autoload
(defun zk-index-switch-to-index ()
  "Switch to ZK-Index buffer."
  (interactive)
  (let ((buffer "*ZK-Index*"))
    (unless (get-buffer buffer)
      (progn
        (generate-new-buffer buffer)
        (zk-index-refresh)))
    (switch-to-buffer buffer)))

;;;###autoload
(defun zk-index-switch-to-desktop ()
  "Switch to ZK-Desktop.
With prefix-argument, raise ZK-Desktop in other frame."
  (interactive)
  (unless (and zk-index-desktop-current
               (buffer-live-p (get-buffer zk-index-desktop-current)))
    (zk-index-desktop-select))
  (let ((buffer zk-index-desktop-current))
    (if current-prefix-arg
        (if (get-buffer-window buffer 'visible)
            (display-buffer-pop-up-frame
             buffer
             ;; not general
             '((pop-up-frame-parameters . ((top . 80)
                                           (left . 850)
                                           (width . 80)
                                           (height . 35)))))
          (switch-to-buffer-other-frame buffer))
      (switch-to-buffer buffer))))

;;; ZK-Desktop Keymap Commands

(defun zk-index-desktop-newline ()
  "Insert new line in 'zk-index-desktop-mode'."
  (interactive)
  (zk-index-desktop-edit-mode)
  (newline)
  (zk-index-desktop-edit-mode))

(defun zk-index-desktop-undo ()
  "Undo in 'zk-index-desktop-mode'."
  (interactive)
  (zk-index-desktop-edit-mode)
  (undo)
  (zk-index-desktop-edit-mode))

(defun zk-index-desktop-delete-line ()
  "Delete line in 'zk-index-desktop-mode'."
  (interactive)
  (when (bolp)
    (if (save-excursion
          (beginning-of-line)
          (looking-at-p "[[:space:]]*$"))
        (progn
          (zk-index-desktop-edit-mode)
          (delete-char 1)
          (zk-index-desktop-edit-mode))
      (progn
        (zk-index-desktop-edit-mode)
        (delete-region (line-beginning-position)
                       (line-end-position))
        (zk-index-desktop-edit-mode)))))

(defun zk-index-desktop-kill-line ()
  "Kill line in 'zk-index-desktop-mode'."
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at-p "[[:space:]]*$"))
      (progn
        (zk-index-desktop-edit-mode)
        (kill-line)
        (zk-index-desktop-edit-mode))
    (progn
      (zk-index-desktop-edit-mode)
      (kill-region (line-beginning-position)
                     (line-end-position))
      (zk-index-desktop-edit-mode))))

(defun zk-index-desktop-kill-region ()
  "Kill-region in 'zk-indexk-desktop-mode'."
  (interactive)
  (progn
    (zk-index-desktop-edit-mode)
    (kill-region (region-beginning)(region-end))
    (beginning-of-line)
    (zk-index-desktop-edit-mode)))

(defun zk-index-desktop-yank ()
  "Yank in 'zk-index-desktop-mode'."
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at-p "[[:space:]]*$"))
    (progn
      (zk-index-desktop-edit-mode)
      (yank)
      (beginning-of-line)
      (zk-index-desktop-edit-mode))))

(defun zk-index-desktop-edit-mode ()
  "Toggle 'read-only-mode' in ZK-Desktop.
If toggled on via key binding, the same key binding toggles off."
  (interactive)
  (let ((key (vector last-input-event))
        (modeline (default-value 'mode-line-misc-info)))
    (if zk-index-desktop-mode
        (progn
          (read-only-mode -1)
          (local-set-key key #'zk-index-desktop-edit-mode)
          ;; other way to disable keymap?
          (setq zk-index-desktop-mode nil)
          (setq-local mode-line-misc-info "ZK-Desktop Edit Mode"))
      (progn
        (read-only-mode)
        (local-unset-key key)
        (setq zk-index-desktop-mode t))
      (setq-local mode-line-misc-info modeline))))

(provide 'zk-index)

;;; zk-index.el ends here
