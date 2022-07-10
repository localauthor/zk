;;; zk-index.el --- Index and Desktop for zk   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 25, 2022
;; License: GPL-3.0-or-later
;; Version: 0.7
;; Homepage: https://github.com/localauthor/zk

;; Package-Requires: ((emacs "27.1")(zk "0.3"))

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
(require 'hl-line)

;;; Custom Variables

(defgroup zk-index nil
  "Index and Desktop interfaces for zk."
  :group 'text
  :group 'files
  :prefix "zk-index")

(defcustom zk-index-buffer-name "*ZK-Index*"
  "Name for ZK-Index buffer."
  :type 'string)

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
  "String to prepend to note names in ZK-Index."
    :type 'string)

(defcustom zk-index-auto-scroll t
  "Enable automatically showing note at point in ZK-Index."
  :type 'boolean)

(defcustom zk-index-view-hide-cursor t
  "Hide cursor in `zk-index-view-mode'."
  :type 'boolean)

(defcustom zk-index-button-display-function 'zk-index-button-display-action
  "Function called when buttons pressed in ZK-Index and ZK-Desktop.
The function is called by `zk-index-button-action'. A custom
function must take two arguments, FILE and BUFFER respectively.
See the default function `zk-index-button-display-action' for an
example."
  :type 'function)

(defcustom zk-index-desktop-directory nil
  "Directory for saved ZK-Desktops."
  :type 'directory)

(defcustom zk-index-desktop-basename "*ZK-Desktop:"
  "Basename for ZK-Desktops.
The names of all ZK-Desktops should begin with this string."
  :type 'string)

(defcustom zk-index-desktop-prefix ""
  "String to prepend to note names in ZK-Desktop."
  :type 'string)

(defcustom zk-index-desktop-major-mode nil
  "Name of major-mode for ZK-Desktop buffers.
The value should be a symbol that is a major mode command.
If nil, buffers will be in `fundamental-mode'."
  :type 'function)

(defcustom zk-index-desktop-add-pos 'append
  "Behavior for placement of notes in ZK-Desktop via `zk-index-send-to-desktop'.

Options:
1. `append - Place notes at end of current ZK-Desktop
2. `prepend - Place notes at beginning of current ZK-Desktop
3. `at-point - Place notes at current point of current ZK-Desktop

To quickly change this setting, call `zk-index-desktop-add-toggle'."
  :type '(choice (const :tag "Append" append)
                 (const :tag "Prepend" prepend)
                 (const :tag "At point" at-point)))

(defface zk-index-desktop-button
  '((t :inherit default))
  "Face used for buttons in `zk-index-desktop-mode'.")

;;; ZK-Index Major Mode Settings

(defvar zk-index-last-query nil)
(defvar zk-index-last-focus-terms nil)
(defvar zk-index-last-search-terms nil)
(defvar zk-index-mode-line-orig nil)

(defvar zk-index-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'zk-index-next-line)
    (define-key map (kbd "p") #'zk-index-previous-line)
    (define-key map (kbd "v") #'zk-index-view-note)
    (define-key map (kbd "o") #'other-window)
    (define-key map (kbd "f") #'zk-index-focus)
    (define-key map (kbd "s") #'zk-index-search)
    (define-key map (kbd "d") #'zk-index-send-to-desktop)
    (define-key map (kbd "D") #'zk-index-switch-to-desktop)
    (define-key map (kbd "c") #'zk-index-current-notes)
    (define-key map (kbd "i") #'zk-index-refresh)
    (define-key map (kbd "S") #'zk-index-sort-size)
    (define-key map (kbd "M") #'zk-index-sort-modified)
    (define-key map (kbd "C") #'zk-index-sort-created)
    (define-key map (kbd "RET") #'zk-index-open-note)
    (define-key map (kbd "q") #'delete-window)
    (make-composed-keymap map tabulated-list-mode-map))
  "Keymap for ZK-Index buffer.")

(define-derived-mode zk-index-mode nil "ZK-Index"
  "Mode for `zk-index'.
\\{zk-index-mode-map}"
  (setq mode-name "ZK-Index")
  (setq zk-index-mode-line-orig mode-line-misc-info)
  (read-only-mode)
  (hl-line-mode)
  (make-local-variable 'show-paren-mode)
  (setq-local show-paren-mode nil)
  (setq cursor-type nil))

;;; ZK-Desktop Minor Mode Settings

(defvar zk-index-desktop-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<up>") #'zk-index-move-line-up)
    (define-key map (kbd "C-<down>") #'zk-index-move-line-down)
    (define-key map (kbd "C-k") #'zk-index-desktop-kill-line)
    (define-key map (kbd "C-d") #'zk-index-desktop-delete-char)
    (define-key map (kbd "DEL") #'zk-index-desktop-delete-backward-char)
    (define-key map (kbd "C-w") #'zk-index-desktop-kill-region)
    (define-key map (kbd "C-y") #'zk-index-desktop-yank)
    ;; (define-key map (kbd "I") #'zk-index-switch-to-index)
    ;; (define-key map (kbd "S") #'zk-index-desktop-select)
    map)
  "Keymap for ZK-Desktop buffers.")

(define-minor-mode zk-index-desktop-mode
  "Minor mode for `zk-index-desktop'."
  :init-value nil
  :keymap zk-index-desktop-map
  (when zk-index-desktop-mode
    (zk-index-desktop-make-buttons)
    (when-let ((mode zk-index-desktop-major-mode))
      (funcall mode)
      (use-local-map zk-index-desktop-map))
    (setq truncate-lines t)))

(defvar zk-index-desktop-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'zk-index-view-note)
    (define-key map (kbd "n") #'zk-index-next-line)
    (define-key map (kbd "p") #'zk-index-previous-line)
    (define-key map [remap self-insert-command] 'ignore)
    (define-key map (kbd "C-k") #'zk-index-desktop-kill-line)
    (define-key map (kbd "C-d") #'zk-index-desktop-delete-char)
    (define-key map (kbd "C-w") #'zk-index-desktop-kill-region)
    (set-keymap-parent map button-map)
    map)
  "Keymap for ZK-Desktop buttons.")

;;; Declarations

(defvar zk-index-last-sort-function nil)
(defvar zk-index-last-format-function nil)
(defvar zk-index-query-mode-line nil)
(defvar zk-index-desktop-current nil)
(defvar zk-index-query-history nil)

(declare-function zk-file-p zk)
(declare-function zk--grep-id-list zk)


;;; Embark Integration

(defvar embark-multitarget-actions)
(defvar embark-target-finders)
(defvar embark-exporters-alist)

(defun zk-index-setup-embark ()
  "Setup Embark integration for zk.
Adds zk-id as an Embark target, and adds `zk-id-map' and
`zk-file-map' to `embark-keymap-alist'."
  (with-eval-after-load 'embark
    (add-to-list 'embark-multitarget-actions 'zk-index)
    (add-to-list 'embark-multitarget-actions 'zk-index-send-to-desktop)
    (add-to-list 'embark-multitarget-actions 'zk-copy-link-and-title)
    (add-to-list 'embark-multitarget-actions 'zk-follow-link-at-point)
    (add-to-list 'embark-target-finders 'zk-index-embark-target)
    (add-to-list 'embark-exporters-alist '(zk-file . zk-index))
    (define-key zk-file-map (kbd "d") #'zk-index-send-to-desktop)
    (define-key zk-id-map (kbd "d") #'zk-index-send-to-desktop)
    (define-key zk-id-map (kbd "i") #'zk-index-insert-link)))

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

FORMAT must be a `format-spec' template, wherein `%i' is replaced
by the ID and `%t' by the title. It can be a string, such as \"%t
[[%i]]\", or a variable whose value is a string. If nil,
`zk-completion-at-point-format' will be used by default.

FILES must be a list of filepaths. If nil, all files in
`zk-directory' will be returned as formatted candidates."
  (let* ((zk-index-format (if zk-index-invisible-ids "%t %i"
                            zk-index-format))
         (format (or format
                     zk-index-format))
         (list (or files
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
            (push (format-spec format
                               `((?i . ,id)(?t . ,title)))
                  output)))))
    output))

;;; Main Stack

;;;###autoload
(defun zk-index (&optional files format-fn sort-fn buf-name)
  "Open ZK-Index, with optional FILES, FORMAT-FN, SORT-FN, BUF-NAME."
  (interactive)
  (setq zk-index-last-format-function format-fn)
  (setq zk-index-last-sort-function sort-fn)
  (let ((inhibit-message t)
        (buf-name (or buf-name
                      zk-index-buffer-name))
        (list (or files
                  (zk--directory-files t))))
    (if (not (get-buffer buf-name))
        (progn
          (when zk-default-backlink
            (unless (zk-file-p)
              (zk-find-file-by-id zk-default-backlink)))
          (generate-new-buffer buf-name)
          (with-current-buffer buf-name
            (zk-index--sort list format-fn sort-fn)
            (zk-index-mode)
            (setq truncate-lines t)
            (goto-char (point-min)))
          (pop-to-buffer buf-name
                         '(display-buffer-at-bottom)))
      (when files
        (zk-index-refresh files format-fn sort-fn buf-name))
      (pop-to-buffer buf-name
                     '(display-buffer-at-bottom)))))

(defun zk-index-refresh (&optional files format-fn sort-fn buf-name)
  "Refresh the index.
Optionally refresh with FILES, using FORMAT-FN, SORT-FN, BUF-NAME."
  (interactive)
  (let ((inhibit-message t)
        (inhibit-read-only t)
        (files (or files
                   (zk--directory-files t)))
        (sort-fn (or sort-fn
                     (setq zk-index-last-sort-function nil)))
        (buf-name (or buf-name
                      zk-index-buffer-name))
        (line))
    (with-current-buffer buf-name
      (setq line (line-number-at-pos))
      (erase-buffer)
      (zk-index--sort files format-fn sort-fn)
      (goto-char (point-min))
      (setq truncate-lines t)
      (unless (zk-index-narrowed-p buf-name)
        (progn
          (zk-index--reset-mode-line)
          (zk-index--reset-mode-name)
          (forward-line line))))))

(defun zk-index--sort (files &optional format-fn sort-fn)
  "Sort FILES, with option FORMAT-FN and SORT-FN."
  (let* ((sort-fn (or sort-fn
                      'zk-index--sort-modified))
         (files (if (eq 1 (length files))
                    files
                  (nreverse (funcall sort-fn files)))))
    (funcall #'zk-index--format files format-fn)))

(defun zk-index--format (files &optional format-fn)
  "Format FILES with optional custom FORMAT-FN."
  (let* ((format-fn (or format-fn
                        zk-index-format-function))
         (candidates (funcall format-fn files)))
    (zk-index--insert candidates)))

(eval-and-compile
  (define-button-type 'zk-index
    'follow-link t
    'face 'default))

(defun zk-index--insert (candidates)
  "Insert CANDIDATES into ZK-Index."
  (dolist (file candidates)
    (insert (concat zk-index-prefix file "\n")))
  (goto-char (point-min))
  (zk-index-make-buttons)
  (message "Notes: %s" (length candidates)))

;;;###autoload
(defun zk-index-make-buttons ()
  "Make buttons in ZK-Index."
  (interactive)
  (let ((inhibit-read-only t)
        (ids (zk--id-list)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward zk-id-regexp nil t)
        (let* ((beg (line-beginning-position))
               (end (line-end-position))
               (id (match-string-no-properties 1)))
          (when (member id ids)
            (beginning-of-line)
            (make-text-button beg end
                              'type 'zk-index
                              'action 'zk-index-button-action
                              'help-echo 'zk-index-help-echo)
            (when zk-index-invisible-ids
              (beginning-of-line)
              (re-search-forward id)
              (replace-match
               (propertize id 'invisible t))
              (goto-char (match-end 0)))))))))

;;;; Utilities

(defun zk-index-button-display-action (file buffer)
  "Function to display FILE or BUFFER on button press in Index and Desktop."
  (if (file-in-directory-p zk-index-desktop-directory
                           default-directory)
      ;; display action for ZK-Desktop
      (progn
        (if (one-window-p)
            (pop-to-buffer buffer
                           (display-buffer-in-direction
                            buffer
                            '((direction . bottom)
                              (window-height . 0.5))))
          (find-file-other-window file)))
    ;; display action for ZK-Index
    (if (one-window-p)
        (pop-to-buffer buffer
                       (display-buffer-in-direction
                        buffer
                        '((direction . top)
                          (window-height . 0.6))))
      (find-file-other-window file))))

(defun zk-index-button-action (_)
  "Action taken when `zk-index' button is pressed."
  (let* ((id (zk-index--button-at-point-p))
         (file (zk--parse-id 'file-path id))
         (buffer
          (find-file-noselect file)))
    (funcall zk-index-button-display-function file buffer)))

(defun zk-index-help-echo (win _obj pos)
  "Generate help-echo zk-index button in WIN at POS."
  (with-selected-window win
    (let ((id (save-excursion
                (goto-char pos)
                (re-search-forward zk-id-regexp (line-end-position) t)
                (match-string-no-properties 0))))
      (format "%s" (zk--parse-id 'title id)))))

(defun zk-index-narrowed-p (buf-name)
  "Return t when index is narrowed in buffer BUF-NAME."
  (with-current-buffer (or buf-name
                           zk-index-buffer-name)
    (if (< (count-lines (point-min) (point-max))
           (length (zk--id-list)))
        t nil)))

;;; Index Search and Focus Functions

;;;; Index Search
;; narrow index based on search of notes' full text

(defun zk-index-search ()
  "Narrow index based on regexp search of note contents."
  (interactive)
  (if (eq major-mode 'zk-index-mode)
      (zk-index-refresh
       (zk-index-query-files)
       zk-index-last-format-function
       zk-index-last-sort-function
       (buffer-name))
    (user-error "Not in a ZK-Index")))

;;;; Index Focus
;; narrow index based on search of note titles (case sensitive)
;; an alternative to consult-focus-lines

(defun zk-index-focus ()
  "Narrow index based on regexp search of note titles."
  (interactive)
  (if (eq major-mode 'zk-index-mode)
      (zk-index-refresh
       (zk-index-query-files)
       zk-index-last-format-function
       zk-index-last-sort-function
       (buffer-name))
    (user-error "Not in a ZK-Index")))

;;;; Low-level Query Functions

(defun zk-index-query-files ()
  "Return narrowed list of notes, based on focus or search query."
  (let* ((command this-command)
         (scope (if (zk-index-narrowed-p (buffer-name))
                    (zk-index--current-id-list (buffer-name))
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
         (ids
          (mapcar
           (lambda (x)
             (when (member x scope)
               x))
           query))
         (files (zk--parse-id 'file-path (remq nil ids))))
    (add-to-history 'zk-index-query-history string)
    (when files
      (let ((mode-line
             (cond ((eq command 'zk-index-focus)
                    (zk-index-focus-mode-line string))
                   ((eq command 'zk-index-search)
                    (zk-index-search-mode-line string)))))
        (setq zk-index-query-mode-line mode-line)
        (zk-index--set-mode-line mode-line)
        (zk-index--reset-mode-name)))
    (when (stringp files)
      (setq files (list files)))
    (or files
        (error "No matches for \"%s\"" string))))

(defun zk-index-focus-mode-line (string)
  "Add STRING to modeline for `zk-index-focus'."
  (cond
   ;;same
   ((eq zk-index-last-query 'focus)
    ;;outcome
    (setq zk-index-last-focus-terms
          (if zk-index-last-focus-terms
              (concat zk-index-last-focus-terms "\" + \"" string)
            string))
    (concat "[Focus: \"" zk-index-last-focus-terms "\"]"))
   ;;mix
   ((eq zk-index-last-query 'search)
    ;;outcome
    (setq zk-index-last-query 'focus)
    (setq zk-index-last-focus-terms
          (if zk-index-last-focus-terms
              (concat zk-index-last-focus-terms "\" + \"" string)
            string))
    (concat "[Search: \"" zk-index-last-search-terms "\" |"
            " Focus: \"" zk-index-last-focus-terms "\"]"))
   ;;neither
   ((not zk-index-last-query)
    ;; outcome
    (setq zk-index-last-query 'focus)
    (setq zk-index-last-focus-terms string)
    (concat "[Focus: \"" string "\"]"))))

(defun zk-index-search-mode-line (string)
  "Add STRING to modeline for `zk-index-search'."
  (cond
   ;;same
   ((eq zk-index-last-query 'search)
    ;;outcome
    (setq zk-index-last-search-terms
          (if zk-index-last-search-terms
              (concat zk-index-last-search-terms "\" + \"" string)
            string))
    (concat "[Search: \"" zk-index-last-search-terms "\"]"))
   ;;mix
   ((eq zk-index-last-query 'focus)
    ;;outcome
    (setq zk-index-last-query 'search)
    (setq zk-index-last-search-terms
          (if zk-index-last-search-terms
              (concat zk-index-last-search-terms "\" + \"" string)
            string))
    (concat "[Focus: \"" zk-index-last-focus-terms "\" |"
            " Search: \"" zk-index-last-search-terms "\"]"))
   ;;neither
   ((not zk-index-last-query)
    ;; outcome
    (setq zk-index-last-query 'search)
    (setq zk-index-last-search-terms string)
    (concat "[Search: \"" string "\"]"))))

(defun zk-index--set-mode-line (string)
  "Add STRING to mode-line in `zk-index-mode'."
  (when (eq major-mode 'zk-index-mode)
    (setq-local mode-line-misc-info string)))

(defun zk-index--reset-mode-line ()
  "Reset mode-line in `zk-index-mode'."
  (setq-local mode-line-misc-info zk-index-mode-line-orig)
  (setq zk-index-query-mode-line nil
        zk-index-last-focus-terms nil
        zk-index-last-search-terms nil))

(defun zk-index--current-id-list (buf-name)
  "Return list of IDs for index in BUF-NAME, as filepaths."
  (let (ids)
    (with-current-buffer (or buf-name
                             zk-index-buffer-name)
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
  (if (eq major-mode 'zk-index-mode)
      (progn
        (zk-index-refresh (zk-index--current-file-list)
                          zk-index-last-format-function
                          #'zk-index--sort-modified
                          (buffer-name))
        (zk-index--set-mode-name " by modified"))
    (user-error "Not in a ZK-Index")))

(defun zk-index-sort-created ()
  "Sort index by date created."
  (interactive)
  (if (eq major-mode 'zk-index-mode)
      (progn
        (zk-index-refresh (zk-index--current-file-list)
                          zk-index-last-format-function
                          #'zk-index--sort-created
                          (buffer-name))
        (zk-index--set-mode-name " by created"))
    (user-error "Not in a ZK-Index")))

(defun zk-index-sort-size ()
  "Sort index by size."
  (interactive)
  (if (eq major-mode 'zk-index-mode)
      (progn
        (zk-index-refresh (zk-index--current-file-list)
                          zk-index-last-format-function
                          #'zk-index--sort-size
                          (buffer-name))
        (zk-index--set-mode-name " by size"))
    (user-error "Not in a ZK-Index")))

(defun zk-index--set-mode-name (string)
  "Add STRING to `mode-name' in `zk-index-mode'."
  (when (eq major-mode 'zk-index-mode)
    (setq mode-name (concat "ZK-Index" string))))

(defun zk-index--reset-mode-name ()
  "Reset `mode-name' in `zk-index-mode'."
  (setq mode-name "ZK-Index"))

(defun zk-index--current-file-list ()
  "Return list files in current index."
  (let* ((ids (zk-index--current-id-list (buffer-name)))
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
          (> (file-attribute-size (file-attributes a))
             (file-attribute-size (file-attributes b))))))

;;; ZK-Index Keymap Commands

(defun zk-index-open-note ()
  "Open note."
  (interactive)
  (beginning-of-line)
  (push-button nil t))

(defvar-local zk-index-view--kill nil)

(defun zk-index-view-note ()
  "View note in `zk-index-view-mode'."
  (interactive)
  (beginning-of-line)
  (let* ((id (zk-index--button-at-point-p))
        (kill (unless (get-file-buffer (zk--parse-id 'file-path id))
                t)))
    (push-button nil t)
    (setq-local zk-index-view--kill kill)
    (zk-index-view-mode)))

(defun zk-index-current-notes ()
  "Open ZK-Index listing currently open notes."
  (interactive)
  (zk-index
   (zk--current-notes-list)
   zk-index-last-format-function
   zk-index-last-sort-function))

(defun zk-index--button-at-point-p (&optional pos)
  "Return zk-id when `zk-index' button is at point.
Takes an option POS position argument."
  (let ((button (or pos
                    (button-at (point)))))
    (when (and button
               (or (eq (button-type button) 'zk-index)
                   (eq (button-type button) 'zk-index-desktop)))
      (save-excursion
        (re-search-forward zk-id-regexp)
        (match-string-no-properties 1)))))

(defun zk-index-insert-link (&optional id)
  "Insert zk-link in `other-window' for button ID at point."
  (interactive)
  (let ((id (or id
                (zk-index--button-at-point-p))))
    (with-selected-window (other-window-for-scrolling)
      (zk-insert-link id)
      (newline))))

(defvar-local zk-index-view--cursor nil)

(define-minor-mode zk-index-view-mode
  "Minor mode for `zk-index-auto-scroll'."
  :init-value nil
  :global nil
  :keymap '(((kbd "n") . zk-index-next-line)
            ((kbd "p") . zk-index-previous-line)
            ((kbd "d") . zk-index-send-to-desktop)
            ([remap read-only-mode] . zk-index-view-mode)
            ((kbd "q") . quit-window))
  (if zk-index-view-mode
      (progn
        (read-only-mode)
        (use-local-map zk-index-mode-map)
        (when zk-index-view-hide-cursor
          (progn
            (scroll-lock-mode 1)
            (setq-local zk-index-view--cursor
                        cursor-type)
            (setq-local cursor-type nil))))
    (read-only-mode -1)
    (use-local-map nil)
    (when zk-index-view-hide-cursor
      (scroll-lock-mode -1)
      (setq-local cursor-type (or zk-index-view--cursor
                                  t)))))

(defun zk-index-next-line ()
  "Move to next line.
If `zk-index-auto-scroll' is non-nil, show note in other window."
  (interactive)
  (let ((split-width-threshold nil))
    (if zk-index-auto-scroll
        (progn
          (cond ((not (zk-file-p)))
                (zk-index-view--kill
                 (kill-buffer)
                 (other-window -1))
                ((not zk-index-view--kill)
                 (zk-index-view-mode)
                 (other-window -1)))
          (forward-button 1)
          (hl-line-highlight)
          (unless (looking-at-p "[[:space:]]*$")
            (zk-index-view-note)))
      (forward-button 1))))

(defun zk-index-previous-line ()
  "Move to previous line.
If `zk-index-auto-scroll' is non-nil, show note in other window."
  (interactive)
  (let ((split-width-threshold nil))
    (if zk-index-auto-scroll
        (progn
          (cond ((not (zk-file-p)))
                (zk-index-view--kill
                 (kill-buffer)
                 (other-window -1))
                ((not zk-index-view--kill)
                 (zk-index-view-mode)
                 (other-window -1)))
          (forward-button -1)
          (hl-line-highlight)
          (unless (looking-at-p "[[:space:]]*$")
            (zk-index-view-note)))
      (forward-button -1))))


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
        (choice (unless (eq (current-buffer) zk-index-desktop-current)
                  (read-char "Choice: \[s\]witch or \[p\]op-up?"))))
    (pcase choice
      ('?s (switch-to-buffer buffer))
      ('?p (pop-to-buffer buffer
                          '(display-buffer-at-bottom)))
      (_ nil))))


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
                           nil nil nil nil
                           (concat zk-index-desktop-basename " ")))
         (file (concat zk-index-desktop-directory "/" desktop)))
    (if (file-exists-p (expand-file-name file))
        (setq zk-index-desktop-current
              (find-file-noselect file))
      (progn
        (generate-new-buffer desktop)
        (setq zk-index-desktop-current desktop)))
    (with-current-buffer zk-index-desktop-current
      (setq require-final-newline 'visit-save)
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))
      (set-visited-file-name file t t)
      (zk-index-desktop-mode)
      (save-buffer))
    (if (and (not (eq last-command 'zk-index-desktop))
             (y-or-n-p (format "Visit %s? " zk-index-desktop-current)))
        (switch-to-buffer zk-index-desktop-current)
      (message "Desktop set to: %s" zk-index-desktop-current)))
  zk-index-desktop-current)

(eval-and-compile
  (define-button-type 'zk-index-desktop
    'read-only t
    'front-sticky t
    'rear-sticky t
    'face 'zk-index-desktop-button
    'cursor-face 'highlight))

;;;###autoload
(defun zk-index-desktop-make-buttons ()
  "Re-make buttons in ZK-Desktop."
  (interactive)
  (when (and (string-match-p zk-index-desktop-basename (buffer-name))
             (file-in-directory-p default-directory zk-index-desktop-directory))
    (let ((inhibit-read-only t))
      (save-excursion
        ;; replace titles
        (goto-char (point-min))
        (let ((ids (zk--id-list))
              (zk-alist (zk--alist)))
          (while (re-search-forward zk-id-regexp nil t)
            (let* ((beg (line-beginning-position))
                   (end (line-end-position))
                   (id  (progn
                          (save-match-data
                            (beginning-of-line)
                            (when (re-search-forward "\\[\\[" end t)
                              (replace-match ""))
                            (when (re-search-forward "]]" end t)
                              (replace-match "")))
                          (match-string-no-properties 1)))
                   (title (buffer-substring-no-properties beg (match-beginning 0)))
                   (new-title (concat zk-index-desktop-prefix
                                      (zk--parse-id 'title id zk-alist) " ")))
              (when (member id ids)
                (beginning-of-line)
                (unless (string= title new-title)
                  (progn
                    (search-forward title end)
                    (replace-match new-title)))))
            (goto-char (match-end 0))))
        ;; make buttons
        (goto-char (point-min))
        (while (re-search-forward zk-id-regexp nil t)
          (let* ((beg (line-beginning-position))
                 (end (line-end-position))
                 (id (match-string-no-properties 1)))
            (make-text-button beg end 'type 'zk-index-desktop
                              'keymap zk-index-desktop-button-map
                              'action 'zk-index-button-action
                              'help-echo 'zk-index-help-echo)
            (when zk-index-invisible-ids
              (beginning-of-line)
              ;; find zk-links and plain zk-ids
              (if (re-search-forward zk-link-regexp (line-end-position) t)
                  (replace-match
                   (propertize (match-string 0) 'invisible t) nil t)
                (progn
                  (re-search-forward id)
                  (replace-match
                   (propertize id
                               'invisible t
                               'read-only t
                               'front-sticky t
                               'rear-sticky t)))))
            (add-text-properties beg (+ beg 1)
                                 '(front-sticky nil))
            (goto-char (match-end 0))))))))

;;;###autoload
(defun zk-index-send-to-desktop (&optional files)
  "Send notes from ZK-Index to ZK-Desktop.
In ZK-Index, works on note at point or notes in active region.
Also works on FILES or group of files in minibuffer, and on zk-id
at point."
  (interactive)
  (unless zk-index-desktop-directory
    (error "Please set 'zk-index-desktop-directory'"))
  (let ((inhibit-read-only t)
        (buffer) (items))
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
          ((string= (buffer-name) zk-index-buffer-name) ;; FIX-ME
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
                          (line-end-position)))))
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
      (setq require-final-newline 'visit-save)
      (pcase zk-index-desktop-add-pos
        ('append (progn
                   (goto-char (point-max))
                   (beginning-of-line)
                   (when (looking-at-p ".")
                     (end-of-line)
                     (newline))))
        ('prepend (progn
                    (goto-char (point-min))))
        ('at-point (goto-char (point))))
      (insert items "\n")
      (beginning-of-line)
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))
      (zk-index-desktop-mode))
    (if (string= (buffer-name) zk-index-buffer-name) ;; FIX-ME
        (message "Sent to %s - press D to switch" buffer)
      (message "Sent to %s" buffer))))

(defun zk-index-desktop-add-toggle ()
  "Set `zk-index-desktop-add-pos' interactively."
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
  (let ((buffer zk-index-buffer-name))
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

(defun zk-index-move-line-down ()
  "Move line at point down in ZK-Desktop buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(defun zk-index-move-line-up ()
  "Move line at point up in ZK-Desktop buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (transpose-lines 1)
    (forward-line -2)))

(defun zk-index-desktop-delete-region-maybe ()
  "Maybe delete region in `zk-index-desktop-mode'."
  (cond ((and (not (use-region-p))
              (zk-index--button-at-point-p))
         (delete-region (line-beginning-position)
                        (line-end-position)))
        ((and (use-region-p)
              (zk-index--button-at-point-p (region-beginning))
              (not (zk-index--button-at-point-p (region-end))))
         (delete-region (save-excursion
                          (goto-char (region-beginning))
                          (line-beginning-position))
                        (region-end))
         t)
        ((and (use-region-p)
              (not (zk-index--button-at-point-p (region-beginning)))
              (zk-index--button-at-point-p (region-end)))
         (delete-region (region-beginning)
                        (save-excursion
                          (goto-char (region-end))
                          (line-end-position)))
         t)
        ((and (use-region-p)
              (zk-index--button-at-point-p (region-beginning))
              (zk-index--button-at-point-p (region-end)))
         (delete-region
          (save-excursion
            (goto-char (region-beginning))
            (line-beginning-position))
          (save-excursion
            (goto-char (region-end))
            (line-end-position)))
         t)
        ((use-region-p)
         (delete-region (region-beginning)
                        (region-end))
         t)))

(defun zk-index-desktop-delete-char ()
  "Wrapper around `delete-char' for `zk-index-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (unless (zk-index-desktop-delete-region-maybe)
      (funcall #'delete-char (or current-prefix-arg 1)))))

(defun zk-index-desktop-delete-backward-char ()
  "Wrapper around `delete-backward-char' for `zk-index-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (unless (zk-index-desktop-delete-region-maybe)
      (funcall #'delete-char (or current-prefix-arg -1)))))

(defun zk-index-desktop-kill-line ()
  "Kill line in `zk-index-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (if (not (zk-index--button-at-point-p))
        (kill-line)
      (kill-region (line-beginning-position)
                   (line-end-position)))))

(defun zk-index-desktop-kill-region ()
  "Wrapper around `kill-region' for `zk-index-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (cond ((and (use-region-p)
                (zk-index--button-at-point-p (region-beginning))
                (not (zk-index--button-at-point-p (region-end))))
           (kill-region (save-excursion
                            (goto-char (region-beginning))
                            (line-beginning-position))
                          (region-end)))
          ((and (use-region-p)
                (not (zk-index--button-at-point-p (region-beginning)))
                (zk-index--button-at-point-p (region-end)))
           (kill-region (region-beginning)
                          (save-excursion
                            (goto-char (region-end))
                            (line-end-position))))
          ((and (use-region-p)
                (zk-index--button-at-point-p (region-beginning))
                (zk-index--button-at-point-p (region-end)))
           (kill-region
            (save-excursion
              (goto-char (region-beginning))
              (line-beginning-position))
            (save-excursion
              (goto-char (region-end))
              (line-end-position))))
          ((use-region-p)
           (kill-region (region-beginning)
                          (region-end))))))

(defun zk-index-desktop-yank ()
  "Wrapper around `yank' for `zk-index-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (yank)
    (zk-index-desktop-make-buttons)))

(provide 'zk-index)

;;; zk-index.el ends here
