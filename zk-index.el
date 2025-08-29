;;; zk-index.el --- Index for zk   -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 25, 2022
;; License: GPL-3.0-or-later
;; Version: 0.10
;; URL: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "28.1")(zk "0.7"))

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

;; ZK-Index: A sortable, searchable, narrowable, semi-persistent selection of
;; notes, in the form of clickable links.

;; To enable integration with Embark, include '(zk-index-setup-embark)' in
;; your init config.

;;; Code:

(require 'zk)
(require 'hl-line)

;;; Custom Variables

(defgroup zk-index nil
  "Index interface for zk."
  :group 'text
  :group 'files
  :prefix "zk-index")

(defcustom zk-index-buffer-name "*ZK-Index*"
  "Name for ZK-Index buffer."
  :local t
  :type 'string)

(defcustom zk-index-format-function 'zk-index--format-candidates
  "Default formatting function for ZK-Index candidates."
  :type 'function)

(defcustom zk-index-invisible-ids t
  "If non-nil, IDs will not be visible in the index."
  :type 'boolean)

(defcustom zk-index-format "%t %i"
  "Default format for candidates in the index."
  :type 'string)

(defcustom zk-index-prefix "-> "
  "String to prepend to note names in ZK-Index."
  :type 'string)

(defcustom zk-index-help-echo-function 'zk-index-help-echo
  "Default help-echo function for ZK-Index buttons.
Set to nil to inhibit help-echo."
  :type 'function)

(defcustom zk-index-auto-scroll t
  "Enable automatically showing note at point in ZK-Index."
  :type 'boolean)

(defcustom zk-index-view-debounce-delay 0.40
  "Seconds to wait before auto viewing note at point in ZK-Index.
Only relevant when `zk-index-auto-scroll’ is non-nil."
  :type 'number)

(defcustom zk-index-cursor nil
  "Cursor to use when `zk-index’ is in the selected window.
See `cursor-type’ for description of possible values."
  :type
  '(choice (const :tag "Frame default" t)
           (const :tag "Filled box" box)
           (cons :tag "Box with specified size" (const box) integer)
           (const :tag "Hollow cursor" hollow)
           (const :tag "Vertical bar" bar)
           (cons :tag "Vertical bar with specified height" (const bar) integer)
           (const :tag "Horizontal bar" hbar)
           (cons :tag "Horizontal bar with specified width" (const hbar) integer)
           (const :tag "None " nil)))

(defcustom zk-index-button-display-function 'zk-index-button-display-action
  "Function called when buttons pressed in ZK-Index.
The function is called by `zk-index-button-action'. A custom
function must take two arguments, FILE and BUFFER respectively.
See the default function `zk-index-button-display-action' for an
example."
  :type 'function)

(defcustom zk-index-view-hide-cursor t
  "Hide cursor in `zk-index-view-mode'."
  :type 'boolean)

(defcustom zk-index-view-mode-lighter " ZK-View"
  "Lighter for `zk-view-mode’."
  :type 'string)

;;; ZK-Index Major Mode Settings

(defvar zk-index-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'zk-index-next-line)
    (define-key map (kbd "p") #'zk-index-previous-line)
    (define-key map (kbd "v") #'zk-index-view-note)
    (define-key map (kbd "o") #'other-window)
    (define-key map (kbd "f") #'zk-index-focus)
    (define-key map (kbd "s") #'zk-index-search)
    (define-key map (kbd "g") #'zk-index-query-refresh)
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
  (read-only-mode)
  (hl-line-mode)
  (setq-local zk-index-buffer-name (buffer-name))
  (setq-local show-paren-mode nil)
  (setq-local cursor-type zk-index-cursor))


;;; Declarations

(defvar zk-index-last-sort-function nil)
(defvar zk-index-last-format-function nil)
(defvar zk-index-query-mode-line nil)
(defvar zk-index-query-terms nil)
(defvar zk-search-history)
(defvar zk--no-gc)

(declare-function zk-file-p zk)
(declare-function zk--grep-id-list zk)


;;; Embark Integration

(defvar embark-multitarget-actions)
(defvar embark-target-finders)
(defvar embark-exporters-alist)

(defun zk-index-setup-embark ()
  "Setup Embark integration for `zk-index'."
  (with-eval-after-load 'embark
    (add-to-list 'embark-multitarget-actions 'zk-index)
    (add-to-list 'embark-multitarget-actions 'zk-copy-link-and-title)
    (add-to-list 'embark-multitarget-actions 'zk-follow-link-at-point)
    (add-to-list 'embark-multitarget-actions 'zk-index-insert-link)
    (add-to-list 'embark-multitarget-actions 'zk-index-narrow)
    (add-to-list 'embark-target-finders 'zk-index-embark-target)
    (add-to-list 'embark-exporters-alist '(zk-file . zk-index-narrow))
    (add-to-list 'embark-exporters-alist '(zk-id . zk-index-narrow))
    (define-key zk-file-map (kbd "n")  #'zk-index-narrow)
    (define-key zk-file-map (kbd "i") #'zk-index-insert-link)))

(defun zk-index-embark-target ()
  "Target zk-id of button at point in ZK-Index."
  (when (zk-index--button-at-point-p)
    (save-excursion
      (beginning-of-line)
      (re-search-forward zk-id-regexp (line-end-position)))
    (let* ((zk-id (match-string-no-properties 1))
           (zk-file (zk--parse-id 'file-path zk-id)))
      `(zk-file ,zk-file . ,(cons (line-beginning-position) (line-end-position))))))

(defun zk-index-narrow (arg)
  "Produce a ZK-Index narrowed to notes listed in ARG.
For details of ARG see `zk--processor'. When called on items
selected by `embark-select', narrows index to selected
candidates. Alternatively, `embark-export' exports candidates to
a new index."
  (let ((files (zk--processor arg)))
    (zk-index files)
    (zk-index--reset-mode-line)))

;;; Formatting

(defun zk-index--format-candidates (&optional files format)
  "Return a list of FILES as formatted candidates, following FORMAT.
See `zk--format' for details about FORMAT. If nil, `zk-index-format'
will be used by default. FILES must be a list of filepaths. If nil,
all files in `zk-directory' will be returned as formatted candidates."
  (let* ((format (or format zk-index-format))
         (list (or files
                   (zk--directory-files)))
         (output))
    (dolist (file list)
      (when (string-match (zk-file-name-regexp) file)
        (let ((id (if zk-index-invisible-ids
                      (propertize (match-string 1 file) 'invisible t)
                    (match-string 1 file)))
              (title (replace-regexp-in-string
                      zk-file-name-separator
                      " "
                      (match-string 2 file))))
          (push (zk--format format id title) output))))
    output))

;;; Main Stack

;;;###autoload
(defun zk-index (&optional files format-fn sort-fn buf-name)
  "Open ZK-Index, with optional FILES, FORMAT-FN, SORT-FN, BUF-NAME."
  (interactive)
  (setq zk-index-last-format-function format-fn)
  (when sort-fn
    (setq zk-index-last-sort-function sort-fn))
  (let* ((zk--no-gc t)
         (inhibit-message nil)
         (inhibit-read-only t)
         (buf-name (or buf-name
                       zk-index-buffer-name))
         (files (or files
                    (zk--directory-files t))))
    (if (not (get-buffer buf-name))
        (progn
          (when zk-default-backlink
            (unless (zk-file-p)
              (zk-find-file-by-id zk-default-backlink)))
          (generate-new-buffer buf-name)
          (with-current-buffer buf-name
            (setq default-directory (expand-file-name zk-directory))
            (zk-index-mode)
            (zk-index--sort files format-fn sort-fn)
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
  (let* ((zk--no-gc t)
         (inhibit-message t)
         (inhibit-read-only t)
         (files (or files
                    (zk--directory-files t)))
         (sort-fn (or sort-fn
                      (setq zk-index-last-sort-function nil)))
         (buf-name (or buf-name
                       zk-index-buffer-name))
         pos)
    (setq zk-index-last-format-function format-fn)
    (setq zk-index-last-sort-function sort-fn)
    (with-current-buffer buf-name
      (setq pos (point))
      (erase-buffer)
      (zk-index--reset-mode-name)
      (zk-index--sort files format-fn sort-fn)
      (goto-char (point-min))
      (setq truncate-lines t)
      (unless (zk-index-narrowed-p buf-name)
        (zk-index--reset-mode-line)
        (goto-char pos)))))

(defun zk-index--sort (files &optional format-fn sort-fn)
  "Sort FILES, with option FORMAT-FN and SORT-FN."
  (let* ((sort-fn (or sort-fn
                      'zk-index--sort-modified))
         (files (if (zk--singleton-p files)
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
  (defvar zk-index-button-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map button-map)
      map)
    "Keymap for ZK-Index buttons."))

(eval-and-compile
  (define-button-type 'zk-index
    'follow-link t
    'action 'zk-index-button-action
    'keymap 'zk-index-button-map
    'face 'default))

(defun zk-index--insert (candidates)
  "Insert CANDIDATES into ZK-Index."
  (when (derived-mode-p 'zk-index-mode)
    (save-excursion
      (dolist (file candidates)
        (insert zk-index-prefix file "\n"))
      (zk-index--make-buttons)
      (zk-index--set-mode-name (format " [%s]" (length candidates))))))

(defun zk-index--make-buttons ()
  "Make buttons in ZK-Index buffer."
  (goto-char (point-min))
  (while (re-search-forward zk-id-regexp nil t)
    (let* ((beg (line-beginning-position))
           (end (line-end-position)))
      (beginning-of-line)
      (make-text-button beg end
                        'type 'zk-index
                        'help-echo zk-index-help-echo-function)
      (end-of-line))))

;;; Utilities

(defun zk-index-button-display-action (file buffer)
  "Function to display FILE or BUFFER on button press in ZK-Index."
  (if (one-window-p)
      (pop-to-buffer buffer
                     (display-buffer-in-direction
                      buffer
                      '((direction . top)
                        (window-height . 0.6))))
    (find-file-other-window file)))

(defun zk-index-button-action (_)
  "Action taken when `zk-index' button is pressed."
  (let* ((id (zk-index--button-at-point-p))
         (file (zk--parse-id 'file-path id))
         (buffer (find-file-noselect file)))
    (funcall zk-index-button-display-function file buffer)))

(defun zk-index-help-echo (win _obj pos)
  "Generate help-echo for `zk-index' button in WIN at POS."
  (with-selected-window win
    (goto-char pos)
    (let* ((beg (+ (line-beginning-position)
                   (length zk-index-prefix)))
           (end (line-end-position))
           (title (buffer-substring beg end)))
      (format "%s" title))))

(defun zk-index-narrowed-p (buf-name)
  "Return t when index is narrowed in buffer BUF-NAME."
  (when (get-buffer buf-name)
    (with-current-buffer buf-name
      (if (< (count-lines (point-min) (point-max))
             (length (zk--directory-files)))
          t nil))))

;;; Index Search and Focus Functions

;;;; Index Search
;; narrow index based on search of notes' full text

(defun zk-index-search (&optional string)
  "Narrow index to notes containing STRING."
  (interactive (list
                (read-string "Narrow index by content: "
                             nil 'zk-search-history)))
  (zk-index-query-files string))

;;;; Index Focus
;; narrow index based on search of note titles (case sensitive)
;; an alternative to consult-focus-lines

(defun zk-index-focus (&optional string)
  "Narrow index to notes with STRING in title."
  (interactive (list
                (read-string "Narrow index by title: "
                             nil 'zk-search-history)))
  (zk-index-query-files string))

;;;; Low-level Query Functions

(defvar zk-index-query-terms nil
  "Ordered list of current query terms.
Takes form of (COMMAND . TERM), where COMMAND is `ZK-INDEX-FOCUS
or `ZK-INDEX-SEARCH, and TERM is the query string. Recent
items listed first.")

(defun zk-index-query-files (&optional string)
  "Return narrowed list of notes, based on focus or search query.
Optional STRING arg."
  (let* ((zk--no-gc t)
         (command (if (eq this-command 'zk-index-focus)
                      'zk-index-focus
                    'zk-index-search))
         (index-buf zk-index-buffer-name)
         (scope (if (zk-index-narrowed-p index-buf)
                    (zk-index--current-id-list index-buf)
                  (setq zk-index-query-terms nil)
                  (zk--id-list)))
         (string (or string
                     (read-string "Narrow by title: "
                                  nil 'zk-search-history)))
         (query (cond
                 ((string-empty-p string)
                  (user-error "Must enter a search string"))
                 ((eq command 'zk-index-focus)
                  (zk--id-list string))
                 (t
                  (zk--grep-id-list string))))
         (mode-line (zk-index-query-mode-line command string))
         (ids (mapcar (lambda (x) (when (member x scope) x))
                      query))
         (files (ensure-list (zk--parse-id 'file-path (remq nil ids)))))
    (add-to-history 'zk-search-history string)
    (if files
        (progn
          (zk-index files
                    zk-index-last-format-function
                    zk-index-last-sort-function
                    index-buf)
          (setq zk-index-query-mode-line
                (propertize mode-line
                            'help-echo mode-line))
          (add-to-list 'mode-line-misc-info '(:eval (zk-index--query-mode-line)))
          (force-mode-line-update t))
      (error "No matches for \"%s\" in %s" string zk-index-buffer-name))))

(defun zk-index-query-refresh ()
  "Refresh narrowed index, based on last focus or search query."
  (interactive)
  (let ((mode mode-name)
        (files (zk-index--current-file-list)))
    (unless (stringp files)
      (zk-index-refresh files
                        nil
                        zk-index-last-sort-function)
      (setq mode-name mode))))

(defun zk-index-query-mode-line (query-command string)
  "Generate new mode line after query.
QUERY-COMMAND is either `zk-index-focus' or `zk-index-search',
with query term STRING."
  (push (cons query-command string) zk-index-query-terms)
  ;; Sort the different terms into two lists
  (let (focused
        searched)
    (dolist (term zk-index-query-terms)
      (if (equal (car term) 'zk-index-focus)
          (push term focused)
        (push term searched)))
    ;; Format each list and update appropriate list
    (let* ((formatted
            (mapcar (lambda (term-list)
                      (when term-list
                        ;; (CMD . STRING)
                        (cons (caar term-list)
                              (mapconcat #'cdr term-list "\" + \""))))
                    ;;      CAR     CDR
                    (list focused searched))))
      (concat "["
              (mapconcat (lambda (query)
                           (when query
                             (concat
                              (cond
                               ((eq (car query) 'zk-index-focus)
                                "Title")
                               ((eq (car query) 'zk-index-search)
                                "Content"))
                              ": \""
                              (cdr query))))
                         ;; Put the last query type at the end
                         (sort (remq nil formatted)
                               (lambda (a _b)
                                 (not (equal (car a) query-command))))
                         "\" | ")
              "\"]"))))

(defun zk-index--query-mode-line ()
  "Add STRING to mode-line in `zk-index-mode'."
  (when (derived-mode-p 'zk-index-mode)
    zk-index-query-mode-line))

(defun zk-index--reset-mode-line ()
  "Reset mode-line in `zk-index-mode'."
  (setq zk-index-query-mode-line nil
        zk-index-query-terms nil))

(defun zk-index--current-id-list (buf-name)
  "Return list of IDs for index in BUF-NAME, as filepaths."
  (let (ids)
    (with-current-buffer (or buf-name
                             zk-index-buffer-name)
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (while (re-search-forward zk-id-regexp nil t)
            (push (match-string-no-properties 1) ids)))
        ids))))

;;; Index Sort Functions

(defun zk-index--sort-call (sort-fn mode-string)
  "Call SORT-FN with MODE-STRING."
  (if (derived-mode-p 'zk-index-mode)
      (let ((zk--no-gc t))
        (zk-index-refresh (zk-index--current-file-list)
                          zk-index-last-format-function
                          sort-fn
                          (buffer-name))
        (zk-index--set-mode-name mode-string))
    (user-error "Not in a ZK-Index")))

(defun zk-index-sort-modified ()
  "Sort index by last modified."
  (interactive)
  (zk-index--sort-call #'zk-index--sort-modified " by modified"))

(defun zk-index-sort-created ()
  "Sort index by date created."
  (interactive)
  (zk-index--sort-call #'zk-index--sort-created " by created"))

(defun zk-index-sort-size ()
  "Sort index by size."
  (interactive)
  (zk-index--sort-call #'zk-index--sort-size " by size"))

(defun zk-index--set-mode-name (string)
  "Add STRING to `mode-name' in `zk-index-mode'."
  (setq mode-name (concat mode-name string)))

(defun zk-index--reset-mode-name ()
  "Reset `mode-name' in `zk-index-mode'."
  (setq mode-name "ZK-Index"))

(defun zk-index--current-file-list ()
  "Return list files in current index."
  (let ((ids (zk-index--current-id-list (buffer-name))))
    (zk--parse-id 'file-path ids)))

(defun zk-index--sort-created (list)
  "Sort LIST for latest created."
  (let ((ht (make-hash-table :test #'equal :size 5000)))
    (dolist (x list)
      (puthash x (zk--parse-file 'id x) ht))
    (sort list
          (lambda (a b)
            (string>
             (gethash a ht)
             (gethash b ht))))))

(defun zk-index--sort-modified (list)
  "Sort LIST for latest modification."
  (let ((ht (make-hash-table :test #'equal :size 5000)))
    (dolist (x list)
      (puthash x (file-attribute-modification-time (file-attributes x)) ht))
    (sort list
          (lambda (a b)
            (time-less-p
             (gethash b ht)
             (gethash a ht))))))

(defun zk-index--sort-size (list)
  "Sort LIST for latest modification."
  (let ((ht (make-hash-table :test #'equal :size 5000)))
    (dolist (x list)
      (puthash x (file-attribute-size (file-attributes x)) ht))
    (sort list
          (lambda (a b)
            (>
             (gethash a ht)
             (gethash b ht))))))

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
  (let* ((zk--no-gc t)
         (zk-enable-link-buttons nil)
         (id (zk-index--button-at-point-p))
         (file (zk--parse-id 'file-path id))
         (kill (unless (get-file-buffer file)
                 t))
         (buffer (find-file-noselect file))
         (index-name (buffer-name)))
    (funcall zk-index-button-display-function file buffer)
    (setq-local zk-index-buffer-name index-name)
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
  (save-excursion
    (beginning-of-line)
    (let ((button (or pos
                      (button-at (point)))))
      (when (and button
                 (button-has-type-p button 'zk-index))
        (save-excursion
          (when (re-search-forward zk-id-regexp)
            (match-string-no-properties 1)))))))

(defun zk-index-insert-link (&optional id)
  "Insert zk-link in `other-window' for button ID at point."
  (interactive (list (or (zk--id-at-point)
                         (zk-index--button-at-point-p))))
  (cond ((derived-mode-p 'zk-index-mode)
         (with-selected-window (other-window-for-scrolling)
           (zk-insert-link id)))
        ((zk--id-at-point)
         (user-error "Move point off zk-id before inserting"))
        (t
         (zk-insert-link id))))

(defvar-local zk-index-view--cursor nil)

(define-minor-mode zk-index-view-mode
  "Minor mode for `zk-index-auto-scroll'."
  :init-value nil
  :global nil
  :lighter (:eval zk-index-view-mode-lighter)
  :keymap '(((kbd "n") . zk-index-next-line)
            ((kbd "p") . zk-index-previous-line)
            ([return] . zk-index-view-mode)
            ([remap read-only-mode] . zk-index-view-mode)
            ((kbd "s") . zk-index-search)
            ((kbd "/") . zk-index-focus)
            ((kbd "q") . zk-index-view-quit-window))
  (if zk-index-view-mode
      (if (zk-file-p)
          (progn
            (read-only-mode)
            (when zk-index-view-hide-cursor
              (progn
                (scroll-lock-mode 1)
                (setq-local zk-index-view--cursor
                            cursor-type)
                (setq-local cursor-type nil))))
        (error "Not a zk file"))
    (read-only-mode -1)
    (kill-local-variable 'zk-index-buffer-name)
    (when zk-enable-link-buttons
      (zk-make-link-buttons))
    (when zk-index-view-hide-cursor
      (scroll-lock-mode -1)
      (setq-local cursor-type (or zk-index-view--cursor
                                  t)))))

(defun zk-index-view-quit-window ()
  "Quit `zk-index-view-mode’ window and select `zk-index’ buffer."
  (interactive)
  (let ((index zk-index-buffer-name))
    ;; mode-off kills local value of zk-index-buffer-name,
    ;; so need to let bind it, to avoid switching to default index
    (zk-index-view-mode -1)
    (quit-window zk-index-view--kill)
    (zk-index-switch-to-index index)))

(defvar zk-index--debounce-timer nil)

(defun zk-index--view-note-debounce ()
  "Delay calling of `zk-index-view-note’ by."
  (if (timerp zk-index--debounce-timer)
      (timer-set-idle-time zk-index--debounce-timer
                           zk-index-view-debounce-delay)
    (setq zk-index--debounce-timer
          (run-with-idle-timer
           zk-index-view-debounce-delay nil
           (lambda ()
             (setq zk-index--debounce-timer nil)
             (other-window 1)
             (when (zk-file-p)
               (if zk-index-view--kill
                   (kill-buffer)
                 (zk-index-view-mode -1)))
             (other-window 1)
             (zk-index-view-note))))))

(defun zk-index-forward-button (N)
  "Move to the Nth next button, or Nth previous button if N is negative.
If `zk-index-auto-scroll' is non-nil, show note in other window."
  (let ((split-width-threshold nil)
        (index-window (get-buffer-window
                       zk-index-buffer-name)))
    (if zk-index-auto-scroll
        (progn
          (if (and (zk-file-p)
                   index-window)
              (select-window index-window))
          (forward-button N)
          (hl-line-highlight)
          (unless (looking-at-p "[[:space:]]*$")
            (zk-index--view-note-debounce)))
      (forward-button N))))

(defun zk-index-next-line ()
  "Move to next line.
If `zk-index-auto-scroll' is non-nil, show note in other window."
  (interactive)
  (zk-index-forward-button 1))

(defun zk-index-previous-line ()
  "Move to previous line.
If `zk-index-auto-scroll' is non-nil, show note in other window."
  (interactive)
  (zk-index-forward-button -1))

;;;###autoload
(defun zk-index-switch-to-index ()
  "Switch to ZK-Index buffer."
  (interactive)
  (let ((buffer zk-index-buffer-name))
    (if (get-buffer buffer)
        (pop-to-buffer zk-index-buffer-name)
      (zk-index))))

(provide 'zk-index)

;;; zk-index.el ends here
