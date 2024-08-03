;;; zk-desktop.el --- Desktop environment for zk   -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: November 4, 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "27.1")(zk "0.6")(zk-index "0.9"))

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

;; ZK-Desktop: A place (or places) for collecting, grouping, arranging, and
;; saving curated selections of notes (also in txhe form of clickable links).

;; To enable integration with Embark, include '(zk-desktop-setup-embark)' in
;; your init config.

;;; Code:

(require 'zk)
(require 'zk-index)

;;; Custom Variables

(defgroup zk-desktop nil
  "Desktop interface for zk."
  :group 'text
  :group 'files
  :prefix "zk-desktop")

(defcustom zk-desktop-directory nil
  "Directory for saved ZK-Desktops."
  :type 'directory)

(defcustom zk-desktop-basename "*ZK-Desktop:"
  "Basename for ZK-Desktops.
The names of all ZK-Desktops should begin with this string."
  :type 'string)

(defcustom zk-desktop-entry-prefix ""
  "String to prepend to entries in Zk-Desktop."
  :type 'string)

(defcustom zk-desktop-entry-suffix ""
  "String to append to entries in Zk-Desktop."
  :type 'string)

(defcustom zk-desktop-entry-format "%t %i"
  "Format string for entries in ZK-Desktop.
This is the part of each line in ZK-Desktop buffer that
become buttons (see `zk-desktop-make-buttons'); use
`zk-desktop-entry-prefix' and `zk-desktop-entry-suffix' to
add arbitary text around the entry, and which would not be
part of the buttons themselves.

See `zk-format-function' and `zk-format-id-and-title' for
valid control strings."
  :type 'string)

(defcustom zk-desktop-make-buttons t
  "If non-nil, Zk-Desktop will make buttons.
Possible values are t (make normal buttons), 'invisible
\(make buttons with invisible IDs), or nil (don't make any
buttons)."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Yes, with invisible IDs" invisible)
                 (const :tag "No" nil)))

(make-obsolete-variable 'zk-desktop-invisible-ids 'zk-desktop-make-buttons "0.6")

(defcustom zk-desktop-mark-missing "<- ID NOT FOUND"
  "If non-nil, Zk-Desktop will mark missing IDs.
Possible values are a string for the text of an overlay to
add at the end of lines with missing IDs, non-nil to merely
display their buttons with `zk-desktop-missing-button' face,
or nil to eschew checking for missing IDs at all."
  :type '(choice (string :tag "Add overlay text" "<- ID NOT FOUND")
                 (const :tag "Propertize missing" t)
                 (const :tag "Do not mark" nil)))

(defun zk-desktop-line-regexp ()
  "Return the regexp for the relevant Zk-Desktop lines.
The value is computed from `zk-desktop-entry-prefix',
`zk-desktop-entry-suffix', `zk-desktop-entry-format', and
`zk-id-regexp'.

Group 1 is the note zk-ID.
Group 2 is the note title.
Group 3 is the entire entry."
  (zk--format (concat (regexp-quote zk-desktop-entry-prefix)
                      "\\(?3:"
                      (regexp-quote zk-desktop-entry-format)
                      "\\)"
                      (regexp-quote zk-desktop-entry-suffix))
              (concat "\\(?1:" zk-id-regexp "\\)")
              (concat "\\(?2:" ".*" "\\)"))) ; FIXME: `zk-title-regexp' (PR #68)

(defcustom zk-desktop-major-mode nil
  "Name of major-mode for ZK-Desktop buffers.
The value should be a symbol that is a major mode command.
If nil, buffers will be in `fundamental-mode'."
  :type 'function)

(defcustom zk-desktop-button-display-function 'zk-desktop-button-display-action
  "Function called when buttons pressed in ZK-Desktop.
The function is called by `zk-desktop-button-action'. A custom
function must take two arguments, FILE and BUFFER respectively.
See the default function `zk-desktop-button-display-action' for an
example."
  :type 'function)

(defcustom zk-desktop-help-echo-function 'zk-desktop-help-echo
  "Default help-echo function for ZK-Index buttons.
Set to nil to inhibit help-echo."
  :type 'function)

(defcustom zk-desktop-add-pos 'append
  "Behavior for placement of notes in ZK-Desktop via `zk-desktop-send-to-desktop'.

Options:
1. `append - Place notes at end of current ZK-Desktop
2. `prepend - Place notes at beginning of current ZK-Desktop
3. `at-point - Place notes at current point of current ZK-Desktop

To quickly change this setting, call `zk-desktop-add-toggle'."
  :type '(choice (const :tag "Append" append)
                 (const :tag "Prepend" prepend)
                 (const :tag "At point" at-point)))

(defface zk-desktop-button
  '((t :inherit default))
  "Face used for buttons in `zk-desktop-mode'.")

(defface zk-desktop-missing-button
  '((t :inherit error))
  "Face used for buttons in `zk-desktop-mode' with missing IDs.")

;;; Declarations

(defvar zk-desktop-current nil
  "Buffer object of the current Zk-Desktop.")

;;; Embark Integration

(defvar embark-multitarget-actions)
(defvar embark-target-finders)
(defvar embark-exporters-alist)

(defun zk-desktop-setup-embark ()
  "Setup Embark integration for `zk-desktop'."
  (with-eval-after-load 'embark
    (add-to-list 'embark-multitarget-actions 'zk-desktop-send-to-desktop)
    (define-key zk-file-map (kbd "d") #'zk-desktop-send-to-desktop)
    (define-key zk-id-map (kbd "d") #'zk-desktop-send-to-desktop)))

;;; ZK-Desktop Minor Mode Settings

(defvar zk-desktop-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<up>") #'zk-desktop-move-line-up)
    (define-key map (kbd "C-<down>") #'zk-desktop-move-line-down)
    (define-key map [remap delete-char] #'zk-desktop-delete-char)
    (define-key map [remap delete-backward-char] #'zk-desktop-delete-backward-char)
    (define-key map [remap kill-region] #'zk-desktop-kill-region)
    (define-key map [remap yank] #'zk-desktop-yank)
    map)
  "Keymap for ZK-Desktop buffers.")

(define-minor-mode zk-desktop-mode
  "Minor mode for `zk-desktop'."
  :lighter " Zk-Desktop"
  :init-value nil
  :keymap zk-desktop-map
  (cond (zk-desktop-mode                ; enabled
         (when zk-desktop-make-buttons
           (zk-desktop-make-buttons))
         (when-let ((major-mode zk-desktop-major-mode))
           (funcall major-mode))
         (setq zk-desktop-mode t))
        (t                              ; disabled
         (zk-desktop--clear))))

(eval-and-compile
  (defvar zk-desktop-button-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-<up>") #'zk-desktop-move-line-up)
      (define-key map (kbd "C-<down>") #'zk-desktop-move-line-down)
      (define-key map [remap kill-line] #'zk-desktop-kill-line)
      (define-key map [remap delete-char] #'zk-desktop-delete-char)
      (define-key map [remap kill-region] #'zk-desktop-kill-region)
      (define-key map (kbd "v") #'zk-index-view-note)
      (define-key map (kbd "n") #'zk-index-next-line)
      (define-key map (kbd "p") #'zk-index-previous-line)
      (define-key map [remap self-insert-command] 'ignore)
      (set-keymap-parent map button-map)
      map)
    "Keymap for ZK-Desktop buttons."))

(define-key zk-index-view-mode-map (kbd "d") #'zk-desktop-send-to-desktop)

;;; ZK-Desktop

;;;###autoload
(defun zk-desktop ()
  "Open ZK-Desktop."
  (interactive)
  (let ((buffer (if (and zk-desktop-current
                         (buffer-live-p (get-buffer zk-desktop-current)))
                    zk-desktop-current
                  (zk-desktop-select)))
        (choice (unless (eq (current-buffer) zk-desktop-current)
                  (read-char "Choice: \[s\]witch or \[p\]op-up?"))))
    (pcase choice
      ('?s (switch-to-buffer buffer))
      ('?p (pop-to-buffer buffer
                          '(display-buffer-at-bottom)))
      (_ nil))))


;;;###autoload
(defun zk-desktop-select ()
  "Select a ZK-Desktop to work with.
Return the buffer object visiting the selected or created
desktop."
  (interactive)
  (unless zk-desktop-directory
    (error "Please set `zk-desktop-directory' first"))
  (let* ((last-command last-command)
         (desktop
          (completing-read "Select or Create ZK-Desktop: "
                           (directory-files zk-desktop-directory
                                            nil
                                            (concat
                                             zk-desktop-basename
                                             ".*"))
                           nil nil zk-desktop-basename nil))
         (file (concat zk-desktop-directory "/" desktop)))
    (setq zk-desktop-current
      (if (file-exists-p (expand-file-name file))
          (find-file-noselect file)
        (generate-new-buffer desktop)))
    (with-current-buffer zk-desktop-current
      (setq require-final-newline 'visit-save)
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))
      (set-visited-file-name file t t)
      (zk-desktop-mode)
      (save-buffer))
    (if (and (not (eq last-command 'zk-desktop))
             (y-or-n-p (format "Visit %s? " zk-desktop-current)))
        (switch-to-buffer zk-desktop-current)
      (message "Desktop set to: %s" zk-desktop-current)))
  zk-desktop-current)

(eval-and-compile
  (define-button-type 'zk-desktop
    'supertype 'zk-index
    'read-only t
    'front-sticky t
    'rear-sticky t
    'button-data nil                    ; filled by `zk-desktop--make-button'
    'keymap zk-desktop-button-map
    'action 'zk-desktop-button-action
    'face 'zk-desktop-button
    'cursor-face 'highlight))

(defun zk-desktop--make-button ()
  "Try to make a ZK-Desktop button after point.
Return nil if there are no more buttons to be made in the
buffer. Otherwise, move point after the button created and
return a tuple of button boundaries."
  (save-match-data
    (when-let* ((beg        (point))
                (_          (re-search-forward (zk-desktop-line-regexp) nil t))
                (id         (match-string-no-properties 1))
                (id-beg     (match-beginning 1))
                (id-end     (match-end 1))
                (title      (match-string-no-properties 2))
                (button-beg (match-beginning 3))
                (button-end (match-end 3)))
      (replace-match (save-match-data
                       (zk--format zk-desktop-entry-format id title))
                     nil t nil 3)
      (if (not (eq 'invisible zk-desktop-make-buttons))
          ;; I.e. can add text in front of the button?
          (add-text-properties button-beg (1+ button-beg) '(front-sticky nil))
        ;; Make entire link invisible, not just the ID
        (goto-char beg)
        (when (re-search-forward (zk-link-regexp) (line-end-position) t)
          (setq id-beg (match-beginning 0)
                id-end (match-end 0)))
        ;; I.e. can add text in the rear of invisible IDs, but not in the front?
        (add-text-properties id-beg id-end '(invisible t rear-nonsticky t))
        ;; Org-mode requires more drastic measures
        (when (eq zk-desktop-major-mode 'org-mode)
          (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put overlay 'invisible t)
            (overlay-put overlay 'type 'zk-desktop))))
      (make-text-button button-beg button-end
                        'type 'zk-desktop
                        'button-data (list id title nil) ; matches `zk--alist'
                        'help-echo zk-desktop-help-echo-function)
      (goto-char button-end)
      (cons button-beg button-end))))

(defun zk-desktop--clear ()
  "Clear special text properties added by `zk-desktop-make-buttons'.
This removes buttons, overlays, and text properties from the
entire buffer."
  (save-excursion
    (let ((inhibit-read-only t))
      (remove-overlays (point-min) (point-max) 'type 'zk-desktop)
      (set-text-properties (point-min) (point-max) '()))))

;;;###autoload
(defun zk-desktop-make-buttons ()
  "Re-make buttons in ZK-Desktop.
If `zk-desktop-make-buttons' is nil, just clear any existing
buttons and overlays."
  (interactive)
  (unless (and (string-match-p zk-desktop-basename (buffer-name))
               (file-in-directory-p default-directory zk-desktop-directory))
    (user-error "Can only make buttons in Zk desktop file; %s isn't"
                (buffer-name)))
  (let* ((inhibit-read-only t)
         (ids (if zk-desktop-mark-missing
                  (zk--id-list nil (zk--alist))
                nil))
         button-bounds)
    (zk-desktop--clear)
    (when zk-desktop-make-buttons
      (save-excursion
        (goto-char (point-min))
        (while (setq button-bounds (zk-desktop--make-button))
          (let* ((button-data (get-text-property (car button-bounds) 'button-data))
                 (button-id (car button-data)))
            (cond ((and (stringp zk-desktop-mark-missing)
                        (not (member button-id ids)))
                   (let ((overlay (make-overlay (line-end-position) (line-end-position))))
                     (overlay-put overlay 'type 'zk-desktop)
                     (overlay-put overlay 'before-string
                                  (propertize zk-desktop-mark-missing
                                              'font-lock-face 'zk-desktop-missing-button))))
                  ((and zk-desktop-mark-missing
                        (not (member button-id ids)))
                   (add-text-properties (car button-bounds) (cdr button-bounds)
                                        '(face zk-desktop-missing-button)))
                  (t
                   ;; do nothing
                   ))))))))

;;; Utilities

(defun zk-desktop-button-display-action (file buffer)
  "Function to display FILE or BUFFER on button press in ZK-Desktop."
  (if (one-window-p)
      (pop-to-buffer buffer
                     (display-buffer-in-direction
                      buffer
                      '((direction . bottom)
                        (window-height . 0.5))))
    (find-file-other-window file)))

(defun zk-desktop-button-action (_)
  "Action taken when `zk-desktop' button is pressed."
  (let* ((id (zk-index--button-at-point-p))
         (file (zk--parse-id 'file-path id))
         (buffer (find-file-noselect file)))
    (funcall zk-desktop-button-display-function file buffer)))

(defun zk-desktop-help-echo (win _obj pos)
  "Generate help-echo for `zk-desktop' button in WIN at POS."
  (save-excursion
    (with-selected-window win
      (goto-char pos)
      (let* ((beg (+ (line-beginning-position)
                     (length zk-desktop-entry-prefix)))
             (end (- (line-end-position)
                     (length zk-desktop-entry-suffix)))
             (title (buffer-substring-no-properties beg end)))
        (format "%s" title)))))

;;; Commands

(defun zk-desktop--gather-items (arg)
  "Normalize ARG into a list of files."
  (cond ((stringp arg)
         (zk--formatter arg zk-desktop-entry-format))
        ((eq major-mode 'zk-index-mode)
         (let ((ids (if (use-region-p)
                        (zk-index--current-id-list (current-buffer)
                                                   (region-beginning)
                                                   (region-end))
                      (zk-index--current-id-list (current-buffer)
                                                 (line-beginning-position)
                                                 (line-end-position)))))
           (zk--formatter ids zk-desktop-entry-format)))
        ((zk-file-p)
         (zk--formatter buffer-file-name zk-desktop-entry-format))
        (t (user-error "No item to send to desktop"))))

;;;###autoload
(defun zk-desktop-send-to-desktop (&optional items suffix)
  "Add ITEMS to the current ZK-Desktop.
In ZK-Index, works on note at point or notes in active
region. Also works on files or group of files in minibuffer,
passed as ITEMS, and on Zk-ID at point. With non-nil SUFFIX,
insert it after each entry. New entries are inserted
according to `zk-desktop-add-pos'.

See `zk-desktop-entry-format', `zk-desktop-entry-prefix',
and `zk-desktop-entry-suffix' for the format of each line."
  (interactive)
  (unless zk-desktop-directory
    (error "Please set `zk-desktop-directory' first"))
  (let ((inhibit-read-only t)
        (items (zk-desktop--gather-items items))
        (buffer (if (buffer-live-p zk-desktop-current)
                    zk-desktop-current
                  (zk-desktop-select))))
    (with-current-buffer buffer
      (setq require-final-newline 'visit-save)
      (pcase zk-desktop-add-pos
        ('append (goto-char (point-max))
                 (beginning-of-line)
                 (when (looking-at-p ".")
                   (end-of-line)
                   (newline)))
        ('prepend (goto-char (point-min)))
        ('at-point (goto-char (point))))
      (mapc (lambda (item)
              (insert (concat zk-desktop-entry-prefix
                              item
                              (or suffix zk-desktop-entry-suffix)
                              "\n")))
            items)
      (beginning-of-line)
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))
      (when zk-desktop-make-buttons
        (zk-desktop--make-button)))
    (if (eq major-mode 'zk-index-mode)
        (message "Sent to %s - press D to switch" buffer)
      (message "Sent to %s" buffer))))

(defun zk-desktop-add-toggle ()
  "Set `zk-desktop-add-pos' interactively."
  (interactive)
  (let ((choice (read-char "Choice: \[a\]ppend; \[p\]repend; at-\[P\]oint")))
    (pcase choice
      ('?a (setq zk-desktop-add-pos 'append))
      ('?p (setq zk-desktop-add-pos 'prepend))
      ('?P (setq zk-desktop-add-pos 'at-point)))))

;;;###autoload
(defun zk-desktop-switch-to-desktop ()
  "Switch to ZK-Desktop.
With prefix-argument, raise ZK-Desktop in other frame."
  (interactive)
  (unless (and zk-desktop-current
               (buffer-live-p (get-buffer zk-desktop-current)))
    (zk-desktop-select))
  (let ((buffer zk-desktop-current))
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

(defun zk-desktop-move-line-down ()
  "Move line at point down in ZK-Desktop buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (when zk-desktop-make-buttons
      (zk-desktop-make-buttons))))

(defun zk-desktop-move-line-up ()
  "Move line at point up in ZK-Desktop buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (transpose-lines 1)
    (forward-line -2)
    (when zk-desktop-make-buttons
      (zk-desktop-make-buttons))))

(defun zk-desktop-delete-region-maybe ()
  "Maybe delete region in `zk-desktop-mode'."
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

(defun zk-desktop-delete-char ()
  "Wrapper around `delete-char' for `zk-desktop-mode'."
  (interactive)
  (unless (and (and (looking-back zk-id-regexp
                                  (line-beginning-position))
                    (looking-at "$"))
               (save-excursion
                 (beginning-of-line)
                 (zk-index--button-at-point-p)))
    (let ((inhibit-read-only t))
      (unless (zk-desktop-delete-region-maybe)
        (funcall #'delete-char (or current-prefix-arg 1))))))

(defun zk-desktop-delete-backward-char ()
  "Wrapper around `delete-backward-char' for `zk-desktop-mode'."
  (interactive)
  (unless (and (looking-back zk-id-regexp
                             (line-beginning-position))
               (save-excursion
                 (beginning-of-line)
                 (zk-index--button-at-point-p)))
    (let ((inhibit-read-only t))
      (unless (zk-desktop-delete-region-maybe)
        (funcall #'delete-char (or current-prefix-arg -1))))))

(defun zk-desktop-kill-line ()
  "Kill line in `zk-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (if (not (zk-index--button-at-point-p))
        (kill-line)
      (kill-region (line-beginning-position)
                   (line-end-position)))))

(defun zk-desktop-kill-region ()
  "Wrapper around `kill-region' for `zk-desktop-mode'."
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

(defun zk-desktop-yank ()
  "Wrapper around `yank' for `zk-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (yank)
    (when zk-desktop-make-buttons
      (zk-desktop-make-buttons))))

(provide 'zk-desktop)

;;; zk-desktop.el ends here
