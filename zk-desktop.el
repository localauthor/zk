;;; zk-desktop.el --- Desktop environment for zk   -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Grant Rosson

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

(defcustom zk-desktop-prefix ""
  "String to prepend to note names in ZK-Desktop."
  :type 'string)

(defcustom zk-desktop-button-format "%t %i"
  "Format string for notes in ZK-Desktop.
This is the format for the buttons in ZK-Desktop buffer; use
`zk-desktop-prefix' to add arbitary text that should not be
part of the button itself.

See `zk-format-function' and `zk-format-id-and-title' for
valid control strings."
  :type 'string)

(defcustom zk-desktop-invisible-ids t
  "If non-nil, IDs will not be visible in the index."
  :type 'boolean)

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


;;; Declarations

(defvar zk-desktop-current nil)


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
  :init-value nil
  :keymap zk-desktop-map
  (zk-desktop-make-buttons)
  (when-let ((mode zk-desktop-major-mode))
    (funcall mode))
  ;;(setq truncate-lines t)
  (setq-local zk-desktop-mode t))

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
  "Select a ZK-Desktop to work with."
  (interactive)
  (unless zk-desktop-directory
    (error "Please set `zk-desktop-directory' first"))
  (let* ((last-command last-command)
         (desktop
          (completing-read "Select or Create ZK-Desktop: "
                           (directory-files
                            zk-desktop-directory
                            nil
                            (concat
                             zk-desktop-basename
                             ".*"))
                           nil nil nil nil
                           (concat zk-desktop-basename " ")))
         (file (concat zk-desktop-directory "/" desktop)))
    (if (file-exists-p (expand-file-name file))
        (setq zk-desktop-current
              (find-file-noselect file))
      (progn
        (generate-new-buffer desktop)
        (setq zk-desktop-current desktop)))
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
    'keymap zk-desktop-button-map
    'action 'zk-desktop-button-action
    'face 'zk-desktop-button
    'cursor-face 'highlight))

(defun zk-desktop--normalize-line (id title missing)
  "Prepare the current line in ZK-Desktop buffer for a button.
ID is the zk-ID; TITLE is either existing text in the buffer
or ID's current title; MISSING, if non-nil, means the ID is
not found in the current `zk-directory'.

Return a tuple of bounds (BEG . END) for the actual
zk-desktop-button according to `zk-desktop-button-format'.

This is a helper function used by `zk-desktop-make-buttons'
and should not be called directly."
  (let* ((lbeg   (line-beginning-position))
         (lend   (line-end-position)))
    (delete-region lbeg lend)
    (cons (point)
          (progn
            (insert (zk--format zk-desktop-button-format id title))
            (point)))))

(defun zk-desktop--make-button (id beg end)
  "Make text between BEG and END into a ZK-Desktop button for zk ID.
BEG and END should be the bounds of the button itself, which
will inherit `zk-desktop-button' face and all text
properties defined for `zk-desktop-button' type."
  (make-text-button beg end
                    'type 'zk-desktop
                    'help-echo zk-desktop-help-echo-function)
  (if (not zk-desktop-invisible-ids)
      ;; I.e. can add text in front of the button
      (add-text-properties beg (+ beg 1) '(front-sticky nil))
    ;; Make both zk-links and plain zk-ids invisible
    (beginning-of-line)
    (cond ((re-search-forward (zk-link-regexp) (line-end-position) t)
           (replace-match (propertize (match-string 0) 'invisible t) nil t)
           ;; Org-mode requires more drastic measures
           (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                        'invisible t))
          ((re-search-forward id)
           ;; I.e. can add text in the rear of invis. IDs, but not in the front
           (replace-match (propertize id
                                      'read-only t
                                      'front-sticky t
                                      'rear-nonsticky t))))))

;;;###autoload
(defun zk-desktop-make-buttons ()
  "Re-make buttons in ZK-Desktop."
  (interactive)
  (unless (and (string-match-p zk-desktop-basename (buffer-name))
               (file-in-directory-p default-directory zk-desktop-directory))
    (user-error "Can only make buttons in Zk desktop file; %s isn't"
                (buffer-name)))
  (let* ((inhibit-read-only t)
         (zk-alist (zk--alist))
         (ids (zk--id-list nil zk-alist)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward zk-id-regexp nil t)
        (let* ((id      (match-string-no-properties 0))
               (title   (buffer-substring-no-properties
                         (line-beginning-position) (match-beginning 0)))
               (missing (not (member id ids)))
               (bounds  (zk-desktop--normalize-line id title missing)))
          (if (not missing)
              (zk-desktop--make-button id (car bounds) (cdr bounds))
            (end-of-line)
            (overlay-put (make-overlay (point) (point))
                         'before-string
                         (propertize" <- ID NOT FOUND" 'font-lock-face 'error))))
        (end-of-line)))))

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
                     (length zk-desktop-prefix)))
             (end (line-end-position))
             (title (buffer-substring beg end)))
        (format "%s" title)))))

;;; Commands

;;;###autoload
(defun zk-desktop-send-to-desktop (&optional arg)
  "Send notes from ZK-Index to ZK-Desktop.
In ZK-Index, works on note at point or notes in active region.
Also works on files or group of files in minibuffer, as ARG, and
on zk-id at point."
  (interactive)
  (unless zk-desktop-directory
    (error "Please set `zk-desktop-directory' first"))
  (let ((inhibit-read-only t)
        buffer
        (items
         (cond
          (arg (zk--formatted-string arg zk-desktop-button-format))
          ((eq major-mode 'zk-index-mode)
           (if (use-region-p)
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
          ((zk-file-p)
           (car
            (funcall
             zk-index-format-function
             (list buffer-file-name))))
          (t (user-error "No item to send to desktop")))))
    (if (and zk-desktop-current
             (buffer-live-p (get-buffer zk-desktop-current)))
        (setq buffer zk-desktop-current)
      (setq buffer (zk-desktop-select)))
    (unless (get-buffer buffer)
      (generate-new-buffer buffer))
    (with-current-buffer buffer
      (setq require-final-newline 'visit-save)
      (pcase zk-desktop-add-pos
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
      (zk-desktop-mode))
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
    (when zk-desktop-invisible-ids
      (zk-desktop-make-buttons))))

(defun zk-desktop-move-line-up ()
  "Move line at point up in ZK-Desktop buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (transpose-lines 1)
    (forward-line -2)
    (when zk-desktop-invisible-ids
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
    (zk-desktop-make-buttons)))


(provide 'zk-desktop)

;;; zk-desktop.el ends here
