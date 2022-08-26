;;; zk-link-hint.el --- Link-Hint integration for zk  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 4, 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "24.4") (link-hint "0.1") (zk "0.4"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides integration between link-hint.el and zk. To use,
;; arrange for it to be loaded once both of those are loaded:

;; (with-eval-after-load 'link-hint
;;   (with-eval-after-load 'zk
;;     (require 'zk-link-hint)))

;;; Code:

(require 'zk)
(require 'link-hint)

(defun zk-link-hint--zk-link-at-point-p ()
  "Return the ID for the zk-link at the point or nil."
  (and (zk--id-at-point)
       (thing-at-point-looking-at (zk-link-regexp))))

(defun zk-link-hint--next-zk-link (bound)
  "Find the next zk-link.
Only search the range between just after the point and BOUND."
  (link-hint--next-regexp zk-id-regexp bound))

(link-hint-define-type 'zk-link
  :next #'zk-link-hint--next-zk-link
  :at-point-p #'zk-link-hint--zk-link-at-point-p
  :open #'zk-follow-link-at-point
  :copy #'kill-new)

(push 'link-hint-zk-link link-hint-types)


;;; link-hint-aw-select support

(with-eval-after-load 'link-hint-aw-select

  (link-hint-define-type 'zk-link
    :aw-select #'link-hint--aw-select-zk-link)

  (defun link-hint--aw-select-zk-link (id)
    (with-demoted-errors "%s"
      (if (> (length (aw-window-list)) 1)
          (let ((window (aw-select nil))
                (buffer (current-buffer))
                (new-buffer))
            (zk-follow-link-at-point id)
            (setq new-buffer
                  (current-buffer))
            (switch-to-buffer buffer)
            (aw-switch-to-window window)
            (switch-to-buffer new-buffer))
        (link-hint-open-link-at-point))))

  ;; add exception for zk-index buttons
  (defun link-hint--aw-select-button (_link)
    (with-demoted-errors "%s"
      (if (> (length (aw-window-list)) 1)
          (let ((window (aw-select nil))
                (buffer (current-buffer))
                (new-buffer))
            (if (re-search-forward zk-id-regexp (line-end-position))
                (zk-follow-link-at-point (match-string-no-properties 0))
              (push-button))
            (setq new-buffer
                  (current-buffer))
            (switch-to-buffer buffer)
            (aw-switch-to-window window)
            (switch-to-buffer new-buffer))
        (link-hint-open-link-at-point)))))

;;; link-hint-preview support

(with-eval-after-load 'link-hint-preview

  (link-hint-define-type 'zk-link
    :preview #'link-hint-preview-zk-link)

  (defun link-hint-preview-zk-link (&optional id)
    "Pop up a frame containing zk-file for ID at point.
Set pop-up frame parameters in 'link-hint-preview-frame-parameters'."
    (interactive)
    (let* ((id (or (zk--id-at-point)
                   (zk-index--button-at-point-p)))
           (file (zk--parse-id 'file-path id))
           (buffer (get-file-buffer file))
           (frame (selected-frame)))
      (if (get-file-buffer file)
          (setq link-hint-preview--kill-last nil)
        (setq buffer (find-file-noselect file))
        (setq link-hint-preview--kill-last t))
      (display-buffer-pop-up-frame
       buffer
       `((pop-up-frame-parameters . ,(link-hint-preview--params 'delete-before frame))
         (dedicated . t)))
      (with-current-buffer buffer
        (setq-local link-hint-preview--origin-frame frame)
        (link-hint-preview-mode))))

  (defalias 'zk-preview 'link-hint-preview-zk-link)

  (link-hint-define-type 'button
    :preview #'link-hint-preview-button)

;; add exception for zk-index buttons
(defun link-hint-preview-button ()
  (interactive)
  (let ((buffer (current-buffer))
        (frame (selected-frame))
        (new-buffer))
    (if-let (id (zk-index--button-at-point-p))
        (progn
          (if (get-file-buffer (zk--parse-id 'file-path id))
              (setq link-hint-preview--kill-last nil)
            (setq link-hint-preview--kill-last t))
          (zk-follow-link-at-point id))
      (push-button))
    (setq new-buffer
          (current-buffer))
    (switch-to-buffer buffer)
    (display-buffer-pop-up-frame
     new-buffer
     `((pop-up-frame-parameters . ,(link-hint-preview--params 'delete-before frame))
       (dedicated . t)))
    (with-current-buffer new-buffer
      (setq-local link-hint-preview--origin-frame frame)
      (link-hint-preview-mode))))
)

(provide 'zk-link-hint)

;;; zk-link-hint.el ends here
