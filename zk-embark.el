;;; zk-embark.el --- Embark integration for zk  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Grant Rosson

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

;; This package provides integration between Embark and zk. To use is,
;; arrange for it to be loaded once both of those are loaded:

;; (with-eval-after-load 'embark
;;   (with-eval-after-load 'zk
;;     (require 'zk-embark)))

;; After loading, two Embark target types become available, one called zk-id,
;; for use on zk IDs at point, and one called zk-file, for use in the context
;; of filename completion, whether in the minibuffer or in a completion
;; buffer.

;;; Code:

(require 'zk)
(require 'thingatpt)
(require 'embark)

(defvar embark-zk-id-map)
(defvar embark-zk-file-map)

;;;###autoload
(defun embark-target-zk-id-at-point ()
  "Target zk-id at point."
  (when (thing-at-point-looking-at zk-id-regexp)
    (let ((zk-id (thing-at-point 'symbol t)))
      `(zk-id ,zk-id . ,(bounds-of-thing-at-point 'symbol)))))

(add-to-list 'embark-target-finders 'embark-target-zk-id-at-point)

(eval-when-compile
  (embark-define-keymap embark-zk-id-map
    "Keymap for Embark zk-id actions
To be used on zk-ids at point in buffers."
    ("RET" zk-follow-link-at-point)))

(add-to-list 'embark-keymap-alist '(zk-id . embark-zk-id-map))

(eval-when-compile
  (embark-define-keymap embark-zk-file-map
    "Keymap for Embark zk-file actions.
To be used in the context of filename completion, as in the minibuffer."
    :parent embark-file-map
    ("i" zk-insert-link)
    ("f" zk-find-file)))

(add-to-list 'embark-keymap-alist '(zk-file . embark-zk-file-map))


(provide 'zk-embark)

;;; zk-embark.el ends here
