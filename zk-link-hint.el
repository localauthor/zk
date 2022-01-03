;;; zk-link-hint.el --- Link-Hint integration for zk  -*- lexical-binding: t; -*-

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

;; This package provides integration between link-hint.el and zk. To use,
;; arrange for it to be loaded once both of those are loaded:

;; (with-eval-after-load 'link-hint
;;   (with-eval-after-load 'zk
;;     (require 'zk-link-hint)))

;;; Code:

(require 'zk)
(require 'link-hint)

(defun link-hint--zk-id-at-point-p ()
  (thing-at-point-looking-at zk-id-regexp))

(defun link-hint--next-zk-id (&optional bound)
  (link-hint--next-regexp zk-id-regexp bound))

(defun link-hint--open-zk-id ()
  (zk-follow-id-at-point))

(link-hint-define-type 'zk-id
  :next #'link-hint--next-zk-id
  :at-point-p #'link-hint--zk-id-at-point-p
  :open #'link-hint--open-zk-id
  :copy #'kill-new)

(push 'link-hint-zk-id link-hint-types)

(provide 'zk-link-hint)

;;; zk-link-hint.el ends here
