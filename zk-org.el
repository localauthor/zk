;;; zk-org.el --- Org-Link integration for zk.el  -*- lexical-binding: t; -*-

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

;; This adds click-to-follow links for notes in 'org-mode'.

;;; Code:

(require 'zk)
(require 'org)

(defun zk-try-to-follow-link (func &optional arg)
  "When 'org-open-at-point' FUNC fails, try 'zk-follow-ilnk-at-point'.
Optional ARG."
  (let ((org-link-search-must-match-exact-headline t))
    (condition-case nil
	(apply func arg)
      (error (zk-follow-link-at-point)))))

(advice-add 'org-open-at-point :around #'zk-try-to-follow-link)

(provide 'zk-org)

;;; zk-org.el ends here
