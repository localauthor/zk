;;; zk-org.el --- Org-Link integration for zk.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 4, 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "24.3"))

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

(defun zk-org-try-to-follow-link (func &optional arg)
  "When 'org-open-at-point' FUNC fails, try 'zk-follow-ilnk-at-point'.
Optional ARG."
  (let ((org-link-search-must-match-exact-headline t))
    (condition-case nil
	(apply func arg)
      (error (zk-follow-link-at-point)))))

(advice-add 'org-open-at-point :around #'zk-org-try-to-follow-link)

(provide 'zk-org)

;;; zk-org.el ends here
