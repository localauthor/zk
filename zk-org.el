;;; zk-org -- Helper functions for working org-style zettelkasten  -*- lexical-binding: t -*-
; -*-

;; Copyright (C) 2022 jgru

;; Author: jgru <https://github.com/jgru> inspired by Ashlynn Anderson
;; Created: February 21, 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "24.4"))

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

;; This set of functions aims to implement support for org-style links within zk

;;; Code:
(require 'zk)

(with-eval-after-load 'org
  (org-link-set-parameters "zk"
			   :follow #'zk-org--follow-link
			   :store #'zk-org--store-link))

(defun zk-org--follow-link (id)
  "Follow an zk ID."
  (let ((file (zk--parse-id 'file-path id)))
        (if file
            (find-file file)
          (user-error "Could not find zk-note with ID %s" id))))

(defun zk-org--store-link ()
  "Store a link to a zk-note."
  (when (zk-file-p)
    (let ((zk-cur-id (zk--current-id)))
      ;;(print (concat "zk:" zk-curid))
      (org-store-link-props
        :type "zk"
        :link (concat "zk:" zk-curid)
        :description zk-cur-id))))

(provide 'zk-org)
