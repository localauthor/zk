;;; zk-org.el --- Org-link integration for zk        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Grant Rosson

;; Author: Grant Rosson <grantrosson@gmail.com>
;; Keywords:

;;; zk-org -- Org-link integration for zk -*- lexical-binding: t -*- -

;; Copyright (C) 2022 Grant Rosson

;; Author: Grant Rosson  <https://github.com/localauthor>
;; Created: June 25, 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "27.1"))


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

;; This package provides for the use of org-links with zk, through the
;; creation of a `zk' org-link type. To use, add the following to your
;; init.el:

;; (with-eval-after-load 'org
;;   (with-eval-after-load 'zk
;;     (require 'zk-org)))

;;; Code:

(require 'zk)

(declare-function org-link-set-parameters "ol.el")
(declare-function org-link-store-props "ol.el")

(org-link-set-parameters "zk"
			 :follow #'zk-org--follow
                         ;;:complete #'zk-org--complete
			 :store #'zk-org--store)

;; Set up org-style link format by setting defcustoms
(setq zk-link-format "[[zk:%s]]" )
(setq zk-link-and-title-format "[[zk:%i][%t]]")
(setq zk-link-regexp (format (regexp-quote zk-link-format) zk-id-regexp))
(setq zk-completion-at-point-format  "[[zk:%i][%t]]")
(setq zk-enable-link-buttons nil)

;; FIX
;; zk-completion-at-point?


(defun zk-org--follow (id)
  "Follow an zk ID."
  (let ((file (zk--parse-id 'file-path id)))
    (if file
        (find-file file)
      (user-error "Could not find zk-note with ID %s" id))))

(defun zk-org--store ()
  "Store a link to a zk-note."
  (when (zk-file-p)
    (let ((id (zk--id-at-point)))
      (org-link-store-props
       :type "zk"
       :link (concat "zk:" id)
       :description (zk--parse-id 'title id)))))

(provide 'zk-org)

;;; zk-org.el ends here
