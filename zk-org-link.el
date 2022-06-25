;;; zk-org-link.el --- Org-link integration for zk        -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
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
;;     (require 'zk-org-link)))

;; Thanks to @jgru for getting this started, and @protesilaos for the
;; push to finish it.

;;; Code:

(require 'zk)

(declare-function org-link-set-parameters "ol.el")
(declare-function org-link-store-props "ol.el")

(org-link-set-parameters "zk"
			 :follow #'zk-org-link--follow
                         :export #'zk-org-link--export
			 :store #'zk-org-link--store
                         :complete #'zk-org-link--complete
                         :help-echo #'zk-org-link--help-echo)

;; Set up org-style link format by setting variables
(setq zk-link-format "[[zk:%s]]")
(setq zk-link-and-title-format "[[zk:%i][%t]]")
(setq zk-link-regexp (format (regexp-quote zk-link-format) zk-id-regexp))
(setq zk-enable-link-buttons nil)

(defun zk-org-link--follow (id)
  "Follow an zk ID."
  (let ((file (zk--parse-id 'file-path id)))
    (if file
        (find-file file)
      (user-error "Could not find zk-note with ID %s" id))))

(defun zk-org-link--export (link description format)
  "Export a `zk:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend."
  (let* ((id link)
         (path (zk--parse-id 'file-path id))
         (p (file-name-sans-extension path))
	 (desc (or description (concat "zk:" id))))
    (cond
     ((eq format 'html) (format "<a target=\"_blank\" href=\"%s.html\">%s</a>" p desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "[%s] <zk:%s>" desc path))
     ((eq format 'md) (format "[%s](%s.md)" desc p))
     (t path))))

(defun zk-org-link--store ()
  "Store a link to a zk-note."
  (when (zk-file-p)
    (let ((id (zk--id-at-point)))
      (org-link-store-props
       :type "zk"
       :link (concat "zk:" id)
       :description (zk--parse-id 'title id)))))

(defun zk-org-link--complete ()
  "Like `zk-insert-link' but for Org integration.
This lets the user complete a link through the `org-insert-link'
interface by first selecting the `zk:' hyperlink type."
  (concat
   "zk:"
   (zk--parse-file 'id (zk--select-file))))

(defun zk-org-link--help-echo (_win _obj pos)
  "Generate help-echo tooltip for `zk:' Org links.
Takes WIN, OBJ, and POS arguments."
  (save-excursion
    (goto-char pos)
    (re-search-backward zk-id-regexp)
    (format
     "%s"
     (zk--parse-id
      'title
      (match-string 0)))))

(provide 'zk-org-link)

;;; zk-org-link.el ends here
