;;; zk-consult.el --- Consult integration for zk  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 4, 2022
;; License: GPL-3.0-or-later
;; Version: 0.2
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "24.3") (consult "0.14"))


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

;; This package offers two functions as alternatives to the default search
;; functions. Instead of displaying search results in a 'grep' buffer, these
;; functions display search results using Consult.

;; To use, load Consult and evaluate the following:

;; (with-eval-after-load 'consult
;;   (with-eval-after-load 'zk
;;     (require 'zk-consult)))

;; Then set one or both of the following variables:

;; (setq zk-search-function 'zk-consult-grep)
;; (setq zk-tag-search-function 'zk-consult-grep-tag-search)

;;; Code:

(require 'zk)
(require 'consult)

;;; Consult-Grep Functions

(defun zk-consult-grep (&optional initial)
  "Search 'zk-directory' with 'consult-grep'.
With option for INITIAL input when called non-interactively."
  (interactive "sSearch: ")
  (if initial
      (consult-grep zk-directory (format "%s" initial))
    (consult-grep zk-directory)))

(defun zk-consult-grep-tag-search (tag)
  "Search for TAG in 'zk-directory' using 'consult-grep'.
Select TAG, with completion, from list of all tags in zk notes."
  (interactive (list (completing-read "Tag: " (zk--grep-tag-list))))
  (consult-grep zk-directory tag))

;;; Current Notes Consult Source

(defvar zk-consult-source
  `(:name "zk"
          :narrow (?z . "zk - current notes")
          :hidden n
          :category buffer
          :history zk-history
          :state ,#'consult--buffer-state
          :items ,(lambda ()
                    (remq nil
                        (mapcar
                         (lambda (x)
                           (when
                               (and (buffer-file-name x)
                                    (zk-file-p (buffer-file-name x)))
                             (buffer-name x)))
                         (buffer-list))))))

;; evaluate the following to add this source to 'consult-buffer-sources':
;; (add-to-list 'consult-buffer-sources 'zk-consult-source 'append)

(defun zk-consult-current-notes ()
  "Select a currently open note using 'consult-buffer'.
To use, set the variable 'zk-current-notes-function' to the
name of this function."
  (minibuffer-with-setup-hook
      '(lambda ()
         (setq unread-command-events
               (append unread-command-events (list ?z 32))))
    (consult-buffer)))

(provide 'zk-consult)

;;; zk-consult.el ends here
