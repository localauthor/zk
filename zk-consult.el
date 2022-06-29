;;; zk-consult.el --- Consult integration for zk  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 4, 2022
;; License: GPL-3.0-or-later
;; Version: 0.2
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "27.1") (consult "0.14"))


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

;; This package offers several integrations of Consult with zk:


;; 1. Two functions as alternatives to the default 'zk-grep' functions:
;; 'zk-consult-grep' and 'zk-consult-grep-tag-search'. Instead of displaying
;; search results in a 'grep' buffer, these functions display search results
;; using Consult.

;;   To use these alternative functions, set one or both of the following variables:
;;   (setq zk-grep-function 'zk-consult-grep)
;;   (setq zk-tag-grep-function 'zk-consult-grep-tag-search)


;; 2. Two ways of accessing a list of currently open notes via 'consult-buffer':
;; first through 'consult-buffer' itself, accessible via narrowing with the
;; 'z' key; second, as alternative to the command 'zk-current-notes', such
;; that it brings up the Consult buffer source directly.

;;   To add the zk Consult buffer source to 'consult-buffer-sources', evaluate:
;;   (add-to-list 'consult-buffer-sources 'zk-consult-source 'append)

;;   To set the alternative 'zk-current-note' function, evaluate:
;;   (setq zk-current-notes-function 'zk-consult-current-notes)


;; 3. Note previews when selecting a zk-file in the minibuffer.

;;   To implement note previews, evaluate:
;;   (setq zk-select-file-function 'zk-consult-select-file)

;;   NOTE: The list of functions for which previews will be shown can be
;;   customized by amending the functions listed in the variable
;;   'zk-consult-preview-functions'.


;; To load this package, put 'zk-consult.el' into your load path, load Consult,
;; and evaluate the following:

;; (with-eval-after-load 'consult
;;   (with-eval-after-load 'zk
;;     (require 'zk-consult)))

;;; Code:

(require 'zk)
(require 'consult)

;;; Customizations

(defcustom zk-consult-preview-functions
  '(zk-find-file
    zk-find-file-by-full-text-search
    zk-current-notes
    zk-links-in-note
    zk-insert-link
    zk-copy-link-and-title
    zk-backlinks
    zk-unlinked-notes)
  "List of functions for which previews should be rendered."
  :group 'zk
  :type '(repeat function))

;;; Consult-Grep Functions

(defun zk-consult-grep (&optional initial)
  "Search 'zk-directory' with 'consult-grep'.
With option for INITIAL input when called non-interactively."
  (interactive)
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

(defun zk-consult-current-notes ()
  "Select a currently open note using 'consult-buffer'.
To use, set the variable 'zk-current-notes-function' to the
name of this function."
  (minibuffer-with-setup-hook
      '(lambda ()
         (setq unread-command-events
               (append unread-command-events (list ?z 32))))
    (consult-buffer)))

;;; Consult Select File with Preview

(defun zk-consult-select-file (&optional prompt list)
  "Wrapper around `consult--read' to select a zk-file.
Offers candidates from 'zk--directory-files', or from LIST when
supplied. Can take a PROMPT argument."
  (let* ((files (if list list
                  (zk--directory-files t)))
         (prompt (if prompt prompt
                 "Select File: ")))
     (consult--read
      files
      :prompt prompt
      :sort t
      :require-match t
      :group 'zk--group-function
      :category 'zk-file
      :state (consult--file-preview)
      :preview-key (zk-consult--preview-functions)
      :history 'zk-history)))

(defun zk-consult--preview-functions ()
  (when (member this-command zk-consult-preview-functions)
    consult-preview-key))

(provide 'zk-consult)

;;; zk-consult.el ends here
