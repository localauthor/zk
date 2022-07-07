;;; zk-citar.el --- Citar integration                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: July 7, 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "27.1") (citar "1.0") (zk "0.4"))
;; Keywords: tools, extensions

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

;; This package provides integration between citar and zk, allowing  To use,
;; arrange for the package to be loaded:

;; (with-eval-after-load 'citar
;;   (with-eval-after-load 'zk
;;     (require 'zk-citar)))

;; And set the following variable:

;; (setq citar-notes-source 'zk)

;; Then set `zk-citar-citekey-regexp' to a regular expression that will match
;; the citekeys in the title of your notes.

;; You can also customize the template for titling new notes, with the
;; variable `zk-citar-title-template'.

;;; Code:

(require 'zk)
(require 'citar)

;;;; Register citar-note-source

(citar-register-notes-source 'zk '(:name "zk"
                                   :category zk-file
                                   :hasnote zk-citar-file-has-notes
                                   :action zk-citar-open-note
                                   :items zk-citar--get-note-files))

;;;; Custom variables

(defgroup zk-citar nil
  "Citar integration for zk."
  :group 'text
  :group 'files
  :prefix "zk-citar")

(defcustom zk-citar-citekey-regexp "[a-z]+[0-9]\\{4\\}[a-z]?"
  "Regular expression to match citekeys in note file-names."
  :type 'string)

(defcustom zk-citar-title-template "${=key=} - ${author} - ${title} (${year})"
  "Template for formatting new note titles.
Must include \"${=key=}\"."
  :type 'string)

;;;; hasnote

(defun zk-citar-file-has-notes (&optional _entries)
  "Return predicate testing whether cite key has associated notes."
  (let ((files (zk-citar--get-notes-hash)))
    (lambda (key)
      (gethash key files))))

(defun zk-citar--get-notes-hash (&optional keys)
  "Return hash-table with KEYS with file notes."
  (let ((files (make-hash-table :test 'equal))
        (filematch (or (car keys) zk-citar-citekey-regexp)))
    (prog1 files
      (dolist (file (zk--directory-files t filematch))
        (let ((key (or (car keys)
                       (and (string-match zk-citar-citekey-regexp file)
                            (match-string 0 file)))))
          (push file (gethash key files)))))))

;;;; action

(defun zk-citar-open-note (key entry)
  "Open a note file from KEY and ENTRY."
  (let ((file (car (zk-citar--get-note-files (list key)))))
    (if file
        (funcall 'find-file file)
      (when (y-or-n-p "No note associated - create one?")
        (let* ((title
                (subst-char-in-string ?: ?-
                                      (citar-format--entry
                                       zk-citar-title-template
                                       entry))))
          (zk-new-note title))))))

;;;; items

(defun zk-citar--get-note-files (keys)
  "Return list of notes associated with KEYS."
  (let ((notehash (zk-citar--get-notes-hash keys)))
    (flatten-list (map-values notehash))))


(provide 'zk-citar)
;;; zk-citar.el ends here
