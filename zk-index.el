;;; zk-index.el --- Index and desktop for zk         -*- lexical-binding: t; -*-

;;; Code:

(require 'zk)
(require 'zk-extras)

;;; ZK Desktop

;;;###autoload
(defun zk-index-desktop ()
  "Open ZK-Desktop."
  (interactive)
  ;; make new desktop, or add to existing
  ;; list existing desktops
  (let ((buffer "*ZK-Desktop*"))
    (when (string= (buffer-name) "*ZK-Index*")
        (progn
          (read-only-mode -1)
          (let ((items (if (use-region-p)
                           (buffer-substring (save-excursion
                                               (goto-char (region-beginning))
                                               (line-beginning-position))
                                             (save-excursion
                                               (goto-char (region-end))
                                               (line-end-position)))
                         (buffer-substring (line-beginning-position)(line-end-position)))))
            (read-only-mode)
            (unless (get-buffer buffer)
              (generate-new-buffer buffer))
            (with-current-buffer buffer
              (goto-char (point-max))
              (newline)
              (insert items)
              (newline)
              (unless (bound-and-true-p truncate-lines)
                (toggle-truncate-lines))
              (goto-char (point-max))))))
      (unless (get-buffer-window buffer 'visible)
        (progn
          (display-buffer buffer
                          '(display-buffer-pop-up-frame
                            (inhibit-switch-frame . t)))
          (set-frame-position (selected-frame) 850 80)))))

;;; ZK Index View

;; a semi-persistent curated selection of notes
;; for more persistent selections, use ZK Desktop

;; make focus and search case-insensitive
;; but how to stack sort functions? luhmann and modified, for example

(defvar zk-index-last-sort-function nil
  "Set to the name of the last sort function used.
If no sort function, gets set to nil.")

(defvar zk-index-last-format-function nil
  "Set to the name of the last format function used.
If no format function, gets set to nil.")

;; (zk-index (zk--directory-files t) nil 'zk-index--sort-latest-modified)

;;;###autoload
(defun zk-index-luhmann ()
  "Open index for Luhmann-style notes."
  (interactive)
  (zk-index (zk--luhmann-files) 'zk--luhmann-format-candidates 'zk--luhmann-sort))

;; (zk-index-refresh (zk--directory-files t) nil 'zk-index--sort-latest-modified)

;;;###autoload
(defun zk-index (&optional files format-fn sort-fn)
  "Open ZK-Index, with optional FILES, FORMAT-FN, and SORT-FN."
  (interactive)
  (setq zk-index-last-format-function format-fn)
  (setq zk-index-last-sort-function sort-fn)
  (let ((buffer "*ZK-Index*")
        (list (if files files
                 (zk--directory-files t))))
    (unless (get-buffer buffer)
      (progn
        (zk-find-file-by-id zk-default-backlink)
        (generate-new-buffer buffer)
        (with-current-buffer buffer
          (zk-index-sort list format-fn sort-fn)
          (local-set-key (kbd "n") 'next-line)
          (local-set-key (kbd "p") 'previous-line)
          (local-set-key (kbd "o") 'link-hint-aw-select)
          (local-set-key (kbd "f") 'zk-index-focus)
          (local-set-key (kbd "l") 'zk-index-luhmann)
          (local-set-key (kbd "s") 'zk-index-search)
          (local-set-key (kbd "d") 'zk-index-desktop)
          (local-set-key (kbd "g") 'zk-index-refresh)
          (local-set-key (kbd "M") 'zk-index-sort-modified)
          (local-set-key (kbd "C") 'zk-index-sort-created)
          (local-set-key (kbd "L") 'zk-index-sort-luhmann)
          (local-set-key (kbd "q") 'delete-window)
          (read-only-mode 1)
          (toggle-truncate-lines)
          (goto-char (point-min)))))
    (when files
      (zk-index-refresh files format-fn sort-fn))
    (pop-to-buffer buffer)))

(defun zk-index-refresh (&optional files format-fn sort-fn)
  (interactive)
  (let ((files (if files files
                 (zk--directory-files t)))
        (sort-fn (if sort-fn sort-fn
                   nil))
        (line))
  (with-current-buffer "*ZK-Index*"
    (setq line (line-number-at-pos))
    (read-only-mode -1)
    (erase-buffer)
    (zk-index-sort files format-fn sort-fn)
    (goto-char (point-min))
    (unless (zk-index-narrowed-p)
      (forward-line line))
    (read-only-mode))))

(defun zk-index-sort (files &optional format-fn sort-fn)
  "Format zk-index candidates."
  (let* ((sort-fn (if sort-fn sort-fn
                   'zk-index--sort-modified))
        (files (nreverse (funcall sort-fn files))))
    (funcall #'zk-index-format files format-fn)))

(defun zk-index-format (files &optional format-fn)
  "Format zk-index candidates."
  (let* ((format-fn (if format-fn format-fn
                      'zk--completion-at-point-candidates))
         (candidates (funcall format-fn files)))
    (zk-index-insert candidates)))

(defun zk-index-insert (candidates)
  (dolist (file candidates)
    (string-match zk-id-regexp file)
    (insert-button file
                   'follow-link t
                   'face 'default
                   'action
                   `(lambda (_)
                      (progn
                        (view-file-other-window
                         (zk--parse-id 'file-path
                                       ,(match-string 0 file))))))
    (newline))
  (message "Notes: %s" (length candidates)))

(defun zk-index-quit ()
  (interactive)
  (if (zk-index-narrowed-p)
      (if (y-or-n-p "Refresh index? ")
          (zk-index)
        (delete-window))
    (delete-window)))

(defun zk-index-narrowed-p ()
  (with-current-buffer "*ZK-Index*"
    (if (< (count-lines (point-min) (point-max))
           (length (zk--id-list)))
        t nil)))

;; Index Sort Functions

(defun zk-index-current-file-list ()
  "Return narrowed list of candidates.
Asks whether to search all files or only those in current index."
  (interactive)
  (let* ((ids (zk-index-current-id-list))
         (files (zk--parse-id 'file-path ids)))
    (when files
      files)))

(defun zk-index--sort-created (list)
  "Sort LIST for latest created."
  (let ((ht (make-hash-table :test #'equal :size 5000)))
    (dolist (x list)
      (puthash x (zk--parse-file 'id x) ht))
    (sort list
          (lambda (a b)
            (let ((one
                   (gethash a ht))
                  (two
                   (gethash b ht)))
              (string< two one))))))

(defun zk-index--sort-modified (list)
  "Sort LIST for latest modification."
  (let ((ht (make-hash-table :test #'equal :size 5000)))
    (dolist (x list)
      (puthash x (file-attribute-modification-time (file-attributes x)) ht))
    (sort list
          (lambda (a b)
            (let ((one
                   (gethash a ht))
                  (two
                   (gethash b ht)))
              (time-less-p two one))))))

(defun zk-index-sort-modified ()
  (interactive)
  (zk-index-refresh (zk-index-current-file-list)
                    zk-index-last-format-function
                    #'zk-index--sort-modified))

(defun zk-index-sort-created ()
  (interactive)
  (zk-index-refresh (zk-index-current-file-list)
                    zk-index-last-format-function
                    #'zk-index--sort-created))

(defun zk-index-sort-luhmann ()
  (interactive)
  (if (eq zk-index-last-format-function 'zk--luhmann-format-candidates)
      (zk-index-refresh (zk-index-current-file-list)
                        zk-index-last-format-function
                        #'zk--luhmann-sort)
    (error "Not Luhmann format - press \"l\" to switch")))
  
(defun zk-index--sort-size (list)
  "Sort LIST for latest modification."
  (sort list
        (lambda (a b)
          (let ((one  
                 (file-attribute-size (file-attributes a)))
                (two
                 (file-attribute-size (file-attributes b))))
            (time-less-p two one)))))

;;;; Index Narrowing Functions

(defun zk-index-narrowed-files ()
  "Return narrowed list of candidates.
Asks whether to search all files or only those in current index."
  (let* ((command this-command)
         (scope (if (zk-index-narrowed-p)
                    (if (y-or-n-p "Query subset only? ")
                        (zk-index-current-id-list)
                      (zk--id-list))
                  (zk--id-list)))
         (string (read-string "Search: "))
         (query (cond
                 ((eq command 'zk-index-focus)
                  (mapcar
                   (lambda (x)
                     (zk--parse-file 'id x))
                   (zk--directory-files t (regexp-quote string))))
                 ((eq command 'zk-index-search)
                  (zk--grep-id-list string))))
         (focus
          (mapcar
           (lambda (x)
             (when (member x scope)
               x))
           query))
         (files (zk--parse-id 'file-path (remq nil focus))))
    (if files files
      (error "No matches for \"%s\"" string))))

(defun zk-index-current-id-list ()
  "Return list of IDs for current index, as filepaths."
  (let (ids)
    (with-current-buffer "*ZK-Index*"
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (while (re-search-forward zk-id-regexp nil t)
            (push (match-string-no-properties 0) ids)))
        ids))))

(defun zk--grep-id-list (str)
  "Return a list of IDs for files containing STR."
  (let ((files (zk--grep-file-list str)))
    (mapcar
     (lambda (x)
       (zk--parse-file 'id x))
     files)))

;;;; zk-index-focus
;; narrow index based on search of note titles (case sensitive)
;; an alternative to consult-focus-lines

(defun zk-index-focus ()
  "Narrow index based on search of note titles."
  (interactive)
  (zk-index-refresh (zk-index-narrowed-files) zk-index-last-format-function zk-index-last-sort-function))

;;;; zk-index-search
;; narrow index based on search of notes' full text

(defun zk-index-search ()
  "Narrow index based on search of note titles."
  (interactive)
  (zk-index-refresh (zk-index-narrowed-files) zk-index-last-format-function zk-index-last-sort-function))

(provide 'zk-index)

;;; zk-index.el ends here
