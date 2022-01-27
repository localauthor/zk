;;; zk-index.el --- Index and desktop for zk         -*- lexical-binding: t; -*-

;;; Commentary:

;; A semi-persistent, curated selection of notes titles.

;; For making more persistent, more curated selections, use zk-desktop.

;;; Code:

(require 'zk)
(require 'view)
(require 'zk-extras)

;; TODO make focus and search case-insensitive?

;; TODO remove anything not general, and in zk-extras

;; how to stack sort functions? luhmann and modified, for example

;; NOTE zk-index uses text buttons to allow buttons to be sent to zk-desktop
;; so link-hint becomes unmanagable, bc it finds ids and buttons
;; disable zk-link-hint in buffer?

;;; Variables

(defvar zk-index-last-sort-function nil
  "Set to the name of the last sort function used.
If no sort function, gets set to nil.")

(defvar zk-index-last-format-function nil
  "Set to the name of the last format function used.
If no format function, gets set to nil.")

(defvar zk-index-map
  (let ((map (make-sparse-keymap)))
          (define-key map (kbd "n") #'zk-index-next-line)
          (define-key map (kbd "p") #'zk-index-previous-line)
          (define-key map (kbd "RET") #'zk-index-open-note)
          (define-key map (kbd "v") #'zk-index-view-note)
          (define-key map (kbd "o") #'other-window)
          (define-key map (kbd "f") #'zk-index-focus)
          (define-key map (kbd "l") #'zk-index-luhmann)
          (define-key map (kbd "s") #'zk-index-search)
          (define-key map (kbd "d") #'zk-index-send-to-desktop)
          (define-key map (kbd "D") #'zk-index-switch-to-desktop)
          (define-key map (kbd "g") #'zk-index-refresh)
          (define-key map (kbd "M") #'zk-index-sort-modified)
          (define-key map (kbd "C") #'zk-index-sort-created)
          (define-key map (kbd "L") #'zk-index-sort-luhmann) ;; not general
          (define-key map (kbd "q") #'delete-window)
          map)
  "Keymap for ZK-Index buffer.")

(add-to-list 'minor-mode-map-alist (cons 'zk-index-map-enable zk-index-map))

(defvar-local zk-index-map-enable nil
  "Enable or disable 'zk-index-map'.")

(defvar zk-index-auto-scroll t
  "Enable automatically showing note at point in ZK-Index.")

(defvar zk-index-desktop-map
  (let ((map (make-sparse-keymap)))
          (define-key map [remap move-line-up] #'zk-index-move-line-up) ;; why is remap needed?
          (define-key map [remap move-line-down] #'zk-index-move-line-down)
          (define-key map (kbd "i") #'zk-index-switch-to-index)
          (define-key map (kbd "C-+") #'zk-index-desktop-edit-mode)
          (define-key map (kbd "o") #'other-window)
          (define-key map (kbd "q") #'delete-window)
          map)
  "Keymap for ZK-Desktop buffer.")

(add-to-list 'minor-mode-map-alist (cons 'zk-index-desktop-map-enable zk-index-desktop-map))

(defvar-local zk-index-desktop-map-enable nil
  "Enable or disable 'zk-index-desktop-map'.")

;;; Main Stack

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
        (unless (zk-directory-p)
          (zk-find-file-by-id zk-default-backlink))
        (generate-new-buffer buffer)
        (with-current-buffer buffer
          (zk-index--sort list format-fn sort-fn)
          (setq zk-index-map-enable t)
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
    (zk-index--sort files format-fn sort-fn)
    (goto-char (point-min))
    (unless (zk-index-narrowed-p)
      (forward-line line))
    (read-only-mode))))

(defun zk-index--sort (files &optional format-fn sort-fn)
  "Format zk-index candidates."
  (let* ((sort-fn (if sort-fn sort-fn
                   'zk-index--sort-modified))
        (files (nreverse (funcall sort-fn files))))
    (funcall #'zk-index--format files format-fn)))

(defun zk-index--format (files &optional format-fn)
  "Format zk-index candidates."
  (let* ((format-fn (if format-fn format-fn
                      'zk--completion-at-point-candidates))
         (candidates (funcall format-fn files)))
    (zk-index--insert candidates)))

(defun zk-index--insert (candidates)
  (dolist (file candidates)
    (string-match zk-id-regexp file)
    (insert-text-button file
                   'follow-link t
                   'face 'default
                   'action
                   `(lambda (_)
                      (progn
                        (find-file-other-window
                         (zk--parse-id 'file-path
                                       ,(match-string 0 file))))))
    (newline))
  (message "Notes: %s" (length candidates)))

(defun zk-index-narrowed-p ()
  (with-current-buffer "*ZK-Index*"
    (if (< (count-lines (point-min) (point-max))
           (length (zk--id-list)))
        t nil)))


;;; Keymap Commands

(defun zk-index-open-note ()
  (interactive)
  (push-button nil t)
  (other-window -1))

(defun zk-index-view-note ()
  (interactive)
  (push-button nil t)
  (view-mode)
  (other-window -1))

(defun zk-index-next-line ()
  "Move to next line.
If 'zk-index-auto-scroll' is non-nil, show note in other-window."
  (interactive)
  (if zk-index-auto-scroll
      (progn
        (other-window 1)
        (when view-mode
          (kill-buffer))
        (other-window -1)
        (forward-line)
        (push-button nil t)
        (view-mode)
        (other-window -1))
    (forward-line)))

(defun zk-index-previous-line ()
  "Move to previous line.
If 'zk-index-auto-scroll' is non-nil, show note in other-window."
  (interactive)
  (if zk-index-auto-scroll
      (progn
        (other-window 1)
        (when view-mode
          (kill-buffer))
        (other-window -1)
        (forward-line -1)
        (push-button nil t)
        (view-mode)
        (other-window -1))
    (forward-line -1)))

(defun zk-index-move-line-down ()
  "Move line at point down in 'read-only-mode'."
  (interactive)
  (read-only-mode -1)
   (forward-line 1)
   (transpose-lines 1)
   (forward-line -1)
  (read-only-mode))

(defun zk-index-move-line-up ()
  "Move line at point up in 'read-only-mode'."
  (interactive)
  (read-only-mode -1)
  (transpose-lines 1)
  (forward-line -2)
  (read-only-mode))

(defun zk-index-switch-to-desktop ()
  "Switch to *ZK-Desktop* in other frame."
  (interactive)
  (switch-to-buffer-other-frame "*ZK-Desktop*"))


;;; Index Luhmann

;;;###autoload
(defun zk-index-luhmann ()
  "Open index for Luhmann-style notes."
  (interactive)
  (zk-index (zk--luhmann-files) 'zk--luhmann-format-candidates 'zk--luhmann-sort))

(defun zk-index-sort-luhmann ()
  (interactive)
  (if (eq zk-index-last-format-function 'zk--luhmann-format-candidates)
      (zk-index-refresh (zk-index--current-file-list)
                        zk-index-last-format-function
                        #'zk--luhmann-sort)
    (error "Not Luhmann format - press \"l\" to switch")))


;;; Index Sort Functions

(defun zk-index-sort-modified ()
  (interactive)
  (zk-index-refresh (zk-index--current-file-list)
                    zk-index-last-format-function
                    #'zk-index--sort-modified))

(defun zk-index-sort-created ()
  (interactive)
  (zk-index-refresh (zk-index--current-file-list)
                    zk-index-last-format-function
                    #'zk-index--sort-created))


(defun zk-index--current-file-list ()
  "Return narrowed list of candidates.
Asks whether to search all files or only those in current index."
  (interactive)
  (let* ((ids (zk-index--current-id-list))
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

(defun zk-index--sort-size (list)
  "Sort LIST for latest modification."
  (sort list
        (lambda (a b)
          (let ((one
                 (file-attribute-size (file-attributes a)))
                (two
                 (file-attribute-size (file-attributes b))))
            (time-less-p two one)))))


;;; Querying Functions

(defun zk-index-query-files ()
  "Return narrowed list of notes, based on focus or search query."
  (let* ((command this-command)
         (scope (if (zk-index-narrowed-p)
                    (zk-index--current-id-list)
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

(defun zk-index--current-id-list ()
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


;;; Index Focus

;; narrow index based on search of note titles (case sensitive)
;; an alternative to consult-focus-lines

(defun zk-index-focus ()
  "Narrow index based on search of note titles."
  (interactive)
  (zk-index-refresh (zk-index-query-files) zk-index-last-format-function zk-index-last-sort-function))


;;; Index Search
;; narrow index based on search of notes' full text

(defun zk-index-search ()
  "Narrow index based on search of note titles."
  (interactive)
  (zk-index-refresh (zk-index-query-files) zk-index-last-format-function zk-index-last-sort-function))


;;; Desktop
;; index's less rigid cousin; a place to collect and order note titles

;; TODO allow multiple, saved desktops?
;; TODO make new desktop or add to existing?
;; TODO list existing desktops?

;;;###autoload
(defun zk-index-send-to-desktop ()
  "Send notes at point, or notes in active region, to ZK-Desktop."
  (interactive)
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
              (read-only-mode -1)
              (setq zk-index-desktop-map-enable t)
              (goto-char (point-max))
              (newline)
              (insert items)
              (unless (bound-and-true-p truncate-lines)
                (toggle-truncate-lines))
              (goto-char (point-max))
              (beginning-of-line)
              (read-only-mode)))))
    (unless (get-buffer-window buffer 'visible)
      (special-display-popup-frame "*ZK-Desktop*"
                                   '((top . 80)
                                     (left . 850)
                                     (width . 80)
                                     (height . 35)
                                     (no-focus-on-map . t))))))

(defun zk-index-switch-to-index ()
  "Switch to ZK-Index buffer."
  (interactive)
  (when (get-buffer "*ZK-Index*")
    (switch-to-buffer-other-frame "*ZK-Index*")))

(defun zk-index-desktop-edit-mode ()
  "Toggle 'read-only-mode' in ZK-Desktop."
  (interactive)
  (if zk-index-desktop-map-enable
      (progn
        (read-only-mode -1)
        (local-set-key (kbd "C-+") #'zk-index-desktop-edit-mode)
        (setq zk-index-desktop-map-enable nil)
        (message "ZK-Desktop Edit Mode"))
    (progn
      (read-only-mode)
      (local-unset-key (kbd "C-+"))
      (setq zk-index-desktop-map-enable t))))

(provide 'zk-index)

;;; zk-index.el ends here
