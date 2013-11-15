(require 'init)

(setq ibuffer-formats
      `((mark
         modified
         read-only
         " "
         (name 30 30 :left :elide)
         ,(propertize "| " 'font-lock-face ibuffer-title-face)
         (mode 10 10 :left)
         ,(propertize " | " 'font-lock-face ibuffer-title-face)
         filename)))

(defconst gcs-ibuffer-fontification-alist
  '((ruby-mode . font-lock-string-face)
    (sh-mode . font-lock-string-face)
    (objc-mode . font-lock-constant-face)
    (c-mode . font-lock-constant-face)
    (java-mode . font-lock-constant-face)
    (emacs-lisp-mode . font-lock-variable-name-face)
    (org-mode . font-lock-negation-char-face)
    (term-mode . font-lock-doc-string-face)))
    
(setq ibuffer-fontification-alist
      `(,@(mapcar (lambda (b)
                    `(9999 (eq major-mode ',(car b)) ,(cdr b)))
                  gcs-ibuffer-fontification-alist)
        (90 (string-match "magit" (symbol-name major-mode))
            font-lock-function-name-face)
        (90 (or (string-match "^*" (buffer-name))
                (memq major-mode ibuffer-help-buffer-modes))
            font-lock-comment-face)))

(define-ibuffer-filter project
  "Filter by buffer's eproject-root"
  (:description "project"
   :reader (read-from-minibuffer "Filter by project root: "))
  (with-current-buffer buf (string-equal eproject-root (file-truename qualifier))))

(defun gcs-get-eproject-filter-groups ()
  (let (projects-with-buffers-filters)
    ;; Fill a list of filters for projects that have open buffers
    (mapc (lambda (project)
            (when (assoc (cdr project) (eproject--project-buffers))
              (add-to-list 'projects-with-buffers-filters (list (first project) `(project . ,(cdr project))))))
          (eproject-projects))
    ;; Return a list of filters for projects, with projects that have buffers
    ;; at the start of the list.
    projects-with-buffers-filters))

(defun gcs-setup-eproject-filter-groups ()
  (interactive)
  (setq ibuffer-saved-filter-groups
        `(("default"
           ,@(gcs-get-eproject-filter-groups))))
  (when (get-buffer "*Ibuffer*") (kill-buffer "*Ibuffer*")))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")
    
    (face-remap-add-relative 'default 'font-lock-comment-face)
    
    (copy-face 'font-lock-keyword-face 'tempface )
    (setq ibuffer-filter-group-name-face 'tempface)
    (face-remap-add-relative ibuffer-filter-group-name-face 
                             :box '(:style released-button
                                           :line-width 2))))

;; Ibuffer vehemently does not want me to fontify the footer line, but it should
;; really be the same color as the header. So, use this hacky wrapper around the
;; ibuffer function to override the face on the footer.
(defun gcs-ibuffer ()
  (interactive)
  (ibuffer)
  (run-at-time nil nil
    (lambda ()
      (save-excursion
        (switch-to-buffer (get-buffer "*Ibuffer*"))
        (end-of-buffer)
        (let ((inhibit-read-only t))
          (set-text-properties (line-beginning-position) (line-end-position) `(font-lock-face ,ibuffer-title-face)))))))
    
(define-key ibuffer-mode-map (kbd "C-g") 'quit-window)
(define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
(define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line)
(define-key ibuffer-mode-map (kbd "C-j") 'ibuffer-forward-filter-group)
(define-key ibuffer-mode-map (kbd "C-k") 'ibuffer-backward-filter-group)

(provide 'ibuffer-customizations)

