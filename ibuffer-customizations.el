(require 'init)

(defconst gcs-ibuffer-fontification-alist
  '((ruby-mode . font-lock-string-face)
    (sh-mode . font-lock-string-face)
    (objc-mode . font-lock-constant-face)
    (c-mode . font-lock-constant-face)
    (java-mode . font-lock-constant-face)
    (emacs-lisp-mode . font-lock-variable-name-face)
    (org-mode . font-lock-negation-char-face)
    (dired-mode . font-lock-function-name-face)
    (term-mode . font-lock-doc-string-face)))

(setq ibuffer-formats
      `((mark
         modified
         read-only
         vc-status-mini
         " "
         (name 30 30 :left :elide)
         ,(propertize "| " 'font-lock-face ibuffer-title-face)
         (mode 10 10 :left)
         ,(propertize " | " 'font-lock-face ibuffer-title-face)
         filename)))

(setq ibuffer-fontification-alist
      `(,@(mapcar (lambda (b)
                    `(9999 (eq major-mode ',(car b)) ,(cdr b)))
                  gcs-ibuffer-fontification-alist)
        (90 (string-match "magit" (symbol-name major-mode))
            font-lock-function-name-face)
        (90 (or (string-match "^*" (buffer-name))
               (memq major-mode ibuffer-help-buffer-modes))
            font-lock-comment-face)))

(define-key ibuffer-mode-map (kbd "C-g") 'quit-window)
(define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
(define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line)
(define-key ibuffer-mode-map (kbd "C-j") 'ibuffer-forward-filter-group)
(define-key ibuffer-mode-map (kbd "C-k") 'ibuffer-backward-filter-group)

(gcs-package ibuffer-vc
  :config
  (progn
    (defun gcs-ibuffer-hook ()
      (face-remap-add-relative 'default 'font-lock-comment-face)
      (copy-face 'font-lock-keyword-face 'tempface )
      (setq ibuffer-filter-group-name-face 'tempface)
      (face-remap-add-relative ibuffer-filter-group-name-face 
                               :box '(:style released-button
                                             :line-width 2))
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))
    
    (add-hook 'ibuffer-hook 'gcs-ibuffer-hook)))

(provide 'ibuffer-customizations)

