(require 'init)

(setq Buffer-menu-size-width 7)
(setq Buffer-menu-name-width 33)

(setq buffer-menu-buffer-font-lock-keywords
      '(("^....[*]Man .*Man.*"   . font-lock-variable-name-face) ; Man page
        (".*Dired.*"             . font-lock-comment-face)       ; Dired
        ("^....[*]shell.*"       . font-lock-preprocessor-face)  ; shell buff
        (".*[*]scratch[*].*"     . font-lock-function-name-face) ; scratch buffer
        ("^....[*].*"            . font-lock-string-face)        ; "*" named buffers
        ("^..[*].*"              . font-lock-constant-face)      ; Modified
        ("^.[%].*"               . font-lock-keyword-face)))     ; Read only

(defun gcs-buffer-menu-custom-font-lock  ()
  (let ((font-lock-unfontify-region-function
	 (lambda (start end)
	   (remove-text-properties start end '(font-lock-face nil)))))
    (font-lock-unfontify-buffer)
    (set (make-local-variable 'font-lock-defaults)
	 '(buffer-menu-buffer-font-lock-keywords t))
    (font-lock-fontify-buffer)))

(defadvice buffer-menu (after buffer-menu-font-lock activate)
  (gcs-buffer-menu-custom-font-lock))

(define-key Buffer-menu-mode-map (kbd "C-g") 'quit-window)
(define-key Buffer-menu-mode-map (kbd "g")
  (lambda ()
    (interactive)
    (revert-buffer)
    (gcs-buffer-menu-custom-font-lock)))

(provide 'buffer-menu-customizations)
