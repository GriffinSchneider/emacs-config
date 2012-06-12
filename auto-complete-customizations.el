(require 'auto-complete-config)
(require 'ac-emacs-eclim-source)

(ac-config-default)
(setq ac-quick-help-delay 0.1)
(ac-flyspell-workaround)
(add-to-list 'ac-dictionary-directories (concat gcs-config-directory "auto-complete/dict"))

(global-set-key (kbd "C-SPC") 'auto-complete)

;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

;; add the emacs-eclim source
(add-hook 'eclim-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-emacs-eclim)))

(provide 'auto-complete-customizations)
