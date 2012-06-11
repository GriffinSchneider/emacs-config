(require 'auto-complete-config)
(require 'ac-emacs-eclim-source)

(ac-config-default)
(setq ac-quick-help-delay 0.1)
(ac-flyspell-workaround)
(add-to-list 'ac-dictionary-directories (concat gcs-config-directory "auto-complete/dict"))

(global-set-key (kbd "C-SPC") 'auto-complete)

;; add the emacs-eclim source
(add-hook 'eclim-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-emacs-eclim)))

(provide 'auto-complete-customizations)

