(require 'auto-complete-config)
(require 'ac-emacs-eclim-source)

(ac-config-default)
(setq ac-quick-help-delay 1.5
      ac-use-menu-map t)
(ac-flyspell-workaround)
(add-to-list 'ac-dictionary-directories (concat gcs-config-directory "auto-complete/dict"))

(add-to-list 'ac-sources 'ac-source-words-in-all-buffer 'append)

(global-set-key (kbd "C-SPC") 'auto-complete)
(define-key ac-menu-map (kbd "s-j") 'ac-next)
(define-key ac-menu-map (kbd "s-k") 'ac-previous)

;; dirty fix for having AC everywhere
(defun auto-complete-mode-maybe ()
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))
(global-auto-complete-mode t)

;; add the emacs-eclim source
(add-hook 'eclim-mode-hook (lambda () (setq ac-sources '(ac-source-emacs-eclim ac-source-eclim))))

(provide 'auto-complete-customizations)

