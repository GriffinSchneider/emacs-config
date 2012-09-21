(require 'init)

;; Start In evil-mode with surround.vim emulation
(evil-mode 1)
(global-surround-mode 1)

;; Use tab to move between links in help mode.
(evil-define-key 'motion help-mode-map (read-kbd-macro "TAB") 'forward-button)

;; Make cursor red in Emacs mode.
(setq evil-emacs-state-cursor '("red" box)
      evil-cross-lines t)

(mapc (lambda (mode) (evil-set-initial-state mode 'emacs))
       '(inferior-emacs-lisp-mode
         comint-mode
         shell-mode
         term-mode
         magit-branch-manager-mode
         pianobar-mode))

(provide 'evil-mode-customizations)
