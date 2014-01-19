(require 'init)

;; Start In evil-mode with surround.vim emulation
(evil-mode 1)
(global-surround-mode 1)

;; Use tab to move between links in help mode.
(evil-define-key 'motion help-mode-map (read-kbd-macro "TAB") 'forward-button)

;; Make cursor red in Emacs mode.
(setq evil-emacs-state-cursor '("red" box)
      evil-cross-lines t)

(evil-global-set-key 'insert (kbd "<RET>") 'evil-ret-and-indent)

(mapc (lambda (mode) (evil-set-initial-state mode 'emacs))
       '(inferior-emacs-lisp-mode
         comint-mode
         shell-mode
         git-rebase-mode
         term-mode
         magit-branch-manager-mode
         eww-mode
         pianobar-mode))

(provide 'evil-mode-customizations)
