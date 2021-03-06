(require 'init)

(gcs-package evil
  :config
  (progn
    (evil-mode 1)
    ;; Use tab to move between links in help mode.
    (evil-define-key 'motion help-mode-map (read-kbd-macro "TAB") 'forward-button)

    ;; Swap "[]" with "{}"
    (evil-global-set-key 'motion "]" 'evil-forward-paragraph)
    (evil-global-set-key 'motion "[" 'evil-backward-paragraph)
    (evil-global-set-key 'motion "}" 'evil-forward-sentence)
    (evil-global-set-key 'motion "{" 'evil-backward-sentence)

    ;; Make cursor red in Emacs mode.
    (setq evil-emacs-state-cursor '("red" box)
          evil-normal-state-cursor '("white")
          evil-cross-lines t)
    (setq-default evil-symbol-word-search t)

    (evil-global-set-key 'insert (kbd "<RET>") 'evil-ret-and-indent)

    (mapc (lambda (mode) (evil-set-initial-state mode 'emacs))
          '(inferior-emacs-lisp-mode
            comint-mode
            shell-mode
            git-rebase-mode
            term-mode
            magit-branch-manager-mode
            eww-mode))
    (mapc (lambda (mode) (evil-set-initial-state mode 'normal))
          '(git-commit-mode))
    ))

(gcs-package evil-visualstar)

(provide 'evil-mode-customizations)
