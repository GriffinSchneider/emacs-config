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

;; Use these motions instead of ace-jump's commands
;; to get ace-jump to work in evil visual mode
(defmacro gcs-without-evil-visual-hooks (&rest body)
  `(let ((old-mark (mark)))
     (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
     (remove-hook 'post-command-hook #'evil-visual-post-command t)
     (unwind-protect
         (progn
           ,@body
           (recursive-edit))
        (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
        (add-hook 'post-command-hook #'evil-visual-post-command nil t)
        (set-mark old-mark))))

(evil-define-motion evil-ace-jump-char-mode (count)
  :type exclusive
  (gcs-without-evil-visual-hooks
   (ace-jump-mode 5)))

(evil-define-motion evil-ace-jump-line-mode (count)
  :type line
  (gcs-without-evil-visual-hooks
   (ace-jump-mode 9)))

(evil-define-motion evil-ace-jump-word-mode (count)
  :type exclusive
  (gcs-without-evil-visual-hooks
   (ace-jump-mode 1)))

(evil-define-motion evil-ace-jump-char-direct-mode (count)
  :type inclusive
  (gcs-without-evil-visual-hooks
   (ace-jump-mode 5)
   (forward-char 1)))

(add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)


(provide 'evil-mode-customizations)
