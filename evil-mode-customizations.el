(require 'evil)
(require 'surround)

;; Start In evil-mode with surround.vim emulation
(evil-mode 1)
(global-surround-mode 1)

;; Make cursor red in Emacs mode.
(setq evil-emacs-state-cursor '("red" box))

;; Use space and backspace to move up/down 10 lines. 
(define-key evil-normal-state-map " " (lambda () (interactive) (next-line 10)))
(define-key evil-visual-state-map " " (lambda () (interactive) (next-line 10)))
(define-key evil-normal-state-map (kbd "DEL") (lambda () (interactive) (previous-line 10)))

;; Setup prefix keybindings
(defconst gcs-prefix-key "\\")
(defun gcs-define-key-with-prefix (key binding)
  (define-key evil-normal-state-map (concat gcs-prefix-key key) binding)
  (define-key evil-motion-state-map (concat gcs-prefix-key key) binding))

(gcs-define-key-with-prefix "f" 'find-file)
(gcs-define-key-with-prefix "w" 'save-buffer)
(gcs-define-key-with-prefix "b" 'buffer-menu)
(gcs-define-key-with-prefix "B" 'switch-to-buffer)

(gcs-define-key-with-prefix "c" 'compile)
(gcs-define-key-with-prefix "e" 'next-error)
(gcs-define-key-with-prefix "E" 'previous-error)

(gcs-define-key-with-prefix "j" 'ace-jump-mode)

(gcs-define-key-with-prefix "x" 'execute-extended-command)

(gcs-define-key-with-prefix "0" 'delete-window)
(gcs-define-key-with-prefix "1" 'delete-other-windows)
(gcs-define-key-with-prefix "2" 'split-window-vertically)
(gcs-define-key-with-prefix "3" 'split-window-horizontally)


(provide 'evil-mode-customizations)
