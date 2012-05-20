(require 'evil)
(require 'surround)
(require 'magit)

;; Start In evil-mode with surround.vim emulation
(evil-mode 1)
(global-surround-mode 1)

;; Make cursor red in Emacs mode.
(setq evil-emacs-state-cursor '("red" box)
      evil-cross-lines t)

;; Use space and backspace to move up/down 10 lines. 
(define-key evil-normal-state-map " " (lambda () (interactive) (next-line 10)))
(define-key evil-visual-state-map " " (lambda () (interactive) (next-line 10)))
(define-key evil-motion-state-map " " (lambda () (interactive) (next-line 10)))

(define-key evil-normal-state-map (kbd "DEL") (lambda () (interactive) (previous-line 10)))
(define-key evil-visual-state-map (kbd "DEL") (lambda () (interactive) (previous-line 10)))
(define-key evil-motion-state-map (kbd "DEL") (lambda () (interactive) (previous-line 10)))

;; Setup prefix keybindings
(defconst gcs-prefix-key "\\")
(define-key evil-normal-state-map gcs-prefix-key nil)
(define-key evil-motion-state-map gcs-prefix-key nil)
(defun gcs-define-key-with-prefix (key binding)
  (define-key evil-normal-state-map (concat gcs-prefix-key key) binding)
  (define-key evil-motion-state-map (concat gcs-prefix-key key) binding)
  (define-key evil-emacs-state-map (concat gcs-prefix-key key) binding))

(gcs-define-key-with-prefix "f" 'find-file)
(gcs-define-key-with-prefix "F" 'find-alternate-file)
(gcs-define-key-with-prefix "w" 'save-buffer)
(gcs-define-key-with-prefix "b" 'buffer-menu)
(gcs-define-key-with-prefix "B" 'iswitchb-buffer)

(gcs-define-key-with-prefix "c" 'compile)
(gcs-define-key-with-prefix "e" 'next-error)
(gcs-define-key-with-prefix "E" 'previous-error)

(gcs-define-key-with-prefix "j" 'ace-jump-mode)

(gcs-define-key-with-prefix "x" 'smex)
(gcs-define-key-with-prefix "X" 'smex-major-mode-commands)

(gcs-define-key-with-prefix "0" 'delete-window)
(gcs-define-key-with-prefix "1" 'delete-other-windows)
(gcs-define-key-with-prefix "2" 'split-window-vertically)
(gcs-define-key-with-prefix "3" 'split-window-horizontally)

;; Use j and k pressed within .05 seconds to exit insert mode
(defun gcs-evil-maybe-exit (entry-key exit-key)
  (let ((modified (buffer-modified-p)))
    (insert entry-key)
    (let ((evt (read-event nil nil 0.05)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt exit-key))
	(delete-char -1)
	(set-buffer-modified-p modified)
	(push 'escape unread-command-events))
       (t (push evt unread-command-events))))))

(evil-define-command gcs-evil-maybe-exit-j ()
  :repeat change
  (interactive)
  (gcs-evil-maybe-exit ?j ?k))
(define-key evil-insert-state-map "j" 'gcs-evil-maybe-exit-j)

(evil-define-command gcs-evil-maybe-exit-k ()
  :repeat change
  (interactive)
  (gcs-evil-maybe-exit ?k ?j))
(define-key evil-insert-state-map "k" 'gcs-evil-maybe-exit-k)

(provide 'evil-mode-customizations)
