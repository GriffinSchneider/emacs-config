(require 'evil)
(require 'surround)

;; Start In evil-mode with surround.vim emulation
(evil-mode 1)
(global-surround-mode 1)

;; Make cursor red in Emacs mode.
(setq evil-emacs-state-cursor '("red" box))

(defun gcs-escape-if-next-char (c)
  (self-insert-command 1)
  (let ((next-key (read-event)))
    (if (= c next-key)
	(progn
	  (delete-backward-char 1)
	  (evil-esc))
        (setq unread-command-events (list next-key)))))
(defun gcs-escape-if-next-char-is-j (arg)
  (interactive "p")
  (if (= arg 1)
      (gcs-escape-if-next-char ?j)
      (self-insert-command arg)))

;; make "kj" exit insert mode.
(define-key evil-insert-state-map (kbd "k") 'gcs-escape-if-next-char-is-j)

;; Use space and backspace to move up/down 10 lines. 
(define-key evil-normal-state-map " " (lambda () (interactive) (next-line 10)))
(define-key evil-visual-state-map " " (lambda () (interactive) (next-line 10)))
(define-key evil-normal-state-map (kbd "DEL") (lambda () (interactive) (previous-line 10)))

(define-key evil-normal-state-map "\\f" 'find-file)
(define-key evil-normal-state-map "\\w" 'save-buffer)
(define-key evil-normal-state-map "\\B" 'buffer-menu)
(define-key evil-normal-state-map "\\b" 'switch-to-buffer)

(define-key evil-normal-state-map "\\c" 'compile)
(define-key evil-normal-state-map "\\e" 'next-error)
(define-key evil-normal-state-map "\\E" 'previous-error)

(define-key evil-normal-state-map "\\j" 'ace-jump-mode)

(define-key evil-normal-state-map "\\x" 'execute-extended-command)

(define-key evil-normal-state-map "\\0" 'delete-window)
(define-key evil-normal-state-map "\\1" 'delete-other-windows)
(define-key evil-normal-state-map "\\2" 'split-window-vertically)
(define-key evil-normal-state-map "\\3" 'split-window-horizontally)
