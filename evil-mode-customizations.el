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
(define-key evil-normal-state-map (kbd "DEL") (lambda () (interactive) (previous-line 10)))

;; Use <leader>f for find-file
(define-key evil-normal-state-map "\\f" 'find-file)

;; Use <leader>w for save-buffer
(define-key evil-normal-state-map "\\w" 'save-buffer)

;; Use <leader>x instead of M-x
(define-key evil-normal-state-map "\\x" 'execute-extended-command)

