(when (and (require 'evil nil 'noerror)
	   (require 'surround nil 'noerror))
  
  ;; Start In evil-mode with surround.vim emulation
  (evil-mode 1)
  (global-surround-mode 1)

  ;; Make cursor red in Emacs mode.
  (setq evil-emacs-state-cursor '("red" box))
  
  ;; make "kj" exit insert mode.
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
  
  (define-key evil-insert-state-map (kbd "k") 'gcs-escape-if-next-char-is-j))