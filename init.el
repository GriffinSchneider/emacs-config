;; Add everything in ~/emacs to the load-path
(let* ((default-directory "~/emacs-config")
       (orig-load-path load-path))
  (setq load-path (cons default-directory nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))
(add-to-list 'load-path "~/emacs-config/maxframe.el")


(when (and (require 'evil nil 'noerror)
	   (require 'surround nil 'noerror))
  
  ;; Start In evil-mode with surround.vim emulation
  (evil-mode 1)
  (global-surround-mode 1)
  
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


(when (require 'color-theme-zenburn nil 'noerror)
  (color-theme-zenburn))


(when (require 'maxframe nil 'noerror)
  ;; Maximize window on startup
  (add-hook 'window-setup-hook 'maximize-frame t))


(when (require 'sml-modeline nil 'noerror)
  ;; Scroll indicator in modeline
  (sml-modeline-mode t))


(when (require 'sr-speedbar nil 'noerror)
  (setq speedbar-show-unknown-files t
        sr-speedbar-width-x 30
	sr-speedbar-right-side nil)
  (global-set-key (kbd "s-s") 'sr-speedbar-toggle))


;; Setup haskell-mode
;;   NOTE: If the ghci prompt is changed in your .ghci file,
;;   inferior-haskell-mode's regex to match the prompt may 
;;   not work. If this is the case, do something like:
;;
;;     (add-hook
;;       'inferior-haskell-mode-hook
;;       (lambda () (set (make-local-variable 'comint-prompt-regexp) "REGEX")))
;;
;;  where REGEX matches the ghci prompt. Otherwise, emacs will hang on 
;;  inferior-haskell-load-file.
(load "~/emacs-config/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Turn off startup message
(setq inhibit-startup-message t)

;; On OSX, use cmd-r to compile
(global-set-key (kbd "s-r") 'compile)

;; use buffer-menu instead of list-buffers
(global-set-key "\C-x\C-b" 'buffer-menu)

;; Make scrolling not suck.
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-up-aggressively 0
      scroll-down-aggressively 0)

;; No scrollbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Highlight the current line
(global-hl-line-mode 1)

;; Highlight matching parens
(show-paren-mode 1)

;; Use C-c [h, j, k, l] for window navigation
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

;; Make C-M-g the same as C-g - in case 'Esc' is pressed accidentally
(global-set-key "\C-\M-g" 'keyboard-quit)
