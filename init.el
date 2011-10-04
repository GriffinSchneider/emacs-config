;; Add everything in ~/emacs to the load-path
(let* ((default-directory "~/emacs-config")
       (orig-load-path load-path))
  (setq load-path (cons default-directory nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))
(add-to-list 'load-path "~/emacs-config/maxframe.el")


(require 'color-theme-zenburn nil 'noerror)
(color-theme-zenburn)


(require 'maxframe nil 'noerror)
;; Maximize window on startup
(add-hook 'window-setup-hook 'maximize-frame t)


(require 'sml-modeline nil 'noerror)
;; Scroll indicator in modeline
(sml-modeline-mode t)


(require 'sr-speedbar nil 'noerror)
(setq speedbar-show-unknown-files t
      sr-speedbar-width-x 30
      sr-speedbar-right-side nil)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)


(require 'ace-jump-mode)


(load "evil-mode-customizations")


(load "org-mode-customizations")


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

;; Turn on winner-mode
(winner-mode t)

;; Setup backups
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)

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

;; Use s-[h, j, k, l] for window navigation
(global-set-key (kbd "s-h")  'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-k")    'windmove-up)
(global-set-key (kbd "s-j")  'windmove-down)

;; Make C-M-g the same as C-g - in case 'Esc' is pressed accidentally
(global-set-key "\C-\M-g" 'keyboard-quit)
