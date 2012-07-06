;; The following variables should be set by a local config file:
;;   eclim-executable 
;;   android-mode-sdk-dir

(when load-in-progress
  (setq gcs-config-directory (file-name-directory load-file-name))
  (setq gcs-thirdparty-directory (concat gcs-config-directory "thirdparty/")))

;; Turn off toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Add everything in this directory to the load-path
(let* ((default-directory gcs-config-directory)
       (orig-load-path load-path))
  (setq load-path (cons default-directory nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))
(add-to-list 'load-path (concat gcs-config-directory "maxframe.el"))

(when load-in-progress (byte-recompile-directory gcs-config-directory))

(add-to-list 'custom-theme-load-path (concat gcs-thirdparty-directory "zenburn-emacs"))
(load-theme 'zenburn 'no-confirm)


(require     'evil-mode-customizations)
(require      'org-mode-customizations)
(require   'buffer-menu-customizations)
(require         'eclim-customizations)
(require 'auto-complete-customizations)
(require     'powerline-customizations)

(require 'moonscript-mode)
(require 'android-mode)
(require 'ace-jump-mode)
(require 'typing)


(require 'sunrise-commander)
(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))
(setq find-directory-functions (cons 'sr-dired find-directory-functions))


;; XCode-like line wrapping
(require 'adaptive-wrap-prefix)
(global-adaptive-wrap-prefix-mode t)
(global-visual-line-mode)


(require 'yascroll)
(global-yascroll-bar-mode 1)
(setq yascroll:delay-to-hide nil)
(set-face-background 'yascroll:thumb-fringe powerline-color1)
(set-face-foreground 'yascroll:thumb-fringe powerline-color1)
(set-face-background 'yascroll:thumb-text-area powerline-color1)
;; Don't hide scrollbar when editing
(defadvice yascroll:before-change (around always-show-bar activate) ())


(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")


(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-ignore-buffers (cons "\\*Buffer List\\*" ido-ignore-buffers))
;; Show ido completions vertically
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
                              " [No match]" " [Matched]" " [Not readable]"
                              " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)


(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


(require 'magit)
(require 'magit-blame)
;; "q" always kills magit buffers
(define-key magit-mode-map "q" (lambda () (interactive) (magit-quit-window 'kill-buffer)))


(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 4)


(require 'yasnippet) ;; not yasnippet-bundle
(setq yas/snippet-dirs (list (concat gcs-config-directory "yasnippet/snippets")))
(yas/initialize)


(require 'sr-speedbar nil 'noerror)
(setq speedbar-show-unknown-files t
      sr-speedbar-width-x 30
      sr-speedbar-right-side nil)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)


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
(load (concat gcs-thirdparty-directory "haskell-mode/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; Setup starting frame size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 155))

;; Turn off startup message
(setq inhibit-startup-message t)

;; Turn on winner-mode
(winner-mode t)

;; Use 4-wide tabs
(setq tab-width 4)

;; Don't insert tabs
(setq-default indent-tabs-mode nil)

;; Use electric-indent-mode
(electric-indent-mode)

;; Always show line and column numbers in mode-line
(setq line-number-mode t)
(setq column-number-mode t)

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

;; Half-width fringes
(fringe-mode 4)

;; Highlight the current line
(global-hl-line-mode 1)

;; Highlight surrounding parens
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; Highlight matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
;; Make show-paren-mode use the same color as highlight-parentheses
(set-face-foreground 'show-paren-match (car hl-paren-colors))
(set-face-background 'show-paren-match nil)
(set-face-bold-p 'show-paren-match nil)

;; Use "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Use C-w for backward-kill-word in the minibuffer
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

;; Use C-s-f to toggle fullscreen
(global-set-key (kbd "C-s-f") 'ns-toggle-fullscreen)

;; Use [C-]s-[y, u, i, o] to resize windows
(global-set-key (kbd "s-y")   (lambda () (interactive) (shrink-window-horizontally 5)))
(global-set-key (kbd "C-s-y") (lambda () (interactive) (shrink-window-horizontally 1)))
(global-set-key (kbd "s-u")   (lambda () (interactive) (shrink-window 5)))
(global-set-key (kbd "C-s-u") (lambda () (interactive) (shrink-window 1)))
(global-set-key (kbd "s-i")   (lambda () (interactive) (enlarge-window 5)))
(global-set-key (kbd "C-s-i") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "s-o")   (lambda () (interactive) (enlarge-window-horizontally 5)))
(global-set-key (kbd "C-s-o") (lambda () (interactive) (enlarge-window-horizontally 1)))

;; Use s-[h, j, k, l] for window navigation
(global-set-key (kbd "s-h")  'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-k")    'windmove-up)
(global-set-key (kbd "s-j")  'windmove-down)

;; Make C-M-g the same as C-g - in case 'Esc' is pressed accidentally
(global-set-key "\C-\M-g" 'keyboard-quit)

;; Prevent annoying "Active processes exist" query when Emacs is quit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (flet ((process-list ())) ad-do-it))
