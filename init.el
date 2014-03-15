;;; Setup:
;; The following variables should be set by a local config file:
;;   eclim-executable 
;;   android-mode-sdk-dir
;;   pianobar-password
;; External programs to install somewhere on exec-path (all installable throgh homebrew on OSX):
;;   mplayer
;;   pianobar
;;   mpg321
;;   mplayer
;;   ag
;; Things that need building:
;;   auto-complete-clang-async

(provide 'init)

(when load-in-progress
  (setq gcs-config-directory (file-name-directory load-file-name)
        gcs-thirdparty-directory (concat gcs-config-directory "thirdparty/")))

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

(when load-in-progress (byte-recompile-directory gcs-thirdparty-directory 0))

;; Add all themes to the custom theme path
(add-to-list 'custom-theme-load-path (concat gcs-thirdparty-directory "zenburn-emacs"))
(add-to-list 'custom-theme-load-path (concat gcs-thirdparty-directory "tomorrow-theme/GNU Emacs"))
(load-theme 'zenburn t)

;; exec-path-from-shell needs to happen before other requires, so the path will be setup properly
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(add-to-list 'exec-path (concat gcs-config-directory "thirdparty/emms/src/"))

;; Setup package archibes
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(require 'use-package)
;; I always want to download packages if they aren't installed, so always use :ensure
(defmacro gcs-package (package &rest options)
  `(use-package ,package
     :ensure ,package
     ,@options))
(put 'gcs-package 'lisp-indent-function 'defun)

;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;
(gcs-package projectile)
(gcs-package ace-jump-mode)
(gcs-package typing)
(gcs-package auto-complete-c-headers)

;; TODO: powerline's new version requires changes.
(require 'powerline)
;; TODO: move this down. colors in here are required elsewhere
(require 'powerline-customizations)
(gcs-package yascroll
  :config
  (progn
    ;; Yascroll
    (global-yascroll-bar-mode 1)
    (setq yascroll:delay-to-hide nil)
    (set-face-background 'yascroll:thumb-fringe powerline-color1)
    (set-face-foreground 'yascroll:thumb-fringe powerline-color1)
    (set-face-background 'yascroll:thumb-text-area powerline-color1)
    ;; Don't hide scrollbar when editing
    (defadvice yascroll:before-change (around always-show-bar activate) ())))

(gcs-package yasnippet
  :config
  (progn 
    (setq yas-snippet-dirs (list (concat gcs-thirdparty-directory "yasnippet/snippets")
                                 (concat gcs-config-directory "snippets")))
    (yas-global-mode 1)))

;; ido and smex
(gcs-package ido)
(gcs-package smex)
(gcs-package flx-ido
  :config
  (progn 
    ;; Ido
    (ido-mode t)
    (setq ido-everywhere t
          ido-ignore-buffers (append (list "\\*Buffer List\\*" "*magit-process*")
                                     ido-ignore-buffers)
          ;; Show ido completions vertically
          ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
                                  " [No match]" " [Matched]" " [Not readable]"
                                  " [Too big]" " [Confirm]")))
    (setq ido-default-file-method 'selected-window)
    
    (flx-ido-mode t)
    (setq gc-cons-threshold 20000000)
    (set-face-foreground 'flx-highlight-face (face-foreground 'isearch))
    (set-face-background 'flx-highlight-face (face-background 'isearch))
    (set-face-attribute 'flx-highlight-face nil :inherit 'font-lock-keyword-face)
    ;; disable ido faces to see flx highlights.
    (setq ido-use-faces nil)

    (remove-hook 'ido-minibuffer-setup-hook 'gcs-ido-minibuffer-setup)
    (defun gcs-ido-minibuffer-setup ()
      ;; Disable line truncation
      (set (make-local-variable 'truncate-lines) nil)
      ;; Delete backward by word with C-w
      (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
      (define-key ido-completion-map (kbd "s-j") 'ido-next-match)
      (define-key ido-completion-map (kbd "s-k") 'ido-prev-match))
    (add-hook 'ido-minibuffer-setup-hook 'gcs-ido-minibuffer-setup)))


;;;;;;;;;;;;;;;;;;
;; Non-package thirdparty requires
;;;;;;;;;;;;;;;;;;
(require 'moonscript-mode)
(require 'android-mode)

(require 'uniquify)
(require 'magit)
(require 'magit-blame)
(require 'lua-mode)
;; (require 'sr-speedbar nil 'noerror)
(require 'highlight-parentheses)
(require 'eclim)
(require 'pianobar)
(require 'framemove)
(require 'ag)
(require 'multi-term)
(require 'tabbar)
(require 'xcode-document-viewer)
(require 'helm)
(require 'helm-config)
(require 'helm-css-scss)
(require 'eproject)
(require 'eproject-extras)
(require 'helm-eproject)
(require 'auto-complete-clang-async)
(require 'ediff)
(require 'emms-setup)
(require 'emms-browser)
(require 'git-gutter)
(require 'tern)
(require 'tern-auto-complete) 
(require 'js2-mode)
(require 'key-chord)
(require 'color-identifiers-mode)
(require 'hl-defined)
(require 'idle-highlight-mode)

;;;;;;;;;;;;;;;;;;
;; Non-package forked requires
;;;;;;;;;;;;;;;;;;
(require 'adaptive-wrap-prefix)

;;;;;;;;;;;;;;;;;;
;; My requires
;;;;;;;;;;;;;;;;;;
(require 'evil-mode-customizations)
(require 'keybindings)
(require 'ibuffer-customizations)
(require 'auto-complete-customizations)
(require 'pianobar-customizations)
(require 'tabbar-customizations)
(require 'eproject-customizations)
(require 'objective-c-customizations)
(require 'eww-customizations)

;; idle-highlight-mode
(setq idle-highlight-idle-time 0.1)
(defun idle-highlight-hook () (idle-highlight-mode t))
(mapc (lambda (m) (add-hook m 'idle-highlight-hook))
      '(emacs-lisp-mode-hook js2-mode-hook ruby-mode-hook objc-mode-hook))

;; hl-defined
(set-face-attribute 'hdefd-functions nil :foreground nil)
(set-face-attribute 'hdefd-functions nil :inherit 'font-lock-constant-face)
(set-face-attribute 'hdefd-variables nil :foreground nil)
(set-face-attribute 'hdefd-variables nil :inherit 'font-lock-variable-name-face)
(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode)

;; js2-mode and tern
(tern-ac-setup)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(defun gcs-js2-get-type ()
    (when (eq major-mode 'js2-mode)
      (tern-get-type)))
(run-with-idle-timer 0.5 t 'gcs-js2-get-type)
(add-hook 'js2-mode-hook 
          (lambda () 
            (tern-mode t)
            (setq ac-sources '(ac-source-tern-completion))
            (evil-define-key 'insert js2-mode-map (kbd "C-<SPC>") 'tern-ac-complete)))

;; markdowm-mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

;; git-gutter
(global-git-gutter-mode t)

;; emms
(emms-all)
(emms-default-players)
(setq emms-completing-read-function 'ido-completing-read)
(defalias 'emms-read-directory-name 'ido-read-directory-name)
(setq emms-info-functions '(emms-info-mp3info))
(setq later-do-interval 0.001
      emms-info-asynchronously t)

;; processing-mode
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(setq processing-location "/Applications/Processing.app/Contents/Resources/Java")

;; eproject
(setq eproject-completing-read-function 'eproject--ido-completing-read)

;; auto-complete-clang-async
(setq ac-clang-complete-executable (concat gcs-thirdparty-directory "emacs-clang-complete-async/clang-complete"))

;; Helm
(define-key helm-map (kbd "s-j") 'helm-next-line)
(define-key helm-map (kbd "s-k") 'helm-previous-line)
;; Stop Helm file sources from showing only the basenames of the files.
(setq helm-ff-transformer-show-only-basename nil)

;; Multi-term
(setq term-buffer-maximum-size 10000)
(defun gcs-term-mode-hook ()
  (yas-minor-mode  -1)
  (auto-complete-mode -1)
  (setq ac-sources '(ac-source-filename
                     ac-source-words-in-buffer
                     ac-source-words-in-same-mode-buffers
                     ac-source-words-in-all-buffer)))
(setq multi-term-program "/bin/zsh")
(define-key term-raw-map (kbd "s-v") 'term-paste)
(add-hook 'term-mode-hook 'gcs-term-mode-hook)

;; Ag
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)

;; Framemove
(setq framemove-hook-into-windmove t)

;; XCode-like line wrapping
(global-adaptive-wrap-prefix-mode t)


;; Uniquify-buffer
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; Magit
(set-face-attribute 'magit-item-highlight nil :inherit nil :background nil)
;; "q" always kills magit buffers
(define-key magit-mode-map "q" (lambda () (interactive) (magit-mode-quit-window 'kill-buffer)))
(define-key magit-mode-map ";" 'magit-toggle-section)
;; Use j and k for navigation in magit-mode.
;; Remap "k" to be magit-goto-previous-section everywhere
(define-key magit-status-mode-map "k" 'magit-goto-previous-section)
(define-key magit-branch-manager-mode-map "k" 'magit-goto-previous-section)
(define-key magit-mode-map "k" 'magit-goto-previous-section)
;; Remap "K" to do what "k" used to do, wherever "k" used to be defined
(define-key magit-status-mode-map "K" 'magit-discard-item)
(define-key magit-branch-manager-mode-map "K" 'magit-discard-item)
;; Map "j" to magit-goto-next-section in eveywhere
(defun gcs-magit-j ()
  (interactive)
  (let ((next (magit-find-section-after (point))))
    (if next
        (magit-goto-section next)
      (goto-char (+ -1 (magit-section-end (magit-current-section)))))))
(define-key magit-status-mode-map "j" 'gcs-magit-j)
(define-key magit-mode-map "j" 'gcs-magit-j)
;; git-rebase-mode
(define-key git-rebase-mode-map "j" 'forward-line)
(define-key git-rebase-mode-map "k" 'git-rebase-backward-line)
(define-key git-rebase-mode-map "p" 'git-rebase-pick)
(define-key git-rebase-mode-map "K" 'git-rebase-kill-line)
(key-chord-define magit-mode-map "k;" (lambda () (interactive) (magit-goto-previous-section) (magit-toggle-section)))
(key-chord-define magit-mode-map "j;" (lambda () (interactive) (magit-goto-next-section) (magit-toggle-section)))
(key-chord-define magit-mode-map "l;" 'magit-git-command)

;; Lua-mode
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 4)


;; Eclim
(setq eclim-auto-save t)
(global-eclim-mode)
(setq eclim-interactive-completion-function 'ido-completing-read)

;; Org-mode
(setq org-hide-leading-stars t
      org-odd-levels-only t
      ;; Align tags to column 90
      org-tags-column -90)

;; Sr-speedbar
(setq speedbar-show-unknown-files t
      sr-speedbar-width-x 30
      sr-speedbar-right-side nil)

;; Highlight-parenthesis
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; Haskell-mode
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

;; Ediff
(set-face-attribute 'ediff-current-diff-A nil :background "#553333" :foreground nil)
(set-face-attribute 'ediff-current-diff-B nil :background "#335533" :foreground nil)
(set-face-attribute 'ediff-current-diff-C nil :background "#888800" :foreground nil)
(set-face-attribute 'ediff-fine-diff-A    nil :background "#331111" :foreground nil)
(set-face-attribute 'ediff-fine-diff-B    nil :background "#113311" :foreground nil)
(set-face-attribute 'ediff-fine-diff-C    nil :background "#666600" :foreground nil)
(set-face-attribute 'ediff-even-diff-A    nil :background "Grey15"  :foreground nil)
(set-face-attribute 'ediff-even-diff-B    nil :background "Grey15"  :foreground nil)
(set-face-attribute 'ediff-even-diff-C    nil :background "Grey15"  :foreground nil)
(set-face-attribute 'ediff-odd-diff-A     nil :background "Grey10"  :foreground nil)
(set-face-attribute 'ediff-odd-diff-B     nil :background "Grey10"  :foreground nil)
(set-face-attribute 'ediff-odd-diff-C     nil :background "Grey10"  :foreground nil)

;; Use css-mode for SASS
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; Setup starting frame size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 155))

;; Turn on winner-mode
(winner-mode t)

;; Use electric-indent-mode
(electric-indent-mode)

;; Don't insert tabs
(setq-default indent-tabs-mode nil)

;; Auto revert
(global-auto-revert-mode t)
              
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows t)

(setq
 ;; Turn off startup message
 inhibit-startup-message t

 ;; Use 4-wide tabs
 tab-width 4

 ;; Always show line and column numbers in mode-line
 line-number-mode t
 column-number-mode t

 ;; Setup backups
 backup-directory-alist `(("." . "~/.emacs.d/saves"))
 backup-by-copying t

 ;; Make scrolling not suck.
 scroll-margin 0
 scroll-conservatively 100000
 scroll-up-aggressively 0.0
 scroll-down-aggressively 0.0)

;; No scrollbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Only one half-width fringe on the right
(fringe-mode (cons 0 4))

;; Make window divider line the same color as the fringe
(set-face-foreground 'vertical-border (face-background 'fringe))

;; Highlight the current line
;; (global-hl-line-mode 1)
;; (set-face-attribute 'hl-line nil :inherit nil)

;; Use visual bell
(setq visible-bell t)

;; Save the session
(setq desktop-load-locked-desktop t)
(desktop-save-mode 1)
(desktop-read)

;; Save minibuffer history
(savehist-mode t)

;; Setup emacsclient
(server-start)
(setenv "EDITOR" "emacsclient")
;; Don't prompt when killing buffers with clients.
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Highlight matching parens
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
;; Make show-paren-mode use a different color than highlight-parentheses
(set-face-foreground 'show-paren-match "Orange")
(set-face-background 'show-paren-match nil)
(set-face-bold-p 'show-paren-match t)

;; Use "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Prevent annoying "Active processes exist" query when Emacs is quit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-flet ((process-list ())) ad-do-it))

(setq ruby-indent-level 4)
