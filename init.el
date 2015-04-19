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
;;   auto-complete-clang-async needs make
;;   tern needs needs npm install

(provide 'init)


;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;
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

;; exec-path-from-shell needs to happen before other requires, so the path will be setup properly
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(add-to-list 'exec-path (concat gcs-config-directory "thirdparty/emms/src/"))
(add-to-list 'exec-path (concat gcs-config-directory "thirdparty/tern/bin/"))

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

(gcs-package zenburn-theme :init (load-theme 'zenburn t))


;;;;;;;;;;;;;;;;;;
;; Non-package thirdparty requires
;;;;;;;;;;;;;;;;;;
(require 'moonscript-mode)
(require 'android-mode)

(require 'uniquify)
(require 'lua-mode)
(require 'highlight-parentheses)
(require 'eclim)
(require 'pianobar)
(require 'framemove)
(require 'ag)
(require 'tabbar)
(require 'xcode-document-viewer)
(require 'helm)
(require 'helm-config)
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
(require 'key-chord)
(require 'hl-defined)
(require 'idle-highlight-mode)




;;;;;;;;;;;;;;;;;;
;; Non-package forked requires
;;;;;;;;;;;;;;;;;;


(require 'adaptive-wrap-prefix)

(gcs-package dash)
(require 'color-identifiers-mode)


;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;
(gcs-package s)
(gcs-package projectile)
(gcs-package ace-jump-mode)
(gcs-package typing)
(gcs-package auto-complete-c-headers)
(gcs-package multi-term)
(gcs-package expand-region)

(gcs-package glsl-mode
  :commands glsl-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))))

(gcs-package js2-mode
  :config
  (progn
    (tern-ac-setup)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (defun gcs-js2-mode-hook ()
      (tern-mode t)
      (color-identifiers-mode)
      (setq ac-sources '(ac-source-tern-completion))
      (evil-define-key 'insert js2-mode-map (kbd "C-<SPC>") 'tern-ac-complete))
    (add-hook 'js2-mode-hook  'gcs-js2-mode-hook)))

(gcs-package rainbow-delimiters
  :config
  (progn
    (global-rainbow-delimiters-mode t)))

(gcs-package autopair
  :config
  (autopair-global-mode))

(gcs-package web-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))))

(gcs-package pretty-mode
  :config
  (progn
    (pretty-add-keywords 'js2-mode '(("\\_<var\\_>" . "∃")))
    (pretty-activate-groups '(:greek))
    (pretty-deactivate-groups (list :equality) (list 'javascript-mode))
    (global-pretty-mode 1)))

(gcs-package dired+
  :config
  (progn
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-hide-details-propagate-flag nil)))

(gcs-package powerline
  :config
  (progn
    (require 'powerline-customizations)))

(gcs-package yascroll
  :config
  (progn
    ;; Yascroll
    (global-yascroll-bar-mode 1)
    (setq gcs-yascroll-color "#598559")
    (setq yascroll:delay-to-hide nil)
    (zenburn-with-color-variables)
    (set-face-background 'yascroll:thumb-fringe gcs-yascroll-color)
    (set-face-foreground 'yascroll:thumb-fringe gcs-yascroll-color)
    (set-face-background 'yascroll:thumb-text-area gcs-yascroll-color)
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

(gcs-package multi-term
  :config
  (progn
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
    (add-hook 'term-mode-hook 'gcs-term-mode-hook)))

(gcs-package magit :config (require 'magit-mode-customizations))

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

;; color-identifiers-mode
(setq color-identifiers:num-colors 10)
(setq color-identifiers:color-luminance 0.8)
(setq color-identifiers:min-color-saturation 0.6)
(setq color-identifiers:max-color-saturation 1.0)

;; idle-highlight-mode
(setq idle-highlight-idle-time 0.1)
(defun idle-highlight-hook () (idle-highlight-mode t))
(mapc (lambda (m) (add-hook m 'idle-highlight-hook))
      '(emacs-lisp-mode-hook js2-mode-hook ruby-mode-hook objc-mode-hook))

;; hl-defined
(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode)
;; For some reason these need to be in a hook or else they don't apply to
;; new frames.
(defun gcs-fix-hdefd-faces (x)
  (set-face-attribute 'hdefd-functions nil :foreground nil)
  (set-face-attribute 'hdefd-functions nil :inherit 'font-lock-constant-face)
  (set-face-attribute 'hdefd-variables nil :foreground nil)
  (set-face-attribute 'hdefd-variables nil :inherit 'font-lock-variable-name-face))
(add-hook 'after-make-frame-functions 'gcs-fix-hdefd-faces)

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

;; Lua-mode
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 4)

;; Eclim
(setq eclim-auto-save t)
(global-eclim-mode)
(setq eclim-interactive-completion-function 'ido-completing-read)

;; java-mode
(defun gcs-java-mode-hook ()
  (setq c-basic-offset 2
        tab-width 2
        indent-tabs-mode t))
(add-hook 'java-mode-hook 'gcs-java-mode-hook)

;; Org-mode
(setq org-hide-leading-stars t
      org-odd-levels-only t
      ;; Align tags to column 90
      org-tags-column -90)

;; Highlight-parenthesis
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

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

;; Save the session
(setq desktop-load-locked-desktop t)
(setq desktop-dirname "~/.emacs.d/")
(desktop-save-mode 1)
;; Save desktop when opening files, to avoid losing state if Emacs crashes.
(defun gcs-desktop-find-file-hook-helper ()
  (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
  (desktop-save user-emacs-directory))
(defun gcs-desktop-find-file-hook ()
  (run-with-timer 2 nil 'gcs-desktop-find-file-hook-helper))
(add-hook 'find-file-hook 'gcs-desktop-find-file-hook)

(setq ruby-indent-level 2)
