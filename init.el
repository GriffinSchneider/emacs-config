;; The following variables should be set by a local config file:
;;   eclim-executable 
;;   android-mode-sdk-dir
;; Optional:
;;    pianobar-password
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

(add-to-list 'custom-theme-load-path (concat gcs-thirdparty-directory "zenburn-emacs"))
(load-theme 'zenburn 'no-confirm)

;; Third-party requires
(require 'evil)
(require 'surround)
(require 'moonscript-mode)
(require 'android-mode)
(require 'ace-jump-mode)
(require 'typing)
(require 'sunrise-commander)
(require 'adaptive-wrap-prefix)
(require 'yascroll)
(require 'uniquify)
(require 'ido)
(require 'smex)
(require 'magit)
(require 'magit-blame)
(require 'lua-mode)
(require 'yasnippet) ;; not yasnippet-bundle
(require 'sr-speedbar nil 'noerror)
(require 'highlight-parentheses)
(require 'eclim)
(require 'powerline)
(require 'pianobar)
(require 'framemove)
(require 'find-file-in-project)
(require 'project)
(require 'ack)
(require 'multi-term)

;; My requires
(require 'keybindings)
(require 'evil-mode-customizations)
(require 'buffer-menu-customizations)
(require 'auto-complete-customizations)
(require 'powerline-customizations)
(require 'pianobar-customizations)

(when (executable-find "w3m")
  (require 'w3m)
  (require 'w3m-customizations))

;; Multi-term
(setq multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook (lambda () (yas-minor-mode  -1)))

;; Ack
(setq ack-command "ack -i --match ")
;; Gets run after ack output is inserted into buffer by comint and
;; processed for color escape codes by ack.el.
;; Replaces all whitespace at the beginning of ack matches with 1 tab for alignment.
(defadvice ack-filter (after align-ack-results activate)
  (goto-char compilation-filter-start)
  (while (re-search-forward "^[0-9]*:\\([ \t]*\\)[^ ]+" nil t)
    (replace-match "\t" nil nil nil 1)))
;; Enable evil's g bindings in ack-mode. 
(define-key ack-mode-map "g" nil)

;; Sunrise commander
(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))
(setq find-directory-functions (cons 'sr-dired find-directory-functions))
(define-key sr-mode-map "j" 'dired-next-line)
(define-key sr-mode-map "k" 'dired-previous-line)
(define-key sr-mode-map "J" 'sr-dired-prev-subdir)

;; Framemove
(setq framemove-hook-into-windmove t)

;; XCode-like line wrapping
(global-adaptive-wrap-prefix-mode t)

;; Yascroll
(global-yascroll-bar-mode 1)
(setq yascroll:delay-to-hide nil)
(set-face-background 'yascroll:thumb-fringe powerline-color1)
(set-face-foreground 'yascroll:thumb-fringe powerline-color1)
(set-face-background 'yascroll:thumb-text-area powerline-color1)
;; Don't hide scrollbar when editing
(defadvice yascroll:before-change (around always-show-bar activate) ())

;; Uniquify-buffer
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; Ido
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-ignore-buffers (cons "\\*Buffer List\\*" ido-ignore-buffers)
      ;; Show ido completions vertically
      ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
                              " [No match]" " [Matched]" " [Not readable]"
                              " [Too big]" " [Confirm]")))
(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            ;; Disable line truncation
            (set (make-local-variable 'truncate-lines) nil)
            ;; Delete backward by word with C-w
            (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
            (define-key ido-completion-map (kbd "s-j") 'ido-next-match)
            (define-key ido-completion-map (kbd "s-k") 'ido-prev-match)))

;; Smex
(smex-initialize)

;; Magit
(set-face-background 'magit-item-highlight nil)
;; "q" always kills magit buffers
(define-key magit-mode-map "q" (lambda () (interactive) (magit-quit-window 'kill-buffer)))
(define-key magit-mode-map ";" 'magit-toggle-section)
;; Use j and k for navigation in magit-mode. For some reason, magit
;; overrides the k binding if I don't use evil-add-hjkl-bindings.
(evil-add-hjkl-bindings magit-mode-map 'emacs
  "K" 'magit-discard-item
  ;; If j is pressed and we're already at the last section, move to end of the section.
  ;; This fixes the problem when you're at the first line of the last section and the
  ;; rest of the last section is off the screen, but you can't press j to see the rest
  ;; of the section.
  "j" (lambda () (interactive)
        (let ((next (magit-find-section-after (point))))
          (cond (next (magit-goto-section next))
                (t    (goto-char (+ -1 (magit-section-end (magit-current-section))))))))
  "k" 'magit-goto-previous-section
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk
  ":" 'magit-git-command)

;; Lua-mode
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 4)

;; Yasnippet
(setq yas-snippet-dirs (list (concat gcs-config-directory "yasnippet/snippets")))
(yas-global-mode 1)

;; Eclim
(setq eclim-auto-save t
      eclim-eclipse-dirs '("~/Dev/eclipse"))

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

;; Setup starting frame size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 155))

;; Turn on winner-mode
(winner-mode t)

;; Use electric-indent-mode
(electric-indent-mode)

;; Don't insert tabs
(setq-default indent-tabs-mode nil)
              
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
 scroll-up-aggressively 0
 scroll-down-aggressively 0)

;; No scrollbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Only one half-width fringe on the right
(fringe-mode (cons 0 4))

;; Make window divider line the same color as the fringe
(set-face-foreground 'vertical-border (face-background 'fringe))

;; Highlight the current line
(global-hl-line-mode 1)

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
  (flet ((process-list ())) ad-do-it))

