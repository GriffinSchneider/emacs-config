(require 'evil)
(require 'surround)
(require 'magit)
(require 'pianobar)

(require 'buffer-menu-customizations)

;; Start In evil-mode with surround.vim emulation
(evil-mode 1)
(global-surround-mode 1)

;; Use C-u to scoll up like vim, move emacs's universal argument to C-S-u
(global-set-key (kbd "C-u") 'evil-scroll-up)
(global-set-key (kbd "C-S-u") 'universal-argument)

;; Make cursor red in Emacs mode.
(setq evil-emacs-state-cursor '("red" box)
      evil-cross-lines t)

;; Use space for ace-jump
(defun gcs-define-evil-motion-key (key def)
  (define-key evil-normal-state-map key def)
  (define-key evil-visual-state-map key def)
  (define-key evil-motion-state-map key def))
(gcs-define-evil-motion-key (kbd "SPC") 'ace-jump-word-mode)
(gcs-define-evil-motion-key (kbd "C-SPC") 'ace-jump-char-mode)
(gcs-define-evil-motion-key (kbd "M-SPC") 'ace-jump-line-mode)

(define-key evil-normal-state-map "'" 'evil-goto-mark)
(define-key evil-normal-state-map "`" 'evil-goto-mark-line)

;; Get rid of the "K" binding for evil-lookup
(define-key evil-motion-state-map "K" nil)

;; For some reason, magit overrides the k binding if I don't use
;;  evil-add-hjkl-bindings.
(evil-add-hjkl-bindings magit-mode-map 'emacs
  "K" 'magit-discard-item
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk
  ":" 'magit-git-command)

;; Setup prefix keybindings
(defconst gcs-prefix-key "\\")
(defconst gcs-prefix-key-maps (list evil-normal-state-map
                                    evil-motion-state-map
                                    evil-emacs-state-map
                                    pianobar-mode-map))

(define-prefix-command 'gcs-prefix-key-map)
(mapc (lambda (keymap) (define-key keymap gcs-prefix-key 'gcs-prefix-key-map)) gcs-prefix-key-maps)
(defun gcs-define-prefix-key (key command)
  (define-key 'gcs-prefix-key-map key command))

(gcs-define-prefix-key "q" 'quit-window)
(gcs-define-prefix-key "g" 'magit-status)
(gcs-define-prefix-key "s" 'sunrise)
(gcs-define-prefix-key "S" 'sunrise-cd)

(gcs-define-prefix-key "u" 'undo-tree-visualize)
(gcs-define-prefix-key "f" 'ido-find-file)
(gcs-define-prefix-key "F" 'ido-find-alternate-file)
(gcs-define-prefix-key "w" 'save-buffer)
(gcs-define-prefix-key "W" 'write-file)
(gcs-define-prefix-key "b" 'buffer-menu)
(gcs-define-prefix-key "v" 'ido-switch-buffer)
(gcs-define-prefix-key "V" 'ido-switch-buffer-other-frame)
;; "\k" kills the buffer without asking and makes sure the buffer menu
;;  opens with point at the first line.
(gcs-define-prefix-key "k"
 (lambda ()
   (interactive)
   (kill-buffer (current-buffer))
   (let ((buffer-menu-buffer (get-buffer "*Buffer List*")))
     (when buffer-menu-buffer
       (with-current-buffer buffer-menu-buffer
         (revert-buffer)
         (gcs-buffer-menu-custom-font-lock)
         (goto-char (point-min)))))))

(gcs-define-prefix-key "c" 'compile)
(gcs-define-prefix-key "e" 'next-error)
(gcs-define-prefix-key "E" 'previous-error)
(gcs-define-prefix-key "r" 'eval-buffer)
(gcs-define-prefix-key "j" 'ace-jump-mode)
(gcs-define-prefix-key "x" 'smex)
(gcs-define-prefix-key "X" 'smex-major-mode-commands)
(gcs-define-prefix-key "0" 'delete-window)
(gcs-define-prefix-key "1" 'delete-other-windows)
(gcs-define-prefix-key "2" 'split-window-vertically)
(gcs-define-prefix-key "3" 'split-window-horizontally)

;; Use j and k pressed within .15 seconds to exit insert mode
(defun gcs-evil-maybe-exit (entry-key exit-key)
  (let ((modified (buffer-modified-p)))
    (insert entry-key)
    (let ((evt (read-event nil nil 0.15)))
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

;; Use \<left> and \<right> to navigate buffer list, ignoring buffer menu
(defun gcs-previous-buffer ()
  (interactive)
  (previous-buffer)
  (when (string= (buffer-name) "*Buffer List*") (previous-buffer)))
(gcs-define-prefix-key (kbd "<left>") 'gcs-previous-buffer)

(defun gcs-next-buffer ()
  (interactive)
  (next-buffer)
  (when (string= (buffer-name) "*Buffer List*") (next-buffer)))
(gcs-define-prefix-key (kbd "<right>") 'gcs-next-buffer)

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(gcs-define-prefix-key "\\" 'comment-dwim-line)

(defun gcs-toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8"
  (interactive)
  (setq tab-width (cond ((= tab-width 8) 4)
                        ((= tab-width 4) 2)
                        (t 8)))
  (message (format "Tab width is now %d" tab-width))
  (redraw-display))
(gcs-define-prefix-key "t" 'gcs-toggle-tab-width-setting)

(mapc (lambda (mode) (evil-set-initial-state mode 'emacs))
       '(inferior-emacs-lisp-mode
         comint-mode
         shell-mode
         term-mode
         magit-branch-manager-mode
         pianobar-mode))

(provide 'evil-mode-customizations)
