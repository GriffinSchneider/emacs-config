(require 'evil)
(require 'surround)
(require 'magit)

;; Start In evil-mode with surround.vim emulation
(evil-mode 1)
(global-surround-mode 1)

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
(define-key evil-normal-state-map gcs-prefix-key nil)
(define-key evil-motion-state-map gcs-prefix-key nil)
(defun gcs-define-key-with-prefix (key binding)
  (let* ((key     (read-kbd-macro (concat gcs-prefix-key " " key)))
         (maps    (list evil-normal-state-map evil-motion-state-map evil-emacs-state-map))
         (add-key (lambda (keymap) (define-key keymap key binding))))
    (mapcar add-key  maps)))

(gcs-define-key-with-prefix "g" 'magit-status)

(gcs-define-key-with-prefix "u" 'undo-tree-visualize)
(gcs-define-key-with-prefix "f" 'ido-find-file)
(gcs-define-key-with-prefix "F" 'ido-find-alternate-file)
(gcs-define-key-with-prefix "w" 'save-buffer)
(gcs-define-key-with-prefix "W" 'write-file)
(gcs-define-key-with-prefix "b" 'buffer-menu)
(gcs-define-key-with-prefix "v" 'ido-switch-buffer)
(gcs-define-key-with-prefix "V" 'ido-switch-buffer-other-frame)
;; "\k" kills the buffer without asking and makes sure the buffer menu
;;  opens with point at the first line.
(gcs-define-key-with-prefix "k"
 (lambda ()
   (interactive)
   (kill-buffer (current-buffer))
   (let ((buffer-menu-buffer (get-buffer "*Buffer List*")))
     (when buffer-menu-buffer
       (with-current-buffer buffer-menu-buffer
         (goto-line 0))))))

(gcs-define-key-with-prefix "c" 'compile)
(gcs-define-key-with-prefix "e" 'next-error)
(gcs-define-key-with-prefix "E" 'previous-error)
(gcs-define-key-with-prefix "r" 'eval-buffer)

(gcs-define-key-with-prefix "j" 'ace-jump-mode)

(gcs-define-key-with-prefix "x" 'smex)
(gcs-define-key-with-prefix "X" 'smex-major-mode-commands)

(gcs-define-key-with-prefix "0" 'delete-window)
(gcs-define-key-with-prefix "1" 'delete-other-windows)
(gcs-define-key-with-prefix "2" 'split-window-vertically)
(gcs-define-key-with-prefix "3" 'split-window-horizontally)

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
(gcs-define-key-with-prefix "<left>" 'gcs-previous-buffer)

(defun gcs-next-buffer ()
  (interactive)
  (next-buffer)
  (when (string= (buffer-name) "*Buffer List*") (next-buffer)))
(gcs-define-key-with-prefix "<right>" 'gcs-next-buffer)

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
(gcs-define-key-with-prefix "\\" 'comment-dwim-line)

(defun gcs-toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8"
  (interactive)
  (setq tab-width (cond ((= tab-width 8) 4)
                        ((= tab-width 4) 2)
                        (t 8)))
  (message (format "Tab width is now %d" tab-width))
  (redraw-display))
(gcs-define-key-with-prefix "t" 'gcs-toggle-tab-width-setting)

(loop for (mode . state) in '((inferior-emacs-lisp-mode  . emacs)
                              (comint-mode               . emacs)
                              (shell-mode                . emacs)
                              (term-mode                 . emacs)
                              (magit-branch-manager-mode . emacs))
      do (evil-set-initial-state mode state))

(provide 'evil-mode-customizations)
