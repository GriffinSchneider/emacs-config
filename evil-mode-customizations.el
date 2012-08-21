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

;; "\k" kills the buffer without asking and makes sure the buffer menu
;;  opens with point at the first line.
(defun gcs-kill-buffer-command ()
  (interactive)
  (kill-buffer (current-buffer))
  (let ((buffer-menu-buffer (get-buffer "*Buffer List*")))
    (when buffer-menu-buffer
      (with-current-buffer buffer-menu-buffer
        (revert-buffer)
        (gcs-buffer-menu-custom-font-lock)
        (goto-char (point-min))))))

;; Use \<left> and \<right> to navigate buffer list, ignoring buffer menu
(defun gcs-previous-buffer ()
  (interactive)
  (previous-buffer)
  (when (string= (buffer-name) "*Buffer List*") (previous-buffer)))

(defun gcs-next-buffer ()
  (interactive)
  (next-buffer)
  (when (string= (buffer-name) "*Buffer List*") (next-buffer)))

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

(defun gcs-toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8"
  (interactive)
  (setq tab-width (cond ((= tab-width 8) 4)
                        ((= tab-width 4) 2)
                        (t 8)))
  (message (format "Tab width is now %d" tab-width))
  (redraw-display))

(defun gcs-prefix-key-command ()
  (interactive)
  (let ((old-cursor-color (face-background 'cursor))
        (old-overriding-local-map overriding-local-map))
    (set-cursor-color "Green")
    (let* ((overriding-local-map (make-sparse-keymap))
           (key (read-key-sequence nil))
           (overriding-local-map old-overriding-local-map))
      (set-cursor-color old-cursor-color)
      (mapc (lambda (binding)
              (when (equal (car binding) key)
                (call-interactively (cadr binding))))
            '(("q" quit-window)
              ("g" magit-status)
              ("s" sunrise)
              ("S" sunrise-cd)

              ("u" undo-tree-visualize)
              ("f" ido-find-file)
              ("F" ido-find-alternate-file)
              ("w" save-buffer)
              ("W" write-file)
              ("b" buffer-menu)
              ("v" ido-switch-buffer)
              ("V" ido-switch-buffer-other-frame)

              ("k" gcs-kill-buffer-command)
              
              ("c" compile)
              ("e" next-error)
              ("E" previous-error)
              ("r" eval-buffer)
              ("j" ace-jump-mode)
              ("x" smex)
              ("X" smex-major-mode-commands)
              ("0" delete-window)
              ("1" delete-other-windows)
              ("2" split-window-vertically)
              ("3" split-window-horizontally)

              ([left]  gcs-previous-buffer)
              ([right] gcs-next-buffer)
              ("\\"    comment-dwim-line)
              ("t"     gcs-toggle-tab-width-setting))))))

(defconst gcs-prefix-key "\\")
(defconst gcs-prefix-key-maps (list evil-normal-state-map
                                    evil-motion-state-map
                                    evil-emacs-state-map
                                    pianobar-mode-map))
(mapc (lambda (keymap)
        (define-key keymap gcs-prefix-key 'gcs-prefix-key-command))
      gcs-prefix-key-maps)

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


(mapc (lambda (mode) (evil-set-initial-state mode 'emacs))
       '(inferior-emacs-lisp-mode
         comint-mode
         shell-mode
         term-mode
         magit-branch-manager-mode
         pianobar-mode))

(provide 'evil-mode-customizations)
