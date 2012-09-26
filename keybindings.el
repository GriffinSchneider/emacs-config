;; This file contains all keybindings that are global (they will affect all buffers unless overriden).
;; Bindings in the various evil state maps are considered global, since evil is global.

(require 'init)

(defun map2 (function list)
  (case (length list)
    (0 list)
    (1 (error "map2 got an odd-length list"))
    (t (cons (funcall function (first list) (second list))
             (map2 function (cddr list))))))

(defmacro global-set-keys (&rest bindings)
  `(progn ,@(map2 (lambda (key command)
                    (if (listp command)
                        `(global-set-key (kbd ,key) (lambda () (interactive) ,command))
                      `(global-set-key (kbd ,key) ',command)))
                  bindings)))

;;;;; GLOBAL-SET-KEY KEYS ;;;;;

(defun gcs-swap-windows (dir)
  (let* ((this-window (selected-window))
         (other-window (progn
                         (windmove-do-window-select dir)
                         (selected-window)))
         (this-buffer  (window-buffer this-window))
         (other-buffer (window-buffer other-window))
         (this-start   (window-start this-window))
         (other-start  (window-start other-window)))
    (set-window-buffer this-window  other-buffer)
    (set-window-buffer other-window this-buffer)
    (set-window-start  this-window  other-start)
    (set-window-start  other-window this-start)))

(defun gcs-change-around-paren ()
  (interactive)
  (let ((range (evil-a-paren)))
    (evil-change (first range) (second range))))

(global-set-keys
 ;; Replace normal m-x with smex
 "M-x" smex
 "M-X" smex-major-mode-commands

 "M-SPC" gcs-change-around-paren
 
 ;; Use s-s to toggle sr-speedbar
 "s-s" sr-speedbar-toggle

 ;; Use cmd-r to compile
 "s-r" compile

 ;; Use buffer-menu instead of list-buffers
 "\C-x\C-b" buffer-menu

 ;; Use C-w for backward-kill-word in the minibuffer
 "C-w" backward-kill-word

 ;; Use C-s-f to toggle fullscreen
 "C-s-f" ns-toggle-fullscreen

 ;; Use [C-]s-[y, u, i, o] to resize windows
 "s-y"   (shrink-window-horizontally 5)
 "s-u"   (shrink-window 5)
 "s-i"   (enlarge-window 5)
 "s-o"   (enlarge-window-horizontally 5)

 ;; Xcode-like keybindings
 "s-O" find-file-in-project

 ;; Use s-[h, j, k, l] for window navigation
 "s-h" windmove-left
 "s-l" windmove-right
 "s-k" windmove-up
 "s-j" windmove-down

 ;; Also use C-[arrow keys] for window navigation. Useful in terminal emacs.
 "C-<left>" windmove-left
 "C-<right>" windmove-right
 "C-<up>" windmove-up
 "C-<down>" windmove-down

 ;; Use s-[H, J, K, L] to swap windows
 "s-H" (gcs-swap-windows 'left)
 "s-J" (gcs-swap-windows 'down)
 "s-K" (gcs-swap-windows 'up)
 "s-L" (gcs-swap-windows 'right)

 ;; Make C-M-g the same as C-g - in case 'Esc' is pressed accidentally
 "\C-\M-g" keyboard-quit

 ;; Use C-u to scoll up like vim, move emacs's universal argument to C-S-u
 "C-u" evil-scroll-up
 "C-S-u" universal-argument)

;;;;;  PIANOBAR KEYS ;;;;;

;; Setup various pianobar commands with a s-p prefix
(defun gcs-map-pianobar-key (key command)
  (global-set-key (read-kbd-macro (concat "s-p " key)) command)
  (global-set-key (read-kbd-macro (concat "s-p s-" key)) command))
(global-set-key (kbd "s-p") nil)

(gcs-map-pianobar-key "p" 'pianobar)
(gcs-map-pianobar-key "n" 'pianobar-next-song)
(gcs-map-pianobar-key "SPC" 'pianobar-play-or-pause)
(gcs-map-pianobar-key "s" 'pianobar-change-station)
(gcs-map-pianobar-key "+" 'pianobar-love-current-song)
;; Use "s-p o" to print the current song, artist, and album
(gcs-map-pianobar-key "o"
  (lambda ()
    (interactive)
    (message "\"%s\" by \"%s\" on \"%s\""
             pianobar-current-song
             pianobar-current-artist
             pianobar-current-album)))

;;;;; EVIL STATE MAP KEYS ;;;;;

;; Use space for ace-jump
(defun gcs-define-evil-motion-key (key def)
  (define-key evil-normal-state-map key def)
  (define-key evil-visual-state-map key def)
  (define-key evil-motion-state-map key def))
(gcs-define-evil-motion-key (kbd "SPC") 'ace-jump-word-mode)
(gcs-define-evil-motion-key (kbd "C-SPC") 'ace-jump-char-mode)

;; Swap evil-goto-mark and evil-goto-mark-line bindings
(define-key evil-normal-state-map "'" 'evil-goto-mark)
(define-key evil-normal-state-map "`" 'evil-goto-mark-line)

;; Get rid of the "K" binding for evil-lookup
(define-key evil-motion-state-map "K" nil)
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


;;;;; PREFIX KEYBINDINGS ;;;;;

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

;; "\K" kills the buffer like gcs-kill-buffer-command, while also killing
;; the window.
(defun gcs-kill-buffer-and-window ()
  (interactive)
  (gcs-kill-buffer-command)
  (delete-window))

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

(defun gcs-find-file-dwim ()
  (interactive)
  (if (project-root)
      (find-file-in-project)
    (ido-find-file)))

(defun gcs-ack-in-project (command-args)
  (interactive (list (read-from-minibuffer "Run ack like this: " ack-command nil nil 'ack-history)))
  (if (project-root)
      (ack command-args (concat (project-root) "/"))
    (ack command-args)))

(defconst gcs-prefix-key-commands
  (mapcar
   (lambda (binding) (list (read-kbd-macro (first binding)) (second binding)))
   '(("q" quit-window)
     ("k" gcs-kill-buffer-command)
     ("K" gcs-kill-buffer-and-window)
     ("g" magit-status)
     ("s" sunrise-cd)
     ("S" sunrise)
     ("u" undo-tree-visualize)
     ("x" smex)
     ("X" smex-major-mode-commands)
     ("a" gcs-ack-in-project)
     
     ("d" gcs-find-file-dwim)
     ("f" ido-find-file)
     ("F" ido-find-alternate-file)
     
     ("w" save-buffer)
     ("W" write-file)
     ("b" buffer-menu)
     ("v" ido-switch-buffer)
     ("V" ido-switch-buffer-other-frame)

     ("s-v" visual-line-mode)
     ("s-b" magit-blame-mode)

     ("c" compile)
     ("e" next-error)
     ("E" previous-error)
     ("r" eval-buffer)
     
     ("0" delete-window)
     ("7" delete-window)
     ("1" delete-other-windows)
     ("2" split-window-horizontally)
     ("3" split-window-vertically)
     ("4" balance-windows)

     ("<left>"  gcs-previous-buffer)
     ("<right>" gcs-next-buffer)
     ("\\"    comment-dwim-line)
     ("t"     gcs-toggle-tab-width-setting))))

(defun gcs-prefix-key-command ()
  (interactive)
  (let* ((old-cursor-color (prog1 (face-background 'cursor) (set-cursor-color "Green")))
         (old-overriding-local-map overriding-local-map)
         (overriding-local-map (make-sparse-keymap))
         (key (read-key-sequence nil)))
    (setq overriding-local-map old-overriding-local-map)
    (set-cursor-color old-cursor-color)
    (call-interactively (second (assoc key gcs-prefix-key-commands)))))

(defconst gcs-prefix-key "\\")
(defconst gcs-prefix-key-maps (list evil-normal-state-map
                                    evil-motion-state-map
                                    evil-emacs-state-map
                                    pianobar-mode-map))
(mapc (lambda (keymap)
        (define-key keymap gcs-prefix-key 'gcs-prefix-key-command))
      gcs-prefix-key-maps)


(provide 'keybindings)
