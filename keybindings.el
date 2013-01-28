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

(defun gcs-put-buffer-in-window (dir)
  "Pop the current window's buffer off the window's buffer list
and push it onto the buffer list of the window in direction DIR."
  (let* ((this-window (selected-window))
         (other-window (progn
                         (windmove-do-window-select dir)
                         (selected-window)))
         (this-buffer  (window-buffer this-window))
         (other-buffer (window-buffer other-window))
         (this-start   (window-start this-window))
         (other-start  (window-start other-window)))
    (set-window-buffer other-window this-buffer)
    (set-window-start  other-window this-start)
    (switch-to-prev-buffer this-window)))

(defun gcs-change-around-paren ()
  (interactive)
  (let ((range (evil-a-paren)))
    (evil-change (first range) (second range))))

(defun gcs-ibuffer ()
  (interactive)
  (gcs-setup-eproject-filter-groups)
  (ibuffer))

(global-set-keys
 ;; Replace normal m-x with smex
 "M-x" smex
 "M-X" smex-major-mode-commands

 "M-SPC" gcs-change-around-paren
 
 ;; Use s-s to toggle sr-speedbar
 "s-s" sr-speedbar-toggle

 "s-t" (progn (multi-term-dedicated-toggle) (when (multi-term-dedicated-exist-p) (multi-term-dedicated-select)))
 "s-f" ns-popup-font-panel

 ;; Use cmd-r to compile
 "s-r" compile

 ;; Use ibuffer instead of list-buffers
 "\C-x\C-b" gcs-ibuffer

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
 "s-O" gcs-find-file-dwim
 "C-s-<up>" ff-find-other-file

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
 "s-H" (gcs-put-buffer-in-window 'left)
 "s-J" (gcs-put-buffer-in-window 'down)
 "s-K" (gcs-put-buffer-in-window 'up)
 "s-L" (gcs-put-buffer-in-window 'right)

 ;; Make C-M-g the same as C-g - in case 'Esc' is pressed accidentally
 "\C-\M-g" keyboard-quit

 ;; Use C-u to scoll up like vim, move emacs's universal argument to C-S-u
 "C-u" evil-scroll-up
 "C-S-u" universal-argument

 ;; Tab navigation
 "M-j" tabbar-backward-tab
 "M-k" tabbar-forward-tab)

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
(defun gcs-define-evil-motion-key (key def)
  (define-key evil-normal-state-map key def)
  (define-key evil-visual-state-map key def)
  (define-key evil-motion-state-map key def))

;; Goto the next overlay. Useful to switch between overlays
;; from hs-mode (used by evil-mode for folding).
(defun gcs-goto-next-overlay ()
  (interactive)
  (next-line)
  (goto-char (next-overlay-change (point))))
(defun gcs-goto-previous-overlay ()
  (interactive)
  (previous-line)
  (goto-char (previous-overlay-change (point))))

;; Use space for ace-jump
(gcs-define-evil-motion-key (kbd "SPC") 'ace-jump-word-mode)
(gcs-define-evil-motion-key (kbd "C-SPC") 'ace-jump-char-mode)

;; Up and down to change navigate between overlays
(gcs-define-evil-motion-key (kbd "<up>")   'gcs-goto-previous-overlay)
(gcs-define-evil-motion-key (kbd "<down>") 'gcs-goto-next-overlay)

;; Swap evil-goto-mark and evil-goto-mark-line bindings
(define-key evil-normal-state-map "'" 'evil-goto-mark)
(define-key evil-normal-state-map "`" 'evil-goto-mark-line)

;; Use C-j and C-k for scrolling 2 lines up/down, similar to w3m binding
(gcs-define-evil-motion-key (read-kbd-macro "C-j") (lambda () (interactive) (evil-scroll-line-down 2) (evil-next-line 2)))
(gcs-define-evil-motion-key (read-kbd-macro "C-k") (lambda () (interactive) (evil-scroll-line-up 2) (evil-previous-line 2)))

;; Use return for fold-toggling
(define-key evil-normal-state-map (kbd "<return>") 'evil-toggle-fold)
(define-key evil-normal-state-map (kbd "C-<return>") 'evil-close-folds)
(define-key evil-normal-state-map (kbd "s-<return>") 'evil-open-folds)

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
  (let ((buffer-menu-buffer (get-buffer "*Ibuffer*")))
    (when buffer-menu-buffer
      (with-current-buffer buffer-menu-buffer
        (revert-buffer)
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
  (when (string= (buffer-name) "*Ibuffer*") (previous-buffer)))

(defun gcs-next-buffer ()
  (interactive)
  (next-buffer)
  (when (string= (buffer-name) "*Ibuffer*") (next-buffer)))

(defun comment-dwim-line-or-toggle-term-mode (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line.
   Also, toggles between term-line-mode and term-char-mode in multi-term"
  (interactive "*P")
  (if (equal 'term-mode major-mode)
      (if (term-in-line-mode)
          (progn (term-char-mode) (message "CHAR MODE"))
        (term-line-mode) (message "LINE MODE"))
    
    (comment-normalize-vars)
    (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (comment-dwim arg))))

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
  (if (eproject-root)
      (helm-eproject)
    (ido-find-file)))

(defun gcs-ack-in-project (command-args)
  (interactive (list (read-from-minibuffer "Run ack like this: " ack-command nil nil 'ack-history)))
  (if (eproject-root)
      (ack command-args (concat (eproject-root) "/"))
    (ack command-args)))

(defun gcs-show-in-finder ()
  (interactive)
  (shell-command "open ."))

(defun gcs-open-with-osx ()
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))


(defconst gcs-prefix-key-commands
  (mapcar
   (lambda (binding) (list (read-kbd-macro (first binding)) (second binding)))
   '(("q"   quit-window)
     ("k"   gcs-kill-buffer-command)
     ("s-k" delete-window)
     ("K"   gcs-kill-buffer-and-window)
     ("g"   magit-status)
     ("l"   magit-file-log)
     ("s"   sunrise-cd)
     ("S"   sunrise)
     ("u"   undo-tree-visualize)
     ("x"   smex)
     ("X"   smex-major-mode-commands)
     ("a"   gcs-ack-in-project)
     ("m"   multi-term)
     
     ("d"   gcs-find-file-dwim)
     ("f"   ido-find-file)
     ("F"   ido-find-alternate-file)
     ("s-f" gcs-show-in-finder)
     ("s-o" gcs-open-with-osx)
     
     ("w" save-buffer)
     ("W" write-file)
     ("b" gcs-ibuffer)
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
     ("\\"      comment-dwim-line-or-toggle-term-mode)
     ("t"       helm-c-etags-select)
     ("s-t"     gcs-toggle-tab-width-setting))))

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
