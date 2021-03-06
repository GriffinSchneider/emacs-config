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

;; Fixup OSX keybindings
(when (equal system-type 'darwin)
  ;; Treat option as meta and command as super
  (setq mac-option-key-is-meta t)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  
  ;; Keybindings
  (global-set-keys
   "s-q" save-buffers-kill-terminal
   "s-v" yank
   "s-c" kill-ring-save
   "s-x" kill-region
   "s-w" delete-frame
   "s-n" make-frame
   "s-z" undo-tree-undo
   "s-s" save-buffer
   "s-Z" undo-tree-redo))

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

(defun gcs-multi-term-dedicated-toggle ()
  "Toggle the multi-term dedicated window. Then, if the window
was created, select it. If the window was dismissed, kill the
multi-term dedicated buffer without prompting."
  (interactive)
  (multi-term-dedicated-toggle)
  (if (multi-term-dedicated-exist-p)
      (multi-term-dedicated-select)
    (set-process-query-on-exit-flag (get-buffer-process multi-term-dedicated-buffer) nil)
    (kill-buffer multi-term-dedicated-buffer)))

(global-set-keys
 ;; Replace normal m-x with smex
 "M-x" smex
 "M-X" smex-major-mode-commands

 "M-0" (insert-char ?º)

 "M-SPC" gcs-change-around-paren
 
 "s-t" gcs-multi-term-dedicated-toggle
 "s-f" ns-popup-font-panel

 ;; Use cmd-r to compile
 "s-r" gcs-compile

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
 "M-k" tabbar-forward-tab

 ;; Revert buffer with no confirmation
 "M-s-R" (revert-buffer t t)

 ;; s-=, s--, and s-0 to adjust font size like in a browser
 "s-=" (text-scale-increase 1)
 "s--" (text-scale-increase -1)
 "s-0" (text-scale-adjust 0)
 )

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
(gcs-define-evil-motion-key (kbd "SPC") 'evil-ace-jump-word-mode)
(gcs-define-evil-motion-key (kbd "C-SPC") 'evil-ace-jump-char-mode)

;; Up and down to change navigate between overlays
(gcs-define-evil-motion-key (kbd "<up>")   'gcs-goto-previous-overlay)
(gcs-define-evil-motion-key (kbd "<down>") 'gcs-goto-next-overlay)

;; Swap evil-goto-mark and evil-goto-mark-line bindings
(define-key evil-normal-state-map "'" 'evil-goto-mark)
(define-key evil-normal-state-map "`" 'evil-goto-mark-line)

;; Use C-j and C-k for scrolling 2 lines up/down, similar to w3m binding
(gcs-define-evil-motion-key (read-kbd-macro "C-j") (lambda () (interactive) (evil-scroll-line-down 2) (evil-next-line 2)))
(gcs-define-evil-motion-key (read-kbd-macro "C-k") (lambda () (interactive) (evil-scroll-line-up 2) (evil-previous-line 2)))

(gcs-define-evil-motion-key (kbd "K") 'er/expand-region)
(gcs-define-evil-motion-key (kbd "s-K") 'er/contract-region)

;; Use return for fold-toggling, but only bind it in modes that support hs-minor-mode
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (not (string-match "Minibuf" (buffer-name)))
              (condition-case nil
                  (progn
                    (hs-minor-mode t)
                    (evil-local-set-key 'normal (kbd "<return>") 'evil-toggle-fold)
                    (evil-local-set-key 'normal (kbd "C-<return>") 'hs-hide-level)
                    (evil-local-set-key 'normal (kbd "s-<return>") 'evil-open-folds))
                (error nil)))))

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

;; Smooth half-page scrolling
(defun gcs-smooth-scroll-up (n)
  (when (> n 0)
    (scroll-down-line 20)
    (run-at-time 0.016 nil 'gcs-smooth-scroll-up (- n 20))))
(defun gcs-smooth-scroll-down (n)
  (when (> n 0)
    (scroll-up-line 20)
    (run-at-time 0.016 nil 'gcs-smooth-scroll-down (- n 15))))
(defun gcs-smooth-scroll-down-half-screen ()
  (interactive)
  (gcs-smooth-scroll-down (/ (window-height) 2)))
(defun gcs-smooth-scroll-up-half-screen ()
  (interactive)
  (gcs-smooth-scroll-up (/ (window-height) 2)))
(evil-global-set-key 'normal (kbd "C-d") 'gcs-smooth-scroll-down-half-screen)
(evil-global-set-key 'normal (kbd "C-u") 'gcs-smooth-scroll-up-half-screen)

;;;;; PREFIX KEYBINDINGS ;;;;;
;; "\k" kills the buffer without asking and makes sure the buffer menu
;;  opens with point at the first line.
(defun gcs-kill-buffer-command ()
  (interactive)
  (kill-buffer (current-buffer))
  (let ((buffer-menu-buffer (get-buffer "*Ibuffer*")))
    (when buffer-menu-buffer
      (with-current-buffer buffer-menu-buffer
        (ibuffer-update nil)))))

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
  (if (projectile-project-p)
      (projectile-find-file)
    (ido-find-file)))

(defun gcs-helm-dwim ()
  "Use helm to navigate the current buffer, with a list of things in the
buffer based on the buffer's major mode."
  (interactive)
  (case major-mode
    ('css-mode (helm-css-scss))
    ('objc-mode (projectile-find-tag))
    ('js2-mode (progn (evil-set-jump) (tern-find-definition)))
    ('c-mode (helm-etags-select 1))
    (t (message "Don't know how to helm here"))))

(defun gcs-show-in-finder ()
  (interactive)
  (if (shell-command "open -a \"Path Finder\" .")
      (shell-command "open .")))

(defun gcs-open-with-external-editor ()
  "If this buffer isn't visiting a file, show default-directory in finder.
If it is, open it with an external editor based on its major mode, or fallback
to OSX defaults for unknon modes."
  (interactive)
  (if (not (buffer-file-name))
      (gcs-show-in-finder)
    (let* ((appname (case major-mode
                      ('objc-mode "Xcode")
                      ('java-mode "Eclipse")
                      ('emacs-lisp-mode "TextEdit")
                      (t nil)))
           (command (concat "open "
                            (if appname (concat "-a " appname " ") " ")
                            "\"" (buffer-file-name) "\"")))
      (shell-command command))))

;; Like compile, but don't popup the compile buffer
(defun gcs-notify-compilation-result (buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (switch-to-buffer (car (buffer-list)))
  (if (string-match "^finished" msg)
      (gcs-big-temporary-popup "Compilation Successful :-)")
    (gcs-big-temporary-popup "Compilation Failed :-(")))
(when (not (member 'gcs-notify-compilation-result compilation-finish-functions))
  (add-to-list 'compilation-finish-functions 'gcs-notify-compilation-result))

(defun gcs-big-temporary-popup (message)
  (set-face-attribute 'popup-tip-face nil :height 3.0)
  (let ((buffer (current-buffer))
        (popup (popup-tip message
                          :nowait t
                          :point (save-excursion (next-line) (beginning-of-line) (point)))))
    (run-at-time "1 sec" nil
                 (lambda (popup)
                   (set-face-attribute 'popup-tip-face nil :height 1.0)
                   (popup-delete popup))
                 popup)))

(defun gcs-compile (command &optional comint)
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
    (save-window-excursion (compile command comint)))
  
(defconst gcs-prefix-key-commands
  (mapcar
   (lambda (binding) (list (read-kbd-macro (first binding)) (second binding)))
   '(("q"   quit-window)
     ("k"   gcs-kill-buffer-command)
     ("s-k" delete-window)
     ("K"   gcs-kill-buffer-and-window)
     ("g"   magit-status)
     ("l"   magit-file-log)
     ("u"   undo-tree-visualize)
     ("x"   smex)
     ("X"   smex-major-mode-commands)
     ("a"   ag-project)
     ("m"   multi-term)
     ("j"   gcs-helm-dwim)
     
     ("d"   gcs-find-file-dwim)
     ("f"   ido-find-file)
     ("F"   ido-find-alternate-file)
     ("s-f" gcs-show-in-finder)
     ("s-x" gcs-open-with-external-editor)
     
     ("w" save-buffer)
     ("W" write-file)
     ("b" ibuffer)
     ("v" ido-switch-buffer)
     ("V" ido-switch-buffer-other-frame)

     ("s-v" visual-line-mode)
     ("s-b" magit-blame-mode)

     ("c" gcs-compile)
     ("e" next-error)
     ("E" previous-error)
     ("r" eval-buffer)
     
     ("0" delete-window)
     ("7" delete-window)
     ("1" delete-other-windows)
     ("2" split-window-vertically)
     ("3" split-window-horizontally)
     ("4" balance-windows)

     ("<left>"  gcs-previous-buffer)
     ("<right>" gcs-next-buffer)
     ("\\"      comment-dwim-line-or-toggle-term-mode)
     ("s-t"     gcs-toggle-tab-width-setting))))

(defun gcs-prefix-key-command ()
  (interactive)
  (let* ((old-cursor-color (prog1 (face-background 'cursor) (set-cursor-color "Green")))
         (key (read-key-sequence nil)))
    (set-cursor-color old-cursor-color)
    (call-interactively (second (assoc key gcs-prefix-key-commands)))))

(defconst gcs-prefix-key "\\")
(defconst gcs-prefix-key-maps (list evil-normal-state-map
                                    evil-motion-state-map
                                    evil-emacs-state-map
                                    pianobar-mode-map))
(mapc (lambda (keymap)
        (define-key keymap gcs-prefix-key 'gcs-prefix-key-command)
        (define-key keymap (read-kbd-macro (concat "s-" gcs-prefix-key)) 'gcs-prefix-key-command))
      gcs-prefix-key-maps)


;;;;; KEY-CHORD KEYBINDINGS ;;;;;
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)

;; Any prefix key, "\x" can also be triggered with the key chord "jx"
(mapc (lambda (prefix-command)
        (let* ((key-string (first prefix-command))
               (key (aref key-string 0)))
          (when (and (numberp key) (<= key 126) (>= key 32)
                     (not (equal key-string "j"))
                     (not (equal key-string "k")))
            (key-chord-define-global (vector (aref "j" 0) key) (second prefix-command)))))
      gcs-prefix-key-commands)

(key-chord-define-global "jl" 'gcs-helm-dwim)

;; Numbers for window splitting
(key-chord-define-global "89" 'split-window-vertically)
(key-chord-define-global "78" 'split-window-horizontally)

;; First fingers column
(key-chord-define evil-normal-state-map "jk" 'keyboard-quit)
(key-chord-define minibuffer-local-map "jk" 'abort-recursive-edit)
(key-chord-define ibuffer-mode-map "jk" 'ibuffer-quit)
(key-chord-define-global "m," 'smex)

;; K + o or . for killing buffer or window
(key-chord-define-global "k." 'delete-window)
(key-chord-define-global "ko" 'gcs-kill-buffer-command)

;; H-chords for help
(key-chord-define-global "hf" 'describe-function)
(key-chord-define-global "hv" 'describe-variable)
(key-chord-define-global "hk" 'describe-key)

;; K + u or m for moving by half-screen
(key-chord-define-global "ku" 'gcs-smooth-scroll-up-half-screen)
(key-chord-define-global "km" 'gcs-smooth-scroll-down-half-screen)

(key-chord-define-global "kg" 'evil-goto-line)
(key-chord-define-global "kv" 'evil-visual-line)

(key-chord-define-global " j" 'yas-expand)

;; Semicolon chords for evaluation
(defun gcs-eval-dwim ()
  (interactive)
  (if (not mark-active)
      (call-interactively 'eval-last-sexp)
    (call-interactively 'eval-region)
    (message "eval-ed.")))

(key-chord-define-global "j;" 'gcs-eval-dwim)
(key-chord-define-global "k;" 'eval-defun)
(key-chord-define-global "l;" 'eval-expression)


(provide 'keybindings)
