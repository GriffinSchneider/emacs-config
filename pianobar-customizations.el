(require 'pianobar)

(setq pianobar-username "griffinschneider@gmail.com")

;; Setup pianobar faces to follow the color scheme
(set-face-foreground 'pianobar-mode-input-face         (face-foreground 'font-lock-variable-name-face))
(set-face-foreground 'pianobar-mode-song-name-face     (face-foreground 'font-lock-builtin-face))
(set-face-foreground 'pianobar-mode-time-face          (face-foreground 'font-lock-constant-face))
(set-face-foreground 'pianobar-mode-choice-number-face (face-foreground 'font-lock-constant-face))
(set-face-foreground 'pianobar-mode-choice-item-face   (face-foreground 'font-lock-variable-name-face))
(set-face-foreground 'pianobar-mode-info-face          (face-foreground 'font-lock-comment-face))
(set-face-foreground 'pianobar-mode-prompt-face        (face-foreground 'font-lock-keyword-face))
(set-face-bold-p     'pianobar-mode-prompt-face t)

;; Use q to quit the pianobar window
(define-key pianobar-mode-map (kbd "q")
  (lambda (N)
    (interactive "p")
    ;; Check if pianobar is prompting, so you can still type "q" into prompts
    (if pianobar-is-prompting
        (self-insert-command N)
        (quit-window))))

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

;; Comint wants to put an overlay over whatever it thinks is the "prompt,"
;; setting the overlay's font-lock-face to comint-highlight-prompt whenever
;; comint-output-filter is run. This messes up pianobar-mode's highlighting,
;; so advise comint-output-filter to remover the font-lock-face of the overlay.
(defadvice comint-output-filter (after remove-comint-prompt-overlay activate)
  (when comint-last-prompt-overlay
      (overlay-put comint-last-prompt-overlay 'font-lock-face nil)))
;; Set comint-use-prompt-regexp to fix some additional comint highlighting
;; problems
(add-hook 'pianobar-mode-hook (lambda () (setq comint-use-prompt-regexp t)))

(provide 'pianobar-customizations)
