(require 'init)

(setq pianobar-username "griffinschneider@gmail.com")

;; Setup pianobar faces to follow the color scheme
(set-face-foreground 'pianobar-mode-input-face         (face-foreground 'font-lock-variable-name-face))
(set-face-foreground 'pianobar-mode-song-name-face     (face-foreground 'font-lock-function-name-face))
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

(defun gcs-pianobar-mode-hook ()
  (rainbow-delimiters-mode-disable))
(add-hook 'pianobar-mode-hook 'gcs-pianobar-mode-hook)

(provide 'pianobar-customizations)
