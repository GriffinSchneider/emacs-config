(require 'powerline)


(setq powerline-arrow-shape 'arrow14)

;; Setup colors
(setq powerline-color1 "#598559")
(setq powerline-color2 "#383838")
(set-face-foreground 'mode-line "#030303")
(set-face-background 'mode-line "#f0dfaf")

;; Setup modeline font
(if (find-font (font-spec :name "Inconsolata"))
    (progn
      (set-face-font 'mode-line "Inconsolata-12")
      (set-face-font 'mode-line-inactive "Inconsolata-12")))

;; Setup modeline items
(defun gcs-propertized-evil-mode-tag ()
  (propertize evil-mode-line-tag 'font-lock-face
    ;; Don't propertize if we're not in the selected buffer
    (cond ((not (eq (current-buffer) (car (buffer-list)))) '())
          ((evil-emacs-state-p)  '(:background "red"))
          ((evil-motion-state-p) '(:background "orange"))
          ((evil-visual-state-p) '(:background "blue" :foreground "white"))
          ((evil-insert-state-p) '(:background "green"))
          (t '()))))

(setq-default mode-line-format
  (list "%e"
    '(:eval (concat
	     (gcs-propertized-evil-mode-tag)
	     (powerline-rmw           'left   nil)
	     (powerline-buffer-id     'left   nil  powerline-color1)
	     (powerline-major-mode    'left        powerline-color1)
	     (powerline-make-text     " : "        powerline-color1)
	     (powerline-minor-modes   'left        powerline-color1)
	     (powerline-narrow        'left        powerline-color1  powerline-color2)
	     (powerline-vc            'center                        powerline-color2)
	     (powerline-make-fill                                    powerline-color2)
	     (powerline-row           'right       powerline-color1  powerline-color2)
	     (powerline-make-text     ":"          powerline-color1)
	     (powerline-column        'right       powerline-color1)
	     (powerline-percent       'right  nil  powerline-color1)
	     (powerline-make-text     "  "    nil)))))


(provide 'powerline-customizations)

