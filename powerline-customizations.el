(require 'init)

(zenburn-with-color-variables
  (set-face-foreground 'mode-line-buffer-id nil)
  (defface powerline-active3 `((t (:foreground ,zenburn-yellow :weight bold))) nil)
  (defface powerline-inactive3 `((t (:foreground ,zenburn-green))) nil))

(setq powerline-default-separator 'slant)

(defun gcs-propertized-evil-mode-tag ()
  (propertize evil-mode-line-tag 'font-lock-face
              ;; Don't propertize if we're not in the selected buffer
              (cond ((not (eq (current-buffer) (car (buffer-list)))) '())
                    ((evil-emacs-state-p)  '(:background "red"))
                    ((evil-motion-state-p) '(:background "orange"))
                    ((evil-visual-state-p) '(:background "blue" :foreground "white"))
                    ((evil-insert-state-p) '(:background "green"))
                    (t '()))))

(setq-default
 mode-line-format
 '("%e"
   (:eval
    (let* ((active (powerline-selected-window-active))
           (mode-line (if active 'mode-line 'mode-line-inactive))
           (face1 (if active 'powerline-active1 'powerline-inactive1))
           (face2 (if active 'powerline-active2 'powerline-inactive2))
           (face3 (if active 'powerline-active3 'powerline-inactive3))
           (separator-left (intern (format "powerline-%s-%s"
                                           (powerline-current-separator)
                                           (car powerline-default-separator-dir))))
           (separator-right (intern (format "powerline-%s-%s"
                                            (powerline-current-separator)
                                            (cdr powerline-default-separator-dir))))
           (lhs (list (powerline-raw "%*" nil)
                      (powerline-buffer-size nil 'l)
                      (powerline-buffer-id face3 'l)
                      (powerline-raw " ")
                      (funcall separator-left mode-line face1)
                      (powerline-narrow face1 'l)
                      (powerline-vc face1)
                      (powerline-raw " " face1)
                      (funcall separator-left face1 face2)))
           (rhs (list
                 (funcall separator-right face2 face1)
                 (powerline-raw global-mode-string face1 'r)
                 (powerline-raw "%4l" face1 'r)
                 (powerline-raw ":" face1)
                 (powerline-raw "%3c" face1 'r)
                 (funcall separator-right face1 mode-line)
                 (powerline-raw " ")
                 (powerline-raw "%6p" face3 'r)))
           (center (list (when (boundp 'erc-modified-channels-object)
                           (powerline-raw erc-modified-channels-object face2 'l))
                         (powerline-major-mode face2 'l)
                         (powerline-process face2)
                         (powerline-raw " :" face2)
                         (powerline-minor-modes face2 'l)
                         (powerline-raw " " face2)
                         )))
      (concat (gcs-propertized-evil-mode-tag)
              (powerline-render lhs)
              (powerline-fill-center face2 (/ (powerline-width center) 2.0))
              (powerline-render center)
              (powerline-fill face2 (powerline-width rhs))
              (powerline-render rhs))))))

(provide 'powerline-customizations)
