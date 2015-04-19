(require 'init)

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(zenburn-with-color-variables
  (set-face-foreground 'mode-line-buffer-id nil)
  (defface powerline-active3 `((t (:foreground ,zenburn-yellow :weight bold))) nil)
  (defface powerline-inactive3 `((t (:foreground ,zenburn-green))) nil)

  (defface evil-mode-line-tag-normal-face `((t (:foreground ,zenburn-green))) nil)
  (defface evil-mode-line-tag-emacs-face `((t (:background "red" :foreground "white"))) nil)
  (defface evil-mode-line-tag-motion-face `((t (:background "orange"))) nil)
  (defface evil-mode-line-tag-visual-face `((t (:background "blue" :foreground "white"))) nil)
  (defface evil-mode-line-tag-insert-face `((t (:background "green" :foreground "black"))) nil))

(setq powerline-default-separator 'slant)

(defun gcs-evil-mode-line-tag-face ()
  (cond ((not (eq (current-buffer) (car (buffer-list)))) 'evil-mode-line-tag-normal-face)
        ((evil-normal-state-p) 'evil-mode-line-tag-normal-face)
        ((evil-emacs-state-p)  'evil-mode-line-tag-emacs-face)
        ((evil-motion-state-p) 'evil-mode-line-tag-motion-face)
        ((evil-visual-state-p) 'evil-mode-line-tag-visual-face)
        ((evil-insert-state-p) 'evil-mode-line-tag-visual-face)
        (t 'evil-mode-line-tag-normal-face)))

(setq-default
 mode-line-format
 '("%e"
   (:eval
    (let* ((active (powerline-selected-window-active))
           (mode-line (if active 'mode-line 'mode-line-inactive))
           (face1 (if active 'powerline-active1 'powerline-inactive1))
           (face2 (if active 'powerline-active2 'powerline-inactive2))
           (face3 (if active 'powerline-active3 'powerline-inactive3))
           (face4 (gcs-evil-mode-line-tag-face))
           (separator-left (intern (format "powerline-%s-%s"
                                           (powerline-current-separator)
                                           (car powerline-default-separator-dir))))
           (separator-right (intern (format "powerline-%s-%s"
                                            (powerline-current-separator)
                                            (cdr powerline-default-separator-dir))))
           (lhs (list (funcall separator-right mode-line face4)
                      (powerline-raw (s-chop-prefix " <" (s-chop-suffix "> " evil-mode-line-tag)) face4)
                      (funcall separator-left face4 mode-line)
                      (powerline-raw "%*" nil)
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
           (center (list (powerline-major-mode face2 'l)
                         (powerline-process face2))))
      (concat (powerline-render lhs)
              (powerline-fill-center face2 (/ (powerline-width center) 2.0))
              (powerline-render center)
              (powerline-fill face2 (powerline-width rhs))
              (powerline-render rhs))))))

(provide 'powerline-customizations)
