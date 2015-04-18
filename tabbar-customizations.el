(require 'init)

(set-face-attribute 'tabbar-default nil
  :inherit nil
  :height 1.0
  :background "#383838"
  :box '(:line-width -1 :style released-button))
(set-face-attribute 'tabbar-unselected nil
  :foreground "#7f7f7f"
  :background nil
  :inherit 'tabbar-default)
(set-face-attribute 'tabbar-selected nil
  :foreground (face-foreground 'default)
  :inherit 'tabbar-default)
(set-face-attribute 'tabbar-button nil
                    :foreground (face-foreground 'tabbar-unselected)
                    :background nil
                    :box '(:line-width -1 :style released-button)
                    :inherit 'tabbar-default)
(set-face-attribute 'tabbar-separator nil
                    :height 1.0
                    :foreground (face-foreground 'tabbar-unselected)
                    :weight 'ultra-light
                    :inherit 'tabbar-default)

(setq tabbar-use-images nil
      tabbar-separator '("]["))
(tabbar-mode 1)

(add-hook 'sr-mode-hook (lambda () (tabbar-local-mode 1)))

(provide 'tabbar-customizations)
