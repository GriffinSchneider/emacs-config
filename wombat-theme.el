;; Wombat color theme for Emacs
;;
;; Defines a color scheme resembling that of the Wombat color theme
;; for Vim by Lars H. Nielsen @ http://dengmao.wordpress.com/2007/01/22/vim-color-scheme-wombat/
;;
;; MIT License Copyright (c) 2009 Ben Lowery <blowery at gmail dot com>
;; Credits due to the excellent Wombat Vim theme
;;
;; All patches welcome

(require 'color-theme)

;;;###autoload
(defun color-theme-wombat ()
  "Color theme by Ben Lowery, based off the Wombat Vim theme by Lars H. Nielsen @ http://dengmao.wordpress.com/2007/01/22/vim-color-scheme-wombat/"
  (interactive)
  (color-theme-install
   '(color-theme-wombat
     ((background-color . "#111111")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "SteelBlue")
      (foreground-color . "#f6f3e8"))
     (default ((t (:background "#242424" :foreground "#f6f3e8"))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#0C1021" :foreground "#F8F8F8"))))
     (font-lock-builtin-face ((t (:foreground "#f6f3e8"))))
     (font-lock-comment-face ((t (:foreground "#99968b"))))
     (font-lock-constant-face ((t (:foreground "#e5786d"))))
     (font-lock-doc-string-face ((t (:italic t :foreground "#99968b"))))
     (font-lock-function-name-face ((t (:foreground "#cae682"))))
     (font-lock-variable-name-face ((t (:foreground "#8ac6f2"))))
     (font-lock-keyword-face ((t (:foreground "#8ac6f2"))))
     (font-lock-preprocessor-face ((t (:foreground "#e5786d"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))

     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))

     (font-lock-string-face ((t (:foreground "#95e454"))))
     (font-lock-type-face ((t (:foreground "#cae682"))))
     
     (font-lock-warning-face ((t (:bold t :foreground "yellow"))))

     (js2-error-face ((t (:background "red3"))))
     (js2-warning-face ((t (:background "purple3"))))
     (js2-external-variable-face ((t (:foreground "red" :bold t))))
     
     ;;(gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (region ((t (:background "#435869"))))
     (mode-line ((t (:background "#161616" :foreground "white"))))
     (highlight ((t (:background "#222222"))))
     ;;(highline-face ((t (:background "#777777"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (show-paren-match ((t (:foreground "goldenrod" :bold t))))
     ;;(text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     ;;(underline ((nil (:underline nil))))
     (org-todo ((t (:foreground "yellow" :bold t))))
     (org-hide ((t (:foreground "#0b0b0b"))))
)))
(provide 'color-theme-wombat)
