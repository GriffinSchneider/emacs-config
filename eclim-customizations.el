(require 'eclim)
(setq eclim-auto-save t)
(setq eclim-eclipse-dirs '("~/Dev/eclipse"))

(define-key evil-normal-state-map "\\E" 'global-eclim-mode)
(define-key evil-normal-state-map "\\p" 'eclim-manage-projects)

(provide 'eclim-customizations)
