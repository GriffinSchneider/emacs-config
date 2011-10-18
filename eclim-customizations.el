(require 'eclim)
(setq eclim-auto-save t)
(global-eclim-mode)
(setq eclim-eclipse-dirs '("~/Dev/eclipse"))

(define-key evil-normal-state-map "\\p" 'eclim-manage-projects)

(provide 'eclim-customizations)
