(require 'init)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;; Use cookies
(setq w3m-use-cookies t)

;; Use bookmarks as homepage.
(setq w3m-home-page "about://bookmark/")

;; Tab switching
(define-key w3m-mode-map (read-kbd-macro "C-<tab>") 'w3m-next-buffer)
(define-key w3m-mode-map (read-kbd-macro "C-S-<tab>") 'w3m-previous-buffer)
(define-key w3m-mode-map (read-kbd-macro "J") 'w3m-previous-buffer)
(define-key w3m-mode-map (read-kbd-macro "K") 'w3m-next-buffer)
(define-key w3m-mode-map (read-kbd-macro "t") 'w3m-select-buffer)

;; Tab creation/deletion
(define-key w3m-mode-map (read-kbd-macro "s-t") 'w3m-bookmark-view-new-session)
(define-key w3m-mode-map (read-kbd-macro "C-t C-t") 'w3m-copy-buffer)
(define-key w3m-mode-map (read-kbd-macro "s-w") 'w3m-delete-buffer)
(define-key w3m-mode-map (read-kbd-macro "x") 'w3m-delete-buffer)
(define-key w3m-mode-map (read-kbd-macro "s-T") 'w3m-session-select) ;; like undo close tab
(define-key w3m-mode-map (read-kbd-macro "X") 'w3m-session-select)

;; Link following
(define-key w3m-mode-map (read-kbd-macro "f") 'w3m-lnum-follow)
;; F to follow in new session. Stupid function only works with 4 as an argument :-/
(define-key w3m-mode-map (read-kbd-macro "F") (lambda () (interactive) (w3m-lnum-follow 4)))
;; Shift-enter to open in new tab
(define-key w3m-mode-map (read-kbd-macro "s-<return>") 'w3m-view-this-url-new-session)

;; Unbind space in w3m-mode-map, so it can still be used for ace-jump
(define-key w3m-mode-map (kbd "SPC") nil)

;; Use vim kebindings for searching and jumping, with p for previous search
;; since N is taken by w3m-mode.
(define-key w3m-mode-map (read-kbd-macro "/") 'evil-search-forward)
(define-key w3m-mode-map (read-kbd-macro "?") 'evil-search-backward)
(define-key w3m-mode-map (read-kbd-macro "n") 'evil-search-next)
(define-key w3m-mode-map (read-kbd-macro "p") 'evil-search-previous)
(define-key w3m-mode-map (read-kbd-macro "'") 'evil-goto-mark)
(define-key w3m-mode-map (read-kbd-macro "C-o") 'evil-jump-backward)
(define-key w3m-mode-map (read-kbd-macro "C-i") 'evil-jump-forward)

;; Use d and u for scrolling half-pages, C-j and C-k for scrolling less
(defun gcs-w3m-scroll-half-page (dir)
  (w3m-scroll-up-1 (* dir (/ (window-height) 2))))
(define-key w3m-mode-map (read-kbd-macro "d") (lambda () (interactive) (gcs-w3m-scroll-half-page 1)))
(define-key w3m-mode-map (read-kbd-macro "u") (lambda () (interactive) (gcs-w3m-scroll-half-page -1)))
(define-key w3m-mode-map (read-kbd-macro "C-j") (lambda () (interactive) (w3m-scroll-up-1 2)))
(define-key w3m-mode-map (read-kbd-macro "C-k") (lambda () (interactive) (w3m-scroll-up-1 -2)))

;; Emacs-w3m defines the session-select mode weirdly, so evil-set-initial-state doesn't work.
(add-to-list 'evil-buffer-regexps (cons " \\*w3m-session select\\*" 'emacs))

(provide 'w3m-customizations)
