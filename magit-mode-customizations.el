
(require 'magit-blame)

;; Magit
(set-face-attribute 'magit-item-highlight nil :inherit nil :background nil)
;; "q" always kills magit buffers
(define-key magit-mode-map "q" (lambda () (interactive) (magit-mode-quit-window 'kill-buffer)))
(define-key magit-mode-map ";" 'magit-toggle-section)
;; Use j and k for navigation in magit-mode.
;; Remap "k" to be magit-goto-previous-section everywhere
(define-key magit-status-mode-map "k" 'magit-goto-previous-section)
(define-key magit-branch-manager-mode-map "k" 'magit-goto-previous-section)
(define-key magit-mode-map "k" 'magit-goto-previous-section)
;; Remap "K" to do what "k" used to do, wherever "k" used to be defined
(define-key magit-status-mode-map "K" 'magit-discard-item)
(define-key magit-branch-manager-mode-map "K" 'magit-discard-item)
;; Map "j" to magit-goto-next-section in eveywhere
(defun gcs-magit-j ()
  (interactive)
  (let ((next (magit-find-section-after (point))))
    (if next
        (magit-goto-section next)
      (goto-char (+ -1 (magit-section-end (magit-current-section)))))))
(define-key magit-status-mode-map "j" 'gcs-magit-j)
(define-key magit-mode-map "j" 'gcs-magit-j)
;; git-rebase-mode
(define-key git-rebase-mode-map "j" 'forward-line)
(define-key git-rebase-mode-map "k" 'git-rebase-backward-line)
(define-key git-rebase-mode-map "p" 'git-rebase-pick)
(define-key git-rebase-mode-map "K" 'git-rebase-kill-line)
(key-chord-define magit-mode-map "k;" (lambda () (interactive) (magit-goto-previous-section) (magit-toggle-section)))
(key-chord-define magit-mode-map "j;" (lambda () (interactive) (magit-goto-next-section) (magit-toggle-section)))
(key-chord-define magit-mode-map "l;" 'magit-git-command)

(defcustom magit-recent-log-max-count 15
  "How far back to go with `magit-insert-recent-log'"
  :package-version '(magit . "2.0.0")
  :group 'magit-status
  :type 'integer)

(defun magit-insert-recent-commits-graph ()
  ;; doesn't work if there aren't any commits yet. If `git rev-parse HEAD'
  ;; fails then there aren't any commits.
  (when (= 0 (call-process "git" nil nil nil "rev-parse" "HEAD"))
    (let ((revs (magit-git-lines "rev-list"
                                 (format "--max-count=%d" (1+ magit-recent-log-max-count))
                                 "HEAD")))
      (magit-git-insert-section (recent "Recent commits:")
          (lambda ()
            (ansi-color-apply-on-region (point-min) (point-max)))
        "log"
        "--graph"
        "--oneline"
        "--decorate"
        "--color"
        (if (> (length revs) magit-recent-log-max-count)
            ;; here we are. the reason we go through all this
            ;; `rev-list' effort is for this range. This results in a
            ;; dramatic performance improvement over --graph
            ;; --max-count for repos with lots of commits.
            (format "%s.." (car (last revs)))
          "HEAD")
        (format "--max-count=%d" magit-recent-log-max-count)))))

(setq magit-status-sections-hook
      '(magit-insert-status-local-line
        magit-insert-status-remote-line
        magit-insert-status-head-line
        magit-insert-status-tags-line
        magit-insert-status-merge-line
        magit-insert-status-rebase-lines
        magit-insert-empty-line
        magit-insert-pending-commits
        magit-insert-untracked-files
        magit-insert-unstaged-changes
        magit-insert-staged-changes
        magit-insert-unpushed-cherries
        magit-insert-unpulled-cherries
        magit-insert-recent-commits-graph
        magit-insert-stashes))

(provide 'magit-mode-customizations)
