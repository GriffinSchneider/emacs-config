;;; adaptive-wrap-prefix.el --- Perform smart line-wrapping with wrap-prefix

;; Copyright (C) 2011  Stefan Monnier

;; Author: Stephen Berman <stephen.berman@gmx.net>
;;         Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the `adaptive-wrap-prefix-mode' minor mode which sets
;; the wrap-prefix property on the fly so that single-long-line paragraphs get
;; word-wrapped in a way similar to what you'd get with M-q using
;; adaptive-fill-mode, but without actually changing the buffer's text.

;;; Code:

(defun adaptive-wrap-prefix-function (beg end)
  "Indent the region between BEG and END with adaptive filling."
  (goto-char beg)
  (while (< (point) end)
    (let ((blp (line-beginning-position)))
      (put-text-property (point)
                         (progn (search-forward "\n" end 'move) (point))
                         'wrap-prefix
                         (concat "    " (fill-context-prefix blp (point)))))))

;;;###autoload
(define-minor-mode adaptive-wrap-prefix-mode
  "Wrap the buffer text with adaptive filling."
  :lighter ""
  (if adaptive-wrap-prefix-mode
      (jit-lock-register #'adaptive-wrap-prefix-function)
    (jit-lock-unregister #'adaptive-wrap-prefix-function)
    (with-silent-modifications
      (save-restriction
        (widen)
        (remove-text-properties (point-min) (point-max) '(wrap-prefix nil))))))

;;;###autoload
(define-globalized-minor-mode global-adaptive-wrap-prefix-mode
  adaptive-wrap-prefix-mode
  (lambda ()
    (adaptive-wrap-prefix-mode t)))

(provide 'adaptive-wrap-prefix)
;;; adaptive-wrap-prefix.el ends here
