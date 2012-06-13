;;; moonscript-mode.el --- a simplistic major-mode for editing Moonscript

(defvar moonscript-keywords
  '("class" "extends" "with" "export" "import" "from" "for" "in"))
(defvar moonscript-keywords-regex (regexp-opt moonscript-keywords 'symbols))

;; Class names - just match any word starting with a capital letter
(defvar moonscript-class-name-regex "\\<[A-Z]\\w*\\>")

(defvar moonscript-function-keywords
  '("->" "=>" "(" ")" "[" "]" "{" "}"))
(defvar moonscript-function-regex (regexp-opt moonscript-function-keywords))

;; Assignments - =, +=, -=. etc.
(defvar moonscript-assignment-regex "\\([-+/*%]\\|\\.\\.\\)?=")

(defvar moonscript-font-lock-defaults
  (eval-when-compile
    `((,moonscript-class-name-regex . font-lock-builtin-face)
      (,moonscript-function-regex   . font-lock-doc-face)
      (,moonscript-assignment-regex . font-lock-keyword-face)
      (,moonscript-keywords-regex   . font-lock-keyword-face)
      ("!"                          . font-lock-warning-face))))



(define-derived-mode moonscript-mode fundamental-mode
  (setq font-lock-defaults '(moonscript-font-lock-defaults))
  (setq mode-name "moonscript")

  (modify-syntax-entry ?\- ". 12b" moonscript-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" moonscript-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.moon$" . moonscript-mode))

(provide 'moonscript-mode)
