(require 'init)


;; Xcode-document-viewer
(setq gcs-docsetutil-path "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil")
;; Needs an absolute path
(setq xcdoc:document-path (concat (getenv "HOME") "/Library/Developer/Shared/Documentation/DocSets/com.apple.adc.documentation.AppleiOS7.0.iOSLibrary.docset"))

(setq xcdoc:open-eww-other-buffer t)
(defun xcdoc:docsetutil-command () ""
  (if (file-executable-p gcs-docsetutil-path)
      gcs-docsetutil-path
    (message "Couldn't find docsetutil!")))

;; Helm source for definitions and pragmas in current file
(defvar helm-c-source-objc-headline
  '((name . "Objective-C Headline")
    (headline  "^[-+@]\\|^#pragma mark")))
(defun objc-headline ()
  (interactive)
  ;; Set to 500 so it is displayed even if all methods are not narrowed down.
  (let ((helm-candidate-number-limit 500))
    (helm-other-buffer '(helm-c-source-objc-headline) "*ObjC Headline*")))

(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
;; Use objc-mode for objective-c++ files
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.pch$" . objc-mode))

(defun gcs-objc-mode-hook ()
  ;; Make ff-find-other-file toggle between .m and .h
  (set (make-local-variable 'cc-other-file-alist)
       '(("\\.m" (".h")) ("\\.h" (".m"))))
  
  ;; Setup indentation
  (setq tab-width 4)
  (c-set-style "java")
  (c-set-offset 'brace-list-close '-)
  (c-set-offset 'brace-list-intro '0)
  (c-set-offset 'arglist-close '0)
  ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'substatement-open '0) 
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  ; all "opens" should be indented by the c-indent-level
  (c-set-offset 'brace-list-open '+)   
  ; indent case labels by c-indent-level, too
  (c-set-offset 'case-label '+))
(add-hook 'objc-mode-hook 'gcs-objc-mode-hook)


(provide 'objective-c-customizations)
