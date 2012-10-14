(require 'init)


;; Xcode-document-viewer
(setq gcs-docsetutil-path "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil")
;; Needs an absolute path
(setq xcdoc:document-path (concat (getenv "HOME") "/Library/Developer/Shared/Documentation/DocSets/com.apple.adc.documentation.AppleiOS5_1.iOSLibrary.docset"))
(setq xcdoc:open-w3m-other-buffer t)
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

;; Make ff-find-other-file toggle between .m and .h
(add-hook 'objc-mode-hook (lambda ()
                            (set (make-local-variable 'cc-other-file-alist)
                                 '(("\\.m" (".h")) ("\\.h" (".m"))))))


(provide 'objective-c-customizations)
