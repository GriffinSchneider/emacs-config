(require 'init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Objective-C Project Type 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This type should be used as a supertype for individual obj-c projects
;;
;; User-provided attributes:
;;  :ac-user-cflags - flags to add to ac-clang-flags.
;;
;; Auto-generated Attributes:
;;  :ac-clang-cflags - Flags to use for clang autocompletion. This contains absolute paths, so
;;    it's an individual-project-level variable. This attribute is updated only once per project
;;    root.
;;
;;  :relative-include-paths - Relative paths of all folders in the project that contain .h files.
;;    This attribute is also updated only once per project root.

(define-project-type gcs-objective-c (generic-git) nil
  :ac-clang-cflags (lambda (root) (gcs-project-get-ac-clang-cflags root)))

(defconst gcs-objective-c-ac-clang-flags
  '("-fblocks"
    "-fno-caret-diagnostics"
    "-std=gnu99"))

(defun directory-dirs-with-headers (dir)
  "Find all directories in dir containing any .h files."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
        (let ((file (concat dir "/" file)))
          (when (file-directory-p file)
            (if (> (length (directory-files file nil ".*\\.h" t)) 0)
                (setq dirs (append (cons file (directory-dirs-with-headers file)) dirs))
              (setq dirs (append (directory-dirs-with-headers file) dirs)))))))
    dirs))

(defun gcs-project-get-relative-include-paths (root)
  (mapcar (lambda (dir) (file-relative-name dir root))
          (directory-dirs-with-headers root)))

(defun gcs-project-make-absolute-include-paths (root relative-include-paths)
  (mapcar (lambda (relative-dir) (concat "-I" root relative-dir))
          relative-include-paths))

(defun gcs-project-get-ac-clang-cflags (root)
  (let* ((relative-include-paths (gcs-project-get-relative-include-paths root))
         (absolute-include-flags (gcs-project-make-absolute-include-paths root relative-include-paths)))
    (append gcs-objective-c-ac-clang-flags
            (eproject-attribute :ac-user-cflags root)
            absolute-include-flags)))

(defun gcs-objecive-c-project-file-visit ()
  (when (equal major-mode 'objc-mode)
    (setq ac-clang-cflags (eproject-attribute :ac-clang-cflags))
    (message "Setting up clang autocompletion")
    (setq ac-sources '(ac-source-clang-async))
    (ac-clang-launch-completion-process)))


(provide 'eproject-customizations)
