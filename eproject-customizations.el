(require 'init)

(defconst gcs-buffer-regexps-never-in-projects
  '("*Help*" "*Completions*"))

(defun gcs-buffer-name-excluded-fromp-projects-p (name)
  (some (lambda (r) (string-match r name))
        gcs-buffer-regexps-never-in-projects))

;; These advices make eproject use a buffer's default-directory to determine its project
;; membership if the buffer isn't visiting a file. Buffers in
;; gcs-buffer-regexps-never-in-projects are never included in projects

;; When eproject tries to get the buffer's filename, return the buffer's default directory
;; if it isn't visiting a file.
(defadvice eproject--buffer-file-name (around eproject-fallback-to-default-dir activate)
  (if (gcs-buffer-name-excluded-fromp-projects-p (buffer-name))
      (setq ad-return-value nil)
    (setq ad-return-value (or (buffer-file-name)
                              default-directory))))
;; Stop eproject from checking whether the current buffer is visiting a file before activating.
(defadvice eproject--after-change-major-mode-hook (around eproject-dont-check-for-filename activate)
  (when (and (eproject--buffer-file-name) (not eproject-root)) (eproject-maybe-turn-on)))

;; This is a replacement for eproject's usual "look-for" form in the define-project-type
;; macro's SELECTOR. Use this in local project defs so non-file buffers will be included
;; in your projects.
(defun gcs-eproject-look-for (filename)
  (eproject--find-file-named (file-name-directory file) filename))

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
