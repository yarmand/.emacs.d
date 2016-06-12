;; Set path to dependencies

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
       (expand-file-name "init.d" user-emacs-directory))

(setq auto-save-dir
      (expand-file-name "auto-save/" user-emacs-directory))

;; Set up load path
;;(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; keep auto-saves local
(setq backup-directory-alist `((".*" . ,auto-save-dir)))
(setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
(setq tramp-auto-save-directory temporary-file-directory)
(setq create-lockfiles nil)

;; we have enough memory, the default GC threshold is set to 0.8MB, I think 20MB are fine as well
(setq gc-cons-threshold 20000000)

(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(load (expand-file-name "packages.el" user-emacs-directory))

(if (file-exists-p settings-dir)
    (dolist (file (directory-files settings-dir t "\\.el$"))
      (load file)))


(require 'server)
(unless (server-running-p)
  (server-start))
