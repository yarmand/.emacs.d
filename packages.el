;; handle packages from melpa and others to be automagically installed on boot of emacs if not available

(require 'package)

(when (>= emacs-major-version 24)
  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           )))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((smex . "melpa-stable")
          (slime . "melpa-stable")
          (rainbow-delimiters . "melpa-stable")
          (puppet-mode . "melpa-stable")
          (ido-vertical-mode . "melpa-stable")
          (ag . "melpa-stable")
          (robe . "melpa"))))

(setq my:packages
      '(better-defaults
        rainbow-delimiters
        ag
        s
        php-mode
        ido-vertical-mode
        jdee
        markdown-mode
        js2-mode
        puppet-mode
        enh-ruby-mode
        textmate
        magit
        yaml-mode
        multi-term
        request
        rvm
        fsharp-mode
        slime
        ac-slime
        multiple-cursors
        noflet ;; required by kill-ring-ido
        smex
        feature-mode
        robe

        iy-go-to-char
        magit-filenotify
        elscreen
        undo-tree
;; installed via git while I'm working on this mode.
;;        expand-region
        auto-complete))

(package-initialize)

(require 'multiple-cursors)
(global-set-key (kbd "s->") 'mc/mark-next-like-this)
(global-set-key (kbd "s-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c s-<") 'mc/mark-all-like-this)
(global-set-key (kbd "s-r") 'set-rectangular-region-anchor)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(when (null package-archive-contents)
  (package-refresh-contents))

(dolist (p my:packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'expand-region)

(require 'puppet-mode)

(require 'feature-mode)

(require 'ag)

;; Setup packages
(defun enable-flyspell-mode () (flyspell-mode 1))
(global-set-key (kbd "<f12>") 'flyspell-correct-word-before-point)

(require 'slime)
(require 'ac-slime)
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(add-hook 'text-mode-hook 'enable-flyspell-mode)
(add-hook 'change-log-mode-hook 'enable-flyspell-mode)
(add-hook 'log-edit-mode-hook 'enable-flyspell-mode)

(require 'textmate)
(textmate-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(setq js2-basic-offset 2)
(setq js2-strict-missing-semi-warning nil)

(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'enh-ruby-mode)

;; (add-hook 'ruby-mode-hook 'robe-mode)
;; (add-hook 'enh-ruby-mode-hook 'robe-mode)
;; (add-hook 'robe-mode-hook 'ac-robe-setup)
;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
;;   (rvm-activate-corresponding-ruby))

;; enable markdown mode for the common markdown extensions
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

(require 'smex)
(smex-initialize)

(require 'iy-go-to-char)

(require 'fsharp-mode)
(setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/local/bin/fsharpc")

;; make sure multi-term uses a login shell, otherwise weird stuff happens, like rvm not working
(setq multi-term-program-switches "--login")

;; better-defaults turns off the menubar, but I find it actually useful, at least in GUI mode
(when (display-graphic-p)
  (menu-bar-mode 1))

(require 'kill-ring-ido)
(global-set-key (kbd "M-y") 'kill-ring-ido)

(require 'ido-preview)

(require 'flyspell)
(add-hook 'markdown-mode 'flyspell-mode)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; org mode
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-agenda-files '("~/OneDrive/Notes"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)")))

;; ediff stolen from http://oremacs.com/2015/01/17/setting-up-ediff/
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")

;;
(winner-mode)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (bury-buffer "*compilation*")
         (winner-undo)
         (message "Build successful."))
        (t
         (message "Compilation exited abnormally: %s" string))))
(setq compilation-finish-functions 'compile-autoclose)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; go
(add-hook 'before-save-hook #'gofmt-before-save)

;; github
;; (require 'github_pr)

;; triling whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(which-key-mode 1)

(elscreen-start)

(undo-tree-mode 1)

(require 'back-button)
(back-button-mode 1)
