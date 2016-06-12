
(setq inhibit-startup-message t)
(setq column-number-mode t)
(set-face-attribute 'default nil :family "Consolas" :height 120 :weight 'normal)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 80)

(pending-delete-mode)

(global-linum-mode 1)

;; (global-hl-line-mode 1)

(blink-cursor-mode 0)

(load-theme 'wombat)

;; turn off all bells as they are mainly distracting
(setq visible-bell 'nil)
(setq ring-bell-function 'ignore)

;; hightlight the current active buffer modeline for realz!
;; make this a function so we can call it if somebody messes with it, I'm looking at you Minitest!
(defun coder/mode-line-highlight-reset ()
  (interactive)
  (set-face-foreground 'mode-line "#00CC00")
  (set-face-background 'mode-line "#000000"))
(coder/mode-line-highlight-reset)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elscreen undo-tree which-key go-mode zoom-window yaml-mode textmate smex rvm robe request rainbow-delimiters puppet-mode php-mode noflet multiple-cursors multi-term monokai-theme markdown-mode magit-filenotify kill-ring-search js2-mode jdee iy-go-to-char ido-vertical-mode fsharp-mode feature-mode expand-region enh-ruby-mode color-theme-solarized better-defaults ag ac-slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "nil" :family "Consolas")))))
