
(global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c C-e") 'eval-region)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; windmove means Shift+arrow keys work
(windmove-default-keybindings)

;; frames keys
(global-set-key (kbd "C-x ]") 'elscreen-next)
(global-set-key (kbd "C-x [") 'elscreen-previous)
(global-set-key (kbd "C-x t c") 'elscreen-create)
(global-set-key (kbd "C-x t k") 'elscreen-kill)
(defalias 'tabedit 'elscreen-find-file)
(defalias 'tabnew 'elscreen-create)
(defalias 'tabclose 'elscreen-kill)
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)


(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; ag the current project
(setq ag-reuse-window 't)
(setq ag-highlight-search t)
(global-set-key (kbd "C-c /") 'ag-projecti-regex)

;; make sure we always have find-tag available as some modes overwrite the default M-.
(global-set-key (kbd "C-c .") 'find-tag)
(global-set-key (kbd "C-c C-c t") 'compile-tags)
(global-set-key (kbd "M-o") 'pop-tag-mark)


;; compile
(global-set-key (kbd "C-c c") 'recompile)

;; Textmate emulation from textmate mode bindings for reference
;;    ⌘T - Go to File
;;  ⇧⌘T - Go to Symbol
;;    ⌘L - Go to Line
;;  ⇧⌘L - Select Line (or expand Selection to select lines)
;;    ⌘/ - Comment Line (or Selection/Region)
;;    ⌘] - Shift Right (currently indents region)
;;    ⌘[ - Shift Left  (not yet implemented)
;;  ⌥⌘] - Align Assignments
;;  ⌥⌘[ - Indent Line
;;    ⌥↑ - Column Up
;;    ⌥↓ - Column Down
;;  ⌘RET - Insert Newline at Line's End
;;  ⌥⌘T - Reset File Cache (for Go to File)
(global-set-key (kbd "C-p") 'textmate-goto-file)
(global-set-key (kbd "C-c s") 'textmate-goto-symbol)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c C-c /") 'comment-line)

(global-set-key (kbd "C-c z") 'zap-to-char)
(global-set-key (kbd "C-c C-p C-p") 'eval-print-last-sexp)

(global-set-key (kbd "C-o") 'vi-open-line-below)
(global-set-key (kbd "C-c C-c o") 'vi-open-line-above)

;; easy spell check
(global-set-key (kbd "<f9>") 'ispell-word)
(global-set-key (kbd "C-<f9>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f9>") 'flyspell-check-next-highlighted-word)

(defun my:flyspell-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))
(global-set-key (kbd "M-<f10>") 'my:flyspell-save-word)
