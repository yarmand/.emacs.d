(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun vi-open-line (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (if abovep
      (vi-open-line-above)
    (vi-open-line-below)))

;; code navigation
(defun gcd ()
  "change current folder to git root"
  (interactive)
  (cd (vc-root-dir)))

(defun compile-tags ()
  "compile etags for the current project defined by git root"
  (interactive)
  (gcd)
  (compile "etags --extra=+f -R *"))

;; check https://github.com/sigma/magit-gh-pulls/issues/76 if we can merge that
;; in to handle enterprise

;; URL for current branch PR should
;; look like this:
;; https://github.com/sideshowcoder/tbrokersetip/compare/master...sideshowcoder-patch-1?quick_pull=1
;; (requires  magit)
(defun coder/string-remove-until (string seperator)
  (nth 1 (split-string string seperator)))

(defun coder/string-remove-after (string seperator)
  (nth 0 (split-string string seperator)))

(defun coder/git-origin-url-host ()
  "Get the origin host part of the remote origin url, where the
url is in the format git@somehost:org/repo.git"
  (let ((url (magit-get "remote.origin.url")))
    (coder/string-remove-after (coder/string-remove-until url "@") ":")))

(defun coder/git-origin-org-repo ()
  (let ((url (magit-get "remote.origin.url")))
    (coder/string-remove-after (coder/string-remove-until url ":") ".git")))

(defun coder/visit-pr-url ()
  "Visit current branch PR url"
  (interactive)
  (let ((remote-branch (magit-get-current-branch)))
    (browse-url
     (format "https://%s/%s/pull/new/master...%s?quick_pull=1"
             (coder/git-origin-url-host)
             (coder/git-origin-org-repo)
             (magit-get-current-branch)))))
