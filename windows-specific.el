(defun msbuild-solution(solution-path)
  (interactive)
  (let ((msbuild-path "C:/Windows/Microsoft.NET/Framework64/v4.0.30319/"))
    (async-shell-command (concat msbuild-path "/MSBuild.exe " solution-path))))

(defun msbuild-publish-solution(solution-path publish-profile)
  (interactive)

  (let* ((msbuild-path "C:/Windows/Microsoft.NET/Framework64/v4.0.30319/")
         (command-line (concat msbuild-path "/MSBuild.exe " solution-path " /p:DeployOnBuild=true /p:PublishProfile=" publish-profile)))
    (message command-line)
    (async-shell-command command-line)))

(defun open-in-explorer ()
  (interactive)
  (call-process-shell-command (concat "start " default-directory) nil 0))

;; JIRA helpers
(defun jira-browse-issue ()
  (interactive)
  (if (boundp 'jira-base-url)
    (progn
      (setq issue-number (read-string "Issue number:"))
      (call-process-shell-command (concat "start " jira-base-url "/browse/" issue-number)))
    (message "jira-base-url is not set")))


;; Config file management
(defun export-config-to-git ()
  (interactive)
  (if (boundp 'emacs-config-git-dir)
      (progn
	(copy-file "~/.emacs" (concat emacs-config-git-dir "/emacs.el") "y")
	(copy-file "~/windows-specific.el" (concat emacs-config-git-dir "/windows-specific.el") "y")
	(message "config exported to git. Don't forget to go and commit it!"))
    (message "emacs-config-git-dir is not set.")))
	

;;; Add file extensions to major modes
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode))

(setenv "PATH"
	(concat
	 "C:\\cygwin64\\bin;"
	 (getenv "PATH")))


(global-set-key [f9] 'open-in-explorer)
