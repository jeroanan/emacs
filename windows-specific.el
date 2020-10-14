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

;;; Add file extensions to major modes
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode))

(setenv "PATH"
	(concat
	 "C:\\cygwin64\\bin;"
	 (getenv "PATH")))
