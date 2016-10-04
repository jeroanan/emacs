(global-set-key [f5] "\M-x eshell")
(global-set-key [f6] "\M-x magit-status")
(global-set-key [f8] 'toggle-frame-fullscreen)
(global-set-key [f10] 'save-buffers-kill-emacs)
(global-set-key [f11] 'revert-buffer)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-unset-key "\C-x\C-c")
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-xm")
(global-unset-key "\C-x\C-b")

(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-x>" 'replace-string)
(global-set-key "\C-x/" 'comment-region)

(global-set-key "\C-x\C-b" 'switch-to-buffer)
(global-set-key "\C-x\C-p" 'emms-pause)

(server-start)

;; Get rid of unneeded screen chrome.
(defun turn-off-screen-crap ()
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(case window-system
  (x (turn-off-screen-crap))
  (otherwise ()))

(setq make-backup-files nil)
(column-number-mode)

;; Status bar settings
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Package management
(require 'package)

; The contents of package-list will be downloaded and installed on startup if they're not already present.
(setq package-list '(projectile auto-complete jedi magit))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
	 (package-install package)))

;; Add file extensions to major modes
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode))

;; General editing preferences
(setq show-trailing-whitespace 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq default-tab-width 3)
(setq indent-tabs-mode -1)
(global-linum-mode 1) ;; Line numbers in all buffers
(setq ring-bell-function 'ignore) ;; turn off the bell
(setq js-indent-level 2)
(add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
(add-hook 'elisp-mode-hook 'rainbow-delimiters-mode)

;; Python IDE -- https://www.youtube.com/watch?v=6BlTGPsjGJk
(require 'projectile)
(projectile-global-mode)

(require 'auto-complete-config)
(ac-config-default)
(setq ac-show-menu-immediately-on-auto-complete t)

(require 'jedi)
(setq jedi:server-args
		'("--sys-path" "/home/david/src/GameCollection"))

(defvar jedi-config:vcs-root-sentinel ".git")
(defvar jedi-config:python-module-sentinel "__init__.py")
(add-hook 'python-mode-hook 'jedi:setup)

(random t)

(defun insert-random-uuid ()
  "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d
WARNING: this is a simple implementation. The chance of generating the same UUID is much higher than a robust algorithm.."
  (interactive)
  (insert
   (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 6))
           (random (expt 16 6)) ) ) )
           
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))



(defun zip-directory (directory-name)
  "Zip a directory name into the same name.zip"
  (interactive)
  (let ((zip-file-name (concat directory-name ".zip")))
	 (if (file-exists-p zip-file-name)
		  (delete-file zip-file-name)
		()) 
	 (async-shell-command (concat "zip -r " zip-file-name " " directory-name))))

(defun javascript-doc-skeleton()
  "Insert a sekeleton javascript docstring"
  (interactive)
  (insert-string "/**\n")
  (insert-string " * ")
  (beginning-of-line)
  (indent-relative)
  (end-of-line)
  (insert-string " \n*/")
  (beginning-of-line)
  (indent-relative)  
  (previous-line)
  (end-of-line))

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
