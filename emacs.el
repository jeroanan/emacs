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

(defun xhtml-doctype-strict ()
 "insert doctype"
 (interactive "*")
 (insert "&lt;!DOCTYPE html
       PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"&gt;")
)

(server-start)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq make-backup-files nil)
(column-number-mode)
(put 'upcase-region 'disabled nil)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(put 'downcase-region 'disabled nil)

(require 'package)

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

(setq show-trailing-whitespace 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq default-tab-width 3);; (add-to-list 'package-archives
(setq indent-tabs-mode -1)

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

