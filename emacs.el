(global-set-key [f5] "\M-x eshell")
(global-set-key [f10] 'save-buffers-kill-emacs)
(global-set-key [f11] 'revert-buffer)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-unset-key "\C-x\C-c")
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-xm")
(global-unset-key "\C-x\C-b")
(global-set-key "\C-xm" 'execute-extended-command)
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
(set-background-color "WhiteSmoke")
(set-foreground-color "black")
(set-cursor-color "black")
(put 'downcase-region 'disabled nil)

(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(defun show-trailing-whitespace ()
  (interactive "*")
   "Show trailing whitespace"
	(setq show-trailing-whitespace 1)
)

(setq show-trailing-whitespace 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq default-tab-width 3)
(setq indent-tabs-mode -1)

(load-library "/emacs/sql-server.el")
(color-theme-solarized-dark)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((todo-categories "#" "r2r" "a" "arse" "new" "wl") (todo-categories "a" "arse" "new" "wl") (todo-categories "arse" "new" "wl") (todo-categories "wl" "Todo")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
