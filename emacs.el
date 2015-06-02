(global-set-key [f5] "\M-x ansi-term")
(global-set-key [f10] 'save-buffers-kill-emacs)
(global-set-key [f11] 'revert-buffer)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-unset-key "\C-x\C-c")
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-xm")
(global-unset-key "\C-x\C-b")
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\M-." 'jedi:goto-definition)
(global-set-key "\M-," 'jedi:goto-definition-pop-marker)
(global-set-key "\M-/" 'jedi:get-in-function-call)

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
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

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

;; (setq jedi:complete-on-dot t)

;; (defun get-project-root (buf repo-type init-file)
;;   (vc-find-root (expand-file-name (buffer-file-name buf)) repo-type))

;; (defvar jedi-config:find-root-function 'get-project-root)

;; (defun current-buffer-project-root ()
;;   (funcall jedi-config:find-root-function
;; 			  (current-buffer)
;; 			  jedi-config:vcs-root-sentinel
;; 			  jedi-config:python-module-sentinel))



;; (add-to-list 'ac-sources 'ac-source-jedi-direct)

;; (defvar jedi-config:with-virtualenv nil)



;; (defun jedi-config:setup-server-args ()
;;   (defmacro add-args (arg-list arg-name arg-value)
;;   	 '(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))

;;    (let ((project-root (current-buffer-project-root)))

;; 	  (make-local-variable 'jedi:server-args)

;; 	  (when project-root
;; 		 (add-args jedi:server-args "--sys-path" project-root))))

;; (add-hook 'python-mode-hook 'jedi-config:setup-server-args)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((todo-categories "#" "r2r" "a" "arse" "new" "wl") (todo-categories "a" "arse" "new" "wl") (todo-categories "arse" "new" "wl") (todo-categories "wl" "Todo")))))

(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

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

