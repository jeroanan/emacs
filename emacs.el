(global-set-key [f5] "\M-x eshell")
(global-set-key [f6] "\M-x magit-status")
(global-set-key [f8] 'toggle-frame-fullscreen)
(global-set-key [f10] 'save-buffers-kill-emacs)
(global-set-key [f11] 'revert-buffer)
(global-set-key "\C-x\C-m" 'execute-extended-command)

(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-xm")
(global-unset-key "\C-x\C-b")

(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-x>" 'replace-string)
(global-set-key "\C-x/" 'comment-region)

(global-set-key "\C-x\C-b" 'switch-to-buffer)
(global-set-key "\C-Z" 'zap-up-to-char)
(global-set-key "\C-x\C-j" 'join-to-next-line)

(defun auto-complete-mode-maybe ()
  ""
  (unless (minibufferp (current-buffer))
    (auto-complete-mode t)))

(setq ac-modes '(racket-mode))

(server-start)

(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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
(setq package-list '(auto-complete
		     solarized-theme
		     magit
		     racket-mode
		     linum-relative
		     rainbow-delimiters
		     evil
		     markdown-mode
		     nyan-mode))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
	 (package-install package)))

(require 'auto-complete)
(global-auto-complete-mode t)

;; General editing preferences
(setq show-trailing-whitespace 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq default-tab-width 3)
(setq indent-tabs-mode -1)
(global-linum-mode 1) ;; Line numbers in all buffers

;;Activate evil-mode
(unless evil-mode (evil-mode))

;; nyan nyan nyan
(unless nyan-mode (nyan-mode))

(add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
(add-hook 'elisp-mode-hook 'rainbow-delimiters-mode)

;; javascrpit preferences
(setq js-indent-level 4)

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

;; Load OS-specific init
(cond ((string= system-type "gnu/linux")
       (load "~/.emacs.d/gnu-specific.el"))
      ((string= system-type "windows-nt")
       (load "~/windows-specific.el")))

;;Load site-specific init
(setq site-specific-config "~/site-specific.el")
(when (file-exists-p "~/site-specific.el")
  (load site-specific-config))

(defun join-to-next-line ()
  (interactive)
  (next-line)
  (join-line))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(solarized-gruvbox-dark))
 '(custom-safe-themes
   '("51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#3b6b40f432d6" "#07b9463c4d36" "#47a3341e358a" "#1d873c3f56d5" "#2d86441c3361" "#43b7362d3199" "#061d417f59d7"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(lsp-ui-doc-border "#93a1a1")
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
   '(csharp-mode nyan-mode markdown-mode evil rainbow-delimiters emms linum-relative racket-mode solarized-theme slime auto-complete))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
