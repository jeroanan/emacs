;; Package management
(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package emacs
  :ensure nil
  :custom
  (blink-cursor-mode nil)
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (column-number-mode)
  (inhibit-startup-message t)
  (make-backup-files nil)
  (default-tab-width 3)
  (random t))

(use-package auto-complete
  :ensure t
  :demand t
  :config
  (global-auto-complete-mode t))

;; Ensured packages -- these will be auto-loaded if not already present
(use-package evil
  :ensure t
  :demand t
  :config
  (evil-mode 1))

(use-package helm
  :ensure t
  :config
  (helm-mode t))

(use-package linum-relative
  :ensure t
  :demand t
  :config
  (linum-relative-mode t))

(use-package magit
  :ensure t)

;; nyan nyan nyan
(use-package nyan-mode
  :ensure t
  :demand t
  :config
  (nyan-mode t))

(use-package rainbow-delimiters
  :ensure t)

(use-package js
  :custom
  (js-indent-level 4))

(use-package time
  :demand t
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date t)
  :config
  (display-time-mode t))

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

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key "\C-x\C-m" 'helm-M-x)

;; General editing preferences
(fset 'yes-or-no-p 'y-or-n-p)
(global-linum-mode 1) ;; Line numbers in all buffers

;; Disable evil for some modes...
(setq evil-disabled-modes '('dired-mode
			    'newsticker-mode
			    'newsticker-treeview-mode
			    'eww-mode))

(dolist (m evil-disabled-modes)
  (evil-set-initial-state m 'emacs))

(add-hook 'newsticker-treeview-mode-hook (lambda ()
					   (turn-off-evil-mode)))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'elisp-mode-hook 'rainbow-delimiters-mode)


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
(when (file-exists-p site-specific-config)
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
 '(custom-enabled-themes '(lavender))
 '(custom-safe-themes
   '("70936e3b54ca6d668354fdc87eea5f0a5129380c0c459598be943efba6ae1563" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default))
 '(display-battery-mode t)
 '(erc-nick "jeroanan")
 '(erc-user-full-name nil)
 '(fci-rule-color "#073642")
 '(helm-completion-style 'emacs)
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
 '(newsticker-url-list
   '(("Arch Linux News" "https://www.archlinux.org/feeds/news/" nil nil nil)))
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
   '(auto-complete use-package helm exwm vterm lavender-theme csharp-mode markdown-mode evil rainbow-delimiters emms racket-mode solarized-theme slime))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-background-moode nil)
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
(put 'magit-clean 'disabled nil)
