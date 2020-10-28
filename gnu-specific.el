(global-set-key "\C-x\C-p" 'emms-pause)

;; mu4e-related stuff

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 300)

;; Misc. functions
(defun get-cpu-temp ()
  (interactive)
  (message (shell-command-to-string  "/opt/vc/bin/vcgencmd measure_temp")))

;; Backlight
(defun show-backlight-level ()
  (interactive)
  (message (concat "Backlight: "
		  (string-trim (shell-command-to-string "xbacklight")))))

(defun backlight-set ()
  (interactive)
  (let ((x (read-string "Set backlight to: ")))
    (shell-command (concat "xbacklight -set " x))
    (show-backlight-level)))

(defun backlight-up ()
  (interactive)
  (shell-command "xbacklight +10")
  (show-backlight-level))

(defun backlight-down ()
  (interactive)
  (shell-command "xbacklight -10")
  (show-backlight-level))

;; slime stuff
(setq inferior-lisp-program "/usr/bin/sbcl")
;; emms config
(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/Music/")
