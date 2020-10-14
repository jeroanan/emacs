(global-set-key "\C-x\C-p" 'emms-pause)

;; mu4e-related stuff

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 300)
