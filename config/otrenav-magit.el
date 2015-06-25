;;; otrenav-magit.el --- Magit configuration
;;
;;; Commentary:
;;
;;; Code:

(require 'magit)

;; I've seen the notice
(setq magit-last-seen-setup-instructions "1.4.0")

;;
;; Full screen magit-status
;;
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'otrenav-magit)
;;; otrenav-magit.el ends here
