;;; otrenav-windows.el --- Visual layout configuration

;;; Commentary:
;;
;; This changes frequently

;;; Code:

(toggle-frame-fullscreen)
(neotree-dir "~/")
(other-window 1)
(split-window-vertically)
(ansi-term explicit-shell-file-name)
(other-window 1)
(enlarge-window 13)
;; (split-window-right)
(other-window 1)

(provide 'otrenav-windows)
;;; otrenav-windows.el ends here
