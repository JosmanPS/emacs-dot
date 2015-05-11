;;
;; Initial window setup
;;
(toggle-frame-fullscreen)
(split-window-right)
(find-file "~/Projects")
(ansi-term explicit-shell-file-name)
(previous-buffer)
(other-window 1)
(switch-to-buffer "*ansi-term*")
(other-window 1)

