;;
;; Emacs configuration file
;; @author: Omar Trejo
;;

;; Order is important. Be careful.

(load "~/.emacs.d/path.el")
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/editing.el")
(load "~/.emacs.d/org-mode.el")
(load "~/.emacs.d/web.el")
(load "~/.emacs.d/terminal.el")
(load "~/.emacs.d/latex.el")
(load "~/.emacs.d/helm.el")
(load "~/.emacs.d/spellcheck.el")
(load "~/.emacs.d/matlab.el")
(load "~/.emacs.d/ess.el")
(load "~/.emacs.d/lisp.el")
(load "~/.emacs.d/python.el")
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/init_setup.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(matlab-comment-anti-indent 0)
 '(matlab-fill-code t)
 '(matlab-functions-have-end t)
 '(matlab-indent-function-body t)
 '(matlab-shell-command-switches (quote ("-nodesktop -nosplash"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
