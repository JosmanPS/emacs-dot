;;; lisp.el --- Lisp configuration

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("/.el$'" . emacs-lisp-mode))

(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'emacs-lisp-mode-hook (lambda () (setq truncate-lines t)))


(provide 'lisp)
;;; lisp.el ends here
