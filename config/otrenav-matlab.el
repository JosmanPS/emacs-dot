;;; otrenav-matlab.el --- Matlab configuration
;;
;;; Commentary:
;;
;;; Code:

(add-to-list
 'auto-mode-alist
 '("/.m$" . matlab-mode))

(custom-set-variables
 '(matlab-comment-anti-indent 0)
 '(matlab-fill-code t)
 '(matlab-functions-have-end t)
 '(matlab-indent-function-body t)
 '(matlab-shell-command-switches (quote ("-nodesktop -nosplash"))))

(global-set-key (kbd "<f10>") 'gud-cont)
(global-set-key (kbd "<f9>") 'gud-step)    ;; Step in
(global-set-key (kbd "<f8>") 'gud-next)    ;; Step 1
(global-set-key (kbd "<f7>") 'gud-finish)  ;; Step back

;; TODO: (otn) fix this
(add-hook 'matlab-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'matlab-mode-hook (lambda () (setq visual-line-mode nil)))

(provide 'otrenav-matlab)
;;; otrenav-matlab.el ends here
