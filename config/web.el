;;; web.el --- Web development configuration

;;; Commentary:

;; TODO: (otn) put JavaScript in a different section

;;; Code:

;;
;; HTML
;;
(setq sgml-basic-offset 4)
(defun otrenav-syntax-color-hex ()
  "Syntax color hex values buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(add-hook 'css-mode-hook 'otrenav-syntax-color-hex)
(add-hook 'php-mode-hook 'otrenav-syntax-color-hex)
(add-hook 'html-mode-hook 'otrenav-syntax-color-hex)
(add-hook 'web-mode-hook 'otrenav-syntax-color-hex)
(add-hook 'markdown-mode-hook 'otrenav-syntax-color-hex)
(add-hook 'emacs-lisp-mode-hook 'otrenav-syntax-color-hex)

;;
;; CSS
;;

(provide 'web)
;;; web.el ends here
