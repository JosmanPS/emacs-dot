;;; web.el --- Web development configuration
;;
;;; Commentary:
;;
;; Useful functions:
;; - web-beautify-html
;; - web-beautify-css
;; - M-x httpd-start
;; - M-x impatient-mode
;;
;; Dependencies:
;; - js-beautify (npm install -g js-beautify)
;;
;; Resources:
;; - http://web-mode.org/
;; - https://github.com/smihica/emmet-mode
;; - https://github.com/netguy204/imp.el
;;
;;; Code:

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

;; Web-beautify keybindings
(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;; Web-beautify autoformat on save
(eval-after-load 'sgml-mode
  '(add-hook 'html-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))
(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

;; Emmet mode (M-x emmet-mode)
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; Skewer (live editing)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(provide 'web)
;;; web.el ends here
