;;; otrenav-web.el --- Web development configuration
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

;; Em met mode (M-x emmet-mode)
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; Skewer (live editing)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;;
;; Web-mode
;;

;; Pre
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(setq web-mode-ac-sources-alist
  '(("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
    ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'")))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 8)
  (setq web-mode-script-padding 8)
  (setq web-mode-block-padding 8)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-comment-keywords t)
  ;; (setq web-mode-comment-style 2) ;; Comment with server comment instead of client
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; This is for smartparens
;; (defun my-web-mode-hook ()
;;   (setq web-mode-enable-auto-pairing nil))

;; (add-hook 'web-mode-hook  'my-web-mode-hook)

;; (defun sp-web-mode-is-code-context (id action context)
;;   (when (and (eq action 'insert)
;;              (not (or (get-text-property (point) 'part-side)
;;                       (get-text-property (point) 'block-side))))

;;     t))

;; (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

;; This is for snippets
;; (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
;; (setq web-mode-extra-snippets
;;       '(("erb" . (("toto" . ("<% toto | %>\n\n<% end %>"))))
;;         ("php" . (("dowhile" . ("<?php do { ?>\n\n<?php } while (|); ?>"))
;;                   ("debug" . ("<?php error_log(__LINE__); ?>"))))
;;        ))

(provide 'otrenav-web)
;;; otrenav-web.el ends here
