;;; otrenav-javascript.el --- JavaScript configuration
;;
;;; Commentary:
;;
;; Useful functions:
;; 1. web-beautify-js
;;
;; Dependencies:
;; 1. jshint      (npm install -g jshint)
;; 2. js-beautify (npm install -g js-beautify)
;;
;;; Code:

;; NPM Repl (M-x nodejs-repl)
(require 'nodejs-repl)

;; JSHint (JavaScript tips)
(add-hook 'javascript-mode-hook
     (lambda () (flymake-mode t)))

(setq js-indent-level 4)
(defun otrenav-js-custom ()
  "JavaScript mode hook."
  (setq js-indent-level 4))
(add-hook 'js-mode-hook 'otrenav-js-custom)
(add-hook 'web-mode-hook 'otrenav-js-custom)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; Use the jshint lint
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;; '(js3-auto-indent-p t)         ; it's nice for commas to right themselves.
;; '(js3-enter-indents-newline t) ; don't need to push tab before typing
;; '(js3-indent-on-enter-key t)   ; fix indenting before moving on

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; Disable JSHint
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; Enable ESLint
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; Highlight everything you can
(setq js2-highlight-level 3)

(defun otrenav-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))
(add-hook 'web-mode-hook 'otrenav-mode-hook)


;; Web-beautify key bindings
(require 'web-beautify)
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

;; Web-beautify autoformat on save
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))
(eval-after-load 'js
  '(add-hook 'js-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))
(eval-after-load 'json-mode
  '(add-hook 'json-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(provide 'otrenav-javascript)
;;; otrenav-javascript.el ends here
