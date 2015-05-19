;;; plain_text.el --- Plain text configuration

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("/.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("/.mdown$" . markdown-mode))

(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (flyspell-mode t)))

(provide 'plain_text)
;;; plain_text.el ends here
