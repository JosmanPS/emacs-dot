;;; otrenav-plain-text.el --- Plain text configuration

;;; Commentary:

;;; Code:

;;; Markdown: http://jblevins.org/projects/markdown-mode/

(add-to-list 'auto-mode-alist '("/.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("/.mdown$" . markdown-mode))

(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (flyspell-mode t)))

(provide 'otrenav-plain-text)
;;; otrenav-plain-text.el ends here
