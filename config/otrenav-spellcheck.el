;;; spellcheck.el --- Spell check configuration

;;; Commentary:

;;; Code:

(require 'flyspell-lazy)
(flyspell-lazy-mode t)
(setq flyspell-issue-welcome-flag nil)
(setq ispell-dictionary "english")
(setq ispell-program-name "aspell")

(require 'flymake)
(defun flymake-get-tex-args (file-name)
  (list "pdflatex" (list
                    "-file-line-error"
                    "-draftmode"
                    "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)

(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))

(setq-default ispell-list-command "list")

(dolist (mode '(emacs-lisp-mode-hook
                latex-mode-hook
                python-mode-hook
                R-mode-hook))
  (add-hook mode '(lambda () (flyspell-prog-mode))))

(defun flyspell-check-next-word ()
  "Spell check next word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(global-set-key (kbd "M-<f8>") 'flyspell-check-next-word)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'spellcheck)
;;; spellcheck.el ends here
