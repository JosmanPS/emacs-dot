;;; spellcheck.el --- Spell check configuration

;;; Commentary:

;;; Code:

(require 'flyspell-lazy)
(flyspell-lazy-mode t)
(setq flyspell-issue-welcome-flag nil)

(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))

(setq-default ispell-list-command "list")

;; TODO: (otn) Add other programming languages

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

(provide 'spellcheck)
;;; spellcheck.el ends here
