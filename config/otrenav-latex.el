;;; otrenav-latex.el --- Latex configuration

;;; Commentary:

;; C-c C-c: compile
;; C-c C-c: to view

;; Resources:
;; 1. http://piotrkazmierczak.com/2010/emacs-as-the-ultimate-latex-editor/
;; 2. http://tex.stackexchange.com/questions/50827/
;;           a-simpletons-guide-to-tex-workflow-with-emacs
;; 3. http://www.emacswiki.org/emacs/LaTeXPreviewPane

;;; Code:

(add-to-list 'auto-mode-alist '("/.tex$'" . latex-mode))

(require 'latex-pretty-symbols)
(require 'auctex-latexmk)

(add-hook 'latex-mode-hook 'flyspell-buffer)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'imenu-add-menubar-index)
(add-hook 'latex-mode-hook 'latex-math-mode)
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

;; (auctex-latexmk-setup)
(setq reftex-plug-into-AUCTeX t)
(setq tex-auto-save t)
(setq tex-electric-sub-and-superscript t)
(setq tex-parse-self t)
(setq tex-pdf-mode t)
(setq tex-save-query nil)
(setq-default tex-master nil)

(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o")

;; Reftex
;; TODO (otn) fix this
(require 'tex-site)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(provide 'otrenav-latex)
;;; otrenav-latex.el ends here
