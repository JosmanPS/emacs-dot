;;; python.el --- Python configuration
;;
;;; Commentary:
;;
;; Useful functions:
;; - M-.  anaconda-mode-goto-definitions
;; - M-*  anaconda-nav-pop-marker
;; - M-?  anaconda-mode-view-doc
;; - M-r  anaconda-mode-usages
;; 
;; Doable things:
;; - Autocomplete with "TAB"
;; - Change autocomplete with "M-x elpy-set-backend" (rope or jedi)
;; - Snippet expansion with "C-c k"
;; - Indentation highlighting with "M-x highlight-identation-mode"
;; - Simultaneous editing with "C-c o"
;; - Moving/editing code blocks with "C-c >" and "C-c <"
;;
;; Requirements:
;; - rope        (pip install rope)
;; - jedi        (pip install jedi)
;; - flake8      (pip install flake8)
;; - importmagic (pip install importmagic)
;;
;; Resources:
;; - http://elpy.readthedocs.org/en/latest/index.html
;; - https://code.djangoproject.com/wiki/Emacs
;; - https://github.com/aculich/virtualenv.el
;; - http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/
;; - http://tkf.github.io/emacs-jedi/latest/
;;
;;; Code:

(require 'py-autopep8)

;; ElDoc mode (see the function footprint in echo area)
(add-hook 'python-mode-hook 'eldoc-mode)

;; Annotate the PDB call
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "ipdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

(add-to-list 'auto-mode-alist '("/.py$'" . python-mode))
(elpy-enable)

(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c o") 'iedit-mode)

;; Python Hook
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4))))

;; Anaconda mode (lookup documentation)
(add-hook 'python-mode-hook 'anaconda-mode)

(provide 'python)
;;; python.el ends here
