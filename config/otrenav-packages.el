;;; packages.el --- List of packages to be installed

;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(defvar packages '(ac-anaconda
                   ac-emmet
                   ac-helm
                   ac-html
                   ac-html-bootstrap
                   ac-html-csswatcher
                   ac-js2
                   ac-math
                   ace-jump-mode
                   ace-window
                   anaconda-mode
                   anzu
                   auctex
                   auctex-latexmk
                   auto-complete
                   auto-complete-auctex
                   autopair
                   bookmark+
                   diff-hl
                   dired+
                   dired-details+
                   dired-imenu
                   dired-rainbow
                   dired-subtree
                   direx
                   django-mode
                   django-snippets
                   editorconfig
                   elpy
                   emmet-mode
                   epc
                   ess
                   ess-R-data-view
                   ess-R-object-popup
                   expand-region
                   flycheck
                   flymake-python-pyflakes
                   flyspell-lazy
                   git-gutter-fringe
                   guide-key
                   helm
                   helm-ag
                   helm-R
                   helm-bind-key
                   helm-css-scss
                   helm-dired-recent-dirs
                   helm-dirset
                   helm-emmet
                   helm-flymake
                   helm-flyspell
                   helm-git
                   helm-helm-commands
                   ibuffer
                   ido-vertical-mode
                   jedi
                   jedi-direx
                   key-chord
                   latex-pretty-symbols
                   markdown-mode
                   matlab-mode
                   mc-extras
                   multiple-cursors
                   nodejs-repl
                   nose
                   neotree
                   pony-mode
                   popup
                   powerline
                   pungi
                   py-autopep8
                   py-import-check
                   py-isort
                   pydoc-info
                   pyenv-mode
                   pyimpsort
                   pylint
                   python-django
                   python-environment
                   python-info
                   python-mode
                   pyvenv
                   r-autoyas
                   rainbow-mode
                   smex
                   solarized-theme
                   tabbar
                   virtualenvwrapper
                   web-beautify
                   web-mode
                   yasnippet))

(dolist (package packages)
  (when (not (package-installed-p package))
    (package-install package)))


(provide 'packages)
;;; packages.el ends here
