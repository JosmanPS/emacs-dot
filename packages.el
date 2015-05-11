;;
;; Automadted packages
;;

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(defvar packages '(ac-helm
                   ac-html
                   ac-html-bootstrap
                   ac-html-csswatcher
                   ac-js2
                   ac-math
                   ace-jump-mode
                   ace-window
                   async
                   auctex
                   auctex-latexmk
                   auto-complete
                   auto-complete-auctex
                   auto-yasnippet
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
                   epc
                   ess
                   ess-R-data-view
                   ess-R-object-popup
                   ess-smart-equals
                   expand-region
                   flymake-python-pyflakes
                   flyspell-lazy
                   git-gutter-fringe
                   helm
                   helm-R
                   helm-bind-key
                   helm-chrome
                   helm-css-scss
                   helm-dictionary
                   helm-dired-recent-dirs
                   helm-dirset
                   helm-flymake
                   helm-flyspell
                   helm-git
                   helm-helm-commands
                   ibuffer
                   ido-vertical-mode
                   jedi
                   jedi-direx
                   latex-pretty-symbols
                   markdown-mode
                   matlab-mode
                   nodejs-repl
                   nose
                   org
                   pony-mode
                   popup
                   powerline
                   projectile
                   pungi
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
                   skewer-mode
                   smex
                   solarized-theme
		   theme-changer
                   virtualenvwrapper
                   web-beautify
                   web-mode
                   yasnippet))

(dolist (package packages)
  (when (not (package-installed-p package))
    (package-install package)))


