;;; init.el --- Omar Trejo's Emacs Setup
;;
;;; Commentary:
;;
;; Any code product of myself is released
;; under the MIT license. For any code created
;; by other people, refer to their respective
;; release notes.
;;
;; May, 2015

;;; Code:

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(load "~/.emacs.d/config/josman-path.el")
(load "~/.emacs.d/config/josman-packages.el")
(load "~/.emacs.d/config/josman-settings.el")
;; (load "~/.emacs.d/config/otrenav-functions.el")
;; (load "~/.emacs.d/config/otrenav-keybindings.el")
;; (load "~/.emacs.d/config/otrenav-spellcheck.el")
(load "~/.emacs.d/config/otrenav-addons.el")
;; (load "~/.emacs.d/config/otrenav-plain-text.el")
;; (load "~/.emacs.d/config/otrenav-web.el")
;; (load "~/.emacs.d/config/otrenav-javascript.el")
;; (load "~/.emacs.d/config/otrenav-terminal.el")
;; (load "~/.emacs.d/config/otrenav-latex.el")
;; (load "~/.emacs.d/config/otrenav-helm.el")
;; (load "~/.emacs.d/config/otrenav-magit.el")
(load "~/.emacs.d/config/otrenav-matlab.el")
;; (load "~/.emacs.d/config/otrenav-lisp.el")
(load "~/.emacs.d/config/otrenav-python.el")
;; (load "~/.emacs.d/config/otrenav-ess.el")
;; (load "~/.emacs.d/config/otrenav-windows.el")

(provide 'init)
;;; init.el ends here
