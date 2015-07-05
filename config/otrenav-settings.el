;;; otrenav-settings.el --- General settings configuration

;;; Commentary:

;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p)

(autopair-global-mode t)

(delete-selection-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)
(global-linum-mode 0)

(pending-delete-mode t)
(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq echo-keystrokes 0.1)
(setq indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq tab-width 4)
(setq use-dialog-box nil)
(setq visible-bell t)
(setq x-select-enable-clipboard t)
(setq menu-bar-mode -1)

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; TODO: (otn) fix this
(set-default 'global-visual-line-mode nil)
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows t)

(show-paren-mode t)
(toggle-diredp-find-file-reuse-dir 1)
(tool-bar-mode -1)
(transient-mark-mode t)

;; Theme
;;(load-theme 'solarized-dark t)
(load-theme 'material-design t)

;; Whitespace
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
;; (setq-default show-trailing-whitespace t)

;; Mouse scrolling
(setq auto-window-vscroll nil)
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-progressive-speed nil)
(scroll-bar-mode -1)

(if (display-graphic-p)
    (scroll-bar-mode -1)
  (progn
    (require 'mouse)
    (xterm-mouse-mode 1)
    (global-set-key [mouse-4] '(lambda ()
                                 (interactive)
                                 (scroll-down 1)))
    (global-set-key [mouse-5] '(lambda ()
                                 (interactive)
                                 (scroll-up 1)))
    (defun track-mouse(e))
    (setq mouse-sel-mode 1)))

;; Font
(set-face-attribute 'default nil
                    :font "Droid Sans Mono-12")

;; Backups
(defvar backup-dir (expand-file-name "~/.emacs.d/backups/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/backups/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq auto-save-list-file-prefix autosave-dir)

;;
;; Dired
;;
;; Auto refresh buffers
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(provide 'otrenav-settings)
;;; otrenav-settings.el ends here
