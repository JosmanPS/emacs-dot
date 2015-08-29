;;; otrenav-addons.el --- Various small configurations
;;
;;; Commentary:
;;
;; Dependencies:
;; - Ag, The Silver Seacher (yum install the_silver_searcher)
;;
;; Useful functions:
;; - C-c o   Open other window
;; - C-l     Search in parent directory
;; - C-c C-e Switch to edit mode
;; - C-x C-s Save ag results to buffer
;; - C-c ?   Show help message
;; - C-c C-c Commit changes
;; - C-c C-k Abort
;;
;; Resources:
;; - https://github.com/magnars/multiple-cursors.el
;; - http://www.emacswiki.org/emacs/TabBarMode
;; - http://capitaomorte.github.io/yasnippet/
;; = https://emacs-helm.github.io/helm/
;;
;;; Code:

;; Tabbar
(require 'tabbar)
(tabbar-mode 1)

;; Powerline
(require 'powerline)
(powerline-default-theme)
(powerline-reset)

(setq powerline-arrow-shape 'curve)

;; Ido
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)

(global-set-key (kbd "s-o") 'ido-find-file)

;; Smex
(require 'smex)
(smex-initialize)
(setq smex-save-file
      (expand-file-name "misc/smex-items" user-emacs-directory))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Guide-key
;; TODO: (otn) adjust this list
(require 'guide-key)
(setq guide-key/guide-key-sequence
      '("C-x r" "C-x 4"
        (org-mode "C-c C-x")
        (outline-minor-mode "C-c @")))
(guide-key-mode 1)

;; Anzu
(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; Dired
(require 'dired+)
(require 'dired-x)
(setq-default dired-omit-files-p t)

;; Acejump
(require 'ace-jump-mode)
(global-set-key (kbd "C-o SPC") 'ace-jump-mode)
(global-set-key (kbd "C-o w") 'ace-window)

;; Bookmarks
(require 'bookmark+)

;; Yasnippet
;; Should be loaded before auto-complete
;; for them to work together correctly.
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/config/snippets"))
;; (yas-global-mode 1)

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")

;; Others
(require 'autopair)
(require 'diff-hl)
(require 'editorconfig)

(if (display-graphic-p)
    (require 'git-gutter-fringe))

;; Neotree
(require 'neotree)
(setq neo-theme 'ascii)

;; Outline-mode
(defun turn-on-outline-minor-mode () (outline-minor-mode 1))

;; Multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Helm with Ag and Projectile
(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

;; Key-chord
(require 'key-chord)
(key-chord-mode 1)
;; (key-chord-define-global "||" 'undo)

(provide 'otrenav-addons)
;;; otrenav-addons.el ends here

