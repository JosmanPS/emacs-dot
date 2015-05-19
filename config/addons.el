;;; addons.el --- Various small configurations

;;; Commentary:

;;; Code:

;; Powerline
(require 'powerline)
(powerline-default-theme)
(powerline-reset)

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
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-o w") 'ace-window)

;; Bookmarks
(require 'bookmark+)

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; Others
(require 'autopair)
(require 'diff-hl)
(require 'editorconfig)
(require 'git-gutter-fringe)
(require 'neotree)

(provide 'addons)
;;; addons.el ends here

