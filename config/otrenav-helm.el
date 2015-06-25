;;; otrenav-helm.el --- Helm Configuration
;;
;;; Commentary:
;;
;;; Resources:
;; - http://tuhdo.github.io/helm-intro.html
;;
;;; Code:

(require 'helm)
(require 'helm-config)

;;
;; Keybindings
;;
(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "M-x"))

(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "<f2>")                         'helm-execute-kmacro)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-M-S-s")                      'helm-resume)
(global-set-key (kbd "C-M-g")                        'instant-rgrep-using-helm)
(global-set-key (kbd "C-M-s")                        'instant-search-using-helm)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key (kbd "C-c f")                        'helm-recentf)
(global-set-key (kbd "C-c g")                        'helm-gid)
(global-set-key (kbd "C-c h")                        'helm-command-prefix)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "C-h d")                        'helm-info-at-point)
(global-set-key (kbd "C-h r")                        'helm-info-emacs)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-x b")                        'helm-mini)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)

(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-buffers-list)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)

;; (define-key helm-buffer-map (kbd "C-d")              'helm-buffer-run-kill-persistent)
;; (define-key helm-find-files-map (kbd "C-d")          'helm-ff-persistent-delete)

(define-key helm-map (kbd "<tab>")                   'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i")                     'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")                     'helm-select-action)

(define-key helm-command-map (kbd "#")               'helm-emms)
(define-key helm-command-map (kbd "I")               'helm-imenu-in-all-buffers)
(define-key helm-command-map (kbd "g")               'helm-apt)
(define-key helm-command-map (kbd "w")               'helm-psession)

;; Describe key-bindings
;; (helm-descbinds-install)  ; C-h b, C-x C-h

;;
;; Helm variables
;;
(setq helm-M-x-fuzzy-match                       t)
(setq helm-always-two-windows                    t)
(setq helm-apropos-fuzzy-match                   t)
(setq helm-autoresize-max-height                 80)
(setq helm-autoresize-min-height                 20)
(setq helm-boring-file-regexp-list               '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$"))
(setq helm-buffer-skip-remote-checking           t)
(setq helm-buffers-fuzzy-matching                t)
(setq helm-completion-in-region-fuzzy-match      t)
(setq helm-default-external-file-browser         "nautilus")
;; (setq helm-default-zgrep-command                 "zgrep --color=always -a -n%cH -e %p %f")
(setq helm-echo-input-in-header-line             t)
(setq helm-ff-auto-update-initial-value          t)
(setq helm-ff-file-name-history-use-recentf      t)
(setq helm-ff-search-library-in-sexp             t)
(setq helm-google-suggest-use-curl-p             t)
;; (setq helm-grep-default-command                  "ack-grep -Hn --smart-case --no-group %e %p %f")
;; (setq helm-grep-default-recurse-command          "ack-grep -H --smart-case --no-group %e %p %f")
;; (setq helm-idle-delay                            0.01)
;; (setq helm-input-idle-delay                      0.01)
(setq helm-lisp-fuzzy-completion                 t)
;; (setq helm-locate-command                        "locate %s -e -A --regex %s")
;; (setq helm-ls-git-grep-command                   "git grep -n%cH --color=always --exclude-standard --no-index --full-name -e %p %f")
;; (setq helm-ls-git-status-command                 'magit-status)
;; (setq helm-pdfgrep-default-read-command          "evince --page-label=%p '%f'")
(setq helm-recentf-fuzzy-match                   t)
(setq helm-reuse-last-window-split-state         t)
(setq helm-scroll-amount                         4)
(setq helm-split-window-in-side-p                t)
(setq ido-use-virtual-buffers                    t)

;;
;; Initialize
;;
(helm-mode t)
(helm-autoresize-mode t)
(helm-adaptive-mode t)
(helm-push-mark-mode t)

;; Use default-as-input in grep
(add-to-list 'helm-sources-using-default-as-input 'helm-source-grep)

;; Ignored extensions
(add-to-list 'completion-ignored-extensions ".gvfs/")

;;
;; Toggle grep program
;;
(defun eselect-grep ()
  (interactive)
  (when (y-or-n-p (format "Current grep program is %s, switching? "
                          (helm-grep-command)))
    (if (helm-grep-use-ack-p)
        (setq helm-grep-default-command
              "grep --color=always -d skip %e -n%cH -e %p %f"
              helm-grep-default-recurse-command
              "grep --color=always -d recurse %e -n%cH -e %p %f")
        (setq helm-grep-default-command
              "ack-grep -Hn --smart-case --no-group %e %p %f"
              helm-grep-default-recurse-command
              "ack-grep -H --smart-case --no-group %e %p %f"))
    (message "Switched to %s" (helm-grep-command))))

;;
;; Add Magit to `helm-source-ls-git'
;;
;; (defmethod helm-setup-user-source ((source helm-ls-git-source))
;;   (let ((actions (oref source :action)))
;;     (set-slot-value
;;      source
;;      'action
;;      (helm-append-at-nth
;;       actions
;;       (helm-make-actions
;;        "Magit status"
;;        (lambda (_candidate)
;;          (magit-status (helm-default-directory))))
;;       1))))

;; (defmethod helm-setup-user-source ((source helm-source-buffers))
;;   (set-slot-value source 'candidate-number-limit 200))

;; (defmethod helm-setup-user-source :after ((source helm-source-buffers))
;;   (let ((actions (oref source :action))
;;         (name (oref source :name)))
;;     (when (string= name "Buffers in project")
;;       (set-slot-value
;;        source
;;        'action
;;        (helm-append-at-nth
;;         actions
;;         (helm-make-actions
;;          "Magit status"
;;          (lambda (_candidate)
;;            (magit-status (helm-default-directory))))
;;         1)))))

;;
;; Debugging
;;
;; (defun helm-debug-toggle ()
;;   (interactive)
;;   (setq helm-debug (not helm-debug))
;;   (message "Helm Debug is now %s"
;;            (if helm-debug "Enabled" "Disabled")))

;; (defun helm-ff-candidates-lisp-p (candidate)
;;   (cl-loop for cand in (helm-marked-candidates)
;;            always (string-match "\.el$" cand)))

(provide 'otrenav-helm)
;;; otrenav-helm.el ends here
