;;; helm.el --- Helm Configuration

;;; Commentary:

;;; Code:

(global-set-key (kbd "C-M-S-s") 'helm-resume)
(global-set-key (kbd "C-M-s") 'instant-search-using-helm)
(global-set-key (kbd "C-M-g") 'instant-rgrep-using-helm)

(provide 'helm)
;;; helm.el ends here
