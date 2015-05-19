;;; javascript.el --- JavaScript configuration

;;; Commentary:

;;; Code:

(setq js-indent-level 4)
(defun otrenav-js-custom ()
  "JavaScript mode hook."
  (setq js-indent-level 4))
(add-hook 'js-mode-hook 'otrenav-js-custom)
(add-hook 'web-mode-hook 'otrenav-js-custom)

(provide 'javascript)
;;; javascript.el ends here
