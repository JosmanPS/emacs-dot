;;; path.el --- Path setup

;;; Commentary:

;;; Code:

(getenv "PATH")

(setenv "PATH"
        (concat
         "/usr/texbin" ":"
         (getenv "PATH")))

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(provide 'path)
;;; path.el ends here
