;;; otrenav-terminal.el --- Terminal configuration

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("/.zsh$" . shell-script-mode))

(if (eq system-type 'darwin)
    (progn
      (setq multi-term-program "/usr/local//bin/bash")
      (setq explicit-shell-file-name "/usr/local/bin/bash"))
  (setq multi-term-program "/bin/bash")
  (setq explicit-shell-file-name "/bin/bash"))

;;
;; Multi term
;;
(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "The key list that will need to be unbind."
  :type 'list
  :group 'multi-term)

(defcustom term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-raw)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("M-r" . term-send-reverse-search-history)
    ("M-," . term-send-input)
    ("M-." . comint-dynamic-complete))
  "Custom set keybindings."
  :type 'alist
  :group 'multi-term)

(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;;
;; Ansi-term
;;
(defadvice ansi-term (after advise-ansi-term-coding-system)
(set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(defun otrenav-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'otrenav-term-exec-hook)

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

(provide 'otrenav-terminal)
;;; otrenav-terminal.el ends here
