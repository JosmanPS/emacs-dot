;;; functions.el --- Various functions

;;; Commentary:

;;; Code:

(global-set-key (kbd "C-o") nil)
(global-set-key (kbd "C-o e") 'eval-region)

(defun otrenav-bol-and-inl ()
  "Insert line at point."
  (interactive)
  (beginning-of-visual-line)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command))

(global-set-key (kbd "<C-S-return>") 'otrenav-bol-and-inl)

(defun otrenav-eof-and-inl ()
  "Insert new line after current one."
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'otrenav-eof-and-inl)

;; Expand selection
;; https://github.com/magnars/expand-region.el
(require 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)

;;
;; Navigate brackets and quotation marks
;;
(global-set-key (kbd "<C-M-left>") nil)
(global-set-key (kbd "<C-M-right>") nil)

(defvar otrenav-left-brackets nil "list of open bracket chars.")
(setq otrenav-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "'" "\"" "`" "“" "‘" "‹" "«"))

(defvar otrenav-right-brackets nil "list of close bracket chars.")
(setq otrenav-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "'" "\"" "`" "”" "’" "›" "»"))

(defun otrenav-backward-left-bracket ()
  (interactive)
  (search-backward-regexp (eval-when-compile (regexp-opt otrenav-left-brackets)) nil t))

(global-set-key (kbd "<C-M-left>") 'otrenav-backward-left-bracket)

(defun otrenav-forward-right-bracket ()
  (interactive)
  (search-forward-regexp (eval-when-compile (regexp-opt otrenav-right-brackets)) nil t))

(global-set-key (kbd "<C-M-right>") 'otrenav-forward-right-bracket)

;;
;; Search sites
;;
(defun otrenav-lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
   If there is a text selection (a phrase), use that.
   This command switches you to your browser."
  (interactive)
  (let (my-word my-url)
    (setq my-word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))

    (setq my-word (replace-regexp-in-string " " "_" my-word))
    (setq my-url (concat "http://en.wikipedia.org/wiki/" my-word))
    (browse-url my-url)))

(defun otrenav-lookup-google ()
  "Look up the word under cursor in Google.
   If there is a text selection (a phrase), use that.
   This command switches you to your browser."
  (interactive)
  (let (my-word my-url)
    (setq my-word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))

    (setq my-word (replace-regexp-in-string " " "_" my-word))
    (setq my-url (concat "http://www.google.com/search?q=" my-word))
    (browse-url my-url)))

(defun otrenav-lookup-wolfram-alpha ()
  "Look up the word under cursor in Wolfram Alpha.
   If there is a text selection (a phrase), use that.
   This command switches you to your browser."
  (interactive)
  (let (my-word my-url)
    (setq my-word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))

    (setq my-word (replace-regexp-in-string " " "_" my-word))
    (setq my-url (concat "http://www.wolframalpha.com/input/?i=" my-word))
    (browse-url my-url)))

(defun otrenav-lookup-word-definition ()
  "Look up the current word's definition in a browser.
   If a region is active (a phrase), lookup that phrase."
  (interactive)
  (let (my-word my-url)
    (setq my-word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))
    (setq my-word (replace-regexp-in-string " " "%20" my-word))
    (setq my-url (concat "http://www.answers.com/main/ntquery?s=" my-word))
    (browse-url my-url)))

;; Search and replace
(defun otrenav-uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun otrenav-uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (otrenav-uniquify-all-lines-region (point-min) (point-max)))

(defun otrenav-delete-file-and-buffer ()
  "Deletes the current file and buffer (assumes file exists)."
  (interactive)
  (delete-file buffer-file-name)
  (kill-buffer (buffer-name)))

(defun otrenav-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun otrenav-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun otrenav-cleanup-buffer ()
  "Do things the right way. ;)"
  (interactive)
  (whitespace-cleanup)
  (otrenav-untabify-buffer)
  (otrenav-indent-buffer))

(global-set-key (kbd "C-o c") 'otrenav-cleanup-buffer)

(defun otrenav-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
   If there's no region, the current line will be duplicated.
   If there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-d") nil)
(global-set-key (kbd "C-d") 'otrenav-duplicate-current-line-or-region)

(defun otrenav-open-with ()
  "Open current file with external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(global-set-key (kbd "C-o o") 'otrenav-open-with)

;; Switch theme - unused for now
;; (setq current-theme 'solarized-light)
;; (load-theme current-theme t)
;; (defun otrenav-switch-theme ()
;;   "Switch between light and dark solarized themes."
;;   (interactive)
;;   (if (eq current-theme 'solarized-light)
;;       (setq current-theme 'solarized-dark)
;;     (setq current-theme 'solarized-light))
;;   (load-theme current-theme t)
;;   (powerline-reset))
;; (global-set-key (kbd "C-o t") 'otrenav-switch-theme)

(provide 'functions)
;;; functions.el ends here
