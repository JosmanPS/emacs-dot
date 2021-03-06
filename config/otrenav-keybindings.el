;;; otrenav-keybindings.el --- Keybindings configuration

;;; Commentary:

;;; Code:

;; repeat last command      C-x z
;; describe-bindings        C-h b
;; describe-mode            C-h m
;; describe-key             C-h k
;; kill-rectangle           C-x r k
;; yank-rectangle           C-x r y
;; insert-rectangle         C-x r t
;; delete-rectangle         C-x r d
;; open-rectangle           C-x r o
;; number-to-register       C-x r N
;; transpose-lines          C-x C-t
;; delete-indentation       M-^
;; kill-word                M-d
;; backward-kill-word       M-<DEL>
;; backward-kill-line       C-0 C-k
;; exchange-point-and-mark  C-x C-x
;; kill-region              C-w
;; downcase-region          C-x C-l
;; upcase-region            C-x C-u
;; M-x ediff: compare two files
;; M-x dired: file manager
;; C-x C-q: rename files (C-x C-s)
;; M-x gnus: news, email and RSS
;; M-g g: go-to-line
;; C-x C-b: list buffers
;; C-x 2: split vertically
;; C-x 3: split horizontally
;; C-x 0: delete window
;; C-x 1: delete other windows
;; C-x o: switch window
;; C-M-v: scroll other window
;; C-u NUMBER COMMAND: numeric arguments

;; Help:
;; C-h: help prefix
;; C-h k: documentation for given key
;; C-h f: documentation for command
;; C-h a: search for commands by keywords or regexp
;; C-h m: describes major and minor modes

;; C-s: incremental search forward
;; C-r: incremental search backward
;; C-u C-SPC: mark ring
;; C-x h: select all
;; M-h: select current paragraph
;; C-y: yank
;; M-y: use a previous yank
;; C-x n n: narrow buffer to current region
;; C-x n w: restore ("widen") buffer
;; C-w: cut
;; M-w: copy
;; M-d: kill next word
;; M-k: kill to end of sentence
;; C-/: undo
;; C-_: undo
;; C-x u: undo
;; C-s C-s:search for most recently searched item
;; C-s M-p: previous item in search history
;; C-s M-n: next item in search history
;; C-r: backward incremental search
;; M-%: query replace
;; C-M-s: regular expression incremental search
;; M-x regexp-builder: try out regex expressions
;; F3: start recording macro
;; F4: Stop recording macro
;; F4: Play back macro once
;; M-5 F4: Play back macro 5 times
;; M-0 F4: Play back macro over and over until failure
;; M-x shell
;; M-x compile: invoke make
;; M-x gdb
;; M-x grep
;; M-x man
;; M-x calendar
;; C-x C-f /sudo::/etc/file : Edit a file with sudo
;; M-p and M-n: previous text in prompt
;; C-g: cancel an operation

;; Pending topics:
;; - Regular expression search and replacement
;; - Version control

;; Settings:
;; M-x auto-fill-mode: wrapping
;; M-x flyspell-mode: spelling
;; M-x follow-mode: two sided document
;; M-x icomplete-mode: show completions as you type
;; M-x iswitchb-mode: Show all buffer names when switching

;; Region
;; M-@ mark-word
;; M-h mark-paragraph
;; C-M-@ mark-sexp
;; C-M-h mark-defun
;; C-x C-p mark-page
;; C-x h mark-whole-buffer
;; C-x C-x exchange-point-and-mark
;; C-w kill-region
;; M-w kill-ring-save
;; C-x C-i indent-rigidly
;; C-x C-l downcase-region
;; C-x C-u upcase-region
;; M-x fill-region

;; Remember this stuff
;; Undo in regions!
;; 

;; Replace bunch of text:
;; 1. Set point
;; 2. Place to form a rectangle
;; 3. C-x r t [STRING]

;; Transpose: C-t
;; Transpose line: C-x C-t
;; Transpose word: M-t

;; Move to closing parenthesis: C-M-n
;; Move to opening parenthesis: C-M-p

;; TODO: Add expand-region
;; TODO: Keychords

;; Pop the mark: C-u C-SPC
;; Searc-backward: C-s C-r

;; General
(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "s-g") 'goto-line)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "s-r") 'query-replace)
(global-set-key (kbd "s-i") 'browse-url-at-point)
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-h s-f") 'find-function)
(global-set-key (kbd "C-h s-t") 'occur)
(global-set-key (kbd "S-s-a") 'align-regexp)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-o C-q") 'save-buffers-kill-terminal)

;; TODO: Set record kmacro at F1
;; TODO: Repeat last kmacro at F2

(provide 'otrenav-keybindings)
;;; otrenav-keybindings.el ends here
