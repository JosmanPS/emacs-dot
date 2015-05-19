;;; material-design-theme.el -- A light material design theme

;; Author: Omar Trejo
;; URL:
;; Version: 0.1

;; MIT License

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(deftheme material-design "A light material design theme")

(defvar material-design-colors-alist
  '(("grey-one"   . "#edf3f7")
    ("grey-two"   . "#d0d5d9")
    ("grey-three" . "#9fa3a6")
    ("grey-four"  . "#6e7173")
    ("grey-five"  . "#565859")
    ("grey-six"   . "#3d3f40")

    ("cyan-one"   . "#00bcd4")
    ("cyan-two"   . "#0097a7")

    ("green-one"  . "#4caf50")
    ("green-two"  . "#388e3c")

    ("blue-one"   . "#2196f3")
    ("blue-two"   . "#1976d2")

    ("purple-one" . "#9c27b0")
    ("purple-two" . "#7b1fa2")

    ("yellow-one" . "#ffc107")
    ("yellow-two" . "#ffa000")

    ("orange-one" . "#ff9800")
    ("orange-two" . "#f57c00")

    ("red-one"    . "#f44336")
    ("red-two"    . "#d32f2f"))
  "Light material design colors.")

(defmacro material-design/with-color-variables (&rest body)
  "`let' bind all colors defined in `material-design-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   material-design-colors-alist))
     ,@body))

(material-design/with-color-variables
 (custom-theme-set-faces
  'material-design

  ;; Basic coloring

  '(button ((t (:underline t))))
  `(link ((t (:foreground ,blue-one :underline t :weight bold))))
  `(link-visited ((t (:foreground ,purple-one :underline t :weight normal))))
  `(default ((t (:foreground ,grey-six :background ,grey-one))))
  `(cursor ((t (:foreground ,grey-six :background ,grey-five))))
  `(escape-glyph ((t (:foreground ,yellow-one :bold t))))
  `(fringe ((t (:foreground ,grey-five :background ,grey-two))))
  `(header-line ((t (:foreground ,cyan-two
                                 :background ,grey-five
                                 :box (:line-width -1 :style released-button)))))
  `(highlight ((t (:background ,grey-two))))
  `(success ((t (:foreground ,green-two :weight bold))))
  `(warning ((t (:foreground ,orange-two :weight bold))))

  ;; Compilation

  `(compilation-column-face ((t (:foreground ,yellow-two))))
  `(compilation-enter-directory-face ((t (:foreground ,cyan-two))))
  `(compilation-error-face ((t (:foreground ,red-two :weight bold :underline t))))
  `(compilation-face ((t (:foreground ,grey-six))))
  `(compilation-info-face ((t (:foreground ,blue-one))))
  `(compilation-info ((t (:foreground ,green-two :underline t))))
  `(compilation-leave-directory-face ((t (:foreground ,purple-two))))
  `(compilation-line-face ((t (:foreground ,yellow-one))))
  `(compilation-line-number ((t (:foreground ,yellow-one))))
  `(compilation-message-face ((t (:foreground ,grey-five))))
  `(compilation-warning-face ((t (:foreground ,orange-two :weight bold :underline t))))
  `(compilation-mode-line-exit ((t (:foreground ,cyan-one :weight bold))))
  `(compilation-mode-line-fail ((t (:foreground ,red-two :weight bold))))
  `(compilation-mode-line-run ((t (:foreground ,yellow-two :weight bold))))

  ;; Grep

  `(grep-context-face ((t (:foreground ,grey-five))))
  `(grep-error-face ((t (:foreground ,red-two :weight bold :underline t))))
  `(grep-hit-face ((t (:foreground ,cyan-one :weight bold))))
  `(grep-match-face ((t (:foreground ,yellow-one :weight bold))))
  `(match ((t (:background ,cyan-one :foreground ,grey-five))))

  ;; isearch

  `(isearch ((t (:foreground ,grey-one :weight bold :background ,red-one))))
  `(isearch-fail ((t (:foreground ,yellow-one :weight bold :background ,red-two))))
  `(lazy-highlight ((t (:foreground ,grey-one :weight bold :background ,blue-one))))

  `(menu ((t (:foreground ,grey-six :background ,grey-two))))
  `(minibuffer-prompt ((t (:foreground ,grey-five :weight bold))))
  `(region ((,class (:background ,blue-one :foreground ,grey-one))
            (t :inverse-video t)))
  `(secondary-selection ((t (:background ,cyan-one))))
  `(trailing-whitespace ((t (:background ,red-one))))
  `(vertical-border ((t (:foreground ,grey-two))))

  ;; Font lock

  `(font-lock-builtin-face ((t (:foreground ,cyan-two))))
  `(font-lock-comment-face ((t (:foreground ,grey-four))))
  `(font-lock-comment-delimiter-face ((t (:foreground ,grey-three))))
  `(font-lock-constant-face ((t (:foreground ,green-two))))
  `(font-lock-doc-face ((t (:foreground ,grey-four))))
  `(font-lock-function-name-face ((t (:foreground ,grey-five :weight bold))))
  `(font-lock-keyword-face ((t (:foreground ,blue-one))))
  `(font-lock-negation-char-face ((t (:foreground ,red-one :weight bold))))
  `(font-lock-preprocessor-face ((t (:foreground ,red-one :weight bold))))
  `(font-lock-regexp-grouping-construct ((t (:foreground ,yellow-two :weight bold))))
  `(font-lock-regexp-grouping-backslash ((t (:foreground ,purple-one :weight bold))))
  `(font-lock-string-face ((t (:foreground ,green-one))))
  `(font-lock-type-face ((t (:foreground ,blue-one))))
  `(font-lock-variable-name-face ((t (:foreground ,orange-two))))
  `(font-lock-warning-face ((t (:foreground ,orange-two :weight bold))))

  `(c-annotation-face ((t (:inherit font-lock-constant-face))))

  ;; Acejump, agile movement to characters in buffer

  `(ace-jump-face-background
    ((t (:foreground ,grey-three :background ,grey-two :inverse-video nil))))
  `(ace-jump-face-foreground
    ((t (:foreground ,red-one :background ,grey-one :inverse-video nil))))

  ;; Anzu, display isearch counts on mode-line

  `(anzu-mode-line ((t (:foreground ,cyan-one :weight bold))))

  ;; Auto-complete

  `(ac-candidate-face ((t (:background ,grey-three :foreground ,grey-six))))
  `(ac-selection-face ((t (:background ,grey-four :foreground ,grey-six))))
  `(popup-tip-face ((t (:background ,blue-one :foreground ,grey-one))))
  `(popup-scroll-bar-foreground-face ((t (:background ,grey-four))))
  `(popup-scroll-bar-background-face ((t (:background ,grey-two))))
  `(popup-isearch-match ((t (:background ,grey-one :foreground ,grey-six))))

  ;; TODO: bm, visible bookmarks

  ;; `(bm-face ((t (:background ,zenburn-yellow-1 :foreground ,zenburn-bg))))
  ;; `(bm-fringe-face ((t (:background ,zenburn-yellow-1 :foreground ,zenburn-bg))))
  ;; `(bm-fringe-persistent-face ((t (:background ,zenburn-green-1 :foreground ,zenburn-bg))))
  ;; `(bm-persistent-face ((t (:background ,zenburn-green-1 :foreground ,zenburn-bg))))

  ;; Clojure test-mode

  `(clojure-test-failure-face ((t (:foreground ,orange-one :weight bold :underline t))))
  `(clojure-test-error-face ((t (:foreground ,red-one :weight bold :underline t))))
  `(clojure-test-success-face ((t (:foreground ,green-one :weight bold :underline t))))

  ;; Diff

  `(diff-added ((,class (:foreground ,green-two :background ,grey-one))
                (t (:foreground ,green-two :background ,grey-one))))
  `(diff-changed ((t (:foreground ,orange-one))))
  `(diff-context ((t (:foreground ,grey-three))))
  `(diff-removed ((,class (:foreground ,red-one :background ,grey-one))
                  (t (:foreground ,red-one :background ,grey-one))))
  `(diff-refine-added ((t :inherit diff-added :background ,green-one :weight bold)))
  `(diff-refine-change ((t :inherit diff-changed :weight bold)))
  `(diff-refine-removed ((t :inherit diff-removed :background ,red-two :weight bold)))
  `(diff-header ((,class (:foreground ,grey-six :weight bold))
                 (t (:foreground ,grey-six :weight bold))))
  `(diff-file-header
    ((,class (:foreground ,grey-six :weight bold))
     (t (:foreground ,grey-six :weight bold))))
  `(diff-hunk-header
    ((,class (:foreground ,purple-two :weight bold))
     (t (:foreground ,purple-two :weight bold))))

  ;; Diff-hl

  `(diff-hl-insert ((t (:foreground ,cyan-two :background ,green-two))))
  `(diff-hl-delete ((t (:foreground ,red-two :background ,red-one))))
  `(diff-hl-change ((t (:foreground ,orange-two :background ,orange-one))))

  ;; Dired, Dired+, Dired-subtree

  `(diredp-display-msg ((t (:foreground ,blue-two))))
  `(diredp-compressed-file-suffix ((t (:foreground ,purple-one))))
  `(diredp-date-time ((t (:foreground ,orange-one))))
  `(diredp-deletion ((t (:foreground ,red-one))))
  `(diredp-deletion-file-name ((t (:foreground ,red-one))))
  `(diredp-dir-heading ((t (:foreground ,blue-one :background ,grey-two :weight bold))))
  `(diredp-dir-priv ((t (:foreground ,blue-one))))
  `(diredp-exec-priv ((t (:foreground ,orange-one))))
  `(diredp-executable-tag ((t (:foreground ,orange-two))))
  `(diredp-file-name ((t (:foreground ,grey-six))))
  `(diredp-file-suffix ((t (:foreground ,purple-two))))
  `(diredp-flag-mark ((t (:foreground ,yellow-one))))
  `(diredp-flag-mark-line ((t (:foreground ,yellow-two))))
  `(diredp-ignored-file-name ((t (:foreground ,grey-three))))
  `(diredp-link-priv ((t (:foreground ,purple-one))))
  `(diredp-mode-line-flagged ((t (:foreground ,yellow-one))))
  `(diredp-mode-line-marked ((t (:foreground ,yellow-two))))
  `(diredp-no-priv ((t (:foreground ,grey-six))))
  `(diredp-number ((t (:foreground ,blue-two))))
  `(diredp-other-priv ((t (:foreground ,grey-five))))
  `(diredp-rare-priv ((t (:foreground ,orange-two))))
  `(diredp-read-priv ((t (:foreground ,cyan-two))))
  `(diredp-symlink ((t (:foreground ,grey-one :background ,purple-one))))
  `(diredp-write-priv ((t (:foreground ,orange-two))))
  `(dired-subtree-depth-1-face ((t (:background ,grey-two))))
  `(dired-subtree-depth-2-face ((t (:background ,grey-three))))
  `(dired-subtree-depth-3-face ((t (:background ,grey-four))))

  ;; Ediff

  `(ediff-current-diff-A ((t (:foreground ,grey-six :background ,red-one))))
  `(ediff-current-diff-Ancestor ((t (:foreground ,grey-six :background ,red-one))))
  `(ediff-current-diff-B ((t (:foreground ,grey-six :background ,green-one))))
  `(ediff-current-diff-C ((t (:foreground ,grey-one :background ,blue-one))))
  `(ediff-even-diff-A ((t (:background ,grey-one))))
  `(ediff-even-diff-Ancestor ((t (:background ,grey-one))))
  `(ediff-even-diff-B ((t (:background ,grey-two))))
  `(ediff-even-diff-C ((t (:background ,grey-two))))
  `(ediff-fine-diff-A ((t (:foreground ,grey-six :background ,red-one :weight bold))))
  `(ediff-fine-diff-Ancestor ((t (:foreground ,grey-six :background ,red-one weight bold))))
  `(ediff-fine-diff-B ((t (:foreground ,grey-six :background ,green-one :weight bold))))
  `(ediff-fine-diff-C ((t (:foreground ,grey-one :background ,blue-one :weight bold ))))
  `(ediff-odd-diff-A ((t (:background ,grey-two))))
  `(ediff-odd-diff-Ancestor ((t (:background ,grey-two))))
  `(ediff-odd-diff-B ((t (:background ,grey-two))))
  `(ediff-odd-diff-C ((t (:background ,grey-two))))

  ;; Eshell

  `(eshell-prompt ((t (:foreground ,red-two :weight bold))))
  `(eshell-ls-archive ((t (:foreground ,purple-one :weight bold))))
  `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
  `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
  `(eshell-ls-directory ((t (:foreground ,blue-one :weight bold))))
  `(eshell-ls-executable ((t (:foreground ,orange-two))))
  `(eshell-ls-unreadable ((t (:foreground ,grey-three))))
  `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
  `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
  `(eshell-ls-special ((t (:foreground ,yellow-one :weight bold))))
  `(eshell-ls-symlink ((t (:foreground ,grey-one :background ,purple-one))))

  ;; Flycheck, check syntax as you type

  `(flycheck-error
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,red-one) :inherit unspecified))
     (t (:foreground ,red-one :weight bold :underline t))))
  `(flycheck-warning
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,yellow-two) :inherit unspecified))
     (t (:foreground ,yellow-one :weight bold :underline t))))
  `(flycheck-info
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,purple-two) :inherit unspecified))
     (t (:foreground ,purple-one :weight bold :underline t))))
  `(flycheck-fringe-error ((t (:foreground ,red-two :weight bold))))
  `(flycheck-fringe-warning ((t (:foreground ,yellow-two :weight bold))))
  `(flycheck-fringe-info ((t (:foreground ,purple-two :weight bold))))

  ;; Flymake, check syntax as you type

  `(flymake-errline
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,red-one)
                  :inherit unspecified :foreground unspecified :background unspecified))
     (t (:foreground ,red-one :weight bold :underline t))))
  `(flymake-warnline
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,yellow-two)
                  :inherit unspecified :foreground unspecified :background unspecified))
     (t (:foreground ,yellow-two :weight bold :underline t))))
  `(flymake-infoline
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,purple-two)
                  :inherit unspecified :foreground unspecified :background unspecified))
     (t (:foreground ,purple-two :weight bold :underline t))))

  ;; Flyspell, check spelling as you type

  `(flyspell-duplicate
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,yellow-two) :inherit unspecified))
     (t (:foreground ,yellow-two :weight bold :underline t))))
  `(flyspell-incorrect
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,red-one) :inherit unspecified))
     (t (:foreground ,red-one :weight bold :underline t))))

  ;; Git gutter

  `(git-gutter:added ((t (:foreground ,green-one :weight bold))))
  `(git-gutter:deleted ((t (:foreground ,red-one :weight bold))))
  `(git-gutter:modified ((t (:foreground ,orange-one :weight bold))))
  `(git-gutter:unchanged ((t (:foreground ,grey-six :weight bold))))

  ;; Git gutter-fringe

  `(git-gutter-fr:added ((t (:foreground ,green-one  :weight bold))))
  `(git-gutter-fr:deleted ((t (:foreground ,red-one :weight bold))))
  `(git-gutter-fr:modified ((t (:foreground ,orange-one :weight bold))))

  ;; Guide-key, display available keybindings

  `(guide-key/highlight-command-face ((t (:foreground ,blue-one))))
  `(guide-key/key-face ((t (:foreground ,yellow-two))))
  `(guide-key/prefix-command-face ((t (:foreground ,purple-two))))

  ;; Helm, incremental completion and selection narrowing framework

  `(helm-action ((t (:underline nil))))
  `(helm-selection ((t (:background ,blue-one :underline nil :weight bold))))
  `(helm-candidate-number ((t (:foreground ,cyan-one))))
  `(helm-source-header ((t (:foreground ,grey-six :weight bold))))
  `(helm-visible-mark ((t (:foreground ,red-one))))
  `(helm-ff-directory ((t (:foreground ,blue-one))))
  `(helm-ff-executable ((t (:foreground ,orange-two))))
  `(helm-ff-symlink ((t (:foreground ,grey-one :background ,purple-one))))
  `(helm-ff-prefix ((t (:foreground ,red-one))))
  `(helm-M-x-key ((t (:foreground ,grey-four))))
  `(helm-grep-file ((t (:foreground ,cyan-one))))
  `(helm-grep-lineno ((t (:foreground ,grey-four))))
  `(helm-grep-running ((t (:foreground ,red-one))))
  `(helm-grep-finish ((t (:foreground ,cyan-one))))
  `(helm-buffer-saved-out ((t (:foreground ,yellow-two :background nil))))
  `(helm-moccur-buffer ((t (:foreground ,grey-four))))

  ;; Highlight symbol

  `(highlight-symbol-face ((t (:background "gray88" :underline t))))

  ;; Highlight line-mode

  `(hl-line-face ((,class (:background ,grey-two))
                  (t :weight bold)))
  `(hl-line ((,class (:background ,grey-two)) ; old emacsen
             (t :weight bold)))

  ;; Hl-sexp, highlight for s-expressions

  `(hl-sexp-face ((,class (:background ,grey-two))
                  (t :weight bold)))

  ;; ido-mode, interactively do things

  `(ido-first-match ((t (:foreground ,orange-two :weight bold))))
  `(ido-only-match ((t (:foreground ,orange-one :weight bold))))
  `(ido-subdir ((t (:foreground ,yellow-two))))
  `(ido-indicator ((t (:foreground ,yellow-one :background ,orange-two))))

  ;; Indentation

  `(indent-guide-face ((t (:foreground ,grey-three))))

  ;; JavaScript

  `(js2-warning ((t (:underline ,orange-two))))
  `(js2-error ((t (:foreground ,red-one :weight bold))))
  `(js2-jsdoc-tag ((t (:foreground ,purple-one))))
  `(js2-jsdoc-type ((t (:foreground ,purple-two))))
  `(js2-jsdoc-value ((t (:foreground ,blue-one))))
  `(js2-function-param ((t (:foreground, grey-five))))
  `(js2-external-variable ((t (:foreground ,orange-two))))

  ;; Linum-mode, line numbers on fringe

  `(linum ((t (:foreground ,grey-three :background ,grey-two))))

  ;; Magit, git interface

  `(magit-header ((t (:foreground ,grey-six
                                  :background nil
                                  :weight bold))))
  `(magit-section-title ((t (:foreground ,grey-six
                                         :background nil
                                         :weight bold))))
  `(magit-branch ((t (:foreground ,grey-six
                                  :background ,cyan-one
                                  :weight bold
                                  :box (:line-width 1 :color ,cyan-two)))))
  `(magit-item-highlight ((t (:background ,grey-two))))
  `(magit-log-author ((t (:foreground ,blue-two))))
  `(magit-log-sha1 ((t (:foreground ,orange-one :weight bold))))
  `(magit-tag ((t (:foreground ,purple-two :weight bold))))
  `(magit-log-head-label-head ((t (:foreground ,grey-six
                                               :background ,cyan-one
                                               :weight bold
                                               :box (:line-width 1 :color ,cyan-two)))))
  `(magit-log-head-label-local ((t (:foreground ,grey-six
                                                :background ,cyan-one
                                                :weight bold
                                                :box (:line-width 1 :color ,cyan-two)))))
  `(magit-log-head-label-default ((t (:foreground ,grey-six
                                                  :background ,cyan-one
                                                  :weight bold
                                                  :box (:line-width 1 :color ,cyan-two)))))
  `(magit-log-head-label-remote ((t (:foreground ,grey-one
                                                 :background ,blue-one
                                                 :weight bold
                                                 :box (:line-width 1 :color ,yellow-two)))))
  `(magit-log-head-label-tags ((t (:foreground ,purple-two :weight bold))))

  ;; Outline, general outline mode for Emacs

  `(outline-1 ((t (:foreground ,cyan-two))))
  `(outline-2 ((t (:foreground ,blue-two))))
  `(outline-3 ((t (:foreground ,purple-two))))
  `(outline-4 ((t (:foreground ,yellow-two))))
  `(outline-5 ((t (:foreground ,orange-two))))
  `(outline-6 ((t (:foreground ,red-two))))

  ;; Rainbow delimiters: parenthesis, brackets, ...

  `(rainbow-delimiters-depth-1-face ((t (:foreground ,grey-five))))
  `(rainbow-delimiters-depth-2-face ((t (:foreground ,cyan-one))))
  `(rainbow-delimiters-depth-3-face ((t (:foreground ,green-one))))
  `(rainbow-delimiters-depth-4-face ((t (:foreground ,blue-one))))
  `(rainbow-delimiters-depth-5-face ((t (:foreground ,purple-one))))
  `(rainbow-delimiters-depth-6-face ((t (:foreground ,yellow-one))))
  `(rainbow-delimiters-depth-7-face ((t (:foreground ,orange-one))))
  `(rainbow-delimiters-depth-8-face ((t (:foreground ,red-one))))
  `(rainbow-delimiters-depth-9-face ((t (:foreground ,cyan-two))))
  `(rainbow-delimiters-depth-10-face ((t (:foreground ,green-two))))
  `(rainbow-delimiters-depth-11-face ((t (:foreground ,blue-two))))
  `(rainbow-delimiters-depth-12-face ((t (:foreground ,purple-two))))

  ;; Show parenthesis mode

  `(show-paren-mismatch ((t (:foreground ,yellow-one :background ,red-two :weight bold))))
  `(show-paren-match ((t (:foreground ,grey-one :background ,red-one :weight bold))))

  ;; SLIME, superior lisp interaction mode

  `(slime-repl-output-face ((t (:foreground ,grey-six))))
  `(slime-repl-inputed-output-face ((t (:foreground ,grey-five))))
  `(slime-error-face
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,red-one)))
     (t
      (:underline ,red-one))))
  `(slime-warning-face
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,yellow-two)))
     (t
      (:underline ,yellow-two))))
  `(slime-style-warning-face
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,yellow-one)))
     (t
      (:underline ,yellow-one))))
  `(slime-note-face
    ((((supports :underline (:style wave)))
      (:underline (:style wave :color ,purple-two)))
     (t
      (:underline ,purple-two))))
  `(slime-highlight-face ((t (:inherit cyan-one))))

  ;; Terminal

  `(term-color-black ((t (:foreground ,grey-six :background ,grey-six))))
  `(term-color-red ((t (:foreground ,red-two :background ,red-two))))
  `(term-color-green ((t (:foreground ,green-two :background ,green-two))))
  `(term-color-yellow ((t (:foreground ,yellow-two :background ,yellow-two))))
  `(term-color-blue ((t (:foreground ,blue-two :background ,blue-two))))
  `(term-color-magenta ((t (:foreground ,purple-two :background ,purple-two))))
  `(term-color-cyan ((t (:foreground ,cyan-two :background ,cyan-two))))
  `(term-color-white ((t (:foreground ,grey-one :background ,grey-one))))
  '(term-default-fg-color ((t (:inherit term-color-white))))
  '(term-default-bg-color ((t (:inherit term-color-black))))

  ;; Web mode

  `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
  `(web-mode-css-at-rule-face ((t (:foreground ,orange-two ))))
  `(web-mode-css-prop-face ((t (:foreground ,orange-two))))
  `(web-mode-css-pseudo-class-face ((t (:foreground ,yellow-two :weight bold))))
  `(web-mode-css-rule-face ((t (:foreground ,blue-two))))
  `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
  `(web-mode-folded-face ((t (:underline t))))
  `(web-mode-function-name-face ((t (:foreground ,grey-six :weight bold))))
  `(web-mode-html-attr-name-face ((t (:foreground ,purple-two))))
  `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
  `(web-mode-html-tag-face ((t (:foreground ,cyan-one :weight bold))))
  `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
  `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
  `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
  `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
  `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
  `(web-mode-server-background-face ((t (:background ,grey-one))))
  `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
  `(web-mode-server-string-face ((t (:foreground ,red-one))))
  `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
  `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
  `(web-mode-whitespaces-face ((t (:background ,red-one))))
  `(web-mode-block-face ((t (:background "gray88"))))
  `(web-mode-current-element-highlight-face ((t (:inverse-video t))))

  ;; Whitespace mode

  `(whitespace-space ((t (:background ,grey-one :foreground ,yellow-one))))
  `(whitespace-hspace ((t (:background ,grey-one :foreground ,yellow-one))))
  `(whitespace-tab ((t (:background ,orange-one))))
  `(whitespace-newline ((t (:foreground ,yellow-one))))
  `(whitespace-trailing ((t (:background ,red-one))))
  `(whitespace-line ((t (:background nil :foreground ,red-one))))
  `(whitespace-space-before-tab ((t (:background ,grey-one :foreground ,red-one))))
  `(whitespace-indentation ((t (:background ,grey-one :foreground ,yellow-one))))
  `(whitespace-empty ((t (:background ,yellow-two))))
  `(whitespace-space-after-tab ((t (:background ,grey-one :foreground ,red-one))))

  ))

;;
;; Theme Variables
;;

(material-design/with-color-variables
 (custom-theme-set-variables
  'material-design

  ;; ANSI Color

  `(ansi-color-names-vector [,grey-one ,red-one ,green-one ,yellow-one
                                     ,blue-one ,purple-one ,cyan-one ,grey-six])
  ;; Fill column indicator

  `(fci-rule-color ,yellow-one)

  ;; Highlight parenthesis

  `(hl-paren-colors '(,grey-one ,grey-one ,orange-one))
  `(hl-paren-background-colors '(,blue-one ,grey-three nil))
  ))

;; Footer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (deftheme material
;;   "A UI Theme for Emacs based on material design colors")
;; (display-color-cells (selected-frame))
;; (let* ((class '((class color) (min-colors 89)))
;;        (256color (eq (display-color-cells (selected-frame)) 256))

;;        (background (if window-system "#263238" "#262626")) ;; sidebar-container
;;        (current-line (if window-system  "#37474f" "#3a3a3a")) ;; tree-row
;;        (far-background (if window-system  "#1c1f26" "#121212")) ;; panel-control
;;        (inactive-gray (if window-system "#78909c" "#8a8a8a"))
;;        (header-color (if window-system "#455A64" "#5f5f5f"))
;;        (subtle "#a7adba") ;; tree-row-hover-disclosure-button-control
;;        (selection "#555555") ;; tab-control-dirty-tab-close-button
;;        (secondary-selection "#bf616a") ;; tab-control-hover-tab-close-button
;;        (foreground "#ffffff")
;;        (comment "#b0bec5") ;; table-row
;;        (red "#f36c60") ;; tab-control-hover-tab-close-button
;;        (orange "#ff9800") ;; darker tab-control-dirty-tab-close-butto
;;        (yellow "#fff59d") ;; tab-control-dirty-tab-close-button
;;        (green "#8bc34a") ;; complement tab-control-dirty-tab-close-button
;;        (aqua "#81d4fa") ;; lighter complement tab-control-dirty-tab-close-button
;;        (blue "#4dd0e1") ;; complement tab-control-dirty-tab-close-button
;;        (purple "#b39ddb")) ;; complement tab-control-dirty-tab-close-button

;;   (custom-theme-set-faces
;;    'material
;;    `(default ((,class (:foreground ,foreground :background ,background))))
;;    `(bold ((,class (:weight bold))))
;;    `(bold-italic ((,class (:slant italic :weight bold))))
;;    `(underline ((,class (:underline t))))
;;    `(italic ((,class (:slant italic))))
;;    `(font-lock-builtin-face ((,class (:foreground "#ff8A65"))))
;;    `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
;;    `(font-lock-comment-face ((,class (:foreground ,comment))))
;;    `(font-lock-constant-face ((,class (:foreground ,green))))
;;    `(font-lock-doc-face ((,class (:foreground "moccasin"))))
;;    `(font-lock-doc-string-face ((,class (:foreground ,yellow))))
;;    `(font-lock-function-name-face ((,class (:foreground ,"#84ffff"))))
;;    `(font-lock-keyword-face ((,class (:foreground ,yellow))))
;;    `(font-lock-negation-char-face ((,class (:foreground ,blue))))
;;    `(font-lock-preprocessor-face ((,class (:foreground "gold"))))
;;    `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
;;    `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
;;    `(font-lock-string-face ((,class (:foreground "#9ccc65"))))
;;    `(font-lock-type-face ((,class (:foreground "#84ffff"))))
;;    `(font-lock-variable-name-face ((,class (:foreground ,"#ffcc80"))))
;;    `(font-lock-warning-face ((,class (:weight bold :foreground ,red))))
;;    `(highlight-numbers-number ((,class (:foreground ,"#9ccc65"))))
;;    `(shadow ((,class (:foreground ,comment))))
;;    `(success ((,class (:foreground "SeaGreen2"))))
;;    `(error ((,class (:foreground ,red))))
;;    `(warning ((,class (:foreground ,orange))))

;;    ;; ace-window faces
;;    `(aw-leading-char-face ((,class (:foreground ,
;;                                     foreground
;;                                     :background ,
;;                                     "#ef6c00"
;;                                     :height ,
;;                                     1.7
;;                                     :weight
;;                                     bold))))

;;    ;; ace-jump-faces
;;    `(ace-jump-face-foreground ((,class (:foreground ,foreground
;;                                         :background ,"#ef6c00"
;;                                         :weight bold))))

;;    `(ace-jump-face-background ((,class (:foreground ,inactive-gray
;;                                         :weight normal))))

;;    ;; Flycheck
;;    `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
;;    `(flycheck-warning ((,class (:underline (:style wave :color ,orange)))))

;;    ;; highlight indentation
;;    `(highlight-indentation-face ((,class (:background, current-line))))
;;    `(highlight-indentation-current-column-face ((,class (:background, far-background))))

;;    ;; Flymake
;;    `(flymake-warnline ((,class (:underline (:style wave :color ,orange) :background ,background))))
;;    `(flymake-errline ((,class (:underline (:style wave :color ,red) :background ,background))))

;;    ;; Clojure errors
;;    `(clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
;;    `(clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
;;    `(clojure-test-success-face ((,class (:background nil :foreground nil :underline ,green))))
;;    `(clojure-keyword-face ((,class (:inherit font-lock-builtin-face))))

;;    ;; EDTS errors
;;    `(edts-face-warning-line ((t (:background nil :inherit flymake-warnline))))
;;    `(edts-face-warning-mode-line ((,class (:background nil :foreground ,orange :weight bold))))
;;    `(edts-face-error-line ((t (:background nil :inherit flymake-errline))))
;;    `(edts-face-error-mode-line ((,class (:background nil :foreground ,red :weight bold))))

;;    ;; For Brian Carper's extended clojure syntax table
;;    `(clojure-keyword ((,class (:foreground ,yellow))))
;;    `(clojure-parens ((,class (:foreground ,foreground))))
;;    `(clojure-braces ((,class (:foreground ,green))))
;;    `(clojure-brackets ((,class (:foreground ,yellow))))
;;    `(clojure-double-quote ((,class (:foreground ,aqua :background nil))))
;;    `(clojure-special ((,class (:foreground ,blue))))
;;    `(clojure-java-call ((,class (:foreground ,purple))))

;;    ;; Rainbow-delimiters
;;    `(rainbow-delimiters-depth-1-face ((,class (:foreground ,"#e91e63"))))
;;    `(rainbow-delimiters-depth-2-face ((,class (:foreground ,"#2196F3"))))
;;    `(rainbow-delimiters-depth-3-face ((,class (:foreground ,"#EF6C00"))))
;;    `(rainbow-delimiters-depth-4-face ((,class (:foreground ,"#B388FF"))))
;;    `(rainbow-delimiters-depth-5-face ((,class (:foreground ,"#76ff03"))))
;;    `(rainbow-delimiters-depth-6-face ((,class (:foreground ,"#26A69A"))))
;;    `(rainbow-delimiters-depth-7-face ((,class (:foreground ,"#FFCDD2"))))
;;    `(rainbow-delimiters-depth-8-face ((,class (:foreground ,"#795548"))))
;;    `(rainbow-delimiters-depth-9-face ((,class (:foreground ,"#DCE775"))))
;;    `(rainbow-delimiters-unmatched-face ((,class (:foreground ,foreground :background ,"#EF6C00"))))

;;    ;; MMM-mode
;;    `(mmm-code-submode-face ((,class (:background ,current-line))))
;;    `(mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))))
;;    `(mmm-output-submode-face ((,class (:background ,current-line))))

;;    ;; Search
;;    `(match ((,class (:foreground ,background :background ,green :inverse-video nil))))
;;    `(isearch ((,class (:foreground ,foreground :background ,green))))
;;    `(isearch-lazy-highlight-face ((,class (:foreground ,background :background ,green :inverse-video nil))))
;;    `(lazy-highlight-face ((,class (:foreground ,background :background ,green :inverse-video nil))))
;;    `(isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

;;    ;; Evil
;;    `(evil-search-highlight-persist-highlight-face ((,class (:foreground ,background
;;                                                             :background ,green))))
;;    ;; iedit
;;    `(iedit-occurrence ((,class (:foreground ,background :background ,green))))

;;    ;; ahs
;;    `(ahs-face ((,class (:foreground ,background :background ,green))))
;;    `(ahs-plugin-whole-buffer-face ((,class (:foreground ,foreground :background ,green))))

;;    ;; Anzu
;;    `(anzu-mode-line ((,class (:foreground ,orange))))
;;    `(anzu-replace-highlight ((,class (:inherit isearch-lazy-highlight-face))))
;;    `(anzu-replace-to ((,class (:inherit isearch))))

;;    ;; IDO
;;    `(ido-subdir ((,class (:foreground ,purple))))
;;    `(ido-first-match ((,class (:foreground ,orange))))
;;    `(ido-only-match ((,class (:foreground ,green))))
;;    `(ido-indicator ((,class (:foreground ,red :background ,background))))
;;    `(ido-virtual ((,class (:foreground ,comment))))

;;    ;; flx-ido
;;    `(flx-highlight-face ((,class (:inherit nil :foreground ,yellow :weight bold :underline nil))))

;;    ;; which-function
;;    `(which-func ((,class (:foreground ,blue :background nil))))

;;    ;; Emacs interface
;;    `(cursor ((,class (:background ,orange))))
;;    `(fringe ((,class (:background ,background))))
;;    `(linum ((,class (:background ,background :foreground ,subtle))))
;;    `(linum-highlight-face ((,class (:background ,current-line :foreground ,foreground))))
;;    `(border ((,class (:background ,current-line))))
;;    `(border-glyph ((,class (nil))))
;;    `(highlight ((,class (:inverse-video nil :background ,current-line))))
;;    `(gui-element ((,class (:background ,current-line :foreground ,foreground))))
;;    `(mode-line ((,class (:foreground ,foreground :background ,far-background))))
;;    `(mode-line-buffer-id ((,class (:foreground ,foreground :background nil))))
;;    `(mode-line-inactive ((,class (:inherit mode-line
;;                                            :foreground ,subtle
;;                                            :background ,far-background :weight normal
;;                                            :box nil))))
;;    `(mode-line-emphasis ((,class (:foreground ,foreground :slant italic))))
;;    `(mode-line-highlight ((,class (:foreground ,purple :box nil))))
;;    `(minibuffer-prompt ((,class (:foreground ,blue))))
;;    `(region ((,class (:background ,selection))))
;;    `(secondary-selection ((,class (:background ,secondary-selection))))

;;    `(header-line ((,class (:inherit mode-line :foreground ,purple :background nil))))

;;    `(trailing-whitespace ((,class (:foreground ,red :inverse-video t :underline nil))))
;;    `(whitespace-trailing ((,class (:foreground ,red :inverse-video t :underline nil))))
;;    `(whitespace-space-after-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
;;    `(whitespace-space-before-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
;;    `(whitespace-empty ((,class (:foreground ,red :inverse-video t :underline nil))))
;;    `(whitespace-line ((,class (:background nil :foreground ,red))))
;;    `(whitespace-indentation ((,class (:background nil :foreground ,aqua))))
;;    `(whitespace-space ((,class (:background nil :foreground ,selection))))
;;    `(whitespace-newline ((,class (:background nil :foreground ,selection))))
;;    `(whitespace-tab ((,class (:background nil :foreground ,selection))))
;;    `(whitespace-hspace ((,class (:background nil :foreground ,selection))))

;;    ;; Parenthesis matching (built-in)
;;    `(show-paren-match-face ((,class (:background ,aqua :foreground "black"))))
;;    `(show-paren-mismatch-face ((,class (:background "red1" :foreground "white"))))

;;    ;; Smartparens paren matching
;;    `(sp-show-pair-match-face ((,class (:foreground "black" :background ,aqua :inherit show-paren-match))))
;;    `(sp-show-pair-mismatch-face ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

;;    ;; Parenthesis matching (mic-paren)
;;    `(paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
;;    `(paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
;;    `(paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

;;    ;; Parenthesis dimming (parenface)
;;    `(paren-face ((,class (:foreground ,comment :background nil))))

;;    `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
;;    `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
;;    `(slime-highlight-edits-face ((,class (:weight bold))))
;;    `(slime-repl-input-face ((,class (:weight normal :underline nil))))
;;    `(slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,purple))))
;;    `(slime-repl-result-face ((,class (:foreground ,green))))
;;    `(slime-repl-output-face ((,class (:foreground ,blue :background ,background))))

;;    `(csv-separator-face ((,class (:foreground ,orange))))

;;    `(diff-added ((,class (:foreground ,green))))
;;    `(diff-changed ((,class (:foreground ,purple))))
;;    `(diff-removed ((,class (:foreground ,orange))))
;;    `(diff-header ((,class (:foreground ,aqua :background nil))))
;;    `(diff-file-header ((,class (:foreground ,blue :background nil))))
;;    `(diff-hunk-header ((,class (:foreground ,purple))))
;;    `(diff-refine-added ((,class (:inherit diff-added :inverse-video t))))
;;    `(diff-refine-removed ((,class (:inherit diff-removed :inverse-video t))))

;;    `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
;;    `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
;;    `(ediff-odd-diff-A  ((,class (:foreground ,comment :background nil :inverse-video t))))
;;    `(ediff-odd-diff-B  ((,class (:foreground ,comment :background nil :inverse-video t))))

;;    `(eldoc-highlight-function-argument ((,class (:foreground ,green :weight bold))))

;;    ;; macrostep
;;    `(macrostep-expansion-highlight-face ((,class (:inherit highlight :foreground nil))))

;;    ;; undo-tree
;;    `(undo-tree-visualizer-default-face ((,class (:foreground ,foreground))))
;;    `(undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
;;    `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
;;    `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

;;    ;; dired+
;;    `(diredp-compressed-file-suffix ((,class (:foreground ,blue))))
;;    `(diredp-deletion ((,class (:inherit error :inverse-video t))))
;;    `(diredp-deletion-file-name ((,class (:inherit error))))
;;    `(diredp-dir-heading ((,class (:foreground ,green :weight bold))))
;;    `(diredp-dir-priv ((,class (:foreground ,aqua :background nil))))
;;    `(diredp-exec-priv ((,class (:foreground ,blue :background nil))))
;;    `(diredp-executable-tag ((,class (:foreground ,red :background nil))))
;;    `(diredp-file-name ((,class (:foreground ,yellow))))
;;    `(diredp-file-suffix ((,class (:foreground ,green))))
;;    `(diredp-flag-mark ((,class (:foreground ,green :inverse-video t))))
;;    `(diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
;;    `(diredp-ignored-file-name ((,class (:foreground ,comment))))
;;    `(diredp-link-priv ((,class (:background nil :foreground ,purple))))
;;    `(diredp-mode-line-flagged ((,class (:foreground ,red))))
;;    `(diredp-mode-line-marked ((,class (:foreground ,green))))
;;    `(diredp-no-priv ((,class (:background nil))))
;;    `(diredp-number ((,class (:foreground ,yellow))))
;;    `(diredp-other-priv ((,class (:background nil :foreground ,purple))))
;;    `(diredp-rare-priv ((,class (:foreground ,red :background nil))))
;;    `(diredp-read-priv ((,class (:foreground ,green :background nil))))
;;    `(diredp-symlink ((,class (:foreground ,purple))))
;;    `(diredp-write-priv ((,class (:foreground ,yellow :background nil))))

;;    ;; Magit (a patch is pending in magit to make these standard upstream)
;;    `(magit-branch ((,class (:foreground ,green))))
;;    `(magit-diff-add ((,class (:inherit diff-added))))
;;    `(magit-diff-del ((,class (:inherit diff-removed))))
;;    `(magit-header ((,class (:inherit nil :weight bold))))
;;    `(magit-item-highlight ((,class (:inherit highlight :background nil))))
;;    `(magit-log-author ((,class (:foreground ,aqua))))
;;    `(magit-log-graph ((,class (:foreground ,comment))))
;;    `(magit-log-head-label-bisect-bad ((,class (:foreground ,red))))
;;    `(magit-log-head-label-bisect-good ((,class (:foreground ,green))))
;;    `(magit-log-head-label-default ((,class (:foreground ,yellow :box nil :weight bold))))
;;    `(magit-log-head-label-local ((,class (:foreground ,purple :box nil :weight bold))))
;;    `(magit-log-head-label-remote ((,class (:foreground ,purple :box nil :weight bold))))
;;    `(magit-log-head-label-tags ((,class (:foreground ,aqua :box nil :weight bold))))
;;    `(magit-log-sha1 ((,class (:foreground ,yellow))))
;;    `(magit-section-title ((,class (:foreground ,blue :weight bold))))

;;    ;; git-gutter
;;    `(git-gutter:modified ((,class (:foreground ,purple :weight bold))))
;;    `(git-gutter:added ((,class (:foreground ,green :weight bold))))
;;    `(git-gutter:deleted ((,class (:foreground ,red :weight bold))))
;;    `(git-gutter:unchanged ((,class (:background ,yellow))))

;;    ;; git-gutter-fringe
;;    `(git-gutter-fr:modified ((,class (:foreground ,purple :weight bold))))
;;    `(git-gutter-fr:added ((,class (:foreground ,green :weight bold))))
;;    `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))

;;    `(link ((,class (:foreground nil :underline t))))
;;    `(widget-button ((,class (:underline t :weight bold))))
;;    `(widget-field ((,class (:background ,current-line :box (:line-width 1 :color ,foreground)))))

;;    ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
;;    `(compilation-column-number ((,class (:foreground ,yellow))))
;;    `(compilation-line-number ((,class (:foreground ,yellow))))
;;    `(compilation-message-face ((,class (:foreground ,blue))))
;;    `(compilation-mode-line-exit ((,class (:foreground ,green))))
;;    `(compilation-mode-line-fail ((,class (:foreground ,red))))
;;    `(compilation-mode-line-run ((,class (:foreground ,blue))))

;;    ;; Grep
;;    `(grep-context-face ((,class (:foreground ,comment))))
;;    `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
;;    `(grep-hit-face ((,class (:foreground ,blue))))
;;    `(grep-match-face ((,class (:foreground nil :background nil :inherit match))))

;;    `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

;;    ;; Helm
;;    `(helm-header ((,class (:foreground ,foreground :background ,background))))
;;    `(helm-selection ((,class (:background ,current-line :foreground ,yellow))))
;;    `(helm-ff-file ((,class (:foreground ,foreground ))))
;;    `(helm-ff-directory ((,class (:foreground ,aqua ))))
;;    `(helm-ff-executable ((,class (:foreground ,green ))))
;;    `(helm-buffer-directory ((,class (:foreground ,aqua))))
;;    `(helm-buffer-file ((,class (:foreground ,foreground))))
;;    `(helm-grep-file ((,class (:foreground ,aqua :underline t))))
;;    `(helm-buffer-process ((,class (:foreground ,red))))
;;    `(helm-buffer-not-saved ((,class (:foreground ,orange))))
;;    `(helm-candidate-number ((,class (:foreground ,foreground :background ,"#ef6c00"))))
;;    `(helm-source-header ((,class (:background ,header-color :foreground ,"#eceff1" :height 1.3 :bold t ))))

;;    ;; guide-key
;;    `(guide-key/key-face ((,class (:foreground ,foreground ))))
;;    `(guide-key/highlight-command-face ((,class (:foreground ,yellow ))))
;;    `(guide-key/prefix-command-face ((,class (:foreground ,aqua ))))


;;    ;; mark-multiple
;;    `(mm/master-face ((,class (:inherit region :foreground nil :background nil))))
;;    `(mm/mirror-face ((,class (:inherit region :foreground nil :background nil))))

;;    `(org-agenda-structure ((,class (:foreground ,aqua :bold t))))
;;    `(org-agenda-date ((,class (:foreground ,blue :underline nil))))
;;    `(org-agenda-done ((,class (:foreground ,green))))
;;    `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
;;    `(org-block ((,class (:foreground ,green :background ,far-background))))
;;    `(org-block-background ((,t (:background ,far-background))))
;;    `(org-code ((,class (:foreground ,green :background ,far-background))))
;;    `(org-column ((,class (:background ,current-line))))
;;    `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
;;    `(org-date ((,class (:foreground ,"#80cbc4" :underline t))))
;;    `(org-document-info ((,class (:foreground ,aqua :height 1.35))))
;;    `(org-document-info-keyword ((,class (:foreground ,green :height 1.35))))
;;    `(org-document-title ((,class (:weight bold :foreground ,orange :height 1.35))))
;;    `(org-done ((,class (:foreground ,green :bold t :background,"#1b5e20"))))
;;    `(org-ellipsis ((,class (:foreground ,comment))))
;;    `(org-footnote ((,class (:foreground ,aqua))))
;;    `(org-formula ((,class (:foreground ,red))))
;;    `(org-hide ((,class (:foreground ,background :background ,background))))
;;    `(org-link ((,class (:foreground ,blue :underline t))))
;;    `(org-scheduled ((,class (:foreground ,green))))
;;    `(org-scheduled-previously ((,class (:foreground ,orange))))
;;    `(org-scheduled-today ((,class (:foreground ,green))))
;;    `(org-special-keyword ((,class (:foreground ,comment))))
;;    `(org-table ((,class (:foreground ,"#e3f2fd" :background ,far-background))))
;;    `(org-todo ((,class (:foreground ,"#ffab91" :bold t :background ,"#dd2c00"))))
;;    `(org-upcoming-deadline ((,class (:foreground ,orange))))
;;    `(org-warning ((,class (:weight bold :foreground ,red))))
;;    `(org-block-begin-line ((,class (:foreground ,"#b3e5fc" :background "#1e2930" :underline ,"#e1f5fe"))))
;;    `(org-block-end-line ((,class (:foreground ,"#b3e5fc" :background "#1e2930" :overline ,"#e1f5fe"))))

;;    `(org-level-1 ((,class (:inherit nil
;;                          :overline ,"#b0bec5"
;;                          :foreground ,"#eceff1"
;;                          :background ,header-color
;;                          :weight bold
;;                          :height 1.3))))
;;    `(org-level-2 ((,class (:inherit nil
;;                                   :foreground ,"#e1f5fe"
;;                                   :background ,"#35575b"
;;                                   :overline ,"#65757e"
;;                          :height 1.2))))
;;    `(org-level-3 ((,class (:inherit nil :foreground ,"#a5d6a7" :height 1.1))))
;;    `(org-level-4 ((,class (:inherit nil :foreground ,"#ffcc80" :height 1.0))))
;;    `(org-level-5 ((,class (:inherit nil :foreground ,"#b3e5fc"))))
;;    `(org-level-6 ((,class (:inherit nil :foreground ,"CadetBlue1"))))
;;    `(org-level-7 ((,class (:inherit nil :foreground ,"aquamarine1"))))
;;    `(org-level-8 ((,class (:inherit nil :foreground ,purple))))
;;    `(org-level-9 ((,class (:inherit nil :foreground ,"LightSteelBlue1"))))

;;    `(markdown-header-face-1 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.3 ))))
;;    `(markdown-header-face-2 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.2 ))))
;;    `(markdown-header-face-3 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
;;    `(markdown-header-face-4 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
;;    `(markdown-header-face-5 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
;;    `(markdown-header-face-6 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
;;    `(markdown-header-face-7 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
;;    `(markdown-header-face-8 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
;;    `(markdown-header-face-9 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
;;    `(markdown-header-delimiter-face ((,class (:inherit font-lock-function-name-face :weight bold
;;                                               :height 1.2))))
;;    `(markdown-url-face ((,class (:inherit link))))
;;    `(markdown-link-face ((,class (:foreground ,blue :underline t))))

;;    ;`(hl-sexp-face ((,class (:background ,current-line))))
;;    `(highlight-symbol-face ((,class (:background ,selection))))
;;    `(highlight-80+ ((,class (:background ,current-line))))

;;    ;; Python-specific overrides
;;    `(py-builtins-face ((,class (:foreground ,"#ff7043" :weight normal))))

;;    ;; js2-mode
;;    `(js2-warning ((,class (:underline ,orange))))
;;    `(js2-error ((,class (:foreground nil :underline ,red))))
;;    `(js2-external-variable ((,class (:foreground ,purple))))
;;    `(js2-function-param ((,class (:foreground ,blue))))
;;    `(js2-instance-member ((,class (:foreground ,blue))))
;;    `(js2-private-function-call ((,class (:foreground ,red))))

;;    ;; js3-mode
;;    `(js3-warning-face ((,class (:underline ,orange))))
;;    `(js3-error-face ((,class (:foreground nil :underline ,red))))
;;    `(js3-external-variable-face ((,class (:foreground ,purple))))
;;    `(js3-function-param-face ((,class (:foreground ,blue))))
;;    `(js3-jsdoc-tag-face ((,class (:foreground ,orange))))
;;    `(js3-jsdoc-type-face ((,class (:foreground ,aqua))))
;;    `(js3-jsdoc-value-face ((,class (:foreground ,yellow))))
;;    `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))))
;;    `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))))
;;    `(js3-instance-member-face ((,class (:foreground ,blue))))
;;    `(js3-private-function-call-face ((,class (:foreground ,red))))

;;    ;; coffee-mode
;;    `(coffee-mode-class-name ((,class (:foreground ,orange :weight bold))))
;;    `(coffee-mode-function-param ((,class (:foreground ,purple))))

;;    ;; nxml
;;    `(nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
;;    `(nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
;;    `(nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
;;    `(nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
;;    `(nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
;;    `(rng-error-face ((,class (:underline ,red))))

;;    ;; RHTML
;;    `(erb-delim-face ((,class (:background ,current-line))))
;;    `(erb-exec-face ((,class (:background ,current-line :weight bold))))
;;    `(erb-exec-delim-face ((,class (:background ,current-line))))
;;    `(erb-out-face ((,class (:background ,current-line :weight bold))))
;;    `(erb-out-delim-face ((,class (:background ,current-line))))
;;    `(erb-comment-face ((,class (:background ,current-line :weight bold :slant italic))))
;;    `(erb-comment-delim-face ((,class (:background ,current-line))))

;;    ;; Message-mode
;;    `(message-header-other ((,class (:foreground nil :background nil :weight normal))))
;;    `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,yellow))))
;;    `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))))
;;    `(message-header-cc ((,class (:inherit message-header-to :foreground nil))))
;;    `(message-header-name ((,class (:foreground ,blue :background nil))))
;;    `(message-header-newsgroups ((,class (:foreground ,aqua :background nil :slant normal))))
;;    `(message-separator ((,class (:foreground ,purple))))

;;    ;; Jabber
;;    `(jabber-chat-prompt-local ((,class (:foreground ,yellow))))
;;    `(jabber-chat-prompt-foreign ((,class (:foreground ,orange))))
;;    `(jabber-chat-prompt-system ((,class (:foreground ,yellow :weight bold))))
;;    `(jabber-chat-text-local ((,class (:foreground ,yellow))))
;;    `(jabber-chat-text-foreign ((,class (:foreground ,orange))))
;;    `(jabber-chat-text-error ((,class (:foreground ,red))))

;;    `(jabber-roster-user-online ((,class (:foreground ,green))))
;;    `(jabber-roster-user-xa ((,class :foreground ,comment)))
;;    `(jabber-roster-user-dnd ((,class :foreground ,yellow)))
;;    `(jabber-roster-user-away ((,class (:foreground ,orange))))
;;    `(jabber-roster-user-chatty ((,class (:foreground ,purple))))
;;    `(jabber-roster-user-error ((,class (:foreground ,red))))
;;    `(jabber-roster-user-offline ((,class (:foreground ,comment))))

;;    `(jabber-rare-time-face ((,class (:foreground ,comment))))
;;    `(jabber-activity-face ((,class (:foreground ,purple))))
;;    `(jabber-activity-personal-face ((,class (:foreground ,aqua))))

;;    ;; Company autocomplete
;;    ;; `(company-echo ((,class ())))
;;    ;; `(company-echo-common ((,class ())))
;;    ;; `(company-preview ((,class ())))
;;    `(company-preview-common ((,class (:foreground "#C0C0C0" :background "#FFFFD7")))) ; same background as highlight-line
;;    ;; `(company-preview-search ((,class ())))
;;    `(company-scrollbar-bg ((,class (:background "#F0F0F0"))))
;;    `(company-scrollbar-fg ((,class (:background "#C0C0C0"))))
;;    ;; `(company-template-field ((,class ())))
;;    `(company-tooltip ((,class (:weight bold :foreground, far-background :background ,inactive-gray))))
;;    `(company-tooltip-annotation ((,class (:weight normal :foreground ,comment :background ,inactive-gray))))
;;    `(company-tooltip-common ((,class (:weight normal :inherit company-tooltip))))
;;    `(company-tooltip-common-selection ((,class (:weight normal :inherit company-tooltip-selection))))
;;    ;; `(company-tooltip-mouse ((,class ())))
;;    ;; `(company-tooltip-search ((,class ())))
;;    `(company-tooltip-selection ((,class (:weight bold :foreground ,foreground :background ,current-line))))
   
;;    ;; Powerline
;;    `(powerline-active1 ((t (:foreground ,foreground :background ,selection))))
;;    `(powerline-active2 ((t (:foreground ,foreground :background ,inactive-gray))))
;;    `(powerline-inactive1 ((t (:foreground ,comment :background ,selection))))
;;    `(powerline-inactive2 ((t (:foreground ,comment :background ,selection))))

;;    ;; Outline
;;    `(outline-1 ((,class (:inherit nil
;;                 :foreground ,"#cfd8dc" 
;;                 ))))
;;    `(outline-2 ((,class (:inherit nil
;;                          :foreground ,"#b0bec5"
;;                 ))))
;;    `(outline-3 ((,class (:inherit nil :foreground ,"#a5d6a7" ))))
;;    `(outline-4 ((,class (:inherit nil :foreground ,"#ffcc80" ))))
;;    `(outline-5 ((,class (:inherit nil :foreground ,"#b3e5fc"))))
;;    `(outline-6 ((,class (:inherit nil :foreground ,"CadetBlue1"))))
;;    `(outline-7 ((,class (:inherit nil :foreground ,"aquamarine1"))))
;;    `(outline-8 ((,class (:inherit nil :foreground ,purple))))
;;    `(outline-9 ((,class (:inherit nil :foreground ,"LightSteelBlue1"))))


;;    ;; Ledger-mode
;;    `(ledger-font-comment-face ((,class (:inherit font-lock-comment-face))))
;;    `(ledger-font-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
;;    `(ledger-font-occur-xact-face ((,class (:inherit highlight))))
;;    `(ledger-font-payee-cleared-face ((,class (:foreground ,green))))
;;    `(ledger-font-payee-uncleared-face ((,class (:foreground ,aqua))))
;;    `(ledger-font-posting-account-cleared-face ((,class (:foreground ,blue))))
;;    `(ledger-font-posting-account-face ((,class (:foreground ,purple))))
;;    `(ledger-font-posting-account-pending-face ((,class (:foreground ,yellow))))
;;    `(ledger-font-xact-highlight-face ((,class (:inherit highlight))))
;;    `(ledger-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
;;    `(ledger-occur-xact-face ((,class (:inherit highlight))))

;;    ;; mu4e
;;    `(mu4e-header-highlight-face ((,class (:underline nil :inherit region))))
;;    `(mu4e-header-marks-face ((,class (:underline nil :foreground ,yellow))))
;;    `(mu4e-flagged-face ((,class (:foreground ,orange :inherit nil))))
;;    `(mu4e-replied-face ((,class (:foreground ,blue :inherit nil))))
;;    `(mu4e-unread-face ((,class (:foreground ,yellow :inherit nil))))
;;    `(mu4e-cited-1-face ((,class (:inherit outline-1 :slant normal))))
;;    `(mu4e-cited-2-face ((,class (:inherit outline-2 :slant normal))))
;;    `(mu4e-cited-3-face ((,class (:inherit outline-3 :slant normal))))
;;    `(mu4e-cited-4-face ((,class (:inherit outline-4 :slant normal))))
;;    `(mu4e-cited-5-face ((,class (:inherit outline-5 :slant normal))))
;;    `(mu4e-cited-6-face ((,class (:inherit outline-6 :slant normal))))
;;    `(mu4e-cited-7-face ((,class (:inherit outline-7 :slant normal))))
;;    `(mu4e-ok-face ((,class (:foreground ,green))))
;;    `(mu4e-view-contact-face ((,class (:inherit nil :foreground ,yellow))))
;;    `(mu4e-view-link-face ((,class (:inherit link :foreground ,blue))))
;;    `(mu4e-view-url-number-face ((,class (:inherit nil :foreground ,aqua))))
;;    `(mu4e-view-attach-number-face ((,class (:inherit nil :foreground ,orange))))
;;    `(mu4e-highlight-face ((,class (:inherit highlight))))
;;    `(mu4e-title-face ((,class (:inherit nil :foreground ,green))))

;;    ;; Gnus
;;    `(gnus-cite-1 ((,class (:inherit outline-1 :foreground nil))))
;;    `(gnus-cite-2 ((,class (:inherit outline-2 :foreground nil))))
;;    `(gnus-cite-3 ((,class (:inherit outline-3 :foreground nil))))
;;    `(gnus-cite-4 ((,class (:inherit outline-4 :foreground nil))))
;;    `(gnus-cite-5 ((,class (:inherit outline-5 :foreground nil))))
;;    `(gnus-cite-6 ((,class (:inherit outline-6 :foreground nil))))
;;    `(gnus-cite-7 ((,class (:inherit outline-7 :foreground nil))))
;;    `(gnus-cite-8 ((,class (:inherit outline-8 :foreground nil))))
;;    ;; there are several more -cite- faces...
;;    `(gnus-header-content ((,class (:inherit message-header-other))))
;;    `(gnus-header-subject ((,class (:inherit message-header-subject))))
;;    `(gnus-header-from ((,class (:inherit message-header-other-face :weight bold :foreground ,orange))))
;;    `(gnus-header-name ((,class (:inherit message-header-name))))
;;    `(gnus-button ((,class (:inherit link :foreground nil))))
;;    `(gnus-signature ((,class (:inherit font-lock-comment-face))))

;;    `(gnus-summary-normal-unread ((,class (:foreground ,foreground :weight bold))))
;;    `(gnus-summary-normal-read ((,class (:foreground ,comment :weight normal))))
;;    `(gnus-summary-normal-ancient ((,class (:foreground ,aqua :weight normal))))
;;    `(gnus-summary-normal-ticked ((,class (:foreground ,orange :weight normal))))
;;    `(gnus-summary-low-unread ((,class (:foreground ,comment :weight normal))))
;;    `(gnus-summary-low-read ((,class (:foreground ,comment :weight normal))))
;;    `(gnus-summary-low-ancient ((,class (:foreground ,comment :weight normal))))
;;    `(gnus-summary-high-unread ((,class (:foreground ,yellow :weight normal))))
;;    `(gnus-summary-high-read ((,class (:foreground ,green :weight normal))))
;;    `(gnus-summary-high-ancient ((,class (:foreground ,green :weight normal))))
;;    `(gnus-summary-high-ticked ((,class (:foreground ,orange :weight normal))))
;;    `(gnus-summary-cancelled ((,class (:foreground ,red :background nil :weight normal))))

;;    `(gnus-group-mail-low ((,class (:foreground ,comment))))
;;    `(gnus-group-mail-low-empty ((,class (:foreground ,comment))))
;;    `(gnus-group-mail-1 ((,class (:foreground nil :weight normal :inherit outline-1))))
;;    `(gnus-group-mail-2 ((,class (:foreground nil :weight normal :inherit outline-2))))
;;    `(gnus-group-mail-3 ((,class (:foreground nil :weight normal :inherit outline-3))))
;;    `(gnus-group-mail-4 ((,class (:foreground nil :weight normal :inherit outline-4))))
;;    `(gnus-group-mail-5 ((,class (:foreground nil :weight normal :inherit outline-5))))
;;    `(gnus-group-mail-6 ((,class (:foreground nil :weight normal :inherit outline-6))))
;;    `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :foreground ,comment))))
;;    `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :foreground ,comment))))
;;    `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :foreground ,comment))))
;;    `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-mail-4 :foreground ,comment))))
;;    `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-mail-5 :foreground ,comment))))
;;    `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-mail-6 :foreground ,comment))))
;;    `(gnus-group-news-1 ((,class (:foreground nil :weight normal :inherit outline-5))))
;;    `(gnus-group-news-2 ((,class (:foreground nil :weight normal :inherit outline-6))))
;;    `(gnus-group-news-3 ((,class (:foreground nil :weight normal :inherit outline-7))))
;;    `(gnus-group-news-4 ((,class (:foreground nil :weight normal :inherit outline-8))))
;;    `(gnus-group-news-5 ((,class (:foreground nil :weight normal :inherit outline-1))))
;;    `(gnus-group-news-6 ((,class (:foreground nil :weight normal :inherit outline-2))))
;;    `(gnus-group-news-1-empty ((,class (:inherit gnus-group-news-1 :foreground ,comment))))
;;    `(gnus-group-news-2-empty ((,class (:inherit gnus-group-news-2 :foreground ,comment))))
;;    `(gnus-group-news-3-empty ((,class (:inherit gnus-group-news-3 :foreground ,comment))))
;;    `(gnus-group-news-4-empty ((,class (:inherit gnus-group-news-4 :foreground ,comment))))
;;    `(gnus-group-news-5-empty ((,class (:inherit gnus-group-news-5 :foreground ,comment))))
;;    `(gnus-group-news-6-empty ((,class (:inherit gnus-group-news-6 :foreground ,comment))))

;;    ;; emms
;;    `(emms-playlist-selected-face ((,class (:foreground ,orange))))
;;    `(emms-playlist-track-face ((,class (:foreground ,blue))))
;;    `(emms-browser-track-face ((,class (:foreground ,blue))))
;;    `(emms-browser-artist-face ((,class (:foreground ,red :height 1.3))))
;;    `(emms-browser-composer-face ((,class (:inherit emms-browser-artist-face))))
;;    `(emms-browser-performer-face ((,class (:inherit emms-browser-artist-face))))
;;    `(emms-browser-album-face ((,class (:foreground ,green :height 1.2))))

;;    ;; stripe-buffer
;;    `(stripe-highlight ((,class (:background ,current-line))))
;;    `(stripe-hl-line ((,class (:background ,selection :foreground ,foreground))))

;;    ;; erc
;;    `(erc-direct-msg-face ((,class (:foreground ,orange))))
;;    `(erc-error-face ((,class (:foreground ,red))))
;;    `(erc-header-face ((,class (:foreground ,foreground :background ,selection))))
;;    `(erc-input-face ((,class (:foreground ,green))))
;;    `(erc-keyword-face ((,class (:foreground ,yellow))))
;;    `(erc-current-nick-face ((,class (:foreground ,green))))
;;    `(erc-my-nick-face ((,class (:foreground ,green))))
;;    `(erc-nick-default-face ((,class (:weight normal :foreground ,purple))))
;;    `(erc-nick-msg-face ((,class (:weight normal :foreground ,yellow))))
;;    `(erc-notice-face ((,class (:foreground ,comment))))
;;    `(erc-pal-face ((,class (:foreground ,orange))))
;;    `(erc-prompt-face ((,class (:foreground ,blue))))
;;    `(erc-timestamp-face ((,class (:foreground ,aqua))))
;;    `(erc-keyword-face ((,class (:foreground ,green))))

;;    ;; twittering-mode
;;    `(twittering-username-face ((,class (:inherit erc-pal-face))))
;;    `(twittering-uri-face ((,class (:foreground ,blue :inherit link))))
;;    `(twittering-timeline-header-face ((,class (:foreground ,green :weight bold))))
;;    `(twittering-timeline-footer-face ((,class (:inherit twittering-timeline-header-face))))

;;    `(custom-variable-tag ((,class (:foreground ,blue))))
;;    `(custom-group-tag ((,class (:foreground ,blue))))
;;    `(custom-state ((,class (:foreground ,green))))

;;    ;; ansi-term
;;    `(term ((,class (:foreground nil :background nil :inherit default))))
;;    `(term-color-black   ((,class (:foreground ,foreground :background ,foreground))))
;;    `(term-color-red     ((,class (:foreground ,red :background ,red))))
;;    `(term-color-green   ((,class (:foreground ,green :background ,green))))
;;    `(term-color-yellow  ((,class (:foreground ,yellow :background ,yellow))))
;;    `(term-color-blue    ((,class (:foreground ,blue :background ,blue))))
;;    `(term-color-magenta ((,class (:foreground ,purple :background ,purple))))
;;    `(term-color-cyan    ((,class (:foreground ,aqua :background ,aqua))))
;;    `(term-color-white   ((,class (:foreground ,background :background ,background)))))

;;   (custom-theme-set-variables
;;    'material
;;    `(fci-rule-color ,current-line)
;;    `(vc-annotate-color-map
;;      '((20  . ,red)
;;        (40  . ,orange)
;;        (60  . ,yellow)
;;        (80  . ,green)
;;        (100 . ,aqua)
;;        (120 . ,blue)
;;        (140 . ,purple)
;;        (160 . ,red)
;;        (180 . ,orange)
;;        (200 . ,yellow)
;;        (220 . ,green)
;;        (240 . ,aqua)
;;        (260 . ,blue)
;;        (280 . ,purple)
;;        (300 . ,red)
;;        (320 . ,orange)
;;        (340 . ,yellow)
;;        (360 . ,green)))
;;    `(vc-annotate-very-old-color nil)
;;    `(vc-annotate-background nil)
;;    `(ansi-color-names-vector (vector ,foreground ,red ,green ,yellow ,blue ,purple ,aqua ,background))
;;    '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))

;; ;;;###autoload
;; (when (and (boundp 'custom-theme-load-path)
;;            load-file-name)
;;   ;; add theme folder to `custom-theme-load-path' when installing over MELPA
;;   (add-to-list 'custom-theme-load-path
;;                (file-name-as-directory (file-name-directory load-file-name))))

;; (provide-theme 'material)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'material-design)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; material-design-theme.el ends here
