(deftheme base16-generic
  "A base16 theme which takes its colour values from __BASE16 environment
  variables.")

(let ((base00 (concat "#" (getenv "__BASE00")))
      (base01 (concat "#" (getenv "__BASE01")))
      (base02 (concat "#" (getenv "__BASE02")))
      (base03 (concat "#" (getenv "__BASE03")))
      (base04 (concat "#" (getenv "__BASE04")))
      (base05 (concat "#" (getenv "__BASE05")))
      (base06 (concat "#" (getenv "__BASE06")))
      (base07 (concat "#" (getenv "__BASE07")))
      (base08 (concat "#" (getenv "__BASE08")))
      (base09 (concat "#" (getenv "__BASE09")))
      (base0A (concat "#" (getenv "__BASE0A")))
      (base0B (concat "#" (getenv "__BASE0B")))
      (base0C (concat "#" (getenv "__BASE0C")))
      (base0D (concat "#" (getenv "__BASE0D")))
      (base0E (concat "#" (getenv "__BASE0E")))
      (base0F (concat "#" (getenv "__BASE0F"))))
  (apply
   #'custom-theme-set-faces
   'base16-generic
   (mapcar
    (lambda (elt)
      (let ((face (car elt))
            (props (cdr elt)))
        `(,face ((((type graphic)) ,@props)))))
    `(
;;; Built-in
;;;; basic colors
     (border                                       :background  ,base03)
     (cursor                                       :background  ,base08)
     (default                                      :foreground  ,base05 :background  ,base00)
     (fringe                                       :background  ,base00)
     (gui-element                                  :background  ,base01)
     (header-line                                  :foreground  ,base0E :background nil :inherit mode-line)
     (highlight                                    :background  ,base01)
     (link                                         :foreground  ,base0D :underline t)
     (link-visited                                 :foreground  ,base0E :underline t)
     (minibuffer-prompt                            :foreground  ,base0D :background ,base00)
     (region                                       :background  ,base02 :distant-foreground ,base05)
     (secondary-selection                          :background  ,base03 :distant-foreground ,base05)
     (trailing-whitespace                          :foreground  ,base0A :background  ,base0C)
     (vertical-border                              :foreground  ,base02)
     (widget-button                                :underline t)
     (widget-field                                 :background  ,base03 :box (:line-width 1 :color  ,base06))

     (error                                        :foreground  ,base08 :weight bold)
     (warning                                      :foreground  ,base09 :weight bold)
     (success                                      :foreground  ,base0B :weight bold)
     (shadow                                       :foreground  ,base03)

;;;; compilation
     (compilation-column-number                    :foreground  ,base0A)
     (compilation-line-number                      :foreground  ,base0A)
     (compilation-message-face                     :foreground  ,base0D)
     (compilation-mode-line-exit                   :foreground  ,base0B)
     (compilation-mode-line-fail                   :foreground  ,base08)
     (compilation-mode-line-run                    :foreground  ,base0D)

;;;; custom
     (custom-variable-tag                          :foreground  ,base0D)
     (custom-group-tag                             :foreground  ,base0D)
     (custom-state                                 :foreground  ,base0B)

;;;; font-lock
     (font-lock-builtin-face                       :foreground  ,base0C)
     (font-lock-comment-delimiter-face             :foreground  ,base02)
     (font-lock-comment-face                       :foreground  ,base03)
     (font-lock-constant-face                      :foreground  ,base09)
     (font-lock-doc-face                           :foreground  ,base04)
     (font-lock-doc-string-face                    :foreground  ,base03)
     (font-lock-function-name-face                 :foreground  ,base0D)
     (font-lock-keyword-face                       :foreground  ,base0E)
     (font-lock-negation-char-face                 :foreground  ,base0B)
     (font-lock-preprocessor-face                  :foreground  ,base0D)
     (font-lock-regexp-grouping-backslash          :foreground  ,base0A)
     (font-lock-regexp-grouping-construct          :foreground  ,base0E)
     (font-lock-string-face                        :foreground  ,base0B)
     (font-lock-type-face                          :foreground  ,base0A)
     (font-lock-variable-name-face                 :foreground  ,base08)
     (font-lock-warning-face                       :foreground  ,base08)

;;;; isearch
     (match                                        :foreground  ,base0D :background  ,base01 :inverse-video t)
     (isearch                                      :foreground  ,base0A :background  ,base01 :inverse-video t)
     (lazy-highlight                               :foreground  ,base0C :background  ,base01 :inverse-video t)
     (isearch-lazy-highlight-face                  :inherit lazy-highlight) ;; was replaced with 'lazy-highlight in emacs 22
     (isearch-fail                                 :background  ,base01 :inverse-video t :inherit font-lock-warning-face)

;;;; line-numbers
     (line-number                                  :foreground  ,base03 :background  ,base00)
     (line-number-current-line                     :inverse-video t)

;;;; mode-line
     (mode-line                                    :foreground  ,base04 :background  ,base00)
     (mode-line-buffer-id                          :foreground  ,base0B :background nil)
     (mode-line-emphasis                           :foreground  ,base06)
     (mode-line-highlight                          :foreground  ,base09 :slant italic)
     (mode-line-inactive                           :foreground  ,base00 :background  ,base00 :box nil)

;;; Third-party

;;;; auctex
     (font-latex-bold-face                         :foreground  ,base0B)
     (font-latex-doctex-documentation-face         :background  ,base03)
     (font-latex-italic-face                       :foreground  ,base0B)
     (font-latex-math-face                         :foreground  ,base09)
     (font-latex-sectioning-0-face                 :foreground  ,base0A)
     (font-latex-sectioning-1-face                 :foreground  ,base0A)
     (font-latex-sectioning-2-face                 :foreground  ,base0A)
     (font-latex-sectioning-3-face                 :foreground  ,base0A)
     (font-latex-sectioning-4-face                 :foreground  ,base0A)
     (font-latex-sectioning-5-face                 :foreground  ,base0A)
     (font-latex-sedate-face                       :foreground  ,base0C)
     (font-latex-string-face                       :foreground  ,base0A)
     (font-latex-verbatim-face                     :foreground  ,base09)
     (font-latex-warning-face                      :foreground  ,base08)

     (TeX-error-description-error                  :inherit error)
     (TeX-error-description-tex-said               :inherit font-lock-function-name-face)
     (TeX-error-description-warning                :inherit warning)

;;;; avy
     (avy-lead-face-0                              :foreground  ,base00 :background  ,base0C)
     (avy-lead-face-1                              :foreground  ,base00 :background  ,base05)
     (avy-lead-face-2                              :foreground  ,base00 :background  ,base0E)
     (avy-lead-face                                :foreground  ,base00 :background  ,base09)
     (avy-background-face                          :foreground  ,base03)
     (avy-goto-char-timer-face                     :inherit highlight)

;;;; company-mode
     (company-tooltip                              :inherit tooltip)
     (company-scrollbar-bg                         :background  ,base07)
     (company-scrollbar-fg                         :background  ,base04)
     (company-tooltip-annotation                   :foreground  ,base08)
     (company-tooltip-common                       :inherit font-lock-constant-face)
     (company-tooltip-selection                    :background  ,base02 :inherit font-lock-function-name-face)
     (company-tooltip-search                       :inherit match)
     (company-tooltip-search-selection             :inherit match)
     (company-preview-common                       :inherit secondary-selection)
     (company-preview                              :foreground  ,base04)
     (company-preview-search                       :inherit match)
     (company-echo-common                          :inherit secondary-selection)

;;;; cperl-mode
     (cperl-array-face                             :weight bold :inherit font-lock-variable-name-face)
     (cperl-hash-face                              :weight bold :slant italic :inherit font-lock-variable-name-face)
     (cperl-nonoverridable-face                    :inherit font-lock-builtin-face)

;;;; cscope-minor-mode
     (cscope-file-face                             :foreground  ,base0B)
     (cscope-function-face                         :foreground  ,base0D)
     (cscope-line-number-face                      :foreground  ,base0A)
     (cscope-mouse-face                            :foreground  ,base04 :background  ,base01)
     (cscope-separator-face                        :foreground  ,base08 :overline t :underline t :weight bold)

;;;; csv-mode
     (csv-separator-face                           :foreground  ,base09)

;;;; diff-hl-mode
     (diff-hl-change                               :foreground  ,base0E)
     (diff-hl-delete                               :foreground  ,base08)
     (diff-hl-insert                               :foreground  ,base0B)

;;;; diff-mode
     (diff-added                                   :foreground  ,base0B)
     (diff-changed                                 :foreground  ,base0E)
     (diff-removed                                 :foreground  ,base08)
     (diff-header                                  :background  ,base01)
     (diff-file-header                             :background  ,base02)
     (diff-hunk-header                             :foreground  ,base0E :background  ,base01)

;;;; dired+
     (diredp-compressed-file-suffix                :foreground  ,base0D)
     (diredp-dir-heading                           :foreground nil :background nil :inherit heading)
     (diredp-dir-priv                              :foreground  ,base0C :background nil)
     (diredp-exec-priv                             :foreground  ,base0D :background nil)
     (diredp-executable-tag                        :foreground  ,base08 :background nil)
     (diredp-file-name                             :foreground  ,base0A)
     (diredp-file-suffix                           :foreground  ,base0B)
     (diredp-flag-mark-line                        :background nil :inherit highlight)
     (diredp-ignored-file-name                     :foreground  ,base04)
     (diredp-link-priv                             :foreground  ,base0E :background nil)
     (diredp-mode-line-flagged                     :foreground  ,base08)
     (diredp-mode-line-marked                      :foreground  ,base0B)
     (diredp-no-priv                               :background nil)
     (diredp-number                                :foreground  ,base0A)
     (diredp-other-priv                            :foreground  ,base0E :background nil)
     (diredp-rare-priv                             :foreground  ,base08 :background nil)
     (diredp-read-priv                             :foreground  ,base0B :background nil)
     (diredp-symlink                               :foreground  ,base0E)
     (diredp-write-priv                            :foreground  ,base0A :background nil)

;;;; ediff-mode
     (ediff-even-diff-A                            :foreground nil :background nil :inverse-video t)
     (ediff-even-diff-B                            :foreground nil :background nil :inverse-video t)
     (ediff-odd-diff-A                             :foreground  ,base04 :background nil :inverse-video t)
     (ediff-odd-diff-B                             :foreground  ,base04 :background nil :inverse-video t)

;;;; eldoc-mode
     (eldoc-highlight-function-argument            :foreground  ,base0B :weight bold)

;;;; erc
     (erc-direct-msg-face                          :foreground  ,base09)
     (erc-error-face                               :foreground  ,base08)
     (erc-header-face                              :foreground  ,base06 :background  ,base04)
     (erc-input-face                               :foreground  ,base0B)
     (erc-keyword-face                             :foreground  ,base0A)
     (erc-current-nick-face                        :foreground  ,base0B)
     (erc-my-nick-face                             :foreground  ,base0B)
     (erc-nick-default-face                        :foreground  ,base0E :weight normal)
     (erc-nick-msg-face                            :foreground  ,base0A :weight normal)
     (erc-notice-face                              :foreground  ,base04)
     (erc-pal-face                                 :foreground  ,base09)
     (erc-prompt-face                              :foreground  ,base0D)
     (erc-timestamp-face                           :foreground  ,base0C)

;;;; eshell
     (eshell-ls-archive                            :foreground  ,base08)
     (eshell-ls-backup                             :foreground  ,base0F)
     (eshell-ls-clutter                            :foreground  ,base09)
     (eshell-ls-directory                          :foreground  ,base0D)
     (eshell-ls-executable                         :foreground  ,base0B)
     (eshell-ls-missing                            :foreground  ,base08)
     (eshell-ls-product                            :foreground  ,base0F)
     (eshell-ls-readonly                           :foreground  ,base06)
     (eshell-ls-special                            :foreground  ,base0E)
     (eshell-ls-symlink                            :foreground  ,base0C)
     (eshell-ls-unreadable                         :foreground  ,base04)
     (eshell-prompt                                :foreground  ,base05)

;;;; evil-mode
     (evil-search-highlight-persist-highlight-face :background  ,base01 :inverse-video t :inherit font-lock-warning-face)

;;;; flymake-mode
     (flymake-warnline                             :background  ,base01 :underline  ,base09)
     (flymake-errline                              :background  ,base01 :underline  ,base08)
     (flymake-warning                              :background  ,base01 :underline  ,base09)
     (flymake-error                                :background  ,base01 :underline  ,base08)

;;;; flyspell-mode
     (flyspell-duplicate                           :underline (:style wave :color  ,base09))
     (flyspell-incorrect                           :underline (:style wave :color  ,base08))

;;;; gnus
     (gnus-cite-1                                  :foreground nil :inherit outline-1)
     (gnus-cite-2                                  :foreground nil :inherit outline-2)
     (gnus-cite-3                                  :foreground nil :inherit outline-3)
     (gnus-cite-4                                  :foreground nil :inherit outline-4)
     (gnus-cite-5                                  :foreground nil :inherit outline-5)
     (gnus-cite-6                                  :foreground nil :inherit outline-6)
     (gnus-cite-7                                  :foreground nil :inherit outline-7)
     (gnus-cite-8                                  :foreground nil :inherit outline-8)
     ;; there are several more -cite- faces...
     (gnus-header-content                          :inherit message-header-other)
     (gnus-header-subject                          :inherit message-header-subject)
     (gnus-header-from                             :foreground  ,base09 :weight bold :inherit message-header-other-face)
     (gnus-header-name                             :inherit message-header-name)
     (gnus-button                                  :foreground nil :inherit link)
     (gnus-signature                               :inherit font-lock-comment-face)

     (gnus-summary-normal-unread                   :foreground  ,base0D :weight normal)
     (gnus-summary-normal-read                     :foreground  ,base06 :weight normal)
     (gnus-summary-normal-ancient                  :foreground  ,base0C :weight normal)
     (gnus-summary-normal-ticked                   :foreground  ,base09 :weight normal)
     (gnus-summary-low-unread                      :foreground  ,base04 :weight normal)
     (gnus-summary-low-read                        :foreground  ,base04 :weight normal)
     (gnus-summary-low-ancient                     :foreground  ,base04 :weight normal)
     (gnus-summary-high-unread                     :foreground  ,base0A :weight normal)
     (gnus-summary-high-read                       :foreground  ,base0B :weight normal)
     (gnus-summary-high-ancient                    :foreground  ,base0B :weight normal)
     (gnus-summary-high-ticked                     :foreground  ,base09 :weight normal)
     (gnus-summary-cancelled                       :foreground  ,base08 :background nil :weight normal)

     (gnus-group-mail-low                          :foreground  ,base04)
     (gnus-group-mail-low-empty                    :foreground  ,base04)
     (gnus-group-mail-1                            :foreground nil :weight normal :inherit outline-1)
     (gnus-group-mail-2                            :foreground nil :weight normal :inherit outline-2)
     (gnus-group-mail-3                            :foreground nil :weight normal :inherit outline-3)
     (gnus-group-mail-4                            :foreground nil :weight normal :inherit outline-4)
     (gnus-group-mail-5                            :foreground nil :weight normal :inherit outline-5)
     (gnus-group-mail-6                            :foreground nil :weight normal :inherit outline-6)
     (gnus-group-mail-1-empty                      :foreground  ,base04 :inherit gnus-group-mail-1)
     (gnus-group-mail-2-empty                      :foreground  ,base04 :inherit gnus-group-mail-2)
     (gnus-group-mail-3-empty                      :foreground  ,base04 :inherit gnus-group-mail-3)
     (gnus-group-mail-4-empty                      :foreground  ,base04 :inherit gnus-group-mail-4)
     (gnus-group-mail-5-empty                      :foreground  ,base04 :inherit gnus-group-mail-5)
     (gnus-group-mail-6-empty                      :foreground  ,base04 :inherit gnus-group-mail-6)
     (gnus-group-news-1                            :foreground nil :weight normal :inherit outline-5)
     (gnus-group-news-2                            :foreground nil :weight normal :inherit outline-6)
     (gnus-group-news-3                            :foreground nil :weight normal :inherit outline-7)
     (gnus-group-news-4                            :foreground nil :weight normal :inherit outline-8)
     (gnus-group-news-5                            :foreground nil :weight normal :inherit outline-1)
     (gnus-group-news-6                            :foreground nil :weight normal :inherit outline-2)
     (gnus-group-news-1-empty                      :foreground  ,base04 :inherit gnus-group-news-1)
     (gnus-group-news-2-empty                      :foreground  ,base04 :inherit gnus-group-news-2)
     (gnus-group-news-3-empty                      :foreground  ,base04 :inherit gnus-group-news-3)
     (gnus-group-news-4-empty                      :foreground  ,base04 :inherit gnus-group-news-4)
     (gnus-group-news-5-empty                      :foreground  ,base04 :inherit gnus-group-news-5)
     (gnus-group-news-6-empty                      :foreground  ,base04 :inherit gnus-group-news-6)

;;;; grep
     (grep-context-face                            :foreground  ,base04)
     (grep-error-face                              :foreground  ,base08 :weight bold :underline t)
     (grep-hit-face                                :foreground  ,base0D)
     (grep-match-face                              :foreground nil :background nil :inherit match)

;;;; hl-line-mode
     (hl-line                                      :background  ,base01)
     (col-highlight                                :background  ,base01)

;;;; ido-mode
     (ido-subdir                                   :foreground  ,base04)
     (ido-first-match                              :foreground  ,base09 :weight bold)
     (ido-only-match                               :foreground  ,base08 :weight bold)
     (ido-indicator                                :foreground  ,base08 :background  ,base01)
     (ido-virtual                                  :foreground  ,base04)

;;;; imenu-list
     (imenu-list-entry-face-0                      :foreground  ,base0A)
     (imenu-list-entry-face-1                      :foreground  ,base0B)
     (imenu-list-entry-face-2                      :foreground  ,base0D)
     (imenu-list-entry-face-3                      :foreground  ,base0F)

;;;; ivy-mode
     (ivy-current-match                            :foreground  ,base09 :background  ,base01)
     (ivy-minibuffer-match-face-1                  :foreground  ,base0E)
     (ivy-minibuffer-match-face-2                  :foreground  ,base0D)
     (ivy-minibuffer-match-face-3                  :foreground  ,base0C)
     (ivy-minibuffer-match-face-4                  :foreground  ,base0B)
     (ivy-confirm-face                             :foreground  ,base0B)
     (ivy-match-required-face                      :foreground  ,base08)
     (ivy-virtual                                  :foreground  ,base04)
     (ivy-action                                   :foreground  ,base0D)

;;;; js2-mode
     (js2-warning-face                             :underline  ,base09)
     (js2-error-face                               :foreground nil :underline  ,base08)
     (js2-external-variable-face                   :foreground  ,base0E)
     (js2-function-param-face                      :foreground  ,base0D)
     (js2-instance-member-face                     :foreground  ,base0D)
     (js2-private-function-call-face               :foreground  ,base08)

;;;; js3-mode
     (js3-warning-face                             :underline  ,base09)
     (js3-error-face                               :foreground nil :underline  ,base08)
     (js3-external-variable-face                   :foreground  ,base0E)
     (js3-function-param-face                      :foreground  ,base0D)
     (js3-jsdoc-tag-face                           :foreground  ,base09)
     (js3-jsdoc-type-face                          :foreground  ,base0C)
     (js3-jsdoc-value-face                         :foreground  ,base0A)
     (js3-jsdoc-html-tag-name-face                 :foreground  ,base0D)
     (js3-jsdoc-html-tag-delimiter-face            :foreground  ,base0B)
     (js3-instance-member-face                     :foreground  ,base0D)
     (js3-private-function-call-face               :foreground  ,base08)

;;;; linum-mode
     (linum                                        :foreground  ,base03 :background  ,base01)

;;;; magit
     (magit-blame-culprit                          :background  ,base01)
     (magit-blame-heading                          :background  ,base01 :foreground  ,base05)
     (magit-branch                                 :foreground  ,base04 :weight bold)
     (magit-branch-current                         :foreground  ,base0C :weight bold :box t)
     (magit-branch-local                           :foreground  ,base0C :weight bold)
     (magit-branch-remote                          :foreground  ,base0B :weight bold)
     (magit-cherry-equivalent                      :foreground  ,base0E)
     (magit-cherry-unmatched                       :foreground  ,base0C)
     (magit-diff-context-highlight                 :background  ,base01 :foreground  ,base05)
     (magit-diff-file-header                       :background  ,base01 :foreground  ,base05)
     (magit-hash                                   :foreground  ,base0D)
     (magit-header-line                            :background  ,base02 :foreground  ,base05 :weight bold)
     (magit-hunk-heading                           :background  ,base03)
     (magit-hunk-heading-highlight                 :background  ,base03)
     (magit-diff-hunk-heading                      :background  ,base01)
     (magit-diff-hunk-heading-highlight            :background  ,base01)
     (magit-item-highlight                         :background  ,base01)
     (magit-log-author                             :foreground  ,base0D)
     (magit-process-ng                             :foreground  ,base08 :inherit magit-section-heading)
     (magit-process-ok                             :foreground  ,base0B :inherit magit-section-heading)
     (magit-reflog-amend                           :foreground  ,base0E)
     (magit-reflog-checkout                        :foreground  ,base0D)
     (magit-reflog-cherry-pick                     :foreground  ,base0B)
     (magit-reflog-commit                          :foreground  ,base0B)
     (magit-reflog-merge                           :foreground  ,base0B)
     (magit-reflog-other                           :foreground  ,base0C)
     (magit-reflog-rebase                          :foreground  ,base0E)
     (magit-reflog-remote                          :foreground  ,base0C)
     (magit-reflog-reset                           :foreground  ,base08)
     (magit-section-highlight                      :background  ,base01)
     (magit-signature-bad                          :foreground  ,base08 :weight bold)
     (magit-signature-error                        :foreground  ,base08)
     (magit-signature-expired                      :foreground  ,base09)
     (magit-signature-good                         :foreground  ,base0B)
     (magit-signature-revoked                      :foreground  ,base0E)
     (magit-signature-untrusted                    :foreground  ,base0C)
     (magit-tag                                    :foreground  ,base05)

;;;; markdown-mode
     (markdown-url-face                            :inherit link)
     (markdown-link-face                           :foreground  ,base0D :underline t)

;;;; message-mode
     (message-header-other                         :foreground nil :background nil :weight normal)
     (message-header-subject                       :foreground  ,base0A :weight bold :inherit message-header-other)
     (message-header-to                            :foreground  ,base09 :weight bold :inherit message-header-other)
     (message-header-cc                            :foreground nil :inherit message-header-to)
     (message-header-name                          :foreground  ,base0D :background nil)
     (message-header-newsgroups                    :foreground  ,base0C :background nil :slant normal)
     (message-separator                            :foreground  ,base0E)

;;;; mode-welt (my modeline)
     (mode-welt-evil-normal                        :foreground  ,base0B :weight bold :inherit mode-line)
     (mode-welt-evil-insert                        :foreground  ,base0D :weight bold :inherit mode-line)
     (mode-welt-evil-visual                        :foreground  ,base0E :weight bold :inherit mode-line)
     (mode-welt-evil-replace                       :foreground  ,base08 :weight bold :inherit mode-line)
     (mode-welt-evil-operator                      :foreground  ,base01 :weight bold :inherit mode-line)
     (mode-welt-evil-motion                        :foreground  ,base0C :weight bold :inherit mode-line)
     (mode-welt-evil-emacs                         :foreground  ,base0E :weight bold :inherit mode-line)
     (mode-welt-evil-inactive                      :foreground  ,base00 :weight bold)

;;;; nxml-mode
     (nxml-name-face                               :foreground unspecified :inherit font-lock-constant-face)
     (nxml-attribute-local-name-face               :foreground unspecified :inherit font-lock-variable-name-face)
     (nxml-ref-face                                :foreground unspecified :inherit font-lock-preprocessor-face)
     (nxml-delimiter-face                          :foreground unspecified :inherit font-lock-keyword-face)
     (nxml-delimited-data-face                     :foreground unspecified :inherit font-lock-string-face)
     (rng-error-face                               :underline  ,base08)

;;;; org-mode
     (org-agenda-structure                         :foreground  ,base0E)
     (org-agenda-date                              :foreground  ,base0D :underline nil)
     (org-agenda-done                              :foreground  ,base0B)
     (org-agenda-dimmed-todo-face                  :foreground  ,base04)
     (org-block                                    :foreground  ,base09)
     (org-code                                     :foreground  ,base0A)
     (org-column                                   :background  ,base01)
     (org-column-title                             :weight bold :underline t :inherit org-column)
     (org-date                                     :foreground  ,base0E :underline t)
     (org-document-info                            :foreground  ,base0C)
     (org-document-info-keyword                    :foreground  ,base0B)
     (org-document-title                           :foreground  ,base09 :weight bold :height 1.44)
     (org-done                                     :foreground  ,base0B :background  ,base01)
     (org-ellipsis                                 :foreground  ,base04)
     (org-footnote                                 :foreground  ,base0C)
     (org-formula                                  :foreground  ,base08)
     (org-hide                                     :foreground  ,base03)
     (org-link                                     :foreground  ,base0D)
     (org-scheduled                                :foreground  ,base0B)
     (org-scheduled-previously                     :foreground  ,base09)
     (org-scheduled-today                          :foreground  ,base0B)
     (org-special-keyword                          :foreground  ,base09)
     (org-table                                    :foreground  ,base0E)
     (org-todo                                     :foreground  ,base08 :background  ,base01)
     (org-upcoming-deadline                        :foreground  ,base09)
     (org-warning                                  :foreground  ,base08 :weight bold)

;;;; popup
     (popup-face                                   :foreground  ,base05 :background  ,base02)
     (popup-isearch-match                          :foreground  ,base00 :background  ,base0B)
     (popup-scroll-bar-background-face             :background  ,base03)
     (popup-scroll-bar-foreground-face             :background  ,base05)
     (popup-summary-face                           :foreground  ,base04)
     (popup-tip-face                               :foreground  ,base00 :background  ,base0A)
     (popup-menu-mouse-face                        :foreground  ,base00 :background  ,base0D)
     (popup-menu-selection-face                    :foreground  ,base00 :background  ,base0C)

;;;; python-mode
     (py-builtins-face                             :foreground  ,base09 :weight normal)

;;;; rainbow-delimiters
     (rainbow-delimiters-depth-1-face              :foreground  ,base0E)
     (rainbow-delimiters-depth-2-face              :foreground  ,base0D)
     (rainbow-delimiters-depth-3-face              :foreground  ,base0C)
     (rainbow-delimiters-depth-4-face              :foreground  ,base0B)
     (rainbow-delimiters-depth-5-face              :foreground  ,base0A)
     (rainbow-delimiters-depth-6-face              :foreground  ,base09)
     (rainbow-delimiters-depth-7-face              :foreground  ,base08)
     (rainbow-delimiters-depth-8-face              :foreground  ,base03)
     (rainbow-delimiters-depth-9-face              :foreground  ,base05)

;;;; regex-tool
     (regex-tool-matched-face                      :foreground nil :background nil :inherit match)

;;;; sh-mode
     (sh-heredoc                                   :foreground nil :weight normal :inherit font-lock-string-face)
     (sh-quoted-exec                               :foreground nil :inherit font-lock-preprocessor-face)

;;;; show-paren-mode
     (show-paren-match                             :foreground  ,base01 :background  ,base0D)
     (show-paren-mismatch                          :foreground  ,base01 :background  ,base09)

;; telephone-line
     (telephone-line-accent-active                 :foreground  ,base00 :background  ,base05)
     (telephone-line-accent-inactive               :foreground  ,base01 :background  ,base03)
     (telephone-line-evil-normal                   :foreground  ,base01 :background  ,base0B :weight bold)
     (telephone-line-evil-insert                   :foreground  ,base01 :background  ,base0D :weight bold)
     (telephone-line-evil-visual                   :foreground  ,base06 :background  ,base0E :weight bold)
     (telephone-line-evil-replace                  :foreground  ,base01 :background  ,base08 :weight bold)
     (telephone-line-evil-operator                 :foreground  ,base0B :background  ,base01 :weight bold)
     (telephone-line-evil-motion                   :foreground  ,base00 :background  ,base0C :weight bold)
     (telephone-line-evil-emacs                    :foreground  ,base07 :background  ,base0E :weight bold)
     (telephone-line-warning                       :foreground  ,base09 :weight bold)
     (telephone-line-error                         :foreground  ,base08 :weight bold)

;;;; term and ansi-term
     (term                                         :foreground  ,base05 :background  ,base00)
     (term-color-black                             :foreground  ,base02 :background  ,base00)
     (term-color-white                             :foreground  ,base05 :background  ,base07)
     (term-color-red                               :foreground  ,base08 :background  ,base08)
     (term-color-yellow                            :foreground  ,base0A :background  ,base0A)
     (term-color-green                             :foreground  ,base0B :background  ,base0B)
     (term-color-cyan                              :foreground  ,base0C :background  ,base0C)
     (term-color-blue                              :foreground  ,base0D :background  ,base0D)
     (term-color-magenta                           :foreground  ,base0E :background  ,base0E)

;;;; tooltip
     (tooltip                                      :background  ,base01 :inherit default)
     
;;;; undo-tree-mode
     (undo-tree-visualizer-default-face            :foreground  ,base06)
     (undo-tree-visualizer-current-face            :foreground  ,base0B :weight bold)
     (undo-tree-visualizer-active-branch-face      :foreground  ,base08)
     (undo-tree-visualizer-register-face           :foreground  ,base0A)

;;;; utop-mode
     (utop-prompt                                  :foreground  ,base0E)
     (utop-error                                   :underline (:style wave :color  ,base08) :inherit error)

;;;; which-func-mode
     (which-func                                   :foreground  ,base0D :background nil :weight bold)

;;;; whitespace-mode
     (whitespace-empty                             :foreground  ,base08 :background  ,base0A)
     (whitespace-hspace                            :foreground  ,base04 :background  ,base04)
     (whitespace-indentation                       :foreground  ,base08 :background  ,base0A)
     (whitespace-line                              :foreground  ,base0F :background  ,base01)
     (whitespace-newline                           :foreground  ,base04)
     (whitespace-space                             :foreground  ,base03 :background  ,base01)
     (whitespace-space-after-tab                   :foreground  ,base08 :background  ,base0A)
     (whitespace-space-before-tab                  :foreground  ,base08 :background  ,base09)
     (whitespace-tab                               :foreground  ,base03 :background  ,base01)
     (whitespace-trailing                          :foreground  ,base0A :background  ,base08))))

  (setq evil-emacs-state-cursor `(,base0D box)
        evil-insert-state-cursor `(,base05 bar)
        evil-motion-state-cursor `(,base0E box)
        evil-normal-state-cursor `(,base05 box)
        evil-replace-state-cursor `(,base08 hollow)
        evil-visual-state-cursor `(,base05 box)
        pos-tip-foreground-color base00
        pos-tip-background-color base06))

(provide-theme 'base16-generic)
