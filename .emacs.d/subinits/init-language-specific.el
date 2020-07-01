;; -*- lexical-binding: t -*-
;; default indentation settings (no TABs) - other settings on a per-mode basis
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; make shell scripts executable after save if they include a shebang
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; pos-tip setup for use by both company and flycheck
(use-package pos-tip
  :after company
  :config
  (setq x-gtk-use-system-tooltips nil)
  (add-hook 'focus-out-hook #'pos-tip-hide))

;; language server
(use-package eglot
  :hook ((python-mode go-mode) . eglot-ensure))

;; autocompletion
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (delq 'company-echo-metadata-frontend company-frontends)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq company-idle-delay 0.1)
  (push 'company-tng-frontend company-frontends)

  (defun my/company-select-next ()
    "Navigate company-mode and also open the quickhelp popup."
    (interactive)
    (company-quickhelp-manual-begin)
    (company-select-next))

  (defun my/company-select-previous ()
    "Navigate company-mode and also open the quickhelp popup."
    (interactive)
    (company-quickhelp-manual-begin)
    (company-select-previous))
  (evil-declare-not-repeat #'my/company-select-next)
  (evil-declare-not-repeat #'my/company-select-previous))

(use-package company-flx
  :after company
  :config
  (setq company-flx-limit 250)
  (company-flx-mode 1))

(use-package company-quickhelp
  :after (company pos-tip)
  :config
  (setq company-quickhelp-delay 0))

(use-package company-auctex
  :after (company tex))

;; syntax checking
(use-package flymake
  :config
  (setq flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe))

(use-package yasnippet
  :hook ((emacs-lisp-mode go-mode fish-mode snippet-mode python-mode mu4e-compose-mode) . yas-minor-mode)
  :config
  (yas-reload-all)
  ;; bind this here because yas-maybe-expand needs to be loaded first
  (general-def
    :states         'insert
    :keymaps        'yas-minor-mode-map
    "SPC"           yas-maybe-expand
    "<return>"      yas-maybe-expand)

  ;; expansion for some python snippets
  (general-def
    :keymaps    'python-mode-map
    :states     'insert
    ":"         yas-maybe-expand)

  ;; yas related functions
  (defun my/yas-choose-greeting (name lang)
    "Create a list of possible greetings from NAME and LANG and call
yas-choose-value on it."
    (cl-flet
        ((ncat (x) (concat x " " (my/last-name name))))
      (let
          ((name-list (pcase lang
                        ('de `(,@(mapcar #'ncat '("Liebe Frau" "Lieber Herr"))
                               ,(concat "Guten Tag " name)))
                        ('en `(,@(mapcar #'ncat '("Dear Ms." "Dear Mr."))
                               ,(concat "Dear " name)
                               ,(concat "Dear " (car (split-string name))))))))
        (yas-choose-value (cl-remove-duplicates name-list :test #'equal)))))

  (defun my/yas-func-padding (count &optional down)
    "Add COUNT empty lines above current position.

If DOWN is non-nil, then add lines below instead."
    (let ((counter count)
          (non-break t)
          (fillstr "")
          (direction (if down 1 -1))
          (current-line (line-number-at-pos)))
      ;; do nothing if we're already at the end or beginning of the file
      (unless (or
               (= current-line 1)
               (>= current-line (- (line-number-at-pos (max-char)) 1)))
        (save-excursion
          (while (and (> counter 0) non-break)
            (forward-line direction)
            (if (string= "" (my/get-line))
                (setq counter (1- counter))
              (setq non-break nil)))
          (make-string counter ?\n)))))

  (defun my/yas-indented-p (line)
    "Return t if LINE is indented, else return nil."
    (if (string-match-p "^\s" line) t nil))

  (defun my/mu4e-message-field (msg field)
    "Like `mu4e-message-field' but return nil if msg doesn't exist."
    (when msg
      (mu4e-message-field msg field)))

  (defun my/yas-snippet-key ()
    "Retrieve the key of the snippet that's currently being edited."
    (save-excursion
      (goto-char 0)
      (search-forward-regexp "# key:[[:space:]]*")
      (thing-at-point 'symbol t)))

  (defun my/yas-python-class-field-splitter (arg-string)
    "Return ARG-STRING as a conventional Python class field assignment block."
    (if (= (length arg-string) 0)
        ""
      (let ((clean-string)
            (field-list))
        (setq clean-string
              (string-trim-left (replace-regexp-in-string " ?[:=][^,]+" "" arg-string) ", "))
        (setq field-list (split-string clean-string ", +"))
        (string-join (mapcar (lambda (s) (concat "self." s " = " s "\n")) field-list)))))

  (defun my/yas-python-doc-wrapper (docstring side)
    "Wrap DOCSTRING in quotes on either left or right SIDE."
    (let* ((line-length (+ (python-indent-calculate-indentation) 6 (length docstring)))
           (nl ""))
      (when (> (+ (python-indent-calculate-indentation) 6 (length docstring)) fill-column)
        (setq nl "\n"))
      (apply 'concat
             (cond ((eq side 'left)
                    `("\"\"\"" ,nl))
                   ((eq side 'right)
                    `(,nl "\"\"\""))))))

  (defun my/yas-python-func-padding (indent &optional down)
    "Use Python INDENT to determine necessary padding for class or function declaration.
If decorator syntax is found a line above the current, don't do any padding."
    (let ((decorated nil))
      (unless down
        (save-excursion
          (forward-line -1)
          (setq decorated (string-match-p "^[ \t]*@" (my/get-line)))))
      ;; exit without any padding here if this is a decorated function
      (if decorated
          ""
        (my/yas-func-padding (if (> indent 0) 1 2) down)))))

;; language specific major modes and their settings
;; elisp helpers
(use-package evil-cleverparens
  :after evil-surround
  :commands (evil-cp-delete
             evil-cp-delete-line
             evil-cp-change
             evil-cp-change-line
             evil-cp-change-whole-line)
  :config
  (evil-cp--enable-surround-operators))

(use-package pcre2el
  :defer t)

(use-package suggest
  :commands suggest)

;; shell scripting
(use-package fish-mode
  :defer t
  :config
  (setq fish-enable-auto-indent t))

(use-package pkgbuild-mode
  :commands pkgbuild-mode)

;; latex
(use-package tex
  :straight auctex
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  :config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-auctex-init))

;; markdown
(use-package markdown-mode
  :defer t)

(use-package flymd
  :after markdown-mode
  :config
  (setq flymd-output-directory temporary-file-directory))

;; python settings
(add-hook
 'python-mode-hook
 (lambda ()
   ;; auto-fill
   (auto-fill-mode)
   (setq-local comment-auto-fill-only-comments t)
   (setq python-fill-docstring-style 'symmetric)
   ;; width settings
   (setq-local fill-column 79)
   (setq-local column-enforce-column 79)
   (setq-local electric-pair-open-newline-between-pairs nil)
   (make-local-variable 'write-file-functions)
   (add-to-list 'write-file-functions (my/nillify-func (eglot-format-buffer)))))

(use-package blacken
  :hook (python-mode . blacken-mode))

;; golang settings
(use-package go-mode
  :commands go-mode
  :config
  (add-hook
   'go-mode-hook
   (lambda ()
     (make-local-variable 'write-file-functions)
     (add-to-list 'write-file-functions (my/nillify-func (eglot-format-buffer))))))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

(provide 'init-language-specific)
