;; -*- lexical-binding: t -*-
;; default indentation settings (no TABs) - other settings on a per-mode basis
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; abbreviation settings
;; expand abbreviation upon exiting insert stat
(add-hook 'evil-insert-state-exit-hook #'expand-abbrev)

;; mode associations
(push '(".gitignore" . prog-mode) auto-mode-alist)

;; pos-tip setup for use by both company and flymake
(use-package pos-tip
  :after company
  :config
  (setq x-gtk-use-system-tooltips nil)
  (add-hook 'focus-out-hook #'pos-tip-hide))

;; syntax checking
(use-package flymake
  :config
  (setq flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe)
  (mapc #'evil-declare-not-repeat #'(flymake-goto-next-error flymake-goto-prev-error)))

(use-package yasnippet
  :hook ((go-mode fish-mode snippet-mode python-mode mu4e-compose-mode) . yas-minor-mode)
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
  (defun °yas-choose-greeting (name lang)
    "Create a list of possible greetings from NAME and LANG and call
yas-choose-value on it."
    (setq name (capitalize (or name "")))
    (cl-flet
        ((ncat (x) (concat x " " (°last-name name))))
      (let
          ((name-list (pcase lang
                        ('de `(,@(mapcar #'ncat '("Liebe Frau" "Lieber Herr"))
                               ,(concat "Guten Tag " name)
                               "Guten Tag"))
                        ('en `(,@(mapcar #'ncat '("Dear Ms." "Dear Mr."))
                               ,(concat "Dear " name)
                               ,(concat "Dear " (car (split-string name)))
                               "Hello")))))
        (yas-choose-value (cl-remove-duplicates name-list :test #'equal)))))

  (defun °yas-content (snippet)
    "Return plain-text content of SNIPPET."
    (yas--template-content (yas-lookup-snippet snippet)))

  (defun °yas-func-padding (count &optional down)
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
            (if (string= "" (°get-line))
                (setq counter (1- counter))
              (setq non-break nil)))
          (make-string counter ?\n)))))

  (defun °yas-indented-p (line)
    "Return t if LINE is indented, else return nil."
    (if (string-match-p "^\s" line) t nil))

  (defun °yas-snippet-key ()
    "Retrieve the key of the snippet that's currently being edited."
    (save-excursion
      (goto-char 0)
      (search-forward-regexp "# key:[[:space:]]*")
      (thing-at-point 'symbol t)))

  (defun °yas-python-class-field-splitter (arg-string)
    "Return ARG-STRING as a conventional Python class field assignment block."
    (if (= (length arg-string) 0)
        ""
      (let ((clean-string)
            (field-list))
        (setq clean-string
              (string-trim-left (replace-regexp-in-string " ?[:=][^,]+" "" arg-string) ", "))
        (setq field-list (split-string clean-string ", +"))
        (string-join (mapcar (lambda (s) (concat "self." s " = " s "\n")) field-list)))))

  (defun °yas-python-doc-wrapper (docstring side)
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

  (defun °yas-python-func-padding (indent &optional down)
    "Use Python INDENT to determine necessary padding for class or function declaration.
If decorator syntax is found a line above the current, don't do any padding."
    (let ((decorated nil))
      (unless down
        (save-excursion
          (forward-line -1)
          (setq decorated (string-match-p "^[ \t]*@" (°get-line)))))
      ;; exit without any padding here if this is a decorated function
      (if decorated
          ""
        (°yas-func-padding (if (> indent 0) 1 2) down)))))

;; language server
(use-package eglot
  :hook ((python-mode go-mode) . eglot-ensure)
  :init
  ;; work around for wrong project version being loaded
  (use-package project)
  (setq eglot-workspace-configuration
        '((:pyls . (:plugins (:pycodestyle (:enabled nil)))))))

;; autocompletion
(use-package company
  :hook ((prog-mode . company-mode)
         (company-mode . company-tng-mode))
  :config
  (delq 'company-echo-metadata-frontend company-frontends)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq company-idle-delay 0.1)

  (defun °company-select-next ()
    "Navigate company-mode and also open the quickhelp popup."
    (interactive)
    (company-quickhelp-manual-begin)
    (company-select-next))

  (defun °company-select-previous ()
    "Navigate company-mode and also open the quickhelp popup."
    (interactive)
    (company-quickhelp-manual-begin)
    (company-select-previous))
  (mapc #'evil-declare-not-repeat #'(°company-select-next °company-select-previous)))

(use-package company-flx
  :hook (company-mode . company-flx-mode)
  :config
  (setq company-flx-limit 250)
  (company-flx-mode 1))

(use-package company-quickhelp
  :after (company pos-tip)
  :config
  (setq company-quickhelp-delay 0))

(use-package magit
  :commands magit-status
  :hook ((magit-mode . °source-ssh-env)
         (with-editor-mode . evil-insert-state))
  :config
  (defun °force-git-access ()
    (interactive)
    (let ((index-file (concat
                       (projectile-project-root) (file-name-as-directory ".git") "index.lock")))
      (when (yes-or-no-p (concat "Really delete " index-file "?"))
        (delete-file index-file)))))

(use-package projectile
  :hook (prog-mode . projectile-mode))

(use-package quickrun
  :commands quickrun
  :config
  (setq quickrun-focus-p nil))


(provide 'init-general-programming)
