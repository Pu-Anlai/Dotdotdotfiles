;; -*- lexical-binding: t -*-
;; default indentation settings (no TABs) - other settings on a per-mode basis
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; abbreviation settings
;; expand abbreviation upon exiting insert stat
(add-hook 'evil-insert-state-exit-hook #'expand-abbrev)
(setq save-abbrevs 'silently)

;; mode associations
(push '(".gitignore" . prog-mode) auto-mode-alist)

;; syntax checking
(use-package flymake
  :after evil
  :hook ((go-mode python-mode) . flymake-mode)
  :general
  (:states          'normal
   :keymaps         'flymake-mode-map
   "M--"            'flymake-goto-next-error
   "M-_"            'flymake-goto-prev-error)
  (general-goleader
    :states         'normal
    :keymaps        'flymake-mode-map
    "!"             'flymake-diagnostic-buffer)
  ;; flymake diagnostics buffer keybinds
  (:states          'normal
   :keymaps         'flymake-diagnostics-buffer-mode-map
   "j"              'next-line
   "k"              'previous-line
   "RET"            'flymake-goto-diagnostic)

  :config
  (setq flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe)
  (mapc #'evil-declare-not-repeat #'(flymake-goto-next-error flymake-goto-prev-error)))

(use-package yasnippet
  :hook ((go-mode fish-mode snippet-mode python-mode mu4e-compose-mode) . yas-minor-mode)
  :general
  (:keymaps         '(yas-keymap yas/keymap)
   "M-j"            'yas-next-field-or-maybe-expand
   "M-k"            'yas-prev-field
   "M-S-j"          'yas-skip-and-clear-field)
  (general-leader
    :states         'normal
    :keymaps        'snippet-mode-map
    "YY"            'yas-load-snippet-buffer-and-close
    "Yy"            'yas-load-snippet-buffer)
  (general-leader
    :keymaps        '(go-mode-map fish-mode-map python-mode-map mu4e-compose-mode-map)
    :states         'normal
    "Yn"            'yas-new-snippet
    "Ye"            'yas-visit-snippet-file
    "Yi"            'yas-insert-snippet
    "Yt"            'yas-describe-tables)

  :config
  (yas-reload-all)
  ;; bind this here because yas-maybe-expand needs to be loaded first
  (general-define-key
    :states         'insert
    :keymaps        'yas-minor-mode-map
    "SPC"           yas-maybe-expand
    "<return>"      yas-maybe-expand)  

  ;; expansion for some python snippets
  (general-define-key
   :keymaps         'python-mode-map
   :states          'insert
   ":"              yas-maybe-expand)

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
               (>= current-line (- (line-number-at-pos (buffer-end 1)) 1)))
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
  :general
  (general-leader
    :states         'motion
    :keymaps        'eglot-mode-map
    "hx"            'eglot-help-at-point)
  (general-goleader
    :states         'motion
    :keymaps        'eglot-mode-map
    "d"             'xref-find-definitions
    "="             'eglot-format-buffer
    "*"             'xref-find-references)
  (general-goleader
    :states          'visual
    :keymaps         'eglot-mode-map
    "="              'eglot-format)
  :init
  (setq eglot-workspace-configuration
        '((:pyls . (:plugins (:pycodestyle (:enabled nil)))))))

;; autocompletion
(use-package company
  :hook ((prog-mode . company-mode)
         (company-mode . company-tng-mode))
  :general
  (:keymaps         'company-tng-map
   "<return>"       (general-l
                      (unless (company-tooltip-visible-p)
                        (company-complete)
                        (company-pseudo-tooltip-hide))
                      (newline 1 t))
   "M-<return>"     (general-l
                      (company-abort)
                      (newline 1 t))
   "C-n"            'company-select-next
   "C-p"            'company-select-previous) 
  (:states          'insert
   :keymaps         'company-search-map
   "<return>"       (general-l
                      (company-complete)
                      (company-pseudo-tooltip-hide)
                      (newline 1 t)))
  (general-unbind
   :keymaps         '(company-active-map company-tng-map)
    "<tab>"
    "TAB"
    "<backtab>")
  
  :config
  (setq company-minimum-prefix-length 3
        company-selection-wrap-around t
        company-idle-delay 0.01
        company-echo-delay 0.5)

  (mapc #'evil-declare-not-repeat #'(°company-select-next °company-select-previous)))

(use-package company-box
  :after company
  ;; load tng-mode first so it doesn't overwrite company-frontends
  :hook (company-tng-mode . company-box-mode)
  :config
  (setq company-frontends '(company-tng-frontend company-box-frontend))
  (setq company-box-doc-delay 0))

(use-package company-flx
  :hook (company-mode . company-flx-mode)
  :config
  (setq company-flx-limit 250)
  (company-flx-mode 1))

(use-package magit
  :hook ((magit-mode . °source-ssh-env)
         (with-editor-mode . evil-insert-state))
  :general
  (:states          'emacs
   :keymaps         'magit-mode-map
   "j"              'magit-section-forward
   "k"              'magit-section-backward
   "p"              'magit-push
   "d"              'magit-delete-thing
   "D"              'magit-diff
   "Ju"             'magit-jump-to-unstaged
   "Js"             'magit-jump-to-staged
   "Jn"             'magit-jump-to-untracked
   "Jz"             'magit-jump-to-stashes
   "Jt"             'magit-jump-to-tracked)
  (general-goleader
    :states         'normal
    "G"             'magit-status)
  (:states          'normal
   :keymaps         'magit-mode-map
   "q"              'quit-window)

  :config
  (defun °force-git-access ()
    (interactive)
    (let ((index-file (°join-path nil
                       (project-root (project-current)) (file-name-as-directory ".git") "index.lock")))
      (when (yes-or-no-p (concat "Really delete " index-file "?"))
        (delete-file index-file)))))

(use-package project
  :general
  (:keymaps         'motion
   "C-q"            'project-switch-project
   "Q"              'project-find-file))

(use-package quickrun
  :general
  (general-leader
    :keymaps        'normal
    "RET"           'quickrun)
  (general-leader
    :keymaps        'visual
    "RET"           'quickrun-region)
  (:states          'normal
   :keymaps         'quickrun--mode-map
   "q"              'quit-window)

  :config
  (setq quickrun-focus-p nil))


(provide 'init-general-programming)
