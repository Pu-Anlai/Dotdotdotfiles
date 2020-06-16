;; undo-tree is a dependency but not available in melpa so get it from elpa (straight takes care of this automatically)
(use-package undo-tree
  :config
  (setq undo-tree-enable-undo-in-region nil))

(use-package evil
  :after undo-tree
  :init
  (setq evil-search-module 'evil-search)
  :config
  (setq-default evil-symbol-word-search t)
  ;; workaround for view-mode keybinding behavior
  (add-hook 'view-mode-hook (lambda ()
                              (general-def :states 'normal :keymaps 'local
                                "q" nil)))
  (evil-mode 1)
  ;; sensible Y behavior
  (customize-set-variable 'evil-want-Y-yank-to-eol t)

  ;; set initial states for specific modes
  (dolist (modestate '((dashboard-mode . emacs)
                       (edebug-mode . emacs)
                       (vterm-mode . emacs)
                       (telega-root-mode . emacs)
                       (telega-chat-mode . insert)))
    (evil-set-initial-state (car modestate) (cdr modestate)))
  (add-hook 'evil-insert-state-entry-hook (lambda () (blink-cursor-mode 1)))
  (add-hook 'evil-insert-state-exit-hook (lambda () (blink-cursor-mode -1))))

(use-package vertigo
  :commands vertigo-set-digit-argument
  :config
  (setq vertigo-home-row '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?ö))
  (setq vertigo-cut-off 9)
  (evil-declare-motion #'vertigo-set-digit-argument)
  (evil-add-command-properties #'vertigo-set-digit-argument :jump t)
  (defun my/vertigo--remember-arg (func num)
    (setq-local my/vertigo--last-arg num)
    (funcall func num))
  (advice-add #'vertigo--set-digit-argument :around #'my/vertigo--remember-arg)
  (defun my/vertigo-reuse-last-arg ()
    (interactive)
    (if (boundp 'my/vertigo--last-arg)
        (vertigo--set-digit-argument my/vertigo--last-arg)
      (message "No previously used vertigo."))))

(use-package evil-commentary
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-line))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-replace-with-register
  :commands evil-replace-with-register)

(use-package evil-goggles
  :hook (after-init . evil-goggles-mode)
  :init
  (setq evil-goggles-duration 0.500)
  (setq evil-goggles-blocking-duration 0.001)
  (setq evil-goggles-enable-shift nil)
  (setq evil-goggles-enable-undo nil)
  (setq evil-goggles-enable-paste nil)
  (setq evil-goggles-enable-commentary nil)
  (setq evil-goggles-enable-surround nil)
  (setq evil-goggles-enable-delete nil))

(use-package evil-mc
  :commands (evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-make-all-cursors)
  :config
  (global-evil-mc-mode 1)
  (setq evil-mc-custom-known-commands
        '((indent-relative ((:default . evil-mc-execute-default-call))))))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

;; evil commands and ex-commands
(evil-define-command my/mv-buf-and-file (new-filename)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "<a>")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-filename)
          (message "A buffer named '%s' already exists!" new-filename)
        (progn
          (rename-file filename new-filename 1)
          (rename-buffer new-filename)
          (set-visited-file-name new-filename)
          (set-buffer-modified-p nil))))))

(evil-ex-define-cmd "mv" 'my/mv-buf-and-file)

(provide 'init-evil)
