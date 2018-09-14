;; undo-tree is a dependency but not available in melpa so get it from elpa
(use-package undo-tree
  :defer t)

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
  ;; cursor coloring to use with base16 package
  (defvar my/base16-colors base16-generic-colors)
  (setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
        evil-insert-state-cursor  `(,(plist-get my/base16-colors :base05) bar)
        evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
        evil-normal-state-cursor  `(,(plist-get my/base16-colors :base05) box)
        evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) hollow)
        evil-visual-state-cursor  `(,(plist-get my/base16-colors :base05) box)))

(use-package vertigo
  :commands vertigo-set-digit-argument
  :config
  (setq vertigo-home-row '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?ö))
  (setq vertigo-cut-off 9)
  (evil-declare-motion 'vertigo-set-digit-argument))

(use-package evil-commentary
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
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
