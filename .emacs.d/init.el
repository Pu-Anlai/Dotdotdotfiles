;; -*- lexical-binding: t -*-
;; get rid of the custom blabla by using custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; set up a separate location for backup and temp files
(defconst emacs-tmp-dir (expand-file-name "auto-save" user-emacs-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,(concat emacs-tmp-dir "/\\1") t)))
    (setq auto-save-list-file-prefix
      emacs-tmp-dir)

(defconst emacs-subinit-dir (expand-file-name "subinits" user-emacs-directory))
;; enable sourcing from init scripts in emacs.d/subinits
(push emacs-subinit-dir load-path)

(require 'init-package-management)

;; everything that can be deferred goes in here
(use-package init-my-functions
  :straight nil
  :commands (my/add-hook-to-mode
             my/dired-mark-toggle
             my/eshell
             my/eval-at-point
             my/eval-normal-line
             my/eval-visual-region
             my/evil-dry-open-above
             my/evil-dry-open-below
             my/evil-lisp-append-line
             my/evil-lisp-first-non-blank
             my/evil-lisp-insert-line
             my/evil-lisp-open-above
             my/evil-lisp-open-below
             my/evil-lisp-paste-with-newline-above
             my/evil-lisp-paste-with-newline-below
             my/evil-paste-with-newline-above
             my/evil-paste-with-newline-below
             my/evil-search-visual-selection
             my/get-line
             my/ispell-cycle-dicts
             my/join-path
             my/nillify-func
             my/python-remove-breakpoints
             my/python-test
             my/restore-window-layout
             my/source-ssh-env
             my/split-window-and-do
             my/split-window-sensibly
             my/straight-update
             my/sudo-find-file
             my/toggle-scratch-buffer
             my/window-clear-side
             my//window-layout-stack-push))

;; setup gui early to avoid modeline troubles
(require 'init-gui-setup)

;; load up org-mode with workarounds
(require 'init-org-mode)

;; various mode setting options
(push '(".gitignore" . prog-mode) auto-mode-alist)

;; mu4e (lazily so emacs still runs without it)
(unless (require 'init-mu4e nil t)
  (message "Error loading mu4e."))

(require 'init-ivy)

(require 'init-evil)

(require 'init-emacs-extensions)

(require 'init-general-programming)

(require 'init-keybinds)

(require 'init-language-specific)
