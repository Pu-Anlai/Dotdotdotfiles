;; get rid of the custom blabla by using custom-file
(defconst emacs-custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p emacs-custom-file)
  (write-region "" "" emacs-custom-file))

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

;; and everything that can be deferred goes in here
(use-package init-my-functions
  :straight nil
  :commands (my/get-line
             my/sudo-find-file
             my/dired-mark-toggle
             my/eshell
             my/eval-at-point
             my/eval-visual-region
             my/eval-normal-line
             my/evil-dry-open-below
             my/evil-dry-open-above
             my/evil-lisp-first-non-blank
             my/evil-lisp-append-line
             my/evil-lisp-insert-line
             my/evil-lisp-open-below
             my/evil-lisp-open-above
             my/evil-lisp-paste-with-newline-above
             my/evil-lisp-paste-with-newline-below
             my/evil-paste-with-newline-above
             my/evil-paste-with-newline-below
             my/evil-search-visual-selection
             my/nillify-func
             my/python-remove-breakpoints
             my/python-test
             my/source-ssh-env
             my/split-window-sensibly
             my/straight-update
             my/toggle-scratch-buffer
             my/window-clear-side
             my/split-window-and-do))

;; setup gui early to avoid modeline troubles
(require 'init-gui-setup)

;; load up org-mode with workarounds
(require 'init-org-mode)

;; various mode setting options
(push '(".gitignore" . prog-mode) auto-mode-alist)

;; mu4e
(require 'init-mu4e)

(require 'init-ivy)

(require 'init-evil)

(require 'init-emacs-extensions)

(require 'init-general-programming)

(require 'init-keybinds)

(require 'init-language-specific)

;; load custom file late so it can make use of previously defined references
(load (expand-file-name emacs-custom-file user-emacs-directory))
