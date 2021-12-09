;; -*- lexical-binding: t -*-
(use-package evil
  :init
  (setq evil-search-module 'evil-search)
  (evil-mode 1)
  :general
  ;; window navigation
  (:keymaps         'override
   :states          '(motion emacs)
   "M-c"            'evil-window-delete
   "M-h"            'evil-window-left
   "M-j"            'evil-window-down
   "M-k"            'evil-window-up
   "M-l"            'evil-window-right
   "M-H"            'evil-window-move-far-left
   "M-J"            'evil-window-move-very-bottom
   "M-K"            'evil-window-move-very-top
   "M-L"            'evil-window-move-far-right)
  ;; other override bindings
  (general-leader
    :keymaps        'override
    :states         'motion
    "<tab>"         'evil-switch-to-windows-last-buffer)
  ;; MOTION state bindings
  (:keymaps         'motion
   "("              'evil-backward-paragraph
   ")"              'evil-forward-paragraph
   "C-u"            'evil-scroll-up
   "<escape>"       (general-l
                      (evil-ex-nohighlight)
                      (evil-force-normal-state))
   "{"              'evil-backward-sentence-begin
   "}"              'evil-forward-sentence-begin)
  (general-leader
    :keymaps        'motion
    "v"             'evil-window-split
    "s"             'evil-window-vsplit
    "S"             (general-l
                      (evil-window-vsplit) (evil-window-right 1))
    "V"             (general-l
                      (evil-window-split) (evil-window-down 1)))
  (general-goleader
    :states         'motion
    :keymaps        'Info-mode-map
    "g"             'evil-goto-first-line)
  ;; NORMAL state bindings
  (:keymaps         'normal
   "<escape>"       (general-l
                      (evil-ex-nohighlight)
                      (evil-force-normal-state))
   "ö"              '°evil-dry-open-below
   "Ö"              '°evil-dry-open-above)
  (general-leader
    :states         'normal
    "P"             '°evil-paste-with-newline-above
    "p"             '°evil-paste-with-newline-below)
  ;; VISUAL state bindings
  (:keymaps         'visual
   "*"              (lambda (count)
                      (interactive "P")
                      (°evil-search-visual-selection 'forward count))
   "#"              (lambda (count)
                      (interactive "P")
                      (°evil-search-visual-selection 'backward count)))
  
  :config
  (setq-default evil-symbol-word-search t)
  ;; workaround for view-mode keybinding behavior
  (add-hook 'view-mode-hook (lambda ()
                              (general-define-key :states 'normal :keymaps 'local
                                "q" nil)))
  
  ;; sensible Y behavior
  (customize-set-variable 'evil-want-Y-yank-to-eol t)
  ;; set undo backend to undo-fu
  (evil-set-undo-system 'undo-fu)
  
  ;; set initial states for specific modes
  (dolist (modestate '((edebug-mode . emacs)
                       (vterm-mode . emacs)))
    (evil-set-initial-state (car modestate) (cdr modestate)))
  (add-hook 'evil-insert-state-entry-hook (lambda () (blink-cursor-mode 1)))
  (add-hook 'evil-insert-state-exit-hook (lambda () (blink-cursor-mode -1)))

  ;; evil commands and ex-commands
  (evil-define-command °mv-buf-and-file (new-filename)
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

  (evil-ex-define-cmd "mv" '°mv-buf-and-file))

(use-package undo-fu
  :commands (evil-undo evil-redo))

(use-package vertigo
  :general
  (:keymaps         'motion
   "<S-SPC><S-SPC>" '°vertigo-reuse-last-arg)
  (general-leader
    :keymaps        'motion
    "SPC"           'vertigo-set-digit-argument)
  :config
  (setq vertigo-home-row '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?ö))
  (setq vertigo-cut-off 9)
  (evil-declare-motion #'vertigo-set-digit-argument)
  (evil-add-command-properties #'vertigo-set-digit-argument :jump t)
  (defun °vertigo--remember-arg (func num)
    (setq-local °vertigo--last-arg num)
    (funcall func num))
  (advice-add #'vertigo--set-digit-argument :around #'°vertigo--remember-arg)
  (defun °vertigo-reuse-last-arg ()
    (interactive)
    (if (boundp '°vertigo--last-arg)
        (vertigo--set-digit-argument °vertigo--last-arg)
      (message "No previously used vertigo."))))

(use-package evil-commentary
  :general
  (general-goleader
    :states         'normal
    "c"             'evil-commentary))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-replace-with-register
  :general
  (general-goleader
    :states         'normal
    "r"             'evil-replace-with-register))

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
  :general
  ;; evil-mc-key-map is forced on us so unbind it here
  (general-unbind
    :states         '(normal motion visual)
    :keymaps        'evil-mc-key-map
    "gr"
    "C-p"
    "C-n"
    "M-N"
    "M-n"
    "M-P"
    "M-p")
  (:keymaps         'motion
    "M-n"           'evil-mc-make-and-goto-next-match
    "M-N"           'evil-mc-skip-and-goto-next-match
    "M-p"           'evil-mc-skip-and-goto-prev-cursor
    "M-P"           (general-l
                      (evil-mc-undo-cursor-at-pos (point))
                      (evil-mc-skip-and-goto-prev-cursor))
    "Ä"             'evil-mc-undo-all-cursors
    "ä"             'evil-mc-make-all-cursors)
  (:keymaps         'visual
   "M-n"            'evil-mc-make-and-goto-next-match
   "M-p"            'evil-mc-skip-and-goto-prev-cursor)
  :config
  (global-evil-mc-mode 1)
  (setq evil-mc-custom-known-commands
        '((indent-relative ((:default . evil-mc-execute-default-call))))))

(use-package evil-numbers
  :general
  (:keymaps         'normal
   "C-a"            'evil-numbers/inc-at-pt
   "C-x"            'evil-numbers/dec-at-pt))

(provide 'init-evil)
