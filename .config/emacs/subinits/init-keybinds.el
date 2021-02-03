;; -*- lexical-binding: t -*-
(use-package general
  :config
  (general-auto-unbind-keys)

  (general-create-definer general-def-leader
    :prefix "SPC")

  (general-create-definer general-def-goleader
    :prefix "g")

  ;; use these EVERYWHERE
  (general-def
    :keymaps            'override
    :states             '(motion emacs)
    "M-o"               'delete-other-windows
    "M-i"               '°restore-window-layout
    "M-c"               'evil-window-delete
    "M-O"               '°window-clear-side
    "M-h"               'evil-window-left
    "M-j"               'evil-window-down
    "M-k"               'evil-window-up
    "M-l"               'evil-window-right
    "M-H"               'evil-window-move-far-left
    "M-J"               'evil-window-move-very-bottom
    "M-K"               'evil-window-move-very-top
    "M-L"               'evil-window-move-far-right
    "C-¼"               '°vterm
    "C-¤"               '°eshell)

  (general-def-leader
    :keymaps            'override
    :states             'motion
    "<tab>"             'evil-switch-to-windows-last-buffer)

  ;; global F-key binds
  (general-def
    :keymaps            'override
    "<f1>"              'eww
    "S-<f1>"            (general-lambda (°split-window-and-do (call-interactively 'eww)))
    "<f2>"              'mu4e
    "S-<f2>"            (general-lambda (°split-window-and-do (mu4e)))
    "<f3>"              'telega
    "S-<f3>"            (general-lambda (°split-window-and-do (telega)))
    "<f12>"             '°straight-update
    "M-<f12>"           'restart-emacs
    "S-M-<f12>"         (general-lambda
                          (shell-command "merge-configs")
                          (restart-emacs)))

  ;; normal state keybinds
  (general-def
    :states         'normal
    "<escape>"      (general-lambda
                      (evil-ex-nohighlight)
                      (evil-force-normal-state))
    "C-a"           'evil-numbers/inc-at-pt
    "C-x"           'evil-numbers/dec-at-pt
    "ö"             '°evil-dry-open-below
    "Ö"             '°evil-dry-open-above
    "_"             'goto-last-change
    "-"             'goto-last-change-reverse)

  (general-def-leader
    :states         'normal
    "RET"           'quickrun
    "P"             '°evil-paste-with-newline-above
    "p"             '°evil-paste-with-newline-below)

  (general-def-goleader
    :states         'normal
    "\""            'counsel-evil-registers
    "C-s"           'vr/replace
    "C-S-s"         'vr/query-replace
    "r"             nil
    "r"             'evil-replace-with-register
    "G"             'magit-status
    "c"             'evil-commentary)

  (general-def-goleader
    :states         'motion
    "s"             '°toggle-scratch-buffer
    "S"             (general-lambda
                      (°split-window-and-do
                       (°toggle-scratch-buffer)))
    "/"             'evil-ex-search-forward
    "?"             'evil-ex-search-backward
    "I"             'counsel-imenu)

  ;; motion state keybinds
  (general-def
    :states             'motion
    "/"                 'swiper
    "("                 'evil-backward-paragraph
    ")"                 'evil-forward-paragraph
    "+"                 'goto-last-change-reverse
    "-"                 'goto-last-change
    "C-l"               'link-hint-open-link
    "C-S-l"             'link-hint-copy-link
    "M-n"               'evil-mc-make-and-goto-next-match
    "M-N"               'evil-mc-skip-and-goto-next-match
    "M-p"               'evil-mc-skip-and-goto-prev-cursor
    "M-P"               (general-lambda
                          (evil-mc-undo-cursor-at-pos (point))
                          (evil-mc-skip-and-goto-prev-cursor))
    "C-q"               'counsel-projectile-switch-project
    "C-u"               'evil-scroll-up
    "Ä"                 'evil-mc-undo-all-cursors
    "ä"                 'evil-mc-make-all-cursors
    "<escape>"          (general-lambda
                          (evil-ex-nohighlight)
                          (evil-force-normal-state))
    "C-s"               'vr/isearch-forward
    "C-S-s"             'vr/isearch-backward
    "M-H"               'helpful-kill-buffers
    "Q"                 'counsel-projectile-find-file
    "{"                 'evil-backward-sentence-begin
    "}"                 'evil-forward-sentence-begin
    "<S-SPC><S-SPC>"    '°vertigo-reuse-last-arg)

  (general-def-leader
    :states         'motion
    "rc"            (general-lambda
                      (find-file (expand-file-name "init.el" user-emacs-directory)))
    "rf"            'counsel-recentf
    "hx"            'helpful-at-point
    "hf"            'helpful-callable
    "hF"            'helpful-command
    "hv"            'helpful-variable
    "hk"            'helpful-key
    "hg"            (general-lambda
                      (°split-window-and-do
                       (info "elisp")))
    "hG"            (general-lambda
                      (°split-window-and-do
                       (info-emacs-manual)))
    "hb"            'counsel-descbinds
    "hm"            'describe-mode
    "SPC"           'vertigo-set-digit-argument
    "%"             '°counsel-ag-projectile
    "C-%"           '°counsel-ag-prompt-path
    "b"             (general-lambda
                      (let ((ivy-use-virtual-buffers nil)
                            (ivy-ignore-buffers (cons "^\*.+?\*$" ivy-ignore-buffers)))
                        (counsel-switch-buffer)))
    "B"             (general-lambda
                      (let ((ivy-use-virtual-buffers t))
                        (counsel-switch-buffer)))
    "k"             'kill-this-buffer
    "K"             'kill-buffer-and-window
    "v"             'evil-window-split
    "s"             'evil-window-vsplit
    "S"             (general-lambda
                      (evil-window-vsplit) (evil-window-right 1))
    "V"             (general-lambda
                      (evil-window-split) (evil-window-down 1))
    "q"             'find-file
    "Q"             '°sudo-find-file
    "Yn"            'yas-new-snippet
    "Ye"            'yas-visit-snippet-file
    "Yi"            'yas-insert-snippet
    "Yt"            'yas-describe-tables
    "/"             'swiper-all)

  ;; visual keybinds
  (general-def
    :states         'visual
    "M-n"           'evil-mc-make-and-goto-next-match
    "M-p"           'evil-mc-skip-and-goto-prev-cursor
    "*"             (lambda (count)
                      (interactive "P")
                      (°evil-search-visual-selection 'forward count))
    "#"             (lambda (count)
                      (interactive "P")
                      (°evil-search-visual-selection 'backward count)))

  (general-def-leader
    :states         'visual
    "RET"            'quickrun-region)


  ;; insert keybinds
  (general-def
    :states         'insert
    "C-n"           nil
    "C-p"           nil
    "C-a"           'move-beginning-of-line
    "C-e"           'move-end-of-line
    "C-S-f"         'forward-word
    "C-S-b"         'backward-word
    "<backtab>"     'indent-relative
    "C-j"           'newline)

  ;; isearch keybinds
  (general-def
    :keymaps        'isearch-mode-map
    "C-S-s"         'isearch-repeat-backward)

  ;;  evil-ex and minibuffer keybinds
  (general-def
    :keymaps        '(evil-ex-completion-map evil-ex-search-keymap read-expression-map
                                             minibuffer-local-map)
    "C-h k"         'helpful-key
    "C-a"           'move-beginning-of-line
    "C-e"           'move-end-of-line
    "C-f"           'forward-char
    "C-b"           'backward-char
    "C-S-f"         'forward-word
    "C-S-b"         'backward-word
    "C-d"           'delete-char
    "C-S-d"         'kill-word
    "M-k"           'previous-line-or-history-element
    "M-j"           'next-line-or-history-element
    "C-v"           'yank
    "C-M-v"         'yank-pop
    "<escape>"      'minibuffer-keyboard-quit)

  ;; simple escape for multiple modes
  (general-def
    :states         'normal
    :keymaps        '(helpful-mode-map
                      flycheck-error-list-mode-map godoc-mode-map
                      quickrun--mode-map magit-mode-map xref--xref-buffer-mode-map
                      telega-image-mode-map)
    "q"             'quit-window)

  (general-def
    :states         'normal
    :keymaps        'view-mode-map
    "q"             'View-quit)
  ;; i don't know why this is necessary...?
  (add-hook 'view-mode-hook (lambda ()
                              (use-local-map view-mode-map)))
  

  ;; workaround for disabling evil-mc-key-map
  (general-def
    :states         '(normal motion visual)
    :keymaps        'evil-mc-key-map
    "gr"            nil
    "C-p"           nil
    "C-n"           nil
    "M-N"           nil
    "M-n"           nil
    "M-P"           nil
    "M-p"           nil)

  ;;; keymap/mode-specific keybinds:
  ;; company keybinds
  (general-def
    :keymaps        'company-active-map
    "<tab>"         nil
    "<return>"      (general-lambda
                      (unless (company-tooltip-visible-p)
                          (company-complete)
                      (company-pseudo-tooltip-hide))
                      (newline 1 t))
    "M-<return>"    (general-lambda
                      (company-abort)
                      (newline 1 t))
    "C-n"           '°company-select-next
    "C-p"           '°company-select-previous)

  (general-def
    :states         'insert
    :keymaps        'company-search-map
    "<return>"      (general-lambda
                      (company-complete)
                      (company-pseudo-tooltip-hide)
                      (newline 1 t)))

  ;; dired keybinds
  (general-def
    :keymaps        'dired-mode-map
    "SPC"           nil
    "t"             '°dired-mark-toggle
    "T"             'dired-toggle-marks)

  ;; eglot keybinds
  (general-def-leader
    :states         'motion
    :keymaps        'eglot-mode-map
    "hx"            'eglot-help-at-point)
  
  (general-def-goleader
    :states         'motion
    :keymaps        'eglot-mode-map
    "d"             'xref-find-definitions
    "="             'eglot-format-buffer
    "*"             'xref-find-references)

  (general-def-goleader
    :states          'visual
    :keymaps         'eglot-mode-map
    "="              'eglot-format)

  ;; eshell keybinds (eshell-mode-keymap is buffer-local and only gets
  ;; initialized after eshell is started - why?)
  (defun °eshell-set-keys ()
    (general-def
      :states           'insert
      :keymaps          'eshell-mode-map
      "<return>"        'eshell-send-input
      "M-k"             'eshell-previous-matching-input-from-input
      "M-j"             'eshell-next-matching-input-from-input)

    (general-def
      :states           'normal
      "^"               'eshell-bol
      "S"               (general-lambda
                          (eshell-bol)
                          (kill-line)
                          (evil-insert-state))))
  (add-hook 'eshell-first-time-mode-hook '°eshell-set-keys)

  ;; ewwwwwwwww keybinds
  (general-def
    :states             'motion
    :keymaps            'eww-mode-map
    "C-o"               'eww-back-url
    "C-i"               'eww-forward-url
    "o"                 'eww)

  ;; edebug keybinds
  (general-def
    :states         'emacs
    :keymaps        'edebug-mode-map
    "SPC"           'edebug-step-mode)

  ;; helpful keybindings
  (general-def
    :states         'motion
    :keymaps        'helpful-mode-map
    "C-o"           'backward-button
    "C-i"           'forward-button)

  ;; ivy keybinds
  (general-def
    :keymaps        'ivy-minibuffer-map
    "<S-return>"    'ivy-call
    "<C-return>"    'ivy-immediate-done
    "<escape>"      'keyboard-escape-quit
    "C-S-p"         'ivy-beginning-of-buffer
    "C-S-n"         'ivy-end-of-buffer
    "C-u"           'ivy-scroll-down-command
    "C-d"           'ivy-scroll-up-command
    "M-k"           'ivy-previous-history-element
    "M-j"           'ivy-next-history-element)

  (general-def
    :keymaps        'ivy-switch-buffer-map
    "C-k"           'ivy-switch-buffer-kill)


  ;; fish-mode keybinds
  (general-def-leader
    :states         'normal
    :keymaps        'fish-mode-map
    "hx"            'man-follow)

  ;; Info-mode keybinds
  (general-def
    :states         'motion
    :keymaps        'Info-mode-map
    "p"             'Info-prev
    "n"             'Info-next
    "m"             'Info-menu
    "K"             'Info-up
    "q"             'kill-buffer-and-window)

  (general-def-goleader
    :states         'motion
    :keymaps        'Info-mode-map
    "n"             'Info-goto-node
    "g"             'evil-goto-first-line)

  ;; flycheck-mode keybinds
  (general-def
    :states         'normal
    :keymaps        'flymake-mode-map
    "M--"           'flymake-goto-next-error
    "M-_"           'flymake-goto-prev-error)

  (general-def-goleader
    :states         'normal
    :keymaps        'flymake-mode-map
    "!"             'flymake-diagnostic-buffer)

  ;; flymake diagnostics buffer keybinds
  (general-def
    :states         'normal
    :keymaps        'flymake-diagnostics-buffer-mode-map
    "j"             'next-line
    "k"             'previous-line
    "RET"           'flymake-goto-diagnostic)

  ;; go-mode keybinds
  (general-def-leader
    :states          'normal
    :keymaps         'go-mode-map
    "ci"             'go-import-add)

  ;; (emacs-)lisp keybindings
  (general-def
    :states         'normal
    :keymaps        'lisp-mode-shared-map
    "D"             'evil-cp-delete-line
    "C"             'evil-cp-change-line
    "c"             'evil-cp-change
    "d"             'evil-cp-delete
    "S"             'evil-cp-change-whole-line
    "^"             '°evil-lisp-first-non-blank
    "A"             '°evil-lisp-append-line
    "I"             '°evil-lisp-insert-line
    "o"             '°evil-lisp-open-below
    "O"             '°evil-lisp-open-above)

  (general-def
    :states         'visual
    :keymaps        'lisp-mode-shared-map
    "c"             'evil-cp-change)

  (general-def-leader
    :states         'motion
    :keymaps        'lisp-mode-shared-map
    "e"             '°eval-at-point
    "E"             '°eval-line
    "M-e"           'eval-buffer
    "C-e"           'eval-defun)

  ;; markdown-mode keybinds
  (general-def-leader
    :states          'normal
    :keymaps         'flymd-map
    "RET"            'flymd-flyit)

  ;; magit keybindings
  (general-def
    :states         'emacs
    :keymaps        'magit-mode-map
    "j"             'magit-section-forward
    "k"             'magit-section-backward
    "p"             'magit-push
    "d"             'magit-delete-thing
    "D"             'magit-diff
    "Ju"            'magit-jump-to-unstaged
    "Js"            'magit-jump-to-staged
    "Jn"            'magit-jump-to-untracked
    "Jz"            'magit-jump-to-stashes
    "Jt"            'magit-jump-to-tracked)

  ;; messages mode

  ;; mu4e keybindings
  (general-def
    :states         'emacs
    :keymaps        'mu4e-main-mode-map
    "J"             'mu4e~headers-jump-to-maildir)

  (general-def-leader
    :states         'emacs
    :keymaps        'mu4e-main-mode-map
    "U"             '°mu4e-main-update-all)

  (general-def
    :states         'emacs
    :keymaps        'mu4e-headers-mode-map
    "q"             '°mu4e-headers-quit-view-or-headers
    "J"             'mu4e~headers-jump-to-maildir
    "j"             'mu4e-headers-next
    "k"             'mu4e-headers-prev
    "E"             '°mu4e-compose-edit-anything
    "G"             'evil-goto-line
    "C-j"           'mu4e-headers-next-unread
    "C-k"           'mu4e-headers-prev-unread
    "C-d"           (general-lambda
                      (evil-scroll-down 0))
    "C-u"           (general-lambda
                      (evil-scroll-up 0))
    "/"             'mu4e-headers-search-edit
    "S"             'mu4e-headers-change-sorting
    "<tab>"         'mu4e-headers-toggle-include-related
    "<backtab>"     'mu4e-headers-toggle-threading
    "t"             '°mu4e-headers-mark-toggle
    "%"             'mu4e-headers-mark-pattern
    "T"             '°mu4e-headers-mark-pattern
    "D"             (general-lambda (°mu4e-headers-handle-deferred 'trash))
    "M"             (general-lambda (°mu4e-headers-handle-deferred 'move))
    "V"             '°mu4e-search-sender
    "$"             (general-lambda
                      (mu4e-mark-execute-all t)))

  (general-def-leader
    :states         'emacs
    :keymaps        'mu4e-headers-mode-map
    "%"             'mu4e-headers-mark-pattern
    "/"             'mu4e-headers-search-narrow
    "d"             'mu4e-headers-mark-for-delete)

  (general-def-goleader
    :states         'emacs
    :keymaps        'mu4e-headers-mode-map
    "g"             'evil-goto-first-line)

  (general-def
    :states         'emacs
    :keymaps        'mu4e-view-mode-map
    "C-d"           'scroll-up-command
    "C-u"           'scroll-down-command
    "E"             '°mu4e-compose-edit-anything
    "k"             'mu4e-scroll-down
    "j"             'mu4e-scroll-up
    "f"             'link-hint-open-link
    "n"             'mu4e-view-headers-next
    "p"             'mu4e-view-headers-prev
    "/"             'swiper
    "gg"            'evil-goto-first-line
    "G"             'evil-goto-line
    "t"             '°mu4e-view-mark-toggle
    "T"             'mu4e-view-mark-pattern
    "V"             '°mu4e-search-sender
    "%"             '°mu4e-view-mark-pattern
    "$"             (general-lambda
                      (mu4e~view-in-headers-context
                       (mu4e-mark-execute-all t))))

  (general-def-leader
    :states 'emacs
    :keymaps 'mu4e-view-mode-map
    "/"      'mu4e-view-search-narrow
    "d"      'mu4e-view-mark-for-delete)
  
  (general-def-leader
    :states     'normal
    :keymaps    'mu4e-compose-mode-map
    "!"         'message-send-and-exit
    "K"         'mu4e-message-kill-buffer
    "d"         (general-lambda
                  (°ispell-cycle-dicts)
                  (flyspell-buffer))
    "D"         (general-lambda
                  (ispell-change-dictionary)
                  (flyspell-buffer))
    "a"         'mail-add-attachment)

  ;; python and jedi keybinds
  (general-def-leader
    :states         'normal
    :keymaps        'python-mode-map
    "C-$"           'run-python
    "cB"            '°python-remove-breakpoints
    "S-<return>"    (general-lambda
                      (if (string-match-p "^test_" (buffer-file-name))
                          '°python-test
                        'quickrun)))

  (general-def
    :states         'insert
    :keymaps        'inferior-python-mode-map
    "<return>"      'comint-send-input)

  ;; telega keybinds
  (general-def
    :states         'emacs
    :keymaps        'telega-root-mode-map
    "f"             'link-hint-open-link
    "j"             'telega-button-forward
    "k"             'telega-button-backward)

  (general-def
    :states         'insert
    :keymaps        'telega-chat-mode-map
    "<return>"      'telega-chatbuf-input-send
    "C-c C-c"       'kill-this-buffer
    "C-g"           'kill-this-buffer)
  
  (general-def
    :states         'normal
    :keymaps        'telega-chat-mode-map
    "q"             'kill-this-buffer
    "j"             'evil-next-line
    "k"             'evil-previous-line)
  
  (general-def
    :keymaps        'telega-msg-button-map
    "j"             nil
    "k"             nil
    "l"             nil
    "i"             nil) ; don't allow overriding evil binds

  ;; vterm keybinds
  (general-def
    :states         'emacs
    :keymaps        'vterm-mode-map
    "C-h k"         'helpful-key
    "C-c $"         '°vterm)

  (general-def-leader
    :states         'normal
    :keymaps        'lisp-mode-shared-map
    "A"             'evil-append-line
    "I"             'evil-insert-line
    "^"             'evil-first-non-blank
    "o"             'evil-open-below
    "O"             'evil-open-above
    "p"             '°evil-lisp-paste-with-newline-below
    "P"             '°evil-lisp-paste-with-newline-above)

  (general-def-leader
    :states         'visual
    :keymaps        'lisp-mode-shared-map
    "e"             '°eval-visual-region)

  ;; visual regexp keybinds
  (general-def
    :keymaps        'vr/minibuffer-keymap
    "<escape>"      'minibuffer-keyboard-quit)

  ;; yasnippet keybinds
  ;; yas-maybe-expand must be bound after the package is loaded because it's a var
  (general-def
    :keymaps        '(yas-keymap yas/keymap)
    "M-j"           'yas-next-field-or-maybe-expand
    "M-k"           'yas-prev-field
    "M-S-j"         'yas-skip-and-clear-field)

  (general-def-leader
    :keymaps        'snippet-mode-map
    :states         'normal
    "YY"            'yas-load-snippet-buffer-and-close
    "Yy"            'yas-load-snippet-buffer))

(provide 'init-keybinds)
