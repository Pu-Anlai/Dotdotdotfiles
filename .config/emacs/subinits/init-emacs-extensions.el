;; -*- lexical-binding: t -*-
;; carry history variables across sessions
(savehist-mode)

;; set up default browser
(setq browse-url-generic-program "qutebrowser")
(setq browse-url-browser-function 'browse-url-generic)

;; eshell settings
(use-package eshell
  :init 
  ;; for some reason eshell-mode-map is buffer-local... - why?
  (defun °eshell-setup-keys ()
    (general-define-key
     :states        'insert
     :keymaps       'eshell-mode-map
     "<return>"     'eshell-send-input
     "M-k"          'eshell-previous-matching-input-from-input
     "M-j"          'eshell-next-matching-input-from-input)

    (general-define-key
     :states        'normal
     :keymaps       'eshell-mode-map
     "^"            'eshell-bol
     "S"            (general-l
                      (eshell-bol)
                      (kill-line)
                      (evil-insert-state))))

  :hook (eshell-first-time-mode . °eshell-setup-keys)

  :general
  (:keymaps         'override
   :states          '(motion emacs)
   "C-¤"            '°eshell)

  :config
  (setq eshell-banner-message "")
  (add-hook 'eshell-exit-hook (lambda ()
                                (when
                                    (string= (buffer-name (window-buffer (selected-window)))
                                             "*eshell*")
                                  (delete-window)))))

;; ewwwwwwwwwwwwwwwwwww settings
(use-package eww
  :general
  (:keymaps         'override
   "<f1>"           'eww
   "S-<f1>"         (general-l (°split-window-and-do (call-interactively 'eww))))
  (:states          'motion
   :keymaps         'eww-mode-map
   "C-o"            'eww-back-url
   "C-i"            'eww-forward-url
   "o"              'eww))

;; spellchecking settings
(setq ispell-program-name "hunspell")
(defvar °ispell-dicts-in-use
  '("de_DE" "en_AU")
  "List of dicts to cycle through by using °ispell-cycle-dicts.")

;; use more conservative sentence definition
(setq sentence-end-double-space nil)

;; tramp settings
(setq tramp-default-method "ssh")
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (°source-ssh-env))))

;; use pass or an encrypted file for auth-sources
(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

;; sexier builtin help
(use-package helpful
  :init
  (defun °helpful-previous-buffer ()
    (interactive)
    (let ((switch-to-prev-buffer-skip
           (lambda (window buffer bury-or-kill)
             (message (buffer-name buffer))
             (not (string-match-p "^*helpful" (buffer-name buffer))))))
      (call-interactively #'previous-buffer)
      (helpful-update)))
  :general
  (:keymaps         'motion
   "M-H"            'helpful-kill-buffers)
  (general-leader
    :keymaps        'motion
    "hx"            'helpful-at-point
    "hf"            'helpful-callable
    "hF"            'helpful-command
    "hv"            'helpful-variable
    "hk"            'helpful-key)
  (:keymaps         '(evil-ex-completion-map
                      evil-ex-search-keymapread-expression-map
                      minibuffer-local-map)
   "C-h k"          'helpful-key)
  (:states          'normal
   :keymaps         'helpful-mode-map
   "q"              'quit-window)
  (:states          'motion
   :keymaps         'helpful-mode-map
   "q"              'quit-window
   "C-o"            '°helpful-previous-buffer
   "C-i"            'next-buffer)

  :config
  (setq helpful-switch-buffer-function '°helpful-buffer-other-window)
  (setq helpful-max-buffers 2)

  ;; helpful related functions
  (defun °helpful-buffer-other-window (buf)
    "Display helpful buffer BUF the way I want it, ie:
Replace buffer/window if in helpful-mode, lazy-open otherwise."
    (let (sw)
      (if (eq major-mode 'helpful-mode)
          (progn
            (quit-window)
            (pop-to-buffer buf))
        (progn (setq sw (selected-window))
               (switch-to-buffer-other-window buf)))
      (helpful-update)
      (when sw (select-window sw)))))

;; vimperator-style link-hints
(use-package link-hint
  :general
  (:states          'motion
   "C-l"            'link-hint-open-link
   "C-S-l"          'link-hint-copy-link))

;; use recentf mode to keep file visiting history
(use-package recentf
  :general
  (general-leader
    :keymaps        'normal
    "rf"            'consult-recent-file)
  :init
  (recentf-mode))

(use-package restart-emacs
  :general
  (:keymaps         'override
   "M-<f12>"        'restart-emacs
   "S-M-<f12>"      (general-l
                      (shell-command "merge-configs")
                      (restart-emacs))))

(use-package pcre2el
  :after visual-regexp-steroids)

(use-package visual-regexp)

(use-package visual-regexp-steroids
  :general
  (general-goleader
    :keymaps        'normal
    "C-s"           'vr/replace
    "C-S-s"         'vr/query-replace)
  (:keymaps         'vr/minibuffer-keymap
   "<escape>"       'minibuffer-keyboard-quit))

;; use locally installed package (from AUR) of emacs-vterm
(use-package vterm
  :straight nil
  :general
  (:keymaps         'override
   :states          '(motion emacs)
   "C-¼"            '°vterm)
  (:states          'emacs
   :keymaps         'vterm-mode-map
   "C-h k"          'helpful-key
   "C-c $"          '°vterm)

  :config
  (defun °vterm ()
    "Hide or show vterm window.
Start terminal if it isn't running already."
    (interactive)
    (let* ((vterm-buf "*vterm*")
           (vterm-win (get-buffer-window vterm-buf)))
      (if vterm-win
          (progn
            (select-window vterm-win)
            (delete-window))
        (if (get-buffer vterm-buf)
            (pop-to-buffer vterm-buf)
          (vterm-other-window)))))

  ;; delete vterm window on exit
  (add-hook 'vterm-exit-functions
            (lambda (buf event)
              (delete-window (get-buffer-window buf)))))


(provide 'init-emacs-extensions)
