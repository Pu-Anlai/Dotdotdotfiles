;; carry history variables across sessions
(savehist-mode)

;; set up default browser
(setq browse-url-generic-program "qutebrowser")
(setq browse-url-browser-function 'browse-url-generic)

;; eshell settings
(setq eshell-banner-message "")
(add-hook 'eshell-exit-hook (lambda ()
                              (when
                                  (string= (buffer-name (window-buffer (selected-window)))
                                           "*eshell*")
                                (delete-window))))

;; spellchecking settings
(setq ispell-program-name "hunspell")
(defvar my/ispell-dicts-in-use
  '("de_DE" "en_AU")
  "List of dicts to cycle through by using my/ispell-cycle-dicts.")

;; use more conservative sentence definition
(setq sentence-end-double-space nil)

;; tramp settings (so far not many)
(setq tramp-default-method "ssh")
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (my/source-ssh-env))))

;; use pass or an encrypted file for auth-sources
(use-package auth-source-pass
  :defer 5
  :config
  (auth-source-pass-enable))

(use-package all-the-icons
  :defer t)

(use-package f)

;; sexier builtin help
(use-package helpful
  :defer t
  :config
  (setq helpful-switch-buffer-function 'my/helpful-buffer-other-window)
  (setq helpful-max-buffers 2)

  ;; helpful related functions
  (defun my/helpful-buffer-other-window (buf)
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
  :commands link-hint-open-link)

(use-package restart-emacs
  :commands restart-emacs)

(use-package visual-regexp-steroids
  :commands (vr/replace vr/query-replace vr/isearch-forward vr/isearch-backward)
  :after pcre2el
  :config
  (setq vr/engine 'pcre2el))

;; use locally installed package (from AUR) of emacs-vterm
(use-package vterm
  :straight nil
  :commands my/vterm
  :config
  (defun my/vterm ()
    "Hide or show vterm window.
Start terminal if it isn't running already."
    (interactive)
    (let* ((vterm-buf "vterm")
           (vterm-win (get-buffer-window vterm-buf)))
      (if vterm-win
          (progn
            (select-window vterm-win)
            (delete-window))
        (if (get-buffer vterm-buf)
            (pop-to-buffer vterm-buf)
          (vterm-other-window))))))


(provide 'init-emacs-extensions)
