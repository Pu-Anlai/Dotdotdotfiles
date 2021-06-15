;; -*- lexical-binding: t -*-

;; language specific major modes and their settings
;; elisp helpers
(use-package evil-cleverparens
  :after evil-surround
  :commands (evil-cp-delete
             evil-cp-delete-line
             evil-cp-change
             evil-cp-change-line
             evil-cp-change-whole-line)
  :config
  (evil-cp--enable-surround-operators))

(use-package pcre2el
  :defer t)

(use-package suggest
  :commands suggest)

;; shell scripting
;; make shell scripts executable after save if they include a shebang
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(use-package fish-mode
  :defer t
  :config
  (setq fish-enable-auto-indent t))

(use-package pkgbuild-mode
  :commands pkgbuild-mode)

;; latex
(use-package tex
  :straight auctex
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  :config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-auctex-init))

;; markdown
(use-package markdown-mode
  :defer t)

(use-package flymd
  :after markdown-mode
  :config
  (setq flymd-output-directory temporary-file-directory))

;; python settings
(add-hook
 'python-mode-hook
 (lambda ()
   ;; auto-fill
   (auto-fill-mode)
   (setq-local comment-auto-fill-only-comments t)
   (setq python-fill-docstring-style 'symmetric)
   ;; ;; width settings
   (setq-local fill-column 79)
   (setq-local column-enforce-column 79)
   (setq-local electric-pair-open-newline-between-pairs nil)
   (make-local-variable 'write-file-functions)
   (add-to-list 'write-file-functions (°nillify-func (eglot-format-buffer)))))

;; golang settings
(use-package go-mode
  :commands go-mode
  :config
  (add-hook
   'go-mode-hook
   (lambda ()
     (make-local-variable 'write-file-functions)
     (add-to-list 'write-file-functions (°nillify-func (eglot-format-buffer))))))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

(provide 'init-language-specific)
