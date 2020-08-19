;; -*- lexical-binding: t -*-
(use-package flx)

(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  ;; don't show lockfiles
  (setq counsel-find-file-ignore-regexp "^.#")
  (ivy-mode 1)
  (counsel-mode 1)

  ;; counsel/ivy related functions
  (defun °counsel-ag-prompt-path ()
    (interactive)
    (let ((ag-root (read-file-name "ag root: ")))
      (counsel-ag nil ag-root))))

(use-package counsel-projectile
  :after (counsel projectile)
  :commands (counsel-projectile-switch-project
             counsel-projectile-find-file))

(provide 'init-ivy)
