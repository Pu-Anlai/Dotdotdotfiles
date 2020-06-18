;; -*- lexical-binding: t -*-
(use-package org
  :commands org-mode
  :config
  (setq org-log-done 'time)
  (add-hook 'org-mode-hook (lambda ()
                             (setq evil-auto-indent nil))))

(provide 'init-org-mode)
