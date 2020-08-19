;; -*- lexical-binding: t -*-
(use-package org
  :commands org-mode
  :config
  (setq org-log-done 'time)
  (add-hook 'org-mode-hook (lambda ()
                             (setq evil-auto-indent nil))))

(use-package evil-org
  :after org)

(provide 'init-org-mode)
