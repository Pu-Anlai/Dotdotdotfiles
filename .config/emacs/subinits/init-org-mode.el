;; -*- lexical-binding: t -*-
(use-package org
  :commands org-mode
  :config
  (setq org-log-done 'time)
  (add-hook 'org-mode-hook #'visual-line-mode))

(provide 'init-org-mode)
