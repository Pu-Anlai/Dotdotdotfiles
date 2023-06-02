;; -*- lexical-binding: t -*-
(use-package org
  :commands org-mode
  :general
  (:states          'normal
   :keymaps         'org-mode-map
   "o"              '°evil-org-meta-open-below
   "O"              '°evil-org-meta-open-above)
  :config
  (evil-collection-init 'org)
  (defun °evil-org-meta-open-below (count)
    (interactive "p")
    (if (org-in-item-p)
        (org-end-of-item)
        (end-of-line))
    (org-meta-return)
    (evil-insert count))
  (defun °evil-org-meta-open-above (count)
    (interactive "p")
    (forward-line -1)
    (if (org-in-item-p)
        (org-end-of-item)
      (end-of-line))
    (org-meta-return)
    (evil-insert count))
  (setq org-log-done 'time)
  (add-hook 'org-mode-hook #'visual-line-mode))

(provide 'init-org-mode)
