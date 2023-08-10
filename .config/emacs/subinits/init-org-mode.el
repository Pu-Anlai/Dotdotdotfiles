;; -*- lexical-binding: t -*-
(use-package org
  :commands org-mode
  :general
  (:states          'normal
   :keymaps         'org-mode-map
   "("              'org-backward-element
   ")"              'org-forward-element
    "o"             '°org-meta-open-below
    "O"             '°org-meta-open-above
    "M-o"           '°org-meta-ctrl-open-below
    "M-O"           '°org-meta-ctrl-open-above)
  (general-leader
    :states         'normal
    :keymaps        'org-mode-map
    "o"             'evil-open-below
    "O"             'evil-open-above
    "t"             'org-todo
    "d"             'org-deadline
    "s"             'org-schedule
    "H"             'org-metaleft
    "L"             'org-metaright
    "J"             'org-metadown
    "K"             'org-metaup)
  (general-goleader
    :states         'motion
    :keymaps        'org-mode-map
    "A"             'org-agenda)
  ;; keybindings for agenda-view
  (:states          'normal
   :keymaps         'org-agenda-mode-map
   "("              'org-agenda-earlier
   ")"              'org-agenda-later
   "r"              'org-agenda-redo
   "q"              'org-agenda-quit)
  (general-leader
    :states         'normal
    :keymaps        'org-agenda-mode-map
    "t"             'org-agenda-todo)
  :config
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-collection-org-setup)
  (setq org-log-done 'time
        org-adapt-indentation t)

  (defun °org-meta-open-below ()
  (interactive)
  (if (org-at-item-p)
      (org-end-of-item)
    (org-end-of-line))
    (while (org-fold-folded-p)
      (forward-char))
    (org-meta-return)
  (evil-insert 1))

  (defun °org-meta-open-above ()
    (interactive)
    (if (org-at-item-p)
        (org-beginning-of-item)
      (org-beginning-of-line))
    (org-meta-return)
    (evil-insert 1))

  (defun °org-meta-ctrl-open-below ()
    (interactive)
    (evil-with-single-undo
      (°org-meta-open-below)
      (when (org-at-heading-p)
        (°°org-insert-todo-keyword))))
  
  (defun °org-meta-ctrl-open-above ()
    (interactive)
    (evil-with-single-undo
      (°org-meta-open-above)
      (when (org-at-heading-p)
        (°°org-insert-todo-keyword))))

  (defun °°org-insert-todo-keyword ()
    (org-todo (if (member "TODO" org-todo-keywords-1)
                  "TODO"
              (car org-todo-keywords-1))))

  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'org-fold-hide-drawer-all))

(provide 'init-org-mode)
