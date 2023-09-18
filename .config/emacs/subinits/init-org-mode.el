;; -*- lexical-binding: t -*-
(use-package org
  :commands org-mode
  :general
  (:states          'normal
   :keymaps         'org-mode-map
   "{"              '°org-prev-element
   "}"              '°org-next-element
   "o"              '°org-meta-open-below
   "O"              '°org-meta-open-above)
   (:states         'insert
   :keymaps         'org-mode-map
   "M-L"            'org-metaright
   "M-H"            'org-metaleft)
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
    "K"             'org-metaup
    "X"             'org-toggle-checkbox)
  (general-goleader
    :states         'motion
    :keymaps        'org-mode-map
    "A"             'org-agenda
    "o"             '°org-meta-ctrl-open-below
    "O"             '°org-meta-ctrl-open-above
    "L"             'org-toggle-link-display)
  ;; keybindings for agenda-view
  (:states          'normal
   :keymaps         'org-agenda-mode-map
   "j"              'org-agenda-next-line
   "k"              'org-agenda-previous-line
   "("              'org-agenda-earlier
   ")"              'org-agenda-later
   "r"              'org-agenda-redo
   "q"              'org-agenda-quit
   "RET"            (general-l (org-agenda-show-1 3)))
  (general-leader
    :states         'normal
    :keymaps        'org-agenda-mode-map
    "t"             'org-agenda-todo)
  :config
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-collection-org-setup)
  (setq org-log-done 'time
        org-adapt-indentation t
        org-cycle-separator-lines -1)

  (dolist (func
           #'(°°org-visual-line-mode
              org-fold-hide-drawer-all))
    (add-hook 'org-mode-hook func))

  (°add-hook-to-mode 'before-save-hook
                     (lambda () (org-align-tags t))
                     'org-mode)
  
  (defun °org-prev-element ()
    (interactive)
    (if (> (or (org-current-level) 0) 1)
        (org-up-element)
      (org-backward-element)))

  (defun °org-next-element ()
    (interactive)
    (when (> (or (org-current-level) 0) 1)
      (org-up-element))
    (org-forward-element))

  (defun °org-meta-open-below ()
    (interactive)
    (let ((context-func (cond ((org-at-table-p)
                               (call-interactively #'org-table-end-of-field)
                               #'org-table-wrap-region)
                              ((org-at-item-p)
                               (org-end-of-item)
                               (unless (org-at-item-p)
                                 (previous-line)
                                 (end-of-line))
                               #'org-insert-item)
                              ((org-at-heading-p)
                               (org-end-of-line)
                               #'org-insert-heading-respect-content)
                              (t
                               (lambda () (interactive) (evil-insert-newline-below))))))
      (while (org-fold-folded-p)
        (forward-char))
      (call-interactively context-func)
      (indent-according-to-mode)
      (evil-insert 1)))

  (defun °org-meta-open-above ()
    (interactive)
    (let ((context-func
           (cond ((org-at-table-p)
                  (call-interactively #'org-table-beginning-of-field)
                  #'org-table-insert-row)
                 ((org-at-item-p)
                  (org-beginning-of-item)
                  #'org-insert-item)
                 (t
                  (org-beginning-of-line)
                  #'org-meta-return))))
    (call-interactively context-func)
    (evil-insert 1)))

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

  (defun °°org-undone-children-at-point-p ()
    (save-excursion
      (or
       (not (org-at-heading-p))
       (org-goto-first-child)
       (while (and
               (member (org-get-todo-state) org-done-keywords)
               (org-get-next-sibling))))))
  

  (defun °°org-visual-line-mode ()
    (visual-line-mode)
    (setq-local display-line-numbers 'visual)))

(provide 'init-org-mode)
