;; -*- lexical-binding: t -*-
(use-package magit
  :commands magit-status
  :hook ((magit-mode . my/source-ssh-env)
         (with-editor-mode . evil-insert-state))
  :config
  (defun my/force-git-access ()
    (interactive)
    (let ((index-file (concat
                       (projectile-project-root) (file-name-as-directory ".git") "index.lock")))
      (when (yes-or-no-p (concat "Really delete " index-file "?"))
        (delete-file index-file)))))

(use-package projectile
  :hook (prog-mode . projectile-mode))

(use-package quickrun
  :commands quickrun
  :config
  (setq quickrun-focus-p nil))


(provide 'init-general-programming)
