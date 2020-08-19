;; -*- lexical-binding: t -*-
;; GUI and Highlighting settings
(setq inhibit-startup-message t)
(fringe-mode '(0 . 8))
(scroll-bar-mode -1)
(setq scroll-step 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil)
(setq echo-keystrokes .01)
(setq eldoc-idle-delay .45)
(setq-default fill-column 80)
(dolist (hook '(prog text conf telega-root))
  (add-hook
   (intern (concat (symbol-name hook) "-mode-hook"))
   #'hl-line-mode))

;; window splitting settings
(setq split-width-threshold 100)
;; WHY is vertical splitting preferred over horizontal?
(setq split-window-preferred-function '°split-window-sensibly)

;; keep track of window layout changes
(defun °°first-push-to-window-layout-stack (&rest args)
  (unless (eql (count-windows) 1)
    (°°window-layout-stack-push)))
;; only add this advice to high-level functions; otherwise it will be called
;; multiple times for each time the parent function calls the lower-level one
(advice-add #'delete-other-windows :before #'°°first-push-to-window-layout-stack)
(advice-add #'evil-window-delete :before #'°°first-push-to-window-layout-stack)

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda ()
                   (setq display-line-numbers 'relative
                         display-line-numbers-widen t
                         display-line-numbers-current-absolute t))))

;; highlight cursor when scrolling
(use-package beacon
  :hook ((prog-mode text-mode conf-mode) . beacon-mode)
  :config
  (setq beacon-color (concat "#" (getenv "__BASE0C"))
        beacon-blink-when-window-changes nil
        beacon-blink-when-buffer-changes nil
        beacon-blink-delay 0.1
        beacon-blink-duration 0.25
        beacon-size 20
        beacon-push-mark nil)
  (setq beacon-dont-blink-major-modes '(mu4e-main-mode
                                        mu4e-view-mode
                                        mu4e-headers-mode
                                        vterm-mode))
  (setq beacon-dont-blink-commands '(evil-ex-search-next
                                     evil-ex-search-previous
                                     exit-minibuffer
                                     previous-line
                                     next-line))
  (add-hook 'focus-in-hook
            (defun °beacon-blink-upon-refocus ()
              (unless (member major-mode beacon-dont-blink-major-modes)
                (beacon-blink)))))

;; delimiter highlighting and matching
(setq electric-pair-open-newline-between-pairs t)
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'electric-pair-mode))

;; modeline
(use-package telephone-line
  :config
  (telephone-line-defsegment telephone-line-my-projectile-segment ()
    (cond
     ((not (buffer-file-name)) ; buffer is not a file
      "Ø")
     ((and (fboundp 'projectile-project-name)
           (projectile-project-p)) ; buffer is part of a project
      (let*
          ((branch (car (vc-git-branches)))
           (branch-str (unless (string= branch "master")
                         (concat "[" branch "]"))))
        (concat (projectile-project-name) branch-str)))
     (t ; buffer is a file but not part of a project
      (file-name-base (directory-file-name (file-name-directory (buffer-file-name)))))))

  (telephone-line-defsegment telephone-line-my-buffer-segment ()
    (if (and (buffer-file-name)
             (fboundp 'projectile-project-name)
             (fboundp 'projectile-project-p)
             (projectile-project-p))
        (let*
            ((file-path
              (abbreviate-file-name (file-relative-name buffer-file-name (projectile-project-root))))
             (dir-list (f-split file-path))
             (mode-line-str))

          (if (> (length dir-list) 2)
              (let
                  ((first-part-str
                    (apply #'concat
                           (mapcar (lambda (dir)
                                     (file-name-as-directory
                                      (if (> (length dir) 4)
                                          (concat (substring dir 0 2)
                                                  (substring dir -2))
                                        dir)))
                                   (cl-subseq dir-list 0 (- (length dir-list) 2)))))
                   (last-part-str
                    (concat
                     (file-name-as-directory (car (last dir-list 2)))
                     (car (last dir-list)))))
                (setq mode-line-str (concat first-part-str last-part-str)))
            (setq mode-line-str file-path))

          (if (buffer-modified-p)
              (propertize (concat mode-line-str "!") 'face 'telephone-line-warning)
            mode-line-str))
      (buffer-name)))

  (setq telephone-line-lhs
        '((evil      . (telephone-line-evil-tag-segment))
          (accent    . (telephone-line-my-projectile-segment))
          (nil       . (telephone-line-my-buffer-segment))))

  (setq telephone-line-rhs
        '((nil       . (telephone-line-misc-info-segment))
          (accent    . (telephone-line-major-mode-segment))
          (evil      . (telephone-line-airline-position-segment))))
  (setq telephone-line-primary-left-separator     'telephone-line-cubed-left
        telephone-line-secondary-left-separator   'telephone-line-abs-left
        telephone-line-primary-right-separator    'telephone-line-cubed-right
        telephone-line-secondary-right-separator  'telephone-line-abs-right)
  (telephone-line-mode t))

(push (expand-file-name "themes" user-emacs-directory) custom-theme-load-path)
(load-theme 'base16-generic t)
(push `(font . ,(concat (or (getenv "FONT_MONO") "Monospace") " 11")) default-frame-alist)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package shackle
  :config
  (shackle-mode 1)
  (setq shackle-rules
        '(("*eshell*"
           :regexp t :select t :popup t :align below :size 0.2)
          ("^\\*ansi-term.*"
           :regexp t :select t :popup t :align below :size 0.2)
          ('inferior-python-mode
           :select t :popup t :align below :size 0.2)
          ('vterm-mode
           :select t :popup t :align below :size 0.2))))

(provide 'init-gui-setup)
