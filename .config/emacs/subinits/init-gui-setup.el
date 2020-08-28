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
;; only add this advice to high-level functions to avoid infinite recursion
(advice-add #'delete-other-windows :before #'°°first-push-to-window-layout-stack)
(advice-add #'evil-window-delete :before #'°°first-push-to-window-layout-stack)

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda ()
                   (setq display-line-numbers 'relative
                         display-line-numbers-widen t
                         display-line-numbers-current-absolute t))))

;; delimiter highlighting and matching
(setq electric-pair-open-newline-between-pairs t)
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'electric-pair-mode))

;; modeline
;; register all the mode-welt faces (defined in the theme)
(dolist (state '(normal insert visual replace operator motion emacs inactive))
  (custom-declare-face (°concat-symbols 'mode-welt-evil- state) nil nil))

(defvar mode-welt-selected-window (frame-selected-window)
  "Holds the currently selected window to implement custom inactive faces.")

(defun mode-welt-update-selected-window ()
  (while-no-input
    (let ((win (frame-selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq mode-welt-selected-window win)))))

(defun mode-welt-space-barf (length)
  "Return as string consisting only of the number of spaces provided by LENGTH."
  (format (concat "%" (number-to-string length) "s") ""))

(defun mode-welt-align (left right &optional pad-left pad-right)
  "Use the width of the window to align LEFT to the left and RIGHT to the right.
  Return the formatted string. Both values should be lists of strings. PAD-LEFT
  and PAD-RIGHT should be integer values and indicate additional padding on the
  sides of the modeline."
  (let* ((pad-left (or pad-left 0))
         (pad-right (or pad-right 0))
         (str-left (mapconcat #'identity (remq nil left) "  "))
         (str-right (mapconcat #'identity (remq nil right) "  "))
         (lr-lengths (mapcar (lambda (str) (length (format-mode-line str)))
                      (list str-left str-right)))
         (space-size (- (window-total-width) (apply #'+ lr-lengths) (+ pad-left pad-right)))
         (space (mode-welt-space-barf space-size)))
    (concat
     (mode-welt-space-barf pad-left)
     str-left
     space
     str-right
     (mode-welt-space-barf pad-right))))

(defun mode-welt-evil-state-propertized ()
  "Create a string of the current evil state with the respective mode-welt face
  applied."
  (let ((face (if (eq mode-welt-selected-window (frame-selected-window))
                  (°concat-symbols 'mode-welt-evil- evil-state)
                'mode-welt-evil-inactive)))
    (concat
     (propertize "%*%+" 'face `(:inherit ,face :inverse-video t))
     (propertize
     (upcase (symbol-name evil-state))
     'face
     face))))

(defun mode-welt-project-name ()
  (cond
   ((not (buffer-file-name)) ; buffer is not a file
    nil)
   ((and (fboundp 'projectile-project-name)
         (projectile-project-p)) ; buffer is part of a project
    (let*
        ((branch (car (vc-git-branches)))
         (branch-str (unless (string= branch "master")
                       (concat "[" branch "]"))))
      (concat (projectile-project-name) branch-str)))
   (t ; buffer is a file but not part of a project
    (file-name-base (directory-file-name (file-name-directory (buffer-file-name)))))))

(defun mode-welt-buffer-name ()
  (propertize "%+%b%+" 'face (if (buffer-modified-p)
                               'mode-line-highlight
                             'mode-line-emphasis)))

(defun mode-welt-set-mode-line ()
  "Set my modeline."
  (let ((mlf '(:eval (mode-welt-align (list
                                       (mode-welt-evil-state-propertized)
                                       (mode-welt-project-name)
                                       (mode-welt-buffer-name))
                                      (list
                                       (format-mode-line mode-line-misc-info)
                                       mode-name
                                       "C:%c")
                                      0 2))))
    (setq-default mode-line-format mlf)
    (setq mode-line-format mlf)))

;; for keeping track of the selected window in the modeline
(add-hook 'post-command-hook #'mode-welt-update-selected-window)
(add-hook 'emacs-startup-hook #'mode-welt-set-mode-line)

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
