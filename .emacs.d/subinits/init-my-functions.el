;; -*- lexical-binding: t -*-

(require 'cl-lib)

;; macros
(defmacro my/flet (bindings &rest body)
  "Like flet but using cl-letf and therefore not deprecated."
  `(cl-letf ,(mapcar
              (lambda (binding)
                `((symbol-function (quote ,(car binding)))
                  ,@(cdr binding)))
              bindings)
     ,@body))

(defmacro my/split-window-and-do (&rest funcs)
  `(progn
     (ignore-errors
       (select-window (funcall split-window-preferred-function)))
     ,@funcs))

(defmacro my/nillify-func (&rest funcs)
  "Return a function that runs FUNCS but always returns nil."
  `(lambda ()
     ,@funcs
     nil))

(defmacro my/defun-newline-paste (func-name &rest open-funcs)
  "Create a function that pastes after opening lines with OPEN-FUNCS."
  `(defun ,func-name (count)
     (interactive "p")
     (evil-with-single-undo
       (while (> (setq count (1- count)) -1)
         (evil-save-state
           ,@open-funcs)
         (evil-paste-after 1)
         (indent-according-to-mode)))))

;; functions
(defun my/add-hook-to-mode (hook function mode &optional append)
  "Add FUNCTION to HOOK but limit it to MODE.  See `add-hook' for option APPEND."
  (add-hook (my/concat-symbols mode '-hook)
            (lambda ()
              (add-hook hook function append t))))

(defun my/concat-symbols (&rest symbols)
  "Concatenate SYMBOLS together to form a single symbol."
  (intern (apply #'concat (mapcar #'symbol-name symbols))))

(defun my/dired-mark-toggle ()
  "Toggle mark for currently selected file."
  (interactive)
  (let ((inhibit-read-only t))
    (when (not (dired-between-files))
      (save-excursion
        (beginning-of-line)
        (apply 'subst-char-in-region
               (point) (1+ (point))
               (if (eq (following-char) ?\040)
                   (list ?\040 dired-marker-char)
                 (list dired-marker-char ?\040)))))))

(defun my/eshell ()
  "Hide or show eshell window.
Start eshell if it isn't running already."
  (interactive)
  (if (get-buffer-window "*eshell*")
      (progn
        (select-window (get-buffer-window "*eshell*"))
        (delete-window))
    (eshell)))

(defun my/eval-visual-region ()
  "Evaluate region."
  (interactive)
  (when (> (mark) (point))
    (exchange-point-and-mark))
  (eval-region (mark) (point) t)
  (ignore-errors
   (evil-normal-state)))

(defun my/eval-line ()
  "Evaluate current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (eval-last-sexp nil)))

(defun my/eval-at-point ()
  "Move out to closest sexp and evaluate."
  (interactive)
  (let ((point-char (thing-at-point 'char))
        (reg-start)
        (reg-end))
    (save-excursion
      (while (not (or (string= point-char "(")
                      (string= point-char ")")))
        (ignore-errors
            (backward-sexp))
          (backward-char)
        (setq point-char (thing-at-point 'char)))
      (if (string= point-char "(")
          (setq reg-start (point))
        (setq reg-end (+ (point) 1)))
      (evil-jump-item)
      (if reg-start
          (setq reg-end (+ (point) 1))
        (setq reg-start (point))))
    (eval-region reg-start reg-end t)))

;; evil-related-functions
(defun my/evil-dry-open-below (count)
  "Open LINE number of lines below but stay in current line."
  (interactive "p")
  (save-excursion
    (end-of-line)
    (open-line count)))

(defun my/evil-dry-open-above (count)
  "Open LINE number of lines above but stay in current line."
  (interactive "p")
  ;; this does not work with save-excursion if it's done at the beginning of
  ;; the buffer
  (let ((col (current-column)))
    (beginning-of-line)
    (open-line count)
    (forward-line count)
    (move-to-column col)))

;; lisp related functions
(defun my/evil-lisp-append-line (count)
  (interactive "p")
  (my//evil-lisp-end-of-depth)
  (setq evil-insert-count count)
  (evil-insert-state 1))

(defun my//evil-lisp-end-of-depth ()
  "Go to last point of current syntax depth on the current line."
  ;; if we're on a parens move into its scope
  (unless (eq (length (my/get-line)) 0) ; don't move if on empty line
    (when (and (not (my//in-string-p))
               (or (mapcar #'looking-at '("(" ")"))))
      (forward-char))
    (let ((depth (my//syntax-depth)))
      (end-of-line)
      (while (not (eq depth (my//syntax-depth)))
        (backward-char)))))

(defun my/evil-lisp-insert-line (count)
  (interactive "p")
  (my//evil-lisp-start-of-depth)
  (when (looking-at "\s")
    (my/evil-lisp-first-non-blank))
  (setq evil-insert-count count)
  (evil-insert-state 1))

(defun my/evil-lisp-first-non-blank ()
    (interactive)
  (evil-first-non-blank)
  (while (and (equal (thing-at-point 'char) "(")
              (not (my//in-string-p)))
    (evil-forward-char)))

(defun my/evil-lisp-open-above (count)
  (interactive "p")
  (my//evil-lisp-start-of-depth)
  (save-excursion
    (newline 1)
    (indent-according-to-mode))
  (indent-according-to-mode)
  (setq evil-insert-count count
        evil-insert-lines t)
  (evil-insert-state 1))

(defun my/evil-lisp-open-below (count)
  (interactive "p")
  (my//evil-lisp-end-of-depth)
  (newline 1)
  (indent-according-to-mode)
  (setq evil-insert-count count
        evil-insert-lines t)
  (evil-insert-state 1))

(defun my//evil-lisp-start-of-depth ()
  "Go to first point of current syntax depth on the current line."
  (let ((depth (my//syntax-depth)))
    (evil-beginning-of-line)
    (while (not (eq depth (my//syntax-depth)))
      (evil-forward-char))))

(my/defun-newline-paste
 my/evil-paste-with-newline-above
 (evil-open-above 1))

(my/defun-newline-paste
 my/evil-paste-with-newline-below
 (evil-open-below 1))

(my/defun-newline-paste
 my/evil-lisp-paste-with-newline-above
 (my/evil-lisp-open-above 1))

(my/defun-newline-paste
 my/evil-lisp-paste-with-newline-below
 (my/evil-lisp-open-below 1))
 
(defun my/evil-search-visual-selection (direction count)
  "Search for visually selected text in buffer.
DIRECTION can be forward or backward.  Don't know what COUNT does."
  (when (> (mark) (point))
    (exchange-point-and-mark))
  (when (eq direction 'backward)
    (setq count (+ (or count 1) 1)))
  (let ((regex (format "\\<%s\\>" (regexp-quote (buffer-substring (mark) (point))))))
    (setq evil-ex-search-count count
          evil-ex-search-direction direction
          evil-ex-search-pattern
          (evil-ex-make-search-pattern regex)
          evil-ex-search-offset nil
          evil-ex-last-was-search t)
    ;; update search history unless this pattern equals the
    ;; previous pattern
    (unless (equal (car-safe evil-ex-search-history) regex)
      (push regex evil-ex-search-history))
    (evil-push-search-history regex (eq direction 'forward))
    (evil-ex-delete-hl 'evil-ex-search)
    (evil-exit-visual-state)
    (when (fboundp 'evil-ex-search-next)
      (evil-ex-search-next count))))

(defun my/get-line ()
  "Uniform way to get content of current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun my//in-string-p ()
  "Returns t if point is within a string according to syntax-ppss.  Otherwise nil."
  (not (eq (nth 3 (syntax-ppss)) nil)))

(defun my/ispell-cycle-dicts ()
  "Cycle through the dicts in `my/ispell-dicts-in-use'."
  (interactive)
  (ispell-change-dictionary
   (catch 'dict
     (while t
       (nconc my/ispell-dicts-in-use (list (pop my/ispell-dicts-in-use)))
       (unless (string= ispell-current-dictionary (car my/ispell-dicts-in-use))
         (throw 'dict (car my/ispell-dicts-in-use)))))))

(defun my/join-path (&rest elements)
  "Join ELEMENTS to create a path.  The last element should be the name of a file."
  (let ((file (car (last elements)))
        (folders (butlast elements)))
    (apply #'concat `(,@(mapcar #'file-name-as-directory folders) ,file))))

(defun my/last-name (name)
  (let* ((nlist (reverse (split-string (downcase name))))
         (lname (capitalize (pop nlist)))
         (pres (mapcar #'downcase my/last-name-prefixes))
         (pre (pop nlist))
         (rpre (cl-position pre pres :test #'string=)))
    (if rpre
      (concat (nth rpre my/last-name-prefixes) " " lname)
      lname)))

(defun my/python-remove-breakpoints ()
  "Remove all breakpoint declarations in buffer."
  (interactive)
  (let ((counter 0))
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "^[[:space:]]*breakpoint()[[:space:]]*\n" nil t)
        (replace-match "")
        (setq counter (1+ counter))))
    (message "%s breakpoint%s removed." counter (if (= counter 1) "" "s"))))

(defun my/python-test ()
  "Run pytest."
  (interactive)
  (let ((old-py-path (getenv "PYTHONPATH"))
        (new-py-path (projectile-project-root)))
    (setenv "PYTHONPATH" new-py-path)
    (quickrun :source `((:command . "pytest")
                        (:default-directory . ,new-py-path)
                        (:exec . ("pytest"))))
    (setenv "PYTHONPATH" old-py-path)))

(defun my/restore-window-layout ()
  "Restore window layout that is on top of `my//window-layout-stack'."
  (interactive)
  (let ((layout (pop my//window-layout-stack)))
    (when layout
      (set-window-configuration layout))))

(defun my/source-ssh-env ()
  "Read environment variables for the ssh environment from '~/.ssh/environment'."
  (let (pos1 pos2 (var-strs '("SSH_AUTH_SOCK" "SSH_AGENT_PID")))
    (unless (cl-some 'getenv var-strs)
      (with-temp-buffer
        (ignore-errors
          (insert-file-contents "~/.ssh/environment")
          (mapc
           (lambda (var-str)
             (goto-char 0)
             (search-forward var-str)
             (setq pos1 (+ (point) 1))
             (search-forward ";")
             (setq pos2 (- (point) 1))
             (setenv var-str (buffer-substring-no-properties pos1 pos2)))
           var-strs))))))

(defun my/split-window-sensibly (&optional window)
  "Prefer horizontal splits for state-of-the-art widescreen monitors. Also don't
  split when there's 3 or more windows open."
  (if (or
       (>= (count-windows) 3)
       (> (length (get-buffer-window-list)) 1))
      nil
    (let* ((window (or window (selected-window)))
           (window-size-h (window-size window))
           (window-size-w (window-size window t))
           (frame-size-h (window-size (frame-root-window)))
           (frame-size-w (window-size (frame-root-window) t)))
      (or
       (and
        (> window-size-w split-width-threshold)
        (eq frame-size-w window-size-w)
        (with-selected-window window
          (split-window-right)))
       (and
        (eq frame-size-h window-size-h)
        (with-selected-window window
          (split-window-below)))))))


(defun my/straight-update ()
  "Fetch, merge and rebuild all straight packages."
  (interactive)
  (when (y-or-n-p "Fetch package remotes and rebuild modified packages? ")
    (straight-pull-all)
    (straight-check-all)
    (restart-emacs)))

(defun my/sudo-find-file ()
  "Open 'find-file' with sudo prefix."
  (interactive)
  (let ((default-directory "/sudo::/"))
    (command-execute 'find-file)))

(defun my//syntax-depth ()
  "Return depth at point within syntax tree. "
  (nth 0 (syntax-ppss)))

(defun my/toggle-scratch-buffer ()
  "Go back and forth between scratch buffer and most recent other buffer."
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (evil-switch-to-windows-last-buffer)
    (switch-to-buffer "*scratch*")))

(defun my/window-clear-side ()
  "Clear selected pane from vertically split windows."
  (interactive)
  (cl-flet ((clear
             (direction)
             (while
                 (ignore-errors
                   (funcall (my/concat-symbols 'windmove- direction)))
               (delete-window))))
    (mapc #'clear '(up down))))

(defun my//window-layout-stack-push ()
  (push (current-window-configuration) my//window-layout-stack))

;; variables
(defvar my/last-name-prefixes '("von" "de" "van" "Al")
  "List of possible last name prefixes for `my/last-name' to consider.")

(defvar my//window-layout-stack nil
  "Stack of recently recorded layout changes.")

(provide 'init-my-functions.el)
