;; -*- lexical-binding: t -*-
(use-package vertico
  :general
  (:keymaps         'vertico-map
    "M-k"           'previous-history-element
    "M-j"           'next-history-element)
  :init
  (setq completion-styles (append completion-styles '(flex))
        completion-ignore-case t)
  (vertico-mode))

(use-package embark
  :general
  (:keymaps         '(motion emacs evil-ex-completion-map
                             evil-ex-search-keymap read-expression-map
                             minibuffer-local-map)
   "C-,"            'embark-act
   "C-;"            'embark-dwim))

(use-package consult
  :general
  (general-leader
    :keymaps        'normal
    "B"             'consult-buffer))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

(use-package embark-consult
  :after (consult embark))

(provide 'init-completion)
