;; ido mode

(use-package ido
  :ensure
  :config

  (setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
  (ido-mode t)
)


(use-package ido-yes-or-no
  :ensure
  :requires (ido)

  :config

  (setq ido-yes-or-no-mode t)
)
