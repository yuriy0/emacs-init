;; ido mode
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(require 'ido)
(ido-mode t)

(require 'ido-yes-or-no)
(setq ido-yes-or-no-mode t)
