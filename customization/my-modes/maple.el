;; Maple 

(use-package maplev
  :commands (maplev-mode)
  :load-path "~/.emacs.d/maplev/lisp"
  :mode ("\\.mpl\\'" . maplev-mode)
  :init (autoload 'maplev-mode "maplev" "Maple editing mode" t)

  :config
  (setq maplev-indent-level 2)
  (add-hook 'maplev-mode-hook
            (lambda ()
              (progn
                (run-hooks 'prog-mode-hook)
                )))
)
