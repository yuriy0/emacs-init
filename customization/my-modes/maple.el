;; Maple 
(setq load-path (cons "~/.emacs.d/maple" load-path))
(autoload 'maplev-mode "maplev" "Maple editing mode" t)
(add-to-list 'auto-mode-alist '("\\.mpl\\'" . maplev-mode))
(setq maplev-indent-level 2)

