(use-package dired
  :defer
  :commands (dired)

  :bind
  (:map dired-mode-map
        ("a" . dired-find-file)

        ;; https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
        ("^" . (lambda () (interactive) (find-alternate-file "..")))
        )

  :config
  (setq dired-recursive-copies (quote always))
  (setq dired-recursive-deletes (quote top))
  (save-place-mode 1)

  ;; find file with `a' in same buffer
  (put 'dired-find-alternate-file 'disabled nil)
  )

(use-package dired-hacks-utils
  :ensure t
  :after (dired)
)

(use-package dired-rainbow
  :ensure t
  :after (dired)
)

(use-package dired-subtree
  :ensure t
  :defer t

  :config
  (setq dired-subtree-use-backgrounds nil)

  :bind
  (:map dired-mode-map
        ("i" . dired-subtree-insert)
        ("<tab>" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-cycle)
        ("<RET>" . dired-find-alternate-file)
        ("a" . dired-find-file)
        )
)
