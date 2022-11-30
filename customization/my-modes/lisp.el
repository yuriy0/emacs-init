(use-package lisp-extra-font-lock
  :quelpa
  ((lisp-extra-font-lock
    :fetcher github
    :repo "Lindydancer/lisp-extra-font-lock"))

  :config
  (lisp-extra-font-lock-global-mode 1)

  :custom-face
  (lisp-extra-font-lock-backquote ((t (:foreground "orange red"))))
  (lisp-extra-font-lock-quoted ((t (:foreground "grey35"))))
)

(use-package highlight-parentheses
  :disabled ;; gives random errors?
  :ensure
  :hook ((emacs-lisp-mode . highlight-parentheses))
)

(use-package elisp-mode
  :config

  (defun eval-macroexpand-last-sexp()
    (interactive)
    (insert "\n")
    (pp-macroexpand-last-sexp t))


  (defun eval-macroexpand-all-last-sexp()
    (interactive)
    (insert "\n")
    (insert (pp-to-string (macroexpand-all (pp-last-sexp)))))

  :bind
  (:map lisp-interaction-mode-map
        ("C-M-j" . eval-macroexpand-last-sexp)
        ("C-M-S-J" . eval-macroexpand-all-last-sexp)
        )
)
