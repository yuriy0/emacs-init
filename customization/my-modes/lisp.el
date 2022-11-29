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
  :ensure
  :hook ((emacs-lisp-mode . highlight-parentheses))
)
