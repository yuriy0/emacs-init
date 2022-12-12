;; -*- lexical-binding: t; -*-
(use-package topsy
  :quelpa (topsy :fetcher github :repo "alphapapa/topsy.el")
  :hook
  ((emacs-lisp-mode . #'topsy-mode)
   (lisp-interaction-mode-hook . #'topsy-mode)
   )
)
