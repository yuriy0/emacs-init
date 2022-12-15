(use-package magit
  :ensure

  ;; maybe we can add more things here but most git workflows start with "status"
  :commands (magit-status)

  :custom
  (magit-auto-revert-mode nil)
  (magit-diff-refine-hunk 'all)
)
