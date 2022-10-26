;; visual regexp
(use-package visual-regexp
  :ensure

  :bind
  (
  ("C-c r" . vr/replace)
  ("C-c q r" . vr/query-replace)
  )
  )
