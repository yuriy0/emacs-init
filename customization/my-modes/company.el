(use-package company
  :ensure

  :custom

  ;; idle completion
  (company-idle-delay (lambda() (if (company-in-string-or-comment) nil 0.0)))

  ;; completion starts with any # of characters
  (company-minimum-prefix-length 1)

  ;; put completions from recent buffers at the top
  (company-transformers '(company-sort-by-occurrence))

  (company-frontends
   '(
     company-pseudo-tooltip-unless-just-one-frontend
     company-preview-if-just-one-frontend))

  :bind

  ;; explicit completion
  ("M-/" . #'company-complete)

  (:map company-active-map
        ;; esc while completion popup is active closes it
        ("ESC" . 'company-abort)

        ;; ("TAB" . company-complete-selection)
        ("<tab>" . company-complete-selection)
        )
)