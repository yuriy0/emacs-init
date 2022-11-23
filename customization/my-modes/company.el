(use-package company
  :ensure
  :commands (company-mode)
  :delight company-mode

  :custom

  ;; idle completion
  (company-idle-delay (lambda() (if (company-in-string-or-comment) nil 0.0)))

  ;; completion starts with any # of characters
  (company-minimum-prefix-length 1)

  ;; sorting/filtering company candidates
  (company-transformers
   '(
     ;; put completions from recent buffers at the top
     ;; TODO: disabled because this breaks more sensible sort orders, like local variables before
     ;; other things, and expressions in expression contexts
     ;; company-sort-by-occurrence

     ))

  (company-frontends
   '(
     company-pseudo-tooltip-unless-just-one-frontend
     company-preview-if-just-one-frontend))

  :bind

  ;; explicit completion
  (:map company-mode-map
        ("M-/" . #'company-complete))

  (:map company-active-map
        ;; esc while completion popup is active closes it
        ("ESC" . 'company-abort)

        ;; ("TAB" . company-complete-selection)
        ("<tab>" . company-complete-selection)

        ("`" . company-complete-common-or-show-delayed-tooltip)
        )
)


(use-package yasnippet
  :ensure
  :commands (yas-minor-mode)
  :delight yas-minor-mode
  :config
  (yas-reload-all)
)
