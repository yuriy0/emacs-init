(use-package json-mode
  :ensure
  :commands (json-mode)

  :config

  (add-to-list 'auto-mode-alist '("\\.shader\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.materialtype\\'" . json-mode))
)
