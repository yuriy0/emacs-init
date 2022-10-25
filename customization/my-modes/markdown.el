;; markdown mode

(use-package markdown-mode
  :ensure

  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  (set-face-attribute 'markdown-code-face nil :family "Cascadia Mono" :height 110)
)
