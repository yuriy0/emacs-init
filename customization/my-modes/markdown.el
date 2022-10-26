;; markdown mode

(use-package markdown-mode
  :ensure
  :commands (markdown-mode)

  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  ;; the face used for markdown code preview, default is an ugly raster font
  (set-face-attribute 'markdown-code-face nil :family "Cascadia Mono" :height 110)
)
