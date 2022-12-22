;; lsp for javascript & typescript
(use-package lsp-javascript
  :ensure lsp-mode
  :defer t ;; this is actually loaded by the appropriate major mode

  :config

  ;; todo: is this every useful in JS? everything has type `any' ...
  (progn
    (setq lsp-javascript-display-variable-type-hints t)
    (setq lsp-javascript-display-enum-member-value-hints t)
    (setq lsp-javascript-display-parameter-name-hints t)
    (setq lsp-javascript-display-inlay-hints t)
    (setq lsp-javascript-display-parameter-type-hints t)
    (setq lsp-javascript-display-property-declaration-type-hints t)
    (setq lsp-javascript-display-return-type-hints t)
    (setq lsp-javascript-implicit-project-config-check-js t)
    )

  (defvar lsp-javascript-extra-lines-range-for-inlay-hints 50)
  (defvar lsp-javascript-inlay-hints-on-scroll-rate-limit 0.5)

  ;; better handling of inlay hints in javascript mode.
  ;; - the LSP mode config for javascript only gives inlay hints for the visible regions in the window.
  ;;   this is presumably due to LSP performance issues, but we can be more generous and ask for extra
  ;;   inlay hints for regions just outside the visible region
  ;; - less frequent updating of inlay hints during scroll; if we include some "extra" regions outside
  ;;   the visible region, then there is less need to always be sending inlay hints requests. for fast
  ;;   scrolling this can be considerable performance gain
  (defun my/lsp-javascript-update-inlay-hints-with-expansion(&optional a b)
    (-let*
        ((a (or a (window-start)))
         (b (or b (window-end nil t)))
         (buf (window-buffer (selected-window)))
         ((a b) (expand-position-range-by-lines buf a b lsp-javascript-extra-lines-range-for-inlay-hints))
         )
      (lsp-javascript--update-inlay-hints a b)))

  (define-advice lsp-javascript-update-inlay-hints
      (:override () my)
    (my/lsp-javascript-update-inlay-hints-with-expansion))

  (define-advice lsp-javascript-update-inlay-hints-scroll-function
      (:override (window start) my)
    (if-with-rate-limit
     lsp-javascript-inlay-hints-on-scroll-rate-limit
     'lsp-javascript-update-inlay-hints-scroll-function
     (my/lsp-javascript-update-inlay-hints-with-expansion
      start (window-end window t))))
)

(use-package js
  :config
  (setq js-indent-level 2) ;; used by json files
)

(defun my/use-web-mode() (version< emacs-version "29.0"))

;; Web mode
(use-package web-mode
  :if (my/use-web-mode)
  :ensure t
  :commands (web-mode)

  :init
  (when (version< emacs-version "29.0")
    (mapc (lambda(f)
            (add-to-list 'auto-mode-alist `(,f . web-mode)))
          '("\\.jsx?$"
            "\\.js?$"
            "\\.html?$"
            "\\.css?$"
            "\\.ts?$"
            "\\.tsx?$"
          )))

  :config

  (define-key web-mode-map (kbd "RET") 'electric-indent-just-newline)
  (add-hook 'web-mode-hook #'lsp-deferred)

  (defun my/web-mode-hook()
    (setq-local lsp-ui-peek-fontify 'always) ;; lsp-ui-peek breaks for web-mode files because the fontification only happens for file-visiting buffers
    )
  (add-hook 'web-mode-hook #'my/web-mode-hook)

  (setq web-mode-enable-auto-indentation nil)

  ;; needed to get LSP to consider the indent size the same as which we set above
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2915#issuecomment-855156802
  (with-eval-after-load 'lsp-mode
    (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset))

  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
)

(use-package web-server
  :ensure
  :after (web-mode)
)

(use-package typescript-ts-mode
  :if (not (my/use-web-mode))
  :commands (typescript-ts-mode)
  :config
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred))
