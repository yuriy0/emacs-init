;; Haskell mode
(use-package haskell-mode
  :ensure

  :config

  (customize-set-variables 
   'haskell-process-suggest-remove-import-lines t
   'haskell-process-auto-import-loaded-modules t
   'haskell-process-log t
   'haskell-process-type 'auto)

  (setq haskell-indent-offset 2
        haskell-indentation-left-offset 0
        haskell-literate-default (quote tex)
        haskell-process-show-debug-tips nil)
  (add-to-list 'font-lock-maximum-decoration '(haskell-mode . 2))
  
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

  ;; Haskell mode bindings
  (many 2 (apply-partially 'define-key haskell-mode-map)

        ;; Interactive Haskell mode 
        (kbd "C-c C-l") 'haskell-process-load-or-reload

        ;; interactive process commands mostly made obsolete by lsp-mode
        ;; (kbd "C-c C-t") 'haskell-process-do-type
        ;; (kbd "C-c C-i") 'haskell-process-do-info

        (kbd "RET") 'electric-indent-just-newline)

  (setq haskell-lexeme--char-literal-rx 
        (rx-to-string 
         `(: (group "'")
             (| (: (group (regexp "[[:alpha:]_([]")) (group "'")) 
                (: (group (| (regexp "\\\\[^\n][^'\n]*") 
                             (regexp "[^[:alpha:]_(['\n]")
                             ;; used to be
                             ;; (regexp "[^[:alpha:]_(['\n][^'\n]*") 
                             ))
                   (| (group "'") "\n" (regexp "\\'")))))) )
)


(use-package company
  :hook (haskell-mode . company-mode)
  :commands (company-mode))

(require 's)
(defun get-haskell-language-server-path ()
  (when (executable-find "ghcup")
    (let ((hls-path (shell-command-to-string "ghcup whereis hls 2> /dev/null")))
      (when (and hls-path (not (s-blank? hls-path)))
        hls-path
        ))))

(when-let ((hls-path (get-haskell-language-server-path)))
  (progn

    (use-package lsp-haskell
      :ensure
      :after (lsp-mode)

      :config

      (setq lsp-haskell-server-path (get-haskell-language-server-path))

      (setq
       ;; disables all hlint diagnostics which are very aggressive...
       lsp-haskell-plugin-hlint-diagnostics-on nil
       lsp-haskell-plugin-hlint-code-actions-on nil

       ;; some combination of the following makes CodeActions extremely slow?
       lsp-haskell-plugin-tactics-global-on nil ; "wingman"
       ;; lsp-haskell-plugin-import-lens-code-actions-on nil
       ;; lsp-haskell-plugin-pragmas-code-actions-on nil
       )

      ;; configure additional lsp properties which aren't provided by lsp-haskell
      ;; (defcustom-lsp lsp-haskell/plugin/ghcide-code-actions-fill-holes/globalOn
      ;;   nil
      ;;   ""
      ;;   :group 'lsp-haskell-plugins
      ;;   :type 'boolean
      ;;   :lsp-path "haskell.plugin.ghcide-code-actions-fill-holes.globalOn")
      )

    (use-package lsp-mode
      :hook (haskell-mode . lsp-deferred)
      :commands (lsp lsp-deferred)
      )

    (use-package yasnippet
      :hook (haskell-mode . yas-minor-mode)
      :commands (yas-minor-mode)
      )
    ))
