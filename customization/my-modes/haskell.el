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
  
  ;; Interactive Haskell mode 
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

  ;; Haskell mode bindings
  (many 2 (apply-partially 'define-key haskell-mode-map)
        (kbd "C-c C-l") 'haskell-process-load-or-reload
        (kbd "C-c C-t") 'haskell-process-do-type
        (kbd "C-c C-i") 'haskell-process-do-info
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
      :config

      (setq lsp-haskell-server-path hls-path)
      )

    (use-package lsp-mode
      :hook (haskell-mode . lsp-deferred)
      :commands (lsp lsp-deferred)
      )

    ))
