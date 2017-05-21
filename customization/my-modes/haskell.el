;; Haskell mode
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
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Haskell mode bindings
(many 2 (apply-partially 'define-key haskell-mode-map)
 (kbd "C-c C-l") 'haskell-process-load-or-reload
 (kbd "C-c C-t") 'haskell-process-do-type
 (kbd "C-c C-i") 'haskell-process-do-info
 (kbd "RET") 'electric-indent-just-newline)
