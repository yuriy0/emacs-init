;; whitespace mode
(require 'whitespace)
(setq-default
 whitespace-line-column nil
 whitespace-style       '(face lines-tail tabs tab-mark trailing)
 global-whitespace-mode nil)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; https://www.emacswiki.org/emacs/WhiteSpace
(setq whitespace-display-mappings
  '((space-mark   ?\    [?\xB7]     [?.])     ; space
    (space-mark   ?\xA0 [?\xA4]     [?_])     ; hard space
    (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n]) ; end-of-line
    ))

(set-face-attribute 'whitespace-trailing nil
   :background "bisque")
