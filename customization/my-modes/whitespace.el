(use-package lua-mode
  :ensure
  :requires (whitespace)

  :config
  ;; (add-hook 'lua-mode-hook
  ;;           (lambda ()
  ;;             (whitespace-toggle-options '(lines-tail))
  ;;             ) )
)

(use-package whitespace
  :ensure
  :delight whitespace-mode

  :config

  ;; whitespace mode
  (setq-default
   whitespace-line-column nil
   whitespace-style       '(face tabs tab-mark trailing)
   global-whitespace-mode nil)

  (add-hook 'prog-mode-hook 'whitespace-mode)

  ;; https://www.emacswiki.org/emacs/WhiteSpace
  (setq whitespace-display-mappings
    '((space-mark   ?\    [?\xB7]     [?.])     ; space
      (space-mark   ?\xA0 [?\xA4]     [?_])     ; hard space
      (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n]) ; end-of-line
      ))

  (add-hook 'focus-in-hook '(lambda()
    (set-face-attribute 'whitespace-trailing nil
       :background "bisque")
    (set-face-attribute 'whitespace-line nil
       :foreground 'unspecified
       :background "RosyBrown1")))

  ;; cleanup whitespace before saving
  ;; TODO: figure out how to stop this from screwing files in source control
  ;; (add-hook 'before-save-hook 'whitespace-cleanup)
)
