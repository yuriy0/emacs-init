;; auto complete
(require 'auto-complete)

(defun my-ac-mode()
  (auto-complete-mode) 
  (setq-default 
     ac-sources
     '(ac-source-words-in-same-mode-buffers
       ac-source-dictionary
       ac-source-abbrev)))

(with-eval-after-load "auto-complete"
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB"))

;; auto complete - haskell
(define-key haskell-mode-map (kbd "M-/") 'ac-complete)
;; http://pastebin.com/tJyyEBAS
(ac-define-source ghc-mod
  '((depends ghc)
    (candidates . (ghc-select-completion-symbol))
    (symbol . "s")
    (cache)))

(defun my-ac-haskell-mode ()
  (my-ac-mode)
  (setq ac-sources (append '(ac-source-ghc-mod) ac-sources)))
(add-hook 'haskell-mode-hook 'my-ac-haskell-mode)

(defun my-haskell-ac-init ()
  (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
    (my-ac-haskell-mode)))
(add-hook 'find-file-hook 'my-haskell-ac-init)

;; ;; ac - C/c++
;; (defun my-c-ac-init ()
;;   (when (member (file-name-extension buffer-file-name) '("c" "cpp" "h" "hpp"))
;;     (my-ac-mode))
;;  )
;; (add-hook 'c-mode-common-hook 'my-ac-mode)
;; (add-hook 'find-file-hook 'my-c-ac-init)
