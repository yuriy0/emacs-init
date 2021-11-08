;; (require 'hideshow)
;; (require 'sgml-mode)
;; (require 'nxml-mode)

;; (add-to-list 'hs-special-modes-alist
;;              '(nxml-mode
;;                "<!--\\|<[^/>]*[^/]>"
;;                "-->\\|</[^/>]*[^/]>"

;;                "<!--"
;;                sgml-skip-tag-forward
;;                nil))

;; (add-hook 'nxml-mode-hook 'hs-minor-mode)
;; (add-hook 'prog-mode-hook 'hs-minor-mode)

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))



(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key hs-minor-mode-map [C-tab] 'hs-toggle-hiding)
