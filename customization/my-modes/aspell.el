;; aspell
(setq ispell-program-name "aspell")
(require 'ispell)
(global-set-key (kbd "<f8>") 'ispell-word)
(setq ispell-local-dictionary "british" 
      ispell-personal-dictionary "~/emacs.d/.aspell.en.pws")
