(use-package treesitter-grammar-ensure
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :load-path "lisp-pkg/treesitter-grammar-ensure")
