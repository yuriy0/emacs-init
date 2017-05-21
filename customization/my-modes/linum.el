;; line numbers 
;;;###autoload
(defun enable-linum-mode-in-mode (the-mode) (add-hook the-mode (lambda () (linum-mode 1))))
(-each '(prog-mode-hook haskell-mode-hook maplev-mode-hook) 
       'enable-linum-mode-in-mode)
