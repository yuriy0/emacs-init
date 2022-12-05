;; Local Variables:
;; no-byte-compile: t
;; lexical-binding: t
;; End:

(add-to-list 'load-path (concat user-emacs-directory "/lisp-pkg/auto-compile"))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
