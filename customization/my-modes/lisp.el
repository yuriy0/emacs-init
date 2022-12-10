;; -*- lexical-binding: t; -*-

(use-package lisp-extra-font-lock
  :quelpa
  ((lisp-extra-font-lock
    :fetcher github
    :repo "Lindydancer/lisp-extra-font-lock"))

  :config
  (lisp-extra-font-lock-global-mode 1)

  :custom-face
  (lisp-extra-font-lock-backquote ((t (:foreground "orange red"))))
  (lisp-extra-font-lock-quoted ((t (:foreground "grey35"))))
)

(use-package highlight-parentheses
  :disabled ;; gives random errors?
  :ensure
  :hook ((emacs-lisp-mode . highlight-parentheses))
)

(use-package elisp-mode
  :config

  (defun eval-macroexpand-last-sexp()
    (interactive)
    (insert "\n")
    (pp-macroexpand-last-sexp t))


  (defun eval-macroexpand-all-last-sexp()
    (interactive)
    (insert "\n")
    (insert (pp-to-string (macroexpand-all (pp-last-sexp)))))

  :bind
  (:map lisp-interaction-mode-map
        ("C-M-j" . eval-macroexpand-last-sexp)
        ("C-M-S-J" . eval-macroexpand-all-last-sexp)
        )
)

(use-package paredit
  :ensure t
  :diminish
  :disabled
  :hook
  ((emacs-lisp-mode . #'enable-paredit-mode)
   (lisp-interaction-mode-hook . #'enable-paredit-mode)
   )

  :bind
  (:map paredit-mode-map
        ("C-<left>" . nil)
        ("C-<right>" . nil)
        )
)

(use-package lispy
  :ensure t
  :disabled

  :hook
  ((emacs-lisp-mode . (lambda () (lispy-mode 1)))
   (lisp-interaction-mode-hook . (lambda () (lispy-mode 1)))
   )

  :config
  ;; see https://sachachua.com/blog/2021/04/emacs-making-a-hydra-cheatsheet-for-lispy/
  (let ((bindings '(("<" "lispy-barf" "") ("A" "lispy-beginning-of-defun" "") ("j" "lispy-down" "") ("Z" "lispy-edebug-stop" "") ("B" "lispy-ediff-regions" "") ("G" "lispy-goto-local" "") ("h" "lispy-left" "") ("N" "lispy-narrow" "") ("y" "lispy-occur" "") ("o" "lispy-other-mode" "") ("J" "lispy-outline-next" "") ("K" "lispy-outline-prev" "") ("P" "lispy-paste" "") ("l" "lispy-right" "") ("I" "lispy-shifttab" "") (">" "lispy-slurp" "") ("SPC" "lispy-space" "") ("xB" "lispy-store-region-and-buffer" "") ("u" "lispy-undo" "") ("k" "lispy-up" "") ("v" "lispy-view" "") ("V" "lispy-visit" "") ("W" "lispy-widen" "") ("D" "pop-tag-mark" "") ("x" "see" "") ("L" "unbound" "") ("U" "unbound" "") ("X" "unbound" "") ("Y" "unbound" "") ("H" "lispy-ace-symbol-replace" "Edit") ("c" "lispy-clone" "Edit") ("C" "lispy-convolute" "Edit") ("n" "lispy-new-copy" "Edit") ("O" "lispy-oneline" "Edit") ("r" "lispy-raise" "Edit") ("R" "lispy-raise-some" "Edit") ("\\" "lispy-splice" "Edit") ("S" "lispy-stringify" "Edit") ("i" "lispy-tab" "Edit") ("xj" "lispy-debug-step-in" "Eval") ("xe" "lispy-edebug" "Eval") ("xT" "lispy-ert" "Eval") ("e" "lispy-eval" "Eval") ("E" "lispy-eval-and-insert" "Eval") ("xr" "lispy-eval-and-replace" "Eval") ("p" "lispy-eval-other-window" "Eval") ("q" "lispy-ace-paren" "Move") ("z" "lispy-knight" "Move") ("s" "lispy-move-down" "Move") ("w" "lispy-move-up" "Move") ("t" "lispy-teleport" "Move") ("Q" "lispy-ace-char" "Nav") ("-" "lispy-ace-subword" "Nav") ("a" "lispy-ace-symbol" "Nav") ("b" "lispy-back" "Nav") ("d" "lispy-different" "Nav") ("f" "lispy-flow" "Nav") ("F" "lispy-follow" "Nav") ("g" "lispy-goto" "Nav") ("xb" "lispy-bind-variable" "Refactor") ("xf" "lispy-flatten" "Refactor") ("xc" "lispy-to-cond" "Refactor") ("xd" "lispy-to-defun" "Refactor") ("xi" "lispy-to-ifs" "Refactor") ("xl" "lispy-to-lambda" "Refactor") ("xu" "lispy-unbind-variable" "Refactor") ("M" "lispy-multiline" "Other") ("xh" "lispy-describe" "Other") ("m" "lispy-mark-list" "Other"))))
    (eval
     (append
      '(defhydra my-lispy-cheat-sheet (:hint nil :foreign-keys run)
         ("<f14>" nil "Exit" :exit t))
      (cl-loop for x in bindings
               unless (string= "" (elt x 2))
               collect
               (list (car x)
                     (intern (elt x 1))
                     (when (string-match "lispy-\\(?:eval-\\)?\\(.+\\)"
                                         (elt x 1))
                       (match-string 1 (elt x 1)))
                     :column
                     (elt x 2))))))

  (define-key lispy-mode-map (kbd "<f12>") 'my/lispy-cheat-sheet/body)

  ;; remove default "[" "]" bindings, these are in fact too commonly used
  ;; as insert characters. instead remapping bindings for left/right word
  (define-key lispy-mode-map (kbd "[") #'lispy-open-square)
  (define-key lispy-mode-map (kbd "]") #'lispy-close-square)
  (define-key lispy-mode-map [remap left-word] #'lispy-backward)
  (define-key lispy-mode-map [remap right-word] #'lispy-forward)

  (setq lispy-comment-use-single-semicolon t))
