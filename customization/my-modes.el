(load-dir-one "~/.emacs.d/customization/my-modes/")

;; ;; whitespace mode 
;; (require 'whitespace)
;; (setq whitespace-style '(tabs tab-mark trailing))
;; (setq-default whitespace-line-column 80)
;; (global-whitespace-mode 1)

;; ;; ido mode 
;; (require 'ido)
;; (ido-mode t) 
;; (require 'ido-yes-or-no)

;; ;; helm 
;; (require 'helm-config)
;; (helm-mode 1)
;; (helm-autoresize-mode t)

;; (many 2 (apply-partially 'define-key helm-map)
;;  (kbd "<tab>") 'helm-execute-persistent-action ; rebind tab to run persistent action
;;  (kbd "C-i") 'helm-execute-persistent-action   ; make TAB works in terminal
;;  (kbd "C-z")  'helm-select-action)             ; list actions using C-z

;; (global-set-keys
;;  (kbd "M-x") 'helm-M-x
;;  (kbd "M-y") 'helm-show-kill-ring
;;  (kbd "C-M-y") 'helm-global-mark-ring
;;  (kbd "C-x C-f") 'helm-find-files
;;  (kbd "C-x b") 'helm-buffers-list
;;  (kbd "C-x q") 'helm-resume)

;; (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;; ;; helm-ag
;; (require 'helm-ag)
;; (global-set-keys
;;  (kbd "C-x / 1") 'helm-do-ag
;;  (kbd "C-x / 2") 'helm-do-ag-this-file
;;  (kbd "C-x / 3") 'helm-do-ag-project-root
;;  (kbd "C-x / 4") 'helm-do-ag-buffers)

;; (setq helm-ag-fuzzy-match t)

;; ;; uniquify
;; (require 'uniquify) 
;; (setq uniquify-buffer-name-style 'forward)

;; ;; Maple 
;; (setq load-path (cons "~/.emacs.d/maple" load-path))
;; (autoload 'maplev-mode "maplev" "Maple editing mode" t)
;; (add-to-list 'auto-mode-alist '("\\.mpl\\'" . maplev-mode))
;; (setq maplev-indent-level 2)

;; ;; MATLAB
;; (setq octave-comment-start "%")
;; (autoload 'octave-mode "octave-mod" nil t)
;; (setq auto-mode-alist
;;       (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; ;; C
;; (require 'cc-mode) 

;; ;; Web mode 
;; (require 'web-mode)
;; (define-key web-mode-map (kbd "RET") 'electric-indent-just-newline)

;; ;; PHP
;; (autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;; (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; ;; hakaru
;; (load "~/hakaru/tools/hakaru")
 
;; ;; cygwin
;; (require 'cygwin-mount)
;; (require 'setup-cygwin)

;; (cygwin-mount-activate)

;; (add-hook 'comint-output-filter-functions
;;     'shell-strip-ctrl-m nil t)
;; (add-hook 'comint-output-filter-functions
;;     'comint-watch-for-password-prompt nil t)
;; (setq explicit-shell-file-name "bash.exe")
;; ;; For subprocesses invoked via the shell
;; ;; (e.g., "shell -c command")
;; (setq shell-file-name explicit-shell-file-name)

;; ;; eshell
;; (require 'helm-eshell)
;; (fset 'shell 'eshell) ; replace shell with eshell 
;; (add-hook 'eshell-mode-hook 
;;   (lambda () 
;;     (define-key eshell-mode-map (kbd "M-]") 'helm-eshell-history)
;;     (setq eshell-cmpl-ignore-case t)
;;     (eshell-cmpl-initialize)
;;     (set-face-attribute 'eshell-prompt nil :foreground "chartreuse4")
;;     (push-mark)
;;     ))
;; (setq eshell-prompt-function #'(lambda nil
;;   (concat
;;     (getenv "USER") "@" (system-name) ":" (abbreviate-file-name (eshell/pwd))
;;     (if (= (user-uid) 0) " # " " $ "))))

;; ;; shell pop + eshell 
;; (setq shell-pop-shell-type "eshell"
;;       shell-pop-universal-key "C-'")

;; ;; Haskell mode
;; (customize-set-variables 
;;   'haskell-process-suggest-remove-import-lines t
;;   'haskell-process-auto-import-loaded-modules t
;;   'haskell-process-log t
;;   'haskell-process-type 'auto)

;; (setq haskell-indent-offset 2
;;       haskell-indentation-left-offset 0
;;       haskell-literate-default (quote tex)
;;       haskell-process-show-debug-tips nil)
;; (add-to-list 'font-lock-maximum-decoration '(haskell-mode . 2))
     
;; ;; Interactive Haskell mode 
;; (require 'haskell-interactive-mode)
;; (require 'haskell-process)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; ;; Haskell mode bindings
;; (many 2 (apply-partially 'define-key haskell-mode-map)
;;  (kbd "C-c C-l") 'haskell-process-load-or-reload
;;  (kbd "C-c C-t") 'haskell-process-do-type
;;  (kbd "C-c C-i") 'haskell-process-do-info
;;  (kbd "RET") 'electric-indent-just-newline)

;; ;; Agda mode
;; (load-file (let ((coding-system-for-read 'utf-8))
;;                  (shell-command-to-string "agda-mode locate")))

;; (require 'agda2-mode)
;; (add-hook 'agda2-mode-hook '(lambda () (buffer-face-mode 0)))

;; (setq agda2-program-args
;;   (mapcar (lambda (q) (concat "--include-path=" (canon-win-path (concat "~/" q))))
;;     '("agda/newlib"
;;       "agda/categories" 
;;       "stdlib/agda-stdlib/src")))
  
;; (setq agda-input-user-translations '(
;;   ;; random symbols 
;;   ("Nat" "ℕ") 
;;   ("BBN" "ℕ") 
;;   ("BBZ" "ℤ") 
;;   ("BBQ" "ℚ")
;;   ("SCup" "⊔")
;;   ("<#" "⇐")
;;   ("La" "⇐")
;;   ("ddown" "⇩")
;;   ("|-" "⊢")
;;   ("[=" "⊑")
;;   ("eps" "ε")
;;   ("cv" "⋎")
;;   ("{" "｛")
;;   ("}" "｝")
;;   ;; greek letters 
;;   ("Del" "Δ")
;;   ("del" "δ")
;;   ("Gam" "Γ") 
;;   ("gam" "γ")
;; ))

;; ;;;###autoload
;; (defun agda-quit-if-no-agda-buffs ()
;;   "Kill the agda process if there are no buffers visiting agda files."
;;   (when
;;     (and (agda2-running-p)
;;          (-none? (lambda (buf) 
;;            (with-current-buffer buf 
;;              (if (buffer-file-name) 
;;                  (string= (file-name-extension (buffer-file-name)) "agda")
;;                nil))) 
;;                  (buffer-list)))
;;     (progn (agda2-quit) (message "No agda buffers remaining; quitting agda"))))
;; (add-hook 'agda2-mode-hook '(lambda () (add-hook 'buffer-list-update-hook 'agda-quit-if-no-agda-buffs)))

;; ;; Agda mode character mappings
;; (many 2 (lambda (a b) (set-fontset-font "fontset-default" a b nil 'prepend))
;;   ;; Greek Extended     U+1F00  U+1FFF  (233)
;;   ;; General Punctuation        U+2000  U+206F  (111)
;;   ;; Superscripts and Subscripts        U+2070  U+209F  (42)
;;    '(#x1f00 . #x209f) "FreeMono" 
 
;;   ;; Letterlike Symbols         U+2100  U+214F  (80)
;;   ;; Number Forms       U+2150  U+218F  (60)
;;   ;; Arrows     U+2190  U+21FF  (112)
;;    '(#x2100 . #x21ff) "FreeMono" 

;;   ;; Mathematical Operators     U+2200  U+22FF  (256)
;;    '(#x2200 . #x22ff) "DejaVu Sans Mono" 

;;   ;; Miscellaneous Technical    U+2300  U+23FF  (255)
;;    '(#x2300 . #x23ff) "FreeMono" 
  
;;   ;; Enclosed Alphanumerics     U+2460  U+24FF  (160)
;;    '(#x2460 . #x24ff) "FreeMono" 
  
;;   ;; Miscellaneous Symbols      U+2600  U+26FF  (256)
;;    '(#x2600 . #x26ff) "FreeMono" 
  
;;   ;; Miscellaneous Mathematical Symbols-A       U+27C0  U+27EF  (48)
;;   ;; Supplemental Arrows-A      U+27F0  U+27FF  (16)
;;    '(#x27c0 . #x27ff) "FreeMono" 
  
;;   ;; Supplemental Arrows-B      U+2900  U+297F  (128)
;;   ;; Miscellaneous Mathematical Symbols-B       U+2980  U+29FF  (128)
;;    '(#x2900 . #x29ff) "FreeMono" 
  
;;   ;; Supplemental Mathematical Operators        U+2A00  U+2AFF  (256)
;;    '(#x2a00 . #x2aff) "FreeMono" 
  
;;   ;; Miscellaneous Symbols and Arrows   U+2B00  U+2BFF  (206)
;;    '(#x2b00 . #x2bff) "FreeMono" 
  
;;   ;; Superscripts and Subscripts - U+2070 To U+209F
;;    '(#x2070 . #x209f) "FreeMono" 
  
;;   ;; LATIN SUBSCRIPT SMALL LETTER H (U+2095)
;;   ;; LATIN SUBSCRIPT SMALL LETTER K (U+2096)
;;   ;; LATIN SUBSCRIPT SMALL LETTER L (U+2097)
;;   ;; LATIN SUBSCRIPT SMALL LETTER M (U+2098)
;;   ;; LATIN SUBSCRIPT SMALL LETTER N (U+2099)
;;   ;; LATIN SUBSCRIPT SMALL LETTER P (U+209A)
;;   ;; LATIN SUBSCRIPT SMALL LETTER S (U+209B)
;;   ;; LATIN SUBSCRIPT SMALL LETTER T (U+209C)
;;    '(#x2095 . #x209C) "DejaVu Sans Mono" 
  
;;   ;; angle brackets ⟨ ⟩
;;    '(#x27e8 . #x27f0) "DejaVu Sans Mono" 
;;    '(#x230a . #x230b) "DejaVu Sans Mono" 
  
;;   ;; double curly brace ⦃ ⦄  
;;    '(#x2983 . #x2984) "DejaVu Sans" 
  
;;   ;; relation compositon ⨾  
;;    '(#x2a3e . #x2a3e) "Cambria" )

;; ;; doc-view mode
;; (setq doc-view-continuous t
;;       doc-view-ghostscript-program "gswin64c") 

;; ;; auto complete
;; (require 'auto-complete)

;; (defun my-ac-mode()
;;   (auto-complete-mode) 
;;   (setq-default 
;;      ac-sources
;;      '(ac-source-words-in-same-mode-buffers
;;        ac-source-dictionary
;;        ac-source-abbrev)))

;; ;; auto complete - haskell
;; (define-key haskell-mode-map (kbd "M-/") 'ac-complete)
;; ;; http://pastebin.com/tJyyEBAS
;; (ac-define-source ghc-mod
;;   '((depends ghc)
;;     (candidates . (ghc-select-completion-symbol))
;;     (symbol . "s")
;;     (cache)))

;; (defun my-ac-haskell-mode ()
;;   (my-ac-mode)
;;   (setq ac-sources (append '(ac-source-ghc-mod) ac-sources)))
;; (add-hook 'haskell-mode-hook 'my-ac-haskell-mode)

;; (defun my-haskell-ac-init ()
;;   (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
;;     (my-ac-haskell-mode)))
;; (add-hook 'find-file-hook 'my-haskell-ac-init)

;; ;; ;; ac - C/c++
;; ;; (defun my-c-ac-init ()
;; ;;   (when (member (file-name-extension buffer-file-name) '("c" "cpp" "h" "hpp"))
;; ;;     (my-ac-mode))
;; ;;  )
;; ;; (add-hook 'c-mode-common-hook 'my-ac-mode)
;; ;; (add-hook 'find-file-hook 'my-c-ac-init)

;; ;; visual regexp
;; (require 'visual-regexp)
;; (define-key global-map (kbd "C-c r") 'vr/replace)
;; (define-key global-map (kbd "C-c q r") 'vr/query-replace)

;; ;; multi-cursor
;; (require 'multiple-cursors)
;; (global-set-keys
;;  (kbd "C->") 'mc/mark-next-like-this
;;  (kbd "C-<") 'mc/mark-previous-like-this
;;  (kbd "C-.") 'mc/mark-next-symbol-like-this
;;  (kbd "C-,") 'mc/mark-previous-symbol-like-this
;;  (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click
;;  (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; ;; smooth scroll
;; ;; http://www.emacswiki.org/emacs/SmoothScrolling
;; (setq scroll-step 1
;;       scroll-conservatively 10000
;;       auto-window-vscroll nil
;;       mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

;; ;; line numbers 
;; ;;;###autoload
;; (defun enable-linum-mode-in-mode (the-mode) (add-hook the-mode (lambda () (linum-mode 1))))
;; (-each '(prog-mode-hook haskell-mode-hook maplev-mode-hook) 
;;        'enable-linum-mode-in-mode)

;; ;; aspell
;; (setq ispell-program-name "aspell")
;; (require 'ispell)
;; (global-set-key (kbd "<f8>") 'ispell-word)
;; (setq ispell-local-dictionary "british")

;; ;; Latex
;; (require 'tex-mode)

;; (setq latex-run-command "pdflatex") 
;; (setq tex-start-commands "SumatraPDF")

;; ;;;###autoload
;; (defun custom-tex-hooks () 
;;   (many 1 (lambda (x) (add-to-list 'tex-compile-commands x))
;;                '("SumatraPDF.exe \"%r.pdf\"" nil nil)
;;                '("pdflatex.exe -interaction=nonstopmode \"%r.tex\"" nil nil)
;;                '("bibtex.exe \"%r\"" nil nil)
;;                '((concat "pdflatex.exe -interaction=nonstopmode \"%r.tex\" && "
;;                          "bibtex.exe \"%r\" && "
;;                          "pdflatex.exe -interaction=nonstopmode \"%r.tex\" && "
;;                          "pdflatex.exe -interaction=nonstopmode \"%r.tex\"")
;;                  nil nil) ))
;; (add-hook 'tex-mode-hook 'custom-tex-hooks)

;; ;; markdown mode
;; (require 'markdown-mode)
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
