;; Agda mode
(load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode.exe locate")))


;; fixes an issue with emacs hanging when viewing some unicode files
;; see https://github.com/purcell/emacs.d/issues/273
(setq inhibit-compacting-font-caches t)

(require 'agda2-mode)

;;;###autoload
(defun agda-quit-if-no-agda-buffs ()
  "Kill the agda process if there are no buffers visiting agda files."
  (when
    (and (agda2-running-p)
         (-none? (lambda (buf) 
           (with-current-buffer buf 
             (if (buffer-file-name) 
                 (string= (file-name-extension (buffer-file-name)) "agda")
               nil))) 
                 (buffer-list)))
    (progn (agda2-quit) (message "No agda buffers remaining; quitting agda"))))
(add-hook 'agda2-mode-hook #'(lambda () (add-hook 'buffer-list-update-hook #'agda-quit-if-no-agda-buffs)))

(defun agda-init-unicode ()
  (setq agda-input-user-translations '(
    ;; random symbols 
    ("Nat" "ℕ") 
    ("BBN" "ℕ") 
    ("BBZ" "ℤ") 
    ("BBQ" "ℚ")
    ("SCup" "⊔")
    ("<#" "⇐")
    ("La" "⇐")
    ("ddown" "⇩")
    ("|-" "⊢")
    ("[=" "⊑")
    ("eps" "ε")
    ("cv" "⋎")
    ("{" "｛")
    ("}" "｝")
    ;; greek letters 
    ("Del" "Δ")
    ("del" "δ")
    ("Gam" "Γ") 
    ("gam" "γ")
  ))
  (agda-input-setup)
)
(agda-init-unicode)

;; find a good font which has all the characters?
(add-hook 'agda2-mode-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Mononoki"))
            (buffer-face-mode)))

;; Agda mode character mappings for characters which don't have definitions 
;; in the base font
(many 2 (lambda (a b) (set-fontset-font "fontset-default" a b nil 'prepend))
  ;; Greek Extended     U+1F00  U+1FFF  (233)
  ;; General Punctuation        U+2000  U+206F  (111)
  ;; Superscripts and Subscripts        U+2070  U+209F  (42)
   '(#x1f00 . #x209f) "FreeMono" 
 
  ;; Letterlike Symbols         U+2100  U+214F  (80)
  ;; Number Forms       U+2150  U+218F  (60)
  ;; Arrows     U+2190  U+21FF  (112)
   '(#x2100 . #x21ff) "FreeMono" 

  ;; Mathematical Operators     U+2200  U+22FF  (256)
   '(#x2200 . #x22ff) "DejaVu Sans Mono" 

  ;; Miscellaneous Technical    U+2300  U+23FF  (255)
   '(#x2300 . #x23ff) "FreeMono" 
  
  ;; Enclosed Alphanumerics     U+2460  U+24FF  (160)
   '(#x2460 . #x24ff) "FreeMono" 
  
  ;; Miscellaneous Symbols      U+2600  U+26FF  (256)
   '(#x2600 . #x26ff) "FreeMono" 
  
  ;; Miscellaneous Mathematical Symbols-A       U+27C0  U+27EF  (48)
  ;; Supplemental Arrows-A      U+27F0  U+27FF  (16)
   '(#x27c0 . #x27ff) "FreeMono" 
  
  ;; Supplemental Arrows-B      U+2900  U+297F  (128)
  ;; Miscellaneous Mathematical Symbols-B       U+2980  U+29FF  (128)
   '(#x2900 . #x29ff) "FreeMono" 
  
  ;; Supplemental Mathematical Operators        U+2A00  U+2AFF  (256)
   '(#x2a00 . #x2aff) "FreeMono" 
  
  ;; Miscellaneous Symbols and Arrows   U+2B00  U+2BFF  (206)
   '(#x2b00 . #x2bff) "FreeMono" 
  
  ;; Superscripts and Subscripts - U+2070 To U+209F
   '(#x2070 . #x209f) "FreeMono" 
  
  ;; LATIN SUBSCRIPT SMALL LETTER H (U+2095)
  ;; LATIN SUBSCRIPT SMALL LETTER K (U+2096)
  ;; LATIN SUBSCRIPT SMALL LETTER L (U+2097)
  ;; LATIN SUBSCRIPT SMALL LETTER M (U+2098)
  ;; LATIN SUBSCRIPT SMALL LETTER N (U+2099)
  ;; LATIN SUBSCRIPT SMALL LETTER P (U+209A)
  ;; LATIN SUBSCRIPT SMALL LETTER S (U+209B)
  ;; LATIN SUBSCRIPT SMALL LETTER T (U+209C)
   '(#x2095 . #x209C) "DejaVu Sans Mono" 
  
  ;; angle brackets ⟨ ⟩
   '(#x27e8 . #x27f0) "DejaVu Sans Mono" 
   '(#x230a . #x230b) "DejaVu Sans Mono" 
  
  ;; double curly brace ⦃ ⦄  
   '(#x2983 . #x2984) "DejaVu Sans" 
  
  ;; relation compositon ⨾  
   '(#x2a3e . #x2a3e) "Cambria"
)

(set-fontset-font "fontset-default" '(#x25A0 . #x265F)  '("Symbola" . "iso10646-1") nil 'prepend)

(add-hook 'agda2-mode-hook #'agda-init-unicode)
