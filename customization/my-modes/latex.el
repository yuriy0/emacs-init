;; Latex
(require 'tex-mode)

(setq latex-run-command "pdflatex") 
(setq tex-start-commands "SumatraPDF")

;;;###autoload
(defun custom-tex-hooks () 
  (many 1 (lambda (x) (add-to-list 'tex-compile-commands x))
               '("SumatraPDF.exe \"%r.pdf\"" nil nil)
               '("pdflatex.exe -interaction=nonstopmode \"%r.tex\"" nil nil)
               '("bibtex.exe \"%r\"" nil nil)
               '((concat "pdflatex.exe -interaction=nonstopmode \"%r.tex\" && "
                         "bibtex.exe \"%r\" && "
                         "pdflatex.exe -interaction=nonstopmode \"%r.tex\" && "
                         "pdflatex.exe -interaction=nonstopmode \"%r.tex\"")
                 nil nil) ))
(add-hook 'tex-mode-hook 'custom-tex-hooks)
