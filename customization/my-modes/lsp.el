;; -*- lexical-binding: t; -*-

(defface lsp-inlay-face
  '((t :inherit font-lock-comment-face))
  "The face to use for LSP inlays."
  :group 'lsp-faces)

(use-package lsp-mode
  :ensure
  :commands (lsp lsp-deferred)

  :init
  (setq lsp-keymap-prefix "C-c l")

  :custom

  ;;"lens" = count references to symbols
  (lsp-lens-enable t)

  ;; full doc strings are very long and get broken when put into the minibuffer
  ;; the "short" version shows types for some subexpressions (but occasionally shows
  ;; nothing useful?)
  (lsp-eldoc-render-all nil)

  (lsp-idle-delay 0.6)

  ;; after changes clear diagnostics since they will usually refer to invalid line/column numbers
  ;; NOTE: invalid line/columns on errors fixed in `lsp-ui-sideline-companions'
  ;; (lsp-diagnostic-clean-after-change t)

  ;; don't execute an action automatically when its the only one
  (lsp-auto-execute-action nil)

  ;; disables some types of diagnostics from rust-analyzer
  ;; see https://rust-analyzer.github.io/manual.html#diagnostics
  ;; (lsp-rust-analyzer-diagnostics-disabled [])

  :config
  ;; its possible that lsp-mode is required before this file is loaded
  ;; in which case the prefix keymap is set with the wrong binding
  (define-key lsp-mode-map (kbd lsp-keymap-prefix) lsp-command-map)

  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; improves lsp-mode performance
  (setq read-process-output-max (expt 2 16))
  (setq gc-cons-threshold (round (* 0.75 (expt 10 8))))

  ;; auto start lsp-ui
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)

  (with-eval-after-load 'which-key (lsp-enable-which-key-integration t))

  ;; fixes a bug in windows, where paths are case-insensitive so the canonical form
  ;; should be case-normalized

  ;; temp: disabled because this in fact breaks a ton of things ? ...

  ;; (when (system-type-windowslike-p)
  ;;   (defun my/lsp-f-canonical (file-name)
  ;;     "Return the canonical FILE-NAME, without a trailing slash."
  ;;     (downcase (directory-file-name (expand-file-name file-name))))

  ;;   (advice-add 'lsp-f-canonical :override #'my/lsp-f-canonical))

  :custom-face
  (lsp-inlay-face ((t (:height 0.8 :slant italic :foreground "pale violet red" :inherit font-lock-comment-face))))

  (lsp-javascript-inlay-face
   ((t :inherit lsp-inlay-face)))

  (lsp-rust-analyzer-inlay-face
   ((t :inherit lsp-inlay-face)))
  )

(use-package eldoc
  :defer t
  :diminish)

(use-package lsp-lens
  :defer t
  :diminish)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode

  :bind
  (:map lsp-ui-mode-map
        ("C-c C-c s" . #'toggle-lsp-ui-sideline-show-hover)
        ("C-c C-c t" . #'lsp-ui-doc-mode)
        ("C-c C-c >" . #'lsp-describe-thing-at-point)
        ("M-j" . #'lsp-treemacs-symbols)
        ("C-c C-c a" . #'lsp-execute-code-action)
        ("C-c C-c y" . #'lsp-ui-flycheck-list)
        ("C-c C-c g" . #'lsp-ui-doc-show)

        ;; standard xref jump using lsp-ui instead
        (([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . #'lsp-ui-peek-find-references)
         )
        )

  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-doc-show-with-mouse nil) ;; causes massive flicker on the taskbar, even when no keybinds/handlers registered?

  :config

  ;; fixes a bug with lsp-ui-doc / lsp help buffers
  ;; see https://github.com/emacs-lsp/lsp-ui/issues/452
  (advice-add 'markdown-follow-thing-at-point :around #'lsp-ui-follow-thing-at-point/advice-around)

  ;; for completeness
  (define-key help-mode-map (kbd "<return>") #'universal-follow-thing-at-point)

  ;; lsp modeline will usually contain some funky unicode
  (unicode-fonts-setup)

  ;; better display for sideline
  (advice-add 'lsp-ui-sideline--align :override #'my/lsp-ui-sideline--align)
  (advice-add 'lsp-ui-sideline--compute-height :override #'my/lsp-ui-sideline--compute-height)

  ;; bug: this face is referenced but not defined???
  (when (not (facep 'lsp-flycheck-warning-unnecessary))
    (defface lsp-flycheck-warning-unnecessary
      '((t :inherit warning))
      "???"
      :group 'lsp-faces))

  :custom-face
  (lsp-modeline-code-actions-preferred-face ((t (:foreground "dark goldenrod"))))

  ;; '(lsp-face-highlight-write
  ;;   ((t
  ;;     (:height 1
  ;;      :weight normal
  ;;      :inherit (highlight)))))
)

;; this is identical to `lsp-ui-doc--open-markdown-link' except:
;;  - we return `t' in case we actually followed a link
(defun lsp-ui-follow-thing-at-point ()
  (interactive)
  (let ((buffer-list-update-hook nil))
    (-let [(buffer point) (if-let* ((valid (and (listp last-input-event)
                                                (eq (car last-input-event) 'mouse-2)))
                                    (event (cadr last-input-event))
                                    (win (posn-window event))
                                    (buffer (window-buffer win)))
                              `(,buffer ,(posn-point event))
                            `(,(current-buffer) ,(point)))]
      (with-current-buffer buffer
        ;; Markdown-mode puts the url in 'help-echo
        (-some--> (get-text-property point 'help-echo)
          (and (string-match-p goto-address-url-regexp it)
               (progn (browse-url it) t)))))))

(defun lsp-ui-follow-thing-at-point/advice-around (fn &rest fn-args)
  (or (lsp-ui-follow-thing-at-point)
      (apply fn fn-args)))


(defun toggle-lsp-ui-sideline-show-hover ()
  (interactive)
  (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
)

(defvar lsp-ui-sideline-height-mult 0.8 "Relative line hieght of lsp-ui-sideline text")

(defun my/lsp-ui-sideline--align (len margin)
  "Align sideline string by LENGTHS from the right of the window."
  (+ margin
     (if (display-graphic-p) 1 2)
     (* lsp-ui-sideline-height-mult len)
     ))

(defun my/lsp-ui-sideline--compute-height ()
  "Return a fixed size for text in sideline."
  (if (null text-scale-mode-remapping)
      `(height ,lsp-ui-sideline-height-mult)
    ;; Readjust height when text-scale-mode is used
    (list 'height
          (/ 1 (or (plist-get (cdar text-scale-mode-remapping) :height)
                   1)))))



(defun universal-follow-thing-at-point()
  (interactive)
  (or (push-button)
      (lsp-ui-follow-thing-at-point)
      (markdown-follow-thing-at-point nil)))



;; customize the lsp-modeline diagnostics display
;; fixes a bug with the modeline mouse click action, it uses `lsp-treemacs-errors-list' which doesn't work for me
(with-eval-after-load "lsp-modeline"
  ;; this strange require mode is used because the pattern matching `-let' forms below are macro-expanded during compilation
  ;; but their expansion is ill-defined until some other functions are defined in lsp-modeline (in particular the symbols `&Diagnostic?'
  ;; have a user-specified meaning which is given in lsp-modeline
  (eval-and-compile (require 'lsp-modeline))

  (defun lsp-count-diagnostics-of-types()
    (let ((diagnostics (cond
                        ((equal :file lsp-modeline-diagnostics-scope)
                         (list (lsp--get-buffer-diagnostics)))
                        (t (->> (eq :workspace lsp-modeline-diagnostics-scope)
                                (lsp-diagnostics)
                                (ht-values)))))
          (stats (make-vector lsp/diagnostic-severity-max 0))
          )

      (mapc (lambda (buf-diags)
              (mapc (lambda (diag)
                      (-let [(&Diagnostic? :severity?) diag]
                        (when severity?
                          (cl-incf (aref stats severity?)))))
                    buf-diags))
            diagnostics)

      stats
      ))

  (defun my--lsp-modeline-diagnostics-statistics ()
    "Calculate diagnostics statistics based on `lsp-modeline-diagnostics-scope'."
    (let ((stats (lsp-count-diagnostics-of-types))
          strs
          (i 0))
      (while (< i lsp/diagnostic-severity-max)
        (when (> (aref stats i) 0)
          (setq strs
                (nconc strs
                       `(,(propertize
                           (format "%s" (aref stats i))
                           'face
                           (cond
                            ((= i lsp/diagnostic-severity-error) 'error)
                            ((= i lsp/diagnostic-severity-warning) 'warning)
                            ((= i lsp/diagnostic-severity-information) 'success)
                            ((= i lsp/diagnostic-severity-hint) 'success)))))))
        (cl-incf i))
      (-> (s-join "•" strs)
          (propertize 'mouse-face 'mode-line-highlight
                      'help-echo "mouse-1: Show diagnostics"
                      'local-map (make-mode-line-mouse-map
                                  'mouse-1
                                  (if (require 'lsp-treemacs nil t) #'lsp-treemacs-errors-list #'lsp-ui-flycheck-list))
                      ))))

  (advice-add 'lsp-modeline-diagnostics-statistics :override #'my--lsp-modeline-diagnostics-statistics)

  ;; fixes `lsp-diagnostic-stats' having totally broken values...
  (defun sub-stats (a b) (cl-map 'vector #'- a b))
  (lsp-defun my--lsp--on-diagnostics-update-stats (workspace
                                               (&PublishDiagnosticsParams :uri :diagnostics))
    (let*
        ((path (lsp--fix-path-casing (lsp--uri-to-path uri)))
         (prev-stats (or (lsp-diagnostics-stats-for path) (make-vector 5 0)))
         (new-stats
          (sub-stats
           (let ((v (make-vector 5 0)))
            (mapc (-lambda ((&Diagnostic :severity?))
              (cl-incf (aref v (or severity? 1))))
            diagnostics)
            v)
           prev-stats)
          )
         )
      (lsp-diagnostics--update-path path new-stats)
      (while (not (string= path (setf path (file-name-directory
                                            (directory-file-name path)))))
        (lsp-diagnostics--update-path path new-stats))))
  (advice-add 'lsp--on-diagnostics-update-stats :override #'my--lsp--on-diagnostics-update-stats)
)


(use-package lsp-ui-sideline-companions
  :after (lsp-ui)
  :load-path "lisp-pkg/lsp-ui-sideline-companions"
  :config

  (add-hook 'lsp-ui-sideline-mode-hook
            (lambda()
              (lsp-ui-sideline-companions-mode (if lsp-ui-sideline-mode 1 -1)))
            )
)

(use-package lsp-preview-text-edits
  :after (lsp-mode)
  :load-path "lisp-pkg/lsp-preview-text-edits"
  :config

  (lsp-preview-text-edits-mode)

  (with-eval-after-load 'helm-lsp
    (advice-add 'helm-lsp-code-actions :override #'helm-lsp-code-actions-with-preview))
)
