;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'ht)
(require 'lsp-ui-sideline)
(require 'flycheck)

(defun ht-equal?-rec (table1 table2)
  "Return t if TABLE1 and TABLE2 have the same keys and values.
Does not compare equality predicates."
  (declare (side-effect-free t))

  (if (and (hash-table-p table1) (hash-table-p table2))
      (let ((keys1 (ht-keys table1))
            (keys2 (ht-keys table2))
            (sentinel (make-symbol "ht-sentinel")))
        (and (equal (length keys1) (length keys2))
             (--all?
              (ht-equal?-rec (ht-get table1 it)
                             (ht-get table2 it sentinel))
              keys1)))
    (equal table1 table2)
    ))

;;;###autoload
(defun my/flycheck-filtering (err)
  ;; (message (format "%s" err))
  ;; t ;; note that returning non-nil prevents further functions being called
  nil
)



(defun my/lsp-diagnostic-get-origin-range (diag)
  (if-let (
           (diag-related-infos (lsp:diagnostic-related-information? diag))
           (at-least-1 (> (length diag-related-infos) 0))
           (diag-related-info0 (aref diag-related-infos 0))
           (rel-info-msg (or (lsp:diagnostic-related-information-message diag-related-info0) t))
           (diag-msg-is-orign (equal rel-info-msg "original diagnostic"))
           )
      (lsp:location-range
       (lsp:diagnostic-related-information-location diag-related-info0)
       )
  ))


;;;###autoload
(defun my/lsp-diagnostics-rust-partition-associated-message (all-diags diag)
  (require 'yaml)
  (if-let (
           (diag-origin-range (my/lsp-diagnostic-get-origin-range diag))
           )
      (progn
        (push (list diag-origin-range diag) my/lsp-associated-overlays)
        nil)
    t))


;;;###autoload
(defun my/lsp-diagnostics--flycheck-start-around (fn checker callback)
  "start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."

  (remove-hook 'lsp-on-idle-hook #'lsp-diagnostics--flycheck-buffer t)
  (my/lsp-diagnostics-pre-send-to-flycheck)

  (let ((diags (lsp--get-buffer-diagnostics)))
    (setq my/lsp-all-buffer-diags diags)
    (->> diags
         (-filter (-partial #'my/lsp-diagnostics-rust-partition-associated-message diags))
         (-map (-lambda ((&Diagnostic :message :severity? :tags? :code? :source?
                                      :range (&Range :start (&Position :line      start-line
                                                                       :character start-character)
                                                     :end   (&Position :line      end-line
                                                                       :character end-character))))
                 (flycheck-error-new
                  :buffer (current-buffer)
                  :checker checker
                  :filename buffer-file-name
                  :message message
                  :level (lsp-diagnostics--flycheck-calculate-level severity? tags?)
                  :id code?
                  :group source?
                  :line (lsp-translate-line (1+ start-line))
                  :column (1+ (lsp-translate-column start-character))
                  :end-line (lsp-translate-line (1+ end-line))
                  :end-column (1+ (lsp-translate-column end-character)))))
         (funcall callback 'finished))
    )
  )

(defun my/lsp-diagnostics-flycheck-error-level (diag)
  (with-demoted-errors "my/lsp-diagnostics-flycheck-error-level %s"
    (-let (
           ((&Diagnostic :message :severity? :tags?) diag))
      (lsp-diagnostics--flycheck-calculate-level severity? tags?))
    )
  )

;;;###autoload
(defun get-visual-line-start-end (n)
  (save-excursion
    (goto-line 1)
    (vertical-motion n)
    (let ((line-start (point)))
      (end-of-visual-line) ; for the end of the line instead 
      (list line-start (point)))
    ))

;;;###autoload
(defun get-logical-line-start-end (n)
  (save-excursion
    (goto-line n)
    (let ((line-start (point)))
      (end-of-line)
      (list line-start (point)))
    ))


(defvar-local my/lsp-diags-overlays nil)

(defvar-local my/lsp-associated-overlays nil)

(defvar-local my/lsp-all-buffer-diags nil)

(defun my/lsp-diagnostics-find-exact-range (diags range)
  (-filter (lambda (i) (ht-equal?-rec (lsp:diagnostic-range i) range)) diags)
)

(defun my/lsp-diagnostic-make-companion-overlap (origin-diag diag diag-origin-range text-properties &optional override-msg)
  (-let* (
          (mode-inline nil)
          (source-loc-offset (if mode-inline 1 1))

          ((&Range :start
                  (&Position :line line-pos
                             :character char-pos)
                  :end
                  (&Position :line end-line-pos
                             :character end-char-pos))
           (lsp:diagnostic-range diag)
           )
          ((p0 p1) (get-logical-line-start-end (+ line-pos source-loc-offset)))

          (base-msg (or override-msg (lsp:diagnostic-message diag)))
          (base-msg
                (propertize
                 (concat base-msg)
                 ;; 'face '(:background "red")
                 'face 'error
                 ))
          (base-msg-len (length base-msg))
          (ignore
           (when text-properties
             (set-text-properties 0 base-msg-len text-properties base-msg)
             ))

          (msg (concat
                 (apply 'concat (-repeat char-pos " "))
                base-msg
                ))

          (ov-subline (make-overlay
                       (+ -1 p1)
                       (+ 0 p1) (current-buffer) nil t))
          (ov-inline (make-overlay
                      (+ char-pos p0)
                      (+ end-char-pos p0) (current-buffer) nil t))
         )
    (push ov-subline my/lsp-diags-overlays)
    (push ov-inline my/lsp-diags-overlays)

    (overlay-put ov-subline 'intangible t)
    (overlay-put ov-subline 'after-string (concat "\n" msg))
    (overlay-put ov-subline 'companion-original-range diag-origin-range)

    (overlay-put ov-inline 'intangible t)
    (overlay-put ov-inline
                 'face
                 '(
                   :box '(:line-width (-1 . -1)
                                    :color "dim grey"
                                    :style nil))
                 )
  ))


(defun my/lsp-diagnostics-clear-companion-overlays ()
  (-each my/lsp-diags-overlays
    (lambda(o)
      (delete-overlay o)))
  (setq my/lsp-diags-overlays nil)
  )

(defun my/lsp-diagnostics-pre-send-to-flycheck ()
  (my/lsp-diagnostics-clear-companion-overlays)
  (setq my/lsp-associated-overlays nil)
  )

(defun my/lsp-range-contains-line (range line)
  (-let ((
          (&Range :start (&Position :line start-line)
                  :end   (&Position :line end-line))
          range))
    (and (>= line start-line) (<= line end-line))
    )
)

(defun my/lsp-diags-overlays-switch-line (original &optional text-properties)
  (if (not original)
      (my/lsp-diagnostics-clear-companion-overlays)
    (-let* (
           (pos (overlay-get original 'position))
           (line (line-number-at-pos pos))
           (companions-for-line
            (--filter
             (my/lsp-range-contains-line (nth 0 it) line)
             my/lsp-associated-overlays
             )
            )
           (any-companion-for-line (car companions-for-line))
           )

      (-each companions-for-line
        (-lambda ((diag-origin-range diag))
          (let* (
                 (origin-diag
                  (car (my/lsp-diagnostics-find-exact-range
                        my/lsp-all-buffer-diags diag-origin-range)))
                 )

            (my/lsp-diagnostic-make-companion-overlap
             origin-diag
             diag
             diag-origin-range
             text-properties
             )
            )))

      ;; sometimes the original diagnostic is actually a multi-line diagnostic
      ;; which we treat as a "sideline" message for the first line + "subline"
      ;; message for the rest of the lines
      (if any-companion-for-line
          (-let* (
                  ((diag-origin-range diag) any-companion-for-line)
                  (origin-diag
                   (car (my/lsp-diagnostics-find-exact-range
                         my/lsp-all-buffer-diags diag-origin-range)))
                  (origin-diag-lines
                   (s-split "\n" (lsp:diagnostic-message origin-diag)))
                  )

            (if (> (length origin-diag-lines) 1)
                (-each (cdr origin-diag-lines)
                  (lambda (submsg)

                    (my/lsp-diagnostic-make-companion-overlap
                     origin-diag
                     origin-diag
                     diag-origin-range
                     text-properties
                     submsg
                     )

                    ))
              )
            ))
      )
    )
  )


(defun get-text-properties (n from props)
  (apply #'-concat (--map (list it (get-text-property n it from)) props)))

(defun copy-text-properties (n from to props)
  (set-text-properties 0 (if to (length to) 1000) (get-text-properties n from props) to)
  to
  )


(defun my/lsp-ui-sideline--diagnostics--after (&rest _)
  (my/lsp-diags-overlays-switch-line nil)

  (-when-let* ((diags-overlays
           (--filter
            (and
             (equal (overlay-get it 'kind) 'diagnostics)
             )
            lsp-ui-sideline--ovs)
           )
          (sideline-displayed-overlay (car diags-overlays))
          (overlay-text (overlay-get sideline-displayed-overlay 'after-string))
          (overlay-text-props (get-text-properties 1 overlay-text '(face display)))
          )
      (my/lsp-diags-overlays-switch-line sideline-displayed-overlay overlay-text-props)
      )
  )

(define-minor-mode lsp-ui-sideline-companions-mode
  ""
  :init-value nil
  (cond
   (lsp-ui-sideline-companions-mode
    (advice-add 'lsp-ui-sideline--diagnostics :after #'my/lsp-ui-sideline--diagnostics--after)
    (add-hook 'flycheck-process-error-functions #'my/flycheck-filtering -50)
    (advice-add 'lsp-diagnostics--flycheck-start :around #'my/lsp-diagnostics--flycheck-start-around)
    (flycheck-buffer)
    )
   (t
    (advice-remove 'lsp-ui-sideline--diagnostics #'my/lsp-ui-sideline--diagnostics--after)
    (remove-hook 'flycheck-process-error-functions #'my/flycheck-filtering)
    (advice-remove 'lsp-diagnostics--flycheck-start #'my/lsp-diagnostics--flycheck-start-around)

    (my/lsp-diagnostics-pre-send-to-flycheck)
    )
   )
)

(defun lsp-ui-sideline-companions-mode-toggle()
  (interactive)
  (lsp-ui-sideline-companions-mode 'toggle))

(provide 'lsp-ui-sideline-companions)
