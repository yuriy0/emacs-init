;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'ht)

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
           ;; (diag-related-infos (lsp:diagnostic-related-information? diag))
           ;; (at-least-1 (> (length diag-related-infos) 0))
           ;; (diag-related-info0 (aref diag-related-infos 0))
           ;; (rel-info-msg (or (lsp:diagnostic-related-information-message diag-related-info0) t))
           ;; (diag-msg-is-orign (equal rel-info-msg "original diagnostic"))
           (diag-origin-range (my/lsp-diagnostic-get-origin-range diag))
           )
      (progn
        ;; (message (format "==\nSkipping %s\n" (yaml-encode diag)))

        ;; (my/lsp-diagnostic-make-companion-overlap all-diags diag diag-origin-range)
        ;; this message in fact is a "related info" message for another real erro

        ;; (ht-set my/lsp-associated-overlays diag-origin-range diag)
        (push (list diag-origin-range diag) my/lsp-associated-overlays)

        nil)
    t))


;;;###autoload
(defun my/lsp-diagnostics--flycheck-start-around (fn checker callback)
  "start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."

  (remove-hook 'lsp-on-idle-hook #'lsp-diagnostics--flycheck-buffer t)
  (my/lsp-diagnostics-pre-send-to-flycheck)

  ;; (when my/lsp-diags-overlays-switch-line
  ;;   (apply my/lsp-diags-overlays-switch-line nil))

  ;; (message (format "===all diags: %s==="
  ;;                  (yaml-encode (lsp--get-buffer-diagnostics))))

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


(defun test-ov ()
  (interactive)
  (delete-all-overlays)
  (-let* (
         ((p0 p1) (get-visual-line-start-end (- (line-number-at-pos) 0)))
         (overlay (make-overlay p0 p1 (current-buffer) t t))
         )
    (overlay-put overlay 'intangible t)
    (overlay-put overlay 'before-string
                 (propertize "foobar\n" 'face '(:background "red"))
                 )
    (overlay-put overlay 'face '(:foreground "blue"))
    overlay))

(defvar my/lsp-diags-overlays nil)

(defvar my/lsp-temp nil)

(defvar my/lsp-associated-overlays nil)

(defvar my/lsp-all-buffer-diags nil)

(defun my/lsp-diagnostics-find-exact-range (diags range)
  (-filter (lambda (i) (ht-equal?-rec (lsp:diagnostic-range i) range)) diags)
)

(defun my/lsp-diagnostic-make-companion-overlap (origin-diag diag diag-origin-range text-properties &optional override-msg)
  (-let* (
          (mode-inline nil)
          (source-loc-offset (if mode-inline 1 1))
          ;; (source-loc (lsp:diagnostic-range diag))
          ;; (source-loc-start (lsp:range-start source-loc))
          ;; (line-pos (lsp:position-line source-loc-start))

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
             ;; (message (format "setting text properties %s" text-properties))
             (set-text-properties 0 base-msg-len text-properties base-msg)

             ))

          (msg (concat
                ;; (propertize
                 (apply 'concat (-repeat char-pos " "))
                 ;; 'face '(:background "blue")
                 ;; )
                base-msg
                ))
          (ov-subline (make-overlay (+ -1 p1) (+ 0 p1) (current-buffer) nil t))

          (ov-inline (make-overlay
                      (+ char-pos p0)
                      (+ end-char-pos p0) (current-buffer) nil t))

          (err-level
           (or

            (my/lsp-diagnostics-flycheck-error-level origin-diag)

            'unknown)
           )
         )
    ;; (message (format "overlay at %s (errlevel %s) ===origin: %s\n ===diag:%s\n"
    ;;                  line-pos
    ;;                  err-level
    ;;                  (yaml-encode diag-origin-range)
    ;;                  (yaml-encode diag)))

    (push ov-subline my/lsp-diags-overlays)
    (push ov-inline my/lsp-diags-overlays)

    (overlay-put ov-subline 'intangible t)

    ;; (overlay-put overlay
    ;;              'face 
    ;;              ;; '(:background "orange")
    ;;              '(
    ;;                :box '(:line-width (-1 . -1)
    ;;                                 :color "gtk_selection_bg_color"
    ;;                                 :style nil))
    ;;              )

    (overlay-put ov-subline 'after-string (concat "\n" msg))
    (overlay-put ov-subline 'companion-original-range diag-origin-range)


    ;; (overlay-put overlay 'invisible t)

    ;; (if (not mode-inline)
    ;;     (overlay-put overlay 'before-string (concat "" msg ""))
    ;;   (overlay-put overlay 'display msg)
    ;;   )


    (overlay-put ov-inline 'intangible t)
    (overlay-put ov-inline
                 'face 
                 ;; '(:background "orange")
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
  ;; (message (format "Deleting overlays: %s" my/lsp-diags-overlays))
  (-each my/lsp-diags-overlays
    (lambda(o)
      (delete-overlay o)))
  (setq my/lsp-diags-overlays nil)
  ;; (ht-clear my/lsp-associated-overlays)
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
      (message (format "companion for line %s = %s " line
                       (--map (lsp:diagnostic-message (cadr it)) companions-for-line)
                       ))

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
      (setq my/lsp-temp overlay-text)
      ;; (message (format "diags overlays %s %s"
      ;;                  sideline-displayed-overlay
      ;;                  (overlay-get sideline-displayed-overlay 'after-string)
      ;;                  ))
      

      (my/lsp-diags-overlays-switch-line sideline-displayed-overlay overlay-text-props)
      )
  )

(advice-add 'lsp-ui-sideline--diagnostics :after #'my/lsp-ui-sideline--diagnostics--after)
;; (advice-remove 'lsp-ui-sideline--diagnostics #'my/lsp-ui-sideline--diagnostics--after)

(add-hook 'flycheck-process-error-functions #'my/flycheck-filtering -50)
;; (remove-hook 'flycheck-process-error-functions #'my/flycheck-filtering)

(advice-add 'lsp-diagnostics--flycheck-start :around #'my/lsp-diagnostics--flycheck-start-around)
;; (advice-remove 'lsp-diagnostics--flycheck-start #'my/lsp-diagnostics--flycheck-start-around)
