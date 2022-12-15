;; -*- lexical-binding: t; read-symbol-shorthands: (("my" . "lsp-preview-text-edits--")) -*-

(require 'lsp-mode)
(require 'dash)

(defface lsp-preview-text-edit-remove-text
  '((t (:strike-through "RoyalBlue4")))
  ""
  :group 'lsp-diagnostics)


(defface lsp-preview-text-edit-add-text
  '((t (:background "Paleturquoise3")))
  ""
  :group 'lsp-diagnostics)


(defface lsp-preview-text-edit-replace-text
  '((t (:background "Paleturquoise3")))
  ""
  :group 'lsp-diagnostics)


(defcustom lsp-preview-text-edits-captured-text-edit-type
  'all
  "The types of text edit operations which can be previewed. Be
careful with changing this as some types of text edits are called
in contexts where previewing makes no sense and could randomly
break (e.g. text edits on key press for automatic re-format or
automated LSP server text edit requests)."
  :type
  '(choice
    (const :tag "Always capture" all)
    (repeat
     (choice
      (const :tag "No type (default)" nil)
      (const :tag "completion-cleanup" completion-cleanup)
      (const :tag "format" format)
      (const :tag "color-presentation" color-presentation)
      (const :tag "code-action" code-action)
      (const :tag "rename" rename)
      (const :tag "rename-file" rename-file)
      )
     ))
)

(defvar my/lsp-previewing-text-edit-overlays nil)
(defvar my/lsp-previewing-text-edit-actions nil)
(defvar my/lsp-inside-previewing nil)

(defun my/symbol-name-safe(sym)
  (cond ((symbolp sym) (symbol-name sym))
        (t nil)))

(defun my/make-closure-for-text-edit-operation(fn &rest args)
  (let*
      ((save-buf (current-buffer))
       (save-point (point))
       (save-point-min (point-min))
       (save-point-max (point-max))
       )
    (push
     (lambda()
       (with-demoted-errors (format "command '%s' failed because %%s"
                                    (my/symbol-name-safe fn))
         (with-current-buffer save-buf
           (save-excursion
             (goto-char save-point)
             (save-restriction
               (widen)
               (narrow-to-region save-point-min save-point-max)
               (apply fn args)
               )
             )
           )
         )
       )
     my/lsp-previewing-text-edit-actions)
    )
  )

(lsp-defun my/lsp-preview-text-edit
  ((edit &as &TextEdit :range (&RangeToPoint :start :end) :new-text))

  ;;;; visualization the effect of:
  ;; (goto-char start)
  ;; (delete-region start end)
  ;; (insert new-text)

  (let*
      ((is-replace (> end start))
       (ov (make-overlay
            start
            end (current-buffer) t t))
       (new-text (s-replace "\r" "" (or new-text "")))
       (new-text (if is-replace (concat "→" new-text) new-text))
       (new-text
        (propertize
         new-text
         'face (if is-replace 'lsp-preview-text-edit-replace-text 'lsp-preview-text-edit-add-text)
         )
        )
       )
    (overlay-put ov 'face 'lsp-preview-text-edit-remove-text)
    (overlay-put ov 'after-string new-text)
    ov
    )
)

(defun my/lsp-preview-text-edits (edits)
  (->> edits
       ;; We sort text edits so as to apply edits that modify latter
       ;; parts of the document first. Furthermore, because the LSP
       ;; spec dictates that: "If multiple inserts have the same
       ;; position, the order in the array defines which edit to
       ;; apply first."  We reverse the initial list and sort stably
       ;; to make sure the order among edits with the same position
       ;; is preserved.
       (nreverse)
       (seq-sort #'lsp--text-edit-sort-predicate)
       (-map #'my/lsp-preview-text-edit)
       (-filter #'identity)
       )
)

(defun my-around/lsp--apply-text-edits (fn edits &optional operation)
  (if (and my/lsp-inside-previewing
           (or (eq lsp-preview-text-edits-captured-text-edit-type 'all)
               (memq operation lsp-preview-text-edits-captured-text-edit-type))
           )
      ;; create some overlays, save them for later deletion
      ;; and append the real action to be taken later
      (progn
        (setq my/lsp-previewing-text-edit-overlays
              (append my/lsp-previewing-text-edit-overlays
                      (my/lsp-preview-text-edits (copy-sequence edits))))
        (message "saving %s edits for later..." (length edits))
        (my/make-closure-for-text-edit-operation
         fn edits operation)
        )
    ;; perform the action immediately
    (funcall fn edits operation)
    )
)

(defun my-around/perhaps-lsp-will-edit-text(fn &rest args)
  (if my/lsp-inside-previewing
      ;; this is non-rentrant, although we could make it so with little effort,
      ;; this probably wouldn't make sense as then the user is applying/rejecting
      ;; multiple different changes? instead we error out
      (error "command '%s' was used while already previewing another LSP edit command '%s'"
             (my/symbol-name-safe fn)
             (my/symbol-name-safe my/lsp-inside-previewing))
    (let*
        ((my/lsp-previewing-text-edit-overlays nil)
         (my/lsp-previewing-text-edit-actions nil)
         (result)
         )
      (unwind-protect
          (progn

            ;; in the scope where the above variables are set, we except this function
            ;; to create the preview and save the actions for later
            (let ((my/lsp-inside-previewing fn))
              (setq result
                    (apply fn args)))

            (cond
             ;; if there is anything to preview, ask the user if they want to keep these changes
             (my/lsp-previewing-text-edit-overlays
              (let ((apply-edits (y-or-n-p "Apply edits?")))
                (when apply-edits
                  (-each my/lsp-previewing-text-edit-actions #'funcall))
                )
              )

             ;; if there is nothing to preview just execute any pending actions
             (my/lsp-previewing-text-edit-actions
              (-each my/lsp-previewing-text-edit-actions #'funcall))
             )
            )

        ;; always clear the overlays
        (-each my/lsp-previewing-text-edit-overlays #'delete-overlay)
      )

      result ;; this almost certainly isn't reliable anyways
      )
    )
)

(defvar lsp-preview-text-edits-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "")

(define-minor-mode lsp-preview-text-edits-mode
  "When this global minor mode is enabled, LSP commands which
modify text within the "
  :init-value nil
  :global t
  :keymap lsp-preview-text-edits-mode-map
  (cond
   (lsp-preview-text-edits-mode
    ;; enable
    (advice-add 'lsp--apply-text-edits :around #'my-around/lsp--apply-text-edits)
    (advice-add 'lsp--execute-code-action :around #'my-around/perhaps-lsp-will-edit-text)
    (advice-add 'lsp--before-save :around #'my-around/perhaps-lsp-will-edit-text)
    (advice-add 'lsp-format-buffer :around #'my-around/perhaps-lsp-will-edit-text)
    (advice-add 'lsp-rename :around #'my-around/perhaps-lsp-will-edit-text)
    )
   (t
    ;; disable
    (advice-remove 'lsp--apply-text-edits #'my-around/lsp--apply-text-edits)
    (advice-remove 'lsp--execute-code-action #'my-around/perhaps-lsp-will-edit-text)
    (advice-remove 'lsp--before-save #'my-around/perhaps-lsp-will-edit-text)
    (advice-remove 'lsp-format-buffer #'my-around/perhaps-lsp-will-edit-text)
    (advice-remove 'lsp-rename #'my-around/perhaps-lsp-will-edit-text)
    )
   )
)

(defun lsp-preview-text-edits-mode-toggle()
  (interactive)
  (lsp-preview-text-edits-mode 'toggle))

(provide 'lsp-preview-text-edits)
