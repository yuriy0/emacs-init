;; -*- lexical-binding: t; -*-

(require 'ht)
(require 'dash)
(require 'f)
(require 'desktop)
(require 'window)

(defvar-local desktop-lazy-restore-pending-buffers (ht))
(defvar-local desktop-lazy-restore-read-desktopfile-completed nil)

(defvar desktop-first-buffer)
(defvar desktop-buffer-ok-count)
(defvar desktop-buffer-fail-count)

(defcustom desktop-lazy-restore-completed-hook nil
  "Hook to run when desktop lazy restoring is completed")

(defun my/get-visible-windows ()
  "Returns windows visibles on current frame."
  (let (result)
    (walk-windows
     (lambda (x) (push x result))
     nil 'visible)
    result))

(defun lazy-restore-buffer-invoke(act)
  (with-demoted-errors "Desktop: restoring buffer failed: %S"
    (let ((desktop-first-buffer nil)
          (desktop-buffer-ok-count 0)
          (desktop-buffer-fail-count 0))
      (funcall act))
    ))

(defun my-around/desktop-create-buffer
    (fn
     file-version
     buffer-filename
     buffer-name &rest args)
  (let ((do-it
         (lambda() (apply fn file-version buffer-filename buffer-name args))))

    (if (or (not buffer-filename) (equal buffer-filename ""))
        ;; if not visiting a file, do everything normally, as we don't have a
        ;; good way to "automagically" substitute the right buffer later
        (funcall do-it)

      (let ((buf (generate-new-buffer
                  buffer-name
                  t;inhibit hooks
                  )))

        ;; save our dummy buffer for later...
        (message "Desktop: created dummy buffer '%s' for file '%s'" buf buffer-filename)
        (ht-set! desktop-lazy-restore-pending-buffers (f-canonical buffer-filename)
                 (list buf do-it))
        )
      )))

(defun my-around/create-file-buffer(fn filename)
  (setq filename (f-canonical filename))
  (if-let (buf (nth 0 (ht-get desktop-lazy-restore-pending-buffers filename)))
      (progn
        (message "Desktop: found buffer '%s' for file '%s'" buf filename)
        (ht-remove! desktop-lazy-restore-pending-buffers filename)
        buf
        )
    ;; (message "%s" `(funcall ,fn ,filename))
    (message "Did not find existing buffer to reuse for file %s" filename)
    (funcall fn filename))
  )

(defun my/check-restore-any-visible-windows(&rest _args)
  ;; find all visible windows, and if any is displaying one of our dummy buffers, fill that buffer now
  (dolist (wind (my/get-visible-windows))
    (-let*
        ((wind-buf (window-buffer wind))
         ((buf-fname (_ mk-buf-act)) (ht-find (-lambda(_ (b _)) (eq b wind-buf)) desktop-lazy-restore-pending-buffers))
         )
      ;; if we have found a corresponding lazy restore buffer for the given window...
      (when buf-fname
        (message "Restoring visible buffer '%s' (file '%s') from desktop" wind-buf buf-fname)
        (with-selected-window wind (lazy-restore-buffer-invoke mk-buf-act))

        ;; just in case the restore fails, make sure to clear this dummy buffer and action (so we don't try again)
        (ht-remove! desktop-lazy-restore-pending-buffers buf-fname)
        )
      )
    )

  ;; if there's no more buffers to restore, remove our hooks
  (my/desktop-lazy-read-check-fin)
  )

(defun my/desktop-after-read-hook (&rest _args)
  (setq desktop-lazy-restore-read-desktopfile-completed t)
  (my/check-restore-any-visible-windows))

(advice-add 'create-file-buffer :around #'my-around/create-file-buffer '((depth . -100)))
(advice-add 'desktop-create-buffer :around #'my-around/desktop-create-buffer)
(add-hook 'desktop-after-read-hook #'my/desktop-after-read-hook)
(add-hook 'window-selection-change-functions #'my/check-restore-any-visible-windows)

(defun desktop-lazy-ready-completed-p()
  (and desktop-lazy-restore-read-desktopfile-completed
       (ht-empty-p desktop-lazy-restore-pending-buffers)))

(defun my/desktop-lazy-read-check-fin()
  (when (desktop-lazy-ready-completed-p)
    (message "Desktop lazy read completed; all buffers restored")
    (advice-remove 'create-file-buffer #'my-around/create-file-buffer)
    (advice-remove 'desktop-create-buffer #'my-around/desktop-create-buffer)
    (remove-hook 'desktop-after-read-hook #'my/desktop-after-read-hook)
    (remove-hook 'window-selection-change-functions #'my/check-restore-any-visible-windows)
    (setq desktop-lazy-restore-read-desktopfile-completed nil)
    (run-hooks 'desktop-lazy-restore-completed-hook)
  ))

;; we need these settings to work properly.
;; - must be using eager restore, because we need to create buffers right away.
;; - must use absolute fps as we use canonical-f on the file paths, if those paths would be interpreted relative to
;;   some custom base path, the normalization makes no sense (canonical-f will treat "relative-looking" paths improperly then).
(setq
 desktop-restore-eager t
 desktop-file-name-format 'absolute)

(provide 'desktop-lazy-read)
