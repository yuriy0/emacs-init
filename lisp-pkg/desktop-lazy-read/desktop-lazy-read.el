;; -*- lexical-binding: t; -*-

(require 'ht)
(require 'dash)
(require 'f)
(require 'desktop)
(require 'window)

(defvar-local desktop-lazy-restore-pending-buffers (ht))

(defvar desktop-first-buffer)
(defvar desktop-buffer-ok-count)
(defvar desktop-buffer-fail-count)

(defun my/get-visible-windows ()
  "Returns windows visibles on current frame."
  (let (result)
    (walk-windows
     (lambda (x) (push x result))
     nil 'visible)
    result))

(defun lazy-restore-buffer-invoke(act)
  (with-demoted-errors "Restoring buffer from desktop file failed: %S"
    (let ((desktop-first-buffer nil)
          (desktop-buffer-ok-count 0)
          (desktop-buffer-fail-count 0))
      (funcall act)
      )
    ))

(defun my-around/desktop-create-buffer(fn
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
        (message "Created dummy buffer %s for file %s" buf buffer-filename)
        (ht-set! desktop-lazy-restore-pending-buffers (f-canonical buffer-filename)
                 (list buf do-it))

        ;; add a buffer-local hook to `window-selection-change-functions', which
        ;; will be called any time this buffer is displayed. This hook will
        ;; actually initialize the buffer as desktop would normally do it.
        (with-current-buffer buf
          (add-hook-once
           'window-selection-change-functions
           (lambda (_visible-window)
             (message "Restoring buffer %s /file %s from desktop" buf buffer-filename)
             (lazy-restore-buffer-invoke do-it)
             t) ;; returning t removes the "once" hook
           nil;depth(default)
           t;local
           )
          nil)
        )
      )))

(defun my-around/create-file-buffer(fn filename)
  (setq filename (f-canonical filename))
  (if-let (buf (nth 0 (ht-get desktop-lazy-restore-pending-buffers filename)))
      (progn
        (message "Found buffer %s for file %s" buf filename)
        (ht-remove! desktop-lazy-restore-pending-buffers filename)
        buf
        )
    ;; (message "%s" `(funcall ,fn ,filename))
    (message "Did not find existing buffer to reuse for file %s" filename)
    (funcall fn filename)
    )

  ;; (funcall fn filename)
  )

;; (defun create-real-buffer-if-visible(visible-bufs buf-file-name dummy-buf make-buf-act)
;;   ;; (when (member (buffer-name dummy-buf) visible-bufs)
;;   ;;   (with-selected-window (window-buffer
;;   ;;   (funcall make-buf-act)
;;   ;;   t
;;   ;; )
;;   )

(defun my-after/desktop-read(&rest _args)
  ;; (-let [pending-by-buf
  ;;        (--> desktop-lazy-restore-pending-buffers
  ;;             ht->alist
  ;;             (-map (-lambda() ?))
  ;;             ht<-alist)
  ;;        ]
  
  ;; find all visible windows, and if any is displaying one of our dummy buffers, fill that buffer now
  (dolist (wind (my/get-visible-windows))
    (-let*
        ((wind-buf (window-buffer wind))
         ((buf-fname (_ mk-buf-act)) (ht-find (-lambda(_ (b _)) (eq b wind-buf)) desktop-lazy-restore-pending-buffers))
         )
      ;; 
      (when buf-fname
        (message "Restoring initially visible buffer %s /file %s from desktop" wind-buf buf-fname)
        (with-selected-window wind 
          ;; (message "%s" `(lazy-restore-buffer-invoke ,mk-buf-act))
          (lazy-restore-buffer-invoke mk-buf-act)
          )

        ;; just in case the restore fails, make sure to clear this dummy buffer and action (so we don't try again)
        (ht-remove! desktop-lazy-restore-pending-buffers buf-fname)
        )
      )
    ;; (let ((visible-bufs (get-visible-buffers)))
    ;;   (setq desktop-lazy-restore-pending-buffers
    ;;         (--> (ht->alist desktop-lazy-restore-pending-buffers)
    ;;              (-filter (-lambda((a (b . c))) (not (create-real-buffer-if-visible visible-bufs a b c))))
    ;;              ht<-alist))

    ;;   ;; (ht<-alist
    ;;   ;; (mapcar (lambda(x) (create-real-buffer-if-visible visible-bufs (nth 0 x) (nth 1 x)) (ht->alist desktop-lazy-restore-pending-buffers))
    ;;   ;; )

    ;; ;; (dolist (buf+act (get-visible-buffers))
    

    ;; ;;   ;; (when-let (buf (ht-get desktop-lazy-restore-pending-buffers (f-canonical filename)))
    ;; ;;   ;;   (message "Found buffer %s for file %s" buf filename)
    ;; ;;   ;;   (ht-remove! desktop-lazy-restore-pending-buffers filename)
    ;; ;;   ;;   buf
    ;; ;;   ;;   )
    ;; ;;   ;; )
    ;; ;; )
    ;; )
    ))

(advice-add 'create-file-buffer :around #'my-around/create-file-buffer '((depth . -100)))
(advice-add 'desktop-create-buffer :around #'my-around/desktop-create-buffer)
(add-hook 'desktop-after-read-hook #'my-after/desktop-read)

;; we need these settings to work properly.
;; - must be using eager restore, because we need to create buffers right away.
;; - must use absolute fps as we use canonical-f on the file paths, if those paths would be interpreted relative to
;;   some custom base path, the normalization makes no sense (canonical-f will treat "relative-looking" paths improperly then).
(setq
 desktop-restore-eager t
 desktop-file-name-format 'absolute)

(provide 'desktop-lazy-read)
