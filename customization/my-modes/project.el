(use-package project
  :config

  (defun project--buffer-list--fixed (pr)
    "Return the list of all buffers in project PR."
    (let ((conn (file-remote-p (project-root pr)))
          bufs)
      (dolist (buf (buffer-list))
        ;; For now we go with the assumption that a project must reside
        ;; entirely on one host.  We might relax that in the future.
        (when (and (equal conn
                          (file-remote-p 
                         ;;;; NOTE: this was broken because the buffer local value may be nil...
                           (or (buffer-local-value 'default-directory buf)
                               default-directory
                               )
                         ;;;; END NOTE
                           ))
                   (equal pr
                          (with-current-buffer buf
                            (project-current))))
          (push buf bufs)))
      (nreverse bufs)))
  (advice-add 'project--buffer-list :override 'project--buffer-list--fixed)
)
