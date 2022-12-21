;;;  -*- lexical-binding: t; -*- 

;;;###autoload
(defmacro with-negated-prefix-arg (fn) 
  "Defines a lambda which takes a prefix arg
and passes the negated version to the given function, 
along with any other arguments"
  `(lambda (k &rest args) 
     (interactive "p") 
     (apply ,fn (cons (- k) args))
     ))

;;;###autoload
(defun kill-all-buffers ()
  "Kill every buffer except the current one."
  (interactive)
  (funcall-interactively 'kill-old-buffers 1))

;;;###autoload
(defun recent-file-buffers-list (&optional keep-cur) 
  (-let*
      ((bufs (-filter 'buffer-file-name (buffer-list))) 
       (bufs-sorted bufs)
       )
    (if keep-cur 
        (-let*
            ((curr (current-buffer))
             (curr-i (-elem-index curr bufs-sorted))
             (bufs-sorted-curr
              (if curr-i
                  (cons curr (-remove-at curr-i bufs-sorted))
                bufs-sorted))
             )
          bufs-sorted-curr)
      bufs-sorted)))

;;;###autoload
(defun kill-old-buffers (count-to-keep)
  "Kill the oldest buffers visiting files, keeping
`COUNT-TO-KEEP' buffers. The current buffer is considered the
newest buffer for this purpose (that is, when `COUNT-TO-KEEP' is
1, all but the current buffer is killed)."
  (interactive "p")
  (-let* 
  ( (bufs-sorted-curr (recent-file-buffers-list))
    (bufs-to-kill (-drop count-to-keep bufs-sorted-curr))
    (bufs-str (s-join ", " (-map 'buffer-name bufs-to-kill)) )
    )
  (progn
    (when 
        (y-or-n-p (format "Kill buffers: %s?" bufs-str))
      (message "Killing %s" bufs-str)
      (-each bufs-to-kill 'kill-buffer)) )))
(global-set-key (kbd "C-x C-\\") 'kill-old-buffers)

;;;###autoload
(defun shell-command-silent (cmd)
  "Execute the shell command but do not open the MESSAGES buffer even if there is output."
  (interactive "sCommand: ")
  (with-temp-buffer (shell-command cmd t)))
(global-set-key (kbd "M-#") 'shell-command-silent)

;;;###autoload
(defun toggle-window-split ()
  "If frame is split into two horizontally/vertically, split it vertically/horizontally instead."
  (interactive)
  (if (> (count-windows) 1)
      (let* ((this-win-buffer (window-buffer))
           (next-win-buffer (window-buffer (next-window)))
           (this-win-edges (window-edges (selected-window)))
           (next-win-edges (window-edges (next-window)))
           (this-win-2nd (not (and (<= (car this-win-edges)
                                       (car next-win-edges))
                                   (<= (cadr this-win-edges)
                                       (cadr next-win-edges)))))
           (splitter
            (if (= (car this-win-edges)
                  (car (window-edges (next-window))))
               'split-window-horizontally
               'split-window-vertically)))
        (if this-win-2nd 
          (delete-window (selected-window))
          (delete-window (other-window 1))
          )
        (let ((first-win (selected-window)))
        (funcall splitter)
        (if this-win-2nd (other-window 1))
        (set-window-buffer (selected-window) this-win-buffer)
        (set-window-buffer (next-window) next-win-buffer)
        (select-window first-win)
        (if this-win-2nd (other-window 1))))))
(global-set-key (kbd "C-x 4 t") 'toggle-window-split)

;;;###autoload
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;###autoload
(defun delete-dos-eol ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward (concat (char-to-string 13) "$") (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d CR removed from buffer." remove-count))))))

;; Counts how many times `regex' matches in `string'
(defun count-occurences (regex string)
  (let ((current-index 0)
        (next-index nil)
        (count 0)
        )
    (while
        (and
         (setq next-index (string-match regex string current-index))
         (not (equal next-index current-index))
         )
      (setq current-index (+ 1 next-index))
      (setq count (+ 1 count))
    )
    count
   )
)

;; Entire contents of a buffer with no properties
(defun buffer-string-no-prop ()
  (save-restriction
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))
  )
)

;;;###autoload
(defun normalize-eol ()
  "Tries to guess what type of line endings the current buffer should have,
and replaces any which are inconsistent."
  (interactive)
  (save-match-data
    (save-excursion
      (let* ((buf-str (buffer-string-no-prop))
             (eol-c (count-occurences "\n" buf-str))
             (dos-eol-c (count-occurences "\r\n" buf-str))
             (nix-eol-c (count-occurences "\n$" buf-str))
            )
        (cond
         ((or (equal dos-eol-c 0) (equal nix-eol-c 0))
          (message "Line endings already consistent")
          )

         ((>= nix-eol-c dos-eol-c) (delete-dos-eol))

         ((< nix-eol-c dos-eol-c)
          (message "TODO: REPLACE LF WITH CRLF")
          )
         )))))

;;;###autoload 
(defun canon-win-path (path)
  "Convert the given path to a canonical, Windows path"
  (replace-regexp-in-string "/" "\\\\" (convert-standard-filename (expand-file-name path))))

;;;###autoload
(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))

;;;###autoload
(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos t))

;;;###autoload
(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-mac t))

;; http://batsov.com/articles/2011/11/12/emacs-tip-number-2-open-file-in-external-program/
;;;###autoload
(defun prelude-open-with ()
  "Open the underlying file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command-silent (concat "cmd /C start \"\" \"" buffer-file-name "\""))))
(global-set-key (kbd "C-c o") 'prelude-open-with)

;;;###autoload
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
;;;###autoload
(defun rename-file-and-buffer ()
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (if (not (buffer-file-name))
      (message "Buffer '%s' is not visiting a file!" (buffer-name))
    (call-interactively 'do-rename-file-and-buffer) ) )

;;;###autoload
(defun do-rename-file-and-buffer (new-name)
  (interactive
   (let* ((pr (file-name-nondirectory (buffer-file-name)))
          (str (read-string "New name: " pr)))
     (list str) ))
  (let ((name (buffer-name)))
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file (file-name-nondirectory name) new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))

;; based on: https://www.emacswiki.org/emacs/RevertBuffer#toc1
;;;###autoload
(defun revert-all-buffers ()
  "Iterate through the list of buffers and revert them, e.g. after a
    new branch has been checked out."
  (interactive)
  (when (yes-or-no-p "Are you sure - any changes in open buffers will be lost! ")
    (-let* 
        ((bufs-todo
          (-filter 'buffer-file-name (buffer-list)))
         ((to-rev to-del)
          (-map 'cdr (-group-by (-compose 'file-exists-p 'buffer-file-name) bufs-todo)))
         )
      (when to-del 
        (message "Deleting %s" to-del))
      (--each to-rev (with-current-buffer it (revert-buffer t t t)))
      (--each to-del (funcall-interactively 'kill-buffer it)) )))

;;;###autoload
(defun enclose-region-in (before-str after-str)
  "Insert the given text pair before/after the region."
  (interactive "sBefore text: \nsAfter text: ")
  (progn
   (save-excursion
     (goto-char (region-beginning))
     (insert before-str))
   (save-excursion
     (goto-char (region-end))
     (insert after-str))))

;;;###autoload
(defun unary-comment-syntax ()
  (if (string= comment-end "") (s-trim-right comment-start) nil))

(defun s-replicate (len str)
  "Repeats `STR' until it has length exactly `LEN'. If `STR' is
 shorter than `LEN', the prefix of length `LEN' is returned"
  (let* ((str-len (length str))
         (count (/ len str-len))
         (str-out (s-left len (s-repeat (1+ count) str)))
         )
    str-out))

;;;###autoload
(defun enclose-region (&optional sep-str-arg)
  "Enclose the marked region in a box made of `SEP-STR-ARG', 
   or the comment string if the arg is nil (otherwise does nothing).
   If the region is not at the beginning/end of the line, the enclosing 
   box seperates the previous/subsequent text with a newline. 
   Does not work very well if the region spans multiple lines."
  (interactive (list (if 
    current-prefix-arg 
    (read-from-minibuffer "Enclose region with: ") 
    nil)))
  (let ((sep-str (or sep-str-arg (unary-comment-syntax))))
    (if sep-str
      (if (use-region-p)
        (let*
         ((sep-str-sz (length sep-str))
          (beg (region-beginning)) (end (region-end))
          (region-sz (- end beg))
          (region-sep-sz (+ region-sz (* 2 sep-str-sz) 2) )
          (enc-str (s-replicate region-sep-sz sep-str))
          (enc-strs (list enc-str "\n" sep-str " "))
          (before-str (apply 'concat (cons (if (eq beg (line-beginning-position)) "" "\n") enc-strs)))
          (after-str (apply 'concat (reverse (cons (if (eq end (line-end-position)) "" "\n") enc-strs))))
          )
        (enclose-region-in before-str after-str))
        (error (message "No region selected.")))
      (error (message "No seperator string given and no single-line comment syntax defined.")))))

(defun text-hrule ()
  "Inserts a horizontal rule consisting of comment characters (if
there is a comment syntax) starting from current point to length
of `fill-column'."
  (interactive)
  (let ((str (unary-comment-syntax)))
    (if str
        (insert (s-replicate (- fill-column (current-column)) str))
      (user-error "No comment syntax defined"))))
(global-set-key (kbd "C-h C-r") 'text-hrule)


;;;###autoload
(defun reload-emacs ()
  "(Re)Loads the emacs init file"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;;;###autoload
;; https://superuser.com/questions/132225/how-to-get-back-to-an-active-minibuffer-prompt-in-emacs-without-the-mouse
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))
(global-set-key (kbd "C-c m") 'switch-to-minibuffer-window)

;;;###autoload
(defun delete-file-and-kill-buffer ()
  "Delete the file visited by the current buffer, then kill the
current buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (condition-case nil
          (progn
            (if (vc-backend filename)
                (vc-delete-file filename)
              (delete-file filename t))
            (message "Deleted file %s" filename) )
        (error (message "Failed to delete file %s" filename)) ) )
    (kill-buffer)))
(global-set-key (kbd "C-c D") 'delete-file-and-kill-buffer)



;;;###autoload
(defun add-hook-once (hook-var hook-fn &optional depth local)
  "Add `hook-fn' to all of hooks found in `hook-vars' (list or single hook); the first time that `hook-fn' returns `t', all of the installed hooks will be removed"
  (letrec
      ((wrapped-hook-fn
        (lambda (&rest args)
          (if (apply hook-fn args) (funcall remove-hooks))))
       (remove-hooks
         (if (listp hook-var)
             (lambda ()
               (mapc (lambda(h) (remove-hook h wrapped-hook-fn local)) hook-var))
             (lambda ()
               (remove-hook hook-var wrapped-hook-fn local))
             )
         )
       )
    (if (listp hook-var)
        (mapc (lambda(h) (add-hook h wrapped-hook-fn depth local)) hook-var)
        (add-hook hook-var wrapped-hook-fn depth local)
      )
    ))

(defmacro init-once (symbol &rest body)
  "If SYMBOL is nil, execute the forms in BODY and bind the result to the value of SYMBOL"
  `(if ,symbol
       ,symbol
     (setq ,symbol (progn ,@body)))
)


;;;###autoload
(defun filter-buffer-list-frame-parameters (frame filter &optional filter-burried)
  (set-frame-parameter nil
                       'buffer-list
                       (-filter (-partial #'funcall filter) (frame-parameter nil 'buffer-list)))
  (set-frame-parameter nil
                       'buried-buffer-list
                       (-filter (-partial #'funcall (or filter-burried filter)) (frame-parameter nil 'buried-buffer-list))))


(defmacro unquote (arg)
  (if (and (consp arg)
           (eq (car arg) 'quote))
      (cadr arg)
    arg))

(defmacro add-to-list-at (list index el)
  (let ((list-exp (macroexpand `(unquote ,list))))
    `(setq ,list-exp (-insert-at ,index ,el ,list-exp))
  ))

(defmacro lam (&rest args)
  (-let [(vars body) (-split-on :-> args)]
    (if body
        `(lambda ,vars ,body)
      `(lambda () ,vars)
    )))

(defun lexographic< (a b)
  (pcase (list a b)
    (`((,a0 . ,as) (,b0 . ,bs))
     (cond
      ((lexographic< a0 b0) t) ;; lt
      ((lexographic< b0 a0) nil) ;; gt
      (t (lexographic< as bs))
      )
     )

    (`(nil nil)
     nil) ;;eq

    (`(nil ,_)
     t) ;;lt

    (`(,_ nil)
     nil) ;;gt

    (t (< a b)) ;;not two lists
    )
)

(defun -sort-by-key (compare keyfn seq)
  ;; (cl-stable-sort seq compare :key keyfn)
  (-sort (lambda(x y) (funcall compare (funcall keyfn x) (funcall keyfn y))) seq)
)

(defmacro override-fun-nonrecursive (orignm overridenm &rest body)
  "Within the scope of BODY, the function symbol `originm' will in fact refer to `overridenm',
except that if the function `overridenm' actually calls the
function symbol `originm', that second (recursive) call will
invoke the original function."
  (declare (indent defun))
  `(let* ((inside-inner 0)
          (original (symbol-function (quote ,orignm)))
          (overridesfn
           (lambda (&rest innerargs)
             (cl-letf
                 (
                  ((symbol-function (quote ,orignm)) original)
                  )
               (apply ,(cond
                        ((symbolp overridenm)
                         `(function ,overridenm))
                        ((functionp overridenm)
                         overridenm)
                        (t
                         (error "expecting functionp, got %S" overridenm))
                        )
                      innerargs)
               )
             )
           )
          )
     (cl-letf
         (
          ((symbol-function (quote ,orignm)) overridesfn)
          )
       (progn ,@body)
       )
     )
  )


(defmacro modf (place func)
  "Apply `FUNC' to `PLACE' (a gv) and set the value of `PLACE' to the return of the `FUNC'"
  `(setf ,place
         ,(if (listp func) (append func (list place)) (list func place))
     )
  )

(defmacro modf-v (place var &rest args)
  "Retrieve the value of the `PLACE', bind it to the variable
`VAR' and execute `ARGS', then set the value of `PLACE' to the
value produced by `ARGS'"
  `(setf ,place (let ((,var ,place)) (progn ,@args))))

(defun buffer-name-or-string (name-or-buffer)
  (if (stringp name-or-buffer) name-or-buffer (buffer-name name-or-buffer)))


(defmacro with-temp-buffer-var (var &rest body)
  "Like `with-temp-buffer' except that in the context of `BODY' the symbol `var' is bound to the temporary buffer. Useful if you want to create multiple temporary buffers"
  (declare (indent defun) (debug t))
  (macroexpand
   `(with-temp-buffer
      (let ((,var (current-buffer)))
        (progn ,@body)
        ))))

;;;###autoload
(defun shell-command-with-stdin (cmd stdin)
  "Call a shell command and pipe the given string to the standard
input of the process. This is exactly like
`shell-command-on-region' except with a literal string. Use this
instead of trying to format piping to stdin of shell commands
manually (i.e. by writing `shell-command-to-string (format \"echo %s | CMD\")`) because
this uses `shell-command-on-region' which correctly handles all
sorts of exotic text contents.

Returns a list `(ERRORCODE STDOUTANDERR)'"
  (let ((stdoutstr) (resultcode)
        )
    (with-temp-buffer
         (insert stdin)
         (list
          (call-shell-region
           (point-min) (point-max)
           cmd
           t ; delete contents
           '(t t) ; write stdout AND stderr to current buffer
           )

          ;; at this point call-shell-region has written the stdout/err to the buffer
          (buffer-string))
         )
    )
  )

(defmacro if-with-rate-limit (rate func then &rest else)
  "Eval THEN forms if the symbol FUNC has **not** been passed to this
  function for RATE seconds; otherwise eval ELSE form"
  `(let ((last-called (get ,func 'last-called-time))
        (tnow (float-time)))
    (if (or (not last-called)
            (< (+ last-called ,rate) tnow))
        (progn
          (put ,func 'last-called-time tnow)
          ,then
          )
      ,@else))
)

;;;###autoload
(defun add-lines-to-point(buffer point lines-to-add)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (save-excursion
      (goto-char point)
      (line-move lines-to-add t)
      (point))))

;;;###autoload
(defun expand-position-range-by-lines(buffer a b more)
  (list (add-lines-to-point buffer a (- more))
        (add-lines-to-point buffer b more)))


;;;###autoload
(defun make-repeat-map-symbol-default(sym)
  (let* ((nm (symbol-name sym))
         (nm-parts (s-split "-map" nm))
         )
    (intern
     (concat
      (pcase nm-parts
        (`(,pref "") pref)
        (_ nm))
      "-repeat-map"))))

(defun make-prefixed-keybinds (prefix args)
  (mapcar (-lambda ((key . fun)) (cons (concat prefix " " key) fun)) args))

(defmacro bind-keys-repeating (map repeat-map_ prefix &rest args)
  (declare (indent 3))
  (let ((repeat-map_ (or repeat-map_ (make-repeat-map-symbol-default map)))
        (binds (make-prefixed-keybinds prefix args))
        )
    `(bind-keys :map ,map
                 ,@binds
                 :repeat-map ,repeat-map_
                 ,@args)
     ))

