;;;###autoload
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device. Prevent issues with
   the Windows null device (NUL) when using cygwin find with rgrep."
  (let ((null-device "/dev/null"))
        ad-do-it))
(ad-activate 'grep-compute-defaults)
