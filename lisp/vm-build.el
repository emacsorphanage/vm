;; Add the current dir to the load-path
(setq load-path (cons default-directory load-path))

;; Add additional dirs to the load-path
(if (getenv "OTHERLISPDIRS")
    (let ((ps (delete "" (split-string (getenv "OTHERLISPDIRS") "[ :;]"))))
      (while ps
        (message "adding user load-path: <%s>" (car ps))
        (setq load-path (cons (car ps) load-path)
              ps (cdr ps)))))

;; Load byte compile 
(require 'bytecomp)
(setq byte-compile-warnings '(free-vars))
(put 'inhibit-local-variables 'byte-obsolete-variable nil)

;; Preload these to get macros right 
(require 'vm-version)
(require 'vm-message)
(require 'vm-macro)
(require 'vm-vars)
(require 'sendmail)

(provide 'vm-build)
