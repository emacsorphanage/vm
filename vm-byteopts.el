;; Add the current dir to the load-path
(setq load-path (cons default-directory load-path))
;; Add additional dirs to the load-path
(if (getenv "OTHERLISPDIRS")
    (let ((ps (delete "" (split-string (getenv "OTHERLISPDIRS") "[ :;]"))))
      (while ps
        (message "adding user load-path: <%s>" (car ps))
        (setq load-path (cons (car ps) load-path)
              ps (cdr ps)))))
;; get the compiler loaded so we can undo some of the things that
;; happen when it's loaded.
(load "bytecomp" t t nil)
;; Emacs 19 byte compiler complains about too much stuff by default.
;; Turn off most of the warnings here.
(setq byte-compile-warnings '(free-vars))
;; need to use these variables for v18 support.
;; stifle the compiler.
(put 'inhibit-local-variables 'byte-obsolete-variable nil)
(setq byte-compile-dynamic t)
(setq byte-compile-dynamic-docstrings t)
;; avoid v20 features because users are going
;; to try to share elc files no matter what we tell them.
(setq byte-compile-emacs19-compatibility t)

(provide 'vm-byteopts)
