;; Add the current dir to the load-path
(setq load-path (cons default-directory load-path))

;; Add additional dirs to the load-path
(if (getenv "OTHERDIRS")
    (let ((ps (delete "" (split-string (getenv "OTHERDIRS") "[:;]"))))
      (while ps
;        (message "adding user load-path: <%s>" (car ps))
        (setq load-path (cons (car ps) load-path)
              ps (cdr ps)))))

;; Load byte compile 
(require 'bytecomp)
(setq byte-compile-warnings '(free-vars))
(put 'inhibit-local-variables 'byte-obsolete-variable nil)

;; Preload these to get macros right 
(require 'cl)
(require 'vm-version)
(require 'vm-message)
(require 'vm-macro)
(require 'vm-vars)
(require 'sendmail)

(defun vm-fix-cygwin-path (path)
  (cond ((functionp 'mswindows-cygwin-to-win32-path)
         (mswindows-cygwin-to-win32-path path))
        ((string-match "^/cygdrive/\\([a-z]\\)" path)
         (replace-match (format "%s:" (match-string 1 path)) t t path))
        (t
         path)))

(defun vm-built-autoloads ()
  (setq file (car command-line-args-left)
	dir (car (cdr command-line-args-left))
        debug-on-error t)
  (if (not (file-exists-p dir))
      ;; to handle emacsW32 path problems when building on cygwin
      (setq file (vm-fix-cygwin-path file)
            dir (vm-fix-cygwin-path dir)))
  (if (not (file-exists-p dir))
      (error "Built %S directory does not exist!" dir))
  (message "Building autoloads file %S\nin directory %S." file dir)
  (load-library "autoload")
  (set-buffer (find-file-noselect file))
  (erase-buffer)
  (setq generated-autoload-file file)
  (setq make-backup-files nil)
  (if (featurep 'xemacs)
      (error "This is only for GNU Emacs.")
    ;; GNU Emacs 21 wants some content, but 22 does not like it ...
    (insert ";;; vm-autoloads.el --- automatically extracted autoloads\n")
    (insert ";;\n")
    (insert ";;; Code:\n")
    (if (>= emacs-major-version 21)
	(update-autoloads-from-directories dir)
      (if (>= emacs-major-version 22)
	  (update-directory-autoloads dir)
	(error "Do not know how to generate autoloads")))))

(provide 'vm-build)
