;;; Copyright (C) Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(defconst vm-version "7.19"
  "Version number of VM.")

(defvar vm-revno nil
  "The exact version for tarbundles.
This is the nick and revision number of the BZR repository from which VM was
released.")  

(defvar vm-author nil
  "The author for tarbundles.
This is the author of the BZR repository from which VM was released.")

(defun vm-version ()
  "Returns the value of the variable `vm-version'."
  (interactive)
  (save-excursion
    (unless (get-buffer " *vm-version*")
      (set-buffer (get-buffer-create " *vm-version*"))
      (let* ((f (locate-library "vm"))
             (d (file-name-directory f)))
        (setq default-directory d)
        (erase-buffer)
        (cond ((file-exists-p (expand-file-name ".bzr" (concat d "../")))
               ;; get the current branch nick and revno from bzr 
               (insert (concat (shell-command-to-string "bzr --no-plugins nick") "-"
                               (shell-command-to-string "bzr --no-plugins revno"))))
              ((and (locate-library "vm-revno") (load-library "vm-revno"))
               (insert vm-revno))
              (t
               (insert vm-version 
                       "-cannot_determine_precise_VM_version.")))
        (goto-char (point-min))
        ;; remove any whitespace 
        (while (re-search-forward "[\n\t\r ]+" (point-max) t)
          (replace-match ""))))
    (set-buffer (get-buffer " *vm-version*"))
    (buffer-substring (point-min) (point-max))))

(defconst vm-xemacs-p 
  (featurep 'xemacs))
(defconst vm-xemacs-mule-p 
  (and vm-xemacs-p (featurep 'mule)))
(defconst vm-xemacs-file-coding-p
  (and vm-xemacs-p (featurep 'file-coding)
       ;; paranoia
       (fboundp 
	'set-buffer-file-coding-system)))
(defconst vm-fsfemacs-p 
  (not vm-xemacs-p))
(defconst vm-fsfemacs-mule-p 
  (and (not vm-xemacs-mule-p) (featurep 'mule)
       (fboundp 'set-buffer-file-coding-system)))

(defun vm-xemacs-p () vm-xemacs-p)
(defun vm-xemacs-mule-p () vm-xemacs-mule-p)
(defun vm-xemacs-file-coding-p () vm-xemacs-file-coding-p)
(defun vm-fsfemacs-p () vm-fsfemacs-p)
(defun vm-fsfemacs-mule-p () vm-fsfemacs-mule-p)

(defun vm-mouse-fsfemacs-mouse-p ()
  (and vm-fsfemacs-p
       (fboundp 'set-mouse-position)))

(defun vm-mouse-xemacs-mouse-p ()
  (and vm-xemacs-p
       (fboundp 'set-mouse-position)))

(defun vm-menu-fsfemacs-menus-p ()
  (and vm-fsfemacs-p
       (fboundp 'menu-bar-mode)))

(defun vm-menu-fsfemacs19-menus-p ()
  (and vm-fsfemacs-p
       (fboundp 'menu-bar-mode)
       (= emacs-major-version 19)))

(defun vm-menu-xemacs-menus-p ()
  (and vm-xemacs-p
       (fboundp 'set-buffer-menubar)))

(defun vm-menu-can-eval-item-name ()
  (and vm-xemacs-p
       (fboundp 'check-menu-syntax)
       (condition-case nil
	   (check-menu-syntax '("bar" ((identity "foo") 'ding t)))
	 (error nil))))

(defun vm-multiple-frames-possible-p () 
  (cond (vm-xemacs-p 
	 (or (memq 'win (device-matching-specifier-tag-list))
	     (featurep 'tty-frames)))
        (vm-fsfemacs-p 
         (fboundp 'make-frame))))
 
(defun vm-mouse-support-possible-p () 
  (cond (vm-xemacs-p 
         (featurep 'window-system)) 
        (vm-fsfemacs-p 
         (fboundp 'track-mouse))))
 
(defun vm-mouse-support-possible-here-p ()
  (cond (vm-xemacs-p
	 (memq 'win (device-matching-specifier-tag-list)))
	(vm-fsfemacs-p
	 (memq window-system '(x mac w32 win32)))))

(defun vm-menu-support-possible-p ()
  (cond (vm-xemacs-p
	 (featurep 'menubar))
	(vm-fsfemacs-p
	 (fboundp 'menu-bar-mode))))
 
(defun vm-toolbar-support-possible-p ()
  (or (and vm-xemacs-p (featurep 'toolbar))
      (and vm-fsfemacs-p (fboundp 'tool-bar-mode) (boundp 'tool-bar-map))))

(defun vm-multiple-fonts-possible-p ()
  (cond (vm-xemacs-p
	 (memq (device-type) '(x gtk mswindows)))
	(vm-fsfemacs-p
	 (memq window-system '(x mac w32 win32)))))

(defun vm-images-possible-here-p ()
  (or (and vm-xemacs-p (memq (device-type) '(x gtk mswindows)))
      (and vm-fsfemacs-p window-system
	   (or (fboundp 'image-type-available-p)
	       (and (stringp vm-imagemagick-convert-program)
		    (stringp vm-imagemagick-identify-program))))))

(defun vm-image-type-available-p (type)
  (if (fboundp 'image-type-available-p)
      (image-type-available-p type)
    (or (featurep type) (eq type 'xbm))))

(provide 'vm-version)
