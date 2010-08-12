;;; vm-summary-faces.el --- faces support for VM summary buffers
;;
;; This file is part of VM
;; 
;; Copyright (C) 2001 Robert Fenk
;; Copyright (C) 2010 Uday S Reddy
;;
;; Author:      Robert Fenk
;; Status:      Tested with XEmacs 21.4.15 & VM 7.18
;; Keywords:    VM 
;; X-URL:       http://www.robf.de/Hacking/elisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:
;;
;;  to use this add the following line to your ~/.vm file
;;
;;  (add-hook 'vm-summary-mode-hook 'vm-summary-faces-mode)
;;
;;; Code

(provide 'vm-summary-faces)

(eval-when-compile
  (require 'cl))

(eval-when-compile
  (require 'vm-misc))

(eval-and-compile
  (require 'vm-summary)
  (require 'vm-virtual))

;; (eval-and-compile
;;   (if vm-xemacs-p (require 'overlay)))

(declare-function vm-extent-property "vm-misc.el" (overlay prop) t)
(declare-function vm-set-extent-property "vm-misc.el" (overlay prop value) t)


(defgroup vm-summary-faces nil
  "VM additional virtual folder selectors and functions."
  :group 'vm)

(defcustom vm-summary-faces-alist
  '(
    ((or (header "Priority: urgent")
         (header "Importance: high")
         (header "X-Priority: 1")
         (label "!")
	 (label "\\flagged")
         (header "X-VM-postponed-data:"))
     vm-summary-high-priority-face)
    ((deleted)   	vm-summary-deleted-face)
    ((new)       	vm-summary-new-face)
    ((unread)    	vm-summary-unread-face)
    ((marked)    	vm-summary-marked-face)
    ((replied)   	vm-summary-replied-face)
    ((or (filed)
	 (written))     vm-summary-saved-face)
    ((or (forwarded) 
	 (redistributed)) vm-summary-forwarded-face)
    ((edited)    	vm-summary-edited-face)
    ((outgoing)  	vm-summary-outgoing-face)
    ((any)       	vm-summary-default-face))
  "*Alist of virtual folder conditions and corresponding faces.
Order matters. The first matching one will be used as the face.  

See `vm-virtual-folder-alist' for a description of the conditions."
  :type '(repeat (cons (sexp) (face)))
  :group 'vm-summary-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface vm-summary-selected-face
  '((t ;; (:bold on)
     (:background "grey85")
     ))
  "The face used in VM Summary buffers for the selected message."
  :group 'vm-summary-faces)

(defface vm-summary-marked-face
  '((((type x)) (:foreground "red3")))
  "The face used in VM Summary buffers for marked messages."
  :group 'vm-summary-faces)

(defface vm-summary-deleted-face
     (if (featurep 'xemacs)
         '((t (:foreground "grey50" :strikethru t)))
       '((t (:foreground "grey50" :strike-through "grey70"))))
     "The face used in VM Summary buffers for deleted messages."
     :group 'vm-summary-faces)

(defface vm-summary-new-face
  '((t (:foreground "blue")))
  "The face used in VM Summary buffers for new messages."
  :group 'vm-summary-faces)

(defface vm-summary-unread-face
  '((t (:foreground "blue4")))
  "The face used in VM Summary buffers for unread messages."
  :group 'vm-summary-faces)

(defface vm-summary-saved-face
  '((t (:foreground "green4")))
  "The face used in VM Summary buffers for saved messages."
  :group 'vm-summary-faces)

;; These faces are obsolete 
;; (define-obsolete-face-alias 'vm-summary-filed-face
;;   'vm-summary-saved-face "8.1.93a")
;; (define-obsolete-face-alias 'vm-summary-written-face
;;   'vm-summary-saved-face "8.1.93a")
(put 'vm-summary-filed-face 'face-alias 'vm-summary-saved-face)
(put 'vm-summary-written-face 'face-alias 'vm-summary-saved-face)
(make-obsolete 'vm-summary-filed-face 'vm-summary-saved-face "8.1.93a")
(make-obsolete 'vm-summary-written-face 'vm-summary-saved-face "8.1.93a")

(defface vm-summary-replied-face
  '((t (:foreground "grey30")))
  "The face used in VM Summary buffers for replied messages."
  :group 'vm-summary-faces)

(defface vm-summary-forwarded-face
  '((t (:foreground "grey20")))
  "The face used in VM Summary buffers for forwarded messages."
  :group 'vm-summary-faces)

;; (define-obsolete-face-alias 'vm-summary-redistributed-face
;;   'vm-summary-forwarded-face "8.1.93a")
(put 'vm-summary-redistributed-face 'face-alias
     'vm-summary-forwarded-face)
(make-obsolete 'vm-summary-redistributed-face
	       'vm-summary-forwarded-face "8.1.93a")

(defface vm-summary-edited-face 
  nil
  "The face used in VM Summary buffers for edited messages."
  :group 'vm-summary-faces)

(defface vm-summary-outgoing-face
  '((t (:foreground "grey30")))
  "The face used in VM Summary buffers for outgoing messages."
  :group 'vm-summary-faces)

(defface vm-summary-expanded-face
  '((t ()))
  "The face used in VM Summary buffers for the root messages of
expanded threads."
  :group 'vm-summary-faces)

(defface vm-summary-collapsed-face
  '((t (:weight normal :slant oblique)))
  "The face used in VM Summary buffers for the root messages of
collapsed threads."
  :group 'vm-summary-faces)

(defface vm-summary-high-priority-face
  '((t (:foreground "red")))
  "The face used in VM Summary buffers for high-priority messages."
  :group 'vm-summary-faces)

(defface vm-summary-default-face
  nil
  "The default face used in VM Summary buffers."
  :group 'vm-summary-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-and-compile
  (if (fboundp 'mapcar-extents)
      (defun vm-summary-faces-list-extents () (mapcar-extents 'identity))
    (defun vm-summary-faces-list-extents ()
      (let ((o (overlay-lists))) (nconc (car o) (cdr o))))))

(defvar vm-summary-faces-hide nil
  "Last face hidden by `vm-summary-faces-hide'.")

;;;###autoload
(defun vm-summary-faces-hide (&optional prop)
  "Toggle visibility of a particular vm-summary-face.  By
default, the deleted face is toggled (with the effect that all
deleted messages will be hidden or unhidden).  

With a prefix argument, the property name identifying the face is
queried interactively.  The property is a keyword such as edited,
collapsed or outgoing which has an associated face such as
vm-summary-edited-face.  See `vm-summary-faces-alist' for a list
of available face names."
  (interactive "P")
  (if (and (listp prop) (numberp (car prop)))
      (setq prop (completing-read "Face name: "
                                  (mapcar (lambda (f)
                                            (list (format "%s" (caar f))))
                                          vm-summary-faces-alist)
                                  nil t "deleted")))
  (setq prop (or prop vm-summary-faces-hide "deleted"))
  (vm-select-folder-buffer)
  (vm-summarize)
  (set-buffer vm-summary-buffer)
  (let ((extents (vm-summary-faces-list-extents))
        (face (intern (concat "vm-summary-" prop "-face")))
        x)
    (while extents
      (setq x (car extents)) 
      (when (equal face (vm-extent-property x 'face))
        (vm-set-extent-property 
	 x 'invisible (not (vm-extent-property x 'invisible))))
      (setq extents (cdr extents)))))

;;;###autoload
(defun vm-summary-faces-add (msg)
  "Add a face to a summary entry according to `vm-summary-faces-alist'."
  (let ((faces vm-summary-faces-alist)
        (x (or (vm-su-summary-mouse-track-overlay-of msg)
               (vm-extent-at (vm-su-start-of msg))
               (vm-extent-at (vm-su-end-of msg)))))
    (while faces
      (when (apply 'vm-vs-or msg (list (caar faces)))
	(cond ((vm-summary-collapsed-root-p msg)
	       (vm-set-extent-property 
		x 'face (list (cadar faces) 'vm-summary-collapsed-face)))
	      ((vm-summary-expanded-root-p msg)
	       (vm-set-extent-property
		x 'face (list (cadar faces) 'vm-summary-expanded-face)))
	      (t
	       (vm-set-extent-property
		x 'face (list (cadar faces)))))
        (setq faces nil))
      (setq faces (cdr faces)))))

(defun vm-summary-faces-destroy ()
  "Removes the face from all summary entries."
  (let ((extents (vm-summary-faces-list-extents))
        x)
    (while extents
      (setq x (car extents))
      (vm-set-extent-property x 'face nil)
      (setq extents (cdr extents)))))

;;;###autoload
(defun vm-summary-faces-mode (&optional arg)
  "Toggle `vm-summary-faces-mode'.  Optional argument ARG should be 0
or 1, indicating whether the summary faces should be off or on.

When it is on, the VM summary buffers are decorated with faces, i.e.,
fonts and colors, for easy recogniton of the message status."
  (interactive "P")
  (if (null arg)
      (setq vm-summary-enable-faces (not vm-summary-enable-faces))
    (if (> (prefix-numeric-value arg) 0)
        (setq vm-summary-enable-faces t)
      (setq vm-summary-enable-faces nil)))

  (when (interactive-p)
    (message "VM summary faces mode is %s"
             (if vm-summary-enable-faces "on" "off")))
  
  (if (memq major-mode '(vm-mode vm-virtual-mode vm-summary-mode
                                 vm-presentation-mode))
      (save-excursion
        (vm-select-folder-buffer)
        (vm-summarize)
        (set-buffer vm-summary-buffer)
        (if vm-summary-enable-faces
	    (progn
	      (mapc 'vm-summary-faces-add vm-message-list)
	      (if vm-summary-overlay
		  (vm-set-extent-property vm-summary-overlay 'face
					  'vm-summary-selected-face)))
          (vm-summary-faces-destroy)
          (if vm-summary-overlay
              (vm-set-extent-property vm-summary-overlay 'face
                                      vm-summary-highlight-face))))))

;; No need for advice because the code has been integrated into 
;; VM.  USR, 2010-08-01 

;; (defadvice vm-mouse-set-mouse-track-highlight 
;;	(after vm-summary-faces activate)
;;   (when (and vm-summary-enable-faces
;;              (eq major-mode 'vm-summary-mode)
;;              (boundp 'm)
;;              m)
;;     ;; FIXME there is a warning about a free variable here, sorry!
;;     (vm-summary-faces-add m)))

(defun vm-summary-faces-fix-pointer ()
  (if vm-summary-overlay
      (vm-set-extent-property vm-summary-overlay 'face
			            (if vm-summary-enable-faces
					'vm-summary-selected-face
				      vm-summary-highlight-face))))

(add-hook 'vm-summary-pointer-update-hook 'vm-summary-faces-fix-pointer)

