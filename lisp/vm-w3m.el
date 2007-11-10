;;; vm-w3m.el --- additional functions to make VM use emacs-w3m for HTML mails

;; Copyright (C) 2003, 2005, 2006 Katsumi Yamaoka,
;; Copyright (C)             2007 Robert Widhopf-Fenk

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; You need to have w3m and emacs-w3m installed for this module to
;; work.  Visit <URL:http://emacs-w3m.namazu.org/> for details.
;; You don't have to change VM at all.  Simply load this module and
;; you will see HTML mails inlined by emacs-w3m in the VM presentation
;; buffer.

;;; Code:

(require 'vm-mime)
(require 'w3m)

(defvar vm-message-pointer)
(defvar vm-mode-map)
(defvar vm-w3m-mode-map)
(defvar w3m-minor-mode-map)

(defcustom vm-w3m-display-inline-images t
  "Non-nil means VM will allow retrieving images in the HTML contents
with the <img> tags.  See also the documentation for the variable
`vm-w3m-safe-url-regexp'."
  :group 'w3m
  :type 'boolean)

(defcustom vm-w3m-safe-url-regexp "\\`cid:"
  "Regexp matching URLs which are considered to be safe.
Some HTML mails might contain a nasty trick used by spammers, using
the <img> tag which is far more evil than the [Click Here!] button.
It is most likely intended to check whether the ominous spam mail has
reached your eyes or not, in which case the spammer knows for sure
that your email address is valid.  It is done by embedding an
identifier string into a URL that you might automatically retrieve
when displaying the image.  The default value is \"\\\\`cid:\" which only
matches parts embedded to the Multipart/Related type MIME contents and
VM will never connect to the spammer's site arbitrarily.  You may set
this variable to nil if you consider all urls to be safe."
  :group 'w3m
  :type '(choice (regexp :tag "Regexp")
		 (const :tag "All URLs are safe" nil)))

(defcustom vm-w3m-use-w3m-minor-mode-map t
  "Say whether to use emacs-w3m command keys in VM presentation buffers.
Set this variable to nil if you don't want vm-w3m to override any VM
commend keys.  If it is non-nil, you will not be able to use some VM
command keys, which are bound to emacs-w3m commands defined in the
`w3m-minor-mode-command-alist' variable."
  :group 'w3m
  :type 'boolean)

(eval-and-compile
  (or (featurep 'xemacs) (>= emacs-major-version 21)
      (defvar vm-w3m-mode-map nil
	"Keymap for text/html parts inlined by emacs-w3m.
This keymap will be bound only when Emacs 20 is running and overwritten
by the value of `w3m-minor-mode-map'.  In order to add some commands to
this keymap, add them to `w3m-minor-mode-map' instead of this keymap.")))

(defun vm-w3m-cid-retrieve (url &rest args)
  "Insert a content pointed by URL if it has the cid: scheme."
  (if (string-match "\\`cid:" url)
      (let ((part-list (save-excursion
			 (set-buffer w3m-current-buffer)
			 (vm-mm-layout-parts
			  (vm-mm-layout (car vm-message-pointer)))))
	    part start type)
	(setq url (concat "<" (substring url (match-end 0)) ">"))
	(while (and part-list (not type))
          (setq part (car part-list))
          (if (vm-mime-composite-type-p (car (vm-mm-layout-type part)))
              (setq part-list (nconc (copy-sequence (vm-mm-layout-parts part))
                                     (cdr part-list))))
	  (setq part-list (cdr part-list))
	  (if (equal url (vm-mm-layout-id part))
	      (progn
		(setq start (point))
		(vm-mime-insert-mime-body part)
		(vm-mime-transfer-decode-region part start (point))
		(setq type (car (vm-mm-layout-type part))))))
	type)))

(or (assq 'vm-presentation-mode w3m-cid-retrieve-function-alist)
    (setq w3m-cid-retrieve-function-alist
	  (cons '(vm-presentation-mode . vm-w3m-cid-retrieve)
		w3m-cid-retrieve-function-alist)))

(defun vm-w3m-local-map-property ()
  (if (and (boundp 'w3m-minor-mode-map) w3m-minor-mode-map)
      (if (or (featurep 'xemacs) (>= emacs-major-version 21))
	  (list 'keymap w3m-minor-mode-map)
	(list 'local-map
	      (or vm-w3m-mode-map
		  (progn
		    (setq vm-w3m-mode-map
			  (copy-keymap w3m-minor-mode-map))
		    (set-keymap-parent vm-w3m-mode-map vm-mode-map)
		    vm-w3m-mode-map))))))

(defun vm-mime-display-internal-w3m-text/html (start end layout)
  "Use emacs-w3m to inline HTML mails in the VM presentation buffer."
  (setq w3m-display-inline-images vm-w3m-display-inline-images)
  (let ((w3m-safe-url-regexp vm-w3m-safe-url-regexp))
    ;; do not mess up the scrollbar, i.e. we want to see the start of the
    ;; message not the end of this part after decoding ...
    (save-window-excursion
      (w3m-region start (1- end)))
    (add-text-properties
     start end
     (nconc (if vm-w3m-use-w3m-minor-mode-map
                (vm-w3m-local-map-property))
            ;; Put the mark meaning that this part was
            ;; inlined by emacs-w3m.
            '(text-rendered-by-emacs-w3m t)))))

(eval-when-compile
  (defvar vm-mail-buffer)
  (defvar vm-presentation-buffer)
  (autoload 'vm-buffer-variable-value "vm-misc"))

(defun vm-w3m-safe-toggle-inline-images (&optional arg)
  "Toggle displaying of all images in the presentation buffer.
If the prefix arg is given, all images are considered to be safe."
  (interactive "P")
  (let ((buffer (cond ((eq major-mode 'vm-summary-mode)
		       (vm-buffer-variable-value vm-mail-buffer
						 'vm-presentation-buffer))
		      ((eq major-mode 'vm-presentation-mode)
		       (current-buffer))
		      ((eq major-mode 'vm-mode)
		       vm-presentation-buffer))))
    (if (buffer-live-p buffer)
	(save-excursion
	  (set-buffer buffer)
	  (w3m-safe-toggle-inline-images arg)))))

(provide 'vm-w3m)
;;; vm-w3m.el ends here
