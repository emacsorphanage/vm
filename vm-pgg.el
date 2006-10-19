;;; vm-pgg.el --- PGP(/MIME) support for VM by pgg.el
;; 
;; Copyright (C) 2006 Robert Widhopf-Fenk
;;
;; Author:      Robert Widhopf-Fenk
;; Status:      Tested with XEmacs 21.4.19 & VM 7.19
;; Keywords:    VM helpers
;; X-URL:       http://www.robf.de/Hacking/elisp
;; Version:     $Id$

;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; This is a replacement for mailcrypt adding PGP/MIME support to VM. 
;;
;; It is still in BETA state thus you must explicitly load it by
;; 
;;      (and (locate-library "vm-pgg") (require 'vm-pgg))
;;
;; To customize vm-pgg use: M-x customize-group RET vm-pgg RET 
;;
;; Displaying of messages in the PGP/MIME format will automatically trigger:
;;  * decrypted of encrypted MIME parts
;;  * verification of signed MIME parts 
;;  * snarfing of public keys
;;
;; To create messages according to PGP/MIME you should use:
;;  * M-x vm-pgg-encrypt       for encrypting
;;  * M-x vm-pgg-sign          for signing  
;;  * C-u M-x vm-pgg-encrypt   for encrypting + signing 
;;
;; All these commands are also available in the menu PGP/MIME which is
;; activated by the minor mode `vm-pgg-compose-mode'.
;;
;; If you get annoyed by answering password prompts you might want to set the
;; variable `password-cache-expiry' to a higher value or nil!
;;

;;; References:
;;
;; Code partially stems from the sources:
;; * mml2015.el (Gnus)
;; * mc-toplev.el (Mailcrypt) 
;;
;; For PGP/MIME see:
;; * http://www.faqs.org/rfcs/rfc2015.html
;; * http://www.faqs.org/rfcs/rfc3156.html
;;

;;; TODO:
;;
;; * do nicer cleartext sig-check and auto decrypt 
;; * attaching of other keys from key-ring
;;

;;; Code:

(require 'pgg)
(require 'easymenu)

(eval-when-compile
  (require 'cl))

(defgroup vm nil
  "VM"
  :group 'mail)

(defgroup vm-pgg nil
  "PGP and PGP/MIME support for VM by PGG."
  :group  'vm)

(defface vm-pgg-bad-signature
  '((((type tty) (class color))
     (:foreground "red" :bold t))
    (((type tty))
     (:bold t))
    (((background light))
     (:foreground "red" :bold t))
    (((background dark))
     (:foreground "red" :bold t:)))
  "The face used to highlight bad signature messages."
  :group 'vm-pgg)

(defface vm-pgg-good-signature
  '((((type tty) (class color))
     (:foreground "green" :bold t))
    (((type tty))
     (:bold t))
    (((background light))
     (:foreground "green"))
    (((background dark))
     (:foreground "green")))
  "The face used to highlight good signature messages."
  :group 'vm-pgg)

(defface vm-pgg-error
  '((((type tty) (class color))
     (:foreground "red" :bold t))
    (((type tty))
     (:bold t))
    (((background light))
     (:foreground "red" :bold t))
    (((background dark))
     (:foreground "red" :bold t:)))
  "The face used to highlight error messages."
  :group 'vm-pgg)

(defcustom vm-pgg-always-replace nil
  "*If t, decrypt mail messages in place without prompting.

If 'never, always use a viewer instead of replacing."
  :group 'vm-pgg
  :type '(choice (const never)
                 (const :tag "always" t)
                 (const :tag "ask" nil)))

(defvar vm-pgg-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c#e" 'vm-pgg-encrypt-and-sign)
    (define-key map "\C-c#E" 'vm-pgg-encrypt)
    (define-key map "\C-c#s" 'vm-pgg-sign)
    (define-key map "\C-c#k" 'vm-pgg-attach-public-key)
    map))

(defvar vm-pgg-compose-mode-menu nil
  "The composition menu of vm-pgg.")

(easy-menu-define
 vm-pgg-compose-mode-menu (if (featurep 'xemacs) nil (list vm-pgg-compose-mode-map))
 "PGP/MIME compose mode menu."
 '("PGP/MIME"
   ["Encrypt"           vm-pgg-encrypt t]
   ["Sign"              vm-pgg-sign t]
   ["Encrypt+Sign"      vm-pgg-encrypt-and-sign t]
   ["Attach Public Key" vm-pgg-attach-public-key t]))

(defvar vm-pgg-compose-mode nil
  "None-nil means PGP/MIME composition mode key bindings and menu are available.")

(make-variable-buffer-local 'vm-pgg-compose-mode)

(defun vm-pgg-compose-mode (&optional arg)
  "\nMinor mode for interfacing with cryptographic functions.
\\<vm-pgg-compose-mode-map>
\\[vm-pgg-encrypt]\t\tEncrypt (and optionally sign) message
\\[vm-pgg-sign]\t\tClearsign message
\\[vm-pgg-attach-public-key]\t\Attach public key from keyring"
  (interactive)
  (setq vm-pgg-compose-mode
	(if (null arg) (not vm-pgg-compose-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if vm-pgg-compose-mode
      (easy-menu-add vm-pgg-compose-mode-menu)
    (easy-menu-remove vm-pgg-compose-mode-menu)))

(defvar vm-pgg-compose-mode-string " vm-pgg"
  "*String to put in mode line when `vm-pgg-compose-mode' is active.")


(if (not (assq 'vm-pgg-compose-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons (cons 'vm-pgg-compose-mode vm-pgg-compose-mode-map)
		minor-mode-map-alist)))

(if (not (assq 'vm-pgg-compose-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(vm-pgg-compose-mode vm-pgg-compose-mode-string) minor-mode-alist)))

(defun vm-pgg-compose-mode-activate ()
  "Activate `vm-pgg-compose-mode'."
  (vm-pgg-compose-mode 1))

(add-hook 'vm-mail-mode-hook 'vm-pgg-compose-mode-activate t)

(defun vm-pgg-get-emails (headers)
  "Return email addresses found in the given HEADERS."
  (let (content recipients)
    (while headers
      (setq content (vm-mail-mode-get-header-contents (car headers)))
      (when content
        (setq recipients (append (rfc822-addresses content) recipients)))
      (setq headers (cdr headers)))
    recipients))

(defvar vm-pgg-get-recipients-headers '("To:" "CC:" "BCC:")
  "The list of headers to get recipients from.")
  
(defun vm-pgg-get-recipients ()
  "Return a list of recipients."
  (vm-pgg-get-emails vm-pgg-get-recipients-headers))

(defvar vm-pgg-get-author-headers '("From:" "Sender:")
  "The list of headers to get the author from.")

(defun vm-pgg-get-author ()
  "Return the author of the message."
  (car (vm-pgg-get-emails vm-pgg-get-author-headers)))

(defun vm-pgp-prepare-composition ()
  "Prepare the composition for encrypting or signing."
  ;; encode message
  (unless (vm-mail-mode-get-header-contents "MIME-Version:")
    (vm-mime-encode-composition))
  ;; ensure newline at end
  (goto-char (point-max))
  (skip-chars-backward " \t\r\n\f")
  (delete-region (point) (point-max))
  (insert "\n")
  ;; skip headers
  (goto-char (point-min))
  (search-forward (concat "\n" mail-header-separator "\n"))
  (goto-char (match-end 0))
  ;; guess the author 
  (make-local-variable 'pgg-default-user-id)
  (setq pgg-default-user-id (or (vm-pgg-get-author) pgg-default-user-id)))

;;; ###autoload
(defun vm-pgg-cleartext-encrypt (sign)
  "*Encrypt the composition as cleartext and with a prefix also SIGN it."
  (interactive "P")
  (save-excursion 
    (vm-pgp-prepare-composition)
    (let ((start (point)) (end   (point-max)))
      (unless (pgg-encrypt-region start end (vm-pgg-get-recipients) sign)
        (pop-to-buffer pgg-errors-buffer)
        (error "Encrypt error"))
      (delete-region start end)
      (insert-buffer-substring pgg-output-buffer))))

(defun vm-pgg-cleartext-automode ()
  (goto-char (point-min))
  (search-forward "\n\n")
  (if (looking-at "^-----BEGIN PGP \\(ENCRYPTED\\|SIGNED\\) MESSAGE----")
      (condition-case nil
          (cond ((string= (match-string 1) "ENCRYPTED")
                 (vm-pgg-cleartext-decrypt))
                ((string= (match-string 1) "SIGNED")
                 (vm-pgg-cleartext-verify)))
        (error nil))
    (let ((window (get-buffer-window pgg-output-buffer)))
      (when window
        (delete-window window)))))

(defadvice vm-preview-current-message (after vm-pgg-cleartext-automode activate)
  (if (not (eq vm-system-state 'previewing))
      (vm-pgg-cleartext-automode)))

(defadvice vm-scroll-forward (around vm-pgg-cleartext-automode activate)
  (let ((vm-system-state-was vm-system-state))
    ad-do-it
    (if (eq vm-system-state-was 'previewing)
        (vm-pgg-cleartext-automode))))

;;; ###autoload
(defun vm-pgg-cleartext-sign ()
  "*Sign the message."
  (interactive)
  (save-excursion 
    (vm-pgp-prepare-composition)
    (let ((start (point)) (end (point-max)))
      (unless (pgg-sign-region start end t)
        (pop-to-buffer pgg-errors-buffer)
        (error "Signing error"))
      (delete-region start end)
      (insert-buffer-substring pgg-output-buffer))))

;;; ###autoload
(defun vm-pgg-cleartext-verify ()
  "*Verify the signature in the current message."
  (interactive)
  (let ((current-window (selected-window)))
    (if (interactive-p)
        (vm-follow-summary-cursor))
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    ;; ensure we are in the right buffer
    (if vm-presentation-buffer
        (set-buffer vm-presentation-buffer))
    ;; skip headers 
    (goto-char (point-min))
    (search-forward "\n\n")
    (goto-char (match-end 0))
    ;; verify 
    (unless (pgg-verify-region (point) (point-max))
      (save-excursion
        (set-buffer pgg-errors-buffer)
        (if (re-search-forward "NO_PUBKEY[^\n\r]+" (point-max) t)
            (message (match-string 0))
          (pop-to-buffer pgg-errors-buffer)
          (error "Verification failed"))))
    (save-excursion
      (set-buffer pgg-output-buffer)
      (skip-chars-backward " \t\t\n\f")
      (let ((start) (end (point)))
        (beginning-of-line)
        (setq start (point))
        (message (buffer-substring start end))
        (sit-for 2)))))

;;; ###autoload
(defun vm-pgg-cleartext-decrypt ()
  "*Decrypt the contents of the current message."
  (interactive)
  (let ((vm-frame-per-edit nil)
	from-line)
    (if (interactive-p)
	(vm-follow-summary-cursor))
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-read-only)
    (vm-error-if-folder-empty)

    ;; store away a valid "From " line for possible later use.
    (save-excursion
      (vm-select-folder-buffer)
      (set-buffer (vm-buffer-of (vm-real-message-of (car vm-message-pointer))))
      (setq from-line (vm-leading-message-separator)))
    
    (vm-edit-message)

    ;; skip headers 
    (goto-char (point-min))
    (search-forward "\n\n")
    (goto-char (match-end 0))
    
    ;; decrypt 
    (unless (pgg-decrypt-region (point) (point-max))
      (vm-edit-message-abort)
      (pop-to-buffer pgg-errors-buffer)
      (error "Decryption failed"))

    ;; replace message body 
    (delete-region (point) (point-max))
    (insert-buffer-substring pgg-output-buffer)

    ;; remove carrige returns
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n" t t))

    ;; display/replace the message
    (if (and (not (eq vm-pgg-always-replace 'never))
             (or vm-pgg-always-replace
                 (y-or-n-p "Replace encrypted message with decrypted? ")))
        (let ((this-command 'vm-edit-message-end))
          (vm-edit-message-end))
      (let ((tmp (generate-new-buffer "*Viewing decrypted message*")))
        (copy-to-buffer tmp (point-min) (point-max))
        (vm-edit-message-abort)
        (switch-to-buffer tmp t)
        (goto-char (point-min))
        (insert from-line)	     
        (set-buffer-modified-p nil)
        (vm-mode t)))))

(defun vm-pgg-crlf-cleanup (start end)
  "Convert CRLF to LF in region from START to END."
  (save-excursion
    (goto-char start)
    (while (search-forward "\r\n" end t)
      (replace-match "\n" t t))))

(defun vm-pgg-make-crlf (start end)
  "Convert CRLF to LF in region from START to END."
  (save-excursion
    (goto-char start)
    (while (search-forward "\n" end t)
      (replace-match "\r\n" t t))))

;;; ###autoload
(defun vm-mime-display-internal-multipart/encrypted (layout)
  "Display multipart/encrypted LAYOUT."
  (let* ((part-list (vm-mm-layout-parts layout))
         (header (car part-list))
         (message (car (cdr part-list)))
         status)
    (if (not (and (= (length part-list) 2)
                  (vm-mime-types-match (car (vm-mm-layout-type header))
                                       "application/pgp-encrypted")
                  ;; TODO: check version and protocol here?
                  (vm-mime-types-match (car (vm-mm-layout-type message))
                                       "application/octet-stream")))
        (insert "Unknown multipart/encrypted format.")
      ;; decode the message now
      (save-excursion
        (set-buffer (vm-buffer-of (vm-mm-layout-message message)))
        (save-restriction
          (widen)
          (setq status (pgg-decrypt-region (vm-mm-layout-body-start message)
                                           (vm-mm-layout-body-end message)))))
      (if (not status)
          (let ((start (point)))
            (insert-buffer-substring pgg-errors-buffer)
            (put-text-property start (point) 'face 'vm-pgg-error))
        (save-excursion
          (set-buffer pgg-output-buffer)
          (vm-pgg-crlf-cleanup (point-min) (point-max))
          (setq message (vm-mime-parse-entity-safe nil nil nil t)))
        (if message
          (vm-decode-mime-layout message)
          (insert-buffer-substring pgg-output-buffer))
        (setq status (save-excursion
                       (set-buffer pgg-errors-buffer)
                       (goto-char (point-min))
                       (if (re-search-forward "GOODSIG [^\n\r]+" (point-max) t)
                           (match-string 0))))
        (if status
            (insert "\n" status "\n"))
        t))))

;;; ###autoload
(defun vm-mime-display-internal-multipart/signed (layout)
  "Display multipart/signed LAYOUT."
  (let* ((part-list (vm-mm-layout-parts layout))
         (message (car part-list))
         (signature (car (cdr part-list)))
         status signature-file)
    (if (not (and (= (length part-list) 2)
                  ;; TODO: check version and protocol here?
                  (vm-mime-types-match (car (vm-mm-layout-type signature))
                                       "application/pgp-signature")))
        (insert "Unknown multipart/signed format.")
      ;; insert the message 
      (vm-decode-mime-layout message)
      ;; verify the message now
      (save-excursion
        (set-buffer (vm-buffer-of (vm-mm-layout-message message)))
        (save-restriction
          (widen)
          ;; write signature to a temp file
          (write-region (vm-mm-layout-body-start signature)
                        (vm-mm-layout-body-end signature)
                        (setq signature-file (make-temp-file "vm-pgg-signature")))
          (setq message (buffer-substring  (vm-mm-layout-header-start message)
                                           (vm-mm-layout-body-end message)))))
      (let ((start (point)) end)
        (insert message)
        ;; according to the RFC 3156 we need to skip trailing white space, but
        ;; IMHO a least a single ^M must remain ... odd!
        (if (< (skip-chars-backward " \t\r\n\f" start) 0)
            (forward-char 1))
        (setq end (point-marker))
        (vm-pgg-make-crlf start end)
        (setq status (pgg-verify-region start end signature-file))
        (delete-file signature-file)
        (delete-region start end))
      ;; now insert the content
      (insert "\n")
      (let ((start (point)) end)
        (if (not status)
            (insert-buffer-substring pgg-errors-buffer)
          (insert-buffer-substring pgg-output-buffer)
          (vm-pgg-crlf-cleanup start (point)))
        (setq end (point))
        (put-text-property start end 'face (if status 'vm-pgg-good-signature 'vm-pgg-bad-signature)))
      t)))

;; we must add these in order to force VM to call our handler
;(add-to-list 'vm-auto-displayed-mime-content-types "application/pgp-keys")
;(add-to-list 'vm-mime-internal-content-types "application/pgp-keys")

;;; ###autoload
(defun vm-mime-display-internal-application/pgp-keys (layout)
  "Snarf keys in LAYOUT and display result of snarfing."
  ;; insert the keys
  (let ((start (point)) end status)
    (vm-mime-insert-mime-body layout)
    (setq end (point-marker))
    (vm-mime-transfer-decode-region layout start end)
    (save-excursion
      (setq status (pgg-snarf-keys-region start end)))
    (delete-region start end)
    ;; now insert the result of snafing 
    (if status
        (insert-buffer-substring pgg-output-buffer)
      (insert-buffer-substring pgg-errors-buffer))
    t))

;;; ###autoload
(defun vm-pgg-snarf-keys ()
  "*Snarf keys from the current message."
  (interactive)
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (save-restriction
    ;; ensure we are in the right buffer
    (if vm-presentation-buffer
        (set-buffer vm-presentation-buffer))
    ;; skip headers 
    (goto-char (point-min))
    (search-forward "\n\n")
    (goto-char (match-end 0))
    ;; verify 
    (unless (pgg-snarf-keys)
      (error "Snarfing failed"))
    (save-excursion
      (set-buffer pgg-output-buffer)
      (message (buffer-substring)))))

;;; ###autoload
(defun vm-pgg-attach-public-key ()
  "Attach your public key to a composition."
  (interactive)
  (let* ((pgg-default-user-id (or (vm-pgg-get-author) pgg-default-user-id))
         (description (concat "public key of " pgg-default-user-id))
         (buffer (get-buffer-create (concat " *" description "*"))))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (let ((start (point)))
        (pgg-insert-key)
        (if (= start (point))
            (error "%s has no public key!" pgg-default-user-id))))
    (save-excursion
      (goto-char (point-max))
      (insert "\n")
      (vm-mime-attach-buffer buffer "application/pgp-keys" nil description))))

;;; ###autoload
(defun vm-pgg-sign ()
  "Sign the composition with PGP/MIME."
  (interactive)
  (vm-mime-encode-composition)
  (let ((content-type (vm-mail-mode-get-header-contents "Content-Type:"))
        (encoding (vm-mail-mode-get-header-contents "Content-Transfer-Encoding:"))
        (boundary (vm-mime-make-multipart-boundary))
        micalg
        body-start)
    ;; fix the body
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n"))
    (goto-char (match-end 0))
    (setq body-start (point-marker))
    (insert "Content-Type:" (or content-type "text/plain") "\n")
    (insert "Content-Transfer-Encoding:" (or encoding "7bit") "\n")
    (insert "\n")
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (delete-region (point) (point-max))
    (vm-pgg-cleartext-sign)
    (vm-pgg-crlf-cleanup  body-start (point-max))
    (goto-char body-start)
    (forward-line 1)
    (delete-region body-start (point))
    (if (not (looking-at "^Hash: \\([^ \t\n\r]+\\)"))
        (error "Could not determine micalg."))
    (setq micalg (downcase (match-string 1)))
    (forward-line 2)
    (delete-region body-start (point))
    (insert "--" boundary "\n")
    (search-forward "-----BEGIN PGP SIGNATURE-----")
    (goto-char (match-beginning 0))
    (insert "--" boundary "\n")
    (insert "Content-Type: application/pgp-signature\n\n")
    (goto-char (point-max))
    (insert "--" boundary "--\n")
    ;; fix the headers 
    (vm-mail-mode-remove-header "MIME-Version:")
    (vm-mail-mode-remove-header "Content-Type:")
    (vm-mail-mode-remove-header "Content-Transfer-Encoding:")
    (mail-position-on-field "MIME-Version")
    (insert "1.0")
    (mail-position-on-field "Content-Type")
    (insert "multipart/signed; boundary=\"" boundary "\";\n"
            "\tmicalg=pgp-" micalg "; protocol=\"application/pgp-signature\"")))
    
;;; ###autoload
(defun vm-pgg-encrypt (sign)
  "Encrypt the composition as PGP/MIME. With a prefix arg SIGN also sign it."
  (interactive "P")
  (vm-mime-encode-composition)
  (let ((content-type (vm-mail-mode-get-header-contents "Content-Type:"))
        (encoding (vm-mail-mode-get-header-contents "Content-Transfer-Encoding:"))
        (boundary (vm-mime-make-multipart-boundary))
        body-start)
    ;; fix the body
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n"))
    (goto-char (match-end 0))
    (setq body-start (point-marker))
    (insert "Content-Type: " (or content-type "text/plain") "\n")
    (insert "Content-Transfer-Encoding: " (or encoding "7bit") "\n")
    (insert "\n")
    (goto-char (point-max))
    (insert "\n")
    (vm-pgg-cleartext-encrypt sign)
    (goto-char body-start)
    (insert "--" boundary "\n")
    (insert "Content-Type: application/pgp-encrypted\n\n")
    (insert "Version: 1\n\n")
    (insert "--" boundary "\n")
    (insert "Content-Type: application/octet-stream\n\n")
    (goto-char (point-max))
    (insert "--" boundary "--\n")
    ;; fix the headers 
    (vm-mail-mode-remove-header "MIME-Version:")
    (vm-mail-mode-remove-header "Content-Type:")
    (vm-mail-mode-remove-header "Content-Transfer-Encoding:")
    (mail-position-on-field "MIME-Version")
    (insert "1.0")
    (mail-position-on-field "Content-Type")
    (insert "multipart/encrypted; boundary=\"" boundary "\";\n"
            "\tprotocol=\"application/pgp-encrypted\"")))

(defun vm-pgg-encrypt-and-sign ()
  "*Encrypt and sign the composition as PGP/MIME."
  (interactive)
  (vm-pgg-encrypt t))

(provide 'vm-pgg)

;;; vm-pgg.el ends here
