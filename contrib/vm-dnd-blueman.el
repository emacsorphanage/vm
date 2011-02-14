;From: blueman <NOSPAM@nospam.com>
;Subject: Function for drag-and-dropping vm-mime attachments...
;Newsgroups: gnu.emacs.vm.info
;Date: Jan 1 2010, 9:30am

; Here is a cute hack to allow for dragging-and-dropping files as 
; mime attachments into vm-mail buffers. This is done by advising 
; dnd-handle-one-url to temporarily change the dnd-protocol-alist handlers 
(defun dnd-insert-vm-mime (uri action) 
  "Insert local file as vm mime attachment in current vm composition  buffer. 
URI is the url for the file, and must have the format: 
file:file-name or file:///file-name. 
The last / in file:/// is part of the file name. 
If the system natively supports unc file names, then remote urls of the form 
file://server-name/file-name will also be handled by this function. 
ACTION is ignored. (JJK)" 
  (let* ((f (dnd-get-local-file-name uri t))) 
        (vm-mime-attach-file f 
                 (or (vm-mime-default-type-from-filename f) 
                         "application/octet-stream")))) 
(defadvice dnd-handle-one-url 
  (around vm-mime-drag-n-drop nil activate) 
  "Adds vm-mime attachment rather than opening file if dragged to a vm 
composition buffer (specificially, where 'current-local-map' is 
equal to 'vm-mail-mode map') (JJK - advice)" 
  (if (member 'vm-forget-composition-buffer kill-buffer-hook) 
;  (if (eq (current-local-map) vm-mail-mode-map) ; For vm-version < 8.1 
          (let* ((dnd-protocol-alist (copy-alist dnd-protocol-alist))) 
                (setcdr (assoc "^file:///" dnd-protocol-alist) 
			'dnd-insert-vm-mime) ;X 
                (setcdr (assoc "^file:" dnd-protocol-alist) 
			'dnd-insert-vm-mime); W32 
                ad-do-it)  ; call dnd-handle-one-url within wrapper 
        ad-do-it  ; call dnd-handle-one-url normally 
        )) 
