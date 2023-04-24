(require 'cl-lib)

;; Loogoot split algorithm
;; André, Luc, et al. "Supporting adaptable granularity of changes for massive-scale collaborative editing." 9th IEEE International Conference on Collaborative Computing: Networking, Applications and Worksharing. IEEE, 2013.
(defvar crdt--local-clock 0)
(defvar crdt--local-id)
(defvar crdt--inhibit-update nil "When set, don't call CRDT--LOCAL-* on change.
This is useful for functions that apply remote change to local buffer,
to avoid recusive calling of CRDT synchronization functions.")

;; CRDT IDs are represented by unitbyte strings (for efficient comparison)
;; Every two bytes represent a big endian encoded integer
;; For base IDs, last two bytes are always representing site ID
(defconst crdt--max-value (lsh 1 16))
;; (defconst crdt--max-value 4)
;; for debug
(defconst crdt--low-byte-mask 255)
(defsubst crdt--get-two-bytes (string index)
  (logior (lsh (elt string index) 8)
          (elt string (1+ index))))
(defsubst crdt--get-two-bytes-with-offset (string offset index default)
  (cond ((= index (- (string-bytes string) 2))
         offset)
        ((< (1+ index) (string-bytes string))
         (logior (lsh (elt string index) 8)
                 (elt string (1+ index))))
        (t default)))

(defsubst crdt--id-offset (id)
  "Get the literal offset integer from ID.
Note that it might deviate from real offset for a character
in the middle of a block."
  (crdt--get-two-bytes id (- (string-bytes id) 2)))
(defsubst crdt--set-id-offset (id offset)
  (let ((length (string-bytes id)))
    (aset id (- length 2) (lsh offset -8))
    (aset id (- length 1) (logand offset crdt--low-byte-mask))))
(defsubst crdt--id-site (id)
  (crdt--get-two-bytes id (- (string-bytes id) 4)))
(defsubst crdt--generate-id (low-id low-offset high-id high-offset site-id)
  (let* ((l (crdt--get-two-bytes-with-offset low-id low-offset 0 0))
         (h (crdt--get-two-bytes-with-offset high-id high-offset 0 crdt--max-value))
         (bytes (cl-loop for pos from 2 by 2
                         while (< (- h l) 2)
                         append (list (lsh l -8)
                                      (logand l crdt--low-byte-mask))
                         do (setq l (crdt--get-two-bytes-with-offset low-id low-offset pos 0))
                         do (setq h (crdt--get-two-bytes-with-offset high-id high-offset pos crdt--max-value))))
         (m (+ l 1 (random (- h l 1)))))
    (apply #'unibyte-string
           (append bytes (list (lsh m -8)
                               (logand m crdt--low-byte-mask)
                               (lsh site-id -8)
                               (logand site-id crdt--low-byte-mask)
                               0
                               0)))))

;; CRDT-ID text property actually stores a cons of (ID-STRING . END-OF-BLOCK-P)
(defsubst crdt--get-crdt-id-pair (pos &optional obj)
  (get-text-property pos 'crdt-id obj))
(defsubst crdt--get-starting-id (pos &optional obj)
  (car (crdt--get-crdt-id-pair pos obj)))
(defsubst crdt--end-of-block-p (pos &optional obj)
  (cdr (crdt--get-crdt-id-pair pos obj)))
(defsubst crdt--get-starting-id-maybe (pos &optional obj limit)
  (unless (< pos (or limit (point-min)))
    (car (get-text-property pos 'crdt-id obj))))
(defsubst crdt--get-id-offset (starting-id pos &optional obj limit)
  "Get the real offset integer for a character at POS,
assuming the stored literal ID is STARTING-ID."
  (let* ((start-pos (previous-single-property-change (1+ pos) 'crdt-id obj (or limit (point-min)))))
    (+ (- pos start-pos) (crdt--id-offset starting-id))))

(defsubst crdt--set-id (pos id &optional end-of-block obj limit)
  (put-text-property pos (next-single-property-change pos 'crdt-id obj (or limit (point-max))) 'crdt-id (cons id end-of-block) obj))

(defsubst crdt--id-replace-offset (id offset)
  (let ((new-id (substring id)))
    (crdt--set-id-offset new-id offset)
    new-id))
(cl-defmacro crdt--with-insertion-information
    ((beg end &optional beg-obj end-obj beg-limit end-limit) &body body)
  `(let* ((not-begin (> ,beg ,(or beg-limit '(point-min)))) ; if it's nil, we're at the beginning of buffer
          (left-pos (1- ,beg))
          (starting-id-pair (when not-begin (crdt--get-crdt-id-pair left-pos ,beg-obj)))
          (starting-id (if not-begin (car starting-id-pair) ""))
          (left-offset (if not-begin (crdt--get-id-offset starting-id left-pos ,beg-obj ,beg-limit) 0))
          (not-end (< ,end ,(or end-limit '(point-max))))
          (ending-id (if not-end (crdt--get-starting-id ,end ,end-obj) ""))
          (right-offset (if not-end (crdt--id-offset ending-id) 0))
          (beg ,beg)
          (end ,end)
          (beg-obj ,beg-obj)
          (end-obj ,end-obj)
          (beg-limit ,beg-limit)
          (end-limit ,end-limit))
     ,@body))
(defmacro crdt--split-maybe ()
  '(when (and not-end (eq starting-id (crdt--get-starting-id end end-obj)))
     ;; need to split id block
     (crdt--set-id end (crdt--id-replace-offset starting-id (1+ left-offset))
                   (crdt--end-of-block-p left-pos beg-obj) end-obj end-limit)
     (rplacd (get-text-property left-pos 'crdt-id beg-obj) nil) ;; clear end-of-block flag
     t))

;; The protocol
;; Text-based version
;; (it should be easy to migrate to a binary version. Using text for better debugging for now)
;; Every message takes the form (type . body)
;; type can be: insert hello sync
;; - insert
;;   body takes the form (crdt-id position-hint content)
;;   - position-hint is the buffer position where the operation happens at the site
;;     which generates the operation. Then we can play the trick that start search
;;     near this position at other sites to speedup crdt-id search
;;   - content is the string to be inserted
;; - delete
;;   body takes the form (position-hint (crdt-id . length)*)
;; - cursor
;;   body takes the form (site-id point-position-hint point-crdt-id mark-position-hint mark-crdt-id)
;;   *-crdt-id can be either a CRDT ID string, or
;;     - nil, which means clear the cursor/mark
;;     - t, which means end of buffer
;; - hello
;;   This message is sent from client to server, when a client connect to the server.
;;   body is currently nil
;; - sync
;;   This message is sent from server to client to get it sync to the state on the server.
;;   It's always sent after server receives a hello message.
;;   Might be used for error recovery or other optimization in the future.
;;   One optimization I have in mind is let server try to merge all CRDT item into a single
;;   one and try to synchronize this state to clients at best effort.
;;   body takes the form (site-id content . crdt-id-list)
;;   - site-id is the site ID the server assigned to the client
;;   - content is the string in the buffer
;;   - crdt-id-list is generated from CRDT--DUMP-IDS

(defsubst crdt--same-base-p (a b)
  (let* ((a-length (string-bytes a))
         (b-length (string-bytes b)))
    (and (eq a-length b-length)
         (let ((base-length (- a-length 2)))
           (eq t (compare-strings a 0 base-length b 0 base-length))))))
(defun crdt--local-insert (beg end)
  "To be called after a local insert happened in current buffer, from BEG to END.
Returns a list of (insert type) messages to be sent."
  (let (resulting-commands)
    (crdt--with-insertion-information
     (beg end)
     (unless (crdt--split-maybe)
       (when (and not-begin
                  (eq (crdt--id-site starting-id) crdt--local-id)
                  (crdt--end-of-block-p left-pos))
         ;; merge crdt id block
         (let* ((max-offset crdt--max-value)
                (merge-end (min end (+ (- max-offset left-offset 1) beg))))
           (unless (= merge-end beg)
             (put-text-property beg merge-end 'crdt-id starting-id-pair)
             (let ((virtual-id (substring starting-id)))
               (crdt--set-id-offset virtual-id (1+ left-offset))
               (push `(insert ,virtual-id ,beg
                              ,(buffer-substring-no-properties beg merge-end))
                     resulting-commands))
             (setq beg merge-end)))))
     (while (< beg end)
       (let ((block-end (min end (+ crdt--max-value beg))))
         (let ((new-id (crdt--generate-id starting-id left-offset ending-id right-offset crdt--local-id)))
           (put-text-property beg block-end 'crdt-id (cons new-id t))
           (push `(insert ,new-id ,beg
                          ,(buffer-substring-no-properties beg block-end))
                 resulting-commands)
           (setq beg block-end)
           (setq left-offset (1- crdt--max-value)) ; this is always true when we need to continue
           (setq starting-id new-id)))))
    (crdt--verify-buffer)
    (nreverse resulting-commands)))

(defun crdt--find-id (id pos)
  (let* ((left-pos (previous-single-property-change (if (< pos (point-max)) (1+ pos) pos)
                                                    'crdt-id nil (point-min)))
         (left-id (crdt--get-starting-id left-pos))
         (right-pos (next-single-property-change pos 'crdt-id nil (point-max)))
         (right-id (crdt--get-starting-id right-pos)))
    (print (list left-pos left-id right-pos right-id))
    (cl-block nil
      (while t
        (cond ((<= right-pos (point-min))
               (cl-return (point-min)))
              ((>= left-pos (point-max))
               (cl-return (point-max)))
              ((and right-id (not (string< id right-id)))
               (setq left-pos right-pos)
               (setq left-id right-id)
               (setq right-pos (next-single-property-change right-pos 'crdt-id nil (point-max)))
               (setq right-id (crdt--get-starting-id right-pos)))
              ((string< id left-id)
               (setq right-pos left-pos)
               (setq right-id left-id)
               (setq left-pos (previous-single-property-change left-pos 'crdt-id nil (point-min)))
               (setq left-id (crdt--get-starting-id left-pos)))
              (t
               ;; will unibyte to multibyte conversion cause any problem?
               (cl-return
                (if (eq t (compare-strings left-id 0 (- (string-bytes left-id) 2)
                                           id 0 (- (string-bytes left-id) 2)))
                    (min right-pos (+ left-pos 1
                                      (- (crdt--get-two-bytes id (- (string-bytes left-id) 2))
                                         (crdt--id-offset left-id))))
                  right-pos))))))))
(defun crdt--remote-insert (message)
  (let ((crdt--inhibit-update t))
    (cl-destructuring-bind (id position-hint content) message
      (let ((beg (crdt--find-id id position-hint)) end)
        (when beg
          (goto-char beg)
          (insert content)
          (setq end (point))
          (crdt--with-insertion-information
           (beg end)
           (let ((base-length (- (string-bytes starting-id) 2)))
             (if (and (eq (string-bytes id) (string-bytes starting-id))
                      (eq t (compare-strings starting-id 0 base-length
                                             id 0 base-length))
                      (eq (1+ left-offset) (crdt--id-offset id)))
                 (put-text-property beg end 'crdt-id starting-id-pair)
               (put-text-property beg end 'crdt-id (cons id t))))
           (crdt--split-maybe))))))
  (crdt--verify-buffer))

(defun crdt--local-delete (beg end)
  (let ((outer-end end))
    (crdt--with-insertion-information
     (beg 0 nil crdt--changed-string nil (length crdt--changed-string))
     (if (crdt--split-maybe)
         (let* ((not-end (< outer-end (point-max)))
                (ending-id (when not-end (crdt--get-starting-id outer-end))))
           (when (and not-end (eq starting-id (crdt--get-starting-id outer-end)))
             (crdt--set-id outer-end (crdt--id-replace-offset starting-id (+ 1 left-offset (length crdt--changed-string))))
             t))
       (crdt--with-insertion-information
        ((length crdt--changed-string) outer-end crdt--changed-string nil 0 nil)
        (crdt--split-maybe)))))
  (crdt--verify-buffer)
  `(delete ,beg ,@ (crdt--dump-ids 0 (length crdt--changed-string) crdt--changed-string t)))
(defun crdt--remote-delete (message)
  (cl-destructuring-bind (position-hint . id-pairs) message
    (dolist (id-pair id-pairs)
      (cl-destructuring-bind (length . id) id-pair
        (while (> length 0)
          (goto-char (1- (crdt--find-id id position-hint)))
          (let* ((end-of-block (next-single-property-change (point) 'crdt-id nil (point-max)))
                 (block-length (- end-of-block (point))))
            (cl-case (cl-signum (- length block-length))
              ((1) (delete-char block-length)
               (cl-decf length block-length)
               (crdt--set-id-offset id (+ (crdt--id-offset id) block-length)))
              ((0) (delete-char length)
               (setq length 0))
              ((-1)
               (let* ((starting-id (crdt--get-starting-id (point)))
                      (left-offset (crdt--get-id-offset starting-id (point))))
                 (delete-char length)
                 (crdt--set-id (point) (crdt--id-replace-offset starting-id (+ left-offset length))))
               (setq length 0)))))
        (crdt--verify-buffer)))))

(defvar crdt--changed-string nil)
(defun crdt--before-change (beg end)
  (unless crdt--inhibit-update
    (setq crdt--changed-string (buffer-substring beg end))))

(defun crdt--after-change (beg end length)
  (unless crdt--inhibit-update
    (let ((crdt--inhibit-update t))
      ;; we're only interested in text change
      ;; ignore property only changes
      (save-excursion
        (goto-char beg)
        (unless (and (= length (- end beg)) (looking-at (regexp-quote crdt--changed-string)))
          (widen)
          (unless (= length 0)
            (crdt--broadcast-maybe
             (format "%S" (let ((m (crdt--local-delete beg end)))
                            (print m) m))))
          (unless (= beg end)
            (dolist (message (crdt--local-insert beg end))
              (crdt--broadcast-maybe
               (format "%S" (progn (print message) message))))))))))

(defun crdt--dump-ids (beg end object &optional omit-end-of-block-p)
  "Serialize all CRDT ids in OBJECT from BEG to END into a list of
CONSes of the form (LENGTH CRDT-ID . END-OF-BLOCK-P),
or (LENGTH . CRDT-ID) if OMIT-END-OF-BLOCK-P is non-NIL.
in the order that they appears in the document"
  (let (ids (pos end))
    (while (> pos beg)
      (let ((prev-pos (previous-single-property-change pos 'crdt-id object beg)))
        (push (cons (- pos prev-pos)
                    (if omit-end-of-block-p
                        (crdt--get-starting-id prev-pos object)
                      (crdt--get-crdt-id-pair prev-pos object)))
              ids)
        (setq pos prev-pos)))
    ids))
(defun crdt--load-ids (ids)
  "Load the CRDT ids in IDS (generated by CRDT--DUMP-IDS)
into current buffer."
  (let ((pos (point-min)))
    (dolist (id-pair ids)
      (let ((next-pos (+ pos (car id-pair))))
        (put-text-property pos next-pos 'crdt-id (cdr id-pair))
        (setq pos next-pos)))))
(defun crdt--verify-buffer ()
  "Debug helper function to verify that CRDT IDs in a document follows
ascending order."
  (let* ((pos (point-min))
         (id (crdt--get-starting-id pos)))
    (cl-block
        (while t
          (let* ((next-pos (next-single-property-change pos 'crdt-id))
                 (next-id (if (< next-pos (point-max))
                              (crdt--get-starting-id next-pos)
                            (cl-return)))
                 (prev-id (substring id)))
            (crdt--set-id-offset id (+ (- next-pos pos) (crdt--id-offset id)))
            (unless (string< prev-id next-id)
              (error "Not monotonic!"))
            (setq pos next-pos)
            (setq id next-id))))))

(defvar crdt--network-process)
(defvar crdt--network-clients)
(defvar crdt--next-client-id)
(cl-defun crdt--broadcast-maybe (message-string &optional (without t))
  "Broadcast or send MESSAGE-STRING depends on whether CRDT--NETWORK-PROCESS
is a server process.
If CRDT--NETWORK-PROCESS is a server process, broadcast MESSAGE-STRING
to clients except the one of which CLIENT-ID property is EQ to WITHOUT.
If CRDT--NETWORK-PROCESS is a server process, send MESSAGE-STRING
to server unless WITHOUT is NIL."
  (if (process-contact crdt--network-process :server)
      (dolist (client crdt--network-clients)
        (when (and (eq (process-status client) 'open)
                   (not (eq (process-get client 'client-id) without)))
          (process-send-string client message-string)))
    (when without
      (process-send-string crdt--network-process message-string))))
(defun crdt--network-filter (process string)
  (unless (process-buffer process)
    (set-process-buffer process (generate-new-buffer "*crdt-server*"))
    (set-marker (process-mark process) 1))
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when enable-multibyte-characters
        (set-buffer-multibyte nil))
      (save-excursion
        (goto-char (process-mark process))
        (insert string)
        (set-marker (process-mark process) (point))
        (goto-char (point-min))
        (let (message)
          (while (setq message (ignore-errors (read (current-buffer))))
            (print (list 'received message))
            (with-current-buffer (process-get process 'crdt-buffer)
              (save-excursion
                (widen)
                (cl-destructuring-bind (type . body) message
                  (let ((crdt--inhibit-update t))
                    (cl-case type
                      ((insert)
                       (unless (eq (crdt--id-site (car body)) crdt--local-id)
                         (crdt--remote-insert body)
                         (crdt--broadcast-maybe (format "%S" message) (process-get process 'client-id))))
                      ((delete) (crdt--remote-delete body)
                       (crdt--broadcast-maybe (format "%S" message) (process-get process 'client-id)))
                      ((hello) (cl-pushnew process crdt--network-clients)
                       (process-send-string process (format "%S" `(sync
                                                                   ,crdt--next-client-id
                                                                   ,(buffer-substring-no-properties (point-min) (point-max))
                                                                   ,@ (crdt--dump-ids (point-min) (point-max) nil))))
                       (process-put process 'client-id crdt--next-client-id)
                       (cl-incf crdt--next-client-id))
                      ((sync) (erase-buffer)
                       (cl-destructuring-bind (id content . ids) body
                         (insert content)
                         (setq crdt--local-id id)
                         (crdt--load-ids ids))))))))
            (delete-region (point-min) (point))
            (goto-char (point-min))))))))
(defun crdt-serve-buffer (port)
  ""
  (interactive "nPort: ")
  (crdt-mode)
  (setq crdt--local-id 0)
  (setq crdt--network-clients nil)
  (setq crdt--local-clock 0)
  (setq crdt--next-client-id 1)
  (save-excursion
    (widen)
    (let ((crdt--inhibit-update t))
      (crdt--local-insert (point-min) (point-max))))
  (setq crdt--network-process
        (make-network-process
         :name "CRDT Server"
         :server t
         :family 'ipv4
         :host "0.0.0.0"
         :service port
         :filter 'crdt--network-filter
         :plist `(crdt-buffer ,(current-buffer)))))
(defun crdt-stop-serve-buffer ()
  (interactive)
  (delete-process crdt--network-process)
  (dolist (client crdt--network-clients)
    (when (process-live-p client)
      (delete-process client))
    (when (process-buffer client)
      (kill-buffer (process-buffer client))))
  (setq crdt--network-process nil)
  (setq crdt--network-clients nil)
  (crdt-mode 0))
(defun crdt-connect (address port)
  ""
  (interactive "MAddress: \nnPort: ")
  (switch-to-buffer (generate-new-buffer "CRDT Client"))
  (crdt-mode)
  (setq crdt--network-process
        (make-network-process
         :name "CRDT Client"
         :buffer (generate-new-buffer "*crdt-client*")
         :host address
         :family 'ipv4
         :service port
         :filter 'crdt--network-filter
         :plist `(crdt-buffer ,(current-buffer))))
  (process-send-string crdt--network-process
                       (format "%S" '(hello))))
(defun crdt-test-client ()
  (interactive)
  (crdt-connect "127.0.0.1" 1333))
(defun crdt-test-server ()
  (interactive)
  (crdt-serve-buffer 1333))

(define-minor-mode crdt-mode
  "CRDT mode" nil " CRDT" nil
  (if crdt-mode
      (progn
        (add-hook 'after-change-functions #'crdt--after-change nil t)
        (add-hook 'before-change-functions #'crdt--before-change nil t))
    (remove-hook 'after-change-functions #'crdt--after-change t)
    (remove-hook 'before-change-functions #'crdt--before-change t)))
