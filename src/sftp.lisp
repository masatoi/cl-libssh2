;; -*- mode: lisp; tab-width: 4; ident-tabs-mode: nil -*-

(in-package :libssh2)

(defmacro with-sftp ((sftp-session ssh-connection) &body body)
  `(let* ((,sftp-session (libssh2-sftp-init (session ,ssh-connection)))
          (*sftp-session* ,sftp-session))
     (if (or (null ,sftp-session) (null-pointer-p ,sftp-session))
         (error "Cannot establish an SFTP session for SSH connection to ~A (port ~A)" (host,ssh-connection) (port ,ssh-connection))
         (unwind-protect
              (handler-bind ((libssh2-invalid-error-code
                              (lambda (condition)
                                (declare (ignore condition))
                                (throw-last-error (session ,ssh-connection)))))
                ,@body)
           (libssh2-sftp-shutdown ,sftp-session)))))


(defun ends-with? (name extension)
  (assert (> (length extension) 0))
  (assert (> (length name) 0))
  (let ((pos (search extension name :from-end t)))
    (when (and pos (= pos (- (length name) (length extension))))
      t)))

(defun ends-with-any? (name extensions)
  (dolist (ext extensions)
    (when (ends-with? name ext)
      (return t))))

(defun sftp-list-directory (ssh-connection path &key (maxfiles most-positive-fixnum) (extensions nil))
  "Return a list of files in directory `PATH' on the server to which we are connected with `SSH-CONNECTION'.
Restrict the number of files to retrieve by providing
  - `MAXFILES' - a fixnum (which is by default set to an insanely large number)
  - `EXTENSIONS' - a list of strings (default: nil)

It is possible to combine `MAXFILES' and `EXTENSIONS' (retrieve 5 files with extensions '(\".json\", \".js\")) "

  (with-sftp (sftp ssh-connection)
    (let* ((handle (libssh2-sftp-open-ex sftp path 0 0 (foreign-enum-value 'sftp-open-types :dir)))
           (buffer (foreign-alloc :char :count 1024 :initial-element 0))
           (longentry (foreign-alloc :char :count 1024 :initial-element 0)))
      (unwind-protect
           (with-foreign-object (attrs '(:struct _libssh2-sftp-attributes))
             (loop while (> (libssh2-sftp-readdir-ex handle buffer 1024 longentry 1024 attrs) 0)
                   while (> maxfiles (length files))
                   for attr-plist = (convert-from-foreign attrs '(:struct _libssh2-sftp-attributes))
                   do (ssh2.dribble "Attributes of ~A: ~A, permissions: ~A" (foreign-string-to-lisp buffer) attr-plist (foreign-bitfield-symbols 'sftp-modes (getf attr-plist 'permissions)))
                   when (or (null extensions)
                            (ends-with-any? (foreign-string-to-lisp buffer) extensions)) collect (foreign-string-to-lisp buffer) into files
                   finally (return files)))
        (when handle (libssh2-sftp-close-handle handle))
        (when buffer (foreign-free buffer))
        (when longentry (foreign-free longentry))))))


(defconstant +sftp-read-buffer-size+ 1000000)

(defun sftp-delete (ssh-connection remote-path)
  "Delete a remote file `PATH' on the server to which we are connected with `SSH-CONNECTION'."
  (with-sftp (sftp ssh-connection)
    (ssh2.debug "Trying to delete remote file ~A" remote-path)
    (let ((result (libssh2-sftp-unlink-ex sftp remote-path)))
      (ssh2.debug "Deleting ~A resulted in ~A." remote-path result))))

(defclass sftp-stream (fundamental-stream)
  ((remote-path :initarg :remote-path
                :accessor sftp-stream-remote-path)
   (direction :initarg :direction
              :reader sftp-stream-direction)
   (session :initarg :session
            :initform *sftp-session*
            :accessor sftp-stream-session)
   (handle :accessor sftp-stream-handle)))

(defclass sftp-input-stream (sftp-stream fundamental-input-stream) ()
  (:default-initargs
   :direction :input))

(defclass sftp-output-stream (sftp-stream fundamental-output-stream) ()
  (:default-initargs
   :direction :output))

(defmethod initialize-instance :after ((stream sftp-input-stream) &key remote-path)
  (setf (sftp-stream-handle stream)
        (libssh2-sftp-open-ex (sftp-stream-session stream)
                              remote-path
                              (foreign-bitfield-value 'sftp-flags '(:read))
                              0
                              :file))
  stream)

(defmethod initialize-instance :after ((stream sftp-output-stream) &key remote-path (if-exists :error) (if-does-not-exist :create) (file-mode #o644))
  (setf (sftp-stream-handle stream)
        (libssh2-sftp-open-ex (sftp-stream-session stream)
                              remote-path
                              (foreign-bitfield-value 'sftp-flags
                                                      `(:write
                                                        ,@(ecase if-exists
                                                            (:supersede
                                                             '(:trunc))
                                                            (:append
                                                             '(:append))
                                                            (:error
                                                             '(:excl))
                                                            (:overwrite))
                                                        ,@(ecase if-does-not-exist
                                                            (:create
                                                             '(:creat))
                                                            (:error))))
                              file-mode
                              :file))
  stream)

(defun sftp-stream-open-p (stream)
  (slot-boundp stream 'handle))

(defun close-sftp-stream (stream)
  (unless (sftp-stream-open-p stream)
    (error "SFTP stream is already closed."))
  (libssh2-sftp-close-handle (sftp-stream-handle stream))
  (values))

(defmethod stream-read-byte ((stream sftp-input-stream))
  (cffi:with-foreign-pointer (buffer 1)
    (if (= 1 (libssh2-sftp-read (sftp-stream-handle stream)
                                buffer
                                1))
        (cffi:mem-aref buffer :uint8 0)
        nil)))

(defmethod stream-element-type ((stream sftp-stream))
  '(unsigned-byte 8))

(defmethod stream-read-sequence ((stream sftp-input-stream) sequence start end &key &allow-other-keys)
  (let* ((max-size (- end start))
         (buffer-size (min max-size +sftp-read-buffer-size+)))
    (cffi:with-foreign-pointer (buffer buffer-size)
      (loop with index = start
            for read-bytes = (libssh2-sftp-read (sftp-stream-handle stream)
                                                buffer
                                                buffer-size)
            until (zerop read-bytes)
            do (loop repeat read-bytes
                     for i from index
                     for j from 0
                     do (setf (aref sequence i)
                              (cffi:mem-aref buffer :uint8 j)))
               (incf index read-bytes)
            while (< index max-size)
            finally (return index)))))

(defmethod stream-write-byte ((stream sftp-output-stream) byte)
  (cffi:with-foreign-pointer (buffer 1)
    (cffi:mem-aref buffer :uint8 byte)
    (libssh2-sftp-write (sftp-stream-handle stream)
                        buffer
                        1))
  byte)

(defmethod stream-write-sequence ((stream sftp-output-stream) sequence start end &key &allow-other-keys)
  (let* ((size (- end start))
         (buffer-size (min size +sftp-read-buffer-size+)))
    (cffi:with-foreign-pointer (buffer buffer-size)
      (loop for s = start then e
            for e = (min end (+ s buffer-size))
            do (loop for i from s below e
                     for j from 0
                     do (setf (cffi:mem-aref buffer :uint8 j)
                              (aref sequence i))
                     finally
                        (libssh2-sftp-write (sftp-stream-handle stream)
                                            buffer
                                            (1+ j)))
            while (< e end)))
    sequence))

(defun call-with-sftp-open-file (fn remote-path
                                 &rest open-initargs
                                 &key (direction :input)
                                 &allow-other-keys)
  (let ((stream
          (apply #'make-instance (ecase direction
                                   (:input 'sftp-input-stream)
                                   (:output 'sftp-output-stream))
                 :remote-path remote-path
                 (remove-from-plist open-initargs :direction))))
    (unwind-protect
         (funcall fn stream)
      (when (sftp-stream-open-p stream)
        (close-sftp-stream stream)))))

(defmacro with-sftp-open-file ((stream remote-path
                                &rest open-initargs
                                &key direction if-exists if-does-not-exist file-mode)
                               &body body)
  (declare (ignore direction if-exists if-does-not-exist file-mode))
  `(call-with-sftp-open-file
    (lambda (,stream) ,@body)
    ,remote-path
    ,@open-initargs))

(defun sftp-put (ssh-connection local-path remote-path &key (mode #o644))
  (with-sftp (sftp ssh-connection)
    (with-open-file (in local-path :direction :input :element-type '(unsigned-byte 8))
      (with-sftp-open-file (out remote-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :file-mode mode)
        (copy-stream in out)))))

(defun sftp-get (ssh-connection remote-path local-path)
  "Receive a remote file `PATH' on the server to which we are connected with `SSH-CONNECTION' to a local file at `LOCAL-PATH'."
  (with-sftp (sftp ssh-connection)
    (with-open-file (out local-path :direction :output :element-type '(unsigned-byte 8))
      (with-sftp-open-file (in remote-path)
        (copy-stream in out)))))
