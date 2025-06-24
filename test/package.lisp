;;; -*- mode: lisp; syntax: common-lisp; indent-tabs-mode: nil -*-

(in-package #:cl-user)

(defpackage #:libssh2.test
  (:use #:common-lisp #:libssh2 #:rove)
  (:export #:run-unit-tests
           #:run-integration-tests
           #:run-all-tests))

(in-package #:libssh2.test)

;; The suite 'unit' is used for real unit tests which just test the code
;; and don't depend on the presence of an SSH server
(defpackage #:libssh2.test.unit
  (:use #:common-lisp #:libssh2 #:rove))

;; The suite 'integration' is used for integration tests which depend on an
;; SSH server and some previously created users
(defpackage #:libssh2.test.integration
  (:use #:common-lisp #:libssh2 #:rove))

(defun run-all-tests ()
  (handler-bind ((libssh2::known-hosts-reading-error (lambda (c) (declare (ignore c)) (invoke-restart 'accept-always) t))
                 (libssh2::ssh-unknown-hostkey (lambda (c) (declare (ignore c)) (invoke-restart 'accept-always) t))
                 (libssh2::ssh-authentication-failure (lambda (c) (declare (ignore c)) (invoke-restart 'accept-always) t)))
    (progn
      (run 'libssh2.test.unit)
      (run 'libssh2.test.integration))))

(defun run-unit-tests ()
  (run 'libssh2.test.unit))

(defun run-integration-tests ()
  (handler-bind ((libssh2::known-hosts-reading-error (lambda (c) (declare (ignore c)) (invoke-restart 'accept-always) t))
                 (libssh2::ssh-unknown-hostkey (lambda (c) (declare (ignore c)) (invoke-restart 'accept-always) t))
                 (libssh2::ssh-authentication-failure (lambda (c) (declare (ignore c)) (invoke-restart 'accept-always) t)))
    (run 'libssh2.test.integration)))

(defparameter *known-hosts-path* (namestring
                                  (merge-pathnames
                                   (make-pathname :directory '(:relative ".ssh")
                                                  :name "libssh2-known_hosts")
                                   (user-homedir-pathname))))

(defparameter *test-host* "localhost"
  "Host name or IP of the SSH server which is used for integration testing. Default value is 'localhost'; set this via command line (-e) before starting tests.")

(defparameter *user1* nil
  "Login name of the first user for integration testing; set this via command line (-e) before starting tests. ")

(defparameter *password1* nil
  "Password of the first user for integration testing; set this via command line (-e) before starting tests. ")

(defparameter *user2* nil
  "Login name of the second user for integration testing; set this via command line (-e) before starting tests. ")

(defparameter *password2* nil
  "Password of the second user for integration testing; set this via command line (-e) before starting tests. ")
