;;; -*- mode: Lisp; syntax: Common-Lisp; -*-

(in-package :asdf)

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(defsystem :libssh2
  :description "Trivial libssh2 bindings"
  :version      "0.1"
  :author       "Oleksii Shevchuk <alxchk@gmail.com>"
  :license      "Public Domain"
  :depends-on   (:babel
                 :cffi
                 :cl-fad
                 :log4cl
                 :split-sequence
                 :trivial-gray-streams
                 :usocket
                 :alexandria)
  :serial       t
  :components   ((:module "src"
                  :serial t
                  :components ((:file "package")
                               (:file "logging")
                               (:file "types")
                               (cffi-grovel:grovel-file "libssh2-libc-cffi")
                               (:file "util")
                               (:file "libssh2-cffi")
                               (:file "streams")
                               (:file "scp")
                               (:file "sftp")
                               (:file "solutions")))))
