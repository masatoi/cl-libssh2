;; -*- mode: lisp; syntax: common-lisp -*-

(in-package :libssh2)

;; Define logger categories
(log:config :info)

;; Define logging macros for different levels
(defmacro ssh2.dribble (format-string &rest args)
  `(log:debug ,format-string ,@args))

(defmacro ssh2.debug (format-string &rest args)
  `(log:debug ,format-string ,@args))

(defmacro ssh2.info (format-string &rest args)
  `(log:info ,format-string ,@args))

(defmacro ssh2.warn (format-string &rest args)
  `(log:warn ,format-string ,@args))

(defmacro ssh2.error (format-string &rest args)
  `(log:error ,format-string ,@args))

(defmacro ssh2.fatal (format-string &rest args)
  `(log:fatal ,format-string ,@args))
