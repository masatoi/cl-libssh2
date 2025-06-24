# cl-libssh2

[![CI](https://github.com/masatoi/cl-libssh2/workflows/CI/badge.svg)](https://github.com/masatoi/cl-libssh2/actions/workflows/ci.yml)
[![License](https://img.shields.io/badge/license-Public%20Domain-blue.svg)](LICENSE)

A Common Lisp wrapper around the libssh2 C library for SSH2 protocol support.

## Overview

cl-libssh2 provides both low-level CFFI bindings and high-level stream-based APIs for SSH connections, SCP file transfers, and SFTP operations.

## Features

- **SSH Connection Management**: Easy-to-use macros for establishing and managing SSH connections
- **Multiple Authentication Methods**: Support for password, public key, and SSH agent authentication
- **SCP File Transfer**: Simple file upload/download functionality
- **SFTP Support**: Directory listing, file operations, and stream-based file access
- **Stream Integration**: Gray streams implementation for seamless integration with Common Lisp I/O
- **Error Handling**: Comprehensive error handling with restart-based recovery

## Dependencies

- libssh2 C library
- CFFI
- Babel
- CL-FAD
- Log4CL
- Split-Sequence
- Trivial-Gray-Streams
- Usocket
- Alexandria

## Installation

```lisp
(ql:quickload :libssh2)
```

## Basic Usage

### SSH Connection

```lisp
(libssh2:with-ssh-connection sshc
    ("example.com"
     (libssh2:make-password-auth "username" "password")
     :hosts-db "/path/to/known_hosts")
  ;; Execute commands, transfer files, etc.
  )
```

### SCP File Transfer

```lisp
;; Upload a file
(libssh2:scp-put local-file remote-path)

;; Download a file
(libssh2:scp-get remote-path local-file)
```

### SFTP Operations

```lisp
;; List directory contents
(libssh2:sftp-list-directory ssh-connection "/remote/path")

;; Upload file via SFTP
(libssh2:sftp-put ssh-connection local-file remote-file)

;; Download file via SFTP
(libssh2:sftp-get ssh-connection remote-file local-file)
```

### Remote Command Execution

```lisp
(libssh2:with-execute* (stream ssh-connection "ls -la")
  (loop for line = (read-line stream nil)
        while line
        do (format t "~A~%" line)))
```

## Testing

The library includes comprehensive tests using the Rove testing framework:

```lisp
;; Load test system
(ql:quickload :libssh2.test)

;; Run all tests (requires SSH server setup)
(libssh2.test:run-all-tests)

;; Run only unit tests (no SSH server required)
(libssh2.test:run-unit-tests)

;; Run integration tests (requires SSH server)
(libssh2.test:run-integration-tests)
```

For integration tests, you need to set up test parameters:
```lisp
(setf libssh2.test::*test-host* "localhost"
      libssh2.test::*user1* "testuser1"
      libssh2.test::*password1* "testpass1"
      libssh2.test::*user2* "testuser2"
      libssh2.test::*password2* "testpass2")
```

## License

This software is released into the Public Domain.

## Author

Oleksii Shevchuk <alxchk@gmail.com>