# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is `cl-libssh2`, a Common Lisp wrapper around the libssh2 C library for SSH2 protocol support. It provides both low-level CFFI bindings and high-level stream-based APIs for SSH connections, SCP file transfers, and SFTP operations.

## Development Commands

### Building and Loading
```lisp
;; Load the system
(ql:quickload :libssh2)

;; Load tests
(ql:quickload :libssh2.test)

;; Run all tests
(libssh2.test:run-all-tests)

;; Run only unit tests (no SSH server required)
(libssh2.test:run-unit-tests)

;; Run integration tests (requires SSH server)
(libssh2.test:run-integration-tests)
```

### Test System
- Uses `rove` testing framework
- Unit tests in `libssh2.test.unit` package don't require external SSH server
- Integration tests in `libssh2.test.integration` package require SSH server setup
- Test configuration uses parameters: `*test-host*`, `*user1*`, `*password1*`, `*user2*`, `*password2*`
- CI/CD runs on GitHub Actions with matrix testing across SBCL, CCL, and ECL

## Architecture

### Core Components

**CFFI Layer** (`src/libssh2-cffi.lisp`):
- Direct bindings to libssh2 C library
- Foreign function definitions and error handling
- Uses `result-or-error` macro for consistent error handling

**High-Level API** (`src/solutions.lisp`):
- Stream-based abstractions over raw libssh2 calls
- Authentication methods: password, public key, SSH agent
- Connection management with `with-ssh-connection` macro

**Protocol Implementations**:
- **SCP** (`src/scp.lisp`): File transfer using SCP protocol
- **SFTP** (`src/sftp.lisp`): File operations using SFTP protocol
- **Streams** (`src/streams.lisp`): Gray streams implementation for SSH channels

### Key Design Patterns

**Dynamic Variables**:
- `*ssh-connection*`: Current SSH connection context
- `*sftp-session*`: Active SFTP session
- `*errors-list*`: Configurable error handling

**Macro-Based Resource Management**:
- `with-ssh-connection`: Establishes and tears down SSH connections
- `with-scp-input/output`: Manages SCP file transfers
- `with-execute*`: Executes remote commands

**Authentication System**:
- Pluggable auth methods: `auth-password`, `auth-publickey`, `auth-agent`
- Automatic key discovery in standard SSH directories
- Fallback authentication chain support

### Error Handling
- Custom condition system with `ssh-generic-error`
- Configurable error lists for different scenarios
- Restart-based error recovery (accept-once, accept-always, drop)

## Dependencies
- `cffi`: C foreign function interface
- `trivial-gray-streams`: Stream protocol implementation
- `usocket`: Network socket abstraction
- `log4cl`: Logging framework
- `rove`: Testing framework (tests only)