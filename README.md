
[![tdrhq](https://circleci.com/gh/tdrhq/cl-unix-sockets.svg?style=shield)](https://app.circleci.com/pipelines/github/tdrhq/cl-unix-sockets?branch=master)

# UNIX socket support for Common Lisp

Most major Lisp implementations don't come with any in-built support
for UNIX sockets. This is a simple library that provides a reasonable
implementation using FFI.

At the moment I'm prioritizing stability over performance (so for
instance I don't have stream-read-sequence implemented). In the
future, we'll add those tweaks.

The biggest issue with using FFI, is making sure all the FFI calls are
interruptible. This interruptible IO is currently only supported on
Lispworks, where we use Lispworks' native socket stack as much as
possible. In theory this should be doable on at least SBCL too.


## Author

Arnold Noronha <arnold@tdrhq.com>
