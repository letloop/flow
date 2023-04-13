# `(import (letloop flow))`

> Asynchronous, transparent input, and output on Linux

⚠️ 🚧 work in progress, use at your own risks 🚧 ⚠️

Until it is stable the procedures are available from `(letloop flow
untangle)` and `(letloop flow entangle)` instead of the prefix `flow`,
the library name prefix e.g. `make-flow` becomes `make-entangle`, and
`flow-abort` becomes `entangle-abort`.

Both legacy epoll, and edgy io-uring are supported, respectivly
`entangle`, and `untangle`.

## `(make-flow)`

## `(flow-abort handler . rest)

Pause the current flow, and call `HANDLER` with the current
continuation, and `REST`.

## `(flow-run)`

## `(flow-sleep duration)`

## `(flow-spawn [duration] thunk)`

## `(flow-spawn-threadsafe thunk)`

## `(flow-stop)`

## `(flow-tcp-serve ip port)` `string?` `integer?` → `procedure?` `procedure?`

Connect to local `IP`, and `PORT` return two values:

- A procedure that accept new client connections;
- A procedure to shutdown the connection;

The first procedure is a generator that produce three values:

- a reader procedure that generates bytevectors;
- a writer procedure that accumulates bytevectors;
- a procedure to close the associated client connection;

## LICENSE

Distributed under the SRFI license. See `LICENSE` for more
information.
