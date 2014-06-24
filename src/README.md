## turtl-core C library

This directory houses a very thin C API aimed to ease the burden of embedding
turtl-core into other systems.

The main api is housed in `turtl.h`, and should be fairly self-documenting.

### Lisp <--> UI communication

Generally the way things work is that each system (both lisp and whatever UI is
communicating with lisp) registers a message callback before sending messages.
Once complete, messages are sent from one system to the registered calback of
the other.

There are two ways for lisp to communicate with the UI thread. It can either
push messages or let the UI call a polling function (which in turn calls the
registered message callback for each message).

Pushing can only be done if the UI system is thread-aware. For instance, when
using a Node variant (eg Node-webkit) our comm system can pass messages to the
main UI event loop using libuv's cross-thread messaging capabilities.

In something like Firefox we're not so lucky, and Firefox itself must poll for
lisp -> UI messages manually.

The method used is determined by the `TURTL_FLAG_PUSH_MESSAGES` bit sent into
the flags passed to `turtl_init`.

