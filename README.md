Turtl core
==========

This was an experimental project started in the hopes to move all of Turtl's
core logic to lisp, as a DLL, and have various platforms (desktop/mobile) load
it.

It accomplished this with a fairly high level of success, however its continued
existence could not be justified as platforms that ran HTML5 became more and
more sophisticated.

As such, it has been retired and now lives as a reference, specifically for
those interested in running a lisp app embedded in another platform.


## Architecture

This will be brief, but mainly the app is loaded as a DLL. The DLL includes
nanomsg, and the loading platform uses this to set up communication channels
between itself and the lisp app.

Essentially, everything is done via message passing, which must be set up on
the loading platform's side (although once done, then it had full access to
turtl-core's abilities).

This was tested and worked in what used to be node-webkit (now nw.js) as a
native node module, and also on firefox's app platform.

It was never tested on any mobile platform, although in theory one could cross
compile ECL and the other required libs for android/ios without too much
trouble, then it's as simple as loading a third-party lib.

