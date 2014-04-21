#ifdef __cplusplus
extern "C" {
#endif

/**
 * Start our lisp process, do any needed setup, and load our main lisp app.
 */
extern void turtl_init();

/**
 * Shutdown the lisp world.
 */
extern void turtl_shutdown();

/**
 * Initialize our (empty) message callback functions. These are used to register
 * message passing callbacks between our lisp and UI worlds.
 */
typedef void (*msg_callback)(unsigned long long, char *);

/**
 * Set our lisp message handler
 */
extern void turtl_set_lisp_msg_handler(msg_callback callback);

/**
 * Set our UI message handler
 */
extern void turtl_set_ui_msg_handler(msg_callback callback);

/**
 * Pass a message from UI to lisp
 */
extern void turtl_msg_to_lisp(unsigned long long msg_length, char *msg);

/**
 * Pass a message from lisp to UI
 */
extern void turtl_msg_to_ui(unsigned long long msg_length, char *msg);

#ifdef __cplusplus
}		// extern "C" { ... }
#endif
