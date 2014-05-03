#ifndef _TURTLH_

#define _TURTLH_

#ifdef _WIN32
	#define TURTL_EXPORT __declspec(dllexport)
	#define TURTL_CONV __cdecl
#else
	#define TURTL_EXPORT extern
	#define TURTL_CONV
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Start our lisp process, do any needed setup, and load our main lisp app.
 */
TURTL_EXPORT int TURTL_CONV turtl_init();

/**
 * Shutdown the lisp world.
 */
TURTL_EXPORT void TURTL_CONV turtl_shutdown();

/**
 * Get the last error from turtl_init
 */
TURTL_EXPORT char* TURTL_CONV turtl_get_last_error();

/**
 * Initialize our (empty) message callback functions. These are used to register
 * message passing callbacks between our lisp and UI worlds.
 */
typedef void (*turtl_msg_callback_t)(unsigned long, const char *);

/**
 * Set our lisp message handler
 */
TURTL_EXPORT void TURTL_CONV turtl_set_lisp_msg_handler(turtl_msg_callback_t callback);

/**
 * Set our UI message handler
 */
TURTL_EXPORT void TURTL_CONV turtl_set_ui_msg_handler(turtl_msg_callback_t callback);

/**
 * Pass a message from UI to lisp
 */
TURTL_EXPORT void TURTL_CONV turtl_msg_to_lisp(unsigned long msg_length, const char *msg);

/**
 * Pass a message from lisp to UI
 */
TURTL_EXPORT void TURTL_CONV turtl_msg_to_ui(unsigned long msg_length, const char *msg);

#ifdef __cplusplus
}		// extern "C" { ... }
#endif

#endif //_TURTLH_
