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

// these flags are bit positions set in the value passed to turtl_init
#define TURTL_FLAG_SINGLE_THREADED 7
#define TURTL_FLAG_DISABLE_OUTPUT 6
#define TURTL_FLAG_PUSH_MESSAGES 5

/**
 * Start our lisp process, do any needed setup, and load our main lisp app.
 */
TURTL_EXPORT int TURTL_CONV turtl_init(uint8_t);

/**
 * Shutdown the lisp world.
 */
TURTL_EXPORT void TURTL_CONV turtl_shutdown();

/**
 * Get the last error from turtl_init
 */
TURTL_EXPORT char* TURTL_CONV turtl_get_last_error();

/**
 * Define our callback types
 */
typedef void (*turtl_msg_callback_t)(unsigned long, const unsigned char *);
typedef int (*turtl_ui_poll_callback_t)(void);

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
TURTL_EXPORT void TURTL_CONV turtl_msg_to_lisp(unsigned long msg_length, const unsigned char *msg);

/**
 * Pass a message from lisp to UI
 */
TURTL_EXPORT void TURTL_CONV turtl_msg_to_ui(unsigned long msg_length, const unsigned char *msg);

/**
 * Allow the UI to poll for pending messages (as opposed to having them pushed
 * to the UI thread which can cause problems unless libuv is running).
 */
TURTL_EXPORT void TURTL_CONV turtl_set_ui_msg_poll_handler(turtl_ui_poll_callback_t callback);

/**
 * Poll the UI message queue
 */
TURTL_EXPORT int TURTL_CONV turtl_poll_ui_messages();

#ifdef __cplusplus
}		// extern "C" { ... }
#endif

#endif //_TURTLH_
