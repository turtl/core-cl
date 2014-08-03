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

#ifdef __cplusplus
}		// extern "C" { ... }
#endif

#endif //_TURTLH_
