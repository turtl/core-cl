#include <stdio.h>
#include <stdlib.h>
#include <ecl/ecl.h>
#include "turtl.h"

#ifdef __cplusplus
extern "C" {
#endif

int turtl_running = 0;

/**
 * Start our lisp process, do any needed setup, and load our main lisp app.
 */
TURTL_EXPORT void TURTL_CONV turtl_init()
{
	cl_object res;
	cl_boot(0, (char **)&"");

	if(turtl_running == 1)
	{
		printf("turtl_init() called, but turtl is already running.");
		return;
	}

	turtl_running = 1;
	atexit(turtl_shutdown);

	// this will boot our app
	res	=	si_safe_eval(3, c_string_to_object("\
		(progn\
		  (ext:set-limit 'ext:frame-stack 2048)\
		  (ext:set-limit 'ext:c-stack (expt 2 18))\
		  (ext:set-limit 'ext:lisp-stack (expt 2 19))\
		  (handler-case (load \"app/loader\") (t (e) (format t \"error: ~a~%\" e))))\
	"), Cnil, OBJNULL);
	if(res == OBJNULL)
	{
		printf("Error occured\n");
	}
}

/**
 * Shutdown the lisp world.
 */
TURTL_EXPORT void TURTL_CONV turtl_shutdown()
{
	if(turtl_running == 0)
	{
		return;
	}
	turtl_running = 0;
	cl_shutdown();
}

/**
 * Initialize our (empty) message callback functions. These are used to register
 * message passing callbacks between our lisp and UI worlds.
 */
typedef void (*msg_callback)(unsigned long long, char *);
void turtl_void_msg_handler(unsigned long long msg_length, char *msg) { }
msg_callback lisp_msg_handler = turtl_void_msg_handler;
msg_callback ui_msg_handler = turtl_void_msg_handler;

/**
 * Set our lisp message handler
 */
TURTL_EXPORT void TURTL_CONV turtl_set_lisp_msg_handler(msg_callback callback)
{
	lisp_msg_handler = callback;
}

/**
 * Set our UI message handler
 */
TURTL_EXPORT void TURTL_CONV turtl_set_ui_msg_handler(msg_callback callback)
{
	ui_msg_handler = callback;
}

/**
 * Pass a message from UI to lisp
 */
TURTL_EXPORT void TURTL_CONV turtl_msg_to_lisp(unsigned long long msg_length, char *msg)
{
	(*lisp_msg_handler)(msg_length, msg);
}

/**
 * Pass a message from lisp to UI
 */
TURTL_EXPORT void TURTL_CONV turtl_msg_to_ui(unsigned long long msg_length, char *msg)
{
	(*ui_msg_handler)(msg_length, msg);
}

#ifdef EXE
int main(int argc, char **argv)
{
	turtl_init();
	return 0;
}
#endif

#ifdef __cplusplus
}		// extern "C" { ... }
#endif
