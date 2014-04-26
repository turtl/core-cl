#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ecl/ecl.h>
#include "turtl.h"

#ifdef __cplusplus
extern "C" {
#endif

int turtl_running = 0;

/**
 * Start our lisp process, do any needed setup, and load our main lisp app.
 */
TURTL_EXPORT int TURTL_CONV turtl_init()
{
	int res;
	cl_object evalres;

	res = cl_boot(0, (char **)&"");

	if(res != 1)
	{
		printf("call to cl_boot() failed.");
		return 1;
	}

	if(turtl_running == 1)
	{
		printf("turtl_init() called, but turtl is already running.");
		return 3;
	}

	turtl_running = 1;
	atexit(turtl_shutdown);

	// this will boot our app
	evalres = si_safe_eval(3, c_string_to_object("\
		(progn\
		  (si::trap-fpe t nil)\
		  (ext:set-limit 'ext:c-stack (expt 2 18))\
		  (ext:set-limit 'ext:lisp-stack (expt 2 19))\
		  (handler-case (load \"app/loader\") (t (e) (format t \"error: ~a~%\" e))))\
	"), Cnil, OBJNULL);
	if(evalres == OBJNULL)
	{
		printf("Uncaught error while evaluating top-level lisp code.\n");
		return 2;
	}

	return 0;
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
	return;
}

/**
 * Initialize our (empty) message callback functions. These are used to register
 * message passing callbacks between our lisp and UI worlds.
 */
typedef void (*turtl_msg_callback_t)(unsigned long, const char *);
void turtl_void_msg_handler(unsigned long msg_length, const char *msg) { }
turtl_msg_callback_t lisp_msg_handler = turtl_void_msg_handler;
turtl_msg_callback_t ui_msg_handler = turtl_void_msg_handler;

/**
 * Set our lisp message handler (for ui -> lisp messages)
 */
TURTL_EXPORT void TURTL_CONV turtl_set_lisp_msg_handler(turtl_msg_callback_t callback)
{
	lisp_msg_handler = callback;
}

/**
 * Set our UI message handler (for lisp -> ui messages)
 */
TURTL_EXPORT void TURTL_CONV turtl_set_ui_msg_handler(turtl_msg_callback_t callback)
{
	ui_msg_handler = callback;
}

/**
 * Pass a message from UI to lisp
 */
TURTL_EXPORT void TURTL_CONV turtl_msg_to_lisp(unsigned long msg_length, const char *msg)
{
	(*lisp_msg_handler)(msg_length, msg);
}

/**
 * Pass a message from lisp to UI
 */
TURTL_EXPORT void TURTL_CONV turtl_msg_to_ui(unsigned long msg_length, const char *msg)
{
	(*ui_msg_handler)(msg_length, msg);
}

#ifdef EXE

#ifdef _WIN32
	#define SLEEP _sleep
#else
	#define SLEEP sleep
#endif

int main(int argc, char **argv)
{
	turtl_init();
	while(1)
	{
		SLEEP(.1);
	}
	return 0;
}
#endif

#ifdef __cplusplus
}		// extern "C" { ... }
#endif
