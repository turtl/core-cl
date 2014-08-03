#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ecl/ecl.h>
#include "turtl.h"

#ifdef __cplusplus
extern "C" {
#endif

#define TURTL_ERROR_BUFFSIZE 1024

int turtl_running = 0;
char turtl_last_error[TURTL_ERROR_BUFFSIZE];

/**
 * Start our lisp process, do any needed setup, and load our main lisp app.
 */
TURTL_EXPORT int TURTL_CONV turtl_init(uint8_t flags)
{
	int res;
	cl_object evalres;

	turtl_last_error[0] = '\0';

	res = cl_boot(0, (char **)&"");

	if(res != 1)
	{
		printf("turtl: init: err: call to cl_boot() failed.");
		return 1;
	}

	if(turtl_running == 1)
	{
		printf("turtl: init: err: turtl is already running.");
		return 3;
	}

	if((flags >> TURTL_FLAG_SINGLE_THREADED) & 1)
	{
		si_safe_eval(3, c_string_to_object("(defparameter cl-user::*turtl-single-threaded* t)"), Cnil, OBJNULL);
	}

	if((flags >> TURTL_FLAG_DISABLE_OUTPUT) & 1)
	{
		si_safe_eval(3, c_string_to_object("\
			#+windows\
			(progn\
			  (defvar *old-terminal-io* *terminal-io*)\
			  (defvar *old-error-output* *error-output*)\
			  (setf *error-output* (make-broadcast-stream))\
			  (setf *standard-output* (make-string-output-stream)))\
		"), Cnil, OBJNULL);
	}

	cl_object init_form = c_string_to_object("\
		(handler-case\
		  (progn\
		    (si:trap-fpe t nil)\
			(load \"core/app/bootstrap\")\
			(values t nil))\
		  (t (e) (values nil e)))\
	");
	cl_object output = si_safe_eval(3, init_form, Cnil, OBJNULL);
	cl_object err = VALUES(1);

	if(err != Cnil)
	{
		cl_object errstr = cl_format(3, Cnil, c_string_to_object("\"~a\""), err);
		int i;
		int c = errstr->string.dim;
		if(c > TURTL_ERROR_BUFFSIZE - 1) c = TURTL_ERROR_BUFFSIZE - 1;
		for(i = 0; i < c; i++)
		{
			turtl_last_error[i] = ecl_char(errstr, i);
		}
		turtl_last_error[i + 1] = '\0';
		printf("turtl: init: err: failed to eval main loader: %s\n", turtl_last_error);
		return 2;
	}

	turtl_running = 1;
	atexit(turtl_shutdown);

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
 * Get the last error from turtl_init
 */
TURTL_EXPORT char* TURTL_CONV turtl_get_last_error()
{
	return turtl_last_error;
}

#ifdef EXE

#ifdef _WIN32
	#define SLEEP _sleep
#else
	#define SLEEP sleep
#endif

int main(int argc, char **argv)
{
	int init = turtl_init(0);
	if(init != 0)
	{
		printf("%s\n", turtl_get_last_error());
		return 1;
	}

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
