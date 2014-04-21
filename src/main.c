#include <stdio.h>
#include <ecl/ecl.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Start our lisp process, do any needed setup, and load our main lisp app.
 */
extern void turtl_init()
{
	char **argv;
	cl_object res;
	cl_boot(0, argv);
	// this will boot our app
	res	=	si_safe_eval(3, c_string_to_object("\
		(progn\
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
extern void turtl_shutdown()
{
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
extern void turtl_set_lisp_msg_handler(msg_callback callback)
{
	lisp_msg_handler = callback;
}

/**
 * Set our UI message handler
 */
extern void turtl_set_ui_msg_handler(msg_callback callback)
{
	ui_msg_handler = callback;
}

/**
 * Pass a message from UI to lisp
 */
extern void turtl_msg_to_lisp(unsigned long long msg_length, char *msg)
{
	(*lisp_msg_handler)(msg_length, msg);
}

/**
 * Pass a message from lisp to UI
 */
extern void turtl_msg_to_ui(unsigned long long msg_length, char *msg)
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
