#include <stdio.h>
#include <ecl/ecl.h>

void turtl_init()
{
	char **argv;
	cl_object res;
	cl_boot(0, argv);
	// this will boot our app
	res	=	si_safe_eval(3, c_string_to_object("\
		(progn\
		  (handler-case (load \"src/loader\") (t (e) (format t \"error: ~a~%\" e))))\
	"), Cnil, OBJNULL);
	if(res == OBJNULL)
	{
		printf("Error occured\n");
	}
}

void turtl_shutdown()
{
	cl_shutdown();
}

#ifdef EXE
int main(int argc, char **argv)
{
	turtl_init();
	return 0;
}
#endif
