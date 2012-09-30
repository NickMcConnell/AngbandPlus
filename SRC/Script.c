/* File: script.c */

/* Purpose: Script interface */

#include "angband.h"

#ifdef USE_SCRIPT

#include <python.h>

/*
 * Execute a Python script
 */
errr script_execute(char *name)
{
	char buf[1024];

	if (!name || !*name)
	{
		msg_print("Oops!  No script given.");
		return (1);
	}

	/* Load a script file */
	if (name[0] == '@')
	{
		FILE *fff;
		int result;

		/* Skip the '@' */
		name++;

		/* Build the filename */
		path_build(buf, 1024, ANGBAND_DIR_SCRIPT, name);

		fff = my_fopen(buf, "rw");

		result = PyRun_SimpleFile(fff, buf);

		my_fclose(fff);

		return result;
	}
	else
	{
		return PyRun_SimpleString(name);
	}

	return (0);
}


#ifdef STATIC_PYTHON
extern void initeventc(void);
extern void initplayerc(void);
extern void initioc(void);
extern void initobjectsc(void);
extern void initcavec(void);
extern void initmonsterc(void);
extern void initpclassc(void);
extern void initterrainc(void);
extern void initcommandsc(void);
extern void initrealmsc(void);
extern void initrandomc(void);
#endif /* STATIC_PYTHON */


/*
 * Initialize the script support
 */
errr init_script(void)
{
	char buf[1024];
	char path[1024];

	Py_SetProgramName((char*)argv0);

	/* Set the enviroment variables */
#if __djgpp__
	setenv("PYTHONPATH", ANGBAND_DIR_SCRIPT, 1);
	setenv("PYTHONHOME", ANGBAND_DIR_SCRIPT, 1);
#endif /* __djgpp__ */

	/* Initialize the Python interpreter */
	Py_Initialize();

	/* Convert the script path to a nice string */
	ascii_to_text(path, ANGBAND_DIR_SCRIPT);

	/* Add the "script" directory to the module search path */
	sprintf(buf, "import sys; sys.path.insert(0, '%s')", path);

	if (PyRun_SimpleString(buf) == 0)
	{
#ifdef STATIC_PYTHON
		initeventc();
		initplayerc();
		initioc();
		initobjectsc();
		initcavec();
		initmonsterc();
		initpclassc();
		initterrainc();
		initcommandsc();
		initrealmsc();
		initrandomc();
#endif /* STATIC_PYTHON */

		if (PyRun_SimpleString("import init") == 0) return 0;
	}

	return -1;
}

#endif /* USE_SCRIPT */
