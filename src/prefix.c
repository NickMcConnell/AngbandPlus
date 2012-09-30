/*
 * BinReloc - a library for creating relocatable executables
 * Written by: Mike Hearn <mike@theoretic.com>
 *             Hongli Lai <h.lai@chello.nl>
 * http://autopackage.org/
 * 
 * This source code is public domain.
 *
 * VERSIONING:
 *   If you make a breaking change to any of these functions, you MUST version them
 * by appending a number to the end, or incrementing it. If an organization other than
 * autopackage.org changes these functions, they must namespace it with the name of the
 * project or software also. This is to allow many different objects that may be linked
 * together to use this code, without symbol table conflicts.
 *
 */

#if !defined(_PREFIX_C_) && defined(__GNUC__) && defined(ENABLE_BINRELOC)
#define _PREFIX_C_

#include <stdlib.h>
#include <stdio.h>
#include <features.h>
#define __USE_GNU
#include <dlfcn.h>
#include <limits.h>
#include <pthread.h>
#define __USE_GNU
#include <string.h>

#include "prefix.h"


static pthread_key_t __br_thread_key;
static pthread_once_t __br_thread_key_once = PTHREAD_ONCE_INIT;

#define br_return_val_if_fail(expr,val) if (!(expr)) {fprintf (stderr, "** BinReloc (%s): assertion %s failed\n", __PRETTY_FUNCTION__, #expr); return val;}
static void
__br_thread_local_store_fini ()
{
	char *specific;

	specific = pthread_getspecific (__br_thread_key);
	if (specific)
	{
		free (specific);
		pthread_setspecific (__br_thread_key, NULL);
	}
	pthread_key_delete (__br_thread_key);
	__br_thread_key = 0;
}


static void
__br_thread_local_store_init ()
{
	pthread_key_create (&__br_thread_key, free);
	atexit (__br_thread_local_store_fini);
}


/**
 * br_thread_local_store:
 * str: A string.
 * Returns: str. This return value must not be freed.
 *
 * Store str in a thread-local variable and return str. The next
 * you run this function, that variable is freed too.
 * This function is created so you don't have to worry about freeing
 * strings.
 *
 * Example:
 * char *foo;
 * foo = br_thread_local_store (strdup ("hello")); // foo == "hello"
 * foo = br_thread_local_store (strdup ("world")); // foo == "world"; "hello" is now freed.
 */
const char *
br_thread_local_store (char *str)
{
	char *specific;

	pthread_once (&__br_thread_key_once, __br_thread_local_store_init);

	specific = pthread_getspecific (__br_thread_key);
	if (specific) free (specific);
	pthread_setspecific (__br_thread_key, str);

	return str;
}


/**
 * br_locate:
 * symbol: A symbol that belongs to the app/library you want to locate.
 * Returns: A newly allocated string containing the full path of the
 *	    app/library that func belongs to, or NULL on error. This
 *	    string should be freed when not when no longer needed.
 *
 * Finds out to which application or library symbol belongs, then locate
 * the full path of that application or library.
 * Note that symbol cannot be a pointer to a function. That will not work.
 *
 * Example:
 * // main.c
 * #include "prefix.h"
 * #include "libfoo.h"
 *
 * int main (int argc, char *argv[]) {
 *	printf ("Full path of this app: %s\n", br_locate (&argc));
 *	libfoo_start ();
 *	return 0;
 * }
 *
 * // libfoo.c starts here
 * #include "prefix.h"
 *
 * void libfoo_start () {
 *	// "" is a symbol that belongs to libfoo (because it's called
 *	// from libfoo_start()); that's why this works.
 *	printf ("libfoo is located in: %s\n", br_locate (""));
 * }
 */
char *
br_locate (void *symbol)
{
	Dl_info info;
	char line[5000];
	FILE *f;
	char *path;

	br_return_val_if_fail (symbol != NULL, NULL);

	if (!dladdr (symbol, &info))
	{
		fprintf (stderr, "** BinReloc (br_locate): dladdr() returned NULL!\n");
		return NULL;
	}
	f = fopen ("/proc/self/maps", "r");
	if (!f)
	{
		fprintf (stderr, "** BinReloc (br_locate): cannot open /proc/self/maps for reading!\n");
		return NULL;
	}

	while (fgets (line, sizeof (line), f))
	{
		unsigned int start;

		sscanf (line, "%x-", &start);

		if (start == (unsigned int) info.dli_fbase)
		{
			char *tmp;
			size_t len;

			/* Extract the filename; it is always an absolute path */
			path = strchr (line, '/');

			/* Get rid of the newline */
			tmp = strrchr (path, '\n');
			if (tmp) *tmp = 0;

			/* Get rid of "(deleted)" */
			len = strlen (path);
			if (len > 10 && strcmp (path + len - 10, " (deleted)") == 0)
			{
				tmp = path + len - 10;
				*tmp = 0;
			}

			fclose(f);
			return strdup (path);
		}
	}

	fprintf (stderr, "BinReloc (br_locate) WARNING: unable to locate linker map for symbol 0x%p\n", symbol);
	fclose (f);
	return NULL;
}


/**
 * br_locate_prefix:
 * symbol: A symbol that belongs to the app/library you want to locate.
 * Returns: A prefix. This string should be freed when no longer needed.
 *
 * Locates the full path of the app/library that symbol belongs to, and return
 * the prefix of that path, or NULL on error.
 * Note that symbol cannot be a pointer to a function. That will not work.
 *
 * Example:
 * // This application is located in /usr/bin/foo
 * br_locate_prefix (&argc);   // returns: "/usr"
 */
char *
br_locate_prefix (void *symbol)
{
	char *path, *prefix;

	br_return_val_if_fail (symbol != NULL, NULL);

	path = br_locate (symbol);
	if (!path) return NULL;

	prefix = br_extract_prefix (path);
	free (path);
	return prefix;
}


/**
 * br_strcat:
 * str1: A string.
 * str2: Another string.
 * Returns: A newly-allocated string. This string should be freed when no longer needed.
 *
 * Concatenate str1 and str2 to a newly allocated string.
 */
char *
br_strcat (const char *str1, const char *str2)
{
	char *result;

	if (!str1) str1 = "";
	if (!str2) str2 = "";

	result = calloc (sizeof (char), strlen (str1) + strlen (str2) + 1);
	result = strcpy (result, str1);
	result = strcat (result, str2);
	return result;
}


/**
 * br_extract_prefix:
 * path: The full path of an executable or library.
 * Returns: The prefix, or NULL on error. This string should be freed when no longer needed.
 *
 * Extracts the prefix from path. This function assumes that your executable
 * or library is installed in an LSB-compatible directory structure.
 *
 * Example:
 * br_extract_prefix ("/usr/bin/gnome-panel");   // Returns "/usr"
 * br_extract_prefix ("/usr/local/libfoo.so");   // Returns "/usr/local"
 */
char *
br_extract_prefix (const char *path)
{
	char *end, *tmp, *result;

	br_return_val_if_fail (path != NULL, NULL);

	end = strrchr (path, '/');
	if (!end) return strdup (path);

	tmp = strndup ((char *) path, end - path);
	end = strrchr (tmp, '/');
	if (!end) return tmp;

	result = strndup (tmp, end - tmp);
	free (tmp);

	if (!*result)
	{
		free (result);
		result = strdup ("/");
	}

	return result;
}


/**
 * br_prepend_prefix:
 * symbol: A symbol that belongs to the app/library you want to locate.
 * dir: The path that you want to prepend the prefix to.
 * Returns: The new path, or NULL on error. This string should be freed when no
 *	    longer needed.
 *
 * Gets the prefix of the app/library that symbol belongs to. Prepend that prefix to path.
 * Note that symbol cannot be a pointer to a function. That will not work.
 *
 * Example:
 * // The application is /usr/bin/foo
 * br_prepend_prefix (&argc, "/share/foo/data.png");   // Returns "/usr/share/foo/data.png"
 */
char *
br_prepend_prefix (void *symbol, char *path)
{
	char *tmp, *newpath;

	br_return_val_if_fail (symbol != NULL, NULL);
	br_return_val_if_fail (path != NULL, NULL);

	tmp = br_locate_prefix (symbol);
	if (!tmp) return NULL;

	if (strcmp (tmp, "/") == 0)
		newpath = strdup (path);
	else
		newpath = br_strcat (tmp, path);

	/* Get rid of compiler warning ("br_prepend_prefix never used") */
	if (0) br_prepend_prefix (NULL, NULL);

	free (tmp);
	return newpath;
}

#endif
