
#ifdef SMALL_BOYS_FOR_BREAKFAST
/*
 * Hack -- Explain a broken "lib" folder and quit (see below).
 *
 * XXX XXX XXX This function is "messy" because various things
 * may or may not be initialized, but the "plog()" and "quit()"
 * functions are "supposed" to work under any conditions.
 */
static void init_angband_aux(cptr why)
{
    /* Why */
    plog(why);

        /* Explain */
    plog("The 'lib' directory is probably missing or broken.");

    /* More details */
    plog("Perhaps the archive was not extracted correctly.");

    /* Explain */
    plog("See the 'README' file for more information.");

    /* Quit with error */
    quit("Fatal Error.");
}
#endif


int
test_calling(const char *arr, const char *alt) {

    if (arr == NULL) {
//	fprintf(stderr, "Arr is NULL\n");
    }
    if (alt == NULL) {
//	fprintf(stderr, "Alt is NULL\n");
    }
    if (alt == NULL && arr == NULL) {
	return 0;
    }

    if (arr && alt) {
//	fprintf(stderr, "Have two strings\n");
	if (arr == alt) {
//	    fprintf(stderr, "Strings are eq: [%s]\n", alt);
	    return 0;
	}
	else if (!strcmp(arr,alt)) {
//	    fprintf(stderr, "Strings are equal: [%s]\n", alt);
	    return 0;
	}
	else if (!strcasecmp(arr,alt)) {
//	    fprintf(stderr, "Strings are equalp, and [%s] [%s]\n", arr, alt);
	    return 1;
	}
	else {
//	    fprintf(stderr, "Strings aren't even equalp: [%s] vs [%s]\n", arr, alt);
	    return -1;
	}
    }
    
    return -1;
    
}


int
test_calling_2(const char *arr) {

    if (arr) {
	int len = strlen(arr);
	if (len > 10) {
	    printf("My function got |%s|\n", arr);
	}
	return len;
    }
    else {
	return -1;
    }
}
