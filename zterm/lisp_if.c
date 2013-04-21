#include <stdio.h>
#include "angband.h"
#include "langband.h"

typedef unsigned long cmucl_lispobj;
extern cmucl_lispobj funcall0(cmucl_lispobj function);
static cmucl_lispobj cmucl_callback_fun = 0;
static void set_cmucl_callback(cmucl_lispobj fun);
static int (*acl_callback_fun)() = 0;
static void set_acl_callback(int (*fun)());

void
set_lisp_system(LISP_SYSTEMS val) {

    if (val == LISPSYS_CMUCL || val == LISPSYS_ACL) {
	current_lisp_system = val;
    }
    else if (val == LISPSYS_CLISP || val == LISPSYS_LISPWORKS ||
	     val == LISPSYS_SBCL || val == LISPSYS_CORMAN) {
	current_lisp_system = val;
	lisp_will_use_callback = 0;
    }
    else {
	fprintf(stderr,"Unknown lisp-system given: %d.\n", val);
    }
    
}

void
set_lisp_callback (void *ptr) {

    if (current_lisp_system == LISPSYS_CMUCL) {
	set_cmucl_callback((cmucl_lispobj)ptr);
    }
    else if (current_lisp_system == LISPSYS_ACL) {
	set_acl_callback(ptr);
    }
    else {
	fprintf(stderr,"Don't know how to set callback for lisp-system %d.\n", current_lisp_system);
    }
}


void
set_acl_callback(int (*fun)()) {
//    printf("Setting cb to %p\n", fun);
    lisp_will_use_callback = 1;
    acl_callback_fun = fun;
}


void
set_cmucl_callback(cmucl_lispobj fun) {
//    printf("Setting cb to %uld\n", fun);
    lisp_will_use_callback = 1;
    cmucl_callback_fun = fun;
}

void
play_game_lisp() {

    if (lisp_will_use_callback) {
/*	fprintf(stderr,"Note: playing lisp-game through callback from C\n"); */
	
	if (current_lisp_system == LISPSYS_CMUCL && cmucl_callback_fun) {
#ifndef WIN32
	    funcall0(cmucl_callback_fun);
#endif
	}
	else if (current_lisp_system == LISPSYS_ACL && acl_callback_fun) {
	    (*acl_callback_fun)();
	}
	else {
	    fprintf(stderr,"Unable to handle callback for system %d..\n", current_lisp_system);
	}
    }
    else {
	fprintf(stderr,"Tried to play by callback, but lisp-system %d doesn't want callbacking.\n",
		current_lisp_system);
    }
    
}

