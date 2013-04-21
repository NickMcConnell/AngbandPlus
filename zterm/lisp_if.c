#include <stdio.h>
#include "angband.h"
#include "langband.h"

typedef unsigned long cmucl_lispobj;
extern cmucl_lispobj funcall0(cmucl_lispobj function);
extern cmucl_lispobj funcall2(cmucl_lispobj function,
			      cmucl_lispobj first_arg,
			      cmucl_lispobj second_arg);
extern cmucl_lispobj funcall3(cmucl_lispobj function,
			      cmucl_lispobj first_arg,
			      cmucl_lispobj second_arg,
			      cmucl_lispobj third_arg);


static void set_cmucl_callback(char *name, cmucl_lispobj fun);
static void set_sbcl_callback(char *name, cmucl_lispobj fun);
static void set_acl_callback(char *name, int (*fun)());
static void set_lispworks_callback(char *name, int (*fun)());

static cmucl_lispobj cmucl_callback_play = 0;
static cmucl_lispobj cmucl_callback_resize = 0;
static cmucl_lispobj cmucl_callback_mouseclick = 0;

static cmucl_lispobj sbcl_callback_play = 0;
static cmucl_lispobj sbcl_callback_resize = 0;
static cmucl_lispobj sbcl_callback_mouseclick = 0;

static int (*acl_callback_play)() = 0;
static int (*acl_callback_resize)(int,int) = 0;
static int (*acl_callback_mouseclick)(int,int,int) = 0;

static int (*lispworks_callback_play)() = 0;
static int (*lispworks_callback_resize)(int,int) = 0;
static int (*lispworks_callback_mouseclick)(int,int,int) = 0;


void
set_lisp_system(LISP_SYSTEMS val) {

    if (val == LISPSYS_CMUCL || val == LISPSYS_ACL || val == LISPSYS_SBCL || val == LISPSYS_LISPWORKS) {
	current_lisp_system = val;
    }
    else if (val == LISPSYS_CLISP || val == LISPSYS_CORMAN) {
	current_lisp_system = val;
	lisp_will_use_callback = 0;
    }
    else {
	fprintf(stderr,"Unknown lisp-system given: %d.\n", val);
    }
    
}

void
set_lisp_callback (char *name, void *ptr) {

//    fprintf(stderr,"callback %s %p\n", name, ptr);
    if (current_lisp_system == LISPSYS_CMUCL) {
	set_cmucl_callback(name, (cmucl_lispobj)ptr);
    }
    else if (current_lisp_system == LISPSYS_ACL) {
	set_acl_callback(name, ptr);
    }
    else if (current_lisp_system == LISPSYS_SBCL) {
	set_sbcl_callback(name, (cmucl_lispobj)ptr);
    }
    else if (current_lisp_system == LISPSYS_LISPWORKS) {
	set_lispworks_callback(name, ptr);
    }
    else {
	fprintf(stderr,"Don't know how to set callback '%s' for lisp-system %d.\n", name, current_lisp_system);
    }
}


void
set_acl_callback(char *name, int (*fun)()) {
//    printf("Setting cb to %p\n", fun);
    if (name != NULL && strlen(name)> 0) {
	if (!strcmp(name, "play-game")) {
	    lisp_will_use_callback = 1;
	    acl_callback_play = fun;
	}
	else if (!strcmp(name, "adjust-size")) {
	    acl_callback_resize = fun;
	}
	else if (!strcmp(name, "mouse-clicked")) {
	    acl_callback_mouseclick = fun;
	}
	else {
	    fprintf(stderr, "Unknown callback '%s'\n", name);
	}
    }
}

void
set_lispworks_callback(char *name, int (*fun)()) {
//    printf("Setting cb to %p\n", fun);
    if (name != NULL && strlen(name)> 0) {
	if (!strcmp(name, "play-game")) {
	    lisp_will_use_callback = 1;
	    lispworks_callback_play = fun;
	}
	else if (!strcmp(name, "adjust-size")) {
	    lispworks_callback_resize = fun;
	}
	else if (!strcmp(name, "mouse-clicked")) {
	    lispworks_callback_mouseclick = fun;
	}
	else {
	    fprintf(stderr, "Unknown callback '%s'\n", name);
	}
    }
}


void
set_cmucl_callback(char *name, cmucl_lispobj fun) {
//    printf("Setting cb to %uld\n", fun);
    if (name != NULL && strlen(name)> 0) {
	if (!strcmp(name, "play-game")) {
	    lisp_will_use_callback = 1;
	    cmucl_callback_play = fun;
	    }
	else if (!strcmp(name, "adjust-size")) {
	    cmucl_callback_resize = fun;
	}
	else if (!strcmp(name, "mouse-clicked")) {
	    cmucl_callback_mouseclick = fun;
	}
	else {
	    fprintf(stderr, "Unknown callback '%s'\n", name);
	}
    }
}

void
set_sbcl_callback(char *name, cmucl_lispobj fun) {
//    printf("Setting cb to %uld\n", fun);
    if (name != NULL && strlen(name)> 0) {
	if (!strcmp(name, "play-game")) {
	    lisp_will_use_callback = 1;
	    sbcl_callback_play = fun;
	    }
	else if (!strcmp(name, "adjust-size")) {
	    sbcl_callback_resize = fun;
	}
	else if (!strcmp(name, "mouse-clicked")) {
	    sbcl_callback_mouseclick = fun;
	}
	else {
	    fprintf(stderr, "Unknown callback '%s'\n", name);
	}
    }
}


void
play_game_lisp() {

    if (lisp_will_use_callback) {
/*	fprintf(stderr,"Note: playing lisp-game through callback from C\n"); */
	
	if (current_lisp_system == LISPSYS_CMUCL && cmucl_callback_play) {
#ifndef WIN32
	    funcall0(cmucl_callback_play);
#endif
	}
	
	else if (current_lisp_system == LISPSYS_SBCL && sbcl_callback_play) {
//	    fprintf(stderr,"Trying to phone home with %lu\n", sbcl_callback_play);
#ifndef WIN32
	    funcall0(sbcl_callback_play);
#endif
//	    fprintf(stderr,"Returned\n");
	}

	else if (current_lisp_system == LISPSYS_ACL && acl_callback_play) {
	    (*acl_callback_play)();
	}

	else if (current_lisp_system == LISPSYS_LISPWORKS && lispworks_callback_play) {
	    (*lispworks_callback_play)();
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

#define make_fixnum(n) ((cmucl_lispobj)((n)<<2))

void
readjust_screen_lisp(int width, int height) {

    if (lisp_will_use_callback) {
//	fprintf(stderr,"Note: calling resize on lisp-side\n"); 
	
	if (current_lisp_system == LISPSYS_CMUCL && cmucl_callback_resize) {
#ifndef WIN32
	    funcall2(cmucl_callback_resize, make_fixnum(width), make_fixnum(height));
#endif
	}
	
	else if (current_lisp_system == LISPSYS_SBCL && sbcl_callback_resize) {
#ifndef WIN32
	    funcall2(sbcl_callback_resize, make_fixnum(width), make_fixnum(height));
#endif
	}

	else if (current_lisp_system == LISPSYS_ACL && acl_callback_resize) {
	    (*acl_callback_resize)(width, height);
	}

	else if (current_lisp_system == LISPSYS_LISPWORKS && lispworks_callback_resize) {
	    (*lispworks_callback_resize)(width, height);
	}

	else {
	    fprintf(stderr,"Unable to handle resize-callback for system %d..\n", current_lisp_system);
	}
    }
    else {
//	fprintf(stderr,"Tried to resize by callback, but lisp-system %d doesn't want callbacking.\n",
//		current_lisp_system);
    }
 

}

void
mouse_clicked(int button, int x, int y) {

    if (lisp_will_use_callback) {

	if (current_lisp_system == LISPSYS_CMUCL && cmucl_callback_mouseclick) {
#ifndef WIN32
	    funcall3(cmucl_callback_mouseclick, make_fixnum(button), make_fixnum(x), make_fixnum(y));
#endif
	}
	
	else if (current_lisp_system == LISPSYS_SBCL && sbcl_callback_mouseclick) {
#ifndef WIN32
	    funcall3(sbcl_callback_mouseclick, make_fixnum(button), make_fixnum(x), make_fixnum(y));
#endif
	}

	else if (current_lisp_system == LISPSYS_ACL && acl_callback_mouseclick) {
	    (*acl_callback_mouseclick)(button, x, y);
	}

	else if (current_lisp_system == LISPSYS_LISPWORKS && lispworks_callback_mouseclick) {
	    (*lispworks_callback_mouseclick)(button, x, y);
	}

	else {
	    fprintf(stderr,"Unable to handle mouseclick-callback for system %d..\n", current_lisp_system);
	}
    }
    else {
//	fprintf(stderr,"Tried to resize by callback, but lisp-system %d doesn't want callbacking.\n",
//		current_lisp_system);
    }
 
    

}
