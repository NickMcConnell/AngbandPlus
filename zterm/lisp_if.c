#include <stdio.h>
#include "langband.h"
#include "lbtools.h"

typedef unsigned long cmucl_lispobj;
extern cmucl_lispobj funcall0(cmucl_lispobj function);
extern cmucl_lispobj funcall2(cmucl_lispobj function,
			      cmucl_lispobj first_arg,
			      cmucl_lispobj second_arg);
extern cmucl_lispobj funcall3(cmucl_lispobj function,
			      cmucl_lispobj first_arg,
			      cmucl_lispobj second_arg,
			      cmucl_lispobj third_arg);


static void lbui_set_cmucl_callback(char *name, cmucl_lispobj fun);
static void lbui_set_sbcl_callback(char *name, cmucl_lispobj fun);
static void lbui_set_acl_callback(char *name, int (*fun)());
static void lbui_set_lispworks_callback(char *name, int (*fun)());

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
lbui_set_lisp_system(LISP_SYSTEMS val) {

    //DBGPUT("Lisp system %d vs %d.\n", val, LISPSYS_ACL);
    
    if (val == LISPSYS_CMUCL || val == LISPSYS_ACL || val == LISPSYS_SBCL || val == LISPSYS_LISPWORKS) {
	lbui_current_lisp_system = val;
    }
    else if (val == LISPSYS_CLISP || val == LISPSYS_CORMAN) {
	lbui_current_lisp_system = val;
	lbui_will_use_callback = 0;
    }
    else {
	ERRORMSG("Unknown lisp-system given: %d.\n", val);
    }
    
}

int
lbui_cleanup_callbacks() {

    //DBGPUT("Cleaning callbacks\n");
    
    cmucl_callback_play = 0;
    cmucl_callback_resize = 0;
    cmucl_callback_mouseclick = 0;
    
    sbcl_callback_play = 0;
    sbcl_callback_resize = 0;
    sbcl_callback_mouseclick = 0;
    
    acl_callback_play = 0;
    acl_callback_resize = 0;
    acl_callback_mouseclick = 0;
    
    lispworks_callback_play = 0;
    lispworks_callback_resize = 0;
    lispworks_callback_mouseclick = 0;

    return 0;
}

void
lbui_set_lisp_callback (char *name, void *ptr) {

    //DBGPUT("callback %s %p\n", name, ptr);
    if (lbui_current_lisp_system == LISPSYS_CMUCL) {
	lbui_set_cmucl_callback(name, (cmucl_lispobj)ptr);
    }
    else if (lbui_current_lisp_system == LISPSYS_ACL) {
	lbui_set_acl_callback(name, ptr);
    }
    else if (lbui_current_lisp_system == LISPSYS_SBCL) {
	lbui_set_sbcl_callback(name, (cmucl_lispobj)ptr);
    }
    else if (lbui_current_lisp_system == LISPSYS_LISPWORKS) {
	lbui_set_lispworks_callback(name, ptr);
    }
    else {
	ERRORMSG("Don't know how to set callback '%s' for lisp-system %d.\n", name, lbui_current_lisp_system);
    }
}


void
lbui_set_acl_callback(char *name, int (*fun)()) {
//    printf("Setting cb to %p\n", fun);
    if (name != NULL && strlen(name)> 0) {
	if (!strcmp(name, "play-game")) {
	    lbui_will_use_callback = 1;
	    acl_callback_play = fun;
	}
	else if (!strcmp(name, "adjust-size")) {
	    acl_callback_resize = fun;
	}
	else if (!strcmp(name, "mouse-clicked")) {
	    acl_callback_mouseclick = fun;
	}
	else {
	    ERRORMSG("Unknown callback '%s'\n", name);
	}
    }
}

void
lbui_set_lispworks_callback(char *name, int (*fun)()) {
//    printf("Setting cb to %p\n", fun);
    if (name != NULL && strlen(name)> 0) {
	if (!strcmp(name, "play-game")) {
	    lbui_will_use_callback = 1;
	    lispworks_callback_play = fun;
	}
	else if (!strcmp(name, "adjust-size")) {
	    lispworks_callback_resize = fun;
	}
	else if (!strcmp(name, "mouse-clicked")) {
	    lispworks_callback_mouseclick = fun;
	}
	else {
	    ERRORMSG("Unknown callback '%s'\n", name);
	}
    }
}


void
lbui_set_cmucl_callback(char *name, cmucl_lispobj fun) {
//    printf("Setting cb to %uld\n", fun);
    if (name != NULL && strlen(name)> 0) {
	if (!strcmp(name, "play-game")) {
	    lbui_will_use_callback = 1;
	    cmucl_callback_play = fun;
	    }
	else if (!strcmp(name, "adjust-size")) {
	    cmucl_callback_resize = fun;
	}
	else if (!strcmp(name, "mouse-clicked")) {
	    cmucl_callback_mouseclick = fun;
	}
	else {
	    ERRORMSG("Unknown callback '%s'\n", name);
	}
    }
}

void
lbui_set_sbcl_callback(char *name, cmucl_lispobj fun) {
    //printf("Setting cb to %lu\n", fun);
    if (name != NULL && strlen(name)> 0) {
	if (!strcmp(name, "play-game")) {
	    lbui_will_use_callback = 1;
	    sbcl_callback_play = fun;
	    }
	else if (!strcmp(name, "adjust-size")) {
	    sbcl_callback_resize = fun;
	}
	else if (!strcmp(name, "mouse-clicked")) {
	    sbcl_callback_mouseclick = fun;
	}
	else {
	    ERRORMSG("Unknown callback '%s'\n", name);
	}
    }
}


int
lbui_play_game_lisp() {

    if (lbui_will_use_callback) {
	// DBGPUT("Note: playing lisp-game through callback from C\n");
	
	if (lbui_current_lisp_system == LISPSYS_CMUCL && cmucl_callback_play) {
#ifndef WIN32
	    funcall0(cmucl_callback_play);
#endif
	}
	
	else if (lbui_current_lisp_system == LISPSYS_SBCL && sbcl_callback_play) {
	    // DBGPUT("Trying to phone home with %lu\n", sbcl_callback_play);
#ifndef WIN32
	    funcall0(sbcl_callback_play);
#endif
	    // DBGPUT("Returned\n");
	}

	else if (lbui_current_lisp_system == LISPSYS_ACL && acl_callback_play) {
	    (*acl_callback_play)();
	}

	else if (lbui_current_lisp_system == LISPSYS_LISPWORKS && lispworks_callback_play) {
	    (*lispworks_callback_play)();
	}

	else {
	    ERRORMSG("Unable to handle callback for system %d..\n", lbui_current_lisp_system);
	    return -5;
	}
    }
    else {
	ERRORMSG("Tried to play by callback, but lisp-system %d doesn't want callbacking.\n",
		 lbui_current_lisp_system);
	return -6;
    }

    return 0;
}

#define make_fixnum(n) ((cmucl_lispobj)((n)<<2))

void
lbui_readjust_screen_lisp(int width, int height) {

    if (lbui_will_use_callback) {
	// DBGPUT("Note: calling resize on lisp-side\n"); 
	
	if (lbui_current_lisp_system == LISPSYS_CMUCL && cmucl_callback_resize) {
#ifndef WIN32
	    funcall2(cmucl_callback_resize, make_fixnum(width), make_fixnum(height));
#endif
	}
	
	else if (lbui_current_lisp_system == LISPSYS_SBCL && sbcl_callback_resize) {
#ifndef WIN32
	    funcall2(sbcl_callback_resize, make_fixnum(width), make_fixnum(height));
#endif
	}

	else if (lbui_current_lisp_system == LISPSYS_ACL && acl_callback_resize) {
	    (*acl_callback_resize)(width, height);
	}

	else if (lbui_current_lisp_system == LISPSYS_LISPWORKS && lispworks_callback_resize) {
	    (*lispworks_callback_resize)(width, height);
	}

	else {
	    ERRORMSG("Unable to handle resize-callback for system %d..\n", lbui_current_lisp_system);
	}
    }
    else {
//	ERRORMSG("Tried to resize by callback, but lisp-system %d doesn't want callbacking.\n",
//		lbui_current_lisp_system);
    }
 

}

void
lbui_mouse_clicked(int button, int x, int y) {

    if (lbui_will_use_callback) {

	if (lbui_current_lisp_system == LISPSYS_CMUCL && cmucl_callback_mouseclick) {
#ifndef WIN32
	    funcall3(cmucl_callback_mouseclick, make_fixnum(button), make_fixnum(x), make_fixnum(y));
#endif
	}
	
	else if (lbui_current_lisp_system == LISPSYS_SBCL && sbcl_callback_mouseclick) {
#ifndef WIN32
	    funcall3(sbcl_callback_mouseclick, make_fixnum(button), make_fixnum(x), make_fixnum(y));
#endif
	}

	else if (lbui_current_lisp_system == LISPSYS_ACL && acl_callback_mouseclick) {
	    (*acl_callback_mouseclick)(button, x, y);
	}

	else if (lbui_current_lisp_system == LISPSYS_LISPWORKS && lispworks_callback_mouseclick) {
	    (*lispworks_callback_mouseclick)(button, x, y);
	}

	else {
	    ERRORMSG("Unable to handle mouseclick-callback for system %d..\n", lbui_current_lisp_system);
	}
    }
    else {
	// INFOMSG("Tried to resize by callback, but lisp-system %d doesn't want callbacking.\n",
	//         lbui_current_lisp_system);
    }
 
    

}
