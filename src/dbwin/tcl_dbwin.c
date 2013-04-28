#include <string.h>
#include <tcl.h>
#include "dbwin.h"

DLLEXPORT int Dbwin_Init _ANSI_ARGS_((Tcl_Interp *interp));

static int Dbwin_ObjCmd(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	char *t, buf[500];
	
    if (objc < 2)
    {
		Tcl_WrongNumArgs(interp, 1, objv, "string");
		return TCL_ERROR;
    }

	t = Tcl_GetStringFromObj(objv[1], NULL);
	t = strncpy(buf, t, 499);
	t[499] = '\0';
	
	dbwin("%s", t);

	return TCL_OK;
}

int Dbwin_Init(Tcl_Interp *interp)
{
#ifdef USE_TCL_STUBS

	if (Tcl_InitStubs(interp, "8.0", 0) == NULL)
	{
		return TCL_ERROR;
	}

#endif /* USE_TCL_STUBS */

    Tcl_CreateObjCommand(interp, "dbwin", Dbwin_ObjCmd, NULL, NULL);

    return Tcl_PkgProvide(interp, "dbwin", "1.0");
}
