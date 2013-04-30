#ifndef MACH_O_CARBON

# define FILE_TYPE(X) ((void)0)

#else 

/* The rest is OS X specific */


#include <unistd.h>
#include <sys/stat.h>
#include <pwd.h>

#define HAVE_USLEEP

extern u32b _fcreator;
extern u32b _ftype;

# define FILE_TYPE_TEXT 'TEXT'
# define FILE_TYPE_DATA 'DATA'
# define FILE_TYPE_SAVE 'SAVE'
# define FILE_TYPE(X) (_ftype = (X))


#endif /* MACH_O_CARBON */

