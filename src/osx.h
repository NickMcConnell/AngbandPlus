#ifdef MACH_O_CARBON
#include <unistd.h>
#include <sys/stat.h>
#include <pwd.h>

#define HAVE_USLEEP
# define FILE_TYPE_TEXT 'TEXT'
# define FILE_TYPE_DATA 'DATA'
# define FILE_TYPE_SAVE 'SAVE'
# define FILE_TYPE(X) (_ftype = (X))


void fsetfileinfo(cptr pathname, u32b fcreator, u32b ftype);

extern u32b _ftype;
extern u32b _fcreator;

#else
# define FILE_TYPE(X) ((void)0)
#endif /* MACH_O_CARBON */

