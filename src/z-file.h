#ifndef __angband_z_file__
#define __angband_z_file__

/*** Various system-specific fixes ***/

/*
 * Use POSIX file control where we can, otherwise help out other platforms
 */
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#else
# define O_RDONLY   0
# define O_WRONLY   1
# define O_RDWR     2
#endif

/*
 * Several systems have no "O_BINARY" flag
 */
#ifndef O_BINARY
# define O_BINARY 0
#endif /* O_BINARY */

/*
 * Hack -- force definitions for pre-C89 compilers -- see fd_seek()
 */
#ifndef SEEK_SET
# define SEEK_SET   0
#endif
#ifndef SEEK_CUR
# define SEEK_CUR   1
#endif
#ifndef SEEK_END
# define SEEK_END   2
#endif

/*
 * Hack -- force definitions -- see fd_lock()  XXX
 */
#ifndef F_UNLCK
# define F_UNLCK    0
#endif
#ifndef F_RDLCK
# define F_RDLCK    1
#endif
#ifndef F_WRLCK
# define F_WRLCK    2
#endif


/*** Functions provided in the package ***/

extern errr path_parse(char *buf, size_t max, const char* file);
extern errr path_build(char *buf, size_t max, const char* path, const char* file);
extern FILE *my_fopen(const char* file, const char* mode);
extern FILE *my_fopen_temp(char *buf, size_t max);
extern errr my_fclose(FILE *fff);
extern errr my_fgets(FILE *fff, char *buf, size_t n);
extern errr my_fputs(FILE *fff, const char* buf, size_t n);
extern bool my_fexists(const char *fname);
extern errr fd_kill(const char* file);
extern errr fd_move(const char* file, const char* what);
extern int fd_make(const char* file, int mode);
extern int fd_open(const char* file, int flags);
extern errr fd_lock(int fd, int what);
extern errr fd_seek(int fd, long n);
extern errr fd_read(int fd, char *buf, size_t n);
extern errr fd_write(int fd, const char* buf, size_t n);
extern errr fd_close(int fd);
extern errr check_modification_date(int fd, const char* template_file);


#endif
