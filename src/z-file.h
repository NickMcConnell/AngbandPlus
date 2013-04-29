#ifndef INCLUDED_Z_FILE_H
#define INCLUDED_Z_FILE_H

/*** Various system-specific fixes ***/

/*
 * Use POSIX file control where we can, otherwise help out other platforms
 */
#if (defined (HAVE_FCNTL_H) || defined (WINDOWS)) && !defined(_WIN32_WCE)
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

extern errr path_parse(char *buf, size_t max, cptr file);
extern errr path_build(char *buf, size_t max, cptr path, cptr file);
extern FILE *my_fopen(cptr file, cptr mode);
extern FILE *my_fopen_temp(char *buf, size_t max);
extern errr my_fclose(FILE *fff);
extern errr my_fgets(FILE *fff, char *buf, size_t n);
extern errr my_fputs(FILE *fff, cptr buf, size_t n);
extern bool my_fexists(const char *fname);
extern errr fd_kill(cptr file);
extern errr fd_move(cptr file, cptr what);
extern int fd_make(cptr file, int mode);
extern int fd_open(cptr file, int flags);
extern errr fd_lock(int fd, int what);
extern errr fd_seek(int fd, long n);
extern errr fd_read(int fd, char *buf, size_t n);
extern errr fd_write(int fd, cptr buf, size_t n);
extern errr fd_close(int fd);
extern errr check_modification_date(int fd, cptr template_file);
extern void x_fprintf(FILE *fff, int encoding, cptr fmt, ...);

/*** File access code ***/

/** Data types **/

/*
 * An opaque file handle for Angband file handling.
 */
typedef struct ang_file ang_file;

/*
 * Specifies what kind of access is required to a file.  See file_open().
 */
typedef enum
{
	MODE_WRITE,
	MODE_READ,
	MODE_APPEND
} file_mode;

/*
 * Specified what kind of thing a file is, when writing.  See file_open().
 */
typedef enum
{
	FTYPE_TEXT = 1,
	FTYPE_SAVE,
	FTYPE_RAW,
	FTYPE_HTML
} file_type;


/** Utility functions **/

/*
 * Returns TRUE if `fname` exists (and is a file), FALSE otherwise.
 */
bool file_exists(const char *fname);

/*
 * Tries to delete `fname`.
 *
 * Returns TRUE if successful, FALSE otherwise.
 */
bool file_delete(const char *fname);

/*
 * Moves the file `fname` to `newname`.
 *
 * Returns TRUE if successful, FALSE otherwise.
 */
bool file_move(const char *fname, const char *newname);

/*
 * Returns TRUE if the file `first` is newer than `second`.
 */
bool file_newer(const char *first, const char *second);


/** File handle creation **/

/*
 * Open file `buf`, returning a a file handling representing that file.
 *
 * The file mode specifies what kind of access is required to the file:
 *  - MODE_WRITE will overwrite the current contents of the file
 *  - MODE_READ will allow read-only access to the file
 *  - MODE_APPEND will allow write-only access, but will not overwrite the
 *    current contents of the file.
 *
 * The file type is specified to allow systems which don't use file extensions
 * to set the type of the file appropriately.  When reading, pass -1 as ftype;
 * when writing, use whichever filetype seems most appropriate.
 *
 * On any kind of error, this function returns NULL.
 */
ang_file *file_open(const char *buf, file_mode mode, file_type ftype);

/*
 * Attempt to close the file handle `f`.
 *
 * Returns TRUE if successful, FALSE otherwise.
 */
bool file_close(ang_file *f);


/** File locking **/

/*
 * Lock or unlock the file represented by `f` for writing.
 * If the file is not open for writing, this call will fail.
 *
 * If `f` is closed, the file is automatically unlocked.
 */
void file_lock(ang_file *f);
void file_unlock(ang_file *f);


/** Line-based IO **/

/*
 * Get a line of text from the file represented by `f`, placing it into `buf`
 * to a maximum length of `n`.
 *
 * This expands tabs, replaces non-printables with '?', and deals with differing
 * line endings.
 *
 * Returns TRUE when data is returned; FALSE otherwise.
 */
bool file_getl(ang_file *f, char *buf, size_t n);

/*
 * Write the string pointed to by `buf` to the file represented by `f`.
 *
 * Returns TRUE if successful, FALSE otherwise.
 */
bool file_put(ang_file *f, const char *buf);

/*
 * Format (using strnfmt) the given args, and then call file_put().
 */
bool file_putf(ang_file *f, const char *fmt, ...);


/** Byte-based IO */

/*
 * Seek to position `pos` in the file represented by `f`.
 *
 * Returns TRUE if successful, FALSE otherwise. 
 */
bool file_seek(ang_file *f, u32b pos);

/*
 * Reads `n` bytes from the file represented by `f` into the buffer `buf`.
 * Do not mix with calls to file_readc().
 *
 * Returns the number of bytes read. 
 */
size_t file_read(ang_file *f, char *buf, size_t n);

/*
 * Write the first `n` bytes following the pointer `buf` to the file represented
 * by `f`.  Do not mix with calls to file_writec().
 *
 * Returns TRUE if successful, FALSE otherwise.
 */
bool file_write(ang_file *f, const char *buf, size_t n);

/*
 * Read a byte from the file represented by `f` and place it at the location
 * specified by 'b'.
 *
 * Returns TRUE if successful, FALSE otherwise.
 */
bool file_readc(ang_file *f, byte *b);

/*
 * Write the byte `b` to the file represented by `f`.
 *
 * Returns TRUE if successful, FALSE otherwise.
 */
bool file_writec(ang_file *f, byte b);



typedef struct ang_dir ang_dir;

ang_dir *my_dopen(const char *dirname);
bool my_dread(ang_dir *dir, char *fname, size_t len);
void my_dclose(ang_dir *dir);


#endif
