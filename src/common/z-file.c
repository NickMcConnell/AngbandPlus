/*
 * File: z-file.c
 * Purpose: Low-level file (and directory) handling
 *
 * Copyright (c) 1997-2007 Ben Harrison, pelpel, Andrew Sidwell, Matthew Jones
 * Copyright (c) 2012 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


#include "angband.h"
#include <sys/stat.h>


/*
 * Hack -- Fake declarations from "dos.h" XXX XXX XXX
 */
#define INVALID_FILE_NAME (DWORD)0xFFFFFFFF


/*
 * Apply special system-specific processing before dealing with a filename.
 */
static void path_parse(char *buf, size_t max, const char *file)
{
    /* Accept the filename */
    my_strcpy(buf, file, max);
}


static void path_process(char *buf, size_t len, size_t *cur_len, const char *path)
{
    strnfcat(buf, len, cur_len, "%s", path);
}


/*
 * Create a new path string by appending a 'leaf' to 'base'.
 *
 * Remember to free the return value.
 */
size_t path_build(char *buf, size_t len, const char *base, const char *leaf)
{
    size_t cur_len = 0;
    buf[0] = '\0';

    if (!leaf || !leaf[0])
    {
        if (base && base[0]) path_process(buf, len, &cur_len, base);

        return cur_len;
    }

    /*
     * If the leafname starts with the separator or there's no base path,
     * we use the leafname only.
     */
    if ((!base || !base[0]) || prefix(leaf, PATH_SEP))
    {
        path_process(buf, len, &cur_len, leaf);
        return cur_len;
    }

    /* There is both a relative leafname and a base path from which it is relative */
    path_process(buf, len, &cur_len, base);
    strnfcat(buf, len, &cur_len, "%s", PATH_SEP);
    path_process(buf, len, &cur_len, leaf);

    return cur_len;
}


/*** File-handling API ***/


/* Some defines for compatibility between various build platforms */
#ifndef S_IRUSR
#define S_IRUSR S_IREAD
#endif

#ifndef S_IWUSR
#define S_IWUSR S_IWRITE
#endif


#ifndef O_BINARY
#define O_BINARY 0
#endif


/* Avoid a compiler warning when cross compiling for windows */
#ifdef __STRICT_ANSI__
FILE *fdopen(int handle, const char *mode);
#endif


/* Private structure to hold file pointers and useful info. */
struct ang_file
{
    FILE *fh;
    char *fname;
    file_mode mode;
};


/** Utility functions **/


/*
 * Delete file 'fname'
 */
bool file_delete(const char *fname)
{
    char buf[MSG_LEN];

    /* Get the system-specific paths */
    path_parse(buf, sizeof(buf), fname);

    return (remove(buf) == 0);
}


/*
 * Move file 'fname' to 'newname'
 */
bool file_move(const char *fname, const char *newname)
{
    char buf[MSG_LEN];
    char aux[MSG_LEN];

    /* Get the system-specific paths */
    path_parse(buf, sizeof(buf), fname);
    path_parse(aux, sizeof(aux), newname);

    return (rename(buf, aux) == 0);
}


/*
 * Decide whether a file exists or not
 */
bool file_exists(const char *fname)
{
    char path[MAX_PATH];
    DWORD attrib;

    /* API says we mustn't pass anything larger than MAX_PATH */
    my_strcpy(path, fname, sizeof(path));

    attrib = GetFileAttributes(path);
    if (attrib == INVALID_FILE_NAME) return FALSE;
    if (attrib & FILE_ATTRIBUTE_DIRECTORY) return FALSE;

    return TRUE;
}


/*
 * Return TRUE if first is newer than second, FALSE otherwise.
 */
bool file_newer(const char *first, const char *second)
{
    struct stat stat1, stat2;

    /* Remove W8080 warning: _fstat is declared but never used */
    _fstat(0, NULL);

    /* If the first doesn't exist, the first is not newer. */
    if (stat(first, &stat1) != 0) return FALSE;

    /* If the second doesn't exist, the first is always newer. */
    if (stat(second, &stat2) != 0) return TRUE;

    /* Compare modification times. */
    return stat1.st_mtime > stat2.st_mtime ? TRUE : FALSE;
}


/** File-handle functions **/


/* Mode: write (MODE_WRITE), read (MODE_READ), append (MODE_APPEND) */
static char modechar[4] = "wra";


/* Type: text (FTYPE_TEXT), binary (FTYPE_RAW + FTYPE_SAVE) */
static char typechar[3] = "tbb";


/*
 * Open file 'fname', in mode 'mode', with filetype 'ftype'.
 * Returns file handle or NULL.
 */
ang_file *file_open(const char *fname, file_mode mode, file_type ftype)
{
    ang_file *f = ZNEW(ang_file);
    char modestr[3] = "__";
    char buf[MSG_LEN];

    /* Get the system-specific path */
    path_parse(buf, sizeof(buf), fname);

    modestr[0] = modechar[mode];
    modestr[1] = typechar[ftype];

    if (ftype == FTYPE_SAVE)
    {
        int fd;

        /* Open only if the file does not exist */
        fd = open(buf, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, S_IRUSR | S_IWUSR);
        if (fd < 0)
        {
            /* There was some error */
            f->fh = NULL;
        }
        else
            f->fh = fdopen(fd, "wb");
    }
    else
        f->fh = fopen(buf, modestr);

    if (f->fh == NULL)
    {
        mem_free(f);
        return NULL;
    }

    f->fname = string_make(buf);
    f->mode = mode;

    return f;
}


/*
 * Close file handle 'f'
 */
bool file_close(ang_file *f)
{
    if (fclose(f->fh) != 0) return FALSE;

    string_free(f->fname);
    mem_free(f);

    return TRUE;
}


/*
 * Acquire a "temporary" file name if possible
 *
 * This filename is always in "system-specific" form.
 */
ang_file *file_temp(char *fname, size_t len)
{
    ang_file *fff;
    char prefix[] = "mng";

    /* Temporary file */
    if (!GetTempPath(len, fname)) return NULL;
    if (!GetTempFileName(fname, prefix, 0, fname)) return NULL;

    /* Open a new file */
    fff = file_open(fname, MODE_WRITE, FTYPE_TEXT);

    /* Paranoia */
    if (!fff)
    {
        plog_fmt("ERROR! %s (writing %s)", strerror(errno), fname);
        return NULL;
    }

    return fff;
}


/** Locking functions **/


/*
 * Lock a file on platforms where this is supported
 */
void file_lock(ang_file *f)
{
}


/*
 * Unlock a file on platforms where this is supported
 */
void file_unlock(ang_file *f)
{
}


/** Byte-based IO and functions **/


/*
 * Seek to location 'pos' in file 'f'
 */
bool file_seek(ang_file *f, u32b pos)
{
    return (fseek(f->fh, pos, SEEK_SET) == 0);
}


/*
 * Read a single, 8-bit character from file 'f'.
 */
bool file_readc(ang_file *f, byte *b)
{
    int i = fgetc(f->fh);

    if (i == EOF) return FALSE;

    *b = (byte)i;
    return TRUE;
}


/*
 * Write a single, 8-bit character 'b' to file 'f'.
 */
bool file_writec(ang_file *f, byte b)
{
    return file_write(f, (const char *)&b, 1);
}


/*
 * Read 'n' bytes from file 'f' into array 'buf'
 */
int file_read(ang_file *f, char *buf, size_t n)
{
    size_t read = fread(buf, 1, n, f->fh);

    if ((read == 0) && ferror(f->fh)) return -1;
    return read;
}


/*
 * Append 'n' bytes of array 'buf' to file 'f'
 */
bool file_write(ang_file *f, const char *buf, size_t n)
{
    return (fwrite(buf, 1, n, f->fh) == n);
}


/** Line-based IO **/


/*
 * Read a line of text from file 'f' into buffer 'buf' of size 'n' bytes.
 *
 * Support both \r\n and \n as line endings, but not the outdated \r that used
 * to be used on Macs. Replace \ts with ' '.
 */
#define TAB_COLUMNS 4
bool file_getl(ang_file *f, char *buf, size_t len)
{
    bool seen_cr = FALSE;
    byte b;
    size_t i = 0;

    /* Leave a byte for the terminating 0 */
    size_t max_len = len - 1;

    while (i < max_len)
    {
        char c;

        if (!file_readc(f, &b))
        {
            buf[i] = '\0';
            return ((i == 0)? FALSE: TRUE);
        }

        c = (char) b;

        if (c == '\r')
        {
            seen_cr = TRUE;
            continue;
        }

        if (seen_cr && (c != '\n'))
        {
            fseek(f->fh, -1, SEEK_CUR);
            buf[i] = '\0';
            return TRUE;
        }

        if (c == '\n')
        {
            buf[i] = '\0';
            return TRUE;
        }

        /* Expand tabs */
        if (c == '\t')
        {
            /* Next tab stop */
            size_t tabstop = ((i + TAB_COLUMNS) / TAB_COLUMNS) * TAB_COLUMNS;

            if (tabstop >= len) break;

            /* Convert to spaces */
            while (i < tabstop) buf[i++] = ' ';

            continue;
        }

        buf[i++] = c;
    }

    buf[i] = '\0';
    return TRUE;
}


/*
 * Append a line of text 'buf' to the end of file 'f', using system-dependent
 * line ending.
 */
bool file_put(ang_file *f, const char *buf)
{
    return file_write(f, buf, strlen(buf));
}


/*
 * Append a formatted line of text to the end of file 'f'.
 *
 * file_putf() is the ellipsis version. Most file output will call this
 * version. It calls file_vputf() to do the real work. It returns TRUE
 * if the write was successful and FALSE otherwise.
 */
bool file_putf(ang_file *f, const char *fmt, ...)
{
    va_list vp;
    bool status;

    if (!f) return FALSE;

    va_start(vp, fmt);
    status = file_vputf(f, fmt, vp);
    va_end(vp);

    return status;
}


/*
 * Append a formatted line of text to the end of file 'f'.
 *
 * file_vputf() is the va_list version. It returns TRUE if the write was
 * successful and FALSE otherwise.
 */
bool file_vputf(ang_file *f, const char *fmt, va_list vp)
{
    char buf[MSG_LEN];

    if (!f) return FALSE;

    vstrnfmt(buf, sizeof(buf), fmt, vp);
    return file_put(f, buf);
}


/*
 * Format and translate a string, then print it out to file.
 */
bool x_file_putf(ang_file *f, const char *fmt, ...)
{
    va_list vp;
    char buf[MSG_LEN];

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Format the args, save the length */
    vstrnfmt(buf, sizeof(buf), fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    return file_put(f, buf);
}


void file_flush(ang_file *f)
{
    fflush(f->fh);
}


long file_tell(ang_file *f)
{
    return ftell(f->fh);
}


void file_rewind(ang_file *f)
{
    rewind(f->fh);
}


/*
 * Return whether or not a directory exists
 */
bool dir_exists(const char *path)
{
    char dirpath[MAX_PATH];
    DWORD attrib;

    /* API says we mustn't pass anything larger than MAX_PATH */
    my_strcpy(dirpath, path, sizeof(dirpath));

    attrib = GetFileAttributes(dirpath);
    if (attrib == INVALID_FILE_NAME) return FALSE;
    if (attrib & FILE_ATTRIBUTE_DIRECTORY) return TRUE;

    return FALSE;
}


/*
 * Creates the given directory, creating intermediate directories if
 * needed and possible. Returns whether or not the directory was created
 * successfully.
 */
bool dir_create(const char *path)
{
    const char *ptr;
    char buf[MSG_LEN];

    /* If the directory already exists then we're done */
    if (dir_exists(path)) return TRUE;

    /* If we're on windows, we need to skip past the "C:" part. */
    if (isalpha(path[0]) && path[1] == ':') path += 2;

    /*
     * Iterate through the path looking for path segements. At each step,
     * create the path segment if it doesn't already exist.
     */
    for (ptr = path; *ptr; ptr++)
    {
        if (*ptr == PATH_SEPC)
        {
            /* Find the length of the parent path string */
            size_t len = (size_t)(ptr - path);

            /* Skip the initial slash */
            if (len == 0) continue;

            /* If this is a duplicate path separator, continue */
            if (*(ptr - 1) == PATH_SEPC) continue;

            /* We can't handle really big filenames */
            if (len - 1 > MSG_LEN) return FALSE;

            /* Create the parent path string, plus null-padding */
            my_strcpy(buf, path, len + 1);

            /* Skip if the parent exists */
            if (dir_exists(buf)) continue;

            /* The parent doesn't exist, so create it or fail */
            if (mkdir(buf) != 0) return FALSE;
        }
    }

    return (mkdir(path) == 0)? TRUE: FALSE;
}


/*** Directory scanning API ***/


/* System-specific struct */
struct ang_dir
{
    HANDLE h;
    char *first_file;
};


ang_dir *my_dopen(const char *dirname)
{
    WIN32_FIND_DATA fd;
    HANDLE h;
    ang_dir *dir;

    /* Try to open it */
    h = FindFirstFile(format("%s\\*", dirname), &fd);

    /* Abort */
    if (h == INVALID_HANDLE_VALUE) return NULL;

    /* Set up the handle */
    dir = ZNEW(ang_dir);
    dir->h = h;
    dir->first_file = string_make(fd.cFileName);

    /* Success */
    return dir;
}


bool my_dread(ang_dir *dir, char *fname, size_t len)
{
    WIN32_FIND_DATA fd;
    BOOL ok;

    /* Try the first file */
    if (dir->first_file)
    {
        /* Copy the string across, then free it */
        my_strcpy(fname, dir->first_file, len);
        string_free(dir->first_file);
        dir->first_file = NULL;

        /* Wild success */
        return TRUE;
    }

    /* Try the next file */
    while (1)
    {
        ok = FindNextFile(dir->h, &fd);
        if (!ok) return FALSE;

        /* Skip directories */
        if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ||
            strcmp(fd.cFileName, ".") == 0 ||
            strcmp(fd.cFileName, "..") == 0)
                continue;

        /* Take this one */
        break;
    }

    /* Copy name */
    my_strcpy(fname, fd.cFileName, len);

    return TRUE;
}


void my_dclose(ang_dir *dir)
{
    /* Close directory */
    if (dir->h) FindClose(dir->h);

    /* Free memory */
    string_free(dir->first_file);
    mem_free(dir);
}


