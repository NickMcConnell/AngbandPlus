/*
 * File: md5.h
 * Purpose: MD5 message-digest algorithm
 */

#ifndef INCLUDED_MD5_H
#define INCLUDED_MD5_H

/*
 * Maximum length for names and passwords
 *
 */
#define MAX_NAME_LEN    15
#define MAX_PASS_LEN    40

extern void MD5Password(char *string);

#endif
