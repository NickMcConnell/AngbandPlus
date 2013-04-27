/*         ______   ___    ___ 
 *        /\  _  \ /\_ \  /\_ \ 
 *        \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___ 
 *         \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\
 *          \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \
 *           \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/
 *            \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/
 *                                           /\____/
 *                                           \_/__/
 *      By Shawn Hargreaves,
 *      1 Salisbury Road,
 *      Market Drayton,
 *      Shropshire,
 *      England, TF9 1AJ.
 *
 *      A few macros to make my asm code (slightly :-) more readable.
 *
 *      See readme.txt for copyright information.
 */


#ifndef ASMDEFS_H
#define ASMDEFS_H

/* readable way to access arguments passed from C code */
#define ARG1      8(%ebp)
#define ARG2      12(%ebp)
#define ARG3      16(%ebp)
#define ARG4      20(%ebp)
#define ARG5      24(%ebp)
#define ARG6      28(%ebp)
#define ARG7      32(%ebp)
#define ARG8      36(%ebp)
#define ARG9      40(%ebp)


/* How many stacks to allocate for the irq wrappers. This can't be in the 
 * main headers, because it is used by both C and asm code. You could 
 * probably get away with fewer of these, if you want to save memory and
 * you are feeling brave...
 */
#define IRQ_STACKS      8


/* automatically generated structure offsets for use by asm code */

#define IRQ_SIZE              16
#define IRQ_HANDLER           0
#define IRQ_NUMBER            4
#define IRQ_OLDVEC            8

#endif   /* ifndef ASMDEFS_H */

