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
 *      Some definitions for internal use by the library code.
 *      This should not be included by user programs.
 *
 *      See readme.txt for copyright information.
 */


#ifndef INTERNAL_H
#define INTERNAL_H

#define __INLINE__ extern inline

/* interrupt hander stuff */
int _install_irq(int num, int (*handler)());
void _remove_irq(int num);

typedef struct _IRQ_HANDLER
{
   int (*handler)();             /* our C handler */
   int number;                   /* irq number */
   __dpmi_paddr old_vector;      /* original protected mode vector */
} _IRQ_HANDLER;

#endif          /* ifndef INTERNAL_H */
