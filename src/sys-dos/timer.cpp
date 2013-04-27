// File: timer.cpp

/*
 * This file was taken from Allegro.  See original copyright below.
 *
 * Adapted for Utumno by Matt Craighead.
 */

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
 *      Timer interrupt routines. 
 *
 *      See readme.txt for copyright information.
 */


#include "../utumno.h"
#include "sys-dos.h"
/* SAW 
 #include "internal.h"
*/
const int DIVISOR = 8; // Multiply by 18.2 for ticks per second

const int TIMER_INT = 8;

static volatile unsigned long ticker = 0;
static int bios_wait = 0;


/*
 * Get the value of ticker.
 */
u32b get_timer_value(void) { return ticker; }


/*
 * Reset the ticker.
 */
void reset_timer(void) { ticker = 0; }


/* set_timer:
 *  Sets the delay time for PIT channel 1.
 */
static inline void set_timer(long time)
{
/*SAW
    outportb(0x43, 0x34);
    outportb(0x40, time & 0xff);
    outportb(0x40, time >> 8);
    */
}



/* my_timerint:
 *  Hardware level timer interrupt (int 8) handler. Calls whatever user
 *  timer routines are due for dealing with, and calculates how long until
 *  we need another timer tick.
 */
static int my_timerint()
{
	
    bool bios = FALSE;
/*SAW
    // Always increment the ticker
    ticker++;

    // update bios time 
    if (!bios_wait) bios_wait = DIVISOR;
    bios_wait--;
    if (!bios_wait) bios = TRUE;

    if (!bios) {
        ENABLE();
        outportb(0x20, 0x20);      // ack. the interrupt 
        DISABLE();
    }
*/
    return bios;
}

static END_OF_FUNCTION(my_timerint);



/* install_timer:
 *  Installs the timer interrupt handler. You must do this before installing
 *  any user timer routines.
 */
int install_timer(void)
{
/*SAW
    int x;
    LOCK_VARIABLE(ticker);
    LOCK_VARIABLE(bios_wait);
    LOCK_FUNCTION(my_timerint);

    if (_install_irq(TIMER_INT, my_timerint) != 0) return -1;

    DISABLE();

    // windoze doesn't seem to notice if we only do this once... 
    for (x=0; x<16; x++) set_timer(0x10000L / DIVISOR);

    ENABLE();
*/
    return 0;
}



/* remove_timer:
 *  Removes our timer handler and resets the BIOS clock. You don't normally
 *  need to call this, because allegro_exit() will do it for you.
 */
void remove_timer(void)
{
    // Shut off interrupts while in this critical section
/*SAW
    DISABLE();

    // Reset it to 18.2 ticks/sec
    outportb(0x43, 0x34);
    outportb(0x40, 0);
    outportb(0x40, 0);

    // Remove the interrupt handler
    _remove_irq(TIMER_INT);

    // Reset it again
    outportb(0x43, 0x34);
    outportb(0x40, 0);
    outportb(0x40, 0);

    // Turn interrupts back on
    ENABLE();
    */
}

