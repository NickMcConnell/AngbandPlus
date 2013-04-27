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
#include "sys-lin.h"
#include <sys/time.h>
const int DIVISOR = 8; // Multiply by 18.2 for ticks per second
const int TIMER_INT = 8;

//static volatile unsigned long ticker = 0;
struct timeval ticker;
// static int bios_wait = 0;

/*
 * Get the value of ticker.
 */
u32b get_timer_value(void) { 

	/*
	 *  SAW : wait 1/8 of the turn lenght (i.e. 1 frame lenght),
	 *        corrected by the time spent drawing the frame.
	 */
	struct timeval t;
	double time;
	gettimeofday(&t , NULL);
	time=((double)(t.tv_sec-ticker.tv_sec)+(double)(t.tv_usec-ticker.tv_usec)/1000000.0);
	if ((double)game_speed*7.0/100.0 > 8*time)
	{
       	   usleep((int)(1000000.0*((double)game_speed*7.0/100.0-time)/8.0));
	   return game_speed;
	}
	else   // game speed is too fast for system resources
		return game_speed;  // SAW CORREGGI !! cosa bisogna ritornare ?
}


/*
 * Reset the ticker.
 */
void reset_timer(void) {
	gettimeofday(&ticker , NULL);
}


/* set_timer:
 *  Sets the delay time for PIT channel 1.
 */
static inline void set_timer(long time){}



/* my_timerint:
 *  Hardware level timer interrupt (int 8) handler. Calls whatever user
 *  timer routines are due for dealing with, and calculates how long until
 *  we need another timer tick.
 */
// -- MV: this is an old function used in the DOS porting. We don't need it no more!
/* 
static int my_timerint(){}
static END_OF_FUNCTION(my_timerint);

*/

/* install_timer:
 *  Installs the timer interrupt handler. You must do this before installing
 *  any user timer routines.
 */
int install_timer(void){return 1;}



/* remove_timer:
 *  Removes our timer handler and resets the BIOS clock. You don't normally
 *  need to call this, because allegro_exit() will do it for you.
 */
void remove_timer(void){}
