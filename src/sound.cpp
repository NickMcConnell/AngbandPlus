/*
 * File: sound.c
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "src/npp.h"
#include <QApplication>

/*
 * Hack -- Make a (relevant?) sound
 */
void sound(int val)
{
    (void)val;

    /* No sound */
    //TODO process sounds

}


void bell(QString this_message)
{
    QApplication::beep();
    if (this_message.length())
    {
         pop_up_message_box(this_message);
    }
}
