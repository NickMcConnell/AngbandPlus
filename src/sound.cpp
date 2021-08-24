/*
 * File: sound.c
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
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
