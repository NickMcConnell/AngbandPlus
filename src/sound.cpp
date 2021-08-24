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
#include <QSoundEffect>
#include <src/init.h>

/*
 * Hack -- Make a (relevant?) sound
 */
void sound(int val)
{
    QSoundEffect effect;

    QString sound_file;
    sound_file.clear();

    // Play just once
    effect.setLoopCount(1);

    qreal this_volume = (ui_get_sound_volume() / 100);

    if (!this_volume) return;

    effect.setVolume(this_volume);

    switch (val)
    {
        case SOUND_HIT:
        {
            int which_sound = randint0(7);

            if (which_sound == 0)       sound_file = "attack_hit.wav";
            else if (which_sound == 1)  sound_file = "attack_hit1.wav";
            else if (which_sound == 2)  sound_file = "attack_hit2.wav";
            else if (which_sound == 3)  sound_file = "attack_hit3.wav";
            else if (which_sound == 4)  sound_file = "attack_hit4.wav";
            else if (which_sound == 5)  sound_file = "attack_hit5.wav";
            else if (which_sound == 6)  sound_file = "attack_hit6.wav";
            break;
        }
        //  No Sound File
        default: return;
    }

    // No sound file
    if (!sound_file.length()) return;

    effect.setSource(QUrl::fromLocalFile(QString("%1/%2") .arg(npp_dir_sound.path()) .arg(sound_file)));

    effect.play();
}


void bell(QString this_message)
{
    QApplication::beep();
    if (this_message.length())
    {
         pop_up_message_box(this_message);
    }
}
