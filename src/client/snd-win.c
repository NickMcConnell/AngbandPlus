/*
 * File: snd-win.c
 * Purpose: Windows sound support
 *
 * Copyright (c) 2016 Graeme Russ <graeme.russ@gmail.com>
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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

#include "c-angband.h"


/* Supported file types */
enum
{
    WIN_NULL = 0,
    WIN_MP3,
    WIN_WAV
};


static const struct sound_file_type supported_sound_files[] =
{
    {".mp3", WIN_MP3},
    {".wav", WIN_WAV},
    {"", WIN_NULL}
};


/*
 * Struct representing all data about an event sample
 */
typedef struct
{
    int type;
    MCI_OPEN_PARMS op;
    MCIDEVICEID device;
    char *filename;
} win_sample;


static bool open_audio_win(void)
{
    return true;
}


/*
 * Load a sound from file.
 */
static bool load_sample_win(const char *filename, int file_type, win_sample *sample)
{
    switch (file_type)
    {
        case WIN_MP3:
        {
            /* Open if not already */
            if (!sample->device)
            {
                sample->op.dwCallback = 0;
                sample->op.lpstrDeviceType = (char*)MCI_ALL_DEVICE_ID;
                sample->op.lpstrElementName = filename;
                sample->op.lpstrAlias = NULL;

                /* Open command */
                mciSendCommand(0, MCI_OPEN, MCI_OPEN_ELEMENT | MCI_WAIT, (DWORD)&sample->op);
                sample->device = sample->op.wDeviceID;
            }
            if (sample->device) return true;
            break;
        }

        case WIN_WAV:
        {
            sample->filename = mem_zalloc(strlen(filename) + 1);
            my_strcpy(sample->filename, filename, strlen(filename) + 1);
            return true;
        }

        default:
            plog("Oops - Unsupported file type");
            break;
    }

    return false;
}


/*
 * Load a sound and return a pointer to the associated Windows Sound data
 * structure back to the core sound module.
 */
static bool load_sound_win(const char *filename, int file_type, struct sound_data *data)
{
    win_sample *sample = (win_sample *)(data->plat_data);

    if (!sample) sample = mem_zalloc(sizeof(*sample));

    /* Try and load the sample file */
    data->loaded = load_sample_win(filename, file_type, sample);

    if (data->loaded)
        sample->type = file_type;
    else
    {
        mem_free(sample);
        sample = NULL;
    }

    data->plat_data = (void *)sample;

    return (NULL != sample);
}


/*
 * Play the sound stored in the provided Windows Sound data structure.
 */
static bool play_sound_win(struct sound_data *data)
{
    win_sample *sample = (win_sample *)(data->plat_data);

    if (sample)
    {
        switch (sample->type)
        {
            case WIN_MP3:
            {
                if (sample->device)
                {
                    MCI_PLAY_PARMS pp;

                    /* Play command */
                    pp.dwCallback = 0;
                    pp.dwFrom = 0;
                    return (!mciSendCommand(sample->device, MCI_PLAY, MCI_NOTIFY | MCI_FROM, (DWORD)&pp));
                }
                break;
            }

            case WIN_WAV:
            {
                /* If another sound is currently playing, stop it */
                PlaySound(NULL, 0, SND_PURGE);

                if (sample->filename)
                {
                    /* Play the sound */
                    return PlaySound(sample->filename, 0, SND_FILENAME | SND_ASYNC);
                }
                break;
            }

            default: break;
        }
    }

    return false;
}


/*
 * Free resources referenced in the provided Windows Sound data structure.
 */
static bool unload_sound_win(struct sound_data *data)
{
    win_sample *sample = (win_sample *)(data->plat_data);

    if (sample)
    {
        switch (sample->type)
        {
            case WIN_MP3:
            {
                if (sample->device)
                    mciSendCommand(sample->device, MCI_CLOSE, MCI_WAIT, NULL);
                break;
            }

            case WIN_WAV:
            {
                mem_free(sample->filename);
                break;
            }

            default: break;
        }

        mem_free(sample);
        data->plat_data = NULL;
        data->loaded = false;
    }

    return true;
}


static bool close_audio_win(void)
{
    return true;
}


const struct sound_file_type *supported_files_win(void)
{
    return supported_sound_files;
}


/*
 * Init the Windows sound "module".
 */
errr init_sound_win(struct sound_hooks *hooks)
{
    hooks->open_audio_hook = open_audio_win;
    hooks->supported_files_hook = supported_files_win;
    hooks->close_audio_hook = close_audio_win;
    hooks->load_sound_hook = load_sound_win;
    hooks->unload_sound_hook = unload_sound_win;
    hooks->play_sound_hook = play_sound_win;

    /* Success */
    return (0);
}
