/*
 * File: conf.c
 * Purpose: INI file configuration
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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


/*
 * Client config file handler
 */
static char config_name[MSG_LEN];  /* Config filename */


void conf_init(void* param)
{
    char path[MSG_LEN];
    HINSTANCE hInstance = param;

    /* Search for file in user directory */
    if (GetEnvironmentVariable("USERPROFILE", path, sizeof(path)))
    {
        my_strcat(path, "\\mangclient.ini", sizeof(path));

        /* Ok */
        if (file_exists(path))
        {
            my_strcpy(config_name, path, sizeof(config_name));
            return;
        }
    }

    /* Get full path to executable */
    GetModuleFileName(hInstance, path, sizeof(path));
    my_strcpy(path + strlen(path) - 4, ".ini", 5);
    my_strcpy(config_name, path, sizeof(config_name));
}


void conf_save(void)
{
}


void conf_timer(int ticks)
{
}


bool conf_section_exists(const char *section)
{
    char sections[MSG_LEN];
    int n;
    size_t i;

    n = GetPrivateProfileSectionNames(sections, MSG_LEN, config_name);
    if (n != MSG_LEN - 2)
    {
        for (i = 0; sections[i]; i += (strlen(&sections[i]) + 1))
        {
            if (!my_stricmp(&sections[i], section)) return true;
        }
    }

    return false;
}


const char *conf_get_string(const char *section, const char *name, const char *default_value)
{
    static char value[100];

    GetPrivateProfileString(section, name, default_value, value, 100, config_name);
    return &value[0];
}


s32b conf_get_int(const char *section, const char *name, s32b default_value)
{
    return GetPrivateProfileInt(section, name, default_value, config_name);
}


void conf_set_string(const char *section, const char *name, const char *value)
{
    WritePrivateProfileString(section, name, value, config_name);
}


void conf_set_int(const char *section, const char *name, s32b value)
{
    char s_value[100];

    strnfmt(s_value, sizeof(s_value), "%" PRId32, value);
    WritePrivateProfileString(section, name, s_value, config_name);
}


/* Hack -- append section */
void conf_append_section(const char *sectionFrom, const char *sectionTo, const char *filename)
{
    char keys[2 * MSG_LEN];
    char value[MSG_LEN];
    int n;
    size_t i;

    /* Get all keys */
    n = GetPrivateProfileString(sectionTo, NULL, NULL, keys, 2 * MSG_LEN, filename);
    if (n != 2 * MSG_LEN - 2)
    {
        for (i = 0; keys[i]; i += (strlen(&keys[i]) + 1))
        {
            /* Extract key */
            GetPrivateProfileString(sectionFrom, &keys[i], "", value, sizeof(value),
                filename);

            /* Hack -- append key to original config */
            value[100] = '\0'; /* FIXME: change "strings" len */
            conf_set_string(sectionTo, &keys[i], value);
        }
    }
}


bool conf_exists(void)
{
    return file_exists(config_name);
}


static int p_argc = 0;
static const char **p_argv = NULL;


void clia_init(int argc, const char **argv)
{
    /* If it's unsafe, we'll just copy */
    p_argc = argc;
    p_argv = argv;
}


static int clia_find(const char *key)
{
    int i;
    bool awaiting_argument = false;
    bool key_matched = false;
    bool got_hostname = false;

    for (i = 1; i < p_argc; i++)
    {
        if (prefix(p_argv[i], "--"))
        {
            const char *c = &p_argv[i][2];

            if (awaiting_argument && key_matched)
            {
                /*
                 * Hack -- if this is second --longopt in a row, and the
                 * last one was matching our key, assume we're done!
                 */
                return i - 1;
            }
            awaiting_argument = true;
            key_matched = false;
            if (!STRZERO(c) && streq(key, c)) key_matched = true;
        }
        else if (awaiting_argument)
        {
            awaiting_argument = false;
            if (key_matched)
            {
                /* Found */
                return i;
            }
        }
        else if (i == p_argc - 1 || (i == p_argc - 2 && !got_hostname))
        {
            /* Could be hostname */
            if (!got_hostname)
            {
                got_hostname = true;
                if (streq(key, "host"))
                {
                    /* Found */
                    return i;
                }
            }

            /* Port! */
            else
            {
                if (streq(key, "port"))
                {
                    /* Found */
                    return i;
                }
            }
        }
    }

    return -1;
}


static bool clia_cpy_string(char *dst, int len, int i)
{
    if (i > 0 && i < p_argc)
    {
        my_strcpy(dst, p_argv[i], len);
        return true;
    }
    return false;
}


static bool clia_cpy_int(s32b *dst, int i)
{
    if (i > 0 && i < p_argc)
    {
        *dst = atoi(p_argv[i]);
        return true;
    }
    return false;
}


bool clia_read_string(char *dst, int len, const char *key)
{
    int i = clia_find(key);

    return clia_cpy_string(dst, len, i);
}


bool clia_read_int(s32b *dst, const char *key)
{
    int i = clia_find(key);

    return clia_cpy_int(dst, i);
}
