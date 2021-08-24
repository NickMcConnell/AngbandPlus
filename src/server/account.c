/*
 * File: account.c
 * Purpose: Account management
 *
 * Copyright (c) 2020 PWMAngband Developers
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


#include "s-angband.h"


u32b get_account(const char *name, const char *pass)
{
    ang_file *fh;
    char filename[MSG_LEN];
    char filebuf[MSG_LEN];
    u32b account_id = 1;
    bool check_name = true, name_ok = false, pass_ok = false;
    char *str;

    /* Check account file */
    path_build(filename, sizeof(filename), ANGBAND_DIR_SAVE, "account");
    if (file_exists(filename))
    {
        /* Open the file */
        fh = file_open(filename, MODE_READ, FTYPE_TEXT);
        if (!fh)
        {
            plog("Failed to open account file!");
            return 0L;
        }

        /* Process the file */
        while (file_getl(fh, filebuf, sizeof(filebuf)))
        {
            /* Get account name */
            if (check_name) name_ok = !my_stricmp(filebuf, name);

            /* Get account password */
            else pass_ok = streq(filebuf, pass);

            /* Check name + password */
            check_name = !check_name;
            if (check_name)
            {
                /* Same name */
                if (name_ok)
                {
                    /* Same password */
                    if (pass_ok) break;

                    /* Incorrect password */
                    return 0L;
                }
                name_ok = false;
                pass_ok = false;
                account_id++;
            }
        }

        /* Close the file */
        file_close(fh);

        /* Found a match */
        if (name_ok && pass_ok) return account_id;
    }

    /* Append to the file */
    fh = file_open(filename, MODE_APPEND, FTYPE_TEXT);
    if (!fh)
    {
        plog("Failed to open account file!");
        return 0L;
    }

    /* Lowercase account name */
    my_strcpy(filebuf, name, sizeof(filebuf));
    for (str = filebuf; *str; str++) *str = tolower((unsigned char)*str);

    /* Create new account */
    file_putf(fh, "%s\n", filebuf);
    file_putf(fh, "%s\n", pass);

    /* Close */
    file_close(fh);

    return account_id;
}
