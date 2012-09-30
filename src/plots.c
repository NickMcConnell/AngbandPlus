/* File: plots.c */

/* Purpose: plots & quests */

/*
 * Copyright (c) 2001 James E. Wilson, Robert A. Koeneke, DarkGod
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/* #define DEBUG_HOOK */

/******** Hooks stuff *********/
s32b hook_option;
FILE *hook_file;

typedef struct hooks_chain hooks_chain;
struct hooks_chain
{
        hook_type hook;
        char name[40];
        hooks_chain *next;
};

static hooks_chain *hooks_heads[MAX_HOOKS];

/* Wipe hooks and init them with quest hooks */
void init_hooks()
{
        int i;

        for (i = 0; i < MAX_HOOKS; i++)
        {
                hooks_heads[i] = NULL;
        }

        for (i = 0; i < MAX_Q_IDX; i++)
        {
                if (quest[i].init != NULL) quest[i].init(i);
        }
}

/* Add a hook */
void add_hook(int h_idx, hook_type hook, cptr name)
{
        hooks_chain *new, *c = hooks_heads[h_idx];

        /* Find it */
        while ((c != NULL) && (c->hook != hook))
        {
                c = c->next;
        }

        /* If not already in the list, add it */
        if (c == NULL)
        {
                MAKE(new, hooks_chain);
                new->hook = hook;
                sprintf(new->name, name);
#ifdef DEBUG_HOOK
                if (wizard) cmsg_format(TERM_VIOLET, "HOOK ADD: %s", name);
                if (take_notes) add_note(format("HOOK ADD: %s", name), 'D');
#endif
                new->next = hooks_heads[h_idx];
                hooks_heads[h_idx] = new;
        }
}

/* Remove a hook */
void del_hook(int h_idx, hook_type hook)
{
        hooks_chain *c = hooks_heads[h_idx], *p = NULL;

        /* Find it */
        while ((c != NULL) && (c->hook != hook))
        {
                p = c;
                c = c->next;
        }

        /* Remove it */
        if (c != NULL)
        {
                if (p == NULL)
                {
#ifdef DEBUG_HOOK
                        if (wizard) cmsg_format(TERM_VIOLET, "HOOK DEL: %s", c->name);
                        if (take_notes) add_note(format("HOOK DEL: %s", c->name), 'D');
#endif
                        FREE(c, hooks_chain);
                        hooks_heads[h_idx] = NULL;
                }
                else
                {
#ifdef DEBUG_HOOK
                        if (wizard) cmsg_format(TERM_VIOLET, "HOOK DEL: %s", c->name);
                        if (take_notes) add_note(format("HOOK DEL: %s", c->name), 'D');
#endif
                        p->next = c->next;
                        FREE(c, hooks_chain);
                }
        }
}

/* Actually process the hooks */
static process_hooks_restart = FALSE;
bool process_hooks(int h_idx, int q_idx)
{
        hooks_chain *c = hooks_heads[h_idx];

        while (c != NULL)
        {
                /* Should we restart ? */
                if (c->hook(q_idx)) return TRUE;

                if (process_hooks_restart)
                {
                        c = hooks_heads[h_idx];
                        process_hooks_restart = FALSE;
                }
                else
                {
                        c = c->next;
                }
        }

        return FALSE;
}

/******** Plots & Quest stuff ********/

static void quest_describe(int q_idx)
{
        int i = 0;

        while ((i < 10) && (quest[q_idx].desc[i] != NULL))
        {
                cmsg_print(TERM_YELLOW, quest[q_idx].desc[i++]);
        }
}

/* Catch-all quest hook */
bool quest_null_hook(int q_idx)
{
        /* Do nothing */
        return (FALSE);
}

/************************** Random Quests *************************/
#include "q_rand.c"

/**************************** Main plot ***************************/
#include "q_main.c"

/**************************** Bree plot ***************************/
#include "q_thief.c"
#include "q_hobbit.c"
#include "q_troll.c"
#include "q_wight.c"
#include "q_nazgul.c"

/*************************** Lorien plot **************************/
#include "q_spider.c"
#include "q_poison.c"

/************************** Gondolin plot *************************/
#include "q_eol.c"
#include "q_nirna.c"
#include "q_invas.c"

/************************* Minas Anor plot **************************/
#include "q_betwen.c"

/*************************** Other plot **************************/
#include "q_narsil.c"
