#include "angband.h"

#include <assert.h>

/**********************************************************************
 * Memorized Forms
 **********************************************************************/
#define _MAX_FORMS 5
static sym_t _forms[_MAX_FORMS];

static bool _is_memorized(int r_idx)
{
    int i;
    for (i = 0; i < _MAX_FORMS; i++)
    {
        if (_forms[i] == r_idx)
            return TRUE;
    }
    return FALSE;
}

bool mimic_is_memorized(int r_idx)
{
    return _is_memorized(r_idx);
}

static int _count_memorized(void)
{
    int ct = 0, i;

    for (i = 0; i < _MAX_FORMS; i++)
    {
        if (_forms[i])
            ct++;
    }
    return ct;
}

/* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
   You are about to enter UI hell. The road looks dangerous and a hot
   breeze carries odors most foul. Do you dare continue?

   I suggest you start reading at the bottom of this section and work
   your way upwards. */

/*
          1         2         3         4         5         6         7
01234567890123456789012345678901234567890123456789012345678901234567890123456789
<----------Name-----------><--------------Extra Info ----------------------->
Name                       STR  INT  WIS  DEX  CON  CHR  Life  Body
=============================================================================
Xiclotlan                   +6   -2   +0   -1   +4   +1  +122% ********
Great Storm Wyrm            +7   +0   +1   -1   +4   +6  +126% ======"~(]

          1         2         3         4         5         6         7
01234567890123456789012345678901234567890123456789012345678901234567890123456789
<----------Name-----------><--------------Extra Info ----------------------->
Name                       Dsrm   Dvce   Save   Stlh  Srch  Prcp  Melee  Bows
=============================================================================
Xiclotlan                  20+12  20+7   36+10     0    14    11  85+30  50+30
Great Storm Wyrm           15+7   30+10  33+10     0    19    24  77+14  63+25

          1         2         3         4         5         6         7
01234567890123456789012345678901234567890123456789012345678901234567890123456789
<----------Name-----------><--------------Extra Info ----------------------->
Name                       Lvl  Max  Speed    AC  Pseudo-Class
=============================================================================
Xiclotlan                   25   30     +0   +60  Warrior
Great Storm Wyrm            58   63    +10  +175  Beastmaster
*/
enum _choice_type_e
{
    _TYPE_UNINITIALIZED,
    _TYPE_NEW,
    _TYPE_KNOWN,
    _TYPE_VISIBLE,
};
struct _choice_s
{
    int  type;
    int  r_idx;
    int  slot;
    char key;
};
typedef struct _choice_s _choice_t, *_choice_ptr;

enum _choose_mode_e
{
    _CHOOSE_MODE_MIMIC,
    _CHOOSE_MODE_LEARN,
    _CHOOSE_MODE_BROWSE,
};

#define _MAX_CHOICES 50

struct _choice_array_s
{
    int        mode;
    _choice_t  choices[_MAX_CHOICES];
    int        size;
    int        current;
};
typedef struct _choice_array_s _choice_array_t, *_choice_array_ptr;

enum _display_mode_e
{
    _DISPLAY_MODE_STATS,
    _DISPLAY_MODE_SKILLS,
    _DISPLAY_MODE_EXTRA,
    _DISPLAY_MODE_MAX
};

static int _display_mode = _DISPLAY_MODE_STATS;

static void _next_display_mode(void)
{
    _display_mode++;
    if (_display_mode == _DISPLAY_MODE_MAX)
        _display_mode = _DISPLAY_MODE_STATS;
}

static void _prt_equippy(int row, int col, int tval, int sval) /* Signatures s/b (x, y) -or- (row, col). This is standard. */
{
    int k_idx = lookup_kind(tval, sval);
    object_kind *k_ptr = &k_info[k_idx];
    Term_putch(col, row, k_ptr->x_attr, k_ptr->x_char);
}

static int _display_width(void)
{
    /*return MIN(ui_map_rect().cx, 80);*/
    return 80; /* We are printing this wide, anyway ... */
}

static int _start_col(void)
{
    return ui_map_rect().x;
}

static int _extra_col(void)
{
    int c = _start_col();
    c += 27;
    return c;
}

static void _clear_row(int row)
{
    Term_erase(_start_col(), row, _display_width());
}

static cptr _choose_prompt(_choice_array_ptr choices)
{
    switch (choices->mode)
    {
    case _CHOOSE_MODE_MIMIC:
        return "Mimic which form?";
    case _CHOOSE_MODE_LEARN:
        return "Replace which existing form?";
    case _CHOOSE_MODE_BROWSE:
        return "Available forms:";
    }
    return "";
}

static int _learn_chance(int r_idx);
static int _mimic_chance(int r_idx);
static void _format_pml(char *buf, int pml)
{
    if (pml == 0)
        strcpy(buf, "0%");
    else if (pml == 1000)
        strcpy(buf, "100%");
    else
        sprintf(buf, "%2d.%1d%%", pml/10, pml%10);
}

static void _list(_choice_array_ptr choices)
{
    int start_col = _start_col();
    int extra_col = _extra_col();
    int row = 1;
    int current_type = _TYPE_UNINITIALIZED;
    int current_row = 0;
    int i;

    _clear_row(1); /* Prompt */
    _clear_row(2); /* Header Line */
    _clear_row(3); /* Header Underline */

    c_put_str(TERM_YELLOW, _choose_prompt(choices), row++, start_col);
    switch (_display_mode)
    {
    case _DISPLAY_MODE_STATS:
                            /* [         1         2     ]   3         4         5         6         7       */
                            /* 01234567890123456789012345678901234567890123456789012345678901234567890123456 */
        c_put_str(TERM_WHITE, "Name                       STR  INT  WIS  DEX  CON  CHR  Life  Body            ", row++, start_col);
        c_put_str(TERM_WHITE, "===============================================================================", row++, start_col);
        break;
    case _DISPLAY_MODE_SKILLS:
        c_put_str(TERM_WHITE, "Name                       Dsrm   Dvce   Save   Stlh  Srch  Prcp  Melee  Bows  ", row++, start_col);
        c_put_str(TERM_WHITE, "===============================================================================", row++, start_col);
        break;
    case _DISPLAY_MODE_EXTRA:
        c_put_str(TERM_WHITE, "Name                       Lvl Max Mimic Learn Speed  AC Pseudo-Class          ", row++, start_col);
        c_put_str(TERM_WHITE, "===============================================================================", row++, start_col);
        break;
    }

    for (i = 0; i < choices->size; i++)
    {
        _choice_ptr choice = &choices->choices[i];

        /* Group Header */
        if (choice->type != current_type)
        {
            if (current_type != _TYPE_UNINITIALIZED)
                _clear_row(row++);

            current_type = choice->type;
            _clear_row(row);
            switch (current_type)
            {
            case _TYPE_NEW:
                c_put_str(TERM_YELLOW, "New Form", row, start_col);
                break;
            case _TYPE_KNOWN:
                c_put_str(TERM_RED, "Known Forms", row, start_col);
                break;
            case _TYPE_VISIBLE:
                c_put_str(TERM_UMBER, "Visible Forms", row, start_col);
                break;
            }
            row++;
        }

        _clear_row(row);

        if (i == choices->current)
            current_row = row;

        if (!choice->r_idx)
        {
            assert(choice->type == _TYPE_KNOWN);
            c_put_str((i == choices->current) ? TERM_L_BLUE : TERM_L_DARK,
                  format(" %-23.23s", "Unused"),
                  row, start_col + 1
            );
        }
        else
        {
            char          buf[255];
            byte          attr = TERM_WHITE;
            monster_race *r_ptr = mon_race_lookup(choice->r_idx);
            term_char_t   r_tc = mon_race_visual(r_ptr);

            /* Name */
            if (i == choices->current)
                attr = TERM_L_BLUE;

            if (choice->key)
                sprintf(buf, " %c) %-20.20s", choice->key, r_ptr->name);
            else
                sprintf(buf, "    %-20.20s", r_ptr->name);

            Term_putch(start_col, row, r_tc.a, r_tc.c);
            c_put_str(attr, buf, row, start_col + 1);

            /* Extra Info */
            if ((plr->wizard || (r_ptr->lore.flags & RFL_POSSESSOR)) && !(r_ptr->body.flags & RF_POS_DISABLED))
            {
                if (_display_mode == _DISPLAY_MODE_STATS)
                {
                    int                j;
                    equip_template_ptr body = equip_template_lookup(r_ptr->body.body_id);

                    for (j = 0; j < 6; j++)
                    {
                        sprintf(buf, "%+3d", r_ptr->body.stats[j]);
                        c_put_str(j == r_ptr->body.spell_stat ? TERM_L_GREEN : TERM_WHITE,
                                    buf, row, extra_col + j * 5);
                    }
                    sprintf(buf, "%+3d%%", r_ptr->body.life ? r_ptr->body.life : 100);
                    c_put_str(TERM_WHITE, buf, row, extra_col + 30);

                    for (j = 1; j <= body->max; j++)
                    {
                        int c = extra_col + 35 + j;
                        int r = row;
                        switch (body->slots[j].type)
                        {
                        case EQUIP_SLOT_GLOVES:
                            _prt_equippy(r, c, TV_GLOVES, SV_SET_OF_GAUNTLETS);
                            break;
                        case EQUIP_SLOT_WEAPON_SHIELD:
                            if (body->slots[j].hand % 2)
                                _prt_equippy(r, c, TV_SHIELD, SV_LARGE_METAL_SHIELD);
                            else
                                _prt_equippy(r, c, TV_SWORD, SV_LONG_SWORD);
                            break;
                        case EQUIP_SLOT_WEAPON:
                            _prt_equippy(r, c, TV_SWORD, SV_LONG_SWORD);
                            break;
                        case EQUIP_SLOT_RING:
                            _prt_equippy(r, c, TV_RING, 0);
                            break;
                        case EQUIP_SLOT_BOW:
                            _prt_equippy(r, c, TV_BOW, SV_LONG_BOW);
                            break;
                        case EQUIP_SLOT_AMULET:
                            _prt_equippy(r, c, TV_AMULET, 0);
                            break;
                        case EQUIP_SLOT_LIGHT:
                            _prt_equippy(r, c, TV_LIGHT, SV_LIGHT_FEANOR);
                            break;
                        case EQUIP_SLOT_BODY_ARMOR:
                            _prt_equippy(r, c, TV_HARD_ARMOR, SV_CHAIN_MAIL);
                            break;
                        case EQUIP_SLOT_CLOAK:
                            _prt_equippy(r, c, TV_CLOAK, SV_CLOAK);
                            break;
                        case EQUIP_SLOT_BOOTS:
                            _prt_equippy(r, c, TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS);
                            break;
                        case EQUIP_SLOT_HELMET:
                            _prt_equippy(r, c, TV_HELM, SV_IRON_HELM);
                            break;
                        case EQUIP_SLOT_ANY:
                            Term_putch(c, r, TERM_WHITE, '*');
                            break;
                        case EQUIP_SLOT_CAPTURE_BALL:
                            _prt_equippy(r, c, TV_CAPTURE, 0);
                            break;
                        case EQUIP_SLOT_QUIVER:
                            _prt_equippy(r, c, TV_QUIVER, 0);
                            break;
                        }
                    }
                }
                else if (_display_mode == _DISPLAY_MODE_SKILLS)
                {
                    sprintf(buf, "%2d+%-2d  %2d+%-2d  %2d+%-2d  %4d  %4d  %4d  %2d+%-2d  %2d+%-2d\n",
                        r_ptr->body.skills.dis, r_ptr->body.extra_skills.dis,
                        r_ptr->body.skills.dev, r_ptr->body.extra_skills.dev,
                        r_ptr->body.skills.sav, r_ptr->body.extra_skills.sav,
                        r_ptr->body.skills.stl,
                        r_ptr->body.skills.srh,
                        r_ptr->body.skills.fos,
                        r_ptr->body.skills.thn, r_ptr->body.extra_skills.thn,
                        r_ptr->body.skills.thb, r_ptr->body.extra_skills.thb
                    );
                    c_put_str(TERM_WHITE, buf, row, extra_col);
                }
                else if (_display_mode == _DISPLAY_MODE_EXTRA)
                {
                    int speed = possessor_r_speed(choice->r_idx);
                    int ac = possessor_r_ac(choice->r_idx);
                    char mimic_buf[10], learn_buf[10];

                    _format_pml(mimic_buf, _mimic_chance(choice->r_idx));
                    _format_pml(learn_buf, _learn_chance(choice->r_idx));

                    sprintf(buf, "%3d %3d %5s %5s %+5d %+3d %-20.20s",
                        r_ptr->alloc.lvl, possessor_max_plr_lvl(choice->r_idx),
                        mimic_buf, learn_buf,
                        speed, ac, get_class_aux(r_ptr->body.class_id, 0)->name);
                    c_put_str(TERM_WHITE, buf, row, extra_col);
                }
            }
            else if (_display_mode == _DISPLAY_MODE_EXTRA)
            {
                char mimic_buf[10], learn_buf[10];

                _format_pml(mimic_buf, _mimic_chance(choice->r_idx));
                _format_pml(learn_buf, _learn_chance(choice->r_idx));

                sprintf(buf, "        %5s %5s", mimic_buf, learn_buf);
                c_put_str(TERM_WHITE, buf, row, extra_col);
            }
        }
        row++;
    }
    _clear_row(row++);
    _clear_row(row);
    if (choices->mode == _CHOOSE_MODE_BROWSE)
        c_put_str(TERM_WHITE, "['?' to recall, '=' for more info, ESC to exit]", row++, start_col);
    else
        c_put_str(TERM_WHITE, "['?' to recall, '=' for more info, ESC to cancel, ENTER to select]", row++, start_col);
    _clear_row(row);

    if (current_row)
        Term_gotoxy(start_col, current_row);
}

static bool _confirm(_choice_array_ptr choices, int which)
{
    if (choices->mode == _CHOOSE_MODE_BROWSE)
        return FALSE;

    if (choices->mode == _CHOOSE_MODE_LEARN)
    {
        _choice_ptr choice = &choices->choices[which];
        if (choice->type != _TYPE_KNOWN)
        {
            msg_print("Choose an existing slot for this new form.");
            return FALSE;
        }
        assert(0 <= choice->slot && choice->slot < _MAX_FORMS);
        if (_forms[choice->slot])
        {
            int           r_idx1 = choices->choices[0].r_idx; /* Hack: We just know this is correct :) */
            monster_race *r_ptr1 = mon_race_lookup(r_idx1);
            int           r_idx2 = _forms[choice->slot];
            monster_race *r_ptr2 = mon_race_lookup(r_idx2);
            char          prompt[512];

            sprintf(prompt, "Really replace %s with %s? ", r_ptr2->name, r_ptr1->name);
            if (!get_check(prompt))
                return FALSE;
        }
    }
    return TRUE;
}

static bool _choose(_choice_array_ptr choices)
{
    int  key = 0, i;
    bool redraw = TRUE;
    bool done = FALSE, result = FALSE;

    assert(choices->size);

    choices->current = 0;
    if (choices->mode == _CHOOSE_MODE_LEARN)
    {
        /* In this mode, the first choice is the form to learn followed by a single group of existing slots */
        assert(choices->size > 1);
        choices->current = 1;
    }

    screen_save();
    while (!done)
    {
        if (redraw)
        {
            /* XXX Currently, the size of the menu is unchanging and _list() clears
             * each menu row on each call. Otherwise, we should use Term_load() and
             * Term_save() instead. See skillmaster.c for how to hack this up */
            _list(choices);
            redraw = FALSE;
        }
        {
            int r_idx = choices->choices[choices->current].r_idx;
            if (r_idx > 0)
            {
                monster_race_track(mon_race_lookup(r_idx));
                window_stuff();
            }
        }

        /* No macros. The problem is that arrow keys are implemented with macros! */
        key = inkey_special(TRUE);

        switch (key)
        {
        case ESCAPE:
            done = TRUE;
            break;
        case '?':
        {
            int r_idx = choices->choices[choices->current].r_idx;
            if (r_idx > 0)
            {
                int x = Term->scr->cx; /* No way to query this? */
                int y = Term->scr->cy;

                screen_load();
                mon_display(mon_race_lookup(r_idx));
                screen_save();

                Term_gotoxy(x, y);
                redraw = TRUE; /* screen_save buggily misses row 0 */
            }
            break;
        }
        case '=':
            _next_display_mode();
            redraw = TRUE;
            break;

        case '8':
        case SKEY_UP:
        {
            int old_current = choices->current;
            choices->current--;
            if (choices->current < 0)
                choices->current = 0;
            if (old_current != choices->current)
                redraw = TRUE;
            break;
        }

        case '2':
        case SKEY_DOWN:
        {
            int old_current = choices->current;
            choices->current++;
            if (choices->current > choices->size - 1)
                choices->current = choices->size - 1;
            if (old_current != choices->current)
                redraw = TRUE;
            break;
        }

        case '\t':
        {
            int old_current = choices->current;
            int old_type = choices->choices[old_current].type;
            /* Tab to next group in the list. Wrap to first group as needed. */
            for (;;)
            {
                choices->current++;
                if (choices->current == choices->size) /* Wrap */
                    choices->current = 0;
                if (choices->choices[choices->current].type != old_type)
                    break;
                if (choices->current == old_current)
                    break;
            }
            if (old_current != choices->current)
                redraw = TRUE;
            break;
        }
        case ' ': case '\r': case '\n':
            if (_confirm(choices, choices->current))
            {
                result = TRUE;
                done = TRUE;
            }
            redraw = TRUE;
            break;
        default:
            for (i = 0; i < choices->size; i++)
            {
                if (choices->choices[i].key == key)
                {
                    choices->current = i;
                    if (_confirm(choices, choices->current))
                    {
                        result = TRUE;
                        done = TRUE;
                    }
                    redraw = TRUE;
                    break;
                }
            }
        }
    }

    screen_load();
    return result;
}

static void _add_visible_form(_choice_array_ptr choices, int r_idx)
{
    int       i = 0;
    _choice_t src = {0};

    src.type = _TYPE_VISIBLE;
    src.r_idx = r_idx;

    for (i = 0; i < _MAX_CHOICES; i++)
    {
        _choice_ptr dest = &choices->choices[i];

        /* Already in list? */
        if (dest->r_idx == src.r_idx)
            break;

        /* Sort in order of decreasing power */
        if (dest->type == _TYPE_VISIBLE && mon_race_lookup(dest->r_idx)->alloc.lvl < mon_race_lookup(src.r_idx)->alloc.lvl)
        {
            /* Swap */
            _choice_t tmp = *dest;
            *dest = src;
            src = tmp;
        }

        /* End of list? */
        if (dest->type == _TYPE_UNINITIALIZED)
        {
            *dest = src;
            choices->size++;
            break;
        }
    }
}

static int _choose_mimic_form(bool browse)
{
    int             r_idx = -1;
    int             i;
    _choice_array_t choices = {0};

    /* List Known Forms */
    for (i = 0; i < _MAX_FORMS; i++)
    {
        if (_forms[i])
        {
            int        j = choices.size++;
            _choice_ptr choice = &choices.choices[j];

            choice->r_idx = _forms[i];
            choice->slot = i;
            choice->type = _TYPE_KNOWN;
            choice->key = I2A(j);
        }
    }

    /* List Visible Forms */
   {point_map_iter_ptr iter;
    for (iter = point_map_iter_alloc(cave->mon_pos);
            point_map_iter_is_valid(iter);
            point_map_iter_next(iter))
    {
        mon_ptr mon = point_map_iter_current(iter);

        if (!mon->ml) continue;
        if (mon->mflag2 & MFLAG2_FUZZY) continue;
        if (!plr_view(mon->pos)) continue;
        if (mon->race->body.flags & RF_POS_DISABLED) continue;
        if (mon_race_is_(mon->race, "q.tanuki")) continue;
        if (mon_race_is_(mon->apparent_race, "N.shadower")) continue;

        _add_visible_form(&choices, mon->race->id);
    }
    point_map_iter_free(iter);}

    /* Assign menu keys at the end due to insertion sort */
    for (i = 0; i < choices.size; i++)
    {
        _choice_ptr choice = &choices.choices[i];

        if (choice->type == _TYPE_VISIBLE)
            choice->key = I2A(i);
    }

    if (choices.size)
    {
        choices.mode = browse ? _CHOOSE_MODE_BROWSE : _CHOOSE_MODE_MIMIC;
        if (_choose(&choices))
            r_idx = choices.choices[choices.current].r_idx;
    }
    else
        msg_print("You see nothing to mimic.");
    return r_idx;
}

static int _choose_new_slot(int new_r_idx)
{
    int             slot = -1;
    int             i;
    _choice_array_t choices = {0};

    /* Display the Newly Learned Form */
    assert(new_r_idx);
    {
        _choice_ptr choice = &choices.choices[choices.size++];
        choice->r_idx = new_r_idx;
        choice->slot = -1; /* paranoia ... it should not be possible to choose this choice! */
        choice->type = _TYPE_NEW;
    }

    /* List Existing Slots/Known Forms */
    for (i = 0; i < _MAX_FORMS; i++)
    {
        if (_forms[i])
        {
            int        j = choices.size++;
            _choice_ptr choice = &choices.choices[j];
            choice->r_idx = _forms[i];
            choice->slot = i;
            choice->type = _TYPE_KNOWN;
            choice->key = I2A(j-1);
        }
        else
        {
            /* Simply use the first empty slot */
            return i;
        }
    }

    choices.mode = _CHOOSE_MODE_LEARN;
    if (_choose(&choices))
        slot = choices.choices[choices.current].slot;

    return slot;
}

static bool _memorize_form(int r_idx)
{
    int           i;
    monster_race *r_ptr = mon_race_lookup(r_idx);

    if (_is_memorized(r_idx))
    {
        msg_format("You already know this form (%s).", r_ptr->name);
        return FALSE;
    }

    i = _choose_new_slot(r_idx);
    if (i >= 0 && i < _MAX_FORMS)
    {
        _forms[i] = r_idx;
        msg_format("You have learned this form (%s).", r_ptr->name);
        return TRUE;
    }
    return FALSE;
}
/* You have finally left UI hell. A bit singed but unbroken!
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ */

static void _load(savefile_ptr file)
{
    int ct, i;

    for (i = 0; i < _MAX_FORMS; i++)
        _forms[i] = 0;

    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        int r_idx = savefile_read_sym(file);
        if (i < _MAX_FORMS)
            _forms[i] = r_idx;
    }
    possessor_on_load(file);
}

static void _save(savefile_ptr file)
{
    int i;

    savefile_write_s16b(file, _count_memorized());

    for (i = 0; i < _MAX_FORMS; i++)
    {
        if (_forms[i])
            savefile_write_sym(file, _forms[i]);
    }
    possessor_on_save(file);
}

/**********************************************************************
 * Utilities
 **********************************************************************/
/* What is the max monster level the player can mimic? Be careful
 * here, since, unlike the possessor, the player can assume forms
 * they otherwise would be unable to kill. The possessor must be
 * strong enough to kill the monster *before* getting the corpse.
 * In fact, I think you could remove level restrictions from the
 * possessor altogether and the game would still be fair. But not
 * from mimics! And mimics were using the possessor calc, allowing,
 * for example, L50 forms at CL32, which is quite ridiculous! The
 * following is a bit harsh but mimic's get 5 forms and are quite
 * powerful. They can also learn forms without DROP_CORPSE ... */
static int _max_level[51] = {
     0,
     5,  6,  7,  8,  9,
    10, 11, 12, 13, 14, /* CL10 */
    15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, /* CL20 */
    25, 26, 27, 28, 29,
    30, 31, 32, 33, 34, /* CL30 */
    35, 36, 37, 38, 39,
    40, 41, 42, 43, 45, /* CL40 */
    47, 50, 55, 60, 65,
    70, 75, 80, 90, 100 /* CL50 */
};
int mimic_max_lvl(void)
{
    int l = 5;
    if (1 <= plr->lev && plr->lev <= 50) /* paranoia */
        l = _max_level[plr->lev];
    return l;
}

/* What are the odds of learning a form? Low level forms are easily
 * learned. Learning forms below the current max level is also easier
 * (except for uniques). Players should not be able to count on learning
 * any particular form ... This enhances replayability. */
static int _learn_chance(int r_idx) /* per mil */
{
    mon_race_ptr race = mon_race_lookup(r_idx);
    int          pml = 0;
    int          max = _max_level[plr->lev];

    if (race->alloc.lvl <= max)
    {
        pml = 5000 / MAX(3, race->alloc.lvl); /* 5% chance to learn Morgoth */
        if (!mon_race_is_unique(race))
            pml += (max - race->alloc.lvl)*2;
    }

    return MAX(0, MIN(250, pml));
}

static int _mimic_chance(int r_idx) /* per mil */
{
    mon_race_ptr race = mon_race_lookup(r_idx);
    int          pml = 0;

    if (race->alloc.lvl <= plr->lev)
        pml = 1000;
    else if (_is_memorized(r_idx))
        pml = 1000;
    else
    {
        int pl = _max_level[plr->lev];
        int rl = race->alloc.lvl;
        int fudge_pct = 120; /* tweakable way to make mimicry, say, 20% easier */
        pl += 3 + plr->stat_ind[A_DEX];
        if (pl > rl)
            pml = (pl - rl) * 1000 * fudge_pct / (pl * 100);
        else
            pml = 0;
    }
    return MAX(0, MIN(1000, pml));
}

static void _dismiss_pets(void)
{
    vec_ptr pets = plr_pets_for_dismiss();
    int i, ct = vec_length(pets), dismissed = 0;
    for (i = 0; i < ct; i++)
    {
        mon_ptr mon = vec_get(pets, i);
        char name[MAX_NLEN];
        monster_desc(name, mon, MD_ASSUME_VISIBLE);
        msg_format("%s disappears.", name);
        delete_monster(mon);
        dismissed++;
    }
    vec_free(pets);
    if (dismissed) calculate_upkeep();
}

static void _set_current_r_idx(int r_idx)
{
    if (r_idx == plr->current_r_idx)
        return;

    disturb(1, 0);
    if (sym_equals(r_idx, "@.mimic") && plr->current_r_idx)
    {
        msg_format("You stop mimicking %s.", plr_mon_race()->name);
        plr_tim_remove(T_INVULN); /* XXX dispel_player? what is this here for?? */
        _dismiss_pets(); /* They no longer recognize you as their leader! */
    }
    possessor_set_current_r_idx(r_idx);
    if (!sym_equals(r_idx, "@.mimic"))
        msg_format("You start mimicking %s.", plr_mon_race()->name);
    /* Mimics shift forms often enough to be annoying if shapes
       have dramatically different body types (e.g. dragons vs humanoids).
       Inscribe gear with @mimic to autoequip on shifing. */
    equip_shuffle("@mimic1");
    equip_shuffle("@mimic2");
    equip_shuffle("@mimic3");
    equip_shuffle("@mimic4");
    equip_shuffle("@mimic");
}

static void _birth(void)
{
    object_type forge;
    int i;

    possessor_on_birth(); /* Cleanup history from last character */

    for (i = 0; i < _MAX_FORMS; i++)
        _forms[i] = 0;

    plr->current_r_idx = mon_race_parse("@.mimic")->id;
    equip_on_change_race();

    object_prep(&forge, lookup_kind(TV_SWORD, SV_LONG_SWORD));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 3;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    plr_birth_food();
    plr_birth_light();
}

static bool _is_visible(int r_idx)
{
    vec_ptr v = dun_filter_mon(cave, plr_project_mon);
    bool b = vec_length(v) > 0;
    vec_free(v);
    return b;
}

static void _player_action(void)
{
    if (possessor_get_toggle() == LEPRECHAUN_TOGGLE_BLINK)
        teleport_player(10, TELEPORT_LINE_OF_SIGHT);

    /* Maintain current form. Non-memorized forms require los of target race */
    if ( !sym_equals(plr->current_r_idx, "@.mimic")
      && !_is_memorized(plr->current_r_idx) )
    {
        cptr msg = NULL;

        if (plr_tim_find(T_CONFUSED))
            msg = "You are too confused to maintain your current form.";
        else if (plr_tim_find(T_HALLUCINATE))
            msg = "Groovy! I think I'll mimic that guy instead!!";
        else if (one_in_(100) && (plr_tim_find(T_BLIND) || !_is_visible(plr->current_r_idx)))
            msg = "You can no longer see the source of your current form.";

        if (msg)
        {
            msg_print(msg);
            _set_current_r_idx(mon_race_parse("@.mimic")->id);
        }
    }
}

/**********************************************************************
 * Powers
 **********************************************************************/
static void _browse_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Browse Forms");
        break;
    case SPELL_DESC:
        var_set_string(res, "Browse available forms without leaving your current form.");
        break;
    case SPELL_CAST:
        _choose_mimic_form(TRUE);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        var_set_int(res, 0);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _mimic_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        if (sym_equals(plr->current_r_idx, "@.mimic"))
            var_set_string(res, "Mimic");
        else
            var_set_string(res, "Stop Mimicry");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Max L%d", mimic_max_lvl()));
        break;
    case SPELL_DESC:
        if (sym_equals(plr->current_r_idx, "@.mimic"))
        {
            str_ptr s = str_alloc();
            str_append_s(s, "Mimic a nearby visible monster, gaining the powers and abilities of that form.");
            str_printf(s, " You may attempt to mimic a monster of any level, but may fail if the monster is higher than level %d.", plr->lev);
            str_printf(s, " You may permanently learn monster forms up to level %d.", _max_level[plr->lev]);
            var_set_string(res, str_buffer(s));
            str_free(s);
        }
        else
            var_set_string(res, "Return to your native form.");
        break;
    case SPELL_CAST: {
        var_set_bool(res, FALSE);
        if (sym_equals(plr->current_r_idx, "@.mimic"))
        {
            int           r_idx = _choose_mimic_form(FALSE);
            monster_race *r_ptr = 0;
            int           pml;

            if (!r_idx) return;

            r_ptr = mon_race_lookup(r_idx);
            if (!r_ptr) return;

            pml = _mimic_chance(r_idx);
            if (pml <= 0)
                msg_format("You are not powerful enough to mimic this form (%s: Lvl %d).", r_ptr->name, r_ptr->alloc.lvl);
            else if (randint1(1000) > pml)
                msg_print("<color:v>Failed!</color>");
            else
                _set_current_r_idx(r_idx);
        }
        else
            _set_current_r_idx(mon_race_parse("@.mimic")->id);
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _add_power(spell_info* spell, int lvl, int cost, int fail, ang_spell fn, int stat_idx)
{
    spell->level = lvl;
    spell->cost = cost;
    spell->fail = calculate_fail_rate(lvl, fail, stat_idx);
    spell->fn = fn;
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    if (ct < max)
        _add_power(&spells[ct++], 1, 0, 0, _mimic_spell, plr->stat_ind[A_DEX]);

    ct += possessor_get_powers(spells + ct, max - ct);

    if (!sym_equals(plr->current_r_idx, "@.mimic"))
        _add_power(&spells[ct++], 1, 0, 0, _browse_spell, plr->stat_ind[A_DEX]);
    return ct;
}

void _character_dump(doc_ptr doc)
{
    int i;
    bool first = TRUE;

    for (i = 0; i < _MAX_FORMS; i++)
    {
        if (_forms[i])
        {
            if (first)
            {
                doc_printf(doc, "<topic:LearnedForms>================================ <color:keypress>L</color>earned Forms ================================\n\n");
                first = FALSE;
            }
            doc_printf(doc, " %s\n", mon_race_lookup(_forms[i])->name);
        }
    }
    doc_newline(doc);
    possessor_character_dump(doc);
}

/**********************************************************************
 * Public
 **********************************************************************/
plr_race_ptr mon_mimic_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_MON_MIMIC);
        me->name = "Mimic";
        me->desc = "Mimics are similar to possessors but instead of controlling the corpses of the "
                    "vanquished, the mimic imitates those about them. This allows the mimic to assume "
                    "the forms of foes they have yet to conquer and is quite useful. However, there is "
                    "a small catch: The mimic can only copy what they see! This limitation forces the "
                    "mimic to change forms much more often than the possessor would as knowledge of their "
                    "current body rapidly fades when the original is no longer about. Occasionally, the "
                    "mimic is able to memorize a particular form well enough to use it again without the "
                    "original body to imitate, though this does not happen very often and the mimic can "
                    "only memorize a small number of forms. To have a chance of this, the mimic must be "
                    "in the desired form when slaying the original.\n \n"
                    "Mimics are monsters and do not choose a normal class. Their stats, skills, resistances "
                    "and spells are determined by the form they assume. Their current body also "
                    "determines their spell stat (e.g. a novice priest uses wisdom, a novice mage uses intelligence). "
                    "Their current body may offer innate powers (e.g. breath weapons or rockets) in addition to or in lieu "
                    "of magical powers (e.g. mana storms and frost bolts). Be sure to check both the racial power "
                    "command ('U') and the magic command ('m') after assuming a new body.";

        me->exp = 250;
        me->shop_adjust = 110; /* Really should depend on current form */

        me->hooks.birth = _birth;
        me->hooks.get_powers = _get_powers;
        me->hooks.calc_innate_attacks = possessor_calc_innate_attacks;
        me->hooks.calc_bonuses = possessor_calc_bonuses;
        me->hooks.calc_shooter_bonuses = possessor_calc_shooter_bonuses;
        me->hooks.calc_weapon_bonuses = possessor_calc_weapon_bonuses;
        me->hooks.get_flags = possessor_get_flags;
        me->hooks.player_action = _player_action;
        me->hooks.character_dump = _character_dump;
        me->hooks.load_player = _load;
        me->hooks.save_player = _save;

        me->flags = RACE_IS_MONSTER;
        me->boss_r_idx = mon_race_parse("R.Chameleon")->id;
    }
    possessor_init_race_t(me, mon_race_parse("@.mimic")->id);
    return me;
}

void mimic_dispel_player(void)
{
    if (plr->prace != RACE_MON_MIMIC) return;
    if (sym_equals(plr->current_r_idx, "@.mimic")) return;

    if (randint0(150) < plr->skills.sav) /* Anti-magic gives 145 */
        msg_print("You maintain your current form.");
    else
        _set_current_r_idx(mon_race_parse("@.mimic")->id);
}

void mimic_on_kill_monster(int r_idx)
{
    int pml;
    if (plr->prace != RACE_MON_MIMIC) return;

    /* To learn a form, you must be mimicking it when you land the killing blow. */
    if (r_idx != plr->current_r_idx) return;
    if (_is_memorized(r_idx)) return;

    pml = _learn_chance(r_idx);
    if (randint0(1000) < pml)
        _memorize_form(r_idx);
}

