#include "angband.h"

#include <assert.h>

/****************************************************************
 * Spells: The Blue-Mage learns monster spells thru direct experience.
 * Unlike Hengband, the source of the spell is remembered and determines
 * the power of the spell. Also unlike Hengband, there is a relatively
 * small number of slots for remembering spells.
 ****************************************************************/

/* Learned spells are stored in a small number of slots */
#define _MAX_SLOTS      15
#define _INVALID_SLOT   -1
typedef struct {
    mon_spell_ptr spell;
    mon_race_ptr  race;
    spell_stats_t stats;
} _spell_t, *_spell_ptr;

static _spell_t _spells[_MAX_SLOTS];
static void _clear_spells(void)
{
    int i;
    for (i = 0; i < _MAX_SLOTS; i++)
    {
        memset(&_spells[i], 0, sizeof(_spell_t));
    }
}
static _spell_ptr _get_spell(int slot)
{
    assert(0 <= slot && slot < _MAX_SLOTS);
    return &_spells[slot];
}
static int _count(void)
{
    int i, ct = 0;
    for (i = 0; i < _MAX_SLOTS; i++)
    {
        _spell_ptr spell = _get_spell(i);
        if (spell) ct++;
    }
    return ct;
}
static _spell_ptr _find(mon_spell_ptr spell, mon_race_ptr race)
{
    int i;
    for (i = 0; i < _MAX_SLOTS; i++)
    {
        _spell_ptr s = _get_spell(i);
        if (s->spell == spell && s->race == race) /* racial check is redundant ... s->spell points into s->race->spells */
            return s;
    }
    return NULL;
}

/* Savefile Support */
static void _load_player(savefile_ptr file)
{
    _clear_spells();
    while (1)
    {
        int i = savefile_read_u16b(file);
        sym_t r_id;
        mon_spell_id_t id;
        _spell_ptr spell;

        if (i == 0xFFFF) break;

        spell = _get_spell(i);
        id.type = savefile_read_byte(file);
        id.effect = savefile_read_s16b(file);
        if (savefile_is_older_than(file, 7, 3, 1, 1))
            spell_stats_wipe(&spell->stats);
        else
            spell_stats_load(&spell->stats, file);
        r_id = savefile_read_sym(file);
        spell->race = mon_race_lookup(r_id);
        /* paranoid checks to handle r_info changes (e.g. monster deleted or
         * its spell table altered). In this case, the player loses the learned
         * spell (but this should never happen). */
        if (!spell->race) continue;
        if (spell->race->id != r_id) continue;
        if (!spell->race->spells)
        {
            spell->race = NULL;
            continue;
        }
        spell->spell = mon_spells_find(spell->race->spells, id);
        if (!spell->spell)
        {
            spell->race = NULL;
            continue;
        }
    }
}
static void _save_player(savefile_ptr file)
{
    int i;
    for (i = 0; i < _MAX_SLOTS; i++)
    {
        _spell_ptr spell = _get_spell(i);
        if (spell->spell)
        {
            savefile_write_u16b(file, i);
            savefile_write_byte(file, spell->spell->id.type);
            savefile_write_s16b(file, spell->spell->id.effect);
            spell_stats_save(&spell->stats, file);
            savefile_write_sym(file, spell->race->id);
        }
    }
    savefile_write_u16b(file, 0xFFFF); /* sentinel */
}

/* List slot contents to the user (for learning/browsing/casting/character sheet) */
#define _ALLOW_EMPTY    0x01
#define _ALLOW_EXCHANGE 0x02
#define _SHOW_INFO      0x04
#define _SHOW_STATS     0x08
#define _SHOW_GRAPHICS  0x10
#define _LEARNING       0x20
static int _max_cost(_spell_ptr spell)
{
    int avail = plr->csp;
    /* XXX Normally, innate spell costs spill over into hp. But the BM
     * is a bit too powerful wrt breaths atm, so let's nerf this.
    if (spell->spell->flags & MSF_INNATE)
        avail += plr->chp;
    else*/ if (plr_tim_find(T_BERSERK)) /* blocks non-innate spells */
        avail = 0;
    return avail;
}
static void _list_spell_aux(doc_ptr doc, _spell_ptr spell, int options)
{
    int cost = mon_spell_cost_plr(spell->spell, spell->race);
    int fail = mon_spell_fail_plr(spell->spell, spell->race);
    cptr stat = "   ";

    if (spell->spell->id.type == MST_BREATH)
        stat = "Con";
    else if (spell->race->body.spell_stat != A_NONE)
        stat = stat_abbrev_true[spell->race->body.spell_stat];

    if (options & _SHOW_GRAPHICS)
    {
        doc_insert_term_char(doc, mon_race_visual(spell->race));
        doc_space(doc);
    }
    else
    {
        doc_insert_term_char(doc, mon_race_visual_ascii(spell->race));
        doc_space(doc);
    }
    mon_spell_doc(spell->spell, doc);
    doc_printf(doc, "<tab:30>%s %3d %3d %3d%%", stat, spell->race->alloc.lvl, cost, fail);

    if (options & _SHOW_INFO)
        mon_spell_list_info(doc, spell->spell, spell->race);
    if (options & _SHOW_STATS)
    {
        if (spell->stats.ct_cast + spell->stats.ct_fail)
        {
            doc_printf(doc, "<tab:60>%5d %4d %3d%%",
                spell->stats.ct_cast,
                spell->stats.ct_fail,
                spell_stats_fail(&spell->stats)
            );
        }
    }
}
static void _list_spell(doc_ptr doc, _spell_ptr spell, int choice, int options)
{
    int cost = mon_spell_cost_plr(spell->spell, spell->race);
    int avail = _max_cost(spell);

    if (cost > avail)
        doc_insert(doc, "<color:D>");
    else
        doc_insert(doc, "<color:w>");

    doc_printf(doc, " %c) ", I2A(choice));
    _list_spell_aux(doc, spell, options);
    doc_insert(doc, "</color>\n");
}

static mon_spell_cast_ptr _learning = NULL;
static void _list_spells(doc_ptr doc, int options)
{
    int i;

    doc_insert(doc, "<style:table>");
    doc_printf(doc, "<color:G>    %-20.20s <tab:29>Stat Lvl  SP Fail %-15.15s", "Name", "Desc");
    if (options & _SHOW_STATS)
        doc_insert(doc, "<tab:60> Cast Fail");
    doc_insert(doc, "</color>\n");

    for (i = 0; i < _MAX_SLOTS; i++)
    {
        _spell_ptr spell = _get_spell(i);
        if (spell->spell)
            _list_spell(doc, spell, i, options);
        else
        {
            if (options & _ALLOW_EMPTY)
                doc_printf(doc, " %c) <color:D>(Empty)</color>\n", I2A(i));
            else
                doc_printf(doc, " <color:D>%c) (Empty)</color>\n", I2A(i));
        }
    }

    if (options & _LEARNING)
    {
        _spell_t spell = {0};
        assert(_learning);
        spell.spell = _learning->spell;
        spell.race = _learning->race;
        doc_insert(doc, "<color:B>Learning:</color>\n    ");
        _list_spell_aux(doc, &spell, options);
        doc_newline(doc);
    }
    doc_insert(doc, "</style>");
}

static void _display(rect_t r, int options)
{
    doc_ptr doc = doc_alloc(r.cx);
    _list_spells(doc, options);
    doc_sync_term(doc, doc_range_all(doc), doc_pos_create(r.x, r.y));
    doc_free(doc);
}

void blue_mage_wizard_probe(mon_race_ptr race, doc_ptr doc)
{
    int i, j, ct = 0;
    _spell_t spell = {0};
    if (!race->spells) return;
    doc_insert(doc, "<style:table>");
    doc_printf(doc, "<color:G>    %-20.20s <tab:29>Stat Lvl  SP Fail %-15.15s</color>\n", "Name", "Desc");
    spell.race = race;
    for (i = MST_BREATH; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = race->spells->groups[i];
        if (!group) continue;
        for (j = 0; j < group->count; j++)
        {
            spell.spell = &group->spells[j];
            doc_printf(doc, " %c) ", I2A(ct++));
            _list_spell_aux(doc, &spell, _SHOW_INFO | _SHOW_GRAPHICS);
            doc_newline(doc);
        }
    }
    doc_insert(doc, "</style>");
}

static rect_t _menu_rect(void)
{
    rect_t r = ui_menu_rect();
    if (r.cx > 80)
        r.cx = 80;
    return r;
}

static _spell_ptr _choose(cptr verb, int options)
{
    _spell_ptr     result = NULL;
    int            slot = 0;
    int            cmd;
    rect_t         r = _menu_rect();
    str_ptr        prompt = NULL;
    bool           done = FALSE;
    bool           exchange = FALSE;
    int            slot1 = _INVALID_SLOT, slot2 = _INVALID_SLOT;

    if (REPEAT_PULL(&cmd))
    {
        slot = A2I(cmd);
        if (0 <= slot && slot < _MAX_SLOTS)
            return &_spells[slot];
    }

    prompt = str_alloc();
    screen_save();
    while (!done)
    {
        str_clear(prompt);

        if (exchange)
        {
            if (slot1 == _INVALID_SLOT)
                str_append_s(prompt, "Select the first spell:");
            else
                str_append_s(prompt, "Select the second spell:");
        }
        else
        {
            str_printf(prompt, "%s which spell", verb);
            if (options & _ALLOW_EXCHANGE)
                str_append_s(prompt, " [Press 'X' to Exchange]");
            str_append_c(prompt, ':');
        }
        prt(str_buffer(prompt), 0, 0);
        _display(r, options);

        cmd = inkey_special(FALSE);

        if (cmd == ESCAPE || cmd == 'q' || cmd == 'Q')
            done = TRUE;

        if (options & _ALLOW_EXCHANGE)
        {
            if (!exchange && (cmd == 'x' || cmd == 'X'))
            {
                exchange = TRUE;
                slot1 = slot2 = _INVALID_SLOT;
            }
        }

        if ('a' <= cmd && cmd < 'a' + _MAX_SLOTS)
        {
            slot = A2I(cmd);
            if (exchange)
            {
                if (slot1 == _INVALID_SLOT)
                    slot1 = slot;
                else
                {
                    slot2 = slot;
                    if (slot1 != slot2)
                    {
                        _spell_t  tmp = _spells[slot1];
                        _spells[slot1] = _spells[slot2];
                        _spells[slot2] = tmp;
                    }
                    exchange = FALSE;
                    slot1 = slot2 = _INVALID_SLOT;
                }
            }
            else
            {
                if (_spells[slot].spell || (options & _ALLOW_EMPTY))
                {
                    result = &_spells[slot];
                    done = TRUE;
                    if (result->spell && (options & _LEARNING))
                    {
                        char    c;
                        str_ptr name = str_alloc();
                        str_ptr prompt;

                        mon_spell_print(result->spell, name);
                        prompt = str_alloc_format(
                            "Really replace %s? <color:y>[y/N]</color>", str_buffer(name));

                        c = msg_prompt(str_buffer(prompt), "ny", PROMPT_DEFAULT);
                        str_free(name);
                        str_free(prompt);
                        if (c == 'n')
                        {
                            result = NULL;
                            done = FALSE;
                        }
                    }
                }
            }
        }
    }

    if (result)
    {
        REPEAT_PUSH(I2A(slot));
    }

    screen_load();
    str_free(prompt);
    return result;
}

/* Learning a spell requires the player use "Learning" power and directly observe the spell */
void _learning_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Learning");
        break;
    case SPELL_DESC:
        var_set_string(res, "This technique allows you to learn new monster spells.");
        break;
    case SPELL_CAST:
        if (plr->action == ACTION_LEARN) set_action(ACTION_NONE);
        else set_action(ACTION_LEARN);
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

static void _learn_aux(mon_spell_cast_ptr cast, _spell_ptr spell)
{
    str_ptr name = str_alloc();
    mon_spell_print(cast->spell, name);
    spell->spell = cast->spell;
    spell->race = cast->race;
    spell_stats_wipe(&spell->stats);
    msg_format("You have learned %s.", str_buffer(name));
    str_free(name);
    new_mane = TRUE; /* XXX Yuk */
    plr->redraw |= PR_STATE;
}
void blue_mage_learn(mon_spell_cast_ptr cast)
{
    _spell_ptr spell;
    int pl, ml, stun;

    if (plr->action != ACTION_LEARN) return;
    if (plr->pclass != CLASS_BLUE_MAGE) return;
    if (mon_is_pet(cast->mon)) return;
    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        return;
    }
    if (plr_tim_find(T_HALLUCINATE))
    {
        msg_print("You are hallucinating!");
        return;
    }
    if (plr_tim_find(T_BERSERK))
    {
        msg_print("You are too enraged!");
        return;
    }
    if (!plr_can_see(cast->src))
    {
        msg_print("You need to see the caster to learn this spell.");
        return;
    }
    stun = plr_tim_amount(T_STUN);
    if (stun && randint0(50) < stun)
    {
        msg_print("You are too stunned!");
        return;
    }

    if (_find(cast->spell, cast->race))
    {
        if (0 || plr->wizard)
        {
            str_ptr s = str_alloc();
            mon_spell_print(cast->spell, s);
            msg_format("<color:D>You already know %s from this race.</color>", str_buffer(s));
            str_free(s);
        }
        return;
    }

    ml = 20 + 2*cast->race->alloc.lvl;
    pl = plr->lev + plr->stat_ind[A_INT];
    if (0 || plr->wizard)
    {
        int odds; /* odds that 1dP <= 1dM; i.e. odds of failure */
        str_ptr s = str_alloc();
        mon_spell_print(cast->spell, s);
        if (pl<=ml)
            odds=(2*ml-pl+1)*1000/(2*ml);
        else
            odds=(ml+1)*1000/(2*pl);
        msg_format("<color:D>Attempting to learn %s (1d%d >= 1d%d => %d.%d%% Fail)...</color>",
            str_buffer(s), ml, pl, odds/10, odds%10);
        str_free(s);
    }
    if (randint1(ml) >= randint1(pl))
    {
        if (0 || plr->wizard)
            msg_print("<color:D>Failed.</color>");
        return;
    }

    /* When learning a new spell, it really helps to show the spell being learned
     * so the player can decide if it is worth keeping. Otherwise, they need to keep
     * a "scratch" slot free to learn into rather than risk over-writing something
     * more useful. */
    _learning = cast;
    spell = _choose("Replace", _ALLOW_EMPTY | _SHOW_INFO | _SHOW_GRAPHICS | _LEARNING);
    _learning = NULL;

    if (!spell) return;
    _learn_aux(cast, spell);
}

/* Casting Learned spells */
void blue_mage_cast(void)
{
    _spell_ptr spell = NULL;

    if (_count() == 0)
    {
        msg_print("You haven't learned any spells yet. Use the <color:keyword>Learning</color> class power to learn monster spells.");
        return;
    }
    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused.");
        return;
    }

    energy_use = 0;
    spell = _choose("Cast", _ALLOW_EXCHANGE | _SHOW_INFO | _SHOW_GRAPHICS);
    if (spell)
    {
        int cost = mon_spell_cost_plr(spell->spell, spell->race);
        int avail = _max_cost(spell);

        if (cost > avail)
        {
            /*if (spell->spell->flags & MSF_INNATE)
            {
                msg_print("Casting this spell would kill you!");
                return;
            }
            else*/ if (plr_tim_find(T_BERSERK)) /* XXX berserk allows innate spells? */
            {
                msg_format("You cannot think clearly!");
                return;
            }
            else
            {
                msg_print("You do not have enough mana to cast this spell.");
                return;
            }
        }
        energy_use = 100;
        switch (mon_spell_cast_blue_mage(spell->spell, spell->race))
        {
        case CAST_ABORT:
            energy_use = 0;
            break;
        case CAST_OK:
            spell->stats.ct_cast++;
            break;
        case CAST_FAIL:
            spell->stats.ct_fail++;
            break;
        }
    }
}

void blue_mage_browse(void)
{
    bool done = FALSE;

    screen_save();
    while (!done)
    {
        _spell_ptr spell = _choose("Browse", _ALLOW_EXCHANGE | _SHOW_INFO | _SHOW_GRAPHICS);
        if (!spell)
            done = TRUE;
    }
    screen_load();
}

/****************************************************************
 * Hooks
 ****************************************************************/
static void _birth(void)
{
    _clear_spells(); /* XXX from last character (eg. Quick Start) */
    plr_birth_obj_aux(TV_HAFTED, SV_QUARTERSTAFF, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_ROBE, 1);
    plr_birth_obj_aux(TV_WAND, EFFECT_BOLT_MISSILE, 1);
}

static void _calc_bonuses(void)
{
    #if 0
    if (plr->lev >= 30)
        plr->wizard_sight = TRUE;
    #endif
}

static void _character_dump(doc_ptr doc)
{
    doc_printf(doc, "<topic:Spells>==================================== <color:keypress>S</color>pells ===================================\n\n");
    _list_spells(doc, _SHOW_INFO | _SHOW_STATS);
    doc_newline(doc);
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = _learning_spell;

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "spell";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 400;
        me.encumbrance.weapon_pct = 50;
        me.encumbrance.enc_wgt = 800;
        me.options |= CASTER_ALLOW_DEC_MANA;
        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Class
 ****************************************************************/
plr_class_ptr blue_mage_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  35,  38,   3,  16,  20,  46,  35 };
    skills_t xs = { 35,  65,  50,   0,   0,   0,  60,  50 };

        me = plr_class_alloc(CLASS_BLUE_MAGE);
        me->name = "Blue-Mage";
        me->desc = "The Blue-Mage is not your ordinary mage. Rather than using spellbooks, "
            "the blue-mage learns monster spells through direct experience. With their special "
            "technique of <color:keyword>Learning</color> they have a chance to learn spells every "
            "time they see a monster perform a spell. The "
            "odds of learning the spell depend on the player's level and the monster's level. "
            "Moreover, when performing learned spells, the effectiveness depends upon the monster "
            "they learned the spell from; breathing fire learned from a fire hound is <color:v>much</color> "
            "less effective then the same learned from a Great Hell Wyrm! Finally, the blue-mage "
            "can only remember a limited number of spells at a time. Learning a new spell must "
            "replace an existing spell once this limit is reached.\n\n"
            "The Blue-Mage is weak in melee and archery, but not nearly so bad as a normal "
            "mage would be. This is fortunate, since it may take a little while to learn that "
            "first offensive spell. Starting players will need to use melee and archery until "
            "an arsenal of good monster spells is acquired.";
        me->stats[A_STR] = -1;
        me->stats[A_INT] =  3;
        me->stats[A_WIS] =  0;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] = -1;
        me->stats[A_CHR] =  0;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 130;
        me->pets = 35;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_powers = _get_powers;
        me->hooks.caster_info = _caster_info;
        me->hooks.character_dump = _character_dump;
        me->hooks.load_player = _load_player;
        me->hooks.save_player = _save_player;
    }

    return me;
}
