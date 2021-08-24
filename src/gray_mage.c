#include "angband.h"

#include <assert.h>

/**********************************************************************
 * The Gray Mage learn spells from books, but commits these spells
 * to memory (Gray Matter). They can only remember a certain number
 * of spells at a time, depending on level or intelligence.
 *
 * They may access multiple realms, depending on their subclass. All
 * can use Sorcery, Chaos, Trump, Arcane, Craft, and Armageddon. Good
 * allows Life and Crusade; Neutral adds Nature; and Evil adds Death
 * and Daemon.
 *
 * There is no spell proficiency system for us. Also, we can't use
 * any of the stock routines for gaining, browsing and casting spells.
 **********************************************************************/

#define _MAX_SLOTS 10
#define _INVALID_SLOT -1

#define _ALLOW_EMPTY    0x01
#define _ALLOW_EXCHANGE 0x02
#define _SHOW_INFO      0x04
#define _SHOW_STATS     0x08
#define _FROM_BOOK      0x10

static int _browse_choice = _INVALID_SLOT;

typedef struct {
    int realm;
    int spell;
} _slot_info_t, *_slot_info_ptr;

static _slot_info_t _spells[_MAX_SLOTS];

static magic_type *_get_spell_info(int realm, int spell)
{
    return &mp_ptr->info[realm - 1][spell];
}

static int _spell_fail(int realm, int spell)
{
    magic_type *spell_ptr = _get_spell_info(realm, spell);
    cptr        name = do_spell(realm, spell, SPELL_NAME);
    int         fail = virtue_mod_spell_fail(realm, spell_ptr->sfail);
    spell_stats_ptr stats = spell_stats_aux(name);

    fail = calculate_fail_rate_aux(plr->lev, realm, spell_ptr->slevel, fail, plr->stat_ind[A_INT]);
    if (stats->skill >= SPELL_EXP_EXPERT) fail--;
    if (stats->skill >= SPELL_EXP_MASTER) fail--;

    return MAX(0, fail);
}
static void _list_spell(doc_ptr doc, int realm, int spell, int choice, int options)
{
    magic_type *spell_ptr = _get_spell_info(realm, spell);
    cptr        name = do_spell(realm, spell, SPELL_NAME);
    int         cost = spell_cost_aux(realm, name, spell_ptr->smana);
    int         fail = _spell_fail(realm, spell);

    if (cost > plr->csp)
        doc_insert(doc, "<color:D>");
    else if (choice == _browse_choice)
        doc_insert(doc, "<color:B>");
    else if (spell_ptr->slevel > plr->lev)
    {
        if (options & _FROM_BOOK)
            doc_insert(doc, "<color:D>");
        else
            doc_insert(doc, "<color:y>");
    }
    else
        doc_insert(doc, "<color:w>");

    if (spell_ptr->slevel > plr->lev)
        doc_printf(doc, " <color:D>%c)</color> ", I2A(choice));
    else
        doc_printf(doc, " %c) ", I2A(choice));

    doc_printf(doc, "%-23.23s ", name);
    spell_skill_doc(spell_stats_aux(name), doc);
    doc_printf(doc, "%3d %3d %3d%% ", spell_ptr->slevel, cost, fail);

    if (spell_ptr->slevel > plr->lev)
    {
        if (options & _FROM_BOOK)
            doc_printf(doc, "%-15.15s", "");
        else
            doc_printf(doc, "%-15.15s", "Forgotten");
    }
    else if (options & _SHOW_INFO)
        doc_printf(doc, "%-15.15s", do_spell(realm, spell, SPELL_INFO));

    if (options & _SHOW_STATS)
    {
        spell_stats_ptr stats = spell_stats_old(realm, spell);
        if (stats->ct_cast + stats->ct_fail)
        {
            doc_printf(doc, " %5d %4d %3d%%",
                stats->ct_cast,
                stats->ct_fail,
                spell_stats_fail(stats)
            );
        }
    }

    doc_insert(doc, "</color>\n");
}

static void _list_spells(doc_ptr doc, int options)
{
    int i;

    doc_insert(doc, "<style:table>");
    doc_printf(doc, "<color:G>    %-23.23s Skill Lvl  SP Fail %-15.15s", "Name", "Desc");
    if (options & _SHOW_STATS)
        doc_insert(doc, "  Cast Fail");
    doc_insert(doc, "</color>\n");

    for (i = 0; i < _MAX_SLOTS; i++)
    {
        if (_spells[i].realm != REALM_NONE)
            _list_spell(doc, _spells[i].realm, _spells[i].spell, i, options);
        else
        {
            if (options & _ALLOW_EMPTY)
                doc_printf(doc, " %c) <color:D>(Empty)</color>\n", I2A(i));
            else
                doc_printf(doc, " <color:D>%c) (Empty)</color>\n", I2A(i));
        }
    }
    doc_insert(doc, "</style>");

    if (_browse_choice != -1 && _spells[_browse_choice].realm != REALM_NONE)
    {
        doc_newline(doc);
        doc_printf(doc, "    <indent>%s</indent>\n\n",
            do_spell(_spells[_browse_choice].realm, _spells[_browse_choice].spell, SPELL_DESC));
    }
}

static void _display(rect_t r, int options)
{
    doc_ptr doc = doc_alloc(r.cx);
    _list_spells(doc, options);
    doc_sync_term(doc, doc_range_all(doc), doc_pos_create(r.x, r.y));
    doc_free(doc);
}

static rect_t _menu_rect(void)
{
    rect_t r = ui_menu_rect();
    if (r.cx > 80)
        r.cx = 80;
    return r;
}

static _slot_info_ptr _choose(cptr verb, int options)
{
    _slot_info_ptr result = NULL;
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
                        _slot_info_t  tmp = _spells[slot1];
                        _spells[slot1] = _spells[slot2];
                        _spells[slot2] = tmp;
                    }
                    exchange = FALSE;
                    slot1 = slot2 = _INVALID_SLOT;
                }
            }
            else
            {
                if (_spells[slot].realm != REALM_NONE || (options & _ALLOW_EMPTY))
                {
                    result = &_spells[slot];
                    done = TRUE;
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

/**********************************************************************
 * Birth
 **********************************************************************/
static void _birth(void)
{
    object_type forge;
    int i;

    object_prep(&forge, lookup_kind(TV_SWORD, SV_DAGGER));
    plr_birth_obj(&forge);

    if (plr->psubclass == GRAY_MAGE_GOOD)
    {
        object_prep(&forge, lookup_kind(TV_LIFE_BOOK, 0));
        plr_birth_obj(&forge);

        object_prep(&forge, lookup_kind(TV_CRUSADE_BOOK, 0));
        plr_birth_obj(&forge);
    }
    else if (plr->psubclass == GRAY_MAGE_NEUTRAL)
    {
        object_prep(&forge, lookup_kind(TV_NATURE_BOOK, 0));
        plr_birth_obj(&forge);

        object_prep(&forge, lookup_kind(TV_CHAOS_BOOK, 0));
        plr_birth_obj(&forge);
    }
    else if (plr->psubclass == GRAY_MAGE_EVIL)
    {
        object_prep(&forge, lookup_kind(TV_DEATH_BOOK, 0));
        plr_birth_obj(&forge);

        object_prep(&forge, lookup_kind(TV_DAEMON_BOOK, 0));
        plr_birth_obj(&forge);
    }

    object_prep(&forge, lookup_kind(TV_ARCANE_BOOK, 0));
    plr_birth_obj(&forge);

    /* Restart? player_wipe doesn't know about this stuff, of course ... */
    for (i = 0; i < _MAX_SLOTS; i++)
    {
        _spells[i].realm = REALM_NONE;
        _spells[i].spell = 0;
    }
}

/**********************************************************************
 * Private Helpers
 **********************************************************************/
static void _calc_bonuses(void)
{
    if (plr->lev >= 30)
        plr->wizard_sight = TRUE;
}

static void _character_dump(doc_ptr doc)
{
    doc_printf(doc, "<topic:Spells>==================================== <color:keypress>S</color>pells ===================================\n\n");
    _list_spells(doc, _SHOW_INFO | _SHOW_STATS);
    doc_newline(doc);
}

static void _load_player(savefile_ptr file)
{
    int i;
    for (i = 0; i < _MAX_SLOTS; i++)
    {
        _spells[i].realm = REALM_NONE;
        _spells[i].spell = 0;
    }

    while (1)
    {
        i = savefile_read_u16b(file);
        if (i == 0xFFFF) break;
        assert(0 <= i && i < _MAX_SLOTS);
        _spells[i].realm = savefile_read_byte(file);
        _spells[i].spell = savefile_read_byte(file);
    }
}

static void _save_player(savefile_ptr file)
{
    int i;
    for (i = 0; i < _MAX_SLOTS; i++)
    {
        if (_spells[i].realm != REALM_NONE)
        {
            savefile_write_u16b(file, (u16b)i);
            savefile_write_byte(file, _spells[i].realm); /* 1 to 12 */
            savefile_write_byte(file, _spells[i].spell); /* 0 to 31 */
        }
    }
    savefile_write_u16b(file, 0xFFFF); /* sentinel */
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 1;
    spell->fail = calculate_fail_rate(spell->level, 90, plr->stat_ind[A_INT]);
    spell->fn = eat_magic_spell;

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
        me.encumbrance.max_wgt = 430;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 600;
        me.options = CASTER_ALLOW_DEC_MANA | CASTER_GLOVE_ENCUMBRANCE | CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}

static bool _is_allowed_realm(int realm)
{
    switch (realm)
    {
    case REALM_LIFE:
    case REALM_CRUSADE:
        return plr->psubclass == GRAY_MAGE_GOOD;

    case REALM_NATURE:
    case REALM_CHAOS:
        return plr->psubclass == GRAY_MAGE_NEUTRAL;

    case REALM_DEATH:
    case REALM_DAEMON:
        return plr->psubclass == GRAY_MAGE_EVIL;

    case REALM_SORCERY:
    case REALM_TRUMP:
    case REALM_ARCANE:
    case REALM_CRAFT:
    case REALM_ILLUSION:
        return TRUE;

    case REALM_ARMAGEDDON:
        /* XXX Consider removing access to this realm to make the 
         * primary realm choice more meaningful. Currently, gray-mages
         * cast armageddon spells about as well as a priest. XXX */
        return TRUE;
    }
    return FALSE;
}

static bool _is_spellbook(int tval)
{
    if (tval < TV_LIFE_BOOK || tval > TV_BURGLARY_BOOK) return FALSE;
    return TRUE;
}

bool gray_mage_is_allowed_book(int tval, int sval) /* For autopick.c */
{
    if (!_is_spellbook(tval)) return FALSE;
    return _is_allowed_realm(tval2realm(tval));
}

static bool _spell_book_p(object_type *o_ptr)
{
    if (!_is_spellbook(o_ptr->tval)) return FALSE;
    return gray_mage_is_allowed_book(o_ptr->tval, o_ptr->sval);
}

/* cmd5.c get_spell() was blowing up when I attempted code reuse ...
   so roll our own (much simpler) version */
#define _SPELLS_PER_BOOK 8
static void _display_spells_to_gain(object_type *o_ptr, rect_t r)
{
    doc_ptr doc = doc_alloc(r.cx);
    int     i;
    int     realm = tval2realm(o_ptr->tval);
    int     start_idx = o_ptr->sval * _SPELLS_PER_BOOK;

    doc_insert(doc, "<style:table>");
    doc_printf(doc, "<color:G>    %-23.23s Skill Lvl  SP Fail Desc</color>\n", "Name");

    for (i = start_idx; i < start_idx + _SPELLS_PER_BOOK; i++)
        _list_spell(doc, realm, i, i - start_idx, _FROM_BOOK);

    doc_insert(doc, "</style>");

    doc_sync_term(doc, doc_range_all(doc), doc_pos_create(r.x, r.y));
    doc_free(doc);
}

static int _choose_spell_to_gain(object_type *o_ptr)
{
    rect_t r = _menu_rect();
    int    result = -1;
    int    cmd;
    bool   done = FALSE;

    screen_save();
    while (!done)
    {
        prt("Memorize which spell?", 0, 0);
        _display_spells_to_gain(o_ptr, r);

        cmd = inkey_special(FALSE);

        if (cmd == ESCAPE || cmd == 'q' || cmd == 'Q')
            done = TRUE;

        if ('a' <= cmd && cmd < 'a' + _SPELLS_PER_BOOK)
        {
            int         spell_idx = o_ptr->sval * _SPELLS_PER_BOOK + A2I(cmd);
            magic_type *spell_ptr = _get_spell_info(tval2realm(o_ptr->tval), spell_idx);

            if (spell_ptr->slevel <= plr->lev) /* Note: Illegible spells have slevel == 99 in m_info.txt */
            {
                done = TRUE;
                result = spell_idx;
            }
        }
    }
    screen_load();
    return result;
}

/**********************************************************************
 * Public
 **********************************************************************/
void gray_mage_browse(void)
{
    bool done = FALSE;

    screen_save();
    _browse_choice = 0;
    while (!done)
    {
        _slot_info_ptr slot = _choose("Browse", _ALLOW_EXCHANGE | _SHOW_INFO | _SHOW_STATS);
        if (!slot)
            done = TRUE;
        else
            _browse_choice = slot - _spells;
    }
    _browse_choice = -1;
    screen_load();
}

void gray_mage_cast(void)
{
    _slot_info_ptr slot_ptr;

    /* Blind is OK!!! */

    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        return;
    }

    slot_ptr = _choose("Cast", _ALLOW_EXCHANGE | _SHOW_INFO);
    if (slot_ptr)
    {
        magic_type *spell_ptr = _get_spell_info(slot_ptr->realm, slot_ptr->spell);
        cptr        name = do_spell(slot_ptr->realm, slot_ptr->spell, SPELL_NAME);
        int         cost = spell_cost_aux(slot_ptr->realm, name, spell_ptr->smana);
        int         fail = _spell_fail(slot_ptr->realm, slot_ptr->spell);
        spell_stats_ptr stats = spell_stats_aux(name);

        if (spell_ptr->slevel > plr->lev) /* Experience Drain? */
        {
            msg_format("You need to be level %d to use that spell.", spell_ptr->slevel);
            return;
        }

        if (cost > plr->csp)
        {
            msg_print("You do not have enough mana to cast this spell.");
            return;
        }

        plr->csp -= cost;
        energy_use = 100;

        if (randint0(100) < fail)
        {
            if (flush_failure) flush();

            cmsg_format(TERM_VIOLET, "You failed to cast %s!", do_spell(slot_ptr->realm, slot_ptr->spell, SPELL_NAME));
            if (demigod_is_(DEMIGOD_ATHENA))
                plr->csp += cost/2;
            spell_stats_on_fail_old(slot_ptr->realm, slot_ptr->spell);
            sound(SOUND_FAIL);
            do_spell(slot_ptr->realm, slot_ptr->spell, SPELL_FAIL);
            virtue_on_fail_spell(slot_ptr->realm, fail);
        }
        else
        {
            if (!do_spell(slot_ptr->realm, slot_ptr->spell, SPELL_CAST))
            {  /* Canceled */
                plr->csp += cost;
                energy_use = 0;
                return;
            }
            sound(SOUND_ZAP);
            if (!stats->ct_cast)
                virtue_on_first_cast_spell(slot_ptr->realm);
            stats->ct_cast++;
            spell_stats_gain_skill_aux(name, spell_ptr->slevel);
            virtue_on_cast_spell(slot_ptr->realm, cost, fail);
        }
    }
    plr->redraw |= PR_MANA;
    plr->window |= PW_SPELL;
}

void gray_mage_gain_spell(void)
{
    obj_prompt_t    prompt = {0};
    int             spell_idx;
    _slot_info_ptr  slot_ptr;

    if (plr_tim_find(T_BLIND) || no_light())
    {
        msg_print("You cannot see!");
        return;
    }

    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        return;
    }

    if (!plr->new_spells)
    {
        msg_print("You cannot learn any new spells!");
        return;
    }

    prompt.prompt = "Study which book?";
    prompt.error = "You have no books that you can read.";
    prompt.filter = _spell_book_p;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return;

    /* Pick a spell to learn */
    spell_idx = _choose_spell_to_gain(prompt.obj);
    if (spell_idx == -1) return;

    /* Pick a slot for storage (possibly replacing an already learned spell) */
    slot_ptr = _choose("Replace", _ALLOW_EMPTY | _SHOW_INFO);
    if (!slot_ptr) return;

    if (slot_ptr->realm != REALM_NONE)
    {
        char    c;
        str_ptr prompt = str_alloc_format(
            "Really replace %s? <color:y>[y/N]</color>",
            do_spell(slot_ptr->realm, slot_ptr->spell, SPELL_NAME));

        c = msg_prompt(str_buffer(prompt), "ny", PROMPT_DEFAULT);
        str_free(prompt);
        if (c == 'n') return;
    }

    /* Learn the spell: Note, we don't bother with spell_learned# and spell_order[], since
       these are hard coded for 2 spell realms. Hopefully, ticking up learned_spells is enough? */
    plr->learned_spells++;
    slot_ptr->realm = tval2realm(prompt.obj->tval);
    slot_ptr->spell = spell_idx;
    msg_format("You have learned the spell '%s'.", do_spell(slot_ptr->realm, slot_ptr->spell, SPELL_NAME));
    switch (slot_ptr->realm)
    {
    case REALM_LIFE:
        virtue_add(VIRTUE_FAITH, 1);
        break;
    case REALM_DEATH:
        virtue_add(VIRTUE_UNLIFE, 1);
        break;
    case REALM_NATURE:
        virtue_add(VIRTUE_NATURE, 1);
        break;
    default:
        virtue_add(VIRTUE_KNOWLEDGE, 1);
        break;
    }
    plr->update |= PU_SPELLS;
    plr->redraw |= PR_EFFECTS;
    energy_use = 100;
}

extern cptr gray_mage_speciality_name(int psubclass)
{
    switch (psubclass)
    {
    case GRAY_MAGE_GOOD: return "Good Bias";
    case GRAY_MAGE_NEUTRAL: return "Neutral Bias";
    case GRAY_MAGE_EVIL: return "Evil Bias";
    }
    return "";
}

extern cptr gray_mage_speciality_desc(int psubclass)
{
    switch (psubclass)
    {
    case GRAY_MAGE_GOOD: return "You gain access to Life and Crusade magic.";
    case GRAY_MAGE_NEUTRAL: return "You gain access to Nature magic.";
    case GRAY_MAGE_EVIL: return "You gain access to Daemon and Death magic.";
    }
    return "";
}

plr_class_ptr gray_mage_get_class(int psubclass)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  40,  38,   3,  16,  20,  34,  20};
    skills_t xs = { 35,  75,  55,   0,   0,   0,  30,  35};

        me = plr_class_alloc(CLASS_GRAY_MAGE);
        me->name = "Gray-Mage";
        me->desc = "The Gray-Mage casts spells from memory, rather than from a "
                    "book.  Indeed, the spell book is only required for the initial learning process. "
                    "However, only a small number of spells may be learned at "
                    "any given time, and while the Gray-Mage may replace old spells "
                    "with new ones, they can only learn a fixed number of total spells "
                    "(based upon Intelligence and Experience).\n \n"
                    "The Gray-Mage does not choose spell realms the way an ordinary "
                    "spell caster would. Instead, they choose a general bias towards "
                    "one of Good, Neutral or Evil magic. So while all Gray-Mages may "
                    "learn spells from the Arcane, Armageddon, Craft, Sorcery and "
                    "Trump realms, only a Good Bias allows access to Life and Crusade magic; "
                    "only a Neutral Bias allows access to Nature and Chaos magic; and only an Evil Bias "
                    "allows access to Death and Daemon magic. Still, it is obvious that "
                    "the Gray Mage will have an extremely large pool of spells from which "
                    "to choose. Like the Mage, Intelligence is the key stat.";

        me->stats[A_STR] = -4;
        me->stats[A_INT] =  3;
        me->stats[A_WIS] =  0;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] = -2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 95;
        me->base_hp = 0;
        me->exp = 130;
        me->pets = 30;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG | CLASS_MAGE_BONUS;

        me->hooks.caster_info = _caster_info;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.character_dump = _character_dump;
        me->hooks.get_powers = _get_powers;
        me->hooks.birth = _birth;

        me->hooks.load_player = _load_player;
        me->hooks.save_player = _save_player;
    }
    me->subid = psubclass;
    me->subname = gray_mage_speciality_name(psubclass);
    me->subdesc = gray_mage_speciality_desc(psubclass);
    return me;
}
