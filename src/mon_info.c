/* Display Monster Lore to the User
   Adapted from the ancient roff_aux() but redesigned for a more
   readable display; a 'Monster Sheet' if you will. */

#include "angband.h"

#include <stdlib.h>
#include <assert.h>

extern void mon_display(monster_race *r_ptr);
extern void mon_display_rect(monster_race *r_ptr, rect_t display);
extern void mon_display_doc(monster_race *r_ptr, doc_ptr doc);
extern void mon_display_possessor(monster_race *r_ptr, doc_ptr doc);
static bool _possessor_hack = FALSE;

static void _display_basic(monster_race *r_ptr, doc_ptr doc);
static void _display_resists(monster_race *r_ptr, doc_ptr doc);
static void _display_spells(monster_race *r_ptr, doc_ptr doc);
static void _display_attacks(monster_race *r_ptr, doc_ptr doc);
static void _display_other(monster_race *r_ptr, doc_ptr doc);
static void _display_kills(monster_race *r_ptr, doc_ptr doc);
static void _display_desc(monster_race *r_ptr, doc_ptr doc);

static void _print_list(vec_ptr v, doc_ptr doc, char sep, char term);
static string_ptr _get_res_name(int res);

/**************************************************************************
 * Helpers
 **************************************************************************/
static void _print_list(vec_ptr v, doc_ptr doc, char sep, char term)
{
    int ct = vec_length(v);
    int i;
    for (i = 0; i < ct; i++)
    {
        string_ptr s = vec_get(v, i);
        if (i < ct - 1 && sep)
            doc_printf(doc, "%s%c ", string_buffer(s), sep);
        else if (i == ct - 1 && term)
            doc_printf(doc, "%s%c", string_buffer(s), term);
        else
            doc_insert(doc, string_buffer(s));
    }
}

static string_ptr _get_res_name(int res)
{
    return string_alloc_format(
        "<color:%c>%s</color>",
        attr_to_attr_char(res_color(res)),
        res_name(res)
    );
}

/**************************************************************************
 * Basic Info
 **************************************************************************/
static char _speed_color(int speed)
{
    if (speed >= 30) return 'r';
    else if (speed >= 20) return 'o';
    else if (speed >= 15) return 'u';
    else if (speed >= 10) return 'R';
    else if (speed >= 1) return 'U';
    else if (speed == 0) return 'w';
    else return 'G';
}
static void _display_level(monster_race *r_ptr, doc_ptr doc)
{
    doc_insert(doc, "Level   : ");
    if (r_ptr->level == 0)
        doc_insert(doc, "<color:G>Town</color>");
    else
    {
        if (r_ptr->max_level != 999)
            doc_printf(doc, "<color:G>%d to %d</color>", r_ptr->level, r_ptr->max_level);
        else
            doc_printf(doc, "<color:G>%d</color>", r_ptr->level);
    }
    doc_newline(doc);
    if (spoiler_hack)
        doc_printf(doc, "Rarity  : <color:G>%d</color>\n", r_ptr->rarity);
}
static void _display_ac(monster_race *r_ptr, doc_ptr doc)
{
    doc_insert(doc, "AC      : ");
    doc_printf(doc, "<color:G>%d</color>", r_ptr->ac);
    doc_newline(doc);
}
static void _display_hp(monster_race *r_ptr, doc_ptr doc)
{
    doc_insert(doc, "HP      : ");

    if ((r_ptr->flags1 & RF1_FORCE_MAXHP) || r_ptr->hside == 1)
    {
        int hp = r_ptr->hdice * r_ptr->hside;
        doc_printf(doc, "<color:G>%d</color>", hp);
    }
    else
    {
        doc_printf(doc, "<color:G>%dd%d</color>", r_ptr->hdice, r_ptr->hside);
    }
    
    doc_newline(doc);
}
static void _display_speed(monster_race *r_ptr, doc_ptr doc)
{                        /* v~~~~~~byte */
    int speed = r_ptr->speed - 110;
    int rand = 0;
    if (effective_speed) doc_printf(doc, "Speed: <color:%c>%d.%dx</color>", _speed_color(speed), SPEED_TO_ENERGY(r_ptr->speed) / 10, SPEED_TO_ENERGY(r_ptr->speed) % 10);
    else doc_printf(doc, "Speed: <color:%c>%+d</color>", _speed_color(speed), speed);

    if (r_ptr->flags1 & RF1_RAND_50) rand += 50;
    if (r_ptr->flags1 & RF1_RAND_25) rand += 25;
    if (rand == 75) doc_insert(doc, " <color:r>Extremely Erratic</color>");
    else if (rand == 50) doc_insert(doc, " <color:R>Somewhat Erratic</color>");
    else if (rand == 25) doc_insert(doc, " <color:o>A Bit Erratic</color>");

    if (r_ptr->flags1 & RF1_NEVER_MOVE) doc_insert(doc, ", <color:u>Stationary</color>");

    doc_newline(doc);
}
static void _display_alertness(monster_race *r_ptr, doc_ptr doc)
{
    doc_insert(doc, "Alert: ");
    if (r_ptr->sleep > 200)
        doc_insert(doc, "<color:D>Ignores Intruders</color>");
    else if (r_ptr->sleep > 95)
        doc_insert(doc, "<color:w>Very Inattentive</color>");
    else if (r_ptr->sleep > 75)
        doc_insert(doc, "<color:W>Inattentive</color>");
    else if (r_ptr->sleep > 45)
        doc_insert(doc, "<color:U>Overlooks</color>");
    else if (r_ptr->sleep > 25)
        doc_insert(doc, "<color:y>Unseeing</color>");
    else if (r_ptr->sleep > 10)
        doc_insert(doc, "<color:y>Fairly Unseeing</color>");
    else if (r_ptr->sleep > 5)
        doc_insert(doc, "<color:o>Fairly Observant</color>");
    else if (r_ptr->sleep > 3)
        doc_insert(doc, "<color:R>Observant</color>");
    else if (r_ptr->sleep > 1)
        doc_insert(doc, "<color:r>Very Observant</color>");
    else if (r_ptr->sleep > 0)
        doc_insert(doc, "<color:r>Vigilant</color>");
    else
        doc_insert(doc, "<color:v>Ever Vigilant</color>");
    doc_printf(doc, " <color:G>(%d')</color>\n", 10 * r_ptr->aaf);
}
static void _display_type(monster_race *r_ptr, doc_ptr doc)
{
    vec_ptr v = vec_alloc((vec_free_f)string_free);
    doc_insert(doc, "Type : <indent><style:indent>");

    if (r_ptr->flags2 & RF2_ELDRITCH_HORROR)
        vec_add(v, string_copy_s("<color:v>Sanity Blasting</color>"));
    if (r_ptr->flags3 & RF3_ANIMAL)
        vec_add(v, string_copy_s("<color:G>Natural</color>"));
    if (r_ptr->flags3 & RF3_EVIL)
        vec_add(v, string_copy_s("<color:D>Evil</color>"));
    if (r_ptr->flags3 & RF3_GOOD)
        vec_add(v, string_copy_s("<color:y>Good</color>"));
    if (r_ptr->flags3 & RF3_UNDEAD)
        vec_add(v, string_copy_s("<color:v>Undead</color>"));
    if (r_ptr->flags3 & RF3_NONLIVING)
        vec_add(v, string_copy_s("<color:U>Nonliving</color>"));
    if (r_ptr->flags3 & RF3_AMBERITE)
        vec_add(v, string_copy_s("<color:v>Amberite</color>"));
    if (r_ptr->flags3 & RF3_DRAGON)
        vec_add(v, string_copy_s("<color:o>Dragon</color>"));
    if (r_ptr->flags3 & RF3_DEMON)
        vec_add(v, string_copy_s("<color:v>Demon</color>"));
    if (r_ptr->flags3 & RF3_GIANT)
        vec_add(v, string_copy_s("<color:U>Giant</color>"));
    if (r_ptr->flags3 & RF3_TROLL)
        vec_add(v, string_copy_s("<color:B>Troll</color>"));
    if (r_ptr->flags3 & RF3_ORC)
        vec_add(v, string_copy_s("<color:u>Orc</color>"));
    if (r_ptr->flags2 & RF2_HUMAN)
        vec_add(v, string_copy_s("<color:W>Human</color>"));
    if (r_ptr->flags2 & RF2_THIEF)
        vec_add(v, string_copy_s("<color:D>Thief</color>"));
    /*if (r_ptr->flags2 & RF2_QUANTUM)
        vec_add(v, string_copy_s("<color:v>Quantum</color>"));*/
    if (r_ptr->flags1 & RF1_MALE)
        vec_add(v, string_copy_s("<color:b>Male</color>"));
    if (r_ptr->flags1 & RF1_FEMALE)
        vec_add(v, string_copy_s("<color:R>Female</color>")); /* Pink? */
    if (p_ptr->pclass == CLASS_WARLOCK && warlock_is_pact_monster(r_ptr))
        vec_add(v, string_copy_s("<color:v>Pact</color>"));

    _print_list(v, doc, ',', '\0');
    vec_free(v);
    doc_insert(doc, "</style></indent>\n");
}
static void _display_basic(monster_race *r_ptr, doc_ptr doc)
{
    doc_printf(doc, "Name    : <indent><style:indent><color:B>%s</color> ", r_name + r_ptr->name);
    assert(r_ptr->d_char);
    doc_printf(doc, "(<color:%c>%c</color>", attr_to_attr_char(r_ptr->d_attr), r_ptr->d_char);
    if (use_graphics && (r_ptr->x_char != r_ptr->d_char || r_ptr->x_attr != r_ptr->d_attr))
    {
        doc_insert(doc, " / ");
        doc_insert_char(doc, r_ptr->x_attr, r_ptr->x_char);
    }
    doc_insert(doc, ")</style></indent>\n");

    {
        doc_ptr cols[2];

        cols[0] = doc_alloc(20);
        cols[1] = doc_alloc(MAX(20, MIN(50, doc_width(doc) - 20))); /* Monster Recall Terminal */

        /* Column 1 */
        _display_level(r_ptr, cols[0]);
        _display_ac(r_ptr, cols[0]);
        _display_hp(r_ptr, cols[0]);

        /* Column 2 */
        _display_speed(r_ptr, cols[1]);
        if (!_possessor_hack) _display_alertness(r_ptr, cols[1]);
        _display_type(r_ptr, cols[1]);

        doc_insert_cols(doc, cols, 2, 0);

        doc_free(cols[0]);
        doc_free(cols[1]);
    }
}

/**************************************************************************
 * Resists
 **************************************************************************/
static void _display_resists(monster_race *r_ptr, doc_ptr doc)
{
    int        i;
    int        ct = 0;
    vec_ptr    v = vec_alloc((vec_free_f)string_free);
    const int  flags[RES_MAX] = {
        RFR_RES_ACID, RFR_RES_ELEC, RFR_RES_FIRE, RFR_RES_COLD, RFR_RES_POIS,
        RFR_RES_LITE, RFR_RES_DARK, -1, RFR_RES_NETH, RFR_RES_NEXU, RFR_RES_SOUN,
        RFR_RES_SHAR, RFR_RES_CHAO, RFR_RES_DISE, RFR_RES_TIME, -1, -1, -1};

    for (i = 0; i < RES_MAX; i++)
    {
        int which = flags[i];
        if (which >= 0 && (r_ptr->flagsr & which))
            vec_add(v, _get_res_name(i));
    }
    if ((r_ptr->flagsr & RFR_RES_TELE) && !(r_ptr->flags1 & RF1_UNIQUE) && !(r_ptr->flagsr & RFR_RES_ALL))
        vec_add(v, string_copy_s("<color:o>Teleportation</color>"));
    if (r_ptr->flagsr & RFR_RES_WATE)
        vec_add(v, string_copy_s("<color:b>Water</color>"));
    if (r_ptr->flagsr & RFR_RES_PLAS)
        vec_add(v, string_copy_s("<color:R>Plasma</color>"));
    if (r_ptr->flagsr & RFR_RES_WALL)
        vec_add(v, string_copy_s("<color:u>Force</color>"));
    if (r_ptr->flagsr & RFR_RES_GRAV)
        vec_add(v, string_copy_s("<color:s>Gravity</color>"));

    if (vec_length(v))
    {
        doc_insert(doc, "Resist  : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Immunities */
    vec_clear(v);
    if (r_ptr->flagsr & RFR_RES_ALL)
    {
        vec_add(v, string_copy_s("<color:y>Everything</color>"));
    }
    if (r_ptr->flagsr & RFR_IM_ACID)
        vec_add(v, _get_res_name(RES_ACID));
    if (r_ptr->flagsr & RFR_IM_ELEC)
        vec_add(v, _get_res_name(RES_ELEC));
    if (r_ptr->flagsr & RFR_IM_FIRE)
        vec_add(v, _get_res_name(RES_FIRE));
    if (r_ptr->flagsr & RFR_IM_COLD)
        vec_add(v, _get_res_name(RES_COLD));
    if (r_ptr->flagsr & RFR_IM_POIS)
        vec_add(v, _get_res_name(RES_POIS));
    if (r_ptr->flags3 & RF3_NO_FEAR)
        vec_add(v, string_copy_s("<color:s>Fear</color>"));
    if (r_ptr->flags3 & RF3_NO_STUN)
        vec_add(v, string_copy_s("<color:o>Stunning</color>"));
    if (r_ptr->flags3 & RF3_NO_CONF)
        vec_add(v, string_copy_s("<color:U>Confusion</color>"));
    if (r_ptr->flags3 & RF3_NO_SLEEP)
        vec_add(v, string_copy_s("<color:b>Sleep</color>"));
    if ((r_ptr->flagsr & RFR_RES_TELE) && ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flagsr & RFR_RES_ALL)))
        vec_add(v, string_copy_s("<color:o>Teleportation</color>"));

    if (vec_length(v))
    {
        doc_insert(doc, "Immune  : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Vulnerabilities */
    vec_clear(v);
    if (r_ptr->flags3 & RF3_HURT_FIRE)
        vec_add(v, _get_res_name(RES_FIRE));
    if (r_ptr->flags3 & RF3_HURT_COLD)
        vec_add(v, _get_res_name(RES_COLD));
    if (r_ptr->flags3 & RF3_HURT_LITE)
        vec_add(v, _get_res_name(RES_LITE));
    if (r_ptr->flags3 & RF3_HURT_ROCK)
        vec_add(v, string_copy_s("<color:u>Rock Remover</color>"));

    if (vec_length(v))
    {
        doc_insert(doc, "Vuln    : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    if (ct) doc_newline(doc);
    vec_free(v);
}

/**************************************************************************
 * Spells
 **************************************************************************/
static void _display_frequency(monster_race *r_ptr, doc_ptr doc)
{
    int pct = 0;
    assert(r_ptr->spells);
    pct = r_ptr->spells->freq * 100;
    
    if (pct)
    {
        vec_ptr v = vec_alloc((vec_free_f)string_free);

        doc_printf(doc, "Spells  : <indent><color:G>%d.%02d%%</color> ", pct/100, pct%100);
        if (!spoiler_hack && r_ptr->r_spell_turns + r_ptr->r_move_turns > 0)
            doc_printf(doc, "(%d of %d moves) ", r_ptr->r_spell_turns, r_ptr->r_spell_turns + r_ptr->r_move_turns);

        if (r_ptr->flags2 & RF2_SMART)
            vec_add(v, string_copy_s("<color:y>Intelligent</color>"));
        _print_list(v, doc, ',', '\0');
        vec_free(v);
        doc_insert(doc, "</indent>");
    }
    else
    {
        doc_printf(doc, "Spells  : <color:y>?%%</color>");
    }
    doc_newline(doc);
}
static bool _is_attack_spell(mon_spell_ptr spell)
{
    switch (spell->id.type)
    {
    case MST_BREATH: case MST_BALL: case MST_BOLT: case MST_BEAM:
    case MST_CURSE: return TRUE;
    }
    return FALSE;
}
static void _display_spell_group(mon_race_ptr r_ptr, mon_spell_group_ptr group, vec_ptr v)
{
    int i;
    if (!group) return;
    for (i = 0; i < group->count; i++)
    {
        mon_spell_ptr spell = &group->spells[i];
        string_ptr s = string_alloc();
        mon_spell_display(spell, s);

        if (spell->parm.tag && spell->id.type != MST_SUMMON)
        {
            string_append_c(s, ' ');
            string_append_c(s, '(');
            if (spoiler_hack || !_is_attack_spell(spell))
            {
                mon_spell_parm_print(&spell->parm, s, r_ptr);
                if (spell->id.type == MST_CURSE && spell->id.effect == GF_HAND_DOOM)
                    string_append_c(s, '%');
            }
            else
                string_printf(s, "%d", mon_spell_avg_dam(spell, r_ptr, !_possessor_hack));
            if (!spoiler_hack && !_possessor_hack && spell->lore) /* XXX stop repeating yourself! */
                string_printf(s, ", %dx", spell->lore);
            string_append_c(s, ')');
        }
        else if (!spoiler_hack && !_possessor_hack && spell->lore) /* XXX stop repeating yourself! */
            string_printf(s, " (%dx)", spell->lore);
        vec_add(v, s);
    }
}
static void _display_spells(monster_race *r_ptr, doc_ptr doc)
{
    int            ct = 0;
    vec_ptr        v = vec_alloc((vec_free_f)string_free);
    mon_spells_ptr spells = r_ptr->spells;

	if (spells)
	{
		assert(spells);
		if (!_possessor_hack) _display_frequency(r_ptr, doc);

		/* Breaths */
		_display_spell_group(r_ptr, spells->groups[MST_BREATH], v);
		if (vec_length(v))
		{
			doc_insert(doc, "Breathe : <indent><style:indent>");
			_print_list(v, doc, ',', '\0');
			doc_insert(doc, "</style></indent>\n");
			ct += vec_length(v);
		}

		/* Offense */
		vec_clear(v);
		_display_spell_group(r_ptr, spells->groups[MST_BALL], v);
		_display_spell_group(r_ptr, spells->groups[MST_BOLT], v);
		_display_spell_group(r_ptr, spells->groups[MST_BEAM], v);
		_display_spell_group(r_ptr, spells->groups[MST_CURSE], v);
		if (vec_length(v))
		{
			doc_insert(doc, "Offense : <indent><style:indent>");
			_print_list(v, doc, ',', '\0');
			doc_insert(doc, "</style></indent>\n");
			ct += vec_length(v);
		}

		/* Annoy */
		vec_clear(v);
		_display_spell_group(r_ptr, spells->groups[MST_ANNOY], v);
		if (vec_length(v))
		{
			doc_insert(doc, "Annoy   : <indent><style:indent>");
			_print_list(v, doc, ',', '\0');
			doc_insert(doc, "</style></indent>\n");
			ct += vec_length(v);
		}

		/* Defense */
		vec_clear(v);
		_display_spell_group(r_ptr, spells->groups[MST_BUFF], v);
		_display_spell_group(r_ptr, spells->groups[MST_BIFF], v);
		_display_spell_group(r_ptr, spells->groups[MST_HEAL], v);
		_display_spell_group(r_ptr, spells->groups[MST_ESCAPE], v);
		_display_spell_group(r_ptr, spells->groups[MST_TACTIC], v);
		_display_spell_group(r_ptr, spells->groups[MST_WEIRD], v);
		if (vec_length(v))
		{
			doc_insert(doc, "Defense : <indent><style:indent>");
			_print_list(v, doc, ',', '\0');
			doc_insert(doc, "</style></indent>\n");
			ct += vec_length(v);
		}

		/* Summoning */
		vec_clear(v);
		_display_spell_group(r_ptr, spells->groups[MST_SUMMON], v);
		if (vec_length(v))
		{
			doc_insert(doc, "Summon  : <indent><style:indent>");
			_print_list(v, doc, ',', '\0');
			doc_insert(doc, "</style></indent>\n");
			ct += vec_length(v);
		}

	}

    if (ct) doc_newline(doc);
    vec_free(v);
}

/**************************************************************************
 * Attacks
 **************************************************************************/
static cptr _method_desc(int method)
{
    switch (method)
    {
    case RBM_HIT:     return "Hit";
    case RBM_TOUCH:   return "Touch";
    case RBM_PUNCH:   return "Punch";
    case RBM_KICK:    return "Kick";
    case RBM_CLAW:    return "Claw";
    case RBM_BITE:    return "Bite";
    case RBM_STING:   return "Sting";
    case RBM_SLASH:   return "Slash";
    case RBM_BUTT:    return "Butt";
    case RBM_CRUSH:   return "Crush";
    case RBM_ENGULF:  return "Engulf";
    case RBM_CHARGE:  return "Charge";
    case RBM_CRAWL:   return "Crawl";
    case RBM_DROOL:   return "Drool";
    case RBM_SPIT:    return "Spit";
    case RBM_EXPLODE: return "Explode";
    case RBM_GAZE:    return "Gaze";
    case RBM_WAIL:    return "Wail";
    case RBM_SPORE:   return "Spores";
    case RBM_BEG:     return "Beg";
    case RBM_INSULT:  return "Insult";
    case RBM_MOAN:    return "Moan";
    case RBM_SHOW:    return "Sing";
    }
    return "Weird";
}
static string_ptr _effect_desc(mon_race_ptr race, mon_effect_ptr effect)
{
    string_ptr s;

    switch (effect->effect)
    {
    case RBE_HURT:        s = string_copy_s("Hurt"); break;
    case RBE_DRAIN_CHARGES: s = string_copy_s("Drain Charges"); break;
    case RBE_EAT_GOLD:    s = string_copy_s("Steal Gold"); break;
    case RBE_EAT_ITEM:    s = string_copy_s("Steal Item"); break;
    case RBE_EAT_FOOD:    s = string_copy_s("Steal Food"); break;
    case RBE_EAT_LITE:    s = string_copy_s("Absorb Light"); break;
    case RBE_LOSE_STR:    s = string_copy_s("Reduce Strength"); break;
    case RBE_LOSE_INT:    s = string_copy_s("Reduce Intelligence"); break;
    case RBE_LOSE_WIS:    s = string_copy_s("Reduce Wisdom"); break;
    case RBE_LOSE_DEX:    s = string_copy_s("Reduce Dexterity"); break;
    case RBE_LOSE_CON:    s = string_copy_s("Reduce Constitution"); break;
    case RBE_LOSE_CHR:    s = string_copy_s("Reduce Charisma"); break;
    case RBE_LOSE_ALL:    s = string_copy_s("Reduce All Stats"); break;
	case RBE_HALLUCINATE: s = string_copy_s("Cause Hallucination"); break;
    case RBE_SHATTER:     s = string_copy_s("Shatter"); break;
    case RBE_DRAIN_EXP:   s = string_copy_s("<color:D>Lower Experience</color>"); break;
    case RBE_DISEASE:     s = string_copy_s("Disease"); break;
    case RBE_VAMP:        s = string_copy_s("<color:D>Vampiric</color>"); break;
    case RBE_CUT:         s = string_copy_s("<color:r>Cut</color>"); break;
    case GF_MISSILE:      s = string_copy_s("Damage"); break;
    case GF_TURN_ALL:     s = string_copy_s("<color:r>Terrify</color>"); break;
    default:              s = string_copy_s(gf_name(effect->effect));
    }
    assert(s);
    if (effect->pct && effect->dd && effect->ds)
        string_printf(s, " (%dd%d,%d%%)", effect->dd, effect->ds, effect->pct);
    else if (effect->dd && effect->ds)
        string_printf(s, " (%dd%d)", effect->dd, effect->ds);
    else if (effect->pct)
        string_printf(s, " (%d%%)", effect->pct);

	return s;
}
static int _ct_known_attacks(monster_race *r_ptr)
{
    int ct = 0;
    int i;
    for (i = 0; i < MAX_MON_BLOWS; i++)
    {
        mon_blow_ptr blow = &r_ptr->blows[i];
        if (!blow->method) continue;
        ct++;
    }
    return ct;
}
static void _display_attacks(monster_race *r_ptr, doc_ptr doc)
{
    if (r_ptr->flags1 & RF1_NEVER_BLOW)
        doc_insert(doc, "Attacks : <color:D>None</color>\n");
    else if (_ct_known_attacks(r_ptr))
    {
        int i,j;
        /* XXX Damage display needs some rethinking ... */
        doc_printf(doc, "Attacks : <color:G>%-7.7s Effects</color>\n", "Type");
        for (i = 0; i < MAX_MON_BLOWS; i++)
        {
            mon_blow_ptr blow = &r_ptr->blows[i];
            vec_ptr      v;

            if (!blow->method) continue;

            v = vec_alloc((vec_free_f)string_free);
            for (j = 0; j < MAX_MON_BLOW_EFFECTS; j++)
            {
                mon_effect_ptr effect = &blow->effects[j];
                if (!effect->effect) continue;
                vec_add(v, _effect_desc(r_ptr, effect));
            }
            doc_printf(doc, "          %-7.7s",  _method_desc(blow->method));
            if (vec_length(v))
            {
                doc_insert(doc, " <indent><style:indent>");
                _print_list(v, doc, ',', '\0');
                doc_insert(doc, "</style></indent>");
            }
            doc_newline(doc);
            vec_free(v);
        }
    }
    else
        doc_insert(doc, "Attacks : <color:y>?</color>\n");

    doc_newline(doc);
}

/**************************************************************************
 * Other Info
 **************************************************************************/
static void _display_other(monster_race *r_ptr, doc_ptr doc)
{
    int        ct = 0, i;
    vec_ptr    v = vec_alloc((vec_free_f)string_free);

    if (r_ptr->flags2 & RF2_KILL_WALL)
        vec_add(v, string_copy_s("<color:U>Destroys Walls</color>"));

    if (r_ptr->flags2 & RF2_PASS_WALL)
        vec_add(v, string_copy_s("<color:B>Passes through Walls</color>"));

    if (r_ptr->flags2 & RF2_REFLECTING)
        vec_add(v, string_copy_s("<color:o>Reflection</color>"));

    if (r_ptr->flags7 & (RF7_SELF_LITE_1 | RF7_SELF_LITE_2))
        vec_add(v, string_copy_s("<color:y>Shining</color>"));

    if (r_ptr->flags7 & (RF7_SELF_DARK_1 | RF7_SELF_DARK_2))
        vec_add(v, string_copy_s("<color:D>Shrouded in Darkness</color>"));

    if (r_ptr->flags2 & RF2_INVISIBLE)
        vec_add(v, string_copy_s("<color:B>Invisible</color>"));

    if (r_ptr->flags2 & RF2_COLD_BLOOD)
        vec_add(v, string_copy_s("<color:w>Cold Blooded</color>"));

    if (r_ptr->flags2 & RF2_EMPTY_MIND)
        vec_add(v, string_copy_s("<color:o>Shielded from Telepathy</color>"));

    if (r_ptr->flags2 & RF2_WEIRD_MIND)
        vec_add(v, string_copy_s("<color:w>Weird Mind</color>"));

    if (r_ptr->flags2 & RF2_MULTIPLY)
        vec_add(v, string_copy_s("<color:U>Breeds Explosively</color>"));

    if (r_ptr->flags2 & RF2_REGENERATE)
        vec_add(v, string_copy_s("<color:r>Regeneration</color>"));

    if (r_ptr->flags7 & RF7_RANGED_MELEE)
        vec_add(v, string_copy_s("<color:o>Long Reach</color>"));

    if (r_ptr->flags7 & RF7_RIDING)
        vec_add(v, string_copy_s("<color:s>Suitable for Riding</color>"));

    if ((r_ptr->flags7 & RF7_GUARDIAN) && !no_wilderness)
        vec_add(v, string_copy_s("<color:R>Dungeon Guardian</color>"));

    if (vec_length(v))
    {
        doc_insert(doc, "Info    : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Auras */
    vec_clear(v);

    if (r_ptr->flags2 & RF2_AURA_REVENGE)
        vec_add(v, string_copy_s("<color:v>Retaliation</color>"));
    if (r_ptr->flags2 & RF2_AURA_FEAR)
        vec_add(v, string_copy_s("<color:v>Fear</color>"));
    if (r_ptr->flags2 & RF2_AURA_FIRE)
        vec_add(v, _get_res_name(RES_FIRE));
    if (r_ptr->flags3 & RF3_AURA_COLD)
        vec_add(v, _get_res_name(RES_COLD));
    if (r_ptr->flags2 & RF2_AURA_ELEC)
        vec_add(v, _get_res_name(RES_ELEC));

    for (i = 0; i < MAX_MON_AURAS; i++)
    {
        mon_effect_ptr aura = &r_ptr->auras[i];
        gf_info_ptr    gf;
        if (!aura->effect) continue;
        gf = gf_lookup(aura->effect);
        if (gf)
        {
            string_ptr s = string_alloc_format("<color:%c>%s</color>", attr_to_attr_char(gf->color), gf->name);
            string_printf(s, " (%dd%d)", aura->dd, aura->ds);
            vec_add(v, s);
        }
    }

    if (vec_length(v))
    {
        doc_insert(doc, "Auras   : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    if (ct) doc_newline(doc);
    vec_free(v);
}

/**************************************************************************
 * Kills and Drops
 **************************************************************************/
static void _display_drops(monster_race *r_ptr, doc_ptr doc)
{
    int ct_gold = 0;
    int ct_obj = 0;

    if (r_ptr->flags1 & RF1_DROP_4D2) ct_gold += 8;
    if (r_ptr->flags1 & RF1_DROP_3D2) ct_gold += 6;
    if (r_ptr->flags1 & RF1_DROP_2D2) ct_gold += 4;
    if (r_ptr->flags1 & RF1_DROP_1D2) ct_gold += 2;
    if (r_ptr->flags1 & RF1_DROP_90) ct_gold += 1;
    if (r_ptr->flags1 & RF1_DROP_60) ct_gold += 1;

    ct_obj = ct_gold;

    /* Hack -- but only "valid" drops */
    if (r_ptr->flags1 & RF1_ONLY_GOLD) ct_obj = 0;
    if (r_ptr->flags1 & RF1_ONLY_ITEM) ct_gold = 0;
    
    if (ct_gold || ct_obj)
    {
        int ct = MAX(ct_gold, ct_obj);
        cptr obj_text = (ct_obj > 1) ? "Objects" : "Object";
        cptr gold_text = (ct_gold > 1) ? "Treasures" : "Treasure";

        doc_insert(doc, "Drops   : ");

        if (ct == 1)
            doc_insert(doc, "1 ");
        else if (ct == 2)
            doc_insert(doc, "1 or 2 ");
        else
            doc_printf(doc, "Up to %d ", ct);

        if (r_ptr->flags1 & RF1_DROP_GREAT)
            doc_insert(doc, "<color:v>Exceptional</color> ");
        else if (r_ptr->flags1 & RF1_DROP_GOOD)
            doc_insert(doc, "<color:r>Good</color> ");

        if (ct_gold && ct_obj)
            doc_printf(doc, "%s or %s", obj_text, gold_text);
        else if (ct_obj)
            doc_printf(doc, "%s", obj_text);
        else if (ct_gold)
            doc_printf(doc, "%s", gold_text);

        doc_newline(doc);
    }
}
static void _display_kills(monster_race *r_ptr, doc_ptr doc)
{
    if (r_ptr->flags1 & RF1_UNIQUE)
    {
        if (spoiler_hack)
            doc_insert(doc, "Status  : <color:v>Unique</color>");
        else
        {
            doc_insert(doc, "Status  : ");
            if (r_ptr->max_num == 0)
                doc_insert(doc, "<color:D>Dead</color>");
            else if (mon_is_wanted(r_ptr->id))
                doc_insert(doc, "<color:v>Wanted</color>");
            else
                doc_insert(doc, "<color:y>Alive</color>");
        }
        doc_newline(doc);
    }
    else if (!spoiler_hack)
    {
        doc_printf(doc, "Kills   : <color:G>%d</color>\n", r_ptr->r_pkills);
    }

    int plev = spoiler_hack ? 50 : p_ptr->max_plv;
    int xp = r_ptr->mexp * r_ptr->level / (plev + 2);
    char buf[10];

    if (r_ptr->r_akills > (coffee_break ? 49 : 99))
    {
        xp *= 2;
        xp /= divide_exp_by(r_ptr->r_akills);
    }

    big_num_display(xp, buf);
    doc_printf(doc, "Exp     : <color:G>%s</color> at CL%d\n", buf, plev);
    
    _display_drops(r_ptr, doc);
    doc_newline(doc);
}

/**************************************************************************
 * Desc
 **************************************************************************/
static void _display_desc(monster_race *r_ptr, doc_ptr doc)
{
    doc_insert(doc, r_text + r_ptr->text);
    doc_newline(doc);
}

/**************************************************************************
 * Public
 **************************************************************************/
void mon_display(monster_race *r_ptr)
{
    mon_display_rect(r_ptr, ui_menu_rect());
}
void mon_display_rect(monster_race *r_ptr, rect_t display)
{
    doc_ptr doc = doc_alloc(MIN(display.cx, 72));

    if (display.cx > 80)
        display.cx = 80;

    mon_display_doc(r_ptr, doc);

    screen_save();
    if (doc_cursor(doc).y < display.cy - 3)
    {
        doc_insert(doc, "\n<color:B>[Press Any Key to Continue]</color>\n\n");
        doc_sync_term(doc, doc_range_all(doc), doc_pos_create(display.x, display.y));
        inkey();
    }
    else
    {
        doc_display_aux(doc, "Monster Info", 0, display);
    }
    screen_load();

    doc_free(doc);
}
void mon_display_doc(monster_race *r_ptr, doc_ptr doc)
{
    _display_basic(r_ptr, doc);
    _display_resists(r_ptr, doc);
    _display_spells(r_ptr, doc);
    _display_attacks(r_ptr, doc);
    _display_other(r_ptr, doc);
    if (!_possessor_hack) _display_kills(r_ptr, doc);

    _display_desc(r_ptr, doc);
}

void mon_display_possessor(monster_race *r_ptr, doc_ptr doc)
{
    _possessor_hack = TRUE;
    mon_display_doc(r_ptr, doc);
    _possessor_hack = FALSE;
}

