#include "angband.h"

#include <assert.h>

static cptr _desc = 
    "Dragons are powerful winged serpents. They are the strongest fighters "
    "with razor sharp claws and a bone crushing bite. Each dragon has a unique "
    "type of breath that becomes more deadly as the dragon grows and matures.\n \n"
    "Due to their non-humanoid bodies, dragons are unable to wear armor, gloves "
    "or boots. However, being creatures of magic, they are able to wear 6 rings. "
    "They can also wear a helmet, a light source, a cloak and an amulet. Because "
    "of these equipment restrictions, dragons may have a difficult time covering "
    "all resistances despite the fact that each dragon has one or more innate "
    "resistances (or even immunities).\n \n"
    "Dragons begin life in a weak form, being very young. As their bodies mature, "
    "their scales grow tough and their claws sharp. Their breath grows more "
    "deadly and they frequently gain additional magical powers and resistances. "
    "All dragons can fly, but younger dragons are not so quick as their elders.\n \n"
    "Dragons are magical creatures, and each dragon may choose to specialize in a "
    "specific type of magic. This magic does not require books to learn and also "
    "has a direct influence on the dragon's stats and skills.";

static dragon_realm_ptr _get_realm(void);

/**********************************************************************
 * Dragon Equipment
 **********************************************************************/
static void _dragon_birth(void) 
{ 
    object_type    forge;

    skills_innate_init("Claw", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);
    skills_innate_init("Bite", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);
    
    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_h = 3;
    forge.to_d = 3;
    forge.pval = 1;
    add_flag(forge.flags, OF_STR);
    add_flag(forge.flags, OF_DEX);
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    plr_birth_food();
    plr_birth_light();
}

/************************************************************************
 * Private Timers
 ************************************************************************/
enum { _AURA_DOMINATION = T_CUSTOM };
static bool _aura_domination_on(plr_tim_ptr timer)
{
    msg_print("Your presence becomes truly awe-inspiring!");
    return TRUE;
}
static void _aura_domination_off(plr_tim_ptr timer)
{
    msg_print("Your presence returns to normal.");
}
static status_display_t _aura_domination_display(plr_tim_ptr timer)
{
    return status_display_create("Dominate", "Dm", TERM_RED);
}
static plr_tim_info_ptr _aura_domination(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_AURA_DOMINATION, "Aura of Domination");
    info->desc = "Your protective aura of subjugation dominates all who dare attack you.";
    info->on_f = _aura_domination_on;
    info->off_f = _aura_domination_off;
    info->status_display_f = _aura_domination_display;
    return info;
}
static void _register_timers(void)
{
    plr_tim_register(_aura_domination());
}
/************************************************************************
 * Melee Hooks
 ************************************************************************/
static void _after_hit(mon_attack_ptr context)
{
    int dam;
    if (context->stop) return;
    if (!plr_tim_find(_AURA_DOMINATION)) return;
    dam = (subjugation_power()+1)/2;
    msg_format("%^s feels the force of your presence!", context->mon_name);
    gf_affect_m(who_create_plr(), context->mon, GF_SUBJUGATION, dam, GF_AFFECT_AURA);
}
static void _mon_attack_init(mon_attack_ptr context)
{
    if (plr_tim_find(_AURA_DOMINATION))
        context->after_hit_f = _after_hit;
}
/**********************************************************************
 * Dragon Breath
 **********************************************************************/

static void _do_breathe(int effect, point_t pos, int dam)
{
    /* Dragon breath changes shape with maturity */
    if (plr->lev < 20)
        plr_bolt(pos, effect, dam);
    else if (plr->lev < 30)
        plr_beam(pos, effect, dam);
    else
        plr_breath(1 + plr->lev/20, pos, effect, dam);
}

cptr gf_name(int which)
{
    gf_info_ptr gf;
    switch (which)
    {
    case GF_ELDRITCH_HOWL: return "<color:R>Fear</color>";
    case GF_ANIM_DEAD: return "<color:D>Reanimation</color>";
    case GF_OLD_DRAIN: return "<color:D>Vampirism</color>";
    case GF_GENOCIDE: return "<color:D>Death</color>";
    case GF_OLD_POLY: return "<color:v>Change</color>";
    }
    gf = gf_lookup(which);
    if (gf)
    {
        static char buf[100];
        sprintf(buf, "<color:%c>%s</color>", attr_to_attr_char(gf->color), gf->name);
        return buf;
    }
    return "something";
}

static int _count(int list[])
{
    int i;
    for (i = 0; ; i++)
    {
        if (list[i] == -1) return i;
    }
    /* return 0;  error: missing sentinel ... unreachable */
}

static int _random(int list[])
{
    return list[randint0(_count(list))];
}

static void _effect_menu_fn(int cmd, int which, vptr cookie, var_ptr res)
{
    int  idx = ((int*)cookie)[which];

    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, format("%s", gf_name(idx)));
        break;
    }
}

static int _choose_effect(int list[])
{
    int i;
    int ct = _count(list);

    if (REPEAT_PULL(&i))
    {
        if (i >= 0 && i < ct)
            return list[i];
    }

    {
        menu_t menu = { "Choose which effect?", NULL, NULL,
                        _effect_menu_fn, list, ct};
        
        i = menu_choose(&menu);
        if (i >= 0)
        {
            REPEAT_PUSH(i);
            i = list[i];
        }
    }
    return i;
}

static int _get_effect(int list[]) /* va_args is probably a better sig ... */
{
    if (plr->dragon_realm == DRAGON_REALM_BREATH && plr->lev >= 35)
        return _choose_effect(list);
    else
        return _random(list);
}

static int _breath_effect(void)
{
    switch (plr->psubrace)
    {
    case DRAGON_RED: return GF_FIRE;
    case DRAGON_WHITE: return GF_COLD;
    case DRAGON_BLUE: return GF_ELEC;
    case DRAGON_BLACK: return GF_ACID;
    case DRAGON_GREEN: return GF_POIS;
    case DRAGON_BRONZE: return GF_CONFUSION;
    case DRAGON_GOLD: return GF_SOUND;
    case DRAGON_NETHER: 
        if (plr->lev >= 45)
        {
            int effects[] = { GF_NETHER, GF_NEXUS, GF_DISENCHANT, -1 };
            return _get_effect(effects);
        }
        return GF_NETHER;
    case DRAGON_LAW: 
    {
        int effects[] = { GF_SOUND, GF_SHARDS, -1 };
        return _get_effect(effects);
    }
    case DRAGON_CHAOS: 
    {
        int effects[] = { GF_CHAOS, GF_DISENCHANT, -1 };
        return _get_effect(effects);
    }
    case DRAGON_ETHEREAL: 
        if (plr->lev < 40)
        {
            int effects[] = { GF_LIGHT, GF_DARK, -1 };
            return _get_effect(effects);
        }
        else
        {
            int effects[] = { GF_LIGHT, GF_DARK, GF_CONFUSION, -1 };
            return _get_effect(effects);
        }
    case DRAGON_CRYSTAL: return GF_SHARDS;
    case DRAGON_BALANCE:
    {
        int effects[] = { GF_SOUND, GF_SHARDS, GF_CHAOS, GF_DISENCHANT, -1 };
        return _get_effect(effects);
    }
    }
    return 0;
}
static int _breath_amount(void)
{
    int l = plr->lev;
    int amt = 0;
    dragon_realm_ptr realm = _get_realm();

    switch (plr->psubrace)
    {
    case DRAGON_RED:
    case DRAGON_WHITE:
    case DRAGON_BLUE:
    case DRAGON_BLACK:
    case DRAGON_GREEN:
        amt = MIN(600, plr->chp * (25 + l*l*l/2500) / 100);
        break;

    case DRAGON_LAW:
    case DRAGON_CHAOS:
    case DRAGON_CRYSTAL:
    case DRAGON_BRONZE:
    case DRAGON_GOLD:
        amt = MIN(450, plr->chp * (20 + l*l*l*30/125000) / 100);
        break;

    case DRAGON_BALANCE:
    case DRAGON_NETHER:
    case DRAGON_ETHEREAL:
        amt = MIN(400, plr->chp * (20 + l*l*l*25/125000) / 100);
        break;

    case DRAGON_STEEL:
        return 0;
    }
    amt = MAX(1, amt * realm->breath / 100);
    return amt;
}

static int _breath_cost(void)
{
    int l = plr->lev;
    int cost = l/2 + l*l*15/2500;
    /* XXX re-think this ... perhaps: cost = cost * 100 / realm->breath */
    if (plr->dragon_realm == DRAGON_REALM_BREATH)
    {
        if (plr->lev >= 40)
            cost = cost * 3 / 4;
    }
    else if (plr->dragon_realm == DRAGON_REALM_ATTACK)
    {
        cost = cost * 5 / 3;
    }
    else
    {
    /*    cost += 5; */
    }
    return MAX(cost, 1);
}

static cptr _breath_desc(void)
{
    switch (plr->psubrace)
    {
    case DRAGON_RED: return "fire";
    case DRAGON_WHITE: return "cold";
    case DRAGON_BLUE: return "lightning";
    case DRAGON_BLACK: return "acid";
    case DRAGON_GREEN: return "poison";
    case DRAGON_BRONZE: return "confusion";
    case DRAGON_GOLD: return "sound";
    case DRAGON_NETHER: 
        if (plr->lev >= 40) return "nether, nexus or disenchantment";
        return "nether";
    case DRAGON_LAW: return "sound or shards";
    case DRAGON_CHAOS: return "chaos or disenchantment";
    case DRAGON_ETHEREAL: return "light, dark or confusion";
    case DRAGON_CRYSTAL: return "shards";
    case DRAGON_BALANCE: return "sound, shards, chaos or disenchantment";
    }
    return 0;
}

static void _breathe_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe");
        break;
    case SPELL_DESC:
        var_set_string(res, format("Breathes %s at your opponent.", _breath_desc()));
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _breath_amount()));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost());
        break;
    case SPELL_CAST:
    {
        point_t pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
        var_set_bool(res, FALSE);
        if (dun_pos_interior(cave, pos))
        {
            int e = _breath_effect();
            int dam = _breath_amount();
            if (e < 0) return;
            stop_mouth();
            msg_format("You breathe %s.", gf_name(e));
            _do_breathe(e, pos, dam);
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

/**********************************************************************
 * Dragon Melee
 **********************************************************************/
int prorate(int amt, int l, int max, int w1, int w2, int w3)
{
    int result = 0;
    int wt = w1 + w2 + w3;

    if (l == max)
        return amt;

    result += amt * l * w1 / (max*wt);
    result += amt * l * l * w2 / (max*max*wt);
    result += (amt * l * l / max) * l * w3 / (max*max*wt);

    return result;
}
static int _attack_level_aux(int l)
{
    l = prorate(100, l, 50, 1, 1, 1);
    switch (plr->psubrace)
    {
    case DRAGON_STEEL:
        l = MAX(1, l * 130 / 100);
        break;

    case DRAGON_RED:
    case DRAGON_WHITE:
        l = MAX(1, l * 105 / 100);
        break;

    case DRAGON_BLACK:
    case DRAGON_GREEN:
        break;

    case DRAGON_BLUE:
        l = MAX(1, l * 95 / 100);
        break;

    case DRAGON_ETHEREAL:
    case DRAGON_CRYSTAL:
    case DRAGON_BRONZE:
    case DRAGON_GOLD:
        l = MAX(1, l * 90 / 100);
        break;

    case DRAGON_LAW:
    case DRAGON_CHAOS:
    case DRAGON_NETHER:
    case DRAGON_BALANCE:
        l = MAX(1, l * 85 / 100);
        break;
    }
    l = MAX(1, l * _get_realm()->attack / 100);
    return l;
}
static int _attack_level(void)
{
    return _attack_level_aux(plr->lev);
}
static int _bite_max_blows(int l)
{
    int b;
    /* XXX We already prorate when calculating the attack level
     * b = 50 + prorate(150, l, 100, 1, 1, 1);*/
    b = 50 + 150*l/100;
    if (b < 100) b = 100;
    if (b > 200) b = 200;
    return b;
}
void dragon_calc_innate_bonuses(mon_blow_ptr blow, int attack_level)
{
    if (blow->method == RBM_CLAW)
    {
        plr->innate_attack_info.blows_calc.wgt = 150;
        plr->innate_attack_info.blows_calc.mul = 20 + 30 * attack_level / 100;
        plr->innate_attack_info.blows_calc.max = 400;
        plr_calc_blows_innate(blow);
    }
    else if (blow->method == RBM_BITE)
    {
        plr->innate_attack_info.blows_calc.wgt = 250;
        plr->innate_attack_info.blows_calc.mul = 20 + 30 * attack_level / 100;
        plr->innate_attack_info.blows_calc.max = _bite_max_blows(attack_level);
        plr_calc_blows_innate(blow);
    }
}
static void _calc_innate_bonuses(mon_blow_ptr blow)
{
    dragon_calc_innate_bonuses(blow, _attack_level());
}
static dice_t _calc_dice(int dam, int pct_dice, bool claw)
{
    dice_t dice = {0};
    int dice_dam = dam * pct_dice; /* scaled by 100 */
    int x; /* scaled by sqrt(100) = 10 */
    if (claw)
    {
        x = mysqrt(4*dice_dam);
        dice.dd = MAX(1, (x + 5)/10);
        dice.ds = MAX(2, (x + 5)/17);
    }
    else
    {
        x = mysqrt(6*dice_dam);
        dice.dd = MAX(1, (x + 5)/30);
        dice.ds = MAX(5, (x + 5)/10);
    }
    dice.base = MAX(0, dam - dice_avg_roll(dice));
    return dice;
}
static void _spoiler_dump(doc_ptr doc)
{
    int cl;
    for (cl = 1; cl <= 50; cl++)
    {
        int l = _attack_level_aux(cl);
        int claw_dam = 5 + 45*l/100;
        dice_t claw_dice = _calc_dice(claw_dam, 30, TRUE);
        int claw_avg_dam = dice_avg_roll(claw_dice);
        int bite_dam = 9 + 91*l/100;
        dice_t bite_dice = _calc_dice(bite_dam, 50, FALSE);
        int bite_avg_dam = dice_avg_roll(bite_dice);
        int bite_blows = _bite_max_blows(l);

        doc_printf(doc, "%2d %3d ", cl, l);
        doc_printf(doc, "<color:%c>Claw(%dd%d+%d)=%d</color>",
            claw_dam == claw_avg_dam ? 'w' : 'r',
            claw_dice.dd, claw_dice.ds, claw_dice.base, claw_avg_dam);
        doc_printf(doc, "<tab:25><color:%c>Bite(%dd%d+%d)=%d</color>",
            bite_dam == bite_avg_dam ? 'w' : 'r',
            bite_dice.dd, bite_dice.ds, bite_dice.base, bite_avg_dam);
        doc_printf(doc, "<tab:45>Bite x%d.%02d   Dam %3d\n", bite_blows/100, bite_blows%100,
            (400*claw_avg_dam + bite_blows*bite_avg_dam)/100);
    }
}
void dragon_calc_innate_attacks(int attack_level)
{
    mon_blow_ptr blow;

    /* Claws */
    blow = mon_blow_alloc(RBM_CLAW);
    blow->power = 3*attack_level/4;
    blow->weight = 100 + attack_level;
    mon_blow_push_effect(blow, RBE_HURT, _calc_dice(5 + 45*attack_level/100, 30, TRUE)); 
    _calc_innate_bonuses(blow);
    vec_add(plr->innate_blows, blow);

    /* Bite */
    blow = mon_blow_alloc(RBM_BITE);
    blow->power = 3*attack_level/4;
    blow->weight = 200 + 2*attack_level;
    mon_blow_push_effect(blow, RBE_HURT, _calc_dice(9 + 91*attack_level/100, 50, FALSE));
    if (plr->dragon_realm == DRAGON_REALM_DEATH && plr->lev >= 45)
        mon_blow_push_effect(blow, RBE_VAMP, _calc_dice(70*attack_level/100, 50, FALSE));
    _calc_innate_bonuses(blow);
    vec_add(plr->innate_blows, blow);
}
static void _calc_innate_attacks(void)
{
    dragon_calc_innate_attacks(_attack_level());
}
/**********************************************************************
 * Dragon Realms
 **********************************************************************/
static dragon_realm_t _realms[DRAGON_REALM_MAX] = {
    { DRAGON_REALM_NONE, "None", 
        "",
    /*  S   I   W   D   C   C    Dsrm Dvce Save Stlh Srch Prcp Thn Thb  Life  Exp Attack Breath*/
      { 0,  0,  0,  0,  0,  0}, {   0,   0,   0,   0,   0,   0,  0,  0}, 100, 100,   100,   100, A_NONE},

    { DRAGON_REALM_LORE, "Lore", 
        "Dragons specializing in lore are seekers of knowledge. They are the most "
        "intelligent of dragonkind and use their vast intellects to drive their magic. "
        "Armed with a vast array of detection and knowledge spells, dragons of lore "
        "seek power through knowledge. They eventually gain powers of telepathy and "
        "automatic object identification. Lore dragons are quick learners and gain "
        "maturity much more rapidly than the rest of their kind.",
    /*  S   I   W   D   C   C    Dsrm Dvce Save Stlh Srch Prcp Thn Thb  Life  Exp Attack Breath*/
      {-1, +3,  0, -1, -1,  0}, {   3,   8,   2,   0,   5,   5, -8,  0}, 100,  80,   100,   100, A_INT, 3},

    { DRAGON_REALM_BREATH, "Breath", 
        "Dragon breath is the stuff of legends, and this realm seeks to enhance this most "
        "powerful attribute of dragonkind. With this speciality, you will be able to shape "
        "and control your breaths to maximize deadliness for a given situation. In addition, "
        "dragons of this realm may choose their breath types if applicable, and breathing "
        "becomes less costly as they mature. This focus requires great fortitude to master "
        "and somewhat diminishes the dragon's defenses and melee.",
    /*  S   I   W   D   C   C    Dsrm Dvce Save Stlh Srch Prcp Thn Thb  Life  Exp Attack Breath*/
      { 0, -1, -1,  0, +3, +1}, {   0,   0,   3,  -1,   0,   0,  0,  0}, 103, 105,    90,   115, A_CON, 3},

    { DRAGON_REALM_ATTACK, "Attack", 
        "Attack dragons seek melee supremacy above all else. This realm offers powerful attack "
        "spells to support a race that is already among the melee elite, and the result can "
        "be truly awe-inspiring. With this realm, the dragon may rend their opponents with "
        "extra sharp claws, may snatch an adjacent opponent in their powerful jaws and then "
        "toss them about like a rag doll, and may even augment their bite attacks with their "
        "breath element! Truly, a rampaging dragon is an awe inspiring sight, one that is "
        "seldom witnessed (or perhaps seldom survived?). This focus values strength above all else.",
    /*  S   I   W   D   C   C    Dsrm Dvce Save Stlh Srch Prcp Thn Thb  Life  Exp Attack Breath*/
      {+3, -2, -2, +1, -1,  0}, {  -5,  -5,  -3,  -1,  -2,  -2, 15,  0},  97, 105,   115,    80, A_STR, -2},

    { DRAGON_REALM_CRAFT, "Craft", 
        "The most powerful magical items have long been believed forged by dragonflame. The "
        "craft dragon gains powers of enchantment and may even reforge artifacts into the objects "
        "of their choosing! Otherwise, craft dragons are not particularly powerful as they trade "
        "melee and breath prowess for magical understanding. This focus requires great wisdom to "
        "master.",
    /*  S   I   W   D   C   C    Dsrm Dvce Save Stlh Srch Prcp Thn Thb  Life  Exp Attack Breath*/
      {-1, -1, +3, -1, -1, -1}, {   3,  15,   3,   0,   0,   0, -5,  0}, 100,  95,   100,   100, A_WIS, 3},

    { DRAGON_REALM_ARMOR, "Armor", 
        "Dragon scales have thwarted many a would be dragonslayer. Naturally tough and resistant, "
        "the dragon's armor is even further enhanced by this realm. This specialization gives enhanced "
        "armor class, reflection, resistance to cuts and stunning, resistance to poison "
        "and life draining, and sustaining of several key stats, albeit not all at once. With all "
        "of these extra innate bonuses, the magic spells of this realm are few in number but serve "
        "to offer temporary defensive augmentations. Unlike their kin, dragons of this order prize "
        "agility above all else.",
    /*  S   I   W   D   C   C    Dsrm Dvce Save Stlh Srch Prcp Thn Thb  Life  Exp Attack Breath*/
      {-1, -1, -1, +3, +1, +1}, {  -2,  -3,   7,   1,   0,   0,-10,  0}, 102, 105,    90,    90, A_DEX, -2},

    { DRAGON_REALM_DOMINATION, "Domination", 
        "All dragons have a formidable presence and make fearsome opponents. But Domination dragons "
        "are truly a breed apart seeking to bend and control the will of all they meet. Convinced "
        "of their right to rule, these dragons may subjugate the weak, terrify the uncertain, "
        "and stun the unwary with their awesome presence. They are the best dragons at controlling "
        "minions and can summon mighty aid. Enemy monsters summoned to battle may switch sides "
        "when they notice whom they have been commanded to fight. And in the end, the dragon of "
        "domination may sever all oaths of allegiance, returning enemy summons to where they came. "
        "Needless to say, dragons of this order value force of will above all else.",
    /*  S   I   W   D   C   C    Dsrm Dvce Save Stlh Srch Prcp Thn Thb  Life  Exp Attack Breath*/
      {-1, -1, -1, -1, -1, +3}, {  -2,  -3,  -2,   0,   0,   0, -7,  0},  95, 105,    95,    90, A_CHR, 2},

    { DRAGON_REALM_CRUSADE, "Crusade", 
        "Crusade dragons are on a mission to destroy the forces of evil. As such, this realm is only "
        "available to Gold Dragons and Law Dragons. Being of a single focus, the Crusade dragon is not "
        "as powerful in melee or breaths as other dragons, but their spells more than make up for this "
        "deficit. At least against evil opponents. For one thing, Crusade dragons can breathe holy "
        "elements not normally accessible. At the sight of evil, they become enraged and may haste "
        "for battle. They can even heal a bit and ultimately they may smite the forces of evil with "
        "holy power, both in melee and in breath. Strong in personality, dragons of this order "
        "may summon like minded kin for the final battles.",
    /*  S   I   W   D   C   C    Dsrm Dvce Save Stlh Srch Prcp Thn Thb  Life  Exp Attack Breath*/
      {+1, -1, -1, +1, -1, +2}, {  -5,   0,  -2,   0,  -2,  -2,  7,  0},  95, 107,    90,    90, A_CHR, 2},

    { DRAGON_REALM_DEATH, "Death", 
        "Death dragons are enemies of life itself, seeking to destroy all living creatures. With this "
        "realm, the dragon may bend their breath weapon to suit their necromantic desires, eventually "
        "breathing mastery over both death and life. At high levels, the death dragon's bite gains "
        "a powerful draining effect against living creatures. This focus values strength above "
        "all else. This foul realm is only available to Shadow and Chaos dragons.",
    /*  S   I   W   D   C   C    Dsrm Dvce Save Stlh Srch Prcp Thn Thb  Life  Exp Attack Breath*/
      {+2, -2, -2,  0, -2, +1}, {  -5,  -3,  -3,   2,  -2,  -2,  5,  0},  95, 105,    90,    90, A_STR, 3},
};

dragon_realm_ptr dragon_get_realm(int which)
{
    assert(0 <= which && which < DRAGON_REALM_MAX);
    return &_realms[which];
}

dragon_realm_ptr _get_realm(void)
{
    return dragon_get_realm(plr->dragon_realm);
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "dragon spell";
        me.encumbrance.max_wgt = 750;
        me.encumbrance.weapon_pct = 0;
        me.encumbrance.enc_wgt = 800;
        init = TRUE;
    }
    me.which_stat = _get_realm()->spell_stat; /* Careful: Birthing may invoke this multiple times with different realms */
    return &me;
}

/* Lore */
static spell_info _lore_spells[] = {
    {  1,  1, 30, detect_traps_spell },
    {  3,  2, 30, detect_treasure_spell },
    {  5,  3, 40, detect_monsters_spell },
    {  7,  5, 50, detect_objects_spell },
    { 12, 10, 60, identify_spell },
    { 15, 12, 60, sense_surroundings_spell },
    { 20, 15, 60, detection_spell },
    { 22, 17, 60, probing_spell },
    { 25, 20, 65, self_knowledge_spell },
    { 30, 25, 70, identify_fully_spell },
    { 40, 50, 90, clairvoyance_spell },
    { -1, -1, -1, NULL}
};

/* Breath */
static void _bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, format("Breathes a bolt of %s at your opponent. This is quicker, though less deadly, as you become more powerful.", _breath_desc()));
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, MAX(1, _breath_amount()/2)));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, MAX(1, _breath_cost()/2));
        break;
    case SPELL_CAST: {
        point_t pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
        var_set_bool(res, FALSE);
        if (dun_pos_interior(cave, pos))
        {
            int e = _breath_effect(); /* menu */
            int dam = MAX(1, _breath_amount()/2);
            var_set_bool(res, FALSE);
            if (e < 0) return;
            msg_format("You breathe %s.", gf_name(e));
            plr_bolt(pos, e, dam);
            var_set_bool(res, TRUE);
        }
        break; }
    case SPELL_ENERGY:
        var_set_int(res, 101 - plr->lev);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _beam_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Beam");
        break;
    case SPELL_DESC:
        var_set_string(res, format("Breathes a beam of %s at your opponent. This is quicker as you become more powerful.", _breath_desc()));
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _breath_amount()));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost());
        break;
    case SPELL_CAST: {
        point_t pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
        var_set_bool(res, FALSE);
        if (dun_pos_interior(cave, pos))
        {
            int e = _breath_effect(); /* menu */
            int dam = _breath_amount();
            var_set_bool(res, FALSE);
            if (e < 0) return;
            msg_format("You breathe %s.", gf_name(e));
            plr_beam(pos, e, dam);
            var_set_bool(res, TRUE);
        }
        break; }
    case SPELL_ENERGY:
        var_set_int(res, 110 - plr->lev);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _cone_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cone");
        break;
    case SPELL_DESC:
        var_set_string(res, format("Breathes a cone of %s at your opponent. This is quicker as you become more powerful.", _breath_desc()));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost());
        break;
    case SPELL_ENERGY:
        var_set_int(res, 120 - plr->lev);
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _breath_amount()));
        break;
    case SPELL_CAST: {
        point_t pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
        var_set_bool(res, FALSE);
        if (dun_pos_interior(cave, pos))
        {
            int e = _breath_effect(); /* menu */
            int dam = _breath_amount();
            var_set_bool(res, FALSE);
            if (e < 0) return;
            msg_format("You breathe %s.", gf_name(e));
            plr_breath(2, pos, e, dam);
            var_set_bool(res, TRUE);
        }
        break; }
    default:
        default_spell(cmd, res);
    }
}

static void _split_beam_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Split Beam");
        break;
    case SPELL_DESC:
        var_set_string(res, format("You breathe a beam of %s a two chosen targets, albeit with reduced damage.", _breath_desc()));
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, MAX(1, _breath_amount()/2)));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost());
        break;
    case SPELL_CAST:
    {
        point_t pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
        var_set_bool(res, FALSE);
        if (dun_pos_interior(cave, pos))
        {
            int e = _breath_effect(); /* menu */
            int dam = MAX(1, _breath_amount()/2);
            var_set_bool(res, FALSE);
            if (e < 0) return;
            msg_format("You breathe %s.", gf_name(e));
            plr_beam(pos, e, dam);
            
            command_dir = 0; /* Code is buggy asking for a direction 2x in a single player action! */
            plr->target = who_create_null();  /* TODO: Repeat command is busted ... */
            plr_cast_beam(e, dice_create(0, 0 , dam));

            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _retreating_breath_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Retreating Breath");
        break;
    case SPELL_DESC:
        var_set_string(res, format("Breathes %s at your opponent and then take a movement.", _breath_desc()));
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _breath_amount()));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost());
        break;
    case SPELL_CAST:
    {
        point_t pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
        var_set_bool(res, FALSE);
        if (dun_pos_interior(cave, pos))
        {
            int dir;
            int e = _breath_effect();  /* menu */
            int dam = _breath_amount();
            var_set_bool(res, FALSE);
            if (e < 0) return;
            msg_format("You breathe %s.", gf_name(e));
            plr_breath(2, pos, e, dam);

            command_dir = 0; /* Code is buggy asking for a direction 2x in a single player action! */
            plr->target = who_create_null();  /* TODO: Repeat command is busted ... */

            if (get_rep_dir2(&dir) && dir != 5)
            {
                point_t pos = point_step(plr->pos, dir);
                dun_cell_ptr cell = dun_cell_at(cave, pos);
                if (cell_allow_plr(cell) && !floor_has_known_trap(cell) && !dun_mon_at(cave, pos))
                    move_player_effect(pos, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
            }

            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _deadly_breath_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Deadly Breath");
        break;
    case SPELL_DESC:
        var_set_string(res, format("Breathes %s powerfully at your opponent.", _breath_desc()));
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _breath_amount() * 125 / 100));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost() * 125 / 100);
        break;
    case SPELL_CAST:
    {
        point_t pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
        var_set_bool(res, FALSE);
        if (dun_pos_interior(cave, pos))
        {
            int e = _breath_effect();  /* menu */
            int dam = _breath_amount() * 125 / 100;
            var_set_bool(res, FALSE);
            if (e < 0) return;
            msg_format("You breathe %s.", gf_name(e));
            plr_breath(3, pos, e, dam);
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _star_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Star Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Unleash your breath uncontrollably and at random, though with devastating effect.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost());
        break;
    case SPELL_CAST: {
        int e = _breath_effect(); /* menu */
        var_set_bool(res, FALSE);
        if (e < 0) return;
        plr_star_ball(_5d(3), e, dice_create(0, 0, _breath_amount()/3));
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static spell_info _breath_spells[] = {
    {  1,  0, 30, _bolt_spell },
    { 10,  0, 30, _beam_spell },
    { 20,  0, 30, _cone_spell },
    { 25, 10, 50, _split_beam_spell },
    { 30, 15, 50, _retreating_breath_spell },
    { 40, 15, 60, _deadly_breath_spell },
    { 50, 50, 70, _star_ball_spell },
    { -1, -1, -1, NULL}
};

/* Craft */
static void _detect_magic_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Magic");
        break;
    case SPELL_DESC:
        var_set_string(res, "Locate nearby magical objects.");
        break;
    case SPELL_CAST:
        detect_objects_magic(DETECT_RAD_DEFAULT);    
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static obj_ptr _get_reforge_src(int max_power)
{
    obj_prompt_t prompt = {0};
    char buf[255];

    sprintf(buf, "Use what artifact for reforging (Max Power = %d)? ", max_power);
    prompt.prompt = buf;
    prompt.error = "You have no artifacts to reforge.";
    prompt.filter = obj_is_art;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;
    prompt.flags = INV_SHOW_VALUE;

    obj_prompt(&prompt);
    return prompt.obj;
}

static obj_ptr _get_reforge_dest(int max_power)
{
    obj_prompt_t prompt = {0};
    char buf[255];

    sprintf(buf, "Reforge which object (Max Power = %d)? ", max_power);
    prompt.prompt = buf;
    prompt.error = "You have nothing to reforge.";
    prompt.filter = item_tester_hook_nameless_weapon_armour;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;
    prompt.flags = INV_SHOW_VALUE;

    obj_prompt(&prompt);
    return prompt.obj;
}

static void _reforging_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Reforging");
        break;
    case SPELL_DESC:
        var_set_string(res, "Reforge a chosen artifact.");
        break;
    case SPELL_CAST:
    {
        int cost;
        char o_name[MAX_NLEN];
        obj_ptr src, dest;
        int power = plr->lev * 5 / 2 + (plr->lev >= 50 ? 5 : 0);
        int src_max_power = power * power * 10;
        int dest_max_power = 0;

        var_set_bool(res, FALSE);
        src = _get_reforge_src(src_max_power);
        if (!src) return;
        if (!obj_is_art(src)) /* paranoia */
        {
            msg_print("You must choose an artifact for reforging.");
            return;
        }
        if (obj_value_real(src) > src_max_power)
        {
            msg_print("You are not powerful enough to reforge that item.");
            return;
        }

        cost = obj_value_real(src);
    
        dest_max_power = cost / 2;
        if (dest_max_power < 1000) /* Reforging won't try to power match weak stuff ... */
            dest_max_power = 1000;

        object_desc(o_name, src, OD_NAME_ONLY);    
        if (!get_check(format("Really use %s? (It will be destroyed!) ", o_name))) 
            return;

        dest = _get_reforge_dest(dest_max_power);
        if (!dest) return;

        if (dest->number > 1)
        {
            msg_print("Don't be greedy! You may only reforge a single object.");
            return;
        }

        if (obj_is_art(dest))
        {
            msg_print("This item is already an artifact!");
            return;
        }

        if (obj_is_ego(dest))
        {
            msg_print("This item is already an ego item!");
            return;
        }

        if (!equip_first_slot(dest))
        {
            msg_print("You may only create items you can actually use.");
            return;
        }

        if (obj_value_real(dest) > dest_max_power)
        {
            msg_print("This item is too powerful for the source artifact you have chosen.");
            return;
        }

        if (!art_reforge(src, dest, power))
        {
            msg_print("The reforging failed!");
            return;
        }

        src->number--;
        obj_release(src, 0);

        obj_identify_fully(dest);

        plr->update |= PU_BONUS;
        plr->window |= (PW_INVEN | PW_EQUIP);
        handle_stuff();

        obj_display(dest);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static spell_info _craft_spells[] = {
    {  1,  1, 30, _detect_magic_spell },
    {  5,  7, 60, minor_enchantment_spell },
    { 12, 10, 60, remove_curse_I_spell },
    { 17, 15, 60, identify_spell },
    { 30, 25, 70, enchantment_spell },
    { 32, 30, 70, recharging_spell },
    { 35, 90, 70, _reforging_spell },
    { 40, 30, 70, dispel_magic_spell },
    { -1, -1, -1, NULL}
};

/* Attack */
static void _war_cry_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dragon's Roar");
        break;
    case SPELL_DESC:
        var_set_string(res, "You will roar out mightily, alerting all nearby monsters of your presence.");
        break;
    case SPELL_CAST:
        msg_print("You roar out!");
        plr_project_los(GF_SOUND, randint1(plr->lev));
        aggravate_monsters(who_create_null());
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _rend_begin(plr_attack_ptr context)
{
    if (!context->blow) return TRUE;
    if (context->blow->method == RBM_CLAW || context->blow->method == RBM_BITE)
        mon_blow_push_effect(context->blow, RBE_CUT, dice_create(0, 0, 0))->pct = 15 + plr->lev/3;
    /* Note: Dice don't matter. Checkout _innate_vorpal_pct in plr_attack.c
       Slay: 1 + p(1 + 1/4 + ...) = 1 + p(4/3). So 40% gives a 1.53x multiplier! */
    return TRUE;
}
static void _rend_end(plr_attack_ptr context)
{
    if (!context->blow) return;
    if (context->blow->method == RBM_CLAW || context->blow->method == RBM_BITE)
        mon_blow_pop_effect(context->blow);
}
static void _rend_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rend");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with cutting blows.");
        break;
    case SPELL_CAST:
    case SPELL_ON_BROWSE: {
        plr_attack_t context = {0};
        context.hooks.begin_weapon_f = _rend_begin;
        context.hooks.end_weapon_f = _rend_end;
        if (cmd == SPELL_CAST)
            var_set_bool(res, plr_attack_special_aux(&context, 1));
        else
        {
            plr_attack_display_aux(&context);
            var_set_bool(res, TRUE);
        }
        break; }
    case SPELL_COST_EXTRA:
        var_set_int(res, plr->lev/2);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _rage_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rage");
        break;
    default:
        berserk_spell(cmd, res);
        break;
    }
}

static void _three_way_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "3-Way Attack");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack in chosen direction, and to either side of chosen direction, in a single action.");
        break;
    case SPELL_CAST:
    {
        int cdir, dir;
        point_t pos;

        var_set_bool(res, FALSE);
        if (!get_rep_dir2(&dir)) return;
        if (dir == 5) return;

        for (cdir = 0;cdir < 8; cdir++)
        {
            if (cdd[cdir] == dir) break;
        }

        if (cdir == 8) return;

        pos = point_step(plr->pos, cdd[cdir]);
        if (dun_mon_at(cave, pos))
            plr_attack_normal(pos);
        else
            msg_print("You attack the empty air.");

        pos = point_step(plr->pos, cdd[(cdir + 7)%8]);
        if (dun_mon_at(cave, pos))
            plr_attack_normal(pos);
        else
            msg_print("You attack the empty air.");

        pos = point_step(plr->pos, cdd[(cdir + 1)%8]);
        if (dun_mon_at(cave, pos))
            plr_attack_normal(pos);
        else
            msg_print("You attack the empty air.");

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _deadly_bite_begin(plr_attack_ptr context)
{
    if (!context->blow) return TRUE;
    if (context->blow->method == RBM_BITE)
    {
        dice_t d = mon_blow_base_dice(context->blow);
        mon_blow_push_effect(context->blow, _breath_effect(), dice_create(d.dd*5/2, d.ds, 0));
    }
    return TRUE;
}
static void _deadly_bite_end(plr_attack_ptr context)
{
    if (!context->blow) return;
    if (context->blow->method == RBM_BITE)
        mon_blow_pop_effect(context->blow);
}
static void _deadly_bite_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Deadly Bite");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent as usual, but augment your bite attacks with your breath element.");
        break;
    case SPELL_CAST:
    case SPELL_ON_BROWSE: {
        plr_attack_t context = {0};
        context.hooks.begin_weapon_f = _deadly_bite_begin;
        context.hooks.end_weapon_f = _deadly_bite_end;
        if (cmd == SPELL_CAST)
            var_set_bool(res, plr_attack_special_aux(&context, 1));
        else
        {
            plr_attack_display_aux(&context);
            var_set_bool(res, TRUE);
        }
        break; }
    case SPELL_COST_EXTRA:
        var_set_int(res, 15*plr->lev/50);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _snatch_begin(plr_attack_ptr context)
{
    if (!context->blow) return FALSE;
    if (context->blow->method != RBM_BITE) return FALSE;
    return TRUE;
}
static void _snatch_after_hit(plr_attack_ptr context)
{
    if (context->stop) return;
    msg_format("You grab %s in your jaws.", context->mon_name);
    msg_print(NULL); /* otherwise the targetting will erase the msg_line */
    monster_toss(context->mon->id);
    context->stop = STOP_PLR_SPECIAL;
}
static void _snatch_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Snatch");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to snatch an adjacent opponent in your jaws. If successful, you may toss the monster away from you.");
        break;
    case SPELL_CAST: {
        plr_attack_t context = {0};
        context.hooks.begin_weapon_f = _snatch_begin;
        context.hooks.after_hit_f = _snatch_after_hit;
        var_set_bool(res, plr_attack_special_aux(&context, 1));
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _charge_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Charge");
        break;
    case SPELL_DESC:
        var_set_string(res, "Charge a nearby monster and attack in a single action.");
        break;
    case SPELL_CAST:
        var_set_bool(res, rush_attack(5, NULL));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _rapid_strike_begin(plr_attack_ptr context)
{
    if (!context->blow) return TRUE;
    if (context->blow->method == RBM_CLAW) context->blow->blows += 100;
    if (context->blow->method == RBM_BITE) context->blow->blows += 25;
    return TRUE;
}
static void _rapid_strike_end(plr_attack_ptr context)
{
    if (!context->blow) return;
    if (context->blow->method == RBM_CLAW) context->blow->blows -= 100;
    if (context->blow->method == RBM_BITE) context->blow->blows -= 25;
}
static void _rapid_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rapid Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with extra blows.");
        break;
    case SPELL_CAST:
    case SPELL_ON_BROWSE: {
        plr_attack_t context = {0};
        context.hooks.begin_weapon_f = _rapid_strike_begin;
        context.hooks.end_weapon_f = _rapid_strike_end;
        if (cmd == SPELL_CAST)
            var_set_bool(res, plr_attack_special_aux(&context, 1));
        else
        {
            plr_attack_display_aux(&context);
            var_set_bool(res, TRUE);
        }
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _power_strike_begin(plr_attack_ptr context)
{
    if (!context->blow) return TRUE;
    if (context->blow->method == RBM_CLAW || context->blow->method == RBM_BITE)
    {
        context->blow->weight += 200;
        context->blow->effects[0].dice.dd += 2;
    }
    return TRUE;
}
static void _power_strike_end(plr_attack_ptr context)
{
    if (!context->blow) return;
    if (context->blow->method == RBM_CLAW || context->blow->method == RBM_BITE)
    {
        context->blow->weight -= 200;
        context->blow->effects[0].dice.dd -= 2;
    }
}
static void _power_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Power Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with more powerful blows.");
        break;
    case SPELL_CAST:
    case SPELL_ON_BROWSE: {
        plr_attack_t context = {0};
        context.hooks.begin_weapon_f = _power_strike_begin;
        context.hooks.end_weapon_f = _power_strike_end;
        if (cmd == SPELL_CAST)
            var_set_bool(res, plr_attack_special_aux(&context, 1));
        else
        {
            plr_attack_display_aux(&context);
            var_set_bool(res, TRUE);
        }
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static spell_info _attack_spells[] = {
    {  1,  1, 20, _war_cry_spell },
    {  5,  3, 40, detect_menace_spell },
    {  7,  5,  0, _rend_spell },
    {  9,  9, 50, _rage_spell },
    { 12, 10,  0, _three_way_attack_spell },
    { 20, 15,  0, _deadly_bite_spell },
    { 22, 15,  0, _snatch_spell },
    { 25, 30, 50, _charge_spell },
    { 30, 35, 50, _rapid_strike_spell },
    { 40, 40, 60, _power_strike_spell },
    { -1, -1, -1, NULL}
};

/* Armor */
static void _shard_skin_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shard Skin");
        break;
    case SPELL_DESC:
        var_set_string(res, "Temporarily gain an aura of shards which damages any monsters that strike you.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_AURA_SHARDS, randint1(30) + 20);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _dragon_cloak_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dragon Cloak");
        break;
    case SPELL_DESC:
        var_set_string(res, "Temporarily gain protective elemental auras for a bit.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_AURA_FIRE, randint1(30) + 20);
        if (plr->lev >= 25)
            plr_tim_add(T_AURA_COLD, randint1(30) + 20);
        if (plr->lev >= 35)
            plr_tim_add(T_AURA_ELEC, randint1(30) + 20);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _magic_resistance_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Magic Resistance");
        break;
    case SPELL_DESC:
        var_set_string(res, "Temporarily gain enhanced resistance to magic.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_RES_MAGIC, randint1(30) + 20);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static spell_info _armor_spells[] = {
    { 10, 10, 50, stone_skin_spell },
    { 15, 12, 50, _shard_skin_spell },
    { 20, 15, 60, _dragon_cloak_spell },
    { 25, 20, 60, resistance_spell },
    { 30, 30, 70, _magic_resistance_spell },
    { -1, -1, -1, NULL}
};

/* Crusade */
static void _breathe_spell_aux(int effect, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _breath_amount()));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost());
        break;
    case SPELL_CAST:
    {
        point_t pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
        var_set_bool(res, FALSE);
        if (dun_pos_interior(cave, pos))
        {
            int dam = _breath_amount();

            msg_format("You breathe %s.", gf_name(effect));
            _do_breathe(effect, pos, dam);

            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _breathe_retribution_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Retribution");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes lightning at chosen target.");
        break;
    default:
        _breathe_spell_aux(GF_ELEC, cmd, res);
        break;
    }
}

static void _breathe_light_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Light");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes light at chosen target.");
        break;
    default:
        _breathe_spell_aux(GF_LIGHT, cmd, res);
        break;
    }
}

static void _healing_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Healing");
        break;
    case SPELL_DESC:
        var_set_string(res, "Powerful healing magic:  heals hitpoints, cuts and stun.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Heals %d", 200));
        break;
    case SPELL_CAST:
        hp_player(200);
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _breathe_holiness_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Holiness");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes holy fire at chosen target. This hurts evil monsters greatly, but non-evil monsters resist.");
        break;
    default:
        _breathe_spell_aux(GF_HOLY_FIRE, cmd, res);
        break;
    }
}

static bool _smite_evil_begin(plr_attack_ptr context)
{
    if (!context->blow) return TRUE;
    if (context->blow->method == RBM_CLAW || context->blow->method == RBM_BITE)
    {
        add_flag(context->obj_flags, OF_SLAY_EVIL); /* flags are a copy */
    }
    return TRUE;
}
static void _smite_evil_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Smite Evil");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent evil opponent with holy fury!");
        break;
    case SPELL_CAST:
    case SPELL_ON_BROWSE: {
        plr_attack_t context = {0};
        context.hooks.begin_weapon_f = _smite_evil_begin;
        if (cmd == SPELL_CAST)
            var_set_bool(res, plr_attack_special_aux(&context, 1));
        else
        {
            plr_attack_display_aux(&context);
            var_set_bool(res, TRUE);
        }
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static spell_info _crusade_spells[] = {
    {  1,  1, 30, bless_spell },
    {  5,  3, 30, remove_fear_spell },
    {  7,  4, 40, detect_evil_spell },
    { 10,  5, 50, _breathe_retribution_spell },
    { 15,  7, 50, heroism_spell },
    { 25, 15, 60, haste_self_spell },
    { 30, 20, 60, curing_spell },
    { 32, 10, 60, _breathe_light_spell },
    { 35, 30, 60, _healing_spell },
    { 37, 30, 70, _breathe_holiness_spell },
    { 40, 60,  0, _smite_evil_spell },
    { 45,100, 90, summon_hi_dragon_spell },
    { -1, -1, -1, NULL}
};

/* Death */
static void _breathe_poison_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Poison");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes poison at chosen target.");
        break;
    default:
        _breathe_spell_aux(GF_POIS, cmd, res);
        break;
    }
}

static void _breathe_fear_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Fear");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes fear at chosen target.");
        break;
    default:
        _breathe_spell_aux(GF_ELDRITCH_HOWL, cmd, res);
        break;
    }
}

static void _breathe_dark_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Darkness");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes darkness at chosen target.");
        break;
    default:
        _breathe_spell_aux(GF_DARK, cmd, res);
        break;
    }
}

static void _breathe_nether_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Nether");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes nether at chosen target.");
        break;
    default:
        _breathe_spell_aux(GF_NETHER, cmd, res);
        break;
    }
}

static void _breathe_reanimation_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Reanimation");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes reanimation. Any corpses or skeletons hit by this breath may come back to life to serve you.");
        break;
    case SPELL_INFO:
        break;
    default:
        _breathe_spell_aux(GF_ANIM_DEAD, cmd, res);
        break;
    }
}

int dragon_vamp_amt = 0;
bool dragon_vamp_hack = FALSE;

static void _breathe_vampirism_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Vampirism");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes vampirism. Any living creatures hit by this breath have their life energies stolen to regain your hitpoints.");
        break;
    default:
        dragon_vamp_hack = TRUE;
        dragon_vamp_amt = 0;
        _breathe_spell_aux(GF_OLD_DRAIN, cmd, res);
        dragon_vamp_hack = FALSE;
        if (dragon_vamp_amt)
        {
            int amt = MIN(500, (dragon_vamp_amt + 1) / 2);
            hp_player(amt);
        }
        break;
    }
}

static void _breathe_unholiness_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Unholiness");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes hell fire at chosen target.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _breath_amount() * 3 / 2));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost() * 3 / 2);
        break;
    case SPELL_CAST: {
        point_t pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
        var_set_bool(res, FALSE);
        if (dun_pos_interior(cave, pos))
        {
            int dam = _breath_amount() * 3 / 2;
            msg_print("You breathe hell fire.");
            plr_breath(1 + plr->lev/20, pos, GF_HELL_FIRE, dam);
            var_set_bool(res, TRUE);
        }
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _breathe_genocide_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Genocide");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes genocide. Any monsters hit by this breath may be removed from the level, but you take damage for each monster so removed.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_power(_breath_amount()));
        break;
    default:
        _breathe_spell_aux(GF_GENOCIDE, cmd, res);
        break;
    }
}

static spell_info _death_spells[] = {
    {  1,  1, 30, evil_bless_spell },
    {  5,  2, 40, detect_evil_spell },
    { 10,  5, 50, _breathe_poison_spell },
    { 15,  5, 50, _breathe_fear_spell },
    { 20, 10, 60, _breathe_dark_spell },
    { 25, 20, 60, restore_life_spell },
    { 27, 10, 60, _breathe_nether_spell },
    { 30, 25, 65, battle_frenzy_spell },
    { 32, 20, 65, _breathe_reanimation_spell },
    { 35, 20, 65, _breathe_vampirism_spell },
    { 37, 20, 70, _breathe_unholiness_spell },
    { 40, 35, 75, _breathe_genocide_spell },
    { -1, -1, -1, NULL}
};

/* Domination */
static void _frightful_presence_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Frightful Presence");
        break;
    default:
        scare_spell(cmd, res);
        break;
    }
}

static void _detect_minions_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Minions");
        break;
    default:
        detect_monsters_spell(cmd, res);
        break;
    }
}

static void _baffling_presence_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Baffling Presence");
        break;
    default:
        confuse_spell(cmd, res);
        break;
    }
}

static void _enslave_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Enslave");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to force a single monster to serve you.");
        break;
    default:
        bolt_spell_aux(cmd, res, GF_CHARM, spell_dice(0, 0, 2*plr->lev));
    }
}

static int _plev(void)
{
    if (plr->lev <= 40)
        return 5 + plr->lev;

    return 45 + (plr->lev - 40)*2;
}

int subjugation_power(void)
{
    return MAX(1, _plev() + adj_stat_save[plr->stat_ind[A_CHR]]);
}

static void _breathe_subjugation_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Subjugation");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes dominance at chosen target. Affected monsters may be forced to serve you or may be stunned or terrified by your awesome presence.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_power(subjugation_power()));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost());
        break;
    case SPELL_CAST:
    {
        point_t pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
        var_set_bool(res, FALSE);
        if (dun_pos_interior(cave, pos))
        {
            int dam = subjugation_power();

            msg_print("You breathe subjugation.");
            _do_breathe(GF_SUBJUGATION, pos, dam);

            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _aura_of_domination_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Aura of Domination");
        break;
    case SPELL_DESC:
        var_set_string(res, "Temporarily gain an aura of domination which attempts to enslave, terrify or stun any monster that strikes you.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(10, 20));
        break;
    case SPELL_CAST:
        plr_tim_add(_AURA_DOMINATION, randint1(20) + 10);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _banish_p(mon_ptr mon)
{
    if (!mon->parent_id) return FALSE;
    if (!plr_view(mon->pos)) return FALSE;
    if (mon_is_pet(mon)) return FALSE;
    return TRUE;
}
static void _banish_summons_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Banish Summons");
        break;
    case SPELL_DESC:
        var_set_string(res, "All enemy summons in view are returned to where they came.");
        break;
    case SPELL_CAST: {
        int i;
        vec_ptr v = dun_filter_mon(cave, _banish_p);
        msg_print("You shatter all oaths of allegiance!");
        for (i = 0; i < vec_length(v); i++)
        {
            mon_ptr mon = vec_get(v, i);
            delete_monster(mon);
        }
        vec_free(v);
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}


static spell_info _domination_spells[] = {
    {  1,  1, 30, _frightful_presence_spell },
    {  5,  2, 35, _detect_minions_spell }, 
    { 10,  5, 40, _baffling_presence_spell },
    { 15, 10, 50, slow_spell },
    { 20, 15, 55, _enslave_spell },
    { 25, 25, 60, summon_kin_spell },
    { 30, 10, 60, _breathe_subjugation_spell },
    { 35, 30, 60, _aura_of_domination_spell },
    { 40, 70, 90, summon_hi_dragon_spell },
    { 45, 80, 85, _banish_summons_spell },
    { -1, -1, -1, NULL}
};

int _realm_get_spells(spell_info* spells, int max)
{
    switch (plr->dragon_realm)
    {
    case DRAGON_REALM_LORE:
        return get_spells_aux(spells, max, _lore_spells);
    case DRAGON_REALM_BREATH:
        return get_spells_aux(spells, max, _breath_spells);
    case DRAGON_REALM_ATTACK:
        return get_spells_aux(spells, max, _attack_spells);
    case DRAGON_REALM_ARMOR:
        return get_spells_aux(spells, max, _armor_spells);
    case DRAGON_REALM_CRAFT:
        return get_spells_aux(spells, max, _craft_spells);
    case DRAGON_REALM_CRUSADE:
        return get_spells_aux(spells, max, _crusade_spells);
    case DRAGON_REALM_DEATH:
        return get_spells_aux(spells, max, _death_spells);
    case DRAGON_REALM_DOMINATION:
        return get_spells_aux(spells, max, _domination_spells);
    }
    return 0;
}

static void _realm_calc_bonuses(void)
{
    switch (plr->dragon_realm)
    {
    case DRAGON_REALM_LORE:
        if (plr->lev >= 35)
            plr->telepathy = TRUE;
        if (plr->lev >= 40)
            plr->auto_id = TRUE;
        else
        {
            if (plr->lev > 20)
                plr->auto_id_sp = 10;
            plr->auto_pseudo_id = TRUE;
        }
        break;
    case DRAGON_REALM_BREATH:
        plr->to_a -= plr->lev/2;
        plr->dis_to_a -= plr->lev/2;
        break;
    case DRAGON_REALM_ATTACK:
        res_add(GF_FEAR);
        break;
    case DRAGON_REALM_ARMOR: {
        int ac = plr_prorata_level(50);
        plr->to_a += ac;
        plr->dis_to_a += ac;
        if (plr->lev >= 5)
            plr->sustain_dex = TRUE;
        if (plr->lev >= 10)
            plr->sustain_str = TRUE;
        if (plr->lev >= 15)
            plr->sustain_con = TRUE;
        if (plr->lev >= 20)
            plr->sustain_chr = TRUE;
        if (plr->lev >= 25)
            plr->hold_life++;
        if (plr->lev >= 30)
            plr->no_cut = TRUE;
        if (plr->lev >= 35)
            res_add(GF_POIS);
        if (plr->lev >= 40)
        {
            plr->reflect = TRUE;
            res_add_immune(GF_STUN);
        }
        if (plr_tim_find(T_RES_MAGIC) && plr->lev >= 30) 
            plr->magic_resistance = 5 + (plr->lev - 30) / 2;
        break; }
    case DRAGON_REALM_CRUSADE:
        plr->align += 200;
        if (plr->lev >= 15)
            plr->hold_life++;
        if (plr->lev >= 30)
            res_add(GF_FEAR);
        break;
    case DRAGON_REALM_DEATH:
        plr->align -= 200;
        break;
    case DRAGON_REALM_DOMINATION:
        res_add(GF_FEAR);
        if (plr->lev >= 30)
            plr->cult_of_personality = TRUE;
        if (plr->lev >= 50)
            res_add_immune(GF_FEAR);
        break;
    }
}

static void _realm_get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    switch (plr->dragon_realm)
    {
    case DRAGON_REALM_LORE:
        if (plr->lev >= 35)
            add_flag(flgs, OF_TELEPATHY);
        break;
    case DRAGON_REALM_ATTACK:
        add_flag(flgs, OF_RES_(GF_FEAR));
        break;
    case DRAGON_REALM_ARMOR:
        if (plr->lev >= 5)
            add_flag(flgs, OF_SUST_DEX);
        if (plr->lev >= 10)
            add_flag(flgs, OF_SUST_STR);
        if (plr->lev >= 15)
            add_flag(flgs, OF_SUST_CON);
        if (plr->lev >= 20)
            add_flag(flgs, OF_SUST_CHR);
        if (plr->lev >= 25)
            add_flag(flgs, OF_HOLD_LIFE);
        /*if (plr->lev >= 30)
            add_flag(flgs, TR_NO_CUT);*/
        if (plr->lev >= 35)
            add_flag(flgs, OF_RES_(GF_POIS));
        if (plr->lev >= 40)
        {
            add_flag(flgs, OF_REFLECT);
            /*add_flag(flgs, TR_NO_STUN);*/
        }
        if (plr_tim_find(T_RES_MAGIC) && plr->lev >= 30) 
            add_flag(flgs, OF_MAGIC_RESISTANCE); /* s/b a temp flag ... */
        break;
    case DRAGON_REALM_CRUSADE:
        if (plr->lev >= 15)
            add_flag(flgs, OF_HOLD_LIFE);
        if (plr->lev >= 30)
            add_flag(flgs, OF_RES_(GF_FEAR));
        break;
    case DRAGON_REALM_DOMINATION:
        add_flag(flgs, OF_RES_(GF_FEAR));
        if (plr->lev >= 50)
            add_flag(flgs, OF_IM_(GF_FEAR));
        break;
    }
}

/**********************************************************************
 * Dragon Bonuses (Common to all Types)
 **********************************************************************/
static void _dragon_calc_bonuses(void) 
{
    plr->skill_dig += 100;
    plr->levitation = TRUE;
    if (plr->lev >= 20)
    {
        plr->free_act++;
        plr->see_inv++;
    }
    if (plr->lev >= 30)
    {
        res_add(GF_CONF);
        /*Attack, Crusade, and Domination Realms: res_add(GF_FEAR);*/
    }
    _realm_calc_bonuses();
}

static void _dragon_get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    add_flag(flgs, OF_LEVITATION);
    if (plr->lev >= 20)
    {
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_SEE_INVIS);
    }
    if (plr->lev >= 30)
    {
        add_flag(flgs, OF_RES_(GF_CONF));
        /*Attack, Crusade, and Domination Realms: add_flag(flgs, TR_RES_FEAR);*/
    }
    _realm_get_flags(flgs);
}

/**********************************************************************
 * Dragon Powers (Common to all Types)
 **********************************************************************/
static bool _reach_begin(plr_attack_ptr context)
{
    if (!context->blow) return FALSE;
    if (context->blow->method != RBM_BITE) return FALSE;
    return TRUE;
}
static void _reach_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Reach");
        break;
    case SPELL_DESC:
        var_set_string(res, "Reach out and bite a distant monster.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_range(2 + plr->lev/40));
        break;
    case SPELL_CAST: {
        plr_attack_t context = {0};
        context.flags = PAC_ONE_BLOW;
        context.hooks.begin_weapon_f = _reach_begin;
        var_set_bool(res, plr_attack_special_aux(&context, 2 + plr->lev/40));
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _wing_storm_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Wing Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "This talent uses your wings to create massive wind gusts. Nearby foes are damaged, stunned, and blown about.");
        break;
    case SPELL_CAST:
        msg_print("You bring your wings down powerfully!");
        plr_burst(5, GF_STORM, _1d(3*plr->lev));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static power_info _dragon_powers[] = {
    { A_CON, {  1,  0, 30, _breathe_spell}},
    { A_DEX, { 20,  7,  0, _reach_spell}},
    { A_DEX, { 30, 20,  0, _wing_storm_spell}},
    {    -1, { -1, -1, -1, NULL} }
};
static power_info _steel_powers[] = {
    { A_DEX, { 20,  7,  0, _reach_spell}},
    { A_DEX, { 30, 20,  0, _wing_storm_spell}},
    {    -1, { -1, -1, -1, NULL} }
};
static int _dragon_get_powers(spell_info* spells, int max) {
    if (plr->psubrace == DRAGON_STEEL)
        return get_powers_aux(spells, max, _steel_powers);
    else
        return get_powers_aux(spells, max, _dragon_powers);
}

/**********************************************************************
 * Elemental Dragon (Red, White, Blue, Black, Green)
 *   Baby -> Young -> Mature -> Ancient -> Great Foo Wyrm
 **********************************************************************/
 typedef struct {
    cptr r_idx[5];
    int  which_res;
    cptr name; /* For Birth and Helpfiles ... */
    cptr desc;
} _elemental_info_t;

static _elemental_info_t _elemental_info[5] = { /* relies on #define DRAGON_RED 0 ... */
    { {"d.red.baby", "d.red.young", "d.red.mature", "D.red", "D.hell" },
      GF_FIRE, "Red Dragon",
        "Red Dragons are elemental dragons of fire and are the second strongest fighters among "
        "dragons. Their fiery breaths are the stuff of legends with damage unsurpassed. Even their "
        "bites are likely to burn their opponents rendering their melee damage quite impressive. "
        "As the Red Dragon matures, it becomes more and more resistant to fire, eventually gaining "
        "total immunity." },
    { {"d.white.baby", "d.white.young", "d.white.mature", "D.white", "D.ice"},
      GF_COLD, "White Dragon",
        "White Dragons are to cold what Red Dragons are to fire. Their melee is truly awe-inspiring "
        "and their icy breath can be felt even in their bite. Like Red Dragons, White Dragons "
        "have the most deadly breath possible among dragonkind and they too become more and more "
        "resistant to cold as they mature." },
    { {"d.blue.baby", "d.blue.young", "d.blue.mature", "D.blue", "D.storm"},
      GF_ELEC, "Blue Dragon",
        "Blue Dragons are elemental dragons of lightning. Their melee and breaths are not so "
        "strong as their Red and White brethren, but lightning is a bit more useful than fire or "
        "cold. Their bites eventually shock their foes. Blue Dragons become more and more resistant "
        "to lightning as they mature." },
    { {"d.black.baby", "d.black.young", "d.black.mature", "D.black", "D.bile"},
      GF_ACID, "Black Dragon",
        "Black Dragons are to acid what Blue Dragons are to lightning. Like the Blue Dragon, their "
        "breaths and melee fall short of their Red and White brethren. As they mature, their bites "
        "corrode their enemies and the Black Dragon also becomes more and more resistant to acid." },
    { {"d.green.baby", "d.green.young", "d.green.mature", "D.green", "D.venom"},
      GF_POIS, "Green Dragon",
        "Green Dragons are elemental dragons of poison. They are not so strong as Red or White dragons, "
        "but are still fearsome opponents. As they mature, their bites poison their enemies. Also, "
        "Green Dragons become more and more resistant to poison." },
};

static void _elemental_calc_bonuses(void) {
    int l = plr->lev;
    int to_a = plr_prorata_level(75);
    int ac = 15 + (l/10)*5;
    int res = _elemental_info[plr->psubrace].which_res;

    plr->ac += ac;
    plr->dis_ac += ac;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    res_add(res);
    
    if (plr->lev >= 30)
    {
        plr->pspeed += 3;
        res_add(res);
    }
    if (plr->lev >= 40)
    {
        plr->pspeed += 2;
        res_add_immune(res);
        res_add(GF_BLIND);
        switch (res)
        {
        case GF_FIRE: plr->sh_fire = TRUE; break;
        case GF_COLD: plr->sh_cold = TRUE; break;
        case GF_ELEC: plr->sh_elec = TRUE; break;
        }
    }
    _dragon_calc_bonuses();
}
static void _elemental_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    int res = _elemental_info[plr->psubrace].which_res;
    add_flag(flgs, OF_RES_(res));
    if (plr->lev >= 30)
    {
        add_flag(flgs, OF_SPEED);
    }
    if (plr->lev >= 40)
    {
        add_flag(flgs, OF_RES_(GF_BLIND));
        switch (res)
        {
        case GF_FIRE: add_flag(flgs, OF_AURA_FIRE); break;
        case GF_COLD: add_flag(flgs, OF_AURA_COLD); break;
        case GF_ELEC: add_flag(flgs, OF_AURA_ELEC); break;
        }
        add_flag(flgs, OF_IM_(res));
    }
    _dragon_get_flags(flgs);
}
static void _elemental_birth(void) { 
    plr_mon_race_set(_elemental_info[plr->psubrace].r_idx[0]); 
    _dragon_birth();
}
static bool _elemental_race_is_(int rank)
{
    cptr which = _elemental_info[plr->psubrace].r_idx[rank];
    return plr_mon_race_is_(which);
}
static void _elemental_race_evolve(int rank)
{
    cptr which = _elemental_info[plr->psubrace].r_idx[rank];
    plr_mon_race_evolve(which);
}
static void _elemental_gain_level(int new_level) {
    if (_elemental_race_is_(0) && new_level >= 10)
        _elemental_race_evolve(1);
    if (_elemental_race_is_(1) && new_level >= 20)
        _elemental_race_evolve(2);
    if (_elemental_race_is_(2) && new_level >= 30)
        _elemental_race_evolve(3);
    if (_elemental_race_is_(3) && new_level >= 40)
        _elemental_race_evolve(4);
}
static plr_race_ptr _elemental_get_race_t(int subrace)
{
    static plr_race_ptr me = NULL;
    int rank = 0;

    if (plr->lev >= 10) rank++;
    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;
    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  38,   2,  25,  26,  70,  30};
    skills_t xs = { 40,  45,  50,   0,   0,   0, 100,  35};

        me = plr_race_alloc(RACE_MON_DRAGON);
        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 250;

        me->hooks.birth = _elemental_birth;
        me->hooks.get_powers = _dragon_get_powers;
        me->hooks.calc_bonuses = _elemental_calc_bonuses;
        me->hooks.get_flags = _elemental_get_flags;
        me->hooks.gain_level = _elemental_gain_level;
    }

    me->subid = subrace;
    if (spoiler_hack || birth_hack)
        me->subname = _elemental_info[subrace].name;
    else
    {
        mon_race_ptr r = mon_race_parse(_elemental_info[subrace].r_idx[rank]);
        me->subname = r->name;
    }
    me->subdesc = _elemental_info[subrace].desc;
    me->stats[A_STR] =  1 + rank;
    me->stats[A_INT] = -1 + rank;
    me->stats[A_WIS] = -2 + rank;
    me->stats[A_DEX] = -2 + rank;
    me->stats[A_CON] =  0 + rank;
    me->stats[A_CHR] = -1 + rank;
    me->life = 100 + 5*rank;

    return me;
}

/**********************************************************************
 * Nether: Shadow Drake -> Death Drake -> Spectral Wyrm
 **********************************************************************/
static void _nether_calc_bonuses(void) {
    int l = plr->lev;
    int to_a = plr_prorata_level(75);
    int ac = 15 + (l/10)*2;

    plr->ac += ac;
    plr->dis_ac += ac;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    res_add(GF_NETHER);
    
    if (plr->lev >= 30)
    {
        plr->pspeed += 3;
        res_add(GF_COLD);
        res_add(GF_TELEPORT);
        plr->pass_wall = TRUE;
        plr->no_passwall_dam = TRUE;
    }
    if (plr->lev >= 45)
    {
        plr->align -= 200;
        plr->pspeed += 2;
        plr->sh_cold = TRUE;
        res_add(GF_POIS);
        res_add_immune(GF_NETHER);
        res_add(GF_NEXUS);
        res_add(GF_DISEN);
        res_add(GF_TELEPORT);
    }
    _dragon_calc_bonuses();
}
static void _nether_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_RES_(GF_NETHER));
    if (plr->lev >= 30)
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_COLD));
    }
    if (plr->lev >= 45)
    {
        add_flag(flgs, OF_AURA_COLD);
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_NEXUS));
        add_flag(flgs, OF_RES_(GF_DISEN));
        add_flag(flgs, OF_IM_(GF_NETHER));
    }
    _dragon_get_flags(flgs);
}
static void _nether_birth(void) { 
    plr_mon_race_set("d.shadow");
    _dragon_birth();
}
static void _nether_gain_level(int new_level) {
    if (plr_mon_race_is_("d.shadow") && new_level >= 30)
        plr_mon_race_evolve("D.death");
    if (plr_mon_race_is_("D.death") && new_level >= 45)
        plr_mon_race_evolve("D.spectral");
}
static plr_race_ptr _nether_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[3] =  {"Shadow Drake", "Death Drake", "Spectral Wyrm"};    
    int           rank = 0;

    if (plr->lev >= 30) rank++;
    if (plr->lev >= 45) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  38,   4,  25,  26,  50,  30};
    skills_t xs = { 40,  50,  55,   0,   0,   0,  75,  35};

        me = plr_race_alloc_aux(RACE_MON_DRAGON, DRAGON_NETHER);
        me->subdesc = "Shadow Drakes are bit more stealthy than your average dragon. They are creatures of nether "
            "and eventually evolve into Death Drakes. Their melee is the weakest among dragonkind and "
            "their breaths also are lacking, but they still make fearsome opponents. As they advance, "
            "these dragons eventually gain the ability to pass through walls and also become more and "
            "more resistant to nether.";

        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 300;

        me->hooks.birth = _nether_birth;
        me->hooks.get_powers = _dragon_get_powers;
        me->hooks.calc_bonuses = _nether_calc_bonuses;
        me->hooks.get_flags = _nether_get_flags;
        me->hooks.gain_level = _nether_gain_level;
    }

    if (spoiler_hack || birth_hack)
        rank = 0;

    me->subname = titles[rank];
    me->stats[A_STR] =  0 + 2*rank;
    me->stats[A_INT] = -1 + 2*rank;
    me->stats[A_WIS] = -2 + rank;
    me->stats[A_DEX] = -2 + rank;
    me->stats[A_CON] = -1 + rank;
    me->stats[A_CHR] = -1 + 3*rank;
    me->life = 90 + 5*rank;

    return me;
}

/**********************************************************************
 * Law: Law Drake -> Great Wyrm of Law
 **********************************************************************/
static void _law_calc_bonuses(void) {
    int l = plr->lev;
    int to_a = plr_prorata_level(75);
    int ac = 15 + (l/10)*2;

    plr->ac += ac;
    plr->dis_ac += ac;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    res_add(GF_SOUND);
    res_add(GF_SHARDS);

    if (plr->lev >= 40)
    {
        plr->align += 200;
        plr->pspeed += 5;
        res_add(GF_SOUND);
        res_add(GF_SHARDS);
    }

    _dragon_calc_bonuses();
}
static void _law_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_RES_(GF_SOUND));
    add_flag(flgs, OF_RES_(GF_SHARDS));
    if (plr->lev >= 40)
    {
        add_flag(flgs, OF_SPEED);
    }
    _dragon_get_flags(flgs);
}
static void _law_birth(void) { 
    plr_mon_race_set("d.law");
    _dragon_birth();
}
static void _law_gain_level(int new_level) {
    if (plr_mon_race_is_("d.law") && new_level >= 40)
        plr_mon_race_evolve("D.law");
}
static plr_race_ptr _law_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[2] =  {"Law Drake", "Great Wyrm of Law"};    
    int           rank = 0;

    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  40,  40,   2,  25,  26,  55,  30};
    skills_t xs = { 40,  55,  55,   0,   0,   0,  75,  35};

        me = plr_race_alloc_aux(RACE_MON_DRAGON, DRAGON_LAW);
        me->subdesc = "Law Drakes are powerful dragons of order. They can breathe sound or shards and eventually "
                    "evolve into Great Wyrms of Law, though not so quickly as you might hope. Their breaths "
                    "are much weaker than those of the elemental dragons but very few monsters resist sound "
                    "or shards. Their melee is among the weakest of all dragonkind but they still fight rather "
                    "well ... What dragon doesn't?";

        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 280;

        me->hooks.birth = _law_birth;
        me->hooks.get_powers = _dragon_get_powers;
        me->hooks.calc_bonuses = _law_calc_bonuses;
        me->hooks.get_flags = _law_get_flags;
        me->hooks.gain_level = _law_gain_level;
    }

    if (spoiler_hack || birth_hack)
        rank = 0;

    me->subname = titles[rank];
    me->stats[A_STR] =  0 + 5*rank;
    me->stats[A_INT] = -1 + 5*rank;
    me->stats[A_WIS] = -2 + 2*rank;
    me->stats[A_DEX] = -2 + 3*rank;
    me->stats[A_CON] = -1 + 4*rank;
    me->stats[A_CHR] = -1 + 5*rank;
    me->life = 100 + 10*rank;

    return me;
}

/**********************************************************************
 * Chaos: Chaos Drake -> Great Wyrm of Chaos
 **********************************************************************/
static void _chaos_calc_bonuses(void) {
    int l = plr->lev;
    int to_a = plr_prorata_level(75);
    int ac = 15 + (l/10)*2;

    plr->ac += ac;
    plr->dis_ac += ac;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    res_add(GF_CHAOS);
    res_add(GF_DISEN);
    
    if (plr->lev >= 40)
    {
        plr->align -= 200;
        plr->pspeed += 5;
        res_add(GF_CHAOS);
        res_add(GF_DISEN);
    }

    _dragon_calc_bonuses();
}
static void _chaos_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_RES_(GF_CHAOS));
    add_flag(flgs, OF_RES_(GF_DISEN));
    if (plr->lev >= 40)
    {
        add_flag(flgs, OF_SPEED);
    }
    _dragon_get_flags(flgs);
}
static void _chaos_birth(void) { 
    plr_mon_race_set("d.chaos");
    _dragon_birth();
}
static void _chaos_gain_level(int new_level) {
    if (plr_mon_race_is_("d.chaos") && new_level >= 40)
        plr_mon_race_evolve("D.chaos");
}
static plr_race_ptr _chaos_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[2] =  {"Chaos Drake", "Great Wyrm of Chaos"};    
    int           rank = 0;

    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  40,  40,   2,  25,  26,  55,  30};
    skills_t xs = { 40,  55,  55,   0,   0,   0,  75,  35};

        me = plr_race_alloc_aux(RACE_MON_DRAGON, DRAGON_CHAOS);
        me->subdesc = "Chaos Drakes are powerful dragons of chaos. They can breathe chaos or disenchantment and eventually "
        "evolve into Great Wyrms of Chaos, though not so quickly as you might hope. Their breaths "
        "are much weaker than those of the elemental dragons but fewer monsters resist chaos "
        "or disenchantment. Their melee is among the weakest of all dragonkind but they still fight rather "
        "well ... What dragon doesn't?";

        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 280;

        me->hooks.birth = _chaos_birth;
        me->hooks.get_powers = _dragon_get_powers;
        me->hooks.calc_bonuses = _chaos_calc_bonuses;
        me->hooks.get_flags = _chaos_get_flags;
        me->hooks.gain_level = _chaos_gain_level;
    }

    if (spoiler_hack || birth_hack)
        rank = 0;

    me->subname = titles[rank];
    me->stats[A_STR] =  0 + 5*rank;
    me->stats[A_INT] = -1 + 5*rank;
    me->stats[A_WIS] = -2 + 2*rank;
    me->stats[A_DEX] = -2 + 3*rank;
    me->stats[A_CON] = -1 + 4*rank;
    me->stats[A_CHR] = -1 + 5*rank;
    me->life = 100 + 10*rank;

    return me;
}

/**********************************************************************
 * Balance: Balance Drake -> Great Wyrm of Balance
 **********************************************************************/
static void _balance_calc_bonuses(void) {
    int l = plr->lev;
    int to_a = plr_prorata_level(75);
    int ac = 10 + (l/10)*2;

    plr->ac += ac;
    plr->dis_ac += ac;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    res_add(GF_SOUND);
    res_add(GF_SHARDS);
    res_add(GF_CHAOS);
    res_add(GF_DISEN);
    
    if (plr->lev >= 40)
    {
        plr->pspeed += 5;
    }
    _dragon_calc_bonuses();
}
static void _balance_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_RES_(GF_SOUND));
    add_flag(flgs, OF_RES_(GF_SHARDS));
    add_flag(flgs, OF_RES_(GF_CHAOS));
    add_flag(flgs, OF_RES_(GF_DISEN));
    if (plr->lev >= 40)
    {
        add_flag(flgs, OF_SPEED);
    }
    _dragon_get_flags(flgs);
}
static void _balance_birth(void) { 
    plr_mon_race_set("d.balance");
    _dragon_birth();
}
static void _balance_gain_level(int new_level) {
    if (plr_mon_race_is_("d.balance") && new_level >= 40)
        plr_mon_race_evolve("D.balance");
}
static plr_race_ptr _balance_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[2] =  {"Balance Drake", "Great Wyrm of Balance"};    
    int           rank = 0;

    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  35,   2,  25,  26,  50,  30};
    skills_t xs = { 40,  50,  50,   0,   0,   0,  75,  35};

        me = plr_race_alloc_aux(RACE_MON_DRAGON, DRAGON_BALANCE);
        me->subdesc = "Balance Drakes are a blend of Chaos and Law Drakes. They can breathe sound, shards, "
        "chaos or disenchantment and eventually evolve into Great Wyrms of Balance, though not "
        "so quickly as you might hope. Their breaths are much weaker than those of the elemental "
        "dragons and they are weaker than either of Chaos or Law Drakes, though not by much.";


        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 300;

        me->hooks.birth = _balance_birth;
        me->hooks.get_powers = _dragon_get_powers;
        me->hooks.calc_bonuses = _balance_calc_bonuses;
        me->hooks.get_flags = _balance_get_flags;
        me->hooks.gain_level = _balance_gain_level;
    }

    if (spoiler_hack || birth_hack)
        rank = 0;

    me->subname = titles[rank];
    me->stats[A_STR] =  0 + 4*rank;
    me->stats[A_INT] = -1 + 4*rank;
    me->stats[A_WIS] = -2 + 2*rank;
    me->stats[A_DEX] = -2 + 3*rank;
    me->stats[A_CON] = -1 + 3*rank;
    me->stats[A_CHR] = -1 + 5*rank;
    me->life = 95 + 10*rank;

    return me;
}

/**********************************************************************
 * Ethereal: Pseudo Dragon -> Ethereal Drake -> Ethereal Dragon
 **********************************************************************/
static void _ethereal_calc_bonuses(void) {
    int l = plr->lev;
    int to_a = plr_prorata_level(75);
    int ac = 15 + (l/10)*2;

    plr->ac += ac;
    plr->dis_ac += ac;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    res_add(GF_LIGHT);
    res_add(GF_DARK);
    
    if (plr->lev >= 20)
    {
        plr->pass_wall = TRUE;
        plr->no_passwall_dam = TRUE;
    }
    if (plr->lev >= 40)
    {
        plr->pspeed += 5;
        res_add(GF_LIGHT);
        res_add(GF_DARK);
        res_add(GF_CONF);
    }
    _dragon_calc_bonuses();
}
static void _ethereal_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_RES_(GF_LIGHT));
    add_flag(flgs, OF_RES_(GF_DARK));
    if (plr->lev >= 40)
    {
        add_flag(flgs, OF_SPEED);
    }
    _dragon_get_flags(flgs);
}
static void _ethereal_birth(void) { 
    plr_mon_race_set("d.pseudo");
    _dragon_birth();
}
static void _ethereal_gain_level(int new_level) {
    if (plr_mon_race_is_("d.pseudo") && new_level >= 20)
        plr_mon_race_evolve("d.ethereal");
    if (plr_mon_race_is_("d.ethereal") && new_level >= 40)
        plr_mon_race_evolve("D.ethereal");
}
static plr_race_ptr _ethereal_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[3] =  {"Pseudo Dragon", "Ethereal Drake", "Ethereal Dragon"};    
    int           rank = 0;

    if (plr->lev >= 20) rank++;
    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  37,   4,  25,  26,  52,  30};
    skills_t xs = { 40,  50,  55,   0,   0,   0,  75,  35};

        me = plr_race_alloc_aux(RACE_MON_DRAGON, DRAGON_ETHEREAL);
        me->subdesc =
        "Ethereal Drakes are dragons of light and darkness. They actually begin life as Pseudo "
        "Dragons but quickly evolve into Ethereal Drakes and then Ethereal Dragons. As they "
        "mature, they gain the ability to pass through walls and become more and more resistant "
        "to light, darkness and confusion. They are fairly weak fighters and have the weakest "
        "breaths in all of dragonkind (except for Steel Dragons which cannot breathe at all).";

        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 250;

        me->hooks.birth = _ethereal_birth;
        me->hooks.get_powers = _dragon_get_powers;
        me->hooks.calc_bonuses = _ethereal_calc_bonuses;
        me->hooks.get_flags = _ethereal_get_flags;
        me->hooks.gain_level = _ethereal_gain_level;
    }

    if (spoiler_hack || birth_hack)
    {
        me->subname = "Ethereal Drake";
        rank = 0;
    }
    else
        me->subname = titles[rank];

    me->stats[A_STR] =  0 + 2*rank;
    me->stats[A_INT] = -1 + 2*rank;
    me->stats[A_WIS] = -2 + rank;
    me->stats[A_DEX] = -2 + 2*rank;
    me->stats[A_CON] = -1 + 2*rank;
    me->stats[A_CHR] = -1 + 2*rank;
    me->life = 95 + 7*rank;

    return me;
}

/**********************************************************************
 * Crystal: Crystal Drake -> Great Crystal Drake
 **********************************************************************/
static void _crystal_calc_bonuses(void) {
    int l = plr->lev;
    int to_a = plr_prorata_level_aux(125, 1, 2, 2);
    int ac = 15 + (l/10)*2;

    plr->ac += ac;
    plr->dis_ac += ac;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    res_add(GF_COLD);
    res_add(GF_SHARDS);
    if (plr->lev >= 10)
    {
        plr->pspeed++;
    }    
    if (plr->lev >= 20)
    {
        plr->pspeed++;
    }
    if (plr->lev >= 30)
    {
        plr->pspeed++;
    }
    if (plr->lev >= 40)
    {
        plr->pspeed += 2;
        res_add(GF_SHARDS);
        plr->reflect = TRUE;
    }
    _dragon_calc_bonuses();
}
static void _crystal_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_SHARDS));
    if (plr->lev >= 10)
    {
        add_flag(flgs, OF_SPEED);
    }
    if (plr->lev >= 40)
    {
        add_flag(flgs, OF_REFLECT);
    }
    _dragon_get_flags(flgs);
}
static void _crystal_birth(void) { 
    plr_mon_race_set("d.crystal");
    _dragon_birth();
}
static void _crystal_gain_level(int new_level) {
    if (plr_mon_race_is_("d.crystal") && new_level >= 40)
        plr_mon_race_evolve("D.crystal");
}
static plr_race_ptr _crystal_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[2] =  {"Crystal Drake", "Great Crystal Drake"};    
    int           rank = 0;

    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  40,   1,  25,  26,  70,  30};
    skills_t xs = { 40,  35,  60,   0,   0,   0, 110,  35};

        me = plr_race_alloc_aux(RACE_MON_DRAGON, DRAGON_CRYSTAL);
        me->subdesc =
        "Crystal Drakes are dragons of a strange crystalline form. They breathe shards and melee "
        "powerfully with razor sharp claws and teeth. At high levels, they gain the power of "
        "reflection.";

        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 275;

        me->hooks.birth = _crystal_birth;
        me->hooks.get_powers = _dragon_get_powers;
        me->hooks.calc_bonuses = _crystal_calc_bonuses;
        me->hooks.get_flags = _crystal_get_flags;
        me->hooks.gain_level = _crystal_gain_level;
    }

    if (spoiler_hack || birth_hack)
        rank = 0;

    me->subname = titles[rank];
    me->stats[A_STR] =  1 + 5*rank;
    me->stats[A_INT] = -1 + 5*rank;
    me->stats[A_WIS] = -2 + 2*rank;
    me->stats[A_DEX] =  0 + 3*rank;
    me->stats[A_CON] =  0 + 4*rank;
    me->stats[A_CHR] =  0 + 3*rank;
    me->life = 100 + 15*rank;

    return me;
}

/**********************************************************************
 * Bronze: Young -> Mature -> Ancient
 **********************************************************************/
static void _bronze_calc_bonuses(void) {
    int l = plr->lev;
    int to_a = plr_prorata_level(75);
    int ac = 15 + (l/10)*2;

    plr->ac += ac;
    plr->dis_ac += ac;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    res_add(GF_CONF);
    
    if (plr->lev >= 30)
    {
        plr->pspeed += 3;
    }
    if (plr->lev >= 40)
    {
        plr->pspeed += 2;
    }
    _dragon_calc_bonuses();
}
static void _bronze_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_RES_(GF_CONF));
    if (plr->lev >= 30)
    {
        add_flag(flgs, OF_SPEED);
    }
    _dragon_get_flags(flgs);
}
static void _bronze_birth(void) { 
    plr_mon_race_set("d.bronze.young");
    _dragon_birth();
}
static void _bronze_gain_level(int new_level) {
    if (plr_mon_race_is_("d.bronze.young") && new_level >= 20)
        plr_mon_race_evolve("d.bronze.mature");
    if (plr_mon_race_is_("d.bronze.mature") && new_level >= 30)
        plr_mon_race_evolve("D.bronze");
}
static plr_race_ptr _bronze_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[3] =  {"Young Bronze Dragon", "Mature Bronze Dragon", "Ancient Bronze Dragon"};    
    int           rank = 0;

    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  38,   3,  25,  26,  55,  30};
    skills_t xs = { 40,  50,  55,   0,   0,   0,  75,  35};

        me = plr_race_alloc_aux(RACE_MON_DRAGON, DRAGON_BRONZE);
        me->subdesc =
        "Bronze Dragons are wyrms of confusion. While they are not quite as strong as most other "
        "dragons, they eventually confuse monsters with their bite attack. Also, they become "
        "more and more resistant to confusion as they mature.";

        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 250;

        me->hooks.birth = _bronze_birth;
        me->hooks.get_powers = _dragon_get_powers;
        me->hooks.calc_bonuses = _bronze_calc_bonuses;
        me->hooks.get_flags = _bronze_get_flags;
        me->hooks.gain_level = _bronze_gain_level;
    }

    if (spoiler_hack || birth_hack)
    {
        me->subname = "Bronze Dragon";
        rank = 0;
    }
    else
        me->subname = titles[rank];

    me->stats[A_STR] =  0 + 2*rank;
    me->stats[A_INT] = -1 + 2*rank;
    me->stats[A_WIS] = -2 + rank;
    me->stats[A_DEX] = -2 + 2*rank;
    me->stats[A_CON] = -1 + 2*rank;
    me->stats[A_CHR] = -1 + 2*rank;
    me->life = 100 + 5*rank;

    return me;
}

/**********************************************************************
 * Gold: Young -> Mature -> Ancient
 **********************************************************************/
static void _gold_calc_bonuses(void) {
    int l = plr->lev;
    int to_a = plr_prorata_level(75);
    int ac = 15 + (l/10)*2;

    plr->ac += ac;
    plr->dis_ac += ac;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    res_add(GF_SOUND);
    
    if (plr->lev >= 30)
    {
        plr->pspeed += 3;
    }
    if (plr->lev >= 40)
    {
        res_add(GF_SOUND);
        plr->pspeed += 2;
    }
    _dragon_calc_bonuses();
}
static void _gold_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_RES_(GF_SOUND));
    if (plr->lev >= 30)
    {
        add_flag(flgs, OF_SPEED);
    }
    _dragon_get_flags(flgs);
}
static void _gold_birth(void) { 
    plr_mon_race_set("d.gold.young");
    _dragon_birth();
}
static void _gold_gain_level(int new_level) {
    if (plr_mon_race_is_("d.gold.young") && new_level >= 20)
        plr_mon_race_evolve("d.gold.mature");
    if (plr_mon_race_is_("d.gold.mature") && new_level >= 30)
        plr_mon_race_evolve("D.gold");
}
static plr_race_ptr _gold_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[3] =  {"Young Gold Dragon", "Mature Gold Dragon", "Ancient Gold Dragon"};    
    int           rank = 0;

    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  38,   2,  25,  26,  55,  30};
    skills_t xs = { 40,  45,  55,   0,   0,   0, 100,  35};

        me = plr_race_alloc_aux(RACE_MON_DRAGON, DRAGON_GOLD);
        me->subdesc =
        "Gold Dragons are wyrms of sound. While they are not quite as strong as most other "
        "dragons, they are able to breathe sound on command, stunning their foes. Also, they become "
        "more and more resistant to sound as they mature.";

        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 250;

        me->hooks.birth = _gold_birth;
        me->hooks.get_powers = _dragon_get_powers;
        me->hooks.calc_bonuses = _gold_calc_bonuses;
        me->hooks.get_flags = _gold_get_flags;
        me->hooks.gain_level = _gold_gain_level;
    }

    if (spoiler_hack || birth_hack)
    {
        me->subname = "Gold Dragon";
        rank = 0;
    }
    else
        me->subname = titles[rank];

    me->stats[A_STR] =  0 + 2*rank;
    me->stats[A_INT] = -1 + 2*rank;
    me->stats[A_WIS] = -2 + rank;
    me->stats[A_DEX] = -2 + 2*rank;
    me->stats[A_CON] = -1 + 2*rank;
    me->stats[A_CHR] = -1 + 2*rank;
    me->life = 100 + 5*rank;

    return me;
}

/**********************************************************************
 * Steel: Stone Dragon -> Steel Dragon
 **********************************************************************/
static void _steel_calc_bonuses(void) {
    int l = plr->lev;
    int to_a = plr_prorata_level(150);
    int ac = 15 + (l/10)*2;

    plr->skill_dig += 100;
    
    plr->ac += ac;
    plr->dis_ac += ac;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    if (plr->lev < 40)
        res_add_vuln(GF_COLD);

    res_add(GF_FIRE);
    res_add(GF_ELEC);
    res_add(GF_POIS);
    plr->no_cut = TRUE;
    
    if (plr->lev >= 30)
    {
        res_add_immune(GF_STUN);
    }
    if (plr->lev >= 40)
    {
        res_add(GF_SHARDS);
        plr->pspeed += 2;
    }
    _dragon_calc_bonuses();
}
static void _steel_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_RES_(GF_ELEC));
    add_flag(flgs, OF_RES_(GF_POIS));
    if (plr->lev >= 40)
    {
        add_flag(flgs, OF_RES_(GF_SHARDS));
        add_flag(flgs, OF_SPEED);
    }
    if (plr->lev < 40)
        add_flag(flgs, OF_VULN_(GF_COLD));

    _dragon_get_flags(flgs);
}
static void _steel_birth(void) { 
    plr_mon_race_set("D.stone");
    _dragon_birth();
}
static void _steel_gain_level(int new_level) {
    if (plr_mon_race_is_("D.stone") && new_level >= 40)
        plr_mon_race_evolve("D.steel");
}
static plr_race_ptr _steel_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[2] =  {"Stone Dragon", "Steel Dragon"};    
    int           rank = 0;

    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  18,  40,   0,  10,   7,  75,  30};
    skills_t xs = { 40,  35,  75,   0,   0,   0, 150,  35};

        me = plr_race_alloc_aux(RACE_MON_DRAGON, DRAGON_STEEL);
        me->subdesc =
        "Steel Dragons are magical dragons formed from rock. As they mature, their form hardens "
        "from stone into steel. Needless to say, their armor class is phenomenal, but their "
        "dexterity actually decreases with maturity. Steel dragons begin life being susceptible "
        "to cold damage, though they will eventually outgrow this vulnerability. They are not "
        "as fast as other dragons and they have no powers whatsoever, not even the ubiquitous "
        "dragon breath! But their fighting is impossibly strong, putting all the other dragons "
        "to complete and utter shame. They also have the most hitpoints of all dragons.";

        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 250;

        me->hooks.birth = _steel_birth;
        me->hooks.calc_bonuses = _steel_calc_bonuses;
        me->hooks.get_flags = _steel_get_flags;
        me->hooks.get_powers = _dragon_get_powers;
        me->hooks.gain_level = _steel_gain_level;
    }

    if (spoiler_hack || birth_hack)
    {
        me->subname = "Steel Dragon";
        rank = 0;
    }
    else
        me->subname = titles[rank];

    me->stats[A_STR] =  5 + (plr->lev / 10);
    me->stats[A_INT] = -6;
    me->stats[A_WIS] = -6;
    me->stats[A_DEX] =  0 - (plr->lev / 10);
    me->stats[A_CON] =  4 + (plr->lev / 10);
    me->stats[A_CHR] =  0 + (plr->lev / 10);
    me->life = 125 + 5*(plr->lev / 10);

    return me;
}

/**********************************************************************
 * Public
 **********************************************************************/
plr_race_ptr mon_dragon_get_race(int psubrace)
{
    plr_race_ptr result = NULL;

    if (birth_hack && psubrace >= DRAGON_MAX)
        psubrace = 0;

    assert(0 <= psubrace && psubrace < DRAGON_MAX);

    switch (psubrace)
    {
    case DRAGON_RED:
    case DRAGON_WHITE:
    case DRAGON_BLUE:
    case DRAGON_BLACK:
    case DRAGON_GREEN:
        result = _elemental_get_race_t(psubrace);
        break;
    case DRAGON_NETHER:
        result = _nether_get_race_t();
        break;
    case DRAGON_LAW:
        result = _law_get_race_t();
        break;
    case DRAGON_CHAOS:
        result = _chaos_get_race_t();
        break;
    case DRAGON_BALANCE:
        result = _balance_get_race_t();
        break;
    case DRAGON_ETHEREAL:
        result = _ethereal_get_race_t();
        break;
    case DRAGON_CRYSTAL:
        result = _crystal_get_race_t();
        break;
    case DRAGON_BRONZE:
        result = _bronze_get_race_t();
        break;
    case DRAGON_GOLD:
        result = _gold_get_race_t();
        break;
    case DRAGON_STEEL:
        result = _steel_get_race_t();
        break;
    default: /* Birth Menus */
        result = _nether_get_race_t();
    }

    if (plr->dragon_realm && !spoiler_hack && !birth_hack)
    {
        dragon_realm_ptr realm = _get_realm();
        int              i;

        for (i = 0; i < MAX_STATS; i++)
            result->stats[i] += realm->stats[i];

        result->hooks.caster_info = _caster_info;
        result->hooks.get_spells = _realm_get_spells;
    }
    else
    {
        result->hooks.caster_info = NULL;
        result->hooks.get_spells = NULL;
    }

    result->name = "Dragon";
    result->desc = _desc;
    result->flags = RACE_IS_MONSTER;
    result->hooks.character_dump = plr->wizard ? _spoiler_dump : NULL;
    result->hooks.calc_innate_attacks = _calc_innate_attacks;
    result->hooks.calc_innate_bonuses = _calc_innate_bonuses;
    result->hooks.register_timers = _register_timers;
    result->hooks.mon_attack_init = _mon_attack_init;
    result->equip_template = plr_equip_template();
    result->base_hp = 40;
    result->pseudo_class_id = CLASS_BEASTMASTER;
    result->shop_adjust = 130;

    result->boss_r_idx = mon_race_parse("D.Glaurung")->id;
    return result;
}


