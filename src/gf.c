#include "angband.h"
#include "gf.h"

#include <assert.h>

static gf_info_t _gf_tbl[] = {
    /* Resistable Effects (Mostly Elemental, but some Status) */
    { GF_ACID, "Acid", TERM_GREEN, "ACID",
        GFF_ELEMENTAL | GFF_RESIST | GFF_DAMAGE | GFF_RIDING | GFF_DISPLAY },
    { GF_ELEC, "Lightning", TERM_BLUE, "ELEC",
        GFF_ELEMENTAL | GFF_RESIST | GFF_DAMAGE | GFF_RIDING | GFF_DISPLAY },
    { GF_FIRE, "Fire", TERM_RED, "FIRE",
        GFF_ELEMENTAL | GFF_RESIST | GFF_DAMAGE | GFF_RIDING | GFF_DISPLAY },
    { GF_COLD, "Frost", TERM_L_WHITE, "COLD",
        GFF_ELEMENTAL | GFF_RESIST | GFF_DAMAGE | GFF_RIDING | GFF_DISPLAY },
    { GF_POIS, "Poison", TERM_L_GREEN, "POISON",
        GFF_ELEMENTAL | GFF_RESIST | GFF_DAMAGE | GFF_RIDING | GFF_DISPLAY },
    { GF_LIGHT, "Light", TERM_YELLOW, "LIGHT",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE | GFF_RIDING | GFF_DISPLAY },
    { GF_DARK, "Darkness", TERM_L_DARK, "DARK",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE | GFF_RIDING | GFF_DISPLAY },
    { GF_CONFUSION, "Confusion", TERM_L_UMBER, "CONFUSION",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE | GFF_DISPLAY },
    { GF_NETHER, "Nether", TERM_L_DARK, "NETHER",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE | GFF_DISPLAY },
    { GF_NEXUS, "Nexus", TERM_VIOLET, "NEXUS",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE | GFF_DISPLAY },
    { GF_SOUND, "Sound", TERM_ORANGE, "SOUND",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE | GFF_DISPLAY },
    { GF_SHARDS, "Shards", TERM_L_UMBER, "SHARDS",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE | GFF_RIDING | GFF_DISPLAY },
    { GF_CHAOS, "Chaos", TERM_VIOLET, "CHAOS",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE | GFF_DISPLAY },
    { GF_DISENCHANT, "Disenchantment", TERM_VIOLET, "DISENCHANTMENT",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE | GFF_DISPLAY },
    { GF_TIME, "Time", TERM_L_BLUE, "TIME",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE | GFF_DISPLAY },
    { GF_WATER, "Water", TERM_L_BLUE, "WATER",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE },
    { GF_PLASMA, "Plasma", TERM_L_RED, "PLASMA",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE },
    { GF_FORCE, "Force", TERM_L_BLUE, "FORCE",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE },
    { GF_INERTIA, "Inertia", TERM_L_UMBER, "INERTIA",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE },
    { GF_GRAVITY, "Gravity", TERM_L_UMBER, "GRAVITY",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE },
    { GF_DISINTEGRATE, "Disintegration", TERM_L_DARK, "DISINTEGRATE",
        GFF_ELEMENTAL | GFF_RESIST_HI | GFF_DAMAGE },
    { GF_BLIND, "Blind", TERM_L_DARK, "BLIND",
        GFF_RESIST | GFF_STATUS | GFF_HIDE | GFF_DISPLAY },
    { GF_FEAR, "Fear", TERM_RED, "FEAR",
        GFF_RESIST | GFF_STATUS | GFF_DISPLAY },
    { GF_STUN, "Stun", TERM_L_BLUE, "STUN",
        GFF_RESIST | GFF_STATUS | GFF_HIDE },
    { GF_TELEPORT, "Teleport", TERM_L_BLUE, "TELEPORT",
        GFF_RESIST | GFF_TELEPORT },
    { GF_SLEEP, "Sleep", TERM_BLUE, "SLEEP",
        GFF_RESIST | GFF_STATUS | GFF_HIDE },
    { GF_SLOW, "Slow", TERM_L_UMBER, "SLOW",
        GFF_RESIST | GFF_STATUS | GFF_HIDE },

    { GF_MANA, "Mana", TERM_L_BLUE, "MANA", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_NUKE, "Toxic Waste", TERM_L_GREEN, "NUKE", GFF_ELEMENTAL | GFF_DAMAGE, GF_POIS },
    { GF_STORM, "Storm Winds", TERM_BLUE, "STORM", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_HOLY_FIRE, "Holy Fire", TERM_YELLOW, "HOLY_FIRE", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_HELL_FIRE, "Hell Fire", TERM_L_DARK, "HELL_FIRE", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_ICE, "Ice", TERM_L_WHITE, "ICE", GFF_ELEMENTAL | GFF_DAMAGE | GFF_RIDING, GF_COLD },
    { GF_ROCKET, "Rocket", TERM_RED, "ROCKET", GFF_ELEMENTAL | GFF_DAMAGE | GFF_NO_REFLECT, GF_SHARDS },
    { GF_METEOR, "Meteor", TERM_RED, "METEOR", GFF_ELEMENTAL | GFF_DAMAGE | GFF_NO_REFLECT },
    { GF_ROCK, "Rock", TERM_L_UMBER, "ROCK", GFF_ELEMENTAL | GFF_DAMAGE | GFF_NO_REFLECT },
    { GF_ARROW, "Arrow", TERM_L_UMBER, "ARROW", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_SUPER_ARROW, "Arrow", TERM_L_UMBER, "ARROW", GFF_ELEMENTAL | GFF_DAMAGE | GFF_NO_REFLECT },
    { GF_MISSILE, "Missile", TERM_L_UMBER, "MISSILE", GFF_ELEMENTAL | GFF_DAMAGE }, /* perhaps GFF_PHYSICAL? */

    /* Curses */
    { GF_CAUSE_1, "Cause Light Wounds", TERM_RED, "CAUSE_1", GFF_CURSE | GFF_DAMAGE | GFF_HIDE | GFF_NO_REFLECT },
    { GF_CAUSE_2, "Cause Serious Wounds", TERM_RED, "CAUSE_2", GFF_CURSE | GFF_DAMAGE | GFF_HIDE | GFF_NO_REFLECT },
    { GF_CAUSE_3, "Cause Critical Wounds", TERM_RED, "CAUSE_3", GFF_CURSE | GFF_DAMAGE | GFF_HIDE | GFF_NO_REFLECT },
    { GF_CAUSE_4, "Cause Mortal Wounds", TERM_RED, "CAUSE_4", GFF_CURSE | GFF_DAMAGE | GFF_HIDE | GFF_NO_REFLECT },
    { GF_HAND_DOOM, "Hand of Doom", TERM_VIOLET, "HAND_DOOM", GFF_CURSE | GFF_DAMAGE | GFF_HIDE | GFF_NO_REFLECT },
    { GF_BLOOD_CURSE, "Blood Curse", TERM_VIOLET, "BLOOD_CURSE", GFF_CURSE | GFF_DAMAGE | GFF_HIDE | GFF_NO_REFLECT },

    /* Mental Attacks */
    { GF_PSY_SPEAR, "Psycho-Spear", TERM_L_BLUE, "PSY_SPEAR", GFF_MENTAL | GFF_DAMAGE },
    { GF_PSI, "Psionics", TERM_L_BLUE, "PSI", GFF_MENTAL | GFF_DAMAGE },
    { GF_PSI_DRAIN, "Psionic Drain", TERM_L_BLUE, "PSI_DRAIN", GFF_MENTAL | GFF_HIDE },
    { GF_PSI_BRAIN_SMASH, "Brain Smash", TERM_L_BLUE, "PSI_BRAIN_SMASH", GFF_MENTAL | GFF_DAMAGE | GFF_HIDE },
    { GF_PSI_STORM, "Psycho-Storm", TERM_L_BLUE, "PSI_STORM", GFF_MENTAL | GFF_DAMAGE },
    { GF_TELEKINESIS, "Pulverise", TERM_L_BLUE, "TELEKINESIS", GFF_MENTAL | GFF_DAMAGE },
    { GF_DOMINATION, "Domination", TERM_RED, "DOMINATION", GFF_MENTAL | GFF_HIDE },
    { GF_SUBJUGATION, "Subjugation", TERM_RED, "SUBJUGATION", GFF_MENTAL },
    { GF_DRAIN_MANA, "Drain Mana", TERM_L_BLUE, "DRAIN_MANA", GFF_MENTAL | GFF_HIDE },
    { GF_MIND_BLAST, "Mind Blast", TERM_L_BLUE, "MIND_BLAST", GFF_MENTAL | GFF_HIDE },
    { GF_BRAIN_SMASH, "Brain Smash", TERM_L_BLUE, "BRAIN_SMASH", GFF_MENTAL | GFF_DAMAGE | GFF_HIDE },
    { GF_AMNESIA, "Amnesia", TERM_L_DARK, "AMNESIA", GFF_MENTAL | GFF_HIDE },

    /* Status Effects */
    { GF_OLD_CLONE, "Clone", TERM_RED, "OLD_CLONE", GFF_STATUS | GFF_HIDE },
    { GF_OLD_POLY, "Polymorph", TERM_RED, "OLD_POLY", GFF_STATUS | GFF_HIDE },
    { GF_OLD_HEAL, "Heal", TERM_WHITE, "OLD_HEAL", GFF_STATUS | GFF_HIDE | GFF_TARGET_PET },
    { GF_STAR_HEAL, "Heal", TERM_WHITE, "STAR_HEAL", GFF_STATUS | GFF_HIDE },
    { GF_OLD_SPEED, "Haste", TERM_L_RED, "OLD_SPEED", GFF_STATUS | GFF_HIDE | GFF_TARGET_PET },
    { GF_OLD_CONF, "Confuse", TERM_L_UMBER, "OLD_CONF", GFF_STATUS | GFF_HIDE },
    { GF_OLD_DRAIN, "Drain", TERM_L_DARK, "OLD_DRAIN", GFF_STATUS },
    { GF_STASIS, "Freeze", TERM_BLUE, "STASIS", GFF_STATUS | GFF_HIDE },
    { GF_STASIS_EVIL, "Freeze Evil", TERM_BLUE, "STASIS_EVIL", GFF_STATUS | GFF_HIDE },
    { GF_PARALYSIS, "Paralyze", TERM_VIOLET, "PARALYZE", GFF_STATUS | GFF_HIDE },
    { GF_ELDRITCH, "Eldritch Horror", TERM_VIOLET, "ELDRITCH", GFF_STATUS }, /* warlock's Eldritch Blast */
    { GF_ANTIMAGIC, "Anti-magic", TERM_RED, "ANTIMAGIC", GFF_STATUS | GFF_HIDE },
    { GF_CRUSADE, "Crusade", TERM_WHITE, "CRUSADE", GFF_STATUS | GFF_HIDE },
    { GF_UNHOLY_WORD, "Unholy Word", TERM_L_DARK, "UNHOLY_WORD", GFF_STATUS | GFF_HIDE },
    { GF_UNLIFE, "Unlife", TERM_L_DARK, "UNLIFE", GFF_STATUS | GFF_HIDE },

    /* Terrain Effects */
    { GF_LIGHT_WEAK, "Light", TERM_YELLOW, "LIGHT_WEAK", GFF_TERRAIN },
    { GF_DARK_WEAK, "Dark", TERM_L_DARK, "DARK_WEAK", GFF_TERRAIN },
    { GF_KILL_WALL, "Stone to Mud", TERM_L_UMBER, "KILL_WALL", GFF_TERRAIN },
    { GF_KILL_DOOR, "Door Destruction", TERM_RED, "KILL_DOOR", GFF_TERRAIN },
    { GF_KILL_TRAP, "Trap Destruction", TERM_RED, "KILL_TRAP", GFF_TERRAIN },
    { GF_REMOVE_OBSTACLE, "Remove Obstacle", TERM_RED, "REMOVE_OBSTACLE", GFF_TERRAIN },
    { GF_MAKE_DOOR, "Door Creation", TERM_L_BLUE, "MAKE_DOOR", GFF_TERRAIN | GFF_HIDE },
    { GF_MAKE_TRAP, "Trap Creation", TERM_L_RED, "MAKE_TRAP", GFF_TERRAIN | GFF_HIDE },
    { GF_MAKE_TREE, "Forest Creation", TERM_L_GREEN, "MAKE_TREE", GFF_TERRAIN | GFF_HIDE },
    { GF_MAKE_GLYPH, "Glyph of Warding", TERM_L_BLUE, "MAKE_GLYPH", GFF_TERRAIN | GFF_HIDE },
    { GF_MAKE_WALL, "Wall Creation", TERM_SLATE, "MAKE_WALL", GFF_TERRAIN | GFF_HIDE },
    { GF_JAM_DOOR, "Wizard Lock", TERM_RED, "JAM_DOOR", GFF_TERRAIN },
    { GF_WATER_FLOW, "Flow of Water", TERM_BLUE, "WATER_FLOW", GFF_TERRAIN | GFF_HIDE },
    { GF_WATER2, "Flow of Water", TERM_BLUE, "WATER2", GFF_TERRAIN | GFF_HIDE },
    { GF_LAVA_FLOW, "Flow of Lava", TERM_RED, "LAVA_FLOW", GFF_TERRAIN | GFF_HIDE },
    { GF_WEB, "Web Spinning", TERM_SLATE, "WEB", GFF_TERRAIN | GFF_HIDE },
    { GF_QUAKE, "Earthquake", TERM_L_UMBER, "QUAKE", GFF_TERRAIN | GFF_HIDE },

    /* Turning, Dispelling, Controlling, etc */
    { GF_AWAY_UNDEAD, "Banish Undead", TERM_L_BLUE, "AWAY_UNDEAD", GFF_TELEPORT },
    { GF_AWAY_EVIL, "Banish Evil", TERM_L_BLUE, "AWAY_EVIL", GFF_TELEPORT },
    { GF_ISOLATION, "Isolation", TERM_L_BLUE, "ISOLATION", GFF_TELEPORT },
    { GF_TURN_UNDEAD, "Turn Undead", TERM_RED, "TURN_UNDEAD", GFF_STATUS },
    { GF_TURN_EVIL, "Turn Evil", TERM_RED, "TURN_EVIL", GFF_STATUS },
    { GF_DISP_UNDEAD, "Dispel Undead", TERM_L_RED, "DISP_UNDEAD", GFF_DAMAGE },
    { GF_DISP_EVIL, "Dispel Evil", TERM_L_RED, "DISP_EVIL", GFF_DAMAGE },
    { GF_DISP_GOOD, "Dispel Good", TERM_L_RED, "DISP_GOOD", GFF_DAMAGE },
    { GF_DISP_DEMON, "Dispel Demon", TERM_L_RED, "DISP_DEMON", GFF_DAMAGE },
    { GF_DISP_LIVING, "Dispel Living", TERM_L_RED, "DISP_LIVING", GFF_DAMAGE },
    { GF_DISP_ALL, "Dispel Monsters", TERM_L_RED, "DISP_ALL", GFF_DAMAGE },
    { GF_CONTROL_UNDEAD, "Enslave Undead", TERM_L_BLUE, "CONTROL_UNDEAD", GFF_CHARM },
    { GF_CONTROL_DEMON, "Dominate Demon", TERM_L_BLUE, "CONTROL_DEMON", GFF_CHARM },
    { GF_CONTROL_ANIMAL, "Charm Animal", TERM_L_BLUE, "CONTROL_ANIMAL", GFF_CHARM },
    { GF_CONTROL_LIVING, "Charm Living", TERM_L_BLUE, "CONTROL_LIVING", GFF_CHARM },
    { GF_CONTROL_PACT_MONSTER, "Control Pact Monster", TERM_L_BLUE, "CONTROL_PACT_MONSTER", GFF_CHARM },
    { GF_CHARM, "Charm Monster", TERM_L_BLUE, "CHARM", GFF_CHARM },
    { GF_CHARM_RING_BEARER, "Charm Ring Bearer", TERM_L_BLUE, "CHARM_RING_BEARER", GFF_CHARM },
    { GF_CAPTURE, "Capture Monster", TERM_L_BLUE, "CAPTURE", GFF_CHARM },
    { GF_ANIM_DEAD, "Raise Dead", TERM_L_DARK, "ANIM_DEAD", GFF_HIDE },
    { GF_DEATH_RAY, "Death Ray", TERM_L_DARK, "DEATH_RAY" },
    { GF_GENOCIDE, "Genocide", TERM_L_DARK, "GENOCIDE", GFF_HIDE },

    /* Object Effects */
    { GF_IDENTIFY, "Identify", TERM_L_BLUE, "IDENTIFY", GFF_OBJECT | GFF_HIDE },

    /* Class Specific */
    { GF_ATTACK, "Attack", TERM_RED, "ATTACK", GFF_SPECIAL | GFF_HIDE | GFF_NO_REFLECT },
    { GF_ENGETSU, "Moon Dazzling", TERM_YELLOW, "ENGETSU", GFF_SPECIAL | GFF_HIDE },
    { GF_SEEKER, "Seeker Ray", TERM_YELLOW, "SEEKER", GFF_SPECIAL | GFF_DAMAGE },
    { GF_SUPER_RAY, "Super Ray", TERM_YELLOW, "SUPER_RAY", GFF_SPECIAL | GFF_DAMAGE },
    { GF_BLOOD, "Blood", TERM_RED, "BLOOD", GFF_SPECIAL },
    { GF_ELDRITCH_STUN, "Eldritch Stun", TERM_L_BLUE, "ELDRITCH_STUN", GFF_SPECIAL },
    { GF_ELDRITCH_DRAIN, "Eldritch Drain", TERM_L_DARK, "ELDRITCH_DRAIN", GFF_SPECIAL },
    { GF_ELDRITCH_DISPEL, "Eldritch Dispel", TERM_L_RED, "ELDRITCH_DISPEL", GFF_SPECIAL | GFF_DAMAGE },
    { GF_ELDRITCH_CONFUSE, "Eldritch Confuse", TERM_L_UMBER, "ELDRITCH_CONFUSE", GFF_SPECIAL },
    { GF_ELDRITCH_HOWL, "Eldritch Howl", TERM_L_DARK, "ELDRITCH_HOWL", GFF_SPECIAL },
    { GF_ENTOMB, "Entomb", TERM_L_UMBER, "ENTOMB", GFF_SPECIAL | GFF_HIDE },
    { GF_MANA_CLASH, "Mana Clash", TERM_L_BLUE, "MANA_CLASH", GFF_SPECIAL | GFF_HIDE },
    { GF_PHARAOHS_CURSE, "Pharaoh's Curse", TERM_VIOLET, "PHARAOHS_CURSE", GFF_SPECIAL | GFF_CURSE | GFF_DAMAGE | GFF_HIDE },
    { GF_DRAINING_TOUCH, "Draining Touch", TERM_L_DARK, "DRAINING_TOUCH", GFF_SPECIAL | GFF_HIDE },
    { GF_DEATH_TOUCH, "Touch of Death", TERM_L_DARK, "DEATH_TOUCH", GFF_SPECIAL | GFF_HIDE },
    { GF_STEAL, "Steal", TERM_WHITE, "STEAL", GFF_SPECIAL | GFF_HIDE },
    { GF_LICH_DRAIN, "Orb of Draining", TERM_L_DARK, "LICH_DRAIN", GFF_DAMAGE },
    { GF_LICH_GENOCIDE, "Send to Netherworld", TERM_L_DARK, "LICH_GENOCIDE", GFF_HIDE },
    { GF_LICH_WORD, "Word of Vecna", TERM_VIOLET, "LICH_WORD", GFF_STATUS | GFF_DAMAGE | GFF_HIDE },

    /* New Stuff ... Reorder later */
    { GF_FRIENDSHIP, "Friendship", TERM_ORANGE, "FRIENDSHIP", GFF_SPECIAL | GFF_HIDE },
    { GF_OBEDIENCE, "Obedience", TERM_ORANGE, "OBEDIENCE", GFF_SPECIAL | GFF_HIDE },
    { GF_EXORCISM, "Exorcism", TERM_L_DARK, "EXORCISM", GFF_HIDE },
    { GF_REPENTANCE, "Repentance", TERM_ORANGE, "REPENTANCE", GFF_SPECIAL | GFF_HIDE },
    
    { 0 }
};

typedef struct {
    cptr parse;
    int id;
} _alias_t, *_alias_ptr;
static _alias_t _aliases[] = {
    { "DAM", GF_MISSILE },
    { "POIS", GF_POIS },
    { "CONF", GF_CONFUSION },
    { "CONFUSE", GF_CONFUSION },
    { "DISENCHANT", GF_DISENCHANT },
    { "DISEN", GF_DISENCHANT }, /* XXX RES_DISEN in k_info, etc */
    { "PULVERISE", GF_TELEKINESIS },
    { "TERRIFY", GF_FEAR },
    { "TELE", GF_TELEPORT },
    { "POLYMORPH", GF_OLD_POLY },
    { 0 }
};

gf_info_ptr gf_parse_name(cptr token)
{
    static str_map_ptr _map = NULL;
    if (!_map)
    {
        int i;
        _map = str_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            gf_info_ptr info = &_gf_tbl[i];
            if (!info->id) break;
            str_map_add(_map, info->parse, info);
        }
        for (i = 0; ; i++)
        {
            _alias_ptr alias = &_aliases[i];
            if (!alias->parse) break;
            str_map_add(_map, alias->parse, gf_lookup(alias->id));
        }
    }
    return str_map_find(_map, token);
}

gf_info_ptr gf_lookup(int id)
{
    static int_map_ptr _map = NULL;
    if (!_map)
    {
        int i;
        _map = int_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            gf_info_ptr info = &_gf_tbl[i];
            if (!info->id) break;
            int_map_add(_map, info->id, info);
        }
    }
    return int_map_find(_map, id);
}

int gf_resist(int id)
{
    int res_id = id;
    gf_info_ptr gfi = gf_lookup(id);
    if (gfi->resist)
        res_id = gfi->resist;
    if (GF_RES_MIN <= res_id && res_id <= GF_RES_MAX)
        return res_id;
    return GF_NONE;
}

#define HURT_CHANCE 16

static int _rlev(who_t who)
{
    if (who_is_mon(who))
        return mon_lvl(who_mon(who));
    return 0; /* XXX cave->difficulty? */
}
static int _plr_save_odds(who_t who, int boost)
{
    int rlev = _rlev(who);
    int roll = 100 + rlev/2 + boost;
    int sav = plr_skill_sav(who);
    int odds = sav * 100 / roll;
    return odds;
}
static bool _plr_save(who_t who, int boost)
{
    int odds = _plr_save_odds(who, boost);
    return randint0(100) < odds;
}
int plr_holy_dam(int dam)
{
    return dam * holy_align_dam_pct(plr->align) / 100;
}
int plr_hell_dam(int dam)
{
    return dam * hell_align_dam_pct(plr->align) / 100;
}
int gf_affect_p(who_t who, int type, int dam, int flags)
{
    int          result = 0;
    mon_ptr      m_ptr = NULL;
    mon_race_ptr r_ptr = NULL;
    int          rlev = 1;
    char         m_name[MAX_NLEN], m_name_subject[MAX_NLEN];
    bool         aura = BOOL(flags & GF_AFFECT_AURA);
    bool         touch = BOOL(flags & (GF_AFFECT_AURA | GF_AFFECT_ATTACK));
    bool         fuzzy = plr_tim_find(T_BLIND) && (flags & GF_AFFECT_SPELL);
    int          damage_type = aura ? DAMAGE_NOESCAPE : DAMAGE_ATTACK;
    int          stat_drain_odds = aura ? 3 * HURT_CHANCE : HURT_CHANCE;

    #if 0
    if (flags & GF_AFFECT_SPELL)
        msg_format("<color:D>gf_affect_p(%s, %d)</color>", gf_lookup(type)->name, dam);
    #endif
    if (who_is_mon(who))
    {
        m_ptr = who_mon(who);
        r_ptr = m_ptr->race;
        rlev = MAX(1, r_ptr->alloc.lvl*m_ptr->mpower/1000);

        monster_desc(m_name, m_ptr, 0);
        monster_desc(m_name_subject, m_ptr, MD_PRON_VISIBLE);
    }
    else
    {
        switch (who.tag)
        {
        case WHO_UNCTRL_POWER:
            strcpy(m_name, "uncontrollable power storm");
            break;

        case WHO_TRAP:
        default:
            strcpy(m_name, "a trap");
            break;
        }
    }

    /* Analyze the damage */
    switch (type)
    {
    case GF_ACID:
        dam = res_calc_dam(GF_ACID, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:G>dissolved</color>!");
            else if (fuzzy) msg_print("You are hit by acid!");
            if (aura || !CHECK_MULTISHADOW())
            {
                if (!res_save_default(GF_ACID) && one_in_(stat_drain_odds))
                    do_dec_stat(A_CHR);
                if (minus_ac()) dam = (dam + 1) / 2;
            }
            result = take_hit(damage_type, dam, m_name);
            if (!aura || randint0(50) < dam)
                inven_damage(set_acid_destroy, 3, GF_ACID);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_ACID);
        break;
    case GF_FIRE:
        dam = res_calc_dam(GF_FIRE, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:r>burned</color>!");
            else if (fuzzy) msg_print("You are hit by fire!");
            if (aura || !CHECK_MULTISHADOW())
            {
                if (!res_save_default(GF_FIRE) && one_in_(stat_drain_odds))
                    do_dec_stat(A_STR);
            }

            result = take_hit(damage_type, dam, m_name);
            if (!aura || randint0(50) < dam)
                 inven_damage(set_fire_destroy, 3, GF_FIRE);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_FIRE);
        break;
    case GF_COLD:
        dam = res_calc_dam(GF_COLD, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:W>frozen</color>!");
            else if (fuzzy) msg_print("You are hit by cold!");
            if (aura || !CHECK_MULTISHADOW())
            {
                if (!res_save_default(GF_COLD) && one_in_(stat_drain_odds))
                    do_dec_stat(A_STR);
            }
            result = take_hit(damage_type, dam, m_name);
            if (!aura || randint0(50) < dam)
                 inven_damage(set_cold_destroy, 3, GF_COLD);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_COLD);
        break;
    case GF_ELEC:
        dam = res_calc_dam(GF_ELEC, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:b>shocked</color>!");
            else if (fuzzy) msg_print("You are hit by lightning!");
            if (aura || !CHECK_MULTISHADOW())
            {
                if (!res_save_default(GF_ELEC) && one_in_(stat_drain_odds))
                    do_dec_stat(A_DEX);
            }
            result = take_hit(damage_type, dam, m_name);
            if (!aura || randint0(50) < dam)
                 inven_damage(set_elec_destroy, 3, GF_ELEC);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_ELEC);
        break;
    case GF_POIS:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_POIS, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:G>poisoned</color>!");
            else if (fuzzy) msg_print("You are hit by poison!");
            /* Moving damage from immediate to delayed can't simply leave the
             * value unchanged, else this is a monster nerf! We can scale everything
             * in r_info and BR_POIS, but that is tedious and I'm unsure what a good
             * scale factor is without some playtesting. cf GF_NUKE below. */
            plr_tim_add(T_POISON, dam*4/3);
            result = take_hit(damage_type, dam/3, m_name);
            if (!res_save_default(GF_POIS) && one_in_(stat_drain_odds))
                do_dec_stat(A_CON);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_POIS);
        break;
    case GF_NUKE:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_POIS, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:G>irradiated</color>!");
            else if (fuzzy) msg_print("You are hit by radiation!");
            plr_tim_add(T_POISON, dam*4/3);
            result = take_hit(damage_type, dam/3, m_name);
            if (!res_save_default(GF_POIS))
            {
                if (one_in_(5))
                {
                    msg_print("You undergo a freakish metamorphosis!");
                    if (one_in_(4))
                        do_poly_self();
                    else
                        mutate_player();
                }
                inven_damage(set_acid_destroy, 2, GF_POIS);
            }
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_POIS);
        break;
    case GF_MISSILE:
    case GF_BLOOD:  /* Monsters can't do this ... */
        if (fuzzy) msg_print("You are hit by something!");
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_HOLY_FIRE: {
        int pct = holy_align_dam_pct(plr->align);
        dam = dam * pct / 100;
        if (!dam)
        {
            if (!touch) msg_print("You are immune.");
        }
        else
        {
            if (touch) msg_format("You are <color:y>%s</color>!", pct > 100 ? "*burned*" : "burned");
            else if (fuzzy) msg_print("You are hit by something!");
            result = take_hit(damage_type, dam, m_name);
        }
        break; }
    case GF_HELL_FIRE: {
        int pct = hell_align_dam_pct(plr->align);
        dam = dam * pct / 100;
        if (!dam)
        {
            if (!touch) msg_print("You are immune.");
        }
        else
        {
            if (touch) msg_format("You are <color:D>%s</color>!", pct > 100 ? "*burned*" : "burned");
            else if (fuzzy) msg_print("You are hit by something!");
            result = take_hit(damage_type, dam, m_name);
        }
        break; }
    case GF_ARROW:
    case GF_SUPER_ARROW:
        if (fuzzy) msg_print("You are hit by something sharp!");
        else if (equip_find_art("|.Zantetsuken"))
        {
            msg_print("You cut down the arrow!");
            break;
        }
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_PLASMA:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_PLASMA, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:R>burned</color>!");
            else if (fuzzy) msg_print("You are hit by something *HOT*!");
            result = take_hit(damage_type, dam, m_name);
            if (!res_save(GF_STUN, 100) && !res_save_default(GF_SOUND))
            {
                int k = (randint1((dam > 40) ? 35 : (dam * 3 / 4 + 5)));
                plr_tim_add(T_STUN, k);
            }

            if (!touch) inven_damage(set_acid_destroy, 3, GF_FIRE);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_PLASMA);
        break;
    case GF_UNLIFE:
        if (!(get_race()->flags & RACE_IS_NONLIVING) && !life_save_p(rlev))
        {
            plr_drain_life(dam);
            if (touch && m_ptr)
            {
                m_ptr->mpower += dam;
                msg_format("<color:R>%^s grows more powerful!</color>", m_name_subject);
            }
        }
        break;
    case GF_NETHER:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_NETHER, dam);
        if (dam)
        {
            int unlife;
            if (touch) msg_print("You are <color:D>drained</color>!");
            else if (fuzzy) msg_print("You are hit by nether forces!");
            unlife = dam*15/100;
            if (unlife)
            {
                gf_affect_p(who, GF_UNLIFE, unlife, flags);
                dam -= unlife;
            }
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_NETHER);
        break;
    case GF_WATER:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_WATER, dam);
        if (dam)
        {
            if (fuzzy) msg_print("You are hit by something wet!");
            if (plr->resist[GF_WATER] > 0)
            {
            }
            else
            {
                if (!res_save(GF_STUN, 100) && !res_save_default(GF_SOUND))
                    plr_tim_add(T_STUN, randint1(40));
                if (!res_save_default(GF_CONF))
                    plr_tim_add(T_CONFUSED, randint1(5) + 5);
                inven_damage(set_cold_destroy, 3, GF_SOUND);
            }
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_WATER);
        break;
    case GF_CHAOS:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_CHAOS, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:v>unmade</color>!");
            else if (fuzzy) msg_print("You are hit by a wave of anarchy!");
            if (!res_save_default(GF_CONF))
                plr_tim_add(T_CONFUSED, randint0(20) + 10);
            if (!res_save_default(GF_CHAOS))
            {
                int count = mut_count(mut_unlocked_pred);
                if (prace_is_(RACE_BEASTMAN)) count = 0;
                if (one_in_(3 + count*count))
                {
                    msg_print("Your body is twisted by chaos!");
                    mut_gain_random(NULL);
                }
                if (plr->pclass == CLASS_WILD_TALENT && one_in_(7))
                    wild_talent_scramble();

                if (!mut_present(MUT_WEIRD_MIND))
                    plr_tim_add(T_HALLUCINATE, randint1(10));
            }
            if (!res_save_default(GF_NETHER) && !res_save_default(GF_CHAOS))
                drain_exp(5000 + (plr->exp / 100), 500 + (plr->exp / 1000), 75);

            if (!touch)
            {
                inven_damage(set_elec_destroy, 2, GF_CHAOS);
                inven_damage(set_fire_destroy, 2, GF_CHAOS);
            }
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_CHAOS);
        break;
    case GF_ROCK:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        if (fuzzy) msg_print("You are hit by something solid!");
        if (one_in_(2))
        {
            if (!plr->no_cut && !res_save_default(GF_SHARDS))
                plr_tim_add(T_CUT, dam/2);
            inven_damage(set_cold_destroy, 2, GF_SHARDS);
        }
        else
        {
            if (!res_save(GF_STUN, 100) && !res_save_default(GF_SOUND))
            {
                int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
                plr_tim_add(T_STUN, k);
            }
            inven_damage(set_cold_destroy, 2, GF_SOUND);
        }
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_SHARDS:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_SHARDS, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:U>shredded</color>!");
            else if (fuzzy) msg_print("You are hit by something sharp!");
            if (!plr->no_cut && !res_save_default(GF_SHARDS))
                plr_tim_add(T_CUT, dam);
            if (!touch) inven_damage(set_cold_destroy, 2, GF_SHARDS);
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_SHARDS);
        break;
    case GF_SOUND:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_SOUND, dam);
        if (dam)
        {
            if (!touch && fuzzy) msg_print("You are hit by a loud noise!");
            if (!res_save(GF_STUN, 100) && !res_save_default(GF_SOUND))
            {
                int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
                plr_tim_add(T_STUN, k);
            }
            if (!touch) inven_damage(set_cold_destroy, 2, GF_SOUND);
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_SOUND);
        break;
    case GF_CONFUSION:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_CONFUSION, dam);
        if (dam)
        {
            if (!touch && fuzzy) msg_print("You are hit by something puzzling!");
            if (!res_save_default(GF_CONFUSION))
                plr_tim_add(T_CONFUSED, _1d(20) + 10);
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_CONFUSION);
        break;
    case GF_DISENCHANT:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_DISENCHANT, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:v>disenchanted</color>!");
            else if (fuzzy) msg_print("You are hit by something static!");
            if (!(flags & GF_AFFECT_SPELL) && !one_in_(5))
            {
                if (!res_save_default(GF_DISENCHANT) || one_in_(5))
                    plr_tim_disenchant();
            }
            else if (plr->prace == RACE_MON_SWORD && one_in_(touch ? 13 : 6))
                sword_disenchant();
            else if (!res_save(GF_DISEN, 31) && !CHECK_MULTISHADOW())
                apply_disenchant(0);
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_DISENCHANT);
        break;
    case GF_NEXUS:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_NEXUS, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:v>scrambled</color>!");
            else if (fuzzy) msg_print("You are hit by something strange!");
            if (!res_save_default(GF_NEXUS))
                apply_nexus(m_ptr);
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_NEXUS);
        break;
    case GF_FORCE:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_FORCE, dam);
        if (dam)
        {
            if (fuzzy) msg_print("You are hit by kinetic force!");
            if (!res_save(GF_STUN, 100) && !res_save_default(GF_SOUND))
                plr_tim_add(T_STUN, randint1(20));
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_FORCE);
        break;
    case GF_ROCKET:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_SHARDS, dam);
        if (dam)
        {
            if (fuzzy) msg_print("There is an explosion!");
            if (!res_save(GF_STUN, 100) && !res_save_default(GF_SOUND))
                plr_tim_add(T_STUN, randint1(20));
            if (!plr->no_cut && !res_save_default(GF_SHARDS))
                plr_tim_add(T_CUT, (dam / 2));
            inven_damage(set_cold_destroy, 3, GF_SHARDS);
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_SHARDS);
        break;
    case GF_INERTIA:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_INERTIA, dam);
        if (dam)
        {
            if (!touch && fuzzy) msg_print("You are hit by something slow!");
            if (!CHECK_MULTISHADOW() /*&& !free_act_save_p(MAX(rlev, dam))*/)
                plr_tim_add(T_SLOW, randint0(4) + 4);
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_INERTIA);
        break;
    case GF_LIGHT:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_LIGHT, dam);
        if (plr_tim_find(T_WRAITH)) /* cf take_hit, which will do dam /= 2. We want the plr to take */
            dam *= 2;               /* full damage from this hit (but don't want annoying torch damage) */
        if (dam)
        {
            if (touch) msg_print("You are <color:y>dazzled</color>!");
            else if (fuzzy) msg_print("You are hit by something!");
            if (!plr_tim_find(T_BLIND) && !res_save_default(GF_LIGHT) && !res_save_default(GF_BLIND))
                plr_tim_add(T_BLIND, _1d(5) + 2);

            result = take_hit(damage_type, dam, m_name);
            if (prace_is_(RACE_MON_VAMPIRE))
                vampire_take_light_damage(dam);

            if (plr_tim_find(T_WRAITH) && !touch)
            {
                msg_print("The light forces you out of your incorporeal shadow form.");
                plr_tim_remove(T_WRAITH);
            }
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_LIGHT);
        break;
    case GF_DARK:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_DARK, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:D>benighted</color>!");
            else if (fuzzy) msg_print("You are hit by something!");
            if (!plr_tim_find(T_BLIND) && !res_save_default(GF_DARK) && !res_save_default(GF_BLIND))
                plr_tim_add(T_BLIND, _1d(5) + 2);
            result = take_hit(damage_type, dam, m_name);
            if (prace_is_(RACE_MON_VAMPIRE))
                vampire_take_dark_damage(dam);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_DARK);
        break;
    case GF_ELDRITCH:
        if (touch && m_ptr)
            sanity_blast(m_ptr, FALSE);
        break;
    case GF_STUN:
        if (!res_save(GF_STUN, 100))
            plr_tim_add(T_STUN, dam);
        if (m_ptr) mon_smart_learn(m_ptr, GF_STUN);
        break;
    case GF_AMNESIA:
        if (_plr_save(who, 0))
        {
            if (!touch) msg_print("You resist the effects!");
        }
        else if (lose_all_info())
        {
            msg_print("Your memories fade away.");
        }
        break;
    case GF_TIME:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_TIME, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:B>chronosmashed</color>!");
            else if (fuzzy) msg_print("You are hit by a blast from the past!");
            if (!res_save_default(GF_TIME))
            {
                int k = A_STR;
                cptr act = "";
                switch (randint1(10))
                {
                case 1: case 2: case 3: case 4: case 5:
                    if (plr->prace == RACE_ANDROID) break;
                    msg_print("You feel life has clocked back.");
                    lose_exp(100 + (plr->exp / 100) * MON_DRAIN_LIFE);
                    break;

                case 6: case 7: case 8: case 9:
                    switch (randint1(6))
                    {
                        case 1: k = A_STR; act = "strong"; break;
                        case 2: k = A_INT; act = "bright"; break;
                        case 3: k = A_WIS; act = "wise"; break;
                        case 4: k = A_DEX; act = "agile"; break;
                        case 5: k = A_CON; act = "hale"; break;
                        case 6: k = A_CHR; act = "confident"; break;
                    }
                    msg_format("You're not as %s as you used to be...", act);
                    plr->stat_cur[k] = (plr->stat_cur[k] * 3) / 4;
                    if (plr->stat_cur[k] < 3) plr->stat_cur[k] = 3;
                    plr->update |= (PU_BONUS);
                    break;
                case 10:
                    msg_print("You're not as powerful as you used to be...");
                    for (k = 0; k < 6; k++)
                    {
                        plr->stat_cur[k] = (plr->stat_cur[k] * 7) / 8;
                        if (plr->stat_cur[k] < 3) plr->stat_cur[k] = 3;
                    }
                    plr->update |= (PU_BONUS);
                    break;
                }
            }
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_TIME);
        break;
    case GF_STORM:
        msg_print("You are hit by gale force winds!");
        if (!CHECK_MULTISHADOW())
        {
            teleport_player(5, TELEPORT_PASSIVE);
            if (!plr->levitation)
                plr_tim_add(T_SLOW, randint0(4) + 4);
            if (!plr->levitation && !res_save(GF_STUN, 100) && !res_save_default(GF_SOUND))
            {
                int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
                plr_tim_add(T_STUN, k);
            }
        }
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_GRAVITY:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_GRAVITY, dam);
        if (dam)
        {
            if (!touch && fuzzy) msg_print("You are hit by something heavy!");
            msg_print("<color:U>Gravity warps around you.</color>");
            if (!CHECK_MULTISHADOW())
            {
                teleport_player(5, TELEPORT_PASSIVE);
                if (!plr->levitation)
                    plr_tim_add(T_SLOW, randint0(4) + 4);
                if (!plr->levitation && !res_save(GF_STUN, 100) && !res_save_default(GF_SOUND))
                {
                    int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
                    plr_tim_add(T_STUN, k);
                }
            }
            if (plr->levitation)
            {
                dam = (dam * 2) / 3;
            }
            inven_damage(set_cold_destroy, 2, GF_SOUND);
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_GRAVITY);
        break;
    case GF_DISINTEGRATE:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(GF_DISINTEGRATE, dam);
        if (dam)
        {
            if (fuzzy) msg_print("You are hit by pure energy!");
            result = take_hit(damage_type, dam, m_name);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_DISINTEGRATE);
        break;
    case GF_OLD_HEAL:
        if (fuzzy) msg_print("You are hit by something invigorating!");

        hp_player(dam);
        break;
    case GF_OLD_SPEED:
        if (fuzzy) msg_print("You are hit by something!");

        plr_tim_add(T_FAST, randint1(5));
        break;
    case GF_SLOW:
        if (fuzzy) msg_print("You are hit by something slow!");
        if (!free_act_save_p(MAX(rlev, dam)))
            plr_tim_add(T_SLOW, randint0(4) + 4);
        break;
    case GF_SLEEP:
        if (!free_act_save_p(rlev))
        {
            if (fuzzy) msg_print("You fall asleep!");
            plr_tim_add(T_PARALYZED, _1d(2));
        }
        break;
    case GF_BLIND:
        if (res_save_default(GF_BLIND) || (!touch && _plr_save(who, 0)))
            msg_print("You resist the effects!");
        else
            plr_tim_add(T_BLIND, 12 + randint0(4));
        if (m_ptr) mon_smart_learn(m_ptr, GF_BLIND);
        break;
    case GF_OLD_CONF:
        if (res_save_default(GF_CONFUSION) || _plr_save(who, 0))
            msg_print("You disbelieve the feeble spell.");
        else
            plr_tim_add(T_CONFUSED, randint0(4) + 4);
        if (m_ptr) mon_smart_learn(m_ptr, GF_CONFUSION);
        break;
    case GF_FEAR:
        if (fuzzy) msg_print("Your will is shaken!");
        fear_scare_p(m_ptr);
        if (m_ptr) mon_smart_learn(m_ptr, GF_FEAR);
        break;
    case GF_PARALYSIS:
        if (free_act_save_p(rlev) || (!touch && _plr_save(who, dam)))
            msg_print("You resist the effects!");
        else
            plr_tim_add(T_PARALYZED, randint1(3));
        if (m_ptr) mon_smart_learn(m_ptr, SM_FREE_ACTION);
        break;
    case GF_MANA:
    case GF_SEEKER:
    case GF_SUPER_RAY:
        if (fuzzy) msg_print("You are hit by a touch of magic!");
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_PSY_SPEAR:
        if (fuzzy) msg_print("You are hit by pure energy!");
        result = take_hit(DAMAGE_FORCE, dam, m_name);
        break;
    case GF_METEOR:
        if (fuzzy) msg_print("Something falls from the sky on you!");
        result = take_hit(damage_type, dam, m_name);
        inven_damage(set_fire_destroy, 2, GF_FIRE);
        inven_damage(set_cold_destroy, 2, GF_SHARDS);
        break;
    case GF_ICE:
        if (touch) msg_print("You are <color:W>frozen</color>!");
        else if (fuzzy) msg_print("You are hit by something sharp and cold!");
        result = gf_affect_p(who, GF_COLD, dam, 0);
        if (!CHECK_MULTISHADOW())
        {
            if (!plr->no_cut && !res_save_default(GF_SHARDS))
                plr_tim_add(T_CUT, (touch ? damroll(3, 5) : damroll(5, 8)));
            if (!res_save(GF_STUN, 100) && !res_save_default(GF_SOUND))
                plr_tim_add(T_STUN, (touch ? randint1(7) : randint1(15)));
            inven_damage(set_cold_destroy, 3, GF_COLD);
        }
        if (m_ptr) mon_smart_learn(m_ptr, GF_COLD);
        break;
    case GF_DEATH_RAY:
        if (fuzzy) msg_print("You are hit by something extremely cold!");
        if (!(get_race()->flags & RACE_IS_NONLIVING))
            result = take_hit(damage_type, dam, m_name);
        break;
    case GF_DRAIN_MANA:
        if (CHECK_MULTISHADOW())
        {
            if (!touch) msg_print("The attack hits Shadow, you are unharmed!");
        }
        else if (psion_mental_fortress())
        {
            if (!touch) msg_print("Your mental fortress is impenetrable!");
        }
        else if ( prace_is_(RACE_DEMIGOD)
                && plr->psubrace == DEMIGOD_HERA
                && randint1(100) > r_ptr->alloc.lvl - 2*(plr->stat_ind[A_WIS] + 3))
        {
            if (!touch) msg_print("You keep your wits about you!");
        }
        else if (plr->csp)
        {
            if (who_is_mon(who)) msg_format("%^s draws psychic energy from you!", m_name);
            else msg_print("Your psychic energy is drawn!");
            if (dam >= plr->csp)
            {
                dam = plr->csp;
                plr->csp = 0;
                plr->csp_frac = 0;
            }
            else
                plr->csp -= dam;

            plr->redraw |= (PR_MANA);
            plr->window |= (PW_SPELL);

            if (m_ptr && m_ptr->hp < m_ptr->maxhp)
            {
                m_ptr->hp += (6 * dam);
                if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;
                check_mon_health_redraw(m_ptr);
                if (m_ptr->ml)
                    msg_format("%^s appears healthier.", m_name);
            }
        }
        break;
    case GF_MIND_BLAST:
        if (_plr_save(who, dam/5) && !CHECK_MULTISHADOW())
        {
            if (!touch)
            {
                msg_print("You resist the effects!");
            }
        }
        else if (!CHECK_MULTISHADOW())
        {
            msg_print("Your mind is blasted by psionic energy.");

            if (!res_save_default(GF_CONF))
                plr_tim_add(T_CONFUSED, randint0(4) + 4);

            if (!res_save_default(GF_CHAOS) && !mut_present(MUT_WEIRD_MIND) && one_in_(3))
                plr_tim_add(T_HALLUCINATE, randint0(25) + 15);

            plr->csp -= (touch || plr->pclass == CLASS_RUNE_KNIGHT) ? 10 : 50;
            if (plr->csp < 0)
            {
                plr->csp = 0;
                plr->csp_frac = 0;
            }
            plr->redraw |= PR_MANA;
        }
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_BRAIN_SMASH:
        if (_plr_save(who, dam/3) && !CHECK_MULTISHADOW())
        {
            if (!touch)
            {
                msg_print("You resist the effects!");
            }
        }
        else
        {
            if ( prace_is_(RACE_DEMIGOD)
              && plr->psubrace == DEMIGOD_HERA
              && randint1(100) > r_ptr->alloc.lvl - 2*(plr->stat_ind[A_WIS] + 3))
            {
                if (!touch) msg_print("You keep your wits about you!");
            }
            else if (!CHECK_MULTISHADOW())
            {
                msg_print("Your mind is blasted by psionic energy.");
                if (touch || plr->pclass == CLASS_RUNE_KNIGHT)
                    plr->csp -= 10;
                else
                    plr->csp -= 100;
                if (plr->csp < 0)
                {
                    plr->csp = 0;
                    plr->csp_frac = 0;
                }
                plr->redraw |= PR_MANA;
            }
            result = take_hit(damage_type, dam, m_name);
            if (!CHECK_MULTISHADOW())
            {
                if (!res_save_default(GF_BLIND))
                    plr_tim_add(T_BLIND, 8 + randint0(8));
                if (!res_save_default(GF_CONF))
                    plr_tim_add(T_CONFUSED, randint0(4) + 4);
                if (!free_act_save_p(rlev))
                    plr_tim_add(T_PARALYZED, randint1(4));

                if (!touch || !free_act_save_p(rlev))
                    plr_tim_add(T_SLOW, randint0(4) + 4);
                if (!res_save(GF_STUN, 100))
                    plr_tim_add(T_STUN, MIN(50, dam/6 + randint1(dam/6)));

                while (!_plr_save(who, 0))
                    do_dec_stat(A_INT);
                while (!_plr_save(who, 0))
                    do_dec_stat(A_WIS);

                if (!res_save_default(GF_CHAOS) && !mut_present(MUT_WEIRD_MIND))
                    plr_tim_add(T_HALLUCINATE, randint0(25) + 15);
            }
        }
        break;
    case GF_TELEKINESIS:
        if (!CHECK_MULTISHADOW())
        {
            if (one_in_(4))
                teleport_player(5, TELEPORT_PASSIVE);
            if (!res_save(GF_STUN, 100) && !_plr_save(who, dam/5))
                plr_tim_add(T_STUN, MIN(25, dam/6 + randint1(dam/6)));
        }
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_CAUSE_1:
        if (_plr_save(who, dam/5) && !CHECK_MULTISHADOW())
        {
            if (!touch)
            {
                msg_print("You resist the effects!");
            }
        }
        else
        {
            if (!CHECK_MULTISHADOW()) curse_equipment(15, 0);
            result = take_hit(damage_type, dam, m_name);
        }
        break;
    case GF_CAUSE_2:
        if (_plr_save(who, dam/5) && !CHECK_MULTISHADOW())
        {
            if (!touch)
            {
                msg_print("You resist the effects!");
            }
        }
        else
        {
            if (!CHECK_MULTISHADOW()) curse_equipment(25, MIN(rlev / 2 - 15, 5));
            result = take_hit(damage_type, dam, m_name);
        }
        break;
    case GF_CAUSE_3:
        if (_plr_save(who, dam/5) && !CHECK_MULTISHADOW())
        {
            if (!touch)
            {
                msg_print("You resist the effects!");
            }
        }
        else
        {
            if (!CHECK_MULTISHADOW()) curse_equipment(33, MIN(rlev / 2 - 15, 15));
            result = take_hit(damage_type, dam, m_name);
        }
        break;
    case GF_CAUSE_4:
        if (_plr_save(who, dam/5) && !mon_race_is_(m_ptr->race, "p.Kenshirou") && !CHECK_MULTISHADOW())
        {
            if (!touch)
            {
                msg_print("You resist the effects!");
            }
        }
        else
        {
            result = take_hit(damage_type, dam, m_name);
            if (!plr->no_cut && !CHECK_MULTISHADOW()) plr_tim_add(T_CUT, damroll(10, 10));
        }
        break;
    case GF_HAND_DOOM:
        if (_plr_save(who, 0) && !CHECK_MULTISHADOW())
        {
            if (!touch)
            {
                msg_print("You resist the effects!");
            }
        }
        else
        {
            if (!CHECK_MULTISHADOW())
            {
                msg_print("You feel your life fade away!");
                curse_equipment(40, 20);
            }

            result = take_hit(damage_type, dam, m_name);

            if (plr->chp < 1) plr->chp = 1; /* Paranoia */
        }
        break;
    case GF_OLD_POLY:
        if ( prace_is_(RACE_ANDROID)
          || plr->pclass == CLASS_MONSTER
          || plr->prace == RACE_DOPPELGANGER
          || mut_present(MUT_DRACONIAN_METAMORPHOSIS) )
        {
            if (flags & GF_AFFECT_SPELL)
                msg_print("You are unaffected!");
        }
        else if (_plr_save(who, 0))
        {
            if (flags & GF_AFFECT_SPELL)
                msg_print("You resist the effects!");
        }
        else
        {
            int which;
            switch(randint1(5))
            {
            case 1:
                if (plr->prace != RACE_SNOTLING)
                {
                    which = RACE_SNOTLING;
                    break;
                }
            case 2:
                if (plr->prace != RACE_YEEK)
                {
                    which = RACE_YEEK;
                    break;
                }
            case 3:
                which = MIMIC_SMALL_KOBOLD;
                break;
            case 4:
                which = MIMIC_MANGY_LEPER;
                break;
            default:
                which = plr_race_polymorph();
            }
            set_mimic(50 + randint1(50), which, FALSE);
        }
        break;
    case GF_ATTACK:
        mon_attack(m_ptr, plr->pos);
        break;
    }
    return result;
}
static bool _mon_save_aux(mon_ptr mon, int power)
{
    if (mon_save_aux(mon, power)) return TRUE;
    if (mon_is_unique(mon) && mon_save_aux(mon, power)) return TRUE;
    return FALSE;
}
static bool _mon_save_p(mon_ptr mon, int stat)
{
    if (mon_save_p(mon, stat)) return TRUE;
    if (mon_is_unique(mon) && mon_save_p(mon, stat)) return TRUE;
    return FALSE;
}
bool gf_affect_m(who_t who, mon_ptr mon, int gf, int dam, int flags)
{
    int tmp,gfr;

    point_t       where = mon->pos;
    monster_type *caster_ptr = who_mon(who);
    int           caster_lev = caster_ptr ? mon_lvl(caster_ptr) : spell_power(plr->lev * 2);
    bool          touch = BOOL(flags & (GF_AFFECT_AURA | GF_AFFECT_ATTACK));
    bool          quiet = BOOL(flags & GF_AFFECT_QUIET);

    monster_race *race = mon->race;

    char killer[80];

    /* Is the monster "seen"? Note: mon_show_msg() requires
     * the monster sqaure to be lit if ignore_unview is on. This
     * means that if the player is lobbing fireballs at a telepathically
     * seen monster, they would miss the resistance message. So, if
     * the player is doing this, then we just use ml instead. If the
     * player is not doing this affect, then we better respect ignore_unview
     * or we'll turn the message spammer back on full blast! */
    bool seen = mon->ml;
    bool seen_msg = who_is_plr(who) ? mon->ml : mon_show_msg(mon);

    bool slept = BOOL(mon_tim_find(mon, MT_SLEEP));

    /* Were the effects "obvious" (if seen)? */
    bool obvious = FALSE;

    /* Can the player know about this effect? */
    bool known = mon->cdis <= MAX_SIGHT;

    /* Were the effects "irrelevant"? */
    bool skipped = FALSE;

    /* Gets the monster angry at the source of the effect? */
    bool get_angry = FALSE;

    /* Various effects: These all happen at the end provided mon survives dam */
    int do_poly = 0;
    int do_dist = 0;
    int do_conf = 0;
    int do_blind = 0;
    int do_stun = 0;
    int do_sleep = 0;
    int do_paralyzed = 0;
    int do_fear = 0;
    int do_time = 0;
    int res_pct = 0;
    gf_info_ptr gfi;

    bool heal_leper = FALSE;

    /* Hold the monster name */
    char m_name[MAX_NLEN];
    char m_name_object[MAX_NLEN];

    char m_poss[80];

    /* Assume no note */
    cptr note = NULL;

    /* Assume a default death */
    cptr note_dies = extract_note_dies(real_r_ptr(mon));

    int ty = mon->pos.y;
    int tx = mon->pos.x;


    bool who_is_pet = FALSE;
    if (caster_ptr && mon_is_pet(caster_ptr))
        who_is_pet = TRUE;

    /* Never affect projector */
    if (caster_ptr == mon) return FALSE;

    if (who_is_plr(who) && mon->id == plr->riding)
    {
        switch (gf)
        {
        case GF_OLD_HEAL:
        case GF_OLD_SPEED:
        case GF_STAR_HEAL:
        case GF_CRUSADE:
        case GF_UNHOLY_WORD:
            break;
        default:
            return FALSE;
        }
    }

    /* Don't affect already dead monsters */
    /* Prevents problems with chain reactions of exploding monsters */
    if (mon->hp < 0) return (FALSE);

    #if 0
    if (plr->riding == mon->id && (flags & GF_AFFECT_SPELL))
        msg_format("<color:D>gf_affect_m(%s, %d)</color>", gf_lookup(gf)->name, dam);
    #endif

    /* Big hack here ... Jump Spiders are immune to the jumps of their kin
     * (They come in packs, and it would be silly for them to destroy each other) */
    if (mon_spell_current() && race->spells)
    {
        mon_spell_ptr spell = mon_spell_current()->spell;
        mon_spell_group_ptr group = race->spells->groups[MST_TACTIC];
        if (spell->id.type == MST_TACTIC && group && mon_spell_group_find(group, spell->id))
        {
            mon_tim_delete(mon, MT_SLEEP);
            return FALSE;
        }
    }

    /* Get the monster name (BEFORE polymorphing) */
    if (flags & GF_AFFECT_SPELL)
    {
        monster_desc(m_name, mon, 0);
        monster_desc(m_name_object, mon, 0);
    }
    else
    {
        monster_desc(m_name, mon, MD_PRON_VISIBLE);
        monster_desc(m_name_object, mon, MD_PRON_VISIBLE | MD_OBJECTIVE);
    }
    /* Get the monster possessive ("his"/"her"/"its") */
    monster_desc(m_poss, mon, MD_PRON_VISIBLE | MD_POSSESSIVE);

    if (plr->riding && mon->id == plr->riding) disturb(1, 0);

    /* Handle resistances ... some gf codes use other codes for resistance,
     * notably NUKE, ROCKET and ICE. 'gfr' tracks the gf code used for resistance.
     * 'gf' is needed un-altered for the giant switch statement. */
    gfr = gf;
    gfi = gf_lookup(gfr);
    if (!gfi) return FALSE;
    if (gfi->resist)
    {
        gfr = gfi->resist;
        gfi = gf_lookup(gfr);
    }
    if (gfi->flags & (GFF_RESIST | GFF_RESIST_HI))
    {
        res_pct = mon_res_pct(mon, gfr);
        if (gfi->flags & GFF_DAMAGE)
        {
            dam -= dam * res_pct / 100;
            if (dam < 0) dam = 0;
            if (!quiet)
            {
                if (res_pct == 100) note = " is immune.";
                else if (res_pct > 0) note = " resists.";
                else if (res_pct < 0) note = " is hit hard.";
            }
            if (res_pct != 0)
                mon_lore_resist(mon, gfr);
        }
    }

    switch (gf)
    {
    case GF_ACID:
        if (touch && seen_msg && dam) msg_format("%^s is <color:G>dissolved</color>!", m_name);
        if (seen) obvious = TRUE;
        break;
    case GF_ELEC:
        if (touch && seen_msg && dam) msg_format("%^s is <color:b>shocked</color>!", m_name);
        if (seen) obvious = TRUE;
        break;
    case GF_FIRE:
        if (touch && seen_msg && dam) msg_format("%^s is <color:r>burned</color>!", m_name);
        if (seen) obvious = TRUE;
        break;
    case GF_COLD:
        if (touch && seen_msg && dam) msg_format("%^s is <color:W>frozen</color>!", m_name);
        if (seen) obvious = TRUE;
        break;
    case GF_POIS:
        if (touch && seen_msg && dam) msg_format("%^s is <color:G>poisoned</color>!", m_name);
        if (seen) obvious = TRUE;
        break;
    case GF_LIGHT:
        if (touch && seen_msg && dam) msg_format("%^s is <color:y>dazzled</color>!", m_name);
        if (seen) obvious = TRUE;
        if (res_pct < 0) /* flavor ... HURT_LITE is now VULN(LITE) */
        {
            note = " cringes from the light!";
            note_dies = " shrivels away in the light!";
        }
        break;
    case GF_DARK:
        if (touch && seen_msg && dam) msg_format("%^s is <color:D>benighted</color>!", m_name);
        if (seen) obvious = TRUE;
        break;
    case GF_CONFUSION:
        if (touch && seen_msg && dam) msg_format("%^s is <color:U>baffled</color>!", m_name);
        if (seen) obvious = TRUE;
        if (res_pct <= 0)
            do_conf = 10 + _1d(15);
        break;
    case GF_NETHER:
        if (touch && seen_msg && dam) msg_format("%^s is <color:D>drained</color>!", m_name);
        if (seen) obvious = TRUE;
        /* XXX Living monsters get a weak nether resistance XXX */
        if (!res_pct && mon_is_nonliving(mon))
        {
            if (!quiet) note = " resists somewhat.";
            dam = dam*3/4;
            mon_lore_nonliving(mon);
        }
        break;
    case GF_NEXUS:
        if (touch && seen_msg && dam) msg_format("%^s is <color:v>scrambled</color>!", m_name);
        if (seen) obvious = TRUE;
        break;
    case GF_SOUND:
        if (seen) obvious = TRUE;
        if (res_pct <= 0)
        {
            if (who_is_plr(who) && mon_save_stun(race->alloc.lvl, dam))
            {
                if (!quiet) note = " resists stunning.";
            }
            else
                do_stun = mon_stun_amount(dam);
        }
        break;
    case GF_SHARDS:
        if (touch && seen_msg && dam) msg_format("%^s is <color:U>shredded</color>!", m_name);
        if (seen) obvious = TRUE;
        break;
    case GF_CHAOS:
        if (touch && seen_msg && dam) msg_format("%^s is <color:v>unmade</color>!", m_name);
        if (seen) obvious = TRUE;

        /* XXX XXX XXX
        else if (mon_is_demon(mon) && one_in_(3))
        {
            if (!quiet) note = " resists somewhat.";
            dam *= 3; dam /= randint1(6) + 6;
            mon_lore_demon(mon);
        } */

        if (res_pct == 0)
        {
            if (touch && plr->prace == RACE_MON_VORTEX) /* XXX Aether and Chaos vortices XXX */
                do_poly = dam / 5;
            else
                do_poly = dam;

            do_conf = 5 + _1d(11);
        }
        break;
    case GF_DISENCHANT:
        if (touch && seen_msg && dam) msg_format("%^s is <color:v>disenchanted</color>!", m_name);
        if (seen) obvious = TRUE;
        if (res_pct <= 0)
        {
            if (who_is_plr(who) && !mon_save_disenchant(race->id, dam, flags))
                mon_tim_disenchant(mon);
        }
        break;
    case GF_TIME:
        if (touch && seen_msg && dam) msg_format("%^s is <color:B>chronosmashed</color>!", m_name);
        if (seen) obvious = TRUE;
        if (res_pct > 0) break;
        if (who_is_plr(who))
        {
            bool unique = mon_race_is_unique(race);

            if (mon_save_time(race->id, dam, flags))
            {
                if (plr->wizard)
                    note = " resists the ravages of time.";
            }
            else
            {
                int which = randint1(100);
                if (which <= 15)
                {
                    if (unique && mon_save_time(race->id, dam, flags))
                    {
                        if (plr->wizard)
                            note = " resists being slowed.";
                    }
                    else mon_tim_add(mon, T_SLOW, 10 + randint1(15));
                }
                else if (which <= 20)
                {
                    if (devolve_monster(mon->id, plr->wizard))
                        return TRUE;
                }
                else if (which <= 25)
                {
                    if (evolve_monster(mon->id, plr->wizard))
                        return TRUE;
                }
                else if (which <= 40)
                {
                    if (unique && mon_save_time(race->id, dam, flags))
                    {
                        if (plr->wizard)
                            note = " resists being suspended.";
                    }
                    else
                    {
                        /*note = " is suspended!";*/
                        do_paralyzed = 5;
                    }
                }
                else if (which <= 50)
                {
                    mon->ac_adj -= randint1(10);
                    note = " is more exposed!";
                }
                else if (which <= 60)
                {
                    mon_spell_ptr spell = mon_spell_random(mon);
                    if (spell)
                    {
                        int parm = mon_spell_pack(spell->id);
                        mon_tim_add_aux(mon, MT_AMNESIA, 10 + randint1(10), parm);
                    }
                    else
                        do_time = (dam + 1) / 2;
                }
                else if (which <= 70)
                {
                    mon->mspeed -= randint1(2);
                    note = " is permanently slowed!";
                }
                else if (which <= 80)
                {
                    mon->mpower = MAX(300, mon->mpower * (850 + randint0(100)) / 1000);
                    note = " shrinks!";
                }
                else
                {
                    note = " is suddenly sluggish.";
                    mon->energy_need += ENERGY_NEED();
                }
            }
        }
        else
            do_time = (dam + 1) / 2;
        break;
    case GF_WATER: /* GF_WATER2 is for terrain effects only */
        if (seen) obvious = TRUE;
        if (res_pct <= 0)
        {
            if (who_is_plr(who) && mon_save_stun(race->alloc.lvl, dam))
            {
                if (!quiet) note = " resists stunning.";
            }
            else do_stun = mon_stun_amount(dam);
        }
        break;
    case GF_PLASMA:
        if (touch && seen_msg && dam) msg_format("%^s is <color:R>burned</color>!", m_name);
        if (seen) obvious = TRUE;
        if (res_pct <= 0)
        {
            if (who_is_plr(who) && mon_save_stun(race->alloc.lvl, dam))
            {
                if (!quiet) note = " resists stunning.";
            }
            else do_stun = mon_stun_amount(dam);
        }
        break;
    case GF_FORCE:
        if (seen) obvious = TRUE;
        if (res_pct <= 0)
            do_stun = mon_stun_amount(dam);
        break;
    case GF_INERTIA:
        if (seen) obvious = TRUE;
        if (res_pct <= 0)
        {
            /* Powerful monsters can resist */
            if ( mon_save_slow(race->alloc.lvl, dam)
              || (mon_race_is_unique(race) && mon_save_slow(race->alloc.lvl + 15, dam)) )
            {
                obvious = FALSE;
            }
            /* Normal monsters slow down */
            else mon_tim_add(mon, T_SLOW, 50);
        }
        break;
    case GF_STORM: /* TODO */
    case GF_GRAVITY: {
        if (seen) obvious = TRUE;

        /* 1. teleport */
        if (plr->riding && (mon->id == plr->riding)) {} /* XXX */
        else if (touch && !one_in_(5)) {} /* XXX Aether vortex XXX */
        else if (_1d(100) > mon_res_pct(mon, GF_TELEPORT))
            do_dist = 10;
        else
            mon_lore_resist(mon, GF_TELEPORT);

        if (res_pct <= 0)
        {
            /* 2. slow */
            if ( mon_save_slow(race->alloc.lvl, dam)
              || (mon_race_is_unique(race) && mon_save_slow(race->alloc.lvl + 15, dam)) )
            {
            }
            else mon_tim_add(mon, T_SLOW, 50);

            /* 3. stun */
            if (_1d(100) > mon_res_pct(mon, GF_STUN))
            {
                do_stun = mon_stun_amount(dam);

                if ( mon_save_stun(race->alloc.lvl, dam)
                  || (mon_race_is_unique(race) && mon_save_stun(race->alloc.lvl + 15, dam)) )
                {
                    do_stun = 0;
                }
            }
        }
        break; }
    case GF_DISINTEGRATE:
        if (touch && seen_msg) msg_format("%^s is <color:D>disintegrated</color>!", m_name);
        if (seen) obvious = TRUE;
        if (res_pct < 0) /* XXX HURT_ROCK is now VULN(DISINTEGRATE) */
        {
            note = " loses some skin!";
            note_dies = " evaporates!";
        }
        break;

    case GF_MISSILE:
        /* XXX Note that mon vs mon *melee* currently uses gf_affect_m(GF_MISSILE) to
         * handle damaging the monster. See _effect_mon in mon_attack.c. */
        if (seen) obvious = TRUE;
        break;
    case GF_BLOOD:
    case GF_ELDRITCH:
    case GF_ELDRITCH_DRAIN:  /* Lazy ... I'll give back hp later */
        if (seen) obvious = TRUE;
        break;
    case GF_MANA_CLASH:
        if (seen) obvious = TRUE;
        if (!race->spells || !race->spells->freq)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
        }
        else /* 900 max dam coming in ... ~600 max dam going out */
            dam = dam * MIN(66, race->spells->freq) / 100;
        break;
    case GF_NUKE:
        if (touch && seen_msg && dam) msg_format("%^s is <color:G>irradiated</color>!", m_name);
        if (seen) obvious = TRUE;
        if (dam && one_in_(3)) do_poly = dam;
        break;
    case GF_HELL_FIRE: {
        int pct = hell_align_dam_pct(race->align); /* XXX race->align not mon->align */
        if (seen) obvious = TRUE;
        dam = dam * pct / 100;
        if (!dam)
            note = " is immune.";
        else if (pct > 100)
        {
            if (touch && seen_msg) msg_format("%^s is <color:D>*burned*</color>!", m_name);
            if (!touch) note = " is hit hard.";
        }
        else
        {
            if (touch && seen_msg) msg_format("%^s is <color:D>burned</color>!", m_name);
        }
        mon_lore_align(mon);
        break; }
    case GF_HOLY_FIRE: {
        int pct = holy_align_dam_pct(race->align); /* XXX race->align not mon->align */
        if (seen) obvious = TRUE;
        dam = dam * pct / 100;
        if (!dam)
            note = " is immune.";
        else if (pct > 100)
        {
            if (touch && seen_msg) msg_format("%^s is <color:y>*burned*</color>!", m_name);
            if (!touch) note = " is hit hard.";
        }
        else
        {
            if (touch && seen_msg) msg_format("%^s is <color:y>burned</color>!", m_name);
            if (!touch && pct < 100) note = " resists.";
        }
        mon_lore_align(mon);
        break; }
    case GF_ARROW:
    case GF_SUPER_ARROW:
        if (seen) obvious = TRUE;
        break;
    case GF_UNLIFE:
        if (mon_is_nonliving(mon))
        {
            mon_lore_nonliving(mon);
            if (seen_msg && !quiet) msg_format("%^s is unaffected.", m_name);
        }
        else if (who_is_plr(who) && _mon_save_aux(mon, plr->lev + dam/2))
        {
            if (seen_msg && !quiet) msg_format("%^s resists.", m_name);
        }
        else
        {
            /* XXX Diminishing returns on player GF_UNLIFE attacks. Consider
             * polymorphing the monster into an undead form instead if life hits 0 */
            if (who_is_plr(who))
                dam = dam*mon->mpower/1000;
            if (mon->mpower - dam < 0)
                dam = mon->mpower;
            mon->mpower -= dam;
            if (seen_msg)
            {
                if (who_is_plr(who))
                    msg_format("<color:D>You drain life from %s.</color>", m_name_object);
                else
                    msg_format("<color:D>%^s grows less powerful.</color>", m_name);
            }
            if (who_is_plr(who))
            {
                if (get_race()->flags & RACE_IS_UNDEAD)
                {
                    if (!mon_is_pet(mon)) /* XXX no spamming _SummonHydras et. al. */
                        plr_gain_life(dam);
                }
                else if (!(flags & GF_AFFECT_SPELL))
                    plr_restore_life(dam);
            }
            return TRUE; /* using note causes messages out of order */
        }
        return FALSE;
    case GF_LICH_DRAIN:
        if (seen) obvious = TRUE;
        if (mon_is_nonliving(mon))
        {
            mon_lore_nonliving(mon);
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        else
        {
            if (!gf_affect_m(who, mon, GF_UNLIFE, dam, flags))
                dam = 0;
        }
        break;
    case GF_LICH_GENOCIDE:
        if (seen) obvious = TRUE;
        if (mon_is_nonliving(mon))
        {
            mon_lore_nonliving(mon);
            if (seen_msg && !quiet)
                msg_format("%^s is unaffected!", m_name);
        }
        else if (genocide_aux(mon, dam, who_is_plr(who), (race->alloc.lvl + 1) / 2, "Genocide One"))
        {
            if (seen_msg)
                msg_format("%^s is sent to the netherworld!", m_name);

            virtue_add(VIRTUE_VITALITY, -1);
            return TRUE; /* monster has been deleted! */
        }
        skipped = TRUE;
        break;
    case GF_LICH_WORD:
        if (seen) obvious = TRUE;
        if (mon_is_pet(mon))
            return FALSE;
        if (mon_is_undead(mon))
        {
            if (gf_affect_m(who, mon, GF_CONTROL_UNDEAD, dam, flags))
                return TRUE;
            else if (gf_affect_m(who, mon, GF_AWAY_UNDEAD, dam, flags))
                return TRUE;
        }
        if (mon_is_living(mon))
        {
            int power = (dam + 2)/3; /* dam is 150 at CL50, boosted by unlife */
            gf_affect_m(who, mon, GF_ELDRITCH_HOWL, power, flags);
            if (!mon_save_aux(mon, power))
                do_conf = 5 + _1d(11);
            if (!mon_save_stun(race->alloc.lvl, power))
                do_stun = mon_stun_amount(power);
            dam *= 2;
        }
        break;
    case GF_ROCKET:
        if (seen) obvious = TRUE;
        break;
    case GF_ELDRITCH_STUN:
        if (seen) obvious = TRUE;

        if (_mon_save_p(mon, A_CHR))
        {
            if (!quiet) note = " resists stunning.";
        }
        else
            do_stun = mon_stun_amount(dam);
        break;
    case GF_ROCK:
        if (seen) obvious = TRUE;
        if (mon_res_pct(mon, GF_SOUND) <= 0)
        {
            if (who_is_plr(who)  && mon_save_stun(race->alloc.lvl, dam))
            {
                if (!quiet) note = " resists stunning.";
            }
            else
                do_stun = mon_stun_amount(dam);
        }
        break;
    case GF_ELDRITCH_DISPEL:
        if (seen) obvious = TRUE;

        dispel_monster_status(mon);

        /* More Powerful vs. Evil */
        if (mon_is_evil(mon) && one_in_(5))
        {
            msg_print("You blast the forces of evil with great power!");
            dam = dam * 2;

            /* Attempt a Powerful Slow */
            if (mon_save_p(mon, A_CHR))
            {
                if (!quiet) note = " resists being slowed!";
            }
            else
            {
                mon_tim_add(mon, T_SLOW, 50);
            }
        }
        break;
    case GF_PHARAOHS_CURSE:
        if (seen) obvious = TRUE;

        dam = spell_power(MIN(mon->hp*8/100 + dam, 400));
        if (plr->lev >= 50)
            dam += 50;
        note = " is cursed by ancient pharaohs of long ago!";
        break;
    case GF_ELDRITCH_CONFUSE:
        if (seen) obvious = TRUE;

        /* XXX normally RESIST(CONF) prevents T_CONFUSED ... cf below when do_conf is applied */
        if (mon_res_pct(mon, GF_CONFUSION) > 0 && mon_save_p(mon, A_NONE))
        {   /* XXX Was: race->level + randint1(100) > plr->lev*2 + (plr->stat_ind[A_CHR] + 3) - 10
               Note the A_NONE on mon_save_p for an easy saving throw. */
            if (!quiet) note = " resists confusion.";
        }
        else
        {
            /* Recovery is randint1(r_info[mon->r_idx].level / 20 + 1) */
            do_conf = 3 + randint0(5);
        }
        break;
    case GF_MANA:
    case GF_SEEKER:
    case GF_SUPER_RAY:
        if (seen) obvious = TRUE;

        break;
    case GF_PSI_STORM:
        if (seen) obvious = TRUE;

        if (mon_has_empty_mind(mon))
        {
            dam = 0;
            if (!quiet) note = " is immune!";
            mon_lore_empty_mind(mon);
            break;

        }
        else if (mon_is_stupid(mon) || mon_has_weird_mind(mon))
        {
            dam /= 3;
            if (!quiet) note = " resists.";
            mon_lore_stupid(mon);
            mon_lore_weird_mind(mon);
            break;
        }
        if (one_in_(4))
        {
            if (plr->riding && (mon->id == plr->riding)) do_dist = 0;
            else do_dist = 7;
        }
        if (one_in_(2))
        {
            do_stun = mon_stun_amount(dam);
            if (mon_save_stun(race->alloc.lvl, dam))
            {
                do_stun = 0;
                obvious = FALSE;
            }
        }
        if (one_in_(4))
        {
            switch (randint1(3))
            {
                case 1:
                    do_conf = 3 + randint1(dam);
                    break;
                case 2:
                    do_fear = 3 + randint1(dam);
                    break;
                case 3:
                    do_sleep = 3 + randint1(dam);
                    break;
            }
        }
        note_dies = " collapses, a mindless husk.";
        break;
    case GF_PSI:
        if (seen) obvious = TRUE;

        /* PSI only works if the monster can see you! -- RG */
        if (!point_los(mon->pos, plr->pos))
        {
            if (seen_msg) msg_format("%^s can't see you, and isn't affected!", m_name);
            skipped = TRUE;
            break;
        }
        if (mon_has_empty_mind(mon))
        {
            dam = 0;
            if (!quiet) note = " is immune!";
            mon_lore_empty_mind(mon);
        }
        else if ( mon_is_stupid(mon)
               || mon_has_weird_mind(mon) 
               || mon_is_animal(mon)
               || mon_save_psi(race->alloc.lvl, dam) )
        {
            dam /= 3;
            if (!quiet) note = " resists.";

            /*
             * Powerful demons & undead can turn a mindcrafter's
             * attacks back on them
             */
            if ( (mon_is_undead(mon) || mon_is_demon(mon))
              && mon_lvl(mon) > plr->lev/2
              && one_in_(2) )
            {
                note = NULL;
                msg_format("%^s%s corrupted mind backlashes your attack!",
                    m_name, (seen ? "'s" : "s"));

                /* Saving throw */
                if ((randint0(100 + race->alloc.lvl / 2) < plr->skills.sav) && !CHECK_MULTISHADOW())
                {
                    msg_print("You resist the effects!");

                }
                else
                {
                    /* Injure +/- confusion */
                    monster_desc(killer, mon, MD_IGNORE_HALLU | MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE);
                    take_hit(DAMAGE_ATTACK, dam, killer);  /* has already been /3 */
                    if (one_in_(4) && !CHECK_MULTISHADOW())
                    {
                        switch (randint1(4))
                        {
                        case 1:
                            plr_tim_add(T_CONFUSED, 3 + randint1(dam));
                            break;
                        case 2:
                            if (!res_save(GF_STUN, 100)) plr_tim_add(T_STUN, randint1(dam));
                            break;
                        case 3:
                            fear_add_p(FEAR_SCARED);
                            break;
                        default:
                            if (!free_act_save_p(race->alloc.lvl))
                                plr_tim_add(T_PARALYZED, randint1(dam));
                        }
                    }
                }
                dam = 0;
            }
        }

        if ((dam > 0) && one_in_(4))
        {
            switch (randint1(4))
            {
                case 1:
                    do_conf = 3 + randint1(dam);
                    break;
                case 2:
                    do_stun = 3 + randint1(dam);
                    break;
                case 3:
                    do_fear = 3 + randint1(dam);
                    break;
                default:
                    do_sleep = 3 + randint1(dam);
                    break;
            }
        }

        note_dies = " collapses, a mindless husk.";
        break;
    case GF_PSI_DRAIN:
        if (seen) obvious = TRUE;

        if (mon_has_empty_mind(mon))
        {
            dam = 0;
            if (!quiet) note = " is immune!";
            mon_lore_empty_mind(mon);
        }
        else if ( mon_is_stupid(mon)
               || mon_has_weird_mind(mon) 
               || mon_is_animal(mon)
               || mon_save_psi(race->alloc.lvl, dam) )
        {
            dam /= 3;
            if (!quiet) note = " resists.";


            /*
             * Powerful demons & undead can turn a mindcrafter's
             * attacks back on them
             */
            if ( (mon_is_undead(mon) || mon_is_demon(mon))
              && mon_lvl(mon) > plr->lev/2
              && one_in_(2) )
            {
                note = NULL;
                msg_format("%^s%s corrupted mind backlashes your attack!",
                    m_name, (seen ? "'s" : "s"));

                /* Saving throw */
                if ((randint0(100 + race->alloc.lvl / 2) < plr->skills.sav) && !CHECK_MULTISHADOW())
                {
                    msg_print("You resist the effects!");
                }
                else
                {
                    /* Injure + mana drain */
                    monster_desc(killer, mon, MD_IGNORE_HALLU | MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE);
                    if (!CHECK_MULTISHADOW())
                    {
                        msg_print("Your psychic energy is drained!");

                        plr->csp -= damroll(5, dam) / 2;
                        if (plr->csp < 0) plr->csp = 0;
                        plr->redraw |= PR_MANA;
                        plr->window |= (PW_SPELL);
                    }
                    take_hit(DAMAGE_ATTACK, dam, killer);  /* has already been /3 */
                }
                dam = 0;
            }
        }
        else if (dam > 0)
        {
            int b = damroll(5, dam) / 4;
            cptr str = (plr->pclass == CLASS_MINDCRAFTER) ? "psychic energy" : "mana";
            msg_format("You convert %s%s pain into %s!",
                m_name, (seen ? "'s" : "s"), str);

            b = MIN(plr->msp, plr->csp + b);
            plr->csp = b;
            plr->redraw |= PR_MANA;
            plr->window |= (PW_SPELL);
        }

        note_dies = " collapses, a mindless husk.";
        break;
    case GF_TELEKINESIS:
        if (seen) obvious = TRUE;

        if (one_in_(4))
        {
            if (plr->riding && (mon->id == plr->riding)) do_dist = 0;
            else do_dist = 7;
        }

        do_stun = mon_stun_amount(dam);
        if (mon_save_stun(race->alloc.lvl, dam))
        {
            do_stun = 0;
            obvious = FALSE;
        }
        break;
    case GF_PSY_SPEAR:
        if (seen) obvious = TRUE;

        break;
    case GF_METEOR:
        if (seen) obvious = TRUE;

        break;
    case GF_DOMINATION: {
        int power = dam;

        /* No "real" damage */
        dam = 0;

        if (!mon_is_hostile(mon)) break;
        if (seen) obvious = TRUE;


        /* Attempt a saving throw */
        if (mon_save_aux(mon, power))
        {
            /*
             * Powerful demons & undead can turn a mindcrafter's
             * attacks back on them
             */
            if ( (mon_is_undead(mon) || mon_is_demon(mon))
              && mon_lvl(mon) > plr->lev/2
              && one_in_(2) )
            {
                note = NULL;
                msg_format("%^s%s corrupted mind backlashes your attack!",
                    m_name, (seen ? "'s" : "s"));

                /* Saving throw */
                if (randint0(100 + race->alloc.lvl/2) < plr->skills.sav)
                {
                    msg_print("You resist the effects!");

                }
                else
                {
                    /* Confuse, stun, terrify */
                    switch (randint1(4))
                    {
                        case 1:
                            if (!res_save(GF_STUN, 100)) plr_tim_add(T_STUN, power / 2);
                            break;
                        case 2:
                            plr_tim_add(T_CONFUSED, power / 2);
                            break;
                        default:
                            fear_add_p(power);
                    }
                }
            }
            else
            {
                if (!quiet) note = " is unaffected!";
                obvious = FALSE;
            }
        }
        else
        {
            bool unique = mon_race_is_unique(race);

            if (!unique && !(mon->mflag2 & MFLAG2_QUESTOR) && power > 29 && randint1(100) < power)
            {
                note = " is in your thrall!";
                set_pet(mon);
            }
            else
            {
                switch (randint1(4))
                {
                case 1:
                    do_stun = power / 2;
                    break;
                case 2:
                    if (mon_res_pct(mon, GF_CONFUSION) > 0)
                    {
                        mon_lore_resist(mon, GF_CONFUSION);
                        if (!quiet) note = " is unaffected.";
                        break;
                    }
                    else if (!unique)
                    {
                        do_conf = power / 2;
                        break;
                    }
                /* FALL THROUGH */
                default:
                    if (prace_is_(RACE_MON_VAMPIRE))
                    {
                        if (!unique && !mon_save_aux(mon, power))
                        {
                            note = " is frozen in terror!";
                            do_paralyzed = randint1(3);
                        }
                        break;
                    }
                    do_fear = power;
                }
            }
        }
        break; }
    case GF_ICE:
        if (touch && seen_msg && dam) msg_format("%^s is <color:W>frozen</color>!", m_name);
        if (seen) obvious = TRUE;
        if (res_pct <= 0)
        {
            if (!mon_save_stun(race->alloc.lvl, dam))
                do_stun = mon_stun_amount(dam);
        }
        break;
    case GF_OLD_DRAIN:
        if (touch && seen_msg) msg_format("%^s is <color:D>drained</color>!", m_name);
        if (seen) obvious = TRUE;

        if (mon_is_nonliving(mon))
        {
            mon_lore_nonliving(mon);
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
            dam = 0;
        }
        else
        {
            if (dragon_vamp_hack)
                dragon_vamp_amt += dam;
            do_time = (dam+7)/8;
        }
        break;
    case GF_DEATH_TOUCH:
        if (seen) obvious = TRUE;

        if (mon_is_nonliving(mon))
        {
            mon_lore_nonliving(mon);
            if (!quiet) note = " is immune.";
            obvious = FALSE;
            dam = 0;
        }
        else if ((mon_race_is_unique(race) && randint1(888) != 666)
                || mon_save_p(mon, A_INT)
                || mon_save_p(mon, A_INT) )
        {
            if (!quiet) note = " resists!";
            obvious = FALSE;
            dam = 0;
        }
        break;
    case GF_DEATH_RAY:
        if (seen) obvious = TRUE;

        if (mon_is_nonliving(mon))
        {
            mon_lore_nonliving(mon);
            if (!quiet) note = " is immune.";
            obvious = FALSE;
            dam = 0;
        }
        else if ( (mon_race_is_unique(race) && randint1(888) != 666)
               || (race->alloc.lvl + randint1(20) > randint1(caster_lev)))
        {
            if (!quiet) note = " resists!";
            obvious = FALSE;
            dam = 0;
        }

        break;
    case GF_OLD_POLY:
        if (seen) obvious = TRUE;

        do_poly = dam; /* save later ... */
        dam = 0;
        break;
    case GF_OLD_CLONE:
        if (seen) obvious = TRUE;


        if ( mon_is_pet(mon)
          || mon_race_is_unique(race)
          || mon_race_is_nazgul(race)
          || (mon->mflag2 & MFLAG2_QUESTOR) )
        {
            if (!quiet) note = " is unaffected!";
        }
        else
        {
            mon->hp = mon->maxhp;
            if (multiply_monster(mon, TRUE, 0L))
                note = " spawns!";
        }
        dam = 0;
        break;
    case GF_STAR_HEAL:
        if (seen) obvious = TRUE;

        mon_tim_delete(mon, MT_SLEEP);

        if (mon->maxhp < mon->max_maxhp)
        {
            if (seen_msg) msg_format("%^s recovers %s vitality.", m_name, m_poss);
            mon->maxhp = mon->max_maxhp;
        }
        if (mon->mpower < 1000)
        {
            if (seen_msg) msg_format("%^s recovers %s life.", m_name, m_poss);
            mon->mpower = 1000;
        }

        if (!dam)
        {
            /* Redraw (later) if needed */
            check_mon_health_redraw(mon);
            break;
        }

        /* Fall through */
    case GF_OLD_HEAL:
        if (seen) obvious = TRUE;


        mon_tim_delete(mon, MT_SLEEP);
        mon_tim_remove(mon, T_STUN);
        mon_tim_remove(mon, T_CONFUSED);
        mon_tim_remove(mon, T_FEAR);

        if (who_is_plr(who) && !mon_is_evil(mon))
            dam = dam * (625 + virtue_current(VIRTUE_COMPASSION))/625;

        /* Heal */
        if (mon->hp < 30000) mon->hp += dam;

        /* No overflow */
        if (mon->hp > mon->maxhp) mon->hp = mon->maxhp;

        if (who_is_plr(who))
        {
            virtue_add(VIRTUE_VITALITY, 1);

            if (mon_race_is_unique(race))
                virtue_add(VIRTUE_INDIVIDUALISM, 1);

            if (mon_is_friendly(mon))
                virtue_add(VIRTUE_HONOUR, 1);
            else if (!mon_is_evil(mon))
            {
                if (mon_is_good(mon))
                    virtue_add(VIRTUE_COMPASSION, 2);
                else
                    virtue_add(VIRTUE_COMPASSION, 1);
            }

            if (mon_is_animal(mon))
                virtue_add(VIRTUE_NATURE, 1);
        }

        if (mon_race_is_(mon->race, "t.leper"))
        {
            heal_leper = TRUE;
            if (who_is_plr(who)) virtue_add(VIRTUE_COMPASSION, 5);
        }

        /* Redraw (later) if needed */
        check_mon_health_redraw(mon);

        /* Message */
        note = " looks healthier.";

        /* No "real" damage */
        dam = 0;
        break;
    case GF_OLD_SPEED:
        if (seen) obvious = TRUE;

        mon_tim_add(mon, T_FAST, 100);
        if (who_is_plr(who))
        {
            if (mon_race_is_unique(race))
                virtue_add(VIRTUE_INDIVIDUALISM, 1);
            if (mon_is_friendly(mon))
                virtue_add(VIRTUE_HONOUR, 1);
        }

        /* No "real" damage */
        dam = 0;
        break;
    case GF_UNHOLY_WORD:
        if (mon_is_pet(mon) && mon_is_evil(mon))
        {
            if (seen) obvious = TRUE;

            mon_tim_delete(mon, MT_SLEEP);
            mon_tim_remove(mon, T_STUN);
            mon_tim_remove(mon, T_CONFUSED);
            mon_tim_remove(mon, T_FEAR);

            if (mon->hp < 30000) mon->hp += dam;
            if (mon->hp > mon->maxhp) mon->hp = mon->maxhp;
            mon_tim_add(mon, T_FAST, 100);
            note = " fights with renewed vigor!";
        }
        dam = 0;
        break;
    case GF_STASIS_EVIL:
        if (seen) obvious = TRUE;

        if (!mon_is_evil(mon) || _mon_save_aux(mon, dam))
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
        }
        else
        {
            note = " is suspended!";
            do_paralyzed = 5;
        }
        dam = 0;
        break;
    case GF_PARALYSIS:
        if (seen) obvious = TRUE;

        if (_mon_save_aux(mon, dam))
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
        }
        else if (_1d(100) <= mon_res_pct(mon, GF_SLEEP))
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
        }
        else
        {
            do_paralyzed = randint1(3);
        }
        dam = 0;
        break;
    case GF_STASIS:
        if (seen) obvious = TRUE;

        if (_mon_save_aux(mon, dam))
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
        }
        else
        {
            do_paralyzed = 3;
        }
        dam = 0;
        break;
    case GF_SUBJUGATION: {
        bool unique = mon_race_is_unique(race);
        int  attempts = _1d(1 + plr->lev/50);
        int  ct = 0;

        if (seen) obvious = TRUE;

        mon_tim_delete(mon, MT_SLEEP);
        if (mon_is_pet(mon))
            return FALSE;
        while (attempts--)
        {
            switch (_1d(5))
            {
            case 1:
                if (unique || (mon->mflag2 & MFLAG2_QUESTOR))
                    attempts++;
                else if (!mon_save_aux(mon, dam))
                {
                    note = " is frozen in terror!";
                    do_paralyzed = _1d(3);
                    attempts = 0;
                    ct++;
                }
                break;
            case 2:
                if (unique || (mon->mflag2 & MFLAG2_QUESTOR))
                    attempts++;
                else if ((mon->mflag2 & MFLAG2_NOPET) || mon_save_aux(mon, dam))
                {
                    if (one_in_(4))
                        mon->mflag2 |= MFLAG2_NOPET;
                }
                else
                {
                    msg_format("%^s bows to your will!", m_name);

                    set_pet(mon);
                    attempts = 0;
                    ct++;

                    virtue_add(VIRTUE_INDIVIDUALISM, -1);
                    if (mon_is_animal(mon))
                        virtue_add(VIRTUE_NATURE, 1);

                    /* Ignore any prior effects */
                    return TRUE;
                }
                break;
            case 3:
                if (!mon_save_aux(mon, dam))
                {
                    do_conf = dam / 2;
                    ct++;
                }
                break;
            case 4:
                if (!mon_save_aux(mon, dam))
                {
                    do_stun = dam / 2;
                    ct++;
                }
                break;
            case 5:
                if (!mon_save_aux(mon, dam))
                {
                    do_fear = dam;
                    ct++;
                }
                break;
            }
        }
        if (!ct && !quiet)
            note = " resists!";
        dam = 0;
        break; }
    case GF_ELDRITCH_HOWL:
        if (seen) obvious = TRUE;
        dam = mon_res_calc_dam(mon, GF_FEAR, dam);
        if (dam)
        {
            do_fear = damroll(3, (dam / 2)) + 1;
            if ( mon_save_aux(mon, dam)
                   || (mon_race_is_unique(race) && mon_save_aux(mon, dam)) )
            {
                if (!quiet) note = " resists!";
                obvious = FALSE;
                do_fear = 0;
            }
            else if (!mon_save_aux(mon, dam))
            {
                note = " is frozen with terror!";
                do_paralyzed = randint1(3);
            }
            dam = 0;
        }
        else
        {
            if (!quiet) note = " is immune!";
        }
        break;
    case GF_CHARM:
    case GF_CHARM_RING_BEARER:
        if (seen) obvious = TRUE;

        if (gf == GF_CHARM_RING_BEARER)
        {
            if (!mon_is_type(mon->race, SUMMON_RING_BEARER))
            {
                note = " is not a suitable ring bearer.";
                dam = 0;
                break;
            }
        }
        else
        {
            dam += (adj_con_fix[plr->stat_ind[A_CHR]] - 1);
            dam += virtue_current(VIRTUE_HARMONY)/10;
            dam -= virtue_current(VIRTUE_INDIVIDUALISM)/20;
            if (mon_race_is_unique(race) || mon_race_is_nazgul(race))
                dam = dam * 2 / 3;
        }

        if (mon->mflag2 & MFLAG2_QUESTOR)
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
        }
        else if ((mon->mflag2 & MFLAG2_NOPET) || race->alloc.lvl > randint1(dam))
        {
            if (!quiet) note = " resists!";
            obvious = FALSE;
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else if (plr->cursed & OFC_AGGRAVATE)
        {
            note = " hates you too much!";
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else if (!mon_is_pet(mon))
        {
            note = " suddenly seems friendly!";

            set_pet(mon);

            virtue_add(VIRTUE_INDIVIDUALISM, -1);
            if (mon_is_animal(mon))
                virtue_add(VIRTUE_NATURE, 1);
        }
        dam = 0;
        break;
    case GF_CONTROL_UNDEAD:
        if (seen) obvious = TRUE;

        dam += virtue_current(VIRTUE_UNLIFE)/10;
        dam -= virtue_current(VIRTUE_INDIVIDUALISM)/20;

        if (mon_race_is_unique(race) || mon_race_is_nazgul(race))
            dam = dam * 2 / 3;

        /* Attempt a saving throw */
        if (mon_is_pet(mon))
        {
            note = " is already in your thrall!";
        }
        else if ( (mon->mflag2 & MFLAG2_QUESTOR)
               || !mon_is_undead(mon)
               || (mon->mflag2 & MFLAG2_NOPET)
               || race->alloc.lvl > 10 + _1d(MAX(1, dam - 10)) )
        {
            /* No obvious effect */
            if (!quiet) note = " is unaffected!";

            obvious = FALSE;
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else if (plr->cursed & OFC_AGGRAVATE)
        {
            note = " hates you too much!";

            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else
        {
            note = " is in your thrall!";
            set_pet(mon);
        }
        dam = 0;
        break;
    case GF_CONTROL_DEMON:
        if (seen) obvious = TRUE;

        dam += virtue_current(VIRTUE_UNLIFE)/10;
        dam -= virtue_current(VIRTUE_INDIVIDUALISM)/20;

        if (mon_race_is_unique(race) || mon_race_is_nazgul(race))
            dam = dam * 2 / 3;

        /* Attempt a saving throw */
        if ((mon->mflag2 & MFLAG2_QUESTOR) ||
            !mon_is_demon(mon) ||
            (mon->mflag2 & MFLAG2_NOPET) ||
            (race->alloc.lvl > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
        {
            /* No obvious effect */
            if (!quiet) note = " is unaffected!";

            obvious = FALSE;
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else if (plr->cursed & OFC_AGGRAVATE)
        {
            note = " hates you too much!";

            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else
        {
            note = " is in your thrall!";

            set_pet(mon);
        }

        /* No "real" damage */
        dam = 0;
        break;
    case GF_CONTROL_ANIMAL:
        if (seen) obvious = TRUE;

        dam += virtue_current(VIRTUE_NATURE)/10;
        dam -= virtue_current(VIRTUE_INDIVIDUALISM)/20;

        if (mon_race_is_unique(race) || mon_race_is_nazgul(race))
            dam = dam * 2 / 3;

        /* Attempt a saving throw */
        if ((mon->mflag2 & MFLAG2_QUESTOR) ||
            !mon_is_animal(mon) ||
            (mon->mflag2 & MFLAG2_NOPET) ||
            (race->alloc.lvl > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
        {
            /* Resist */
            /* No obvious effect */
            if (!quiet) note = " is unaffected!";

            obvious = FALSE;
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else if (plr->cursed & OFC_AGGRAVATE)
        {
            note = " hates you too much!";

            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else
        {
            note = " is tamed!";

            set_pet(mon);

            if (mon_is_animal(mon))
                virtue_add(VIRTUE_NATURE, 1);
        }

        /* No "real" damage */
        dam = 0;
        break;
    case GF_CONTROL_PACT_MONSTER:
        if (warlock_is_pact_monster(race) && !mon_is_pet(mon))
        {
            if (seen) obvious = TRUE;

            /* Attempt a saving throw */
            if ( (mon->mflag2 & MFLAG2_QUESTOR)
              || (mon->mflag2 & MFLAG2_NOPET)
              || mon_save_p(mon, A_CHR)
              || (mon_race_is_unique(race) && mon_save_p(mon, A_CHR)) )
            {
                if (warlock_is_(WARLOCK_HOUNDS))
                    note = " growls at you in defiance!";
                else
                    note = " resists your control.";
                obvious = FALSE;
                if (one_in_(4))
                    mon->mflag2 |= MFLAG2_NOPET;
            }
            else if (plr->cursed & OFC_AGGRAVATE)
            {
                note = " finds you very aggravating!";
                if (one_in_(4))
                    mon->mflag2 |= MFLAG2_NOPET;
            }
            else
            {
                if (warlock_is_(WARLOCK_HOUNDS))
                    note = " rolls on its back in submission.";
                else
                    note = " obeys your will.";
                set_pet(mon);
            }
        }

        /* No "real" damage */
        dam = 0;
        break;
    case GF_CONTROL_LIVING:
        if (seen) obvious = TRUE;

        dam += (adj_chr_chm[plr->stat_ind[A_CHR]]);
        dam -= virtue_current(VIRTUE_UNLIFE)/10;
        dam -= virtue_current(VIRTUE_INDIVIDUALISM)/20;

        if (dam < 1) dam = 1;
        msg_format("You stare into %s.", m_name_object);

        if (mon_race_is_unique(race) || mon_race_is_nazgul(race))
            dam = dam * 2 / 3;

        /* Attempt a saving throw */
        if ((mon->mflag2 & MFLAG2_QUESTOR) ||
            (mon->mflag2 & MFLAG2_NOPET) ||
             !mon_is_living(mon) ||
             ((race->alloc.lvl+10) > randint1(dam)))
        {
            /* Resist */
            /* No obvious effect */
            if (!quiet) note = " is unaffected!";

            obvious = FALSE;
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else if (plr->cursed & OFC_AGGRAVATE)
        {
            note = " hates you too much!";

            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else
        {
            note = " is tamed!";

            set_pet(mon);

            if (mon_is_animal(mon))
                virtue_add(VIRTUE_NATURE, 1);
        }
        dam = 0;
        break;
    case GF_FRIENDSHIP:
        if (mon_is_pet(mon) || mon_is_friendly(mon)) return FALSE;
        if (mon->mflag2 & MFLAG2_QUESTOR) return FALSE;
        if (mon_race_is_unique(race) && randint0(dam) < race->alloc.lvl) return FALSE;
        if (_1d(150) <= -mon->race->align) return FALSE; /* evil monsters resist */
        if (_mon_save_aux(mon, dam)) return FALSE;
        mon_tim_delete(mon, MT_SLEEP);
        if (mon_show_msg(mon))
        {
            char name[MAX_NLEN_MON];
            monster_desc(name, mon, 0);
            msg_format("%^s is soothed by your calming chant.", name);
        }
        set_temp_friendly(mon);
        return TRUE; /* bypass mon_take_hit ... */
    case GF_OBEDIENCE:
        if (mon_is_pet(mon) || mon_is_temp_friendly(mon)) return FALSE;
        if (mon->mflag2 & MFLAG2_QUESTOR) return FALSE;
        if (mon_race_is_unique(race) && randint0(dam) < race->alloc.lvl) return FALSE;
        if (_1d(150) <= -mon->race->align) return FALSE; /* evil monsters resist */
        if (_mon_save_aux(mon, dam)) return FALSE;
        mon_tim_delete(mon, MT_SLEEP);
        if (mon_show_msg(mon))
        {
            char name[MAX_NLEN_MON];
            monster_desc(name, mon, 0);
            msg_format("%^s is soothed by your calming chant.", name);
        }
        set_temp_pet(mon);
        return TRUE; /* bypass mon_take_hit ... */
    case GF_EXORCISM:
        if (seen) obvious = TRUE;
        if (!mon_is_undead(mon) && !mon_is_demon(mon))
        {
            mon_lore_undead(mon);
            mon_lore_demon(mon);
            if (seen_msg && !quiet)
                msg_format("%^s is unaffected!", m_name);
        }
        else if (genocide_aux(mon, dam, who_is_plr(who), (race->alloc.lvl + 1) / 2, "Exorcism"))
        {
            if (seen_msg)
                msg_format("%^s is cast from the world of the living!", m_name);

            virtue_add(VIRTUE_FAITH, 1);
            return TRUE; /* monster has been deleted! */
        }
        skipped = TRUE;
        break;
    case GF_REPENTANCE:
        if (mon_is_pet(mon) || mon_is_temp_friendly(mon)) return FALSE;
        if (!mon->parent_id) return FALSE;
        { /* check for missing parent (cf _preprocess in mon_ai.c ... pure paranoia)
             only sway minions not actively serving the cause of good ... 
             neutrality is not an option! */
            mon_ptr parent = mon_parent(mon);
            if (!parent || parent->race->align >= ALIGN_NEUTRAL_GOOD) return FALSE;
        }
        if (mon->mflag2 & MFLAG2_QUESTOR) return FALSE;
        if (mon_race_is_unique(race) && randint0(dam) < race->alloc.lvl) return FALSE;
        if (_mon_save_aux(mon, dam)) return FALSE;
        mon_tim_delete(mon, MT_SLEEP);
        if (one_in_(7))
        {
            if (mon_show_msg(mon))
            {
                char name[MAX_NLEN_MON];
                monster_desc(name, mon, 0);
                msg_format("%^s switches allegiances.", name);
            }
            mon->parent_id = 0; /* XXX set_pet should probably do this ... plr is now master! */
            set_pet(mon);
        }
        else
        {
            if (mon_show_msg(mon))
            {
                char name[MAX_NLEN_MON];
                monster_desc(name, mon, 0);
                msg_format("%^s repents and withdraws from the battle.", name);
            }
            delete_monster(mon);
        }
        return TRUE; /* bypass mon_take_hit ... */
    case GF_SLEEP:
        if (seen) obvious = TRUE;
        dam = mon_res_calc_dam(mon, GF_SLEEP, dam);
        if (dam)
        {
            if (_mon_save_aux(mon, dam))
            {
                if (!quiet) note = " resists!";
                obvious = FALSE;
            }
            else
            {
                do_sleep = 500;
            }
            dam = 0;
        }
        else
        {
            if (!quiet) note = " is immune!";
        }
        break;
    case GF_SLOW:
        if (seen) obvious = TRUE;
        dam = mon_res_calc_dam(mon, GF_SLOW, dam);
        if (dam)
        {
            if (_mon_save_aux(mon, dam))
            {
                if (!quiet) note = " is unaffected!";
                obvious = FALSE;
            }
            else mon_tim_add(mon, T_SLOW, 10 + _1d(15));
            dam = 0;
        }
        else
        {
            if (!quiet) note = " is immune!";
        }
        break;
    case GF_BLIND: /* `dam` is the power of the effect */
        if (seen) obvious = TRUE;
        dam = mon_res_calc_dam(mon, GF_BLIND, dam);
        if (dam)
        {
            do_blind = 10 + _1d(15);
            if (_mon_save_aux(mon, dam))
            {
                do_blind = 0;
                if (!quiet) note = " resists!";
                obvious = FALSE;
            }
            dam = 0;
        }
        else
        {
            if (!quiet) note = " is immune!";
        }
        break;
    case GF_OLD_CONF: /* `dam` is the power of the effect */
        if (seen) obvious = TRUE;
        dam = mon_res_calc_dam(mon, GF_CONFUSION, dam);
        if (dam)
        {
            do_conf = 10 + _1d(15);
            if (_mon_save_aux(mon, dam))
            {
                do_conf = 0;
                if (!quiet) note = " resists!";
                obvious = FALSE;
            }
            dam = 0;
        }
        else
        {
            if (!quiet) note = " is immune!";
        }
        break;
    case GF_STUN:
        if (seen) obvious = TRUE;
        dam = mon_res_calc_dam(mon, GF_STUN, dam);
        if (dam)
        {
            do_stun = dam;
            dam = 0;
        }
        else
        {
            if (!quiet) note = " is immune!";
        }
        break;
    case GF_FEAR:
        if (seen) obvious = TRUE;
        dam = mon_res_calc_dam(mon, GF_FEAR, dam);
        if (dam)
        {
            do_fear = damroll(3, (dam / 2)) + 1;
            if (fear_save_m(mon))
            {
                if (!quiet) note = " resists!";
                obvious = FALSE;
                do_fear = 0;
            }
            dam = 0;
        }
        else
        {
            if (!quiet) note = " is immune!";
        }
        break;
    case GF_TELEPORT:
        if (seen) obvious = TRUE;
        dam = mon_res_calc_dam(mon, GF_TELEPORT, dam);
        if (dam)
        {
            do_dist = dam;
            dam = 0;
        }
        else
        {
            if (!quiet) note = " is immune!";
        }
        break;
    case GF_LIGHT_WEAK:
        if (!dam)
        {
            skipped = TRUE;
            break;
        }
        if (mon_vuln(mon, GF_LIGHT)) /* LITE_WEAK requires VULN(LITE) for damage (unscaled) */
        {
            if (seen) obvious = TRUE;
            mon_lore_resist(mon, GF_LIGHT);

            note = " cringes from the light!";
            note_dies = " shrivels away in the light!";
        }
        else dam = 0;
        break;
    case GF_KILL_WALL: /* e.g. Stone to Mud */
        if (mon_vuln(mon, GF_DISINTEGRATE)) /* formerly HURT_ROCK */
        {
            if (seen) obvious = TRUE;
            mon_lore_resist(mon, GF_DISINTEGRATE);
            note = " loses some skin!";
            note_dies = " dissolves!";
        }
        /* Usually, ignore the effects */
        else dam = 0;
        break;
    case GF_AWAY_UNDEAD:
        if (mon_is_undead(mon))
        {
            if (seen) obvious = TRUE;
            mon_lore_undead(mon);
            do_dist = dam;
        }
        else skipped = TRUE;
        dam = 0;
        break;
    case GF_AWAY_EVIL:
        if (mon_is_evil(mon))
        {
            if (seen) obvious = TRUE;
            mon_lore_evil(mon);
            do_dist = dam;
        }
        else skipped = TRUE;
        dam = 0;
        break;
    case GF_ISOLATION:
        if (mon == who_mon(plr->duelist_target))
        {
            dam = 0;
            return TRUE;
        }
        if (seen) obvious = TRUE;
        do_dist = dam;
        dam = 0;
        break;
    case GF_TURN_UNDEAD:
        if (mon_is_undead(mon))
        {
            if (seen) obvious = TRUE;
            mon_lore_undead(mon);

            do_fear = damroll(3, (dam / 2)) + 1;
            if (fear_save_m(mon))
            {
                if (!quiet) note = " is unaffected!";
                obvious = FALSE;
                do_fear = 0;
            }
        }
        else skipped = TRUE;
        dam = 0;
        break;
    case GF_TURN_EVIL:
        if (mon_is_evil(mon))
        {
            if (seen) obvious = TRUE;
            mon_lore_align(mon);

            do_fear = damroll(3, (dam / 2)) + 1;

            if (fear_save_m(mon))
            {
                if (!quiet) note = " is unaffected!";
                obvious = FALSE;
                do_fear = 0;
            }
        }
        else skipped = TRUE;
        dam = 0;
        break;
    case GF_DISP_UNDEAD:
        if (mon_is_undead(mon))
        {
            if (seen) obvious = TRUE;
            mon_lore_undead(mon);
            note = " shudders.";
            note_dies = " dissolves!";
        }
        else
        {
            skipped = TRUE;
            dam = 0;
        }
        break;
    case GF_DISP_EVIL:
        if (mon_is_evil(mon))
        {
            if (seen) obvious = TRUE;
            mon_lore_align(mon);
            note = " shudders.";
            note_dies = " dissolves!";
        }
        else
        {
            skipped = TRUE;
            dam = 0;
        }
        break;
    case GF_DISP_GOOD:
        if (mon_is_good(mon))
        {
            if (seen) obvious = TRUE;
            mon_lore_align(mon);
            note = " shudders.";
            note_dies = " dissolves!";
        }
        else
        {
            skipped = TRUE;
            dam = 0;
        }
        break;
    case GF_DISP_LIVING:
        if (mon_is_living(mon))
        {
            if (seen) obvious = TRUE;
            mon_lore_living(mon);
            note = " shudders.";
            note_dies = " dissolves!";
        }
        else
        {
            skipped = TRUE;
            dam = 0;
        }
        break;
    case GF_DISP_DEMON:
        if (mon_is_demon(mon))
        {
            if (seen) obvious = TRUE;
            mon_lore_demon(mon);
            note = " shudders.";
            note_dies = " dissolves!";
        }
        else
        {
            skipped = TRUE;
            dam = 0;
        }
        break;
    case GF_DISP_ALL:
        if (seen) obvious = TRUE;
        note = " shudders.";
        note_dies = " dissolves!";
        break;
    case GF_DRAINING_TOUCH:
        if (seen) obvious = TRUE;

        if (mon_is_magical(mon))
        {
            if (who_is_plr(who) && sp_player(dam))
            {
                if (seen_msg) msg_format("You draw psychic energy from %s.", m_name_object);
            }
        }
        else
        {
            if (!quiet) msg_format("%^s is unaffected.", m_name);
            dam = 0;
        }
        break;
    case GF_DRAIN_MANA:
        if (seen) obvious = TRUE;

        if (mon_is_magical(mon))
        {
            if (caster_ptr)
            {
                if (caster_ptr->hp < caster_ptr->maxhp)
                {
                    caster_ptr->hp += 6 * dam;
                    if (caster_ptr->hp > caster_ptr->maxhp) caster_ptr->hp = caster_ptr->maxhp;
                    check_mon_health_redraw(caster_ptr);
                    monster_desc(killer, caster_ptr, 0);
                    if (seen_msg) msg_format("%^s appears healthier.", killer);
                }
            }
            else if (who_is_plr(who) && plr->chp < plr->mhp)
            {
                msg_format("You draw psychic energy from %s.", m_name_object);
                hp_player(dam);
            }
        }
        else if (seen_msg && !quiet) msg_format("%^s is unaffected.", m_name);
        dam = 0;
        break;
    case GF_ANTIMAGIC: { /* Formerly restricted to rage-mage. But now, mon vs mon */
        bool save = FALSE;/* and the possessor can use the ANTIMAGIC monster spell */
        if (seen) obvious = TRUE;
                                  /*v---Possessor uses monster level (dam) for the save */
        if (who_is_plr(who) && !mon_spell_current())
        {
            int stat = A_CHR;
            int rolls = 1, made = 0;
            int i;
            if (plr->pclass == CLASS_RAGE_MAGE)
            {
                stat = A_STR;
                if (plr_tim_find(T_BERSERK)) rolls++;
            }
            for (i = 0; i < rolls; i++)
            {
                if (mon_save_p(mon, stat))
                    made++;
            }
            save = (made == rolls);
        }
        else
        {
            save = mon_save_aux(mon, dam);
        }
        if (save)
        {
            if (seen_msg && !quiet) msg_format("%^s resists!", m_name);
            dam = 0;
            mon->anti_magic_ct = 0;
            return TRUE;
        }
        else
        {
            int dur = 2 + randint1(2);
            mon->anti_magic_ct = dur;
            if (seen_msg) msg_format("%^s can no longer cast spells!", m_name);
            dam = 0;
            return TRUE;
        }
        break; }
    case GF_PSI_BRAIN_SMASH: /* dam is the power of the effect (1-5) */
        if (seen) obvious = TRUE;

        if (mon_has_empty_mind(mon))
        {
            mon_lore_empty_mind(mon);
            if (!quiet) note = " is immune!";
            dam = 0;
        }
        else if (psion_mon_save_p(mon->race->id, dam))
        {
            if (!quiet) note = " resists!";
            dam = 0;
        }
        else if (mon_has_weird_mind(mon))
        {
            mon_lore_weird_mind(mon);
            if (!quiet) note = " resists somewhat.";
            dam /= 2;
            if (dam == 0) dam = 1;
        }
        if (dam)
        {
            note = " is blasted by psionic energy.";
            do_conf = 2*dam;
            do_stun = 2*dam;
            if (dam) mon_tim_add(mon, T_SLOW, 2*dam);
            dam = 0;
        }
        break;
    case GF_MIND_BLAST:
        if (seen) obvious = TRUE;
        if (who_is_plr(who)) msg_format("You gaze intently at %s.", m_name_object);

        if ( mon_save_smash(race->alloc.lvl, dam)
          || (mon_race_is_unique(race) && mon_save_smash(race->alloc.lvl+15, dam)) )
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        else if (mon_has_empty_mind(mon))
        {
            mon_lore_empty_mind(mon);
            if (!quiet) note = " is immune!";
            dam = 0;
        }
        else if (mon_has_weird_mind(mon))
        {
            mon_lore_weird_mind(mon);
            if (!quiet) note = " resists.";
            dam /= 3;
        }
        else
        {
            note = " is blasted by psionic energy.";
            note_dies = " collapses, a mindless husk.";

            if (caster_ptr) do_conf = randint0(4) + 4;
            else do_conf = randint0(8) + 8;
        }
        break;
    case GF_BRAIN_SMASH:
        if (seen) obvious = TRUE;
        if (who_is_plr(who)) msg_format("You gaze intently at %s.", m_name_object);

        if ( mon_save_smash(race->alloc.lvl, dam)
          || (mon_race_is_unique(race) && mon_save_smash(race->alloc.lvl+15, dam)) )
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        else if (mon_has_empty_mind(mon))
        {
            mon_lore_empty_mind(mon);
            if (!quiet) note = " is immune!";
            dam = 0;
        }
        else if (mon_has_weird_mind(mon))
        {
            mon_lore_weird_mind(mon);
            if (!quiet) note = " resists.";
            dam /= 3;
        }
        else
        {
            note = " is blasted by psionic energy.";
            note_dies = " collapses, a mindless husk.";

            if (caster_ptr)
            {
                do_conf = randint0(4) + 4;
                do_stun = randint0(4) + 4;
            }
            else
            {
                do_conf = randint0(8) + 8;
                do_stun = randint0(8) + 8;
            }
            if (!mon_race_is_unique(race) || !mon_save_smash(race->alloc.lvl+15, dam))
                mon_tim_add(mon, T_SLOW, 10);
        }
        break;
    case GF_CAUSE_1:
        if (seen) obvious = TRUE;
        if (who_is_plr(who)) msg_format("You %s at %s and curse.", prace_is_(RACE_MON_BEHOLDER) ? "gaze" : "point", m_name);

        if (randint0(100 + (caster_lev / 2)) < (race->alloc.lvl + 35))
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        break;
    case GF_CAUSE_2:
        if (seen) obvious = TRUE;
        if (who_is_plr(who)) msg_format("You %s at %s and curse horribly.", prace_is_(RACE_MON_BEHOLDER) ? "gaze" : "point", m_name);

        if (randint0(100 + (caster_lev / 2)) < (race->alloc.lvl + 35))
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        break;
    case GF_CAUSE_3:
        if (seen) obvious = TRUE;
        if (who_is_plr(who)) msg_format("You point at %s, incanting terribly!", m_name);

        if (randint0(100 + (caster_lev / 2)) < (race->alloc.lvl + 35))
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        break;
    case GF_CAUSE_4: {
        bool save = FALSE;
        if (seen) obvious = TRUE;
        if (who_is_plr(who)) msg_format("You %s at %s and scream the word, 'DIE!'.", prace_is_(RACE_MON_BEHOLDER) ? "gaze" : "point", m_name);

        if (who_is_plr(who))
        {
            save = !plr_mon_race_is_("p.Kenshirou") && mon_save_p(mon, A_WIS);
        }
        else
        {
            save = randint0(100 + caster_lev / 2) < race->alloc.lvl + 35
                && (!caster_ptr || !mon_race_is_(caster_ptr->race, "p.Kenshirou"));
        }
        if (save)
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        break; }
    case GF_HAND_DOOM:
        if (seen) obvious = TRUE;

        if (mon_race_is_unique(race))
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        else
        {
            if (randint1(dam) >= race->alloc.lvl + randint1(20))
            {
                dam = ((40 + randint1(20)) * mon->hp) / 100;
                if (mon->hp < dam) dam = mon->hp - 1;
            }
            else
            {
                if (!quiet) note = " resists!";
                dam = 0;
            }
        }
        break;
    case GF_CAPTURE: {
        int nokori_hp;
        if (!quests_allow_all_spells() && !mon_is_pet(mon))
        {
            msg_format("%^s is unaffected.", m_name);
            skipped = TRUE;
            break;
        }
        if ( mon_race_is_unique(race)
          || mon_race_is_nazgul(race)
          || (mon->mflag2 & MFLAG2_QUESTOR)
          || (mon->mflag2 & MFLAG2_ILLUSION)
          || mon->parent_id )
        {
            msg_format("%^s is unaffected.", m_name);
            skipped = TRUE;
            break;
        }
        if (mon_is_pet(mon)) nokori_hp = mon->maxhp * 4;
        else if ((plr->pclass == CLASS_BEASTMASTER) && mon_is_living(mon))
            nokori_hp = mon->maxhp * 3 / 10;
        else if (plr->easy_capture)
            nokori_hp = mon->maxhp * 3 / 10;
        else if (warlock_is_(WARLOCK_DRAGONS) && mon_is_dragon(mon))
            nokori_hp = mon->maxhp * 3 / 15;
        else
            nokori_hp = mon->maxhp * 3 / 20;

        if (mon->hp >= nokori_hp)
        {
            msg_format("You need to weaken %s more.", m_name);
            skipped = TRUE;
        }
        else if (mon->hp < randint0(nokori_hp))
        {
            if (mon->mflag2 & MFLAG2_CHAMELEON)
                choose_new_monster(mon, FALSE, mon_race_parse("R.chameleon"));
            msg_format("You capture %^s!", m_name);
            quests_on_kill_mon(mon);
            mon_tim_clear(mon); /* undo T_FAST et. al. before "capturing" mspeed */
            cap_mon = mon->race->id;
            cap_mspeed = mon->mspeed;
            cap_hp = mon->hp;
            cap_maxhp = mon->max_maxhp;
            cap_nickname = mon->nickname;
            if (mon->id == plr->riding)
            {
                if (rakuba(-1, FALSE))
                {
                    msg_format("You have fallen from %s.", m_name);
                }
            }
            delete_monster(mon);
            return TRUE;
        }
        else
        {
            msg_format("You failed to capture %s.", m_name);
            skipped = TRUE;
        }
        break; }
    case GF_ATTACK:
        if (caster_ptr)
            return mon_attack(caster_ptr, mon->pos);
        else if (who_is_plr(who))
        {
            plr_attack_t context = {0};
            context.mode = dam;
            return plr_attack(&context, mon->pos);
        }
        return FALSE;
    case GF_ENGETSU: {
        int effect = 0;
        bool done = TRUE;

        if (seen) obvious = TRUE;

        if (mon_has_empty_mind(mon))
        {
            mon_lore_empty_mind(mon);
            note = " is immune!";
            dam = 0;
            skipped = TRUE;
            break;
        }
        if (mon_tim_find(mon, MT_SLEEP))
        {
            note = " is immune!";
            dam = 0;
            skipped = TRUE;
            break;
        }

        if (one_in_(5)) effect = 1;
        else if (one_in_(4)) effect = 2;
        else if (one_in_(3)) effect = 3;
        else done = FALSE;

        if (effect == 1)
        {
            /* Powerful monsters can resist */
            if (mon_race_is_unique(race) ||
                (race->alloc.lvl > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
            {
                note = " is unaffected!";

                obvious = FALSE;
            }

            /* Normal monsters slow down */
            else mon_tim_add(mon, T_SLOW, 50);
        }

        else if (effect == 2)
        {
            do_stun = damroll((plr->lev / 10) + 3 , (dam)) + 1;

            /* Attempt a saving throw */
            if (mon_race_is_unique(race) ||
                (race->alloc.lvl > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
            {
                /* Resist */
                do_stun = 0;

                /* No obvious effect */
                note = " is unaffected!";

                obvious = FALSE;
            }
        }

        else if (effect == 3)
        {
            if (mon_race_is_unique(race))
            {
                note = " is immune!";
                obvious = FALSE;
            }
            else if (_1d(100) < mon_res_pct(mon, GF_SLEEP))
            {
                mon_lore_resist(mon, GF_SLEEP);
                note = " resists!";
                obvious = FALSE;
            }
            else if (race->alloc.lvl > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10)
            {
                note = " resists!";
                obvious = FALSE;
            }
            else
            {
                note = " is paralyzed!";
                do_paralyzed = 5;
            }
        }

        if (!done)
        {
            note = " is immune!";
        }
        dam = 0;
        break; }
    case GF_ENTOMB: {
        int dir;

        if (mon_is_pet(mon) || mon_is_friendly(mon))
        {
            msg_print("Failed!");
            return FALSE;
        }

        for (dir = 0; dir < 8; dir++)
        {
            point_t p = point_step(mon->pos, ddd[dir]);

            if (!dun_naked_at(cave, p)) continue;
            if (plr->lev < 45)
                dun_place_rubble(cave, p);
            else
                dun_place_granite(cave, p);
        }
        plr->update |= PU_FLOW; /* XXX no PU_MON_FLOW ... let hunters continue to seek */
        plr->redraw |= PR_MAP;
        return TRUE; }
    case GF_GENOCIDE:
        if (seen) obvious = TRUE;

        if (genocide_aux(mon, dam, who_is_plr(who), (race->alloc.lvl + 1) / 2, "Genocide One"))
        {
            if (seen_msg)
                msg_format("%^s disappeared!", m_name);

            virtue_add(VIRTUE_VITALITY, -1);
            return TRUE;
        }
        skipped = TRUE;
        break;
    case GF_BLOOD_CURSE:
        if (seen) obvious = TRUE;

        break;
    case GF_CRUSADE: {
        bool success = FALSE;
        if (seen) obvious = TRUE;

        if (mon_is_good(mon))
        {
            if (dam < 1) dam = 1;

            /* No need to tame your pet */
            if (mon_is_pet(mon))
            {
                mon_tim_add(mon, T_FAST, 100);
                success = TRUE;
            }

            /* Attempt a saving throw */
            else if ((mon->mflag2 & MFLAG2_QUESTOR) ||
                mon_race_is_unique(race) ||
                (mon->mflag2 & MFLAG2_NOPET) ||
                (plr->cursed & OFC_AGGRAVATE) ||
                 ((race->alloc.lvl+10) > randint1(dam)))
            {
                /* Resist */
                if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
            }
            else
            {
                note = " is tamed!";
                set_pet(mon);
                mon_tim_add(mon, T_FAST, 100);
                mon_lore_align(mon);
                success = TRUE;
            }
        }

        if (!success)
        {
            do_fear = randint1(90)+10;
        }
        dam = 0;
        break; }
    default:
        skipped = TRUE;
        dam = 0;
    }

    /* Absolutely no effect */
    if (skipped) return (FALSE);

    /* "Unique" monsters cannot be polymorphed */
    if (mon_race_is_unique(race)) do_poly = 0;

    /* Quest monsters cannot be polymorphed */
    if (mon->mflag2 & MFLAG2_QUESTOR) do_poly = 0;

    if (plr->riding && (mon->id == plr->riding)) do_poly = 0;

    /* "Unique" and "quest" monsters can only be "killed" by the player. */
    if ((mon_race_is_unique(race) || mon_race_is_nazgul(race) || (mon->mflag2 & MFLAG2_QUESTOR))
      && !prace_is_(RACE_MON_QUYLTHULG))
    {
        if (!who_is_plr(who) && (dam > mon->hp)) dam = mon->hp;
    }

    if (who_is_plr(who) && slept)
    {
        if (!mon_is_evil(mon) || one_in_(5)) virtue_add(VIRTUE_COMPASSION, -1);
        if (!mon_is_evil(mon) || one_in_(5)) virtue_add(VIRTUE_HONOUR, -1);
    }

    /* Modify the damage */
    tmp = dam;
    if (dam)
    {
        if (who_is_plr(who))
            dam = mon_damage_mod(mon, dam, gf == GF_PSY_SPEAR);
        else
            dam = mon_damage_mod_mon(mon, dam, gf == GF_PSY_SPEAR);
    }
    if (tmp > 0 && dam == 0)
        note = " is unharmed.";

    /* Check for death */
    if (dam > mon->hp)
    {
        /* Extract method of death */
        note = note_dies;
    }
    else
    {
        if (do_stun)
        {
            if (_1d(100) <= mon_res_pct(mon, GF_STUN))
            {
                mon_lore_resist(mon, GF_STUN);
            }
            else
            {
                mon_stun(mon, do_stun);
                if (seen) obvious = TRUE;
                get_angry = TRUE;
            }
        }

        if (do_blind)
        {
            if (_1d(100) <= mon_res_pct(mon, GF_BLIND))
            {
                mon_lore_resist(mon, GF_BLIND);
            }
            else
            {
                if (seen) obvious = TRUE;
                mon_tim_add(mon, T_BLIND, do_blind);
                get_angry = TRUE;
            }
        }
        if (do_conf)
        {
            if (gf != GF_ELDRITCH_CONFUSE && mon_res_pct(mon, GF_CONFUSION) > 0)
            { /* XXX note the lack of resistance roll! */
                mon_lore_resist(mon, GF_CONFUSION);
            }
            else
            {
                if (seen) obvious = TRUE;
                mon_tim_add(mon, T_CONFUSED, do_conf);
                get_angry = TRUE;
            }
        }

        if (do_time)
        {
            /* Obvious */
            if (seen) obvious = TRUE;

            if (do_time >= mon->maxhp) do_time = mon->maxhp - 1;

            if (do_time)
            {
                if (flags & GF_AFFECT_SPELL)
                    note = " seems weakened.";
                mon->maxhp -= do_time;
                if ((mon->hp - dam) > mon->maxhp) dam = mon->hp - mon->maxhp;
            }
            get_angry = TRUE;
        }

        /* Mega-Hack -- Handle "polymorph" -- monsters get a saving throw */
        if (do_poly && !mon_save_poly(race->alloc.lvl, do_poly))
        {
            if (polymorph_monster(mon))
            {
                if (seen) obvious = TRUE;
                race = mon->race;
            }
            else
            {
                note = " is unaffected!";
            }
        }

        /* Teleport */
        if (do_dist)
        {
            dun_grid_ptr g = dun_grid_at(cave, mon->pos);
            if ((mon->mflag2 & MFLAG2_VAULT) && !one_in_(3))
            {
                note = " resists!";
            }
            else if (g->flags & CELL_VAULT)
            {
                note = " is held in place by a mysterious force!";
            }
            else if (_1d(100) <= mon_res_pct(mon, GF_TELEPORT))
            {
                note = " resists!";
                mon_lore_resist(mon, GF_TELEPORT);
            }
            else
            {
                if (seen) obvious = TRUE;
                note = " disappears!";
                teleport_away(mon, do_dist,
                            (who_is_plr(who) ? TELEPORT_DEC_VALOUR : 0L) | TELEPORT_PASSIVE);
                where = mon->pos;
            }
        }

        /* Fear */
        if (do_fear)
        {
            if (mon_tim_find(mon, T_BERSERK))
            {
                note = " is immune!";
            }
            else if (_1d(100) <= mon_res_pct(mon, GF_FEAR))
            {
                note = " resists!";
                mon_lore_resist(mon, GF_FEAR);
            }
            else
            {
                mon_tim_add(mon, T_FEAR, do_fear);
                if ((flags & GF_AFFECT_ATTACK) && plr_attack_current()) /* beholder */
                    plr_attack_current()->fear = TRUE;
                get_angry = TRUE;
            }
        }
    }

    if (gf == GF_DRAIN_MANA)
    {
        /* Drain mana does nothing */
    }
    /* If another monster did the damage, hurt the monster by hand */
    else if (!who_is_plr(who))
    {
        check_mon_health_redraw(mon);
        mon_tim_delete(mon, MT_SLEEP);

        /* Hurt the monster */
        mon->project_dam += MIN(dam, mon->hp);
        mon->hp -= dam;

        /* Dead monster */
        if (mon->hp < 0)
        {
            bool sad = FALSE;

            if (mon_is_pet(mon) && !(mon->ml))
                sad = TRUE;

            if (mon->mflag2 & MFLAG2_ILLUSION)
                note = " is dispelled.";

            /* Give detailed messages if destroyed */
            if (known && note && seen_msg)
            {
                monster_desc(m_name, mon, MD_TRUE_NAME);
                msg_format("%^s%s", m_name, note);
            }

            if (caster_ptr)
                monster_gain_exp(caster_ptr, mon->race);

            mon_check_kill_unique(mon);

            /* Generate treasure, etc */
            monster_death(mon, who_is_pet || plr->spell_turned);

            /* Delete the monster */
            delete_monster(mon);

            if (sad)
            {
                msg_print("You feel sad for a moment.");
            }
        }

        /* Damaged monster */
        else
        {
            /* Give detailed messages if visible or destroyed */
            if (note && seen_msg) msg_format("%^s%s", m_name, note);

            /* Hack -- Pain message */
            else
            {
                mon_fight = TRUE;
            }

            if (do_sleep) mon_tim_add(mon, MT_SLEEP, do_sleep);
            if (do_paralyzed) mon_tim_add(mon, T_PARALYZED, do_paralyzed);
        }
    }
    else if (heal_leper)
    {
        if (seen_msg) msg_print("The Mangy looking leper is healed!");
        delete_monster(mon);
    }
    /* If the player did it, give him experience, check fear */
    else
    {
        bool fear = FALSE;

        /* Hacks  ... these effects probably belong in the gargantuan switch statement above ... sigh */
        /* Hack: The Draining Blast power gives hitpoints back. */
        if (gf == GF_ELDRITCH_DRAIN && mon_is_living(mon))
        {
            int heal = dam;
            if (heal > mon->hp)
                heal = mon->hp;
            heal /= 2;
            hp_player(heal);
        }

        /* Hack for device lore: Keep track of max damage experienced by any single
         * monster for this effect. */
        if (dam)
            mon->project_dam += MIN(dam, mon->hp);

        /* Player innate blows often gf_affect_m for effects. Keep the total damage up to
         * date, but be prepared for weirdness like Splatter that might affect other monsters. */
        if (plr_attack_current() && plr_attack_current()->mon == mon)
            plr_attack_current()->dam_total += dam;

        /* Hurt the monster, check for fear and death
              v---- Revert 525c2ace: Warlocks: Playtesting Dragon Pact. Massive problems with project
                    The problem here is that many attacks, like Slow Monster, do no physical damage, but
                    rely on mon_take_hit to do other things, like wake up sleeping monsters (or reveal
                    player ring mimics). This code needs refactoring ...*/
        if (/*dam &&*/ mon_take_hit(mon, dam, &fear, note_dies))
        {
            /* Dead monster */
        }

        /* Damaged monster */
        else
        {
            if (note == note_dies) /* Hack around crap code design ... Above we assumed monster would die but alas, we were wrong! */
                note = NULL;

            /* HACK - anger the monster before showing the sleep message */
            if (do_sleep) anger_monster(mon);

            /* HACK - tick off smart monsters whenever damaged by the player
               from a distance. */
            if ( who_is_plr(who)
              && dam > 0     /* v---- Too easy for spellcasters otherwise! */
              && (mon->cdis > 1 || (cave->flags & DF_NO_MELEE)) )
            {
                bool splashed = !plr_project(mon->pos);
                if (allow_ticked_off(race))
                {
                    if (!mut_present(MUT_SUBTLE_CASTING))
                        mon_anger_spell(mon, dam);
                    if (splashed)
                        mon_anger(mon);
                    /* Attempt to deal with Dungeon Guardians splash exploit.
                       Dungeon guardians use AI_GUARD_POS, so cannot be lured
                       away from the dungeon entrance. Attempting this exploit
                       makes them really mad, and if they are mad enough, then
                       they will actually pursue the player (cf get_moves in melee2.c) */
                    if (splashed && mon->cdis > MAX_RANGE)
                    {
                        if (mon->pack && mon->pack->ai == AI_GUARD_POS)
                            mon->anger = 100;
                        else
                            mon_anger(mon);
                    }
                }
                /* Splashing Uniques out of LOS makes them rethink their approach */
                if (splashed && mon_race_is_unique(race))
                {
                    if (mon->pack)
                    {
                        int odds = MAX(1, 7 - mon->anger/10);

                        if (!allow_ticked_off(race)) /* Paranoia ... These should already be seeking! */
                            odds = 1;

                        switch (mon->pack->ai)
                        {
                        case AI_MAINTAIN_DISTANCE:
                            if (one_in_(odds))
                                mon->pack->ai = AI_SEEK;
                            else
                                mon->pack->distance += 2;
                            break;

                        case AI_SHOOT:
                        case AI_LURE:
                            if (one_in_(odds))
                                mon->pack->ai = AI_SEEK;
                            break;
                        }
                    }
                }
            }

            /* Give detailed messages if visible or destroyed */
            if (seen_msg)
            {
                if (note)
                    msg_format("%^s%s", m_name, note);

                /* Hack -- Pain message */
                else if (known && dam && (flags & GF_AFFECT_SPELL))
                {
                    message_pain(mon->id, dam);
                }
            }

            /* Anger monsters */
            if (((dam > 0) || get_angry) && !do_sleep)
                anger_monster(mon);

            /* Take note */
            if ((fear || do_fear) && seen && !plr_attack_current())
            {
                /* Sound */
                sound(SOUND_FLEE);

                /* Message */
                if (seen_msg)
                    msg_format("%^s flees in terror!", m_name);
            }

            /* Hack -- handle sleep */
            if (do_sleep) mon_tim_add(mon, MT_SLEEP, do_sleep);
            if (do_paralyzed) mon_tim_add(mon, T_PARALYZED, do_paralyzed);
        }
    }

    if (gf == GF_BLOOD_CURSE && mon_is_valid(mon) && one_in_(4))
    {
        int count = 0;
        do
        {
            switch (randint1(28))
            {
            case 1: case 2:
                if (!count)
                {
                    msg_print("The ground trembles...");

                    earthquake(mon->pos, 3 + _1d(4));
                    if (!one_in_(6)) break;
                }
            case 3: case 4: case 5: case 6: case 7: case 8:
                if (!count)
                {
                    msg_print("A portal opens to a plane of raw mana!");
                    plr_ball_direct(8, mon->pos, GF_MANA, _10d(10));
                    if (!one_in_(6)) break;
                }
            case 9: case 10: case 11:
                if (!count)
                {
                    msg_print("Space warps about it!");

                    if (mon->race->id) teleport_away(mon, damroll(10, 10), TELEPORT_PASSIVE);
                    if (one_in_(13)) count += activate_hi_summon(point_create(tx, ty), TRUE);
                    if (!one_in_(6)) break;
                }
            case 12: case 13: case 14: case 15: case 16:
                msg_print("It feels a surge of energy!");
                plr_ball_direct(7, mon->pos, GF_DISINTEGRATE, 50);
                if (!one_in_(6)) break;
            case 17: case 18: case 19:
                aggravate_monsters(who_create_null());
                if (!one_in_(6)) break;
            case 20: case 21:
                count += activate_hi_summon(point_create(tx, ty), TRUE);
                if (!one_in_(6)) break;
            case 22: case 23: case 24: case 25: case 26:
            {
                bool pet = !one_in_(3);
                who_t  who = pet ? who_create_plr() : who_create_null();
                u32b mode = PM_ALLOW_GROUP;

                if (pet) mode |= PM_FORCE_PET;
                else mode |= (PM_NO_PET | PM_FORCE_FRIENDLY);

                count += summon_specific(who, plr->pos, (pet ? plr->lev*2/3+randint1(plr->lev/2) : cave->dun_lvl), 0, mode);
                if (!one_in_(6)) break;
            }
            case 27:
                if (plr->hold_life && (randint0(100) < 75)) break;
                msg_print("You feel your life draining away...");

                if (plr->hold_life) lose_exp(plr->exp / 160);
                else lose_exp(plr->exp / 16);
                if (!one_in_(6)) break;
            case 28:
            {
                int i = 0;
                if (one_in_(13))
                {
                    while (i < 6)
                    {
                        do
                        {
                            (void)do_dec_stat(i);
                        }
                        while (one_in_(2));

                        i++;
                    }
                }
                else
                {
                    (void)do_dec_stat(randint0(6));
                }
                break;
            }
            }
        }
        while (one_in_(5));
    }

    /* XXX XXX XXX Verify this code */

    /* Update the monster */
    if (mon_is_valid(mon)) update_mon(mon, FALSE);

    /* Redraw the monster grid */
    draw_pos(where);

    /* Update monster recall window */
    if (plr->monster_race_idx == mon->race->id && (seen || mon_is_dead(mon)))
    {
        /* Window stuff */
        plr->window |= (PW_MONSTER);
    }

    /* Update monster target: Spells only. For melee, cf plr_attack_end and
     * mon_attack_end. Auras and traps should never alter the target.
     * XXX Not every spell does damage ... */
    if (dam > 0 && mon_is_valid(mon) && (flags & GF_AFFECT_SPELL))
    {
        if (who_is_plr(who))
        {
            /* FYI: monster_target is a hack for the mirror-master, and
             * it is only used via project. This allows mirror trick
             * shots to fool monsters, luring them to the last broken mirror. */
            if (mon_is_hostile(mon))
            {
                if (flags & GF_AFFECT_PROJECT) /* XXX cf project_m */
                    mon_set_target(mon, monster_target);
                else
                    mon_set_target(mon, plr->pos);
            }
        }
        else if (caster_ptr)
        {
            if (are_enemies(mon, caster_ptr))
                mon_set_target(mon, caster_ptr->pos);
        }
    }

    /* Return "Anything seen?" */
    mon->project_notice = obvious;
    return (obvious);
}

bool gf_affect_f(who_t who, point_t where, int type, int dam, int flags)
{
    dun_cell_ptr cell = dun_grid_at(cave, where);
    return cell_affect(cave, where, cell, type, dam);
}

bool gf_affect_o(who_t who, point_t where, int type, int dam, int flags)
{
    int i;
    bool notice = FALSE;
    bool los = plr_view(where);
    u32b flgs[OF_ARRAY_SIZE];
    char o_name[MAX_NLEN];

    /* Note that potion_smash_effect induces recursion, 
     * perhaps destroying the very pile we are processing!
     * Thus, we smash potions after processing this pile. */
    int potion_k_idx[32];
    int potion_ct = 0;

    /* Using a pile (vs obj->next links) simplifies deletion during iteration. */
    vec_ptr pile = dun_pile_at(cave, where);

    /* Scan all objects in the grid */
    for (i = 0; i < vec_length(pile); i++)
    {
        obj_ptr obj = vec_get(pile, i);
        bool is_art = FALSE;
        bool ignore = FALSE;
        bool do_kill = FALSE;

        cptr note_kill = NULL;

        /* Get the "plural"-ness */
        bool plural = (obj->number > 1);

        /* Extract the flags */
        obj_flags(obj, flgs);

        /* Check for artifact */
        if (obj_is_art(obj)) is_art = TRUE;
        else if (obj->name2 == EGO_AMMO_ENDURANCE) is_art = TRUE; /* lazy */

        /* Analyze the type */
        switch (type)
        {
        case GF_ACID:
            if (hates_acid(obj))
            {
                do_kill = TRUE;
                note_kill = (plural ? " melt!" : " melts!");
                if (have_flag(flgs, OF_IGNORE_ACID)) ignore = TRUE;
            }
            break;
        case GF_ELEC:
            if (hates_elec(obj))
            {
                do_kill = TRUE;
                note_kill = (plural ? " are destroyed!" : " is destroyed!");
                if (have_flag(flgs, OF_IGNORE_ELEC)) ignore = TRUE;
            }
            break;
        case GF_FIRE:
            if (hates_fire(obj))
            {
                do_kill = TRUE;
                note_kill = (plural ? " burn up!" : " burns up!");
                if (have_flag(flgs, OF_IGNORE_FIRE)) ignore = TRUE;
            }
            break;
        case GF_COLD:
            if (hates_cold(obj))
            {
                note_kill = (plural ? " shatter!" : " shatters!");
                do_kill = TRUE;
                if (have_flag(flgs, OF_IGNORE_COLD)) ignore = TRUE;
            }
            break;
        case GF_PLASMA:
            if (hates_fire(obj))
            {
                do_kill = TRUE;
                note_kill = (plural ? " burn up!" : " burns up!");
                if (have_flag(flgs, OF_IGNORE_FIRE)) ignore = TRUE;
            }
            if (hates_elec(obj))
            {
                ignore = FALSE;
                do_kill = TRUE;
                note_kill = (plural ? " are destroyed!" : " is destroyed!");
                if (have_flag(flgs, OF_IGNORE_ELEC)) ignore = TRUE;
            }
            break;
        case GF_METEOR:
        case GF_ROCKET:
            if (hates_fire(obj))
            {
                do_kill = TRUE;
                note_kill = (plural ? " burn up!" : " burns up!");
                if (have_flag(flgs, OF_IGNORE_FIRE)) ignore = TRUE;
            }
            if (hates_cold(obj))
            {
                ignore = FALSE;
                do_kill = TRUE;
                note_kill = (plural ? " shatter!" : " shatters!");
                if (have_flag(flgs, OF_IGNORE_COLD)) ignore = TRUE;
            }
            break;
        case GF_ICE:
        case GF_SHARDS:
        case GF_ROCK:
        case GF_FORCE:
        case GF_SOUND:
            if (hates_cold(obj))
            {
                note_kill = (plural ? " shatter!" : " shatters!");
                do_kill = TRUE;
            }
            break;
        case GF_MANA:
        case GF_SEEKER:
        case GF_SUPER_RAY:
            do_kill = TRUE;
            note_kill = (plural ? " are destroyed!" : " is destroyed!");
            break;
        case GF_DISINTEGRATE:
            do_kill = TRUE;
            note_kill = (plural ? " evaporate!" : " evaporates!");
            break;
        case GF_CHAOS:
            do_kill = TRUE;
            note_kill = (plural ? " are destroyed!" : " is destroyed!");
            if (have_flag(flgs, OF_RES_(GF_CHAOS))) ignore = TRUE;
            break;
        case GF_HOLY_FIRE:
        case GF_HELL_FIRE:
            if (obj_is_cursed(obj))
            {
                do_kill = TRUE;
                note_kill = (plural ? " are destroyed!" : " is destroyed!");
            }
            break;
        case GF_IDENTIFY:
            if (!obj_is_identified(obj))
                notice = TRUE; /* XXX identify_item has queer semantics ... */
            identify_item(obj);
            autopick_alter_obj(obj, FALSE); /* inscription */
            break;
    case GF_KILL_TRAP:
    case GF_KILL_DOOR:
    case GF_REMOVE_OBSTACLE:
            /* Chests are noticed only if trapped or locked */
            if (obj->tval == TV_CHEST)
            {
                /* Disarm/Unlock traps */
                if (obj->pval > 0)
                {
                    /* Disarm or Unlock */
                    obj->pval = (0 - obj->pval);

                    /* Identify */
                    obj_identify(obj);

                    /* Notice */
                    if (los && (obj->marked & OM_FOUND))
                    {
                        msg_print("Click!");
                        notice = TRUE;
                    }
                }
            }
            break;
        case GF_ANIM_DEAD:
            if (obj->tval == TV_CORPSE)
            {
                int i;
                u32b mode = 0L;

                if (who_is_plr(who) || who_is_pet(who))
                    mode |= PM_FORCE_PET;

                for (i = 0; i < obj->number ; i++)
                {
                    if ( (obj->sval == SV_CORPSE && randint1(100) > 80)
                      || (obj->sval == SV_SKELETON && randint1(100) > 60) )
                    {
                        if (!note_kill)
                            note_kill = (plural ? " become dust." : " becomes dust.");
                        continue;
                    }
                    else if (summon_named_creature(who, where, mon_race_lookup(obj->race_id), mode))
                    {
                        note_kill = " revived.";
                    }
                    else if (!note_kill)
                    {
                        note_kill = (plural ? " become dust." : " becomes dust.");
                    }
                }
                do_kill = TRUE;
                notice = TRUE;
            }
            break;
        }

        /* Attempt to destroy the object */
        if (do_kill)
        {
            /* Effect "observed" */
            if (los && (obj->marked & OM_FOUND))
            {
                notice = TRUE;
                object_desc(o_name, obj, (OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED));
            }

            /* Artifacts, and other objects, get to resist */
            if (is_art || ignore)
            {
                if (los && (obj->marked & OM_FOUND))
                {
                    msg_format("The %s %s unaffected!",
                            o_name, (plural ? "are" : "is"));
                }
            }
            /* Kill it */
            else
            {
                if (los && (obj->marked & OM_FOUND) && note_kill)
                {
                    msg_format("The %s%s", o_name, note_kill);
                    stats_on_m_destroy(obj, obj->number);
                }

                if (obj_is_potion(obj)) /* smash it later */
                    potion_k_idx[potion_ct++] = obj->k_idx;

                delete_object_idx(obj->loc.v.floor.obj_id);
                draw_pos(where);
            }
        }
    }
    vec_free(pile);

    /* Potions produce effects when 'shattered' */
    for (i = 0; i < potion_ct; i++)
        potion_smash_effect(who, where, potion_k_idx[i]);

    return notice;
}
