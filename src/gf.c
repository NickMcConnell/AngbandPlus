#include "angband.h"
#include "gf.h"

#include <assert.h>

static gf_info_t _gf_tbl[GF_COUNT] = {
    { GF_NONE, "None", TERM_L_DARK, RES_INVALID, "NONE"},

    /* Elemental Effects */
    { GF_ACID, "Acid", TERM_GREEN, RES_ACID, "ACID", GFF_ELEMENTAL | GFF_DAMAGE | GFF_RIDING },
    { GF_ELEC, "Lightning", TERM_BLUE, RES_ELEC, "ELEC", GFF_ELEMENTAL | GFF_DAMAGE | GFF_RIDING },
    { GF_FIRE, "Fire", TERM_RED, RES_FIRE, "FIRE", GFF_ELEMENTAL | GFF_DAMAGE | GFF_RIDING },
    { GF_COLD, "Frost", TERM_L_WHITE, RES_COLD, "COLD", GFF_ELEMENTAL | GFF_DAMAGE | GFF_RIDING },
    { GF_POIS, "Poison", TERM_L_GREEN, RES_POIS, "POIS", GFF_ELEMENTAL | GFF_DAMAGE | GFF_RIDING },
    { GF_LITE, "Light", TERM_YELLOW, RES_LITE, "LITE", GFF_ELEMENTAL | GFF_DAMAGE | GFF_RIDING },
    { GF_DARK, "Darkness", TERM_L_DARK, RES_DARK, "DARK", GFF_ELEMENTAL | GFF_DAMAGE | GFF_RIDING },
    { GF_CONFUSION, "Confusion", TERM_L_UMBER, RES_CONF, "CONFUSION", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_NETHER, "Nether", TERM_L_DARK, RES_NETHER, "NETHER", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_NEXUS, "Nexus", TERM_VIOLET, RES_NEXUS, "NEXUS", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_SOUND, "Sound", TERM_ORANGE, RES_SOUND, "SOUND", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_SHARDS, "Shards", TERM_L_UMBER, RES_SHARDS, "SHARDS", GFF_ELEMENTAL | GFF_DAMAGE | GFF_RIDING },
    { GF_CHAOS, "Chaos", TERM_VIOLET, RES_CHAOS, "CHAOS", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_DISENCHANT, "Disenchantment", TERM_VIOLET, RES_DISEN, "DISENCHANT", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_TIME, "Time", TERM_L_BLUE, RES_TIME, "TIME", GFF_ELEMENTAL | GFF_DAMAGE },

    { GF_MANA, "Mana", TERM_L_BLUE, RES_INVALID, "MANA", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_GRAVITY, "Gravity", TERM_L_UMBER, RES_INVALID, "GRAVITY", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_INERT, "Inertia", TERM_L_UMBER, RES_INVALID, "INERTIA", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_PLASMA, "Plasma", TERM_L_RED, RES_INVALID, "PLASMA", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_FORCE, "Force", TERM_L_BLUE, RES_INVALID, "FORCE", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_NUKE, "Toxic Waste", TERM_L_GREEN, RES_POIS, "NUKE", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_DISINTEGRATE, "Disintegration", TERM_L_DARK, RES_INVALID, "DISINTEGRATE", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_STORM, "Storm Winds", TERM_BLUE, RES_INVALID, "STORM", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_HOLY_FIRE, "Holy Fire", TERM_YELLOW, RES_INVALID, "HOLY_FIRE", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_HELL_FIRE, "Hell Fire", TERM_L_DARK, RES_INVALID, "HELL_FIRE", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_ICE, "Ice", TERM_L_WHITE, RES_COLD, "ICE", GFF_ELEMENTAL | GFF_DAMAGE | GFF_RIDING },
    { GF_WATER, "Water", TERM_L_BLUE, RES_INVALID, "WATER", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_ROCKET, "Rocket", TERM_RED, RES_SHARDS, "ROCKET", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_METEOR, "Meteor", TERM_RED, RES_INVALID, "METEOR", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_ROCK, "Rock", TERM_L_UMBER, RES_INVALID, "ROCK", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_ARROW, "Arrow", TERM_L_UMBER, RES_INVALID, "ARROW", GFF_ELEMENTAL | GFF_DAMAGE },
    { GF_MISSILE, "Missile", TERM_L_UMBER, RES_INVALID, "MISSILE", GFF_ELEMENTAL | GFF_DAMAGE },

    /* Curses */
    { GF_CAUSE_1, "Cause Light Wounds", TERM_RED, RES_INVALID, "CAUSE_1", GFF_CURSE | GFF_DAMAGE },
    { GF_CAUSE_2, "Cause Serious Wounds", TERM_RED, RES_INVALID, "CAUSE_2", GFF_CURSE | GFF_DAMAGE },
    { GF_CAUSE_3, "Cause Critical Wounds", TERM_RED, RES_INVALID, "CAUSE_3", GFF_CURSE | GFF_DAMAGE },
    { GF_CAUSE_4, "Cause Mortal Wounds", TERM_RED, RES_INVALID, "CAUSE_4", GFF_CURSE | GFF_DAMAGE },
    { GF_HAND_DOOM, "Hand of Doom", TERM_VIOLET, RES_INVALID, "HAND_DOOM", GFF_CURSE | GFF_DAMAGE },
    { GF_BLOOD_CURSE, "Blood Curse", TERM_VIOLET, RES_INVALID, "BLOOD_CURSE", GFF_CURSE | GFF_DAMAGE },

    /* Mental Attacks */
    { GF_PSY_SPEAR, "Psycho-Spear", TERM_L_BLUE, RES_INVALID, "PSY_SPEAR", GFF_MENTAL | GFF_DAMAGE },
    { GF_PSI, "Psionics", TERM_L_BLUE, RES_INVALID, "PSI", GFF_MENTAL | GFF_DAMAGE },
    { GF_PSI_DRAIN, "Psionic Drain", TERM_L_BLUE, RES_INVALID, "PSI_DRAIN", GFF_MENTAL },
    { GF_PSI_BRAIN_SMASH, "Brain Smash", TERM_L_BLUE, RES_INVALID, "PSI_BRAIN_SMASH", GFF_MENTAL | GFF_DAMAGE },
    { GF_PSI_STORM, "Psycho-Storm", TERM_L_BLUE, RES_INVALID, "PSI_STORM", GFF_MENTAL | GFF_DAMAGE },
    { GF_TELEKINESIS, "Pulverise", TERM_L_BLUE, RES_INVALID, "TELEKINESIS", GFF_MENTAL | GFF_DAMAGE },
    { GF_DOMINATION, "Domination", TERM_RED, RES_INVALID, "DOMINATION", GFF_MENTAL },
    { GF_SUBJUGATION, "Subjugation", TERM_RED, RES_INVALID, "SUBJUGATION", GFF_MENTAL },
    { GF_DRAIN_MANA, "Drain Mana", TERM_L_BLUE, RES_INVALID, "DRAIN_MANA", GFF_MENTAL },
    { GF_MIND_BLAST, "Mind Blast", TERM_L_BLUE, RES_INVALID, "MIND_BLAST", GFF_MENTAL },
    { GF_BRAIN_SMASH, "Brain Smash", TERM_L_BLUE, RES_INVALID, "BRAIN_SMASH", GFF_MENTAL | GFF_DAMAGE },
    { GF_AMNESIA, "Amnesia", TERM_L_DARK, RES_INVALID, "AMNESIA", GFF_MENTAL },

    /* Status Effects */
    { GF_BLIND, "Blind", TERM_L_DARK, RES_INVALID, "BLIND", GFF_STATUS },
    { GF_OLD_CLONE, "Clone", TERM_RED, RES_INVALID, "OLD_CLONE", GFF_STATUS },
    { GF_OLD_POLY, "Polymorph", TERM_RED, RES_INVALID, "OLD_POLY", GFF_STATUS },
    { GF_OLD_HEAL, "Heal", TERM_WHITE, RES_INVALID, "OLD_HEAL", GFF_STATUS },
    { GF_STAR_HEAL, "Heal", TERM_WHITE, RES_INVALID, "STAR_HEAL", GFF_STATUS },
    { GF_OLD_SPEED, "Haste", TERM_L_RED, RES_INVALID, "OLD_SPEED", GFF_STATUS },
    { GF_OLD_SLOW, "Slow", TERM_L_UMBER, RES_INVALID, "OLD_SLOW", GFF_STATUS },
    { GF_OLD_CONF, "Confuse", TERM_L_UMBER, RES_INVALID, "OLD_CONF", GFF_STATUS },
    { GF_OLD_SLEEP, "Sleep", TERM_BLUE, RES_INVALID, "OLD_SLEEP", GFF_STATUS },
    { GF_OLD_DRAIN, "Drain", TERM_L_DARK, RES_INVALID, "OLD_DRAIN", GFF_STATUS },
    { GF_STASIS, "Freeze", TERM_BLUE, RES_INVALID, "STASIS", GFF_STATUS },
    { GF_STASIS_EVIL, "Freeze Evil", TERM_BLUE, RES_INVALID, "STASIS_EVIL", GFF_STATUS },
    { GF_PARALYSIS, "Paralyze", TERM_VIOLET, RES_INVALID, "PARALYZE", GFF_STATUS },
    { GF_STUN, "Stun", TERM_L_BLUE, RES_INVALID, "STUN", GFF_STATUS },
    { GF_ELDRITCH, "Eldritch Horror", TERM_VIOLET, RES_INVALID, "ELDRITCH", GFF_STATUS },
    { GF_ANTIMAGIC, "Anti-magic", TERM_RED, RES_INVALID, "ANTIMAGIC", GFF_STATUS },
    { GF_CRUSADE, "Crusade", TERM_WHITE, RES_INVALID, "CRUSADE", GFF_STATUS },
    { GF_UNHOLY_WORD, "Unholy Word", TERM_L_DARK, RES_INVALID, "UNHOLY_WORD", GFF_STATUS },
    { GF_UNLIFE, "Unlife", TERM_L_DARK, RES_INVALID, "UNLIFE", GFF_STATUS },

    /* Terrain Effects */
    { GF_LITE_WEAK, "Light", TERM_YELLOW, RES_INVALID, "LITE_WEAK", GFF_TERRAIN },
    { GF_DARK_WEAK, "Dark", TERM_L_DARK, RES_INVALID, "DARK_WEAK", GFF_TERRAIN },
    { GF_KILL_WALL, "Stone to Mud", TERM_L_UMBER, RES_INVALID, "KILL_WALL", GFF_TERRAIN },
    { GF_KILL_DOOR, "Door Destruction", TERM_RED, RES_INVALID, "KILL_DOOR", GFF_TERRAIN },
    { GF_KILL_TRAP, "Trap Destruction", TERM_RED, RES_INVALID, "KILL_TRAP", GFF_TERRAIN },
    { GF_REMOVE_OBSTACLE, "Remove Obstacle", TERM_RED, RES_INVALID, "REMOVE_OBSTACLE", GFF_TERRAIN },
    { GF_MAKE_DOOR, "Door Creation", TERM_L_BLUE, RES_INVALID, "MAKE_DOOR", GFF_TERRAIN },
    { GF_MAKE_TRAP, "Trap Creation", TERM_L_RED, RES_INVALID, "MAKE_TRAP", GFF_TERRAIN },
    { GF_MAKE_TREE, "Forest Creation", TERM_L_GREEN, RES_INVALID, "MAKE_TREE", GFF_TERRAIN },
    { GF_MAKE_GLYPH, "Glyph of Warding", TERM_L_BLUE, RES_INVALID, "MAKE_GLYPH", GFF_TERRAIN },
    { GF_MAKE_WALL, "Wall Creation", TERM_SLATE, RES_INVALID, "MAKE_WALL", GFF_TERRAIN },
    { GF_JAM_DOOR, "Wizard Lock", TERM_RED, RES_INVALID, "JAM_DOOR", GFF_TERRAIN },
    { GF_WATER_FLOW, "Flow of Water", TERM_BLUE, RES_INVALID, "WATER_FLOW", GFF_TERRAIN },
    { GF_WATER2, "Flow of Water", TERM_BLUE, RES_INVALID, "WATER2", GFF_TERRAIN },
    { GF_LAVA_FLOW, "Flow of Lava", TERM_RED, RES_INVALID, "LAVA_FLOW", GFF_TERRAIN },
    { GF_WEB, "Web Spinning", TERM_SLATE, RES_INVALID, "WEB", GFF_TERRAIN },
    { GF_QUAKE, "Earthquake", TERM_L_UMBER, RES_INVALID, "QUAKE", GFF_TERRAIN },

    /* Turning, Dispelling, Controlling, etc */
    { GF_AWAY_UNDEAD, "Banish Undead", TERM_L_BLUE, RES_INVALID, "AWAY_UNDEAD", GFF_TELEPORT },
    { GF_AWAY_EVIL, "Banish Evil", TERM_L_BLUE, RES_INVALID, "AWAY_EVIL", GFF_TELEPORT },
    { GF_AWAY_ALL, "Banishment", TERM_L_BLUE, RES_INVALID, "AWAY_ALL", GFF_TELEPORT },
    { GF_ISOLATION, "Isolation", TERM_L_BLUE, RES_INVALID, "ISOLATION", GFF_TELEPORT },
    { GF_TURN_UNDEAD, "Turn Undead", TERM_RED, RES_INVALID, "TURN_UNDEAD", GFF_STATUS },
    { GF_TURN_EVIL, "Turn Evil", TERM_RED, RES_INVALID, "TURN_EVIL", GFF_STATUS },
    { GF_TURN_ALL, "Turn Monsters", TERM_RED, RES_INVALID, "TURN_ALL", GFF_STATUS },
    { GF_DISP_UNDEAD, "Dispel Undead", TERM_L_RED, RES_INVALID, "DISP_UNDEAD", GFF_DAMAGE },
    { GF_DISP_EVIL, "Dispel Evil", TERM_L_RED, RES_INVALID, "DISP_EVIL", GFF_DAMAGE },
    { GF_DISP_GOOD, "Dispel Good", TERM_L_RED, RES_INVALID, "DISP_GOOD", GFF_DAMAGE },
    { GF_DISP_DEMON, "Dispel Demon", TERM_L_RED, RES_INVALID, "DISP_DEMON", GFF_DAMAGE },
    { GF_DISP_LIVING, "Dispel Living", TERM_L_RED, RES_INVALID, "DISP_LIVING", GFF_DAMAGE },
    { GF_DISP_ALL, "Dispel Monsters", TERM_L_RED, RES_INVALID, "DISP_ALL", GFF_DAMAGE },
    { GF_CONTROL_UNDEAD, "Enslave Undead", TERM_L_BLUE, RES_INVALID, "CONTROL_UNDEAD", GFF_CHARM },
    { GF_CONTROL_DEMON, "Dominate Demon", TERM_L_BLUE, RES_INVALID, "CONTROL_DEMON", GFF_CHARM },
    { GF_CONTROL_ANIMAL, "Charm Animal", TERM_L_BLUE, RES_INVALID, "CONTROL_ANIMAL", GFF_CHARM },
    { GF_CONTROL_LIVING, "Charm Living", TERM_L_BLUE, RES_INVALID, "CONTROL_LIVING", GFF_CHARM },
    { GF_CONTROL_PACT_MONSTER, "Control Pact Monster", TERM_L_BLUE, RES_INVALID, "CONTROL_PACT_MONSTER", GFF_CHARM },
    { GF_CHARM, "Charm Monster", TERM_L_BLUE, RES_INVALID, "CHARM", GFF_CHARM },
    { GF_CHARM_RING_BEARER, "Charm Ring Bearer", TERM_L_BLUE, RES_INVALID, "CHARM_RING_BEARER", GFF_CHARM },
    { GF_CAPTURE, "Capture Monster", TERM_L_BLUE, RES_INVALID, "CAPTURE", GFF_CHARM },
    { GF_ANIM_DEAD, "Raise Dead", TERM_L_DARK, RES_INVALID, "ANIM_DEAD" },
    { GF_DEATH_RAY, "Death Ray", TERM_L_DARK, RES_INVALID, "DEATH_RAY" },
    { GF_GENOCIDE, "Genocide", TERM_L_DARK, RES_INVALID, "GENOCIDE" },

    /* Object Effects */
    { GF_IDENTIFY, "Identify", TERM_L_BLUE, RES_INVALID, "IDENTIFY", GFF_OBJECT },

    /* Class Specific */
    { GF_ATTACK, "Attack", TERM_RED, RES_INVALID, "ATTACK", GFF_SPECIAL },
    { GF_ENGETSU, "Moon Dazzling", TERM_YELLOW, RES_INVALID, "ENGETSU", GFF_SPECIAL },
    { GF_SEEKER, "Seeker Ray", TERM_YELLOW, RES_INVALID, "SEEKER", GFF_SPECIAL | GFF_DAMAGE },
    { GF_SUPER_RAY, "Super Ray", TERM_YELLOW, RES_INVALID, "SUPER_RAY", GFF_SPECIAL | GFF_DAMAGE },
    { GF_BLOOD, "Blood", TERM_RED, RES_INVALID, "BLOOD", GFF_SPECIAL },
    { GF_ELDRITCH_STUN, "Eldritch Stun", TERM_L_BLUE, RES_INVALID, "ELDRITCH_STUN", GFF_SPECIAL },
    { GF_ELDRITCH_DRAIN, "Eldritch Drain", TERM_L_DARK, RES_INVALID, "ELDRITCH_DRAIN", GFF_SPECIAL },
    { GF_ELDRITCH_DISPEL, "Eldritch Dispel", TERM_L_RED, RES_INVALID, "ELDRITCH_DISPEL", GFF_SPECIAL | GFF_DAMAGE },
    { GF_ELDRITCH_CONFUSE, "Eldritch Confuse", TERM_L_UMBER, RES_INVALID, "ELDRITCH_CONFUSE", GFF_SPECIAL },
    { GF_ELDRITCH_HOWL, "Eldritch Howl", TERM_L_DARK, RES_INVALID, "ELDRITCH_HOWL", GFF_SPECIAL },
    { GF_ENTOMB, "Entomb", TERM_L_UMBER, RES_INVALID, "ENTOMB", GFF_SPECIAL },
    { GF_MANA_CLASH, "Mana Clash", TERM_L_BLUE, RES_INVALID, "MANA_CLASH", GFF_SPECIAL },
    { GF_PHARAOHS_CURSE, "Pharaoh's Curse", TERM_VIOLET, RES_INVALID, "PHARAOHS_CURSE", GFF_SPECIAL | GFF_CURSE | GFF_DAMAGE },
    { GF_DRAINING_TOUCH, "Draining Touch", TERM_L_DARK, RES_INVALID, "DRAINING_TOUCH", GFF_SPECIAL },
    { GF_DEATH_TOUCH, "Touch of Death", TERM_L_DARK, RES_INVALID, "DEATH_TOUCH", GFF_SPECIAL },
    { GF_STEAL, "Steal", TERM_WHITE, RES_INVALID, "STEAL", GFF_SPECIAL },
    { GF_LICH_DRAIN, "Orb of Draining", TERM_L_DARK, RES_INVALID, "LICH_DRAIN", GFF_DAMAGE },
    { GF_LICH_GENOCIDE, "Send to Netherworld", TERM_L_DARK, RES_INVALID, "LICH_GENOCIDE" },
    { GF_LICH_WORD, "Word of Vecna", TERM_VIOLET, RES_INVALID, "LICH_WORD", GFF_STATUS | GFF_DAMAGE },

    /* New Stuff ... Reorder later */
};

typedef struct {
    cptr parse;
    int id;
} _alias_t, *_alias_ptr;
static _alias_t _aliases[] = {
    { "DAM", GF_MISSILE },
    { "POISON", GF_POIS },
    { "LIGHT", GF_LITE },
    { "CONF", GF_CONFUSION },
    { "CONFUSE", GF_CONFUSION },
    { "DISENCHANTMENT", GF_DISENCHANT },
    { "PULVERISE", GF_TELEKINESIS },
    { "TERRIFY", GF_TURN_ALL },
    { "POLYMORPH", GF_OLD_POLY },
    { "SLOW", GF_OLD_SLOW },
    { "SLEEP", GF_OLD_SLEEP },
    { 0 }
};

gf_info_ptr gf_parse_name(cptr token)
{
    static str_map_ptr _map = NULL;
    if (!_map)
    {
        int i;
        _map = str_map_alloc(NULL);
        for (i = 0; i < GF_COUNT; i++)
        {
            gf_info_ptr info = &_gf_tbl[i];
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
    gf_info_ptr gf = NULL;
    if (0 < id && id < GF_COUNT)
    {
        gf = &_gf_tbl[id];
        assert(gf->id == id);
    }
    return gf;
}

#define HURT_CHANCE 16

static int _rlev(int m_idx)
{
    if (m_idx > 0)
    {
        mon_ptr mon = dun_mon(cave, m_idx);
        mon_race_ptr race = mon_race(mon);
        return race->level;
    }
    return 0;
}
static int _plr_save_odds(int m_idx, int boost)
{
    int rlev = _rlev(m_idx);
    int roll = 100 + rlev/2 + boost;
    int sav = plr_skill_sav(m_idx);
    int odds = sav * 100 / roll;
    return odds;
}
static bool _plr_save(int m_idx, int boost)
{
    int odds = _plr_save_odds(m_idx, boost);
    return randint0(100) < odds;
}
static int _align_dam_pct(int align)
{
    static point_t tbl[6] = { {-150, 200}, {-50, 150}, {-10, 125},
                              {10, 80}, {50, 66}, {150, 50} };

    return interpolate(align, tbl, 6);
}
int gf_holy_dam(int dam)
{
    return dam * _align_dam_pct(p_ptr->align) / 100;
}
int gf_hell_dam(int dam)
{
    return dam * _align_dam_pct(-p_ptr->align) / 100;
}
int gf_affect_p(int who, int type, int dam, int flags)
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

    if (who > 0)
    {
        m_ptr = dun_mon(cave, who);
        r_ptr = mon_race(m_ptr);
        rlev = MAX(1, r_ptr->level*m_ptr->mpower/1000);

        monster_desc(m_name, m_ptr, 0);
        monster_desc(m_name_subject, m_ptr, MD_PRON_VISIBLE);
    }
    else
    {
        switch (who)
        {
        case PROJECT_WHO_UNCTRL_POWER:
            strcpy(m_name, "uncontrollable power storm");
            break;

        case PROJECT_WHO_GLASS_SHARDS:
            strcpy(m_name, "shards of glass");
            break;

        case GF_WHO_TRAP:
        default:
            strcpy(m_name, "a trap");
            break;
        }
    }

    /* Analyze the damage */
    switch (type)
    {
    case GF_ACID:
        dam = res_calc_dam(RES_ACID, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:G>dissolved</color>!");
            else if (fuzzy) msg_print("You are hit by acid!");
            if (aura || !CHECK_MULTISHADOW())
            {
                if (!res_save_default(RES_ACID) && one_in_(stat_drain_odds))
                    do_dec_stat(A_CHR);
                if (minus_ac()) dam = (dam + 1) / 2;
            }
            result = take_hit(damage_type, dam, m_name);
            if (!aura || randint0(50) < dam)
                inven_damage(set_acid_destroy, 3, RES_ACID);
        }
        update_smart_learn(who, RES_ACID);
        break;
    case GF_FIRE:
        dam = res_calc_dam(RES_FIRE, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:r>burned</color>!");
            else if (fuzzy) msg_print("You are hit by fire!");
            if (aura || !CHECK_MULTISHADOW())
            {
                if (!res_save_default(RES_FIRE) && one_in_(stat_drain_odds))
                    do_dec_stat(A_STR);
            }

            result = take_hit(damage_type, dam, m_name);
            if (!aura || randint0(50) < dam)
                 inven_damage(set_fire_destroy, 3, RES_FIRE);
        }
        update_smart_learn(who, RES_FIRE);
        break;
    case GF_COLD:
        dam = res_calc_dam(RES_COLD, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:W>frozen</color>!");
            else if (fuzzy) msg_print("You are hit by cold!");
            if (aura || !CHECK_MULTISHADOW())
            {
                if (!res_save_default(RES_COLD) && one_in_(stat_drain_odds))
                    do_dec_stat(A_STR);
            }
            result = take_hit(damage_type, dam, m_name);
            if (!aura || randint0(50) < dam)
                 inven_damage(set_cold_destroy, 3, RES_COLD);
        }
        update_smart_learn(who, RES_COLD);
        break;
    case GF_ELEC:
        dam = res_calc_dam(RES_ELEC, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:b>shocked</color>!");
            else if (fuzzy) msg_print("You are hit by lightning!");
            if (aura || !CHECK_MULTISHADOW())
            {
                if (!res_save_default(RES_ELEC) && one_in_(stat_drain_odds))
                    do_dec_stat(A_DEX);
            }
            result = take_hit(damage_type, dam, m_name);
            if (!aura || randint0(50) < dam)
                 inven_damage(set_elec_destroy, 3, RES_ELEC);
        }
        update_smart_learn(who, RES_ELEC);
        break;
    case GF_POIS:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(RES_POIS, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:G>poisoned</color>!");
            else if (fuzzy) msg_print("You are hit by poison!");
            /* Moving damage from immediate to delayed can't simply leave the
             * value unchanged, else this is a monster nerf! We can scale everything
             * in r_info and BR_POIS, but that is tedious and I'm unsure what a good
             * scale factor is without some playtesting. cf GF_NUKE below. */
            dam = dam*7/4;
            plr_tim_add(T_POISON, dam);
            if (!res_save_default(RES_POIS) && one_in_(stat_drain_odds))
                do_dec_stat(A_CON);
        }
        update_smart_learn(who, RES_POIS);
        break;
    case GF_NUKE:
        if (!aura && CHECK_MULTISHADOW())
        {
            msg_print("The attack hits your Shadow. You are unharmed!");
            break;
        }
        dam = res_calc_dam(RES_POIS, dam);
        if (dam)
        {
            if (touch) msg_print("You are <color:G>irradiated</color>!");
            else if (fuzzy) msg_print("You are hit by radiation!");
            dam = dam*7/4;
            plr_tim_add(T_POISON, dam);
            if (!res_save_default(RES_POIS))
            {
                if (one_in_(5))
                {
                    msg_print("You undergo a freakish metamorphosis!");
                    if (one_in_(4))
                        do_poly_self();
                    else
                        mutate_player();
                }
                inven_damage(set_acid_destroy, 2, RES_POIS);
            }
        }
        update_smart_learn(who, RES_POIS);
        break;
    case GF_MISSILE:
    case GF_BLOOD:  /* Monsters can't do this ... */
        if (fuzzy) msg_print("You are hit by something!");
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_HOLY_FIRE:
        if (touch) msg_format("You are <color:y>%s</color>!", p_ptr->align < -10 ? "*burned*" : "burned");
        else if (fuzzy) msg_print("You are hit by something!");
        dam = gf_holy_dam(dam);
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_HELL_FIRE:
        if (touch) msg_format("You are <color:D>%s</color>!", p_ptr->align > 10 ? "*burned*" : "burned");
        else if (fuzzy) msg_print("You are hit by something!");
        dam = gf_hell_dam(dam);
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_ARROW:
        if (fuzzy) msg_print("You are hit by something sharp!");
        else if (equip_find_art(ART_ZANTETSU))
        {
            msg_print("You cut down the arrow!");
            break;
        }
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_PLASMA:
        if (touch) msg_print("You are <color:R>burned</color>!");
        else if (fuzzy) msg_print("You are hit by something *HOT*!");
        result = take_hit(damage_type, dam, m_name);
        if (!p_ptr->no_stun && !res_save_default(RES_SOUND) && !CHECK_MULTISHADOW())
        {
            int k = (randint1((dam > 40) ? 35 : (dam * 3 / 4 + 5)));
            plr_tim_add(T_STUN, k);
        }

        if (!touch) inven_damage(set_acid_destroy, 3, RES_FIRE);
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
    case GF_NETHER: {
        int unlife;
        if (touch) msg_print("You are <color:D>drained</color>!");
        else if (fuzzy) msg_print("You are hit by nether forces!");
        dam = res_calc_dam(RES_NETHER, dam);
        unlife = dam*15/100;
        if (unlife)
        {
            gf_affect_p(who, GF_UNLIFE, unlife, flags);
            dam -= unlife;
        }
        result = take_hit(damage_type, dam, m_name);
        update_smart_learn(who, RES_NETHER);
        break; }
    case GF_WATER:
    case GF_WATER2:
        if (fuzzy) msg_print("You are hit by something wet!");
        if (prace_is_(RACE_WATER_ELF) || elemental_is_(ELEMENTAL_WATER))
        {
            dam /= 2;
        }
        else if(!CHECK_MULTISHADOW())
        {
            if (!p_ptr->no_stun && !res_save_default(RES_SOUND))
                plr_tim_add(T_STUN, randint1(40));
            if (!res_save_default(RES_CONF))
                plr_tim_add(T_CONFUSED, randint1(5) + 5);
            inven_damage(set_cold_destroy, 3, RES_SOUND);
        }
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_CHAOS:
        if (touch) msg_print("You are <color:v>unmade</color>!");
        else if (fuzzy) msg_print("You are hit by a wave of anarchy!");
        dam = res_calc_dam(RES_CHAOS, dam);
        if (!CHECK_MULTISHADOW())
        {
            if (!res_save_default(RES_CONF))
                plr_tim_add(T_CONFUSED, randint0(20) + 10);
            if (!res_save_default(RES_CHAOS))
            {
                int count = mut_count(mut_unlocked_pred);
                if (prace_is_(RACE_BEASTMAN)) count = 0;
                if (one_in_(3 + count*count))
                {
                    msg_print("Your body is twisted by chaos!");
                    mut_gain_random(NULL);
                }
                if (p_ptr->pclass == CLASS_WILD_TALENT && one_in_(7))
                    wild_talent_scramble();

                if (!mut_present(MUT_WEIRD_MIND))
                    plr_tim_add(T_HALLUCINATE, randint1(10));
            }
            if (!res_save_default(RES_NETHER) && !res_save_default(RES_CHAOS))
                drain_exp(5000 + (p_ptr->exp / 100), 500 + (p_ptr->exp / 1000), 75);

            if (!touch)
            {
                inven_damage(set_elec_destroy, 2, RES_CHAOS);
                inven_damage(set_fire_destroy, 2, RES_CHAOS);
            }
        }
        result = take_hit(damage_type, dam, m_name);
        update_smart_learn(who, RES_CHAOS);
        break;
    case GF_ROCK:
        if (fuzzy) msg_print("You are hit by something solid!");
        if (one_in_(2))
        {
            if (!p_ptr->no_cut && !res_save_default(RES_SHARDS) && !CHECK_MULTISHADOW())
                plr_tim_add(T_CUT, dam/2);
            inven_damage(set_cold_destroy, 2, RES_SHARDS);
        }
        else
        {
            if (!p_ptr->no_stun && !res_save_default(RES_SOUND) && !CHECK_MULTISHADOW())
            {
                int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
                plr_tim_add(T_STUN, k);
            }
            inven_damage(set_cold_destroy, 2, RES_SOUND);
        }
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_SHARDS:
        if (touch) msg_print("You are <color:U>shredded</color>!");
        else if (fuzzy) msg_print("You are hit by something sharp!");
        dam = res_calc_dam(RES_SHARDS, dam);
        if (!p_ptr->no_cut && !res_save_default(RES_SHARDS) && !CHECK_MULTISHADOW())
            plr_tim_add(T_CUT, dam);
        if (!touch) inven_damage(set_cold_destroy, 2, RES_SHARDS);
        result = take_hit(damage_type, dam, m_name);
        update_smart_learn(who, RES_SHARDS);
        break;
    case GF_SOUND:
        if (!touch && fuzzy) msg_print("You are hit by a loud noise!");
        /*if (touch) ... */
        dam = res_calc_dam(RES_SOUND, dam);
        if (!p_ptr->no_stun && !res_save_default(RES_SOUND) && !CHECK_MULTISHADOW())
        {
            int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
            plr_tim_add(T_STUN, k);
        }
        if (!touch) inven_damage(set_cold_destroy, 2, RES_SOUND);
        result = take_hit(damage_type, dam, m_name);
        update_smart_learn(who, RES_SOUND);
        break;
    case GF_CONFUSION:
        if (!touch && fuzzy) msg_print("You are hit by something puzzling!");
        /*if (touch) ... */
        dam = res_calc_dam(RES_CONF, dam);
        if (!res_save_default(RES_CONF) && !CHECK_MULTISHADOW())
            plr_tim_add(T_CONFUSED, randint1(20) + 10);
        result = take_hit(damage_type, dam, m_name);
        update_smart_learn(who, RES_CONF);
        break;
    case GF_DISENCHANT:
        if (touch) msg_print("You are <color:v>disenchanted</color>!");
        else if (fuzzy) msg_print("You are hit by something static!");
        dam = res_calc_dam(RES_DISEN, dam);
        if (!(flags & GF_AFFECT_SPELL) && !one_in_(5) && !CHECK_MULTISHADOW())
        {
            if (!res_save_default(RES_DISEN) || one_in_(5))
                plr_tim_disenchant();
        }
        else if (!res_save(RES_DISEN, 31) && !CHECK_MULTISHADOW())
            apply_disenchant(0);
        result = take_hit(damage_type, dam, m_name);
        update_smart_learn(who, RES_DISEN);
        break;
    case GF_NEXUS:
        if (touch) msg_print("You are <color:v>scrambled</color>!");
        else if (fuzzy) msg_print("You are hit by something strange!");
        dam = res_calc_dam(RES_NEXUS, dam);
        if (!res_save_default(RES_NEXUS) && !CHECK_MULTISHADOW())
            apply_nexus(m_ptr);
        result = take_hit(damage_type, dam, m_name);
        update_smart_learn(who, RES_NEXUS);
        break;
    case GF_FORCE:
        if (fuzzy) msg_print("You are hit by kinetic force!");
        if (!p_ptr->no_stun && !res_save_default(RES_SOUND) && !CHECK_MULTISHADOW())
            plr_tim_add(T_STUN, randint1(20));
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_ROCKET:
        if (fuzzy) msg_print("There is an explosion!");
        dam = res_calc_dam(RES_SHARDS, dam);
        if (!p_ptr->no_stun && !res_save_default(RES_SOUND) && !CHECK_MULTISHADOW())
            plr_tim_add(T_STUN, randint1(20));
        if (!p_ptr->no_cut && !res_save_default(RES_SHARDS) && !CHECK_MULTISHADOW())
            plr_tim_add(T_CUT, (dam / 2));
        inven_damage(set_cold_destroy, 3, RES_SHARDS);
        result = take_hit(damage_type, dam, m_name);
        update_smart_learn(who, RES_SHARDS);
        break;
    case GF_INERT:
        if (!touch && fuzzy) msg_print("You are hit by something slow!");
        /*if (touch) ... */
        if (!CHECK_MULTISHADOW() && !free_act_save_p(MAX(rlev, dam)))
            plr_tim_add(T_SLOW, randint0(4) + 4);
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_LITE:
        if (touch) msg_print("You are <color:y>dazzled</color>!");
        else if (fuzzy) msg_print("You are hit by something!");
        dam = res_calc_dam(RES_LITE, dam);
        if (!plr_tim_find(T_BLIND) && !res_save_default(RES_LITE) && !res_save_default(RES_BLIND) && !CHECK_MULTISHADOW())
            plr_tim_add(T_BLIND, randint1(5) + 2);

        result = take_hit(damage_type, dam, m_name);
        if (prace_is_(RACE_MON_VAMPIRE))
            vampire_take_light_damage(dam);

        if (plr_tim_find(T_WRAITH) && !CHECK_MULTISHADOW() && !touch)
        {
            msg_print("The light forces you out of your incorporeal shadow form.");
            plr_tim_remove(T_WRAITH);
        }
        update_smart_learn(who, RES_LITE);
        break;
    case GF_DARK:
        if (touch) msg_print("You are <color:D>benighted</color>!");
        else if (fuzzy) msg_print("You are hit by something!");
        dam = res_calc_dam(RES_DARK, dam);
        if (!plr_tim_find(T_BLIND) && !res_save_default(RES_DARK) && !res_save_default(RES_BLIND) && !CHECK_MULTISHADOW())
            plr_tim_add(T_BLIND, randint1(5) + 2);
        result = take_hit(damage_type, dam, m_name);
        if (prace_is_(RACE_MON_VAMPIRE))
            vampire_take_dark_damage(dam);
        update_smart_learn(who, RES_DARK);
        break;
    case GF_ELDRITCH:
        if (touch && m_ptr)
            sanity_blast(m_ptr, FALSE);
        break;
    case GF_STUN:
        if (!p_ptr->no_stun) plr_tim_add(T_STUN, dam);
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
        if (touch) msg_print("You are <color:B>chronosmashed</color>!");
        else if (fuzzy) msg_print("You are hit by a blast from the past!");
        if (!res_save_default(RES_TIME) && !CHECK_MULTISHADOW())
        {
            int k = A_STR;
            cptr act = "";
            switch (randint1(10))
            {
            case 1: case 2: case 3: case 4: case 5:
                if (p_ptr->prace == RACE_ANDROID) break;
                msg_print("You feel life has clocked back.");
                lose_exp(100 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
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
                p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
                if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
                p_ptr->update |= (PU_BONUS);
                break;
            case 10:
                msg_print("You're not as powerful as you used to be...");
                for (k = 0; k < 6; k++)
                {
                    p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 7) / 8;
                    if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
                }
                p_ptr->update |= (PU_BONUS);
                break;
            }
        }
        dam = res_calc_dam(RES_TIME, dam);
        result = take_hit(damage_type, dam, m_name);
        update_smart_learn(who, RES_TIME);
        break;
    case GF_STORM:
        msg_print("You are hit by gale force winds!");
        if (!CHECK_MULTISHADOW())
        {
            teleport_player(5, TELEPORT_PASSIVE);
            if (!p_ptr->levitation)
                plr_tim_add(T_SLOW, randint0(4) + 4);
            if (!(p_ptr->no_stun || res_save_default(RES_SOUND) || p_ptr->levitation))
            {
                int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
                plr_tim_add(T_STUN, k);
            }
        }
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_GRAVITY:
        if (!touch && fuzzy) msg_print("You are hit by something heavy!");
        /*if (touch) ... */
        msg_print("<color:U>Gravity warps around you.</color>");
        if (!CHECK_MULTISHADOW())
        {
            teleport_player(5, TELEPORT_PASSIVE);
            if (!p_ptr->levitation)
                plr_tim_add(T_SLOW, randint0(4) + 4);
            if (!(p_ptr->no_stun || res_save_default(RES_SOUND) || p_ptr->levitation))
            {
                int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
                plr_tim_add(T_STUN, k);
            }
        }
        if (p_ptr->levitation)
        {
            dam = (dam * 2) / 3;
        }
        inven_damage(set_cold_destroy, 2, RES_SOUND);
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_DISINTEGRATE:
        if (fuzzy) msg_print("You are hit by pure energy!");
        result = take_hit(damage_type, dam, m_name);
        break;
    case GF_OLD_HEAL:
        if (fuzzy) msg_print("You are hit by something invigorating!");

        hp_player(dam);
        break;
    case GF_OLD_SPEED:
        if (fuzzy) msg_print("You are hit by something!");

        plr_tim_add(T_FAST, randint1(5));
        break;
    case GF_OLD_SLOW:
        if (fuzzy) msg_print("You are hit by something slow!");
        if (!free_act_save_p(MAX(rlev, dam)))
            plr_tim_add(T_SLOW, randint0(4) + 4);
        break;
    case GF_OLD_SLEEP:
        if (!free_act_save_p(rlev))
        {
            if (fuzzy) msg_print("You fall asleep!");
            plr_tim_add(T_PARALYZED, dam);
        }
        break;
    case GF_BLIND:
        if (res_save_default(RES_BLIND) || (!touch && _plr_save(who, 0)))
            msg_print("You resist the effects!");
        else
            plr_tim_add(T_BLIND, 12 + randint0(4));
        update_smart_learn(who, RES_BLIND);
        break;
    case GF_OLD_CONF:
        if (res_save_default(RES_CONF) || _plr_save(who, 0))
            msg_print("You disbelieve the feeble spell.");
        else
            plr_tim_add(T_CONFUSED, randint0(4) + 4);
        update_smart_learn(who, RES_CONF);
        break;
    case GF_TURN_ALL:
        if (fuzzy) msg_print("Your will is shaken!");
        fear_scare_p(m_ptr);
        update_smart_learn(who, RES_FEAR);
        break;
    case GF_PARALYSIS:
        if (free_act_save_p(rlev) || (!touch && _plr_save(who, dam)))
            msg_print("You resist the effects!");
        else
            plr_tim_add(T_PARALYZED, randint1(3));
        update_smart_learn(who, SM_FREE_ACTION);
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
        inven_damage(set_fire_destroy, 2, RES_FIRE);
        inven_damage(set_cold_destroy, 2, RES_SHARDS);
        break;
    case GF_ICE:
        if (touch) msg_print("You are <color:W>frozen</color>!");
        else if (fuzzy) msg_print("You are hit by something sharp and cold!");
        result = gf_affect_p(who, GF_COLD, dam, 0);
        if (!CHECK_MULTISHADOW())
        {
            if (!p_ptr->no_cut && !res_save_default(RES_SHARDS))
                plr_tim_add(T_CUT, (touch ? damroll(3, 5) : damroll(5, 8)));
            if (!p_ptr->no_stun && !res_save_default(RES_SOUND))
                plr_tim_add(T_STUN, (touch ? randint1(7) : randint1(15)));
            inven_damage(set_cold_destroy, 3, RES_COLD);
        }
        update_smart_learn(who, RES_COLD);
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
                && p_ptr->psubrace == DEMIGOD_HERA
                && randint1(100) > r_ptr->level - 2*(p_ptr->stat_ind[A_WIS] + 3))
        {
            if (!touch) msg_print("You keep your wits about you!");
        }
        else if (p_ptr->csp)
        {
            if (who > 0) msg_format("%^s draws psychic energy from you!", m_name);
            else msg_print("Your psychic energy is drawn!");
            if (dam >= p_ptr->csp)
            {
                dam = p_ptr->csp;
                p_ptr->csp = 0;
                p_ptr->csp_frac = 0;
            }
            else
                p_ptr->csp -= dam;

            p_ptr->redraw |= (PR_MANA);
            p_ptr->window |= (PW_SPELL);

            if (who > 0 && m_ptr->hp < m_ptr->maxhp)
            {
                m_ptr->hp += (6 * dam);
                if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;
                check_mon_health_redraw(who);
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

            if (!res_save_default(RES_CONF))
                plr_tim_add(T_CONFUSED, randint0(4) + 4);

            if (!res_save_default(RES_CHAOS) && !mut_present(MUT_WEIRD_MIND) && one_in_(3))
                plr_tim_add(T_HALLUCINATE, randint0(25) + 15);

            p_ptr->csp -= touch ? 10 : 50;
            if (p_ptr->csp < 0)
            {
                p_ptr->csp = 0;
                p_ptr->csp_frac = 0;
            }
            p_ptr->redraw |= PR_MANA;
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
              && p_ptr->psubrace == DEMIGOD_HERA
              && randint1(100) > r_ptr->level - 2*(p_ptr->stat_ind[A_WIS] + 3))
            {
                if (!touch) msg_print("You keep your wits about you!");
            }
            else if (!CHECK_MULTISHADOW())
            {
                msg_print("Your mind is blasted by psionic energy.");
                if (touch)
                    p_ptr->csp -= 10;
                else
                    p_ptr->csp -= 100;
                if (p_ptr->csp < 0)
                {
                    p_ptr->csp = 0;
                    p_ptr->csp_frac = 0;
                }
                p_ptr->redraw |= PR_MANA;
            }
            result = take_hit(damage_type, dam, m_name);
            if (!CHECK_MULTISHADOW())
            {
                if (!res_save_default(RES_BLIND))
                    plr_tim_add(T_BLIND, 8 + randint0(8));
                if (!res_save_default(RES_CONF))
                    plr_tim_add(T_CONFUSED, randint0(4) + 4);
                if (!free_act_save_p(rlev))
                    plr_tim_add(T_PARALYZED, randint1(4));

                plr_tim_add(T_SLOW, randint0(4) + 4);
                if (!p_ptr->no_stun) plr_tim_add(T_STUN, MIN(50, dam/6 + randint1(dam/6)));

                while (!_plr_save(who, 0))
                    do_dec_stat(A_INT);
                while (!_plr_save(who, 0))
                    do_dec_stat(A_WIS);

                if (!res_save_default(RES_CHAOS) && !mut_present(MUT_WEIRD_MIND))
                    plr_tim_add(T_HALLUCINATE, randint0(25) + 15);
            }
        }
        break;
    case GF_TELEKINESIS:
        if (!CHECK_MULTISHADOW())
        {
            if (one_in_(4))
                teleport_player(5, TELEPORT_PASSIVE);
            if (!p_ptr->no_stun && !_plr_save(who, dam/5))
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
        if (_plr_save(who, dam/5) && m_ptr->r_idx != MON_KENSHIROU && !CHECK_MULTISHADOW())
        {
            if (!touch)
            {
                msg_print("You resist the effects!");
            }
        }
        else
        {
            result = take_hit(damage_type, dam, m_name);
            if (!p_ptr->no_cut && !CHECK_MULTISHADOW()) plr_tim_add(T_CUT, damroll(10, 10));
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

            if (p_ptr->chp < 1) p_ptr->chp = 1; /* Paranoia */
        }
        break;
    case GF_OLD_POLY:
        if ( prace_is_(RACE_ANDROID)
          || p_ptr->pclass == CLASS_MONSTER
          || p_ptr->prace == RACE_DOPPELGANGER
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
                if (p_ptr->prace != RACE_SNOTLING)
                {
                    which = RACE_SNOTLING;
                    break;
                }
            case 2:
                if (p_ptr->prace != RACE_YEEK)
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
        mon_attack(m_ptr, p_ptr->pos);
        break;
    }
    return result;
}
static bool _is_mental_attack(int type)
{
    switch (type)
    {
    case GF_PSI:
    case GF_PSI_BRAIN_SMASH:
    case GF_BRAIN_SMASH:
    case GF_PSI_STORM:
    case GF_ELDRITCH_STUN:
    case GF_DOMINATION:
    case GF_SUBJUGATION:
    case GF_STUN: /* XXX */
    case GF_LICH_WORD: /* XXX */
        return TRUE;
    }
    return FALSE;
}
static bool _mon_save_aux(mon_race_ptr race, int power)
{
    if (mon_save_aux(race->id, power)) return TRUE;
    if ((race->flags1 & RF1_UNIQUE) && mon_save_aux(race->id, power)) return TRUE;
    return FALSE;
}
static bool _mon_save_p(mon_race_ptr race, int stat)
{
    if (mon_save_p(race->id, stat)) return TRUE;
    if ((race->flags1 & RF1_UNIQUE) && mon_save_p(race->id, stat)) return TRUE;
    return FALSE;
}
#define _BABBLE_HACK() \
            if (race->flagsr & RFR_RES_ALL) \
            { \
                note = " is immune."; \
                dam = 0; \
                mon_lore_r(mon, RFR_RES_ALL); \
                break; \
            }

bool gf_affect_m(int who, mon_ptr mon, int type, int dam, int flags)
{
    int tmp;

    point_t       where = mon->pos;
    monster_type *caster_ptr = (who > 0) ? dun_mon(cave, who) : NULL;
    int           caster_lev = (who > 0) ? mon_race(caster_ptr)->level : spell_power(p_ptr->lev * 2);
    bool          touch = BOOL(flags & (GF_AFFECT_AURA | GF_AFFECT_ATTACK));
    bool          quiet = BOOL(flags & GF_AFFECT_QUIET);

    monster_race *race = mon_race(mon);

    char killer[80];

    /* Is the monster "seen"? Note: mon_show_msg() requires
     * the monster sqaure to be lit if ignore_unview is on. This
     * means that if the player is lobbing fireballs at a telepathically
     * seen monster, they would miss the resistance message. So, if
     * the player is doing this, then we just use ml instead. If the
     * player is not doing this affect, then we better respect ignore_unview
     * or we'll turn the message spammer back on full blast! */
    bool seen = mon->ml;
    bool seen_msg = (who == GF_WHO_PLAYER) ? mon->ml : mon_show_msg(mon);

    bool slept = BOOL(mon_tim_find(mon, MT_SLEEP));

    /* Were the effects "obvious" (if seen)? */
    bool obvious = FALSE;

    /* Can the player know about this effect? */
    bool known = mon->cdis <= MAX_SIGHT;

    /* Were the effects "irrelevant"? */
    bool skipped = FALSE;

    /* Gets the monster angry at the source of the effect? */
    bool get_angry = FALSE;

    /* Polymorph setting (power of polymorph) */
    int do_poly = 0;

    /* Teleport setting (max distance) */
    int do_dist = 0;

    /* Confusion setting (amount to confuse) */
    int do_conf = 0;

    /* Stunning setting (amount to stun) */
    int do_stun = 0;

    /* Sleep amount (amount to sleep) */
    int do_sleep = 0;
    int do_paralyzed = 0;

    /* Fear amount (amount to fear) */
    int do_fear = 0;

    /* Time amount (amount to time) */
    int do_time = 0;

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
    if (who > 0 && is_pet(dun_mon(cave, who)))
        who_is_pet = TRUE;

    /* Never affect projector */
    if (mon->id == who) return FALSE;

    if (mon->id == p_ptr->riding && who == PROJECT_WHO_PLAYER)
    {
        switch (type)
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

    /* Big hack here ... Jump Spiders are immune to the jumps of their kin
     * (They come in packs, and it would be silly for them to destroy each other) */
    if (mon_spell_current() && race->spells)
    {
        mon_spell_ptr spell = mon_spell_current();
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


    if (p_ptr->riding && mon->id == p_ptr->riding) disturb(1, 0);

    /* Analyze the damage type */
    switch (type)
    {
    case GF_MISSILE:
    case GF_BLOOD:
    case GF_ELDRITCH:
    case GF_ELDRITCH_DRAIN:  /* Lazy ... I'll give back hp later */
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        break;
    case GF_MANA_CLASH:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (!race->spells || !race->spells->freq)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
        }
        else /* 900 max dam coming in ... ~600 max dam going out */
            dam = dam * MIN(66, race->spells->freq) / 100;
        break;
    case GF_ACID:
        if (touch && seen_msg) msg_format("%^s is <color:G>dissolved</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_IM_ACID)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_IM_ACID);
        }
        else if (race->flagsr & RFR_RES_ACID)
        {
            if (!quiet) note = " resists.";
            dam /= 3;
            mon_lore_r(mon, RFR_RES_ACID);
        }
        break;
    case GF_ELEC:
        if (touch && seen_msg) msg_format("%^s is <color:b>shocked</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_IM_ELEC)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_IM_ELEC);
        }
        else if (race->flagsr & RFR_RES_ELEC)
        {
            if (!quiet) note = " resists.";
            dam /= 3;
            mon_lore_r(mon, RFR_RES_ELEC);
        }
        break;
    case GF_FIRE:
        if (touch && seen_msg) msg_format("%^s is <color:r>burned</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_IM_FIRE)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_IM_FIRE);
        }
        else if (race->flagsr & RFR_RES_FIRE)
        {
            if (!quiet) note = " resists.";
            dam /= 3;
            mon_lore_r(mon, RFR_RES_FIRE);
        }
        else if (race->flags3 & RF3_HURT_FIRE)
        {
            note = " is hit hard.";
            dam *= 2;
            mon_lore_3(mon, RF3_HURT_FIRE);
        }
        break;
    case GF_COLD:
        if (touch && seen_msg) msg_format("%^s is <color:W>frozen</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_IM_COLD)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_IM_COLD);
        }
        else if (race->flagsr & RFR_RES_COLD)
        {
            if (!quiet) note = " resists.";
            dam /= 3;
            mon_lore_r(mon, RFR_RES_COLD);
        }
        else if (race->flags3 & RF3_HURT_COLD)
        {
            note = " is hit hard.";
            dam *= 2;
            mon_lore_3(mon, RF3_HURT_COLD);
        }
        break;
    case GF_POIS:
        if (touch && seen_msg && !(race->flagsr & RFR_IM_POIS))
            msg_format("%^s is <color:G>poisoned</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_IM_POIS)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_IM_POIS);
        }
        else if (race->flagsr & RFR_RES_POIS)
        {
            if (!quiet) note = " resists.";
            dam /= 3;
            mon_lore_r(mon, RFR_RES_POIS);
        }
        break;
    case GF_NUKE:
        if (touch && seen_msg) msg_format("%^s is <color:G>irradiated</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_IM_POIS)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_IM_POIS);
        }
        else if (race->flagsr & RFR_RES_POIS)
        {
            if (!quiet) note = " resists.";
            dam /= 2;
            mon_lore_r(mon, RFR_RES_POIS);
        }
        else if (one_in_(3)) do_poly = dam;
        break;
    case GF_HELL_FIRE:
        if (touch && seen_msg) msg_format("%^s is <color:D>%s</color>!", m_name, (race->flags3 & RF3_GOOD) ? "*burned*" : "burned");
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flags3 & RF3_GOOD)
        {
            dam *= 2;
            if (!touch) note = " is hit hard.";
            mon_lore_3(mon, RF3_GOOD);
        }
        break;
    case GF_HOLY_FIRE:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flags3 & RF3_GOOD)
        {
            dam = 0;
            if (!quiet) note = " is immune.";
            mon_lore_3(mon, RF3_GOOD);
        }
        else if (race->flags3 & RF3_EVIL)
        {
            if (touch && seen_msg) msg_format("%^s is <color:D>*burned*</color>!", m_name);
            dam *= 2;
            if (!touch) note = " is hit hard.";
            mon_lore_3(mon, RF3_EVIL);
        }
        else
        {
            if (touch && seen_msg) msg_format("%^s is <color:D>burned</color>!", m_name);
            dam *= 3; dam /= randint1(6) + 6;
        }
        break;
    case GF_ARROW:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        break;
    case GF_PLASMA:
        if (touch && seen_msg) msg_format("%^s is <color:R>burned</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_PLAS)
        {
            if (!quiet) note = " resists.";
            dam *= 3; dam /= randint1(6) + 6;
            mon_lore_r(mon, RFR_RES_PLAS);
        }
        else if (who == GF_WHO_PLAYER && mon_save_stun(race->level, dam))
        {
            if (!quiet) note = " resists stunning.";
        }
        else do_stun = mon_stun_amount(dam);
        break;
    case GF_UNLIFE:
        if (!monster_living(race))
        {
            mon_lore_3(mon, RF3_DEMON | RF3_UNDEAD | RF3_NONLIVING);
            if (seen_msg && !quiet) msg_format("%^s is unaffected.", m_name);
        }
        else if (who == GF_WHO_PLAYER && _mon_save_aux(race, p_ptr->lev + dam/2))
        {
            if (seen_msg && !quiet) msg_format("%^s resists.", m_name);
        }
        else
        {
            /* XXX Diminishing returns on player GF_UNLIFE attacks. Consider
             * polymorphing the monster into an undead form instead if life hits 0 */
            if (who == GF_WHO_PLAYER)
                dam = dam*mon->mpower/1000;
            if (mon->mpower - dam < 0)
                dam = mon->mpower;
            mon->mpower -= dam;
            if (seen_msg)
            {
                if (who == GF_WHO_PLAYER)
                    msg_format("<color:D>You drain life from %s.</color>", m_name_object);
                else
                    msg_format("<color:D>%^s grows less powerful.</color>", m_name);
            }
            if (who == GF_WHO_PLAYER)
            {
                if (get_race()->flags & RACE_IS_UNDEAD)
                    plr_gain_life(dam);
                else if (!(flags & GF_AFFECT_SPELL))
                    plr_restore_life(dam);
            }
            return TRUE; /* using note causes messages out of order */
        }
        return FALSE;
    case GF_LICH_DRAIN:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (!monster_living(race))
        {
            mon_lore_3(mon, RF3_DEMON | RF3_UNDEAD | RF3_NONLIVING);
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
        _BABBLE_HACK()
        if (!monster_living(race))
        {
            mon_lore_3(mon, RF3_DEMON | RF3_UNDEAD | RF3_NONLIVING);
            if (seen_msg && !quiet)
                msg_format("%^s is unaffected!", m_name);
        }
        else if (genocide_aux(mon, dam, who==GF_WHO_PLAYER, (race->level + 1) / 2, "Genocide One"))
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
        _BABBLE_HACK()
        if (is_pet(mon))
            return FALSE;
        if (race->flags3 & RF3_UNDEAD)
        {
            if (gf_affect_m(who, mon, GF_CONTROL_UNDEAD, dam, flags))
                return TRUE;
            else if (gf_affect_m(who, mon, GF_AWAY_UNDEAD, dam, flags))
                return TRUE;
        }
        if (monster_living(race))
        {
            gf_affect_m(who, mon, GF_ELDRITCH_HOWL, dam, flags);
            if (!mon_save_aux(race->level, dam))
                do_conf = 5 + randint1(11);
            dam *= 2;
        }
        if (!mon_save_stun(race->level, dam))
            do_stun = mon_stun_amount(dam);
        break;
    case GF_NETHER:
        if (touch && seen_msg) msg_format("%^s is <color:D>drained</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        /* XXX Previously, EVIL monsters had 50% nether resistance, a mechanic
         * I've never liked. I've changed nether to work against living monsters,
         * so now non-living monsters get a small automatic resistance. Player
         * damage amounts have been rescaled. If we want alignment effects, let's
         * do this in a small way, scaling by no more than +/- 20%. Also, some
         * monsters are more evil than others, so make alignment a number in r_info.
         * XXX Statistics reveal that a full 2/3 of all killed monsters are EVIL. */
        if (race->flagsr & RFR_RES_NETH)
        {
            if (race->flags3 & RF3_UNDEAD)  /* Undead are immune, of course! */
            {
                if (!quiet) note = " is immune.";
                dam = 0;
                mon_lore_3(mon, RF3_UNDEAD);
            }
            else if (!monster_living(race)) /* Nonliving monsters get strong resistance */
            {
                if (!quiet) note = " resists.";
                dam = dam*rand_range(25, 50)/100;
            }
            else
            {
                if (!quiet) note = " resists.";
                dam = dam/2;
            }
            mon_lore_r(mon, RFR_RES_NETH);
        }
        else if (!monster_living(race)) /* Without RES_NETH, nonliving monsters get a weak resistance */
        {
            if (!quiet) note = " resists somewhat.";
            dam = dam*3/4;
            mon_lore_3(mon, RF3_DEMON | RF3_UNDEAD | RF3_NONLIVING);
        }
        break;
    case GF_WATER:
    case GF_WATER2:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_WATE)
        {
            if ((mon->r_idx == MON_WATER_ELEM) || (mon->r_idx == MON_UNMAKER))
            {
                if (!quiet) note = " is immune.";
                dam = 0;
            }
            else
            {
                if (!quiet) note = " resists.";
                dam *= 3; dam /= randint1(6) + 6;
            }
            mon_lore_r(mon, RFR_RES_WATE);
        }
        else if (who == GF_WHO_PLAYER && mon_save_stun(race->level, dam))
        {
            if (!quiet) note = " resists stunning.";
        }
        else do_stun = mon_stun_amount(dam);
        break;
    case GF_CHAOS:
        if (touch && seen_msg) msg_format("%^s is <color:v>unmade</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_CHAO)
        {
            if (!quiet) note = " resists.";
            dam *= 3; dam /= randint1(6) + 6;
            mon_lore_r(mon, RFR_RES_CHAO);
        }
        else if ((race->flags3 & RF3_DEMON) && one_in_(3))
        {
            if (!quiet) note = " resists somewhat.";
            dam *= 3; dam /= randint1(6) + 6;
            mon_lore_3(mon, RF3_DEMON);
        }
        else
        {
            if (p_ptr->current_r_idx == MON_CHAOS_VORTEX)
                do_poly = dam / 5;
            else
                do_poly = dam;

            do_conf = 5 + randint1(11);
        }
        break;
    case GF_SHARDS:
        if (touch && seen_msg) msg_format("%^s is <color:U>shredded</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_SHAR)
        {
            if (!quiet) note = " resists.";
            dam *= 3; dam /= randint1(6) + 6;
            mon_lore_r(mon, RFR_RES_SHAR);
        }
        break;
    case GF_ROCKET:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_SHAR)
        {
            if (!quiet) note = " resists somewhat.";
            dam /= 2;
            mon_lore_r(mon, RFR_RES_SHAR);
        }
        break;
    case GF_ELDRITCH_STUN:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flags3 & RF3_NO_STUN)
            mon_lore_3(mon, RF3_NO_STUN);
        else if (_mon_save_p(race, A_CHR))
        {
            if (!quiet) note = " resists stunning.";
        }
        else
            do_stun = mon_stun_amount(dam);
        break;
    case GF_ROCK:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (!(race->flagsr & RFR_RES_SOUN))
        {
            if (who == GF_WHO_PLAYER  && mon_save_stun(race->level, dam))
            {
                if (!quiet) note = " resists stunning.";
            }
            else
                do_stun = mon_stun_amount(dam);
        }
        break;
    case GF_SOUND:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_SOUN)
        {
            if (!quiet) note = " resists.";
            dam *= 2; dam /= randint1(6) + 6;
            mon_lore_r(mon, RFR_RES_SOUN);
        }
        else if (who == GF_WHO_PLAYER && mon_save_stun(race->level, dam))
        {
            if (!quiet) note = " resists stunning.";
        }
        else
            do_stun = mon_stun_amount(dam);
        break;
    case GF_ELDRITCH_DISPEL:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        dispel_monster_status(mon->id);

        /* More Powerful vs. Evil */
        if ((race->flags3 & RF3_EVIL) && one_in_(5))
        {
            msg_print("You blast the forces of evil with great power!");
            dam = dam * 2;

            /* Attempt a Powerful Slow */
            if (mon_save_p(mon->r_idx, A_CHR))
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
        _BABBLE_HACK()
        dam = spell_power(MIN(mon->hp*8/100 + dam, 400));
        if (p_ptr->lev >= 50)
            dam += 50;
        note = " is cursed by ancient pharaohs of long ago!";
        break;
    case GF_ELDRITCH_CONFUSE:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        /* 593 monsters have NO_CONF ... it is more widely resisted than fire ...
           Eldritch confusion will bypass this check, but that is hacked below */
        if ((race->flags3 & RF3_NO_CONF) && mon_save_p(mon->r_idx, A_NONE))
        {   /* XXX Was: race->level + randint1(100) > p_ptr->lev*2 + (p_ptr->stat_ind[A_CHR] + 3) - 10
               Note the A_NONE on mon_save_p for an easy saving throw. */
            if (!quiet) note = " resists confusion.";
        }
        else
        {
            /* Recovery is randint1(r_info[mon->r_idx].level / 20 + 1) */
            do_conf = 3 + randint0(5);
        }
        break;
    case GF_CONFUSION:
        if (touch && seen_msg && dam) msg_format("%^s is <color:U>baffled</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flags3 & RF3_NO_CONF)
        {
            if (!quiet) note = " resists.";
            dam *= 3; dam /= randint1(6) + 6;
            mon_lore_3(mon, RF3_NO_CONF);
        }
        else do_conf = 10 + randint1(15);
        break;
    case GF_DISENCHANT:
        if (touch && seen_msg) msg_format("%^s is <color:v>disenchanted</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_DISE)
        {
            if (!quiet) note = " resists.";
            dam *= 3; dam /= randint1(6) + 6;
            mon_lore_r(mon, RFR_RES_DISE);
        }
        else if (who == GF_WHO_PLAYER && !mon_save_disenchant(race->id, dam, flags))
        {
            mon_tim_disenchant(mon);
        }
        break;
    case GF_NEXUS:
        if (touch && seen_msg) msg_format("%^s is <color:v>scrambled</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_NEXU)
        {
            if (!quiet) note = " resists.";
            dam *= 3; dam /= randint1(6) + 6;
            mon_lore_r(mon, RFR_RES_NEXU);
        }
        break;
    case GF_FORCE:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_WALL)
        {
            if (!quiet) note = " resists.";
            dam *= 3; dam /= randint1(6) + 6;
            mon_lore_r(mon, RFR_RES_WALL);
        }
        else
            do_stun = mon_stun_amount(dam);
        break;
    case GF_INERT:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_INER)
        {
            if (!quiet) note = " resists.";

            dam *= 3; dam /= randint1(6) + 6;
            mon_lore_r(mon, RFR_RES_INER);
        }
        else
        {
            /* Powerful monsters can resist */
            if ( mon_save_slow(race->level, dam)
              || ((race->flags1 & RF1_UNIQUE) && mon_save_slow(race->level + 15, dam)) )
            {
                obvious = FALSE;
            }
            /* Normal monsters slow down */
            else mon_tim_add(mon, T_SLOW, 50);
        }
        break;
    case GF_TIME:
        if (touch && seen_msg) msg_format("%^s is <color:B>chronosmashed</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_TIME)
        {
            if (!quiet) note = " resists.";
            dam *= 3; dam /= randint1(6) + 6;
            mon_lore_r(mon, RFR_RES_TIME);
        }
        else if (who == GF_WHO_PLAYER)
        {
            bool unique = BOOL(race->flags1 & RF1_UNIQUE);

            if (mon_save_time(race->id, dam, flags))
            {
                if (p_ptr->wizard)
                    note = " resists the ravages of time.";
            }
            else
            {
                int which = randint1(100);
                if (which <= 15)
                {
                    if (unique)
                    {
                        if (p_ptr->wizard)
                            note = " cannot be slowed.";
                    }
                    else mon_tim_add(mon, T_SLOW, 50);
                }
                else if (which <= 20)
                {
                    if (devolve_monster(mon->id, p_ptr->wizard))
                        return TRUE;
                }
                else if (which <= 25)
                {
                    if (evolve_monster(mon->id, p_ptr->wizard))
                        return TRUE;
                }
                else if (which <= 40)
                {
                    if (!unique)
                    {
                        note = " is suspended!";
                        do_paralyzed = 5;
                    }
                    else
                    {
                        if (p_ptr->wizard)
                            note = " cannot be suspended.";
                    }
                }
                else if (which <= 50)
                {
                    mon->ac_adj -= randint1(10);
                    note = " is more exposed!";
                }
                else if (which <= 60)
                {
                    do_time = (dam + 1) / 2;
                }
                else if (which <= 70)
                {
                    mon->mspeed -= randint1(2);
                    note = " is permanently slowed!";
                }
                else if (which <= 90)
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
        {
            do_time = (dam + 1) / 2;
        }
        break;
    case GF_STORM: /* TODO */
    case GF_GRAVITY: {
        bool resist_tele = FALSE;

        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_TELE)
        {
            if (race->flags1 & (RF1_UNIQUE))
            {
                mon_lore_r(mon, RFR_RES_TELE);
                if (!quiet) note = " is unaffected!";
                resist_tele = TRUE;
            }
            else if (race->level > randint1(100))
            {
                mon_lore_r(mon, RFR_RES_TELE);
                if (!quiet) note = " resists!";
                resist_tele = TRUE;
            }
        }

        if (!resist_tele) do_dist = 10;
        else do_dist = 0;
        if (p_ptr->riding && (mon->id == p_ptr->riding)) do_dist = 0;

        if (type == GF_GRAVITY && (race->flagsr & RFR_RES_GRAV))
        {
            if (!quiet) note = " resists.";

            dam *= 3; dam /= randint1(6) + 6;
            do_dist = 0;
            mon_lore_r(mon, RFR_RES_GRAV);
        }
        else
        {
            /* 1. slowness */
            /* Powerful monsters can resist */
            if ( mon_save_slow(race->level, dam)
              || ((race->flags1 & RF1_UNIQUE) && mon_save_slow(race->level + 15, dam)) )
            {
                obvious = FALSE;
            }
            /* Normal monsters slow down */
            else mon_tim_add(mon, T_SLOW, 50);

            /* 2. stun */
            do_stun = mon_stun_amount(dam);

            /* Attempt a saving throw */
            if ( mon_save_stun(race->level, dam)
              || ((race->flags1 & RF1_UNIQUE) && mon_save_stun(race->level + 15, dam)) )
            {
                /* Resist */
                do_stun = 0;
                /* No obvious effect */
                if (!quiet) note = " is unaffected!";

                obvious = FALSE;
            }
        }
        break; }
    case GF_MANA:
    case GF_SEEKER:
    case GF_SUPER_RAY:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        break;
    case GF_DISINTEGRATE:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flags3 & RF3_HURT_ROCK)
        {
            mon_lore_3(mon, RF3_HURT_ROCK);
            note = " loses some skin!";
            note_dies = " evaporates!";
            dam *= 2;
        }
        break;
    case GF_PSI_STORM:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flags2 & RF2_EMPTY_MIND)
        {
            dam = 0;
            if (!quiet) note = " is immune!";
            mon_lore_2(mon, RF2_EMPTY_MIND);
            break;

        }
        else if (race->flags2 & (RF2_STUPID | RF2_WEIRD_MIND))
        {
            dam /= 3;
            if (!quiet) note = " resists.";
            break;
        }
        if (one_in_(4))
        {
            if (p_ptr->riding && (mon->id == p_ptr->riding)) do_dist = 0;
            else do_dist = 7;
        }
        if (one_in_(2))
        {
            do_stun = mon_stun_amount(dam);
            if (mon_save_stun(race->level, dam))
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
        _BABBLE_HACK()
        /* PSI only works if the monster can see you! -- RG */
        if (!point_los(mon->pos, p_ptr->pos))
        {
            if (seen_msg) msg_format("%^s can't see you, and isn't affected!", m_name);
            skipped = TRUE;
            break;
        }
        if (race->flags2 & RF2_EMPTY_MIND)
        {
            dam = 0;
            if (!quiet) note = " is immune!";
            mon_lore_2(mon, RF2_EMPTY_MIND);

        }
        else if ((race->flags2 & (RF2_STUPID | RF2_WEIRD_MIND)) ||
                 (race->flags3 & RF3_ANIMAL) ||
                 mon_save_psi(race->level, dam))
        {
            dam /= 3;
            if (!quiet) note = " resists.";


            /*
             * Powerful demons & undead can turn a mindcrafter's
             * attacks back on them
             */
            if ((race->flags3 & (RF3_UNDEAD | RF3_DEMON)) &&
                (race->level > p_ptr->lev / 2) &&
                one_in_(2))
            {
                note = NULL;
                msg_format("%^s%s corrupted mind backlashes your attack!",
                    m_name, (seen ? "'s" : "s"));

                /* Saving throw */
                if ((randint0(100 + race->level / 2) < p_ptr->skills.sav) && !CHECK_MULTISHADOW())
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
                            if (!p_ptr->no_stun) plr_tim_add(T_STUN, randint1(dam));
                            break;
                        case 3:
                            if (race->flags3 & RF3_NO_FEAR)
                                note = " is unaffected.";
                            else
                                fear_add_p(FEAR_SCARED);
                            break;
                        default:
                            if (!free_act_save_p(race->level))
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
        _BABBLE_HACK()
        if (race->flags2 & RF2_EMPTY_MIND)
        {
            dam = 0;
            if (!quiet) note = " is immune!";

        }
        else if ((race->flags2 & (RF2_STUPID | RF2_WEIRD_MIND)) ||
                 (race->flags3 & RF3_ANIMAL) ||
                 mon_save_psi(race->level, dam))
        {
            dam /= 3;
            if (!quiet) note = " resists.";


            /*
             * Powerful demons & undead can turn a mindcrafter's
             * attacks back on them
             */
            if ((race->flags3 & (RF3_UNDEAD | RF3_DEMON)) &&
                 (race->level > p_ptr->lev / 2) &&
                 (one_in_(2)))
            {
                note = NULL;
                msg_format("%^s%s corrupted mind backlashes your attack!",
                    m_name, (seen ? "'s" : "s"));

                /* Saving throw */
                if ((randint0(100 + race->level / 2) < p_ptr->skills.sav) && !CHECK_MULTISHADOW())
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

                        p_ptr->csp -= damroll(5, dam) / 2;
                        if (p_ptr->csp < 0) p_ptr->csp = 0;
                        p_ptr->redraw |= PR_MANA;
                        p_ptr->window |= (PW_SPELL);
                    }
                    take_hit(DAMAGE_ATTACK, dam, killer);  /* has already been /3 */
                }
                dam = 0;
            }
        }
        else if (dam > 0)
        {
            int b = damroll(5, dam) / 4;
            cptr str = (p_ptr->pclass == CLASS_MINDCRAFTER) ? "psychic energy" : "mana";
            msg_format("You convert %s%s pain into %s!",
                m_name, (seen ? "'s" : "s"), str);

            b = MIN(p_ptr->msp, p_ptr->csp + b);
            p_ptr->csp = b;
            p_ptr->redraw |= PR_MANA;
            p_ptr->window |= (PW_SPELL);
        }

        note_dies = " collapses, a mindless husk.";
        break;
    case GF_TELEKINESIS:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (one_in_(4))
        {
            if (p_ptr->riding && (mon->id == p_ptr->riding)) do_dist = 0;
            else do_dist = 7;
        }

        do_stun = mon_stun_amount(dam);
        if (mon_save_stun(race->level, dam))
        {
            do_stun = 0;
            obvious = FALSE;
        }
        break;
    case GF_PSY_SPEAR:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        break;
    case GF_METEOR:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        break;
    case GF_DOMINATION: {
        int power = dam;

        /* No "real" damage */
        dam = 0;

        if (!is_hostile(mon)) break;
        if (seen) obvious = TRUE;
        _BABBLE_HACK()

        /* Attempt a saving throw */
        if (mon_save_aux(mon->r_idx, power))
        {
            /*
             * Powerful demons & undead can turn a mindcrafter's
             * attacks back on them
             */
            if ((race->flags3 & (RF3_UNDEAD | RF3_DEMON)) &&
                (race->level > p_ptr->lev / 2) &&
                (one_in_(2)))
            {
                note = NULL;
                msg_format("%^s%s corrupted mind backlashes your attack!",
                    m_name, (seen ? "'s" : "s"));

                /* Saving throw */
                if (randint0(100 + race->level/2) < p_ptr->skills.sav)
                {
                    msg_print("You resist the effects!");

                }
                else
                {
                    /* Confuse, stun, terrify */
                    switch (randint1(4))
                    {
                        case 1:
                            if (!p_ptr->no_stun) plr_tim_add(T_STUN, power / 2);
                            break;
                        case 2:
                            plr_tim_add(T_CONFUSED, power / 2);
                            break;
                        default:
                        {
                            if (race->flags3 & RF3_NO_FEAR)
                                note = " is unaffected.";

                            else
                                fear_add_p(power);
                        }
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
            bool unique = BOOL(race->flags1 & RF1_UNIQUE);

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
                    if (race->flags3 & RF3_NO_CONF)
                    {
                        mon_lore_3(mon, RF3_NO_CONF);
                        if (!quiet) note = " is unaffected.";
                        break;
                    }
                    else if (!unique)
                    {
                        do_conf = power / 2;
                        break;
                    }
                default:
                    if (prace_is_(RACE_MON_VAMPIRE))
                    {
                        if (!unique && !mon_save_aux(mon->r_idx, power))
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
        if (touch && seen_msg) msg_format("%^s is <color:W>frozen</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_IM_COLD)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_IM_COLD);
        }
        else if (race->flagsr & RFR_RES_COLD)
        {
            if (!quiet) note = " resists.";
            dam /= 2;
            mon_lore_r(mon, RFR_RES_COLD);
        }
        else
        {
            if (race->flags3 & (RF3_HURT_COLD))
            {
                note = " is hit hard.";
                dam *= 2;
                mon_lore_3(mon, RF3_HURT_COLD);
            }
            if (!mon_save_stun(race->level, dam))
                do_stun = mon_stun_amount(dam);
        }
        break;
    case GF_OLD_DRAIN:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (!monster_living(race))
        {
            mon_lore_3(mon, RF3_DEMON);
            mon_lore_3(mon, RF3_UNDEAD);
            mon_lore_3(mon, RF3_NONLIVING);
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
        _BABBLE_HACK()
        if (!monster_living(race))
        {
            mon_lore_3(mon, RF3_DEMON);
            mon_lore_3(mon, RF3_UNDEAD);
            mon_lore_3(mon, RF3_NONLIVING);
            if (!quiet) note = " is immune.";
            obvious = FALSE;
            dam = 0;
        }
        else if (((race->flags1 & RF1_UNIQUE) && randint1(888) != 666)
                || mon_save_p(mon->r_idx, A_INT)
                || mon_save_p(mon->r_idx, A_INT) )
        {
            if (!quiet) note = " resists!";
            obvious = FALSE;
            dam = 0;
        }
        break;
    case GF_DEATH_RAY:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (!monster_living(race))
        {
            mon_lore_3(mon, RF3_DEMON);
            mon_lore_3(mon, RF3_UNDEAD);
            mon_lore_3(mon, RF3_NONLIVING);
            if (!quiet) note = " is immune.";
            obvious = FALSE;
            dam = 0;
        }
        else if ( ((race->flags1 & RF1_UNIQUE) && randint1(888) != 666)
               || (race->level + randint1(20) > randint1(caster_lev)))
        {
            if (!quiet) note = " resists!";
            obvious = FALSE;
            dam = 0;
        }

        break;
    case GF_OLD_POLY:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        do_poly = dam; /* save later ... */
        dam = 0;
        break;
    case GF_OLD_CLONE:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()

        if ( is_pet(mon)
          || (race->flags1 & RF1_UNIQUE)
          || (race->flags7 & (RF7_NAZGUL | RF7_UNIQUE2))
          || (mon->mflag2 & MFLAG2_QUESTOR) )
        {
            if (!quiet) note = " is unaffected!";
        }
        else
        {
            mon->hp = mon->maxhp;
            if (multiply_monster(mon->id, TRUE, 0L))
                note = " spawns!";
        }
        dam = 0;
        break;
    case GF_STAR_HEAL:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        mon_tim_delete(mon, MT_SLEEP);

        if (mon->maxhp < mon->max_maxhp)
        {
            if (seen_msg) msg_format("%^s recovers %s vitality.", m_name, m_poss);
            mon->maxhp = mon->max_maxhp;
        }

        if (!dam)
        {
            /* Redraw (later) if needed */
            check_mon_health_redraw(mon->id);
            break;
        }

        /* Fall through */
    case GF_OLD_HEAL:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()

        mon_tim_delete(mon, MT_SLEEP);
        mon_tim_remove(mon, T_STUN);
        mon_tim_remove(mon, T_CONFUSED);
        mon_tim_remove(mon, T_FEAR);

        if (!who && !(race->flags3 & RF3_EVIL))
            dam = dam * (625 + virtue_current(VIRTUE_COMPASSION))/625;

        /* Heal */
        if (mon->hp < 30000) mon->hp += dam;

        /* No overflow */
        if (mon->hp > mon->maxhp) mon->hp = mon->maxhp;

        if (!who)
        {
            virtue_add(VIRTUE_VITALITY, 1);

            if (race->flags1 & RF1_UNIQUE)
                virtue_add(VIRTUE_INDIVIDUALISM, 1);

            if (is_friendly(mon))
                virtue_add(VIRTUE_HONOUR, 1);
            else if (!(race->flags3 & RF3_EVIL))
            {
                if (race->flags3 & RF3_GOOD)
                    virtue_add(VIRTUE_COMPASSION, 2);
                else
                    virtue_add(VIRTUE_COMPASSION, 1);
            }

            if (race->flags3 & RF3_ANIMAL)
                virtue_add(VIRTUE_NATURE, 1);
        }

        if (mon->r_idx == MON_LEPER)
        {
            heal_leper = TRUE;
            if (!who) virtue_add(VIRTUE_COMPASSION, 5);
        }

        /* Redraw (later) if needed */
        check_mon_health_redraw(mon->id);

        /* Message */
        note = " looks healthier.";

        /* No "real" damage */
        dam = 0;
        break;
    case GF_OLD_SPEED:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        mon_tim_add(mon, T_FAST, 100);
        if (!who)
        {
            if (race->flags1 & RF1_UNIQUE)
                virtue_add(VIRTUE_INDIVIDUALISM, 1);
            if (is_friendly(mon))
                virtue_add(VIRTUE_HONOUR, 1);
        }

        /* No "real" damage */
        dam = 0;
        break;
    case GF_OLD_SLOW:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()

        if (_mon_save_aux(race, dam))
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
        }
        else mon_tim_add(mon, T_SLOW, 50);
        dam = 0;
        break;
    case GF_UNHOLY_WORD:
        if (is_pet(mon) && (race->flags3 & RF3_EVIL))
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
        _BABBLE_HACK()
        if (!(race->flags3 & RF3_EVIL) || _mon_save_aux(race, dam))
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
        _BABBLE_HACK()
        if (_mon_save_aux(race, dam))
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
        }
        else if ((race->flags3 & RF3_NO_SLEEP) && mon_save_aux(mon->r_idx, dam))
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
        }
        else
        {
            note = " is <color:v>paralyzed</color>!";
            do_paralyzed = randint1(3);
        }
        dam = 0;
        break;
    case GF_STASIS:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (_mon_save_aux(race, dam))
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
        }
        else
        {
            /*note = " is suspended!";*/
            do_paralyzed = 3;
        }
        dam = 0;
        break;
    case GF_SUBJUGATION: {
        bool unique = BOOL(race->flags1 & RF1_UNIQUE);
        int  attempts = randint1(1 + p_ptr->lev/50);
        int  ct = 0;

        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        mon_tim_delete(mon, MT_SLEEP);
        if (is_pet(mon))
            return FALSE;
        while (attempts--)
        {
            switch (randint1(5))
            {
            case 1:
                if (!mon_save_aux(mon->r_idx, dam))
                {
                    do_stun = dam / 2;
                    ct++;
                }
                break;
            case 2:
                if (!(race->flags3 & RF3_NO_CONF) && !mon_save_aux(mon->r_idx, dam))
                {
                    do_conf = dam / 2;
                    ct++;
                }
                break;
            case 3:
                if (!unique && !(mon->mflag2 & MFLAG2_QUESTOR) && !mon_save_aux(mon->r_idx, dam))
                {
                    note = " is frozen in terror!";
                    do_paralyzed = randint1(3);
                    attempts = 0;
                    ct++;
                }
                break;
            case 4:
                if (!mon_save_aux(mon->r_idx, dam))
                {
                    do_fear = dam;
                    ct++;
                }
                break;
            default:
                if (unique || (mon->mflag2 & MFLAG2_QUESTOR))
                {
                }
                else if ((mon->mflag2 & MFLAG2_NOPET) || mon_save_aux(mon->r_idx, dam))
                {
                    if (one_in_(4))
                        mon->mflag2 |= MFLAG2_NOPET;
                }
                else
                {
                    msg_format("%s bows to your will!", m_name);

                    set_pet(mon);
                    attempts = 0;
                    ct++;

                    virtue_add(VIRTUE_INDIVIDUALISM, -1);
                    if (race->flags3 & RF3_ANIMAL)
                        virtue_add(VIRTUE_NATURE, 1);

                    /* Ignore any prior effects */
                    return TRUE;
                }
            }
        }
        if (!ct && !quiet)
            note = " resists";
        dam = 0;
        break; }
    case GF_ELDRITCH_HOWL:
        if (race->flagsr & RFR_RES_ALL)
        {
            skipped = TRUE;
            break;
        }
        if (seen) obvious = TRUE;

        do_fear = damroll(3, (dam / 2)) + 1;

        if (race->flags3 & RF3_NO_FEAR)
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
            do_fear = 0;
        }
        else if ( mon_save_aux(mon->r_idx, dam)
               || ((race->flags1 & RF1_UNIQUE) && mon_save_aux(mon->r_idx, dam)) )
        {
            if (!quiet) note = " resists!";
            obvious = FALSE;
            do_fear = 0;
        }
        else if (!mon_save_aux(mon->r_idx, dam))
        {
            note = " is frozen with terror!";
            do_paralyzed = randint1(3);
        }
        dam = 0;
        break;
    case GF_CHARM:
    case GF_CHARM_RING_BEARER:
        if (seen) obvious = TRUE;

        if (type == GF_CHARM_RING_BEARER)
        {
            if (!mon_is_type(mon->r_idx, SUMMON_RING_BEARER))
            {
                note = " is not a suitable ring bearer.";
                dam = 0;
                break;
            }
        }
        else
        {
            dam += (adj_con_fix[p_ptr->stat_ind[A_CHR]] - 1);
            dam += virtue_current(VIRTUE_HARMONY)/10;
            dam -= virtue_current(VIRTUE_INDIVIDUALISM)/20;
            if ((race->flags1 & RF1_UNIQUE) || (race->flags7 & RF7_NAZGUL))
                dam = dam * 2 / 3;
        }

        if (race->flagsr & RFR_RES_ALL)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_RES_ALL);
            break;
        }
        else if (mon->mflag2 & MFLAG2_QUESTOR)
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
        }
        else if ((mon->mflag2 & MFLAG2_NOPET) || race->level > randint1(dam))
        {
            if (!quiet) note = " resists!";
            obvious = FALSE;
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else if (p_ptr->cursed & OFC_AGGRAVATE)
        {
            note = " hates you too much!";
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else
        {
            note = " suddenly seems friendly!";

            set_pet(mon);

            virtue_add(VIRTUE_INDIVIDUALISM, -1);
            if (race->flags3 & RF3_ANIMAL)
                virtue_add(VIRTUE_NATURE, 1);
        }
        dam = 0;
        break;
    case GF_CONTROL_UNDEAD:
        if (seen) obvious = TRUE;

        dam += virtue_current(VIRTUE_UNLIFE)/10;
        dam -= virtue_current(VIRTUE_INDIVIDUALISM)/20;

        if (race->flagsr & RFR_RES_ALL)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_RES_ALL);
            break;
        }

        if ((race->flags1 & RF1_UNIQUE) || (race->flags7 & RF7_NAZGUL))
            dam = dam * 2 / 3;

        /* Attempt a saving throw */
        if ((mon->mflag2 & MFLAG2_QUESTOR) ||
            !(race->flags3 & RF3_UNDEAD) ||
            (mon->mflag2 & MFLAG2_NOPET) ||
            (race->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
        {
            /* No obvious effect */
            if (!quiet) note = " is unaffected!";

            obvious = FALSE;
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else if (p_ptr->cursed & OFC_AGGRAVATE)
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

        if (race->flagsr & RFR_RES_ALL)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_RES_ALL);
            break;
        }

        if ((race->flags1 & RF1_UNIQUE) || (race->flags7 & RF7_NAZGUL))
            dam = dam * 2 / 3;

        /* Attempt a saving throw */
        if ((mon->mflag2 & MFLAG2_QUESTOR) ||
            !(race->flags3 & RF3_DEMON) ||
            (mon->mflag2 & MFLAG2_NOPET) ||
            (race->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
        {
            /* No obvious effect */
            if (!quiet) note = " is unaffected!";

            obvious = FALSE;
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else if (p_ptr->cursed & OFC_AGGRAVATE)
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

        if (race->flagsr & RFR_RES_ALL)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_RES_ALL);
            break;
        }

        if ((race->flags1 & RF1_UNIQUE) || (race->flags7 & RF7_NAZGUL))
            dam = dam * 2 / 3;

        /* Attempt a saving throw */
        if ((mon->mflag2 & MFLAG2_QUESTOR) ||
            !(race->flags3 & RF3_ANIMAL) ||
            (mon->mflag2 & MFLAG2_NOPET) ||
            (race->flags3 & RF3_NO_CONF) ||
            (race->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
        {
            /* Memorize a flag */
            if (race->flags3 & (RF3_NO_CONF))
            {
                mon_lore_3(mon, RF3_NO_CONF);
            }

            /* Resist */
            /* No obvious effect */
            if (!quiet) note = " is unaffected!";

            obvious = FALSE;
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else if (p_ptr->cursed & OFC_AGGRAVATE)
        {
            note = " hates you too much!";

            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else
        {
            note = " is tamed!";

            set_pet(mon);

            if (race->flags3 & RF3_ANIMAL)
                virtue_add(VIRTUE_NATURE, 1);
        }

        /* No "real" damage */
        dam = 0;
        break;
    case GF_CONTROL_PACT_MONSTER:
        if (warlock_is_pact_monster(race) && !is_pet(mon))
        {
            if (seen) obvious = TRUE;

            /* Attempt a saving throw */
            if ( (mon->mflag2 & MFLAG2_QUESTOR)
              || (mon->mflag2 & MFLAG2_NOPET)
              || mon_save_p(mon->r_idx, A_CHR)
              || ((race->flags1 & RF1_UNIQUE) && mon_save_p(mon->r_idx, A_CHR)) )
            {
                if (warlock_is_(WARLOCK_HOUNDS))
                    note = " growls at you in defiance!";
                else
                    note = " resists your control.";
                obvious = FALSE;
                if (one_in_(4))
                    mon->mflag2 |= MFLAG2_NOPET;
            }
            else if (p_ptr->cursed & OFC_AGGRAVATE)
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

        dam += (adj_chr_chm[p_ptr->stat_ind[A_CHR]]);
        dam -= virtue_current(VIRTUE_UNLIFE)/10;
        dam -= virtue_current(VIRTUE_INDIVIDUALISM)/20;

        if (race->flags3 & (RF3_NO_CONF)) dam -= 30;
        if (dam < 1) dam = 1;
        msg_format("You stare into %s.", m_name_object);
        if (race->flagsr & RFR_RES_ALL)
        {
            if (!quiet) note = " is immune.";
            dam = 0;
            mon_lore_r(mon, RFR_RES_ALL);
            break;
        }

        if ((race->flags1 & RF1_UNIQUE) || (race->flags7 & RF7_NAZGUL))
            dam = dam * 2 / 3;

        /* Attempt a saving throw */
        if ((mon->mflag2 & MFLAG2_QUESTOR) ||
            (mon->mflag2 & MFLAG2_NOPET) ||
             !monster_living(race) ||
             ((race->level+10) > randint1(dam)))
        {
            /* Resist */
            /* No obvious effect */
            if (!quiet) note = " is unaffected!";

            obvious = FALSE;
            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else if (p_ptr->cursed & OFC_AGGRAVATE)
        {
            note = " hates you too much!";

            if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
        }
        else
        {
            note = " is tamed!";

            set_pet(mon);

            if (race->flags3 & RF3_ANIMAL)
                virtue_add(VIRTUE_NATURE, 1);
        }
        dam = 0;
        break;
    case GF_OLD_SLEEP:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flags3 & RF3_NO_SLEEP)
        {
            if (!quiet) note = " is immune!";
            mon_lore_3(mon, RF3_NO_SLEEP);
            obvious = FALSE;
        }
        else if (_mon_save_aux(race, dam))
        {
            if (!quiet) note = " resists!";
            obvious = FALSE;
        }
        else
        {
            do_sleep = 500;
        }
        dam = 0;
        break;
    case GF_BLIND:
    case GF_OLD_CONF:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        do_conf = damroll(3, dam/2) + 1;
        if (race->flags3 & RF3_NO_CONF)
        {
            do_conf = 0;
            if (!quiet) note = " is immune!";
            mon_lore_3(mon, RF3_NO_CONF);
            obvious = FALSE;
        }
        else if (_mon_save_aux(race, dam))
        {
            do_conf = 0;
            if (!quiet) note = " resists!";
            obvious = FALSE;
        }
        dam = 0;
        break;
    case GF_STUN:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        do_stun = dam;
        if (race->flags3 & RF3_NO_STUN)
        {
            do_stun = 0;
            if (!quiet) note = " is unaffected!";
            mon_lore_3(mon, RF3_NO_STUN);
            obvious = FALSE;
        }
        dam = 0;
        break;
    case GF_LITE_WEAK:
        if (!dam)
        {
            skipped = TRUE;
            break;
        }
        if (race->flagsr & RFR_RES_ALL)
        {
            dam = 0;
            break;
        }
        /* Hurt by light */
        if (race->flags3 & (RF3_HURT_LITE))
        {
            /* Obvious effect */
            if (seen) obvious = TRUE;

            /* Memorize the effects */
            mon_lore_3(mon, RF3_HURT_LITE);

            /* Special effect */
            note = " cringes from the light!";
            note_dies = " shrivels away in the light!";

        }
        /* Normally no damage */
        else dam = 0;
        break;
    case GF_LITE:
        if (touch && seen_msg) msg_format("%^s is <color:y>dazzled</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_LITE)
        {
            if (!quiet) note = " resists.";

            dam *= 2; dam /= (randint1(6)+6);
            mon_lore_r(mon, RFR_RES_LITE);
        }
        else if (race->flags3 & (RF3_HURT_LITE))
        {
            mon_lore_3(mon, RF3_HURT_LITE);
            note = " cringes from the light!";
            note_dies = " shrivels away in the light!";

            dam *= 2;
        }
        break;
    case GF_DARK:
        if (touch && seen_msg) msg_format("%^s is <color:D>benighted</color>!", m_name);
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flagsr & RFR_RES_DARK)
        {
            if (!quiet) note = " resists.";
            dam *= 2; dam /= (randint1(6)+6);
            mon_lore_r(mon, RFR_RES_DARK);
        }
        break;
    case GF_KILL_WALL: /* e.g. Stone to Mud */
        if (race->flagsr & RFR_RES_ALL)
        {
            dam = 0;
            break;
        }
        if (race->flags3 & (RF3_HURT_ROCK))
        {
            if (seen) obvious = TRUE;
            mon_lore_3(mon, RF3_HURT_ROCK);
            note = " loses some skin!";
            note_dies = " dissolves!";
        }
        /* Usually, ignore the effects */
        else dam = 0;
        break;
    case GF_AWAY_UNDEAD:
        if (race->flags3 & (RF3_UNDEAD))
        {
            bool resists_tele = FALSE;
            if (race->flagsr & RFR_RES_TELE)
            {
                if ((race->flags1 & (RF1_UNIQUE)) || (race->flagsr & RFR_RES_ALL))
                {
                    mon_lore_r(mon, RFR_RES_TELE);
                    if (!quiet) note = " is unaffected!";

                    resists_tele = TRUE;
                }
                else if (race->level > randint1(100))
                {
                    mon_lore_r(mon, RFR_RES_TELE);
                    if (!quiet) note = " resists!";

                    resists_tele = TRUE;
                }
            }
            if (!resists_tele)
            {
                if (seen) obvious = TRUE;
                mon_lore_3(mon, RF3_UNDEAD);
                do_dist = dam;
            }
        }
        else skipped = TRUE;
        dam = 0;
        break;
    case GF_AWAY_EVIL:
        if (race->flags3 & (RF3_EVIL))
        {
            bool resists_tele = FALSE;
            if (race->flagsr & RFR_RES_TELE)
            {
                if ((race->flags1 & (RF1_UNIQUE)) || (race->flagsr & RFR_RES_ALL))
                {
                    mon_lore_r(mon, RFR_RES_TELE);
                    if (!quiet) note = " is unaffected!";

                    resists_tele = TRUE;
                }
                else if (race->level > randint1(100))
                {
                    mon_lore_r(mon, RFR_RES_TELE);
                    if (!quiet) note = " resists!";

                    resists_tele = TRUE;
                }
            }
            if (!resists_tele)
            {
                if (seen) obvious = TRUE;
                mon_lore_3(mon, RF3_EVIL);
                do_dist = dam;
            }
        }
        else skipped = TRUE;
        dam = 0;
        break;
    case GF_ISOLATION: {
        bool resists_tele = FALSE;
        if (mon->id == p_ptr->duelist_target_idx)
        {
            dam = 0;
            return TRUE;
        }
        if (race->flagsr & RFR_RES_TELE)
        {
            if (race->flagsr & RFR_RES_ALL)
            {
                mon_lore_r(mon, RFR_RES_TELE);
                if (!quiet) note = " is unaffected!";
                resists_tele = TRUE;
            }
            else if (mon_save_p(mon->r_idx, A_DEX))
            {
                mon_lore_r(mon, RFR_RES_TELE);
                if (!quiet) note = " resists!";
                resists_tele = TRUE;
            }
        }
        if (!resists_tele)
        {
            if (seen) obvious = TRUE;
            do_dist = dam;
        }
        dam = 0;
        break; }
    case GF_AWAY_ALL: {
        bool resists_tele = FALSE;
        if (race->flagsr & RFR_RES_TELE)
        {
            if ( (race->flags1 & RF1_UNIQUE)
              || (race->flagsr & RFR_RES_ALL) 
              || (mon->smart & (1U << SM_GUARDIAN)) )
            {
                mon_lore_r(mon, RFR_RES_TELE);
                if (!quiet) note = " is unaffected!";
                resists_tele = TRUE;
            }
            else if (race->level > randint1(100))
            {
                mon_lore_r(mon, RFR_RES_TELE);
                if (!quiet) note = " resists!";
                resists_tele = TRUE;
            }
        }
        else if (mon->smart & (1U << SM_GUARDIAN))
        {
            if (!quiet) note = " is unaffected!";
            resists_tele = TRUE;
        }
        if (!resists_tele)
        {
            if (seen) obvious = TRUE;
            do_dist = dam;
        }
        dam = 0;
        break; }
    case GF_TURN_UNDEAD:
        if (race->flagsr & RFR_RES_ALL)
        {
            skipped = TRUE;
            break;
        }
        if (race->flags3 & (RF3_UNDEAD))
        {
            if (seen) obvious = TRUE;
            mon_lore_3(mon, RF3_UNDEAD);

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
        if (race->flagsr & RFR_RES_ALL)
        {
            skipped = TRUE;
            break;
        }
        if (race->flags3 & (RF3_EVIL))
        {
            if (seen) obvious = TRUE;
            mon_lore_3(mon, RF3_EVIL);

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
    case GF_TURN_ALL:
        if (race->flagsr & RFR_RES_ALL)
        {
            skipped = TRUE;
            break;
        }
        if (seen) obvious = TRUE;
        do_fear = damroll(3, (dam / 2)) + 1;
        if ((race->flags3 & RF3_NO_FEAR) || fear_save_m(mon))
        {
            if (!quiet) note = " is unaffected!";
            obvious = FALSE;
            do_fear = 0;
        }
        dam = 0;
        break;
    case GF_DISP_UNDEAD:
        if (race->flagsr & RFR_RES_ALL)
        {
            skipped = TRUE;
            dam = 0;
            break;
        }
        if (race->flags3 & (RF3_UNDEAD))
        {
            if (seen) obvious = TRUE;
            mon_lore_3(mon, RF3_UNDEAD);
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
        if (race->flagsr & RFR_RES_ALL)
        {
            skipped = TRUE;
            dam = 0;
            break;
        }
        if (race->flags3 & RF3_EVIL)
        {
            if (seen) obvious = TRUE;
            mon_lore_3(mon, RF3_EVIL);
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
        if (race->flagsr & RFR_RES_ALL)
        {
            skipped = TRUE;
            dam = 0;
            break;
        }
        if (race->flags3 & RF3_GOOD)
        {
            if (seen) obvious = TRUE;
            mon_lore_3(mon, RF3_GOOD);
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
        if (race->flagsr & RFR_RES_ALL)
        {
            skipped = TRUE;
            dam = 0;
            break;
        }
        if (monster_living(race))
        {
            if (seen) obvious = TRUE;
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
        if (race->flagsr & RFR_RES_ALL)
        {
            skipped = TRUE;
            dam = 0;
            break;
        }
        if (race->flags3 & (RF3_DEMON))
        {
            if (seen) obvious = TRUE;
            mon_lore_3(mon, RF3_DEMON);
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
        if (race->flagsr & RFR_RES_ALL)
        {
            skipped = TRUE;
            dam = 0;
            break;
        }
        if (seen) obvious = TRUE;
        note = " shudders.";
        note_dies = " dissolves!";
        break;
    case GF_DRAINING_TOUCH:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (mon_is_magical(mon))
        {
            if (who == GF_WHO_PLAYER && sp_player(dam))
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
        _BABBLE_HACK()
        if (mon_is_magical(mon))
        {
            if (who > 0)
            {
                if (caster_ptr->hp < caster_ptr->maxhp)
                {
                    caster_ptr->hp += 6 * dam;
                    if (caster_ptr->hp > caster_ptr->maxhp) caster_ptr->hp = caster_ptr->maxhp;
                    check_mon_health_redraw(who);
                    monster_desc(killer, caster_ptr, 0);
                    if (seen_msg) msg_format("%^s appears healthier.", killer);
                }
            }
            else if (who == GF_WHO_PLAYER && p_ptr->chp < p_ptr->mhp)
            {
                msg_format("You draw psychic energy from %s.", m_name_object);
                hp_player(dam);
            }
        }
        else if (!quiet) msg_format("%^s is unaffected.", m_name);
        dam = 0;
        break;
    case GF_ANTIMAGIC: { /* Formerly restricted to rage-mage. But now, mon vs mon */
        bool save = FALSE;/* and the possessor can use the ANTIMAGIC monster spell */
        if (seen) obvious = TRUE;
        _BABBLE_HACK()            /*v---Possessor uses monster level (dam) for the save */
        if (who == GF_WHO_PLAYER && !mon_spell_current())
        {
            int stat = A_CHR;
            int rolls = 1, made = 0;
            int i;
            if (p_ptr->pclass == CLASS_RAGE_MAGE)
            {
                stat = A_STR;
                if (plr_tim_find(T_BERSERK)) rolls++;
            }
            for (i = 0; i < rolls; i++)
            {
                if (mon_save_p(mon->r_idx, stat))
                    made++;
            }
            save = (made == rolls);
        }
        else
        {
            save = mon_save_aux(mon->r_idx, dam);
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
        _BABBLE_HACK()
        if (race->flags2 & RF2_EMPTY_MIND)
        {
            mon_lore_2(mon, RF2_EMPTY_MIND);
            if (!quiet) note = " is immune!";
            dam = 0;
        }
        else if (psion_mon_save_p(mon->r_idx, dam))
        {
            if (!quiet) note = " resists!";
            dam = 0;
        }
        else if (race->flags2 & RF2_WEIRD_MIND)
        {
            mon_lore_2(mon, RF2_WEIRD_MIND);
            if (!quiet) note = " resists somewhat.";
            dam /= 2;
            if (dam == 0) dam = 1;
        }
        if (dam)
        {
            note = " is blasted by psionic energy.";
            if (race->flags3 & RF3_NO_CONF)
                mon_lore_3(mon, RF3_NO_CONF);
            else
                do_conf = 2*dam;
            if (race->flags3 & RF3_NO_STUN)
                mon_lore_3(mon, RF3_NO_STUN);
            else
                do_stun = 2*dam;
            if (dam) mon_tim_add(mon, T_SLOW, 2*dam);
            dam = 0;
        }
        break;
    case GF_MIND_BLAST:
        if (seen) obvious = TRUE;
        if (who == GF_WHO_PLAYER) msg_format("You gaze intently at %s.", m_name_object);
        _BABBLE_HACK()
        if ( mon_save_smash(race->level, dam)
          || ((race->flags1 & RF1_UNIQUE) && mon_save_smash(race->level+15, dam)) )
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        else if (race->flags2 & RF2_EMPTY_MIND)
        {
            mon_lore_2(mon, RF2_EMPTY_MIND);
            if (!quiet) note = " is immune!";
            dam = 0;
        }
        else if (race->flags2 & RF2_WEIRD_MIND)
        {
            mon_lore_2(mon, RF2_WEIRD_MIND);
            if (!quiet) note = " resists.";
            dam /= 3;
        }
        else
        {
            note = " is blasted by psionic energy.";
            note_dies = " collapses, a mindless husk.";

            if (who > 0) do_conf = randint0(4) + 4;
            else do_conf = randint0(8) + 8;
            if (race->flags3 & RF3_NO_CONF)
            {
                do_conf = 0;
                if (race->flags3 & (RF3_NO_CONF))
                    mon_lore_3(mon, RF3_NO_CONF);
            }
        }
        break;
    case GF_BRAIN_SMASH:
        if (seen) obvious = TRUE;
        if (who == GF_WHO_PLAYER) msg_format("You gaze intently at %s.", m_name_object);
        _BABBLE_HACK()
        if ( mon_save_smash(race->level, dam)
          || ((race->flags1 & RF1_UNIQUE) && mon_save_smash(race->level+15, dam)) )
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        else if (race->flags2 & RF2_EMPTY_MIND)
        {
            mon_lore_2(mon, RF2_EMPTY_MIND);
            if (!quiet) note = " is immune!";
            dam = 0;
        }
        else if (race->flags2 & RF2_WEIRD_MIND)
        {
            mon_lore_2(mon, RF2_WEIRD_MIND);
            if (!quiet) note = " resists.";
            dam /= 3;
        }
        else
        {
            note = " is blasted by psionic energy.";
            note_dies = " collapses, a mindless husk.";

            if (who > 0)
            {
                do_conf = randint0(4) + 4;
                do_stun = randint0(4) + 4;
            }
            else
            {
                do_conf = randint0(8) + 8;
                do_stun = randint0(8) + 8;
            }
            if (!(race->flags1 & RF1_UNIQUE) || !mon_save_smash(race->level+15, dam))
                mon_tim_add(mon, T_SLOW, 10);

            if (race->flags3 & RF3_NO_CONF)
            {
                do_conf = 0;
                if (race->flags3 & (RF3_NO_CONF))
                    mon_lore_3(mon, RF3_NO_CONF);
            }
        }
        break;
    case GF_CAUSE_1:
        if (seen) obvious = TRUE;
        if (!who) msg_format("You %s at %s and curse.", prace_is_(RACE_MON_BEHOLDER) ? "gaze" : "point", m_name);
        _BABBLE_HACK()
        if (randint0(100 + (caster_lev / 2)) < (race->level + 35))
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        break;
    case GF_CAUSE_2:
        if (seen) obvious = TRUE;
        if (!who) msg_format("You %s at %s and curse horribly.", prace_is_(RACE_MON_BEHOLDER) ? "gaze" : "point", m_name);
        _BABBLE_HACK()
        if (randint0(100 + (caster_lev / 2)) < (race->level + 35))
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        break;
    case GF_CAUSE_3:
        if (seen) obvious = TRUE;
        if (!who) msg_format("You point at %s, incanting terribly!", m_name);
        _BABBLE_HACK()
        if (randint0(100 + (caster_lev / 2)) < (race->level + 35))
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        break;
    case GF_CAUSE_4: {
        bool save = FALSE;
        if (seen) obvious = TRUE;
        if (!who) msg_format("You %s at %s and scream the word, 'DIE!'.", prace_is_(RACE_MON_BEHOLDER) ? "gaze" : "point", m_name);
        _BABBLE_HACK()
        if (who == GF_WHO_PLAYER)
        {
            save = p_ptr->current_r_idx != MON_KENSHIROU && mon_save_p(mon->r_idx, A_WIS);
        }
        else
        {
            save = ((randint0(100 + (caster_lev / 2)) < (race->level + 35))
                && ((who <= 0) || (caster_ptr->r_idx != MON_KENSHIROU)));
        }
        if (save)
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        break; }
    case GF_HAND_DOOM:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (race->flags1 & RF1_UNIQUE)
        {
            if (!quiet) note = " is unaffected!";
            dam = 0;
        }
        else
        {
            if (randint1(dam) >= race->level + randint1(20))
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
        if (!quests_allow_all_spells() && !is_pet(mon))
        {
            msg_format("%^s is unaffected.", m_name);
            skipped = TRUE;
            break;
        }
        if ( (race->flags1 & RF1_UNIQUE)
          || (race->flags7 & RF7_NAZGUL)
          || (race->flags7 & RF7_UNIQUE2)
          || (mon->mflag2 & MFLAG2_QUESTOR)
          || mon->parent_m_idx )
        {
            msg_format("%^s is unaffected.", m_name);
            skipped = TRUE;
            break;
        }
        if (is_pet(mon)) nokori_hp = mon->maxhp * 4;
        else if ((p_ptr->pclass == CLASS_BEASTMASTER) && monster_living(race))
            nokori_hp = mon->maxhp * 3 / 10;
        else if (p_ptr->easy_capture)
            nokori_hp = mon->maxhp * 3 / 10;
        else if (warlock_is_(WARLOCK_DRAGONS) && (race->flags3 & RF3_DRAGON))
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
            if (mon->mflag2 & MFLAG2_CHAMELEON) choose_new_monster(mon, FALSE, MON_CHAMELEON);
            msg_format("You capture %^s!", m_name);
            quests_on_kill_mon(mon);
            cap_mon = mon->r_idx;
            cap_mspeed = mon->mspeed;
            cap_hp = mon->hp;
            cap_maxhp = mon->max_maxhp;
            cap_nickname = mon->nickname;
            if (mon->id == p_ptr->riding)
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
        if (who > 0)
            return mon_attack(caster_ptr, mon->pos);
        else if (who == GF_WHO_PLAYER)
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
        _BABBLE_HACK()
        if (race->flags2 & RF2_EMPTY_MIND)
        {
            note = " is immune!";
            dam = 0;
            skipped = TRUE;
            mon_lore_2(mon, RF2_EMPTY_MIND);
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
            if ((race->flags1 & RF1_UNIQUE) ||
                (race->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
            {
                note = " is unaffected!";

                obvious = FALSE;
            }

            /* Normal monsters slow down */
            else mon_tim_add(mon, T_SLOW, 50);
        }

        else if (effect == 2)
        {
            do_stun = damroll((p_ptr->lev / 10) + 3 , (dam)) + 1;

            /* Attempt a saving throw */
            if ((race->flags1 & (RF1_UNIQUE)) ||
                (race->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
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
            /* Attempt a saving throw */
            if ((race->flags1 & RF1_UNIQUE) ||
                (race->flags3 & RF3_NO_SLEEP) ||
                (race->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
            {
                /* Memorize a flag */
                if (race->flags3 & RF3_NO_SLEEP)
                {
                    mon_lore_3(mon, RF3_NO_SLEEP);
                }

                /* No obvious effect */
                note = " is unaffected!";

                obvious = FALSE;
            }
            else
            {
                /* Go to sleep (much) later */
                note = " falls asleep!";
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

        if (is_pet(mon) || is_friendly(mon))
        {
            msg_print("Failed!");
            return FALSE;
        }

        for (dir = 0; dir < 8; dir++)
        {
            point_t p = point_step(mon->pos, ddd[dir]);

            if (!cave_naked_at(p)) continue;
            if (p_ptr->lev < 45)
                cave_set_feat(p.y, p.x, feat_rubble);
            else
                cave_set_feat(p.y, p.x, feat_granite);
        }
        return TRUE; }
    case GF_GENOCIDE:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        if (genocide_aux(mon, dam, !who, (race->level + 1) / 2, "Genocide One"))
        {
            if (seen_msg)
            {
                if (dam == 666) /* Hack for Daemon flavored message */
                    msg_format("%^s is sent directly to hell!", m_name);
                else
                    msg_format("%^s disappeared!", m_name);
            }

            virtue_add(VIRTUE_VITALITY, -1);
            return TRUE;
        }
        skipped = TRUE;
        break;
    case GF_BLOOD_CURSE:
        if (seen) obvious = TRUE;
        _BABBLE_HACK()
        break;
    case GF_CRUSADE: {
        bool success = FALSE;
        if (seen) obvious = TRUE;

        if (race->flags3 & RF3_GOOD)
        {
            if (race->flags3 & (RF3_NO_CONF)) dam -= 50;
            if (dam < 1) dam = 1;

            /* No need to tame your pet */
            if (is_pet(mon))
            {
                mon_tim_add(mon, T_FAST, 100);
                success = TRUE;
            }

            /* Attempt a saving throw */
            else if ((mon->mflag2 & MFLAG2_QUESTOR) ||
                (race->flags1 & (RF1_UNIQUE)) ||
                (mon->mflag2 & MFLAG2_NOPET) ||
                (p_ptr->cursed & OFC_AGGRAVATE) ||
                 ((race->level+10) > randint1(dam)))
            {
                /* Resist */
                if (one_in_(4)) mon->mflag2 |= MFLAG2_NOPET;
            }
            else
            {
                note = " is tamed!";
                set_pet(mon);
                mon_tim_add(mon, T_FAST, 100);
                mon_lore_3(mon, RF3_GOOD);
                success = TRUE;
            }
        }

        if (!success)
        {
            if (!(race->flags3 & RF3_NO_FEAR))
            {
                do_fear = randint1(90)+10;
            }
            else
            {
                mon_lore_3(mon, RF3_NO_FEAR);
            }
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
    if (race->flags1 & (RF1_UNIQUE)) do_poly = 0;

    /* Quest monsters cannot be polymorphed */
    if (mon->mflag2 & MFLAG2_QUESTOR) do_poly = 0;

    if (p_ptr->riding && (mon->id == p_ptr->riding)) do_poly = 0;

    /* "Unique" and "quest" monsters can only be "killed" by the player. */
    if (((race->flags1 & RF1_UNIQUE) || (race->flags7 & RF7_NAZGUL) || (mon->mflag2 & MFLAG2_QUESTOR))
      && !prace_is_(RACE_MON_QUYLTHULG))
    {
        if (who && (dam > mon->hp)) dam = mon->hp;
    }

    if (!who && slept)
    {
        if (!(race->flags3 & RF3_EVIL) || one_in_(5)) virtue_add(VIRTUE_COMPASSION, -1);
        if (!(race->flags3 & RF3_EVIL) || one_in_(5)) virtue_add(VIRTUE_HONOUR, -1);
    }

    /* Modify the damage */
    tmp = dam;
    if (dam)
    {
        if (who > 0)
            dam = mon_damage_mod_mon(mon, dam, type == GF_PSY_SPEAR);
        else
            dam = mon_damage_mod(mon, dam, type == GF_PSY_SPEAR);
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
        /* Sound and Impact resisters never stun
         * XXX Why should RES_SOUND protect from mental attacks?! */
        if ( do_stun
          && (_is_mental_attack(type) || !(race->flagsr & (RFR_RES_SOUN | RFR_RES_WALL)))
          && !(race->flags3 & RF3_NO_STUN) )
        {
            mon_stun(mon, do_stun);
            if (seen) obvious = TRUE;
            get_angry = TRUE;
        }

        if (do_conf && (type == GF_ELDRITCH_CONFUSE || !(race->flags3 & RF3_NO_CONF)))
        {
            if (seen) obvious = TRUE;
            mon_tim_add(mon, T_CONFUSED, do_conf);
            get_angry = TRUE;
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
        if (do_poly && !mon_save_poly(race->level, do_poly))
        {
            if (polymorph_monster(mon))
            {
                if (seen) obvious = TRUE;
                race = mon_race(mon);
            }
            else
            {
                note = " is unaffected!";
            }
        }

        /* Teleport */
        if (do_dist)
        {
            if ((mon->mflag2 & MFLAG2_VAULT) && !one_in_(3))
            {
                note = " resists!";
            }
            else if (mon->smart & (1U << SM_GUARDIAN))
            {
                note = " is unaffected!";
            }
            else
            {
                if (seen) obvious = TRUE;
                note = " disappears!";
                teleport_away(mon->id, do_dist,
                            (!who ? TELEPORT_DEC_VALOUR : 0L) | TELEPORT_PASSIVE);
                where = mon->pos;
            }
        }

        /* Fear */
        if (do_fear && !mon_tim_find(mon, T_BERSERK))
        {
            mon_tim_add(mon, T_FEAR, do_fear);
            if ((flags & GF_AFFECT_ATTACK) && plr_attack_current()) /* beholder */
                plr_attack_current()->fear = TRUE;
            get_angry = TRUE;
        }
    }

    if (type == GF_DRAIN_MANA)
    {
        /* Drain mana does nothing */
    }
    /* If another monster did the damage, hurt the monster by hand */
    else if (who)
    {
        check_mon_health_redraw(mon->id);
        mon_tim_delete(mon, MT_SLEEP);

        /* Hurt the monster */
        mon->hp -= dam;

        /* Dead monster */
        if (mon->hp < 0)
        {
            bool sad = FALSE;

            if (is_pet(mon) && !(mon->ml))
                sad = TRUE;

            /* Give detailed messages if destroyed */
            if (known && note && seen_msg)
            {
                monster_desc(m_name, mon, MD_TRUE_NAME);
                msg_format("%^s%s", m_name, note);
            }

            if (who > 0) monster_gain_exp(who, mon->r_idx);

            mon_check_kill_unique(mon->id);

            /* Generate treasure, etc */
            monster_death(mon, who_is_pet || p_ptr->spell_turned);

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
        if (type == GF_ELDRITCH_DRAIN && monster_living(race))
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
        {
            int lore_dam = MIN(dam, mon->hp);
            hack_max_m_dam = MAX(hack_max_m_dam, lore_dam);
        }

        /* Player innate blows often gf_affect_m for effects. Keep the total damage up to
         * date, but be prepared for weirdness like Splatter that might affect other monsters. */
        if (plr_attack_current() && plr_attack_current()->mon == mon)
            plr_attack_current()->dam_total += dam;

        /* Hurt the monster, check for fear and death
              v---- Revert 525c2ace: Warlocks: Playtesting Dragon Pact. Massive problems with project()
                    The problem here is that many attacks, like Slow Monster, do no physical damage, but
                    rely on mon_take_hit to do other things, like wake up sleeping monsters (or reveal
                    player ring mimics). This code needs refactoring ...*/
        if (/*dam &&*/ mon_take_hit(mon->id, dam, &fear, note_dies))
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
            if ( who == GF_WHO_PLAYER
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
                        pack_info_t *pack_ptr = pack_info_ptr(mon->id);
                        if (pack_ptr && pack_ptr->ai == AI_GUARD_POS)
                            mon->anger = 100;
                        else
                            mon_anger(mon);
                    }
                }
                /* Splashing Uniques out of LOS makes them rethink their approach */
                if (splashed && (race->flags1 & RF1_UNIQUE))
                {
                    pack_info_t *pack_ptr = pack_info_ptr(mon->id);

                    if (pack_ptr)
                    {
                        int odds = MAX(1, 7 - mon->anger/10);

                        if (!allow_ticked_off(race)) /* Paranoia ... These should already be seeking! */
                            odds = 1;

                        switch (pack_ptr->ai)
                        {
                        case AI_MAINTAIN_DISTANCE:
                            if (one_in_(odds))
                                pack_ptr->ai = AI_SEEK;
                            else
                                pack_ptr->distance += 2;
                            break;

                        case AI_SHOOT:
                        case AI_LURE:
                            if (one_in_(odds))
                                pack_ptr->ai = AI_SEEK;
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

    if ((type == GF_BLOOD_CURSE) && one_in_(4))
    {
        int curse_flg = (PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_JUMP);
        int count = 0;
        do
        {
            switch (randint1(28))
            {
            case 1: case 2:
                if (!count)
                {
                    msg_print("The ground trembles...");

                    earthquake(point_create(tx, ty), 4 + randint0(4));
                    if (!one_in_(6)) break;
                }
            case 3: case 4: case 5: case 6: case 7: case 8:
                if (!count)
                {
                    int dam = damroll(10, 10);
                    msg_print("A portal opens to a plane of raw mana!");

                    project(0, 8, ty,tx, dam, GF_MANA, curse_flg);
                    if (!one_in_(6)) break;
                }
            case 9: case 10: case 11:
                if (!count)
                {
                    msg_print("Space warps about it!");

                    if (mon->r_idx) teleport_away(mon->id, damroll(10, 10), TELEPORT_PASSIVE);
                    if (one_in_(13)) count += activate_hi_summon(ty, tx, TRUE);
                    if (!one_in_(6)) break;
                }
            case 12: case 13: case 14: case 15: case 16:
                msg_print("It feels a surge of energy!");

                project(0, 7, ty, tx, 50, GF_DISINTEGRATE, curse_flg);
                if (!one_in_(6)) break;
            case 17: case 18: case 19:
                aggravate_monsters(0);
                if (!one_in_(6)) break;
            case 20: case 21:
                count += activate_hi_summon(ty, tx, TRUE);
                if (!one_in_(6)) break;
            case 22: case 23: case 24: case 25: case 26:
            {
                bool pet = !one_in_(3);
                u32b mode = PM_ALLOW_GROUP;

                if (pet) mode |= PM_FORCE_PET;
                else mode |= (PM_NO_PET | PM_FORCE_FRIENDLY);

                count += summon_specific((pet ? -1 : 0), p_ptr->pos, (pet ? p_ptr->lev*2/3+randint1(p_ptr->lev/2) : cave->dun_lvl), 0, mode);
                if (!one_in_(6)) break;
            }
            case 27:
                if (p_ptr->hold_life && (randint0(100) < 75)) break;
                msg_print("You feel your life draining away...");

                if (p_ptr->hold_life) lose_exp(p_ptr->exp / 160);
                else lose_exp(p_ptr->exp / 16);
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
    if (mon->r_idx) update_mon(mon, FALSE);

    /* Redraw the monster grid */
    lite_pos(where);


    /* Update monster recall window */
    if (p_ptr->monster_race_idx == mon->r_idx && (seen || mon_is_dead(mon)))
    {
        /* Window stuff */
        p_ptr->window |= (PW_MONSTER);
    }

    if ((dam > 0) && !is_pet(mon) && !is_friendly(mon))
    {
        if (who == GF_WHO_PLAYER)
        {
            assert(GF_WHO_PLAYER == PROJECT_WHO_PLAYER);
            /* FYI: monster_target_* is generally the player, but
             * it gets changed for various mirror master effects. I
             * suppose this is a subtle touch to confuse monsters
             * but I never noticed this when I played one ... */
            set_target(mon, monster_target);
        }
        else if ((who > 0) && is_pet(caster_ptr) && !plr_at(mon->target))
        {
            set_target(mon, caster_ptr->pos);
        }
    }

    if (p_ptr->riding && (p_ptr->riding == mon->id) && (dam > 0))
    {
        if (mon->hp > mon->maxhp/3) dam = (dam + 1) / 2;
        rakubadam_m = (dam > 200) ? 200 : dam;
    }

    /* Return "Anything seen?" */
    return (obvious);
}

