
/* File: misc.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Purpose: misc code */

#include "angband.h"
#include "equip.h"

/*
 * Wrap calculation of AC bonuses from Dex
 */
static int calc_adj_dex_ta(void)
{
    return ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
}

/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(int val, char *out_val)
{
    /* Above 18 */
    if (val > 18)
    {
        int bonus = (val - 18);

        if (bonus >= 220)
        {
            sprintf(out_val, "18/%3s", "***");
        }
        else if (bonus >= 100)
        {
            sprintf(out_val, "18/%03d", bonus);
        }
        else
        {
            sprintf(out_val, " 18/%02d", bonus);
        }
    }

    /* From 3 to 18 */
    else
    {
        sprintf(out_val, "    %2d", val);
    }
}



/*
 * Modify a stat value by a "modifier", return new value
 *
 * Stats go up: 3,4,...,17,18,18/10,18/20,...,18/220
 * Or even: 18/13, 18/23, 18/33, ..., 18/220
 *
 * Stats go down: 18/220, 18/210,..., 18/10, 18, 17, ..., 3
 * Or even: 18/13, 18/03, 18, 17, ..., 3
 */
s16b modify_stat_value(int value, int amount)
{
    int    i;

    /* Reward */
    if (amount > 0)
    {
        /* Apply each point */
        for (i = 0; i < amount; i++)
        {
            /* One point at a time */
            if (value < 18) value++;

            /* Ten "points" at a time */
            else value += 10;
        }
    }

    /* Penalty */
    else if (amount < 0)
    {
        /* Apply each point */
        for (i = 0; i < (0 - amount); i++)
        {
            /* Ten points at a time */
            if (value >= 18+10) value -= 10;

            /* Hack -- prevent weirdness */
            else if (value > 18) value = 18;

            /* One point at a time */
            else if (value > 3) value--;
        }
    }

    /* Return new value */
    return (value);
}



/*
 * Print character info at given row, column in a 13 char field
 */
static void prt_field(cptr info, int row, int col)
{
    /* Dump 13 spaces to clear */
    c_put_str(TERM_WHITE, "             ", row, col);

    /* Dump the info itself */
    c_put_str(TERM_L_BLUE, info, row, col);
}


/*
 *  Whether daytime or not
 */
bool is_daytime(void)
{
    s32b len = TURNS_PER_TICK * TOWN_DAWN;
    if ((turn % len) < (len / 2))
        return TRUE;
    else
        return FALSE;
}

/*
 * Extract day, hour, min
 */
void extract_day_hour_min(int *day, int *hour, int *min)
{
    extract_day_hour_min_imp(turn, day, hour, min);
}
void extract_day_hour_min_imp(int turn, int *day, int *hour, int *min)
{
    const s32b A_DAY = TURNS_PER_TICK * TOWN_DAWN;
    s32b turn_in_today = (turn + A_DAY / 4) % A_DAY;

    switch (p_ptr->start_race)
    {
    case RACE_VAMPIRE:
    case RACE_MON_VAMPIRE:
    case RACE_SKELETON:
    case RACE_ZOMBIE:
    case RACE_SPECTRE:
        *day = (turn - A_DAY * 3 / 4) / A_DAY + 1;
        break;
    default:
        *day = (turn + A_DAY / 4) / A_DAY + 1;
        break;
    }
    *hour = (24 * turn_in_today / A_DAY) % 24;
    *min = (1440 * turn_in_today / A_DAY) % 60;
}

/*
 * Print time
 */
void prt_time(void)
{
    int day, hour, min;

    /* Dump 13 spaces to clear */
    c_put_str(TERM_WHITE, "             ", ROW_DAY, COL_DAY);

    extract_day_hour_min(&day, &hour, &min);

    /* Dump the info itself */
    if (day < 1000) c_put_str(TERM_WHITE, format("Day%3d", day), ROW_DAY, COL_DAY);
    else c_put_str(TERM_WHITE, "Day***", ROW_DAY, COL_DAY);

    c_put_str(TERM_WHITE, format("%2d:%02d", hour, min), ROW_DAY, COL_DAY+7);
}


cptr map_name(void)
{
    if (p_ptr->inside_quest && is_fixed_quest_idx(p_ptr->inside_quest)
        && (quest[p_ptr->inside_quest].flags & QUEST_FLAG_PRESET))
        return "Quest";
    else if (p_ptr->wild_mode)
        return "Surface";
    else if (p_ptr->inside_arena)
        return "Arena";
    else if (p_ptr->inside_battle)
        return "Monster Arena";
    else if (!dun_level && p_ptr->town_num)
        return town[p_ptr->town_num].name;
    else
        return d_name+d_info[dungeon_type].name;
}

/*
 * Print dungeon
 */
static void prt_dungeon(void)
{
    int col;
    char buf[100];

    /* Dump 13 spaces to clear */
    c_put_str(TERM_WHITE, "             ", ROW_DUNGEON, COL_DUNGEON);

    my_strcpy(buf, map_name(), 13);

    col = COL_DUNGEON + 6 - strlen(buf)/2;
    if (col < 0) col = 0;

    /* Dump the info itself */
    c_put_str(TERM_L_UMBER, buf,
          ROW_DUNGEON, col);
}




/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat)
{
    char tmp[32];

    /* Display "injured" stat */
    if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
    {
        put_str(stat_names_reduced[stat], ROW_STAT + stat, 0);
        cnv_stat(p_ptr->stat_use[stat], tmp);
        c_put_str(TERM_YELLOW, tmp, ROW_STAT + stat, COL_STAT + 6);
    }

    /* Display "healthy" stat */
    else
    {
        put_str(stat_names[stat], ROW_STAT + stat, 0);
        cnv_stat(p_ptr->stat_use[stat], tmp);
        c_put_str(TERM_L_GREEN, tmp, ROW_STAT + stat, COL_STAT + 6);
    }

    /* Indicate natural maximum */
    if (p_ptr->stat_max[stat] == p_ptr->stat_max_max[stat])
    {
        put_str("!", ROW_STAT + stat, 3);

    }
}


/*
 *  Data structure for status bar
 */
#define BAR_TSUYOSHI 0
#define BAR_HALLUCINATION 1
#define BAR_BLINDNESS 2
#define BAR_PARALYZE 3
#define BAR_CONFUSE 4
#define BAR_POISONED 5
#define BAR_AFRAID 6
#define BAR_LEVITATE 7
#define BAR_REFLECTION 8
#define BAR_PASSWALL 9
#define BAR_WRAITH 10
#define BAR_PROTEVIL 11
#define BAR_KAWARIMI 12
#define BAR_MAGICDEFENSE 13
#define BAR_EXPAND 14
#define BAR_STONESKIN 15
#define BAR_MULTISHADOW 16
#define BAR_REGMAGIC 17
#define BAR_ULTIMATE 18
#define BAR_INVULN 19
#define BAR_IMMACID 20
#define BAR_RESACID 21
#define BAR_IMMELEC 22
#define BAR_RESELEC 23
#define BAR_IMMFIRE 24
#define BAR_RESFIRE 25
#define BAR_IMMCOLD 26
#define BAR_RESCOLD 27
#define BAR_RESPOIS 28
#define BAR_RESNETH 29
#define BAR_RESTIME 30
#define BAR_DUSTROBE 31
#define BAR_SHFIRE 32
#define BAR_TOUKI 33
#define BAR_SHHOLY 34
#define BAR_EYEEYE 35
#define BAR_BLESSED 36
#define BAR_HEROISM 37
#define BAR_BERSERK 38
#define BAR_ATTKFIRE 39
#define BAR_ATTKCOLD 40
#define BAR_ATTKELEC 41
#define BAR_ATTKACID 42
#define BAR_ATTKPOIS 43
#define BAR_ATTKCONF 44
#define BAR_SENSEUNSEEN 45
#define BAR_TELEPATHY 46
#define BAR_REGENERATION 47
#define BAR_INFRAVISION 48
#define BAR_STEALTH 49
#define BAR_SUPERSTEALTH 50
#define BAR_RECALL 51
#define BAR_ALTER 52
#define BAR_SHCOLD 53
#define BAR_SHELEC 54
#define BAR_SHSHADOW 55
#define BAR_MIGHT 56
#define BAR_BUILD 57
#define BAR_ANTIMULTI 58
#define BAR_ANTITELE 59
#define BAR_ANTIMAGIC 60
#define BAR_PATIENCE 61
#define BAR_REVENGE 62
#define BAR_RUNESWORD 63
#define BAR_VAMPILIC 64
#define BAR_CURE 65
#define BAR_ESP_EVIL 66
#define BAR_SPEED_ESSENTIA 67
#define BAR_BLOOD_SHIELD 68
#define BAR_BLOOD_SEEK 69
#define BAR_BLOOD_REVENGE 70
#define BAR_BLOOD_SIGHT 71
#define BAR_BLOOD_FEAST 72
#define BAR_NO_SPELLS 73
#define BAR_TIME_SPURT 74
#define BAR_SPECIAL 75
#define BAR_DUELIST 76
#define BAR_SHOT_ON_THE_RUN 77
#define BAR_RAPID_SHOT 78
#define BAR_FLYING_DAGGER 79
#define BAR_SHADOW_STANCE 80
#define BAR_FRENZY_STANCE 81
#define BAR_GENJI 82
#define BAR_FORCE 83
#define BAR_COMBAT_EXPERTISE 84
#define BAR_STONE_BONES 85
#define BAR_TRADE_BLOWS 86
#define BAR_POWER_ATTACK 87
#define BAR_VICIOUS_STRIKE 88
#define BAR_BURNING_BLADE 89
#define BAR_ICE_BLADE 90
#define BAR_THUNDER_BLADE 91
#define BAR_BLOOD_BLADE 92
#define BAR_HOLY_BLADE 93
#define BAR_ORDER_BLADE 94
#define BAR_WILD_BLADE 95
#define BAR_MANY_STRIKE 96
#define BAR_PIERCING_STRIKE 97
#define BAR_TRIP 98
#define BAR_ENTRENCHED 99
#define BAR_ENLARGE_WEAPON 100
#define BAR_FLURRY_OF_BLOWS 101
#define BAR_GREATER_FLURRY 102
#define BAR_STRENGTH_OF_THE_UNDERTAKER 103
#define BAR_STOICISM 104
#define BAR_INDUSTRIOUS_MORTICIAN 105
#define BAR_SHIELD_BASH 106
#define BAR_BULWARK 107
#define BAR_BLOOD_RITE 108
#define BAR_WEAPON_GRAFT 109
#define BAR_PSIONIC_CLARITY 110
#define BAR_PSIONIC_BLENDING 111
#define BAR_PSIONIC_SHIELDING 112
#define BAR_PSIONIC_COMBAT 113
#define BAR_MENTAL_FORTRESS 114
#define BAR_MINDSPRING 115
#define BAR_PSIONIC_FORESIGHT 116
#define BAR_RES_DISENCHANTMENT 117
#define BAR_SPELL_REACTION 118
#define BAR_RESIST_CURSES 119
#define BAR_ARMOR_OF_FURY 120
#define BAR_SPELL_TURNING 121
#define BAR_FASTING 122
#define BAR_SUSTAIN_STR 123
#define BAR_SUSTAIN_INT 124
#define BAR_SUSTAIN_WIS 125
#define BAR_SUSTAIN_DEX 126
#define BAR_SUSTAIN_CON 127
#define BAR_SUSTAIN_CHR 128
#define BAR_HOLD_LIFE 129
#define BAR_TRANSCENDENCE 130
#define BAR_THE_WORLD 131
#define BAR_DARK_STALKER 132
#define BAR_NIMBLE_DODGE 133
#define BAR_STEALTHY_SNIPE 134
#define BAR_BLOCK 135
#define BAR_DRAIN 136
#define BAR_SHATTER 137
#define BAR_MAUL 138
#define BAR_KILLING_SPREE 139
#define BAR_TUNNEL 140
#define BAR_QUICK_WALK 141
#define BAR_INVEN_PROT 142
#define BAR_SHRIKE 143
#define BAR_WEAPONMASTERY 144
#define BAR_DEVICE_POWER 145
#define BAR_SPLATTER 146
#define BAR_SH_TIME 147
#define BAR_DBL_MOVE 148
#define BAR_READIED_SHOT 149
#define BAR_PIERCING_ARROW 150
#define BAR_RAPID_RELOAD 151
#define BAR_EXPLODING_BOLT 152
#define BAR_OVERDRAW 153
#define BAR_PSIONIC_ARCHERY 154
#define BAR_MYSTIC_STEALTH 155
#define BAR_MYSTIC_FAST 156
#define BAR_MYSTIC_RETALIATE 157
#define BAR_MYSTIC_OFFENSE 158
#define BAR_MYSTIC_DEFENSE 159
#define BAR_BLINK 160
#define BAR_DTRAP 161
#define BAR_DTRAP_EDGE 162
#define BAR_VAMPIRE_LIGHT 163
#define BAR_VAMPIRE_DARK  164
#define BAR_SH_SHARDS 165
#define BAR_SH_DOMINATION 166
#define BAR_PSIONIC_DISRUPTION 167
#define BAR_PSIONIC_DRAIN 168

static struct {
    byte attr;
    cptr sstr;
    cptr lstr;
} bar[]
= {
    {TERM_YELLOW, "Ts", "Tsuyoshi"},
    {TERM_VIOLET, "Ha", "Halluc"},
    {TERM_L_DARK, "Bl", "Blind"},
    {TERM_RED, "Pa", "Paralyzed"},
    {TERM_VIOLET, "Cf", "Confused"},
    {TERM_GREEN, "Po", "Poisoned"},
    {TERM_YELLOW, "Af", "Afraid"},
    {TERM_L_BLUE, "Lv", "Levit"},
    {TERM_SLATE, "Rf", "Reflect"},
    {TERM_SLATE, "Pw", "PassWall"},
    {TERM_L_DARK, "Wr", "Wraith"},
    {TERM_SLATE, "Ev", "PrtEvl"},
    {TERM_VIOLET, "Kw", "Kawarimi"},
    {TERM_YELLOW, "Md", "MgcArm"},
    {TERM_L_UMBER, "Eh", "Expand"},
    {TERM_WHITE, "Ss", "StnSkn"},
    {TERM_L_BLUE, "Ms", "MltShdw"},
    {TERM_SLATE, "Rm", "ResMag"},
    {TERM_YELLOW, "Ul", "Ultima"},
    {TERM_YELLOW, "Iv", "Invuln"},
    {TERM_L_GREEN, "IAc", "ImmAcid"},
    {TERM_GREEN, "Ac", "Acid"},
    {TERM_L_BLUE, "IEl", "ImmElec"},
    {TERM_BLUE, "El", "Elec"},
    {TERM_L_RED, "IFi", "ImmFire"},
    {TERM_RED, "Fi", "Fire"},
    {TERM_WHITE, "ICo", "ImmCold"},
    {TERM_SLATE, "Co", "Cold"},
    {TERM_GREEN, "Po", "Pois"},
    {TERM_L_DARK, "Nt", "Nthr"},
    {TERM_L_BLUE, "Ti", "Time"},
    {TERM_L_DARK, "Mr", "Mirr"},
    {TERM_L_RED, "SFi", "SFire"},
    {TERM_WHITE, "Fo", "Force"},
    {TERM_WHITE, "Ho", "Holy"},
    {TERM_VIOLET, "Ee", "EyeEye"},
    {TERM_WHITE, "Bs", "Bless"},
    {TERM_WHITE, "He", "Hero"},
    {TERM_RED, "Br", "Berserk"},
    {TERM_L_RED, "BFi", "BFire"},
    {TERM_WHITE, "BCo", "BCold"},
    {TERM_L_BLUE, "BEl", "BElec"},
    {TERM_SLATE, "BAc", "BAcid"},
    {TERM_L_GREEN, "BPo", "BPois"},
    {TERM_RED, "TCf", "TchCnf"},
    {TERM_L_BLUE, "Se", "SInv"},
    {TERM_ORANGE, "Te", "Telepa"},
    {TERM_L_BLUE, "Rg", "Regen"},
    {TERM_L_RED, "If", "Infr"},
    {TERM_UMBER, "Sl", "Stealth"},
    {TERM_YELLOW, "Stlt", "Stealth"},
    {TERM_WHITE, "Rc", "Recall"},
    {TERM_WHITE, "Al", "Alter"},
    /* Hex */
    {TERM_WHITE, "SCo", "SCold"},
    {TERM_BLUE, "SEl", "SElec"},
    {TERM_L_DARK, "SSh", "SShadow"},
    {TERM_YELLOW, "EMi", "ExMight"},
    {TERM_RED, "Gi", "Giant"},
    {TERM_L_DARK, "AMl", "AntiMulti"},
    {TERM_ORANGE, "AT", "AntiTele"},
    {TERM_RED, "AM", "AntiMagic"},
    {TERM_SLATE, "Pa", "Patience"},
    {TERM_SLATE, "Rv", "Revenge"},
    {TERM_L_DARK, "Rs", "RuneSword"},
    {TERM_RED, "Vm", "Vampiric"},
    {TERM_WHITE, "Cu", "Cure"},
    {TERM_L_DARK, "ET", "EvilTele"},
    {TERM_RED, "At", "Attacks"},
    {TERM_ORANGE, "Sh", "Shield"},
    {TERM_YELLOW, "Sk", "Seek"},
    {TERM_RED, "Rv", "Revenge"},
    {TERM_L_BLUE, "Si", "Sight"},
    {TERM_WHITE, "Fs", "Feast"},
    {TERM_VIOLET, "NS", "No Spells"},
    {TERM_YELLOW, "Q", "Quick"},
    {TERM_L_BLUE, "Sp", "Special"},
    {TERM_YELLOW, "", "Duelist Target Goes Here!"},
    {TERM_L_BLUE, "Rn", "Shoot on Run"},
    {TERM_L_BLUE, "Rp", "Rapid Shot"},
    {TERM_L_BLUE, "FD", "Flying Dagger"},
    {TERM_L_BLUE, "Sw", "Shadow"},
    {TERM_L_BLUE, "Fz", "Frenzy"},
    {TERM_YELLOW, "Gj", "Genji"},
    {TERM_L_BLUE, "Fc", "Force"},
    {TERM_L_BLUE, "DS", "Defensive Stance"},
    {TERM_UMBER, "SB", "Stone Bones"},
    {TERM_L_BLUE, "Tr", "Trade Blows"},
    {TERM_L_BLUE, "Pw", "Power Attack"},
    {TERM_RED, "Vs", "Exposed"},
    {TERM_RED, "BB", "Burning Blade"},
    {TERM_BLUE, "IB", "Ice Blade"},
    {TERM_YELLOW, "TB", "Thunder Blade"},
    {TERM_RED, "Bl", "Blood Blade"},
    {TERM_WHITE, "HB", "Holy Blade"},
    {TERM_ORANGE, "OB", "Order Blade"},
    {TERM_GREEN, "WB", "Wild Blade"},
    {TERM_L_BLUE, "MS", "Many Strike"},
    {TERM_L_BLUE, "PS", "Piercing Strike"},
    {TERM_L_BLUE, "Trp", "Trip"},
    {TERM_UMBER, "En", "Entrenched"},
    {TERM_RED, "EW", "Enlarge"},
    {TERM_L_RED, "Fl", "Flurry"},
    {TERM_RED, "Fl", "FLURRY"},
    {TERM_UMBER, "Str", "Undertaker"},
    {TERM_ORANGE, "Sc", "Stoicism"},
    {TERM_YELLOW, "At", "Mortician"},
    {TERM_L_BLUE, "SB", "Shield Bash"},
    {TERM_L_BLUE, "Bw", "Bulwark"},
    {TERM_RED, "Rt", "Rite"},
    {TERM_WHITE, "Gft", "Graft"},
    {TERM_YELLOW, "Cl", "Clarity"},
    {TERM_L_DARK, "Bl", "Blending"},
    {TERM_ORANGE, "Sh", "Shielding"},
    {TERM_RED, "Ct", "Combat"},
    {TERM_VIOLET, "Ft", "Fortress"},
    {TERM_GREEN, "MS", "Mindspring"},
    {TERM_YELLOW, "Fs", "Foresight"},
    {TERM_L_DARK, "Dis", "Disenchant"},
    {TERM_L_BLUE, "Rct", "Reaction"},
    {TERM_YELLOW, "RC", "Curses"},
    {TERM_RED, "Fy", "Fury"},
    {TERM_GREEN, "Tn", "Turning"},
    {TERM_GREEN, "Fs", "Fasting"},
    {TERM_YELLOW, "(Str", "SustStr"},
    {TERM_YELLOW, "(Int", "SustInt"},
    {TERM_YELLOW, "(Wis", "SustWis"},
    {TERM_YELLOW, "(Dex", "SustDex"},
    {TERM_YELLOW, "(Con", "SustCon"},
    {TERM_YELLOW, "(Chr", "SustChr"},
    {TERM_YELLOW, "(HL", "HLife"},
    {TERM_WHITE, "Tr", "Transcendence"},
    {TERM_L_BLUE, "ST", "StopTime"},
    {TERM_L_DARK, "DS", "Stealth"},
    {TERM_L_BLUE, "ND", "Dodge"},
    {TERM_UMBER, "SS", "Snipe"},
    {TERM_L_BLUE, "Bl", "Block"},
    {TERM_RED, "Dr", "Drain"},
    {TERM_YELLOW, "Qk", "Quake"},
    {TERM_RED, "Ml", "Maul"},
    {TERM_VIOLET, "KS", "Spree"},
    {TERM_L_DARK, "Tn", "Tunnel"},
    {TERM_YELLOW, "QW", "Quickwalk"},
    {TERM_L_BLUE, "IP", "InvenProt"},
    {TERM_YELLOW, "Sk", "Shrike"},
    {TERM_L_BLUE, "Wp", "Weapon"},
    {TERM_VIOLET, "Dv", "Device"},
    {TERM_RED, "*", "Splatter"},
    {TERM_L_BLUE, "ST", "STime"},
    {TERM_YELLOW, "2", "DblMove"},
    {TERM_L_BLUE, "RS", "Ready"},
    {TERM_L_BLUE, "PA", "Pierce"},
    {TERM_L_BLUE, "RR", "Reload"},
    {TERM_L_BLUE, "Ex", "Explode"},
    {TERM_L_BLUE, "OD", "Overdraw"},
    {TERM_RED, "Ay", "Archery"},
    {TERM_L_BLUE, "Sl", "Stealth"},
    {TERM_L_BLUE, "Fs", "Fast"},
    {TERM_L_BLUE, "Rl", "Retaliate"},
    {TERM_L_BLUE, "Of", "Offense"},
    {TERM_L_BLUE, "Df", "Defense"},
    {TERM_L_BLUE, "Bl", "Blink"},
    {TERM_L_GREEN, "DT", "DTrap"},
    {TERM_YELLOW, "DT", "DTrap"},
    {TERM_YELLOW, "Lt", "Light"},
    {TERM_L_DARK, "Dk", "Dark"},
    {TERM_UMBER, "SSh", "SShards"},
    {TERM_L_BLUE, "Dom", "Dominate"},
    {TERM_RED, "[M", "Disruption"},
    {TERM_YELLOW, "Dr", "Drain"},
    {0, NULL, NULL}
};

#define ADD_FLG(FLG) (bar_flags[FLG / 32] |= (1L << (FLG % 32)))
#define IS_FLG(FLG) (bar_flags[FLG / 32] & (1L << (FLG % 32)))


/*
 *  Show status bar
 */
static void prt_status(void)
{
    u32b bar_flags[7];
    int wid, hgt, row_statbar, max_col_statbar;
    int i, col = 0, num = 0;
    int space = 2;

    Term_get_size(&wid, &hgt);
    row_statbar = hgt + ROW_STATBAR;
    max_col_statbar = wid + MAX_COL_STATBAR;

    Term_erase(0, row_statbar, max_col_statbar);

    for (i = 0; i < 7; i++)
        bar_flags[i] = 0L;

    if (!view_unsafe_grids && in_bounds(py, px))
    {
        if (cave[py][px].info & CAVE_DETECT_EDGE)
            ADD_FLG(BAR_DTRAP_EDGE);
        else if (cave[py][px].info & CAVE_IN_DETECT)
            ADD_FLG(BAR_DTRAP);
    }

    if (prace_is_(RACE_MON_VAMPIRE))
    {
        if ((cave[py][px].info & (CAVE_GLOW | CAVE_MNDK)) == CAVE_GLOW)
            ADD_FLG(BAR_VAMPIRE_LIGHT);
        else
            ADD_FLG(BAR_VAMPIRE_DARK);
    }

    /* Tsuyoshi  */
    if (p_ptr->tsuyoshi) ADD_FLG(BAR_TSUYOSHI);

    /* Hallucinating */
    if (p_ptr->image) ADD_FLG(BAR_HALLUCINATION);

    /* Blindness */
    if (p_ptr->blind) ADD_FLG(BAR_BLINDNESS);

    /* Paralysis */
    if (p_ptr->paralyzed) ADD_FLG(BAR_PARALYZE);

    /* Confusion */
    if (p_ptr->confused) ADD_FLG(BAR_CONFUSE);

    /* Posioned */
    if (p_ptr->poisoned) ADD_FLG(BAR_POISONED);

    /* Times see-invisible */
    if (p_ptr->tim_invis) ADD_FLG(BAR_SENSEUNSEEN);

    /* Timed esp */
    if (IS_TIM_ESP()) ADD_FLG(BAR_TELEPATHY);

    /* Timed regenerate */
    if (p_ptr->tim_regen) ADD_FLG(BAR_REGENERATION);

    /* Timed infra-vision */
    if (IS_TIM_INFRA()) ADD_FLG(BAR_INFRAVISION);

    /* Protection from evil */
    if (IS_PROT_EVIL()) ADD_FLG(BAR_PROTEVIL);

    /* Invulnerability */
    if (IS_INVULN()) ADD_FLG(BAR_INVULN);

    /* Wraith form */
    if (IS_WRAITH()) ADD_FLG(BAR_WRAITH);

    /* Kabenuke */
    if (IS_PASSWALL()) ADD_FLG(BAR_PASSWALL);

    if (p_ptr->tim_reflect) ADD_FLG(BAR_REFLECTION);

    /* Heroism */
    if (IS_HERO()) ADD_FLG(BAR_HEROISM);

    /* Super Heroism / berserk */
    if (IS_SHERO()) ADD_FLG(BAR_BERSERK);

    /* Blessed */
    if (IS_BLESSED()) ADD_FLG(BAR_BLESSED);

    /* Shield */
    if (p_ptr->magicdef) ADD_FLG(BAR_MAGICDEFENSE);

    if (p_ptr->tsubureru) ADD_FLG(BAR_EXPAND);

    if (IS_STONE_SKIN()) ADD_FLG(BAR_STONESKIN);
    
    if (p_ptr->special_defense & NINJA_KAWARIMI) 
    {
        ADD_FLG(BAR_KAWARIMI);
        if (prace_is_(RACE_MON_SPIDER))
            bar[BAR_KAWARIMI].lstr = "PhaseShield";
    }

    /* Oppose Acid */
    if (p_ptr->special_defense & DEFENSE_ACID) ADD_FLG(BAR_IMMACID);
    if (IS_OPPOSE_ACID()) ADD_FLG(BAR_RESACID);

    /* Oppose Lightning */
    if (p_ptr->special_defense & DEFENSE_ELEC) ADD_FLG(BAR_IMMELEC);
    if (IS_OPPOSE_ELEC()) ADD_FLG(BAR_RESELEC);

    /* Oppose Fire */
    if (p_ptr->special_defense & DEFENSE_FIRE) ADD_FLG(BAR_IMMFIRE);
    if (IS_OPPOSE_FIRE()) ADD_FLG(BAR_RESFIRE);

    /* Oppose Cold */
    if (p_ptr->special_defense & DEFENSE_COLD) ADD_FLG(BAR_IMMCOLD);
    if (IS_OPPOSE_COLD()) ADD_FLG(BAR_RESCOLD);

    /* Oppose Poison */
    if (IS_OPPOSE_POIS()) ADD_FLG(BAR_RESPOIS);

    /* Word of Recall */
    if (p_ptr->word_recall) ADD_FLG(BAR_RECALL);

    /* Alter realiry */
    if (p_ptr->alter_reality) ADD_FLG(BAR_ALTER);

    /* Resist time */
    if (p_ptr->tim_res_time) ADD_FLG(BAR_RESTIME);

    if (p_ptr->multishadow) ADD_FLG(BAR_MULTISHADOW);

    /* Confusing Hands */
    if (p_ptr->special_attack & ATTACK_CONFUSE) ADD_FLG(BAR_ATTKCONF);

    if (IS_RESIST_MAGIC()) ADD_FLG(BAR_REGMAGIC);

    /* Ultimate-resistance */
    if (p_ptr->ult_res) ADD_FLG(BAR_ULTIMATE);

    /* tim levitation */
    if (p_ptr->tim_levitation) ADD_FLG(BAR_LEVITATE);

    if (p_ptr->tim_res_nether) ADD_FLG(BAR_RESNETH);
    if (p_ptr->tim_res_disenchantment) ADD_FLG(BAR_RES_DISENCHANTMENT);
    
    if (p_ptr->tim_spell_reaction) ADD_FLG(BAR_SPELL_REACTION);
    if (p_ptr->tim_resist_curses) ADD_FLG(BAR_RESIST_CURSES);
    if (p_ptr->tim_armor_of_fury) ADD_FLG(BAR_ARMOR_OF_FURY);
    if (p_ptr->tim_spell_turning) ADD_FLG(BAR_SPELL_TURNING);

    if (p_ptr->dustrobe) ADD_FLG(BAR_DUSTROBE);

    /* Mahouken */
    if (p_ptr->special_attack & ATTACK_FIRE) ADD_FLG(BAR_ATTKFIRE);
    if (p_ptr->special_attack & ATTACK_COLD) ADD_FLG(BAR_ATTKCOLD);
    if (p_ptr->special_attack & ATTACK_ELEC) ADD_FLG(BAR_ATTKELEC);
    if (p_ptr->special_attack & ATTACK_ACID) ADD_FLG(BAR_ATTKACID);
    if (p_ptr->special_attack & ATTACK_POIS) ADD_FLG(BAR_ATTKPOIS);
    if (p_ptr->special_defense & NINJA_S_STEALTH) ADD_FLG(BAR_SUPERSTEALTH);

    if (p_ptr->tim_sh_fire) ADD_FLG(BAR_SHFIRE);
    if (p_ptr->tim_sh_shards) ADD_FLG(BAR_SH_SHARDS);
    if (p_ptr->tim_sh_domination) ADD_FLG(BAR_SH_DOMINATION);
    if (p_ptr->tim_sh_elements)
    {
        ADD_FLG(BAR_SHFIRE);
        if (p_ptr->lev >= 25)    
            ADD_FLG(BAR_SHCOLD);
        if (p_ptr->lev >= 35)
            ADD_FLG(BAR_SHELEC);
    }
    if (p_ptr->tim_weaponmastery) ADD_FLG(BAR_WEAPONMASTERY);

    /* tim stealth */
    if (IS_TIM_STEALTH()) ADD_FLG(BAR_STEALTH);

    if (p_ptr->tim_sh_touki) ADD_FLG(BAR_TOUKI);

    /* Holy aura */
    if (p_ptr->tim_sh_holy) ADD_FLG(BAR_SHHOLY);

    /* An Eye for an Eye */
    if (IS_REVENGE()) ADD_FLG(BAR_EYEEYE);

    if (p_ptr->tim_spurt) ADD_FLG(BAR_TIME_SPURT);
    if (p_ptr->tim_speed_essentia) ADD_FLG(BAR_SPEED_ESSENTIA);
    if (p_ptr->tim_shrike) ADD_FLG(BAR_SHRIKE);
    if (p_ptr->tim_blood_shield) ADD_FLG(BAR_BLOOD_SHIELD);
    if (p_ptr->tim_blood_seek) ADD_FLG(BAR_BLOOD_SEEK);
    if (p_ptr->tim_blood_sight) ADD_FLG(BAR_BLOOD_SIGHT);
    if (p_ptr->tim_blood_feast) ADD_FLG(BAR_BLOOD_FEAST);
    if (p_ptr->tim_blood_rite) ADD_FLG(BAR_BLOOD_RITE);
    if (p_ptr->tim_no_spells) ADD_FLG(BAR_NO_SPELLS);
    if (p_ptr->tim_blood_revenge) ADD_FLG(BAR_BLOOD_REVENGE);
    if (p_ptr->tim_genji) ADD_FLG(BAR_GENJI);
    if (p_ptr->tim_force) ADD_FLG(BAR_FORCE);
    if (p_ptr->pclass == CLASS_WEAPONMASTER)
    {
        switch (weaponmaster_get_toggle())
        {
        case TOGGLE_SHOT_ON_THE_RUN:
            ADD_FLG(BAR_SHOT_ON_THE_RUN);
            break;
        case TOGGLE_RAPID_SHOT:
            ADD_FLG(BAR_RAPID_SHOT);
            break;
        case TOGGLE_FLYING_DAGGER_STANCE:
            ADD_FLG(BAR_FLYING_DAGGER);
            break;
        case TOGGLE_SHADOW_STANCE:
            ADD_FLG(BAR_SHADOW_STANCE);
            break;
        case TOGGLE_FRENZY_STANCE:
            ADD_FLG(BAR_FRENZY_STANCE);
            break;
        case TOGGLE_COMBAT_EXPERTISE:
            ADD_FLG(BAR_COMBAT_EXPERTISE);
            break;
        case TOGGLE_TRADE_BLOWS:
            ADD_FLG(BAR_TRADE_BLOWS);
            break;
        case TOGGLE_POWER_ATTACK:
            ADD_FLG(BAR_POWER_ATTACK);
            break;
        case TOGGLE_BURNING_BLADE:
            ADD_FLG(BAR_BURNING_BLADE);
            break;
        case TOGGLE_ICE_BLADE:
            ADD_FLG(BAR_ICE_BLADE);
            break;
        case TOGGLE_THUNDER_BLADE:
            ADD_FLG(BAR_THUNDER_BLADE);
            break;
        case TOGGLE_BLOOD_BLADE:
            ADD_FLG(BAR_BLOOD_BLADE);
            break;
        case TOGGLE_HOLY_BLADE:
            ADD_FLG(BAR_HOLY_BLADE);
            break;
        case TOGGLE_ORDER_BLADE:
            ADD_FLG(BAR_ORDER_BLADE);
            break;
        case TOGGLE_WILD_BLADE:
            ADD_FLG(BAR_WILD_BLADE);
            break;
        case TOGGLE_MANY_STRIKE:
            ADD_FLG(BAR_MANY_STRIKE);
            break;
        case TOGGLE_PIERCING_STRIKE:
            ADD_FLG(BAR_PIERCING_STRIKE);
            break;
        case TOGGLE_TRIP:
            ADD_FLG(BAR_TRIP);
            break;
        case TOGGLE_FLURRY_OF_BLOWS:
            ADD_FLG(BAR_FLURRY_OF_BLOWS);
            break;
        case TOGGLE_GREATER_FLURRY:
            ADD_FLG(BAR_GREATER_FLURRY);
            break;
        case TOGGLE_STRENGTH_OF_THE_UNDERTAKER:
            ADD_FLG(BAR_STRENGTH_OF_THE_UNDERTAKER);
            break;
        case TOGGLE_STOICISM:
            ADD_FLG(BAR_STOICISM);
            break;
        case TOGGLE_INDUSTRIOUS_MORTICIAN:
            ADD_FLG(BAR_INDUSTRIOUS_MORTICIAN);
            break;
        case TOGGLE_SHIELD_BASH:
            ADD_FLG(BAR_SHIELD_BASH);
            break;
        case TOGGLE_BULWARK:
            ADD_FLG(BAR_BULWARK);
            break;
        case TOGGLE_SHIELD_REVENGE:
            ADD_FLG(BAR_EYEEYE);
            break;
        case TOGGLE_READIED_SHOT:
            ADD_FLG(BAR_READIED_SHOT);
            break;
        case TOGGLE_PIERCING_ARROW:
            ADD_FLG(BAR_PIERCING_ARROW);
            break;
        case TOGGLE_RAPID_RELOAD:
            ADD_FLG(BAR_RAPID_RELOAD);
            break;
        case TOGGLE_EXPLODING_BOLT:
            ADD_FLG(BAR_EXPLODING_BOLT);
            break;
        case TOGGLE_OVERDRAW:
            ADD_FLG(BAR_OVERDRAW);
            break;
        }

        if (p_ptr->entrenched) ADD_FLG(BAR_ENTRENCHED);
    }

    if (p_ptr->pclass == CLASS_MYSTIC)
    {
        switch(mystic_get_toggle())
        {
        case MYSTIC_TOGGLE_STEALTH:
            ADD_FLG(BAR_MYSTIC_STEALTH);
            break;
        case MYSTIC_TOGGLE_FAST:
            ADD_FLG(BAR_MYSTIC_FAST);
            break;
        case MYSTIC_TOGGLE_RETALIATE:
            ADD_FLG(BAR_MYSTIC_RETALIATE);
            break;
        case MYSTIC_TOGGLE_OFFENSE:
            ADD_FLG(BAR_MYSTIC_OFFENSE);
            break;
        case MYSTIC_TOGGLE_DEFENSE:
            ADD_FLG(BAR_MYSTIC_DEFENSE);
            break;
        }
    }

    if (p_ptr->prace == RACE_MON_LEPRECHAUN)
    {
        switch(leprechaun_get_toggle())
        {
        case LEPRECHAUN_TOGGLE_BLINK:
            ADD_FLG(BAR_BLINK);
            break;
        }
    }
    if (p_ptr->prace == RACE_MON_POSSESSOR || p_ptr->prace == RACE_MON_MIMIC)
    {
        switch(possessor_get_toggle())
        {
        case LEPRECHAUN_TOGGLE_BLINK:
            ADD_FLG(BAR_BLINK);
            break;
        }
    }
    if (p_ptr->pclass == CLASS_MAULER)
    {
        switch (mauler_get_toggle())
        {
        case MAULER_TOGGLE_BLOCK:
            ADD_FLG(BAR_BLOCK);
            break;
        case MAULER_TOGGLE_DRAIN:
            ADD_FLG(BAR_DRAIN);
            break;
        case MAULER_TOGGLE_SHATTER:
            ADD_FLG(BAR_SHATTER);
            break;
        case MAULER_TOGGLE_SPLATTER:
            ADD_FLG(BAR_SPLATTER);
            break;
        case MAULER_TOGGLE_MAUL:
            ADD_FLG(BAR_MAUL);
            break;
        case MAULER_TOGGLE_TUNNEL:
            ADD_FLG(BAR_TUNNEL);
            break;
        }
    }
    if (p_ptr->pclass == CLASS_PSION)
    {
        if (psion_weapon_graft()) ADD_FLG(BAR_WEAPON_GRAFT);
        if (psion_clarity()) ADD_FLG(BAR_PSIONIC_CLARITY);
        if (psion_blending()) ADD_FLG(BAR_PSIONIC_BLENDING);
        if (psion_shielding()) ADD_FLG(BAR_PSIONIC_SHIELDING);
        if (psion_combat()) ADD_FLG(BAR_PSIONIC_COMBAT);
        if (psion_archery()) ADD_FLG(BAR_PSIONIC_ARCHERY);
        if (psion_mental_fortress()) ADD_FLG(BAR_MENTAL_FORTRESS);
        if (psion_mindspring()) ADD_FLG(BAR_MINDSPRING);
        if (psion_foresight()) ADD_FLG(BAR_PSIONIC_FORESIGHT);
        if (psion_disruption()) ADD_FLG(BAR_PSIONIC_DISRUPTION);
        if (psion_drain()) ADD_FLG(BAR_PSIONIC_DRAIN);
    }

    if (p_ptr->sense_artifact) ADD_FLG(BAR_SPECIAL);

    if (p_ptr->pclass == CLASS_DUELIST)
    {
        static char duelist_buffer[100];
        ADD_FLG(BAR_DUELIST);
        if (p_ptr->duelist_target_idx)
            bar[BAR_DUELIST].attr = TERM_YELLOW;
        else
            bar[BAR_DUELIST].attr = TERM_L_DARK;
        strnfmt(duelist_buffer, 100, "%^s", duelist_current_challenge());
        bar[BAR_DUELIST].lstr = duelist_buffer;
    }

    if (p_ptr->tim_building_up) ADD_FLG(BAR_BUILD);
    if (p_ptr->tim_vicious_strike) ADD_FLG(BAR_VICIOUS_STRIKE);
    if (p_ptr->tim_enlarge_weapon) ADD_FLG(BAR_ENLARGE_WEAPON);

    if (p_ptr->fasting) ADD_FLG(BAR_FASTING);
    if (p_ptr->tim_sustain_str) ADD_FLG(BAR_SUSTAIN_STR);
    if (p_ptr->tim_sustain_int) ADD_FLG(BAR_SUSTAIN_INT);
    if (p_ptr->tim_sustain_wis) ADD_FLG(BAR_SUSTAIN_WIS);
    if (p_ptr->tim_sustain_dex) ADD_FLG(BAR_SUSTAIN_DEX);
    if (p_ptr->tim_sustain_con) ADD_FLG(BAR_SUSTAIN_CON);
    if (p_ptr->tim_sustain_chr) ADD_FLG(BAR_SUSTAIN_CHR);
    if (p_ptr->tim_hold_life) ADD_FLG(BAR_HOLD_LIFE);
    if (p_ptr->tim_transcendence) ADD_FLG(BAR_TRANSCENDENCE);
    if (p_ptr->tim_quick_walk) ADD_FLG(BAR_QUICK_WALK);
    if (p_ptr->tim_inven_prot) ADD_FLG(BAR_INVEN_PROT);
    if (p_ptr->tim_device_power) ADD_FLG(BAR_DEVICE_POWER);
    if (p_ptr->tim_sh_time) ADD_FLG(BAR_SH_TIME);
    if (p_ptr->free_turns) ADD_FLG(BAR_DBL_MOVE);
    if (p_ptr->tim_foresight) ADD_FLG(BAR_PSIONIC_FORESIGHT);

    if (p_ptr->tim_dark_stalker) ADD_FLG(BAR_DARK_STALKER);
    if (p_ptr->tim_nimble_dodge) ADD_FLG(BAR_NIMBLE_DODGE);
    if (p_ptr->tim_stealthy_snipe) ADD_FLG(BAR_STEALTHY_SNIPE);

    if (p_ptr->tim_killing_spree) ADD_FLG(BAR_KILLING_SPREE);

    if (world_player) ADD_FLG(BAR_THE_WORLD);

    /* Hex spells */
    if (p_ptr->realm1 == REALM_HEX)
    {
        if (hex_spelling(HEX_BLESS)) ADD_FLG(BAR_BLESSED);
        if (hex_spelling(HEX_DEMON_AURA)) { ADD_FLG(BAR_SHFIRE); ADD_FLG(BAR_REGENERATION); }
        if (hex_spelling(HEX_XTRA_MIGHT)) ADD_FLG(BAR_MIGHT);
        if (hex_spelling(HEX_DETECT_EVIL)) ADD_FLG(BAR_ESP_EVIL);
        if (hex_spelling(HEX_ICE_ARMOR)) ADD_FLG(BAR_SHCOLD);
        if (hex_spelling(HEX_RUNESWORD)) ADD_FLG(BAR_RUNESWORD);
        if (hex_spelling(HEX_BUILDING)) ADD_FLG(BAR_BUILD);
        if (hex_spelling(HEX_ANTI_TELE)) ADD_FLG(BAR_ANTITELE);
        if (hex_spelling(HEX_SHOCK_CLOAK)) ADD_FLG(BAR_SHELEC);
        if (hex_spelling(HEX_SHADOW_CLOAK)) ADD_FLG(BAR_SHSHADOW);
        if (hex_spelling(HEX_CONFUSION)) ADD_FLG(BAR_ATTKCONF);
        if (hex_spelling(HEX_EYE_FOR_EYE)) ADD_FLG(BAR_EYEEYE);
        if (hex_spelling(HEX_ANTI_MULTI)) ADD_FLG(BAR_ANTIMULTI);
        if (hex_spelling(HEX_VAMP_BLADE)) ADD_FLG(BAR_VAMPILIC);
        if (hex_spelling(HEX_ANTI_MAGIC)) ADD_FLG(BAR_ANTIMAGIC);
        if (hex_spelling(HEX_CURE_LIGHT) ||
            hex_spelling(HEX_CURE_SERIOUS) ||
            hex_spelling(HEX_CURE_CRITICAL)) ADD_FLG(BAR_CURE);

        if (p_ptr->magic_num2[2])
        {
            if (p_ptr->magic_num2[1] == 1) ADD_FLG(BAR_PATIENCE);
            if (p_ptr->magic_num2[1] == 2) ADD_FLG(BAR_REVENGE);
        }
    }

    /* Calculate length */
    for (i = 0; bar[i].sstr; i++)
    {
        if (IS_FLG(i))
        {
            col += strlen(bar[i].lstr) + 1;
            num++;
        }
    }

    /* If there are not excess spaces for long strings, use short one */
    if (col - 1 > max_col_statbar)
    {
        space = 0;
        col = 0;

        for (i = 0; bar[i].sstr; i++)
        {
            if (IS_FLG(i))
            {
                col += strlen(bar[i].sstr);
            }
        }

        /* If there are excess spaces for short string, use more */
        if (col - 1 <= max_col_statbar - (num-1))
        {
            space = 1;
            col += num - 1;
        }
    }


    /* Centering display column */
    col = (max_col_statbar - col) / 2;

    /* Display status bar */
    for (i = 0; bar[i].sstr; i++)
    {
        if (IS_FLG(i))
        {
            cptr str;
            if (space == 2) str = bar[i].lstr;
            else str = bar[i].sstr;

            c_put_str(bar[i].attr, str, row_statbar, col);
            col += strlen(str);
            if (space > 0) col++;
            if (col > max_col_statbar) break;
        }
    }
}



/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(void)
{
    cptr p = "";
    char str[14];

    /* Wizard */
    if (p_ptr->wizard)
    {
        p = "[=-WIZARD-=]";

    }

    /* Winner */
    else if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
    {
        if (p_ptr->arena_number > MAX_ARENA_MONS + 2)
        {
            p = "*TRUEWINNER*";
        }
        else
        {
            p = "***WINNER***";
        }
    }

    /* Normal */
    else
    {
        my_strcpy(str, player_title[p_ptr->pclass][(p_ptr->lev - 1) / 5], sizeof(str));
        p = str;
    }

    prt_field(p, ROW_TITLE, COL_TITLE);
}


/*
 * Prints level
 */
static void prt_level(void)
{
    char tmp[32];

    sprintf(tmp, "%6d", p_ptr->lev);


    if (p_ptr->lev >= p_ptr->max_plv)
    {
        put_str("LEVEL ", ROW_LEVEL, 0);
        c_put_str(TERM_L_GREEN, tmp, ROW_LEVEL, COL_LEVEL + 6);

    }
    else
    {
        put_str("Level ", ROW_LEVEL, 0);
        c_put_str(TERM_YELLOW, tmp, ROW_LEVEL, COL_LEVEL + 6);

    }
}


/*
 * Display the experience
 */
static void prt_exp(void)
{
    char out_val[32];

    if ((!exp_need)||(p_ptr->prace == RACE_ANDROID))
    {
    (void)sprintf(out_val, "%8d", p_ptr->exp);
    }
    else
    {
        if (p_ptr->lev >= PY_MAX_LEVEL)
        {
            (void)sprintf(out_val, "********");
        }
        else
        {
            (void)sprintf(out_val, "%8d", exp_requirement(p_ptr->lev) - p_ptr->exp);
        }
    }

    if (p_ptr->exp >= p_ptr->max_exp)
    {
        if (p_ptr->prace == RACE_ANDROID) put_str("Cst ", ROW_EXP, 0);
        else put_str("EXP ", ROW_EXP, 0);
        c_put_str(TERM_L_GREEN, out_val, ROW_EXP, COL_EXP + 4);
    }
    else
    {
        put_str("Exp ", ROW_EXP, 0);
        c_put_str(TERM_YELLOW, out_val, ROW_EXP, COL_EXP + 4);
    }
}

/*
 * Prints current gold
 */
static void prt_gold(void)
{
    char tmp[32];

    put_str("AU ", ROW_GOLD, COL_GOLD);

    sprintf(tmp, "%9d", p_ptr->au);
    c_put_str(TERM_L_GREEN, tmp, ROW_GOLD, COL_GOLD + 3);
}



/*
 * Prints current AC
 */
static void prt_ac(void)
{
    char tmp[32];

    put_str("AC ", ROW_AC, COL_AC);
    sprintf(tmp, "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
    c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_AC + 7);

}

/*
 * Prints Cur/Max hit points
 */
static void prt_hp(void)
{
    char tmp[32];
  
    byte color;
  

    put_str("HP", ROW_CURHP, COL_CURHP);

    sprintf(tmp, "%4d", p_ptr->chp);

    if (p_ptr->chp >= p_ptr->mhp)
    {
        color = TERM_L_GREEN;
    }
    else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10)
    {
        color = TERM_YELLOW;
    }
    else
    {
        color = TERM_RED;
    }

    c_put_str(color, tmp, ROW_CURHP, COL_CURHP+3);

    put_str( "/", ROW_CURHP, COL_CURHP + 7 );

    sprintf(tmp, "%4d", p_ptr->mhp);
    color = TERM_L_GREEN;

    c_put_str(color, tmp, ROW_CURHP, COL_CURHP + 8 );
}


/*
 * Prints players max/cur spell points
 */
static void prt_sp(void)
{
    char tmp[32];
    byte color;

    put_str("SP", ROW_CURSP, COL_CURSP);
    sprintf(tmp, "%4d", p_ptr->csp);
    if (p_ptr->csp >= p_ptr->msp)
    {
        color = TERM_L_GREEN;
    }
    else if (p_ptr->csp > (p_ptr->msp * mana_warn) / 10)
    {
        color = TERM_YELLOW;
    }
    else
    {
        color = TERM_RED;
    }

    c_put_str(color, tmp, ROW_CURSP, COL_CURSP+3);

    put_str( "/", ROW_CURSP, COL_CURSP + 7 );

    sprintf(tmp, "%4d", p_ptr->msp);
    color = TERM_L_GREEN;

    c_put_str(color, tmp, ROW_CURSP, COL_CURSP + 8);
}

static void prt_fear(void)
{
    int lvl = fear_level_p();

    switch (lvl)
    {
    case FEAR_BOLD:
        put_str(               "          ", ROW_FEAR, COL_FEAR);
        break;
    case FEAR_UNEASY:
        c_put_str(TERM_L_UMBER,"Uneasy    ", ROW_FEAR, COL_FEAR);
        break;
    case FEAR_NERVOUS:
        c_put_str(TERM_YELLOW, "Nervous   ", ROW_FEAR, COL_FEAR);
        break;
    case FEAR_SCARED:
        c_put_str(TERM_ORANGE, "Scared    ", ROW_FEAR, COL_FEAR);
        break;
    case FEAR_TERRIFIED:
        c_put_str(TERM_RED,    "Terrified ", ROW_FEAR, COL_FEAR);
        break;
    case FEAR_PETRIFIED:
        c_put_str(TERM_VIOLET, "Petrified ", ROW_FEAR, COL_FEAR);
        break;
    }
}


/*
 * Prints depth in stat area
 */
static void prt_depth(void)
{
    char depths[32];
    int wid, hgt, row_depth, col_depth;
    byte attr = TERM_WHITE;

    Term_get_size(&wid, &hgt);
    col_depth = wid + COL_DEPTH;
    row_depth = hgt + ROW_DEPTH;

    if (!dun_level)
    {
        strcpy(depths, "Surf.");
    }
    else if (p_ptr->inside_quest && !dungeon_type)
    {
        strcpy(depths, "Quest");
    }
    else
    {
        if (depth_in_feet) (void)sprintf(depths, "%d ft", dun_level * 50);
        else (void)sprintf(depths, "Lev %d", dun_level);


        /* Get color of level based on feeling  -JSV- */
        switch (p_ptr->feeling)
        {
        case  0: attr = TERM_SLATE;   break; /* Unknown */
        case  1: attr = TERM_L_BLUE;  break; /* Special */
        case  2: attr = TERM_VIOLET;  break; /* Horrible visions */
        case  3: attr = TERM_RED;     break; /* Very dangerous */
        case  4: attr = TERM_L_RED;   break; /* Very bad feeling */
        case  5: attr = TERM_ORANGE;  break; /* Bad feeling */
        case  6: attr = TERM_YELLOW;  break; /* Nervous */
        case  7: attr = TERM_L_UMBER; break; /* Luck is turning */
        case  8: attr = TERM_L_WHITE; break; /* Don't like */
        case  9: attr = TERM_WHITE;   break; /* Reasonably safe */
        case 10: attr = TERM_WHITE;   break; /* Boring place */
        }
    }

    /* Right-Adjust the "depth", and clear old values */
    c_prt(attr, format("%7s", depths), row_depth, col_depth);
}

static void food_redraw(void)
{
    byte attr;
    int  pct;
    int  len;

    if (!display_food_bar) /* user might toggle option on and off ... */
    {
        Term_erase(COL_FOOD, ROW_FOOD, 12);
        Term_erase(COL_FOOD, ROW_FOOD + 1, 12);
        return;
    }

    pct = 100 * p_ptr->food / PY_FOOD_FULL;
    len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

    if (pct >= 100) attr = TERM_L_GREEN;
    else if (pct >= 50) attr = TERM_WHITE;
    else if (pct >= 30) attr = TERM_YELLOW;
    else if (pct >= 20) attr = TERM_ORANGE;
    else if (pct >= 10) attr = TERM_L_RED;
    else attr = TERM_VIOLET;

    Term_putstr(COL_FOOD, ROW_FOOD, 5, TERM_WHITE, "Food:");
    Term_putstr(COL_FOOD, ROW_FOOD + 1, 12, attr, "[----------]");
    Term_putstr(COL_FOOD + 1, ROW_FOOD + 1, len, attr, "**********");
}


/*
 * Prints status of hunger
 */
static void prt_hunger(void)
{
    food_redraw();

    /* Fainting / Starving */
    if (p_ptr->food < PY_FOOD_FAINT)
    {
        c_put_str(TERM_RED, "Weak  ", ROW_HUNGRY, COL_HUNGRY);

    }

    /* Weak */
    else if (p_ptr->food < PY_FOOD_WEAK)
    {
        c_put_str(TERM_ORANGE, "Weak  ", ROW_HUNGRY, COL_HUNGRY);

    }

    /* Hungry */
    else if (p_ptr->food < PY_FOOD_ALERT)
    {
        c_put_str(TERM_YELLOW, "Hungry", ROW_HUNGRY, COL_HUNGRY);

    }

    /* Normal */
    else if (p_ptr->food < PY_FOOD_FULL)
    {
        c_put_str(TERM_L_GREEN, "      ", ROW_HUNGRY, COL_HUNGRY);
    }

    /* Full */
    else if (p_ptr->food < PY_FOOD_MAX)
    {
        c_put_str(TERM_L_GREEN, "Full  ", ROW_HUNGRY, COL_HUNGRY);

    }

    /* Gorged */
    else
    {
        c_put_str(TERM_GREEN, "Gorged", ROW_HUNGRY, COL_HUNGRY);

    }
}


/*
 * Prints Searching, Resting, Paralysis, or 'count' status
 * Display is always exactly 10 characters wide (see below)
 *
 * This function was a major bottleneck when resting, so a lot of
 * the text formatting code was optimized in place below.
 */
static void prt_state(void)
{
    byte attr = TERM_WHITE;

    char text[20];

    /* Repeating */
    if (command_rep)
    {
        if (command_rep > 999)
        {
            (void)sprintf(text, "%2d00", command_rep / 100);

        }
        else
        {
            (void)sprintf(text, "  %2d", command_rep);

        }
    }

    /* Action */
    else
    {
        switch(p_ptr->action)
        {
            case ACTION_GLITTER:
            {
                strcpy(text, "Lure");
                break;
            }
            case ACTION_SEARCH:
            {
                strcpy(text, "Sear");
                break;
            }
            case ACTION_REST:
            {
                int i;

                /* Start with "Rest" */
                strcpy(text, "    ");


                /* Extensive (timed) rest */
                if (resting >= 1000)
                {
                    i = resting / 100;
                    text[3] = '0';
                    text[2] = '0';
                    text[1] = '0' + (i % 10);
                    text[0] = '0' + (i / 10);
                }

                /* Long (timed) rest */
                else if (resting >= 100)
                {
                    i = resting;
                    text[3] = '0' + (i % 10);
                    i = i / 10;
                    text[2] = '0' + (i % 10);
                    text[1] = '0' + (i / 10);
                }

                /* Medium (timed) rest */
                else if (resting >= 10)
                {
                    i = resting;
                    text[3] = '0' + (i % 10);
                    text[2] = '0' + (i / 10);
                }

                /* Short (timed) rest */
                else if (resting > 0)
                {
                    i = resting;
                    text[3] = '0' + (i);
                }

                /* Rest until healed */
                else if (resting == -1)
                {
                    text[0] = text[1] = text[2] = text[3] = '*';
                }

                /* Rest until done */
                else if (resting == -2)
                {
                    text[0] = text[1] = text[2] = text[3] = '&';
                }
                break;
            }
            case ACTION_LEARN:
            {
                strcpy(text, "Lear");
                if (new_mane) attr = TERM_L_RED;
                break;
            }
            case ACTION_FISH:
            {
                strcpy(text, "Fish");
                break;
            }
            case ACTION_KAMAE:
            {
                int i;
                for (i = 0; i < MAX_KAMAE; i++)
                    if (p_ptr->special_defense & (KAMAE_GENBU << i)) break;
                switch (i)
                {
                    case 0: attr = TERM_GREEN;break;
                    case 1: attr = TERM_WHITE;break;
                    case 2: attr = TERM_L_BLUE;break;
                    case 3: attr = TERM_L_RED;break;
                }
                strcpy(text, kamae_shurui[i].desc);
                break;
            }
            case ACTION_KATA:
            {
                int i;
                for (i = 0; i < MAX_KATA; i++)
                    if (p_ptr->special_defense & (KATA_IAI << i)) break;
                strcpy(text, kata_shurui[i].desc);
                break;
            }
            case ACTION_SING:
            {
                strcpy(text, "Sing");
                break;
            }
            case ACTION_QUICK_WALK:
            {
                strcpy(text, "Fast");
                break;
            }
            case ACTION_SPELL:
            {
                strcpy(text, "Spel");
                break;
            }
            case ACTION_STALK:
            {
                strcpy(text, "Stlk");
                break;
            }
            default:
            {
                strcpy(text, "    ");
                break;
            }
        }
    }

    /* Display the info (or blanks) */
    c_put_str(attr, format("%5.5s",text), ROW_STATE, COL_STATE);
}


/*
 * Prints the speed of a character.            -CJS-
 */
static void prt_speed(void)
{
    int i = p_ptr->pspeed;
    bool is_fast = IS_FAST();

    byte attr = TERM_WHITE;
    char buf[32] = "";
    int wid, hgt, row_speed, col_speed;

    Term_get_size(&wid, &hgt);
    col_speed = wid + COL_SPEED;
    row_speed = hgt + ROW_SPEED;

    /* Hack -- Visually "undo" the Search Mode Slowdown */
    if (p_ptr->action == ACTION_SEARCH && !IS_LIGHT_SPEED()) i += 10;

    /* Fast */
    if (i > 110)
    {
        if (p_ptr->riding)
        {
            monster_type *m_ptr = &m_list[p_ptr->riding];
            if (MON_FAST(m_ptr) && !MON_SLOW(m_ptr)) attr = TERM_L_BLUE;
            else if (MON_SLOW(m_ptr) && !MON_FAST(m_ptr)) attr = TERM_VIOLET;
            else attr = TERM_GREEN;
        }
        else if ((is_fast && !p_ptr->slow) || IS_LIGHT_SPEED() || psion_speed()) attr = TERM_YELLOW;
        else if (p_ptr->slow && !is_fast) attr = TERM_VIOLET;
        else attr = TERM_L_GREEN;
        sprintf(buf, "Fast(+%d)", (i - 110));

    }

    /* Slow */
    else if (i < 110)
    {
        if (p_ptr->riding)
        {
            monster_type *m_ptr = &m_list[p_ptr->riding];
            if (MON_FAST(m_ptr) && !MON_SLOW(m_ptr)) attr = TERM_L_BLUE;
            else if (MON_SLOW(m_ptr) && !MON_FAST(m_ptr)) attr = TERM_VIOLET;
            else attr = TERM_RED;
        }
        else if (is_fast && !p_ptr->slow) attr = TERM_YELLOW;
        else if (p_ptr->slow && !is_fast) attr = TERM_VIOLET;
        else attr = TERM_L_UMBER;
        sprintf(buf, "Slow(-%d)", (110 - i));
    }
    else if (p_ptr->riding)
    {
        attr = TERM_GREEN;
        strcpy(buf, "Riding");
    }

    /* Display the speed */
    c_put_str(attr, format("%-9s", buf), row_speed, col_speed);
}


static void prt_study(void)
{
    int wid, hgt, row_study, col_study;

    Term_get_size(&wid, &hgt);
    col_study = wid + COL_STUDY;
    row_study = hgt + ROW_STUDY;

    if (p_ptr->new_spells)
    {
        put_str("Stud", row_study, col_study);

    }
    else
    {
        put_str("    ", row_study, col_study);
    }
}


static void prt_imitation(void)
{
    int wid, hgt, row_study, col_study;

    Term_get_size(&wid, &hgt);
    col_study = wid + COL_STUDY;
    row_study = hgt + ROW_STUDY;

    if (p_ptr->pclass == CLASS_IMITATOR)
    {
        if (p_ptr->mane_num)
        {
            byte attr;
            if (new_mane) attr = TERM_L_RED;
            else attr = TERM_WHITE;
            c_put_str(attr, "Imit", row_study, col_study);
        }
        else
        {
            put_str("    ", row_study, col_study);
        }
    }
}


static void prt_cut(void)
{
    int c = p_ptr->cut;

    if (c > 1000)
    {
        c_put_str(TERM_L_RED, "Mortal wound", ROW_CUT, COL_CUT);

    }
    else if (c > 200)
    {
        c_put_str(TERM_RED, "Deep gash   ", ROW_CUT, COL_CUT);

    }
    else if (c > 100)
    {
        c_put_str(TERM_RED, "Severe cut  ", ROW_CUT, COL_CUT);

    }
    else if (c > 50)
    {
        c_put_str(TERM_ORANGE, "Nasty cut   ", ROW_CUT, COL_CUT);

    }
    else if (c > 25)
    {
        c_put_str(TERM_ORANGE, "Bad cut     ", ROW_CUT, COL_CUT);

    }
    else if (c > 10)
    {
        c_put_str(TERM_YELLOW, "Light cut   ", ROW_CUT, COL_CUT);

    }
    else if (c)
    {
        c_put_str(TERM_YELLOW, "Graze       ", ROW_CUT, COL_CUT);

    }
    else
    {
        put_str("            ", ROW_CUT, COL_CUT);
    }
}



static void prt_stun(void)
{
    int s = p_ptr->stun;

    if (s > 100)
    {
        c_put_str(TERM_RED, "Knocked out ", ROW_STUN, COL_STUN);

    }
    else if (s > 50)
    {
        c_put_str(TERM_ORANGE, "Heavy stun  ", ROW_STUN, COL_STUN);

    }
    else if (s)
    {
        c_put_str(TERM_ORANGE, "Stun        ", ROW_STUN, COL_STUN);

    }
    else
    {
        put_str("            ", ROW_STUN, COL_STUN);
    }
}



/*
 * Redraw the "monster health bar"    -DRS-
 * Rather extensive modifications by    -BEN-
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targetting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.
 *
 * Display the monster health bar (affectionately known as the
 * "health-o-meter").  Clear health bar if nothing is being tracked.
 * Auto-track current target monster when bored.  Note that the
 * health-bar stops tracking any monster that "disappears".
 */
static void health_redraw(bool riding)
{
    s16b health_who;
    int row, col;
    monster_type *m_ptr;

    if (riding)
    {
        health_who = p_ptr->riding;
        row = ROW_RIDING_INFO;
        col = COL_RIDING_INFO;
    }
    else
    {
        health_who = p_ptr->health_who;
        row = ROW_INFO;
        col = COL_INFO;
    }

    m_ptr = &m_list[health_who];

    /* Not tracking */
    if (!health_who)
    {
        /* Erase the health bar */
        Term_erase(col, row, 12);
    }

    /* Tracking an unseen monster */
    else if (!m_ptr->ml)
    {
        /* Indicate that the monster health is "unknown" */
        Term_putstr(col, row, 12, TERM_WHITE, "[----------]");
    }

    /* Tracking a hallucinatory monster */
    else if (p_ptr->image)
    {
        /* Indicate that the monster health is "unknown" */
        Term_putstr(col, row, 12, TERM_WHITE, "[----------]");
    }

    /* Tracking a dead monster (???) */
    else if (m_ptr->hp < 0)
    {
        /* Indicate that the monster health is "unknown" */
        Term_putstr(col, row, 12, TERM_WHITE, "[----------]");
    }

    /* Tracking a visible monster */
    else if (m_ptr->maxhp > 0 && m_ptr->max_maxhp > 0)
    {
        /* Extract the "percent" of health */
        int pct = 100L * m_ptr->hp / m_ptr->maxhp;
        int pct2 = 100L * m_ptr->hp / m_ptr->max_maxhp;

        /* Convert percent into "health" */
        int len = (pct2 < 10) ? 1 : (pct2 < 90) ? (pct2 / 10 + 1) : 10;

        /* Default to almost dead */
        byte attr = TERM_RED;

        if (MON_INVULNER(m_ptr)) attr = TERM_WHITE;
        else if (m_ptr->paralyzed) attr = TERM_BLUE;
        else if (MON_CSLEEP(m_ptr)) attr = TERM_BLUE;
        else if (MON_STUNNED(m_ptr)) attr = TERM_L_BLUE;
        else if (MON_CONFUSED(m_ptr)) attr = TERM_UMBER;
        else if (MON_MONFEAR(m_ptr)) attr = TERM_VIOLET;
        else if (pct >= 100) attr = TERM_L_GREEN;
        else if (pct >= 60) attr = TERM_YELLOW;
        else if (pct >= 25) attr = TERM_ORANGE;
        else if (pct >= 10) attr = TERM_L_RED;

        Term_putstr(col, row, 12, TERM_WHITE, "[----------]");

        if (m_ptr->ego_whip_ct)
            Term_putstr(col + 1, row, len, attr, "wwwwwwwwww");
        else
            Term_putstr(col + 1, row, len, attr, "**********");
    }
}

/*
 * Display basic info (mostly left of map)
 */
static void prt_frame_basic(void)
{
    int i;

    /* Race and Class */
    {
        char buf1[100];
        char buf2[100];
        race_t *race_ptr = get_race_t();
        if (race_ptr->mimic)
        {
            sprintf(buf1, "[%s]", race_ptr->name);
            my_strcpy(buf2, buf1, 13);
        }
        else
            my_strcpy(buf2, race_ptr->name, 13);

        prt_field(buf2, ROW_RACE, COL_RACE);
    }


    /* Title */
    prt_title();

    /* Level/Experience */
    prt_level();
    prt_exp();

    /* All Stats */
    for (i = 0; i < 6; i++) prt_stat(i);

    /* Armor */
    prt_ac();

    /* Hitpoints */
    prt_hp();

    /* Spellpoints */
    prt_sp();

    /* Gold */
    prt_gold();

    /* Current depth */
    prt_depth();

    /* Special */
    health_redraw(FALSE);
    health_redraw(TRUE);
}


/*
 * Display extra info (mostly below map)
 */
static void prt_frame_extra(void)
{
    /* Cut/Stun */
    prt_cut();
    prt_stun();
    prt_fear();

    /* Food */
    prt_hunger();

    /* State */
    prt_state();

    /* Speed */
    prt_speed();

    /* Study spells */
    prt_study();

    prt_imitation();

    prt_status();
}


/*
 * Hack -- display inventory in sub-windows
 */
static void fix_inven(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_INVEN))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Display inventory */
        display_inven();

        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}



/*
 * Hack -- display equipment in sub-windows
 */
static void fix_equip(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_EQUIP))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Display equipment */
        display_equip();

        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}


/*
 * Hack -- display equipment in sub-windows
 */
static void fix_spell(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_SPELL))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Display spell list */
        /* display_spell_list();*/

        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}


/*
 * Hack -- display character in sub-windows
 */
static void fix_player(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_PLAYER))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        update_playtime();

        /* Display player */
        display_player(0);

        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}



/*
 * Hack -- display recent messages in sub-windows
 *
 * XXX XXX XXX Adjust for width and split messages
 */
static void fix_message(void)
{
    int j, i;
    int w, h;
    int x, y;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_MESSAGE))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Get size */
        Term_get_size(&w, &h);

        /* Dump messages */
        for (i = 0; i < h; i++)
        {
            /* Dump the message on the appropriate line */
            Term_putstr(0, (h - 1) - i, -1, (byte)((i < now_message) ? TERM_WHITE : TERM_SLATE), message_str((s16b)i));

            /* Cursor */
            Term_locate(&x, &y);

            /* Clear to end of line */
            Term_erase(x, y, 255);
        }

        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}


/*
 * Hack -- display overhead view in sub-windows
 *
 * Note that the "player" symbol does NOT appear on the map.
 */
static void fix_overhead(void)
{
    int j;

    int cy, cx;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;
        int wid, hgt;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_OVERHEAD))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Full map in too small window is useless  */
        Term_get_size(&wid, &hgt);
        if (wid > COL_MAP + 2 && hgt > ROW_MAP + 2)
        {
            /* Redraw map */
            display_map(&cy, &cx);

            /* Fresh */
            Term_fresh();
        }

        /* Restore */
        Term_activate(old);
    }
}


/*
 * Hack -- display dungeon view in sub-windows
 */
static void fix_dungeon(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_DUNGEON))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Redraw dungeon view */
        display_dungeon();

        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}


/*
 * Hack -- display monster recall in sub-windows
 */
static void fix_monster(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_MONSTER))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Display monster race info */
        if (p_ptr->monster_race_idx) display_roff(p_ptr->monster_race_idx);

        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}


/*
 * Hack -- display object recall in sub-windows
 */
static void fix_object(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* No window */
        if (!angband_term[j]) continue;

        /* No relevant flags */
        if (!(window_flag[j] & (PW_OBJECT))) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Display monster race info */
        if (p_ptr->object_kind_idx) display_koff(p_ptr->object_kind_idx);

        /* Fresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}


/*
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 */
static void calc_spells(void)
{
    int            i, j, k, levels;
    int            num_allowed;
    int                     num_boukyaku = 0;

    magic_type        *s_ptr;
    int which;
    int bonus = 0;


    cptr p;

    /* Hack -- must be literate */
    if (!mp_ptr->spell_book) return;

    /* Hack -- wait for creation */
    if (!character_generated) return;

    /* Hack -- handle "xtra" mode */
    if (character_xtra) return;

    if ( p_ptr->pclass == CLASS_SORCERER
      || p_ptr->pclass == CLASS_RED_MAGE )
    {
        p_ptr->new_spells = 0;
        return;
    }

    p = spell_category_name(mp_ptr->spell_book);

    /* Determine the number of spells allowed */
    levels = p_ptr->lev - mp_ptr->spell_first + 1;

    /* Hack -- no negative spells */
    if (levels < 0) levels = 0;

    /* Extract total allowed spells */
    num_allowed = (adj_mag_study[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 2);

    if ((p_ptr->pclass != CLASS_SAMURAI) && (mp_ptr->spell_book != TV_LIFE_BOOK))
    {
        bonus = 4;
    }
    if (p_ptr->pclass == CLASS_SAMURAI || p_ptr->pclass == CLASS_RAGE_MAGE)
    {
        num_allowed = 32;
    }
    else if (p_ptr->realm2 == REALM_NONE)
    {
        num_allowed = (num_allowed+1)/2;
        if (num_allowed>(32+bonus)) num_allowed = 32+bonus;
    }
    else if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_BLOOD_MAGE) || (p_ptr->pclass == CLASS_PRIEST))
    {
        if (num_allowed>(96+bonus)) num_allowed = 96+bonus;
    }
    else
    {
        if (num_allowed>(80+bonus)) num_allowed = 80+bonus;
    }

    /* Count the number of spells we know */
    for (j = 0; j < 64; j++)
    {
        /* Count known spells */
        if ((j < 32) ?
            (p_ptr->spell_forgotten1 & (1L << j)) :
            (p_ptr->spell_forgotten2 & (1L << (j - 32))))
        {
            num_boukyaku++;
        }
    }

    /* See how many spells we must forget or may learn */
    p_ptr->new_spells = num_allowed + p_ptr->add_spells + num_boukyaku - p_ptr->learned_spells;

    /* Forget spells which are too hard */
    for (i = 63; i >= 0; i--)
    {
        /* Efficiency -- all done */
        if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2) break;
        if (p_ptr->pclass == CLASS_RAGE_MAGE) break;

        /* Access the spell */
        j = p_ptr->spell_order[i];

        /* Skip non-spells */
        if (j >= 99) continue;


        /* Get the spell */
        if (!is_magic((j < 32) ? p_ptr->realm1 : p_ptr->realm2))
        {
            if (j < 32)
                s_ptr = &technic_info[p_ptr->realm1 - MIN_TECHNIC][j];
            else
                s_ptr = &technic_info[p_ptr->realm2 - MIN_TECHNIC][j%32];
        }
        else if (j < 32)
            s_ptr = &mp_ptr->info[p_ptr->realm1-1][j];
        else
            s_ptr = &mp_ptr->info[p_ptr->realm2-1][j%32];

        /* Skip spells we are allowed to know */
        if (s_ptr->slevel <= p_ptr->lev) continue;

        /* Is it known? */
        if ((j < 32) ?
            (p_ptr->spell_learned1 & (1L << j)) :
            (p_ptr->spell_learned2 & (1L << (j - 32))))
        {
            /* Mark as forgotten */
            if (j < 32)
            {
                p_ptr->spell_forgotten1 |= (1L << j);
                which = p_ptr->realm1;
            }
            else
            {
                p_ptr->spell_forgotten2 |= (1L << (j - 32));
                which = p_ptr->realm2;
            }

            /* No longer known */
            if (j < 32)
            {
                p_ptr->spell_learned1 &= ~(1L << j);
                which = p_ptr->realm1;
            }
            else
            {
                p_ptr->spell_learned2 &= ~(1L << (j - 32));
                which = p_ptr->realm2;
            }

            /* Message */
            msg_format("You have forgotten the %s of %s.", p,
            do_spell(which, j%32, SPELL_NAME));


            /* One more can be learned */
            p_ptr->new_spells++;
        }
    }


    /* Forget spells if we know too many spells */
    for (i = 63; i >= 0; i--)
    {
        /* Stop when possible */
        if (p_ptr->new_spells >= 0) break;

        if (p_ptr->pclass == CLASS_RAGE_MAGE) break;

        /* Efficiency -- all done */
        if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2) break;

        /* Get the (i+1)th spell learned */
        j = p_ptr->spell_order[i];

        /* Skip unknown spells */
        if (j >= 99) continue;

        /* Forget it (if learned) */
        if ((j < 32) ?
            (p_ptr->spell_learned1 & (1L << j)) :
            (p_ptr->spell_learned2 & (1L << (j - 32))))
        {
            /* Mark as forgotten */
            if (j < 32)
            {
                p_ptr->spell_forgotten1 |= (1L << j);
                which = p_ptr->realm1;
            }
            else
            {
                p_ptr->spell_forgotten2 |= (1L << (j - 32));
                which = p_ptr->realm2;
            }

            /* No longer known */
            if (j < 32)
            {
                p_ptr->spell_learned1 &= ~(1L << j);
                which = p_ptr->realm1;
            }
            else
            {
                p_ptr->spell_learned2 &= ~(1L << (j - 32));
                which = p_ptr->realm2;
            }

            /* Message */
            msg_format("You have forgotten the %s of %s.", p,
                   do_spell(which, j%32, SPELL_NAME));


            /* One more can be learned */
            p_ptr->new_spells++;
        }
    }


    /* Check for spells to remember */
    for (i = 0; i < 64; i++)
    {
        /* None left to remember */
        if (p_ptr->new_spells <= 0) break;
        if (p_ptr->pclass == CLASS_RAGE_MAGE) break;

        /* Efficiency -- all done */
        if (!p_ptr->spell_forgotten1 && !p_ptr->spell_forgotten2) break;

        /* Get the next spell we learned */
        j = p_ptr->spell_order[i];

        /* Skip unknown spells */
        if (j >= 99) break;

        /* Access the spell */
        if (!is_magic((j < 32) ? p_ptr->realm1 : p_ptr->realm2))
        {
            if (j < 32)
                s_ptr = &technic_info[p_ptr->realm1 - MIN_TECHNIC][j];
            else
                s_ptr = &technic_info[p_ptr->realm2 - MIN_TECHNIC][j%32];
        }
        else if (j<32)
            s_ptr = &mp_ptr->info[p_ptr->realm1-1][j];
        else
            s_ptr = &mp_ptr->info[p_ptr->realm2-1][j%32];

        /* Skip spells we cannot remember */
        if (s_ptr->slevel > p_ptr->lev) continue;

        /* First set of spells */
        if ((j < 32) ?
            (p_ptr->spell_forgotten1 & (1L << j)) :
            (p_ptr->spell_forgotten2 & (1L << (j - 32))))
        {
            /* No longer forgotten */
            if (j < 32)
            {
                p_ptr->spell_forgotten1 &= ~(1L << j);
                which = p_ptr->realm1;
            }
            else
            {
                p_ptr->spell_forgotten2 &= ~(1L << (j - 32));
                which = p_ptr->realm2;
            }

            /* Known once more */
            if (j < 32)
            {
                p_ptr->spell_learned1 |= (1L << j);
                which = p_ptr->realm1;
            }
            else
            {
                p_ptr->spell_learned2 |= (1L << (j - 32));
                which = p_ptr->realm2;
            }

            /* Message */
            msg_format("You have remembered the %s of %s.",
                   p, do_spell(which, j%32, SPELL_NAME));


            /* One less can be learned */
            p_ptr->new_spells--;
        }
    }

    k = 0;

    if (p_ptr->realm2 == REALM_NONE)
    {
        /* Count spells that can be learned */
        for (j = 0; j < 32; j++)
        {
            if (!is_magic(p_ptr->realm1)) s_ptr = &technic_info[p_ptr->realm1-MIN_TECHNIC][j];
            else s_ptr = &mp_ptr->info[p_ptr->realm1-1][j];

            /* Skip spells we cannot remember */
            if (s_ptr->slevel > p_ptr->lev) continue;

            /* Skip spells we already know */
            if (p_ptr->spell_learned1 & (1L << j))
            {
                continue;
            }

            /* Count it */
            k++;
        }
        if (k>32) k = 32;
        if ( p_ptr->new_spells > k 
          && (mp_ptr->spell_book == TV_LIFE_BOOK 
           || mp_ptr->spell_book == TV_HISSATSU_BOOK
           || mp_ptr->spell_book == TV_RAGE_BOOK))
        {
            p_ptr->new_spells = k;
        }
    }

    if (p_ptr->new_spells < 0) p_ptr->new_spells = 0;

    /* Spell count changed */
    if (p_ptr->old_spells != p_ptr->new_spells)
    {
        /* Message if needed */
        if (p_ptr->new_spells && p_ptr->pclass != CLASS_RAGE_MAGE)
        {
            /* Message */
            msg_format("You can learn %d more %s%s.",
                   p_ptr->new_spells, p,
                   (p_ptr->new_spells != 1) ? "s" : "");

        }

        /* Save the new_spells value */
        p_ptr->old_spells = p_ptr->new_spells;

        /* Redraw Study Status */
        p_ptr->redraw |= (PR_STUDY);

        /* Redraw object recall */
        p_ptr->window |= (PW_OBJECT);
    }
}


/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static int _racial_mana_adjust(int i)
{               
    int     result = 0;
    race_t *race_ptr;

    /* When doppelgangers mimic, their assumed form affects their mana */
    if (p_ptr->prace == RACE_DOPPELGANGER)
        race_ptr = get_race_t();
    /* but when anybody else mimics, we continue to use their true original race */
    else
        race_ptr = get_true_race_t();
    
    /* psion's best racial modifier wins */
    if (p_ptr->pclass == CLASS_PSION)
    {
        result = MAX(race_ptr->stats[A_INT], 
                    MAX(race_ptr->stats[A_WIS], race_ptr->stats[A_CHR]));
    }
    else
        result = race_ptr->stats[i];

    /* TODO: Some possessor forms have too much mana ... */
    if (p_ptr->prace == RACE_MON_POSSESSOR || p_ptr->prace == RACE_MON_MIMIC)
    {
        switch (i)
        {
        case A_STR: case A_CON:
            result = -5;
            break;
        case A_DEX:
            result = -1;
            break;
        case A_WIS: case A_CHR:
            if (result > 2)
                result = 2;
            break;
        }
    }

    if (result > 5) result = 5;
    if (result < -5) result = -5;

    return result;
}

static void _calc_encumbrance(void)
{
    caster_info *caster_ptr = get_caster_info();
    int          weight = 0;

    /* Gloves */
    if (caster_ptr->options & CASTER_GLOVE_ENCUMBRANCE)
    {
        int slot;
        for (slot = equip_find_first(object_is_gloves);
                slot;
                slot = equip_find_next(object_is_gloves, slot))
        {
            u32b         flgs[TR_FLAG_SIZE];
            object_type *o_ptr = equip_obj(slot);

            object_flags(o_ptr, flgs);

            if (!(have_flag(flgs, TR_FREE_ACT)) &&
                !(have_flag(flgs, TR_MAGIC_MASTERY)) &&
                !((have_flag(flgs, TR_DEX)) && (o_ptr->pval > 0)))
            {
                p_ptr->cumber_glove = TRUE;
                break;
            }
        }
    }

    /* Armor/Weapon Weight */
    weight = equip_weight(object_is_armour);
    switch (get_class_idx())
    {
    case CLASS_MAGE:
    case CLASS_NECROMANCER:
    case CLASS_BLOOD_MAGE:
    case CLASS_HIGH_MAGE:
    case CLASS_BLUE_MAGE:
    case CLASS_MONK:
    case CLASS_FORCETRAINER:
    case CLASS_SORCERER:
        weight += equip_weight(object_is_melee_weapon);
        break;

    case CLASS_PRIEST:
    case CLASS_BARD:
    case CLASS_TOURIST:
    case CLASS_SCOUT:
    case CLASS_MONSTER:
        weight += equip_weight(object_is_melee_weapon) * 2 / 3;
        break;

    case CLASS_MINDCRAFTER:
    case CLASS_PSION:
    case CLASS_BEASTMASTER:
    case CLASS_MIRROR_MASTER:
        weight += equip_weight(object_is_melee_weapon) / 2;
        break;

    case CLASS_ROGUE:
    case CLASS_RANGER:
    case CLASS_RED_MAGE:
    case CLASS_WARRIOR_MAGE:
    case CLASS_ARCHAEOLOGIST:
        weight += equip_weight(object_is_melee_weapon) / 3;
        break;

    case CLASS_PALADIN:
    case CLASS_CHAOS_WARRIOR:
    case CLASS_RAGE_MAGE:
        weight += equip_weight(object_is_melee_weapon) / 5;
        break;

    default:
        weight += equip_weight(object_is_melee_weapon);
    }

    if (weight > caster_ptr->weight)
    {
        p_ptr->cumber_armor = TRUE;
        p_ptr->cumber_armor_amt = weight - caster_ptr->weight;
    }
}

static void _report_encumbrance(void)
{
    if (character_xtra) return;

    if (p_ptr->old_cumber_glove != p_ptr->cumber_glove)
    {
        if (p_ptr->cumber_glove)
            msg_print("Your covered hands feel unsuitable for spellcasting.");
        else
            msg_print("Your hands feel more suitable for spellcasting.");
        p_ptr->old_cumber_glove = p_ptr->cumber_glove;
    }

    if (p_ptr->old_cumber_armor != p_ptr->cumber_armor)
    {
        if (p_ptr->cumber_armor)
            msg_print("The weight of your equipment encumbers your movement.");
        else
            msg_print("You feel able to move more freely.");
        p_ptr->old_cumber_armor = p_ptr->cumber_armor;
    }
}

static void calc_mana(void)
{
    int             msp, lvl;
    caster_info    *caster_ptr = get_caster_info();

    p_ptr->cumber_glove = FALSE;
    p_ptr->cumber_armor = FALSE;
    p_ptr->cumber_armor_amt = 0;

    if (!caster_ptr)
    {
        if (p_ptr->msp) /* Possessor shifted from Caster body to non-caster body ... */
        {
            p_ptr->msp = 0;
            p_ptr->csp = 0;
            p_ptr->redraw |= (PR_MANA);
        }
        return;
    }
    if ( (caster_ptr->options & CASTER_USE_HP) 
      || (caster_ptr->options & CASTER_NO_SPELL_COST) 
      || p_ptr->lev < caster_ptr->min_level)
    {
        p_ptr->msp = 0;
        p_ptr->csp = 0;
        p_ptr->redraw |= (PR_MANA);

        if (p_ptr->pclass == CLASS_BLOOD_MAGE)
        {
            /* No more free ride for Blood Mages. They will get increased fail rates
               instead.  See mod_spell_chance_* in spells3.c for details */
            _calc_encumbrance();
            _report_encumbrance();
        }

        return;
    }

    if (caster_ptr->min_level) /* Hack: 0 => 1 */
        lvl = p_ptr->lev - caster_ptr->min_level + 1;
    else
        lvl = p_ptr->lev;


    if (p_ptr->pclass == CLASS_SAMURAI || p_ptr->pclass == CLASS_MYSTIC)
    {
        msp = (adj_mag_mana[p_ptr->stat_ind[caster_ptr->which_stat]] + 10) * 2;
        if (msp) msp += (msp * _racial_mana_adjust(caster_ptr->which_stat) / 20);
    }
    else
    {
        int idx = p_ptr->stat_ind[caster_ptr->which_stat];

        msp = adj_mag_mana[idx] * (lvl + 3)/4;
        if (msp) msp++;
        if (msp) 
        {
            int adj = _racial_mana_adjust(caster_ptr->which_stat);
            msp += (msp * adj / 20);
        }

        if (msp && (p_ptr->personality == PERS_MUNCHKIN)) msp += msp/2;
        if (msp && (get_class_idx() == CLASS_SORCERER)) msp += msp*(25+p_ptr->lev)/100;
    }

    _calc_encumbrance();
    if (p_ptr->cumber_glove)
        msp = (3 * msp) / 4;

    if (p_ptr->cumber_armor)
    {
        switch (get_class_idx())
        {
        case CLASS_MAGE:
        case CLASS_NECROMANCER:
        case CLASS_BLOOD_MAGE:
        case CLASS_HIGH_MAGE:
        case CLASS_BLUE_MAGE:
            msp -= msp * p_ptr->cumber_armor_amt / 600;
            break;

        case CLASS_PRIEST:
        case CLASS_MINDCRAFTER:
        case CLASS_PSION:
        case CLASS_BEASTMASTER:
        case CLASS_BARD:
        case CLASS_FORCETRAINER:
        case CLASS_TOURIST:
        case CLASS_MIRROR_MASTER:
        case CLASS_MONSTER:
            msp -= msp * p_ptr->cumber_armor_amt / 800;
            break;

        case CLASS_SORCERER:
            msp -= msp * p_ptr->cumber_armor_amt / 900;
            break;

        case CLASS_ROGUE:
        case CLASS_RANGER:
        case CLASS_MONK:
        case CLASS_RED_MAGE:
            msp -= msp * p_ptr->cumber_armor_amt / 1000;
            break;

        case CLASS_PALADIN:
        case CLASS_CHAOS_WARRIOR:
        case CLASS_WARRIOR_MAGE:
        case CLASS_RAGE_MAGE:
            msp -= msp * p_ptr->cumber_armor_amt / 1200;
            break;

        case CLASS_SAMURAI:
        case CLASS_MYSTIC:
            p_ptr->cumber_armor = FALSE;
            p_ptr->cumber_armor_amt = 0;
            break;

        default:
            msp -= msp * p_ptr->cumber_armor_amt / 800;
        }
    }

    if (msp < 0) msp = 0;
    msp = spell_cap(msp);

    if (p_ptr->msp != msp)
    {
        int delta = p_ptr->msp - p_ptr->csp;
        int csp = msp - delta;

        p_ptr->msp = msp;

        /* Preserve the amount of used up mana whenever the total changes */
        if (p_ptr->csp > 0 && csp >= 0)
            p_ptr->csp = csp;

        if (p_ptr->csp >= msp && p_ptr->pclass != CLASS_SAMURAI && p_ptr->pclass != CLASS_MYSTIC)
        {
            p_ptr->csp = msp;
            p_ptr->csp_frac = 0;
        }

        p_ptr->redraw |= (PR_MANA);
        p_ptr->window |= (PW_PLAYER);
        p_ptr->window |= (PW_SPELL);
    }

    _report_encumbrance();
}

/*
 * Calculate the players (maximal) hit points
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints(void)
{
    int      mhp;
    race_t  *race_ptr = get_race_t();
    class_t *class_ptr = get_class_t();
/*    pers_t  *pers_ptr = get_pers_t(); */

    mhp = p_ptr->player_hp[p_ptr->lev - 1] * 13 / 100;
    mhp += 3 * p_ptr->lev / 2;
    mhp += 3 * p_ptr->lev * p_ptr->lev / 100;
    mhp += 3 * p_ptr->lev * p_ptr->lev * p_ptr->lev / 5000;

    mhp = mhp * race_ptr->life / 100;
    mhp = mhp * class_ptr->life / 100;
/*    mhp = mhp * pers_ptr->life / 100; */
    mhp = mhp * ap_ptr->life / 100;
    mhp = mhp * p_ptr->life / 100;
    
    if (p_ptr->prace == RACE_MON_DRAGON)
    {
        dragon_realm_ptr realm = dragon_get_realm(p_ptr->dragon_realm);
        mhp = mhp * realm->life / 100;
    }

    mhp += class_ptr->base_hp;
    mhp += race_ptr->base_hp;

    if (mhp < 1)
        mhp = 1;

    if (IS_HERO()) mhp += 10;
    if (IS_SHERO() && (p_ptr->pclass != CLASS_BERSERKER)) mhp += 30;
    if (p_ptr->tsuyoshi) mhp += 50;
    if (p_ptr->pclass == CLASS_WARLOCK && p_ptr->psubclass == PACT_UNDEAD) mhp += 100 * p_ptr->lev/50;
    if (hex_spelling(HEX_XTRA_MIGHT)) mhp += 15;
    if (hex_spelling(HEX_BUILDING)) mhp += 60;
    if (p_ptr->tim_building_up) mhp += 10 + p_ptr->lev/2;
    if (mut_present(MUT_UNYIELDING)) mhp += p_ptr->lev;

    if (p_ptr->mhp != mhp)
    {
        if (p_ptr->mhp)
            p_ptr->chp = mhp * p_ptr->chp / p_ptr->mhp;
        else /* birthing */
            p_ptr->chp = mhp;

        p_ptr->chp_frac = 0;
        p_ptr->mhp = mhp;
        p_ptr->redraw |= (PR_HP);
        p_ptr->window |= (PW_PLAYER);
    }
}



/*
 * Extract and set the current "lite radius"
 *
 * SWD: Experimental modification: multiple light sources have additive effect.
 *
 */
static void _calc_torch_imp(object_type *o_ptr)
{
    if (o_ptr->tval == TV_LITE)
    {
        if (o_ptr->name2 == EGO_LITE_DARKNESS || have_flag(o_ptr->art_flags, TR_DARKNESS))
        {
            if (o_ptr->sval == SV_LITE_TORCH)
                p_ptr->cur_lite -= 1;
            else if (o_ptr->sval == SV_LITE_LANTERN)
                p_ptr->cur_lite -= 2;
            else
                p_ptr->cur_lite -= 3;
        }
        else if (o_ptr->sval == SV_LITE_TORCH && o_ptr->xtra4 > 0)
            p_ptr->cur_lite += 1;
        else if (o_ptr->sval == SV_LITE_LANTERN && o_ptr->xtra4 > 0)
            p_ptr->cur_lite += 2;
        else if (o_ptr->sval == SV_LITE_FEANOR)
            p_ptr->cur_lite += 2;
        else if (o_ptr->name1 || o_ptr->art_name || o_ptr->name3)
        {
            p_ptr->cur_lite += 3;
        }
        if (o_ptr->name2 == EGO_LITE_EXTRA_LIGHT) p_ptr->cur_lite++;
        if (o_ptr->sval == SV_LITE_EYE) p_ptr->cur_lite -= 10;
    }
    else
    {
        u32b flgs[TR_FLAG_SIZE] = {0};
        object_flags(o_ptr, flgs);
        if (have_flag(flgs, TR_LITE))
        {
            if (o_ptr->name2 == EGO_HELMET_VAMPIRE || o_ptr->name1 == ART_NIGHT) p_ptr->cur_lite--;
            else p_ptr->cur_lite++;
        }
    }
}
static void calc_torch(void)
{
    p_ptr->cur_lite = 0;

    if (demigod_is_(DEMIGOD_APOLLO))
        p_ptr->cur_lite++;
    if (prace_is_(RACE_MON_ANGEL))
        p_ptr->cur_lite++;
    if (prace_is_(RACE_MON_SWORD))
        p_ptr->cur_lite += sword_calc_torch();
    if (prace_is_(RACE_MON_RING))
        p_ptr->cur_lite += ring_calc_torch();

    equip_for_each_obj(_calc_torch_imp);

    if (d_info[dungeon_type].flags1 & DF1_DARKNESS && p_ptr->cur_lite > 1)
        p_ptr->cur_lite = 1;

    /*
     * check if the player doesn't have light radius, 
     * but does weakly glow as an intrinsic.
     */
    if (p_ptr->cur_lite <= 0 && p_ptr->lite) p_ptr->cur_lite++;

    /* max radius is 14 (was 5) without rewriting other code -- */
    /* see cave.c:update_lite() and defines.h:LITE_MAX */
    if (p_ptr->cur_lite > 14) p_ptr->cur_lite = 14;
    if (p_ptr->cur_lite < 0) p_ptr->cur_lite = 0;

    if (p_ptr->tim_superstealth)
        p_ptr->cur_lite = 0;

    /* Notice changes in the "lite radius" */
    if (p_ptr->old_lite != p_ptr->cur_lite)
    {
        p_ptr->update |= (PU_LITE | PU_MON_LITE | PU_MONSTERS);
        p_ptr->old_lite = p_ptr->cur_lite;
        if ((p_ptr->cur_lite > 0) && (p_ptr->special_defense & NINJA_S_STEALTH))
            set_superstealth(FALSE);
    }
}



/*
 * Computes current weight limit.
 */
u32b weight_limit(void)
{
    u32b i;

    /* Weight limit based only on strength */
    i = (u32b)adj_str_wgt[p_ptr->stat_ind[A_STR]] * 50; /* Constant was 100 */
    if (p_ptr->pclass == CLASS_BERSERKER) i = i * 3 / 2;

    /* Return the result */
    return i;
}


/*
 * Calculate the players current "state", taking into account
 * not only race/class intrinsics, but also objects being worn
 * and temporary spell effects.
 *
 * See also calc_mana() and calc_hitpoints().
 *
 * Take note of the new "speed code", in particular, a very strong
 * player will start slowing down as soon as he reaches 150 pounds,
 * but not until he reaches 450 pounds will he be half as fast as
 * a normal kobold.  This both hurts and helps the player, hurts
 * because in the old days a player could just avoid 300 pounds,
 * and helps because now carrying 300 pounds is not very painful.
 *
 * The "weapon" and "bow" do *not* add to the bonuses to hit or to
 * damage, since that would affect non-combat things.  These values
 * are actually added in later, at the appropriate place.
 *
 * This function induces various "status" messages.
 */
void calc_bonuses(void)
{
    int             i, j, hold, neutral[2], arm, slot;
    s16b            old_speed = p_ptr->pspeed;
    s16b            old_life = p_ptr->life;
    object_type     *o_ptr;
    u32b flgs[TR_FLAG_SIZE];
    bool            riding_levitation = FALSE;

    class_t *class_ptr = get_class_t();
    race_t *race_ptr = get_race_t();

    /* Save the old vision stuff */
    bool old_telepathy = p_ptr->telepathy;
    bool old_esp_animal = p_ptr->esp_animal;
    bool old_esp_undead = p_ptr->esp_undead;
    bool old_esp_demon = p_ptr->esp_demon;
    bool old_esp_orc = p_ptr->esp_orc;
    bool old_esp_troll = p_ptr->esp_troll;
    bool old_esp_giant = p_ptr->esp_giant;
    bool old_esp_dragon = p_ptr->esp_dragon;
    bool old_esp_human = p_ptr->esp_human;
    bool old_esp_evil = p_ptr->esp_evil;
    bool old_esp_good = p_ptr->esp_good;
    bool old_esp_nonliving = p_ptr->esp_nonliving;
    bool old_esp_unique = p_ptr->esp_unique;
    bool old_esp_magical = p_ptr->esp_magical;
    bool old_see_inv = p_ptr->see_inv;
    bool old_mighty_throw = p_ptr->mighty_throw;

    /* Save the old armor class */
    s16b old_dis_ac = p_ptr->dis_ac;
    s16b old_dis_to_a = p_ptr->dis_to_a;

    /* Clear the stat modifiers */
    for (i = 0; i < 6; i++) p_ptr->stat_add[i] = 0;


    /* Clear the Displayed/Real armor class */
    p_ptr->dis_ac = p_ptr->ac = 0;

    /* Clear the Displayed/Real Bonuses */
    p_ptr->shooter_info.to_h = 0;
    p_ptr->shooter_info.to_d = 0;
    p_ptr->shooter_info.dis_to_h = 0;
    p_ptr->shooter_info.dis_to_d = 0;
    p_ptr->shooter_info.num_fire = 100;
    p_ptr->shooter_info.heavy_shoot = FALSE;
    p_ptr->shooter_info.to_mult = 0;
    p_ptr->shooter_info.tval_ammo = 0;

    for (i = 0; i < TR_FLAG_SIZE; i++)
        p_ptr->shooter_info.flags[i] = 0;

    if (p_ptr->tim_speed_essentia)
        p_ptr->shooter_info.num_fire += 100;

    p_ptr->dis_to_a = p_ptr->to_a = 0;
    p_ptr->to_h_m = 0;
    p_ptr->to_d_m = 0;
    p_ptr->easy_2weapon = FALSE;
    p_ptr->speciality_equip = FALSE;
    p_ptr->sneak_attack = FALSE;

    p_ptr->to_d_spell = 0;

    p_ptr->to_m_chance = 0;

    p_ptr->weapon_ct = 0;
    for (i = 0; i < MAX_HANDS; i++)
    {
        p_ptr->weapon_info[i].wield_how = WIELD_NONE;
        p_ptr->weapon_info[i].omoi = FALSE;
        p_ptr->weapon_info[i].bare_hands = FALSE;
        p_ptr->weapon_info[i].riding = FALSE;
        p_ptr->weapon_info[i].slot = 0;
        p_ptr->weapon_info[i].genji = p_ptr->tim_genji > 0;
        p_ptr->weapon_info[i].dis_to_h = 0;
        p_ptr->weapon_info[i].to_h = 0;
        p_ptr->weapon_info[i].dis_to_d = 0;
        p_ptr->weapon_info[i].to_d = 0;

        p_ptr->weapon_info[i].to_dd = 0;
        p_ptr->weapon_info[i].to_ds = 0;
        p_ptr->weapon_info[i].to_mult = 0;
        for (j = 0; j < TR_FLAG_SIZE; j++)
            p_ptr->weapon_info[i].flags[j] = 0;

        p_ptr->weapon_info[i].base_blow = 100;
        p_ptr->weapon_info[i].xtra_blow = 0;
        p_ptr->weapon_info[i].dual_wield_pct = 1000;

        p_ptr->weapon_info[i].heavy_wield = FALSE;
        p_ptr->weapon_info[i].icky_wield = FALSE;
        p_ptr->weapon_info[i].riding_wield = FALSE;
        p_ptr->weapon_info[i].giant_wield = FALSE;

        p_ptr->weapon_info[i].info_attr = TERM_WHITE;
        p_ptr->weapon_info[i].info = 0;
    }
    p_ptr->innate_attack_ct = 0;
    p_ptr->innate_attack_info.to_dd = 0;
    p_ptr->innate_attack_info.xtra_blow = 0;
    for (i = 0; i < TR_FLAG_SIZE; i++)
        p_ptr->innate_attack_info.flags[i] = 0;

    /* Start with "normal" speed */
    p_ptr->pspeed = 110;

    /* Clear all the flags */
    p_ptr->cursed = 0L;
    p_ptr->bless_blade = FALSE;
    p_ptr->pass_wall = FALSE;
    p_ptr->kill_wall = FALSE;
    p_ptr->dec_mana = FALSE;
    p_ptr->spell_power = 0;
    p_ptr->device_power = 0;
    p_ptr->spell_cap = 0;
    p_ptr->easy_spell = FALSE;
    p_ptr->heavy_spell = FALSE;
    p_ptr->see_inv = FALSE;
    p_ptr->free_act = FALSE;
    p_ptr->slow_digest = FALSE;
    p_ptr->regenerate = FALSE;
    p_ptr->super_regenerate = FALSE;
    p_ptr->can_swim = FALSE;
    p_ptr->levitation = FALSE;
    p_ptr->hold_life = FALSE;
    p_ptr->loremaster = FALSE;
    p_ptr->auto_id_sp = 0;
    p_ptr->cult_of_personality = FALSE;
    p_ptr->telepathy = FALSE;
    p_ptr->esp_animal = FALSE;
    p_ptr->esp_undead = FALSE;
    p_ptr->esp_demon = FALSE;
    p_ptr->esp_orc = FALSE;
    p_ptr->esp_troll = FALSE;
    p_ptr->esp_giant = FALSE;
    p_ptr->esp_dragon = FALSE;
    p_ptr->esp_human = FALSE;
    p_ptr->esp_evil = FALSE;
    p_ptr->esp_good = FALSE;
    p_ptr->esp_nonliving = FALSE;
    p_ptr->esp_unique = FALSE;
    p_ptr->esp_magical = FALSE;
    p_ptr->lite = FALSE;
    p_ptr->sustain_str = FALSE;
    p_ptr->sustain_int = FALSE;
    p_ptr->sustain_wis = FALSE;
    p_ptr->sustain_con = FALSE;
    p_ptr->sustain_dex = FALSE;
    p_ptr->sustain_chr = FALSE;
    
    res_clear();

    p_ptr->life = 0;
    p_ptr->reflect = FALSE;
    
    p_ptr->sh_fire = FALSE;
    p_ptr->sh_elec = FALSE;
    p_ptr->sh_cold = FALSE;
    p_ptr->sh_shards = FALSE;
    p_ptr->sh_retaliation = FALSE;
    p_ptr->sh_fear = FALSE;

    p_ptr->anti_magic = FALSE;
    p_ptr->anti_tele = FALSE;
    p_ptr->anti_summon = FALSE;
    p_ptr->warning = FALSE;
    p_ptr->mighty_throw = FALSE;
    p_ptr->see_nocto = FALSE;
    p_ptr->easy_realm1 = REALM_NONE;

    p_ptr->move_random = FALSE;

    p_ptr->magic_resistance = 0;
    p_ptr->good_luck = FALSE;
    p_ptr->rune_elem_prot = FALSE;
    p_ptr->no_eldritch = FALSE;
    p_ptr->no_charge_drain = FALSE;
    p_ptr->no_stun = FALSE;
    p_ptr->no_cut = FALSE;
    p_ptr->no_passwall_dam = FALSE;
    p_ptr->melt_armor = FALSE;

    p_ptr->ryoute = FALSE;
    p_ptr->migite = FALSE;
    p_ptr->hidarite = FALSE;
    p_ptr->no_flowed = FALSE;

    p_ptr->unlimited_quiver = FALSE;
    p_ptr->return_ammo = FALSE;
    p_ptr->big_shot = FALSE;
    p_ptr->painted_target = FALSE;
    p_ptr->enhanced_crit = FALSE;
    p_ptr->cleave = FALSE;
    p_ptr->constant_hero = FALSE;
    p_ptr->vorpal = FALSE;
    p_ptr->whirlwind = FALSE;
    p_ptr->entrenched = FALSE;
    p_ptr->lightning_reflexes = FALSE;
    p_ptr->clear_mind = FALSE;
    p_ptr->inven_prot = FALSE;
    p_ptr->ambush = FALSE;
    p_ptr->peerless_stealth = FALSE;
    p_ptr->fairy_stealth = FALSE;
    p_ptr->open_terrain_ct = 0;

    p_ptr->quick_walk = FALSE;

    p_ptr->align = friend_align;
    p_ptr->maul_of_vice = FALSE;


    if (p_ptr->tim_sustain_str) p_ptr->sustain_str = TRUE;
    if (p_ptr->tim_sustain_int) p_ptr->sustain_int = TRUE;
    if (p_ptr->tim_sustain_wis) p_ptr->sustain_wis = TRUE;
    if (p_ptr->tim_sustain_dex) p_ptr->sustain_dex = TRUE;
    if (p_ptr->tim_sustain_con) p_ptr->sustain_con = TRUE;
    if (p_ptr->tim_sustain_chr) p_ptr->sustain_chr = TRUE;
    if (p_ptr->tim_hold_life) p_ptr->hold_life = TRUE;
    if (p_ptr->tim_inven_prot) p_ptr->inven_prot = TRUE;
    if (p_ptr->tim_quick_walk) p_ptr->quick_walk = TRUE;

    if (p_ptr->special_attack & ATTACK_ACID)
    {
        add_flag(p_ptr->weapon_info[0].flags, TR_BRAND_ACID);
        add_flag(p_ptr->weapon_info[1].flags, TR_BRAND_ACID);
        add_flag(p_ptr->shooter_info.flags, TR_BRAND_ACID);
    }
    if (p_ptr->special_attack & ATTACK_COLD)
    {
        add_flag(p_ptr->weapon_info[0].flags, TR_BRAND_COLD);
        add_flag(p_ptr->weapon_info[1].flags, TR_BRAND_COLD);
        add_flag(p_ptr->shooter_info.flags, TR_BRAND_COLD);
    }
    if (p_ptr->special_attack & ATTACK_FIRE)
    {
        add_flag(p_ptr->weapon_info[0].flags, TR_BRAND_FIRE);
        add_flag(p_ptr->weapon_info[1].flags, TR_BRAND_FIRE);
        add_flag(p_ptr->shooter_info.flags, TR_BRAND_FIRE);
    }
    if (p_ptr->special_attack & ATTACK_ELEC)
    {
        add_flag(p_ptr->weapon_info[0].flags, TR_BRAND_ELEC);
        add_flag(p_ptr->weapon_info[1].flags, TR_BRAND_ELEC);
        add_flag(p_ptr->shooter_info.flags, TR_BRAND_ELEC);
    }
    if (p_ptr->special_attack & ATTACK_POIS)
    {
        add_flag(p_ptr->weapon_info[0].flags, TR_BRAND_POIS);
        add_flag(p_ptr->weapon_info[1].flags, TR_BRAND_POIS);
        add_flag(p_ptr->shooter_info.flags, TR_BRAND_POIS);
    }

    if (p_ptr->tim_device_power)
    {
        int bonus = 1 + (p_ptr->lev + 10)/15;
        p_ptr->device_power += bonus;
    }

    if (mut_present(MUT_FLEET_OF_FOOT)) p_ptr->quick_walk = TRUE;
    if (mut_present(MUT_DEMONIC_GRASP)) p_ptr->no_charge_drain = TRUE;

    p_ptr->see_infra = race_ptr->infra;

    /* calc_skills() */
    {
        skills_t c_extra = class_ptr->extra_skills;
        skills_t r_extra = race_ptr->extra_skills;
        skills_t a_extra = ap_ptr->skills;

        skills_scale(&c_extra, p_ptr->lev, 10);
        skills_scale(&r_extra, p_ptr->lev, 10);

        a_extra.stl = 0; /* Hengband never gave extra personality stealth with level ... */
        skills_scale(&a_extra, p_ptr->lev, 50);

        skills_init(&p_ptr->skills);
        skills_add(&p_ptr->skills, &class_ptr->base_skills);
        skills_add(&p_ptr->skills, &c_extra);
        skills_add(&p_ptr->skills, &race_ptr->skills);
        skills_add(&p_ptr->skills, &r_extra);
        skills_add(&p_ptr->skills, &ap_ptr->skills);
        skills_add(&p_ptr->skills, &a_extra);

        if (p_ptr->prace == RACE_MON_DRAGON)
        {
            dragon_realm_ptr realm = dragon_get_realm(p_ptr->dragon_realm);
            skills_t         extra = realm->skills;

            extra.stl = 0;
            skills_scale(&extra, p_ptr->lev, 10);

            skills_add(&p_ptr->skills, &realm->skills);
            skills_add(&p_ptr->skills, &extra);
        }
    }

    p_ptr->skill_tht = p_ptr->skills.thb;
    p_ptr->skill_dig = 0;

    if (p_ptr->tim_superstealth)
        p_ptr->see_nocto = TRUE;

    slot = equip_find_object(TV_LITE, SV_ANY);
    if (slot)
    {
        o_ptr = equip_obj(slot);
        switch (o_ptr->name1)
        {
        case ART_EYE_OF_VECNA:
            p_ptr->see_nocto = TRUE;
            break;
        case ART_STONE_OF_NATURE:
            p_ptr->easy_realm1 = REALM_NATURE;
            break;
        case ART_STONE_OF_LIFE:
            p_ptr->easy_realm1 = REALM_LIFE;
            break;
        case ART_STONE_OF_SORCERY:
            p_ptr->easy_realm1 = REALM_SORCERY;
            break;
        case ART_STONE_OF_CHAOS:
            p_ptr->easy_realm1 = REALM_CHAOS;
            break;
        case ART_STONE_OF_DEATH:
            p_ptr->easy_realm1 = REALM_DEATH;
            break;
        case ART_STONE_OF_TRUMP:
            p_ptr->easy_realm1 = REALM_TRUMP;
            break;
        case ART_STONE_OF_DAEMON:
            p_ptr->easy_realm1 = REALM_DAEMON;
            break;
        case ART_STONE_OF_CRUSADE:
            p_ptr->easy_realm1 = REALM_CRUSADE;
            break;
        case ART_STONE_OF_CRAFT:
            p_ptr->easy_realm1 = REALM_CRAFT;
            break;
        case ART_STONE_OF_ARMAGEDDON:
            p_ptr->easy_realm1 = REALM_ARMAGEDDON;
            break;
        }
    }

    if (p_ptr->ult_res || (p_ptr->special_defense & KATA_MUSOU))
    {
        p_ptr->see_inv = TRUE;
        p_ptr->free_act = TRUE;
        p_ptr->slow_digest = TRUE;
        p_ptr->regenerate = TRUE;
        p_ptr->levitation = TRUE;

        if (p_ptr->special_defense & KATA_MUSOU)
        {
            p_ptr->hold_life = TRUE;
            p_ptr->sustain_str = TRUE;
            p_ptr->sustain_int = TRUE;
            p_ptr->sustain_wis = TRUE;
            p_ptr->sustain_con = TRUE;
            p_ptr->sustain_dex = TRUE;
            p_ptr->sustain_chr = TRUE;
        }

        p_ptr->telepathy = TRUE;
        p_ptr->lite = TRUE;
        res_add_all();
        p_ptr->reflect = TRUE;
        p_ptr->sh_fire = TRUE;
        p_ptr->sh_elec = TRUE;
        p_ptr->sh_cold = TRUE;
        p_ptr->to_a += 100;
        p_ptr->dis_to_a += 100;
    }
    /* Temporary shield */
    else if (p_ptr->tsubureru || IS_STONE_SKIN() || p_ptr->magicdef)
    {
        int bonus = 10 + 40*p_ptr->lev/50;
        p_ptr->to_a += bonus;
        p_ptr->dis_to_a += bonus;
    }

    if (IS_OPPOSE_ACID()) res_add(RES_ACID);
    if (IS_OPPOSE_ELEC()) res_add(RES_ELEC);
    if (IS_OPPOSE_FIRE()) res_add(RES_FIRE);
    if (IS_OPPOSE_COLD()) res_add(RES_COLD);
    if (IS_OPPOSE_POIS()) res_add(RES_POIS);
    if (p_ptr->tim_res_nether) res_add(RES_NETHER);
    if (p_ptr->tim_res_disenchantment) res_add(RES_DISEN);
    if (p_ptr->tim_res_time) res_add(RES_TIME);

    if (mut_present(MUT_VULN_ELEM) || (p_ptr->special_defense & KATA_KOUKIJIN))
    {
        res_add_vuln(RES_ACID);
        res_add_vuln(RES_ELEC);
        res_add_vuln(RES_FIRE);
        res_add_vuln(RES_COLD);
    }
    
    if (p_ptr->tim_sh_fire)
        p_ptr->sh_fire = TRUE;

    if (p_ptr->tim_sh_shards)
        p_ptr->sh_shards = TRUE;

    if (p_ptr->tim_sh_elements)
    {
        p_ptr->sh_fire = TRUE;
        if (p_ptr->lev >= 25)
            p_ptr->sh_cold = TRUE;
        if (p_ptr->lev >= 35)
            p_ptr->sh_elec = TRUE;
    }

    /* Personalities */
    if (p_ptr->personality == PERS_SEXY) p_ptr->cursed |= TRC_AGGRAVATE;
    if (p_ptr->personality == PERS_LAZY) p_ptr->to_m_chance += 10;
    if (p_ptr->personality == PERS_SHREWD) p_ptr->to_m_chance -= 3;
    if (p_ptr->personality == PERS_PATIENT || p_ptr->personality == PERS_MIGHTY) p_ptr->to_m_chance++;
    if (p_ptr->personality == PERS_FEARLESS) res_add(RES_FEAR);
    if (p_ptr->personality == PERS_HASTY)
    {
        p_ptr->pspeed += 2;
        p_ptr->to_m_chance += 1;
    }

    if ( p_ptr->personality == PERS_LUCKY
      && !mut_present(MUT_GOOD_LUCK) )
    {
        mut_gain(MUT_GOOD_LUCK);
        mut_lock(MUT_GOOD_LUCK);
    }

    if (mut_present(MUT_GOOD_LUCK))
        p_ptr->good_luck = TRUE;

    if (p_ptr->personality == PERS_MUNCHKIN)
    {
        res_add(RES_BLIND);
        res_add(RES_CONF);
        p_ptr->hold_life = TRUE;
        if (p_ptr->pclass != CLASS_NINJA) p_ptr->lite = TRUE;

        if (p_ptr->prace != RACE_KLACKON && p_ptr->prace != RACE_SPRITE)
            p_ptr->pspeed += (p_ptr->lev) / 10 + 5;
    }

    if (music_singing(MUSIC_WALL))
        p_ptr->kill_wall = TRUE;

    /* Hack -- apply racial/class stat maxes */
    /* Apply the racial modifiers */
    for (i = 0; i < 6; i++)
    {
        p_ptr->stat_add[i] += (race_ptr->stats[i] + class_ptr->stats[i] + ap_ptr->a_adj[i]);
    }


    /* I'm adding the mutations here for the lack of a better place... */
    mut_calc_bonuses();
    if (mut_present(MUT_ILL_NORM))
        p_ptr->stat_add[A_CHR] = 0;

    if (p_ptr->tsuyoshi)
    {
        p_ptr->stat_add[A_STR] += 4;
        p_ptr->stat_add[A_CON] += 4;
    }

    /* Some Runes work when placed in Inventory */
    for (i = 0; i < INVEN_PACK; i++)
    {
        o_ptr = &inventory[i];
        if (!o_ptr->k_idx) continue;
        if (o_ptr->name1 == ART_MAUL_OF_VICE)
            p_ptr->maul_of_vice = TRUE;
        if (o_ptr->rune == RUNE_ELEMENTAL_PROTECTION) 
            p_ptr->rune_elem_prot = TRUE;
        if (o_ptr->rune == RUNE_GOOD_FORTUNE) 
            p_ptr->good_luck = TRUE;
    }

    equip_calc_bonuses();

    if (p_ptr->special_defense & KAMAE_MASK)
    {
        if (p_ptr->pclass != CLASS_WILD_TALENT && !p_ptr->weapon_info[0].bare_hands)
            set_action(ACTION_NONE);
    }

    if (old_mighty_throw != p_ptr->mighty_throw)
    {
        /* Redraw average damege display of Shuriken */
        p_ptr->window |= PW_INVEN;
    }

    if (p_ptr->cursed & TRC_TELEPORT) p_ptr->cursed &= ~(TRC_TELEPORT_SELF);

    if ( (p_ptr->pclass == CLASS_MONK && !heavy_armor())
      || p_ptr->pclass == CLASS_WILD_TALENT)
    {
        /* Massive Hacks:  The Wild Talent gets monk posture benefits without monk
           restrictions, so we duplicate a bunch of code here that is set later for monks
           after checking weapon status */
        if (p_ptr->special_defense & KAMAE_BYAKKO)
        {
            p_ptr->stat_add[A_STR] += 2;
            p_ptr->stat_add[A_DEX] += 2;
            p_ptr->stat_add[A_CON] -= 3;
            
            if (p_ptr->pclass == CLASS_WILD_TALENT)
            {
                p_ptr->to_a -= 40;
                p_ptr->dis_to_a -= 40;
            }
        }
        else if (p_ptr->special_defense & KAMAE_SEIRYU)
        {
            if (p_ptr->pclass == CLASS_WILD_TALENT)
            {
                p_ptr->to_a -= 50;
                p_ptr->dis_to_a -= 50;
                res_add(RES_ACID);
                res_add(RES_FIRE);
                res_add(RES_ELEC);
                res_add(RES_COLD);
                res_add(RES_POIS);
                p_ptr->sh_fire = TRUE;
                p_ptr->sh_elec = TRUE;
                p_ptr->sh_cold = TRUE;
                p_ptr->levitation = TRUE;
            }
        }
        else if (p_ptr->special_defense & KAMAE_GENBU)
        {
            p_ptr->stat_add[A_INT] -= 1;
            p_ptr->stat_add[A_WIS] -= 1;
            p_ptr->stat_add[A_DEX] -= 2;
            p_ptr->stat_add[A_CON] += 3;

            if (p_ptr->pclass == CLASS_WILD_TALENT)
            {
                p_ptr->to_a += (p_ptr->lev*p_ptr->lev)/50;
                p_ptr->dis_to_a += (p_ptr->lev*p_ptr->lev)/50;
                p_ptr->reflect = TRUE;
            }
        }
        else if (p_ptr->special_defense & KAMAE_SUZAKU)
        {
            p_ptr->stat_add[A_STR] -= 2;
            p_ptr->stat_add[A_INT] += 1;
            p_ptr->stat_add[A_WIS] += 1;
            p_ptr->stat_add[A_DEX] += 2;
            p_ptr->stat_add[A_CON] -= 2;
            if (p_ptr->pclass == CLASS_WILD_TALENT)
                p_ptr->levitation = TRUE;
        }
    }


    if (p_ptr->special_defense & KATA_KOUKIJIN)
    {
        for (i = 0; i < 6; i++)
            p_ptr->stat_add[i] += 5;
        p_ptr->to_a -= 50;
        p_ptr->dis_to_a -= 50;
    }

    /* Hack -- aura of fire also provides light */
    if (p_ptr->sh_fire) p_ptr->lite = TRUE;

    if (p_ptr->tim_building_up)
    {
        int amt = 4 * p_ptr->lev / 50; /* 13, 25, 38, 50 */
        p_ptr->stat_add[A_STR] += amt;
        p_ptr->stat_add[A_DEX] += amt;
        p_ptr->stat_add[A_CON] += amt;
        p_ptr->skills.thn += 60*p_ptr->lev/50;
    }

    /* Hex bonuses */
    if (p_ptr->realm1 == REALM_HEX)
    {
        if (hex_spelling_any()) p_ptr->skills.stl -= (1 + p_ptr->magic_num2[0]);
        if (hex_spelling(HEX_DETECT_EVIL)) p_ptr->esp_evil = TRUE;
        if (hex_spelling(HEX_XTRA_MIGHT)) p_ptr->stat_add[A_STR] += 4;
        if (hex_spelling(HEX_BUILDING))
        {
            p_ptr->stat_add[A_STR] += 4;
            p_ptr->stat_add[A_DEX] += 4;
            p_ptr->stat_add[A_CON] += 4;
        }
        if (hex_spelling(HEX_DEMON_AURA))
        {
            p_ptr->sh_fire = TRUE;
            p_ptr->regenerate = TRUE;
        }
        if (hex_spelling(HEX_ICE_ARMOR))
        {
            p_ptr->sh_cold = TRUE; 
            p_ptr->to_a += 30;
            p_ptr->dis_to_a += 30;
        }
        if (hex_spelling(HEX_SHOCK_CLOAK))
        {
            p_ptr->sh_elec = TRUE;
            p_ptr->pspeed += 3;
        }
        for (i = EQUIP_BEGIN; i <= EQUIP_BEGIN + equip_count(); i++)
        {
            int ac = 0;
            o_ptr = equip_obj(i);
            if (!o_ptr) continue;
            if (!object_is_armour(o_ptr)) continue;
            if (!object_is_cursed(o_ptr)) continue;
            ac += 5;
            if (o_ptr->curse_flags & TRC_HEAVY_CURSE) ac += 7;
            if (o_ptr->curse_flags & TRC_PERMA_CURSE) ac += 13;
            p_ptr->to_a += ac;
            p_ptr->dis_to_a += ac;
        }
    }

    /* Apply temporary "stun" */
    if (p_ptr->stun > 50)
    {
        for (i = 0; i < MAX_HANDS; i++)
        {
            p_ptr->weapon_info[i].to_h -= 20;
            p_ptr->weapon_info[i].dis_to_h -= 20;
            p_ptr->weapon_info[i].to_d -= 20;
            p_ptr->weapon_info[i].dis_to_d -= 20;
        }
        p_ptr->shooter_info.to_h  -= 20;
        p_ptr->to_h_m  -= 20;
        p_ptr->shooter_info.dis_to_h  -= 20;
        p_ptr->to_d_m -= 20;
        p_ptr->shooter_info.to_d -= 20;
        p_ptr->shooter_info.dis_to_d -= 20;
    }
    else if (p_ptr->stun)
    {
        for (i = 0; i < MAX_HANDS; i++)
        {
            p_ptr->weapon_info[i].to_h -= 5;
            p_ptr->weapon_info[i].dis_to_h -= 5;
            p_ptr->weapon_info[i].to_d -= 5;
            p_ptr->weapon_info[i].dis_to_d -= 5;
        }
        p_ptr->shooter_info.to_h -= 5;
        p_ptr->to_h_m -= 5;
        p_ptr->shooter_info.dis_to_h -= 5;
        p_ptr->to_d_m -= 5;
        p_ptr->shooter_info.to_d -= 5;
        p_ptr->shooter_info.dis_to_d -= 5;
    }

    if (IS_WRAITH())
    {
        res_add_immune(RES_DARK);
        res_add_vuln(RES_LITE);
        p_ptr->reflect = TRUE;
        p_ptr->pass_wall = TRUE;
        p_ptr->no_passwall_dam = TRUE;
    }

    if (IS_PASSWALL())
    {
        p_ptr->pass_wall = TRUE;
        p_ptr->no_passwall_dam = TRUE;
    }

    if (IS_BLESSED())
    {
        for (i = 0; i < MAX_HANDS; i++)
        {
            p_ptr->weapon_info[i].to_h += 10;
            p_ptr->weapon_info[i].dis_to_h += 10;
        }
        p_ptr->to_a += 5;
        p_ptr->dis_to_a += 5;
        p_ptr->shooter_info.to_h  += 10;
        p_ptr->to_h_m  += 10;
        p_ptr->shooter_info.dis_to_h += 10;
    }

    if (p_ptr->magicdef)
    {
        res_add(RES_BLIND);
        res_add(RES_CONF);
        p_ptr->reflect = TRUE;
        p_ptr->free_act = TRUE;
        p_ptr->levitation = TRUE;
    }

    /* assert: equip_calc_bonuses() already called */
    if (IS_SHERO())
    {
        int ct = 0;
        int to_d = 3 + p_ptr->lev/5;

        for (i = 0; i < MAX_HANDS; i++)
        {
            if (p_ptr->weapon_info[i].wield_how != WIELD_NONE) ct++;
            p_ptr->weapon_info[i].to_h += 12;
            p_ptr->weapon_info[i].dis_to_h += 12;
        }

        for (i = 0; i < MAX_HANDS; i++)
        {
            if (p_ptr->weapon_info[i].wield_how == WIELD_NONE) continue;
            if (p_ptr->pclass == CLASS_BERSERKER || p_ptr->weapon_info[i].genji)
            {
                p_ptr->weapon_info[i].to_d += to_d;
                p_ptr->weapon_info[i].dis_to_h += to_d;
            }
            else
            {
                p_ptr->weapon_info[i].to_d += to_d / ct;
                p_ptr->weapon_info[i].dis_to_h += to_d / ct;
            }
        }
        p_ptr->weapon_info[0].to_h += 12;
        p_ptr->weapon_info[1].to_h += 12;
        p_ptr->shooter_info.to_h  -= 12;
        p_ptr->to_h_m  += 12;
        p_ptr->to_d_m  += 3+(p_ptr->lev/5);
        p_ptr->shooter_info.dis_to_h  -= 12;
        p_ptr->to_a -= 10;
        p_ptr->dis_to_a -= 10;
        p_ptr->skills.stl -= 7;
        p_ptr->skills.dev -= 20;
        p_ptr->skills.sav -= 30;
        p_ptr->skills.srh -= 15;
        p_ptr->skills.fos -= 15;
        p_ptr->skill_tht -= 20;
        p_ptr->skill_dig += 30;
    }

    if (IS_FAST())
        p_ptr->pspeed += 10;
    else if (p_ptr->tim_spurt)
        p_ptr->pspeed += 3;

    if (p_ptr->slow)
        p_ptr->pspeed -= 10;

    if (IS_TIM_ESP())
        p_ptr->telepathy = TRUE;

    if (p_ptr->tim_esp_magical)
        p_ptr->esp_magical = TRUE;

    if (p_ptr->ele_immune)
    {
        if (p_ptr->special_defense & DEFENSE_ACID)
            res_add_immune(RES_ACID);
        if (p_ptr->special_defense & DEFENSE_ELEC)
            res_add_immune(RES_ELEC);
        if (p_ptr->special_defense & DEFENSE_FIRE)
            res_add_immune(RES_FIRE);
        if (p_ptr->special_defense & DEFENSE_COLD)
            res_add_immune(RES_COLD);
    }

    if (p_ptr->tim_invis)
        p_ptr->see_inv = TRUE;

    if (IS_TIM_INFRA())
        p_ptr->see_infra+=3;

    if (p_ptr->tim_regen)
        p_ptr->regenerate = TRUE;

    if (p_ptr->tim_levitation)
        p_ptr->levitation = TRUE;

    if (p_ptr->tim_reflect)
        p_ptr->reflect = TRUE;

    if (p_ptr->telepathy != old_telepathy)
        p_ptr->update |= PU_MONSTERS;

    if ((p_ptr->esp_animal != old_esp_animal) ||
        (p_ptr->esp_undead != old_esp_undead) ||
        (p_ptr->esp_demon != old_esp_demon) ||
        (p_ptr->esp_orc != old_esp_orc) ||
        (p_ptr->esp_troll != old_esp_troll) ||
        (p_ptr->esp_giant != old_esp_giant) ||
        (p_ptr->esp_dragon != old_esp_dragon) ||
        (p_ptr->esp_human != old_esp_human) ||
        (p_ptr->esp_evil != old_esp_evil) ||
        (p_ptr->esp_good != old_esp_good) ||
        (p_ptr->esp_nonliving != old_esp_nonliving) ||
        (p_ptr->esp_unique != old_esp_unique) ||
        p_ptr->esp_magical != old_esp_magical )
    {
        p_ptr->update |= PU_MONSTERS;
    }

    if (p_ptr->see_inv != old_see_inv)
        p_ptr->update |= PU_MONSTERS;

    /* Call the class hook after scanning equipment but before calculating encumbrance, et. al.*/
    if (class_ptr->calc_bonuses != NULL)
        class_ptr->calc_bonuses();

    if (race_ptr->calc_bonuses != NULL)
        race_ptr->calc_bonuses();

    /* Temporary "Hero" ... moved after class processing for the Swordmaster */
    if (IS_HERO())
    {
        for (i = 0; i < MAX_HANDS; i++)
        {
            p_ptr->weapon_info[i].to_h += 12;
            p_ptr->weapon_info[i].dis_to_h += 12;
        }
        p_ptr->shooter_info.to_h  += 12;
        p_ptr->to_h_m  += 12;
        p_ptr->shooter_info.dis_to_h  += 12;
    }

    if (IS_HERO() || IS_SHERO())
        res_add(RES_FEAR);

    /* Calculate stats after calling class hook */
    for (i = 0; i < 6; i++)
    {
        int top, use, ind;

        top = modify_stat_value(p_ptr->stat_max[i], p_ptr->stat_add[i]);
        if (p_ptr->stat_top[i] != top)
        {
            p_ptr->stat_top[i] = top;
            p_ptr->redraw |= (PR_STATS);
            p_ptr->window |= (PW_PLAYER);
        }

        use = modify_stat_value(p_ptr->stat_cur[i], p_ptr->stat_add[i]);
        if (i == A_CHR && mut_present(MUT_ILL_NORM))
        {
            /* 10 to 18/90 charisma, guaranteed, based on level */
            if (use < 8 + 2 * p_ptr->lev)
                use = 8 + 2 * p_ptr->lev;
        }
        if (p_ptr->stat_use[i] != use)
        {
            p_ptr->stat_use[i] = use;
            p_ptr->redraw |= (PR_STATS);
            p_ptr->window |= (PW_PLAYER);
        }

        /* Values: 3, 4, ..., 17 */
        if (use <= 18) ind = (use - 3);

        /* Ranges: 18/00-18/09, ..., 18/210-18/219 */
        else if (use <= 18+219) ind = (15 + (use - 18) / 10);

        /* Range: 18/220+ */
        else ind = (37);

        if (p_ptr->stat_ind[i] != ind)
        {
            p_ptr->stat_ind[i] = ind;
            if (i == A_CON)
                p_ptr->update |= (PU_HP);
            else if (mp_ptr->spell_stat == i)
                p_ptr->update |= (PU_MANA | PU_SPELLS);
            p_ptr->window |= (PW_PLAYER);
        }
    }

    p_ptr->life += adj_con_mhp[p_ptr->stat_ind[A_CON]];
    if (p_ptr->life != old_life)
        p_ptr->update |= PU_HP;

    /* Bloating slows the player down (a little) */
    if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

    if (p_ptr->special_defense & KAMAE_SUZAKU) p_ptr->pspeed += 10;

    /* Barehanded Combat */
    for (i = 0; i < MAX_HANDS; i++)
    {
        if (p_ptr->weapon_info[i].wield_how != WIELD_NONE && p_ptr->weapon_info[i].bare_hands)
        {
            int skill = skills_martial_arts_current();
            p_ptr->weapon_info[i].to_h += (skill - WEAPON_EXP_BEGINNER) / 200;
            p_ptr->weapon_info[i].dis_to_h += (skill - WEAPON_EXP_BEGINNER) / 200;
        }
    }

    /* Dual Wielding */
    for (arm = 0; arm < MAX_ARMS; arm++)
    {
        int rhand = 2*arm;
        int lhand = 2*arm+1;
        object_type *robj = NULL, *lobj = NULL;

        if (p_ptr->weapon_info[rhand].wield_how != WIELD_NONE)
            robj = equip_obj(p_ptr->weapon_info[rhand].slot);

        if (p_ptr->weapon_info[lhand].wield_how != WIELD_NONE)
            lobj = equip_obj(p_ptr->weapon_info[lhand].slot);

        if (robj && lobj)
        {
            int pct, to_d, w1, w2;
            int w_div = 8;
            int skill = skills_dual_wielding_current();
            int class_idx = p_ptr->pclass;

            if (class_idx == CLASS_MONSTER)
                class_idx = race_ptr->pseudo_class_idx;

            if (p_ptr->tim_genji && skill < 7000)
                skill = 7000;

            /* Some classes (e.g. Berserkers) don't mind dual wielding with heavy weapons */
            switch (class_idx)
            {
            case CLASS_BERSERKER:
                w_div = 24;
                break;
            case CLASS_WARRIOR:
            case CLASS_BLOOD_KNIGHT:
                w_div = 18;
                break;
            case CLASS_SAMURAI:
            case CLASS_PALADIN:
                w_div = 12;
                break;
            }

            if (robj->name1 == ART_QUICKTHORN && lobj->name1 == ART_TINYTHORN)
            {
                p_ptr->weapon_info[rhand].genji = TRUE;
                p_ptr->weapon_info[lhand].genji = TRUE;
                p_ptr->pspeed += 7;
                p_ptr->to_a += 10;
                p_ptr->dis_to_a += 10;
            }

            w1 = robj->weight;
            w2 = lobj->weight;

            pct = 550 * skill/WEAPON_EXP_MASTER;

            if (p_ptr->weapon_info[rhand].genji)
            {
                pct += 150;
                if (w1 >= 130)
                    w1 -= (w1 - 130) / 2;

                if (w2 >= 130)
                    w2 -= (w2 - 130) / 2;
            }
            else if (mut_present(MUT_AMBIDEXTROUS))
                pct += 100;

            if (p_ptr->easy_2weapon)
                pct += 100;

            if ( lobj->tval == TV_SWORD
              && (lobj->sval == SV_MAIN_GAUCHE || lobj->sval == SV_WAKIZASHI) )
            {
                pct += 50;
            }

            p_ptr->weapon_info[rhand].dual_wield_pct = pct + 10 * (130 - w1) / w_div;
            p_ptr->weapon_info[lhand].dual_wield_pct = pct + 10 * (130 - w2) / w_div;

            if (robj->tval == TV_POLEARM && robj->weight > 100)
                p_ptr->weapon_info[rhand].dual_wield_pct -= 50;

            if (lobj->tval == TV_POLEARM && lobj->weight > 100)
                p_ptr->weapon_info[lhand].dual_wield_pct -= 50;

            if (robj->name1 == ART_MUSASI_KATANA && lobj->name1 == ART_MUSASI_WAKIZASI)
            {
                p_ptr->weapon_info[rhand].dual_wield_pct = 1000;
                p_ptr->weapon_info[lhand].dual_wield_pct = 1000;
            }
            else
            {
                if (robj->name1 == ART_MUSASI_KATANA)
                    p_ptr->weapon_info[rhand].dual_wield_pct = MAX(p_ptr->weapon_info[rhand].dual_wield_pct, 750);
                if (lobj->name1 == ART_MUSASI_WAKIZASI)
                    p_ptr->weapon_info[lhand].dual_wield_pct = MAX(p_ptr->weapon_info[lhand].dual_wield_pct, 750);
            }

            p_ptr->weapon_info[rhand].dual_wield_pct = MIN(MAX(100, p_ptr->weapon_info[rhand].dual_wield_pct), 1000);
            p_ptr->weapon_info[lhand].dual_wield_pct = MIN(MAX(100, p_ptr->weapon_info[lhand].dual_wield_pct), 1000);

            /* Cap Str bonus unless Genji */
            to_d = ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
            if (to_d > 0 && !p_ptr->weapon_info[rhand].genji && p_ptr->pclass != CLASS_BERSERKER)
            {
                int new_to_d = to_d * skill/WEAPON_EXP_MASTER;

                p_ptr->weapon_info[0].to_d -= (to_d - new_to_d);
                p_ptr->weapon_info[0].dis_to_d -= (to_d - new_to_d);
                p_ptr->weapon_info[1].to_d -= (to_d - new_to_d);
                p_ptr->weapon_info[1].dis_to_d -= (to_d - new_to_d);
            }
        }
    }

    /* Having more than one set of arms for combat is very powerful ... */
    for (i = 2; i < MAX_HANDS; i++)
    {
        if (p_ptr->weapon_info[i].wield_how != WIELD_NONE)
        {
            p_ptr->weapon_info[i].dual_wield_pct = p_ptr->weapon_info[i].dual_wield_pct * 90 / 100;
            if (p_ptr->weapon_info[i].dual_wield_pct < 100)
                p_ptr->weapon_info[i].dual_wield_pct = 100;
        }
    }


    /* Extract the current weight (in tenth pounds) */
    j = p_ptr->total_weight;

    if (!p_ptr->riding)
    {
        /* Extract the "weight limit" (in tenth pounds) */
        i = (int)weight_limit();
    }
    else
    {
        monster_type *riding_m_ptr = &m_list[p_ptr->riding];
        monster_race *riding_r_ptr = &r_info[riding_m_ptr->r_idx];
        int speed = riding_m_ptr->mspeed;
        int old_pspeed = p_ptr->pspeed;

        if (riding_m_ptr->mspeed > 110)
        {
            p_ptr->pspeed = 110 + (s16b)((speed - 110) * (skills_riding_current() * 3 + p_ptr->lev * 160L - 10000L) / (22000L));
            if (p_ptr->pspeed < 110) p_ptr->pspeed = 110;
        }
        else
        {
            p_ptr->pspeed = speed;
        }

        if (p_ptr->prace == RACE_MON_RING)
        {
            /* Hey, the player *is* a Ring of Speed, right? */
            if (old_pspeed > 110)
                p_ptr->pspeed += (old_pspeed - 110 + 2) / 3;
        }
        else
        {
            p_ptr->pspeed += (skills_riding_current() + p_ptr->lev *160)/3200;
        }
        if (MON_FAST(riding_m_ptr)) p_ptr->pspeed += 10;
        if (MON_SLOW(riding_m_ptr)) p_ptr->pspeed -= 10;
        riding_levitation = (riding_r_ptr->flags7 & RF7_CAN_FLY) ? TRUE : FALSE;
        if (riding_r_ptr->flags7 & (RF7_CAN_SWIM | RF7_AQUATIC)) p_ptr->can_swim = TRUE;

        if (!(riding_r_ptr->flags2 & RF2_PASS_WALL)) p_ptr->pass_wall = FALSE;
        if (riding_r_ptr->flags2 & RF2_KILL_WALL) p_ptr->kill_wall = TRUE;

        if (skills_riding_current() < RIDING_EXP_SKILLED) j += (150 * 3 * (RIDING_EXP_SKILLED - skills_riding_current())) / RIDING_EXP_SKILLED;

        /* Extract the "weight limit" */
        i = 1500 + riding_r_ptr->level * 25;
    }

    /* XXX XXX XXX Apply "encumbrance" from weight */
    if (j > i) p_ptr->pspeed -= ((j - i) / (i / 5));

    /* Searching slows the player down */
    if (p_ptr->action == ACTION_SEARCH) p_ptr->pspeed -= 10;


    /* Actual Modifier Bonuses (Un-inflate stat bonuses) */
    p_ptr->to_a += calc_adj_dex_ta();
    p_ptr->dis_to_a += calc_adj_dex_ta();

    p_ptr->to_d_m  += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
    p_ptr->to_h_m  += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
    p_ptr->to_h_m  += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);

    p_ptr->shooter_info.to_h  += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
    p_ptr->shooter_info.dis_to_h  += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
    p_ptr->shooter_info.to_h  += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);
    p_ptr->shooter_info.dis_to_h  += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);

    for (i = 0; i < MAX_HANDS; i++)
    {
        p_ptr->weapon_info[i].to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
        p_ptr->weapon_info[i].to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
        p_ptr->weapon_info[i].to_h += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);
        p_ptr->weapon_info[i].dis_to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
        p_ptr->weapon_info[i].dis_to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
        p_ptr->weapon_info[i].dis_to_h += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);
    }

    /* Obtain the "hold" value */
    hold = adj_str_hold[p_ptr->stat_ind[A_STR]];

    /* Examine the "current bow" */
    p_ptr->shooter_info.slot = equip_find_object(TV_BOW, SV_ANY);
    if (p_ptr->shooter_info.slot)
    {
        o_ptr = equip_obj(p_ptr->shooter_info.slot);

        if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS))
        {
            int idx = p_ptr->stat_ind[A_STR] + 4;
            if (idx > 40-3)
                idx = 40-3;
            hold = adj_str_hold[idx];
        }

        /* It is hard to carry a heavy bow */
        if (hold < o_ptr->weight / 10)
        {
            p_ptr->shooter_info.to_h  += 2 * (hold - o_ptr->weight / 10);
            p_ptr->shooter_info.dis_to_h  += 2 * (hold - o_ptr->weight / 10);
            p_ptr->shooter_info.heavy_shoot = TRUE;
            p_ptr->shooter_info.num_fire = 100;
        }

        /* Hack: hold is also used for inventory weapons and other stuff, so undo
           the Crossbowmaster adjustment */
        if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS))
        {
            hold = adj_str_hold[p_ptr->stat_ind[A_STR]];
        }

        switch (o_ptr->sval)
        {
            case SV_SLING:
            {
                p_ptr->shooter_info.tval_ammo = TV_SHOT;
                break;
            }

            case SV_SHORT_BOW:
            case SV_LONG_BOW:
            case SV_NAMAKE_BOW:
            {
                p_ptr->shooter_info.tval_ammo = TV_ARROW;
                break;
            }

            case SV_LIGHT_XBOW:
            case SV_HEAVY_XBOW:
            {
                p_ptr->shooter_info.tval_ammo = TV_BOLT;
                break;
            }
            case SV_CRIMSON:
            case SV_RAILGUN:
            case SV_HARP:
            {
                p_ptr->shooter_info.tval_ammo = TV_NO_AMMO;
                break;
            }
        }

        if (race_ptr != NULL && race_ptr->calc_shooter_bonuses != NULL)
            race_ptr->calc_shooter_bonuses(o_ptr, &p_ptr->shooter_info);

        if (class_ptr != NULL && class_ptr->calc_shooter_bonuses != NULL)
            class_ptr->calc_shooter_bonuses(o_ptr, &p_ptr->shooter_info);

        /* TODO: Convert old code to use class_ptr->calc_shooter_bonuses() */
        if (!p_ptr->shooter_info.heavy_shoot)
        {
            if (p_ptr->pclass == CLASS_WARLOCK &&
                p_ptr->psubclass == PACT_ABERRATION &&
                p_ptr->shooter_info.tval_ammo <= TV_BOLT &&
                p_ptr->shooter_info.tval_ammo >= TV_SHOT)
            {
                p_ptr->shooter_info.num_fire += (p_ptr->lev * 2);
            }
        }
        if (p_ptr->shooter_info.num_fire < 0) p_ptr->shooter_info.num_fire = 0;
    }

    /* Blows Calculation */
    for (i = 0; i < MAX_HANDS; i++)
    {
        weapon_info_t *info_ptr = &p_ptr->weapon_info[i];
        int            tmp_hold = hold;        

        if (info_ptr->wield_how == WIELD_NONE) continue;

        if (p_ptr->tim_weaponmastery) /* Note: Monks won't have an equipped weapon, but should still gain weaponmastery! */
            info_ptr->to_dd += p_ptr->lev/23;

        o_ptr = equip_obj(info_ptr->slot);
        if (!o_ptr) continue;
        
        object_flags(o_ptr, flgs);

        if (p_ptr->tim_enlarge_weapon)
        {
            /* Hack: At the moment, only the Monkey King's Cudgel ('Ruyi Jingu Bang')
             * should have this power, and it really should only enlarge that weapon (So
             * no swapping, or dual wielding, or whatever) */
            if (o_ptr->name1 == ART_MONKEY_KING)
            {
                info_ptr->to_dd += 2;
                info_ptr->to_ds += 2;

                info_ptr->dis_to_h -= 20;
                info_ptr->to_h -= 20;
            }
        }

        if (info_ptr->wield_how == WIELD_TWO_HANDS)
            tmp_hold *= 2;

        if (tmp_hold < o_ptr->weight / 10)
        {
            info_ptr->to_h += 2 * (tmp_hold - o_ptr->weight / 10);
            info_ptr->dis_to_h += 2 * (tmp_hold - o_ptr->weight / 10);
            info_ptr->heavy_wield = TRUE;
        }
        else if (info_ptr->wield_how == WIELD_TWO_HANDS && tmp_hold < o_ptr->weight/5) 
        {
            if (p_ptr->pclass != CLASS_MAULER)
                info_ptr->omoi = TRUE;
        }

        if ( i % 2 == 1 
          && o_ptr->tval == TV_SWORD 
          && (o_ptr->sval == SV_MAIN_GAUCHE || o_ptr->sval == SV_WAKIZASHI) )
        {
            p_ptr->to_a += 5;
            p_ptr->dis_to_a += 5;
        }


        if (class_ptr->calc_weapon_bonuses)
            class_ptr->calc_weapon_bonuses(o_ptr, info_ptr);
        if (race_ptr->calc_weapon_bonuses)
            race_ptr->calc_weapon_bonuses(o_ptr, info_ptr);

        /* Normal weapons */
        if (!info_ptr->heavy_wield)
        {
            info_ptr->base_blow = calculate_base_blows(i, p_ptr->stat_ind[A_STR], p_ptr->stat_ind[A_DEX]);

            if (p_ptr->tim_speed_essentia && p_ptr->pclass != CLASS_MAULER)
                info_ptr->xtra_blow += 200;

            if (p_ptr->special_defense & KATA_FUUJIN) info_ptr->xtra_blow -= 100;

            if (o_ptr->tval == TV_SWORD && o_ptr->sval == SV_POISON_NEEDLE) 
            {
                info_ptr->base_blow = 100;
                info_ptr->xtra_blow = 0;
            }

            if (NUM_BLOWS(i) < 0)
            {
                info_ptr->base_blow = 0;
                info_ptr->xtra_blow = 0;
            }

            p_ptr->skill_dig += (o_ptr->weight / 10);
        }

        /* Two Handed wielding bonus */
        if ( p_ptr->weapon_info[i].wield_how == WIELD_TWO_HANDS 
          && p_ptr->pclass != CLASS_DUELIST 
          && !p_ptr->weapon_info[i].omoi )
        {
            int bonus_to_h=0, bonus_to_d=0;
            bonus_to_d = ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128) * 3/4;
            bonus_to_h = ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128) + ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);

            p_ptr->weapon_info[i].to_h += MAX(bonus_to_h,1);
            p_ptr->weapon_info[i].dis_to_h += MAX(bonus_to_h,1);
            p_ptr->weapon_info[i].to_d += MAX(bonus_to_d,1);
            p_ptr->weapon_info[i].dis_to_d += MAX(bonus_to_d,1);
        }

        /* Priest weapon penalty for non-blessed edged weapons */
        if ((p_ptr->pclass == CLASS_PRIEST) && (!(have_flag(flgs, TR_BLESSED))) &&
            ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
        {
            /* Reduce the real bonuses */
            p_ptr->weapon_info[i].to_h -= 2;
            p_ptr->weapon_info[i].to_d -= 2;

            /* Reduce the mental bonuses */
            p_ptr->weapon_info[i].dis_to_h -= 2;
            p_ptr->weapon_info[i].dis_to_d -= 2;

            /* Icky weapon */
            p_ptr->weapon_info[i].icky_wield = TRUE;
        }
        /* Hex bonuses */
        if (p_ptr->realm1 == REALM_HEX)
        {
            if (object_is_cursed(o_ptr))
            {
                if (o_ptr->curse_flags & (TRC_CURSED)) { p_ptr->weapon_info[i].to_h += 5; p_ptr->weapon_info[i].dis_to_h += 5; }
                if (o_ptr->curse_flags & (TRC_HEAVY_CURSE)) { p_ptr->weapon_info[i].to_h += 7; p_ptr->weapon_info[i].dis_to_h += 7; }
                if (o_ptr->curse_flags & (TRC_PERMA_CURSE)) { p_ptr->weapon_info[i].to_h += 13; p_ptr->weapon_info[i].dis_to_h += 13; }
                if (o_ptr->curse_flags & (TRC_TY_CURSE)) { p_ptr->weapon_info[i].to_h += 5; p_ptr->weapon_info[i].dis_to_h += 5; }
                if (hex_spelling(HEX_RUNESWORD))
                {
                    if (o_ptr->curse_flags & (TRC_CURSED)) { p_ptr->weapon_info[i].to_d += 5; p_ptr->weapon_info[i].dis_to_d += 5; }
                    if (o_ptr->curse_flags & (TRC_HEAVY_CURSE)) { p_ptr->weapon_info[i].to_d += 7; p_ptr->weapon_info[i].dis_to_d += 7; }
                    if (o_ptr->curse_flags & (TRC_PERMA_CURSE)) { p_ptr->weapon_info[i].to_d += 13; p_ptr->weapon_info[i].dis_to_d += 13; }
                }
            }
        }
        if (p_ptr->riding)
        {
            if ((o_ptr->tval == TV_POLEARM) && ((o_ptr->sval == SV_LANCE) || (o_ptr->sval == SV_HEAVY_LANCE)))
            {
                p_ptr->weapon_info[i].to_h +=15;
                p_ptr->weapon_info[i].dis_to_h +=15;
                p_ptr->weapon_info[i].to_dd += 2;
            }
            else if (!(have_flag(flgs, TR_RIDING)))
            {
                int penalty;
                if ((p_ptr->pclass == CLASS_BEASTMASTER) || (p_ptr->pclass == CLASS_CAVALRY))
                {
                    penalty = 5;
                }
                else
                {
                    penalty = r_info[m_list[p_ptr->riding].r_idx].level - skills_riding_current() / 80;
                    penalty += 30;
                    if (penalty < 30) penalty = 30;
                }
                p_ptr->weapon_info[i].to_h -= penalty;
                p_ptr->weapon_info[i].dis_to_h -= penalty;

                /* Riding weapon */
                p_ptr->weapon_info[i].riding_wield = TRUE;
            }
        }
    }

    /* Stats need to be set for proper blows calculation. */
    if (race_ptr->calc_innate_attacks)
        race_ptr->calc_innate_attacks();

    /* Adjust Innate Attacks for Proficiency */
    for (i = 0; i < p_ptr->innate_attack_ct; i++)
    {
        innate_attack_ptr attack = &p_ptr->innate_attacks[i];
        cptr              name = skills_innate_calc_name(attack);
        int               bonus = skills_innate_calc_bonus(name);

        attack->to_h += bonus;
    }

    /* Kamikaze Warrior with a Monster Race/Possessor */
    if (!p_ptr->weapon_ct && p_ptr->tim_speed_essentia)
        p_ptr->innate_attack_info.xtra_blow += 200;

    if (p_ptr->riding)
    {
        int penalty = 0;
        if ((p_ptr->pclass == CLASS_BEASTMASTER) || (p_ptr->pclass == CLASS_CAVALRY))
        {
            if (p_ptr->shooter_info.tval_ammo != TV_ARROW) penalty = 5;
        }
        else
        {
            penalty = r_info[m_list[p_ptr->riding].r_idx].level - skills_riding_current() / 80;
            penalty += 30;
            if (penalty < 30) penalty = 30;
        }
        if (p_ptr->shooter_info.tval_ammo == TV_BOLT) penalty *= 2;
        p_ptr->shooter_info.to_h -= penalty;
        p_ptr->shooter_info.dis_to_h -= penalty;
    }

    /* Different calculation for monks with empty hands */
    for (i = 0; i < MAX_HANDS; i++)
    {
        int arm = i / 2;
        if (p_ptr->weapon_info[i].wield_how != WIELD_NONE && p_ptr->weapon_info[i].bare_hands)
        {
            int blow_base = p_ptr->lev + adj_dex_blow[p_ptr->stat_ind[A_DEX]];
            p_ptr->weapon_info[i].base_blow = 100;

            if (p_ptr->pclass == CLASS_FORCETRAINER)
            {
                p_ptr->weapon_info[i].base_blow += MIN(400, 400 * blow_base / 60);
                if (p_ptr->magic_num1[0])
                {
                    p_ptr->weapon_info[i].to_d += (p_ptr->magic_num1[0]/5);
                    p_ptr->weapon_info[i].dis_to_d += (p_ptr->magic_num1[0]/5);
                }
            }
            else if (p_ptr->pclass == CLASS_MYSTIC)
            {
                p_ptr->weapon_info[i].base_blow += MIN(500, 500 * blow_base / 60);
            }
            else
            {
                p_ptr->weapon_info[i].base_blow += MIN(600, 600 * blow_base / 60);
            }

            if (heavy_armor() && (p_ptr->pclass != CLASS_BERSERKER))
                p_ptr->weapon_info[i].base_blow  /= 2;
            else
            {
                p_ptr->weapon_info[i].to_h += (p_ptr->lev / 3);
                p_ptr->weapon_info[i].dis_to_h += (p_ptr->lev / 3);

                p_ptr->weapon_info[i].to_d += (p_ptr->lev / 6);
                p_ptr->weapon_info[i].dis_to_d += (p_ptr->lev / 6);
            }

            p_ptr->weapon_info[i].base_blow -= arm*100;
            if (p_ptr->weapon_info[i].base_blow <= 0)
                p_ptr->weapon_info[i].base_blow = 100;

            if (p_ptr->weapon_info[i].wield_how == WIELD_TWO_HANDS)
            {
                int bonus_to_h=0, bonus_to_d=0;
                bonus_to_d = ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128) * 3/4;
                bonus_to_h = ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128) + ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);

                p_ptr->weapon_info[i].to_h += MAX(bonus_to_h,1);
                p_ptr->weapon_info[i].dis_to_h += MAX(bonus_to_h,1);
                p_ptr->weapon_info[i].to_d += MAX(bonus_to_d,1);
                p_ptr->weapon_info[i].dis_to_d += MAX(bonus_to_d,1);
            }
        }
    }

    monk_posture_calc_bonuses();

    /* Hack for Wild Talents assuming Monk Postures */
    if (p_ptr->pclass == CLASS_WILD_TALENT)
    {
        if (p_ptr->special_defense & KAMAE_GENBU)
        {
            for (i = 0; i < MAX_HANDS; i++)
                p_ptr->weapon_info[0].xtra_blow -= 200;
        }
        else if (p_ptr->special_defense & KAMAE_BYAKKO)
        {
            /* Hack: This should "strengthen your attacks" */
            for (i = 0; i < MAX_HANDS; i++)
                p_ptr->weapon_info[i].xtra_blow += 100;
        }
        else if (p_ptr->special_defense & KAMAE_SUZAKU)
        {
            for (i = 0; i < MAX_HANDS; i++)
            {
                p_ptr->weapon_info[i].to_h -= (p_ptr->lev / 3);
                p_ptr->weapon_info[i].to_d -= (p_ptr->lev / 6);

                p_ptr->weapon_info[i].dis_to_h -= (p_ptr->lev / 3);
                p_ptr->weapon_info[i].dis_to_d -= (p_ptr->lev / 6);
                p_ptr->weapon_info[i].base_blow /= 2;
            }
        }
    }

    if (p_ptr->riding && p_ptr->prace != RACE_MON_RING) 
        p_ptr->levitation = riding_levitation;

    monk_armour_aux = FALSE;

    if (heavy_armor())
    {
        monk_armour_aux = TRUE;
    }

    /* Weapon Skills */
    for (i = 0; i < MAX_HANDS; i++)
    {
        object_type *obj = NULL;
        if (p_ptr->weapon_info[i].wield_how != WIELD_NONE)
            obj = equip_obj(p_ptr->weapon_info[i].slot);

        if (obj)
        {
            if (obj->tval == TV_SHIELD)
            {
                int skill = 6000;
                p_ptr->weapon_info[i].to_h += (skill - WEAPON_EXP_BEGINNER) / 200;
                p_ptr->weapon_info[i].dis_to_h += (skill - WEAPON_EXP_BEGINNER) / 200;
            }
            else
            {
                int skill = skills_weapon_current(obj->tval, obj->sval);
                p_ptr->weapon_info[i].to_h += (skill - WEAPON_EXP_BEGINNER) / 200;
                p_ptr->weapon_info[i].dis_to_h += (skill - WEAPON_EXP_BEGINNER) / 200;
                if (p_ptr->pclass == CLASS_MONK || p_ptr->pclass == CLASS_FORCETRAINER || p_ptr->pclass == CLASS_MYSTIC)
                {
                    if (skills_weapon_is_icky(obj->tval, obj->sval))
                    {
                        p_ptr->weapon_info[i].to_h -= 40;
                        p_ptr->weapon_info[i].dis_to_h -= 40;
                        p_ptr->weapon_info[i].icky_wield = TRUE;
                    }
                }
            }
            if (obj->name1 == ART_IRON_BALL) p_ptr->align -= 1000;
        }
    }

    /* Maximum speed is (+99). (internally it's 110 + 99) */
    /* Temporary lightspeed forces to be maximum speed */
    if ((IS_LIGHT_SPEED() && !p_ptr->riding) || (p_ptr->pspeed > 209))
        p_ptr->pspeed = 209;

    /* Minimum speed is (-99). (internally it's 110 - 99) */
    if (p_ptr->pspeed < 11)
        p_ptr->pspeed = 11;

    /* Display the speed (if needed) */
    if (p_ptr->pspeed != old_speed)
        p_ptr->redraw |= PR_SPEED;

    /* Robe of the Twilight forces AC to 0 */
    if (equip_find_ego(EGO_ROBE_TWILIGHT))
    {
        if (p_ptr->to_a > (0 - p_ptr->ac))
            p_ptr->to_a = 0 - p_ptr->ac;
        if (p_ptr->dis_to_a > (0 - p_ptr->dis_ac))
            p_ptr->dis_to_a = 0 - p_ptr->dis_ac;
    }

    /* Redraw armor (if needed) */
    if (p_ptr->dis_ac != old_dis_ac || p_ptr->dis_to_a != old_dis_to_a)
    {
        p_ptr->redraw |= PR_ARMOR;
        p_ptr->window |= PW_PLAYER;
    }

    /* Affect Skill -- stealth (bonus one) */
    p_ptr->skills.stl += 1;

    if (IS_TIM_STEALTH()) p_ptr->skills.stl += 99;
    if (p_ptr->action == ACTION_STALK) p_ptr->skills.stl += (p_ptr->lev+2)/3;

    if (p_ptr->tim_dark_stalker)
        p_ptr->skills.stl += 3 + p_ptr->lev/5;

    /* Affect Skill -- disarming (DEX and INT) */
    p_ptr->skills.dis += adj_dex_dis[p_ptr->stat_ind[A_DEX]];
    p_ptr->skills.dis += adj_int_dis[p_ptr->stat_ind[A_INT]];

    /* Affect Skill -- magic devices (INT) */
    p_ptr->skills.dev += adj_int_dev[p_ptr->stat_ind[A_INT]];

    /* Affect Skill -- saving throw (WIS) */
    p_ptr->skills.sav += adj_wis_sav[p_ptr->stat_ind[A_WIS]];

    /* Affect Skill -- digging (STR) */
    p_ptr->skill_dig += adj_str_dig[p_ptr->stat_ind[A_STR]];

    if ( p_ptr->fairy_stealth 
      && p_ptr->personality != PERS_SEXY 
      && (p_ptr->cursed & TRC_AGGRAVATE) )
    {
        p_ptr->cursed &= ~(TRC_AGGRAVATE);
        p_ptr->skills.stl = MIN(p_ptr->skills.stl - 3, (p_ptr->skills.stl + 2) / 2);
    }

    /* Peerless Stealth is just like the Shadow Fairy, but can even negate the
       aggravation of Sexy characters! */
    if (p_ptr->peerless_stealth && p_ptr->cursed & TRC_AGGRAVATE)
    {
        p_ptr->cursed &= ~(TRC_AGGRAVATE);
        p_ptr->skills.stl = MIN(p_ptr->skills.stl - 3, (p_ptr->skills.stl + 2) / 2);
    }

    /* Limit Skill -- stealth from 0 to 30 */
    if (p_ptr->skills.stl > 30) p_ptr->skills.stl = 30;
    if (p_ptr->skills.stl < 0) p_ptr->skills.stl = 0;

    /* Limit Skill -- digging from 1 up */
    if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

    if (p_ptr->anti_magic && (p_ptr->skills.sav < (90 + p_ptr->lev))) p_ptr->skills.sav = 90 + p_ptr->lev;

    /* Kutar's Expand Horizontally */
    if (p_ptr->tsubureru) p_ptr->skills.sav = 10;

    if ((p_ptr->ult_res || IS_RESIST_MAGIC() || p_ptr->magicdef) && (p_ptr->skills.sav < (95 + p_ptr->lev))) p_ptr->skills.sav = 95 + p_ptr->lev;

    if (enable_virtues)
    {
        for (i = 0, j = 0; i < 8; i++)
        {
            switch (p_ptr->vir_types[i])
            {
            case VIRTUE_JUSTICE:
                p_ptr->align += p_ptr->virtues[i] * 2;
                break;
            case VIRTUE_CHANCE:
                /* Do nothing */
                break;
            case VIRTUE_NATURE:
            case VIRTUE_HARMONY:
                neutral[j++] = i;
                break;
            case VIRTUE_UNLIFE:
                p_ptr->align -= p_ptr->virtues[i];
                break;
            default:
                p_ptr->align += p_ptr->virtues[i];
                break;
            }
        }

        for (i = 0; i < j; i++)
        {
            if (p_ptr->align > 0)
            {
                p_ptr->align -= p_ptr->virtues[neutral[i]] / 2;
                if (p_ptr->align < 0) p_ptr->align = 0;
            }
            else if (p_ptr->align < 0)
            {
                p_ptr->align += p_ptr->virtues[neutral[i]] / 2;
                if (p_ptr->align > 0) p_ptr->align = 0;
            }
        }
    }

    /* Hack -- handle "xtra" mode */
    if (character_xtra) return;

    /* Take note when "heavy bow" changes */
    if (p_ptr->old_heavy_shoot != p_ptr->shooter_info.heavy_shoot)
    {
        if (p_ptr->shooter_info.heavy_shoot)
            msg_print("You have trouble wielding such a heavy bow.");
        else if (equip_find_object(TV_BOW, SV_ANY))
            msg_print("You have no trouble wielding your bow.");
        else
            msg_print("You feel relieved to put down your heavy bow.");

        p_ptr->old_heavy_shoot = p_ptr->shooter_info.heavy_shoot;
    }

    for (i = 0 ; i < MAX_HANDS ; i++)
    {
        if (p_ptr->old_heavy_wield[i] != p_ptr->weapon_info[i].heavy_wield)
        {
            if (p_ptr->weapon_info[i].heavy_wield)
                msg_print("You have trouble wielding such a heavy weapon.");
            else if (p_ptr->weapon_info[i].wield_how != WIELD_NONE)
                msg_print("You have no trouble wielding your weapon.");
            else
                msg_print("You feel relieved to put down your heavy weapon.");

            p_ptr->old_heavy_wield[i] = p_ptr->weapon_info[i].heavy_wield;
        }

        if (p_ptr->old_riding_wield[i] != p_ptr->weapon_info[i].riding_wield)
        {
            if (p_ptr->weapon_info[i].riding_wield)
                msg_print("This weapon is not suitable for use while riding.");
            else if (!p_ptr->riding)
                msg_print("This weapon was not suitable for use while riding.");
            else if (p_ptr->weapon_info[i].wield_how != WIELD_NONE)
                msg_print("This weapon is suitable for use while riding.");

            p_ptr->old_riding_wield[i] = p_ptr->weapon_info[i].riding_wield;
        }

        if (p_ptr->old_icky_wield[i] != p_ptr->weapon_info[i].icky_wield)
        {
            if (p_ptr->weapon_info[i].icky_wield)
            {
                msg_print("You do not feel comfortable with your weapon.");
                if (hack_mind)
                    virtue_add(VIRTUE_FAITH, -1);
            }
            else if (p_ptr->weapon_info[i].wield_how != WIELD_NONE)
                msg_print("You feel comfortable with your weapon.");
            else
                msg_print("You feel more comfortable after removing your weapon.");

            p_ptr->old_icky_wield[i] = p_ptr->weapon_info[i].icky_wield;
        }
    }

    if (p_ptr->riding && (p_ptr->old_riding_ryoute != p_ptr->riding_ryoute))
    {
        if (p_ptr->riding_ryoute)
            msg_print("You are using all hands for fighting and you can't control your mount.");
        else
            msg_print("You begin to control your mount with one hand.");

        p_ptr->old_riding_ryoute = p_ptr->riding_ryoute;
    }

    if ((p_ptr->pclass == CLASS_MONK
      || p_ptr->pclass == CLASS_MYSTIC 
      || p_ptr->pclass == CLASS_FORCETRAINER
      || p_ptr->pclass == CLASS_NINJA
      || p_ptr->pclass == CLASS_SCOUT) && (monk_armour_aux != monk_notify_aux))
    {
        if (heavy_armor())
        {
            msg_print("The weight of your armor disrupts your balance.");
            if (hack_mind)
                virtue_add(VIRTUE_HARMONY, -1);
        }
        else
            msg_print("You regain your balance.");

        monk_notify_aux = monk_armour_aux;
    }

    /* Can someone please explain the purpose of this?
    if (p_ptr->pass_wall && !p_ptr->kill_wall) p_ptr->no_flowed = TRUE;
    */

    /* Apply some maximums ... */
    if ( p_ptr->magic_resistance > 15 
      && !prace_is_(RACE_MON_GOLEM) 
      && !prace_is_(MIMIC_MIST)
      && !prace_is_(RACE_MON_POSSESSOR)
      && !prace_is_(RACE_MON_MIMIC) )
    {
        p_ptr->magic_resistance = 15;
    }

    /* Hack: Vicious Strike should not put AC below 0, but I can't find out a better
       way to achieve this! */
    if (p_ptr->ac + p_ptr->to_a < 0 && p_ptr->tim_vicious_strike)
    {
        int delta = -(p_ptr->ac + p_ptr->to_a);
        p_ptr->to_a += delta;
        p_ptr->dis_to_a += delta;
    }
}



/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(void)
{
    /* Notice stuff */
    if (!p_ptr->notice) return;


    /* Actually do auto-destroy */
    if (p_ptr->notice & (PN_AUTODESTROY))
    {
        p_ptr->notice &= ~(PN_AUTODESTROY);
        autopick_delayed_alter();
    }

    /* Combine the pack */
    if (p_ptr->notice & (PN_COMBINE))
    {
        p_ptr->notice &= ~(PN_COMBINE);
        combine_pack();
    }

    /* Reorder the pack */
    if (p_ptr->notice & (PN_REORDER))
    {
        p_ptr->notice &= ~(PN_REORDER);
        reorder_pack();
    }
}


/*
 * Handle "p_ptr->update"
 */
void update_stuff(void)
{
    /* Update stuff */
    if (!p_ptr->update) return;


    if (p_ptr->update & (PU_BONUS))
    {
        p_ptr->update &= ~(PU_BONUS);
        calc_bonuses();
    }

    if (p_ptr->update & (PU_TORCH))
    {
        p_ptr->update &= ~(PU_TORCH);
        calc_torch();
    }

    if (p_ptr->update & (PU_HP))
    {
        p_ptr->update &= ~(PU_HP);
        calc_hitpoints();
    }

    if (p_ptr->update & (PU_MANA))
    {
        p_ptr->update &= ~(PU_MANA);
        calc_mana();
    }

    if (p_ptr->update & (PU_SPELLS))
    {
        p_ptr->update &= ~(PU_SPELLS);
        calc_spells();
    }


    /* Character is not ready yet, no screen updates */
    if (!character_generated) return;


    /* Character is in "icky" mode, no screen updates */
    if (character_icky) return;


    if (p_ptr->update & (PU_UN_LITE))
    {
        p_ptr->update &= ~(PU_UN_LITE);
        forget_lite();
    }

    if (p_ptr->update & (PU_UN_VIEW))
    {
        p_ptr->update &= ~(PU_UN_VIEW);
        forget_view();
    }

    if (p_ptr->update & (PU_VIEW))
    {
        p_ptr->update &= ~(PU_VIEW);
        update_view();
    }

    if (p_ptr->update & (PU_LITE))
    {
        p_ptr->update &= ~(PU_LITE);
        update_lite();
    }


    if (p_ptr->update & (PU_FLOW))
    {
        p_ptr->update &= ~(PU_FLOW);
        update_flow();
    }

    if (p_ptr->update & (PU_DISTANCE))
    {
        p_ptr->update &= ~(PU_DISTANCE);

        /* Still need to call update_monsters(FALSE) after update_mon_lite() */ 
        /* p_ptr->update &= ~(PU_MONSTERS); */

        update_monsters(TRUE);
    }

    if (p_ptr->update & (PU_MON_LITE))
    {
        p_ptr->update &= ~(PU_MON_LITE);
        update_mon_lite();
    }

    /*
     * Mega-Hack -- Delayed visual update
     * Only used if update_view(), update_lite() or update_mon_lite() was called
     */
    if (p_ptr->update & (PU_DELAY_VIS))
    {
        p_ptr->update &= ~(PU_DELAY_VIS);
        delayed_visual_update();
    }

    if (p_ptr->update & (PU_MONSTERS))
    {
        p_ptr->update &= ~(PU_MONSTERS);
        update_monsters(FALSE);
    }
}


/*
 * Handle "p_ptr->redraw"
 */
void redraw_stuff(void)
{
    /* Redraw stuff */
    if (!p_ptr->redraw) return;


    /* Character is not ready yet, no screen updates */
    if (!character_generated) return;


    /* Character is in "icky" mode, no screen updates */
    if (character_icky) return;



    /* Hack -- clear the screen */
    if (p_ptr->redraw & (PR_WIPE))
    {
        p_ptr->redraw &= ~(PR_WIPE);
        msg_print(NULL);
        Term_clear();
    }


    if (p_ptr->redraw & (PR_MAP))
    {
        p_ptr->redraw &= ~(PR_MAP);
        prt_map();
    }


    if (p_ptr->redraw & (PR_BASIC))
    {
        p_ptr->redraw &= ~(PR_BASIC);
        p_ptr->redraw &= ~(PR_MISC | PR_TITLE | PR_STATS);
        p_ptr->redraw &= ~(PR_LEV | PR_EXP | PR_GOLD);
        p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA);
        p_ptr->redraw &= ~(PR_DEPTH | PR_HEALTH | PR_UHEALTH);
        prt_frame_basic();
        prt_time();
        prt_dungeon();
    }

    if (p_ptr->redraw & (PR_EQUIPPY))
    {
        p_ptr->redraw &= ~(PR_EQUIPPY);
        print_equippy(); /* To draw / delete equippy chars */
    }

    if (p_ptr->redraw & (PR_MISC))
    {
        char buf[100];
        race_t *race_ptr = get_race_t();
        p_ptr->redraw &= ~(PR_MISC);
        if (race_ptr->mimic)
            sprintf(buf, "[%s]", race_ptr->name);
        else
            sprintf(buf, "%s", race_ptr->name);
        prt_field(buf, ROW_RACE, COL_RACE);
    }

    if (p_ptr->redraw & (PR_TITLE))
    {
        p_ptr->redraw &= ~(PR_TITLE);
        prt_title();
    }

    if (p_ptr->redraw & (PR_LEV))
    {
        p_ptr->redraw &= ~(PR_LEV);
        prt_level();
    }

    if (p_ptr->redraw & (PR_EXP))
    {
        p_ptr->redraw &= ~(PR_EXP);
        prt_exp();
    }

    if (p_ptr->redraw & (PR_STATS))
    {
        p_ptr->redraw &= ~(PR_STATS);
        prt_stat(A_STR);
        prt_stat(A_INT);
        prt_stat(A_WIS);
        prt_stat(A_DEX);
        prt_stat(A_CON);
        prt_stat(A_CHR);
    }

    if (p_ptr->redraw & (PR_STATUS))
    {
        p_ptr->redraw &= ~(PR_STATUS);
        prt_status();
    }

    if (p_ptr->redraw & (PR_ARMOR))
    {
        p_ptr->redraw &= ~(PR_ARMOR);
        prt_ac();
    }

    if (p_ptr->redraw & (PR_HP))
    {
        p_ptr->redraw &= ~(PR_HP);
        prt_hp();
    }

    if (p_ptr->redraw & (PR_MANA))
    {
        p_ptr->redraw &= ~(PR_MANA);
        prt_sp();
    }

    if (p_ptr->redraw & (PR_GOLD))
    {
        p_ptr->redraw &= ~(PR_GOLD);
        prt_gold();
    }

    if (p_ptr->redraw & (PR_DEPTH))
    {
        p_ptr->redraw &= ~(PR_DEPTH);
        prt_depth();
    }

    if (p_ptr->redraw & (PR_HEALTH))
    {
        p_ptr->redraw &= ~(PR_HEALTH);
        health_redraw(FALSE);
    }

    if (p_ptr->redraw & (PR_UHEALTH))
    {
        p_ptr->redraw &= ~(PR_UHEALTH);
        health_redraw(TRUE);
    }


    if (p_ptr->redraw & (PR_EXTRA))
    {
        p_ptr->redraw &= ~(PR_EXTRA);
        p_ptr->redraw &= ~(PR_CUT | PR_STUN | PR_FEAR);
        p_ptr->redraw &= ~(PR_HUNGER);
        p_ptr->redraw &= ~(PR_STATE | PR_SPEED | PR_STUDY | PR_IMITATION | PR_STATUS);
        prt_frame_extra();
    }

    if (p_ptr->redraw & (PR_CUT))
    {
        p_ptr->redraw &= ~(PR_CUT);
        prt_cut();
    }

    if (p_ptr->redraw & (PR_STUN))
    {
        p_ptr->redraw &= ~(PR_STUN);
        prt_stun();
    }

    if (p_ptr->redraw & PR_FEAR)
    {
        p_ptr->redraw &= ~PR_FEAR;
        prt_fear();
    }

    if (p_ptr->redraw & (PR_HUNGER))
    {
        p_ptr->redraw &= ~(PR_HUNGER);
        prt_hunger();
    }

    if (p_ptr->redraw & (PR_STATE))
    {
        p_ptr->redraw &= ~(PR_STATE);
        prt_state();
    }

    if (p_ptr->redraw & (PR_SPEED))
    {
        p_ptr->redraw &= ~(PR_SPEED);
        prt_speed();
    }

    if (p_ptr->pclass == CLASS_IMITATOR)
    {
        if (p_ptr->redraw & (PR_IMITATION))
        {
            p_ptr->redraw &= ~(PR_IMITATION);
            prt_imitation();
        }
    }
    else if (p_ptr->redraw & (PR_STUDY))
    {
        p_ptr->redraw &= ~(PR_STUDY);
        prt_study();
    }
}


/*
 * Handle "p_ptr->window"
 */
void window_stuff(void)
{
    int j;

    u32b mask = 0L;


    /* Nothing to do */
    if (!p_ptr->window) return;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        /* Save usable flags */
        if (angband_term[j]) mask |= window_flag[j];
    }

    /* Apply usable flags */
    p_ptr->window &= mask;

    /* Nothing to do */
    if (!p_ptr->window) return;


    /* Display inventory */
    if (p_ptr->window & (PW_INVEN))
    {
        p_ptr->window &= ~(PW_INVEN);
        fix_inven();
    }

    /* Display equipment */
    if (p_ptr->window & (PW_EQUIP))
    {
        p_ptr->window &= ~(PW_EQUIP);
        fix_equip();
    }

    /* Display spell list */
    if (p_ptr->window & (PW_SPELL))
    {
        p_ptr->window &= ~(PW_SPELL);
        fix_spell();
    }

    /* Display player */
    if (p_ptr->window & (PW_PLAYER))
    {
        p_ptr->window &= ~(PW_PLAYER);
        fix_player();
    }

    /* Display overhead view */
    if (p_ptr->window & (PW_MESSAGE))
    {
        p_ptr->window &= ~(PW_MESSAGE);
        fix_message();
    }

    /* Display overhead view */
    if (p_ptr->window & (PW_OVERHEAD))
    {
        p_ptr->window &= ~(PW_OVERHEAD);
        fix_overhead();
    }

    /* Display overhead view */
    if (p_ptr->window & (PW_DUNGEON))
    {
        p_ptr->window &= ~(PW_DUNGEON);
        fix_dungeon();
    }

    /* Display monster recall */
    if (p_ptr->window & (PW_MONSTER))
    {
        p_ptr->window &= ~(PW_MONSTER);
        fix_monster();
    }

    /* Display object recall */
    if (p_ptr->window & (PW_OBJECT))
    {
        p_ptr->window &= ~(PW_OBJECT);
        fix_object();
    }
}


/*
 * Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window"
 */
void handle_stuff(void)
{
    if (p_ptr->update) update_stuff();
    if (p_ptr->redraw) redraw_stuff();
    if (p_ptr->window) window_stuff();
}


bool heavy_armor(void)
{
    u16b monk_arm_wgt = 0;

    if (p_ptr->pclass != CLASS_MONK
     && p_ptr->pclass != CLASS_MYSTIC
     && p_ptr->pclass != CLASS_FORCETRAINER
     && p_ptr->pclass != CLASS_NINJA
     && p_ptr->pclass != CLASS_SCOUT)
    {
        return FALSE;
    }

    monk_arm_wgt = equip_weight(object_is_armour);
    return (monk_arm_wgt > (100 + (p_ptr->lev * 4)));
}

