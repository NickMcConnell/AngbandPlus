/* File: script.c */

/* Purpose: S-Lang interface */

#include "angband.h"

#ifdef USE_SLANG

#include <slang.h>
#include "script.h"


static char *info_buf = 0;
static int msg_pending = 0;
static char pending_msg[80] = "";

/* Damage types */
static int damage_types_const[] =
{
	GF_ELEC, GF_POIS, GF_ACID, GF_COLD,
	GF_FIRE, GF_MISSILE, GF_ARROW,
	GF_PLASMA, GF_WATER, GF_LITE,
	GF_DARK, GF_LITE_WEAK, GF_DARK_WEAK,
	GF_SHARDS, GF_SOUND, GF_CONFUSION,
	GF_FORCE, GF_INERTIA, GF_MANA,
	GF_METEOR, GF_ICE, GF_CHAOS,
	GF_NETHER, GF_DISENCHANT, GF_NEXUS,
	GF_TIME, GF_GRAVITY, GF_KILL_WALL,
	GF_KILL_DOOR, GF_KILL_TRAP, GF_MAKE_WALL,
	GF_MAKE_DOOR, GF_MAKE_TRAP, GF_OLD_CLONE, 
	GF_OLD_POLY, GF_OLD_HEAL, GF_OLD_SPEED,
	GF_OLD_SLOW, GF_OLD_CONF, GF_OLD_SLEEP,
	GF_OLD_DRAIN, GF_AWAY_UNDEAD, GF_AWAY_EVIL,
	GF_AWAY_ALL, GF_TURN_UNDEAD, GF_TURN_EVIL,
	GF_TURN_ALL, GF_DISP_UNDEAD, GF_DISP_EVIL,
	GF_DISP_ALL, GF_DISP_DEMON, GF_DISP_LIVING,
	GF_ROCKET, GF_NUKE, GF_MAKE_GLYPH,
	GF_STASIS, GF_STONE_WALL, GF_DEATH_RAY,
	GF_STUN, GF_HOLY_FIRE, GF_HELL_FIRE,
	GF_DISINTEGRATE, GF_CHARM, GF_CONTROL_UNDEAD,
	GF_CONTROL_ANIMAL, GF_PSI, GF_PSI_DRAIN,
	GF_TELEKINESIS, GF_JAM_DOOR, GF_DOMINATION,
	GF_DISP_GOOD,
};

/* Setable player fields */
static int player_fields_const[] =
{
	FL_BLIND, FL_CONF, FL_POIS, FL_FEAR,
	FL_PARALYZ, FL_HALLU, FL_FAST, FL_SLOW,
	FL_SHIELD, FL_BLESS, FL_HERO, FL_SHERO,
	FL_PROTEVIL, FL_INVULN, FL_SEE_INV, FL_INFRA,
	FL_OPP_ACID, FL_OPP_ELEC, FL_OPP_FIRE, FL_OPP_COLD,
	FL_OPP_POIS, FL_STUN, FL_CUT, FL_FOOD,
};

/* Damage effect flags */
static int damage_effect_const[] =
{
	PROJECT_JUMP, PROJECT_BEAM , PROJECT_THRU, PROJECT_STOP,
	PROJECT_GRID,	PROJECT_ITEM,	PROJECT_KILL,	PROJECT_HIDE,
};

/* Detection types */
static int detection_const[] =
{
	DET_TRAPS, DET_DOORS, DET_STAIRS,
	DET_TREASURE, DET_OBJECTS_GOLD, DET_OBJECTS_NORMAL,
	DET_OBJECTS_MAGIC, DET_MONSTERS_NORMAL, DET_MONSTERS_INVIS,
	DET_MONSTERS_EVIL, DET_MONSTERS_NONLIVING, DET_ALL,
};

/* Restorable stats */
static int stat_const[] =
{
	A_STR,	A_INT,	A_WIS,	A_DEX,	A_CON,	A_CHR,
};

/* Miscellaneous effects */
static int misc_effects_const[] =
{
	MI_ID, MI_FULL_ID, MI_RES_LEV, MI_UNCURSE,
	MI_FULL_UNCURSE, MI_PROBING, MI_MK_STAIR, MI_GLYPH,
	MI_GENOCIDE, MI_MASS_GENO, MI_TELE_LEV, MI_MAP_AREA,
	MI_WIZ_LITE, MI_BRAND_WEAP, MI_BRAND_BOLT, MI_LITE_ROOM,
	MI_DESTROY, MI_QUAKE, MI_ALTER, MI_RAZORBACK,
	MI_RING_POWER,
};

/* Class codes */
static int class_const[] =
{
	CLASS_WARRIOR, CLASS_MAGE, CLASS_PRIEST,
	CLASS_ROGUE, CLASS_RANGER, CLASS_PALADIN,
	CLASS_WARRIOR_MAGE, CLASS_CHAOS_WARRIOR,
	CLASS_MONK, CLASS_MINDCRAFTER, CLASS_HIGH_MAGE,
};

/* Race codes */
static int race_const[] =
{
	RACE_HUMAN, RACE_HALF_ELF, RACE_ELF,
	RACE_HOBBIT, RACE_GNOME, RACE_DWARF,
	RACE_HALF_ORC, RACE_HALF_TROLL, RACE_AMBERITE,
	RACE_HIGH_ELF, RACE_BARBARIAN, RACE_HALF_OGRE,
	RACE_HALF_GIANT, RACE_HALF_TITAN, RACE_CYCLOPS,
	RACE_YEEK, RACE_KLACKON, RACE_KOBOLD,
	RACE_NIBELUNG, RACE_DARK_ELF, RACE_DRACONIAN,
	RACE_MIND_FLAYER, RACE_IMP, RACE_GOLEM,
	RACE_SKELETON, RACE_ZOMBIE, RACE_VAMPIRE,
	RACE_SPECTRE, RACE_SPRITE, RACE_BEASTMAN,
};

/* Sex codes */
static int sex_const[] =
{
	SEX_FEMALE, SEX_MALE,
};

/* Magic realms */
static int realm_const[] =
{
	REALM_NONE, REALM_LIFE, REALM_SORCERY,
	REALM_NATURE, REALM_CHAOS, REALM_DEATH,
	REALM_TRUMP, REALM_ARCANE,
};

/* Miscellaneous identifiers */
static int misc_const[] =
{
	PY_FOOD_MAX, MAX_SIGHT,
};

static bool (*field_setters[])(int) =
{
	set_blind, set_confused, set_poisoned,
	set_afraid, set_paralyzed, set_image,
	set_fast, set_slow, set_shield,
	set_blessed, set_hero, set_shero,
	set_protevil, set_invuln, set_tim_invis, 
	set_tim_infra, set_oppose_acid, set_oppose_elec,
	set_oppose_fire, set_oppose_cold, set_oppose_pois, 
	set_stun, set_cut, set_food,
};

extern player_type p_body;

static s16b *field_vals[] =
{
	&p_body.blind, &p_body.confused, &p_body.poisoned, 
	&p_body.afraid, &p_body.paralyzed, &p_body.image, 
	&p_body.fast, &p_body.slow, &p_body.shield, 
	&p_body.blessed, &p_body.hero, &p_body.shero, 
	&p_body.protevil, &p_body.invuln, &p_body.tim_invis, 
	&p_body.tim_infra, &p_body.oppose_acid, &p_body.oppose_elec, 
	&p_body.oppose_fire, &p_body.oppose_cold, &p_body.oppose_pois,
	&p_body.stun, &p_body.cut, &p_body.food,
};


static int do_plev(void)
{
	return p_ptr->lev;
}

static int do_pclass(void)
{
	return p_ptr->pclass;
}

static int do_prace(void)
{
	return p_ptr->prace;
}

static int do_psex(void)
{
	return p_ptr->psex;
}

static int do_realm1(void)
{
	return p_ptr->realm1;
}

static int do_realm2(void)
{
	return p_ptr->realm2;
}

static int do_gold(void)
{
	return p_ptr->au;
}

static int do_dice(int *dn, int *ds)
{
	return damroll(*dn, *ds);
}

static void do_message(char *msg)
{
	msg_print(msg);
}

static void do_message_delayed(char *msg)
{
	strncpy(pending_msg, msg, 79);
	pending_msg[79] = '\0';
	msg_pending = 1;
}

static void do_set_field(int *fld, int *val)
{
	(void)(*field_setters[*fld])(*val);
}

static void do_add_field(int *fld, int *val)
{
	(void)(*field_setters[*fld])(*field_vals[*fld] + *val);
}

static int do_get_field(int *fld)
{
	return (int)(*field_vals[*fld]);
}

static void do_heal(int *amt)
{
	hp_player(*amt);
}

static void do_restore_stat(int *n)
{
	do_res_stat(*n);
}

static void do_teleport_self(int *dis)
{
	teleport_player(*dis);
}

static void do_recharge(int *num)
{
	(void)recharge(*num);
}

static void do_enchant_weapon(int *to_hit, int *to_dam)
{
	(void)enchant_spell(*to_hit, *to_dam, 0);
}

static void do_enchant_armor(int *to_ac)
{
	(void)enchant_spell(0, 0, *to_ac);
}

static bool get_aim_dir_msg(int *dp)
{
	bool ret = get_aim_dir(dp);
	
	if (ret && msg_pending) msg_print(pending_msg);
	msg_pending = 0;
	return ret;
}

static void do_bolt(int *typ, int *dam)
{
	int dir;
	if (!get_aim_dir_msg(&dir)) return;
	fire_bolt(*typ, dir, *dam);
}

static void do_ball(int *rad, int *typ, int *dam)
{
	int dir;
	if (!get_aim_dir_msg(&dir)) return;
	fire_ball(*typ, dir, *dam, *rad);
}

static void do_beam(int *typ, int *dam, int *flg, int *prob)
{
	int dir;
	if (!get_aim_dir_msg(&dir)) return;
	if (rand_int(100) < *prob)
	{
		project_hook(*typ, dir, *dam, PROJECT_BEAM | *flg);
	}
	else
	{
		fire_bolt(*typ, dir, *dam);
	}
}

static void do_burst(int *rad, int *typ, int *dam, int *flg)
{
	(void)project(0, *rad, py, px, *dam, *typ, *flg);
}

static void do_los(int *typ, int *dam)
{
	(void)project_hack(*typ, *dam);
}

static void do_detect(int *typ)
{
	switch (*typ)
	{
    case DET_TRAPS: detect_traps(); return;
    case DET_DOORS: detect_doors(); return;
	case DET_STAIRS: detect_stairs(); return;
    case DET_TREASURE: detect_treasure(); return;
	case DET_OBJECTS_GOLD: detect_objects_gold(); return;
	case DET_OBJECTS_NORMAL: detect_objects_normal(); return;
	case DET_OBJECTS_MAGIC: detect_objects_magic(); return;
    case DET_MONSTERS_NORMAL: detect_monsters_normal(); return;
    case DET_MONSTERS_INVIS: detect_monsters_invis(); return;
    case DET_MONSTERS_EVIL: detect_monsters_evil(); return;
	case DET_MONSTERS_NONLIVING: detect_monsters_nonliving(); return;
    case DET_ALL: detect_all(); return;
	}
}

static void do_misc(int *typ)
{
	switch (*typ)
	{
    case MI_ID: ident_spell(); return;
    case MI_FULL_ID: identify_fully(); return;
    case MI_RES_LEV: restore_level(); return;
    case MI_UNCURSE: remove_curse(); return;
    case MI_FULL_UNCURSE: remove_all_curse(); return;
    case MI_PROBING: probing(); return;
    case MI_MK_STAIR: stair_creation(); return;
    case MI_GLYPH: warding_glyph(); return;
    case MI_GENOCIDE:
		genocide(FALSE);
		return;
    case MI_MASS_GENO:
		mass_genocide(FALSE);
		return;
    case MI_TELE_LEV: teleport_player_level(); return;
    case MI_MAP_AREA: map_area(); return;
    case MI_WIZ_LITE: wiz_lite(); return;
    case MI_BRAND_WEAP: brand_weapon(2); return;
    case MI_BRAND_BOLT: brand_bolts(); return;
    case MI_LITE_ROOM: lite_room(py, px); return;
    case MI_DESTROY: destroy_area(py, px, 15, TRUE); return;
    case MI_QUAKE:
		/* Prevent destruction of quest levels and town */
		if (!is_quest(dun_level) && dun_level)
			earthquake(py, px, 10);
		return;
    case MI_ALTER: alter_reality(); return;
    case MI_RAZORBACK:
		{
			int i;
			for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
			return;
		}
    case MI_RING_POWER:
		{
			int dir;
			if (get_aim_dir_msg(&dir)) ring_of_power(dir);
			return;
		}
	}
}

static void do_recall(int *n)
{
	if (!p_ptr->word_recall)
	{
		p_ptr->word_recall = *n;
		msg_print("The air about you becomes charged...");
	}
	else
	{
		p_ptr->word_recall = 0;
		msg_print("A tension leaves the air around you...");
	}
}

static int do_info(char *info)
{
	if (!info_buf) return 0;
	strcpy(info_buf, info);
	return 1;
}


static void error_handler(char *err)
{
	msg_print(err);
}


static SLang_Intrin_Fun_Type angband_intrinsics[] =
{
	MAKE_INTRINSIC("plev", do_plev, INT_TYPE, 0),
	MAKE_INTRINSIC("pclass", do_pclass, INT_TYPE, 0),
	MAKE_INTRINSIC("prace", do_prace, INT_TYPE, 0),
	MAKE_INTRINSIC("psex", do_psex, INT_TYPE, 0),
	MAKE_INTRINSIC("realm1", do_realm1, INT_TYPE, 0),
	MAKE_INTRINSIC("realm2", do_realm2, INT_TYPE, 0),
	MAKE_INTRINSIC("gold", do_gold, INT_TYPE, 0),
	MAKE_INTRINSIC("dice", do_dice, INT_TYPE, 2),
	MAKE_INTRINSIC("message", do_message, VOID_TYPE, 1),
	MAKE_INTRINSIC("message_delayed", do_message_delayed, VOID_TYPE, 1),
	MAKE_INTRINSIC("set_field", do_set_field, VOID_TYPE, 2),
	MAKE_INTRINSIC("add_field", do_add_field, VOID_TYPE, 2),
	MAKE_INTRINSIC("get_field", do_get_field, INT_TYPE, 1),
	MAKE_INTRINSIC("ball", do_ball, VOID_TYPE, 3),
	MAKE_INTRINSIC("bolt", do_bolt, VOID_TYPE, 2),
	MAKE_INTRINSIC("beam", do_beam, VOID_TYPE, 4),
	MAKE_INTRINSIC("burst", do_burst, VOID_TYPE, 4),
	MAKE_INTRINSIC("los", do_los, VOID_TYPE, 2),
	MAKE_INTRINSIC("heal", do_heal, VOID_TYPE, 1),
	MAKE_INTRINSIC("restore_stat", do_restore_stat, VOID_TYPE, 1),
	MAKE_INTRINSIC("detect", do_detect, VOID_TYPE, 1),
	MAKE_INTRINSIC("teleport_self", do_teleport_self, VOID_TYPE, 1),
	MAKE_INTRINSIC("recharge", do_recharge, VOID_TYPE, 1),
	MAKE_INTRINSIC("recall", do_recall, VOID_TYPE, 1),
	MAKE_INTRINSIC("enchant_weapon", do_enchant_weapon, VOID_TYPE, 2),
	MAKE_INTRINSIC("enchant_armor", do_enchant_armor, VOID_TYPE, 1),
	MAKE_INTRINSIC("misc", do_misc, VOID_TYPE, 1),
	MAKE_INTRINSIC("info", do_info, INT_TYPE, 1),

	SLANG_END_TABLE
};

static SLang_Intrin_Var_Type angband_vars [] =
{
	/* Damage types */
	MAKE_VARIABLE("GF_ELEC",           &damage_types_const[ 0], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_POIS",           &damage_types_const[ 1], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_ACID",           &damage_types_const[ 2], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_COLD",           &damage_types_const[ 3], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_FIRE",           &damage_types_const[ 4], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_MISSILE",        &damage_types_const[ 5], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_ARROW",          &damage_types_const[ 6], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_PLASMA",         &damage_types_const[ 7], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_WATER",          &damage_types_const[ 8], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_LITE",           &damage_types_const[ 9], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DARK",           &damage_types_const[10], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_LITE_WEAK",      &damage_types_const[11], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DARK_WEAK",      &damage_types_const[12], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_SHARDS",         &damage_types_const[13], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_SOUND",          &damage_types_const[14], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_CONFUSION",      &damage_types_const[15], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_FORCE",          &damage_types_const[16], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_INERTIA",        &damage_types_const[17], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_MANA",           &damage_types_const[18], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_METEOR",         &damage_types_const[19], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_ICE",            &damage_types_const[20], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_CHAOS",          &damage_types_const[21], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_NETHER",         &damage_types_const[22], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DISENCHANT",     &damage_types_const[23], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_NEXUS",          &damage_types_const[24], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_TIME",           &damage_types_const[25], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_GRAVITY",        &damage_types_const[26], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_KILL_WALL",      &damage_types_const[27], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_KILL_DOOR",      &damage_types_const[28], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_KILL_TRAP",      &damage_types_const[29], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_MAKE_WALL",      &damage_types_const[30], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_MAKE_DOOR",      &damage_types_const[31], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_MAKE_TRAP",      &damage_types_const[32], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_OLD_CLONE",      &damage_types_const[33], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_OLD_POLY",       &damage_types_const[34], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_OLD_HEAL",       &damage_types_const[35], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_OLD_SPEED",      &damage_types_const[36], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_OLD_SLOW",       &damage_types_const[37], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_OLD_CONF",       &damage_types_const[38], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_OLD_SLEEP",      &damage_types_const[39], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_OLD_DRAIN",      &damage_types_const[40], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_AWAY_UNDEAD",    &damage_types_const[41], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_AWAY_EVIL",      &damage_types_const[42], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_AWAY_ALL",       &damage_types_const[43], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_TURN_UNDEAD",    &damage_types_const[44], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_TURN_EVIL",      &damage_types_const[45], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_TURN_ALL",       &damage_types_const[46], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DISP_UNDEAD",    &damage_types_const[47], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DISP_EVIL",      &damage_types_const[48], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DISP_ALL",       &damage_types_const[49], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DISP_DEMON",     &damage_types_const[50], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DISP_LIVING",    &damage_types_const[51], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_ROCKET",         &damage_types_const[52], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_NUKE",           &damage_types_const[53], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_MAKE_GLYPH",     &damage_types_const[54], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_STASIS",         &damage_types_const[55], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_STONE_WALL",     &damage_types_const[56], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DEATH_RAY",      &damage_types_const[57], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_STUN",           &damage_types_const[58], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_HOLY_FIRE",      &damage_types_const[59], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_HELL_FIRE",      &damage_types_const[60], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DISINTEGRATE",   &damage_types_const[61], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_CHARM",          &damage_types_const[62], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_CONTROL_UNDEAD", &damage_types_const[63], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_CONTROL_ANIMAL", &damage_types_const[64], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_PSI",            &damage_types_const[65], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_PSI_DRAIN",      &damage_types_const[66], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_TELEKINESIS",    &damage_types_const[67], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_JAM_DOOR",       &damage_types_const[68], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DOMINATION",     &damage_types_const[69], SLANG_INT_TYPE, 1),
	MAKE_VARIABLE("GF_DISP_GOOD",      &damage_types_const[70], SLANG_INT_TYPE, 1),

	/* Settable player fields */
	MAKE_VARIABLE("FL_BLIND",    &player_fields_const[ 0], INT_TYPE, 1),
	MAKE_VARIABLE("FL_CONF",     &player_fields_const[ 1], INT_TYPE, 1),
	MAKE_VARIABLE("FL_POIS",     &player_fields_const[ 2], INT_TYPE, 1),
	MAKE_VARIABLE("FL_FEAR",     &player_fields_const[ 3], INT_TYPE, 1),
	MAKE_VARIABLE("FL_PARALYZ",  &player_fields_const[ 4], INT_TYPE, 1),
	MAKE_VARIABLE("FL_HALLU",    &player_fields_const[ 5], INT_TYPE, 1),
	MAKE_VARIABLE("FL_FAST",     &player_fields_const[ 6], INT_TYPE, 1),
	MAKE_VARIABLE("FL_SLOW",     &player_fields_const[ 7], INT_TYPE, 1),
	MAKE_VARIABLE("FL_SHIELD",   &player_fields_const[ 8], INT_TYPE, 1),
	MAKE_VARIABLE("FL_BLESS",    &player_fields_const[ 9], INT_TYPE, 1),
	MAKE_VARIABLE("FL_HERO",     &player_fields_const[10], INT_TYPE, 1),
	MAKE_VARIABLE("FL_SHERO",    &player_fields_const[11], INT_TYPE, 1),
	MAKE_VARIABLE("FL_PROTEVIL", &player_fields_const[12], INT_TYPE, 1),
	MAKE_VARIABLE("FL_INVULN",   &player_fields_const[13], INT_TYPE, 1),
	MAKE_VARIABLE("FL_SEE_INV",  &player_fields_const[14], INT_TYPE, 1),
	MAKE_VARIABLE("FL_INFRA",    &player_fields_const[15], INT_TYPE, 1),
	MAKE_VARIABLE("FL_OPP_ACID", &player_fields_const[16], INT_TYPE, 1),
	MAKE_VARIABLE("FL_OPP_ELEC", &player_fields_const[17], INT_TYPE, 1),
	MAKE_VARIABLE("FL_OPP_FIRE", &player_fields_const[18], INT_TYPE, 1),
	MAKE_VARIABLE("FL_OPP_COLD", &player_fields_const[19], INT_TYPE, 1),
	MAKE_VARIABLE("FL_OPP_POIS", &player_fields_const[20], INT_TYPE, 1),
	MAKE_VARIABLE("FL_STUN",     &player_fields_const[21], INT_TYPE, 1),
	MAKE_VARIABLE("FL_CUT",      &player_fields_const[22], INT_TYPE, 1),
	MAKE_VARIABLE("FL_FOOD",     &player_fields_const[23], INT_TYPE, 1),

	/* Damage effect flags */
	MAKE_VARIABLE("PROJECT_JUMP", &damage_effect_const[0], INT_TYPE, 1),
	MAKE_VARIABLE("PROJECT_BEAM", &damage_effect_const[1], INT_TYPE, 1),
	MAKE_VARIABLE("PROJECT_THRU", &damage_effect_const[2], INT_TYPE, 1),
	MAKE_VARIABLE("PROJECT_STOP", &damage_effect_const[3], INT_TYPE, 1),
	MAKE_VARIABLE("PROJECT_GRID", &damage_effect_const[4], INT_TYPE, 1),
	MAKE_VARIABLE("PROJECT_ITEM", &damage_effect_const[5], INT_TYPE, 1),
	MAKE_VARIABLE("PROJECT_KILL", &damage_effect_const[6], INT_TYPE, 1),
	MAKE_VARIABLE("PROJECT_HIDE", &damage_effect_const[7], INT_TYPE, 1),

	/* Detection types */
	MAKE_VARIABLE("DET_TRAPS",              &detection_const[ 0], INT_TYPE, 1),
	MAKE_VARIABLE("DET_DOORS",              &detection_const[ 1], INT_TYPE, 1),
	MAKE_VARIABLE("DET_STAIRS",             &detection_const[ 2], INT_TYPE, 1),
	MAKE_VARIABLE("DET_TREASURE",           &detection_const[ 3], INT_TYPE, 1),
	MAKE_VARIABLE("DET_OBJECTS_GOLD",       &detection_const[ 4], INT_TYPE, 1),
	MAKE_VARIABLE("DET_OBJECTS_NORMAL",     &detection_const[ 5], INT_TYPE, 1),
	MAKE_VARIABLE("DET_OBJECTS_MAGIC",      &detection_const[ 6], INT_TYPE, 1),
	MAKE_VARIABLE("DET_MONSTERS_NORMAL",    &detection_const[ 7], INT_TYPE, 1),
	MAKE_VARIABLE("DET_MONSTERS_INVIS",     &detection_const[ 8], INT_TYPE, 1),
	MAKE_VARIABLE("DET_MONSTERS_EVIL",      &detection_const[ 9], INT_TYPE, 1),
	MAKE_VARIABLE("DET_MONSTERS_NONLIVING", &detection_const[10], INT_TYPE, 1),
	MAKE_VARIABLE("DET_ALL",                &detection_const[11], INT_TYPE, 1),

	/* Restorable stats */
	MAKE_VARIABLE("A_STR", &stat_const[0], INT_TYPE, 1),
	MAKE_VARIABLE("A_INT", &stat_const[1], INT_TYPE, 1),
	MAKE_VARIABLE("A_WIS", &stat_const[2], INT_TYPE, 1),
	MAKE_VARIABLE("A_DEX", &stat_const[3], INT_TYPE, 1),
	MAKE_VARIABLE("A_CON", &stat_const[4], INT_TYPE, 1),
	MAKE_VARIABLE("A_CHR", &stat_const[5], INT_TYPE, 1),

	/* Miscellaneous effects */
	MAKE_VARIABLE("MI_ID",           &misc_effects_const[ 0], INT_TYPE, 1),
	MAKE_VARIABLE("MI_FULL_ID",      &misc_effects_const[ 1], INT_TYPE, 1),
	MAKE_VARIABLE("MI_RES_LEV",      &misc_effects_const[ 2], INT_TYPE, 1),
	MAKE_VARIABLE("MI_UNCURSE",      &misc_effects_const[ 3], INT_TYPE, 1),
	MAKE_VARIABLE("MI_FULL_UNCURSE", &misc_effects_const[ 4], INT_TYPE, 1),
	MAKE_VARIABLE("MI_PROBING",      &misc_effects_const[ 5], INT_TYPE, 1),
	MAKE_VARIABLE("MI_MK_STAIR",     &misc_effects_const[ 6], INT_TYPE, 1),
	MAKE_VARIABLE("MI_GLYPH",        &misc_effects_const[ 7], INT_TYPE, 1),
	MAKE_VARIABLE("MI_GENOCIDE",     &misc_effects_const[ 8], INT_TYPE, 1),
	MAKE_VARIABLE("MI_MASS_GENO",    &misc_effects_const[ 9], INT_TYPE, 1),
	MAKE_VARIABLE("MI_TELE_LEV",     &misc_effects_const[11], INT_TYPE, 1),
	MAKE_VARIABLE("MI_MAP_AREA",     &misc_effects_const[12], INT_TYPE, 1),
	MAKE_VARIABLE("MI_WIZ_LITE",     &misc_effects_const[13], INT_TYPE, 1),
	MAKE_VARIABLE("MI_BRAND_WEAP",   &misc_effects_const[14], INT_TYPE, 1),
	MAKE_VARIABLE("MI_BRAND_BOLT",   &misc_effects_const[15], INT_TYPE, 1),
	MAKE_VARIABLE("MI_LITE_ROOM",    &misc_effects_const[16], INT_TYPE, 1),
	MAKE_VARIABLE("MI_DESTROY",      &misc_effects_const[17], INT_TYPE, 1),
	MAKE_VARIABLE("MI_QUAKE",        &misc_effects_const[18], INT_TYPE, 1),
	MAKE_VARIABLE("MI_ALTER",        &misc_effects_const[19], INT_TYPE, 1),
	MAKE_VARIABLE("MI_RAZORBACK",    &misc_effects_const[20], INT_TYPE, 1),
	MAKE_VARIABLE("MI_RING_POWER",   &misc_effects_const[21], INT_TYPE, 1),

	/* Class codes */
	MAKE_VARIABLE("CLASS_WARRIOR",       &class_const[ 0], INT_TYPE, 1),
	MAKE_VARIABLE("CLASS_MAGE",          &class_const[ 1], INT_TYPE, 1),
	MAKE_VARIABLE("CLASS_PRIEST",        &class_const[ 2], INT_TYPE, 1),
	MAKE_VARIABLE("CLASS_ROGUE",         &class_const[ 3], INT_TYPE, 1),
	MAKE_VARIABLE("CLASS_RANGER",        &class_const[ 4], INT_TYPE, 1),
	MAKE_VARIABLE("CLASS_PALADIN",       &class_const[ 5], INT_TYPE, 1),
	MAKE_VARIABLE("CLASS_WARRIOR_MAGE",  &class_const[ 6], INT_TYPE, 1),
	MAKE_VARIABLE("CLASS_CHAOS_WARRIOR", &class_const[ 7], INT_TYPE, 1),
	MAKE_VARIABLE("CLASS_MONK",          &class_const[ 8], INT_TYPE, 1),
	MAKE_VARIABLE("CLASS_MINDCRAFTER",   &class_const[ 9], INT_TYPE, 1),
	MAKE_VARIABLE("CLASS_HIGH_MAGE",     &class_const[10], INT_TYPE, 1),

	MAKE_VARIABLE("RACE_HUMAN",       &race_const[ 0], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_HALF_ELF",    &race_const[ 1], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_ELF",         &race_const[ 2], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_HOBBIT",      &race_const[ 3], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_GNOME",       &race_const[ 4], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_DWARF",       &race_const[ 5], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_HALF_ORC",    &race_const[ 6], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_HALF_TROLL",  &race_const[ 7], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_AMBERITE",    &race_const[ 8], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_HIGH_ELF",    &race_const[ 9], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_BARBARIAN",   &race_const[10], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_HALF_OGRE",   &race_const[11], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_HALF_GIANT",  &race_const[12], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_HALF_TITAN",  &race_const[13], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_CYCLOPS",     &race_const[14], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_YEEK",        &race_const[15], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_KLACKON",     &race_const[16], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_KOBOLD",      &race_const[17], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_NIBELUNG",    &race_const[18], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_DARK_ELF",    &race_const[19], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_DRACONIAN",   &race_const[20], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_MIND_FLAYER", &race_const[21], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_IMP",         &race_const[22], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_GOLEM",       &race_const[23], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_SKELETON",    &race_const[24], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_ZOMBIE",      &race_const[25], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_VAMPIRE",     &race_const[26], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_SPECTRE",     &race_const[27], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_SPRITE",      &race_const[28], INT_TYPE, 1),
	MAKE_VARIABLE("RACE_BEASTMAN",    &race_const[29], INT_TYPE, 1),

	/* Sex codes */
	MAKE_VARIABLE("SEX_FEMALE", &sex_const[0], INT_TYPE, 1),
	MAKE_VARIABLE("SEX_MALE",   &sex_const[1], INT_TYPE, 1),

	/* Magic realms */
	MAKE_VARIABLE("REALM_NONE",    &realm_const[0], INT_TYPE, 1),
	MAKE_VARIABLE("REALM_LIFE",    &realm_const[1], INT_TYPE, 1),
	MAKE_VARIABLE("REALM_SORCERY", &realm_const[2], INT_TYPE, 1),
	MAKE_VARIABLE("REALM_NATURE",  &realm_const[3], INT_TYPE, 1),
	MAKE_VARIABLE("REALM_CHAOS",   &realm_const[4], INT_TYPE, 1),
	MAKE_VARIABLE("REALM_DEATH",   &realm_const[5], INT_TYPE, 1),
	MAKE_VARIABLE("REALM_TRUMP",   &realm_const[6], INT_TYPE, 1),
	MAKE_VARIABLE("REALM_ARCANE",  &realm_const[7], INT_TYPE, 1),

	/* Assorted identifiers */
	MAKE_VARIABLE("PY_FOOD_MAX", &misc_const[0], INT_TYPE, 1),
	MAKE_VARIABLE("MAX_SIGHT",   &misc_const[1], INT_TYPE, 1),
	
	SLANG_END_TABLE
};

void want_info(char *buf)
{
	info_buf = buf; if (info_buf) *info_buf = '\0';
}

errr execute_function(char *name, int *retval)
{
	int dummy;
	
	/* Paranoia */
	if (!retval) retval = &dummy;
	if (!name || !*name)
	{
		msg_print("Oops!  No spell name given.");
		return 1;
	}
	
	/* "In-place" script */
	if (name[0] == '@' || name[0] == '%')
	{
		/* In-place scripts of type '@' don't generate info */
		if (info_buf && name[0] == '@') return 0;
		(void)SLang_load_string(name + 1);
		*retval = 0;
		if (SLang_Error == 0) return 0;
		SLang_restart(1);
		SLang_Error = 0;
		return 1;
	}
	else
	{
		char buf[1024];
		
		path_build(buf, 1024, ANGBAND_DIR_EDIT, name);
		
		if (SLang_execute_function(buf) == 0)
		{
			msg_format("Oops!  Script '%s' does not exist or is buggy.", buf);
			SLang_restart(1);
			SLang_Error = 0;
			return 1;
		}
		if (SLang_pop_integer(retval) != 0)
		{
			msg_format("Oops!  Script '%s' returned no value...", buf);
			SLang_restart(1);
			SLang_Error = 0;
			return 1;
		}
	}
	return 0;
}


errr init_script(void)
{
	char buf[1024];
	
	/* Initialize S-Lang */
	if ((-1 == SLang_init_slang ()) ||
		(-1 == SLadd_intrin_fun_table (angband_intrinsics, NULL)) ||
		(-1 == SLadd_intrin_var_table (angband_vars, NULL)))
		quit("Unable to initialize S-Lang");

	SLang_Error_Hook = error_handler;

	/* Build the pathname */
	path_build(buf, 512, ANGBAND_DIR_EDIT, "s_info.sl");
	
	/* Attempt to load the "sl" file */
	SLang_load_file(buf);
	
	/* Error */
	if (SLang_Error != 0) quit("Cannot parse 's_info.sl' file.");
	
	/* Success */
	return 0;
}

#endif /* USE_SLANG */
