/* snatch.c */

#include "angband.h"


/*
 * TABLE: which equipments can monsters weild/wear ?
 */
struct monster_wield_type
{
	char sym;
	bool flag[INVEN_TOTAL - INVEN_WIELD];
};

static struct monster_wield_type is_monster_wield[] =
{
	/* 字   武 盾 弓 左 右 首 光 鎧 上 兜 手 靴 */
	{ 'b', { 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 } },
	{ 'd', { 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 } },
	{ 'f', { 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 } },
	{ 'g', { 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0 } },
	{ 'h', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },
	{ 'k', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },
	{ 'n', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0 } },
	{ 'o', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },
	{ 'p', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },
	{ 'q', { 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0 } },
	{ 'r', { 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 } },
	{ 's', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },
	{ 't', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },
	{ 'u', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },
	{ 'y', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },
	{ 'z', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },

	{ 'A', { 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1 } },
	{ 'B', { 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 } },
	{ 'C', { 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 } },
	{ 'D', { 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 } },
	{ 'H', { 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 } },
	{ 'L', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },
	{ 'M', { 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 } },
	{ 'O', { 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0 } },
	{ 'P', { 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0 } },
	{ 'R', { 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 } },
	{ 'T', { 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0 } },
	{ 'U', { 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1 } },
	{ 'V', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },
	{ 'W', { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } },
	{ 'X', { 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0 } },
	{ 'Y', { 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0 } },
	{ 'Z', { 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 } },

	{   0, { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } },
};


/*
 * TABLE: Status bonuses that player gains.
 */
struct monster_bonus_type
{
	char sym;
	s16b r_adj[A_MAX];	/* Racial stat bonuses */
	s16b dis;			/* disarming */
	s16b dev;			/* magic devices */
	s16b sav;			/* saving throw */
	s16b stl;			/* stealth */
	s16b srh;			/* search ability */
	s16b fos;			/* search frequency */
	s16b thn;			/* combat (normal) */
	s16b thb;			/* combat (shooting) */
	byte infra;			/* Infra-vision	range */
	s16b mhp;			/* Race hit-dice modifier */
};

struct monster_bonus_type monster_bonus[] =
{
	/* 字    腕  知  賢  器  耐  魅    解  道  防  隠  探  知  攻  射  赤  HD */
	{ 'b', { -4, -2, -2,  2, -2, -1 },-20,  0,  0,  8,  0, 30,-10,-20,  9,  0 },
	{ 'd', {  4, -2, -3,  4,  4, -1 }, -8,  0,  5, -1,  0,  0, 15,-10,  5,  8 },
	{ 'e', { -2,  2, -4, -2, -2, -4 },-20,  0, 15,  5, 20, 10,-20,-20,  7,  2 },
	{ 'f', {  1, -2, -2,  3,  2,  2 }, -5,  0,  0,  4, 20, 10, 15,-20,  7,  2 },
	{ 'g', {  4, -5, -5, -2,  4, -2 }, -5, -5,  6, -3, -1,  0, 20,  0,  0,  5 },
	{ 'h', { -1,  1,  1,  1, -1,  2 },  5,  6,  4,  1,  8,  2, -5, 10,  2,  0 },
	{ 'k', { -1, -1, -1,  2,  1, -1 }, -2, -3, -1, -1,  1, -2, 10, -8,  1,  0 },
	{ 'n', {  2,  0,  1,  2,  3, -1 },  0,  3,  5,  0,  0,  2, 10,  0,  2,  2 },
	{ 'o', {  2, -2, -1,  1,  2, -2 }, -1, -2, -1, -1,  0, -3, 12, -5,  3,  0 },
	{ 'p', {  0,  0,  0,  0,  0,  0 },  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
	{ 'q', {  4, -3, -3,  4,  4, -1 },-10,  0,  0,  0,  0,  0, 15,  0,  2,  3 },
	{ 'r', { -2, -1, -1,  2, -2, -2 },  2, -3, -3,  2,  5,  5,  0,  0,  5,  0 },
	{ 's', {  0,  0,  0,  0,  0, -5 },  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
	{ 't', { -1, -1, -1, -1, -1, -1 }, -1, -1, -1, -1, -1, -1, -5, -5,  0, -1 },
	{ 'u', {  0,  2, -1,  2,  1, -2 }, -3,  2, -1,  0, -1,  0,  5,  0,  3,  3 },
	{ 'y', { -4,  1,  1,  2, -2, -4 },  2,  4,  6,  2,  5,  5, -5, -5,  3, -2 },
	{ 'z', {  0,  0,  0,  0,  0, -5 },  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },

	{ 'A', {  2,  2,  4,  2,  2,  4 },  0, 12,  8,  2,  2,  1, 10, 10,  5,  0 },
	{ 'B', {  1, -1, -1,  1,  2,  0 },-20,  3,  5,  3,  0, 15, 10,-20,  7,  2 },
	{ 'C', {  2, -2, -2,  4,  4,  0 }, -8,  0,  0,  3, 10, 15, 20,-20,  7,  2 },
	{ 'D', {  8, -1, -2,  8,  8, -2 },-10,  5, 10, -2,  0,  5, 30,-10,  5, 10 },
	{ 'H', {  4, -2, -1, -2,  4, -3 }, -8, -5,  5,  1,  0,  5, 20,-10,  3,  3 },
	{ 'J', {  0, -2, -2,  0,  0, -3 }, -8,  0,  5,  3,  0,  5,  5,  0,  5,  0 },
	{ 'L', { -3,  4, -2,  1, -2, -2 },  0, 10, 10,  2,  2,  0, -8,  0,  0, -1 },
	{ 'M', {  6, -4, -3, -2,  6, -5 }, -8, -5,  0, -2, 10, 10, 25,-10,  3,  8 },
	{ 'O', {  3, -2, -1, -1,  2, -2 }, -3, -3, -3, -2, -1, -5, 20,  0,  3,  3 },
	{ 'P', {  4, -2, -2, -2,  3, -3 }, -5, -5, -5, -3, -1, -5, 25,  5,  3,  5 },
	{ 'R', {  2, -3, -3, -2,  2, -3 }, -8, -8, -8,  0, -1, -5, 25,-10,  3,  0 },
	{ 'T', {  5, -4, -2, -3,  4, -5 }, -5, -8, -8, -5, -1, -5, 20,-10,  3,  5 },
	{ 'U', {  2,  2, -5,  2,  4, -5 }, -3, 12, 15, -2,  1, -2, 20,  0,  3,  5 },
	{ 'V', {  3,  3, -2, -1,  1,  2 },  4,  8,  6,  4,  1, -2,  5,  0,  5,  0 },
	{ 'W', { -1,  3,  0,  1,  2,  0 },  0,  0,  0,  0,  0,  0,  0,  0,  5,  0 },
	{ 'X', {  4, -2, -2, -2,  4, -2 }, -8, -3,  8,  0,  0,  0, 15,-10,  3,  3 },
	{ 'Y', {  3, -1, -1,  0,  2, -1 }, -3, -1, -2, -1,  0,  0, 10, -5,  3,  2 },
	{ 'Z', {  2, -2, -2,  2,  4,  0 }, -8,  0, 10,  3, 10, 15, 20,-20,  7,  5 },

	{   0, {  0,  0,  0,  0,  0,  0 },  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 },
};


/*
 * Unsnatchable races
 */
#define MAX_UNSNATCH 6

char unsnatchable_race[MAX_UNSNATCH] =
{
	'j',	/* Sea moons, etc... */
	'l',	/* Aquantics */
	's',	/* Skeletons have no mass */
	'z',	/* Zombies are rotten */
	'G',	/* Ghosts have no body */
	'W',	/* Wraiths have no body */ 
};

/* 
 * Set first race
 */
void set_first_monster_race(void)
{
	/* Novice Warrior */
	p_ptr->r_idx = 43;

	return;
}


/*
 * Checking which a monster can wield/wear it.
 */
bool check_monster_wield(int slot)
{
	int i;
	int item = (slot - INVEN_WIELD);
	monster_race *r_ptr = &r_info[p_ptr->r_idx];
	char sym = r_ptr->d_char;

	if (slot == INVEN_LITE) return (TRUE);

	for (i = 0; is_monster_wield[i].sym != 0; i++)
	{
		if (sym == is_monster_wield[i].sym)
		{
			return is_monster_wield[i].flag[item];
		}
	}

	return (FALSE);
}


s16b monster_blows(int r_idx)
{
	int i, cnt = 0;
	monster_race *r_ptr = &r_info[r_idx];

	/* Paranoia */
	if (inventory[INVEN_WIELD].k_idx) return (0);

	for (i = 0; i < 4; i++)
	{
		if (r_ptr->blow[i].method) cnt++;
	}

	return (cnt);
}


int monster_avgdam(int r_idx)
{
	int i, tdam = 0;
	int cnt = monster_blows(r_idx);
	monster_race *r_ptr = &r_info[r_idx];

	/* Paranoia */
	if (inventory[INVEN_WIELD].k_idx) return (0);

	for (i = 0; i < cnt; i++)
	{
		tdam += (r_ptr->blow[i].d_dice * (r_ptr->blow[i].d_side + 1) / 2);
	}

	return (tdam);
}


int add_monster_melee_skill(void)
{
	monster_race *r_ptr = &r_info[p_ptr->r_idx];

	if (inventory[INVEN_WIELD].k_idx) return 0;

	return (p_ptr->lev);
}


/*
 * Checking flags for a character dump.
 */
void monster_flags(u32b *g1, u32b *g2, u32b *g3)
{
	monster_race *r_ptr = &r_info[p_ptr->r_idx];

	u32b f1 = r_ptr->flags1;
	u32b f2 = r_ptr->flags2;
	u32b f3 = r_ptr->flags3;
	u32b f4 = r_ptr->flags4;
	u32b f5 = r_ptr->flags5;
	u32b f6 = r_ptr->flags6;
	u32b f7 = r_ptr->flags7;

	/* Resistances */
	if (f3 & RF3_IM_ACID) (*g2) |= TR2_RES_ACID;
	if (f3 & RF3_IM_ELEC) (*g2) |= TR2_RES_ELEC;
	if (f3 & RF3_IM_FIRE) (*g2) |= TR2_RES_FIRE;
	if (f3 & RF3_IM_COLD) (*g2) |= TR2_RES_COLD;
	if (f3 & RF3_IM_POIS) (*g2) |= TR2_RES_POIS;
	if (f3 & RF3_NO_FEAR) (*g2) |= TR2_RES_FEAR;

	if (f4 & RF4_BR_LITE) (*g2) |= TR2_RES_LITE;
	if (f4 & RF4_BR_DARK) (*g2) |= TR2_RES_DARK;
	if (f4 & RF4_BR_SHAR) (*g2) |= TR2_RES_SHARDS;
	if (f3 & RF3_NO_CONF) (*g2) |= TR2_RES_CONF;
	if (f4 & RF4_BR_SOUN) (*g2) |= TR2_RES_SOUND;
	if (f3 & RF3_RES_NETH) (*g2) |= TR2_RES_NETHER;
	if (f3 & RF3_RES_NEXU) (*g2) |= TR2_RES_NEXUS;
	if (f4 & RF4_BR_CHAO) (*g2) |= TR2_RES_CHAOS;
	if (f3 & RF3_RES_DISE) (*g2) |= TR2_RES_DISEN;
#if 0
	if (f3 & RF3_RES_WATE) (*g2) |= TR2_RES_SOUND;
#endif
	if (f3 & RF3_RES_PLAS) { (*g2) |= TR2_RES_FIRE; (*g2) |= TR2_RES_SOUND; }
	if (f3 & RF3_NO_STUN) (*g2) |= TR2_RES_SOUND;
	if (f3 & RF3_NO_SLEEP) (*g2) |= TR2_FREE_ACT;
	if (f4 & RF4_BR_TIME) (*g2) |= TR2_HOLD_LIFE;
#if 0
	if (f4 & RF4_BR_INER)
	if (f4 & RF4_BR_GRAV)
	if (f4 & RF4_BR_WALL)
	if (f4 & RF4_BR_DISI)
	if (f3 & RF3_RES_TELE) 
#endif

	/* Abilities */
	if (f2 & RF2_REFLECTING) (*g2) |= TR2_REFLECT;
	if (f2 & RF2_REGENERATE) (*g3) |= TR3_REGEN;
	if (f2 & RF2_AURA_FIRE) (*g3) |= TR3_SH_FIRE;
	if (f2 & RF2_AURA_ELEC) (*g3) |= TR3_SH_ELEC;
	if (f3 & RF3_AURA_COLD) (*g3) |= TR3_SH_COLD;
	if (f7 & RF7_CAN_FLY) (*g3) |= TR3_FEATHER;
	if (f7 & RF7_HAS_LITE_1) (*g3) |= TR3_LITE;
	if (f7 & RF7_SELF_LITE_1) (*g3) |= TR3_LITE;
	if (f7 & RF7_HAS_LITE_2) (*g3) |= TR3_LITE;
	if (f7 & RF7_SELF_LITE_2) (*g3) |= TR3_LITE;

	/* Race bonuses */
	if (f3 & RF3_ORC) (*g2) |= TR2_RES_DARK;
	if (f3 & RF3_TROLL) (*g2) |= TR2_SUST_STR;
	if (f3 & RF3_GIANT) (*g2) |= TR2_SUST_STR;
	if (f3 & RF3_UNDEAD) { (*g2) |= TR2_RES_NETHER; (*g2) |= TR2_HOLD_LIFE; }
	if (f3 & RF3_NONLIVING) (*g2) |= TR2_HOLD_LIFE;
	if (f3 & RF3_DEMON) (*g3) |= TR3_SEE_INVIS;
#if 0
	if (f3 & RF3_DEMON) (*g2) |= TR2_RES_CHAOS;
#endif

	return;
}


int add_monster_stat(int n)
{
	int i = 0;
	int p = 0;
	monster_race *r_ptr = &r_info[p_ptr->r_idx];
	u32b f2 = r_ptr->flags2;

	/* Status */
	switch (n)
	{
	case A_STR:
		if (f2 & RF2_BASH_DOOR) p += 1;
		if (f2 & RF2_KILL_BODY) p += 2;
		break;
	case A_INT:
		if (f2 & RF2_STUPID) p -= 4;
		if (f2 & RF2_SMART) p += 4;
		if (f2 & RF2_POWERFUL) p += 2;
		break;
	case A_WIS:
		if (f2 & RF2_STUPID) p -= 4;
		if (f2 & RF2_SMART) p += 4;
		if (f2 & RF2_POWERFUL) p += 2;
		break;
	case A_DEX:
		if (f2 & RF2_OPEN_DOOR) p += 1;
		if (f2 & RF2_MOVE_BODY) p += 2;
		break;
	case A_CON:
		if (!r_ptr->flags4 && !r_ptr->flags5 && !r_ptr->flags6) p += 1;
		break;
	case A_CHR:
		if (f2 & RF2_ELDRITCH_HORROR) p -= 10;
		break;
	default:
		break;
	}

	while(1)
	{
		struct monster_bonus_type *mb_ptr = &monster_bonus[i];

		if (!mb_ptr->sym) return (p);
		if (mb_ptr->sym == r_ptr->d_char) return (p + mb_ptr->r_adj[n]);
		i++;
	}
}


/*
 * Culcurates all bonuses that are gaind by a monster race.
 */
void monster_bonuses(void)
{
	int i, j;

	monster_race *r_ptr = &r_info[p_ptr->r_idx];

	u32b f1 = r_ptr->flags1;
	u32b f2 = r_ptr->flags2;
	u32b f3 = r_ptr->flags3;
	u32b f4 = r_ptr->flags4;
	u32b f5 = r_ptr->flags5;
	u32b f6 = r_ptr->flags6;
	u32b f7 = r_ptr->flags7;

	/* Resistances */
	if (f3 & RF3_IM_ACID) p_ptr->resist_acid = TRUE;
	if (f3 & RF3_IM_ELEC) p_ptr->resist_elec = TRUE;
	if (f3 & RF3_IM_FIRE) p_ptr->resist_fire = TRUE;
	if (f3 & RF3_IM_COLD) p_ptr->resist_cold = TRUE;
	if (f3 & RF3_IM_POIS) p_ptr->resist_pois = TRUE;
	if (f3 & RF3_NO_FEAR) p_ptr->resist_fear = TRUE;

	if (f4 & RF4_BR_LITE) p_ptr->resist_lite = TRUE;
	if (f4 & RF4_BR_DARK) p_ptr->resist_dark = TRUE;
	if (f4 & RF4_BR_SHAR) p_ptr->resist_shard = TRUE;
	if (f3 & RF3_NO_CONF) p_ptr->resist_conf = TRUE;
	if (f4 & RF4_BR_SOUN) p_ptr->resist_sound = TRUE;
	if (f3 & RF3_RES_NETH) p_ptr->resist_neth = TRUE;
	if (f3 & RF3_RES_NEXU) p_ptr->resist_nexus = TRUE;
	if (f4 & RF4_BR_CHAO) p_ptr->resist_chaos = TRUE;
	if (f3 & RF3_RES_DISE) p_ptr->resist_disen = TRUE;
#if 0
	if (f3 & RF3_RES_WATE) p_ptr->resist_sound = TRUE; 
#endif
	if (f3 & RF3_RES_PLAS) { p_ptr->resist_fire = TRUE; p_ptr->resist_sound = TRUE; }
	if (f3 & RF3_NO_STUN) p_ptr->resist_sound = TRUE;
	if (f3 & RF3_NO_SLEEP) p_ptr->free_act = TRUE;
	if (f4 & RF4_BR_TIME) p_ptr->hold_life = TRUE;
#if 0
	if (f4 & RF4_BR_INER)
	if (f4 & RF4_BR_GRAV)
	if (f4 & RF4_BR_WALL)
	if (f4 & RF4_BR_DISI)
	if (f3 & RF3_RES_TELE) p_ptr->anti_tele = TRUE;
#endif

	/* Abilities */
	if (f2 & RF2_PASS_WALL) p_ptr->pass_wall = TRUE;
	if (f2 & RF2_KILL_WALL) p_ptr->kill_wall = TRUE;
	if (f2 & RF2_REFLECTING) p_ptr->reflect= TRUE;
	if (f2 & RF2_REGENERATE) p_ptr->regenerate = TRUE;
	if (f2 & RF2_AURA_FIRE) p_ptr->sh_fire = TRUE;
	if (f2 & RF2_AURA_ELEC) p_ptr->sh_elec = TRUE;
	if (f3 & RF3_AURA_COLD) p_ptr->sh_cold = TRUE;
	if (f7 & RF7_CAN_FLY) p_ptr->ffall = TRUE;
	if (f7 & RF7_HAS_LITE_1) p_ptr->lite = TRUE;
	if (f7 & RF7_SELF_LITE_1) p_ptr->lite = TRUE;
	if (f7 & RF7_HAS_LITE_2) p_ptr->lite = TRUE;
	if (f7 & RF7_SELF_LITE_2) p_ptr->lite = TRUE;

	/* Race bonus */
	if (f3 & RF3_ORC) p_ptr->resist_dark = TRUE;
	if (f3 & RF3_TROLL) p_ptr->sustain_str = TRUE;
	if (f3 & RF3_GIANT) p_ptr->sustain_str = TRUE;
	if (f3 & RF3_UNDEAD) { p_ptr->resist_neth = TRUE; p_ptr->hold_life = TRUE; }
	if (f3 & RF3_NONLIVING) p_ptr->hold_life = TRUE;
	if (f3 & RF3_DEMON) p_ptr->see_inv = TRUE;
#if 0
	if (f3 & RF3_DEMON) p_ptr->resist_chaos = TRUE;
#endif

	/* Status */
	if (f2 & RF2_STUPID) { p_ptr->stat_add[A_INT] -= 4; p_ptr->stat_add[A_WIS] -= 4; }
	if (f2 & RF2_SMART) { p_ptr->stat_add[A_INT] += 4; p_ptr->stat_add[A_WIS] += 4; }
	if (f2 & RF2_POWERFUL) { p_ptr->stat_add[A_INT] += 2; p_ptr->stat_add[A_WIS] += 2; }
	if (f2 & RF2_ELDRITCH_HORROR) p_ptr->stat_add[A_CHR] -= 10;
	if (f2 & RF2_OPEN_DOOR) p_ptr->stat_add[A_DEX] += 1;
	if (f2 & RF2_BASH_DOOR) p_ptr->stat_add[A_STR] += 1;
	if (f2 & RF2_MOVE_BODY) p_ptr->stat_add[A_DEX] += 2;
	if (f2 & RF2_KILL_BODY) p_ptr->stat_add[A_STR] += 2;

	/* Unless spell caster, gain 1 CON bonus */
	if (!r_ptr->flags4 && !r_ptr->flags5 && !r_ptr->flags6) p_ptr->stat_add[A_CON] += 1;

	/* Gains a half of speeds */
	if (r_ptr->speed) p_ptr->pspeed += (r_ptr->speed - 110) / 2;

	/* Gains a half of AC */
	if (r_ptr->ac) {
		int ct = 0;

		for (i = INVEN_BODY; i < INVEN_TOTAL; i++)
		{
			if (!check_monster_wield(i)) ct++;
		}

		p_ptr->ac += (r_ptr->ac * ct / 5);
		p_ptr->dis_ac += p_ptr->ac;
	}

	/* Race bonus */
	i = 0;

	while(1)
	{
		struct monster_bonus_type *mb_ptr = &monster_bonus[i];

		if (!mb_ptr->sym) break;

		if (mb_ptr->sym == r_ptr->d_char)
		{
			for (j = 0; j < A_MAX; j++)
			{
				p_ptr->stat_add[j] += mb_ptr->r_adj[j];
			}

			p_ptr->see_infra += mb_ptr->infra;

			p_ptr->skill_dis += mb_ptr->dis;
			p_ptr->skill_dev += mb_ptr->dev;
			p_ptr->skill_sav += mb_ptr->sav;
			p_ptr->skill_stl += mb_ptr->stl;
			p_ptr->skill_srh += mb_ptr->srh;
			p_ptr->skill_fos += mb_ptr->fos;
			p_ptr->skill_thn += mb_ptr->thn;
			p_ptr->skill_thb += mb_ptr->thb;
			p_ptr->skill_tht += mb_ptr->thb;
		}

		i++;
	}

	return;
}


/*
 * Drops all items when successful body snatching.
 */
static void body_snatch_drop(int x, int y)
{
	int i;
	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	for (i = INVEN_TOTAL - 1; i >= 0; i--)
	{
		/* Access original object */
		o_ptr = &inventory[i];

		/* Skip nothing */
		if (!o_ptr->k_idx) continue;

		/* Get local object */
		q_ptr = &forge;

		/* Obtain local object */
		object_copy(q_ptr, o_ptr);

		/* Modify quantity (Paranoia) */
		if (q_ptr->number != o_ptr->number) q_ptr->number = o_ptr->number;

		/* Drop it near the player */
		(void)drop_near(q_ptr, 0, y, x);

		/* Modify, Describe, Optimize */
		inven_item_increase(i, (0 - q_ptr->number));
		inven_item_optimize(i);
	}

	p_ptr->redraw |= (PR_EQUIPPY);
}


/*
 * Calculate hit dice for snatched monsters
 */
int monster_hitdie(int r_idx)
{
	int i;
	int x = 99;
	int y = 67;
	int z = 20;

	monster_race *r_ptr = &r_info[r_idx];
	u32b f1 = r_ptr->flags1;
	u32b f3 = r_ptr->flags3;
	u32b f4 = r_ptr->flags4;
	u32b f5 = r_ptr->flags5;
	u32b f6 = r_ptr->flags6;

	/* Default hit dice is 9 */
	int hd = 9;

	for (i = 0; i < 100; i++)
	{
		z = (z * x) / y;

		if (z > (r_ptr->hdice * (r_ptr->hside + 1) / 2))
		{
			hd += i;
			break;
		}
	}

	/*
	hd = 9 + (r_ptr->hdice * (r_ptr->hside + 1) / 2) / (R_ptr->level + 2);
	return (hd);
	*/

	/* Non-unique monsters gain more bonus to hit dice */
	if (f1 & (RF1_UNIQUE))
		hd += r_ptr->hdice * (r_ptr->hside + 1) / 1332;
	else
	{
		hd += r_ptr->hdice * (r_ptr->hside + 1) / 666;

		/* Unless spell caster, gain 1 hit dice bonus */
		if (!f4 & !f5 & !f6) hd += 1;
	}


	/* Race bonuses */
	i = 0;

	while(1)
	{
		struct monster_bonus_type *mb_ptr = &monster_bonus[i];

		if (!mb_ptr->sym) break;
		if (mb_ptr->sym == r_ptr->d_char)
		{
			hd += mb_ptr->mhp;
			break;
		}
		i++;
	}

	return (hd);
}


void culc_monster_hitpoints()
{
	p_ptr->hitdie = cp_ptr->c_mhp + monster_hitdie(p_ptr->r_idx);
	do_cmd_rerate(TRUE);
}


void lore_do_snatch(monster_race *r_ptr)
{
	int m;

	/* Hack -- Maximal info */
	r_ptr->r_wake = r_ptr->r_ignore = MAX_UCHAR;

	/* Observe "maximal" attacks */
	for (m = 0; m < 4; m++)
	{
		/* Examine "actual" blows */
		if (r_ptr->blow[m].effect || r_ptr->blow[m].method)
		{
			/* Hack -- maximal observations */
			r_ptr->r_blows[m] = MAX_UCHAR;
		}
	}

	/* Hack -- observe many spells */
	r_ptr->r_cast_inate = MAX_UCHAR;
	r_ptr->r_cast_spell = MAX_UCHAR;

	/* Hack -- know all the flags */
	r_ptr->r_flags1 = r_ptr->flags1;
	r_ptr->r_flags2 = r_ptr->flags2;
	r_ptr->r_flags3 = r_ptr->flags3;
	r_ptr->r_flags4 = r_ptr->flags4;
	r_ptr->r_flags5 = r_ptr->flags5;
	r_ptr->r_flags6 = r_ptr->flags6;
}


/*
 * Effects of body snatching.
 */
bool body_snatch(u16b m_idx)
{
	int i;
	int ox = px, oy = py;
	bool fail = FALSE;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f1 = r_ptr->flags1;
	u32b f2 = r_ptr->flags2;
	u32b f3 = r_ptr->flags3;

	int power = p_ptr->lev * 2;
	int mon_power;

	/* Some monsters resist snatching */
	if ((f1 & RF1_UNIQUE) && (f1 & RF1_QUESTOR)) fail = TRUE;
	if (f2 & RF2_EMPTY_MIND) fail = TRUE;
	if (f2 & RF2_WEIRD_MIND) fail = TRUE;
	if (f2 & RF2_QUANTUM) fail = TRUE;
	if (!(f3 & RF3_DEMON) && (f3 & RF3_NONLIVING)) fail = TRUE;

	for (i = 0; i < MAX_UNSNATCH; i++)
	{
		if (r_ptr->d_char == unsnatchable_race[i])
		{
			fail = TRUE;
			break;
		}
	}

	if (fail)
	{
		char m_name[80];
		monster_desc(m_name, m_ptr, 0);

		if (is_seen(m_ptr))
#ifdef JP
			msg_format("%^sには効果がなかった。", m_name);
#else
			msg_format("%^s is unaffected. ", m_name);
#endif
		else
#ifdef JP
			msg_print("何者かには効果がなかった。");
#else
			msg_print("It is unaffected. ");
#endif

		return (FALSE);
	}

	if (m_ptr->hp > m_ptr->maxhp / 5)
	{
		if (is_seen(m_ptr))
#ifdef JP
			msg_format("もっと弱らせないと。");
#else
		{
			char m_name[80];
			monster_desc(m_name, m_ptr, 0);
			msg_format("You need to weaken %s more.", m_name);
		}
#endif
		else
#ifdef JP
			msg_print("何者かには効果がなかった。");
#else
			msg_print("It is unaffected. ");
#endif

		return (FALSE);
	}

	/* Prompt when target monster do never move */
	if (f1 & (RF1_NEVER_MOVE))
	{
#ifdef JP
		if (!get_check("このモンスターは歩けそうにない。本当に憑依しますか？"))
#else
		if (!get_check("This monster is unable to walk, maybe. Do you snatch?"))
#endif
		{
#ifdef JP
			msg_print("身体の交換をあきらめた。");
#else
			msg_print("Gives up to snatch each body.");
#endif
			return (FALSE);
		}
	}

	/* Get snatch bonus when target monster fears or confuses */
	if (m_ptr->monfear) power += p_ptr->lev;
	if (m_ptr->confused) power += p_ptr->lev;

	/* Calculate resist power of target monster */
	mon_power = ((r_ptr->level + 1) * 5 * m_ptr->hp) / (2 * m_ptr->maxhp);

	/* Uniques and Questers can resist stronger than non-uniques */
	if ((f1 & RF1_UNIQUE) || (f1 & RF1_QUESTOR))mon_power *= 4;

	if (wizard)
#ifdef JP
		msg_format("チェンジパワー %d:%d", power, mon_power);
#else
		msg_format("Power for changing %d:%d", power, mon_power);
#endif

	/* Check for snatch */
	if (randint1(power) < mon_power)
	{
		char m_name[80];
		monster_desc(m_name, m_ptr, 0);

		if (is_seen(m_ptr))
#ifdef JP
			msg_format("%sの精神があなたの支配を跳ね返した。", m_name);
#else
			msg_format("%^s resists your control.", m_name);
#endif
		else
#ifdef JP
			msg_print("何者かには効果がなかった。");
#else
			msg_print("It is unaffected. ");
#endif

		return (FALSE);
	}

	/* Success */
	p_ptr->r_idx = m_ptr->r_idx;
	px = m_ptr->fx;
	py = m_ptr->fy;
	lite_spot(py, px);

#if 0
	monster_death(m_idx, FALSE);
#else
	check_quest_completion(m_ptr);
#endif

	/* When the player snatches a Unique, it stays dead */
	if ((f1 & (RF1_UNIQUE)) && !(m_ptr->smart & (SM_CLONED)))
		r_ptr->max_num = 0;

	/* When the player snatches a Nazgul, it stays dead */
	if (f3 & (RF3_UNIQUE_7)) r_ptr->max_num--;

	r_ptr->r_tkills++;	/* Increase kill count */

	delete_monster_idx(m_idx);

	body_snatch_drop(ox, oy);
	culc_monster_hitpoints();
	check_experience();

#ifdef JP
	msg_print("新たな身体を手に入れた！");
#else
	msg_print("Gained new body!!");
#endif
	msg_print(NULL);

	lore_do_snatch(r_ptr);
	add_note((r_name + r_ptr->name), 's');

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_MAP | PR_TITLE);

	Term_erase(COL_CURSP, ROW_CURSP, 12);
	handle_stuff();

#ifdef JP
	msg_print("元の身体は朽ち果ててしまった．．．");
#else
	msg_print(" Previous body rotted away...");
#endif

	return (TRUE);
}
