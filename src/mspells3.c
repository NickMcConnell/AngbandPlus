/* File: mspells3.c */

/* Purpose: Player spells (attack monster) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#define SX          15
#define SY           1

typedef struct monster_spell monster_spell;

struct monster_spell
{
	int min_lev;
	int cost;
	int fail;
	bool linear;
	int stat;
	cptr name;
};

static monster_spell monst_spell[96] =
{
#ifdef JP
	/* RF4_XXX */
	{  1,  1, 20, 0, A_INT, "叫び" },
	{  0,  0,  0, 0, A_INT, "XXX2" },
	{ 40, 42, 90, 0, A_INT, "スターバースト" },
	{ 35, 30, 80, 0, A_STR, "ロケット" },
	{  1,  1, 20, 0, A_STR, "矢" },
	{  5,  1, 20, 0, A_DEX, "複数の矢" },
	{ 10,  2, 20, 0, A_STR, "強い矢" },
	{ 15,  2, 20, 0, A_DEX, "複数の強い矢" },
	{ 20, 15, 55, 1, A_CON, "酸のブレス" },
	{ 20, 15, 55, 1, A_CON, "電撃のブレス" },
	{ 20, 15, 55, 1, A_CON, "火炎のブレス" },
	{ 20, 15, 55, 1, A_CON, "冷気のブレス" },
	{ 20, 15, 55, 1, A_CON, "毒ののブレス" },
	{ 20, 15, 70, 1, A_CON, "地獄のブレス" },
	{ 20, 15, 70, 1, A_CON, "閃光のブレス" },
	{ 20, 15, 70, 1, A_CON, "暗黒のブレス" },
	{ 20, 20, 70, 1, A_CON, "混乱のブレス" },
	{ 20, 20, 70, 1, A_CON, "轟音のブレス" },
	{ 20, 20, 70, 1, A_CON, "カオスのブレス" },
	{ 20, 20, 70, 1, A_CON, "劣化のブレス" },
	{ 20, 25, 80, 1, A_CON, "因果混乱のブレス" },
	{ 30, 15, 70, 1, A_CON, "時間のブレス" },
	{ 30, 25, 70, 1, A_CON, "遅鈍のブレス" },
	{ 30, 25, 70, 1, A_CON, "重力のブレス" },
	{ 20, 15, 70, 1, A_CON, "破片のブレス" },
	{ 30, 25, 70, 1, A_CON, "プラズマのブレス" },
	{ 30, 25, 70, 1, A_CON, "フォースのブレス" },
	{ 30, 25, 80, 1, A_CON, "魔力のブレス" },
	{ 25, 20, 95, 0, A_INT, "放射能球" },
	{ 20, 15, 70, 1, A_CON, "放射性廃棄物のブレス" },
	{ 30, 32, 85, 0, A_INT, "カオス球" },
	{ 30, 25, 70, 1, A_CON, "分解のブレス" },

	/* RF5_XXX */
	{ 18, 13, 55, 0, A_INT, "アシッド・ボール" },
	{ 14, 10, 45, 0, A_INT, "サンダー・ボール" },
	{ 20, 14, 60, 0, A_INT, "ファイア・ボール" },
	{ 15, 11, 50, 0, A_INT, "アイス・ボール" },
	{  5,  3, 40, 0, A_INT, "悪臭雲" },
	{ 25, 18, 70, 0, A_INT, "地獄球" },
	{ 30, 22, 75, 0, A_INT, "ウォーター・ボール" },
	{ 44, 45, 85, 0, A_INT, "魔力の嵐" },
	{ 40, 42, 90, 0, A_INT, "暗黒の嵐" },
	{ 10,  5, 50, 0, A_INT, "魔力吸収" },
	{ 25, 10, 60, 0, A_INT, "精神攻撃" },
	{ 30, 14, 65, 0, A_INT, "脳攻撃" },
	{  3,  1, 25, 0, A_WIS, "軽傷" },
	{ 12,  2, 35, 0, A_WIS, "重傷" },
	{ 22,  6, 50, 0, A_WIS, "致命傷" },
	{ 32, 10, 70, 0, A_WIS, "瀕死傷" },
	{ 13,  7, 40, 0, A_INT, "アシッド・ボルト" },
	{ 10,  5, 35, 0, A_INT, "サンダー・ボルト" },
	{ 15,  9, 50, 0, A_INT, "ファイア・ボルト" },
	{ 12,  6, 35, 0, A_INT, "アイス・ボルト" },
	{  9,  4, 35, 0, A_INT, "ポイズン・ボルト" },
	{ 25, 17, 60, 0, A_INT, "地獄の矢" },
	{ 25, 20, 65, 0, A_INT, "ウォーター・ボルト" },
	{ 25, 24, 90, 0, A_INT, "魔力の矢" },
	{ 25, 20, 80, 0, A_INT, "プラズマ・ボルト" },
	{ 25, 16, 60, 0, A_INT, "極寒の矢" },
	{  3,  1, 25, 0, A_INT, "マジック・ミサイル" },
	{  5,  3, 35, 0, A_INT, "恐怖" },
	{ 10,  5, 40, 0, A_INT, "盲目" },
	{ 10,  5, 40, 0, A_INT, "パニック・モンスター" },
	{ 10,  5, 40, 0, A_INT, "スロー・モンスター" },
	{ 10,  5, 40, 0, A_INT, "スリープ・モンスター" },

	/* RF6_XXX */
	{ 25, 25, 80, 0, A_INT, "加速" },
	{ 40,120, 95, 0, A_INT, "破滅の手" },
	{ 30, 30, 80, 0, A_WIS, "体力回復" },
	{ 48,100, 90, 0, A_INT, "無傷球" },
	{  2,  2, 30, 0, A_INT, "ショート・テレポート" },
	{  5,  5, 40, 0, A_INT, "テレポート" },
	{  0,  0,  0, 0, A_INT, "XXX3" },
	{  0,  0,  0, 0, A_INT, "XXX4" },
	{ 15,  8, 50, 0, A_INT, "引き寄せる" },
	{ 20, 13, 80, 0, A_INT, "テレポート・アウェイ" },
	{ 30, 20, 90, 0, A_INT, "テレポート・レベル" },
	{  0,  0,  0, 0, A_INT, "XXX5" },
	{  5,  1, 20, 0, A_INT, "暗闇" },
	{  5,  1, 20, 0, A_INT, "トラップ生成" },
	{ 15,  3, 40, 0, A_INT, "記憶消去" },
	{ 25, 20, 80, 0, A_INT, "死者復活" },
	{ 40, 70, 70, 0, A_INT, "救援召喚" },
	{ 45, 90, 90, 0, A_INT, "サイバーデーモン召喚" },
	{ 25, 20, 65, 0, A_INT, "モンスター召喚" },
	{ 35, 30, 75, 0, A_INT, "複数モンスター召喚" },
	{ 25, 25, 65, 0, A_INT, "アリ召喚" },
	{ 25, 20, 60, 0, A_INT, "クモ召喚" },
	{ 35, 26, 75, 0, A_INT, "ハウンド召喚" },
	{ 30, 23, 70, 0, A_INT, "ヒドラ召喚" },
	{ 40, 50, 85, 0, A_INT, "天使召喚" },
	{ 35, 50, 80, 0, A_INT, "デーモン召喚" },
	{ 30, 30, 75, 0, A_INT, "アンデッド召喚" },
	{ 39, 70, 80, 0, A_INT, "ドラゴン召喚" },
	{ 43, 85, 85, 0, A_INT, "上級アンデッド召喚" },
	{ 46, 90, 85, 0, A_INT, "古代ドラゴン召喚" },
	{ 45, 90, 85, 0, A_INT, "上級デーモン召喚" },
	{ 50,150, 95, 0, A_INT, "ユニーク召喚" },
#else
	/* RF4_XXX */
	{  1,  1, 20, 0, A_INT, "Shriek" },
	{  0,  0,  0, 0, A_INT, "XXX2" },
	{ 40, 42, 90, 0, A_INT, "Star Burst" },
	{ 35, 30, 80, 0, A_STR, "Rocket" },
	{  1,  1, 20, 0, A_STR, "Arrow" },
	{  5,  1, 20, 0, A_DEX, "Arrows" },
	{ 10,  2, 20, 0, A_STR, "Strong Arrow" },
	{ 15,  2, 20, 0, A_DEX, "Strong Arrows" },
	{ 20, 15, 55, 1, A_CON, "Breath of Acid" },
	{ 20, 15, 55, 1, A_CON, "Breath of Lightning" },
	{ 20, 15, 55, 1, A_CON, "Breath of Fire" },
	{ 20, 15, 55, 1, A_CON, "Breath of Cold" },
	{ 20, 15, 55, 1, A_CON, "Breath of Poison" },
	{ 20, 15, 70, 1, A_CON, "Breath of Nether" },
	{ 20, 15, 70, 1, A_CON, "Breath of Light" },
	{ 20, 15, 70, 1, A_CON, "Breath of Dark" },
	{ 20, 20, 70, 1, A_CON, "Breath of Confusion" },
	{ 20, 20, 70, 1, A_CON, "Breath of Sound" },
	{ 20, 20, 70, 1, A_CON, "Breath of Chaos" },
	{ 20, 20, 70, 1, A_CON, "Breath of Disenchant" },
	{ 20, 25, 80, 1, A_CON, "Breath of Nexus" },
	{ 30, 15, 70, 1, A_CON, "Breath of Time" },
	{ 30, 25, 70, 1, A_CON, "Breath of Inertia" },
	{ 30, 25, 70, 1, A_CON, "Breath of Gravity" },
	{ 20, 15, 70, 1, A_CON, "Breath of Shard" },
	{ 30, 25, 70, 1, A_CON, "Breath of Plasma" },
	{ 30, 25, 70, 1, A_CON, "Breath of Force" },
	{ 30, 25, 80, 1, A_CON, "Breath of Mana" },
	{ 25, 20, 95, 0, A_INT, "Ball of Radiation" },
	{ 20, 15, 70, 1, A_CON, "Breath of Toxic Wastes" },
	{ 30, 32, 85, 0, A_INT, "Ball of Chaos" },
	{ 30, 25, 70, 1, A_CON, "Breath of Disen." },

	/* RF5_XXX */
	{ 18, 13, 55, 0, A_INT, "Acid Ball" },
	{ 14, 10, 45, 0, A_INT, "Lightning Ball" },
	{ 20, 14, 60, 0, A_INT, "Fire Ball" },
	{ 15, 11, 50, 0, A_INT, "Frost Ball" },
	{  5,  3, 40, 0, A_INT, "Stinking Cloud" },
	{ 25, 18, 70, 0, A_INT, "Nether Ball" },
	{ 30, 22, 75, 0, A_INT, "Water Ball" },
	{ 44, 45, 85, 0, A_INT, "Mana Storm" },
	{ 40, 42, 90, 0, A_INT, "Dark Storm" },
	{ 10,  5, 50, 0, A_INT, "Drain Mana" },
	{ 25, 10, 60, 0, A_INT, "Mind Blast" },
	{ 30, 14, 65, 0, A_INT, "Brain Smash" },
	{  3,  1, 25, 0, A_WIS, "Light Wounds" },
	{ 12,  2, 35, 0, A_WIS, "Serious Wounds" },
	{ 22,  6, 50, 0, A_WIS, "Critical Wounds" },
	{ 32, 10, 70, 0, A_WIS, "Mortal Wounds" },
	{ 13,  7, 40, 0, A_INT, "Acid Bolt" },
	{ 10,  5, 35, 0, A_INT, "Lightning Bolt" },
	{ 15,  9, 50, 0, A_INT, "Fire Bolt" },
	{ 12,  6, 35, 0, A_INT, "Frost Bolt" },
	{  9,  4, 35, 0, A_INT, "Poison Bolt" },
	{ 25, 17, 60, 0, A_INT, "Nether Bolt" },
	{ 25, 20, 65, 0, A_INT, "Water Bolt" },
	{ 25, 24, 90, 0, A_INT, "Mana Bolt" },
	{ 25, 20, 80, 0, A_INT, "Plasma Bolt" },
	{ 25, 16, 60, 0, A_INT, "Ice Bolt" },
	{  3,  1, 25, 0, A_INT, "Magic Missile" },
	{  5,  3, 35, 0, A_INT, "Scare Monster" },
	{ 10,  5, 40, 0, A_INT, "Blind Monster" },
	{ 10,  5, 40, 0, A_INT, "Confuse Monster" },
	{ 10,  5, 40, 0, A_INT, "Slow Monster" },
	{ 10,  5, 40, 0, A_INT, "Sleep Monster" },

	/* RF6_XXX */
	{ 25, 25, 80, 0, A_INT, "Haste Self" },
	{ 40,120, 95, 0, A_INT, "Hand of Doom" },
	{ 30, 30, 80, 0, A_WIS, "Healing" },
	{ 48,100, 90, 0, A_INT, "Invulnerability" },
	{  2,  2, 30, 0, A_INT, "Phase Door" },
	{  5,  5, 40, 0, A_INT, "Teleport Self" },
	{  0,  0,  0, 0, A_INT, "XXX3" },
	{  0,  0,  0, 0, A_INT, "XXX4" },
	{ 15,  8, 50, 0, A_INT, "Teleport To" },
	{ 20, 13, 80, 0, A_INT, "Teleport Monster" },
	{ 30, 20, 90, 0, A_INT, "Teleport Level" },
	{  0,  0,  0, 0, A_INT, "XXX5" },
	{  5,  1, 20, 0, A_INT, "Darkness" },
	{  5,  1, 20, 0, A_INT, "Trap Creation" },
	{ 15,  3, 40, 0, A_INT, "Amnesia" },
	{ 25, 20, 80, 0, A_INT, "Raise Dead" },
	{ 40, 70, 70, 0, A_INT, "Summon Kin" },
	{ 45, 90, 90, 0, A_INT, "Summon Cyber Demon" },
	{ 25, 20, 65, 0, A_INT, "Summon Monster" },
	{ 35, 30, 75, 0, A_INT, "Summon Monsters" },
	{ 25, 25, 65, 0, A_INT, "Summon Ants" },
	{ 25, 20, 60, 0, A_INT, "Summon Spiders" },
	{ 35, 26, 75, 0, A_INT, "Summon Hounds" },
	{ 30, 23, 70, 0, A_INT, "Summon Hydra" },
	{ 40, 50, 85, 0, A_INT, "Summon Angel" },
	{ 35, 50, 80, 0, A_INT, "Summon Demon" },
	{ 30, 30, 75, 0, A_INT, "Summon Undead" },
	{ 39, 70, 80, 0, A_INT, "Summon Dragon" },
	{ 43, 85, 85, 0, A_INT, "Summon Greater Undead" },
	{ 46, 90, 85, 0, A_INT, "Summon Ancient Dragon" },
	{ 45, 90, 85, 0, A_INT, "Summon Greater Demon" },
	{ 50,150, 95, 0, A_INT, "Summon Unique" },
#endif
};


static s16b monster_cost(int cost, bool linear)
{
	int tmp_mana = cost;

	if (p_ptr->dec_mana) tmp_mana = tmp_mana * 3 / 4;

	if (linear)
	{
		int frac = (p_ptr->chp < 500) ? (300 + (500 * p_ptr->chp) / 500) : 800;

		tmp_mana = tmp_mana * frac / 800;
	}

	if (tmp_mana < 1) tmp_mana = 1;

	return (tmp_mana);
}


static void monster_info(char *p, int power)
{
	int rlev = p_ptr->lev * 2;
	int chp = p_ptr->chp;

	strcpy(p, "");

	switch (96 + power)
	{
#ifdef JP
	case 96 + 2:  sprintf(p, " 損傷:%d+10d10", rlev * 4); break;
	case 96 + 3:  sprintf(p, " 損傷:%d", MIN(800, (chp / 4))); break;
	case 96 + 4:  sprintf(p, " 損傷:%dd%d", 1, 6); break;
	case 96 + 5:  sprintf(p, " 損傷:%dd%d", 3, 6); break;
	case 96 + 6:  sprintf(p, " 損傷:%dd%d", 5, 6); break;
	case 96 + 7:  sprintf(p, " 損傷:%dd%d", 7, 6); break;
	case 96 + 8:  sprintf(p, " 損傷:%d", MIN(1200, (chp / 3))); break;
	case 96 + 9:  sprintf(p, " 損傷:%d", MIN(1200, (chp / 3))); break;
	case 96 + 10:  sprintf(p, " 損傷:%d", MIN(1200, (chp / 3))); break;
	case 96 + 11:  sprintf(p, " 損傷:%d", MIN(1200, (chp / 3))); break;
	case 96 + 12:  sprintf(p, " 損傷:%d", MIN(800, (chp / 3))); break;
	case 96 + 13:  sprintf(p, " 損傷:%d", MIN(500, (chp / 6))); break;
	case 96 + 14:  sprintf(p, " 損傷:%d", MIN(400, (chp / 6))); break;
	case 96 + 15:  sprintf(p, " 損傷:%d", MIN(400, (chp / 6))); break;
	case 96 + 16:  sprintf(p, " 損傷:%d", MIN(400, (chp / 6))); break;
	case 96 + 17:  sprintf(p, " 損傷:%d", MIN(400, (chp / 6))); break;
	case 96 + 18:  sprintf(p, " 損傷:%d", MIN(500, (chp / 6))); break;
	case 96 + 19:  sprintf(p, " 損傷:%d", MIN(500, (chp / 6))); break;
	case 96 + 20:  sprintf(p, " 損傷:%d", MIN(250, (chp / 3))); break;
	case 96 + 21:  sprintf(p, " 損傷:%d", MIN(150, (chp / 3))); break;
	case 96 + 22:  sprintf(p, " 損傷:%d", MIN(200, (chp / 6))); break;
	case 96 + 23:  sprintf(p, " 損傷:%d", MIN(200, (chp / 3))); break;
	case 96 + 24:  sprintf(p, " 損傷:%d", MIN(400, (chp / 6))); break;
	case 96 + 25:  sprintf(p, " 損傷:%d", MIN(150, (chp / 6))); break;
	case 96 + 26:  sprintf(p, " 損傷:%d", MIN(200, (chp / 6))); break;
	case 96 + 27:  sprintf(p, " 損傷:%d", MIN(250, (chp / 3))); break;
	case 96 + 28:  sprintf(p, " 損傷:%d+10d6", rlev); break;
	case 96 + 29:  sprintf(p, " 損傷:%d", MIN(800, (chp / 3))); break;
	case 96 + 30:  sprintf(p, " 損傷:%d+10d10", rlev * 2); break;
	case 96 + 31:  sprintf(p, " 損傷:%d", MIN(150, (chp / 3))); break;

	case 128 + 0:  sprintf(p, " 損傷:15+d%d", rlev * 3); break;
	case 128 + 1:  sprintf(p, " 損傷:8+d%d", rlev * 3 / 2); break;
	case 128 + 2:  sprintf(p, " 損傷:10+d%d", rlev * 7 / 2); break;
	case 128 + 3:  sprintf(p, " 損傷:10+d%d", rlev * 3 / 2); break;
	case 128 + 4:  sprintf(p, " 損傷:%dd%d", 12, 2); break;
	case 128 + 5:  sprintf(p, " 損傷:%d+10d10", 50 + rlev); break;
	case 128 + 6:  sprintf(p, " 損傷:50+d%d", rlev * 5 / 2); break;
	case 128 + 7:  sprintf(p, " 損傷:%d+10d10", rlev * 4); break;
	case 128 + 8:  sprintf(p, " 損傷:%d+10d10", rlev * 4); break;
	case 128 + 9:  sprintf(p, " 回復:6+d%d", rlev / 2); break;
	case 128 + 10:  sprintf(p, " 損傷:%dd%d", 8, 8); break;
	case 128 + 11:  sprintf(p, " 損傷:%dd%d", 12, 15); break;
	case 128 + 12:  sprintf(p, " 損傷:%dd%d", 3, 8); break;
	case 128 + 13:  sprintf(p, " 損傷:%dd%d", 8, 8); break;
	case 128 + 14:  sprintf(p, " 損傷:%dd%d", 10, 15); break;
	case 128 + 15:  sprintf(p, " 損傷:%dd%d", 15, 15); break;
	case 128 + 16:  sprintf(p, " 損傷:%d+7d8", rlev / 3); break;
	case 128 + 17:  sprintf(p, " 損傷:%d+4d8", rlev / 3); break;
	case 128 + 18:  sprintf(p, " 損傷:%d+9d8", rlev / 3); break;
	case 128 + 19:  sprintf(p, " 損傷:%d+6d8", rlev / 3); break;
	case 128 + 20:  sprintf(p, " 損傷:%d+3d8", rlev / 3); break;
	case 128 + 21:  sprintf(p, " 損傷:%d+5d5", rlev * 3 / 2); break;
	case 128 + 22:  sprintf(p, " 損傷:%d+10d10", rlev); break;
	case 128 + 23:  sprintf(p, " 損傷:50+d%d", rlev * 7 / 2); break;
	case 128 + 24:  sprintf(p, " 損傷:%d+8d7", rlev); break;
	case 128 + 25:  sprintf(p, " 損傷:%d+6d6", rlev); break;
	case 128 + 26:  sprintf(p, " 損傷:%d+2d6", rlev / 3); break;
	case 128 + 27:  sprintf(p, " 効力:%d", MAX(1, rlev - 10)); break;
	case 128 + 28:  sprintf(p, " 効力:%d", MAX(1, rlev - 10)); break;
	case 128 + 29:  sprintf(p, " 効力:%d", MAX(1, rlev - 10)); break;
	case 128 + 30:  sprintf(p, " 効力:%d", MAX(1, rlev - 10)); break;
	case 128 + 31:  sprintf(p, " 効力:%d", MAX(1, rlev - 10)); break;

	case 160 + 0:  sprintf(p, " 期間:%d+d%d", rlev / 2, 20 + rlev / 2); break;
	case 160 + 2:  sprintf(p, " 回復:%d", rlev * 4); break;
	case 160 + 3:  sprintf(p, " 期間:%d+d%d", 8, 8); break;
	case 160 + 4:  sprintf(p, " 距離:%d", 10); break;
	case 160 + 5:  sprintf(p, " 距離:%d", MAX_SIGHT * 2 + 5); break;
	case 160 + 9:  sprintf(p, " 距離:%d", MAX_SIGHT * 2 + 5); break;
	case 160 + 14:  sprintf(p, " 効力:%d", MAX(1, rlev - 10)); break;
#else
	case 96 + 2:  sprintf(p, " dam %d+10d10", rlev * 4); break;
	case 96 + 3:  sprintf(p, " dam %d", MIN(800, (chp / 4))); break;
	case 96 + 4:  sprintf(p, " dam %dd%d", 1, 6); break;
	case 96 + 5:  sprintf(p, " dam %dd%d", 3, 6); break;
	case 96 + 6:  sprintf(p, " dam %dd%d", 5, 6); break;
	case 96 + 7:  sprintf(p, " dam %dd%d", 7, 6); break;
	case 96 + 8:  sprintf(p, " dam %d", MIN(1200, (chp / 3))); break;
	case 96 + 9:  sprintf(p, " dam %d", MIN(1200, (chp / 3))); break;
	case 96 + 10:  sprintf(p, " dam %d", MIN(1200, (chp / 3))); break;
	case 96 + 11:  sprintf(p, " dam %d", MIN(1200, (chp / 3))); break;
	case 96 + 12:  sprintf(p, " dam %d", MIN(800, (chp / 3))); break;
	case 96 + 13:  sprintf(p, " dam %d", MIN(500, (chp / 6))); break;
	case 96 + 14:  sprintf(p, " dam %d", MIN(400, (chp / 6))); break;
	case 96 + 15:  sprintf(p, " dam %d", MIN(400, (chp / 6))); break;
	case 96 + 16:  sprintf(p, " dam %d", MIN(400, (chp / 6))); break;
	case 96 + 17:  sprintf(p, " dam %d", MIN(400, (chp / 6))); break;
	case 96 + 18:  sprintf(p, " dam %d", MIN(500, (chp / 6))); break;
	case 96 + 19:  sprintf(p, " dam %d", MIN(500, (chp / 6))); break;
	case 96 + 20:  sprintf(p, " dam %d", MIN(250, (chp / 3))); break;
	case 96 + 21:  sprintf(p, " dam %d", MIN(150, (chp / 3))); break;
	case 96 + 22:  sprintf(p, " dam %d", MIN(200, (chp / 6))); break;
	case 96 + 23:  sprintf(p, " dam %d", MIN(200, (chp / 3))); break;
	case 96 + 24:  sprintf(p, " dam %d", MIN(400, (chp / 6))); break;
	case 96 + 25:  sprintf(p, " dam %d", MIN(150, (chp / 6))); break;
	case 96 + 26:  sprintf(p, " dam %d", MIN(200, (chp / 6))); break;
	case 96 + 27:  sprintf(p, " dam %d", MIN(250, (chp / 3))); break;
	case 96 + 28:  sprintf(p, " dam %d+10d6", rlev); break;
	case 96 + 29:  sprintf(p, " dam %d", MIN(800, (chp / 3))); break;
	case 96 + 30:  sprintf(p, " dam %d+10d10", rlev * 2); break;
	case 96 + 31:  sprintf(p, " dam %d", MIN(150, (chp / 3))); break;

	case 128 + 0:  sprintf(p, " dam 15+d%d", rlev * 3); break;
	case 128 + 1:  sprintf(p, " dam 8+d%d", rlev * 3 / 2); break;
	case 128 + 2:  sprintf(p, " dam 10+d%d", rlev * 7 / 2); break;
	case 128 + 3:  sprintf(p, " dam 10+d%d", rlev * 3 / 2); break;
	case 128 + 4:  sprintf(p, " dam %dd%d", 12, 2); break;
	case 128 + 5:  sprintf(p, " dam %d+10d10", 50 + rlev); break;
	case 128 + 6:  sprintf(p, " dam 50+d%d", rlev * 5 / 2); break;
	case 128 + 7:  sprintf(p, " dam %d+10d10", rlev * 4); break;
	case 128 + 8:  sprintf(p, " dam %d+10d10", rlev * 4); break;
	case 128 + 9:  sprintf(p, " heal 6+d%d", rlev / 2); break;
	case 128 + 10:  sprintf(p, " dam %dd%d", 8, 8); break;
	case 128 + 11:  sprintf(p, " dam %dd%d", 12, 15); break;
	case 128 + 12:  sprintf(p, " dam %dd%d", 3, 8); break;
	case 128 + 13:  sprintf(p, " dam %dd%d", 8, 8); break;
	case 128 + 14:  sprintf(p, " dam %dd%d", 10, 15); break;
	case 128 + 15:  sprintf(p, " dam %dd%d", 15, 15); break;
	case 128 + 16:  sprintf(p, " dam %d+7d8", rlev / 3); break;
	case 128 + 17:  sprintf(p, " dam %d+4d8", rlev / 3); break;
	case 128 + 18:  sprintf(p, " dam %d+9d8", rlev / 3); break;
	case 128 + 19:  sprintf(p, " dam %d+6d8", rlev / 3); break;
	case 128 + 20:  sprintf(p, " dam %d+3d8", rlev / 3); break;
	case 128 + 21:  sprintf(p, " dam %d+5d5", rlev * 3 / 2); break;
	case 128 + 22:  sprintf(p, " dam %d+10d10", rlev); break;
	case 128 + 23:  sprintf(p, " dam 50+d%d", rlev * 7 / 2); break;
	case 128 + 24:  sprintf(p, " dam %d+8d7", rlev); break;
	case 128 + 25:  sprintf(p, " dam %d+6d6", rlev); break;
	case 128 + 26:  sprintf(p, " dam %d+2d6", rlev / 3); break;
	case 128 + 27:  sprintf(p, " pow %d", MAX(1, rlev - 10)); break;
	case 128 + 28:  sprintf(p, " pow %d", MAX(1, rlev - 10)); break;
	case 128 + 29:  sprintf(p, " pow %d", MAX(1, rlev - 10)); break;
	case 128 + 30:  sprintf(p, " pow %d", MAX(1, rlev - 10)); break;
	case 128 + 31:  sprintf(p, " pow %d", MAX(1, rlev - 10)); break;

	case 160 + 0:  sprintf(p, " dur %d+d%d", rlev / 2, 20 + rlev / 2); break;
	case 160 + 2:  sprintf(p, " heal %d", rlev * 4); break;
	case 160 + 3:  sprintf(p, " dur %d+d%d", 8, 8); break;
	case 160 + 4:  sprintf(p, " range %d", 10); break;
	case 160 + 5:  sprintf(p, " range %d", MAX_SIGHT * 2 + 5); break;
	case 160 + 9:  sprintf(p, " range %d", MAX_SIGHT * 2 + 5); break;
	case 160 + 14:  sprintf(p, " pow %d", MAX(1, rlev - 10)); break;
#endif
	}
}


static bool cast_monster_spell(int n)
{
	int k, dir, dam, count = 0;

	monster_race *r_ptr = &r_info[p_ptr->r_idx];

	int t_idx;
	monster_type *t_ptr = NULL;
	monster_race *tr_ptr = NULL;

	char t_name[80];

	bool wake_up = FALSE;
	bool fear = FALSE;

	bool friendly = TRUE;
	bool pet = TRUE;

	/* Expected monster level */
	int rlev = p_ptr->lev * 2;

	/* Expected ball spell radius */
	int rad = (r_ptr->flags2 & RF2_POWERFUL) ? 3 : 2;

	switch (n)
	{
		/* RF4_SHRIEK */
		case 96+0:
		{
#ifdef JP
			msg_print("かん高い金切り声をあげた。");
#else
			msg_print("Makes a high pitched shriek.");
#endif
			aggravate_monsters(0);
			return (TRUE);
		}

		/* RF4_XXX2X4 */
		case 96+1:
		{
			break;
		}

		/* RF4_BA_LITE */
		case 96+2:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = (rlev * 4) + damroll(10, 10);
			fire_ball(GF_LITE, dir, dam, 4);
			break;
		}

		/* RF4_ROCKET */
		case 96+3:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_ROCKET);
			dam = (p_ptr->chp / 4) > 800 ? 800 : (p_ptr->chp / 4);
			fire_ball(GF_ROCKET, dir, dam, 2);
			break;
		}

		/* RF4_ARROW_1 */
		case 96+4:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_SHOOT);
			dam = damroll(1, 6);
			fire_bolt(GF_ARROW, dir, dam);
			break;
		}

		/* RF4_ARROW_2 */
		case 96+5:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_SHOOT);
			dam = damroll(3, 6);
			fire_bolt(GF_ARROW, dir, dam);
			break;
		}

		/* RF4_ARROW_3 */
		case 96+6:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_SHOOT);
			dam = damroll(5, 6);
			fire_bolt(GF_ARROW, dir, dam);
			break;
		}

		/* RF4_ARROW_4 */
		case 96+7:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_SHOOT);
			dam = damroll(7, 6);
			fire_bolt(GF_ARROW, dir, dam);
			break;
		}

		/* RF4_BR_ACID */
		case 96+8:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 3) > 1200 ? 1200 : (p_ptr->chp / 3);
			fire_ball(GF_ACID, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_ELEC */
		case 96+9:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 3) > 1200 ? 1200 : (p_ptr->chp / 3);
			fire_ball(GF_ELEC, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_FIRE */
		case 96+10:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 3) > 1200 ? 1200 : (p_ptr->chp / 3);
			fire_ball(GF_FIRE, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_COLD */
		case 96+11:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 3) > 1200 ? 1200 : (p_ptr->chp / 3);
			fire_ball(GF_COLD, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_POIS */
		case 96+12:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 3) > 800 ? 800 : (p_ptr->chp / 3);
			fire_ball(GF_POIS, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_NETH */
		case 96+13:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 6) > 500 ? 500 : (p_ptr->chp / 6);
			fire_ball(GF_NETHER, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_LITE */
		case 96+14:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 6) > 400 ? 400 : (p_ptr->chp / 6);
			fire_ball(GF_LITE, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_DARK */
		case 96+15:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 6) > 400 ? 400 : (p_ptr->chp / 6);
			fire_ball(GF_DARK, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_CONF */
		case 96+16:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 6) > 400 ? 400 : (p_ptr->chp / 6);
			fire_ball(GF_CONFUSION, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_SOUN */
		case 96+17:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 6) > 400 ? 400 : (p_ptr->chp / 6);
			fire_ball(GF_SOUND, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_CHAO */
		case 96+18:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 6) > 500 ? 500 : (p_ptr->chp / 6);
			fire_ball(GF_CHAOS, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_DISE */
		case 96+19:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 6) > 500 ? 500 : (p_ptr->chp / 6);
			fire_ball(GF_DISENCHANT, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_NEXU */
		case 96+20:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 3) > 250 ? 250 : (p_ptr->chp / 3);
			fire_ball(GF_NEXUS, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_TIME */
		case 96+21:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 3) > 150 ? 150 : (p_ptr->chp / 3);
			fire_ball(GF_TIME, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_INER */
		case 96+22:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 6) > 200 ? 200 : (p_ptr->chp / 6);
			fire_ball(GF_INERTIA, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_GRAV */
		case 96+23:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 3) > 200 ? 200 : (p_ptr->chp / 3);
			fire_ball(GF_GRAVITY, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_SHAR */
		case 96+24:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 6) > 400 ? 400 : (p_ptr->chp / 6);
			fire_ball(GF_SHARDS, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_PLAS */
		case 96+25:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 6) > 150 ? 150 : (p_ptr->chp / 6);
			fire_ball(GF_PLASMA, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_WALL */
		case 96+26:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 6) > 200 ? 200 : (p_ptr->chp / 6);
			fire_ball(GF_FORCE, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BR_MANA */
		case 96+27:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 3) > 250 ? 250 : (p_ptr->chp / 3);
			fire_ball(GF_MANA, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BA_NUKE */
		case 96+28:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = rlev + damroll(10, 6);
			fire_ball(GF_NUKE, dir, dam, 2);
			break;
		}

		/* RF4_BR_NUKE */
		case 96+29:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 3) > 800 ? 800 : (p_ptr->chp / 3);
			fire_ball(GF_NUKE, dir, dam, (0 - rad));
			break;
		}

		/* RF4_BA_CHAO */
		case 96+30:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = (rlev * 2) + damroll(10, 10);
			fire_ball(GF_CHAOS, dir, dam, 4);
			break;
		}

		/* RF4_BR_DISI -> Breathe Disintegration */
		case 96+31:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			sound(SOUND_BREATH);
			dam = (p_ptr->chp / 3) > 150 ? 150 : (p_ptr->chp / 3);
			fire_ball(GF_DISINTEGRATE, dir, dam, (0 - rad));
			break;
		}

		/* RF5_BA_ACID */
		case 128+0:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = randint1(rlev * 3) + 15;
			fire_ball(GF_ACID, dir, dam, 2);
			break;
		}

		/* RF5_BA_ELEC */
		case 128+1:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = randint1(rlev * 3 / 2) + 8;
			fire_ball(GF_ELEC, dir, dam, 2);
			break;
		}

		/* RF5_BA_FIRE */
		case 128+2:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = randint1(rlev * 7 / 2) + 10;
			fire_ball(GF_FIRE, dir, dam, 2);
			break;
		}

		/* RF5_BA_COLD */
		case 128+3:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = randint1(rlev * 3 / 2) + 10;
			fire_ball(GF_COLD, dir, dam, 2);
			break;
		}

		/* RF5_BA_POIS */
		case 128+4:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = damroll(12, 2);
			fire_ball(GF_POIS, dir, dam, 2);
			break;
		}

		/* RF5_BA_NETH */
		case 128+5:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = 50 + damroll(10, 10) + rlev;
			fire_ball(GF_NETHER, dir, dam, 2);
			break;
		}

		/* RF5_BA_WATE */
		case 128+6:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = randint1(rlev * 5 / 2) + 50;
			fire_ball(GF_WATER, dir, dam, 4);
			break;
		}

		/* RF5_BA_MANA */
		case 128+7:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = (rlev * 4) + damroll(10, 10);
			fire_ball(GF_MANA, dir, dam, 4);
			break;
		}

		/* RF5_BA_DARK */
		case 128+8:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = (rlev * 4) + damroll(10, 10);
			fire_ball(GF_DARK, dir, dam, 4);
			break;
		}

		/* RF5_DRAIN_MANA */
		case 128+9:
		{
			/* Attack power */
			int power = (randint1(rlev) / 2) + 1;

			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

			/* Basic message */
			if (is_seen(t_ptr))
			{
#ifdef JP
				msg_format("精神エネルギーを%sから吸いとった。", t_name);
#else
				msg_format("Draws psychic energy from %s.", t_name);
#endif
			}

			/* Heal the monster */
			if (p_ptr->chp < p_ptr->mhp)
			{
				if (!tr_ptr->flags4 && !tr_ptr->flags5 && !tr_ptr->flags6)
				{
					if (is_seen(t_ptr))
					{
#ifdef JP
						msg_format("%^sには効果がなかった。", t_name);
#else
						msg_format("%^s is unaffected!", t_name);
#endif
					}
				}
				else
				{
					/* Heal */
					p_ptr->chp += 6 * power;

					if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;

					/* Redraw (later) if needed */
					p_ptr->redraw |= (PR_HP);

					/* Special message */
					if (is_seen(t_ptr))
					{
#ifdef JP
						msg_print("気分が良い。");
#else
						msg_print("Feels healthier.");
#endif
					}
				}
			}

			wake_up = TRUE;
			break;
		}

		/* RF5_MIND_BLAST */
		case 128+10:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_idx = cave[target_row][target_col].m_idx;
			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

			if (is_seen(t_ptr))
			{
#ifdef JP
				msg_format("%sをじっと睨んだ。", t_name);
#else
				msg_format("Gazes intently at %s.", t_name);
#endif
			}

			/* Attempt a saving throw */
			if ((tr_ptr->flags1 & RF1_UNIQUE) ||
				 (tr_ptr->flags3 & RF3_NO_CONF) ||
				 (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10))
			{
				/* No obvious effect */
				if (is_seen(t_ptr))
				{
					/* Memorize a flag */
					if (tr_ptr->flags3 & (RF3_NO_CONF))
					{
						tr_ptr->r_flags3 |= (RF3_NO_CONF);
					}

#ifdef JP
					msg_format("%^sには効果がなかった。", t_name);
#else
					msg_format("%^s is unaffected!", t_name);
#endif
				}
			}
			else
			{
				if (is_seen(t_ptr))
				{
#ifdef JP
					msg_format("%^sは精神攻撃を食らった。", t_name);
#else
					msg_format("%^s is blasted by psionic energy.", t_name);
#endif
				}

				t_ptr->confused += randint0(4) + 4;
#ifdef JP
				mon_take_hit_mon(t_idx, damroll(8, 8), &fear, "の精神は崩壊し、肉体は抜け殻となった。", 0);
#else
				mon_take_hit_mon(t_idx, damroll(8, 8), &fear, " collapses, a mindless husk.", 0);
#endif
			}

			wake_up = TRUE;
			break;
		}

		/* RF5_BRAIN_SMASH */
		case 128+11:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_idx = cave[target_row][target_col].m_idx;
			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

			if (is_seen(t_ptr))
			{
#ifdef JP
				msg_format("%sをじっと睨んだ。", t_name);
#else
				msg_format("Gazes intently at %s.", t_name);
#endif
			}

			/* Attempt a saving throw */
			if ((tr_ptr->flags1 & RF1_UNIQUE) ||
				 (tr_ptr->flags3 & RF3_NO_CONF) ||
				 (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10))
			{
				/* No obvious effect */
				if (is_seen(t_ptr))
				{
				/* Memorize a flag */
				if (tr_ptr->flags3 & (RF3_NO_CONF))
				{
					tr_ptr->r_flags3 |= (RF3_NO_CONF);
				}

#ifdef JP
				msg_format("%^sには効果がなかった。", t_name);
#else
				msg_format("%^s is unaffected!", t_name);
#endif
				}
			}
			else
			{
				if (is_seen(t_ptr))
				{
#ifdef JP
				msg_format("%^sは精神攻撃を食らった。", t_name);
#else
				msg_format("%^s is blasted by psionic energy.", t_name);
#endif
				}

				t_ptr->confused += randint0(4) + 4;
				t_ptr->mspeed -= randint0(4) + 4;
				t_ptr->stunned += randint0(4) + 4;

#ifdef JP
				mon_take_hit_mon(t_idx, damroll(12, 15), &fear, "の精神は崩壊し、肉体は抜け殻となった。", 0);
#else
				mon_take_hit_mon(t_idx, damroll(12, 15), &fear, " collapses, a mindless husk.", 0);
#endif
			}

			wake_up = TRUE;
			break;
		}

		/* RF5_CAUSE_1 */
		case 128+12:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_idx = cave[target_row][target_col].m_idx;
			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

#ifdef JP
			msg_format("%sを指さして呪いをかけた。", t_name);
#else
			msg_format("Points at %s and curses.", t_name);
#endif

			if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは耐性を持っている！", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s resists!", t_name);
#endif
			}
			else
			{
#ifdef JP
				mon_take_hit_mon(t_idx, damroll(3, 8), &fear, "は死んだ。", 0);
#else
				mon_take_hit_mon(t_idx, damroll(3, 8), &fear, " is destroyed.", 0);
#endif
			}

			wake_up = TRUE;
			break;
		}

		/* RF5_CAUSE_2 */
		case 128+13:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_idx = cave[target_row][target_col].m_idx;
			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

#ifdef JP
			msg_format("%sを指さして恐ろしげに呪いをかけた。", t_name);
#else
			msg_format("Points at %s and curses horribly.", t_name);
#endif

			if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは耐性を持っている！", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s resists!", t_name);
#endif
			}
			else
			{
#ifdef JP
				mon_take_hit_mon(t_idx, damroll(8, 8), &fear, "は死んだ。", 0);
#else
				mon_take_hit_mon(t_idx, damroll(8, 8), &fear, " is destroyed.", 0);
#endif
			}

			wake_up = TRUE;
			break;
		}

		/* RF5_CAUSE_3 */
		case 128+14:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_idx = cave[target_row][target_col].m_idx;
			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

#ifdef JP
			msg_format("%sを指さし、恐ろしげに呪文を唱えた！", t_name);
#else
			msg_format("Points at %s, incanting terribly!", t_name);
#endif

			if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは耐性を持っている！", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s resists!", t_name);
#endif
			}
			else
			{
#ifdef JP
				mon_take_hit_mon(t_idx, damroll(10, 15), &fear, "は死んだ。", 0);
#else
				mon_take_hit_mon(t_idx, damroll(10, 15), &fear, " is destroyed.", 0);
#endif
			}

			wake_up = TRUE;
			break;
		}

		/* RF5_CAUSE_4 */
		case 128+15:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_idx = cave[target_row][target_col].m_idx;
			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

#ifdef JP
			msg_format("%sを指差し、「死ね！」と叫んだ。", t_name);
#else
			msg_format("Points at %s, screaming the word, 'DIE!'", t_name);
#endif

			if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは耐性を持っている！", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s resists!", t_name);
#endif
			}
			else
			{
#ifdef JP
				mon_take_hit_mon(t_idx, damroll(15, 15), &fear, "は死んだ。", 0);
#else
				mon_take_hit_mon(t_idx, damroll(15, 15), &fear, " is destroyed.", 0);
#endif
			}

			wake_up = TRUE;
			break;
		}

		/* RF5_BO_ACID */
		case 128+16:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = damroll(7, 8) + (rlev / 3);
			fire_bolt(GF_ACID, dir, dam);
			break;
		}

		/* RF5_BO_ELEC */
		case 128+17:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = damroll(4, 8) + (rlev / 3);
			fire_bolt(GF_ELEC, dir, dam);
			break;
		}

		/* RF5_BO_FIRE */
		case 128+18:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = damroll(9, 8) + (rlev / 3);
			fire_bolt(GF_FIRE, dir, dam);
			break;
		}

		/* RF5_BO_COLD */
		case 128+19:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = damroll(6, 8) + (rlev / 3);
			fire_bolt(GF_COLD, dir, dam);
			break;
		}

		/* RF5_BO_POIS */
		case 128+20:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = damroll(3, 8) + (rlev / 3);
			fire_bolt(GF_COLD, dir, dam);
			break;
		}

		/* RF5_BO_NETH */
		case 128+21:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = 30 + damroll(5, 5) + (rlev * 3) / 2;
			fire_bolt(GF_NETHER, dir, dam);
			break;
		}

		/* RF5_BO_WATE */
		case 128+22:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = damroll(10, 10) + rlev;
			fire_bolt(GF_WATER, dir, dam);
			break;
		}

		/* RF5_BO_MANA */
		case 128+23:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = randint1(rlev * 7 / 2) + 50;
			fire_bolt(GF_MANA, dir, dam);
			break;
		}

		/* RF5_BO_PLAS */
		case 128+24:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = 10 + damroll(8, 7) + rlev;
			fire_bolt(GF_PLASMA, dir, dam);
			break;
		}

		/* RF5_BO_ICEE */
		case 128+25:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = damroll(6, 6) + rlev;
			fire_bolt(GF_ICE, dir, dam);
			break;
		}

		/* RF5_MISSILE */
		case 128+26:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dam = damroll(2, 6) + (rlev / 3);
			fire_bolt(GF_MISSILE, dir, dam);
			break;
		}

		/* RF5_SCARE */
		case 128+27:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

#ifdef JP
			msg_format("%^sに向かって恐ろしげな幻覚を作り出した。", t_name);
#else
			msg_format("Casts a fearful illusion in front of %s.", t_name);
#endif

			if (tr_ptr->flags3 & RF3_NO_FEAR)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは恐怖を感じない。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s refuses to be frightened.", t_name);
#endif
			}
			else if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは恐怖を感じない。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s refuses to be frightened.", t_name);
#endif
			}
			else
			{
				if (!t_ptr->monfear) fear = TRUE;
				t_ptr->monfear += randint0(4) + 4;
			}

			/* wakes up a target monster */
			wake_up = TRUE;
			break;
		}

		/* RF5_BLIND */
		case 128+28:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

#ifdef JP
			msg_format("%^sの目を焼き付かせた。", t_name);
#else
			msg_format("Casts a spell, burning %s's eyes.", t_name);
#endif

			/* Simulate blindness with confusion */
			if (tr_ptr->flags3 & RF3_NO_CONF)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sには効果がなかった。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s is unaffected.", t_name);
#endif
			}
			else if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sには効果がなかった。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s is unaffected.", t_name);
#endif
			}
			else
			{
#ifdef JP
				if (is_seen(t_ptr))   msg_format("%^sは目が見えなくなった！ ", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s is blinded!", t_name);
#endif
				t_ptr->confused += 12 + (byte)randint0(4);
			}

			wake_up = TRUE;
			break;
		}

		/* RF5_CONF */
		case 128+29:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

#ifdef JP
			msg_format("%^sの前に幻惑的な幻をつくり出した。", t_name);
#else
			msg_format("Casts a mesmerizing illusion in front of %s.", t_name);
#endif

			if (tr_ptr->flags3 & RF3_NO_CONF)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは惑わされなかった。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s disbelieves the feeble spell.", t_name);
#endif
			}
			else if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは惑わされなかった。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s disbelieves the feeble spell.", t_name);
#endif
			}
			else
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは混乱したようだ。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s seems confused.", t_name);
#endif
				t_ptr->confused += 12 + (byte)randint0(4);
			}

			wake_up = TRUE;
			break;
		}

		/* RF5_SLOW */
		case 128+30:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

#ifdef JP
			msg_format("%^sの筋肉から力を吸いとった。", t_name);
#else
			msg_format("Drains power from %s%s muscles.", t_name,
					  (streq(t_name, "it") ? "s" : "'s"));
#endif

			if (tr_ptr->flags1 & RF1_UNIQUE)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sには効果がなかった。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s is unaffected.", t_name);
#endif
			}
			else if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sには効果がなかった。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s is unaffected.", t_name);
#endif
			}
			else
			{
				if (!t_ptr->slowed)
				{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%sの動きが遅くなった。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s starts moving slower.", t_name);
#endif
				}
				t_ptr->slowed = MIN(200, t_ptr->slowed + 50);
			}

			wake_up = TRUE;
			break;
		}

		/* RF5_HOLD */
		case 128+31:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

#ifdef JP
			msg_format("%^sをじっと見つめた。", t_name);
#else
			msg_format("Stares intently at %s.", t_name);
#endif

			if ((tr_ptr->flags1 & RF1_UNIQUE) || (tr_ptr->flags3 & RF3_NO_STUN))
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sには効果がなかった。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s is unaffected.", t_name);
#endif
				wake_up = TRUE;
			}
			else if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sには効果がなかった。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s is unaffected.", t_name);
#endif
				wake_up = TRUE;
			}
			else
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは麻痺した！", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s is paralyzed!", t_name);
#endif
				t_ptr->csleep += rlev * 10;
			}

			break;
		}

		/* RF6_HASTE */
		case 160+0:
		{
			if (!p_ptr->fast)
			{
				(void)set_fast(randint1(20 + rlev / 2) + rlev / 2);
			}
			break;
		}

		/* RF6_HAND_DOOM */
		case 160+1:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

#ifdef JP
			msg_format("%^sに<破滅の手>を放った！", t_name);
#else
			msg_format("Invokes the Hand of Doom upon %s!", t_name);
#endif

			if (tr_ptr->flags1 & RF1_UNIQUE)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sには効果がなかった！", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s is unaffected!", t_name);
#endif
			}
			else
			{
				if ((r_ptr->level + randint1(20)) >
				(tr_ptr->level + 10 + randint1(20)))
				{
					t_ptr->hp = t_ptr->hp -
					  (((s32b)((65 + randint1(25)) * t_ptr->hp)) / 100);

					if (t_ptr->hp < 1) t_ptr->hp = 1;
				}
				else
				{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは耐性を持っている！", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s resists!", t_name);
#endif
				}
			}

			wake_up = TRUE;
			break;
		}

		/* RF6_HEAL */
		case 160+2:
		{
#ifdef JP
			msg_print("自分の傷に念を集中した。");
#else
			msg_print("Concentrates on your wounds.");
#endif

			/* Heal some ( was rlev*6 ) */
			p_ptr->chp += (rlev * 4);

			/* Fully healed */
			if (p_ptr->chp >= p_ptr->mhp)
			{
				/* Fully healed */
				p_ptr->chp = p_ptr->mhp;
#ifdef JP
				msg_print("完全に治った！");
#else
				msg_print("Completely healed!");
#endif
			}
			else
			{
#ifdef JP
				msg_print("気分が良い！");
#else
				msg_print("Feels healthier!");
#endif
			}

			/* Redraw (later) if needed */
			p_ptr->redraw |= (PR_HP);

			break;
		}

		/* RF6_INVULNER */
		case 160+3:
		{
			(void)set_invuln(p_ptr->invuln + randint1(8) + 8);
			break;
		}

		/* RF6_BLINK */
		case 160+4:
		{
			teleport_player(10);
			break;
		}

		/* RF6_TPORT */
		case 160+5:
		{
			teleport_player(MAX_SIGHT * 2 + 5);
			break;
		}

		/* RF6_XXX3X6 */
		case 160+6:
		{
			break;
		}

		/* RF6_XXX4X6 */
		case 160+7:
		{
			break;
		}

		/* RF6_TELE_TO */
		case 160+8:
		{
			monster_type *m_ptr;
			monster_race *r_ptr;

			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			m_ptr = &m_list[cave[target_row][target_col].m_idx];
			r_ptr = &r_info[m_ptr->r_idx];
			monster_desc(t_name, m_ptr, 0);

			if (r_ptr->flags3 & (RF3_RES_TELE))
			{
				if (r_ptr->flags1 & (RF1_UNIQUE))
				{
				r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
				msg_format("%sには効果がなかった！", t_name);
#else
				msg_format("%s is unaffected!", t_name);
#endif
				break;
				}
				else if (r_ptr->level > randint1(100))
				{
				r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
				msg_format("%sには耐性がある！", t_name);
#else
				msg_format("%s resists!", t_name);
#endif
				break;
				}
			}
#ifdef JP
			msg_format("%sを引き戻した。", t_name);
#else
			msg_format("You command %s to return.", t_name);
#endif
			teleport_to_player(cave[target_row][target_col].m_idx, 100);
			break;
		}

		/* RF6_TELE_AWAY */
		case 160+9:
		{
			u16b flg = PROJECT_BEAM | PROJECT_KILL;
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)project_hook(GF_AWAY_ALL, dir, MAX_SIGHT * 2 + 5, flg);
			break;
		}

		/* RF6_TELE_LEVEL */
		case 160+10:
		{
			monster_type *m_ptr;
			monster_race *r_ptr;

			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_idx = cave[target_row][target_col].m_idx;
			m_ptr = &m_list[cave[target_row][target_col].m_idx];
			r_ptr = &r_info[m_ptr->r_idx];
			monster_desc(t_name, m_ptr, 0);

			if (p_ptr->inside_quest && (quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM))
			{
#ifdef JP
				msg_format("%sには効果がなかった！", t_name);
#else
				msg_format("%s is unaffected!", t_name);
#endif
				break;
			}

			if (r_ptr->flags3 & (RF3_RES_TELE))
			{
				if (r_ptr->flags1 & (RF1_UNIQUE))
				{
					r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
					msg_format("%sには効果がなかった！", t_name);
#else
					msg_format("%s is unaffected!", t_name);
#endif
					break;
				}
				else if (r_ptr->level > randint1(100))
				{
					r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
					msg_format("%sには耐性がある！", t_name);
#else
					msg_format("%s resists!", t_name);
#endif
					break;
				}
			}
			else if(ironman_hengband && (randint1(rlev*2) < r_ptr->level))
			{
#ifdef JP
				msg_format("%sには効果がなかった！", t_name);
#else
				msg_format("%s is unaffected!", t_name);
#endif
				break;
			}

			if (!dun_level ||
				((dun_level > MAX_DEPTH - 1) && !quest_number(dun_level) && (randint0(100) < 50)))
			{
#ifdef JP
				msg_format("%sは床を突き破って沈んでいく。", t_name);
#else
				msg_format("%^s sinks through the floor.", t_name);
#endif
			}
			else
			{
#ifdef JP
				msg_format("%sは天井を突き破って宙へ浮いていく。", t_name);
#else
				msg_format("%^s rises up through the ceiling.", t_name);
#endif
			}

			/* Check for quest completion */
			check_quest_completion(m_ptr);

			delete_monster_idx(t_idx);
			break;
		}

		/* RF6_XXX5 */
		case 160+11:
		{
			break;
		}

		/* RF6_DARKNESS */
		case 160+12:
		{
#ifdef JP
			msg_format("暗闇の中で手を振った。");
#else
			msg_format("Gestures in shadow.");
#endif
			(void)project(0, 3, py, px, 0, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL);
			unlite_room(py, px);
			break;
		}

		/* RF6_TRAPS */
		case 160+13:
		{
#ifdef JP
			msg_print("邪悪に微笑んだ。");
#else
			msg_print("Casts a spell and cackles evilly.");
#endif
			(void)trap_creation();
			break;
		}

		/* RF6_FORGET */
		case 160+14:
		{
			if (!target_set(TARGET_KILL)) return FALSE;
			if (!cave[target_row][target_col].m_idx) break;
			if (!player_has_los_bold(target_row, target_col)) break;

			t_ptr = &m_list[cave[target_row][target_col].m_idx];
			tr_ptr = &r_info[t_ptr->r_idx];
			monster_desc(t_name, t_ptr, 0);

#ifdef JP
			msg_format("%^sの記憶を消去しようとした。", t_name);
#else
			msg_format("Tries to blank %s%s mind.", t_name,
					  (streq(t_name, "it") ? "s" : "'s"));
#endif

			if (tr_ptr->flags3 & RF3_NO_CONF)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは惑わされなかった。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s disbelieves the feeble spell.", t_name);
#endif
			}
			else if (tr_ptr->level > randint1((rlev - 10) < 1 ? 1 : (rlev - 10)) + 10)
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは惑わされなかった。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s disbelieves the feeble spell.", t_name);
#endif
			}
			else
			{
#ifdef JP
				if (is_seen(t_ptr)) msg_format("%^sは混乱したようだ。", t_name);
#else
				if (is_seen(t_ptr)) msg_format("%^s seems confused.", t_name);
#endif
				t_ptr->confused += 12 + (byte)randint0(4);
			}

			wake_up = TRUE;
			break;
		}

		/* RF6_XXX6X6 */
		case 160+15:
		{
			break;
		}

		/* RF6_S_KIN */
		case 160+16:
		{
#ifdef JP
			msg_format("魔法で%sを召喚した。",
				((r_ptr->flags1 & RF1_UNIQUE) ? "手下" : "仲間"));
#else
			msg_format("Magically summons %s.",
				((r_ptr->flags1 & RF1_UNIQUE) ? "minions" : "kin"));
#endif
			sound(SOUND_SUMMON);

			summon_kin_type = r_ptr->d_char;

			for (k = 0; k < 6; k++)
			{
				count += summon_specific(0, py, px, rlev, SUMMON_KIN, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_CYBER */
		case 160+17:
		{
			int max_cyber = (dun_level / 50) + randint1(6);

#ifdef JP
			msg_print("サイバーデーモンを召喚した！");
#else
			msg_print("Magically summons Cyberdemons!");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < max_cyber; k++)
			{
				count += summon_specific(0, py, px, 100, SUMMON_CYBER, FALSE, friendly, pet);
			}

			break;
		}

		/* RF6_S_MONSTER */
		case 160+18:
		{
			int type = (friendly ? SUMMON_NO_UNIQUES : 0);

#ifdef JP
			msg_print("魔法で仲間を召喚した！");
#else
			msg_print("Magically summons help!");
#endif
			sound(SOUND_SUMMON);

			count += summon_specific(0, py, px, rlev, type, FALSE, friendly, pet);

			break;
		}

		/* RF6_S_MONSTERS */
		case 160+19:
		{
			int max = randint1(8);
			int type = (friendly ? SUMMON_NO_UNIQUES : 0);

#ifdef JP
			msg_print("魔法でモンスターを召喚した！");
#else
			msg_print("Magically summons monsters!");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < max; k++)
			{
				count += summon_specific(0, py, px, rlev, type, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_ANT */
		case 160+20:
		{
#ifdef JP
			msg_print("魔法でアリを召喚した。");
#else
			msg_print("Magically summons ants.");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 6; k++)
			{
				count += summon_specific(0, py, px, rlev, SUMMON_ANT, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_SPIDER */
		case 160+21:
		{
#ifdef JP
			msg_print("魔法でクモを召喚した。");
#else
			msg_print("Magically summons spiders.");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 6; k++)
			{
				count += summon_specific(0, py, px, rlev, SUMMON_SPIDER, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_HOUND */
		case 160+22:
		{
#ifdef JP
			msg_print("魔法でハウンドを召喚した。");
#else
			msg_print("Magically summons hounds.");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 6; k++)
			{
				count += summon_specific(0, py, px, rlev, SUMMON_HOUND, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_HYDRA */
		case 160+23:
		{
#ifdef JP
			msg_print("魔法でヒドラを召喚した。");
#else
			msg_print("Magically summons hydras.");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 6; k++)
			{
				count += summon_specific(0, py, px, rlev, SUMMON_HYDRA, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_ANGEL */
		case 160+24:
		{
#ifdef JP
			msg_print("魔法で天使を召喚した！");
#else
			msg_print("Magically summons an angel!");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(0, py, px, rlev, SUMMON_ANGEL, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_DEMON */
		case 160+25:
		{
#ifdef JP
			msg_print("魔法で地獄の強敵を召喚した！");
#else
			msg_print("Magically summons a hellish adversary!");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(0, py, px, rlev, SUMMON_DEMON, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_UNDEAD */
		case 160+26:
		{
#ifdef JP
			msg_print("魔法でアンデッドを召喚した。");
#else
			msg_print("Magically summons undead.");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(0, py, px, rlev, SUMMON_UNDEAD, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_DRAGON */
		case 160+27:
		{
#ifdef JP
			msg_print("魔法でドラゴンを召喚した！");
#else
			msg_print("Magically summons a dragon!");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(0, py, px, rlev, SUMMON_DRAGON, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_HI_UNDEAD */
		case 160+28:
		{
			int type = (friendly ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_HI_UNDEAD);

#ifdef JP
			msg_print("魔法でアンデッドを召喚した。");
#else
			msg_print("Magically summons undead.");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 8; k++)
			{
				count += summon_specific(0, py, px, rlev, type, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_HI_DRAGON */
		case 160+29:
		{
			int type = (friendly ? SUMMON_HI_DRAGON_NO_UNIQUES : SUMMON_HI_DRAGON);

#ifdef JP
			msg_print("魔法で古代ドラゴンを召喚した！");
#else
			msg_print("Magically summons ancient dragons!");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 8; k++)
			{
				count += summon_specific(0, py, px, rlev, type, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_HI_DEMON */
		case 160+30:
		{
#ifdef JP
			msg_print("魔法で地獄の強敵を召喚した！");
#else
			msg_print("Magically summons a hellish adversary!");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(0, py, px, rlev, SUMMON_HI_DEMON, TRUE, friendly, pet);
			}

			break;
		}

		/* RF6_S_UNIQUE */
		case 160+31:
		{
#ifdef JP
			msg_print("魔法で特別な強敵を召喚した！");
#else
			msg_print("Magically summons special opponents!");
#endif
			sound(SOUND_SUMMON);

			for (k = 0; k < 8; k++)
			{
				count += summon_specific(0, py, px, rlev, SUMMON_UNIQUE, TRUE, FALSE, FALSE);
			}

			break;
		}
	}

	if (wake_up)
	{
		t_ptr->csleep = 0;
		if (tr_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);
		p_ptr->redraw |= (PR_HEALTH);
	}

	if (fear && is_seen(t_ptr))
	{
#ifdef JP
		msg_format("%^sは恐怖して逃げ出した！", t_name);
#else
		msg_format("%^s flees in terror!", t_name);
#endif
	}

	return (TRUE);
}


int get_monster_spell(int *sn, byte num, byte *spell, bool only_browse)
{
	int i;
	int min_lev;
	int mana_cost;
	int fail;
	int chance;
	int ask;

	int ml;
	int pgmax;
	int page;

	char choice;
	char out_val[160];
	char ddesc[160];
	char comment[80];
	cptr sname;

	int old_spell_stat = mp_ptr->spell_stat;

	bool flag = FALSE;
	bool redraw = FALSE;

#ifdef JP
	cptr p = "呪文";
	cptr label[6] = { "腕力", "知能", "賢さ", "器用", "耐久", "魅力" };
#else
	cptr p = "spell";
	cptr label[6] = { " Str", " Int", " Wis", " Dex", " Con", " Chr" };
#endif

#ifdef ALLOW_REPEAT /* TNB */

	*sn = -1;

#endif /* ALLOW_REPEAT -- TNB */

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (*sn < num)
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	ml = INVEN_PACK - 1;
	pgmax = num / ml + 1;
	page = pgmax - 1;

	if (only_browse)
	{
#ifdef JP
		(void)strnfmt(out_val, 78, "(%^s %c-%c,%s'*'で一覧, ESC) どの%sについて知りますか？",
			p, I2A(0), I2A(num - 1), ((num > INVEN_PACK-1) ? " ' 'で次ページ, " : " "), p);
#else
		(void)strnfmt(out_val, 78, "(%^ss %c-%c,%s*=List, ESC=exit) Look which %s? ",
			p, I2A(0), I2A(num - 1), ((num > INVEN_PACK-1) ? " ' '=Next, " : " "), p);
#endif
	}
	else
	{
#ifdef JP
		(void)strnfmt(out_val, 78, "(%^s %c-%c,%s'*'で一覧, ESC) どの%sを使いますか？",
			p, I2A(0), I2A(num - 1), ((num > INVEN_PACK-1) ? " ' 'で次ページ, " : " "), p);
#else
		(void)strnfmt(out_val, 78, "(%^ss %c-%c,%s*=List, ESC=exit) Use which %s? ",
			p, I2A(0), I2A(num - 1), ((num > INVEN_PACK-1) ? " ' '=Next, " : " "), p);
#endif
	}

	/* Get a spell from the user */
	choice = always_show_list ? ESCAPE : 1;

	/* Save the screen */
	screen_save();

	while (!flag)
	{
		if(choice == ESCAPE) choice = ' ';
		else if(!get_com(out_val, &choice)) break; 

		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Restore the screen */
			screen_load();

			/* Save the screen */
			screen_save();

			page++;

			/* Show list */
			if (page == pgmax) page = 0;

			/* Display a list of spells */
			prt("", SY, SX);
#ifdef JP
			put_str("名前", SY, SX + 5);
			put_str("能力   MP 失率 効果", SY, SX + 35);
#else
			put_str("Name", SY, SX + 5);
			put_str("Stat Mana Fail Info", SY, SX + 35);
#endif

			/* Dump the spells */
			for (i = 0; i < MIN((num - ml * page), ml); i++)
			{
				int n = i + ml * page;

				/* Access the spell */
				sname = monst_spell[spell[n]].name;
				min_lev = monst_spell[spell[n]].min_lev;
				mana_cost = monster_cost(monst_spell[spell[n]].cost,
					monst_spell[spell[n]].linear);
				fail = monst_spell[spell[n]].fail;

				mp_ptr->spell_stat = monst_spell[spell[n]].stat;
				chance = get_spell_chance(fail, min_lev, mana_cost);

				/* Get info */
				monster_info(comment, spell[n]);

				/* Dump the spell --(-- */
				sprintf(ddesc, "  %c) %-30s%4s %4d %3d%%%s",
					I2A(i), sname, label[mp_ptr->spell_stat], mana_cost, chance, comment);

				prt(ddesc, SY + i + 1, SX);

				mp_ptr->spell_stat = old_spell_stat;
			}

			/* Clear the bottom line */
			prt("", SY + i + 1, SX);

			/* Redo asking */
			continue;
		}

		/* Note verify */
		ask = isupper(choice);

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		sname = monst_spell[spell[i + ml * page]].name;

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
#ifdef JP
			(void) strnfmt(tmp_val, 78, "%sを使いますか？", sname);
#else
			(void)strnfmt(tmp_val, 78, "Use %s? ", sname);
#endif

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Remember selection */
	*sn = i + ml * page;

	/* Restore the screen */
	screen_load();

	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}

	/* Abort if needed */
	if (!flag) return (FALSE);

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	return (TRUE);
}


/*
 * Monster tries to 'cast a spell' (or breath, etc)
 * at another monster.
 *
 * The player is only disturbed if able to be affected by the spell.
 */
bool do_cmd_monster_spell(void)
{
	int i, j, k;
	int sn;
	int mana_cost;
	int chance;
	int plev = p_ptr->lev;
	int old_csp = p_ptr->csp;
	bool cast;
	monster_spell *sp;

	byte spell[96];
	byte num = 0;

	monster_race *r_ptr;
	u32b f, f4, f5, f6;

	int old_spell_stat = mp_ptr->spell_stat;


	/* Paranoia */
	if (p_ptr->pclass != CLASS_SNATCHER) return (FALSE);

	r_ptr = &r_info[p_ptr->r_idx];

	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;

	if (!f4 && !f5 && !f6)
	{
#ifdef JP
		msg_print("この身体では呪文が使えない！");
#else
		msg_print("Can not cast any spell now!");
#endif
		return (FALSE);
	}

	/* not if confused */
	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱していて集中できない！");
#else
		msg_print("You are too confused!");
#endif
		return (FALSE);
	}

	/* Get monster spells */
	for (i = 0; i < 3; i++)
	{
		switch(i)
		{
		case 0: f = f4; break;
		case 1: f = f5; break;
		case 2: f = f6; break;
		}

		for (j = 0; j < 32; j++)
		{
			k = (1L << j);

			if (f & k) spell[num++] = 32 * i + j;
		}
	}

	/* Get monster spells */
	if (!get_monster_spell(&sn, num, spell, FALSE)) return (FALSE);

	sp = &monst_spell[spell[sn]];
	mana_cost = monster_cost(sp->cost, sp->linear);

	/* Verify "dangerous" spells */
	if (mana_cost > p_ptr->csp)
	{
		/* Warning */
#ifdef JP
		msg_print("ＭＰが足りません。");
#else
		msg_print("You do not have enough mana to use this power.");
#endif

		if (!over_exert) return (FALSE);

		/* Verify */
#ifdef JP
		if (!get_check("それでも挑戦しますか? ")) return (FALSE);
#else
		if (!get_check("Attempt it anyway? ")) return (FALSE);
#endif
	}

	/* Spell failure chance */
	mp_ptr->spell_stat = monst_spell[spell[sn]].stat;
	chance = get_spell_chance(sp->fail, sp->min_lev, mana_cost);
	mp_ptr->spell_stat = old_spell_stat;

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_format("精神の集中に失敗した！");
#else
		msg_format("You failed to concentrate hard enough!");
#endif
		sound(SOUND_FAIL);
	}
	else
	{
		sound(SOUND_ZAP);

		/* A spell was cast */
		cast = cast_monster_spell(96 + spell[sn]);

		if (!cast) return (FALSE);
	}

	/* Take a turn */
	energy_use = 100;

	/* Sufficient mana */
	if (mana_cost <= old_csp)
	{
		/* Use some mana */
		p_ptr->csp -= mana_cost;

		/* Limit */
		if (p_ptr->csp < 0) p_ptr->csp = 0;
	}

	/* Over-exert the player */
	else
	{
		int oops = mana_cost - old_csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
#ifdef JP
		msg_print("精神を集中しすぎて気を失ってしまった！");
#else
		msg_print("You faint from the effort!");
#endif

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint1(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (randint0(100) < 50)
		{
			bool perm = (randint0(100) < 25);

			/* Message */
#ifdef JP
			msg_print("体を悪くしてしまった！");
#else
			msg_print("You have damaged your health!");
#endif

			/* Reduce constitution */
			(void)dec_stat(A_CON, 15 + randint1(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);

	return (TRUE);
}
