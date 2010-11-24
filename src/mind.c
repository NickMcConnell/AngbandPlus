/* File: mind.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Purpose: Mindcrafter code */

#include "angband.h"
#include "mindtips.h"


mind_power mind_powers[MIND_MAX_CLASSES] =
{
  {
    {
      /* Level gained,  cost,  %fail,  name */
#ifdef JP
      { 1,   1,  15, "霊視"},
      { 2,   1,  20, "神経攻撃"},
      { 3,   2,  25, "次元の瞬き"},
      { 7,   6,  35, "虚空の幻影"},
      { 9,   7,  50, "精神支配"},
      { 11,  7,  30, "念動衝撃弾"},
      { 13, 12,  50, "鎧化"},
      { 15, 12,  60, "サイコメトリー"},
      { 18, 10,  45, "精神波動"},
      { 23, 15,  50, "アドレナリン・ドーピング"},
      { 26, 28,  60, "テレキネシス"},
      { 28, 10,  40, "サイキック・ドレイン"},
      { 35, 35,  75, "光の剣"},
      { 45,150,  85, "完全な世界"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
#else
      { 1,   1,  15, "Precognition"},
      { 2,   1,  20, "Neural Blast"},
      { 3,   2,  25, "Minor Displacement"},
      { 7,   6,  35, "Major Displacement"},
      { 9,   7,  50, "Domination"},
      { 11,  7,  30, "Pulverise"},
      { 13, 12,  50, "Character Armour"},
      { 15, 12,  60, "Psychometry" },
      { 18, 10,  45, "Mind Wave" },
      { 23, 15,  50, "Adrenaline Channeling"},
      { 26, 28,  60, "Telekinesis"},
      { 28, 10,  40, "Psychic Drain"},
      { 35, 35,  75, "Psycho-Spear"},
      { 45,150,  85, "The World"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
#endif
      
    }
  },
  
  {
    {
      /* Level gained,  cost,  %fail,  name */
#ifdef JP
      { 1,   1,  15, "小龍"},
      { 3,   3,  30, "閃光"},
      { 5,   6,  35, "舞空術"},
      { 8,   5,  40, "カメハメ波"},
      { 10,  7,  45, "対魔法防御"},
      { 13,  5,  60, "練気"},
      { 17, 17,  50, "纏闘気"},
      { 20, 20,  50, "衝波"},
      { 23, 18,  55, "彗龍"},
      { 25, 30,  70, "いてつく波動"},
      { 28, 26,  50, "幻霊召喚"},
      { 32, 35,  65, "煉獄火炎"},
      { 38, 42,  75, "超カメハメ波"},
      { 44, 50,  80, "光速移動"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
#else
      { 1,   1,  15, "Small Force Ball"},
      { 3,   3,  30, "Flash Light"},
      { 5,   6,  35, "Flying Technique"},
      { 8,   5,  40, "Kamehameha"},
      { 10,  7,  45, "Magic Resistance"},
      { 13,  5,  60, "Improve Force"},
      { 17, 17,  50, "Aura of Force"},
      { 20, 20,  50, "Shock Power"},
      { 23, 18,  55, "Large Force Ball"},
      { 25, 30,  70, "Dispel Magic"},
      { 28, 26,  50, "Summon Ghost"},
      { 32, 35,  65, "Exploding Frame"},
      { 38, 42,  75, "Super Kamehameha"},
      { 44, 50,  80, "Light Speed"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
#endif
      
    }
  },
  
  {
    {
      /* Level gained,  cost,  %fail,  name */
#ifdef JP
      {  8,  5,  40, "殺気感知"},
      { 15, 20,   0, "突撃"},
      { 20, 15,   0, "トラップ粉砕"},
      { 25, 20,  60, "地震"},
      { 30, 80,  75, "皆殺し"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
#else
      {  8,  5,  40, "Detect Atmosphere of Menace"},
      { 15, 20,   0, "Charge"},
      { 20, 15,   0, "Smash a Trap"},
      { 25, 20,  60, "Quake"},
      { 30, 80,  75, "Massacre"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
#endif
      
    }
  },

  {
    {
      /* Level gained,  cost,  %fail,  name */
#ifdef JP
      { 1,   1,  15, "真見の鏡"},
      { 1,   2,  40, "鏡生成"},
      { 2,   2,  20, "光のしずく"},
      { 3,   2,  20, "歪んだ鏡"},
      { 5,   3,  35, "閃光鏡"},
      { 6,   5,  35, "彷える鏡"},

      { 10,  5,  30, "微塵隠れ"},
      { 12, 12,  30, "追放の鏡"},
      { 15, 15,  30, "鏡砕き"},
      { 19, 13,  30, "催眠鏡"},
      { 23, 18,  50, "シーカーレイ"},

      { 25, 20,  40, "鏡の封印"},
      { 27, 30,  60, "水鏡の盾"},
      { 29, 30,  60, "スーパーレイ"},
      { 31, 35,  60, "幻惑の光"},
      { 33, 50,  80, "鏡の国"},

      { 36, 30,  80, "鏡抜け"},
      { 38, 40,  70, "帰還の鏡"},
      { 40, 50,  55, "影分身"},
      { 43, 55,  70, "封魔結界"},
      { 46, 70,  75, "ラフノールの鏡"},
#else
      { 1,   1,  15, "Mirror of Seeing"},
      { 1,   2,  40, "Making a Mirror"},
      { 2,   2,  20, "Drip of Light"},
      { 3,   2,  20, "Warped Mirror"},
      { 5,   3,  35, "Mirror of Light"},
      { 6,   5,  35, "Mirror of Wandering"},

      { 10,  5,  30, "Robe of Dust"},
      { 12, 12,  30, "Banishing Mirror"},
      { 15, 15,  30, "Mirror Clashing"},
      { 19, 13,  30, "Mirror Sleeping"},
      { 23, 18,  50, "Seeker Ray"},

      { 25, 20,  40, "Seal of Mirror"},
      { 27, 30,  60, "Shield of Water"},
      { 29, 30,  60, "Super Ray"},
      { 31, 35,  60, "Illusion Light"},
      { 33, 50,  80, "Mirror Shift"},

      { 36, 30,  80, "Mirror Tunnel"},
      { 38, 40,  70, "Mirror of Recall"},
      { 40, 50,  55, "Multi-Shadow"},
      { 43, 55,  70, "Binding Field"},
      { 46, 70,  75, "Mirror of Ruffnor"},
#endif
      
    }
  },
  
  {
    {
      /* Level gained,  cost,  %fail,  name */
#ifdef JP
      {  1,  1,  20, "暗闇生成"},
      {  2,  2,  25, "周辺調査"},
      {  3,  3,  25, "葉隠れ"},
      {  5,  3,  30, "変わり身"},
      {  7,  8,  35, "高飛び"},
      {  8, 10,  35, "一撃離脱"},
      { 10, 10,  40, "金縛り"},
      { 12, 12,  70, "古の口伝"},
      { 15, 10,  50, "浮雲"},
      { 17, 12,  45, "火遁"},
      { 18, 20,  40, "入身"},
      { 20,  5,  50, "八方手裏剣"},
      { 22, 15,  55, "鎖鎌"},
      { 25, 32,  60, "煙玉"},
      { 28, 32,  60, "転身"},
      { 30, 30,  70, "爆発の紋章"},
      { 32, 40,  40, "土遁"},
      { 34, 35,  50, "霧隠れ"},
      { 38, 40,  60, "煉獄火炎"},
      { 41, 50,  55, "分身"},
      { 99,  0,   0, ""},
#else
      {  1,  1,  20, "Create Darkness"},
      {  2,  2,  25, "Detect Near"},
      {  3,  3,  25, "Hide in Leafs"},
      {  5,  3,  30, "Kawarimi"},
      {  7,  8,  35, "Absconding"},
      {  8, 10,  35, "Hit and Away"},
      { 10, 10,  40, "Bind Monster"},
      { 12, 12,  70, "Ancient Knowledge"},
      { 15, 10,  50, "Floating"},
      { 17, 12,  45, "Hide in Flame"},
      { 18, 20,  40, "Nyusin"},
      { 20,  5,  50, "Syuriken Spreading"},
      { 22, 15,  55, "Chain Hook"},
      { 25, 32,  60, "Smoke Ball"},
      { 28, 32,  60, "Swap Position"},
      { 30, 30,  70, "Glyph of Explosion"},
      { 32, 40,  40, "Hide in Mud"},
      { 34, 35,  50, "Hide in Mist"},
      { 38, 40,  60, "Rengoku-Kaen"},
      { 41, 50,  55, "Bunshin"},
      { 99,  0,   0, ""},
#endif
      
    }
  },
  {
    {
    /* Level gained,  cost,  %fail,  name */
      {  1,  5,  30, "Time Spurt"},
      {  5,  4,  30, "Decay Door"},
      { 10, 10,  50, "Decay Wall"},
	  { 13, 10,  50, "Devolution"},
	  { 15, 10,  50, "Evolution"},
      { 17, 15,  50, "Slow Monster"},
      { 20, 15,  50, "Back to Origins"},
      { 23, 15,  60, "Haste Self"},
	  { 23, 15,  60, "Haste Monster"},
      { 27, 20,  50, "Mass Slow"},
      { 30, 20,  50, "Temporal Prison"},
      { 35, 90,  70, "Rewind Time"},
      { 40, 80,  70, "Remembrance"},
      { 45, 60,  60, "Speed Essentia"},
      { 50,150,  65, "Stop Time"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	}
  },
  {
    {
    /* Level gained,  cost,  %fail,  name */
      {  1,  1,  20, "Blood Flow"},
      {  5,  5,  30, "Blood Sight"},
	  { 10, 10,  30, "Blood Spray"},
	  { 15, 20,  30, "Blood Bath"},
      { 20, 30,  30, "Blood Shield"},
      { 25, 50,  40, "Blood Seeking"},
      { 30, 60,  40, "Blood Rage"},
      { 40,100,  50, "Blood Feast"},
	  { 42,100,   0, "Blood Revenge"},
	  { 45,500,  90, "Blood Pool"},
      { 50,500,  70, "Blood Explosion"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	  { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	}
  },
  { /* Warlock: Undead */
    {
    /* Level gained,  cost,  %fail,  name */
      {  1,  0,  20, "Basic"},
      { 10,  0,  40, "Extended"},
	  { 18,  0,  45, "Spear"},
	  { 26,  0,  60, "Burst"},
      { 33,  0,  60, "Stunning"},
	  { 40,  0,  70, "Draining"},
      { 45,  0,  75, "Empowered"},
	  { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	  { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	}
  },
  { /* Warlock: Dragon */
    {
    /* Level gained,  cost,  %fail,  name */
      {  1,  0,  20, "Basic"},
      { 10,  0,  40, "Extended"},
	  { 18,  0,  45, "Spear"},
	  { 26,  0,  60, "Burst"},
      { 33,  0,  60, "Stunning"},
	  { 40,  0,  70, "Prismatic"},
      { 45,  0,  75, "Empowered"},
	  { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	  { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	}
  },
  { /* Warlock: Angel */
    {
    /* Level gained,  cost,  %fail,  name */
      {  1,  0,  20, "Basic"},
      { 10,  0,  40, "Extended"},
	  { 18,  0,  45, "Spear"},
	  { 26,  0,  60, "Burst"},
      { 33,  0,  60, "Stunning"},
	  { 40,  0,  70, "Dispelling"},
      { 45,  0,  75, "Empowered"},
	  { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	  { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	}
  },
  { /* Warlock: Demon */
    {
    /* Level gained,  cost,  %fail,  name */
      {  1,  0,  20, "Basic"},
      { 10,  0,  40, "Extended"},
	  { 18,  0,  45, "Spear"},
	  { 26,  0,  60, "Burst"},
      { 33,  0,  60, "Stunning"},
	  { 40,  0,  70, "Vengeful"},
      { 45,  0,  75, "Empowered"},
	  { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	  { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	}
  },
  { /* Warlock: Aberration */
    {
    /* Level gained,  cost,  %fail,  name */
      {  1,  0,  20, "Basic"},
      { 10,  0,  40, "Extended"},
	  { 18,  0,  45, "Spear"},
	  { 26,  0,  60, "Burst"},
      { 33,  0,  60, "Stunning"},
	  { 40,  0,  70, "Confusing"},
      { 45,  0,  75, "Empowered"},
	  { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	  { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
	}
  },
};

static int warlock_range(void)
{
	int rng = 5;

	if (p_ptr->lev > 47)
		rng = 8;
	else if (p_ptr->lev > 31)
		rng = 7;
	else if (p_ptr->lev > 15)
		rng = 6;

	return rng; 
}

static int warlock_dice(void)
{
	return 1 + (p_ptr->lev/5) + (p_ptr->lev * p_ptr->lev * 3/500);
}

static int warlock_sides(void)
{
	return warlock_damage_sides[p_ptr->stat_ind[A_CHR]];
}

void mindcraft_info(char *p, int use_mind, int power)
{
#ifdef JP
	cptr s_dam = "損傷:";
	cptr s_dur = "期間:";
	cptr s_range = "範囲:";
#else
	cptr s_dam = "dam ";
	cptr s_dur = "dur ";
	cptr s_range = "rng ";
#endif
  int plev = p_ptr->lev;

  strcpy(p, "");

  switch (use_mind)
    {
    case MIND_MINDCRAFTER:
      switch (power)
	{
	case 0:  break;
	case 1:  sprintf(p, " %s%dd%d", s_dam, spell_power(3 + ((plev - 1) / 4)), 3 + plev/15); break;
	case 2:  sprintf(p, " %s10", s_range); break;
	case 3:  sprintf(p, " %s%d", s_range, plev * 5);  break;
	case 4:  break;
	case 5: sprintf(p, " %s%dd8", s_dam, spell_power(8 + ((plev - 5) / 4)));  break;
	case 6:  sprintf(p, " %s%d", s_dur, plev);  break;
	case 7:  break;
	case 8:  sprintf(p, (plev < 25 ? " %s%d" : " %sd%d"), s_dam, spell_power((plev < 25 ? plev * 3 / 2 : plev * ((plev - 5) / 10 + 1)))); break;
	case 9:  sprintf(p, " %s10+d%d", s_dur, plev * 3 / 2);  break;
#ifdef JP
	case 10: sprintf(p, " 最大重量:%d.%dkg", lbtokg1(plev * 15),lbtokg2(plev * 15));  break;
#else
	case 10: sprintf(p, " max wgt %d", plev * 15);  break;
#endif
	case 11: sprintf(p, " %s%dd6", s_dam, spell_power(plev / 2));  break;
	case 12: sprintf(p, " %sd%d+%d", s_dam, spell_power(plev * 3), spell_power(plev * 3)); break;
#ifdef JP
	case 13: sprintf(p, " 行動:%ld回", (p_ptr->csp + 100-p_ptr->energy_need - 50)/100); break;
#else
	case 13: sprintf(p, " %ld acts.", (p_ptr->csp + 100-p_ptr->energy_need - 50)/100); break;
#endif
	}
      break;
    case MIND_KI:
      {
	int boost = p_ptr->magic_num1[0];

	if (heavy_armor()) boost /= 2;

	switch (power)
	  {
	  case 0:  sprintf(p, " %s%dd4", s_dam, 3 + ((plev - 1) / 5) + boost / 12); break;
	  case 1:  break;
	  case 2:  sprintf(p, " %s%d+d30", s_dur, 30 + boost / 5); break;
	  case 3:  sprintf(p, " %s%dd5", s_dam, 5 + ((plev - 1) / 5) + boost / 10); break;
	  case 4:  sprintf(p, " %s%d+d20", s_dur, 20 + boost / 5); break;
	  case 5:  break;
	  case 6:  sprintf(p, " %s%d+d%d", s_dur, 15 + boost / 7, plev / 2); break;
	  case 7:  sprintf(p, " %s%dd8", s_dam, 8 + ((plev - 5) / 5) + boost / 12); break;
	  case 8:  sprintf(p, " %s10d6+%d", s_dam, plev * 3 / 2 + boost * 3 / 5); break;
	  case 9:  break;
#ifdef JP
	  case 10: sprintf(p, " 最大%d体", 1+boost/100); break;
#else
	  case 10: sprintf(p, " max %d", 1+boost/100); break;
#endif
	  case 11: sprintf(p, " %s%d", s_dam, 100 + plev + boost); break;
	  case 12: sprintf(p, " %s%dd15", s_dam, 10 + plev / 2 + boost * 3 / 10); break;
#ifdef JP
	  case 13: sprintf(p, " 行動:%d+d16回", 16+boost/20); break;
#else
	  case 13: sprintf(p, " %d+d16 acts", 16+boost/20); break;
#endif
	  }
	break;
			case MIND_MIRROR_MASTER:
			{
				switch (power)
				{
				case 0:  break;
				case 1:  break;
				case 2:  sprintf(p, " %s%dd4", s_dam,  spell_power(3 + ((plev - 1) / 5)) ); break;
				case 3:  sprintf(p, " %s10", s_range); break;
				case 4:  break;
				case 5:  sprintf(p, " %s%d", s_range, plev *5); break;
				case 6:  sprintf(p, " %s20+d20", s_dur);  break;
				case 7:  break;
				case 8:  sprintf(p, " %s%dd8", s_dam, spell_power(8+((plev -5)/4)) ); break;
				case 9:  break;
				case 10: sprintf(p, " %s%dd8", s_dam, spell_power(11+(plev-5)/4) ); break;
				case 11: break;
				case 12: sprintf(p, " %s20+d20", s_dur);  break;
				case 13: sprintf(p, " %s150+d%d", s_dam, spell_power(plev*2) ); break;
				case 14: break;
				case 15: break;
				case 16: sprintf(p, " %s%d", s_range, plev/2 +10); break;
				case 17: break;
				case 18: sprintf(p, " %s6+d6", s_dur);  break;
				case 19: sprintf(p, " %s%d", s_dam, spell_power(plev*11+5) ); break;
				case 20: sprintf(p, " %s4+d4", s_dur);  break;
				}
				break;
			}
			case MIND_NINJUTSU:
			{
				switch (power)
				{
				case 0:  break;
				case 1:  break;
				case 2:  sprintf(p, " %s10", s_range); break;
				case 3:  break;
				case 4:  sprintf(p, " %s%d", s_range , plev *5); break;
				case 5:  sprintf(p, " %s30", s_range); break;
				case 6:  break;
				case 7:  break;
				case 8:  sprintf(p, " %s20+d20", s_dur);  break;
				case 9:  sprintf(p, " %s%d", s_dam, (50+plev)/2 ); break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
				case 15: break;
				case 16: sprintf(p, " %s%d+d%d", s_dur, plev/2, plev/2);  break;
				case 17: sprintf(p, " %s%d*3", s_dam, (75+plev*2/3)/2 ); break;
				case 18: sprintf(p, " %s%dd10", s_dam, 6+plev/8 ); break;
				case 19: sprintf(p, " %s6+d6", s_dur);  break;
				}
				break;
			}
			case MIND_TIME_LORD:
			{
				switch (power)
				{
				case 14: sprintf(p, " %ld acts.", (p_ptr->csp + 100-p_ptr->energy_need - 50)/80); break;
				}
				break;
			}
			case MIND_BLOOD_KNIGHT:
			{
				switch (power)
				{
				case 2: sprintf(p, " %s%d+3d5", s_dam, plev + plev/4); break;
				case 4: sprintf(p, " %s30+1d20", s_dur); break;
				case 5: sprintf(p, " %s30+1d30", s_dur); break;
				case 6: sprintf(p, " %s%d+d%d", s_dur, plev, plev); break;
				case 8: sprintf(p, " %s25+1d25", s_dur); break;
				case 9: sprintf(p, " %s%d+d%d", s_dur, 5, 5); break;
				case 11: sprintf(p, " %s500", s_dam); break;
				}
				break;
			}
			case MIND_WARLOCK_UNDEAD:
			case MIND_WARLOCK_DRAGON:
			case MIND_WARLOCK_ANGEL:
			case MIND_WARLOCK_DEMON:
			case MIND_WARLOCK_ABERRATION:
			{
			int rng, dice, sides;
				dice = warlock_dice();
				sides = spell_power(warlock_sides());
				rng = warlock_range();
				switch (power)
				{
				case 0: sprintf(p, " %s%dd%d (%s%d)", s_dam, dice, sides, s_range, rng); break;
				case 1: sprintf(p, " %s%dd%d (%s%d)", s_dam, dice, sides, s_range, rng + 10 * plev/50); break;
				case 2: sprintf(p, " %s%dd%d (%s%d)", s_dam, dice, sides, s_range, rng); break;
				case 3: sprintf(p, " %s%dd%d (%s%d)", s_dam, dice, sides, s_range, rng); break;
				case 4: sprintf(p, " %s%dd%d (%s%d)", s_dam, dice, sides, s_range, rng); break;
				case 5:
					switch (p_ptr->psubclass)
					{
					case PACT_DEMON:
						sprintf(p, " %s%dd%d*2 (%s%d)", s_dam, dice, sides, s_range, rng); break;
					case PACT_DRAGON:
						sprintf(p, " %s%dd%d*5/2 (%s%d)", s_dam, dice, sides, s_range, rng); break;
					default: sprintf(p, " %s%dd%d (%s%d)", s_dam, dice, sides, s_range, rng); break;
					}
					break;
				case 6: sprintf(p, " %s%dd%d*1.5 (%s%d)", s_dam, dice, sides, s_range, rng); break;
				case 7: sprintf(p, " %s%dd%d*2 (%s%d)", s_dam, dice, sides - 2, s_range, rng); break;
				}
				break;
			}
		}
	}
}

/*
 * Allow user to choose a mindcrafter power.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 *
 * nb: This function has a (trivial) display bug which will be obvious
 * when you run it. It's probably easy to fix but I haven't tried,
 * sorry.
 */
static int get_mind_power(int *sn, bool only_browse)
{
	int             i;
	int             num = 0;
	int             y = 1;
	int             x = 10;
	int             minfail = 0;
	int             plev = p_ptr->lev;
	int             chance = 0;
	int             ask = TRUE;
	char            choice;
	char            out_val[160];
	char            comment[80];
	cptr            p;

	mind_type       spell;
	mind_power      *mind_ptr;
	bool            flag, redraw;
	int             use_mind;
	int menu_line = (use_menu ? 1 : 0);

    switch(p_ptr->pclass)
	{
	case CLASS_MINDCRAFTER:
	  {
	    use_mind = MIND_MINDCRAFTER;
#ifdef JP
	    p = "超能力";
#else
	    p = "mindcraft";
#endif
	    break;
	  }
	case CLASS_FORCETRAINER:
	  {
	    use_mind = MIND_KI;
#ifdef JP
	    p = "練気術";
#else
	    p = "Force";
#endif
	    break;
	  }
	case CLASS_BERSERKER:
	  {
	    use_mind = MIND_BERSERKER;
#ifdef JP
	    p = "技";
#else
	    p = "brutal power";
#endif
	    break;
	  }
	case CLASS_MIRROR_MASTER:
	  {
	    use_mind = MIND_MIRROR_MASTER;
#ifdef JP
	    p = "鏡魔法";
#else
	    p = "magic";
#endif
	    break;
	  }
	case CLASS_NINJA:
	  {
	    use_mind = MIND_NINJUTSU;
#ifdef JP
	    p = "忍術";
#else
	    p = "ninjutsu";
#endif
	    break;
	  }

	case CLASS_TIME_LORD:
      {
        use_mind = MIND_TIME_LORD;
        p = "timecraft";
        break;
	  }
	case CLASS_BLOOD_KNIGHT:
      {
        use_mind = MIND_BLOOD_KNIGHT;
        p = "bloodcraft";
        break;
	  }
	case CLASS_WARLOCK:
      {
		switch (p_ptr->psubclass)
		{
		case PACT_UNDEAD: use_mind = MIND_WARLOCK_UNDEAD; break;
		case PACT_DRAGON: use_mind = MIND_WARLOCK_DRAGON; break;
		case PACT_ANGEL: use_mind = MIND_WARLOCK_ANGEL; break;
		case PACT_DEMON: use_mind = MIND_WARLOCK_DEMON; break;
		case PACT_ABERRATION: use_mind = MIND_WARLOCK_ABERRATION; break;
		}
        p = "eldritch blast";
        break;
	  }
	default:
	  {
	    use_mind = 0;
#ifdef JP
	    p = "超能力";
#else
	    p = "mindcraft";
#endif
	    break;
	  }
	}
      mind_ptr = &mind_powers[use_mind];

	/* Assume cancelled */
      *sn = (-1);

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Hack -- If requested INVEN_FORCE(1111), pull again */
		if (*sn == INVEN_FORCE) repeat_pull(sn);

		/* Verify the spell */
		if (mind_ptr->info[*sn].min_lev <= plev)
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

      /* Nothing chosen yet */
      flag = FALSE;

      /* No redraw yet */
      redraw = FALSE;

    for (i = 0; i < MAX_MIND_POWERS; i++)
	{
	  if (mind_ptr->info[i].min_lev <= plev)
	    {
	      num++;
	    }
	}

      /* Build a prompt (accept all spells) */
    if (only_browse)
	{
#ifdef JP
	  (void) strnfmt(out_val, 78, "(%^s %c-%c, '*'で一覧, ESC) どの%sについて知りますか？",
#else
	  (void) strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
#endif
				       p, I2A(0), I2A(num - 1), p);
	  }
	else
	  {
#ifdef JP
(void) strnfmt(out_val, 78, "(%^s %c-%c, '*'で一覧, ESC) どの%sを使いますか？",
#else
		(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
#endif
		p, I2A(0), I2A(num - 1), p);
	}

	if (use_menu && !only_browse) screen_save();
	/* Get a spell from the user */

	choice= (always_show_list || use_menu) ? ESCAPE:1 ;
	while (!flag)
	{
		if(choice==ESCAPE) choice = ' '; 
		else if( !get_com(out_val, &choice, TRUE) )break;

		if (use_menu && choice != ' ')
		{
			switch(choice)
			{
				case '0':
				{
					if (!only_browse) screen_load();
					return (FALSE);
				}

				case '8':
				case 'k':
				case 'K':
				{
					menu_line += (num - 1);
					break;
				}

				case '2':
				case 'j':
				case 'J':
				{
					menu_line++;
					break;
				}

				case 'x':
				case 'X':
				case '\r':
				case '\n':
				{
					i = menu_line - 1;
					ask = FALSE;
					break;
				}
			}
			if (menu_line > num) menu_line -= num;
		}
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?') || (use_menu && ask))
		{
			/* Show the list */
			if (!redraw || use_menu)
			{
				char psi_desc[80];
				bool has_weapon[2];

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				if (!only_browse && !use_menu) screen_save();

				/* Display a list of spells */
				prt("", y, x);
#ifdef JP
put_str("名前", y, x + 5);
#else
				put_str("Name", y, x + 5);
#endif

#ifdef JP
put_str(format("Lv   %s   失率 効果", ((use_mind == MIND_BERSERKER) || (use_mind == MIND_NINJUTSU)) ? "HP" : "MP"), y, x + 35);
#else
put_str(format("Lv   %s   Fail Info", ((use_mind == MIND_BERSERKER) || (use_mind == MIND_NINJUTSU)) ? "HP" : "MP"), y, x + 35);
#endif
				has_weapon[0] = buki_motteruka(INVEN_RARM);
				has_weapon[1] = buki_motteruka(INVEN_LARM);

				/* Dump the spells */
				for (i = 0; i < MAX_MIND_POWERS; i++)
				{
					int mana_cost;

					/* Access the spell */
					spell = mind_ptr->info[i];

					if (spell.min_lev > plev)   break;

					chance = spell.fail;

					mana_cost = spell.mana_cost;
					if (chance)
					{

						/* Reduce failure rate by "effective" level adjustment */
						chance -= 3 * (plev - spell.min_lev);

						/* Reduce failure rate by INT/WIS adjustment */
						chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

						if (use_mind == MIND_KI)
						{
							if (heavy_armor()) chance += 20;
							if (p_ptr->weapon_info[0].icky_wield) chance += 20;
							else if (has_weapon[0]) chance += 10;
							if (p_ptr->weapon_info[1].icky_wield) chance += 20;
							else if (has_weapon[1]) chance += 10;
							if (i == 5)
							{
								int j;
								for (j = 0; j < p_ptr->magic_num1[0] / 50; j++)
									mana_cost += (j+1) * 3 / 2;
							}
						}

						/* Not enough mana to cast */
						if ((use_mind != MIND_BERSERKER) && 
						    (use_mind != MIND_NINJUTSU) && 
						    (use_mind != MIND_BLOOD_KNIGHT) && 
							(use_mind != MIND_WARLOCK_UNDEAD) && 
							(use_mind != MIND_WARLOCK_DRAGON) && 
							(use_mind != MIND_WARLOCK_ANGEL) && 
							(use_mind != MIND_WARLOCK_DEMON) && 
							(use_mind != MIND_WARLOCK_ABERRATION) && 
							(mana_cost > p_ptr->csp))
						{
							chance += 5 * (mana_cost - p_ptr->csp);
						}

						chance += p_ptr->to_m_chance;

						/* Extract the minimum failure rate */
						minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

						/* Minimum failure rate */
						if (chance < minfail) chance = minfail;

						/* Stunning makes spells harder */
						if (p_ptr->stun > 50) chance += 25;
						else if (p_ptr->stun) chance += 15;

						if (use_mind == MIND_KI)
						{
							if (heavy_armor()) chance += 5;
							if (p_ptr->weapon_info[0].icky_wield) chance += 5;
							if (p_ptr->weapon_info[1].icky_wield) chance += 5;
						}
						/* Always a 5 percent chance of working */
						if (chance > 95) chance = 95;
					}

					/* Get info */
					mindcraft_info(comment, use_mind, i);

					if (use_menu)
					{
#ifdef JP
						if (i == (menu_line-1)) strcpy(psi_desc, "  》 ");
#else
						if (i == (menu_line-1)) strcpy(psi_desc, "  >  ");
#endif
						else strcpy(psi_desc, "     ");
					}
					else
						sprintf(psi_desc, "  %c) ",I2A(i));

					/* Dump the spell --(-- */
					strcat(psi_desc,
					       format("%-30s%2d %4d%s %3d%%%s",
						      spell.name, spell.min_lev, mana_cost,
#ifdef JP
						      (((use_mind == MIND_MINDCRAFTER) && (i == 13)) ? "〜" : "  "), 
#else
						      (((use_mind == MIND_MINDCRAFTER) && (i == 13)) ? "~ " : "  "), 
#endif
						      chance, comment));
					prt(psi_desc, y + i + 1, x);
				}

				/* Clear the bottom line */
				prt("", y + i + 1, x);
			}

			/* Hide the list */
			else if (!only_browse)
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
			}

			/* Redo asking */
			continue;
		}

		if (!use_menu)
		{
			/* Note verify */
			ask = isupper(choice);

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		spell = mind_ptr->info[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
#ifdef JP
(void) strnfmt(tmp_val, 78, "%sを使いますか？", spell.name);
#else
			(void)strnfmt(tmp_val, 78, "Use %s? ", spell.name);
#endif


			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw && !only_browse) screen_load();

	/* Show choices */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = i;

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
static bool cast_mindcrafter_spell(int spell)
{
	int             b = 0;
	int             dir;
	int             plev = p_ptr->lev;

	/* spell code */
	switch (spell)
	{
	case 0:   /* Precog */
		if (plev > 44)
		{
			chg_virtue(V_KNOWLEDGE, 1);
			chg_virtue(V_ENLIGHTEN, 1);
			wiz_lite(FALSE);
		}
		else if (plev > 19)
			map_area(DETECT_RAD_MAP);

		if (plev < 30)
		{
			b = detect_monsters_normal(DETECT_RAD_DEFAULT);
			if (plev > 14) b |= detect_monsters_invis(DETECT_RAD_DEFAULT);
			if (plev > 4)  {
				b |= detect_traps(DETECT_RAD_DEFAULT, TRUE);
				b |= detect_doors(DETECT_RAD_DEFAULT);
			}
		}
		else
		{
			b = detect_all(DETECT_RAD_DEFAULT);
		}

		if ((plev > 24) && (plev < 40))
			set_tim_esp(plev, FALSE);

#ifdef JP
if (!b) msg_print("安全な気がする。");
#else
		if (!b) msg_print("You feel safe.");
#endif

		break;
	case 1:
		/* Mindblast */
		if (!get_aim_dir(&dir)) return FALSE;

		if (randint1(100) < plev * 2)
			fire_beam(GF_PSI, dir, spell_power(damroll(3 + ((plev - 1) / 4), (3 + plev / 15))));
		else
			fire_ball(GF_PSI, dir, spell_power(damroll(3 + ((plev - 1) / 4), (3 + plev / 15))), 0);
		break;
	case 2:
		/* Minor displace */
		teleport_player(10, 0L);
		break;
	case 3:
		/* Major displace */
		teleport_player(plev * 5, 0L);
		break;
	case 4:
		/* Domination */
		if (plev < 30)
		{
			if (!get_aim_dir(&dir)) return FALSE;

			fire_ball(GF_DOMINATION, dir, spell_power(plev), 0);
		}
		else
		{
			charm_monsters(spell_power(plev * 2));
		}
		break;
	case 5:
		/* Fist of Force  ---  not 'true' TK  */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball(GF_TELEKINESIS, dir, spell_power(damroll(8 + ((plev - 5) / 4), 8)),
			(plev > 20 ? spell_power((plev - 20) / 8 + 1) : 0));
		break;
	case 6:
		/* Character Armour */
		set_shield(spell_power(plev), FALSE);
		if (plev > 14) set_oppose_acid(spell_power(plev), FALSE);
		if (plev > 19) set_oppose_fire(spell_power(plev), FALSE);
		if (plev > 24) set_oppose_cold(spell_power(plev), FALSE);
		if (plev > 29) set_oppose_elec(spell_power(plev), FALSE);
		if (plev > 34) set_oppose_pois(spell_power(plev), FALSE);
		break;
	case 7:
		/* Psychometry */
		if (plev < 25)
			return psychometry();
		else
			return ident_spell(FALSE);
	case 8:
		/* Mindwave */
#ifdef JP
msg_print("精神を捻じ曲げる波動を発生させた！");
#else
		msg_print("Mind-warping forces emanate from your brain!");
#endif

		if (plev < 25)
			project(0, 2 + plev / 10, py, px,
			spell_power(plev * 3), GF_PSI, PROJECT_KILL, -1);
		else
			(void)mindblast_monsters(spell_power(randint1(plev * ((plev - 5) / 10 + 1))));
		break;
	case 9:
		/* Adrenaline */
		set_afraid(0, TRUE);
		set_stun(0, TRUE);

		/*
		 * Only heal when Adrenalin Channeling is not active. We check
		 * that by checking if the player isn't fast and 'heroed' atm.
		 */
		if (!IS_FAST() || !IS_HERO())
		{
			hp_player(plev);
		}

		b = spell_power(10 + randint1((plev * 3) / 2));
		set_hero(b, FALSE);
		/* Haste */
		(void)set_fast(b, FALSE);
		break;
	case 10:
		/* Telekinesis */
		if (!get_aim_dir(&dir)) return FALSE;

		fetch(dir, plev * 15, FALSE);

		break;
	case 11:
		/* Psychic Drain */
		if (!get_aim_dir(&dir)) return FALSE;

		b = spell_power(damroll(plev / 2, 6));

		/* This is always a radius-0 ball now */
		if (fire_ball(GF_PSI_DRAIN, dir, b, 0))
			p_ptr->energy_need += randint1(150);
		break;
	case 12:
		/* psycho-spear */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_beam(GF_PSY_SPEAR, dir, spell_power(randint1(plev*3)+plev*3));
		break;
	case 13:
	{
		if (world_player)
		{
#ifdef JP
			msg_print("既に時は止まっている。");
#else
			msg_print("Time is already stopped.");
#endif
			return (FALSE);
		}
		world_player = TRUE;
#ifdef JP
		msg_print("「時よ！」");
#else
		msg_print("You yell 'Time!'");
#endif
		msg_print(NULL);

		/* Hack */
		p_ptr->energy_need -= 1000 + (100 + p_ptr->csp - 50)*TURNS_PER_TICK/10;

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Update monsters */
		p_ptr->update |= (PU_MONSTERS);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

		handle_stuff();
		break;
	}
	default:
#ifdef JP
msg_print("なに？");
#else
		msg_print("Zap?");
#endif

	}

	return TRUE;
}

/* Finding what monster to evolve into is trivial, since the monster_race type
   keeps a pointer in that direction.  However, we would like to reverse evolution
   turning harder monsters into easier ones.  This fn will scan the monster race
   table looking for a monster that evolves into this one.  Of course, we assume
   there is at most one such race to be found.
   Returns 0 if no such race can be found.
*/
static int find_evolution_idx(int r_idx)
{
	monster_race *r_ptr;

	if (r_idx <= 0) return 0;
	r_ptr = &r_info[r_idx];
	return r_ptr->next_r_idx;
}

static int find_devolution_idx(int r_idx)
{
	int i;

	if (r_idx <= 0) return 0;

	for (i = 1; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];
		if (r_ptr->next_r_idx == r_idx)
			return i;
	}

	return 0;
}

/*	Evolve or Devolve a Monster.  I spiked this from monster_gain_exp() in melee2.c without
	any great understanding on my part.
*/
static void change_monster_race(int m_idx, int new_r_idx)
{
	char m_name[80];
	int old_hp, old_maxhp, old_r_idx;
	byte old_sub_align;
	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Paranoia */
	if (m_idx <= 0 || new_r_idx <= 0) return;

	/* Get the monster and remember some stuff */
	m_ptr = &m_list[m_idx];
	old_hp = m_ptr->hp;
	old_maxhp = m_ptr->max_maxhp;
	old_r_idx = m_ptr->r_idx;
	old_sub_align = m_ptr->sub_align;

	/* Hack -- Reduce the racial counter of previous monster */
	real_r_ptr(m_ptr)->cur_num--;

	monster_desc(m_name, m_ptr, 0);
	m_ptr->r_idx = new_r_idx;

	/* Count the monsters on the level */
	real_r_ptr(m_ptr)->cur_num++;

	m_ptr->ap_r_idx = m_ptr->r_idx;
	r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags1 & RF1_FORCE_MAXHP)
	{
		m_ptr->max_maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
	}
	else
	{
		m_ptr->max_maxhp = damroll(r_ptr->hdice, r_ptr->hside);
	}
	if (ironman_nightmare)
	{
		u32b hp = m_ptr->max_maxhp * 2L;

		m_ptr->max_maxhp = (s16b)MIN(30000, hp);
	}
	m_ptr->maxhp = m_ptr->max_maxhp;
	m_ptr->hp = old_hp * m_ptr->maxhp / old_maxhp;

	/* Extract the monster base speed */
	m_ptr->mspeed = get_mspeed(r_ptr);

	/* Sub-alignment of a monster */
	if (!is_pet(m_ptr) && !(r_ptr->flags3 & (RF3_EVIL | RF3_GOOD)))
		m_ptr->sub_align = old_sub_align;
	else
	{
		m_ptr->sub_align = SUB_ALIGN_NEUTRAL;
		if (r_ptr->flags3 & RF3_EVIL) m_ptr->sub_align |= SUB_ALIGN_EVIL;
		if (r_ptr->flags3 & RF3_GOOD) m_ptr->sub_align |= SUB_ALIGN_GOOD;
	}

	m_ptr->exp = 0;

	if (is_pet(m_ptr) || m_ptr->ml)
	{
		if (!ignore_unview || player_can_see_bold(m_ptr->fy, m_ptr->fx))
		{
			if (p_ptr->image)
			{
				monster_race *hallu_race;

				do
				{
					hallu_race = &r_info[randint1(max_r_idx - 1)];
				}
				while (!hallu_race->name || (hallu_race->flags1 & RF1_UNIQUE));

#ifdef JP
				msg_format("%sは%sに進化した。", m_name, r_name + hallu_race->name);
#else
				msg_format("%^s changed into %s.", m_name, r_name + hallu_race->name);
#endif
			}
			else
			{
#ifdef JP
				msg_format("%sは%sに進化した。", m_name, r_name + r_ptr->name);
#else
				msg_format("%^s changed into %s.", m_name, r_name + r_ptr->name);
#endif
			}
		}

		if (!p_ptr->image) r_info[old_r_idx].r_xtra1 |= MR1_SINKA;

		/* Now you feel very close to this pet. */
		m_ptr->parent_m_idx = 0;
	}
	update_mon(m_idx, FALSE);
	lite_spot(m_ptr->fy, m_ptr->fx);
}

/*	Time-Lords are unique in that their effects will work against unique monsters.
*/
static bool time_lord_monster_save(monster_race* r_ptr, int power)
{
	if (r_ptr->flagsr & RFR_RES_ALL)
		return TRUE;
	else if (r_ptr->flags1 & RF1_UNIQUE)
		return r_ptr->level > randint1(2*power/3);
	else
		return r_ptr->level > randint1(power);
}
			
/*
 * do_cmd_cast calls this function if the player's class
 * is 'Time Lord'.
 */
static bool cast_time_lord_spell(int spell)
{
	int b = 0;
	int dir;
	int plev = p_ptr->lev;

	/* spell code */
	switch (spell)
	{
	case 0: /* Time Spurt */
		set_tim_spurt(spell_power(2 + randint1(2)), FALSE);
		break;
	case 1:   /* Decay Door 
	             There is a nice destroy_door() fn that fires a beam, but I just wand an adjacent
				 square effect.
	          */
	/*	if (!get_aim_dir(&dir)) return FALSE;
		destroy_door(dir);*/
		{
			int y, x;

			if (!get_rep_dir2(&dir)) return FALSE;
			if (dir == 5) return FALSE;

			y = py + ddy[dir];
			x = px + ddx[dir];

			if (!in_bounds(y, x)) return FALSE;

			if (!cave_have_flag_bold(y, x, FF_DOOR)) break;
	
			/* Destroy the feature */
			cave_alter_feat(y, x, FF_TUNNEL);

			/* Hack: Permanent Door in Arena */
			if (!cave_have_flag_bold(y, x, FF_DOOR))
			{
				msg_print("The door withers away.");
	
				/* Update some things */
				p_ptr->update |= (PU_FLOW);
			}
		}	
		break;

	case 2:	 /* Decay Wall 
	            There is a nice wall_to_mud() fn that fires a beam, but I just want an adjacent
				square effect.
	         */
	/*	if (!get_aim_dir(&dir)) return FALSE;
		wall_to_mud(dir); */
		{
			int y, x;

			if (!get_rep_dir2(&dir)) return FALSE;
			if (dir == 5) return FALSE;

			y = py + ddy[dir];
			x = px + ddx[dir];
			
			if (!in_bounds(y, x)) return FALSE;

		/*	TODO: Hurt Wall Monsters!
		    if (cave[y][x].m_idx)
				py_attack(y, x, HISSATSU_HAGAN); */
	
			if (!cave_have_flag_bold(y, x, FF_HURT_ROCK))  break;
	
			/* Destroy the feature 
			   Note, it is my intention to allow treasure to be recovered this way.
			   The gold, silver, mithril is immune to the effects of time!
			*/
			cave_alter_feat(y, x, FF_HURT_ROCK);
			msg_print("The wall turns to dust.");
	
			/* Update some things */
			p_ptr->update |= (PU_FLOW);
		}	
		break;

	case 3:  /* Devolution */
		{
			int y, x, r_idx, m_idx;
			monster_type *m_ptr;
			monster_race *r_ptr;
			char m_name[80];

			if (!get_rep_dir2(&dir)) return FALSE;
			if (dir == 5) return FALSE;

			y = py + ddy[dir];
			x = px + ddx[dir];

			if (!in_bounds(y, x)) return FALSE;

			/* No monster?  Well just ignore ... */
			m_idx = cave[y][x].m_idx;
		    if (!m_idx) break;

			/* Get the monster */
			m_ptr = &m_list[m_idx];

			/* Skip Dead Monsters? */
			if (!m_ptr->r_idx) break;
			monster_desc(m_name, m_ptr, 0);

			r_idx = real_r_idx(m_ptr);
			if (r_idx <= 0) return FALSE;
			r_ptr = &r_info[r_idx];	/* We'll use the current race for a saving throw */
			r_idx = find_devolution_idx(r_idx);
			
			/* Skip Primitive Monsters */
			if (r_idx <= 0)
			{
				msg_format("%^s is too primitive for further devolution.", m_name);
				break;
			}
			set_monster_csleep(m_idx, 0);
			if (time_lord_monster_save(r_ptr, 2*plev))
			{		
				msg_format("%^s resists.", m_name);
				break;
			}
			else
				change_monster_race(m_idx, r_idx);
		}
		break;

	case 4:  /* Evolution */
		{
			int y, x, r_idx, m_idx;
			monster_type *m_ptr;
			monster_race *r_ptr;
			char m_name[80];

			if (!get_rep_dir2(&dir)) return FALSE;
			if (dir == 5) return FALSE;

			y = py + ddy[dir];
			x = px + ddx[dir];

			if (!in_bounds(y, x)) return FALSE;

			/* No monster?  Well just ignore ... */
			m_idx = cave[y][x].m_idx;
		    if (!m_idx) break;

			/* Get the monster */
			m_ptr = &m_list[m_idx];

			/* Skip Dead Monsters? */
			if (!m_ptr->r_idx) break;
			monster_desc(m_name, m_ptr, 0);

			r_idx = real_r_idx(m_ptr);
			if (r_idx <= 0) return FALSE;
			r_idx = find_evolution_idx(r_idx);
			
			/* Skip Advanced Monsters */
			if (r_idx <= 0)
			{
				msg_format("%^s has reached evolutionary perfection.", m_name);
				break;
			}
			r_ptr = &r_info[r_idx];	/* We'll use the target race for a saving throw */
			set_monster_csleep(m_idx, 0);
			if (time_lord_monster_save(r_ptr, 2*plev))
			{		
				msg_format("%^s resists.", m_name);
				break;
			}
			else
				change_monster_race(m_idx, r_idx);
		}
		break;

	case 5:  /* "Slow Monster" */
		{
			int y, x, m_idx;
			monster_type *m_ptr;
			monster_race *r_ptr;
			char m_name[80];

			if (!get_rep_dir2(&dir)) return FALSE;
			if (dir == 5) return FALSE;

			y = py + ddy[dir];
			x = px + ddx[dir];

			if (!in_bounds(y, x)) return FALSE;

			m_idx = cave[y][x].m_idx;
		    if (!m_idx) break;
			m_ptr = &m_list[m_idx];

			/* Skip Dead Monsters? */
			if (!m_ptr->r_idx) break;
			monster_desc(m_name, m_ptr, 0);
			r_ptr = &r_info[m_ptr->r_idx];
			set_monster_csleep(m_idx, 0);

			if (time_lord_monster_save(r_ptr, 3*plev))
			{		
				msg_format("%^s resists.", m_name);
				break;
			}
			else if (set_monster_slow(m_idx, MON_SLOW(m_ptr) + 50))
				msg_format("%^s starts moving slower.", m_name);
			else
				msg_format("%^s is already slow.", m_name);
		}	
		break;

	case 6:  /* "Back to Origins" */
		{
			int i, ct;

			ct = 0;
			for (i = 1; i < max_m_idx; i++)
			{
			monster_type *m_ptr = &m_list[i];
			monster_race *r_ptr;

				if (!m_ptr->r_idx) continue;
				r_ptr = real_r_ptr(m_ptr);
				if ( (r_ptr->flags2 & RF2_MULTIPLY)
				  && r_ptr->cur_num > 1  /* shouldn't this be 2 ... well, breeding in *band has never been biologically accurate */
				  && !time_lord_monster_save(r_ptr, 3*plev) )
				{
					delete_monster_idx(i);
					ct++;
				}
			}

			if (ct > 0)
				msg_print("You feel the local population has reverted to an earlier state.");
			else
				msg_print("You feel the local population is stable.");
		}
		break;

	case 7:  /* "Haste Self" */
		set_fast(10 + randint1((plev * 3) / 2), FALSE);
		break;

	case 8:  /* "Haste Monster" */
		if (!get_aim_dir(&dir)) return FALSE;
		speed_monster(dir);
		break;

	case 9:  /* "Mass Slow" 
	             Sorry, but this one won't affect uniques at all.  I could hack spells1.c for this
				 class, but actually, I'm OK with this since if you really want to slow a unique, then
				 you better use the riskier touch version.  Plus, hacking spells1.c might also inadvertantly
				 boost _SlowMonster, which I don't want!
			 */
		project_hack(GF_OLD_SLOW, 3*plev + 10);
		break;

	case 10:  /* "Temporal Prison" */
		{
			int y, x, m_idx;
			monster_type *m_ptr;
			monster_race *r_ptr;
			char m_name[80];

			if (!get_rep_dir2(&dir)) return FALSE;
			if (dir == 5) return FALSE;

			y = py + ddy[dir];
			x = px + ddx[dir];

			if (!in_bounds(y, x)) return FALSE;

			m_idx = cave[y][x].m_idx;
		    if (!m_idx) break;
			m_ptr = &m_list[m_idx];

			/* Skip Dead Monsters? */
			if (!m_ptr->r_idx) break;
			monster_desc(m_name, m_ptr, 0);
			r_ptr = &r_info[m_ptr->r_idx];
			set_monster_csleep(m_idx, 0);

			if (time_lord_monster_save(r_ptr, 3*plev))
			{		
				msg_format("%^s resists.", m_name);
				break;
			}
			else 
			{
				msg_format("%^s is suspended!", m_name);
				set_monster_csleep(m_idx, 1500);
			}			
		}	
		break;

	case 11: /* "Rewind Time" */
		if (p_ptr->inside_arena || ironman_downward || !dun_level)
		{
			msg_print("Nothing happens.");
			return FALSE;
		}

		if (!get_check("You will irreversibly alter the time line. Are you sure?")) return FALSE;
		recall_player(1);
		process_world_aux_movement(); /* Hack! Recall Now, Now, Now!!! */

		if (p_ptr->prace == RACE_ANDROID)
		{
			dec_stat(A_CON, 10, TRUE);
			if (one_in_(2)) break;
			dec_stat(A_INT, 10, TRUE);
			if (one_in_(2)) break;
			dec_stat(A_DEX, 10, TRUE);
			if (one_in_(2)) break;
			dec_stat(A_WIS, 10, TRUE);
			if (one_in_(2)) break;
			dec_stat(A_STR, 10, TRUE);
			if (one_in_(2)) break;
			dec_stat(A_CHR, 10, TRUE);
		}
		else
		{
			int amount = 0;
			
			if (p_ptr->lev < 3) break;
			amount = player_exp[p_ptr->lev-2] * p_ptr->expfact / 100L;
			amount -= player_exp[p_ptr->lev-3] * p_ptr->expfact / 100L;
			if (amount > 100000) amount = 100000;
			if (amount > p_ptr->max_exp) amount = p_ptr->max_exp;
			if (amount > p_ptr->exp) p_ptr->exp = 0;
			else p_ptr->exp -= amount;
			p_ptr->max_exp -= amount;
			check_experience();
		}
		break;

	case 12: /* "Remembrance" */
		do_res_stat(A_STR);
		do_res_stat(A_INT);
		do_res_stat(A_WIS);
		do_res_stat(A_DEX);
		do_res_stat(A_CON);
		do_res_stat(A_CHR);
		restore_level();
		break;

	case 13: /* "Speed Essentia" */
		set_tim_speed_essentia(3, FALSE);
		break;

	case 14: /* "The World" */
		if (world_player)
		{
			msg_print("Time is already stopped.");
			return (FALSE);
		}
		world_player = TRUE;
		msg_print("You yell 'Time!'");
		msg_print(NULL);

		/* Hack */
		p_ptr->energy_need -= 1000 + (100 + p_ptr->csp - 50)*TURNS_PER_TICK/8;

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Update monsters */
		p_ptr->update |= (PU_MONSTERS);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

		handle_stuff();
		break;

	default:
		msg_print("Zap?");
		break;
	}

	return TRUE;
}

static bool cast_blood_knight_spell(int spell)
{
	int b = 0;
	int dir;
	int plev = p_ptr->lev;

	/* spell code */
	switch (spell)
	{
	case 0: /* Blood Flow */
		{
			int cut = p_ptr->cut;
			cut += cut/5;
			if (cut < CUT_LIGHT)
				cut = CUT_LIGHT;

			set_cut(cut, FALSE);
		}
		break;
	
	case 1: /* Blood Sight */
		if (plev < 30)
			detect_monsters_living(DETECT_RAD_DEFAULT);
		else
			set_tim_blood_sight(randint1(30) + 30, FALSE);
		break;
	
	case 2: /* Blood Spray */
		{
			int dice = 3;
			int sides = 5;
			int rad = (plev < 30) ? 3 : 4;
			int base = plev + plev/4;

			fire_ball(GF_BLOOD, 5, 2*(damroll(dice, sides) + base), rad);
		}
		break;
	
	case 3: /* Blood Bath */
		{
			bool chg = FALSE;
			if (do_res_stat(A_CON)) chg = TRUE;
			if (set_poisoned(0, TRUE)) chg = TRUE;
			if (!chg) msg_print("You don't need a bath just yet.");
		}
		break;
	
	case 4: /* Blood Shield */
		set_tim_blood_shield(randint1(20) + 30, FALSE);
		break;
	
	case 5: /* Blood Seeking */
		set_tim_blood_seek(randint1(30) + 30, FALSE);
		break;
	
	case 6: /* Blood Rage */
	    {
			int dur = randint1(plev) + plev;
			set_fast(dur, FALSE);
			set_shero(dur, FALSE);
			set_afraid(0, TRUE);
		}
		break;
	
	case 7: /* Blood Feast */
		if (p_ptr->tim_blood_feast)
		{
			if (!get_check("Cancel the Blood Feast? ")) return FALSE;
			set_tim_blood_feast(0, TRUE);
		}
		else
			set_tim_blood_feast(randint1(25) + 25, FALSE);
		break;

	case 8: /* Blood Revenge */
		set_tim_blood_revenge(randint1(5) + 5, FALSE);
		break;

	case 9: /* Blood Pool */
		{
			object_type forge, *q_ptr = &forge;
			msg_print("You feel light headed.");
			object_prep(q_ptr, lookup_kind(TV_POTION, SV_POTION_BLOOD));
			drop_near(q_ptr, -1, py, px);
		}
		break;
	
	case 10: /* Blood Explosion */
		msg_print("You cut too deep ... Your blood explodes!");
		dispel_living(500);
		break;
	
	default:
		msg_print("Zap?");
		break;
	}

	return TRUE;
}

static bool cast_warlock_spell(int spell)
{
	int dir, rng, dice, sides, rad;
	int plev = p_ptr->lev;

	/* Most Eldritch Blasts work the Same ... */
	rng = warlock_range();
	dice = warlock_dice();
	sides = warlock_sides();
	rad = 0;

	/* spell code */
	switch (spell)
	{
	case 0:	/* Basic */
		project_length = rng;
		if (!get_aim_dir(&dir)) return FALSE;
		
		fire_ball(GF_ELDRITCH, dir, spell_power(damroll(dice, sides)), rad);
		break;

	case 1: /* Extended */
		rng += 10 * plev/50;
		project_length = rng;
		if (!get_aim_dir(&dir)) return FALSE;
		
		fire_ball(GF_ELDRITCH, dir, spell_power(damroll(dice, sides)), rad);
		break;

	case 2: /* Spear */
		project_length = rng;
		if (!get_aim_dir(&dir)) return FALSE;
		
		fire_beam(GF_ELDRITCH, dir, spell_power(damroll(dice, sides)));
		break;

	case 3:	/* Burst */
		rad = 2;
		project_length = rng;
		if (!get_aim_dir(&dir)) return FALSE;
		
		fire_ball(GF_ELDRITCH, dir, spell_power(damroll(dice, sides)), rad);
		break;

	case 4:	/* Stunning */
		project_length = rng;
		if (!get_aim_dir(&dir)) return FALSE;
		
		fire_ball(GF_ELDRITCH_STUN, dir, spell_power(damroll(dice, sides)), rad);
		break;

	case 5:	/* Special */
		project_length = rng;
		if (!get_aim_dir(&dir)) return FALSE;
		
		switch (p_ptr->psubclass)
		{
		case PACT_UNDEAD:
			fire_ball(GF_ELDRITCH_DRAIN, dir, spell_power(damroll(dice, sides)), rad);
			break;

		case PACT_DRAGON:
			fire_ball(GF_FIRE, dir, spell_power(damroll(dice, sides)/2), rad);
			fire_ball(GF_COLD, dir, spell_power(damroll(dice, sides)/2), rad);
			fire_ball(GF_ACID, dir, spell_power(damroll(dice, sides)/2), rad);
			fire_ball(GF_ELEC, dir, spell_power(damroll(dice, sides)/2), rad);
			fire_ball(GF_POIS, dir, spell_power(damroll(dice, sides)/2), rad);
			break;

		case PACT_ANGEL:
			fire_ball(GF_ELDRITCH_DISPEL, dir, spell_power(damroll(dice, sides)), rad);
			break;

		case PACT_DEMON:
			{
				int dam = damroll(dice, sides);
				dam *= 2;
				dam = spell_power(dam);
				fire_ball(GF_ELDRITCH, dir, dam, rad);
				take_hit(DAMAGE_USELIFE, dam/3, "vengeful blast", -1);
			}
			break;

		case PACT_ABERRATION:
			fire_ball(GF_ELDRITCH_CONFUSE, dir, spell_power(damroll(dice, sides)), rad);
			break;

		default:
			msg_print("BUG(What pact have you made?)");
			break;
		}
		break;

	case 6:	/* Empowered */
		project_length = rng;
		if (!get_aim_dir(&dir)) return FALSE;
		
		fire_ball(GF_ELDRITCH, dir, spell_power(damroll(dice, sides) * 3/2), rad);
		/* I still don't quite understand process_player() ... I can't see
		   where the player actually moves, so I am mimicing the lightspeed counter, which
		   appears to decrement before player action.  The code is befuddling, so try
		   setting to counter to 2.  It should tick to 1 before the players next action 
		   and block spells for that action, and then tick to 0 before the subsequent action.
		*/
		set_tim_no_spells(p_ptr->tim_no_spells + 1 + 1, FALSE);
		break;

	default:
		msg_print("Zap?");
		break;
	}

	return TRUE;
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'ForceTrainer'.
 */
static bool cast_force_spell(int spell)
{
	int             dir;
	int             plev = p_ptr->lev;
	int             boost = p_ptr->magic_num1[0];

	if (heavy_armor()) boost /= 2;

	/* spell code */
	switch (spell)
	{
	case 0:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_MISSILE, dir, damroll(3 + ((plev - 1) / 5) + boost / 12, 4), 0);
		break;
	case 1:
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 2:
		set_tim_levitation(randint1(30) + 30 + boost / 5, FALSE);
		break;
	case 3:
		project_length = plev / 8 + 3;
		if (!get_aim_dir(&dir)) return FALSE;

		fire_beam(GF_MISSILE, dir, damroll(5 + ((plev - 1) / 5) + boost / 10, 5));
		break;
	case 4:
		set_resist_magic(randint1(20) + 20 + boost / 5, FALSE);
		break;
	case 5:
#ifdef JP
		msg_print("気を練った。");
#else
		msg_print("You improved the Force.");
#endif
		p_ptr->magic_num1[0] += (70 + plev);
		p_ptr->update |= (PU_BONUS);
		if (randint1(p_ptr->magic_num1[0]) > (plev * 4 + 120))
		{
#ifdef JP
			msg_print("気が暴走した！");
#else
			msg_print("The Force exploded!");
#endif
			fire_ball(GF_MANA, 0, p_ptr->magic_num1[0] / 2, 10);
#ifdef JP
			take_hit(DAMAGE_LOSELIFE, p_ptr->magic_num1[0] / 2, "気の暴走", -1);
#else
			take_hit(DAMAGE_LOSELIFE, p_ptr->magic_num1[0] / 2, "Explosion of the Force", -1);
#endif
		}
		else return TRUE;
		break;
	case 6:
		set_tim_sh_touki(randint1(plev / 2) + 15 + boost / 7, FALSE);
		break;
	case 7:
	{
		int y, x, dam;
		project_length = 1;
		if (!get_aim_dir(&dir)) return FALSE;

		y = py + ddy[dir];
		x = px + ddx[dir];
		dam = damroll(8 + ((plev - 5) / 4) + boost / 12, 8);
		fire_beam(GF_MISSILE, dir, dam);
		if (cave[y][x].m_idx)
		{
			int i;
			int ty = y, tx = x;
			int oy = y, ox = x;
			int m_idx = cave[y][x].m_idx;
			monster_type *m_ptr = &m_list[m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			char m_name[80];

			monster_desc(m_name, m_ptr, 0);

			if (randint1(r_ptr->level * 3 / 2) > randint0(dam / 2) + dam/2)
			{
#ifdef JP
				msg_format("%sは飛ばされなかった。", m_name);
#else
				msg_format("%^s was not blown away.", m_name);
#endif
			}
			else
			{
				for (i = 0; i < 5; i++)
				{
					y += ddy[dir];
					x += ddx[dir];
					if (cave_empty_bold(y, x))
					{
						ty = y;
						tx = x;
					}
					else break;
				}
				if ((ty != oy) || (tx != ox))
				{
#ifdef JP
					msg_format("%sを吹き飛ばした！", m_name);
#else
					msg_format("You blow %s away!", m_name);
#endif
					cave[oy][ox].m_idx = 0;
					cave[ty][tx].m_idx = m_idx;
					m_ptr->fy = ty;
					m_ptr->fx = tx;

					update_mon(m_idx, TRUE);
					lite_spot(oy, ox);
					lite_spot(ty, tx);

					if (r_ptr->flags7 & (RF7_LITE_MASK | RF7_DARK_MASK))
						p_ptr->update |= (PU_MON_LITE);
				}
			}
		}
		break;
	}
	case 8:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_MISSILE, dir, damroll(10, 6) + plev * 3 / 2 + boost * 3 / 5, (plev < 30) ? 2 : 3);
		break;
	case 9:
	{
		int m_idx;

		if (!target_set(TARGET_KILL)) return FALSE;
		m_idx = cave[target_row][target_col].m_idx;
		if (!m_idx) break;
		if (!player_has_los_bold(target_row, target_col)) break;
		if (!projectable(py, px, target_row, target_col)) break;
		dispel_monster_status(m_idx);
		break;
	}
	case 10:
	{
		int i;
		bool success = FALSE;

		for (i = 0; i < 1 + boost/100; i++)
			if (summon_specific(-1, py, px, plev, SUMMON_PHANTOM, PM_FORCE_PET))
				success = TRUE;
		if (success)
		{
#ifdef JP
msg_print("御用でございますが、御主人様？");
#else
			msg_print("'Your wish, master?'");
#endif
		}
		else
		{
#ifdef JP
			msg_print("何も現れなかった。");
#else
			msg_print("Nothing happen.");
#endif
		}
		break;
	}
	case 11:
		fire_ball(GF_FIRE, 0, 200 + (2 * plev) + boost * 2, 10);
		break;
	case 12:
		if (!get_aim_dir(&dir)) return FALSE;

		fire_beam(GF_MANA, dir, damroll(10 + (plev / 2) + boost * 3 / 10, 15));
		break;
	case 13:
		set_lightspeed(randint1(16) + 16 + boost / 20, FALSE);
		break;
	default:
#ifdef JP
msg_print("なに？");
#else
		msg_print("Zap?");
#endif

	}
	p_ptr->magic_num1[0] = 0;
	p_ptr->update |= (PU_BONUS);

	return TRUE;
}

/* by henkma */
/* calculate mirrors */
static int number_of_mirrors( void )
{
  int x,y;
  int val=0;
  for( x=0 ; x < cur_wid ; x++ ){
    for( y=0 ; y < cur_hgt ; y++ ){
      if (is_mirror_grid(&cave[y][x])) val++;
    }
  }
  return val;
}

static bool cast_mirror_spell(int spell)
{
	int             dir;
	int             plev = p_ptr->lev;
	int		tmp;
	int		x,y;

	/* spell code */
	switch (spell)
	{
	/* mirror of seeing */
	case 0:
	  tmp = is_mirror_grid(&cave[py][px]) ? 4 : 0;
	  if( plev + tmp > 4)detect_monsters_normal(DETECT_RAD_DEFAULT);
	  if( plev + tmp > 18 )detect_monsters_invis(DETECT_RAD_DEFAULT);
	  if( plev + tmp > 28 )set_tim_esp(plev,FALSE);
	  if( plev + tmp > 38 )map_area(DETECT_RAD_MAP);
	  if( tmp == 0 && plev < 5 ){
#ifdef JP
	    msg_print("鏡がなくて集中できなかった！");
#else
	    msg_print("You need a mirror to concentrate!");
#endif
	  }
	  break;
	/* drip of light */
	case 1:
	  if( number_of_mirrors() < 4 + plev/10 ){
	    place_mirror();
	  }
	  else {
#ifdef JP
msg_format("これ以上鏡は制御できない！");
#else
msg_format("There are too many mirrors to control!");
#endif
	  }
	  break;
	case 2:
	  if (!get_aim_dir(&dir)) return FALSE;
	  if ( plev > 9 && is_mirror_grid(&cave[py][px]) ) {
	    fire_beam(GF_LITE, dir,spell_power(damroll(3+((plev-1)/5),4)));
	  }
	  else {
	    fire_bolt(GF_LITE, dir,spell_power(damroll(3+((plev-1)/5),4)));
	  }
	  break;
	/* warped mirror */
	case 3:
	  teleport_player(10, 0L);
	  break;
	/* mirror of light */
	case 4:
	  (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
	  break;
	/* mirror of wandering */
	case 5:
	  teleport_player(plev * 5, 0L);
	  break;
	/* robe of dust */
	case 6:
	  set_dustrobe(20+randint1(20),FALSE);
	  break;
	/* banishing mirror */
	case 7:
	  if (!get_aim_dir(&dir)) return FALSE;
	  (void)fire_beam(GF_AWAY_ALL, dir , spell_power(plev));
	  break;
	/* mirror clashing */
	case 8:
	  if (!get_aim_dir(&dir)) return FALSE;
	  fire_ball(GF_SHARDS, dir, spell_power(damroll(8 + ((plev - 5) / 4), 8)),
		    (plev > 20 ? spell_power((plev - 20) / 8 + 1) : 0));
	  break;
	/* mirror sleeping */
	case 9:
	  for(x=0;x<cur_wid;x++){
	    for(y=0;y<cur_hgt;y++){
	      if (is_mirror_grid(&cave[y][x])) {
		project(0,2,y,x,plev,GF_OLD_SLEEP,(PROJECT_GRID|PROJECT_ITEM|PROJECT_KILL|PROJECT_JUMP|PROJECT_NO_HANGEKI),-1);
	      }
	    }
	  }
	  break;
	/* seeker ray */
	case 10:
	  if (!get_aim_dir(&dir)) return FALSE;
	  fire_beam(GF_SEEKER,dir, spell_power(damroll(11+(plev-5)/4,8)));
	  break;
	/* seal of mirror */
	case 11:
	  seal_of_mirror(spell_power(plev*4+100));
	  break;
	/* shield of water */
	case 12:
	  tmp = spell_power(20+randint1(20));
	  set_shield(tmp, FALSE);
	  if( plev > 31 )set_tim_reflect(tmp, FALSE);
	  if( plev > 39 )set_resist_magic(tmp,FALSE);
	  break;
	/* super ray */
	case 13:
	  if (!get_aim_dir(&dir)) return FALSE;
	  fire_beam(GF_SUPER_RAY,dir, spell_power(150+randint1(2*plev)));
	  break;
	/* illusion light */
	case 14:
	  tmp = is_mirror_grid(&cave[py][px]) ? 4 : 3;
	  slow_monsters();
	  stun_monsters(spell_power(plev*tmp));
	  confuse_monsters(spell_power(plev*tmp));
	  turn_monsters(spell_power(plev*tmp));
	  stun_monsters(spell_power(plev*tmp));
	  stasis_monsters(spell_power(plev*tmp));
	  break;
	/* mirror shift */
	case 15:
	  if( !is_mirror_grid(&cave[py][px]) ){
#ifdef JP
		msg_print("鏡の国の場所がわからない！");
#else
		msg_print("You cannot find out where is the world of mirror!");
#endif
		break;
	  }
	  alter_reality();
	  break;
	/* mirror tunnel */
	case 16:
#ifdef JP
	  msg_print("鏡の世界を通り抜け…  ");
#else
	  msg_print("Go through the world of mirror...");
#endif
	  return mirror_tunnel();

	/* mirror of recall */
	case 17:
		return word_of_recall();
	/* multi-shadow */
	case 18:
	  set_multishadow(6+randint1(6),FALSE);
	  break;
	/* binding field */
	case 19:
#ifdef JP
	  if( !binding_field(plev*11+5) )msg_print("適当な鏡を選べなかった！");
#else
	  if( !binding_field(spell_power(plev*11+5)) )msg_print("You were not able to choose suitable mirrors!");
#endif
	  break;
	/* mirror of Ruffnor */
	case 20:
	  (void)set_invuln(spell_power(randint1(4)+4),FALSE);
	  break;
	default:
#ifdef JP
msg_print("なに？");
#else
		msg_print("Zap?");
#endif

	}
	p_ptr->magic_num1[0] = 0;

	return TRUE;
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'berserker'.
 */
static bool cast_berserk_spell(int spell)
{
	int y, x;
	int dir;

	/* spell code */
	switch (spell)
	{
	case 0:
		detect_monsters_mind(DETECT_RAD_DEFAULT);
		break;
	case 1:
	{
		if (p_ptr->riding)
		{
#ifdef JP
			msg_print("乗馬中には無理だ。");
#else
			msg_print("You cannot do it when riding.");
#endif
			return FALSE;
		}

		if (!get_rep_dir2(&dir)) return FALSE;

		if (dir == 5) return FALSE;
		y = py + ddy[dir];
		x = px + ddx[dir];

		if (!cave[y][x].m_idx)
		{
#ifdef JP
			msg_print("その方向にはモンスターはいません。");
#else
			msg_print("There is no monster.");
#endif
			return FALSE;
		}

		py_attack(y, x, 0);

		if (!player_can_enter(cave[y][x].feat, 0) || is_trap(cave[y][x].feat))
			break;

		y += ddy[dir];
		x += ddx[dir];

		if (player_can_enter(cave[y][x].feat, 0) && !is_trap(cave[y][x].feat) && !cave[y][x].m_idx)
		{
			msg_print(NULL);

			/* Move the player */
			(void)move_player_effect(y, x, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
		}
		break;
	}
	case 2:
	{
		if (!get_rep_dir2(&dir)) return FALSE;
		y = py + ddy[dir];
		x = px + ddx[dir];
		move_player(dir, easy_disarm, TRUE);
		break;
	}
	case 3:
		earthquake(py, px, 8+randint0(5));
		break;
	case 4:
	{
		cave_type       *c_ptr;
		monster_type    *m_ptr;

		for (dir = 0; dir < 8; dir++)
		{
			y = py + ddy_ddd[dir];
			x = px + ddx_ddd[dir];
			c_ptr = &cave[y][x];

			/* Get the monster */
			m_ptr = &m_list[c_ptr->m_idx];

			/* Hack -- attack monsters */
			if (c_ptr->m_idx && (m_ptr->ml || cave_have_flag_bold(y, x, FF_PROJECT)))
				py_attack(y, x, 0);
		}
		break;
	}
	default:
#ifdef JP
msg_print("なに？");
#else
		msg_print("Zap?");
#endif

	}
	return TRUE;
}



/*
 * do_cmd_cast calls this function if the player's class
 * is 'ninja'.
 */
static bool cast_ninja_spell(int spell)
{
	int x, y;
	int dir;
	int plev = p_ptr->lev;

	/* spell code */
	switch (spell)
	{
	case 0:
		(void)unlite_area(0, 3);
		break;
	case 1:
		if (plev > 44)
		{
			wiz_lite(TRUE);
		}
		detect_monsters_normal(DETECT_RAD_DEFAULT);
		if (plev > 4)
		{
			detect_traps(DETECT_RAD_DEFAULT, TRUE);
			detect_doors(DETECT_RAD_DEFAULT);
			detect_stairs(DETECT_RAD_DEFAULT);
		}
		if (plev > 14)
		{
			detect_objects_normal(DETECT_RAD_DEFAULT);
		}
		break;
	case 2:
	{
		teleport_player(10, 0L);
		break;
	}
	case 3:
	{
		if (!(p_ptr->special_defense & NINJA_KAWARIMI))
		{
#ifdef JP
			msg_print("敵の攻撃に対して敏感になった。");
#else
			msg_print("You are now prepare to evade any attacks.");
#endif

			p_ptr->special_defense |= NINJA_KAWARIMI;
			p_ptr->redraw |= (PR_STATUS);
		}
		break;
	}
	case 4:
	{
		teleport_player(p_ptr->lev * 5, 0L);
		break;
	}
	case 5:
	{
		if (!get_rep_dir(&dir, FALSE)) return FALSE;
		y = py + ddy[dir];
		x = px + ddx[dir];
		if (cave[y][x].m_idx)
		{
			py_attack(y, x, 0);
			if (randint0(p_ptr->skill_dis) < 7)
#ifdef JP
msg_print("うまく逃げられなかった。");
#else
				msg_print("You failed to run away.");
#endif
			else
			{
				teleport_player(30, 0L);
			}
		}
		else
		{
#ifdef JP
msg_print("その方向にはモンスターはいません。");
#else
			msg_print("You don't see any monster in this direction");
#endif

			msg_print(NULL);
		}
		break;
	}
	case 6:
	{
		if (!get_aim_dir(&dir)) return FALSE;
		(void)stasis_monster(dir);
		break;
	}
	case 7:
		return ident_spell(FALSE);
	case 8:
		set_tim_levitation(randint1(20) + 20, FALSE);
		break;
	case 9:
		fire_ball(GF_FIRE, 0, 50+plev, plev/10+2);
		teleport_player(30, 0L);
		set_oppose_fire(plev, FALSE);
		break;
	case 10:
		return rush_attack(NULL);
	case 11:
	{
		int i;
		for (i = 0; i < 8; i++)
		{
			int slot;

			for (slot = 0; slot < INVEN_PACK; slot++)
			{
				if (inventory[slot].tval == TV_SPIKE) break;
			}
			if (slot == INVEN_PACK)
			{
#ifdef JP
				if (!i) msg_print("くさびを持っていない。");
				else msg_print("くさびがなくなった。");
#else
				if (!i) msg_print("You have no Iron Spikes.");
				else msg_print("You have no more Iron Spikes.");
#endif
				return FALSE;
			}

			/* Gives a multiplier of 2 at first, up to 3 at 40th */
			do_cmd_throw_aux(1, FALSE, slot);

			energy_use = 100;
		}
		break;
	}
	case 12:
	{
		monster_type *m_ptr;
		int m_idx;
		char m_name[80];
		int i;
		int path_n;
		u16b path_g[512];
		int ty,tx;

		if (!target_set(TARGET_KILL)) return FALSE;
		m_idx = cave[target_row][target_col].m_idx;
		if (!m_idx) break;
		if (m_idx == p_ptr->riding) break;
		if (!player_has_los_bold(target_row, target_col)) break;
		if (!projectable(py, px, target_row, target_col)) break;
		m_ptr = &m_list[m_idx];
		monster_desc(m_name, m_ptr, 0);
#ifdef JP
		msg_format("%sを引き戻した。", m_name);
#else
		msg_format("You pull back %s.", m_name);
#endif

		path_n = project_path(path_g, MAX_RANGE, target_row, target_col, py, px, 0);
		ty = target_row, tx = target_col;
		for (i = 1; i < path_n; i++)
		{
			int ny = GRID_Y(path_g[i]);
			int nx = GRID_X(path_g[i]);
			cave_type *c_ptr = &cave[ny][nx];

			if (in_bounds(ny, nx) && cave_empty_bold(ny, nx) &&
			    !(c_ptr->info & CAVE_OBJECT) &&
				!pattern_tile(ny, nx))
			{
				ty = ny;
				tx = nx;
			}
		}
		/* Update the old location */
		cave[target_row][target_col].m_idx = 0;

		/* Update the new location */
		cave[ty][tx].m_idx = m_idx;

		/* Move the monster */
		m_ptr->fy = ty;
		m_ptr->fx = tx;

		/* Wake the monster up */
		(void)set_monster_csleep(m_idx, 0);

		/* Update the monster (new location) */
		update_mon(m_idx, TRUE);

		/* Redraw the old grid */
		lite_spot(target_row, target_col);

		/* Redraw the new grid */
		lite_spot(ty, tx);

		if (r_info[m_ptr->r_idx].flags7 & (RF7_LITE_MASK | RF7_DARK_MASK))
			p_ptr->update |= (PU_MON_LITE);

		if (m_ptr->ml)
		{
			/* Auto-Recall if possible and visible */
			if (!p_ptr->image) monster_race_track(m_ptr->ap_r_idx);

			/* Track a new monster */
			health_track(m_idx);
		}

		break;
	}
	case 13:
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_OLD_CONF, dir, plev*3, 3);
		break;
	case 14:
		project_length = -1;
		if (!get_aim_dir(&dir))
		{
			project_length = 0;
			return FALSE;
		}
		project_length = 0;

		(void)teleport_swap(dir);
		break;
	case 15:
		explosive_rune();
		break;
	case 16:
		(void)set_kabenuke(randint1(plev/2) + plev/2, FALSE);
		set_oppose_acid(plev, FALSE);
		break;
	case 17:
		fire_ball(GF_POIS, 0, 75+plev*2/3, plev/5+2);
		fire_ball(GF_OLD_DRAIN, 0, 75+plev*2/3, plev/5+2);
		fire_ball(GF_CONFUSION, 0, 75+plev*2/3, plev/5+2);
		teleport_player(30, 0L);
		break;
	case 18:
	{
		int k;
		int num = damroll(3, 9);

		for (k = 0; k < num; k++)
		{
			int typ = one_in_(2) ? GF_FIRE : one_in_(3) ? GF_NETHER : GF_PLASMA;
			int attempts = 1000;

			while (attempts--)
			{
				scatter(&y, &x, py, px, 4, 0);

				if (!player_bold(y, x)) break;
			}
			project(0, 0, y, x, damroll(6 + plev / 8, 10), typ,
				(PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_KILL), -1);
		}
		break;
	}
	case 19:
		set_multishadow(6+randint1(6), FALSE);
		break;
	default:
#ifdef JP
msg_print("なに？");
#else
		msg_print("Zap?");
#endif

	}
	return TRUE;
}



/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
void do_cmd_mind(void)
{
	int             n = 0,  b = 0;
	int             chance;
	int             minfail = 0;
	int             plev = p_ptr->lev;
	int             old_csp = p_ptr->csp;
	mind_type       spell;
	bool            cast;
	int             use_mind, mana_cost;
#ifdef JP
	cptr            p;
#endif
	bool		on_mirror = FALSE;

	/* not if confused */
	if (p_ptr->confused)
	{
#ifdef JP
msg_print("混乱していて集中できない！");
#else
		msg_print("You are too confused!");
#endif

		return;
	}

	/* get power */
	if (!get_mind_power(&n, FALSE)) return;

#ifdef JP
	switch(p_ptr->pclass)
	{
		case CLASS_MINDCRAFTER: use_mind = MIND_MINDCRAFTER;p = "精神";break;
		case CLASS_FORCETRAINER:          use_mind = MIND_KI;p = "気";break;
		case CLASS_BERSERKER:   use_mind = MIND_BERSERKER;p = "怒り";break;
		case CLASS_MIRROR_MASTER:   use_mind = MIND_MIRROR_MASTER;p = "鏡魔法";break;
		case CLASS_NINJA:       use_mind = MIND_NINJUTSU;p = "精神";break;
		default:                use_mind = 0;p = "超能力";break;
	}
#else
	switch(p_ptr->pclass)
	{
		case CLASS_MINDCRAFTER: use_mind = MIND_MINDCRAFTER;break;
		case CLASS_FORCETRAINER:          use_mind = MIND_KI;break;
		case CLASS_BERSERKER:   use_mind = MIND_BERSERKER;break;
		case CLASS_MIRROR_MASTER:   use_mind = MIND_MIRROR_MASTER;break;
		case CLASS_NINJA:       use_mind = MIND_NINJUTSU;break;
		case CLASS_TIME_LORD:       use_mind = MIND_TIME_LORD;break;
		case CLASS_BLOOD_KNIGHT:    use_mind = MIND_BLOOD_KNIGHT;break;
		case CLASS_WARLOCK:
			switch (p_ptr->psubclass)
			{
			case PACT_UNDEAD: use_mind = MIND_WARLOCK_UNDEAD; break;
			case PACT_DRAGON: use_mind = MIND_WARLOCK_DRAGON; break;
			case PACT_ANGEL: use_mind = MIND_WARLOCK_ANGEL; break;
			case PACT_DEMON: use_mind = MIND_WARLOCK_DEMON; break;
			case PACT_ABERRATION: use_mind = MIND_WARLOCK_ABERRATION; break;
			}
			break;
		default:                use_mind = 0;break;
	}
#endif
	spell = mind_powers[use_mind].info[n];

	/* Spell failure chance */
	chance = spell.fail;

	mana_cost = spell.mana_cost;
	if (use_mind == MIND_KI)
	{
		if (heavy_armor()) chance += 20;
		if (p_ptr->weapon_info[0].icky_wield) chance += 20;
		else if (buki_motteruka(INVEN_RARM)) chance += 10;
		if (p_ptr->weapon_info[1].icky_wield) chance += 20;
		else if (buki_motteruka(INVEN_LARM)) chance += 10;
		if (n == 5)
		{
			int j;
			for (j = 0; j < p_ptr->magic_num1[0] / 50; j++)
				mana_cost += (j+1) * 3 / 2;
		}
	}

	/* Verify "dangerous" spells */
	if ((use_mind == MIND_BERSERKER) || 
	    (use_mind == MIND_NINJUTSU) || 
		(use_mind == MIND_BLOOD_KNIGHT) ||
		(use_mind == MIND_WARLOCK_UNDEAD) ||
		(use_mind == MIND_WARLOCK_DRAGON) ||
		(use_mind == MIND_WARLOCK_ANGEL) ||
		(use_mind == MIND_WARLOCK_DEMON) ||
		(use_mind == MIND_WARLOCK_ABERRATION))
	{
		if (mana_cost > p_ptr->chp)
		{
#ifdef JP
msg_print("ＨＰが足りません。");
#else
			msg_print("You do not have enough hp to use this power.");
#endif
			return;
		}
	}
	else if (mana_cost > p_ptr->csp)
	{
		/* Warning */
#ifdef JP
msg_print("ＭＰが足りません。");
#else
		msg_print("You do not have enough mana to use this power.");
#endif


		if (!over_exert) return;

		/* Verify */
#ifdef JP
if (!get_check("それでも挑戦しますか? ")) return;
#else
		if (!get_check("Attempt it anyway? ")) return;
#endif

	}

	if (chance)
	{
		/* Reduce failure rate by "effective" level adjustment */
		chance -= 3 * (plev - spell.min_lev);

		chance += p_ptr->to_m_chance;

		/* Reduce failure rate by INT/WIS adjustment */
		chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

		/* Not enough mana to cast */
		if ((mana_cost > p_ptr->csp) && 
		    (use_mind != MIND_BERSERKER) && 
			(use_mind != MIND_NINJUTSU) && 
			(use_mind != MIND_BLOOD_KNIGHT) &&
			(use_mind != MIND_WARLOCK_UNDEAD) &&
			(use_mind != MIND_WARLOCK_DRAGON) &&
			(use_mind != MIND_WARLOCK_ANGEL) &&
			(use_mind != MIND_WARLOCK_DEMON) &&
			(use_mind != MIND_WARLOCK_ABERRATION))
		{
			chance += 5 * (mana_cost - p_ptr->csp);
		}

		/* Extract the minimum failure rate */
		minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

		/* Minimum failure rate */
		if (chance < minfail) chance = minfail;

		/* Stunning makes spells harder */
		if (p_ptr->stun > 50) chance += 25;
		else if (p_ptr->stun) chance += 15;

		if (use_mind == MIND_KI)
		{
			if (heavy_armor()) chance += 5;
			if (p_ptr->weapon_info[0].icky_wield) chance += 5;
			if (p_ptr->weapon_info[1].icky_wield) chance += 5;
		}
	}

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();
#ifdef JP
msg_format("%sの集中に失敗した！",p);
#else
		msg_format("You failed to concentrate hard enough!");
#endif

		sound(SOUND_FAIL);

		if ((use_mind != MIND_BERSERKER) && 
		    (use_mind != MIND_NINJUTSU) && 
			(use_mind != MIND_BLOOD_KNIGHT) && 
			(use_mind != MIND_WARLOCK_UNDEAD) &&
			(use_mind != MIND_WARLOCK_DRAGON) &&
			(use_mind != MIND_WARLOCK_ANGEL) &&
			(use_mind != MIND_WARLOCK_DEMON) &&
			(use_mind != MIND_WARLOCK_ABERRATION))
		{
			if ((use_mind == MIND_KI) && (n != 5) && p_ptr->magic_num1[0])
			{
#ifdef JP
				msg_print("気が散ってしまった．．．");
#else
				msg_print("Your improved Force has gone away...");
#endif
				p_ptr->magic_num1[0] = 0;
			}

			if (randint1(100) < (chance / 2))
			{
				/* Backfire */
			  b = randint1(100);

			  if( use_mind == MIND_MINDCRAFTER ){
				if (b < 5)
				{
#ifdef JP
msg_print("なんてこった！頭の中が真っ白になった！");
#else
					msg_print("Oh, no! Your mind has gone blank!");
#endif

					lose_all_info();
				}
				else if (b < 15)
				{
#ifdef JP
msg_print("奇妙な光景が目の前で踊っている...");
#else
					msg_print("Weird visions seem to dance before your eyes...");
#endif

					set_image(p_ptr->image + 5 + randint1(10), FALSE);
				}
				else if (b < 45)
				{
#ifdef JP
msg_print("あなたの頭は混乱した！");
#else
					msg_print("Your brain is addled!");
#endif

					set_confused(p_ptr->confused + randint1(8), FALSE);
				}
				else if (b < 90)
				{
					set_stun(p_ptr->stun + randint1(8), FALSE);
				}
				else
				{
					/* Mana storm */
#ifdef JP
msg_format("%sの力が制御できない氾流となって解放された！", p);
#else
					msg_print("Your mind unleashes its power in an uncontrollable storm!");
#endif

					project(PROJECT_WHO_UNCTRL_POWER, 2 + plev / 10, py, px, plev * 2,
						GF_MANA, PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM, -1);
					p_ptr->csp = MAX(0, p_ptr->csp - plev * MAX(1, plev / 10));
				}
			  }
			  if( use_mind == MIND_TIME_LORD ){
				if (b < 61)
				{
				  /* Slow 
				     I jacked up the duration a bit to compensate for the Time-Lord's duration
					 mitigation effects.  Failing to properly control time should not have
					 diminished effects!
				  */
				  set_fast(0, TRUE);
				  set_slow(randint1(50) + 25, FALSE);
				  msg_print("You feel caught in a temporal inversion!");
				}
				else if (b < 81)
				{
					/* Lose XP */
					lose_exp(p_ptr->exp / 4);
					msg_print("You feel life's experiences fade away!");
				}
				else
				{
					/* Lose Stats */
					dec_stat(A_DEX, 10, FALSE);
					dec_stat(A_WIS, 10, FALSE);
					dec_stat(A_CON, 10, FALSE);
					dec_stat(A_STR, 10, FALSE);
					dec_stat(A_CHR, 10, FALSE);
					dec_stat(A_INT, 10, FALSE);
					msg_print("You feel as weak as a newborn kitten!");
				}
			  }
			  if( use_mind == MIND_MIRROR_MASTER ){
				if (b < 51)
				{
				  /* Nothing has happen */
				}
				else if (b < 81)
				{
#ifdef JP
msg_print("鏡の世界の干渉を受けた！");
#else
					msg_print("Weird visions seem to dance before your eyes...");
#endif
					teleport_player(10, TELEPORT_PASSIVE);
				}
				else if (b < 96)
				{
#ifdef JP
msg_print("まわりのものがキラキラ輝いている！");
#else
					msg_print("Your brain is addled!");
#endif

					set_image(p_ptr->image + 5 + randint1(10), FALSE);
				}
				else
				{
					/* Mana storm */
#ifdef JP
msg_format("%sの力が制御できない氾流となって解放された！", p);
#else
					msg_print("Your mind unleashes its power in an uncontrollable storm!");
#endif

					project(PROJECT_WHO_UNCTRL_POWER, 2 + plev / 10, py, px, plev * 2,
						GF_MANA, PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM, -1);
					p_ptr->csp = MAX(0, p_ptr->csp - plev * MAX(1, plev / 10));
				}
			  }
			}
		}
	}
	else
	{
		sound(SOUND_ZAP);

		switch(use_mind)
		{
		case MIND_MINDCRAFTER:
			/* Cast the spell */
			cast = cast_mindcrafter_spell(n);
			break;
		case MIND_KI:
			/* Cast the spell */
			cast = cast_force_spell(n);
			break;
		case MIND_BERSERKER:
			/* Cast the spell */
			cast = cast_berserk_spell(n);
			break;
		case MIND_MIRROR_MASTER:
			/* Cast the spell */
			if( is_mirror_grid(&cave[py][px]) )on_mirror = TRUE;
			cast = cast_mirror_spell(n);
			break;
		case MIND_NINJUTSU:
			/* Cast the spell */
			cast = cast_ninja_spell(n);
			break;
        case MIND_TIME_LORD:
            cast = cast_time_lord_spell(n);
            break;

        case MIND_BLOOD_KNIGHT:
            cast = cast_blood_knight_spell(n);
            break;

        case MIND_WARLOCK_UNDEAD:
		case MIND_WARLOCK_DRAGON:
		case MIND_WARLOCK_ANGEL:
		case MIND_WARLOCK_DEMON:
		case MIND_WARLOCK_ABERRATION:
            cast = cast_warlock_spell(n);
            break;

		default:
#ifdef JP
			msg_format("謎の能力:%d, %d",use_mind, n);
#else
			msg_format("Mystery power:%d, %d",use_mind, n);
#endif
			return;
		}

		if (!cast) return;
	}


	/* Take a turn */
	energy_use = 100;
	/* teleport from mirror costs small energy */
	if( on_mirror && p_ptr->pclass == CLASS_MIRROR_MASTER )
	{
	  if( n==3 || n==5 || n==7 || n==16 )energy_use = 50;
	}

	if ((use_mind == MIND_BERSERKER) || 
	    (use_mind == MIND_NINJUTSU) || 
		(use_mind == MIND_BLOOD_KNIGHT) ||
		(use_mind == MIND_WARLOCK_UNDEAD) ||
		(use_mind == MIND_WARLOCK_DRAGON) ||
		(use_mind == MIND_WARLOCK_ANGEL) ||
		(use_mind == MIND_WARLOCK_DEMON) ||
		(use_mind == MIND_WARLOCK_ABERRATION))
	{
		if (mana_cost > 0)
		{
#ifdef JP
			take_hit(DAMAGE_USELIFE, mana_cost, "過度の集中", -1);
#else
			take_hit(DAMAGE_USELIFE, mana_cost, "concentrating too hard", -1);
#endif
			/* Redraw hp */
			p_ptr->redraw |= (PR_HP);
		}

		/* Blood Knights are cut by their spells! */
		if (use_mind == MIND_BLOOD_KNIGHT)
		{
			int cut = spell.min_lev;
			set_cut(p_ptr->cut + cut, FALSE);
			p_ptr->update |= PU_BONUS;
		}
	}

	/* Sufficient mana */
	else if (mana_cost <= old_csp)
	{
		/* Use some mana */
		p_ptr->csp -= mana_cost;

		/* Limit */
		if (p_ptr->csp < 0) p_ptr->csp = 0;

		if ((use_mind == MIND_MINDCRAFTER) && (n == 13))
		{
			/* No mana left */
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
		}

        if ((use_mind == MIND_TIME_LORD) && (n == 14))
        {
			/* No mana left */
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
        }
	}

	/* Over-exert the player */
	else
	{
		int oops = mana_cost - old_csp;

		/* No mana left */
		if ((p_ptr->csp - mana_cost) < 0) p_ptr->csp_frac = 0;
		p_ptr->csp = MAX(0, p_ptr->csp - mana_cost);

		/* Message */
#ifdef JP
msg_format("%sを集中しすぎて気を失ってしまった！",p);
#else
		msg_print("You faint from the effort!");
#endif


		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint1(5 * oops + 1), FALSE);

		/* Damage WIS (possibly permanently) */
		if (randint0(100) < 50)
		{
			bool perm = (randint0(100) < 25);

			/* Message */
#ifdef JP
msg_print("自分の精神を攻撃してしまった！");
#else
			msg_print("You have damaged your mind!");
#endif


			/* Reduce constitution */
			(void)dec_stat(A_WIS, 15 + randint1(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
void do_cmd_mind_browse(void)
{
	int n = 0;
	int j, line;
	char temp[62*5];
	int use_mind = 0;

	if (p_ptr->pclass == CLASS_MINDCRAFTER) use_mind = MIND_MINDCRAFTER;
	else if (p_ptr->pclass == CLASS_FORCETRAINER) use_mind = MIND_KI;
	else if (p_ptr->pclass == CLASS_BERSERKER) use_mind = MIND_BERSERKER;
	else if (p_ptr->pclass == CLASS_NINJA) use_mind = MIND_NINJUTSU;
	else if (p_ptr->pclass == CLASS_MIRROR_MASTER) use_mind = MIND_MIRROR_MASTER;
	else if (p_ptr->pclass == CLASS_TIME_LORD) use_mind = MIND_TIME_LORD;
	else if (p_ptr->pclass == CLASS_BLOOD_KNIGHT) use_mind = MIND_BLOOD_KNIGHT;
	else if (p_ptr->pclass == CLASS_WARLOCK)
	{
		switch (p_ptr->psubclass)
		{
		case PACT_UNDEAD: use_mind = MIND_WARLOCK_UNDEAD; break;
		case PACT_DRAGON: use_mind = MIND_WARLOCK_DRAGON; break;
		case PACT_ANGEL: use_mind = MIND_WARLOCK_ANGEL; break;
		case PACT_DEMON: use_mind = MIND_WARLOCK_DEMON; break;
		case PACT_ABERRATION: use_mind = MIND_WARLOCK_ABERRATION; break;
		}
	}

	screen_save();

	while(1)
	{
		/* get power */
		if (!get_mind_power(&n, TRUE))
		{
			screen_load();
			return;
		}

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(12, 21, 255);
		Term_erase(12, 20, 255);
		Term_erase(12, 19, 255);
		Term_erase(12, 18, 255);
		Term_erase(12, 17, 255);
		Term_erase(12, 16, 255);

		roff_to_buf(mind_tips[use_mind][n], 62, temp, sizeof(temp));
		for(j=0, line = 18;temp[j];j+=(1+strlen(&temp[j])))
		{
			prt(&temp[j], line, 15);
			line++;
		}
		switch (use_mind)
		{
		case MIND_MIRROR_MASTER:
		case MIND_NINJUTSU:
#ifdef JP
		  prt("何かキーを押して下さい。",0,0);
#else
		  prt("Hit any key.",0,0);
#endif
		  (void)inkey();
		}
	}
}
