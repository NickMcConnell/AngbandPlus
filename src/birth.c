/* File: birth.c */

/* Purpose: create a player character */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * How often the autoroller will update the display and pause
 * to check for user interuptions.
 * Bigger values will make the autoroller faster, but slower
 * system may have problems because the user can't stop the
 * autoroller for this number of rolls.
 */
#define AUTOROLLER_STEP		5431L

/*
 * Maximum number of tries for selection of a proper quest monster
 */
#define MAX_TRIES 100

/*
 * A structure to hold "rolled" information
 */
typedef struct hist_type hist_type;

/*
 * Player background information
 */
struct hist_type
{
	cptr info;			    /* Textual History */

	byte roll;			    /* Frequency of this entry */
	byte chart;			    /* Chart index */
	byte next;			    /* Next chart index */
	byte bonus;			    /* Social Class Bonus + 50 */
};

/*
 * Background information (see below)
 *
 * Chart progression by race:
 *   Human         -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Elf      -->  4 -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Elf/High-Elf  -->  7 -->  8 -->  9 --> 54 --> 55 --> 56
 *   Hobbit        --> 10 --> 11 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Gnome         --> 13 --> 14 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Dwarf         --> 16 --> 17 --> 18 --> 57 --> 58 --> 59 --> 60 --> 61
 *   Half-Orc      --> 19 --> 20 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Troll    --> 22 --> 23 --> 62 --> 63 --> 64 --> 65 --> 66
 *
 * XXX XXX XXX This table *must* be correct or drastic errors may occur!
 */
static hist_type bg[] =
{
#ifdef JP
	{"妾腹の子で認知すらされていません。",			 10, 2, 3, 25},
	{"妾腹の子ですが認知はされています。",			 20, 2, 3, 35},
	{"幾人かの子供のうちの一人です。",			 95, 2, 3, 45},
	{"長子です。",						100, 2, 3, 50},
#else
	{"You are the illegitimate and unacknowledged child ",   10, 1, 2, 25},
	{"You are the illegitimate but acknowledged child ",     20, 1, 2, 35},
	{"You are one of several children ",                     95, 1, 2, 45},
	{"You are the first child ",                            100, 1, 2, 50},
#endif

#ifdef JP
	{"あなたは農奴の",					 40, 1, 2, 65},
	{"あなたは自作農の",					 65, 1, 2, 80},
	{"あなたは町人の",					 80, 1, 2, 90},
	{"あなたは職人の",					 90, 1, 2,105},
	{"あなたは土着の騎士の",				 96, 1, 2,120},
	{"あなたは混沌の宮廷の爵位ある貴族の",				 100, 1, 2,130},
#else
	{"of a Serf.  ",                                         40, 2, 3, 65},
	{"of a Yeoman.  ",                                       65, 2, 3, 80},
	{"of a Townsman.  ",                                     80, 2, 3, 90},
	{"of a Guildsman.  ",                                    90, 2, 3, 105},
	{"of a Landed Knight.  ",                                96, 2, 3, 120},
	{"of a Noble Family in the Courts of Chaos.  ",         100, 2, 3, 130},
#endif

#ifdef JP
	{"あなたは一家のお荷物です。",				 20, 3,50, 20},
	{"あなたは一家の誇りです。",				 80, 3,50, 55},
	{"あなたは家族に大切にされています。",			100, 3,50, 60},
#else
	{"You are the black sheep of the family.  ",             20, 3, 50, 20},
	{"You are a credit to the family.  ",                    80, 3, 50, 55},
	{"You are a well liked child.  ",                       100, 3, 50, 60},
#endif

#ifdef JP
	{"あなたの母はテレリ族のエルフでした。",		 40, 4, 1, 50},
	{"あなたの父はテレリ族のエルフでした。",		 75, 4, 1, 55},
	{"あなたの母はノルドール族のエルフでした。",		 90, 4, 1, 55},
	{"あなたの父はノルドール族のエルフでした。",		 95, 4, 1, 60},
	{"あなたの母はヴァンヤール族のエルフでした。",		 98, 4, 1, 65},
	{"あなたの父はヴァンヤール族のエルフでした。",				100, 4, 1, 70},
#else
	{"Your mother was of the Teleri.  ",			 40, 4, 1, 50},
	{"Your father was of the Teleri.  ",			 75, 4, 1, 55},
	{"Your mother was of the Noldor.  ",		 	 90, 4, 1, 55},
	{"Your father was of the Noldor.  ",		 	 95, 4, 1, 60},
	{"Your mother was of the Vanyar.  ",			 98, 4, 1, 65},
	{"Your father was of the Vanyar.  ",			100, 4, 1, 70},
#endif

#ifdef JP
	{"幾人かの子供のうちの一人です。",			 60, 9, 54, 50},
	{"一粒種です。",					100, 9, 54, 55},
#else
	{"You are one of several children ",			 60, 7, 8, 50},
	{"You are the only child ",					100, 7, 8, 55},
#endif

#ifdef JP
	{"あなたはテレリ族のエルフの",				 75, 7, 8, 50},
	{"あなたはノルドール族のエルフの",			 95, 7, 8, 55},
	{"あなたはヴァンヤール族のエルフの",			100, 7, 8, 60},
#else
	{"of a Teleri ",						 75, 8, 9, 50},
	{"of a Noldor ",						 95, 8, 9, 55},
	{"of a Vanyar ",						100, 8, 9, 60},
#endif

#ifdef JP
	{"レンジャーの",					 40, 8,9, 80},
	{"アーチャーの",					 70, 8,9, 90},
	{"戦士の",						 87, 8,9,110},
	{"メイジの",						 95, 8,9,125},
	{"王子の",						 99, 8,9,140},
	{"王の",						100, 8,9,145},
#else
	{"Ranger.  ",						 40, 9, 54, 80},
	{"Archer.  ",						 70, 9, 54, 90},
	{"Warrior.  ",						 87, 9, 54, 110},
	{"Mage.  ",							 95, 9, 54, 125},
	{"Prince.  ",						 99, 9, 54, 140},
	{"King.  ",							100, 9, 54, 145},
#endif

#ifdef JP
	{"ホビットの何人かの子供のうちの一人です。",		 85,11,3, 45},
	{"ホビットの一粒種です。",			        100,11,3, 55},
#else
	{"You are one of several children of a Hobbit ",		 85, 10, 11, 45},
	{"You are the only child of a Hobbit ",		        100, 10, 11, 55},
#endif

#ifdef JP
	{"あなたは乞食の",							 20,10,11, 55},
	{"あなたは居酒屋の店主の",						 30,10,11, 80},
	{"あなたは粉屋の",							 40,10,11, 90},
	{"あなたは家主の",							 50,10,11,100},
	{"あなたは盗人の",							 80,10,11,110},
	{"あなたは戦士の",							 95,10,11,115},
	{"あなたはメイジの",							 99,10,11,125},
	{"あなたは一族の長の",							100,10,11,140},
#else
	{"Bum.  ",							 20, 11, 3, 55},
	{"Tavern Owner.  ",						 30, 11, 3, 80},
	{"Miller.  ",						 40, 11, 3, 90},
	{"Home Owner.  ",						 50, 11, 3, 100},
	{"Burglar.  ",						 80, 11, 3, 110},
	{"Warrior.  ",						 95, 11, 3, 115},
	{"Mage.  ",							 99, 11, 3, 125},
	{"Clan Elder.  ",						100, 11, 3, 140},
#endif

#ifdef JP
	{"ノームの幾人かの子供のうちの一人です。",		 85,14,3, 45},
	{"ノームの一粒種です。",			        100,14,3, 55},
#else
	{"You are one of several children of a Gnome ",		 85, 13, 14, 45},
	{"You are the only child of a Gnome ",			100, 13, 14, 55},
#endif

#ifdef JP
	{"あなたは物乞いの",							 20,13,14, 55},
	{"あなたはホラ吹きの",							 50,13,14, 70},
	{"あなたはお調子者の",							 75,13,14, 85},
	{"あなたは戦士の",							 95,13,14,100},
	{"あなたはメイジの",							100,13,14,125},
#else
	{"Beggar.  ",						 20, 14, 3, 55},
	{"Braggart.  ",						 50, 14, 3, 70},
	{"Prankster.  ",						 75, 14, 3, 85},
	{"Warrior.  ",						 95, 14, 3, 100},
	{"Mage.  ",							100, 14, 3, 125},
#endif

#ifdef JP
	{"ドワーフの二人の子供のうちの一人です。",		 25,17,18, 40},
	{"ドワーフの一粒種です。",			        100,17,18, 50},
#else
	{"You are one of two children of a Dwarven ",		 25, 16, 17, 40},
	{"You are the only child of a Dwarven ",			100, 16, 17, 50},
#endif

#ifdef JP
	{"あなたは泥棒の",							 10,16,17, 60},
	{"あなたは牢番の",							 25,16,17, 75},
	{"あなたは坑夫の",							 75,16,17, 90},
	{"あなたは戦士の",							 90,16,17,110},
	{"あなたはプリーストの",					 99,16,17,130},
	{"あなたは王の",							100,16,17,150},
#else
	{"Thief.  ",						 10, 17, 18, 60},
	{"Prison Guard.  ",						 25, 17, 18, 75},
	{"Miner.  ",						 75, 17, 18, 90},
	{"Warrior.  ",						 90, 17, 18, 110},
	{"Priest.  ",						 99, 17, 18, 130},
	{"King.  ",							100, 17, 18, 150},
#endif

#ifdef JP
	{"あなたは一家のお荷物です。",						 15,18,57,10},
	{"あなたは一家の誇りです。",						 85,18,57, 50},
	{"あなたは家族に大切にされています。",				100,18,57, 55},
#else
	{"You are the black sheep of the family.  ",		 15, 18, 57, 10},
	{"You are a credit to the family.  ",			 85, 18, 57, 50},
	{"You are a well liked child.  ",				100, 18, 57, 55},
#endif

#ifdef JP
	{"あなたの母はオークでしたが、それは秘密にされています。",	 25,19,20, 25},
	{"あなたの父はオークでしたが、それは秘密にされています。",	100,19,20, 25},
#else
	{"Your mother was an Orc, but it is unacknowledged.  ",	 25, 19, 20, 25},
	{"Your father was an Orc, but it is unacknowledged.  ",	100, 19, 20, 25},
#endif

#ifdef JP
	{"あなたは農奴の養子です。",						 40,20, 3, 65},
	{"あなたは自作農の養子です。",						 65,20, 3, 80},
	{"あなたは町人の養子です。",						 80,20, 3, 90},
	{"あなたは職人の養子です。",						 90,20, 3,105},
	{"あなたは土着の騎士の養子です。",					 96,20, 3,120},
	{"あなたは爵位ある貴族の養子です。",				 99,20, 3,130},
	{"あなたは王家の血を引く者の養子です。",			100,20, 3,140},
#else
	{"You are the adopted child ",				100, 20, 2, 50},
#endif

#ifdef JP
	{"あなたの母は洞窟トロルの",				 30,22,23, 20},
	{"あなたの父は洞窟トロルの",				 60,22,23, 25},
	{"あなたの母は丘トロルの",					 75,22,23, 30},
	{"あなたの父は丘トロルの",					 90,22,23, 35},
	{"あなたの母は水トロルの",					 95,22,23, 40},
	{"あなたの父は水トロルの",					100,22,23, 45},
#else
	{"Your mother was a Cave-Troll ",				 30, 22, 23, 20},
	{"Your father was a Cave-Troll ",				 60, 22, 23, 25},
	{"Your mother was a Hill-Troll ",				 75, 22, 23, 30},
	{"Your father was a Hill-Troll ",				 90, 22, 23, 35},
	{"Your mother was a Water-Troll ",				 95, 22, 23, 40},
	{"Your father was a Water-Troll ",				100, 22, 23, 45},
#endif

#ifdef JP
	{"コックでした。",							  5,23,62, 60},
	{"戦士でした。",							 95,23,62, 55},
	{"呪術師でした。",							 99,23,62, 65},
	{"一族の長でした。",						100,23,62, 80},
#else
	{"Cook.  ",							  5, 23, 62, 60},
	{"Warrior.  ",						 95, 23, 62, 55},
	{"Shaman.  ",						 99, 23, 62, 65},
	{"Clan Chief.  ",						100, 23, 62, 80},
#endif

#ifdef JP
	{"あなたは深いブラウンの瞳と",				 20,50,51, 50},
	{"あなたはブラウンの瞳と",					 60,50,51, 50},
	{"あなたは淡い色の瞳と",					 70,50,51, 50},
	{"あなたはグリーンの瞳と",					 80,50,51, 50},
	{"あなたは青い瞳と",						 90,50,51, 50},
	{"あなたはブルーグレイの瞳と",				100,50,51, 50},
#else
	{"You have dark brown eyes, ",				 20, 50, 51, 50},
	{"You have brown eyes, ",					 60, 50, 51, 50},
	{"You have hazel eyes, ",					 70, 50, 51, 50},
	{"You have green eyes, ",					 80, 50, 51, 50},
	{"You have blue eyes, ",					 90, 50, 51, 50},
	{"You have blue-gray eyes, ",				100, 50, 51, 50},
#endif

#ifdef JP
	{"なめらかな",							 70,51,52, 50},
	{"波打った",							 90,51,52, 50},
	{"カールした",							100,51,52, 50},
#else
	{"straight ",						 70, 51, 52, 50},
	{"wavy ",							 90, 51, 52, 50},
	{"curly ",							100, 51, 52, 50},
#endif

#ifdef JP
	{"黒髪を持ち、",						 30,52,53, 50},
	{"茶髪を持ち、",						 70,52,53, 50},
	{"とび色の髪を持ち、",					 80,52,53, 50},
	{"赤い髪を持ち、",						 90,52,53, 50},
	{"ブロンドの髪を持ち、",				100,52,53, 50},
#else
	{"black hair, ",						 30, 52, 53, 50},
	{"brown hair, ",						 70, 52, 53, 50},
	{"auburn hair, ",						 80, 52, 53, 50},
	{"red hair, ",						 90, 52, 53, 50},
	{"blond hair, ",						100, 52, 53, 50},
#endif

#ifdef JP
	{"漆黒の肌をしています。",					 10,53, 0, 50},
	{"黒い肌をしています。",					 30,53, 0, 50},
	{"普通の肌色をしています。",				 80,53, 0, 50},
	{"白い肌をしています。",					 90,53, 0, 50},
	{"透き通るような白い肌をしています。",		100,53, 0, 50},
#else
	{"and a very dark complexion.",				 10, 53, 0, 50},
	{"and a dark complexion.",					 30, 53, 0, 50},
	{"and an average complexion.",				 80, 53, 0, 50},
	{"and a fair complexion.",					 90, 53, 0, 50},
	{"and a very fair complexion.",				100, 53, 0, 50},
#endif

#ifdef JP
	{"あなたは明るいグレーの瞳と",					 85,54,55, 50},
	{"あなたは明るいブルーの瞳と",					 95,54,55, 50},
	{"あなたは明るいグリーンの瞳と",					100,54,55, 50},
#else
	{"You have light grey eyes, ",				 85, 54, 55, 50},
	{"You have light blue eyes, ",				 95, 54, 55, 50},
	{"You have light green eyes, ",				100, 54, 55, 50},
#endif

#ifdef JP
	{"なめらかな",							 75,55,56, 50},
	{"波打った",							100,55,56, 50},
#else
	{"straight ",						 75, 55, 56, 50},
	{"wavy ",							100, 55, 56, 50},
#endif

#ifdef JP
	{"黒髪を持ち、白い肌をしています。",				 75,56, 0, 50},
	{"茶髪を持ち、白い肌をしています。",				 85,56, 0, 50},
	{"ブロンドの髪を持ち、白い肌をしています。",				 95,56, 0, 50},
	{"銀髪を持ち、白い肌をしています。",				100,56, 0, 50},
#else
	{"black hair, and a fair complexion.",			 75, 56, 0, 50},
	{"brown hair, and a fair complexion.",			 85, 56, 0, 50},
	{"blond hair, and a fair complexion.",			 95, 56, 0, 50},
	{"silver hair, and a fair complexion.",			100, 56, 0, 50},
#endif

#ifdef JP
	{"あなたは深いブラウンの瞳と",					 99,57,58, 50},
	{"あなたは輝く赤い瞳と",					100,57,58, 60},
#else
	{"You have dark brown eyes, ",				 99, 57, 58, 50},
	{"You have glowing red eyes, ",				100, 57, 58, 60},
#endif

#ifdef JP
	{"なめらかな",							 90,58,59, 50},
	{"波打った",							100,58,59, 50},
#else
	{"straight ",						 90, 58, 59, 50},
	{"wavy ",							100, 58, 59, 50},
#endif

#ifdef JP
	{"黒髪、そして",						 75,59,60, 50},
	{"茶髪、そして",						100,59,60, 50},
#else
	{"black hair, ",						 75, 59, 60, 50},
	{"brown hair, ",						100, 59, 60, 50},
#endif

#ifdef JP
	{" 30cm ほどのヒゲを持ち、",						 25,60,61, 50},
	{" 60cm ほどのヒゲを持ち、",						 60,60,61, 51},
	{" 90cm ほどのヒゲを持ち、",					 90,60,61, 53},
	{" 1m20cm ほどのヒゲを持ち、 ",						100,60,61, 55},
#else
	{"a one foot beard, ",					 25, 60, 61, 50},
	{"a two foot beard, ",					 60, 60, 61, 51},
	{"a three foot beard, ",					 90, 60, 61, 53},
	{"a four foot beard, ",					100, 60, 61, 55},
#endif

#ifdef JP
	{"黒い肌をしています。",					100,61, 0, 50},
#else
	{"and a dark complexion.",					100, 61, 0, 50},
#endif

#ifdef JP
	{"あなたはベトつくような緑の瞳と",					 60,62,63, 50},
	{"あなたは汚物のような黄色い瞳と",					 85,62,63, 50},
	{"あなたは青く血走った瞳と",				 99,62,63, 50},
	{"あなたは輝く赤い瞳と",					100,62,63, 55},
#else
	{"You have slime green eyes, ",				 60, 62, 63, 50},
	{"You have puke yellow eyes, ",				 85, 62, 63, 50},
	{"You have blue-bloodshot eyes, ",				 99, 62, 63, 50},
	{"You have glowing red eyes, ",				100, 62, 63, 55},
#endif

#ifdef JP
	{"汚らしい",							 33,63,64, 50},
	{"不潔な",							 66,63,64, 50},
	{"脂ぎった",							100,63,64, 50},
#else
	{"dirty ",							 33, 63, 64, 50},
	{"mangy ",							 66, 63, 64, 50},
	{"oily ",							100, 63, 64, 50},
#endif

#ifdef JP
	{"ワカメの様な髪を持ち、",					 33,64,65, 50},
	{"明るい赤色の髪を持ち、",						 66,64,65, 50},
	{"暗い紫色の髪を持ち、",						100,64,65, 50},
#else
	{"sea-weed green hair, ",					 33, 64, 65, 50},
	{"bright red hair, ",					 66, 64, 65, 50},
	{"dark purple hair, ",					100, 64, 65, 50},
#endif

#ifdef JP
	{"緑色の",							 25,65,66, 50},
	{"青い",							 50,65,66, 50},
	{"白い",							 75,65,66, 50},
	{"黒い",							100,65,66, 50},
#else
	{"and green ",						 25, 65, 66, 50},
	{"and blue ",						 50, 65, 66, 50},
	{"and white ",						 75, 65, 66, 50},
	{"and black ",						100, 65, 66, 50},
#endif

#ifdef JP
	{"ブツブツした肌をしています。",						 33,66, 0, 50},
	{"カサブタだらけの肌をしています。",						 66,66, 0, 50},
	{"ガサガサの肌をしています。",						100,66, 0, 50},
#else
	{"ulcerous skin.",						 33, 66, 0, 50},
	{"scabby skin.",						 66, 66, 0, 50},
	{"leprous skin.",                       100, 66, 0, 50},
#endif

#ifdef JP
	{"何人かの子供のうちの一人です。",      85, 70, 71, 45},
	{"一粒種です。",         	 100, 70, 71, 55},

	{"あなたはダークエルフの戦士の", 50, 69, 70, 60 },
	{"あなたはダークエルフの魔術士の", 80, 69, 70, 75 },
	{"あなたはダークエルフの貴族の", 100,  69, 70, 95 },
#else
	{"You are one of several children of a Dark Elven ",      85, 69, 70, 45},
	{"You are the only child of a Dark Elven ",          100, 69, 70, 55},

	{"Warrior.  ", 50, 70, 71, 60 },
	{"Warlock.  ", 80, 70, 71, 75 },
	{"Noble.  ", 100, 70, 71, 95 },
#endif

#ifdef JP
	{"あなたは黒い瞳と", 100, 71, 72, 50},
#else
	{"You have black eyes, ", 100, 71, 72, 50},
#endif

#ifdef JP
	{"なめらかな",                        70, 72, 73, 50},
	{"波打った",                            90, 72, 73, 50},
	{"カールした",                          100, 72, 73, 50},

	{"黒い髪、そしてとても暗い色の肌をしています。", 100, 73, 0, 50 },
#else
	{"straight ",                        70, 72, 73, 50},
	{"wavy ",                            90, 72, 73, 50},
	{"curly ",                          100, 72, 73, 50},

	{"black hair and a very dark complexion.", 100, 73, 0, 50 },
#endif

#ifdef JP
	{"あなたの母親はオーガでしたが、それは秘密にされています。", 25, 74, 20, 25},
	{"あなたの父親はオーガでしたが、それは秘密にされています。", 100, 74, 20, 25},
#else
	{"Your mother was an Ogre, but it is unacknowledged.  ", 25, 74, 20, 25},
	{"Your father was an Ogre, but it is unacknowledged.  ", 100, 74, 20, 25},
#endif

#ifdef JP
	{"あなたの母親は丘ジャイアントでした。", 10, 75, 20, 50},
	{"あなたの母親はファイアー・ジャイアントでした。", 12, 75, 20, 55},
	{"あなたの母親はフロスト・ジャイアントでした。", 20, 75, 20, 60},
	{"あなたの母親はクラウド・ジャイアントでした。", 23, 75, 20, 65},
	{"あなたの母親はストーム・ジャイアントでした。", 25, 75, 20, 70},
	{"あなたの父親は丘ジャイアントでした。",  60, 75, 20, 50},
	{"あなたの父親はファイアー・ジャイアントでした。",  70, 75, 20, 55},
	{"あなたの父親はフロスト・ジャイアントでした。",  80, 75, 20, 60},
	{"あなたの父親はクラウド・ジャイアントでした。",  90, 75, 20, 65},
	{"あなたの父親はストーム・ジャイアントでした。", 100, 75, 20, 70},
#else
	{"Your mother was a Hill Giant.  ", 10, 75, 20, 50},
	{"Your mother was a Fire Giant.  ", 12, 75, 20, 55},
	{"Your mother was a Frost Giant.  ", 20, 75, 20, 60},
	{"Your mother was a Cloud Giant.  ", 23, 75, 20, 65},
	{"Your mother was a Storm Giant.  ", 25, 75, 20, 70},
	{"Your father was a Hill Giant.  ",  60, 75, 20, 50},
	{"Your father was a Fire Giant.  ",  70, 75, 20, 55},
	{"Your father was a Frost Giant.  ",  80, 75, 20, 60},
	{"Your father was a Cloud Giant.  ",  90, 75, 20, 65},
	{"Your father was a Storm Giant.  ", 100, 75, 20, 70},
#endif

#ifdef JP
	{"あなたの父親は名の知れぬタイタンでした。", 75, 76, 20, 50 },
	{"あなたの母親はテミスでした。",        80, 76, 20, 100 },
	{"あなたの母親はメノシンでした。",     85, 76, 20, 100 },
	{"あなたの父親はオケアノスでした。",      90, 76, 20, 100 },
	{"あなたの父親はクリウスでした。",         95, 76, 20, 100 },
	{"あなたの父親はハイペリオンでした。",      98, 76, 20, 125 },
	{"あなたの父親はクロノスでした。",       100, 76, 20, 150 },
#else
	{"Your father was an unknown Titan.  ", 75, 76, 20, 50 },
	{"Your mother was Themis.  ",        80, 76, 20, 100 },
	{"Your mother was Mnemosyne.  ",     85, 76, 20, 100 },
	{"Your father was Okeanoas.  ",      90, 76, 20, 100 },
	{"Your father was Crius.  ",         95, 76, 20, 100 },
	{"Your father was Hyperion.  ",      98, 76, 20, 125 },
	{"Your father was Kronos.  ",       100, 76, 20, 150 },
#endif

#ifdef JP
	{"あなたは名の知れぬサイクロプスの子孫です。", 90, 77, 109, 50 },
	{"あなたはポリフェモスの子供です。", 98, 77, 109, 80 },
	{"あなたはウラノスの子供です。", 100, 77, 109, 135 },
#else
	{"You are the offspring of an unknown Cyclops.  ", 90, 77, 109, 50 },
	{"You are Polyphemos's child.  ", 98, 77, 109, 80 },
	{"You are Uranos's child.  ", 100, 77, 109, 135 },
#endif

#ifdef JP
	{"何人かの子供のうちの一人です。", 100, 79, 80, 50 },

	{"あなたはブラウン・イークの", 		50, 78, 79, 50 },
	{"あなたはブルー・イークの", 		75, 78, 79, 50 },
	{"あなたはマスター・イークの", 		95, 78, 79, 85 },
	{"あなたはイークの王『ボルドー』の",   100, 78, 79, 120 },
#else
	{"You are one of several children of ", 100, 78, 79, 50 },

	{"a Brown Yeek. ", 50, 79, 80, 50 },
	{"a Blue Yeek.  ", 75, 79, 80, 50 },
	{"a Master Yeek.  ", 95, 79, 80, 85 },
	{"Boldor, the King of the Yeeks.  ", 100, 79, 80, 120 },
#endif

#ifdef JP
	{"あなたは青い瞳と",    25, 80, 81, 50 },
	{"あなたは光る瞳と",    50, 80, 81, 50 },
	{"あなたは小さな黒い瞳と",    75, 80, 81, 50 },
	{"あなたは黒く輝く瞳と",    100, 80, 81, 50 },

	{"髪のない頭、",        20, 81, 65, 50 },
	{"黒く短い髪、",        40, 81, 65, 50 },
	{"黒く長い髪、",        60, 81, 65, 50 },
	{"燃えるような赤い髪、",        80, 81, 65, 50 },
	{"色のない白い髪、",        100, 81, 65, 50 },
#else
	{"You have pale eyes, ",    25, 80, 81, 50 },
	{"You have glowing eyes, ",    50, 80, 81, 50 },
	{"You have tiny black eyes, ",    75, 80, 81, 50 },
	{"You have shining black eyes, ",    100, 80, 81, 50 },

	{"no hair at all, ",        20, 81, 65, 50 },
	{"short black hair, ",        40, 81, 65, 50 },
	{"long black hair, ",        60, 81, 65, 50 },
	{"bright red hair, ",        80, 81, 65, 50 },
	{"colourless albino hair, ",        100, 81, 65, 50 },
#endif

#ifdef JP
	{"の何人かの子供のうちの一人です。 ", 100, 83, 80, 50 },

	{"あなたはスモール・コボルド",   	40, 82, 83, 50 },
	{"あなたはコボルド",         		75, 82, 83, 55 },
	{"あなたはラージ・コボルド",   		95, 82, 83, 65 },
	{"あなたはコボルドの王『ムガシュ』",   100, 82, 83, 100 },
#else
	{"You are one of several children of ", 100, 82, 83, 50 },

	{"a Small Kobold.  ",   40, 83, 80, 50 },
	{"a Kobold.  ",         75, 83, 80, 55 },
	{"a Large Kobold.  ",   95, 83, 80, 65 },
	{"Mughash, the Kobold Lord.  ",     100, 83, 80, 100 },
#endif

#ifdef JP
	{"あなたは女王クラッコンの何人かの子供のうちの一人です。"
	, 100, 84, 85, 50 },

	{"あなたは赤い肌と", 40, 85, 86, 50 },
	{"あなたは黒い肌と", 90, 85, 86, 50 },
	{"あなたは黄色い肌と", 100, 85, 86, 50 },

	{"黒い目をしています。", 100, 86, 0, 50 },
#else
	{"You are one of several children of a Klackon hive queen.  "
	, 100, 84, 85, 50 },

	{"You have red skin, ", 40, 85, 86, 50 },
	{"You have black skin, ", 90, 85, 86, 50 },
	{"You have yellow skin, ", 100, 85, 86, 50 },

	{"and black eyes.", 100, 86, 0, 50 },
#endif

#ifdef JP
	{"の何人かの子供のうちの一人です。", 100, 88, 18, 89 },

	{"あなたはニーベルングの奴隷", 30,	87, 88, 20 },
	{"あなたはニーベルングの盗賊", 50, 	87, 88, 40 },
	{"あなたはニーベルングの鍛冶屋", 70, 	87, 88, 60 },
	{"あなたはニーベルングの坑夫", 90, 	87, 88, 75 },
	{"あなたはニーベルングのシャーマン", 95,87, 88, 100 },
	{"あなたはニーベルングの王『ミーメ』", 100,87, 88, 100 },
#else
	{"You are one of several children of ", 100, 87, 88, 89 },

	{"a Nibelung Slave.  ", 30, 88, 18, 20 },
	{"a Nibelung Thief.  ", 50, 88, 18, 40 },
	{"a Nibelung Smith.  ", 70, 88, 18, 60 },
	{"a Nibelung Miner.  ", 90, 88, 18, 75 },
	{"a Nibelung Shaman.  ", 95, 88, 18, 100 },
	{"Mime, the Nibelung.  ", 100, 88, 18, 100 },
#endif

#ifdef JP
	{"あなたはドラコニアンの", 100, 89, 90, 50 },

	{"の長子です。", 30, 135, 91, 55 },
	{"の末子です。", 50, 135, 91, 50 },
	{"の養子です。", 55, 135, 91, 50 },
	{"の孤児です。", 60, 135, 91, 45 },
	{"の幾人かの子供のうちの一人です。", 85, 135, 91, 50 },
	{"の一粒種です。", 100, 135, 91, 55 },

	{"乞食", 10, 90, 135, 20 },
	{"盗賊", 21, 90, 135, 30 },
	{"水夫", 26, 90, 135, 45 },
	{"傭兵", 42, 90, 135, 45 },
	{"戦士", 73, 90, 135, 50 },
	{"商人", 78, 90, 135, 50 },
	{"職人", 85, 90, 135, 55 },
	{"治療家", 89, 90, 135, 60 },
	{"僧侶", 94, 90, 135, 65 },
	{"魔術師", 97, 90, 135, 70 },
	{"学者", 99, 90, 135, 80 },
	{"貴族", 100, 90, 135, 100 },

	{"あなたは", 100, 91, 136, 50 },

	{"は黒灰色の翼と肌、そして灰色の腹をしています。", 11, 136, 0, 50 },
	{"ブロンズ色の翼と肌、そして銅色の腹をしています。", 16, 136, 0, 50 },
	{"黄金の翼を持ち、黄金の肌をしています。", 24, 136, 0, 50 },
	{"白い翼を持ち、白い肌をしています。", 26, 136, 0, 60 },
	{"青い翼と肌、そして水色の腹をしています。", 32, 136, 0, 50 },
	{"万色の翼を持ち、肌も万色です。", 33, 136, 0, 70 },
	{"茶色の翼を持ち、茶色の肌をしています。", 37, 136, 0, 45 },
	{"黒い翼と肌、そして白い腹をしています。", 41, 136, 0, 50 },
	{"薄紫色の翼と肌、そして白い腹をしています。", 48, 136, 0, 50 },
	{"緑色の翼と肌、そして黄色い腹をしています。", 65, 136, 0, 50 },
	{"緑色の翼を持ち、緑色の肌をしています。", 75, 136, 0, 50 },
	{"赤い翼を持ち、赤い肌をしています。", 88, 136, 0, 50 },
	{"黒い翼を持ち、黒い肌をしています。", 94, 136, 0, 50 },
	{"きらめく翼を持ち、金属的な肌をしています。", 100, 136, 0, 55},
#else
	{"You are ", 100, 89, 135, 50 },

	{"the oldest child of a Draconian ", 30, 135, 90, 55 },
	{"the youngest child of a Draconian ", 50, 135, 90, 50 },
	{"the adopted child of a Draconian ", 55, 135, 90, 50 },
	{"an orphaned child of a Draconian ", 60, 135, 90, 45 },
	{"one of several children of a Draconian ", 85, 135, 90, 50 },
	{"the only child of a Draconian ", 100, 135, 90, 55 },

	{"Beggar.  ", 10, 90, 91, 20 },
	{"Thief.  ", 21, 90, 91, 30 },
	{"Sailor.  ", 26, 90, 91, 45 },
	{"Mercenary.  ", 42, 90, 91, 45 },
	{"Warrior.  ", 73, 90, 91, 50 },
	{"Merchant.  ", 78, 90, 91, 50 },
	{"Artisan.  ", 85, 90, 91, 55 },
	{"Healer.  ", 89, 90, 91, 60 },
	{"Priest.  ", 94, 90, 91, 65 },
	{"Mage.  ", 97, 90, 91, 70 },
	{"Scholar.  ", 99, 90, 91, 80 },
	{"Noble.  ", 100, 90, 91, 100 },

	{"You have ", 100, 91, 136, 50 },

	{"charcoal wings, charcoal skin and a smoke-gray belly.", 11, 136, 0, 50 },
	{"bronze wings, bronze skin, and a copper belly.", 16, 136, 0, 50 },
	{"golden wings, and golden skin.", 24, 136, 0, 50 },
	{"white wings, and white skin.", 26, 136, 0, 60 },
	{"blue wings, blue skin, and a cyan belly.", 32, 136, 0, 50 },
	{"multi-hued wings, and multi-hued skin.", 33, 136, 0, 70 },
	{"brown wings, and brown skin.", 37, 136, 0, 45 },
	{"black wings, black skin, and a white belly.", 41, 136, 0, 50 },
	{"lavender wings, lavender skin, and a white belly.", 48, 136, 0, 50 },
	{"green wings, green skin and yellow belly.", 65, 136, 0, 50 },
	{"green wings, and green skin.", 75, 136, 0, 50 },
	{"red wings, and red skin.", 88, 136, 0, 50 },
	{"black wings, and black skin.", 94, 136, 0, 50 },
	{"metallic skin, and shining wings.", 100, 136, 0, 55},
#endif

#ifdef JP
	{"あなたは偉大なる長老たちの脳味噌が浮かんでいる母なる池でオタマジャクシとして生まれました。あなたはヌルヌルした肌と輝く空虚な目をしていて、", 100, 92, 93, 80 },
	{"口の周りに三本の触手が生えています。", 20, 93, 0, 45 },
	{"口の周りに四本の触手が生えています。", 80, 93, 0, 50 },
	{"口の周りに五本の触手が生えています。", 100, 93, 0, 55 },
#else
	{"You have slimy skin, empty glowing eyes, and ", 100, 92, 93, 80 },
	{"three tentacles around your mouth.", 20, 93, 0, 45 },
	{"four tentacles around your mouth.", 80, 93, 0, 50 },
	{"five tentacles around your mouth.", 100, 93, 0, 55 },
#endif

#ifdef JP
	{"あなたの祖先は", 100, 94, 95, 50 },

	{"心を持たない地獄の低級な生物でした。", 30, 95, 96, 20 },
	{"下級悪魔でした。", 60, 95, 96, 50 },
	{"上級悪魔でした。", 90, 95, 96, 75 },
	{"魔王でした。", 100, 95, 96, 99 },

	{"あなたは赤い肌と", 50, 96, 97, 50 },
	{"あなたは茶色い肌と", 100, 96, 97, 50},

	{"赤く燃える瞳をしていて、鉤爪と牙と刺が生えています。", 40, 97, 0, 50 },
	{"赤く燃える瞳をしていて、鉤爪と牙が生えています。", 70, 97, 0, 50 },
	{"赤く燃える瞳をしていて、鉤爪が生えています。", 100, 97, 0, 50 },
#else
	{"You ancestor was ", 100, 94, 95, 50 },

	{"a mindless demonic spawn.  ", 30, 95, 96, 20 },
	{"a minor demon.  ", 60, 95, 96, 50 },
	{"a major demon.  ", 90, 95, 96, 75 },
	{"a demon lord.  ", 100, 95, 96, 99 },

	{"You have red skin, ", 50, 96, 97, 50 },
	{"You have brown skin, ", 100, 96, 97, 50},

	{"claws, fangs, spikes, and glowing red eyes.", 40, 97, 0, 50 },
	{"claws, fangs, and glowing red eyes.", 70, 97, 0, 50 },
	{"claws, and glowing red eyes.", 100, 97, 0, 50 },
#endif

#ifdef JP
	{"あなたはカバラの秘術によって", 40,	98, 99, 50 },
	{"あなたは魔法使いによって", 65, 	98, 99, 50 },
	{"あなたは錬金術師によって",	     90,	98, 99, 50},
	{"あなたは僧侶によって", 100,	98, 99, 60},

	{"悪と戦うために", 10, 99, 100, 65 },
	{"",		 100,  99, 100, 50 },

	{"粘土から", 40, 100, 101, 50 },
	{"岩石から", 80, 100, 101, 50 },
	{"木から", 85, 100, 101, 40 },
	{"鉄から", 99, 100, 101, 50 },
	{"純金から", 100, 100, 101, 100},

	{"作り出されました。", 100,101, 0, 50 },
#else
	{"You were shaped from ", 100, 98, 99, 50 },

	{"clay ", 40, 99, 100, 50 },
	{"stone ", 80, 99, 100, 50 },
	{"wood ", 85, 99, 100, 40 },
	{"iron ", 99, 99, 100, 50 },
	{"pure gold ", 100, 99, 100, 100},

	{"by a Kabbalist", 40, 100, 101, 50 },
	{"by a Wizard", 65, 100, 101, 50 },
	{"by an Alchemist", 90, 100, 101, 50},
	{"by a Priest", 100, 100, 101, 60},

	{" to fight evil.", 10, 101, 0, 65 },
	{".", 100, 101, 0, 50 },
#endif

#ifdef JP
	{"あなたは", 100, 102, 103, 50 },

	{"死霊術士により作り出されました。", 30, 103, 104, 50 },
	{"魔法の実験により作り出されました。", 50, 103, 104, 50 },
	{"邪悪な僧侶により作り出されました。", 70, 103, 104, 50 },
	{"悪魔との契約により生み出されました。", 75, 103, 104, 50 },
	{"怨霊から生まれました。", 85, 103, 104, 50 },
	{"呪いから生まれました。", 95, 103, 104, 30 },
	{"神名濫用により生み出されました。", 100, 103, 104, 50 },

	{"あなたは", 100, 104, 105, 50 },
	{"古く汚れた骨で出来ていて、", 40, 105, 106, 50 },
	{"腐った黒い骨で出来ていて、", 60, 105, 106, 50 },
	{"うす汚れた茶色い骨で出来ていて、", 80, 105, 106, 50 },
	{"白く輝く骨で出来ていて、", 100, 105, 106, 50 },

	{"光る目をしています。", 30, 106, 0, 50 },
	{"地獄の劫火が燃えさかる目をしています。", 50, 106, 0, 50 },
	{"眼窩はからっぽです。", 100, 106, 0, 50 },
#else
	{"You were created by ", 100, 102, 103, 50 },

	{"a Necromancer.  ", 30, 103, 104, 50 },
	{"a magical experiment.  ", 50, 103, 104, 50 },
	{"an Evil Priest.  ", 70, 103, 104, 50 },
	{"a pact with the demons.  ", 75, 103, 104, 50 },
	{"a restless spirit.  ", 85, 103, 104, 50 },
	{"a curse.  ", 95, 103, 104, 30 },
	{"an oath.  ", 100, 103, 104, 50 },

	{"You have ", 100, 104, 105, 50 },
	{"dirty, dry bones, ", 40, 105, 106, 50 },
	{"rotten black bones, ", 60, 105, 106, 50 },
	{"filthy, brown bones, ", 80, 105, 106, 50 },
	{"shining white bones, ", 100, 105, 106, 50 },

	{"and glowing eyes.", 30, 106, 0, 50 },
	{"and eyes which burn with hellfire.", 50, 106, 0, 50 },
	{"and empty eyesockets.", 100, 106, 0, 50 },
#endif

#ifdef JP
	{"あなたは", 100, 107, 108, 50 },

	{"死霊術士により生み出されました。", 30, 108, 62, 50 },
	{"魔法使いにより生み出されました。", 50, 108, 62, 50 },
	{"怨霊から生まれました。",60, 108, 62, 50 },
	{"邪悪な僧侶により生み出されました。", 70, 108, 62, 50 },
	{"悪魔との契約により生み出されました。", 80, 108, 62, 50 },
	{"呪いから生まれました。", 95, 108, 62, 30 },
	{"神名濫用により生み出されました。", 100, 108, 62, 50 },

	{"あなたは暗褐色の瞳、",               20, 109, 110, 50},
	{"あなたは褐色の瞳、",                    60, 109, 110, 50},
	{"あなたは薄茶色の瞳、",                    70, 109, 110, 50},
	{"あなたは緑色の瞳、",                    80, 109, 110, 50},
	{"あなたは青い瞳、",                     90, 109, 110, 50},
	{"あなたは淡青色の瞳、",               100, 109, 110, 50}, /* tansei.cc.u-tokyoの由来 */

	{"なめらかな",                        70, 110, 111, 50},
	{"波打った",                            90, 110, 111, 50},
	{"カールした",                          100, 110, 111, 50},

	{"黒い髪、",                         30, 111, 112, 50},
	{"茶色い髪、",                         70, 111, 112, 50},
	{"赤茶色の髪、",                        80, 111, 112, 50},
	{"赤い髪、",                       90, 111, 112, 50},
	{"金髪、",                        100, 111, 112, 50},

	{"そしてとても暗い肌をしています。",              10, 112, 0, 50},
	{"そして暗い肌をしています。",                   30, 112, 0, 50},
	{"そして平均的な肌の色をしています。",               80, 112, 0, 50},
	{"そして血色のいい肌をしています。",                   90, 112, 0, 50},
	{"そしてとても血色のいい肌をしています。",             100, 112, 0, 50},
#else
	{"You were created by ", 100, 107, 108, 50 },

	{"a Necromancer.  ", 30, 108, 62, 50 },
	{"a Wizard.  ", 50, 108, 62, 50 },
	{"a restless spirit.  ", 60, 108, 62, 50 },
	{"an Evil Priest.  ", 70, 108, 62, 50 },
	{"a pact with the demons.  ", 80, 108, 62, 50 },
	{"a curse.  ", 95, 108, 62, 30 },
	{"an oath.  ", 100, 108, 62, 50 },

	{"You have a dark brown eye, ",               20, 109, 110, 50},
	{"You have a brown eye, ",                    60, 109, 110, 50},
	{"You have a hazel eye, ",                    70, 109, 110, 50},
	{"You have a green eye, ",                    80, 109, 110, 50},
	{"You have a blue eye, ",                     90, 109, 110, 50},
	{"You have a blue-gray eye, ",               100, 109, 110, 50},

	{"straight ",                        70, 110, 111, 50},
	{"wavy ",                            90, 110, 111, 50},
	{"curly ",                          100, 110, 111, 50},

	{"black hair, ",                         30, 111, 112, 50},
	{"brown hair, ",                         70, 111, 112, 50},
	{"auburn hair, ",                        80, 111, 112, 50},
	{"red hair, ",                       90, 111, 112, 50},
	{"blond hair, ",                        100, 111, 112, 50},

	{"and a very dark complexion.",              10, 112, 0, 50},
	{"and a dark complexion.",                   30, 112, 0, 50},
	{"and an average complexion.",               80, 112, 0, 50},
	{"and a fair complexion.",                   90, 112, 0, 50},
	{"and a very fair complexion.",             100, 112, 0, 50},
#endif

#ifdef JP
	{"あなたは銘のない墓の中から甦りました。", 20, 113, 114, 50 },
	{"あなたは生前ただの百姓でしたが、強大なバンパイア・ロードの餌食となってしまいました。", 40, 113, 114, 50 },
	{"あなたは生前はバンパイア・ハンターでしたが、彼らの餌食となってしまいました。", 60, 113, 114, 50 },
	{"あなたは生前は死霊術士でした。", 80, 113, 114, 50 },
	{"あなたは生前は強大な貴族でした。", 95, 113, 114, 50 },
	{"あなたは生前は強大で残忍な専制君主でした。", 100, 113, 114, 50 },
#else
	{"You arose from an unmarked grave.  ", 20, 113, 114, 50 },
	{"In life you were a simple peasant, the victim of a powerful Vampire Lord.  ", 40, 113, 114, 50 },
	{"In life you were a Vampire Hunter, but they got you.  ", 60, 113, 114, 50 },
	{"In life you were a Necromancer.  ", 80, 113, 114, 50 },
	{"In life you were a powerful noble.  ", 95, 113, 114, 50 },
	{"In life you were a powerful and cruel tyrant.  ", 100, 113, 114, 50 },
#endif

#ifdef JP
	{"あなたは", 100, 114, 115, 50 },

	{"漆黒の髪、", 25, 115, 116, 50 },
	{"もつれたブラウンの髪、", 50, 115, 116, 50 },
	{"白い髪、", 75, 115, 116, 50 },
	{"髪のない頭、", 100, 115, 116, 50 },
#else
	{"You have ", 100, 114, 115, 50 },

	{"jet-black hair, ", 25, 115, 116, 50 },
	{"matted brown hair, ", 50, 115, 116, 50 },
	{"white hair, ", 75, 115, 116, 50 },
	{"a hairless head, ", 100, 115, 116, 50 },
#endif

#ifdef JP
	{"燃える石炭のような瞳、", 25, 116, 117, 50 },
	{"瞳のない目、", 50, 116, 117, 50 },
	{"凶暴な黄色い瞳、", 75, 116, 117, 50 },
	{"血走った赤い瞳、", 100, 116, 117, 50 },

	{"そして死人のように青ざめた肌をしています。", 100, 117, 0, 50 },
#else
	{"eyes like red coals, ", 25, 116, 117, 50 },
	{"blank white eyes, ", 50, 116, 117, 50 },
	{"feral yellow eyes, ", 75, 116, 117, 50 },
	{"bloodshot red eyes, ", 100, 116, 117, 50 },

	{"and a deathly pale complexion.", 100, 117, 0, 50 },
#endif

#ifdef JP
	{"あなたは", 100, 118, 119, 50 },

	{"死霊術士により作り出されました。", 30, 119, 134, 50 },
	{"魔法の実験により作り出されました。", 50, 119, 134, 50 },
	{"邪悪な僧侶により作り出されました。", 70, 119, 134, 50 },
	{"悪魔との契約により生み出されました。", 75, 119, 134, 50 },
	{"怨霊から生まれました。", 85, 119, 134, 50 },
	{"呪いから生まれました。", 95, 119, 134, 30 },
	{"神名濫用により生み出されました。", 100, 119, 134, 50 },
#else
	{"You were created by ", 100, 118, 119, 50 },

	{"a Necromancer.  ", 30, 119, 134, 50 },
	{"a magical experiment.  ", 50, 119, 134, 50 },
	{"an Evil Priest.  ", 70, 119, 134, 50 },
	{"a pact with the demons.  ", 75, 119, 134, 50 },
	{"a restless spirit.  ", 85, 119, 134, 50 },
	{"a curse.  ", 95, 119, 134, 30 },
	{"an oath.  ", 100, 119, 134, 50 },
#endif

#ifdef JP
	{"漆黒の髪、", 25, 120, 121, 50 },
	{"もつれたブラウンの髪、", 50, 120, 121, 50 },
	{"白い髪、", 75, 120, 121, 50 },
	{"髪のない頭、", 100, 120, 121, 50 },
#else
	{"jet-black hair, ", 25, 120, 121, 50 },
	{"matted brown hair, ", 50, 120, 121, 50 },
	{"white hair, ", 75, 120, 121, 50 },
	{"a hairless head, ", 100, 120, 121, 50 },
#endif

#ifdef JP
	{"燃える石炭のような瞳、", 25, 121, 122, 50 },
	{"瞳のない目、", 50, 121, 122, 50 },
	{"凶暴な黄色い瞳、", 75, 121, 122, 50 },
	{"血走った赤い瞳、", 100, 121, 122, 50 },
#else
	{"eyes like red coals, ", 25, 121, 122, 50 },
	{"blank white eyes, ", 50, 121, 122, 50 },
	{"feral yellow eyes, ", 75, 121, 122, 50 },
	{"bloodshot red eyes, ", 100, 121, 122, 50 },
#endif

#ifdef JP
	{"そして死人のような土色の肌をしています。", 100, 122, 123, 50 },
	{"あなたの周りには不気味な緑色のオーラがただよっています。", 100, 123, 0, 50 },
#else
	{" and a deathly gray complexion. ", 100, 122, 123, 50 },
	{"An eerie green aura surrounds you.", 100, 123, 0, 50 },
#endif

#ifdef JP
	{"あなたの両親は", 100, 124, 125, 50 },

	{"ピクシーでした。", 20, 125, 126, 35 },
	{"ニクシーでした。", 30, 125, 126, 25 },
	{"森の妖精でした。", 75, 125, 126, 50 },
	{"森の精霊でした。", 90, 125, 126, 75 },
	{"妖精の貴族でした。", 100, 125, 126, 85 }, 
#else
	{"Your parents were ", 100, 124, 125, 50 },

	{"pixies.  ", 20, 125, 126, 35 },
	{"nixies.  ", 30, 125, 126, 25 },
	{"wood sprites.  ", 75, 125, 126, 50 },
	{"wood spirits.  ", 90, 125, 126, 75 },
	{"noble faerie folk.  ", 100, 125, 126, 85 },
#endif

#ifdef JP
	{"あなたは背中にライトブルーの羽根が生えていて、", 100, 126, 127, 50 },

	{"なめらかな金髪、",                        80, 127, 128, 50},
	{"波打った金髪、",                            100, 127, 128, 50},

	{"青い瞳、そして非常に生き生きとした肌をしています。", 100, 128, 0, 50},
#else
	{"You have light blue wings attached to your back, ", 100, 126, 127, 50 },

	{"straight blond hair, ",                        80, 127, 128, 50},
	{"wavy blond hair, ",                            100, 127, 128, 50},

	{"blue eyes, and a very fair complexion.", 100, 128, 0, 50},
#endif

#ifdef JP
	{"あなたは魔法の実験により生み出されました。", 30, 129, 130, 40},
	{"あなたは子供時代、愚かにも純ログルスに首を突っ込んでしまいました。",
	50, 129, 130, 50 }, 
	{"あるカオスの魔王が遊びであなたを作り上げました。",
	60, 129, 130, 60 },
	{"あなたは魔法により掛け合わされた動物と人間の子供です。", 75, 129, 130, 50},
	{"あなたは言うもおぞましいカオスの生物の冒涜的な掛け合わせにより生まれました。", 100, 129, 130, 30},
#else
	{"You were produced by a magical experiment.  ", 30, 129, 130, 40},
	{"In your childhood, you were stupid enough to stick your head in raw Logrus.  ",
	50, 129, 130, 50 },
	{"A Demon Lord of Chaos decided to have some fun, and so he created you.  ",
	60, 129, 130, 60 },
	{"You are the magical crossbreed of an animal and a man.  ", 75, 129, 130, 50},
	{"You are the blasphemous crossbreed of unspeakable creatures of chaos.  ", 100, 129, 130, 30},
#endif

#ifdef JP
	{"あなたは緑色の爬虫類の目",              60, 130, 131, 50},
	{"あなたは黒い鳥の目",                    85, 130, 131, 50},
	{"あなたはオレンジ色の猫の目",            99, 130, 131, 50},
	{"あなたは燃えるような悪魔の目",          100, 130, 131, 55},
#else
	{"You have green reptilian eyes, ",              60, 130, 131, 50},
	{"You have the black eyes of a bird, ",              85, 130, 131, 50},
	{"You have the orange eyes of a cat, ",               99, 130, 131, 50},
	{"You have the fiery eyes of a demon, ",             100, 130, 131, 55},
#endif

#ifdef JP

	{"と髪のない頭を持ち、",                 10, 131, 133, 50},
	{"をしていて、汚い",                     33, 131, 132, 50},
	{"をしていて、みすぼらしい",             66, 131, 132, 50},
	{"をしていて、てかった",                100, 131, 132, 50},
#else
	{"no hair at all, ",                 10, 131, 133, 50 },
	{"dirty ",                           33, 131, 132, 50},
	{"mangy ",                           66, 131, 132, 50},
	{"oily ",                           100, 131, 132, 50},
#endif

#ifdef JP

	{"茶色の毛皮と",                    33, 132, 133, 50},
	{"灰色の毛皮と",                    66, 132, 133, 50},
	{"白い毛皮と",                     100, 132, 133, 50},
#else
	{"brown fur, ",                    33, 132, 133, 50},
	{"gray fur, ",                    66, 132, 133, 50},
	{"albino fur, ",                  100, 132, 133, 50},
#endif

#ifdef JP
	{"山羊の蹄があります。",      50, 133, 0, 50 },
	{"人間の足が生えています。",  75, 133, 0, 50 },
	{"鳥の足が生えています。",    85, 133, 0, 50 },
	{"爬虫類の足が生えています。",90, 133, 0, 50 },
	{"牛の足が生えています。",    95, 133, 0, 50 },
	{"猫の足が生えています。",    97, 133, 0, 50 },
	{"犬の足が生えています。",   100, 133, 0, 50 },

	{"あなたは", 100, 134, 120, 50 },
#else
	{"and the hooves of a goat.",      50, 133, 0, 50 },
	{"and human feet.",        75, 133, 0, 50 },
	{"and bird's feet.",       85, 133, 0, 50 },
	{"and reptilian feet.",    90, 133, 0, 50 },
	{"and bovine feet.",       95, 133, 0, 50 },
	{"and feline feet.",       97, 133, 0, 50 },
	{"and canine feet.",       100, 133, 0, 50 },

	{"You have ", 100, 134, 120, 50 },
#endif
};

/*
 * Choose from one of the available magical realms
 */
static byte choose_realm(byte choices)
{
	int picks[MAX_REALM] = {0};
	int k, i, n, cs, os;
	int count = 0;
	byte auto_select = REALM_NONE;
	char c;
	char sym[MAX_REALM];
	char p2 = ')';
	char buf[80], cur[80];

	/* Count the choices */
	if (choices & CH_LIFE)
	{
		count++;
		auto_select = REALM_LIFE;
	}
	if (choices & CH_SORCERY)
	{
		count++;
		auto_select = REALM_SORCERY;
	}

	/* Auto-select the realm */
	if (count < 2) return auto_select;

	/* Extra info */
#ifdef JP
	put_str ("注意：魔法の領域の選択によりあなたが習得する呪文のタイプが決まります。", 23, 5);
#else
	put_str ("Note: The realm of magic will determine which spells you can learn.", 23, 5);
#endif

#if 0
	cs = n = 0;
#else
	n = 0;
#endif
	for (i = 0; i<16; i++)
	{
		/* Analize realms */
		if (choices & (1 << i))
		{
#if 0
			if (p_ptr->realm1 == i+1)
			{
				if (p_ptr->realm2 == 255)
					cs = n;
				else
					continue;
			}
			if (p_ptr->realm2 == i+1)
				cs = n;
#endif
			if (n < 26)
				sym[n] = I2A(n);
			else
				sym[n] = ('A' + n - 26);
			sprintf(buf, "%c%c %s", sym[n], p2, realm_names[i+1]);
			put_str(buf, 12 + (n/5), 2 + 15 * (n%5));
			picks[n++] = i+1;
		}
	}
#ifdef JP
	sprintf(cur, "%c%c %s", '*', p2, "ランダム");
#else
	sprintf(cur, "%c%c %s", '*', p2, "Random");
#endif

	/* Get a realm */
	k = -1;
	cs = 0;
	os = n;
	while (1)
	{
		/* Move Cursol */
		if (cs != os)
		{
			c_put_str(TERM_WHITE, cur, 12 + (os/5), 2 + 15 * (os%5));
			put_str("                                   ", 3, 40);
			put_str("                                   ", 4, 40);

			if(cs == n)
			{
#ifdef JP
				sprintf(cur, "%c%c %s", '*', p2, "ランダム");
#else
				sprintf(cur, "%c%c %s", '*', p2, "Random");
#endif
			}
			else
			{
				sprintf(cur, "%c%c %s", sym[cs], p2, realm_names[picks[cs]]);
			}
			c_put_str(TERM_YELLOW, cur, 12 + (cs/5), 2 + 15 * (cs%5));
			os = cs;
		}

		if (k >= 0) break;

#ifdef JP
		sprintf(buf, "領域を選んで下さい(%c-%c) ('='初期オプション設定): ", sym[0], sym[n-1]);
#else
		sprintf(buf, "Choose a realm (%c-%c) ('=' for options): ", sym[0], sym[n-1]);
#endif

		put_str(buf, 10, 10);
		c = inkey();
		if (c == '8')
		{
			if (cs >= 5) cs -= 5;
			continue;
		}
		if (c == '4')
		{
			if (cs > 0) cs--;
			continue;
		}
		if (c == '6')
		{
			if (cs < n) cs++;
			continue;
		}
		if (c == '2')
		{
			if ((cs + 5) <= n) cs += 5;
			continue;
		}
		if (c == ' ' || c == '\r' || c == '\n')
		{
			if(cs == n)
			{
				k = randint0(n);
				break;
			}
			else
			{
				k = cs;
				break;
			}
		}
		if (c == '*')
		{
			k = randint0(n);
			break;
		}
		if (c == 'Q')
		{
			remove_loc();
			quit(NULL);
		}
		if (c == 'S') return 255;
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n))
		{
			cs = k;
			continue;
		}
		k = (isupper(c) ? (26 + c - 'A') : -1);
		if ((k >= 26) && (k < n))
		{
			cs = k;
			continue;
		}
		else k = -1;
		if (c == '?') do_cmd_help();
		else if (c == '=')
		{
			screen_save();
#ifdef JP
			do_cmd_options_aux(6, "初期オプション");
#else
			do_cmd_options_aux(6, "Startup Options");
#endif
			screen_load();
		}
		else bell();
	}

	/* Clean up */
	clear_from(10);

	return (picks[k]);
}


/*
 * Choose the magical realms
 */
static bool get_player_realms(void)
{
	/* Select the first realm */
	p_ptr->realm1 = choose_realm(realm_choices1[p_ptr->pclass]);

	if (255 == p_ptr->realm1) return FALSE;
	if (p_ptr->realm1)
	{
		/* Print the realm */
#ifdef JP
		put_str("魔法        :", 6, 1);
#else
		put_str("Magic       :", 6, 1);
#endif
		c_put_str(TERM_L_BLUE, realm_names[p_ptr->realm1], 6, 15);

		/* Select the second realm */
		p_ptr->realm2 = choose_realm(realm_choices2[p_ptr->pclass]);

		/* Print the realm */
		if (255 == p_ptr->realm2) return FALSE;
		if (p_ptr->realm2)
			c_put_str(TERM_L_BLUE, format("%s, %s", realm_names[p_ptr->realm1], realm_names[p_ptr->realm2]), 6, 15);
	}

	return (TRUE);
}


/*
 * Save the current data for later
 */
static void save_prev_data(birther *birther_ptr)
{
	int i;

	/* Save the data */
	birther_ptr->psex = p_ptr->psex;
	birther_ptr->prace = p_ptr->prace;
	birther_ptr->pclass = p_ptr->pclass;
	birther_ptr->realm1 = p_ptr->realm1;
	birther_ptr->realm2 = p_ptr->realm2;
	birther_ptr->age = p_ptr->age;
	birther_ptr->ht = p_ptr->ht;
	birther_ptr->wt = p_ptr->wt;
	birther_ptr->sc = p_ptr->sc;
	birther_ptr->au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < 6; i++)
	{
		birther_ptr->stat_max[i] = p_ptr->stat_max[i];
	}

	/* Save the hp */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		birther_ptr->player_hp[i] = player_hp[i];
	}

	birther_ptr->valar_patron = p_ptr->valar_patron;

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(birther_ptr->history[i], history[i]);
	}
}


/*
 * Load the previous data
 */
static void load_prev_data(bool swap)
{
	int i;

	birther	temp;

	/*** Save the current data ***/
	if (swap) save_prev_data(&temp);


	/*** Load the previous data ***/

	/* Load the data */
	p_ptr->psex = previous_char.psex;
	p_ptr->prace = previous_char.prace;
	p_ptr->pclass = previous_char.pclass;
	p_ptr->realm1 = previous_char.realm1;
	p_ptr->realm2 = previous_char.realm2;
	p_ptr->age = previous_char.age;
	p_ptr->ht = previous_char.ht;
	p_ptr->wt = previous_char.wt;
	p_ptr->sc = previous_char.sc;
	p_ptr->au = previous_char.au;

	/* Load the stats */
	for (i = 0; i < 6; i++)
	{
		p_ptr->stat_cur[i] = p_ptr->stat_max[i] = previous_char.stat_max[i];
	}

	/* Load the hp */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		player_hp[i] = previous_char.player_hp[i];
	}
	p_ptr->mhp = player_hp[0];
	p_ptr->chp = player_hp[0];

	p_ptr->valar_patron = previous_char.valar_patron;

	/* Load the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(history[i], previous_char.history[i]);
	}

	/*** Save the previous data ***/
	if (swap)
	{
		COPY(&previous_char, &temp, birther);
	}
}

/*
 * Returns adjusted stat -JK-  Algorithm by -JWT-
 *
 * auto_roll is boolean and states maximum changes should be used rather
 * than random ones to allow specification of higher values to wait for
 *
 * The "maximize" code is important	-BEN-
 */
static int adjust_stat(int value, int amount, int auto_roll)
{
	int i;

	/* Negative amounts */
	if (amount < 0)
	{
		/* Apply penalty */
		for (i = 0; i < (0 - amount); i++)
		{
			if (value >= 18+10)
			{
				value -= 10;
			}
			else if (value > 18)
			{
				value = 18;
			}
			else if (value > 3)
			{
				value--;
			}
		}
	}

	/* Positive amounts */
	else if (amount > 0)
	{
		/* Apply reward */
		for (i = 0; i < amount; i++)
		{
			if (value < 18)
			{
				value++;
			}
			else
			{
				value += 10;
			}
		}
	}

	/* Return the result */
	return (value);
}

/*
 * Roll for a characters stats
 *
 * For efficiency, we include a chunk of "calc_bonuses()".
 */
static void get_stats(void)
{
	/* Roll and verify some stats */
	while (TRUE)
	{
		int i;
		int sum = 0;

		/* Roll some dice */
		for (i = 0; i < 2; i++)
		{
			s32b tmp = randint0(60*60*60);
			int val;

			/* Extract 5 + 1d3 + 1d4 + 1d5 */
			val = 5 + 3;
			val += tmp % 3; tmp /= 3;
			val += tmp % 4; tmp /= 4;
			val += tmp % 5; tmp /= 5;

			/* Save that value */
			sum += val;
			p_ptr->stat_cur[3*i] = p_ptr->stat_max[3*i] = val;

			/* Extract 5 + 1d3 + 1d4 + 1d5 */
			val = 5 + 3;
			val += tmp % 3; tmp /= 3;
			val += tmp % 4; tmp /= 4;
			val += tmp % 5; tmp /= 5;

			/* Save that value */
			sum += val;
			p_ptr->stat_cur[3*i+1] = p_ptr->stat_max[3*i+1] = val;

			/* Extract 5 + 1d3 + 1d4 + 1d5 */
			val = 5 + 3;
			val += tmp % 3; tmp /= 3;
			val += tmp % 4; tmp /= 4;
			val += tmp;

			/* Save that value */
			sum += val;
			p_ptr->stat_cur[3*i+2] = p_ptr->stat_max[3*i+2] = val;
		}

		/* Verify totals */
		if ((sum > 42+5*6) && (sum < 57+5*6)) break;
		/* 57 was 54... I hate 'magic numbers' :< TY */
	}
}

/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(bool roll_hitdie)
{
	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->hitdie;

	/* Roll for hit point unless quick-start */
	if (roll_hitdie)
	{
		do_cmd_rerate(FALSE);
	}
}

/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history(void)
{
	int i, n, chart, roll, social_class;

	char *s, *t;

	char buf[240];

	/* Clear the previous history strings */
	for (i = 0; i < 4; i++) history[i][0] = '\0';

	/* Clear the history text */
	buf[0] = '\0';

	/* Initial social class */
	social_class = randint1(4);

	/* Starting place */
	switch (p_ptr->prace)
	{
		case RACE_HUMAN:
		case RACE_DUNADAN:
		case RACE_BARBARIAN:
		{
			chart = 1;
			break;
		}
		case RACE_HALF_ELF:
		{
			chart = 4;
			break;
		}
		case RACE_ELF:
		case RACE_HIGH_ELF:
		{
			chart = 7;
			break;
		}
		case RACE_HOBBIT:
		{
			chart = 10;
			break;
		}
		case RACE_DWARF:
		{
			chart = 16;
			break;
		}
		case RACE_HALF_ORC:
		{
			chart = 19;
			break;
		}
		default:
		{
			chart = 0;
			break;
		}
	}


	/* Process the history */
	while (chart)
	{
		/* Start over */
		i = 0;

		/* Roll for nobility */
		roll = randint1(100);


		/* Access the proper entry in the table */
		while ((chart != bg[i].chart) || (roll > bg[i].roll)) i++;

		/* Acquire the textual history */
		(void)strcat(buf, bg[i].info);

		/* Add in the social class */
		social_class += (int)(bg[i].bonus) - 50;

		/* Enter the next chart */
		chart = bg[i].next;
	}



	/* Verify social class */
	if (social_class > 100) social_class = 100;
	else if (social_class < 1) social_class = 1;

	/* Save the social class */
	p_ptr->sc = social_class;


	/* Skip leading spaces */
	for (s = buf; *s == ' '; s++) /* loop */;

	/* Get apparent length */
	n = strlen(s);

	/* Kill trailing spaces */
	while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';


#ifdef JP
       {
		char temp[64*4];
		roff_to_buf(s, 60, temp, sizeof(temp));
		t = temp;
		for (i = 0 ; i < 4; i++){
			if (t[0] == 0) break; 
			strcpy(history[i], t);
			t += strlen(t)+1;
		}
       }
#else
	/* Start at first line */
	i = 0;

	/* Collect the history */
	while (TRUE)
	{
		/* Extract remaining length */
		n = strlen(s);

		/* All done */
		if (n < 60)
		{
			/* Save one line of history */
			strcpy(history[i++], s);

			/* All done */
			break;
		}

		/* Find a reasonable break-point */
		for (n = 60; ((n > 0) && (s[n-1] != ' ')); n--) /* loop */;

		/* Save next location */
		t = s + n;

		/* Wipe trailing spaces */
		while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

		/* Save one line of history */
		strcpy(history[i++], s);

		/* Start next line */
		for (s = t; *s == ' '; s++) /* loop */;
	}
#endif

}


/*
 * Computes character's age, height, and weight
 */
void get_ahw(bool newage)
{
	int h_percent; 

	/* Calculate the age */
	if (newage) p_ptr->age = rp_ptr->b_age + randint1(rp_ptr->m_age);

	/* Calculate the height/weight for males */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		h_percent = (int)(p_ptr->ht) * 100 / (int)(rp_ptr->m_b_ht);
		p_ptr->wt = randnor((int)(rp_ptr->m_b_wt) * h_percent / 100,
			(int)(rp_ptr->m_m_wt) * h_percent / 300 );
	}
	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);

		h_percent = (int)(p_ptr->ht) * 100 / (int)(rp_ptr->f_b_ht);
		p_ptr->wt = randnor((int)(rp_ptr->f_b_wt) * h_percent / 100,
			(int)(rp_ptr->f_m_wt) * h_percent / 300 );
	}
}




/*
 * Get the player's starting money
 */
static void get_money(void)
{
	int i, gold;

	/* Social Class determines starting gold */
	gold = (p_ptr->sc * 6) + randint1(100) + 300;

	/* Process the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Mega-Hack -- reduce gold for high stats */
		if (p_ptr->stat_max[i] >= 18 + 50) gold -= 300;
		else if (p_ptr->stat_max[i] >= 18 + 20) gold -= 200;
		else if (p_ptr->stat_max[i] > 18) gold -= 150;
		else gold -= (p_ptr->stat_max[i] - 8) * 10;
	}

	/* Minimum 100 gold */
	if (gold < 100) gold = 100;

	/* Save the gold */
	p_ptr->au = gold;
}



#if 0
/*
 * Display stat values, subset of "put_stats()"
 *
 * See 'display_player()' for screen layout constraints.
 */
static void birth_put_stats(void)
{
	int i, p;
	int col = 42;
	byte attr;
	char buf[80];


	/* Put the stats (and percents) */
	for (i = 0; i < A_MAX; i++)
	{
		/* Put the stat */
		cnv_stat(stat_use[i], buf);
		c_put_str(TERM_L_GREEN, buf, 3+i, col+24);

		/* Put the percent */
		if (stat_match[i])
		{
			p = 1000L * stat_match[i] / auto_round;
			attr = (p < 100) ? TERM_YELLOW : TERM_L_GREEN;
			sprintf(buf, "%3d.%d%%", p/10, p%10);
			c_put_str(attr, buf, 3+i, col+13);
		}

		/* Never happened */
		else
		{
#ifdef JP
			c_put_str(TERM_RED, "(なし)", 3+i, col+13);
#else
			c_put_str(TERM_RED, "(NONE)", 3+i, col+13);
#endif

		}
	}
}

#endif /* 0 */

/*
 * Clear all the global "character" data
 */
static void player_wipe(void)
{
	int i;


	/* Hack -- zero the struct */
	(void)WIPE(p_ptr, player_type);

	/* Wipe the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(history[i], "");
	}

	/* Wipe the quests */
	for (i = 0; i < max_quests; i++)
	{
		quest[i].status = QUEST_STATUS_UNTAKEN;

		quest[i].cur_num = 0;
		quest[i].max_num = 0;
		quest[i].type = 0;
		quest[i].level = 0;
		quest[i].r_idx = 0;
		quest[i].complev = 0;
	}

	/* No weight */
	p_ptr->total_weight = 0;

	/* No items */
	inven_cnt = 0;
	equip_cnt = 0;

	/* Clear the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_wipe(&inventory[i]);
	}


	/* Start with no artifacts made yet */
	for (i = 0; i < max_a_idx; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		a_ptr->cur_num = 0;
	}

	/* Reset the objects */
	k_info_reset();

	/* Reset the "monsters" */
	for (i = 1; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;
		if (r_ptr->flags3 & RF3_UNIQUE_7) r_ptr->max_num = 7;

		/* Clear player kills */
		r_ptr->r_pkills = 0;
	}


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;


	/* Wipe the spells */
	spell_learned1 = spell_learned2 = 0L;
	spell_worked1 = spell_worked2 = 0L;
	spell_forgotten1 = spell_forgotten2 = 0L;
	for (i = 0; i < 64; i++) spell_order[i] = 99;

	/* Clean the mutation count */
	mutant_regenerate_mod = 100;

	/* Clear "cheat" options */
	cheat_peek = FALSE;
	cheat_hear = FALSE;
	cheat_room = FALSE;
	cheat_xtra = FALSE;
	cheat_know = FALSE;
	cheat_live = FALSE;

	/* Assume no winning game */
	total_winner = FALSE;

	/* Assume no panic save */
	panic_save = 0;

	/* Assume no cheating */
	noscore = 0;
	wizard = FALSE;

	/* Default pet command settings */
	p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
	p_ptr->pet_open_doors = FALSE;
	p_ptr->pet_pickup_items = FALSE;

	/* Level one */
	p_ptr->max_plv = p_ptr->lev = 1;

	/* Initialize arena and rewards information -KMW- */
	p_ptr->arena_number = 0;
	p_ptr->inside_arena = 0;
	p_ptr->inside_quest = 0;
	p_ptr->exit_bldg = TRUE; /* only used for arena now -KMW- */

	/* Reset rewards */
	for (i = 0; i < MAX_BACT; i++)
	{
		p_ptr->rewards[i] = 0;
	}

	/* Reset mutations */
	p_ptr->muta = 0;
}

/*
 *  Initialize random quests and final quests
 */
static void init_dungeon_quests(int number_of_quests)
{
	int i;
	monster_race    *r_ptr;

	/* Init the random quests */
	init_flags = INIT_ASSIGN;
	p_ptr->inside_quest = MIN_RANDOM_QUEST;
	process_dungeon_file("q_info.txt", 0, 0, 0, 0);

	p_ptr->inside_quest = 0;

	/* Prepare allocation table */
	get_mon_num_prep(monster_quest, NULL);

	/* Generate quests */
	for (i = MIN_RANDOM_QUEST + number_of_quests - 1; i >= MIN_RANDOM_QUEST; i--)
	{
		quest_type      *q_ptr = &quest[i];
		monster_race    *quest_r_ptr;
		int             r_idx;

		q_ptr->status = QUEST_STATUS_TAKEN;

		/* You have only once cheance to complete Random Quest */
		q_ptr->flags |= QUEST_FLAG_ONCE;

		while (TRUE)
		{
			r_idx = get_mon_num(q_ptr->level + 10);
			r_ptr = &r_info[r_idx];

			/* Accept Only Unique Monster */
			if(!(r_ptr->flags1 & RF1_UNIQUE)) continue;

			if (r_ptr->flags1 & RF1_QUESTOR) continue;
			if (r_ptr->level >= q_ptr->level + 5) continue;
			if (r_ptr->level >= q_ptr->level) break;
		}

		q_ptr->r_idx = r_idx;
		quest_r_ptr = &r_info[q_ptr->r_idx];

		/* Mark uniques */
		quest_r_ptr->flags1 |= RF1_QUESTOR;

		q_ptr->max_num = 1;
	}

	/* Init the two main quests (Oberon + Serpent) */
	init_flags = INIT_ASSIGN;
	p_ptr->inside_quest = QUEST_SAURON;
	process_dungeon_file("q_info.txt", 0, 0, 0, 0);

	quest[QUEST_SAURON].status = QUEST_STATUS_TAKEN;

	p_ptr->inside_quest = QUEST_MORGOTH;
	process_dungeon_file("q_info.txt", 0, 0, 0, 0);

	quest[QUEST_MORGOTH].status = QUEST_STATUS_TAKEN;
	p_ptr->inside_quest = 0;
}

/*
 * Reset turn
 */
static void init_turn(void)
{
	/* We cannot choose undeads on birth, so initial turn is 1 */
	turn = 1;
}

/* 
 * Try to wield everything wieldable in the inventory. 
 * Code taken from Angband 3.1.0 under Angband license
 */ 
static void wield_all(void) 
{ 
	object_type *o_ptr; 
	object_type *i_ptr; 
	object_type object_type_body; 
 
	int slot; 
	int item; 
 
	/* Scan through the slots backwards */ 
	for (item = INVEN_PACK - 1; item >= 0; item--) 
	{ 
		o_ptr = &inventory[item]; 
 
		/* Skip non-objects */ 
		if (!o_ptr->k_idx) continue; 
 
		/* Make sure we can wield it and that there's nothing else in that slot */ 
		slot = wield_slot(o_ptr); 
		if (slot < INVEN_WIELD) continue; 
		if (slot == INVEN_LITE) continue; /* Does not wield toaches because buys a lantern soon */
		if (inventory[slot].k_idx) continue; 
 
		/* Get local object */ 
		i_ptr = &object_type_body; 
		object_copy(i_ptr, o_ptr); 
 
		/* Modify quantity */ 
		i_ptr->number = 1; 
 
		/* Decrease the item (from the pack) */ 
		if (item >= 0) 
		{ 
			inven_item_increase(item, -1); 
			inven_item_optimize(item); 
		} 
 
		/* Decrease the item (from the floor) */ 
		else 
		{ 
			floor_item_increase(0 - item, -1); 
			floor_item_optimize(0 - item); 
		} 
 
		/* Get the wield slot */ 
		o_ptr = &inventory[slot]; 
 
		/* Wear the new stuff */ 
		object_copy(o_ptr, i_ptr); 
 
		/* Increase the weight */ 
		p_ptr->total_weight += i_ptr->weight; 
 
		/* Increment the equip counter by hand */ 
		equip_cnt++;

 	} 
	return; 
} 


/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */
static byte player_init[MAX_CLASS][3][2] =
{
	{
		/* Warrior */
		{ TV_RING, SV_RING_RES_FEAR }, /* Warriors need it! */
		{ TV_SWORD, SV_BASTARD_SWORD },
		{ TV_HARD_ARMOR, SV_CHAIN_MAIL }
	},

	{
		/* Mage */
		{ TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
		{ TV_SWORD, SV_DAGGER },
		{ TV_POTION, SV_POTION_SPEED },
	},

	{
		/* Priest */
		{ TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
		{ TV_HAFTED, SV_MACE },
		{ TV_POTION, SV_POTION_HEALING },
	},

	{
		/* Archer */
		{ TV_SWORD, SV_SHORT_SWORD },
		{ TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL },
		{ TV_BOW, SV_LONG_BOW },
	},

	{
		/* Paladin */
		{ TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
		{ TV_SWORD, SV_LONG_SWORD },
		{ TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL }
	},

	{
		/* Warrior-Mage */
		{ TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
		{ TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL },
		{ TV_SWORD, SV_LONG_SWORD },
	},
};


/*
 * Add an outfit object
 */
static void add_outfit(object_type *o_ptr)
{
	s16b slot;

	object_aware(o_ptr);
	object_known(o_ptr);

	/* These objects are "storebought" */
	o_ptr->ident |= IDENT_STORE;

	slot = inven_carry(o_ptr);

	/* Auto-inscription */
	autopick_alter_item(slot, FALSE);

	/* Now try wielding everything */ 
	wield_all(); 
}


/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
void player_outfit(void)
{
	int i, tv, sv;

	object_type	forge;
	object_type	*q_ptr;


	/* Get local object */
	q_ptr = &forge;

	/* Give the player some food */
	object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));
	q_ptr->number = (byte)rand_range(3, 7);
	add_outfit(q_ptr);

	/* Get local object */
	q_ptr = &forge;

	/* Hack -- Give the player some torches */
	object_prep(q_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
	q_ptr->number = (byte)rand_range(3, 7);
	q_ptr->xtra3 = FUEL_TORCH / 2;
	add_outfit(q_ptr);

	/* Get local object */
	q_ptr = &forge;

	if (p_ptr->pclass == CLASS_ARCHER)
	{
		/* Hack -- Give the player some arrows */
		object_prep(q_ptr, lookup_kind(TV_ARROW, SV_AMMO_NORMAL));
		q_ptr->number = (byte)rand_range(15, 20);

		add_outfit(q_ptr);
	}

	/* Hack -- Give the player three useful objects */
	for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
		tv = player_init[p_ptr->pclass][i][0];
		sv = player_init[p_ptr->pclass][i][1];

		/* Hack to initialize spellbooks */
		if (tv == TV_SORCERY_BOOK) tv = TV_LIFE_BOOK + p_ptr->realm1 - 1;
#if 0
		else if (tv == TV_DEATH_BOOK) tv = TV_LIFE_BOOK + p_ptr->realm2 - 1;
#endif

		else if (tv == TV_RING && sv == SV_RING_RES_FEAR &&
		    p_ptr->prace == RACE_BARBARIAN)
			/* Barbarians do not need a ring of resist fear */
			sv = SV_RING_SUSTAIN_STR;

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player an object */
		object_prep(q_ptr, lookup_kind(tv, sv));

		add_outfit(q_ptr);
	}
}


/*
 * Player race
 */
static bool get_player_race(void)
{
	int     k, n, cs, os;
	int		mr, i2r[MAX_RACES];
	cptr    str;
	char    c;
	char	sym[MAX_RACES];
	char    p2 = ')';
	char    buf[80], cur[80];


	/* Extra info */
#ifdef JP
	put_str("注意：《種族》によってキャラクターの先天的な資質やボーナスが変化します。", 23, 5);
#else
	put_str("Note: Your 'race' determines various intrinsic factors and bonuses.", 23 ,5);
#endif

	hack_mutation = FALSE;

	/* Dump races */
	for (n = 0, mr = 0; n < MAX_RACES; n++)
	{
		/* Analyze */
		p_ptr->prace = n;
		rp_ptr = &race_info[p_ptr->prace];

		/* Hack -- 選べない種族をcan_birthフラグで判断 */
		if (!rp_ptr->can_birth) continue;

		i2r[mr] = n;
		str = rp_ptr->title;

		/* Display */
		if (n < 26)
			sym[mr] = I2A(mr);
		else
			sym[mr] = ('A' + mr - 26);
#ifdef JP
		sprintf(buf, "%c%c%s", sym[mr], p2, str);
#else
		sprintf(buf, "%c%c %s", sym[mr], p2, str);
#endif
		put_str(buf, 12 + (mr/4), 1 + 20 * (mr%4));

		mr++;
	}

#ifdef JP
	sprintf(cur, "%c%c%s", '*', p2, "ランダム");
#else
	sprintf(cur, "%c%c %s", '*', p2, "Random");
#endif

	/* Choose */
	k = -1;
	cs = 0;
	os = mr;
	while (1)
	{
		/* Move Cursol */
		if (cs != os)
		{
			c_put_str(TERM_WHITE, cur, 12 + (os/4), 1 + 20 * (os%4));
			put_str("                                   ", 3, 40);
			if(cs == mr)
			{
#ifdef JP
				sprintf(cur, "%c%c%s", '*', p2, "ランダム");
#else
				sprintf(cur, "%c%c %s", '*', p2, "Random");
#endif
				put_str("                                   ", 4, 40);
				put_str("                                   ", 5, 40);
			}
			else
			{
				rp_ptr = &race_info[i2r[cs]];
				str = rp_ptr->title;
#ifdef JP
				sprintf(cur, "%c%c%s", sym[cs], p2, str);
				c_put_str(TERM_L_BLUE, rp_ptr->title, 3, 40);
				put_str("の種族修正", 3, 40+strlen(rp_ptr->title));
				put_str("腕力 知能 賢さ 器用 耐久 魅力 経験 ", 4, 40);
#else
				sprintf(cur, "%c%c %s", sym[cs], p2, str);
				c_put_str(TERM_L_BLUE, rp_ptr->title, 3, 40);
				put_str(": Race modification", 3, 40+strlen(rp_ptr->title));
				put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
#endif
				sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %+4d%% ",
					rp_ptr->r_adj[0], rp_ptr->r_adj[1], rp_ptr->r_adj[2], rp_ptr->r_adj[3],
					rp_ptr->r_adj[4], rp_ptr->r_adj[5], (rp_ptr->r_exp - 100));
				c_put_str(TERM_L_BLUE, buf, 5, 40);
			}
			c_put_str(TERM_YELLOW, cur, 12 + (cs/4), 1 + 20 * (cs%4));
			os = cs;
		}

		if (k >= 0) break;

#ifdef JP
		sprintf(buf, "種族を選んで下さい (%c-%c) ('='初期オプション設定): ", sym[0], sym[mr-1]);
#else
		sprintf(buf, "Choose a race (%c-%c) ('=' for options): ", sym[0], sym[mr-1]);
#endif

		put_str(buf, 10, 10);
		c = inkey();
		if (c == '8')
		{
			if (cs >= 4) cs -= 4;
			continue;
		}
		if (c == '4')
		{
			if (cs > 0) cs--;
			continue;
		}
		if (c == '6')
		{
			if (cs < mr) cs++;
			continue;
		}
		if (c == '2')
		{
			if ((cs + 4) <= mr) cs += 4;
			continue;
		}
		if (c == ' ' || c == '\r' || c == '\n')
		{
			if(cs == mr)
			{
				k = randint0(mr);
				cs = k;
				continue;
			}
			else
			{
				k = cs;
				break;
			}
		}
		if (c == '*')
		{
			k = randint0(mr);
			cs = k;
			continue;
		}
		if (c == 'Q')
		{
			remove_loc();
			quit(NULL);
		}
		if (c == 'S') return (FALSE);
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < mr))
		{
			cs = k;
			continue;
		}
		k = (isupper(c) ? (26 + c - 'A') : -1);
		if ((k >= 26) && (k < mr))
		{
			cs = k;
			continue;
		}
		else k = -1;
		if (c == '?') do_cmd_help();
		else if (c == '=')
		{
			screen_save();
#ifdef JP
			do_cmd_options_aux(6, "初期オプション");
#else
			do_cmd_options_aux(6, "Startup Options");
#endif
			screen_load();
		}
		else bell();
	}

	/* Set race */
	p_ptr->prace = i2r[k];

	rp_ptr = &race_info[p_ptr->prace];
	str = rp_ptr->title;

	/* Display */
#ifdef JP
	put_str("種族        :", 4, 1);
#else
	put_str("Race        :", 4, 1);
#endif
	put_str("                                   ", 3, 40);
	put_str("                                   ", 4, 40);
	put_str("                                   ", 5, 40);

	c_put_str(TERM_L_BLUE, str, 4, 15);

	/* Success */
	return TRUE;
}


/*
 * Player class
 */
static bool get_player_class(void)
{
	int     k, n, cs, os;
	char    c;
	char	sym[MAX_CLASS];
	char    p2 = ')';
	char    buf[80], cur[80];
	cptr    str;


	/* Extra info */
#ifdef JP
	put_str("注意：《職業》によってキャラクターの先天的な能力やボーナスが変化します。", 23, 5);
#else
	put_str("Note: Your 'class' determines various intrinsic abilities and bonuses.", 23, 5);
#endif

#ifdef JP
	put_str("()で囲まれた選択肢はこの種族には似合わない職業です。", 11, 10);
#else
	put_str("Any entries in parentheses should only be used by advanced players.", 11, 5);
#endif


	/* Dump classes */
	for (n = 0; n < MAX_CLASS; n++)
	{
		/* Analyze */
		p_ptr->pclass = n;
		cp_ptr = &class_info[p_ptr->pclass];
		mp_ptr = &m_info[p_ptr->pclass];
		str = cp_ptr->title;
		if (n < 26)
			sym[n] = I2A(n);
		else
			sym[n] = ('A' + n - 26);

		/* Display */
		if (!(rp_ptr->choice & (1L << n)))
#ifdef JP
			sprintf(buf, "%c%c(%s)", sym[n], p2, str);
#else
			sprintf(buf, "%c%c (%s)", sym[n], p2, str);
#endif
		else
#ifdef JP
			sprintf(buf, "%c%c%s", sym[n], p2, str);
#else
			sprintf(buf, "%c%c %s", sym[n], p2, str);
#endif

		put_str(buf, 13+ (n/4), 2 + 19 * (n%4));
	}

#ifdef JP
	sprintf(cur, "%c%c%s", '*', p2, "ランダム");
#else
	sprintf(cur, "%c%c %s", '*', p2, "Random");
#endif

	/* Get a class */
	k = -1;
	cs = 0;
	os = MAX_CLASS;
	while (1)
	{
		/* Move Cursol */
		if (cs != os)
		{
			c_put_str(TERM_WHITE, cur, 13 + (os/4), 2 + 19 * (os%4));
			put_str("                                   ", 3, 40);
			if(cs == MAX_CLASS)
			{
#ifdef JP
				sprintf(cur, "%c%c%s", '*', p2, "ランダム");
#else
				sprintf(cur, "%c%c %s", '*', p2, "Random");
#endif
				put_str("                                   ", 4, 40);
				put_str("                                   ", 5, 40);
			}
			else
			{
				cp_ptr = &class_info[cs];
				mp_ptr = &m_info[cs];
				str = cp_ptr->title;
				if (!(rp_ptr->choice & (1L << cs)))
#ifdef JP
					sprintf(cur, "%c%c(%s)", sym[cs], p2, str);
#else
					sprintf(cur, "%c%c (%s)", sym[cs], p2, str);
#endif
				else
#ifdef JP
					sprintf(cur, "%c%c%s", sym[cs], p2, str);
#else
					sprintf(cur, "%c%c %s", sym[cs], p2, str);
#endif
#ifdef JP
					c_put_str(TERM_L_BLUE, cp_ptr->title, 3, 40);
					put_str("の職業修正", 3, 40+strlen(cp_ptr->title));
					put_str("腕力 知能 賢さ 器用 耐久 魅力 経験 ", 4, 40);
#else
					c_put_str(TERM_L_BLUE, cp_ptr->title, 3, 40);
					put_str(": Class modification", 3, 40+strlen(cp_ptr->title));
					put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
#endif
					sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %+4d%% ",
						cp_ptr->c_adj[0], cp_ptr->c_adj[1], cp_ptr->c_adj[2], cp_ptr->c_adj[3],
						cp_ptr->c_adj[4], cp_ptr->c_adj[5], cp_ptr->c_exp);
					c_put_str(TERM_L_BLUE, buf, 5, 40);
			}
			c_put_str(TERM_YELLOW, cur, 13 + (cs/4), 2 + 19 * (cs%4));
			os = cs;
		}

		if (k >= 0) break;

#ifdef JP
		sprintf(buf, "職業を選んで下さい (%c-%c) ('='初期オプション設定): ", sym[0], sym[MAX_CLASS-1]);
#else
		sprintf(buf, "Choose a class (%c-%c) ('=' for options): ",  sym[0], sym[MAX_CLASS-1]);
#endif

		put_str(buf, 10, 10);
		c = inkey();
		if (c == '8')
		{
			if (cs >= 4) cs -= 4;
			continue;
		}
		if (c == '4')
		{
			if (cs > 0) cs--;
			continue;
		}
		if (c == '6')
		{
			if (cs < MAX_CLASS) cs++;
			continue;
		}
		if (c == '2')
		{
			if ((cs + 4) <= MAX_CLASS) cs += 4;
			continue;
		}
		if (c == ' ' || c == '\r' || c == '\n')
		{
			if(cs == MAX_CLASS)
			{
				k = randint0(MAX_CLASS);
				cs = k;
				continue;
			}
			else
			{
				k = cs;
				break;
			}
		}
		if (c == '*')
		{
			k = randint0(MAX_CLASS);
			break;
		}
		if (c == 'Q')
		{
			remove_loc();
			quit(NULL);
		}
		if (c == 'S') return (FALSE);
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < MAX_CLASS))
		{
			cs = k;
			continue;
		}
		k = (isupper(c) ? (26 + c - 'A') : -1);
		if ((k >= 26) && (k < MAX_CLASS))
		{
			cs = k;
			continue;
		}
		else k = -1;
		if (c == '?') do_cmd_help();
		else if (c == '=')
		{
			screen_save();
#ifdef JP
			do_cmd_options_aux(6, "初期オプション");
#else
			do_cmd_options_aux(6, "Startup Options");
#endif

			screen_load();
		}
		else bell();
	}

	/* Set class */
	p_ptr->pclass = k;
	cp_ptr = &class_info[p_ptr->pclass];
	mp_ptr = &m_info[p_ptr->pclass];
	str = cp_ptr->title;

	/* Display */
#ifdef JP
	put_str("職業        :", 5, 1);
#else
	put_str("Class       :", 5, 1);
#endif

#ifdef JP
	c_put_str(TERM_L_BLUE, str, 5, 15);
#else
	c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);
#endif
	put_str("                                   ", 3, 40);
	put_str("                                   ", 4, 40);
	put_str("                                   ", 5, 40);

	return TRUE;
}

static bool do_quick_start = FALSE;
static int number_of_rquests = 0;

/*
 * Helper function for 'player_birth()'.
 *
 * This function allows the player to select a sex, race, and class, and
 * modify options (including the birth options).
 */
static bool player_birth_selection(void)
{
	int k, n, cs, os;
	char ch;
	char p2 = ')';
	char buf[80], cur[80];
	char inp[80];
	char c;

	/*** Instructions ***/

	/* Clear screen */
	Term_clear();

	/* Display some helpful information */
#ifdef JP
	put_str("キャラクターを作成します。('S'やり直す, 'Q'終了, '?'ヘルプ)", 8, 10);
#else
	put_str("Make your charactor. ('S' Restart, 'Q' Quit, '?' Help)", 8, 10);
#endif


	/*** Quick Start ***/

	if (previous_char.quick_ok)
	{
		do_quick_start = FALSE;

		/* Extra info */
#ifdef JP
		put_str("クイック・スタートを使うと以前と全く同じキャラクターで始められます。", 11, 5);
#else
		put_str("Do you want to use the quick start function(same character as your last one).", 11, 2);
#endif

		/* Choose */
		while (1)
		{
#ifdef JP
			put_str("クイック・スタートを使いますか？[y/n]", 14, 10);
#else
			put_str("Use quick start? [y/n]", 14, 10);
#endif
			c = inkey();
			if (c == 'Q') quit(NULL);
			else if (c == 'S') return (FALSE);
			else if ((c == 'y') || (c == 'Y'))
			{
				do_quick_start = TRUE;
				break;
			}
			else
			{
				do_quick_start = FALSE;
				break;
			}
		}

		if (do_quick_start)
		{
			load_prev_data(FALSE);
			init_dungeon_quests(previous_char.quests);
			init_turn();

			/* Calc hitdie, but don't roll */
			get_extra(FALSE);

			sp_ptr = &sex_info[p_ptr->psex];
			rp_ptr = &race_info[p_ptr->prace];
			cp_ptr = &class_info[p_ptr->pclass];
			mp_ptr = &m_info[p_ptr->pclass];

			/* Calculate the bonuses and hitpoints */
			p_ptr->update |= (PU_BONUS | PU_HP);

			/* Update stuff */
			update_stuff();

			/* Fully healed */
			p_ptr->chp = p_ptr->mhp;

			/* Fully rested */
			p_ptr->csp = p_ptr->msp;

			/* Process the player name */
			process_player_name(FALSE);

			return TRUE;
		}

		/* Clean up */
		clear_from(10);
	}


	/*** Player sex ***/

	/* Extra info */
#ifdef JP
	put_str("注意：《性別》の違いはゲーム上ほとんど影響を及ぼしません。", 23, 5);
#else
	put_str("Note: Your 'sex' does not have any significant gameplay effects.", 23, 5);
#endif


	/* Prompt for "Sex" */
	for (n = 0; n < MAX_SEXES; n++)
	{
		/* Analyze */
		p_ptr->psex = n;
		sp_ptr = &sex_info[p_ptr->psex];

		/* Display */
#ifdef JP
		sprintf(buf, "%c%c%s", I2A(n), p2, sp_ptr->title);
#else
		sprintf(buf, "%c%c %s", I2A(n), p2, sp_ptr->title);
#endif

		put_str(buf, 12 + (n/5), 2 + 15 * (n%5));
	}

#ifdef JP
	sprintf(cur, "%c%c%s", '*', p2, "ランダム");
#else
	sprintf(cur, "%c%c %s", '*', p2, "Random");
#endif

	/* Choose */
	k = -1;
	cs = 0;
	os = MAX_SEXES;
	while (1)
	{
		if (cs != os)
		{
			put_str(cur, 12 + (os/5), 2 + 15 * (os%5));
			if(cs == MAX_SEXES)
#ifdef JP
				sprintf(cur, "%c%c%s", '*', p2, "ランダム");
#else
				sprintf(cur, "%c%c %s", '*', p2, "Random");
#endif
			else
			{
				sp_ptr = &sex_info[cs];
#ifdef JP
				sprintf(cur, "%c%c%s", I2A(cs), p2, sp_ptr->title);
#else
				sprintf(cur, "%c%c %s", I2A(cs), p2, sp_ptr->title);
#endif
			}
			c_put_str(TERM_YELLOW, cur, 12 + (cs/5), 2 + 15 * (cs%5));
			os = cs;
		}

		if (k >= 0) break;

#ifdef JP
		sprintf(buf, "性別を選んで下さい (%c-%c) '*'でランダム, '='で初期オプション設定: ", I2A(0), I2A(n-1));
#else
		sprintf(buf, "Choose a sex (%c-%c), * for random, or = for options: ", I2A(0), I2A(n-1));
#endif

		put_str(buf, 10, 10);
		ch = inkey();
		if (ch == '4')
		{
			if (cs > 0) cs--;
			continue;
		}
		if (ch == '6')
		{
			if (cs < MAX_SEXES) cs++;
			continue;
		}
		if (ch == ' ' || ch == '\r' || ch == '\n')
		{
			if(cs == MAX_SEXES)
				k = randint0(MAX_SEXES);
			else
				k = cs;
			break;
		}
		if (ch == '*')
		{
			k = randint0(MAX_SEXES);
			break;
		}
		if (ch == 'Q')
		{
			remove_loc();
			quit(NULL);
		}
		if (ch == 'S') return (FALSE);

		k = (islower(ch) ? A2I(ch) : -1);
		if ((k >= 0) && (k < MAX_SEXES))
		{
			cs = k;
			continue;
		}
		else k = -1;
		if (ch == '?') do_cmd_help();
		else if (ch == '=')
		{
			screen_save();
#ifdef JP
			do_cmd_options_aux(6, "初期オプション");
#else
			do_cmd_options_aux(6, "Startup Options");
#endif

			screen_load();
		}
		else bell();
	}

	/* Set sex */
	p_ptr->psex = k;
	sp_ptr = &sex_info[p_ptr->psex];

	/* Sex */
#ifdef JP
	put_str("性別        :", 3, 1);
#else
	put_str("Sex         :", 3, 1);
#endif

	c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 15);

	/* Clean up */
	clear_from(10);

	/* Choose the players race */
	if (!get_player_race()) return FALSE;

	/* Clean up */
	clear_from(10);

	/* Choose the players class */
	if (!get_player_class()) return FALSE;

	/* Clean up */
	clear_from(10);

	/* Choose the magic realms */
	if (!get_player_realms()) return FALSE;

	/* Clear */
	clear_from(10);

	/*** User enters number of quests ***/
	/* Heino Vander Sanden and Jimmy De Laet */

	/* Extra info */
#ifdef JP
	put_str("必須のクエスト(サウロン及び冥王モルゴス)に加えて、追加のクエストの", 10, 5);
	put_str("数を設定することが出来ます。", 11, 5);
	put_str("追加クエストを行ないたくない場合は '0'を入力して下さい。", 12, 5);
	put_str("ランダムに決定するには'*'を入力して下さい。", 13, 5);
#else
	put_str("You can enter the number of quests you'd like to perform in addition", 10, 5);
	put_str("to the two obligatory ones ( Sauron and Mrogoth, Lord of Darkness )", 11, 5);
	put_str("In case you do not want any additional quests, just enter 0", 12, 5);
	put_str("If you want a random number of random quests, just enter *", 13, 5);
#endif


	/* Ask the number of additional quests */
	while (TRUE)
	{

#ifdef JP
		put_str(format("追加クエストの数 (%u以下) ", MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 1), 15, 5);
#else
		put_str(format("Number of additional quests? (<%u) ", MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 2), 15, 5);
#endif


		/* Get a the number of additional quest */
		while (TRUE)
		{
			/* Move the cursor */
			put_str("", 15, 40);

			/* Default */
			strcpy(inp, "5");

			/* Get a response (or escape) */
			if (!askfor_aux(inp, 2, FALSE)) strcpy(inp, "5");

			/* Quit */
			if (inp[0] == 'Q')
			{
				remove_loc();
				quit(NULL);
			}

			/* Start over */
			if (inp[0] == 'S') return (FALSE);

			/* Check for random number of quests */
			if (inp[0] == '*')
			{
				/* 0 to 10 random quests */
				number_of_rquests = randint0(11);
			}
			else
			{
				number_of_rquests = atoi(inp);
			}

			/* Break on valid input */
			if ((number_of_rquests <= MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 1) && (number_of_rquests >= 0)) break;
		}
		break;
	}

	/* Clear */
	clear_from(10);

	/* Reset turn; before auto-roll and after choosing race */
	init_turn();

	/* Done */
	return (TRUE);
}


#ifdef JP
/* 文字列sのxバイト目が漢字の1バイト目かどうか判定する */
static bool iskanji2(cptr s, int x)
{
	int i;

	for (i = 0; i < x; i++)
	{
		if (iskanji(s[i])) i++;
	}
	if ((x == i) && iskanji(s[x])) return TRUE;

	return FALSE;
}
#endif

/*
 *  Character background edit-mode
 */
static void edit_history(void)
{
	char old_history[4][60];
	int y = 0, x = 0;
	int i, j;

	/* Edit character background */
	for (i = 0; i < 4; i++)
	{
		sprintf(old_history[i], "%s", history[i]);
	}
	/* Turn 0 to space */
	for (i = 0; i < 4; i++)
	{
		for (j = 0; history[i][j]; j++) /* loop */;

		for (; j < 59; j++) history[i][j] = ' ';
		history[i][59] = '\0';
	}
	display_player(1);
#ifdef JP
	c_put_str(TERM_L_GREEN, "(キャラクターの生い立ち - 編集モード)", 11, 20);
	put_str("[ カーソルキーで移動、Enterで終了 ]", 17, 23);
#else
	c_put_str(TERM_L_GREEN, "(Character Background - Edit Mode)", 11, 20);
	put_str("[ Cursor key for Move, Enter for End ]", 17, 21);
#endif

	while (TRUE)
	{
		int skey;
		char c;

		for (i = 0; i < 4; i++)
		{
			put_str(history[i], i + 12, 10);
		}
#ifdef JP
		if (iskanji2(history[y], x))
			c_put_str(TERM_L_BLUE, format("%c%c", history[y][x],history[y][x+1]), y + 12, x + 10);
		else
#endif
		c_put_str(TERM_L_BLUE, format("%c", history[y][x]), y + 12, x + 10);

		/* Place cursor just after cost of current stat */
		Term_gotoxy(x + 10, y + 12);

		/* Get special key code */
		skey = inkey_special(TRUE);

		/* Get a character code */
		if (!(skey & SKEY_MASK)) c = (char)skey;
		else c = 0;

		if (skey == SKEY_UP || c == KTRL('p'))
		{
			y--;
			if (y < 0) y = 3;
#ifdef JP
			if ((x > 0) && (iskanji2(history[y], x-1))) x--;
#endif
		}
		else if (skey == SKEY_DOWN || c == KTRL('n'))
		{
			y++;
			if (y > 3) y = 0;
#ifdef JP
			if ((x > 0) && (iskanji2(history[y], x-1))) x--;
#endif
		}
		else if (skey == SKEY_RIGHT || c == KTRL('f'))
		{
#ifdef JP
			if (iskanji2(history[y], x)) x++;
#endif
			x++;
			if (x > 58)
			{
				x = 0;
				if (y < 3) y++;
			}
		}
		else if (skey == SKEY_LEFT || c == KTRL('b'))
		{
			x--;
			if (x < 0)
			{
				if (y)
				{
					y--;
					x = 58;
				}
				else x = 0;
			}

#ifdef JP
			if ((x > 0) && (iskanji2(history[y], x-1))) x--;
#endif
		}
		else if (c == '\r' || c == '\n')
		{
			Term_erase(0, 11, 255);
			Term_erase(0, 17, 255);
#ifdef JP
			put_str("(キャラクターの生い立ち - 編集済み)", 11, 20);
#else
			put_str("(Character Background - Edited)", 11, 20);
#endif
			break;
		}
		else if (c == ESCAPE)
		{
			clear_from(11);
#ifdef JP
			put_str("(キャラクターの生い立ち)", 11, 25);
#else
			put_str("(Character Background)", 11, 25);
#endif

			for (i = 0; i < 4; i++)
			{
				sprintf(history[i], "%s", old_history[i]);
				put_str(history[i], i + 12, 10);
			}
			break;
		}
		else if (c == '\010')
		{
			x--;
			if (x < 0)
			{
				if (y)
				{
					y--;
					x = 58;
				}
				else x = 0;
			}

			history[y][x] = ' ';
#ifdef JP
			if ((x > 0) && (iskanji2(history[y], x - 1)))
			{
				x--;
				history[y][x] = ' ';
			}
#endif
		}
#ifdef JP
		else if (iskanji(c) || isprint(c))
#else
		else if (isprint(c)) /* BUGFIX */
#endif
		{
#ifdef JP
			if (iskanji2(history[y], x))
			{
				history[y][x+1] = ' ';
			}

			if (iskanji(c))
			{
				if (x > 57)
				{
					x = 0;
					y++;
					if (y > 3) y = 0;
				}

				if (iskanji2(history[y], x+1))
				{
					history[y][x+2] = ' ';
				}

				history[y][x++] = c;

				c = inkey();
			}
#endif
			history[y][x++] = c;
			if (x > 58)
			{
				x = 0;
				y++;
				if (y > 3) y = 0;
			}
		}
	} /* while (TRUE) */

}


static int initial_stat[MAX_CLASS][A_MAX] =
{
	{ 17, 3, 3, 16, 16, 3 },	/* Warrior */
	{ 16, 17, 3, 3, 16, 3 },	/* Mage */
	{ 16, 3, 16, 16, 15, 3 },	/* Priest */
	{ 16, 3, 3,	17, 16, 3 },	/* Archer */
	{ 16, 3, 15, 16, 16, 3 },	/* Paradin */
	{ 16, 15, 3, 16, 16, 3 },	/* Warrior-Mage */
};

/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "auto-rolling" and "random-rolling".
 *
 * The delay may be reduced, but is recommended to keep players
 * from continuously rolling up characters, which can be VERY
 * expensive CPU wise.  And it cuts down on player stupidity.
 */
static bool player_birth_autoroll(void)
{
	int i, j, m;
	bool flag;
	bool prev = FALSE;
	char ch;
	char b1 = '[';
	char b2 = ']';
	char buf[80];


#ifdef ALLOW_AUTOROLLER

	s16b stat_limit[A_MAX];
	s32b stat_match[A_MAX];
	s32b auto_round = 0L;


	/*** Autoroll ***/

	/* Initialize */
	for (i = 0; i < 6; i++)
	{
		/* Save the minimum stat */
		stat_limit[i] = initial_stat[p_ptr->pclass][i];
	}

#endif /* ALLOW_AUTOROLLER */

	/* Clean up */
	clear_from(10);


	/*** Generate ***/

	/* Roll */
	while (TRUE)
	{
		int col = 42;

		/* Feedback */
		if (autoroller)
		{
			Term_clear();

			/* Label */
#ifdef JP
			put_str("最小値", 2, col+5);
			put_str("成功率", 2, col+13);
			put_str("現在値", 2, col+24);
#else
			put_str(" Limit", 2, col+5);
			put_str("  Freq", 2, col+13);
			put_str("  Roll", 2, col+24);
#endif

			/* Put the minimal stats */
			for (i = 0; i < A_MAX; i++)
			{
				/* Label stats */
				put_str(stat_names[i], 3+i, col);

				/* Race/Class bonus */
				j = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

				/* Obtain the current stat */
				m = adjust_stat(stat_limit[i], j, TRUE);

				/* Put the stat */
				cnv_stat(m, buf);
				c_put_str(TERM_L_BLUE, buf, 3+i, col+5);
			}

			/* Label count */
#ifdef JP
			put_str("回数 :", 10, col+13);
#else
			put_str("Round:", 10, col+13);
#endif


			/* Indicate the state */
#ifdef JP
			put_str("(ESCで停止)", 12, col+13);
#else
			put_str("(Hit ESC to stop)", 12, col+13);
#endif


			/* Auto-roll */
			while (1)
			{
				bool accept = TRUE;

				/* Get a new character */
				get_stats();

				/* Advance the round */
				auto_round++;

				/* Hack -- Prevent overflow */
				if (auto_round >= 1000000000L)
				{
					auto_round = 1;

					if (autoroller)
					{
						for( i = 0; i < 6; i ++)
						{
							stat_match[i] = 0;
						}
					}
				}

				/* Check and count acceptable stats */
				for (i = 0; i < A_MAX; i++)
				{
					/* This stat is okay */
					if (p_ptr->stat_max[i] >= stat_limit[i])
					{
						stat_match[i]++;
					}

					/* This stat is not okay */
					else
					{
						accept = FALSE;
					}
				}

				/* Break if "happy" */
				if (accept) break;

				/* Take note every 25 rolls */
				flag = (!(auto_round % AUTOROLLER_STEP));

				/* Update display occasionally */
				if (flag)
				{
					/* Put the stats (and percents) */
					for (i = 0; i < A_MAX; i++)
					{
						/* Put the stat */
						cnv_stat(p_ptr->stat_max[i], buf);
						c_put_str(TERM_L_GREEN, buf, 3+i, col+24);

						/* Put the percent */
						if (stat_match[i])
						{
							int p;
							byte attr;
							if (stat_match[i] > 1000000L)
							{
								/* Prevent overflow */
								p = stat_match[i] / (auto_round / 1000L);
							}
							else
							{
								p = 1000L * stat_match[i] / auto_round;
							}

							attr = (p < 100) ? TERM_YELLOW : TERM_L_GREEN;
							sprintf(buf, "%3d.%d%%", p/10, p%10);
							c_put_str(attr, buf, 3+i, col+13);
						}

						/* Never happened */
						else
						{
#ifdef JP
							c_put_str(TERM_RED, "(なし)", 3+i, col+13);
#else
							c_put_str(TERM_RED, "(NONE)", 3+i, col+13);
#endif
						}
					}

					/* Dump round */
					put_str(format("%10ld", auto_round), 10, col+20);

					/* Make sure they see everything */
					Term_fresh();

					/* Delay 1/10 second */
					if (delay_autoroll) Term_xtra(TERM_XTRA_DELAY, 100);
#if 0
					/* Do not wait for a key */
					inkey_scan = TRUE;

					/* Check for a keypress */
					if (inkey()) break;
#endif
				}
			}
		}

		/* Otherwise just get a character */
		else
		{
			/* Get a new character */
			get_stats();
		}

		/* Flush input */
		flush();

		/*** Display ***/

		/* Roll for base hitpoints */
		get_extra(TRUE);

		/* Roll for age/height/weight */
		get_ahw(TRUE);

		/* Roll for social class */
		get_history();

		/* Roll for gold */
		get_money();

		/* Hack -- get a chaos patron even if you are not a chaos warrior */
		p_ptr->valar_patron = randint0(MAX_PATRON);

		/* Input loop */
		while (TRUE)
		{
			/* Calculate the bonuses and hitpoints */
			p_ptr->update |= (PU_BONUS | PU_HP);

			/* Update stuff */
			update_stuff();

			/* Fully healed */
			p_ptr->chp = p_ptr->mhp;

			/* Fully rested */
			p_ptr->csp = p_ptr->msp;

			/* Display the player */
			display_player(0);

			/* Prepare a prompt (must squeeze everything in) */
			Term_gotoxy(2, 23);
			Term_addch(TERM_WHITE, b1);
#ifdef JP
			Term_addstr(-1, TERM_WHITE, "'r'で次の数値");
#else
			Term_addstr(-1, TERM_WHITE, "'r' to reroll");
#endif

#ifdef JP
			if (prev) Term_addstr(-1, TERM_WHITE, ", 'p'で前の数値");
#else
			if (prev) Term_addstr(-1, TERM_WHITE, ", 'p' for prev");
#endif

#ifdef JP
			Term_addstr(-1, TERM_WHITE, ", Enterでこの数値に決定");
#else
			Term_addstr(-1, TERM_WHITE, ", or Enter to accept");
#endif

			Term_addch(TERM_WHITE, b2);

			/* Prompt and get a command */
			ch = inkey();

			/* Quit */
			if (ch == 'Q') quit(NULL);

			/* Start over */
			if (ch == 'S') return (FALSE);

			/* Escape accepts the roll */
			if (ch == ESCAPE || ch == '\r' || ch == '\n') break;

			/* Reroll this character */
			if ((ch == ' ') || (ch == 'r')) break;

			/* Previous character */
			if (prev && (ch == 'p'))
			{
				load_prev_data(TRUE);
				continue;
			}

			/* Help */
			if (ch == '?')
			{
				do_cmd_help();
				continue;
			}
			else if (ch == '=')
			{
				screen_save();
#ifdef JP
				do_cmd_options_aux(6, "初期オプション");
#else
				do_cmd_options_aux(6, "Startup Options");
#endif

				screen_load();
				continue;
			}

			/* Warning */
			bell();
		}

		/* Are we done? */
		if (ch == ESCAPE || ch == '\r' || ch == '\n') break;

		/* Save this for the "previous" character */
		save_prev_data(&previous_char);
		previous_char.quick_ok = FALSE;

		/* Note that a previous roll exists */
		prev = TRUE;
	}

	/* Clear prompt */
	clear_from(23);

	/* Done */
	return (TRUE);
}


static bool player_birth_aux(void)
{
	char ch;

	/* Ask questions */
	if (!player_birth_selection()) return FALSE;
	if (do_quick_start) return TRUE;

	/* Auto-roll */
	else
	{
		if (!player_birth_autoroll()) return FALSE;
	}

	/*** Edit character background ***/
	edit_history();

	/* Get a name, prepare savefile */
	get_character_name();

	/* Display the player */
	display_player(0);

	/* Prompt for it */
#ifdef JP
	prt("[ 'Q' で中断, 'S' で初めから, Enter でゲーム開始 ]", 23, 14);
#else
	prt("['Q' to suicide, 'S' to start over, or Enter to continue]", 23, 10);
#endif


	/* Get a key */
	ch = inkey();

	/* Quit */
	if (ch == 'Q') quit(NULL);

	/* Start over */
	if (ch == 'S') return (FALSE);

	init_dungeon_quests(number_of_rquests);

	/* Save character data for quick start */
	save_prev_data(&previous_char);
	previous_char.quests = (byte)number_of_quests();
	previous_char.quick_ok = TRUE;

	/* Accept */
	return (TRUE);
}


/*
 * Create a new character.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
void player_birth(void)
{
	int i, j, row;


	/* Initialize Playtime */
	playtime = 0;

	/* Create a new character */
	while (1)
	{
		/* Wipe the player */
		player_wipe();

		/* Roll up a new character */
		if (player_birth_aux()) break;
	}

	/* Create a note file if that option is set */
	if (take_notes)
	{
		add_note_type(NOTE_BIRTH);
	}

	/* Note player birth in the message recall */
	message_add(" ");
	message_add("  ");
	message_add("====================");
	message_add("  ");
	message_add(" ");

	/* Init the shops */
	for (i = 1; i < max_towns; i++)
	{
		for (j = 0; j < MAX_STORES; j++)
		{
			/* Initialize */
			store_init(i, j);
		}
	}

	/* Set the message window flag as default */
	if (!window_flag[1])
		window_flag[1] |= PW_MESSAGE;

	/* Set the inv/equip window flag as default */
	if (!window_flag[2])
		window_flag[2] |= PW_INVEN;

	/* Start Message */
	clear_from(0);
	row = 5;
#ifdef JP
	prt("冥王モルゴスが虚空より舞い戻った！                          ", row++, 10);
	prt("モルゴスは闇の力でかつての手下どもを蘇らせ、闇の軍勢を率いて", row++, 10);
	prt("再びアングバンドの要塞に立てこもろうとしている。まだ、その力", row++, 10);
	prt("は完全ではないが、時が経てば世界を闇に覆い尽くす程の力を取り", row++, 10);
	prt("戻すだろう。                                                ", row++, 10);
	prt("神々は、この不測の事態に対して、大いなる軍勢を派遣しようとし", row++, 10);
	prt("ている。このままでは中つ国はめちゃめちゃになってしまうだろう。", row++, 10);
	prt(format("あなたの神%sはこの事態を憂慮し、密かにあなたにモルゴス",
		valar_patrons[p_ptr->valar_patron]), row++, 10);
	prt("討伐を命じられた。モルゴスが力を取り戻す前に、あなたは洞穴の", row++, 10);
	prt("奥深くに潜む冥王モルゴスを打ち倒さなければならない。        ", row++, 10);
	switch(p_ptr->prace)
	{
	case RACE_HALF_ORC:
		row++;
		prt("ハーフオークであるあなたは、自分をこのような境遇に生み出した", row++, 10);
		prt("闇の軍勢にとにかくムカついている。奴らをブチのめせ！", row++, 10);
		break;
	case RACE_BARBARIAN:
		row++;
		prt("かつて大勢の野蛮人が冥王モルゴスに味方したせいで、あなたの種", row++, 10);
		prt("族は肩身の狭い思いをしている。冥王を倒し名誉を挽回せよ！", row++, 10);
		break;
	}
	row++;
	prt("[ 何かキーを押してください。]", row, 25);
#else
	prt("Morgoth, Lord of Darkness, returned from the void!!", row++, 10);
	prt("He has revived his former servants with the power of darkness", row++, 10);
	prt("and is leading his dread troops to try to once again and to", row++, 10);
	prt("hold his fortress 'Angband'. His power is not complete yet, ", row++, 10);
	prt("but with the passage of time he will no doubt regain the power", row++, 10);
	prt("to overwhelm the world with darkness. ", row++, 10);
	prt("The Valar, in face of this unforseen situation have decided to", row++, 10);
	prt("send their armies in in great force.  The great battle in the", row++, 10);
	prt("offing would doubless throw the Middle-Earth into chaos.", row++, 10);
	prt(format("Your valar %s, troubled by this, has ordered you in",
		valar_patrons[p_ptr->valar_patron]), row++, 10);
	prt("secret on a mission to kill Morgoth. You must enter the cave and", row++, 10);
	prt("defeat Morgoth who lurks at recesses of the cave before he", row++, 10);
	prt("regains his great power.", row++, 10);
	switch(p_ptr->prace)
	{
	case RACE_HALF_ORC:
		row++;
		prt("As a half-orc you were born into this situation.", row++, 10);
		prt("The armies of darkness managed to piss you off and", row++, 10);
		prt("now you're going to crush them into the ground!", row++, 10);
		break;
	case RACE_BARBARIAN:
		row++;
		prt("Because many of babarians followed in the past ", row++, 10);
		prt("your tribe is looked on with suspicion.  Defeat ", row++, 10);
		prt("Morgoth, Lord of Darkness, and clear your people's", row++, 10);
		prt("honour!", row++, 10);
		break;
	}
	row++;
	prt("[ Hit Any Key ]", row, 32);
#endif

	(void)inkey();
}
