/* Purpose: Object flavor code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Hack -- note that "TERM_MULTI" is now just "TERM_VIOLET".
 * We will have to find a cleaner method for "MULTI_HUED" later.
 * There were only two multi-hued "flavors" (one potion, one food).
 * Plus five multi-hued "base-objects" (3 dragon scales, one blade
 * of chaos, and one something else).  See the SHIMMER_OBJECTS code
 * in "dungeon.c" and the object color extractor in "cave.c".
 */
#define TERM_MULTI      TERM_VIOLET


/*
 * Max sizes of the following arrays
 */
#define MAX_ROCKS      59       /* Used with rings (min 38) */
#define MAX_AMULETS    30       /* Used with amulets (min 14) */
#define MAX_WOODS      34       /* Used with staffs (min 30) */
#define MAX_METALS     40       /* Used with wands/rods (min 29/29) */
#define MAX_COLORS     70       /* Used with potions (min 60) */
#define MAX_TITLES     65       /* Used with scrolls (min 48) */
#define MAX_SYLLABLES 164       /* Used with scrolls (see below) */


/*
 * Rings (adjectives and colors)
 */

static cptr ring_adj[MAX_ROCKS]
#ifndef JP
= {
	"Alexandrite", "Amethyst", "Aquamarine", "Azurite", "Beryl",
	"Bloodstone", "Calcite", "Carnelian", "Corundum", "Diamond",
	"Emerald", "Fluorite", "Garnet", "Granite", "Jade",
	"Jasper", "Lapis Lazuli", "Malachite", "Marble", "Moonstone",
	"Onyx", "Opal", "Pearl", "Quartz", "Quartzite",
	"Rhodonite", "Ruby", "Sapphire", "Tiger Eye", "Topaz",
	"Turquoise", "Zircon", "Platinum", "Bronze", "Gold",
	"Obsidian", "Silver", "Tortoise Shell", "Mithril", "Jet",
	"Engagement", "Adamantite",
	"Wire", "Dilithium", "Bone", "Wooden",
	"Spikard", "Serpent",   "Wedding", "Double",
	"Plain", "Brass",  "Scarab","Shining",
	"Rusty","Transparent", "Steel", "Tanzanite",
	"Nephrite",
};
#else
= {
	"金緑石の","アメジストの","アクアマリンの","めのうの","緑柱石の",
	"血玉随の","方解石の","赤めのうの","綱玉の","ダイアモンドの",
	"エメラルドの","ホタル石の","ガーネットの","御影石の","ひすいの",
	"ジャスパーの","青瑠璃の","クジャク石の","大理石の","ムーンストーンの",
	"縞めのうの","オパールの","真珠の","水晶の","石英岩の",
	"ザクロ石の","ルビーの","サファイアの","タイガーアイの","トパーズの",
	"トルコ石の","ジルコンの","プラチナの","ブロンズの","金の",
	"黒曜石の","銀の","べっ甲の","ミスリルの","黒玉の",
	"婚約","アダマンタイトの",
	"針金の","ディリシウムの","骨の","木の",
	"スピカの" /*nuke me*/ ,"蛇の","結婚","二重の",
	"飾りのない","青銅の","スカラベの" ,"輝く",
	"錆びた","透明な","鋼鉄の","タンザナイトの",
	"軟玉の",
};
#endif

static byte ring_col[MAX_ROCKS] =
{
	TERM_GREEN, TERM_VIOLET, TERM_L_BLUE, TERM_L_BLUE, TERM_L_GREEN,
	TERM_RED, TERM_WHITE, TERM_RED, TERM_SLATE, TERM_WHITE,
	TERM_GREEN, TERM_L_GREEN, TERM_RED, TERM_L_DARK, TERM_L_GREEN,
	TERM_UMBER, TERM_BLUE, TERM_GREEN, TERM_WHITE, TERM_L_WHITE,
	TERM_L_RED, TERM_L_WHITE, TERM_WHITE, TERM_L_WHITE, TERM_L_WHITE,
	TERM_L_RED, TERM_RED, TERM_BLUE, TERM_YELLOW, TERM_YELLOW,
	TERM_L_BLUE, TERM_L_UMBER, TERM_WHITE, TERM_L_UMBER, TERM_YELLOW,
	TERM_L_DARK, TERM_L_WHITE, TERM_GREEN, TERM_L_BLUE, TERM_L_DARK,
	TERM_YELLOW, TERM_VIOLET,
	TERM_UMBER, TERM_L_WHITE, TERM_WHITE, TERM_UMBER,
	TERM_BLUE, TERM_GREEN, TERM_YELLOW, TERM_ORANGE,
	TERM_YELLOW, TERM_ORANGE, TERM_L_GREEN, TERM_YELLOW,
	TERM_RED, TERM_WHITE, TERM_WHITE, TERM_YELLOW,
	TERM_GREEN,
};


/*
 * Amulets (adjectives and colors)
 */
static cptr amulet_adj[MAX_AMULETS]
#ifndef JP
= {
	"Amber", "Driftwood", "Coral", "Agate", "Ivory",
	"Obsidian", "Bone", "Brass", "Bronze", "Pewter",
	"Tortoise Shell", "Golden", "Azure", "Crystal", "Silver",
	"Copper", "Swastika", "Platinum","Runed", "Rusty",
	"Curved", "Dragon's claw", "Rosary", "Jade", "Mithril",
	"Ruby", "Emerald", "Sapphire", "Garnet", "Diamond"
};
#else
= {
	"琥珀の","流木の","サンゴの","めのうの","象牙の",
	"黒曜石の","骨の","真鍮の","青銅の","しろめの",
	"べっ甲の","金の","瑠璃の","水晶の","銀の",
	"銅の","卍の", "プラチナの", "ルーンが刻まれた","錆びた",
	"曲がった", "ドラゴンの爪の", "数珠の", "ひすいの", "ミスリルの",
	"ルビーの", "エメラルドの", "サファイアの", "ガーネットの", "ダイアモンドの"
};
#endif

static byte amulet_col[MAX_AMULETS] =
{
	TERM_YELLOW, TERM_L_UMBER, TERM_WHITE, TERM_L_WHITE, TERM_WHITE,
	TERM_L_DARK, TERM_WHITE, TERM_L_UMBER, TERM_L_UMBER, TERM_SLATE,
	TERM_GREEN, TERM_YELLOW, TERM_L_BLUE, TERM_L_BLUE, TERM_L_WHITE,
	TERM_L_UMBER, TERM_VIOLET, TERM_WHITE, TERM_UMBER, TERM_RED,
	TERM_GREEN, TERM_L_GREEN, TERM_L_GREEN, TERM_GREEN, TERM_L_BLUE,
	TERM_RED, TERM_GREEN, TERM_BLUE, TERM_RED, TERM_WHITE
};


/*
 * Staffs (adjectives and colors)
 */
static cptr staff_adj[MAX_WOODS]
#ifndef JP
= {
	"Aspen", "Balsa", "Banyan", "Birch", "Cedar",
	"Cottonwood", "Cypress", "Dogwood", "Elm", "Eucalyptus",
	"Hemlock", "Hickory", "Ironwood", "Locust", "Mahogany",
	"Maple", "Mulberry", "Oak", "Pine", "Redwood",
	"Rosewood", "Spruce", "Sycamore", "Teak", "Walnut",
	"Mistletoe", "Hawthorn", "Bamboo", "Silver", "Runed",
	"Golden", "Ashen", "Ivory","Gnarled"/*,"Willow"*/
};
#else
= {
	"ポプラの","バルサの","バンヤンの","カバの","西洋スギの",
	"檜の","イトスギの","ミズキの","ニレの","ユーカリの",
	"ツガの","ブナの","黒檀の","アカシアの","マホガニーの",
	"カエデの","クワの","カシの","松の","杉の",
	"紫檀の","エゾマツの","イチジクの","チークの","クルミの",
	"ヤドリギの","サンザシの","竹の","銀の","ルーンの",
	"金の","トネリコの", "象牙の", "月桂樹の"
};
#endif

static byte staff_col[MAX_WOODS] =
{
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER, TERM_L_UMBER, TERM_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_RED,
	TERM_RED, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER,
	TERM_GREEN, TERM_L_UMBER, TERM_L_UMBER, TERM_L_WHITE, TERM_UMBER,
	TERM_YELLOW, TERM_SLATE, TERM_WHITE, TERM_SLATE
};


/*
 * Wands (adjectives and colors)
 */
static cptr wand_adj[MAX_METALS]
#ifndef JP
= {
	"Aluminum", "Cast Iron", "Chromium", "Copper", "Gold",
	"Iron", "Magnesium", "Molybdenum", "Nickel", "Rusty",
	"Silver", "Steel", "Tin", "Titanium", "Tungsten",
	"Zirconium", "Zinc", "Aluminum-Plated", "Copper-Plated", "Gold-Plated",
	"Nickel-Plated", "Silver-Plated", "Steel-Plated", "Tin-Plated", "Zinc-Plated",
	"Mithril-Plated", "Mithril", "Runed", "Bronze", "Brass",
	"Platinum", "Lead","Lead-Plated", "Ivory" , "Adamantite",
	"Uridium", "Long", "Short", "Hexagonal", "Carbonized"
};
#else
= {
	"アルミの","鋳鉄の","クロムの","銅の","金の",
	"鉄の","マグネシウムの","モリブデンの","ニッケルの","錆びた",
	"銀の","鋼鉄の","ブリキの","チタンの","タングステンの",
	"ジルコンの","亜鉛の","アルミメッキの","銅メッキの","金メッキの",
	"白銅メッキの","銀メッキの","鉄メッキの","スズメッキの","亜鉛メッキの",
	"ミスリルメッキの","ミスリルの","ルーンが刻まれた","青銅の","真鍮の",
	"プラチナの","鉛の","鉛メッキの","象牙の","アダマンタイトの",
	"イリヂウムの","長い","短い","六角形の", "炭素の"
};
#endif

static byte wand_col[MAX_METALS] =
{
	TERM_L_BLUE, TERM_L_DARK, TERM_WHITE, TERM_L_UMBER, TERM_YELLOW,
	TERM_SLATE, TERM_L_WHITE, TERM_L_WHITE, TERM_L_UMBER, TERM_RED,
	TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE, TERM_WHITE, TERM_WHITE,
	TERM_L_WHITE, TERM_L_WHITE, TERM_L_BLUE, TERM_L_UMBER, TERM_YELLOW,
	TERM_L_UMBER, TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE,
	TERM_L_BLUE, TERM_L_BLUE, TERM_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_WHITE, TERM_SLATE, TERM_SLATE, TERM_WHITE, TERM_VIOLET,
	TERM_L_RED, TERM_L_BLUE, TERM_BLUE, TERM_RED, TERM_L_DARK
};


/*
 * Rods (adjectives and colors).
 * Efficiency -- copied from wand arrays
 */

static cptr rod_adj[MAX_METALS];

static byte rod_col[MAX_METALS];


/*
 * Color adjectives and colors, for potions.
 * Hack -- The first four entries are hard-coded.
 * (water, apple juice, slime mold juice, something)
 */

static cptr potion_adj[MAX_COLORS]
#ifndef JP
= {
	"Clear", "Light Brown", "Icky Green", "xxx",
	"Azure", "Blue", "Blue Speckled", "Black", "Brown", "Brown Speckled",
	"Bubbling", "Chartreuse", "Cloudy", "Copper Speckled", "Crimson", "Cyan",
	"Dark Blue", "Dark Green", "Dark Red", "Gold Speckled", "Green",
	"Green Speckled", "Grey", "Grey Speckled", "Hazy", "Indigo",
	"Light Blue", "Light Green", "Magenta", "Metallic Blue", "Metallic Red",
	"Metallic Green", "Metallic Purple", "Misty", "Orange", "Orange Speckled",
	"Pink", "Pink Speckled", "Puce", "Purple", "Purple Speckled",
	"Red", "Red Speckled", "Silver Speckled", "Smoky", "Tangerine",
	"Violet", "Vermilion", "White", "Yellow", "Violet Speckled",
	"Pungent", "Clotted Red", "Viscous Pink", "Oily Yellow", "Gloopy Green",
	"Shimmering", "Coagulated Crimson", "Yellow Speckled", "Gold",
	"Manly", "Stinking", "Oily Black", "Ichor", "Ivory White", "Sky Blue",
	"Gray", "Silver", "Bronze", "Flashing",
};
#else
= {
	"透明な","薄茶色の","よどんだ緑の","漆黒の",
	"紺碧の","青い","青斑の","黒い","茶色の","茶斑の",
	"泡だった","薄黄緑色の","濁った","銅斑の","紅色の","シアン色の",
	"群青色の","深い緑の","深い赤色の","金斑の","緑色の",
	"緑斑の","灰色の","灰斑の","ぼやけた色の","藍色の",
	"薄い青色の","薄い緑色の","マゼンタ色の","青く輝く","赤く輝く",
	"緑に輝く","紫に輝く","霧状の","オレンジ色の","オレンジ斑の",
	"ピンク色の","ピンク斑の","黒褐色の","紫色の","うすい紫斑の",
	"赤い","赤斑の","銀斑の","煙った","橙色の",
	"スミレ色の","朱色の","白い","黄色い", "紫斑の",
	"刺激臭のする","よどんだ赤の","ドロドロの","ねばった黄色の","暗緑色の",
	"輝く","ねばった深紅の","黄斑の","金色の",
	"男臭い" /*nuke me*/,"悪臭のする","黒油色の","脳漿の",
	"象牙色の","空色の", "ねずみ色の", "銀色の", "赤銅色の",
	"キラキラ光る"
};
#endif

static byte potion_col[MAX_COLORS] =
{
	TERM_WHITE, TERM_L_UMBER, TERM_GREEN, 0,
	TERM_L_BLUE, TERM_BLUE, TERM_BLUE, TERM_L_DARK, TERM_UMBER, TERM_UMBER,
	TERM_L_WHITE, TERM_L_GREEN, TERM_WHITE, TERM_L_UMBER, TERM_RED, TERM_L_BLUE,
	TERM_BLUE, TERM_GREEN, TERM_RED, TERM_YELLOW, TERM_GREEN,
	TERM_GREEN, TERM_SLATE, TERM_SLATE, TERM_L_WHITE, TERM_VIOLET,
	TERM_L_BLUE, TERM_L_GREEN, TERM_RED, TERM_BLUE, TERM_RED,
	TERM_GREEN, TERM_VIOLET, TERM_L_WHITE, TERM_ORANGE, TERM_ORANGE,
	TERM_L_RED, TERM_L_RED, TERM_VIOLET, TERM_VIOLET, TERM_VIOLET,
	TERM_RED, TERM_RED, TERM_L_WHITE, TERM_L_DARK, TERM_ORANGE,
	TERM_VIOLET, TERM_RED, TERM_WHITE, TERM_YELLOW, TERM_VIOLET,
	TERM_L_RED, TERM_RED, TERM_L_RED, TERM_YELLOW, TERM_GREEN,
	TERM_MULTI, TERM_RED, TERM_YELLOW, TERM_YELLOW,
	TERM_L_UMBER, TERM_UMBER, TERM_L_DARK, TERM_RED,
	TERM_WHITE, TERM_L_BLUE, TERM_L_WHITE, TERM_WHITE, TERM_RED,
	TERM_YELLOW
};


/*
 * Syllables for scrolls (must be 1-4 letters each)
 */

static cptr syllables[MAX_SYLLABLES]
#ifdef JP
= {
	"天", "地", "無", "用", "一", "刀", "両", "断",
	"衣", "縫", "日", "千", "秋", "満", "身", "創",
	"痍", "七", "転", "八", "倒", "臥", "薪",
	"嘗", "胆", "起", "死", "回", "生", "文",
	"武", "道", "竜", "頭", "蛇", "尾", "単", "刀", "直", "入",
	"進", "退", "五", "臓", "六", "腑", "大", "山", "鳴",
	"動", "風", "林", "火", "絶", "体", "命", "四",
	"面", "楚", "歌", "完", "全", "欠", "跳", "梁",
	"跋", "扈", "魑", "魅", "魍", "魎", "縦",
	"横", "尽", "神", "出", "鬼", "没", "温", "故", "知",
	"新", "春", "夏", "冬", "純", "真", "垢", "興",
	"味", "津", "確", "比", "苦", "心", "惨", "澹",
	"念", "発", "前", "代", "未", "聞", "空", "後",
	"攫", "金", "森", "羅", "万", "象", "変", "化",
	"十", "中", "八", "九", "人", "色", "試", "行",
	"錯", "誤", "事", "実", "根", "吸", "収", "合",
	"併", "全", "国", "制", "覇", "焼", "肉", "定",
	"食", "骨", "牙", "柳", "飛", "翔", "封", "印",
	"疾", "乱", "武", "将", "軽", "忍", "剣", "能",
	"聖", "邪", "呪", "識", "幻", "感", "毒", "闇",
	"落", "陰", "陽"
};
#else
= {
	"a", "ab", "ag", "aks", "ala", "an", "ankh", "app",
	"arg", "arze", "ash", "aus", "ban", "bar", "bat", "bek",
	"bie", "bin", "bit", "bjor", "blu", "bot", "bu",
	"byt", "comp", "con", "cos", "cre", "dalf", "dan",
	"den", "der", "doe", "dok", "eep", "el", "eng", "er", "ere", "erk",
	"esh", "evs", "fa", "fid", "flit", "for", "fri", "fu", "gan",
	"gar", "glen", "gop", "gre", "ha", "he", "hyd", "i",
	"ing", "ion", "ip", "ish", "it", "ite", "iv", "jo",
	"kho", "kli", "klis", "la", "lech", "man", "mar",
	"me", "mi", "mic", "mik", "mon", "mung", "mur", "nag", "nej",
	"nelg", "nep", "ner", "nes", "nis", "nih", "nin", "o",
	"od", "ood", "org", "orn", "ox", "oxy", "pay", "pet",
	"ple", "plu", "po", "pot", "prok", "re", "rea", "rhov",
	"ri", "ro", "rog", "rok", "rol", "sa", "san", "sat",
	"see", "sef", "seh", "shu", "ski", "sna", "sne", "snik",
	"sno", "so", "sol", "sri", "sta", "sun", "ta", "tab",
	"tem", "ther", "ti", "tox", "trol", "tue", "turs", "u",
	"ulk", "um", "un", "uni", "ur", "val", "viv", "vly",
	"vom", "wah", "wed", "werg", "wex", "whon", "wun", "x",
	"yerg", "yp", "zun", "tri", "blaa", "jah", "bul", "on",
	"foo", "ju", "xuxu"
};
#endif


/*
 * Hold the titles of scrolls, 6 to 14 characters each
 * Also keep an array of scroll colors (always WHITE for now)
 */
static char scroll_adj[MAX_TITLES][16];

static byte scroll_col[MAX_TITLES];


/*
 * Certain items, if aware, are known instantly
 * This function is used only by "flavor_init()"
 */
static bool object_easy_know(int i)
{
	object_kind *k_ptr = &k_info[i];

	/* Analyze the "tval" */
	switch (k_ptr->tval)
	{
		/* Spellbooks */
		case TV_MAGERY_BOOK:
		case TV_FIRE_BOOK:
		case TV_AQUA_BOOK:
		case TV_EARTH_BOOK:
		case TV_WIND_BOOK:
		case TV_HOLY_BOOK:
		case TV_DEATH_BOOK:
		case TV_SYMBIOTIC_BOOK:
		case TV_WITCH_BOOK:
		case TV_DRAKONITE_BOOK:
		case TV_CRUSADE_BOOK:
		{
			return (TRUE);
		}

		/* Simple items */
		case TV_FLASK:
		case TV_JUNK:
		case TV_BOTTLE:
		case TV_SKELETON:
		case TV_SPIKE:
		case TV_STONE:
		case TV_TAROT:
		case TV_SCRATCH_CARD:
		{
			return (TRUE);
		}

		/* All Food, Potions, Scrolls, Rods */
		case TV_FOOD:
		case TV_POTION:
		case TV_SCROLL:
		case TV_ROD:
		{
			return (TRUE);
		}
	}

	/* Nope */
	return (FALSE);
}


/*
 * Certain items have a flavor
 * This function is used only by "flavor_init()"
 */
static bool object_flavor(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Analyze the item */
	switch (k_ptr->tval)
	{
		case TV_AMULET:
		{
			return (0x80 + amulet_col[k_ptr->sval]);
		}

		case TV_RING:
		{
			return (0x90 + ring_col[k_ptr->sval]);
		}

		case TV_STAFF:
		{
			return (0xA0 + staff_col[k_ptr->sval]);
		}

		case TV_WAND:
		{
			return (0xB0 + wand_col[k_ptr->sval]);
		}

		case TV_ROD:
		{
			return (0xC0 + rod_col[k_ptr->sval]);
		}

		case TV_SCROLL:
		{
			return (0xD0 + scroll_col[k_ptr->sval]);
		}

		case TV_POTION:
		{
			return (0xE0 + potion_col[k_ptr->sval]);
		}
	}

	/* No flavor */
	return (0);
}


void get_table_name(char *out_string)
{
#ifdef JP
	char Syllable[80];
	strcpy(out_string, "『");
	get_rnd_line("aname_j.txt", 1, Syllable);
	strcat(out_string, Syllable);
	get_rnd_line("aname_j.txt", 2, Syllable);
	strcat(out_string, Syllable);
	strcat(out_string, "』");
#else
	int testcounter = randint1(3) + 1;

	strcpy(out_string, "'");

	if (randint1(3) == 2)
	{
		while (testcounter--)
			strcat(out_string, syllables[randint0(MAX_SYLLABLES)]);
	}
	else
	{
		char Syllable[80];
		testcounter = randint1(2) + 1;
		while (testcounter--)
		{
			(void)get_rnd_line("elvish.txt", 0, Syllable);
			strcat(out_string, Syllable);
		}
	}

	out_string[1] = toupper(out_string[1]);

	strcat(out_string, "'");
#endif


	out_string[18] = '\0';

	return;
}


/*
 * Prepare the "variable" part of the "k_info" array.
 *
 * The "color"/"metal"/"type" of an item is its "flavor".
 * For the most part, flavors are assigned randomly each game.
 *
 * Initialize descriptions for the "colored" objects, including:
 * Rings, Amulets, Staffs, Wands, Rods, Food, Potions, Scrolls.
 *
 * The first 4 entries for potions are fixed (Water, Apple Juice,
 * Slime Mold Juice, Unused Potion).
 *
 * Scroll titles are always between 6 and 14 letters long.  This is
 * ensured because every title is composed of whole words, where every
 * word is from 1 to 8 letters long (one or two syllables of 1 to 4
 * letters each), and that no scroll is finished until it attempts to
 * grow beyond 15 letters.  The first time this can happen is when the
 * current title has 6 letters and the new word has 8 letters, which
 * would result in a 6 letter scroll title.
 *
 * Duplicate titles are avoided by requiring that no two scrolls share
 * the same first four letters (not the most efficient method, and not
 * the least efficient method, but it will always work).
 *
 * Hack -- make sure everything stays the same for each saved game
 * This is accomplished by the use of a saved "random seed", as in
 * "town_gen()".  Since no other functions are called while the special
 * seed is in effect, so this function is pretty "safe".
 *
 * Note that the "hacked seed" may provide an RNG with alternating parity!
 */
void flavor_init(void)
{
	int     i, j;

	byte    temp_col;

	cptr    temp_adj;


	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant flavors */
	Rand_value = seed_flavor;


	/* Efficiency -- Rods/Wands share initial array */
	for (i = 0; i < MAX_METALS; i++)
	{
		rod_adj[i] = wand_adj[i];
		rod_col[i] = wand_col[i];
	}


	/* Rings have "ring colors" */
	for (i = 0; i < MAX_ROCKS; i++)
	{
		j = randint0(MAX_ROCKS);
		temp_adj = ring_adj[i];
		ring_adj[i] = ring_adj[j];
		ring_adj[j] = temp_adj;

		temp_col = ring_col[i];
		ring_col[i] = ring_col[j];
		ring_col[j] = temp_col;
	}

	/* Amulets have "amulet colors" */
	for (i = 0; i < MAX_AMULETS; i++)
	{
		j = randint0(MAX_AMULETS);
		temp_adj = amulet_adj[i];
		amulet_adj[i] = amulet_adj[j];
		amulet_adj[j] = temp_adj;

		temp_col = amulet_col[i];
		amulet_col[i] = amulet_col[j];
		amulet_col[j] = temp_col;
	}

	/* Staffs */
	for (i = 0; i < MAX_WOODS; i++)
	{
		j = randint0(MAX_WOODS);
		temp_adj = staff_adj[i];
		staff_adj[i] = staff_adj[j];
		staff_adj[j] = temp_adj;

		temp_col = staff_col[i];
		staff_col[i] = staff_col[j];
		staff_col[j] = temp_col;
	}

	/* Wands */
	for (i = 0; i < MAX_METALS; i++)
	{
		j = randint0(MAX_METALS);
		temp_adj = wand_adj[i];
		wand_adj[i] = wand_adj[j];
		wand_adj[j] = temp_adj;

		temp_col = wand_col[i];
		wand_col[i] = wand_col[j];
		wand_col[j] = temp_col;
	}

	/* Rods */
	for (i = 0; i < MAX_METALS; i++)
	{
		j = randint0(MAX_METALS);
		temp_adj = rod_adj[i];
		rod_adj[i] = rod_adj[j];
		rod_adj[j] = temp_adj;

		temp_col = rod_col[i];
		rod_col[i] = rod_col[j];
		rod_col[j] = temp_col;
	}

	/* Potions */
	for (i = 4; i < MAX_COLORS; i++)
	{
		j = randint0(MAX_COLORS - 4) + 4;
		temp_adj = potion_adj[i];
		potion_adj[i] = potion_adj[j];
		potion_adj[j] = temp_adj;

		temp_col = potion_col[i];
		potion_col[i] = potion_col[j];
		potion_col[j] = temp_col;
	}

	/* Scrolls (random titles, always white) */
	for (i = 0; i < MAX_TITLES; i++)
	{
		/* Get a new title */
		while (TRUE)
		{
			char buf[80];

			bool okay;

			/* Start a new title */
			buf[0] = '\0';

			/* Collect words until done */
			while (1)
			{
				int q, s;

				char tmp[80];

				/* Start a new word */
				tmp[0] = '\0';

				/* Choose one or two syllables */
				s = ((randint0(100) < 30) ? 1 : 2);

				/* Add a one or two syllable word */
				for (q = 0; q < s; q++)
				{
					/* Add the syllable */
					strcat(tmp, syllables[randint0(MAX_SYLLABLES)]);
				}

				/* Stop before getting too long */
				if (strlen(buf) + 1 + strlen(tmp) > 15) break;

				/* Add a space */
				strcat(buf, " ");

				/* Add the word */
				strcat(buf, tmp);
			}

			/* Save the title */
			strcpy(scroll_adj[i], buf+1);

			/* Assume okay */
			okay = TRUE;

			/* Check for "duplicate" scroll titles */
			for (j = 0; j < i; j++)
			{
				cptr hack1 = scroll_adj[j];
				cptr hack2 = scroll_adj[i];

				/* Compare first four characters */
				if (*hack1++ != *hack2++) continue;
				if (*hack1++ != *hack2++) continue;
				if (*hack1++ != *hack2++) continue;
				if (*hack1++ != *hack2++) continue;

				/* Not okay */
				okay = FALSE;

				/* Stop looking */
				break;
			}

			/* Break when done */
			if (okay) break;
		}

		/* All scrolls are white */
		scroll_col[i] = TERM_WHITE;
	}


	/* Hack -- Use the "complex" RNG */
	Rand_quick = FALSE;

	/* Analyze every object */
	for (i = 1; i < max_k_idx; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip "empty" objects */
		if (!k_ptr->name) continue;

		/* Extract "flavor" (if any) */
		k_ptr->flavor = object_flavor(i);

		/* No flavor yields aware */
		if (!k_ptr->flavor) k_ptr->aware = TRUE;

		/* Check for "easily known" */
		k_ptr->easy_know = object_easy_know(i);
	}
}


/*
 * Print a char "c" into a string "t", as if by sprintf(t, "%c", c),
 * and return a pointer to the terminator (t + 1).
 */
static char *object_desc_chr(char *t, char c)
{
	/* Copy the char */
	*t++ = c;

	/* Terminate */
	*t = '\0';

	/* Result */
	return (t);
}


/*
 * Print a string "s" into a string "t", as if by strcpy(t, s),
 * and return a pointer to the terminator.
 */
static char *object_desc_str(char *t, cptr s)
{
	/* Copy the string */
	while (*s) *t++ = *s++;

	/* Terminate */
	*t = '\0';

	/* Result */
	return (t);
}



/*
 * Print an unsigned number "n" into a string "t", as if by
 * sprintf(t, "%u", n), and return a pointer to the terminator.
 */
static char *object_desc_num(char *t, uint n)
{
	uint p;

	/* Find "size" of "n" */
	for (p = 1; n >= p * 10; p = p * 10) /* loop */;

	/* Dump each digit */
	while (p >= 1)
	{
		/* Dump the digit */
		*t++ = '0' + n / p;

		/* Remove the digit */
		n = n % p;

		/* Process next digit */
		p = p / 10;
	}

	/* Terminate */
	*t = '\0';

	/* Result */
	return (t);
}




#ifdef JP
/*
 * 日本語の個数表示ルーチン
 *（cmd1.c で流用するために object_desc_japanese から移動した。）
 */

char *object_desc_kosuu(char *t, object_type *o_ptr)
{
	t = object_desc_num(t, o_ptr->number);

	switch (o_ptr->tval)
	{
	case TV_BOLT:
	case TV_ARROW:
	case TV_POLEARM:
	case TV_STAFF:
	case TV_WAND:
	case TV_ROD:
	case TV_DIGGING:
		t = object_desc_str(t, "本");
		break;
	case TV_SCROLL:
		t = object_desc_str(t, "巻");
		break;
	case TV_POTION:
		t = object_desc_str(t, "服");
		break;
	case TV_MAGERY_BOOK:
	case TV_FIRE_BOOK:
	case TV_AQUA_BOOK:
	case TV_EARTH_BOOK:
	case TV_WIND_BOOK:
	case TV_HOLY_BOOK:
	case TV_DEATH_BOOK:
	case TV_SYMBIOTIC_BOOK:
	case TV_WITCH_BOOK:
	case TV_DRAKONITE_BOOK:
	case TV_CRUSADE_BOOK:
		t = object_desc_str(t, "冊");
		break;
	case TV_SOFT_ARMOR:
	case TV_HARD_ARMOR:
	case TV_CLOAK:
		t = object_desc_str(t, "着");
		break;
	case TV_SWORD:
	case TV_HAFTED:
	case TV_BOW:
		t = object_desc_str(t, "振");
		break;
	case TV_BOOTS:
		t = object_desc_str(t, "足");
		break;
	case TV_CARD:
	case TV_TAROT:
	case TV_SCRATCH_CARD:
		t = object_desc_str(t, "枚");
		break;
	/* 食べもの by ita */
	case TV_FOOD:
		if (o_ptr->sval == SV_FOOD_JERKY)
		{
			t = object_desc_str(t, "切れ");
			break;
		}
	default:
		if (o_ptr->number < 10)
		{
			t = object_desc_str(t, "つ");
		}
		else
		{
			t = object_desc_str(t, "個");
		}
		break;
	}
	return (t);
}
#endif

/*
 * Print an signed number "v" into a string "t", as if by
 * sprintf(t, "%+d", n), and return a pointer to the terminator.
 * Note that we always print a sign, either "+" or "-".
 */
static char *object_desc_int(char *t, sint v)
{
	uint p, n;

	/* Negative */
	if (v < 0)
	{
		/* Take the absolute value */
		n = 0 - v;

		/* Use a "minus" sign */
		*t++ = '-';
	}

	/* Positive (or zero) */
	else
	{
		/* Use the actual number */
		n = v;

		/* Use a "plus" sign */
		*t++ = '+';
	}

	/* Find "size" of "n" */
	for (p = 1; n >= p * 10; p = p * 10) /* loop */;

	/* Dump each digit */
	while (p >= 1)
	{
		/* Dump the digit */
		*t++ = '0' + n / p;

		/* Remove the digit */
		n = n % p;

		/* Process next digit */
		p = p / 10;
	}

	/* Terminate */
	*t = '\0';

	/* Result */
	return (t);
}


/*
 * Structs and tables for Auto Inscription for flags
 */

typedef struct flag_insc_table
{
#ifdef JP
	cptr japanese;
#endif
	cptr english;
	int flag;
	int except_flag;
} flag_insc_table;

#ifdef JP
static flag_insc_table flag_insc_plus[] =
{
	{ "攻", "At", TR_BLOWS, -1 },
	{ "速", "Sp", TR_SPEED, -1 },
	{ "封", "Am", TR_ANTI_MAGIC, -1 },
	{ "腕", "St", TR_STR, -1 },
	{ "知", "In", TR_INT, -1 },
	{ "賢", "Wi", TR_WIS, -1 },
	{ "器", "Dx", TR_DEX, -1 },
	{ "耐", "Cn", TR_CON, -1 },
	{ "魅", "Ch", TR_CHR, -1 },
	{ "道", "Md", TR_MAGIC_MASTERY, -1 },
	{ "隠", "Sl", TR_STEALTH, -1 },
	{ "探", "Sr", TR_SEARCH, -1 },
	{ "赤", "If", TR_INFRA, -1 },
	{ "掘", "Dg", TR_TUNNEL, -1 },
	{ NULL, NULL, 0, -1 }
};

static flag_insc_table flag_insc_immune[] =
{
	{ "酸", "Ac", TR_IM_ACID, -1 },
	{ "電", "El", TR_IM_ELEC, -1 },
	{ "火", "Fi", TR_IM_FIRE, -1 },
	{ "冷", "Co", TR_IM_COLD, -1 },
	{ NULL, NULL, 0, -1, }
};

static flag_insc_table flag_insc_resistance[] =
{
	{ "酸", "Ac", TR_RES_ACID, TR_IM_ACID },
	{ "電", "El", TR_RES_ELEC, TR_IM_ELEC },
	{ "火", "Fi", TR_RES_FIRE, TR_IM_FIRE },
	{ "冷", "Co", TR_RES_COLD, TR_IM_COLD },
	{ "毒", "Po", TR_RES_POIS, -1 },
	{ "閃", "Li", TR_RES_LITE, -1 },
	{ "暗", "Dk", TR_RES_DARK, -1 },
	{ "破", "Sh", TR_RES_SHARDS, -1 },
	{ "盲", "Bl", TR_RES_BLIND, -1 },
	{ "乱", "Cf", TR_RES_CONF, -1 },
	{ "轟", "So", TR_RES_SOUND, -1 },
	{ "獄", "Nt", TR_RES_NETHER, -1 },
	{ "石", "St", TR_RES_STONE, -1 },
	{ "沌", "Ca", TR_RES_CHAOS, -1 },
	{ "劣", "Di", TR_RES_DISEN, -1 },
	{ "恐", "Fe", TR_RES_FEAR, -1 },
	{ NULL, NULL, 0, -1 }
};

static flag_insc_table flag_insc_misc[] =
{
	{ "魔力", "Ma", TR_DEC_MANA, -1 },
	{ "投", "Th", TR_THROW, -1 },
	{ "反", "Rf", TR_REFLECT, -1 },
	{ "麻", "Fa", TR_FREE_ACT, -1 },
	{ "視", "Si", TR_SEE_INVIS, -1 },
	{ "経", "Hl", TR_HOLD_LIFE, -1 },
	{ "感", "Esp", TR_TELEPATHY, -1 },
	{ "遅", "Sd", TR_SLOW_DIGEST, -1 },
	{ "活", "Rg", TR_REGEN, -1 },
	{ "精", "Rm", TR_REGEN_MANA, -1 },
	{ "浮", "Lv", TR_FEATHER, -1 },
	{ "明", "Lu", TR_LITE, -1 },
	{ "警", "Wr", TR_WARNING, -1 },
	{ "倍", "Xm", TR_XTRA_MIGHT, -1 },
	{ "射", "Xs", TR_XTRA_SHOTS, -1 },
	{ "退", "De", TR_DRAIN_EXP, -1 },
	{ "怒", "Ag", TR_AGGRAVATE, -1 },
	{ "祝", "Bs", TR_BLESSED, -1 },
	{ "穢", "Uh", TR_UNHOLY, -1 },
	{ "忌", "Ty", TR_TY_CURSE, -1 },
	{ "易", "Ea", TR_EASY_SPELL, -1 },
	{ "幽", "Wf", TR_WRAITH, -1 },
	{ "怖", "Ff", TR_FEAR_FIELD, -1 },
	{ "女", "Fo", TR_FEMALE_ONLY, -1 },
	{ "男", "Mo", TR_MALE_ONLY, -1 },
	{ NULL, NULL, 0, -1 }
};

static flag_insc_table flag_insc_aura[] =
{
	{ "炎", "F", TR_SH_FIRE, -1 },
	{ "電", "E", TR_SH_ELEC, -1 },
	{ "冷", "C", TR_SH_COLD, -1 },
	{ "魔", "M", TR_NO_MAGIC, -1 },
	{ "瞬", "T", TR_NO_TELE, -1 },
	{ "防", "R", TR_RES_MAGIC, -1 },
	{ NULL, NULL, 0, -1 }
};

static flag_insc_table flag_insc_xbrand[] =
{
	{ "斬", "S", TR_EXTRA_VORPAL, -1 },
	{ NULL, NULL, 0, -1 }
};

static flag_insc_table flag_insc_brand[] =
{
	{ "酸", "A", TR_BRAND_ACID, -1 },
	{ "電", "E", TR_BRAND_ELEC, -1 },
	{ "焼", "F", TR_BRAND_FIRE, -1 },
	{ "凍", "Co", TR_BRAND_COLD, -1 },
	{ "毒", "P", TR_BRAND_POIS, -1 },
	{ "沌", "Ca", TR_CHAOTIC, -1 },
	{ "吸", "V", TR_VAMPIRIC, -1 },
	{ "震", "Q", TR_IMPACT, -1 },
	{ "切", "S", TR_VORPAL, TR_EXTRA_VORPAL },
	{ "理", "M", TR_FORCE_WEAPON, -1 },
	{ NULL, NULL, 0, -1 }
};

static flag_insc_table flag_insc_kill[] =
{
	{ "龍", "D", TR_KILL_DRAGON, -1 },
	{ NULL, NULL, 0, -1 }
};

static flag_insc_table flag_insc_slay[] =
{
	{ "邪", "*", TR_SLAY_EVIL, -1 },
	{ "善", "A", TR_SLAY_GOOD, -1 },
	{ "人", "p", TR_SLAY_HUMAN, -1 },
	{ "竜", "d", TR_SLAY_DRAGON, TR_KILL_DRAGON },
	{ "オ", "o", TR_SLAY_ORC, -1 },
	{ "ト", "T", TR_SLAY_TROLL, -1 },
	{ "巨", "P", TR_SLAY_GIANT, -1 },
	{ "デ", "U", TR_SLAY_DEMON, -1 },
	{ "生", "Y", TR_SLAY_LIVING, -1 },
	{ "死", "L", TR_SLAY_UNDEAD, -1 },
	{ "動", "Z", TR_SLAY_ANIMAL, -1 },
	{ NULL, NULL, 0, -1 }
};

static flag_insc_table flag_insc_sust[] =
{
	{ "腕", "St", TR_SUST_STR, -1 },
	{ "知", "In", TR_SUST_INT, -1 },
	{ "賢", "Wi", TR_SUST_WIS, -1 },
	{ "器", "Dx", TR_SUST_DEX, -1 },
	{ "耐", "Cn", TR_SUST_CON, -1 },
	{ "魅", "Ch", TR_SUST_CHR, -1 },
	{ NULL, NULL, 0, -1 }
};

#else
static flag_insc_table flag_insc_plus[] =
{
	{ "At", TR_BLOWS, -1 },
	{ "Sp", TR_SPEED, -1 },
	{ "Am", TR_ANTI_MAGIC, -1 },
	{ "St", TR_STR, -1 },
	{ "In", TR_INT, -1 },
	{ "Wi", TR_WIS, -1 },
	{ "Dx", TR_DEX, -1 },
	{ "Cn", TR_CON, -1 },
	{ "Ch", TR_CHR, -1 },
	{ "Md", TR_MAGIC_MASTERY, -1 },
	{ "Sl", TR_STEALTH, -1 },
	{ "Sr", TR_SEARCH, -1 },
	{ "If", TR_INFRA, -1 },
	{ "Dg", TR_TUNNEL, -1 },
	{ NULL, 0, -1 }
};

static flag_insc_table flag_insc_immune[] =
{
	{ "Ac", TR_IM_ACID, -1 },
	{ "El", TR_IM_ELEC, -1 },
	{ "Fi", TR_IM_FIRE, -1 },
	{ "Co", TR_IM_COLD, -1 },
	{ NULL, 0, -1 }
};

static flag_insc_table flag_insc_resistance[] =
{
	{ "Ac", TR_RES_ACID, TR_IM_ACID },
	{ "El", TR_RES_ELEC, TR_IM_ELEC },
	{ "Fi", TR_RES_FIRE, TR_IM_FIRE },
	{ "Co", TR_RES_COLD, TR_IM_COLD },
	{ "Po", TR_RES_POIS, -1 },
	{ "Li", TR_RES_LITE, -1 },
	{ "Dk", TR_RES_DARK, -1 },
	{ "Sh", TR_RES_SHARDS, -1 },
	{ "Bl", TR_RES_BLIND, -1 },
	{ "Cf", TR_RES_CONF, -1 },
	{ "So", TR_RES_SOUND, -1 },
	{ "Nt", TR_RES_NETHER, -1 },
	{ "St", TR_RES_STONE, -1 },
	{ "Ca", TR_RES_CHAOS, -1 },
	{ "Di", TR_RES_DISEN, -1 },
	{ "Fe", TR_RES_FEAR, -1 },
	{ NULL, 0, -1 }
};

static flag_insc_table flag_insc_misc[] =
{
	{ "Ma", TR_DEC_MANA, -1 },
	{ "Th", TR_THROW, -1 },
	{ "Rf", TR_REFLECT, -1 },
	{ "Fa", TR_FREE_ACT, -1 },
	{ "Si", TR_SEE_INVIS, -1 },
	{ "Hl", TR_HOLD_LIFE, -1 },
	{ "Esp", TR_TELEPATHY, -1 },
	{ "Sd", TR_SLOW_DIGEST, -1 },
	{ "Rg", TR_REGEN, -1 },
	{ "Rm", TR_REGEN_MANA, -1 },
	{ "Lv", TR_FEATHER, -1 },
	{ "Lu", TR_LITE, -1 },
	{ "Wr", TR_WARNING, -1 },
	{ "Xm", TR_XTRA_MIGHT, -1 },
	{ "Xs", TR_XTRA_SHOTS, -1 },
	{ "De", TR_DRAIN_EXP, -1 },
	{ "Ag", TR_AGGRAVATE, -1 },
	{ "Bs", TR_BLESSED, -1 },
	{ "Uh", TR_UNHOLY, -1 },
	{ "Ty", TR_TY_CURSE, -1 },
	{ "Ea", TR_EASY_SPELL, -1 },
	{ "Wf", TR_WRAITH, -1 },
	{ "Ff", TR_FEAR_FIELD, -1 },
	{ "Fo", TR_FEMALE_ONLY, -1 },
	{ "Mo", TR_MALE_ONLY, -1 },
	{ NULL, 0, -1 }
};

static flag_insc_table flag_insc_aura[] =
{
	{ "F", TR_SH_FIRE, -1 },
	{ "E", TR_SH_ELEC, -1 },
	{ "C", TR_SH_COLD, -1 },
	{ "M", TR_NO_MAGIC, -1 },
	{ "T", TR_NO_TELE, -1 },
	{ "R", TR_RES_MAGIC, -1 },
	{ NULL, 0, -1 }
};

static flag_insc_table flag_insc_xbrand[] =
{
	{ "S", TR_EXTRA_VORPAL, -1 },
	{ NULL, 0, -1 }
};

static flag_insc_table flag_insc_brand[] =
{
	{ "A", TR_BRAND_ACID, -1 },
	{ "E", TR_BRAND_ELEC, -1 },
	{ "F", TR_BRAND_FIRE, -1 },
	{ "Co", TR_BRAND_COLD, -1 },
	{ "P", TR_BRAND_POIS, -1 },
	{ "Ca", TR_CHAOTIC, -1 },
	{ "V", TR_VAMPIRIC, -1 },
	{ "Q", TR_IMPACT, -1 },
	{ "S", TR_VORPAL, TR_EXTRA_VORPAL },
	{ "M", TR_FORCE_WEAPON, -1 },
	{ NULL, 0, -1 }
};

static flag_insc_table flag_insc_kill[] =
{
	{ "D", TR_KILL_DRAGON, -1 },
	{ NULL, 0, -1 }
};

static flag_insc_table flag_insc_slay[] =
{
	{ "*", TR_SLAY_EVIL, -1 },
	{ "A", TR_SLAY_GOOD, -1 },
	{ "p", TR_SLAY_HUMAN, -1 },
	{ "d", TR_SLAY_DRAGON, TR_KILL_DRAGON },
	{ "o", TR_SLAY_ORC, -1 },
	{ "T", TR_SLAY_TROLL, -1 },
	{ "P", TR_SLAY_GIANT, -1 },
	{ "U", TR_SLAY_DEMON, -1 },
	{ "Y", TR_SLAY_LIVING, -1 },
	{ "L", TR_SLAY_UNDEAD, -1 },
	{ "Z", TR_SLAY_ANIMAL, -1 },
	{ NULL, 0, -1 }
};

static flag_insc_table flag_insc_sust[] =
{
	{ "St", TR_SUST_STR, -1 },
	{ "In", TR_SUST_INT, -1 },
	{ "Wi", TR_SUST_WIS, -1 },
	{ "Dx", TR_SUST_DEX, -1 },
	{ "Cn", TR_SUST_CON, -1 },
	{ "Ch", TR_SUST_CHR, -1 },
	{ NULL, 0, -1 }
};
#endif

/* Simple macro for get_inscription() */
#define ADD_INSC(STR) (void)(ptr = object_desc_str(ptr, (STR)))

/*
 *  Helper function for get_inscription()
 */
static char *inscribe_flags_aux(flag_insc_table *fi_ptr, u32b flgs[TR_FLAG_SIZE], bool kanji, char *ptr)
{
	while (fi_ptr->english)
	{
		if (have_flag(flgs, fi_ptr->flag) &&
		    (fi_ptr->except_flag == -1 || !have_flag(flgs, fi_ptr->except_flag)))
#ifdef JP
			ADD_INSC(kanji ? fi_ptr->japanese : fi_ptr->english);
#else
			ADD_INSC(fi_ptr->english);
#endif
		fi_ptr++;
	}

	return ptr;
}


/*
 *  Special variation of have_flag for auto-inscription
 */
static bool have_flag_of(flag_insc_table *fi_ptr, u32b flgs[TR_FLAG_SIZE])
{
	while (fi_ptr->english)
	{
		if (have_flag(flgs, fi_ptr->flag) &&
		   (fi_ptr->except_flag == -1 || !have_flag(flgs, fi_ptr->except_flag)))
			return (TRUE);
		fi_ptr++;
	}

	return (FALSE);
}


/*
 *  Get object inscription with auto inscription of object flags.
 */
static void get_inscription(char *buff, object_type *o_ptr)
{
	cptr insc = quark_str(o_ptr->inscription);
	char *ptr = buff;
	char *prev_ptr = buff;

	u32b flgs[TR_FLAG_SIZE];

	/* Not fully identified */
	if (!(o_ptr->ident & IDENT_MENTAL))
	{
		/* Copy until end of line or '#' */
		while (*insc)
		{
			if (*insc == '#') break;
#ifdef JP
			if (iskanji(*insc)) *buff++ = *insc++;
#endif
			*buff++ = *insc++;
		}

		*buff = '\0';
		return;
	}

	/* Extract the flags */
	object_flags(o_ptr, flgs);


	*buff = '\0';
	for (; *insc; insc++)
	{
		bool kanji = FALSE;
		bool all;

		/* Ignore fake artifact inscription */
		if (*insc == '#') break;

		/* {%} will be automatically converted */
		else if ('%' == *insc)
		{
			cptr start_percent = ptr;
#ifdef JP
			if ('%' == insc[1])
			{
				insc++;
				kanji = FALSE;
			}
			else
			{
				kanji = TRUE;
			}
#endif
			if ('a' == insc[1] && 'l' == insc[2] && 'l' == insc[3])
			{
				all = TRUE;
				insc += 3;
			}
			else
			{
				all = FALSE;
			}

			/* check for too long inscription */
			if (ptr >= buff + MAX_NLEN) continue;

			/* Remove obvious flags */
			if (!all)
			{
				object_kind *k_ptr = &k_info[o_ptr->k_idx];
				int j;

				/* Base object */
				for (j = 0; j < TR_FLAG_SIZE; j++)
					flgs[j] &= ~k_ptr->flags[j];

				if (o_ptr->name1)
				{
					artifact_type *a_ptr = &a_info[o_ptr->name1];

					for (j = 0; j < TR_FLAG_SIZE; j++)
						flgs[j] &= ~a_ptr->flags[j];
				}

				if (o_ptr->name2)
				{
					bool teleport = have_flag(flgs, TR_TELEPORT);
					ego_item_type *e_ptr = &e_info[o_ptr->name2];

					for (j = 0; j < TR_FLAG_SIZE; j++)
						flgs[j] &= ~e_ptr->flags[j];

					/* Always inscribe {.} for random teleport */
					if (teleport) add_flag(flgs, TR_TELEPORT);
				}
			}


			/* Plusses */
			if (have_flag_of(flag_insc_plus, flgs))
			{
				if (kanji)
					ADD_INSC("+");
			}
			ptr = inscribe_flags_aux(flag_insc_plus, flgs, kanji, ptr);

			/* Immunity */
			if (have_flag_of(flag_insc_immune, flgs))
			{
				if (!kanji && ptr != prev_ptr)
				{
					ADD_INSC(";");
					prev_ptr = ptr;
				}
				ADD_INSC("*");
			}
			ptr = inscribe_flags_aux(flag_insc_immune, flgs, kanji, ptr);

			/* Resistance */
			if (have_flag_of(flag_insc_resistance, flgs))
			{
				if (kanji)
					ADD_INSC("r");
				else if (ptr != prev_ptr)
				{
					ADD_INSC(";");
					prev_ptr = ptr;
				}
			}
			ptr = inscribe_flags_aux(flag_insc_resistance, flgs, kanji, ptr);

			/* Misc Ability */
			if (have_flag_of(flag_insc_misc, flgs))
			{
				if (ptr != prev_ptr)
				{
					ADD_INSC(";");
					prev_ptr = ptr;
				}
			}
			ptr = inscribe_flags_aux(flag_insc_misc, flgs, kanji, ptr);

			/* Aura */
			if (have_flag_of(flag_insc_aura, flgs))
			{
				ADD_INSC("[");
			}
			ptr = inscribe_flags_aux(flag_insc_aura, flgs, kanji, ptr);

			/* Extra Brand Weapon */
			if (have_flag_of(flag_insc_xbrand, flgs))
				ADD_INSC("|X");
			ptr = inscribe_flags_aux(flag_insc_xbrand, flgs, kanji, ptr);

			/* Brand Weapon */
			if (have_flag_of(flag_insc_brand, flgs))
				ADD_INSC("|");
			ptr = inscribe_flags_aux(flag_insc_brand, flgs, kanji, ptr);

			/* Kill Weapon */
			if (have_flag_of(flag_insc_kill, flgs))
				ADD_INSC("/X");
			ptr = inscribe_flags_aux(flag_insc_kill, flgs, kanji, ptr);

			/* Slay Weapon */
			if (have_flag_of(flag_insc_slay, flgs))
				ADD_INSC("/");
			ptr = inscribe_flags_aux(flag_insc_slay, flgs, kanji, ptr);

			/* Random Teleport */
			if (have_flag(flgs, TR_TELEPORT))
			{
				ADD_INSC(".");
			}

			/* Sustain */
			if (have_flag_of(flag_insc_sust, flgs))
			{
				ADD_INSC("(");
			}
			ptr = inscribe_flags_aux(flag_insc_sust, flgs, kanji, ptr);

			if (ptr == start_percent)
				ADD_INSC(" ");
		}
		else
		{
			*ptr++ = *insc;
		}
	}
	*ptr = '\0';
}


#ifdef JP
#undef strchr
#define strchr strchr_j
#endif


/*
 * Variables and functions to print bonuses
 */
void construct_bonus_list(object_type *o_ptr, bonus_list_type bonus_list[A_MAX + OB_MAX + 1],
                          cptr bonus_desc[A_MAX + OB_MAX], cptr to_all_stats_desc)
{
	u32b            flgs[TR_FLAG_SIZE];
	int             i, j;
	bool            all_stats = TRUE;
	s16b            to_stat_0 = o_ptr->to_stat[0];
	bonus_list_type *cur_bonus;
	int             bonus_num = 0;
	cptr            *to_stat_desc = bonus_desc;
	cptr            *to_misc_desc = bonus_desc + A_MAX;
	int             desc_num[A_MAX + OB_MAX + 1];

	C_WIPE(desc_num, A_MAX + OB_MAX + 1, int);

	/* Extract some flags */
	object_flags(o_ptr, flgs);

	/* Affect to all stats? */
	for (i = 0; (i < A_MAX) && all_stats; i++)
	{
		if (!have_flag(flgs, a_to_tr[i]) || (to_stat_0 != o_ptr->to_stat[i])) all_stats = FALSE;
	}

	if (all_stats)
	{
		cur_bonus = &bonus_list[bonus_num];
		cur_bonus->to_bonus = to_stat_0;
		cur_bonus->desc[desc_num[bonus_num++]++] = to_all_stats_desc;
	}
	else
	{
		/* Scan stats bonus */
		for (i = 0; i < A_MAX; i++)
		{
			if (have_flag(flgs, a_to_tr[i]))
			{
				for (j = 0; j < bonus_num; j++)
				{
					if (bonus_list[j].to_bonus == o_ptr->to_stat[i]) break;
				}
				cur_bonus = &bonus_list[j];
				cur_bonus->desc[desc_num[j]++] = to_stat_desc[i];
				if (j == bonus_num)
				{
					cur_bonus->to_bonus = o_ptr->to_stat[i];
					bonus_num++;
				}
			}
		}
	}

	/* Scan misc bonus */
	for (i = 0; i < OB_MAX; i++)
	{
		if (have_flag(flgs, ob_to_tr[i]))
		{
			for (j = 0; j < bonus_num; j++)
			{
				if (bonus_list[j].to_bonus == o_ptr->to_misc[i]) break;
			}
			cur_bonus = &bonus_list[j];
			cur_bonus->desc[desc_num[j]++] = to_misc_desc[i];
			if (j == bonus_num)
			{
				cur_bonus->to_bonus = o_ptr->to_misc[i];
				bonus_num++;
			}
		}
	}

	/* Terminate */
	for (i = 0; i <= bonus_num; i++) bonus_list[i].desc[desc_num[i]] = NULL;
}

static cptr to_desc[A_MAX + OB_MAX] =
{
#ifdef JP
	/* to-stat */
	"腕",
	"知",
	"賢",
	"器",
	"耐",
	"魅",

	/* to-misc */
	"道",
	"隠",
	"探",
	"赤",
	"掘",
	"速",
	"攻",
	"封",
#else
	/* to-stat */
	"St",
	"In",
	"Wi",
	"Dx",
	"Cn",
	"Ch",

	/* to-misc */
	"Md",
	"St",
	"Sl",
	"If",
	"Dg",
	"Sp",
	"At",
	"Am",
#endif
};

static char *object_desc_bonus(char *t, object_type *o_ptr)
{
	char            p1 = '(', p2 = ')';
	int             i, j;
	bonus_list_type *bonus_list, *cur_bonus;
#ifdef JP
	cptr            to_all_stats_desc = "全";
#else
	cptr            to_all_stats_desc = "All";
#endif

	C_MAKE(bonus_list, A_MAX + OB_MAX + 1, bonus_list_type);

	construct_bonus_list(o_ptr, bonus_list, to_desc, to_all_stats_desc);

	cur_bonus = &bonus_list[0];
	if (!cur_bonus->desc[0]) return t;

	/* Start the display */
	t = object_desc_chr(t, ' ');
	t = object_desc_chr(t, p1);

	/* Dump the value itself */
	t = object_desc_int(t, cur_bonus->to_bonus);

	for (j = 0; cur_bonus->desc[j]; j++) t = object_desc_str(t, cur_bonus->desc[j]);

	for (i = 1, cur_bonus = &bonus_list[1]; (i < (A_MAX + OB_MAX)) && cur_bonus->desc[0]; cur_bonus = &bonus_list[++i])
	{
		t = object_desc_chr(t, ',');

		/* Dump the value itself */
		t = object_desc_int(t, cur_bonus->to_bonus);

		for (j = 0; cur_bonus->desc[j]; j++) t = object_desc_str(t, cur_bonus->desc[j]);
	}

	/* Finish the display */
	t = object_desc_chr(t, p2);

	C_KILL(bonus_list, A_MAX + OB_MAX + 1, bonus_list_type);

	return t;
}

/*
 * Creates a description of the item "o_ptr", and stores it in "out_val".
 *
 * One can choose the "verbosity" of the description, including whether
 * or not the "number" of items should be described, and how much detail
 * should be used when describing the item.
 *
 * The given "buf" must be MAX_NLEN chars long to hold the longest possible
 * description, which can get pretty long, including incriptions, such as:
 * "no more Maces of Disruption (Defender) (+10,+10) [+5] (+3 to stealth)".
 * Note that the inscription will be clipped to keep the total description
 * under MAX_NLEN-1 chars (plus a terminator).
 *
 * Note the use of "object_desc_num()" and "object_desc_int()" as hyper-efficient,
 * portable, versions of some common "sprintf()" commands.
 *
 * Note that all ego-items (when known) append an "Ego-Item Name", unless
 * the item is also an artifact, which should NEVER happen.
 *
 * Note that all artifacts (when known) append an "Artifact Name", so we
 * have special processing for "Specials" (artifact Lites, Rings, Amulets).
 * The "Specials" never use "modifiers" if they are "known", since they
 * have special "descriptions", such as "The Necklace of the Dwarves".
 *
 * Special Lite's use the "k_info" base-name (Phial, Star, or Arkenstone),
 * plus the artifact name, just like any other artifact, if known.
 *
 * Special Ring's and Amulet's, if not "aware", use the same code as normal
 * rings and amulets, and if "aware", use the "k_info" base-name (Ring or
 * Amulet or Necklace).  They will NEVER "append" the "k_info" name.  But,
 * they will append the artifact name, just like any artifact, if known.
 *
 * Hack -- Display "The One Ring" as "a Plain Gold Ring" until aware.
 *
 * If "pref" then a "numeric" prefix will be pre-pended.
 *
 * Mode:
 *   0 -- The Cloak of Death
 *   1 -- The Cloak of Death [1,+3]
 *   2 -- The Cloak of Death [1,+3] (+2 to Stealth)
 *   3 -- The Cloak of Death [1,+3] (+2 to Stealth) {nifty}
 */
void object_desc(char *buf, object_type *o_ptr, int pref, int mode)
{
	cptr            basenm, modstr;
	int             power, indexx, i;

	bool            aware = FALSE;
	bool            known = FALSE;

	bool            append_name = FALSE;

	bool            show_weapon = FALSE;
	bool            show_armour = FALSE;

	cptr            s, u;
	char            *t;

	char            p1 = '(', p2 = ')';
	char            b1 = '[', b2 = ']';
	char            c1 = '{', c2 = '}';

	char            tmp_val[MAX_NLEN+160];
	char            tmp_val2[MAX_NLEN+10];

	u32b flgs[TR_FLAG_SIZE];

	object_type	*bow_ptr;


	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Extract some flags */
	object_flags(o_ptr, flgs);

	/* See if the object is "aware" */
	if (object_aware_p(o_ptr) || (o_ptr->ident & IDENT_MENTAL)) aware = TRUE;

	/* See if the object is "known" */
	if (object_known_p(o_ptr)) known = TRUE;

	/* Hack -- Extract the sub-type "indexx" */
	indexx = o_ptr->sval;

	/* Extract default "base" string */
	basenm = get_object_name(o_ptr);

	/* Assume no "modifier" string */
	modstr = "";


	/* Analyze the object */
	switch (o_ptr->tval)
	{
		/* Some objects are easy to describe */
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		case TV_SPIKE:
		case TV_FLASK:
		case TV_CHEST:
		case TV_STONE:
		case TV_FOOD:
		case TV_SCRATCH_CARD:
		{
			break;
		}

		/* Tarot Cards */
		case TV_TAROT:
		{
			modstr = tarot_info[o_ptr->pval].name;
			break;
		}

		/* Figurines/Statues */
		case TV_FIGURINE:
		case TV_STATUE:
		{
			monster_race *r_ptr = &r_info[o_ptr->pval];

#ifdef JP
			modstr = r_name + r_ptr->name;
#else
			cptr t = r_name + r_ptr->name;

			if (!(r_ptr->flags1 & RF1_UNIQUE))
			{
				sprintf(tmp_val2, "%s%s", (is_a_vowel(*t) ? "an " : "a "), t);

				modstr = tmp_val2;
			}
			else
			{
				modstr = t;
			}
#endif


			break;
		}

		/* Corpses */
		case TV_CORPSE:
		{
			monster_race *r_ptr = &r_info[o_ptr->pval];

			modstr = r_name + r_ptr->name;


#ifdef JP
			sprintf(tmp_val2, "#%s", basenm);
#else
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				sprintf(tmp_val2, "& %s %s", basenm, "of #");
			}
			else
			{
				sprintf(tmp_val2, "& # %s", basenm);
			}
#endif


			basenm = tmp_val2;
			break;
		}

		/* Missiles/ Bows/ Weapons */
		case TV_BULLET:
		case TV_ROUND:
		case TV_SHELL:
		case TV_BOLT:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			show_weapon = TRUE;
			break;
		}

		/* Rockets */
		case TV_ROCKET:
		{
			tmp_val2[0] = '\0';

			switch (o_ptr->xtra4)
			{
			case ROCKET_MATERIAL_NORMAL:
			case ROCKET_MATERIAL_NORMAL + ROCKET_ANTIGRAV:
#ifdef JP
				strcpy(tmp_val2, "散弾製");
#else
				strcpy(tmp_val2, "Shot-shell-made ");
#endif
				break;

			case ROCKET_MATERIAL_BALDAR:
			case ROCKET_MATERIAL_BALDAR + ROCKET_ANTIGRAV:
#ifdef JP
				strcpy(tmp_val2, "バルダー製");
#else
				strcpy(tmp_val2, "Baldar ");
#endif
				break;

			case ROCKET_MATERIAL_MITHRIL:
			case ROCKET_MATERIAL_MITHRIL + ROCKET_ANTIGRAV:
#ifdef JP
				strcpy(tmp_val2, "ミスリル製");
#else
				strcpy(tmp_val2, "Mithril ");
#endif
				break;

			case ROCKET_MATERIAL_ADAMANTITE:
			case ROCKET_MATERIAL_ADAMANTITE + ROCKET_ANTIGRAV:
#ifdef JP
				strcpy(tmp_val2, "アダマンタイト製");
#else
				strcpy(tmp_val2, "Adamantite ");
#endif
				break;
			}

			switch ((int)o_ptr->xtra3 - 1)
			{
			case TR_FORCE_WEAPON:
#ifdef JP
				strcat(tmp_val2, "魔力");
#else
				strcat(tmp_val2, "Mana ");
#endif
				break;

			case TR_CHAOTIC:
#ifdef JP
				strcat(tmp_val2, "カオス");
#else
				strcat(tmp_val2, "Chaotic ");
#endif
				break;

			case TR_VAMPIRIC:
#ifdef JP
				strcat(tmp_val2, "生命力吸収");
#else
				strcat(tmp_val2, "Life-drain ");
#endif
				break;

			case TR_SLAY_EVIL:
#ifdef JP
				strcat(tmp_val2, "神聖");
#else
				strcat(tmp_val2, "Holy ");
#endif
				break;

			case TR_IMPACT:
#ifdef JP
				strcat(tmp_val2, "粉砕");
#else
				strcat(tmp_val2, "Shattering ");
#endif
				break;

			case TR_BRAND_POIS:
#ifdef JP
				strcat(tmp_val2, "ガス");
#else
				strcat(tmp_val2, "Gas ");
#endif
				break;

			case TR_BRAND_ACID:
#ifdef JP
				strcat(tmp_val2, "酸性");
#else
				strcat(tmp_val2, "Acid ");
#endif
				break;

			case TR_BRAND_ELEC:
#ifdef JP
				strcat(tmp_val2, "電撃");
#else
				strcat(tmp_val2, "Lightning ");
#endif
				break;

			case TR_BRAND_FIRE:
#ifdef JP
				strcat(tmp_val2, "火炎");
#else
				strcat(tmp_val2, "Fire ");
#endif
				break;

			case TR_BRAND_COLD:
#ifdef JP
				strcat(tmp_val2, "凍結");
#else
				strcat(tmp_val2, "Freezing ");
#endif
				break;

			case TR_SLAY_GOOD:
#ifdef JP
				strcat(tmp_val2, "邪悪");
#else
				strcat(tmp_val2, "Evil ");
#endif
				break;

			default:
				if (o_ptr->xtra4 >= ROCKET_ANTIGRAV)
				{
#ifdef JP
					strcat(tmp_val2, "反重力");
#else
					strcat(tmp_val2, "Anti-gravity ");
#endif
				}
				break;
			}

			if (tmp_val2[0]) modstr = tmp_val2;

			break;
		}

		/* Armour */
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		{
			show_armour = TRUE;
			break;
		}


		/* Lites (including a few "Specials") */
		case TV_LITE:
		{
			break;
		}

		/* Amulets (including a few "Specials") */
		case TV_AMULET:
		{
			/* Known artifacts */
			if (artifact_p(o_ptr) && aware) break;

			if ((k_ptr->gen_flags & TRG_INSTA_ART) && aware) break;

			/* Color the object */
			modstr = amulet_adj[indexx];

			if (aware) append_name = TRUE;

			if (((plain_descriptions) && (aware)) || o_ptr->ident & IDENT_STOREB)
#ifdef JP
				basenm = aware ? "%のアミュレット" : "アミュレット";
			else
				basenm = aware ? "#%のアミュレット" : "#アミュレット";
#else
				basenm = "& Amulet~";
			else
				basenm = aware ? "& # Amulet~" : "& # Amulet~";
#endif

			break;
		}

		/* Rings (including a few "Specials") */
		case TV_RING:
		{
			/* Known artifacts */
			if (artifact_p(o_ptr) && aware) break;

			if ((k_ptr->gen_flags & TRG_INSTA_ART) && aware) break;

			/* Color the object */
			modstr = ring_adj[indexx];

			if (aware) append_name = TRUE;

			if (((plain_descriptions) && (aware)) || o_ptr->ident & IDENT_STOREB)
#ifdef JP
				basenm = aware ? "%の指輪" : "指輪";
			else
				basenm = aware ? "#%の指輪" : "#指輪";
#else
				basenm = "& Ring~";
			else
				basenm = aware ? "& # Ring~" : "& # Ring~";
#endif

			if (!k_ptr->to_h && !k_ptr->to_d && (o_ptr->to_h || o_ptr->to_d)) show_weapon = TRUE;

			break;
		}

		case TV_CARD:
		{
			break;
		}

		case TV_STAFF:
		{
			/* Color the object */
			modstr = staff_adj[indexx];

			if (aware) append_name = TRUE;

			if (((plain_descriptions) && (aware)) || o_ptr->ident & IDENT_STOREB)
#ifdef JP
				basenm = aware ? "%の杖" : "杖";
			else
				basenm = aware ? "#%の杖" : "#杖";
#else
				basenm = "& Staff~";
			else
				basenm = aware ? "& # Staff~" : "& # Staff~";
#endif

			break;
		}

		case TV_WAND:
		{
			/* Color the object */
			modstr = wand_adj[indexx];

			if (aware) append_name = TRUE;

			if (((plain_descriptions) && (aware)) || o_ptr->ident & IDENT_STOREB)
#ifdef JP
				basenm = aware ? "%の魔法棒" : "魔法棒";
			else
				basenm = aware ? "#%の魔法棒" : "#魔法棒";
#else
				basenm = "& Wand~";
			else
				basenm = aware ? "& # Wand~" : "& # Wand~";
#endif

			break;
		}

		case TV_ROD:
		{
			/* Color the object */
			modstr = rod_adj[indexx];

			if (aware) append_name = TRUE;

			if (((plain_descriptions) && (aware)) || o_ptr->ident & IDENT_STOREB)
#ifdef JP
				basenm = aware ? "%のロッド" : "ロッド";
			else
				basenm = aware ? "#%のロッド" : "#ロッド";
#else
				basenm = "& Rod~";
			else
				basenm = aware ? "& # Rod~" : "& # Rod~";
#endif

			break;
		}

		case TV_SCROLL:
		{
			/* Color the object */
			modstr = scroll_adj[indexx];

			if (aware) append_name = TRUE;

			if (((plain_descriptions) && (aware)) || o_ptr->ident & IDENT_STOREB)
#ifdef JP
				basenm = aware ? "%の巻物" : "巻物";
			else
				basenm = aware ? "「#」と書かれた%の巻物" : "「#」と書かれた巻物";
#else
				basenm = "& Scroll~";
			else
				basenm = aware ? "& Scroll~ titled \"#\"" : "& Scroll~ titled \"#\"";
#endif

			break;
		}

		case TV_POTION:
		{
			/* Color the object */
			modstr = potion_adj[indexx];

			if (aware) append_name = TRUE;

			if (((plain_descriptions) && (aware)) || o_ptr->ident & IDENT_STOREB)
#ifdef JP
				basenm = aware ? "%の薬" : "薬";
			else
				basenm = aware ? "#%の薬" : "#薬";
#else
				basenm = "& Potion~";
			else
				basenm = aware ? "& # Potion~" : "& # Potion~";
#endif

			break;
		}

		/* Magic Books */
		case TV_MAGERY_BOOK:
		{
			modstr = basenm;
#ifdef JP
			basenm = "魔道の魔法書#";
#else
			if (mp_ptr->spell_book == TV_HOLY_BOOK)
				basenm = "& Book~ of Magery #";
			else
				basenm = "& Magery Spellbook~ #";
#endif

			break;
		}

		case TV_FIRE_BOOK:
		{
			modstr = basenm;
#ifdef JP
			basenm = "火炎の魔法書#";
#else
			if (mp_ptr->spell_book == TV_HOLY_BOOK)
				basenm = "& Book~ of Fire Magic #";
			else
				basenm = "& Fire Spellbook~ #";
#endif

			break;
		}

		case TV_AQUA_BOOK:
		{
			modstr = basenm;
#ifdef JP
			basenm = "水の魔法書#";
#else
			if (mp_ptr->spell_book == TV_HOLY_BOOK)
				basenm = "& Book~ of Aqua Magic #";
			else
				basenm = "& Aqua Spellbook~ #";
#endif

			break;
		}

		case TV_EARTH_BOOK:
		{
			modstr = basenm;
#ifdef JP
			basenm = "大地の魔法書#";
#else
			if (mp_ptr->spell_book == TV_HOLY_BOOK)
				basenm = "& Book~ of Earth Magic #";
			else
				basenm = "& Earth Spellbook~ #";
#endif

			break;
		}

		case TV_WIND_BOOK:
		{
			modstr = basenm;
#ifdef JP
			basenm = "風の魔法書#";
#else
			if (mp_ptr->spell_book == TV_HOLY_BOOK)
				basenm = "& Book~ of Wind Magic #";
			else
				basenm = "& Wind Spellbook~ #";
#endif

			break;
		}

		case TV_HOLY_BOOK:
		{
			modstr = basenm;
#ifdef JP
			basenm = "神聖の魔法書#";
#else
			if (mp_ptr->spell_book == TV_HOLY_BOOK)
				basenm = "& Book~ of Holy Magic #";
			else
				basenm = "& Holy Spellbook~ #";
#endif

			break;
		}

		case TV_DEATH_BOOK:
		{
			modstr = basenm;
#ifdef JP
			basenm = "暗黒の魔法書#";
#else
			if (mp_ptr->spell_book == TV_HOLY_BOOK)
				basenm = "& Book~ of Death Magic #";
			else
				basenm = "& Death Spellbook~ #";
#endif

			break;
		}

		case TV_SYMBIOTIC_BOOK:
		{
			modstr = basenm;
#ifdef JP
			basenm = "共生の魔法書#";
#else
			if (mp_ptr->spell_book == TV_HOLY_BOOK)
				basenm = "& Book~ of Symbiotic Magic #";
			else
				basenm = "& Symbiotic Spellbook~ #";
#endif

			break;
		}

		case TV_WITCH_BOOK:
		{
			modstr = basenm;
#ifdef JP
			basenm = "ウィッチの魔法書#";
#else
			if (mp_ptr->spell_book == TV_HOLY_BOOK)
				basenm = "& Book~ of Witch Magic #";
			else
				basenm = "& Witch Spellbook~ #";
#endif

			break;
		}

		case TV_DRAKONITE_BOOK:
		{
			modstr = basenm;
#ifdef JP
			basenm = "竜言語の魔法書#";
#else
			if (mp_ptr->spell_book == TV_HOLY_BOOK)
				basenm = "& Book~ of Drakonite Magic #";
			else
				basenm = "& Drakonite Spellbook~ #";
#endif

			break;
		}

		case TV_CRUSADE_BOOK:
		{
			modstr = basenm;
#ifdef JP
			basenm = "破邪の魔法書#";
#else
			if (mp_ptr->spell_book == TV_HOLY_BOOK)
				basenm = "& Book~ of Crusade Magic #";
			else
				basenm = "& Crusade Spellbook~ #";
#endif

			break;
		}



		/* Hack -- Gold/Gems */
		case TV_GOLD:
		{
			strcpy(buf, basenm);
			return;
		}

		/* Used in the "inventory" routine */
		default:
		{
#ifdef JP
			strcpy(buf, "(なし)");
#else
			strcpy(buf, "(nothing)");
#endif

			return;
		}
	}

	/* Use full name from k_info or a_info */
	if (aware && have_flag(flgs, TR_FULL_NAME))
	{
		if (known && o_ptr->name1) basenm = a_name + a_info[o_ptr->name1].name;
		else if (append_name)
		{
			if (plain_descriptions || (o_ptr->ident & IDENT_STOREB))
#ifdef JP
				basenm = get_object_name(o_ptr);
			else
			{
				sprintf(tmp_val2, "#%s", get_object_name(o_ptr));
				basenm = tmp_val2;
			}
#else
				sprintf(tmp_val2, "& %s", get_object_name(o_ptr));
			else
				sprintf(tmp_val2, "& # %s", get_object_name(o_ptr));
			basenm = tmp_val2;
#endif
			append_name = FALSE;
		}
		else basenm = get_object_name(o_ptr);
	}

	/* Start dumping the result */
	t = tmp_val;

#ifdef JP
	if (basenm[0] == '&')
		s = basenm + 2;
	else
		s = basenm;

	/* No prefix */
	if (!pref)
	{
		/* Nothing */
	}
	else if (o_ptr->number > 1)
	{
		if ( change_numeral == FALSE ){
			t = object_desc_num(t, o_ptr->number);
			if (o_ptr->number > 9)
				t = object_desc_str(t, "個の ");
			else
				t = object_desc_str(t, "つの ");
		}
		else
		{
			t = object_desc_kosuu(t,o_ptr);
			t = object_desc_str(t, "の ");
		}
	}

	/* 英語の場合アーティファクトは The が付くので分かるが
	 * 日本語では分からないのでマークをつける 
	 */
	if (known)
	{
		if (artifact_p(o_ptr)) t = object_desc_str(t, "★");
		else if (o_ptr->art_name)
		{
			t = object_desc_str(t, object_is_snapdragon_runeweapon(o_ptr) ? "★" : "☆");
		}
	}

#else

	/* The object "expects" a "number" */
	if (basenm[0] == '&')
	{
		/* Skip the ampersand (and space) */
		s = basenm + 2;

		/* No prefix */
		if (!pref)
		{
			/* Nothing */
		}

		/* Hack -- None left */
		else if (o_ptr->number <= 0)
		{
			t = object_desc_str(t, "no more ");
		}

		/* Extract the number */
		else if (o_ptr->number > 1)
		{
			t = object_desc_num(t, o_ptr->number);
			t = object_desc_chr(t, ' ');
		}

		/* Hack -- The only one of its kind */
		else if (known && (artifact_p(o_ptr) || o_ptr->art_name))
		{
			t = object_desc_str(t, "The ");
		}

		/* Unique corpses are unique */
		else if (o_ptr->tval == TV_CORPSE)
		{
			monster_race *r_ptr = &r_info[o_ptr->pval];

			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				t = object_desc_str(t, "The ");
			}
		}

		/* A single one, with a vowel in the modifier */
		else if ((*s == '#') && (is_a_vowel(modstr[0])))
		{
			t = object_desc_str(t, "an ");
		}

		/* A single one, with a vowel */
		else if (is_a_vowel(*s))
		{
			t = object_desc_str(t, "an ");
		}

		/* A single one, without a vowel */
		else
		{
			t = object_desc_str(t, "a ");
		}
	}

	/* Hack -- objects that "never" take an article */
	else
	{
		/* No ampersand */
		s = basenm;

		/* No pref */
		if (!pref)
		{
			/* Nothing */
		}

		/* Hack -- all gone */
		else if (o_ptr->number <= 0)
		{
			t = object_desc_str(t, "no more ");
		}

		/* Prefix a number if required */
		else if (o_ptr->number > 1)
		{
			t = object_desc_num(t, o_ptr->number);
			t = object_desc_chr(t, ' ');
		}

		/* Hack -- The only one of its kind */
		else if (known && (artifact_p(o_ptr) || o_ptr->art_name))
		{
			t = object_desc_str(t, "The ");
		}

		/* Hack -- single items get no prefix */
		else
		{
			/* Nothing */
		}
	}
#endif

	/* Paranoia -- skip illegal tildes */
	/* while (*s == '~') s++; */

#ifdef JP
	/* 伝説のアイテム、名のあるアイテムの名前を付加する */
	if (known)
	{
		/* ランダム・アーティファクト */
		if (o_ptr->art_name)
		{
			char temp[256];
			strcpy(temp, quark_str(o_ptr->art_name));
			/* '『' から始まらない伝説のアイテムの名前は最初に付加する */
			/* 英語版のセーブファイルから来た 'of XXX' は,「XXXの」と表示する */
			if (strncmp(temp, "of ", 3) == 0)
			{
				t = object_desc_str(t, &temp[3]);
				t = object_desc_str(t, "の");
			}
			else if ((strncmp(temp, "『", 2) != 0) && (temp[0] != '\''))
				t = object_desc_str(t, temp);
		}
		/* 伝説のアイテム */
		else if (o_ptr->name1 && !have_flag(flgs, TR_FULL_NAME))
		{
			artifact_type *a_ptr = &a_info[o_ptr->name1];
			/* '『' から始まらない伝説のアイテムの名前は最初に付加する */
			if (strncmp((a_name + a_ptr->name), "『", 2) != 0)
			{
				t = object_desc_str(t, (a_name + a_ptr->name));
			}
		}
		/* 名のあるアイテム */
		else if (o_ptr->name2)
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];
			t = object_desc_str(t, (e_name + e_ptr->name));
		}
	}
#endif
	/* Copy the string */
	for (; *s; s++)
	{
		/* Pluralizer */
#ifdef JP
		if (*s == '#')
#else
		if (*s == '~')
		{
			/* Add a plural if needed */
			if (pref && o_ptr->number != 1)
			{
				char k = t[-1];

				/* XXX XXX XXX Mega-Hack */

				/* Hack -- "Cutlass-es" and "Torch-es" */
				if ((k == 's') || (k == 'h')) *t++ = 'e';

				/* Add an 's' */
				*t++ = 's';
			}
		}

		/* Modifier */
		else if (*s == '#')
#endif

		{
			/* Insert the modifier */
			for (u = modstr; *u; u++) *t++ = *u;
		}

#ifdef JP
		else if (*s == '%')
		/* saigo の代わり。効能を付加する by ita*/
		{
			modstr = get_object_name(o_ptr);
			for (u = modstr; *u; u++) *t++ = *u;
		}
#endif
		/* Normal */
		else
		{
			/* Copy */
			*t++ = *s;
		}
	}

	/* Terminate */
	*t = '\0';


	/* Append the "kind name" to the "base name" */
#ifndef JP
	if (append_name)
	{
		t = object_desc_str(t, " of ");
		t = object_desc_str(t, get_object_name(o_ptr));
	}
#endif


#ifdef JP
	/* '『'から始まる伝説のアイテムの名前は最後に付加する */
	if (known)
	{
		/* ランダムアーティファクトの名前はセーブファイルに記録
		   されるので、英語版の名前もそれらしく変換する */
		if (o_ptr->art_name)
		{
			char temp[256];
			int itemp;
			strcpy(temp, quark_str(o_ptr->art_name));
			/* MEGA HACK by ita*/
			if (strncmp(temp, "『", 2) == 0) t = object_desc_str(t, temp);
			else if (temp[0] == '\'')
			{
				itemp = strlen(temp);
				temp[itemp - 1] = 0;
				t = object_desc_str(t, "『");
				t = object_desc_str(t, &temp[1]);
				t = object_desc_str(t, "』");
			}
		}
		else if (o_ptr->name1)
		{
			artifact_type *a_ptr = &a_info[o_ptr->name1];
			if (strncmp((a_name + a_ptr->name), "『", 2) == 0)
			{
				t = object_desc_str(t, (a_name + a_ptr->name));
			}
		}
		else if (o_ptr->inscription)
		{
			cptr str = quark_str(o_ptr->inscription);

			while (*str)
			{
				if (iskanji(*str))
				{
					str += 2;
					continue;
				}
				if (*str == '#') break;
				str++;
			}
			if (*str)
			{
				/* Find the '#' */
				cptr str = strchr(quark_str(o_ptr->inscription), '#');

				/* Add the false name */
				t=object_desc_str(t,"『");
				t = object_desc_str(t, &str[1]);
				t=object_desc_str(t,"』");
			}
		}
	}
#else
	/* Hack -- Append "Artifact" or "Special" names */
	if (known && !have_flag(flgs, TR_FULL_NAME))
	{
		/* Is it a new random artifact ? */
		if (o_ptr->art_name)
		{
			t = object_desc_chr(t, ' ');

			t = object_desc_str(t, quark_str(o_ptr->art_name));
		}

		/* Grab any artifact name */
		else if (o_ptr->name1)
		{
			artifact_type *a_ptr = &a_info[o_ptr->name1];

			t = object_desc_chr(t, ' ');
			t = object_desc_str(t, (a_name + a_ptr->name));
		}

		/* Grab any ego-item name */
		else
		{
			if (o_ptr->name2)
			{
				ego_item_type *e_ptr = &e_info[o_ptr->name2];

				t = object_desc_chr(t, ' ');
				t = object_desc_str(t, (e_name + e_ptr->name));
			}

			if (o_ptr->inscription && strchr(quark_str(o_ptr->inscription), '#'))
			{
				/* Find the '#' */
				cptr str = strchr(quark_str(o_ptr->inscription), '#');

				/* Add the false name */
				t = object_desc_chr(t, ' ');
				t = object_desc_str(t, &str[1]);
			}
		}
	}
#endif



	/* No more details wanted */
	if (mode < 1) goto copyback;

	/* Hack -- Chests must be described in detail */
	if (o_ptr->tval == TV_CHEST)
	{
		/* Not searched yet */
		if (!known)
		{
			/* Nothing */
		}

		/* May be "empty" */
		else if (!o_ptr->pval)
		{
#ifdef JP
t = object_desc_str(t, "(空)");
#else
			t = object_desc_str(t, " (empty)");
#endif

		}

		/* May be "disarmed" */
		else if (o_ptr->pval < 0)
		{
			if (chest_traps[0 - o_ptr->pval])
			{
#ifdef JP
t = object_desc_str(t, "(解除済)");
#else
				t = object_desc_str(t, " (disarmed)");
#endif

			}
			else
			{
#ifdef JP
t = object_desc_str(t, "(非施錠)");
#else
				t = object_desc_str(t, " (unlocked)");
#endif

			}
		}

		/* Describe the traps, if any */
		else
		{
			/* Describe the traps */
			switch (chest_traps[o_ptr->pval])
			{
				case 0:
				{
#ifdef JP
t = object_desc_str(t, "(施錠)");
#else
					t = object_desc_str(t, " (Locked)");
#endif

					break;
				}
				case CHEST_LOSE_STR:
				{
#ifdef JP
t = object_desc_str(t, "(毒針)");
#else
					t = object_desc_str(t, " (Poison Needle)");
#endif

					break;
				}
				case CHEST_LOSE_CON:
				{
#ifdef JP
t = object_desc_str(t, "(毒針)");
#else
					t = object_desc_str(t, " (Poison Needle)");
#endif

					break;
				}
				case CHEST_POISON:
				{
#ifdef JP
t = object_desc_str(t, "(ガス・トラップ)");
#else
					t = object_desc_str(t, " (Gas Trap)");
#endif

					break;
				}
				case CHEST_PARALYZE:
				{
#ifdef JP
t = object_desc_str(t, "(ガス・トラップ)");
#else
					t = object_desc_str(t, " (Gas Trap)");
#endif

					break;
				}
				case CHEST_EXPLODE:
				{
#ifdef JP
t = object_desc_str(t, "(爆発装置)");
#else
					t = object_desc_str(t, " (Explosion Device)");
#endif

					break;
				}
				case CHEST_SUMMON:
				case CHEST_BIRD_STORM:
				case CHEST_E_SUMMON:
				case CHEST_H_SUMMON:
				{
#ifdef JP
t = object_desc_str(t, "(召喚のルーン)");
#else
					t = object_desc_str(t, " (Summoning Runes)");
#endif

					break;
				}
				case CHEST_RUNES_OF_EVIL:
				{
#ifdef JP
t = object_desc_str(t, "(邪悪なルーン)");
#else
					t = object_desc_str(t, " (Gleaming Black Runes)");
#endif

					break;
				}
				case CHEST_ALARM:
				{
#ifdef JP
t = object_desc_str(t, "(警報装置)");
#else
					t = object_desc_str(t, " (Alarm)");
#endif

					break;
				}
				default:
				{
#ifdef JP
t = object_desc_str(t, "(マルチ・トラップ)");
#else
					t = object_desc_str(t, " (Multiple Traps)");
#endif

					break;
				}
			}
		}
	}


	/* Display the item like a weapon */
	if (have_flag(flgs, TR_SHOW_MODS)) show_weapon = TRUE;

	/* Display the item like a weapon */
	if (o_ptr->to_h && o_ptr->to_d) show_weapon = TRUE;

	/* Display the item like armour */
	if (o_ptr->ac) show_armour = TRUE;


	/* Dump base weapon info */
	switch (o_ptr->tval)
	{
		/* Missiles and Weapons */
		case TV_BULLET:
		case TV_ROUND:
		case TV_SHELL:
		case TV_BOLT:
		case TV_ARROW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:

		/* Append a "damage" string */
		t = object_desc_chr(t, ' ');
		t = object_desc_chr(t, p1);
		t = object_desc_num(t, known ? o_ptr->dd : k_ptr->dd);
		t = object_desc_chr(t, 'd');
		t = object_desc_num(t, known ? o_ptr->ds : k_ptr->ds);
		t = object_desc_chr(t, p2);

		/* All done */
		break;


		/* Bows get a special "damage string" */
		case TV_BOW:

		if (o_ptr->sval != SV_ROCKET_LAUNCHER)
		{
			/* Mega-Hack -- Extract the "base power" */
			power = bow_tmul(o_ptr);

			/* Apply the "Extra Might" flag */
			if (have_flag(flgs, TR_XTRA_MIGHT) && known) power++;

			/* Append a special "damage" string */
			t = object_desc_chr(t, ' ');
			t = object_desc_chr(t, p1);
			t = object_desc_chr(t, 'x');
			t = object_desc_num(t, power);
			t = object_desc_chr(t, p2);
		}

		/* All done */
		break;
	}


	/* Add the weapon bonuses */
	if (known)
	{
		/* Show the tohit/todam on request */
		if (show_weapon)
		{
			t = object_desc_chr(t, ' ');
			t = object_desc_chr(t, p1);
			t = object_desc_int(t, o_ptr->to_h);
			t = object_desc_chr(t, ',');
			t = object_desc_int(t, o_ptr->to_d);
			t = object_desc_chr(t, p2);
		}

		/* Show the tohit if needed */
		else if (o_ptr->to_h)
		{
			t = object_desc_chr(t, ' ');
			t = object_desc_chr(t, p1);
			t = object_desc_int(t, o_ptr->to_h);
			t = object_desc_chr(t, p2);
		}

		/* Show the todam if needed */
		else if (o_ptr->to_d)
		{
			t = object_desc_chr(t, ' ');
			t = object_desc_chr(t, p1);
			t = object_desc_int(t, o_ptr->to_d);
			t = object_desc_chr(t, p2);
		}
	}

	bow_ptr = &inventory[INVEN_BOW];

	/* if have a firing weapon + ammo matches bow*/
	if (bow_ptr->k_idx &&
	    (((bow_ptr->sval == SV_PISTOL) && (o_ptr->tval == TV_BULLET)) ||
	     (((bow_ptr->sval == SV_ASSAULT_RIFLE) || (bow_ptr->sval == SV_SNIPER_RIFLE) ||
	       (bow_ptr->sval == SV_RUNEGUN)) && (o_ptr->tval == TV_ROUND)) ||
	      ((bow_ptr->sval == SV_SHOTGUN) && (o_ptr->tval == TV_SHELL)) ||
	      ((bow_ptr->sval == SV_ROCKET_LAUNCHER) && (o_ptr->tval == TV_ROCKET)) ||
	     (((bow_ptr->sval == SV_SHORT_BOW) || (bow_ptr->sval == SV_LONG_BOW) ||
	       (bow_ptr->sval == SV_RUNEBOW)) && (o_ptr->tval == TV_ARROW)) ||
	     (((bow_ptr->sval == SV_BOWGUN) ||
	       (bow_ptr->sval == SV_CROSSBOW)) && (o_ptr->tval == TV_BOLT))))
	{
		int avgdam = 10;
		int tmul;
		s32b energy_fire;
		int skill_to_d = skill_lev_var[p_ptr->weapon_exp[get_weapon_type(&k_info[bow_ptr->k_idx])]] - 1;

		if (bow_ptr->sval != SV_ROCKET_LAUNCHER)
		{
			avgdam = (known ? o_ptr->dd : k_ptr->dd) * (known ? o_ptr->ds : k_ptr->ds + 1) * 10 / 2 + skill_to_d * 10;

			/* See if the bow is "known" - then set damage bonus */
			if (object_known_p(bow_ptr))
			{
				avgdam += (bow_ptr->to_d * 10);
			}

			/* effect of ammo */
			if (known) avgdam += (o_ptr->to_d * 10);
		}
		else
		{
			int rocket_to_d = skill_to_d;
			if (object_known_p(bow_ptr)) rocket_to_d += bow_ptr->to_d;
			if (known) rocket_to_d += o_ptr->to_d;
			avgdam = rocket_damage(o_ptr, rocket_to_d);
		}

		energy_fire = bow_energy(bow_ptr);

		/* launcher multiplier */
		if (bow_ptr->sval != SV_ROCKET_LAUNCHER)
		{
			tmul = bow_tmul(bow_ptr);

			/* Get extra "power" from "extra might" */
			if (p_ptr->dis_xtra_might) tmul++;

			tmul = tmul * (100 + ((get_weapon_type(&k_info[bow_ptr->k_idx]) != WT_GUN) ?
				((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128) : 0));

			avgdam *= tmul;
			avgdam /= (100 * 10);
		}
		if (avgdam < 0) avgdam = 0;

		/* display (shot damage/ avg damage) */
		t = object_desc_chr(t, ' ');
		t = object_desc_chr(t, p1);
		t = object_desc_num(t, avgdam);
		t = object_desc_chr(t, '/');

		if (p_ptr->num_fire == 0)
		{
			t = object_desc_chr(t, '0');
		}
		else
		{
			/* calc effects of energy */
			avgdam *= (p_ptr->num_fire * 100);
			if ((bow_ptr->sval == SV_ASSAULT_RIFLE) || (bow_ptr->sval == SV_RUNEGUN)) avgdam *= 3;

			avgdam /= energy_fire;

			t = object_desc_num(t, avgdam);
		}

		t = object_desc_chr(t, p2);
	}

	/* Add the armor bonuses */
	if (known)
	{
		/* Show the armor class info */
		if (show_armour)
		{
			t = object_desc_chr(t, ' ');
			t = object_desc_chr(t, b1);
			t = object_desc_num(t, o_ptr->ac);
			t = object_desc_chr(t, ',');
			t = object_desc_int(t, o_ptr->to_a);
			t = object_desc_chr(t, b2);
		}

		/* No base armor, but does increase armor */
		else if (o_ptr->to_a)
		{
			t = object_desc_chr(t, ' ');
			t = object_desc_chr(t, b1);
			t = object_desc_int(t, o_ptr->to_a);
			t = object_desc_chr(t, b2);
		}
	}

	/* Hack -- always show base armor */
	else if (show_armour)
	{
		t = object_desc_chr(t, ' ');
		t = object_desc_chr(t, b1);
		t = object_desc_num(t, known ? o_ptr->ac : k_ptr->ac);
		t = object_desc_chr(t, b2);
	}


	/* No more details wanted */
	if (mode < 2) goto copyback;


	/*
	 * Hack -- Wands and Staffs have charges.  Make certain how many charges
	 * a stack of staffs really has is clear. -LM-
	 */
	if (known &&
	    ((o_ptr->tval == TV_STAFF) ||
	     (o_ptr->tval == TV_WAND)))
	{
		/* Dump " (N charges)" */
		t = object_desc_chr(t, ' ');
		t = object_desc_chr(t, p1);

		/* Clear explaination for staffs. */
		if ((o_ptr->tval == TV_STAFF) && (o_ptr->number > 1))
		{
			t = object_desc_num(t, o_ptr->number);
			t = object_desc_str(t, "x ");
		}
		t = object_desc_num(t, o_ptr->pval);
#ifdef JP
		t = object_desc_str(t, "回分");
#else
		t = object_desc_str(t, " charge");

		if (o_ptr->pval != 1)
		{
			t = object_desc_chr(t, 's');
		}
#endif


		t = object_desc_chr(t, p2);
	}
	/* Hack -- Rods have a "charging" indicator.  Now that stacks of rods may
	 * be in any state of charge or discharge, this now includes a number. -LM-
	 */
	else if (known && (o_ptr->tval == TV_ROD))
	{
		/* Hack -- Dump " (# charging)" if relevant */
		if (o_ptr->timeout)
		{
			/* Stacks of rods display an exact count of charging rods. */
			if (o_ptr->number > 1)
			{
				/* Paranoia. */
				if (k_ptr->pval == 0) k_ptr->pval = 1;

				/* Find out how many rods are charging, by dividing
				 * current timeout by each rod's maximum timeout.
				 * Ensure that any remainder is rounded up.  Display
				 * very discharged stacks as merely fully discharged.
				 */
				power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;
				if (power > o_ptr->number) power = o_ptr->number;

				/* Display prettily. */
				t = object_desc_str(t, " (");
				t = object_desc_num(t, power);
#ifdef JP
				t = object_desc_str(t, "本 充填中)");
#else
				t = object_desc_str(t, " charging)");
#endif

			}

			/* "one Rod of Perception (1 charging)" would look tacky. */
			else
			{
#ifdef JP
t = object_desc_str(t, "(充填中)");
#else
				t = object_desc_str(t, " (charging)");
#endif

			}
		}
	}

	/* Dump "bonus" flags for wearable items */
	if (known && (have_bonus_flags(flgs))) t = object_desc_bonus(t, o_ptr);

	/* Hack -- Process Lanterns/Torches */
	if (known && (o_ptr->tval == TV_LITE) && (!(artifact_p(o_ptr) || (o_ptr->sval == SV_LITE_FEANOR) || (o_ptr->sval == SV_LITE_MAGICAL_LAMP) || (o_ptr->sval == SV_LITE_EMPTY))))
	{
		/* Hack -- Turns of light for normal lites */
#ifdef JP
t = object_desc_str(t, "(");
#else
		t = object_desc_str(t, " (with ");
#endif

		if (o_ptr->name2 == EGO_LITE_LONG) t = object_desc_num(t, o_ptr->xtra4*2);
		else t = object_desc_num(t, o_ptr->xtra4);
#ifdef JP
t = object_desc_str(t, "ターンの寿命)");
#else
		t = object_desc_str(t, " turns of light)");
#endif

	}


	/* Indicate charging objects, but not rods. */
	if (known && o_ptr->timeout && o_ptr->tval != TV_ROD)
	{
		/* Hack -- Dump " (charging)" if relevant */
#ifdef JP
t = object_desc_str(t, "(充填中)");
#else
		t = object_desc_str(t, " (charging)");
#endif

	}


	/* No more details wanted */
	if (mode < 3) goto copyback;


	/* No inscription yet */
	tmp_val2[0] = '\0';

	/* Use the game-generated "feeling" otherwise, if available */
	if (o_ptr->feeling)
	{
		strcpy(tmp_val2, game_inscriptions[o_ptr->feeling]);
	}

	/* Note "cursed" if the item is known to be cursed */
	else if (cursed_p(o_ptr) && (known || (o_ptr->ident & (IDENT_SENSE))))
	{
#ifdef JP
strcpy(tmp_val2, "呪われている");
#else
		strcpy(tmp_val2, "cursed");
#endif

	}

	/* Note "unidentified" if the item is unidentified */
	else if ((o_ptr->tval == TV_RING || o_ptr->tval == TV_AMULET
		  || o_ptr->tval == TV_LITE || o_ptr->tval == TV_FIGURINE)
		 && object_aware_p(o_ptr) && !object_known_p(o_ptr)
		 && !((o_ptr->ident)&IDENT_SENSE))
	{
#ifdef JP
strcpy(tmp_val2, "未鑑定");
#else
		strcpy(tmp_val2, "unidentified");
#endif
	}

	/* Mega-Hack -- note empty wands/staffs */
	else if (!known && (o_ptr->ident & (IDENT_EMPTY)))
	{
#ifdef JP
strcpy(tmp_val2, "空");
#else
		strcpy(tmp_val2, "empty");
#endif

	}

	/* Note "tried" if the object has been tested unsuccessfully */
	else if (!aware && object_tried_p(o_ptr))
	{
#ifdef JP
strcpy(tmp_val2, "未判明");
#else
		strcpy(tmp_val2, "tried");
#endif

	}

	/* Use the standard inscription if available */
	if (o_ptr->inscription)
	{
		char buff[1024];

		if (tmp_val2[0]) strcat(tmp_val2, ", ");

		/* Get inscription and convert {%} */
		get_inscription(buff, o_ptr);

		/* strcat with correct treating of kanji */
		my_strcat(tmp_val2, buff, sizeof(tmp_val2));
	}

	/* Note the discount, if any */
	else if (o_ptr->discount && !(tmp_val2[0]))
	{
		(void)object_desc_num(tmp_val2, o_ptr->discount);
#ifdef JP
		strcat(tmp_val2, "%引き");
#else
		strcat(tmp_val2, "% off");
#endif

	}

	/* Append the inscription, if any */
	if (tmp_val2[0])
	{
		/* Append the inscription */
		t = object_desc_chr(t, ' ');
		t = object_desc_chr(t, c1);
		t = object_desc_str(t, tmp_val2);
		t = object_desc_chr(t, c2);
	}

copyback:
	t = tmp_val;
	for (i = 0; i < MAX_NLEN-2; i++)
	{
#ifdef JP
		if (iskanji(*(t+i)))
		{
			*(buf+i) = *(t+i);
			i++;
		}
#endif
		*(buf+i) = *(t+i);
	}
	if (i == MAX_NLEN-2)
	{
#ifdef JP
		if (iskanji(*(t+i)))
			*(buf+i) = '\0';
		else
#endif
			*(buf+i) = *(t+i);
	}
	*(buf+MAX_NLEN-1) = '\0';
}

