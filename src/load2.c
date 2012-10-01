/* File: load2.c */

/* Up-to-date savefile handling.
 *
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * This file loads savefiles from Angband 2.7.X and 2.8.X, and from all
 * (non-beta) versions of Oangband.
 *
 * Ancient savefiles (pre-2.7.0) are loaded by another file.
 *
 * Note that Angband 2.7.0 through 2.7.3 are now officially obsolete,
 * and savefiles from those versions may not be successfully converted.
 *
 * We attempt to prevent corrupt savefiles from inducing memory errors.
 *
 * Note that this file should not use the random number generator, the
 * object flavors, the visual attr/char mappings, or anything else which
 * is initialized *after* or *during* the "load character" function.
 *
 * This file assumes that the monster/object records are initialized
 * to zero, and the race/kind tables have been loaded correctly.  The
 * order of object stacks is currently not saved in the savefiles, but
 * the "next" pointers are saved, so all necessary knowledge is present.
 *
 * We should implement simple "savefile extenders" using some form of
 * "sized" chunks of bytes, with a {size,type,data} format, so everyone
 * can know the size, interested people can know the type, and the actual
 * data is available to the parsing routines that acknowledge the type.
 */





/*
 * Local "savefile" pointer
 */
static FILE	*fff;

/*
 * Hack -- old "encryption" byte
 */
static byte	xor_byte;

/*
 * Hack -- simple "checksum" on the actual values
 */
static u32b	v_check = 0L;

/*
 * Hack -- simple "checksum" on the encoded bytes
 */
static u32b	x_check = 0L;



/*
 * This function determines if the version of the savefile
 * currently being read is older than Angband version "x.y.z".
 */
static bool older_than(byte x, byte y, byte z)
{
	/* Much older, or much more recent */
	if (sf_major < x) return (TRUE);
	if (sf_major > x) return (FALSE);

	/* Distinctly older, or distinctly more recent */
	if (sf_minor < y) return (TRUE);
	if (sf_minor > y) return (FALSE);

	/* Barely older, or barely more recent */
	if (sf_patch < z) return (TRUE);
	if (sf_patch > z) return (FALSE);

	/* Identical versions */
	return (FALSE);
}

/*
 * This function determines if the version of the savefile
 * currently being read is older than Oangband version "x.y.z".
 * Note that savefiles from both Oangband version 0.1.0 and
 * 0.2.0 are treated as 0.2.0.
 */
static bool o_older_than(byte x, byte y, byte z)
{
	/* Much older, or much more recent */
	if (o_sf_major < x) return (TRUE);
	if (o_sf_major > x) return (FALSE);

	/* Distinctly older, or distinctly more recent */
	if (o_sf_minor < y) return (TRUE);
	if (o_sf_minor > y) return (FALSE);

	/* Barely older, or barely more recent */
	if (o_sf_patch < z) return (TRUE);
	if (o_sf_patch > z) return (FALSE);

	/* Identical versions */
	return (FALSE);
}



/*
 * Hack -- Show information on the screen, one line at a time.
 *
 * Avoid the top two lines, to avoid interference with "msg_print()".
 */
static void note(cptr msg)
{
	static int y = 2;

	/* Draw the message */
	prt(msg, y, 0);

	/* Advance one line (wrap if needed) */
	if (++y >= 24) y = 2;

	/* Flush it */
	Term_fresh();
}


/*
 * Hack -- determine if an item is "wearable" (or a missile)
 */
static bool wearable_p(object_type *o_ptr)
{
	/* Valid "tval" codes */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_LITE:
		case TV_AMULET:
		case TV_RING:
		{
			return (TRUE);
		}
	}

	/* Nope */
	return (FALSE);
}


/*
 * The following functions are used to load the basic building blocks
 * of savefiles.  They also maintain the "checksum" info for 2.7.0+
 */

static byte sf_get(void)
{
	byte c, v;

	/* Get a character, decode the value */
	c = getc(fff) & 0xFF;
	v = c ^ xor_byte;
	xor_byte = c;

	/* Maintain the checksum info */
	v_check += v;
	x_check += xor_byte;

	/* Return the value */
	return (v);
}

static void rd_byte(byte *ip)
{
	*ip = sf_get();
}

static void rd_u16b(u16b *ip)
{
	(*ip) = sf_get();
	(*ip) |= ((u16b)(sf_get()) << 8);
}

static void rd_s16b(s16b *ip)
{
	rd_u16b((u16b*)ip);
}

static void rd_u32b(u32b *ip)
{
	(*ip) = sf_get();
	(*ip) |= ((u32b)(sf_get()) << 8);
	(*ip) |= ((u32b)(sf_get()) << 16);
	(*ip) |= ((u32b)(sf_get()) << 24);
}

static void rd_s32b(s32b *ip)
{
	rd_u32b((u32b*)ip);
}


/*
 * Hack -- read a string
 */
static void rd_string(char *str, int max)
{
	int i;

	/* Read the string */
	for (i = 0; TRUE; i++)
	{
		byte tmp8u;

		/* Read a byte */
		rd_byte(&tmp8u);

		/* Collect string while legal */
		if (i < max) str[i] = tmp8u;

		/* End of string */
		if (!tmp8u) break;
	}

	/* Terminate */
	str[max-1] = '\0';
}


/*
 * Hack -- strip some bytes
 */
static void strip_bytes(int n)
{
	byte tmp8u;

	/* Strip the bytes */
	while (n--) rd_byte(&tmp8u);
}


/*
 * Owner Conversion -- pre-2.7.8 to 2.7.8
 * Shop is column, Owner is Row, see "tables.c"
 */
static byte convert_owner[24] =
{
	1, 3, 1, 0, 2, 3, 2, 0,
	0, 1, 3, 1, 0, 1, 1, 0,
	3, 2, 0, 2, 1, 2, 3, 0
};



/*
 * Monster conversion -- pre-Oangband 0.5.1 to Oangband 0.5.1.
 *
 * The figure "-1" means that an old monster has no new counterpart, or one
 * that is so much more dangerous as to be an insta-kill risk.
 */
static int mon_index_conv[] =
{
	  0,   1,  12,  13,  14,   2,   3,   4,   5,   6,   7,   8,   9,  10,
	 11,  30,  31,  23,  24,  32,  33,  28,  78,  26,  20,  40,  29,  34,
	 25,  27,  21,  22,  52,  54,  45,  37,  38,  60,  39,  53,  55,  46,
	 43,  47,  56,  41, 664, 666,  51,  50,  49,  42, 665,  67,  72,  61,
	 63,  71,  66,  68,  69,  59,  44,  65,  77,  64,  80,  70,  48,  76,
	 75,  81,  82,  79, 104,  62, 667,  39,  92,  88,  94,  89,  85,  93,
	 87, 119, 131,  86, 106,  60,  37,  38, 105, 101, 139, 108, 107, 103,
	121, 116, 124,  91, 120, 118, 115, 117, 145, 123, 111, 114, 668, 669,
	670, 129,  76, 130, 152, 122,  75, 134, 133,  -1, 137, 142, 141,  90,
	144, 146, 147, 149, 148, 175, 113, 671, 140, 156, 153, 672, 154, 162,
	673, 157, 160, 161, 164, 165, 166, 158, 159, 155, 335, 193, 172,  -1,
	169, 189, 182, 183, 234, 676, 179, 207, 288, 185, 163, 215, 209, 174,
	178, 180, 184, 211, 181, 677, 201, 250, 319, 208, 192, 191, 246, 678,
	197, 199, 202, 173, 196, 198, 204, 213, 243, 214, 226, 221, 222, 228,
	190, 219, 220, 229, 225, 276, 169, 223, 236, 313, 237, 235, 233, 244,
	241, 679, 249, 262, 258, 680, 453, 200, 256, 227, 257, 268, 269, 270,
	277, 266, 272, 683, 684, 280, 283, 255, 292, 291, 282, 685, 300, 299,
	301, 302, 297, 303, 304, 306, 305, 267, 296, 328, 687, 686, 265, 332,
	323, 324, 325, 364, 342, 281, 340, 212, 158, 346, 690, 422, 298, 345,
	347, 248, 688, 689, 352, 355, 353, 691, 692, 330, 354, 358, 327, 341,
	410, 359, 311, 253, 695,  -1, 254, 307, 293, 374, 698, 337, 375, 360,
	372, 373, 201, 706, 361, 378, 380, 390, 389, 386, 696, 387, 393, 697,
	395, 381, 379, 404, 402, 406, 273, 274, 284, 308, 432, 314, 203, 411,
	320, 336, 349, 388, 398, 417, 201, 425, 710, 421, 431, 423, 405, 433,
	441, 382, 203, 440, 490, 506, 383, 701, 702, 703, 435, 436, 447, 434,
	479, 446, 403, 359, 529, 704, 458, 459, 321, 454, 472, 466, 471, 473,
	465, 467, 470, 468, 262, 482, 480, 434, 275, 478, 401, 427, 551, 530,
	489, 418, 368, 487, 315, 707, 426, 437, 443, 316, 492, 460, 488, 562,
	718, 409, 414, 705, 499, 510, 312, 442, 497, 498, 455, 448, 474, 428,
	475, 502, 708, 503, 504, 531, 552, 716, 496, 709, 711, 505, 491, 547,
	481, 523, 712, 713, 532, 537, 512, 509, 361, 533, 517, 520, 524, 589,
	540, 515, 486, 495, 519, 516, 725, 518, 717, 555, 553, 560, 571, 511,
	525, 728, 528, 567, 543, 731, 719, 542, 548, 734, 556, 563, 482, 478,
	572, 579, 737, 568, 720, 602, 721, 545, 578, 482, 605, 732, 745, 738,
	591, 614, 592, 559, 722, 748, 570, 739, 590, 535, 574, 575, 521, 593,
	726, 727, 747, 634, 754, 535, 594, 735, 596, 576, 632, 586, 588, 608,
	729, 730, 601, 522, 733, 744, 751, 736, 639, 740, 749, 482, 750, 619,
	600, 615, 616, 620, 760, 761, 627, 622, 626, 648, 766, 758, 757, 630,
	762, 759, 770, 764, 763, 652, 769, 765, 752, 743, 746, 753,  -1, 771,
	775, 776, 777, 778, 779, 780, 781, 782, 783, 784, 785, 786, 787,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0
};

/*
 * Artifact conversion -- pre-Oangband 0.3.6 to Oangband 0.3.6
 */
static byte conv_arti[128] =
{
	0,
	ART_GALADRIEL,
	ART_ELENDIL,
	ART_THRAIN,
	ART_CARLAMMAS,
	ART_INGWE,
	ART_DWARVES,
	ART_BOROMIR,
	ART_FARAMIR,
	ART_BARAHIR,
	ART_TULKAS,			/* old artifact index 10 */
	ART_NARYA,
	ART_NENYA,
	ART_VILYA,
	ART_POWER,
	ART_STONE_LORE,

	ART_RAZORBACK,
	ART_BLADETURNER,
	0,
	ART_SOULKEEPER,
	ART_ISILDUR,		/* 20  */
	ART_ROHIRRIM,
	ART_BELEGENNON,
	ART_CELEBORN,
	ART_ARVEDUI,
	ART_CASPANION,
	ART_HIMRING,
	ART_HITHLOMIR,
	ART_THALKETTOTH,

	ART_ELEMENTS,
	ART_THORIN,			/* 30 */
	ART_CELEGORM,
	ART_ANARION,
	ART_GIL_GALAD,

	ART_MORGOTH,
	ART_BERUTHIEL,
	ART_THRANDUIL,
	ART_THENGEL,
	ART_HAMMERHAND,
	ART_DOR,
	ART_HOLHENNETH,		/* 40 */
	ART_GORLIM,
	ART_GONDOR,
	ART_NUMENOR,

	ART_VALINOR,
	ART_HOLCOLLETH,
	ART_THINGOL,
	ART_THORONGIL,
	ART_COLANNON,
	ART_LUTHIEN,
	ART_TUOR,			/* 50 */
	0,

	ART_CAMBELEG,
	ART_CAMMITHRIM,
	ART_EOL,
	ART_PAURNIMMEN,
	ART_PAURAEGEN,
	ART_PAURNEN,
	ART_CAMLOST,
	ART_FINGOLFIN,

	ART_FEANOR,			/* 60 */
	ART_DAL,
	ART_THROR,
	ART_NEVRAST,

	ART_MAEDHROS,
	ART_ANGRIST,
	ART_NARTHANC,
	ART_NIMTHANC,
	ART_DETHANC,
	ART_RILIA,
	ART_BELANGIL,		/* 70 */
	ART_CALRIS,
	ART_ARUNRUTH,
	ART_GLAMDRING,
	ART_AEGLIN,
	ART_ORCRIST,
	ART_GURTHANG,
	ART_ZARCUTHRA,
	ART_MORMEGIL,
	ART_GONDRICAM,
	ART_CRISDURIAN,		/* 80 */
	ART_AGLARANG,
	ART_RINGIL,
	ART_ANDURIL,
	ART_ANGUIREL,
	ART_ELVAGIL,
	ART_FORASGIL,
	ART_CARETH,
	ART_STING,
	ART_HARADEKKET,
	ART_GILETTAR,		/* 90 */
	ART_DOOMCALLER,
	0,

	ART_THEODEN,
	ART_PAIN,
	ART_OSONDIR,
	ART_TIL,
	ART_AEGLOS,
	ART_OROME,
	ART_NIMLOTH,
	ART_EORLINGAS,		/* 100 */
	ART_DURIN,
	ART_EONWE,
	ART_BALLI,
	ART_LOTHARANG,
	ART_MUNDWINE,
	ART_BARUKKHELED,
	ART_WRATH,
	ART_ULMO,
	ART_AVAVIR,
	0,				/* 110 */

	ART_GROND,
	ART_TOTILA,
	ART_THUNDERFIST,
	ART_BLOODSPIKE,
	ART_FIRESTAR,
	ART_TARATOL,
	ART_AULE,
	ART_NAR,
	ART_ERIRIL,
	ART_OLORIN,			/* 120 */
	ART_DEATHWREAKER,
	ART_TURMIL,

	ART_HARAD,
	ART_BELTHRONDING,
	ART_BARD,
	ART_CUBRAGOL,
	ART_BUCKLAND		/* 127 */
};

/*
 * Object conversion -- pre-Oangband 0.3.6 to Oangband 0.3.6.
 */
static int obj_index_conv[] =
{
	  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,
	 10,  11,  12,  13,  14,  15,  16,  17,  18,  19,
	 20,  29,  30,  31,  32,  33,  34,  35,  36,  37,
	  0,  61,  58,   0,  56,  50,  70,  65,  45,  52,
	 68,  60,  57,  44,  47,  51,  48,  42,  78,  72,
	 83,  88,  82,  75,  73,  78,  85,  90,   0, 105,
	108, 110, 109, 103,  97, 100, 115, 120, 117,   0,
	106, 112, 125, 130, 131, 134, 135, 127, 142, 143,
	146, 147, 138, 139, 150, 151, 152, 153, 154, 155,
	  0, 159, 160, 161, 167, 168, 170, 171, 175, 176,
	177, 181, 180, 182, 183, 184, 185, 188, 192, 193,
	190, 195, 197, 198, 200, 201, 203, 205, 211, 209,
	207, 196, 253, 230, 233, 237, 238, 240, 245, 247,
	246, 248, 284, 286, 287, 273, 291, 283, 264, 266,
	268, 269, 267, 280, 281, 262, 278, 277, 279, 260,
	263, 289, 288, 276, 261, 282, 270, 271, 272, 274,
	275, 265, 290, 308, 309, 307, 303, 305, 306, 304,
	311, 310, 302, 337, 338, 336, 332, 333, 343, 360,
	334, 344,   0,   0, 324, 328, 329, 330, 356, 345,
	358, 335, 346, 347, 348,   0,   0, 349, 366, 367,
	365, 350, 321, 327, 359, 339, 342, 364, 320, 357,
	352, 362, 341, 323, 340, 322, 325, 353, 353, 355,
	331, 361, 382, 381, 380, 428, 396, 422, 429, 397,
	423, 430, 398, 324, 433, 401, 427, 414, 419, 400,
	415, 416, 417, 432, 439, 391, 387, 389, 386, 409,
	384, 431, 425, 426, 393, 385, 436, 412, 413, 408,
	421, 410, 411, 405, 406, 407, 420, 404, 434, 462,
	472, 474, 473, 461, 468, 455, 456, 464, 465, 463,
	467, 460, 470, 457, 466, 458, 459, 476, 478, 477,
	469, 475, 479,   0, 471,   0,   0,   0, 480, 484,
	512, 510, 511, 504, 528, 503, 508, 529, 507, 502,
	521, 520, 516, 514, 522, 501, 513, 506, 515, 517,
	524, 523, 500, 527, 525, 519, 505, 526, 509, 518,
	530, 531, 532, 533, 485, 481, 482, 483, 616, 617,
	618, 619, 620, 621, 622, 623, 626, 627, 628, 629,
	630, 569, 568, 575, 571, 572, 583, 589, 591, 590,
	587, 585, 584, 586, 581, 582, 593, 595, 594, 592,
	588, 573, 570, 576, 577, 578, 579, 596, 597, 598,
	599,   0,   0,   0,   0,   0,   0,   0,   0, 638,
	639, 641, 642, 644, 643, 648, 647, 646, 645,   0,
	213, 214, 215, 216, 217, 218, 220, 222, 224, 225,
	227, 228, 229,   0,   0, 403, 395, 402, 435, 418,
	419, 438, 437,   0,   0,   0,   0,   0,   0,   0,
	650, 651, 652, 653, 654, 655, 656, 657, 658, 660,
	661, 662, 663, 664, 665, 666, 667, 668, 670, 671,
	672, 673, 674, 675, 676, 677, 678, 680, 681, 682,
	683, 684, 685, 686, 687, 688,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0, 747, 739, 740,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0, 731, 732,
	733, 734, 735, 736, 737, 738, 741, 742, 743, 744,
	745, 746
};

/*
 * Old pre-2.7.4 inventory slot values
 */
#define OLD_INVEN_WIELD     22
#define OLD_INVEN_HEAD      23
#define OLD_INVEN_NECK      24
#define OLD_INVEN_BODY      25
#define OLD_INVEN_ARM       26
#define OLD_INVEN_HANDS     27
#define OLD_INVEN_RIGHT     28
#define OLD_INVEN_LEFT      29
#define OLD_INVEN_FEET      30
#define OLD_INVEN_OUTER     31
#define OLD_INVEN_LITE      32
#define OLD_INVEN_AUX       33

/*
 * Analyze pre-2.7.4 inventory slots
 */
static s16b convert_slot(int old)
{
	/* Move slots */
	switch (old)
	{
		case OLD_INVEN_WIELD: return (INVEN_WIELD);
		case OLD_INVEN_HEAD: return (INVEN_HEAD);
		case OLD_INVEN_NECK: return (INVEN_NECK);
		case OLD_INVEN_BODY: return (INVEN_BODY);
		case OLD_INVEN_ARM: return (INVEN_ARM);
		case OLD_INVEN_HANDS: return (INVEN_HANDS);
		case OLD_INVEN_RIGHT: return (INVEN_RIGHT);
		case OLD_INVEN_LEFT: return (INVEN_LEFT);
		case OLD_INVEN_FEET: return (INVEN_FEET);
		case OLD_INVEN_OUTER: return (INVEN_OUTER);
		case OLD_INVEN_LITE: return (INVEN_LITE);

		/* Hack -- "hold" old aux items */
		case OLD_INVEN_AUX: return (INVEN_WIELD - 1);
	}

	/* Default */
	return (old);
}





/*
 * Hack -- convert pre-2.7.8 ego-item indexes
 */
static byte convert_ego_item[128] =
{
	0,					/* 0 */
	EGO_RESISTANCE,		/* 1 = EGO_RESIST (XXX) */
	EGO_RESIST_ACID,	/* 2 = EGO_RESIST_A (XXX) */
	EGO_RESIST_FIRE,	/* 3 = EGO_RESIST_F (XXX) */
	EGO_RESIST_COLD,	/* 4 = EGO_RESIST_C (XXX) */
	EGO_RESIST_ELEC,	/* 5 = EGO_RESIST_E (XXX) */
	EGO_HA,				/* 6 = EGO_HA */
	EGO_DF,				/* 7 = EGO_DF */
	EGO_SLAY_ANIMAL,	/* 8 = EGO_SLAY_ANIMAL */
	EGO_SLAY_DRAGON,	/* 9 = EGO_SLAY_DRAGON */
	EGO_SLAY_EVIL,		/* 10 = EGO_SLAY_EVIL (XXX) */
	EGO_SLAY_UNDEAD,	/* 11 = EGO_SLAY_UNDEAD (XXX) */
	EGO_BRAND_FIRE,		/* 12 = EGO_FT */
	EGO_BRAND_COLD,		/* 13 = EGO_FB */
	EGO_FREE_ACTION,	/* 14 = EGO_FREE_ACTION (XXX) */
	EGO_SLAYING,		/* 15 = EGO_SLAYING */
	0,					/* 16 */
	0,					/* 17 */
	EGO_SLOW_DESCENT,	/* 18 = EGO_SLOW_DESCENT */
	EGO_SPEED,			/* 19 = EGO_SPEED */
	EGO_STEALTH,		/* 20 = EGO_STEALTH (XXX) */
	0,					/* 21 */
	0,					/* 22 */
	0,					/* 23 */
	EGO_INTELLIGENCE,	/* 24 = EGO_INTELLIGENCE */
	EGO_WISDOM,			/* 25 = EGO_WISDOM */
	EGO_SERENITY,	/* 26 = EGO_INFRAVISION */
	EGO_MIGHT,			/* 27 = EGO_MIGHT */
	EGO_LORDLINESS,		/* 28 = EGO_LORDLINESS */
	EGO_MAGI,			/* 29 = EGO_MAGI (XXX) */
	EGO_BEAUTY,			/* 30 = EGO_BEAUTY */
	EGO_SEEING,			/* 31 = EGO_SEEING (XXX) */
	EGO_REGENERATION,	/* 32 = EGO_REGENERATION */
	0,					/* 33 */
	0,					/* 34 */
	0,					/* 35 */
	0,					/* 36 */
	0,					/* 37 */
	EGO_PERMANENCE,		/* 38 = EGO_ROBE_MAGI */
	EGO_PROTECTION,		/* 39 = EGO_PROTECTION */
	0,					/* 40 */
	0,					/* 41 */
	0,					/* 42 */
	EGO_BRAND_FIRE,		/* 43 = EGO_FIRE (XXX) */
	EGO_HURT_EVIL,		/* 44 = EGO_AMMO_EVIL */
	EGO_HURT_DRAGON,	/* 45 = EGO_AMMO_DRAGON */
	0,					/* 46 */
	0,					/* 47 */
	0,					/* 48 */
	0,					/* 49 */
	EGO_FLAME,			/* 50 = EGO_AMMO_FIRE */
	0,					/* 51 */	/* oops */
	EGO_FROST,			/* 52 = EGO_AMMO_SLAYING */
	0,					/* 53 */
	0,					/* 54 */
	EGO_HURT_ANIMAL,	/* 55 = EGO_AMMO_ANIMAL */
	0,					/* 56 */
	0,					/* 57 */
	0,					/* 58 */
	0,					/* 59 */
	EGO_EXTRA_MIGHT1,	/* 60 = EGO_EXTRA_MIGHT */
	EGO_EXTRA_SHOTS,	/* 61 = EGO_EXTRA_SHOTS */
	0,					/* 62 */
	0,					/* 63 */
	EGO_VELOCITY,		/* 64 = EGO_VELOCITY */
	EGO_ACCURACY,		/* 65 = EGO_ACCURACY */
	0,					/* 66 */
	EGO_SLAY_ORC,		/* 67 = EGO_SLAY_ORC */
	EGO_POWER,			/* 68 = EGO_POWER */
	0,					/* 69 */
	0,					/* 70 */
	EGO_WEST,			/* 71 = EGO_WEST */
	EGO_BLESS_BLADE,	/* 72 = EGO_BLESS_BLADE */
	EGO_SLAY_DEMON,		/* 73 = EGO_SLAY_DEMON */
	EGO_SLAY_TROLL,		/* 74 = EGO_SLAY_TROLL */
	0,					/* 75 */
	0,					/* 76 */
	EGO_WOUNDING,		/* 77 = EGO_AMMO_WOUNDING */
	0,					/* 78 */
	0,					/* 79 */
	0,					/* 80 */
	EGO_LITE,			/* 81 = EGO_LITE */
	EGO_AGILITY,		/* 82 = EGO_AGILITY */
	0,					/* 83 */
	0,					/* 84 */
	EGO_SLAY_GIANT,		/* 85 = EGO_SLAY_GIANT */
	EGO_TELEPATHY,		/* 86 = EGO_TELEPATHY */
	EGO_ELVENKIND,		/* 87 = EGO_ELVENKIND (XXX) */
	0,					/* 88 */
	0,					/* 89 */
	0,				/* Extra attack ego-items are lost. */
	EGO_AMAN,			/* 91 = EGO_AMAN */
	0,					/* 92 */
	0,					/* 93 */
	0,					/* 94 */
	0,					/* 95 */
	0,					/* 96 */
	0,					/* 97 */
	0,					/* 98 */
	0,					/* 99 */
	0,					/* 100 */
	0,					/* 101 */
	0,					/* 102 */
	0,					/* 103 */
	EGO_WEAKNESS,		/* 104 = EGO_WEAKNESS */
	EGO_STUPIDITY,		/* 105 = EGO_STUPIDITY */
	EGO_NAIVETY,		/* 106 = EGO_DULLNESS */
	EGO_SICKLINESS,		/* 107 = EGO_SICKLINESS */
	EGO_CLUMSINESS,		/* 108 = EGO_CLUMSINESS */
	EGO_UGLINESS,		/* 109 = EGO_UGLINESS */
	EGO_TELEPORTATION,	/* 110 = EGO_TELEPORTATION */
	0,					/* 111 */
	EGO_IRRITATION,		/* 112 = EGO_IRRITATION */
	EGO_VULNERABILITY,	/* 113 = EGO_VULNERABILITY */
	EGO_ENVELOPING,		/* 114 = EGO_ENVELOPING */
	0,					/* 115 */
	EGO_SLOWNESS,		/* 116 = EGO_SLOWNESS */
	EGO_NOISE,			/* 117 = EGO_NOISE */
	EGO_TORMENT,		/* 118 = EGO_GREAT_MASS */
	0,					/* 119 */
	EGO_BACKBITING,		/* 120 = EGO_BACKBITING */
	0,					/* 121 */
	0,					/* 122 */
	0,					/* 123 */
	EGO_MORGUL,			/* 124 = EGO_MORGUL */
	0,					/* 125 */
	EGO_SHATTERED,		/* 126 = EGO_SHATTERED */
	EGO_BLASTED			/* 127 = EGO_BLASTED (XXX) */
};



/*
 * Read an object
 *
 * This function attempts to "repair" old savefiles, and to extract
 * the most up to date values for various object fields.
 *
 * Note that Angband 2.7.9 introduced a new method for object "flags"
 * in which the "flags" on an object are actually extracted when they
 * are needed from the object kind, artifact index, ego-item index,
 * and two special "xtra" fields which are used to encode any "extra"
 * power of certain ego-items.  This had the side effect that items
 * imported from pre-2.7.9 savefiles will lose any "extra" powers they
 * may have had, and also, all "uncursed" items will become "cursed"
 * again, including Calris, even if it is being worn at the time.  As
 * a complete hack, items which are inscribed with "uncursed" will be
 * "uncursed" when imported from pre-2.7.9 savefiles.
 *
 * Oangband 0.3.0 changed how rods and wands work, so old-style pvals
 * are converted.  It also changed how artifact and object activations
 * work, so code is included to properly translate old-style activatable
 * items. -LM-
 *
 * Oangband 0.3.6 changed the object and artifact indexes, so old items
 * must be converted to new. -LM-
 *
 * Change extra might to non-pval form if needed -LM-
 *
 * Deal with themed level, gylphs/traps count, player ghosts -LM-
 */
static void rd_item(object_type *o_ptr)
{
	byte old_dd;
	byte old_ds;

	s32b old_cost;

	u32b f1, f2, f3;

	object_kind *k_ptr;

	char buf[128];


	/* Kind */
	rd_s16b(&o_ptr->k_idx);

	/* Location */
	rd_byte(&o_ptr->iy);
	rd_byte(&o_ptr->ix);

	/* Type/Subtype */
	rd_byte(&o_ptr->tval);
	rd_byte(&o_ptr->sval);

	/* Special pval */
	rd_s16b(&o_ptr->pval);

	/* Old method */
	if (older_than(2, 7, 8))
	{
		rd_byte(&o_ptr->name1);
		rd_byte(&o_ptr->name2);
		rd_byte(&o_ptr->ident);
		rd_byte(&o_ptr->number);
		rd_s16b(&o_ptr->weight);
		rd_s16b(&o_ptr->timeout);

		rd_s16b(&o_ptr->to_h);
		rd_s16b(&o_ptr->to_d);
		rd_s16b(&o_ptr->to_a);

		rd_s16b(&o_ptr->ac);

		rd_byte(&old_dd);
		rd_byte(&old_ds);

		strip_bytes(2);

		rd_s32b(&old_cost);

		strip_bytes(4);
	}

	/* New method */
	else
	{
		rd_byte(&o_ptr->discount);
		rd_byte(&o_ptr->number);
		rd_s16b(&o_ptr->weight);

		rd_byte(&o_ptr->name1);
		rd_byte(&o_ptr->name2);
		rd_s16b(&o_ptr->timeout);

		rd_s16b(&o_ptr->to_h);
		rd_s16b(&o_ptr->to_d);
		rd_s16b(&o_ptr->to_a);

		rd_s16b(&o_ptr->ac);

		rd_byte(&old_dd);
		rd_byte(&old_ds);

		rd_byte(&o_ptr->ident);

		rd_byte(&o_ptr->marked);
	}

	/* Old flags */
	strip_bytes(12);

	/* Old version */
	if (older_than(2,8,0))
	{
		/* Old something */
		strip_bytes(2);
	}

	/* New version */
	else
	{
		/* Monster holding object */
		rd_s16b(&o_ptr->held_m_idx);
	}

	if (older_than(2,8,2))
	{
		/* Old special powers */
		strip_bytes(2);
	}

	else
	{
		/* Special powers */
		rd_byte(&o_ptr->xtra1);
		rd_byte(&o_ptr->xtra2);
	}

	/* Feeling */
	if (!o_older_than(0, 4, 1))
	{
		rd_byte(&o_ptr->feel);
	}

	/* Inscription */
	rd_string(buf, 128);

	/* If this savefile is old, maybe we need to translate the feeling */
	if (o_older_than(0, 4, 1))
	{
		int i;

		/* Check each feeling */
		for (i = 0; i < FEEL_MAX; i++)
		{
			/* Skip "empty" feelings */
			if (feel_text[i] == NULL) continue;

			/* Found a match */
			if (streq(buf, feel_text[i]))
			{
				/* Remember the feeling */
				o_ptr->feel = i;

				/* Forget the inscription */
				buf[0] = '\0';

				/* Done */
				break;
			}
		}
	}

	/* Save the inscription */
	if (buf[0]) o_ptr->note = quark_add(buf);


	/* Mega-Hack -- handle "dungeon objects" later */
	if ((older_than(2, 8, 3)) && (o_ptr->k_idx >= 445) &&
		(o_ptr->k_idx <= 479)) return;


	/* Hack - fix a couple svals Oangband changed/deleted along the way. */
	if (o_older_than(0, 3, 6))
	{
		if ((o_ptr->tval == TV_POLEARM) && (o_ptr->sval == 4))
			o_ptr->sval = 5;
		if ((o_ptr->tval == TV_POLEARM) && (o_ptr->sval == 28))
			o_ptr->sval = SV_BATTLE_AXE;
		if ((o_ptr->tval == TV_SWORD) && (o_ptr->sval == 1))
			o_ptr->sval = 2;
		if ((o_ptr->tval == TV_SHOT) && (o_ptr->sval == 0))
			o_ptr->sval = 1;
		if ((o_ptr->tval == TV_CLOAK) && (o_ptr->sval == 6))
			o_ptr->sval = SV_SHADOW_CLOAK;
		if ((o_ptr->tval == TV_BOOTS) && (o_ptr->sval == 6))
			o_ptr->sval = SV_PAIR_OF_METAL_SHOD_BOOTS;
		if ((o_ptr->tval == TV_GLOVES) && (o_ptr->sval == 5))
			o_ptr->sval = SV_SET_OF_CESTI;
		if ((o_ptr->tval == TV_GOLD) && (o_ptr->sval > 11))
			o_ptr->sval = 11;
	}

	/* Object indexes changed in Oangband 0.3.6.  Convert old to new. */
	if (o_older_than(0, 3, 6))
		o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);


	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Obtain tval/sval from k_info */
	o_ptr->tval = k_ptr->tval;
	o_ptr->sval = k_ptr->sval;


	/* Hack -- notice "broken" items */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);


	/* Hack -- the "gold" values changed in 2.7.8 */
	if (older_than(2, 7, 8) && (o_ptr->tval == TV_GOLD))
	{
		/* Extract the value */
		o_ptr->pval = (s16b)old_cost;

		/* Done */
		return;
	}

	/* Convert launchers with +2 or more to might, if not artifacts. */
	if ((o_ptr->name2 == EGO_EXTRA_MIGHT1) && (o_ptr->pval > 1) &&
		(!(o_ptr->name1))) o_ptr->name2 = EGO_EXTRA_MIGHT2;

	/* Ensure that old-style rods and wands obtain appropriate pvals. */
	if (o_older_than(0, 3, 0))
	{
		if (o_ptr->tval == TV_ROD) o_ptr->pval = k_ptr->pval * o_ptr->number;
		if (o_ptr->tval == TV_WAND) o_ptr->pval *= o_ptr->number;
	}


	/* Repair non "wearable" items */
	if (!wearable_p(o_ptr))
	{
		/* Acquire correct fields */
		o_ptr->to_h = k_ptr->to_h;
		o_ptr->to_d = k_ptr->to_d;
		o_ptr->to_a = k_ptr->to_a;

		/* Acquire correct fields */
		o_ptr->ac = k_ptr->ac;
		o_ptr->dd = k_ptr->dd;
		o_ptr->ds = k_ptr->ds;

		/* Acquire correct weight, unless an artifact. */
		if (!o_ptr->name1) o_ptr->weight = k_ptr->weight;


		/* All done */
		return;
	}


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* The ego item indexes changed in 2.7.9 */
	if (older_than(2, 7, 9) && o_ptr->name2)
	{
		/* Convert the ego-item names */
		o_ptr->name2 = convert_ego_item[o_ptr->name2];

		/* Hack -- fix some "Ammo" */
		if ((o_ptr->tval == TV_BOLT) ||
		    (o_ptr->tval == TV_ARROW) ||
		    (o_ptr->tval == TV_SHOT))
		{
			/* Special ego-item indexes */
			if (o_ptr->name2 == EGO_BRAND_FIRE)
			{
				o_ptr->name2 = EGO_FLAME;
			}
			else if (o_ptr->name2 == EGO_SLAYING)
			{
				o_ptr->name2 = EGO_FROST;
			}
			else if (o_ptr->name2 == EGO_SLAY_ANIMAL)
			{
				o_ptr->name2 = EGO_HURT_ANIMAL;
			}
			else if (o_ptr->name2 == EGO_SLAY_EVIL)
			{
				o_ptr->name2 = EGO_HURT_EVIL;
			}
			else if (o_ptr->name2 == EGO_SLAY_DRAGON)
			{
				o_ptr->name2 = EGO_HURT_DRAGON;
			}
		}

		/* Hack -- fix some "Bows" */
		if (o_ptr->tval == TV_BOW)
		{
			/* Special ego-item indexes */
			if (o_ptr->name2 == EGO_MIGHT)
			{
				o_ptr->name2 = EGO_VELOCITY;
			}
		}

		/* Hack -- fix some "Robes" */
		if (o_ptr->tval == TV_SOFT_ARMOR)
		{
			/* Special ego-item indexes */
			if (o_ptr->name2 == EGO_MAGI)
			{
				o_ptr->name2 = EGO_PERMANENCE;
			}
		}

		/* Hack -- fix some "Boots" */
		if (o_ptr->tval == TV_BOOTS)
		{
			/* Special ego-item indexes */
			if (o_ptr->name2 == EGO_STEALTH)
			{
				o_ptr->name2 = EGO_QUIET;
			}
			else if (o_ptr->name2 == EGO_FREE_ACTION)
			{
				o_ptr->name2 = EGO_MOTION;
			}
		}

		/* Hack -- fix some "Shields" */
		if (o_ptr->tval == TV_SHIELD)
		{
			/* Special ego-item indexes */
			if (o_ptr->name2 == EGO_RESIST_ACID)
			{
				o_ptr->name2 = EGO_ENDURE_ACID;
			}
			else if (o_ptr->name2 == EGO_RESIST_ELEC)
			{
				o_ptr->name2 = EGO_ENDURE_ELEC;
			}
			else if (o_ptr->name2 == EGO_RESIST_FIRE)
			{
				o_ptr->name2 = EGO_ENDURE_FIRE;
			}
			else if (o_ptr->name2 == EGO_RESIST_COLD)
			{
				o_ptr->name2 = EGO_ENDURE_COLD;
			}
			else if (o_ptr->name2 == EGO_RESISTANCE)
			{
				o_ptr->name2 = EGO_ENDURANCE;
			}
			else if (o_ptr->name2 == EGO_ELVENKIND)
			{
				o_ptr->name2 = EGO_ENDURANCE;
			}
		}
	}

	/* Hack -- the "searching" bonuses changed in 2.7.6 */
	if (older_than(2, 7, 6))
	{
		/* Reduce the "pval" bonus on "search" */
		if (f1 & (TR1_SEARCH))
		{
			/* Paranoia -- do not lose any search bonuses */
			o_ptr->pval = (o_ptr->pval + 4) / 5;
		}
	}


	/* Verify, convert artifacts if needed. */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Hack -- shift a few old artifact indexes. */
		if ((o_older_than(0, 2, 0)) && (o_ptr->name1 < 14) &&
			(o_ptr->name1 > 7)) o_ptr->name1++;

		/* Artifact indexes changed in Oangband 0.3.6. Convert old to new. */
		if (o_older_than(0, 3, 6))
		{
			if (o_ptr->name1 < 128)
			{
				/* Convert normal artifacts using a table. */
				o_ptr->name1 = conv_arti[o_ptr->name1];
			}
			else
			{
				/* Hack -- shift random artifact indexes. */
				o_ptr->name1 += 82;
			}
		}

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Verify that artifact */
		if (!a_ptr->name) o_ptr->name1 = 0;
	}

	/* Paranoia */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];

		/* Verify that ego-item */
		if (!e_ptr->name) o_ptr->name2 = 0;
	}

	/* Acquire standard fields, unless the item is blasted or shattered. */
	if ((o_ptr->name2 != EGO_BLASTED) && (o_ptr->name2 != EGO_SHATTERED))
		 o_ptr->ac = k_ptr->ac;
	if ((o_ptr->name2 != EGO_BLASTED) && (o_ptr->name2 != EGO_SHATTERED))
		 o_ptr->dd = k_ptr->dd;
	if ((o_ptr->name2 != EGO_BLASTED) && (o_ptr->name2 != EGO_SHATTERED))
		 o_ptr->ds = k_ptr->ds;

	if (o_ptr->name2 == EGO_BALROG) o_ptr->dd = old_dd;
	if (o_ptr->name2 == EGO_DWARVEN) o_ptr->ac += 5;

	/* Acquire standard weight, unless an ego-item. */
	if (!o_ptr->name2) o_ptr->weight = k_ptr->weight;

	/* Starting with Oangband 0.3.0, dragon scale mail activations are assigned
	 * to the object like any other quality.  Perform this task for old dragon
	 * scale mails. -LM-
	 */
	if ((o_older_than(0, 3, 0)) && (o_ptr->tval == TV_DRAG_ARMOR))
	{
		o_ptr->xtra1 = OBJECT_XTRA_TYPE_ACTIVATION;
		switch (o_ptr->sval)
		{
			case SV_DRAGON_BLACK: o_ptr->xtra2 = ACT_DRAGON_BLACK; break;
			case SV_DRAGON_BLUE: o_ptr->xtra2 = ACT_DRAGON_BLUE; break;
			case SV_DRAGON_WHITE: o_ptr->xtra2 = ACT_DRAGON_WHITE; break;
			case SV_DRAGON_RED: o_ptr->xtra2 = ACT_DRAGON_RED; break;
			case SV_DRAGON_GREEN: o_ptr->xtra2 = ACT_DRAGON_GREEN; break;
			case SV_DRAGON_MULTIHUED: o_ptr->xtra2 = ACT_DRAGON_MULTIHUED;
				break;
			case SV_DRAGON_SHINING: o_ptr->xtra2 = ACT_DRAGON_SHINING; break;
			case SV_DRAGON_LAW: o_ptr->xtra2 = ACT_DRAGON_LAW; break;
			case SV_DRAGON_BRONZE: o_ptr->xtra2 = ACT_DRAGON_BRONZE; break;
			case SV_DRAGON_GOLD: o_ptr->xtra2 = ACT_DRAGON_GOLD; break;
			case SV_DRAGON_CHAOS: o_ptr->xtra2 = ACT_DRAGON_CHAOS; break;
			case SV_DRAGON_BALANCE: o_ptr->xtra2 = ACT_DRAGON_BALANCE; break;
			case SV_DRAGON_POWER: o_ptr->xtra2 = ACT_DRAGON_POWER; break;
		}
	}

	/* Hack -- keep some old fields.  Moved from ego-item area. */
	if ((o_ptr->ds < old_ds) && (o_ptr->dd == old_dd))
	{
		/* Keep old enhanced damage dice. */
		o_ptr->ds = old_ds;
	}

	/* Hack -- extract the "broken" flag */
	if (!o_ptr->pval < 0) o_ptr->ident |= (IDENT_BROKEN);


	/* Artifacts */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Acquire new artifact "pval" */
		o_ptr->pval = a_ptr->pval;

		/* Acquire new artifact fields */
		o_ptr->ac = a_ptr->ac;
		o_ptr->dd = a_ptr->dd;
		o_ptr->ds = a_ptr->ds;

		/* Acquire new artifact weight */
		o_ptr->weight = a_ptr->weight;

		/* Hack -- extract the "broken" flag */
		if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- assume "curse" */
		if (older_than(2, 7, 9))
		{
			/* Hack -- assume cursed */
			if (a_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
		}

		/* If artifact index includes an activation, assign it. */
		if (a_ptr->activation)
		{
			o_ptr->xtra1 = OBJECT_XTRA_TYPE_ACTIVATION;
			o_ptr->xtra2 = a_ptr->activation;
		}
	}

	/* Ego items */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];

		/* Hack -- extract the "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- assume "curse" */
		if (older_than(2, 7, 9))
		{
			/* Hack -- assume cursed */
			if (e_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
		}

		/* Hack -- enforce legal pval */
		if (e_ptr->flags1 & (TR1_PVAL_MASK))
		{
			/* Force a meaningful pval */
			if (!o_ptr->pval) o_ptr->pval = 1;
		}
	}


	/* Hack -- assume "cursed" items */
	if (older_than(2, 7, 9))
	{
		/* Hack -- assume cursed */
		if (k_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

		/* Hack -- apply "uncursed" incription */
		if (streq(buf, "uncursed")) o_ptr->ident &= ~(IDENT_CURSED);
	}
}




/*
 * Read a monster
 */
static void rd_monster(monster_type *m_ptr)
{
	byte tmp8u;
	s16b tmp16u;

	/* Read the monster race */
	rd_s16b(&tmp16u);

	/* Convert monster indexes for pre 0.5.1 savefiles. */
	if (o_older_than(0, 5, 1)) m_ptr->r_idx = mon_index_conv[tmp16u];

	/* Otherwise, accept the index stored. */
	else m_ptr->r_idx = tmp16u;

	/* Read the other information */
	rd_byte(&m_ptr->fy);
	rd_byte(&m_ptr->fx);
	rd_s16b(&m_ptr->hp);
	rd_s16b(&m_ptr->maxhp);
	rd_s16b(&m_ptr->csleep);
	rd_byte(&m_ptr->mspeed);
	rd_byte(&m_ptr->energy);
	rd_byte(&m_ptr->stunned);
	rd_byte(&m_ptr->confused);
	rd_byte(&m_ptr->monfear);

	/* Oangband 0.3.3 added monster stasis. */
	if (o_older_than(0, 3, 3)) rd_byte(&tmp8u);
	else rd_byte(&m_ptr->stasis);

	/* Verify monster HPs. */
	if ((o_older_than(0, 5, 1)) && (m_ptr->hp > m_ptr->maxhp))
		m_ptr->hp = m_ptr->maxhp;


	/* Oangband 0.5.0 saves 'smart learn' flags and
	 * Black Breath state.
	 */
	if (!o_older_than(0, 5, 0))
	{
		rd_byte(&tmp8u);
		m_ptr->black_breath = tmp8u;
	}
	if (!o_older_than(0, 5, 0)) rd_u32b(&m_ptr->smart);

	/* Oangband 0.5.0 saves some more data not yet in use */
	if (!o_older_than(0, 5, 0))
	{
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}

	/* Monster extra desire to cast harassment spells */
	if (!o_older_than(0, 5, 0))
	{
		rd_byte(&m_ptr->harass);
	}

	/*
	 * Monster mana.
	 * 0.5.0 saves a placeholder.
	 * 0.5.1 saves real data.
	 */
	if (!o_older_than(0, 5, 0))
	{
		rd_byte(&m_ptr->mana);
	}

	/* Spare */
	if (!o_older_than(0, 5, 0)) rd_s16b(&tmp16u);

}





/*
 * Read the monster lore
 */
static void rd_lore(int r_idx)
{
	byte tmp8u;

	monster_race *r_ptr = &r_info[r_idx];
	monster_lore *l_ptr = &l_list[r_idx];


	/* Pre-2.7.7 */
	if (older_than(2, 7, 7))
	{
		/* Strip old flags */
		strip_bytes(20);

		/* Kills during this life */
		rd_s16b(&l_ptr->pkills);

		/* Strip something */
		strip_bytes(2);

		/* Count observations of attacks */
		rd_byte(&l_ptr->blows[0]);
		rd_byte(&l_ptr->blows[1]);
		rd_byte(&l_ptr->blows[2]);
		rd_byte(&l_ptr->blows[3]);

		/* Count some other stuff */
		rd_byte(&l_ptr->wake);
		rd_byte(&l_ptr->ignore);

		/* Strip something */
		strip_bytes(2);

		/* Count kills by player */
		rd_s16b(&l_ptr->tkills);

		/* Count deaths of player */
		rd_s16b(&l_ptr->deaths);

		/* Read the "Racial" monster limit per level */
		rd_byte(&r_ptr->max_num);

		/* Strip something */
		strip_bytes(1);

		/* Hack -- guess at "sights" */
		l_ptr->sights = MAX(l_ptr->tkills, l_ptr->deaths);
	}

	/* Current */
	else
	{
		/* Count sights/deaths/kills */
		rd_s16b(&l_ptr->sights);
		rd_s16b(&l_ptr->deaths);
		rd_s16b(&l_ptr->pkills);
		rd_s16b(&l_ptr->tkills);

		/* Count wakes and ignores */
		rd_byte(&l_ptr->wake);
		rd_byte(&l_ptr->ignore);

		/* Extra stuff */
		rd_byte(&l_ptr->xtra1);
		rd_byte(&l_ptr->xtra2);

		/* Count drops */
		rd_byte(&l_ptr->drop_gold);
		rd_byte(&l_ptr->drop_item);

		/* Count spells */
		rd_byte(&l_ptr->cast_inate);
		rd_byte(&l_ptr->cast_spell);

		/* Count blows of each type */
		rd_byte(&l_ptr->blows[0]);
		rd_byte(&l_ptr->blows[1]);
		rd_byte(&l_ptr->blows[2]);
		rd_byte(&l_ptr->blows[3]);

		/* Memorize flags */
		rd_u32b(&l_ptr->flags1);
		rd_u32b(&l_ptr->flags2);
		rd_u32b(&l_ptr->flags3);
		rd_u32b(&l_ptr->flags4);
		rd_u32b(&l_ptr->flags5);
		rd_u32b(&l_ptr->flags6);
		if (!o_older_than(1, 1, 0)) rd_u32b(&l_ptr->flags7);

		/* Read the "Racial" monster limit per level */
		rd_byte(&r_ptr->max_num);

		/* Later (?) */
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}

	/* Repair the lore flags */
	l_ptr->flags1 &= r_ptr->flags1;
	l_ptr->flags2 &= r_ptr->flags2;
	l_ptr->flags3 &= r_ptr->flags3;
	l_ptr->flags4 &= r_ptr->flags4;
	l_ptr->flags5 &= r_ptr->flags5;
	l_ptr->flags6 &= r_ptr->flags6;
	if (!o_older_than(1, 1, 0)) l_ptr->flags7 &= r_ptr->flags7;
}




/*
 * Read a store
 */
static errr rd_store(int n)
{
	store_type *st_ptr = &store[n];

	int j;

	byte own, num;

	/* Read the basic info */
	rd_s32b(&st_ptr->store_open);
	rd_s16b(&st_ptr->insult_cur);
	rd_byte(&own);
	rd_byte(&num);
	rd_s16b(&st_ptr->good_buy);
	rd_s16b(&st_ptr->bad_buy);

	/* Extract the owner (see above) */
	st_ptr->owner = (older_than(2, 7, 8) ? convert_owner[own] : own);

	/* Read the items */
	for (j = 0; j < num; j++)
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Read the item */
		rd_item(i_ptr);

		/* Not marked XXX XXX */
		if (older_than(2, 8, 2))
		{
			i_ptr->marked = FALSE;
		}

		/* Acquire valid items */
		if (st_ptr->stock_num < STORE_INVEN_MAX)
		{
			int k = st_ptr->stock_num++;

			/* Acquire the item */
			object_copy(&st_ptr->stock[k], i_ptr);
		}
	}

	/* Success */
	return (0);
}



/*
 * Read RNG state (added in 2.8.0)
 */
static void rd_randomizer(void)
{
	int i;

	u16b tmp16u;

	/* Old version */
	if (older_than(2, 8, 0)) return;

	/* Tmp */
	rd_u16b(&tmp16u);

	/* Place */
	rd_u16b(&Rand_place);

	/* State */
	for (i = 0; i < RAND_DEG; i++)
	{
		rd_u32b(&Rand_state[i]);
	}

	/* Accept */
	Rand_quick = FALSE;
}



/*
 * Read options (ignore most pre-2.8.0 options)
 *
 * Note that the normal options are now stored as a set of 256 bit flags,
 * plus a set of 256 bit masks to indicate which bit flags were defined
 * at the time the savefile was created.  This will allow new options
 * to be added, and old options to be removed, at any time, without
 * hurting old savefiles.
 *
 * The window options are stored in the same way, but note that each
 * window gets 32 options, and their order is fixed by certain defines.
 */
static void rd_options(void)
{
	int i, n;

	byte b;

	u16b c;

	u32b flag[8];
	u32b mask[8];



	/*** Oops ***/

	/* Ignore old options.  Reduced from 16 to 13 in Oangband. */
	/* Reduction accounts for autosave options */
	strip_bytes(13);


	/*** Timed Autosave, inspired by Zangband. ***/
	rd_byte(&b);
	autosave = b;
	rd_s16b(&autosave_freq);


	/*** Special info */

	/* Read "delay_factor" */
	rd_byte(&b);
	op_ptr->delay_factor = b;

	/* Read "hitpoint_warn" */
	rd_byte(&b);
	op_ptr->hitpoint_warn = b;

	rd_u16b(&c); /* Old Cheating options.  Cheaters get a freebe when they convert. */

	/* Pre-2.8.0 savefiles are done */
	if (older_than(2, 8, 0)) return;


	/*** Normal Options ***/

	/* Read the option flags */
	for (n = 0; n < 8; n++) rd_u32b(&flag[n]);

	/* Read the option masks */
	for (n = 0; n < 8; n++) rd_u32b(&mask[n]);

	/* Analyze the options */
	for (i = 0; i < OPT_MAX; i++)
	{
		int os = i / 32;
		int ob = i % 32;

		/* Process real entries */
		if (option_text[i])
		{

			/* Process saved entries */
			if (mask[os] & (1L << ob))
			{
				/* Set flag */
				if (flag[os] & (1L << ob))
				{
					/* Set */
					op_ptr->opt[i] = TRUE;
				}
				/* Clear flag */
				else
				{
					/* Set */
					op_ptr->opt[i] = FALSE;
				}
			}
		}
	}


	/*** Window Options ***/

	/* Read the window flags */
	for (n = 0; n < 8; n++) rd_u32b(&flag[n]);

	/* Read the window masks */
	for (n = 0; n < 8; n++) rd_u32b(&mask[n]);

	/* Analyze the options */
	for (n = 0; n < 8; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 32; i++)
		{
			/* Process valid flags */
			if (window_flag_desc[i])
			{
				/* Process valid flags */
				if (mask[n] & (1L << i))
				{
					/* Set */
					if (flag[n] & (1L << i))
					{
						/* Set */
						op_ptr->window_flag[n] |= (1L << i);
					}
				}
			}
		}
	}

}





/*
 * Hack -- strip the old-style "player ghost" info.
 *
 * XXX XXX XXX This is such a nasty hack it hurts.
 */
static void rd_ghost(void)
{
	char buf[64];

	/* Strip name */
	rd_string(buf, 64);

	/* Older ghosts */
	if (older_than(2, 7, 7))
	{
		/* Strip old data */
		strip_bytes(52);
	}

	/* Newer ghosts */
	else
	{
		/* Strip old data */
		strip_bytes(60);
	}
}


/* String-handling function from Greg Wooledge's random artifact generator. */
static char *my_strdup (const char *s)
{
	char *t = malloc(strlen (s) + 1);
	if (t) strcpy (t, s);
	return t;
}


/*
 * Read the saved random artifacts from a savefile, and add them to the
 * a_name structure.  This code is adopted from Greg Wooledge's random
 * artifacts.
 */
static int convert_saved_names(void)
{
	size_t name_size;
	char *a_base;
	char *a_next;
	char temp[64];

	int i;

	/* Temporary space for names, while reading and randomizing them. */
	char *names[MAX_A_IDX];

	/* Add the permanent artifact names to the temporary array. */
	for (i = 0; i < ART_MIN_RANDOM; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		names[i] = a_name + a_ptr->name;
	}

	/* Add the random artifact names to the temporary array. */
	for (i = ART_MIN_RANDOM; i < MAX_A_IDX; i++)
	{
		rd_string(temp, 64);

		names[i] = my_strdup(temp);
	}


	/* Convert our names array into an a_name structure for later use. */
	name_size = 1;
	for (i = 0; i < MAX_A_IDX; i++)
	{
		name_size += strlen(names[i]) + 1;	/* skip first char */
	}
	if ((a_base = ralloc(name_size)) == NULL)
	{
		note("Memory allocation error");
		return 1;
	}


	a_next = a_base + 1;	/* skip first char */
	for (i = 0; i < MAX_A_IDX; i++)
	{
		strcpy(a_next, names[i]);
		if (a_info[i].tval > 0)		/* skip unused! */
			a_info[i].name = a_next - a_base;
		a_next += strlen(names[i]) + 1;
	}


	/* Free some of our now unneeded memory. */
	KILL (a_name, char);
	for (i = ART_MIN_RANDOM; i < MAX_A_IDX; i++)
	{
		free(names[i]);
	}
	a_name = a_base;

	return 0;
}



/*
 * Read the "extra" information
 */
static errr rd_extra(void)
{
	int i;

	byte tmp8u;
	u16b tmp16u;

	rd_string(op_ptr->full_name, 32);

	rd_string(p_ptr->died_from, 80);

	for (i = 0; i < 4; i++)
	{
		rd_string(p_ptr->history[i], 60);
	}

	/* Class/Race/Gender/Spells */
	rd_byte(&p_ptr->prace);
	rd_byte(&p_ptr->pclass);
	rd_byte(&p_ptr->psex);
	rd_byte(&tmp8u);	/* oops */

	/* Repair psex (2.8.1 beta) */
	if (p_ptr->psex > MAX_SEXES - 1) p_ptr->psex = MAX_SEXES - 1;

	/* Special Race/Class info */
	rd_byte(&p_ptr->hitdie);
	strip_bytes(1);

	/* Age/Height/Weight */
	rd_s16b(&p_ptr->age);
	rd_s16b(&p_ptr->ht);
	rd_s16b(&p_ptr->wt);

	/* Read the stat info */
	for (i = 0; i < A_MAX; i++) rd_s16b(&p_ptr->stat_max[i]);
	for (i = 0; i < A_MAX; i++) rd_s16b(&p_ptr->stat_cur[i]);

	strip_bytes(24);	/* oops */

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);
	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);
	rd_s16b(&p_ptr->lev);

	rd_s16b(&p_ptr->mhp);
	rd_s16b(&p_ptr->chp);
	rd_u16b(&p_ptr->chp_frac);

	rd_s16b(&p_ptr->msp);
	rd_s16b(&p_ptr->csp);
	rd_u16b(&p_ptr->csp_frac);

	rd_s16b(&p_ptr->max_lev);
	rd_s16b(&p_ptr->max_depth);

	/* Hack -- Repair maximum player level */
	if (p_ptr->max_lev < p_ptr->lev) p_ptr->max_lev = p_ptr->lev;

	/* Hack -- Repair maximum dungeon level */
	if (p_ptr->max_depth < 0) p_ptr->max_depth = 1;

	/* More info */
	if (!o_older_than(0, 6, 2))
	{
		rd_s16b(&p_ptr->speed_boost);
		rd_s16b(&p_ptr->heighten_power);
	}
	else strip_bytes(4);
	if (!o_older_than(1, 1, 0))
	{
		rd_byte(&p_ptr->attune_tval);
		rd_byte(&p_ptr->attune_sval);
	}
	else strip_bytes(2);
	strip_bytes(2);
	rd_s16b(&p_ptr->sc);
	strip_bytes(2);

	/* Ignore old redundant info */
	if (older_than(2, 7, 7)) strip_bytes(24);

	/* Read the flags */
	strip_bytes(2);	/* Old "rest" */
	rd_s16b(&p_ptr->blind);
	rd_s16b(&p_ptr->paralyzed);
	rd_s16b(&p_ptr->confused);
	rd_s16b(&p_ptr->food);
	strip_bytes(4);	/* Old "food_digested" / "protection" */
	rd_s16b(&p_ptr->energy);
	rd_s16b(&p_ptr->fast);
	rd_s16b(&p_ptr->slow);
	rd_s16b(&p_ptr->afraid);
	rd_s16b(&p_ptr->cut);
	rd_s16b(&p_ptr->stun);
	rd_s16b(&p_ptr->poisoned);
	rd_s16b(&p_ptr->image);
	rd_s16b(&p_ptr->protevil);
	rd_s16b(&p_ptr->magicdef);
	rd_s16b(&p_ptr->hero);
	rd_s16b(&p_ptr->shero);
	rd_s16b(&p_ptr->shield);
	rd_s16b(&p_ptr->blessed);
	rd_s16b(&p_ptr->tim_invis);
	if (!o_older_than(0, 3, 5))
	{
		rd_s16b(&p_ptr->tim_esp);
		rd_s16b(&p_ptr->superstealth);
		rd_s16b(&p_ptr->ele_attack);
	}
	rd_s16b(&p_ptr->word_recall);
	rd_s16b(&p_ptr->see_infra);


	rd_s16b(&p_ptr->tim_infra);
	rd_s16b(&p_ptr->oppose_fire);
	rd_s16b(&p_ptr->oppose_cold);
	rd_s16b(&p_ptr->oppose_acid);
	rd_s16b(&p_ptr->oppose_elec);
	rd_s16b(&p_ptr->oppose_pois);

	/* Old redundant flags */
	if (older_than(2, 7, 7)) strip_bytes(34);


	if (!o_older_than(0, 3, 5))
	{
		/* Bit flags for various attack modifiers. */
		rd_u32b(&p_ptr->special_attack);
	}
	else
	{
		rd_byte(&tmp8u);	/* Old confusing attack */
	}

	rd_byte(&tmp8u);	/* oops */
	rd_byte(&tmp8u);	/* oops */

	rd_byte((byte *) &p_ptr->black_breath);	/* Status of Black Breath. */

	rd_byte(&p_ptr->searching);

	/* Was maximize mode */
	rd_byte(&tmp8u);

	/* If using an old Angband or a version of O based on an old Angband,
	 * use the old preserve byte.
	 */

	rd_byte(&tmp8u);
	if (older_than(2, 8, 5)) adult_preserve = tmp8u;

	/* Current shapechange. Note: this byte is related to random artifacts
	 * in some versions of Standard Angband */
	rd_byte(&p_ptr->schange);

	/* The number of the bone file (if any) that player ghosts should use to
	 * reacquire a name, sex, class, and race.
	 */
	rd_byte(&bones_selector);

	/* Find out how many thefts have already occured on this level. */
	rd_byte(&number_of_thefts_on_level);

	/* Read number of monster traps on level. */
	rd_byte(&num_trap_on_level);

	/* Read number of glyphs on level. */
	rd_byte(&num_glyph_on_level);

	/* Is the level themed and, if so, which theme is it? */
	rd_byte(&p_ptr->themed_level);

	/* What themed levels have already appeared? */
	rd_u32b(&p_ptr->themed_level_appeared);

	/* Item Squelch */
	for (i = 0; i < 24; i++) rd_byte(&squelch_level[i]);
	for (i = 0; i < 15; i++) rd_byte(&tmp8u);

	/* Specialty Abilities -BR- */
        if (!o_older_than(0, 5, 3))
	{
		for (i = 0; i < MAX_SPECIALTIES; i++) rd_byte(&p_ptr->specialty_order[i]);
	}
	else
	{
		for (i = 0; i < MAX_SPECIALTIES; i++) p_ptr->specialty_order[i] = SP_NO_SPECIALTY;
		strip_bytes(10);
	}

	/* Skip the flags */
	strip_bytes(2);

	/* Hack -- the two "special seeds" */
	rd_u32b(&seed_flavor);
	rd_u32b(&seed_town);


	/* Special stuff */
	rd_u16b(&p_ptr->panic_save);
	rd_u16b(&p_ptr->total_winner);
	rd_u16b(&p_ptr->noscore);


	/* Read "death" */
	rd_byte(&tmp8u);
	p_ptr->is_dead = tmp8u;

	/* Read "feeling" */
	rd_byte(&tmp8u);
	feeling = tmp8u;

	/* Turn of last "feeling" */
	rd_s32b(&old_turn);

	/* Current turn */
	rd_s32b(&turn);

	/* Read the player_hp array */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > PY_MAX_LEVEL)
	{
		note(format("Too many (%u) hitpoint entries!", tmp16u));
		return (25);
	}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++)
	{
		rd_s16b(&p_ptr->player_hp[i]);
	}


	/* Read spell info */
	strip_bytes(8);
	rd_u32b(&p_ptr->spell_worked1);
	rd_u32b(&p_ptr->spell_worked2);
	strip_bytes(72);

	return (0);
}




/*
 * Read the player inventory
 *
 * Note that the inventory changed in Angband 2.7.4.  Two extra
 * pack slots were added and the equipment was rearranged.  Note
 * that these two features combine when parsing old save-files, in
 * which items from the old "aux" slot are "carried", perhaps into
 * one of the two new "inventory" slots.
 *
 * Note that the inventory is "re-sorted" later by "dungeon()".
 */
static errr rd_inventory(void)
{
	int slot = 0;

	object_type *i_ptr;
	object_type object_type_body;


	/* Read until done */
	while (1)
	{
		u16b n;

		/* Get the next item index */
		rd_u16b(&n);

		/* Nope, we reached the end */
		if (n == 0xFFFF) break;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Read the item */
		rd_item(i_ptr);

		/* Not marked XXX XXX */
		if (older_than(2, 8, 2))
		{
			i_ptr->marked = FALSE;
		}

		/* Hack -- verify item */
		if (!i_ptr->k_idx) return(53);

		/* Hack -- convert old slot numbers */
		if (older_than(2, 7, 4)) n = convert_slot(n);

		/* Wield equipment */
		if (n >= INVEN_WIELD)
		{
			/* Copy object */
			object_copy(&inventory[n], i_ptr);

			/* Add the weight */
			p_ptr->total_weight += (i_ptr->number * i_ptr->weight);

			/* One more item */
			p_ptr->equip_cnt++;
		}

		/* Warning -- backpack is full */
		else if (p_ptr->inven_cnt == INVEN_PACK)
		{
			/* Oops */
			note("Too many items in the inventory!");

			/* Fail */
			return (54);
		}

		/* Carry inventory */
		else
		{
			/* Get a slot */
			n = slot++;

			/* Copy object */
			object_copy(&inventory[n], i_ptr);

			/* Add the weight */
			p_ptr->total_weight += (i_ptr->number * i_ptr->weight);

			/* One more item */
			p_ptr->inven_cnt++;
		}
	}

	/* Success */
	return (0);
}



/*
 * Read the saved messages
 */
static void rd_messages(void)
{
	int i;
	char buf[128];
	u16b tmp16u;

	s16b num;

	/* Total */
	rd_s16b(&num);

	/* Read the messages */
	for (i = 0; i < num; i++)
	{
		/* Read the message */
		rd_string(buf, 128);

		/* Read the message type */
		if (!older_than(2, 9, 1))
			rd_u16b(&tmp16u);
		else
			tmp16u = MSG_GENERIC;

		/* Save the message */
		message_add(buf, tmp16u);

	}
}



/*
 * Old "cave grid" flags -- saved in savefile
 */
#define OLD_GRID_W_01	0x0001	/* Wall type (bit 1) */
#define OLD_GRID_W_02	0x0002	/* Wall type (bit 2) */
#define OLD_GRID_PERM	0x0004	/* Wall type is permanent */
#define OLD_GRID_QQQQ	0x0008	/* Unused */
#define OLD_GRID_MARK	0x0010	/* Grid is memorized */
#define OLD_GRID_GLOW	0x0020	/* Grid is illuminated */
#define OLD_GRID_ROOM	0x0040	/* Grid is part of a room */
#define OLD_GRID_ICKY	0x0080	/* Grid is anti-teleport */

/*
 * Masks for the new grid types
 */
#define OLD_GRID_WALL_MASK	0x0003	/* Wall type */

/*
 * Legal results of OLD_GRID_WALL_MASK
 */
#define OLD_GRID_WALL_NONE		0x0000	/* No wall */
#define OLD_GRID_WALL_MAGMA		0x0001	/* Magma vein */
#define OLD_GRID_WALL_QUARTZ	0x0002	/* Quartz vein */
#define OLD_GRID_WALL_GRANITE	0x0003	/* Granite wall */


/*
 * Read pre-2.8.0 dungeon info
 *
 * Try to be more flexible about "too many monsters" XXX XXX
 *
 * Convert the old "flags" and "fake objects" into the new terrain features.
 */
static errr rd_dungeon_aux(s16b depth, s16b py, s16b px)
{
	int i, y, x;
	byte count;
	byte tmp8u;
	u16b start;
	u16b limit;

	/* Read the dungeon */
	for (y = x = 0; y < DUNGEON_HGT; )
	{
		/* Extract some RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			byte info = 0x00;
			byte feat = FEAT_FLOOR;

			/* Old method */
			if (older_than(2, 7, 5))
			{
				/* Extract the old "info" flags */
				if ((tmp8u >> 4) & 0x1) info |= (CAVE_ROOM);
				if ((tmp8u >> 5) & 0x1) info |= (CAVE_MARK);
				if ((tmp8u >> 6) & 0x1) info |= (CAVE_GLOW);

				/* Hack -- process old style "light" */
				if (info & (CAVE_GLOW))
				{
					info |= (CAVE_MARK);
				}

				/* Mega-Hack -- light all walls */
				else if ((tmp8u & 0x0F) >= 12)
				{
					info |= (CAVE_GLOW);
				}

				/* Process the "floor type" */
				switch (tmp8u & 0x0F)
				{
					/* Lite Room Floor */
					case 2:
					{
						info |= (CAVE_GLOW);
					}

					/* Dark Room Floor */
					case 1:
					{
						info |= (CAVE_ROOM);
						break;
					}

					/* Lite Vault Floor */
					case 4:
					{
						info |= (CAVE_GLOW);
					}

					/* Dark Vault Floor */
					case 3:
					{
						info |= (CAVE_ROOM);
						info |= (CAVE_ICKY);
						break;
					}

					/* Corridor Floor */
					case 5:
					{
						break;
					}

					/* Perma-wall */
					case 15:
					{
						feat = FEAT_PERM_SOLID;
						break;
					}

					/* Granite wall */
					case 12:
					{
						feat = FEAT_WALL_EXTRA;
						break;
					}

					/* Quartz vein */
					case 13:
					{
						feat = FEAT_QUARTZ;
						break;
					}

					/* Magma vein */
					case 14:
					{
						feat = FEAT_MAGMA;
						break;
					}
				}
			}

			/* Newer method */
			else
			{
				/* The old "vault" flag */
				if (tmp8u & (OLD_GRID_ICKY)) info |= (CAVE_ICKY);

				/* The old "room" flag */
				if (tmp8u & (OLD_GRID_ROOM)) info |= (CAVE_ROOM);

				/* The old "glow" flag */
				if (tmp8u & (OLD_GRID_GLOW)) info |= (CAVE_GLOW);

				/* The old "mark" flag */
				if (tmp8u & (OLD_GRID_MARK)) info |= (CAVE_MARK);

				/* The old "wall" flags -- granite wall */
				if ((tmp8u & (OLD_GRID_WALL_MASK)) ==
				    OLD_GRID_WALL_GRANITE)
				{
					/* Permanent wall */
					if (tmp8u & (OLD_GRID_PERM))
					{
						feat = FEAT_PERM_SOLID;
					}

					/* Normal wall */
					else
					{
						feat = FEAT_WALL_EXTRA;
					}
				}

				/* The old "wall" flags -- quartz vein */
				else if ((tmp8u & (OLD_GRID_WALL_MASK)) ==
				         OLD_GRID_WALL_QUARTZ)
				{
					/* Assume no treasure */
					feat = FEAT_QUARTZ;
				}

				/* The old "wall" flags -- magma vein */
				else if ((tmp8u & (OLD_GRID_WALL_MASK)) ==
				         OLD_GRID_WALL_MAGMA)
				{
					/* Assume no treasure */
					feat = FEAT_MAGMA;
				}
			}

			/* Save the info */
			cave_info[y][x] = info;

			/* Save the feat */
			cave_set_feat(y, x, feat);

			/* Advance/Wrap */
			if (++x >= DUNGEON_WID)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= DUNGEON_HGT) break;
			}
		}
	}


	/*** Player ***/

	/* Save depth */
	p_ptr->depth = depth;

	/* Place player in dungeon */
	if (!player_place(py, px))
	{
		note(format("Cannot place player (%d,%d)!", py, px));
		return (162);
	}


	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit >= 512)
	{
		note(format("Too many (%d) object entries!", limit));
		return (151);
	}

	/* Read the dungeon items */
	for (i = 1; i < limit; i++)
	{
		object_type *i_ptr;
		object_type object_type_body;


		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Read the item */
		rd_item(i_ptr);

		/* Location */
		y = i_ptr->iy;
		x = i_ptr->ix;


		/* Skip dead objects */
		if (!i_ptr->k_idx) continue;


		/* Hack -- convert old "dungeon" objects */
		if ((older_than(2,8,3)) && (i_ptr->k_idx >= 445) &&
			(i_ptr->k_idx <= 479))
		{
			byte feat = FEAT_FLOOR;

			bool invis = FALSE;

			/* Hack -- catch "invisible traps" */
			if (i_ptr->tval == 101) invis = TRUE;

			/* Analyze the "dungeon objects" */
			switch (i_ptr->k_idx)
			{
				/* Rubble */
				case 445:
				{
					feat = FEAT_RUBBLE;
					break;
				}

				/* Open Door */
				case 446:
				{
					/* Broken door */
					if (i_ptr->pval)
					{
						feat = FEAT_BROKEN;
					}

					/* Open door */
					else
					{
						feat = FEAT_OPEN;
					}

					break;
				}

				/* Closed Door */
				case 447:
				{
					/* Jammed door */
					if (i_ptr->pval < 0)
					{
						feat = (0 - i_ptr->pval) / 2;
						if (feat > 0x07) feat = 0x07;
						feat = FEAT_DOOR_HEAD + 0x08 + feat;
					}

					/* Locked door */
					else
					{
						feat = i_ptr->pval / 2;
						if (feat > 0x07) feat = 0x07;
						feat = FEAT_DOOR_HEAD + feat;
					}

					break;
				}

				/* Secret Door */
				case 448:
				{
					feat = FEAT_SECRET;
					break;
				}

				/* Up Stairs */
				case 449:
				{
					feat = FEAT_LESS;
					break;
				}

				/* Down Stairs */
				case 450:
				{
					feat = FEAT_MORE;
					break;
				}

				/* Store '1' */
				case 451:
				{
					feat = FEAT_SHOP_HEAD + 0x00;
					break;
				}

				/* Store '2' */
				case 452:
				{
					feat = FEAT_SHOP_HEAD + 0x01;
					break;
				}

				/* Store '3' */
				case 453:
				{
					feat = FEAT_SHOP_HEAD + 0x02;
					break;
				}

				/* Store '4' */
				case 454:
				{
					feat = FEAT_SHOP_HEAD + 0x03;
					break;
				}

				/* Store '5' */
				case 455:
				{
					feat = FEAT_SHOP_HEAD + 0x04;
					break;
				}

				/* Store '6' */
				case 456:
				{
					feat = FEAT_SHOP_HEAD + 0x05;
					break;
				}

				/* Store '7' */
				case 457:
				{
					feat = FEAT_SHOP_HEAD + 0x06;
					break;
				}

				/* Store '8' */
				case 458:
				{
					feat = FEAT_SHOP_HEAD + 0x07;
					break;
				}

				/* Glyph of Warding */
				case 459:
				{
					feat = FEAT_GLYPH;
					break;
				}

				/* Trap -- Pit */
				case 460:
				{
					feat = FEAT_TRAP_HEAD + 0x01;
					break;
				}

				/* Trap -- Spiked Pit */
				case 461:
				{
					feat = FEAT_TRAP_HEAD + 0x02;
					break;
				}

				/* Trap -- Trap Door */
				case 462:
				{
					feat = FEAT_TRAP_HEAD + 0x00;
					break;
				}

				/* Trap -- Gas -- Sleep */
				case 463:
				{
					feat = FEAT_TRAP_HEAD + 0x0F;
					break;
				}

				/* Trap -- Loose rock */
				case 464:
				{
					feat = FEAT_TRAP_HEAD + 0x01;
					break;
				}

				/* Trap -- Dart -- lose str */
				case 465:
				{
					feat = FEAT_TRAP_HEAD + 0x09;
					break;
				}

				/* Trap -- Teleport */
				case 466:
				{
					feat = FEAT_TRAP_HEAD + 0x05;
					break;
				}

				/* Trap -- Falling rock */
				case 467:
				{
					feat = FEAT_TRAP_HEAD + 0x03;
					break;
				}

				/* Trap -- Dart -- lose dex */
				case 468:
				{
					feat = FEAT_TRAP_HEAD + 0x0A;
					break;
				}

				/* Trap -- Summoning */
				case 469:
				{
					feat = FEAT_TRAP_HEAD + 0x04;
					break;
				}

				/* Trap -- Fire */
				case 470:
				{
					feat = FEAT_TRAP_HEAD + 0x06;
					break;
				}

				/* Trap -- Acid */
				case 471:
				{
					feat = FEAT_TRAP_HEAD + 0x07;
					break;
				}

				/* Trap -- Gas -- poison */
				case 472:
				{
					feat = FEAT_TRAP_HEAD + 0x0E;
					break;
				}

				/* Trap -- Gas -- blind */
				case 473:
				{
					feat = FEAT_TRAP_HEAD + 0x0C;
					break;
				}

				/* Trap -- Gas -- confuse */
				case 474:
				{
					feat = FEAT_TRAP_HEAD + 0x0D;
					break;
				}

				/* Trap -- Dart -- slow */
				case 475:
				{
					feat = FEAT_TRAP_HEAD + 0x08;
					break;
				}

				/* Trap -- Dart -- lose con */
				case 476:
				{
					feat = FEAT_TRAP_HEAD + 0x0B;
					break;
				}

				/* Trap -- Arrow */
				case 477:
				{
					feat = FEAT_TRAP_HEAD + 0x08;
					break;
				}
			}

			/* Hack -- handle "invisible traps" */
			if (invis) feat = FEAT_INVIS;

			/* Set new bits */
			cave_set_feat(y, x, feat);

			/* Skip it */
			continue;
		}


		/* Hack -- treasure in walls */
		if (i_ptr->tval == TV_GOLD)
		{
			/* Quartz + treasure */
			if ((cave_feat[y][x] == FEAT_QUARTZ) ||
			    (cave_feat[y][x] == FEAT_MAGMA))
			{
				/* Add known treasure */
				cave_set_feat(y, x, cave_feat[y][x] + 0x04);

				/* Done */
				continue;
			}
		}


		/* Give the item to the floor */
		if (!floor_carry(y, x, i_ptr))
		{
			note(format("Cannot place object %d!", o_max));
			return (152);
		}
	}

	/*** Monsters ***/

	/* Extract index of first monster */
	start = (older_than(2, 7, 7) ? 2 : 1);

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify  */
	if (limit >= 1024)
	{
		note(format("Too many (%d) monster entries!", limit));
		return (161);
	}

	/* Read the monsters */
	for (i = start; i < limit; i++)
	{
		monster_type *n_ptr;
		monster_type monster_type_body;

		/* Get local monster */
		n_ptr = &monster_type_body;

		/* Clear the monster */
		(void)WIPE(n_ptr, monster_type);

		/* Read the monster */
		rd_monster(n_ptr);

		/* Hack -- ignore "broken" monsters */
		if (n_ptr->r_idx <= 0) continue;

		/* Hack -- no illegal monsters. */
		if (n_ptr->r_idx >= MAX_R_IDX) continue;


		/* Place monster in dungeon */
		if (!monster_place(n_ptr->fy, n_ptr->fx, n_ptr))
		{
			note(format("Cannot place monster %d!", i));
			return (162);
		}
	}

	/* The dungeon is ready */
	character_dungeon = TRUE;


	/* Success */
	return (0);
}


/*
 * Read the dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 *
 * Note that the size of the dungeon is now hard-coded to
 * DUNGEON_HGT by DUNGEON_WID, and any dungeon with another
 * size will be silently discarded by this routine.
 */
static errr rd_dungeon(void)
{
	int i, y, x;

	s16b depth;
	s16b py, px;
	s16b ymax, xmax;

	byte count;
	byte tmp8u;
	u16b tmp16u;

	u16b limit;


	/*** Basic info ***/

	/* Header info */
	rd_s16b(&depth);
	rd_u16b(&tmp16u);
	rd_s16b(&py);
	rd_s16b(&px);
	rd_s16b(&ymax);
	rd_s16b(&xmax);
	rd_u16b(&tmp16u);
	rd_u16b(&tmp16u);


	/* Ignore illegal dungeons */
	if ((depth < 0) || (depth >= MAX_DEPTH))
	{
		note(format("Ignoring illegal dungeon depth (%d)", depth));
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((ymax != DUNGEON_HGT) || (xmax != DUNGEON_WID))
	{
		/* XXX XXX XXX */
		note(format("Ignoring illegal dungeon size (%d,%d).", xmax, ymax));
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((px < 0) || (px >= DUNGEON_WID) ||
	    (py < 0) || (py >= DUNGEON_HGT))
	{
		note(format("Ignoring illegal player location (%d,%d).", px, py));
		return (1);
	}


	/* Old method */
	if (older_than(2,8,0))
	{
		return (rd_dungeon_aux(depth, py, px));
	}


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < DUNGEON_HGT; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Extract "info" */
			cave_info[y][x] = tmp8u;

			/* Advance/Wrap */
			if (++x >= DUNGEON_WID)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= DUNGEON_HGT) break;
			}
		}
	}


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < DUNGEON_HGT; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Extract "feat" */
			cave_set_feat(y, x, tmp8u);

			/* Advance/Wrap */
			if (++x >= DUNGEON_WID)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= DUNGEON_HGT) break;
			}
		}
	}


	/*** Player ***/

	/* Save depth */
	p_ptr->depth = depth;

	/* Place player in dungeon */
	if (!player_place(py, px))
	{
		note(format("Cannot place player (%d,%d)!", py, px));
		return (162);
	}


	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit >= MAX_O_IDX)
	{
		note(format("Too many (%d) object entries!", limit));
		return (151);
	}

	/* Read the dungeon items */
	for (i = 1; i < limit; i++)
	{
		object_type *i_ptr;
		object_type object_type_body;


		/* Acquire place */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Read the item */
		rd_item(i_ptr);


		/* Monster */
		if (i_ptr->held_m_idx)
		{
			/* Give the object to the monster */
			if (!monster_carry(i_ptr->held_m_idx, i_ptr))
			{
				note(format("Cannot place object %d!", o_max));
				return (152);
			}
		}

		/* Dungeon */
		else
		{
			/* Give the object to the floor */
			if (!floor_carry(i_ptr->iy, i_ptr->ix, i_ptr))
			{
				note(format("Cannot place object %d!", o_max));
				return (152);
			}
		}
	}


	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit >= MAX_M_IDX)
	{
		note(format("Too many (%d) monster entries!", limit));
		return (161);
	}

	/* Read the monsters */
	for (i = 1; i < limit; i++)
	{
		monster_type *n_ptr;
		monster_type monster_type_body;
		monster_race *r_ptr;

		int r_idx;

		/* Get local monster */
		n_ptr = &monster_type_body;

		/* Clear the monster */
		(void)WIPE(n_ptr, monster_type);

		/* Read the monster */
		rd_monster(n_ptr);

		/* Hack -- ignore "broken" monsters */
		if (n_ptr->r_idx <= 0) continue;

		/* Hack -- no illegal monsters. */
		if (n_ptr->r_idx >= MAX_R_IDX) continue;


		/* Access the "r_idx" of the chosen monster */
		r_idx = n_ptr->r_idx;

		/* Access the actual race */
		r_ptr = &r_info[r_idx];

		/* If a player ghost, some special features need to be added. */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			prepare_ghost(n_ptr->r_idx, n_ptr, TRUE);
		}

		/* Place monster in dungeon */
		if (!monster_place(n_ptr->fy, n_ptr->fx, n_ptr))
		{
			note(format("Cannot place monster %d", i));
			return (162);
		}

		/* mark minimum range for recalculation */
		n_ptr->min_range = 0;
	}

	/*
	 * Attach objects carried by a monster to the monster again.
	 * We look for the each object in o_list[] that is carried by
	 * a monster. If the monster isn't carrying any object yet,
	 * then assign it the object. The object with the highest
	 * o_idx is assumed to be at the head of the list of objects
	 * carried by a monster. -TNB-
	 */
	for (i = o_max; i > 0; i--)
	{
		object_type *o_ptr = &o_list[i];
		if (!o_ptr->held_m_idx) continue;
		if (m_list[o_ptr->held_m_idx].hold_o_idx) continue;
		m_list[o_ptr->held_m_idx].hold_o_idx = i;
	}

	/*** Success ***/

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Success */
	return (0);
}



/*
 * Actually read the savefile
 */
static errr rd_savefile_new_aux(void)
{
	int i, j;

	int total_artifacts = 0;
	int random_artifacts = 0;

	bool need_random_artifacts = FALSE;

	byte tmp8u;
	u16b tmp16u;
	u32b tmp32u;


#ifdef VERIFY_CHECKSUMS
	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;
#endif


	/* Mention the savefile version if not current */
	if (o_older_than(O_VERSION_MAJOR, O_VERSION_MINOR, O_VERSION_PATCH))
	{
		if ((o_sf_major == 0) && (o_sf_minor == 2))
			msg_format("Loading an Angband %d.%d.%d savefile.",
			sf_major, sf_minor, sf_patch);
		else msg_format("Loading an Oangband %d.%d.%d savefile.",
			o_sf_major, o_sf_minor, o_sf_patch);
		msg_print(NULL);
	}


	/* Hack -- Warn about "obsolete" versions */
	if (older_than(2, 7, 4))
	{
		note("Warning -- converting obsolete save file.");
	}

	/* Strip the Angband version bytes */
	strip_bytes(4);

	/* Hack -- decrypt */
	xor_byte = sf_extra;


	/* Clear the checksums */
	v_check = 0L;
	x_check = 0L;


	/* Operating system info */
	rd_u32b(&sf_xtra);

	/* Time of savefile creation */
	rd_u32b(&sf_when);

	/* Number of resurrections */
	rd_u16b(&sf_lives);

	/* Number of times played */
	rd_u16b(&sf_saves);

	/* Strip the Oangband version bytes */
	strip_bytes(4);


	/* Later use (always zero) */
	rd_u32b(&tmp32u);


	/* Read RNG state */
	rd_randomizer();
	if (arg_fiddle) note("Loaded Randomizer Info");


	/* Then the options */
	rd_options();
	if (arg_fiddle) note("Loaded Option Flags");

	/* Then the "messages" */
	rd_messages();
	if (arg_fiddle) note("Loaded Messages");


	/* Monster Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > MAX_R_IDX)
	{
		note(format("Too many (%u) monster races!", tmp16u));
		return (21);
	}



	/* Read the available records */
	for (i = 0; i < tmp16u; i++)
	{
		monster_race *r_ptr;
		monster_lore *l_ptr;

		/* Calculate or accept the monster race. */
		if (o_older_than(0, 5, 1))
		{
			j = mon_index_conv[i];

			/* Hack -- handle monsters we can't convert. */
			if ((j < 0) || (j > MAX_R_IDX)) j = 0;
		}
		else j = i;

		/* Read the lore */
		rd_lore(j);

		/* Access that monster */
		r_ptr = &r_info[j];
		l_ptr = &l_list[j];

		/* XXX XXX Hack -- repair old savefiles */
		if (older_than(2, 7, 6))
		{
			/* Assume no kills */
			l_ptr->pkills = 0;

			/* Hack -- no previous lives */
			if (sf_lives == 0)
			{
				/* All kills by this life */
				l_ptr->pkills = l_ptr->tkills;
			}

			/* Hack -- handle uniques */
			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				/* Assume no kills */
				l_ptr->pkills = 0;

				/* Handle dead uniques */
				if (r_ptr->max_num == 0) l_ptr->pkills = 1;
			}
		}
	}
	if (arg_fiddle) note("Loaded Monster Memory");


	/* Object Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > MAX_K_IDX)
	{
		note(format("Too many (%u) object kinds!", tmp16u));
		return (22);
	}


	/* Read the object memory */
	for (i = 0; i < tmp16u; i++)
	{
		byte tmp8u;
		int obj_index;

		object_kind *k_ptr;

		/* Object indexes changed in Oangband 0.3.6.  Convert old to new. */
		if (o_older_than(0, 3, 6))
		{
			obj_index = obj_index_conv[i];
		}
		else
		{
			obj_index = i;
		}

		/* Obtain the "kind" template */
		k_ptr = &k_info[obj_index];

		rd_byte(&tmp8u);

		k_ptr->aware = (tmp8u & 0x01) ? TRUE: FALSE;
		k_ptr->tried = (tmp8u & 0x02) ? TRUE: FALSE;
		k_ptr->known_effect = (tmp8u & 0x04) ? TRUE: FALSE;
		k_ptr->squelch = (tmp8u & 0x08) ? TRUE: FALSE;
	}
	if (arg_fiddle) note("Loaded Object Memory");


	/* Load the Quests */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > MAX_Q_IDX)
	{
		note(format("Too many (%u) quests!", tmp16u));
		return (23);
	}

	/* Load the Quests */
	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&tmp8u);
		q_list[i].level = tmp8u;
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}
	if (arg_fiddle) note("Loaded Quests");


	/* Load the Artifacts */
	rd_u16b(&tmp16u);
	total_artifacts = tmp16u;

	/* If an Oangband 0.3.0 savefile or newer, load the random artifacts.
	 * Although this value is not currently used, it may be useful someday.
	 */
	if (!(o_older_than(0,3,0)))
	{
		rd_u16b(&tmp16u);
		random_artifacts = tmp16u;
	}

	/* Incompatible save files */
	if (total_artifacts > MAX_A_IDX)
	{
		note(format("Too many (%u) artifacts!", total_artifacts));
		return (24);
	}


	/* Reading an old savefile, with no random artifacts. */
	if (o_older_than(0,3,0))
	{
		/* Read the artifact flags */
		for (i = 0; i < total_artifacts; i++)
		{
			rd_byte(&tmp8u);
			a_info[conv_arti[i]].creat_stat = tmp8u;
			rd_byte(&tmp8u);
			rd_byte(&tmp8u);
			rd_byte(&tmp8u);
		}

		/* Later, we need to create the random artifacts, just as if
		 * the player were being born.
		 */
		need_random_artifacts = TRUE;

		if (arg_fiddle) note("Loaded artifacts, will create random artifacts.");
	}

	/* Reading a Oangband 0.3.0+ savefile, with random artifacts. */
	else
	{
		/* Read the artifact info. */
		for (i = 0; i < total_artifacts; i++)
		{
			int j;

			/* Most regular artifact info is stored in a_info.raw.  0.3.X
			 * savefiles have 127 regular artifacts, with indexes that need
			 * converting, and newer savefiles have 209, with up-to-date
			 * indexes.
			 */
			if (i < (o_older_than(0,3,6) ? 128 : ART_MIN_RANDOM))
			{
				rd_byte(&tmp8u);
				if (o_older_than(0,3,6))
					a_info[conv_arti[i]].creat_stat = tmp8u;
				else a_info[i].creat_stat = tmp8u;
				rd_byte(&tmp8u);
				rd_byte(&tmp8u);
				rd_byte(&tmp8u);
			}


			/* But random artifacts are specific to each player. */
			else
			{
				/* Hack - Shift random artifacts for 0.3.X savefiles. */
				if (o_older_than(0,3,6)) j = i + 82;
				else j = i;

				rd_u16b(&tmp16u);
				a_info[j].name = tmp16u;
				rd_u16b(&tmp16u);
				a_info[j].text = tmp16u;

				rd_byte(&tmp8u);
				a_info[j].tval = tmp8u;
				rd_byte(&tmp8u);
				a_info[j].sval = tmp8u;
				rd_u16b(&tmp16u);
				a_info[j].pval = tmp16u;

				rd_u16b(&tmp16u);
				a_info[j].to_h = tmp16u;
				rd_u16b(&tmp16u);
				a_info[j].to_d = tmp16u;
				rd_u16b(&tmp16u);
				a_info[j].to_a = tmp16u;

				rd_byte(&tmp8u);
				a_info[j].dd = tmp8u;
				rd_byte(&tmp8u);
				a_info[j].ds = tmp8u;

				rd_u16b(&tmp16u);
				a_info[j].ac = tmp16u;
				rd_u16b(&tmp16u);
				a_info[j].weight = tmp16u;

				rd_u32b(&tmp32u);
				a_info[j].cost = tmp32u;

				rd_u32b(&tmp32u);
				a_info[j].flags1 = tmp32u;
				rd_u32b(&tmp32u);
				a_info[j].flags2 = tmp32u;
				rd_u32b(&tmp32u);
				a_info[j].flags3 = tmp32u;

				rd_byte(&tmp8u);
				a_info[j].level = tmp8u;
				rd_byte(&tmp8u);
				a_info[j].rarity = tmp8u;
				rd_byte(&tmp8u);
				a_info[j].creat_stat = tmp8u;
				rd_byte(&tmp8u);
				a_info[j].activation = tmp8u;

				/* Extra space. */
				rd_u32b(&tmp32u);
			}
		}
		if (arg_fiddle) note("Loaded artifacts.");
	}


	/* If random artifacts do not need to be generated, read the
	 * random artifact names, and add them to the a_name array.
	 */
	if (!need_random_artifacts)
	{
		u16b num_of_random_arts;

		/* Find out how many random artifacts have stored names. */
		rd_u16b(&num_of_random_arts);

		/* Verify that number, and warn the chap who modified the game that
		 * he still has work to do on failure.
		 */
		if (num_of_random_arts != (MAX_A_IDX - ART_MIN_RANDOM))
		{
			note(format("Number of stored random artifact names (%d) does not", num_of_random_arts));
			note(format("match the number of random artifacts in your copy of Oangband (%d).", MAX_A_IDX - ART_MIN_RANDOM));
		}


		/* Otherwise, add the new names to the a_name structure. */
		else
		{
			int err = convert_saved_names();

			/* Complain if naming fails. */
			if (err) note("Warning - random artifact naming failed!");
		}
	}

	/* Read the extra stuff */
	if (rd_extra()) return (25);

	if (arg_fiddle) note("Loaded extra information");

	/* Important -- Initialize the sex */
	sp_ptr = &sex_info[p_ptr->psex];

	/* Important -- Initialize the race/class */
	rp_ptr = &rp_info[p_ptr->prace];
	cp_ptr = &cp_info[p_ptr->pclass];

	/* Important -- Initialize the magic */
	mp_ptr = &mp_info[p_ptr->pclass];

	/* Important -- Initialize the chest_drops */
	ch_ptr = &ch_info[p_ptr->pclass];

	/* Now that the player information is known, we can generate a set
	 * of random artifacts, if needed.
	 */
	if (need_random_artifacts)
	{
		initialize_random_artifacts();
		if (arg_fiddle) note("Generated random artifacts");
	}


	/* Read the inventory */
	if (rd_inventory())
	{
		note("Unable to read inventory");
		return (21);
	}

	/* Read the stores */
	rd_u16b(&tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		if (rd_store(i)) return (22);
	}

	/* I'm not dead yet... */
	if (!p_ptr->is_dead)
	{
		/* Dead players have no dungeon */
		note("Restoring Dungeon...");
		if (rd_dungeon())
		{
			note("Error reading dungeon data");
			return (34);
		}

		/* Read the ghost info */
		rd_ghost();
	}


#ifdef VERIFY_CHECKSUMS

	/* Recent version */
	if (!older_than(2,8,2))
	{
		/* Save the checksum */
		n_v_check = v_check;

		/* Read the old checksum */
		rd_u32b(&o_v_check);

		/* Verify */
		if (o_v_check != n_v_check)
		{
			note("Invalid checksum");
			return (11);
		}

		/* Save the encoded checksum */
		n_x_check = x_check;

		/* Read the checksum */
		rd_u32b(&o_x_check);

		/* Verify */
		if (o_x_check != n_x_check)
		{
			note("Invalid encoded checksum");
			return (11);
		}
	}

#endif


	/* Hack -- no old-style ghosts. */
	if (older_than(2, 8, 3)) r_info[548].max_num = 0;


	/* Success */
	return (0);

 }


/*
 * Actually read the savefile
 */
errr rd_savefile_new(void)
{
	errr err;

	/* The savefile is a binary file */
	fff = my_fopen(savefile, "rb");

	/* Paranoia */
	if (!fff) return (-1);

	/* Call the sub-function */
	err = rd_savefile_new_aux();

	/* Check for errors */
	if (ferror(fff)) err = -1;

	/* Close the file */
	my_fclose(fff);

	/* Result */
	return (err);
}


/*
 * Open the savefile chosen earlier, and read and save Angband and
 * Oangband version information.  This code is awfully hackish, but it
 * can be adopted for any additional variant version information. -LM-
 */
errr rd_version_info(void)
{
	int fd;

	/* Open the savefile */
	fd = fd_open(savefile, O_RDONLY);

	/* No file.  Report error. */
	if (fd < 0) return (-1);

	fd_read(fd, (char*)&sf_major, 1);
	fd_read(fd, (char*)&sf_minor, 1);
	fd_read(fd, (char*)&sf_patch, 1);
	fd_read(fd, (char*)&sf_extra, 1);

	/* Close the file */
	fd_close(fd);


	/* Open the file */
	fff = my_fopen(savefile, "rb");

	strip_bytes(4);

	/* Hack -- decrypt */
	xor_byte = sf_extra;

	strip_bytes(12);

	/* Read the Oangband version information. */
	rd_byte(&o_sf_major);
	rd_byte(&o_sf_minor);
	rd_byte(&o_sf_patch);
	rd_byte(&o_sf_extra);

	/* Assign a version number of 0.2.0 to savefiles from Angband or
	 * before Oangband 0.3.0.
	 */
	if ((o_sf_major == 0) && (o_sf_minor == 0)) o_sf_minor = 2;


	/* Close the file */
	my_fclose(fff);

	/* Success. */
	return(0);
}
