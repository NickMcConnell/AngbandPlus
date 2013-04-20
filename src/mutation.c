/* File: mutation.c */

/* Purpose: Mutation/Racial Activation Code */

/*
 * Functions included here:
 *
 * gain_random_mutation(); lose_mutation(); dump_mutations();
 * do_cmd_knowledge_mutations(); racial_aux(); cmd_racial_power_aux();
 * do_cmd_racial_power(); process_mutations(); calc_mutations();
 *
 * Note that setting flags for the character display & dump due to
 * mutations is still in files.c, and the effects of the melee attack
 * mutations (Claws, Beak, etc.), are still in cmd1.c. -- Gumby
 *
 * Certain other effects are also scattered about the code. These are mostly
 * for mutations in the fourth set of mutations. I had intended that all such
 * mutations would be in the fourth set, but ended up settling for most. -- RDH
 *
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

/* Psyche powers mutation activations as well as racial powers -- RDH */
void player_psyche(bool birth)
{
	p_ptr->mpsyche = 100 + 5*((p_ptr->lev)/5);
	if (p_ptr->prace == RACE_BEASTMAN)
	{
		p_ptr->mpsyche *= 2;
	}
	if (birth == TRUE) /* Initializing values -- RDH */
	{
		p_ptr->cpsyche = p_ptr->mpsyche;
	}
	if (p_ptr->cpsyche > p_ptr->mpsyche)
	{
		p_ptr->cpsyche = p_ptr->mpsyche;
	}
}

bool gain_random_mutation(int choose_mut)
{
	int attempts_left = 50;
	cptr muta_desc = "";
	bool muta_chosen = FALSE;
	u32b muta_which = 0;
	u32b * muta_class = 0;

	if (choose_mut) attempts_left = 1;

	while (attempts_left--)
	{	/* randint(180) without grav_beam) */
		switch(choose_mut ? choose_mut: randint(181))
		{
		case 1:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_DISTORT_SPACE;
			muta_desc = "You cause spatial distortions.";
			break;
		case 2:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_SCOR_TAIL;
			muta_desc = "You grow a scorpion tail.";
			break;
		case 3: case 4:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SPIT_ACID;
			muta_desc = "You gain the ability to spit acid.";
			break;
		case 5: case 6: case 7:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BR_FIRE;
			muta_desc = "Your lungs feel like furnaces.";
			break;
		case 8: case 9:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_HYPN_GAZE;
			muta_desc = "Your eyes look mesmerizing...";
			break;
		case 10: case 11:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_APPORTATION;
			muta_desc = "You feel a strange magnetism in your bones.";
			break;
		case 12: case 13: case 14:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_VTELEPORT;
			muta_desc = "You gain the power of teleportation at will.";
			break;
		case 15:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BERS_RAGE;
			muta_desc = "You become subject to fits of berserk rage!";
			break;
		case 16:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_COWARDICE;
			muta_desc = "You become an incredible coward!";
			break;
		case 17:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RTELEPORT;
			muta_desc = "Your position seems very uncertain...";
			break;
		case 18:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ALCOHOL;
			muta_desc = "Your body starts producing alcohol!";
			break;
		case 19:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HALLU;
			muta_desc = "You become haunted by strange delusions!";
			break;
		case 20:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_FLATULENT;
			muta_desc = "You become subject to uncontrollable flatulence.";
			break;
		case 21:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_HORNS;
			muta_desc = "You sprout horns!";
			break;
		case 22:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_MUSK;
			muta_desc = "You exude a strange musk!";
			break;
		case 23:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_BEAK;
			muta_desc = "Your mouth and nose become a beak!";
			break;
		case 24:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ABSENT_MIND;
			muta_desc = "You become more forgetful!";
			break;
		case 25:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_TUSKS;
			muta_desc = "You grow long, sharp tusks!";
			break;
		case 26:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BORING;
			muta_desc = "You feel an urge to discuss insurance.";
			break;
		case 27:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_CLAWS;
			muta_desc = "Your fingernails become wicked claws!";
			break;
		case 28: case 29:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HYPER_STR;
			muta_desc = "Your muscles bulge outrageously!";
			break;
		case 30: case 31: case 32:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_PUNY;
			muta_desc = "Your muscles wither away...";
			break;
		case 34:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_TENTACLES;
			muta_desc = "Tentacles erupt from your body!";
			break;
		case 35: case 36:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HYPER_INT;
			muta_desc = "Your intellectual prowess is accentuated!";
			break;
		case 37: case 38: case 39:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MORONIC;
			muta_desc = "Your brain develops a mysterious flaw.";
			break;
		case 40:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_ACID_BLOOD;
			muta_desc = "Your blood becomes highly acidic!";
			break;
		case 41:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_HEART;
			muta_desc = "You grow a second heart!";
			break;
		case 42:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WARTS;
			muta_desc = "Ugly warts cover your body!";
			break;
		case 43:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_FAT;
			muta_desc = "You become sickeningly fat!";
			break;
		case 44:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_VULN_ELEM;
			muta_desc = "You feel vulnerable to the elements!";
			break;
		case 45:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_THIN;
			muta_desc = "You become frightening thin!";
			break;
		case 46:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_FEEL_NUMB;
			muta_desc = "You experience a strange numbness.";
			break;
		case 47: case 48:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FLESH_ROT;
			muta_desc = "Your flesh is afflicted by a rotting disease!";
			break;
		case 49:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WEEP_BLOOD;
			muta_desc = "You begin weeping tears of blood.";
			break;
		case 50:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_METAL_BONES;
			muta_desc = "Your bones become metallic!";
			break;
		case 51: case 52:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RADIATION;
			muta_desc = "You start emitting hard radiation.";
			break;
		case 53:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_THIN_BLOOD;
			muta_desc = "Your blood becomes thin and clots slowly.";
			break;
		case 54:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_BLANK_FAC;
			muta_desc = "Your face becomes completely featureless!";
			break;
		case 55: case 56: case 57:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_EYES;
			muta_desc = "You grow an extra pair of eyes!";
			break;
		case 58: case 59:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_CAMO;
			muta_desc = "You blend into the background.";
			break;
		case 60:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_HUGE_EYEBROW;
			muta_desc = "Your eyebrows become huge.";
			break;
		case 61: case 62:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_NOIS;
			muta_desc = "You start making strange noises!";
			break;
		case 63:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_EAT_ROCK;
			muta_desc = "The walls begin to look tasty.";
			break;
		case 64: case 65:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_INFRAVIS;
			muta_desc = "Your infravision is improved.";
			break;
		case 66: case 67:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_EYES_GLOW;
			muta_desc = "Your eyes begin to glow.";
			break;
		case 68:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_HUGE_ARMS;
			muta_desc = "Your arms bulk up almost comically!";
			break;
		case 69:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MANA_RUNES;
			muta_desc = "Your body is covered in runes!";
			break;
		case 70: case 71:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ELEC_TOUC;
			muta_desc = "Electricity starts running through you!";
			break;
		case 72: case 73:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FIRE_BODY;
			muta_desc = "Your body is enveloped in flames!";
			break;
		case 74: case 75: case 76:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_LEATHER_SKIN;
			muta_desc = "Your skin becomes thick and tough like leather.";
			break;
		case 77:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_ALBINO;
			muta_desc = "You become an albino.";
			break;
		case 78: case 79:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SCALES;
			muta_desc = "You grow a covering of scales!";
			break;
		case 80: case 81:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_PLATES;
			muta_desc = "You grow a covering of armor plates!";
			break;
		case 82: case 83:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FURRY;
			muta_desc = "You grow a coat of thick fur.";
			break;
		case 84:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_ICE_TALONS;
			muta_desc = "Great talons of ice grow from your hands!";
			break;
		case 85: case 86:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_EYE_STALK;
			muta_desc = "Your eyes extend out on stalks.";
			break;
		case 87: case 88:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_REGEN;
			muta_desc = "You start regenerating.";
			break;
		case 89:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_GILLS;
			muta_desc = "You grow gills.";
			break;
		case 90:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ESP;
			muta_desc = "You start hearing strange thoughts - other's thoughts!";
			break;
		case 91: case 92:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_DEMON;
			muta_desc = "You start attracting demons.";
			break;
		case 93: case 94:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MUSHROOMS;
			muta_desc = "Mushrooms grow all over you.";
			break;
		case 95:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_BONY_HEAD;
			muta_desc = "Your skull thickens.";
			break;
		case 96:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIND_BLST;
			muta_desc = "You gain the power of Mind Blast.";
			break;
		case 97:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_PROD_MANA;
			muta_desc = "You start producing magical energy uncontrollably.";
			break;
		case 98:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ILL_NORM;
			muta_desc = "You start projecting a reassuring image.";
			break;
		case 99: case 100:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SPINES;
			muta_desc = "You grow a fearsome covering of sharp spines!";
			break;
		case 101:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_EYELID;
			muta_desc = "You grow a protective secondary eyelid.";
			break;
		case 102:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_SPURS;
			muta_desc = "Your ankles and wrists sprout bony spurs.";
			break;
		case 103: case 104:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_LIMBER;
			muta_desc = "Your muscles become limber.";
			break;
		case 105:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_RAZORS;
			muta_desc = "Razors erupt from your skin in random places.";
			break;
		case 106: case 107:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ARTHRITIS;
			muta_desc = "Your joints suddenly hurt.";
			break;
		case 108:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SLUGGISH;
			muta_desc = "You feel lazy.";
			break;
		case 109:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_ANIMAL;
			muta_desc = "You start attracting animals.";
			break;
		case 110:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_DRAGON;
			muta_desc = "You start attracting dragons.";
			break;
		case 111:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WOUND;
			muta_desc = "Your flesh feels weak.";
			break;
		case 112:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_ANTLERS;
			muta_desc = "You grow antlers!";
			break;
		case 113:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_GREEN_RAD;
			muta_desc = "You sometimes emit green radiation.";
			break;
		case 114:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_HOOVES;
			muta_desc = "You grow hooves!";
			break;
		case 115:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_UNDEAD;
			muta_desc = "You start thinking of cemetaries.";
			break;
		case 116: case 117:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_DISPEL_ALL;
			muta_desc = "You feel a terrifying power lurking behind you.";
			break;
		case 118:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_EAT_LIGHT;
			muta_desc = "You feel a strange hunger for a light snack.";
			break;
		case 119:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RAW_CHAOS;
			muta_desc = "You feel the universe is less stable around you.";
			break;
		case 120:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_UNSTABLE;
			muta_desc = "Your form has become unstable.";
			break;
		case 121:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_POLY_WOUND;
			muta_desc = "You feel forces of Chaos entering your old scars.";
			break;
		case 122:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WASTING;
			muta_desc = "You suddenly contract a horrible wasting disease.";
			break;
		case 123:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_TWISTED;
			muta_desc = "Your body twists itself into an unnatural shape.";
			break;
		case 124:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WEIRD_MIND;
			muta_desc = "Your thoughts suddenly take off in strange directions.";
			break;
		case 125:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_NAUSEA;
			muta_desc = "Your stomach starts to roil.";
			break;
		case 126:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_IRON_HOOVES;
			muta_desc = "You grow iron hooves!";
			break;
		case 127:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_INSIGHT;
			muta_desc = "You have flashes of insight.";
			break;
		case 128:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_PLANE_SHIFT;
			muta_desc = "You can feel the Multiverse all about you.";
			break;
		case 129: case 130:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WARNING;
			muta_desc = "Antennae sprout from your forehead.";
			break;
		case 131:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_INVULN;
			muta_desc = "You are blessed with fits of resilience.";
			break;
		case 132: case 133:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SP_TO_HP;
			muta_desc = "You are subject to fits of magical healing.";
			break;
		case 134:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HP_TO_SP;
			muta_desc = "You are subject to fits of painful clarity.";
			break;
		case 135:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SLOW_MON;
			muta_desc = "Your have a contagious yawn.";
			break;
		case 136:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SUMMON_M;
			muta_desc = "You feel a sudden affinity for life.";
			break;
		case 137:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SUMMON_ANT;
			muta_desc = "You can communicate chemically.";
			break;
		case 138:
			if (!(p_ptr->prace == RACE_VAMPIRE)) /* Vampires are already vampiric! */
			{
				muta_class = &(p_ptr->muta4);
				muta_which = MUT4_V_FANGS;
				muta_desc = "You grow vampiric fangs!";
			}
			break;
		case 139: case 140:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SUMMON_EL;
			muta_desc = "You feel a rapport with the elemental powers.";
			break;
		case 141: case 142:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BR_COLD;
			muta_desc = "Your lungs feel chilly.";
			break;
		case 143:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_NETHER_BALL;
			muta_desc = "You feel in touch with the netherworld.";
			break;
		case 144: case 145:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SHRIEK;
			muta_desc = "Your vocal cords get much tougher.";
			break;
		case 146: case 147: case 148:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_ILLUMINE;
			muta_desc = "You can light up rooms with your presence.";
			break;
		case 149: case 150:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_DET_CURSE;
			muta_desc = "You can feel evil magics.";
			break;
		case 151: case 152: case 153:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BERSERK;
			muta_desc = "You feel a controlled rage.";
			break;
		case 154:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_ICE_SHOWER;
			muta_desc = "You can call down ice storms.";
			break;
		case 155:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_POIS_TONGUE;
			muta_desc = "Your tongue becomes long and venomous.";
			break;
		case 156:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIDAS_TCH;
			muta_desc = "You gain the Midas Touch.";
			break;
		case 157:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_STICKY;
			muta_desc = "You begin exuding a sticky mucous.";
			break;
		case 158: case 159:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RESIST;
			muta_desc = "You feel like you can protect yourself.";
			break;
		case 160:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EARTHQUAKE;
			muta_desc = "You gain the ability to wreck the dungeon.";
			break;
		case 161: case 162: case 163:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_DAZZLE;
			muta_desc = "You gain the ability to emit dazzling lights.";
			break;
		case 164:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_HAND_MOUTH;
			muta_desc = "Round, toothy mouths form in your palms!";
			break;
		case 165:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RECALL;
			muta_desc = "You feel briefly homesick, but it passes.";
			break;
		case 166:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BANISH;
			muta_desc = "You feel a holy wrath fill you.";
			break;
		case 167: case 168:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_LIONS_ROAR;
			muta_desc = "You feel like the king of the beasts.";
			break;
		case 169:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WINGS;
			muta_desc = "Powerful wings erupt from your back!";
			break;
		case 170:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_TRUNK;
			muta_desc = "You grow a trunk like an elephant's.";
			break;
		case 171:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MISSILE;
			muta_desc = "Your hands throb with energy.";
			break;
		case 172: case 173:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SHARD_BOLT;
			muta_desc = "A spiked lump rises from your arm.";
			break;
		case 174:
			muta_class = &(p_ptr->muta1);
			muta_desc = "Your fingers tingle with power.";
			if(rand_int(2)) muta_which = MUT1_INERTIA_BALL;
			break;
		case 175:
			muta_class = &(p_ptr->muta1);
			muta_desc = "Your lungs fill with water.";
			if(rand_int(2)) muta_which = MUT1_BR_WATER;
			break;
		case 176:
			muta_class = &(p_ptr->muta1);
			muta_desc = "Your shoulders swell oddly!";
			muta_which = MUT1_CHAIN_SHARDS;
			break;
		case 177:
			muta_class = &(p_ptr->muta1);
			muta_desc = "Your hair stands on end.";
			muta_which = MUT1_LIGHTNING;
			break;
		case 178:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_POLY_MON;
			muta_desc = "You are filled with polymorphic energies!";
			break;
		case 179: case 180:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_GLOW;
			muta_desc = "Your body starts to shine!";
			break;
		case 181:
			muta_class = &(p_ptr->muta1);
			muta_desc = "You can focus a line of gravity.";
			muta_which = MUT1_GRAV_BEAM;
			break;
		default:
						muta_class = NULL;
						muta_which = 0;
		}

		if (muta_class && muta_which)
		{
			if (!(*(muta_class) & muta_which))
			{
				muta_chosen = TRUE;
			}
		}

		if (muta_chosen == TRUE) break;
	}

	 if (!muta_chosen)
	 {
		  msg_print("You feel quite ordinary.");
		  return FALSE;
	 }
	 else
	 {
		  if (p_ptr->prace == RACE_VAMPIRE && !(p_ptr->muta1 & MUT1_HYPN_GAZE) && (randint(10) < 7))
			{
				muta_class = &(p_ptr->muta1);
				muta_which = MUT1_HYPN_GAZE;
				muta_desc = "Your eyes look mesmerizing...";
			}

		  msg_print("You mutate!");
		  msg_print(muta_desc);
		  *(muta_class) |= muta_which;

		  if (muta_class == &(p_ptr->muta3))
		  {
				if (muta_which == MUT3_PUNY)
				{
					 if (p_ptr->muta3 & MUT3_HYPER_STR)
					 {
						  msg_print("You no longer feel super-strong!");
						  p_ptr->muta3 &= ~(MUT3_HYPER_STR);
					 }
				}
				else if (muta_which == MUT3_HYPER_STR)
				{
					 if (p_ptr->muta3 & MUT3_PUNY)
					 {
						  msg_print("You no longer feel puny!");
						  p_ptr->muta3 &= ~(MUT3_PUNY);
					 }
				}
				else if (muta_which == MUT3_MORONIC)
				{
					 if (p_ptr->muta3 & MUT3_HYPER_INT)
					 {
						  msg_print("You miss Algernon.");
						  p_ptr->muta3 &= ~(MUT3_HYPER_INT);
					 }
				}
				else if (muta_which == MUT3_HYPER_INT)
				{
					 if (p_ptr->muta3 & MUT3_MORONIC)
					 {
						  msg_print("You are feel free of intellectual impairment.");
						  p_ptr->muta3 &= ~(MUT3_MORONIC);
					 }
				}
				else if (muta_which == MUT3_PLATES)
				{
					 if (p_ptr->muta3 & MUT3_SCALES)
					 {
						  msg_print("You lose your scales.");
						  p_ptr->muta3 &= ~(MUT3_SCALES);
					 }
					 if (p_ptr->muta3 & MUT3_LEATHER_SKIN)
					 {
						  msg_print("Your skin is no longer like leather.");
						  p_ptr->muta3 &= ~(MUT3_LEATHER_SKIN);
					 }
				}
				else if (muta_which == MUT3_LEATHER_SKIN)
				{
					 if (p_ptr->muta3 & MUT3_SCALES)
					 {
						  msg_print("You lose your scales.");
						  p_ptr->muta3 &= ~(MUT3_SCALES);
					 }
					 if (p_ptr->muta3 & MUT3_PLATES)
					 {
						  msg_print("Your lose your covering of armor plates.");
						  p_ptr->muta3 &= ~(MUT3_PLATES);
					 }
				}
				else if (muta_which == MUT3_SCALES)
				{
					 if (p_ptr->muta3 & MUT3_LEATHER_SKIN)
					 {
						  msg_print("Your skin is no longer like leather.");
						  p_ptr->muta3 &= ~(MUT3_LEATHER_SKIN);
					 }
					 if (p_ptr->muta3 & MUT3_PLATES)
					 {
						  msg_print("Your lose your covering of armor plates.");
						  p_ptr->muta3 &= ~(MUT3_PLATES);
					 }
					 if (p_ptr->muta3 & MUT3_FURRY)
					 {
						  msg_print("Your lose your covering of fur.");
						  p_ptr->muta3 &= ~(MUT3_FURRY);
					 }
				}
				else if (muta_which == MUT3_FURRY)
				{
					 if (p_ptr->muta3 & MUT3_SCALES)
					 {
						  msg_print("You lose your scales.");
						  p_ptr->muta3 &= ~(MUT3_SCALES);
					 }
				}
				else if (muta_which == MUT3_FLESH_ROT)
				{
					 if (p_ptr->muta3 & MUT3_REGEN)
					 {
						  msg_print("You stop regenerating.");
						  p_ptr->muta3 &= ~(MUT3_REGEN);
					 }
				}
				else if (muta_which == MUT3_REGEN)
				{
					 if (p_ptr->muta3 & MUT3_FLESH_ROT)
					 {
						  msg_print("Your flesh stops rotting.");
						  p_ptr->muta3 &= ~(MUT3_FLESH_ROT);
					 }
				}
				else if (muta_which == MUT3_LIMBER)
				{
					if (p_ptr->muta3 & MUT3_ARTHRITIS)
					{
						msg_print("Your joints stop hurting.");
						p_ptr->muta3 &= ~(MUT3_ARTHRITIS);
					}
					if (p_ptr->muta3 & MUT3_SLUGGISH)
					{
						msg_print("You no longer feel sluggish.");
						p_ptr->muta3 &= ~(MUT3_SLUGGISH);
					}
				}
				else if (muta_which == MUT3_ARTHRITIS)
				{
					if (p_ptr->muta3 & MUT3_LIMBER)
					{
						msg_print("You no longer feel limber.");
						p_ptr->muta3 &= ~(MUT3_LIMBER);
					}
				}
				else if (muta_which == MUT3_XTRA_FAT)
				{
					if (p_ptr->muta3 & MUT3_XTRA_THIN)
					{
						msg_print("You are no longer thin.");
						p_ptr->muta3 &= ~(MUT3_XTRA_THIN);
					}
				}
				else if (muta_which == MUT3_XTRA_THIN)
				{
					if (p_ptr->muta3 & MUT3_XTRA_FAT)
					{
						msg_print("You are no longer fat.");
						p_ptr->muta3 &= ~(MUT3_XTRA_FAT);
					}
				}
				else if (muta_which == MUT3_CAMO)
				{
					if (p_ptr->muta3 & MUT3_XTRA_NOIS)
					{
						msg_print("You stop making strange noises.");
						p_ptr->muta3 &= ~(MUT3_XTRA_NOIS);
					}
				}
				else if (muta_which == MUT3_XTRA_NOIS)
				{
					if (p_ptr->muta3 & MUT3_CAMO)
					{
						msg_print("You can no longer blend into the background.");
						p_ptr->muta3 &= ~(MUT3_CAMO);
					}
				}
				else if (muta_which == MUT3_BLANK_FAC) /* removes many features -- RDH */
				{
					 if (p_ptr->muta1 & MUT1_HYPN_GAZE)
					 {
						  msg_print("You no longer have a gaze with which to hypnotize.");
						  p_ptr->muta1 &= ~(MUT1_HYPN_GAZE);
					 }
					 if (p_ptr->muta3 & MUT3_XTRA_EYES)
					 {
						  msg_print("Your extra eyes disappear along with the originals.");
						  p_ptr->muta3 &= ~(MUT3_XTRA_EYES);
					 }
					 if (p_ptr->muta3 & MUT3_EYES_GLOW)
					 {
						  msg_print("Your eyes, being gone, no longer glow.");
						  p_ptr->muta3 &= ~(MUT3_EYES_GLOW);
					 }
					 if (p_ptr->muta3 & MUT3_EYE_STALK)
					 {
						  msg_print("You no longer have eye stalks.");
						  p_ptr->muta3 &= ~(MUT3_EYE_STALK);
					 }
					 if (p_ptr->muta3 & MUT3_XTRA_EYELID)
					 {
						  msg_print("You no longer have a protective eyelid.");
						  p_ptr->muta3 &= ~(MUT3_XTRA_EYELID);
					 }
					 if (p_ptr->muta4 & MUT4_BEAK)
					 {
						  msg_print("Your beak vanishes.");
						  p_ptr->muta4 &= ~(MUT4_BEAK);
					 }
					 if (p_ptr->muta4 & MUT4_TUSKS)
					 {
						  msg_print("Your tusks vanish.");
						  p_ptr->muta4 &= ~(MUT4_TUSKS);
					 }
					 if (p_ptr->muta4 & MUT4_V_FANGS)
					 {
						  msg_print("Your fangs vanish.");
						  p_ptr->muta4 &= ~(MUT4_V_FANGS);
					 }
					 if (p_ptr->muta4 & MUT4_POIS_TONGUE)
					 {
						  msg_print("Your tongue vanishes.");
						  p_ptr->muta4 &= ~(MUT4_POIS_TONGUE);
					 }
					 if (p_ptr->muta4 & MUT4_TRUNK)
					 {
						  msg_print("Your elephantine trunk vanishes.");
						  p_ptr->muta4 &= ~(MUT4_TRUNK);
					 }
					 if (p_ptr->muta4 & MUT4_HUGE_EYEBROW)
					 {
						  msg_print("Your huge eyebrows vanish.");
						  p_ptr->muta4 &= ~(MUT4_HUGE_EYEBROW);
					 }
				}
		  }
	else if (muta_class == &(p_ptr->muta2))
		  {
				if (muta_which == MUT2_COWARDICE)
				{
					 if (p_ptr->muta1 & MUT1_BERSERK)
					 {
						msg_print("You may no longer berserk at will.");
						p_ptr->muta1 &= ~(MUT1_BERSERK);
					 }
				}
		  }
	else if (muta_class == &(p_ptr->muta1))
		  {
				if (muta_which == MUT1_BERSERK)
				{
					 if (p_ptr->muta2 & MUT2_COWARDICE)
					 {
						msg_print("You are no longer a coward.");
						p_ptr->muta2 &= ~(MUT2_COWARDICE);
					 }
				}
				else if (muta_which == MUT1_LIONS_ROAR)
				{
					 if (p_ptr->muta2 & MUT2_COWARDICE)
					 {
						msg_print("You are no longer a coward.");
						p_ptr->muta2 &= ~(MUT2_COWARDICE);
					 }
				}
				/* Really neat would be combine BR_WATER with BR_FIRE/COLD to get steam/ice breath -- RDH */
				else if (muta_which == MUT1_BR_FIRE)
				{
					 if (p_ptr->muta1 & MUT1_BR_COLD)
					 {
						msg_print("You can no longer exhale deadly cold.");
						p_ptr->muta1 &= ~(MUT1_BR_COLD);
					 }
				}
				else if (muta_which == MUT1_BR_COLD)
				{
					 if (p_ptr->muta1 & MUT1_BR_FIRE)
					 {
						msg_print("You can no longer exhale deadly flame.");
						p_ptr->muta1 &= ~(MUT1_BR_FIRE);
					 }
				}
		  }
	else if (muta_class == &(p_ptr->muta4))
		  {
				if (muta_which == MUT4_BEAK)
				{
					 if (p_ptr->muta4 & MUT4_TUSKS)
					 {
						msg_print("You lose your tusks.");
						p_ptr->muta4 &= ~(MUT4_TUSKS);
					 }
					 if (p_ptr->muta4 & MUT4_TRUNK)
					 {
						msg_print("You lose your trunk.");
						p_ptr->muta4 &= ~(MUT4_TRUNK);
					 }
				}
				else if (muta_which == MUT4_TUSKS)
				{
					 if (p_ptr->muta4 & MUT4_BEAK)
					 {
						msg_print("You lose your beak.");
						p_ptr->muta4 &= ~(MUT4_BEAK);
					 }
					 if (p_ptr->muta4 & MUT4_TRUNK)
					 {
						msg_print("You lose your trunk.");
						p_ptr->muta4 &= ~(MUT4_TRUNK);
					 }
				}
				else if (muta_which == MUT4_TRUNK)
				{
					 if (p_ptr->muta4 & MUT4_TUSKS)
					 {
						msg_print("You lose your tusks.");
						p_ptr->muta4 &= ~(MUT4_TUSKS);
					 }
					 if (p_ptr->muta4 & MUT4_BEAK)
					 {
						msg_print("You lose your beak.");
						p_ptr->muta4 &= ~(MUT4_BEAK);
					 }
				}
				else if (muta_which == MUT4_HOOVES)
				{
					 if (p_ptr->muta4 & MUT4_IRON_HOOVES)
					 {
						p_ptr->muta4 &= ~(MUT4_IRON_HOOVES);
					 }
				}
				else if (muta_which == MUT4_IRON_HOOVES)
				{
					 if (p_ptr->muta4 & MUT4_HOOVES)
					 {
						p_ptr->muta4 &= ~(MUT4_HOOVES);
					 }
				}
				else if (muta_which == MUT4_ANTLERS)
				{
					 if (p_ptr->muta4 & MUT4_HORNS)
					 {
						msg_print("You lose your horns.");
						p_ptr->muta4 &= ~(MUT4_HORNS);
					 }
				}
				else if (muta_which == MUT4_HORNS)
				{
					 if (p_ptr->muta4 & MUT4_ANTLERS)
					 {
						msg_print("You lose your antlers.");
						p_ptr->muta4 &= ~(MUT4_ANTLERS);
					 }
				}
				else if (muta_which == MUT4_CLAWS)
				{
					 if (p_ptr->muta4 & MUT4_ICE_TALONS)
					 {
						msg_print("You lose your talons of ice.");
						p_ptr->muta4 &= ~(MUT4_ICE_TALONS);
					 }
				}
				else if (muta_which == MUT4_ICE_TALONS)
				{
					 if (p_ptr->muta4 & MUT4_CLAWS)
					 {
						msg_print("You lose your claws.");
						p_ptr->muta4 &= ~(MUT4_CLAWS);
					 }
				}
		  }

		  p_ptr->update |= PU_BONUS;
		  handle_stuff();
		  return TRUE;
	 }
}


bool lose_mutation(int choose_mut)
{
	int attempts_left = 500;
	cptr muta_desc = "";
	bool muta_chosen = FALSE;
	u32b muta_which = 0;
	u32b * muta_class = 0;

	if (choose_mut) attempts_left = 1;

	while (attempts_left--)
	{	/* randint(180) without grav_beam */
		switch(choose_mut ? choose_mut: randint(181))
		{
		case 1:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_DISTORT_SPACE;
			muta_desc = "You no longer distort space.";
			break;
		case 2:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_SCOR_TAIL;
			muta_desc = "You lose your scorpion tail.";
			break;
		case 3: case 4:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SPIT_ACID;
			muta_desc = "You lose the ability to spit acid.";
			break;
		case 5: case 6: case 7:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BR_FIRE;
			muta_desc = "Your lungs cool.";
			break;
		case 8: case 9:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_HYPN_GAZE;
			muta_desc = "Your eyes look uninteresting.";
			break;
		case 10: case 11:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_APPORTATION;
			muta_desc = "Objects no longer feel drawn to you.";
			break;
		case 12: case 13: case 14:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_VTELEPORT;
			muta_desc = "You lose the power of teleportation at will.";
			break;
		case 15:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BERS_RAGE;
			muta_desc = "You are no longer subject to fits of berserk rage!";
			break;
		case 16:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_COWARDICE;
			muta_desc = "You are no longer an incredible coward!";
			break;
		case 17:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RTELEPORT;
			muta_desc = "Your position is no longer uncertain...";
			break;
		case 18:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ALCOHOL;
			muta_desc = "Your body stops producing alcohol.";
			break;
		case 19:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HALLU;
			muta_desc = "Reality looks real again.";
			break;
		case 20:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_FLATULENT;
			muta_desc = "Your uncontrollable flatulence is cured!";
			break;
		case 21:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_HORNS;
			muta_desc = "Your horns fall off!";
			break;
		case 22:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_MUSK;
			muta_desc = "Your strange musk goes away!";
			break;
		case 23:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_BEAK;
			muta_desc = "Your beak becomes a mouth and nose!";
			break;
		case 24:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ABSENT_MIND;
			muta_desc = "Your memory returns to normal.";
			break;
		case 25:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_TUSKS;
			muta_desc = "Your tusks shrink back to normal teeth.";
			break;
		case 26:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BORING;
			muta_desc = "You stop discussing insurance.";
			break;
		case 27:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_CLAWS;
			muta_desc = "Your fingernails return to normal.";
			break;
		case 28: case 29:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HYPER_STR;
			muta_desc = "Your muscles revert to normal.";
			break;
		case 30: case 31: case 32:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_PUNY;
			muta_desc = "You recover your strength.";
			break;
		case 34:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_TENTACLES;
			muta_desc = "Your tentacles retract into your body.";
			break;
		case 35: case 36:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HYPER_INT;
			muta_desc = "No more big smarts for you!";
			break;
		case 37: case 38: case 39:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MORONIC;
			muta_desc = "The flaw in your brain is repaired.";
			break;
		case 40:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_ACID_BLOOD;
			muta_desc = "Your blood ceases to be acidic.";
			break;
		case 41:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_HEART;
			muta_desc = "Your body reabsorbs your extra heart!";
			break;
		case 42:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WARTS;
			muta_desc = "Your warts fall off!";
			break;
		case 43:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_FAT;
			muta_desc = "You slim down.";
			break;
		case 44:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_VULN_ELEM;
			muta_desc = "You no longer feel vulnerable to the elements!";
			break;
		case 45:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_THIN;
			muta_desc = "You get some meat on your bones.";
			break;
		case 46:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_FEEL_NUMB;
			muta_desc = "The strange numbness goes away.";
			break;
		case 47: case 48:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FLESH_ROT;
			muta_desc = "You are no longer rotting.";
			break;
		case 49:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WEEP_BLOOD;
			muta_desc = "You stop weeping blood.";
			break;
		case 50:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_METAL_BONES;
			muta_desc = "Your bones are again made of bone!";
			break;
		case 51: case 52:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RADIATION;
			muta_desc = "You no longer emit hard radiation.";
			break;
		case 53:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_THIN_BLOOD;
			muta_desc = "Your blood clots normally again.";
			break;
		case 54:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_BLANK_FAC;
			muta_desc = "Your face returns!";
			break;
		case 55: case 56: case 57:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_EYES;
			muta_desc = "Your extra eyes fall out!";
			break;
		case 58: case 59:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_CAMO;
			muta_desc = "You no longer blend into the background.";
			break;
		case 60:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_HUGE_EYEBROW;
			muta_desc = "Your eyebrows return to normal.";
			break;
		case 61: case 62:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_NOIS;
			muta_desc = "You stop making strange noises!";
			break;
		case 63:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_EAT_ROCK;
			muta_desc = "The walls no longer look tasty.";
			break;
		case 64: case 65:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_INFRAVIS;
			muta_desc = "Your infravision is back to normal.";
			break;
		case 66: case 67:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_EYES_GLOW;
			muta_desc = "Your eyes stop glowing.";
			break;
		case 68:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_HUGE_ARMS;
			muta_desc = "Your arms return to normal size.";
			break;
		case 69:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MANA_RUNES;
			muta_desc = "Your runes fade away.";
			break;
		case 70: case 71:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ELEC_TOUC;
			muta_desc = "You short out.";
			break;
		case 72: case 73:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FIRE_BODY;
			muta_desc = "Your flames go out.";
			break;
		case 74: case 75: case 76:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_LEATHER_SKIN;
			muta_desc = "Your skin returns to normal.";
			break;
		case 77:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_ALBINO;
			muta_desc = "Your skin returns to its normal color.";
			break;
		case 78: case 79:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SCALES;
			muta_desc = "You shed your scales.";
			break;
		case 80: case 81:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_PLATES;
			muta_desc = "Your body reabsorbs the armor plates!";
			break;
		case 82: case 83:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FURRY;
			muta_desc = "You shed your fur.";
			break;
		case 84:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_ICE_TALONS;
			muta_desc = "Your talons melt!";
			break;
		case 85: case 86:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_EYE_STALK;
			muta_desc = "Your eyes return to their sockets.";
			break;
		case 87: case 88:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_REGEN;
			muta_desc = "You stop regenerating.";
			break;
		case 89:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_GILLS;
			muta_desc = "Your gills go away.";
			break;
		case 90:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ESP;
			muta_desc = "Your mind becomes cloudy.";
			break;
		case 91: case 92:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_DEMON;
			muta_desc = "You stop attracting demons.";
			break;
		case 93: case 94:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MUSHROOMS;
			muta_desc = "Your fungal covering falls off.";
			break;
		case 95:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_BONY_HEAD;
			muta_desc = "Your skull returns to normal thickness.";
			break;
		case 96:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIND_BLST;
			muta_desc = "You lose the power of Mind Blast.";
			break;
		case 97:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_PROD_MANA;
			muta_desc = "You stop producing magical energy uncontrollably.";
			break;
		case 98:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ILL_NORM;
			muta_desc = "You stop projecting a reassuring image.";
			break;
		case 99: case 100:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SPINES;
			muta_desc = "Your spines fall off!";
			break;
		case 101:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_EYELID;
			muta_desc = "Your secondary eyelid goes away!";
			break;
		case 102:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_SPURS;
			muta_desc = "Your body reabsorbs its bony spurs.";
			break;
		case 103: case 104:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_LIMBER;
			muta_desc = "Your muscles stiffen.";
			break;
		case 105:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_RAZORS;
			muta_desc = "You shed your razors.";
			break;
		case 106: case 107:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ARTHRITIS;
			muta_desc = "Your joints no longer hurt.";
			break;
		case 108:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SLUGGISH;
			muta_desc = "Your energy returns.";
			break;
		case 109:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_ANIMAL;
			muta_desc = "You stop attracting animals.";
			break;
		case 110:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_DRAGON;
			muta_desc = "You stop attracting dragons.";
			break;
		case 111:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WOUND;
			muta_desc = "Your flesh no longer feels weak.";
			break;
		case 112:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_ANTLERS;
			muta_desc = "Your antlers fall off.";
			break;
		case 113:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_GREEN_RAD;
			muta_desc = "You no longer emit green radiation";
			break;
		case 114:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_HOOVES;
			muta_desc = "Your hooves go away.";
			break;
		case 115:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_UNDEAD;
			muta_desc = "You stop thinking about cemetaries.";
			break;
		case 116: case 117:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_DISPEL_ALL;
			muta_desc = "You no longer feel anything lurking behind you.";
			break;
		case 118:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_EAT_LIGHT;
			muta_desc = "You no longer feel an urge for a light snack.";
			break;
		case 119:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RAW_CHAOS;
			muta_desc = "You feel the universe is more stable around you.";
			break;
		case 120:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_UNSTABLE;
			muta_desc = "Your form stablizes.";
			break;
		case 121:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_POLY_WOUND;
			muta_desc = "You feel forces of Chaos departing your old scars.";
			break;
		case 122:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WASTING;
			muta_desc = "You are cured of the horrible wasting disease.";
			break;
		case 123:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_TWISTED;
			muta_desc = "Your body twists itself back into its natural shape.";
			break;
		case 124:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WEIRD_MIND;
			muta_desc = "Your thoughts stop taking off in strange directions.";
			break;
		case 125:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_NAUSEA;
			muta_desc = "Your stomach settles down.";
			break;
		case 126:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_IRON_HOOVES;
			muta_desc = "You lose your iron hooves.";
			break;
		case 127:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_INSIGHT;
			muta_desc = "You stop having insightful moments.";
			break;
		case 128:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_PLANE_SHIFT;
			muta_desc = "Your senses focus solely on this plane.";
			break;
		case 129: case 130:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WARNING;
			muta_desc = "Your antennae fall off.";
			break;
		case 131:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_INVULN;
			muta_desc = "You are no longer blessed with fits of resilience.";
			break;
		case 132: case 133:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SP_TO_HP;
			muta_desc = "You are no longer subject to fits of magical healing.";
			break;
		case 134:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HP_TO_SP;
			muta_desc = "You are no longer subject to fits of painful clarity.";
			break;
		case 135:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SLOW_MON;
			muta_desc = "You lose your contagious yawn.";
			break;
		case 136:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SUMMON_M;
			muta_desc = "You no longer have an affinity for life.";
			break;
		case 137:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SUMMON_ANT;
			muta_desc = "You no longer produce communications chemicals.";
			break;
		case 138:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_V_FANGS;
			muta_desc = "Your fangs become normal teeth.";
			break;
		case 139: case 140:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SUMMON_EL;
			muta_desc = "You lose your rapport with elementals.";
			break;
		case 141: case 142:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BR_COLD;
			muta_desc = "You can no longer breathe cold.";
			break;
		case 143:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_NETHER_BALL;
			muta_desc = "You feel detached from the netherworld.";
			break;
		case 144: case 145:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SHRIEK;
			muta_desc = "Your vocal cords weaken.";
			break;
		case 146: case 147: case 148:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_ILLUMINE;
			muta_desc = "You can no longer light up rooms.";
			break;
		case 149: case 150:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_DET_CURSE;
			muta_desc = "You can no longer feel evil magics.";
			break;
		case 151: case 152: case 153:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BERSERK;
			muta_desc = "You feel calm.";
			break;
		case 154:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_ICE_SHOWER;
			muta_desc = "You can no longer call down ice storms.";
			break;
		case 155:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_POIS_TONGUE;
			muta_desc = "Your tongue becomes normal.";
			break;
		case 156:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIDAS_TCH;
			muta_desc = "You lose the Midas Touch.";
			break;
		case 157:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_STICKY;
			muta_desc = "You stop exuding a sticky mucous.";
			break;
		case 158: case 159:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RESIST;
			muta_desc = "You feel like you might be vulnerable.";
			break;
		case 160:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EARTHQUAKE;
			muta_desc = "You lose the ability to wreck the dungeon.";
			break;
		case 161: case 162: case 163:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_DAZZLE;
			muta_desc = "You lose the ability to emit dazzling lights.";
			break;
		case 164:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_HAND_MOUTH;
			muta_desc = "The mouths in your hands go away!";
			break;
		case 165:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RECALL;
			muta_desc = "You no longer feel homesick.";
			break;
		case 166:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BANISH;
			muta_desc = "You feel a holy wrath leave you.";
			break;
		case 167: case 168:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_LIONS_ROAR;
			muta_desc = "You feel like a cowardly lion.";
			break;
		case 169:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WINGS;
			muta_desc = "Your wings fly off without you!";
			break;
		case 170:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_TRUNK;
			muta_desc = "Your trunk goes away.";
			break;
		case 171:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MISSILE;
			muta_desc = "Your hands stop throbbing.";
			break;
		case 172: case 173:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SHARD_BOLT;
			muta_desc = "A spiked lump retreats back into your arm.";
			break;
		case 174:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_INERTIA_BALL;
			muta_desc = "Your fingers no longer tremble.";
			break;
		case 175:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BR_WATER;
			muta_desc = "Your lungs dry out.";
			break;
		case 176:
			muta_class = &(p_ptr->muta1);
			muta_desc = "Your shoulders are no longer swelling.";
			muta_which = MUT1_CHAIN_SHARDS;
			break;
		case 177:
			muta_class = &(p_ptr->muta1);
			muta_desc = "Your hair lies flat.";
			muta_which = MUT1_LIGHTNING;
			break;
		case 178:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_POLY_MON;
			muta_desc = "The polymorphic energies go away.";
			break;
		case 179: case 180:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_GLOW;
			muta_desc = "Your body stops shining.";
			break;
		case 181:
			muta_class = &(p_ptr->muta1);
			muta_desc = "You can no longer focus gravity.";
			muta_which = MUT1_GRAV_BEAM;
			break;
		default:
						muta_class = NULL;
						muta_which = 0;
		}

		if (muta_class && muta_which)
		{
			if (*(muta_class) & muta_which)
			{
				muta_chosen = TRUE;
			}
		}
		if (muta_chosen) break;
	}

	if (!muta_chosen)
	{
		return FALSE;
	}
	else
	{
		msg_print(muta_desc);
		*(muta_class) &= ~(muta_which);

		p_ptr->update |= PU_BONUS;
		handle_stuff();
		return TRUE;
	}
}


void dump_mutations(FILE * OutFile)
{
	 if (!OutFile) return;

			if (p_ptr->muta1)
			{
				if (p_ptr->muta1 & MUT1_SPIT_ACID)
				{
					fprintf(OutFile, " You can spit acid (dam lvl*2).\n");
				}
				if (p_ptr->muta1 & MUT1_BR_FIRE)
				{
					fprintf(OutFile, " You can breathe fire (dam lvl*3).\n");
				}
				if (p_ptr->muta1 & MUT1_HYPN_GAZE)
				{
					fprintf(OutFile, " Your gaze is hypnotic.\n");
				}
				if (p_ptr->muta1 & MUT1_APPORTATION)
				{
					fprintf(OutFile, " You can teleport objects.\n");
				}
				if (p_ptr->muta1 & MUT1_VTELEPORT)
				{
					fprintf(OutFile, " You can teleport at will.\n");
				}
				if (p_ptr->muta1 & MUT1_MIND_BLST)
				{
					fprintf(OutFile, " You can Mind Blast your enemies.\n");
				}
				if (p_ptr->muta1 & MUT1_RADIATION)
				{
					fprintf(OutFile, " You can emit hard radiation at will (dam lvl*3).\n");
				}
				if (p_ptr->muta1 & MUT1_MUSHROOMS)
				{
					fprintf(OutFile, " You are covered in edible mushrooms.\n");
				}
				if (p_ptr->muta1 & MUT1_SUMMON_M)
				{
					fprintf(OutFile, " You can summon monsters to aid you.\n");
				}
				if (p_ptr->muta1 & MUT1_SUMMON_EL)
				{
					fprintf(OutFile, " You can summon elementals to your aid.\n");
				}
				if (p_ptr->muta1 & MUT1_BR_COLD)
				{
					fprintf(OutFile, " You can breathe cold (dam lvl*3).\n");
				}
				if (p_ptr->muta1 & MUT1_NETHER_BALL)
				{
					fprintf(OutFile, " You hurl spheres of nether (dam lvl*8).\n");
				}
				if (p_ptr->muta1 & MUT1_SHRIEK)
				{
					fprintf(OutFile, " You can emit a horrible shriek (dam lvl*3).\n");
				}
				if (p_ptr->muta1 & MUT1_ILLUMINE)
				{
					fprintf(OutFile, " You can emit bright light.\n");
				}
				if (p_ptr->muta1 & MUT1_DET_CURSE)
				{
					fprintf(OutFile, " You can feel the danger of evil magic.\n");
				}
				if (p_ptr->muta1 & MUT1_BERSERK)
				{
					fprintf(OutFile, " You can drive yourself into a berserk frenzy.\n");
				}
				if (p_ptr->muta1 & MUT1_ICE_SHOWER)
				{
					fprintf(OutFile, " You can call down ice storms.\n");
				}
				if (p_ptr->muta1 & MUT1_MIDAS_TCH)
				{
					fprintf(OutFile, " You can turn ordinary items to gold.\n");
				}
				if (p_ptr->muta1 & MUT1_SUMMON_ANT)
				{
					fprintf(OutFile, " You can summon ants.\n");
				}
				if (p_ptr->muta1 & MUT1_RESIST)
				{
					fprintf(OutFile, " You can harden yourself to the ravages of the elements.\n");
				}
				if (p_ptr->muta1 & MUT1_EARTHQUAKE)
				{
					fprintf(OutFile, " You can bring down the dungeon around your ears.\n");
				}
				if (p_ptr->muta1 & MUT1_DAZZLE)
				{
					fprintf(OutFile, " You can emit confusing, blinding radiation.\n");
				}
				if (p_ptr->muta1 & MUT1_RECALL)
				{
					fprintf(OutFile, " You can travel between town and the depths.\n");
				}
				if (p_ptr->muta1 & MUT1_BANISH)
				{
					fprintf(OutFile, " You can send evil creatures directly to Hell.\n");
				}
				if (p_ptr->muta1 & MUT1_LIONS_ROAR)
				{
					fprintf(OutFile, " You can roar like a mighty lion. (fear)\n");
				}
				if (p_ptr->muta1 & MUT1_MISSILE)
				{
					fprintf(OutFile, " You can cast magical bolts.\n");
				}
				if (p_ptr->muta1 & MUT1_SHARD_BOLT)
				{
					fprintf(OutFile, " You can cast shards.\n");
				}
				if (p_ptr->muta1 & MUT1_INERTIA_BALL)
				{
					fprintf(OutFile, " You create inertial fields.\n");
				}
				if (p_ptr->muta1 & MUT1_BR_WATER)
				{
					fprintf(OutFile, " You can exhale water.\n");
				}
				if (p_ptr->muta1 & MUT1_CHAIN_SHARDS)
				{
					fprintf(OutFile, " You can cast shards rapidly.\n");
				}
				if (p_ptr->muta1 & MUT1_LIGHTNING)
				{
					fprintf(OutFile, " You can fire lightning bolts (dam lvl*3).\n");
				}
				if (p_ptr->muta1 & MUT1_GRAV_BEAM)
				{
					fprintf(OutFile, " You can shoot a beam of gravity.\n");
				}
		  }

			if (p_ptr->muta2)
			{
				if (p_ptr->muta2 & MUT2_BERS_RAGE)
					{
						fprintf(OutFile, " You are subject to berserker fits.\n");
					}
					if (p_ptr->muta2 & MUT2_COWARDICE)
					{
						fprintf(OutFile, " You are subject to cowardice.\n");
					}
					if (p_ptr->muta2 & MUT2_RTELEPORT)
					{
						fprintf(OutFile, " You are teleporting randomly.\n");
					}
					if (p_ptr->muta2 & MUT2_ALCOHOL)
					{
						fprintf(OutFile, " Your body produces alcohol.\n");
					}
					if (p_ptr->muta2 & MUT2_HALLU)
					{
						fprintf(OutFile, " You have a hallucinatory insanity.\n");
					}
					if (p_ptr->muta2 & MUT2_FLATULENT)
					{
						fprintf(OutFile, " You are subject to uncontrollable flatulence.\n");
					}
					if (p_ptr->muta2 & MUT2_MUSK)
					{
						fprintf(OutFile, " You exude a strange musk.\n");
					}
					if (p_ptr->muta2 & MUT2_ABSENT_MIND)
					{
						fprintf(OutFile, " You sometimes forget where you are.\n");
					}
					if (p_ptr->muta2 & MUT2_BORING)
					{
						fprintf(OutFile, " You are a tremendous bore.\n");
					}
					if (p_ptr->muta2 & MUT2_GREEN_RAD)
					{
						fprintf(OutFile, " You sometimes emit a green radiation.\n");
					}
					if (p_ptr->muta2 & MUT2_ATT_UNDEAD)
					{
						fprintf(OutFile, " You attract the undead.\n");
					}
					if (p_ptr->muta2 & MUT2_PROD_MANA)
					{
						fprintf(OutFile, " You are producing magical energy uncontrollably.\n");
					}
					if (p_ptr->muta2 & MUT2_WOUND)
					{
						fprintf(OutFile, " Your flesh is very delicate.\n");
					}
					if (p_ptr->muta2 & MUT2_ATT_ANIMAL)
					{
						fprintf(OutFile, " You attract animals.\n");
					}
					if (p_ptr->muta2 & MUT2_ATT_DEMON)
					{
						fprintf(OutFile, " You attract demons.\n");
					}
					if (p_ptr->muta2 & MUT2_ATT_DRAGON)
					{
						fprintf(OutFile, " You attract dragons.\n");
					}
					if (p_ptr->muta2 & MUT2_DISPEL_ALL)
					{
						fprintf(OutFile, " You are shrouded in evil.\n");
					}
					if (p_ptr->muta2 & MUT2_EAT_LIGHT)
					{
						fprintf(OutFile, " You sometimes feed on the light around you.\n");
					}
					if (p_ptr->muta2 & MUT2_RAW_CHAOS)
					{
						fprintf(OutFile, " You occasionally are surrounded with raw chaos.\n");
					}
					if (p_ptr->muta2 & MUT2_UNSTABLE)
					{
						fprintf(OutFile, " Your form is unstable.\n");
					}
					if (p_ptr->muta2 & MUT2_POLY_WOUND)
					{
						fprintf(OutFile, " Your health is subject to chaotic forces.\n");
					}
					if (p_ptr->muta2 & MUT2_WASTING)
					{
						fprintf(OutFile, " You have a horrible wasting disease.\n");
					}
					if (p_ptr->muta2 & MUT2_WEIRD_MIND)
					{
						fprintf(OutFile, " Your mind randomly expands and contracts.\n");
					}
					if (p_ptr->muta2 & MUT2_NAUSEA)
					{
						fprintf(OutFile, " You have a seriously upset stomach.\n");
					}
					if (p_ptr->muta2 & MUT2_INSIGHT)
					{
						fprintf(OutFile, " You have flashes of insight.\n");
					}
					if (p_ptr->muta2 & MUT2_PLANE_SHIFT)
					{
						fprintf(OutFile, " You occasionally slip into another plane.\n");
					}
					if (p_ptr->muta2 & MUT2_WARNING)
					{
						fprintf(OutFile, " Your sensitive antennae detect monsters.\n");
					}
					if (p_ptr->muta2 & MUT2_INVULN)
					{
						fprintf(OutFile, " You occasionally feel resilient.\n");
					}
					if (p_ptr->muta2 & MUT2_SP_TO_HP)
					{
						fprintf(OutFile, " Your blood sometimes rushes to your muscles.\n");
					}
					if (p_ptr->muta2 & MUT2_HP_TO_SP)
					{
						fprintf(OutFile, " Your blood sometimes rushes to your head.\n");
					}
					if (p_ptr->muta2 & MUT2_SLOW_MON)
					{
						fprintf(OutFile, " You occasionally slow monsters.\n");
					}
					if (p_ptr->muta2 & MUT2_POLY_MON)
					{
						fprintf(OutFile, " You occasionally polymorph monsters.\n");
					}
			}

		  if (p_ptr->muta3)
		  {
					if (p_ptr->muta3 & MUT3_HYPER_STR)
					{
						fprintf(OutFile, " You are superhumanly strong (+4 STR).\n");
					}
					 if (p_ptr->muta3 & MUT3_PUNY)
					 {
								fprintf(OutFile, " You are puny (-4 STR).\n");
					 }
					 if (p_ptr->muta3 & MUT3_HYPER_INT)
					 {
								fprintf(OutFile, " Your brain performs efficiently (+4 INT/WIS).\n");
					 }
					 if (p_ptr->muta3 & MUT3_MORONIC)
					 {
								fprintf(OutFile, " You are moronic (-4 INT/WIS).\n");
					 }
					 if (p_ptr->muta3 & MUT3_XTRA_HEART)
					 {
								fprintf(OutFile, " You have an extra heart (+4 CON, +2 Speed).\n");
					 }
					 if (p_ptr->muta3 & MUT3_XTRA_FAT)
					 {
								fprintf(OutFile, " You are extremely fat (+2 CON, -2 Speed).\n");
					 }
					 if (p_ptr->muta3 & MUT3_XTRA_THIN)
					 {
								fprintf(OutFile, " You are extremely thin (-2 CON, +2 Speed).\n");
					 }
					 if (p_ptr->muta3 & MUT3_FLESH_ROT)
					 {
								fprintf(OutFile, " Your flesh is rotting (-2 CON, -1 CHR).\n");
					 }
					 if (p_ptr->muta3 & MUT3_METAL_BONES)
					 {
								fprintf(OutFile, " You have metal bones (+10 percent HPs).\n");
					 }
					 if (p_ptr->muta3 & MUT3_BLANK_FAC)
					 {
								fprintf(OutFile, " Your face is featureless (-1 CHR).\n");
					 }
					if (p_ptr->muta3 & MUT3_ILL_NORM)
					{
						fprintf(OutFile, " Your appearance is masked with illusion.\n");
					}
					 if (p_ptr->muta3 & MUT3_XTRA_EYES)
					 {
								fprintf(OutFile, " You have extra eyes (x2 Perc +20 Search -2 Chr).\n");
					 }
					 if (p_ptr->muta3 & MUT3_CAMO)
					 {
								fprintf(OutFile, " You blend into the background (+3 Stealth).\n");
					 }
					 if (p_ptr->muta3 & MUT3_XTRA_NOIS)
					 {
								fprintf(OutFile, " You make a lot of strange noise (-3 stealth).\n");
					 }
					 if (p_ptr->muta3 & MUT3_INFRAVIS)
					 {
								fprintf(OutFile, " You have remarkable infravision (+3).\n");
					 }
					 if (p_ptr->muta3 & MUT3_EYES_GLOW)
					 {
								fprintf(OutFile, " You have glowing eyes (see invisible).\n");
					 }
					 if (p_ptr->muta3 & MUT3_MANA_RUNES)
					 {
								fprintf(OutFile, " You are covered in strange runes (+20 percent mana).\n");
					 }
					 if (p_ptr->muta3 & MUT3_ELEC_TOUC)
					 {
								fprintf(OutFile, " Electricity is running through your veins.\n");
					 }
					 if (p_ptr->muta3 & MUT3_FIRE_BODY)
					 {
								fprintf(OutFile, " Your body is enveloped in flames.\n");
					 }
					if (p_ptr->muta3 & MUT3_SPINES)
					{
						fprintf(OutFile, " Your body is covered with sharp spines.\n");
					}
					 if (p_ptr->muta3 & MUT3_LEATHER_SKIN)
					 {
								fprintf(OutFile, " Your skin is leathery (-1 DEX, +8 AC).\n");
					 }
					 if (p_ptr->muta3 & MUT3_SCALES)
					 {
								fprintf(OutFile, " Your skin is covered in scales (+12 AC).\n");
					 }
					 if (p_ptr->muta3 & MUT3_PLATES)
					 {
								fprintf(OutFile, " You have a covering of armor plates (-2 DEX, +30 AC).\n");
					 }
					 if (p_ptr->muta3 & MUT3_FURRY)
					 {
								fprintf(OutFile, " You are covered in fur (resist cold).\n");
					 }
					 if (p_ptr->muta3 & MUT3_EYE_STALK)
					 {
								fprintf(OutFile, " Your eyes are on stalks (+10 +lvl/5 to hit).\n");
					 }
					 if (p_ptr->muta3 & MUT3_REGEN)
					 {
								fprintf(OutFile, " You are regenerating.\n");
					 }
					 if (p_ptr->muta3 & MUT3_ESP)
					 {
								fprintf(OutFile, " You are telepathic.\n");
					 }
					if (p_ptr->muta3 & MUT3_XTRA_EYELID)
					{
						fprintf(OutFile, " You have an extra eyelid (resist blind).\n");
					}
					if (p_ptr->muta3 & MUT3_LIMBER)
					{
						fprintf(OutFile, " Your body is very limber (+3 DEX).\n");
					}
					if (p_ptr->muta3 & MUT3_ARTHRITIS)
					{
						fprintf(OutFile, " Your joints ache constantly (-3 DEX).\n");
					}
					if (p_ptr->muta3 & MUT3_SLUGGISH)
					{
						fprintf(OutFile, " You react slowly (-3 speed, -10 AC).\n");
					}
					if (p_ptr->muta3 & MUT3_GLOW)
					{
						fprintf(OutFile, " Your body is glowing brightly.\n");
					}
			}

		  if (p_ptr->muta4)
		  {
				if (p_ptr->muta4 & MUT4_SCOR_TAIL)
				{
					fprintf(OutFile, " You have a scorpion tail.\n");
				}
				if (p_ptr->muta4 & MUT4_HORNS)
				{
					fprintf(OutFile, " You have horns.\n");
				}
				if (p_ptr->muta4 & MUT4_BEAK)
				{
					fprintf(OutFile, " You have a beak.\n");
				}
				if (p_ptr->muta4 & MUT4_TUSKS)
				{
					fprintf(OutFile, " You have sharp tusks.\n");
				}
				if (p_ptr->muta4 & MUT4_CLAWS)
				{
					fprintf(OutFile, " You have wicked claws.\n");
				}
				if (p_ptr->muta4 & MUT4_TENTACLES)
				{
					fprintf(OutFile, " You have groping tentacles.\n");
				}
				if (p_ptr->muta4 & MUT4_HUGE_ARMS)
				{
					fprintf(OutFile, " You have huge arms (increase throwing damage).\n");
				}
				if (p_ptr->muta4 & MUT4_SPURS)
				{
					fprintf(OutFile, " You have deadly spurs.\n");
				}
				if (p_ptr->muta4 & MUT4_ANTLERS)
				{
					fprintf(OutFile, " You have antlers.\n");
				}
				if (p_ptr->muta4 & MUT4_HOOVES)
				{
					fprintf(OutFile, " You have hooves.\n");
				}
				if (p_ptr->muta4 & MUT4_IRON_HOOVES)
				{
					fprintf(OutFile, " You have hooves of iron.\n");
				}
				if (p_ptr->muta4 & MUT4_V_FANGS)
				{
					fprintf(OutFile, " You have vampiric fangs.\n");
				}
				if (p_ptr->muta4 & MUT4_POIS_TONGUE)
				{
					fprintf(OutFile, " You have a long, venomous tongue.\n");
				}
				if (p_ptr->muta4 & MUT4_STICKY)
				{
					fprintf(OutFile, " You exude a sticky mucous.\n");
				}
				if (p_ptr->muta4 & MUT4_HAND_MOUTH)
				{
					fprintf(OutFile, " You have round mouths in the palms of your hands.\n");
				}
				if (p_ptr->muta4 & MUT4_WINGS)
				{
					fprintf(OutFile, " You have a pair of wings.\n");
				}
				if (p_ptr->muta4 & MUT4_TRUNK)
				{
					fprintf(OutFile, " You have an elephant-like trunk.\n");
				}
				if (p_ptr->muta4 & MUT4_ICE_TALONS)
				{
					fprintf(OutFile, " Your hands sport large talons of ice.\n");
				}
				if (p_ptr->muta4 & MUT4_ACID_BLOOD)
				{
					fprintf(OutFile, " Your blood is highly acidic.\n");
				}
				if (p_ptr->muta4 & MUT4_THIN_BLOOD)
				{
					fprintf(OutFile, " Your thin blood clots slowly.\n");
				}
				if (p_ptr->muta4 & MUT4_DISTORT_SPACE)
				{
					fprintf(OutFile, " You distort space.\n");
				}
				if (p_ptr->muta4 & MUT4_EAT_ROCK)
				{
					fprintf(OutFile, " You can eat rock.\n");
				}
				if (p_ptr->muta4 & MUT4_BONY_HEAD)
				{
					fprintf(OutFile, " Your skull is unusually thick and hard.\n");
				}
				if (p_ptr->muta4 & MUT4_FEEL_NUMB)
				{
					fprintf(OutFile, " You feel a strange numbness (no pseudo-ID).\n");
				}
				if (p_ptr->muta4 & MUT4_WEEP_BLOOD)
				{
					fprintf(OutFile, " You constantly weep blood. (scary, -15 Search)\n");
				}
				if (p_ptr->muta4 & MUT4_ALBINO)
				{
					fprintf(OutFile, " You are an albino. (-5 Con, +15 spell-casting)\n");
				}
				if (p_ptr->muta4 & MUT4_RAZORS)
				{
					fprintf(OutFile, " Deadly razors stick out of your body.\n");
				}
				if (p_ptr->muta4 & MUT4_TWISTED)
				{
					fprintf(OutFile, " Your body is twisted into an unnatural shape.\n");
				}
				if (p_ptr->muta4 & MUT4_GILLS)
				{
					fprintf(OutFile, " You have gills. (resist water, -1 Chr)\n");
				}
				if (p_ptr->muta4 & MUT4_WARTS)
				{
					fprintf(OutFile, " You have many ugly warts (-3 Chr).\n");
				}
				if (p_ptr->muta4 & MUT4_VULN_ELEM)
				{
					fprintf(OutFile, " You are vulnerable to elemental damage.\n");
				}
				if (p_ptr->muta4 & MUT4_HUGE_EYEBROW)
				{
					fprintf(OutFile, " You have huge eyebrows.\n");
				}
		  }
}


/*
 * List mutations we have...
 */
void do_cmd_knowledge_mutations(void)
{
	FILE *fff;
	char file_name[1024];

	/* Temporary file */
	if (path_temp(file_name, 1024)) return;

	/* Open a new file */
	fff = my_fopen(file_name, "w");

	if (fff) dump_mutations(fff);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Mutations");

	/* Remove the file */
	fd_kill(file_name);
}


/* Here's the racial & mutation activations, moved from cmd2.c */

/* Note: return value indicates that we have succesfully used the power */
bool racial_aux(s16b min_psyche, s16b cost, int difficulty)
{
	if (p_ptr->confused)
	{
		msg_print("You are too confused to use this power.");
		energy_use = 0;
		return FALSE;
	}

	/* Enforce a minimal level of psychic reserve. -- RDH */
	else if (min_psyche > p_ptr->cpsyche)
	{
		msg_print("You lack the strength to use this power.");
		energy_use = 0;
		return FALSE;
	}

	/* Else attempt to do it! */

	if (p_ptr->stun) difficulty += p_ptr -> stun;

	if (difficulty < 5) difficulty = 5;

	/* take time and pay the price */
	energy_use = 100;
	p_ptr->cpsyche -= cost;

/*
* 	p_ptr->redraw |= (PR_PSYCHE);
*  belongs here if I find a place to display it -- RDH
*/

	/* Window stuff - I think this isn't needed with mana/hit pt costs gone -- RDH */
/*	p_ptr->window |= (PW_PLAYER); */
/*	p_ptr->window |= (PW_SPELL); */


	/* Success? - Simplified this quite a bit. -- RDH */
	if (randint(100) > (difficulty))
		return TRUE;

	msg_print("You've failed to concentrate hard enough.");
	return FALSE;
}

static void cmd_racial_power_aux (void)
{
	s16b plev = p_ptr->lev;
	int dir = 0;
	int Type = (randint(3)==1?GF_COLD:GF_FIRE);
	cptr Type_desc = (Type == GF_COLD?"cold":"fire");
	int k;

	switch(p_ptr->prace)
	{
		case RACE_HUMAN:
			if (racial_aux(5, 25, (15 - plev/5)))
			{
				msg_print("You steel yourself for war.");
				(void)set_afraid(0);
				(void)set_hero(p_ptr->hero + 10 + randint(plev));
			}
			break;

		case RACE_ELDREN:
			if (racial_aux(50, 50, (40 - (p_ptr->stat_cur[A_WIS]))  ))
			{
				msg_print("You spend a moment in introspection.");
				(void)self_knowledge();
			}
			break;

		case RACE_DWARF:
			if (racial_aux(5, 20, (12 - plev/5)))
			{
				msg_print("You examine your surroundings.");
				if (p_ptr->lev >= 35) (void)map_area();
				(void)detect_traps();
				(void)detect_stairs();
				(void)detect_doors();
			}
			break;

		case RACE_OAGER_UV:
			if (racial_aux(30, 30, (40 - (p_ptr->stat_cur[A_INT]))))
			{
				msg_print("You sing a little song.");
				(void)detect_objects_magic();
			}
			break;

		case RACE_GNOME:
			if (racial_aux(5, 12, (20 - plev/3)))
			{
				msg_print("Blink!");
				teleport_player(10 + (plev));
			}
			break;

		case RACE_NHADRAGH:
			if (racial_aux(20, 20, 5))
			{
				msg_print("You sense creatures disturbing this plane.");
				detect_monsters_normal();
			}
			break;

		case RACE_HALF_TROLL:
			if (racial_aux(10, 25, 5))
			{
				msg_print("RAAAGH!");
				(void)set_afraid(0);

				(void)set_shero(p_ptr->shero + 10 + randint(plev));
				(void)hp_player(30);
			}
			break;

		case RACE_CATFOLK:
			if (racial_aux(50, 50, (40 - (p_ptr->stat_cur[A_CHR]))))
			{
				if (plev < 50)
				{
					if (!get_aim_dir(&dir)) break;
					msg_print("You act cute.");
					(void)charm_monster(dir, plev);
				}
				else
				{
					msg_print("You act very cute.");
					(void)charm_monsters(plev * 2);
				}
			}
			break;

		case RACE_BARBARIAN:
			if (racial_aux(20, 20, (p_ptr->pclass == CLASS_WARRIOR?6:12)))
			{
				msg_print("Blood and souls!");
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + 10 + randint(plev));
				(void)hp_player(30);
			}
			break;

		case RACE_HALF_GIANT:
			if (racial_aux(20, 40, (35 - (p_ptr->stat_cur[A_STR]))))
			{
				int x,y;
				cave_type *c_ptr;

				if (!get_rep_dir(&dir)) return;
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
						(c_ptr->feat <= FEAT_WALL_SOLID)))

				{
					msg_print("You wave your fists in the air.");
					break;
				}
				msg_print("You smash your fist into the wall!");
				twall(y, x);
			}
			break;

		case RACE_SIDHI:
			if (racial_aux(15, 15, (35 - (p_ptr->stat_cur[A_INT]))))
			{
				msg_print("You examine your foes...");
				probing();
			}
			break;

		case RACE_KLACKON:
			if (racial_aux(10, 10, 0))
			{
				if (!(get_aim_dir(&dir))) break;
				msg_print("You spit acid.");
				if (p_ptr->lev < 25)
					fire_bolt(GF_ACID, dir, plev * 2);
				else
					fire_ball(GF_ACID, dir, plev * 2, 2);
			}
			break;

		case RACE_KOBOLD:
			if (racial_aux(10, 10, (30 - (p_ptr->stat_cur[A_DEX]))))
			{
				if(!get_aim_dir(&dir)) break;
				msg_print("You throw a dart of poison.");
				fire_bolt(GF_POIS, dir, plev * 2);
			}
			break;

		case RACE_DRACONIAN:
		{
			/*
			 * Type of breath is randomly determined, based on
			 * player level - Gumby
			 */
			if (plev < 10)  k = 1;	/* Fire */
			if (plev >= 10) k = 2;	/* Cold */
			if (plev >= 15) k = 3;	/* Acid */
			if (plev >= 20) k = 4;	/* Electricity */
			if (plev >= 25) k = 5;	/* Light */
			if (plev >= 30) k = 6;	/* Dark */
			if (plev >= 35) k = 7;	/* Poison */
			if (plev >= 40) k = 8;	/* Shards */
			if (plev >= 45) k = 9;	/* Chaos */
			if (plev == 50) k = 10;	/* Mana */

			switch (randint(k))
			{
				case 1:  Type = GF_FIRE;   Type_desc = "fire";      break;
				case 2:  Type = GF_COLD;   Type_desc = "frost";     break;
				case 3:  Type = GF_ACID;   Type_desc = "acid";      break;
				case 4:  Type = GF_ELEC;   Type_desc = "lightning"; break;
				case 5:  Type = GF_LITE;   Type_desc = "light";     break;
				case 6:  Type = GF_DARK;   Type_desc = "darkness";  break;
				case 7:  Type = GF_POIS;   Type_desc = "poison";    break;
				case 8:  Type = GF_SHARDS; Type_desc = "shards";    break;
				case 9:  Type = GF_CHAOS;  Type_desc = "chaos";     break;
				case 10: Type = GF_MANA;   Type_desc = "mana";      break;
			}

			if (racial_aux(50, 50, (30 - plev/2)))
			{
				if (!get_aim_dir(&dir)) break;
				msg_format("You breathe %s.", Type_desc);
				fire_ball(Type, dir, (plev*3), (plev/15)+1);
			}
			break;
		}

		case RACE_MIND_FLAYER:
			if (racial_aux(15, 15, (60 - (p_ptr->stat_cur[A_WIS]) - (p_ptr->stat_cur[A_WIS]))))
			{
				if (!get_aim_dir(&dir)) break;
				else
				{
					msg_print("You concentrate and your tentacles wiggle...");
					fire_ball(GF_PSI, dir, plev * 2, 0);
				}
			}
			break;

		case RACE_GOLEM:
			if (racial_aux(10, 35, 15 - plev/5))
			{
				msg_print("You feel hard.");
				(void)set_shield(p_ptr->shield + randint(plev) + 30);
			}
			break;

		case RACE_VAMPIRE:
		/* Old vampire activation was drain life. Now a wonderful melee attack. -- RDH */
			if (racial_aux(40, 40, (40 - (p_ptr->stat_cur[A_CHR]))))
			{
				if (summon_specific_friendly(py, px, (plev*3)/2, SUMMON_BAT, (plev >= 20 ? TRUE : FALSE)))
				msg_print("The children of the night answer your call.");
			}
			break;


		case RACE_HALFLING:
			if (racial_aux(50, 50, 0))
			{
				msg_print("You rearrange your molecules elsewhere!");
				(void) teleport_player(10 + plev);
			}
			break;

		case RACE_YEEK:
			if (racial_aux(25, 25, 5))
			{
				msg_print("YEEK!");
				set_afraid(randint(10)+5);
				set_fast(randint(10)+5);
			}
			break;

		case RACE_MELNIBONEAN:
			if (racial_aux(80, 80, (40 - (p_ptr->stat_cur[A_INT]))))
			{
				if (randint(8)==1)
				{
					if (summon_specific(py, px, (plev*3)/2, SUMMON_DEMON))
					{
						msg_print("The area fills with a stench of sulphur and brimstone.");
						msg_print("'I will not be commanded! Prepare to die!'");
					}
					}
				else
				{
					if (summon_specific_friendly(py, px, (plev*3)/2, SUMMON_DEMON, (plev >= 40 ? TRUE : FALSE)))
					{
						msg_print("The area fills with a stench of sulphur and brimstone.");
						msg_print("'Serve you I shall, for our ancient alliance.'");
					}
				}
			}
			break;

		case RACE_VADHAGH:
			if (racial_aux(50, 50, 50-plev))
			{
				msg_print("You shift into an adjacent plane...");
							 if (autosave_l)
						  {
					is_autosave = TRUE;
					msg_print("Autosaving the game...");
					do_cmd_save_game();
					is_autosave = FALSE;
				}
				new_level_flag = TRUE;
			}
			break;

		default:
			msg_print("This race has no bonus power.");
			energy_use = 0;
			break;
	}

	p_ptr->redraw |= (PR_HP | PR_MANA);
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}



/*
 * Allow user to choose a power (racial / mutation) to activate
 */
void do_cmd_racial_power(void)
{
	int		i = 0;
	int		Power = -1;
	int		num = 0, dir = 0;
	u32b		powers[36];
	char		power_desc[36][80];
	int      power_cost[36], power_min[36];
	bool		flag, redraw;
	int		ask;
	char		choice;
	char		out_val[160];
	int		lvl = p_ptr->lev;
	bool		warrior = ((p_ptr->pclass == CLASS_WARRIOR)?TRUE:FALSE);
	bool		has_racial = FALSE;
	cptr		racial_power = "(none)";
	int		racial_cost = 0, racial_min = 0;

	for (num = 0; num < 36; num++)
	{
		powers[num] = 0;
		strcpy (power_desc[num], "");
	}

	num = 0;

	if (p_ptr->confused)
	{
		msg_print("You are too confused to use any powers!");
		energy_use = 0;
		return;
	}

	switch (p_ptr->prace)
	{
		case RACE_HUMAN:
			racial_power = "heroism     (racial)";
			has_racial = TRUE;
			racial_cost = 25;
			racial_min = 5;
			break;

		case RACE_ELDREN:
			racial_power = "self knowledge     (racial)";
			has_racial = TRUE;
			racial_cost = 50;
			racial_min = 50;
			break;

		case RACE_DWARF:
			racial_power = "sense surroundings (racial)";
			has_racial = TRUE;
			racial_cost = 20;
			racial_min = 5;
			break;

		case RACE_OAGER_UV:
			racial_power = "detect magic items        (racial)";
			has_racial = TRUE;
			racial_cost = 30;
			racial_min = 30;
			break;

		case RACE_GNOME:
			racial_power = "teleport           (racial)";
			has_racial = TRUE;
			racial_cost = 12;
			racial_min = 5;
			break;

		case RACE_NHADRAGH:
			racial_power = "detect monsters       (racial)";
			has_racial = TRUE;
			racial_cost = 20;
			racial_min = 20;
			break;

		case RACE_HALF_TROLL:
			racial_power = "berserk            (racial)";
			has_racial = TRUE;
			racial_cost = 25;
			racial_min = 10;
			break;

		case RACE_BARBARIAN:
			racial_power = "berserk            (racial)";
			has_racial = TRUE;
			racial_cost = 20;
			racial_min = 20;
			break;

		case RACE_CATFOLK:
			racial_power = "charm              (racial)";
			has_racial = TRUE;
			racial_cost = 50;
			racial_min = 50;
			break;

		case RACE_HALF_GIANT:
			racial_power = "smash down a wall  (racial)";
			has_racial = TRUE;
			racial_cost = 40;
			racial_min = 20;
			break;

		case RACE_SIDHI:
			racial_power = "probing            (racial)";
			has_racial = TRUE;
			racial_cost = 15;
			racial_min = 15;
			break;

		case RACE_HALFLING:
			racial_power = "teleport      (racial)";
			has_racial = TRUE;
			racial_cost = 50;
			racial_min = 50;
			break;

		case RACE_KLACKON:
			racial_power = "spit acid          (racial)";
			has_racial = TRUE;
			racial_cost = 10;
			racial_min = 10;
			break;

		case RACE_KOBOLD:
			racial_power = "poison dart        (racial)";
			has_racial = TRUE;
			racial_cost = 10;
			racial_min = 10;
			break;

		case RACE_DRACONIAN:
			racial_power = "breath weapon      (racial)";
			has_racial = TRUE;
			racial_cost = 50;
			racial_min = 50;
			break;

		case RACE_MIND_FLAYER:
			racial_power = "mind blast         (racial)";
			has_racial = TRUE;
			racial_cost = 15;
			racial_min = 15;
			break;

		case RACE_GOLEM:
			racial_power = "stone skin         (racial)";
			has_racial = TRUE;
			racial_cost = 35;
			racial_min = 10;
			break;

		case RACE_VAMPIRE:
			racial_power = "summon bats         (racial)";
			has_racial = TRUE;
			racial_cost = 40;
			racial_min = 40;
			break;

		case RACE_YEEK:
			racial_power = "flee in terror!    (racial)";
			has_racial = TRUE;
			racial_cost = 25;
			racial_min = 25;
			break;

		case RACE_MELNIBONEAN:
			racial_power = "summon a demon     (racial)";
			has_racial = TRUE;
			racial_cost = 80;
			racial_min = 80;
			break;

		case RACE_VADHAGH:
			racial_power = "walk the planes    (racial)";
			has_racial = TRUE;
			racial_cost = 50;
			racial_min = 50;
			break;
	}

	if (!(has_racial) && !(p_ptr->muta1))
	{
		msg_print("You have no powers to activate.");
		energy_use = 0;
		return;
	}

	if (has_racial)
	{
		powers[0] = -1;
		strcpy(power_desc[0], racial_power);
		power_cost[0] = racial_cost;
		power_min[0] = racial_min;
		num++;
	}

	if (p_ptr->muta1)
	{
		int lvl = p_ptr->lev;

		if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
			strcpy(power_desc[num],"spit acid (mutation)");
			power_cost[num] = 15;
			power_min[num] = 15;
			powers[num++] = MUT1_SPIT_ACID;
		}

		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
			strcpy(power_desc[num],"fire breath (mutation)");
			power_cost[num] = 50 - lvl/2;
			power_min[num] = 25;
			powers[num++] = MUT1_BR_FIRE;
		}

		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
			strcpy(power_desc[num],"hypnotic gaze (mutation)");
			power_cost[num] = 20;
			power_min[num] = 20;
			powers[num++] = MUT1_HYPN_GAZE;
		}

		if (p_ptr->muta1 & MUT1_APPORTATION)
		{
			strcpy(power_desc[num],"apportation (mutation)");
			power_cost[num] = 10;
			power_min[num] = 10;
			powers[num++] = MUT1_APPORTATION;
		}

		if (p_ptr->muta1 & MUT1_VTELEPORT)
		{
			strcpy(power_desc[num],"teleport (mutation)");
			power_cost[num] = 25;
			power_min[num] = 10;
			powers[num++] = MUT1_VTELEPORT;
		}

		if (p_ptr->muta1 & MUT1_MIND_BLST)
		{
			strcpy(power_desc[num],"mind blast (mutation)");
			power_cost[num] = 50 - lvl/2;
			power_min[num] = 20;
			powers[num++] = MUT1_MIND_BLST;
		}

		if (p_ptr->muta1 & MUT1_RADIATION)
		{
			strcpy(power_desc[num],"emit radiation (mutation)");
			power_cost[num] = 35;
			power_min[num] = 35;
			powers[num++] = MUT1_RADIATION;
		}

		if (p_ptr->muta1 & MUT1_MUSHROOMS)
		{
			strcpy(power_desc[num],"eat some fungus (mutation)");
			power_cost[num] = 12;
			power_min[num] = 12;
			powers[num++] = MUT1_MUSHROOMS;
		}

		if (p_ptr->muta1 & MUT1_SUMMON_M)
		{
			strcpy(power_desc[num],"summon monsters (mutation)");
			power_cost[num] = 40;
			power_min[num] = 40;
			powers[num++] = MUT1_SUMMON_M;
		}

		if (p_ptr->muta1 & MUT1_SUMMON_EL)
		{
			strcpy(power_desc[num],"summon elementals (mutation)");
			power_cost[num] = 50 - lvl/2;
			power_min[num] = 50 - lvl/2;
			powers[num++] = MUT1_SUMMON_EL;
		}

		if (p_ptr->muta1 & MUT1_BR_COLD)
		{
			strcpy(power_desc[num],"breath cold (mutation)");
			power_cost[num] = 50 - lvl/2;
			power_min[num] = 25;
			powers[num++] = MUT1_BR_COLD;
		}

		if (p_ptr->muta1 & MUT1_NETHER_BALL)
		{
			strcpy(power_desc[num],"nether ball (mutation)");
			power_cost[num] = 30;
			power_min[num] = 30;
			powers[num++] = MUT1_NETHER_BALL;
		}

		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
			strcpy(power_desc[num],"shriek (mutation)");
			power_cost[num] = 50 - lvl/2;
			power_min[num] = 25;
			powers[num++] = MUT1_SHRIEK;
		}

		if (p_ptr->muta1 & MUT1_ILLUMINE)
		{
			strcpy(power_desc[num],"illuminate (mutation)");
			power_cost[num] = 5;
			power_min[num] = 5;
			powers[num++] = MUT1_ILLUMINE;
		}

		if (p_ptr->muta1 & MUT1_DET_CURSE)
		{
			strcpy(power_desc[num],"detect curses (mutation)");
			power_cost[num] = 35;
			power_min[num] = 35;
			powers[num++] = MUT1_DET_CURSE;
		}

		if (p_ptr->muta1 & MUT1_BERSERK)
		{
			strcpy(power_desc[num],"berserk (mutation)");
			power_cost[num] = 20;
			power_min[num] = 20;
			powers[num++] = MUT1_BERSERK;
		}

		if (p_ptr->muta1 & MUT1_ICE_SHOWER)
		{
			strcpy(power_desc[num],"call ice storms (mutation)");
			power_cost[num] = 100 - lvl;
			power_min[num] = 100 - lvl;
			powers[num++] = MUT1_ICE_SHOWER;
		}

		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
			strcpy(power_desc[num],"Midas touch (mutation)");
			power_cost[num] = 70 - lvl;
			power_min[num] = 70 - lvl;
			powers[num++] = MUT1_MIDAS_TCH;
		}

		if (p_ptr->muta1 & MUT1_SUMMON_ANT)
		{
			strcpy(power_desc[num],"summon ants (mutation)");
			power_cost[num] = 40 - lvl/2;
			power_min[num] = 40 - lvl/2;
			powers[num++] = MUT1_SUMMON_ANT;
		}

		if (p_ptr->muta1 & MUT1_RESIST)
		{
			strcpy(power_desc[num],"resist elements (mutation)");
			power_cost[num] = 30;
			power_min[num] = 30;
			powers[num++] = MUT1_RESIST;
		}

		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
			strcpy(power_desc[num],"earthquake (mutation)");
			power_cost[num] = 250;
			power_min[num] = 50;
			powers[num++] = MUT1_EARTHQUAKE;
		}

		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
			strcpy(power_desc[num],"dazzle (mutation)");
			power_cost[num] = 200 - 2*lvl;
			power_min[num] = 100 - lvl;
			powers[num++] = MUT1_DAZZLE;
		}

		if (p_ptr->muta1 & MUT1_RECALL)
		{
			strcpy(power_desc[num],"word of recall (mutation)");
			power_cost[num] = 120;
			power_min[num] = 50;
			powers[num++] = MUT1_RECALL;
		}

		if (p_ptr->muta1 & MUT1_BANISH)
		{
			strcpy(power_desc[num],"banish evil (mutation)");
			power_cost[num] = 100;
			power_min[num] = 25;
			powers[num++] = MUT1_BANISH;
		}

		if (p_ptr->muta1 & MUT1_LIONS_ROAR)
		{
			strcpy(power_desc[num],"roar (mutation)");
			power_cost[num] = 70 - lvl;
			power_min[num] = 20;
			powers[num++] = MUT1_LIONS_ROAR;
		}

		if (p_ptr->muta1 & MUT1_MISSILE)
		{
			strcpy(power_desc[num],"magic missile (mutation)");
			power_cost[num] = 10;
			power_min[num] = 10;
			powers[num++] = MUT1_MISSILE;
		}

		if (p_ptr->muta1 & MUT1_SHARD_BOLT)
		{
			strcpy(power_desc[num],"shard bolt (mutation)");
			power_cost[num] = 18;
			power_min[num] = 18;
			powers[num++] = MUT1_SHARD_BOLT;
		}

		if (p_ptr->muta1 & MUT1_INERTIA_BALL)
		{
			strcpy(power_desc[num],"inertia ball (mutation)");
			power_cost[num] = 36;
			power_min[num] = 36;
			powers[num++] = MUT1_INERTIA_BALL;
		}

		if (p_ptr->muta1 & MUT1_BR_WATER)
		{
			strcpy(power_desc[num],"water breath (mutation)");
			power_cost[num] = 50 - lvl/2;
			power_min[num] = 25;
			powers[num++] = MUT1_BR_WATER;
		}

		if (p_ptr->muta1 & MUT1_CHAIN_SHARDS)
		{
			strcpy(power_desc[num],"rapid shards (mutation)");
			power_cost[num] = 22;
			power_min[num] = 22;
			powers[num++] = MUT1_CHAIN_SHARDS;
		}

		if (p_ptr->muta1 & MUT1_LIGHTNING)
		{
			strcpy(power_desc[num],"lightning (mutation)");
			power_cost[num] = 45 - lvl/2;
			power_min[num] = 20;
			powers[num++] = MUT1_LIGHTNING;
		}

		if (p_ptr->muta1 & MUT1_GRAV_BEAM)
		{
			strcpy(power_desc[num],"gravity beam (mutation)");
			power_cost[num] = 50 - lvl/2;
			power_min[num] = 25;
			powers[num++] = MUT1_GRAV_BEAM;
		}
	}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit, psyche=%d) Use which power? ",
		 I2A(0), I2A(num - 1), p_ptr->cpsyche);

#ifdef ALLOW_REPEAT

	/* Get the power, if available */
	if (repeat_pull(&choice))
	{
		/* Verify the spell */
		if (choice < num)
		{
			/* Success */
			return /* (TRUE) -- function returns (void) -- this is part of what needs fixing -- RDH */;
		}
	}

#endif /* ALLOW_REPEAT */

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 13;
				int ctr = 0;
				char dummy[80];

				strcpy (dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num)
				{
					sprintf(dummy, "%c) cost:%d min:%d %s", I2A(ctr), power_cost[ctr], power_min[ctr], power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}

				prt ("", y + ctr, x);
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		/* Note verify */
		ask = (isupper(choice));

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
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag)
	{
		energy_use = 0;
		return;
	}

#ifdef ALLOW_REPEAT

	repeat_push(choice);

#endif /* ALLOW_REPEAT */

	if (powers[i] == -1)
	{
		cmd_racial_power_aux();
	}
	else
	{
		energy_use = 100;

		switch (powers[i])
		{
			case MUT1_SPIT_ACID:
				if (racial_aux(15, 15, 25 - lvl/3))
				{
					msg_print("You spit acid...");
					if (get_aim_dir(&dir))
						fire_ball(GF_ACID, dir, p_ptr->lev * 2, 1 + (p_ptr->lev/30));
				}
				break;

			case MUT1_BR_FIRE:
				if (racial_aux(25, (50 - (p_ptr->lev)/2), (40 - p_ptr->stat_cur[A_CON])))
				{
					msg_print("You breathe fire...");
					if (get_aim_dir(&dir))
						fire_ball(GF_FIRE, dir, p_ptr->lev * 3, 1 + (p_ptr->lev/20));
				}
				break;

			case MUT1_HYPN_GAZE:
				if (racial_aux(20, 20, (40 - p_ptr->stat_cur[A_CHR])))
				{
					msg_print("Your eyes look mesmerizing...");
					if (get_aim_dir(&dir))
						(void)charm_monster(dir, p_ptr->lev);
				}
				break;

			case MUT1_APPORTATION:
				if (racial_aux(10, 10, (40 - p_ptr->stat_cur[A_WIS])))
				{
					msg_print("You concentrate...");
					if (get_aim_dir(&dir))
						fetch(dir, p_ptr->lev * 10, FALSE);
				}
				break;

			case MUT1_VTELEPORT:
				if (racial_aux(10, 25, (40 - p_ptr->stat_cur[A_WIS])))
				{
					msg_print("Bamf!");
					teleport_player(10 + (p_ptr->lev));
				}
				break;

			case MUT1_MIND_BLST:
				if (racial_aux(20, (50 - lvl/2), (40 - p_ptr->stat_cur[A_WIS])))
				{
					msg_print("You think deadly thoughts...");
					if (!get_aim_dir(&dir)) return;
					fire_bolt(GF_PSI, dir, damroll(3 + ((p_ptr->lev - 1) / 3), 4));
				}
				break;

			case MUT1_RADIATION:
				if (racial_aux(35, 35, 5))
				{
					msg_print("Radiation flows from your body!");
					fire_ball(GF_NUKE, 0, (p_ptr->lev * 3), 3 + (p_ptr->lev / 20));
				}
				break;

			case MUT1_MUSHROOMS:
				if (racial_aux(12, 12, 0))
				{
						if (p_ptr->prace == RACE_VAMPIRE)
						{
							/* Reduced nutritional benefit */
							(void)set_food(p_ptr->food + 10);
							msg_print("Tasty, but not fulfilling.");
							if (p_ptr->food < PY_FOOD_ALERT)   /* Hungry */
								msg_print("Your hunger can only be satisfied with fresh blood!");
						}
						else if (p_ptr->prace == RACE_GOLEM)
						{
							msg_print("The food of mortals is poor sustenance for you.");
							(void)set_food(p_ptr->food + 20);
						}
						else
						{
							msg_print("These mushrooms are really quite tasty.");
							(void)set_food(p_ptr->food + 500);
						}
				}
				break;

			case MUT1_SUMMON_M:
				if (racial_aux(40, 40, (30 - lvl/2)))
				{
					summon_specific_friendly(py, px, lvl, SUMMON_NO_UNIQUES, TRUE);
				}
				break;

			case MUT1_SUMMON_EL:
				if (racial_aux((50 - lvl/2), (50 - lvl/2), (40 - p_ptr->stat_cur[A_WIS])))
				{
					summon_specific_friendly(py, px, lvl, SUMMON_ELEMENTAL, TRUE);
				}
				break;

			case MUT1_BR_COLD:
				if (racial_aux(25, (50 - (p_ptr->lev)/2), (40 - p_ptr->stat_cur[A_CON])))
				{
					msg_print("You breathe cold...");
					if (get_aim_dir(&dir))
						fire_ball(GF_COLD, dir, p_ptr->lev * 3, 1 + (p_ptr->lev/20));
				}
				break;

			case MUT1_NETHER_BALL:
				if (racial_aux(30, 30, 0))
				{
					msg_print("You call on death...");
					if (get_aim_dir(&dir))
						fire_ball(GF_NETHER, dir, p_ptr->lev * 8, 1 + (p_ptr->lev/10));
					msg_print("...and pay the price.");
					p_ptr->exp = (p_ptr->exp)*39/40;
					/* Need to redraw experience after draining it. - RDH */
					p_ptr->redraw |= (PR_EXP);
				}
				break;

			case MUT1_SHRIEK:
				if (racial_aux(25, (50 - (p_ptr->lev)/2), (40 - p_ptr->stat_cur[A_CON])))
				{
					msg_print("You scream...");
					(void)fire_ball(GF_SOUND, 0, 3 * lvl, 8);
					(void)aggravate_monsters(0, FALSE);
				}
				break;

			case MUT1_ILLUMINE:
				if (racial_aux(5, 5, (10 - lvl/10)))
				{
					msg_print("You shine...");
					(void)lite_area(damroll(2, (lvl / 2)), (lvl / 10) + 1);
				}
				break;

			case MUT1_DET_CURSE:
				if (racial_aux(35, 35, 10))
				{
					int i;

					for (i = 0; i < INVEN_TOTAL; i++)
					{
						object_type *o_ptr = &inventory[i];

						if (!o_ptr->k_idx) continue;
						if (!cursed_p(o_ptr)) continue;

						o_ptr->note = quark_add("cursed");
					}
				}
				break;

			case MUT1_BERSERK:
				if (racial_aux(20, 20, (10 - p_ptr->stat_cur[A_STR] + p_ptr->stat_cur[A_INT])))
				{
					msg_print("Blood and glory!");
					(void)set_shero(p_ptr->shero + randint(25) + 25);
					(void)hp_player(30);
					(void)set_afraid(0);
				}
				break;

			case MUT1_ICE_SHOWER:
				if (racial_aux((100 - lvl), (100 - lvl), (50 - lvl)))
				{
					msg_print("You call down icy retribution...");
					if (get_aim_dir(&dir))
						fire_shower(GF_ICE, dir, p_ptr->lev * 3, 1 + (p_ptr->lev/20), 3 + (p_ptr->lev/10));
				}
				break;

			case MUT1_MIDAS_TCH:
				if (racial_aux((70 - lvl), (70 - lvl), 0))
				{
					msg_print("Gold fever...");
					(void)alchemy();
				}
				break;

			case MUT1_SUMMON_ANT:
				if (racial_aux((40 - lvl/2), (40 - lvl/2), (36 - p_ptr->stat_cur[A_CHR])))
				{
					int i;
					for (i = 0; i < 8; i++)
					{
						summon_specific_friendly(py, px, lvl, SUMMON_ANT, TRUE);
					}
				}
				break;

			case MUT1_RESIST:
				if (racial_aux(30, 30, (38 - p_ptr->stat_cur[A_CON])))
				{
					msg_print("You focus on defense...");
					int num = lvl / 10;
					int dur = randint(20) + 20;

					if (rand_int(5) < num)
					{
						(void)set_oppose_acid(p_ptr->oppose_acid + dur);
						num--;
					}
					if (rand_int(4) < num)
					{
						(void)set_oppose_elec(p_ptr->oppose_elec + dur);
						num--;
					}
					if (rand_int(3) < num)
					{
						(void)set_oppose_fire(p_ptr->oppose_fire + dur);
						num--;
					}
					if (rand_int(2) < num)
					{
						(void)set_oppose_cold(p_ptr->oppose_cold + dur);
						num--;
					}
					if (num)
					{
						(void)set_oppose_pois(p_ptr->oppose_pois + dur);
						num--;
					}
					if (1)
					{
						(void)set_shield(p_ptr->shield + dur);
					}
				}
				break;

			case MUT1_EARTHQUAKE:
				if (racial_aux(50, 250, (40 - p_ptr->stat_cur[A_STR])))
				{
					msg_print("You bring down the house!");
					earthquake(py, px, 10);
				}
				break;

			case MUT1_DAZZLE:
				if (racial_aux((100 - lvl), (200 - 2*lvl), (35 - p_ptr->stat_cur[A_CHR])))
				{
					msg_print("You feel dazzling!");
					stun_monsters(lvl * 4);
					confuse_monsters(lvl * 4);
					turn_monsters(lvl * 4);
				}
				break;

			case MUT1_RECALL:
				if (racial_aux(50, 120, (40 - p_ptr->stat_cur[A_INT])))
				{
					if (!dun_level)
					{
						msg_print("You concentrate on the depths...");
					}
					else
					{
						msg_print("You concentrate on home...");
					}
					(void) word_of_recall();
				}
				break;

			case MUT1_BANISH:
				if (racial_aux(25, 100, (40 - p_ptr->stat_cur[A_WIS])))
				{
					int x,y;
					cave_type *c_ptr;
					monster_type *m_ptr;
					monster_race *r_ptr;

					if (!get_rep_dir(&dir)) return;
					y = py + ddy[dir];
					x = px + ddx[dir];
					c_ptr = &cave[y][x];

					if (!c_ptr->m_idx)
					{
						msg_print("You sense no evil there!");
						break;
					}

					m_ptr = &m_list[c_ptr->m_idx];
					r_ptr = &r_info[m_ptr->r_idx];

					if ((r_ptr->flags3 & RF3_EVIL) &&
						 !(r_ptr->flags1 & RF1_QUESTOR) &&
						 !(r_ptr->flags1 & RF1_UNIQUE))
					{
						/* Delete the monster, rather than killing it. */
						delete_monster_idx(c_ptr->m_idx);
						msg_print("The evil creature vanishes in a puff of sulfurous smoke!");
					}
					else
					{
						msg_print("Your invocation is ineffectual!");
					}
				}
				break;

			case MUT1_LIONS_ROAR:
				if (racial_aux(20, (70 - lvl), (30 - p_ptr->stat_cur[A_CON])))
				{
					msg_print("You let out a terrifying roar.");
					(void)scare_monsters();
					break;
				}
				break;

			case MUT1_MISSILE:
				if (racial_aux(10, 10, (10 - lvl/5)))
				{
					msg_print("You cast a magic missile...");
					if (get_aim_dir(&dir))
						fire_bolt(GF_MISSILE, dir, damroll(3 + ((lvl - 1) / 3), 4));
				}
				break;

			case MUT1_SHARD_BOLT:
				if (racial_aux(18, 18, (40 - p_ptr->stat_cur[A_CON])))
				{
					msg_print("You cast a stinging shard...");
					if (get_aim_dir(&dir))
						fire_bolt(GF_SHARDS, dir, damroll(3 + (lvl / 5), 5));
				}
				break;

			case MUT1_INERTIA_BALL:
				if (racial_aux(36, 36, (40 - p_ptr->stat_cur[A_DEX])))
				{
					msg_print("You conjure up inertial forces...");
					if (get_aim_dir(&dir))
						fire_ball(GF_INERTIA, dir, p_ptr->lev * 3, 2 + (p_ptr->lev/20));
				}
				break;

			case MUT1_BR_WATER:
				if (racial_aux(25, (50 - (p_ptr->lev)/2), (40 - p_ptr->stat_cur[A_CON])))
				{
					msg_print("You breathe out water...");
					if (get_aim_dir(&dir))
						fire_ball(GF_SHARDS, dir, p_ptr->lev * 3, 1 + (p_ptr->lev/20));
				}
				break;

			case MUT1_CHAIN_SHARDS:
				if (racial_aux(22, 22, (40 - p_ptr->stat_cur[A_CON])))
				{
					msg_print("You launch a barrage of shards...");
					if (get_aim_dir(&dir))
						fire_blast(GF_SHARDS, dir, 3 + (lvl / 5), 5, 10, 2);
				}
				break;

			case MUT1_LIGHTNING:
				if (racial_aux(20, (45 - (p_ptr->lev)/2), (40 - p_ptr->stat_cur[A_INT])))
				{
					msg_print("Lightning leaps from your fingertips...");
					if (get_aim_dir(&dir))
						fire_beam(GF_ELEC, dir, lvl * 3);
				}
				break;

			case MUT1_GRAV_BEAM:
				if (racial_aux(25, (50 - (p_ptr->lev)/2), (40 - p_ptr->stat_cur[A_CON])))
				{
					msg_print("You focus gravity...");
					if (get_aim_dir(&dir))
						fire_beam(GF_GRAVITY, dir, damroll(10 + (lvl / 5), 8));
				}
				break;

			default:
				energy_use = 0;
				msg_format("Power %s not implemented. Oops.", powers[i]);
		}
	}

	/* Success */
	return;
}


/* Process randomly activating mutations, called from dungeon.c. -- Gumby */
void process_mutations(void)
{
	if ((p_ptr->muta2 & MUT2_BERS_RAGE) && !rand_int(2800))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("RAAAAGHH!");
		msg_print("You feel a fit of rage coming over you!");
		(void) set_shero(p_ptr->shero + 10 + randint(p_ptr->lev));
	}

	if ((p_ptr->muta2 & MUT2_COWARDICE) && !p_ptr->resist_fear &&
		 !p_ptr->hero && !p_ptr->shero && !rand_int(2800))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("It's so dark... so scary!");
		p_ptr->redraw |= PR_AFRAID;
		p_ptr->afraid = (p_ptr->afraid) + 13 + randint(26);
	}

	if ((p_ptr->muta2 & MUT2_RTELEPORT) && !rand_int(4000) &&
		 !p_ptr->anti_tele && !(p_ptr->muta1 & MUT1_VTELEPORT))
	{
		disturb(0,0);
		msg_print("Your position suddenly seems very uncertain...");
		msg_print(NULL);
		teleport_player(40);
	}

	if ((p_ptr->muta2 & MUT2_ALCOHOL) && !p_ptr->resist_conf &&
		 !rand_int(5000))
	{
		disturb(0,0);
		p_ptr->redraw |= PR_EXTRA;
		msg_print("You stagger, suddenly intoxsh... intuxi... drunk!");

		if (randint(20)==1)
		{
			msg_print(NULL);
			if (randint(3)==1) lose_all_info();
			else wiz_dark();
			teleport_player(100);
			msg_print("You wake up somewhere with a sore head...");
			msg_print("You can't remember a thing, or how you got here!");
		}
		else
		{
			(void)set_confused(p_ptr->confused + randint(20) + 15);

			if ((randint(3)==1) && !p_ptr->resist_chaos)
			{
				msg_print("Your blurry vision misleads you.");
				(void)set_image(p_ptr->image + randint(150) + 150);
			}
		}
	}

	if ((p_ptr->muta2 & MUT2_HALLU) && !p_ptr->resist_chaos &&
		 !rand_int(5000))
	{
		if (disturb_minor) disturb(0,0);
		p_ptr->redraw |= PR_EXTRA;
		msg_print("Is this Chaos?");
		(void)set_image(p_ptr->image + rand_int(50) + 20);
	}

	if ((p_ptr->muta2 & MUT2_FLATULENT) && !rand_int(2750))
	{
		if (disturb_minor) disturb(0,0);

		msg_print("BRRAAAP! Oops.");
		msg_print(NULL);
		fire_ball(GF_POIS, 0, 10 + 2*(p_ptr->lev), 4);
	}

	if ((p_ptr->muta2 & MUT2_MUSK) && !rand_int(2000))
	{
		if (disturb_minor) disturb(0,0);

		msg_print("Your musk fills the air.");
		msg_print(NULL);
		fire_ball(GF_OLD_CONF, 0, 5 + (p_ptr->lev), 3 + (p_ptr->lev)/15);
		{
			int i;

			/* Aggravate everyone nearby */
			for (i = 1; i < m_max; i++)
			{
				monster_type    *m_ptr = &m_list[i];
				monster_race    *r_ptr = &r_info[m_ptr->r_idx];

				/* Paranoia -- Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				/* Skip player */
				if (i == 1) continue;

				/* Aggravate *all* animals, who hate the musk - RDH */
				if (r_ptr->flags3 & RF3_ANIMAL)
				{
					/* Wake up */
					if (m_ptr->csleep)
					{
					/* Wake up */
					m_ptr->csleep = 0;
					}

					/* Get mad. */
					if (m_ptr->mspeed < r_ptr->speed + 10)
					{
						m_ptr->mspeed = r_ptr->speed + 10;
					}

					if (randint(2)==1)
					{
						m_ptr->smart &= ~SM_FRIEND;
					}
					else
					{
						p_ptr->pet_follow_distance = 255;
					}
				}
			}
		}
	}

	if ((p_ptr->muta2 & MUT2_ABSENT_MIND) && !rand_int(4000))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("You suddenly can't remember where you going.");
		wiz_dark();
	}

	if ((p_ptr->muta2 & MUT2_BORING) && !rand_int(1600))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("You decide to give an impromptu lecture on cheeses.");
		(void)sleep_monsters();
	}

	if ((p_ptr->muta2 & MUT2_GREEN_RAD) && !rand_int(3000))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("You erupt in green radiation.");
		p_ptr->tim_sterile += (100 + 5*(p_ptr->lev));
	}

	if ((p_ptr->muta2 & MUT2_ATT_UNDEAD) && !p_ptr->anti_magic &&
		 !rand_int(6500))
	{
		bool d_summon = FALSE;

		if (!rand_int(5))
		{
			d_summon = summon_specific_friendly(py, px, dun_level, SUMMON_UNDEAD, TRUE);
		}
		else
		{
			d_summon = summon_specific(py, px, dun_level, SUMMON_UNDEAD);
		}

		if (d_summon)
		{
			msg_print("You have attracted an undead horror!");
			disturb(0,0);
		}
	}

	if ((p_ptr->muta2 & MUT2_ATT_DEMON) && !p_ptr->anti_magic &&
		 (randint(6666)==666))
	{
		bool d_summon = FALSE;

		if (!rand_int(5))
		{
			d_summon = summon_specific_friendly(py, px, dun_level, SUMMON_DEMON, TRUE);
		}
		else
		{
			d_summon = summon_specific(py, px, dun_level, SUMMON_DEMON);
		}

		if (d_summon)
		{
			msg_print("You have attracted a demon!");
			disturb(0,0);
		}
	}

	if ((p_ptr->muta2 & MUT2_PROD_MANA) && !p_ptr->anti_magic &&
		 !rand_int(8000))
	{
		int dire = 0;
		disturb(0,0);
		msg_print("Magical energy flows through you! You must release it!");
		flush();
		msg_print(NULL);
		(void)get_hack_dir(&dire);
		fire_ball(GF_MANA, dire, p_ptr->lev * 2, 3);
	}

	if (p_ptr->muta2 & MUT2_WOUND && !p_ptr->resist_shard &&
		 !rand_int(3000))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("Your skin rips open!  Ouch!");
		set_cut(p_ptr->cut + rand_int(20) + 10);
	}

	if (p_ptr->muta2 & MUT2_DISPEL_ALL && !rand_int(8000))
	{
		if (disturb_minor) disturb(0, 0);
		msg_print("You feel a dark power take hold of you.");
		dispel_monsters(150);
		set_stun(p_ptr->stun + randint(10) + 10);

		if (!dun_level)
		{
			msg_print("You see one of the shopkeepers running for the hills!");
			store_shuffle(rand_int(MAX_STORES));
		}
	}

	if ((p_ptr->muta2 & MUT2_EAT_LIGHT) && !rand_int(3000))
	{
		object_type *o_ptr;

		msg_print("A shadow passes over you.");

		/* Absorb light from the current position */
		if (cave[py][px].info & CAVE_GLOW) hp_player(10);

		o_ptr = &inventory[INVEN_LITE];

		/* Absorb some fuel in the current lite */
		if (o_ptr->tval == TV_LITE)
		{
			/* Use some fuel (except on artifacts) */
			if (!artifact_p(o_ptr) && (o_ptr->pval > 0) &&
				 !(o_ptr->sval == SV_LITE_FEANORAN_LAMP))
			{
				/* Heal the player a bit */
				hp_player(o_ptr->pval / 20);

				/* Decrease life-span of lite */
				o_ptr->pval /= 2;

				msg_print("You absorb energy from your light!");
			}
		}

		/*
		 * Unlite the area (radius 10) around player and
		 * do 50 points damage to every affected monster
		 */
		unlite_area(50, 10);
	}

	if ((p_ptr->muta2 & MUT2_ATT_ANIMAL) && !p_ptr->anti_magic &&
		 !rand_int(5000))
	{
		bool d_summon = FALSE;

		if (!rand_int(5))
		{
			d_summon = summon_specific_friendly(py, px, dun_level, SUMMON_ANIMAL, TRUE);
		}
		else
		{
			d_summon = summon_specific(py, px, dun_level, SUMMON_ANIMAL);
		}

		if (d_summon)
		{
			msg_print("You have attracted an animal!");
			disturb(0, 0);
		}
	}

	if ((p_ptr->muta2 & MUT2_RAW_CHAOS) && !p_ptr->anti_magic &&
		 !rand_int(7500))
	{
		if (disturb_minor) disturb(0, 0);
		msg_print("You feel the world warping around you!");
		msg_print(NULL);
		fire_ball(GF_CHAOS, 0, p_ptr->lev, 8);
	}

	if ((p_ptr->muta2 & MUT2_UNSTABLE) && !rand_int(3000) && !(p_ptr->resist_chaos))
	{
		if (disturb_minor) disturb(0, 0);
		(void)gain_random_mutation(0);
		if (rand_int(5) == 1)  (void)lose_mutation(0);
		if (rand_int(100) == 1) p_ptr->prace = RACE_BEASTMAN;
	}

	if ((p_ptr->muta2 & MUT2_POLY_WOUND) && !rand_int(3000))
	{
		if (disturb_minor) disturb(0, 0);
		do_poly_wounds();
	}

	if ((p_ptr->muta2 & MUT2_WASTING) && !rand_int(3000))
	{
		int which_stat = rand_int(6);
		int sustained = FALSE;

		switch (which_stat)
		{
			case A_STR: if (p_ptr->sustain_str) sustained = TRUE; break;
			case A_INT: if (p_ptr->sustain_int) sustained = TRUE; break;
			case A_WIS: if (p_ptr->sustain_wis) sustained = TRUE; break;
			case A_DEX: if (p_ptr->sustain_dex) sustained = TRUE; break;
			case A_CON: if (p_ptr->sustain_con) sustained = TRUE; break;
			case A_CHR: if (p_ptr->sustain_chr) sustained = TRUE; break;
			default:
				msg_print("Invalid stat chosen!");
				sustained = TRUE;
				break;
		}

		if (!sustained)
		{
			disturb(0, 0);
			msg_print("You can feel yourself wasting away!");
			msg_print(NULL);
			(void)dec_stat(which_stat, randint(6) + 6, randint(3) == 1);
		}
	}

	if ((p_ptr->muta2 & MUT2_ATT_DRAGON) && !p_ptr->anti_magic &&
		 !rand_int(6500))
	{
		bool d_summon = FALSE;

		if (!rand_int(5))
		{
			d_summon = summon_specific_friendly(py, px, dun_level, SUMMON_DRAGON, TRUE);
		}
		else
		{
			d_summon = summon_specific(py, px, dun_level, SUMMON_DRAGON);
		}

		if (d_summon)
		{
			msg_print("You have attracted a dragon!");
			disturb(0,0);
		}
	}

	if ((p_ptr->muta2 & MUT2_WEIRD_MIND) && !p_ptr->anti_magic &&
		 !p_ptr->telepathy && !rand_int(3000))
	{
		if (p_ptr->tim_esp > 0)
		{
			set_tim_esp(0);
		}
		else
		{
			set_tim_esp(p_ptr->lev * 2);
		}
	}

	if ((p_ptr->muta2 & MUT2_NAUSEA) && !p_ptr->slow_digest &&
		 !rand_int(9000))
	{
		disturb(0,0);
		msg_print("Your rebellious stomach rejects your last meal!");
		msg_print(NULL);
		set_food(PY_FOOD_WEAK);
	}

	if ((p_ptr->muta2 & MUT2_INSIGHT) && !p_ptr->anti_magic && !rand_int(2500))
	{
		disturb(0,0);
		msg_print("You have a burst of mental clarity!");
		detect_random();
	}

	if ((p_ptr->muta2 & MUT2_PLANE_SHIFT) && !rand_int(10000))
	{
		disturb(0,0);
		msg_print("You hear voices calling a strange name...");
		new_level_flag = TRUE;
	}

	/* MUT2_WARNING now detects monsters at random. -- Gumby */
	if ((p_ptr->muta2 & MUT2_WARNING) && !rand_int(800))
	{
		msg_print("Your antennae vibrate!");
		(void)detect_monsters_normal();
	}

	if ((p_ptr->muta2 & MUT2_INVULN) && !p_ptr->anti_magic &&
		 !rand_int(5000) && !p_ptr->invuln)
	{
		if (disturb_minor) disturb(0, 0);
		(void)set_invuln(p_ptr->invuln + randint(5) + 5);
		msg_print("You feel invincible!");
	}

	if ((p_ptr->muta2 & MUT2_SP_TO_HP) && !p_ptr->anti_magic &&
		 !rand_int(2000))
	{
		int wounds = p_ptr->mhp - p_ptr->chp;

		if (wounds > 0)
		{
			int healing = p_ptr->csp;

			if (healing > wounds) healing = wounds;

			hp_player(healing);
			p_ptr->csp -= healing;
		}
	}

	if ((p_ptr->muta2 & MUT2_HP_TO_SP) && !p_ptr->anti_magic &&
		!rand_int(2000))
	{
		int wounds = p_ptr->msp - p_ptr->csp;

		if (wounds > 0)
		{
			int healing = p_ptr->chp;

			if (healing > wounds) healing = wounds;

			p_ptr->csp += healing;
			take_hit(healing, "blood rushing to the head");
		}
	}

	if ((p_ptr->muta2 & MUT2_SLOW_MON) && (!rand_int(5000)) && !p_ptr->anti_magic)
	{
		disturb(0, 0);
		msg_print("Yawwwwwn!");
		if (!p_ptr->free_act) p_ptr->slow += 50;
		(void)slow_monsters();
	}

		if ((p_ptr->muta2 & MUT2_POLY_MON) && (!rand_int(5000)) && !p_ptr->anti_magic)
	{
		disturb(0, 0);
		msg_print("Waves of chaotic energy flow from you!");
		(void)poly_all();
	}
}


/*
 * Calculate the effects of mutations on stats, resistances and suchlike.
 * Called from xtra1.c. -- Gumby
 */
void calc_mutations(void)
{
	if (p_ptr->muta2 & MUT2_FLATULENT)
	{
		p_ptr->stat_add[A_CHR] -= 3;
		p_ptr->skill_stl -= 3;
	}

	if (p_ptr->muta2 & MUT2_BORING)	   p_ptr->stat_add[A_CHR] -= 4;

	if (p_ptr->muta3 & MUT3_HYPER_STR)	p_ptr->stat_add[A_STR] += 4;
	if (p_ptr->muta3 & MUT3_PUNY)		   p_ptr->stat_add[A_STR] -= 4;

	if (p_ptr->muta3 & MUT3_HYPER_INT)
	{
		p_ptr->stat_add[A_INT] += 4;
		p_ptr->stat_add[A_WIS] += 4;
	}

	if (p_ptr->muta3 & MUT3_MORONIC)
	{
		p_ptr->stat_add[A_INT] -= 4;
		p_ptr->stat_add[A_WIS] -= 4;
	}

	if (p_ptr->muta3 & MUT3_XTRA_HEART)
	{
		p_ptr->stat_add[A_CON] += 4;
		p_ptr->pspeed += 2;
	}

	if (p_ptr->muta3 & MUT3_XTRA_FAT)
	{
		p_ptr->stat_add[A_CON] += 2;
		p_ptr->pspeed -= 2;
	}

	if (p_ptr->muta3 & MUT3_XTRA_THIN)
	{
		p_ptr->stat_add[A_CON] -= 2;
		p_ptr->pspeed += 2;
	}

	if (p_ptr->muta4 & MUT4_ALBINO)		p_ptr->stat_add[A_CON] -= 5;

	if (p_ptr->muta3 & MUT3_FLESH_ROT)
	{
		p_ptr->stat_add[A_CON] -= 2;
		p_ptr->stat_add[A_CHR] -= 1;
		p_ptr->regenerate = FALSE;
	}

	if (p_ptr->muta3 & MUT3_BLANK_FAC)	p_ptr->stat_add[A_CHR] -= 1;
	if (p_ptr->muta3 & MUT3_XTRA_EYES)
	{
		p_ptr->skill_fos *= 2;
		p_ptr->skill_srh += 20;
		p_ptr->stat_add[A_CHR] -= 2;
	}
	if (p_ptr->muta3 & MUT3_CAMO)	      p_ptr->skill_stl += 3;
	if (p_ptr->muta3 & MUT3_XTRA_NOIS)	p_ptr->skill_stl -= 3;
	if (p_ptr->muta3 & MUT3_INFRAVIS)	p_ptr->see_infra += 3;
	if (p_ptr->muta3 & MUT3_EYES_GLOW)	p_ptr->see_inv = TRUE;

	if (p_ptr->muta3 & MUT3_ELEC_TOUC)
	{
		p_ptr->sh_elec = TRUE;
		p_ptr->resist_elec = TRUE;
	}

	if (p_ptr->muta3 & MUT3_FIRE_BODY)
	{
		p_ptr->sh_fire = TRUE;
		p_ptr->resist_fire = TRUE;
		p_ptr->lite = TRUE;
	}

	if (p_ptr->muta3 & MUT3_LEATHER_SKIN)
	{
		p_ptr->stat_add[A_DEX] -= 1;
		p_ptr->to_a += 8;
		p_ptr->dis_to_a += 8;
	}

	if (p_ptr->muta3 & MUT3_SCALES)
	{
		p_ptr->to_a += 12;
		p_ptr->dis_to_a += 12;
	}

	if (p_ptr->muta3 & MUT3_PLATES)
	{
		p_ptr->stat_add[A_DEX] -= 2;
		p_ptr->to_a += 30;
		p_ptr->dis_to_a += 30;
	}

	if (p_ptr->muta3 & MUT3_FURRY)		p_ptr->resist_cold = TRUE;
	if (p_ptr->muta3 & MUT3_EYE_STALK)
	{
		p_ptr->to_h += (10 + (p_ptr->lev)/5);
		p_ptr->dis_to_h += (10 + (p_ptr->lev)/5);
	}
	if (p_ptr->muta3 & MUT3_REGEN)		p_ptr->regenerate = TRUE;
	if (p_ptr->muta3 & MUT3_ESP)		   p_ptr->telepathy = TRUE;
	if (p_ptr->muta3 & MUT3_XTRA_EYELID)	p_ptr->resist_blind = TRUE;
	if (p_ptr->muta3 & MUT3_SPINES)		p_ptr->sh_spine = TRUE;
	if (p_ptr->muta3 & MUT3_LIMBER)		p_ptr->stat_add[A_DEX] += 3;
	if (p_ptr->muta3 & MUT3_ARTHRITIS)	p_ptr->stat_add[A_DEX] -= 3;
	if (p_ptr->muta3 & MUT3_SLUGGISH)
	{
		p_ptr->to_a -= 10;
		p_ptr->dis_to_a -= 10;
		p_ptr->pspeed -= 3;
	}
	if (p_ptr->muta3 & MUT3_GLOW)
	{
		p_ptr->resist_dark = TRUE;
		p_ptr->resist_lite = TRUE;
		p_ptr->lite = TRUE;
	}

	if (p_ptr->muta4 & MUT4_HAND_MOUTH) p_ptr->stat_add[A_DEX] -= 3;
	if (p_ptr->muta4 & MUT4_WINGS) p_ptr->ffall = TRUE;
	if (p_ptr->muta4 & MUT4_EAT_ROCK) p_ptr->skill_dig += 1600;
	if (p_ptr->muta4 & MUT4_WEEP_BLOOD) p_ptr->skill_srh -= 15;
	if (p_ptr->muta4 & MUT4_GILLS) p_ptr->stat_add[A_CHR] -= 1;
	if (p_ptr->muta4 & MUT4_WARTS) p_ptr->stat_add[A_CHR] -= 3;

	if (p_ptr->muta3 & MUT3_ILL_NORM) p_ptr->stat_add[A_CHR] = 0;
}
