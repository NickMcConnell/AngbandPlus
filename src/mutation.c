/* File: mutation.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Mutation effects (and racial powers) */

#include "angband.h"


bool gain_random_mutation(int choose_mut)
{
	int     attempts_left = 20;
	cptr    muta_desc = "";
	bool    muta_chosen = FALSE;
	u32b    muta_which = 0;
	u32b    *muta_class = NULL;

	if (choose_mut) attempts_left = 1;

	while (attempts_left--)
	{
		switch (choose_mut ? choose_mut : (p_ptr->pclass == CLASS_BERSERKER ? 74+randint1(119) : randint1(193)))
		{
		case 1: case 2: case 3: case 4:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SPIT_ACID;
			muta_desc = "You gain the ability to spit acid.";

			break;
		case 5: case 6: case 7:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BR_FIRE;
			muta_desc = "You gain the ability to breathe fire.";

			break;
		case 8: case 9:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_HYPN_GAZE;
			muta_desc = "Your eyes look mesmerizing...";

			break;
		case 10: case 11:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_TELEKINES;
			muta_desc = "You gain the ability to move objects telekinetically.";

			break;
		case 12: case 13: case 14:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_VTELEPORT;
			muta_desc = "You gain the power of teleportation at will.";

			break;
		case 15: case 16:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIND_BLST;
			muta_desc = "You gain the power of Mind Blast.";

			break;
		case 17: case 18:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RADIATION;
			muta_desc = "You start emitting hard radiation.";

			break;
		case 19: case 20:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_VAMPIRISM;
			muta_desc = "You become vampiric.";

			break;
		case 21: case 22: case 23:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SMELL_MET;
			muta_desc = "You smell a metallic odor.";

			break;
		case 24: case 25: case 26: case 27:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SMELL_MON;
			muta_desc = "You smell filthy monsters.";

			break;
		case 28: case 29: case 30:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BLINK;
			muta_desc = "You gain the power of minor teleportation.";

			break;
		case 31: case 32:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EAT_ROCK;
			muta_desc = "The walls look delicious.";

			break;
		case 33: case 34:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SWAP_POS;
			muta_desc = "You feel like walking a mile in someone else's shoes.";

			break;
		case 35: case 36: case 37:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SHRIEK;
			muta_desc = "Your vocal cords get much tougher.";

			break;
		case 38: case 39: case 40:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_ILLUMINE;
			muta_desc = "You can light up rooms with your presence.";

			break;
		case 41: case 42:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_DET_CURSE;
			muta_desc = "You can feel evil magics.";

			break;
		case 43: case 44: case 45:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BERSERK;
			muta_desc = "You feel a controlled rage.";

			break;
		case 46:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_POLYMORPH;
			muta_desc = "Your body seems mutable.";

			break;
		case 47: case 48:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIDAS_TCH;
			muta_desc = "You gain the Midas touch.";

			break;
		case 49:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_GROW_MOLD;
			muta_desc = "You feel a sudden affinity for mold.";

			break;
		case 50: case 51: case 52:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RESIST;
			muta_desc = "You feel like you can protect yourself.";

			break;
		case 53: case 54: case 55:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EARTHQUAKE;
			muta_desc = "You gain the ability to wreck the dungeon.";

			break;
		case 56:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EAT_MAGIC;
			muta_desc = "Your magic items look delicious.";

			break;
		case 57: case 58:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_WEIGH_MAG;
			muta_desc = "You feel you can better understand the magic around you.";

			break;
		case 59:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_STERILITY;
			muta_desc = "You can give everything around you a headache.";

			break;
		case 60: case 61:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_PANIC_HIT;
			muta_desc = "You suddenly understand how thieves feel.";

			break;
		case 62: case 63: case 64:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_DAZZLE;
			muta_desc = "You gain the ability to emit dazzling lights.";

			break;
		case 65: case 66: case 67:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_LASER_EYE;
			muta_desc = "Your eyes burn for a moment.";

			break;
		case 68: case 69:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RECALL;
			muta_desc = "You feel briefly homesick, but it passes.";

			break;
		case 70:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BANISH;
			muta_desc = "You feel a holy wrath fill you.";

			break;
		case 71: case 72:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_COLD_TOUCH;
			muta_desc = "Your hands get very cold.";

			break;
		case 73: case 74:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_LAUNCHER;
			muta_desc = "Your throwing arm feels much stronger.";

			break;
		case 75:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BERS_RAGE;
			muta_desc = "You become subject to fits of berserk rage!";

			break;
		case 76:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_COWARDICE;
			muta_desc = "You become an incredible coward!";

			break;
		case 77:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RTELEPORT;
			muta_desc = "Your position seems very uncertain...";

			break;
		case 78:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ALCOHOL;
			muta_desc = "Your body starts producing alcohol!";

			break;
		case 79:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HALLU;
			muta_desc = "You are afflicted by a hallucinatory insanity!";

			break;
		case 80:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_FLATULENT;
			muta_desc = "You become subject to uncontrollable flatulence.";

			break;
		case 81: case 82:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SCOR_TAIL;
			muta_desc = "You grow a scorpion tail!";

			break;
		case 83: case 84:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HORNS;
			muta_desc = "Horns pop forth into your forehead!";

			break;
		case 85: case 86:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BEAK;
			muta_desc = "Your mouth turns into a sharp, powerful beak!";

			break;
		case 87: case 88:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_DEMON;
			muta_desc = "You start attracting demons.";

			break;
		case 89:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_PROD_MANA;
			muta_desc = "You start producing magical energy uncontrollably.";

			break;
		case 90: case 91:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SPEED_FLUX;
			muta_desc = "You become manic-depressive.";

			break;
		case 92: case 93:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BANISH_ALL;
			muta_desc = "You feel a terrifying power lurking behind you.";

			break;
		case 94:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_EAT_LIGHT;
			muta_desc = "You feel a strange kinship with Ungoliant.";

			break;
		case 95: case 96:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_TRUNK;
			muta_desc = "Your nose grows into an elephant-like trunk.";

			break;
		case 97:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_ANIMAL;
			muta_desc = "You start attracting animals.";

			break;
		case 98:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_TENTACLES;
			muta_desc = "Evil-looking tentacles sprout from your sides.";

			break;
		case 99:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RAW_CHAOS;
			muta_desc = "You feel the universe is less stable around you.";

			break;
		case 100: case 101: case 102:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_NORMALITY;
			muta_desc = "You feel strangely normal.";

			break;
		case 103:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WRAITH;
			muta_desc = "You start to fade in and out of the physical world.";

			break;
		case 104:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_POLY_WOUND;
			muta_desc = "You feel forces of chaos entering your old scars.";

			break;
		case 105:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WASTING;
			muta_desc = "You suddenly contract a horrible wasting disease.";

			break;
		case 106:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_DRAGON;
			muta_desc = "You start attracting dragons.";

			break;
		case 107: case 108:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WEIRD_MIND;
			muta_desc = "Your thoughts suddenly take off in strange directions.";

			break;
		case 109:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_NAUSEA;
			muta_desc = "Your stomach starts to roil nauseously.";

			break;
		case 110: case 111:
			/* Chaos warriors already have a chaos deity */
			if (p_ptr->pclass != CLASS_CHAOS_WARRIOR)
			{
				muta_class = &(p_ptr->muta2);
				muta_which = MUT2_CHAOS_GIFT;
				muta_desc = "You attract the notice of a chaos deity!";

			}
			break;
		case 112:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WALK_SHAD;
			muta_desc = "You feel like reality is as thin as paper.";

			break;
		case 113: case 114:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WARNING;
			muta_desc = "You suddenly feel paranoid.";

			break;
		case 115:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_INVULN;
			muta_desc = "You are blessed with fits of invulnerability.";

			break;
		case 116: case 117:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SP_TO_HP;
			muta_desc = "You are subject to fits of magical healing.";

			break;
		case 118:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HP_TO_SP;
			muta_desc = "You are subject to fits of painful clarity.";

			break;
		case 119:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_DISARM;
			muta_desc = "Your feet grow to four times their former size.";

			break;
		case 120: case 121: case 122:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HYPER_STR;
			muta_desc = "You turn into a superhuman he-man!";

			break;
		case 123: case 124: case 125:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_PUNY;
			muta_desc = "Your muscles wither away...";

			break;
		case 126: case 127: case 128:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HYPER_INT;
			muta_desc = "Your brain evolves into a living computer!";

			break;
		case 129: case 130: case 131:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MORONIC;
			muta_desc = "Your brain withers away...";

			break;
		case 132: case 133:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_RESILIENT;
			muta_desc = "You become extraordinarily resilient.";

			break;
		case 134: case 135:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_FAT;
			muta_desc = "You become sickeningly fat!";

			break;
		case 136: case 137:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ALBINO;
			muta_desc = "You turn into an albino! You feel frail...";

			break;
		case 138: case 139: case 140:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FLESH_ROT;
			muta_desc = "Your flesh is afflicted by a rotting disease!";

			break;
		case 141: case 142:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SILLY_VOI;
			muta_desc = "Your voice turns into a ridiculous squeak!";

			break;
		case 143: case 144:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_BLANK_FAC;
			muta_desc = "Your face becomes completely featureless!";

			break;
		case 145:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ILL_NORM;
			muta_desc = "You start projecting a reassuring image.";

			break;
		case 146: case 147: case 148:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_EYES;
			muta_desc = "You grow an extra pair of eyes!";

			break;
		case 149: case 150:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MAGIC_RES;
			muta_desc = "You become resistant to magic.";

			break;
		case 151: case 152: case 153:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_NOIS;
			muta_desc = "You start making strange noise!";

			break;
		case 154: case 155: case 156:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_INFRAVIS;
			muta_desc = "Your infravision is improved.";

			break;
		case 157: case 158:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_LEGS;
			muta_desc = "You grow an extra pair of legs!";

			break;
		case 159: case 160:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SHORT_LEG;
			muta_desc = "Your legs turn into short stubs!";

			break;
		case 161: case 162:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ELEC_TOUC;
			muta_desc = "Electricity starts running through you!";

			break;
		case 163: case 164:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FIRE_BODY;
			muta_desc = "Your body is enveloped in flames!";

			break;
		case 165: case 166: case 167:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WART_SKIN;
			muta_desc = "Disgusting warts appear everywhere on you!";

			break;
		case 168: case 169: case 170:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SCALES;
			muta_desc = "Your skin turns into black scales!";

			break;
		case 171: case 172:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_IRON_SKIN;
			muta_desc = "Your skin turns to steel!";

			break;
		case 173: case 174:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WINGS;
			muta_desc = "You grow a pair of wings.";

			break;
		case 175: case 176: case 177:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FEARLESS;
			muta_desc = "You become completely fearless.";

			break;
		case 178: case 179:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_REGEN;
			muta_desc = "You start regenerating.";

			break;
		case 180: case 181:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ESP;
			muta_desc = "You develop a telepathic ability!";

			break;
		case 182: case 183: case 184:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_LIMBER;
			muta_desc = "Your muscles become limber.";

			break;
		case 185: case 186: case 187:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ARTHRITIS;
			muta_desc = "Your joints suddenly hurt.";

			break;
		case 188:
			if (p_ptr->pseikaku == SEIKAKU_LUCKY) break;
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_BAD_LUCK;
			muta_desc = "There is a malignant black aura surrounding you...";

			break;
		case 189:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_VULN_ELEM;
			muta_desc = "You feel strangely exposed.";

			break;
		case 190: case 191: case 192:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MOTION;
			muta_desc = "You move with new assurance.";

			break;
		case 193:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_GOOD_LUCK;
			muta_desc = "There is a benevolent white aura surrounding you...";

			break;
		default:
			muta_class = NULL;
			muta_which = 0;
		}

		if (muta_class && muta_which)
		{
			if (!(*muta_class & muta_which))
			{
				muta_chosen = TRUE;
			}
		}
		if (muta_chosen == TRUE) break;
	}

	if (!muta_chosen)
	{
		msg_print("You feel normal.");

		return FALSE;
	}
	else
	{
		chg_virtue(V_CHANCE, 1);

		/*
		  some races are apt to gain specified mutations
		  This should be allowed only if "choose_mut" is 0.
							--- henkma
		*/
		if(!choose_mut){
			if (p_ptr->prace == RACE_VAMPIRE &&
			  !(p_ptr->muta1 & MUT1_HYPN_GAZE) &&
			   (randint1(10) < 7))
			{
				muta_class = &(p_ptr->muta1);
				muta_which = MUT1_HYPN_GAZE;
				muta_desc = "Your eyes look mesmerizing...";

			}

			else if (p_ptr->prace == RACE_IMP &&
				 !(p_ptr->muta2 & MUT2_HORNS) &&
				 (randint1(10) < 7))
			  {
				muta_class = &(p_ptr->muta2);
				muta_which = MUT2_HORNS;
				muta_desc = "Horns pop forth into your forehead!";

			}

			else if (p_ptr->prace == RACE_YEEK &&
				!(p_ptr->muta1 & MUT1_SHRIEK) &&
				(randint1(10) < 7))
			{
				muta_class = &(p_ptr->muta1);
				muta_which = MUT1_SHRIEK;
				muta_desc = "Your vocal cords get much tougher.";

			}

			else if (p_ptr->prace == RACE_BEASTMAN &&
				!(p_ptr->muta1 & MUT1_POLYMORPH) &&
				(randint1(10) < 2))
			{
				muta_class = &(p_ptr->muta1);
				muta_which = MUT1_POLYMORPH;
				muta_desc = "Your body seems mutable.";

			}

			else if (p_ptr->prace == RACE_MIND_FLAYER &&
				!(p_ptr->muta2 & MUT2_TENTACLES) &&
				(randint1(10) < 7))
			{
				muta_class = &(p_ptr->muta2);
				muta_which = MUT2_TENTACLES;
				muta_desc = "Evil-looking tentacles sprout from your mouth.";

			}
		}
		msg_print("You mutate!");

		msg_print(muta_desc);
		*muta_class |= muta_which;

		if (muta_class == &(p_ptr->muta3))
		{
			if (muta_which == MUT3_PUNY)
			{
				if ( (p_ptr->muta3 & MUT3_HYPER_STR)
				 && !(p_ptr->muta3_lock & MUT3_HYPER_STR))
				{
					msg_print("You no longer feel super-strong!");

					p_ptr->muta3 &= ~(MUT3_HYPER_STR);
				}
			}
			else if (muta_which == MUT3_HYPER_STR)
			{
				if ( (p_ptr->muta3 & MUT3_PUNY)
				 && !(p_ptr->muta3_lock & MUT3_PUNY))
				{
					msg_print("You no longer feel puny!");

					p_ptr->muta3 &= ~(MUT3_PUNY);
				}
			}
			else if (muta_which == MUT3_MORONIC)
			{
				if ((p_ptr->muta3 & MUT3_HYPER_INT) && !(p_ptr->muta3_lock & MUT3_HYPER_INT))
				{
					msg_print("Your brain is no longer a living computer.");

					p_ptr->muta3 &= ~(MUT3_HYPER_INT);
				}
			}
			else if (muta_which == MUT3_HYPER_INT)
			{
				if ((p_ptr->muta3 & MUT3_MORONIC) && !(p_ptr->muta3_lock & MUT3_MORONIC))
				{
					msg_print("You are no longer moronic.");

					p_ptr->muta3 &= ~(MUT3_MORONIC);
				}
			}
			else if (muta_which == MUT3_IRON_SKIN)
			{
				if ((p_ptr->muta3 & MUT3_SCALES) && !(p_ptr->muta3_lock & MUT3_SCALES))
				{
					msg_print("You lose your scales.");

					p_ptr->muta3 &= ~(MUT3_SCALES);
				}
				if ((p_ptr->muta3 & MUT3_FLESH_ROT) && !(p_ptr->muta3_lock & MUT3_FLESH_ROT))
				{
					msg_print("Your flesh rots no longer.");

					p_ptr->muta3 &= ~(MUT3_FLESH_ROT);
				}
				if ((p_ptr->muta3 & MUT3_WART_SKIN) && !(p_ptr->muta3_lock & MUT3_WART_SKIN))
				{
					msg_print("You lose your warts.");

					p_ptr->muta3 &= ~(MUT3_WART_SKIN);
				}
			}
			else if (muta_which == MUT3_WART_SKIN || muta_which == MUT3_SCALES
				|| muta_which == MUT3_FLESH_ROT)
			{
				if ((p_ptr->muta3 & MUT3_IRON_SKIN) && !(p_ptr->muta3_lock & MUT3_IRON_SKIN))
				{
					msg_print("Your skin is no longer made of steel.");

					p_ptr->muta3 &= ~(MUT3_IRON_SKIN);
				}
			}
			else if (muta_which == MUT3_FEARLESS)
			{
				if ((p_ptr->muta2 & MUT2_COWARDICE) && !(p_ptr->muta2_lock & MUT2_COWARDICE))
				{
					msg_print("You are no longer cowardly.");

					p_ptr->muta2 &= ~(MUT2_COWARDICE);
				}
			}
			else if (muta_which == MUT3_FLESH_ROT)
			{
				if ((p_ptr->muta3 & MUT3_REGEN) && !(p_ptr->muta3_lock & MUT3_REGEN))
				{
					msg_print("You stop regenerating.");

					p_ptr->muta3 &= ~(MUT3_REGEN);
				}
			}
			else if (muta_which == MUT3_REGEN)
			{
				if ((p_ptr->muta3 & MUT3_FLESH_ROT) && !(p_ptr->muta3_lock & MUT3_FLESH_ROT))
				{
					msg_print("Your flesh stops rotting.");

					p_ptr->muta3 &= ~(MUT3_FLESH_ROT);
				}
			}
			else if (muta_which == MUT3_LIMBER)
			{
				if ((p_ptr->muta3 & MUT3_ARTHRITIS) && !(p_ptr->muta3_lock & MUT3_ARTHRITIS))
				{
					msg_print("Your joints stop hurting.");

					p_ptr->muta3 &= ~(MUT3_ARTHRITIS);
				}
			}
			else if (muta_which == MUT3_ARTHRITIS)
			{
				if ((p_ptr->muta3 & MUT3_LIMBER) && !(p_ptr->muta3_lock & MUT3_LIMBER))
				{
					msg_print("You no longer feel limber.");

					p_ptr->muta3 &= ~(MUT3_LIMBER);
				}
			}
		}
		else if (muta_class == &(p_ptr->muta2))
		{
			if (muta_which == MUT2_COWARDICE)
			{
				if ((p_ptr->muta3 & MUT3_FEARLESS) && !(p_ptr->muta3_lock & MUT3_FEARLESS))
				{
					msg_print("You no longer feel fearless.");

					p_ptr->muta3 &= ~(MUT3_FEARLESS);
				}
			}
			if (muta_which == MUT2_BEAK)
			{
				if ((p_ptr->muta2 & MUT2_TRUNK) && !(p_ptr->muta2_lock & MUT2_TRUNK))
				{
					msg_print("Your nose is no longer elephantine.");

					p_ptr->muta2 &= ~(MUT2_TRUNK);
				}
			}
			if (muta_which == MUT2_TRUNK)
			{
				if ((p_ptr->muta2 & MUT2_BEAK) && !(p_ptr->muta2_lock & MUT2_BEAK))
				{
					msg_print("You no longer have a hard beak.");

					p_ptr->muta2 &= ~(MUT2_BEAK);
				}
			}
		}

		mutant_regenerate_mod = calc_mutant_regenerate_mod();
		p_ptr->update |= PU_BONUS;
		handle_stuff();
		return TRUE;
	}
}


bool lose_mutation(int choose_mut)
{
	int attempts_left = 20;
	cptr muta_desc = "";
	bool muta_chosen = FALSE;
	u32b muta_which = 0;
	u32b *muta_class = NULL;
	u32b *muta_class_lock = NULL;

	if (choose_mut) attempts_left = 1;

	while (attempts_left--)
	{
		switch (choose_mut ? choose_mut : randint1(193))
		{
		case 1: case 2: case 3: case 4:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_SPIT_ACID;
			muta_desc = "You lose the ability to spit acid.";

			break;
		case 5: case 6: case 7:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_BR_FIRE;
			muta_desc = "You lose the ability to breathe fire.";

			break;
		case 8: case 9:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_HYPN_GAZE;
			muta_desc = "Your eyes look uninteresting.";

			break;
		case 10: case 11:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_TELEKINES;
			muta_desc = "You lose the ability to move objects telekinetically.";

			break;
		case 12: case 13: case 14:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_VTELEPORT;
			muta_desc = "You lose the power of teleportation at will.";

			break;
		case 15: case 16:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_MIND_BLST;
			muta_desc = "You lose the power of Mind Blast.";

			break;
		case 17: case 18:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_RADIATION;
			muta_desc = "You stop emitting hard radiation.";

			break;
		case 19: case 20:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_VAMPIRISM;
			muta_desc = "You are no longer vampiric.";

			break;
		case 21: case 22: case 23:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_SMELL_MET;
			muta_desc = "You no longer smell a metallic odor.";

			break;
		case 24: case 25: case 26: case 27:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_SMELL_MON;
			muta_desc = "You no longer smell filthy monsters.";

			break;
		case 28: case 29: case 30:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_BLINK;
			muta_desc = "You lose the power of minor teleportation.";

			break;
		case 31: case 32:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_EAT_ROCK;
			muta_desc = "The walls look unappetizing.";

			break;
		case 33: case 34:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_SWAP_POS;
			muta_desc = "You feel like staying in your own shoes.";

			break;
		case 35: case 36: case 37:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_SHRIEK;
			muta_desc = "Your vocal cords get much weaker.";

			break;
		case 38: case 39: case 40:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_ILLUMINE;
			muta_desc = "You can no longer light up rooms with your presence.";

			break;
		case 41: case 42:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_DET_CURSE;
			muta_desc = "You can no longer feel evil magics.";

			break;
		case 43: case 44: case 45:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_BERSERK;
			muta_desc = "You no longer feel a controlled rage.";

			break;
		case 46:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_POLYMORPH;
			muta_desc = "Your body seems stable.";

			break;
		case 47: case 48:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_MIDAS_TCH;
			muta_desc = "You lose the Midas touch.";

			break;
		case 49:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_GROW_MOLD;
			muta_desc = "You feel a sudden dislike for mold.";

			break;
		case 50: case 51: case 52:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_RESIST;
			muta_desc = "You feel like you might be vulnerable.";

			break;
		case 53: case 54: case 55:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_EARTHQUAKE;
			muta_desc = "You lose the ability to wreck the dungeon.";

			break;
		case 56:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_EAT_MAGIC;
			muta_desc = "Your magic items no longer look delicious.";

			break;
		case 57: case 58:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_WEIGH_MAG;
			muta_desc = "You no longer sense magic.";

			break;
		case 59:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_STERILITY;
			muta_desc = "You hear a massed sigh of relief.";

			break;
		case 60: case 61:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_PANIC_HIT;
			muta_desc = "You no longer feel jumpy.";

			break;
		case 62: case 63: case 64:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_DAZZLE;
			muta_desc = "You lose the ability to emit dazzling lights.";

			break;
		case 65: case 66: case 67:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_LASER_EYE;
			muta_desc = "Your eyes burn for a moment, then feel soothed.";

			break;
		case 68: case 69:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_RECALL;
			muta_desc = "You feel briefly homesick.";

			break;
		case 70:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_BANISH;
			muta_desc = "You no longer feel a holy wrath.";

			break;
		case 71: case 72:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_COLD_TOUCH;
			muta_desc = "Your hands warm up.";

			break;
		case 73: case 74:
			muta_class = &(p_ptr->muta1);
			muta_class_lock = &(p_ptr->muta1_lock);
			muta_which = MUT1_LAUNCHER;
			muta_desc = "Your throwing arm feels much weaker.";

			break;
		case 75:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_BERS_RAGE;
			muta_desc = "You are no longer subject to fits of berserk rage!";

			break;
		case 76:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_COWARDICE;
			muta_desc = "You are no longer an incredible coward!";

			break;
		case 77:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_RTELEPORT;
			muta_desc = "Your position seems more certain.";

			break;
		case 78:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_ALCOHOL;
			muta_desc = "Your body stops producing alcohol!";

			break;
		case 79:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_HALLU;
			muta_desc = "You are no longer afflicted by a hallucinatory insanity!";

			break;
		case 80:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_FLATULENT;
			muta_desc = "You are no longer subject to uncontrollable flatulence.";

			break;
		case 81: case 82:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_SCOR_TAIL;
			muta_desc = "You lose your scorpion tail!";

			break;
		case 83: case 84:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_HORNS;
			muta_desc = "Your horns vanish from your forehead!";

			break;
		case 85: case 86:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_BEAK;
			muta_desc = "Your mouth reverts to normal!";

			break;
		case 87: case 88:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_ATT_DEMON;
			muta_desc = "You stop attracting demons.";

			break;
		case 89:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_PROD_MANA;
			muta_desc = "You stop producing magical energy uncontrollably.";

			break;
		case 90: case 91:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_SPEED_FLUX;
			muta_desc = "You are no longer manic-depressive.";

			break;
		case 92: case 93:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_BANISH_ALL;
			muta_desc = "You no longer feel a terrifying power lurking behind you.";

			break;
		case 94:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_EAT_LIGHT;
			muta_desc = "You feel the world's a brighter place.";

			break;
		case 95: case 96:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_TRUNK;
			muta_desc = "Your nose returns to a normal length.";

			break;
		case 97:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_ATT_ANIMAL;
			muta_desc = "You stop attracting animals.";

			break;
		case 98:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_TENTACLES;
			muta_desc = "Your tentacles vanish from your sides.";

			break;
		case 99:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_RAW_CHAOS;
			muta_desc = "You feel the universe is more stable around you.";

			break;
		case 100: case 101: case 102:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_NORMALITY;
			muta_desc = "You feel normally strange.";

			break;
		case 103:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_WRAITH;
			muta_desc = "You are firmly in the physical world.";

			break;
		case 104:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_POLY_WOUND;
			muta_desc = "You feel forces of chaos departing your old scars.";

			break;
		case 105:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_WASTING;
			muta_desc = "You are cured of the horrible wasting disease!";

			break;
		case 106:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_ATT_DRAGON;
			muta_desc = "You stop attracting dragons.";

			break;
		case 107: case 108:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_WEIRD_MIND;
			muta_desc = "Your thoughts return to boring paths.";

			break;
		case 109:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_NAUSEA;
			muta_desc = "Your stomach stops roiling.";

			break;
		case 110: case 111:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_CHAOS_GIFT;
			muta_desc = "You lose the attention of the chaos deities.";

			break;
		case 112:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_WALK_SHAD;
			muta_desc = "You feel like you're trapped in reality.";

			break;
		case 113: case 114:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_WARNING;
			muta_desc = "You no longer feel paranoid.";

			break;
		case 115:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_INVULN;
			muta_desc = "You are no longer blessed with fits of invulnerability.";

			break;
		case 116: case 117:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_SP_TO_HP;
			muta_desc = "You are no longer subject to fits of magical healing.";

			break;
		case 118:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_HP_TO_SP;
			muta_desc = "You are no longer subject to fits of painful clarity.";

			break;
		case 119:
			muta_class = &(p_ptr->muta2);
			muta_class_lock = &(p_ptr->muta2_lock);
			muta_which = MUT2_DISARM;
			muta_desc = "Your feet shrink to their former size.";

			break;
		case 120: case 121: case 122:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_HYPER_STR;
			muta_desc = "Your muscles revert to normal.";

			break;
		case 123: case 124: case 125:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_PUNY;
			muta_desc = "Your muscles revert to normal.";

			break;
		case 126: case 127: case 128:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_HYPER_INT;
			muta_desc = "Your brain reverts to normal.";

			break;
		case 129: case 130: case 131:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_MORONIC;
			muta_desc = "Your brain reverts to normal.";

			break;
		case 132: case 133:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_RESILIENT;
			muta_desc = "You become ordinarily resilient again.";

			break;
		case 134: case 135:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_XTRA_FAT;
			muta_desc = "You benefit from a miracle diet!";

			break;
		case 136: case 137:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_ALBINO;
			muta_desc = "You are no longer an albino!";

			break;
		case 138: case 139: case 140:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_FLESH_ROT;
			muta_desc = "Your flesh is no longer afflicted by a rotting disease!";

			break;
		case 141: case 142:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_SILLY_VOI;
			muta_desc = "Your voice returns to normal.";

			break;
		case 143: case 144:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_BLANK_FAC;
			muta_desc = "Your facial features return.";

			break;
		case 145:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_ILL_NORM;
			muta_desc = "You stop projecting a reassuring image.";

			break;
		case 146: case 147: case 148:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_XTRA_EYES;
			muta_desc = "Your extra eyes vanish!";

			break;
		case 149: case 150:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_MAGIC_RES;
			muta_desc = "You become susceptible to magic again.";

			break;
		case 151: case 152: case 153:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_XTRA_NOIS;
			muta_desc = "You stop making strange noise!";

			break;
		case 154: case 155: case 156:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_INFRAVIS;
			muta_desc = "Your infravision is degraded.";

			break;
		case 157: case 158:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_XTRA_LEGS;
			muta_desc = "Your extra legs disappear!";

			break;
		case 159: case 160:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_SHORT_LEG;
			muta_desc = "Your legs lengthen to normal.";

			break;
		case 161: case 162:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_ELEC_TOUC;
			muta_desc = "Electricity stops running through you.";

			break;
		case 163: case 164:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_FIRE_BODY;
			muta_desc = "Your body is no longer enveloped in flames.";

			break;
		case 165: case 166: case 167:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_WART_SKIN;
			muta_desc = "Your warts disappear!";

			break;
		case 168: case 169: case 170:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_SCALES;
			muta_desc = "Your scales vanish!";

			break;
		case 171: case 172:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_IRON_SKIN;
			muta_desc = "Your skin reverts to flesh!";

			break;
		case 173: case 174:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_WINGS;
			muta_desc = "Your wings fall off.";

			break;
		case 175: case 176: case 177:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_FEARLESS;
			muta_desc = "You begin to feel fear again.";

			break;
		case 178: case 179:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_REGEN;
			muta_desc = "You stop regenerating.";

			break;
		case 180: case 181:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_ESP;
			muta_desc = "You lose your telepathic ability!";

			break;
		case 182: case 183: case 184:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_LIMBER;
			muta_desc = "Your muscles stiffen.";

			break;
		case 185: case 186: case 187:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_ARTHRITIS;
			muta_desc = "Your joints stop hurting.";

			break;
		case 188:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_BAD_LUCK;
			muta_desc = "Your black aura swirls and fades.";

			break;
		case 189:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_VULN_ELEM;
			muta_desc = "You feel less exposed.";

			break;
		case 190: case 191: case 192:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_MOTION;
			muta_desc = "You move with less assurance.";

			break;
		case 193:
			muta_class = &(p_ptr->muta3);
			muta_class_lock = &(p_ptr->muta3_lock);
			muta_which = MUT3_GOOD_LUCK;
			muta_desc = "Your white aura shimmers and fades.";

			break;
		default:
			muta_class = NULL;
			muta_class_lock = NULL;
			muta_which = 0;
		}

		if (muta_class && muta_class_lock && muta_which)
		{
			if ( (*(muta_class) & muta_which)
			  && !(*(muta_class_lock) & muta_which))
			{
				muta_chosen = TRUE;
			}
		}
		if (muta_chosen == TRUE) break;
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
		mutant_regenerate_mod = calc_mutant_regenerate_mod();
		return TRUE;
	}
}


void dump_mutations(FILE *OutFile)
{
	if (!OutFile) return;

	if (p_ptr->muta1)
	{
		if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
			fprintf(OutFile, " You can spit acid (dam lvl).\n");

		}
		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
			fprintf(OutFile, " You can breathe fire (dam lvl * 2).\n");

		}
		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
			fprintf(OutFile, " Your gaze is hypnotic.\n");

		}
		if (p_ptr->muta1 & MUT1_TELEKINES)
		{
			fprintf(OutFile, " You are telekinetic.\n");

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
			fprintf(OutFile, " You can emit hard radiation at will.\n");

		}
		if (p_ptr->muta1 & MUT1_VAMPIRISM)
		{
			fprintf(OutFile, " You can drain life from a foe like a vampire.\n");

		}
		if (p_ptr->muta1 & MUT1_SMELL_MET)
		{
			fprintf(OutFile, " You can smell nearby precious metal.\n");

		}
		if (p_ptr->muta1 & MUT1_SMELL_MON)
		{
			fprintf(OutFile, " You can smell nearby monsters.\n");

		}
		if (p_ptr->muta1 & MUT1_BLINK)
		{
			fprintf(OutFile, " You can teleport yourself short distances.\n");

		}
		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
			fprintf(OutFile, " You can consume solid rock.\n");

		}
		if (p_ptr->muta1 & MUT1_SWAP_POS)
		{
			fprintf(OutFile, " You can switch locations with another being.\n");

		}
		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
			fprintf(OutFile, " You can emit a horrible shriek.\n");

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
		if (p_ptr->muta1 & MUT1_POLYMORPH)
		{
			fprintf(OutFile, " You can polymorph yourself at will.\n");

		}
		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
			fprintf(OutFile, " You can turn ordinary items to gold.\n");

		}
		if (p_ptr->muta1 & MUT1_GROW_MOLD)
		{
			fprintf(OutFile, " You can cause mold to grow near you.\n");

		}
		if (p_ptr->muta1 & MUT1_RESIST)
		{
			fprintf(OutFile, " You can harden yourself to the ravages of the elements.\n");

		}
		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
			fprintf(OutFile, " You can bring down the dungeon around your ears.\n");

		}
		if (p_ptr->muta1 & MUT1_EAT_MAGIC)
		{
			fprintf(OutFile, " You can consume magic energy for your own use.\n");

		}
		if (p_ptr->muta1 & MUT1_WEIGH_MAG)
		{
			fprintf(OutFile, " You can feel the strength of the magics affecting you.\n");

		}
		if (p_ptr->muta1 & MUT1_STERILITY)
		{
			fprintf(OutFile, " You can cause mass impotence.\n");

		}
		if (p_ptr->muta1 & MUT1_PANIC_HIT)
		{
			fprintf(OutFile, " You can run for your life after hitting something.\n");

		}
		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
			fprintf(OutFile, " You can emit confusing, blinding radiation.\n");

		}
		if (p_ptr->muta1 & MUT1_LASER_EYE)
		{
			fprintf(OutFile, " Your eyes can fire laser beams.\n");

		}
		if (p_ptr->muta1 & MUT1_RECALL)
		{
			fprintf(OutFile, " You can travel between town and the depths.\n");

		}
		if (p_ptr->muta1 & MUT1_BANISH)
		{
			fprintf(OutFile, " You can send evil creatures directly to Hell.\n");

		}
		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
			fprintf(OutFile, " You can freeze things with a touch.\n");

		}
		if (p_ptr->muta1 & MUT1_LAUNCHER)
		{
			fprintf(OutFile, " You can hurl objects with great force.\n");

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
		if (p_ptr->muta2 & MUT2_PROD_MANA)
		{
			fprintf(OutFile, " You are producing magical energy uncontrollably.\n");

		}
		if (p_ptr->muta2 & MUT2_ATT_DEMON)
		{
			fprintf(OutFile, " You attract demons.\n");

		}
		if (p_ptr->muta2 & MUT2_SCOR_TAIL)
		{
			fprintf(OutFile, " You have a scorpion tail (poison, 3d7).\n");

		}
		if (p_ptr->muta2 & MUT2_HORNS)
		{
			fprintf(OutFile, " You have horns (dam. 2d6).\n");

		}
		if (p_ptr->muta2 & MUT2_BEAK)
		{
			fprintf(OutFile, " You have a beak (dam. 2d4).\n");

		}
		if (p_ptr->muta2 & MUT2_SPEED_FLUX)
		{
			fprintf(OutFile, " You move faster or slower randomly.\n");

		}
		if (p_ptr->muta2 & MUT2_BANISH_ALL)
		{
			fprintf(OutFile, " You sometimes cause nearby creatures to vanish.\n");

		}
		if (p_ptr->muta2 & MUT2_EAT_LIGHT)
		{
			fprintf(OutFile, " You sometimes feed off of the light around you.\n");

		}
		if (p_ptr->muta2 & MUT2_TRUNK)
		{
			fprintf(OutFile, " You have an elephantine trunk (dam 1d4).\n");

		}
		if (p_ptr->muta2 & MUT2_ATT_ANIMAL)
		{
			fprintf(OutFile, " You attract animals.\n");

		}
		if (p_ptr->muta2 & MUT2_TENTACLES)
		{
			fprintf(OutFile, " You have evil looking tentacles (dam 2d5).\n");

		}
		if (p_ptr->muta2 & MUT2_RAW_CHAOS)
		{
			fprintf(OutFile, " You occasionally are surrounded with raw chaos.\n");

		}
		if (p_ptr->muta2 & MUT2_NORMALITY)
		{
			fprintf(OutFile, " You may be mutated, but you're recovering.\n");

		}
		if (p_ptr->muta2 & MUT2_WRAITH)
		{
			fprintf(OutFile, " You fade in and out of physical reality.\n");

		}
		if (p_ptr->muta2 & MUT2_POLY_WOUND)
		{
			fprintf(OutFile, " Your health is subject to chaotic forces.\n");

		}
		if (p_ptr->muta2 & MUT2_WASTING)
		{
			fprintf(OutFile, " You have a horrible wasting disease.\n");

		}
		if (p_ptr->muta2 & MUT2_ATT_DRAGON)
		{
			fprintf(OutFile, " You attract dragons.\n");

		}
		if (p_ptr->muta2 & MUT2_WEIRD_MIND)
		{
			fprintf(OutFile, " Your mind randomly expands and contracts.\n");

		}
		if (p_ptr->muta2 & MUT2_NAUSEA)
		{
			fprintf(OutFile, " You have a seriously upset stomach.\n");

		}
		if (p_ptr->muta2 & MUT2_CHAOS_GIFT)
		{
			fprintf(OutFile, " Chaos deities give you gifts.\n");

		}
		if (p_ptr->muta2 & MUT2_WALK_SHAD)
		{
			fprintf(OutFile, " You occasionally stumble into other shadows.\n");

		}
		if (p_ptr->muta2 & MUT2_WARNING)
		{
			fprintf(OutFile, " You receive warnings about your foes.\n");

		}
		if (p_ptr->muta2 & MUT2_INVULN)
		{
			fprintf(OutFile, " You occasionally feel invincible.\n");

		}
		if (p_ptr->muta2 & MUT2_SP_TO_HP)
		{
			fprintf(OutFile, " Your blood sometimes rushes to your muscles.\n");

		}
		if (p_ptr->muta2 & MUT2_HP_TO_SP)
		{
			fprintf(OutFile, " Your blood sometimes rushes to your head.\n");

		}
		if (p_ptr->muta2 & MUT2_DISARM)
		{
			fprintf(OutFile, " You occasionally stumble and drop things.\n");

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
			fprintf(OutFile, " Your brain is a living computer (+4 INT/WIS).\n");

		}
		if (p_ptr->muta3 & MUT3_MORONIC)
		{
			fprintf(OutFile, " You are moronic (-4 INT/WIS).\n");

		}
		if (p_ptr->muta3 & MUT3_RESILIENT)
		{
			fprintf(OutFile, " You are very resilient (+4 CON).\n");

		}
		if (p_ptr->muta3 & MUT3_XTRA_FAT)
		{
			fprintf(OutFile, " You are extremely fat (+2 CON, -2 speed).\n");

		}
		if (p_ptr->muta3 & MUT3_ALBINO)
		{
			fprintf(OutFile, " You are albino (-4 CON).\n");

		}
		if (p_ptr->muta3 & MUT3_FLESH_ROT)
		{
			fprintf(OutFile, " Your flesh is rotting (-2 CON, -1 CHR).\n");

		}
		if (p_ptr->muta3 & MUT3_SILLY_VOI)
		{
			fprintf(OutFile, " Your voice is a silly squeak (-4 CHR).\n");

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
			fprintf(OutFile, " You have an extra pair of eyes (+15 search).\n");

		}
		if (p_ptr->muta3 & MUT3_MAGIC_RES)
		{
			fprintf(OutFile, " You are resistant to magic.\n");

		}
		if (p_ptr->muta3 & MUT3_XTRA_NOIS)
		{
			fprintf(OutFile, " You make a lot of strange noise (-3 stealth).\n");

		}
		if (p_ptr->muta3 & MUT3_INFRAVIS)
		{
			fprintf(OutFile, " You have remarkable infravision (+3).\n");

		}
		if (p_ptr->muta3 & MUT3_XTRA_LEGS)
		{
			fprintf(OutFile, " You have an extra pair of legs (+3 speed).\n");

		}
		if (p_ptr->muta3 & MUT3_SHORT_LEG)
		{
			fprintf(OutFile, " Your legs are short stubs (-3 speed).\n");

		}
		if (p_ptr->muta3 & MUT3_ELEC_TOUC)
		{
			fprintf(OutFile, " Electricity is running through your veins.\n");

		}
		if (p_ptr->muta3 & MUT3_FIRE_BODY)
		{
			fprintf(OutFile, " Your body is enveloped in flames.\n");

		}
		if (p_ptr->muta3 & MUT3_WART_SKIN)
		{
			fprintf(OutFile, " Your skin is covered with warts (-2 CHR, +5 AC).\n");

		}
		if (p_ptr->muta3 & MUT3_SCALES)
		{
			fprintf(OutFile, " Your skin has turned into scales (-1 CHR, +10 AC).\n");

		}
		if (p_ptr->muta3 & MUT3_IRON_SKIN)
		{
			fprintf(OutFile, " Your skin is made of steel (-1 DEX, +25 AC).\n");

		}
		if (p_ptr->muta3 & MUT3_WINGS)
		{
			fprintf(OutFile, " You have wings.\n");

		}
		if (p_ptr->muta3 & MUT3_FEARLESS)
		{
			fprintf(OutFile, " You are completely fearless.\n");

		}
		if (p_ptr->muta3 & MUT3_REGEN)
		{
			fprintf(OutFile, " You are regenerating.\n");

		}
		if (p_ptr->muta3 & MUT3_ESP)
		{
			fprintf(OutFile, " You are telepathic.\n");

		}
		if (p_ptr->muta3 & MUT3_LIMBER)
		{
			fprintf(OutFile, " Your body is very limber (+3 DEX).\n");

		}
		if (p_ptr->muta3 & MUT3_ARTHRITIS)
		{
			fprintf(OutFile, " Your joints ache constantly (-3 DEX).\n");

		}
		if (p_ptr->muta3 & MUT3_VULN_ELEM)
		{
			fprintf(OutFile, " You are susceptible to damage from the elements.\n");

		}
		if (p_ptr->muta3 & MUT3_MOTION)
		{
			fprintf(OutFile, " Your movements are precise and forceful (+1 STL).\n");

		}
		if (p_ptr->muta3 & MUT3_GOOD_LUCK)
		{
			fprintf(OutFile, " There is a white aura surrounding you.\n");

		}
		if (p_ptr->muta3 & MUT3_BAD_LUCK)
		{
			fprintf(OutFile, " There is a black aura surrounding you.\n");

		}
	}
}

int count_unlocked_mutations(void)
{
	return (count_bits(p_ptr->muta1 & ~(p_ptr->muta1_lock)) +
		count_bits(p_ptr->muta2 & ~(p_ptr->muta2_lock)) +
        count_bits(p_ptr->muta3 & ~(p_ptr->muta3_lock)));
}


/*
 * Return the modifier to the regeneration rate
 * (in percent)
 */
int calc_mutant_regenerate_mod(void)
{
	int regen;
	int mod = 10;
	int count = count_unlocked_mutations(); /* locked mutations do not penalize the player */

	/*
	 * Beastman get 10 "free" mutations and
	 * only 5% decrease per additional mutation
	 */

	if (p_ptr->prace == RACE_BEASTMAN)
	{
		count -= 10;
		mod = 5;
	}

	/* No negative modifier */
	if (count <= 0) return 100;

	regen = 100 - count * mod;

	/* Max. 90% decrease in regeneration speed */
	if (regen < 10) regen = 10;

	return (regen);
}


void mutation_stop_mouth()
{
	if (music_singing_any()) stop_singing();
	if (hex_spelling_any()) stop_hex_spell_all();
}


bool mutation_power_aux(u32b power)
{
	int     dir = 0;
	int     lvl = p_ptr->lev;


	switch (power)
	{
		case MUT1_SPIT_ACID:
			return cast_spit_acid();

		case MUT1_BR_FIRE:
			return cast_breathe_fire();

		case MUT1_HYPN_GAZE:
			return cast_hypnotic_gaze();

		case MUT1_TELEKINES:
			return cast_telekinesis();

		case MUT1_VTELEPORT:
			return cast_teleport();

		case MUT1_MIND_BLST:
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("You concentrate...");

			fire_bolt(GF_PSI, dir, damroll(3 + ((lvl - 1) / 5), 3));
			break;

		case MUT1_RADIATION:
			msg_print("Radiation flows from your body!");

			fire_ball(GF_NUKE, 0, (lvl * 2), 3 + (lvl / 20));
			break;

		case MUT1_VAMPIRISM:
			{
				int x, y, dummy;
				cave_type *c_ptr;

				/* Only works on adjacent monsters */
				if (!get_rep_dir2(&dir)) return FALSE;
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				mutation_stop_mouth();

				if (!(c_ptr->m_idx))
				{
					msg_print("You bite into thin air!");

					break;
				}

				msg_print("You grin and bare your fangs...");


				dummy = lvl * 2;

				if (drain_life(dir, dummy))
				{
					if (p_ptr->food < PY_FOOD_FULL)
						/* No heal if we are "full" */
						(void)hp_player(dummy);
					else
						msg_print("You were not hungry.");

					/* Gain nutritional sustenance: 150/hp drained */
					/* A Food ration gives 5000 food points (by contrast) */
					/* Don't ever get more than "Full" this way */
					/* But if we ARE Gorged,  it won't cure us */
					dummy = p_ptr->food + MIN(5000, 100 * dummy);
					if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
						(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX-1 : dummy);
				}
				else
					msg_print("Yechh. That tastes foul.");

			}
			break;

		case MUT1_SMELL_MET:
			mutation_stop_mouth();
			(void)detect_treasure(DETECT_RAD_DEFAULT);
			break;

		case MUT1_SMELL_MON:
			mutation_stop_mouth();
			(void)detect_monsters_normal(DETECT_RAD_DEFAULT);
			break;

		case MUT1_BLINK:
			teleport_player(10, 0L);
			break;

		case MUT1_EAT_ROCK:
			{
				int x, y;
				cave_type *c_ptr;
				feature_type *f_ptr, *mimic_f_ptr;

				if (!get_rep_dir2(&dir)) return FALSE;
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];
				f_ptr = &f_info[c_ptr->feat];
				mimic_f_ptr = &f_info[get_feat_mimic(c_ptr)];

				mutation_stop_mouth();

				if (!have_flag(mimic_f_ptr->flags, FF_HURT_ROCK))
				{
					msg_print("You cannot eat this feature.");
					break;
				}
				else if (have_flag(f_ptr->flags, FF_PERMANENT))
				{
					msg_format("Ouch!  This %s is harder than your teeth!", f_name + mimic_f_ptr->name);
					break;
				}
				else if (c_ptr->m_idx)
				{
					monster_type *m_ptr = &m_list[c_ptr->m_idx];
					msg_print("There's something in the way!");

					if (!m_ptr->ml || !is_pet(m_ptr)) py_attack(y, x, 0);
					break;
				}
				else if (have_flag(f_ptr->flags, FF_TREE))
				{
					msg_print("You don't like the woody taste!");
					break;
				}
				else if (have_flag(f_ptr->flags, FF_GLASS))
				{
					msg_print("You don't like the glassy taste!");
					break;
				}
				else if (have_flag(f_ptr->flags, FF_DOOR) || have_flag(f_ptr->flags, FF_CAN_DIG))
				{
					(void)set_food(p_ptr->food + 3000);
				}
				else if (have_flag(f_ptr->flags, FF_MAY_HAVE_GOLD) || have_flag(f_ptr->flags, FF_HAS_GOLD))
				{
					(void)set_food(p_ptr->food + 5000);
				}
				else
				{
					msg_format("This %s is very filling!", f_name + mimic_f_ptr->name);
					(void)set_food(p_ptr->food + 10000);
				}

				/* Destroy the wall */
				cave_alter_feat(y, x, FF_HURT_ROCK);

				/* Move the player */
				(void)move_player_effect(y, x, MPE_DONT_PICKUP);
			}
			break;

		case MUT1_SWAP_POS:
			project_length = -1;
			if (!get_aim_dir(&dir))
			{
				project_length = 0;
				return FALSE;
			}
			(void)teleport_swap(dir);
			project_length = 0;
			break;

		case MUT1_SHRIEK:
			mutation_stop_mouth();
			(void)fire_ball(GF_SOUND, 0, 2 * lvl, 8);
			(void)aggravate_monsters(0);
			break;

		case MUT1_ILLUMINE:
			(void)lite_area(damroll(2, (lvl / 2)), (lvl / 10) + 1);
			break;

		case MUT1_DET_CURSE:
			{
				int i;

				for (i = 0; i < INVEN_TOTAL; i++)
				{
					object_type *o_ptr = &inventory[i];

					if (!o_ptr->k_idx) continue;
					if (!object_is_cursed(o_ptr)) continue;

					o_ptr->feeling = FEEL_CURSED;
				}
			}
			break;

		case MUT1_BERSERK:
			(void)set_shero(randint1(25) + 25, FALSE);
			(void)hp_player(30);
			(void)set_afraid(0, TRUE);
			break;

		case MUT1_POLYMORPH:
			if (!get_check("You will polymorph your self. Are you sure? ")) return FALSE;
			do_poly_self();
			break;

		case MUT1_MIDAS_TCH:
			if (!alchemy()) return FALSE;
			break;

		/* Summon pet molds around the player */
		case MUT1_GROW_MOLD:
			{
				int i;
				for (i = 0; i < 8; i++)
				{
					summon_specific(-1, py, px, lvl, SUMMON_BIZARRE1, PM_FORCE_PET);
				}
			}
			break;

		case MUT1_RESIST:
			{
				int num = lvl / 10;
				int dur = randint1(20) + 20;

				if (randint0(5) < num)
				{
					(void)set_oppose_acid(dur, FALSE);
					num--;
				}
				if (randint0(4) < num)
				{
					(void)set_oppose_elec(dur, FALSE);
					num--;
				}
				if (randint0(3) < num)
				{
					(void)set_oppose_fire(dur, FALSE);
					num--;
				}
				if (randint0(2) < num)
				{
					(void)set_oppose_cold(dur, FALSE);
					num--;
				}
				if (num)
				{
					(void)set_oppose_pois(dur, FALSE);
					num--;
				}
			}
			break;

		case MUT1_EARTHQUAKE:
			(void)earthquake(py, px, 10);
			break;

		case MUT1_EAT_MAGIC:
			if (!eat_magic(p_ptr->lev * 2)) return FALSE;
			break;

		case MUT1_WEIGH_MAG:
			report_magics();
			break;

		case MUT1_STERILITY:
			/* Fake a population explosion. */
			msg_print("You suddenly have a headache!");
			take_hit(DAMAGE_LOSELIFE, randint1(17) + 17, "the strain of forcing abstinence", -1);

			num_repro += MAX_REPRO;
			break;

		case MUT1_PANIC_HIT:
			{
				int x, y;

				if (!get_rep_dir2(&dir)) return FALSE;
				y = py + ddy[dir];
				x = px + ddx[dir];
				if (cave[y][x].m_idx)
				{
					py_attack(y, x, 0);
					if (randint0(p_ptr->skill_dis) < 7)
						msg_print("You failed to teleport.");
					else teleport_player(30, 0L);
				}
				else
				{
					msg_print("You don't see any monster in this direction");

					msg_print(NULL);
				}
			}
			break;

		case MUT1_DAZZLE:
			stun_monsters(lvl * 4);
			confuse_monsters(lvl * 4);
			turn_monsters(lvl * 4);
			break;

		case MUT1_LASER_EYE:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_beam(GF_LITE, dir, 2 * lvl);
			break;

		case MUT1_RECALL:
			if (!word_of_recall()) return FALSE;
			break;

		case MUT1_BANISH:
			{
				int x, y;
				cave_type *c_ptr;
				monster_type *m_ptr;
				monster_race *r_ptr;

				if (!get_rep_dir2(&dir)) return FALSE;
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
				    !(r_ptr->flags1 & RF1_UNIQUE) &&
				    !p_ptr->inside_arena && !p_ptr->inside_quest &&
					(r_ptr->level < randint1(p_ptr->lev+50)) &&
					!(m_ptr->mflag2 & MFLAG2_NOGENO))
				{
					if (record_named_pet && is_pet(m_ptr) && m_ptr->nickname)
					{
						char m_name[80];

						monster_desc(m_name, m_ptr, MD_INDEF_VISIBLE);
						do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_GENOCIDE, m_name);
					}

					/* Delete the monster, rather than killing it. */
					delete_monster_idx(c_ptr->m_idx);
					msg_print("The evil creature vanishes in a puff of sulfurous smoke!");

				}
				else
				{
					msg_print("Your invocation is ineffectual!");

					if (one_in_(13)) m_ptr->mflag2 |= MFLAG2_NOGENO;
				}
			}
			break;

		case MUT1_COLD_TOUCH:
			{
				int x, y;
				cave_type *c_ptr;

				if (!get_rep_dir2(&dir)) return FALSE;
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				if (!c_ptr->m_idx)
				{
					msg_print("You wave your hands in the air.");

					break;
				}
				fire_bolt(GF_COLD, dir, 2 * lvl);
			}
			break;

		/* XXX_XXX_XXX Hack!  MUT1_LAUNCHER is negative, see above */
		case 3: /* MUT1_LAUNCHER */
			/* Gives a multiplier of 2 at first, up to 3 at 40th */
			if (!do_cmd_throw_aux(2 + lvl / 40, FALSE, 0)) return FALSE;
			break;

		default:
			energy_use = 0;
			msg_format("Power %s not implemented. Oops.", power);

	}

	return TRUE;
}
