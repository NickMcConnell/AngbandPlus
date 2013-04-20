/* File: mutation.c */

/* Purpose: Mutation-related code (includes racial activations */

/*
 * Functions included here:
 *
 * gain_random_mutation(); lose_mutation(); dump_mutations();
 * do_cmd_knowledge_mutations(); racial_aux(); cmd_racial_power_aux();
 * do_cmd_racial_power();
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

bool gain_random_mutation(int choose_mut)
{
	int attempts_left = 20;
	cptr muta_desc = "";
	bool muta_chosen = FALSE;
	int muta_which = 0;
	u32b * muta_class = 0;

	if (choose_mut) attempts_left = 1;

	while (attempts_left--)
	{	/* randint(180) without grav_beam) */
		switch(choose_mut ? choose_mut: randint(181))
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
                	muta_desc = "You are afflicted by a hallucinatory insanity!";
                	break;
		case 20:
                	muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_FLATULENT;
                	muta_desc = "You become subject to uncontrollable flatulence.";
                	break;
		case 21: case 22:
                	muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_SCOR_TAIL;
                	muta_desc = "You grow a scorpion tail!";
                	break;
		case 23: case 24:
			muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_HORNS;
                	muta_desc = "Horns pop forth into your forehead!";
                	break;
		case 25: case 26:
                	muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_BEAK;
                	muta_desc = "Your mouth turns into a sharp, powerful beak!";
                	break;
		case 27: case 28: case 29:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_HYPER_STR;
                	muta_desc = "Your muscles bulge outrageously!";
                	break;
		case 30: case 31: case 32:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_PUNY;
                	muta_desc = "Your muscles wither away...";
                	break;
		case 34: case 35: case 36:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_HYPER_INT;
                	muta_desc = "Your brain evolves into a living computer!";
                	break;
		case 37: case 38: case 39:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_MORONIC;
                	muta_desc = "Your brain withers away...";
                	break;
		case 40: case 41:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_RESILIENT;
                	muta_desc = "You become extraordinarily resilient.";
                	break;
		case 42: case 43:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_XTRA_FAT;
                	muta_desc = "You become sickeningly fat!";
                	break;
		case 44: case 45:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_ALBINO;
                	muta_desc = "You turn into an albino! You feel frail...";
                	break;
		case 46: case 47: case 48:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_FLESH_ROT;
                	muta_desc = "Your flesh is afflicted by a rotting disease!";
                	break;
		case 49: case 50:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_SILLY_VOI;
                	muta_desc = "Your voice turns into a ridiculous squeak!";
                	break;
		case 51: case 52:
                	muta_class = &(p_ptr->muta1);
                	muta_which = MUT1_RADIATION;
                	muta_desc = "You start emitting hard radiation.";
                	break;
		case 53: case 54:
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
                	muta_which = MUT3_MAGIC_RES;
                	muta_desc = "You become resistant to magic.";
                	break;
		case 60: case 61: case 62:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_XTRA_NOIS;
                	muta_desc = "You start making strange noise!";
                	break;
		case 63: case 64: case 65:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_INFRAVIS;
                	muta_desc = "Your infravision is improved.";
                	break;
		case 66: case 67:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_XTRA_LEGS;
                	muta_desc = "You grow an extra pair of legs!";
                	break;
		case 68: case 69:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_SHORT_LEG;
                	muta_desc = "Your legs turn into short stubs!";
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
                	muta_which = MUT3_WART_SKIN;
                	muta_desc = "Disgusting warts appear everywhere on you!";
                	break;
		case 77: case 78: case 79:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_SCALES;
                	muta_desc = "Your skin turns into black scales!";
                	break;
		case 80: case 81:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_IRON_SKIN;
                	muta_desc = "Your skin turns to steel!";
                	break;
		case 82: case 83:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_WINGS;
                	muta_desc = "You grow a pair of wings.";
                	break;
		case 84: case 85: case 86:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_FEARLESS;
                	muta_desc = "You become completely fearless.";
                	break;
		case 87: case 88:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_REGEN;
                	muta_desc = "You start regenerating.";
                	break;
		case 89: case 90:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_ESP;
                	muta_desc = "You develop a telepathic ability!";
                	break;
		case 91: case 92:
                	muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_ATT_DEMON;
                	muta_desc = "You start attracting demons.";
                	break;
		case 93: case 94:
                	muta_class = &(p_ptr->muta1);
                	muta_which = MUT1_VAMPIRISM;
                	muta_desc = "You become vampiric.";
                	break;
		case 95: case 96:
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
			muta_which = MUT3_TWISTED;
			muta_desc = "Your frame twists into an unnatural shape!";
			break;
		case 102: case 103: case 104:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_LIMBER;
			muta_desc = "Your muscles become limber.";
			break;
		case 105: case 106: case 107:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ARTHRITIS;
			muta_desc = "Your joints suddenly hurt.";
			break;
		case 108:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_VULN_ELEM;
			muta_desc = "You feel strangely exposed.";
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
		case 112: case 113:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_TUSKS;
			muta_desc = "You grow a pair of tusks!";
			break;
		case 114: case 115:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_CLAWS;
			muta_desc = "Your fingers sprout claws!";
			break;
		case 116: case 117:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_DISPEL_ALL;
			muta_desc = "You feel a terrifying power lurking behind you.";
			break;
		case 118:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_EAT_LIGHT;
			muta_desc = "You feel a strange kinship with Ungoliant.";
			break;
		case 119:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RAW_CHAOS;
			muta_desc = "You feel the universe is less stable around you.";
			break;
		case 120:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WRAITH;
			muta_desc = "You start to fade in and out of the physical world.";
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
		case 123: case 124:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WEIRD_MIND;
			muta_desc = "Your thoughts suddenly take off in strange directions.";
			break;
		case 125:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_NAUSEA;
			muta_desc = "Your stomach starts to roil nauseously.";
			break;
		case 126: case 127:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_CHAOS_GIFT;
			muta_desc = "You attract the notice of a Chaos deity!";
			break;
		case 128:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WALK_SHAD;
			muta_desc = "You feel like reality is as thin as paper.";
			break;
		case 129: case 130:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WARNING;
			muta_desc = "You suddenly feel paranoid.";
			break;
		case 131:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_INVULN;
			muta_desc = "You are blessed with fits of invulnerability.";
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
			muta_which = MUT2_DISARM;
			muta_desc = "Your feet grow to four times their former size.";
			break;
		case 136:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SUMMON_M;
			muta_desc = "You feel a sudden affinity for life.";
			break;
		case 137:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_GROW_MOLD;
			muta_desc = "You feel a sudden affinity for mold.";
			break;
		case 138: case 139: case 140:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BLINK;
			muta_desc = "You gain the power of minor teleportation.";
			break;
		case 141: case 142:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EAT_ROCK;
			muta_desc = "The walls look delicious.";
			break;
		case 143: case 144: case 145:
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
			muta_which = MUT1_POLYMORPH;
			muta_desc = "Your body seems mutable.";
			break;
		case 155: case 156:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIDAS_TCH;
			muta_desc = "You gain the Midas Touch.";
			break;
		case 157: case 158: case 159:
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
		case 164: case 165:
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
			muta_which = MUT1_COLD_TOUCH;
			muta_desc = "Your hands get very cold.";
			break;
		case 169: case 170: case 171:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MISSILE;
			muta_desc = "Your hands throb with energy.";
			break;
		case 172:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SHARD_BOLT;
			muta_desc = "A spiked lump rises from your arm.";
			break;
		case 173: case 174: case 175:
			muta_class = &(p_ptr->muta1);
			muta_desc = "Your hands grow knobby protrusions.";
			if(rand_int(2)) muta_which = MUT1_SHARD_BLAST;
			else muta_which = MUT1_DSHARD_BLAST;
			break;
		case 176:
			muta_class = &(p_ptr->muta1);
			muta_desc = "Your shoulders swell oddly!";
			muta_which = MUT1_CHAIN_SHARDS;
			break;
		case 177:
			muta_class = &(p_ptr->muta1);
			muta_desc = "You feel like a Cyberdemon.";
			muta_which = MUT1_ROCKET;
			break;
		case 178:
                	muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_TENTACLES;
                	muta_desc = "You sprout tentacles!";
                	break;
		case 179: case 180:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_GLOW;
                	muta_desc = "Your body starts to shine!";
                	break;
		case 181:
			muta_class = &(p_ptr->muta1);
			muta_desc = "You can focus along a line of gravity.";
			muta_which = MUT1_GRAV_BEAM;
			break;
		default:
                	muta_class = NULL;
                	muta_which = NULL;
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
        msg_print("You feel normal.");
        return FALSE;
    }
    else
    {

        if (p_ptr->prace == RACE_VAMPIRE &&
            !(p_ptr->muta1 & MUT1_HYPN_GAZE) &&
            (randint(10)<7))
        {
            muta_class = &(p_ptr->muta1);
            muta_which = MUT1_HYPN_GAZE;
            muta_desc = "Your eyes look mesmerizing...";
        }

        else if (p_ptr->prace == RACE_IMP &&
            !(p_ptr->muta2 & MUT2_HORNS) &&
            (randint(10)<7))
        {
            muta_class = &(p_ptr->muta2);
            muta_which = MUT2_HORNS;
            muta_desc = "Horns pop forth into your forehead!";
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
                    msg_print("Your brain is no longer a living computer.");
                    p_ptr->muta3 &= ~(MUT3_HYPER_INT);
                }
            }
            else if (muta_which == MUT3_HYPER_INT)
            {
                if (p_ptr->muta3 & MUT3_MORONIC)
                {
                    msg_print("You are no longer moronic.");
                    p_ptr->muta3 &= ~(MUT3_MORONIC);
                }
            }
            else if (muta_which == MUT3_IRON_SKIN)
            {
                if (p_ptr->muta3 & MUT3_SCALES)
                {
                    msg_print("You lose your scales.");
                    p_ptr->muta3 &= ~(MUT3_SCALES);
                }
                if (p_ptr->muta3 & MUT3_FLESH_ROT)
                {
                    msg_print("Your flesh rots no longer.");
                    p_ptr->muta3 &= ~(MUT3_FLESH_ROT);
                }
                if (p_ptr->muta3 & MUT3_WART_SKIN)
                {
                    msg_print("You lose your warts.");
                    p_ptr->muta3 &= ~(MUT3_WART_SKIN);
                }
            }
            else if (muta_which == MUT3_WART_SKIN || muta_which == MUT3_SCALES
                    || muta_which == MUT3_FLESH_ROT)
            {
                if (p_ptr->muta3 & MUT3_IRON_SKIN)
                {
                    msg_print("Your skin is no longer made of steel.");
                    p_ptr->muta3 &= ~(MUT3_IRON_SKIN);
                }
            }
            else if (muta_which == MUT3_FEARLESS)
            {
                if (p_ptr->muta2 & MUT2_COWARDICE)
                {
                    msg_print("You are no longer afraid of the dark.");
                    p_ptr->muta2 &= ~(MUT2_COWARDICE);
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
		}
		else if (muta_which == MUT3_ARTHRITIS)
		{
			if (p_ptr->muta3 & MUT3_LIMBER)
			{
				msg_print("You no longer feel limber.");
				p_ptr->muta3 &= ~(MUT3_LIMBER);
			}
		}
		else if (muta_which == MUT3_RESILIENT)
		{
			if (p_ptr->muta2 & MUT2_WOUND)
			{
				p_ptr->muta2 &= ~(MUT2_WOUND);
			}
		}
        }
	else if (muta_class == &(p_ptr->muta2))
        {
            if (muta_which == MUT2_COWARDICE)
            {
                if (p_ptr->muta3 & MUT3_FEARLESS)
                {
			msg_print("You no longer feel fearless.");
			p_ptr->muta3 &= ~(MUT3_FEARLESS);
                }
            }
	    if (muta_which == MUT2_WOUND)
	    {
		if (p_ptr->muta3 & MUT3_RESILIENT)
		{
			msg_print("You no longer feel resilient.");
			p_ptr->muta3 &= ~(MUT3_RESILIENT);
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
	int attempts_left = 200;
	cptr muta_desc = "";
	bool muta_chosen = FALSE;
	int muta_which = 0;
	u32b * muta_class = 0;

	if (choose_mut) attempts_left = 1;

	while (attempts_left--)
	{	/* randint(180) without grav_beam */
		switch(choose_mut ? choose_mut: randint(181))
		{
		case 1: case 2: case 3: case 4:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SPIT_ACID;
			muta_desc = "You lose the ability to spit acid.";
			break;
		case 5: case 6: case 7:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BR_FIRE;
			muta_desc = "You lose the ability to breathe fire.";
			break;
		case 8: case 9:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_HYPN_GAZE;
			muta_desc = "Your eyes look uninteresting.";
			break;
		case 10: case 11:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_TELEKINES;
			muta_desc = "You lose the ability to move objects telekinetically.";
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
                	muta_desc = "You are no longer afflicted by a hallucinatory insanity!";
                	break;
		case 20:
                	muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_FLATULENT;
                	muta_desc = "You are no longer subject to uncontrollable flatulence.";
                	break;
		case 21: case 22:
                	muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_SCOR_TAIL;
                	muta_desc = "Your scorpion tail falls off!";
                	break;
		case 23: case 24:
			muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_HORNS;
                	muta_desc = "Your horns pop back into your forehead!";
                	break;
		case 25: case 26:
                	muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_BEAK;
                	muta_desc = "Your beak falls off.";
                	break;
		case 27: case 28: case 29:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_HYPER_STR;
                	muta_desc = "Your muscles revert to normal.";
                	break;
		case 30: case 31: case 32:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_PUNY;
                	muta_desc = "Your muscles revert to normal.";
                	break;
		case 34: case 35: case 36:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_HYPER_INT;
                	muta_desc = "Your brain reverts to normal.";
                	break;
		case 37: case 38: case 39:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_MORONIC;
                	muta_desc = "Your brain reverts to normal.";
                	break;
		case 40: case 41:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_RESILIENT;
                	muta_desc = "You are no longer resilient.";
                	break;
		case 42: case 43:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_XTRA_FAT;
                	muta_desc = "You benefit from a miracle diet!";
                	break;
		case 44: case 45:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_ALBINO;
                	muta_desc = "Your skin regains its normal color.";
                	break;
		case 46: case 47: case 48:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_FLESH_ROT;
                	muta_desc = "You are no longer rotting.";
                	break;
		case 49: case 50:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_SILLY_VOI;
                	muta_desc = "You no longer sound like Mickey Mouse.";
                	break;
		case 51: case 52:
                	muta_class = &(p_ptr->muta1);
                	muta_which = MUT1_RADIATION;
                	muta_desc = "You no longer emit hard radiation.";
                	break;
		case 53: case 54:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_BLANK_FAC;
                	muta_desc = "Your nose grows back!";
                	break;
		case 55: case 56: case 57:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_XTRA_EYES;
                	muta_desc = "Your extra eyes fall out!";
                	break;
		case 58: case 59:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_MAGIC_RES;
                	muta_desc = "You are no longer resistant to magic.";
                	break;
		case 60: case 61: case 62:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_XTRA_NOIS;
                	muta_desc = "You stop making strange noises!";
                	break;
		case 63: case 64: case 65:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_INFRAVIS;
                	muta_desc = "Your infravision is back to normal.";
                	break;
		case 66: case 67:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_XTRA_LEGS;
                	muta_desc = "Your extra legs fall off!";
                	break;
		case 68: case 69:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_SHORT_LEG;
                	muta_desc = "Your legs lengthen.";
                	break;
		case 70: case 71:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_ELEC_TOUC;
                	muta_desc = "Electricity stops running through you.";
                	break;
		case 72: case 73:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_FIRE_BODY;
                	muta_desc = "Your flames go out.";
                	break;
		case 74: case 75: case 76:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_WART_SKIN;
                	muta_desc = "You no longer look like a toad.";
                	break;
		case 77: case 78: case 79:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_SCALES;
                	muta_desc = "You shed your scales.";
                	break;
		case 80: case 81:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_IRON_SKIN;
                	muta_desc = "Your steel turns to skin!";
                	break;
		case 82: case 83:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_WINGS;
                	muta_desc = "Your wings fall off.";
                	break;
		case 84: case 85: case 86:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_FEARLESS;
                	muta_desc = "You are no longer fearless.";
                	break;
		case 87: case 88:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_REGEN;
                	muta_desc = "You stop regenerating.";
                	break;
		case 89: case 90:
                	muta_class = &(p_ptr->muta3);
                	muta_which = MUT3_ESP;
                	muta_desc = "You are no longer telepathic!";
                	break;
		case 91: case 92:
                	muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_ATT_DEMON;
                	muta_desc = "You stop attracting demons.";
                	break;
		case 93: case 94:
                	muta_class = &(p_ptr->muta1);
                	muta_which = MUT1_VAMPIRISM;
                	muta_desc = "You no longer thirst for blood.";
                	break;
		case 95: case 96:
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
			muta_which = MUT3_TWISTED;
			muta_desc = "Your frame twists back to normal!";
			break;
		case 102: case 103: case 104:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_LIMBER;
			muta_desc = "Your muscles feel stiff.";
			break;
		case 105: case 106: case 107:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ARTHRITIS;
			muta_desc = "Your joints no longer hurt.";
			break;
		case 108:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_VULN_ELEM;
			muta_desc = "You no longer feel exposed.";
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
		case 112: case 113:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_TUSKS;
			muta_desc = "Your tusks fall out!";
			break;
		case 114: case 115:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_CLAWS;
			muta_desc = "You trim your nails.";
			break;
		case 116: case 117:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_DISPEL_ALL;
			muta_desc = "You no longer feel anything lurking behind you.";
			break;
		case 118:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_EAT_LIGHT;
			muta_desc = "You deny any relationship to Ungoliant.";
			break;
		case 119:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RAW_CHAOS;
			muta_desc = "You feel the universe is more stable around you.";
			break;
		case 120:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WRAITH;
			muta_desc = "You stop fading in and out of the physical world.";
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
		case 123: case 124:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WEIRD_MIND;
			muta_desc = "Your thoughts stop taking off in strange directions.";
			break;
		case 125:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_NAUSEA;
			muta_desc = "Your stomach settles down.";
			break;
		case 126: case 127:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_CHAOS_GIFT;
			muta_desc = "The Chaos deity gets bored with you.";
			break;
		case 128:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WALK_SHAD;
			muta_desc = "Reality feels thick again.";
			break;
		case 129: case 130:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WARNING;
			muta_desc = "You are no longer paranoid.";
			break;
		case 131:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_INVULN;
			muta_desc = "You are no longer blessed with fits of invulnerability.";
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
			muta_which = MUT2_DISARM;
			muta_desc = "Your feet shrink back to normal size.";
			break;
		case 136:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SUMMON_M;
			muta_desc = "You no longer have an affinity for life.";
			break;
		case 137:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_GROW_MOLD;
			muta_desc = "You no longer have an affinity for mold.";
			break;
		case 138: case 139: case 140:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BLINK;
			muta_desc = "You lose the power of minor teleportation.";
			break;
		case 141: case 142:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EAT_ROCK;
			muta_desc = "The walls look unappetizing.";
			break;
		case 143: case 144: case 145:
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
			muta_which = MUT1_POLYMORPH;
			muta_desc = "Your body no loner seems mutable.";
			break;
		case 155: case 156:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIDAS_TCH;
			muta_desc = "You lose the Midas Touch.";
			break;
		case 157: case 158: case 159:
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
		case 164: case 165:
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
			muta_which = MUT1_COLD_TOUCH;
			muta_desc = "Your hands warm up again.";
			break;
		case 169: case 170: case 171:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MISSILE;
			muta_desc = "Your hands stop throbbing.";
			break;
		case 172:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SHARD_BOLT;
			muta_desc = "A spiked lump retreats back into your arm.";
			break;
		case 173: case 174: case 175:
			muta_class = &(p_ptr->muta1);
			muta_desc = "Your hands smooths out.";
			if (p_ptr->muta1 & MUT1_SHARD_BLAST)
				muta_which = MUT1_SHARD_BLAST;
			if (p_ptr->muta1 & MUT1_DSHARD_BLAST)
				 muta_which = MUT1_DSHARD_BLAST;
			break;
		case 176:
			muta_class = &(p_ptr->muta1);
			muta_desc = "Your shoulders are no longer swelling.";
			muta_which = MUT1_CHAIN_SHARDS;
			break;
		case 177:
			muta_class = &(p_ptr->muta1);
			muta_desc = "You no longer feel like a Cyberdemon.";
			muta_which = MUT1_ROCKET;
			break;
		case 178:
                	muta_class = &(p_ptr->muta2);
                	muta_which = MUT2_TENTACLES;
                	muta_desc = "Your tentacles fall off!";
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
                    fprintf(OutFile, " You can emit hard radiation at will (dam lvl*3).\n");
                }
                if (p_ptr->muta1 & MUT1_VAMPIRISM)
                {
                    fprintf(OutFile, " You can drain life from a foe like a vampire.\n");
                }
		if (p_ptr->muta1 & MUT1_SUMMON_M)
		{
			fprintf(OutFile, " You can summon monsters to aid you.\n");
		}
		if (p_ptr->muta1 & MUT1_BLINK)
		{
			fprintf(OutFile, " You can teleport yourself short distances.\n");
		}
		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
			fprintf(OutFile, " You can consume solid rock.\n");
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
		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
			fprintf(OutFile, " You can freeze things with a touch (dam lvl*3).\n");
		}
		if (p_ptr->muta1 & MUT1_MISSILE)
		{
			fprintf(OutFile, " You can cast magical bolts.\n");
		}
		if (p_ptr->muta1 & MUT1_SHARD_BOLT)
		{
			fprintf(OutFile, " You can cast shards.\n");
		}
		if (p_ptr->muta1 & MUT1_SHARD_BLAST)
		{
			fprintf(OutFile, " You can cast volleys of shards.\n");
		}
		if (p_ptr->muta1 & MUT1_DSHARD_BLAST)
		{
			fprintf(OutFile, " You can cast large volleys of shards.\n");
		}
		if (p_ptr->muta1 & MUT1_CHAIN_SHARDS)
		{
			fprintf(OutFile, " You can cast shards rapidly.\n");
		}
		if (p_ptr->muta1 & MUT1_ROCKET)
		{
			fprintf(OutFile, " You can fire rockets (dam lvl*4).\n");
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
		if (p_ptr->muta2 & MUT2_TUSKS)
		{
			fprintf(OutFile, " You have tusks (dam. 2d6).\n");
		}
		if (p_ptr->muta2 & MUT2_CLAWS)
		{
			fprintf(OutFile, " You have claws (dam. 2d3).\n");
		}
		if (p_ptr->muta2 & MUT2_DISPEL_ALL)
		{
			fprintf(OutFile, " You are shrouded in evil.\n");
		}
		if (p_ptr->muta2 & MUT2_EAT_LIGHT)
		{
			fprintf(OutFile, " You sometimes feed off of the light around you.\n");
		}
		if (p_ptr->muta2 & MUT2_RAW_CHAOS)
		{
			fprintf(OutFile, " You occasionally are surrounded with raw chaos.\n");
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
                if (p_ptr->muta2 & MUT2_TENTACLES)
                {
                        fprintf(OutFile, " You have tentacles (slow, 3d3).\n");
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
		if (p_ptr->muta3 & MUT3_SPINES)
		{
			fprintf(OutFile, " Your body is covered with sharp spines.\n");
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
		if (p_ptr->muta3 & MUT3_TWISTED)
		{
			fprintf(OutFile, " Your frame is unnaturally twisted.\n");
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
		if (p_ptr->muta3 & MUT3_GLOW)
		{
			fprintf(OutFile, " Your body is glowing brightly.\n");
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
bool racial_aux(s16b min_level, int cost, int use_stat, int difficulty)
{
	bool use_hp = FALSE;

	/* Use hit points when you don't have enough spell points */
	if (p_ptr->csp < cost) use_hp = TRUE;

	if (p_ptr->lev < min_level)
	{
		msg_format("You need to attain level %d to use this power.", min_level);
		energy_use = 0;
		return FALSE;
	}

	else if (p_ptr->confused)
	{
		msg_print("You are too confused to use this power.");
		energy_use = 0;
		return FALSE;
	}

	else if (use_hp && (p_ptr->chp < cost))
	{
		if (!(get_check("Really use the power in your weakened state? ")))
		{
			energy_use = 0;
			return FALSE;
		}
	}

	/* Else attempt to do it! */

	if (p_ptr->stun) difficulty += p_ptr -> stun;
	else if (p_ptr->lev > min_level)
	{
		int lev_adj = ((p_ptr->lev - min_level)/3);
		if (lev_adj > 10) lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5) difficulty = 5;

	/* take time and pay the price */
	energy_use = 100;
	if (use_hp) take_hit (((cost / 2) + (randint(cost / 2))),
	    "concentrating too hard");
	else p_ptr->csp -= (cost / 2 ) + (randint(cost / 2));

	p_ptr->redraw |= (PR_HP);

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);


	/* Success? */
	if (randint(p_ptr->stat_cur[use_stat]) >=
	    ((difficulty / 2) + randint(difficulty / 2)))
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
	object_type *q_ptr;
	object_type forge;
	int dummy = 0;
	cave_type *c_ptr;
	int y = 0, x = 0;
	int k;    
    
	switch(p_ptr->prace)
	{
		case RACE_HUMAN:
			if (racial_aux(15, 10, A_WIS, 10))
			{
				msg_print("You take stock of your abilities.");
				(void)self_knowledge();
			}
			break;

		case RACE_HALF_ELF:
			if (racial_aux(15, 5, A_WIS, 10))
			{
				msg_print("You look for your animal friends.");
				(void)detect_monsters_xxx(RF3_ANIMAL);
			}
			break;

		case RACE_ELF:
			if (racial_aux(10, 5, A_WIS, 10))
			{
				msg_print("You look for your animal friends.");
				(void)detect_monsters_xxx(RF3_ANIMAL);
			}
			break;

		case RACE_DWARF:
			if (racial_aux(5, 5, A_WIS, 12))
			{
				msg_print("You examine your surroundings.");
				if (p_ptr->lev >= 35) (void)map_area();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
			}
			break;

		case RACE_HOBBIT:
			if (racial_aux(15,10,A_INT,10))
			{
				/* Get local object */
				q_ptr = &forge;

				/* Create the item */
				object_prep(q_ptr, 21);

				/* Drop the object from heaven */
				drop_near(q_ptr, -1, py, px);
				msg_print("You cook some food.");
			}
			break;

		case RACE_GNOME:
			if (racial_aux(5, (5+(plev/5)), A_INT, 12))
			{
				msg_print("Blink!");
				teleport_player(10 + (plev));
			}
			break;

		case RACE_HALF_ORC:
			if (racial_aux(3, 5, A_WIS,
			(p_ptr->pclass == CLASS_WARRIOR?5:10)))
			{
				msg_print("You play tough.");
				(void)set_afraid(0);
			}
			break;

		case RACE_HALF_TROLL:
			if (racial_aux(10, 12, A_WIS,
			(p_ptr->pclass == CLASS_WARRIOR?6:12)))
			{
				msg_print("RAAAGH!");
				(void)set_afraid(0);

				(void)set_shero(p_ptr->shero + 10 + randint(plev));
				(void)hp_player(30);
			}
			break;

		case RACE_GAMBOLT:
			if (racial_aux(20, p_ptr->lev, A_CHR, 15))
			{
				if (plev < 50)
				{
					if (!get_aim_dir(&dir)) break;
					msg_print("You act cute.");
					(void)charm_monster(dir, plev);
				}
				else
				{
					msg_print("You act cute.");
					(void)charm_monsters(plev);
				}
			}
			break;

		case RACE_BARBARIAN:
			if (racial_aux(8, 10, A_WIS, (p_ptr->pclass == CLASS_WARRIOR?6:12)))
			{
				msg_print("RAAARGH!");
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + 10 + randint(plev));
				(void)hp_player(30);
			}
			break;

		case RACE_HALF_OGRE:
			if (racial_aux(25, 35, A_INT, 15))
			{
				msg_print("You carefully set an explosive rune...");
				explosive_rune();
			}
			break;

		case RACE_HALF_GIANT:
			if (racial_aux(20, 10, A_STR, 12))
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

		case RACE_HALF_TITAN:
			if (racial_aux(25, 20, A_INT, 12))
			{
				msg_print("You examine your foes...");
				probing();
			}
			break;

		case RACE_CYCLOPS:
			if (racial_aux(20, 15, A_STR, 12))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You throw a huge boulder.");
				fire_bolt(GF_MISSILE, dir, (3 * p_ptr->lev));
			}
			break;

		case RACE_YEEK:
			if (racial_aux(5, 10, A_WIS, 10))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You make a horrible scream!");
				(void)fear_monster(dir, plev);
			}
			break;

		case RACE_KLACKON:
			if (racial_aux(9, 9, A_DEX, 14))
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
			if (racial_aux(12, 8, A_DEX, 14))
			{
				if(!get_aim_dir(&dir)) break;
				msg_print("You throw a dart of poison.");
				fire_bolt(GF_POIS, dir, plev * 2);
			}
			break;

		case RACE_NIBELUNG:
			if (racial_aux(10, 10, A_WIS, 10))
			{
				msg_print("You examine your surroundings.");
				if (p_ptr->lev >= 35) (void)map_area();
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
			}
			break;

		case RACE_DARK_ELF:
			if (racial_aux(2, 2, A_INT, 9))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You cast a magic missile.");
				fire_bolt_or_beam(10, GF_MISSILE, dir,
				    damroll(3 + ((plev - 1) / 3), 4));
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

			if (racial_aux(1, p_ptr->lev, A_CON, 16))
			{
				if (!get_aim_dir(&dir)) break;
				msg_format("You breathe %s.", Type_desc);
				fire_ball(Type, dir, (plev*3), (plev/15)+1);
			}
			break;
		}

		case RACE_MIND_FLAYER:
			if (racial_aux(15, 12, A_INT, 14))
			{
				if (!get_aim_dir(&dir)) break;
				else
				{
					msg_print("You concentrate and your eyes glow red...");
					fire_bolt(GF_PSI, dir, plev * 2);
				}
			}
			break;

		case RACE_IMP:
			if (racial_aux(9, 15, A_DEX, 15))
			{
				if (!get_aim_dir(&dir)) break;
				if (p_ptr->lev >= 30)
				{
					msg_print("You cast a ball of fire.");
					fire_ball(GF_FIRE, dir, plev * 2, 2);
				}
				else
				{
					msg_print("You cast a bolt of fire.");
					fire_bolt_or_beam(plev * 2, GF_FIRE,
							dir, plev * 2);
				}
			}
			break;

		case RACE_GOLEM:
			if (racial_aux(20, 15, A_CON, 8))
			{
				msg_print("You feel hard.");
				(void)set_shield(p_ptr->shield + randint(20) + 30);
			}
			break;

		case RACE_SKELETON: case RACE_ZOMBIE:
			if (racial_aux(30, 30, A_WIS, 18))
			{
				msg_print("You attempt to restore your lost energies.");
				(void)restore_level();
			}
			break;

		case RACE_VAMPIRE:
			if (racial_aux(2, (1+(plev/3)), A_CON, 9))
			{
				/* Only works on adjacent monsters */
				if (!get_rep_dir(&dir)) break;   /* was get_aim_dir */
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				if (!(c_ptr->m_idx))
				{
					msg_print("You bite into thin air!");
					break;
				}

				msg_print("You grin and bare your fangs...");
				dummy = plev + randint(plev) * MAX(1, plev/10);   /* Dmg */
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

		case RACE_SPECTRE:
			if (racial_aux(4, 6, A_INT, 3))
			{
				msg_print("You emit an eldritch howl!");
				if (!get_aim_dir(&dir)) break;
				(void)fear_monster(dir, plev);
			}
			break;

		case RACE_SPRITE:
			if (racial_aux(8, 12, A_INT, 15))
			{
				msg_print("You throw some magic dust...");
				if (p_ptr->lev < 25)
					sleep_monsters_touch();
				else
					(void)sleep_monsters();
			}
			break;

		default:
			msg_print("This race has no bonus power.");
			energy_use = 0;
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
	bool		flag, redraw;
	int		ask;
	char		choice;
	char		out_val[160];
	int		lvl = p_ptr->lev;
	bool		warrior = ((p_ptr->pclass == CLASS_WARRIOR)?TRUE:FALSE);
	bool		has_racial = FALSE;
	cptr		racial_power = "(none)";

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
			if (lvl < 15)
				racial_power = "self knowledge     (racial, lvl 15, cost 10)";
			else
				racial_power = "self knowledge     (racial, cost 10, WIS 10@15)";
			has_racial = TRUE;
			break;

		case RACE_HALF_ELF:
			if (lvl < 15)
				racial_power = "detect animals     (racial, lvl 15, cost 5)";
			else
				racial_power = "detect animals     (racial, cost 5, WIS 10@15)";
			has_racial = TRUE;
			break;

		case RACE_ELF:
			if (lvl < 10)
				racial_power = "detect animals     (racial, lvl 10, cost 5)";
			else
				racial_power = "detect animals     (racial, cost 5, WIS 10@10)";
			has_racial = TRUE;
			break;

		case RACE_DWARF:
			if (lvl < 5)
				racial_power = "sense surroundings (racial, lvl 5, cost 5)";
			else
				racial_power = "sense surroundings (racial, cost 5, WIS 12@5)";
			has_racial = TRUE;
			break;

		case RACE_NIBELUNG:
			if (lvl < 10)
				racial_power = "sense surroundings (racial, lvl 10, cost 10)";
			else
				racial_power = "sense surroundings (racial, cost 10, WIS 12@10)";
			has_racial = TRUE;
			break;

		case RACE_HOBBIT:
			if (lvl < 15)
				racial_power = "create food        (racial, lvl 15, cost 10)";
			else
				racial_power = "create food        (racial, cost 10, INT 10@15)";
			has_racial = TRUE;
			break;

		case RACE_GNOME:
			if (lvl < 5)
				racial_power = "teleport           (racial, lvl 5, cost 5 + lvl/5)";
			else
				racial_power = "teleport           (racial, cost 5 + lvl/5, INT 12@5)";
			has_racial = TRUE;
			break;

		case RACE_HALF_ORC:
			if (lvl < 3)
				racial_power = "remove fear        (racial, lvl 3, cost 5)";
			else if (warrior)
				racial_power = "remove fear        (racial, cost 5, WIS 5@3)";
			else
				racial_power = "remove fear        (racial, cost 5, WIS 10@3)";
			has_racial = TRUE;
			break;

		case RACE_HALF_TROLL:
			if (lvl < 10)
				racial_power = "berserk            (racial, lvl 10, cost 12)";
			else if (warrior)
				racial_power = "berserk            (racial, cost 12, WIS 6@10)";
			else
				racial_power = "berserk            (racial, cost 12, WIS 12@10)";
			has_racial = TRUE;
			break;

		case RACE_BARBARIAN:
			if (lvl < 8)
				racial_power = "berserk            (racial, lvl 8, cost 10)";
			else if (warrior)
				racial_power = "berserk            (racial, cost 10, WIS 6@8)";
			else
				racial_power = "berserk            (racial, cost 10, WIS 12@8)";
			has_racial = TRUE;
			break;

		case RACE_GAMBOLT:
			if (lvl < 20)
				racial_power = "charm              (racial, lvl 20, cost lvl)";
			else
				racial_power = "charm              (racial, cost lvl, CHR, 15@20)";
			has_racial = TRUE;
			break;

		case RACE_HALF_OGRE:
			if (lvl < 25)
				racial_power = "explosive rune     (racial, lvl 25, cost 35)";
			else
				racial_power = "explosive rune     (racial, cost 35, INT 15@25)";
			has_racial = TRUE;
			break;

		case RACE_HALF_GIANT:
			if (lvl < 20)
				racial_power = "smash down a wall  (racial, lvl 20, cost 10)";
			else
				racial_power = "smash down a wall  (racial, cost 10, STR 12@20)";
			has_racial = TRUE;
			break;

		case RACE_HALF_TITAN:
			if (lvl < 35)
				racial_power = "probing            (racial, lvl 25, cost 20)";
			else
				racial_power = "probing            (racial, cost 20, INT 12@25)";
			has_racial = TRUE;
			break;

		case RACE_CYCLOPS:
			if (lvl < 20)
				racial_power = "throw boulder      (racial, lvl 20, cost 15, dam 3*lvl)";
			else
				racial_power = "throw boulder      (racial, cost 15, dam 3*lvl, STR 12@20)";
			has_racial = TRUE;
			break;

		case RACE_YEEK:
			if (lvl < 5)
				racial_power = "scare monster      (racial, lvl 5, cost 10)";
			else
				racial_power = "scare monster      (racial, cost 10, WIS 10@5)";
			has_racial = TRUE;
			break;

		case RACE_SPECTRE:
			if (lvl < 4)
				racial_power = "scare monster      (racial, lvl 4, cost 3)";
			else
				racial_power = "scare monster      (racial, cost 3, INT 3@5)";
			has_racial = TRUE;
			break;

		case RACE_KLACKON:
			if (lvl < 9)
				racial_power = "spit acid          (racial, lvl 9, cost 9, dam lvl*2)";
			else
				racial_power = "spit acid          (racial, cost 9, dam lvl*2, DEX 14@9)";
			has_racial = TRUE;
			break;

		case RACE_KOBOLD:
			if (lvl < 12)
				racial_power = "poison dart        (racial, lvl 12, cost 8, dam lvl*2)";
			else
				racial_power = "poison dart        (racial, cost 8, dam lvl*2, DEX 14@12)";
			has_racial = TRUE;
			break;

		case RACE_DARK_ELF:
			if (lvl < 2)
				racial_power = "magic missile      (racial, lvl 2, cost 2)";
			else
				racial_power = "magic missile      (racial, cost 2, INT 9@2)";
			has_racial = TRUE;
			break;

		case RACE_DRACONIAN:
			racial_power = "breath weapon              (racial, cost lvl, dam lvl*3, CON 16@1)";
			has_racial = TRUE;
			break;

		case RACE_MIND_FLAYER:
			if (lvl < 15)
				racial_power = "mind blast         (racial, lvl 15, cost 12, dam lvl*2)";
			else
				racial_power = "mind blast         (racial, cost 12, dam lvl*2, INT 14@15)";
			has_racial = TRUE;
			break;

		case RACE_IMP:
			if (lvl < 9)
				racial_power = "fire bolt/ball     (racial, lvl 9/30, cost 15, dam lvl*2)";
			else
				racial_power = "fire bolt/ball(30) (racial, cost 15, dam lvl*2, DEX 15@9)";
			has_racial = TRUE;
			break;

		case RACE_GOLEM:
			if (lvl < 20)
				racial_power = "stone skin         (racial, lvl 20, cost 15, dur 30+d20)";
			else
				racial_power = "stone skin         (racial, cost 15, dur 30+d20, CON 8@20)";
			has_racial = TRUE;
			break;

		case RACE_SKELETON: case RACE_ZOMBIE:
			if (lvl < 30)
				racial_power = "restore life       (racial, lvl 30, cost 30)";
			else
				racial_power = "restore life       (racial, cost 30, WIS 18@30)";
			has_racial = TRUE;
			break;

		case RACE_VAMPIRE:
			if (lvl < 2)
				racial_power = "drain life         (racial, lvl 2, cost 1 + lvl/3) ";
			else
				racial_power = "drain life         (racial, cost 1 + lvl/3, CON 9@2)";
			has_racial = TRUE;
			break;

		case RACE_SPRITE:
			if (lvl < 8)
				racial_power = "sleeping dust      (racial, lvl 8, cost 12)";
			else
				racial_power = "sleeping dust      (racial, cost 12, INT 15@8)";
			has_racial = TRUE;
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
		num++;
	}

	if (p_ptr->muta1)
	{
		int lvl = p_ptr->lev;

		if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
			if (lvl < 9)
				strcpy(power_desc[num],"spit acid          (mutation, lvl 9, cost 9, dam lvl*2)");
			else
				strcpy(power_desc[num],"spit acid          (mutation, cost 9, dam lvl*2, DEX 15@9)");
			powers[num++] = MUT1_SPIT_ACID;
		}

		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"fire breath        (mutation, lvl 20, cost lvl, dam lvl*3)");
			else
				strcpy(power_desc[num],"fire breath        (mutation, cost lvl, dam lvl*3, CON 18@20)");
			powers[num++] = MUT1_BR_FIRE;
		}

		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"hypnotic gaze      (mutation, lvl 12, cost 12)");
			else
				strcpy(power_desc[num],"hypnotic gaze      (mutation, cost 12, CHR 18@12)");
			powers[num++] = MUT1_HYPN_GAZE;
		}

		if (p_ptr->muta1 & MUT1_TELEKINES)
		{
			if (lvl < 9)
				strcpy(power_desc[num],"telekinesis        (mutation, lvl 9, cost 9)");
			else
				strcpy(power_desc[num],"telekinesis        (mutation, cost 9, WIS 14@9)");
			powers[num++] = MUT1_TELEKINES;
		}

		if (p_ptr->muta1 & MUT1_VTELEPORT)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"teleport           (mutation, lvl 7, cost 7)");
			else
				strcpy(power_desc[num],"teleport           (mutation, cost 7, WIS 15@7)");
			powers[num++] = MUT1_VTELEPORT;
		}

		if (p_ptr->muta1 & MUT1_MIND_BLST)
		{
			if (lvl < 5)
				strcpy(power_desc[num],"mind blast         (mutation, lvl 5, cost 3)");
			else
				strcpy(power_desc[num],"mind blast         (mutation, cost 3, WIS 15@7)");
			powers[num++] = MUT1_MIND_BLST;
		}

		if (p_ptr->muta1 & MUT1_RADIATION)
		{
			if (lvl < 15)
				strcpy(power_desc[num],"emit radiation     (mutation, lvl 15, cost 15, dam lvl*3)");
			else
				strcpy(power_desc[num],"emit radiation     (mutation, cost 15, dam lvl*3 CON 14@15)");
			powers[num++] = MUT1_RADIATION;
		}

		if (p_ptr->muta1 & MUT1_VAMPIRISM)
		{
			if (lvl < 13)
				strcpy(power_desc[num],"vampiric drain     (mutation, lvl 13, cost lvl)");
			else
				strcpy(power_desc[num],"vampiric drain     (mutation, cost lvl, CON 14@13)");
			powers[num++] = MUT1_VAMPIRISM;
		}

		if (p_ptr->muta1 & MUT1_BLINK)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"blink              (mutation, lvl 3, cost 3)");
			else
				strcpy(power_desc[num],"blink              (mutation, cost 3, WIS 12@3)");
			powers[num++] = MUT1_BLINK;
		}

		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
			if (lvl < 8)
				strcpy(power_desc[num],"eat rock           (mutation, lvl 8, cost 12)");
			else
				strcpy(power_desc[num],"eat rock           (mutation, cost 12, CON 18@8)");
			powers[num++] = MUT1_EAT_ROCK;
		}

		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"shriek             (mutation, lvl 20, cost 14, dam lvl*3)");
			else
				strcpy(power_desc[num],"shriek             (mutation, cost 14, dam lvl*3, CON 16@20)");
			powers[num++] = MUT1_SHRIEK;
		}

		if (p_ptr->muta1 & MUT1_ILLUMINE)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"illuminate         (mutation, lvl 3, cost 2)");
			else
				strcpy(power_desc[num],"illuminate         (mutation, cost 2, INT 10@3)");
			powers[num++] = MUT1_ILLUMINE;
		}

		if (p_ptr->muta1 & MUT1_DET_CURSE)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"detect curses      (mutation, lvl 7, cost 14)");
			else
				strcpy(power_desc[num],"detect curses      (mutation, cost 14, WIS 14@7)");
			powers[num++] = MUT1_DET_CURSE;
		}

		if (p_ptr->muta1 & MUT1_BERSERK)
		{
			if (lvl < 8)
				strcpy(power_desc[num],"berserk            (mutation, lvl 8, cost 8)");
			else
				strcpy(power_desc[num],"berserk            (mutation, cost 8, STR 14@8)");
			powers[num++] = MUT1_BERSERK;
		}

		if (p_ptr->muta1 & MUT1_POLYMORPH)
		{
			if (lvl < 18)
				strcpy(power_desc[num],"polymorph          (mutation, lvl 18, cost 20)");
			else
				strcpy(power_desc[num],"polymorph          (mutation, cost 20, CON 18@18)");
			powers[num++] = MUT1_POLYMORPH;
		}

		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"Midas touch        (mutation, lvl 20, cost 15)");
			else
				strcpy(power_desc[num],"Midas touch        (mutation, cost 15, INT 12@20)");
			powers[num++] = MUT1_MIDAS_TCH;
		}

		if (p_ptr->muta1 & MUT1_SUMMON_M)
		{
			if (lvl < 10)
				strcpy(power_desc[num],"summon monsters    (mutation, lvl 10, cost lvl / 2)");
			else
				strcpy(power_desc[num],"summon monsters    (mutation, cost lvl / 2, CON 10@10)");
			powers[num++] = MUT1_SUMMON_M;
		}

		if (p_ptr->muta1 & MUT1_GROW_MOLD)
		{
			strcpy(power_desc[num],"grow molds         (mutation, cost 6, CON 14@1)");
			powers[num++] = MUT1_GROW_MOLD;
		}

		if (p_ptr->muta1 & MUT1_RESIST)
		{
			if (lvl < 10)
				strcpy(power_desc[num],"resist elements    (mutation, lvl 10, cost 12)");
			else
				strcpy(power_desc[num],"resist elements    (mutation, cost 12, CON 12@10)");
			powers[num++] = MUT1_RESIST;
		}

		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
			if (lvl < 12)
				strcpy(power_desc[num],"earthquake         (mutation, lvl 12, cost 12)");
			else
				strcpy(power_desc[num],"earthquake         (mutation, cost 12, STR 16@12)");
			powers[num++] = MUT1_EARTHQUAKE;
		}

		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"dazzle             (mutation, lvl 7, cost 15)");
			else
				strcpy(power_desc[num],"dazzle             (mutation, cost 15, CHR 8@7)");
			powers[num++] = MUT1_DAZZLE;
		}

		if (p_ptr->muta1 & MUT1_RECALL)
		{
			if (lvl < 17)
				strcpy(power_desc[num],"word of recall     (mutation, lvl 17, cost 50)");
			else
				strcpy(power_desc[num],"word of recall     (mutation, cost 50, INT 16@17)");
			powers[num++] = MUT1_RECALL;
		}

		if (p_ptr->muta1 & MUT1_BANISH)
		{
			if (lvl < 25)
				strcpy(power_desc[num],"banish evil        (mutation, lvl 25, cost 25)");
			else
				strcpy(power_desc[num],"banish evil        (mutation, cost 25, WIS 18@25)");
			powers[num++] = MUT1_BANISH;
		}

		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
			if (lvl < 2)
				strcpy(power_desc[num],"cold touch         (mutation, lvl 2, cost 2, dam lvl*3)");
			else
				strcpy(power_desc[num],"cold touch         (mutation, cost 2, dam lvl*3, CON 11@2)");
			powers[num++] = MUT1_COLD_TOUCH;
		}

		if (p_ptr->muta1 & MUT1_MISSILE)
		{
			strcpy(power_desc[num],"magic missile      (mutation, cost 1, CON 5@1)");
			powers[num++] = MUT1_MISSILE;
		}

		if (p_ptr->muta1 & MUT1_SHARD_BOLT)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"shard bolt         (mutation, lvl 3, cost 2)");
			else
				strcpy(power_desc[num],"shard bolt         (mutation, cost 2, CON 7@3)");
			powers[num++] = MUT1_SHARD_BOLT;
		}

		if (p_ptr->muta1 & MUT1_SHARD_BLAST)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"shard blast        (mutation, lvl 7, cost 4)");
			else
				strcpy(power_desc[num],"shard blast        (mutation, cost 4, STR 10@7)");
			powers[num++] = MUT1_SHARD_BLAST;
		}

		if (p_ptr->muta1 & MUT1_DSHARD_BLAST)
		{
			if (lvl < 14)
				strcpy(power_desc[num],"large shard blast  (mutation, lvl 14, cost 8)");
			else
				strcpy(power_desc[num],"large shard blast  (mutation, cost 8, CON 12@14)");
			powers[num++] = MUT1_DSHARD_BLAST;
		}

		if (p_ptr->muta1 & MUT1_CHAIN_SHARDS)
		{
			if (lvl < 17)
				strcpy(power_desc[num],"rapid shards       (mutation, lvl 17, cost 10)");
			else
				strcpy(power_desc[num],"rapid shards       (mutation, cost 10, STR 16@17)");
			powers[num++] = MUT1_CHAIN_SHARDS;
		}

		if (p_ptr->muta1 & MUT1_ROCKET)
		{
			if (lvl < 21)
				strcpy(power_desc[num],"rocket             (mutation, lvl 21, cost 15, dam lvl*4)");
			else
				strcpy(power_desc[num],"rocket             (mutation, cost 15, dam lvl*4, STR 18@21)");
			powers[num++] = MUT1_ROCKET;
		}

		if (p_ptr->muta1 & MUT1_GRAV_BEAM)
		{
			if (lvl < 30)
				strcpy(power_desc[num],"gravity beam       (mutation, lvl 30, cost 20)");
			else
				strcpy(power_desc[num],"gravity beam       (mutation, cost 20, CON 18@30)");
			powers[num++] = MUT1_GRAV_BEAM;
		}
	}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
	    I2A(0), I2A(num - 1));

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
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
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
				if (racial_aux(9, 9, A_DEX, 15))
				{
					msg_print("You spit acid...");
					if (get_aim_dir(&dir))
						fire_ball(GF_ACID, dir, p_ptr->lev * 2, 1 + (p_ptr->lev/30));
				}
				break;

			case MUT1_BR_FIRE:
				if (racial_aux(20, p_ptr->lev, A_CON, 18))
				{
					msg_print("You breathe fire...");
					if (get_aim_dir(&dir))
						fire_ball(GF_FIRE, dir, p_ptr->lev * 3, 1 + (p_ptr->lev/20));
				}
				break;

			case MUT1_HYPN_GAZE:
				if (racial_aux(12, 12, A_CHR, 18))
				{
					msg_print("Your eyes look mesmerizing...");
					if (get_aim_dir(&dir))
						(void) charm_monster(dir, p_ptr->lev);
				}
				break;

			case MUT1_TELEKINES:
				if (racial_aux(9, 9, A_WIS, 14))
				{
					msg_print("You concentrate...");
					if (get_aim_dir(&dir))
						fetch(dir, p_ptr->lev * 10, TRUE);
				}
				break;

			case MUT1_VTELEPORT:
				if (racial_aux(7, 7, A_WIS, 15))
				{
					msg_print("Blink!");
					teleport_player(10 + (p_ptr->lev));
				}
				break;

			case MUT1_MIND_BLST:
				if (racial_aux(5, 3, A_WIS, 15))
				{
					msg_print("You concentrate...");
					if (!get_aim_dir(&dir)) return;
					fire_bolt(GF_PSI, dir, damroll(3 + ((p_ptr->lev - 1) / 3), 4));
				}
				break;

			case MUT1_RADIATION:
				if (racial_aux(15, 15, A_CON, 14))
				{
					msg_print("Radiation flows from your body!");
					fire_ball(GF_NUKE, 0, (p_ptr->lev * 3), 3 + (p_ptr->lev / 20));
				}
				break;

			case MUT1_VAMPIRISM:
				if (racial_aux(13, p_ptr->lev, A_CON, 14))
				{
					if (!get_aim_dir(&dir)) return;
					if (drain_life(dir, (p_ptr->lev * 2)))
						hp_player(p_ptr->lev + randint(p_ptr->lev));
				}
				break;

			case MUT1_BLINK:
				if (racial_aux(3, 3, A_WIS, 12))
				{
					teleport_player(10);
				}
				break;

			case MUT1_EAT_ROCK:
				if (racial_aux(8, 12, A_CON, 18))
				{
					int x, y, ox, oy;
					cave_type *c_ptr;

					if (!get_rep_dir(&dir)) break;
					y = py + ddy[dir];
					x = px + ddx[dir];
					c_ptr = &cave[y][x];
					if (cave_floor_bold(y, x))
					{
						msg_print("You bite into thin air!");
						break;
					}
					else if ((c_ptr->feat >= FEAT_PERM_EXTRA) &&
						(c_ptr->feat <= FEAT_PERM_SOLID))
					{
						msg_print("Ouch!  This wall is harder than your teeth!");
						break;
					}
					else if (c_ptr->m_idx)
					{
						msg_print("There's something in the way!");
						break;
					}
					else
					{
						if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
							(c_ptr->feat <= FEAT_RUBBLE))
						{
							msg_print("It could use some salt.");
							(void)set_food(p_ptr->food + 500);
						}
						else if ((c_ptr->feat >= FEAT_MAGMA) &&
							(c_ptr->feat <= FEAT_QUARTZ_K))
						{
							msg_print("This stuff's quite tasty.");
							(void)set_food(p_ptr->food + 1500);
						}
						else
						{
							msg_print("*MUNCH*  *MUNCH*  *MUNCH*  Yummy!");
							(void)set_food(p_ptr->food + 3000);
						}
					}
					(void)wall_to_mud(dir);
				
					oy = py;
					ox = px;
				
					py = y;
					px = x;

					lite_spot(py, px);
					lite_spot(oy, ox);

					verify_panel();

					p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);
					p_ptr->update |= (PU_DISTANCE);
					p_ptr->window |= (PW_OVERHEAD);
				}
				break;

			case MUT1_SHRIEK:
				if (racial_aux(20, 14, A_CON, 16))
				{
					(void)fire_ball(GF_SOUND, 0, 3 * lvl, 8);
					(void)aggravate_monsters(0);
				}
				break;

			case MUT1_ILLUMINE:
				if (racial_aux(3, 2, A_INT, 10))
				{
					(void)lite_area(damroll(2, (lvl / 2)), (lvl / 10) + 1);
				}
				break;

			case MUT1_DET_CURSE:
				if (racial_aux(7, 14, A_WIS, 14))
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
				if (racial_aux(8, 8, A_STR, 14))
				{
					(void)set_shero(p_ptr->shero + randint(25) + 25);
					(void)hp_player(30);
					(void)set_afraid(0);
				}
				break;

			case MUT1_POLYMORPH:
				if (racial_aux(18, 20, A_CON, 18))
				{
					do_poly_self();
				}
				break;

			case MUT1_MIDAS_TCH:
				if (racial_aux(20, 15, A_INT, 12))
				{
					(void)alchemy();
				}
				break;

			/* Summon pet monsters around the player */
			case MUT1_SUMMON_M:
				if (racial_aux(10, p_ptr->lev / 2, A_CON, 10))
				{
					summon_specific_friendly(py, px, lvl, SUMMON_NO_UNIQUES, TRUE);
				}
				break;

			/* Summon pet molds around the player */
			case MUT1_GROW_MOLD:
				if (racial_aux(1, 6, A_CON, 14))
				{
					int i;
					for (i = 0; i < 8; i++)
					{
						summon_specific_friendly(py, px, lvl, SUMMON_BIZARRE1, FALSE);
					}
				}
				break;

			case MUT1_RESIST:
				if (racial_aux(10, 12, A_CON, 12))
				{
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
				}
				break;

			case MUT1_EARTHQUAKE:
				if (racial_aux(12, 12, A_STR, 16))
				{
					msg_print("You put your foot down... Hard!");
					earthquake(py, px, 10);
				}
				break;

			case MUT1_DAZZLE:
				if (racial_aux(7, 15, A_CHR, 8))
				{
					stun_monsters(lvl * 4);
					confuse_monsters(lvl * 4);
					turn_monsters(lvl * 4);
				}
				break;

			case MUT1_RECALL:
				if (racial_aux(17, 50, A_INT, 16))
				{
					/* Astral beings don't WoR! -- G */
					if (p_ptr->astral)
					{
						msg_print("You feel a terrible sense of loss.");
						break;
					}

					if (dun_level && (p_ptr->max_dlv > dun_level))
					{
						if (get_check("Reset recall depth? "))
							p_ptr->max_dlv = dun_level;
					}

					if (!p_ptr->word_recall)
					{
						p_ptr->word_recall = rand_int(21) + 15;
						msg_print("The air about you becomes charged...");
					}
					else
					{
						p_ptr->word_recall = 0;
						msg_print("A tension leaves the air around you...");
					}
				}
				break;

			case MUT1_BANISH:
				if (racial_aux(25, 25, A_WIS, 18))
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

			case MUT1_COLD_TOUCH:
				if (racial_aux(2, 2, A_CON, 11))
				{
					int x,y;
					cave_type *c_ptr;

					if (!get_rep_dir(&dir)) return;
					y = py + ddy[dir];
					x = px + ddx[dir];
					c_ptr = &cave[y][x];

					if (!c_ptr->m_idx)
					{
						msg_print("You wave your hands in the air.");
						break;
					}
					fire_bolt(GF_COLD, dir, 3 * lvl);
				}
				break;

			case MUT1_MISSILE:
				if (racial_aux(1, 1, A_CON, 5))
				{
					msg_print("You cast a magic missile...");
					if (get_aim_dir(&dir))
						fire_bolt(GF_MISSILE, dir, damroll(3 + ((lvl - 1) / 3), 4));
				}
				break;

			case MUT1_SHARD_BOLT:
				if (racial_aux(3, 2, A_CON, 7))
				{
					msg_print("You cast a stinging shard...");
					if (get_aim_dir(&dir))
						fire_bolt(GF_SHARDS, dir, damroll(3 + (lvl / 5), 5));
				}
				break;

			case MUT1_SHARD_BLAST:
				if (racial_aux(7, 4, A_CON, 10))
				{
					msg_print("You cast a volley of shards...");
					if (get_aim_dir(&dir))
						fire_blast(GF_SHARDS, dir, 2 + (lvl / 5), 4, 5, 3);
				}
				break;

			case MUT1_DSHARD_BLAST:
				if (racial_aux(14, 8, A_CON, 12))
				{
					msg_print("You cast a volley of shards...");
					if (get_aim_dir(&dir))
						fire_blast(GF_SHARDS, dir, 2 + (lvl / 5), 4, 10, 4);
				}
				break;

			case MUT1_CHAIN_SHARDS:
				if (racial_aux(17, 10, A_STR, 16))
				{
					msg_print("You launch a barrage of shards...");
					if (get_aim_dir(&dir))
						fire_blast(GF_SHARDS, dir, 3 + (lvl / 5), 5, 10, 2);
				}
				break;

			case MUT1_ROCKET:
				if (racial_aux(21, 15, A_STR, 18))
				{
					msg_print("You fire a rocket...");
					if (get_aim_dir(&dir))
						fire_ball(GF_SHARDS, dir, lvl * 4, 2);
				}
				break;
			case MUT1_GRAV_BEAM:
				if (racial_aux(30, 20, A_CON, 18))
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

