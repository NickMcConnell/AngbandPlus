/* File: mutation.c */

/* Purpose: Mutation-related code */

/*
 * Note that mutation effects (and prompt displays) are still in cmd2.c
 * under do_cmd_racial_power() - not really worth the effort of extracting
 * the mutations from that function to put here. -- Gumby
 *
 * Functions included here:
 *
 * gain_random_mutation(); lose_random_mutation(); dump_mutations();
 * do_cmd_knowledge_mutations();
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
	{	/* was randint(181) */
		switch(choose_mut ? choose_mut: randint(180))
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
	{	/* was randint(181) */
		switch(choose_mut ? choose_mut: randint(180))
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
                    fprintf(OutFile, " You can emit hard radiation at will (dam lvl*2).\n");
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
