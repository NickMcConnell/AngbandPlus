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
 *
 */
 /* 
  * I stole this from gumby. It's the way I want it (everything 
  * in this one file) but now I have to update it to the current
  * Z style. The reason I stole it from gumby is that the mutations
  * and racial activations are both defined in this file. And no Lua.
  * It's not like I need _another_ layer of complexity while learning
  * to code. -ccc
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
	int 	attempts_left = 30;
	cptr	muta_desc = "";
	bool	muta_chosen = FALSE;
	u32b 	muta_which = 0;
	u32b	*muta_class = 0;

	if (choose_mut) attempts_left = 1;

	while (attempts_left--)
	{	
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
			muta_which = MUT1_APPORTATION;
			muta_desc = "You gain the ability to teleport objects.";
			break;
		case 12: case 13: case 14:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_VTELEPORT;
			muta_desc = "You gain the power of teleportation at will.";
			break;
		case 15:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_BERS_RAGE;
			muta_desc = "You become subject to fits of berserk rage!";
			break;
		case 16:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_COWARDICE;
			muta_desc = "You become an incredible coward!";
			break;
		case 17:
			muta_class = &(p_ptr->muta3);
           	muta_which = MUT3_RTELEPORT;
            muta_desc = "Your position seems very uncertain...";
            break;
		case 18:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_ALCOHOL;
            muta_desc = "Your body starts producing alcohol!";
            break;
		case 19:
			muta_class = &(p_ptr->muta3);
            muta_which = MUT3_HALLU;
            muta_desc = "You are afflicted by a hallucinatory insanity!";
            break;
		case 20:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_FLATULENT;
            muta_desc = "You become subject to uncontrollable flatulence.";
            break;
		case 21: case 22:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_SCOR_TAIL;
            muta_desc = "You grow a scorpion tail!";
            break;
		case 23: case 24:
			muta_class = &(p_ptr->muta3);
            muta_which = MUT3_HORNS;
            muta_desc = "Horns pop forth into your forehead!";
            break;
		case 25: case 26:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_BEAK;
            muta_desc = "Your mouth turns into a sharp, powerful beak!";
            break;
		case 27: case 28: case 29:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_HYPER_STR;
            muta_desc = "Your muscles bulge outrageously!";
            break;
		case 30: case 31: case 32:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_PUNY;
            muta_desc = "Your muscles wither away...";
            break;
		case 34: case 35: case 36:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_HYPER_INT;
            muta_desc = "Your brain evolves into a living computer!";
            break;
		case 37: case 38: case 39:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_MORONIC;
            muta_desc = "Your brain withers away...";
            break;
		case 40: case 41:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_RESILIENT;
            muta_desc = "You become extraordinarily tough.";
            break;
		case 42: case 43:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_FAT;
            muta_desc = "You become sickeningly fat!";
            break;
		case 44: case 45:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ALBINO;
            muta_desc = "You turn into an albino! You feel frail...";
            break;
		case 46: case 47: case 48:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FLESH_ROT;
            muta_desc = "Your flesh is afflicted by a rotting disease!";
            break;
		case 49: case 50:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SILLY_VOI;
            muta_desc = "Your voice turns into a ridiculous squeak!";
            break;
		case 51: case 52:
            muta_class = &(p_ptr->muta1);
            muta_which = MUT1_RADIATION;
            muta_desc = "You start emitting hard radiation.";
            break;
		case 53: case 54:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_BLANK_FAC;
            muta_desc = "Your face becomes completely featureless!";
            break;
		case 55: case 56: case 57:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_EYES;
            muta_desc = "You grow an extra pair of eyes!";
            break;
		case 58: case 59:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_MAGIC_RES;
            muta_desc = "You become resistant to magic.";
            break;
		case 60: case 61: case 62:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_NOIS;
            muta_desc = "You start making strange noise!";
            break;
		case 63: case 64: case 65:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_INFRAVIS;
            muta_desc = "Your infravision is improved.";
            break;
		case 66: case 67:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_LEGS;
            muta_desc = "You grow an extra pair of legs!";
            break;
		case 68: case 69:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SHORT_LEG;
            muta_desc = "Your legs turn into short stubs!";
            break;
		case 70: case 71:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ELEC_TOUC;
            muta_desc = "Electricity starts running through you!";
            break;
		case 72: case 73:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FIRE_BODY;
            muta_desc = "Your body is enveloped in flames!";
            break;
		case 74: case 75: case 76:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_WART_SKIN;
            muta_desc = "Disgusting warts appear everywhere on you!";
           break;
 		case 77: case 78: case 79:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SCALES;
            muta_desc = "Your skin turns into black scales!";
            break;
		case 80: case 81:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_IRON_SKIN;
            muta_desc = "Your skin turns to iron!";
            break;
		case 82: case 83:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_WINGS;
            muta_desc = "You grow a pair of wings.";
            break;
		case 84: case 85: case 86:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FEARLESS;
            muta_desc = "You become completely fearless.";
            break;
		case 87: case 88:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_REGEN;
            muta_desc = "You start regenerating.";
            break;
		case 89: case 90:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ESP;
            muta_desc = "You develop a telepathic ability!";
            break;
		case 91: case 92:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_ATT_DEMON;
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
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_PROD_MANA;
            muta_desc = "You start producing magical energy uncontrollably.";
            break;
		case 98:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_ILL_NORM;
			muta_desc = "You start projecting a reassuring image.";
			break;
		case 99: case 100:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_SPINES;
			muta_desc = "You grow a fearsome covering of sharp spines!";
			break;
		case 101:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_TWISTED;
			muta_desc = "Your frame twists into an unnatural shape!";
			break;
		case 102: case 103: case 104:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_LIMBER;
			muta_desc = "Your muscles become limber.";
			break;
		case 105: case 106: case 107:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_ARTHRITIS;
			muta_desc = "Your joints suddenly hurt.";
			break;
		case 108:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_VULN_ELEM;
			muta_desc = "You feel strangely exposed.";
			break;
		case 109:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ATT_ANIMAL;
			muta_desc = "You start attracting animals.";
			break;
		case 110:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ATT_DRAGON;
			muta_desc = "You start attracting dragons.";
			break;
		case 111:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WOUND;
			muta_desc = "Your flesh feels weak.";
			break;
		case 112: case 113:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_TUSKS;
			muta_desc = "You grow a pair of tusks!";
			break;
		case 114: case 115:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_CLAWS;
			muta_desc = "Your fingers sprout claws!";
			break;
		case 116: case 117:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_DISPEL_ALL;
			muta_desc = "You feel a terrifying power lurking behind you.";
			break;
		case 118:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_EAT_LIGHT;
			muta_desc = "You feel a strange kinship with the night.";
			break;
		case 119:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_RAW_CHAOS;
			muta_desc = "You feel the universe is less stable around you.";
			break;
		case 120:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WRAITH;
			muta_desc = "You start to fade in and out of the physical world.";
			break;
		case 121:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_POLY_WOUND;
			muta_desc = "You feel forces of Chaos entering your old scars.";
			break;
		case 122:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WASTING;
			muta_desc = "You suddenly contract a horrible wasting disease.";
			break;
		case 123: case 124:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WEIRD_MIND;
			muta_desc = "Your thoughts suddenly take off in strange directions.";
			break;
		case 125:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_NAUSEA;
			muta_desc = "Your stomach starts to roil.";
			break;
		case 126: case 127:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_CHAOS_GIFT;
			muta_desc = "You attract the notice of an Elder God!";
			break;
		case 128:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WALK_SHAD;
			muta_desc = "You feel like reality is as thin as paper.";
			break;
		case 129: case 130:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WARNING;
			muta_desc = "You suddenly feel paranoid.";
			break;
		case 131:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_INVULN;
			muta_desc = "You are blessed with fits of resilience.";
			break;
		case 132: case 133:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SP_TO_HP;
			muta_desc = "You are subject to fits of magical healing.";
			break;
		case 134:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HP_TO_SP;
			muta_desc = "You are subject to fits of painful clarity.";
			break;
		case 135:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_DISARM;
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
			muta_desc = "You feel a sudden affinity for alien mold.";
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
		case 172: case 173: case 174:
			muta_class = &(p_ptr->muta1);
			muta_desc = "A spiked lump rises from your arm.";
			muta_which = MUT1_SHARD_BOLT;
			break;
		case 175:
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
			muta_desc = "You feel like a Steam-Mecha.";
			muta_which = MUT1_ROCKET;
			break;
		case 178:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_TENTACLES;
            muta_desc = "You sprout tentacles!";
            break;
		case 179: case 180:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_GLOW;
            muta_desc = "Your body starts to shine!";
            break;
		case 181:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_GRAV_BEAM;
			muta_desc = "You can focus a line of gravity.";
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
        msg_print("You feel normal.");
        return FALSE;
    }

    else
    { 
        msg_print("You mutate!");
        msg_print(muta_desc);
        *(muta_class) |= muta_which;

        if (muta_class == &(p_ptr->muta5))
        {
            if (muta_which == MUT5_PUNY)
            {
                if (p_ptr->muta5 & MUT5_HYPER_STR)
                {
                    msg_print("You no longer feel super-strong!");
                    p_ptr->muta5 &= ~(MUT5_HYPER_STR);
                }
            }
            else if (muta_which == MUT5_HYPER_STR)
            {
                if (p_ptr->muta5 & MUT5_PUNY)
                {
                    msg_print("You no longer feel puny!");
                    p_ptr->muta5 &= ~(MUT5_PUNY);
                }
            }
            else if (muta_which == MUT5_MORONIC)
            {
                if (p_ptr->muta5 & MUT5_HYPER_INT)
                {
                    msg_print("Your brain is no longer a living computer.");
                    p_ptr->muta5 &= ~(MUT5_HYPER_INT);
                }
            }
            else if (muta_which == MUT5_HYPER_INT)
            {
                if (p_ptr->muta5 & MUT5_MORONIC)
                {
                    msg_print("You are no longer moronic.");
                    p_ptr->muta5 &= ~(MUT5_MORONIC);
                }
            }
            else if (muta_which == MUT5_IRON_SKIN)
            {
                if (p_ptr->muta5 & MUT5_SCALES)
                {
                    msg_print("You lose your scales.");
                    p_ptr->muta5 &= ~(MUT5_SCALES);
                }
                if (p_ptr->muta5 & MUT5_FLESH_ROT)
                {
                    msg_print("Your flesh rots no longer.");
                    p_ptr->muta5 &= ~(MUT5_FLESH_ROT);
                }
                if (p_ptr->muta5 & MUT5_WART_SKIN)
                {
                    msg_print("You lose your warts.");
                    p_ptr->muta5 &= ~(MUT5_WART_SKIN);
                }
            }
            else if (muta_which == MUT5_WART_SKIN || muta_which == MUT5_SCALES
                    || muta_which == MUT5_FLESH_ROT)
            {
                if (p_ptr->muta5 & MUT5_IRON_SKIN)
                {
                    msg_print("Your skin is no longer made of iron.");
                    p_ptr->muta5 &= ~(MUT5_IRON_SKIN);
                }
            }
            else if (muta_which == MUT5_FEARLESS)
            {
                if (p_ptr->muta3 & MUT3_COWARDICE)
                {
                    msg_print("You are no longer afraid of the dark.");
                    p_ptr->muta3 &= ~(MUT3_COWARDICE);
                }
            }
            else if (muta_which == MUT5_FLESH_ROT)
            {
                if (p_ptr->muta5 & MUT5_REGEN)
                {
                    msg_print("You stop regenerating.");
                    p_ptr->muta5 &= ~(MUT5_REGEN);
                }
            }
            else if (muta_which == MUT5_REGEN)
            {
                if (p_ptr->muta5 & MUT5_FLESH_ROT)
                {
                    msg_print("Your flesh stops rotting.");
                    p_ptr->muta5 &= ~(MUT5_FLESH_ROT);
                }
            }
			else if (muta_which == MUT5_LIMBER)
			{
				if (p_ptr->muta5 & MUT5_ARTHRITIS)
				{
					msg_print("Your joints stop hurting.");
					p_ptr->muta5 &= ~(MUT5_ARTHRITIS);
				}
			}
			else if (muta_which == MUT5_ARTHRITIS)
			{
				if (p_ptr->muta5 & MUT5_LIMBER)
				{
					msg_print("You no longer feel limber.");
					p_ptr->muta5 &= ~(MUT5_LIMBER);
				}
			}
			else if (muta_which == MUT5_RESILIENT)
			{
				if (p_ptr->muta3 & MUT3_WOUND)
				{
					p_ptr->muta3 &= ~(MUT3_WOUND);
				}
			}
        }
		else if (muta_class == &(p_ptr->muta3))
        {
            if (muta_which == MUT3_COWARDICE)
            {
                if (p_ptr->muta5 & MUT5_FEARLESS)
                {
			msg_print("You no longer feel fearless.");
			p_ptr->muta5 &= ~(MUT5_FEARLESS);
                }
            }
	    	if (muta_which == MUT3_WOUND)
	    	{
				if (p_ptr->muta5 & MUT5_RESILIENT)
				{
				msg_print("You no longer feel tough.");
				p_ptr->muta5 &= ~(MUT5_RESILIENT);
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
	int	 attempts_left = 40;
	cptr muta_desc = "";
	bool muta_chosen = FALSE;
	int	 muta_which = 0;
	u32b *muta_class = 0;

	if (choose_mut) attempts_left = 1;

	while (attempts_left--)
	{	
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
			muta_which = MUT1_APPORTATION;
			muta_desc = "You lose the ability to teleport objects.";
			break;
		case 12: case 13: case 14:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_VTELEPORT;
			muta_desc = "You lose the power of teleportation at will.";
			break;
		case 15:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_BERS_RAGE;
			muta_desc = "You are no longer subject to fits of berserk rage!";
			break;
		case 16:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_COWARDICE;
			muta_desc = "You are no longer an incredible coward!";
			break;
		case 17:
			muta_class = &(p_ptr->muta3);
            muta_which = MUT3_RTELEPORT;
            muta_desc = "Your position is no longer uncertain...";
            break;
		case 18:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_ALCOHOL;
            muta_desc = "Your body stops producing alcohol.";
            break;
		case 19:
			muta_class = &(p_ptr->muta3);
            muta_which = MUT3_HALLU;
            muta_desc = "You are no longer afflicted by a hallucinatory insanity!";
            break;
		case 20:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_FLATULENT;
            muta_desc = "You are no longer subject to uncontrollable flatulence.";
            break;
		case 21: case 22:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_SCOR_TAIL;
            muta_desc = "Your scorpion tail falls off!";
            break;
		case 23: case 24:
			muta_class = &(p_ptr->muta3);
            muta_which = MUT3_HORNS;
            muta_desc = "Your horns pop back into your forehead!";
            break;
		case 25: case 26:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_BEAK;
            muta_desc = "Your beak falls off.";
            break;
		case 27: case 28: case 29:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_HYPER_STR;
            muta_desc = "Your muscles revert to normal.";
            break;
		case 30: case 31: case 32:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_PUNY;
            muta_desc = "Your muscles revert to normal.";
            break;
		case 34: case 35: case 36:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_HYPER_INT;
            muta_desc = "Your brain reverts to normal.";
            break;
		case 37: case 38: case 39:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_MORONIC;
            muta_desc = "Your brain reverts to normal.";
            break;
		case 40: case 41:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_RESILIENT;
            muta_desc = "You are no longer tough.";
            break;
		case 42: case 43:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_FAT;
            muta_desc = "You benefit from a miracle diet!";
            break;
		case 44: case 45:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ALBINO;
            muta_desc = "Your skin regains its normal color.";
            break;
		case 46: case 47: case 48:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FLESH_ROT;
            muta_desc = "You are no longer rotting.";
            break;
		case 49: case 50:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SILLY_VOI;
            muta_desc = "You no longer sound like you have inhaled helium.";
            break;
		case 51: case 52:
            muta_class = &(p_ptr->muta1);
            muta_which = MUT1_RADIATION;
            muta_desc = "You no longer emit hard radiation.";
            break;
		case 53: case 54:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_BLANK_FAC;
            muta_desc = "Your nose grows back!";
            break;
		case 55: case 56: case 57:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_EYES;
            muta_desc = "Your extra eyes fall out!";
            break;
		case 58: case 59:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_MAGIC_RES;
            muta_desc = "You are no longer resistant to magic.";
            break;
		case 60: case 61: case 62:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_NOIS;
            muta_desc = "You stop making strange noises!";
            break;
		case 63: case 64: case 65:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_INFRAVIS;
            muta_desc = "Your infravision is back to normal.";
            break;
		case 66: case 67:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_LEGS;
            muta_desc = "Your extra legs fall off!";
            break;
		case 68: case 69:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SHORT_LEG;
            muta_desc = "Your legs lengthen.";
            break;
		case 70: case 71:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ELEC_TOUC;
            muta_desc = "You short out.";
            break;
		case 72: case 73:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FIRE_BODY;
            muta_desc = "Your flames go out.";
            break;
		case 74: case 75: case 76:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_WART_SKIN;
            muta_desc = "You no longer look like a toad.";
            break;
		case 77: case 78: case 79:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SCALES;
            muta_desc = "You shed your scales.";
            break;
		case 80: case 81:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_IRON_SKIN;
            muta_desc = "Your iron turns to skin!";
            break;
		case 82: case 83:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_WINGS;
            muta_desc = "Your wings fall off.";
            break;
		case 84: case 85: case 86:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FEARLESS;
            muta_desc = "You are no longer fearless.";
            break;
		case 87: case 88:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_REGEN;
            muta_desc = "You stop regenerating.";
            break;
		case 89: case 90:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ESP;
            muta_desc = "Your mind becomes cloudy.";
            break;
		case 91: case 92:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_ATT_DEMON;
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
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_PROD_MANA;
            muta_desc = "You stop producing magical energy uncontrollably.";
            break;
		case 98:
		    muta_class = &(p_ptr->muta5);
		    muta_which = MUT5_ILL_NORM;
		    muta_desc = "You stop projecting a reassuring image.";
		    break;
		case 99: case 100:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_SPINES;
			muta_desc = "Your spines fall off!";
			break;
		case 101:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_TWISTED;
			muta_desc = "Your frame twists back to normal!";
			break;
		case 102: case 103: case 104:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_LIMBER;
			muta_desc = "Your muscles feel stiff.";
			break;
		case 105: case 106: case 107:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_ARTHRITIS;
			muta_desc = "Your joints no longer hurt.";
			break;
		case 108:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_VULN_ELEM;
			muta_desc = "You no longer feel exposed.";
			break;
		case 109:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ATT_ANIMAL;
			muta_desc = "You stop attracting animals.";
			break;
		case 110:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ATT_DRAGON;
			muta_desc = "You stop attracting dragons.";
			break;
		case 111:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WOUND;
			muta_desc = "Your flesh no longer feels weak.";
			break;
		case 112: case 113:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_TUSKS;
			muta_desc = "Your tusks fall out!";
			break;
		case 114: case 115:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_CLAWS;
			muta_desc = "You trim your nails.";
			break;
		case 116: case 117:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_DISPEL_ALL;
			muta_desc = "You no longer feel anything lurking behind you.";
			break;
		case 118:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_EAT_LIGHT;
			muta_desc = "You deny any relationship to the dark.";
			break;
		case 119:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_RAW_CHAOS;
			muta_desc = "You feel the universe is more stable around you.";
			break;
		case 120:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WRAITH;
			muta_desc = "You stop fading in and out of the physical world.";
			break;
		case 121:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_POLY_WOUND;
			muta_desc = "You feel forces of Chaos departing your old scars.";
			break;
		case 122:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WASTING;
			muta_desc = "You are cured of the horrible wasting disease.";
			break;
		case 123: case 124:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WEIRD_MIND;
			muta_desc = "Your thoughts stop taking off in strange directions.";
			break;
		case 125:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_NAUSEA;
			muta_desc = "Your stomach settles down.";
			break;
		case 126: case 127:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_CHAOS_GIFT;
			muta_desc = "The Elder God gets bored with you.";
			break;
		case 128:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WALK_SHAD;
			muta_desc = "Reality feels thick again.";
			break;
		case 129: case 130:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WARNING;
			muta_desc = "You are no longer paranoid.";
			break;
		case 131:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_INVULN;
			muta_desc = "You are no longer blessed with fits of resilience.";
			break;
		case 132: case 133:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SP_TO_HP;
			muta_desc = "You are no longer subject to fits of magical healing.";
			break;
		case 134:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HP_TO_SP;
			muta_desc = "You are no longer subject to fits of painful clarity.";
			break;
		case 135:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_DISARM;
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
		case 172: case 173: case 174:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SHARD_BOLT;
			muta_desc = "A spiked lump retreats back into your arm.";
			break;
		case 175:
			muta_class = &(p_ptr->muta1);
			muta_desc = "Your hands smooths out.";
			if (p_ptr->muta1 & MUT1_SHARD_BLAST)
				muta_which = MUT1_SHARD_BLAST;
			if (p_ptr->muta1 & MUT1_DSHARD_BLAST)
				 muta_which = MUT1_DSHARD_BLAST;
			break;
		case 176:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_CHAIN_SHARDS;
			muta_desc = "Your shoulders are no longer swelling.";
			break;
		case 177:
			muta_class = &(p_ptr->muta1);
			muta_desc = "You no longer feel like a Steam-Mecha.";
			muta_which = MUT1_ROCKET;
			break;
		case 178:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_TENTACLES;
            muta_desc = "Your tentacles fall off!";
            break;
		case 179: case 180:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_GLOW;
            muta_desc = "Your body stops shining.";
            break;
		case 181:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_GRAV_BEAM;
			muta_desc = "You can no longer focus gravity.";
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


void dump_mutations(FILE *OutFile)
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

        if (p_ptr->muta3)
        {
            if (p_ptr->muta3 & MUT3_BERS_RAGE)
          	{
                fprintf(OutFile, " You are subject to berserker fits.\n");
            }
            if (p_ptr->muta3 & MUT3_COWARDICE)
            {
                fprintf(OutFile, " You are subject to cowardice.\n");
            }
            if (p_ptr->muta3 & MUT3_RTELEPORT)
            {
                fprintf(OutFile, " You are teleporting randomly.\n");
            }
            if (p_ptr->muta3 & MUT3_ALCOHOL)
            {
                fprintf(OutFile, " Your body produces alcohol.\n");
            }
            if (p_ptr->muta3 & MUT3_HALLU)
            {
                fprintf(OutFile, " You have a hallucinatory insanity.\n");
            }
            if (p_ptr->muta3 & MUT3_FLATULENT)
            {
                fprintf(OutFile, " You are subject to uncontrollable flatulence.\n");
            }
            if (p_ptr->muta3 & MUT3_PROD_MANA)
            {
                fprintf(OutFile, " You are producing magical energy uncontrollably.\n");
            }
			if (p_ptr->muta3 & MUT3_WOUND)
			{
				fprintf(OutFile, " Your flesh is very delicate.\n");
			}
			if (p_ptr->muta3 & MUT3_ATT_ANIMAL)
			{
				fprintf(OutFile, " You attract animals.\n");
			}
            if (p_ptr->muta3 & MUT3_ATT_DEMON)
            {
                fprintf(OutFile, " You attract demons.\n");
            }
			if (p_ptr->muta3 & MUT3_ATT_DRAGON)
			{
				fprintf(OutFile, " You attract dragons.\n");
			}
            if (p_ptr->muta3 & MUT3_SCOR_TAIL)
            {
                fprintf(OutFile, " You have a scorpion tail (poison, 3d7).\n");
            }
            if (p_ptr->muta3 & MUT3_HORNS)
            {
                fprintf(OutFile, " You have horns (dam. 2d6).\n");
            }
            if (p_ptr->muta3 & MUT3_BEAK)
            {
                fprintf(OutFile, " You have a beak (dam. 2d4).\n");
            }
			if (p_ptr->muta3 & MUT3_TUSKS)
			{
				fprintf(OutFile, " You have tusks (dam. 2d6).\n");
			}
			if (p_ptr->muta3 & MUT3_CLAWS)
			{
				fprintf(OutFile, " You have claws (dam. 2d3).\n");
			}
			if (p_ptr->muta3 & MUT3_DISPEL_ALL)
			{
				fprintf(OutFile, " You are shrouded in evil.\n");
			}
			if (p_ptr->muta3 & MUT3_EAT_LIGHT)
			{
				fprintf(OutFile, " You sometimes feed off of the light around you.\n");
			}
			if (p_ptr->muta3 & MUT3_RAW_CHAOS)
			{
				fprintf(OutFile, " You occasionally are surrounded with raw chaos.\n");
			}
			if (p_ptr->muta3 & MUT3_WRAITH)
			{
				fprintf(OutFile, " You fade in and out of physical reality.\n");
			}
			if (p_ptr->muta3 & MUT3_POLY_WOUND)
			{
				fprintf(OutFile, " Your health is subject to chaotic forces.\n");
			}
			if (p_ptr->muta3 & MUT3_WASTING)
			{
				fprintf(OutFile, " You have a horrible wasting disease.\n");
			}
			if (p_ptr->muta3 & MUT3_WEIRD_MIND)
			{
				fprintf(OutFile, " Your mind randomly expands and contracts.\n");
			}
			if (p_ptr->muta3 & MUT3_NAUSEA)
			{
				fprintf(OutFile, " You have a seriously upset stomach.\n");
			}
			if (p_ptr->muta3 & MUT3_CHAOS_GIFT)
			{
				fprintf(OutFile, " Chaos deities give you gifts.\n");
			}
			if (p_ptr->muta3 & MUT3_WALK_SHAD)
			{
				fprintf(OutFile, " You occasionally stumble into other shadows.\n");
			}
			if (p_ptr->muta3 & MUT3_WARNING)
			{
				fprintf(OutFile, " You receive warnings about your foes.\n");
			}
			if (p_ptr->muta3 & MUT3_INVULN)
			{
				fprintf(OutFile, " You occasionally feel resilient.\n");
			}
			if (p_ptr->muta3 & MUT3_SP_TO_HP)
			{
				fprintf(OutFile, " Your blood sometimes rushes to your muscles.\n");
			}
			if (p_ptr->muta3 & MUT3_HP_TO_SP)
			{
				fprintf(OutFile, " Your blood sometimes rushes to your head.\n");
			}
			if (p_ptr->muta3 & MUT3_DISARM)
			{
				fprintf(OutFile, " You occasionally stumble and drop things.\n");
			}
            if (p_ptr->muta3 & MUT3_TENTACLES)
            {
                fprintf(OutFile, " You have tentacles (slow, 3d3).\n");
            }
        }

        if (p_ptr->muta5)
        {
			if (p_ptr->muta5 & MUT5_HYPER_STR)
          	{
                  fprintf(OutFile, " You are superhumanly strong (+4 MUS).\n");
          	}
          	if (p_ptr->muta5 & MUT5_PUNY)
          	{
                  fprintf(OutFile, " You are puny (-4 MUS).\n");
          	}
          	if (p_ptr->muta5 & MUT5_HYPER_INT)
          	{
                  fprintf(OutFile, " Your brain is a living computer (+4 SCH/EGO).\n");
          	}
          	if (p_ptr->muta5 & MUT5_MORONIC)
          	{
                  fprintf(OutFile, " You are moronic (-4 SCH/EGO).\n");
          	}
          	if (p_ptr->muta5 & MUT5_RESILIENT)
          	{
                  fprintf(OutFile, " You are very tough (+4 VIG).\n");
          	}
          	if (p_ptr->muta5 & MUT5_XTRA_FAT)
          	{
                  fprintf(OutFile, " You are extremely fat (+2 VIG, -2 speed).\n");
          	}
          	if (p_ptr->muta5 & MUT5_ALBINO)
          	{
                  fprintf(OutFile, " You are albino (-4 VIG).\n");
          	}
          	if (p_ptr->muta5 & MUT5_FLESH_ROT)
          	{
                  fprintf(OutFile, " Your flesh is rotting (-2 VIG, -1 CHR).\n");
          	}
          	if (p_ptr->muta5 & MUT5_SILLY_VOI)
          	{
                  fprintf(OutFile, " Your voice is a silly squeak (-4 CHR).\n");
          	}
          	if (p_ptr->muta5 & MUT5_BLANK_FAC)
          	{
                  fprintf(OutFile, " Your face is featureless (-1 CHR).\n");
          	}
			if (p_ptr->muta5 & MUT5_ILL_NORM)
			{	
				  fprintf(OutFile, " Your appearance is masked with illusion.\n");
			}
        	if (p_ptr->muta5 & MUT5_XTRA_EYES)
          	{
                  fprintf(OutFile, " You have an extra pair of eyes (x2 Perception).\n");
          	}
          	if (p_ptr->muta5 & MUT5_MAGIC_RES)
          	{
                  fprintf(OutFile, " You are resistant to magic.\n");
          	}
          	if (p_ptr->muta5 & MUT5_XTRA_NOIS)
          	{
                  fprintf(OutFile, " You make a lot of strange noise (-3 stealth).\n");
          	}
          	if (p_ptr->muta5 & MUT5_INFRAVIS)
          	{
                  fprintf(OutFile, " You have remarkable infravision (+3).\n");
          	}
          	if (p_ptr->muta5 & MUT5_XTRA_LEGS)
          	{
                  fprintf(OutFile, " You have an extra pair of legs (+3 speed).\n");
          	}
          	if (p_ptr->muta5 & MUT5_SHORT_LEG)
          	{
                  fprintf(OutFile, " Your legs are short stubs (-3 speed).\n");
          	}
          	if (p_ptr->muta5 & MUT5_ELEC_TOUC)
          	{
                  fprintf(OutFile, " Electricity is running through your veins.\n");
          	}
          	if (p_ptr->muta5 & MUT5_FIRE_BODY)
          	{
                  fprintf(OutFile, " Your body is enveloped in flames.\n");
          	}
			if (p_ptr->muta5 & MUT5_SPINES)
			{
				  fprintf(OutFile, " Your body is covered with sharp spines.\n");
			}
        	if (p_ptr->muta5 & MUT5_WART_SKIN)
        	{
                  fprintf(OutFile, " Your skin is covered with warts (-2 CHR, +5 AC).\n");
          	}
          	if (p_ptr->muta5 & MUT5_SCALES)
          	{
                  fprintf(OutFile, " Your skin has turned into scales (-1 CHR, +10 AC).\n");
          	}
          	if (p_ptr->muta5 & MUT5_IRON_SKIN)
          	{
                  fprintf(OutFile, " Your skin is made of iron (-1 AGI, +25 AC).\n");
          	}
          	if (p_ptr->muta5 & MUT5_WINGS)
          	{
                  fprintf(OutFile, " You have wings.\n");
          	}
          	if (p_ptr->muta5 & MUT5_FEARLESS)
          	{
                  fprintf(OutFile, " You are completely fearless.\n");
          	}
          	if (p_ptr->muta5 & MUT5_REGEN)
          	{
                  fprintf(OutFile, " You are regenerating.\n");
          	}
          	if (p_ptr->muta5 & MUT5_ESP)
          	{
                  fprintf(OutFile, " You are telepathic.\n");
          	}
			if (p_ptr->muta5 & MUT5_TWISTED)
			{
				  fprintf(OutFile, " Your frame is unnaturally twisted.\n");
			}
			if (p_ptr->muta5 & MUT5_LIMBER)
			{
				  fprintf(OutFile, " Your body is very limber (+3 AGI).\n");
			}
			if (p_ptr->muta5 & MUT5_ARTHRITIS)
			{
				  fprintf(OutFile, " Your joints ache constantly (-3 AGI).\n");
			}
			if (p_ptr->muta5 & MUT5_VULN_ELEM)
			{
				  fprintf(OutFile, " You are susceptible to damage from the elements.\n");
			}
			if (p_ptr->muta5 & MUT5_GLOW)
			{
				  fprintf(OutFile, " Your body is glowing brightly.\n");
			}
        }
}


/*
 * List mutations we have...
 * I should at some point make this avaialable to the player
 */
void do_cmd_knowledge_mutations(void)
{
	FILE *fff;
	char file_name[1024];

	/* Temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return;

	if (fff) dump_mutations(fff);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Mutations", 0, 0);

	/* Remove the file */
	fd_kill(file_name);
}

/********************************************************/
/* Here is where I rewrite a lot of code (rearrange?)   */
/* I'm simply updating gumbys info, with Z's nicer code */
/*******************************************************/


void mutation_power_aux(u32b power)
{
	int		dir = 0;
	int		lvl = p_ptr->lev;
	
	
	switch (power)
	{
		case MUT1_SPIT_ACID:
			if (racial_aux(9, 9, A_AGI, 15))
			{
				msg_print("You spit acid...");
				if (get_aim_dir(&dir))
					fire_ball(GF_ACID, dir, p_ptr->lev * 2, 1 + (p_ptr->lev/30));
			}
			break;

		case MUT1_BR_FIRE:
			if (racial_aux(20, p_ptr->lev, A_VIG, 18))
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
					(void)charm_monster(dir, p_ptr->lev);
			}
	
			break;
			
		case MUT1_APPORTATION:
			if (racial_aux(9, 9, A_EGO, 14))
			{
				msg_print("You concentrate...");
				if (get_aim_dir(&dir))
					fetch(dir, p_ptr->lev * 10, FALSE);
			}
			break;

		case MUT1_VTELEPORT:
			if (racial_aux(7, 7, A_EGO, 15))
			{
				msg_print("Blink!");
				teleport_player(10 + (p_ptr->lev));
			}
			break;

		case MUT1_MIND_BLST:
			if (racial_aux(5, 3, A_EGO, 15))
			{
				msg_print("You concentrate...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_PSI, dir, damroll(3 + ((p_ptr->lev - 1) / 3), 4));
			}
			break;

		case MUT1_RADIATION:
			if (racial_aux(15, 15, A_VIG, 14))
			{
				msg_print("Radiation flows from your body!");
				fire_ball(GF_FIRE, 0, (p_ptr->lev * 3), 3 + (p_ptr->lev / 20));
			}
			break;

		case MUT1_VAMPIRISM:
			if (racial_aux(13, p_ptr->lev, A_VIG, 14))
			{
				if (!get_aim_dir(&dir)) return;
				if (drain_life(dir, (p_ptr->lev * 2)))
					hp_player(p_ptr->lev * 2);
			}
			break;

		case MUT1_SUMMON_M:
			if (racial_aux(10, p_ptr->lev / 2, A_VIG, 10))
			{
				summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, 0, FALSE, TRUE);
			}
			break;

		case MUT1_BLINK:
			if (racial_aux(3, 3, A_EGO, 12))
			{
				teleport_player(10);
			}
			break;

		case MUT1_EAT_ROCK:
			if (racial_aux(8, 12, A_VIG, 18))
			{
				int x, y, ox, oy, py, px;
				byte feat;
				s16b m_idx;
				
				if (!get_rep_dir(&dir)) break;
				
				py = p_ptr->py;
				px = p_ptr->px;
				
				y = py + ddy[dir];
				x = px + ddx[dir];
				feat = cave_feat[y][x];
				m_idx = cave_m_idx[y][x];
				
				if (cave_floor_bold(y, x))
				{
					msg_print("You bite into thin air!");
					break;
				}
				else if ((feat >= FEAT_PERM_EXTRA) &&
						(feat <= FEAT_PERM_SOLID))
				{
					msg_print("Ouch!  This wall is harder than your teeth!");
					break;
				}
				else if (m_idx > 0)
				{
					msg_print("There's something in the way!");
					break;
				}
				else
				{
					if ((feat >= FEAT_DOOR_HEAD) &&
						(feat <= FEAT_RUBBLE))
					{
						msg_print("It could use some salt.");
						(void)set_food(p_ptr->food + 500);
					}
					else if ((feat >= FEAT_MAGMA) &&
							(feat <= FEAT_QUARTZ_K))
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
				p_ptr->update |= (PU_DISTANCE);
				p_ptr->window |= (PW_OVERHEAD);
			}
			break;

		case MUT1_SHRIEK:
			if (racial_aux(20, 14, A_VIG, 16))
			{
				(void)fire_ball(GF_SOUND, 0, 3 * lvl, 8);
				(void)aggravate_monsters(-1);
			}
			break;

		case MUT1_ILLUMINE:
			if (racial_aux(3, 2, A_SCH, 10))
			{
				(void)lite_area(damroll(2, (lvl / 2)), (lvl / 10) + 1);
			}
			break;

		case MUT1_DET_CURSE:
			if (racial_aux(7, 14, A_EGO, 14))
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
			if (racial_aux(8, 8, A_MUS, 14))
			{
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)hp_player(30);
				(void)set_afraid(0);
			}
			break;

		case MUT1_POLYMORPH:
			if (racial_aux(18, 20, A_VIG, 18))
			{
				do_poly_self();
			}
			break;

		case MUT1_MIDAS_TCH:
			if (racial_aux(20, 15, A_SCH, 12))
			{
				(void)alchemy();
			}
			break;

		case MUT1_GROW_MOLD:
			if (racial_aux(1, 6, A_VIG, 14))
			{
				summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_ALIEN, FALSE, TRUE);
			}
			break;

		case MUT1_RESIST:
			if (racial_aux(10, 12, A_VIG, 12))
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
			if (racial_aux(12, 12, A_MUS, 16))
			{
				msg_print("You put your foot down... Hard!");
				earthquake(p_ptr->py, p_ptr->px, 10);
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
			
			if (racial_aux(17, 50, A_SCH, 16))
			{
				if (p_ptr->depth && (p_ptr->max_depth > p_ptr->depth))
				{
					if (get_check("Reset recall depth? "))
						p_ptr->max_depth = p_ptr->depth;
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
			if (racial_aux(25, 25, A_EGO, 18))
			{
				int x,y;
				monster_type *m_ptr;
				monster_race *r_ptr;

				if (!get_rep_dir(&dir)) return;
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];

				if (!(cave_m_idx[y][x] > 0))
				{
					msg_print("You sense no evil there!");
					break;
				}
				m_ptr = &m_list[cave_m_idx[y][x]];
				r_ptr = &r_info[m_ptr->r_idx];

				if ((r_ptr->flags3 & RF3_EVIL) &&
				    !(r_ptr->flags1 & RF1_QUESTOR) &&
				    !(r_ptr->flags1 & RF1_UNIQUE))
				{
					delete_monster_idx(cave_m_idx[y][x]);
					msg_print("The evil creature vanishes in a puff of sulfurous smoke!");
				}
				else
				{
					msg_print("Your invocation is ineffectual!");
				}
			}
			break;

		case MUT1_COLD_TOUCH:
			if (racial_aux(2, 2, A_VIG, 11))
			{
				int x,y;

				if (!get_rep_dir(&dir)) return;
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];

				if (!(cave_m_idx[y][x] > 0))
				{
					msg_print("You wave your hands in the air.");
					break;
				}
				fire_bolt(GF_COLD, dir, 3 * lvl);
			}
			break;

		case MUT1_MISSILE:
			if (racial_aux(1, 1, A_VIG, 5))
			{
				msg_print("You cast a magic missile...");
				if (get_aim_dir(&dir))
					fire_bolt(GF_MISSILE, dir, damroll(3 + ((lvl - 1) / 3), 4));
			}
			break;

		case MUT1_SHARD_BOLT:
			if (racial_aux(3, 2, A_VIG, 7))
			{
				msg_print("You cast a stinging shard...");
				if (get_aim_dir(&dir))
					fire_bolt(GF_SHARD, dir, damroll(3 + (lvl / 5), 5));
			}
			break;

		case MUT1_SHARD_BLAST:
			if (racial_aux(7, 4, A_VIG, 10))
			{
				msg_print("You cast a volley of shards...");
				if (get_aim_dir(&dir))
					fire_blast(GF_SHARD, dir, 2 + (lvl / 5), 4, 5, 3);
			}
			break;

		case MUT1_DSHARD_BLAST:
			if (racial_aux(14, 8, A_VIG, 12))
			{
				msg_print("You cast a volley of shards...");
				if (get_aim_dir(&dir))
					fire_blast(GF_SHARD, dir, 2 + (lvl / 5), 4, 10, 4);
			}
			break;

		case MUT1_CHAIN_SHARDS:
			if (racial_aux(17, 10, A_MUS, 16))
			{
				msg_print("You launch a barrage of shards...");
				if (get_aim_dir(&dir))
					fire_blast(GF_SHARD, dir, 3 + (lvl / 5), 5, 10, 2);
			}
			break;
		
		case MUT1_GRAV_BEAM:
			if (racial_aux(30, 20, A_VIG, 18))
			{
				if (get_aim_dir(&dir))
				{
					msg_print("Space bends in front of you!");
					fire_bolt_or_beam(100, GF_GRAVITY, dir, damroll(10 + (lvl / 5), 8));
				}
			}
			break;

		case MUT1_ROCKET:
			if (racial_aux(21, 15, A_MUS, 18))
			{
				msg_print("You fire a rocket...");
				if (get_aim_dir(&dir))
					fire_ball(GF_SHARD, dir, lvl * 4, 2);
			}
			break;

		default:
			p_ptr->energy_use = 0;
			msg_format("Power %s not implemented. Oops.", power);
	}

}
	
	
/*
 * Returns the chance to activate a racial power/mutation
 * Wheeee! I stole this directly from Z! :-) -ccc
 * I need to do something about the fact that this is still based off of level. . . -ccc
 */
	
static int racial_chance(s16b min_level, int use_stat, int difficulty)
{
	int i;
	int val;
	int sum = 0;
	int stat = p_ptr->stat_use[use_stat];

	/* No chance for success */
	if ((p_ptr->lev < min_level) || p_ptr->confused)
	{
		return (0);
	}

	/* Calculate difficulty */
	if (p_ptr->stun)
	{
		difficulty += p_ptr->stun;
	}
	else if (p_ptr->lev > min_level)
	{
		int lev_adj = ((p_ptr->lev - min_level) / 3);
		if (lev_adj > 15) lev_adj = 15;
		difficulty -= lev_adj;
	}

	/* apparently fail rates can't drop too low */
	if (difficulty < 5) difficulty = 5;

	/* We only need halfs of the difficulty */
	difficulty = difficulty / 2;

	for (i = 1; i <= stat; i++)
	{
		val = i - difficulty;
		if (val > 0)
			sum += (val <= difficulty) ? val : difficulty;
	}

	if (difficulty == 0)
		return (100);
	else
		return (((sum * 100) / difficulty) / stat);
}

/* this returns the chance of success for the skill based racial powers. */
static int racial_race_chance(int skill_rate, int use_stat, int difficulty, int skill)
{
	int i;
	int val;
	int sum = 0;
	int stat = p_ptr->stat_use[use_stat] / 9;

	/* No chance for success */
	if ((p_ptr->skills[skill].skill_rank < skill_rate) || p_ptr->confused)
	{
		return (0);
	}

	/* Calculate difficulty */
	if (p_ptr->stun)
	{
		difficulty += p_ptr->stun;
	}
	else if (p_ptr->skills[skill].skill_rank > skill_rate)
	{
		int skill_adj = ((p_ptr->skills[skill].skill_rank - skill_rate));
		/* if (skill_adj > 10) skill_adj = 10; */
		skill_adj = p_ptr->skills[skill].skill_rank * 2;
		difficulty -= skill_adj;
	}

	/* racial powers can have 0% fail */
	if (difficulty < 0) difficulty = 0;

	/* We only need halfs of the difficulty */
	difficulty = difficulty / 2;

	/* I hope this is correct */
	for (i = 1; i <= stat; i++)
	{
		val = i - difficulty;
		if (val > 0)
			sum += (val <= difficulty) ? val : difficulty;
	}

	if (difficulty == 0)
		return (100);
	else
		return (((sum * 100) / difficulty) / stat);
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
		p_ptr->energy_use = 0;
		return FALSE;
	}

	else if (p_ptr->confused)
	{
		msg_print("You are too confused to use this power.");
		p_ptr->energy_use = 0;
		return FALSE;
	}

	else if (use_hp && (p_ptr->chp < cost))
	{
		if (!(get_check("Really use the power in your weakened state? ")))
		{
			p_ptr->energy_use = 0;
			return FALSE;
		}
	}

	/* Else attempt to do it! */

	if (p_ptr->stun)
	{
		difficulty += p_ptr->stun;
	}
	else if (p_ptr->lev > min_level)
	{
		int lev_adj = ((p_ptr->lev - min_level) / 3);
		if (lev_adj > 10) lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5) difficulty = 5;

	/* take time and pay the price */
	p_ptr->energy_use = 100;

	if (use_hp)
	{
		take_hit((cost / 2) + randint(cost / 2),
			"concentrating too hard");
	}
	else
	{
		p_ptr->csp -= (cost / 2) + randint(cost / 2);
	}
	
	/* Redraw Hit Points */
	p_ptr->redraw |= (PR_HP);

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	p_ptr->window |= (PW_OVERHEAD);


	/* Success? */
	if (randint(p_ptr->stat_use[use_stat]) >=
	    ((difficulty / 2) + randint(difficulty / 2)))
	{
	return TRUE;
	}
	
	msg_print("You've failed to concentrate hard enough.");
	return FALSE;
}

/* 
 * This function is similar to the function above, except for the special case 
 * Racial skills that affect racial powers. int skill_rate is the skill level 
 * required to use the power, and int skill is the appropriate skill
 */
bool racial_race_aux(int skill_rate, int cost, int use_stat, int difficulty, int skill)
{
	bool use_hp = FALSE;

	/* Use hit points when you don't have enough spell points */
	if (p_ptr->csp < cost) use_hp = TRUE;

	if (p_ptr->skills[skill].skill_rank < skill_rate)
	{
		msg_format("You need a %s skill rank of %d to use this power.", skills[skill].name, skill_rate);
		p_ptr->energy_use = 0;
		return FALSE;
	}

	else if (p_ptr->confused)
	{
		msg_print("You are too confused to use this power.");
		p_ptr->energy_use = 0;
		return FALSE;
	}

	else if (use_hp && (p_ptr->chp < cost))
	{
		if (!(get_check("Really use the power in your weakened state? ")))
		{
			p_ptr->energy_use = 0;
			return FALSE;
		}
	}

	/* Else attempt to do it! */

	if (p_ptr->stun)
	{
		difficulty += p_ptr->stun;
	}
	else if (p_ptr->skills[skill].skill_rank > skill_rate)
	{
		int skill_adj = ((p_ptr->skills[skill].skill_rank - skill_rate));
		/* if (skill_adj > 10) skill_adj = 10; */
		skill_adj = p_ptr->skills[skill].skill_rank * 2;
		difficulty -= skill_adj;
	}

	/* racial powers can achieve difficulties of 0 */
	if (difficulty < 0) difficulty = 0;

	/* take time and pay the price */
	p_ptr->energy_use = 100;

	if (use_hp)
	{
		take_hit((cost / 2) + randint(cost / 2),
			"concentrating too hard");
	}
	else
	{
		p_ptr->csp -= (cost / 2) + randint(cost / 2);
	}
	
	/* Redraw Hit Points */
	p_ptr->redraw |= (PR_HP);

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	p_ptr->window |= (PW_OVERHEAD);


	/* Success? */
	if (randint(p_ptr->stat_use[use_stat] / 9) >=
	    ((difficulty / 2) + randint(difficulty / 2)))
	{
	return TRUE;
	}
	
	msg_print("You've failed to concentrate hard enough.");
	return FALSE;
}



static void cmd_racial_power_aux(s32b command)
{
	s16b 		plev = p_ptr->lev;
	int 		dir = 0;
    
	switch(p_ptr->prace)
	/* int skill-rate, int cost, int use_stat, int difficulty int skill */
	/* The order of racial aux */
	{
		case RACE_BRITISH: 
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_ASIATIC:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_AMERICAN:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_AFRICAN:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_FRENCH:
			if (racial_race_aux(3, 2, A_EGO, 10, SK_CUISINE))
			{
				if (p_ptr->skills[SK_CUISINE].skill_rank < 10)
				{
					msg_print("Ze cuisine iz ze very good, no?");
					(void)set_food(p_ptr->food + (50 * p_ptr->skills[SK_CUISINE].skill_rank));
				}
				else if (p_ptr->skills[SK_CUISINE].skill_rank < 15)
				{
					msg_print("Zis meal iz fantastique!");
					(void)set_food(p_ptr->food + (100 * p_ptr->skills[SK_CUISINE].skill_rank));
					(void)hp_player(damroll((p_ptr->skills[SK_CUISINE].skill_rank / 3), (p_ptr->skills[SK_CUISINE].skill_rank / 2)));
				}
				else
				{
					msg_print("Zis is the ultimate meal!");
					(void)set_food(PY_FOOD_MAX - 1);
					(void)hp_player(damroll((p_ptr->skills[SK_CUISINE].skill_rank * 2), (p_ptr->skills[SK_CUISINE].skill_rank)));
					(void)set_poisoned(p_ptr->poisoned / 2);
				}
			}
			break;
			
		case RACE_SPANISH:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_GERMAN:
			if (command == -1)
			{
				if (racial_race_aux(3, 2, A_EGO, 10, SK_RANSACK))
				{
					if (p_ptr->skills[SK_RANSACK].skill_rank < 10)
					{
						(void)detect_objects_gold();
						(void)detect_treasure();
					}
					else if (p_ptr->skills[SK_RANSACK].skill_rank < 15)		
					{
						(void)detect_objects_gold();
						(void)detect_treasure();
						(void)detect_objects_normal();
					}
					else 
					{
						(void)detect_objects_gold();
						(void)detect_treasure();
						(void)detect_objects_normal();
						(void)detect_objects_magic();
					}
				}
			}
			if (command == -2)
			{
				if (racial_race_aux(18, 20, A_EGO, 25, SK_RANSACK))
				{
					(void)alchemy();
				} 
			}
			break;
			
		case RACE_RUSSIAN:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_FINNISH:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_ARABIC:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_DWARF:
		if (command == -1)
			if (racial_race_aux(1, 5, A_EGO, 12, SK_STONELORE))
			{
				msg_print("You examine your surroundings.");
				if (p_ptr->skills[SK_STONELORE].skill_rank < 5)
				{
					(void)detect_stairs();
				}
				else if (p_ptr->skills[SK_STONELORE].skill_rank < 10)
				{
					(void)detect_stairs();
					(void)detect_doors();
				}
				else if (p_ptr->skills[SK_STONELORE].skill_rank < 15)
				{
					(void)detect_traps();
					(void)detect_stairs();
					(void)detect_doors();
				}
				else
				{
					(void)map_area();
					(void)detect_traps();
					(void)detect_stairs();
					(void)detect_doors();
				}
				
			}
			if (command == -2)
			{
				if (racial_race_aux(20, 40, A_EGO, 45, SK_STONELORE))
				{
					msg_print("You have discovered a secret passage to another area!");
					p_ptr->leaving = TRUE;
				}
			}
			break;
			
		case RACE_BROWNIE:
			if (racial_race_aux(1, (5 + (p_ptr->skills[SK_FAE_PATH].skill_rank / 2)), A_EGO, 12, SK_FAE_PATH))
			{
				msg_print("Blink!");
				teleport_player(10 + (p_ptr->skills[SK_FAE_PATH].skill_rank));
			}
			break;

		case RACE_DAOINE_SIDHE:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_SEELIE_FAE:
			if (racial_race_aux(1, (5 + (p_ptr->skills[SK_FAE_PATH].skill_rank / 2)), A_SCH, 12, SK_FAE_PATH))
			{
				msg_print("Blink!");
				teleport_player(10 + (p_ptr->skills[SK_FAE_PATH].skill_rank));
			}
			break;
			
		case RACE_UNSEELIE_FAE:
			if (racial_race_aux(1, (5 + (p_ptr->skills[SK_FAE_PATH].skill_rank / 2)), A_SCH, 12, SK_FAE_PATH))
			{
				msg_print("Blink!");
				teleport_player(10 + (p_ptr->skills[SK_FAE_PATH].skill_rank));
			}
			break;
			
		case RACE_AUTOMATA:
			/* Utility Cypher */
			if (command == -1)
			{
				if (racial_race_aux(1, 2, A_SCH, 20, SK_UTILITY_CYPHER))
				{
					if (p_ptr->skills[SK_UTILITY_CYPHER].skill_rank < 4)
					{
						(void)detect_stairs();
						(void)detect_doors();
					}
					else if (p_ptr->skills[SK_UTILITY_CYPHER].skill_rank < 8)
					{
						(void)detect_stairs();
						(void)detect_doors();
						(void)detect_traps();
					}
					else if (p_ptr->skills[SK_UTILITY_CYPHER].skill_rank < 12)
					{
						(void)detect_stairs();
						(void)detect_doors();
						(void)detect_traps();
						(void)detect_monsters_normal();
					}
					else if (p_ptr->skills[SK_UTILITY_CYPHER].skill_rank < 16)
					{
						(void)detect_stairs();
						(void)detect_doors();
						(void)detect_traps();
						(void)detect_monsters_normal();
						(void)detect_monsters_invis();
					}
					else
					{
						(void)detect_stairs();
						(void)detect_doors();
						(void)detect_traps();
						(void)detect_monsters_normal();
						(void)detect_monsters_invis();
						(void)detect_treasure();
						(void)detect_objects_gold();
						(void)detect_objects_normal();
						(void)detect_objects_magic();
					}
				}
			}
			
			if (command == -2)
			{
				if (racial_race_aux(18, 35, A_SCH, 55, SK_UTILITY_CYPHER))
				{
					(void)ident_spell();
				}
			}
			
			/* Systems Cypher */
			if (command == -3)
			{
				if (racial_race_aux(1, 20, A_VIG, 35, SK_SYSTEMS_CYPHER))
				{
					if (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank < 5)
					{
						if (!p_ptr->fast)
						{
							(void)set_fast(randint(30) + 30 + p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank);
						}
						else
						{
							(void)set_fast(p_ptr->fast + randint(10));
						}
					}
					if (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank < 10)
					{
						(void)hp_player(damroll(p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank / 4, p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank));
						if (!p_ptr->fast)
						{
							(void)set_fast(randint(30) + 30 + p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank);
						}
						else
						{
							(void)set_fast(p_ptr->fast + randint(10));
						}
					}
					if (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank < 15)
					{
						(void)set_shero(p_ptr->shero + (randint(20) + (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank)));
						(void)hp_player(damroll((p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank / 2), (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank * 2)));
						if (!p_ptr->fast)
						{
							(void)set_fast(randint(30) + 30 + p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank);
						}
						else
						{
							(void)set_fast(p_ptr->fast + randint(10));
						}
					}
					else
					{
						int time = randint(20) + (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank * 2);
						(void)set_shero(p_ptr->shero + (randint(20) + (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank)));
						(void)hp_player(400);
						(void)set_oppose_acid(p_ptr->oppose_acid + time);
						(void)set_oppose_elec(p_ptr->oppose_elec + time);
						(void)set_oppose_fire(p_ptr->oppose_fire + time);
						(void)set_oppose_cold(p_ptr->oppose_cold + time);
						(void)set_oppose_pois(p_ptr->oppose_pois + time);
						if (!p_ptr->fast)
						{
							(void)set_fast(randint(30) + 30 + (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank * 3));
						}
						else
						{
							(void)set_fast(p_ptr->fast + randint(10));
						}
					}
					break;
	
				}
			}	
			break;
			
		case RACE_STEAM_MECHA:
			if (command == -1)
				if (racial_race_aux(4, 2, A_VIG, 10, SK_ONSLAUGHT_CYPHER))
				{
					/* Guns */
					msg_print ("*THOOM* *THOOM* *THOOM*");
					if (!get_aim_dir(&dir)) return; 
						fire_bolt(GF_SHARD, dir, damroll(3 + (p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_rank / 2), 5));
				}
			if (command == -2)
				if (racial_race_aux(10, 20, A_VIG, 30, SK_ONSLAUGHT_CYPHER))
				{
					/* Rocket */
					msg_print ("You fire a rocket!");
					if (!get_aim_dir(&dir)) return;
					if (p_ptr->skills[SK_ROCKETRY].skill_rank > 0)
					{
						fire_ball(GF_SHARD, dir, damroll(3 + (p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_rank + (p_ptr->skills[SK_ROCKETRY].skill_rank * 3 / 2)), 12), ((p_ptr->skills[SK_ROCKETRY].skill_rank / 4) + 3));
					}
					else 
					{
						fire_ball(GF_SHARD, dir, damroll(3 + (p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_rank), 12), ((p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_rank / 5) + 1));
					}
				}
			if (command == -3)
				if (racial_race_aux(14, 25, A_MUS, 35, SK_ONSLAUGHT_CYPHER))
				{
					/* Drill */
					msg_print ("WHIRRRRRRR*tink*BZZZZZZZZZZZ");
					steam_mecha_drill_level();
				}
			if (command == -4)
				if (racial_race_aux(20, 50, A_VIG, 20, SK_ONSLAUGHT_CYPHER))
				{
					/* High Yeild Devestation */
					if (!get_aim_dir(&dir)) return; 
					msg_print ("You unleash the fires of hell upon your opponents!");
					if (p_ptr->skills[SK_ROCKETRY].skill_rank > 0)
					{
						(void)fire_barrage(GF_SHARD, dir,
							 	(p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_rank + p_ptr->skills[SK_ROCKETRY].skill_rank / 3), 
							 	(((p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_rank * 2) / 3) + (p_ptr->skills[SK_ROCKETRY].skill_rank * 2)), 
							 	(((p_ptr->skills[SK_ROCKETRY].skill_rank) / 4) + 3), 2, 3);
						
					}
					else
					{
						(void)fire_barrage(GF_SHARD, dir,
							 	(p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_rank / 2), 
							 	(p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_rank), 
							 	((p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_rank) / 5), 2, 3);
					}
				}
			if (command == -5)
			{
				if (racial_race_aux(1, 20, A_VIG, 45, SK_SYSTEMS_CYPHER))
				{
					if (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank < 5)
					{
						if (!p_ptr->fast)
						{
							(void)set_fast(randint(30) + 30 + p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank);
						}
						else
						{
							(void)set_fast(p_ptr->fast + randint(10));
						}
					}
					if (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank < 10)
					{
						(void)hp_player(damroll(p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank / 4, p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank));
						if (!p_ptr->fast)
						{
							(void)set_fast(randint(30) + 30 + p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank);
						}
						else
						{
							(void)set_fast(p_ptr->fast + randint(10));
						}
					}
					if (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank < 15)
					{
						(void)set_shero(p_ptr->shero + (randint(20) + (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank)));
						(void)hp_player(damroll((p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank / 2), (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank * 2)));
						if (!p_ptr->fast)
						{
							(void)set_fast(randint(30) + 30 + p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank);
						}
						else
						{
							(void)set_fast(p_ptr->fast + randint(10));
						}
					}
					else
					{
						(void)set_shero(p_ptr->shero + (randint(20) + (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank)));
						(void)hp_player(400);
						if (!p_ptr->fast)
						{
							(void)set_fast(randint(20) + 10 + p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank * 2);
						}
						else
						{
							(void)set_fast(p_ptr->fast + randint(10));
						}
					}
					break;
	
				}
				
			}
			if (command == -6)
			{
				if(racial_race_aux(1, 30, A_MUS, 45, SK_AEGIS_CYPHER))
				{
					if (p_ptr->skills[SK_AEGIS_CYPHER].skill_rank < 10)
					{
						(void)set_tim_harding(20 + randint((p_ptr->skills[SK_AEGIS_CYPHER].skill_rank * 4)));
					}
					else
					{
						int time = randint(20) + (p_ptr->skills[SK_AEGIS_CYPHER].skill_rank * 4);
						(void)set_tim_harding(p_ptr->tim_harding + time);
						(void)set_oppose_acid(p_ptr->oppose_acid + time);
						(void)set_oppose_elec(p_ptr->oppose_elec + time);
						(void)set_oppose_fire(p_ptr->oppose_fire + time);
						(void)set_oppose_cold(p_ptr->oppose_cold + time);
						(void)set_oppose_pois(p_ptr->oppose_pois + time);
					}
				}
			}	

			break;
			
		case RACE_DJINN:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_RAKSHASA:
			if (command == -1)
				if (racial_race_aux(4, 2, A_CHR, 10, SK_DEMON_ATTUNE))
				{
					/* Fear + Confusion*/
					if (!get_aim_dir(&dir)) return; 
					(void)fear_monster(dir, (p_ptr->skills[SK_DEMON_ATTUNE].skill_rank * 3));
					(void)confuse_monster(dir, (p_ptr->skills[SK_DEMON_ATTUNE].skill_rank * 3));
				}
			if (command == -2)
				if (racial_race_aux(14, 20, A_VIG, 30, SK_DEMON_ATTUNE))
				{
					/* Chaos Sphere */
					msg_print ("A wave of dark chaotic forces blasts out from your spirit!");
					(void)project(-1, 1 + p_ptr->skills[SK_DEMON_ATTUNE].skill_rank / 4, p_ptr->py, p_ptr->px,
							  damroll((p_ptr->skills[SK_DEMON_ATTUNE].skill_rank / 2), p_ptr->skills[SK_DEMON_ATTUNE].skill_rank * 2), GF_CHAOS,
							  PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID);
				}
			if (command == -3)
				if (racial_race_aux(10, 50, A_CHR, 50, SK_DARK_CHARM))
				{
					/* Dark Charm */
					if (!get_aim_dir(&dir)) return; 
					(void)fire_bolt(GF_DOMINATION, dir,
							 	damroll((p_ptr->skills[SK_DARK_CHARM].skill_rank / 2), p_ptr->skills[SK_DARK_CHARM].skill_rank * 2));
				}
			break;
			
		case RACE_GIANT:
			if (racial_race_aux(1, 10, A_MUS, 10, SK_ROCK_TOSS))
			{
					/* Big Rock */
					if (!get_aim_dir(&dir)) return; 
					/* This should be 1-10 d12, with a radius of 1-2 */
					(void)fire_ball(GF_SHARD, dir,
							 	damroll(((p_ptr->skills[SK_ROCK_TOSS].skill_rank / 2) + 10), 12), (1+ (p_ptr->skills[SK_ROCK_TOSS].skill_rank / 10)));
			}
			break;
			
		case RACE_OGRE:
			if (racial_race_aux(1, 12, A_VIG, 12, SK_BZRK_STR))
			{
				int b = randint(100);
				if (b < 60) msg_print("RAAAAARRRGGGGGGGGH!");
				else if (b < 80) msg_print("grrrrRRRRRROOOOOOOAAAAARRRRR");
				else if (b < 95) msg_print("DIIIEEEEEEEEEARRRRRRGGGGGGGGGGGGGHHHHHHHH!");
				else msg_print("You're making me angry. You won't like me when I'm angry");
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + 10 + randint(p_ptr->skills[SK_BZRK_STR].skill_rank));
				(void)hp_player(p_ptr->skills[SK_BZRK_STR].skill_rank * 5);
			}
			break;
			
		case RACE_TROLL:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_GHOST:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
			
		case RACE_GOBLIN:
			if (racial_race_aux(1, 2, A_VIG, 5, SK_COWARDICE))
			{
				msg_print("You flee in terror!");
				set_afraid(randint(p_ptr->skills[SK_COWARDICE].skill_rank)+ 2);
				set_fast(randint(p_ptr->skills[SK_COWARDICE].skill_rank)+ 2);
			}
			break;
			
		case RACE_OLD_ONE:
			if (racial_race_aux(10, 10, A_EGO, 10, SK_DEFAULT))
			{
			}
			break;
		default:
			msg_print("This race has no bonus power.");
			p_ptr->energy_use = 0;
			break;
	}

	p_ptr->redraw |= (PR_HP | PR_MANA);
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	p_ptr->window |= (PW_OVERHEAD);
}

typedef struct power_desc_type power_desc_type;

struct power_desc_type
{
	char name[40];
	int  level;
	int  cost;
	int  fail;
	int  number;
};


/*
 * Allow user to choose a power (racial / mutation) to activate
 */
void do_cmd_racial_power(void)
{
	power_desc_type power_desc[36];
	int             num, ask, i = 0;
	bool            flag, redraw;
	bool            has_racial = FALSE;
	char            choice;
	char            out_val[160];
	
	for (num = 0; num < 36; num++)
	{
		strcpy(power_desc[num].name, "");
		power_desc[num].number = 0;
	}

	num = 0;

	if (p_ptr->confused)
	{
		msg_print("You are too confused to use any powers!");
		p_ptr->energy_use = 0;
		return;
	}

	switch (p_ptr->prace)
	{
		case RACE_BRITISH:
			break;
		case RACE_ASIATIC:
			break;
		case RACE_AMERICAN:
			break;
		case RACE_AFRICAN:
			break;
		case RACE_FRENCH:
			strcpy(power_desc[0].name, "Connoisseur");
			power_desc[0].level = 3;
			power_desc[0].cost = 2;
			power_desc[0].fail = 100 - racial_race_chance(3, A_EGO, 10, SK_CUISINE);
			has_racial = TRUE;
			break;
		case RACE_SPANISH:
			break;
		case RACE_GERMAN:
			strcpy(power_desc[0].name, "Visigoth's Plunder");
			power_desc[0].level = 3;
			power_desc[0].cost = 2;
			power_desc[0].fail = 100 - racial_race_chance(3, A_EGO, 10, SK_RANSACK);
			strcpy(power_desc[1].name, "Visigoth's Coin");
			power_desc[1].level = 18;
			power_desc[1].cost = 20;
			power_desc[1].fail = 100 - racial_race_chance(18, A_EGO, 25, SK_RANSACK);
			power_desc[1].number = -2;
			num++;
			has_racial = TRUE;
			break;
		case RACE_RUSSIAN:
			break;
		case RACE_FINNISH:
			break;
		case RACE_ARABIC:
			break;
		case RACE_DWARF:
			strcpy(power_desc[0].name, "Stonelore");
			power_desc[0].level = 1;
			power_desc[0].cost = 5;
			power_desc[0].fail = 100 - racial_race_chance(1, A_EGO, 12, SK_STONELORE);
			strcpy(power_desc[1].name, "Stonepath");
			power_desc[1].level = 20;
			power_desc[1].cost = 40;
			power_desc[1].fail = 100 - racial_race_chance(20, A_EGO, 45, SK_STONELORE);
			power_desc[1].number = -2;
			num++;
			has_racial = TRUE;
			break;
		case RACE_BROWNIE:
			strcpy(power_desc[0].name, "Fae Pathways");
			power_desc[0].level = 1;
			power_desc[0].cost = (5 + (p_ptr->skills[SK_FAE_PATH].skill_rank / 2));
			power_desc[0].fail = 100 - racial_race_chance(1, A_EGO, 12, SK_FAE_PATH);
			has_racial = TRUE;
			break;
		case RACE_DAOINE_SIDHE:
			break;
		case RACE_SEELIE_FAE:
			strcpy(power_desc[0].name, "Fae Pathways");
			power_desc[0].level = 1;
			power_desc[0].cost = (5 + (p_ptr->skills[SK_FAE_PATH].skill_rank / 2));
			power_desc[0].fail = 100 - racial_race_chance(1, A_SCH, 12, SK_FAE_PATH);
			has_racial = TRUE;
			break;
		case RACE_UNSEELIE_FAE:
			strcpy(power_desc[0].name, "Fae Pathways");
			power_desc[0].level = 1;
			power_desc[0].cost = (5 + (p_ptr->skills[SK_FAE_PATH].skill_rank / 2));
			power_desc[0].fail = 100 - racial_race_chance(1, A_SCH, 12, SK_FAE_PATH);
			has_racial = TRUE;
			break;
		case RACE_AUTOMATA:
			/* remember the systems cypher, needs points in utility cypher to activate */
			strcpy(power_desc[0].name, "Sensor Array");
			power_desc[0].level = 1;
			power_desc[0].cost = 2;
			power_desc[0].fail = 100 - racial_race_chance(1, A_SCH, 20, SK_UTILITY_CYPHER);
			strcpy(power_desc[1].name, "Object Analysis");
			power_desc[1].level = 18;
			power_desc[1].cost = 35;
			power_desc[1].fail = 100 - racial_race_chance(18, A_SCH, 55, SK_UTILITY_CYPHER);
			power_desc[1].number = -2;
			strcpy(power_desc[2].name, "Systems Cypher");
			power_desc[2].level = 1;
			power_desc[2].cost = 20;
			power_desc[2].fail = 100 - racial_race_chance(1, A_VIG, 35, SK_SYSTEMS_CYPHER);
			power_desc[2].number = -3;
			num++;			
			num++;			
			has_racial = TRUE;
			break;
		case RACE_STEAM_MECHA:
			strcpy(power_desc[0].name, "Vulcan Cannons");
			power_desc[0].level = 4;
			power_desc[0].cost = 2;
			power_desc[0].fail = 100 - racial_race_chance(4, A_VIG, 10, SK_ONSLAUGHT_CYPHER);
			strcpy(power_desc[1].name, "Fire 'Fulgurator'");
			power_desc[1].level = 10;
			power_desc[1].cost = 20;
			power_desc[1].fail = 100 - racial_race_chance(10, A_VIG, 30, SK_ONSLAUGHT_CYPHER);
			power_desc[1].number = -2;
			strcpy(power_desc[2].name, "Steam Drill");
			power_desc[2].level = 14;
			power_desc[2].cost = 25;
			power_desc[2].fail = 100 - racial_race_chance(14, A_MUS, 35, SK_ONSLAUGHT_CYPHER);
			power_desc[2].number = -3;			
			strcpy(power_desc[3].name, "High Yield Devastation");
			power_desc[3].level = 20;
			power_desc[3].cost = 50;
			power_desc[3].fail = 100 - racial_race_chance(20, A_VIG, 20, SK_ONSLAUGHT_CYPHER);
			power_desc[3].number = -4;
			strcpy(power_desc[4].name, "Systems Cypher");
			power_desc[4].level = 1;
			power_desc[4].cost = 20;
			power_desc[4].fail = 100 - racial_race_chance(1, A_VIG, 45, SK_SYSTEMS_CYPHER);
			power_desc[4].number = -5;
			strcpy(power_desc[5].name, "Defensive Array");
			power_desc[5].level = 1;
			power_desc[5].cost = 30;
			power_desc[5].fail = 100 - racial_race_chance(1, A_VIG, 45, SK_AEGIS_CYPHER);
			power_desc[5].number = -6;
			num++;
			num++;
			num++;
			num++;
			num++;
			has_racial = TRUE;
			break;
		case RACE_DJINN:
			break;
		case RACE_RAKSHASA:
			strcpy(power_desc[0].name, "Demonic Visage");
			power_desc[0].level = 4;
			power_desc[0].cost = 2;
			power_desc[0].fail = 100 - racial_race_chance(1, A_CHR, 10, SK_DEMON_ATTUNE);
			strcpy(power_desc[1].name, "Dark Nexus sphere");
			power_desc[1].level = 14;
			power_desc[1].cost = 20;
			power_desc[1].fail = 100 - racial_race_chance(15, A_VIG, 30, SK_DEMON_ATTUNE);
			power_desc[1].number = -2;
			strcpy(power_desc[2].name, "Dark Charm");
			power_desc[2].level = 10;
			power_desc[2].cost = 50;
			power_desc[2].fail = 100 - racial_race_chance(10, A_CHR, 50, SK_DARK_CHARM);
			power_desc[2].number = -3;
			num++;
			num++;
			has_racial = TRUE;
			break;
		case RACE_GIANT:
			strcpy(power_desc[0].name, "Toss a Rock!");
			power_desc[0].level = 1;
			power_desc[0].cost = 10;
			power_desc[0].fail = 100 - racial_race_chance(1, A_MUS, 10, SK_ROCK_TOSS);
			has_racial = TRUE;
			break;
		case RACE_OGRE:
			strcpy(power_desc[0].name, "Fierce Strength");
			power_desc[0].level = 1;
			power_desc[0].cost = 12;
			power_desc[0].fail = 100 - racial_race_chance(1, A_VIG, 12, SK_BZRK_STR);
			has_racial = TRUE;
			break;
		case RACE_TROLL:
			break;
		case RACE_GHOST:
			break;
		case RACE_GOBLIN:
			strcpy(power_desc[0].name, "Flee in Terror!");
			power_desc[0].level = 1;
			power_desc[0].cost = 2;
			power_desc[0].fail = 100 - racial_race_chance(1, A_VIG, 5, SK_COWARDICE);
			has_racial = TRUE;
			break;
		case RACE_OLD_ONE:
			break;
	}
	if (!(has_racial) && !(p_ptr->muta1))
	{
		msg_print("You have no powers to activate.");
		p_ptr->energy_use = 0;
		return;
	}

	if (has_racial)
	{
		power_desc[0].number = -1;
		num++;
	}

	if (p_ptr->muta1)
	{
		int lvl = p_ptr->lev;

		if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
			strcpy(power_desc[num].name, "spit acid");
			power_desc[num].level = 9;
			power_desc[num].cost = 9;
			power_desc[num].fail = 100 - racial_chance(9, A_AGI, 15);
			power_desc[num++].number = MUT1_SPIT_ACID;
		}

		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
			strcpy(power_desc[num].name, "fire breath");
			power_desc[num].level = 20;
			power_desc[num].cost = lvl;
			power_desc[num].fail = 100 - racial_chance(20, A_VIG, 18);
			power_desc[num++].number = MUT1_BR_FIRE;
		}

		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
			strcpy(power_desc[num].name, "hypnotic gaze");
			power_desc[num].level = 12;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(12, A_CHR, 18);
			power_desc[num++].number = MUT1_HYPN_GAZE;
		}

		if (p_ptr->muta1 & MUT1_APPORTATION)
		{
			strcpy(power_desc[num].name, "apportation");
			power_desc[num].level = 9;
			power_desc[num].cost = 9;
			power_desc[num].fail = 100 - racial_chance(9, A_EGO, 14);
			power_desc[num++].number = MUT1_APPORTATION;
		}

		if (p_ptr->muta1 & MUT1_VTELEPORT)
		{
			strcpy(power_desc[num].name, "teleport");
			power_desc[num].level = 7;
			power_desc[num].cost = 7;
			power_desc[num].fail = 100 - racial_chance(7, A_EGO, 15);
			power_desc[num++].number = MUT1_VTELEPORT;
		}

		if (p_ptr->muta1 & MUT1_MIND_BLST)
		{
			strcpy(power_desc[num].name, "mind blast");
			power_desc[num].level = 5;
			power_desc[num].cost = 3;
			power_desc[num].fail = 100 - racial_chance(5, A_EGO, 15);
			power_desc[num++].number = MUT1_MIND_BLST;
		}

		if (p_ptr->muta1 & MUT1_RADIATION)
		{
			strcpy(power_desc[num].name, "emit radiation");
			power_desc[num].level = 15;
			power_desc[num].cost = 15;
			power_desc[num].fail = 100 - racial_chance(15, A_VIG, 14);
			power_desc[num++].number = MUT1_RADIATION;
		}

		if (p_ptr->muta1 & MUT1_VAMPIRISM)
		{
			strcpy(power_desc[num].name, "vampiric drain");
			power_desc[num].level = 2;
			power_desc[num].cost = (1 + (lvl / 3));
			power_desc[num].fail = 100 - racial_chance(2, A_VIG, 9);
			power_desc[num++].number = MUT1_VAMPIRISM;
		}

		if (p_ptr->muta1 & MUT1_SUMMON_M)
		{
			strcpy(power_desc[num].name, "summon monsters");
			power_desc[num].level = 10;
			power_desc[num].cost = (lvl / 2);
			power_desc[num].fail = 100 - racial_chance(10, A_VIG, 10);
			power_desc[num++].number = MUT1_SUMMON_M;
		}

		if (p_ptr->muta1 & MUT1_BLINK)
		{
			strcpy(power_desc[num].name, "blink");
			power_desc[num].level = 3;
			power_desc[num].cost = 3;
			power_desc[num].fail = 100 - racial_chance(3, A_EGO, 12);
			power_desc[num++].number = MUT1_BLINK;
		}

		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
			strcpy(power_desc[num].name, "eat rock");
			power_desc[num].level = 8;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(8, A_VIG, 18);
			power_desc[num++].number = MUT1_EAT_ROCK;
		}

		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
			strcpy(power_desc[num].name, "shriek");
			power_desc[num].level = 20;
			power_desc[num].cost = 14;
			power_desc[num].fail = 100 - racial_chance(20, A_VIG, 16);
			power_desc[num++].number = MUT1_SHRIEK;
		}

		if (p_ptr->muta1 & MUT1_ILLUMINE)
		{
			strcpy(power_desc[num].name, "illuminate");
			power_desc[num].level = 3;
			power_desc[num].cost = 2;
			power_desc[num].fail = 100 - racial_chance(3, A_SCH, 10);
			power_desc[num++].number = MUT1_ILLUMINE;
		}

		if (p_ptr->muta1 & MUT1_DET_CURSE)
		{
			strcpy(power_desc[num].name, "detect curses");
			power_desc[num].level = 7;
			power_desc[num].cost = 14;
			power_desc[num].fail = 100 - racial_chance(7, A_EGO, 14);
			power_desc[num++].number = MUT1_DET_CURSE;
		}

		if (p_ptr->muta1 & MUT1_BERSERK)
		{
			strcpy(power_desc[num].name, "berserk");
			power_desc[num].level = 8;
			power_desc[num].cost = 8;
			power_desc[num].fail = 100 - racial_chance(8, A_MUS, 14);
			power_desc[num++].number = MUT1_BERSERK;
		}

		if (p_ptr->muta1 & MUT1_POLYMORPH)
		{
			strcpy(power_desc[num].name, "polymorph");
			power_desc[num].level = 18;
			power_desc[num].cost = 20;
			power_desc[num].fail = 100 - racial_chance(18, A_VIG, 18);
			power_desc[num++].number = MUT1_POLYMORPH;
		}

		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
			strcpy(power_desc[num].name, "midas touch");
			power_desc[num].level = 10;
			power_desc[num].cost = 5;
			power_desc[num].fail = 100 - racial_chance(10, A_SCH, 12);
			power_desc[num++].number = MUT1_MIDAS_TCH;
		}

		if (p_ptr->muta1 & MUT1_GROW_MOLD)
		{
			strcpy(power_desc[num].name, "grow mold");
			power_desc[num].level = 1;
			power_desc[num].cost = 6;
			power_desc[num].fail = 100 - racial_chance(1, A_VIG, 14);
			power_desc[num++].number = MUT1_GROW_MOLD;
		}

		if (p_ptr->muta1 & MUT1_RESIST)
		{
			strcpy(power_desc[num].name, "resist elements");
			power_desc[num].level = 10;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(10, A_VIG, 12);
			power_desc[num++].number = MUT1_RESIST;
		}

		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
			strcpy(power_desc[num].name, "earthquake");
			power_desc[num].level = 12;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(12, A_MUS, 16);
			power_desc[num++].number = MUT1_EARTHQUAKE;
		}

		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
			strcpy(power_desc[num].name, "dazzle");
			power_desc[num].level = 7;
			power_desc[num].cost = 15;
			power_desc[num].fail = 100 - racial_chance(7, A_CHR, 8);
			power_desc[num++].number = MUT1_DAZZLE;
		}

		if (p_ptr->muta1 & MUT1_RECALL)
		{
			strcpy(power_desc[num].name, "recall");
			power_desc[num].level = 17;
			power_desc[num].cost = 50;
			power_desc[num].fail = 100 - racial_chance(17, A_SCH, 16);
			power_desc[num++].number = MUT1_RECALL;
		}

		if (p_ptr->muta1 & MUT1_BANISH)
		{
			strcpy(power_desc[num].name, "banish evil");
			power_desc[num].level = 25;
			power_desc[num].cost = 25;
			power_desc[num].fail = 100 - racial_chance(25, A_EGO, 18);
			power_desc[num++].number = MUT1_BANISH;
		}

		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
			strcpy(power_desc[num].name, "cold touch");
			power_desc[num].level = 2;
			power_desc[num].cost = 2;
			power_desc[num].fail = 100 - racial_chance(2, A_VIG, 11);
			power_desc[num++].number = MUT1_COLD_TOUCH;
		}

		if (p_ptr->muta1 & MUT1_MISSILE)
		{
			strcpy(power_desc[num].name, "magic missle");
			power_desc[num].level = 1;
			power_desc[num].cost = 1;
			power_desc[num].fail = 100 - racial_chance(1, A_VIG, 5);
			power_desc[num++].number = MUT1_MISSILE;
		}

		if (p_ptr->muta1 & MUT1_SHARD_BOLT)
		{
			strcpy(power_desc[num].name, "shard bolt");
			power_desc[num].level = 3;
			power_desc[num].cost = 2;
			power_desc[num].fail = 100 - racial_chance(3, A_VIG, 7);
			power_desc[num++].number = MUT1_SHARD_BOLT;
		}

		if (p_ptr->muta1 & MUT1_SHARD_BLAST)
		{
			strcpy(power_desc[num].name, "shard blast");
			power_desc[num].level = 7;
			power_desc[num].cost = 4;
			power_desc[num].fail = 100 - racial_chance(4, A_MUS, 10);
			power_desc[num++].number = MUT1_SHARD_BLAST;
		}

		if (p_ptr->muta1 & MUT1_DSHARD_BLAST)
		{
			strcpy(power_desc[num].name, "large shard blast");
			power_desc[num].level = 14;
			power_desc[num].cost = 8;
			power_desc[num].fail = 100 - racial_chance(14, A_VIG, 12);
			power_desc[num++].number = MUT1_DSHARD_BLAST;
		}

		if (p_ptr->muta1 & MUT1_CHAIN_SHARDS)
		{
			strcpy(power_desc[num].name, "rapid shards");
			power_desc[num].level = 17;
			power_desc[num].cost = 10;
			power_desc[num].fail = 100 - racial_chance(17, A_MUS, 16);
			power_desc[num++].number = MUT1_CHAIN_SHARDS;
		}

		if (p_ptr->muta1 & MUT1_ROCKET)
		{
			strcpy(power_desc[num].name, "rocket");
			power_desc[num].level = 21;
			power_desc[num].cost = 15;
			power_desc[num].fail = 100 - racial_chance(21, A_MUS, 18);
			power_desc[num++].number = MUT1_ROCKET;
		}
		if (p_ptr->muta1 & MUT1_GRAV_BEAM)
		{
			strcpy(power_desc[num].name, "gravity beam");
			power_desc[num].level = 30;
			power_desc[num].cost = 20;
			power_desc[num].fail = 100 - racial_chance(30, A_VIG, 18);
			power_desc[num++].number = MUT1_GRAV_BEAM;
		}

	}	

	

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
	    I2A(0), (num <= 26) ? I2A(num - 1) : '0' + num - 27);

#ifdef ALLOW_REPEAT
if (!repeat_pull(&i) || i<0 || i>=num) {
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
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];
				char letter;
				int x1, y1;

				strcpy (dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				screen_save();

				if (num < 17)
					prt("                            Lv Cost Fail", y++, x);
				else
					prt("                            Lv Cost Fail                            Lv Cost Fail", y++, x);

				while (ctr < num)
				{
					/* letter/number for power selection */
					if (ctr < 26)
						letter = I2A(ctr);
					else
						letter = '0' + ctr - 26;
					x1 = ((ctr < 17) ? x : x + 40);
					y1 = ((ctr < 17) ? y + ctr : y + ctr - 17);

					sprintf(dummy, " %c) %-23.23s %2d %4d %3d%%", letter, power_desc[ctr].name, power_desc[ctr].level, power_desc[ctr].cost, power_desc[ctr].fail);
					prt(dummy, y1, x1);
					ctr++;
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell("Totally Illegal");
			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			(void)strnfmt(tmp_val, 78, "Use %s? ", power_desc[i].name);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw) screen_load();

	/* Abort if needed */
	if (!flag) 
	{
		p_ptr->energy_use = 0;
		return;
	}
#ifdef ALLOW_REPEAT
	repeat_push(i);
	} /*if (!repeat_pull(&i) || ...)*/
#endif /* ALLOW_REPEAT */

	if (power_desc[i].number < 0)
	{
		cmd_racial_power_aux(power_desc[i].number);
	}
	else
	{
		mutation_power_aux(power_desc[i].number);
	}

	/* Success */
	return;
}


/* Process randomly activating mutations, called from dungeon.c. -- Gumby */
void process_mutations(void)
{
	if ((p_ptr->muta3 & MUT3_BERS_RAGE) && !rand_int(3000))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("RAAAAGHH!");
		msg_print("You feel a fit of rage coming over you!");
		(void) set_shero(p_ptr->shero + 10 + randint(p_ptr->lev));
	}

	if ((p_ptr->muta3 & MUT3_COWARDICE) && !p_ptr->resist_fear &&
	    !p_ptr->hero && !p_ptr->shero && !rand_int(3000))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("It's so dark... so scary!");
		p_ptr->redraw |= PR_AFRAID;
		p_ptr->afraid = (p_ptr->afraid) + 13 + randint(26);
	}

	if ((p_ptr->muta3 & MUT3_RTELEPORT) && !rand_int(5000) &&
	     !(p_ptr->muta1 & MUT1_VTELEPORT))
	{
		disturb(0,0);
		msg_print("Your position suddenly seems very uncertain...");
		msg_print(NULL);
		teleport_player(40);
	}

	if ((p_ptr->muta3 & MUT3_ALCOHOL) && !p_ptr->resist_confu &&
	    !rand_int(6500))
	{
		disturb(0,0);
		p_ptr->redraw |= PR_EXTRA;
		msg_print("You feel a SSSCHtupor cOmINg over yOu... *HIC*!");

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
				msg_print("Thishcischs GooDSChtuff!");
				(void)set_image(p_ptr->image + randint(150) + 150);
			}
		}
	}

	if ((p_ptr->muta3 & MUT3_HALLU) && !p_ptr->resist_chaos &&
	    !rand_int(6500))
	{
		if (disturb_minor) disturb(0,0);
		p_ptr->redraw |= PR_EXTRA;
		(void)set_image(p_ptr->image + rand_int(50) + 20);
	}

	if ((p_ptr->muta3 & MUT3_FLATULENT) && !rand_int(3000))
	{
		if (disturb_minor) disturb(0,0);

		msg_print("BRRAAAP! Oops.");
		msg_print(NULL);
		fire_ball(GF_POIS, 0, p_ptr->lev, 3);
	}

	if ((p_ptr->muta3 & MUT3_ATT_DEMON) && 
	    (randint(6666)==666))
	{
		bool d_summon = FALSE;

		if (!rand_int(5))
		{
		}
		else
		{
			d_summon = summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_DEMON, FALSE, FALSE);
		}

		if (d_summon)
		{
			msg_print("You have attracted a demon!");
			disturb(0,0);
		}
	}

	if ((p_ptr->muta3 & MUT3_PROD_MANA) &&
	    !rand_int(9000))
	{
		int dire = 0;
		disturb(0,0);
		msg_print("Magical energy flows through you! You must release it!");
		flush();
		msg_print(NULL);
		(void)get_hack_dir(&dire);
		fire_ball(GF_MANA, dire, p_ptr->lev * 2, 3);
	}

	if (p_ptr->muta3 & MUT3_WOUND && !p_ptr->resist_shard &&
	    !rand_int(3000))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("Your skin rips open!  Ouch!");
		set_cut(p_ptr->cut + rand_int(20) + 10);
	}

	if (p_ptr->muta3 & MUT3_DISPEL_ALL && !rand_int(9000))
	{
		if (disturb_minor) disturb(0, 0);
		msg_print("You feel a dark power take hold of you.");
		dispel_monsters(150);
		set_stun(p_ptr->stun + randint(10) + 10);

		if (p_ptr->depth == 0)
		{
			msg_print("You see one of the shopkeepers running for the hills!");
			store_shuffle(rand_int(MAX_STORES));
		}
	}

	if ((p_ptr->muta3 & MUT3_EAT_LIGHT) && !rand_int(3000))
	{
		object_type *o_ptr;

		msg_print("A shadow passes over you.");

		/* Absorb light from the current position */
		if ((cave_info[p_ptr->py][p_ptr->px]) & CAVE_GLOW) hp_player(10); 

		o_ptr = &inventory[INVEN_LITE];

		/* Absorb some fuel in the current lite */
		if (o_ptr->tval == TV_LITE)
		{
			/* Use some fuel (except on artifacts) */
			if (!artifact_p(o_ptr) && (o_ptr->pval > 0))
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

	if ((p_ptr->muta3 & MUT3_ATT_ANIMAL) && 
	    !rand_int(6500))
	{
		bool d_summon = FALSE;

		if (!rand_int(5))
		{
		}
		else
		{
			d_summon = summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_ANIMAL, FALSE, FALSE);
		}

		if (d_summon)
		{
			msg_print("You have attracted an automata");
			disturb(0, 0);
		}
	}

	if ((p_ptr->muta3 & MUT3_RAW_CHAOS) &&
	    !rand_int(8000))
	{
		if (disturb_minor) disturb(0, 0);
		msg_print("You feel the world warping around you!");
		msg_print(NULL);
		fire_ball(GF_CHAOS, 0, p_ptr->lev, 8);
	}

	if ((p_ptr->muta3 & MUT3_WRAITH) && 
	    !rand_int(3000) && !p_ptr->tim_wraith)
	{
		if (disturb_minor) disturb(0, 0);
		set_shadow(p_ptr->tim_wraith + p_ptr->lev);
	}

	if ((p_ptr->muta3 & MUT3_POLY_WOUND) && !rand_int(3000))
	{
		if (disturb_minor) disturb(0, 0);
		do_poly_wounds();
	}

	if ((p_ptr->muta3 & MUT3_WASTING) && !rand_int(3000))
	{
		int which_stat = rand_int(6);
		int sustained = FALSE;

		switch (which_stat)
		{
			case A_MUS: if (p_ptr->sustain_mus) sustained = TRUE; break;
			case A_AGI: if (p_ptr->sustain_agi) sustained = TRUE; break;
			case A_VIG: if (p_ptr->sustain_vig) sustained = TRUE; break;
			case A_SCH: if (p_ptr->sustain_sch) sustained = TRUE; break;
			case A_EGO: if (p_ptr->sustain_ego) sustained = TRUE; break;
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

	if ((p_ptr->muta3 & MUT3_ATT_DRAGON) &&
	    !rand_int(6500))
	{
		bool d_summon = FALSE;

		if (!rand_int(5))
		{
		}
		else
		{
			d_summon = summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_DRAGON, FALSE, FALSE);
		}

		if (d_summon)
		{
			msg_print("You have attracted a dragon!");
			disturb(0,0);
		}
	}

	if ((p_ptr->muta3 & MUT3_WEIRD_MIND) &&
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

	if ((p_ptr->muta3 & MUT3_NAUSEA) && !p_ptr->slow_digest &&
	    !rand_int(9000))
	{
		disturb(0,0);
		msg_print("Your stomach roils, and you lose your lunch!");
		msg_print(NULL);
		set_food(PY_FOOD_WEAK);
	}

	if ((p_ptr->muta3 & MUT3_WALK_SHAD) && 
	    !rand_int(12000))
	{
		disturb(0,0);
		msg_print("A shadowy gate appears.  You enter...");
		p_ptr->leaving = TRUE;
	}

	/* MUT3_WARNING now detects monsters at random. -- Gumby */
	if ((p_ptr->muta3 & MUT3_WARNING) && !rand_int(1000))
	{
		(void)detect_monsters_normal();
	}
	
	if ((p_ptr->muta3 & MUT3_CHAOS_GIFT) && !rand_int(10000))
	{
		msg_print("The Elder god twists your form!");
		gain_random_mutation(0);
	}

	if ((p_ptr->muta3 & MUT3_INVULN) && 
	    !rand_int(5000) && !p_ptr->invuln)
	{
		if (disturb_minor) disturb(0, 0);
		(void)set_invuln(p_ptr->invuln + randint(5) + 5);
	}

	if ((p_ptr->muta3 & MUT3_SP_TO_HP) && 
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

	if ((p_ptr->muta3 & MUT3_HP_TO_SP) && 
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

	if ((p_ptr->muta3 & MUT3_DISARM) && (!rand_int(10000)) &&
	    (inventory[INVEN_WIELD].k_idx))
	{
		object_type *o_ptr;

		disturb(0, 0); bell("You trip over your own feet!");
		take_hit(randint(p_ptr->wt / 6), "tripping");

		msg_print(NULL);
		o_ptr = &inventory[INVEN_WIELD];
		if (o_ptr->k_idx)
		{
			msg_print("You drop your weapon!");
			inven_drop(INVEN_WIELD,1);
		}
	}
}


/*
 * Calculate the effects of mutations on stats, resistances and suchlike.
 * Called from xtra1.c. -- Gumby
 */
void calc_mutations(void)
{
	if (p_ptr->muta5 & MUT5_HYPER_STR)	p_ptr->stat_add[A_MUS] += 4;
	if (p_ptr->muta5 & MUT5_PUNY)		p_ptr->stat_add[A_MUS] -= 4;

	if (p_ptr->muta5 & MUT5_HYPER_INT)
	{
		p_ptr->stat_add[A_SCH] += 4;
		p_ptr->stat_add[A_EGO] += 4;
	}

	if (p_ptr->muta5 & MUT5_MORONIC)
	{
		p_ptr->stat_add[A_SCH] -= 4;
		p_ptr->stat_add[A_EGO] -= 4;
	}

	if (p_ptr->muta5 & MUT5_RESILIENT)	p_ptr->stat_add[A_VIG] += 4;

	if (p_ptr->muta5 & MUT5_XTRA_FAT)
	{
		p_ptr->stat_add[A_VIG] += 2;
		p_ptr->pspeed -= 2;
	}

	if (p_ptr->muta5 & MUT5_ALBINO)		p_ptr->stat_add[A_VIG] -= 4;

	if (p_ptr->muta5 & MUT5_FLESH_ROT)
	{
		p_ptr->stat_add[A_VIG] -= 2;
		p_ptr->stat_add[A_CHR] -= 1;
		p_ptr->regenerate = FALSE;
	}

	if (p_ptr->muta5 & MUT5_SILLY_VOI)	p_ptr->stat_add[A_CHR] -= 4;
	if (p_ptr->muta5 & MUT5_BLANK_FAC)	p_ptr->stat_add[A_CHR] -= 1;
	if (p_ptr->muta5 & MUT5_ILL_NORM)	p_ptr->stat_add[A_CHR] = 0;
	
	/* More fun! */
	if ((p_ptr->muta5 & MUT5_XTRA_EYES) && (p_ptr->skills[SK_SEARCHING_GOOD].skill_max > 0))
	{
		p_ptr->skills[SK_SEARCHING_GOOD].skill_rank = 2*(p_ptr->skills[SK_SEARCHING_GOOD].skill_rank);
	}
	if ((p_ptr->muta5 & MUT5_XTRA_EYES) && (p_ptr->skills[SK_SEARCHING_NORM].skill_max > 0))
	{
		p_ptr->skills[SK_SEARCHING_NORM].skill_rank = 2*(p_ptr->skills[SK_SEARCHING_NORM].skill_rank);
	}
	if ((p_ptr->muta5 & MUT5_XTRA_EYES) && (p_ptr->skills[SK_SEARCHING_POOR].skill_max > 0))
	{
		p_ptr->skills[SK_SEARCHING_POOR].skill_rank = 2*(p_ptr->skills[SK_SEARCHING_POOR].skill_rank);
	}
	
	/* fun fun ! */
	if ((p_ptr->muta5 & MUT5_MAGIC_RES) && (p_ptr->skills[SK_SAVETH_GOOD].skill_max > 0))
	{
		p_ptr->skills[SK_SAVETH_GOOD].skill_rank += (2+(p_ptr->skills[SK_SAVETH_GOOD].skill_rank/4));
	}
	if ((p_ptr->muta5 & MUT5_MAGIC_RES) && (p_ptr->skills[SK_SAVETH_NORM].skill_max > 0))
	{
		p_ptr->skills[SK_SAVETH_NORM].skill_rank += (2+(p_ptr->skills[SK_SAVETH_NORM].skill_rank/4));
	}
	if ((p_ptr->muta5 & MUT5_MAGIC_RES) && (p_ptr->skills[SK_SAVETH_POOR].skill_max > 0))
	{
		p_ptr->skills[SK_SAVETH_POOR].skill_rank += (2+(p_ptr->skills[SK_SAVETH_POOR].skill_rank/4));
	}

	if ((p_ptr->muta5 & MUT5_XTRA_NOIS) && (p_ptr->skills[SK_STEALTH_GOOD].skill_max > 0))
	{
		p_ptr->skills[SK_STEALTH_GOOD].skill_rank -= 3;
	}
	if ((p_ptr->muta5 & MUT5_XTRA_NOIS) && (p_ptr->skills[SK_STEALTH_NORM].skill_max > 0))
	{
		p_ptr->skills[SK_STEALTH_NORM].skill_rank -= 3;
	}
	if ((p_ptr->muta5 & MUT5_XTRA_NOIS) && (p_ptr->skills[SK_STEALTH_POOR].skill_max > 0))
	{
		p_ptr->skills[SK_STEALTH_POOR].skill_rank -= 3;
	}
	if (p_ptr->muta5 & MUT5_INFRAVIS)	p_ptr->see_infra += 3;
	if (p_ptr->muta5 & MUT5_XTRA_LEGS)	p_ptr->pspeed += 3;
	if (p_ptr->muta5 & MUT5_SHORT_LEG)	p_ptr->pspeed -= 3;

	if (p_ptr->muta5 & MUT5_ELEC_TOUC)
	{
	/* Add in the auras! */
		p_ptr->sh_elec = TRUE; 
		p_ptr->resist_elec = TRUE;
	}

	if (p_ptr->muta5 & MUT5_FIRE_BODY)
	{
	/* Add in the auras! */
		p_ptr->sh_fire = TRUE;
		p_ptr->resist_fire = TRUE;
		p_ptr->lite = TRUE;
	}

	if (p_ptr->muta5 & MUT5_WART_SKIN)
	{
		p_ptr->stat_add[A_CHR] -= 2;
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
	}

	if (p_ptr->muta5 & MUT5_SCALES)
	{
		p_ptr->stat_add[A_CHR] -= 1;
		p_ptr->to_a += 10;
		p_ptr->dis_to_a += 10;
	}

	if (p_ptr->muta5 & MUT5_IRON_SKIN)
	{
		p_ptr->stat_add[A_AGI] -= 1;
		p_ptr->to_a += 25;
		p_ptr->dis_to_a += 25;
	}

	if (p_ptr->muta5 & MUT5_WINGS)		p_ptr->ffall = TRUE;
	if (p_ptr->muta5 & MUT5_FEARLESS)	p_ptr->resist_fear = TRUE;
	if (p_ptr->muta5 & MUT5_REGEN)		p_ptr->regenerate = TRUE;
	if (p_ptr->muta5 & MUT5_ESP)		p_ptr->telepathy =TRUE;
	if (p_ptr->muta5 & MUT5_TWISTED)	p_ptr->stat_add[A_CHR] -= 3;
	if (p_ptr->muta5 & MUT5_SPINES)		p_ptr->sh_spine = TRUE;
	if (p_ptr->muta5 & MUT5_LIMBER)		p_ptr->stat_add[A_AGI] += 3;
	if (p_ptr->muta5 & MUT5_ARTHRITIS)	p_ptr->stat_add[A_AGI] -= 3;

	if (p_ptr->muta5 & MUT5_GLOW)
	{
		p_ptr->resist_dark = TRUE;
		p_ptr->resist_lite = TRUE;
		p_ptr->lite = TRUE;
	}
}
