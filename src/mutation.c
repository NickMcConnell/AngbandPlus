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
 * 
 * I stole this from gumby. It's the way I want it (everything 
 * in this one file) but now I have to update it to the current
 * Z style. The reason I stole it from gumby is that the mutations
 * and racial activations are both defined in this file. And no Lua.
 * It's not like I need _another_ layer of complexity while learning
 * to code. -CCC
 *
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

/* 
 * I'm considering redoing this, to better allow skills to get 
 * activateable mutations. I'm thinking _random_ mutations should
 * be primarly physical type mutations, mods to stats, and calculateable
 * type things (speed, some skill mods, etc.), The activateable mutations
 * should be things that players can access through skills. Random mutations
 * should be primarly bad.
 *
 * Random mutations should be the first x entries, that way if there is no
 * value, it can randomly select a number for the random entries, otherwise
 * we can give and remove mutations by number above the random threshold.
 * Random mutations should be primarly negative. Any mutation that is random
 * cannont (should not rather) be one that can be gotten by any other means.
 * This may mean some duplication at some point if mutations are going to 
 * be used for certain skills, and possibly 'steamware'. It may be necessary 
 * to write a seperate bit of code for steamware.
 *
 * The above has been done and is working.
 *
 * Note that I'm leaving certain cases empty, or blank for the random mutations
 * THIS MUST BE CORRECTED before this code can work properly, or it will return
 * no mutation quite a bit.
 */
 
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
		switch(choose_mut ? choose_mut: randint(96))
		{
		case 1:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_COWARDICE;
			muta_desc = "You become subject to fits of anxiety!";
			break;
		case 2:
			muta_class = &(p_ptr->muta3);
           	muta_which = MUT3_RTELEPORT;
            muta_desc = "Your position seems very uncertain...";
            break;
		case 3:
			muta_class = &(p_ptr->muta3);
            muta_which = MUT3_HALLU;
            muta_desc = "You are afflicted by visions!";
            break;
		case 4:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WOUND;
			/* This text sucks */
			muta_desc = "Your flesh feels weak.";
			break;
		case 5:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_POLY_WOUND;
			muta_desc = "Your body suffers from decay and rejuvenation.";
			break;
		case 6:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WASTING;
			muta_desc = "You suddenly contract a horrible wasting disease.";
			break;
		case 7:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_NAUSEA;
			muta_desc = "Your stomach starts to roil.";
			break;
		case 8:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_DISARM;
			muta_desc = "Your motor control is impared.";
			break;
		/* case 9 - heavy wound MUT3_HVY_WOUND */
		/* case 10-29 - Currently empty*/
		case 30:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_ATT_DEMON;
            muta_desc = "You start attracting demons.";
            break;
		case 31:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ATT_ANIMAL;
			muta_desc = "You start attracting animals.";
			break;
		case 32:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ATT_ELEMENTAL;
			muta_desc = "You start attracting elementals.";
			break;
		case 33:
            muta_class = &(p_ptr->muta4);
            muta_which = MUT4_SCOR_TAIL;
            muta_desc = "You grow a scorpion tail!";
            break;
		case 34: 
			muta_class = &(p_ptr->muta4);
            muta_which = MUT4_HORNS;
            muta_desc = "Horns pop forth into your forehead!";
            break;
		case 35:
            muta_class = &(p_ptr->muta4);
            muta_which = MUT4_BEAK;
            muta_desc = "Your mouth turns into a sharp, powerful beak!";
            break;
		case 36:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_TUSKS;
			muta_desc = "You grow a pair of tusks!";
			break;
		case 37:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_CLAWS;
			muta_desc = "Your fingers sprout claws!";
			break;
		case 38:
            muta_class = &(p_ptr->muta4);
            muta_which = MUT4_TENTACLES;
            muta_desc = "You sprout tentacles!";
            break;
        /* 39-42 nothing yet. */
		case 43:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_BERS_RAGE;
			muta_desc = "You become subject to fits of berserk rage!";
			break;
		case 44:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WEIRD_MIND;
			muta_desc = "Your occasionaly sense the minds of other beings.";
			break;
		case 45:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WARNING;
			muta_desc = "You sometimes sense your enemies.";
			break;
		case 46:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_INVULN;
			muta_desc = "You are blessed with fits of resilience.";
			break;
		case 47:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WRAITH;
			muta_desc = "You start to fade in and out of the physical world.";
			break;
		/* 48-64 Nothing yet */
		case 65:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_HYPER_STR;
            muta_desc = "Your muscles bulge outrageously!";
            break;
		case 66:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_PUNY;
            muta_desc = "Your muscles wither away...";
            break;
		case 67:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_HYPER_INT;
            muta_desc = "Your brain evolves into a living computer!";
            break;
		case 68:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_MORONIC;
            muta_desc = "Your brain withers away...";
            break;
		case 69:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_RESILIENT;
            muta_desc = "You become extraordinarily tough.";
            break;
		case 70:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_FAT;
            muta_desc = "You become sickeningly fat!";
            break;
		case 71:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ALBINO;
            muta_desc = "You turn into an albino! You feel frail...";
            break;
		case 72:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FLESH_ROT;
            muta_desc = "Your flesh is afflicted by a rotting disease!";
            break;
		case 73:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SILLY_VOI;
            muta_desc = "Your voice turns into a ridiculous squeak!";
            break;
		case 74:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_BLANK_FAC;
            muta_desc = "Your face becomes completely featureless!";
            break;
		case 75:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_EYES;
            muta_desc = "You eyesight sharpens!";
            break;
		case 76:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_MAGIC_RES;
            muta_desc = "You become resistant to magic.";
            break;
		case 77:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_NOIS;
            muta_desc = "You start making strange noises!";
            break;
		case 78:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_INFRAVIS;
            muta_desc = "Your infravision is improved.";
            break;
		case 79:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_LEGS;
            muta_desc = "You reflexes are enhanced!";
            break;
		case 80:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SHORT_LEG;
            muta_desc = "Your reflexes are slowed!";
            break;
		case 81:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ELEC_TOUC;
            muta_desc = "Electricity starts running through you!";
            break;
		case 82:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FIRE_BODY;
            muta_desc = "Your body is enveloped in flames!";
            break;
		case 83:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_WART_SKIN;
            muta_desc = "Disgusting warts appear everywhere on you!";
           break;
 		case 84:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SCALES;
            muta_desc = "Your skin turns into black scales!";
            break;
		case 85:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_IRON_SKIN;
            muta_desc = "Your skin turns to iron!";
            break;
		case 86:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_WINGS;
            muta_desc = "You grow a pair of wings.";
            break;
		case 87:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FEARLESS;
            muta_desc = "You become completely fearless.";
            break;
		case 88:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_REGEN;
            muta_desc = "You start regenerating.";
            break;
		case 89:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ESP;
            muta_desc = "You develop a telepathic ability!";
            break;
		case 90:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_ILL_NORM;
			muta_desc = "You start projecting a reassuring image.";
			break;
		case 91:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_SPINES;
			muta_desc = "You grow a fearsome covering of sharp spines!";
			break;
		case 92:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_TWISTED;
			muta_desc = "Your frame twists into an unnatural shape!";
			break;
		case 93:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_LIMBER;
			muta_desc = "Your muscles become limber.";
			break;
		case 94:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_ARTHRITIS;
			muta_desc = "Your joints suddenly hurt.";
			break;
		case 95:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_GLOW;
            muta_desc = "Your body starts to shine!";
            break;
/* END random mutations ??? */
		case 100:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_FIRE_BOLT;
			muta_desc = "You can shoot a bolt of fire!";
			break;
		case 101:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_FIRE_BALL;
			muta_desc = "You can shoot a ball of fire!";
			break;
		case 102:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_FIRE_BREATH;
			muta_desc = "You can fling flame about with ease!";
			break;
		case 103:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_FIRE_STORM;
			muta_desc = "You can create a hellstorm of fire!";
			break;
		case 104:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EARTH_BOLT;
			muta_desc = "You can fling a bolt of earth!";
			break;
		case 105:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EARTH_SHOWER;
			muta_desc = "You can create a storm of earth!";
			break;
		case 106:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BIRDS_VIEW;
			muta_desc = "You can see your surroundings from the air!";
			break;
		case 107:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_CYCLONE;
			muta_desc = "You can create a giant cyclone of air!";
			break;
		case 108:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RUSHING_STREAMS;
			muta_desc = "You can create gushing streams of water!";
			break;
		case 109:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BRIBERY;
			muta_desc = " You can bribe monsters to leave the dungeon!";
			break;
		case 110:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EVASION;
			muta_desc = " You can evade monsters for a short time!";
			break;
		case 111:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SPRING;
			muta_desc = " You can move several dozen metres quickly using acrobatics!";
			break;
		case 112:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BURST;
			muta_desc = " You can move with a quick burst of speed!";
			break;
		case 200:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_ALPHA_EYES;
			muta_desc = "You have alpha level steamware eye implants!";
			break;
		case 201:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_BETA_EYES;
			muta_desc = "You have beta level steamware eye implants!";
			break;
		case 202:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_GAMMA_EYES;
			muta_desc = "You have gamma level steamware eye implants!";
			break;
		case 203:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_DELTA_EYES;
			muta_desc = "You have delta level steamware eye implants!";
			break;
		case 204:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_ALPHA_REFLEX;
			muta_desc = "You have alpha level steamware wired reflexes!";
			break;
		case 205:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_BETA_REFLEX;
			muta_desc = "You have beta level steamware wired reflexes!";
			break;
		case 206:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_GAMMA_REFLEX;
			muta_desc = "You have gamma level steamware wired reflexes!";
			break;
		case 207:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_DELTA_REFLEX;
			muta_desc = "You have delta level steamware wired reflexes!";
			break;
		case 208:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_ALPHA_PLATING;
			muta_desc = "You have alpha level steamware dermal plating!";
			break;
		case 209:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_BETA_PLATING;
			muta_desc = "You have beta level steamware dermal plating!";
			break;
		case 210:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_GAMMA_PLATING;
			muta_desc = "You have gamma level steamware dermal plating!";
			break;
		case 211:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_DELTA_PLATING;
			muta_desc = "You have delta level steamware dermal plating!";
			break;
		case 212:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_ALPHA_CORE;
			muta_desc = "You have alpha level steamware core furnace!";
			break;
		case 213:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_BETA_CORE;
			muta_desc = "You have beta level steamware core furnace!";
			break;
		case 214:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_GAMMA_CORE;
			muta_desc = "You have gamma level steamware core furnace!";
			break;
		case 215:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_DELTA_CORE;
			muta_desc = "You have delta level steamware core furnace!";
			break;
		case 216:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_ALPHA_SPURS;
			muta_desc = "You have alpha level spurs!";
			break;
		case 217:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_BETA_SPURS;
			muta_desc = "You have beta level spurs!";
			break;
		case 218:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_GAMMA_SPURS;
			muta_desc = "You have gamma level spurs!";
			break;
		case 219:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_DELTA_SPURS;
			muta_desc = "You have delta level spurs!";
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
    	if (choose_mut > 199) msg_print("YEARRRRRRGHHHHHHH!!!!");
    	else if (choose_mut) msg_print("You have new powers! ('U')");
        else msg_print("You mutate!");
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
        else if (muta_class == &(p_ptr->muta4))
        {
        	if (muta_which == MUT4_BETA_SPURS)
        	{
        		if (p_ptr->muta4 & MUT4_ALPHA_SPURS)
        		{
        			msg_print("You now have beta level spurs!");
        			p_ptr->muta4 &= ~(MUT4_ALPHA_SPURS);
        		}
        	}
        	if (muta_which == MUT4_GAMMA_SPURS)
        	{
        		if (p_ptr->muta4 & MUT4_ALPHA_SPURS) 
        		{
        			msg_print("You now have gamma level spurs!");
        			p_ptr->muta4 &= ~(MUT4_ALPHA_SPURS);
        		}
         		if (p_ptr->muta4 & MUT4_BETA_SPURS) 
        		{
        			msg_print("You now have gamma level spurs!");
        			p_ptr->muta4 &= ~(MUT4_BETA_SPURS);
        		}
  	     	}
        	if (muta_which == MUT4_DELTA_SPURS)
        	{
        		if (p_ptr->muta4 & MUT4_ALPHA_SPURS) 
        		{
        			msg_print("You now have delta level spurs!");
        			p_ptr->muta4 &= ~(MUT4_ALPHA_SPURS);
        		}
         		if (p_ptr->muta4 & MUT4_BETA_SPURS) 
        		{
        			msg_print("You now have delta level spurs!");
        			p_ptr->muta4 &= ~(MUT4_BETA_SPURS);
        		}
         		if (p_ptr->muta4 & MUT4_GAMMA_SPURS) 
        		{
        			msg_print("You now have delta level spurs!");
        			p_ptr->muta4 &= ~(MUT4_GAMMA_SPURS);
        		}
  	     	}
		}
        else if (muta_class == &(p_ptr->muta6))
        {
        	if (muta_which == MUT6_BETA_EYES)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_EYES)
        		{
        			msg_print("You now have beta level steamware eyes!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_EYES);
        		}
        	}
        	if (muta_which == MUT6_GAMMA_EYES)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_EYES) 
        		{
        			msg_print("You now have gamma level steamware eyes!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_EYES);
        		}
         		if (p_ptr->muta6 & MUT6_BETA_EYES) 
        		{
        			msg_print("You now have gamma level steamware eyes!");
        			p_ptr->muta6 &= ~(MUT6_BETA_EYES);
        		}
  	     	}
        	if (muta_which == MUT6_DELTA_EYES)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_EYES) 
        		{
        			msg_print("You now have delta level steamware eyes!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_EYES);
        		}
         		if (p_ptr->muta6 & MUT6_BETA_EYES) 
        		{
        			msg_print("You now have delta level steamware eyes!");
        			p_ptr->muta6 &= ~(MUT6_BETA_EYES);
        		}
         		if (p_ptr->muta6 & MUT6_GAMMA_EYES) 
        		{
        			msg_print("You now have delta level steamware eyes!");
        			p_ptr->muta6 &= ~(MUT6_GAMMA_EYES);
        		}
  	     	}
        	if (muta_which == MUT6_BETA_REFLEX)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_REFLEX)
        		{
        			msg_print("You now have beta level steamware wired reflexes!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_REFLEX);
        		}
        	}
        	if (muta_which == MUT6_GAMMA_REFLEX)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_REFLEX) 
        		{
        			msg_print("You now have gamma level steamware wired reflexes!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_REFLEX);
        		}
         		if (p_ptr->muta6 & MUT6_BETA_REFLEX) 
        		{
        			msg_print("You now have gamma level steamware wired reflexes!");
        			p_ptr->muta6 &= ~(MUT6_BETA_REFLEX);
        		}
  	     	}
        	if (muta_which == MUT6_DELTA_REFLEX)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_REFLEX) 
        		{
        			msg_print("You now have delta level steamware wired reflexes!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_REFLEX);
        		}
         		if (p_ptr->muta6 & MUT6_BETA_REFLEX) 
        		{
        			msg_print("You now have delta level steamware wired reflexes!");
        			p_ptr->muta6 &= ~(MUT6_BETA_REFLEX);
        		}
         		if (p_ptr->muta6 & MUT6_GAMMA_REFLEX) 
        		{
        			msg_print("You now have delta level steamware wired reflexes!");
        			p_ptr->muta6 &= ~(MUT6_GAMMA_REFLEX);
        		}
  	     	}
        	if (muta_which == MUT6_BETA_PLATING)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_PLATING)
        		{
        			msg_print("You now have beta level steamware dermal plating!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_PLATING);
        		}
        	}
        	if (muta_which == MUT6_GAMMA_PLATING)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_PLATING) 
        		{
        			msg_print("You now have gamma level steamware dermal plating!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_PLATING);
        		}
         		if (p_ptr->muta6 & MUT6_BETA_PLATING) 
        		{
        			msg_print("You now have gamma level steamware dermal plating!");
        			p_ptr->muta6 &= ~(MUT6_BETA_PLATING);
        		}
  	     	}
        	if (muta_which == MUT6_DELTA_PLATING)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_PLATING) 
        		{
        			msg_print("You now have delta level steamware dermal plating!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_PLATING);
        		}
         		if (p_ptr->muta6 & MUT6_BETA_PLATING) 
        		{
        			msg_print("You now have delta level steamware dermal plating!");
        			p_ptr->muta6 &= ~(MUT6_BETA_PLATING);
        		}
         		if (p_ptr->muta6 & MUT6_GAMMA_PLATING) 
        		{
        			msg_print("You now have delta level steamware dermal plating!");
        			p_ptr->muta6 &= ~(MUT6_GAMMA_PLATING);
        		}
  	     	}
        	if (muta_which == MUT6_BETA_CORE)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_CORE)
        		{
        			msg_print("You now have beta level steamware core furnace!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_CORE);
        		}
        	}
        	if (muta_which == MUT6_GAMMA_CORE)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_CORE) 
        		{
        			msg_print("You now have gamma level steamware core furnace!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_CORE);
        		}
         		if (p_ptr->muta6 & MUT6_BETA_CORE) 
        		{
        			msg_print("You now have gamma level steamware core furnace!");
        			p_ptr->muta6 &= ~(MUT6_BETA_CORE);
        		}
  	     	}
        	if (muta_which == MUT6_DELTA_CORE)
        	{
        		if (p_ptr->muta6 & MUT6_ALPHA_CORE) 
        		{
        			msg_print("You now have delta level steamware core furnace!");
        			p_ptr->muta6 &= ~(MUT6_ALPHA_CORE);
        		}
         		if (p_ptr->muta6 & MUT6_BETA_CORE) 
        		{
        			msg_print("You now have delta level steamware core furnace!");
        			p_ptr->muta6 &= ~(MUT6_BETA_CORE);
        		}
         		if (p_ptr->muta6 & MUT6_GAMMA_CORE) 
        		{
        			msg_print("You now have delta level steamware core furnace!");
        			p_ptr->muta6 &= ~(MUT6_GAMMA_CORE);
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
		switch(choose_mut ? choose_mut: randint(96))
		{
		case 1:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_COWARDICE;
			muta_desc = "You are no longer subject to fits of anxiety!";
			break;
		case 2:
			muta_class = &(p_ptr->muta3);
            muta_which = MUT3_RTELEPORT;
            muta_desc = "Your position is no longer uncertain...";
            break;
		case 3:
			muta_class = &(p_ptr->muta3);
            muta_which = MUT3_HALLU;
            muta_desc = "You are no longer afflicted by visions!";
            break;
		case 4:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WOUND;
			muta_desc = "Your flesh no longer feels weak.";
			break;
		case 5:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_POLY_WOUND;
			muta_desc = "Your body no longer suffers from decay and rejuvenation.";
			break;
		case 6:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WASTING;
			muta_desc = "You are cured of the horrible wasting disease.";
			break;
		case 7:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_NAUSEA;
			muta_desc = "Your stomach settles down.";
			break;
		case 8:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_DISARM;
			muta_desc = "Your feet shrink back to normal size.";
			break;
		/* Case 9 - Heavy wound MUT3_HVY_WOUND */
		/* Case 10-29 - Currently Empty */
		case 30:
            muta_class = &(p_ptr->muta3);
            muta_which = MUT3_ATT_DEMON;
            muta_desc = "You stop attracting demons.";
            break;
		case 31:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ATT_ANIMAL;
			muta_desc = "You stop attracting animals.";
			break;
		case 32:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ATT_ELEMENTAL;
			muta_desc = "You stop attracting elementals.";
			break;
		case 33:
            muta_class = &(p_ptr->muta4);
            muta_which = MUT4_SCOR_TAIL;
            muta_desc = "Your scorpion tail falls off!";
            break;
		case 34:
			muta_class = &(p_ptr->muta4);
            muta_which = MUT4_HORNS;
            muta_desc = "Your horns pop back into your forehead!";
            break;
		case 35:
            muta_class = &(p_ptr->muta4);
            muta_which = MUT4_BEAK;
            muta_desc = "Your beak falls off.";
            break;
		case 36:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_TUSKS;
			muta_desc = "Your tusks fall out!";
			break;
		case 37:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_CLAWS;
			muta_desc = "You trim your nails.";
			break;
		case 38:
            muta_class = &(p_ptr->muta4);
            muta_which = MUT4_TENTACLES;
            muta_desc = "Your tentacles fall off!";
            break;
		/* 39-42, nothing yet */
		case 43:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_BERS_RAGE;
			muta_desc = "You are no longer subject to fits of berserk rage!";
			break;
		case 44:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WEIRD_MIND;
			muta_desc = "You no longer occasionaly sense the minds of other beings.";
			break;
		case 45:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WARNING;
			muta_desc = "You no longer sometimes sense your enemies.";
			break;
		case 46:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_INVULN;
			muta_desc = "You are no longer blessed with fits of resilience.";
			break;
		case 47:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_WRAITH;
			muta_desc = "You stop fading in and out of the physical world.";
			break;
		/* 48-64 Nothing yet */
		case 65:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_HYPER_STR;
            muta_desc = "Your muscles revert to normal.";
            break;
		case 66:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_PUNY;
            muta_desc = "Your muscles revert to normal.";
            break;
		case 67:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_HYPER_INT;
            muta_desc = "Your brain reverts to normal.";
            break;
		case 68:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_MORONIC;
            muta_desc = "Your brain reverts to normal.";
            break;
		case 69:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_RESILIENT;
            muta_desc = "You are no longer tough.";
            break;
		case 70:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_FAT;
            muta_desc = "You benefit from a fast metabolism!";
            break;
		case 71:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ALBINO;
            muta_desc = "Your skin regains its normal color.";
            break;
		case 72:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FLESH_ROT;
            muta_desc = "You are no longer rotting.";
            break;
		case 73:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SILLY_VOI;
            muta_desc = "You no longer sound like you have inhaled helium.";
            break;
		case 74:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_BLANK_FAC;
            muta_desc = "Your face becomes more memorable!";
            break;
		case 75:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_EYES;
            muta_desc = "Your eyesight is no longer sharp!";
            break;
		case 76:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_MAGIC_RES;
            muta_desc = "You are no longer resistant to magic.";
            break;
		case 77:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_NOIS;
            muta_desc = "You stop making strange noises!";
            break;
		case 78:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_INFRAVIS;
            muta_desc = "Your infravision is back to normal.";
            break;
		case 79:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_XTRA_LEGS;
            muta_desc = "Your reflexes return to normal.";
            break;
		case 80:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SHORT_LEG;
            muta_desc = "Your reflexes return to normal.";
            break;
		case 81:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ELEC_TOUC;
            muta_desc = "The electrical discharge ends.";
            break;
		case 82:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FIRE_BODY;
            muta_desc = "Your flames go out.";
            break;
		case 83:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_WART_SKIN;
            muta_desc = "You no longer look like a toad.";
            break;
		case 84:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_SCALES;
            muta_desc = "You shed your scales.";
            break;
		case 85:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_IRON_SKIN;
            muta_desc = "Your iron turns to skin!";
            break;
		case 86:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_WINGS;
            muta_desc = "Your wings fall off.";
            break;
		case 87:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_FEARLESS;
            muta_desc = "You are no longer fearless.";
            break;
		case 88:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_REGEN;
            muta_desc = "You stop regenerating.";
            break;
		case 89:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_ESP;
            muta_desc = "Your mind becomes cloudy.";
            break;
		case 90:
		    muta_class = &(p_ptr->muta5);
		    muta_which = MUT5_ILL_NORM;
		    muta_desc = "You stop projecting a reassuring image.";
		    break;
		case 91:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_SPINES;
			muta_desc = "Your spines fall off!";
			break;
		case 92:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_TWISTED;
			muta_desc = "Your frame twists back to normal!";
			break;
		case 93: 
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_LIMBER;
			muta_desc = "Your muscles feel stiff.";
			break;
		case 94:
			muta_class = &(p_ptr->muta5);
			muta_which = MUT5_ARTHRITIS;
			muta_desc = "Your joints no longer hurt.";
			break;
		case 95:
            muta_class = &(p_ptr->muta5);
            muta_which = MUT5_GLOW;
            muta_desc = "Your body stops shining.";
            break;
        /* End Random Mutations */
		case 100:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_FIRE_BOLT;
			muta_desc = "You can no longer shoot a bolt of fire.";
		case 101:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_FIRE_BALL;
			muta_desc = "You can no longer shoot a ball of fire.";
		case 102:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_FIRE_BREATH;
			muta_desc = "You can no longer fling flame about with ease.";
		case 103:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_FIRE_STORM;
			muta_desc = "You can no longer create a firestorm.";
		case 104:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EARTH_BOLT;
			muta_desc = "You can no longer fling bolts of earth.";
		case 105:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EARTH_SHOWER;
			muta_desc = "You can no longer create an earth storm.";
		case 106:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BIRDS_VIEW;
			muta_desc = "You can no longer see your surroundings from the air.";
		case 107:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_CYCLONE;
			muta_desc = "You can no longer create a cyclone of air.";
		case 108:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RUSHING_STREAMS;
			muta_desc = "You can no longer create rushing streams of water.";
		case 109:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BRIBERY;
			muta_desc = "You lose the ability to bribe monsters.";
		case 110:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EVASION;
			muta_desc = "You lose the ability to evade monsters.";
		case 111:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SPRING;
			muta_desc = "You lose the ability to move anywhere in sight.";
		case 112:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BURST;
			muta_desc = "You lose the ability to move with a quick burst of speed.";
		case 200:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_ALPHA_EYES;
			muta_desc = "ERROR, you lose your alpha eyes!";
			break;
		case 201:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_BETA_EYES;
			muta_desc = "ERROR, you lose your beta eyes!";
			break;
		case 202:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_GAMMA_EYES;
			muta_desc = "ERROR, you lose your gamma eyes!";
			break;
		case 203:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_DELTA_EYES;
			muta_desc = "ERROR, you lose your delta eyes!!";
			break;
		case 204:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_ALPHA_REFLEX;
			muta_desc = "ERROR, you lose your alpha wired reflexes!";
			break;
		case 205:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_BETA_REFLEX;
			muta_desc = "ERROR, you lose your beta wired reflexes!";
			break;
		case 206:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_GAMMA_REFLEX;
			muta_desc = "ERROR, you lose your gamma wired reflexes!";
			break;
		case 207:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_DELTA_REFLEX;
			muta_desc = "ERROR, you lose your delta wired reflexes!";
			break;
		case 208:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_ALPHA_PLATING;
			muta_desc = "ERROR, you lose your alpha dermal plating!";
			break;
		case 209:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_BETA_PLATING;
			muta_desc = "ERROR, you lose your beta dermal plating!";
			break;
		case 210:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_GAMMA_PLATING;
			muta_desc = "ERROR, you lose your gamma dermal plating!";
			break;
		case 211:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_DELTA_PLATING;
			muta_desc = "ERROR, you lose your delta dermal plating!";
			break;
		case 212:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_ALPHA_CORE;
			muta_desc = "ERROR, you lose your alpha core furnace!";
			break;
		case 213:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_BETA_CORE;
			muta_desc = "ERROR, you lose your beta core furnace!";
			break;
		case 214:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_GAMMA_CORE;
			muta_desc = "ERROR, you lose your gamma core furnace!";
			break;
		case 215:
			muta_class = &(p_ptr->muta6);
			muta_which = MUT6_DELTA_CORE;
			muta_desc = "ERROR, you lose your delta core furnace!";
			break;
		case 216:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_ALPHA_SPURS;
			muta_desc = "ERROR, you lose your alpha spurs!";
			break;
		case 217:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_BETA_SPURS;
			muta_desc = "ERROR, you lose your beta spurs!";
			break;
		case 218:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_GAMMA_SPURS;
			muta_desc = "ERROR, you lose your gamma spurs!";
			break;
		case 219:
			muta_class = &(p_ptr->muta4);
			muta_which = MUT4_DELTA_SPURS;
			muta_desc = "ERROR, you lose your delta spurs!";
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
        	if (p_ptr->muta1 & MUT1_FIRE_BOLT)
        	{
        		fprintf(OutFile, " You can shoot bolts of fire. \n");
        	}
        	if (p_ptr->muta1 & MUT1_FIRE_BALL)
        	{
        		fprintf(OutFile, " You can shoot balls of fire. \n");
        	}
        	if (p_ptr->muta1 & MUT1_FIRE_BREATH)
        	{
        		fprintf(OutFile, " You can fling flame about with ease. \n");
        	}
        	if (p_ptr->muta1 & MUT1_FIRE_STORM)
        	{
        		fprintf(OutFile, " You can create a hellstorm of fire. \n");
        	}
        	if (p_ptr->muta1 & MUT1_EARTH_BOLT)
        	{
        		fprintf(OutFile, " You can fling a bolt of earth. \n");
        	}
        	if (p_ptr->muta1 & MUT1_EARTH_SHOWER)
        	{
        		fprintf(OutFile, " You can create a storm of earth. \n");
        	}
        	if (p_ptr->muta1 & MUT1_BIRDS_VIEW)
        	{
        		fprintf(OutFile, " You can see your surroundings from the air. \n");
        	}
        	if (p_ptr->muta1 & MUT1_CYCLONE)
        	{
        		fprintf(OutFile, " You can create a giant cyclone of air. \n");
        	}
        	if (p_ptr->muta1 & MUT1_RUSHING_STREAMS)
        	{
        		fprintf(OutFile, " You can create gushing streams of water. \n");
        	}
        	if (p_ptr->muta1 & MUT1_BRIBERY)
        	{
        		fprintf(OutFile, " You can bribe monsters to leave the dungeon. \n");
        	}
        	if (p_ptr->muta1 & MUT1_EVASION)
        	{
        		fprintf(OutFile, " You can evade monsters more effectively for a time. \n");
        	}
        	if (p_ptr->muta1 & MUT1_SPRING)
        	{
        		fprintf(OutFile, " You can move to anywhere you can see. \n");
        	}
        	if (p_ptr->muta1 & MUT1_BURST)
        	{
        		fprintf(OutFile, " You can move in quick bursts of speed. \n");
        	}
        	
		}

        if (p_ptr->muta2)
        {
		}

        if (p_ptr->muta3)
        {
            if (p_ptr->muta3 & MUT3_COWARDICE)
            {
                fprintf(OutFile, " You are subject to cowardice.\n");
            }
            if (p_ptr->muta3 & MUT3_RTELEPORT)
            {
                fprintf(OutFile, " You are teleporting randomly.\n");
            }
            if (p_ptr->muta3 & MUT3_HALLU)
            {
                fprintf(OutFile, " You have a hallucinatory insanity.\n");
            }
			if (p_ptr->muta3 & MUT3_WOUND)
			{
				fprintf(OutFile, " Your flesh is very delicate.\n");
			}
			if (p_ptr->muta3 & MUT3_POLY_WOUND)
			{
				fprintf(OutFile, " Your health is subject to chaotic forces.\n");
			}
			if (p_ptr->muta3 & MUT3_WASTING)
			{
				fprintf(OutFile, " You have a horrible wasting disease.\n");
			}
			if (p_ptr->muta3 & MUT3_NAUSEA)
			{
				fprintf(OutFile, " You have a seriously upset stomach.\n");
			}
			if (p_ptr->muta3 & MUT3_DISARM)
			{
				fprintf(OutFile, " You occasionally stumble and drop things.\n");
			}
			if (p_ptr->muta3 & MUT3_ATT_ANIMAL)
			{
				fprintf(OutFile, " You attract animals.\n");
			}
            if (p_ptr->muta3 & MUT3_ATT_DEMON)
            {
                fprintf(OutFile, " You attract demons.\n");
            }
			if (p_ptr->muta3 & MUT3_ATT_ELEMENTAL)
			{
				fprintf(OutFile, " You attract elementals.\n");
			}
        }

        if (p_ptr->muta4)
        {
            if (p_ptr->muta4 & MUT4_SCOR_TAIL)
            {
                fprintf(OutFile, " You have a scorpion tail (poison, 3d7).\n");
            }
            if (p_ptr->muta4 & MUT4_HORNS)
            {
                fprintf(OutFile, " You have horns (dam. 2d6).\n");
            }
            if (p_ptr->muta4 & MUT4_BEAK)
            {
                fprintf(OutFile, " You have a beak (dam. 2d4).\n");
            }
			if (p_ptr->muta4 & MUT4_TUSKS)
			{
				fprintf(OutFile, " You have tusks (dam. 2d6).\n");
			}
			if (p_ptr->muta4 & MUT4_CLAWS)
			{
				fprintf(OutFile, " You have claws (dam. 2d3).\n");
			}
            if (p_ptr->muta4 & MUT4_TENTACLES)
            {
                fprintf(OutFile, " You have tentacles (slow, 3d3).\n");
            }
            if (p_ptr->muta4 & MUT4_ALPHA_SPURS)
            {
                fprintf(OutFile, " You have alpha level spurs (3d6).\n");
            }
            if (p_ptr->muta4 & MUT4_BETA_SPURS)
            {
                fprintf(OutFile, " You have beta level spurs (stun, 5d9).\n");
            }
            if (p_ptr->muta4 & MUT4_GAMMA_SPURS)
            {
                fprintf(OutFile, " You have gamma level spurs (stun, 9d15).\n");
            }
            if (p_ptr->muta4 & MUT4_DELTA_SPURS)
            {
                fprintf(OutFile, " You have delta level spurs (stun+slow, 15d30).\n");
            }
           if (p_ptr->muta4 & MUT4_BERS_RAGE)
          	{
                fprintf(OutFile, " You are subject to berserker fits.\n");
            }
			if (p_ptr->muta4 & MUT4_WEIRD_MIND)
			{
				fprintf(OutFile, " Your mind randomly expands and contracts.\n");
			}
			if (p_ptr->muta4 & MUT4_WARNING)
			{
				fprintf(OutFile, " You receive warnings about your foes.\n");
			}
 			if (p_ptr->muta4 & MUT4_INVULN)
			{
				fprintf(OutFile, " You occasionally feel resilient.\n");
			}
			if (p_ptr->muta4 & MUT4_WRAITH)
			{
				fprintf(OutFile, " You fade in and out of physical reality.\n");
			}
		}

         if (p_ptr->muta5)
        {
			if (p_ptr->muta5 & MUT5_HYPER_STR)
          	{
                  fprintf(OutFile, " You are superhumanly strong (+80 MUS).\n");
          	}
          	if (p_ptr->muta5 & MUT5_PUNY)
          	{
                  fprintf(OutFile, " You are puny (-80 MUS).\n");
          	}
          	if (p_ptr->muta5 & MUT5_HYPER_INT)
          	{
                  fprintf(OutFile, " Your brain is a living computer (+80 SCH/EGO).\n");
          	}
          	if (p_ptr->muta5 & MUT5_MORONIC)
          	{
                  fprintf(OutFile, " You are moronic (-80 SCH/EGO).\n");
          	}
          	if (p_ptr->muta5 & MUT5_RESILIENT)
          	{
                  fprintf(OutFile, " You are very tough (+80 VIG).\n");
          	}
          	if (p_ptr->muta5 & MUT5_XTRA_FAT)
          	{
                  fprintf(OutFile, " You are extremely fat (+40 VIG, -2 speed).\n");
          	}
          	if (p_ptr->muta5 & MUT5_ALBINO)
          	{
                  fprintf(OutFile, " You are albino (-60 VIG).\n");
          	}
          	if (p_ptr->muta5 & MUT5_FLESH_ROT)
          	{
                  fprintf(OutFile, " Your flesh is rotting (-40 VIG, -20 CHR).\n");
          	}
          	if (p_ptr->muta5 & MUT5_SILLY_VOI)
          	{
                  fprintf(OutFile, " Your voice is a silly squeak (-80 CHR).\n");
          	}
          	if (p_ptr->muta5 & MUT5_BLANK_FAC)
          	{
                  fprintf(OutFile, " Your face is featureless (-40 CHR).\n");
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
                  fprintf(OutFile, " Your reflexes are enhanced (+3 speed).\n");
          	}
          	if (p_ptr->muta5 & MUT5_SHORT_LEG)
          	{
                  fprintf(OutFile, " Your reflexes are slowed (-3 speed).\n");
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
                  fprintf(OutFile, " Your skin is covered with warts (-40 CHR, +5 AC).\n");
          	}
          	if (p_ptr->muta5 & MUT5_SCALES)
          	{
                  fprintf(OutFile, " Your skin has turned into scales (-20 CHR, +10 AC).\n");
          	}
          	if (p_ptr->muta5 & MUT5_IRON_SKIN)
          	{
                  fprintf(OutFile, " Your skin is made of iron (-40 AGI, +25 AC).\n");
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
				  fprintf(OutFile, " Your body is very limber (+60 AGI).\n");
			}
			if (p_ptr->muta5 & MUT5_ARTHRITIS)
			{
				  fprintf(OutFile, " Your joints ache constantly (-60 AGI).\n");
			}
			if (p_ptr->muta5 & MUT5_GLOW)
			{
				  fprintf(OutFile, " Your body is glowing brightly.\n");
			}
        }
        if (p_ptr->muta6)
        {
            if (p_ptr->muta6 & MUT6_ALPHA_EYES)
            {
                fprintf(OutFile, " You have alpha level enhanced eyesight (60' infra).\n");
            }
            if (p_ptr->muta6 & MUT6_BETA_EYES)
            {
                fprintf(OutFile, " You have beta level enhanced eyesight (60'infra + 3*search).\n");
            }
            if (p_ptr->muta6 & MUT6_GAMMA_EYES)
            {
                fprintf(OutFile, " You have gamma level enhanced eyesight (60'infra, 3*search, +res).\n");
            }
            if (p_ptr->muta6 & MUT6_DELTA_EYES)
            {
                fprintf(OutFile, " You have delta level enhanced eyesight (60'infra, 3*search, +res + see invis).\n");
            }
            if (p_ptr->muta6 & MUT6_ALPHA_REFLEX)
            {
                fprintf(OutFile, " You have alpha level wired reflexes (+2 speed).\n");
            }
            if (p_ptr->muta6 & MUT6_BETA_REFLEX)
            {
                fprintf(OutFile, " You have beta level wired reflexes (+3 speed, +20 Agi).\n");
            }
            if (p_ptr->muta6 & MUT6_GAMMA_REFLEX)
            {
                fprintf(OutFile, " You have gamma level wired reflexes (+4 speed, +60 Agi).\n");
            }
            if (p_ptr->muta6 & MUT6_DELTA_REFLEX)
            {
                fprintf(OutFile, " You have delta level wired reflexes (+5 speed, +100 Agi).\n");
            }
            if (p_ptr->muta6 & MUT6_ALPHA_PLATING)
            {
                fprintf(OutFile, " You have alpha level dermal plating (+10 ac).\n");
            }
            if (p_ptr->muta6 & MUT6_BETA_PLATING)
            {
                fprintf(OutFile, " You have beta level dermal plating (+20 ac -1 speed).\n");
            }
            if (p_ptr->muta6 & MUT6_GAMMA_PLATING)
            {
                fprintf(OutFile, " You have gamma level dermal plating (+40 ac -2 speed).\n");
            }
            if (p_ptr->muta6 & MUT6_DELTA_PLATING)
            {
                fprintf(OutFile, " You have delta level dermal plating (+70 ac).\n");
            }
            if (p_ptr->muta6 & MUT6_ALPHA_CORE)
            {
                fprintf(OutFile, " You have alpha level core furnace (+2 Health).\n");
            }
            if (p_ptr->muta6 & MUT6_BETA_CORE)
            {
                fprintf(OutFile, " You have beta level core furnace (+6 Health).\n");
            }
            if (p_ptr->muta6 & MUT6_GAMMA_CORE)
            {
                fprintf(OutFile, " You have gamma level core furnace (+10 Health).\n");
            }
            if (p_ptr->muta6 & MUT6_DELTA_CORE)
            {
                fprintf(OutFile, " You have delta level core furnace (+20 Health).\n");
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
	int		dam, ty, tx, b, k;
	int 	firelore, windlore, earthlore, waterlore;
	int 	firemastery, windmastery, earthmastery, watermastery;
	int 	bribe;
	int 	py = p_ptr->py;
	int 	px = p_ptr->px;
	
	firelore = windlore = earthlore = waterlore = 0;
	firemastery = windmastery = earthmastery = watermastery = 0;
	bribe = 0;
	
	if (p_ptr->skills[SK_FIRE_LORE].skill_max > 1)
		firelore = p_ptr->skills[SK_FIRE_LORE].skill_rank;
	if (p_ptr->skills[SK_WIND_LORE].skill_max > 1)
		windlore = p_ptr->skills[SK_WIND_LORE].skill_rank;
	if (p_ptr->skills[SK_WATER_LORE].skill_max > 1)
		waterlore = p_ptr->skills[SK_WATER_LORE].skill_rank;
	if (p_ptr->skills[SK_EARTH_LORE].skill_max > 1)
		earthlore = p_ptr->skills[SK_EARTH_LORE].skill_rank;
	if (p_ptr->skills[SK_FIRE_MASTERY].skill_max > 0)
		firemastery = p_ptr->skills[SK_FIRE_MASTERY].skill_rank;
	if (p_ptr->skills[SK_WIND_MASTERY].skill_max > 0)
		windmastery = p_ptr->skills[SK_WIND_MASTERY].skill_rank;
	if (p_ptr->skills[SK_WATER_MASTERY].skill_max > 0)
		watermastery = p_ptr->skills[SK_WATER_MASTERY].skill_rank;
	if (p_ptr->skills[SK_EARTH_MASTERY].skill_max > 0)
		earthmastery = p_ptr->skills[SK_EARTH_MASTERY].skill_rank;
	if (p_ptr->skills[SK_BRIBERY].skill_max > 0)
		bribe = p_ptr->skills[SK_BRIBERY].skill_rank;
	
	switch (power)
	{
		case MUT1_FIRE_BOLT:
			if (racial_aux(1, 5, A_MUS, 8))
			{
				dam = damroll(4, (firelore / 2));
				if (firemastery) dam += firemastery;
				if (get_aim_dir(&dir))
					fire_bolt(GF_FIRE, dir, dam);
			}
			break;
		case MUT1_FIRE_BALL:
			if (racial_aux(1, 10, A_MUS, 12))
			{
				dam = damroll(6, (firelore));
				if (firemastery) dam += firemastery * 2;
				if (get_aim_dir(&dir))
				{
					if (firemastery) fire_orb(GF_FIRE, dir, dam, 5);
					else fire_ball(GF_FIRE, dir, dam, 3);
				}
			}
			break;
		case MUT1_FIRE_BREATH:
			if (racial_aux(1, 18, A_MUS, 22))
			{
				dam = damroll(8, (firelore + firemastery));
				if (get_aim_dir(&dir))
				{
					fire_arc(GF_FIRE, dir, dam, 10, 45);
				}
			}
			break;
		/* This is basically the mage spell 'Ice Storm' */
		case MUT1_FIRE_STORM:
			if (racial_aux(1, 65, A_MUS, 35))
			{
				if (get_aim_dir(&dir))
				{
					 for (b = 0; b < (firemastery / 2); b++)
					 {
						 /* Get a new effect index */
						 k = effect_prep();
		
						 /* Note failure XXX */
						 if (k < 0) break;
		
						 /* We want a spirit, */
						 x_list[k].index = EFFECT_SEEKER_VORTEX;
		
						 /* Of fire */
						 x_list[k].type = GF_FIRE;
		
						/* Use the given direction */
						ty = py + ddy[dir];
						tx = px + ddx[dir];
						
						/* Hack -- Use an actual "target" */
						if ((dir == 5) && target_okay())
						{
							ty = p_ptr->target_row;
							tx = p_ptr->target_col;
						}
						 /* That starts at the character location. */
						 x_list[k].y0 = ty;
						 x_list[k].x0 = tx;
		
						 /* Moves with a speed that depends on the wind, */
						 x_list[k].time_delay = 3;
		
						 /* Does damage, */
						 x_list[k].power = damroll(10, firemastery);
		
						 /* And lasts for a certain period of time. */
						 x_list[k].lifespan = firemastery * 2;
					}
		
					k = effect_prep();
		
					/* Note failure XXX */
					if (k < 0) break;
		
					/* Use the given direction */
					ty = py + ddy[dir];
					tx = px + ddx[dir];
					
					/* Hack -- Use an actual "target" */
					if ((dir == 5) && target_okay())
					{
						ty = p_ptr->target_row;
						tx = p_ptr->target_col;
					}
		
					/* We want an lingering cloud, */
					x_list[k].index = EFFECT_IRREGULAR_CLOUD;
		
					/* Of ICE */
					x_list[k].type = GF_PLASMA;
		
					/* That starts at the monster location. */
					x_list[k].y0 = x_list[k].y1 = ty;
					x_list[k].x0 = x_list[k].x1 = tx;
		
					/* It attacks every 8 -> 5 game turns, */
					x_list[k].time_delay = 5;
		
					/* Does damage, has a large radius, */
					x_list[k].power = damroll(firemastery / 3, ((firemastery / 2) + 5));
					x_list[k].power2 = 10;
		
					/* And lasts for about 10 attacks */
					x_list[k].lifespan = firemastery * 2;
				}
			}
			break;
		case MUT1_EARTH_BOLT:
			if (racial_aux(1, 5, A_VIG, 8))
			{
				dam = damroll(4, (earthlore / 2));
				if (earthmastery) dam += earthmastery;
				if (get_aim_dir(&dir))
					fire_bolt(GF_EARTH, dir, dam);
			}
			break;
		/* This is basically the mage Fire Storm */
		case MUT1_EARTH_SHOWER:
			if (racial_aux(1, 65, A_VIG, 33))
			{
				if (get_aim_dir(&dir))
				{
					for (b = 0; b < 12; b++)
					{
						/* Get a new effect index */
						k = effect_prep();
			
						/* Note failure XXX */
						if (k < 0) break;
			
						/* Use the given direction */
						ty = py + ddy[dir];
						tx = px + ddx[dir];
						
						/* Hack -- Use an actual "target" */
						if ((dir == 5) && target_okay())
						{
							ty = p_ptr->target_row;
							tx = p_ptr->target_col;
						}
			
						/* We want an lingering cloud, */
						x_list[k].index = EFFECT_SPHERE;
			
						/* Of poison */
						x_list[k].type = GF_SHARDS;
			
						/* That starts at the monster location. */
						x_list[k].y0 = x_list[k].y1 = ty + rand_range(-2, 2);
						x_list[k].x0 = x_list[k].x1 = tx + rand_range(-2, 2);
			
						/* It attacks every 8 -> 5 game turns, */
						x_list[k].time_delay = 10;
			
						/* Does damage, has a large radius, */
						x_list[k].power =  damroll(10 + (earthmastery), 4);
						x_list[k].power2 = 2;
			
						/* And lasts for about 3 attacks */
						x_list[k].lifespan = 4;
					}
					k = effect_prep();
		
					/* Note failure XXX */
					if (k < 0) break;
		
					/* Use the given direction */
					ty = py + ddy[dir];
					tx = px + ddx[dir];
					
					/* Hack -- Use an actual "target" */
					if ((dir == 5) && target_okay())
					{
						ty = p_ptr->target_row;
						tx = p_ptr->target_col;
					}
		
					/* We want an lingering cloud, */
					x_list[k].index = EFFECT_IRREGULAR_CLOUD;
		
					/* Of ICE */
					x_list[k].type = GF_ROCK;
		
					/* That starts at the monster location. */
					x_list[k].y0 = x_list[k].y1 = ty;
					x_list[k].x0 = x_list[k].x1 = tx;
		
					/* It attacks every 8 -> 5 game turns, */
					x_list[k].time_delay = 5;
		
					/* Does damage, has a large radius, */
					x_list[k].power = damroll(3, 4 + earthmastery);
					x_list[k].power2 = 6;
		
					/* And lasts for about 10 attacks */
					x_list[k].lifespan = 8;
				}
			}
			break;
		case MUT1_BIRDS_VIEW:
			if (racial_aux(1, 3, A_AGI, 12))
			{
				map_area();			
			}
			break;
		case MUT1_CYCLONE:
			if (racial_aux(1, 35, A_AGI, 40))
			{
			 	/* Get a direction */
				if (get_aim_dir(&dir))
				{
					/* Use the given direction */
					ty = py + ddy[dir];
					tx = px + ddx[dir];
					
					/* Hack -- Use an actual "target" */
					if ((dir == 5) && target_okay())
					{
						ty = p_ptr->target_row;
						tx = p_ptr->target_col;
					}
					for (b = 0; b < 5 + (windmastery / 2); b++)
					{
						 /* Get a new effect index */
						 k = effect_prep();
		
						 /* Note failure XXX */
						 if (k < 0) break;
		
						 /* We want a whirlpool, */
						 x_list[k].index = EFFECT_WHIRLPOOL;
		
						 /* Of water */
						 x_list[k].type = GF_GALE;
		
						 /* That starts at the target location. */
						 x_list[k].y0 = ty + ddy_cdd[(b + 1) % 8];
						 x_list[k].x0 = tx + ddx_cdd[(b + 1) % 8]; 
		
						 /* Moves with a speed that depends on the wind, */
						 x_list[k].time_delay = 1;
		
						 /* Does damage, */
						 x_list[k].power = rand_range(10, windmastery * 2);
		
						 /* And lasts for a certain period of time. */
						 x_list[k].lifespan = 80;
					}
				}
			}
 			break;
		case MUT1_RUSHING_STREAMS:
			if (racial_aux(1, 16, A_VIG, 22))
			{
				dam = (waterlore + watermastery) / 6;
				if (get_aim_dir(&dir))
					fire_blast(GF_STORM, dir, 40, watermastery, dam, 1, TRUE);
			}
			break;
		case MUT1_BRIBERY:
			if (racial_aux(1, 2, A_CHR, 35))
			{
				if (get_hack_dir(&dir))
					fire_ball_special(GF_BRIBE, dir, 0, 0, PROJECT_JUMP, 0);
			}
			break;	
		case MUT1_EVASION:
			if (racial_aux(1, 5, A_AGI, 5))
			{
				(void)set_tim_evade(20 + randint(20));
			}
			break;	
		case MUT1_SPRING:
			if (racial_aux(1, 25, A_AGI, 45))
			{
				 msg_print("Choose a location to move to.");
				 message_flush();
				 dimen_door(8, 10);
			}
			break;	
		case MUT1_BURST:
			if (racial_aux(1, 15, A_AGI, 15))
			{
				if (!p_ptr->fast) (void)set_fast(randint(20) + 20);
				else (void)set_fast(p_ptr->fast + randint(5));
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
	if (difficulty < 0) difficulty = 0;

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

	if (difficulty < 0) difficulty = 0;

	if (p_ptr->csp < cost)
	{
		p_ptr->energy_use = 0;
		msg_print("You need more mana.");
		return FALSE;
	}

	/* take time and pay the price */
	p_ptr->energy_use = 100;

	p_ptr->csp -= (cost / 2) + randint(cost / 2);
	
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
#if 0
	bool use_hp = FALSE;

	/* Use hit points when you don't have enough spell points */
	if (p_ptr->csp < cost) use_hp = TRUE;
#endif

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

	else if (p_ptr->csp < cost)
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

#if 0
	if (use_hp)
	{
		take_hit((cost / 2) + randint(cost / 2),
			"concentrating too hard", FALSE);
	}
#endif

	if (p_ptr->csp < cost)
	{
		/* set sp to 0 */
		p_ptr->csp = 0;
		
		/* Take some damage */
		take_hit((cost) + randint(cost / 2),
			"concentrating too hard", FALSE);
		
	}
	else p_ptr->csp -= (cost / 2) + randint(cost / 2);
	
	
	
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
					(void)hp_player(damroll((p_ptr->skills[SK_CUISINE].skill_rank / 3), (p_ptr->skills[SK_CUISINE].skill_rank / 2)));
				}
				else if (p_ptr->skills[SK_CUISINE].skill_rank < 15)
				{
					msg_print("Zis meal iz fantastique!");
					(void)set_food(p_ptr->food + (100 * p_ptr->skills[SK_CUISINE].skill_rank));
					(void)hp_player(damroll((p_ptr->skills[SK_CUISINE].skill_rank * 2), (p_ptr->skills[SK_CUISINE].skill_rank)));
					(void)wp_player(damroll(1, (p_ptr->skills[SK_CUISINE].skill_rank)));
				}
				else
				{
					msg_print("Zis is the ultimate meal!");
					(void)set_food(PY_FOOD_MAX - 1);
					(void)hp_player(damroll((p_ptr->skills[SK_CUISINE].skill_rank * 2), (p_ptr->skills[SK_CUISINE].skill_rank)));
					(void)set_poisoned(p_ptr->poisoned / 2);
					(void)wp_player(p_ptr->skills[SK_CUISINE].skill_rank);
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
						(void)detect_objects_gold(FALSE);
						(void)detect_treasure(FALSE);
					}
					else if (p_ptr->skills[SK_RANSACK].skill_rank < 15)		
					{
						(void)detect_objects_gold(FALSE);
						(void)detect_treasure(FALSE);
						(void)detect_objects_normal(FALSE);
					}
					else 
					{
						(void)detect_objects_gold(FALSE);
						(void)detect_treasure(FALSE);
						(void)detect_objects_normal(FALSE);
						(void)detect_objects_magic(FALSE);
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
					(void)detect_stairs(FALSE);
				}
				else if (p_ptr->skills[SK_STONELORE].skill_rank < 10)
				{
					(void)detect_stairs(FALSE);
					(void)detect_doors(FALSE);
				}
				else if (p_ptr->skills[SK_STONELORE].skill_rank < 15)
				{
					(void)detect_traps(FALSE);
					(void)detect_stairs(FALSE);
					(void)detect_doors(FALSE);
				}
				else
				{
					(void)map_area();
					(void)detect_traps(FALSE);
					(void)detect_stairs(FALSE);
					(void)detect_doors(FALSE);
				}
				
			}
			if (command == -2)
			{
				if (racial_race_aux(20, 40, A_EGO, 0, SK_STONELORE))
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
						(void)detect_stairs(FALSE);
						(void)detect_doors(FALSE);
					}
					else if (p_ptr->skills[SK_UTILITY_CYPHER].skill_rank < 8)
					{
						(void)detect_stairs(FALSE);
						(void)detect_doors(FALSE);
						(void)detect_traps(FALSE);
					}
					else if (p_ptr->skills[SK_UTILITY_CYPHER].skill_rank < 12)
					{
						(void)detect_stairs(FALSE);
						(void)detect_doors(FALSE);
						(void)detect_traps(FALSE);
						detect_monsters_normal(FALSE);
					}
					else if (p_ptr->skills[SK_UTILITY_CYPHER].skill_rank < 16)
					{
						(void)detect_stairs(FALSE);
						(void)detect_doors(FALSE);
						(void)detect_traps(FALSE);
						detect_monsters_normal(FALSE);
						detect_monsters_invis(FALSE);
					}
					else
					{
						(void)detect_stairs(FALSE);
						(void)detect_doors(FALSE);
						(void)detect_traps(FALSE);
						detect_monsters_normal(FALSE);
						detect_monsters_invis(FALSE);
						(void)detect_treasure(FALSE);
						(void)detect_objects_gold(FALSE);
						(void)detect_objects_normal(FALSE);
						(void)detect_objects_magic(FALSE);
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
					else if (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank < 10)
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
					else if (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank < 15)
					{
						(void)set_afraid(0);
						(void)set_hero(p_ptr->hero + (randint(20) + (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank)));
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
						(void)set_afraid(0);
						(void)set_shero(p_ptr->shero + (randint(20) + (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank)));
						(void)hp_player(400);
						(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
						(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
						(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
						(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
						(void)set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + time);
						(void)set_tim_res(RS_ICE, p_ptr->tim_res[RS_ICE] + time);
						(void)set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + time);
						(void)set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + time);
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
		{
			int onslaught, rocketry;
			
			onslaught = rocketry = 0;
			
			if (p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_max > 0)
				onslaught = p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_rank;
			if (p_ptr->skills[SK_ROCKETRY].skill_max > 0)
				rocketry = p_ptr->skills[SK_ROCKETRY].skill_rank;

			if (command == -1)
				if (racial_race_aux(5, 2, A_VIG, 10, SK_ONSLAUGHT_CYPHER))
				{
					/* Guns */
					msg_print ("*THOOM* *THOOM* *THOOM*");
					if (!get_aim_dir(&dir)) return; 
						fire_bolt(GF_SHARDS, dir, damroll(3 + (onslaught / 2), 5));
				}
			if (command == -2)
				if (racial_race_aux(10, 20, A_VIG, 30, SK_ONSLAUGHT_CYPHER))
				{
					/* Rocket */
					msg_print ("You fire a rocket!");
					if (!get_aim_dir(&dir)) return;
					if (rocketry)
					{
						fire_ball(GF_SHARDS, dir, damroll(3 + (onslaught + (rocketry / 2)), 12), ((rocketry / 4) + 3));
					}
					else 
					{
						fire_ball(GF_SHARDS, dir, damroll(3 + (onslaught), 12), ((onslaught / 5) + 1));
					}
				}
			if (command == -3)
				if (racial_race_aux(15, 25, A_MUS, 35, SK_ONSLAUGHT_CYPHER))
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
					if (rocketry)
					{
						(void)fire_barrage(GF_SHARDS, dir, (onslaught + (rocketry / 3)), 
							 	10, (((rocketry) / 4) + 3), 2, 3);
						
					}
					else
					{
						(void)fire_barrage(GF_SHARDS, dir, onslaught, 8, ((onslaught) / 5), 2, 3);
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
					else if (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank < 10)
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
					else if (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank < 15)
					{
						(void)set_afraid(0);
						(void)set_hero(p_ptr->hero + (randint(20) + (p_ptr->skills[SK_SYSTEMS_CYPHER].skill_rank)));
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
						(void)set_afraid(0);
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
						(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
						(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
						(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
						(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
						(void)set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + time);
						(void)set_tim_res(RS_ICE, p_ptr->tim_res[RS_ICE] + time);
						(void)set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + time);
						(void)set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + time);
					}
				}
			}	

			break;
		}
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
					(void)project_ball(-1, (1 + (p_ptr->skills[SK_DEMON_ATTUNE].skill_rank / 4)), p_ptr->py, p_ptr->px, p_ptr->py, p_ptr->px,
										damroll((p_ptr->skills[SK_DEMON_ATTUNE].skill_rank / 2), p_ptr->skills[SK_DEMON_ATTUNE].skill_rank * 2), 
										GF_ECTOPLASM, PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID, 0);
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
					(void)fire_ball(GF_SHARDS, dir,
							 	damroll(((p_ptr->skills[SK_ROCK_TOSS].skill_rank / 2) + 10), 12), (1+ (p_ptr->skills[SK_ROCK_TOSS].skill_rank / 10)));
			}
			break;
			
		case RACE_OGRE:
			if (racial_race_aux(1, 6, A_VIG, 12, SK_BZRK_STR))
			{
				int b = randint(100);
				if (b < 60) msg_print("RAAAAARRRGGGGGGGGH!");
				else if (b < 80) msg_print("grrrrRRRRRROOOOOOOAAAAARRRRR");
				else if (b < 95) msg_print("DIIIEEEEEEEEEARRRRRRGGGGGGGGGGGGGHHHHHHHH!");
				else msg_print("You're making me angry. You won't like me when I'm angry");
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + p_ptr->skills[SK_BZRK_STR].skill_rank + randint(10));
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
			power_desc[0].level = 5;
			power_desc[0].cost = 2;
			power_desc[0].fail = 100 - racial_race_chance(4, A_VIG, 10, SK_ONSLAUGHT_CYPHER);
			strcpy(power_desc[1].name, "Fire 'Fulgurator'");
			power_desc[1].level = 10;
			power_desc[1].cost = 20;
			power_desc[1].fail = 100 - racial_race_chance(10, A_VIG, 30, SK_ONSLAUGHT_CYPHER);
			power_desc[1].number = -2;
			strcpy(power_desc[2].name, "Steam Drill");
			power_desc[2].level = 15;
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
			power_desc[0].fail = 100 - racial_race_chance(4, A_CHR, 10, SK_DEMON_ATTUNE);
			strcpy(power_desc[1].name, "Dark Nexus sphere");
			power_desc[1].level = 14;
			power_desc[1].cost = 20;
			power_desc[1].fail = 100 - racial_race_chance(14, A_VIG, 30, SK_DEMON_ATTUNE);
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
			power_desc[0].cost = 6;
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
		if (p_ptr->muta1 & MUT1_FIRE_BOLT)
		{
			strcpy(power_desc[num].name, "Fire bolt");
			power_desc[num].level = 1;
			power_desc[num].cost = 5;
			power_desc[num].fail = 100 - racial_chance(1, A_MUS, 8);
			power_desc[num++].number = MUT1_FIRE_BOLT;
		}
		if (p_ptr->muta1 & MUT1_FIRE_BALL)
		{
			strcpy(power_desc[num].name, "Fire ball");
			power_desc[num].level = 1;
			power_desc[num].cost = 10;
			power_desc[num].fail = 100 - racial_chance(1, A_MUS, 12);
			power_desc[num++].number = MUT1_FIRE_BALL;
		}
		if (p_ptr->muta1 & MUT1_FIRE_BREATH)
		{
			strcpy(power_desc[num].name, "Flamethrow");
			power_desc[num].level = 1;
			power_desc[num].cost = 18;
			power_desc[num].fail = 100 - racial_chance(1, A_MUS, 22);
			power_desc[num++].number = MUT1_FIRE_BREATH;
		}
		if (p_ptr->muta1 & MUT1_FIRE_STORM)
		{
			strcpy(power_desc[num].name, "Firestorm");
			power_desc[num].level = 1;
			power_desc[num].cost = 65;
			power_desc[num].fail = 100 - racial_chance(1, A_MUS, 35);
			power_desc[num++].number = MUT1_FIRE_STORM;
		}
		if (p_ptr->muta1 & MUT1_EARTH_BOLT)
		{
			strcpy(power_desc[num].name, "Fling rock");
			power_desc[num].level = 1;
			power_desc[num].cost = 5;
			power_desc[num].fail = 100 - racial_chance(1, A_VIG, 8);
			power_desc[num++].number = MUT1_EARTH_BOLT;
		}
		if (p_ptr->muta1 & MUT1_EARTH_SHOWER)
		{
			strcpy(power_desc[num].name, "Earthstorm");
			power_desc[num].level = 1;
			power_desc[num].cost = 65;
			power_desc[num].fail = 100 - racial_chance(1, A_VIG, 33);
			power_desc[num++].number = MUT1_EARTH_SHOWER;
		}
		if (p_ptr->muta1 & MUT1_BIRDS_VIEW)
		{
			strcpy(power_desc[num].name, "Bird's eye");
			power_desc[num].level = 1;
			power_desc[num].cost = 3;
			power_desc[num].fail = 100 - racial_chance(1, A_AGI, 12);
			power_desc[num++].number = MUT1_BIRDS_VIEW;
		}
		if (p_ptr->muta1 & MUT1_CYCLONE)
		{
			strcpy(power_desc[num].name, "Cyclone");
			power_desc[num].level = 1;
			power_desc[num].cost = 35;
			power_desc[num].fail = 100 - racial_chance(1, A_AGI, 40);
			power_desc[num++].number = MUT1_CYCLONE;
		}
		if (p_ptr->muta1 & MUT1_RUSHING_STREAMS)
		{
			strcpy(power_desc[num].name, "Rushing water");
			power_desc[num].level = 1;
			power_desc[num].cost = 16;
			power_desc[num].fail = 100 - racial_chance(1, A_AGI, 22);
			power_desc[num++].number = MUT1_RUSHING_STREAMS;
		}
		if (p_ptr->muta1 & MUT1_BRIBERY)
		{
			strcpy(power_desc[num].name, "Bribery");
			power_desc[num].level = 1;
			power_desc[num].cost = 2;
			power_desc[num].fail = 100 - racial_chance(1, A_CHR, 35);
			power_desc[num++].number = MUT1_BRIBERY;
		}
		if (p_ptr->muta1 & MUT1_EVASION)
		{
			strcpy(power_desc[num].name, "Evasion");
			power_desc[num].level = 1;
			power_desc[num].cost = 5;
			power_desc[num].fail = 100 - racial_chance(1, A_AGI, 5);
			power_desc[num++].number = MUT1_EVASION;
		}
		if (p_ptr->muta1 & MUT1_SPRING)
		{
			strcpy(power_desc[num].name, "Spring");
			power_desc[num].level = 1;
			power_desc[num].cost = 25;
			power_desc[num].fail = 100 - racial_chance(1, A_AGI, 45);
			power_desc[num++].number = MUT1_SPRING;
		}
		if (p_ptr->muta1 & MUT1_BURST)
		{
			strcpy(power_desc[num].name, "Burst");
			power_desc[num].level = 1;
			power_desc[num].cost = 15;
			power_desc[num].fail = 100 - racial_chance(1, A_AGI, 15);
			power_desc[num++].number = MUT1_BURST;
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
/* also checks for new mutations */
void process_mutations(void)
{
	if ((p_ptr->muta4 & MUT4_BERS_RAGE) && !rand_int(3000))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("RAAAAGHH!");
		msg_print("You feel a fit of rage coming over you!");
		(void) set_shero(p_ptr->shero + 10 + randint(p_ptr->lev));
	}

	if ((p_ptr->muta3 & MUT3_COWARDICE) &&
	    !p_ptr->hero && !p_ptr->shero && !rand_int(3000))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("It's so dark... so scary!");
		p_ptr->redraw |= PR_AFRAID;
		p_ptr->afraid = (p_ptr->afraid) + 13 + randint(26);
	}

	if ((p_ptr->muta3 & MUT3_RTELEPORT) && !rand_int(5000))
	{
		disturb(0,0);
		msg_print("Your position suddenly seems very uncertain...");
		msg_print(NULL);
		teleport_player(40);
	}


	if ((p_ptr->muta3 & MUT3_HALLU) && !rand_int(6500))
	{
		if (disturb_minor) disturb(0,0);
		p_ptr->redraw |= PR_EXTRA;
		(void)set_image(p_ptr->image + rand_int(50) + 20);
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
			d_summon = summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_DEMON, FALSE);
		}

		if (d_summon)
		{
			msg_print("You have attracted a demon!");
			disturb(0,0);
		}
	}

	if (p_ptr->muta3 & MUT3_WOUND && !rand_int(3000))
	{
		if (disturb_minor) disturb(0,0);
		msg_print("Your skin rips open!  Ouch!");
		set_cut(p_ptr->cut + rand_int(20) + 10);
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
			d_summon = summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_ANIMAL, FALSE);
		}

		if (d_summon)
		{
			msg_print("You have attracted an animal");
			disturb(0, 0);
		}
	}

	if ((p_ptr->muta4 & MUT4_WRAITH) && 
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

	if ((p_ptr->muta3 & MUT3_ATT_ELEMENTAL) &&
	    !rand_int(6500))
	{
		bool d_summon = FALSE;

		if (!rand_int(5))
		{
		}
		else
		{
			d_summon = summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_ELEMENTAL, FALSE);
		}

		if (d_summon)
		{
			msg_print("You have attracted a elemental!");
			disturb(0,0);
		}
	}

	if ((p_ptr->muta4 & MUT4_WEIRD_MIND) &&
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

	/* MUT3_WARNING now detects monsters at random. -- Gumby */
	if ((p_ptr->muta4 & MUT4_WARNING) && !rand_int(1000))
	{
		detect_monsters_normal(FALSE);
	}
	
	if ((p_ptr->muta4 & MUT4_INVULN) && 
	    !rand_int(5000) && !p_ptr->invuln)
	{
		if (disturb_minor) disturb(0, 0);
		(void)set_invuln(p_ptr->invuln + randint(5) + 5);
	}


	if ((p_ptr->muta3 & MUT3_DISARM) && (!rand_int(10000)) &&
	    (inventory[INVEN_WIELD].k_idx))
	{
		object_type *o_ptr;

		disturb(0, 0); bell("You trip over your own feet!");
		take_hit(randint(p_ptr->wt / 6), "tripping", TRUE);

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
	if (p_ptr->muta5 & MUT5_HYPER_STR)	p_ptr->stat_add[A_MUS] += 8;
	if (p_ptr->muta5 & MUT5_PUNY)		p_ptr->stat_add[A_MUS] -= 8;

	if (p_ptr->muta5 & MUT5_HYPER_INT)
	{
		p_ptr->stat_add[A_SCH] += 8;
		p_ptr->stat_add[A_EGO] += 8;
	}

	if (p_ptr->muta5 & MUT5_MORONIC)
	{
		p_ptr->stat_add[A_SCH] -= 8;
		p_ptr->stat_add[A_EGO] -= 8;
	}

	if (p_ptr->muta5 & MUT5_RESILIENT)	p_ptr->stat_add[A_VIG] += 8;

	if (p_ptr->muta5 & MUT5_XTRA_FAT)
	{
		p_ptr->stat_add[A_VIG] += 4;
		p_ptr->pspeed -= 2;
	}

	if (p_ptr->muta5 & MUT5_ALBINO)		p_ptr->stat_add[A_VIG] -= 6;

	if (p_ptr->muta5 & MUT5_FLESH_ROT)
	{
		p_ptr->stat_add[A_VIG] -= 4;
		p_ptr->stat_add[A_CHR] -= 2;
		p_ptr->regenerate_25 = FALSE;
		p_ptr->regenerate_50 = FALSE;
		p_ptr->regenerate_75 = FALSE;
	}

	if (p_ptr->muta5 & MUT5_SILLY_VOI)	p_ptr->stat_add[A_CHR] -= 8;
	if (p_ptr->muta5 & MUT5_BLANK_FAC)	p_ptr->stat_add[A_CHR] -= 4;
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
	if (p_ptr->muta5 & MUT5_MAGIC_RES) p_ptr->skill_sav += 6;

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
		p_ptr->res[RS_ELC] += 45;
		p_ptr->dis_res[RS_ELC] += 45;	
	}

	if (p_ptr->muta5 & MUT5_FIRE_BODY)
	{
	/* Add in the auras! */
		p_ptr->sh_fire = TRUE;
		p_ptr->res[RS_FIR] += 45;
		p_ptr->dis_res[RS_FIR] += 45;	
		p_ptr->lite = TRUE;
	}

	if (p_ptr->muta5 & MUT5_WART_SKIN)
	{
		p_ptr->stat_add[A_CHR] -= 4;
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
	}

	if (p_ptr->muta5 & MUT5_SCALES)
	{
		p_ptr->stat_add[A_CHR] -= 2;
		p_ptr->to_a += 10;
		p_ptr->dis_to_a += 10;
	}

	if (p_ptr->muta5 & MUT5_IRON_SKIN)
	{
		p_ptr->stat_add[A_AGI] -= 4;
		p_ptr->to_a += 25;
		p_ptr->dis_to_a += 25;
	}

	if (p_ptr->muta5 & MUT5_WINGS)		p_ptr->ffall = TRUE;
	if (p_ptr->muta5 & MUT5_FEARLESS)	p_ptr->resist_fear = TRUE;
	if (p_ptr->muta5 & MUT5_REGEN)		p_ptr->regenerate_75 = TRUE;
	if (p_ptr->muta5 & MUT5_ESP)		p_ptr->telepathy = TRUE;
	if (p_ptr->muta5 & MUT5_TWISTED)	p_ptr->stat_add[A_CHR] -= 6;
	if (p_ptr->muta5 & MUT5_SPINES)		p_ptr->sh_spine = TRUE;
	if (p_ptr->muta5 & MUT5_LIMBER)		p_ptr->stat_add[A_AGI] += 6;
	if (p_ptr->muta5 & MUT5_ARTHRITIS)	p_ptr->stat_add[A_AGI] -= 6;

	if (p_ptr->muta5 & MUT5_GLOW)
	{
		p_ptr->res[RS_LIT] += 30;
		p_ptr->dis_res[RS_LIT] += 30;	
		p_ptr->res[RS_DRK] += 30;
		p_ptr->dis_res[RS_DRK] += 30;	
		p_ptr->lite = TRUE;
	}
	if (p_ptr->muta6 & MUT6_ALPHA_EYES) p_ptr->see_infra += 6;
	if (p_ptr->muta6 & MUT6_BETA_EYES)
	{
		p_ptr->see_infra += 6;
		if (p_ptr->skills[SK_SEARCHING_GOOD].skill_max > 0)
		{
			p_ptr->skills[SK_SEARCHING_GOOD].skill_rank = 3 * (p_ptr->skills[SK_SEARCHING_GOOD].skill_rank);
		}
		if (p_ptr->skills[SK_SEARCHING_NORM].skill_max > 0)
		{
			p_ptr->skills[SK_SEARCHING_NORM].skill_rank = 3 *(p_ptr->skills[SK_SEARCHING_NORM].skill_rank);
		}
		if (p_ptr->skills[SK_SEARCHING_POOR].skill_max > 0)
		{
			p_ptr->skills[SK_SEARCHING_POOR].skill_rank = 2*(p_ptr->skills[SK_SEARCHING_POOR].skill_rank);
		}
	}
	if (p_ptr->muta6 & MUT6_GAMMA_EYES)
	{
		p_ptr->see_infra += 6;
		p_ptr->res[RS_LIT] += 60;
		p_ptr->dis_res[RS_LIT] += 60;	
		p_ptr->res[RS_DRK] += 60;
		p_ptr->dis_res[RS_DRK] += 60;	
		if (p_ptr->skills[SK_SEARCHING_GOOD].skill_max > 0)
		{
			p_ptr->skills[SK_SEARCHING_GOOD].skill_rank = 3 * (p_ptr->skills[SK_SEARCHING_GOOD].skill_rank);
		}
		if (p_ptr->skills[SK_SEARCHING_NORM].skill_max > 0)
		{
			p_ptr->skills[SK_SEARCHING_NORM].skill_rank = 3 *(p_ptr->skills[SK_SEARCHING_NORM].skill_rank);
		}
		if (p_ptr->skills[SK_SEARCHING_POOR].skill_max > 0)
		{
			p_ptr->skills[SK_SEARCHING_POOR].skill_rank = 2*(p_ptr->skills[SK_SEARCHING_POOR].skill_rank);
		}

	}
	if (p_ptr->muta6 & MUT6_DELTA_EYES)
	{
		p_ptr->see_inv = TRUE;
		p_ptr->resist_blind = TRUE;
		p_ptr->see_infra += 6;
		p_ptr->res[RS_LIT] += 100;
		p_ptr->dis_res[RS_LIT] += 100;	
		p_ptr->res[RS_DRK] += 100;
		p_ptr->dis_res[RS_DRK] += 100;	
		if (p_ptr->skills[SK_SEARCHING_GOOD].skill_max > 0)
		{
			p_ptr->skills[SK_SEARCHING_GOOD].skill_rank = 3 * (p_ptr->skills[SK_SEARCHING_GOOD].skill_rank);
		}
		if (p_ptr->skills[SK_SEARCHING_NORM].skill_max > 0)
		{
			p_ptr->skills[SK_SEARCHING_NORM].skill_rank = 3 *(p_ptr->skills[SK_SEARCHING_NORM].skill_rank);
		}
		if (p_ptr->skills[SK_SEARCHING_POOR].skill_max > 0)
		{
			p_ptr->skills[SK_SEARCHING_POOR].skill_rank = 2*(p_ptr->skills[SK_SEARCHING_POOR].skill_rank);
		}
	}
	if (p_ptr->muta6 & MUT6_ALPHA_REFLEX) p_ptr->pspeed += 2;
	if (p_ptr->muta6 & MUT6_BETA_REFLEX)
	{
		p_ptr->pspeed += 3;
		p_ptr->stat_add[A_AGI] += 2;
	}
	if (p_ptr->muta6 & MUT6_GAMMA_REFLEX)
	{
		p_ptr->pspeed += 4;
		p_ptr->stat_add[A_AGI] += 6;
	}
	if (p_ptr->muta6 & MUT6_DELTA_REFLEX)
	{
		p_ptr->pspeed += 5;
		p_ptr->stat_add[A_AGI] += 10;
		p_ptr->to_a += 10;
		p_ptr->dis_to_a += 10;
	}
	if (p_ptr->muta6 & MUT6_ALPHA_PLATING)
	{
		p_ptr->to_a += 10;
		p_ptr->dis_to_a += 10;
	}
	if (p_ptr->muta6 & MUT6_BETA_PLATING)
	{
		p_ptr->pspeed -= 1;
		p_ptr->to_a += 20;
		p_ptr->dis_to_a += 20;
	}
	if (p_ptr->muta6 & MUT6_GAMMA_PLATING)
	{
		p_ptr->pspeed -= 2;
		p_ptr->to_a += 40;
		p_ptr->dis_to_a += 40;
	}
	if (p_ptr->muta6 & MUT6_DELTA_PLATING)
	{
		p_ptr->to_a += 70;
		p_ptr->dis_to_a += 70;
	}
	if (p_ptr->muta6 & MUT6_ALPHA_CORE) p_ptr->health_bonus += 20;
	if (p_ptr->muta6 & MUT6_BETA_CORE) p_ptr->health_bonus += 60;
	if (p_ptr->muta6 & MUT6_GAMMA_CORE) p_ptr->health_bonus += 100;
	if (p_ptr->muta6 & MUT6_DELTA_CORE) p_ptr->health_bonus += 200;
}





