/* File: learn.c */

/* Purpose: Allow player to learn(and use) monsters attacks! */
/* Also allow to learn abilities */
/* The new spellcasting system is built in this file! */
/* Items may also 'learn' abilities in this file! */

#include "angband.h"


/* Maximum number of tries for teleporting */
#define MAX_TRIES 300


int use_monster_power(bool great, bool only_number)
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;
        int     magictype;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;
        s32b brdam = 0;
        s32b dieroll;

        char            choice, ch;

	char            out_val[160];
        int k,count;
        int rlev = dun_level + p_ptr->lev + randint(5);
	int rad;
        int bonusdam;

        if (p_ptr->csp < 5)
        {
                msg_print("A monster magic needs 5 mp to be casted!");
                return;
        }

        brdam = (p_ptr->chp * 2);
        brdam += ((brdam * ((p_ptr->abilities[(CLASS_MONSTER_MAGE)]-1)*10))/100);
        dieroll = p_ptr->lev;
        dieroll += ((dieroll * ((p_ptr->abilities[(CLASS_MONSTER_MAGE)]-1)*10))/100);

        if (!get_com("Which type? A, B, C, D", &ch)) return 0;
        if (ch == 'A' || ch == 'a') magictype = 1;
        else if (ch == 'B' || ch == 'b') magictype = 2;
        else if (ch == 'C' || ch == 'c') magictype = 3;
        else if (ch == 'D' || ch == 'd') magictype = 4;

        /* List the powers */
        if(p_ptr->monster_magic & RF4_SHRIEK && magictype == 1) {strcpy(power_desc[num],"Agravate monsters");powers[num++]=0;}
        if(great) if(p_ptr->monster_magic & RF4_ROCKET && magictype == 1) {strcpy(power_desc[num],"Rocket");powers[num++]=1;}
        if(p_ptr->monster_magic & RF4_ARROW_1 && magictype == 1) {strcpy(power_desc[num],"Arrow1");powers[num++]=2;}
        if(p_ptr->monster_magic & RF4_ARROW_2 && magictype == 1) {strcpy(power_desc[num],"Arrow2");powers[num++]=3;}
        if(great) if(p_ptr->monster_magic & RF4_ARROW_3 && magictype == 1) {strcpy(power_desc[num],"Arrow3");powers[num++]=4;}
        if(great) if(p_ptr->monster_magic & RF4_ARROW_4 && magictype == 1) {strcpy(power_desc[num],"Arrow4");powers[num++]=5;}
        if(great) if(p_ptr->monster_magic & RF4_BR_ACID && magictype == 1) {strcpy(power_desc[num],"Breathe Acid");powers[num++]=6;}
        if(great) if(p_ptr->monster_magic & RF4_BR_ELEC && magictype == 1) {strcpy(power_desc[num],"Breathe Lightning");powers[num++]=7;}
        if(p_ptr->monster_magic & RF4_BR_FIRE && magictype == 1) {strcpy(power_desc[num],"Breathe Fire");powers[num++]=8;}
        if(p_ptr->monster_magic & RF4_BR_COLD && magictype == 1) {strcpy(power_desc[num],"Breathe Cold");powers[num++]=9;}
        if(great) if(p_ptr->monster_magic & RF4_BR_POIS && magictype == 1) {strcpy(power_desc[num],"Breathe Poison");powers[num++]=10;}
        if(great) if(p_ptr->monster_magic & RF4_BR_NETH && magictype == 1) {strcpy(power_desc[num],"Breathe Nether");powers[num++]=11;}
        if(p_ptr->monster_magic & RF4_BR_LITE && magictype == 1) {strcpy(power_desc[num],"Breathe Lite");powers[num++]=12;}
        if(great) if(p_ptr->monster_magic & RF4_BR_DARK && magictype == 1) {strcpy(power_desc[num],"Breathe Darkness");powers[num++]=13;}
        if(great) if(p_ptr->monster_magic & RF4_BR_CONF && magictype == 1) {strcpy(power_desc[num],"Breathe Confusion");powers[num++]=14;}
        if(great) if(p_ptr->monster_magic & RF4_BR_SOUN && magictype == 1) {strcpy(power_desc[num],"Breathe Sound");powers[num++]=15;}
        if(great) if(p_ptr->monster_magic & RF4_BR_CHAO && magictype == 1) {strcpy(power_desc[num],"Breathe Chaos");powers[num++]=16;}
        if(great) if(p_ptr->monster_magic & RF4_BR_DISE && magictype == 1) {strcpy(power_desc[num],"Breathe Disenchantment");powers[num++]=17;}
        if(great) if(p_ptr->monster_magic & RF4_BR_NEXU && magictype == 1) {strcpy(power_desc[num],"Breathe Nexus");powers[num++]=18;}
        if(great) if(p_ptr->monster_magic & RF4_BR_TIME && magictype == 1) {strcpy(power_desc[num],"Breathe Time");powers[num++]=19;}
        if(great) if(p_ptr->monster_magic & RF4_BR_INER && magictype == 1) {strcpy(power_desc[num],"Breathe Inertia");powers[num++]=20;}
        if(great) if(p_ptr->monster_magic & RF4_BR_GRAV && magictype == 1) {strcpy(power_desc[num],"Breathe Gravity");powers[num++]=21;}
        if(great) if(p_ptr->monster_magic & RF4_BR_SHAR && magictype == 1) {strcpy(power_desc[num],"Breathe Shards");powers[num++]=22;}
        if(great) if(p_ptr->monster_magic & RF4_BR_PLAS && magictype == 1) {strcpy(power_desc[num],"Breathe Plasma");powers[num++]=23;}
        if(great) if(p_ptr->monster_magic & RF4_BR_WALL && magictype == 1) {strcpy(power_desc[num],"Breathe Force");powers[num++]=24;}
        if(great) if(p_ptr->monster_magic & RF4_BR_MANA && magictype == 1) {strcpy(power_desc[num],"Breathe Mana");powers[num++]=25;}
        if(great) if(p_ptr->monster_magic & RF4_BA_NUKE && magictype == 1) {strcpy(power_desc[num],"Nuke Ball");powers[num++]=26;}
        if(great) if(p_ptr->monster_magic & RF4_BR_NUKE && magictype == 1) {strcpy(power_desc[num],"Breathe Nuke");powers[num++]=27;}
        if(great) if(p_ptr->monster_magic & RF4_BA_CHAO && magictype == 1) {strcpy(power_desc[num],"Chaos Ball");powers[num++]=28;}
        if(great) if(p_ptr->monster_magic & RF4_BR_DISI && magictype == 1) {strcpy(power_desc[num],"Breathe Disintegration");powers[num++]=29;}
        if(great) if(p_ptr->monster_magic2 & RF5_BA_ACID && magictype == 2) {strcpy(power_desc[num],"Acid Ball");powers[num++]=30;}
        if(great) if(p_ptr->monster_magic2 & RF5_BA_ELEC && magictype == 2) {strcpy(power_desc[num],"Lightning Ball");powers[num++]=31;}
        if(great) if(p_ptr->monster_magic2 & RF5_BA_FIRE && magictype == 2) {strcpy(power_desc[num],"Fire Ball");powers[num++]=32;}
        if(great) if(p_ptr->monster_magic2 & RF5_BA_COLD && magictype == 2) {strcpy(power_desc[num],"Cold Ball");powers[num++]=33;}
        if(great) if(p_ptr->monster_magic2 & RF5_BA_POIS && magictype == 2) {strcpy(power_desc[num],"Poison Ball");powers[num++]=34;}
        if(great) if(p_ptr->monster_magic2 & RF5_BA_NETH && magictype == 2) {strcpy(power_desc[num],"Nether Ball");powers[num++]=35;}
        if(great) if(p_ptr->monster_magic2 & RF5_BA_WATE && magictype == 2) {strcpy(power_desc[num],"Water Ball");powers[num++]=36;}
        if(great) if(p_ptr->monster_magic2 & RF5_BA_MANA && magictype == 2) {strcpy(power_desc[num],"Mana Ball");powers[num++]=37;}
        if(great) if(p_ptr->monster_magic2 & RF5_BA_DARK && magictype == 2) {strcpy(power_desc[num],"Darkness Ball");powers[num++]=38;}
        if(p_ptr->monster_magic2 & RF5_CAUSE_1 && magictype == 2) {strcpy(power_desc[num],"Cause light wounds");powers[num++]=42;}
        if(p_ptr->monster_magic2 & RF5_CAUSE_2 && magictype == 2) {strcpy(power_desc[num],"Cause medium wounds");powers[num++]=43;}
        if(p_ptr->monster_magic2 & RF5_CAUSE_3 && magictype == 2) {strcpy(power_desc[num],"Cause critical wounds");powers[num++]=44;}
        if(p_ptr->monster_magic2 & RF5_CAUSE_4 && magictype == 2) {strcpy(power_desc[num],"Cause mortal wounds");powers[num++]=45;}
        if(p_ptr->monster_magic2 & RF5_BO_ACID && magictype == 2) {strcpy(power_desc[num],"Acid Bolt");powers[num++]=46;}
        if(p_ptr->monster_magic2 & RF5_BO_ELEC && magictype == 2) {strcpy(power_desc[num],"Lightning Bolt");powers[num++]=47;}
        if(p_ptr->monster_magic2 & RF5_BO_FIRE && magictype == 2) {strcpy(power_desc[num],"Fire Bolt");powers[num++]=48;}
        if(p_ptr->monster_magic2 & RF5_BO_COLD && magictype == 2) {strcpy(power_desc[num],"Cold Bolt");powers[num++]=49;}
        if(p_ptr->monster_magic2 & RF5_BO_POIS && magictype == 2) {strcpy(power_desc[num],"Poison Bolt");powers[num++]=56;}
        if(p_ptr->monster_magic2 & RF5_BO_NETH && magictype == 2) {strcpy(power_desc[num],"Nether Bolt");powers[num++]=50;}
        if(p_ptr->monster_magic2 & RF5_BO_WATE && magictype == 2) {strcpy(power_desc[num],"Water Bolt");powers[num++]=51;}
        if(p_ptr->monster_magic2 & RF5_BO_MANA && magictype == 2) {strcpy(power_desc[num],"Mana Bolt");powers[num++]=52;}
        if(p_ptr->monster_magic2 & RF5_BO_PLAS && magictype == 2) {strcpy(power_desc[num],"Plasma Bolt");powers[num++]=53;}
        if(p_ptr->monster_magic2 & RF5_BO_ICEE && magictype == 2) {strcpy(power_desc[num],"Ice Bolt");powers[num++]=54;}
        if(p_ptr->monster_magic2 & RF5_MISSILE && magictype == 2) {strcpy(power_desc[num],"Missile");powers[num++]=55;}
        if(p_ptr->monster_magic2 & RF5_SCARE && magictype == 2) {strcpy(power_desc[num],"Scare");powers[num++]=41;}
        if(p_ptr->monster_magic2 & RF5_BLIND && magictype == 2) {strcpy(power_desc[num],"Blindness");powers[num++]=57;}
        if(p_ptr->monster_magic2 & RF5_CONF && magictype == 2) {strcpy(power_desc[num],"Confusion");powers[num++]=58;}
        if(p_ptr->monster_magic2 & RF5_SLOW && magictype == 2) {strcpy(power_desc[num],"Slow");powers[num++]=59;}
        if(p_ptr->monster_magic2 & RF5_HOLD && magictype == 2) {strcpy(power_desc[num],"Paralyse");powers[num++]=60;}
        if(p_ptr->monster_magic3 & RF6_HASTE && magictype == 3) {strcpy(power_desc[num],"Haste Self");powers[num++]=61;}
        if(great) if(p_ptr->monster_magic3 & RF6_HAND_DOOM && magictype == 3) {strcpy(power_desc[num],"Hand of Doom");powers[num++]=62;}
        if(p_ptr->monster_magic3 & RF6_HEAL && magictype == 3) {strcpy(power_desc[num],"Heal");powers[num++]=63;}
        if(p_ptr->monster_magic3 & RF6_BLINK && magictype == 3) {strcpy(power_desc[num],"Blink");powers[num++]=64;}
        if(p_ptr->monster_magic3 & RF6_TPORT && magictype == 3) {strcpy(power_desc[num],"Teleport");powers[num++]=65;}
        if(great) if(p_ptr->monster_magic3 & RF6_TELE_TO && magictype == 3) {strcpy(power_desc[num],"Teleport To");powers[num++]=66;}
        if(p_ptr->monster_magic3 & RF6_TELE_AWAY && magictype == 3) {strcpy(power_desc[num],"Teleport Away");powers[num++]=67;}
        if(great) if(p_ptr->monster_magic3 & RF6_TELE_LEVEL && magictype == 3) {strcpy(power_desc[num],"Teleport Level");powers[num++]=68;}
        if(p_ptr->monster_magic3 & RF6_DARKNESS && magictype == 3) {strcpy(power_desc[num],"Darkness");powers[num++]=69;}
        if(great) if(p_ptr->monster_magic3 & RF6_TRAPS && magictype == 3) {strcpy(power_desc[num],"Create Traps");powers[num++]=88;}
        if(great) if(p_ptr->monster_magic3 & RF6_RAISE_DEAD && magictype == 3) {strcpy(power_desc[num],"Raise the Dead");powers[num++]=89;}
        if(p_ptr->monster_magic3 & RF6_S_BUG && magictype == 3) {strcpy(power_desc[num],"Summon Sofware Bugs");powers[num++]=70;}
        if(p_ptr->monster_magic3 & RF6_S_RNG && magictype == 3) {strcpy(power_desc[num],"Summon RNG");powers[num++]=71;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_DRAGONRIDER && magictype == 3) {strcpy(power_desc[num],"Summon DragonRider");powers[num++]=72;}
        if(p_ptr->monster_magic3 & RF6_S_KIN && magictype == 3) {strcpy(power_desc[num],"Summon Kin");powers[num++]=73;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_CYBER && magictype == 3) {strcpy(power_desc[num],"Summon Cyberdemon");powers[num++]=74;}
        if(p_ptr->monster_magic3 & RF6_S_MONSTER && magictype == 3) {strcpy(power_desc[num],"Summon Monster");powers[num++]=75;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_MONSTERS && magictype == 3) {strcpy(power_desc[num],"Summon Monsters");powers[num++]=76;}
        if(p_ptr->monster_magic3 & RF6_S_ANT && magictype == 3) {strcpy(power_desc[num],"Summon Ants");powers[num++]=77;}
        if(p_ptr->monster_magic3 & RF6_S_SPIDER && magictype == 3) {strcpy(power_desc[num],"Summon Spiders");powers[num++]=78;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_HOUND && magictype == 3) {strcpy(power_desc[num],"Summon Hound");powers[num++]=79;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_HYDRA && magictype == 3) {strcpy(power_desc[num],"Summon Hydras");powers[num++]=80;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_ANGEL && magictype == 3) {strcpy(power_desc[num],"Summon Angel");powers[num++]=81;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_DEMON && magictype == 3) {strcpy(power_desc[num],"Summon Demon");powers[num++]=82;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_UNDEAD && magictype == 3) {strcpy(power_desc[num],"Summon Undead");powers[num++]=83;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_DRAGON && magictype == 3) {strcpy(power_desc[num],"Summon Dragon");powers[num++]=84;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_HI_UNDEAD && magictype == 3) {strcpy(power_desc[num],"Summon High Undead");powers[num++]=85;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_HI_DRAGON && magictype == 3) {strcpy(power_desc[num],"Summon High Dragon");powers[num++]=86;}
        if(great) if(p_ptr->monster_magic3 & RF6_S_WRAITH && magictype == 3) {strcpy(power_desc[num],"Summon Wraith");powers[num++]=87;}
        if(p_ptr->monster_magic3 & RF6_S_UNIQUE && magictype == 3) {strcpy(power_desc[num],"Summon Unique");powers[num++]=91;}
        if(p_ptr->monster_magic4 & RF5_BA_MANA && magictype == 4) {strcpy(power_desc[num],"Mega Mana");powers[num++]=92;}
        if(!num) {msg_print("No powers to use.");return 0;}

        if(only_number) return num;

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which monster magic? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which monster magic? ",
			I2A(0), '0' + num - 27);
	}

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

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
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
                return num;
	}

        rad = 1 + (p_ptr->lev/15);
        p_ptr->csp -= 5;
        update_and_handle();

        switch(Power)
        {
                case 0: /* Shriek */
                        aggravate_monsters(-1);
                        break;
                case 1: /* Rocket */
                        msg_print("You launch a rocket...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ROCKET, dir, p_ptr->chp, 2 + (p_ptr->lev/20));
                        break;
                case 2: /* Arrow1 */
                        msg_print("You fire a light arrow...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(dieroll,dieroll));
                        break;
                case 3: /* Arrow2 */
                        msg_print("You fire a heavy arrow...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(dieroll + 5,dieroll + 5));
                        break;
                case 4: /* Arrow3 */
                        msg_print("You fire a light missile...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(dieroll + 10,dieroll + 10));
                        break;
                case 5: /* Arrow4 */
                        msg_print("You fire a heavy missile...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(dieroll + 15,dieroll + 15));
                        break;
                case 6: /* Br acid */
                        msg_print("You breathe acid ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, brdam, rad);
                        break;
                case 7: /* Br elec */
                        msg_print("You breathe lightning ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, brdam, rad);
                        break;
                case 8: /* br fire */
                        msg_print("You breathe fire ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, brdam, rad);
                        break;
                case 9: /* br cold */
                        msg_print("You breathe cold ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, brdam, rad);
                        break;
                case 10: /* br pois */
                        msg_print("You breathe poison ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, brdam, rad);
                        break;
                case 11: /* br neth */
                        msg_print("You breathe nether ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, brdam, rad);
                        break;
                case 12: /* br lite */
                        msg_print("You breathe lite ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_LITE, dir, brdam, rad);
                        break;
                case 13: /* br dark */
                        msg_print("You breathe dark ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, brdam, rad);
                        break;
                case 14: /* br conf */
                        msg_print("You breathe confusion ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CONFUSION, dir, brdam, rad);
                        break;
                case 15: /* br soun */
                        msg_print("You breathe sound ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SOUND, dir, brdam, rad);
                        break;
                case 16: /* br chao */
                        msg_print("You breathe chaos ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, brdam, rad);
                        break;
                case 17: /* br dise */
                        msg_print("You breathe disenchantment ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISENCHANT, dir, brdam, rad);
                        break;
                case 18: /* br nexu */
                        msg_print("You breathe nexus ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NEXUS, dir, brdam, rad);
                        break;
                case 19: /* br time */
                        msg_print("You breathe time ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_TIME, dir, brdam, rad);
                        break;
                case 20: /* br iner */
                        msg_print("You breathe inertia ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_INERTIA, dir, brdam, rad);
                        break;
                case 21: /* br grav */
                        msg_print("You breathe gravity ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_GRAVITY, dir, brdam, rad);
                        break;
                case 22: /* br shar */
                        msg_print("You breathe shards ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SHARDS, dir, brdam, rad);
                        break;
                case 23: /* br plas */
                        msg_print("You breathe plasma ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_PLASMA, dir, brdam, rad);
                        break;
                case 24: /* br wall */
                        msg_print("You breathe force ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FORCE, dir, brdam, rad);
                        break;
                case 25: /* br mana */
                        msg_print("You breathe mana ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, brdam, rad);
                        break;
                case 26: /* ba nuke */
                        msg_print("You cast a ball of nuke ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, brdam, 1 + (p_ptr->lev/20));
                        break;
                case 27: /* br nuke */
                        msg_print("You breathe nuke ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, brdam, 1 + (p_ptr->lev/20));
                        break;
                case 28: /* ba chao */
                        msg_print("You cast a ball of chaos ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 29: /* br disi */
                        msg_print("You breathe disintegration ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISINTEGRATE, dir, damroll(dieroll,dieroll), 1 + (p_ptr->lev/20));
                        break;
                case 30: /* ba acid */
                        msg_print("You cast a ball of acid ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 31: /* ba elec */
                        msg_print("You cast a ball of lightning ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 32: /* ba fire */
                        msg_print("You cast a ball of fire ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 33: /* ba cold */
                        msg_print("You cast a ball of cold ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 34: /* ba pois */
                        msg_print("You cast a ball of poison ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 35: /* ba neth */
                        msg_print("You cast a ball of nether ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 36: /* ba wate */
                        msg_print("You cast a ball of water ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_WATER, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 37: /* ba mana */
                        msg_print("You cast a ball of mana ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 38: /* ba dark */
                        msg_print("You cast a ball of darkness ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, damroll(dieroll,dieroll), 2);
                        break;
	        case 42: /* cause1 */
		        msg_print("You cause light wounds ...");
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll,dieroll));
			}
		        break;
	        case 43: /* cause2 */
		        msg_print("You cause serious wounds ...");
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 5,dieroll + 5));
			}
		        break;
	        case 44: /* cause3 */
		        msg_print("You cause critical wounds ...");
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 10,dieroll + 10));
			}
		        break;
	        case 45: /* cause4 */
		        msg_print("You cause mortal wounds ...");
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 15,dieroll + 15));
			}
		        break;
                case 46: /* bo acid */
                        msg_print("You cast a bolt of acid ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ACID, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 47: /* bo elec */
                        msg_print("You cast a bolt of lightning ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ELEC, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 48: /* bo fire */
                        msg_print("You cast a bolt of fire ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_FIRE, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 49: /* bo cold */
                        msg_print("You cast a bolt of cold ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_COLD, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 56: /* bo pois */
                        msg_print("You cast a bolt of poison ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_POIS, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 50: /* bo neth */
                        msg_print("You cast a bolt of nether ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_NETHER, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 51: /* bo wate */
                        msg_print("You cast a bolt of water ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_WATER, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 52: /* bo mana */
                        msg_print("You cast a bolt of mana ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MANA, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 53: /* bo plas */
                        msg_print("You cast a bolt of plasma ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_PLASMA, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 54: /* bo ice */
                        msg_print("You cast a bolt of ice ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ICE, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 55: /* missile */
                        msg_print("You cast a magic missile ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MISSILE, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 57: /* blind */
                        msg_print("You cast blindness ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 41: /* scare */
                        msg_print("You cast scare ...");
			if (get_aim_dir(&dir))
				fear_monster(dir, plev);
                        break;
                case 58: /* conf */
                        msg_print("You cast a bolt of confusion ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 59: /* slow */
                        msg_print("You cast a bolt of unspeed ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLOW, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 60: /* hold */
                        msg_print("You cast a bolt of paralisation ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLEEP, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 61: /* haste */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5));
			}
                        break;
                case 62: /* hand of doom */
                        msg_print("You invoke the Hand of Doom ...");
                        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 30,dieroll + 30) + (p_ptr->lev));
			}
                        break;
                case 63: /* heal */
                        hp_player(p_ptr->lev * 10);
                        break;
                case 64: /* Blink */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        teleport_player(10);
                        break;
                case 65: /* Teleport */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        teleport_player(plev * 5);
                        break;
                case 66: /* tele to */
                        {
                             int ii,ij;

                             if(special_flag) {msg_print("No teleport on special levels ...");break;}
                             msg_print("You go between. Input destination.");
                             if (!tgt_pt(&ii,&ij)) return num;
                             p_ptr->energy -= 60 - plev;
                             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                             (distance(ij,ii,py,px) > plev*20 + 2))
                             {
                                 msg_print("You fail to show the destination correctly!");
                                 p_ptr->energy -= 100;
                                 teleport_player(10);
                             }
                             else teleport_player_to(ij,ii);
                        }
                        break;
                case 67: /* tele away */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        if (!get_aim_dir(&dir)) return num;
                        (void)fire_beam(GF_AWAY_ALL, dir, plev);
                        break;
		case 68: /* tele level */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
			teleport_player_level();
			break;
                case 69: /* darkness */
                        (void)project(-1, 3, py, px, 0, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL);
                        /* Unlite up the room */
                        unlite_room(py, px);
                        break;
                case 88: /* create traps */
		        msg_print("You create traps ...");
			trap_creation();
		        break;
                case 89: /* raise the dead - uses the same code as the
			    nether spell*/
		{
			int item, x, y;
			object_type *o_ptr;

			cptr q, s;

			/* Restrict choices to corpses */
			item_tester_tval = TV_CORPSE;

			/* Get an item */
			q = "Use which corpse? ";
			s = "You have no corpse to use.";
			if (!get_item(&item, q, s, (USE_FLOOR))) break;;

			o_ptr = &o_list[0 - item];

                        if(randint(8)>=5-o_ptr->sval){
                                msg_print("You touch the corpse ... the monster raise from the graveyard!");

                                x=px;
                                y=py;
                                get_pos_player(5, &y, &x);
                                place_monster_one(y, x, o_ptr->pval2, FALSE, TRUE);

                                floor_item_increase(0 - item, -1);
                                floor_item_describe(0 - item);
                                floor_item_optimize(0 - item);
                        }
			break;
		}
                case 70: /* Summon bug */
                                msg_format("You magically code some software bugs.");
				for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_BUG, TRUE);
				}
                        break;
                case 71: /* Summon RNG */
                                msg_format("You magically code some RNGs.");
				for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_RNG, TRUE);
				}
                        break;
                case 72: /* Summon dragonrider */
                                msg_format("You magically summon a DragonRider.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_DRAGONRIDER, TRUE);
				}
                        break;
                case 73: /* Summon kin */
                                msg_format("You magically summon some Kins.");
                                summon_kin_type = p_ptr->body_monster; /* Big hack */
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_KIN, TRUE);
				}
                        break;
                case 74: /* Summon cyber */
                                msg_format("You magically summon a Cyberdemon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_CYBER, TRUE);
				}
                        break;
                case 75: /* Summon monster */
                                msg_format("You magically summon a monster.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, 0, TRUE);
				}
                        break;
                case 76: /* Summon monsters */
                                msg_format("You magically summon monsters.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, 0, TRUE);
				}
                        break;
                case 77: /* Summon ant */
                                msg_format("You magically summon ants.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_ANT, TRUE);
				}
                        break;
                case 78: /* Summon spider */
                                msg_format("You magically summon spiders.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_SPIDER, TRUE);
				}
                        break;
                case 79: /* Summon hound */
                                msg_format("You magically summon hounds.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_HOUND, TRUE);
				}
                        break;
                case 80: /* Summon hydra */
                                msg_format("You magically summon hydras.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_HYDRA, TRUE);
				}
                        break;
                case 81: /* Summon angel */
                                msg_format("You magically summon an angel.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_ANGEL, TRUE);
				}
                        break;
                case 82: /* Summon demon */
                                msg_format("You magically summon a demon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_DEMON, TRUE);
				}
                        break;
                case 83: /* Summon undead */
                                msg_format("You magically summon an undead.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_UNDEAD, TRUE);
				}
                        break;
                case 84: /* Summon dragon */
                                msg_format("You magically summon a dragon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_DRAGON, TRUE);
				}
                        break;
                case 85: /* Summon hiundead */
                                msg_format("You magically summon greater undeads.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE);
				}
                        break;
                case 86: /* Summon hidragon */
                                msg_format("You magically summon greater dragons.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_HI_DRAGON_NO_UNIQUES, TRUE);
				}
                        break;
                case 87: /* Summon wraith */
                                msg_format("You magically summon Wraith.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_WRAITH, TRUE);
				}
                        break;
                case 91: /* Summon Unique */
                                msg_format("You magically summon Unique.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_UNIQUE, TRUE);
				}
                        break;
                case 92: /* Mega Mana */
                        msg_print("You cast a gigantic ball of mana!");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, randint(p_ptr->lev * 10)+20+5000, 3);
                        break;
                        
        }
        energy_use = 100;
        return num;
}

/* Allow player to learn an ability */
/* Completely rewritten for NewAngband 1.7.0 */
void learn_ability()
{

	char            choice;
        char            abils[10][50];
        char            tmpstr[50];

        int            selecteditem = 0;
        int            x = 0;
        int            i;

        bool           learning = TRUE;

        if (p_ptr->pclass == CLASS_APPRENTICE && p_ptr->prace != RACE_MONSTER)
        {
                msg_print("This class doesn't have any abilities!");
                return;
        }

        /* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

        while (learning == TRUE)
        {

        /* Prepare the screen */
        for (i = 0; i < SCREEN_HGT; i++)
        {
                roff("\n");
        }
        c_put_str(TERM_WHITE, "ABILITIES                                                                 ", 0, 0);
        c_put_str(TERM_WHITE, "-----------", 1, 0);

        switch(p_ptr->pclass)
        {
                /* Only Monsters may get these abilities... */
                case CLASS_APPRENTICE:
                {
                        sprintf(abils[0], "Improved Attack");
                        sprintf(abils[1], "Improved Defense");
                        sprintf(abils[2], "Improved Magic");
                        sprintf(abils[3], "-");
                        sprintf(abils[4], "-");
                        sprintf(abils[5], "-");
                        sprintf(abils[6], "-");
                        sprintf(abils[7], "-");
                        sprintf(abils[8], "-");
                        sprintf(abils[9], "-");
                        break;
                }

                case CLASS_WARRIOR:
                {
                        sprintf(abils[0], "Spin Attack");
                        sprintf(abils[1], "Strength");
                        sprintf(abils[2], "Increased Life");
                        sprintf(abils[3], "Battle Skill");
                        sprintf(abils[4], "Accurate Strike");
                        sprintf(abils[5], "Hardiness");
                        sprintf(abils[6], "War Cry");
                        sprintf(abils[7], "Counter Attack");
                        sprintf(abils[8], "Leaping Spin");
                        sprintf(abils[9], "Weapon Mastery");
                        break;
                }
                case CLASS_MAGE:
                {
                        sprintf(abils[0], "Mana Boost");
                        sprintf(abils[1], "Force Field");
                        sprintf(abils[2], "Magic Missile");
                        sprintf(abils[3], "Spell Absorbtion");
                        sprintf(abils[4], "Slow Down");
                        sprintf(abils[5], "Mirror Images");
                        sprintf(abils[6], "Damages Curse");
                        sprintf(abils[7], "Drain Object");
                        sprintf(abils[8], "Stone To Gold");
                        sprintf(abils[9], "Animated Knight");
                        break;
                }
                case CLASS_PRIEST:
                {
                        sprintf(abils[0], "Heal");
                        sprintf(abils[1], "Turn Undeads");
                        sprintf(abils[2], "Divine Blood");
                        sprintf(abils[3], "Mace Of Heaven");
                        sprintf(abils[4], "Dark Prayer");
                        sprintf(abils[5], "Holy Might");
                        sprintf(abils[6], "Harm");
                        sprintf(abils[7], "Forbidden Ritual");
                        sprintf(abils[8], "Divine Armor");
                        sprintf(abils[9], "Godly Wrath");
                        break;
                }
                case CLASS_ROGUE:
                {
                        sprintf(abils[0], "Hide In Shadows");
                        sprintf(abils[1], "Backstabbing");
                        sprintf(abils[2], "Dexterity");
                        sprintf(abils[3], "Thievery");
                        sprintf(abils[4], "Spike Trap");
                        sprintf(abils[5], "Poison Weapon");
                        sprintf(abils[6], "Gas Trap");
                        sprintf(abils[7], "Poison Trap");
                        sprintf(abils[8], "Evasion");
                        sprintf(abils[9], "Rogue Mastery");
                        break;
                }
                case CLASS_RANGER:
                {
                        sprintf(abils[0], "Wilderness Lore");
                        sprintf(abils[1], "Forestry");
                        sprintf(abils[2], "Entangle");
                        sprintf(abils[3], "Animal Empathy");
                        sprintf(abils[4], "Call Animal");
                        sprintf(abils[5], "Warp On Trees");
                        sprintf(abils[6], "Summon Nymphs");
                        sprintf(abils[7], "Thorned Vines");
                        sprintf(abils[8], "Sleep Pollen");
                        sprintf(abils[9], "Force Of Nature");
                        break;
                }
                case CLASS_PALADIN:
                {
                        sprintf(abils[0], "Divine Strength");
                        sprintf(abils[1], "Holy Bolt");
                        sprintf(abils[2], "Aura Of Life");
                        sprintf(abils[3], "Smite Evil");
                        sprintf(abils[4], "Blade Of Purity");
                        sprintf(abils[5], "Resist Impure");
                        sprintf(abils[6], "Feat Of Faith");
                        sprintf(abils[7], "Retrograde Darkness");
                        sprintf(abils[8], "Shining Armor");
                        sprintf(abils[9], "Word Of Peace");
                        break;
                }
                case CLASS_MONK:
                {
                        sprintf(abils[0], "Unarmored Combat");
                        sprintf(abils[1], "Spin Kick");
                        sprintf(abils[2], "Hard Kick");
                        sprintf(abils[3], "Grappling Throw");
                        sprintf(abils[4], "Wisdom");
                        sprintf(abils[5], "Ki Punch");
                        sprintf(abils[6], "One With Body & Mind");
                        sprintf(abils[7], "Monk Speed");
                        sprintf(abils[8], "High Somersault");
                        sprintf(abils[9], "Martial Arts Mastery");
                        break;
                }
                case CLASS_ARCHER:
                {
                        sprintf(abils[0], "Accurate Shots");
                        sprintf(abils[1], "Called Shot");
                        sprintf(abils[2], "Piercing Shots");
                        sprintf(abils[3], "Chain Shot");
                        sprintf(abils[4], "Burning Shots");
                        sprintf(abils[5], "Venomous Shots");
                        sprintf(abils[6], "Multiple Arrows");
                        sprintf(abils[7], "Charged Bolt");
                        sprintf(abils[8], "Knocking Pebbles");
                        sprintf(abils[9], "Marksmanship");
                        break;
                }
                

                /* Advanced Classes */
                case CLASS_HIGH_MAGE:
                {
                        sprintf(abils[0], "Increased Mana");
                        sprintf(abils[1], "Magic Blood");
                        sprintf(abils[2], "Return Magic");
                        sprintf(abils[3], "Archmage Spirit");
                        sprintf(abils[4], "Counter Spell");
                        sprintf(abils[5], "Empower Spells");
                        sprintf(abils[6], "School Focus: Elemental");
                        sprintf(abils[7], "School Focus: Alteration");
                        sprintf(abils[8], "School Focus: Conjuration");
                        sprintf(abils[9], "Spell Mastery");
                        break;
                }
                case CLASS_ELEM_LORD:
                {
                        sprintf(abils[0], "%s Ball", get_element_name(p_ptr->elemlord));
                        sprintf(abils[1], "%s Strike", get_element_name(p_ptr->elemlord));
                        sprintf(abils[2], "Shield Of %s", get_element_name(p_ptr->elemlord));
                        sprintf(abils[3], "Fist Of %s", get_element_name(p_ptr->elemlord));
                        sprintf(abils[4], "Piercing Spells: %s", get_element_name(p_ptr->elemlord));
                        sprintf(abils[5], "Aura Of %s", get_element_name(p_ptr->elemlord));
                        sprintf(abils[6], "Explosive Throw");
                        sprintf(abils[7], "Wave Of %s", get_element_name(p_ptr->elemlord));
                        sprintf(abils[8], "Absorb Elemental Energy");
                        sprintf(abils[9], "Mastery Of %s", get_element_name(p_ptr->elemlord));
                        break;
                }
                case CLASS_MONSTER_MAGE:
                {
                        sprintf(abils[0], "Monster Magics");
                        sprintf(abils[1], "Morphing Abilities");
                        sprintf(abils[2], "Constitution");
                        sprintf(abils[3], "Monstrous Leadership");
                        sprintf(abils[4], "Monstrous Martial Arts");
                        sprintf(abils[5], "Dominate Monster");
                        sprintf(abils[6], "Monstrous Defense");
                        sprintf(abils[7], "Monstrous Wave");
                        sprintf(abils[8], "Monstrous Brutality");
                        sprintf(abils[9], "Improved Life Force");
                        break;
                }
                case CLASS_DEFENDER:
                {
                        sprintf(abils[0], "Heavy Armored Defense");
                        sprintf(abils[1], "Iron Skin");
                        sprintf(abils[2], "Shield Bash");
                        sprintf(abils[3], "Shield Fighting");
                        sprintf(abils[4], "Block Magic");
                        sprintf(abils[5], "Great Guard");
                        sprintf(abils[6], "Armored Health");
                        sprintf(abils[7], "Defensive Strike");
                        sprintf(abils[8], "Boomerang Shield");
                        sprintf(abils[9], "Murderous Defense");
                        break;
                }
                case CLASS_JUSTICE_WARRIOR:
                {
                        sprintf(abils[0], "Shatter Evil");
                        sprintf(abils[1], "Angelic Voice");
                        sprintf(abils[2], "Aura Of Evil Repulsing");
                        sprintf(abils[3], "Bless Weapon");
                        sprintf(abils[4], "School Focus: Healing");
                        sprintf(abils[5], "Sacred Light");
                        sprintf(abils[6], "Slay Evil");
                        sprintf(abils[7], "Angelic Call");
                        sprintf(abils[8], "Protection From Evil");
                        sprintf(abils[9], "Light Of Life");
                        break;
                }
                case CLASS_ZELAR:
                {
                        sprintf(abils[0], "Energy Spin");
                        sprintf(abils[1], "Wave Kick");
                        sprintf(abils[2], "Arms Crush");
                        sprintf(abils[3], "Energy Punch");
                        sprintf(abils[4], "Legs Breaking Throw");
                        sprintf(abils[5], "Disabling Blows");
                        sprintf(abils[6], "Energize Self");
                        sprintf(abils[7], "Dual Wave Fist");
                        sprintf(abils[8], "Gather Power");
                        sprintf(abils[9], "Legendary Agility");
                        break;
                }
                case CLASS_SOUL_GUARDIAN:
                {
                        sprintf(abils[0], "Soul Power");
                        sprintf(abils[1], "Simulacrum");
                        sprintf(abils[2], "Soul Bind");
                        sprintf(abils[3], "Sealing Light");
                        sprintf(abils[4], "Soul Energize");
                        sprintf(abils[5], "Soul Guide");
                        sprintf(abils[6], "Soul Strike");
                        sprintf(abils[7], "Soul Shield");
                        sprintf(abils[8], "Soul Guard");
                        sprintf(abils[9], "Wrath Of Souls");
                        break;
                }
                case CLASS_SHADOW:
                {
                        sprintf(abils[0], "Stealth Attack");
                        sprintf(abils[1], "Displacement");
                        sprintf(abils[2], "Shadow Cloak");
                        sprintf(abils[3], "Shadow Ball");
                        sprintf(abils[4], "Shadow Run");
                        sprintf(abils[5], "Shadow Phase");
                        sprintf(abils[6], "Dark Mist");
                        sprintf(abils[7], "One With Shadows");
                        sprintf(abils[8], "Shadow Magic");
                        sprintf(abils[9], "Storm Of Shadow Edges");
                        break;
                }


        }
        if (p_ptr->prace != RACE_MONSTER)
        {
                for (x = 0; x < 10; x++)
                {
                        int abilnum = (p_ptr->pclass * 10) + x;
                        int abilitynumber = x + 1;                
                        char abil[80];

                        if (abilitynumber > 9) abilitynumber = 0;
                        sprintf (abil, "%d.  %s [%d]", abilitynumber, abils[x], p_ptr->abilities[abilnum]);
                        if (p_ptr->abilities[abilnum] <= 0) c_put_str(TERM_L_DARK, abil, (3 + x), 0);
                        else c_put_str(TERM_WHITE, abil, (3 + x), 0);
                }
        }
        else
        {
                for (x = 0; x < 3; x++)
                {
                        int abilnum = (p_ptr->pclass * 10) + x;
                        int abilitynumber = x + 1;                
                        char abil[80];

                        if (abilitynumber > 9) abilitynumber = 0;
                        sprintf (abil, "%d.  %s [%d]", abilitynumber, abils[x], p_ptr->abilities[abilnum]);
                        if (p_ptr->abilities[abilnum] <= 0) c_put_str(TERM_L_DARK, abil, (3 + x), 0);
                        else c_put_str(TERM_WHITE, abil, (3 + x), 0);
                }
        }        

        sprintf(tmpstr, "%ld  ", p_ptr->ability_points);
        c_put_str(TERM_WHITE, "Remaining AP: ", 15, 0);
        c_put_str(TERM_L_GREEN, tmpstr, 15, 14);
        c_put_str(TERM_WHITE, "Choose an ability to increase, x to exit.", 17, 0);

        choice = inkey();
        if (choice == '1')
        {
                int abilnum = (p_ptr->pclass * 10);
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
                        else
                        {
                                if (p_ptr->abilities[abilnum] <= 0) p_ptr->num_abilities += 1;
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '2')
        {
                int abilnum = (p_ptr->pclass * 10) + 1;
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
                        else
                        {
                                if (p_ptr->abilities[abilnum] <= 0) p_ptr->num_abilities += 1;
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '3')
        {
                int abilnum = (p_ptr->pclass * 10) + 2;
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
                        else
                        {
                                if (p_ptr->abilities[abilnum] <= 0) p_ptr->num_abilities += 1;
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '4')
        {
                int abilnum = (p_ptr->pclass * 10) + 3;
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
                        else
                        {
                                if (p_ptr->abilities[abilnum] <= 0) p_ptr->num_abilities += 1;
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '5')
        {                
                int abilnum = (p_ptr->pclass * 10) + 4;
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
                        else
                        {
                                if (p_ptr->abilities[abilnum] <= 0) p_ptr->num_abilities += 1;
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '6')
        {                
                int abilnum = (p_ptr->pclass * 10) + 5;
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
                        else
                        {
                                if (p_ptr->abilities[abilnum] <= 0) p_ptr->num_abilities += 1;
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '7')
        {
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->class_level[p_ptr->pclass] >= 6)
                {
                        int abilnum = (p_ptr->pclass * 10) + 6;
                        if (p_ptr->ability_points > 0)
                        {
                                if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
                                else
                                {
                                        if (p_ptr->abilities[abilnum] <= 0) p_ptr->num_abilities += 1;
                                        p_ptr->abilities[abilnum] += 1;
                                        p_ptr->ability_points -= 1;
                                }
                        }
                }
                else {
                        msg_print("You must first reach class level 6!");
                        inkey();
                }
        }
        if (choice == '8')
        {
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->class_level[p_ptr->pclass] >= 6)
                {
                        int abilnum = (p_ptr->pclass * 10) + 7;
                        if (p_ptr->ability_points > 0)
                        {
                                if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
                                else
                                {
                                        if (p_ptr->abilities[abilnum] <= 0) p_ptr->num_abilities += 1;
                                        p_ptr->abilities[abilnum] += 1;
                                        p_ptr->ability_points -= 1;
                                }
                        }
                }
                else {
                        msg_print("You must first reach class level 6!");
                        inkey();
                }
        }
        if (choice == '9')
        {
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->class_level[p_ptr->pclass] >= 6)
                {
                        int abilnum = (p_ptr->pclass * 10) + 8;
                        if (p_ptr->ability_points > 0)
                        {
                                if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
                                else
                                {
                                        if (p_ptr->abilities[abilnum] <= 0) p_ptr->num_abilities += 1;
                                        p_ptr->abilities[abilnum] += 1;
                                        p_ptr->ability_points -= 1;
                                }
                        }
                }
                else {
                        msg_print("You must first reach class level 6!");
                        inkey();
                }
        }
        if (choice == '0')
        {
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->class_level[p_ptr->pclass] >= 10)
                {
                        int abilnum = (p_ptr->pclass * 10) + 9;
                        if (p_ptr->ability_points > 0)
                        {
                                if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
                                else
                                {
                                        if (p_ptr->abilities[abilnum] <= 0) p_ptr->num_abilities += 1;
                                        p_ptr->abilities[abilnum] += 1;
                                        p_ptr->ability_points -= 1;
                                }
                        }
                }
                else {
                        msg_print("You must first reach class level 10!");
                        inkey();
                }
        }
        if (choice == 'x' || choice == 'X')
        {
                learning = FALSE;
        }

        }
        Term_load();
        update_and_handle();
}

void pet_skills(int m_idx)
{
        monster_type *m_ptr = &m_list[m_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

	char            choice;
        int flg = PROJECT_STOP | PROJECT_KILL;

	char            out_val[160];
        int x=px,y=py,k,count;
        int rlev = r_ptr->level;
	int rad;

        /* List the powers */
        if(r_ptr->flags4 & 90) {strcpy(power_desc[num],"Multiply");powers[num++]=90;}
        if(r_ptr->flags4 & RF4_SHRIEK) {strcpy(power_desc[num],"Agravate monsters");powers[num++]=0;}
        if(r_ptr->flags4 & RF4_ROCKET) {strcpy(power_desc[num],"Rocket");powers[num++]=1;}
        if(r_ptr->flags4 & RF4_ARROW_1) {strcpy(power_desc[num],"Arrow1");powers[num++]=2;}
        if(r_ptr->flags4 & RF4_ARROW_2) {strcpy(power_desc[num],"Arrow2");powers[num++]=3;}
        if(r_ptr->flags4 & RF4_ARROW_3) {strcpy(power_desc[num],"Arrow3");powers[num++]=4;}
        if(r_ptr->flags4 & RF4_ARROW_4) {strcpy(power_desc[num],"Arrow4");powers[num++]=5;}
        if(r_ptr->flags4 & RF4_BR_ACID) {strcpy(power_desc[num],"Breathe Acid");powers[num++]=6;}
        if(r_ptr->flags4 & RF4_BR_ELEC) {strcpy(power_desc[num],"Breathe Lightning");powers[num++]=7;}
        if(r_ptr->flags4 & RF4_BR_FIRE) {strcpy(power_desc[num],"Breathe Fire");powers[num++]=8;}
        if(r_ptr->flags4 & RF4_BR_COLD) {strcpy(power_desc[num],"Breathe Cold");powers[num++]=9;}
        if(r_ptr->flags4 & RF4_BR_POIS) {strcpy(power_desc[num],"Breathe Poison");powers[num++]=10;}
        if(r_ptr->flags4 & RF4_BR_NETH) {strcpy(power_desc[num],"Breathe Nether");powers[num++]=11;}
        if(r_ptr->flags4 & RF4_BR_LITE) {strcpy(power_desc[num],"Breathe Lite");powers[num++]=12;}
        if(r_ptr->flags4 & RF4_BR_DARK) {strcpy(power_desc[num],"Breathe Darkness");powers[num++]=13;}
        if(r_ptr->flags4 & RF4_BR_CONF) {strcpy(power_desc[num],"Breathe Confusion");powers[num++]=14;}
        if(r_ptr->flags4 & RF4_BR_SOUN) {strcpy(power_desc[num],"Breathe Sound");powers[num++]=15;}
        if(r_ptr->flags4 & RF4_BR_CHAO) {strcpy(power_desc[num],"Breathe Chaos");powers[num++]=16;}
        if(r_ptr->flags4 & RF4_BR_DISE) {strcpy(power_desc[num],"Breathe Disenchantment");powers[num++]=17;}
        if(r_ptr->flags4 & RF4_BR_NEXU) {strcpy(power_desc[num],"Breathe Nexus");powers[num++]=18;}
        if(r_ptr->flags4 & RF4_BR_TIME) {strcpy(power_desc[num],"Breathe Time");powers[num++]=19;}
        if(r_ptr->flags4 & RF4_BR_INER) {strcpy(power_desc[num],"Breathe Inertia");powers[num++]=20;}
        if(r_ptr->flags4 & RF4_BR_GRAV) {strcpy(power_desc[num],"Breathe Gravity");powers[num++]=21;}
        if(r_ptr->flags4 & RF4_BR_SHAR) {strcpy(power_desc[num],"Breathe Shards");powers[num++]=22;}
        if(r_ptr->flags4 & RF4_BR_PLAS) {strcpy(power_desc[num],"Breathe Plasma");powers[num++]=23;}
        if(r_ptr->flags4 & RF4_BR_WALL) {strcpy(power_desc[num],"Breathe Force");powers[num++]=24;}
        if(r_ptr->flags4 & RF4_BR_MANA) {strcpy(power_desc[num],"Breathe Mana");powers[num++]=25;}
        if(r_ptr->flags4 & RF4_BA_NUKE) {strcpy(power_desc[num],"Nuke Ball");powers[num++]=26;}
        if(r_ptr->flags4 & RF4_BR_NUKE) {strcpy(power_desc[num],"Breathe Nuke");powers[num++]=27;}
        if(r_ptr->flags4 & RF4_BA_CHAO) {strcpy(power_desc[num],"Chaos Ball");powers[num++]=28;}
        if(r_ptr->flags4 & RF4_BR_DISI) {strcpy(power_desc[num],"Breathe Disintegration");powers[num++]=29;}
        if(r_ptr->flags5 & RF5_BA_ACID) {strcpy(power_desc[num],"Acid Ball");powers[num++]=30;}
        if(r_ptr->flags5 & RF5_BA_ELEC) {strcpy(power_desc[num],"Lightning Ball");powers[num++]=31;}
        if(r_ptr->flags5 & RF5_BA_FIRE) {strcpy(power_desc[num],"Fire Ball");powers[num++]=32;}
        if(r_ptr->flags5 & RF5_BA_COLD) {strcpy(power_desc[num],"Cold Ball");powers[num++]=33;}
        if(r_ptr->flags5 & RF5_BA_POIS) {strcpy(power_desc[num],"Poison Ball");powers[num++]=34;}
        if(r_ptr->flags5 & RF5_BA_NETH) {strcpy(power_desc[num],"Nether Ball");powers[num++]=35;}
        if(r_ptr->flags5 & RF5_BA_WATE) {strcpy(power_desc[num],"Water Ball");powers[num++]=36;}
        if(r_ptr->flags5 & RF5_BA_MANA) {strcpy(power_desc[num],"Mana Ball");powers[num++]=37;}
        if(r_ptr->flags5 & RF5_BA_DARK) {strcpy(power_desc[num],"Darkness Ball");powers[num++]=38;}
        if(r_ptr->flags5 & RF5_CAUSE_1) {strcpy(power_desc[num],"Cause light wounds");powers[num++]=42;}
        if(r_ptr->flags5 & RF5_CAUSE_2) {strcpy(power_desc[num],"Cause medium wounds");powers[num++]=43;}
        if(r_ptr->flags5 & RF5_CAUSE_3) {strcpy(power_desc[num],"Cause critical wounds");powers[num++]=44;}
        if(r_ptr->flags5 & RF5_CAUSE_4) {strcpy(power_desc[num],"Cause mortal wounds");powers[num++]=45;}
        if(r_ptr->flags5 & RF5_BO_ACID) {strcpy(power_desc[num],"Acid Bolt");powers[num++]=46;}
        if(r_ptr->flags5 & RF5_BO_ELEC) {strcpy(power_desc[num],"Lightning Bolt");powers[num++]=47;}
        if(r_ptr->flags5 & RF5_BO_FIRE) {strcpy(power_desc[num],"Fire Bolt");powers[num++]=48;}
        if(r_ptr->flags5 & RF5_BO_COLD) {strcpy(power_desc[num],"Cold Bolt");powers[num++]=49;}
        if(r_ptr->flags5 & RF5_BO_POIS) {strcpy(power_desc[num],"Poison Bolt");powers[num++]=56;}
        if(r_ptr->flags5 & RF5_BO_NETH) {strcpy(power_desc[num],"Nether Bolt");powers[num++]=50;}
        if(r_ptr->flags5 & RF5_BO_WATE) {strcpy(power_desc[num],"Water Bolt");powers[num++]=51;}
        if(r_ptr->flags5 & RF5_BO_MANA) {strcpy(power_desc[num],"Mana Bolt");powers[num++]=52;}
        if(r_ptr->flags5 & RF5_BO_PLAS) {strcpy(power_desc[num],"Plasma Bolt");powers[num++]=53;}
        if(r_ptr->flags5 & RF5_BO_ICEE) {strcpy(power_desc[num],"Ice Bolt");powers[num++]=54;}
        if(r_ptr->flags5 & RF5_MISSILE) {strcpy(power_desc[num],"Missile");powers[num++]=55;}
        if(r_ptr->flags5 & RF5_SCARE) {strcpy(power_desc[num],"Scare");powers[num++]=41;}
        if(r_ptr->flags5 & RF5_BLIND) {strcpy(power_desc[num],"Blindness");powers[num++]=57;}
        if(r_ptr->flags5 & RF5_CONF) {strcpy(power_desc[num],"Confusion");powers[num++]=58;}
        if(r_ptr->flags5 & RF5_SLOW) {strcpy(power_desc[num],"Slow");powers[num++]=59;}
        if(r_ptr->flags5 & RF5_HOLD) {strcpy(power_desc[num],"Paralyse");powers[num++]=60;}
        if(r_ptr->flags6 & RF6_HASTE) {strcpy(power_desc[num],"Haste Self");powers[num++]=61;}
        if(r_ptr->flags6 & RF6_HAND_DOOM) {strcpy(power_desc[num],"Hand of Doom");powers[num++]=62;}
        if(r_ptr->flags6 & RF6_HEAL) {strcpy(power_desc[num],"Heal");powers[num++]=63;}
        if(r_ptr->flags6 & RF6_BLINK) {strcpy(power_desc[num],"Blink");powers[num++]=64;}
        if(r_ptr->flags6 & RF6_TPORT) {strcpy(power_desc[num],"Teleport");powers[num++]=65;}
        if(r_ptr->flags6 & RF6_TELE_TO) {strcpy(power_desc[num],"Teleport To");powers[num++]=66;}
        if(r_ptr->flags6 & RF6_TELE_AWAY) {strcpy(power_desc[num],"Teleport Away");powers[num++]=67;}
        if(r_ptr->flags6 & RF6_TELE_LEVEL) {strcpy(power_desc[num],"Teleport Level");powers[num++]=68;}
        if(r_ptr->flags6 & RF6_DARKNESS) {strcpy(power_desc[num],"Darkness");powers[num++]=69;}
        if(r_ptr->flags6 & RF6_TRAPS) {strcpy(power_desc[num],"Create Traps");powers[num++]=88;}
        if(r_ptr->flags6 & RF6_RAISE_DEAD) {strcpy(power_desc[num],"Raise the Dead");powers[num++]=89;}
        if(r_ptr->flags6 & RF6_S_BUG) {strcpy(power_desc[num],"Summon Sofware Bugs");powers[num++]=70;}
        if(r_ptr->flags6 & RF6_S_RNG) {strcpy(power_desc[num],"Summon RNG");powers[num++]=71;}
        if(r_ptr->flags6 & RF6_S_DRAGONRIDER) {strcpy(power_desc[num],"Summon DragonRider");powers[num++]=72;}
        if(r_ptr->flags6 & RF6_S_KIN) {strcpy(power_desc[num],"Summon Kin");powers[num++]=73;}
        if(r_ptr->flags6 & RF6_S_CYBER) {strcpy(power_desc[num],"Summon Cyberdemon");powers[num++]=74;}
        if(r_ptr->flags6 & RF6_S_MONSTER) {strcpy(power_desc[num],"Summon Monster");powers[num++]=75;}
        if(r_ptr->flags6 & RF6_S_MONSTERS) {strcpy(power_desc[num],"Summon Monsters");powers[num++]=76;}
        if(r_ptr->flags6 & RF6_S_ANT) {strcpy(power_desc[num],"Summon Ants");powers[num++]=77;}
        if(r_ptr->flags6 & RF6_S_SPIDER) {strcpy(power_desc[num],"Summon Spiders");powers[num++]=78;}
        if(r_ptr->flags6 & RF6_S_HOUND) {strcpy(power_desc[num],"Summon Hound");powers[num++]=79;}
        if(r_ptr->flags6 & RF6_S_HYDRA) {strcpy(power_desc[num],"Summon Hydras");powers[num++]=80;}
        if(r_ptr->flags6 & RF6_S_ANGEL) {strcpy(power_desc[num],"Summon Angel");powers[num++]=81;}
        if(r_ptr->flags6 & RF6_S_DEMON) {strcpy(power_desc[num],"Summon Demon");powers[num++]=82;}
        if(r_ptr->flags6 & RF6_S_UNDEAD) {strcpy(power_desc[num],"Summon Undead");powers[num++]=83;}
        if(r_ptr->flags6 & RF6_S_DRAGON) {strcpy(power_desc[num],"Summon Dragon");powers[num++]=84;}
        if(r_ptr->flags6 & RF6_S_HI_UNDEAD) {strcpy(power_desc[num],"Summon High Undead");powers[num++]=85;}
        if(r_ptr->flags6 & RF6_S_HI_DRAGON) {strcpy(power_desc[num],"Summon High Dragon");powers[num++]=86;}
        if(r_ptr->flags6 & RF6_S_WRAITH) {strcpy(power_desc[num],"Summon Wraith");powers[num++]=87;}

        if(!num) {msg_print("No powers to use.");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which monster magic? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which monster magic? ",
			I2A(0), '0' + num - 27);
	}

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

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
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

        rad = 1 + (p_ptr->lev/15);

        switch(Power)
        {
		case 90: /* Multiply */
			msg_format("You multiply...");
			do_cmd_wiz_named_friendly(p_ptr->body_monster, FALSE);
			break;
                case 0: /* Shriek */
                        aggravate_monsters(-1);
                        break;
                case 1: /* Rocket */
                        fire_ball_pets(m_ptr, GF_ROCKET, m_ptr->maxhp / 3, 3);
                        break;
                case 2: /* Arrow1 */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_ARROW, dir, damroll(1,6), flg);
                        break;
                case 3: /* Arrow2 */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_ARROW, dir, damroll(3,6), flg);
                        break;
                case 4: /* Arrow3 */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_ARROW, dir, damroll(5,6), flg);
                        break;
                case 5: /* Arrow4 */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_ARROW, dir, damroll(7,6), flg);
                        break;
                case 6: /* Br acid */
                        fire_ball_pets(m_ptr, GF_ACID, m_ptr->maxhp / 3, 3);
                        break;
                case 7: /* Br elec */
                        fire_ball_pets(m_ptr, GF_ELEC, m_ptr->maxhp / 3, 3);
                        break;
                case 8: /* br fire */
                        fire_ball_pets(m_ptr, GF_FIRE, m_ptr->maxhp / 3, 3);
                        break;
                case 9: /* br cold */
                        fire_ball_pets(m_ptr, GF_COLD, m_ptr->maxhp / 3, 3);
                        break;
                case 10: /* br pois */
                        fire_ball_pets(m_ptr, GF_POIS, m_ptr->maxhp / 3, 3);
                        break;
                case 11: /* br neth */
                        fire_ball_pets(m_ptr, GF_NETHER, m_ptr->maxhp / 3, 3);
                        break;
                case 12: /* br lite */
                        fire_ball_pets(m_ptr, GF_LITE, m_ptr->maxhp / 3, 3);
                        break;
                case 13: /* br dark */
                        fire_ball_pets(m_ptr, GF_DARK, m_ptr->maxhp / 3, 3);
                        break;
                case 14: /* br conf */
                        fire_ball_pets(m_ptr, GF_CONFUSION, m_ptr->maxhp / 3, 3);
                        break;
                case 15: /* br soun */
                        fire_ball_pets(m_ptr, GF_SOUND, m_ptr->maxhp / 3, 3);
                        break;
                case 16: /* br chao */
                        fire_ball_pets(m_ptr, GF_CHAOS, m_ptr->maxhp / 3, 3);
                        break;
                case 17: /* br dise */
                        fire_ball_pets(m_ptr, GF_DISENCHANT, m_ptr->maxhp / 3, 3);
                        break;
                case 18: /* br nexu */
                        fire_ball_pets(m_ptr, GF_NEXUS, m_ptr->maxhp / 3, 3);
                        break;
                case 19: /* br time */
                        fire_ball_pets(m_ptr, GF_TIME, m_ptr->maxhp / 3, 3);
                        break;
                case 20: /* br iner */
                        fire_ball_pets(m_ptr, GF_INERTIA, m_ptr->maxhp / 3, 3);
                        break;
                case 21: /* br grav */
                        fire_ball_pets(m_ptr, GF_GRAVITY, m_ptr->maxhp / 3, 3);
                        break;
                case 22: /* br shar */
                        fire_ball_pets(m_ptr, GF_SHARDS, m_ptr->maxhp / 3, 3);
                        break;
                case 23: /* br plas */
                        fire_ball_pets(m_ptr, GF_PLASMA, m_ptr->maxhp / 3, 3);
                        break;
                case 24: /* br wall */
                        fire_ball_pets(m_ptr, GF_FORCE, m_ptr->maxhp / 3, 3);
                        break;
                case 25: /* br mana */
                        fire_ball_pets(m_ptr, GF_MANA, m_ptr->maxhp / 3, 3);
                        break;
                case 26: /* ba nuke */
                        fire_ball_pets(m_ptr, GF_NUKE, m_ptr->maxhp / 3, 3);
                        break;
                case 27: /* br nuke */
                        fire_ball_pets(m_ptr, GF_NUKE, m_ptr->maxhp / 3, 3);
                        break;
                case 28: /* ba chao */
                        fire_ball_pets(m_ptr, GF_CHAOS, rlev * 5, 2);
                        break;
                case 29: /* br disi */
                        fire_ball_pets(m_ptr, GF_DISINTEGRATE, m_ptr->maxhp / 3, 3);
                        break;
                case 30: /* ba acid */
                        fire_ball_pets(m_ptr, GF_ACID, rlev * 5, 2);
                        break;
                case 31: /* ba elec */
                        fire_ball_pets(m_ptr, GF_ELEC, rlev * 5, 2);
                        break;
                case 32: /* ba fire */
                        fire_ball_pets(m_ptr, GF_FIRE, rlev * 5, 2);
                        break;
                case 33: /* ba cold */
                        fire_ball_pets(m_ptr, GF_COLD, rlev * 5, 2);
                        break;
                case 34: /* ba pois */
                        fire_ball_pets(m_ptr, GF_POIS, rlev * 5, 2);
                        break;
                case 35: /* ba neth */
                        fire_ball_pets(m_ptr, GF_NETHER, rlev * 5, 2);
                        break;
                case 36: /* ba wate */
                        fire_ball_pets(m_ptr, GF_WATER, rlev * 5, 2);
                        break;
                case 37: /* ba mana */
                        fire_ball_pets(m_ptr, GF_MANA, rlev * 5, 2);
                        break;
                case 38: /* ba dark */
                        fire_ball_pets(m_ptr, GF_DARK, rlev * 5, 2);
                        break;
	        case 42: /* cause1 */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_MANA, dir, damroll(3,8), flg);
		        break;
	        case 43: /* cause2 */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_MANA, dir, damroll(8,8), flg);
		        break;
	        case 44: /* cause3 */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_MANA, dir, damroll(10,15), flg);
		        break;
	        case 45: /* cause4 */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_MANA, dir, damroll(15,15), flg);
		        break;
                case 46: /* bo acid */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_ACID, dir, damroll(7, 8) + (rlev / 3), flg);
                        break;
                case 47: /* bo elec */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_ELEC, dir, damroll(7, 8) + (rlev / 3), flg);
                        break;
                case 48: /* bo fire */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_FIRE, dir, damroll(7, 8) + (rlev / 3), flg);
                        break;
                case 49: /* bo cold */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_COLD, dir, damroll(7, 8) + (rlev / 3), flg);
                        break;
                case 56: /* bo pois */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_POIS, dir, damroll(7, 8) + (rlev / 3), flg);
                        break;
                case 50: /* bo neth */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_NETHER, dir, damroll(7, 8) + (rlev / 3), flg);
                        break;
                case 51: /* bo wate */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_WATER, dir, damroll(7, 8) + (rlev / 3), flg);
                        break;
                case 52: /* bo mana */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_MANA, dir, damroll(7, 8) + (rlev / 3), flg);
                        break;
                case 53: /* bo plas */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_PLASMA, dir, damroll(7, 8) + (rlev / 3), flg);
                        break;
                case 54: /* bo ice */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_ICE, dir, damroll(7, 8) + (rlev / 3), flg);
                        break;
                case 55: /* missile */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_MISSILE, dir, damroll(2, 6) + (rlev / 3), flg);
                        break;
                case 57: /* blind */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_CONFUSION, dir, damroll(1, 8) + (rlev / 3), flg);
                        break;
                case 41: /* scare */
                        msg_print("Not implemented yet...");
                        return;
                        break;
                case 58: /* conf */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_CONFUSION, dir, damroll(7, 8) + (rlev / 3), flg);
                        break;
                case 59: /* slow */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_OLD_SLOW, dir, damroll(6, 8) + (rlev / 3), flg);
                        break;
                case 60: /* hold */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_OLD_SLEEP, dir, damroll(5, 8) + (rlev / 3), flg);
                        break;
                case 61: /* haste */
                        msg_print("The monster Haste itself!");
                        if (m_ptr->mspeed < 160)
			{
                                m_ptr->mspeed += 10;
                                if (m_ptr->mspeed >= 160) m_ptr->mspeed = 160;
			}
                        break;
                case 62: /* hand of doom */
                        if (get_aim_dir(&dir))
                               project_hook_pets(m_ptr, GF_MANA, dir, damroll(10, 8) + rlev, flg);
                        break;
                case 63: /* heal */
                        m_ptr->hp *= 10;
                        if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;
                        break;
                case 64: /* Blink */
                        teleport_away(m_idx, 10);
                        break;
                case 65: /* Teleport */
                        teleport_away(m_idx, 10);
                        break;
                case 66: /* tele to */
                        teleport_away(m_idx, 10);
                        break;
                case 67: /* tele away */
                        msg_print("Not implemented yet...");
                        break;
		case 68: /* tele level */
                        msg_print("Not implemented yet...");
			break;
                case 69: /* darkness */
                        /* Unlite up the room */
                        unlite_room(m_ptr->fy, m_ptr->fx);
                        break;
	        case 88: /* create traps */
                        msg_print("Not implemented yet...");
		        break;
		case 89: /* raise the dead - uses the same code as the
			    nether spell*/
                        msg_print("Not implemented yet...");
			break;
                case 70: /* Summon bug */
				for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_BUG, TRUE);
				}
                        break;
                case 71: /* Summon RNG */
				for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_RNG, TRUE);
				}
                        break;
                case 72: /* Summon dragonrider */
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_DRAGONRIDER, TRUE);
				}
                        break;
                case 73: /* Summon kin */
                                summon_kin_type = m_ptr->r_idx; /* Big hack */
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_KIN, TRUE);
				}
                        break;
                case 74: /* Summon cyber */
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_CYBER, TRUE);
				}
                        break;
                case 75: /* Summon monster */
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, 0, TRUE);
				}
                        break;
                case 76: /* Summon monsters */
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, 0, TRUE);
				}
                        break;
                case 77: /* Summon ant */
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_ANT, TRUE);
				}
                        break;
                case 78: /* Summon spider */
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_SPIDER, TRUE);
				}
                        break;
                case 79: /* Summon hound */
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HOUND, TRUE);
				}
                        break;
                case 80: /* Summon hydra */
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HYDRA, TRUE);
				}
                        break;
                case 81: /* Summon angel */
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_ANGEL, TRUE);
				}
                        break;
                case 82: /* Summon demon */
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_DEMON, TRUE);
				}
                        break;
                case 83: /* Summon undead */
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_UNDEAD, TRUE);
				}
                        break;
                case 84: /* Summon dragon */
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_DRAGON, TRUE);
				}
                        break;
                case 85: /* Summon hiundead */
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE);
				}
                        break;
                case 86: /* Summon hidragon */
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_HI_DRAGON_NO_UNIQUES, TRUE);
				}
                        break;
                case 87: /* Summon wraith */
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(m_ptr->fy, m_ptr->fx, rlev, SUMMON_WRAITH, TRUE);
				}
                        break;
        }
}

/* Since ability(x) is a very useful function, here's a similar one for Rings! */
bool ring(int rsval)
{
        object_type *o_ptr;
        object_type *j_ptr;

        o_ptr = &inventory[INVEN_RING];
        j_ptr = &inventory[INVEN_RING+1];

        if (o_ptr->sval == rsval || j_ptr->sval == rsval) return TRUE;
        else return FALSE;
}

/* Since ability(x) is a very useful function, here's a similar one for Bows! */
s16b examine_bow()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_BOW];

        return (o_ptr->sval - 10);
}

/* Since ability(x) is a very useful function, here's a similar one for daggers! */
bool dagger_check()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (o_ptr->tval == TV_DAGGER) return (TRUE);
        else return (FALSE);
}

bool axe_check()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (o_ptr->tval == TV_AXE) return (TRUE);
        else return (FALSE);
}

bool is_weapon(object_type *o_ptr)
{
        if (o_ptr->tval == TV_SWORD || o_ptr->tval == TV_HAFTED || o_ptr->tval == TV_POLEARM ||
        o_ptr->tval == TV_ROD || o_ptr->tval == TV_MSTAFF || o_ptr->tval == TV_SWORD_DEVASTATION || o_ptr->tval == TV_DAGGER || o_ptr->tval == TV_AXE ||
        o_ptr->tval == TV_HELL_STAFF || o_ptr->tval == TV_VALKYRIE_SPEAR || o_ptr->tval == TV_ZELAR_WEAPON) return (TRUE);

        return (FALSE);
}
bool is_ammo(object_type *o_ptr)
{
        if (o_ptr->tval == TV_ARROW || o_ptr->tval == TV_BOLT || o_ptr->tval == TV_SHOT) return (TRUE);

        return (FALSE);
}

int add_item_ability(object_type *o_ptr)
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;
        int     magictype;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

        char            choice, ch;

	char            out_val[160];
        int k,count;

        /* List the powers */
        {strcpy(power_desc[num],"Random Resistance 1500 golds");powers[num++]=0;}
        if (o_ptr->tval == TV_BOOTS) {strcpy(power_desc[num],"Increase speed 200000 golds");powers[num++]=1;}
        if (is_weapon(o_ptr)) {strcpy(power_desc[num],"Extra attacks 200000 golds");powers[num++]=2;}
        {strcpy(power_desc[num],"Immunity: Fire 200000 golds");powers[num++]=3;}
        {strcpy(power_desc[num],"Immunity: Cold 200000 golds");powers[num++]=4;}
        {strcpy(power_desc[num],"Immunity: Elec 200000 golds");powers[num++]=5;}
        {strcpy(power_desc[num],"Immunity: Acid 200000 golds");powers[num++]=6;}
        {strcpy(power_desc[num],"Invisibility 150000 golds");powers[num++]=7;}
        {strcpy(power_desc[num],"Telepathy 20000 golds");powers[num++]=8;}
        {strcpy(power_desc[num],"Wraith Form 500000 golds");powers[num++]=9;}
        {strcpy(power_desc[num],"Permanent Light 5000 golds");powers[num++]=10;}
        if (is_weapon(o_ptr) || is_ammo(o_ptr)) {strcpy(power_desc[num],"Brand Fire 7500 golds");powers[num++]=11;}
        if (is_weapon(o_ptr) || is_ammo(o_ptr)) {strcpy(power_desc[num],"Brand Cold 7500 golds");powers[num++]=12;}
        if (is_weapon(o_ptr) || is_ammo(o_ptr)) {strcpy(power_desc[num],"Brand Elec 7500 golds");powers[num++]=13;}
        if (is_weapon(o_ptr) || is_ammo(o_ptr)) {strcpy(power_desc[num],"Brand Acid 7500 golds");powers[num++]=14;}
        if (is_weapon(o_ptr) || is_ammo(o_ptr)) {strcpy(power_desc[num],"Brand Poison 7500 golds");powers[num++]=15;}
        if (is_weapon(o_ptr) || is_ammo(o_ptr)) {strcpy(power_desc[num],"Brand Light 10000 golds");powers[num++]=16;}
        if (is_weapon(o_ptr) || is_ammo(o_ptr)) {strcpy(power_desc[num],"Brand Dark 5000 golds");powers[num++]=17;}
        if (is_weapon(o_ptr) || is_ammo(o_ptr)) {strcpy(power_desc[num],"Brand Magic 20000 golds");powers[num++]=18;}
        {strcpy(power_desc[num],"Indestructible 500 golds");powers[num++]=20;}
        {strcpy(power_desc[num],"Regeneration 50000 golds");powers[num++]=21;}
        if (o_ptr->tval == TV_SHIELD) {strcpy(power_desc[num],"Reflecting 25000 golds");powers[num++]=22;}
        {strcpy(power_desc[num],"Eternal 45000 golds");powers[num++]=23;}
        if (o_ptr->tval == TV_HELM || o_ptr->tval == TV_CROWN) {strcpy(power_desc[num],"Safety 150000 golds");powers[num++]=24;}
        if(!num) {msg_print("No powers to use.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Add which ability? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Add which ability? ",
			I2A(0), '0' + num - 27);
	}

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

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
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
                return num;
	}

        switch(Power)
        {
                case 0:
                        if (p_ptr->au >= 1500)
                        {
                                msg_print("Your item gained a resistance!");
                                p_ptr->au -= 1500;
                                random_resistance(o_ptr, FALSE, ((randint(34)+4)));
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 1:
                        if (p_ptr->au >= 200000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 200000;
                                o_ptr->art_flags1 |= TR1_SPEED;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 2:
                        if (p_ptr->au >= 200000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 200000;
                                o_ptr->art_flags1 |= TR1_BLOWS;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 3:
                        if (p_ptr->au >= 200000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 200000;
                                o_ptr->art_flags2 |= TR2_IM_FIRE;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 4:
                        if (p_ptr->au >= 200000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 200000;
                                o_ptr->art_flags2 |= TR2_IM_COLD;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 5:
                        if (p_ptr->au >= 200000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 200000;
                                o_ptr->art_flags2 |= TR2_IM_ELEC;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 6:
                        if (p_ptr->au >= 200000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 200000;
                                o_ptr->art_flags2 |= TR2_IM_ACID;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 7:
                        if (p_ptr->au >= 150000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 22000;
                                o_ptr->art_flags2 |= TR2_INVIS;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 8:
                        if (p_ptr->au >= 20000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 20000;
                                o_ptr->art_flags3 |= TR3_TELEPATHY;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 9:
                        if (p_ptr->au >= 500000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 500000;
                                o_ptr->art_flags3 |= TR3_WRAITH;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 10:
                        if (p_ptr->au >= 5000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 5000;
                                o_ptr->art_flags3 |= TR3_LITE;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 11:
                        if (p_ptr->au >= 7500)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 7500;
                                o_ptr->art_flags1 |= TR1_BRAND_FIRE;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 12:
                        if (p_ptr->au >= 7500)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 7500;
                                o_ptr->art_flags1 |= TR1_BRAND_COLD;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 13:
                        if (p_ptr->au >= 7500)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 7500;
                                o_ptr->art_flags1 |= TR1_BRAND_ELEC;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 14:
                        if (p_ptr->au >= 7500)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 7500;
                                o_ptr->art_flags1 |= TR1_BRAND_ACID;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 15:
                        if (p_ptr->au >= 7500)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 7500;
                                o_ptr->art_flags1 |= TR1_BRAND_POIS;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 16:
                        if (p_ptr->au >= 10000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 10000;
                                o_ptr->art_flags4 |= TR4_BRAND_LIGHT;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 17:
                        if (p_ptr->au >= 5000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 5000;
                                o_ptr->art_flags4 |= TR4_BRAND_DARK;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 18:
                        if (p_ptr->au >= 20000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 20000;
                                o_ptr->art_flags4 |= TR4_BRAND_MAGIC;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 19:
                        if (p_ptr->au >= 150000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 40000;
                                o_ptr->art_flags2 |= TR2_LIFE;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 20:
                        if (p_ptr->au >= 500)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 500;
                                o_ptr->art_flags4 |= TR4_INDESTRUCTIBLE;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 21:
                        if (p_ptr->au >= 50000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 50000;
                                o_ptr->art_flags3 |= TR3_REGEN;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 22:
                        if (p_ptr->au >= 25000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 25000;
                                o_ptr->art_flags2 |= TR2_REFLECT;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 23:
                        if (p_ptr->au >= 45000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 45000;
                                o_ptr->art_flags4 |= TR4_ETERNAL;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 24:
                        if (p_ptr->au >= 150000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 150000;
                                o_ptr->art_flags4 |= TR4_SAFETY;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;

        }
        energy_use = 100;
        return num;
}

/* Improve your skills! :) */
void improve_skills()
{
	int                     Power = -1;
        int                     num = 0, i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask;

        char            choice;

	char            out_val[160];

        /* List the powers */
        {strcpy(power_desc[num],"Swords");powers[num++]=0;}
        {strcpy(power_desc[num],"Hafted");powers[num++]=1;}
        {strcpy(power_desc[num],"Polearms");powers[num++]=2;}
        {strcpy(power_desc[num],"Daggers");powers[num++]=10;}
        {strcpy(power_desc[num],"Axes");powers[num++]=11;}
        {strcpy(power_desc[num],"Rods");powers[num++]=3;}
        {strcpy(power_desc[num],"Shooting");powers[num++]=4;}
        {strcpy(power_desc[num],"Throwing");powers[num++]=5;}
        {strcpy(power_desc[num],"Martial Arts");powers[num++]=6;}
        {strcpy(power_desc[num],"Agility");powers[num++]=7;}
        {strcpy(power_desc[num],"Stealth");powers[num++]=8;}
        {strcpy(power_desc[num],"Spellcraft");powers[num++]=9;}
        {strcpy(power_desc[num],"Leadership");powers[num++]=12;}
        {strcpy(power_desc[num],"Alchemy");powers[num++]=13;}
        {strcpy(power_desc[num],"Crafting");powers[num++]=14;}
        {strcpy(power_desc[num],"Combat Feats");powers[num++]=15;}
        if(!num) {msg_print("No powers to use.");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Skills %c-%c, *=List, ESC=exit) Improve which skill? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Skills %c-%c, *=List, ESC=exit) Improve which skill? ",
			I2A(0), '0' + num - 27);
	}

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

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
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
                        strnfmt(tmp_val, 78, "Improve %s? ", power_desc[i]);

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

        switch(Power)
        {
                case 0:
                        p_ptr->skill_swords_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 1:
                        p_ptr->skill_hafted_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 2:
                        p_ptr->skill_polearms_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 3:
                        p_ptr->skill_rods_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 4:
                        p_ptr->skill_shooting_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 5:
                        p_ptr->skill_throwing_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 6:
                        p_ptr->skill_marts_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 7:
                        p_ptr->skill_agility_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 8:
                        p_ptr->skill_stealth_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 9:
                        p_ptr->skill_spellcraft_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 10:
                        p_ptr->skill_daggers_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 11:
                        p_ptr->skill_axes_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 12:
                        p_ptr->skill_leadership_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 13:
                        p_ptr->skill_alchemy_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 14:
                        p_ptr->skill_crafting_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                case 15:
                        p_ptr->skill_combat_base += 1;
                        p_ptr->skillpoints -= 1;
                        break;
                
        }
}

/* Soul powers */
int use_monster_soul()
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

        char            choice, ch;

	char            out_val[160];
        int k,count;

        /* List the powers */
        strcpy(power_desc[num],"Capture Soul(Cost: 5   Effect: Try to capture a monster's soul.)");powers[num++]=1;
        strcpy(power_desc[num],"Soul Powers(Cost: ??   Effect: Use a soul's powers.)");powers[num++]=2;
        strcpy(power_desc[num],"Soul Bind(Effect: Enchant an item with a soul.)");powers[num++]=3;
        strcpy(power_desc[num],"Simulacrum(Cost: ??   Effect: Summon a copy of the soul's monster.)");powers[num++]=4;

        if(!num) {msg_print("You can't cast any spells from this category!");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Cast which spell? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Cast which spell? ",
			I2A(0), '0' + num - 27);
	}

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

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
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
                return num;
	}

        switch(Power)
        {
                case 1:
                        if (p_ptr->csp >= 5)
                        {
                                int ii, ij;
                                if (!tgt_pt(&ii,&ij)) return num;
                                capture_soul(ii, ij);
                                p_ptr->csp -= 5;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough mana points!");
                        break;
                case 2:
                        use_soul_power();
                        update_and_handle();
                        break;
                case 3:
                        soul_bind();
                        update_and_handle();
                        break;
                case 4:
                        simulacrum();
                        update_and_handle();
                        break;

        }
        energy_use = 100;
        return num;
}


/* Draw offensive powers from a monster's soul */
void use_soul_power()
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;
        s32b brdam = 0;
        s32b dieroll;

        char            choice, ch;

	char            out_val[160];
        int k,count;
        int rlev = dun_level + p_ptr->lev + randint(5);
	int rad;
        int bonusdam;

        int item;
        object_type             *o_ptr;
        monster_race            *r_ptr;
        cptr q, s;

        /* Restrict choices to souls */
        item_tester_tval = TV_SOUL;

        /* Get an item */
        q = "Use which soul? ";
        s = "You have no souls!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        o_ptr = &inventory[item];

        /* Get the monster from the soul */
        r_ptr = &r_info[o_ptr->pval];

        /* Damages. Based on monster's hp value. A monster with */
        /* higher hp will be a great one to use for spells! */
        brdam = maxroll(r_ptr->hdice, r_ptr->hside) * (r_ptr->level / 3) / 10;
        brdam = brdam + ((brdam * (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10)] * 10)) / 100);
        dieroll = ((r_ptr->hdice + r_ptr->hside) / 2) + r_ptr->level;
        dieroll = dieroll + ((dieroll * (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10)] * 3)) / 100);

        /* List the powers */
        if(r_ptr->flags4 & RF4_SHRIEK) {strcpy(power_desc[num],"Agravate monsters");powers[num++]=0;}
        if(r_ptr->flags4 & RF4_ROCKET) {strcpy(power_desc[num],"Rocket");powers[num++]=1;}
        if(r_ptr->flags4 & RF4_ARROW_1) {strcpy(power_desc[num],"Arrow1");powers[num++]=2;}
        if(r_ptr->flags4 & RF4_ARROW_2) {strcpy(power_desc[num],"Arrow2");powers[num++]=3;}
        if(r_ptr->flags4 & RF4_ARROW_3) {strcpy(power_desc[num],"Arrow3");powers[num++]=4;}
        if(r_ptr->flags4 & RF4_ARROW_4) {strcpy(power_desc[num],"Arrow4");powers[num++]=5;}
        if(r_ptr->flags4 & RF4_BR_ACID) {strcpy(power_desc[num],"Breathe Acid");powers[num++]=6;}
        if(r_ptr->flags4 & RF4_BR_ELEC) {strcpy(power_desc[num],"Breathe Lightning");powers[num++]=7;}
        if(r_ptr->flags4 & RF4_BR_FIRE) {strcpy(power_desc[num],"Breathe Fire");powers[num++]=8;}
        if(r_ptr->flags4 & RF4_BR_COLD) {strcpy(power_desc[num],"Breathe Cold");powers[num++]=9;}
        if(r_ptr->flags4 & RF4_BR_POIS) {strcpy(power_desc[num],"Breathe Poison");powers[num++]=10;}
        if(r_ptr->flags4 & RF4_BR_NETH) {strcpy(power_desc[num],"Breathe Nether");powers[num++]=11;}
        if(r_ptr->flags4 & RF4_BR_LITE) {strcpy(power_desc[num],"Breathe Lite");powers[num++]=12;}
        if(r_ptr->flags4 & RF4_BR_DARK) {strcpy(power_desc[num],"Breathe Darkness");powers[num++]=13;}
        if(r_ptr->flags4 & RF4_BR_CONF) {strcpy(power_desc[num],"Breathe Confusion");powers[num++]=14;}
        if(r_ptr->flags4 & RF4_BR_SOUN) {strcpy(power_desc[num],"Breathe Sound");powers[num++]=15;}
        if(r_ptr->flags4 & RF4_BR_CHAO) {strcpy(power_desc[num],"Breathe Chaos");powers[num++]=16;}
        if(r_ptr->flags4 & RF4_BR_DISE) {strcpy(power_desc[num],"Breathe Disenchantment");powers[num++]=17;}
        if(r_ptr->flags4 & RF4_BR_NEXU) {strcpy(power_desc[num],"Breathe Nexus");powers[num++]=18;}
        if(r_ptr->flags4 & RF4_BR_TIME) {strcpy(power_desc[num],"Breathe Time");powers[num++]=19;}
        if(r_ptr->flags4 & RF4_BR_INER) {strcpy(power_desc[num],"Breathe Inertia");powers[num++]=20;}
        if(r_ptr->flags4 & RF4_BR_GRAV) {strcpy(power_desc[num],"Breathe Gravity");powers[num++]=21;}
        if(r_ptr->flags4 & RF4_BR_SHAR) {strcpy(power_desc[num],"Breathe Shards");powers[num++]=22;}
        if(r_ptr->flags4 & RF4_BR_PLAS) {strcpy(power_desc[num],"Breathe Plasma");powers[num++]=23;}
        if(r_ptr->flags4 & RF4_BR_WALL) {strcpy(power_desc[num],"Breathe Force");powers[num++]=24;}
        if(r_ptr->flags4 & RF4_BR_MANA) {strcpy(power_desc[num],"Breathe Mana");powers[num++]=25;}
        if(r_ptr->flags4 & RF4_BA_NUKE) {strcpy(power_desc[num],"Nuke Ball");powers[num++]=26;}
        if(r_ptr->flags4 & RF4_BR_NUKE) {strcpy(power_desc[num],"Breathe Nuke");powers[num++]=27;}
        if(r_ptr->flags4 & RF4_BA_CHAO) {strcpy(power_desc[num],"Chaos Ball");powers[num++]=28;}
        if(r_ptr->flags4 & RF4_BR_DISI) {strcpy(power_desc[num],"Breathe Disintegration");powers[num++]=29;}
        if(r_ptr->flags5 & RF5_BA_ACID) {strcpy(power_desc[num],"Acid Ball");powers[num++]=30;}
        if(r_ptr->flags5 & RF5_BA_ELEC) {strcpy(power_desc[num],"Lightning Ball");powers[num++]=31;}
        if(r_ptr->flags5 & RF5_BA_FIRE) {strcpy(power_desc[num],"Fire Ball");powers[num++]=32;}
        if(r_ptr->flags5 & RF5_BA_COLD) {strcpy(power_desc[num],"Cold Ball");powers[num++]=33;}
        if(r_ptr->flags5 & RF5_BA_POIS) {strcpy(power_desc[num],"Poison Ball");powers[num++]=34;}
        if(r_ptr->flags5 & RF5_BA_NETH) {strcpy(power_desc[num],"Nether Ball");powers[num++]=35;}
        if(r_ptr->flags5 & RF5_BA_WATE) {strcpy(power_desc[num],"Water Ball");powers[num++]=36;}
        if(r_ptr->flags5 & RF5_BA_MANA) {strcpy(power_desc[num],"Mana Ball");powers[num++]=37;}
        if(r_ptr->flags5 & RF5_BA_DARK) {strcpy(power_desc[num],"Darkness Ball");powers[num++]=38;}
        if(r_ptr->flags5 & RF5_CAUSE_1) {strcpy(power_desc[num],"Cause light wounds");powers[num++]=42;}
        if(r_ptr->flags5 & RF5_CAUSE_2) {strcpy(power_desc[num],"Cause medium wounds");powers[num++]=43;}
        if(r_ptr->flags5 & RF5_CAUSE_3) {strcpy(power_desc[num],"Cause critical wounds");powers[num++]=44;}
        if(r_ptr->flags5 & RF5_CAUSE_4) {strcpy(power_desc[num],"Cause mortal wounds");powers[num++]=45;}
        if(r_ptr->flags5 & RF5_BO_ACID) {strcpy(power_desc[num],"Acid Bolt");powers[num++]=46;}
        if(r_ptr->flags5 & RF5_BO_ELEC) {strcpy(power_desc[num],"Lightning Bolt");powers[num++]=47;}
        if(r_ptr->flags5 & RF5_BO_FIRE) {strcpy(power_desc[num],"Fire Bolt");powers[num++]=48;}
        if(r_ptr->flags5 & RF5_BO_COLD) {strcpy(power_desc[num],"Cold Bolt");powers[num++]=49;}
        if(r_ptr->flags5 & RF5_BO_POIS) {strcpy(power_desc[num],"Poison Bolt");powers[num++]=56;}
        if(r_ptr->flags5 & RF5_BO_NETH) {strcpy(power_desc[num],"Nether Bolt");powers[num++]=50;}
        if(r_ptr->flags5 & RF5_BO_WATE) {strcpy(power_desc[num],"Water Bolt");powers[num++]=51;}
        if(r_ptr->flags5 & RF5_BO_MANA) {strcpy(power_desc[num],"Mana Bolt");powers[num++]=52;}
        if(r_ptr->flags5 & RF5_BO_PLAS) {strcpy(power_desc[num],"Plasma Bolt");powers[num++]=53;}
        if(r_ptr->flags5 & RF5_BO_ICEE) {strcpy(power_desc[num],"Ice Bolt");powers[num++]=54;}
        if(r_ptr->flags5 & RF5_MISSILE) {strcpy(power_desc[num],"Missile");powers[num++]=55;}
        if(r_ptr->flags5 & RF5_SCARE) {strcpy(power_desc[num],"Scare");powers[num++]=41;}
        if(r_ptr->flags5 & RF5_BLIND) {strcpy(power_desc[num],"Blindness");powers[num++]=57;}
        if(r_ptr->flags5 & RF5_CONF) {strcpy(power_desc[num],"Confusion");powers[num++]=58;}
        if(r_ptr->flags5 & RF5_SLOW) {strcpy(power_desc[num],"Slow");powers[num++]=59;}
        if(r_ptr->flags5 & RF5_HOLD) {strcpy(power_desc[num],"Paralyse");powers[num++]=60;}
        if(r_ptr->flags6 & RF6_HASTE) {strcpy(power_desc[num],"Haste Self");powers[num++]=61;}
        if(r_ptr->flags6 & RF6_HAND_DOOM) {strcpy(power_desc[num],"Hand of Doom");powers[num++]=62;}
        if(r_ptr->flags6 & RF6_HEAL) {strcpy(power_desc[num],"Heal");powers[num++]=63;}
        if(r_ptr->flags6 & RF6_BLINK) {strcpy(power_desc[num],"Blink");powers[num++]=64;}
        if(r_ptr->flags6 & RF6_TPORT) {strcpy(power_desc[num],"Teleport");powers[num++]=65;}
        if(r_ptr->flags6 & RF6_TELE_TO) {strcpy(power_desc[num],"Teleport To");powers[num++]=66;}
        if(r_ptr->flags6 & RF6_TELE_AWAY) {strcpy(power_desc[num],"Teleport Away");powers[num++]=67;}
        if(r_ptr->flags6 & RF6_TELE_LEVEL) {strcpy(power_desc[num],"Teleport Level");powers[num++]=68;}
        if(r_ptr->flags6 & RF6_DARKNESS) {strcpy(power_desc[num],"Darkness");powers[num++]=69;}
        if(r_ptr->flags6 & RF6_TRAPS) {strcpy(power_desc[num],"Create Traps");powers[num++]=88;}
        if(r_ptr->flags6 & RF6_RAISE_DEAD) {strcpy(power_desc[num],"Raise the Dead");powers[num++]=89;}
        if(r_ptr->flags6 & RF6_S_BUG) {strcpy(power_desc[num],"Summon Sofware Bugs");powers[num++]=70;}
        if(r_ptr->flags6 & RF6_S_RNG) {strcpy(power_desc[num],"Summon RNG");powers[num++]=71;}
        if(r_ptr->flags6 & RF6_S_DRAGONRIDER) {strcpy(power_desc[num],"Summon DragonRider");powers[num++]=72;}
        if(r_ptr->flags6 & RF6_S_KIN) {strcpy(power_desc[num],"Summon Kin");powers[num++]=73;}
        if(r_ptr->flags6 & RF6_S_CYBER) {strcpy(power_desc[num],"Summon Cyberdemon");powers[num++]=74;}
        if(r_ptr->flags6 & RF6_S_MONSTER) {strcpy(power_desc[num],"Summon Monster");powers[num++]=75;}
        if(r_ptr->flags6 & RF6_S_MONSTERS) {strcpy(power_desc[num],"Summon Monsters");powers[num++]=76;}
        if(r_ptr->flags6 & RF6_S_ANT) {strcpy(power_desc[num],"Summon Ants");powers[num++]=77;}
        if(r_ptr->flags6 & RF6_S_SPIDER) {strcpy(power_desc[num],"Summon Spiders");powers[num++]=78;}
        if(r_ptr->flags6 & RF6_S_HOUND) {strcpy(power_desc[num],"Summon Hound");powers[num++]=79;}
        if(r_ptr->flags6 & RF6_S_HYDRA) {strcpy(power_desc[num],"Summon Hydras");powers[num++]=80;}
        if(r_ptr->flags6 & RF6_S_ANGEL) {strcpy(power_desc[num],"Summon Angel");powers[num++]=81;}
        if(r_ptr->flags6 & RF6_S_DEMON) {strcpy(power_desc[num],"Summon Demon");powers[num++]=82;}
        if(r_ptr->flags6 & RF6_S_UNDEAD) {strcpy(power_desc[num],"Summon Undead");powers[num++]=83;}
        if(r_ptr->flags6 & RF6_S_DRAGON) {strcpy(power_desc[num],"Summon Dragon");powers[num++]=84;}
        if(r_ptr->flags6 & RF6_S_HI_UNDEAD) {strcpy(power_desc[num],"Summon High Undead");powers[num++]=85;}
        if(r_ptr->flags6 & RF6_S_HI_DRAGON) {strcpy(power_desc[num],"Summon High Dragon");powers[num++]=86;}
        if(r_ptr->flags6 & RF6_S_WRAITH) {strcpy(power_desc[num],"Summon Wraith");powers[num++]=87;}
        if(r_ptr->flags6 & RF6_S_UNIQUE) {strcpy(power_desc[num],"Summon Unique");powers[num++]=91;}
        if(!num) {msg_print("No powers to use.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
			I2A(0), '0' + num - 27);
	}

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

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
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

        rad = 1 + (p_ptr->lev/15);
        update_and_handle();

        switch(Power)
        {
                case 0: /* Shriek */
                        aggravate_monsters(-1);
                        break;
                case 1: /* Rocket */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ROCKET, dir, p_ptr->chp, 2 + (p_ptr->lev/20));
                        break;
                case 2: /* Arrow1 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(dieroll,dieroll));
                        break;
                case 3: /* Arrow2 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(dieroll + 5,dieroll + 5));
                        break;
                case 4: /* Arrow3 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(dieroll + 10,dieroll + 10));
                        break;
                case 5: /* Arrow4 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(dieroll + 15,dieroll + 15));
                        break;
                case 6: /* Br acid */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, brdam, rad);
                        break;
                case 7: /* Br elec */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, brdam, rad);
                        break;
                case 8: /* br fire */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, brdam, rad);
                        break;
                case 9: /* br cold */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, brdam, rad);
                        break;
                case 10: /* br pois */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, brdam, rad);
                        break;
                case 11: /* br neth */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, brdam, rad);
                        break;
                case 12: /* br lite */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_LITE, dir, brdam, rad);
                        break;
                case 13: /* br dark */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, brdam, rad);
                        break;
                case 14: /* br conf */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CONFUSION, dir, brdam, rad);
                        break;
                case 15: /* br soun */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SOUND, dir, brdam, rad);
                        break;
                case 16: /* br chao */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, brdam, rad);
                        break;
                case 17: /* br dise */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISENCHANT, dir, brdam, rad);
                        break;
                case 18: /* br nexu */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NEXUS, dir, brdam, rad);
                        break;
                case 19: /* br time */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_TIME, dir, brdam, rad);
                        break;
                case 20: /* br iner */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_INERTIA, dir, brdam, rad);
                        break;
                case 21: /* br grav */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_GRAVITY, dir, brdam, rad);
                        break;
                case 22: /* br shar */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SHARDS, dir, brdam, rad);
                        break;
                case 23: /* br plas */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_PLASMA, dir, brdam, rad);
                        break;
                case 24: /* br wall */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FORCE, dir, brdam, rad);
                        break;
                case 25: /* br mana */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, brdam, rad);
                        break;
                case 26: /* ba nuke */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, brdam, 1 + (p_ptr->lev/20));
                        break;
                case 27: /* br nuke */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, brdam, 1 + (p_ptr->lev/20));
                        break;
                case 28: /* ba chao */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 29: /* br disi */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISINTEGRATE, dir, damroll(dieroll,dieroll), 1 + (p_ptr->lev/20));
                        break;
                case 30: /* ba acid */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 31: /* ba elec */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 32: /* ba fire */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 33: /* ba cold */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 34: /* ba pois */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 35: /* ba neth */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 36: /* ba wate */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_WATER, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 37: /* ba mana */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 38: /* ba dark */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, damroll(dieroll,dieroll), 2);
                        break;
	        case 42: /* cause1 */
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll,dieroll));
			}
		        break;
	        case 43: /* cause2 */
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 5,dieroll + 5));
			}
		        break;
	        case 44: /* cause3 */
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 10,dieroll + 10));
			}
		        break;
	        case 45: /* cause4 */
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 15,dieroll + 15));
			}
		        break;
                case 46: /* bo acid */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ACID, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 47: /* bo elec */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ELEC, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 48: /* bo fire */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_FIRE, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 49: /* bo cold */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_COLD, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 56: /* bo pois */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_POIS, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 50: /* bo neth */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_NETHER, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 51: /* bo wate */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_WATER, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 52: /* bo mana */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MANA, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 53: /* bo plas */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_PLASMA, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 54: /* bo ice */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ICE, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 55: /* missile */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MISSILE, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 57: /* blind */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 41: /* scare */
			if (get_aim_dir(&dir))
				fear_monster(dir, plev);
                        break;
                case 58: /* conf */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 59: /* slow */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLOW, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 60: /* hold */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLEEP, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 61: /* haste */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5));
			}
                        break;
                case 62: /* hand of doom */
                        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 30,dieroll + 30) + (p_ptr->lev));
			}
                        break;
                case 63: /* heal */
                        hp_player(dieroll * 20);
                        break;
                case 64: /* Blink */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        teleport_player(10);
                        break;
                case 65: /* Teleport */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        teleport_player(dieroll);
                        break;
                case 66: /* tele to */
                        {
                             int ii,ij;

                             if(special_flag) {msg_print("No teleport on special levels ...");break;}
                             msg_print("You go between. Input destination.");
                             if (!tgt_pt(&ii,&ij)) return;
                             p_ptr->energy -= 60 - plev;
                             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                             (distance(ij,ii,py,px) > plev*20 + 2))
                             {
                                 msg_print("You fail to show the destination correctly!");
                                 p_ptr->energy -= 100;
                                 teleport_player(10);
                             }
                             else teleport_player_to(ij,ii);
                        }
                        break;
                case 67: /* tele away */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_beam(GF_AWAY_ALL, dir, plev);
                        break;
		case 68: /* tele level */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
			teleport_player_level();
			break;
                case 69: /* darkness */
                        (void)project(-1, 3, py, px, 0, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL);
                        /* Unlite up the room */
                        unlite_room(py, px);
                        break;
                case 88: /* create traps */
		        msg_print("You create traps ...");
			trap_creation();
		        break;
                case 89: /* raise the dead - uses the same code as the
			    nether spell*/
		{
			int item, x, y;
			object_type *o_ptr;

			cptr q, s;

			/* Restrict choices to corpses */
			item_tester_tval = TV_CORPSE;

			/* Get an item */
			q = "Use which corpse? ";
			s = "You have no corpse to use.";
			if (!get_item(&item, q, s, (USE_FLOOR))) break;;

			o_ptr = &o_list[0 - item];

                        if(randint(8)>=5-o_ptr->sval){
                                msg_print("You touch the corpse ... the monster raise from the graveyard!");

                                x=px;
                                y=py;
                                get_pos_player(5, &y, &x);
                                place_monster_one(y, x, o_ptr->pval2, FALSE, TRUE);

                                floor_item_increase(0 - item, -1);
                                floor_item_describe(0 - item);
                                floor_item_optimize(0 - item);
                        }
			break;
		}
                case 70: /* Summon bug */
                                msg_format("You magically code some software bugs.");
				for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_BUG, TRUE);
				}
                        break;
                case 71: /* Summon RNG */
                                msg_format("You magically code some RNGs.");
				for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_RNG, TRUE);
				}
                        break;
                case 72: /* Summon dragonrider */
                                msg_format("You magically summon a DragonRider.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_DRAGONRIDER, TRUE);
				}
                        break;
                case 73: /* Summon kin */
                                msg_format("You magically summon some Kins.");
                                summon_kin_type = p_ptr->body_monster; /* Big hack */
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_KIN, TRUE);
				}
                        break;
                case 74: /* Summon cyber */
                                msg_format("You magically summon a Cyberdemon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_CYBER, TRUE);
				}
                        break;
                case 75: /* Summon monster */
                                msg_format("You magically summon a monster.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, 0, TRUE);
				}
                        break;
                case 76: /* Summon monsters */
                                msg_format("You magically summon monsters.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, 0, TRUE);
				}
                        break;
                case 77: /* Summon ant */
                                msg_format("You magically summon ants.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_ANT, TRUE);
				}
                        break;
                case 78: /* Summon spider */
                                msg_format("You magically summon spiders.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_SPIDER, TRUE);
				}
                        break;
                case 79: /* Summon hound */
                                msg_format("You magically summon hounds.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_HOUND, TRUE);
				}
                        break;
                case 80: /* Summon hydra */
                                msg_format("You magically summon hydras.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_HYDRA, TRUE);
				}
                        break;
                case 81: /* Summon angel */
                                msg_format("You magically summon an angel.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_ANGEL, TRUE);
				}
                        break;
                case 82: /* Summon demon */
                                msg_format("You magically summon a demon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_DEMON, TRUE);
				}
                        break;
                case 83: /* Summon undead */
                                msg_format("You magically summon an undead.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_UNDEAD, TRUE);
				}
                        break;
                case 84: /* Summon dragon */
                                msg_format("You magically summon a dragon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_DRAGON, TRUE);
				}
                        break;
                case 85: /* Summon hiundead */
                                msg_format("You magically summon greater undeads.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE);
				}
                        break;
                case 86: /* Summon hidragon */
                                msg_format("You magically summon greater dragons.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_HI_DRAGON_NO_UNIQUES, TRUE);
				}
                        break;
                case 87: /* Summon wraith */
                                msg_format("You magically summon Wraith.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_WRAITH, TRUE);
				}
                        break;
                case 91: /* Summon Unique */
                                msg_format("You magically summon Unique.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(py, px, rlev, SUMMON_UNIQUE, TRUE);
				}
                        break;
                case 92: /* Mega Mana */
                        msg_print("You cast a gigantic ball of mana!");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, randint(p_ptr->lev * 10)+20+5000, 3);
                        break;
                        
        }
        energy_use = 100;
}

/* Psychic powers */
void use_psychic_power()
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

        char            choice, ch;

	char            out_val[160];
        int k,count;

        /* List the powers */
        if (p_ptr->stat_ind[A_WIS] >= 10) {strcpy(power_desc[num],"Psionic Bolt(Damages: 10/level   Effect: May confuse target.)");powers[num++]=1;}
        if (p_ptr->stat_ind[A_WIS] >= 25) {strcpy(power_desc[num],"Sense Monsters(Effect: Detect monsters around you.)");powers[num++]=2;}
        if (p_ptr->stat_ind[A_WIS] >= 30) {strcpy(power_desc[num],"Mental Illusions(Damages: 20/level   Effect: May halve target's hit rate.)");powers[num++]=3;}
        if (p_ptr->stat_ind[A_WIS] >= 40) {strcpy(power_desc[num],"Fearful Illusions(Damages: 25/level   Effect: May scare monster away.)");powers[num++]=4;}
        if (p_ptr->stat_ind[A_WIS] >= 55) {strcpy(power_desc[num],"Concentrate(Effect: Raise your hit rate and defense for 20 turns.)");powers[num++]=5;}
        if (p_ptr->stat_ind[A_WIS] >= 70) {strcpy(power_desc[num],"Telepathic Mind(Effect: Gain telepathy for 200 turns.)");powers[num++]=6;}
        if (p_ptr->stat_ind[A_WIS] >= 80) {strcpy(power_desc[num],"Psi Storm(Damages: 50/level   Effect: May confuse monsters around you.  Radius: 5)");powers[num++]=7;}
        if (p_ptr->stat_ind[A_WIS] >= 90) {strcpy(power_desc[num],"Illusion Of Sickness(Effect: Reduce enemy's hp by 33%.)");powers[num++]=8;}
        if (p_ptr->stat_ind[A_WIS] >= 100) {strcpy(power_desc[num],"Summon Spirit(Effect: Summon a spirit.)");powers[num++]=9;}
        if (p_ptr->stat_ind[A_WIS] >= 120) {strcpy(power_desc[num],"Psychic Wrath(Damages: 100/level   Effect: May halve monsters hit rate.  Radius: 6)");powers[num++]=10;}

        if(!num) {msg_print("You cannot use any powers yet. You need more wisdom!");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Cast which spell? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Cast which spell? ",
			I2A(0), '0' + num - 27);
	}

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

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
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

        switch(Power)
        {
                case 1:
                        {
                                s32b dam;
                                dam = (p_ptr->lev * 10);                        
                                if (get_aim_dir(&dir))
                                fire_bolt(GF_PSI, dir, dam);
                                update_and_handle();
                        }
                        break;
                case 2:
                        detect_monsters_normal();
                        update_and_handle();
                        break;
                case 3:
                        {
                                s32b dam;
                                dam = (p_ptr->lev * 20);                        
                                if (get_aim_dir(&dir))
                                fire_bolt(GF_PSI_HITRATE, dir, dam);
                                update_and_handle();
                        }
                        break;
                case 4:
                        {
                                s32b dam;
                                dam = (p_ptr->lev * 25);                        
                                if (get_aim_dir(&dir))
                                fire_bolt(GF_PSI_FEAR, dir, dam);
                                update_and_handle();
                        }
                        break;
                case 5:
                        (void)set_blessed(20);
                        update_and_handle();
                        break;
                case 6:
                        (void)set_tim_esp(200);
                        update_and_handle();
                        break;
                case 7:
                        {
                                s32b dam;
                                dam = (p_ptr->lev * 50);                                                                
                                attack_aura(GF_PSI, dam, 5);
                                update_and_handle();
                        }
                        break;
                case 8:
                        {
                                s32b dam;
                                dam = 0;                        
                                if (get_aim_dir(&dir))
                                fire_bolt(GF_LIFE_BLAST, dir, dam);
                                update_and_handle();
                        }
                        break;
                case 9:
                        summon_specific_friendly(py, px, 30, SUMMON_MIND_SPIRIT, TRUE);
                        break;

                case 10:
                        {
                                s32b dam;
                                dam = (p_ptr->lev * 100);                                                                
                                attack_aura(GF_PSI_HITRATE, dam, 6);
                                update_and_handle();
                        }
                        break;

        }
        energy_use = 100;
        return;
}

/* Cast a spell */
void do_cmd_cast(void)
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i,k,count;
        s32b                    finalcost;
        s32b                    spellpower;

	int             powers[36];
	char            power_desc[36][80];
        char            summname[30];

        bool            flag, redraw;
        bool            empower = FALSE;
        int             ask;

        char            choice, ch, summchar, ch2;

	char            out_val[160];
        magic_spells *spell_ptr;

        if (!get_com("[C]ast a spell, [M]ake a spell, [D]elete a spell", &ch)) return;

        if (ch == 'C' || ch == 'c')
        {

        for (i = 1; i < 30; i++)
        {
                char spellstring[80];
                spell_ptr = &magic_spell[i];
                if (spell_ptr->created == TRUE)
                {
                        sprintf(spellstring, "%s   Cost: %d", spell_ptr->name, spell_ptr->finalcost);        
                        /* List the powers */
                        strcpy(power_desc[num],spellstring);powers[num++]=i;
                }
        }

        if(!num) {msg_print("No spells to cast.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Cast which spell? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Cast which spell? ",
			I2A(0), '0' + num - 27);
	}

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

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
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
                return num;
	}

        spell_ptr = &magic_spell[Power];
        finalcost = spell_ptr->finalcost;
        if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 5] >= 1)
        {
                get_com("Empower the spell? [y/n]", &ch2);
                if (ch2 == 'y' || ch2 == 'Y')
                {
                        empower = TRUE;
                        finalcost = finalcost * 3;
                }
        }

        for (i = 0; i < 5; i++)
        {
                if (p_ptr->csp < finalcost)
                {
                        msg_print("You don't have enough mana to cast this spell.");
                        return;
                }
                spellpower = spell_ptr->power[i];
                if (empower == TRUE) spellpower += ((spellpower * (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 5] * 3)) / 100);
                if (spell_ptr->effect[i] != 0)
                {
                        /* Spell kind 1 = Attack spell */
                        if (spell_ptr->effect[i] == 1)
                        {
                                /* For physical spells... */
                                nevermiss = TRUE;
                                if (spell_ptr->shape[i] == 1)
                                {
                                        if (dir == 0) get_aim_dir(&dir);
                                        fire_bolt(spell_ptr->type[i], dir, (spellpower * p_ptr->lev) * (p_ptr->to_s));
                                }
                                if (spell_ptr->shape[i] == 2)
                                {
                                        if (dir == 0) get_aim_dir(&dir);
                                        fire_ball(spell_ptr->type[i], dir, (spellpower * p_ptr->lev) * (p_ptr->to_s), spell_ptr->radius[i]);
                                }
                                if (spell_ptr->shape[i] == 3)
                                {
                                        attack_aura(spell_ptr->type[i], (spellpower * p_ptr->lev) * (p_ptr->to_s), spell_ptr->radius[i]);
                                }
                                if (spell_ptr->shape[i] == 4)
                                {
                                        if (dir == 0) get_rep_dir(&dir);
                                        chain_attack(dir, spell_ptr->type[i], (spellpower * p_ptr->lev) * (p_ptr->to_s), spell_ptr->radius[i], 20);
                                }
                                if (spell_ptr->shape[i] == 5)
                                {
                                        s32b dam = 0;
                                        object_type *o_ptr = &inventory[INVEN_WIELD];
                                        if (dir == 0) get_rep_dir(&dir);
                                        if (o_ptr)
                                        {
                                                dam = weapon_damages();
                                        }
                                        dam += p_ptr->to_d;
                                        if (dam < 0) dam = 0;
                                        chain_attack(dir, spell_ptr->type[i], (dam * spellpower), spell_ptr->radius[i], 1);
                                }
                                nevermiss = FALSE;
                        }
                        /* Spell kind 2 = Fixed Attack spell */
                        /* Does fixed damages, not increased by your level. */
                        if (spell_ptr->effect[i] == 2)
                        {
                                if (spell_ptr->shape[i] == 1)
                                {
                                        if (dir == 0) get_aim_dir(&dir);
                                        fire_bolt(spell_ptr->type[i], dir, spellpower);
                                }
                                if (spell_ptr->shape[i] == 2)
                                {
                                        if (dir == 0) get_aim_dir(&dir);
                                        fire_ball(spell_ptr->type[i], dir, spellpower, spell_ptr->radius[i]);
                                }
                                if (spell_ptr->shape[i] == 3)
                                {
                                        attack_aura(spell_ptr->type[i], spellpower, spell_ptr->radius[i]);
                                }
                                if (spell_ptr->shape[i] == 4)
                                {
                                        if (dir == 0) get_rep_dir(&dir);
                                        chain_attack(dir, spell_ptr->type[i], spellpower, spell_ptr->radius[i], 20);
                                }
                        }
                        /* Spell kind 3 = Haste */
                        /* Raise speed by 10. Power is the duration. */
                        if (spell_ptr->effect[i] == 3)
                        {
                                (void)set_fast(spellpower);
                        }
                        /* Spell kind 4 = Stat Boost */
                        /* Raise a stat by a certain amount. */
                        /* The Shape is the duration of the spell. */
                        if (spell_ptr->effect[i] == 4)
                        {
                                switch(spell_ptr->type[i])
                                {
                                        case 1:
                                                p_ptr->str_boost = spellpower;
                                                (void)set_str_boost(spell_ptr->shape[i]);
                                                break;
                                        case 2:
                                                p_ptr->int_boost = spellpower;
                                                (void)set_int_boost(spell_ptr->shape[i]);
                                                break;
                                        case 3:
                                                p_ptr->wis_boost = spellpower;
                                                (void)set_wis_boost(spell_ptr->shape[i]);
                                                break;
                                        case 4:
                                                p_ptr->dex_boost = spellpower;
                                                (void)set_dex_boost(spell_ptr->shape[i]);
                                                break;
                                        case 5:
                                                p_ptr->con_boost = spellpower;
                                                (void)set_con_boost(spell_ptr->shape[i]);
                                                break;
                                        case 6:
                                                p_ptr->chr_boost = spellpower;
                                                (void)set_chr_boost(spell_ptr->shape[i]);
                                                break;
                                }
                        }
                        /* Spell kind 5 = Blessing */
                        /* Doubles to_hit and AC. Power is the duration. */
                        if (spell_ptr->effect[i] == 5)
                        {
                                (void)set_blessed(spellpower);
                        }
                        /* Spell kind 6 = Healing */
                        /* Heals some hp. The power is the number of */
                        /* of hp restored per levels. */
                        if (spell_ptr->effect[i] == 6)
                        {
                                s32b heal;
                                heal = (p_ptr->lev * spellpower) * (p_ptr->to_s);
                                msg_print("You feel better!");
                                p_ptr->chp += heal;
                                if (p_ptr->chp >= p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
                                update_and_handle();
                        }
                        /* Spell kind 7 = Restore Stats */
                        if (spell_ptr->effect[i] == 7)
                        {
                                (void)do_res_stat(A_STR);
                                (void)do_res_stat(A_INT);
                                (void)do_res_stat(A_WIS);
                                (void)do_res_stat(A_DEX);
                                (void)do_res_stat(A_CON);
                                (void)do_res_stat(A_CHR);
                        }
                        /* Spell kind 8 = Restore Status */
                        if (spell_ptr->effect[i] == 8)
                        {
                                (void)set_stun(0);
                                (void)set_poisoned(0);
                                (void)set_confused(0);
                                (void)set_paralyzed(0);
                                (void)set_blind(0);
                                (void)set_afraid(0);
                                restore_level();
                                update_and_handle();                                
                        }
                        /* Spell kind 9 = Cure Bleeding */
                        if (spell_ptr->effect[i] == 9)
                        {
                                (void)set_cut(0);
                        }
                        /* Spell kind 10 = Cure Black Breath */
                        if (spell_ptr->effect[i] == 10)
                        {
                                p_ptr->black_breath = 0;
                        }
                        /* Spell kind 11 = Revive Monster */
                        if (spell_ptr->effect[i] == 11)
                        {
                                revive_monster();
                        }
                        /* Spell kind 12 = Energize! */
                        /* Ahh, good old Energize! :) */
                        if (spell_ptr->effect[i] == 12)
                        {
                                s32b dam;
                                dam = p_ptr->msp / 4;
                                p_ptr->csp += dam;
                                if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
                                update_and_handle();
                                msg_print("You energize yourself!");                                
                        }
                        /* Spell kind 13 = Summon Kind */
                        if (spell_ptr->effect[i] == 13)
                        {
                                for (k = 0; k < spell_ptr->shape[i]; k++)
				{
                                        if (i == 0) summchar = spell_ptr->schar1;
                                        if (i == 1) summchar = spell_ptr->schar2;
                                        if (i == 2) summchar = spell_ptr->schar3;
                                        if (i == 3) summchar = spell_ptr->schar4;
                                        if (i == 4) summchar = spell_ptr->schar5;
                                        summon_specific_friendly_kind(py, px, spellpower, summchar, TRUE);
				}                                
                        }
                        /* Spell kind 14 = Summon Specific */
                        if (spell_ptr->effect[i] == 14)
                        {
                                for (k = 0; k < spell_ptr->shape[i]; k++)
				{
                                        if (i == 0) strcpy(summname, spell_ptr->sspeci1);
                                        if (i == 1) strcpy(summname, spell_ptr->sspeci2);
                                        if (i == 2) strcpy(summname, spell_ptr->sspeci3);
                                        if (i == 3) strcpy(summname, spell_ptr->sspeci4);
                                        if (i == 4) strcpy(summname, spell_ptr->sspeci5);
                                        summon_specific_friendly_name(py, px, summname, FALSE);
				}                                
                        }
                        /* Spell kind 15 = Damage fields */
                        if (spell_ptr->effect[i] == 15)
                        {
                                switch (spell_ptr->shape[i])
                                {
                                        case 1:
                                        {
                                                s32b dam;
                                                dam = (p_ptr->lev * spellpower) * (p_ptr->to_s);                                                                
                                                place_field(spell_ptr->type[i], spell_ptr->radius[i], px, py, dam);
                                                update_and_handle();
                                                break;
                                        }
                                        case 2:
                                        {
                                                s32b dam;
                                                int ii, ij;
                                                dam = (p_ptr->lev * spellpower) * (p_ptr->to_s);                                                                
                                                if (!tgt_pt(&ii,&ij)) return num;
                                                place_field(spell_ptr->type[i], spell_ptr->radius[i], ii, ij, dam);
                                                update_and_handle();
                                                break;
                                        }
                                }
                        }
                        /* Spell kind 16 = Detections */
                        if (spell_ptr->effect[i] == 16)
                        {
                                switch (spell_ptr->type[i])
                                {
                                        case 1:
                                        {
                                                detect_monsters_invis();
                                                detect_monsters_normal();
                                                break;
                                        }
                                        case 2:
                                        {
                                                detect_objects_normal();
                                                detect_objects_gold();
                                                break;
                                        }
                                        case 3:
                                        {
                                                detect_doors();
                                                break;
                                        }
                                        case 4:
                                        {
                                                detect_stairs();
                                                break;
                                        }
                                        case 5:
                                        {
                                                detect_traps();
                                                break;
                                        }

                                }
                        }
                        /* Spell kind 17 = Telepathy */
                        if (spell_ptr->effect[i] == 17)
                        {
                                (void)set_tim_esp(spellpower);
                        }
                        /* Spell kind 18 = Identify */
                        if (spell_ptr->effect[i] == 18)
                        {
                                identify_fully();
                        }
                        /* Spell kind 19 = Scan Monster */
                        if (spell_ptr->effect[i] == 19)
                        {
                                scan_targetting();
                        }
                        /* Spell kind 20 = Reveal Spell */
                        if (spell_ptr->effect[i] == 20)
                        {
                                switch (spell_ptr->shape[i])
                                {
                                        case 1:
                                        {
                                                reveal_spell(px, py, spell_ptr->radius[i]);
                                                update_and_handle();
                                                break;
                                        }
                                        case 2:
                                        {
                                                int ii, ij;
                                                if (!tgt_pt(&ii,&ij)) return num;
                                                reveal_spell(ii, ij, spell_ptr->radius[i]);
                                                update_and_handle();
                                                break;
                                        }
                                }
                        }
                        /* Spell kind 21 = Misc boosts */
                        /* Raise something temporarly, such as resistance or AC. */
                        /* The Shape is the duration of the spell. */
                        if (spell_ptr->effect[i] == 21)
                        {
                                switch(spell_ptr->type[i])
                                {
                                        case 1:
                                                p_ptr->pres = spellpower;
                                                if (p_ptr->pres > 100) p_ptr->pres = 100;
                                                (void)set_pres(spell_ptr->shape[i]);
                                                break;
                                        case 2:
                                                p_ptr->mres = spellpower;
                                                if (p_ptr->mres > 100) p_ptr->mres = 100;
                                                (void)set_mres(spell_ptr->shape[i]);
                                                break;
                                        case 3:
                                                p_ptr->ac_boost = spellpower;
                                                (void)set_ac_boost(spell_ptr->shape[i]);
                                                break;
                                }
                        }
                        /* Spell kind 22 = Conjure Item */
                        if (spell_ptr->effect[i] == 22)
                        {
                                if (i == 0) strcpy(summname, spell_ptr->sspeci1);
                                if (i == 1) strcpy(summname, spell_ptr->sspeci2);
                                if (i == 2) strcpy(summname, spell_ptr->sspeci3);
                                if (i == 3) strcpy(summname, spell_ptr->sspeci4);
                                if (i == 4) strcpy(summname, spell_ptr->sspeci5);
                                switch(spell_ptr->type[i])
                                {
                                        case 1:
                                                conjure_specific_item(summname, spell_ptr->shape[i], FALSE, FALSE);
                                                break;
                                        case 2:
                                                conjure_specific_item(summname, spell_ptr->shape[i], TRUE, FALSE);
                                                break;
                                        case 3:
                                                conjure_specific_item(summname, spell_ptr->shape[i], TRUE, TRUE);
                                                break;
                                }
                        }
                        /* Spell kind 23 = Satisfy Hunger */
                        if (spell_ptr->effect[i] == 23)
                        {
                        	if (p_ptr->prace == RACE_VAMPIRE || p_ptr->prace == RACE_SKELETON)
                        	msg_print("This spell is useless on you!");
                        	else set_food(PY_FOOD_MAX - 1);        
                        }
                
                }
                else
                {
                        p_ptr->csp -= finalcost;
                        update_and_handle();
                        energy_use = 100;
                        return;
                }
        }

        p_ptr->csp -= finalcost;
        update_and_handle();
        energy_use = 100;
        return;
        }
        /* Make a spell */
        else if (ch == 'd' || ch == 'D')
        {
                delete_spell();
        }
        else
        {
                if (dun_level == 0) spell_making();
                else msg_print("You can only create spells in the town!");
        }
}

/* Allow creation of a spell! :) */
void spell_making()
{
        int i;
        int effectschoose = 0;
        int effects_costs[5];
        int finalcost = 0;
        int effbase = 0;
        int effcost = 0;
        int efftype = 0;
        int effnum = 1;
        int rad = 0;
        int power = 0;
        int spelltype = 1;
        int manacost = 0;
        int whichslot = 1;
        int effkind = 0;
        int amountslots = 12;
        int redpercent = 0;

        /* Effect kinds... */
        /* 0. None         */
        /* 1. Attack spell */
        /* --------------- */

        char tmp[30];
        char tmpsumm[30];
	char query;
        char summch;
        char quantitystring[30];
        magic_spells *spell_ptr;

        /* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

        /* Prepare the screen */
        for (i = 0; i < SCREEN_HGT; i++)
        {
                roff("\n");
        }
        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
        c_put_str(TERM_WHITE, "---------------------------", 1, 0);

        if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 3] >= 1)
        {
                amountslots += (1 + (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 3] / 5));
        }

        sprintf(quantitystring, "Select a slot to put the spell (1 - %d): ", amountslots);

        whichslot = get_quantity(quantitystring, amountslots);
        spell_ptr = &magic_spell[whichslot];

        /* No final cost yet */
        spell_ptr->finalcost = 0;
        for (i = 0;i < 5; i++)
        {
                spell_ptr->manacost[i] = 0;
                spell_ptr->effect[i] = 0;
        }
        /* Not created yet...or deleted existing one. */
        spell_ptr->created = FALSE;

        /* We repeat the process for each effects! */
        while (effectschoose < 5)
        {
                /* Prepare the screen */
                for (i = 0; i < SCREEN_HGT; i++)
                {
                        roff("\n");
                }
                c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                
                /* First, select the school. */
                c_put_str(TERM_WHITE, "Select a spell school:", 4, 0);
                c_put_str(TERM_L_RED, "[1] Elemental", 7, 0);
                c_put_str(TERM_VIOLET, "[2] Alteration", 9, 0);
                c_put_str(TERM_L_BLUE, "[3] Healing", 11, 0);
                c_put_str(TERM_YELLOW, "[4] Conjuration", 13, 0);
                c_put_str(TERM_WHITE, "[5] Divination", 15, 0);

                /* End of scanning */
                query = inkey();

                if (query == '1')
                {
                        bool chosen = FALSE;
                        bool readytocreate = FALSE;
                        char choseneffect[30];                        
                        char choice;

                        /* Initialise some stuff */
                        effbase = 0;
                        effcost = 0;
                        efftype = 0;
                        rad = 0;
                        power = 0;
                        spelltype = 1;
                        manacost = 0;

                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Select an effect:", 4, 0);

                        /* All right, let's prepare the list! :) */
                        if (p_ptr->elemental_effects & ELEM_MISSILE) c_put_str(TERM_WHITE, "[0] Missile", 7, 0);
                        else c_put_str(TERM_L_DARK, "[0] Missile", 7, 0);
                        if (p_ptr->elemental_effects & ELEM_FIRE) c_put_str(TERM_WHITE, "[1] Fire", 8, 0);
                        else c_put_str(TERM_L_DARK, "[1] Fire", 8, 0);
                        if (p_ptr->elemental_effects & ELEM_COLD) c_put_str(TERM_WHITE, "[2] Cold", 9, 0);
                        else c_put_str(TERM_L_DARK, "[2] Cold", 9, 0);
                        if (p_ptr->elemental_effects & ELEM_ELEC) c_put_str(TERM_WHITE, "[3] Electricity", 10, 0);
                        else c_put_str(TERM_L_DARK, "[3] Electricity", 10, 0);
                        if (p_ptr->elemental_effects & ELEM_ACID) c_put_str(TERM_WHITE, "[4] Acid", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Acid", 11, 0);
                        if (p_ptr->elemental_effects & ELEM_POIS) c_put_str(TERM_WHITE, "[5] Poison", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Poison", 12, 0);
                        if (p_ptr->elemental_effects & ELEM_LITE) c_put_str(TERM_WHITE, "[6] Light", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Light", 13, 0);
                        if (p_ptr->elemental_effects & ELEM_DARK) c_put_str(TERM_WHITE, "[7] Darkness", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Darkness", 14, 0);
                        if (p_ptr->elemental_effects & ELEM_PHYSICAL) c_put_str(TERM_WHITE, "[8] Physical", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] Physical", 15, 0);
                        if (p_ptr->elemental_effects & ELEM_NETHER) c_put_str(TERM_WHITE, "[9] Nether", 16, 0);
                        else c_put_str(TERM_L_DARK, "[9] Nether", 16, 0);
                        if (p_ptr->elemental_effects & ELEM_NEXUS) c_put_str(TERM_WHITE, "[a] Nexus", 17, 0);
                        else c_put_str(TERM_L_DARK, "[a] Nexus", 17, 0);
                        if (p_ptr->elemental_effects & ELEM_PLASMA) c_put_str(TERM_WHITE, "[b] Plasma", 18, 0);
                        else c_put_str(TERM_L_DARK, "[b] Plasma", 18, 0);
                        if (p_ptr->elemental_effects & ELEM_NUKE) c_put_str(TERM_WHITE, "[c] Nuke", 19, 0);
                        else c_put_str(TERM_L_DARK, "[c] Nuke", 19, 0);
                        if (p_ptr->elemental_effects & ELEM_WATER) c_put_str(TERM_WHITE, "[d] Water", 7, 20);
                        else c_put_str(TERM_L_DARK, "[d] Water", 7, 20);
                        if (p_ptr->elemental_effects & ELEM_CHAOS) c_put_str(TERM_WHITE, "[e] Chaos", 8, 20);
                        else c_put_str(TERM_L_DARK, "[e] Chaos", 8, 20);
                        if (p_ptr->elemental_effects & ELEM_INERTIA) c_put_str(TERM_WHITE, "[f] Inertia", 9, 20);
                        else c_put_str(TERM_L_DARK, "[f] Inertia", 9, 20);
                        if (p_ptr->elemental_effects & ELEM_TIME) c_put_str(TERM_WHITE, "[g] Time", 10, 20);
                        else c_put_str(TERM_L_DARK, "[g] Time", 10, 20);
                        if (p_ptr->elemental_effects & ELEM_SHARDS) c_put_str(TERM_WHITE, "[h] Shards", 11, 20);
                        else c_put_str(TERM_L_DARK, "[h] Shards", 11, 20);
                        if (p_ptr->elemental_effects & ELEM_SHARDS) c_put_str(TERM_WHITE, "[i] Sound", 12, 20);
                        else c_put_str(TERM_L_DARK, "[i] Sound", 12, 20);
                        if (p_ptr->elemental_effects & ELEM_WARP) c_put_str(TERM_WHITE, "[j] Warp", 13, 20);
                        else c_put_str(TERM_L_DARK, "[j] Warp", 13, 20);
                        if (p_ptr->elemental_effects & ELEM_FORCE) c_put_str(TERM_WHITE, "[k] Force", 14, 20);
                        else c_put_str(TERM_L_DARK, "[k] Force", 14, 20);
                        if (p_ptr->elemental_effects & ELEM_GRAVITY) c_put_str(TERM_WHITE, "[l] Gravity", 15, 20);
                        else c_put_str(TERM_L_DARK, "[l] Gravity", 15, 20);
                        if (p_ptr->elemental_effects & ELEM_WIND) c_put_str(TERM_WHITE, "[m] Wind", 16, 20);
                        else c_put_str(TERM_L_DARK, "[m] Wind", 16, 20);
                        if (p_ptr->elemental_effects & ELEM_MANA) c_put_str(TERM_WHITE, "[n] Mana", 17, 20);
                        else c_put_str(TERM_L_DARK, "[n] Mana", 17, 20);

                        while (chosen == FALSE)
                        {
                                choice = inkey();
                                if (choice == '0')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_MISSILE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_MISSILE;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Missile");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '1')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_FIRE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_FIRE;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Fire");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '2')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_COLD)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_COLD;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Cold");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '3')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_ELEC)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_ELEC;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Electricity");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '4')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_ACID)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_ACID;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Acid");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '5')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_POIS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_POIS;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Poison");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '6')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_LITE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_LITE;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Light");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '7')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_DARK)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_DARK;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Darkness");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '8')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_PHYSICAL)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_PHYSICAL;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Physical");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '9')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_NETHER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_NETHER;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Nether");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'a')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_NEXUS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_NEXUS;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Nexus");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'b')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_PLASMA)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_PLASMA;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Plasma");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'c')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_NUKE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_NUKE;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Nuke");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'd')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_WATER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_WATER;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Water");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'e')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_CHAOS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_CHAOS;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Chaos");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'f')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_INERTIA)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_INERTIA;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Inertia");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'g')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_TIME)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_TIME;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Time");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'h')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_SHARDS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_SHARDS;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Shards");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'i')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_SOUND)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_SOUND;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Sound");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'j')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_WARP)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_AWAY_ALL;
                                                effbase = 40;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Warp");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'k')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_FORCE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_FORCE;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Force");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'l')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_GRAVITY)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_GRAVITY;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Gravity");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'm')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_WIND)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_WIND;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                sprintf(choseneffect, "Wind");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'n')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_MANA)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_MANA;
                                                effbase = 0;
                                                effcost = 2;
                                                effkind = 1;
                                                sprintf(choseneffect, "Mana");
                                                chosen = TRUE;
                                        }
                                }
                               
                                else
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Radius:", 8, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Type:", 10, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt         ");
                                else if (spelltype == 2) sprintf(tmp, "Ball         ");
                                else if (spelltype == 3) sprintf(tmp, "Circle       ");
                                else if (spelltype == 4) sprintf(tmp, "Chain        ");
                                else if (spelltype == 5) sprintf(tmp, "Melee Attack ");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                if (spelltype != 5) manacost = (effbase + ((effcost * power) / 2));
                                else manacost = (effbase + ((effcost * power)));
                                if (power > 40) manacost *= (1 + (manacost / 40));
                                manacost = manacost * (rad + 1);
                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }
                                if (spelltype == 5)
                                {
                                        manacost *= 40;
                                        if (rad > 0) manacost *= rad;
                                }
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 6] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 6];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 6] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 5) spelltype = 1;
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }

                }
                /* Alterations! */
                else if (query == '2')
                {
                        bool chosen = FALSE;
                        bool readytocreate = FALSE;
                        char choseneffect[30];                        
                        char choice;
                        int interfacetype;

                        /* Initialise some stuff */
                        effbase = 0;
                        effcost = 0;
                        efftype = 0;
                        rad = 0;
                        power = 0;
                        spelltype = 1;
                        manacost = 0;

                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Select an effect:", 4, 0);

                        /* All right, let's prepare the list! :) */
                        if (p_ptr->alteration_effects & ALTER_REDUCE_HIT) c_put_str(TERM_WHITE, "[0] Reduce Hit Rate", 7, 0);
                        else c_put_str(TERM_L_DARK, "[0] Reduce Hit Rate", 7, 0);
                        if (p_ptr->alteration_effects & ALTER_REDUCE_DEF) c_put_str(TERM_WHITE, "[1] Reduce Defense", 8, 0);
                        else c_put_str(TERM_L_DARK, "[1] Reduce Defense", 8, 0);
                        if (p_ptr->alteration_effects & ALTER_REDUCE_SPEED) c_put_str(TERM_WHITE, "[2] Reduce Speed", 9, 0);
                        else c_put_str(TERM_L_DARK, "[2] Reduce Speed", 9, 0);
                        if (p_ptr->alteration_effects & ALTER_REDUCE_LEVEL) c_put_str(TERM_WHITE, "[3] Reduce Level", 10, 0);
                        else c_put_str(TERM_L_DARK, "[3] Reduce Level", 10, 0);
                        if (p_ptr->alteration_effects & ALTER_LIFE_BLAST) c_put_str(TERM_WHITE, "[4] Life Blast", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Life Blast", 11, 0);
                        if (p_ptr->alteration_effects & ALTER_LOCK) c_put_str(TERM_WHITE, "[5] Lock", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Lock", 12, 0);
                        if (p_ptr->alteration_effects & ALTER_HALVE_POWER) c_put_str(TERM_WHITE, "[6] Halve Power", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Halve Power", 13, 0);
                        if (p_ptr->alteration_effects & ALTER_HALVE_MAGIC) c_put_str(TERM_WHITE, "[7] Halve Magic", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Halve Magic", 14, 0);
                        if (p_ptr->alteration_effects & ALTER_WAR_BLESSING) c_put_str(TERM_WHITE, "[8] War Blessing", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] War Blessing", 15, 0);
                        if (p_ptr->alteration_effects & ALTER_DEMORALIZE) c_put_str(TERM_WHITE, "[9] Demoralize", 16, 0);
                        else c_put_str(TERM_L_DARK, "[9] Demoralize", 16, 0);
                        if (p_ptr->alteration_effects & ALTER_RETROGRADE) c_put_str(TERM_WHITE, "[a] Retrograde", 17, 0);
                        else c_put_str(TERM_L_DARK, "[a] Retrograde", 17, 0);
                        if (p_ptr->alteration_effects & ALTER_EVOLVE) c_put_str(TERM_WHITE, "[b] Evolve", 18, 0);
                        else c_put_str(TERM_L_DARK, "[b] Evolve", 18, 0);
                        if (p_ptr->alteration_effects & ALTER_UNEVOLVE) c_put_str(TERM_WHITE, "[c] Un-Evolve", 19, 0);
                        else c_put_str(TERM_L_DARK, "[c] Un-Evolve", 19, 0);
                        if (p_ptr->alteration_effects & ALTER_HASTE) c_put_str(TERM_WHITE, "[d] Haste", 7, 20);
                        else c_put_str(TERM_L_DARK, "[d] Haste", 7, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_STR) c_put_str(TERM_WHITE, "[e] Raise Strength", 8, 20);
                        else c_put_str(TERM_L_DARK, "[e] Raise Strength", 8, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_INT) c_put_str(TERM_WHITE, "[f] Raise Intelligence", 9, 20);
                        else c_put_str(TERM_L_DARK, "[f] Raise Intelligence", 9, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_WIS) c_put_str(TERM_WHITE, "[g] Raise Wisdom", 10, 20);
                        else c_put_str(TERM_L_DARK, "[g] Raise Wisdom", 10, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_DEX) c_put_str(TERM_WHITE, "[h] Raise Dexterity", 11, 20);
                        else c_put_str(TERM_L_DARK, "[h] Raise Dexterity", 11, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_CON) c_put_str(TERM_WHITE, "[i] Raise Constitution", 12, 20);
                        else c_put_str(TERM_L_DARK, "[i] Raise Constitution", 12, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_CHR) c_put_str(TERM_WHITE, "[j] Raise Charisma", 13, 20);
                        else c_put_str(TERM_L_DARK, "[j] Raise Charisma", 13, 20);
                        if (p_ptr->alteration_effects & ALTER_BLESSING) c_put_str(TERM_WHITE, "[k] Blessing", 14, 20);
                        else c_put_str(TERM_L_DARK, "[k] Blessing", 14, 20);
                        if (p_ptr->alteration_effects & ALTER_HASTE_OTHER) c_put_str(TERM_WHITE, "[l] Haste Other", 15, 20);
                        else c_put_str(TERM_L_DARK, "[l] Haste Other", 15, 20);
                        if (p_ptr->alteration_effects & ALTER_PHYS_RESIST) c_put_str(TERM_WHITE, "[m] Physical Resistance", 16, 20);
                        else c_put_str(TERM_L_DARK, "[m] Physical Resistance", 16, 20);
                        if (p_ptr->alteration_effects & ALTER_MAGIC_RESIST) c_put_str(TERM_WHITE, "[n] Magic Resistance", 17, 20);
                        else c_put_str(TERM_L_DARK, "[n] Magic Resistance", 17, 20);
                        if (p_ptr->alteration_effects & ALTER_STONESKIN) c_put_str(TERM_WHITE, "[o] Stoneskin", 18, 20);
                        else c_put_str(TERM_L_DARK, "[o] Stoneskin", 18, 20);
                        if (p_ptr->alteration_effects & ALTER_PARALYZE) c_put_str(TERM_WHITE, "[p] Paralyze", 19, 20);
                        else c_put_str(TERM_L_DARK, "[p] Paralyze", 19, 20);


                        while (chosen == FALSE)
                        {
                                choice = inkey();
                                if (choice == '0')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_REDUCE_HIT)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_REDUCE_HIT;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Reduce Hit Rate");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '1')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_REDUCE_DEF)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_REDUCE_DEF;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Reduce Defense");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '2')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_REDUCE_SPEED)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_REDUCE_SPEED;
                                                effbase = 0;
                                                effcost = 100;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Reduce Speed");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '3')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_REDUCE_LEVEL)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_WEAKEN;
                                                effbase = 0;
                                                effcost = 35;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Reduce Level");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '4')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_LIFE_BLAST)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_LIFE_BLAST;
                                                effbase = 25;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Life Blast");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '5')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_LOCK)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_LOCK;
                                                effbase = 250;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Lock");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '6')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_HALVE_POWER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_HALVE_DAMAGES;
                                                effbase = 80;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Halve Power");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '7')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_HALVE_MAGIC)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_HALVE_MAGIC;
                                                effbase = 80;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Halve Magic");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '8')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_WAR_BLESSING)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_WAR_BLESSING;
                                                effbase = 30;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "War Blessing");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '9')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_DEMORALIZE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_FEAR_CURSE;
                                                effbase = 0;
                                                effcost = 12;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Demoralize");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'a')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RETROGRADE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_RETROGRADE;
                                                effbase = 1000;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Retrograde");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'b')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_EVOLVE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_EVOLVE;
                                                effbase = 50;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Evolve");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'c')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_UNEVOLVE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_UNEVOLVE;
                                                effbase = 100;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Un-Evolve");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'd')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_HASTE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 3;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Haste");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'e')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_STR)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 1;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Strength");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'f')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_INT)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 2;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Intelligence");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'g')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_WIS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 3;
                                                effbase = 0;
                                                effcost = 50;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Wisdom");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'h')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_DEX)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 4;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Dexterity");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'i')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_CON)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 5;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Constitution");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'j')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_CHR)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 6;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Charisma");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'k')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_BLESSING)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 5;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Blessing");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'l')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_HASTE_OTHER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_OLD_SPEED;
                                                effbase = 30;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Haste Other");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'm')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_PHYS_RESIST)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 1;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 21;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "Physical Resistance");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'n')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_MAGIC_RESIST)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 2;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 21;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "Magic Resistance");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'o')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_STONESKIN)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 3;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 21;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Stoneskin");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'p')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_PARALYZE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_PARALYZE;
                                                effbase = 0;
                                                effcost = 4;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Paralyze");
                                                chosen = TRUE;
                                        }
                                }
                               
                                else
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        /* Choose the appropriate type of interface */
                        /* Depending on the spell. */

                        /* Standard damages interface */
                        if (interfacetype == 1)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Radius:", 8, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Type:", 10, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt  ");
                                else if (spelltype == 2) sprintf(tmp, "Ball  ");
                                else if (spelltype == 3) sprintf(tmp, "Circle");
                                else if (spelltype == 4) sprintf(tmp, "Chain ");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 4) spelltype = 1;
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
                        /* 2. Powerless damages interface */
                        if (interfacetype == 2)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                power = 0;
                                c_put_str(TERM_WHITE, "Radius:", 6, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 6, 8);
                                c_put_str(TERM_WHITE, "Type:", 8, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt  ");
                                else if (spelltype == 2) sprintf(tmp, "Ball  ");
                                else if (spelltype == 3) sprintf(tmp, "Circle");
                                else if (spelltype == 4) sprintf(tmp, "Chain ");
                                c_put_str(TERM_L_GREEN, tmp, 8, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 10, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 10, 11);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 4) spelltype = 1;
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
                        /* 3. Power only interface */
                        /* Used for Haste. Here, the power */
                        /* is the duration. */
                        if (interfacetype == 3)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Duration:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 10);
                                c_put_str(TERM_WHITE, "Mana Cost:", 8, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 4) manacost *= 5;
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 8, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease duration,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Power + Duration interface */
                        /* Used by the Raise spells */
                        if (interfacetype == 4)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Duration:", 8, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 8, 10);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                /* Special code for the Stoneskin spell */
                                if (effkind == 21) manacost = ((effbase + ((effcost * power))) * ((spelltype / 3) + 1)) / 5;
                                else manacost = (effbase + ((effcost * power))) * ((spelltype / 2) + 1);
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease duration,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A') spelltype += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (spelltype > 0) spelltype -= 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
                        /* Power + Duration interface */
                        /* Used by the Resistance spells */
                        /* This one has a max power of 100. */
                        if (interfacetype == 5)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Duration:", 8, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 8, 10);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + (((effcost + (power / 10)) * power))) * spelltype;
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease duration,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q')
                                {
                                        if (power < 100) power += 1;
                                }
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A') spelltype += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (spelltype > 0) spelltype -= 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }


                }
                /* Healings! */
                else if (query == '3')
                {
                        bool chosen = FALSE;
                        bool readytocreate = FALSE;
                        char choseneffect[30];                        
                        char choice;
                        int interfacetype;

                        /* Initialise some stuff */
                        effbase = 0;
                        effcost = 0;
                        efftype = 0;
                        rad = 0;
                        power = 0;
                        spelltype = 1;
                        manacost = 0;

                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Select an effect:", 4, 0);

                        /* All right, let's prepare the list! :) */
                        if (p_ptr->healing_effects & HEAL_HEAL) c_put_str(TERM_WHITE, "[0] Heal", 7, 0);
                        else c_put_str(TERM_L_DARK, "[0] Heal", 7, 0);
                        if (p_ptr->healing_effects & HEAL_RESTORE_STATS) c_put_str(TERM_WHITE, "[1] Restore Stats", 8, 0);
                        else c_put_str(TERM_L_DARK, "[1] Restore Stats", 8, 0);
                        if (p_ptr->healing_effects & HEAL_RESTORE_STATUS) c_put_str(TERM_WHITE, "[2] Restore Status", 9, 0);
                        else c_put_str(TERM_L_DARK, "[2] Restore Status", 9, 0);
                        if (p_ptr->healing_effects & HEAL_CURE_BLEEDING) c_put_str(TERM_WHITE, "[3] Cure Bleeding", 10, 0);
                        else c_put_str(TERM_L_DARK, "[3] Cure Bleeding", 10, 0);
                        if (p_ptr->healing_effects & HEAL_CURE_BLACK_BREATH) c_put_str(TERM_WHITE, "[4] Cure Black Breath", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Cure Black Breath", 11, 0);
                        if (p_ptr->healing_effects & HEAL_HEAL_OTHERS) c_put_str(TERM_WHITE, "[5] Heal Others", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Heal Others", 12, 0);
                        if (p_ptr->healing_effects & HEAL_REVIVE_MONSTER) c_put_str(TERM_WHITE, "[6] Revive Monster", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Revive Monster", 13, 0);
                        if (p_ptr->healing_effects & HEAL_ENERGIZE) c_put_str(TERM_WHITE, "[7] Energize", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Energize", 14, 0);
                        if (p_ptr->healing_effects & HEAL_SATISFY_HUNGER) c_put_str(TERM_WHITE, "[8] Satisfy Hunger", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] Satisfy Hunger", 15, 0);

                        while (chosen == FALSE)
                        {
                                choice = inkey();
                                if (choice == '0')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_HEAL)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 6;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Heal");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '1')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_RESTORE_STATS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 40;
                                                effcost = 0;
                                                effkind = 7;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Restore Stats");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '2')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_RESTORE_STATUS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 40;
                                                effcost = 0;
                                                effkind = 8;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Restore Status");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '3')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_CURE_BLEEDING)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 40;
                                                effcost = 0;
                                                effkind = 9;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Cure Bleeding");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '4')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_CURE_BLACK_BREATH)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 80;
                                                effcost = 0;
                                                effkind = 10;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Cure Black Breath");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '5')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_HEAL_OTHERS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_OLD_HEAL;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Heal Others");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '6')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_REVIVE_MONSTER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 200;
                                                effcost = 0;
                                                effkind = 11;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Revive Monster");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '7')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_ENERGIZE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 0;
                                                effkind = 12;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Energize");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '8')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_SATISFY_HUNGER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 80;
                                                effcost = 0;
                                                effkind = 23;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Satisfy Hunger");
                                                chosen = TRUE;
                                        }
                                }
                               
                                else
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        /* Choose the appropriate type of interface */
                        /* Depending on the spell. */

                        /* Standard damages interface */
                        if (interfacetype == 1)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Radius:", 8, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Type:", 10, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt  ");
                                else if (spelltype == 2) sprintf(tmp, "Ball  ");
                                else if (spelltype == 3) sprintf(tmp, "Circle");
                                else if (spelltype == 4) sprintf(tmp, "Chain ");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power) / 2)) * (rad + 1);
                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }
                                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 4) spelltype = 1;
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
                        /* 2. Power only interface */
                        if (interfacetype == 2)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Mana Cost:", 8, 0);
                                manacost = (effbase + ((effcost * power)));
                                if (manacost > 10) manacost *= (manacost / 5);
                                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 8, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Empty interface */
                        /* You see the mana cost...that's all. */
                        if (interfacetype == 3)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power)));
                                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }


                }
                /* Conjurations! */
                else if (query == '4')
                {
                        bool chosen = FALSE;
                        bool readytocreate = FALSE;
                        char choseneffect[30];                        
                        char choice;
                        int interfacetype;

                        /* Initialise some stuff */
                        effbase = 0;
                        effcost = 0;
                        efftype = 0;
                        rad = 0;
                        power = 0;
                        spelltype = 1;
                        manacost = 0;

                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Select an effect:", 4, 0);

                        /* All right, let's prepare the list! :) */
                        if (p_ptr->conjuration_effects & CONJ_SUMMON_KIND) c_put_str(TERM_WHITE, "[0] Summon Kind", 7, 0);
                        else c_put_str(TERM_L_DARK, "[0] Summon Kind", 7, 0);
                        if (p_ptr->conjuration_effects & CONJ_SUMMON_SPECIFIC) c_put_str(TERM_WHITE, "[1] Summon Specific", 8, 0);
                        else c_put_str(TERM_L_DARK, "[1] Summon Specific", 8, 0);
                        if (p_ptr->conjuration_effects & CONJ_FIRE_FIELD) c_put_str(TERM_WHITE, "[2] Fire Fields", 9, 0);
                        else c_put_str(TERM_L_DARK, "[2] Fire Fields", 9, 0);
                        if (p_ptr->conjuration_effects & CONJ_COLD_FIELD) c_put_str(TERM_WHITE, "[3] Cold Fields", 10, 0);
                        else c_put_str(TERM_L_DARK, "[3] Cold Fields", 10, 0);
                        if (p_ptr->conjuration_effects & CONJ_ELEC_FIELD) c_put_str(TERM_WHITE, "[4] Electric Fields", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Electric Fields", 11, 0);
                        if (p_ptr->conjuration_effects & CONJ_WEBS) c_put_str(TERM_WHITE, "[5] Webs", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Webs", 12, 0);
                        if (p_ptr->conjuration_effects & CONJ_GROW_TREES) c_put_str(TERM_WHITE, "[6] Grow Trees", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Grow Trees", 13, 0);
                        if (p_ptr->conjuration_effects & CONJ_THORNS) c_put_str(TERM_WHITE, "[7] Thorned Vines", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Thorned Vines", 14, 0);
                        if (p_ptr->conjuration_effects & CONJ_STORMS) c_put_str(TERM_WHITE, "[8] Storms", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] Storms", 15, 0);
                        if (p_ptr->conjuration_effects & CONJ_ITEM) c_put_str(TERM_WHITE, "[9] Conjure Item", 16, 0);
                        else c_put_str(TERM_L_DARK, "[9] Conjure Item", 16, 0);
                        if (p_ptr->conjuration_effects & CONJ_MAGIC_ITEM) c_put_str(TERM_WHITE, "[a] Conjure Magic Item", 17, 0);
                        else c_put_str(TERM_L_DARK, "[a] Conjure Magic Item", 17, 0);
                        if (p_ptr->conjuration_effects & CONJ_SPECIAL_ITEM) c_put_str(TERM_WHITE, "[b] Conjure Special Item", 18, 0);
                        else c_put_str(TERM_L_DARK, "[b] Conjure Special Item", 18, 0);


                        while (chosen == FALSE)
                        {
                                choice = inkey();
                                if (choice == '0')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_SUMMON_KIND)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 13;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Summon Kind");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '1')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_SUMMON_SPECIFIC)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 0;
                                                effkind = 14;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Summon Specific");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '2')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_FIRE_FIELD)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_FIRE_FIELD;
                                                effbase = 0;
                                                effcost = 2;
                                                effkind = 15;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Fire Fields");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '3')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_COLD_FIELD)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_COLD_FIELD;
                                                effbase = 0;
                                                effcost = 2;
                                                effkind = 15;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Cold Fields");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '4')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_ELEC_FIELD)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_ELEC_FIELD;
                                                effbase = 0;
                                                effcost = 2;
                                                effkind = 15;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Electric Fields");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '5')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_WEBS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_WEBS;
                                                effbase = 75;
                                                effcost = 0;
                                                effkind = 15;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Webs");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '6')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_GROW_TREES)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_TREES;
                                                effbase = 75;

                                                effcost = 0;
                                                effkind = 15;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Grow Trees");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '7')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_THORNS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_THORNED_VINES;
                                                effbase = 0;
                                                effcost = 2;
                                                effkind = 15;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Thorned Vines");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '8')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_STORMS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_STORMS;
                                                effbase = 0;
                                                effcost = 2;
                                                effkind = 15;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Storms");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '9')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_ITEM)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 1;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 22;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "Conjure Item");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'a')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_MAGIC_ITEM)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 2;
                                                effbase = 100;
                                                effcost = 5;
                                                effkind = 22;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "Conjure Magic Item");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'b')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_SPECIAL_ITEM)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 3;
                                                effbase = 500;
                                                effcost = 20;
                                                effkind = 22;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "Conjure Special Item");
                                                chosen = TRUE;
                                        }
                                }

                               
                                else
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        /* Choose the appropriate type of interface */
                        /* Depending on the spell. */

                        /* Summon Kind Interface */
                        if (interfacetype == 1)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        msg_print("Enter the monster type.  ");
                        summch = inkey();
                        if (effectschoose == 0) spell_ptr->schar1 = summch;
                        if (effectschoose == 1) spell_ptr->schar2 = summch;
                        if (effectschoose == 2) spell_ptr->schar3 = summch;
                        if (effectschoose == 3) spell_ptr->schar4 = summch;
                        if (effectschoose == 4) spell_ptr->schar5 = summch;
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Monster Kind:", 6, 0);
                                sprintf(tmp, "%c          ", summch);
                                c_put_str(TERM_L_GREEN, tmp, 6, 14);                                
                                c_put_str(TERM_WHITE, "Power:", 8, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 8, 7);
                                c_put_str(TERM_WHITE, "Number:", 10, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 10, 8);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                manacost = manacost * spelltype;
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease number,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A') spelltype += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (spelltype > 0) spelltype -= 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        msg_print("Enter the monster type.  ");
                                        summch = inkey();
                                        if (effectschoose == 0) spell_ptr->schar1 = summch;
                                        if (effectschoose == 1) spell_ptr->schar2 = summch;
                                        if (effectschoose == 2) spell_ptr->schar3 = summch;
                                        if (effectschoose == 3) spell_ptr->schar4 = summch;
                                        if (effectschoose == 4) spell_ptr->schar5 = summch;
                                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Summon Specific Interface */
                        if (interfacetype == 2)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        /* Go to the "name" field */
                        move_cursor(6, 14);

                        /* Get an input, ignore "Escape" */
                        if (askfor_aux(tmpsumm, 29))
                        {                                
                                if (effectschoose == 0) strcpy(spell_ptr->sspeci1, tmpsumm);
                                if (effectschoose == 1) strcpy(spell_ptr->sspeci2, tmpsumm);
                                if (effectschoose == 2) strcpy(spell_ptr->sspeci3, tmpsumm);
                                if (effectschoose == 3) strcpy(spell_ptr->sspeci4, tmpsumm);
                                if (effectschoose == 4) strcpy(spell_ptr->sspeci5, tmpsumm);
                        }

                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Monster Name:", 6, 0);
                                sprintf(tmp, "%s          ", tmpsumm);
                                c_put_str(TERM_L_GREEN, tmp, 6, 14);                                
                                c_put_str(TERM_WHITE, "Number:", 8, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = get_name_manacost(tmpsumm);
                                manacost = manacost * spelltype;
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);                                
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease number,", 16, 0);
                                c_put_str(TERM_WHITE, "[n] to change name,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'a' || choice == 'A') spelltype += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (spelltype > 0) spelltype -= 1;
                                }
                                if (choice == 'n' || choice == 'N')
                                {
                                        /* Go to the "name" field */
                                        move_cursor(6, 14);

                                        /* Get an input, ignore "Escape" */
                                        if (askfor_aux(tmpsumm, 29))
                                        {                                
                                                if (effectschoose == 0) strcpy(spell_ptr->sspeci1, tmpsumm);
                                                if (effectschoose == 1) strcpy(spell_ptr->sspeci2, tmpsumm);
                                                if (effectschoose == 2) strcpy(spell_ptr->sspeci3, tmpsumm);
                                                if (effectschoose == 3) strcpy(spell_ptr->sspeci4, tmpsumm);
                                                if (effectschoose == 4) strcpy(spell_ptr->sspeci5, tmpsumm);
                                        }
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Damages interface for fields! */
                        if (interfacetype == 3)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Radius:", 8, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Type:", 10, 0);
                                if (spelltype == 1) sprintf(tmp, "Circle      ");
                                else if (spelltype == 2) sprintf(tmp, "Free Placing");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 2) manacost *= 2;
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'a') rad += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 2) spelltype = 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
                        /* Fields that do no damages! */
                        if (interfacetype == 4)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                power = 0;
                                c_put_str(TERM_WHITE, "Radius:", 6, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 6, 8);
                                c_put_str(TERM_WHITE, "Type:", 8, 0);
                                if (spelltype == 1) sprintf(tmp, "Circle      ");
                                else if (spelltype == 2) sprintf(tmp, "Free Placing");
                                c_put_str(TERM_L_GREEN, tmp, 8, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 2) manacost *= 2;
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);                               
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 16, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'a' || choice == 'a') rad += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 2) spelltype = 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Conjure Item Interface */
                        if (interfacetype == 5)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 1;
                        /* Go to the "name" field */
                        move_cursor(6, 14);

                        /* Get an input, ignore "Escape" */
                        if (askfor_aux(tmpsumm, 29))
                        {                                
                                if (effectschoose == 0) strcpy(spell_ptr->sspeci1, tmpsumm);
                                if (effectschoose == 1) strcpy(spell_ptr->sspeci2, tmpsumm);
                                if (effectschoose == 2) strcpy(spell_ptr->sspeci3, tmpsumm);
                                if (effectschoose == 3) strcpy(spell_ptr->sspeci4, tmpsumm);
                                if (effectschoose == 4) strcpy(spell_ptr->sspeci5, tmpsumm);
                        }

                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Object Name:", 6, 0);
                                sprintf(tmp, "%s          ", tmpsumm);
                                c_put_str(TERM_L_GREEN, tmp, 6, 14);                                
                                c_put_str(TERM_WHITE, "Duration:", 8, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 8, 10);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = get_object_manacost(tmpsumm);
                                manacost = manacost * spelltype;
                                manacost = manacost * effcost;
                                manacost += effbase;
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);                                
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease duration,", 16, 0);
                                c_put_str(TERM_WHITE, "[n] to change name,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'a' || choice == 'A') spelltype += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (spelltype > 1) spelltype -= 1;
                                }
                                if (choice == 'n' || choice == 'N')
                                {
                                        /* Go to the "name" field */
                                        move_cursor(6, 14);

                                        /* Get an input, ignore "Escape" */
                                        if (askfor_aux(tmpsumm, 29))
                                        {                                
                                                if (effectschoose == 0) strcpy(spell_ptr->sspeci1, tmpsumm);
                                                if (effectschoose == 1) strcpy(spell_ptr->sspeci2, tmpsumm);
                                                if (effectschoose == 2) strcpy(spell_ptr->sspeci3, tmpsumm);
                                                if (effectschoose == 3) strcpy(spell_ptr->sspeci4, tmpsumm);
                                                if (effectschoose == 4) strcpy(spell_ptr->sspeci5, tmpsumm);
                                        }
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }


                }
                /* Divinations! */
                else if (query == '5')
                {
                        bool chosen = FALSE;
                        bool readytocreate = FALSE;
                        char choseneffect[30];                        
                        char choice;
                        int interfacetype;

                        /* Initialise some stuff */
                        effbase = 0;
                        effcost = 0;
                        efftype = 0;
                        rad = 0;
                        power = 0;
                        spelltype = 1;
                        manacost = 0;

                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Select an effect:", 4, 0);

                        /* All right, let's prepare the list! :) */
                        if (p_ptr->divination_effects & DIVI_DETECT_MONSTERS) c_put_str(TERM_WHITE, "[0] Detect Monsters", 7, 0);
                        else c_put_str(TERM_L_DARK, "[0] Detect Monsters", 7, 0);
                        if (p_ptr->divination_effects & DIVI_DETECT_OBJECTS) c_put_str(TERM_WHITE, "[1] Detect Objects", 8, 0);
                        else c_put_str(TERM_L_DARK, "[1] Detect Objects", 8, 0);
                        if (p_ptr->divination_effects & DIVI_DETECT_DOORS) c_put_str(TERM_WHITE, "[2] Detect Doors", 9, 0);
                        else c_put_str(TERM_L_DARK, "[2] Detect Doors", 9, 0);
                        if (p_ptr->divination_effects & DIVI_DETECT_STAIRS) c_put_str(TERM_WHITE, "[3] Detect Stairs", 10, 0);
                        else c_put_str(TERM_L_DARK, "[3] Detect Stairs", 10, 0);
                        if (p_ptr->divination_effects & DIVI_DETECT_TRAPS) c_put_str(TERM_WHITE, "[4] Detect Traps", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Detect Traps", 11, 0);
                        if (p_ptr->divination_effects & DIVI_TELEPATHY) c_put_str(TERM_WHITE, "[5] Telepathy", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Telepathy", 12, 0);
                        if (p_ptr->divination_effects & DIVI_IDENTIFY) c_put_str(TERM_WHITE, "[6] Identify", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Identify", 13, 0);
                        if (p_ptr->divination_effects & DIVI_SCAN_MONSTER) c_put_str(TERM_WHITE, "[7] Scan Monster", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Scan Monster", 14, 0);
                        if (p_ptr->divination_effects & DIVI_REVEAL) c_put_str(TERM_WHITE, "[8] Reveal", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] Reveal", 15, 0);



                        while (chosen == FALSE)
                        {
                                choice = inkey();
                                if (choice == '0')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_DETECT_MONSTERS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 1;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 16;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Detect Monsters");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '1')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_DETECT_OBJECTS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 2;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 16;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Detect Objects");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '2')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_DETECT_DOORS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 3;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 16;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Detect Doors");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '3')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_DETECT_STAIRS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 4;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 16;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Detect Stairs");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '4')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_DETECT_TRAPS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 5;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 16;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Detect Traps");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '5')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_TELEPATHY)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 17;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Telepathy");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '6')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_IDENTIFY)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 18;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Identify");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '7')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_SCAN_MONSTER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 40;
                                                effcost = 0;
                                                effkind = 19;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Scan Monster");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '8')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_REVEAL)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 20;
                                                effcost = 0;
                                                effkind = 20;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Reveal");
                                                chosen = TRUE;
                                        }
                                }
                               
                                else
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        /* Choose the appropriate type of interface */
                        /* Depending on the spell. */

                        /* Empty interface */
                        /* You see the mana cost...that's all. */
                        if (interfacetype == 1)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power)));
                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* 2. Power only interface */
                        if (interfacetype == 2)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Mana Cost:", 8, 0);
                                manacost = (effbase + ((effcost * power)));
                                if (manacost > 10) manacost *= (manacost / 5);
                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 8, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Reveal */
                        if (interfacetype == 3)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                power = 0;
                                c_put_str(TERM_WHITE, "Radius:", 6, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 6, 8);
                                c_put_str(TERM_WHITE, "Type:", 8, 0);
                                if (spelltype == 1) sprintf(tmp, "Circle      ");
                                else if (spelltype == 2) sprintf(tmp, "Free Placing");
                                c_put_str(TERM_L_GREEN, tmp, 8, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 2) manacost *= 2;
                                sprintf(tmp, "%d          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);                               
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 16, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[x] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'a' || choice == 'a') rad += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 2) spelltype = 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X')
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }


                }




                spell_ptr->school[effectschoose] = 1;
                spell_ptr->power[effectschoose] = power;
                spell_ptr->radius[effectschoose] = rad;
                spell_ptr->type[effectschoose] = efftype;
                spell_ptr->shape[effectschoose] = spelltype;
                spell_ptr->effect[effectschoose] = effkind;
                spell_ptr->manacost[effectschoose] = manacost;
                if (effectschoose < 4)
                {
                        if (!get_com("Do you want to add another effect? [y/n]", &query))
                        {
                                Term_load();
                                return;
                        }
                        if (query == 'y' || query == 'Y')
                        {
                                effectschoose += 1;
                                effnum += 1;
                        }
                        else effectschoose = 5;
                }
        }


        /* We're done with the creation! */
        /* Now, let's proceed to the final part! :) */
        /* Perpare the screen */
        c_put_str(TERM_WHITE, "", 0, 0);
        for (i = 0; i < SCREEN_HGT; i++)
        {
                roff("\n");
        }
        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
        for (i = 0; i < 5; i++)
        {
                finalcost = finalcost + spell_ptr->manacost[i];
        }
        finalcost = finalcost * effnum;
        c_put_str(TERM_WHITE, "Final Cost:", 8, 0);
        sprintf(tmp, "%d          ", finalcost);
        c_put_str(TERM_L_GREEN, tmp, 8, 12);
        c_put_str(TERM_WHITE, "Name:", 4, 0);

        /* Go to the "name" field */
        move_cursor(4, 6);

        /* Get an input, ignore "Escape" */
        if (askfor_aux(tmp, 29)) strcpy(spell_ptr->name, tmp);

        c_put_str(TERM_WHITE, "Press 'c' to create spell, or any key to exit.", 15, 0);

        query = inkey();
        if (query == 'c' || query == 'C')
        {
                msg_print("Spell created!");
                spell_ptr->finalcost = finalcost;
                spell_ptr->created = TRUE;
        }
        Term_load();
}

/* Returns the mana cost of a specific creature */
int get_name_manacost(char name[30])
{
        int x;
        int manacost = 0;
        bool cansummon = FALSE;

        for (x = 0; x <= max_r_idx; x++)
        {
                monster_race *r_ptr = &r_info[x];
                if (strstr((r_name + r_ptr->name), name) && !(r_ptr->flags1 & (RF1_UNIQUE)))
                {
                        cansummon = TRUE;
                        manacost = r_ptr->level * 6;
                        manacost = manacost * ((r_ptr->level / 40) + 1);
                        return (manacost);
                }
        }
        if (cansummon == FALSE) return (0);
}

/* Delete a spell */
void delete_spell()
{
	int                     Power = -1;
        int                     num = 0, i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask;

        char            choice;

	char            out_val[160];
        magic_spells *spell_ptr;

        for (i = 1; i < 29; i++)
        {
                char spellstring[80];
                spell_ptr = &magic_spell[i];
                if (spell_ptr->created == TRUE)
                {
                        sprintf(spellstring, "%s   Cost: %d", spell_ptr->name, spell_ptr->finalcost);        
                        /* List the powers */
                        strcpy(power_desc[num],spellstring);powers[num++]=i;
                }
        }

        if(!num) {msg_print("No spells to delete.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Delete which spell? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Delete which spell? ",
			I2A(0), '0' + num - 27);
	}

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

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
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
                        strnfmt(tmp_val, 78, "Delete %s? ", power_desc[i]);

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
                return num;
	}

        spell_ptr = &magic_spell[Power];
        for (i = 0; i < 5; i++)
        {
                spell_ptr->effect[i] = 0;
                spell_ptr->manacost[i] = 0;
        }
        spell_ptr->created = FALSE;
        msg_print("Spell deleted.");
}

/* Erase all created spells! */
void delete_all_spells()
{
        int x, i;
        magic_spells *spell_ptr;

        for (x = 0; x < 29; x++)
        {
                spell_ptr = &magic_spell[x];
                for (i = 0; i < 5; i++)
                {
                        spell_ptr->effect[i] = 0;
                        spell_ptr->manacost[i] = 0;
                }
                spell_ptr->created = FALSE;
        }
}        

/* Returns the mana cost of a specific item */
int get_object_manacost(char name[30])
{
        int x;
        int manacost = 0;
        bool cansummon = FALSE;

        for (x = 1; x <= max_k_idx; x++)
        {
                object_kind *k_ptr = &k_info[x];
                if (strstr((k_name + k_ptr->name), name) && !(k_ptr->flags4 & (TR4_SPECIAL_GENE)) && !(k_ptr->flags3 & (TR3_INSTA_ART)))
                {
                        cansummon = TRUE;
                        /* With this formula, junk is VERY cheap to conjure */
                        /* while great stuff is very expensive */
                        manacost = k_ptr->locale[0] / 5;
                        manacost += manacost * ((k_ptr->locale[0] / 11) * 4);
                        manacost = manacost / 10;                         
                        if (manacost < 1) manacost = 1;
                        return (manacost);
                }
        }
        if (cansummon == FALSE) return (0);
}

void conjure_specific_item(char name[30], int dur, bool magic, bool special)
{
        int x;

        for (x = 1; x <= max_k_idx; x++)
        {
                object_kind *k_ptr = &k_info[x];
                if (strstr((k_name + k_ptr->name), name) && !(k_ptr->flags4 & (TR4_SPECIAL_GENE)) && !(k_ptr->flags3 & (TR3_INSTA_ART)) && !(strstr((k_name + k_ptr->name), "Broken")))
                {
                        conjure_item(k_ptr->tval, k_ptr->sval, dur, magic, special);
                        return;
                }
        }
        msg_print("Invalid item.");
        return;
}
