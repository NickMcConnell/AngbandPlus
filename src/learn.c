/* File: learn.c */

/* Purpose: Allow player to learn(and use) monsters attacks! */
/* Also allow to learn abilities */


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

        char            choice, ch;

	char            out_val[160];
        int k,count;
        int rlev = dun_level + p_ptr->lev + randint(5);
	int rad;

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
        if(p_ptr->monster_magic4 & RF6_S_UNIQUE && magictype == 4) {strcpy(power_desc[num],"Summon Unique");powers[num++]=91;}
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
                               fire_bolt(GF_ARROW, dir, damroll(1,6));
                        break;
                case 3: /* Arrow2 */
                        msg_print("You fire a heavy arrow...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(3,6));
                        break;
                case 4: /* Arrow3 */
                        msg_print("You fire a light missile...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(5,6));
                        break;
                case 5: /* Arrow4 */
                        msg_print("You fire a heavy missile...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(7,6));
                        break;
                case 6: /* Br acid */
                        msg_print("You breathe acid ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, p_ptr->chp * 2, rad);
                        break;
                case 7: /* Br elec */
                        msg_print("You breathe lightning ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, p_ptr->chp * 2, rad);
                        break;
                case 8: /* br fire */
                        msg_print("You breathe fire ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, p_ptr->chp * 2, rad);
                        break;
                case 9: /* br cold */
                        msg_print("You breathe cold ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, p_ptr->chp * 2, rad);
                        break;
                case 10: /* br pois */
                        msg_print("You breathe poison ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, p_ptr->chp * 2, rad);
                        break;
                case 11: /* br neth */
                        msg_print("You breathe nether ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, p_ptr->chp * 2, rad);
                        break;
                case 12: /* br lite */
                        msg_print("You breathe lite ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_LITE, dir, p_ptr->chp * 2, rad);
                        break;
                case 13: /* br dark */
                        msg_print("You breathe dark ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, p_ptr->chp * 2, rad);
                        break;
                case 14: /* br conf */
                        msg_print("You breathe confusion ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CONFUSION, dir, p_ptr->chp, rad);
                        break;
                case 15: /* br soun */
                        msg_print("You breathe sound ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SOUND, dir, p_ptr->chp, rad);
                        break;
                case 16: /* br chao */
                        msg_print("You breathe chaos ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, p_ptr->chp * 2, rad);
                        break;
                case 17: /* br dise */
                        msg_print("You breathe disenchantment ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISENCHANT, dir, p_ptr->chp, rad);
                        break;
                case 18: /* br nexu */
                        msg_print("You breathe nexus ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NEXUS, dir, p_ptr->chp * 2, rad);
                        break;
                case 19: /* br time */
                        msg_print("You breathe time ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_TIME, dir, p_ptr->chp * 2, rad);
                        break;
                case 20: /* br iner */
                        msg_print("You breathe inertia ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_INERTIA, dir, p_ptr->chp * 2, rad);
                        break;
                case 21: /* br grav */
                        msg_print("You breathe gravity ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_GRAVITY, dir, p_ptr->chp * 2, rad);
                        break;
                case 22: /* br shar */
                        msg_print("You breathe shards ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SHARDS, dir, p_ptr->chp, rad);
                        break;
                case 23: /* br plas */
                        msg_print("You breathe plasma ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_PLASMA, dir, p_ptr->chp * 2, rad);
                        break;
                case 24: /* br wall */
                        msg_print("You breathe force ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FORCE, dir, p_ptr->chp * 2, rad);
                        break;
                case 25: /* br mana */
                        msg_print("You breathe mana ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, p_ptr->chp * 2, rad);
                        break;
                case 26: /* ba nuke */
                        msg_print("You cast a ball of nuke ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, p_ptr->chp * 2, 1 + (p_ptr->lev/20));
                        break;
                case 27: /* br nuke */
                        msg_print("You breathe nuke ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, p_ptr->chp * 2, 1 + (p_ptr->lev/20));
                        break;
                case 28: /* ba chao */
                        msg_print("You cast a ball of chaos ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, p_ptr->chp * 2, 2);
                        break;
                case 29: /* br disi */
                        msg_print("You breathe disintegration ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISINTEGRATE, dir, p_ptr->chp * 2, 1 + (p_ptr->lev/20));
                        break;
                case 30: /* ba acid */
                        msg_print("You cast a ball of acid ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, randint(p_ptr->lev * 15)+20, 2);
                        break;
                case 31: /* ba elec */
                        msg_print("You cast a ball of lightning ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, randint(p_ptr->lev * 15)+20, 2);
                        break;
                case 32: /* ba fire */
                        msg_print("You cast a ball of fire ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, randint(p_ptr->lev * 15)+20, 2);
                        break;
                case 33: /* ba cold */
                        msg_print("You cast a ball of cold ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, randint(p_ptr->lev * 15)+20, 2);
                        break;
                case 34: /* ba pois */
                        msg_print("You cast a ball of poison ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, damroll(12,2), 2);
                        break;
                case 35: /* ba neth */
                        msg_print("You cast a ball of nether ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, randint(p_ptr->lev * 10)+20, 2);
                        break;
                case 36: /* ba wate */
                        msg_print("You cast a ball of water ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_WATER, dir, randint(p_ptr->lev * 10)+20, 2);
                        break;
                case 37: /* ba mana */
                        msg_print("You cast a ball of mana ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, randint(p_ptr->lev * 10)+20, 2);
                        break;
                case 38: /* ba dark */
                        msg_print("You cast a ball of darkness ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, randint(p_ptr->lev * 10)+20, 2);
                        break;
	        case 42: /* cause1 */
		        msg_print("You cause light wounds ...");
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(3, 8));
			}
		        break;
	        case 43: /* cause2 */
		        msg_print("You cause serious wounds ...");
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(8, 8));
			}
		        break;
	        case 44: /* cause3 */
		        msg_print("You cause critical wounds ...");
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(10, 15));
			}
		        break;
	        case 45: /* cause4 */
		        msg_print("You cause mortal wounds ...");
		        if (get_aim_dir(&dir))
		        {
			        fire_bolt(GF_MANA, dir, damroll(15, 15));
			}
		        break;
                case 46: /* bo acid */
                        msg_print("You cast a bolt of acid ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ACID, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 47: /* bo elec */
                        msg_print("You cast a bolt of lightning ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ELEC, dir, damroll(4, 8) + (p_ptr->lev/3));
                        break;
                case 48: /* bo fire */
                        msg_print("You cast a bolt of fire ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_FIRE, dir, damroll(9, 8) + (p_ptr->lev/3));
                        break;
                case 49: /* bo cold */
                        msg_print("You cast a bolt of cold ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_COLD, dir, damroll(6, 8) + (p_ptr->lev/3));
                        break;
                case 56: /* bo pois */
                        msg_print("You cast a bolt of poison ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_POIS, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 50: /* bo neth */
                        msg_print("You cast a bolt of nether ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_NETHER, dir, damroll(5, 5) + (p_ptr->lev/3));
                        break;
                case 51: /* bo wate */
                        msg_print("You cast a bolt of water ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_WATER, dir, damroll(10, 10) + (p_ptr->lev/3));
                        break;
                case 52: /* bo mana */
                        msg_print("You cast a bolt of mana ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MANA, dir, damroll(3, 8) + (p_ptr->lev/3));
                        break;
                case 53: /* bo plas */
                        msg_print("You cast a bolt of plasma ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_PLASMA, dir, damroll(8, 8) + (p_ptr->lev/3));
                        break;
                case 54: /* bo ice */
                        msg_print("You cast a bolt of ice ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ICE, dir, damroll(6, 6) + (p_ptr->lev/3));
                        break;
                case 55: /* missile */
                        msg_print("You cast a magic missile ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MISSILE, dir, damroll(2, 6) + (p_ptr->lev/3));
                        break;
                case 57: /* blind */
                        msg_print("You cast blindness ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(1, 8) + (p_ptr->lev/3));
                        break;
                case 41: /* scare */
                        msg_print("You cast scare ...");
			if (get_aim_dir(&dir))
				fear_monster(dir, plev);
                        break;
                case 58: /* conf */
                        msg_print("You cast a bolt of confusion ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 59: /* slow */
                        msg_print("You cast a bolt of unspeed ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLOW, dir, damroll(6, 8) + (p_ptr->lev/3));
                        break;
                case 60: /* hold */
                        msg_print("You cast a bolt of paralisation ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLEEP, dir, damroll(5, 8) + (p_ptr->lev/3));
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
                                fire_bolt(GF_MANA, dir, damroll(10, 8) + (p_ptr->lev));
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

/* Simple function to check if an ability is present or not! */
bool ability(int abil)
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_ABILITY];

        if (o_ptr->sval == abil) return TRUE;
        else return FALSE;
}

/* Used in learn_ability() */
void create_ability(int svalnum)
{
        object_type	forge;
	object_type	*q_ptr;
	

	/* Get local object */
	q_ptr = &forge;

        
        object_prep(q_ptr, lookup_kind(TV_ABILITY, svalnum));
        q_ptr->number = 1;
        object_aware(q_ptr);
        object_known(q_ptr);

        /* These objects are "storebought" */
        q_ptr->ident |= IDENT_STOREB;

        (void)inven_carry(q_ptr, TRUE);
        msg_print("You gained a new ability!");
}

/* Allow player to learn an ability */
int learn_ability()
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

	char            choice;

	char            out_val[160];
        /* int x=px,y=py,k,count; */

        /* List the powers */
        if(p_ptr->pclass == CLASS_WARRIOR)
        {
                strcpy(power_desc[num],"Battle Bonus(Warrior's battle bonuses)          200 points");powers[num++]=1;
                strcpy(power_desc[num],"Extra Attack(Get an extra attack)               1000 points");powers[num++]=2;
                strcpy(power_desc[num],"Hp 50%(50% more hp)                             2500 points");powers[num++]=3;
                strcpy(power_desc[num],"Weapon mastery(Any class can equip any weapons) 10000 points");powers[num++]=4;
        }
        if(p_ptr->pclass == CLASS_MAGE)
        {
                strcpy(power_desc[num],"Mana +30(Get 30 extra mana points.)             150 points");powers[num++]=5;
                strcpy(power_desc[num],"Mana 25%(Increase mana by 25%.)                 600 points");powers[num++]=6;
                strcpy(power_desc[num],"Knowledge(Sightly improve spells.)              1000 points");powers[num++]=7;                
        }
        if(p_ptr->pclass == CLASS_PRIEST)
        {
                strcpy(power_desc[num],"Saving(Increase Saving Throw.)                  100 points");powers[num++]=8;
                strcpy(power_desc[num],"Wisdom(Increase Wisdom by 2.)                   750 points");powers[num++]=9;
                strcpy(power_desc[num],"Divine Regeneration(Regenerate very quickly)    25000 points");powers[num++]=10;                
        }
        if(p_ptr->pclass == CLASS_ROGUE)
        {
                strcpy(power_desc[num],"Stealth(Increase Stealth.)                      300 points");powers[num++]=11;
                strcpy(power_desc[num],"Blink Away(Use 'Blink Away' command.)           300 points");powers[num++]=12;
                strcpy(power_desc[num],"Thieving(More chances to steal items in shops.) 2500 points");powers[num++]=13;
                strcpy(power_desc[num],"Thieving Mastery(100% chances to steal.)        200000 points");powers[num++]=14;
        }
        if(p_ptr->pclass == CLASS_RANGER)
        {
                strcpy(power_desc[num],"Agility(Dexterity +2, 'to hit' + 10.)           800 points");powers[num++]=15;
                strcpy(power_desc[num],"Quick Feets(Speed +1.)                          800 points");powers[num++]=16;
                strcpy(power_desc[num],"Mana 10%(Increase mana by 10%.)                 300 points");powers[num++]=17;
                strcpy(power_desc[num],"Hp 25%(Increase hp by 25%.)                     1250 points");powers[num++]=18;
                strcpy(power_desc[num],"Extra Shot(One Extra shot.)                     1100 points");powers[num++]=19;
        }
        if(p_ptr->pclass == CLASS_PALADIN)
        {
                strcpy(power_desc[num],"Holy Light(Increase light radius by 1.)         75 points");powers[num++]=20;
                strcpy(power_desc[num],"Charisma(Increase Charisma by 2.)               200 points");powers[num++]=21;
                strcpy(power_desc[num],"Ball Of Light(Use 'Ball Of Light' command.)     350 points");powers[num++]=22;                
        }
        if(p_ptr->pclass == CLASS_WARRIOR_MAGE)
        {
                strcpy(power_desc[num],"Hp/Mana 10%(Increase Hp and Mana by 10%.)       1000 points");powers[num++]=23;
                strcpy(power_desc[num],"Hp/Mana 25%(Increase Hp and Mana by 25%.)       5000 points");powers[num++]=24;                                
        }
        if(p_ptr->pclass == CLASS_CHAOS_WARRIOR)
        {
                strcpy(power_desc[num],"Fury(GREAT speed, low stats.)                   1000 points");powers[num++]=25;                                                
        }
        if(p_ptr->pclass == CLASS_MONK)
        {
                strcpy(power_desc[num],"Constitution(Increase Constitution by 2.)       900 points");powers[num++]=26;
                strcpy(power_desc[num],"Martial Arts(Fight bare-handed like a Monk.)    3000 points");powers[num++]=27;                
        }
        if(p_ptr->pclass == CLASS_MINDCRAFTER)
        {
                strcpy(power_desc[num],"MindCrafting(Mindcrafters powers.)               1000 points");powers[num++]=28;
                strcpy(power_desc[num],"Invoke Spirit(Summon a spirit to help you.)      1500 points");powers[num++]=29;                
        }
        if(p_ptr->pclass == CLASS_HIGH_MAGE)
        {
                strcpy(power_desc[num],"Mana 50%(Increase mana by 50%.)                  1200 points");powers[num++]=30;
                strcpy(power_desc[num],"Mage Lore(Improve spells more than 'Knowledge'.) 4000 points");powers[num++]=31;
                strcpy(power_desc[num],"Mage Staff(Create a Mage Staff.)                 800 points");powers[num++]=32;
        }
        if(p_ptr->pclass == CLASS_MIMIC)
        {
                strcpy(power_desc[num],"Mimic(Use Mimic powers.)                         1000 points");powers[num++]=33;
        }
        if(p_ptr->pclass == CLASS_ALCHEMIST)
        {
                strcpy(power_desc[num],"Alchemy(Use alchemy.)                            6000 points");powers[num++]=34;
        }
        if(p_ptr->pclass == CLASS_POWERMAGE)
        {
                strcpy(power_desc[num],"Power Magic(Use Power Mages powers.)             1000 points");powers[num++]=35;
                strcpy(power_desc[num],"Mana 75%(Increase Mana by 75%.)                  2000 points");powers[num++]=36;                
        }
        if(p_ptr->pclass == CLASS_RUNECRAFTER)
        {
                strcpy(power_desc[num],"Rune Crafting(Use Rune crafters powers.)         1000 points");powers[num++]=37;
                strcpy(power_desc[num],"Tweaking(Allow to tweak ANY items.)              50000 points");powers[num++]=38;                
        }
        if(p_ptr->pclass == CLASS_POSSESSOR)
        {
                strcpy(power_desc[num],"Possessing(Use Possessors powers.)               3000 points");powers[num++]=39;
                strcpy(power_desc[num],"Wraith Form(Become incorporal.)                  35000 points");powers[num++]=40;                
        }
        if(p_ptr->pclass == CLASS_SORCERER)
        {
                strcpy(power_desc[num],"Mana 100%(Increase mana by 100%.)                4000 points");powers[num++]=41;
                strcpy(power_desc[num],"Spell Mastery(Greatly improve spells.)           10000 points");powers[num++]=42;
                strcpy(power_desc[num],"Sorcery(Use any spell books.)                    7000 points");powers[num++]=43;
        }
        if(p_ptr->pclass == CLASS_ARCHER)
        {
                strcpy(power_desc[num],"Extra Might(Shoot with Extra Might.)             1000 points");powers[num++]=44;
                strcpy(power_desc[num],"Ammo Making(Make ammos from junk.)               1000 points");powers[num++]=45;
                strcpy(power_desc[num],"Double Shots(2 extra shots.)                     3000 points");powers[num++]=46;
        }
        if(p_ptr->pclass == CLASS_ILLUSIONIST)
        {
                strcpy(power_desc[num],"Illusion(Creates an illusion to distract foes.)  100 points");powers[num++]=47;
        }
        if(p_ptr->pclass == CLASS_NECRO)
        {
                strcpy(power_desc[num],"Necromancy(Use necromancy.)                      2500 points");powers[num++]=48;
        }
        if(p_ptr->pclass == CLASS_MAGIWARRIOR)
        {
                strcpy(power_desc[num],"Hp/Mana 50%(Increase Hp and Mana by 50%.)       10000 points");powers[num++]=49;
                strcpy(power_desc[num],"Hp/Mana 75%(Increase Hp and Mana by 75%.)       15000 points");powers[num++]=50;
                strcpy(power_desc[num],"Quadra(Comsume 50 mana when you hit, dam x4.)   25000 points");powers[num++]=51;
                
        }
        if(p_ptr->pclass == CLASS_BERSERKER)
        {
                strcpy(power_desc[num],"Berserk(Always berserk.)                        1500 points");powers[num++]=52;
                strcpy(power_desc[num],"Hp 200%(Increase Hp by 200%.)                   20000 points");powers[num++]=53;
                strcpy(power_desc[num],"Triple Blows(3 extra attacks.)                  12000 points");powers[num++]=54;
                
        }
        if(p_ptr->pclass == CLASS_DARK_LORD)
        {
                strcpy(power_desc[num],"Devastation Mastery(Equip Devastation Swords.)  10000 points");powers[num++]=55;
                strcpy(power_desc[num],"Threaten(Threaten the Shops to get free items.) 10000 points");powers[num++]=56;
                strcpy(power_desc[num],"Devastation Aura(Damaging, large radius aura.)  30000 points");powers[num++]=57;
                strcpy(power_desc[num],"Devastation Beam(Beam that does 10000 damages.) 100000 points");powers[num++]=58;
                
        }
        if(p_ptr->pclass == CLASS_JUSTICE_WARRIOR)
        {
                strcpy(power_desc[num],"Bless Item(Item becomes magic.)                 200000 points");powers[num++]=59;
                strcpy(power_desc[num],"Divine Bolt(Bolt of Holy Energy.)               1000 points");powers[num++]=60;
                strcpy(power_desc[num],"Invoke Angel(Summon an angel.)                  5000 points");powers[num++]=61;
                
        }
        if(p_ptr->pclass == CLASS_LEADER)
        {
                strcpy(power_desc[num],"LeaderShip(Recruit and lead creatures.)         5000 points");powers[num++]=62;
                strcpy(power_desc[num],"Crystals(Put your pets into crystals.)          100 points");powers[num++]=63;                
        }
        if(p_ptr->pclass == CLASS_ZELAR)
        {
                strcpy(power_desc[num],"Zelar Art(Fight like a Zelar.)                  12000 points");powers[num++]=64;
                strcpy(power_desc[num],"Wave Kick(Use 'Wave Kick' command.)             5000 points");powers[num++]=65;
                strcpy(power_desc[num],"Energy Spin(Use 'Energy Spin' command.)         2000 points");powers[num++]=66;
                strcpy(power_desc[num],"Circle of Force(Use 'Circle of Force' command.) 6500 points");powers[num++]=67;
                strcpy(power_desc[num],"Hp 100%(Increase hp by 100%.)                   10000 points");powers[num++]=68;
                strcpy(power_desc[num],"50% Block(50% chances to nulify physical dam.)  8000 points");powers[num++]=82;
                
        }
        if(p_ptr->pclass == CLASS_SKATTER)
        {
                strcpy(power_desc[num],"Skatter Arrows(Make skatter arrows.)            5000 points");powers[num++]=69;
                strcpy(power_desc[num],"Exploding Arrow(Use 'Exploding Arrow' command.) 2000 points");powers[num++]=70;
                strcpy(power_desc[num],"Enchant Bow(Bow become magic.)                  10000 points");powers[num++]=71;
                
        }
        if(p_ptr->pclass == CLASS_ASSASSIN)
        {
                strcpy(power_desc[num],"Assassination(Cause great damages.)             15000 points");powers[num++]=72;                
        }
        if(p_ptr->pclass == CLASS_FIRE_LORD)
        {
                strcpy(power_desc[num],"Fire Ball(Cast a powerful Fire Ball.)           1500 points");powers[num++]=73;
                strcpy(power_desc[num],"Fire Circle(Use 'Fire Circle' command.)         3000 points");powers[num++]=74;
                strcpy(power_desc[num],"Plasma Storm(Giant plasma storm.)               8000 points");powers[num++]=75;
                
        }
        if(p_ptr->pclass == CLASS_VALKYRIE)
        {
                strcpy(power_desc[num],"Valkyrie Spears(Create Valkyrie Spears.)        30000 points");powers[num++]=76;
                strcpy(power_desc[num],"Valkyrian Aura(Aura of the Valkyries.)          7000 points");powers[num++]=77;
                strcpy(power_desc[num],"Flash (Equip 'Flash' command.)                  10000 points");powers[num++]=81;
        }
        if(p_ptr->pclass == CLASS_COMMANDER)
        {
                strcpy(power_desc[num],"Control(Give commands to your pets.)            2000 points");powers[num++]=78;                
        }
        if(p_ptr->pclass == CLASS_MONSTER_MAGE)
        {
                strcpy(power_desc[num],"Learning(Learn monster magics.)                 500 points");powers[num++]=79;
                strcpy(power_desc[num],"Monster Magic(Use Monster Magics.)              35000 points");powers[num++]=80;
        }

        if(!num) {msg_print("This class dosen't have any abilities!");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Ability %c-%c, *=List, ESC=exit) Learn which ability? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Ability %c-%c, *=List, ESC=exit) Learn which ability? ",
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
                case 1:
                        if (p_ptr->ability_points >= 100)
                        {
                                p_ptr->ability_points -= 100;
                                create_ability(1);
                        }
                        else msg_print("You don't have enough ability points!");
                        break;
                case 2:
                        if (p_ptr->ability_points >= 1000)
                        {
                                p_ptr->ability_points -= 1000;
                                create_ability(2);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 3:
                        if (p_ptr->ability_points >= 2500)
                        {
                                p_ptr->ability_points -= 2500;
                                create_ability(3);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 4:
                        if (p_ptr->ability_points >= 10000)
                        {
                                p_ptr->ability_points -= 10000;
                                create_ability(4);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 5:
                        if (p_ptr->ability_points >= 150)
                        {
                                p_ptr->ability_points -= 150;
                                create_ability(5);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 6:
                        if (p_ptr->ability_points >= 600)
                        {
                                p_ptr->ability_points -= 600;
                                create_ability(6);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 7:
                        if (p_ptr->ability_points >= 1000)
                        {
                                p_ptr->ability_points -= 1000;
                                create_ability(7);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 8:
                        if (p_ptr->ability_points >= 100)
                        {
                                p_ptr->ability_points -= 100;
                                create_ability(8);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 9:
                        if (p_ptr->ability_points >= 750)
                        {
                                p_ptr->ability_points -= 750;
                                create_ability(9);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 10:
                        if (p_ptr->ability_points >= 25000)
                        {
                                p_ptr->ability_points -= 25000;
                                create_ability(10);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 11:
                        if (p_ptr->ability_points >= 300)
                        {
                                p_ptr->ability_points -= 300;
                                create_ability(11);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 12:
                        if (p_ptr->ability_points >= 300)
                        {
                                p_ptr->ability_points -= 300;
                                create_ability(12);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 13:
                        if (p_ptr->ability_points >= 2500)
                        {
                                p_ptr->ability_points -= 2500;
                                create_ability(13);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 14:
                        if (p_ptr->ability_points >= 200000)
                        {
                                p_ptr->ability_points -= 200000;
                                create_ability(14);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 15:
                        if (p_ptr->ability_points >= 800)
                        {
                                p_ptr->ability_points -= 800;
                                create_ability(15);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 16:
                        if (p_ptr->ability_points >= 800)
                        {
                                p_ptr->ability_points -= 800;
                                create_ability(16);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 17:
                        if (p_ptr->ability_points >= 300)
                        {
                                p_ptr->ability_points -= 300;
                                create_ability(17);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 18:
                        if (p_ptr->ability_points >= 1250)
                        {
                                p_ptr->ability_points -= 1250;
                                create_ability(18);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 19:
                        if (p_ptr->ability_points >= 1100)
                        {
                                p_ptr->ability_points -= 1100;
                                create_ability(19);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 20:
                        if (p_ptr->ability_points >= 75)
                        {
                                p_ptr->ability_points -= 75;
                                create_ability(20);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 21:
                        if (p_ptr->ability_points >= 200)
                        {
                                p_ptr->ability_points -= 200;
                                create_ability(21);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 22:
                        if (p_ptr->ability_points >= 350)
                        {
                                p_ptr->ability_points -= 350;
                                create_ability(22);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 23:
                        if (p_ptr->ability_points >= 1000)
                        {
                                p_ptr->ability_points -= 1000;
                                create_ability(23);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 24:
                        if (p_ptr->ability_points >= 5000)
                        {
                                p_ptr->ability_points -= 5000;
                                create_ability(24);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 25:
                        if (p_ptr->ability_points >= 1000)
                        {
                                p_ptr->ability_points -= 1000;
                                create_ability(25);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 26:
                        if (p_ptr->ability_points >= 900)
                        {
                                p_ptr->ability_points -= 900;
                                create_ability(26);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 27:
                        if (p_ptr->ability_points >= 3000)
                        {
                                p_ptr->ability_points -= 3000;
                                create_ability(27);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 28:
                        if (p_ptr->ability_points >= 1000)
                        {
                                p_ptr->ability_points -= 1000;
                                create_ability(28);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 29:
                        if (p_ptr->ability_points >= 1500)
                        {
                                p_ptr->ability_points -= 1500;
                                create_ability(29);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 30:
                        if (p_ptr->ability_points >= 1200)
                        {
                                p_ptr->ability_points -= 1200;
                                create_ability(30);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 31:
                        if (p_ptr->ability_points >= 4000)
                        {
                                p_ptr->ability_points -= 4000;
                                create_ability(31);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 32:
                        if (p_ptr->ability_points >= 800)
                        {
                                p_ptr->ability_points -= 800;
                                create_ability(32);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 33:
                        if (p_ptr->ability_points >= 1000)
                        {
                                p_ptr->ability_points -= 1000;
                                create_ability(33);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 34:
                        if (p_ptr->ability_points >= 6000)
                        {
                                p_ptr->ability_points -= 6000;
                                create_ability(34);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 35:
                        if (p_ptr->ability_points >= 1000)
                        {
                                p_ptr->ability_points -= 1000;
                                create_ability(35);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 36:
                        if (p_ptr->ability_points >= 2000)
                        {
                                p_ptr->ability_points -= 2000;
                                create_ability(36);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 37:
                        if (p_ptr->ability_points >= 1000)
                        {
                                p_ptr->ability_points -= 1000;
                                create_ability(37);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 38:
                        if (p_ptr->ability_points >= 50000)
                        {
                                p_ptr->ability_points -= 50000;
                                create_ability(38);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 39:
                        if (p_ptr->ability_points >= 3000)
                        {
                                p_ptr->ability_points -= 3000;
                                create_ability(39);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 40:
                        if (p_ptr->ability_points >= 35000)
                        {
                                p_ptr->ability_points -= 35000;
                                create_ability(40);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 41:
                        if (p_ptr->ability_points >= 4000)
                        {
                                p_ptr->ability_points -= 4000;
                                create_ability(41);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 42:
                        if (p_ptr->ability_points >= 10000)
                        {
                                p_ptr->ability_points -= 10000;
                                create_ability(42);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 43:
                        if (p_ptr->ability_points >= 7000)
                        {
                                p_ptr->ability_points -= 7000;
                                create_ability(43);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 44:
                        if (p_ptr->ability_points >= 1000)
                        {
                                p_ptr->ability_points -= 1000;
                                create_ability(44);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 45:
                        if (p_ptr->ability_points >= 1000)
                        {
                                p_ptr->ability_points -= 1000;
                                create_ability(45);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 46:
                        if (p_ptr->ability_points >= 3000)
                        {
                                p_ptr->ability_points -= 3000;
                                create_ability(46);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 47:
                        if (p_ptr->ability_points >= 100)
                        {
                                p_ptr->ability_points -= 100;
                                create_ability(47);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 48:
                        if (p_ptr->ability_points >= 2500)
                        {
                                p_ptr->ability_points -= 2500;
                                create_ability(48);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 49:
                        if (p_ptr->ability_points >= 10000)
                        {
                                p_ptr->ability_points -= 10000;
                                create_ability(49);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 50:
                        if (p_ptr->ability_points >= 15000)
                        {
                                p_ptr->ability_points -= 15000;
                                create_ability(50);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 51:
                        if (p_ptr->ability_points >= 25000)
                        {
                                p_ptr->ability_points -= 25000;
                                create_ability(51);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 52:
                        if (p_ptr->ability_points >= 1500)
                        {
                                p_ptr->ability_points -= 1500;
                                create_ability(52);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 53:
                        if (p_ptr->ability_points >= 20000)
                        {
                                p_ptr->ability_points -= 20000;
                                create_ability(53);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 54:
                        if (p_ptr->ability_points >= 12000)
                        {
                                p_ptr->ability_points -= 12000;
                                create_ability(54);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 55:
                        if (p_ptr->ability_points >= 10000)
                        {
                                p_ptr->ability_points -= 10000;
                                create_ability(55);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 56:
                        if (p_ptr->ability_points >= 10000)
                        {
                                p_ptr->ability_points -= 10000;
                                create_ability(56);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 57:
                        if (p_ptr->ability_points >= 30000)
                        {
                                p_ptr->ability_points -= 30000;
                                create_ability(57);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 58:
                        if (p_ptr->ability_points >= 100000)
                        {
                                p_ptr->ability_points -= 100000;
                                create_ability(58);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 59:
                        if (p_ptr->ability_points >= 200000)
                        {
                                p_ptr->ability_points -= 200000;
                                create_ability(59);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 60:
                        if (p_ptr->ability_points >= 1000)
                        {
                                p_ptr->ability_points -= 1000;
                                create_ability(60);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 61:
                        if (p_ptr->ability_points >= 5000)
                        {
                                p_ptr->ability_points -= 5000;
                                create_ability(61);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 62:
                        if (p_ptr->ability_points >= 5000)
                        {
                                p_ptr->ability_points -= 5000;
                                create_ability(62);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 63:
                        if (p_ptr->ability_points >= 100)
                        {
                                p_ptr->ability_points -= 100;
                                create_ability(63);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 64:
                        if (p_ptr->ability_points >= 12000)
                        {
                                p_ptr->ability_points -= 12000;
                                create_ability(64);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 65:
                        if (p_ptr->ability_points >= 5000)
                        {
                                p_ptr->ability_points -= 5000;
                                create_ability(65);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 66:
                        if (p_ptr->ability_points >= 2000)
                        {
                                p_ptr->ability_points -= 2000;
                                create_ability(66);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 67:
                        if (p_ptr->ability_points >= 6500)
                        {
                                p_ptr->ability_points -= 6500;
                                create_ability(67);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 68:
                        if (p_ptr->ability_points >= 10000)
                        {
                                p_ptr->ability_points -= 10000;
                                create_ability(68);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 69:
                        if (p_ptr->ability_points >= 5000)
                        {
                                p_ptr->ability_points -= 5000;
                                create_ability(69);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 70:
                        if (p_ptr->ability_points >= 2000)
                        {
                                p_ptr->ability_points -= 2000;
                                create_ability(70);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 71:
                        if (p_ptr->ability_points >= 10000)
                        {
                                p_ptr->ability_points -= 10000;
                                create_ability(71);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 72:
                        if (p_ptr->ability_points >= 15000)
                        {
                                p_ptr->ability_points -= 15000;
                                create_ability(72);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 73:
                        if (p_ptr->ability_points >= 1500)
                        {
                                p_ptr->ability_points -= 1500;
                                create_ability(73);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 74:
                        if (p_ptr->ability_points >= 3000)
                        {
                                p_ptr->ability_points -= 3000;
                                create_ability(74);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 75:
                        if (p_ptr->ability_points >= 8000)
                        {
                                p_ptr->ability_points -= 8000;
                                create_ability(75);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 76:
                        if (p_ptr->ability_points >= 30000)
                        {
                                p_ptr->ability_points -= 30000;
                                create_ability(76);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 77:
                        if (p_ptr->ability_points >= 7000)
                        {
                                p_ptr->ability_points -= 7000;
                                create_ability(77);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 78:
                        if (p_ptr->ability_points >= 2000)
                        {
                                p_ptr->ability_points -= 2000;
                                create_ability(78);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 79:
                        if (p_ptr->ability_points >= 500)
                        {
                                p_ptr->ability_points -= 500;
                                create_ability(79);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 80:
                        if (p_ptr->ability_points >= 35000)
                        {
                                p_ptr->ability_points -= 35000;
                                create_ability(80);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 81:
                        if (p_ptr->ability_points >= 10000)
                        {
                                p_ptr->ability_points -= 10000;
                                create_ability(81);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                case 82:
                        if (p_ptr->ability_points >= 8000)
                        {
                                p_ptr->ability_points -= 8000;
                                create_ability(82);
                        }
                        else msg_print("You don't have enough ability points!");
			break;
                
        }
        return num;
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

void create_ability_restore(int svalnum)
{
        object_type	forge;
	object_type	*q_ptr;
	

	/* Get local object */
	q_ptr = &forge;

        
        object_prep(q_ptr, lookup_kind(TV_ABILITY, svalnum));
        q_ptr->number = 1;
        object_aware(q_ptr);
        object_known(q_ptr);

        /* These objects are "storebought" */
        q_ptr->ident |= IDENT_STOREB;

        (void)inven_carry(q_ptr, TRUE);
        do_cmd_auto_wield(q_ptr);
        msg_print("You gained a new ability!");
}

/* Since ability(x) is a very useful function, here's a similar one for Rings! */
bool ring(int rsval)
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_RING];

        if (o_ptr->sval == rsval) return TRUE;
        else return FALSE;
}

