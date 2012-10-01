/* File: mongen.c */
/* Purpose: A monster generator for NewAngband! */

#include "angband.h"
#include "mongen.h"

void monster_generator()
{
	int monsterno = 1200;
        int moncount = 1;
        int racechoice = 0;
        int forcemsex, leveldun, randspeed;
        int randhp1, randhp2, randvis, randdef, randsleep;
        int monweight, blowsrand, numblows, blowscount, randdrops;
        int numresists;
        s32b randexp;
        char monname[80], monracename[80];
        char monattr;
        char flag1[30], flag2[30], flag3[30], flag4[30], flag5[30], flag6[30];
        FILE *rinfo = fopen("r_info.txt", "a");      

        /* Let write a blank space */
        fprintf(rinfo, "\n");

        while (moncount <= 1270)
	{
                int noflags = 6;
                forcemsex = 0;
                monattr = ' ';
		/* First, the first row */
		/* We need to determine the name + attr first */

                /* Let's choose a Race! */
                racechoice = 0;
                while (racechoice == 0 || racechoice == 20 || racechoice == 39)
                {
                        racechoice = randint(56);
                }
                if (racechoice == 1)
                {
                        strcpy(monracename, "Ant");
                        monattr = 'a';
                }
                else if (racechoice == 2)
                {
                        strcpy(monracename, "Bat");
                        monattr = 'b';
                }
                else if (racechoice == 3)
                {
                        int monvariant;
                        monvariant = randint(100);                        
                        if (monvariant >= 50) strcpy(monracename, "Centipede");
                        else strcpy(monracename, "Caterpillar");                        
                        monattr = 'c';
                }
                else if (racechoice == 4)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 80) strcpy(monracename, "Young Dragon");
                        else if (monvariant >= 60) strcpy(monracename, "Wyvern");
                        else if (monvariant >= 40) strcpy(monracename, "Baby Dragon");
                        else if (monvariant >= 20) strcpy(monracename, "Pseudo Dragon");
                        else strcpy(monracename, "Lesser Dragon");
                        monattr = 'd';
                }
                else if (racechoice == 5)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 75) strcpy(monracename, "Eye");
                        else if (monvariant >= 50) strcpy(monracename, "Beholder");
                        else if (monvariant >= 25) strcpy(monracename, "Watcher");
                        else strcpy(monracename, "Gazer");
                        monattr = 'e';
                }
                else if (racechoice == 6)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 80) strcpy(monracename, "Feline");
                        else if (monvariant >= 60) strcpy(monracename, "Cat");
                        else if (monvariant >= 40) strcpy(monracename, "Lion");
                        else if (monvariant >= 20) strcpy(monracename, "Panther");
                        else strcpy(monracename, "Tiger");
                        monattr = 'f';
                }
                else if (racechoice == 7)
                {
                        strcpy(monracename, "Golem");
                        monattr = 'g';
                }
                else if (racechoice == 8)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 75) strcpy(monracename, "Elf");
                        else if (monvariant >= 50) strcpy(monracename, "Dwarf");
                        else if (monvariant >= 25) strcpy(monracename, "Halfling");
                        else strcpy(monracename, "Gnome");
                        monattr = 'h';
                }
                else if (racechoice == 9)
                {
                        strcpy(monracename, "Icky Thing");
                        monattr = 'i';
                }
                else if (racechoice == 10)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 66) strcpy(monracename, "Slime");
                        else if (monvariant >= 33) strcpy(monracename, "Jelly");
                        else strcpy(monracename, "Blob");
                        monattr = 'j';
                }
                else if (racechoice == 11)
                {
                        strcpy(monracename, "Kobold");
                        monattr = 'k';
                }
                else if (racechoice == 12)
                {
                        strcpy(monracename, "Louse");
                        monattr = 'l';
                }
                else if (racechoice == 13)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 66) strcpy(monracename, "Mold");
                        else if (monvariant >= 33) strcpy(monracename, "Plant");
                        else strcpy(monracename, "Flower");
                        monattr = 'm';
                }
                else if (racechoice == 14)
                {
                        strcpy(monracename, "Naga");
                        forcemsex = 2;
                        monattr = 'n';
                }
                else if (racechoice == 15)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 66) strcpy(monracename, "Orc");
                        else if (monvariant >= 33) strcpy(monracename, "Snaga");
                        else strcpy(monracename, "Uruk");
                        monattr = 'o';
                }
                else if (racechoice == 16)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 80) strcpy(monracename, "Human");
                        else if (monvariant >= 60) strcpy(monracename, "Barbarian");
                        else if (monvariant >= 40) strcpy(monracename, "Soldier");
                        else if (monvariant >= 20) strcpy(monracename, "Mercenary");
                        else
                        {
                                strcpy(monracename, "Amazon");
                                forcemsex = 2;
                        }
                        monattr = 'p';
                }
                else if (racechoice == 17)
                {
                        strcpy(monracename, "Quadroped");
                        monattr = 'q';
                }
                else if (racechoice == 18)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 66) strcpy(monracename, "Mouse");
                        else if (monvariant >= 33) strcpy(monracename, "Rat");
                        else strcpy(monracename, "Rodent");
                        monattr = 'r';
                }
                else if (racechoice == 19)
                {
                        strcpy(monracename, "Skeleton");
                        monattr = 's';
                }
                else if (racechoice == 20)
                {
                        /* Let's not make "Townsfolk" monsters! ;) */
                }
                else if (racechoice == 21)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 66) strcpy(monracename, "Imp");
                        else if (monvariant >= 33) strcpy(monracename, "Minor Demon");
                        else strcpy(monracename, "Quasit");
                        monattr = 'u';
                }
                else if (racechoice == 22)
                {
                        strcpy(monracename, "Vortex");
                        monattr = 'v';
                }
                else if (racechoice == 23)
                {
                        strcpy(monracename, "Worm");
                        monattr = 'w';
                }
                else if (racechoice == 24)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 80) strcpy(monracename, "Octopus");
                        else if (monvariant >= 60) strcpy(monracename, "Fish");
                        else if (monvariant >= 40) strcpy(monracename, "Shark");
                        else if (monvariant >= 20) strcpy(monracename, "Eel");
                        else
                        {
                                strcpy(monracename, "Mermaid");
                                forcemsex = 2;
                        }
                        monattr = 'x';
                }
                else if (racechoice == 25)
                {
                        strcpy(monracename, "Yeek");
                        monattr = 'y';
                }
                else if (racechoice == 26)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 80) strcpy(monracename, "Zombie");
                        else if (monvariant >= 60) strcpy(monracename, "Ghoul");
                        else if (monvariant >= 40) strcpy(monracename, "Ghast");
                        else if (monvariant >= 20) strcpy(monracename, "Wight");
                        else strcpy(monracename, "Specter");
                        monattr = 'z';
                }
                else if (racechoice == 27)
                {
                        strcpy(monracename, "Angel");
                        monattr = 'A';
                }
                else if (racechoice == 28)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 75) strcpy(monracename, "Bird");
                        else if (monvariant >= 50) strcpy(monracename, "Eagle");
                        else if (monvariant >= 25) strcpy(monracename, "Raven");
                        else strcpy(monracename, "Roc");
                        monattr = 'B';
                }
                else if (racechoice == 29)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 66) strcpy(monracename, "Canine");
                        else if (monvariant >= 33) strcpy(monracename, "Dog");
                        else strcpy(monracename, "Wolf");
                        monattr = 'C';
                }
                else if (racechoice == 30)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 50) strcpy(monracename, "Dragon");
                        else strcpy(monracename, "Wyrm");
                        monattr = 'D';
                }
                else if (racechoice == 31)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 50) strcpy(monracename, "Spirit");
                        else strcpy(monracename, "Elemental");
                        monattr = 'E';
                }
                else if (racechoice == 32)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 50) strcpy(monracename, "Dragon Fly");
                        else strcpy(monracename, "Lizard Fly");
                        monattr = 'F';
                }
                else if (racechoice == 33)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 50) strcpy(monracename, "Ghost");
                        else strcpy(monracename, "Phantom");
                        monattr = 'G';
                }
                else if (racechoice == 34)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 75) strcpy(monracename, "Griffon");
                        else if (monvariant >= 50) strcpy(monracename, "Hippogriff");
                        else if (monvariant >= 25) strcpy(monracename, "Minotaur");
                        else strcpy(monracename, "Chimera");
                        monattr = 'H';
                }
                else if (racechoice == 35)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 80) strcpy(monracename, "Insect");
                        else if (monvariant >= 60) strcpy(monracename, "Cockroach");
                        else if (monvariant >= 40) strcpy(monracename, "Wasp");
                        else if (monvariant >= 20) strcpy(monracename, "Insect Swarm");
                        else strcpy(monracename, "Scorpion");
                        monattr = 'I';
                }
                else if (racechoice == 36)
                {
                        strcpy(monracename, "Beetle");
                        monattr = 'K';
                }
                else if (racechoice == 37)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 50) strcpy(monracename, "Lich");
                        else strcpy(monracename, "Demilich");
                        monattr = 'L';
                }
                else if (racechoice == 38)
                {
                        strcpy(monracename, "Hydra");
                        monattr = 'M';
                }
                else if (racechoice == 39)
                {
                        /* Blank. N is unused... */
                }
                else if (racechoice == 40)
                {
                        strcpy(monracename, "Ogre");
                        monattr = 'O';
                }
                else if (racechoice == 41)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 50) strcpy(monracename, "Giant");
                        else strcpy(monracename, "Titan");
                        monattr = 'P';
                }
                else if (racechoice == 42)
                {
                        strcpy(monracename, "Quylthulg");
                        monattr = 'Q';
                }
                else if (racechoice == 43)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 50) strcpy(monracename, "Lizard");
                        else strcpy(monracename, "Reptile");
                        monattr = 'R';
                }
                else if (racechoice == 44)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 66) strcpy(monracename, "Spider");
                        else if (monvariant >= 33) strcpy(monracename, "Aranea");
                        else strcpy(monracename, "Arachnoid");
                        monattr = 'S';
                }
                else if (racechoice == 45)
                {
                        strcpy(monracename, "Troll");
                        monattr = 'T';
                }
                else if (racechoice == 46)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 75) strcpy(monracename, "Balrog");
                        else if (monvariant >= 50) strcpy(monracename, "Demon");
                        else if (monvariant >= 25) strcpy(monracename, "Fiend");
                        else
                        {
                                strcpy(monracename, "Succubus");
                                forcemsex = 2;
                        }
                        monattr = 'U';
                }
                else if (racechoice == 47)
                {
                        strcpy(monracename, "Vampire");
                        monattr = 'V';
                }
                else if (racechoice == 48)
                {
                        strcpy(monracename, "Wraith");
                        monattr = 'W';
                }
                else if (racechoice == 49)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 66) strcpy(monracename, "Xorn");
                        else if (monvariant >= 33) strcpy(monracename, "Xaren");
                        else strcpy(monracename, "Umber Hulk");
                        monattr = 'X';
                }
                else if (racechoice == 50)
                {
                        strcpy(monracename, "Yeti");
                        monattr = 'Y';
                }
                else if (racechoice == 51)
                {
                        strcpy(monracename, "Hound");
                        monattr = 'Z';
                }
                else if (racechoice == 52)
                {
                        int monvariant;
                        monvariant = randint(100);
                        if (monvariant >= 50) strcpy(monracename, "Mushrooms");
                        else strcpy(monracename, "Fungus");
                        monattr = ',';
                }
                else if (racechoice == 53)
                {
                        strcpy(monracename, "Creeping Coins");
                        monattr = '$';
                }
                else if (racechoice == 54)
                {
                        strcpy(monracename, "Devling");
                        monattr = '&';
                }
                else if (racechoice == 55)
                {
                        strcpy(monracename, "Sphere");
                        monattr = '*';
                }
                else if (racechoice == 56)
                {
                        strcpy(monracename, "Snake");
                        monattr = 'J';
                }

                strcpy(monname, complete_mon_name(monracename));
                fprintf(rinfo, "N:%d:%s\n", monsterno, monname);
                fprintf(rinfo, "G:%c:%c\n", monattr, get_attr_color());

                /* Now, the monsters Speed, Hp, Vision, Defense and Vigilence */
                leveldun = (moncount / 10) + 1;
                if (leveldun > 127) leveldun = 127;

                /* First determine a basic speed */
                /* Can vary between 101(slow/average) and 130(fast) */
                randspeed = randint(30) + 100;

                /* Sometimes gives a good speed boost! */
                /* With boost, varies between 121(fast) and 150(very fast) */
                if (randint(100) >= 80) randspeed += 20;

                /* We're done with speed, now hp */
                /* Minimum is 1d1. More hp with higer depth */
                randhp1 = randint(leveldun) + (leveldun / 2) + 1;
                randhp2 = randint(leveldun) + (leveldun / 2) + 1;

                /* Vision. Varies between 16 and 30.*/
                randvis = randint(15) + 15;

                /* Defense now... Hmmm... How are we going do do? */
                /* Some low level monsters does have good def... */
                /* But higher monsters should get more of course... */
                randdef = randint(leveldun) + randint(50);

                /* Sleep...Let's not make it over 10 */
                randsleep = randint(10) - (leveldun / 10);
                if (randsleep < 0) randsleep = 0;

                /* Great! We're done with that! */
                fprintf(rinfo, "I:%d:%dd%d:%d:%d:%d\n", randspeed, randhp1, randhp2, randvis, randdef, randsleep);

                /* Next is the wilderness line... */
                /* Depth, rarity(always 1), weight(some races gets more), experience */
                if (monattr == 'D') monweight = randint(15000) + 15000;
                else if (monattr == 'P') monweight = randint(1000) + 1500;
                else if (monattr == 'g') monweight = randint(1500) + 1000;
                else if (monattr == 'd' || monattr == 'M') monweight = randint(3000) + 500;
                else if (monattr == 'O' || monattr == 'T') monweight = randint(600) + 800;
                else if (monattr == 'p' || monattr == 'h' || monattr == 'o' || monattr == 'z' || monattr == 's' || monattr == 'L' || monattr == 'V') monweight = randint(400) + 500;
                else if (monattr == 'l' || monattr == 'I' || monattr == 'S' || monattr == 'K' || monattr == 'k') monweight = randint(500) + 150;
                else if (monattr == 'R') monweight = randint(1000) + 500;
                else monweight = randint(500) + 700;

                /* The experience is determined randomly! */
                randexp = randint((leveldun * (2 + ((leveldun / 5) * ((leveldun / 10) + 1))))) + (leveldun * leveldun) + 1;

                /* Yay! We're done! :) */
                fprintf(rinfo, "W:%d:1:%d:%ld\n", leveldun, monweight, randexp);

                /* Now, the equip line. Again, determined by the race */
                if (monattr == 'D' || monattr == 'd') fprintf(rinfo, "E:0:1:0:6:1:0\n");
                else if (monattr == 'p' || monattr == 'h' || monattr == 'o' || monattr == 'O' || monattr == 'T' || monattr == 'k' || monattr == 'z' || monattr == 's' || monattr == 'L' || monattr == 'g' || monattr == 'u' || monattr == 'U' || monattr == 'V') fprintf(rinfo, "E:1:1:1:2:1:1\n");
                else if (monattr == 'C' || monattr == 'f' || monattr == 'Z' || monattr == 'R' || monattr == 'q') fprintf(rinfo, "E:0:1:0:2:1:0\n");
                else fprintf(rinfo, "E:0:0:0:0:0:0\n");

                /* Now, we're going to the hits lines! */
                /* The greater depth, the higher chance of four blows! */
                blowscount = 0;
                blowsrand = randint(leveldun);
                if (blowsrand >= 60) numblows = 4;
                else if (blowsrand >= 35) numblows = 3;
                else if (blowsrand >= 10) numblows = 2;
                else numblows = 1;

                while (blowscount < numblows)
                {
                        int rdd, rds;
                        rdd = randint(leveldun / 3) + (leveldun / 10) + 1;
                        rds = randint(leveldun / 3) + (leveldun / 10) + 1;
                        fprintf(rinfo, "B:%s:%s:%dd%d\n", blows_random_method(monattr), blows_random_effect(), rdd, rds);
                        blowscount += 1;
                }

                /* We're done with the blows! Now, the numerous flags... */
                /* Let's begin with the basic flags... */
                strcpy(flag1, "");
                strcpy(flag2, "");
                strcpy(flag3, "");
                strcpy(flag4, "");
                strcpy(flag5, "");
                strcpy(flag6, "");
                /* Some monsters can open doors */
                if (monattr == 'p' || monattr == 'P' || monattr == 'h' || monattr == 'o' || monattr == 'T' || monattr == 'O' || monattr == 'u' || monattr == 'U' || monattr == 'g' || monattr == 'z' || monattr == 's' || monattr == 'L' || monattr == 'W') strcpy(flag1, "OPEN_DOOR | "); 

                /* All monsters can bash doors. */
                strcpy(flag2, "BASH_DOOR | ");

                /* Determine the sex of the monster */
                if (forcemsex == 1) strcpy(flag3, "MALE | ");
                else if (forcemsex == 2) strcpy(flag3, "FEMALE | ");
                else
                {
                        int choosesex;
                        if (monattr == 'p' || monattr == 'h' || monattr == 'V')
                        {
                                choosesex = randint(100);
                                if (choosesex >= 50) strcpy(flag3, "MALE | ");
                                else strcpy(flag3, "FEMALE | ");
                        }
                }

                /* Other stuff...hmmm...force max hp? */
                if (randint(100) >= 75) strcpy(flag4, "FORCE_MAXHP | ");

                /* Force sleep? Let's try not to make it too common... */
                if (randint(100) >= 92) strcpy(flag5, "FORCE_SLEEP | ");

                /* Finally...particularly evil? */
                if ((randint(100) >= 25) || monattr == 'u' || monattr == 'U') strcpy(flag6, "EVIL | ");

                /* First flags row done, write it! */
                fprintf(rinfo, "F:%s%s%s%s%s%s\n", flag1, flag2, flag3, flag4, flag5, flag6);

                /* Clear the flags */
                strcpy(flag1, "");
                strcpy(flag2, "");
                strcpy(flag3, "");
                strcpy(flag4, "");
                strcpy(flag5, "");
                strcpy(flag6, "");

                /* Next, appear in groups, corpse dropping, and drops */
                if (randint(100) >= 75) strcpy(flag1, "FRIENDS | ");
                if (randint(100) >= 10 && monattr != 'G' && monattr != 'E') strcpy(flag2, "DROP_CORPSE | ");
                if (randint(100) >= 50 && monattr != 'G' && monattr != 'E') strcpy(flag3, "DROP_SKELETON | ");
                /* Item drops */
                randdrops = randint(100);
                if (randdrops >= 33)
                {
                        int droptypes, dropquality;
                        droptypes = randint(100);
                        if (droptypes >= 50) strcpy(flag4, "DROP_60 | ");
                        else if (droptypes >= 25) strcpy(flag4, "DROP_90 | ");
                        else if (droptypes >= 15) strcpy(flag4, "DROP_1D2 | ");
                        else if (droptypes >= 8) strcpy(flag4, "DROP_2D2 | ");
                        else if (droptypes >= 2) strcpy(flag4, "DROP_3D2 | ");
                        else strcpy(flag4, "DROP_4D2 | ");

                        dropquality = randint(100);
                        if (dropquality >= 95) strcpy(flag5, "DROP_GOOD | DROP_GREAT | "); 
                        else if (dropquality >= 80) strcpy(flag5, "DROP_GOOD | ");
                }
                /* All right, we're done with this one. Let's write it! */
                fprintf(rinfo, "F:%s%s%s%s%s\n", flag1, flag2, flag3, flag4, flag5);

                /* Clear the flags */
                strcpy(flag1, "");
                strcpy(flag2, "");
                strcpy(flag3, "");
                strcpy(flag4, "");
                strcpy(flag5, "");
                strcpy(flag6, "");

                /* Third row, the basic resistances flags! */
                /* Up to 6 resistances, maybe none */
                if (randint(100) >= 25)
                {
                        int loopbreak;
                        numresists = randint(6);
                        strcpy(flag1, get_basic_mon_resist());
                        if (numresists >= 2)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag2, get_basic_mon_resist());
                                        if (!strstr(flag2, flag1)) loopbreak = 1;
                                }
                        }
                        if (numresists >= 3)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag3, get_basic_mon_resist());
                                        if (!strstr(flag3, flag2)) loopbreak = 1;
                                }
                        }
                        if (numresists >= 4)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag4, get_basic_mon_resist());
                                        if (!strstr(flag4, flag3)) loopbreak = 1;
                                }
                        }
                        if (numresists >= 5)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag5, get_basic_mon_resist());
                                        if (!strstr(flag5, flag4)) loopbreak = 1;
                                }
                        }
                        if (numresists >= 6)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag6, get_basic_mon_resist());
                                        if (!strstr(flag6, flag5)) loopbreak = 1;
                                }
                        }
                        /* All done */
                        fprintf(rinfo, "F:%s%s%s%s%s%s\n", flag1, flag2, flag3, flag4, flag5, flag6);
                }


                /* Clear the flags */
                strcpy(flag1, "");
                strcpy(flag2, "");
                strcpy(flag3, "");
                strcpy(flag4, "");
                strcpy(flag5, "");
                strcpy(flag6, "");
                        
                /* Fourth row, the extra resistances flags! */
                /* Up to 4 resistances, maybe none */
                if (randint(100) >= 50)
                {
                        int loopbreak;
                        numresists = randint(4);
                        strcpy(flag1, get_extra_mon_resist());
                        if (numresists >= 2)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag2, get_extra_mon_resist());
                                        if (!strstr(flag2, flag1)) loopbreak = 1;
                                }
                        }
                        if (numresists >= 3)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag3, get_extra_mon_resist());
                                        if (!strstr(flag3, flag2)) loopbreak = 1;
                                }
                        }
                        if (numresists >= 4)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag4, get_extra_mon_resist());
                                        if (!strstr(flag4, flag3)) loopbreak = 1;
                                }
                        }
                        /* All done */
                        fprintf(rinfo, "F:%s%s%s%s\n", flag1, flag2, flag3, flag4);
                }

                /* Clear the flags */
                strcpy(flag1, "");
                strcpy(flag2, "");
                strcpy(flag3, "");
                strcpy(flag4, "");
                strcpy(flag5, "");
                strcpy(flag6, "");

                /* Fifth row, extra flags such as race, cold blood, etc... */
                if (monattr == 'o') strcpy(flag1, "ORC | ");
                else if (monattr == 'T') strcpy(flag1, "TROLL | ");
                else if (monattr == 'P') strcpy(flag1, "GIANT | ");
                else if (monattr == 'd' || monattr == 'D') strcpy(flag1, "DRAGON | ");
                else if (monattr == 'z' || monattr == 's' || monattr == 'L' || monattr == 'W' || monattr == 'G' || monattr == 'V') strcpy(flag1, "UNDEAD | ");
                else if (monattr == 'u' || monattr == 'U') strcpy(flag1, "DEMON | ");
                else if (monattr == 'C' || monattr == 'f' || monattr == 'Z' || monattr == 'R' || monattr == 'b' || monattr == 'B' || monattr == 'q') strcpy(flag1, "ANIMAL | ");
                else if (monattr == 'x' || monattr == '~') strcpy(flag1, "AQUATIC | ");
                else noflags -= 1;

                /* Cold blooded... */
                if (monattr == 'z' || monattr == 's' || monattr == 'L' || monattr == 'W' || monattr == 'G' || monattr == 'E' || monattr == 'g' || monattr == '*' || monattr == '$') strcpy(flag2, "COLD_BLOOD | ");
                else noflags -= 1;

                /* Non living(never lived before) */
                if (monattr == 'g' || monattr == '*' || monattr == '$') strcpy(flag3, "NONLIVING | ");
                else noflags -= 1;

                /* Never move(mushrooms, sometimes eyes) */
                if (monattr == ',') strcpy(flag4, "NEVER_MOVE | ");
                else if (randint(100) >= 50 && (monattr == 'e' || monattr == 'm')) strcpy(flag4, "NEVER_MOVE | ");
                else noflags -= 1;

                /* Empty mind */
                if (monattr == 'g' || monattr == '*' || monattr == '$') strcpy(flag5, "EMPTY_MIND | ");
                else noflags -= 1;

                /* Smart? */
                if (randint(100) >= 75) strcpy(flag6, "SMART | ");
                else noflags -= 1;

                /* Must have something to write... */
                if (noflags != 0) fprintf(rinfo, "F:%s%s%s%s%s%s\n", flag1, flag2, flag3, flag4, flag5, flag6);

                noflags = 6;

                /* Clear the flags */
                strcpy(flag1, "");
                strcpy(flag2, "");
                strcpy(flag3, "");
                strcpy(flag4, "");
                strcpy(flag5, "");
                strcpy(flag6, "");

                /* Sixth row... Well, what's left? A few things... */
                if (monattr == 'e' || monattr == 'B' || monattr == 'b' || monattr == 'F' || monattr == 'd' || monattr == 'D' || monattr == 'U') strcpy(flag1, "CAN_FLY | ");

                /* Mortal? */
                if (randint(100) >= 25) strcpy(flag2, "MORTAL | ");

                /* BaseAngband monster! */
                strcpy(flag3, "BASEANGBAND");

                /* And that's it, we're done with all flags! */
                fprintf(rinfo, "F:%s%s%s\n", flag1, flag2, flag3);

                /* Clear the flags */
                strcpy(flag1, "");
                strcpy(flag2, "");
                strcpy(flag3, "");
                strcpy(flag4, "");
                strcpy(flag5, "");
                strcpy(flag6, "");

                /* And now for spells...Same code as resistances(almost) */

                if (randint(100) >= 50)
                {
                        int loopbreak, numspells, numcasts;
                        numcasts = randint(10);
                        numspells = randint(6);

                        /* First, the casting speed */
                        fprintf(rinfo, "S:1_IN_%d\n", numcasts);

                        /* Lower depth, weaker spells */
                        if (leveldun <= 20)
                        {
                        strcpy(flag1, get_basic_mon_spell());
                        if (numspells >= 2)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag2, get_basic_mon_spell());
                                        if (!strstr(flag2, flag1)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 3)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag3, get_basic_mon_spell());
                                        if (!strstr(flag3, flag2)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 4)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag4, get_basic_mon_spell());
                                        if (!strstr(flag4, flag3)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 5)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag5, get_basic_mon_spell());
                                        if (!strstr(flag5, flag4)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 6)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag6, get_basic_mon_spell());
                                        if (!strstr(flag6, flag5)) loopbreak = 1;
                                }
                        }
                        /* All done */
                        fprintf(rinfo, "S:%s%s%s%s%s%s\n", flag1, flag2, flag3, flag4, flag5, flag6);
                        }
                        /* Mid depth, medium spells */
                        else if (leveldun <= 40)
                        {
                        strcpy(flag1, get_medium_mon_spell());
                        if (numspells >= 2)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag2, get_medium_mon_spell());
                                        if (!strstr(flag2, flag1)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 3)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag3, get_medium_mon_spell());
                                        if (!strstr(flag3, flag2)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 4)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag4, get_medium_mon_spell());
                                        if (!strstr(flag4, flag3)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 5)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag5, get_medium_mon_spell());
                                        if (!strstr(flag5, flag4)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 6)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag6, get_medium_mon_spell());
                                        if (!strstr(flag6, flag5)) loopbreak = 1;
                                }
                        }
                        /* All done */
                        fprintf(rinfo, "S:%s%s%s%s%s%s\n", flag1, flag2, flag3, flag4, flag5, flag6);
                        }
                        /* Greatest depth, best spells */
                        else
                        {
                        strcpy(flag1, get_major_mon_spell());
                        if (numspells >= 2)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag2, get_major_mon_spell());
                                        if (!strstr(flag2, flag1)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 3)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag3, get_major_mon_spell());
                                        if (!strstr(flag3, flag2)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 4)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag4, get_major_mon_spell());
                                        if (!strstr(flag4, flag3)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 5)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag5, get_major_mon_spell());
                                        if (!strstr(flag5, flag4)) loopbreak = 1;
                                }
                        }
                        if (numspells >= 6)
                        {
                                loopbreak = 0;
                                while (loopbreak == 0)
                                {
                                        strcpy(flag6, get_major_mon_spell());
                                        if (!strstr(flag6, flag5)) loopbreak = 1;
                                }
                        }
                        /* All done */
                        fprintf(rinfo, "S:%s%s%s%s%s%s\n", flag1, flag2, flag3, flag4, flag5, flag6);
                        }
                }
                                
                /* Clear the flags */
                strcpy(flag1, "");
                strcpy(flag2, "");
                strcpy(flag3, "");
                strcpy(flag4, "");
                strcpy(flag5, "");
                strcpy(flag6, "");

                /* And the last thing... the (stupid) description! */
                fprintf(rinfo, "D:%s\n", mon_random_description(monattr));

                /* Let's make a space... */
                fprintf(rinfo, "\n");

                /* Great! We now have a fully functionnal monster! */
                /* Let's make another one, or end the process. */
                moncount += 1;
                monsterno += 1;

        }
        /* Lot of new monsters! Let's rejoice! :) */
        /* But don't forget to close the file! */
        fclose(rinfo);
}

char *complete_mon_name(char monracename[80])
{
        char adjchoice1[80], adjchoice2[80];

        /* Let's get a first choice... */
        strcpy(adjchoice1, get_random_mon_adj());

        /* Now...another one? */
        if (randint(100) >= 50)
        {
                int okay = 0;
                while (okay == 0)
                {
                        strcpy(adjchoice2, get_random_mon_adj());
                        if (!strstr(adjchoice2, adjchoice1)) okay = 1;
                }
                sprintf(finalname, "%s %s %s", adjchoice1, adjchoice2, monracename);
        }
        else sprintf(finalname, "%s %s", adjchoice1, monracename);

        /* All done */
        return (finalname);
}

char *get_random_mon_adj()
{
        int randchoice;

        randchoice = randint(100);
        if (randchoice == 1) strcpy(monadj, "Cruel");
        else if (randchoice == 2) strcpy(monadj, "Mad");
        else if (randchoice == 3) strcpy(monadj, "Deranged");
        else if (randchoice == 4) strcpy(monadj, "Violent");
        else if (randchoice == 5) strcpy(monadj, "Rebel");
        else if (randchoice == 6) strcpy(monadj, "Killer");
        else if (randchoice == 7) strcpy(monadj, "Evil");
        else if (randchoice == 8) strcpy(monadj, "Perverted");
        else if (randchoice == 9) strcpy(monadj, "Stealthy");
        else if (randchoice == 10) strcpy(monadj, "Clever");
        else if (randchoice == 11) strcpy(monadj, "Vicious");
        else if (randchoice == 12) strcpy(monadj, "Bloody");
        else if (randchoice == 13) strcpy(monadj, "Hardened");
        else if (randchoice == 14) strcpy(monadj, "Freak");
        else if (randchoice == 15) strcpy(monadj, "Wild");
        else if (randchoice == 16) strcpy(monadj, "Bloodlusted");
        else if (randchoice == 17) strcpy(monadj, "Devious");
        else if (randchoice == 18) strcpy(monadj, "Fanatic");
        else if (randchoice == 19) strcpy(monadj, "Foul");
        else if (randchoice == 20) strcpy(monadj, "Bad");
        else if (randchoice == 21) strcpy(monadj, "Big");
        else if (randchoice == 22) strcpy(monadj, "Strong");
        else if (randchoice == 23) strcpy(monadj, "Agile");
        else if (randchoice == 24) strcpy(monadj, "Smart");
        else if (randchoice == 25) strcpy(monadj, "Sturdy");
        else if (randchoice == 26) strcpy(monadj, "Mystic");
        else if (randchoice == 27) strcpy(monadj, "Corrupted");
        else if (randchoice == 28) strcpy(monadj, "Lazy");
        else if (randchoice == 29) strcpy(monadj, "Vigilent");
        else if (randchoice == 30) strcpy(monadj, "Powerful");
        else if (randchoice == 31) strcpy(monadj, "Mean");
        else if (randchoice == 32) strcpy(monadj, "Dominant");
        else if (randchoice == 33) strcpy(monadj, "Scary");
        else if (randchoice == 34) strcpy(monadj, "Spooky");
        else if (randchoice == 35) strcpy(monadj, "Haunted");
        else if (randchoice == 36) strcpy(monadj, "Possessed");
        else if (randchoice == 37) strcpy(monadj, "Disgusting");
        else if (randchoice == 38) strcpy(monadj, "Brave");
        else if (randchoice == 39) strcpy(monadj, "Daring");
        else if (randchoice == 40) strcpy(monadj, "Grim");
        else if (randchoice == 41) strcpy(monadj, "Red");
        else if (randchoice == 42) strcpy(monadj, "Yellow");
        else if (randchoice == 43) strcpy(monadj, "Orange");
        else if (randchoice == 44) strcpy(monadj, "Green");
        else if (randchoice == 45) strcpy(monadj, "Blue");
        else if (randchoice == 46) strcpy(monadj, "Purple");
        else if (randchoice == 47) strcpy(monadj, "Black");
        else if (randchoice == 48) strcpy(monadj, "Dark");
        else if (randchoice == 49) strcpy(monadj, "White");
        else if (randchoice == 50) strcpy(monadj, "Pink");
        else if (randchoice == 51) strcpy(monadj, "Slayer");
        else if (randchoice == 52) strcpy(monadj, "Hunter");
        else if (randchoice == 53) strcpy(monadj, "Warrior");
        else if (randchoice == 54) strcpy(monadj, "Heavy");
        else if (randchoice == 55) strcpy(monadj, "Light");
        else if (randchoice == 56) strcpy(monadj, "Pale");
        else if (randchoice == 57) strcpy(monadj, "Enraged");
        else if (randchoice == 58) strcpy(monadj, "Berserker");
        else if (randchoice == 59) strcpy(monadj, "Violet");
        else if (randchoice == 60) strcpy(monadj, "Umber");
        else if (randchoice == 61) strcpy(monadj, "Creepy");
        else if (randchoice == 62) strcpy(monadj, "Mysterious");
        else if (randchoice == 63) strcpy(monadj, "Grey");
        else if (randchoice == 64) strcpy(monadj, "Brown");
        else if (randchoice == 65) strcpy(monadj, "Slimy");
        else if (randchoice == 66) strcpy(monadj, "Disfigured");
        else if (randchoice == 67) strcpy(monadj, "Greedy");
        else if (randchoice == 68) strcpy(monadj, "Shiny");
        else if (randchoice == 69) strcpy(monadj, "Spiky");
        else if (randchoice == 70) strcpy(monadj, "Ugly");
        else if (randchoice == 71) strcpy(monadj, "Aggressive");
        else if (randchoice == 72) strcpy(monadj, "Dire");
        else if (randchoice == 73) strcpy(monadj, "Spotted");
        else if (randchoice == 74) strcpy(monadj, "Goon");
        else if (randchoice == 75) strcpy(monadj, "Gold");
        else if (randchoice == 76) strcpy(monadj, "Silver");
        else if (randchoice == 77) strcpy(monadj, "Bronze");
        else if (randchoice == 78) strcpy(monadj, "Metallic");
        else if (randchoice == 79) strcpy(monadj, "Fearless");
        else if (randchoice == 80) strcpy(monadj, "Reckless");
        else if (randchoice == 81) strcpy(monadj, "Frantic");
        else if (randchoice == 82) strcpy(monadj, "Confident");
        else if (randchoice == 83) strcpy(monadj, "Desperate");
        else if (randchoice == 84) strcpy(monadj, "Murderous");
        else if (randchoice == 85) strcpy(monadj, "Angry");
        else if (randchoice == 86) strcpy(monadj, "Ravenous");
        else if (randchoice == 87) strcpy(monadj, "Ferocious");
        else if (randchoice == 88) strcpy(monadj, "Sneaky");
        else if (randchoice == 89) strcpy(monadj, "Lunatic");
        else if (randchoice == 90) strcpy(monadj, "Solid");
        else if (randchoice == 91) strcpy(monadj, "Fierce");
        else if (randchoice == 92) strcpy(monadj, "Frightening");
        else if (randchoice == 93) strcpy(monadj, "Feral");
        else if (randchoice == 94) strcpy(monadj, "Blood-covered");
        else if (randchoice == 95) strcpy(monadj, "Diabolic");
        else if (randchoice == 96) strcpy(monadj, "Terrible");
        else if (randchoice == 97) strcpy(monadj, "Unholy");
        else if (randchoice == 98) strcpy(monadj, "Odd");
        else if (randchoice == 99) strcpy(monadj, "Strange");
        else if (randchoice == 100) strcpy(monadj, "Ancient");


        return (monadj);
}

char get_attr_color()
{
        int randattr;
        char chosenattr;

        randattr = randint(110);
        if (randattr >= 100) chosenattr = 'G';
        else if (randattr >= 90) chosenattr = 'B';
        else if (randattr >= 80) chosenattr = 'w';
        else if (randattr >= 70) chosenattr = 'y';
        else if (randattr >= 60) chosenattr = 'r';
        else if (randattr >= 50) chosenattr = 'g';
        else if (randattr >= 40) chosenattr = 'D';
        else if (randattr >= 30) chosenattr = 's';
        else if (randattr >= 20) chosenattr = 'o';
        else if (randattr >= 10) chosenattr = 'b';
        else chosenattr = 'u';

        return (chosenattr);
}

char *blows_random_method(char monattr)
{
        int randmeth;

        if (monattr == 'e') strcpy(meth, "GAZE");
        else if (monattr == 'l' || monattr == 'I') strcpy(meth, "STING");
        else if (monattr == 'E')
        {
                randmeth = randint(100);
                if (randmeth >= 50) strcpy(meth, "ENGULF");
                else strcpy(meth, "HIT");
        }
        else if (monattr == 'F')
        {
                randmeth = randint(100);
                if (randmeth >= 50) strcpy(meth, "STING");
                else strcpy(meth, "HIT");
        }
        else if (monattr == 'D' || monattr == 'd' || monattr == 'C' || monattr == 'f' || monattr == 'Z' || monattr == 'q' || monattr == 'R' || monattr == 'u' || monattr == 'U')
        {
                randmeth = randint(100);
                if (randmeth >= 75) strcpy(meth, "CLAW");
                else if (randmeth >= 50) strcpy(meth, "HIT");
                else if (randmeth >= 25) strcpy(meth, "BITE");
                else strcpy(meth, "CLAW");
        }
        else if (monattr == 'z' || monattr == 'L' || monattr == 'W')
        {
                randmeth = randint(100);
                if (randmeth >= 50) strcpy(meth, "TOUCH");
                else strcpy(meth, "HIT");
        }
        else if (monattr == 'g')
        {
                randmeth = randint(100);
                if (randmeth >= 50) strcpy(meth, "CRUSH");
                else strcpy(meth, "HIT");
        }

        else strcpy(meth, "HIT");

        return (meth);
}


char *blows_random_effect()
{
        int randmeth;

        randmeth = randint(40);

        if (randmeth >= 18) strcpy(bloeff, "HURT");
        else if (randmeth >= 17) strcpy(bloeff, "FIRE");
        else if (randmeth >= 16) strcpy(bloeff, "COLD");
        else if (randmeth >= 15) strcpy(bloeff, "ELEC");
        else if (randmeth >= 14) strcpy(bloeff, "POISON");
        else if (randmeth >= 13) strcpy(bloeff, "LOSE_STR");
        else if (randmeth >= 12) strcpy(bloeff, "LOSE_DEX");
        else if (randmeth >= 11) strcpy(bloeff, "LOSE_INT");
        else if (randmeth >= 10) strcpy(bloeff, "LOSE_WIS");
        else if (randmeth >= 9) strcpy(bloeff, "LOSE_CON");
        else if (randmeth >= 8) strcpy(bloeff, "LOSE_CHR");
        else if (randmeth >= 7) strcpy(bloeff, "LOSE_ALL");
        else if (randmeth >= 6) strcpy(bloeff, "PARALYZE");
        else if (randmeth >= 5) strcpy(bloeff, "UN_POWER");
        else if (randmeth >= 4) strcpy(bloeff, "UN_BONUS");
        else if (randmeth >= 3) strcpy(bloeff, "ACID");
        else if (randmeth >= 2) strcpy(bloeff, "CONFUSE");
        else strcpy(bloeff, "TERRIFY");

        return (bloeff);
}

char *get_basic_mon_resist()
{
        int randmeth;

        randmeth = randint(18);

        if (randmeth == 18) strcpy(meth, "RES_NUKE | ");
        else if (randmeth == 17) strcpy(meth, "RES_DARK | ");
        else if (randmeth == 16) strcpy(meth, "RES_LITE | ");
        else if (randmeth == 15) strcpy(meth, "RES_CHAOS | ");
        else if (randmeth == 14) strcpy(meth, "RES_GRAV | ");
        else if (randmeth == 13) strcpy(meth, "RES_FORCE | ");
        else if (randmeth == 12) strcpy(meth, "RES_WIND | ");
        else if (randmeth == 11) strcpy(meth, "IM_FIRE | ");
        else if (randmeth == 10) strcpy(meth, "IM_COLD | ");
        else if (randmeth == 9) strcpy(meth, "IM_ELEC | ");
        else if (randmeth == 8) strcpy(meth, "IM_ACID | ");
        else if (randmeth == 7) strcpy(meth, "IM_POIS | ");
        else if (randmeth == 6) strcpy(meth, "RES_NETH | ");
        else if (randmeth == 5) strcpy(meth, "RES_NEXU | ");
        else if (randmeth == 4) strcpy(meth, "RES_WATE | ");
        else if (randmeth == 3) strcpy(meth, "RES_TELE | ");
        else if (randmeth == 2) strcpy(meth, "RES_PLAS | ");
        else strcpy(meth, "RES_DISE | ");

        return (meth);
}

char *get_extra_mon_resist()
{
        int randmeth;

        randmeth = randint(4);

        if (randmeth == 4) strcpy(meth, "NO_SLEEP | ");
        else if (randmeth == 3) strcpy(meth, "NO_CONF | ");
        else if (randmeth == 2) strcpy(meth, "NO_STUN | ");
        else strcpy(meth, "NO_FEAR | ");

        return (meth);
}

/* Minor spells */
char *get_basic_mon_spell()
{
        int randmeth;

        randmeth = randint(12);

        if (randmeth == 12) strcpy(meth, "BLIND | ");
        else if (randmeth == 11) strcpy(meth, "MISSILE | ");
        else if (randmeth == 10) strcpy(meth, "CAUSE_1 | ");
        else if (randmeth == 9) strcpy(meth, "CONF | ");
        else if (randmeth == 8) strcpy(meth, "BLINK | ");
        else if (randmeth == 7) strcpy(meth, "SCARE | ");
        else if (randmeth == 6) strcpy(meth, "BO_FIRE | ");
        else if (randmeth == 5) strcpy(meth, "BO_COLD | ");
        else if (randmeth == 4) strcpy(meth, "BO_ELEC | ");
        else if (randmeth == 3) strcpy(meth, "BO_ACID | ");
        else if (randmeth == 2) strcpy(meth, "HASTE | ");
        else strcpy(meth, "HEAL | ");

        return (meth);
}

/* Medium spells */
char *get_medium_mon_spell()
{
        int randmeth;

        randmeth = randint(56);

        if (randmeth == 56) strcpy(meth, "BA_DARK | ");
        else if (randmeth == 55) strcpy(meth, "BO_ICEE | ");
        else if (randmeth == 54) strcpy(meth, "BO_NETH | ");
        else if (randmeth == 53) strcpy(meth, "BO_MANA | ");
        else if (randmeth == 52) strcpy(meth, "BO_WATE | ");
        else if (randmeth == 51) strcpy(meth, "BO_POIS | ");
        else if (randmeth == 50) strcpy(meth, "DARKNESS | ");
        else if (randmeth == 49) strcpy(meth, "ARROW_4 | ");
        else if (randmeth == 48) strcpy(meth, "ARROW_3 | ");
        else if (randmeth == 47) strcpy(meth, "ARROW_2 | ");
        else if (randmeth == 46) strcpy(meth, "ARROW_1 | ");
        else if (randmeth == 45) strcpy(meth, "CAUSE_4 | ");
        else if (randmeth == 44) strcpy(meth, "CAUSE_3 | ");
        else if (randmeth == 43) strcpy(meth, "CAUSE_2 | ");
        else if (randmeth == 42) strcpy(meth, "ROCKET | ");
        else if (randmeth == 41) strcpy(meth, "BLIND | ");
        else if (randmeth == 40) strcpy(meth, "BA_FIRE | ");
        else if (randmeth == 39) strcpy(meth, "BA_COLD | ");
        else if (randmeth == 38) strcpy(meth, "BA_ACID | ");
        else if (randmeth == 37) strcpy(meth, "BA_ELEC | ");
        else if (randmeth == 36) strcpy(meth, "BA_POIS | ");
        else if (randmeth == 35) strcpy(meth, "BA_NUKE | ");
        else if (randmeth == 34) strcpy(meth, "BA_CHAO | ");
        else if (randmeth == 33) strcpy(meth, "BA_WATE | ");
        else if (randmeth == 32) strcpy(meth, "BA_NETH | ");
        else if (randmeth == 31) strcpy(meth, "BA_MANA | ");
        else if (randmeth == 30) strcpy(meth, "BR_FIRE | ");
        else if (randmeth == 29) strcpy(meth, "BR_COLD | ");
        else if (randmeth == 28) strcpy(meth, "BR_ACID | ");
        else if (randmeth == 27) strcpy(meth, "BR_ELEC | ");
        else if (randmeth == 26) strcpy(meth, "BR_POIS | ");
        else if (randmeth == 25) strcpy(meth, "BR_NUKE | ");
        else if (randmeth == 24) strcpy(meth, "BR_CHAO | ");
        else if (randmeth == 23) strcpy(meth, "BR_SOUN | ");
        else if (randmeth == 22) strcpy(meth, "BR_CONF | ");
        else if (randmeth == 21) strcpy(meth, "BR_DISE | ");
        else if (randmeth == 20) strcpy(meth, "BR_NEXU | ");
        else if (randmeth == 19) strcpy(meth, "BR_PLAS | ");
        else if (randmeth == 18) strcpy(meth, "BR_GRAV | ");
        else if (randmeth == 17) strcpy(meth, "BR_DISI | ");
        else if (randmeth == 16) strcpy(meth, "BR_WALL | ");
        else if (randmeth == 15) strcpy(meth, "BR_INER | ");
        else if (randmeth == 14) strcpy(meth, "BR_LITE | ");
        else if (randmeth == 13) strcpy(meth, "BR_DARK | ");
        else if (randmeth == 12) strcpy(meth, "BR_MANA | ");
        else if (randmeth == 11) strcpy(meth, "MISSILE | ");
        else if (randmeth == 10) strcpy(meth, "CAUSE_1 | ");
        else if (randmeth == 9) strcpy(meth, "CONF | ");
        else if (randmeth == 8) strcpy(meth, "BLINK | ");
        else if (randmeth == 7) strcpy(meth, "SCARE | ");
        else if (randmeth == 6) strcpy(meth, "BO_FIRE | ");
        else if (randmeth == 5) strcpy(meth, "BO_COLD | ");
        else if (randmeth == 4) strcpy(meth, "BO_ELEC | ");
        else if (randmeth == 3) strcpy(meth, "BO_ACID | ");
        else if (randmeth == 2) strcpy(meth, "HASTE | ");
        else strcpy(meth, "HEAL | ");

        return (meth);
}

/* Major spells */
/* Same as medium + summon spells */
char *get_major_mon_spell()
{
        int randmeth;

        randmeth = randint(67);

        if (randmeth == 67) strcpy(meth, "S_DEMON | ");
        else if (randmeth == 66) strcpy(meth, "S_DRAGON | ");
        else if (randmeth == 65) strcpy(meth, "S_HI_DRAGON | ");
        else if (randmeth == 64) strcpy(meth, "S_SPIDER | ");
        else if (randmeth == 63) strcpy(meth, "S_MONSTER | ");
        else if (randmeth == 62) strcpy(meth, "S_MONSTERS | ");
        else if (randmeth == 61) strcpy(meth, "S_KIN | ");
        else if (randmeth == 60) strcpy(meth, "S_ANGEL | ");
        else if (randmeth == 59) strcpy(meth, "S_HYDRA | ");
        else if (randmeth == 58) strcpy(meth, "S_UNDEAD | ");
        else if (randmeth == 57) strcpy(meth, "S_HI_UNDEAD | ");
        else if (randmeth == 56) strcpy(meth, "BA_DARK | ");
        else if (randmeth == 55) strcpy(meth, "BO_ICEE | ");
        else if (randmeth == 54) strcpy(meth, "BO_NETH | ");
        else if (randmeth == 53) strcpy(meth, "BO_MANA | ");
        else if (randmeth == 52) strcpy(meth, "BO_WATE | ");
        else if (randmeth == 51) strcpy(meth, "BO_POIS | ");
        else if (randmeth == 50) strcpy(meth, "DARKNESS | ");
        else if (randmeth == 49) strcpy(meth, "ARROW_4 | ");
        else if (randmeth == 48) strcpy(meth, "ARROW_3 | ");
        else if (randmeth == 47) strcpy(meth, "ARROW_2 | ");
        else if (randmeth == 46) strcpy(meth, "ARROW_1 | ");
        else if (randmeth == 45) strcpy(meth, "CAUSE_4 | ");
        else if (randmeth == 44) strcpy(meth, "CAUSE_3 | ");
        else if (randmeth == 43) strcpy(meth, "CAUSE_2 | ");
        else if (randmeth == 42) strcpy(meth, "ROCKET | ");
        else if (randmeth == 41) strcpy(meth, "BLIND | ");
        else if (randmeth == 40) strcpy(meth, "BA_FIRE | ");
        else if (randmeth == 39) strcpy(meth, "BA_COLD | ");
        else if (randmeth == 38) strcpy(meth, "BA_ACID | ");
        else if (randmeth == 37) strcpy(meth, "BA_ELEC | ");
        else if (randmeth == 36) strcpy(meth, "BA_POIS | ");
        else if (randmeth == 35) strcpy(meth, "BA_NUKE | ");
        else if (randmeth == 34) strcpy(meth, "BA_CHAO | ");
        else if (randmeth == 33) strcpy(meth, "BA_WATE | ");
        else if (randmeth == 32) strcpy(meth, "BA_NETH | ");
        else if (randmeth == 31) strcpy(meth, "BA_MANA | ");
        else if (randmeth == 30) strcpy(meth, "BR_FIRE | ");
        else if (randmeth == 29) strcpy(meth, "BR_COLD | ");
        else if (randmeth == 28) strcpy(meth, "BR_ACID | ");
        else if (randmeth == 27) strcpy(meth, "BR_ELEC | ");
        else if (randmeth == 26) strcpy(meth, "BR_POIS | ");
        else if (randmeth == 25) strcpy(meth, "BR_NUKE | ");
        else if (randmeth == 24) strcpy(meth, "BR_CHAO | ");
        else if (randmeth == 23) strcpy(meth, "BR_SOUN | ");
        else if (randmeth == 22) strcpy(meth, "BR_CONF | ");
        else if (randmeth == 21) strcpy(meth, "BR_DISE | ");
        else if (randmeth == 20) strcpy(meth, "BR_NEXU | ");
        else if (randmeth == 19) strcpy(meth, "BR_PLAS | ");
        else if (randmeth == 18) strcpy(meth, "BR_GRAV | ");
        else if (randmeth == 17) strcpy(meth, "BR_DISI | ");
        else if (randmeth == 16) strcpy(meth, "BR_WALL | ");
        else if (randmeth == 15) strcpy(meth, "BR_INER | ");
        else if (randmeth == 14) strcpy(meth, "BR_LITE | ");
        else if (randmeth == 13) strcpy(meth, "BR_DARK | ");
        else if (randmeth == 12) strcpy(meth, "BR_MANA | ");
        else if (randmeth == 11) strcpy(meth, "MISSILE | ");
        else if (randmeth == 10) strcpy(meth, "CAUSE_1 | ");
        else if (randmeth == 9) strcpy(meth, "CONF | ");
        else if (randmeth == 8) strcpy(meth, "BLINK | ");
        else if (randmeth == 7) strcpy(meth, "SCARE | ");
        else if (randmeth == 6) strcpy(meth, "BO_FIRE | ");
        else if (randmeth == 5) strcpy(meth, "BO_COLD | ");
        else if (randmeth == 4) strcpy(meth, "BO_ELEC | ");
        else if (randmeth == 3) strcpy(meth, "BO_ACID | ");
        else if (randmeth == 2) strcpy(meth, "HASTE | ");
        else strcpy(meth, "HEAL | ");

        return (meth);
}

char *mon_random_description(char monattr)
{
        int choosestring;
        char monrace[20], monadj1[20];

        if (monattr == 'a') strcpy(monrace, "ant");
        else if (monattr == 'b') strcpy(monrace, "bat");
        else if (monattr == 'c') strcpy(monrace, "centipede");
        else if (monattr == 'd') strcpy(monrace, "lesser dragon");
        else if (monattr == 'e') strcpy(monrace, "eye");
        else if (monattr == 'f') strcpy(monrace, "feline");
        else if (monattr == 'g') strcpy(monrace, "golem");
        else if (monattr == 'h') strcpy(monrace, "humanoid");
        else if (monattr == 'i') strcpy(monrace, "icky thing");
        else if (monattr == 'j') strcpy(monrace, "jelly");
        else if (monattr == 'k') strcpy(monrace, "kobold");
        else if (monattr == 'l') strcpy(monrace, "louse");
        else if (monattr == 'm') strcpy(monrace, "plant");
        else if (monattr == 'n') strcpy(monrace, "naga");
        else if (monattr == 'o') strcpy(monrace, "orc");
        else if (monattr == 'p') strcpy(monrace, "human");
        else if (monattr == 'q') strcpy(monrace, "quadroped");
        else if (monattr == 'r') strcpy(monrace, "rodent");
        else if (monattr == 's') strcpy(monrace, "skeleton");
        else if (monattr == 't') strcpy(monrace, "townsfolk");
        else if (monattr == 'u') strcpy(monrace, "minor demon");
        else if (monattr == 'v') strcpy(monrace, "vortex");
        else if (monattr == 'w') strcpy(monrace, "worm");
        else if (monattr == 'x' || monattr == '~') strcpy(monrace, "seafolk");
        else if (monattr == 'y') strcpy(monrace, "yeek");
        else if (monattr == 'z') strcpy(monrace, "zombie");
        else if (monattr == 'A') strcpy(monrace, "angel");
        else if (monattr == 'B') strcpy(monrace, "bird");
        else if (monattr == 'C') strcpy(monrace, "canine");
        else if (monattr == 'D') strcpy(monrace, "dragon");
        else if (monattr == 'E') strcpy(monrace, "spirit");
        else if (monattr == 'F') strcpy(monrace, "dragon fly");
        else if (monattr == 'G') strcpy(monrace, "ghost");
        else if (monattr == 'H') strcpy(monrace, "hybrid");
        else if (monattr == 'I') strcpy(monrace, "insect");
        else if (monattr == 'J') strcpy(monrace, "snake");
        else if (monattr == 'K') strcpy(monrace, "killer beetle");
        else if (monattr == 'L') strcpy(monrace, "lich");
        else if (monattr == 'M') strcpy(monrace, "hydra");
        else if (monattr == 'N') strcpy(monrace, "unknown monster");
        else if (monattr == 'O') strcpy(monrace, "ogre");
        else if (monattr == 'P') strcpy(monrace, "giant");
        else if (monattr == 'Q') strcpy(monrace, "quylthulg");
        else if (monattr == 'R') strcpy(monrace, "reptile");
        else if (monattr == 'S') strcpy(monrace, "spider");
        else if (monattr == 'T') strcpy(monrace, "troll");
        else if (monattr == 'U') strcpy(monrace, "demon");
        else if (monattr == 'V') strcpy(monrace, "vampire");
        else if (monattr == 'W') strcpy(monrace, "wraith");
        else if (monattr == 'X') strcpy(monrace, "stone hulk");
        else if (monattr == 'Y') strcpy(monrace, "yeti");
        else if (monattr == 'Z') strcpy(monrace, "hound");
        else if (monattr == '$') strcpy(monrace, "creeping coin");
        else if (monattr == '*') strcpy(monrace, "sphere");
        else if (monattr == ',') strcpy(monrace, "mushroom");
        else if (monattr == '&') strcpy(monrace, "devling");
        else strcpy(monrace, "unknown monster");

        choosestring = randint(100);
        if (choosestring >= 90) strcpy(monadj1, "to kill you.");
        else if (choosestring >= 80) strcpy(monadj1, "you dead.");
        else if (choosestring >= 70) strcpy(monadj1, "to slay you.");
        else if (choosestring >= 60) strcpy(monadj1, "to defeat you.");
        else if (choosestring >= 50) strcpy(monadj1, "your head.");
        else if (choosestring >= 40) strcpy(monadj1, "your blood.");
        else if (choosestring >= 30) strcpy(monadj1, "to murder you.");
        else if (choosestring >= 20) strcpy(monadj1, "your treasures.");
        else if (choosestring >= 10) strcpy(monadj1, "to end your life.");
        else strcpy(monadj1, "to hurt you.");

        /* Done */
        sprintf(finalstring, "A %s who wants %s", monrace, monadj1);

        return (finalstring);
}
