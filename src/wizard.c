/* wizard.c: Version history and info, and wizard mode debugging aids.

   Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

   This software may be copied and distributed for educational, research, and
   not for profit purposes provided that this copyright and statement are
   included in all such copies. */

#include <stdio.h>
#include "constant.h"
#include "config.h"
#include "types.h"
#include "externs.h"

#ifdef USG
#ifndef ATARIST_MWC
#include <string.h>
#endif
#else
#include <strings.h>
#endif

extern void pause_if_screen_full(int *, int);

long atol();

int is_wizard(uid)
  int uid;
{
  FILE *fp;
  char buf[100];
  int test;

  if ((fp=(FILE *)my_tfopen(ANGBAND_WIZ, "r"))==NULL) {
    fprintf(stderr, "Can't get wizard check...");
    exit_game();
  }
  do {
    (void) fgets(buf, sizeof buf, fp);
    if (sscanf(buf, "%d", &test)) {
      if (test==uid && buf[0]!='#') {
	fclose(fp);
	return TRUE;
      }
    }
  } while (!feof(fp));
  fclose(fp);
  return FALSE;
}

/* Check to see which artifacts have been seen		*/
void artifact_check()
{
  int i,j;
  j=15;
  save_screen();
  for(i=1;i<23;i++) erase_line(i, j-2);
  i=2;
  prt("Artifacts that Have Been Seen:",1,30);
  if (GROND) prt("Grond", i++, j);
  if (RINGIL) prt("Ringil", i++, j);
  if (AEGLOS) prt("Aeglos", i++, j);
  if (ARUNRUTH) prt("Arunruth", i++, j);
  if (MORMEGIL) prt("Mormegil", i++, j);
  if (ANGRIST) prt("Angrist", i++, j);
  if (GURTHANG) prt("Gurthang", i++, j);
  if (CALRIS) prt("Calris", i++, j);
  if (ANDURIL) prt("Anduril", i++, j);
  if (STING) prt("Sting", i++, j);
  if (ORCRIST) prt("Orcrist", i++, j);
  if (GLAMDRING) prt("Glamdring", i++, j);
  if (DURIN) prt("Durin", i++, j);
  if (AULE) prt("Aule", i++, j);
  if (THUNDERFIST) prt("Thunderfist", i++, j);
  if (BLOODSPIKE) prt("Bloodspike", i++, j);
  pause_if_screen_full(&i, j);
  if (DOOMCALLER) prt("Doomcaller", i++, j);
  pause_if_screen_full(&i, j);
  if (NARTHANC) prt("Narthanc", i++, j);
  pause_if_screen_full(&i, j);
  if (NIMTHANC) prt("Nimthanc", i++, j);
  pause_if_screen_full(&i, j);
  if (DETHANC) prt("Dethanc", i++, j);
  pause_if_screen_full(&i, j);
  if (GILETTAR) prt("Gilettar", i++, j);
  pause_if_screen_full(&i, j);
  if (RILIA) prt("Rilia", i++, j);
  pause_if_screen_full(&i, j);
  if (BELANGIL) prt("Belangil", i++, j);
  pause_if_screen_full(&i, j);
  if (BALLI) prt("Balli Stonehand", i++, j);
  pause_if_screen_full(&i, j);
  if (LOTHARANG) prt("Lotharang", i++, j);
  pause_if_screen_full(&i, j);
  if (FIRESTAR) prt("Firestar", i++, j);
  pause_if_screen_full(&i, j);
  if (ERIRIL) prt("Eriril", i++, j);
  pause_if_screen_full(&i, j);
  if (CUBRAGOL) prt("Cubragol", i++, j);
  pause_if_screen_full(&i, j);
  if (BARD) prt("Longbow of Bard", i++, j);
  pause_if_screen_full(&i, j);
  if (COLLUIN) prt("Colluin", i++, j);
  pause_if_screen_full(&i, j);
  if (HOLCOLLETH) prt("Holcolleth", i++, j);
  pause_if_screen_full(&i, j);
  if (TOTILA) prt("Totila", i++, j);
  pause_if_screen_full(&i, j);
  if (PAIN) prt("Glaive of Pain", i++, j);
  pause_if_screen_full(&i, j);
  if (ELVAGIL) prt("Elvagil", i++, j);
  pause_if_screen_full(&i, j);
  if (AGLARANG) prt("Aglarang", i++, j);
  pause_if_screen_full(&i, j);
  if (EORLINGAS) prt("Eorlingas", i++, j);
  pause_if_screen_full(&i, j);
  if (BARUKKHELED) prt("Barukkheled", i++, j);
  pause_if_screen_full(&i, j);
  if (WRATH) prt("Trident of Wrath", i++, j);
  pause_if_screen_full(&i, j);
  if (HARADEKKET) prt("Haradekket", i++, j);
  pause_if_screen_full(&i, j);
  if (MUNDWINE) prt("Mundwine", i++, j);
  pause_if_screen_full(&i, j);
  if (GONDRICAM) prt("Gondricam", i++, j);
  pause_if_screen_full(&i, j);
  if (ZARCUTHRA) prt("Zarcuthra", i++, j);
  pause_if_screen_full(&i, j);
  if (CARETH) prt("Careth Asdriag", i++, j);
  pause_if_screen_full(&i, j);
  if (FORASGIL) prt("Forasgil", i++, j);
  pause_if_screen_full(&i, j);
  if (CRISDURIAN) prt("Crisdurian", i++, j);
  pause_if_screen_full(&i, j);
  if (COLANNON) prt("Colannon", i++, j);
  pause_if_screen_full(&i, j);
  if (HITHLOMIR) prt("Hithlomir", i++, j);
  pause_if_screen_full(&i, j);
  if (THALKETTOTH) prt("Thalkettoth", i++, j);
  pause_if_screen_full(&i, j);
  if (ARVEDUI) prt("Arvedui", i++, j);
  pause_if_screen_full(&i, j);
  if (THRANDUIL) prt("Thranduil", i++, j);
  pause_if_screen_full(&i, j);
  if (THENGEL) prt("Thengel", i++, j);
  pause_if_screen_full(&i, j);
  if (HAMMERHAND) prt("Hammerhand", i++, j);
  pause_if_screen_full(&i, j);
  if (CELEFARN) prt("Celefarn", i++, j);
  pause_if_screen_full(&i, j);
  if (THROR) prt("Thror", i++, j);
  pause_if_screen_full(&i, j);
  if (MAEDHROS) prt("Maedhros", i++, j);
  pause_if_screen_full(&i, j);
  if (OLORIN) prt("Olorin", i++, j);
  pause_if_screen_full(&i, j);
  if (ANGUIREL) prt("Anguirel", i++, j);
  pause_if_screen_full(&i, j);
  if (OROME) prt("Orome", i++, j);
  pause_if_screen_full(&i, j);
  if (EONWE) prt("Eonwe", i++, j);
  pause_if_screen_full(&i, j);
  if (THEODEN) prt("Theoden", i++, j);
  pause_if_screen_full(&i, j);
  if (ULMO) prt("Trident of Ulmo", i++, j);
  pause_if_screen_full(&i, j);
  if (OSONDIR) prt("Osondir", i++, j);
  pause_if_screen_full(&i, j);
  if (TURMIL) prt("Turmil", i++, j);
  pause_if_screen_full(&i, j);
  if (TIL) prt("Til-i-arc", i++, j);
  pause_if_screen_full(&i, j);
  if (DEATHWREAKER) prt("Deathwreaker", i++, j);
  pause_if_screen_full(&i, j);
  if (AVAVIR) prt("Avavir", i++, j);
  pause_if_screen_full(&i, j);
  if (TARATOL) prt("Taratol", i++, j);
  pause_if_screen_full(&i, j);
  if (DOR_LOMIN) prt("Dor-Lomin", i++, j);
  pause_if_screen_full(&i, j);
  if (BELEGENNON) prt("Belegennon", i++, j);
  pause_if_screen_full(&i, j);
  if (FEANOR) prt("Feanor", i++, j);
  pause_if_screen_full(&i, j);
  if (ISILDUR) prt("Isildur", i++, j);
  pause_if_screen_full(&i, j);
  if (SOULKEEPER) prt("Soulkeeper", i++, j);
  pause_if_screen_full(&i, j);
  if (FINGOLFIN) prt("Fingolfin", i++, j);
  pause_if_screen_full(&i, j);
  if (ANARION) prt("Anarion", i++, j);
  pause_if_screen_full(&i, j);
  if (BELEG) prt("Beleg Cuthalion", i++, j);
  pause_if_screen_full(&i, j);
  if (DAL) prt("Dal-i-thalion", i++, j);
  pause_if_screen_full(&i, j);
  if (PAURHACH) prt("Paurhach", i++, j);
  pause_if_screen_full(&i, j);
  if (PAURNIMMEN) prt("Paurnimmen", i++, j);
  pause_if_screen_full(&i, j);
  if (PAURAEGEN) prt("Pauragen", i++, j);
  pause_if_screen_full(&i, j);
  if (PAURNEN) prt("Paurnen", i++, j);
  pause_if_screen_full(&i, j);
  if (CAMMITHRIM) prt("Cammithrin", i++, j);
  pause_if_screen_full(&i, j);
  if (CAMBELEG) prt("Cambeleg", i++, j);
  pause_if_screen_full(&i, j);
  if (HOLHENNETH) prt("Holhenneth", i++, j);
  pause_if_screen_full(&i, j);
  if (AEGLIN) prt("Aeglin", i++, j);
  pause_if_screen_full(&i, j);
  if (CAMLOST) prt("Camlost", i++, j);
  pause_if_screen_full(&i, j);
  if (NIMLOTH) prt("Nimloth", i++, j);
  pause_if_screen_full(&i, j);
  if (NAR) prt("Nar-i-vagil", i++, j);
  pause_if_screen_full(&i, j);
  if (BERUTHIEL) prt("Beruthiel", i++, j);
  pause_if_screen_full(&i, j);
  if (GORLIM) prt("Gorlim", i++, j);
  pause_if_screen_full(&i, j);
  if (THORIN) prt("Thorin", i++, j);
  pause_if_screen_full(&i, j);
  if (CELEBORN) prt("Celeborn", i++, j);
  pause_if_screen_full(&i, j);
  if (GONDOR) prt("Gondor", i++, j);
  pause_if_screen_full(&i, j);
  if (THINGOL) prt("Thingol", i++, j);
  pause_if_screen_full(&i, j);
  if (THORONGIL) prt("Thorongil", i++, j);
  pause_if_screen_full(&i, j);
  if (LUTHIEN) prt("Luthien", i++, j);
  pause_if_screen_full(&i, j);
  if (TUOR) prt("Tuor", i++, j);
  pause_if_screen_full(&i, j);
  if (ROHAN) prt("Rohan", i++, j);
  pause_if_screen_full(&i, j);
  if (CASPANION) prt("Caspanion", i++, j);
  pause_if_screen_full(&i, j);
  if (NARYA) prt("Narya", i++, j);
  pause_if_screen_full(&i, j);
  if (NENYA) prt("Nenya", i++, j);
  pause_if_screen_full(&i, j);
  if (VILYA) prt("Vilya", i++, j);
  pause_if_screen_full(&i, j);
  if (POWER) prt("The One Ring", i++, j);
  pause_if_screen_full(&i, j);
  if (PHIAL) prt("The Phial of Galadriel", i++, j);
  pause_if_screen_full(&i, j);
  if (INGWE) prt("The Amulet of Ingwe", i++, j);
  pause_if_screen_full(&i, j);
  if (CARLAMMAS) prt("The Amulet of Carlammas", i++, j);
  pause_if_screen_full(&i, j);
  if (TULKAS) prt("The Ring of Tulkas", i++, j);
  pause_if_screen_full(&i, j);
  if (NECKLACE) prt("The Amulet of the Dwarves", i++, j);
  pause_if_screen_full(&i, j);
  if (BARAHIR) prt("The Ring of Barahir", i++, j);
  pause_if_screen_full(&i, j);
  if (ELENDIL) prt("The Star of Elendil", i++, j);
  pause_if_screen_full(&i, j);
  if (THRAIN) prt("The Arkenstone of Thrain", i++, j);
  pause_if_screen_full(&i, j);
  if (RAZORBACK) prt("Skullcleaver", i++, j);
  pause_if_screen_full(&i, j);
  if (BLADETURNER) prt("Bladeturner", i++, j);
  pause_if_screen_full(&i, j);
  if (ROBEMED) prt("Robe of Enlightement", i++, j);
  pause_line(i);
  restore_screen();
}

/* Light up the dungeon					-RAK-	*/
void wizard_light(light)
  int light;
{
  register cave_type *c_ptr;
  register int k, l, i, j;
  int flag;

  if (!light) {
    if (cave[char_row][char_col].pl)
      flag = FALSE;
    else
      flag = TRUE;
  } else {
    flag = (light>0)?1:0;
  }
  for (i = 0; i < cur_height; i++)
    for (j = 0; j < cur_width; j++)
      if (cave[i][j].fval <= MAX_CAVE_FLOOR)
	for (k = i-1; k <= i+1; k++)
	  for (l = j-1; l <= j+1; l++)
	    {
	      c_ptr = &cave[k][l];
	      c_ptr->pl = flag;
	      if (!flag)
		c_ptr->fm = FALSE;
	    }
  prt_map();
}


/* Wizard routine for gaining on stats			-RAK-	*/
void change_character()
{
  register int tmp_val, loop;
  register int32 tmp_lval;
  int16u *a_ptr;
  vtype tmp_str;
  register struct misc *m_ptr;

  a_ptr = py.stats.max_stat;
  prt("(3 - 118) Strength     = ", 0, 0);
  if (get_string(tmp_str, 0, 25, 3))
    {
      tmp_val = atoi(tmp_str);
      if ((tmp_val > 2) && (tmp_val < 119))
	{
	  a_ptr[A_STR] = tmp_val;
	  (void) res_stat(A_STR);
	}
    }
  else
    return;

  prt("(3 - 118) Intelligence = ", 0, 0);
  if (get_string(tmp_str, 0, 25, 3))
    {
      tmp_val = atoi(tmp_str);
      if ((tmp_val > 2) && (tmp_val < 119))
	{
	  a_ptr[A_INT] = tmp_val;
	  (void) res_stat(A_INT);
	}
    }
  else
    return;

  prt("(3 - 118) Wisdom       = ", 0, 0);
  if (get_string(tmp_str, 0, 25, 3))
    {
      tmp_val = atoi(tmp_str);
      if ((tmp_val > 2) && (tmp_val < 119))
	{
	  a_ptr[A_WIS] = tmp_val;
	  (void) res_stat(A_WIS);
	}
    }
  else
    return;

  prt("(3 - 118) Dexterity    = ", 0, 0);
  if (get_string(tmp_str, 0, 25, 3))
    {
      tmp_val = atoi(tmp_str);
      if ((tmp_val > 2) && (tmp_val < 119))
	{
	  a_ptr[A_DEX] = tmp_val;
	  (void) res_stat(A_DEX);
	}
    }
  else
    return;

  prt("(3 - 118) Constitution = ", 0, 0);
  if (get_string(tmp_str, 0, 25, 3))
    {
      tmp_val = atoi(tmp_str);
      if ((tmp_val > 2) && (tmp_val < 119))
	{
	  a_ptr[A_CON] = tmp_val;
	  (void) res_stat(A_CON);
	}
    }
  else
    return;

  prt("(3 - 118) Charisma     = ", 0, 0);
  if (get_string(tmp_str, 0, 25, 3))
    {
      tmp_val = atoi(tmp_str);
      if ((tmp_val > 2) && (tmp_val < 119))
	{
	  a_ptr[A_CHR] = tmp_val;
	  (void) res_stat(A_CHR);
	}
    }
  else
    return;
  prt("(3 - 118) Luck     = ", 0, 0);
  if (get_string(tmp_str, 0, 25, 3))
    {
      tmp_val = atoi(tmp_str);
      if ((tmp_val > 2) && (tmp_val < 119))
	{
	  a_ptr[A_LUC] = tmp_val;
	  (void) res_stat(A_LUC);
	}
    }
  else
   return;
  m_ptr = &py.misc;
  prt("(1 - 32767) Hit points = ", 0, 0);
  if (get_string(tmp_str, 0, 25, 5))
    {
      tmp_val = atoi(tmp_str);
      if ((tmp_val > 0) && (tmp_val <= MAX_SHORT))
	{
	  m_ptr->mhp  = tmp_val;
	  m_ptr->chp  = tmp_val;
	  m_ptr->chp_frac = 0;
	  prt_mhp();
	  prt_chp();
	}
    }
  else
    return;

  prt("(0 - 32767) Mana       = ", 0, 0);
  if (get_string(tmp_str, 0, 25, 5))
    {
      tmp_val = atoi(tmp_str);
      if ((tmp_val > -1) && (tmp_val <= MAX_SHORT) && (*tmp_str != '\0'))
	{
	  m_ptr->mana  = tmp_val;
	  m_ptr->cmana = tmp_val;
	  m_ptr->cmana_frac = 0;
	  prt_cmana();
	}
    }
  else
    return;

  (void) sprintf(tmp_str, "Current=%ld  Gold = ", m_ptr->au);
  tmp_val = strlen(tmp_str);
  prt(tmp_str, 0, 0);
  if (get_string(tmp_str, 0, tmp_val, 7))
    {
      tmp_lval = atol(tmp_str);
      if (tmp_lval > -1 && (*tmp_str != '\0'))
	{
	  m_ptr->au = tmp_lval;
	  prt_gold();
	}
    }
  else
    return;
  (void) sprintf(tmp_str, "Current=%ld  Max Exp = ", m_ptr->max_exp);
  tmp_val = strlen(tmp_str);
  prt(tmp_str, 0, 0);
  if (get_string(tmp_str, 0, tmp_val, 7))
    {
      tmp_lval = atol(tmp_str);
      if (tmp_lval > -1 && (*tmp_str != '\0'))
	{
	  m_ptr->max_exp = tmp_lval;
	  prt_experience();
	}
    }
  else
    return;

  for(loop=0;loop<S_NUM;loop++)
    {
      if (snames[loop][0])
	{
	  (void) sprintf(tmp_str, "Current=%d (0-255) %s = ",
			 py.skills.cur_skill[loop],snames[loop]);
	  tmp_val = strlen(tmp_str);
	  prt(tmp_str, 0, 0);
	  if (get_string(tmp_str, 0, tmp_val, 3))
	    {
	      tmp_val = atoi(tmp_str);
	      if ((tmp_val > -1) && (tmp_val < 256) && (*tmp_str != '\0'))
		py.skills.cur_skill[loop]  = tmp_val;
	    }
	  else
	    return;
	}
    }
  (void) sprintf(tmp_str, "Current=%d  Weight = ", m_ptr->wt);
  tmp_val = strlen(tmp_str);
  prt(tmp_str, 0, 0);
  if (get_string(tmp_str, 0, tmp_val, 3))
    {
      tmp_val = atoi(tmp_str);
      if (tmp_val > -1 && (*tmp_str != '\0'))
	m_ptr->wt = tmp_val;
    }
  else
    return;

  while(get_com("Alter speed? (+/-)", tmp_str))
    {
      if (*tmp_str == '+')
	change_speed(-1);
      else if (*tmp_str == '-')
	change_speed(1);
      else
	break;
      prt_speed();
    }
}


/* Wizard routine for creating objects			-RAK-	*/
void wizard_create()
{
  register int tmp_val;
  int i, j, k;
  int32 tmp_lval;
  char tmp_str[100];
  register inven_type *i_ptr;
  treasure_type t_type, *t_ptr;
  inven_type forge;
  register cave_type *c_ptr;
  char ch;
  int more = FALSE;

  t_ptr = &t_type;
  i_ptr = &forge;
  i_ptr->name2 = 0;
  i_ptr->ident = ID_KNOWN2|ID_STOREBOUGHT;

  save_screen();
  prt("What type of item?    : ", 0, 0);
  prt("[W]eapon, [A]rmour, [O]thers.", 1, 0);
  if (!get_com((char *)0, &ch))
    {restore_screen();return;}
  switch (ch) {
  case 'W':
  case 'w':
    prt("What type of Weapon?    : ", 0, 0);
    prt("[S]word, [H]afted, [P]olearm, [B]ow, [A]mmo.", 1, 0);
    if (!get_com((char *)0, &ch))
      {restore_screen();return;}
    switch (ch) {
    case 'S':
    case 's':
      i_ptr->tval=TV_SWORD;
      break;
    case 'H':
    case 'h':
      i_ptr->tval=TV_HAFTED;
      break;
    case 'P':
    case 'p':
      i_ptr->tval=TV_POLEARM;
      break;
    case 'B':
    case 'b':
      i_ptr->tval=TV_BOW;
      break;
    case 'A':
    case 'a':
      prt("What type of Ammo?    : ", 0, 0);
      prt("[A]rrow, [B]olt, [P]ebble.", 1, 0);
      if (!get_com((char *)0, &ch))
	{restore_screen();return;}
      switch (ch) {
      case 'A':
      case 'a':
	i_ptr->tval=TV_ARROW;
	break;
      case 'B':
      case 'b':
	i_ptr->tval=TV_BOLT;
	break;
      case 'P':
      case 'p':
	i_ptr->tval=TV_SLING_AMMO;
	break;
      default:
	break;
      }
      break;
    default:
      restore_screen();
      return;
    }
    break;
  case 'A':
  case 'a':
    prt("What type of Armour?    : ", 0, 0);
    prt("[A]rmour, [G]loves, [B]oots, [S]hields, [H]elms, [C]loaks.", 1, 0);
    if (!get_com((char *)0, &ch))
      {restore_screen();return;}
    switch (ch) {
    case 'S':
    case 's':
      i_ptr->tval=TV_SHIELD;
      break;
    case 'H':
    case 'h':
      i_ptr->tval=TV_HELM;
      break;
    case 'G':
    case 'g':
      i_ptr->tval=TV_GLOVES;
      break;
    case 'B':
    case 'b':
      i_ptr->tval=TV_BOOTS;
      break;
    case 'C':
    case 'c':
      i_ptr->tval=TV_CLOAK;
      break;
    case 'A':
    case 'a':
      prt("What type of Armour?    : ", 0, 0);
      prt("[H]ard armour, [S]oft armour.", 1, 0);
      if (!get_com((char *)0, &ch))
	{restore_screen();return;}
      switch (ch) {
      case 'H':
      case 'h':
	i_ptr->tval=TV_HARD_ARMOR;
	break;
      case 'S':
      case 's':
	i_ptr->tval=TV_SOFT_ARMOR;
	break;
      default:
	break;
      }
      break;
    default:
      restore_screen();
      return;
    }
    break;
  case 'O':
  case 'o':
    prt("What type of Object?    : ", 0, 0);
    prt(
"[R]ing, [P]otion, [W]and/staff, [S]croll, [M]agicbook, [A]mulet, [T]ool",
	1, 0);
    if (!get_com((char *)0, &ch))
      {restore_screen();return;}
    switch (ch) {
    case 'R':
    case 'r':
      i_ptr->tval=TV_RING;
      break;
    case 'P':
    case 'p':
      i_ptr->tval=TV_POTION1;
      break;
    case 'S':
    case 's':
      i_ptr->tval=TV_SCROLL1;
      break;
    case 'A':
    case 'a':
      i_ptr->tval=TV_AMULET;
      break;
    case 'W':
    case 'w':
      prt("Wand, Staff or Rod?    : ", 0, 0);
      prt("[W]and, [S]taff, [R]od.", 1, 0);
      if (!get_com((char *)0, &ch))
	{restore_screen();return;}
      switch (ch) {
      case 'W':
      case 'w':
	i_ptr->tval=TV_WAND;
	break;
      case 'S':
      case 's':
	i_ptr->tval=TV_STAFF;
	break;
      case 'R':
      case 'r':
	i_ptr->tval=TV_ROD;
	break;
      default:
	restore_screen();
	return;
      }
      break;
    case 'M':
    case 'm':
      prt("Spellbook, Prayerbook, Naturebook, or Darkbook?    : ", 0, 0);
      prt("[S]pellbook, [P]rayerbook, [N]aturebook, [D]arkbook", 1, 0);
      if (!get_com((char *)0, &ch))
	{restore_screen();return;}
      switch (ch) {
      case 'P':
      case 'p':
	i_ptr->tval=TV_PRAYER_BOOK;
	break;
      case 'S':
      case 's':
	i_ptr->tval=TV_MAGIC_BOOK;
	break;
      case 'N':
      case 'n':
	i_ptr->tval=TV_NATURE_BOOK;
	break;
      case 'D':
      case 'd':
	i_ptr->tval=TV_DARK_BOOK;
	break;
      default:
	restore_screen();
	return;
      }
      break;
    case 'T':
    case 't':
      prt("Which Tool etc...?  : ", 0, 0);
      prt("[S]pike, [D]igger, [C]hest, [L]ight, [F]ood, [O]il, [P]iece", 1, 0);
      if (!get_com((char *)0, &ch))
	{restore_screen();return;}
      switch (ch) {
      case 'S':
      case 's':
	i_ptr->tval=TV_SPIKE;
	break;
      case 'd':
      case 'D':
	i_ptr->tval=TV_DIGGING;
	break;
      case 'P':
      case 'p':
	i_ptr->tval=TV_COMPONENT;
	break;
      case 'C':
      case 'c':
	i_ptr->tval=TV_CHEST;
	break;
      case 'L':
      case 'l':
	i_ptr->tval=TV_LIGHT;
	break;
      case 'F':
      case 'f':
	i_ptr->tval=TV_FOOD;
	break;
      case 'O':
      case 'o':
	i_ptr->tval=TV_FLASK;
	break;
      default:
	restore_screen();
	return;
      }
      break;
    default:
      restore_screen();
      return;
    }
    break;
  default:
    restore_screen();
    return;
  }

  j=0;
  i=0;
  k=0;
 again:
  restore_screen();
  save_screen();
  prt("Which Item?  : ", 0, 0);
  for (; i<MAX_DUNGEON_OBJ; i++) {
    switch (i_ptr->tval) {
    case TV_POTION1:
      if ((object_list[i].tval == TV_POTION1) ||
          (object_list[i].tval == TV_POTION2)) {
         sprintf(tmp_str, "%c) %s", 'a'+j, object_list[i].name);
         prt(tmp_str, 1+j, 0);
         j++;
      }
      break;
    case TV_SCROLL1:
      if ((object_list[i].tval == TV_SCROLL1) ||
          (object_list[i].tval == TV_SCROLL2)) {
         sprintf(tmp_str, "%c) %s", 'a'+j, object_list[i].name);
         prt(tmp_str, 1+j, 0);
         j++;
      }
      break;
    default:
      if (object_list[i].tval == i_ptr->tval) {
         sprintf(tmp_str, "%c) %s", 'a'+j, object_list[i].name);
         prt(tmp_str, 1+j, 0);
         j++;
      }
      break;
    }
    if (j==21) {
      more = TRUE;
      break;
    }
  }
  if (j<21) {
    for (i=(i-(MAX_DUNGEON_OBJ-1))+(SPECIAL_OBJ-1); i<MAX_OBJECTS; i++) {
      switch (i_ptr->tval) {
      case TV_POTION1:
        if ((object_list[i].tval == TV_POTION1) ||
            (object_list[i].tval == TV_POTION2)) {
           sprintf(tmp_str, "%c) %s", 'a'+j, object_list[i].name);
           prt(tmp_str, 1+j, 0);
           j++;
        }
        break;
      case TV_SCROLL1:
        if ((object_list[i].tval == TV_SCROLL1) ||
            (object_list[i].tval == TV_SCROLL2)) {
           sprintf(tmp_str, "%c) %s", 'a'+j, object_list[i].name);
           prt(tmp_str, 1+j, 0);
           j++;
        }
        break;
      default:
        if (object_list[i].tval == i_ptr->tval) {
           sprintf(tmp_str, "%c) %s", 'a'+j, object_list[i].name);
           prt(tmp_str, 1+j, 0);
           j++;
        }
        break;
      }
      if (j==21) {
	more=TRUE;
	break;
      }
    }
  }
  if (more) prt("v) NEXT PAGE", 22, 0);

  do {
    if (!get_com((char *)0, &ch))
      {restore_screen();return;}
  } while ((ch<'a' && ch>('a'+j))||(more && ch<'a' && ch>('a'+j+1)));

  if ((ch=='v')&&more) {
    more=FALSE;
    k+=(j-1);
    j=0;
    goto again;
  }

  k+=(ch-'a'+1);

  j=0;
  for (i=0; i<MAX_DUNGEON_OBJ; i++) {
    switch (i_ptr->tval) {
    case TV_POTION1:
      if ((object_list[i].tval == TV_POTION1) ||
          (object_list[i].tval == TV_POTION2)) {
         j++;
      }
      break;
    case TV_SCROLL1:
      if ((object_list[i].tval == TV_SCROLL1) ||
          (object_list[i].tval == TV_SCROLL2)) {
         j++;
      }
      break;
    default:
      if (object_list[i].tval == i_ptr->tval) {
         j++;
      }
      break;
    }
    if (j==k) break;
  }
  if (j!=k) {
    for (i=(SPECIAL_OBJ-1); i<MAX_OBJECTS; i++) {
      switch (i_ptr->tval) {
      case TV_POTION1:
        if ((object_list[i].tval == TV_POTION1) ||
            (object_list[i].tval == TV_POTION2)) {
           j++;
        }
        break;
      case TV_SCROLL1:
        if ((object_list[i].tval == TV_SCROLL1) ||
            (object_list[i].tval == TV_SCROLL2)) {
           j++;
        }
        break;
      default:
        if (object_list[i].tval == i_ptr->tval) {
           j++;
        }
        break;
      }
      if (j==k) break;
    }
  }

  if (j!=k) {restore_screen();return;}

  invcopy(i_ptr, i);
  i_ptr->timeout=0;
  restore_screen();
  save_screen();

  prt("Number of items? [return=1]: ", 0, 0);
  if (!get_string(tmp_str, 0, 33, 5)) goto end;
  tmp_val = atoi(tmp_str);
  if (tmp_val!=0) i_ptr->number = tmp_val;

  prt("Weight of item? [return=default]: ", 0, 0);
  if (!get_string(tmp_str, 0, 35, 5)) goto end;
  tmp_val = atoi(tmp_str);
  if (tmp_val!=0) i_ptr->weight = tmp_val;

  if ((i_ptr->tval==TV_SWORD) ||
      (i_ptr->tval==TV_HAFTED) ||
      (i_ptr->tval==TV_POLEARM) ||
      (i_ptr->tval==TV_ARROW) ||
      (i_ptr->tval==TV_BOLT) ||
      (i_ptr->tval==TV_SLING_AMMO) ||
      (i_ptr->tval==TV_DIGGING)) {
	i_ptr->ident|=ID_SHOW_HITDAM;
	prt("Damage (dice): ", 0, 0);
	if (!get_string(tmp_str, 0, 15, 3)) goto end;
	tmp_val = atoi(tmp_str);
	if (tmp_val!=0) i_ptr->damage[0] = tmp_val;
	prt("Damage (sides): ", 0, 0);
	if (!get_string(tmp_str, 0, 16, 3)) goto end;
	tmp_val = atoi(tmp_str);
	if (tmp_val!=0) i_ptr->damage[1] = tmp_val;
      }

  prt("+To hit: ", 0, 0);
  if (!get_string(tmp_str, 0, 9, 3)) goto end;
  tmp_val = atoi(tmp_str);
  if (tmp_val!=0) i_ptr->tohit = tmp_val;

  prt("+To dam: ", 0, 0);
  if (!get_string(tmp_str, 0, 9, 3)) goto end;
  tmp_val = atoi(tmp_str);
  if (tmp_val!=0) i_ptr->todam = tmp_val;

  if ((i_ptr->tval==TV_SOFT_ARMOR) ||
      (i_ptr->tval==TV_HARD_ARMOR) ||
      (i_ptr->tval==TV_HELM) ||
      (i_ptr->tval==TV_CLOAK) ||
      (i_ptr->tval==TV_BOOTS) ||
      (i_ptr->tval==TV_GLOVES) ||
      (i_ptr->tval==TV_SHIELD)) {
    prt("Base AC : ", 0, 0);
    if (!get_string(tmp_str, 0, 10, 3)) goto end;
    tmp_val = atoi(tmp_str);
    if (tmp_val!=0) i_ptr->ac = tmp_val;
  }

  prt("+To AC : ", 0, 0);
  if (!get_string(tmp_str, 0, 9, 3)) goto end;
  tmp_val = atoi(tmp_str);
  if (tmp_val!=0) i_ptr->toac = tmp_val;

  prt("Magic Plus Flag  : ", 0, 0);
  if (!get_string(tmp_str, 0, 20, 5)) goto end;
  tmp_val = atoi(tmp_str);
  if (tmp_val!=0) i_ptr->p1 = tmp_val;


  save_screen();

  if ((i_ptr->tval==TV_SWORD) ||
      (i_ptr->tval==TV_HAFTED) ||
      (i_ptr->tval==TV_POLEARM) ||
      (i_ptr->tval==TV_ARROW) ||
      (i_ptr->tval==TV_BOLT) ||
      (i_ptr->tval==TV_SLING_AMMO) ||
      (i_ptr->tval==TV_DIGGING)) {
	if (get_com("Slay Evil? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags |= TR_SLAY_EVIL;
	} else if (ch=='\033') goto end;
	if (get_com("Slay Animal? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags |= TR_SLAY_ANIMAL;
	} else if (ch=='\033') goto end;
	if (get_com("Slay Undead? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags |= TR_SLAY_UNDEAD;
	} else if (ch=='\033') goto end;
	if (get_com("Slay Giant? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_SLAY_GIANT;
	} else if (ch=='\033') goto end;
	if (get_com("Slay Demon? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_SLAY_DEMON;
	} else if (ch=='\033') goto end;
	if (get_com("Slay Troll? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_SLAY_TROLL;
	} else if (ch=='\033') goto end;
	if (get_com("Slay Orc? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_SLAY_ORC;
	} else if (ch=='\033') goto end;
	if (get_com("Slay Dragon? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags |= TR_SLAY_DRAGON;
	} else if (ch=='\033') goto end;
	if (get_com("Execute Dragon? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags |= TR_SLAY_X_DRAGON;
	} else if (ch=='\033') goto end;
	if (get_com("Frost Brand? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags |= TR_FROST_BRAND;
	} else if (ch=='\033') goto end;
	if (get_com("Fire Brand? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags |= TR_FLAME_TONGUE;
	} else if (ch=='\033') goto end;
	if (get_com("Lightning Brand? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_LIGHTNING;
	} else if (ch=='\033') goto end;
	if (get_com("Earthquake Brand? [yn]: ", &ch)) {
	  if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_IMPACT;
	} else if (ch=='\033') goto end;
      }

  if (get_com("Affect Strength? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_STR;
  } else if (ch=='\033') goto end;
  if (get_com("Affect Intelligence? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_INT;
  } else if (ch=='\033') goto end;
  if (get_com("Affect Wisdom? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_WIS;
  } else if (ch=='\033') goto end;
  if (get_com("Affect Dexterity? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_DEX;
  } else if (ch=='\033') goto end;
  if (get_com("Affect Constitution? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_CON;
  } else if (ch=='\033') goto end;
  if (get_com("Affect Charisma? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_CHR;
  } else if (ch=='\033') goto end;
  if (get_com("Automatic Searching? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_SEARCH;
  } else if (ch=='\033') goto end;
  if (get_com("Slow Digestion? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_SLOW_DIGEST;
  } else if (ch=='\033') goto end;
  if (get_com("Stealth? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_STEALTH;
  } else if (ch=='\033') goto end;
  if (get_com("Aggravate Monsters? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_AGGRAVATE;
  } else if (ch=='\033') goto end;
  if (get_com("Regeneration? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_REGEN;
  } else if (ch=='\033') goto end;
  if (get_com("Speed? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_SPEED;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Fire? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_RES_FIRE;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Cold? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_RES_COLD;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Acid? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_RES_ACID;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Lightning? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_RES_LIGHT;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Poison? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_POISON;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Confusion? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_RES_CONF;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Sound? [yn]: ",&ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_RES_SOUND;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Light? [yn]: ",&ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_RES_LT;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Dark? [yn]: ",&ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_RES_DARK;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Chaos? [yn]: ",&ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_RES_CHAOS;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Disenchantment? [yn]: ",&ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_RES_DISENCHANT;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Shards? [yn]: ",&ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_RES_SHARDS;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Nexus? [yn]: ",&ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_RES_NEXUS;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Nether? [yn]: ",&ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_RES_NETHER;
  } else if (ch=='\033') goto end;
  if (get_com("Resist Blindness? [yn]: ",&ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_RES_BLIND;
  } else if (ch=='\033') goto end;
  if (get_com("Sustain a stat (Magic value 10 for all stats)? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_SUST_STAT;
  } else if (ch=='\033') goto end;
  if (get_com("See invisible? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_SEE_INVIS;
  } else if (ch=='\033') goto end;
  if (get_com("Free Action? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_FREE_ACT;
  } else if (ch=='\033') goto end;
  if (get_com("Feather Falling? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_FFALL;
  } else if (ch=='\033') goto end;
  if (get_com("Tunneling? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_TUNNEL;
  } else if (ch=='\033') goto end;
  if (get_com("Infra-vision? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_INFRA;
  } else if (ch=='\033') goto end;
  if (get_com("Resist life level loss? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_HOLD_LIFE;
  } else if (ch=='\033') goto end;
  if (get_com("Telepathy? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_TELEPATHY;
  } else if (ch=='\033') goto end;
  if (get_com("Immune to Fire? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_IM_FIRE;
  } else if (ch=='\033') goto end;
  if (get_com("Immune to Cold? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_IM_COLD;
  } else if (ch=='\033') goto end;
  if (get_com("Immune to Acid? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_IM_ACID;
  } else if (ch=='\033') goto end;
  if (get_com("Immune to Lightning? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_IM_LIGHT;
  } else if (ch=='\033') goto end;
  if (get_com("Immune to Poison? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_IM_POISON;
  } else if (ch=='\033') goto end;
  if (get_com("Give off Light? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_LIGHT;
  } else if (ch=='\033') goto end;
  if (get_com("Is it an Artifact? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_ARTIFACT;
  } else if (ch=='\033') goto end;
  if (get_com("Active Artifact? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags2 |= TR_ACTIVATE;
  } else if (ch=='\033') goto end;
  if (get_com("Cursed? [yn]: ", &ch)) {
    if (ch=='y'||ch=='Y') i_ptr->flags |= TR_CURSED;
  } else if (ch=='\033') goto end;

  prt("Cost : ", 0, 0);
  if (!get_string(tmp_str, 0, 9, 8)) {restore_screen();return;}
  tmp_lval = atol(tmp_str);
  if (tmp_val!=0) i_ptr->cost = tmp_lval;

  prt("Dungeon Level on which it is found : ", 0, 0);
  if (!get_string(tmp_str, 0, 39, 3)) {restore_screen();return;}
  tmp_val = atoi(tmp_str);
  if (tmp_val!=0) i_ptr->level = tmp_val;

  j=0;
  i=0;
  k=0;
  more=FALSE;
 SNagain:
  restore_screen();
  save_screen();
  for (; i<SN_ARRAY_SIZE; i++) {
    sprintf(tmp_str, "%c) %s", 'a'+j, special_names[i]);
    prt(tmp_str, 1+j, 0);
    j++;
    if (j==21) {
      more = TRUE;
      break;
    }
  }
  if (more) prt("v) NEXT PAGE", 22, 0);

  do {
    if (!get_com("Please choose a secondary name for the item : ", &ch))
      {restore_screen();return;}
  } while ((ch<'a' && ch>('a'+j))||(more && ch<'a' && ch>('a'+j+1)));

  if ((ch=='v')&&more) {
    more=FALSE;
    k+=(j-1);
    j=0;
    goto SNagain;
  } else {
    i_ptr->name2=k+(ch-'a');
  }
  restore_screen();
  save_screen();

 end:
  if (get_check("Allocate?")) {
    /* delete object first if any, before call popt */
      c_ptr = &cave[char_row][char_col];
      if (c_ptr->tptr != 0)
	(void) delete_object(char_row, char_col);

      store_bought(i_ptr);
      tmp_val = popt();
      t_list[tmp_val] = forge;
      c_ptr->tptr = tmp_val;
      msg_print("Allocated.");
    }
  else
    msg_print("Aborted.");
  restore_screen();
}
