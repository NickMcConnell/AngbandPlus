/* File: file_character.c */

/* Purpose: character dump */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "tnb.h"

extern cptr likert(int x, int y);

#if defined(ANGBANDTK) || defined(KANGBANDTK)

/*01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
static char *s_layout[] = {
#if defined(ANGBANDTK)
 "  [AngbandTk %vers Character Dump]                                              ",
#endif /* */
#if defined(KANGBANDTK)
 "  [KAngbandTk %vers Character Dump]                                             ",
#endif /* */
 "                                                                                ",
 " Name   %name                                    Self  RB  CB  EB   Best        ",
 " Sex    %sex              Age      %age   %nstr   %s1 %s2 %s3 %s4    %s5    %s6 ",
 " Race   %race             Height   %hgt   %nint   %i1 %i2 %i3 %i4    %i5    %i6 ",
 " Class  %class            Weight   %wgt   %nwis   %w1 %w2 %w3 %w4    %w5    %w6 ",
 " Title  %title            Status    %sc   %ndex   %d1 %d2 %d3 %d4    %d5    %d6 ",
 " HP     %hp               Maximize %max   %ncon   %c1 %c2 %c3 %c4    %c5    %c6 ",
 " SP     %sp               Preserve %prv   %nchr   %h1 %h2 %h3 %h4    %h5    %h6 ",
 "                                                                                ",
 " Level         %lev       Armor       %armor     Saving Throw      %save        ",
 " Cur Exp       %exp       Fight       %fight     Stealth           %stel        ",
 " Max Exp      %mexp       Melee       %melee     Fighting          %figt        ",
 " Adv Exp       %adv       Shoot       %shoot     Shooting          %bows        ",
 "                          Blows       %blows     Disarming         %disr        ",
 " Gold           %au       Shots       %shots     Magic Device      %magd        ",
 "                                                 Perception        %perc        ",
 " Burden     %burden       Infra       %infra     Searching         %srch        ",
 "                                                                                ",
 "     %hist                                                                      ",
 "                                                                                ",
 "                                                                                ",
 "                                                                                ",
NULL
};

#endif /* ANGBANDTK, KANGBANDTK */

#if defined(OANGBANDTK)

/*01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
static char *s_layout[] = {
 "  [OangbandTk %vers Character Dump]                                             ",
 "                                                                                ",
 " Name    : %name           Age                %age   %nstr     %str    %str2    ",
 " Sex     : %sex            Height             %hgt   %nint     %int    %int2    ",
 " Race    : %race           Weight             %wgt   %nwis     %wis    %wis2    ",
 " Class   : %clas           Social Class        %sc   %ndex     %dex    %dex2    ",
 "                                                     %ncon     %con    %con2    ",
 "                                                     %nchr     %chr    %chr2    ",
 "                                                                                ",
 " Max Hit Points     %mhp   Level              %lev   Max SP (Mana)      %msp    ",
 " Cur Hit Points     %chp   Experience         %exp   Cur SP (Mana)      %csp    ",
 "                           Max Exp           %mexp                              ",
 "        (Fighting)         Exp to Adv.        %adv          (Shooting)          ",
 " Blows/Round       %blow   Gold                %au   Shots/Round       %shot    ",
 " + to Skill        %fskl                             + to Skill        %sskl    ",
 " Deadliness (%)    %fded   Base AC/+ To AC %ac/%ta   Deadliness (%)    %sded    ",
 "                                                                                ",
 "                            (Character Abilities)                               ",
 " Fighting    : %figt       Stealth     : %stel       Disarming   : %disr        ",
 " Bows/Throw  : %bows       Perception  : %perc       Magic Device: %magd        ",
 " Saving Throw: %save       Searching   : %srch       Infra-Vision: %infr        ",
 "                                                                                ",
 "                            (Character Background)                              ",
 "          %hist                                                                 ",
 "                                                                                ",
 "                                                                                ",
 "                                                                                ",
NULL
};

#endif /* OANGBANDTK */

#if defined(ZANGBANDTK)

/*01234567890123456789012345678901234567890123456789012345678901234567890123456789*/
static char *s_layout[] = {
 "  [ZAngbandTk %vers Character Dump]                                             ",
 "                                                                                ",
 " Name     %name           Age      %age          Self  RB  CB  EB   Best        ",
 " Sex      %sex            Height   %hgt   %nstr   %s1 %s2 %s3 %s4    %s5    %s6 ",
 " Race     %race           Weight   %wgt   %nint   %i1 %i2 %i3 %i4    %i5    %i6 ",
 " Class    %class          Status    %sc   %nwis   %w1 %w2 %w3 %w4    %w5    %w6 ",
 " Title    %title          HP        %hp   %ndex   %d1 %d2 %d3 %d4    %d5    %d6 ",
 " Realm 1  %realm1         SP        %sp   %ncon   %c1 %c2 %c3 %c4    %c5    %c6 ",
 " Realm 2  %realm2         Maximize %max   %nchr   %h1 %h2 %h3 %h4    %h5    %h6 ",
 " Patron   %patron         Preserve %prv                                         ",
 "                                                                                ",
 " Level         %lev       Fighting               Saving Throw      %save        ",
 " Cur Exp       %exp       + to Skill   %fskl     Stealth           %stel        ",
 " Max Exp      %mexp       Deadliness   %fded     Fighting          %figt        ",
 " Adv Exp       %adv       Blows/Round  %blow     Shooting          %bows        ",
 "                                                 Disarming         %disr        ",
 " Gold           %au       Shooting               Magic Device      %magd        ",
 "                          + to Skill   %sskl     Perception        %perc        ",
 " Armor       %armor       Deadliness   %sded     Searching         %srch        ",
 " Burden     %burden       Shots/Round  %shot     Infravision      %infra        ",
 "                                                                                ",
 "     %hist                                                                      ",
 "                                                                                ",
 "                                                                                ",
 "                                                                                ",
NULL
};

#endif /* ZANGBANDTK */

static char **s_buffer;

/*
 * Searches for the symbol "sym" in s_buffer. If found, the symbol
 * (and the leading % char) are replaced with spaces.
 */
static int find_sym(cptr sym)
{
	char *p;
	int i, j;
	int len = strlen(sym);

	for (i = 0; s_buffer[i]; i++)
	{
		p = s_buffer[i];
		for (j = 0; p[j]; j++)
		{
			if (p[j] != '%') continue;
			if (strncmp(&p[j + 1], sym, len)) continue;
			if ((p[j + 1 + len] != ' ') && (p[j + 1 + len] != '/')) continue;
			(void) memset(p + j, ' ', len + 1);
			return i * 100 + j;
		}
	}
	return -1;
}

/*
 * Searches for the symbol "sym" in s_buffer. If found, the symbol
 * (and the leading % char) are replaced with "str". If "str" is
 * NULL then the symbol is simply replaced with spaces.
 */
static void replace_sym(cptr sym, cptr str, bool right)
{
	char *p;
	int i, n;

	i = find_sym(sym);
	
	if (str && (i != -1))
	{
		if (right) i -= strlen(str) - (strlen(sym) + 1);
		p = &s_buffer[i / 100][i % 100];
		n = strlen(str);
		memcpy(p, str, n);
	}
}

#if defined(OANGBANDTK)

errr file_character(cptr name, bool full)
{
	char *line[30], buf[30][90];
	FILE *fff;
	int i;

	int tmp;
	int xthn, xthb, xfos, xsrh;
	int xdis, xdev, xsav, xstl;

	int show_m_tohit = p_ptr->dis_to_h;
	int show_a_tohit = p_ptr->dis_to_h;
	int show_m_todam = p_ptr->dis_to_d;
	int show_a_todam = p_ptr->dis_to_d;

	object_type *o_ptr;
	char o_name[O_NAME_MAX];

	store_type *st_ptr = &store[STORE_HOME];

	int msg_max = message_num();

	/* Drop priv's */
	safe_setuid_drop();

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(name, "w");

	/* Grab priv's */
	safe_setuid_grab();

	/* Invalid file */
	if (!fff)
	{
		/* Error */
		return (-1);
	}

	for (i = 0; s_layout[i]; i++)
	{
		(void) strcpy(buf[i], s_layout[i]);
		line[i] = buf[i];
	}
	line[i] = NULL;
	s_buffer = line;

	replace_sym("vers", VERSION_STRING, FALSE);
	replace_sym("name", op_ptr->full_name, FALSE);
	replace_sym("sex", sp_ptr->title, FALSE);
	replace_sym("race", p_name + rp_ptr->name, FALSE);
	replace_sym("clas", cp_ptr->title, FALSE);

	/* Fighting Skill (with current weapon) */
	o_ptr = &inventory[INVEN_WIELD];
	tmp = p_ptr->to_h + o_ptr->to_h;
	xthn = p_ptr->skill_thn + (tmp * BTH_PLUS_ADJ);

	/* Shooting Skill (with current bow and normal missile) */
	o_ptr = &inventory[INVEN_BOW];
	tmp = p_ptr->to_h + o_ptr->to_h;
	xthb = p_ptr->skill_thb + (tmp * BTH_PLUS_ADJ);

	/* Basic abilities */
	xdis = p_ptr->skill_dis;
	xdev = p_ptr->skill_dev;
	xsav = p_ptr->skill_sav;
	xstl = p_ptr->skill_stl;
	xsrh = p_ptr->skill_srh;
	xfos = p_ptr->skill_fos;

	replace_sym("figt", likert(xthn, 10), FALSE);
	replace_sym("bows", likert(xthb, 10), FALSE);
	replace_sym("save", likert(xsav, 7), FALSE);
	replace_sym("stel", likert(xstl, 1), FALSE);
	replace_sym("perc", likert(xfos, 6), FALSE);
	replace_sym("srch", likert(xsrh, 6), FALSE);
	replace_sym("disr", likert(xdis, 8), FALSE);
	replace_sym("magd", likert(xdev, 8), FALSE);
	
	replace_sym("infr",
		use_metric ?
			format("%d meters", p_ptr->see_infra * 3) :
			format("%d feet", p_ptr->see_infra * 10)
		, FALSE);

	replace_sym("age", format("%d", (int) p_ptr->age), TRUE);
	if (use_metric)
	{
		replace_sym("hgt", format("%d", (int) p_ptr->ht * 254 / 100), TRUE);
		replace_sym("wgt", format("%d", (int) p_ptr->wt * 10 / 22), TRUE);
	}
	else
	{
		replace_sym("hgt", format("%d", (int) p_ptr->ht), TRUE);
		replace_sym("wgt", format("%d", (int) p_ptr->wt), TRUE);
	}
	replace_sym("sc", format("%d", (int) p_ptr->sc), TRUE);

	/* Display the stats */
	for (i = 0; i < 6; i++)
	{
		char buf[80];
		cptr stat_sym[6] = {"str", "int", "wis", "dex", "con", "chr"};
		cptr stat_sym_max[6] = {"str2", "int2", "wis2", "dex2", "con2", "chr2"};
		cptr stat_sym_name[6] = {"nstr", "nint", "nwis", "ndex", "ncon", "nchr"};
		
		/* Special treatment of "injured" stats */
		if (p_ptr->stat_cur[i] < p_ptr->stat_max[i])
		{
			int value;

			/* Use lowercase stat name */
			replace_sym(stat_sym_name[i], stat_names_reduced[i], FALSE);

			/* Get the current stat */
			value = p_ptr->stat_use[i];

			/* Obtain the current stat (modified) */
			cnv_stat(value, buf);

			/* Display the current stat (modified) */
			replace_sym(stat_sym[i], buf, TRUE);

			/* Acquire the max stat */
			value = p_ptr->stat_top[i];

			/* Obtain the maximum stat (modified) */
			cnv_stat(value, buf);

			/* Display the maximum stat (modified) */
			replace_sym(stat_sym_max[i], buf, TRUE);
		}

		/* Normal treatment of "normal" stats */
		else
		{
			/* Assume uppercase stat name */
			replace_sym(stat_sym_name[i], stat_names[i], FALSE);

			/* Obtain the current stat (modified) */
			cnv_stat(p_ptr->stat_use[i], buf);

			/* Display the current stat (modified) */
			replace_sym(stat_sym[i], buf, TRUE);

			/* Clear the maximum stat */
			replace_sym(stat_sym_max[i], NULL, TRUE);
		}
	}

	replace_sym("mhp", format("%d", (int) p_ptr->mhp), TRUE);
	replace_sym("chp", format("%d", (int) p_ptr->chp), TRUE);

	replace_sym("lev", format("%d", (int) p_ptr->lev), TRUE);
	replace_sym("exp", format("%d", (int) p_ptr->exp), TRUE);
	replace_sym("mexp", format("%d", (int) p_ptr->max_exp), TRUE);
	replace_sym("adv", format("%ld", (long)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L)), TRUE);
	replace_sym("au", format("%d", (int) p_ptr->au), TRUE);

	replace_sym("msp", format("%d", (int) p_ptr->msp), TRUE);
	replace_sym("csp", format("%d", (int) p_ptr->csp), TRUE);

	replace_sym("ac", format("%d", (int) p_ptr->dis_ac), TRUE);
	replace_sym("ta", format("%d", (int) p_ptr->dis_to_a), TRUE);

	o_ptr = &inventory[INVEN_WIELD];

	/* Hack -- add in weapon info if known */
	if (object_known_p(o_ptr)) show_m_tohit += o_ptr->to_h;
	if (object_known_p(o_ptr)) show_m_todam += o_ptr->to_d;

	replace_sym("blow", format("%d", (int) p_ptr->num_blow), TRUE);
	replace_sym("fskl", format("%d", (int) show_m_tohit), TRUE);
	replace_sym("fded", format("%d", deadliness_conversion[show_m_todam]), TRUE);

	o_ptr = &inventory[INVEN_BOW];

	/* Hack -- add in weapon info if known */
	if (object_known_p(o_ptr)) show_a_tohit += o_ptr->to_h;
	if (object_known_p(o_ptr)) show_a_todam += o_ptr->to_d;

	replace_sym("shot", format("%d", (int) p_ptr->num_fire), TRUE);
	replace_sym("sskl", format("%d", (int) show_a_tohit), TRUE);
	replace_sym("sded", format("%d", deadliness_conversion[show_a_todam]), TRUE);

	/* Character history */
	i = find_sym("hist");
	if (i != -1)
	{
		int j;
		for (j = 0; j < 4; j++)
		{
			char *p = &s_buffer[i / 100 + j][i % 100];
			int n = strlen(p_ptr->history[j]);
			(void) memcpy(p, p_ptr->history[j], n);
		}
	}

	/* Dump the entire buffer */
	for (i = 0; s_buffer[i]; i++)
	{
		char *p = s_buffer[i];
		int j = strlen(p) - 1;
		while ((j >= 0) && (p[j] == ' ')) p[j--] = '\0';
		fprintf(fff, "%s\n", p);
	}

	/* Skip some lines */
	fprintf(fff, "\n\n");


	/* Dump the equipment */
	if (p_ptr->equip_cnt)
	{
		fprintf(fff, "  [Character Equipment]\n\n");
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			o_ptr = &inventory[i];

			object_desc(o_name, o_ptr, TRUE, 3);
			fprintf(fff, "%c) %s\n", index_to_label(i), o_name);
		}
		fprintf(fff, "\n\n");
	}

	/* Dump the inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		/* Stop on real objects */
		if (inventory[i].k_idx) break;
	}
	if (i < INVEN_PACK)
	{
		fprintf(fff, "  [Character Inventory]\n\n");
		for (i = 0; i < INVEN_PACK; i++)
		{
			o_ptr = &inventory[i];
			if (!o_ptr->k_idx) break;

			object_desc(o_name, o_ptr, TRUE, 3);
			fprintf(fff, "%c) %s\n", index_to_label(i), o_name);
		}
		fprintf(fff, "\n\n");
	}

	/* Dump the Home */
	if (st_ptr->stock_num)
	{
		fprintf(fff, "  [Home Inventory]\n\n");
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			o_ptr = &st_ptr->stock[i];
			object_desc(o_name, o_ptr, TRUE, 3);
			fprintf(fff, "%c) %s\n", I2A(i), o_name);
		}
		fprintf(fff, "\n\n");
	}

	/* Dump options */
	fprintf(fff, "  [Options]\n\n");
	for (i = OPT_ADULT; i < OPT_score_end; i++)
	{
		if (option_desc[i])
		{
			fprintf(fff, "%-45s: %s (%s)\n", option_desc[i],
				op_ptr->opt[i] ? "yes" : "no ", option_text[i]);
		}
	}
	fprintf(fff, "\n\n");

	if (msg_max > 100)
		msg_max = 100;
	fprintf(fff, "  [Message Log (last %d messages)]\n\n", msg_max);
	for (i = msg_max - 1; i >= 0; i--)
	{
		fprintf(fff, "%s\n", message_str(i));
	}
	fprintf(fff, "\n\n");

	/* Close it */
	my_fclose(fff);

	return 0;
}

#endif /* OANGBANDTK */

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)

errr file_character(cptr name, bool full)
{
	char *line[30], buf[30][90];
	int i, tohit, todam;
	char o_name[O_NAME_MAX], tmp[80];
	FILE *fff = NULL;
	object_type *o_ptr;
	char version[20];

	int xthn, xthb, xfos, xsrh;
	int xdis, xdev, xsav, xstl;

#if defined(ANGBANDTK) || defined(KANGBANDTK)
	int k, info_length;
	cptr info[128];
	cptr blanks = "     ";
	s16b *stat_add = p_ptr->stat_add;
	store_type *st_ptr = &store[STORE_HOME];
#endif /* ANGBANDTK, KANGBANDTK */

#if defined(ZANGBANDTK)
	int j, stat_add[6];
	int percentdam;
	int blows, muta_att;
	int shots, shots_frac;
	object_type o_body;
#endif /* ZANGBANDTK */

	int msg_max = message_num();

	/* Drop priv's */
	safe_setuid_drop();

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(name, "w");

	/* Grab priv's */
	safe_setuid_grab();

	/* Invalid file */
	if (!fff)
	{
		/* Error */
		return (-1);
	}

	for (i = 0; s_layout[i]; i++)
	{
		(void) strcpy(buf[i], s_layout[i]);
		line[i] = buf[i];
	}
	line[i] = NULL;
	s_buffer = line;

#if defined(ANGBANDTK) || defined(KANGBANDTK)
	(void) sprintf(version, "%d.%d.%d", VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
	(void) sprintf(version, "%d.%d.%d", FAKE_VER_MAJOR, FAKE_VER_MINOR, FAKE_VER_PATCH);
#endif /* ZANGBANDTK */
	replace_sym("vers", version, FALSE);

	replace_sym("name", op_ptr_full_name, FALSE);
	replace_sym("sex", sp_ptr->title, FALSE);
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	replace_sym("race", p_name + rp_ptr->name, FALSE);
#endif
#if defined(ZANGBANDTK)
	replace_sym("race", rp_ptr->title, FALSE);
#endif
	replace_sym("class", cp_ptr->title, FALSE);
	replace_sym("title", player_title[p_ptr->pclass][(p_ptr->lev-1)/5], FALSE);
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	replace_sym("hp", format("%d/%d", p_ptr->chp, p_ptr->mhp), FALSE);
	replace_sym("sp", format("%d/%d", p_ptr->csp, p_ptr->msp), FALSE);
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
	replace_sym("realm1", realm_names[p_ptr->realm1], FALSE);
	replace_sym("realm2", realm_names[p_ptr->realm2], FALSE);
	replace_sym("patron", chaos_patrons[p_ptr->chaos_patron], FALSE);
	replace_sym("hp", format("%d/%d", p_ptr->chp, p_ptr->mhp), TRUE);
	replace_sym("sp", format("%d/%d", p_ptr->csp, p_ptr->msp), TRUE);
#endif /* ZANGBANDTK */
	replace_sym("age", format("%d", (int) p_ptr->age), TRUE);
	replace_sym("hgt", format("%d", (int) p_ptr->ht), TRUE);
	replace_sym("wgt", format("%d", (int) p_ptr->wt), TRUE);
	replace_sym("sc", format("%d", (int) p_ptr->sc), TRUE);
	replace_sym("max", p_ptr_maximize ? "Y" : "N", TRUE);
	replace_sym("prv", p_ptr_preserve ? "Y" : "N", TRUE);

#if defined(ZANGBANDTK)
	/* stat_add[] is not for equipment only in ZAngband */
	for (i = 0; i < 6; i++) stat_add[i] = 0;

	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];
		u32b f1, f2, f3;

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Affect stats */
		if (f1 & (TR1_STR)) stat_add[A_STR] += o_ptr->pval;
		if (f1 & (TR1_INT)) stat_add[A_INT] += o_ptr->pval;
		if (f1 & (TR1_WIS)) stat_add[A_WIS] += o_ptr->pval;
		if (f1 & (TR1_DEX)) stat_add[A_DEX] += o_ptr->pval;
		if (f1 & (TR1_CON)) stat_add[A_CON] += o_ptr->pval;
		if (f1 & (TR1_CHR)) stat_add[A_CHR] += o_ptr->pval;
	}
#endif /* ZANGBANDTK */

	for (i = 0; i < 6; i++)
	{
		char sym[3] = "  "; /* s1, s2, s3, etc */
		char *stat_char = "siwdch";
		cptr stat_sym_name[6] = {"nstr", "nint", "nwis", "ndex", "ncon", "nchr"};

		/* Reduced */
		if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
		{
			/* Use lowercase stat name */
			replace_sym(stat_sym_name[i], stat_names_reduced[i], FALSE);
		}

		/* Normal */
		else
		{
			/* Assume uppercase stat name */
			replace_sym(stat_sym_name[i], stat_names[i], FALSE);
		}

		/* s1, s2, s3, etc */
		sym[0] = stat_char[i];

		/* Internal "natural" maximum value */
		sym[1] = '1';
		cnv_stat(p_ptr->stat_max[i], tmp);
		replace_sym(sym, tmp, TRUE);

		/* Race Bonus */
		sym[1] = '2';
		replace_sym(sym, format("%+3d", rp_ptr->r_adj[i]), TRUE);

		/* Class Bonus */
		sym[1] = '3';
		replace_sym(sym, format("%+3d", cp_ptr->c_adj[i]), TRUE);

		/* Equipment Bonus */
		sym[1] = '4';
		if (stat_add[i] > 99)
		{
			replace_sym(sym, "+++", TRUE);
		}
		else
		{
			replace_sym(sym, format("%+3d", stat_add[i]), TRUE);
		}
		
		/* Resulting "modified" maximum value */
		sym[1] = '5';
		cnv_stat(p_ptr->stat_top[i], tmp);
		replace_sym(sym, tmp, TRUE);

		/* Only display stat_use if not maximal */
		sym[1] = '6';
		if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
		{
			cnv_stat(p_ptr->stat_use[i], tmp);
			replace_sym(sym, tmp, TRUE);
		}
		else
		{
			replace_sym(sym, "", TRUE);
		}
	}

	replace_sym("lev", format("%d", (int) p_ptr->lev), TRUE);
	replace_sym("exp", format("%d", (int) p_ptr->exp), TRUE);
	replace_sym("mexp", format("%d", (int) p_ptr->max_exp), TRUE);

	if (p_ptr->lev < PY_MAX_LEVEL)
	{
		s32b advance = (player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L);
		replace_sym("adv", format("%ld", (long) advance), TRUE);
	}
	else
	{
		replace_sym("adv", "********", TRUE);
	}

	replace_sym("au", format("%d", (int) p_ptr->au), TRUE);
	replace_sym("burden", format("%d.%d lbs", p_ptr->total_weight / 10,
		p_ptr->total_weight % 10), TRUE);

	replace_sym("armor", format("[%d,%+d]", (int) p_ptr->dis_ac,
		(int) p_ptr->dis_to_a), TRUE);
	replace_sym("fight", format("(%+d,%+d)", (int) p_ptr->dis_to_h,
		(int) p_ptr->dis_to_d), TRUE);

	/* Melee */
	tohit = p_ptr->dis_to_h;
	todam = p_ptr->dis_to_d;
	o_ptr = &inventory[INVEN_WIELD];
	if (object_known_p(o_ptr)) tohit += o_ptr->to_h;
	if (object_known_p(o_ptr)) todam += o_ptr->to_d;
	replace_sym("melee", format("(%+d,%+d)", tohit, todam), TRUE);

#if defined(ZANGBANDTK)
	replace_sym("fskl", format("%d", (int) tohit), TRUE);
	if (todam > 0)
	{
		percentdam = 100 + deadliness_conversion[todam];
	}
	else if (todam > -31)
	{
		percentdam = 100 - deadliness_conversion[ABS(todam)];
	}
	else
	{
		percentdam = 0;
	}
	replace_sym("fded", format("%d%%", percentdam), TRUE);

	blows_per_round(&blows, &muta_att);
	replace_sym("blow", format(muta_att ? "%d+%d" : "%d", blows, muta_att), TRUE);
#endif

	/* Shoot */
	tohit = p_ptr->dis_to_h;
	todam = p_ptr->dis_to_d;
	o_ptr = &inventory[INVEN_BOW];
	if (object_known_p(o_ptr)) tohit += o_ptr->to_h;
	if (object_known_p(o_ptr)) todam += o_ptr->to_d;
	replace_sym("shoot", format("(%+d,%+d)", tohit, todam), TRUE);

#if defined(ZANGBANDTK)
	replace_sym("sskl", format("%d", (int) tohit), TRUE);
	if (todam > 0)
	{
		percentdam = 100 + deadliness_conversion[todam];
	}
	else if (todam > -31)
	{
		percentdam = 100 - deadliness_conversion[ABS(todam)];
	}
	else
	{
		percentdam = 0;
	}
	replace_sym("sded", format("%d%%", percentdam), TRUE);

	shots_per_round(&shots, &shots_frac);
	replace_sym("shot", format("%d.%d", shots, shots_frac), TRUE);
#endif

	replace_sym("blows", format("%d/turn", (int) p_ptr->num_blow), TRUE);
	replace_sym("shots", format("%d/turn", (int) p_ptr->num_fire), TRUE);
	replace_sym("infra", format("%d ft", (int) p_ptr->see_infra * 10), TRUE);

	/* Fighting Skill (with current weapon) */
	o_ptr = &inventory[INVEN_WIELD];
	i = p_ptr->to_h + o_ptr->to_h;
	xthn = p_ptr->skill_thn + (i * BTH_PLUS_ADJ);

	/* Shooting Skill (with current bow and normal missile) */
	o_ptr = &inventory[INVEN_BOW];
	i = p_ptr->to_h + o_ptr->to_h;
	xthb = p_ptr->skill_thb + (i * BTH_PLUS_ADJ);

	/* Basic abilities */
	xdis = p_ptr->skill_dis;
	xdev = p_ptr->skill_dev;
	xsav = p_ptr->skill_sav;
	xstl = p_ptr->skill_stl;
	xsrh = p_ptr->skill_srh;
	xfos = p_ptr->skill_fos;

	replace_sym("figt", likert(xthn, 12), TRUE);
	replace_sym("bows", likert(xthb, 12), TRUE);
	replace_sym("save", likert(xsav, 6), TRUE);
	replace_sym("stel", likert(xstl, 1), TRUE);
	replace_sym("perc", likert(xfos, 6), TRUE);
	replace_sym("srch", likert(xsrh, 6), TRUE);
	replace_sym("disr", likert(xdis, 8), TRUE);
	replace_sym("magd", likert(xdev, 6), TRUE);

	/* Character history */
	i = find_sym("hist");
	if (i != -1)
	{
		int j;
		for (j = 0; j < 4; j++)
		{
			char *p = &s_buffer[i / 100 + j][i % 100];
			int n = strlen(p_ptr_history[j]);
			(void) memcpy(p, p_ptr_history[j], n);
		}
	}

	/* Dump the entire buffer */
	for (i = 0; s_buffer[i]; i++)
	{
		char *p = s_buffer[i];
		int j = strlen(p) - 1;
		while ((j >= 0) && (p[j] == ' ')) p[j--] = '\0';
		fprintf(fff, "%s\n", p);
	}

#if defined(ZANGBANDTK)
	fprintf(fff, "\n\n  [Miscellaneous information]\n");
	fprintf(fff, "\n Autoscum:           %s", ironman_autoscum ? "ALWAYS" : (auto_scum ? "ON" : "OFF"));
	fprintf(fff, "\n Small Levels:       %s",
		ironman_small_levels ? "ALWAYS" :
		always_small_levels ? "ON" :
		small_levels ? "ENABLED" :
		"OFF");
	if (vanilla_town)
		fprintf(fff, "\n Vanilla Town:       ON");
	else if (lite_town)
		fprintf(fff, "\n Lite Town:          ON");
	if (ironman_shops)
		fprintf(fff, "\n No Shops:           ON");
	if (ironman_downward)
		fprintf(fff, "\n Diving only:        ON");
	fprintf(fff, "\n Arena Levels:       %s",
		ironman_empty_levels ? "ALWAYS" :
		empty_levels ? "ENABLED" :
		"OFF");
	fprintf(fff, "\n Hard Quests:        %s", ironman_hard_quests ? "ON" : "OFF");
	fprintf(fff, "\n Num. Random Quests: %d", number_of_quests());
	fprintf(fff, "\n Nightmare Mode:     %s", ironman_nightmare ? "ON" : "OFF");
	fprintf(fff, "\n Recall Depth:       Level %d (%d')\n", p_ptr->max_dlv,
		50 * p_ptr->max_dlv);

	if (noscore)
		fprintf(fff, "\n You have done something illegal.");

	if (stupid_monsters)
		fprintf(fff, "\n Your opponents are behaving stupidly.");

	if (munchkin_death)
		fprintf(fff, "\n You possess munchkinish power over death.");

	/* Monsters slain */
	{
		int k;
		s32b Total = 0;

		for (k = 1; k < max_r_idx; k++)
		{
			monster_race *r_ptr = &r_info[k];

			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				bool dead = (r_ptr->max_num == 0);
				if (dead)
				{
					Total++;
				}
			}
			else
			{
				s16b This = r_ptr->r_pkills;
				if (This > 0)
				{
					Total += This;
				}
			}
		}

		if (Total < 1)
			fprintf(fff,"\n You have defeated no enemies yet.\n");
		else if (Total == 1)
			fprintf(fff,"\n You have defeated one enemy.\n");
		else
		   fprintf(fff,"\n You have defeated %lu enemies.\n", Total);
	}

	fprintf(fff, "\n\n  [Virtues]\n\n");
	dump_virtues(fff);

    if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
    {
        fprintf(fff, "\n\n  [Mutations]\n\n");
        dump_mutations(fff);
    }
#endif /* ZANGBANDTK */

	/* Skip some lines */
	fprintf(fff, "\n\n");

	/* Dump the equipment */
	if (p_ptr_equip_cnt)
	{
		fprintf(fff, "  [Character Equipment]\n\n");
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			o_ptr = &inventory[i];
#if defined(ZANGBANDTK)
			if (p_ptr_is_dead)
			{
				object_copy(&o_body, o_ptr);
				o_ptr = &o_body;
				object_aware(o_ptr);
				object_known(o_ptr);
			}
#endif
			object_desc(o_name, o_ptr, TRUE, 3);
			fprintf(fff, "%c) %s\n", index_to_label(i), o_name);

#if defined(ANGBANDTK) || defined(KANGBANDTK)
			/* Describe random object attributes */
			info_length = identify_random_gen(o_ptr, info, 128);

			/* Write it */
			for (k = 0; k < info_length; k++)
			{
				fprintf(fff, "%s%s\n", blanks, info[k]);
			}
#endif /* A */
		}
		fprintf(fff, "\n\n");
	}

	/* Dump the inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		/* Stop on real objects */
		if (inventory[i].k_idx) break;
	}
	if (i < INVEN_PACK)
	{
		fprintf(fff, "  [Character Inventory]\n\n");
		for (i = 0; i < INVEN_PACK; i++)
		{
			o_ptr = &inventory[i];
			if (!inventory[i].k_idx) break;
#if defined(ZANGBANDTK)
			if (p_ptr_is_dead)
			{
				object_copy(&o_body, o_ptr);
				o_ptr = &o_body;
				object_aware(o_ptr);
				object_known(o_ptr);
			}
#endif
			object_desc(o_name, o_ptr, TRUE, 3);
			fprintf(fff, "%c) %s\n", index_to_label(i), o_name);

#if defined(ANGBANDTK) || defined(KANGBANDTK)
			/* Describe random object attributes */
			info_length = identify_random_gen(o_ptr, info, 128);

			/* Write it */
			for (k = 0; k < info_length; k++)
			{
				fprintf(fff, "%s%s\n", blanks, info[k]);
			}
#endif /* A, K */
		}
		fprintf(fff, "\n\n");
	}

#if defined(ANGBANDTK) || defined(KANGBANDTK)

	/* Dump the Home */
	if (st_ptr->stock_num)
	{
		fprintf(fff, "  [Home Inventory]\n\n");
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			o_ptr = &st_ptr->stock[i];

			object_desc(o_name, o_ptr, TRUE, 3);
			fprintf(fff, "%c) %s\n", I2A(i), o_name);

			/* Describe random object attributes */
			info_length = identify_random_gen(o_ptr, info, 128);

			/* Write it */
			for (k = 0; k < info_length; k++)
			{
				fprintf(fff, "%s%s\n", blanks, info[k]);
			}
		}
		fprintf(fff, "\n\n");
	}

	/* Dump options */
	fprintf(fff, "  [Options]\n\n");
	for (i = OPT_ADULT; i < OPT_MAX; i++)
	{
		if (option_desc[i])
		{
			fprintf(fff, "%-45s: %s (%s)\n", option_desc[i],
				op_ptr->opt[i] ? "yes" : "no ", option_text[i]);
		}
	}
	fprintf(fff, "\n\n");

#endif /* ANGBANDTK, KANGBANDTK */

#if defined(ZANGBANDTK)

	/* Init the wilderness */
	process_dungeon_file("w_info.txt", 0, 0, max_wild_y, max_wild_x);

	/* Print all homes in the different towns */
	for (j = 1; j < max_towns; j++)
	{
		store_type *st_ptr = &town[j].store[STORE_HOME];
		
		/* Ignore empty Home */
		if (!st_ptr->stock_num) continue;
		
		/* Header with name of the town */
		fprintf(fff, "  [Home Inventory - %s]\n\n", town[j].name);

		/* Dump all available items */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			o_ptr = &st_ptr->stock[i];
			if (p_ptr_is_dead)
			{
				object_copy(&o_body, o_ptr);
				o_ptr = &o_body;
				object_aware(o_ptr);
				object_known(o_ptr);
			}
			object_desc(o_name, o_ptr, TRUE, 3);
			fprintf(fff, "%c) %s\n", I2A(i), o_name);
		}

		/* Add an empty line */
		fprintf(fff, "\n\n");
	}
	
#endif /* ZANGBANDTK */

	if (msg_max > 100)
		msg_max = 100;
	fprintf(fff, "  [Message Log (last %d messages)]\n\n", msg_max);
	for (i = msg_max - 1; i >= 0; i--)
	{
		fprintf(fff, "%s\n", message_str(i));
	}
	fprintf(fff, "\n\n");

	/* Close it */
	my_fclose(fff);

	/* Success */
	return (0);
}

#endif /* ANGBANDTK, KANGBANDTK, ZANGBANDTK */
