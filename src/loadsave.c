/* File: loadsave.c */

/* Purpose: interact with savefiles. This file was made by
   unifying load2.c and save.c from the old codebase. Doing it
   this way makes maintenance easier and lets us share code. */

#include "angband.h"

/* Don't play with this yet */
/* #define BZ_SAVES */
#ifdef BZ_SAVES
#include <bzlib.h>
#endif /* BZ_SAVES */

static void do_byte(byte*, int);
static void do_u16b(u16b*, int);
static void do_s16b(s16b*, int);
static void do_u32b(u32b*, int);
static void do_s32b(s32b*, int);
static void do_string(char*, int, int);
static void do_lore(int, int);
static void do_monster(monster_type*, int);
static void do_randomizer(int flag);
static void do_spells(int, int);
static void note(cptr);
static void do_fate(int,int);
static void do_ghost(int);
static void do_item(object_type*,int);
static void do_options(int);
static bool do_store(store_type*, int);
static void do_messages(int flag);
static void do_xtra(int, int);
static bool do_savefile_aux(int); 
static void junkinit(void);
static void morejunk(void);
static bool do_inventory(int);
static bool do_dungeon(int);
static void do_grid(int);
static void my_sentinel(char*, u16b, int);

static void bz_done(int);
static void bz_prep(int);

static void do_ver_byte(byte*, u32b, byte, int);
static void do_ver_u16b(u16b*, u32b, u16b, int);
static void do_ver_s16b(s16b*, u32b, s16b, int);
static void do_ver_u32b(u32b*, u32b, u32b, int);
static void do_ver_s32b(s32b*, u32b, s32b, int);
static void do_ver_string(char*, int, u32b, char*, int);

static void skip_ver_byte(u32b, int);
static void skip_ver_u16b(u32b, int);
static void skip_ver_s16b(u32b, int);
static void skip_ver_u32b(u32b, int);
static void skip_ver_s32b(u32b, int);
static void skip_ver_string(u32b, int);

errr rd_savefile(void);

#ifdef SAFER_PANICS
bool panicload;
#endif

static FILE *fff; /* Local savefile ptr */

#ifdef BZ_SAVES
BZFILE* bzf;
int bzerr;
#endif /* BZ_SAVES */

/*
 * Basic byte-level reading from savefile. This provides a single point of
 * interface to the pseudoencryption that Pernangband (and Angband) uses.
 * I'm thinking about if it might be faster/better to modify all the do_*
 * functions to directly do this stuff -- it'd make the code somewhat
 * uglier to maintain, but concievably might be much faster. Or is it better
 * maybe to scrap the pseudoencryption entirely and adopt some other means
 * of obfuscation, should it still prove useful in any way? -- Improv
 */


static byte sf_get(void)
{
	byte c;

	/* Get a character, decode the value */
#ifndef BZ_SAVES
	c = getc(fff) & 0xFF;
#else
	BZ2_bzRead(&bzerr, bzf, &c, 1);

	if(bzerr != 0)
		{
		note(format("Compression error on read"));
		bz_done(LS_LOAD);
		printf("Died in sf_get\n");
		exit(0);
		}
#endif /* BZ_SAVES */
	/* Return the value */
	return (c);
}


static void sf_put(byte v)
{
#ifndef BZ_SAVES
	(void)putc((int)v, fff);
#else
	BZ2_bzWrite(&bzerr, bzf, &v, 1);
	if(bzerr != 0)
		{
		note(format("Compression error on write"));
		bz_done(LS_SAVE);
		printf("Died in sf_put\n");
		exit(0);
		}
#endif
}


static void bz_prep(int flag)
	/* Does nothing if BZ_SAVES isn't defined */
{
#ifdef BZ_SAVES
if(flag == LS_LOAD)
	{
	bzf = BZ2_bzReadOpen(&bzerr, fff, 0, 0, NULL, 0);
	}
if(flag == LS_SAVE)
	{
	bzf = BZ2_bzWriteOpen(&bzerr, fff, 9, 0, 30);
	}
if(bzerr == 0)
	{
	return; /* All is Good */
	}
	/* Otherwise, all is bad */
note(format("Compression error on prep"));
printf("Died in bz_prep\n");
exit(0);
bz_done(flag);
#endif /* BZ_SAVES */
}

static void bz_done(int flag)
{
#ifdef BZ_SAVES
if(flag == LS_LOAD)
	{
	BZ2_bzReadClose(&bzerr, bzf);
	}
if(flag == LS_SAVE)
	{
	BZ2_bzWriteClose(&bzerr, bzf, 0, NULL, NULL);
	}
#endif /* BZ_SAVES */ 
}

/*
 * Do object memory and similar stuff
 */
static void do_xtra(int k_idx, int flag)
{
byte tmp8u = 0;
object_kind *k_ptr = &k_info[k_idx];
if(flag == LS_SAVE)
	{
	if (k_ptr->aware) tmp8u |= 0x01;
	if (k_ptr->tried) tmp8u |= 0x02;
	if (k_ptr->know)  tmp8u |= 0x04;
	if (k_ptr->squeltch == 1)  tmp8u |= 0x08;
	if (k_ptr->squeltch == 2)  tmp8u |= 0x10;
	if (k_ptr->squeltch == 3)  tmp8u |= 0x20;
	if (k_ptr->squeltch == 4)  tmp8u |= 0x40;
	if (k_ptr->artifact) tmp8u |= 0x80;
	do_byte(&tmp8u, flag);
	}
if(flag == LS_LOAD)
	{
	do_byte(&tmp8u, flag);
	k_ptr->aware = (tmp8u & 0x01)? TRUE:FALSE;
	k_ptr->tried = (tmp8u & 0x02)? TRUE:FALSE;
	k_ptr->know  = (tmp8u & 0x04)? TRUE:FALSE;
	if(tmp8u & 0x08) k_ptr->squeltch = 1;
	if(tmp8u & 0x10) k_ptr->squeltch = 2;
	if(tmp8u & 0x20) k_ptr->squeltch = 3;
	if(tmp8u & 0x40) k_ptr->squeltch = 4;
	k_ptr->artifact = (tmp8u & 0x80)? TRUE:FALSE;
	}
}

/*
 * Load/Save quick start data
 */
void do_quick_start(int flag)
{
        int i;

        do_ver_s16b(&previous_char.sex, 29, 0, flag);
        do_ver_s16b(&previous_char.race, 29, 0, flag);
        do_ver_s16b(&previous_char.rmod, 29, 0, flag);
        do_ver_s16b(&previous_char.class, 29, 0, flag);
        do_ver_byte(&previous_char.quests, 29, 0, flag);
        do_ver_s16b(&previous_char.realm1, 29, 0, flag);
        do_ver_s16b(&previous_char.realm2, 29, 0, flag);
        do_ver_byte(&previous_char.god, 29, 0, flag);
        do_ver_s32b(&previous_char.grace, 29, 0, flag);
        do_ver_s32b(&previous_char.god_favor, 29, 0, flag);
        do_ver_s16b(&previous_char.age, 29, 0, flag);
        do_ver_s16b(&previous_char.wt, 29, 0, flag);
        do_ver_s16b(&previous_char.ht, 29, 0, flag);
        do_ver_s16b(&previous_char.sc, 29, 0, flag);
        do_ver_s32b(&previous_char.au, 29, 0, flag);

        for (i = 0; i < 6; i++) do_ver_s16b(&(previous_char.stat[i]), 29, 0, flag);

        do_ver_s16b(&previous_char.chaos_patron, 29, 0, flag);
        do_ver_u32b(&previous_char.weapon, 29, 0, flag);
        do_ver_byte(&previous_char.quick_ok, 29, 0, flag);

        for (i = 0; i < 4; i++) do_ver_string(previous_char.history[i], 60, 29, "", flag);
}

/*
 * Misc. other data
 */
static void do_extra(int flag)
{
	int i, j;
	byte tmp8u;
	s16b tmp16s;

	do_string(player_name, 32, flag);

	do_string(died_from, 80, flag);

	for (i = 0; i < 4; i++)
	{
		do_string(history[i], 60, flag);
	}

	/* Handle the special levels info */
	if(flag == LS_SAVE)
		{
		tmp8u = max_d_idx;
		tmp16s = MAX_DUNGEON_DEPTH;
		}
	do_byte(&tmp8u,flag);
	if(flag == LS_LOAD)
		{
		if( tmp8u > max_d_idx)
			note(format("Too many (%d) dungeon types!", tmp8u));
		}
	do_s16b(&tmp16s,flag);
	if(flag == LS_LOAD)
		{
		if( tmp16s > MAX_DUNGEON_DEPTH)
			note(format("Too many (%d) max level by dungeon type!", tmp16s));
		}

        for (i = 0; i < tmp8u; i++)
                for (j = 0; j < tmp16s; j++)
                        skip_ver_byte(21, flag);

        /* Load the special levels history */
        for (i = 0; i < 1 + (max_d_idx * MAX_DUNGEON_DEPTH / 32); i++)
        {
                skip_ver_s32b(25, flag);
        }

        /* Load the special levels history */
        for (i = 0; i < max_d_idx; i++)
        for (j = 0; j < MAX_DUNGEON_DEPTH; j++)
        {
                do_ver_byte(&special_lvl[j][i], 26, 0, flag);
        }

        /* Load the quick start data */
        do_quick_start(flag);

	/* Race/Class/Gender/Spells */
	do_byte(&p_ptr->prace, flag);
	do_byte(&p_ptr->pracem, flag);
	do_byte(&p_ptr->pclass, flag);
	do_byte(&p_ptr->psex, flag);
	do_u16b(&p_ptr->realm1, flag);
	do_u16b(&p_ptr->realm2, flag);
	do_byte(&p_ptr->mimic_form, flag);
	if(flag == LS_SAVE) tmp8u = 0;

	do_byte(&p_ptr->hitdie, flag);
	do_u16b(&p_ptr->expfact, flag);

	do_s16b(&p_ptr->age, flag);
	do_s16b(&p_ptr->ht, flag);
	do_s16b(&p_ptr->wt, flag);

	/* Dump the stats (maximum and current) */
	for (i = 0; i < 6; ++i) do_s16b(&p_ptr->stat_max[i], flag);
	for (i = 0; i < 6; ++i) do_s16b(&p_ptr->stat_cur[i], flag);
	for (i = 0; i < 6; ++i) do_s16b(&p_ptr->stat_cnt[i], flag);
	for (i = 0; i < 6; ++i) do_s16b(&p_ptr->stat_los[i], flag);

	tmp16s = 0;
	for(i = 0; i < 12; ++i) do_s16b(&tmp16s, flag); /* Transient */

	do_u32b(&p_ptr->au, flag);

	do_u32b(&p_ptr->max_exp, flag);
	do_u32b(&p_ptr->exp, flag);
	do_u16b(&p_ptr->exp_frac, flag);
	do_s16b(&p_ptr->lev, flag);

	do_s16b(&p_ptr->town_num, flag); /* -KMW- */

	/* Write arena and rewards information -KMW- */
	do_s16b(&p_ptr->arena_number, flag);
	do_s16b(&p_ptr->inside_arena, flag);
	do_s16b(&p_ptr->inside_quest, flag);
	do_byte(&p_ptr->exit_bldg, flag);

	do_byte(&tmp8u, flag); /* tmp8u should be 0 at this point */

	if(flag == LS_SAVE) tmp8u = MAX_PLOTS;
	do_byte(&tmp8u, flag);
	if( (flag == LS_LOAD) && (tmp8u > MAX_PLOTS)) quit("Too many plots");
	for (i = 0; i < tmp8u; i++)
		do_s16b(&plots[i], flag);

        if(flag == LS_SAVE)
        {
                tmp8u = MAX_RANDOM_QUEST;
        }
	do_byte(&tmp8u, flag);

	if( (flag == LS_SAVE) && (tmp8u > MAX_RANDOM_QUEST) ) quit("Too many random quests");
	for (i = 0; i < tmp8u; i++)
        {
		do_byte(&random_quests[i].type, flag);
		do_s16b(&random_quests[i].r_idx, flag);
		do_byte(&random_quests[i].done, flag);
        }

	do_s16b(&p_ptr->oldpx, flag);
	do_s16b(&p_ptr->oldpy, flag);

	/* Save builing rewards */
	if(flag == LS_SAVE) tmp16s = MAX_BACT;
	do_s16b(&tmp16s, flag);
	if((flag == LS_LOAD) && (tmp16s > MAX_BACT) ) /* Check validity */
		note(format("Too many (%d) building rewards!", tmp16s));

	for (i = 0; i < tmp16s; i++) do_s16b(&p_ptr->rewards[i], flag);

	do_s16b(&p_ptr->mhp, flag);
	do_s16b(&p_ptr->chp, flag);
	do_u16b(&p_ptr->chp_frac, flag);
	do_s16b(&p_ptr->hp_mod, flag);

	do_s16b(&p_ptr->msane, flag);
	do_s16b(&p_ptr->csane, flag);
	do_u16b(&p_ptr->csane_frac, flag);

	do_s16b(&p_ptr->msp, flag);
	do_s16b(&p_ptr->csp, flag);
	do_u16b(&p_ptr->csp_frac, flag);

	do_s16b(&p_ptr->mtp, flag);
	do_s16b(&p_ptr->ctp, flag);
	do_s16b(&p_ptr->tp_aux1, flag);
	do_s16b(&p_ptr->tp_aux2, flag);

	/* Gods */
	do_s32b(&p_ptr->grace, flag);
	do_s32b(&p_ptr->god_favor, flag);
	do_byte(&p_ptr->pgod, flag);

	/* Max Player and Dungeon Levels */
	do_s16b(&p_ptr->max_plv, flag);

	tmp8u = max_d_idx;
	do_byte(&tmp8u, flag);
	for (i = 0; i < tmp8u; i++)
		do_s16b(&max_dlv[i], flag);

		/* Repair max player level??? */
	if((flag == LS_LOAD) && (p_ptr->max_plv < p_ptr->lev))
		p_ptr->max_plv = p_ptr->lev;

	do_ver_byte(&(p_ptr->help.enabled), 15, TRUE, flag);
	do_ver_s32b(&(p_ptr->help.help1), 15, 0, flag);

	/* More info */
	tmp16s = 0;
	do_s16b(&p_ptr->sc, flag);
	do_s16b(&p_ptr->blind, flag);
	do_s16b(&p_ptr->paralyzed, flag);
	do_s16b(&p_ptr->confused, flag);
	do_s16b(&p_ptr->food, flag);
	do_s32b(&p_ptr->energy, flag);
	do_s16b(&p_ptr->fast, flag);
	do_s16b(&p_ptr->slow, flag);
	do_s16b(&p_ptr->afraid, flag);
	do_s16b(&p_ptr->cut, flag);
	do_s16b(&p_ptr->stun, flag);
	do_s16b(&p_ptr->poisoned, flag);
	do_s16b(&p_ptr->image, flag);
	do_s16b(&p_ptr->protevil, flag);
	do_s16b(&p_ptr->protundead, flag);
	do_s16b(&p_ptr->invuln, flag);
	do_s16b(&p_ptr->hero, flag);
	do_s16b(&p_ptr->shero, flag);
	do_s16b(&p_ptr->shield, flag);
	do_s16b(&p_ptr->shield_power, flag);
	do_s16b(&p_ptr->shield_opt, flag);
	do_s16b(&p_ptr->blessed, flag);
	do_s16b(&p_ptr->tim_invis, flag);
	do_s16b(&p_ptr->word_recall, flag);
	do_s16b(&p_ptr->recall_dungeon, flag);
	do_s16b(&p_ptr->see_infra, flag);
	do_s16b(&p_ptr->tim_infra, flag);
	do_s16b(&p_ptr->oppose_fire, flag);
	do_s16b(&p_ptr->oppose_cold, flag);
	do_s16b(&p_ptr->oppose_acid, flag);
	do_s16b(&p_ptr->oppose_elec, flag);
	do_s16b(&p_ptr->oppose_pois, flag);
	do_s16b(&p_ptr->oppose_ld, flag);
	do_s16b(&p_ptr->oppose_cc, flag);
	do_s16b(&p_ptr->oppose_ss, flag);
	do_s16b(&p_ptr->oppose_nex, flag);

	do_s16b(&p_ptr->tim_esp, flag);
	do_s16b(&p_ptr->tim_wraith, flag);
	do_s16b(&p_ptr->tim_ffall, flag);
	do_s16b(&p_ptr->tim_fire_aura, flag);
	do_s16b(&p_ptr->resist_magic, flag);
	do_s16b(&p_ptr->tim_invisible, flag);
	do_s16b(&p_ptr->tim_inv_pow, flag);
	do_s16b(&p_ptr->tim_mimic, flag);
	do_s16b(&p_ptr->lightspeed, flag);
	do_s16b(&p_ptr->tim_lite, flag);
	do_s16b(&p_ptr->holy, flag);
	do_s16b(&p_ptr->walk_water, flag);
	do_s16b(&p_ptr->tim_mental_barrier, flag);
	do_s16b(&p_ptr->immov_cntr, flag);
	do_s16b(&p_ptr->strike, flag);
	do_s16b(&p_ptr->meditation, flag);
	do_s16b(&p_ptr->tim_reflect, flag);
	do_s16b(&p_ptr->tim_res_time, flag);
	do_s16b(&p_ptr->tim_deadly, flag);
	do_s16b(&p_ptr->prob_travel, flag);
	do_s16b(&p_ptr->disrupt_shield, flag);
        do_s16b(&p_ptr->parasite, flag);
        do_s16b(&p_ptr->parasite_r_idx, flag);
        do_ver_u32b(&p_ptr->loan, 19, 0, flag);
        do_ver_u32b(&p_ptr->loan_time, 19, 0, flag);

	do_s16b(&p_ptr->chaos_patron, flag);
	do_u32b(&p_ptr->muta1, flag);
	do_u32b(&p_ptr->muta2, flag);
	do_u32b(&p_ptr->muta3, flag);

	do_byte(&p_ptr->confusing, flag);
	do_byte(&p_ptr->black_breath, flag);
	do_byte(&fate_flag, flag);
	do_byte(&p_ptr->searching, flag);
	do_byte(&p_ptr->maximize, flag);
	do_byte(&p_ptr->preserve, flag);
	do_byte(&p_ptr->special, flag);
        skip_ver_byte(24, flag);
        do_ver_byte(&ambush_flag, 20, FALSE, flag);
	do_byte(&p_ptr->allow_one_death, flag);
	do_s16b(&p_ptr->xtra_spells, flag);

	do_byte(&vanilla_town, flag);

	do_u16b(&no_breeds, flag);
	do_s16b(&p_ptr->protgood, flag);

	/* Auxilliary variables */
	do_u32b(&p_ptr->class_extra1, flag);
	do_u32b(&p_ptr->class_extra2, flag);
	do_u32b(&p_ptr->class_extra3, flag);
	do_u32b(&p_ptr->class_extra4, flag);
	do_u32b(&p_ptr->class_extra5, flag);
	do_u32b(&p_ptr->class_extra6, flag);
	do_u32b(&p_ptr->class_extra7, flag);

	do_u32b(&p_ptr->race_extra1, flag);
	do_u32b(&p_ptr->race_extra2, flag);
	do_u32b(&p_ptr->race_extra3, flag);
	do_u32b(&p_ptr->race_extra4, flag);
	do_u32b(&p_ptr->race_extra5, flag);
	do_u32b(&p_ptr->race_extra6, flag);
	do_u32b(&p_ptr->race_extra7, flag);

	do_u16b(&p_ptr->body_monster, flag);
	do_byte(&p_ptr->disembodied, flag);

	/* Are we in astral mode? */
	do_byte(&p_ptr->astral, flag);

	if(flag == LS_SAVE) tmp16s = POWER_MAX;
	do_s16b(&tmp16s, flag);
	if((flag == LS_LOAD) && (tmp16s > POWER_MAX) )
		note(format("Too many (%u) powers!", tmp16s));
	if(flag == LS_SAVE) tmp16s = POWER_SLOT;
	if(flag == LS_LOAD) tmp16s = (tmp16s/32)+1;
	for(i=0; i < tmp16s; i++)
		do_s32b(&p_ptr->powers_mod[i], flag);

	/* The music */
	do_byte(&p_ptr->music, flag);

	/* The tactic */
	do_byte(&p_ptr->tactic, flag);

	/* The movement */
	do_byte(&p_ptr->movement, flag);

	/* The comapnions killed */
	do_s16b(&p_ptr->companion_killed, flag);

	/* The fate */
	do_byte(&p_ptr->no_mortal, flag);

	/* The bounties */
	for (i = 0; i < MAX_BOUNTIES; i++) 
		{
		do_s16b(&bounties[i][0], flag);
		do_s16b(&bounties[i][1], flag);
		}
	do_u32b(&total_bounties, flag);
	do_s16b(&spell_num, flag);
	for(i = 0; i < MAX_SPELLS; i++)
		do_spells(i, flag);
	do_s16b(&rune_num, flag);
	for (i = 0; i < MAX_RUNES; i++)
		{
		do_string(rune_spells[i].name, 30, flag); 
		do_s16b(&rune_spells[i].type, flag);
		do_s16b(&rune_spells[i].rune2, flag);
		do_s16b(&rune_spells[i].mana, flag);
		}

	/* Write the "object seeds" */
	do_u32b(&seed_dungeon, flag);
	do_u32b(&seed_flavor, flag);
	do_u32b(&seed_town, flag);

	/* Special stuff */
	do_u16b(&panic_save, flag);
	do_u16b(&total_winner, flag);
	do_u16b(&noscore, flag);

	/* Write death */
	if(flag == LS_SAVE) tmp8u = death;
	do_byte(&tmp8u, flag);
	if(flag == LS_LOAD) death = tmp8u;

	/* Write feeling */
	if(flag == LS_SAVE) tmp8u = feeling;
	do_byte(&tmp8u, flag);
	if(flag == LS_LOAD) feeling = tmp8u;

	/* Turn of last "feeling" */
	do_s32b(&old_turn, flag);

	/* Current turn */
	do_s32b(&turn, flag);

}

/* Save the current persistent dungeon -SC- */
void save_dungeon(void)
{
	char tmp[16];
	char name[1024], buf[5];

	/* Save only persistent dungeons */
	if (!get_dungeon_save(buf) || (!dun_level)) return;

	/* Construct filename */
	sprintf(tmp, "%s.%s", player_base, buf);
	path_build(name, 1024, ANGBAND_DIR_SAVE, tmp);
   
	/* Open the file */
	safe_setuid_grab();
	fff = my_fopen(name, "wb");
	bz_prep(LS_SAVE);
	safe_setuid_drop();

	/* Save the dungeon */
	do_dungeon(LS_SAVE);

	/* Done */
	bz_done(LS_SAVE);
	my_fclose(fff);
}

/*
 * Medium level player saver
 *
 */
static bool save_player_aux(char *name)
{
	bool ok = FALSE;
	int fd = -1;
	int mode = 0644;

	/* No file yet */
	fff = NULL;

	/* File type is "SAVE" */
	FILE_TYPE(FILE_TYPE_SAVE);

	/* Create the savefile */
	safe_setuid_grab();
	fd = fd_make(name, mode);
	safe_setuid_drop();

	/* File is okay */
	if (fd >= 0)
	{
		/* Close the "fd" */
		(void)fd_close(fd);

		/* Open the savefile */
		safe_setuid_grab();
		fff = my_fopen(name, "wb");
		bz_prep(LS_SAVE);

		/* Successful open */
		if (fff)
		{
			bz_prep(LS_SAVE);
			safe_setuid_drop();
			/* Write the savefile */
			if (do_savefile_aux(LS_SAVE)) ok = TRUE;

			bz_done(LS_SAVE);
			/* Attempt to close it */
			if (my_fclose(fff)) ok = FALSE;
		}
		/* Remove "broken" files */
		safe_setuid_grab();
		if (!ok) (void)fd_kill(name);
		safe_setuid_drop();
	}

	/* Failure */
	if (!ok) return (FALSE);

	/* Successful save */
	character_saved = TRUE;

	/* Success */
	return (TRUE);
}

/*
 * Attempt to save the player in a savefile
 */
bool save_player(void)
{
	int result = FALSE;
	char safe[1024];
#ifdef SAFER_PANICS
	char panicsave[1024];
#endif /* SAFER PANICS */


#ifdef SET_UID

#ifdef SECURE

	/* Get "games" permissions */
	beGames();

#endif

#endif

#ifdef SAFER_PANICS
	if(panic_save)
		{
		strcpy(panicsave, savefile);
		strcat(panicsave, ".pnc"); /* Not sure how to do this so it's
					nicely portable to brain-damaged OS's
					with short filenames */
		safe_setuid_grab();
		fd_kill(panicsave);     /* Remove any old panic saves */
		safe_setuid_drop();
		save_player_aux(panicsave);
#ifdef SECURE
		/* Drop "games" permissions */
		bePlayer();
#endif /* SECURE */
		return TRUE;
		}
#endif /* SAFER_PANICS */


	/* New savefile */
	strcpy(safe, savefile);
	strcat(safe, ".new");

#ifdef VM
	/* Hack -- support "flat directory" usage on VM/ESA */
	strcpy(safe, savefile);
	strcat(safe, "n");
#endif /* VM */

	/* Remove it */
	safe_setuid_grab();
	fd_kill(safe);
	safe_setuid_drop();

	/* Attempt to save the player */
	if (save_player_aux(safe))
		{
		char temp[1024];

		/* Old savefile */
		strcpy(temp, savefile);
		strcat(temp, ".old");

#ifdef VM
		/* Hack -- support "flat directory" usage on VM/ESA */
		strcpy(temp, savefile);
		strcat(temp, "o");
#endif /* VM */

		/* Remove it */
		safe_setuid_grab();
		fd_kill(temp);
		/* Preserve old savefile */
		fd_move(savefile, temp);

		/* Activate new savefile */
		fd_move(safe, savefile);
		
		/* Remove preserved savefile */
		fd_kill(temp);
		safe_setuid_drop();

		/* Hack -- Pretend the character was loaded */
		character_loaded = TRUE;

#ifdef VERIFY_SAVEFILE

		/* Lock on savefile */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Remove lock file */
		safe_setuid_grab();
		fd_kill(temp);
		safe_setuid_drop();

#endif

		/* Success */
		result = TRUE;
		}


#ifdef SET_UID
#ifdef SECURE

	/* Drop "games" permissions */
	bePlayer();

#endif /* SECURE */
#endif /* SET_UID */

	save_savefile_names();

	/* Return the result */
	return (result);
}

static bool file_exist(char *buf)
{
        int fd;
        bool result;

#ifdef SET_UID
#ifdef SECURE
	/* Set "games" permissions */
	beGames();
#endif /* SECURE */
#endif /* SET_UID */

        safe_setuid_grab();
        fd = fd_open(buf, O_RDONLY);
        safe_setuid_drop();
        if(fd >= 0)
	{
                fd_close(fd);
                result = TRUE;
	}
        else result=FALSE;

#ifdef SET_UID
#ifdef SECURE
	/* Drop "games" permissions */
	bePlayer();
#endif /* SECURE */
#endif /* SET_UID */
	return result;
}

/*
 * Attempt to Load a "savefile"
 *
 * On multi-user systems, you may only "read" a savefile if you will be
 * allowed to "write" it later, this prevents painful situations in which
 * the player loads a savefile belonging to someone else, and then is not
 * allowed to save his game when he quits.
 *
 * We return "TRUE" if the savefile was usable, and we set the global
 * flag "character_loaded" if a real, living, character was loaded.
 *
 * Note that we always try to load the "current" savefile, even if
 * there is no such file, so we must check for "empty" savefile names.
 */
bool load_player(void)
{
	int fd = -1;

	errr err = 0;

#ifdef SAFER_PANICS
	char panic_fname[1024]; /* Filename for panic savefiles */
	int testfd = -1;
#endif /* SAFER_PANICS */

#ifdef VERIFY_TIMESTAMP
	struct stat statbuf;
#endif /* VERIFY_TIMESTAMP */

	cptr what = "generic";

#ifdef SAFER_PANICS
	panicload = FALSE;
	strncpy(panic_fname, savefile,1024);
	strcat(panic_fname, ".pnc"); /* This might concievably cause a buffer
					overflow, but the rest of the code
					in this file does likewise. If someone
					ever audits pernband for security
					problems, well, don't blame me. The rest
					of the code was like this before I even
					got here -- Pat */

#endif /* SAFER_PANICS */


	/* Paranoia */
	turn = 0;

	/* Paranoia */
	death = FALSE;


	/* Allow empty savefile name */
	if (!savefile[0]) return (TRUE);


	/* XXX XXX XXX Fix this */

	/* Verify the existance of the savefile */
	if ((!file_exist(savefile))
#ifdef SAFER_PANICS
	&& (!file_exist(panic_fname))
#endif /* SAFER_PANICS */
				)
	{
		/* Give a message */
		msg_format("Savefile does not exist: %s", savefile);
		msg_print(NULL);

		/* Allow this */
		return (TRUE);
	}


#ifdef VERIFY_SAVEFILE

	/* Verify savefile usage */
	if (!err)
	{
		FILE *fkk;

		char temp[1024];

		/* Extract name of lock file */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Check for lock */
		safe_setuid_grab();
		fkk = my_fopen(temp, "r");
		safe_setuid_drop();

		/* Oops, lock exists */
		if (fkk)
		{
			/* Close the file */
			my_fclose(fkk);

			/* Message */
			msg_print("Savefile is currently in use.");
			msg_print(NULL);

			/* Oops */
			return (FALSE);
		}

		/* Create a lock file */
		safe_setuid_grab();
		fkk = my_fopen(temp, "w");
		safe_setuid_drop();

		/* Dump a line of info */
		fprintf(fkk, "Lock file for savefile '%s'\n", savefile);

		/* Close the lock file */
		my_fclose(fkk);
	}

#endif


	/* Okay */
	if (!err)
	{
		/* Open the savefile */
		fd = fd_open(savefile, O_RDONLY);

		/* No file */
		if (fd < 0) err = -1;

		/* Message (below) */
		if (err) what = "Cannot open savefile";
	}

#ifdef SAFER_PANICS
	testfd = fd_open(panic_fname, O_RDONLY);
	fd_close(testfd);
	if (testfd > 0)  /* A panic save exists, which is not normally the case */
	{
		panicload = 1;
		fd_close(fd);
		fd = fd_open(panic_fname, O_RDONLY); /* Prefer panic saves
						over real saves */
		what = "";	/* This is not the error if we're at this pt */
		err = 0;
	}
#endif /* SAFER_PANICS */

	/* Process file */
	if (!err)
	{

#ifdef VERIFY_TIMESTAMP
		/* Get the timestamp */
		(void)fstat(fd, &statbuf);
#endif
		fff = fdopen(fd, "r");
		bz_prep(LS_LOAD);
		/* Read the first four bytes */
		do_u32b(&vernum, LS_LOAD);
		do_byte(&sf_extra, LS_LOAD);
		bz_done(LS_LOAD);
		fclose(fff);
		/* Close the file */
		fd_close(fd);
	}

	/* Process file */
	if (!err)
	{

		/* Extract version */
		sf_extra = 0;
		sf_major = 2;
		sf_minor = 8;
		sf_patch = 1;

		/* Clear screen */
		Term_clear();

		/* Attempt to load */
		err = rd_savefile();

		/* Message (below) */
		if (err) what = "Cannot parse savefile";
	}

	/* Paranoia */
	if (!err)
	{
		/* Invalid turn */
		if (!turn) err = -1;

		/* Message (below) */
		if (err) what = "Broken savefile";
	}

#ifdef VERIFY_TIMESTAMP
	/* Verify timestamp */
	if (!err && !arg_wizard)
	{
		/* Hack -- Verify the timestamp */
		if (sf_when > (statbuf.st_ctime + 100) ||
		    sf_when < (statbuf.st_ctime - 100))
		{
			/* Message */
			what = "Invalid timestamp";

			/* Oops */
			err = -1;
		}
	}
#endif


	/* Okay */
	if (!err)
	{
		/* Player is dead */
		if (death)
		{
			/* Player is no longer "dead" */
			death = FALSE;

			/* Cheat death */
			if (arg_wizard)
			{
				/* A character was loaded */
				character_loaded = TRUE;

				/* Done */
				return (TRUE);
			}

			/* Count lives */
			sf_lives++;

			/* Forget turns */
			turn = old_turn = 0;

			/* Done */
			return (TRUE);
		}

		/* A character was loaded */
		character_loaded = TRUE;

		/* Still alive */
		if (p_ptr->chp >= 0)
		{
			/* Reset cause of death */
			(void)strcpy(died_from, "(alive and well)");
		}

#ifdef SAFER_PANICS
		if(panicload)
			{
			safe_setuid_grab();
			fd_kill(panic_fname); /* Done loading, it'll either
					immediately panic and re-save, or
					we don't need the panicsave file
					anymore. Either way, it's safe to
					zap the original panicsave */
			safe_setuid_drop();
			}
#endif /* SAFER_PANICS */

		/* Success */
		return (TRUE);
	}


#ifdef VERIFY_SAVEFILE

	/* Verify savefile usage */
	if (TRUE)
	{
		char temp[1024];

		/* Extract name of lock file */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Remove lock */
		safe_setuid_grab();
		fd_kill(temp);
		safe_setuid_drop();
	}

#endif


	/* Message */
	msg_format("Error (%s) reading %d.%d.%d savefile.",
		   what, sf_major, sf_minor, sf_patch);
	msg_print(NULL);

	/* Oops */
	return (FALSE);
}


/* 
 * Size-aware read/write routines for the savefile, do all their
 * work through sf_get and sf_put. 
 */

static void do_byte(byte *v, int flag)
{
if(flag == LS_LOAD)
	{
	*v = sf_get();
	return;
	}
if(flag == LS_SAVE)
	{
	byte val = *v;
	sf_put(val); 
	return;
	}
/* We should never reach here, so bail out */
printf("FATAL: do_byte passed %d\n",flag);
exit(0);
}

static void do_u16b(u16b *v, int flag)
{
if(flag == LS_LOAD)
	{
	(*v) = sf_get();
	(*v) |= ((u16b)(sf_get()) << 8);
	return;
	}
if(flag == LS_SAVE)
	{
	u16b val;
	val = *v;
	sf_put((byte)(val & 0xFF));
	sf_put((byte)((val >> 8) & 0xFF));
	return;
	}
/* Never should reach here, bail out */
printf("FATAL: do_u16b passed %d\n",flag);
exit(0);
}

static void do_s16b(s16b *ip, int flag)
{
if(flag == LS_LOAD)
	{
	do_u16b( (u16b*)ip, flag);
	return;
	}
if(flag == LS_SAVE)
	{
	s16b val;
	val = *ip;
	do_u16b( (u16b*)ip, flag);
	return;
	}
/* Blah blah, never should reach here, die */
printf("FATAL: do_s16b passed %d\n",flag);
exit(0);
}

static void do_u32b(u32b *ip, int flag)
{
if(flag == LS_LOAD)
	{
	(*ip) = sf_get();
	(*ip) |= ((u32b)(sf_get()) << 8);
	(*ip) |= ((u32b)(sf_get()) << 16);
	(*ip) |= ((u32b)(sf_get()) << 24);
	return;
	}
if(flag == LS_SAVE)
	{
	u32b val = *ip;
	sf_put( (byte)(val & 0xFF));
	sf_put( (byte)((val >> 8) & 0xFF));
	sf_put( (byte)((val >> 16) & 0xFF));
	sf_put( (byte)((val >> 24) & 0xFF));
	return;
	}
/* Bad news if you're here, because you're going down */
printf("FATAL: do_u32b passed %d\n",flag);
exit(0);
}

static void do_s32b(s32b *ip, int flag)
{
if(flag == LS_LOAD)
	{
	do_u32b( (u32b*)ip, flag);
	return;
	}
if(flag == LS_SAVE)
	{
	do_u32b( (u32b*)ip, flag);
	return;
	}
/* Raus! Schnell! */
printf("FATAL: do_s32b passed %d\n",flag);
exit(0);
}

static void do_string(char* str, int max, int flag)
	/* Max is ignored for writing */
{
if(flag == LS_LOAD)
	{
	int i;

	/* Read the string */
	for (i = 0; TRUE; i++)
		{
		byte tmp8u;

		/* Read a byte */
		do_byte(&tmp8u, LS_LOAD);

		/* Collect string while legal */
		if (i < max) str[i] = tmp8u;

		/* End of string */
		if (!tmp8u) break;
		}
	/* Terminate */
	str[max-1] = '\0';
	return;
	}
if(flag == LS_SAVE)
	{
	while(*str)
		{
		do_byte(str,flag);
		str++;
		}
	do_byte(str,flag); /* Output a terminator */
	return;
	}
printf("FATAL: do_string passed flag %d\n", flag);
exit(0);
}

	/* 
	Periodically, to reduce overhead and code clutter, we'll probably want
	to convert all calls to do_ver_??? with direct do_??? calls, and break
	the backwards compatibility. How often this needs to happen remains to
	be seen, as the rate of accumulation isn't very predictable. -- Improv
	*/
static void do_ver_byte(byte* v, u32b version, byte defval, int flag)
{
if((flag == LS_LOAD) && (vernum < version))
	{
	*v = defval; /* Use the default value, and DO NOT READ */
	return;
	}
do_byte(v,flag); /* Otherwise, go as normal */
}

static void skip_ver_byte(u32b version, int flag)
	/* Reads and discards a byte if the savefile is as old as/older than version */
{
if((flag == LS_LOAD) && (vernum <= version))
	{
	byte forget;
	do_byte(&forget, flag);
	}
return;
}

static void do_ver_u16b(u16b* v, u32b version, u16b defval, int flag)
{
if((flag == LS_LOAD) && (vernum < version))
	{
	*v = defval;
	return;
	}
do_u16b(v,flag);
}

static void skip_ver_u16b(u32b version, int flag)
{
if( (flag == LS_LOAD) && (vernum <= version))
	{
	u16b forget;
	do_u16b(&forget, flag);
	}
return;
}

static void do_ver_s16b(s16b* v, u32b version, s16b defval, int flag)
{
if((flag == LS_LOAD) && (vernum < version))
	{
	*v = defval;
	return;
	}
do_s16b(v,flag);
}

static void skip_ver_s16b(u32b version, int flag)
{
if((flag == LS_LOAD) && (vernum <= version))
	{
	s16b forget;
	do_s16b(&forget, flag);
	}
return;
}

static void do_ver_u32b(u32b* v, u32b version, u32b defval, int flag)
{
if((flag == LS_LOAD) && (vernum < version))
	{
	*v = defval;
	return;
	}
do_u32b(v,flag);
}

static void skip_ver_u32b(u32b version, int flag)
{
if( (flag == LS_LOAD) && (vernum <= version))
	{
	u32b forget;
	do_u32b(&forget, flag);
	}
return;
}

static void do_ver_s32b(s32b* v, u32b version, s32b defval, int flag)
{
if((flag == LS_LOAD) && (vernum < version))
	{
	*v = defval;
	return;
	}
do_s32b(v,flag);
}

static void skip_ver_s32b(u32b version, int flag)
{
if( (flag == LS_LOAD) && (vernum <= version))
	{
	s32b forget;
	do_s32b(&forget, flag);
	}
return;
}

static void do_ver_string(char* str, int max, u32b version, char* defval, int flag)
	/* Careful, remember the argument order here */
{
	if((flag == LS_LOAD) && (vernum < version))
		{
		strncpy(str,defval,max);
		str[max-1] = '\0'; /* Ensure that whatever happens, the result string is term'd */
		return;
		}
do_string(str, max, flag);
}

static void skip_ver_string(u32b version, int flag)
	/* This function is slow and bulky */
{
if( (flag == LS_LOAD) && (vernum <= version))
	{
	char forget[1000];
	do_string(forget, 999, flag);
	}
return;
}

/*
 * Show information on the screen, one line at a time.
 *
 * Avoid the top two lines, to avoid interference with "msg_print()".
 */
static void note(cptr msg)
{
	static int y = 2;

	/* Draw the message */
	prt(msg, y, 0);

	/* Advance one line (wrap if needed) */
	if (++y >= 24) y = 2;

	/* Flush it */
	Term_fresh();
}


/*
 * Determine if an item can be wielded/worn (e.g. helmet, sword, bow, arrow)
 */
static bool wearable_p(object_type *o_ptr)
{
	/* Valid "tval" codes */
	switch (o_ptr->tval)
	{
		case TV_WAND:
		case TV_STAFF:
		case TV_ROD:
		case TV_ROD_MAIN:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOOMERANG:
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_MSTAFF:
		case TV_SWORD:
		case TV_AXE:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_SCROLL:
		case TV_LITE:
		case TV_POTION:
		case TV_POTION2:
		case TV_AMULET:
		case TV_RING:
		case TV_HYPNOS:
		case TV_INSTRUMENT:
		case TV_DAEMON_BOOK:
		case TV_TRAPKIT:
                case TV_TOOL:
		{
			return (TRUE);
		}
	}

	/* Nope */
	return (FALSE);
}


/*
 * rd/wr an object
 *
 * FIXME! This code probably has a lot of cruft from the old Z/V codebase.
 *
 */
static void do_item(object_type *o_ptr, int flag)
{
	byte old_dd;
	byte old_ds;

	u32b f1, f2, f3, f4, f5, esp;

	object_kind *k_ptr;

	/* Kind */
	do_s16b(&o_ptr->k_idx, flag);

	/* Location */
	do_byte(&o_ptr->iy, flag);
	do_byte(&o_ptr->ix, flag);

	/* Type/Subtype */
	do_byte(&o_ptr->tval, flag);
	do_byte(&o_ptr->sval, flag);

	/* Special pval */
	do_s32b(&o_ptr->pval, flag);

	/* Special pval */
	do_s16b(&o_ptr->pval2, flag);

	/* Special pval */
	do_s32b(&o_ptr->pval3, flag);

	do_byte(&o_ptr->discount, flag);
	do_byte(&o_ptr->number, flag);
	do_s32b(&o_ptr->weight, flag);

	do_byte(&o_ptr->name1, flag);
        skip_ver_byte(27, flag);
        do_ver_s16b(&o_ptr->name2, 28, 0, flag);
        do_ver_s16b(&o_ptr->name2b, 28, 0, flag);
	do_s16b(&o_ptr->timeout, flag);

	do_s16b(&o_ptr->to_h, flag);
	do_s16b(&o_ptr->to_d, flag);
	do_s16b(&o_ptr->to_a, flag);

	do_s16b(&o_ptr->ac, flag);

		/* We do special processing of this flag when reading */
if(flag == LS_LOAD)
	{
	do_byte(&old_dd, LS_LOAD);
	do_byte(&old_ds, LS_LOAD);
	}
if(flag == LS_SAVE)
	{
	do_byte(&o_ptr->dd, LS_SAVE);
	do_byte(&o_ptr->ds, LS_SAVE);
	}

	do_byte(&o_ptr->ident, flag);

	do_byte(&o_ptr->marked, flag);

	/* Old flags */
	do_u32b(&o_ptr->art_flags1, flag);
	do_u32b(&o_ptr->art_flags2, flag);
	do_u32b(&o_ptr->art_flags3, flag);
	do_u32b(&o_ptr->art_flags4, flag);
        do_ver_u32b(&o_ptr->art_flags5, 18, 0, flag);
        do_ver_u32b(&o_ptr->art_esp, 18, 0, flag);

	/* Monster holding object */
	do_s16b(&o_ptr->held_m_idx, flag);

	/* Special powers */
	do_byte(&o_ptr->xtra1, flag);
	do_s16b(&o_ptr->xtra2, flag);

	do_byte(&o_ptr->elevel, flag);
	do_s32b(&o_ptr->exp, flag);

	/* Read the pseudo-id */
	do_byte(&o_ptr->sense, flag);

if(flag == LS_LOAD)
	{
	char buf[128];
	/* Inscription */
	do_string(buf, 128, LS_LOAD);
	/* Save the inscription */
	if (buf[0]) o_ptr->note = quark_add(buf);

	do_string(buf, 128, LS_LOAD);
	if (buf[0]) o_ptr->art_name = quark_add(buf);
	}
if(flag == LS_SAVE)
	{
	/* Save the inscription (if any) */
	if (o_ptr->note)
		{
		do_string((char*)quark_str(o_ptr->note), 0, LS_SAVE);
		}
	else
		{
		do_string("",0,LS_SAVE);
		}
	if( o_ptr->art_name)
		{
		do_string((char*)quark_str(o_ptr->art_name), 0, LS_SAVE);
		}
	else
		{
		do_string("",0,LS_SAVE);
		}
	}

if(flag == LS_SAVE) return; /* Stick any more shared code before this. The rest
				of this function is reserved for LS_LOAD's
				cleanup functions */
/*********** END OF LS_SAVE ***************/

	/* Mega-Hack -- handle "dungeon objects" later */
	if ((o_ptr->k_idx >= 445) && (o_ptr->k_idx <= 479)) return;

	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Obtain tval/sval from k_info */
	o_ptr->tval = k_ptr->tval;
	if(o_ptr->tval != TV_RANDART) o_ptr->sval = k_ptr->sval;


	/* Repair non "wearable" items */
	if (!wearable_p(o_ptr))
	{
		/* Acquire correct fields */
		o_ptr->to_h = k_ptr->to_h;
		o_ptr->to_d = k_ptr->to_d;
		o_ptr->to_a = k_ptr->to_a;

		/* Acquire correct fields */
		o_ptr->ac = k_ptr->ac;
		o_ptr->dd = k_ptr->dd;
		o_ptr->ds = k_ptr->ds;

#if 0
		/* Acquire correct weight */
		if((o_ptr->tval != TV_CORPSE)&&(o_ptr->tval != TV_EGG)) o_ptr->weight = k_ptr->weight;

		/* Paranoia */
		o_ptr->name1 = o_ptr->name2 = 0;
#endif
		/* All done */
		return;
	}


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

	/* Paranoia */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Verify that artifact */
		if (!a_ptr->name) o_ptr->name1 = 0;
	}

	/* Paranoia */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];

		/* Verify that ego-item */
		if (!e_ptr->name) o_ptr->name2 = 0;
	}


	/* Acquire standard fields */
	o_ptr->ac = k_ptr->ac;
	o_ptr->dd = k_ptr->dd;
	o_ptr->ds = k_ptr->ds;

	/* Artifacts */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Acquire new artifact fields */
		o_ptr->ac = a_ptr->ac;
		o_ptr->dd = a_ptr->dd;
		o_ptr->ds = a_ptr->ds;

		/* Acquire new artifact weight */
		o_ptr->weight = a_ptr->weight;
	}

	/* Ego items */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];


		o_ptr->dd = old_dd;
		o_ptr->ds = old_ds;
	}

	if (o_ptr->art_name) /* A random artifact */
	{
		o_ptr->dd = old_dd;
		o_ptr->ds = old_ds;
	}
}




/*
 * Read a monster
 */
static void do_monster(monster_type *m_ptr, int flag)
{
	int i;

	/* Read the monster race */
	do_s16b(&m_ptr->r_idx,flag);

	do_u16b(&m_ptr->ego,flag);

	/* Read the other information */
	do_byte(&m_ptr->fy, flag);
	do_byte(&m_ptr->fx, flag);
	do_s16b(&m_ptr->hp, flag);
	do_s16b(&m_ptr->maxhp, flag);
	do_s16b(&m_ptr->csleep, flag);
	do_byte(&m_ptr->mspeed, flag);
	do_byte(&m_ptr->energy, flag);
	do_byte(&m_ptr->stunned, flag);
	do_byte(&m_ptr->confused, flag);
	do_byte(&m_ptr->monfear, flag);
	do_u32b(&m_ptr->smart, flag);
	do_s16b(&m_ptr->status, flag);
	do_s16b(&m_ptr->possessor, flag);
	do_byte(&m_ptr->speed, flag);
	do_byte(&m_ptr->level, flag);
	do_s16b(&m_ptr->ac, flag);
	do_s32b(&m_ptr->exp, flag);
	do_s16b(&m_ptr->target, flag);

        do_ver_s16b(&m_ptr->bleeding, 27, 0, flag);
        do_ver_s16b(&m_ptr->poisoned, 27, 0, flag);

        do_ver_byte(&m_ptr->mflag, 21, 0, flag);
        if (flag == LS_LOAD) m_ptr->mflag &= PERM_MFLAG_MASK;

	/* Attacks */
	for (i = 0; i < 4; i++)
	{
		do_byte(&m_ptr->blow[i].method, flag);
		do_byte(&m_ptr->blow[i].effect, flag);
		do_byte(&m_ptr->blow[i].d_dice, flag);
		do_byte(&m_ptr->blow[i].d_side, flag);
	}
}





/*
 * Handle monster lore
 */
static void do_lore(int r_idx, int flag)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Count sights/deaths/kills */
	do_s16b(&r_ptr->r_sights, flag);
	do_s16b(&r_ptr->r_deaths, flag);
	do_s16b(&r_ptr->r_pkills, flag);
	do_s16b(&r_ptr->r_tkills, flag);

	/* Count wakes and ignores */
	do_byte(&r_ptr->r_wake, flag);
	do_byte(&r_ptr->r_ignore, flag);

	/* Extra stuff */
	do_byte(&r_ptr->r_xtra1, flag);
	do_byte(&r_ptr->r_xtra2, flag);

	/* Count drops */
	do_byte(&r_ptr->r_drop_gold, flag);
	do_byte(&r_ptr->r_drop_item, flag);

	/* Count spells */
	do_byte(&r_ptr->r_cast_inate, flag);
	do_byte(&r_ptr->r_cast_spell, flag);

	/* Count blows of each type */
	do_byte(&r_ptr->r_blows[0], flag);
	do_byte(&r_ptr->r_blows[1], flag);
	do_byte(&r_ptr->r_blows[2], flag);
	do_byte(&r_ptr->r_blows[3], flag);

	/* Memorize flags */
	do_u32b(&r_ptr->r_flags1, flag); /* Just to remind you */
	do_u32b(&r_ptr->r_flags2, flag); /* flag is unrelated to */
	do_u32b(&r_ptr->r_flags3, flag); /* the other argument */
	do_u32b(&r_ptr->r_flags4, flag);
	do_u32b(&r_ptr->r_flags5, flag);
	do_u32b(&r_ptr->r_flags6, flag);
	do_u32b(&r_ptr->r_flags7, flag);
	do_u32b(&r_ptr->r_flags8, flag);
	do_u32b(&r_ptr->r_flags9, flag);

	/* Read the "Racial" monster tmp16b per level */
	do_s16b(&r_ptr->max_num, flag);

	do_byte(&r_ptr->on_saved, flag);

	if(flag == LS_LOAD)
		{
			/* Lore flag repair? */
		r_ptr->r_flags1 &= r_ptr->flags1;
		r_ptr->r_flags2 &= r_ptr->flags2;
		r_ptr->r_flags3 &= r_ptr->flags3;
		r_ptr->r_flags4 &= r_ptr->flags4;
		r_ptr->r_flags5 &= r_ptr->flags5;
		r_ptr->r_flags6 &= r_ptr->flags6;
		}
}




/*
 * Read a store
 */
static bool do_store(store_type* str, int flag)
		 /* FIXME! Why does this return anything when
		it always returns the same thing? */
{
	int j;

	byte num;

	/* Some basic info */
	do_s32b(&str->store_open, flag);
	do_s16b(&str->insult_cur, flag);
	do_u16b(&str->owner, flag); 
	if(flag == LS_SAVE) num = str->stock_num; 

        /* Could be cleaner, done this way for benefit of the for loop later on */
	do_byte(&num, flag);

	do_s16b(&str->good_buy, flag);
	do_s16b(&str->bad_buy, flag);

	/* Last visit */
	do_s32b(&str->last_visit, flag);

	/* Items */
	for (j = 0; j < num; j++)
		{
		if(flag == LS_LOAD)
		/* Can't this be cleaner? */
			{
			object_type forge;
			/* Wipe the object */
			object_wipe(&forge);
			/* Read the item */
			do_item(&forge, LS_LOAD);
			/* Acquire valid items */
			if (str->stock_num < STORE_INVEN_MAX)
				{
				int k = str->stock_num++;
				/* Acquire the item */
				object_copy(&str->stock[k], &forge);
				}
			}
		if(flag == LS_SAVE) do_item(&str->stock[j], flag);
		}

	/* Success */
	return (TRUE);
}

/*
 * RNG state
 */
static void do_randomizer(int flag)
{
	int i;

	u16b tmp16u = 0;

	/* Tmp */
	do_u16b(&tmp16u, flag);
	
	/* Place */
	do_u16b(&Rand_place, flag);
	
	/* State */
	for (i = 0; i < RAND_DEG; i++)
	{
		do_u32b(&Rand_state[i], flag);
	}

	/* Accept */
	if(flag == LS_LOAD)
		{
		Rand_quick = FALSE;
		}
}

/*
 * Handle options
 *
 * Normal options are stored as a set of 256 bit flags,
 * plus a set of 256 bit masks to indicate which bit flags were defined
 * at the time the savefile was created.  This will allow new options
 * to be added, and old options to be removed, at any time, without
 * hurting old savefiles.
 *
 * The window options are stored in the same way, but note that each
 * window gets 32 options, and their order is fixed by certain defines.
 */
static void do_options(int flag)
{
	int i, n;

	u32b oflag[8];
	u32b mask[8];

	/*** Special info */

	/* Read "delay_factor" */
	do_byte(&delay_factor, flag);

	/* Read "hitpoint_warn" */
	do_byte(&hitpoint_warn, flag);

	/*** Cheating options ***/
if(flag == LS_LOAD)	/* There *MUST* be some nice way to unify this! */
	{
	u16b c;
	do_u16b(&c, LS_LOAD);
	if (c & 0x0002) wizard = TRUE;
	cheat_peek = (c & 0x0100) ? TRUE : FALSE;
	cheat_hear = (c & 0x0200) ? TRUE : FALSE;
	cheat_room = (c & 0x0400) ? TRUE : FALSE;
	cheat_xtra = (c & 0x0800) ? TRUE : FALSE;
	cheat_know = (c & 0x1000) ? TRUE : FALSE;
	cheat_live = (c & 0x2000) ? TRUE : FALSE;
	}
if(flag == LS_SAVE)
	{
	u16b c = 0;
	if (wizard) c |= 0x0002;
	if (cheat_peek) c |= 0x0100;
	if (cheat_hear) c |= 0x0200;
	if (cheat_room) c |= 0x0400;
	if (cheat_xtra) c |= 0x0800;
	if (cheat_know) c |= 0x1000;
	if (cheat_live) c |= 0x2000;
	do_u16b(&c, LS_SAVE);
	}

	do_byte(&autosave_l, flag);
	do_byte(&autosave_t, flag);
	do_s16b(&autosave_freq, flag);

if(flag == LS_LOAD)
  {
	/* Read the option flags */
	for (n = 0; n < 8; n++) do_u32b(&oflag[n], flag);

	/* Read the option masks */
	for (n = 0; n < 8; n++) do_u32b(&mask[n], flag);

	/* Analyze the options */
	for (n = 0; n < 8; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 32; i++)
		{
			/* Process valid flags */
			if (mask[n] & (1L << i))
			{
				/* Process valid flags */
				if (option_mask[n] & (1L << i))
				{
					/* Set */
					if (oflag[n] & (1L << i))
					{
						/* Set */
						option_flag[n] |= (1L << i);
					}
				
					/* Clear */
					else
					{
						/* Clear */
						option_flag[n] &= ~(1L << i);
					}
				}
			}
		}
	}


	/*** Window Options ***/

	/* Read the window flags */
	for (n = 0; n < 8; n++) do_u32b(&oflag[n], flag);

	/* Read the window masks */
	for (n = 0; n < 8; n++) do_u32b(&mask[n], flag);

	/* Analyze the options */
	for (n = 0; n < 8; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 32; i++)
		{
			/* Process valid flags */
			if (mask[n] & (1L << i))
			{
				/* Process valid flags */
				if (window_mask[n] & (1L << i))
				{
					/* Set */
					if (oflag[n] & (1L << i))
					{
						/* Set */
						window_flag[n] |= (1L << i);
					}
				
					/* Clear */
					else
					{
						/* Clear */
						window_flag[n] &= ~(1L << i);
					}
				}
			}
		}
	}
  }
if(flag == LS_SAVE)
  {
	/* Analyze the options */
	for (i = 0; option_info[i].o_desc; i++)
	{
		int os = option_info[i].o_page;
		int ob = option_info[i].o_bit;

		/* Process real entries */
		if (option_info[i].o_var)
		{
			/* Set */
			if (*option_info[i].o_var)
			{
				/* Set */
				option_flag[os] |= (1L << ob);
			}
			
			/* Clear */
			else
			{
				/* Clear */
				option_flag[os] &= ~(1L << ob);
			}
		}
	}


	/*** Normal options ***/

	/* Dump the flags */
	for (i = 0; i < 8; i++) do_u32b(&option_flag[i], flag);

	/* Dump the masks */
	for (i = 0; i < 8; i++) do_u32b(&option_mask[i], flag);

	/*** Window options ***/

	/* Dump the flags */
	for (i = 0; i < 8; i++) do_u32b(&window_flag[i], flag);

	/* Dump the masks */
	for (i = 0; i < 8; i++) do_u32b(&window_mask[i], flag);
  }
}





/*
 * Handle "ghost" info
 *
 */
static void do_ghost(int flag)
{
int i;
monster_race *r_ptr = &r_info[max_r_idx - 1];

	/* Name */
do_string(r_name + r_ptr->name, 64, flag);

	/* Visuals */
do_byte(&r_ptr->d_char, flag);
do_byte(&r_ptr->d_attr, flag);

/* Level/Rarity */
do_byte(&r_ptr->level, flag);
do_byte(&r_ptr->rarity, flag);

/* Misc info */
do_byte(&r_ptr->hdice, flag);
do_byte(&r_ptr->hside, flag);
do_s16b(&r_ptr->ac, flag);
do_s16b(&r_ptr->sleep, flag);
do_byte(&r_ptr->aaf, flag);
do_byte(&r_ptr->speed, flag);

/* Experience */
do_s32b(&r_ptr->mexp, flag);

/* Frequency */
do_byte(&r_ptr->freq_inate, flag);
do_byte(&r_ptr->freq_spell, flag);

/* Flags */
do_u32b(&r_ptr->flags1, flag);
do_u32b(&r_ptr->flags2, flag);
do_u32b(&r_ptr->flags3, flag);
do_u32b(&r_ptr->flags4, flag);
do_u32b(&r_ptr->flags5, flag);
do_u32b(&r_ptr->flags6, flag);
do_u32b(&r_ptr->flags7, flag);
do_u32b(&r_ptr->flags8, flag);
do_u32b(&r_ptr->flags9, flag);

/* Attacks */
for (i = 0; i < 4; i++)
	{
	do_byte(&r_ptr->blow[i].method, flag);
	do_byte(&r_ptr->blow[i].effect, flag);
	do_byte(&r_ptr->blow[i].d_dice, flag);
	do_byte(&r_ptr->blow[i].d_side, flag);
	}
/* Hack -- set the "graphic" info */
if(flag == LS_LOAD)
	{
	r_ptr->x_attr = r_ptr->d_attr;
	r_ptr->x_char = r_ptr->d_char;
	}
}

/* Load/Save the random spells info */
static void do_spells(int i, int flag)
{
	random_spell *s_ptr = &random_spells[i];
	do_string(s_ptr->name, 30, flag);
	do_string(s_ptr->desc, 30, flag);
	do_s16b(&s_ptr->mana, flag);
	do_s16b(&s_ptr->fail, flag);
	do_u32b(&s_ptr->proj_flags, flag);
	do_byte(&s_ptr->GF, flag);
	do_byte(&s_ptr->radius, flag);
	do_byte(&s_ptr->dam_sides, flag);
	do_byte(&s_ptr->dam_dice, flag);
	do_byte(&s_ptr->level, flag);
	do_byte(&s_ptr->untried, flag);
}


/*
 * Handle player inventory
 *
 * FIXME! This function probably could be unified better
 * Note that the inventory is "re-sorted" later by "dungeon()".
 */
static bool do_inventory(int flag) 
{
if(flag == LS_LOAD)
	{
	int slot = 0;

	object_type forge;
	object_type *q_ptr;

	/* No items */
	inven_cnt = 0;
	equip_cnt = 0;

	/* Read until done */
	while (1)
	{
		u16b n;

		/* Get the next item index */
		do_u16b(&n, LS_LOAD);

		/* Nope, we reached the end */
		if (n == 0xFFFF) break;

		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Read the item */
		do_item(q_ptr, LS_LOAD);

		/* Hack -- verify item */
		if (!q_ptr->k_idx) return (FALSE);

		/* Wield equipment */
		if (n >= INVEN_WIELD)
		{
			/* Copy object */
			object_copy(&inventory[n], q_ptr);

			/* One more item */
			equip_cnt++;
		}

		/* Warning -- backpack is full */
		else if (inven_cnt == INVEN_PACK)
		{
			/* Oops */
			note("Too many items in the inventory!");

			/* Fail */
			return (FALSE);
		}

		/* Carry inventory */
		else
		{
			/* Get a slot */
			n = slot++;

			/* Copy object */
			object_copy(&inventory[n], q_ptr);

			/* One more item */
			inven_cnt++;
			}
		}
	}
if(flag == LS_SAVE)
	{
	u16b i;
	u16b sent = 0xFFFF;
	for(i=0;i<INVEN_TOTAL;i++)
		{
		object_type* o_ptr = &inventory[i];
		if(! o_ptr->k_idx) continue;
		do_u16b(&i, flag);
		do_item(o_ptr, flag);
		}
	do_u16b(&sent, LS_SAVE); /* Sentinel */ 
	}
	/* Success */
	return (TRUE);
}



/*
 * Read the saved messages
 */
static void do_messages(int flag) /* FIXME! We should be able to unify this better */
{
	int i;
	char buf[128];
	byte color;

	s16b num;

	if(flag == LS_SAVE) num = (compress_savefile && (message_num() > 40))? 40 : message_num();

	/* Total */
	do_s16b(&num, flag);

	/* Read the messages */
if(flag == LS_LOAD)
	{
	for (i = 0; i < num; i++)
		{
		/* Read the message */
		do_string(buf, 128, LS_LOAD);
		do_byte(&color, flag);

		/* Save the message */
		message_add(buf, color);
		}
	}
if(flag == LS_SAVE)
	{
	byte holder;
	for(i = num-1; i >= 0; i--)
		{
		do_string((char*)message_str( (s16b)i), 0, LS_SAVE);
		holder = message_color((s16b)i);
		do_byte(&holder, flag);
		}
	}
}

/*
 * Handle dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 */
static bool do_dungeon(int flag)
{
	int i; 

	cave_type *c_ptr;

	/* Read specific */
	u16b tmp16b = 0;

	my_sentinel("Before do_dungeon",324, flag);

	/* Header info */
	do_s16b(&dun_level, flag);
	do_byte(&dungeon_type, flag);
	do_s16b(&num_repro, flag);
	do_s16b(&py, flag);
	do_s16b(&px, flag);
	do_s16b(&cur_hgt, flag);
	do_s16b(&cur_wid, flag);
	do_s16b(&max_panel_rows, flag);
	do_s16b(&max_panel_cols, flag);

        do_ver_s32b(&dungeon_flags1, 24, 0, flag);

        /* TO prevent bugs with evolving dungeons */
        for (i = 0; i < 100; i++)
        {
                do_ver_s16b(&floor_type[i], 17, FEAT_FLOOR, flag);
                do_ver_s16b(&fill_type[i], 17, FEAT_WALL_EXTRA, flag);
        }

	if((flag == LS_LOAD) && (!dun_level && !p_ptr->inside_quest))
        {
		int xstart = 0;
		int ystart = 0;
		/* Init the wilderness */
		process_dungeon_file("w_info.txt", &ystart, &xstart, cur_hgt, cur_wid, TRUE);

		/* Init the town */
		xstart = 0;
		ystart = 0;
		init_flags = 0;
		process_dungeon_file("t_info.txt", &ystart, &xstart, cur_hgt, cur_wid, TRUE);
        }

	do_grid(flag);

	/*** Objects ***/

	if(flag == LS_SAVE) compact_objects(0);
	if(flag == LS_SAVE){tmp16b = o_max;}
	/* Item count */
	do_u16b(&tmp16b, flag);

	/* Verify maximum */
	if( (flag == LS_LOAD) && (tmp16b >= max_o_idx))
		{
		note(format("Too many (%d) object entries!", tmp16b));
		return (FALSE);
		}

	/* Dungeon items */
	for (i = 1; i < tmp16b; i++)
		{
		int o_idx;

		object_type *o_ptr;

		if(flag == LS_SAVE)
			{
			o_ptr = &o_list[i];
			do_item(o_ptr, LS_SAVE);
			continue; /* Saving is easy */
			}
		/* Until the end of the loop, this is all LS_LOAD */

		/* Get a new record */
		o_idx = o_pop();

		/* Oops */
		if (i != o_idx)
			{
			note(format("Object allocation error (%d <> %d)",
				i, o_idx));
			return(FALSE);
			}


		/* Acquire place */
		o_ptr = &o_list[o_idx];

		/* Read the item */
		do_item(o_ptr, LS_LOAD);

		/* Monster */
		if (o_ptr->held_m_idx)
			{
			monster_type *m_ptr;

			/* Monster */
			m_ptr = &m_list[o_ptr->held_m_idx];

			/* Build a stack */
			o_ptr->next_o_idx = m_ptr->hold_o_idx;

			/* Place the object */
			m_ptr->hold_o_idx = o_idx;
			}
		
		/* Dungeon */
		else
			{
			/* Access the item location */
			c_ptr = &cave[o_ptr->iy][o_ptr->ix];

			/* Build a stack */
			o_ptr->next_o_idx = c_ptr->o_idx;

			/* Place the object */
			c_ptr->o_idx = o_idx;
			}
		}

	/*** Monsters ***/

	if(flag == LS_SAVE) compact_monsters(0);
	if(flag == LS_SAVE) tmp16b = m_max;
	/* Read the monster count */
	do_u16b(&tmp16b, flag);

	/* Validate */
	if ((flag == LS_LOAD) && (tmp16b >= max_m_idx))
		{
		note(format("Too many (%d) monster entries!", tmp16b));
		return (FALSE);
		}

	/* Read the monsters */
	for (i = 1; i < tmp16b; i++)
		{
		int m_idx;
		monster_type *m_ptr;
		monster_race *r_ptr;

		if(flag == LS_SAVE)
			{
			m_ptr = &m_list[i];
			do_monster(m_ptr, LS_SAVE);
			continue; /* Easy to save a monster */
			}
			/* From here on, it's all LS_LOAD */
		/* Get a new record */
		m_idx = m_pop();

		/* Oops */
		if (i != m_idx)
			{
			note(format("Monster allocation error (%d <> %d)",
				i, m_idx));
			return(FALSE);
			}

		/* Acquire monster */
		m_ptr = &m_list[m_idx];

		/* Read the monster */
		do_monster(m_ptr, LS_LOAD);

		/* Access grid */
		c_ptr = &cave[m_ptr->fy][m_ptr->fx];

		/* Mark the location */
		c_ptr->m_idx = m_idx;

		/* Access race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Count XXX XXX XXX */
		r_ptr->cur_num++;
		}

	/* Read the kept monsters */

	if(flag == LS_SAVE) tmp16b = max_m_idx;
	/* Read the monster count */
	do_u16b(&tmp16b, flag);

	/* Hack -- verify */
	if((flag == LS_LOAD) && (tmp16b > max_m_idx))
		{
		note(format("Too many (%d) monster entries!", tmp16b));
		return (FALSE);
		}
	for (i = 1; i < tmp16b; i++)
		{
		monster_type *m_ptr;

		/* Acquire monster */
		m_ptr = &km_list[i];

		/* Read the monster */
		do_monster(m_ptr, flag);
		}

	/*** Success ***/

	/* The dungeon is ready */
	if(flag == LS_LOAD) character_dungeon = TRUE;

	/* Success */
	return(TRUE);
}

/* Returns TRUE if we successfully load the dungeon */
bool load_dungeon(char *ext)
{
	char tmp[16];
	char name[1024];
	byte old_dungeon_type = dungeon_type;
	s16b old_dun = dun_level;
  
	/* Construct name */
	sprintf(tmp, "%s.%s", player_base, ext);
	path_build(name, 1024, ANGBAND_DIR_SAVE, tmp);

	/* Open the file */
	safe_setuid_grab();
	fff = my_fopen(name, "rb");

	if (fff == NULL)
	{
		dun_level = old_dun;
		dungeon_type = old_dungeon_type;

		my_fclose(fff);
		return(FALSE);
	}
	bz_prep(LS_LOAD);
	safe_setuid_drop();

	/* Read the dungeon */
	if (!do_dungeon(LS_LOAD))
		{
		dun_level = old_dun;
		dungeon_type = old_dungeon_type;

		bz_done(LS_LOAD);
		my_fclose(fff);
		return(FALSE);
		}

	dun_level = old_dun;
	dungeon_type = old_dungeon_type;

	/* Done */
	bz_done(LS_LOAD);
	my_fclose(fff);
	return(TRUE);
}

void do_fate(int i, int flag)
{
	if ((flag == LS_LOAD) && (i >= MAX_FATES) ) i = MAX_FATES - 1;

	do_byte(&fates[i].fate, flag);
	do_byte(&fates[i].level, flag);
	do_byte(&fates[i].serious, flag);
	do_s16b(&fates[i].o_idx, flag);
	do_s16b(&fates[i].e_idx, flag);
	do_s16b(&fates[i].a_idx, flag);
	do_s16b(&fates[i].v_idx, flag);
	do_s16b(&fates[i].r_idx, flag);
	do_s16b(&fates[i].count, flag);
	do_s16b(&fates[i].time, flag);
	do_byte(&fates[i].know, flag);
}

/*
 * Actually read the savefile
 */
static bool do_savefile_aux(int flag)
{
	int i, j;

	byte tmp8u;
	u16b tmp16u;
        u16b town_count;

	/* Mention the savefile version */
if(flag == LS_LOAD)
	note(format("Loading version %lu savefile... key (%d)", vernum, sf_extra));
if(flag == LS_SAVE)
	{
	sf_when = time((time_t *)0);	/* Note when file was saved */
	sf_xtra = 0L;	/* What the hell is this? */
	sf_saves++; /* Increment the saves ctr */
	}

	/* Handle version bytes. FIXME! DG wants me to change this all around */
if(flag == LS_LOAD)
	{
	u32b mt32b;
	byte mtbyte;
		/* Discard all this, we've already read it */
	do_u32b(&mt32b, flag);
	do_byte(&mtbyte, flag);
	}
if(flag == LS_SAVE)
	{
	u32b saver;
	saver = SAVEFILE_VERSION;
	do_u32b(&saver, flag);
	tmp8u = (byte)rand_int(256);
	do_byte(&tmp8u, flag); /* 'encryption' */
	}

	/* Operating system info? Not really. This is just set to 0L */
	do_u32b(&sf_xtra, flag);

	/* Time of last save */
	do_u32b(&sf_when, flag);

	/* Number of past lives */
	do_u16b(&sf_lives, flag);

	/* Number of times saved */
	do_u16b(&sf_saves, flag);

	/* Read RNG state */
	do_randomizer(flag);
	if((flag == LS_LOAD) && (arg_fiddle)) note("Loaded Randomizer Info");

	/* Then the options */
	do_options(flag);
	if((flag == LS_LOAD) && (arg_fiddle)) note("Loaded Option Flags");

	/* Then the "messages" */
	do_messages(flag);
	if((flag == LS_LOAD) && (arg_fiddle)) note("Loaded Messages");

	/* Monster Memory */
	if(flag == LS_SAVE) tmp16u = max_r_idx;
	do_u16b(&tmp16u, flag);

	/* Incompatible save files */
	if ((flag == LS_LOAD) && (tmp16u > max_r_idx) )
	{
		note(format("Too many (%u) monster races!", tmp16u));
		return (FALSE);
	}

	/* Read the available records */
	for (i = 0; i < tmp16u; i++)
	{
		monster_race *r_ptr;

		/* Read the lore */
		do_lore(i, flag);

		/* Access that monster */
		r_ptr = &r_info[i];	/* FIXME! Why the hell are we doing this? */
	}

	if ((flag == LS_LOAD) && (arg_fiddle)) note("Loaded Monster Memory");
	/* Object Memory */
	if(flag == LS_SAVE) tmp16u = max_k_idx;
	do_u16b(&tmp16u, flag);

	/* Incompatible save files */
	if((flag == LS_LOAD) &&  (tmp16u > max_k_idx))
	{
		note(format("Too many (%u) object kinds!", tmp16u));
		return (FALSE);
	}

	/* Read the object memory */
	for (i = 0; i < tmp16u; i++) do_xtra(i, flag);
	if((flag == LS_LOAD) && (arg_fiddle)) note("Loaded Object Memory");
	if(flag == LS_LOAD) junkinit();

	{
		u16b max_towns_ldsv;
		u16b max_quests_ldsv;
		if(flag == LS_SAVE) max_towns_ldsv = max_towns;
		/* Number of towns */
		do_u16b(&max_towns_ldsv, flag);
		/* Incompatible save files */
		if((flag == LS_LOAD) && (max_towns_ldsv > max_towns))
			{
			note(format("Too many (%u) towns!", max_towns_ldsv));
			return (FALSE);
			}
		/* Min of random towns */
		if(flag == LS_SAVE) max_towns_ldsv = TOWN_RANDOM;
		do_u16b(&max_towns_ldsv, flag);
		/* Incompatible save files */
		if ((flag == LS_LOAD) && (max_towns_ldsv != TOWN_RANDOM))
			{
			note(format("Different random towns base (%u)!",
				max_towns_ldsv));
			return (FALSE);
			}

		for (i = TOWN_RANDOM; i < max_towns; i++)
                {
			do_u32b(&town[i].seed, flag);
			do_byte(&town[i].numstores, flag);
                        do_byte(&town[i].real, flag);

                        /* If the town is realy used create a sock */
                        if (town[i].real && (flag == LS_LOAD))
                        {
                                create_stores_stock(i);
                        }
                }

                /* Number of dungeon */
                if (flag == LS_SAVE) max_towns_ldsv = max_d_idx;
		do_u16b(&max_towns_ldsv, flag);

		/* Incompatible save files */
		if ((flag == LS_LOAD) && (max_towns_ldsv > max_d_idx))
                {
			note(format("Too many dungeon types (%u)!",
				max_towns_ldsv));
			return (FALSE);
                }

                /* Number of towns per dungeon */
		if(flag == LS_SAVE) max_quests_ldsv = TOWN_DUNGEON;
		do_u16b(&max_quests_ldsv, flag);
		/* Incompatible save files */
		if ((flag == LS_LOAD) && (max_quests_ldsv > max_d_idx))
			{
			note(format("Too many town per dungeons (%u)!",
				max_quests_ldsv));
			return (FALSE);
			}

		for (i = 0; i < max_towns_ldsv; i++)
			{
			for (j = 0; j < max_quests_ldsv; j++)
				{
				do_s16b(&(d_info[i].t_idx[j]), flag);
				do_s16b(&(d_info[i].t_level[j]), flag);
				}
			do_s16b(&(d_info[i].t_num), flag);
			}
		if(flag == LS_SAVE) max_quests_ldsv = MAX_Q_IDX;
		/* Number of quests */
		do_u16b(&max_quests_ldsv, flag);

		/* Incompatible save files */
                if ((flag == LS_LOAD) && (max_quests_ldsv > MAX_Q_IDX))
                {
			note(format("Too many (%u) quests!", max_quests_ldsv));
			return(FALSE);
                }

		for (i = 0; i < max_quests_ldsv; i++)
		{
			do_s16b(&quest[i].status, flag);
                        for (j = 0; j < 4; j++)
			{
				do_s32b(&(quest[i].data[j]), flag);
			}

			/* Init the hooks */
                        if (flag == LS_LOAD) quest[i].init(i);
		}

		/* Position in the wilderness */
		do_s32b(&p_ptr->wilderness_x, flag);
		do_s32b(&p_ptr->wilderness_y, flag);
		do_byte(&p_ptr->wild_mode, flag);
		do_byte(&p_ptr->old_wild_mode, flag);

			{
			s32b wild_x_size, wild_y_size;
			if(flag == LS_SAVE)
				{
				wild_x_size = max_wild_x;
				wild_y_size = max_wild_y;
				}
			/* Size of the wilderness */
			do_s32b(&wild_x_size, flag);
			do_s32b(&wild_y_size, flag);
			/* Incompatible save files */
			if((flag == LS_LOAD) && ((wild_x_size > max_wild_x) || (wild_y_size > max_wild_y)))
				{
				note(format("Wilderness is too big (%u/%u)!",
					wild_x_size, wild_y_size));
				return (FALSE);
				}
			/* Wilderness seeds */
			for (i = 0; i < wild_x_size; i++)
				{
				for (j = 0; j < wild_y_size; j++)
					{
					do_u32b(&wild_map[j][i].seed, flag);
					do_u16b(&wild_map[j][i].entrance, flag);
					do_byte(&wild_map[j][i].known, flag);
					}
				}
			}
		}
	if((flag == LS_LOAD) && (arg_fiddle)) note("Loaded Quests");

	/* Load the random artifacts. */
	if(flag == LS_SAVE) tmp16u = MAX_RANDARTS;
	do_u16b(&tmp16u, flag);
	if((flag == LS_LOAD) && (tmp16u > MAX_RANDARTS))
		{
		note(format("Too many (%u) random artifacts!", tmp16u));
		return(FALSE);
		}
	for (i = 0; i < tmp16u; i++) {
		random_artifact* ra_ptr = &random_artifacts[i];

		do_string(ra_ptr->name_full, 80, flag);
		do_string(ra_ptr->name_short, 80, flag);
		do_byte(&ra_ptr->level, flag);
		do_byte(&ra_ptr->attr, flag);
		do_u32b(&ra_ptr->cost, flag);
		do_byte(&ra_ptr->activation, flag);
		do_byte(&ra_ptr->generated, flag);
	}

	/* Load the Artifacts */
	if(flag == LS_SAVE) tmp16u = max_a_idx;
	do_u16b(&tmp16u, flag);
	/* Incompatible save files */
	if((flag == LS_LOAD) && (tmp16u > max_a_idx))
	{
		note(format("Too many (%u) artifacts!", tmp16u));
		return (FALSE);
	}

	/* Read the artifact flags */
	for (i = 0; i < tmp16u; i++)
		{
		do_byte(&(&a_info[i])->cur_num, flag);
		}
	if ((flag == LS_LOAD) && arg_fiddle) note("Loaded Artifacts");

	/* Fates */
	if(flag == LS_SAVE) tmp16u = MAX_FATES;
	do_u16b(&tmp16u, flag);

	/* Incompatible save files */
	if((flag == LS_LOAD) && (tmp16u > MAX_FATES))
		{
		note(format("Too many (%u) fates!", tmp16u));
		return (FALSE);
		}

	/* Read the fate flags */
	for (i = 0; i < tmp16u; i++)
	{
		do_fate(i, flag);
	}
	if ((flag == LS_LOAD) && arg_fiddle) note("Loaded Fates");

	/* Load the Traps */
	if(flag == LS_SAVE) tmp16u = max_t_idx;
	do_u16b(&tmp16u, flag);

	/* Incompatible save files */
	if ((flag == LS_LOAD) && (tmp16u > max_t_idx))
		{
		note(format("Too many (%u) traps!", tmp16u));
		return (FALSE);
		}

	/* fate flags */
	for (i = 0; i < tmp16u; i++)
		{
		do_byte(&t_info[i].ident, flag);
		}
	if((flag == LS_LOAD) && (arg_fiddle)) note("Loaded Traps");

	/* inscription knowledge */
	if(flag == LS_SAVE) tmp16u = MAX_INSCRIPTIONS;
	do_u16b(&tmp16u, flag);

	/* Incompatible save files */
	if((flag == LS_LOAD) && (tmp16u > MAX_INSCRIPTIONS))
		{
		note(format("Too many (%u) inscriptions!", tmp16u));
		return (FALSE);
		}

	/* Read the inscription flag */
	for (i = 0; i < tmp16u; i++)
		do_byte(&inscription_info[i].know, flag);
	if ((flag == LS_LOAD) && arg_fiddle) note("Loaded Inscriptions");


	/* Read the extra stuff */
	do_extra(flag);
	if((flag == LS_LOAD) && arg_fiddle) note("Loaded extra information");


	/* player_hp array */
	if(flag == LS_SAVE) tmp16u = PY_MAX_LEVEL;
	do_u16b(&tmp16u, flag);
	/* Incompatible save files */
	if ((flag == LS_LOAD) && (tmp16u > PY_MAX_LEVEL))
		{
		note(format("Too many (%u) hitpoint entries!", tmp16u));
		return (FALSE);
		}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++)
		{
		do_s16b(&player_hp[i], flag);
		}

	if(flag == LS_LOAD) morejunk();

	/* spell info */
	if(flag == LS_SAVE) tmp16u = MAX_REALM;
	do_u16b(&tmp16u, flag);

	/* Incompatible save files */
	if ((flag == LS_LOAD) && (tmp16u > MAX_REALM))
		{
		note(format("Too many (%u) realm entries!", tmp16u));
		return (FALSE);
		}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++)
		{
		do_u32b(&spell_learned[i][0], flag);
		do_u32b(&spell_learned[i][1], flag);
		do_u32b(&spell_worked[i][0], flag);
		do_u32b(&spell_worked[i][1], flag);
		do_u32b(&spell_forgotten[i][0], flag);
		do_u32b(&spell_forgotten[i][1], flag);
		}

	for (i = 0; i < 64; i++) /* FIXME! What the hell is 64? Hardcoding */
		{
		do_byte(&realm_order[i], flag);
		do_byte(&spell_order[i], flag);
		}

	for(i = 0; i < MAX_REALM; i++)
		for(j = 0; j < 64; j++)
			{
			do_byte(&spell_level[i][j], flag);
			}

	/* Read the pet command settings */
	do_byte(&p_ptr->pet_follow_distance, flag);
	do_byte(&p_ptr->pet_open_doors, flag);
	do_byte(&p_ptr->pet_pickup_items, flag);

	/* Read the inventory */
	if (!do_inventory(flag) && (flag == LS_LOAD)) /* do NOT reverse this ordering */
		{
		note("Unable to read inventory");
		return (FALSE);
		}

		/* Read number of towns */
		if(flag == LS_SAVE) town_count = max_towns;
		do_u16b(&town_count, flag);
		/* Read the stores */
		if(flag == LS_SAVE) tmp16u = max_st_idx;
		do_u16b(&tmp16u, flag);
		for (i = 1; i < town_count; i++)
                {
                        if (!town[i].real) continue;

                        /* Ultra paranoia */
                        if (!town[i].stocked) create_stores_stock(i);

			for (j = 0; j < tmp16u; j++)
                        {
				if (!do_store(&town[i].store[j], flag) && (flag == LS_LOAD))
					 return (FALSE);
			}
		}
	/* I'm not dead yet... */
	if (!death)
		{
		/* Dead players have no dungeon */
		if(flag == LS_LOAD) note("Restoring Dungeon...");
		if((flag == LS_LOAD) && (!do_dungeon(LS_LOAD)))
			{
			note("Error reading dungeon data");
			return (FALSE);
			}
		if(flag == LS_SAVE) do_dungeon(LS_SAVE);
		my_sentinel("Before ghost data", 435, flag);
		/* ghost info */
		do_ghost(flag);
		my_sentinel("After ghost data", 320, flag);
		}
/* AFTER THIS, it's all LS_LOAD, baby */

	/* Hack -- ghosts */
	r_info[max_r_idx - 1].max_num = 1;

	{
	byte foo=0;
	if(flag == LS_SAVE)
		{
		do_byte(&foo, LS_SAVE); /* Safety Padding. It's there
					for a good reason. Trust me on
					this. Keep this at the *END*
					of the file, and do *NOT* try to
					read it. Insert any new stuff before
					this position. */
		}
	}

	/* Success */
	return (TRUE);
}


/*
 * Actually read the savefile
 */
errr rd_savefile(void)
{
	errr err=0;
#ifdef SAFER_PANICS
	char panic_fname[1024];

	if (!panicload)
	{
#endif /* SAFER_PANICS */
		/* The savefile is a binary file */
		safe_setuid_grab();
		fff = my_fopen(savefile, "rb");
		bz_prep(LS_LOAD);
		safe_setuid_drop();
#ifdef SAFER_PANICS
	}
	if (panicload)
	{
		strcpy(panic_fname, savefile);
		strcat(panic_fname, ".pnc");
		safe_setuid_grab();
		fff = my_fopen(panic_fname, "rb");
		if(fff) {bz_prep(LS_LOAD);}
		safe_setuid_drop();
	}
#endif /* SAFER_PANICS */

	/* Paranoia */
	if (!fff) return (-1);

	/* Call the sub-function */
	err = !do_savefile_aux(LS_LOAD);

	/* Check for errors */
	if (ferror(fff)) err = -1;

	/* Close the file */
	bz_done(LS_LOAD);
	my_fclose(fff);

	/* Result */
	return (err);
}

static void junkinit(void) 	/* FIXME Not sure if what this stuff does is really useful. 
				taken out of load_player_aux. How much of this is cruft? */
{
int i,j;
p_ptr->arena_number = 0;
p_ptr->inside_arena = 0;
p_ptr->inside_quest = 0;
p_ptr->exit_bldg = TRUE;
p_ptr->exit_bldg = TRUE;
p_ptr->town_num = 1;
p_ptr->wilderness_x = 4;
p_ptr->wilderness_y = 4;
for(i = 0; i < max_wild_x; i++)
	for(j = 0; j < max_wild_y; j++)
		wild_map[j][i].seed = rand_int(0x10000000);
}

static void morejunk(void)
{
sp_ptr = &sex_info[p_ptr->psex]; /* Sex */
rp_ptr = &race_info[p_ptr->prace]; /* Raceclass */
rmp_ptr = &race_mod_info[p_ptr->pracem];
cp_ptr = &class_info[p_ptr->pclass];
mp_ptr = &magic_info[p_ptr->pclass]; /* Magic */
}

#ifdef BZ_SAVES
static void do_grid(int flag)
		/* I'm still working on unifying this, obviously */
{
int y=0,x=0;
cave_type* c_ptr;
int ymax=cur_hgt, xmax=cur_wid;

int part; /* Which section of the grid we're on */

my_sentinel("Before grid",17, flag);

for(part=0;part<8;part++) /* There are 8 fields to the grid, each stored
			in a seperate data structure */
	{
	for(y=0;y<ymax;y++)
		{
		for(x=0;x<xmax;x++)
			{
			c_ptr = &cave[y][x];
			switch(part)
				{
				case 0:
				do_u16b(&c_ptr->info, flag);
				break;

				case 1:
				do_byte(&c_ptr->feat, flag);
				break;

				case 2:
				do_byte(&c_ptr->mimic, flag);
				break;

				case 3:
				do_u16b(&c_ptr->special, flag);
				break;

				case 4:
				do_u16b(&c_ptr->special2, flag);
				break;

				case 5:
				do_u16b(&c_ptr->t_idx, flag);
				break;

				case 6:
				do_u16b(&c_ptr->inscription, flag);
				break;

				case 7:
				do_byte(&c_ptr->mana, flag);
				break;
				}
			}
		}
	}
my_sentinel("In grid", 36, flag);
}
#endif /* BZ_SAVES */

#ifndef BZ_SAVES
static void do_grid(int flag)
	/* Does the grid, RLE, blahblah. RLE sucks. I hate it. */
{
int i=0,y=0,x=0;
byte count=0;
byte tmp8u=0;
u16b tmp16s=0;
cave_type* c_ptr;
byte prev_char=0;
s16b prev_s16b=0;
int ymax=cur_hgt, xmax=cur_wid;

int part; /* Which section of the grid we're on */

for(part=0;part<8;part++) /* There are 8 fields to the grid, each stored
			in a seperate RLE data structure */
	{
	if(flag == LS_SAVE)
		{
		count = 0;
		prev_s16b = 0;
		prev_char = 0; /* Clear, prepare for RLE */
		for(y=0;y<cur_hgt;y++)
			{
			for(x=0;x<cur_wid;x++)
				{
				c_ptr = &cave[y][x];
				switch(part)
					{
					case 0:
					tmp16s = c_ptr->info;
					break;

					case 1:
					tmp8u = c_ptr->feat;
					break;

					case 2:
					tmp8u = c_ptr->mimic;
					break;

					case 3:
					tmp16s = c_ptr->special;
					break;

					case 4:
					tmp16s = c_ptr->special2;
					break;

					case 5:
					tmp16s = c_ptr->t_idx;
					break;

					case 6:
					tmp16s = c_ptr->inscription;
					break;

					case 7:
					tmp8u = c_ptr->mana;
					break;
					}
				/* Flush a full run */
				if( ( ((part!=1)&&(part!=2)&&(part!=7)) &&(tmp16s != prev_s16b)) || (((part==1)||(part==2)||(part==7)) && (tmp8u != prev_char)) || (count == MAX_UCHAR))
					{
					do_byte(&count, LS_SAVE);
					switch(part)
						{
						case 0:
						case 3:
						case 4:
						case 5:
						case 6:
						do_u16b(&prev_s16b, LS_SAVE);
						prev_s16b=tmp16s;
						break;

						case 1:
						case 2:
						case 7:
						do_byte(&prev_char, LS_SAVE);
						prev_char=tmp8u;
						break;
						}
					count = 1; /* Reset RLE */
					}
				else count++; /* Otherwise, keep going */
				}
			}
		/* Fallen off the end of the world, flush anything left */
		if(count)
			{
			do_byte(&count, LS_SAVE);
			switch(part)
				{
				case 0:
				case 3:
				case 4:
				case 5:
				case 6:
				do_u16b(&prev_s16b, LS_SAVE);
				break;

				case 1:
				case 2:
				case 7:
				do_byte(&prev_char, LS_SAVE);
				break;
				}
			}
		}
	if(flag == LS_LOAD)
		{
		x=0;
		for(y=0;y < ymax;)
			{
			do_byte(&count, LS_LOAD);
			switch(part)
				{
				case 0: case 3: case 4: case 5: case 6:
				do_s16b(&tmp16s, LS_LOAD);
				break;

				case 1: case 2: case 7:
				do_byte(&tmp8u, LS_LOAD);
				break;
				}
			for(i = count;i >0; i--) /* RLE */
				{
				c_ptr = &cave[y][x];
				switch(part)
					{
					case 0:
					c_ptr->info = tmp16s;
					break;

					case 1:
					c_ptr->feat = tmp8u;
					break;

					case 2:
					c_ptr->mimic = tmp8u;
					break;

					case 3:
					c_ptr->special = tmp16s;
					break;

					case 4:
					c_ptr->special2 = tmp16s;
					break;

					case 5:
					c_ptr->t_idx = tmp16s;
					break;

					case 6:
					c_ptr->inscription = tmp16s;
					break;

					case 7:
					c_ptr->mana = tmp8u;
					break;
					}
				if(++x >= xmax)
					{
					/* Wrap */
					x = 0;
					if((++y) >= ymax) break;
					}
				}
			}
		}
	}
}

#endif /* !BZ_SAVES */

static void my_sentinel(char* place, u16b value, int flag)
	/* This function lets us know exactly where a savefile is
	broken by reading/writing conveniently a sentinel at this
	spot */
{
if(flag == LS_SAVE)
	{
	do_u16b(&value, flag);
	return;
	}
if(flag == LS_LOAD)
	{
	u16b found;
	do_u16b(&found, flag);
	if(found == value) /* All is good */
		return;
			/* All is bad */
	note(format("Savefile broken %s", place));
	return;
	}
note(format("Impossible has occurred")); /* Programmer error */
exit(0);
}
