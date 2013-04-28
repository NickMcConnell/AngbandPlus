/*
 * cvtscore.exe --
 *     Convert a ZAngband highscore file to another format.
 *     Copyright (c) Tim Baker 1998
 *
 * Usage:
 *    cvtscore oldFile oldVersion newFile newVersion
 * OldFile must exist, newFile must not exist.
 */

#include <stdio.h>

/* Master record encompassing all high-score records */
typedef struct highscore_master highscore_master;

struct highscore_master
{
	char what[8];		/* Version info (string) */

	char pts[10];		/* Total Score (number) */

	char gold[10];		/* Total Gold (number) */

	char turns[10];		/* Turns Taken (number) */

	char day[10];		/* Time stamp (string) */

	char who[16];		/* Player Name (string) */

	char uid[8];		/* Player UID (number) */

	char sex[2];		/* Player Sex (string) */
	char p_r[3];		/* Player Race (number) */
	char p_c[3];		/* Player Class (number) */

	char cur_lev[4];		/* Current Player Level (number) */
	char cur_dun[4];		/* Current Dungeon Level (number) */
	char max_lev[4];		/* Max Player Level (number) */
	char max_dun[4];		/* Max Dungeon Level (number) */

	char arena_number[4];	/* Arena level attained -KMW- */
	char inside_arena[4];   /* Did the player die in the arena? */
	char inside_quest[4];   /* Did the player die in a quest? */
	char exit_bldg[4];	/* Can the player exit arena? Goal obtained? -KMW- */

	char how[32];		/* Method of death (string) */
};

/*
 * 2.1.1 == 2.2.1
 * 2.1.3b == 2.1.4b == 2.2.0
 */
typedef struct high_score_211 high_score_211;
typedef struct high_score_220 high_score_220;

typedef struct high_score_211 high_score_221;
typedef struct high_score_220 high_score_213b;
typedef struct high_score_220 high_score_214b;

struct high_score_211
{
	char what[8];		/* Version info (string) */

	char pts[10];		/* Total Score (number) */

	char gold[10];		/* Total Gold (number) */

	char turns[10];		/* Turns Taken (number) */

	char day[10];		/* Time stamp (string) */

	char who[16];		/* Player Name (string) */

	char uid[8];		/* Player UID (number) */

	char sex[2];		/* Player Sex (string) */
	char p_r[3];		/* Player Race (number) */
	char p_c[3];		/* Player Class (number) */

	char cur_lev[4];		/* Current Player Level (number) */
	char cur_dun[4];		/* Current Dungeon Level (number) */
	char max_lev[4];		/* Max Player Level (number) */
	char max_dun[4];		/* Max Dungeon Level (number) */

	char how[32];		/* Method of death (string) */
};

struct high_score_220
{
	char what[8];		/* Version info (string) */

	char pts[10];		/* Total Score (number) */

	char gold[10];		/* Total Gold (number) */

	char turns[10];		/* Turns Taken (number) */

	char day[10];		/* Time stamp (string) */

	char who[16];		/* Player Name (string) */

	char uid[8];		/* Player UID (number) */

	char sex[2];		/* Player Sex (string) */
	char p_r[3];		/* Player Race (number) */
	char p_c[3];		/* Player Class (number) */

	char cur_lev[4];		/* Current Player Level (number) */
	char cur_dun[4];		/* Current Dungeon Level (number) */
	char max_lev[4];		/* Max Player Level (number) */
	char max_dun[4];		/* Max Dungeon Level (number) */

	char arena_number[4];	/* Arena level attained -KMW- */
	char inside_arena[4];   /* Did the player die in the arena? */
	char inside_quest[4];   /* Did the player die in a quest? */
	char exit_bldg[4];	/* Can the player exit arena? Goal obtained? -KMW- */

	char how[32];		/* Method of death (string) */
};

/* Fill a master record with default values */
int highscore_master_wipe(highscore_master *master)
{
	sprintf(master->what, "0.0.0");
	sprintf(master->pts, "%9lu", 0L);
	sprintf(master->gold, "%9lu", 0L);
	sprintf(master->turns, "%9lu", 0L);
	sprintf(master->day, "0/0/0");
	sprintf(master->who, "Nobody");
	sprintf(master->uid, "0");
	sprintf(master->sex, "m");
	sprintf(master->p_r, "%2d", 0);
	sprintf(master->p_c, "%2d", 0);
	sprintf(master->cur_lev, "%3d", 0);
	sprintf(master->cur_dun, "%3d", 0);
	sprintf(master->max_lev, "%3d", 0);
	sprintf(master->max_dun, "%3d", 0);
	sprintf(master->arena_number, "%3d", 0);
	sprintf(master->inside_arena, "%3d", 0);
	sprintf(master->inside_quest, "%3d", 0);
	sprintf(master->exit_bldg, "%3d", 0);
	sprintf(master->how, "%-.31s", "Nothing");
	return 0;
}

/* Convert a 2.1.1 record to master record */
int v211_to_master(char *buf, highscore_master *master)
{
	high_score_211 *score = (high_score_211 *) buf;

	strcpy(master->what, score->what);
	strcpy(master->pts, score->pts);
	strcpy(master->gold, score->gold);
	strcpy(master->turns, score->turns);
	strcpy(master->day, score->day);
	strcpy(master->who, score->who);
	strcpy(master->uid, score->uid);
	strcpy(master->sex, score->sex);
	strcpy(master->p_r, score->p_r);
	strcpy(master->p_c, score->p_c);
	strcpy(master->cur_lev, score->cur_lev);
	strcpy(master->cur_dun, score->cur_dun);
	strcpy(master->max_lev, score->max_lev);
	strcpy(master->max_dun, score->max_dun);
	strcpy(master->how, score->how);
	return 0;
}

/* Convert a master record to 2.1.1 record */
int master_to_211(char *buf, highscore_master *master)
{
	high_score_211 *score = (high_score_211 *) buf;

	strcpy(score->what, master->what);
	strcpy(score->pts, master->pts);
	strcpy(score->gold, master->gold);
	strcpy(score->turns, master->turns);
	strcpy(score->day, master->day);
	strcpy(score->who, master->who);
	strcpy(score->uid, master->uid);
	strcpy(score->sex, master->sex);
	strcpy(score->p_r, master->p_r);
	strcpy(score->p_c, master->p_c);
	strcpy(score->cur_lev, master->cur_lev);
	strcpy(score->cur_dun, master->cur_dun);
	strcpy(score->max_lev, master->max_lev);
	strcpy(score->max_dun, master->max_dun);
	strcpy(score->how, master->how);
	return 0;
}

/* Convert a 2.1.3b record to master record */
int v213b_to_master(char *buf, highscore_master *master)
{
	high_score_213b *score = (high_score_213b *) buf;
	
	strcpy(master->what, score->what);
	strcpy(master->pts, score->pts);
	strcpy(master->gold, score->gold);
	strcpy(master->turns, score->turns);
	strcpy(master->day, score->day);
	strcpy(master->who, score->who);
	strcpy(master->uid, score->uid);
	strcpy(master->sex, score->sex);
	strcpy(master->p_r, score->p_r);
	strcpy(master->p_c, score->p_c);
	strcpy(master->cur_lev, score->cur_lev);
	strcpy(master->cur_dun, score->cur_dun);
	strcpy(master->max_lev, score->max_lev);
	strcpy(master->max_dun, score->max_dun);
	strcpy(master->arena_number, score->arena_number);
	strcpy(master->inside_arena, score->inside_arena);
	strcpy(master->inside_quest, score->inside_quest);
	strcpy(master->exit_bldg, score->exit_bldg);
	strcpy(master->how, score->how);
	return 0;
}

/* Convert a master record to 2.1.3b record */
int master_to_213b(char *buf, highscore_master *master)
{
	high_score_213b *score = (high_score_213b *) buf;

	strcpy(score->what, master->what);
	strcpy(score->pts, master->pts);
	strcpy(score->gold, master->gold);
	strcpy(score->turns, master->turns);
	strcpy(score->day, master->day);
	strcpy(score->who, master->who);
	strcpy(score->uid, master->uid);
	strcpy(score->sex, master->sex);
	strcpy(score->p_r, master->p_r);
	strcpy(score->p_c, master->p_c);
	strcpy(score->cur_lev, master->cur_lev);
	strcpy(score->cur_dun, master->cur_dun);
	strcpy(score->max_lev, master->max_lev);
	strcpy(score->max_dun, master->max_dun);
	strcpy(score->arena_number, master->arena_number);
	strcpy(score->inside_arena, master->inside_arena);
	strcpy(score->inside_quest, master->inside_quest);
	strcpy(score->exit_bldg, master->exit_bldg);
	strcpy(score->how, master->how);
	return 0;
}

typedef struct vers_info vers_info;

struct vers_info {
	char *vers; /* Version string */
	size_t size; /* Size of highscore record */
	int (*to_master)(char *score, highscore_master *master);
	int (*from_master)(char *score, highscore_master *master);
};

vers_info g_vers[] = {
	{"211", sizeof(high_score_211), v211_to_master, master_to_211},
	{"213b", sizeof(high_score_213b), v213b_to_master, master_to_213b},
	{"214b", sizeof(high_score_214b), v213b_to_master, master_to_213b},
	{"220", sizeof(high_score_220), v213b_to_master, master_to_213b},
	{"221", sizeof(high_score_221), v211_to_master, master_to_211},
	{NULL, 0}
};

int main(int argc, char **argv)
{
	char *oldFile, *newFile, *oldVers, *newVers;
	FILE *oldFP, *newFP;
	int i;
	vers_info *oldVersPtr, *newVersPtr;
	
	if (argc != 5)
	{
		fprintf(stderr, "Usage: %s oldFile oldVersion newFile newVersion\n", argv[0]);
		return 1;
	}

	oldFile = argv[1];
	oldVers = argv[2];
	newFile = argv[3];
	newVers = argv[4];

	/* Verify the old version */
	for (i = 0; g_vers[i].vers; i++)
	{
		if (!strcmp(oldVers, g_vers[i].vers)) break;
	}
	if (g_vers[i].vers == NULL)
	{
		fprintf(stderr, "unsupported version \"%s\"\n", oldVers);
		return 1;
	}
	oldVersPtr = &g_vers[i];

	/* Verify the new version */
	for (i = 0; g_vers[i].vers; i++)
	{
		if (!strcmp(newVers, g_vers[i].vers)) break;
	}
	if (g_vers[i].vers == NULL)
	{
		fprintf(stderr, "unsupported version \"%s\"\n", newVers);
		return 1;
	}
	newVersPtr = &g_vers[i];

	/* Open the old file for reading */
	oldFP = fopen(oldFile, "rb");
	if (!oldFP)
	{
		fprintf(stderr, "error opening \"%s\" for reading\n", oldFile);
		return 1;
	}

	/* Verify the new file doesn't exist */
	newFP = fopen(newFile, "rb");
	if (newFP)
	{
		fprintf(stderr, "file \"%s\" exists\n", newFile);
		fclose(oldFP);
		fclose(newFP);
		return 1;
	}

	/* Create the new file for writing */
	newFP = fopen(newFile, "wb");
	if (!newFP)
	{
		fprintf(stderr, "error opening \"%s\" for writing\n", newFile);
		fclose(oldFP);
		return 1;
	}
	
	while (1)
	{
		size_t n;
		char buf[512];

		n = fread(&buf, oldVersPtr->size, 1, oldFP);
		if (n == 1)
		{
			highscore_master master;
			highscore_master_wipe(&master);

			/* Convert old record to master record */
			(*oldVersPtr->to_master)(buf, &master);
			
			fprintf(stdout, "%s: %s, killed by %s on level %s\n",
				master.what, master.who, master.how, master.cur_dun);

			/* Convert master record to new record */
			(*newVersPtr->from_master)(buf, &master);

			/* Write the new record */
			n = fwrite(&buf, newVersPtr->size, 1, newFP);
			if (n != 1)
			{
				fprintf(stderr, "error writing record\n");
				break;
			}
			
			continue;
		}

		/* Done */
		break;
	}

	/* Clean up */
	fclose(oldFP);
	fclose(newFP);
	
	return 0;
}
