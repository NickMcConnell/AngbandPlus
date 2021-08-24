/*
 * File: player.h
 * Purpose: Player implementation
 */

#ifndef PLAYER_H
#define PLAYER_H

/*
 * The range of possible indexes into tables based upon stats.
 * Currently things range from 3 to 18/220 = 40.
 */
#define STAT_RANGE  38

/*
 * Player constants
 */

/*
 * Flags for struct player.spell_flags[]
 */
#define PY_SPELL_LEARNED    0x01    /* Spell has been learned */
#define PY_SPELL_WORKED     0x02    /* Spell has been successfully tried */
#define PY_SPELL_FORGOTTEN  0x04    /* Spell has been forgotten */

#define BTH_PLUS_ADJ        3       /* Adjust BTH per plus-to-hit */

/* Terrain that the player has a chance of digging through */
enum
{
    DIGGING_TREE = 0,
    DIGGING_RUBBLE,
    DIGGING_MAGMA,
    DIGGING_QUARTZ,
    DIGGING_GRANITE,
    DIGGING_DOORS,

    DIGGING_MAX
};

/*
 * Externs
 */

/* Dungeon master flags */
#define DM_IS_MASTER        0x00000001
#define DM_SECRET_PRESENCE  0x00000002  /* Hidden dungeon master */
#define DM_CAN_MUTATE_SELF  0x00000004  /* This option allows change of the DM_ options (self) */
#define DM_CAN_ASSIGN       0x00000008  /* This option allows change of the DM_ options (other) */
#define DM___MENU           0x000000F0  /* Dungeon Master Menu: (shortcut to set all) */
#define DM_CAN_BUILD        0x00000010  /* Building menu */
#define DM_LEVEL_CONTROL    0x00000020  /* Static/unstatic level */
#define DM_CAN_SUMMON       0x00000040  /* Summon monsters */
#define DM_CAN_GENERATE     0x00000080  /* Generate vaults/items */
#define DM_MONSTER_FRIEND   0x00000100  /* Monsters are non hostile */
#define DM_INVULNERABLE     0x00000200  /* Cannot be harmed */
#define DM_GHOST_HANDS      0x00000400  /* Can interact with world even as a ghost */
#define DM_GHOST_BODY       0x00000800  /* Can carry/wield items even as a ghost */
#define DM_NEVER_DISTURB    0x00001000  /* Never disturbed (currently unused) */
#define DM_SEE_LEVEL        0x00002000  /* See all level */
#define DM_SEE_MONSTERS     0x00004000  /* Free ESP */
#define DM_SEE_PLAYERS      0x00008000  /* Can ESP other players */
#define DM_HOUSE_CONTROL    0x00010000  /* Can reset houses */

/*
 * DM macros
 */
#define is_dm_p(P) \
    (((P)->dm_flags & DM_IS_MASTER)? true: false)

#define restrict_winner(P, T) \
    ((P)->total_winner && !kf_has((T)->kind->kind_flags, KF_QUEST_ART))

#define restrict_artifacts(P, T) \
    (OPT_P(P, birth_no_artifacts) && !kf_has((T)->kind->kind_flags, KF_QUEST_ART))

/*
 * Should we shimmer stuff for this player?
 */
#define allow_shimmer(P) \
    (OPT_P(P, animate_flicker) && !(P)->use_graphics)

/*
 * Prevents abuse from level 1 characters
 */
#define newbies_cannot_drop(P) \
    (((P)->lev == 1) && cfg_newbies_cannot_drop)

#define player_undead(P) \
    ((P)->ghost && player_can_undead(P))

#define player_passwall(P) \
    ((P)->ghost || (P)->timed[TMD_WRAITHFORM])

extern int NumPlayers;

extern int stat_name_to_idx(const char *name);
extern const char *stat_idx_to_name(int type);
extern bool player_stat_inc(struct player *p, int stat);
extern bool player_stat_dec(struct player *p, int stat, bool permanent);
extern void player_exp_gain(struct player *p, s32b amount);
extern void player_exp_lose(struct player *p, s32b amount, bool permanent);
extern void player_flags(struct player *p, bitflag f[OF_SIZE]);
extern void init_players(void);
extern void free_players(void);
extern struct player *player_get(int id);
extern void player_set(int id, struct player *p);
extern void player_death_info(struct player *p, const char *died_from);
extern const char *player_safe_name(const char *name);
extern void init_player(struct player *p, int conn, bool old_history);
extern void cleanup_player(struct player *p);
extern void player_cave_new(struct player *p, int height, int width);
extern void player_cave_free(struct player *p);
extern void player_cave_clear(struct player *p, bool full);
extern bool player_square_in_bounds(struct player *p, int y, int x);
extern bool player_square_in_bounds_fully(struct player *p, int y, int x);

#endif /* PLAYER_H */
