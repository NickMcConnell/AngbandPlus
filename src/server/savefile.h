/*
 * File: savefile.h
 * Purpose: Savefile loading and saving main routines
 */

#ifndef INCLUDED_SAVEFILE_H
#define INCLUDED_SAVEFILE_H

#define ITEM_VERSION    1

/* Writing bits */
extern void wr_byte(byte v);
extern void wr_u16b(u16b v);
extern void wr_s16b(s16b v);
extern void wr_u32b(u32b v);
extern void wr_s32b(s32b v);
extern void wr_hturn(hturn* pv);
extern void wr_string(const char *str);

/* Reading bits */
extern void rd_byte(byte *ip);
extern void rd_bool(bool *ip);
extern void rd_u16b(u16b *ip);
extern void rd_s16b(s16b *ip);
extern void rd_u32b(u32b *ip);
extern void rd_s32b(s32b *ip);
extern void rd_hturn(hturn *ip);
extern void rd_string(char *str, int max);
extern void strip_bytes(int n);
extern void strip_string(int max);

/* load.c */
extern int rd_monster_memory(struct player *p);
extern int rd_object_memory(struct player *p);
extern int rd_player_artifacts(struct player *p);
extern int rd_artifacts(struct player *unused);
extern int rd_player(struct player *p);
extern int rd_player_misc(struct player *p);
extern int rd_misc(struct player *unused);
extern int rd_player_hp(struct player *p);
extern int rd_player_spells(struct player *p);
extern int rd_inventory(struct player *p);
extern int rd_stores(struct player *unused);
extern int rd_player_dungeon(struct player *p);
extern int rd_depth_dungeon(struct player *unused);
extern int rd_dungeon(struct player *unused);
extern int rd_objects(struct player *unused);
extern int rd_monsters(struct player *unused);
extern int rd_history(struct player *p);
extern int rd_header(struct player *p);
extern int rd_wild_map(struct player *p);
extern int rd_parties(struct player *unused);
extern int rd_houses(struct player *unused);
extern int rd_arenas(struct player *unused);
extern int rd_wilderness(struct player *unused);
extern int rd_player_names(struct player *unused);

/* save.c */
extern void wr_monster_memory(int Ind);
extern void wr_object_memory(int Ind);
extern void wr_player_artifacts(int Ind);
extern void wr_artifacts(int unused);
extern void wr_player(int Ind);
extern void wr_player_misc(int Ind);
extern void wr_misc(int unused);
extern void wr_player_hp(int Ind);
extern void wr_player_spells(int Ind);
extern void wr_inventory(int Ind);
extern void wr_stores(int unused);
extern void wr_player_dungeon(int Ind);
extern void wr_depth_dungeon(int depth);
extern void wr_dungeon(int unused);
extern void wr_objects(int unused);
extern void wr_monsters(int unused);
extern void wr_history(int Ind);
extern void wr_header(int Ind);
extern void wr_wild_map(int Ind);
extern void wr_parties(int unused);
extern void wr_houses(int unused);
extern void wr_arenas(int unused);
extern void wr_wilderness(int unused);
extern void wr_player_names(int unused);

#endif /* INCLUDED_SAVEFILE_H */
