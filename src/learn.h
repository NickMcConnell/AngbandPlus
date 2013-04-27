/* File: learn.h */

/*
 * Copyright (c) 1997 Ben Harrison, David Reeve Sward, Keldon Jones.
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

/*
 * Some constants for the "learn" code
 *
 * Most of these come from the "SM_xxx" flags
 */
#define DRS_FREE		14
#define DRS_MANA		15
#define DRS_RES_ACID	16
#define DRS_RES_ELEC	17
#define DRS_RES_FIRE	18
#define DRS_RES_COLD	19
#define DRS_RES_POIS	20
#define DRS_RES_FEAR	21
#define DRS_RES_LITE	22
#define DRS_RES_DARK	23
#define DRS_RES_BLIND	24
#define DRS_RES_CONFU	25
#define DRS_RES_SOUND	26
#define DRS_RES_SHARD	27
#define DRS_RES_NEXUS	28
#define DRS_RES_NETHR	29
#define DRS_RES_CHAOS	30
#define DRS_RES_DISEN	31

/*
 * Some bit-flags for the "smart" field of "monster_type".
 *
 * Most of these map to the "TR2_xxx" flags.
 */
#define SM_OPP_ACID		0x00000001
#define SM_OPP_ELEC		0x00000002
#define SM_OPP_FIRE		0x00000004
#define SM_OPP_COLD		0x00000008
#define SM_OPP_POIS		0x00000010
#define SM_OPP_XXX1		0x00000020
#define SM_OPP_XXX2		0x00000040
#define SM_OPP_XXX3		0x00000080
#define SM_IMM_XXX5		0x00000100
#define SM_IMM_XXX6		0x00000200
#define SM_IMM_FREE		0x00000400
#define SM_IMM_MANA		0x00000800
#define SM_IMM_ACID		0x00001000
#define SM_IMM_ELEC		0x00002000
#define SM_IMM_FIRE		0x00004000
#define SM_IMM_COLD		0x00008000
#define SM_RES_ACID		0x00010000
#define SM_RES_ELEC		0x00020000
#define SM_RES_FIRE		0x00040000
#define SM_RES_COLD		0x00080000
#define SM_RES_POIS		0x00100000
#define SM_RES_FEAR		0x00200000
#define SM_RES_LITE		0x00400000
#define SM_RES_DARK		0x00800000
#define SM_RES_BLIND	0x01000000
#define SM_RES_CONFU	0x02000000
#define SM_RES_SOUND	0x04000000
#define SM_RES_SHARD	0x08000000
#define SM_RES_NEXUS	0x10000000
#define SM_RES_NETHR	0x20000000
#define SM_RES_CHAOS	0x40000000
#define SM_RES_DISEN	0x80000000

/* monster2.c */
extern void update_smart_learn(const m_idx_type m_idx, int what);

