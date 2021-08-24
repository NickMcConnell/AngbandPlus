/*
 * File: list-project-monsters.h
 * Purpose: Spell types used by project() and related functions.
 *
 * Fields:
 * name - type name
 * desc - text description of attack if blind
 * force_obv - true to force obvious if seen in project_m(), false to let the handler decide
 * color - color of the effect
 * description - description of the effect
 * PvP type - type of attack (for PvP)
 */

/* name  desc  force_obv  color  description  PvP type */
PROJ_MON(AWAY_EVIL, NULL, false, COLOUR_WHITE, "", 0)
PROJ_MON(AWAY_ALL, "feel you are somewhere else", true, COLOUR_WHITE, "teleports up to {100} squares away", 0)
PROJ_MON(TURN_UNDEAD, NULL, false, COLOUR_WHITE, "", ATT_SAVE)
PROJ_MON(TURN_ALL, "hear scary noises", false, COLOUR_WHITE, "attempts to scare for %s turns all non-unique monsters", ATT_SAVE)
PROJ_MON(DISP_UNDEAD, NULL, false, COLOUR_WHITE, "inflicts %s points of damage on all undead creatures", ATT_DAMAGE | ATT_NON_PHYS)
PROJ_MON(DISP_EVIL, NULL, false, COLOUR_WHITE, "inflicts %s points of damage on all evil monsters", ATT_DAMAGE | ATT_NON_PHYS)
PROJ_MON(DISP_ALL, NULL, true, COLOUR_WHITE, "inflicts %s points of damage on", ATT_DAMAGE | ATT_NON_PHYS)
PROJ_MON(OLD_CLONE, NULL, true, COLOUR_WHITE, "hastes, heals and magically duplicates (if not unique)", 0)
PROJ_MON(OLD_POLY, "feel bizarre", false, COLOUR_WHITE, "attempts to polymorph all non-unique monsters", 0)
PROJ_MON(OLD_HEAL, NULL, true, COLOUR_WHITE, "cures for {4d6} points of damage", 0)
PROJ_MON(OLD_SPEED, NULL, true, COLOUR_WHITE, "hastes for %s turns", 0)
PROJ_MON(OLD_SLOW, "feel something draining power from your muscles", false, COLOUR_WHITE, "attempts to slow down for %s turns all non-unique monsters", ATT_SAVE)
PROJ_MON(OLD_CONF, "hear puzzling noises", false, COLOUR_WHITE, "attempts to confuse for %s turns all non-unique monsters", ATT_SAVE)
PROJ_MON(OLD_SLEEP, "hear strange mumbling", false, COLOUR_WHITE, "attempts to put to sleep for %s turns all non-unique monsters", ATT_SAVE)
PROJ_MON(OLD_DRAIN, NULL, true, COLOUR_WHITE, "damages living monsters", ATT_DAMAGE | ATT_NON_PHYS)
PROJ_MON(PSI, "are hit by psionic energy", true, COLOUR_WHITE, "", ATT_SAVE | ATT_DAMAGE)
PROJ_MON(OLD_STUN, "hear strange mumbling", false, COLOUR_WHITE, "", ATT_SAVE)
PROJ_MON(DEATH, NULL, true, COLOUR_L_DARK, "", ATT_SAVE | ATT_DAMAGE | ATT_NON_PHYS)
PROJ_MON(PSI_DRAIN, NULL, true, COLOUR_WHITE, "", ATT_DAMAGE)
PROJ_MON(CURSE, "hear loud mumbling", true, COLOUR_WHITE, "", ATT_SAVE | ATT_DAMAGE | ATT_NON_PHYS)
PROJ_MON(CURSE2, "hear loud mumbling", true, COLOUR_WHITE, "", ATT_SAVE | ATT_DAMAGE | ATT_NON_PHYS)
PROJ_MON(DRAIN, NULL, true, COLOUR_WHITE, "", 0)
PROJ_MON(GUARD, NULL, false, COLOUR_WHITE, "", 0)
PROJ_MON(FOLLOW, NULL, false, COLOUR_WHITE, "", 0)
PROJ_MON(TELE_TO, "feel you are somewhere else", true, COLOUR_WHITE, "", 0)
PROJ_MON(TELE_LEVEL, "hear strange mumbling", true, COLOUR_WHITE, "", ATT_SAVE)
PROJ_MON(OLD_BLIND, "hear low mumbling", false, COLOUR_WHITE, "", ATT_SAVE)
PROJ_MON(DRAIN_MANA, "feel your mana draining away", true, COLOUR_WHITE, "", 0)
PROJ_MON(FORGET, "feel something trying to blank your mind", false, COLOUR_WHITE, "", ATT_SAVE)
PROJ_MON(BLAST, "are hit by psionic energy", true, COLOUR_WHITE, "", ATT_SAVE | ATT_DAMAGE)
PROJ_MON(SMASH, "are hit by psionic energy", true, COLOUR_WHITE, "", ATT_SAVE | ATT_DAMAGE)
PROJ_MON(ATTACK, NULL, false, COLOUR_WHITE, "", 0)
PROJ_MON(CONTROL, NULL, true, COLOUR_WHITE, "", 0)
PROJ_MON(PROJECT, "are hit by something", false, COLOUR_WHITE, "", ATT_RAW)
