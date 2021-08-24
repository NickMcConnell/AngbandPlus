/*
 * File: list-project-environs.h
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
PROJ_ENV(LIGHT_WEAK, "are hit by something", true, COLOUR_ORANGE, "light", ATT_DAMAGE | ATT_NON_PHYS)
PROJ_ENV(DARK_WEAK, NULL, false, COLOUR_L_DARK, "", 0)
PROJ_ENV(KILL_WALL, NULL, true, COLOUR_WHITE, "rock remover", ATT_DAMAGE | ATT_NON_PHYS)
PROJ_ENV(KILL_DOOR, NULL, false, COLOUR_WHITE, "destroys all doors and disarms all traps", 0)
PROJ_ENV(KILL_TRAP, NULL, false, COLOUR_WHITE, "disarms all traps, unlocks all locked doors and reveals all secret doors", 0)
PROJ_ENV(MAKE_DOOR, NULL, false, COLOUR_WHITE, "", 0)
PROJ_ENV(MAKE_TRAP, NULL, false, COLOUR_WHITE, "creates traps on all empty squares", 0)
PROJ_ENV(STONE_WALL, NULL, false, COLOUR_WHITE, "", 0)
PROJ_ENV(RAISE, NULL, false, COLOUR_WHITE, "", 0)
PROJ_ENV(IDENTIFY, NULL, false, COLOUR_WHITE, "", 0)
