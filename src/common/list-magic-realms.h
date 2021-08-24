/*
 * File: list-magic-realms.h
 * Purpose: Spell realms
 */

/* index  stat  verb  spell_noun  book_noun  adjective */
REALM(NONE, STAT_STR, "", "", "", NULL)
REALM(ARCANE, STAT_INT, "cast", "spell", "magic book", "arcane magic")
REALM(PIOUS, STAT_WIS, "recite", "prayer", "prayer book", "divine magic")
REALM(SORCERY, STAT_INT, "cast", "spell", "sorcery book", "sorcery magic")
REALM(SHADOW, STAT_INT, "cast", "spell", "shadow book", "shadow magic")
REALM(HUNT, STAT_WIS, "cast", "spell", "hunt book", "hunting magic")
REALM(PSI, STAT_WIS, "use", "psi power", "psi book", "psi powers")
REALM(DEATH, STAT_INT, "cast", "spell", "death book", "death magic")
REALM(ELEM, STAT_INT, "cast", "spell", "elemental book", "elemental magic")
REALM(SUMMON, STAT_WIS, "cast", "spell", "summoning book", "summon magic")
REALM(GHOST, STAT_STR, "use", "power", "", NULL)
REALM(MIMIC, STAT_STR, "cast", "spell", "", NULL)