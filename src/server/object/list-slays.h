/*
 * File: list-slays.h
 * Purpose: List of slay/brand types
 */

/*
 * Entries in this table should be in ascending order of multiplier, to
 * ensure that the highest one takes precedence. Structure is name,
 * object flag, vulnerable flag, monster resist flag, multiplier, ranged verb,
 * melee verb, verb describing what the thing does when it is active,
 * description of affected creatures, brand, chance of ESP, ESP flag
 */

SLAY(XXX, FLAG_END, FLAG_END, FLAG_END, 0, NULL, NULL, NULL, NULL, NULL, 0, FLAG_END)
SLAY(ANIMAL2, OF_SLAY_ANIMAL, RF_ANIMAL, FLAG_END, 2, "pierces", "smite", "glows", "animals", NULL, 2, OF_ESP_ANIMAL)
SLAY(EVIL2, OF_SLAY_EVIL, RF_EVIL, FLAG_END, 2, "pierces", "smite", "glows", "evil creatures", NULL, 3, OF_ESP_EVIL)
SLAY(ACID2, OF_BRAND_FIZZ, FLAG_END, RF_IM_ACID, 2, "corrodes", "corrode", "fizzes", "creatures not resistant to acid", "weak acid", 0, FLAG_END)
SLAY(ELEC2, OF_BRAND_BUZZ, FLAG_END, RF_IM_ELEC, 2, "zaps", "zap", "buzzes", "creatures not resistant to lightning", "weak lightning", 0, FLAG_END)
SLAY(FIRE2, OF_BRAND_WARM, FLAG_END, RF_IM_FIRE, 2, "singes", "singe", "grows warm", "creatures not resistant to fire", "weak flames", 0, FLAG_END)
SLAY(COLD2, OF_BRAND_COOL, FLAG_END, RF_IM_COLD, 2, "chills", "chill", "grows cool", "creatures not resistant to cold", "weak frost", 0, FLAG_END)
SLAY(POISON2, OF_BRAND_ICKY, FLAG_END, RF_IM_POIS, 2, "sickens", "sicken", "glows green", "creatures not resistant to poison", "weak venom", 0, FLAG_END)
SLAY(UNDEAD3, OF_SLAY_UNDEAD, RF_UNDEAD, FLAG_END, 3, "pierces", "smite", "glows", "undead", NULL, 2, OF_ESP_UNDEAD)
SLAY(DEMON3, OF_SLAY_DEMON, RF_DEMON, FLAG_END, 3, "pierces", "smite", "glows", "demons", NULL, 2, OF_ESP_DEMON)
SLAY(ORC3, OF_SLAY_ORC, RF_ORC, FLAG_END, 3, "pierces", "smite", "glows", "orcs", NULL, 2, OF_ESP_ORC)
SLAY(TROLL3, OF_SLAY_TROLL, RF_TROLL, FLAG_END, 3, "pierces", "smite", "glows", "trolls", NULL, 2, OF_ESP_TROLL)
SLAY(GIANT3, OF_SLAY_GIANT, RF_GIANT, FLAG_END, 3, "pierces", "smite", "glows", "giants", NULL, 2, OF_ESP_GIANT)
SLAY(DRAGON3, OF_SLAY_DRAGON, RF_DRAGON, FLAG_END, 3, "pierces", "smite", "glows", "dragons", NULL, 2, OF_ESP_DRAGON)
SLAY(ACID3, OF_BRAND_ACID, FLAG_END, RF_IM_ACID, 3, "dissolves", "dissolve", "spits", "creatures not resistant to acid", "acid", 0, FLAG_END)
SLAY(ELEC3, OF_BRAND_ELEC, FLAG_END, RF_IM_ELEC, 3, "shocks", "shock", "crackles", "creatures not resistant to lightning", "lightning", 0, FLAG_END)
SLAY(FIRE3, OF_BRAND_FIRE, FLAG_END, RF_IM_FIRE, 3, "burns", "burn", "flares", "creatures not resistant to fire", "flames", 0, FLAG_END)
SLAY(COLD3, OF_BRAND_COLD, FLAG_END, RF_IM_COLD, 3, "freezes", "freeze", "grows cold", "creatures not resistant to cold", "frost", 0, FLAG_END)
SLAY(POISON3, OF_BRAND_POIS, FLAG_END, RF_IM_POIS, 3, "poisons", "poison", "seethes", "creatures not resistant to poison", "venom", 0, FLAG_END)
SLAY(DRAGON5, OF_KILL_DRAGON, RF_DRAGON, FLAG_END, 5, "deeply pierces", "fiercely smite", "glows brightly", "dragons", NULL, 1, OF_ESP_DRAGON)
SLAY(DEMON5, OF_KILL_DEMON, RF_DEMON, FLAG_END, 5, "deeply pierces", "fiercely smite", "glows brightly", "demons", NULL, 1, OF_ESP_DEMON)
SLAY(UNDEAD5, OF_KILL_UNDEAD, RF_UNDEAD, FLAG_END, 5, "deeply pierces", "fiercely smite", "glows brightly", "undead", NULL, 1, OF_ESP_UNDEAD)
SLAY(FIRE6, OF_BRAND_FIRE, RF_HURT_FIRE, FLAG_END, 6, "deeply burns", "fiercely burn", "flares", "creatures susceptible to fire", NULL, 0, FLAG_END)
SLAY(COLD6, OF_BRAND_COLD, RF_HURT_COLD, FLAG_END, 6, "deeply freezes", "fiercely freeze", "grows cold", "creatures susceptible to cold", NULL, 0, FLAG_END)

