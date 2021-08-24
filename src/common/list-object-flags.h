/*
 * File: list-object-flags.h
 * Purpose: Object flags for all objects
 *
 * index: the flag number
 * id: when the flag is IDd
 * type: what type of flag is it?
 * power: base power rating for the flag (0 means it is unused or derived)
 * message: what is printed when the flag is IDd (but see also obj-identify.c)
 *          - must be "" or contain exactly one %s
 */

/* index  id  type  power  message */
OF(PROT_FEAR, OFID_NORMAL, OFT_PROT, 6, "Your %s glows.")
OF(PROT_BLIND, OFID_NORMAL, OFT_PROT, 16, "Your %s glows.")
OF(PROT_CONF, OFID_NORMAL, OFT_PROT, 24, "Your %s glows.")
OF(PROT_STUN, OFID_NORMAL, OFT_PROT, 12, "Your %s glows.")
OF(SLOW_DIGEST, OFID_TIMED, OFT_MISC, 2, "You feel your %s slow your metabolism.")
OF(FEATHER, OFID_NORMAL, OFT_MISC, 1, "Your %s slows your fall.")
OF(REGEN, OFID_TIMED, OFT_MISC, 5, "You feel your %s speed up your recovery.")
OF(ESP_POWER, 0, OFT_MISC, 0, "")
OF(SEE_INVIS, OFID_WIELD, OFT_MISC, 6, "")
OF(FREE_ACT, OFID_NORMAL, OFT_MISC, 8, "Your %s glows.")
OF(HOLD_LIFE, OFID_NORMAL, OFT_MISC, 5, "Your %s glows.")
OF(IMPACT, OFID_NORMAL, OFT_MELEE, 10, "Your %s causes an earthquake!")
OF(BLESSED, OFID_WIELD, OFT_MELEE, 1, "")
OF(BURNS_OUT, OFID_WIELD, OFT_LIGHT, 0, "")
OF(TAKES_FUEL, OFID_WIELD, OFT_LIGHT, 0, "")
OF(NO_FUEL, OFID_WIELD, OFT_LIGHT, 5, "")
OF(IMPAIR_HP, OFID_TIMED, OFT_BAD, -9, "You feel your %s slow your recovery.")
OF(IMPAIR_MANA, OFID_TIMED, OFT_BAD, -9, "You feel your %s slow your mana recovery.")
OF(AFRAID, OFID_WIELD, OFT_BAD, -20, "")
OF(TELEPORT, OFID_NORMAL, OFT_BAD, -20, "Your %s teleports you.")
OF(AGGRAVATE, OFID_TIMED, OFT_BAD, -20, "You feel your %s aggravate things around you.")
OF(DRAIN_EXP, OFID_TIMED, OFT_BAD, -5, "You feel your %s drain your life.")
OF(LIGHT_CURSE, OFID_WIELD, OFT_CURSE, -5, "")
OF(HEAVY_CURSE, OFID_WIELD, OFT_CURSE, -15, "")
OF(PERMA_CURSE, OFID_WIELD, OFT_CURSE, -25, "")
OF(ANTI_MAGIC, OFID_TIMED, OFT_MELEE, 1, "Your %s provides an antimagic shield.")
OF(KNOWLEDGE, OFID_WIELD, OFT_KNOW, 60, "")
OF(ESP_ANIMAL, OFID_WIELD, OFT_ESP, 10, "")
OF(ESP_EVIL, OFID_WIELD, OFT_ESP, 35, "")
OF(ESP_UNDEAD, OFID_WIELD, OFT_ESP, 14, "")
OF(ESP_DEMON, OFID_WIELD, OFT_ESP, 14, "")
OF(ESP_ORC, OFID_WIELD, OFT_ESP, 3, "")
OF(ESP_TROLL, OFID_WIELD, OFT_ESP, 7, "")
OF(ESP_GIANT, OFID_WIELD, OFT_ESP, 10, "")
OF(ESP_DRAGON, OFID_WIELD, OFT_ESP, 14, "")
OF(ESP_ALL, OFID_WIELD, OFT_ESP, 70, "")
OF(ESP_RADIUS, OFID_WIELD, OFT_ESP, 35, "")
