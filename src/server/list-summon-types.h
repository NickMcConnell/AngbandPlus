/*
 * File: list-summon-types.h
 * Purpose: Summon method details
 *
 * Fields:
 * name    - summon type name
 * message - message type
 * uniq    - whether uniques are allowed
 * base1-3 - allowed monster bases if any
 * flag    - allowed racial flag if any
 * description - description of the effect
 *
 * Note that if base1 and flag are both set, any allowed race must have the flag
 * and a valid base
 */

/* name  message  uniq  base1  base2  base3  flag  description */
S(ANY, MSG_SUM_MONSTER, true, NULL, NULL, NULL, 0, "monsters")
S(KIN, MSG_SUM_MONSTER, false, NULL, NULL, NULL, 0, "")
S(MONSTER, MSG_SUM_MONSTER, false, NULL, NULL, NULL, 0, "")
S(MONSTERS, MSG_SUM_MONSTER, false, NULL, NULL, NULL, 0, "")
S(ANIMAL, MSG_SUM_ANIMAL, false, NULL, NULL, NULL, RF_ANIMAL, "")
S(SPIDER, MSG_SUM_SPIDER, false, "spider", NULL, NULL, 0, "")
S(HOUND, MSG_SUM_HOUND, false, "zephyr hound", "canine", NULL, 0, "")
S(HYDRA, MSG_SUM_HYDRA, false, "hydra", NULL, NULL, 0, "")
S(AINU, MSG_SUM_AINU, false, "ainu", NULL, NULL, 0, "")
S(DEMON, MSG_SUM_DEMON, false, NULL, NULL, NULL, RF_DEMON, "")
S(UNDEAD, MSG_SUM_UNDEAD, false, NULL, NULL, NULL, RF_UNDEAD, "non-unique undead monsters")
S(DRAGON, MSG_SUM_DRAGON, false, NULL, NULL, NULL, RF_DRAGON, "")
S(HI_DEMON, MSG_SUM_HI_DEMON, true, "major demon", NULL, NULL, 0, "")
S(HI_UNDEAD, MSG_SUM_HI_UNDEAD, true, "vampire", "wraith", "lich", 0, "")
S(HI_DRAGON, MSG_SUM_HI_DRAGON, true, "ancient dragon", NULL, NULL, 0, "")
S(WRAITH, MSG_SUM_WRAITH, true, "wraith", NULL, NULL, RF_UNIQUE, "")
S(UNIQUE, MSG_SUM_UNIQUE, true, NULL, NULL, NULL, RF_UNIQUE, "")

/* PWMAngband */
S(JELLY, MSG_SUM_MONSTER, false, "jelly", "mold", NULL, 0, "a jelly")
S(GOLEM, MSG_SUM_MONSTER, false, "golem", NULL, NULL, 0, "a golem")
S(VORTEX, MSG_SUM_MONSTER, false, "vortex", NULL, NULL, 0, "a vortex")
