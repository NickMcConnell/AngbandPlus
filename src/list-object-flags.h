/**
 * \file list-object-flags.h
 * \brief object flags for all objects
 *
 * Changing flag order will break savefiles. Flags
 * below start from 1 on line 14, so a flag's sequence number is its line
 * number minus 13.
 *
 * Each sustain flag (SUST_*) has a matching stat in src/list-stats.h,
 * which should be at the same index in that file as the sustain in this file.
 *
 * The second argument to OF is the label used in the debugging commands
 * object flag display.  At most the first five characters are used.
 *
 * Flag properties are defined in lib/gamedata/object_property.txt
 */
OF(SUST_STR, " sStr")
OF(SUST_INT, " sInt")
OF(SUST_WIS, " sWis")
OF(SUST_DEX, " sDex")
OF(SUST_CON, " sCon")
OF(SUST_CHR, " sChr")
OF(SUST_SPD, " sSpd")
OF(PROT_FEAR, "pFear")
OF(PROT_BLIND, "pBlnd")
OF(PROT_CONF, "pConf")
OF(PROT_STUN, "pStun")
OF(SLOW_DIGEST, "SloDi")
OF(FEATHER, "Feath")
OF(REGEN, "Regen")
OF(TELEPATHY, "  ESP")
OF(SENSE_ANIMAL, "Animl")
OF(SENSE_METAL, "Metal")
OF(SEE_INVIS, "S.Inv")
OF(FREE_ACT, "FrAct")
OF(HOLD_LIFE, "HLife")
OF(IMPACT, "Impct")
OF(BLESSED, " Bless")
OF(BURNS_OUT, "BuOut")
OF(TAKES_FUEL, "TFuel")
OF(NO_FUEL, "NFuel")
OF(IMPAIR_HP, "ImpHP")
OF(IMPAIR_MANA, "ImpSP")
OF(AFRAID, " Fear")
OF(NO_TELEPORT, "NoTel")
OF(AGGRAVATE, "Aggro")
OF(DRAIN_EXP, "DrExp")
OF(STICKY, "Stick")
OF(FRAGILE, "Fragl")
OF(LIGHT_2, "Lite2")
OF(LIGHT_3, "Lite3")
OF(LIGHT_4, "Lite4")
OF(LIGHT_5, "Lite5")
OF(DIG_1, " Dig1")
OF(DIG_2, " Dig2")
OF(DIG_3, " Dig3")
OF(EXPLODE, "Explo")
OF(TRAP_IMMUNE, "TrpIm")
OF(THROWING, "Throw")
OF(STOLEN, "Stole")
OF(NO_ACTIVATION, "NoAct")
OF(HIDE_ACTIVATION, "HdAct")
OF(QUEST_SPECIAL, "Quest")
OF(NO_EQUIP, "NoEqu")
OF(MAGNETIC, "Magnt")
OF(MAX, @@)
