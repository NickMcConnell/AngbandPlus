/*
 * File: list-player-timed.h
 * Purpose: Timed player properties
 *
 * CUT/STUN/BOWBRAND/ADRENALINE/BIOFEEDBACK/HARMONY -- handled separately
 *
 * Fields:
 * symbol - the effect name
 * description - description of the effect
 * on_begin - the message on beginning the effect
 * on_end - the message on ending the effect
 * on_increase - the message on the effect increasing
 * on_decrease - the message on the effect decreasing
 * flag_redraw - the things to be redrawn when the effect is set
 * flag_update - the things to be updated when the effect is set
 * msg - the message type for this effect
 * code - determines what flag type makes the effect fail: 1 means object flag,
 *        2 means resist, 3 means vulnerability
 * fail - the actual flag that causes the failure
 */

/* symbol  description  on_begin  on_end  on_increase  on_decrease  near_begin  near_end  flag_redraw  flag_update  msg  code  fail */
TMD(FAST, "haste ({+10} to speed)", "You feel yourself moving faster!", "You feel yourself slow down.", NULL, NULL, " starts moving faster.", " slows down.", 0, PU_BONUS, MSG_SPEED, 0, 0)
TMD(SLOW, "slowness ({-10} to speed)", "You feel yourself moving slower!", "You feel yourself speed up.", NULL, NULL, " starts moving slower.", " speeds up.", 0, PU_BONUS, MSG_SLOW, 1, OF_FREE_ACT)
TMD(BLIND, "blindness", "You are blind!", "You blink and your eyes clear.", NULL, NULL, " gropes around blindly.", " can see again.", (PR_MAP | PR_FLOOR), (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS), MSG_BLIND, 1, OF_PROT_BLIND)
TMD(PARALYZED, "paralysis", "You are paralyzed!", "You can move again.", NULL, NULL, " becomes rigid.", " can move again.", 0, 0, MSG_PARALYZED, 1, OF_FREE_ACT)
TMD(CONFUSED, "confusion", "You are confused!", "You are no longer confused.", "You are more confused!", "You feel a little less confused.", " appears confused.", " appears less confused.", 0, PU_BONUS, MSG_CONFUSED, 1, OF_PROT_CONF)
TMD(AFRAID, "fear", "You are terrified!", "You feel bolder now.", "You are more scared!", "You feel a little less scared.", " cowers in fear.", " appears bolder now.", 0, PU_BONUS, MSG_AFRAID, 1, OF_PROT_FEAR)
TMD(IMAGE, "hallucinations", "You feel drugged!", "You can see clearly again.", "You feel more drugged!", "You feel less drugged.", " appears drugged.", " can see clearly again.", (PR_MAP | PR_MONLIST | PR_ITEMLIST), (PU_MONSTERS | PU_BONUS), MSG_DRUGGED, 2, ELEM_CHAOS)
TMD(POISONED, "poisoning", "You are poisoned!", "You are no longer poisoned.", "You are more poisoned!", "You are less poisoned.", " appears poisoned.", " appears less poisoned.", 0, PU_BONUS, MSG_POISONED, 2, ELEM_POIS)
TMD(CUT, "cuts", NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0)
TMD(STUN, "stunning", NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 1, OF_PROT_STUN)
TMD(PROTEVIL, "protection from evil", "You feel safe from evil!", "You no longer feel safe from evil.", "You feel even safer from evil!", "You feel less safe from evil.", " is surrounded by a holy aura.", "'s holy aura disappears.", 0, 0, MSG_PROT_EVIL, 0, 0)
TMD(INVULN, "", "You feel invulnerable!", "You feel vulnerable once more.", NULL, NULL, " is surrounded by an aura of power.", "'s aura of power disappears.", PR_MAP, (PU_BONUS | PU_MONSTERS), MSG_INVULN, 0, 0)
TMD(HERO, "heroism (a bonus of {+12} to-hit)", "You feel like a hero!", "You no longer feel heroic.", "You feel more like a hero!", "You feel less heroic.", " appears heroic.", " appears less heroic.", 0, PU_BONUS, MSG_HERO, 0, 0)
TMD(SHERO, "berserk strength (a bonus of {+24} to-hit, and a penalty of {-10} to AC)", "You feel like a killing machine!", "You no longer feel berserk.", "You feel even more berserk!", "You feel less berserk.", " enters a battle rage.", " calms down.", 0, PU_BONUS, MSG_BERSERK, 0, 0)
TMD(SHIELD, "", "A mystic shield forms around your body!", "Your mystic shield crumbles away.", "The mystic shield strengthens.", "The mystic shield weakens.", " forms a mystic shield.", "'s mystic shield crumbles away.", 0, PU_BONUS, MSG_SHIELD, 0, 0)
TMD(BLESSED, "blessing (a bonus of {+5} to armor class and {+10} to-hit)", "You feel righteous!", "The prayer has expired.", "You feel more righteous!", "You feel less righteous.", " is surrounded by a bright aura.", "'s bright aura disappears.", 0, PU_BONUS, MSG_BLESSED, 0, 0)
TMD(SINVIS, "the power to see invisible", "Your eyes feel very sensitive!", "Your eyes no longer feel so sensitive.", "Your eyes feel more sensitive!", "Your eyes feel less sensitive.", "'s eyes glow brightly.", "'s eyes are back to normal.", 0, (PU_BONUS | PU_MONSTERS), MSG_SEE_INVIS, 0, 0)
TMD(SINFRA, "extra infravision ({+50} feet)", "Your eyes begin to tingle!", "Your eyes stop tingling.", "Your eyes' tingling intensifies.", "Your eyes tingle less.", "'s eyes begin to tingle.", "'s eyes stop tingling.", 0, (PU_BONUS | PU_MONSTERS), MSG_INFRARED, 0, 0)
TMD(OPP_ACID, "temporary resistance to acid", "You feel resistant to acid!", "You are no longer resistant to acid.", "You feel more resistant to acid!", "You feel less resistant to acid.", " is surrounded by an acidic aura.", "'s acidic aura disappears.", 0, PU_BONUS, MSG_RES_ACID, 3, ELEM_ACID)
TMD(OPP_ELEC, "temporary resistance to lightning", "You feel resistant to lightning!", "You are no longer resistant to lightning.", "You feel more resistant to lightning!", "You feel less resistant to lightning.", " is surrounded by an electric aura.", "'s electric aura disappears.", 0, PU_BONUS, MSG_RES_ELEC, 3, ELEM_ELEC)
TMD(OPP_FIRE, "temporary resistance to fire", "You feel resistant to fire!", "You are no longer resistant to fire.", "You feel more resistant to fire!", "You feel less resistant to fire.", " is surrounded by a fiery aura.", "'s fiery aura disappears.", 0, PU_BONUS, MSG_RES_FIRE, 3, ELEM_FIRE)
TMD(OPP_COLD, "temporary resistance to cold", "You feel resistant to cold!", "You are no longer resistant to cold.", "You feel more resistant to cold!", "You feel less resistant to cold.", " is surrounded by a chilling aura.", "'s chilling aura disappears.", 0, PU_BONUS, MSG_RES_COLD, 3, ELEM_COLD)
TMD(OPP_POIS, "temporary resistance to poison", "You feel resistant to poison!", "You are no longer resistant to poison.", "You feel more resistant to poison!", "You feel less resistant to poison.", " is surrounded by a toxic aura.", "'s toxic aura disappears.", 0, PU_BONUS, MSG_RES_POIS, 0, 0)
TMD(OPP_CONF, "temporary resistance to confusion", "You feel resistant to confusion!", "You are no longer resistant to confusion.", "You feel more resistant to confusion!", "You feel less resistant to confusion.", " is surrounded by a clear aura.", "'s clear aura disappears.", 0, PU_BONUS, MSG_GENERIC, 0, 0)
TMD(AMNESIA, "amnesia", "You feel your memories fade!", "Your memories come flooding back.", NULL, NULL, " appears disoriented.", " appears less disoriented.", 0, PU_BONUS, MSG_GENERIC, 0, 0)
TMD(ESP, "the power of telepathy", "Your mind expands!", "Your mind retracts.", "Your mind expands further.", NULL, " appears receptive.", " appears less receptive.", 0, (PU_BONUS | PU_MONSTERS), MSG_GENERIC, 0, 0)
TMD(STONESKIN, "stoneskin (a bonus of {+40} to AC, and a penalty of {-5} to speed)", "Your skin turns to stone!", "A fleshy shade returns to your skin.", NULL, NULL, "'s skin turns to stone.", "'s skin turns back to flesh.", 0, PU_BONUS, MSG_GENERIC, 0, 0)
TMD(TERROR, "terror (uncontrollable fear, with a bonus of {+10} to speed)", "You feel the need to run away, and fast!", "The urge to run dissipates.", NULL, NULL, " feels the need to run away, and fast!", "'s urge to run dissipates.", 0, PU_BONUS, MSG_AFRAID, 0, 0)
TMD(SPRINT, "sprinting ({+10} to speed, then {-10} to speed)", "You start sprinting!", "You suddenly stop sprinting.", NULL, NULL, " starts sprinting.", " suddenly stops sprinting.", 0, PU_BONUS, MSG_SPEED, 0, 0)
TMD(BOLD, "temporary resistance to fear", "You feel bold!", "You no longer feel bold.", "You feel even bolder!", "You feel less bold.", " feels bold.", " no longer feels bold.", 0, PU_BONUS, MSG_BOLD, 0, 0)
TMD(WRAITHFORM, "", "You turn into a wraith!", "You lose your wraith powers.", NULL, NULL, " turns into a wraith.", " appears more solid.", 0, 0, MSG_GENERIC, 0, 0)
TMD(MEDITATE, "", "You start a calm meditation!", "You stop your meditation.", NULL, NULL, " starts a calm meditation.", " stops meditating.", 0, PU_BONUS, MSG_GENERIC, 0, 0)
TMD(MANASHIELD, "", "You feel immortal!", "You feel less immortal.", NULL, NULL, " forms a mana shield.", "'s mana shield crumbles away.", PR_MAP, PU_MONSTERS, MSG_GENERIC, 0, 0)
TMD(INVIS, "", "You fade in the shadows!", "The shadows enveloping you dissipate.", NULL, NULL, " fades in the shadows.", " is visible once again.", 0, PU_MONSTERS, MSG_GENERIC, 0, 0)
TMD(MIMIC, "", "Your image changes!", "Your image is back to normality.", NULL, NULL, "'s image changes.", "'s image is back to normality.", 0, PU_MONSTERS, MSG_GENERIC, 0, 0)
TMD(TRAPS, "", "You can avoid all the traps!", "You should worry about traps again.", NULL, NULL, " appears stealthy.", " appears less stealthy.", 0, 0, MSG_GENERIC, 0, 0)
TMD(BOWBRAND, "", NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0)
TMD(ANCHOR, "", "The space/time continuum seems to solidify!", "The space/time continuum seems more flexible.", NULL, NULL, " tightens the space/time continuum.", " loosens the space/time continuum.", 0, PU_BONUS, MSG_GENERIC, 0, 0)
TMD(PROBTRAVEL, "the ability to cross floors and ceilings", "You feel instable!", "You feel more stable.", NULL, NULL, " appears instable.", " appears more stable.", 0, 0, MSG_GENERIC, 0, 0)
TMD(ADRENALINE, "", NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0)
TMD(BIOFEEDBACK, "", NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0)
TMD(TOUCH, "", "Your touch becomes vampiric!", "Your touch is back to normal.", NULL, NULL, "'s touch becomes vampiric.", "'s touch is back to normal.", 0, 0, MSG_GENERIC, 0, 0)
TMD(SOUL, "", "You start absorbing the souls of your foes!", "You stop absorbing the souls of your foes.", NULL, NULL, " is surrounded by a dark aura.", "'s dark aura disappears.", 0, 0, MSG_GENERIC, 0, 0)
TMD(DEADLY, "", "Your hands begin to glow black!", "Your hands stop glowing black.", NULL, NULL, "'s hands begin to glow black.", "'s hands stop glowing black.", PR_MAP, PU_MONSTERS, MSG_GENERIC, 0, 0)
TMD(EPOWER, "", "You feel attuned to the elements!", "You feel less attuned to the elements.", NULL, NULL, " appears more powerful.", " appears less powerful.", 0, 0, MSG_GENERIC, 0, 0)
TMD(ICY_AURA, "", "Your skin turns icy!", "Your skin is no longer icy.", NULL, NULL, " is surrounded by an icy aura.", "'s icy aura disappears.", 0, 0, MSG_GENERIC, 0, 0)
TMD(SGRASP, "", "Your hands are covered with lightning!", "Your hands are normal once again.", NULL, NULL, "'s hands are covered with lightning.", "'s hands are normal once again.", 0, PU_BONUS, MSG_GENERIC, 0, 0)
TMD(FARSIGHT, "", "Your sight expands!", "Your sight is back to normal.", "Your sight expands further!", NULL, "'s sight expands.", "'s sight is back to normal.", 0, PU_BONUS, MSG_GENERIC, 0, 0)
TMD(ZFARSIGHT, "", "Your sight expands!", "Your sight is back to normal.", "Your sight expands further!", NULL, "'s sight expands.", "'s sight is back to normal.", 0, PU_BONUS, MSG_GENERIC, 0, 0)
TMD(REGEN, "", "You start to regenerate quickly!", "Your regeneration rate is back to normal.", NULL, NULL, " starts to regenerate quickly.", "'s regeneration rate is back to normal.", 0, 0, MSG_GENERIC, 0, 0)
TMD(HARMONY, "", NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0)
TMD(ANTISUMMON, "", "You are surrounded by an anti-summon field!", "You should worry about summons again.", NULL, NULL, " is surrounded by a black aura.", "'s black aura disappears.", 0, 0, MSG_GENERIC, 0, 0)
