#include "angband.h"

#include "mon_spell.h"
#include <assert.h>

/*************************************************************************
 * Parse Tables (for r_info.txt)
 *
 * The parser will first look in various parse tables  for spell names,
 * such as THROW, SHOOT or HASTE. If not present, then the token prefix
 * determines the spell type (such as BR_ for MST_BREATH) while the
 * token suffix determines the effect type (currently we use the GF_*
 * system, though this needs some reworking). For example, BA_ACID for
 * an acid ball {MST_BALL, GF_ACID}.
 *
 * Now, sometimes you want to override the default combinatorics. For
 * example, BA_MANA is indeed {MST_BALL, GF_MANA}, but it is not "mana ball"
 * at all, but "mana storm". Similar for BA_CHAOS ("invoke logrus") and
 * a few others. Thus, the parse tables give you the option for display
 * overrides, while the default fallback combinatorics make it easier to
 * add new stuff without tabling everything up (Though if you want default
 * damage numbers, you'll need to add those by hand ... we also currently
 * assert these cases into errors but they may change).
 *
 * You can BA_MANA for a mana storm, but you can also BA_MANA(350) for a 350hp
 * mana storm, bypassing the normal damage calculation.
 *
 * You can BR_FIRE to breathe fire, but you can also BR_FIRE(80%) to use
 * 80% of chp for damage rather than the default of just 20%.
 *
 * You can BA_NETHER for a normal damage nether ball, BA_NETHER(10d10+200)
 * for custom damage, or even S:POWER_200% | BA_NETHER to use double the
 * normal damage calculation. POWER_* only works for spells that use damage
 * dice (so it skips breaths).
 *
 ************************************************************************/

/* Spells: We use a series of parse tables to handle overrides, as
 * discussed above. Each MST_* code gets its own table to preserve
 * my sanity. Note: We store spell lore by the numeric code, so
 * any changes to these values will cause problems! */
typedef struct {
    cptr token;
    mon_spell_id_t id;
    mon_spell_display_t display;
    u32b flags;
} _parse_t, *_parse_ptr;

/* MST_ANNOY */
enum {
    ANNOY_AMNESIA,
    ANNOY_ANIMATE_DEAD,
    ANNOY_BLIND,
    ANNOY_CONFUSE,
    ANNOY_DARKNESS,
    ANNOY_PARALYZE,
    ANNOY_SCARE,
    ANNOY_SHRIEK,
    ANNOY_SLOW,
    ANNOY_TELE_LEVEL,
    ANNOY_TELE_TO,
    ANNOY_TRAPS,
    ANNOY_WORLD,
    ANNOY_EGO_WHIP,
    ANNOY_QUAKE,
};
static _parse_t _annoy_tbl[] = {
    { "AMNESIA", { MST_ANNOY, ANNOY_AMNESIA },
        { "Amnesia", TERM_L_BLUE,
          "$CASTER tries to blank your mind.",
          "$CASTER tries to blank your mind.",
          "$CASTER tries to blank $TARGET_POS mind.",
          "You try to blank $TARGET_POS mind."}, MSF_TARGET | MSF_DIRECT},
    { "ANIM_DEAD", { MST_ANNOY, ANNOY_ANIMATE_DEAD },
        { "Animate Dead", TERM_L_DARK,
          "$CASTER casts a spell to revive the dead.",
          "$CASTER mumbles.",
          "",
          "You cast a spell to revive the dead."}},
    { "BLIND", { MST_ANNOY, ANNOY_BLIND },
        { "Blind", TERM_WHITE,
          "$CASTER casts a spell, burning your eyes!",
          "$CASTER mumbles.",
          "$CASTER casts a spell, burning $TARGET_POS eyes!",
          "You cast a spell, burning $TARGET_POS eyes!"}, MSF_TARGET | MSF_DIRECT},
    { "CONFUSE", { MST_ANNOY, ANNOY_CONFUSE },
        { "Confuse", TERM_L_UMBER,
          "$CASTER creates a mesmerizing illusion.",
          "$CASTER mumbles, and you hear puzzling noises.",
          "$CASTER creates a mesmerizing illusion at $TARGET.",
          "You create a mesmerizing illusion at $TARGET."}, MSF_TARGET | MSF_DIRECT},
    { "DARKNESS", { MST_ANNOY, ANNOY_DARKNESS },
        { "Create Darkness", TERM_L_DARK }},
    { "PARALYZE", { MST_ANNOY, ANNOY_PARALYZE },
        { "Paralyze", TERM_RED,
          "$CASTER stares deep into your eyes!",
          "$CASTER mumbles.",
          "$CASTER stares deep into $TARGET_POS eyes!",
          "You stare deep into $TARGET_POS eyes!"}, MSF_TARGET | MSF_DIRECT},
    { "SCARE", { MST_ANNOY, ANNOY_SCARE },
        { "Terrify", TERM_RED,
          "$CASTER casts a fearful illusion.",
          "$CASTER mumbles, and you hear scary noises.",
          "$CASTER casts a fearful illusion at $TARGET.",
          "You cast a fearful illusion at $TARGET."}, MSF_TARGET | MSF_DIRECT},
    { "SLOW", { MST_ANNOY, ANNOY_SLOW },
        { "Slow", TERM_L_UMBER,
          "$CASTER drains power from your muscles!",
          "$CASTER drains power from your muscles!",
          "$CASTER drains power from $TARGET_POS muscles!",
          "You drain power from $TARGET_POS muscles!"}, MSF_TARGET | MSF_DIRECT},
    { "SHRIEK", { MST_ANNOY, ANNOY_SHRIEK },
        { "Shriek", TERM_L_BLUE,
          "$CASTER makes a high pitched shriek.",
          "$CASTER makes a high pitched shriek.",
          "$CASTER makes a high pitched shriek.",
          "You make a high pitched shriek."}, MSF_INNATE },
    { "TELE_LEVEL", { MST_ANNOY, ANNOY_TELE_LEVEL },
        { "Teleport Level", TERM_WHITE,
          "$CASTER gestures at your feet.",
          "$CASTER mumbles strangely.",
          "$CASTER gestures at $TARGET_POS feet.",
          "You gesture at $TARGET_POS feet."}, MSF_TARGET | MSF_DIRECT},
    { "TELE_TO", { MST_ANNOY, ANNOY_TELE_TO },
        { "Teleport To", TERM_WHITE,
          "$CASTER commands you to return.",
          "$CASTER mumbles.",
          "$CASTER commands $TARGET to return.",
          "You command $TARGET to return."}, MSF_TARGET | MSF_DIRECT},
    { "TRAPS", { MST_ANNOY, ANNOY_TRAPS },
        { "Create Traps", TERM_WHITE,
          "$CASTER casts a spell and cackles evilly.",
          "$CASTER mumbles gleefully.",
          "$CASTER casts a spell and cackles evilly.",
          "You create a trap." }},
    { "WORLD", { MST_ANNOY, ANNOY_WORLD },
        { "Stop Time", TERM_L_BLUE}},
    { "EGO_WHIP", { MST_ANNOY, ANNOY_EGO_WHIP },
        { "Ego Whip", TERM_L_BLUE,
          "$CASTER focuses on your mind!",
          "$CASTER focuses on your mind!",
          "$CASTER focuses on $TARGET_POS mind!",
          "You focus on $TARGET_POS mind!"}, MSF_TARGET | MSF_DIRECT},
    { "QUAKE", { MST_ANNOY, ANNOY_QUAKE },
        { "Earthquake", TERM_UMBER,
          "$CASTER strikes the ground powerfully with his staff!",
          "$CASTER mumbles.",
          "$CASTER strikes the ground powerfully with his staff!",
          "You conjure an <color:u>Earthquake</color>." }},
    {0}
};

/* MST_BIFF */
enum {
    BIFF_ANTI_MAGIC,
    BIFF_DISPEL_MAGIC,
    BIFF_POLYMORPH,
};
static _parse_t _biff_tbl[] = {
    { "ANTI_MAGIC", { MST_BIFF, BIFF_ANTI_MAGIC },
        { "Anti-Magic", TERM_L_BLUE,
          "$CASTER invokes <color:B>Anti-Magic</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER invokes <color:B>Anti-Magic</color> at $TARGET.",
          "You invoke <color:B>Anti-Magic</color>." }, MSF_TARGET | MSF_DIRECT},
    { "DISPEL_MAGIC", { MST_BIFF, BIFF_DISPEL_MAGIC },
        { "Dispel Magic", TERM_L_BLUE,
          "$CASTER invokes <color:B>Dispel Magic</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER invokes <color:B>Dispel Magic</color> at $TARGET.",
          "You invoke <color:B>Dispel Magic</color>." }, MSF_TARGET | MSF_DIRECT},
    { "POLYMORPH", { MST_BIFF, BIFF_POLYMORPH },
        { "Polymorph Other", TERM_RED,
          "$CASTER invokes <color:r>Polymorph Other</color>.",
          "$CASTER mumbles powerfully.",
          "",
          "You invoke <color:r>Polymorph Other</color>." }, MSF_TARGET},
    {0}
};

/* MST_BUFF */
enum {
    BUFF_HASTE,
    BUFF_INVULN,
    BUFF_BERSERK,
    BUFF_PROT_EVIL,
    BUFF_PROT_GOOD,
};
static _parse_t _buff_tbl[] = {
    { "HASTE", { MST_BUFF, BUFF_HASTE },
        { "Haste Self", TERM_WHITE,
          "$CASTER concentrates on $CASTER_POS body.",
          "$CASTER mumbles.",
          "$CASTER concentrates on $CASTER_POS body.",
          "You concentrate on your body." }},
    { "INVULN", { MST_BUFF, BUFF_INVULN },
        { "Invulnerability", TERM_YELLOW,
          "$CASTER casts a <color:y>Globe of Invulnerability</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER casts a <color:y>Globe of Invulnerability</color>.",
          "You cast a <color:y>Globe of Invulnerability</color>." }},
    { "BERSERK", { MST_BUFF, BUFF_BERSERK },
        { "Berserk", TERM_RED,
          "$CASTER enters into a berserk frenzy.",
          "$CASTER sounds furious!",
          "$CASTER enters into a berserk frenzy.",
          "You enter into a berserk frenzy."}, MSF_INNATE},
    { "PROT_EVIL", { MST_BUFF, BUFF_PROT_EVIL },
        { "Protection from Evil", TERM_YELLOW,
          "$CASTER casts <color:y>Protection from Evil</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER casts <color:y>Protection from Evil</color>.",
          "You cast <color:y>Protection from Evil</color>." }},
    { "PROT_GOOD", { MST_BUFF, BUFF_PROT_GOOD },
        { "Protection from Good", TERM_L_DARK,
          "$CASTER casts <color:D>Protection from Good</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER casts <color:D>Protection from Good</color>.",
          "You cast <color:D>Protection from Good</color>." }},
    {0}
};

/* MST_BALL */
static _parse_t _ball_tbl[] = {
    { "BA_CHAOS", { MST_BALL, GF_CHAOS },
        { "Invoke Logrus", TERM_VIOLET,
          "$CASTER invokes a <color:v>Raw Logrus</color>.",
          "$CASTER mumbles frighteningly.",
          "$CASTER invokes a <color:v>Raw Logrus</color> at $TARGET.",
          "You invoke a <color:v>Raw Logrus</color>." }, MSF_TARGET | MSF_BALL4},
    { "BA_DARK", { MST_BALL, GF_DARK },
        { "Darkness Storm", TERM_L_DARK,
          "$CASTER invokes a <color:D>Darkness Storm</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER invokes a <color:D>Darkness Storm</color> at $TARGET.",
          "You invoke a <color:D>Darkness Storm</color>." }, MSF_TARGET | MSF_BALL4},
    { "BA_LIGHT", { MST_BALL, GF_LIGHT },
        { "Starburst", TERM_YELLOW,
          "$CASTER invokes a <color:y>Starburst</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER invokes a <color:y>Starburst</color> at $TARGET.",
          "You invoke a <color:y>Starburst</color>." }, MSF_TARGET | MSF_BALL4},
    { "MANA_STORM", { MST_BALL, GF_MANA },
        { "Mana Storm", TERM_L_BLUE,
          "$CASTER invokes a <color:B>Mana Storm</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER invokes a <color:B>Mana Storm</color> at $TARGET.",
          "You invoke a <color:B>Mana Storm</color>." }, MSF_TARGET | MSF_BALL4},
    { "BA_DISENCHANT", { MST_BALL, GF_DISENCHANT },
        { "Anti-magic Storm", TERM_VIOLET,
          "$CASTER invokes an <color:v>Anti-magic Storm</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER invokes an <color:v>Anti-magic Storm</color> at $TARGET.",
          "You invoke an <color:v>Anti-magic Storm</color>." }, MSF_TARGET | MSF_BALL4},
    { "BA_SOUND", { MST_BALL, GF_SOUND },
        { "Sonic Storm", TERM_ORANGE,
          "$CASTER invokes an <color:o>Sonic Storm</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER invokes an <color:o>Sonic Storm</color> at $TARGET.",
          "You invoke an <color:o>Sonic Storm</color>." }, MSF_TARGET | MSF_BALL4},
    { "BA_ICE", { MST_BALL, GF_ICE },
        { "Blizzard", TERM_L_WHITE,
          "$CASTER conjures a <color:W>Blizzard</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER conjures a <color:W>Blizzard</color> at $TARGET.",
          "You conjure a <color:W>Blizzard</color>." }, MSF_TARGET | MSF_BALL4},
    { "HURRICANE", { MST_BALL, GF_STORM },
        { "Hurricane", TERM_BLUE,
          "$CASTER conjures a <color:b>Hurricane</color>.",
          "$CASTER mumbles powerfully.",
          "$CASTER conjures a <color:b>Hurricane</color> at $TARGET.",
          "You conjure a <color:b>Hurricane</color>." }, MSF_TARGET | MSF_BALL4},
    { "BA_NUKE", { MST_BALL, GF_NUKE },
        { "Radiation Ball", TERM_L_GREEN,
          "$CASTER casts a <color:G>Ball of Radiation</color>.",
          "$CASTER mumbles.",
          "$CASTER casts a <color:G>Ball of Radiation</color> at $TARGET.",
          "You cast a <color:G>Ball of Radiation</color>." }, MSF_TARGET},
    { "BA_POISON", { MST_BALL, GF_POIS },
        { "Stinking Cloud", TERM_L_GREEN,
          "$CASTER casts a <color:G>Stinking Cloud</color>.",
          "$CASTER mumbles.",
          "$CASTER casts a <color:G>Stinking Cloud</color> at $TARGET.",
          "You cast a <color:G>Stinking Cloud</color>." }, MSF_TARGET},
    { "BA_WATER", { MST_BALL, GF_WATER },
        { "Whirlpool", TERM_L_BLUE,
          "$CASTER gestures fluidly. You are engulfed in a <color:B>Whirlpool</color>.",
          "$CASTER mumbles. You are engulfed in a <color:B>Whirlpool</color>.",
          "$CASTER gestures fluidly. $TARGET is engulfed in a <color:B>Whirlpool</color>.",
          "You gesture fluidly." }, MSF_TARGET},
    { "DRAIN_MANA", { MST_BALL, GF_DRAIN_MANA },
        { "Drain Mana", TERM_L_BLUE,
          "$CASTER drains psychic energy from you.",
          "$CASTER drains psychic energy from you.",
          "$CASTER drains psychic energy from $TARGET.",
          "You drain psychic energy from $TARGET."}, MSF_BALL0 | MSF_TARGET },
    { "BRAIN_SMASH", { MST_BALL, GF_BRAIN_SMASH },
        { "Brain Smash", TERM_L_BLUE,
          "$CASTER gazes deep into your eyes.",
          "You feel something focusing on your mind.", 
          "$CASTER gazes deep into the eyes of $TARGET.",
          "You gaze deep into the eyes of $TARGET." }, MSF_BALL0 | MSF_TARGET },
    { "MIND_BLAST", { MST_BALL, GF_MIND_BLAST },
        { "Mind Blast", TERM_L_BLUE,
          "$CASTER gazes deep into your eyes.",
          "You feel something focusing on your mind.", 
          "$CASTER gazes deep into the eyes of $TARGET.",
          "You gaze deep into the eyes of $TARGET." }, MSF_BALL0 | MSF_TARGET},
    { "PULVERISE", { MST_BALL, GF_TELEKINESIS },
        { "Pulverise", TERM_L_BLUE,
          "$CASTER <color:B>pulverises</color> you.",
          "Something <color:B>pulverises</color> you.",
          "$CASTER <color:B>pulverises</color> $TARGET.",
          "" }, MSF_TARGET},
    { "ROCKET", { MST_BALL, GF_ROCKET },
        { "Rocket", TERM_L_UMBER,
          "$CASTER fires a <color:U>Rocket</color>.",
          "$CASTER shoots something.",
          "$CASTER fires a <color:U>Rocket</color> at $TARGET.",
          "You fire a <color:U>Rocket</color>." }, MSF_INNATE | MSF_TARGET },
    { "THROW", { MST_BALL, GF_ROCK }, /* non-reflectable! */
        { "Throw Boulder", TERM_L_UMBER,
          "$CASTER throws a large rock.",
          "$CASTER shouts, 'Haaa!!'.",
          "$CASTER throws a large rock at $TARGET.", 
          "You throw a large rock." }, MSF_INNATE | MSF_BALL0 | MSF_TARGET},
    { "HOLY_ORB", { MST_BALL, GF_HOLY_FIRE },
        { "Holy Orb", TERM_YELLOW,
          "$CASTER casts a <color:y>Holy Orb</color>.",
          "$CASTER mumbles.",
          "$CASTER casts a <color:y>Holy Orb</color> at $TARGET.",
          "You cast a <color:y>Holy Orb</color>." }, MSF_TARGET},
    { "WRATH_OF_GOD", { MST_BALL, GF_DISINTEGRATE }, /* this makes BA_DISINTEGRATE not allowed */
        { "Wrath of God", TERM_VIOLET,
          "$CASTER invokes the <color:v>Wrath of God</color>!",
          "$CASTER mumbles powerfully.",
          "$CASTER invokes the <color:v>Wrath of God</color> at $TARGET!",
          "You invoke the <color:v>Wrath of God</color>!" }, MSF_TARGET},
    { "METEOR", { MST_BALL, GF_METEOR },
        { "Meteor", TERM_RED,
          "$CASTER conjures a <color:r>Meteor</color>!",
          "$CASTER mumbles powerfully.",
          "$CASTER conjures a <color:v>Meteor</color> at $TARGET!",
          "You conjure a <color:v>Meteor</color>!" }, MSF_TARGET},
    {0}
};

/* MST_BOLT */
static _parse_t _bolt_tbl[] = {
    { "MISSILE", { MST_BOLT, GF_MISSILE },
        { "Magic Missile", TERM_WHITE,
          "$CASTER casts a Magic Missile.",
          "$CASTER mumbles.",
          "$CASTER casts a Magic Missile at $TARGET.",
          "You cast a Magic Missile." }, MSF_TARGET},
    { "SHOOT", { MST_BOLT, GF_ARROW },
        { "Shoot", TERM_L_UMBER,
          "$CASTER fires an arrow.",
          "$CASTER makes a strange noise.",
          "$CASTER fires an arrow at $TARGET.",
          "You fire an arrow." }, MSF_INNATE | MSF_TARGET },
    {0}
};

/* MST_BEAM */
static _parse_t _beam_tbl[] = {
    { "PSY_SPEAR", { MST_BEAM, GF_PSY_SPEAR },
        { "Psycho-Spear", TERM_L_BLUE,
          "$CASTER throws a <color:B>Psycho-Spear</color>.",
          "$CASTER mumbles.", 
          "$CASTER throws a <color:B>Psycho-Spear</color> at $TARGET.",
          "You throw a <color:B>Psycho-Spear</color>." }, MSF_TARGET },
    { "HELL_LANCE", { MST_BEAM, GF_HELL_FIRE },
        { "Hell Lance", TERM_RED,
          "$CASTER throws a <color:r>Hell Lance</color>.",
          "$CASTER mumbles.",
          "$CASTER throws a <color:r>Hell Lance</color> at $TARGET.",
          "You throw a <color:r>Hell Lance</color>." }, MSF_TARGET},
    { "HOLY_LANCE", { MST_BEAM, GF_HOLY_FIRE },
        { "Holy Lance", TERM_YELLOW,
          "$CASTER throws a <color:y>Holy Lance</color>.",
          "$CASTER mumbles.",
          "$CASTER throws a <color:y>Holy Lance</color> at $TARGET.",
          "You throw a <color:y>Holy Lance</color>." }, MSF_TARGET},
    { "PLASMA_LANCE", { MST_BEAM, GF_PLASMA },
        { "Plasma Lance", TERM_RED,
          "$CASTER throws a <color:R>Plasma Lance</color>.",
          "$CASTER mumbles.",
          "$CASTER throws a <color:R>Plasma Lance</color> at $TARGET.",
          "You throw a <color:R>Plasma Lance</color>." }, MSF_TARGET},
    { "BLACK_LANCE", { MST_BEAM, GF_DARK },
        { "Black Lance", TERM_RED,
          "$CASTER throws a <color:D>Black Lance</color>.",
          "$CASTER mumbles.",
          "$CASTER throws a <color:D>Black Lance</color> at $TARGET.",
          "You throw a <color:D>Black Lance</color>." }, MSF_TARGET},
    {0}
};

/* MST_LOS: Note that gf_affect_m spams messages for GF_CAUSE_?,
 * so we must omit the player casting messages. */
static _parse_t _los_tbl[] = {
    { "CAUSE_1", { MST_LOS, GF_CAUSE_1 },
        { "Cause Light Wounds", TERM_RED,
          "$CASTER points at you and curses.",
          "$CASTER curses.",
          "$CASTER points at $TARGET and curses." }, MSF_TARGET },
    { "CAUSE_2", { MST_LOS, GF_CAUSE_2 },
        { "Cause Serious Wounds", TERM_RED,
          "$CASTER points at you and curses horribly.",
          "$CASTER curses horribly.",
          "$CASTER points at $TARGET and curses horribly." }, MSF_TARGET },
    { "CAUSE_3", { MST_LOS, GF_CAUSE_3 },
        { "Cause Critical Wounds", TERM_RED,
          "$CASTER points at you, incanting terribly!",
          "$CASTER incants terribly.",
          "$CASTER points at $TARGET, incanting terribly!" }, MSF_TARGET },
    { "CAUSE_4", { MST_LOS, GF_CAUSE_4 },
        { "Cause Mortal Wounds", TERM_RED,
          "$CASTER points at you, screaming the word DIE!",
          "$CASTER screams the word DIE!", 
          "$CASTER points at $TARGET, screaming the word DIE!" }, MSF_TARGET },
    { "HAND_DOOM", { MST_LOS, GF_HAND_DOOM },
        { "Hand of Doom", TERM_RED,
          "$CASTER invokes the <color:r>Hand of Doom</color>!",
          "$CASTER invokes the <color:r>Hand of Doom</color>!",
          "$CASTER invokes the <color:r>Hand of Doom</color> at $TARGET.",
          "You invoke the <color:r>Hand of Doom</color>!" }, MSF_TARGET },
    { "GAZE", { MST_LOS, GF_ATTACK },
        { "Gaze", TERM_RED,
          "$CASTER gazes at you.",
          "",  /* spell removed if plr is blind (_ai_direct) */
          "$CASTER gazes at $TARGET.",
          "You gaze at $TARGET."}, MSF_INNATE | MSF_TARGET }, 
    {0}
};

/* MST_ESCAPE */
enum {
    ESCAPE_TELE_SELF,
    ESCAPE_TELE_OTHER,
};
static _parse_t _escape_tbl[] = {
    { "TELE_OTHER", { MST_ESCAPE, ESCAPE_TELE_OTHER },
        { "Teleport Away", TERM_WHITE }, MSF_TARGET | MSF_DIRECT },
    { "TELE_SELF", { MST_ESCAPE, ESCAPE_TELE_SELF },
        { "Teleport", TERM_WHITE }},
    {0}
};
enum {
    TACTIC_BLINK = 1000,
    TACTIC_BLINK_OTHER,
};
static _parse_t _tactic_tbl[] = {
    { "BLINK", { MST_TACTIC, TACTIC_BLINK },
        { "Blink", TERM_WHITE }},
    { "BLINK_OTHER", { MST_TACTIC, TACTIC_BLINK_OTHER },
        { "Blink Away", TERM_WHITE }, MSF_TARGET | MSF_DIRECT },
    {0}
};

/* MST_HEAL */
enum {
    HEAL_SELF,
};
static _parse_t _heal_tbl[] = {
    { "HEAL", { MST_HEAL, HEAL_SELF },
        { "Heal Self", TERM_WHITE,
          "$CASTER concentrates on $CASTER_POS wounds.",
          "$CASTER mumbles.",
          "$CASTER concentrates on $CASTER_POS wounds.",
          "You concentrate on your wounds." }},
    {0}
};

/* MST_WEIRD */
enum {
    WEIRD_SPECIAL,
    WEIRD_BIRD,
};
static _parse_t _weird_tbl[] = {
    { "SPECIAL", { MST_WEIRD, WEIRD_SPECIAL },
        { "Something Weird", TERM_RED }},
    { "BIRD_DROP", { MST_WEIRD, WEIRD_BIRD },
        { "Drop Monster", TERM_RED }, MSF_TARGET | MSF_DIRECT},
    {0}
};

/* MST_POSSESSOR: These spells won't be cast by monsters.
 * Rather, they exist for the mimic and the possessor, offering
 * access to gameplay aspects that don't affect monsters 
 * (such as detection or object lore). */
enum {
    POS_DETECT_TRAPS,
    POS_DETECT_EVIL,
    POS_DETECT_MONSTERS,
    POS_DETECT_OBJECTS,
    POS_IDENTIFY,
    POS_MAPPING,
    POS_CLAIRVOYANCE,
    POS_MULTIPLY,
    POS_BLESS,   /* XXX move to MST_BUFF */
    POS_HEROISM, /* XXX move to MST_BUFF */
    POS_WEB,
};
static _parse_t _pos_tbl[] = {
    { "DETECT_TRAPS", { MST_POSSESSOR, POS_DETECT_TRAPS },
        { "Detect Traps", TERM_L_UMBER }},
    { "DETECT_EVIL", { MST_POSSESSOR, POS_DETECT_EVIL },
        { "Detect Evil", TERM_L_UMBER }},
    { "DETECT_MONSTERS", { MST_POSSESSOR, POS_DETECT_MONSTERS },
        { "Detect Monsters", TERM_L_UMBER }},
    { "DETECT_OBJECTS", { MST_POSSESSOR, POS_DETECT_OBJECTS },
        { "Detect Objects", TERM_L_UMBER }},
    { "IDENTIFY", { MST_POSSESSOR, POS_IDENTIFY },
        { "Identify", TERM_L_BLUE }},
    { "MAPPING", { MST_POSSESSOR, POS_MAPPING },
        { "Mapping", TERM_L_BLUE }},
    { "CLAIRVOYANCE", { MST_POSSESSOR, POS_CLAIRVOYANCE },
        { "Clairvoyance", TERM_L_BLUE }},
    { "MULTIPLY", { MST_POSSESSOR, POS_MULTIPLY },
        { "Multiply", TERM_RED }, MSF_INNATE},
    { "BLESS", { MST_POSSESSOR, POS_BLESS },
        { "Bless", TERM_WHITE,
          "$CASTER prays for aid.",
          "$CASTER mumbles a petition.",
          "$CASTER prays for aid.",
          "You pray for aid."}},
    { "HEROISM", { MST_POSSESSOR, POS_HEROISM },
        { "Heroism", TERM_WHITE,
          "$CASTER prays for aid.",
          "$CASTER mumbles a petition.",
          "$CASTER prays for aid.",
          "You pray for aid."}},
    { "WEB", { MST_POSSESSOR, POS_WEB },
        { "Web", TERM_L_DARK }},
    {0}
};

static _parse_ptr _spell_parse_name_aux(cptr token, _parse_ptr tbl)
{
    int i;
    for (i = 0; ; i++)
    {
        _parse_ptr info = &tbl[i];
        if (!info->token) return NULL;
        if (strcmp(info->token, token) == 0) return info;
    }
}

static _parse_ptr _spell_parse_name(cptr token)
{
    _parse_ptr tbls[] = {_annoy_tbl, _ball_tbl, _beam_tbl, _biff_tbl,
                         _bolt_tbl, _buff_tbl, _los_tbl, _escape_tbl,
                         _heal_tbl, _tactic_tbl, _weird_tbl, _pos_tbl,  NULL};
    int i;
    for (i = 0;; i++)
    {
        _parse_ptr tbl = tbls[i];
        _parse_ptr p;
        if (!tbl) return NULL;
        p = _spell_parse_name_aux(token, tbl);
        if (p) return p;
    }
}

static bool _spell_is_(mon_spell_ptr spell, int type, int effect)
{
    return spell->id.type == type && spell->id.effect == effect;
}

typedef struct {
    int id;
    cptr name;
    byte color;
    int  prob;
} _mst_info_t, *_mst_info_ptr;
static _mst_info_t _mst_tbl[MST_COUNT] = {
    { MST_BREATH, "Breathe", TERM_RED, 15 },
    { MST_BALL, "Ball", TERM_RED, 15 },
    { MST_BOLT, "Bolt", TERM_RED, 15 },
    { MST_BEAM, "Beam", TERM_RED, 15 },
    { MST_LOS, "Curse/Gaze", TERM_RED, 15 },
    { MST_BUFF, "Buff", TERM_L_BLUE, 5 },
    { MST_BIFF, "Biff", TERM_RED, 10 },
    { MST_ESCAPE, "Escape", TERM_L_BLUE, 5 },
    { MST_ANNOY, "Annoy", TERM_ORANGE, 5 },
    { MST_SUMMON, "Summon", TERM_ORANGE, 8 },
    { MST_HEAL, "Heal", TERM_L_BLUE, 10 },
    { MST_TACTIC, "Tactic", TERM_L_BLUE, 10 },
    { MST_WEIRD, "Weird", TERM_L_UMBER, 100 },
    { MST_POSSESSOR, "Possessor", TERM_L_BLUE, 0 }, /* <== 0 prevents monster casting */
};
static _mst_info_ptr _mst_lookup(int which)
{
    _mst_info_ptr p;
    assert (0 <= which && which < MST_COUNT);
    p = & _mst_tbl[which];
    assert(p->id == which);
    return p;
}

/*************************************************************************
 * ID
 ************************************************************************/
mon_spell_id_t mon_spell_id(int type, int effect)
{
    mon_spell_id_t id;
    id.type = type;
    id.effect = effect;
    return id;
}

static mon_spell_id_t _id(int type, int effect)
{
    return mon_spell_id(type, effect);
}

int mon_spell_hash(mon_spell_id_t id)
{
    int hash = id.type << 16;
    hash += id.effect;
    return hash;
}

int mon_spell_pack(mon_spell_id_t id)
{
    int pack = id.type << 16;
    pack += id.effect;
    return pack;
}

mon_spell_id_t mon_spell_unpack(int packed)
{
    mon_spell_id_t id;
    id.type = (packed & 0xFFFF0000) >> 16;
    id.effect = packed & 0x0000FFFF;
    return id;
}

/*************************************************************************
 * Parm
 ************************************************************************/
static dice_t _dice(int dd, int ds, int base)
{
    dice_t dice = {0};
    dice.dd = dd;
    dice.ds = ds;
    dice.base = base;
    return dice;
}

static hp_pct_t _hp_pct(int pct, int max)
{
    hp_pct_t hp;
    hp.pct = pct;
    hp.max = max;
    return hp;
}

mon_spell_parm_t mon_spell_parm_dice(int dd, int ds, int base)
{
    mon_spell_parm_t parm;
    parm.tag = MSP_DICE;
    parm.v.dice = _dice(dd, ds, base);
    return parm;
}

mon_spell_parm_t mon_spell_parm_hp_pct(int pct, int max)
{
    mon_spell_parm_t parm;
    parm.tag = MSP_HP_PCT;
    parm.v.hp_pct = _hp_pct(pct, max);
    return parm;
}

/* This is for clang that complains about any default {0} or { {0} } initialization.
 * I think it wants {{{0}}}, but I got tired of trying to appease it. */
static mon_spell_parm_t _empty(void)
{
    mon_spell_parm_t p;
    memset(&p, 0, sizeof(mon_spell_parm_t));
    return p;
}
static mon_spell_parm_t _breath_parm(int which)
{
    mon_spell_parm_t parm = _empty();
    parm.tag = MSP_HP_PCT;
    switch (which)
    {
    case GF_ACID:
    case GF_ELEC:
    case GF_FIRE:
    case GF_COLD:
        parm.v.hp_pct = _hp_pct(20, 900);
        break;
    case GF_POIS:
    case GF_NUKE:
        parm.v.hp_pct = _hp_pct(17, 600);
        break;
    case GF_NETHER:
        parm.v.hp_pct = _hp_pct(14, 550);
        break;
    case GF_LIGHT:
    case GF_DARK:
    case GF_CONFUSION:
        parm.v.hp_pct = _hp_pct(17, 400);
        break;
    case GF_SOUND:
        parm.v.hp_pct = _hp_pct(17, 450);
        break;
    case GF_CHAOS:
        parm.v.hp_pct = _hp_pct(17, 600);
        break;
    case GF_DISENCHANT:
    case GF_SHARDS:
        parm.v.hp_pct = _hp_pct(17, 500);
        break;
    case GF_NEXUS:
        parm.v.hp_pct = _hp_pct(33, 250);
        break;
    case GF_STORM:
        parm.v.hp_pct = _hp_pct(13, 250);
        break;
    case GF_INERTIA:
    case GF_PLASMA:
    case GF_HELL_FIRE:
    case GF_HOLY_FIRE:
        parm.v.hp_pct = _hp_pct(17, 250);
        break;
    case GF_GRAVITY:
    case GF_FORCE:
        parm.v.hp_pct = _hp_pct(33, 200);
        break;
    case GF_MANA:
        parm.v.hp_pct = _hp_pct(33, 250);
        break;
    case GF_DISINTEGRATE:
        parm.v.hp_pct = _hp_pct(17, 150);
        break;
    case GF_TIME:
        parm.v.hp_pct = _hp_pct(33, 150);
        break;
    default:
        /*assert(FALSE);*/
        msg_format("Unsupported breath %s (%d)", gf_name(which), which);
    }
    return parm;
}

static mon_spell_parm_t _ball_parm(int which, int rlev)
{
    mon_spell_parm_t parm = _empty();
    parm.tag = MSP_DICE;
    switch (which)
    {
    case GF_ACID:
        parm.v.dice = _dice(1, 3*rlev, 15);
        break;
    case GF_ELEC:
        parm.v.dice = _dice(1, 3*rlev/2, 8);
        break;
    case GF_FIRE:
        parm.v.dice = _dice(1, 7*rlev/2, 10);
        break;
    case GF_COLD:
        parm.v.dice = _dice(1, 3*rlev/2, 10);
        break;
    case GF_POIS:
        parm.v.dice = _dice(12, 2, 0);
        break;
    case GF_NUKE:
        parm.v.dice = _dice(10, 6, rlev);
        break;
    case GF_NETHER:
        parm.v.dice = _dice(10, 10, 50 + rlev);
        break;
    case GF_DARK:
    case GF_LIGHT:
    case GF_MANA:
        parm.v.dice = _dice(10, 10, 50 + 4*rlev);
        break;
    case GF_CHAOS:
    case GF_ICE:
        parm.v.dice = _dice(10, 10, rlev);
        break;
    case GF_DISENCHANT:
    case GF_SOUND:
        parm.v.dice = _dice(10, 10, 3*rlev);
        break;
    case GF_WATER:
        parm.v.dice = _dice(1, rlev, 50);
        break;
    case GF_DRAIN_MANA:
        parm.v.dice = _dice(0, 0, 1 + rlev/2);
        break;
    case GF_MIND_BLAST:
        parm.v.dice = _dice(7, 7, 0);
        break;
    case GF_BRAIN_SMASH:
        parm.v.dice = _dice(12, 12, 0);
        break;
    case GF_TELEKINESIS:
        parm.v.dice = _dice(8, 8, 0);
        break;
    case GF_ROCK:
        parm.v.dice = _dice(0, 0, 3*rlev);
        break;
    case GF_ROCKET:
        parm.v.dice = _dice(0, 0, 6*rlev);
        break;
    case GF_METEOR:
    case GF_STORM:
        parm.v.dice = _dice(0, 0, 3*rlev); /* unresistable */
        break;
    case GF_HOLY_FIRE:
        parm.v.dice = _dice(3, 6, rlev);
        break;
    case GF_DISINTEGRATE: /* Wrath of God: 10+d10 balls spread about plr */
        parm.v.dice = _dice(0, 0, 15 + rlev/2); /* cf _wrath_of_god_aux discussion */
        break;
    default:
        parm.v.dice = _dice(5, 5, rlev);
    }
    return parm;
}

static mon_spell_parm_t _bolt_parm(int which, int rlev)
{
    mon_spell_parm_t parm = _empty();
    parm.tag = MSP_DICE;
    switch (which)
    {
    case GF_ACID:
        parm.v.dice = _dice(7, 8, rlev/3);
        break;
    case GF_ELEC:
        parm.v.dice = _dice(4, 8, rlev/3);
        break;
    case GF_FIRE:
        parm.v.dice = _dice(9, 8, rlev/3);
        break;
    case GF_COLD:
        parm.v.dice = _dice(6, 8, rlev/3);
        break;
    case GF_ICE:
        parm.v.dice = _dice(6, 8, rlev);
        break;
    case GF_NETHER:
        parm.v.dice = _dice(5, 5, 30 + rlev);
        break;
    case GF_WATER:
    case GF_SOUND:
        parm.v.dice = _dice(10, 10, rlev);
        break;
    case GF_PLASMA:
        parm.v.dice = _dice(8, 7, 10 + rlev);
        break;
    case GF_MANA:
        parm.v.dice = _dice(1, 7*rlev/2, 50);
        break;
    case GF_MISSILE:
        parm.v.dice = _dice(2, 6, rlev/3);
    case GF_ARROW:
        /* SHOOT always specifies dice overrides */
        break;
    default:
        assert(FALSE);
    }
    return parm;
}

static mon_spell_parm_t _beam_parm(int which, int rlev)
{
    mon_spell_parm_t parm = _empty();
    parm.tag = MSP_DICE;
    switch (which)
    {
    case GF_PSY_SPEAR:
        parm.v.dice = _dice(1, rlev*3/2, 100);
        break;
    case GF_HELL_FIRE:
    case GF_HOLY_FIRE:
    case GF_DARK:
        parm.v.dice = _dice(0, 0, 2*rlev);
        break;
    case GF_PLASMA:
    case GF_SOUND:
        parm.v.dice = _dice(0, 0, 3*rlev);
        break;
    default:
        assert(FALSE);
    }
    return parm;
}

static mon_spell_parm_t _los_parm(int which)
{
    mon_spell_parm_t parm = _empty();
    parm.tag = MSP_DICE;
    switch (which)
    {
    case GF_CAUSE_1:
        parm.v.dice = _dice(3, 8, 0);
        break;
    case GF_CAUSE_2:
        parm.v.dice = _dice(8, 8, 0);
        break;
    case GF_CAUSE_3:
        parm.v.dice = _dice(10, 15, 0);
        break;
    case GF_CAUSE_4:
        parm.v.dice = _dice(15, 15, 0);
        break;
    case GF_HAND_DOOM:
        parm.v.dice = _dice(1, 20, 40); /* This is percentage of chp! */
        break;
    case GF_ATTACK:
        parm.v.dice = _dice(0, 0, 0);   /* must project 0 for possessor */
        break;
    default:
        assert(FALSE);
    }
    return parm;
}

static mon_spell_parm_t _heal_parm(int which, int rlev)
{
    mon_spell_parm_t parm = _empty();
    parm.tag = MSP_DICE;
    switch (which)
    {
    case HEAL_SELF:
        parm.v.dice = _dice(0, 0, rlev*5);
        break;
    default:
        assert(FALSE);
    }
    return parm;
}

static mon_spell_parm_t _annoy_parm(int which)
{
    mon_spell_parm_t parm = _empty();
    switch (which)
    {
    case ANNOY_EGO_WHIP:
        parm.tag = MSP_DICE;
        parm.v.dice = _dice(1, 2, 2);
        break;
    }
    return parm;
}
static mon_spell_parm_t _summon_parm(int which)
{
    mon_spell_parm_t parm = _empty();
    parm.tag = MSP_DICE;
    switch (which)
    {
    case SUMMON_CYBER:
    case SUMMON_HI_UNDEAD:
    case SUMMON_HI_DRAGON:
    case SUMMON_HI_DEMON:
        parm.v.dice = _dice(1, 3, 0);
        break;
    case SUMMON_UNIQUE:
    case SUMMON_GUARDIAN:
    case SUMMON_AMBERITE:
    case SUMMON_OLYMPIAN: /* Zeus, Hermes, Aprhrodite */
        parm.v.dice = _dice(1, 2, 0);
        break;
    case SUMMON_SPIDER:
    case SUMMON_HOUND:
    case SUMMON_KIN:
        parm.v.dice = _dice(1, 2, 1);
        break;
    default:
        parm.v.dice = _dice(1, 3, 1);
    }
    return parm;
}

static mon_spell_parm_t _tactic_parm(int which, int rlev)
{
    mon_spell_parm_t parm = _empty();
    if (which >= TACTIC_BLINK) return parm;
    parm.tag = MSP_DICE;
    parm.v.dice = _dice(0, 0, rlev);
    return parm;
}

mon_spell_parm_t mon_spell_parm_default(mon_spell_id_t id, int rlev)
{
    mon_spell_parm_t empty = _empty();

    switch (id.type)
    {
    case MST_BREATH:
        return _breath_parm(id.effect);
    case MST_BALL:
        return _ball_parm(id.effect, rlev);
    case MST_BOLT:
        return _bolt_parm(id.effect, rlev);
    case MST_BEAM:
        return _beam_parm(id.effect, rlev);
    case MST_LOS:
        return _los_parm(id.effect);
    case MST_HEAL:
        return _heal_parm(id.effect, rlev);
    case MST_SUMMON:
        return _summon_parm(id.effect);
    case MST_TACTIC:
        return _tactic_parm(id.effect, rlev);
    case MST_ANNOY:
        return _annoy_parm(id.effect);
    }

    return empty;
}

errr mon_spell_parm_parse(mon_spell_parm_ptr parm, char *token)
{
    char arg[100], sentinel = '~', check;
    int  dd, ds, base, pct, wgt;

    sprintf(arg, "%s%c", token, sentinel);

    /* Note: The parser will default parm with mon_spell_parm_default(id),
     * but we allow the user to override. This means that BR_FOO should
     * already be set with the default max, which is good since we don't
     * currently support syntax for overriding it. */
    if (2 == sscanf(arg, "%d%%%c", &pct, &check) && check == sentinel)
    {
        if (parm->tag != MSP_HP_PCT)
        {
            msg_print("Error: A hitpoint percentage is only valid on BR_* spells.");
            return PARSE_ERROR_GENERIC;
        }
        parm->v.hp_pct.pct = MAX(0, MIN(100, pct));
    }
    else if (2 == sscanf(arg, "x%d%c", &wgt, &check) && check == sentinel)
    {
        parm->wgt = MAX(0, MIN(100, wgt));
    }
    else if (4 == sscanf(arg, "%dd%d+%d%c", &dd, &ds, &base, &check) && check == sentinel)
    {
        if (parm->tag != MSP_DICE)
        {
            msg_print("Error: Dice parameters are not supported on this spell type.");
            return PARSE_ERROR_GENERIC;
        }
        parm->v.dice.dd = MAX(0, dd);
        parm->v.dice.ds = MAX(0, ds);
        parm->v.dice.base = base;
    }
    else if (3 == sscanf(arg, "%dd%d%c", &dd, &ds, &check) && check == sentinel)
    {
        if (parm->tag != MSP_DICE)
        {
            msg_print("Error: Dice parameters are not supported on this spell type.");
            return PARSE_ERROR_GENERIC;
        }
        parm->v.dice.dd = MAX(0, dd);
        parm->v.dice.ds = MAX(0, ds);
        parm->v.dice.base = 0;
    }
    else if (2 == sscanf(arg, "%d%c", &base, &check) && check == sentinel)
    {
        if (parm->tag != MSP_DICE)
        {
            msg_print("Error: Dice parameters are not supported on this spell type.");
            return PARSE_ERROR_GENERIC;
        }
        parm->v.dice.dd = 0;
        parm->v.dice.ds = 0;
        parm->v.dice.base = base;
    }
    else
    {
        msg_format("Error: Unknown argument %s.", token);
        return PARSE_ERROR_GENERIC;
    }
    return 0;
}

static int _avg_hp(mon_race_ptr r)
{
    return dice_avg_roll(r->hp);
}
void mon_spell_parm_print(mon_spell_parm_ptr parm, str_ptr s, mon_race_ptr race)
{
    if (parm->tag == MSP_DICE)
    {
        if (parm->v.dice.dd && parm->v.dice.ds)
        {
            str_printf(s, "%dd%d", parm->v.dice.dd, parm->v.dice.ds);
            if (parm->v.dice.base)
                str_append_c(s, '+');
        }
        if (parm->v.dice.base)
            str_printf(s, "%d", parm->v.dice.base);
    }
    else if (parm->tag == MSP_HP_PCT)
    {
        if (race)
        {
            int hp = _avg_hp(race);
            int dam = hp * parm->v.hp_pct.pct / 100;
            if (dam > parm->v.hp_pct.max)
                dam = parm->v.hp_pct.max;
            str_printf(s, "%d", dam);
        }
        else
            str_printf(s, "%d%% up to %d", parm->v.hp_pct.pct, parm->v.hp_pct.max);
    }
}

/*************************************************************************
 * Spell
 ************************************************************************/
/* BA_MANA or BR_FIRE(30%) or BO_FIRE(3d5+10) or HEAL(120) ... */
errr mon_spell_parse(mon_spell_ptr spell, int rlev, char *token)
{
    int           i;
    char         *name;
    char         *args[10];
    int           arg_ct = parse_args(token, &name, args, 10);
    _parse_ptr    p;

    if (arg_ct < 0)
    {
        msg_format("Error: Malformed argument %s. Missing )?", name);
        return PARSE_ERROR_GENERIC;
    }

    p = _spell_parse_name(name);
    if (p)
    {
        spell->id = p->id;
        spell->display = &p->display;
        spell->flags = p->flags;
    }
    else if (prefix(name, "S_"))
    {
        parse_tbl_ptr p = summon_type_parse(name + 2);
        if (!p)
        {
            msg_format("Error: Unknown summon type %s.", name + 2);
            return PARSE_ERROR_GENERIC;
        }
        spell->id.type = MST_SUMMON;
        spell->id.effect = p->id;
    }
    else
    {
        gf_info_ptr  gf;
        cptr         suffix;
        if (prefix(name, "BR_"))
        {
            spell->id.type = MST_BREATH;
            spell->flags |= MSF_INNATE | MSF_TARGET;
            suffix = name + 3;
        }
        else if (prefix(name, "BA_"))
        {
            spell->id.type = MST_BALL;
            spell->flags |= MSF_TARGET;
            suffix = name + 3;
        }
        else if (prefix(name, "BO_"))
        {
            spell->id.type = MST_BOLT;
            spell->flags |= MSF_TARGET;
            suffix = name + 3;
        }
        else if (prefix(name, "JMP_"))
        {
            spell->id.type = MST_TACTIC;
            suffix = name + 4;
        }
        else
        {
            msg_format("Error: Unkown spell %s.", name);
            return PARSE_ERROR_GENERIC;
        }
        gf = gf_parse_name(suffix);
        if (!gf)
        {
            msg_format("Error: Unkown type %s.", name + 3);
            return PARSE_ERROR_GENERIC;
        }
        spell->id.effect = gf->id;
    }

    spell->parm = mon_spell_parm_default(spell->id, rlev);
    for (i = 0; i < arg_ct; i++) /* XXX should only be 1 at the moment */
    {
        errr rc = mon_spell_parm_parse(&spell->parm, args[i]);
        if (rc) return rc;
    }
    return 0;
}

void mon_spell_print(mon_spell_ptr spell, str_ptr s)
{
    if (spell->display)
    {
        str_printf(s, "<color:%c>%s</color>",
            attr_to_attr_char(spell->display->color), spell->display->name);
    }
    else if (spell->id.type == MST_SUMMON)
    {
        parse_tbl_ptr p = summon_type_lookup(spell->id.effect);
        if (!p)
            str_printf(s, "Summon %d", spell->id.effect);
        else
            str_printf(s, "<color:%c>Summon %s</color>", attr_to_attr_char(p->color), p->name);
    }
    else if (spell->id.type == MST_BREATH)
    {
        gf_info_ptr gf = gf_lookup(spell->id.effect);
        if (gf)
        {
            str_printf(s, "<color:%c>Breathe %s</color>",
                attr_to_attr_char(gf->color), gf->name);
        }
        else
            str_printf(s, "Breathe %d", spell->id.effect);
    }
    else if (spell->id.type == MST_TACTIC) /* BLINK and BLINK_OTHER have spell->display set */
    {
        gf_info_ptr gf = gf_lookup(spell->id.effect);
        if (gf)
        {
            str_printf(s, "<color:%c>%s Jump</color>",
                attr_to_attr_char(gf->color), gf->name);
        }
        else
            str_printf(s, "%d Jump", spell->id.effect);
    }
    else
    {
        gf_info_ptr   gf = gf_lookup(spell->id.effect);
        _mst_info_ptr mst = _mst_lookup(spell->id.type);
        assert(mst);
        if (gf)
        {
            str_printf(s, "<color:%c>%s %s</color>",
                attr_to_attr_char(gf->color), gf->name, mst->name);
        }
        else
            str_printf(s, "%s %d", mst->name, spell->id.effect);
    }
}
void mon_spell_display(mon_spell_ptr spell, str_ptr s)
{
    if (spell->id.type == MST_BREATH)
    {
        gf_info_ptr gf = gf_lookup(spell->id.effect);
        if (gf)
        {
            str_printf(s, "<color:%c>%s</color>",
                attr_to_attr_char(gf->color), gf->name);
        }
        else
            str_printf(s, "Unknown %d", spell->id.effect);
    }
    else if (spell->id.type == MST_SUMMON)
    {
        parse_tbl_ptr p = summon_type_lookup(spell->id.effect);
        if (p)
            str_printf(s, "<color:%c>%s</color>", attr_to_attr_char(p->color), p->name);
        else
            str_printf(s, "Unknown %d", spell->id.effect);
    }
    else
        mon_spell_print(spell, s);
}

void mon_spell_doc(mon_spell_ptr spell, doc_ptr doc)
{
    str_ptr s = str_alloc();
    mon_spell_print(spell, s);
    doc_insert(doc, str_buffer(s));
    str_free(s);
}

/*************************************************************************
 * Spell Group
 ************************************************************************/
mon_spell_group_ptr mon_spell_group_alloc(void)
{
    mon_spell_group_ptr group = malloc(sizeof(mon_spell_group_t));
    memset(group, 0, sizeof(mon_spell_group_t));
    return group;
}

void mon_spell_group_free(mon_spell_group_ptr group)
{
    if (group)
    {
        if (group->spells)
            free(group->spells);
        free(group);
    }
}

static void _group_grow(mon_spell_group_ptr group)
{
    if (!group->allocated)
    {
        group->allocated = 1;
        group->spells = malloc(sizeof(mon_spell_t)*group->allocated);
    }
    else
    {
        group->allocated *= 2;
        group->spells = realloc(group->spells, sizeof(mon_spell_t)*group->allocated);
        assert(group->spells);
    }
}

void mon_spell_group_add(mon_spell_group_ptr group, mon_spell_ptr spell)
{
    int i;
    assert(spell);
    if (group->count == group->allocated)
        _group_grow(group);
    assert(group->count < group->allocated);
/*  group->spells[group->count++] = *spell; */
    for (i = 0; i < group->count; i++)
    {
        if (group->spells[i].id.effect == spell->id.effect) return;
        if (group->spells[i].id.effect > spell->id.effect)
            break;
    }

    if (i < group->count)
        memmove(group->spells + i + 1, group->spells + i, (group->count - i)*sizeof(mon_spell_t));
    group->spells[i] = *spell;
    group->count++;
}

mon_spell_ptr mon_spell_group_find(mon_spell_group_ptr group, mon_spell_id_t id)
{
    int i;
    for (i = 0; i < group->count; i++)
    {
        mon_spell_ptr spell = &group->spells[i];
        if (spell->id.type == id.type && spell->id.effect == id.effect)
            return spell;
    }
    return NULL;
}

/*************************************************************************
 * Spells
 ************************************************************************/
mon_spells_ptr mon_spells_alloc(void)
{
    mon_spells_ptr spells = malloc(sizeof(mon_spells_t));
    memset(spells, 0, sizeof(mon_spells_t));
    return spells;
}

void mon_spells_free(mon_spells_ptr spells)
{
    if (spells)
    {
        int i;
        for (i = 0; i < MST_COUNT; i++)
        {
            mon_spell_group_ptr group = spells->groups[i];
            if (group)
                mon_spell_group_free(group);
            spells->groups[i] = NULL;
        }
        free(spells);
    }
}

void mon_spells_add(mon_spells_ptr spells, mon_spell_ptr spell)
{
    mon_spell_group_ptr group = spells->groups[spell->id.type];
    if (!group)
    {
        group = mon_spell_group_alloc();
        group->type = spell->id.type;
        spells->groups[spell->id.type] = group;
    }
    mon_spell_group_add(group, spell);
}

errr mon_spells_parse(mon_spells_ptr spells, int rlev, char *token)
{
    mon_spell_t spell = {{0}};
    errr        rc = mon_spell_parse(&spell, rlev, token);

    if (rc == 0)
    {
        if (mon_spells_find(spells, spell.id)) /* duplicate flags were not a problem, but duplicate spells are */
            return 1;
        mon_spells_add(spells, &spell);
    }
    return rc;
}

vec_ptr mon_spells_all(mon_spells_ptr spells)
{
    vec_ptr v = vec_alloc(NULL);
    int     i, j;
    for (i = 0; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = spells->groups[i];
        if (!group) continue;
        for (j = 0; j < group->count; j++)
        {
            mon_spell_ptr spell = &group->spells[j];
            vec_add(v, spell);
        }
    }
    return v;
}

mon_spell_ptr mon_spells_find(mon_spells_ptr spells, mon_spell_id_t id)
{
    int i;
    mon_spell_group_ptr group = spells->groups[id.type];
    if (!group) return NULL;
    for (i = 0; i < group->count; i++)
    {
        mon_spell_ptr spell = &group->spells[i];
        if (spell->id.effect != id.effect) continue;
        return spell;
    }
    return NULL;
}

mon_spell_ptr mon_spells_random(mon_spells_ptr spells)
{
    vec_ptr v = mon_spells_all(spells);
    mon_spell_ptr spell = NULL;
    if (vec_length(v))
    {
        int i = randint0(vec_length(v));
        spell = vec_get(v, i);
    }
    vec_free(v);
    return spell;
}

/*************************************************************************
 * Cast
 ************************************************************************/
static mon_spell_cast_t _current = {0};

mon_spell_cast_ptr mon_spell_current(void)
{
    if (_current.spell) return &_current;
    return NULL;
}

static void _spell_cast_aux(void);
static bool _default_ai(mon_spell_cast_ptr cast);
static bool _default_ai_mon(mon_spell_cast_ptr cast);

static void _mon_desc(mon_ptr mon, char *buf, char color)
{
    char tmp[MAX_NLEN];
    monster_desc(tmp, mon, 0);
    tmp[0] = toupper(tmp[0]);
    sprintf(buf, "<color:%c>%s</color>", color, tmp);
}
static mon_ptr _random_mon;
static int  _random_weight(point_t pos, dun_grid_ptr grid)
{
    if (!cell_project(grid)) return 0;
    if (!point_project(_random_mon->pos, pos)) return 0;
    return 1;
}
static point_t _random_target(mon_ptr mon)
{
    dun_ptr dun = mon->dun;
    rect_t  r = rect_create_centered(mon->pos, 10, 10);
    /* XXX testing an alternative to scatter. There are situations
     * where only one valid scatter point will work out of 400 or so, 
     * and it seems wrong to spin until that point is randomly chosen. */
    _random_mon = mon;
    return dun_random_grid_in_rect(dun, r, _random_weight);
}
static void _spell_cast_init(mon_spell_cast_ptr cast, mon_ptr mon)
{
    cast->mon = mon;
    cast->race = mon->race;
    cast->spell = NULL;
    cast->src = mon->pos;
    if (mon_tim_find(mon, T_CONFUSED))
        cast->dest = _random_target(cast->mon);
    else if (mon_tim_find(mon, T_BLIND))
        cast->dest = mon->last_enemy_pos;
    else if (plr->special_defense & DEFENSE_INVISIBLE)
        cast->dest = mon_fuzzy_pos(mon, plr->pos);
    else
        cast->dest = plr->pos;
    _mon_desc(mon, cast->name, 'G'); 
    cast->flags = MSC_SRC_MONSTER | MSC_DEST_PLAYER;
}
static void _spell_cast_init_mon(mon_spell_cast_ptr cast, mon_ptr mon)
{
    cast->mon = mon;
    cast->race = mon->race;
    cast->spell = NULL;
    cast->src = mon->pos;
    _mon_desc(mon, cast->name, 'G'); 
    cast->flags = MSC_SRC_MONSTER | MSC_DEST_MONSTER;
}
static void _spell_cast_init_plr(mon_spell_cast_ptr cast, mon_race_ptr race)
{
    cast->mon = NULL;
    cast->race = race;
    cast->spell = NULL;
    cast->src = plr->pos;
    cast->dest = plr->pos;
    cast->flags = MSC_SRC_PLAYER | MSC_DEST_MONSTER;
}

static bool _can_cast(mon_ptr mon)
{
    if (!mon_is_hostile(mon)) return FALSE;
    if (!is_aware(mon)) return FALSE;
    if (plr->dun_id != cave->id) return FALSE;
    if (!plr->playing || plr->is_dead) return FALSE;

    if (!mon->turns && mon_has_worthy_attack_spell(mon))
        return FALSE;

    if (mon_tim_find(mon, T_BLIND))
    {
        return dun_pos_interior(mon->dun, mon->last_enemy_pos);
    }
    return TRUE;
}

static bool _not_innate_p(mon_spell_ptr spell)
{
    return !(spell->flags & MSF_INNATE);
}

bool mon_spell_cast(mon_ptr mon, mon_spell_ai ai)
{
    mon_spell_cast_t cast = {0};

    if (!_can_cast(mon)) return FALSE;
    if (mon->cdis > MAX_RANGE && !dun_pos_interior(cave, mon->target_pos)) return FALSE;

    if (!ai)
    {
        if (1 && plr->wizard && mon == who_mon(plr->target)) ai = mon_spell_ai_wizard;
        else ai = _default_ai;
    }

    _spell_cast_init(&cast, mon);
    if (ai(&cast))
    {
        /* XXX Historically, the spell ai has removed non-innate spells from consideration
         * prior to choosing a spell inside the Anti-magic caves. The result is that certain
         * monsters get very very hard (e.g. Ghatanathoa). Instead, we'll now let the ai
         * pick spells using the normal frequencies, and then reject magical spells. */
        if ( plr_in_dungeon()
          && (cave->flags & DF_NO_MAGIC)
          && _not_innate_p(cast.spell) )
        {
            /* attack instead */
            return FALSE;
        }
        if (mon_tim_find(mon, T_BERSERK) && _not_innate_p(cast.spell))
            return FALSE;

        _current = cast;
        _spell_cast_aux();
        memset(&_current, 0, sizeof(mon_spell_cast_t));
        return TRUE;
    }
    return FALSE;
}

bool mon_spell_cast_mon(mon_ptr mon, mon_spell_ai ai)
{
    mon_spell_cast_t cast = {0};

    if (mon_tim_find(mon, T_CONFUSED))
    {
        /* XXX Fling a random innate spell in a random direction instead! */
        return FALSE;
    }
    if (mon_tim_find(mon, T_BLIND)) return FALSE; /* XXX */

    if (!ai) ai = _default_ai_mon;

    _spell_cast_init_mon(&cast, mon);
    if (ai(&cast))
    {
        _current = cast;
        if (_current.flags & MSC_UNVIEW)
            mon_fight = TRUE;
        _spell_cast_aux();
        memset(&_current, 0, sizeof(mon_spell_cast_t));
        return TRUE;
    }
    return FALSE;
}

static bool _projectable(point_t src, point_t dest);
static bool _allow_dec_mana(mon_spell_ptr spell, mon_race_ptr race);
int mon_spell_fail_plr(mon_spell_ptr spell, mon_race_ptr race)
{
    int fail = 25 - (race->alloc.lvl + 3)/4;
    int stun = plr_tim_amount(T_STUN);
    int spell_stat = race->body.spell_stat;

    /* XXX Innate breath attacks are very strong for the Blue-Mage, so let's
     * give them fail rates to compensate. Possessors and Mimics are just like
     * monsters, though, and never fail innate attacks. */
    if ((spell->flags & MSF_INNATE) && plr->pclass != CLASS_BLUE_MAGE) return 0;

    /* Fail rates go down as player level exceeds base level.
     * For example, a Novice Mage has a ridiculously un-useful
     * Magic Missile (23% fail). But at CL15, this becomes just
     * 13% (which still sucks, but high Int can help here) */
    if (plr->lev > race->alloc.lvl)
        fail -= (plr->lev - race->alloc.lvl);

    if (_allow_dec_mana(spell, race))
        fail -= plr->dec_mana + plr->easy_spell;

    /* XXX Possessors and mimics should not get a free ride wrt
     * spell casting stats, but the mechanics should not be too 
     * harsh either since early game stats are bound to be poor.
     * Note that poor stats also means a poor mana pool.
     * XXX Hack: Breaths always use Con for fail rates. This is
     * odd since they are MSF_INNATE, but this is for the Blue-Mage. */
    if (spell->id.type == MST_BREATH) spell_stat = A_CON;
    if (spell_stat != A_NONE)
    {
        int     stat = plr->stat_ind[spell_stat] + 3;
        point_t tbl[5] = { {3, 25}, {10, 10}, {15, 0}, {20, 0}, {40, -10} };
        int     adj = interpolate(stat, tbl, 5);
        int     min = adj_mag_fail[stat];

        fail += adj;
        if (fail < min) fail = min;
    }

    /* And finally, trying to learn a new form puts the player at
     * a slight disadvantage. No fair taking down Loki with his own
     * mana storms!! */
    if (plr->prace == RACE_MON_MIMIC && !mimic_is_memorized(plr->current_r_idx))
    {
        fail += 15;
    }
    if (stun > 0)
        fail += 50 * MIN(100, stun)/100;
    return fail;
}
static bool _spell_fail(void)
{
    int fail;

    if (plr_in_dungeon() && (cave->flags & DF_NO_MAGIC))
        return TRUE;

    if (_current.flags & MSC_SRC_PLAYER)
        fail = mon_spell_fail_plr(_current.spell, _current.race);
    else
    {
        int stun;

        if (_current.spell->flags & MSF_INNATE)
            return FALSE;
        if (mon_is_stupid(_current.mon)) /* XXX ? */
            return FALSE;

        stun = mon_tim_amount(_current.mon, T_STUN);
        fail = 25 - (_current.race->alloc.lvl + 3)/4;
        if (stun > 0)
            fail += 50 * MIN(100, stun)/100;
    }
    if (fail && randint0(100) < fail)
    {
        if (_current.flags & MSC_SRC_PLAYER)
        {
            if (0 || plr->wizard)
                msg_format("You try to cast a spell, but fail (%d%%).", fail);
            else
                msg_print("You try to cast a spell, but fail.");
        }
        else if (mon_show_msg(_current.mon))
        {
            if (plr_view(_current.src))
                mon_lore_spell_failure(_current.mon);
            msg_format("%s tries to cast a spell, but fails.", _current.name);
        }
        return TRUE;
    }
    return FALSE;
}
static bool _spell_blocked(void)
{
    if (_current.spell->flags & MSF_INNATE)
        return FALSE;
    if (plr_block_magic(_current.mon))
    {
        msg_format("Your anti-magic barrier blocks the spell which %^s casts.", _current.name);
        return (TRUE);
    }

    if (psion_check_disruption_aux(_current.mon))
    {
        msg_format("Your psionic disruption blocks the spell which %^s casts.", _current.name);
        return TRUE;
    }
    return FALSE;
}
static who_t _who(void)
{
    if (_current.flags & MSC_SRC_PLAYER) return who_create_plr();
    return who_create_mon(_current.mon);
}
static int _scale(int dam)
{
    if (_current.mon)
        return MAX(1, dam * _current.mon->mpower / 1000);
    else if (_current.flags & MSC_SRC_PLAYER)
        return MAX(1, dam * plr->clp / 1000);
    return dam;
}
static int _breath_rad(mon_race_ptr r)
{
    int rad = r->alloc.lvl >= 50 ? 3 : 2;
    if (mon_race_is_char(r, 'D')) rad = 3;
    return rad;
}
byte _breath_con_adj[] =
{
    30   /* 3 */,
    40   /* 4 */,
    47   /* 5 */,
    51   /* 6 */,
    54   /* 7 */,
    57   /* 8 */,
    59   /* 9 */,
    61   /* 10 */,
    63   /* 11 */,
    64   /* 12 */,
    65   /* 13 */,
    66   /* 14 */,
    67   /* 15 */,
    68   /* 16 */,
    69   /* 17 */,
    70   /* 18/00-18/09 */,
    71   /* 18/10-18/19 */,
    72   /* 18/20-18/29 */,
    73   /* 18/30-18/39 */,
    74   /* 18/40-18/49 */,
    75   /* 18/50-18/59 */,
    76   /* 18/60-18/69 */,
    77   /* 18/70-18/79 */,
    78   /* 18/80-18/89 */,
    79   /* 18/90-18/99 */,
    80   /* 18/100-18/109 */,
    82   /* 18/110-18/119 */,
    84   /* 18/120-18/129 */,
    86   /* 18/130-18/139 */,
    88   /* 18/140-18/149 */,
    90   /* 18/150-18/159 */,
    92   /* 18/160-18/169 */,
    94   /* 18/170-18/179 */,
    96   /* 18/180-18/189 */,
    98   /* 18/190-18/199 */,
   100   /* 18/200-18/209 */,
   100   /* 18/210-18/219 */,
   100   /* 18/220+ */
};
static int _breath_max_plr(mon_spell_ptr spell)
{
    int max = spell->parm.v.hp_pct.max;
    if (plr->pclass == CLASS_BLUE_MAGE) /* nerf */
    {
        int pct = 100;
        switch (spell->id.effect)
        {
        case GF_ACID: case GF_ELEC: case GF_FIRE: case GF_COLD:
            pct = 66;
            break;
        case GF_POIS: case GF_NUKE:
            pct = 75;
            break;
        case GF_LIGHT: case GF_DARK: case GF_CONFUSION:
        case GF_NETHER: case GF_SOUND: case GF_SHARDS:
        case GF_CHAOS: case GF_DISENCHANT:
            pct = 80;
            break;
        }
        max = (max*pct + 50)/100;
    }
    return max;
}
static int _breath_amt_plr(mon_spell_ptr spell, mon_race_ptr race)
{
    int pct = spell->parm.v.hp_pct.pct;
    int max = _breath_max_plr(spell);
    int hp = _avg_hp(race);
    int hp_pct = plr->chp * 100 / plr->mhp;
    int con_pct = _breath_con_adj[plr->stat_ind[A_CON]];
    int chp = hp * hp_pct; /* scaled by 100 */
    int dam = chp * pct / 100;
    dam = (dam + 50) / 100;
    if (dam > max) dam = max;
    dam = (dam * con_pct + 50) / 100;
    if (dam > plr->chp) dam = plr->chp;
    return dam;
}
static void _breath(void)
{
    int dam;
    int pct = _current.spell->parm.v.hp_pct.pct;
    int max = _current.spell->parm.v.hp_pct.max;
    int rad = _breath_rad(_current.race);

    assert(_current.spell->parm.tag == MSP_HP_PCT);
    if (_current.flags & MSC_SRC_PLAYER)
    {
        dam = _breath_amt_plr(_current.spell, _current.race);
        plr_breath(rad, _current.dest, _current.spell->id.effect, _scale(dam));
    }
    else
    {
        dam = _current.mon->hp * pct / 100;
        if (dam > max) dam = max;
        mon_breath(_current.mon, rad, _current.dest, _current.spell->id.effect, _scale(dam));
    }
}

static int _roll(dice_t dice)
{
    return dice_roll(dice);
}
static int _avg_roll(dice_t dice)
{
    int roll = dice.base;
    if (dice.dd && dice.ds)
        roll += dice.dd * (dice.ds + 1)/2;
    return roll;
}
static dice_t _mon_dice(mon_ptr mon, mon_spell_ptr spell)
{
    dice_t dice = spell->parm.v.dice;
    if (mon) /* XXX */
        dice.scale = mon->mpower;
    return dice;
}
static bool _is_attack_spell(mon_spell_ptr spell);
static dice_t _plr_dice(mon_spell_ptr spell)
{
    dice_t dice = spell->parm.v.dice;
    /* XXX consider possessor forms as well (e.g. Sorcerer) XXX */
    if (plr->pclass == CLASS_BLUE_MAGE && !(spell->flags & MSF_INNATE))
    {
        if (_is_attack_spell(spell))
            dice.base += plr->to_d_spell;
        dice.scale = spell_power(1000);
    }
    return dice;
}
static dice_t _current_dice(void)
{
    assert(_current.spell->parm.tag = MSP_DICE);
    if (_current.flags & MSC_SRC_PLAYER)
        return _plr_dice(_current.spell);
    return _mon_dice(_current.mon, _current.spell);
}
static void _ball(void)
{
    dice_t dice = _current_dice();
    int    rad = 2;

    if (_current.spell->flags & MSF_BALL0) rad = 0;
    else if (_current.spell->flags & MSF_BALL4) rad = 4;


    assert(_current.spell->parm.tag == MSP_DICE);
    if (_current.spell->id.effect == GF_DISINTEGRATE)
    {
        if (_current.flags & MSC_SRC_PLAYER)
            plr_wrath_of_god(_current.dest, GF_DISINTEGRATE, dice);
        else
            mon_wrath_of_god(_current.mon, _current.dest, GF_DISINTEGRATE, dice);
    }
    else
    {
        int dam = dice_roll(dice);
        if (_current.flags & MSC_SRC_PLAYER)
            plr_ball(rad, _current.dest, _current.spell->id.effect, dam);
        else
            mon_ball(_current.mon, rad, _current.dest, _current.spell->id.effect, dam);
    }
}
static void _bolt(void)
{
    dice_t dice = _current_dice();

    if (mon_race_is_(_current.race, "P.Artemis") && _spell_is_(_current.spell, MST_BOLT, GF_ARROW))
    {
        int i;
        for (i = 0; i < 4; i++)
        {
            int dam = dice_roll(dice);
            mon_bolt(_current.mon, _current.dest, GF_SUPER_ARROW, dam); /* un-reflectable hack */
        }
    }
    else if (_current.flags & MSC_SRC_PLAYER)
    {
        int dam = dice_roll(dice);
        plr_bolt(_current.dest, _current.spell->id.effect, dam);
    }
    else
    {
        int dam = dice_roll(dice);
        mon_bolt(_current.mon, _current.dest, _current.spell->id.effect, dam);
    }
}
static void _beam(void)
{
    dice_t dice = _current_dice();
    if (_current.flags & MSC_SRC_PLAYER)
    {
        int dam = dice_roll(dice);
        plr_beam(_current.dest, _current.spell->id.effect, dam);
    }
    else
    {
        int dam = dice_roll(dice);
        mon_beam(_current.mon, _current.dest, _current.spell->id.effect, dam);
    }
}
static void _los(void)
{
    dice_t dice = _current_dice();
    int    dam = dice_roll(dice);

    if (_current.spell->id.effect == GF_HAND_DOOM) /* XXX hackish */
    {
        if (_current.flags & MSC_DEST_PLAYER)
            dam = dam * plr->chp / 100;
        else if (_current.flags & MSC_SRC_PLAYER)
            dam = 3 * plr->lev;
    }

    /* XXX Samurai Dagonic Flash projects a beam of GF_ATTACK with _HISSATSU_ISSEN
     * ... redo and fix gf_affect_m(GF_ATTACK) to ignore "dam". XXX */
    if (_current.spell->id.effect == GF_ATTACK) dam = 0; 

    /* Curses are now bolts: the monster points at you and curses horribly, so
     * if another monster intervenes, they can't really point at you, can they?
     * Gazes also require bolt semantics ... not sure why these used to be BALL0 */
    if (_current.flags & MSC_SRC_PLAYER)
        plr_bolt(_current.dest, _current.spell->id.effect, dam);
    else
        mon_bolt(_current.mon, _current.dest, _current.spell->id.effect, dam);
}
static int _curse_save_odds_aux(int rlev, int sav)
{
    int roll = 100 + rlev/2;
    int odds = sav * 100 / roll;
    return odds;
}
static int _curse_save_odds(void)
{
    who_t who = who_create_mon(_current.mon);
    return _curse_save_odds_aux(_current.race->alloc.lvl, plr_skill_sav(who));
}
static bool _curse_save(void)
{
    int odds = _curse_save_odds();
    return randint0(100) < odds;
}
static bool _m_resist_tele(mon_ptr mon, cptr name)
{
    if (_1d(100) <= mon_res_pct(mon, GF_TELEPORT))
    {
        mon_lore_resist(mon, GF_TELEPORT);
        if (mon_show_msg(mon))
            msg_format("%s is unaffected!", name);
        return TRUE;
    }
    return FALSE;
}
static void _annoy_m(void)
{
    switch (_current.spell->id.effect)
    {
    case ANNOY_AMNESIA:
        if (!_current.mon2) break; /* MSF_DIRECT */
        gf_affect_m(_who(), _current.mon2, GF_AMNESIA, 0, GF_AFFECT_SPELL);
        break;
    case ANNOY_QUAKE:
        if (_current.flags & MSC_SRC_PLAYER)
            earthquake(plr->pos, 10);
        else
            earthquake_aux(_current.mon->pos, DUN_VIEW_MAX, _current.mon->id);
        break;
    case ANNOY_ANIMATE_DEAD:
        if (_current.flags & MSC_SRC_PLAYER)
            plr_animate_dead();
        else
            mon_animate_dead(_current.mon);
        break;
    case ANNOY_BLIND:
        if (!_current.mon2) break; /* MSF_DIRECT */
        gf_affect_m(_who(), _current.mon2, GF_BLIND, _current.race->alloc.lvl, GF_AFFECT_SPELL);
        break;
    case ANNOY_CONFUSE:
        if (!_current.mon2) break; /* MSF_DIRECT */
        gf_affect_m(_who(), _current.mon2, GF_OLD_CONF, _current.race->alloc.lvl, GF_AFFECT_SPELL);
        break;
    case ANNOY_DARKNESS:
        unlite_room(_current.dest);
        break;
    case ANNOY_PARALYZE:
        if (!_current.mon2) break; /* MSF_DIRECT */
        gf_affect_m(_who(), _current.mon2, GF_PARALYSIS, _current.race->alloc.lvl, GF_AFFECT_SPELL);
        break;
    case ANNOY_SCARE:
        if (!_current.mon2) break; /* MSF_DIRECT */
        gf_affect_m(_who(), _current.mon2, GF_FEAR, _current.race->alloc.lvl, GF_AFFECT_SPELL);
        break;
    case ANNOY_SHRIEK:
        aggravate_monsters(_who());
        break;
    case ANNOY_SLOW:
        if (!_current.mon2) break; /* MSF_DIRECT */
        gf_affect_m(_who(), _current.mon2, GF_SLOW, _current.race->alloc.lvl, GF_AFFECT_SPELL);
        break;
    case ANNOY_TELE_LEVEL: {
        mon_race_ptr race2;
        if (!_current.mon2) break; /* MSF_DIRECT */
        race2 = _current.mon2->race;
        if (_1d(100) <= mon_res_pct(_current.mon2, GF_NEXUS)) /* XXX Keep this? */
        {
            mon_lore_resist(_current.mon2, GF_NEXUS);
            if (mon_show_msg(_current.mon2))
                msg_format("%s is unaffected!", _current.name2);
        }
        else if (_1d(100) <= mon_res_pct(_current.mon2, GF_TELEPORT))
        {
            mon_lore_resist(_current.mon2, GF_TELEPORT);
            if (mon_show_msg(_current.mon2))
                msg_format("%s is unaffected!", _current.name2);
        }
        else if ((_current.mon2->mflag2 & MFLAG2_QUESTOR) ||
                 randint0(100 + _current.race->alloc.lvl/2) < race2->alloc.lvl )
        {
            if (mon_show_msg(_current.mon2))
                msg_format("%s resists the effects!", _current.name2);
        }
        else
        {
            dun_teleport_level_mon(cave, _current.mon2);
        }
        if (mon_is_valid(_current.mon2))
            mon_tim_delete(_current.mon2, MT_SLEEP);
        break; }
    case ANNOY_TELE_TO:
        if (!_current.mon2) break; /* MSF_DIRECT */
        if (!_m_resist_tele(_current.mon2, _current.name2))
        {
            if (_current.mon2->id == plr->riding)
                teleport_player_to(_current.src, TELEPORT_PASSIVE);
            else
                teleport_monster_to(_current.mon2, _current.src, 100, TELEPORT_PASSIVE);
        }
        mon_tim_delete(_current.mon2, MT_SLEEP);
        break;
    case ANNOY_TRAPS:
        if (_current.flags & MSC_SRC_PLAYER)
        {
            if (plr->lev < 30) /* e.g. possessing Wormtongue */
                dun_place_plr_trap_minor(cave, _current.src);
            else
                dun_place_plr_trap_major(cave, _current.src);
        }
        break;
    case ANNOY_WORLD:
        if (_current.flags & MSC_SRC_PLAYER)
            cast_spell(stop_time_spell);
        break;
    case ANNOY_EGO_WHIP:
        mon_tim_add_aux(_current.mon2, T_EGO_WHIP, _roll(_current.spell->parm.v.dice), _current.race->alloc.lvl);
        break;
    }
}
static void _annoy_p(void)
{
    switch (_current.spell->id.effect)
    {
    case ANNOY_AMNESIA:
        gf_affect_p(_who(), GF_AMNESIA, 0, GF_AFFECT_SPELL);
        break;
    case ANNOY_QUAKE:
        earthquake_aux(_current.mon->pos, 10, _current.mon->id);
        break;
    case ANNOY_ANIMATE_DEAD:
        mon_animate_dead(_current.mon);
        break;
    case ANNOY_BLIND:
        gf_affect_p(_who(), GF_BLIND, 0, GF_AFFECT_SPELL);
        break;
    case ANNOY_CONFUSE:
        gf_affect_p(_who(), GF_OLD_CONF, 0, GF_AFFECT_SPELL);
        break;
    case ANNOY_DARKNESS:
        if (plr_tim_find(T_BLIND))
            msg_format("%s mumbles.", _current.name);

        if ( plr->pclass == CLASS_NINJA
          && !mon_race_is_undead(_current.race)
          && !mon_race_vuln(_current.race, GF_LIGHT)
          && _current.race->light + _current.race->lantern >= 0 )
        {
            if (!plr_tim_find(T_BLIND))
                msg_format("%s cast a spell to light up.", _current.name);
            lite_area(0, 3);
        }
        else
        {
            if (!plr_tim_find(T_BLIND))
                msg_format("%s gestures in shadow.", _current.name);
            unlite_area(0, 3);
        }
        break;
    case ANNOY_PARALYZE:
        gf_affect_p(_who(), GF_PARALYSIS, 0, GF_AFFECT_SPELL);
        break;
    case ANNOY_SCARE:
        gf_affect_p(_who(), GF_FEAR, 0, GF_AFFECT_SPELL);
        break;
    case ANNOY_SHRIEK:
        aggravate_monsters(_who());
        break;
    case ANNOY_SLOW:
        if (free_act_save_p(_current.race->alloc.lvl) || _curse_save())
            msg_print("You resist the effects!");
        else
            plr_tim_add(T_SLOW, randint0(4) + 4);
        mon_smart_learn(_current.mon, SM_FREE_ACTION);
        break;
    case ANNOY_TELE_LEVEL:
        if (res_save_default(GF_NEXUS) || _curse_save())
            msg_print("You resist the effects!");
        else
            dun_teleport_level_plr(cave);
        mon_smart_learn(_current.mon, GF_NEXUS);
        break;
    case ANNOY_TELE_TO:
        /* Only powerful monsters can choose this spell when the player is not in
           los. In this case, it is nasty enough to warrant a saving throw. */
        if (!_projectable(_current.src, _current.dest) && _curse_save())
            msg_print("You resist the effects!");
        else if (res_save_default(GF_TELEPORT))
            msg_print("You resist the effects!");
        else
            teleport_player_to(_current.src, TELEPORT_PASSIVE);
        mon_smart_learn(_current.mon, GF_TELEPORT);
        break;
    case ANNOY_TRAPS:
        trap_creation(_current.dest);
        break;
    case ANNOY_WORLD:
        process_the_world(2 + _1d(2), TRUE);
        break;
    /* mental attacks should be parameterized with the monster's level (e.g. _ego_whip_tick) */
    case ANNOY_EGO_WHIP:
        plr_tim_add_aux(T_EGO_WHIP, _roll(_current.spell->parm.v.dice), _current.race->alloc.lvl);
        break;
    }
    /* XXX this sort of stuff needs to be a class hook ... */
    if (plr->pclass == CLASS_RAGE_MAGE)
        rage_mage_spell_reaction(_current.mon);
}
static void _annoy(void)
{
    if (_current.flags & MSC_DEST_PLAYER)
        _annoy_p();
    else
        _annoy_m();
}
static void _biff_p(void)
{
    if (check_foresight()) return;
    switch (_current.spell->id.effect)
    {
    case BIFF_ANTI_MAGIC:
        if (_curse_save())
            msg_print("You resist the effects!");
        else if (mut_present(MUT_ONE_WITH_MAGIC))
            msg_print("You resist the effects!");
        else if (psion_mental_fortress())
            msg_print("Your mental fortress is impenetrable!");
        else
            plr_tim_add(T_NO_SPELLS, 3 + randint1(3));
        break;
    case BIFF_DISPEL_MAGIC:
        if (mut_present(MUT_ONE_WITH_MAGIC))
            msg_print("You resist the effects!");
        else if (psion_mental_fortress())
            msg_print("Your mental fortress is impenetrable!");
        else
        {
            dispel_player();
            if (plr->riding)
                dispel_monster_status(plr_riding_mon());
        }
        break;
    case BIFF_POLYMORPH:
        gf_affect_p(_who(), GF_OLD_POLY, 0, GF_AFFECT_SPELL);
        break;
    }
}
static void _biff_m(void)
{
    if (!_current.mon2) return; /* MSF_DIRECT */
    switch (_current.spell->id.effect)
    {
    case BIFF_ANTI_MAGIC:
        gf_affect_m(_who(), _current.mon2, GF_ANTIMAGIC, mon_save_r_level(_current.race->id), GF_AFFECT_SPELL);
        break;
    case BIFF_DISPEL_MAGIC:
        if (_current.mon2->id == plr->riding) dispel_player();
        dispel_monster_status(_current.mon2);
        break;
    case BIFF_POLYMORPH:
        gf_affect_m(_who(), _current.mon2, GF_OLD_POLY, _current.race->alloc.lvl, GF_AFFECT_SPELL);
        break;
    }
}
static void _biff(void)
{
    if (_current.flags & MSC_DEST_PLAYER)
        _biff_p();
    else
        _biff_m();
}
static void _m_buff(void)
{
    switch (_current.spell->id.effect)
    {
    case BUFF_HASTE:
        mon_tim_add(_current.mon, T_FAST, 50 + _1d(50));
        break;
    case BUFF_INVULN:
        mon_tim_add(_current.mon, T_INVULN, 500 + _1d(1000));
        break;
    case BUFF_BERSERK:
        mon_tim_add(_current.mon, T_BERSERK, 10 + _1d(10));
        break;
    case BUFF_PROT_EVIL:
        mon_tim_add(_current.mon, T_PROT_EVIL, mon_lvl(_current.mon) + _1d(25));
        break;
    case BUFF_PROT_GOOD:
        mon_tim_add(_current.mon, T_PROT_GOOD, mon_lvl(_current.mon) + _1d(25));
        break;
    }
}
static void _p_buff(void)
{
    switch (_current.spell->id.effect)
    {
    case BUFF_HASTE:
        plr_tim_add(T_FAST, 50 + _1d(50));
        break;
    case BUFF_INVULN:
        if (!plr_tim_find(T_INVULN))
            plr_tim_add(T_INVULN, 500 + _1d(1000));
        break;
    case BUFF_BERSERK:
        plr_tim_add(T_BERSERK, 10 + _1d(plr->lev));
        break;
    case BUFF_PROT_EVIL:
        plr_tim_add(T_PROT_EVIL, 3*plr->lev + _1d(25));
        break;
    case BUFF_PROT_GOOD:
        plr_tim_add(T_PROT_GOOD, 3*plr->lev + _1d(25));
        break;
    }
}
static void _buff(void)
{
    if (_current.flags & MSC_SRC_PLAYER)
        _p_buff();
    else
        _m_buff();
}
static void _escape(void)
{
    switch (_current.spell->id.effect)
    {
    case ESCAPE_TELE_SELF:
        if (_current.flags & MSC_SRC_PLAYER)
            teleport_player(10 + 2*_current.race->alloc.lvl, 0);
        else if (plr_block_teleport(_current.mon))
            msg_format("Your magic barrier obstructs the teleportation of %s.", _current.name);
        else
        {
            if (mon_show_msg(_current.mon))
                msg_format("%s teleports away.", _current.name);
            if (plr->riding == _current.mon->id)
                teleport_player(10 + 2*_current.race->alloc.lvl, 0);
            else
                teleport_away_followable(_current.mon);
        }
        break;
    case ESCAPE_TELE_OTHER:
        if (_current.flags & MSC_DEST_PLAYER)
        {
            /* Duelist Unending Pursuit */
            if ( plr->pclass == CLASS_DUELIST
              && _current.mon == who_mon(plr->duelist_target)
              && plr->lev >= 30 )
            {
                if (get_check(format("%^s is attempting to teleport you. Prevent? ", _current.name)))
                {
                    if (one_in_(3))
                        msg_print("Failed!");
                    else
                    {
                        msg_print("You invoke Unending Pursuit ... The duel continues!");
                        break;
                    }
                }
            }
            msg_format("%s teleports you away.", _current.name);
            if (res_save_default(GF_TELEPORT))
                msg_print("You resist the effects!");
            else
                teleport_player_away(_current.mon, 100);
            mon_smart_learn(_current.mon, GF_TELEPORT);
        }
        else if (_current.mon2) /* MSF_DIRECT */
        {
            if (!_m_resist_tele(_current.mon2, _current.name2))
            {
                if (_current.mon2->id == plr->riding)
                    teleport_player_away(_current.mon, MAX_SIGHT * 2 + 5); /* XXX Player targets mount? Seems unlikely ... */
                else
                    teleport_away(_current.mon2, MAX_SIGHT * 2 + 5, TELEPORT_PASSIVE);
            }
            mon_tim_delete(_current.mon2, MT_SLEEP);
        }
        break;
    }
}
static void _m_tactic(void)
{
    switch (_current.spell->id.effect)
    {
    case TACTIC_BLINK:
        if (plr_block_teleport(_current.mon))
            msg_format("Your magic barrier obstructs the teleportation of %s.", _current.name);
        else
        {
            if (!plr_tim_find(T_BLIND) && _current.mon->ml)
                msg_format("%s blinks away.", _current.name);
            if (plr->riding == _current.mon->id)
                teleport_player(10, 0);
            else
                teleport_away(_current.mon, 10, 0);
            plr->update |= PU_MONSTERS;
        }
        break;
    case TACTIC_BLINK_OTHER:
        if (_current.flags & MSC_DEST_PLAYER)
        {
            msg_format("%s blinks you away.", _current.name);
            if (res_save_default(GF_TELEPORT))
                msg_print("You resist the effects!");
            else
                teleport_player_away(_current.mon, 10);
            mon_smart_learn(_current.mon, GF_TELEPORT);
        }
        else
        {
            if (!_m_resist_tele(_current.mon2, _current.name2))
            {
                if (_current.mon2->id == plr->riding)
                    teleport_player(10, 0);
                else
                    teleport_away(_current.mon2, 10, 0);
            }
            mon_tim_delete(_current.mon2, MT_SLEEP);
        }
        break;
    default: { /* JMP_<type> */
        dice_t dice = _current_dice();
        int    dam = dice_roll(dice);
        mon_burst(_current.mon, 5, _current.spell->id.effect, dam);
        if (plr->riding == _current.mon->id)
            teleport_player(10, 0);
        else
            teleport_away(_current.mon, 10, 0); 
        plr->update |= PU_MONSTERS; }
    }
}
static void _p_tactic(void)
{
    switch (_current.spell->id.effect)
    {
    case TACTIC_BLINK:
        teleport_player(10, 0);
        break;
    case TACTIC_BLINK_OTHER:
        if (!_current.mon2) break; /* MSF_DIRECT */
        if (!_m_resist_tele(_current.mon2, _current.name2))
        {
            if (_current.mon2->id == plr->riding)
                teleport_player(10, 0);
            else
                teleport_away(_current.mon2, 10, 0);
        }
        mon_tim_delete(_current.mon2, MT_SLEEP);
        break;
    default: { /* JMP_<type> */
        dice_t dice = _current_dice();
        int    dam = dice_roll(dice);
        plr_burst(5, _current.spell->id.effect, dam);
        teleport_player(10, 0); }
    }
}
static void _tactic(void)
{
    if (_current.flags & MSC_SRC_PLAYER)
        _p_tactic();
    else
        _m_tactic();
}

static void hp_mon(mon_ptr mon, int amt) /* this should be public */
{
    mon->hp += amt;
    if (mon->hp >= mon->maxhp)
    {
        mon->hp = mon->maxhp;
        if (!plr_tim_find(T_BLIND))
        {
            if (mon_show_msg(mon))
                msg_format("%s looks completely healed!", _current.name); /* XXX */
        }
        else if (mon_show_msg(mon))
            msg_format("%s sounds healed!", _current.name); /* XXX */
    }
    else
    {
        if (!plr_tim_find(T_BLIND))
        {
            if (mon_show_msg(mon))
                msg_format("%s looks healthier.", _current.name); /* XXX */
        }
        else if (mon_show_msg(mon))
            msg_format("%s looks sounds healthier.", _current.name); /* XXX */
    }
    check_mon_health_redraw(mon);
    mon_tim_remove(mon, T_FEAR);
}
static void _heal(void)
{
    dice_t dice = _current_dice();
    int    amt = dice_roll(dice);

    if (_current.flags & MSC_SRC_PLAYER)
    {
        hp_player(amt);
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
    }
    else
        hp_mon(_current.mon, amt);
}
static who_t _summon_who(void)
{
    if (_current.flags & MSC_SRC_PLAYER)
        return who_create_plr();
    else if (_current.mon)
        return who_create_mon(_current.mon);
    return who_create_null();
}
static point_t _summon_where(void)
{
    point_t where = _current.dest;
    if (_current.flags & MSC_SRC_PLAYER)
        where = _current.src;
    return where;
}
static u32b _summon_mode(u32b mode)
{
    if (_current.flags & MSC_SRC_PLAYER)
        mode |= PM_FORCE_PET;
    else
        mode |= PM_NO_FRIEND; /* summoned monsters side with summoner, not player */
    return mode;
}
static mon_ptr _summon_race(mon_race_ptr race, u32b mode)
{
    return summon_named_creature(
        _summon_who(),
        _summon_where(),
        race,
        _summon_mode(mode)
    );
}
static mon_ptr _summon_r_idx(cptr which)
{
    mon_race_ptr race = mon_race_parse(which);
    mon_ptr mon = NULL;
    if (race)
        mon = _summon_race(race, PM_ALLOW_GROUP | PM_ALLOW_UNIQUE);
    return mon;
}
static void _summon_type(int type)
{
    summon_specific(
        _summon_who(),
        _summon_where(),
        _current.race->alloc.lvl,
        type,
        _summon_mode(PM_ALLOW_GROUP | PM_ALLOW_UNIQUE)
    );
}
static int _summon_kin_aux(mon_rule_ptr rule)
{
    int result = 0;
    int ct = mon_rule_amt(rule), i;
    mon_race_ptr race = NULL;

    for (i = 0; i < ct; i++)
    {
        if (!race || !(rule->flags & MON_RULE_SAME))
            race = mon_rule_race(rule);
        if (race && _summon_race(race, mon_rule_mode(rule)))
            result++;
    }
    return result;
}
static int _summon_kin(void)
{
    mon_rule_ptr rule;
    int ct = 0;

    if (mon_race_is_(_current.race, "P.Hades"))
        plr_burst(8, GF_LAVA_FLOW, 3);
    if (mon_race_is_(_current.race, "P.Poseidon"))
        plr_burst(8, GF_WATER_FLOW, 3);

    for (rule = _current.race->kin; rule; rule = rule->next)
    {
        int ct2 = _summon_kin_aux(rule);
        ct += ct2;
        if (ct2 && (rule->flags & MON_RULE_STOP))
            break;
    }
    return ct;
}
static void _summon(void)
{
    int ct, i;
    /* check for preferred handling of SUMMON_KIN using mon_race->kin rules */
    if (_current.spell->id.effect == SUMMON_KIN && _current.race->kin)
    {
        _summon_kin();
        return;
    }
    assert(_current.spell->parm.tag == MSP_DICE);
    ct = _roll(_current.spell->parm.v.dice);
    if (_current.spell->id.effect == SUMMON_KIN) /* default if no mon_race->kin rules */
        summon_kin_type = mon_race_char(_current.race);
    for (i = 0; i < ct; i++)
        _summon_type(_current.spell->id.effect);
}
static void _weird_bird_p(void)
{
     if (one_in_(3) || !(_current.flags & MSC_DIRECT))
     {
         msg_format("%s suddenly goes out of your sight!", _current.name);
        teleport_away(_current.mon, 10, TELEPORT_NONMAGICAL);
        plr->update |= PU_MONSTERS;
    }
    else
    {
        int dam = 0;
        int get_damage = 0;

        msg_format("%s holds you, and drops from the sky.", _current.name);
        dam = damroll(4, 8);
        teleport_player_to(_current.src, TELEPORT_NONMAGICAL | TELEPORT_PASSIVE);

        sound(SOUND_FALL);

        if (plr->levitation)
            msg_print("You float gently down to the ground.");
        else
        {
            msg_print("You crashed into the ground.");
            dam += damroll(6, 8);
        }

        /* Mega hack -- this special action deals damage to the player. Therefore the code of "eyeeye" is necessary.
           -- henkma
         */
        get_damage = take_hit(DAMAGE_NOESCAPE, dam, _current.name);
        if (get_damage > 0)
            weaponmaster_do_readied_shot(_current.mon);

        if (plr->revenge && get_damage > 0 && !plr->is_dead)
        {
            char m_name_self[80];
            monster_desc(m_name_self, _current.mon, MD_PRON_VISIBLE | MD_POSSESSIVE | MD_OBJECTIVE);
            msg_format("The attack of %s has wounded %s!", _current.name, m_name_self);
            gf_affect_m(who_create_plr(), _current.mon, GF_MISSILE, psion_backlash_dam(get_damage), 0);
            plr_tim_subtract(T_REVENGE, 5);
        }

        if (plr->riding)
        {
            bool fear;
            mon_take_hit_mon(plr_riding_mon(), dam, &fear,
                 extract_note_dies(real_r_ptr(dun_mon(cave, plr->riding))), _current.mon);
        }
    }
}
static void _weird_bird_m(void)
{
    if (one_in_(3) && (_current.flags & MSC_SRC_MONSTER))
    {
        if (mon_show_msg(_current.mon))
            msg_format("%s suddenly goes out of your sight!", _current.name);
        teleport_away(_current.mon, 10, TELEPORT_NONMAGICAL);
        plr->update |= PU_MONSTERS;
    }
    else if (_current.mon2) /* MSF_DIRECT */
    {
        bool fear = FALSE;
        int dam = 0;
        if (_current.flags & MSC_SRC_PLAYER)
            msg_format("You hold %s and drop from the sky.", _current.name2);
        else if (mon_show_msg(_current.mon) || mon_show_msg(_current.mon2))
            msg_format("%s holds %s and drops from the sky.", _current.name, _current.name2);

        dam = damroll(4, 8);
        if (_current.mon2->id == plr->riding)
            teleport_player_to(_current.src, TELEPORT_NONMAGICAL | TELEPORT_PASSIVE);
        else
            teleport_monster_to(_current.mon2, _current.src, 100, TELEPORT_NONMAGICAL | TELEPORT_PASSIVE);

        sound(SOUND_FALL);
        if (mon_can_fly(_current.mon2))
        {
            if (mon_show_msg(_current.mon2))
                msg_format("%s floats gently down to the ground.", _current.name2);
        }
        else
        {
            if (mon_show_msg(_current.mon2))
                msg_format("%s crashes into the ground.", _current.name2);
            dam += damroll(6, 8);
        }

        if (_current.mon2->id == plr->riding)
        {
            int get_damage = 0;

            /* Mega hack -- this special action deals damage to the player. Therefore the code of "eyeeye" is necessary.
               -- henkma
             */
            get_damage = take_hit(DAMAGE_NOESCAPE, dam, _current.name);
            if (get_damage > 0)
                weaponmaster_do_readied_shot(_current.mon);
            if (plr->revenge && get_damage > 0 && !plr->is_dead)
            {
                char m_name_self[80];
                monster_desc(m_name_self, _current.mon, MD_PRON_VISIBLE | MD_POSSESSIVE | MD_OBJECTIVE);
                msg_format("The attack of %s has wounded %s!", _current.name, m_name_self);
                gf_affect_m(who_create_plr(), _current.mon, GF_MISSILE, psion_backlash_dam(get_damage), 0);
                plr_tim_subtract(T_REVENGE, 5);
            }
        }

        mon_take_hit_mon(_current.mon2, dam, &fear,
            extract_note_dies(real_r_ptr(_current.mon2)), _current.mon);
    }
}
static bool _banor_or_rupart_p(mon_ptr mon)
{
    return mon_race_is_(mon->race, "p.Banor")
        || mon_race_is_(mon->race, "p.Rupart");
}
static bool _unique_is_dead(cptr which) { return mon_race_is_dead_unique(mon_race_parse(which)); }
static void _weird(void)
{
    if (_current.spell->id.effect == WEIRD_BIRD)
    {
        if (_current.flags & MSC_DEST_PLAYER)
            _weird_bird_p();
        else
            _weird_bird_m();
        return;
    }
    if (mon_race_is_(_current.race, "p.Banor=Rupart"))
    {
        int hp = (_current.mon->hp + 1) / 2;
        int maxhp = _current.mon->maxhp/2;
        mon_ptr mon;

        if (!summon_possible(_current.mon->pos)) return;

        delete_monster(_current.mon);
        _current.mon = NULL;
        mon = summon_named_creature(who_create_null(), _current.src, mon_race_parse("p.Banor"), 0);
        if (mon)
        {
            mon->hp = hp;
            mon->maxhp = maxhp;
        }

        mon = summon_named_creature(who_create_null(), _current.src, mon_race_parse("p.Rupart"), 0);
        if (mon)
        {
            mon->hp = hp;
            mon->maxhp = maxhp;
        }

        msg_print("Banor=Rupart splits in two!");
    }
    else if ( mon_race_is_(_current.race, "p.Banor")
           || mon_race_is_(_current.race, "p.Rupart") )
    {
        vec_ptr v;
        int k, hp = 0, maxhp = 0;
        point_t where;

        if (_unique_is_dead("p.Banor") || _unique_is_dead("p.Rupart")) return;

        v = dun_filter_mon(cave, _banor_or_rupart_p);
        for (k = 0; k < vec_length(v); k++)
        {
            mon_ptr mon = vec_get(v, k);
            hp += mon->hp;
            maxhp += mon->maxhp;
            if (mon->race->id != _current.race->id)
                where = mon->pos;
            delete_monster(mon);
        }
        vec_free(v);

        _current.mon = NULL;
        _current.dest = where;
        _current.mon = _summon_r_idx("p.Banor=Rupart");
        if (_current.mon)
        {
            _current.mon->hp = hp;
            _current.mon->maxhp = maxhp;
        }
        msg_print("Banor and Rupart combine into one!");
    }
} 
static void _spell_msg(void);
static void _possessor(void);
static void _spell_cast_aux(void)
{
    if (_current.flags & MSC_SRC_MONSTER)
    {
        assert(_current.mon);
        if (_current.flags & MSC_DEST_PLAYER)
            disturb(1, 0);
        if (_spell_fail() || _spell_blocked())
        {
            _current.fail = TRUE;
            return;
        }
        /* Do lore now since Banor=Rupart may disappear ...
         * Note: We only lore projectable monster moves, so we
         * really should only lore projectable monster spells as
         * well. In addition, include splashes against the player.*/
        if ((_current.flags & MSC_DEST_PLAYER) || plr_view(_current.src))
            mon_lore_spell(_current.mon, _current.spell);
        _current.mon->last_enemy_pos = _current.dest;
    }
    else if (_spell_fail())
    {
        _current.fail = TRUE;
        return;
    }

    _spell_msg();
    switch (_current.spell->id.type)
    {
    case MST_ANNOY:   _annoy();   break;
    case MST_BALL:    _ball();    break;
    case MST_BEAM:    _beam();    break;
    case MST_BIFF:    _biff();    break;
    case MST_BOLT:    _bolt();    break;
    case MST_BREATH:  _breath();  break;
    case MST_BUFF:    _buff();    break;
    case MST_LOS:     _los();     break;
    case MST_ESCAPE:  _escape();  break;
    case MST_HEAL:    _heal();    break;
    case MST_SUMMON:  _summon();  break;
    case MST_TACTIC:  _tactic();  break;
    case MST_WEIRD:   _weird();   break;
    case MST_POSSESSOR: _possessor(); break;
    }
    if (plr->action == ACTION_LEARN && (_current.flags & MSC_SRC_MONSTER) )
        blue_mage_learn(&_current);
}

/*************************************************************************
 * Message
 ************************************************************************/
static char _msg[255];
static cptr _a_an(cptr noun)
{
    if (strchr("aeiou", noun[0])) return "an";
    return "a";
}
static cptr _possessive(mon_race_ptr race)
{
    if (!race) return "its";
    if (mon_race_is_male(race)) return "his";
    if (mon_race_is_female(race)) return "her";
    return "its";
}
/* Some monsters override the default message with something cutesy */
typedef struct {
    cptr race_id;
    mon_spell_id_t spell_id;
    cptr cast_msg;
    cptr blind_msg;
    cptr cast_mon_msg;
    cptr cast_plr_msg;
} _custom_msg_t, *_custom_msg_ptr;
static _custom_msg_t _mon_msg_tbl[] = {
    { "p.ninja", {MST_BOLT, GF_ARROW},
        "$CASTER throws a syuriken.",
        "",
        "$CASTER throws a syuriken at $TARGET.",
        "You throw a syuriken." },
    { "P.Botei", {MST_BREATH, GF_SHARDS},
        "'Botei-Build cutter!!!'",
        "'Botei-Build cutter!!!'",
        "'Botei-Build cutter!!!'",
        "'Botei-Build cutter!!!'" } ,
    { "p.Rolento", {MST_BALL, GF_FIRE},
        "$CASTER throws a hand grenade.", 
        "$CASTER throws a hand grenade.", 
        "$CASTER throws a hand grenade at $TARGET.",
        "You throw a hand grenade." },
    { "p.Rolento", {MST_SUMMON, SUMMON_KIN},
        "$CASTER throws some hand grenades.", 
        "$CASTER throws some hand grenades.", 
        "$CASTER throws some hand grenade at $TARGET.",
        "You throw some hand grenades." },
    { "p.Caldarm", {MST_SUMMON, SUMMON_KIN},
        "$CASTER summons his minions.",
        "$CASTER summons his minions.",
        "$CASTER summons his minions.",
        "You summon your minions." },
    {0}
};
static cptr _custom_msg(void)
{
    int i;
    for (i = 0;; i++)
    {
        _custom_msg_ptr msg = &_mon_msg_tbl[i];
        if (!msg->race_id) return NULL;
        if (!mon_race_is_(_current.race, msg->race_id)) continue;
        if (msg->spell_id.type != _current.spell->id.type) continue;
        if (msg->spell_id.effect != _current.spell->id.effect) continue;
        if (_current.flags & MSC_SRC_PLAYER)
            return msg->cast_plr_msg;
        else if (_current.flags & MSC_DEST_PLAYER)
        {
            if (plr_tim_find(T_BLIND)) return msg->blind_msg;
            return msg->cast_msg;
        }
        else if (_current.flags & MSC_DEST_MONSTER)
        {
            if (_current.flags & MSC_UNVIEW)
                return "";
            else
                return msg->cast_mon_msg;
        }
    }
}
static cptr _display_msg(void)
{
    if (!_current.spell->display) return NULL;
    else if (_current.flags & MSC_SRC_PLAYER)
        return _current.spell->display->cast_plr_msg;
    else if (_current.flags & MSC_DEST_PLAYER)
    {
        if (plr_tim_find(T_BLIND)) return _current.spell->display->blind_msg;
        return _current.spell->display->cast_msg;
    }
    else if (_current.flags & MSC_DEST_MONSTER)
    {
        if (_current.flags & MSC_UNVIEW)
            return "";
        else
            return _current.spell->display->cast_mon_msg;
    }
    return "";
}
static cptr _breath_msg(void)
{
    gf_info_ptr gf = gf_lookup(_current.spell->id.effect);
    assert(gf);
    if (_current.flags & MSC_SRC_PLAYER)
    {
        sprintf(_msg, "You breathe <color:%c>%s</color>.",
            attr_to_attr_char(gf->color), gf->name);
        return _msg;
    }
    else if (_current.flags & MSC_DEST_PLAYER)
    {
        if (plr_tim_find(T_BLIND)) return "$CASTER roars.";
        sprintf(_msg, "$CASTER breathes <color:%c>%s</color>.",
            attr_to_attr_char(gf->color), gf->name);
        return _msg;
    }
    else if (_current.flags & MSC_DEST_MONSTER)
    {
        if (!(_current.flags & MSC_UNVIEW))
        {
            sprintf(_msg, "$CASTER breathes <color:%c>%s</color> at $TARGET.",
                attr_to_attr_char(gf->color), gf->name);
            return _msg;
        }
    }
    return NULL;
}
static cptr _ball_msg(void)
{
    gf_info_ptr gf = gf_lookup(_current.spell->id.effect);
    if (!gf)
    {
        msg_format("Unkown Ball %d", _current.spell->id.effect);
        return NULL;
    }
    assert(gf);
    if (_current.flags & MSC_SRC_PLAYER)
    {
        sprintf(_msg, "You cast %s <color:%c>%s Ball</color>.",
            _a_an(gf->name), attr_to_attr_char(gf->color), gf->name);
        return _msg;
    }
    else if (_current.flags & MSC_DEST_PLAYER)
    {
        if (plr_tim_find(T_BLIND)) return "$CASTER mumbles.";
        sprintf(_msg, "$CASTER casts %s <color:%c>%s Ball</color>.",
            _a_an(gf->name), attr_to_attr_char(gf->color), gf->name);
        return _msg;
    }
    else if (_current.flags & MSC_DEST_MONSTER)
    {
        if (!(_current.flags & MSC_UNVIEW))
        {
            sprintf(_msg, "$CASTER casts %s <color:%c>%s Ball</color> at $TARGET.",
                _a_an(gf->name), attr_to_attr_char(gf->color), gf->name);
            return _msg;
        }
    }
    return NULL;
}
static cptr _bolt_msg(void)
{
    gf_info_ptr gf = gf_lookup(_current.spell->id.effect);
    assert(gf);
    if (_current.flags & MSC_SRC_PLAYER)
    {
        sprintf(_msg, "You cast %s <color:%c>%s Bolt</color>.",
            _a_an(gf->name), attr_to_attr_char(gf->color), gf->name);
        return _msg;
    }
    else if (_current.flags & MSC_DEST_PLAYER)
    {
        if (plr_tim_find(T_BLIND)) return "$CASTER mumbles.";
        sprintf(_msg, "$CASTER casts %s <color:%c>%s Bolt</color>.",
            _a_an(gf->name), attr_to_attr_char(gf->color), gf->name);
        return _msg;
    }
    else if (_current.flags & MSC_DEST_MONSTER)
    {
        if (!(_current.flags & MSC_UNVIEW))
        {
            sprintf(_msg, "$CASTER casts %s <color:%c>%s Bolt</color> at $TARGET.",
                _a_an(gf->name), attr_to_attr_char(gf->color), gf->name);
            return _msg;
        }
    }
    return NULL;
}
static cptr _summon_msg(void)
{
    parse_tbl_ptr p = summon_type_lookup(_current.spell->id.effect);
    p = summon_type_lookup(_current.spell->id.effect);
    assert(p);
    if (_current.flags & MSC_SRC_PLAYER)
    {
        sprintf(_msg, "You summon <color:%c>%s</color>.",
            attr_to_attr_char(p->color), p->name);
        return _msg;
    }
    else if (_current.flags & MSC_DEST_PLAYER)
    {
        if (plr_tim_find(T_BLIND)) return "$CASTER mumbles.";
        sprintf(_msg, "$CASTER summons <color:%c>%s</color>.",
            attr_to_attr_char(p->color), p->name);
        return _msg;
    }
    else if (_current.flags & MSC_DEST_MONSTER)
    {
        if (!(_current.flags & MSC_UNVIEW))
        {
            sprintf(_msg, "$CASTER summons <color:%c>%s</color>.",
                attr_to_attr_char(p->color), p->name);
            return _msg;
        }
    }
    return NULL;
}
static cptr _tactic_msg(void)
{
    if (_current.spell->id.effect < GF_COUNT)
    {
        if (_current.flags & MSC_SRC_PLAYER)
            return "You jump away.";
        else if (!(_current.flags & MSC_UNVIEW))
            return "$CASTER jumps away.";
    }
    return NULL;
}
static cptr _get_msg(void)
{
    cptr msg = _custom_msg();
    if (!msg) msg = _display_msg();
    if (!msg)
    {
        switch (_current.spell->id.type)
        {
        case MST_BREATH:
            msg = _breath_msg();
            break;
        case MST_BALL:
            msg = _ball_msg();
            break;
        case MST_BOLT:
            msg = _bolt_msg();
            break;
        case MST_SUMMON:
            msg = _summon_msg();
            break;
        case MST_TACTIC:
            msg = _tactic_msg();
            break;
        }
    }
    return msg;
}
static cptr _msg_var(cptr var)
{
    if (strcmp(var, "CASTER") == 0)
        return _current.name;
    if (strcmp(var, "CASTER_POS") == 0)
    {
        if (_current.flags & MSC_SRC_PLAYER)
            return "your";
        return _possessive(_current.race);
    }
    if (strcmp(var, "TARGET") == 0)
        return _current.name2;
    if (strcmp(var, "TARGET_POS") == 0)
    {
        if (_current.flags & MSC_DEST_PLAYER)
            return "your";
        return _possessive(_current.mon2->race);
    }
    
    /*return format("<color:v>%s</color>", var);*/
    return "<color:v>?</color>";
}
static bool _is_ident_char(char c)
{
    if (c == '_') return TRUE;
    return BOOL(isupper(c));
}
static void _spell_msg(void)
{
    char  out[255], token[50];
    cptr  pos;
    char *dest;
    cptr  msg = _get_msg();
    if (!msg) return;
    if (!strlen(msg)) return;
    pos = msg;
    dest = out;
    for (;;)
    {
        char c = *pos;
        if (!c) break;
        if (c == '$')
        {
            cptr seek = ++pos;
            cptr replace;
            while (_is_ident_char(*seek))
                seek++;
            strncpy(token, pos, seek - pos);
            token[seek - pos] = '\0';
            replace = _msg_var(token);
            if (replace)
            {
                strcpy(dest, replace); 
                dest += strlen(replace);
            }
            pos = seek;
        }
        else
            *dest++ = *pos++;
    }
    *dest++ = '\0';
    if (!strlen(out)) return;
    msg_print(out);
}

/*************************************************************************
 * AI
 ************************************************************************/
static void _ai_init(mon_spells_ptr spells)
{
    int i, j;
    for (i = 0; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = spells->groups[i];
        _mst_info_ptr       mp;
        if (!group) continue;
        mp = _mst_lookup(i);
        assert(mp);
        for (j = 0; j < group->count; j++)
        {
            mon_spell_ptr spell = &group->spells[j];
            spell->prob = mp->prob;
            if (spell->parm.wgt)
                spell->prob *= spell->parm.wgt;
        }
    }
}

static bool _projectable(point_t src, point_t dest)
{
    return point_project(src, dest);
}
static bool _projectable_splash(mon_ptr mon, point_t dest)
{
    dun_cell_ptr cell;
    if (!mon_project(mon, dest)) return FALSE;
    cell = dun_cell_at(cave, dest);
    if (!illusion_project(cell)) return FALSE;
    return TRUE;
}
static bool _projectable_splash2(mon_ptr mon, point_t dest)
{
    dun_cell_ptr cell;
    if (!mon_project(mon, dest)) return FALSE;
    cell = dun_cell_at(cave, dest);
    if (cell_is_tree(cell)) return TRUE;
    if (floor_has_web(cell)) return TRUE;
    return FALSE;
}
static bool _disintegrable(point_t src, point_t dest)
{
    return dun_in_disintegration_range(cave, src, dest);
}
static bool _distance(point_t src, point_t dest)
{
    return point_distance(src, dest);
}

typedef bool (*_path_p)(mon_ptr mon, point_t dest);
typedef bool (*_spell_p)(mon_spell_ptr spell);

static bool _ball0_p(mon_spell_ptr spell)
{
    return BOOL(spell->flags & MSF_BALL0);
}
static bool _blink_check_p(mon_spell_ptr spell)
{
    switch (spell->id.type)
    {
    case MST_BREATH:
    case MST_BALL:
    case MST_BOLT:
    case MST_BEAM:
    case MST_LOS: return TRUE;
    }
    return _spell_is_(spell, MST_ANNOY, ANNOY_TRAPS);
}
static bool _jump_p(mon_spell_ptr spell)
{
    if (spell->id.type != MST_TACTIC) return FALSE;
    if (spell->id.effect >= TACTIC_BLINK) return FALSE;
    return TRUE;
}

static point_t _choose_splash_point(mon_ptr mon, point_t dest, _path_p filter)
{
    point_t best_pos = {0};
    int     i, best_distance = 99999;

    assert(filter);
    for (i = 0; i < 8; i++)
    {
        point_t pos = point_step(dest, ddd[i]);
        int     d;

        if (!dun_pos_interior(cave, pos)) continue;
        if (!filter(mon, pos)) continue;
        d = _distance(mon->pos, pos);
        if (d < best_distance)
        {
            best_distance = d;
            best_pos = pos;
        }
    }
    return best_pos;
}

static void _adjust_group(mon_spell_group_ptr group, _spell_p p, int pct)
{
    int i;
    if (!group) return;
    for (i = 0; i < group->count; i++)
    {
        mon_spell_ptr spell = &group->spells[i];
        if (p && !p(spell)) continue;
        spell->prob = MIN(250, spell->prob * pct / 100);
    }
}
static void _remove_group(mon_spell_group_ptr group, _spell_p p)
{
    _adjust_group(group, p, 0);
}
static void _adjust_spells(mon_spells_ptr spells, _spell_p p, int pct)
{
    int i;
    for (i = 0; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = spells->groups[i];
        if (!group) continue;
        _adjust_group(group, p, pct);
    }
}
static void _remove_spells(mon_spells_ptr spells, _spell_p p)
{
    _adjust_spells(spells, p, 0);
}
static mon_spell_ptr _find_spell(mon_spells_ptr spells, _spell_p p)
{
    int i, j;
    assert(p);
    for (i = 0; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = spells->groups[i];
        if (!group) continue;
        for (j = 0; j < group->count; j++)
        {
            mon_spell_ptr spell = &group->spells[j];
            if (p(spell)) return spell;
        }
    }
    return NULL;
}

static void _smart_tweak_res_dam(mon_spell_ptr spell, int gf)
{
    int pct, tweak = 100;
    pct = res_pct(gf);
    if (!pct) return;
    if (pct == 100) tweak = 0;
    else if (res_is_high(gf) && pct > 30) tweak = 100 - pct/2;
    else if (pct > 50) tweak = 100 - pct/3;
    spell->prob = MIN(200, spell->prob*tweak/100);
}
static void _smart_tweak_res_sav(mon_spell_ptr spell, int gf)
{
    int pct, tweak = 100, need;
    pct = res_pct(gf);
    if (!pct) return;
    need = res_is_high(gf) ? 33 : 55;
    if (pct >= need) tweak = 0;
    else tweak = 100 - pct*80/need;
    spell->prob = MIN(200, spell->prob*tweak/100);
}
static void _smart_remove_annoy(mon_spell_group_ptr group, mon_ptr mon)
{
    int i;
    if (!group) return;
    for (i = 0; i < group->count; i++)
    {
        mon_spell_ptr spell = &group->spells[i];
        switch (spell->id.effect)
        {
        case ANNOY_BLIND:
            if (mon_has_smart_flag(mon, GF_BLIND))
                _smart_tweak_res_sav(spell, GF_BLIND);
            break;
        case ANNOY_CONFUSE:
            if (mon_has_smart_flag(mon, GF_CONFUSION))
                _smart_tweak_res_sav(spell, GF_CONFUSION);
            break;
        case ANNOY_PARALYZE:
        case ANNOY_SLOW:
            if (mon_has_smart_flag(mon, SM_FREE_ACTION) && plr->free_act)
                spell->prob = 0;
            break;
        case ANNOY_SCARE:
            if (mon_has_smart_flag(mon, GF_FEAR))
            {
                int ct = plr->resist[GF_FEAR];
                int i;
                for (i = 0; i < ct; i++)
                    spell->prob = spell->prob * 75 / 100;
            }
            break;
        case ANNOY_TELE_TO:
            if (mon_has_smart_flag(mon, GF_TELEPORT))
                _smart_tweak_res_sav(spell, GF_TELEPORT);
            break;
        case ANNOY_TELE_LEVEL:
            if (mon_has_smart_flag(mon, GF_NEXUS))
                _smart_tweak_res_sav(spell, GF_NEXUS);
            break;
        }
    }
}
static void _smart_remove_aux(mon_spell_group_ptr group, mon_ptr mon)
{
    int i;
    if (!group) return;
    for (i = 0; i < group->count; i++)
    {
        mon_spell_ptr spell = &group->spells[i];
        gf_info_ptr   gf = gf_lookup(spell->id.effect);
        if (!gf) continue;
        if (GF_RES_MIN <= gf->id && gf->id <= GF_RES_MAX)
        {
            if (mon_has_smart_flag(mon, gf->id))
                _smart_tweak_res_dam(spell, gf->id);
        }
    }
}

static void _remove_spell(mon_spells_ptr spells, mon_spell_id_t id)
{
    mon_spell_ptr spell = mon_spells_find(spells, id);
    if (spell)
        spell->prob = 0;
}

static void _smart_remove(mon_spell_cast_ptr cast)
{
    mon_spells_ptr spells = cast->race->spells;

    _smart_remove_aux(spells->groups[MST_BREATH], cast->mon);
    _smart_remove_aux(spells->groups[MST_BALL], cast->mon);
    if (mon_has_smart_flag(cast->mon, SM_REFLECTION) && plr->reflect)
        _remove_group(spells->groups[MST_BOLT], NULL);
    else
        _smart_remove_aux(spells->groups[MST_BOLT], cast->mon);
    _smart_remove_aux(spells->groups[MST_BEAM], cast->mon);
    _smart_remove_annoy(spells->groups[MST_ANNOY], cast->mon);
}
static bool _summon_possible(point_t where)
{
    return summon_possible(where);
}
static int _wall_ct(point_t pos)
{
    int ct = 0;
    point_t p;

    for (p.y = pos.y - 2; p.y <= pos.y + 2; p.y++)
    {
        for (p.x = pos.x - 2; p.x <= pos.x + 2; p.x++)
        {
            dun_cell_ptr cell;
            if (!dun_pos_interior(cave, p)) continue;
            if (point_fast_distance(p, pos) > 2) continue;
            cell = dun_cell_at(cave, p);
            if (cell->type != FEAT_WALL) continue;
            if (cell->flags & CELL_PERM) continue;
            ct++;
        }
    }
    return ct;
}

/* When wounded, skew the spell probabilities away from offense
 * and trickery to summoning, healing and escape tactics. Make
 * this transition gradual but allow smart monsters to panic near
 * death. */
static void _ai_wounded(mon_spell_cast_ptr cast)
{
    bool           smart  = mon_is_smart(cast->mon);
    mon_spells_ptr spells = cast->race->spells;
    if ( spells->groups[MST_HEAL]
      || spells->groups[MST_ESCAPE]
      || spells->groups[MST_SUMMON] )
    {
        int pct_healthy = cast->mon->hp * 100 / cast->mon->maxhp;
        int pct_wounded = 100 - pct_healthy;
        if (pct_wounded > 20)
        {
            int buff = pct_wounded * pct_wounded * pct_wounded / 500;
            int biff = pct_wounded / 2;
            if (smart && pct_wounded > 90 && one_in_(2))
            {
                biff = 100;
                _adjust_group(spells->groups[MST_SUMMON], NULL, 100 + buff/3);
            }
            _adjust_group(spells->groups[MST_BREATH], NULL, 100 - biff);
            _adjust_group(spells->groups[MST_BALL], NULL, 100 - biff);
            _adjust_group(spells->groups[MST_BOLT], NULL, 100 - biff);
            _adjust_group(spells->groups[MST_BEAM], NULL, 100 - biff);
            _adjust_group(spells->groups[MST_LOS], NULL, 100 - biff);
            _adjust_group(spells->groups[MST_BIFF], NULL, 100 - biff);
            _adjust_group(spells->groups[MST_ANNOY], NULL, 100 - biff);
            _adjust_group(spells->groups[MST_WEIRD], NULL, 100 - biff);

            _adjust_group(spells->groups[MST_HEAL], NULL, 100 + buff);
            _adjust_group(spells->groups[MST_ESCAPE], NULL, 100 + buff);
        }
        else /*if (pct_wounded <= 20)*/
        {
            _remove_group(spells->groups[MST_HEAL], NULL);
            _remove_group(spells->groups[MST_ESCAPE], NULL);
        }
    }
}
static int _antimagic_prob(void)
{
    if (plr->anti_magic)
        return 0;

    if (plr_tim_find(T_NO_SPELLS))
        return 0;

    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_antimagic_prob();

    switch (plr->pclass)
    {
    case CLASS_WARRIOR:
    case CLASS_WEAPONSMITH:
    case CLASS_WEAPONMASTER:
    case CLASS_ARCHER:
    case CLASS_CAVALRY:
    case CLASS_MAULER:
    case CLASS_MAGIC_EATER:
    case CLASS_DEVICEMASTER:
    case CLASS_BLOOD_KNIGHT:
    case CLASS_SAMURAI: /* XXX anti-magic probably should not block Kendo */
    case CLASS_NINJA:   /* XXX nor ninjitsu */
    case CLASS_SNIPER:
    case CLASS_RUNE_KNIGHT:
    case CLASS_RAGE_MAGE: /* XXX */
    case CLASS_DUELIST:   /* XXX */
        return 0;

    case CLASS_ROGUE:
    case CLASS_SCOUT:
    case CLASS_RANGER:
    case CLASS_PALADIN:
    case CLASS_WARRIOR_MAGE:
    case CLASS_CHAOS_WARRIOR:
    case CLASS_MONK:
    case CLASS_MYSTIC:
    case CLASS_BEASTMASTER:
    case CLASS_ARCHAEOLOGIST:
        return 100;

    case CLASS_MINDCRAFTER:
    case CLASS_FORCETRAINER:
    case CLASS_PSION:
    case CLASS_RED_MAGE:
    case CLASS_NECROMANCER:
    case CLASS_PRIEST:
    case CLASS_HIGH_PRIEST:
    case CLASS_BARD:
    case CLASS_TIME_LORD:
    case CLASS_WARLOCK:
    case CLASS_WILD_TALENT:
        return 200;

    case CLASS_MAGE:
    case CLASS_YELLOW_MAGE:
    case CLASS_GRAY_MAGE:
    case CLASS_HIGH_MAGE:
    case CLASS_MIRROR_MASTER:
    case CLASS_BLUE_MAGE:
        return 300;

    case CLASS_SORCERER:
        return 400;

    case CLASS_MONSTER:
        switch (plr->prace)
        {
        case RACE_MON_LICH:
        case RACE_MON_BEHOLDER:
        case RACE_MON_QUYLTHULG:
        case RACE_MON_RING:
            return 300;

        case RACE_MON_POSSESSOR:
        case RACE_MON_MIMIC:
            return possessor_antimagic_prob();

        case RACE_MON_DRAGON:
        case RACE_MON_ANGEL:
        case RACE_MON_DEMON:
        case RACE_MON_LEPRECHAUN:
        case RACE_MON_VAMPIRE:
            return 100;

        case RACE_MON_JELLY:
        case RACE_MON_SPIDER:
        case RACE_MON_XORN:
        case RACE_MON_HOUND:
        case RACE_MON_GIANT:
        case RACE_MON_HYDRA:
        case RACE_MON_TROLL:
        case RACE_MON_ELEMENTAL:
        case RACE_MON_SWORD:
        case RACE_MON_GOLEM:
        case RACE_MON_CENTIPEDE:
        case RACE_MON_VORTEX:
            return 0;
        }
    }

    return 100;
}

static bool _allow_summon(mon_spell_cast_ptr cast)
{
    bool result = FALSE;
    int_map_iter_ptr iter;

    /* pets can always summon (cf _ai_think_pet for PF_SUMMON_SPELL restriction) */
    if (mon_is_pet(cast->mon)) return TRUE;

    /* plr presence allows unrestricted summoning */
    if (plr_view(cast->src)) return TRUE;

    /* pet presence allows unrestricted summoning. otherwise,
     * it is too easy to handle vicious summoners (cf Dread of Night) */
    for (iter = int_map_iter_alloc(cave->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        if (mon_is_pet(mon) && point_project(mon->pos, cast->src))
        {
            result = TRUE;
            break;
        }
    }
    int_map_iter_free(iter);
    return result;
}
static void _ai_direct(mon_spell_cast_ptr cast)
{
    bool           stupid = mon_is_stupid(cast->mon);
    bool           smart  = mon_is_smart(cast->mon);
    mon_spells_ptr spells = cast->race->spells;
    mon_spell_ptr  spell;
    int            dist = _distance(cast->src, cast->dest);

    cast->flags |= MSC_DIRECT;

    /* Stupid monsters are done! */
    if (stupid)
        return;

    /* Apply monster knowledge of player's strengths and weaknesses */
    if (smart_cheat || smart_learn)
        _smart_remove(cast);

    _ai_wounded(cast);

    if (smart)
    {
        /*spell = mon_spells_find(spells, _id(MST_ANNOY, ANNOY_TELE_LEVEL));
        if (spell && TELE_LEVEL_IS_INEFF(0))
            spell->prob = 0;*/
        spell = mon_spells_find(spells, _id(MST_BIFF, BIFF_DISPEL_MAGIC));
        if (spell)
            spell->prob = dispel_check(cast->mon->id) ? 50 : 0;
        spell = mon_spells_find(spells, _id(MST_BIFF, BIFF_ANTI_MAGIC));
        if (spell)
            spell->prob = spell->prob * _antimagic_prob() / 100;
    }
    spell = mon_spells_find(spells, _id(MST_ANNOY, ANNOY_TELE_TO));
    if (spell && spell->prob) /* XXX _smart_remove may notice GF_TELEPORT! */
    {
        if (dist < 2)
            spell->prob = 0;
        else
            spell->prob += cast->mon->anger;
    }
    if (cast->mon->anger)
    {
        spell = mon_spells_find(spells, _id(MST_BALL, GF_BRAIN_SMASH));
        if (spell)
            spell->prob += cast->mon->anger/2;
    }

    spell = mon_spells_find(spells, _id(MST_ANNOY, ANNOY_WORLD));
    if (spell && world_monster) /* prohibit if already cast */
        spell->prob = 0;

    /* XXX Currently, tactical spells involve making space for spellcasting monsters. */
    if (spells->groups[MST_TACTIC] && _find_spell(spells, _blink_check_p) && !world_monster)
    {
        if (dist < 4)
            _adjust_group(spells->groups[MST_TACTIC], NULL, 700);
        else if (dist > 5)
        {
            spell = mon_spells_find(spells, _id(MST_TACTIC, TACTIC_BLINK));
            if (spell)
                spell->prob = 0;
        }
    }
    if (dist > 5)
        _remove_group(spells->groups[MST_TACTIC], _jump_p);

    /* beholders prefer to gaze, but won't do so if blind/adjacent; requires LoS
     * XXX note: _ai_direct is called if projectable (FF_PROJECT). But a curtain
     * might blocks FF_LOS while not blocking FF_PROJECT. cf _ai_indirect for
     * reverse logic ... */
    spell = mon_spells_find(spells, _id(MST_LOS, GF_ATTACK));
    if (spell)
    {
        if (plr_tim_find(T_BLIND) || mon_tim_find(cast->mon, T_BLIND) || !plr_view(cast->mon->pos))
            spell->prob = 0;
        else if (cast->mon->cdis < 2)
            spell->prob = 0;
        else
            spell->prob *= 7;
    }

    /* Useless buffs? */
    spell = mon_spells_find(spells, _id(MST_BUFF, BUFF_INVULN));
    if (spell && mon_tim_find(cast->mon, T_INVULN))
        spell->prob = 0;

    spell = mon_spells_find(spells, _id(MST_BUFF, BUFF_HASTE));
    if (spell && mon_tim_find(cast->mon, T_FAST))
        spell->prob = 0;

    spell = mon_spells_find(spells, _id(MST_BUFF, BUFF_BERSERK));
    if (spell && mon_tim_find(cast->mon, T_BERSERK))
        spell->prob = 0;

    spell = mon_spells_find(spells, _id(MST_BUFF, BUFF_PROT_EVIL));
    if (spell)
    {
        if (mon_tim_find(cast->mon, T_PROT_EVIL) || mon_tim_find(cast->mon, T_INVULN))
            spell->prob = 0;
        else if (plr->align > ALIGN_NEUTRAL_EVIL)
            spell->prob = 0;
        else
            spell->prob = spell->prob * (-plr->align) / 50;
    }

    spell = mon_spells_find(spells, _id(MST_BUFF, BUFF_PROT_GOOD));
    if (spell)
    {
        if (mon_tim_find(cast->mon, T_PROT_GOOD) || mon_tim_find(cast->mon, T_INVULN))
            spell->prob = 0;
        else if (plr->align < ALIGN_NEUTRAL_GOOD)
            spell->prob = 0;
        else
            spell->prob = spell->prob * plr->align / 50;
    }
    /* Uselss annoys? */
    if (!plr->csp)
        _remove_spell(spells, _id(MST_BALL, GF_DRAIN_MANA));
    if (plr_tim_find(T_BLIND))
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_BLIND));
    if (plr_tim_find(T_SLOW))
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_SLOW));
    if (plr_tim_find(T_PARALYZED))
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_PARALYZE));
    if (plr_tim_find(T_CONFUSED))
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_CONFUSE));

    /* require a direct shot to player for bolts, curse, gaze and rocket) */
    if (!mon_clean_bolt(cast->mon, cast->dest))
    {
        _remove_group(spells->groups[MST_BOLT], NULL);
        _remove_group(spells->groups[MST_LOS], NULL);
        _remove_spell(spells, _id(MST_BALL, GF_ROCKET));
    }

    if (spells->groups[MST_SUMMON])
    {
        if (!point_equals(cast->dest, plr->pos) && !_allow_summon(cast)) /* invisibility */
            _remove_group(spells->groups[MST_SUMMON], NULL);
        else if (!_summon_possible(cast->dest))
        {
            _remove_group(spells->groups[MST_SUMMON], NULL);
            spell = mon_spells_find(spells, _id(MST_BREATH, GF_DISINTEGRATE));
            if (spell && _wall_ct(plr->pos) > 1)
                spell->prob *= 10;
            spell = mon_spells_find(spells, _id(MST_BALL, GF_DISINTEGRATE));
            if (spell && _wall_ct(plr->pos) > 1)
                spell->prob *= 10;
        }
        else if (cave->type->id != D_AMBER)
        {
            spell = mon_spells_find(spells, _id(MST_SUMMON, SUMMON_AMBERITE));
            if (spell) spell->prob = 0;
        }
    }

    spell = mon_spells_find(spells, _id(MST_ANNOY, ANNOY_ANIMATE_DEAD));
    if (spell && !raise_possible(cast->mon))
        spell->prob = 0;

    if (plr_tim_find(T_INVULN))
    {
        _remove_group(spells->groups[MST_BREATH], NULL);
        _remove_group(spells->groups[MST_BALL], NULL);
        _remove_group(spells->groups[MST_BOLT], NULL);
        _remove_group(spells->groups[MST_BEAM], NULL);
        _remove_group(spells->groups[MST_LOS], NULL);
        spell = mon_spells_find(spells, _id(MST_BEAM, GF_PSY_SPEAR));
        if (spell)
            spell->prob = 30;
    }
}
static bool _gf_can_uncover(int which) /* FF_TREE *and* FF_WEB */
{
    switch (which)
    {
    case GF_ACID:
    case GF_ELEC:
    case GF_FIRE:
    case GF_COLD:
    case GF_ICE:
    case GF_PLASMA:
    case GF_METEOR:
    case GF_CHAOS:
    case GF_MANA:
    case GF_SHARDS:
    case GF_ROCK:
    case GF_ROCKET:
    case GF_FORCE:
    case GF_GRAVITY:
    case GF_DISINTEGRATE:
        return TRUE;
    }
    return FALSE;
}
static bool _has_uncover_spell_aux(mon_spell_group_ptr group)
{
    int i;
    if (!group) return FALSE;
    for (i = 0; i < group->count; i++)
    {
        mon_spell_ptr spell = &group->spells[i];
        if (_gf_can_uncover(spell->id.effect)) return TRUE;
    }
    return FALSE;
}
static bool _has_uncover_spell(mon_race_ptr race)
{
    return _has_uncover_spell_aux(race->spells->groups[MST_BALL])
        || _has_uncover_spell_aux(race->spells->groups[MST_BREATH]);
}
static void _adjust_group_uncover(mon_spell_group_ptr group)
{
    int i;
    if (!group) return;
    for (i = 0; i < group->count; i++)
    {
        mon_spell_ptr spell = &group->spells[i];
        if (_gf_can_uncover(spell->id.effect)) continue;
        spell->prob = 0;
    }
}
static bool _pt_is_valid(point_t pt)
{
    return dun_pos_interior(cave, pt);
}
bool mon_could_splash(mon_ptr mon, point_t tgt)
{
    point_t pt = _choose_splash_point(mon, tgt, _projectable_splash);
    return _pt_is_valid(pt);
}
static bool _is_web(point_t pos)
{
    dun_cell_ptr cell = dun_cell_at(cave, pos);
    return floor_has_web(cell);
}
static bool _ai_stuck(mon_spell_cast_ptr cast)
{
    mon_spells_ptr spells = cast->race->spells;

    if (!(cast->mon->mflag2 & MFLAG2_WEB)) return FALSE;
    if (!_is_web(cast->mon->pos))
    {
        cast->mon->mflag2 &= ~MFLAG2_WEB;
        return FALSE;
    }

    _remove_group(spells->groups[MST_ANNOY], NULL);
    _remove_group(spells->groups[MST_BOLT], NULL);
    _remove_group(spells->groups[MST_BEAM], NULL);
    _remove_group(spells->groups[MST_LOS], NULL);
    _remove_group(spells->groups[MST_BIFF], NULL);
    _remove_group(spells->groups[MST_BUFF], NULL);
    _remove_group(spells->groups[MST_WEIRD], NULL);
    _remove_group(spells->groups[MST_SUMMON], NULL);
    _remove_group(spells->groups[MST_HEAL], NULL);
    _remove_spell(spells, _id(MST_TACTIC, TACTIC_BLINK_OTHER));

    _adjust_group_uncover(spells->groups[MST_BREATH]);
    _adjust_group_uncover(spells->groups[MST_BALL]);

    cast->dest = cast->src;
    return TRUE;
}
static void _ai_indirect(mon_spell_cast_ptr cast)
{
    bool           stupid = mon_is_stupid(cast->mon);
    bool           smart = mon_is_smart(cast->mon);
    mon_spells_ptr spells = cast->race->spells;
    mon_spell_ptr  spell;
    point_t        new_dest = {0};
    int            prob = 0;

    if (smart) prob = 75;
    else if (mon_race_is_char(cast->race, 'Z')) prob = 33;
    else prob = 50;

    if (!stupid && randint0(100) < prob)
    {
        new_dest = _choose_splash_point(cast->mon, cast->dest, _projectable_splash);
        if (!_pt_is_valid(new_dest) && smart && _has_uncover_spell(cast->race))
            new_dest = _choose_splash_point(cast->mon, cast->dest, _projectable_splash2);
    }

    _remove_group(spells->groups[MST_ANNOY], NULL);
    _remove_group(spells->groups[MST_BOLT], NULL);
    _remove_group(spells->groups[MST_BEAM], NULL);
    _remove_group(spells->groups[MST_LOS], NULL);
    _remove_group(spells->groups[MST_BIFF], NULL);
    _remove_group(spells->groups[MST_BUFF], NULL);
    _remove_group(spells->groups[MST_WEIRD], NULL);
    _remove_spell(spells, _id(MST_BALL, GF_ROCKET));

    if (_pt_is_valid(new_dest))
    {
        dun_cell_ptr cell = dun_cell_at(cave, new_dest);

        cast->dest = new_dest;
        cast->flags |= MSC_SPLASH;

        if (!stupid && (smart_cheat || smart_learn))
            _smart_remove(cast);
        _ai_wounded(cast);

        if (cell_is_tree(cell) || floor_has_web(cell))
        {
            _adjust_group_uncover(spells->groups[MST_BREATH]);
            _adjust_group_uncover(spells->groups[MST_BALL]);
            _remove_group(spells->groups[MST_SUMMON], NULL);
            _remove_group(spells->groups[MST_HEAL], NULL);
            _remove_group(spells->groups[MST_ESCAPE], NULL);
            _remove_group(spells->groups[MST_TACTIC], NULL);
        }
        else
        {
            _adjust_group(spells->groups[MST_BREATH], NULL, 50);
            _adjust_group(spells->groups[MST_BALL], NULL, 50);
            _remove_group(spells->groups[MST_BALL], _ball0_p);
            _adjust_group(spells->groups[MST_SUMMON], NULL, 50);
            _remove_group(spells->groups[MST_TACTIC], _jump_p);
            /* Heal and Self Telportation OK */
            _remove_spell(spells, _id(MST_ESCAPE, ESCAPE_TELE_OTHER));
        }

        /* earthquake to uncover a hiding player */
        spell = mon_spells_find(spells, _id(MST_ANNOY, ANNOY_QUAKE));
        if (spell)
            spell->prob = 30; /* XXX we turned off all of MST_ANNOY above */
    }
    else
    {
        _remove_group(spells->groups[MST_BREATH], NULL);
        _remove_group(spells->groups[MST_BALL], NULL);
        _remove_group(spells->groups[MST_SUMMON], NULL);
        _remove_group(spells->groups[MST_HEAL], NULL);
        _remove_group(spells->groups[MST_ESCAPE], NULL);
        _remove_group(spells->groups[MST_TACTIC], NULL);
        spell = mon_spells_find(spells, _id(MST_BREATH, GF_DISINTEGRATE));
        if ( spell
          && cast->mon->cdis < MAX_RANGE / 2
          && _disintegrable(cast->src, cast->dest)
          && one_in_(spells->freq / 10) )
        {
            spell->prob = 150;
        }
        /* Glass Castle */
        spell = mon_spells_find(spells, _id(MST_BREATH, GF_LIGHT));
        if ( spell
          && cast->mon->cdis < MAX_RANGE/2
          && dun_los(cave, cast->src, cast->dest)
          && one_in_(5) )
        {
            spell->prob = 150;
        }
        spell = mon_spells_find(spells, _id(MST_BREATH, GF_DARK));
        if ( spell
          && cast->mon->cdis < MAX_RANGE/2
          && dun_los(cave, cast->src, cast->dest)
          && one_in_(5) )
        {
            spell->prob = 150;
        }
        /* XXX Splash BA_LIGHT and BA_DARK */

        /* XXX Bring back evil non-direct TELE_TO? */
    }
}
static void _amnesia(mon_spells_ptr spells, mon_ptr mon)
{
    mon_tim_ptr t;
    for (t = mon->timers; t; t = t->next)
    {
        if (t->id == MT_AMNESIA)
        {
            mon_spell_id_t id = mon_spell_unpack(t->parm);
            mon_spell_ptr spell = mon_spells_find(spells, id);
            if (spell) spell->prob = 0;
        }
    }
}
static void _apply_timers(mon_spells_ptr spells, mon_ptr mon)
{
    _amnesia(spells, mon);
    if (mon_tim_find(mon, T_CONFUSED) || mon_tim_find(mon, T_BLIND))
        _remove_spells(spells, _not_innate_p);
}
static void _ai_think(mon_spell_cast_ptr cast)
{
    if (mon_project(cast->mon, cast->dest)) /* fooled by CAVE_ILLUSION */
        _ai_direct(cast);
    else
        _ai_indirect(cast);

    _apply_timers(cast->race->spells, cast->mon); /* XXX do this last! */
}
static mon_spell_ptr _choose_random(mon_spells_ptr spells)
{
    int i, j, total = 0, roll;
    for (i = 0; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = spells->groups[i];
        if (!group) continue;
        for (j = 0; j < group->count; j++)
        {
            mon_spell_ptr spell = &group->spells[j];
            total += spell->prob;
        }
    }
    if (!total) return NULL;
    roll = randint1(total);
    for (i = 0; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = spells->groups[i];
        if (!group) continue;
        for (j = 0; j < group->count; j++)
        {
            mon_spell_ptr spell = &group->spells[j];
            roll -= spell->prob;
            if (roll <= 0) return spell;
        }
    }
    return NULL; /* ?! */
}
static void _ai_choose(mon_spell_cast_ptr cast)
{
    cast->spell = _choose_random(cast->race->spells);
}
static void _ai_think_mon(mon_spell_cast_ptr cast);
static bool _default_ai(mon_spell_cast_ptr cast)
{
    if (!cast->race->spells) return FALSE;
    _ai_init(cast->race->spells);
    if (!_ai_stuck(cast))
    {
        /* XXX I think this logic got lost during the 7.0 mon_spell re-write.
         * Note that _ai_think_mon will currently assert projectability, since
         * I haven't implemented splash AI for mon vs mon fighting. */
        if ( plr->riding 
          && point_equals(cast->dest, plr->pos) /* XXX not confused or blinded */
          && mon_project(cast->mon, cast->dest)
          && one_in_(2) )
        {
            cast->flags &= ~MSC_DEST_PLAYER;
            cast->flags |= MSC_DEST_MONSTER | MSC_DEST_MOUNT;
            cast->mon2 = plr_riding_mon();
            _mon_desc(cast->mon2, cast->name2, 'o');
            assert(point_equals(cast->dest, cast->mon2->pos));
            _ai_think_mon(cast);
        }
        else
        {
            _ai_think(cast);
        }
    }
    _ai_choose(cast);
    return cast->spell != NULL;
}

/*************************************************************************
 * AI Mon
 ************************************************************************/
static vec_ptr _enemies(mon_ptr mon)
{
    vec_ptr v = vec_alloc(NULL);
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(cave->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr tgt = int_map_iter_current(iter);
        if (tgt->id == mon->id) continue;
        if (!are_enemies(mon, tgt)) continue;
        if (!mon_project(mon, tgt->pos)) continue;
        vec_add(v, tgt);
    }
    int_map_iter_free(iter);
    return v;
}
static bool _choose_target(mon_spell_cast_ptr cast)
{
    mon_ptr mon2 = NULL;
    if (mon_is_pet(cast->mon) && who_is_mon(plr->pet_target))
    {
        mon_ptr tgt = who_mon(plr->pet_target);
        if ( tgt->dun == cast->mon->dun
          && mon_project(cast->mon, tgt->pos)
          && tgt != cast->mon ) /* paranoia: plr->pet_target should not be a pet! */
        {
            mon2 = tgt;
        }
    }
    if (!mon2 && cast->mon->target_id)
    {
        mon2 = dun_mon(cave, cast->mon->target_id);
        if (mon2)
        {
            if ( mon2->id == cast->mon->id
              || !are_enemies(cast->mon, mon2)
              || !mon_project(cast->mon, mon2->pos) )
            {
                mon2 = NULL;
            }
        }
    }
    if (!mon2 && cast->mon->pack)
    {
        if (cast->mon->pack->ai == AI_HUNT) mon2 = dun_mon(cave, cast->mon->pack->prey_id);
        if (mon2)
        {
            assert(mon2->id != cast->mon->id);
            if (!mon_project(cast->mon, mon2->pos))
            {
                mon2 = NULL;
            }
        }
    }
    if (!mon2)
    {
        vec_ptr v = _enemies(cast->mon);
        if (vec_length(v))
            mon2 = vec_get(v, randint0(vec_length(v)));
        vec_free(v);
    }
    if (mon2)
    {
        cast->mon2 = mon2;
        _mon_desc(mon2, cast->name2, 'o');
        cast->dest = mon2->pos;
        if (!mon_show_msg(cast->mon) && !mon_show_msg(cast->mon2))
            cast->flags |= MSC_UNVIEW;
        return TRUE;
    }
    return FALSE;
}
static point_t _project_pt(point_t start, point_t stop, int flags)
{
    point_t pt;
    dun_path_ptr path = dun_path_alloc(cave, start, stop, flags);
    dun_path_fix(path, GF_MISSILE);
    pt = path->stop;
    dun_path_free(path);
    return pt;
}
static void _ai_think_pet(mon_spell_cast_ptr cast)
{
    mon_spells_ptr spells = cast->race->spells;
    mon_spell_ptr  spell;

    assert(mon_is_pet(cast->mon));

    _remove_spell(spells, _id(MST_ANNOY, ANNOY_SHRIEK));
    _remove_spell(spells, _id(MST_ANNOY, ANNOY_DARKNESS));
    _remove_spell(spells, _id(MST_ANNOY, ANNOY_TRAPS));

    if (!(plr->pet_extra_flags & PF_TELEPORT))
    {
        _remove_group(spells->groups[MST_TACTIC], NULL);
        _remove_group(spells->groups[MST_ESCAPE], NULL);
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_TELE_TO));
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_TELE_LEVEL));
    }

    if (!(plr->pet_extra_flags & PF_ATTACK_SPELL))
    {
        _remove_group(spells->groups[MST_BREATH], NULL);
        _remove_group(spells->groups[MST_BALL], NULL);
        _remove_group(spells->groups[MST_BOLT], NULL);
        _remove_group(spells->groups[MST_BEAM], NULL);
        _remove_group(spells->groups[MST_LOS], NULL);
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_BLIND));
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_CONFUSE));
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_PARALYZE));
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_SCARE));
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_SLOW));
        _remove_spell(spells, _id(MST_ANNOY, ANNOY_EGO_WHIP));
    }

    if (!(plr->pet_extra_flags & PF_SUMMON_SPELL))
    {
        _remove_group(spells->groups[MST_SUMMON], NULL);
    }

    /* Prevent collateral damage XXX PF_BALL_SPELL is a horrible misnomer XXX
     * All the logic here is from the old mspells2.c code */
    if (!(plr->pet_extra_flags & PF_BALL_SPELL) && (cast->mon->id != plr->riding))
    {
        if (spells->groups[MST_BALL])
        {
            point_t explode = _project_pt(cast->src, cast->dest, 0);

            if (plr_project(explode))
            {
                int dis = plr_distance(explode), i;
                mon_spell_group_ptr group = spells->groups[MST_BALL];
                for (i = 0; i < group->count; i++)
                {
                    mon_spell_ptr spell = &group->spells[i];
                    int           rad = 2;
                    if (spell->flags & MSF_BALL0) rad = 0;
                    else if (spell->flags & MSF_BALL4) rad = 4;
                    if (rad >= dis)
                        spell->prob = 0;
                }
            }
            else /* Glass walls and such have LOS but not PROJECT */
            {
                spell = mon_spells_find(spells, _id(MST_BALL, GF_LIGHT));
                if (spell && plr_distance(explode) <= 4 && plr_view(explode))
                    spell->prob = 0;
            }
        }

        /* rockets project like MST_BOLT spells ... */
        spell = mon_spells_find(spells, _id(MST_BALL, GF_ROCKET));
        if (spell)
        {
            point_t explode = _project_pt(cast->src, cast->dest, PROJECT_STOP);
            if (plr_project(explode) && plr_distance(explode) <= 2)
                spell->prob = 0;
        }

        if (spells->groups[MST_BEAM] && !mon_clean_beam(cast->mon, cast->dest))
        {
            _remove_group(spells->groups[MST_BEAM], NULL);
        }

        if (spells->groups[MST_BREATH])
        {
            int rad = _breath_rad(cast->race);

            if (!mon_clean_breath(cast->mon, cast->dest, rad, GF_MISSILE))
                _remove_group(spells->groups[MST_BREATH], NULL);
            else
            {
                spell = mon_spells_find(spells, _id(MST_BREATH, GF_LIGHT));
                if (spell && !mon_clean_breath(cast->mon, cast->dest, rad, GF_LIGHT))
                    spell->prob = 0;
                spell = mon_spells_find(spells, _id(MST_BREATH, GF_DISINTEGRATE));
                if (spell && !mon_clean_breath(cast->mon, cast->dest, rad, GF_DISINTEGRATE))
                    spell->prob = 0;
            }
        }
    }

    /* Special moves restriction */
    spell = mon_spells_find(spells, _id(MST_WEIRD, WEIRD_SPECIAL));
    if (spell)
    {
        if (mon_race_is_char(cast->race, 'B'))
        {
            if ((plr->pet_extra_flags & (PF_ATTACK_SPELL | PF_TELEPORT)) != (PF_ATTACK_SPELL | PF_TELEPORT))
                spell->prob = 0;
        }
        else spell->prob = 0;
    }
}

static void _ai_think_mon(mon_spell_cast_ptr cast)
{
    mon_spells_ptr spells = cast->race->spells;
    mon_spell_ptr  spell;

    /* _choose_target only selects projectable foes for now */
    assert(_projectable(cast->src, cast->dest));
    cast->flags |= MSC_DIRECT;

    /* Remove spells that don't work well against monsters. For example,
     * traps only affect the player! */
    _remove_spell(spells, _id(MST_ANNOY, ANNOY_AMNESIA));
    _remove_spell(spells, _id(MST_ANNOY, ANNOY_DARKNESS));
    _remove_spell(spells, _id(MST_ANNOY, ANNOY_TRAPS));
    _remove_spell(spells, _id(MST_ANNOY, ANNOY_WORLD));

    /* beholders prefer to gaze, but won't do so if blind/adjacent; requires LoS */
    spell = mon_spells_find(spells, _id(MST_LOS, GF_ATTACK));
    if (spell)
    {
        if (mon_tim_find(cast->mon, T_BLIND))
            spell->prob = 0;
        else if (!cast->mon2)  /* paranoia ... but future might add mon vs mon splash fighting! */
            spell->prob = 0;
        else if (mon_tim_find(cast->mon2, T_BLIND))
            spell->prob = 0;
        else if (!dun_los(cave, cast->src, cast->dest)) /* curtains */
            spell->prob = 0;
        else if (_distance(cast->src, cast->dest) < 2)
            spell->prob = 0;
    }

    /* XXX not implemented ... yet */
    _remove_spell(spells, _id(MST_BALL, GF_DRAIN_MANA));

    /* Restrict summoning to require LoS on the player. Otherwise, 
     * good vs evil monster battles can explode into infinite summoning
     * nonsense. Note that Qlzzqlzuup is STUPID, so we need to check early. */
    if (!_allow_summon(cast))
        _remove_group(spells->groups[MST_SUMMON], NULL); /* XXX before _ai_think_pet */

    if (mon_is_pet(cast->mon))
        _ai_think_pet(cast);

    /* Stupid monsters are done! */
    if (mon_is_stupid(cast->mon))
        return;

    /* Anti-magic caves? Don't bother casting spells with 100% fail rates */
    if (plr_in_dungeon() && (cave->flags & DF_NO_MAGIC))
        _remove_spells(spells, _not_innate_p);

    if (!(cast->flags & MSC_UNVIEW))
        _ai_wounded(cast);

    /* XXX Currently, tactical spells involve making space for spellcasting monsters. */
    if (spells->groups[MST_TACTIC] && _distance(cast->src, cast->dest) < 4 && _find_spell(spells, _blink_check_p))
        _adjust_group(spells->groups[MST_TACTIC], NULL, 700);

    if (_distance(cast->src, cast->dest) > 5)
        _remove_group(spells->groups[MST_TACTIC], _jump_p);

    /* Useless buffs? */
    spell = mon_spells_find(spells, _id(MST_BUFF, BUFF_INVULN));
    if (spell && mon_tim_find(cast->mon, T_INVULN))
        spell->prob = 0;

    spell = mon_spells_find(spells, _id(MST_BUFF, BUFF_HASTE));
    if (spell && mon_tim_find(cast->mon, T_FAST))
        spell->prob = 0;

    spell = mon_spells_find(spells, _id(MST_BUFF, BUFF_BERSERK));
    if (spell && mon_tim_find(cast->mon, T_BERSERK))
        spell->prob = 0;

    spell = mon_spells_find(spells, _id(MST_BUFF, BUFF_PROT_EVIL));
    if (spell)
    {
        if (mon_tim_find(cast->mon, T_PROT_EVIL) || mon_tim_find(cast->mon, T_INVULN))
            spell->prob = 0;
        else if (cast->mon2->align > ALIGN_NEUTRAL_EVIL)
            spell->prob = 0;
        else
            spell->prob = spell->prob * (-cast->mon2->align) / 50;
    }

    spell = mon_spells_find(spells, _id(MST_BUFF, BUFF_PROT_GOOD));
    if (spell)
    {
        if (mon_tim_find(cast->mon, T_PROT_GOOD) || mon_tim_find(cast->mon, T_INVULN))
            spell->prob = 0;
        else if (cast->mon2->align < ALIGN_NEUTRAL_GOOD)
            spell->prob = 0;
        else
            spell->prob = spell->prob * cast->mon2->align / 50;
    }

    /* Useless biffs? */
    spell = mon_spells_find(spells, _id(MST_BIFF, BIFF_DISPEL_MAGIC));
    if (spell && !mon_tim_find(cast->mon2, T_INVULN)) /* XXX mon_tim_dispel_check */
        spell->prob = 0;
    spell = mon_spells_find(spells, _id(MST_BIFF, BIFF_ANTI_MAGIC));
    if (spell && cast->mon2->anti_magic_ct)
        spell->prob = 0;

    /* require a direct shot for bolts, curse, gaze and rocket */
    if (!mon_clean_bolt(cast->mon, cast->dest))
    {
        _remove_group(spells->groups[MST_BOLT], NULL);
        _remove_group(spells->groups[MST_LOS], NULL);
        _remove_spell(spells, _id(MST_BALL, GF_ROCKET));
    }

    if (spells->groups[MST_SUMMON] && !_summon_possible(cast->dest))
    {
        _remove_group(spells->groups[MST_SUMMON], NULL);
        if (mon_race_is_(cast->race, "p.Banor=Rupart"))
            _remove_spell(spells, _id(MST_WEIRD, WEIRD_SPECIAL));
    }

    spell = mon_spells_find(spells, _id(MST_ANNOY, ANNOY_ANIMATE_DEAD));
    if (spell && !raise_possible(cast->mon))
        spell->prob = 0;

    _apply_timers(cast->race->spells, cast->mon); /* XXX do this last! */
}
static bool _default_ai_mon(mon_spell_cast_ptr cast)
{
    if (!cast->race->spells) return FALSE;
    if (!_ai_stuck(cast))
    {
        if (!_choose_target(cast)) return FALSE;
        _ai_init(cast->race->spells);
        _ai_think_mon(cast);
    }
    _ai_choose(cast);
    return cast->spell != NULL;
}

/*************************************************************************
 * Wizard Probe
 ************************************************************************/
static bool _is_attack_spell(mon_spell_ptr spell)
{
    switch (spell->id.type)
    {
    case MST_BREATH: case MST_BALL: case MST_BOLT: case MST_BEAM: case MST_LOS:
        return TRUE;
    }
    return FALSE;
}
static bool _is_gf_spell(mon_spell_ptr spell)
{
    switch (spell->id.type)
    {
    case MST_BREATH: case MST_BALL: case MST_BOLT: case MST_BEAM:
        return TRUE;
    }
    return FALSE;
}
static int _spell_res(mon_spell_ptr spell)
{
    if (_is_gf_spell(spell))
    {
        int gf = gf_resist(spell->id.effect);
        if (gf) return res_pct(gf);
    }
    return 0;
}
static int _align_dam(mon_spell_ptr spell, int dam)
{
    if (_is_gf_spell(spell))
    {
        switch (spell->id.effect)
        {
        case GF_HOLY_FIRE: return plr_holy_dam(dam);
        case GF_HELL_FIRE: return plr_hell_dam(dam);
        }
    }
    return dam;
}
static int _avg_spell_dam_aux(mon_spell_ptr spell, int hp, bool apply_resist)
{
    if (!_is_attack_spell(spell)) return 0;
    if (spell->parm.tag == MSP_DICE)
    {
        dice_t dice = spell->parm.v.dice;
        int dam = _avg_roll(dice);
        int res = apply_resist ? _spell_res(spell) : 0;
        if (apply_resist) /* XXX plr casting Holy Orb XXX */
            dam = _align_dam(spell, dam);
        if (res)
            dam -= dam * res / 100;
        if (spell->id.type == MST_LOS && spell->id.effect == GF_HAND_DOOM)
            dam = plr->chp * dam / 100;
        return dam;
    }
    if (spell->parm.tag == MSP_HP_PCT)
    {
        int dam = hp * spell->parm.v.hp_pct.pct / 100;
        int res = apply_resist ? _spell_res(spell) : 0;
        if (dam > spell->parm.v.hp_pct.max)
            dam = spell->parm.v.hp_pct.max;
        if (apply_resist) /* XXX plr breathing Holy Fire XXX */
            dam = _align_dam(spell, dam);
        if (res)
            dam -= dam * res / 100;
        return dam;
    }
    return 0;
}
int mon_spell_avg_dam(mon_spell_ptr spell, mon_race_ptr race, bool apply_resist)
{
    return _avg_spell_dam_aux(spell, _avg_hp(race), apply_resist);
}
int _avg_spell_dam(mon_ptr mon, mon_spell_ptr spell)
{
    return _avg_spell_dam_aux(spell, mon->hp, TRUE);
}
void mon_spell_wizard(mon_ptr mon, mon_spell_ai ai, doc_ptr doc)
{
    mon_spell_cast_t cast = {0};
    int              i, j, total = 0, total_dam = 0;
    _spell_cast_init(&cast, mon);
    if (!ai) ai = _default_ai;
    if (!cast.race->spells) return;
    if (!ai(&cast))
    {
        ai = _default_ai_mon;
        if (!ai(&cast)) return;
    }
    doc_printf(doc, "<color:G>Spells  :</color> %d%%\n", cast.race->spells->freq + mon->anger);
    for (i = 0; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = cast.race->spells->groups[i];
        if (!group) continue;
        group->prob = 0;
        for (j = 0; j < group->count; j++)
        {
            mon_spell_ptr spell = &group->spells[j];
            total += spell->prob;
            group->prob += spell->prob;
        }
    }
    if (!total) return;
    for (i = 0; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = cast.race->spells->groups[i];
        _mst_info_ptr       mp;
        if (!group) continue;
        mp = _mst_lookup(i);
        assert(mp);
        doc_printf(doc, "%2d.%d%%", group->prob * 100 / total, (group->prob * 1000 / total)%10);
        doc_printf(doc, " <color:%c>%-10.10s</color>", attr_to_attr_char(mp->color), mp->name);
        for (j = 0; j < group->count; j++)
        {
            mon_spell_ptr spell = &group->spells[j];
            int           dam = _avg_spell_dam(mon, spell);
            doc_printf(doc, "<tab:20>%2d.%d%% ", spell->prob * 100 / total, (spell->prob * 1000 / total) % 10);
            mon_spell_doc(spell, doc);
            if (spell->parm.tag == MSP_DICE)
            {
                dice_t dice = spell->parm.v.dice;
                doc_insert(doc, "<tab:50>");
                if (dice.dd)
                    doc_printf(doc, "%dd%d", dice.dd, dice.ds);
                if (dice.base)
                {
                    if (dice.dd) doc_insert(doc, "+");
                    doc_printf(doc, "%d", dice.base);
                }
            }
            else if (spell->parm.tag == MSP_HP_PCT)
            {
                hp_pct_t hp = spell->parm.v.hp_pct;
                int      dam = mon->hp * hp.pct / 100;
                if (dam > hp.max) dam = hp.max;
                doc_printf(doc, "<tab:50>%d", dam);
            }
            if (dam)
            {
                if (spell->id.type == MST_LOS)
                    dam -= dam * _curse_save_odds_aux(cast.race->alloc.lvl, plr->skills.sav) / 100;
                total_dam += spell->prob * dam;
                doc_printf(doc, "<tab:65>%d", dam);
            }
            doc_newline(doc);
        }
    }
    doc_printf(doc, "<tab:65><color:r>%d</color>", total_dam / total);
    doc_newline(doc);
}

/*************************************************************************
 * Savefiles
 ************************************************************************/
void mon_spells_load(mon_spells_ptr spells, savefile_ptr file)
{
    byte type = savefile_read_byte(file); /* never read inside an assert()!! */
    s16b effect, lore;
    assert(type == 0xEE);
    for (;;)
    {
        type = savefile_read_byte(file);
        if (type == 0xFF) break;
        effect = savefile_read_s16b(file);
        lore = savefile_read_s16b(file); /* be sure to read if spell no longer exists */
        if (spells) /* don't break savefiles on r_info.txt edits */
        {
            mon_spell_ptr spell = mon_spells_find(spells, _id(type, effect));
            if (spell)
                spell->lore = lore;
        }
    }
}

void mon_spells_save(mon_spells_ptr spells, savefile_ptr file)
{
    int i,j;
    savefile_write_byte(file, 0xEE); /* early detection of corrupt savefile, please */
    if (spells) /* don't break savefiles on r_info.txt edits */
    {
        for (i = 0; i < MST_COUNT; i++)
        {
            mon_spell_group_ptr group = spells->groups[i];
            if (!group) continue;
            for (j = 0; j < group->count; j++)
            {
                mon_spell_ptr spell = &group->spells[j];
                if (!spell->lore) continue;
                savefile_write_byte(file, spell->id.type);
                savefile_write_s16b(file, spell->id.effect);
                savefile_write_s16b(file, spell->lore);
            }
        }
    }
    savefile_write_byte(file, 0xFF);
}

/*************************************************************************
 * Queries that belong in mon.h.
 ************************************************************************/
bool mon_has_spell_type(mon_ptr mon, int type)
{
    return mon_race_has_spell_type(mon->race, type);
}
bool mon_race_has_spell_type(mon_race_ptr race, int type)
{
    if (!race->spells) return FALSE;
    return race->spells->groups[type] != NULL;
}
bool mon_race_has_spell(mon_race_ptr race, int type, int effect)
{
    if (!race->spells) return FALSE;
    return mon_spells_find(race->spells, _id(type, effect)) != NULL;
}
bool mon_has_summon_spell(mon_ptr mon)
{
    return mon_has_spell_type(mon, MST_SUMMON);
}
bool mon_race_has_summon_spell(mon_race_ptr race)
{
    return mon_race_has_spell_type(race, MST_SUMMON);
}
bool mon_has_breath(mon_ptr mon, int gf)
{
    return mon_race_has_breath(mon->race, gf);
}
bool mon_race_has_breath(mon_race_ptr race, int gf)
{
    if (!race->spells) return FALSE;
    return mon_spells_find(race->spells, _id(MST_BREATH, gf)) != NULL;
}
bool mon_has_attack_spell(mon_ptr mon)
{
    return mon_race_has_attack_spell(mon->race);
}
bool mon_race_has_attack_spell(mon_race_ptr race)
{
    return mon_race_has_spell_type(race, MST_BREATH)
        || mon_race_has_spell_type(race, MST_BALL)
        || mon_race_has_spell_type(race, MST_BOLT)
        || mon_race_has_spell_type(race, MST_BEAM)
        || mon_race_has_spell_type(race, MST_LOS);
}
bool mon_has_worthy_attack_spell(mon_ptr mon)
{
    return mon_race_has_worthy_attack_spell(mon->race);
}
bool mon_race_has_worthy_attack_spell(mon_race_ptr race)
{
    /* XXX */
    return mon_race_has_attack_spell(race);
}
int mon_spell_freq(mon_ptr mon)
{
    return mon_race_spell_freq(mon->race);
}
int mon_race_spell_freq(mon_race_ptr race)
{
    if (!race->spells) return 0;
    return race->spells->freq;
}
bool mon_is_magical(mon_ptr mon)
{
    return mon_race_is_magical(mon->race);
}
bool mon_race_is_magical(mon_race_ptr race)
{
    int i, j;
    if (!race->spells) return FALSE;
    for (i = MST_BALL; i < MST_COUNT; i++) /* assume all MST_BREATH spells are innate */
    {
        mon_spell_group_ptr group = race->spells->groups[i];
        if (!group) continue;
        for (j = 0; j < group->count; j++)
        {
            mon_spell_ptr spell = &group->spells[j];
            if (!(spell->flags & MSF_INNATE)) return TRUE;
        }
    }
    return FALSE;
}
bool mon_has_innate_spell(mon_ptr mon) /* for anti-magic caves */
{
    return mon_race_has_innate_spell(mon->race);
}
bool mon_race_has_innate_spell(mon_race_ptr race)
{
    int i, j;
    if (!race->spells) return FALSE;
    for (i = MST_BREATH; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = race->spells->groups[i];
        if (!group) continue;
        for (j = 0; j < group->count; j++)
        {
            mon_spell_ptr spell = &group->spells[j];
            if (spell->flags & MSF_INNATE) return TRUE;
        }
    }
    return FALSE;
}
bool mon_race_has_noninnate_spell(mon_race_ptr race)
{
    int i, j;
    if (!race->spells) return FALSE;
    for (i = MST_BREATH; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = race->spells->groups[i];
        if (!group) continue;
        for (j = 0; j < group->count; j++)
        {
            mon_spell_ptr spell = &group->spells[j];
            if (!(spell->flags & MSF_INNATE)) return TRUE;
        }
    }
    return FALSE;
}
bool mon_race_needs_mana(mon_race_ptr race) { return mon_race_has_noninnate_spell(race); }
bool mon_race_has_invulnerability(mon_race_ptr race)
{
    if (!race->spells) return FALSE;
    return mon_spells_find(race->spells, _id(MST_BUFF, BUFF_INVULN)) != NULL;
}
bool mon_race_has_healing(mon_race_ptr race)
{
    if (!race->spells) return FALSE;
    return mon_spells_find(race->spells, _id(MST_HEAL, HEAL_SELF)) != NULL;
}
bool mon_race_has_drain_mana(mon_race_ptr race)
{
    if (!race->spells) return FALSE;
    return mon_spells_find(race->spells, _id(MST_BALL, GF_DRAIN_MANA)) != NULL;
}
bool mon_race_can_summon(mon_race_ptr race, int summon_type)
{
    mon_spell_group_ptr group;
    int                i;
    if (!race->spells) return FALSE;
    group = race->spells->groups[MST_SUMMON];
    if (!group) return FALSE;
    for (i = 0; i < group->count; i++)
    {
        mon_spell_ptr spell = &group->spells[i];
        if (spell->id.effect == summon_type) return TRUE;
    }
    return FALSE;
}
bool mon_race_can_teleport(mon_race_ptr race)
{
    if (!race->spells) return FALSE;
    return mon_spells_find(race->spells, _id(MST_ESCAPE, ESCAPE_TELE_SELF)) != NULL;
}
bool mon_race_has_lite_dark_spell(mon_race_ptr race) /* glass castle */
{
    if (!race->spells) return FALSE;
    return mon_spells_find(race->spells, _id(MST_BREATH, GF_LIGHT))
        || mon_spells_find(race->spells, _id(MST_BREATH, GF_DARK))
        || mon_spells_find(race->spells, _id(MST_BALL, GF_LIGHT))
        || mon_spells_find(race->spells, _id(MST_BALL, GF_DARK));
}

/*************************************************************************
 * Possessor/Mimic/Blue-Mage
 ************************************************************************/
static void _possessor(void)
{
    switch (_current.spell->id.effect)
    {
    case POS_DETECT_TRAPS:
        detect_traps(DETECT_RAD_DEFAULT, TRUE);
        break;
    case POS_DETECT_EVIL:
        detect_monsters_evil(DETECT_RAD_DEFAULT);
        break;
    case POS_DETECT_MONSTERS:
        detect_monsters_normal(DETECT_RAD_DEFAULT);
        break;
    case POS_DETECT_OBJECTS:
        detect_objects_normal(DETECT_RAD_DEFAULT);
        break;
    case POS_IDENTIFY:
        ident_spell(NULL);
        break;
    case POS_MAPPING:
        map_area(DETECT_RAD_MAP);
        break;
    case POS_CLAIRVOYANCE:
        wiz_lite();
        break;
    case POS_MULTIPLY:
        /* XXX cleanup */
        _summon_r_idx(sym_str(plr->current_r_idx));
        break;
    case POS_BLESS:
        plr_tim_add(T_BLESSED, randint1(12) + 12);
        break;
    case POS_HEROISM:
        plr_tim_add(T_HERO, randint1(25) + 25);
        break;
    case POS_WEB:
        cast_spell(spider_web_spell);
    }
}
static void _sync_term(doc_ptr doc)
{
    rect_t r = ui_map_rect();
    Term_load();
    doc_sync_term(doc, doc_range_top_lines(doc, r.cy), doc_pos_create(r.x, r.y));
}
static int _inkey(void)
{
    return inkey_special(TRUE);
}
static int _dam_cost_imp(int dam, int range, int div, int step)
{
    int cost = 0;
    while (dam > 0)
    {
        int xtra = MIN(range, dam);
        cost += (10*xtra + div - 1)/div;
        dam -= xtra;
        div += step;
    }
    return (cost + 5)/10;
}
static int _dam_cost(int dam)
{
    return _dam_cost_imp(dam, 50, 6, 1);
}
static int _breath_cost_pct1(mon_race_ptr race)
{
    switch (mon_race_char(race))
    {
    case 'D': return 90;
    case 'U': return 93;
    case 'd': return 95;
    case 'v': return 105;
    case 'Z': return 110;
    case 'b': return 120;
    }
    return 100;
}
static int _breath_cost_pct2(int gf)
{
    switch (gf)
    {
    case GF_LIGHT: case GF_DARK: case GF_CONFUSION:
        return 105;
    case GF_NETHER: case GF_SOUND: case GF_SHARDS:
    case GF_PLASMA: case GF_FORCE: case GF_INERTIA:
        return 107;
    case GF_CHAOS: case GF_DISENCHANT: case GF_MANA:
        return 110;
    case GF_GRAVITY: case GF_TIME: case GF_DISINTEGRATE: case GF_STORM:
        return 112;
    case GF_HOLY_FIRE: case GF_HELL_FIRE:
        return 115;
    }
    return 100;
}
static int _breath_cost(mon_spell_ptr spell, mon_race_ptr race)
{
    int dam = mon_spell_avg_dam(spell, race, FALSE);
    int max = _breath_max_plr(spell);
    int base = _dam_cost(MIN(max, dam));
    int pct1 = _breath_cost_pct1(race);
    int pct2 = _breath_cost_pct2(spell->id.effect);
    return (base*pct1*pct2 + 5000)/10000;
}
static int _heal_pct(mon_race_ptr race)
{
    switch (race->body.class_id)
    {
    case CLASS_PRIEST: return 75;
    case CLASS_HIGH_PRIEST: return 70;
    case CLASS_PALADIN: return 85;
    case CLASS_MYSTIC:
    case CLASS_MONK: return 100;
    case CLASS_MAGE: return 110;
    case CLASS_ROGUE: return 125;
    case CLASS_RAGE_MAGE:
    case CLASS_WARRIOR:
    case CLASS_MAULER: return 150;
    }
    return 125;
}
static int _buff_cost(mon_spell_ptr spell)
{
    switch (spell->id.effect)
    {
    case BUFF_HASTE: return 15;
    case BUFF_INVULN: return 80;
    case BUFF_BERSERK: return 10;
    case BUFF_PROT_EVIL: return 25;
    case BUFF_PROT_GOOD: return 25;
    }
    return 0;
}
static int _biff_cost(mon_spell_ptr spell)
{
    switch (spell->id.effect)
    {
    case BIFF_ANTI_MAGIC: return 35;
    case BIFF_DISPEL_MAGIC: return 25;
    case BIFF_POLYMORPH: return 15;
    }
    return 0;
}
static int _escape_cost(mon_spell_ptr spell)
{
    switch (spell->id.effect)
    {
    case ESCAPE_TELE_OTHER: return 12;
    case ESCAPE_TELE_SELF: return 7;
    }
    return 0;
}
static int _annoy_cost(mon_spell_ptr spell)
{
    switch (spell->id.effect)
    {
    case ANNOY_AMNESIA: return 10;
    case ANNOY_ANIMATE_DEAD: return 15;
    case ANNOY_BLIND: return 5;
    case ANNOY_CONFUSE: return 5;
    case ANNOY_DARKNESS: return 1;
    case ANNOY_PARALYZE: return 10;
    case ANNOY_SCARE: return 5;
    case ANNOY_SHRIEK: return 3;
    case ANNOY_SLOW: return 10;
    case ANNOY_TELE_LEVEL: return 20;
    case ANNOY_TELE_TO: return 15;
    case ANNOY_TRAPS: return 10;
    case ANNOY_WORLD: return 150;
    case ANNOY_EGO_WHIP: return 20;
    }
    return 0;
}
static int _summon_cost(mon_spell_ptr spell)
{
    parse_tbl_ptr p = summon_type_lookup(spell->id.effect);
    if (p) return p->xtra;
    return 0;
}
static int _tactic_cost(mon_spell_ptr spell)
{
    switch (spell->id.effect)
    {
    case TACTIC_BLINK: return 2;
    case TACTIC_BLINK_OTHER: return 10;
    default: return 15;
    }
    return 0;
}
static int _weird_cost(mon_spell_ptr spell, mon_race_ptr race)
{
    switch (spell->id.effect)
    {
    case WEIRD_BIRD: return 15;
    case WEIRD_SPECIAL: return 10; /* might use race someday */
    }
    return 0;
}
static int _possessor_cost(mon_spell_ptr spell, mon_race_ptr race)
{
    switch (spell->id.effect)
    {
    case POS_DETECT_TRAPS: return 1;
    case POS_DETECT_EVIL: return 2;
    case POS_DETECT_MONSTERS: return 2;
    case POS_DETECT_OBJECTS: return 3;
    case POS_IDENTIFY: return 5;
    case POS_MAPPING: return 8;
    case POS_CLAIRVOYANCE: return 30;
    case POS_MULTIPLY: return 1 + race->alloc.lvl/2;
    case POS_BLESS: return 3;
    case POS_HEROISM: return 8;
    case POS_WEB: return 10;
    }
    return 0;
}
static int _spell_cost_aux(mon_spell_ptr spell, mon_race_ptr race)
{
    switch (spell->id.type)
    {
    case MST_BREATH: return _breath_cost(spell, race);
    case MST_LOS:
        if (spell->id.effect == GF_ATTACK) return 25;
        /* FALL THROUGH */
    case MST_BALL:
    case MST_BOLT:
    case MST_BEAM:
        return _dam_cost(mon_spell_avg_dam(spell, race, FALSE));
    case MST_HEAL: {
        dice_t dice = spell->parm.v.dice;
        int heal = _avg_roll(dice);
        int pct = _heal_pct(race);
        return _dam_cost(heal)*pct/100; }
    case MST_BUFF: return _buff_cost(spell);
    case MST_BIFF: return _biff_cost(spell);
    case MST_ESCAPE: return _escape_cost(spell);
    case MST_ANNOY: return _annoy_cost(spell);
    case MST_SUMMON: return _summon_cost(spell);
    case MST_TACTIC: return _tactic_cost(spell);
    case MST_WEIRD: return _weird_cost(spell, race);
    case MST_POSSESSOR: return _possessor_cost(spell, race);
    }
    return 0;
}
static bool _allow_dec_mana(mon_spell_ptr spell, mon_race_ptr race)
{
    /* For the possessor, race->body.class_idx implies the "caster_info" which
     * governs whether or not plr->dec_mana can be set (CASTER_ALLOW_DEC_MANA). */
    if (plr->pclass != CLASS_BLUE_MAGE) return TRUE;
    /* But the Blue-Mage can learn from various races, so we need special handling. */
    if (spell->flags & MSF_INNATE) return FALSE;
    switch (race->body.class_id)
    {
    case CLASS_MAGE:
    case CLASS_HIGH_MAGE:
    case CLASS_SORCERER:
    case CLASS_PRIEST: /* XXX */
    case CLASS_HIGH_PRIEST:
        return TRUE;
    }
    return FALSE;
}
int mon_spell_cost_plr(mon_spell_ptr spell, mon_race_ptr race)
{
    int cost = _spell_cost_aux(spell, race);
    if (plr->dec_mana && cost > 0 && _allow_dec_mana(spell, race))
        cost = MAX(1, (cost + 1) * dec_mana_cost(plr->dec_mana) / 100);
    return MAX(1, cost);
}
void mon_spell_list_info(doc_ptr doc, mon_spell_ptr spell, mon_race_ptr race)
{
    switch (spell->id.type)
    {
    case MST_BREATH:
        doc_printf(doc, " dam %d", _breath_amt_plr(spell, race));
        break;
    case MST_LOS:
        if (spell->id.effect == GF_ATTACK) return;
        /* FALL THROUGH */
    case MST_BALL:
    case MST_BOLT:
    case MST_BEAM:
        doc_space(doc);
        doc_insert(doc, dice_info_dam(_plr_dice(spell)));
        break;
    case MST_HEAL:
        doc_space(doc);
        doc_insert(doc, dice_info_heal(_plr_dice(spell)));
        break;
    }
}
static bool _no_magic(void)
{
    if (plr->anti_magic) return TRUE;
    if (cave->type->id == D_SURFACE && (cave->flags & DF_NO_MAGIC)) return TRUE;
    return FALSE;
}
static int _cmp_spells(mon_spell_ptr left, mon_spell_ptr right)
{
    if (left->id.type < right->id.type) return -1;
    if (left->id.type > right->id.type) return 1;
    /* XXX */
    if (left->id.effect < right->id.effect) return -1;
    if (left->id.effect > right->id.effect) return 1;
    return 0;
}
static vec_ptr _spells_plr(mon_race_ptr race, _spell_p filter)
{
    vec_ptr v = vec_alloc(NULL);
    int     i, j;
    bool    no_magic = _no_magic();
    bool    blind = plr_tim_find(T_BLIND);

    for (i = 0; i < MST_COUNT; i++)
    {
        mon_spell_group_ptr group = race->spells->groups[i];
        if (!group) continue;
        for (j = 0; j < group->count; j++)
        {
            mon_spell_ptr spell = &group->spells[j];
            if (no_magic && !(spell->flags & MSF_INNATE)) continue;
            if (blind && spell->id.type == MST_LOS) continue; /* no cursing or gazing */
            if (filter && !filter(spell)) continue;

            if ( _spell_is_(spell, MST_BUFF, BUFF_INVULN)
              && plr->prace == RACE_MON_MIMIC
              && plr->lev < 45 ) continue;

            vec_add(v, spell);
        }
    }

    vec_sort(v, (vec_cmp_f)_cmp_spells);
    return v;
}
static void _list_spells(doc_ptr doc, vec_ptr spells, mon_spell_cast_ptr cast)
{
    int i;
    doc_insert(doc, " <color:R>Cast which spell?</color>");
    doc_insert(doc, "<color:G><tab:30>Cost Info</color>\n");
    for (i = 0; i < vec_length(spells); i++)
    {
        mon_spell_ptr spell = vec_get(spells, i);
        int           cost = 0;
        int           avail = 0;
        int           color = 'y';

        if (cast->flags & MSC_SRC_PLAYER)
        {
            cost = mon_spell_cost_plr(spell, cast->race);
            avail = plr->csp;
            if (spell->flags & MSF_INNATE)
                avail += plr->chp;
            if (cost > avail) color = 'D';
        }
        else if (spell == cast->spell)
            color = 'v';
        doc_printf(doc, " <color:%c>%c</color>) ", color, I2A(i));
        mon_spell_doc(spell, doc);
        if (cost)
            doc_printf(doc, "<tab:30>%4d", cost);
        else
            doc_insert(doc, "<tab:30>    ");
        mon_spell_list_info(doc, spell, cast->race);
        doc_newline(doc);
    }
}
static void _prompt_plr_aux(mon_spell_cast_ptr cast, vec_ptr spells)
{
    doc_ptr doc;
    int     cmd, i;
    bool    monster = BOOL(cast->flags & MSC_SRC_MONSTER); /* wizard */

    if (!monster && REPEAT_PULL(&cmd))
    {
        i = A2I(cmd);
        if (0 <= i && i < vec_length(spells))
        {
            mon_spell_ptr spell = vec_get(spells, i);
            int           cost = mon_spell_cost_plr(spell, cast->race);
            int           avail = plr->csp;
            if (spell->flags & MSF_INNATE)
                avail += plr->chp;
            if (cost <= avail)
            {
                cast->spell = spell;
                return;
            }
        }
    }

    doc = doc_alloc(MIN(72, ui_map_rect().cx));
    msg_line_clear();
    Term_save();
    for (;;)
    {
        doc_clear(doc);
        _list_spells(doc, spells, cast);
        _sync_term(doc);
        cmd = _inkey();
        if (cmd == ESCAPE) break;
        if (islower(cmd))
        {
            i = A2I(cmd);
            if (0 <= i && i < vec_length(spells))
            {
                mon_spell_ptr spell = vec_get(spells, i);
                int           cost = mon_spell_cost_plr(spell, cast->race);
                int           avail = plr->csp;
                if (spell->flags & MSF_INNATE)
                    avail += plr->chp;
                if (!monster && cost > avail) continue; /* already grayed out */
                cast->spell = spell;
                REPEAT_PUSH(cmd);
                break;
            }
        }
    }
    Term_load();
    doc_free(doc);
}
static void _set_target(mon_spell_cast_ptr cast, int m_idx)
{
    cast->mon2 = dun_mon(cave, m_idx);
    _mon_desc(cast->mon2, cast->name2, 'o');
    cast->dest = cast->mon2->pos;
}
static bool _spell_is_breath(mon_spell_ptr spell) { return spell->id.type == MST_BREATH; }
static bool _spell_is_offense(mon_spell_ptr spell) { return _is_attack_spell(spell) && !_spell_is_breath(spell); }
static bool _spell_is_summon(mon_spell_ptr spell) { return spell->id.type == MST_SUMMON; }
static bool _spell_is_defense(mon_spell_ptr spell) { return !_is_attack_spell(spell) && !_spell_is_summon(spell); }
typedef struct {
    cptr name;
    _spell_p filter;
} _group_t, *_group_ptr;
static _group_t _groups[] = {
    { "Breath", _spell_is_breath },
    { "Offense", _spell_is_offense },
    { "Defense", _spell_is_defense },
    { "Summon", _spell_is_summon },
    { 0 }
};
static vec_ptr _spell_groups(mon_race_ptr race)
{
    int i;
    vec_ptr groups = vec_alloc(NULL);
    for (i = 0;; i++)
    {
        _group_ptr g = &_groups[i];
        vec_ptr    v;
        if (!g->name) break;
        v = _spells_plr(race, g->filter);
        if (vec_length(v))
            vec_add(groups, g);
        vec_free(v);
    }
    return groups;
}
static void _list_groups(doc_ptr doc, vec_ptr groups)
{
    int i;
    doc_insert(doc, " <color:R>Cast spell from which group?</color>\n");
    for (i = 0; i < vec_length(groups); i++)
    {
        _group_ptr group = vec_get(groups, i);
        doc_printf(doc, " <color:y>%c</color>) %s\n", I2A(i), group->name);
    }
}
static vec_ptr _prompt_spell_group(mon_race_ptr race)
{
    doc_ptr doc;
    int     cmd, i;
    vec_ptr groups = _spell_groups(race);
    vec_ptr spells = NULL;

    if (REPEAT_PULL(&cmd))
    {
        i = A2I(cmd);
        if (0 <= i && i < vec_length(groups))
        {
            _group_ptr g = vec_get(groups, i);
            spells = _spells_plr(race, g->filter);
            vec_free(groups);
            return spells;
        }
    }

    doc = doc_alloc(MIN(72, ui_map_rect().cx));
    msg_line_clear();
    Term_save();
    for (;;)
    {
        doc_clear(doc);
        _list_groups(doc, groups);
        _sync_term(doc);
        cmd = _inkey();
        if (cmd == ESCAPE) break;
        if (islower(cmd))
        {
            i = A2I(cmd);
            if (0 <= i && i < vec_length(groups))
            {
                _group_ptr g = vec_get(groups, i);
                spells = _spells_plr(race, g->filter);
                REPEAT_PUSH(cmd);
                break;
            }
        }
    }
    Term_load();
    doc_free(doc);
    vec_free(groups);
    return spells;
}
static bool _prompt_plr_target(mon_spell_cast_ptr cast)
{
    if ( cast->spell != NULL
      && (cast->flags & MSC_SRC_PLAYER)
      && (cast->spell->flags & MSF_TARGET) )
    {
        if (cast->spell->flags & MSF_DIRECT)
        {
            mon_ptr mon = plr_target_mon();
            if (!mon) return FALSE;
            _set_target(cast, mon->id);
        }
        else
        {
            point_t pos;
            mon_ptr mon;
            if (cast->spell->id.type == MST_BOLT)
                pos = get_fire_pos();
            else if (_spell_is_(cast->spell, MST_BREATH, GF_DISINTEGRATE))
                pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL | TARGET_DISI);
            else if (_spell_is_(cast->spell, MST_LOS, GF_ATTACK))
                pos = get_fire_pos_aux(TARGET_KILL | TARGET_LOS);
            else if (cast->spell->id.type == MST_LOS)
                pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL | TARGET_LOS);
            else
                pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
            if (!dun_pos_interior(cave, pos)) return FALSE;
            mon = dun_mon_at(cave, pos);
            if (mon) _set_target(cast, mon->id);
            else
            {
                dun_cell_ptr cell = dun_cell_at(cave, pos);
                cast->dest = pos;
                sprintf(cast->name2, "the %s", cell_desc(cell));
            }
        }
    }
    return cast->spell != NULL;
}
static bool _prompt_plr(mon_spell_cast_ptr cast)
{
    vec_ptr spells = _spells_plr(cast->race, NULL);
    if (vec_length(spells) > 26)
    {
        vec_free(spells);
        spells = _prompt_spell_group(cast->race);
        if (!spells) return FALSE;
    }
    if (vec_length(spells))
        _prompt_plr_aux(cast, spells);
    vec_free(spells);

    return _prompt_plr_target(cast);
}
bool mon_spell_cast_possessor(mon_race_ptr race)
{
    mon_spell_cast_t cast = {0};
    _spell_cast_init_plr(&cast, race);
    assert(cast.race->spells);
    if (_prompt_plr(&cast))
    {
        int cost = mon_spell_cost_plr(cast.spell, cast.race);
        if ((cast.spell->flags & MSF_INNATE) && plr->csp < cost)
        {
            int hp = cost - plr->csp;
            sp_player(-plr->csp);
            take_hit(DAMAGE_USELIFE, hp, "concentrating too hard");
        }
        else sp_player(-cost);
        _current = cast;
        _spell_cast_aux();
        memset(&_current, 0, sizeof(mon_spell_cast_t));
        return TRUE;
    }
    return FALSE;
}
int mon_spell_cast_blue_mage(mon_spell_ptr spell, mon_race_ptr race)
{
    mon_spell_cast_t cast = {0};
    int rc = CAST_ABORT;
    _spell_cast_init_plr(&cast, race);
    cast.spell = spell;
    /* XXX The Blue-Mage will provide its own spell selection UI, but
     * we must handle the actual target selection and casting. */
    if (_prompt_plr_target(&cast))
    {
        int cost = mon_spell_cost_plr(cast.spell, cast.race);
        if ((cast.spell->flags & MSF_INNATE) && plr->csp < cost)
        {
            int hp = cost - plr->csp;
            sp_player(-plr->csp);
            take_hit(DAMAGE_USELIFE, hp, "concentrating too hard");
        }
        else
        {
            assert(plr->csp >= cost); /* XXX Blue-Mage needs to verify this */
            sp_player(-cost);
        }
        _current = cast;
        _spell_cast_aux();
        rc = _current.fail ? CAST_FAIL : CAST_OK;
        memset(&_current, 0, sizeof(mon_spell_cast_t));
    }
    return rc;
}
bool mon_spell_ai_wizard(mon_spell_cast_ptr cast)
{
    if (!cast->race->spells) return FALSE;
    if ((cast->flags & MSC_DEST_MONSTER) && !_choose_target(cast)) return FALSE;
    if ((cast->flags & MSC_DEST_PLAYER) && !_default_ai(cast)) return FALSE;
    return _prompt_plr(cast);
}

/* pick a random spell for monster amnesia */
mon_spell_ptr mon_spell_random(mon_ptr mon)
{
    mon_race_ptr race = mon->race;
    if (!race->spells) return NULL;
    _ai_init(race->spells);
    _amnesia(race->spells, mon);
    return _choose_random(race->spells);
}

