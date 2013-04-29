/* list-terrain-flags.h - monster race blow effects
 *
 * Adjusting these flags does not break savefiles. Flags below start from 1
 * on line 11, so a flag's sequence number is its line number minus 10.
 *
 *
 */

/*  symbol     descr */
TF(NONE,        "")
TF(LOS,        "Allows line of sight")
TF(PROJECT,    "Allows projections to pass through")
TF(PASSABLE,   "Can be passed through by all creatures")
TF(INTERESTING,"Is noticed on looking around")
TF(PERMANENT,  "Is permanent")
TF(NO_SCENT,   "Cannot store scent")
TF(OBJECT,     "Can hold objects")
TF(TORCH_ONLY, "Becomes bright only when torch-lit")
TF(FLOOR,      "Is a clear floor")
TF(WALL,       "Is a solid wall")
TF(ROCK,       "Is rocky")
TF(GRANITE,    "Is a granite rock wall")
TF(DOOR_ANY,   "Is any door")
TF(DOOR_CLOSED,"Is a closed door")
TF(SHOP,       "Is a shop")
TF(TREE,       "Is a tree")
TF(TRAP,       "Can hold a trap")
TF(STAIR,      "Is a stair or path")
TF(RUNE,       "Is a rune")
TF(DOOR_JAMMED,"Is a jammed door")
TF(HIDE_OBJ,   "Hides objects")
TF(NO_NOISE,   "Cannot transmit sound")
TF(HIDDEN,     "Can be found by searching")
TF(GOLD,       "Contains treasure")
TF(TRAPPABLE,  "Can hold a trap")
TF(ORGANIC,    "Is growing")
TF(BURN,       "Can burn")
TF(FREEZE,     "Can freeze")
TF(FIERY,      "Is fire-based")
TF(WATERY,     "Is water based")
TF(ICY,        "Is ice based")
TF(PROTECT,    "Makes the occupant harder to hit")
TF(EXPOSE,     "Makes the occupant easier to hit")
TF(CLOSABLE,   "Can be closed")
TF(DOOR_LOCKED,"Is a locked door")
TF(MAGMA,      "Is a magma seam")
TF(QUARTZ,     "Is a quartz seam")
TF(EASY,       "Is easily passed through")
TF(RUN1,       "First choice for running")
TF(RUN2,       "Second choice for running")
TF(FALL,       "Non-flying creatures will fall")