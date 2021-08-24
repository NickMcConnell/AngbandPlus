Option Descriptions
===================

Options are accessible through the ``=`` command, which provides an
interface to the various sets of options available to the player.

In the descriptions below, each option is listed as the textual summary
which is shown on the "options" screen, plus the internal name of the
option in brackets, followed by a textual description of the option.

Note that the internal name of the option can be used in user pref files to
force the option to a given setting, see "customize.txt" for more info.

Various concepts are mentioned in the descriptions below, including 
"disturb", (cancel any running, resting, or repeated commands, which are in
progress), "flush" (forget any keypresses waiting in the keypress queue),
"fresh" (dump any pending output to the screen), and "sub-windows" (see
below).

User Interface Options
----------------------

..

Rogue-like commands ``rogue_like_commands``
  Selects the "roguelike" command set (see "command.txt" for info).

..

Use old target by default ``use_old_target``
  Forces all commands which normally ask for a "direction" to use the
  current "target" if there is one. Use of this option can be dangerous if
  you target locations on the ground, unless you clear them when done.

..

Always pickup items ``pickup_always``
  Automatically picks up items when you walk upon them, provided it is safe
  to do so.

..

Always pickup items matching inventory ``pickup_inven``
  Like ``pickup_always``, but picks up an item only if it is a copy of an
  item that you already have in your inventory.

..

Notify on object recharge ``notify_recharge``
  This causes the game to print a message when any rechargeable object
  (i.e. a rod or activatable weapon or armour item) finishes recharging.

..

Show flavors in object descriptions ``show_flavors``
  Display "flavors" (color or variety) in object descriptions, even for
  objects whose type is known. This does not affect objects in stores.

..

Center map continuously ``center_player``
  The map always centres on the player with this option on. With it off, it
  is divided into 25 sections, with coordinates (0,0) to (4,4), and will
  show one section at a time - the display will "flip" to the next section
  when the player nears the edge.

..

Disturb whenever viewable monster moves ``disturb_near``
  Disturb the player when any viewable monster moves, and also whenever any
  monster becomes viewable for the first time. This option ignores the
  existence of "telepathy" for the purpose of determining whether a monster is
  "viewable".

..

Show damage player deals to monsters ``show_damage``
  Shows the damage that the player deals to monsters for melee and ranged
  combat in the messages.

..

Color: Illuminate torchlight in yellow ``view_yellow_light``
  This option causes special colors to be used for "torch-lit" grids.
  Turning this option off will slightly improve game speed.

..

Color: Shimmer multi-colored things ``animate_flicker``
  Certain powerful monsters and items will shimmer in real time, i.e.
  between keypresses.

..

Color: Player color indicates low hit points ``hp_changes_color``
  This option makes the player ``@`` turn various shades of colour from
  white to red, depending on percentage of HP remaining.

..

Color: Show unique monsters in purple ``purple_uniques``
  All "unique" monsters will be shown in a light purple colour, which is
  not used for any "normal" monsters - so you can tell at a glance that
  they are unique. If you like the idea but don't like the colour, you can
  edit it via the "interact with colors" option.

..

Show walls as solid blocks ``solid_walls``
  Walls are solid blocks instead of # for granite and % for veins. Veins
  are coloured darker than granite for differentiation purposes.

..

Show walls with shaded background ``hybrid_walls``
  Walls appear as # and % symbols overlaid on a gray background block.
  This overrides ``solid_walls``.

..

Use sound ``use_sound``
  Turns on sound effects, if your system supports them.

..

Show effective speed as multiplier ``effective_speed``
  Instead of showing absolute speed modifier (e.g. 'Slow (-2)' or 'Fast (+38)'),
  show the effective rate at which the character is moving (e.g. 'Slow (x0.8)'
  or 'Fast (x4.1)').

MAngband options
----------------

..

Use orange color for torch-lit grids ``view_orange_light``
  This option causes special "torch-lit" grids to be displayed in "orange"
  instead of "yellow".

..

Use special color for party leader ``highlight_leader``
  This option activates a special color scheme for party leaders, so that party
  members can quickly determine who's the leader and follow that player in
  the dungeon easily.

..

Disturb whenever map panel changes ``disturb_panel``
  This option causes you to be disturbed by the screen "scrolling", as it does
  when you get close to the "edge" of the screen.

..

Always say Yes to Yes/No prompts ``auto_accept``
  Allows to say "yes" to any "[y/n]" prompt without having to press a key.

..

Get out of icky screens when disturbed ``disturb_icky``
  This causes the game to leave "icky" screens when the character is disturbed,
  as if the ESC key was pressed. Very helpful to avoid being killed while
  consulting knowledge screens, setting options or creating keymaps.

..

Active auto-retaliator ``active_auto_retaliator``
  If this option is on, a player is able to auto-attack any monster when there
  is only one monster around the player. If this option is off, or when there
  are more than one monster around the player, a player need to target
  a monster first in order to attack it repeatedly (by moving into that monster
  or using the target command).

..

Freeze screen after detecting monsters ``pause_after_detect``
  This option causes detected monsters to stay displayed on the screen until
  the player presses a command. If the option is off, the screen will be
  automatically refreshed after one turn has passed.

..

Disturb whenever monsters bash down doors ``disturb_bash``
  Disturb the player when any monster bashes a door on the level.

..

Activate fire-till-kill mode ``fire_till_kill``
  This option enables repeat casting and shooting until there are no more
  enemies on the screen (or the player is disturbed).

..

Kick out when starving while at full hps ``disturb_faint``
  Disconnect the player immediately if starving while at full hps. This will
  prevent people from starving while afk.

..

Risky casting ``risky_casting``
  Allow the player to cast spells while not having the required amount of mana
  at the risk of damaging constitution and getting paralyzed

Birth options
-------------

..

Force player descent ``birth_force_descend``
  Upwards staircases do not work. All downward staircases, including the
  one in town, transport the character one level below the previous maximum
  depth. Recalling from the dungeon works and brings the character to the
  town. However, recalling from town brings the character one level
  below the previous maximum depth. The character cannot recall from quest
  levels until the quest is complete, however you will be warned before
  descending into a quest level. Any status effects that sometimes
  teleports the character up and sometimes teleports them down will always
  choose down. When combined with the option for word of recall scrolls
  to have no effect, this recreates the previous "ironman" option.

..

Word of Recall has no effect ``birth_no_recall``
  Word of Recall scrolls have no effect. When combined with the option
  to force player descent, this recreates the previous "ironman" option.

..

Restrict creation of artifacts ``birth_no_artifacts``
  No artifacts will be created. Ever. Just *how* masochistic are you?

..

Show level feelings ``birth_feelings``
  With this option turned on, the game will give you hints about what a new
  level has on it. With this option off, these hints will not be shown.

..

Increase gold drops but disable selling ``birth_no_selling``
  Shopkeepers will never pay you for items you sell, though they will still
  identify unknown items for you, and will still sell you their wares. To
  balance out income in the game, gold found in the dungeon will be
  increased if this option is on.

..

Start with a kit of useful gear ``birth_start_kit``
  Start with items, a useful option for new players, or ones that wish
  to descend immediately into the dungeon. If turned off, the character
  will start with additional gold with which to purchase starting gear.

..

Restrict the use of stores/home ``birth_no_stores``
  The stores are all closed. The home is someone else's, and locked. You
  can keep nothing but what you carry with you, and get nothing but what
  you find in the dungeon. No selling items, or buying mushrooms of 
  vigor... Not recommended for new players, or indeed for sane players.

..

Death is permanent ``birth_no_ghost``
  This option, not recommended for non-advanced players, prevents dead
  characters from turning into ghosts. Death is final, like in most single
  player roguelikes.

..

Play as a fruit bat ``birth_fruit_bat``
  Turns you into a fruit bat at birth, giving you +10 speed at the cost of 40%
  of your maximum HPs. This makes the early game a lot easier and the end game
  a lot harder. Not available if you play a Dragon.

Window flags
------------

Some platforms support "sub-windows", which are windows which can be used
to display useful information generally available through other means. The
best thing about these windows is that they are updated automatically
(usually) to reflect the current state of the world. The "window options"
can be used to specify what should be displayed in each window. The 
possible choices should be pretty obvious.

..

Display inven/equip
  Display the player inventory (and sometimes the equipment).

..

Display equip/inven
  Display the player equipment (and sometimes the inventory).

..

Display player (basic)
  Display a brief description of the character, including a breakdown of
  the current player "skills" (including attacks/shots per round).

..

Display player (extra)
  Display a special description of the character, including some of the
  "flags" which pertain to a character, and a breakdown of the contributions
  of each equipment item to various resistances and stats.

..

Display player (compact)
  Display a brief description of the character (also available on the main
  window).

..

Display map view
  Display an overhead view of the entire dungeon level.

Display messages
  Display the most recently generated "messages".

..

Display monster recall
  Display a description of the monster which has been most recently
  attacked, targeted, or examined in some way.

..

Display object recall
  Display a description of the most recently examined object.

..

Display monster list
  Display a list of monsters you know about and their distance from you (also
  available via the '[' command).

..

Display status
  Display the current status of the player, with permanent or temporary boosts,
  resistances and status ailments (also available on the main window).

..

Display item list
  Display a list of items you know about and their distance from you.

..
 
Display chat messages
  Display chat messages in a separate sub-window. This will only work when
  selecting "Term-4" (or "Chat Window") as the chat window.

..

Display spell list
  Display the list of all spells that have been learned by the player.

..

Display special info
  Display special recall screens in a separate sub-window.

Left Over Information
---------------------

The ``hitpoint_warn`` value, if non-zero, is the percentage of maximal
hitpoints at which the player is warned that they may die. It is also used as
the cut-off for using the color red to display both hitpoints and mana, and as
a trigger for the time bubble slowdown effect.

The ``delay_factor`` value, if non-zero, will slow down the visual effects
used for missile, bolt, beam, and ball attacks. The actual time delay is
equal to ``delay_factor`` squared, in milliseconds.

The ``ignore_lvl`` value, if non-zero, is the quality level of items that are
considered "junk items" by the player. It can range from 1 (worthless items)
to 4 (all items except artifacts). This value can be set for nine different
categories of items: jewelry, dragon scale mails, melee weapons, missiles that
can be fired, missiles that can be thrown, other wearable items, books, junk
items and other consumable items.

The ``lazymove_delay`` value, if non-zero, will allow the player to move
diagonally by pressing the two appropriate arrow keys within the delay time.
This may be useful particularly when using a keyboard with no numpad.
