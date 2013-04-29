*** WARNING: Unangband 0.4.7 versions are unstable,
and have not been thoroughly tested. There is at least
1 bug with level generation that will wreck your
savefile, by panic saving before the level is generated.

This occurs frequently (ie every 3 or 4 levels
generated) and I am still debugging this.

Having said that, the rest of the improvements are
quite fun, if a little cosmetic until friendly monsters
are implemented.


26-12-2001 Unangband 0.4.7a Bugfix release
- Fixed bug with statues resulting in infinite loops.
- Added rock hounds.
- Added bottles, cages, figurines, sacks, boxes.
- Some internal changes to tval/sval names.

26-12-2001 Unangband 0.4.7 "Silent Night release"
- Modified monster drops so that each tval will only
be dropped by some types of monsters.
- Don't remember all types of floors. Need to get
these to color correctly in a future release.
- Made brass lanterns orange, like brass monsters, and
shards of pottery light umber.
- Updated cockroaches to use correct graphic.
- Night and mirkwood goblins did too much melee damage.
- Fixed bug with reading in inscription preferences.
- Fixed bug with filling from stack of lanterns.
- Remove duplicated unique eagles in monster list.
- Added option to allow more than 255 features in
dungeon. Most of the new feature flags are not
implemented yet. You'll also need to switch on the new
Variant save-file option so that you don't lose the
features when you save your game.
- Changed rods/spells to use timeouts rather than pvals.
All artifacts now charge in pack and floor as well as
when wielded.
- Added variant_drop_body option to allow monster
corpses. Regenerate monsters need their corpse destroyed
or they will quickly come back to life.
- Added eggs which hatch into monsters after a length of
time.
- Changed chests to be a floor item and use the '*'
graphic. Some of the trapped chests match treasure in
wall graphic, but only when trap detected on them.
- Renamed stone visages to stone idols.
- Faerie lights were way wrong.
