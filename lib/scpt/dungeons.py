import dungeon
import object
import cave
import misc
from io import msg_print

def enter_levels(level):
        if ((level == 530) and (dungeon.dungeon_type() == dungeon.DUNGEON_HELL)):
                msg_print("You feel as if this level was special..")
                an_object = object.create_artifact(127)
                if(an_object):
                        an_object.drop_near(-1, misc.rand_int(cave.cur_hgt() - 1) + 1, misc.rand_int(cave.cur_wid() - 1) + 1)
                an_object = object.create_artifact(147)
                if(an_object):
                        an_object.drop_near(-1, misc.rand_int(cave.cur_hgt() - 1) + 1, misc.rand_int(cave.cur_wid() - 1) + 1)
		return 1

	# Still unrecognized, throw "bad command" error
	return 0
