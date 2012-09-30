from event import *
import event
import spell
import player
import quest
from io import msg_print

# Print a message after entering the quest
def event_feeling(level, questnumber):
        if (questnumber != 1):
                return 0
        msg_print("You wake up in a prison cell.")
        msg_print("All your possessions have been stolen!")
        event.remove_handler(event.EVENT_FEELING, event_feeling)
        return 1

# Handle player movement
def event_move(y, x, p_y, p_x):
        if ((y == 3) and (x == 3)):
                msg_print("You see light shining through a small window in the cell door.")
        elif ((y == 4) and (x == 3)):
                msg_print("There is a strong magical rune blocking the doorway.")
                return 1
        elif ((y == 1) and (x == 7)):
                msg_print("Looks like another prison cell.")
        # The main hall has an alarm system
        elif (((y == 14) or (y == 15) or (y == 16)) and (x == 19)):
                from cave import set_feat
                from player import *
                spell.aggravate_monsters(0)
                msg_print("An alarm sounds.")
                set_feat(15, 20, 38)
                msg_print("The door behind you closes.")
                set_feat(12, 6, 4)
                set_feat(18, 6, 4)
                set_feat(12, 10, 4)
                set_feat(18, 10, 4)
                set_feat(12, 14, 4)
                set_feat(18, 14, 4)
                set_feat(12, 18, 4)
                set_feat(18, 18, 4)
                event.remove_handler(event.EVENT_MOVE, event_move)
        return 0

search_counter = 0

# You can find an escape
def event_search(y, x):
        if ((y == 1) and (x == 6)):
                import quest1
                quest1.search_counter = quest1.search_counter + 1
                if (quest1.search_counter > 5):
                        msg_print("One of the stones in the wall is lose.")
                        msg_print("You have found an escape tunnel!")
                        from cave import set_feat
                        set_feat(1, 6, 1)
                        event.remove_handler(event.EVENT_SEARCH, event_search)
                        del quest1.search_counter
                        return 1
        return 0

# Clean up when leaving the quest
def event_go_up(level, questnumber):
        if (questnumber != 1):
                return 0
        if (quest.status(1) < quest.QUEST_STATUS_COMPLETED):
                msg_print("You have not finished your quest.")
                return 1
        msg_print("The entrance collapses behind you!")
        event.remove_handler(event.EVENT_GO_UP, event_go_up)
        event.remove_handler(event.EVENT_MOVE, event_move)
        event.remove_handler(event.EVENT_SEARCH, event_search)
        event.remove_handler(event.EVENT_FEELING, event_feeling)
        return 0

# When the player enter
def event_enter(questnumber, danger_level):
        if (questnumber != 1):
                return 0
        import object
        from player import *
        import cave
        player.player_place(3, 3)
        msg_print("You feel a vicious blow on your head.")
        # Take away all items and store them in another room
        for i in range(object.INVEN_TOTAL-1, -1, -1):
                an_object = object.inventory(i)
                if (an_object.k_idx):
                       object.inven_increase(i, -99)
                       object.inven_optimize(i)
                       an_object.drop_near(-1, 3, 22)
        # Install hooks
        event.add_handler(event.EVENT_MOVE, event_move)
        event.add_handler(event.EVENT_SEARCH, event_search)
        event.add_handler(event.EVENT_GO_UP, event_go_up)
        event.add_handler(event.EVENT_FEELING, event_feeling)
        return 1
