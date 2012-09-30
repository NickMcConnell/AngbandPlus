import event
import cave
import quest
from io import msg_print

# Print a message when trying to use illegal command
def event_command_magic(key):
        if((key == "m") or (key == "r") or (key == "q") or (key == "a") or (key == "z") or (key == "u") or (key == "A") or (key == "f") or (key == "v")):
                msg_print("You are not allowed to do that now.")
                return 1
        return 0

def event_attack():
        msg_print("You can't attack the thread hand-to-hand.")
        return 1

def event_xtra_power(choice):
        if(choice != 0):
                msg_print("You are not allowed to do that now.")
                return 1
        return 0

# Return the percentage of trees on the level
def count_trees():
        num = 0
        for y in range(1, 11):
                for x in range(1, 45):
                        if(cave.get_feat(y, x) == cave.FEAT_TREES):
                                num = num + 1
        # This convert the number into a percentage
        return (num * 100 / 440)

def event_go_up(level, questnumber):
        if (questnumber != 30):
                return 0
        if (count_trees() < 40):
                msg_print("You have failed to save at least 50% of the forest. Quest NOT finished.")
                quest.set_status(30, quest.QUEST_STATUS_TAKEN)
        event.remove_handler(event.EVENT_ATTACK, event_attack)
        event.remove_handler(event.EVENT_GO_UP, event_go_up)
        event.remove_handler(event.EVENT_COMMAND, event_command_magic)
        event.remove_handler(event.EVENT_XTRA_POWER, event_xtra_power)
        return 0

def event_enter(questnum, danger_level):
        if(questnum != 30):
                return 0
        event.add_handler(event.EVENT_COMMAND, event_command_magic)
        event.add_handler(event.EVENT_XTRA_POWER, event_xtra_power)
        event.add_handler(event.EVENT_GO_UP, event_go_up)
        event.add_handler(event.EVENT_ATTACK, event_attack)
        return 1
