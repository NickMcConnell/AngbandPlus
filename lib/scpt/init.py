# The 'io' module lets us use stuff like 'msg_print'

import io

import error

# The 'event' module lets us handle game events
import event

# The 'quest' module lets us handle quest things
import quest

import shoutkey

import quest1

import intro

import quest30

def enter_quests(quest_number, danger_level):
        if(quest_number == 1):
                quest1.event_enter(quest_number, danger_level)
        elif(quest_number == 30):
                quest30.event_enter(quest_number, danger_level)
        return 0

#
# This code is run every time the game is started.
#
# Here we associate some events to functions
#

scpt_name = "PernAngband default script"

event.add_handler(event.EVENT_COMMAND, shoutkey.event_command)
event.add_handler(event.EVENT_START_GAME, intro.intro)
event.add_handler(event.EVENT_ENTER_QUEST, enter_quests)
