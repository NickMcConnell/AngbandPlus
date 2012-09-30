--
-- This file is loaded at the initialisation of ToME
-- Load the system functions
--

-- Name of globals to save
__loadsave_name = {}
__loadsave_max = 0
__loadsave_tmp = 0

tome_dofile("player.lua")
tome_dofile("objects.lua")
tome_dofile("monsters.lua")
tome_dofile("powers.lua")
tome_dofile("building.lua")
tome_dofile("s_aux.lua")

-- Add the schools of magic
tome_dofile("spells.lua")

-- Add the quest helpers
tome_dofile("quests.lua")

-- Add the bounty quest
tome_dofile("bounty.lua")

--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
-----Include here all your files( tome_dofile("foo.lua") )----
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
tome_dofile("intro.lua")
tome_dofile("lebohaum.lua")

-- This is a test file, if it is not present, it is very well
tome_dofile("test.lua")

--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--
-- Do not thouch after this line
--
tome_dofile("quests2.lua")
