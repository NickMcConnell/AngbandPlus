--
-- This file is loaded at the initialisation of ToME
-- Load the system functions
--

-- Name of globals to save
__loadsave_name = {}
__loadsave_max = 0
__loadsave_tmp = 0

-- Very thin xml parser(49 lines ;)
tome_dofile("xml.lua")

tome_dofile("util.lua")
tome_dofile("player.lua")
tome_dofile("objects.lua")
tome_dofile("monsters.lua")
tome_dofile("powers.lua")
tome_dofile("building.lua")
tome_dofile("dungeon.lua")
tome_dofile("s_aux.lua")
tome_dofile("crpt_aux.lua")
tome_dofile("quests.lua")

-- Load the ingame contextual help
tome_dofile("help.lua")

-- let the store specific stuff happen!
tome_dofile("stores.lua")

-- Add the corruptions
tome_dofile("corrupt.lua")

-- Add the schools of magic
tome_dofile("spells.lua")


-- Add some quests
tome_dofile("bounty.lua")
tome_dofile("god.lua")
tome_dofile("fireprof.lua")

--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
-----Include here all your files( tome_dofile("foo.lua") )----
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
tome_dofile("intro.lua")

-- This is a test file, if it is not present, it is very well
tome_dofile("test.lua")

--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--
-- Do not thouch after this line
--
tome_dofile("quests2.lua")
