--
-- This file is loaded ad the initialisation of ToME
-- Load the system functions
--

-- Name of globals to save
__loadsave_name = {}
__loadsave_max = 0
__loadsave_tmp = 0

pern_dofile("player.lua")
pern_dofile("objects.lua")
pern_dofile("powers.lua")

-- Add the quest helper functions
pern_dofile("quests.lua")



--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
-----Include here all your files( pern_dofile("foo.lua") )----
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
pern_dofile("intro.lua")
pern_dofile("lebohaum.lua")

-- This is a test file, if it is not present, it is very well
pern_dofile("test.lua")


--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--
-- Do not thouch after this line
--
pern_dofile("quests2.lua")
