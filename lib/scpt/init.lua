--
-- This file is loaded ad the initialisation of PernAngband
-- Load the system functions
--
pern_dofile("player.lua")
pern_dofile("objects.lua")

--
-- Include here all your files( pern_dofile("foo.lua") )
--
pern_dofile("intro.lua")
pern_dofile("lebohaum.lua")

-- This is a test file, if it is not present, it is very well
pern_dofile("test.lua")
