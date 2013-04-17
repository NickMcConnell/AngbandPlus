--
-- This file is loaded ad the initialisation of Conglomoband
-- Load the system functions
--
conglomo_dofile("player.lua")
conglomo_dofile("objects.lua")

--
-- Include here all your files( conglomo_dofile("foo.lua") )
--
conglomo_dofile("intro.lua")
conglomo_dofile("lebohaum.lua")

-- This is a test file, if it is not present, it is very well
conglomo_dofile("test.lua")
