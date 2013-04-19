--
-- This file is loaded at the initialisation of ToME
--

-- Load the class specific stuff
tome_dofile("player.lua")

-- Load the ingame contextual help
tome_dofile("help.lua")

-- let the store specific stuff happen!
tome_dofile("stores.lua")

-- Add various 'U' powers
tome_dofile("powers.lua")

-- Add the mimic shapes
tome_dofile("mimic.lua")
tome_dofile("riding.lua")

-- Add the corruptions
tome_dofile("corrupt.lua")

-- Add the mkey activations
tome_dofile("mkeys.lua")

-- Add god stuff
tome_dofile("gods.lua")
tome_dofile("gods_new.lua")

-- Add the schools of magic
tome_dofile("spells.lua")

-- Add some quests
tome_dofile("bounty.lua")
tome_dofile("god.lua")
tome_dofile("fireprof.lua")
tome_dofile("library.lua")
tome_dofile("weapon_skills.lua")
tome_dofile("berserk.lua")
tome_dofile("arcan.lua")
tome_dofile("building.lua")
-- Add joke stuff
tome_dofile("drunk.lua")
tome_dofile("joke.lua")
tome_dofile("yelling.lua")
tome_dofile("newskills.lua")

-- Some tests, if the file is not present, this is fine
tome_dofile_anywhere(ANGBAND_DIR_SCPT, "dg_test.lua", FALSE)

-- A nice custom intro :)
tome_dofile("intro.lua")

-- Add monster interaction
tome_dofile("monsters.lua")

-- Add miscellaneous stuff
tome_dofile("misc.lua")
-- tome_dofile("c.lua")
tome_dofile("runecrft.lua")
tome_dofile("aws_road.lua")