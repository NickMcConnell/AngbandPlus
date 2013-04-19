constructor_powers = 88
SURVEY_AREA = add_spell
		{
			["name"] = 	"Survey area",
			["desc"] = 	"Level [1] Surveys stairs/doors [15] maps area [28] fully map area",
			["school"] =    {SCHOOL_CONST },
			["mana"] = 	1,
			["level"] = 	1,
			["fail"] = 	35,	
			["random"] =    0,
				["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
			["spell"] = 	function()
		
					if (get_level(constructor_powers, 50) >= 28) then
						-- enlightenment
						wiz_lite()
					elseif (get_level(constructor_powers, 50) >= 15) then
						--magic map and detect traps
						map_area()
						detect_traps(15 + get_level(constructor_powers, 40, 0))
					elseif (get_level(constructor_powers, 50) >= 5) then
						-- detect doors, traps, stairs
						detect_traps(15 + get_level(constructor_powers, 40, 0))
						detect_stairs(DEFAULT_RADIUS)
						detect_doors(DEFAULT_RADIUS)
					else 
						detect_stairs(DEFAULT_RADIUS)
						detect_doors(DEFAULT_RADIUS)
					end
			end,
			["info"] = 	function()
                                return " "
                        end,
		}
DISMANTLE = add_spell
		{
			["name"] = 	"Dismantle",
			["desc"] = 	"Dismantles adjacent traps, at lvl 11 it becomes a beam",
			["school"] =    {SCHOOL_CONST },
			["mana"] = 	4,
			["level"] = 	3,
				["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
			["fail"] = 		10,
				["random"] =    0,
			["spell"] =	function()
					local ret, dir, dam
	
					if (get_level(constructor_powers, 50) >= 11) then

						-- Get the direction
						ret, dir = get_aim_dir();
	
						-- Got direction ok?
						if (ret == FALSE) then return end

						-- fire beam of disarming (like a wand of trap/door destruction
						fire_beam(GF_KILL_TRAP, dir, 1)
					else
						-- player-centered radius 1 ball of disarm trap. (Works like a spell of trap/door destruction)
						fire_ball(GF_KILL_TRAP, 0, 1, 1)
					end
			end,
			["info"] =	function()
                                return " "
                        end,
		}
SPARKY_SKILLS = add_spell
		{
			["name"] = 	"Sparky Skills",
			["desc"] =	"Casts electric bolt and grants temp resist electricity. Bolt is a ball at lvl 21",
			["school"] =    {SCHOOL_CONST },
			["mana"] = 	8,
			["level"] =	7,
				["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
				["random"] =    0,
			["fail"] =	20,
			["spell"] =	function()
					local ret, dir, dam
	
					-- Get the direction
					ret, dir = get_aim_dir();
	
					-- Got direction ok?
					if (ret == FALSE) then 
						return 
					end

					-- calculate damage  as (skill level * 2) +d20
					dam = get_level(constructor_powers, 50)*2 + randint(20)

					-- Fire the bolt/ball.
					if (get_level(constructor_powers, 50) >= 21) then
						fire_ball(GF_ELEC, dir, dam, 2)
					else
						fire_bolt(GF_ELEC, dir, dam)
					end
					
					-- grant temp electric resist for 20+(skill level * 3) + d10
					if player.oppose_elec == 0 then set_oppose_elec(randint(10) + 20 + get_level(constructor_powers, 20)*3) end

			end,
			["info"] =	function()
                                return " dam "..(get_level(constructor_powers, 50)*2).."+d20  dur "..(20 + get_level(constructor_powers, 20)*3).."+1d10"
                        end,
		}
BUILD_DOOR = add_spell
		{
			["name"] = 	"Build door",
			["desc"] = 	"Builds a single door where you stand",
			["school"] =    {SCHOOL_CONST },
			["mana"] = 	10,
			["level"] = 	9,
				["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
				["random"] =    0,
			["fail"] = 	35,
			["spell"] = 	function()
					-- project a door under the player
					project(0, 0, player.py, player.px, 0, GF_MAKE_DOOR, PROJECT_GRID + PROJECT_ITEM)
					msg_print("You build a door.")
			end,
			["info"] = 	function()
                                return " "
                        end,
		}
KNOCK_DOWN_WALL = add_spell
		{
			["name"] = 	"Knock down wall",
			["desc"] = 	"Knocks down a single section of wall, at lvl 34 excavates a corridor/chamber",
			["school"] =    {SCHOOL_CONST },
			["mana"] = 	11,
			["level"] = 	13,
				["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
				["random"] =    0,
			["fail"] = 	35,
			["spell"] = 	function()
					local ret, ret2, dir, which
					-- Get the direction
					ret, dir = get_aim_dir();
	
					-- Got direction ok?
					if (ret == FALSE) then return end					

					if (get_level(constructor_powers, 50) >= 34) then
						-- ask for input
						ret2, which = get_com("[D]ig corridor or [E]xcavate chamber?", 2)

						-- did user press ESC?
						if (ret2 == FALSE) then return end
						-- which did they choose?
						if (which == strbyte('D')) or (which == strbyte('d')) then
							-- fire a beam
							project_hook(GF_KILL_WALL, dir, 1, PROJECT_BEAM + PROJECT_KILL + PROJECT_GRID + PROJECT_WALL)
							return
						end 
	
						if (which == strbyte('E')) or (which == strbyte('e')) then 
							fire_ball(GF_KILL_WALL, dir, 1, 3)
							return
						end

					else
						wall_to_mud(dir)
					end
			end,
			["info"] = 	function()
                                return " "
                        end,
		}
PLUMBERS_MATE = add_spell
		{
			["name"] = 	"Plumbers Mate",
			["desc"] = 	"Grants temporary resists poison and casts poison bolt, or a ball at lvl 25",
			["school"] =    {SCHOOL_CONST },
			["mana"] = 	15,
				["random"] =    0,
			["level"] = 	17,
				["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
			["fail"] = 	35,
			["spell"] = 	function()
					local ret, dir, dam
	
					-- Get the direction
					ret, dir = get_aim_dir();
	
					-- Got direction ok?
					if (ret == FALSE) then 
						return 
					end

					dam = get_level(constructor_powers, 50)*2 + 30

					-- Fire the bolt/ball.
					if (get_level(constructor_powers, 50) >= 25) then
						fire_ball(GF_POIS, dir, dam, 3)
					else
						fire_bolt(GF_POIS, dir, dam)
					end

					if player.oppose_pois == 0 then 
						set_oppose_pois(randint(10) + 30 + get_level(constructor_powers, 20)*3) 
					end

			end,
			["info"] = 	function()
                                return " dam "..(get_level(constructor_powers, 50)*2 + 30).."    dur "..(30 + get_level(constructor_powers, 20)*3).."+1d10"
                        end,
		}
BUILD_WALL = add_spell
		{
			["name"] = 	"Build walls",
			["desc"] = 	"Builds walls. Single section first, then either a long wall or fills in a hole",
			["school"] =    {SCHOOL_CONST },
			["mana"] = 	17,
			["random"] =    0,
			["level"] = 	19,
				["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
			["fail"] = 	35,
			["spell"] = 	function()
					local which, ret, dir, x, y


					if (get_level(constructor_powers, 50) >= 42) then
						ret2, which = get_com("[B]uild straight wall or [F]ill hole?", 2)
						-- corridor?
						if (ret2 == FALSE) then return end

						-- Get the direction
						ret, dir = get_aim_dir();
	
						-- Got direction ok?
						if (ret == FALSE) then return end

						if (which == strbyte('B')) or (which == strbyte('b')) then
							-- Fire the bolt/ball.
							project_hook(GF_STONE_WALL, dir, 1, PROJECT_BEAM + PROJECT_KILL + PROJECT_GRID)
							return
						end 
	
						if (which == strbyte('F')) or (which == strbyte('f')) then 
							fire_ball(GF_STONE_WALL, dir, 1, 3)
							return
						end
					else
						project(0, 0, player.py, player.px, 0, GF_STONE_WALL, PROJECT_GRID + PROJECT_ITEM)
						msg_print("You build a section of wall.")
					end
			end,
			["info"] = 	function()
                                return " "
                        end,
		}
BUILD_STAIR = add_spell
		{
			["name"] = 	"Build stairs",
			["desc"] = 	"Builds stairs. But only in a proper dungeon",
			["school"] =    {SCHOOL_CONST },
			["mana"] = 	26,
			["level"] = 	23,
				["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
				["random"] =    0,
			["fail"] = 	35,
			["spell"] = 	function()
					stair_creation();
			end,
			["info"] = 	function()
                                return " "
                        end,
		}
NAIL_GUNS = add_spell
		{
			["name"] = 	"Nail guns",
			["desc"] = 	"Fires a shard bolt, a powerful ball at lvl 40",
			["school"] =    {SCHOOL_CONST },
			["mana"] = 	10,
			["level"] = 	31,
				["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
				["random"] =    0,
			["fail"] = 	35,
			["spell"] = 	function()
					local ret, dir, dam, repeats, y, x
	
					dam = 20 + get_level(constructor_powers, 50)*3
					repeats = get_level(constructor_powers)/2

					if (get_level(constructor_powers, 50) >= 40) then

						-- fire multiple nails at random grids
						while (repeats > 0) do

							-- initialise tries variable
							tries = 0
							
							while (tries == 0) do

								-- get grid coordinates near(ish) player
								x = px - 5 + randint(10)
								y = py - 5 + randint(10)
								grid = cave(y, x)

								-- are the coordinates in a wall, or the player?
								if (cave_is(grid, FF1_WALL) ~= 0) or ((x == px) and (y == py)) then

									-- try again
									tries = 0
								else
									--neither player, nor wall, then stop this 'while'
									tries = 1
								end
							end

							-- fire a nail
							project(0, 0, y, x, dam, GF_SHARDS, PROJECT_JUMP + PROJECT_GRID + PROJECT_ITEM + PROJECT_KILL + PROJECT_THRU)

							-- one less repeat
							repeats = repeats - 1
						end
					else
						-- Get the direction
						ret, dir = get_aim_dir();
	
						-- Got direction ok?
						if (ret == FALSE) then return end

						fire_bolt(GF_SHARDS, dir, dam)
						return
					end
			end,
			["info"] = 	function()
                                return " dam "..(20 + get_level(constructor_powers, 50)*3).." "
                        end,
		}
DEMOLITION = add_spell
		{
			["name"] = 	"Demolition",
			["desc"] =	"Destroys entire parts of the dungeon", 
			["school"] =    {SCHOOL_CONST },
			["mana"] = 	35,
			["level"] = 	37,
				["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
				["random"] =    0,
			["fail"] = 	35,
			["spell"] = 	function()
					msg_print("You call in the demolition men!")
					fire_ball(GF_DISINTEGRATE, 0, 1, 40)
			end,
			["info"] = 	function()
                                return " "
                        end,
		}
REBUILD_DUNGEON = add_spell
		{
			["name"] = 	"Rebuild entire dungeon",
			["desc"] = 	"Reconstructs whole dungeon level",
			["school"] =    {SCHOOL_CONST },
			["mana"] = 	40,
			["level"] = 	45,
			["fail"] = 	40,
				["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
				["random"] =    0,
			["spell"] = 	function()
					alter_reality();
			end,
			["info"] = 	function()
                                return " "
                        end,
		}
	

FIND_HIDDEN = add_spell
{
	["name"] = 	"Find Traps",
	["school"] = 	{SCHOOL_CONST},
	["level"] = 	5,
	["mana"] = 	2,
	["mana_max"] = 	10,
	["fail"] = 	25,
	["random"] =    0,
		["stick"] =
	{
			["charge"] =    { 5, 7 },
			[TV_WAND] =
			{
				["rarity"] = 		15,
				["base_level"] =	{ 1, 15 },
				["max_level"] =		{ 25, 50 },
			},
	},
	["inertia"] = 	{ 1, 10 },
	["spell"] = 	function()
			local obvious = nil
			obvious = detect_traps(15 + get_level(FIND_HIDDEN, 40, 0))
			if get_level(FIND_HIDDEN, 50) >= 15 then
				obvious = is_obvious(set_tim_invis(10 + randint(20) + get_level(FIND_HIDDEN, 40)), obvious)
			end
			return obvious
	end,
	["info"] = 	function()
			if get_level(FIND_HIDDEN, 50) >= 15 then
				return "rad "..(15 + get_level(FIND_HIDDEN, 40)).." dur "..(10 + get_level(FIND_HIDDEN, 40)).."+d20"
			else
				return "rad "..(15 + get_level(FIND_HIDDEN, 40))
			end
	end,
	["desc"] =	{
			"Detects the traps in a certain radius around you",
			"At level 15 it allows you to sense invisible for a while"
	}
}



