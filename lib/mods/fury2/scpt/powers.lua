-- Various 'U' powers

-- Invisibility power, for the mouse mimic shape
POWER_INVISIBILITY = add_power
{
	["name"] =      "invisibility",
	["desc"] =      "You are able melt into the shadows to become invisible.",
	["desc_get"] =  "You suddently become able to melt in the shadows.",
	["desc_lose"] = "You loose your shadow melting ability.",
	["level"] =     30,
	["cost"] =      10,
	["stat"] =      A_DEX,
	["fail"] =      20,
	["power"] =     function()
			set_invis(20 + randint(30), 30)
	end,
}

-- Web power, for the mouse mimic shape
POWER_WEB = add_power
{
	["name"] =      "web",
	["desc"] =      "You are able throw a thick and very resistant spider web.",
	["desc_get"] =  "You suddently become able to weave webs.",
	["desc_lose"] = "You lose your web weaving capability.",
	["level"] =     1,
	["cost"] =      10,
	["stat"] =      A_DEX,
	["fail"] =      6,
	["power"] =     function()
			-- Warning, beware of f_info changes .. I hate to do that ..
			grow_things(16, 1 + (player.lev / 10))
	end,
}

-- Activating/stopping space-continuum
-- When stopped it will induce constant mana loss
player.corrupt_anti_teleport_stopped = FALSE
add_loadsave("player.corrupt_anti_teleport_stopped", FALSE)
POWER_COR_SPACE_TIME = add_power
{
	["name"] =      "control space/time continuum",
	["desc"] =      "You are able to control the space/time continuum.",
	["desc_get"] =  "You become able to control the space/time continuum.",
	["desc_lose"] = "You are no more able to control the space/time continuum.",
	["level"] =     1,
	["cost"] =      10,
	["stat"] =      A_WIS,
	["fail"] =      10,
	["power"] =     function()
		if player.corrupt_anti_teleport_stopped == TRUE then
			player.corrupt_anti_teleport_stopped = FALSE
			msg_print("You stop controlling your corruption.")
			player.update = bor(player.update, PU_BONUS)
		else
			player.corrupt_anti_teleport_stopped = TRUE
			msg_print("You start controlling your corruption, teleportation works once more.")
			player.update = bor(player.update, PU_BONUS)
		end
	end,
}

FIRE_BURST = add_power
{
        ["name"] =      "burst of fire",
        ["desc"] =      "You are able to create a burst of fire.",
        ["desc_get"] =  "You now feel able to create a burst of fire.",
        ["desc_lose"] = "You can no longer create a burst of fire.",
        ["level"] =     3,
        ["cost"] =      15,
        ["stat"] =      A_STR,
        ["fail"] =      14,
	  ["power"] =     function()
			local ret, dir, damage
			ret, dir = get_aim_dir();
			if (ret == FALSE) then 
				return 
			end
			damage = (player.lev*3) + 2
			msg_print("You create a burst of fire.")
			fire_bolt(GF_FIRE, dir, damage)	
			end
}


EVERNIGHT = add_power
{
	["name"] =	"Domain of Evernight",
	["desc"] =	"Turn day into twilight (darkness will fall within a few turns)",
	["desc_get"] =	"You feel you can darken the sunniest day",
	["desc_lose"] =	"You lose your power over day and night",
	["level"] =	30,
	["cost"] =	45,
	["stat"] =	A_INT,
	["fail"] =	15,
	["power"] = 	function()
			local hour = bst(480, turn)
			if (hour < 18) and (hour >= 6) then
				while bst(480, turn) >= 6 and bst(480,turn) < 18 do
					turn = turn + (10 * 40)
				end
					turn = turn - 50
				return TRUE
			else
				msg_print("It is already night!")
				return FALSE
			end
	end,
}

SHARD_BOLT = add_power
{
        ["name"] =      "shard bolt",
        ["desc"] =      "You are able to create a bolt of stone shards.",
        ["desc_get"] =  "You now feel able to create a bolt of stone shards.",
        ["desc_lose"] = "You can no longer create a bolt of stone shards.",
        ["level"] =     3,
        ["cost"] =      15,
        ["stat"] =      A_STR,
        ["fail"] =      14,
	  ["power"] =     function()
			local ret, dir, damage
			ret, dir = get_aim_dir();
			if (ret == FALSE) then 
				return 
			end
			damage = player.lev*3
			msg_print("You create a bolt of shards.")
			fire_bolt(GF_SHARDS, dir, damage)
			end
}

GIANT_THUNDERBOLT = add_power
{
        ["name"] =      "thunderbolt",
        ["desc"] =      "You are able to create a powerful thunderbolt.",
        ["desc_get"] =  "You now feel able to create a powerful thunderbolt.",
        ["desc_lose"] = "You can no longer craete a powerful thunderbolt.",
        ["level"] =     15,
        ["cost"] =      30,
        ["stat"] =      A_STR,
        ["fail"] =      14,
	  ["power"] =     function()
			local ret, dir, damage
			ret, dir = get_aim_dir();
			if (ret == FALSE) then 
				return 
			end
			damage = (player.lev*10)+2
			msg_print("You shape a powerful thunderbolt and throw it.")
			fire_bolt(GF_ELEC, dir, damage)
			end
}


WATER_SPLASH = add_power
{
        ["name"] =      "water jet",
        ["desc"] =      "You are able to create a high-pressure splash of water.",
        ["desc_get"] =  "You now feel able to create a high-pressure splash of water.",
        ["desc_lose"] = "You can no longer craete a high-pressure splash of water.",
        ["level"] =     3,
        ["cost"] =      15,
        ["stat"] =      A_STR,
        ["fail"] =      14,
	  ["power"] =     function()
			local ret, dir, damage
			ret, dir = get_aim_dir();
			if (ret == FALSE) then 
				return 
			end
			damage = (player.lev*3)+2
			msg_print("You create a powerful splash of water.")
			fire_ball(GF_WATER, dir, damage)	
			end
}


PROTECTION_GOOD = add_power
{
        ["name"] =      "Protection from Good",
        ["desc"] =      "You are able to protect yourself from good creatures",
        ["desc_get"] =  "You feel able to protect yourself from good creatures",
        ["desc_lose"] = "You no longer feel able to protect yourself from good creatures",
        ["level"] =     1,
        ["cost"] =      30,
        ["stat"] =      A_WIS,
        ["fail"] =      16,
        ["power"] =     function()
				if player.protgood == 0 then
					set_protgood(player.lev * 2)
					end
			  	end,
}

TRUE_BERSERK  = add_power
{
        ["name"] =      "True berserk",
        ["desc"] =      "You are able to go in a true berserk rage",
        ["desc_get"] =  "You are now able to go in a true berserk rage",
        ["desc_lose"] = "You no longer feel able to go in a true berserk rage",
        ["level"] =     1,
        ["cost"] =      3,
        ["stat"] =      A_STR,
        ["fail"] =      16,
        ["power"] =     function()
				if player.hero == 0 then
					set_hero(player.lev * 2)
					end
				if player.shero == 0 then
					set_shero(player.lev * 2)
					end
				if player.strike == 0 then
					set_strike(player.lev * 2)
					end
				end
}

TURN_BAT = add_power
{
        ["name"] =      "turn into a bat",
        ["desc"] =      "You are able to turn into a bat.",
        ["desc_get"] =  "You feel able to turn into a bat.",
        ["desc_lose"] = "You no longer feel able to turn into a bat.",
        ["level"] =     1,
        ["cost"] =      25,
        ["stat"] =      A_INT,
        ["fail"] =      10,
        ["power"] =     function()
				set_mimic(get_skill(SKILL_VAMPIRISM)* 5,resolve_mimic_name("Bat"),get_skill(SKILL_VAMPIRISM))
				end
}

-- Vampires no longer get this
TURN_WOLF = add_power
{
        ["name"] =      "turn into a wolf",
        ["desc"] =      "You are able to turn into a wolf.",
        ["desc_get"] =  "You feel able to turn into a wolf.",
        ["desc_lose"] = "You no longer feel able to turn into a wolf.",
        ["level"] =     1,
        ["cost"] =      25,
        ["stat"] =      A_INT,
        ["fail"] =      10,
        ["power"] =     function()
				set_mimic(get_skill(SKILL_VAMPIRISM) * 5,resolve_mimic_name("Dire Wolf"),get_skill(SKILL_VAMPIRISM))
				end

}

SPIT_POISON = add_power
{
        ["name"] =      "Poison spit",
        ["desc"] =      "You are able to spit a deadly poison.",
        ["desc_get"] =  "You now feel able to spit a deadly poison.",
        ["desc_lose"] = "You are no longer able to spit a deadly poison.",
        ["level"] =     3,
        ["cost"] =      0,
        ["stat"] =      A_CON,
        ["fail"] =      14,
	  ["power"] =     function()
				if player.food >= 2000 then
					local ret, dir, damage
					ret, dir = get_aim_dir();
					if (ret == FALSE) then 
						return 
					end
					damage = player.lev*3
					msg_print("You spit poison.")
					fire_bolt(GF_POIS, dir, damage)
					set_food(player.food - 1000)	
				else
				msg_print("Your organism needs FOOD to produce POISON.")
				end
end,
}


BREATH_FIRE = add_power
{
        ["name"] =      "Breath of fire",
        ["desc"] =      "You can breathe fire.",
        ["desc_get"] =  "Your feel like you can breathe fire.",
        ["desc_lose"] = "You no longer feel able to breathe fire.",
        ["level"] =     10,
        ["cost"] =      30,
        ["stat"] =      A_STR,
        ["fail"] =      14,
        ["power"] =     function()

			local ret, dir, damage
	
			ret, dir = get_aim_dir();
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			dam = rand_range(2,20) + (player.lev * 2)
			if get_race_name() == "Dragon" then
				dam = dam * 25 / 10
			end


			msg_print("You breathe fire.")
	
			fire_ball(GF_FIRE, dir, dam, 2)
			set_food(player.food - 500)
        end,
}

BREATH_THUNDER = add_power
{
        ["name"] =      "Breath of thunder",
        ["desc"] =      "You can breathe thunder.",
        ["desc_get"] =  "Your feel like you can breathe thunder.",
        ["desc_lose"] = "You no longer feel able to breathe thunder.",
        ["level"] =     10,
        ["cost"] =      30,
        ["stat"] =      A_STR,
        ["fail"] =      14,
        ["power"] =     function()

			local ret, dir, damage
	
			ret, dir = get_aim_dir();
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			dam = rand_range(2,20) + (player.lev * 2)
			if get_race_name() == "Dragon" then
				dam = dam * 25 / 10
			end
	
			msg_print("You breathe thunder.")
	
			fire_ball(GF_ELEC, dir, dam, 2)
			set_food(player.food - 500)
        end,
}

BREATH_COLD = add_power
{
        ["name"] =      "Breath of cold",
        ["desc"] =      "You can breathe cold.",
        ["desc_get"] =  "Your feel like you can breathe cold.",
        ["desc_lose"] = "You no longer feel able to breathe cold.",
        ["level"] =     10,
        ["cost"] =      30,
        ["stat"] =      A_STR,
        ["fail"] =      14,
        ["power"] =     function()

			local ret, dir, damage
	
			ret, dir = get_aim_dir();
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			dam = rand_range(2,20) + (player.lev * 2)
			if get_race_name() == "Dragon" then
				dam = dam * 25 / 10
			end
		
			msg_print("You breathe cold.")
	
			fire_ball(GF_COLD, dir, dam, 2)
			set_food(player.food - 500)
        end,
}

BREATH_ACID = add_power
{
        ["name"] =      "Breath of acid",
        ["desc"] =      "You can breathe acid.",
        ["desc_get"] =  "Your feel like you can breathe acid.",
        ["desc_lose"] = "You no longer feel able to breathe acid.",
        ["level"] =     10,
        ["cost"] =      30,
        ["stat"] =      A_STR,
        ["fail"] =      14,
        ["power"] =     function()

			local ret, dir, damage
	
			ret, dir = get_aim_dir();
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			dam = rand_range(2,20) + (player.lev * 2)
			if get_race_name() == "Dragon" then
				dam = dam * 25 / 10
			end
	
			msg_print("You breathe acid.")
	
			fire_ball(GF_ACID, dir, dam, 2)
			
        end,
}

SPIT_ACID = add_power
{
        ["name"] =      "Acid spit",
        ["desc"] =      "You are able to spit acid.",
        ["desc_get"] =  "You now feel able to acid.",
        ["desc_lose"] = "You are no longer able to spit acid.",
        ["level"] =     10,
        ["cost"] =      0,
        ["stat"] =      A_CON,
        ["fail"] =      16,
	  ["power"] =     function()
				if player.food >= 4500 then
					local ret, dir, damage
					ret, dir = get_aim_dir();
					if (ret == FALSE) then 
						return 
					end
					damage = player.lev*4
					msg_print("You spit acid.")
					fire_bolt(GF_ACID, dir, damage)
					set_food(player.food - 2500)	
		
				end
end,
}

BREATH_THUNDER = add_power
{
        ["name"] =      "Lightening Pulse",
        ["desc"] =      "You can shoot pulses of Lightening.",
        ["desc_get"] =  "Your feel like you can shoot pulses of Lightening.",
        ["desc_lose"] = "You no longer feel able to shoot pulses of Lightening.",
        ["level"] =     1,
        ["cost"] =      5,
        ["stat"] =      A_STR,
        ["fail"] =      1,
        ["power"] =     function()

			local ret, dir, damage
	
			ret, dir = get_aim_dir();
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			dam = rand_range(2,20) + (player.lev * 2)
			if get_race_name() == "dragon" then
				dam = dam * 25 / 10
			end
	
			msg_print("You fire Lightening.")
	
			fire_bolt(GF_ELEC, dir, dam)
			
        end,
}

BEAM_LIGHTENING_FURY = add_power
{
        ["name"] =      "Lightening Beam",
        ["desc"] =      "You can shoot a beam of Lightening.",
        ["desc_get"] =  "Your feel like you can shoot a beam of Lightening.",
        ["desc_lose"] = "You no longer feel able to shoot a beam of Lightening.",
        ["level"] =     1,
        ["cost"] =      5,
        ["stat"] =      A_STR,
        ["fail"] =      1,
        ["power"] =     function()

			local ret, dir, damage
	
			ret, dir = get_aim_dir();
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			dam = rand_range(2,20) + (player.lev * 2)
			if get_race_name() == "dragon" then
				dam = dam * 25 / 10
			end
	
			msg_print("You fire Lightening.")
	
			fire_beam(GF_ELEC, dir, dam)
			
        end,
}

EAGLE_POWER = add_power
{
        ["name"] =      "throw",
        ["desc"] =      "You can pick up enemies and throw them",
        ["desc_get"] =  "you can pick enemies up anymore",
        ["desc_lose"] = "you can't throw enemies anymore",
        ["level"] =     1,
        ["cost"] =      5,
        ["stat"] =      A_STR,
        ["fail"] =      10,
        ["power"] =     function()

			local ret, dir, weight
	
			--get direction
			ret, dir = get_aim_dir();
	
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			
			local target = monster(target_who)
			
			-- Get the monsters weight
			weight = race_info_idx(target.r_idx, 0).weight
			
			m_ptr = monster(target_who)
			
			-- Work out whether we can actually throw it
			if ((((player.lev* 2000) * player.stat_cur[A_STR + 1]) / 10) < weight) then
			--if (player.lev * 2000 < weight) then
				-- Can't throw it
				msg_print("It is too heavy to throw")
				return
			else
				-- Check to see whether we are next to it
				--if (distance(m_ptr.fy, m_ptr.fx, player.py, player.px) <= 1) then
				if (m_ptr.fy - player.py <=1) and (m_ptr.fx  - player.px <=1) then
					-- Do damage equal to its hp
					return fire_bolt(GF_ARROW, dir, m_ptr.hp)
				else
					msg_print("Too far away")
				end
			end
			
        end,
} 


POWER_ETHER_FORM = add_power
{
	["name"] =      "ethereal form",
	["desc"] =      "You can walk through walls.",
	["desc_get"] =  "You can walk through walls.",
	["desc_lose"] = "You are no longer able to walk through walls.",
	["level"] =     20,
	["cost"] =      50,
	["stat"] =      A_CON,
	["fail"] =      85,
	["power"] =     function()
				return set_shadow(randint(30) + 45)			
	end,
}


BREATH_WATER = add_power
{
        ["name"] =      "Breath water",
        ["desc"] =      "You can breathe water.",
        ["desc_get"] =  "Your feel like you can breathe water.",
        ["desc_lose"] = "You no longer feel able to breathe water.",
        ["level"] =     10,
        ["cost"] =      30,
        ["stat"] =      A_STR,
        ["fail"] =      14,
        ["power"] =     function()

			local ret, dir, damage
	
			ret, dir = get_aim_dir();
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			dam = rand_range(2,20) + (player.lev * 2)
			if get_race_name() == "Dragon" then
				dam = dam * 25 / 10
			end


			msg_print("You breathe water.")
	
			fire_ball(GF_WATER, dir, dam, 2)
			
        end,
}

BREATH_SHARDS = add_power
{
        ["name"] =      "Breath shards",
        ["desc"] =      "You can breathe shards.",
        ["desc_get"] =  "Your feel like you can breathe shards.",
        ["desc_lose"] = "You no longer feel able to breathe shards.",
        ["level"] =     10,
        ["cost"] =      30,
        ["stat"] =      A_STR,
        ["fail"] =      14,
        ["power"] =     function()

			local ret, dir, damage
	
			ret, dir = get_aim_dir();
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			dam = rand_range(2,20) + (player.lev * 2)
			if get_race_name() == "Dragon" then
				dam = dam * 25 / 10
			end


			msg_print("You breathe shards.")
	
			fire_ball(GF_SHARDS, dir, dam, 2)
			
        end,
}

BREATH_POISON = add_power
{
        ["name"] =      "Breath Poison",
        ["desc"] =      "You can breathe Poison.",
        ["desc_get"] =  "Your feel like you can breathe Poison.",
        ["desc_lose"] = "You no longer feel able to breathe Poison.",
        ["level"] =     10,
        ["cost"] =      30,
        ["stat"] =      A_STR,
        ["fail"] =      14,
        ["power"] =     function()

			local ret, dir, damage
	
			ret, dir = get_aim_dir();
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			dam = rand_range(2,20) + (player.lev * 2)
			if get_race_name() == "Dragon" then
				dam = dam * 25 / 10
			end


			msg_print("You breathe Poison.")
	
			fire_cloud(GF_POIS, dir, 7 + dam, 3, 5 + dam)
			
        end,
}

BREATH_LIGHTENING2 = add_power
{
        ["name"] =      "Breath Lightening",
        ["desc"] =      "You can breathe Lightening",
        ["desc_get"] =  "Your feel like you can breathe Lightening.",
        ["desc_lose"] = "You no longer feel able to breathe Lightening.",
        ["level"] =     10,
        ["cost"] =      30,
        ["stat"] =      A_STR,
        ["fail"] =      14,
        ["power"] =     function()

			local ret, dir, damage
	
			ret, dir = get_aim_dir();
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			dam = rand_range(2,20) + (player.lev * 2)
			if get_race_name() == "Dragon" then
				dam = dam * 25 / 10
			end


			msg_print("You breathe Poison.")
	
			fire_ball(GF_ELEC, dir, dam, 2)
			
        end,
}
POWER_INVISIBILITY = add_power
{
	["name"] =      "Feed Self",
	["desc"] =      "You are able to feed yourself",
	["desc_get"] =  "You suddently become able to feed yourself.",
	["desc_lose"] = "You loose your feeding ability.",
	["level"] =     1,
	["cost"] =      1,
	["stat"] =      A_STR,
	["fail"] =      1,
	["power"] =     function()
			set_food(PY_FOOD_MAX - 1)
	end,
}

POWER_SWOOP = add_power
{
        ["name"] =      "Swoop",
        ["desc"] =      "You can pick up enemies and throw them.",
        ["desc_get"] =  "You feel your talons grow stronger.",
        ["desc_lose"] = "Your talons grow weak again.",
        ["level"] =     1,
        ["cost"] =      5,
        ["stat"] =      A_STR,
        ["fail"] =      10,
        ["power"] =     function()

			local ret, dir, weight
	
			--get direction
			ret, dir = get_aim_dir()
	
			-- direction selected ok? 
			if (ret == FALSE) then 
				return 
			end
			
			local target = monster(target_who)
			
			-- Get the monster's weight
			weight = race_info_idx(target.r_idx, 0).weight
			
			m_ptr = monster(target_who)
			
			-- Work out whether we can actually throw it
			if ((((player.lev* 2000) * player.stat_cur[A_STR + 1]) / 10) < weight) then
				-- Can't throw it
				msg_print("That monster is too heavy to throw.")
				return
			else
				-- Check to see whether we are next to it
				if (m_ptr.fy - player.py <=1) and (m_ptr.fx  - player.px <=1) then
					-- Do damage equal to its hp
					return fire_bolt(GF_ARROW, dir, m_ptr.hp)
				else
					msg_print("That creature is too far away.")
				end
			end
			
        end,
} 


-- This gives the Dragon a little something to compensate for the lack of armour...
POWER_ARMOURY = add_power
{
	["name"] = "Golden Armour",
	["desc"] = "You can use jewels collected in the dungeons to add armour to your hide.",
	["desc_get"] = "You feel all those jewels might be worth something...",
	["desc_lose"] = "You feel less of an armourer.",
	["level"] = 1,
	["cost"] = 0,
	["stat"] = A_INT,
	["fail"] = 10,
	["power"] = function() 

		local amount = get_quantity("Specify the amount you wish to use: ", 1000000000)
		local bonus
		if (amount > player.au) then
			msg_print("You do not have enough gold.")
		else
			armour_pwr_bonus = (amount/(player.lev*player.lev*player.lev)*armour_pwr_used)
		end
		if (armour_pwr_bonus > player.lev) then 
			armour_pwr_bonus = player.lev
		end
		if (armour_pwr_bonus == 0) then
			msg_print("You do not feel an amount that small will help much.")
		else
			armour_pwr_do = 1
			armour_pwr_cu_bonus = armour_pwr_cu_bonus + armour_pwr_bonus
			msg_print("You carefully arrange the jewels over your hide.")
--			msg_print(format("used = %s, bonus = %s", armour_pwr_used, armour_pwr_bonus))
			player.to_a = player.to_a + armour_pwr_bonus
			player.dis_to_a = player.dis_to_a + armour_pwr_bonus
			player.au = player.au - amount
			player.redraw = bor(player.redraw, PR_GOLD, PR_ARMOR)
			armour_pwr_used = armour_pwr_used + 1
		end
	end,
}

add_loadsave("armour_pwr_used", 1)
add_loadsave("armour_pwr_do", 0)
add_loadsave("armour_pwr_cu_bonus", 0)

-- Helper function, uses hook_calc_bonus to apply the bonus, otherwise it would be temporary, not permanent.
function armour_pwr()
	if (armour_pwr_do == 1) and (get_race_name() == "Dragon") then
		player.to_a = player.to_a + armour_pwr_cu_bonus
		player.dis_to_a = player.dis_to_a + armour_pwr_cu_bonus
	end
end
add_hook_script(HOOK_CALC_BONUS, "armour_pwr", "armour_pwr")











