-- File: magic.lua
-- This file contains various scripts and codes related to spells.
-- This handles various codes, for both the player and the monsters.

-- Damages done by player spells.
-- The "stat" parameter is which stat we're going to use. It varies
-- from 0 to 5, and the code uses stat+1, arrays starting from 1 instead
-- of 0 in the lua code.
-- This formula is essentially the same as the melee and ranged
-- formulas, though the power isn't random.
-- The "school" parameter is used to reference the spell school used.
-- Doesn't do anything at the moment, but the parameters are:
-- -1. Uses the "stat" variable as the stat multiplier.
-- 0. No school
-- 1. Elemental
-- 2. Alteration
-- 3. Mysticism
-- 4. Conjuration
-- 5. Divination
function spell_damages(power, stat, school)

        local k = 0

	-- Base power.
	-- k = lua_randint((power / 2)) + (power / 2)
	k = power

	-- Add Rod/Essence bonus.
	k = k + rod_bonus()

	-- Damages are multiplied by every points of (stat) above 5.
	if (not(school == -1)) then
		if (p_ptr.stat_ind[stat+1] > 5) then

			k = k + (k * (p_ptr.stat_ind[stat+1]-5))
		end
	else
		k = k + (k * stat)
	end
	
        return k
end

-- Bonus from Rods/Main Essence.
function rod_bonus ()

	local bonus = 0

	-- Check for rods.
	if (inven(INVEN_WIELD).tval == TV_ROD) then

		bonus = bonus + damroll(inven(INVEN_WIELD).dd, inven(INVEN_WIELD).ds)
	end
	if (inven(INVEN_WIELD + 1).tval == TV_ROD) then

		bonus = bonus + damroll(inven(INVEN_WIELD + 1).dd, inven(INVEN_WIELD + 1).ds)
	end
	if (not(inven(INVEN_ESSENCE).tval == 0) and (unarmed())) then

		bonus = bonus + damroll(inven(INVEN_ESSENCE).dd, inven(INVEN_ESSENCE).ds)
	end

	return bonus
end

add_event_handler("spell_damages", spell_damages)
add_event_handler("rod_bonus", rod_bonus)
