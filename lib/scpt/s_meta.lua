-- handle the meta school

RECHARGE = add_spell
{
	["name"] = 	"Recharge",
        ["school"] = 	{SCHOOL_META},
        ["level"] = 	5,
        ["mana"] = 	10,
        ["mana_max"] = 	100,
        ["fail"] = 	10,
        ["spell"] = 	function()
        		recharge(60 + get_level(RECHARGE, 140))
	end,
	["info"] = 	function()
                	return "power "..(60 + get_level(RECHARGE, 140))
	end,
        ["desc"] =	{
        		"Taps on the ambient mana to recharge an object's power (charges or mana)",
        }
}

function get_spellbinder_max()
	local i

	i = get_level(SPELLBINDER, 4)
        if i > 4 then i = 4 end
	return i
end

SPELLBINDER = add_spell
{
	["name"] = 	"Spellbinder",
        ["school"] = 	{SCHOOL_META},
        ["level"] = 	20,
        ["mana"] = 	100,
        ["mana_max"] = 	300,
        ["fail"] = 	30,
        ["spell"] = 	function()
        		local i, ret, c

        		if player.spellbinder_num ~= 0 then
                        	msg_print("The spellbinder is already active.")
                        	return
                        end

			ret, c = get_com("Trigger at [a]75% hp [b]50% hp [c]25% hp?", strbyte("a"))
			if ret == FALSE then return end
                        
                        if c == strbyte("a") then
	        		player.spellbinder_trigger = SPELLBINDER_HP75
                        elseif c == strbyte("b") then
	        		player.spellbinder_trigger = SPELLBINDER_HP50
                        elseif c == strbyte("c") then
	        		player.spellbinder_trigger = SPELLBINDER_HP25
                        else
                                return
                        end
                        player.spellbinder_num = get_spellbinder_max()
                        i = player.spellbinder_num
                        while i > 0 do
                        	local s

				s = get_school_spell("bind")
                                if s == -1 then
	        			player.spellbinder_trigger = 0
        		                player.spellbinder_num = 0
                                	return
                                else
                                	if spell(s).skill_level > 7 + get_level(SPELLBINDER, 35) then
                                        	msg_print("You are only allowed spells with a base level of "..(7 + get_level(SPELLBINDER, 35))..".");
                                                return
                                        end
                                end
	                        player.spellbinder[i] = s
                        	i = i - 1
                        end
                        player.energy = player.energy - 3100;
                        msg_print("Spellbinder ready.")
	end,
	["info"] = 	function()
                	return "number "..(get_spellbinder_max()).." max level "..(7 + get_level(SPELLBINDER, 35))
	end,
        ["desc"] =	{
        		"Stores spells in a trigger.",
                        "When the condition is met all spells fire off at the same time",
                        "This spell takes a long time to cast so you are advised to prepare it",
                        "in a safe area.",
                        "Also it will use the mana for the Spellbinder and the mana for the",
                        "selected spells"
        }
}

DISPERSEMAGIC = add_spell
{
	["name"] = 	"Disperse Magic",
        ["school"] = 	{SCHOOL_META},
        ["level"] = 	15,
        ["mana"] = 	30,
        ["mana_max"] = 	60,
        ["fail"] = 	10,
        -- Unnafected by blindness
        ["blind"] =     FALSE,
        -- Unnafected by confusion
        ["confusion"] = FALSE,
        ["spell"] = 	function()
                        set_blind(0)
                        set_lite(0)
                        if get_level(DISPERSEMAGIC, 50) >= 5 then
	                        set_confused(0)
	                        set_image(0)
                        end
                        if get_level(DISPERSEMAGIC, 50) >= 10 then
	                        set_slow(0)
	                        set_fast(0, 0)
                                set_light_speed(0)
                        end
                        if get_level(DISPERSEMAGIC, 50) >= 15 then
	                        set_stun(0)
	                        set_meditation(0)
	                        set_cut(0)
                        end
                        if get_level(DISPERSEMAGIC, 50) >= 20 then
	                        set_hero(0)
	                        set_shero(0)
                                set_blessed(0)
                                set_shield(0)
                                set_afraid(0)
                                set_parasite(0, 0)
                                set_mimic(0)
                        end
	end,
	["info"] = 	function()
                	return ""
	end,
        ["desc"] =	{
        		"Dispels a lot of magic that can affect you, be it good or bad",
                        "Level 1: blindness and light",
                        "Level 5: confusion and hallucination",
                        "Level 10: speed (both bad or good) and light speed",
                        "Level 15: stunning, meditation, cuts",
                        "Level 20: hero, super hero, bless, shields, afraid, parasites, mimicry",
        }
}
