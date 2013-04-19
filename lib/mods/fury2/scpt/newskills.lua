
--- Constants
MKEY_FIRSTAID = 1004
SKILL_FIRSTAID = 68



function firstaid()
	skill_firstaid_level = get_skill(SKILL_FIRSTAID)
	firstaid_difficulty = (skill_firstaid_level * 3)+15
	randomhealingroll = rand_range(1,100)
	healingamount = (skill_firstaid_level / 3) +1 + rand_range(0,3)
	if player.stun > 0 then
		msg_print("You cannot use this if you are stunned!")
		return TRUE
	end
	if player.chp == player.mhp then
		msg_print("You are in perfect health!")
		return TRUE
	else
		msg_print("You start treating yourself...")
		first_aidtime = 1000-(skill_firstaid_level * 10) 
		energy_use = energy_use + first_aidtime;
	end
	if randomhealingroll <= firstaid_difficulty then
		msg_print("You succed in healing yourself a bit.")
		hp_player(healingamount)
		set_cut(player.cut - healingamount)
	elseif randomhealingroll > firstaid_difficulty and randomhealingroll < 100 then
		msg_print("You fail in healing yourself.")
	elseif randomhealingroll == 100 then
		msg_print("You hurt yourself!")
		take_hit(5,"bad medicine")
	end
	

end


add_mkey
{
        ["mkey"] =      MKEY_FIRSTAID,
        ["fct"] =       function()
			ret = firstaid()
                	energy_use = energy_use + 50;
        end
}







