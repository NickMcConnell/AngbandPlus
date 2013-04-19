


SKILL_SWORD = 18
SKILL_POLEARM = 20
SKILL_AXE = 19
SKILL_LITERACY = 79
SKILL_SHIELDUSE = 59
SKILL_COMBAT = 16
MKEY_ALLAROUNDATTACK = 1001
MKEY_WHIRLWIND_ATTACK = 1011



add_mkey
{
        ["mkey"] =      MKEY_ALLAROUNDATTACK,
        ["fct"] =       function()
				ret = allaroundattack()
		end
}





add_mkey
{
        ["mkey"] =      MKEY_WHIRLWIND_ATTACK,
        ["fct"] =       function()
				ret = whirlwind_attack()
		end
}




function allaroundattack()
				msg_print("You swing at all foes near you!")
				fire_ball(GF_ATTACK, 0, 1, 1)
                		energy_use = energy_use + 600 - ( get_skill(SKILL_COMBAT) * 3 );
			
end




function whirlwind_attack()
				WHIRLWIND_BLOWS = get_skill(SKILL_COMBAT) / 10
				msg_print("You spin in a whirwlind of destruction!")
				fire_ball(GF_ATTACK, 0, WHIRLWIND_BLOWS, 1)
           			energy_use = energy_use + 1000 - ( get_skill(SKILL_COMBAT) * 5 );
				return
			
end





function __hook_ShieldSkillAcBonus()
	local shield1 = get_object(INVEN_ARM);
	local shield2 = get_object(INVEN_ARM+1);
	local shield3 = get_object(INVEN_ARM+2);
	shieldskillacbonus1 = get_skill(SKILL_SHIELDUSE)
	shieldskillacbonus2 = get_skill(SKILL_SHIELDUSE)
	shieldskillacbonus3 = get_skill(SKILL_SHIELDUSE)
	maxshieldskillacbonus1 = 0 + (shield1.ac * 4)
	maxshieldskillacbonus2 = 0 + (shield2.ac * 4)
	maxshieldskillacbonus3 = 0 + (shield3.ac * 4)


	if shieldskillacbonus1 >= maxshieldskillacbonus1 then
		shieldskillacbonus1 = maxshieldskillacbonus1
	end
	if shieldskillacbonus2 >= maxshieldskillacbonus2 then
		shieldskillacbonus2 = maxshieldskillacbonus2
	end
	if shieldskillacbonus3 >= maxshieldskillacbonus3 then
		shieldskillacbonus3 = maxshieldskillacbonus3
	end
	-- No shield skill bonus for berserkers
	if (player.shero == 0) then
		player.to_a = player.to_a + shieldskillacbonus1 + shieldskillacbonus2 + shieldskillacbonus3
		player.dis_to_a = player.dis_to_a + shieldskillacbonus1 + shieldskillacbonus2 + shieldskillacbonus3
	end
end



add_hook_script(HOOK_CALC_BONUS, "__hook_ShieldSkillAcBonus", "__hook_ShieldSkillAcBonus")
