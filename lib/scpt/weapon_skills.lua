-- Additional mkeys for attacking with weapons

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
	energy_use = energy_use + 300 - ( get_skill(SKILL_COMBAT) * 3 );
	return

end

function whirlwind_attack()

	WHIRLWIND_BLOWS = get_skill(SKILL_COMBAT) / 10
	msg_print("You spin in a whirwlind of destruction!")
	fire_ball(GF_ATTACK, 0, WHIRLWIND_BLOWS, 1)
	energy_use = energy_use + 5000 - ( get_skill(SKILL_COMBAT) * 5 );
	return
			
end




