-- File: passive.lua
-- This file contains the code needed to calculate passive bonus of characters.

-- Calculate some passive bonus from the body you incarnate, if morphed or Monster.
function calc_body_bonus ()

        -- If in the player body nothing have to be done
        if (p_ptr.body_monster > 0) then

        	if (p_ptr.disembodied) then
        
                	p_ptr.wraith_form = 20
        	end

        	p_ptr.ac = p_ptr.ac + m_race(p_ptr.body_monster).ac + ((m_race(p_ptr.body_monster).ac * (p_ptr.lev * 5)) / 100)
		p_ptr.dis_ac = p_ptr.dis_ac + m_race(p_ptr.body_monster).ac + ((m_race(p_ptr.body_monster).ac * (p_ptr.lev * 5)) / 100)
        	p_ptr.pspeed = m_race(p_ptr.body_monster).speed

        	if(get_monster_flag1(p_ptr.body_monster, RF1_NEVER_MOVE)) then p_ptr.immovable = TRUE end
        	if(get_monster_flag2(p_ptr.body_monster, RF2_STUPID)) then p_ptr.stat_add[A_INT+1] = p_ptr.stat_add[A_INT+1] - 1 end
        	if(get_monster_flag2(p_ptr.body_monster, RF2_SMART)) then p_ptr.stat_add[A_INT+1] = p_ptr.stat_add[A_INT+1] + 1 end
        	if(get_monster_flag2(p_ptr.body_monster, RF2_REFLECTING)) then p_ptr.reflect = TRUE end
        	if(get_monster_flag2(p_ptr.body_monster, RF2_INVISIBLE)) then p_ptr.invis = p_ptr.invis + 20 end
        	if(get_monster_flag2(p_ptr.body_monster, RF2_REGENERATE)) then p_ptr.regenerate = TRUE end
        	if(get_monster_flag2(p_ptr.body_monster, RF2_AURA_FIRE)) then p_ptr.sh_fire = TRUE end
        	if(get_monster_flag2(p_ptr.body_monster, RF2_AURA_ELEC)) then p_ptr.sh_elec = TRUE end
        	if(get_monster_flag2(p_ptr.body_monster, RF2_PASS_WALL)) then p_ptr.wraith_form = 20 end
        	if(get_monster_flag3(p_ptr.body_monster, RF3_NO_FEAR)) then p_ptr.resist_fear = TRUE end
        	if(get_monster_flag3(p_ptr.body_monster, RF3_NO_SLEEP)) then p_ptr.free_act = TRUE end
        	if(get_monster_flag3(p_ptr.body_monster, RF3_NO_CONF)) then p_ptr.resist_conf = TRUE end
        	if(get_monster_flag7(p_ptr.body_monster, RF7_CAN_FLY)) then p_ptr.ffall = TRUE end
	end
end

-- Calculate total resistances!
function calc_resistances ()

        local i

        i = 24
        while (i <= 52) do

                -- Augment resistances!
                if (inven(i).k_idx > 0) then

			p_ptr.fireres = p_ptr.fireres + inven(i).fireres
			p_ptr.coldres = p_ptr.coldres + inven(i).coldres
			p_ptr.elecres = p_ptr.elecres + inven(i).elecres
			p_ptr.acidres = p_ptr.acidres + inven(i).acidres
			p_ptr.poisres = p_ptr.poisres + inven(i).poisres
			p_ptr.lightres = p_ptr.lightres + inven(i).lightres
			p_ptr.darkres = p_ptr.darkres + inven(i).darkres
			p_ptr.warpres = p_ptr.warpres + inven(i).warpres
			p_ptr.waterres = p_ptr.waterres + inven(i).waterres
			p_ptr.windres = p_ptr.windres + inven(i).windres
			p_ptr.earthres = p_ptr.earthres + inven(i).earthres
			p_ptr.soundres = p_ptr.soundres + inven(i).soundres
			p_ptr.radiores = p_ptr.radiores + inven(i).radiores
			p_ptr.chaosres = p_ptr.chaosres + inven(i).chaosres
			p_ptr.physres = p_ptr.physres + inven(i).physres
			p_ptr.manares = p_ptr.manares + inven(i).manares
		end

                i = i + 1
        end

	if (not(p_ptr.body_monster == 0)) then

		p_ptr.fireres = p_ptr.fireres + m_race(p_ptr.body_monster).fireres
		p_ptr.coldres = p_ptr.coldres + m_race(p_ptr.body_monster).coldres
		p_ptr.elecres = p_ptr.elecres + m_race(p_ptr.body_monster).elecres
		p_ptr.acidres = p_ptr.acidres + m_race(p_ptr.body_monster).acidres
		p_ptr.poisres = p_ptr.poisres + m_race(p_ptr.body_monster).poisres
		p_ptr.lightres = p_ptr.lightres + m_race(p_ptr.body_monster).lightres
		p_ptr.darkres = p_ptr.darkres + m_race(p_ptr.body_monster).darkres
		p_ptr.warpres = p_ptr.warpres + m_race(p_ptr.body_monster).warpres
		p_ptr.waterres = p_ptr.waterres + m_race(p_ptr.body_monster).waterres
		p_ptr.windres = p_ptr.windres + m_race(p_ptr.body_monster).windres
		p_ptr.earthres = p_ptr.earthres + m_race(p_ptr.body_monster).earthres
		p_ptr.soundres = p_ptr.soundres + m_race(p_ptr.body_monster).soundres
		p_ptr.radiores = p_ptr.radiores + m_race(p_ptr.body_monster).radiores
		p_ptr.chaosres = p_ptr.chaosres + m_race(p_ptr.body_monster).chaosres
		p_ptr.physres = p_ptr.physres + m_race(p_ptr.body_monster).physres
		p_ptr.manares = p_ptr.manares + m_race(p_ptr.body_monster).manares
	end

	-- Resistances cannot exceed 100
	if (p_ptr.fireres > 100) then p_ptr.fireres = 100 end
	if (p_ptr.coldres > 100) then p_ptr.coldres = 100 end
	if (p_ptr.elecres > 100) then p_ptr.elecres = 100 end
	if (p_ptr.acidres > 100) then p_ptr.acidres = 100 end
	if (p_ptr.poisres > 100) then p_ptr.poisres = 100 end
	if (p_ptr.lightres > 100) then p_ptr.lightres = 100 end
	if (p_ptr.darkres > 100) then p_ptr.darkres = 100 end
	if (p_ptr.warpres > 100) then p_ptr.warpres = 100 end
	if (p_ptr.waterres > 100) then p_ptr.waterres = 100 end
	if (p_ptr.windres > 100) then p_ptr.windres = 100 end
	if (p_ptr.earthres > 100) then p_ptr.earthres = 100 end
	if (p_ptr.soundres > 100) then p_ptr.soundres = 100 end
	if (p_ptr.radiores > 100) then p_ptr.radiores = 100 end
	if (p_ptr.chaosres > 100) then p_ptr.chaosres = 100 end
	if (p_ptr.physres > 100) then p_ptr.physres = 100 end
	if (p_ptr.manares > 100) then p_ptr.manares = 100 end
end

-- Calculate the player's max hp.
function calc_hitpoints ()

	local mhp
	local i
	local hpmult
        
	hpmult = 0

        -- You gain a little hp every levels...
        mhp = p_ptr.lev * 5;
        -- You gain 25 hp per points of constitution as well.
	-- Also, every points beyond 5 increase hp by 5%.
        -- The first 4 constitutions points does not give you any bonuses.
        mhp = mhp + ((p_ptr.stat_ind[A_CON+1] - 4) * 25)
	if (p_ptr.stat_ind[A_CON+1] > 5) then

		mhp = mhp + ((mhp * ((p_ptr.stat_ind[A_CON+1] - 5) * 5)) / 100)
	end

        -- Monk's One With Body & Mind
        mhp = mhp + (p_ptr.abilities[(CLASS_MONK * 10) + 7] * 15)

        -- Defender's Armored health
        if (((inven(INVEN_BODY).tval == TV_HARD_ARMOR) or (inven(INVEN_BODY).tval == TV_DRAG_ARMOR)) and p_ptr.abilities[(CLASS_DEFENDER * 10) + 7] >= 1) then

                mhp = mhp + ((inven(INVEN_BODY).ac * 3) * p_ptr.abilities[(CLASS_DEFENDER * 10) + 7])
        end

        -- Make sure we at least have 1 hp
        if (mhp < 1) then mhp = 1 end

	-- Potions of Heroism/Berserk Strength improve hp.
	if (p_ptr.hero > 0) then mhp = mhp + (mhp / 10) end
	if (p_ptr.shero > 0) then mhp = mhp + (mhp / 4) end

	-- Check for the "LIFE" flag.
        i = 24
        while (i <= 52) do

                -- Hitpoints multiplier
                if ((inven(i).k_idx > 0) and (get_object_flag2(inven(i), TR2_LIFE))) then

                        hpmult = hpmult + (inven(i).pval * 10)
                end

                i = i + 1
        end

        -- Check for life boosting abilities
        hpmult = hpmult + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 3] * 3)

        -- Augment Hitpoint
        mhp = mhp + ((mhp * hpmult) / 100)

        i = 0;

        -- Hp if you're in a monster's body.
        if (p_ptr.body_monster > 0) then

                mhp = (maxroll(m_race(p_ptr.body_monster).hdice, m_race(p_ptr.body_monster).hside) / 10)
                mhp = mhp * p_ptr.lev
		if (p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 10] >= 1) then
			mhp = mhp + ((mhp * (p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 10] * 3)) / 100)
		end
        end

        if (p_ptr.disembodied) then mhp = 1 end

	-- New maximum hitpoints
	if (not(p_ptr.mhp == mhp)) then

		-- Enforce maximum
		if (p_ptr.chp >= mhp) then

			p_ptr.chp = mhp
			p_ptr.chp_frac = 0
		end

		-- Save the new max-hitpoints
		p_ptr.mhp = mhp

		-- Update stuff
		lua_update_stuff()
	end

        -- Check the hp...
        if (p_ptr.mhp > 999999 or p_ptr.mhp < 0) then p_ptr.mhp = 999999 end

end

-- Calculate mana.
function calc_mana ()

	local msp
	local cur_wgt
	local max_wgt
	local i
	local manamult
	manamult = 0

        -- Start with no mana.
        msp = 0
        -- You gain 10 mana per points of intelligence.
        -- The first 5 intelligence points does not give you any bonuses.
        msp = msp + ((p_ptr.stat_ind[A_INT+1] - 5) * 10)

        -- Mage's Mana Boost ability!
        msp = msp + (p_ptr.abilities[(CLASS_MAGE * 10) + 1] * 20)

        -- Monk's One With Body & Mind
        msp = msp + (p_ptr.abilities[(CLASS_MONK * 10) + 7] * 15)

        -- Spellcraft skill gives you more mana...
        if (p_ptr.skill[2] >= 80) then msp = msp + (msp / 4) end

        i = 24
        while (i <= 52) do

                -- Mana multiplier
                if ((inven(i).k_idx > 0) and (get_object_flag1(inven(i), TR1_MANA))) then

                        manamult = manamult + (inven(i).pval * 10)
                end

                i = i + 1
        end

        -- Increased Mana ability...
        manamult = manamult + (p_ptr.abilities[(CLASS_HIGH_MAGE * 10) + 1] * 3)

        -- Augment Mana
        msp = msp + ((msp * manamult) / 100)

	-- Armor causes encumbrance.
	-- Each equipment parts adds up to a percentile penality based on weight.
	-- For example, wearing an armor that weights 20.0 lb will cause a loss
	-- of 20% of your mana.
	cur_wgt = 0
	cur_wgt = (inven(INVEN_BODY).weight / 10)
	cur_wgt = cur_wgt + (inven(INVEN_FEET).weight / 10)
	cur_wgt = cur_wgt + (inven(INVEN_HANDS).weight / 10)
	cur_wgt = cur_wgt + (inven(INVEN_HEAD).weight / 10)
	cur_wgt = cur_wgt + (inven(INVEN_OUTER).weight / 10)

	-- For the arms slot, arm bands/bracers will not penalize
	if (inven(INVEN_ARM).tval == TV_SHIELD) then
		cur_wgt = cur_wgt + (inven(INVEN_ARM).weight / 10)
	end

	-- Check the mana loss percentage...
	if (cur_wgt > 100) then cur_wgt = 100 end
	if (cur_wgt < 0) then cur_wgt = 0 end

	-- Reduce mana by percentile amount.
	msp = msp - ((msp * cur_wgt) / 100)

	-- Wearing gloves is especially bad.
	if (inven(INVEN_HANDS).k_idx > 0) then msp = (msp / 2) end

	-- Warriors gets less mana...
	if (p_ptr.pclass == CLASS_WARRIOR) then msp = msp - (msp / 5) end

	-- Fighters very low mana...
	if (p_ptr.pclass == CLASS_FIGHTER) then msp = msp - (msp / 2) end

        -- Check the mana...
        -- Probably will never reach this cap.
        if (msp > 99999) then p_ptr.msp = 99999 end

	-- Mana can never be negative
	if (msp < 0) then msp = 0 end

	-- New maximum mana
	if (not(p_ptr.msp == msp)) then

		-- Enforce maximum
		if (p_ptr.csp >= msp) then

			p_ptr.csp = msp
			p_ptr.csp_frac = 0
		end

		-- Save the new max-mana
		p_ptr.msp = msp

		-- Update stuff
		lua_update_stuff()
	end

end

function calc_bonuses ()

	local	i
	local	j
	local	v
	local	hold
        local   old_invis
	local   old_speed
	local   old_telepathy
	local   old_see_inv
	local   old_dis_ac
	local   old_dis_to_a
	local   extra_blows
	local   extra_shots


	-- Save the old speed
	old_speed = p_ptr.pspeed

	-- Save the old vision stuff
	old_telepathy = p_ptr.telepathy
	old_see_inv = p_ptr.see_inv

	-- Save the old armor class
	old_dis_ac = p_ptr.dis_ac
	old_dis_to_a = p_ptr.dis_to_a

        -- Save the old invisibility
        old_invis = p_ptr.invis

	-- Clear extra blows/shots
	extra_blows = 0
	extra_shots = 0

	-- Clear the stat modifiers
	calc_stats(0)

        -- Mana multiplier
        p_ptr.to_m = 0

        -- Spell power
        p_ptr.to_s = 1

	-- Clear the Displayed/Real armor class
	p_ptr.ac = 0
	p_ptr.dis_ac = 0

	-- Clear the Displayed/Real Bonuses
	p_ptr.to_h = 0
	p_ptr.dis_to_h = 0
	p_ptr.to_d = 0
	p_ptr.dis_to_d = 0
	p_ptr.to_a = 0
	p_ptr.dis_to_a = 0

	-- Start with "normal" speed
	p_ptr.pspeed = 110

	-- Start with a single blow per turn
	p_ptr.num_blow = 1

	-- Start with a single blow per turn
	p_ptr.num_blow2 = 1

	-- Start with a single shot per turn
	p_ptr.num_fire = 1

	-- Reset the "xtra" tval
	p_ptr.tval_xtra = 0

	-- Reset the "ammo" tval
	p_ptr.tval_ammo = 0

	-- Clear all the flags
        p_ptr.invis = 0
	p_ptr.aggravate = FALSE
	p_ptr.teleport = FALSE
	p_ptr.exp_drain = FALSE
	p_ptr.xtra_might = FALSE
	p_ptr.see_inv = FALSE
	p_ptr.free_act = FALSE
	p_ptr.slow_digest = FALSE
	p_ptr.regenerate = FALSE
        p_ptr.fly = FALSE
        p_ptr.climb = FALSE
	p_ptr.ffall = FALSE
	p_ptr.hold_life = FALSE
	p_ptr.telepathy = FALSE
	p_ptr.lite = FALSE
	p_ptr.sustain_str = FALSE
	p_ptr.sustain_int = FALSE
	p_ptr.sustain_wis = FALSE
	p_ptr.sustain_con = FALSE
	p_ptr.sustain_dex = FALSE
	p_ptr.sustain_chr = FALSE
	p_ptr.resist_conf = FALSE
	p_ptr.resist_blind = FALSE
	p_ptr.resist_fear = FALSE
	p_ptr.reflect = FALSE
	p_ptr.sh_fire = FALSE
	p_ptr.sh_elec = FALSE

	p_ptr.fireres = 0
	p_ptr.coldres = 0
	p_ptr.elecres = 0
	p_ptr.acidres = 0
	p_ptr.poisres = 0
	p_ptr.lightres = 0
	p_ptr.darkres = 0
	p_ptr.warpres = 0
	p_ptr.waterres = 0
	p_ptr.earthres = 0
	p_ptr.soundres = 0
	p_ptr.radiores = 0
	p_ptr.chaosres = 0
	p_ptr.physres = 0
	p_ptr.manares = 0

        -- Start with no bonuses in skills...
	calc_skills(0)

	-- Base infravision (purely racial)
	p_ptr.see_infra = 0
        
        calc_body_bonus()

	-- Bonuses from classes.
	
	-- General stats bonuses
	p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] + ((p_ptr.stat_cur[A_STR+1] * classes_def[p_ptr.pclass + 1].str_bonus) / 100)
	p_ptr.stat_add[A_INT+1] = p_ptr.stat_add[A_INT+1] + ((p_ptr.stat_cur[A_INT+1] * classes_def[p_ptr.pclass + 1].int_bonus) / 100)
	p_ptr.stat_add[A_WIS+1] = p_ptr.stat_add[A_WIS+1] + ((p_ptr.stat_cur[A_WIS+1] * classes_def[p_ptr.pclass + 1].wis_bonus) / 100)
	p_ptr.stat_add[A_DEX+1] = p_ptr.stat_add[A_DEX+1] + ((p_ptr.stat_cur[A_DEX+1] * classes_def[p_ptr.pclass + 1].dex_bonus) / 100)
	p_ptr.stat_add[A_CON+1] = p_ptr.stat_add[A_CON+1] + ((p_ptr.stat_cur[A_CON+1] * classes_def[p_ptr.pclass + 1].con_bonus) / 100)
	p_ptr.stat_add[A_CHR+1] = p_ptr.stat_add[A_CHR+1] + ((p_ptr.stat_cur[A_CHR+1] * classes_def[p_ptr.pclass + 1].chr_bonus) / 100)
	
	-- General skills bonuses
	calc_skills(1)

	-- Special bonuses/penalities for some classes.

	if (p_ptr.pclass == CLASS_FIGHTER) then
	  p_ptr.resist_fear = TRUE
	end
	if (p_ptr.pclass == CLASS_PALADIN) then
	  p_ptr.resist_fear = TRUE
	end
	if (p_ptr.pclass == CLASS_MONK) then
	  p_ptr.free_act = TRUE
	end
	if (p_ptr.pclass == CLASS_DEFENDER) then
	  p_ptr.free_act = TRUE
	  p_ptr.resist_fear = TRUE
	  p_ptr.resist_conf = TRUE
	  p_ptr.to_a = p_ptr.to_a + (p_ptr.lev * 2)
          p_ptr.dis_to_a = p_ptr.dis_to_a + (p_ptr.lev * 2)
	end
	if (p_ptr.pclass == CLASS_JUSTICE_WARRIOR) then
	  p_ptr.lightres = 50
	  p_ptr.hold_life = TRUE
	  p_ptr.resist_fear = TRUE
	  p_ptr.sustain_wis = TRUE
	  p_ptr.sustain_chr = TRUE
	  p_ptr.lite = TRUE
	end
	if (p_ptr.pclass == CLASS_ZELAR) then
	  p_ptr.free_act = TRUE
	  p_ptr.resist_fear = TRUE
	  p_ptr.resist_conf = TRUE
	  p_ptr.sustain_str = TRUE
	  p_ptr.sustain_con = TRUE
	  p_ptr.sustain_wis = TRUE
	end
	if (p_ptr.pclass == CLASS_SOUL_GUARDIAN) then
	  p_ptr.hold_life = TRUE
	  p_ptr.resist_fear = TRUE
	  p_ptr.resist_conf = TRUE
	  p_ptr.sustain_wis = TRUE
	  p_ptr.sustain_chr = TRUE
	end
	if (p_ptr.pclass == CLASS_SHADOW) then
	  p_ptr.sustain_dex = TRUE
	end

	-- Bonuses from races
	if (p_ptr.prace == RACE_HUMAN) then
	  p_ptr.stat_add[A_INT+1] = p_ptr.stat_add[A_INT+1] + (p_ptr.stat_cur[A_INT+1] / 4)
	  p_ptr.stat_add[A_CHR+1] = p_ptr.stat_add[A_CHR+1] + (p_ptr.stat_cur[A_CHR+1] / 4)
	  p_ptr.skill_bonus[10] = p_ptr.skill_bonus[10] + (p_ptr.skill_base[10] / 4)
	  p_ptr.skill_bonus[11] = p_ptr.skill_bonus[11] + (p_ptr.skill_base[11] / 4)
	  p_ptr.skill_bonus[12] = p_ptr.skill_bonus[12] + (p_ptr.skill_base[12] / 4)
	end
	if (p_ptr.prace == RACE_HALF_ELF) then
	  p_ptr.stat_add[A_DEX+1] = p_ptr.stat_add[A_DEX+1] + (p_ptr.stat_cur[A_DEX+1] / 10)
	  p_ptr.stat_add[A_WIS+1] = p_ptr.stat_add[A_WIS+1] + (p_ptr.stat_cur[A_WIS+1] / 10)
	  p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] - (p_ptr.stat_cur[A_STR+1] / 10)
	  p_ptr.stat_add[A_CON+1] = p_ptr.stat_add[A_CON+1] - (p_ptr.stat_cur[A_CON+1] / 10)
	  p_ptr.skill_bonus[20] = p_ptr.skill_bonus[20] + (p_ptr.skill_base[20] / 10)
	  p_ptr.skill_bonus[6] = p_ptr.skill_bonus[6] + (p_ptr.skill_base[6] / 10)
	  p_ptr.skill_bonus[2] = p_ptr.skill_bonus[2] + (p_ptr.skill_base[2] / 10)
	  p_ptr.skill_bonus[19] = p_ptr.skill_bonus[19] + (p_ptr.skill_base[19] / 4)
	end
	if (p_ptr.prace == RACE_ELF) then
	  p_ptr.stat_add[A_DEX+1] = p_ptr.stat_add[A_DEX+1] + (p_ptr.stat_cur[A_DEX+1] / 4)
	  p_ptr.stat_add[A_WIS+1] = p_ptr.stat_add[A_WIS+1] + (p_ptr.stat_cur[A_WIS+1] / 4)
	  p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] - (p_ptr.stat_cur[A_STR+1] / 4)
	  p_ptr.stat_add[A_CON+1] = p_ptr.stat_add[A_CON+1] - (p_ptr.stat_cur[A_CON+1] / 4)
	  p_ptr.skill_bonus[20] = p_ptr.skill_bonus[20] + (p_ptr.skill_base[20] / 4)
	  p_ptr.skill_bonus[6] = p_ptr.skill_bonus[6] + (p_ptr.skill_base[6] / 4)
	  p_ptr.skill_bonus[2] = p_ptr.skill_bonus[2] + (p_ptr.skill_base[2] / 4)
	end
	if (p_ptr.prace == RACE_DWARF) then
	  p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] + (p_ptr.stat_cur[A_STR+1] / 4)
	  p_ptr.stat_add[A_CON+1] = p_ptr.stat_add[A_CON+1] + (p_ptr.stat_cur[A_CON+1] / 3)
	  p_ptr.stat_add[A_WIS+1] = p_ptr.stat_add[A_WIS+1] - (p_ptr.stat_cur[A_WIS+1] / 4)
	  p_ptr.stat_add[A_CHR+1] = p_ptr.stat_add[A_CHR+1] - (p_ptr.stat_cur[A_CHR+1] / 4)
	  p_ptr.skill_bonus[17] = p_ptr.skill_bonus[17] + (p_ptr.skill_base[17] / 4)
	  p_ptr.skill_bonus[5] = p_ptr.skill_bonus[5] + (p_ptr.skill_base[5] / 4)
	  p_ptr.skill_bonus[12] = p_ptr.skill_bonus[12] + (p_ptr.skill_base[12] / 2)
	  p_ptr.skill_bonus[6] = p_ptr.skill_bonus[6] - (p_ptr.skill_base[6] / 4)
	  p_ptr.skill_bonus[2] = p_ptr.skill_bonus[2] - (p_ptr.skill_base[2] / 4)
	end
	if (p_ptr.prace == RACE_GNOME) then
	  p_ptr.stat_add[A_INT+1] = p_ptr.stat_add[A_INT+1] + (p_ptr.stat_cur[A_INT+1] / 4)
	  p_ptr.stat_add[A_CON+1] = p_ptr.stat_add[A_CON+1] + (p_ptr.stat_cur[A_CON+1] / 10)
	  p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] - (p_ptr.stat_cur[A_STR+1] / 4)
	  p_ptr.stat_add[A_DEX+1] = p_ptr.stat_add[A_DEX+1] - (p_ptr.stat_cur[A_DEX+1] / 4)
	  p_ptr.skill_bonus[2] = p_ptr.skill_bonus[2] + (p_ptr.skill_base[2] / 4)
	  p_ptr.skill_bonus[11] = p_ptr.skill_bonus[11] + (p_ptr.skill_base[11] / 2)
	  p_ptr.skill_bonus[6] = p_ptr.skill_bonus[6] - (p_ptr.skill_base[6] / 4)
	end
	if (p_ptr.prace == RACE_KOBOLD) then
	  p_ptr.stat_add[A_CHR+1] = p_ptr.stat_add[A_CHR+1] - (p_ptr.stat_cur[A_CHR+1] / 3)
	  p_ptr.stat_add[A_CON+1] = p_ptr.stat_add[A_CON+1] - (p_ptr.stat_cur[A_CON+1] / 3)
	  p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] - (p_ptr.stat_cur[A_STR+1] / 3)
	  p_ptr.stat_add[A_DEX+1] = p_ptr.stat_add[A_DEX+1] + (p_ptr.stat_cur[A_DEX+1] / 4)
	  p_ptr.skill_bonus[21] = p_ptr.skill_bonus[21] + (p_ptr.skill_base[21] / 4)
	  p_ptr.skill_bonus[6] = p_ptr.skill_bonus[6] + (p_ptr.skill_base[6] / 4)
	  p_ptr.skill_bonus[7] = p_ptr.skill_bonus[7] + (p_ptr.skill_base[7] / 4)
	  p_ptr.skill_bonus[10] = p_ptr.skill_bonus[10] + (p_ptr.skill_base[10] / 4)
	  p_ptr.poisres = p_ptr.poisres + 75
	end
	if (p_ptr.prace == RACE_DEVLING) then
	  p_ptr.stat_add[A_CON+1] = p_ptr.stat_add[A_CON+1] - (p_ptr.stat_cur[A_CON+1] / 3)
	  p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] - (p_ptr.stat_cur[A_STR+1] / 3)
	  p_ptr.stat_add[A_DEX+1] = p_ptr.stat_add[A_DEX+1] + (p_ptr.stat_cur[A_DEX+1] / 4)
	  p_ptr.skill_bonus[15] = p_ptr.skill_bonus[15] + (p_ptr.skill_base[15] / 4)
	  p_ptr.skill_bonus[6] = p_ptr.skill_bonus[6] + p_ptr.skill_base[6]
	  p_ptr.skill_bonus[10] = p_ptr.skill_bonus[10] + (p_ptr.skill_base[10] / 4)
	  p_ptr.fireres = p_ptr.fireres + 25
	  p_ptr.darkres = p_ptr.darkres + 25
	  p_ptr.lightres = p_ptr.lightres - 25
	end
	if (p_ptr.prace == RACE_CELESTIAL) then
	  p_ptr.stat_add[A_WIS+1] = p_ptr.stat_add[A_WIS+1] + (p_ptr.stat_cur[A_WIS+1] / 4)
	  p_ptr.stat_add[A_CHR+1] = p_ptr.stat_add[A_CHR+1] + (p_ptr.stat_cur[A_CHR+1] / 4)
	  p_ptr.lightres = p_ptr.lightres + 100
	  p_ptr.elecres = p_ptr.elecres + 25
	  p_ptr.windres = p_ptr.windres + 25
	  p_ptr.darkres = p_ptr.darkres - 100
	  p_ptr.fly = TRUE
	end
	if (p_ptr.prace == RACE_DEMON) then
	  p_ptr.stat_add[A_INT+1] = p_ptr.stat_add[A_INT+1] + (p_ptr.stat_cur[A_INT+1] / 4)
	  p_ptr.stat_add[A_CHR+1] = p_ptr.stat_add[A_CHR+1] + (p_ptr.stat_cur[A_CHR+1] / 4)
	  p_ptr.darkres = p_ptr.darkres + 100
	  p_ptr.fireres = p_ptr.fireres + 25
	  p_ptr.coldres = p_ptr.coldres + 25
	  p_ptr.lightres = p_ptr.lightres - 100
	  p_ptr.fly = TRUE
	end
	if (p_ptr.prace == RACE_ZULGOR) then
	  p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] + p_ptr.stat_cur[A_STR+1]
	  p_ptr.chaosres = p_ptr.chaosres + 100
	end
	if (p_ptr.prace == RACE_MONSTER) then
	  local strbonus
	  local dexbonus
	  local intbonus
	  local martsbonus
	  local spellbonus
	  strbonus = (m_race(p_ptr.body_monster).str + ((m_race(p_ptr.body_monster).str * ((p_ptr.lev - 1) * 5)) / 100)) - 5
	  dexbonus = (m_race(p_ptr.body_monster).dex + ((m_race(p_ptr.body_monster).dex * ((p_ptr.lev - 1) * 5)) / 100)) - 5
	  intbonus = (m_race(p_ptr.body_monster).mind + ((m_race(p_ptr.body_monster).mind * ((p_ptr.lev - 1) * 5)) / 100)) - 5
	  martsbonus = m_race(p_ptr.body_monster).skill_attack + ((m_race(p_ptr.body_monster).skill_attack * ((p_ptr.lev - 1) * 5)) / 100)
	  spellbonus = m_race(p_ptr.body_monster).skill_magic + ((m_race(p_ptr.body_monster).skill_magic * ((p_ptr.lev - 1) * 5)) / 100)
	  if (strbonus < 0) then strbonus = 0 end
	  if (dexbonus < 0) then dexbonus = 0 end
	  if (intbonus < 0) then intbonus = 0 end
	  if (martsbonus < 0) then martsbonus = 0 end
	  if (spellbonus < 0) then spellbonus = 0 end
	  p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] + strbonus
	  p_ptr.stat_add[A_DEX+1] = p_ptr.stat_add[A_DEX+1] + dexbonus
	  p_ptr.stat_add[A_INT+1] = p_ptr.stat_add[A_INT+1] + intbonus
	  p_ptr.skill_bonus[19] = p_ptr.skill_bonus[19] + martsbonus
	  p_ptr.skill_bonus[3] = p_ptr.skill_bonus[3] + martsbonus
	  p_ptr.skill_bonus[2] = p_ptr.skill_bonus[2] + spellbonus
          p_ptr.resist_fear = TRUE
          p_ptr.sustain_str = TRUE
          p_ptr.sustain_dex = TRUE
          p_ptr.sustain_int = TRUE
          p_ptr.sustain_con = TRUE
          p_ptr.sustain_wis = TRUE
          p_ptr.sustain_chr = TRUE
          p_ptr.stun = 0
          p_ptr.lite = TRUE
	end

        if (p_ptr.prace == RACE_MONSTER) then

                p_ptr.pspeed = p_ptr.pspeed + (p_ptr.lev / 5)
        end

        -- Misc stuff
	
	-- Calculate final resistances!
	-- Includes any previous bonuses/penalities
	calc_resistances()

        -- Stat boosting abilities
        -- Warrior's Strength
        if (p_ptr.abilities[(CLASS_WARRIOR * 10) + 2] >= 1) then
        
                p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] + (((p_ptr.stat_cur[A_STR+1] * p_ptr.abilities[(CLASS_WARRIOR * 10) + 2]) * 3) / 100)
        end
        -- Priest's Divine Blood
        if (p_ptr.abilities[(CLASS_PRIEST * 10) + 3] >= 1) then
        
                p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] + (((p_ptr.stat_cur[A_STR+1] * p_ptr.abilities[(CLASS_PRIEST * 10) + 3]) * 1) / 100)
                p_ptr.stat_add[A_INT+1] = p_ptr.stat_add[A_INT+1] + (((p_ptr.stat_cur[A_INT+1] * p_ptr.abilities[(CLASS_PRIEST * 10) + 3]) * 1) / 100)
                p_ptr.stat_add[A_WIS+1] = p_ptr.stat_add[A_WIS+1] + (((p_ptr.stat_cur[A_WIS+1] * p_ptr.abilities[(CLASS_PRIEST * 10) + 3]) * 1) / 100)
                p_ptr.stat_add[A_DEX+1] = p_ptr.stat_add[A_DEX+1] + (((p_ptr.stat_cur[A_DEX+1] * p_ptr.abilities[(CLASS_PRIEST * 10) + 3]) * 1) / 100)
                p_ptr.stat_add[A_CON+1] = p_ptr.stat_add[A_CON+1] + (((p_ptr.stat_cur[A_CON+1] * p_ptr.abilities[(CLASS_PRIEST * 10) + 3]) * 1) / 100)
                p_ptr.stat_add[A_CHR+1] = p_ptr.stat_add[A_CHR+1] + (((p_ptr.stat_cur[A_CHR+1] * p_ptr.abilities[(CLASS_PRIEST * 10) + 3]) * 1) / 100)
        end
        -- Rogue's Dexterity 
        if (p_ptr.abilities[(CLASS_ROGUE * 10) + 3] >= 1) then

                p_ptr.stat_add[A_DEX+1] = p_ptr.stat_add[A_DEX+1] + (((p_ptr.stat_cur[A_DEX+1] * p_ptr.abilities[(CLASS_ROGUE * 10) + 3]) * 3) / 100)
        end
        -- Ranger's Wilderness lore!
        if (p_ptr.abilities[(CLASS_RANGER * 10) + 1] >= 1) then

                p_ptr.see_infra = p_ptr.see_infra + ((p_ptr.abilities[(CLASS_RANGER * 10) + 1]) / 3)
                if (p_ptr.abilities[(CLASS_RANGER * 10) + 1] >= 20) then p_ptr.telepathy = TRUE end
        end
        -- Monk's Wisdom
        if (p_ptr.abilities[(CLASS_MONK * 10) + 5] >= 1) then

                p_ptr.stat_add[A_WIS+1] = p_ptr.stat_add[A_WIS+1] + (((p_ptr.stat_cur[A_WIS+1] * p_ptr.abilities[(CLASS_MONK * 10) + 5]) * 3) / 100)
        end
        -- Monster Mage's Constitution
        if (p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 3] >= 1) then

                p_ptr.stat_add[A_CON+1] = p_ptr.stat_add[A_CON+1] + (((p_ptr.stat_cur[A_CON+1] * p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 3]) * 3) / 100)
        end
        -- Soul Guardian's Soul Guide
        if (p_ptr.abilities[(CLASS_SOUL_GUARDIAN * 10) + 6] >= 1) then

                p_ptr.stat_add[A_WIS+1] = p_ptr.stat_add[A_WIS+1] + (((p_ptr.stat_cur[A_WIS+1] * p_ptr.abilities[(CLASS_SOUL_GUARDIAN * 10) + 6]) * 2) / 100)
                p_ptr.stat_add[A_CHR+1] = p_ptr.stat_add[A_CHR+1] + (((p_ptr.stat_cur[A_CHR+1] * p_ptr.abilities[(CLASS_SOUL_GUARDIAN * 10) + 6]) * 2) / 100)
        end

   -- Wielding a rod + high rod skill? Raise the spellcraft! ;)
   if (rod_has() and p_ptr.skill[18] >= 70) then

	p_ptr.skill_bonus[2] = p_ptr.skill_bonus[2] + p_ptr.skill_base[2] / 2
	p_ptr.skill_bonus[23] = p_ptr.skill_bonus[23] + p_ptr.skill_base[23] / 2
	p_ptr.skill_bonus[24] = p_ptr.skill_bonus[24] + p_ptr.skill_base[24] / 2
	p_ptr.skill_bonus[25] = p_ptr.skill_bonus[25] + p_ptr.skill_base[25] / 2
	p_ptr.skill_bonus[26] = p_ptr.skill_bonus[26] + p_ptr.skill_base[26] / 2
	p_ptr.skill_bonus[27] = p_ptr.skill_bonus[27] + p_ptr.skill_base[27] / 2
   end

   -- Weapon Mastery!
   if (p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] >= 1) then

           p_ptr.skill_bonus[13] = p_ptr.skill_bonus[13] + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] * 2)
           p_ptr.skill_bonus[14] = p_ptr.skill_bonus[14] + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] * 2)
           p_ptr.skill_bonus[15] = p_ptr.skill_bonus[15] + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] * 2)
           p_ptr.skill_bonus[16] = p_ptr.skill_bonus[16] + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] * 2)
           p_ptr.skill_bonus[17] = p_ptr.skill_bonus[17] + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] * 2)
   end
   -- Fighter's Fighting Mastery!
   if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 10] >= 1) then

           p_ptr.skill_bonus[1] = p_ptr.skill_bonus[1] + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 10] * 2)
   end
   -- Rogue's Rogue Mastery!
   if (p_ptr.abilities[(CLASS_ROGUE * 10) + 10] >= 1) then

           p_ptr.skill_bonus[6] = p_ptr.skill_bonus[6] + (p_ptr.abilities[(CLASS_ROGUE * 10) + 10] * 2)
           p_ptr.skill_bonus[7] = p_ptr.skill_bonus[7] + (p_ptr.abilities[(CLASS_ROGUE * 10) + 10] * 2)
   end
   -- Monk's Martial Arts Mastery!
   if (p_ptr.abilities[(CLASS_MONK * 10) + 10] >= 1) then

           p_ptr.skill_bonus[19] = p_ptr.skill_bonus[19] + (p_ptr.abilities[(CLASS_MONK * 10) + 10] * 2)
   end
   -- Archer's Marksmanship!
   if (p_ptr.abilities[(CLASS_ARCHER * 10) + 10] >= 1) then

           p_ptr.skill_bonus[3] = p_ptr.skill_bonus[3] + (p_ptr.abilities[(CLASS_ARCHER * 10) + 10] * 2)
   end

   -- High-Mage's Spell Mastery!
   if (p_ptr.abilities[(CLASS_HIGH_MAGE * 10) + 10] >= 1) then

           p_ptr.skill_bonus[2] = p_ptr.skill_bonus[2] + (p_ptr.abilities[(CLASS_HIGH_MAGE * 10) + 10] * 2)
   end
   -- Monster Mage's Monstrous Leadership!
   if (p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 4] >= 1 and not(p_ptr.body_monster == 0)) then

           p_ptr.skill_bonus[10] = p_ptr.skill_bonus[10] + (p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 4] * 3)
   end
   -- Monster Mage's Monstrous Martial Arts!
   if (p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 5] >= 1 and not(p_ptr.body_monster == 0)) then

           p_ptr.skill_bonus[19] = p_ptr.skill_bonus[19] + (p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 5] * 2)
   end
   -- Monsers Improved Attack!
   if (p_ptr.abilities[(CLASS_APPRENTICE * 10) + 1] >= 1 and not(p_ptr.body_monster == 0)) then

           p_ptr.to_h = 5 * p_ptr.abilities[(CLASS_APPRENTICE * 10) + 1]
           p_ptr.dis_to_h = 5 * p_ptr.abilities[(CLASS_APPRENTICE * 10) + 1]
           p_ptr.skill_bonus[19] = p_ptr.skill_bonus[19] + (p_ptr.abilities[(CLASS_APPRENTICE * 10) + 1] * 2)
   end

   -- High Monk's Legendary Agility!
   if (p_ptr.abilities[(CLASS_ZELAR * 10) + 10] >= 1) then

           p_ptr.skill_bonus[6] = p_ptr.skill_bonus[6] + (p_ptr.abilities[(CLASS_ZELAR * 10) + 10] * 3)
   end

	-- If you have the "SAFETY" ability, or you're a Monster, you can't be paralyzed!
	if (safety_check() or p_ptr.prace == RACE_MONSTER) then

                p_ptr.stun = 0
                p_ptr.paralyzed = 0
        end

	-- Apply temporary "stun"
	if (p_ptr.stun > 50) then

		p_ptr.to_h = p_ptr.to_h - (p_ptr.to_h / 2)
		p_ptr.dis_to_h = p_ptr.dis_to_h - (p_ptr.dis_to_h / 2)
		p_ptr.to_d = p_ptr.to_d - (p_ptr.to_d / 2)
		p_ptr.dis_to_d = p_ptr.dis_to_d - (p_ptr.dis_to_d / 2)

	elseif (p_ptr.stun > 0) then

		p_ptr.to_h = p_ptr.to_h - (p_ptr.to_h / 4)
		p_ptr.dis_to_h = p_ptr.dis_to_h - (p_ptr.dis_to_h / 4)
		p_ptr.to_d = p_ptr.to_d - (p_ptr.to_d / 4)
		p_ptr.dis_to_d = p_ptr.dis_to_d - (p_ptr.dis_to_d / 4)
	end

	-- Temporary blessing
	if (p_ptr.blessed > 0) then

                p_ptr.ac = p_ptr.ac * 2
                p_ptr.dis_ac = p_ptr.dis_ac * 2
                p_ptr.to_h = p_ptr.to_h + (p_ptr.to_h * 2)
                p_ptr.dis_to_h = p_ptr.dis_to_h + (p_ptr.dis_to_h * 2)
	end

	-- Temporary invisibility
        if (p_ptr.tim_invisible > 0) then

                if (p_ptr.abilities[(CLASS_SHADOW * 10) + 8] >= 1) then

                        local statbonus
			statbonus = p_ptr.skill[7] / 4
                        statbonus = statbonus + (statbonus * ((p_ptr.abilities[(CLASS_SHADOW * 10) + 8] * 10) / 100))
                        p_ptr.skill_bonus[6] = p_ptr.skill_bonus[6] + statbonus
                end                               
                p_ptr.invis = p_ptr.invis + p_ptr.tim_inv_pow
	end

	-- Temporary shield
	if (p_ptr.shield > 0) then

                p_ptr.to_a = p_ptr.to_a + p_ptr.shield_power
                p_ptr.dis_to_a = p_ptr.dis_to_a + p_ptr.shield_power
	end

	-- Temporary "fast"
	if (p_ptr.fast > 0) then

		p_ptr.pspeed = p_ptr.pspeed + 10
	end

	-- Temporary "slow"
	if (p_ptr.slow > 0) then

		p_ptr.pspeed = p_ptr.pspeed - 10
	end

        -- Temporary ac boost! :)
        if (p_ptr.ac_boost_dur > 0) then

                p_ptr.to_a = p_ptr.to_a + p_ptr.ac_boost
                p_ptr.dis_to_a = p_ptr.dis_to_a + p_ptr.ac_boost
	end

	-- Temporary Telepathy
	if (p_ptr.tim_esp > 0) then

		p_ptr.telepathy = TRUE
	end

	-- Temporary see invisible
	if (p_ptr.tim_invis > 0) then

		p_ptr.see_inv = TRUE;
	end

        -- Hack -- Can Fly -> Can Levitate
        if (p_ptr.fly) then

                p_ptr.ffall = TRUE
	end

	-- Hack -- Hero/Shero -> Res fear
	if ((p_ptr.hero > 0) or (p_ptr.shero > 0)) then

		p_ptr.resist_fear = TRUE
	end


	-- Hack -- Telepathy Change + See Invis Change 
	-- (adelie) Combined these two hacks to prevent an extra redraw in the event that the player has both see invisibility and telepathy.
	if ((not(p_ptr.telepathy == old_telepathy)) or (not(p_ptr.see_inv == old_see_inv))) then

		lua_update_monsters()
	end
	
	-- Fighter's Accuracy!
        if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 6] >= 1) then

                p_ptr.to_h = p_ptr.to_h + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 6] * 3) * p_ptr.skill[1]
                p_ptr.dis_to_h = p_ptr.dis_to_h + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 6] * 3) * p_ptr.skill[1]
        end

        -- Fencer's Spirit sword skill!
        if (sword_has() or p_ptr.skill[13] >= 50) then

                p_ptr.to_h = p_ptr.to_h * 2
                p_ptr.dis_to_h = p_ptr.dis_to_h * 2
        end

	-- Temporary "Hero"
	if (p_ptr.hero > 0) then

		p_ptr.to_h = p_ptr.to_h + (p_ptr.to_h / 5) * (p_ptr.skill[11] + 1)
		p_ptr.dis_to_h = p_ptr.dis_to_h + (p_ptr.dis_to_h / 5) * (p_ptr.skill[11] + 1)
		p_ptr.to_d = p_ptr.to_d + (p_ptr.to_d / 5) * (p_ptr.skill[11] + 1)
		p_ptr.dis_to_d = p_ptr.dis_to_d + (p_ptr.dis_to_d / 5) * (p_ptr.skill[11] + 1)
	end

	-- Temporary "Beserk"
	if (p_ptr.shero > 0) then

		p_ptr.to_h = p_ptr.to_h + (p_ptr.to_h / 3) * (p_ptr.skill[11] + 1)
		p_ptr.dis_to_h = p_ptr.dis_to_h + (p_ptr.dis_to_h / 3) * (p_ptr.skill[11] + 1)
		p_ptr.to_d = p_ptr.to_d + (p_ptr.to_d / 3) * (p_ptr.skill[11] + 1)
		p_ptr.dis_to_d = p_ptr.dis_to_d + (p_ptr.dis_to_d / 3) * (p_ptr.skill[11] + 1)
		p_ptr.to_a = p_ptr.to_a - (p_ptr.to_a / 2)
		p_ptr.dis_to_a = p_ptr.dis_to_a - (p_ptr.dis_to_a / 2)
		p_ptr.to_a = p_ptr.to_a - 100
		p_ptr.dis_to_a = p_ptr.dis_to_a - 100
	end

	-- Scan the usable inventory
	for i = INVEN_WIELD, (INVEN_TOTAL - 1) do
		
		-- Skip non-objects
		if (inven(i).k_idx > 0) then

			-- Affect stats
			if (get_object_flag1(inven(i), TR1_STR)) then p_ptr.stat_add[A_STR+1] = p_ptr.stat_add[A_STR+1] + inven(i).pval end
			if (get_object_flag1(inven(i), TR1_INT)) then p_ptr.stat_add[A_INT+1] = p_ptr.stat_add[A_INT+1] + inven(i).pval end
			if (get_object_flag1(inven(i), TR1_WIS)) then p_ptr.stat_add[A_WIS+1] = p_ptr.stat_add[A_WIS+1] + inven(i).pval end
			if (get_object_flag1(inven(i), TR1_DEX)) then p_ptr.stat_add[A_DEX+1] = p_ptr.stat_add[A_DEX+1] + inven(i).pval end
			if (get_object_flag1(inven(i), TR1_CON)) then p_ptr.stat_add[A_CON+1] = p_ptr.stat_add[A_CON+1] + inven(i).pval end
			if (get_object_flag1(inven(i), TR1_CHR)) then p_ptr.stat_add[A_CHR+1] = p_ptr.stat_add[A_CHR+1] + inven(i).pval end

                	-- Affect spell power
                	if (get_object_flag1(inven(i), TR1_SPELL)) then p_ptr.to_s = p_ptr.to_s + inven(i).pval end

                	-- Affect mana capacity
                	if (get_object_flag1(inven(i), TR1_MANA)) then p_ptr.to_m = p_ptr.to_m + inven(i).pval end

			-- Affect stealth
                	if (get_object_flag1(inven(i), TR1_STEALTH)) then p_ptr.skill_bonus[7] = p_ptr.skill_bonus[7] + inven(i).pval end

			-- Affect infravision
			if (get_object_flag1(inven(i), TR1_INFRA)) then p_ptr.see_infra = p_ptr.see_infra + inven(i).pval end

			-- Affect speed
                	if (get_object_flag1(inven(i), TR1_SPEED)) then p_ptr.pspeed = p_ptr.pspeed + inven(i).pval end

			-- Affect blows
			if (get_object_flag1(inven(i), TR1_BLOWS)) then extra_blows = extra_blows + inven(i).pval end

			-- Affect invisibility
                	if (get_object_flag2(inven(i), TR2_INVIS)) then p_ptr.invis = p_ptr.invis + (inven(i).pval * 10) end

			-- Boost shots
			if (get_object_flag3(inven(i), TR3_XTRA_SHOTS)) then extra_shots = extra_shots + 1 end

			-- Various flags
			if (get_object_flag3(inven(i), TR3_AGGRAVATE)) then p_ptr.aggravate = TRUE end
			if (get_object_flag3(inven(i), TR3_TELEPORT)) then p_ptr.teleport = TRUE end
			if (get_object_flag3(inven(i), TR3_DRAIN_EXP)) then p_ptr.exp_drain = TRUE end
			if (get_object_flag3(inven(i), TR3_XTRA_MIGHT)) then p_ptr.xtra_might = TRUE end
			if (get_object_flag3(inven(i), TR3_SLOW_DIGEST)) then p_ptr.slow_digest = TRUE end
			if (get_object_flag3(inven(i), TR3_REGEN)) then p_ptr.regenerate = TRUE end
			if (get_object_flag3(inven(i), TR3_TELEPATHY)) then p_ptr.telepathy = TRUE end
                	if (get_object_flag3(inven(i), TR3_LITE)) then p_ptr.lite = TRUE end
			if (get_object_flag3(inven(i), TR3_SEE_INVIS)) then p_ptr.see_inv = TRUE end
			if (get_object_flag2(inven(i), TR2_FREE_ACT)) then p_ptr.free_act = TRUE end
			if (get_object_flag2(inven(i), TR2_HOLD_LIFE)) then p_ptr.hold_life = TRUE end
			if (get_object_flag3(inven(i), TR3_WRAITH)) then p_ptr.wraith_form = 20 end
			if (get_object_flag3(inven(i), TR3_FEATHER)) then p_ptr.ffall = TRUE end
                	if (get_object_flag4(inven(i), TR4_FLY)) then p_ptr.fly = TRUE end
                	if (get_object_flag4(inven(i), TR4_CLIMB)) then p_ptr.climb = TRUE end

			-- Resistance flags
			if (get_object_flag2(inven(i), TR2_RES_FEAR)) then p_ptr.resist_fear = TRUE end
			if (get_object_flag2(inven(i), TR2_RES_CONF)) then p_ptr.resist_conf = TRUE end
			if (get_object_flag2(inven(i), TR2_RES_BLIND)) then p_ptr.resist_blind = TRUE end

			if (get_object_flag2(inven(i), TR2_REFLECT)) then p_ptr.reflect = TRUE end
			if (get_object_flag3(inven(i), TR3_SH_FIRE)) then p_ptr.sh_fire = TRUE end
			if (get_object_flag3(inven(i), TR3_SH_ELEC)) then p_ptr.sh_elec = TRUE end

			-- Sustain flags
			if (get_object_flag2(inven(i), TR2_SUST_STR)) then p_ptr.sustain_str = TRUE end
			if (get_object_flag2(inven(i), TR2_SUST_INT)) then p_ptr.sustain_int = TRUE end
			if (get_object_flag2(inven(i), TR2_SUST_WIS)) then p_ptr.sustain_wis = TRUE end
			if (get_object_flag2(inven(i), TR2_SUST_DEX)) then p_ptr.sustain_dex = TRUE end
			if (get_object_flag2(inven(i), TR2_SUST_CON)) then p_ptr.sustain_con = TRUE end
			if (get_object_flag2(inven(i), TR2_SUST_CHR)) then p_ptr.sustain_chr = TRUE end

			-- Modify the base armor class
			p_ptr.ac = p_ptr.ac + inven(i).ac + ((inven(i).ac / 3) * p_ptr.skill[5])

			-- The base armor class is always known
			p_ptr.dis_ac = p_ptr.dis_ac + inven(i).ac + ((inven(i).ac / 3) * p_ptr.skill[5])

			-- Apply the bonuses to armor class
			p_ptr.to_a = p_ptr.to_a + inven(i).to_a

			-- Apply the mental bonuses to armor class
			p_ptr.dis_to_a = p_ptr.dis_to_a + inven(i).to_a

			-- Do not apply shooter bonuses
			if (not(i == INVEN_BOW)) then

				-- Apply the bonuses to hit/damage
				p_ptr.to_h = p_ptr.to_h + inven(i).to_h
				p_ptr.to_d = p_ptr.to_d + inven(i).to_d

				-- Apply the mental bonuses tp hit/damage, if known
				p_ptr.dis_to_h = p_ptr.dis_to_h + inven(i).to_h
				p_ptr.dis_to_d = p_ptr.dis_to_d + inven(i).to_d
			end
		end
	end

	-- Actually give the skills a value!
	calc_skills(2)

	-- Calculate stats
	calc_stats(1)

	-- Apply Agility speed bonus here.
	p_ptr.pspeed = p_ptr.pspeed + (p_ptr.skill[6] / 5)

	-- Extract the current weight (in tenth pounds)
	j = total_weight

	-- Extract the "weight limit" (in tenth pounds)
	i = weight_limit()

	-- XXX XXX XXX Apply "encumbrance" from weight
	if (j > i/2) then p_ptr.pspeed = p_ptr.pspeed - ((j - (i/2)) / (i / 10)) end

	-- Bloating slows the player down (a little)
	if (p_ptr.food >= PY_FOOD_MAX) then p_ptr.pspeed = p_ptr.pspeed - 10 end

	-- Searching slows the player down
	if (p_ptr.searching > 0) then p_ptr.pspeed = p_ptr.pspeed - 10 end

	

	-- Actual Modifier Bonuses (Un-inflate stat bonuses)
        p_ptr.to_a = p_ptr.to_a + ((p_ptr.stat_ind[A_DEX+1] - 5) / 2)

	if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1) then

		local strbonus
		local dexbonus
		local useddex

		strbonus = (p_ptr.stat_ind[A_STR+1] - 5) * 5
		useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		dexbonus = (useddex * 5)
		if (dexbonus > strbonus) then

			p_ptr.to_d = p_ptr.to_d + dexbonus
			p_ptr.dis_to_d = p_ptr.dis_to_d + dexbonus
		
		else

			p_ptr.to_d = p_ptr.to_d + strbonus
			p_ptr.dis_to_d = p_ptr.dis_to_d + strbonus
		end
        else 

		p_ptr.to_d = p_ptr.to_d + ((p_ptr.stat_ind[A_STR+1] - 5) * 5)
		p_ptr.dis_to_d = p_ptr.dis_to_d + ((p_ptr.stat_ind[A_STR+1] - 5) * 5)
	end

        p_ptr.to_h = p_ptr.to_h + ((p_ptr.stat_ind[A_DEX+1] - 5) * 10)
        p_ptr.to_h = p_ptr.to_h + (p_ptr.stat_ind[A_STR+1] / 2)
	p_ptr.to_h = p_ptr.to_h + 2

	-- Displayed Modifier Bonuses (Un-inflate stat bonuses)
        p_ptr.dis_to_a = p_ptr.dis_to_a + (p_ptr.stat_ind[A_DEX+1] - 5) / 2
        p_ptr.dis_to_h = p_ptr.dis_to_h + (p_ptr.stat_ind[A_DEX+1] - 5) * 10
        p_ptr.dis_to_h = p_ptr.dis_to_h + p_ptr.stat_ind[A_STR+1] / 2
	p_ptr.dis_to_h = p_ptr.dis_to_h + 2
	p_ptr.dis_to_h = p_ptr.dis_to_h + ((p_ptr.dis_to_h * ((p_ptr.stat_ind[A_DEX+1] - 5) * 5)) / 100)

	-- Temporary blessing
	if (p_ptr.blessed > 0) then

                p_ptr.ac = p_ptr.ac * 2
                p_ptr.dis_ac = p_ptr.dis_ac * 2
                p_ptr.to_h = p_ptr.to_h + (p_ptr.to_h * 2)
                p_ptr.dis_to_h = p_ptr.dis_to_h + (p_ptr.dis_to_h * 2)
	end

	-- Dual Wield adjustments.
	if (two_weapon_wield()) then

		local hitpenality
		local avgwgt

		avgwgt = ((inven(INVEN_WIELD).weight / 10) + (inven(INVEN_WIELD+1).weight / 10)) / 2
		if (p_ptr.skill[9] < ((avgwgt) * 3)) then

			hitpenality = (((avgwgt) * 3) - p_ptr.skill[9]) * 2
			if (hitpenality >= 0) then

				if (hitpenality > 95) then hitpenality = 95 end
				p_ptr.to_h = p_ptr.to_h - ((p_ptr.to_h * hitpenality) / 100)
				p_ptr.dis_to_h = p_ptr.dis_to_h - ((p_ptr.dis_to_h * hitpenality) / 100)
			else

				hitpenality = hitpenality * -1
				p_ptr.to_h = p_ptr.to_h - ((p_ptr.to_h * hitpenality) / 100)
				p_ptr.dis_to_h = p_ptr.dis_to_h - ((p_ptr.dis_to_h * hitpenality) / 100)
			end
		end
	end

        -- Defender's Heavy Armored Defense!
        if (p_ptr.abilities[(CLASS_DEFENDER * 10) + 1] >= 1 and (inven(INVEN_BODY).tval == TV_HARD_ARMOR or inven(INVEN_BODY).tval == TV_DRAG_ARMOR)) then

                p_ptr.to_a = p_ptr.to_a + ((inven(INVEN_BODY).ac * 3) * p_ptr.abilities[(CLASS_DEFENDER * 10) + 1])
                p_ptr.dis_to_a = p_ptr.dis_to_a + ((inven(INVEN_BODY).ac * 3) * p_ptr.abilities[(CLASS_DEFENDER * 10) + 1])
        end
	-- Fighter's Defensive Fighting
        if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 2] >= 1) then

                p_ptr.to_a = p_ptr.to_a + (p_ptr.skill[1] * p_ptr.abilities[(CLASS_FIGHTER * 10) + 2])
                p_ptr.dis_to_a = p_ptr.dis_to_a + (p_ptr.skill[1] * p_ptr.abilities[(CLASS_FIGHTER * 10) + 2])
        end

        -- Bonus to_ac never go below 0
        if (p_ptr.to_a < 0) then p_ptr.to_a = 0 end
        if (p_ptr.dis_to_a < 0) then p_ptr.dis_to_a = 0 end

        -- Base AC bonus for the Unarmored Combat monk ability!
        if (p_ptr.abilities[(CLASS_MONK * 10) + 1] >= 1 and (inven(INVEN_BODY).tval == 0)) then

                p_ptr.ac = p_ptr.ac + (p_ptr.abilities[(CLASS_MONK * 10) + 1] * 4)
                p_ptr.dis_ac = p_ptr.dis_ac + (p_ptr.abilities[(CLASS_MONK * 10) + 1] * 4)
        end
        -- Base AC bonus for the Monstrous Defense monster mage ability!
        if (p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 7] >= 1 and p_ptr.body_monster > 0) then

                p_ptr.ac = p_ptr.ac + ((m_race(p_ptr.body_monster).ac * (p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 7] * 10)) / 100)
                p_ptr.dis_ac = p_ptr.dis_ac + ((m_race(p_ptr.body_monster).ac * (p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 7] * 10)) / 100)
        end
        -- Base AC bonus for the Monsters Improved Defense!
        if (p_ptr.abilities[(CLASS_APPRENTICE * 10) + 2] >= 1 and p_ptr.body_monster > 0) then

                p_ptr.ac = p_ptr.ac + ((m_race(p_ptr.body_monster).ac * (p_ptr.abilities[(CLASS_APPRENTICE * 10) + 2] * 10)) / 100)
                p_ptr.dis_ac = p_ptr.dis_ac + ((m_race(p_ptr.body_monster).ac * (p_ptr.abilities[(CLASS_APPRENTICE * 10) + 2] * 10)) / 100)
                p_ptr.ac = p_ptr.ac + (2 * p_ptr.abilities[(CLASS_APPRENTICE * 10) + 2])
                p_ptr.dis_ac = p_ptr.dis_ac + (2 * p_ptr.abilities[(CLASS_APPRENTICE * 10) + 2])
                p_ptr.to_a = p_ptr.to_a + (10 * p_ptr.abilities[(CLASS_APPRENTICE * 10) + 2])
                p_ptr.dis_to_a = p_ptr.dis_to_a + (10 * p_ptr.abilities[(CLASS_APPRENTICE * 10) + 2])
        end
	-- Base AC from Martial Arts skill
	if (inven(INVEN_BODY).tval == 0) then

        	p_ptr.ac = p_ptr.ac + (p_ptr.skill[19] * 2)
        	p_ptr.dis_ac = p_ptr.dis_ac + (p_ptr.skill[19] * 2)
        end


        -- Agility armor class modifier
        p_ptr.to_a = p_ptr.to_a + ((p_ptr.ac / 10) * p_ptr.skill[6])
        p_ptr.dis_to_a = p_ptr.dis_to_a + ((p_ptr.ac / 10) * p_ptr.skill[6])
        p_ptr.to_a = p_ptr.to_a + (p_ptr.skill[6] * 3)
        p_ptr.dis_to_a = p_ptr.dis_to_a + (p_ptr.skill[6] * 3)

        -- Base AC never go below 0.
        if (p_ptr.ac < 0) then p_ptr.ac = 0 end
        if (p_ptr.dis_ac < 0) then p_ptr.dis_ac = 0 end

        -- If your shooting skill is 40+, get 1 extra shot!
        if (p_ptr.skill[3] >= 40) then

                p_ptr.num_fire = p_ptr.num_fire + 1
        end

	

	-- Obtain the "hold" value
        hold = max_carry() * 2

        -- Take note of required "tval" for missiles
	if (inven(INVEN_BOW).tval > 0) then

		if (inven(INVEN_BOW).sval == 2) then p_ptr.tval_ammo = TV_SHOT end
		if (inven(INVEN_BOW).sval == 12) then p_ptr.tval_ammo = TV_ARROW end
		if (inven(INVEN_BOW).sval == 13) then p_ptr.tval_ammo = TV_ARROW end
		if (inven(INVEN_BOW).sval == 14) then p_ptr.tval_ammo = TV_ARROW end
		if (inven(INVEN_BOW).sval == 15) then p_ptr.tval_ammo = TV_ARROW end
		if (inven(INVEN_BOW).sval == 16) then p_ptr.tval_ammo = TV_ARROW end
		if (inven(INVEN_BOW).sval == 23) then p_ptr.tval_ammo = TV_BOLT end
		if (inven(INVEN_BOW).sval == 24) then p_ptr.tval_ammo = TV_BOLT end
		if (inven(INVEN_BOW).sval == 25) then p_ptr.tval_ammo = TV_BOLT end
		if (inven(INVEN_BOW).sval == 26) then p_ptr.tval_ammo = TV_BOLT end

		-- Compute "extra shots" if needed
		if (inven(INVEN_BOW).k_idx > 0) then

			-- Add in the "bonus shots"
			p_ptr.num_fire = p_ptr.num_fire + extra_shots

			-- Require at least one shot
			if (p_ptr.num_fire < 1) then p_ptr.num_fire = 1 end
		end
	end

	-- Normal weapons
	if (inven(INVEN_WIELD).k_idx > 0) then

                local avg

                -- The average of strength and dexterity. Both of them
                -- can give you extra blows equally, so...
                avg = (p_ptr.stat_ind[A_STR+1] + p_ptr.stat_ind[A_DEX+1]) / 2

                -- Calculate the blows
                -- The average of str and dex divided by the weapon's weight
                if (inven(INVEN_WIELD).weight < 10) then
			p_ptr.num_blow = p_ptr.num_blow + avg
                else
			p_ptr.num_blow = p_ptr.num_blow + (avg / (inven(INVEN_WIELD).weight / 10))
		end

		-- Add in the "bonus blows"
		p_ptr.num_blow = p_ptr.num_blow + extra_blows

		-- Dual Wielding reduces the number of blows if weapons are
		-- too heavy...
		-- It takes a skill of 3 times the weapon's weight to properly
		-- wield two of them.
		if (two_weapon_wield()) then

			local blowpenality
			blowpenality = 0
			if (p_ptr.skill[9] < ((inven(INVEN_WIELD).weight / 10) * 3)) then

				blowpenality = (((inven(INVEN_WIELD).weight / 10) * 3) - p_ptr.skill[9]) / 5
				-- You never actually GAIN blows!
				if (blowpenality < 0) then blowpenality = 0 end
			end
			p_ptr.num_blow = p_ptr.num_blow - blowpenality
		end

		-- Require at least one blow
		if (p_ptr.num_blow < 1) then p_ptr.num_blow = 1 end

        -- We're bare-handed...let's just calculate the blows using
        -- Martial Arts skill...
        elseif (unarmed()) then

                local avg

                -- The average of strength and dexterity. Both of them
                -- can give you extra blows equally, so...
                avg = (p_ptr.stat_ind[A_STR+1] + p_ptr.stat_ind[A_DEX+1]) / 2

                -- Calculate the blows
                p_ptr.num_blow = p_ptr.num_blow + (avg / 10)
		p_ptr.num_blow2 = p_ptr.num_blow2 + (avg / 10)

		-- Add in the "bonus blows"
		p_ptr.num_blow = p_ptr.num_blow + extra_blows
		p_ptr.num_blow2 = p_ptr.num_blow2 + extra_blows

                -- Add in some bonuses from the Martial Arts skill...
                p_ptr.num_blow = p_ptr.num_blow + (p_ptr.skill[19] / 10)
		p_ptr.num_blow2 = p_ptr.num_blow2 + (p_ptr.skill[19] / 10)

		-- Fighting skill can also help.
		p_ptr.num_blow = p_ptr.num_blow + (p_ptr.skill[1] / 20)
		p_ptr.num_blow2 = p_ptr.num_blow2 + (p_ptr.skill[1] / 20)

		-- We can also gain blows from Unarmed Fighting ability!
		if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 5] >= 1) then

			p_ptr.num_blow = p_ptr.num_blow + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 5] / 5)
			p_ptr.num_blow2 = p_ptr.num_blow2 + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 5] / 5)
		end

		-- Require at least one blow
		if (p_ptr.num_blow < 1) then p_ptr.num_blow = 1 end
        end

	-- The second weapon.
	if (inven(INVEN_WIELD+1).k_idx > 0) then

                local avg

		-- The average of strength and dexterity. Both of them
                -- can give you extra blows equally, so...
                avg = (p_ptr.stat_ind[A_STR+1] + p_ptr.stat_ind[A_DEX+1]) / 2

                -- Calculate the blows
                -- The average of str and dex divided by the weapon's weight
                if (inven(INVEN_WIELD+1).weight < 10) then
			p_ptr.num_blow = p_ptr.num_blow2 + avg
                else
			p_ptr.num_blow = p_ptr.num_blow2 + (avg / (inven(INVEN_WIELD+1).weight / 10))
		end

		-- Add in the "bonus blows"
		p_ptr.num_blow2 = p_ptr.num_blow2 + extra_blows


		-- For the second weapon, unless we have a Dual Wield skill of 20+
		-- we only get half the number of blows.
		if (p_ptr.skill[9] < 20) then p_ptr.num_blow2 = (p_ptr.num_blow2 / 2) end

		-- Dual Wielding reduces the number of blows if weapons are
		-- too heavy...
		-- It takes a skill of 3 times the weapon's weight to properly
		-- wield two of them.
		if (two_weapon_wield()) then

			local blowpenality
			blowpenality = 0
			if (p_ptr.skill[9] < ((inven(INVEN_WIELD+1).weight / 10) * 3)) then

				blowpenality = (((inven(INVEN_WIELD+1).weight / 10) * 3) - p_ptr.skill[9]) / 5
				-- You never actually GAIN blows!
				if (blowpenality < 0) then blowpenality = 0 end
			end
			p_ptr.num_blow2 = p_ptr.num_blow2 - blowpenality
		end

		-- Require at least one blow
		if (p_ptr.num_blow2 < 1) then p_ptr.num_blow2 = 1 end
	end

        -- When fighting bare handed, use the martial arts skill to boost
        -- the damages. Note that claws are NOT considered martial arts!!
        if (unarmed()) then

                if (p_ptr.body_monster > 0) then

                        if (inven(INVEN_ARM).tval == 0 or inven(INVEN_ARM).tval == TV_ARM_BAND) then

                                if (inven(INVEN_BODY).tval == TV_SOFT_ARMOR or inven(INVEN_BODY).tval == TV_HARD_ARMOR or inven(INVEN_BODY).tval == TV_DRAG_ARMOR) then

                                        p_ptr.to_h = p_ptr.to_h + p_ptr.skill[19]
                                        p_ptr.dis_to_h = p_ptr.dis_to_h + p_ptr.skill[19]
                                        
                                else

                                        p_ptr.to_h = p_ptr.to_h + (2 * p_ptr.skill[19])
                                        p_ptr.dis_to_h = p_ptr.dis_to_h + (2 * p_ptr.skill[19])
                                        
                                end
                        end

                else

                        p_ptr.to_h = p_ptr.to_h + (2 * p_ptr.skill[19])
                        p_ptr.dis_to_h = p_ptr.dis_to_h + (2 * p_ptr.skill[19])
                end
        end

        if ((inven(INVEN_WIELD).tval == TV_SWORD_DEVASTATION) or (inven(INVEN_WIELD+1).tval == TV_SWORD_DEVASTATION)) then

                  p_ptr.mhp = 1;
		  if (p_ptr.chp > p_ptr.mhp) then p_ptr.chp = 1 end
	end
        
        -- Battle Skill Warrior's ability!
        if (p_ptr.abilities[(CLASS_WARRIOR * 10) + 4] >= 1) then

                p_ptr.to_h = p_ptr.to_h + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 4] * 10)
                p_ptr.dis_to_h = p_ptr.dis_to_h + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 4] * 10)
                p_ptr.to_d = p_ptr.to_d + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 4] * 10)
                p_ptr.dis_to_d = p_ptr.dis_to_d + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 4] * 10)
        end
        -- Hardiness Warrior's ability!
        if (p_ptr.abilities[(CLASS_WARRIOR * 10) + 6] >= 1) then

                p_ptr.to_a = p_ptr.to_a + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 6] * 15)
                p_ptr.dis_to_a = p_ptr.dis_to_a + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 6] * 15)
        end
        -- Rogue's Evasion ability!
        if (p_ptr.abilities[(CLASS_ROGUE * 10) + 5] >= 1) then

                p_ptr.to_a = p_ptr.to_a + (p_ptr.abilities[(CLASS_ROGUE * 10) + 5] * 10)
                p_ptr.dis_to_a = p_ptr.dis_to_a + (p_ptr.abilities[(CLASS_ROGUE * 10) + 5] * 10)
        end
        -- Ranger's Forestry ability!
        if ((p_ptr.abilities[(CLASS_RANGER * 10) + 2] >= 1) and (standing_on_forest())) then

                p_ptr.to_h = p_ptr.to_h + (p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 20)
                p_ptr.dis_to_h = p_ptr.dis_to_h + (p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 20)
                p_ptr.to_d = p_ptr.to_d + (p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 20)
                p_ptr.dis_to_d = p_ptr.dis_to_d + (p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 20)
                p_ptr.to_a = p_ptr.to_a + (p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 20)
                p_ptr.dis_to_a = p_ptr.dis_to_a + (p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 20)
                p_ptr.pspeed = p_ptr.pspeed + ((p_ptr.abilities[(CLASS_RANGER * 10) + 2] / 3) + 1)
                p_ptr.skill_bonus[7] = p_ptr.skill_bonus[7] + (p_ptr.abilities[(CLASS_RANGER * 10) + 2])              
        end
        -- Monk Speed!
        if (p_ptr.abilities[(CLASS_MONK * 10) + 8] >= 1) then

                if (not(inven(INVEN_BODY).tval == TV_SOFT_ARMOR) or not(inven(INVEN_BODY).tval == TV_HARD_ARMOR) or not(inven(INVEN_BODY).tval == TV_DRAG_ARMOR)) then

                        p_ptr.pspeed = p_ptr.pspeed + ((p_ptr.abilities[(CLASS_MONK * 10) + 8] / 2) + 1)
                end
        end
        -- Defender's Shield Fighting!
        if ((p_ptr.abilities[(CLASS_DEFENDER * 10) + 4] >= 1) and (shield_has())) then

                p_ptr.to_h = p_ptr.to_h + (p_ptr.abilities[(CLASS_DEFENDER * 10) + 4] * 20)
                p_ptr.dis_to_h = p_ptr.dis_to_h + (p_ptr.abilities[(CLASS_DEFENDER * 10) + 4] * 20)
                p_ptr.to_d = p_ptr.to_d + (p_ptr.abilities[(CLASS_DEFENDER * 10) + 4] * 20)
                p_ptr.dis_to_d = p_ptr.dis_to_d + (p_ptr.abilities[(CLASS_DEFENDER * 10) + 4] * 20)
                p_ptr.to_a = p_ptr.to_a + (p_ptr.abilities[(CLASS_DEFENDER * 10) + 4] * 50)
                p_ptr.dis_to_a = p_ptr.dis_to_a + (p_ptr.abilities[(CLASS_DEFENDER * 10) + 4] * 50)
        end
        -- Shadow Stalker's Shadow Run!!
        if (p_ptr.abilities[(CLASS_SHADOW * 10) + 5] >= 1) then
 
                if (p_ptr.tim_invisible > 0) then

			local speedamount
                        speedamount = p_ptr.abilities[(CLASS_SHADOW * 10) + 5]
                        if (speedamount > p_ptr.tim_inv_pow) then speedamount = p_ptr.tim_inv_pow end
                        p_ptr.pspeed = p_ptr.pspeed + speedamount
                end
        end
        -- Shadow Stalker's One With Shadows!!
        if (p_ptr.abilities[(CLASS_SHADOW * 10) + 8] >= 1) then

                if (p_ptr.tim_invisible > 0) then

			local acbonus
                        acbonus = p_ptr.skill[7]
                        acbonus = acbonus + (acbonus * ((p_ptr.abilities[(CLASS_SHADOW * 10) + 8] * 10) / 100))
                        p_ptr.to_a = p_ptr.to_a + acbonus
                        p_ptr.dis_to_a = p_ptr.dis_to_a + acbonus
                end
        end

        -- Spell multiplier has a minimum of 1
        if (p_ptr.to_s == 0) then p_ptr.to_s = 1 end

	--(adelie) Combine the armor and speed checks to only run lua_update_stuff once if needed! 
	--(adelie) Note that this check must come after all possible modifiers to speed, to_a or ac!
	if ((not(p_ptr.pspeed == old_speed)) or (not(p_ptr.dis_to_a == old_dis_to_a)) or (not(p_ptr.dis_ac == old_dis_ac))) then
		lua_update_stuff()
	end
end

-- Event handlers
add_event_handler("calc_hitpoints", calc_hitpoints)
add_event_handler("calc_mana", calc_mana)
add_event_handler("calc_bonuses", calc_bonuses)
