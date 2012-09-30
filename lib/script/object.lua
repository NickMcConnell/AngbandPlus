-- Use objects


-- Larger values makes using devices harder
USE_DEVICE = 3


function eat_food(object)
	local ident = FALSE

	if object.sval == SV_FOOD_RATION or
	   object.sval == SV_FOOD_BISCUIT or
	   object.sval == SV_FOOD_JERKY or
	   object.sval == SV_FOOD_SLIME_MOLD or
	   object.sval == SV_FOOD_PINT_OF_ALE or
	   object.sval == SV_FOOD_PINT_OF_WINE then
		msgf("That tastes good.")
		ident = TRUE
	elseif object.sval == SV_FOOD_WAYBREAD then
		msgf("That tastes good.")
		set_poisoned(0)
		hp_player(damroll(4, 8))
		ident = TRUE
	elseif object.sval == SV_FOOD_RESTORING then
		if do_res_stat(A_STR) then ident = TRUE end
		if do_res_stat(A_INT) then ident = TRUE end
		if do_res_stat(A_WIS) then ident = TRUE end
		if do_res_stat(A_DEX) then ident = TRUE end
		if do_res_stat(A_CON) then ident = TRUE end
		if do_res_stat(A_CHR) then ident = TRUE end
	elseif object.sval == SV_FOOD_RESTORE_CON then
		if do_res_stat(A_CON) then ident = TRUE end
	elseif object.sval == SV_FOOD_RESTORE_STR then
		if do_res_stat(A_STR) then ident = TRUE end
	elseif object.sval == SV_FOOD_CURE_SERIOUS then
		if hp_player(damroll(4, 8)) then ident = TRUE end
	elseif object.sval == SV_FOOD_CURE_CONFUSION then
		if set_confused(0) then ident = TRUE end
	elseif object.sval == SV_FOOD_CURE_PARANOIA then
		if set_afraid(0) then ident = TRUE end
	elseif object.sval == SV_FOOD_CURE_BLINDNESS then
		if set_blind(0) then ident = TRUE end
	elseif object.sval == SV_FOOD_CURE_POISON then
		if set_poisoned(0) then ident = TRUE end
	elseif object.sval == SV_FOOD_DISEASE then
		take_hit(damroll(10, 10), "poisonous food")
		do_dec_stat(A_STR)
		ident = TRUE
	elseif object.sval == SV_FOOD_UNHEALTH then
		take_hit(damroll(10, 10), "poisonous food")
		do_dec_stat(A_CON)
		ident = TRUE
	elseif object.sval == SV_FOOD_NAIVETY then
		take_hit(damroll(8, 8), "poisonous food")
		do_dec_stat(A_WIS)
		ident = TRUE
	elseif object.sval == SV_FOOD_STUPIDITY then
		take_hit(damroll(8, 8), "poisonous food")
		do_dec_stat(A_INT)
		ident = TRUE
	elseif object.sval == SV_FOOD_SICKNESS then
		take_hit(damroll(6, 6), "poisonous food")
		do_dec_stat(A_CON)
		ident = TRUE
	elseif object.sval == SV_FOOD_WEAKNESS then
		take_hit(damroll(6, 6), "poisonous food")
		do_dec_stat(A_STR)
		ident = TRUE
	elseif object.sval == SV_FOOD_PARALYSIS then
		if not player.free_act then
			if set_paralyzed(player.paralyzed + rand_int(10) + 10) then
				ident = TRUE
			end
		end
	elseif object.sval == SV_FOOD_HALLUCINATION then
		if not player.resist_chaos then
			if set_image(player.image + rand_int(250) + 250) then
				ident = TRUE
			end
		end
	elseif object.sval == SV_FOOD_CONFUSION then
		if  not player.resist_confu then
			if set_confused(player.confused + rand_int(10) + 10) then
				ident = TRUE
			end
		end
	elseif object.sval == SV_FOOD_PARANOIA then
		if not player.resist_fear then
			if set_afraid(player.afraid + rand_int(10) + 10) then
				ident = TRUE
			end
		end
	elseif object.sval == SV_FOOD_POISON then
		if not (player.resist_pois or (player.oppose_pois > 0)) then
			if set_poisoned(player.poisoned + rand_int(10) + 10) then
				ident = TRUE
			end
		end
	elseif object.sval == SV_FOOD_BLINDNESS then
		if not player.resist_blind then
			if set_blind(player.blind + rand_int(200) + 200) then
				ident = TRUE
			end
		end
	end

	return ident, TRUE
end


function quaff_potion(object)
	local ident = FALSE

	if object.sval == SV_POTION_WATER or
	   object.sval == SV_POTION_APPLE_JUICE or
	   object.sval == SV_POTION_SLIME_MOLD then
		msgf("You feel less thirsty.")
		ident = TRUE
	elseif object.sval == SV_POTION_SLOWNESS then
		if set_slow(player.slow + rand_range(15, 40)) then ident = TRUE end
	elseif object.sval == SV_POTION_SALT_WATER then
		msgf("The potion makes you vomit!")
		if player.food > PY_FOOD_STARVE - 1 then
			set_food(PY_FOOD_STARVE - 1)
		end
		set_poisoned(0)
		set_paralyzed(player.paralyzed + 4)
		ident = TRUE
	elseif object.sval == SV_POTION_POISON then
		if not (player.resist_pois or (player.oppose_pois > 0)) then
			if set_poisoned(player.poisoned + rand_range(10, 25)) then
				ident = TRUE
			end
		end
	elseif object.sval == SV_POTION_BLINDNESS then
		if not player.resist_blind then
			if set_blind(player.blind + rand_range(100, 200)) then
				ident = TRUE
			end
		end
	-- Booze
	elseif object.sval == SV_POTION_CONFUSION then
		if not player.resist_confu then
			if set_confused(player.confused + rand_range(15, 35)) then
				ident = TRUE
			end
		end

		if not player.resist_chaos then
			if one_in_(2) then
				if set_image(player.image + rand_range(150, 300)) then
					ident = TRUE
				end
			end
			if one_in_(13) then
				ident = TRUE
				if one_in_(3) then
					lose_all_info()
				else
					wiz_dark()
				end
				teleport_player(100)
				wiz_dark()
				msgf("You wake up somewhere with a sore head...")
				msgf("You can't remember a thing, or how you got here!")
			end
		end
	elseif object.sval == SV_POTION_SLEEP then
		if not player.free_act then
			msgf("You fall asleep.")

			if ironman_nightmare then
				msgf("A horrible vision enters your mind.")

				-- Pick a nightmare
				get_mon_num_prep(get_nightmare, NULL)

				-- Have some nightmares
				have_nightmare(get_mon_num(MAX_DEPTH))

				-- Remove the monster restriction
				get_mon_num_prep(NULL, NULL)
			end

			if set_paralyzed(player.paralyzed + rand_range(4, 8)) then
				ident = TRUE
			end
		end
	elseif object.sval == SV_POTION_LOSE_MEMORIES then
		if not player.hold_life and (player.exp > 0) then
			msgf("You feel your memories fade.")

			lose_exp(player.exp / 4)
			ident = TRUE
		end
	elseif object.sval == SV_POTION_RUINATION then
		msgf("Your nerves and muscles feel weak and lifeless!")
		take_hit(damroll(10, 10), "a potion of Ruination")
		dec_stat(A_DEX, 25, TRUE)
		dec_stat(A_WIS, 25, TRUE)
		dec_stat(A_CON, 25, TRUE)
		dec_stat(A_STR, 25, TRUE)
		dec_stat(A_CHR, 25, TRUE)
		dec_stat(A_INT, 25, TRUE)
		ident = TRUE
	elseif object.sval == SV_POTION_DEC_STR then
		if do_dec_stat(A_STR) then ident = TRUE end
	elseif object.sval == SV_POTION_DEC_INT then
		if do_dec_stat(A_INT) then ident = TRUE end
	elseif object.sval == SV_POTION_DEC_WIS then
		if do_dec_stat(A_WIS) then ident = TRUE end
	elseif object.sval == SV_POTION_DEC_DEX then
		if do_dec_stat(A_DEX) then ident = TRUE end
	elseif object.sval == SV_POTION_DEC_CON then
		if do_dec_stat(A_CON) then ident = TRUE end
	elseif object.sval == SV_POTION_DEC_CHR then
		if do_dec_stat(A_CHR) then ident = TRUE end
	elseif object.sval == SV_POTION_DETONATIONS then
		msgf("Massive explosions rupture your body!")
		take_hit(damroll(50, 20), "a potion of Detonation")
		set_stun(player.stun + 75)
		set_cut(player.cut + 5000)
		ident = TRUE
	elseif object.sval == SV_POTION_DEATH then
		msgf("A feeling of Death flows through your body.")
		take_hit(5000, "a potion of Death")
		ident = TRUE
	elseif object.sval == SV_POTION_INFRAVISION then
		if set_tim_infra(player.tim_infra + rand_range(100, 200)) then
			ident = TRUE
		end
	elseif object.sval == SV_POTION_DETECT_INVIS then
		if set_tim_invis(player.tim_invis + rand_range(12, 24)) then
			ident = TRUE
		end
	elseif object.sval == SV_POTION_SLOW_POISON then
		if set_poisoned(player.poisoned / 2) then ident = TRUE end
	elseif object.sval == SV_POTION_CURE_POISON then
		if set_poisoned(0) then ident = TRUE end
	elseif object.sval == SV_POTION_BOLDNESS then
		if set_afraid(0) then ident = TRUE end
	elseif object.sval == SV_POTION_SPEED then
		if player.fast == 0 then
			if set_fast(rand_range(15, 40)) then ident = TRUE end
		else
			set_fast(player.fast + 5)
		end
	elseif object.sval == SV_POTION_RESIST_HEAT then
		if set_oppose_fire(player.oppose_fire + rand_range(10, 20)) then
			ident = TRUE
		end
	elseif object.sval == SV_POTION_RESIST_COLD then
		if set_oppose_cold(player.oppose_cold + rand_range(10, 20)) then
			ident = TRUE
		end
	elseif object.sval == SV_POTION_HEROISM then
		if set_afraid(0) then ident = TRUE end
		if set_hero(player.hero + rand_range(25, 50)) then ident = TRUE end
		if hp_player(10)then ident = TRUE end
	elseif object.sval == SV_POTION_BERSERK_STRENGTH then
		if set_afraid(0) then ident = TRUE end
		if set_shero(player.shero + rand_range(25, 50)) then ident = TRUE end
		if hp_player(30) then ident = TRUE end
	elseif object.sval == SV_POTION_CURE_LIGHT then
		if hp_player(38) then ident = TRUE end
		if set_blind(0) then ident = TRUE end
		if set_cut(player.cut - 10) then ident = TRUE end
	elseif object.sval == SV_POTION_CURE_SERIOUS then
		if hp_player(75) then ident = TRUE end
		if set_blind(0) then ident = TRUE end
		if set_confused(0) then ident = TRUE end
		if set_cut((player.cut / 2) - 50) then ident = TRUE end
	elseif object.sval == SV_POTION_CURE_CRITICAL then
		if hp_player(150) then ident = TRUE end
		if set_blind(0) then ident = TRUE end
		if set_confused(0) then ident = TRUE end
		if set_poisoned(0) then ident = TRUE end
		if set_stun(0) then ident = TRUE end
		if set_cut(0) then ident = TRUE end
	elseif object.sval == SV_POTION_HEALING then
		if hp_player(300) then ident = TRUE end
		if set_blind(0) then ident = TRUE end
		if set_confused(0) then ident = TRUE end
		if set_poisoned(0) then ident = TRUE end
		if set_stun(0) then ident = TRUE end
		if set_cut(0) then ident = TRUE end
	elseif object.sval == SV_POTION_STAR_HEALING then
		if hp_player(1200) then ident = TRUE end
		if set_blind(0) then ident = TRUE end
		if set_confused(0) then ident = TRUE end
		if set_poisoned(0) then ident = TRUE end
		if set_stun(0) then ident = TRUE end
		if set_cut(0) then ident = TRUE end
	elseif object.sval == SV_POTION_LIFE then
		msgf("You feel life flow through your body!")
		restore_level()
		set_poisoned(0)
		set_blind(0)
		set_confused(0)
		set_image(0)
		set_stun(0)
		set_cut(0)
		do_res_stat(A_STR)
		do_res_stat(A_CON)
		do_res_stat(A_DEX)
		do_res_stat(A_WIS)
		do_res_stat(A_INT)
		do_res_stat(A_CHR)

		-- Recalculate max. hitpoints
		update_stuff()

		hp_player(5000)
		ident = TRUE
	elseif object.sval == SV_POTION_RESTORE_MANA then
		if player.csp < player.msp then
			player.csp = player.msp
			player.csp_frac = 0
			msgf("Your feel your head clear.")
			player.redraw = bOr(player.redraw, PR_MANA)
			player.window = bOr(player.window, PW_PLAYER)
			player.window = bOr(player.window, PW_SPELL)
			ident = TRUE
		end
	elseif object.sval == SV_POTION_RESTORE_EXP then
		if restore_level() then ident = TRUE end
	elseif object.sval == SV_POTION_RES_STR then
		if do_res_stat(A_STR) then ident = TRUE end
	elseif object.sval == SV_POTION_RES_INT then
		if do_res_stat(A_INT) then ident = TRUE end
	elseif object.sval == SV_POTION_RES_WIS then
		if do_res_stat(A_WIS) then ident = TRUE end
	elseif object.sval == SV_POTION_RES_DEX then
		if do_res_stat(A_DEX) then ident = TRUE end
	elseif object.sval == SV_POTION_RES_CON then
		if do_res_stat(A_CON) then ident = TRUE end
	elseif object.sval == SV_POTION_RES_CHR then
		if do_res_stat(A_CHR) then ident = TRUE end
	elseif object.sval == SV_POTION_INC_STR then
		if do_inc_stat(A_STR) then ident = TRUE end
	elseif object.sval == SV_POTION_INC_INT then
		if do_inc_stat(A_INT) then ident = TRUE end
	elseif object.sval == SV_POTION_INC_WIS then
		if do_inc_stat(A_WIS) then ident = TRUE end
	elseif object.sval == SV_POTION_INC_DEX then
		if do_inc_stat(A_DEX) then ident = TRUE end
	elseif object.sval == SV_POTION_INC_CON then
		if do_inc_stat(A_CON) then ident = TRUE end
	elseif object.sval == SV_POTION_INC_CHR then
		if do_inc_stat(A_CHR) then ident = TRUE end
	elseif object.sval == SV_POTION_AUGMENTATION then
		if do_inc_stat(A_STR) then ident = TRUE end
		if do_inc_stat(A_INT) then ident = TRUE end
		if do_inc_stat(A_WIS) then ident = TRUE end
		if do_inc_stat(A_DEX) then ident = TRUE end
		if do_inc_stat(A_CON) then ident = TRUE end
		if do_inc_stat(A_CHR) then ident = TRUE end
	elseif object.sval == SV_POTION_ENLIGHTENMENT then
		msgf("An image of your surroundings forms in your mind...")
		wiz_lite()
		ident = TRUE
	elseif object.sval == SV_POTION_STAR_ENLIGHTENMENT then
		msgf("You begin to feel more enlightened...")
		message_flush()
		wiz_lite()
		do_inc_stat(A_INT)
		do_inc_stat(A_WIS)
		detect_traps()
		detect_doors()
		detect_stairs()
		detect_treasure()
		detect_objects_gold()
		detect_objects_normal()
		identify_pack()
		self_knowledge()
		ident = TRUE
	elseif object.sval == SV_POTION_SELF_KNOWLEDGE then
		msgf("You begin to know yourself a little better...")
		message_flush()
		self_knowledge()
		ident = TRUE
	elseif object.sval == SV_POTION_EXPERIENCE then
		if player.exp < PY_MAX_EXP then
			local ee = (player.exp / 2) + 10
			if ee > 100000 then ee = 100000 end
			msgf("You feel more experienced.")
			gain_exp(ee)
			ident = TRUE
		end
	elseif object.sval == SV_POTION_RESISTANCE then
		set_oppose_acid(player.oppose_acid + rand_range(20, 40))
		set_oppose_elec(player.oppose_elec + rand_range(20, 40))
		set_oppose_fire(player.oppose_fire + rand_range(20, 40))
		set_oppose_cold(player.oppose_cold + rand_range(20, 40))
		set_oppose_pois(player.oppose_pois + rand_range(20, 40))
		ident = TRUE
	elseif object.sval == SV_POTION_CURING then
		if hp_player(150) then ident = TRUE end
		if set_blind(0) then ident = TRUE end
		if set_poisoned(0) then ident = TRUE end
		if set_confused(0) then ident = TRUE end
		if set_stun(0) then ident = TRUE end
		if set_cut(0) then ident = TRUE end
		if set_image(0) then ident = TRUE end
	elseif object.sval == SV_POTION_INVULNERABILITY then
		set_invuln(player.invuln + rand_range(7, 14))
		ident = TRUE
	elseif object.sval == SV_POTION_NEW_LIFE then
		do_cmd_rerate()
		if (player.muta1 ~= 0) or (player.muta2 ~= 0) or (player.muta3 ~= 0) then
			msgf("You are cured of all mutations.")
			player.muta1 = 0
			player.muta2 = 0
			player.muta3 = 0
			player.update = bOr(player.update, PU_BONUS)
			handle_stuff()
		end
		ident = TRUE
	end

	return ident, TRUE
end


function read_scroll(object)
	local ident = FALSE
	local used_up = TRUE

	if object.sval == SV_SCROLL_DARKNESS then
		if not player.resist_blind and not player.resist_dark then
			set_blind(player.blind + rand_range(3, 8))
		end
		if unlite_area(10, 3) then ident = TRUE end
	elseif object.sval == SV_SCROLL_AGGRAVATE_MONSTER then
		msgf("There is a high pitched humming noise.")
		aggravate_monsters(0)
		ident = TRUE
	elseif object.sval == SV_SCROLL_CURSE_ARMOR then
		if curse_armor() then ident = TRUE end
	elseif object.sval == SV_SCROLL_CURSE_WEAPON then
		if curse_weapon() then ident = TRUE end
	elseif object.sval == SV_SCROLL_SUMMON_MONSTER then
		for k = 0, randint1(3) do
			if summon_specific(0, player.px, player.py, player.depth, 0, TRUE, FALSE, FALSE) then
				ident = TRUE
			end
		end
	elseif object.sval == SV_SCROLL_SUMMON_UNDEAD then
		for k = 0, randint1(3) do
			if summon_specific(0, player.px, player.py, player.depth, SUMMON_UNDEAD, TRUE, FALSE, FALSE) then
				ident = TRUE
			end
		end
	elseif object.sval == SV_SCROLL_TRAP_CREATION then
		if trap_creation() then ident = TRUE end
	elseif object.sval == SV_SCROLL_PHASE_DOOR then
		teleport_player(10)
		ident = TRUE
	elseif object.sval == SV_SCROLL_TELEPORT then
		teleport_player(100)
		ident = TRUE
	elseif object.sval == SV_SCROLL_TELEPORT_LEVEL then
		teleport_player_level()
		ident = TRUE
	elseif object.sval == SV_SCROLL_WORD_OF_RECALL then
		word_of_recall()
		ident = TRUE
	elseif object.sval == SV_SCROLL_IDENTIFY then
		ident = TRUE
		if not ident_spell() then used_up = FALSE end
	elseif object.sval == SV_SCROLL_STAR_IDENTIFY then
		ident = TRUE
		if not identify_fully() then used_up = FALSE end
	elseif object.sval == SV_SCROLL_REMOVE_CURSE then
		if remove_curse() then
			msgf("You feel as if someone is watching over you.")
			ident = TRUE
		end
	elseif object.sval == SV_SCROLL_STAR_REMOVE_CURSE then
		remove_all_curse()
		ident = TRUE
	elseif object.sval == SV_SCROLL_ENCHANT_ARMOR then
		ident = TRUE
		if not enchant_spell(0, 0, 1) then used_up = FALSE end
	elseif object.sval == SV_SCROLL_ENCHANT_WEAPON_TO_HIT then
		if not enchant_spell(1, 0, 0) then used_up = FALSE end
		ident = TRUE
	elseif object.sval == SV_SCROLL_ENCHANT_WEAPON_TO_DAM then
		if not enchant_spell(0, 1, 0) then used_up = FALSE end
		ident = TRUE
	elseif object.sval == SV_SCROLL_STAR_ENCHANT_ARMOR then
		if not enchant_spell(0, 0, rand_range(2, 7)) then used_up = FALSE end
		ident = TRUE
	elseif object.sval == SV_SCROLL_STAR_ENCHANT_WEAPON then
		if not enchant_spell(randint1(5), randint1(5), 0) then used_up = FALSE end
		ident = TRUE
	elseif object.sval == SV_SCROLL_RECHARGING then
		if not recharge(130) then used_up = FALSE end
		ident = TRUE
	elseif object.sval == SV_SCROLL_MUNDANITY then
		ident = TRUE
		if not mundane_spell() then used_up = FALSE end
	elseif object.sval == SV_SCROLL_LIGHT then
		if lite_area(damroll(2, 8), 2) then ident = TRUE end
	elseif object.sval == SV_SCROLL_MAPPING then
		map_area()
		ident = TRUE
	elseif object.sval == SV_SCROLL_DETECT_GOLD then
		if detect_treasure() then ident = TRUE end
		if detect_objects_gold() then ident = TRUE end
	elseif object.sval == SV_SCROLL_DETECT_ITEM then
		if detect_objects_normal() then ident = TRUE end
	elseif object.sval == SV_SCROLL_DETECT_TRAP then
		if detect_traps() then ident = TRUE end
	elseif object.sval == SV_SCROLL_DETECT_DOOR then
		if detect_doors() then ident = TRUE end
		if detect_stairs() then ident = TRUE end
	elseif object.sval == SV_SCROLL_DETECT_INVIS then
		if detect_monsters_invis() then ident = TRUE end
	elseif object.sval == SV_SCROLL_SATISFY_HUNGER then
		if set_food(PY_FOOD_MAX - 1) then ident = TRUE end
	elseif object.sval == SV_SCROLL_BLESSING then
		if set_blessed(player.blessed + rand_range(6, 18)) then ident = TRUE end
	elseif object.sval == SV_SCROLL_HOLY_CHANT then
		if set_blessed(player.blessed + rand_range(12, 36)) then ident = TRUE end
	elseif object.sval == SV_SCROLL_HOLY_PRAYER then
		if set_blessed(player.blessed + rand_range(24, 72)) then ident = TRUE end
	elseif object.sval == SV_SCROLL_MONSTER_CONFUSION then
		if player.confusing == 0 then
			msgf("Your hands begin to glow.")
			player.confusing = TRUE
			ident = TRUE
			player.redraw = bOr(player.redraw, PR_STATUS)
		end
	elseif object.sval == SV_SCROLL_PROTECTION_FROM_EVIL then
		k = 3 * player.lev
		if set_protevil(player.protevil + randint1(25) + k) then ident = TRUE end
	elseif object.sval == SV_SCROLL_RUNE_OF_PROTECTION then
		if not warding_glyph() then used_up = FALSE end
		ident = TRUE
	elseif object.sval == SV_SCROLL_TRAP_DOOR_DESTRUCTION then
		if destroy_doors_touch() then ident = TRUE end
	elseif object.sval == SV_SCROLL_STAR_DESTRUCTION then
		if destroy_area(player.px, player.py, 15) then
			ident = TRUE
		else
			msgf("The dungeon trembles...")
		end
	elseif object.sval == SV_SCROLL_DISPEL_UNDEAD then
		if dispel_undead(60) then ident = TRUE end
	elseif object.sval == SV_SCROLL_GENOCIDE then
		genocide(TRUE)
		ident = TRUE
	elseif object.sval == SV_SCROLL_MASS_GENOCIDE then
		mass_genocide(TRUE)
		ident = TRUE
	elseif object.sval == SV_SCROLL_ACQUIREMENT then
		acquirement(player.px, player.py, 1, TRUE, FALSE)
		ident = TRUE
	elseif object.sval == SV_SCROLL_STAR_ACQUIREMENT then
		acquirement(player.px, player.py, rand_range(2, 3), TRUE, FALSE)
		ident = TRUE
	elseif object.sval == SV_SCROLL_FIRE then
		fire_ball(GF_FIRE, 0, 300, 4)
		-- Note: "Double" damage since it is centered on the player ...
		if not ((player.oppose_fire > 0) or player.resist_fire or player.immune_fire) then
			take_hit(rand_range(50, 100), "a Scroll of Fire")
		end
		ident = TRUE
	elseif object.sval == SV_SCROLL_ICE then
		fire_ball(GF_ICE, 0, 350, 4)
		if not ((player.oppose_cold > 0) or player.resist_cold or player.immune_cold) then
			take_hit(rand_range(100, 200), "a Scroll of Ice")
		end
		ident = TRUE
	elseif object.sval == SV_SCROLL_CHAOS then
		fire_ball(GF_CHAOS, 0, 400, 4)
		if not player.resist_chaos then
			take_hit(rand_range(150, 300), "a Scroll of Logrus")
		end
		ident = TRUE
	elseif object.sval == SV_SCROLL_RUMOR then
		msgf("There is message on the scroll. It says:")
		message_flush()
		msgf(get_rumor())
		message_flush()
		msgf("The scroll disappears in a puff of smoke!")
		ident = TRUE
	elseif object.sval == SV_SCROLL_ARTIFACT then
		if not artifact_scroll() then used_up = FALSE end
		ident = TRUE
	end

	return ident, used_up
end


function use_staff(object)
	local ident = FALSE
	local use_charge = TRUE

	local sval = object.sval

	if sval == SV_STAFF_DARKNESS then
		if not player.resist_blind and not player.resist_dark then
			if set_blind(player.blind + rand_range(4, 8)) then ident = TRUE end
		end
		if unlite_area(10, 3) then ident = TRUE end
	elseif sval == SV_STAFF_SLOWNESS then
		if set_slow(player.slow + rand_range(15, 45)) then ident = TRUE end
	elseif sval == SV_STAFF_HASTE_MONSTERS then
		if speed_monsters() then ident = TRUE end
	elseif sval == SV_STAFF_SUMMONING then
		for k = 0, randint1(4) do
			if summon_specific(0, player.px, player.py, player.depth, 0, TRUE, FALSE, FALSE) then
				ident = TRUE
			end
		end
	elseif sval == SV_STAFF_TELEPORTATION then
		teleport_player(100)
		ident = TRUE
	elseif sval == SV_STAFF_IDENTIFY then
		if not ident_spell() then use_charge = FALSE end
		ident = TRUE
	elseif sval == SV_STAFF_REMOVE_CURSE then
		if remove_curse() then
			if player.blind == 0 then
				msgf("The staff glows blue for a moment...")
			end
			ident = TRUE
		end
	elseif sval == SV_STAFF_STARLITE then
		if player.blind == 0 then
			msgf("The end of the staff glows brightly...")
		end
		starlite()
		ident = TRUE
	elseif sval == SV_STAFF_LITE then
		if lite_area(damroll(2, 8), 2) then ident = TRUE end
	elseif sval == SV_STAFF_MAPPING then
		map_area()
		ident = TRUE
	elseif sval == SV_STAFF_DETECT_GOLD then
		if detect_treasure() then ident = TRUE end
		if detect_objects_gold() then ident = TRUE end
	elseif sval == SV_STAFF_DETECT_ITEM then
		if detect_objects_normal() then ident = TRUE end
	elseif sval == SV_STAFF_DETECT_TRAP then
		if detect_traps() then ident = TRUE end
	elseif sval == SV_STAFF_DETECT_DOOR then
		if detect_doors() then ident = TRUE end
		if detect_stairs() then ident = TRUE end
	elseif sval == SV_STAFF_DETECT_INVIS then
		if detect_monsters_invis() then ident = TRUE end
	elseif sval == SV_STAFF_DETECT_EVIL then
		if detect_monsters_evil() then ident = TRUE end
	elseif sval == SV_STAFF_CURE_LIGHT then
		if hp_player(50) then ident = TRUE end
	elseif sval == SV_STAFF_CURING then
		if hp_player(150) then ident = TRUE end
		if set_blind(0) then ident = TRUE end
		if set_poisoned(0) then ident = TRUE end
		if set_confused(0) then ident = TRUE end
		if set_stun(0) then ident = TRUE end
		if set_cut(0) then ident = TRUE end
		if set_image(0) then ident = TRUE end
	elseif sval == SV_STAFF_HEALING then
		if hp_player(300) then ident = TRUE end
		if set_stun(0) then ident = TRUE end
		if set_cut(0) then ident = TRUE end
	elseif sval == SV_STAFF_THE_MAGI then
		if do_res_stat(A_INT) then ident = TRUE end
		if player.csp < player.msp then
			player.csp = player.msp
			player.csp_frac = 0
			msgf("Your feel your head clear.")
			player.redraw = bOr(player.redraw, PR_MANA)
			player.window = bOr(player.window, PW_PLAYER)
			player.window = bOr(player.window, PW_SPELL)
			ident = TRUE
		end
	elseif sval == SV_STAFF_SLEEP_MONSTERS then
		if sleep_monsters() then ident = TRUE end
	elseif sval == SV_STAFF_SLOW_MONSTERS then
		if slow_monsters() then ident = TRUE end
	elseif sval == SV_STAFF_SPEED then
		if player.fast == 0 then
			if set_fast(rand_range(15, 45)) then ident = TRUE end
		else
			set_fast(player.fast + 5)
		end
	elseif sval == SV_STAFF_PROBING then
		probing()
		ident = TRUE
	elseif sval == SV_STAFF_DISPEL_EVIL then
		if dispel_evil(60) then ident = TRUE end
	elseif sval == SV_STAFF_POWER then
		if dispel_monsters(300) then ident = TRUE end
	elseif sval == SV_STAFF_HOLINESS then
		if dispel_evil(300) then ident = TRUE end
		local k = 3 * player.lev
		if set_protevil(player.protevil + randint1(25) + k) then ident = TRUE end
		if set_poisoned(0) then ident = TRUE end
		if set_afraid(0) then ident = TRUE end
		if hp_player(50) then ident = TRUE end
		if set_stun(0) then ident = TRUE end
		if set_cut(0) then ident = TRUE end
	elseif sval == SV_STAFF_GENOCIDE then
		genocide(TRUE)
		ident = TRUE
	elseif sval == SV_STAFF_EARTHQUAKES then
		if earthquake(player.px, player.py, 10) then
			ident = TRUE
		else
			msgf("The dungeon trembles.")
		end
	elseif sval == SV_STAFF_DESTRUCTION then
		if destroy_area(player.px, player.py, 15) then ident = TRUE end
	end

	return ident, use_charge
end


function aim_wand(object)
	local success
	local dir

	-- Allow direction to be cancelled for free
	success, dir = get_aim_dir()
	if not success then return FALSE, FALSE end

	-- Take a turn
	player.energy_use = min(75, 200 - 5 * player.skill_dev / 8)

	-- Not identified yet
	local ident = FALSE

	-- Get the object level
	local lev = k_info[object.k_idx].level

	-- Base chance of success
	local chance = player.skill_dev

	-- Confusion hurts skill
	if (player.confused ~= 0) then chance = chance / 2 end

	-- High level objects are harder
	chance = chance - lev / 2

	-- Give everyone a (slight) chance
	if (chance < USE_DEVICE) and one_in_(USE_DEVICE - chance + 1) then
		chance = USE_DEVICE
	end

	-- Roll for usage
	if (chance < USE_DEVICE) or (randint1(chance) < USE_DEVICE) then
		if flush_failure then flush() end
		msgf("You failed to use the wand properly.")
		sound(SOUND_FAIL)
		return FALSE, FALSE
	end

	-- The wand is already empty!
	if object.pval <= 0 then
		if flush_failure then flush() end
		msgf("The wand has no charges left.")

		object.ident = bOr(object.ident, IDENT_EMPTY)
		player.notice = bOr(player.notice, bOr(PN_COMBINE, PN_REORDER))
		player.window = bOr(player.window, PW_INVEN)

		return FALSE, FALSE
	end

	-- Sound
	sound(SOUND_ZAP)

	local sval = object.sval

	-- Hack -- Wand of wonder can do anything before it
	if sval == SV_WAND_WONDER then sval = randint0(SV_WAND_WONDER) end

	if sval == SV_WAND_HEAL_MONSTER then
		if heal_monster(dir) then ident = TRUE end
	elseif sval == SV_WAND_HASTE_MONSTER then
		if speed_monster(dir) then ident = TRUE end
	elseif sval == SV_WAND_CLONE_MONSTER then
		if clone_monster(dir) then ident = TRUE end
	elseif sval == SV_WAND_TELEPORT_AWAY then
		if teleport_monster(dir) then ident = TRUE end
	elseif sval == SV_WAND_DISARMING then
		if disarm_trap(dir) then ident = TRUE end
	elseif sval == SV_WAND_TRAP_DOOR_DEST then
		if destroy_door(dir) then ident = TRUE end
	elseif sval == SV_WAND_STONE_TO_MUD then
		if wall_to_mud(dir) then ident = TRUE end
	elseif sval == SV_WAND_LITE then
		msgf("A line of blue shimmering light appears.")
		lite_line(dir)
		ident = TRUE
	elseif sval == SV_WAND_SLEEP_MONSTER then
		if sleep_monster(dir) then ident = TRUE end
	elseif sval == SV_WAND_SLOW_MONSTER then
		if slow_monster(dir) then ident = TRUE end
	elseif sval == SV_WAND_CONFUSE_MONSTER then
		if confuse_monster(dir, 20) then ident = TRUE end
	elseif sval == SV_WAND_FEAR_MONSTER then
		if fear_monster(dir, 20) then ident = TRUE end
	elseif sval == SV_WAND_DRAIN_LIFE then
		if drain_life(dir, 150) then ident = TRUE end
	elseif sval == SV_WAND_POLYMORPH then
		if poly_monster(dir) then ident = TRUE end
	elseif sval == SV_WAND_STINKING_CLOUD then
		ident = fire_ball(GF_POIS, dir, 15, 2)
	elseif sval == SV_WAND_MAGIC_MISSILE then
		ident = fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(2, 6))
	elseif sval == SV_WAND_ACID_BOLT then
		ident = fire_bolt_or_beam(20, GF_ACID, dir, damroll(6, 8))
	elseif sval == SV_WAND_CHARM_MONSTER then
		ident = charm_monster(dir, 45)
	elseif sval == SV_WAND_FIRE_BOLT then
		ident = fire_bolt_or_beam(20, GF_FIRE, dir, damroll(10, 8))
	elseif sval == SV_WAND_COLD_BOLT then
		ident = fire_bolt_or_beam(20, GF_COLD, dir, damroll(6, 8))
	elseif sval == SV_WAND_ACID_BALL then
		ident = fire_ball(GF_ACID, dir, 125, 2)
	elseif sval == SV_WAND_ELEC_BALL then
		ident = fire_ball(GF_ELEC, dir, 75, 2)
	elseif sval == SV_WAND_FIRE_BALL then
		ident = fire_ball(GF_FIRE, dir, 150, 2)
	elseif sval == SV_WAND_COLD_BALL then
		ident = fire_ball(GF_COLD, dir, 100, 2)
	elseif sval == SV_WAND_WONDER then
		msgf("Oops.  Wand of wonder activated.")
	elseif sval == SV_WAND_DRAGON_FIRE then
		ident = fire_ball(GF_FIRE, dir, 250, 3)
		ident = TRUE
	elseif sval == SV_WAND_DRAGON_COLD then
		ident = fire_ball(GF_COLD, dir, 200, 3)
		ident = TRUE
	elseif sval == SV_WAND_DRAGON_BREATH then
		local choice = randint1(5)

		if choice == 1 then
			ident = fire_ball(GF_ACID, dir, 250, 3)
		elseif choice == 2 then
			ident = fire_ball(GF_ELEC, dir, 150, 3)
		elseif choice == 3 then
			ident = fire_ball(GF_FIRE, dir, 200, 3)
		elseif choice == 4 then
			ident = fire_ball(GF_COLD, dir, 200, 3)
		else
			ident = fire_ball(GF_POIS, dir, 200, 3)
		end

		ident = TRUE
	elseif sval == SV_WAND_ANNIHILATION then
		ident = fire_ball(GF_DISINTEGRATE, dir, rand_range(125, 225), 2)
	elseif sval == SV_WAND_ROCKETS then
		msgf("You launch a rocket!")
		fire_ball(GF_ROCKET, dir, 250, 2)
		ident = TRUE
	end

	return ident, TRUE
end



function use_object_hook(object)
	local ident = FALSE
	local used = FALSE

	if object.tval == TV_FOOD then
		ident, used = eat_food(object)
	elseif object.tval == TV_POTION then
		ident, used = quaff_potion(object)
	elseif object.tval == TV_SCROLL then
		ident, used = read_scroll(object)
	elseif object.tval == TV_STAFF then
		ident, used = use_staff(object)
	elseif object.tval == TV_WAND then
		ident, used = aim_wand(object)
	end

	return ident, used
end

