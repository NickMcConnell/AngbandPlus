#include "angband.h"

/* Devices: We are following the do_spell() pattern which is quick and dirty,
   but not my preferred approach ... */

/* Fail Rates ... Scaled by 10 (95.2% returned as 952) */
static int _rod_calc_fail_rate(object_type *o_ptr)
{
	int lev, chance, fail;

	lev = k_info[o_ptr->k_idx].level;
	chance = p_ptr->skills.dev;
	if (p_ptr->confused) chance = chance / 2;

	fail = lev+5;
	if (chance > fail) fail -= (chance - fail)*2;
	else chance -= (fail - chance)*2;
	if (fail < USE_DEVICE) fail = USE_DEVICE;
	if (chance < USE_DEVICE) chance = USE_DEVICE;

	if (chance > fail)
		return fail*1000/(chance*2);

	return 1000 - chance*1000/(fail*2);
}

int device_calc_fail_rate(object_type *o_ptr)
{
	int lev, chance, fail;

	if (p_ptr->pclass == CLASS_BERSERKER) return 1000;
	if (o_ptr->tval == TV_ROD) return _rod_calc_fail_rate(o_ptr);

	lev = k_info[o_ptr->k_idx].level;
	if (lev > 50) lev = 50 + (lev - 50)/2;
	chance = p_ptr->skills.dev;
	if (p_ptr->confused) chance = chance / 2;
	chance = chance - lev;
	if (chance < USE_DEVICE) 
		fail = 1000 - 1000/(3 * (USE_DEVICE - chance + 1));
	else
		fail = (USE_DEVICE-1)*1000/chance;

	if (o_ptr->tval == TV_SCROLL && fail > 500) fail = 500;

	return fail;
}

/* Hack: When using an unkown rod we force the user to target. Also
   Trap Location should not spoil with the view_unsafe_grids option. */
bool device_known = FALSE;

/* Hack: When using an unkown device, was there an observable effect?
   If so, identify the device. */
bool device_noticed = FALSE;

/* Using Devices 
      if (!device_try(o_ptr)) ... "You failed to use the device" ...
	  if (device_use(o_ptr)) ... Decrement Charges/Unstack/Etc. ...
*/
bool device_try(object_type *o_ptr)
{
	int fail = device_calc_fail_rate(o_ptr);
	if (randint0(1000) < fail)
		return FALSE;
	return TRUE;
}

bool device_use(object_type *o_ptr)
{
	device_known = object_is_known(o_ptr);
	if (do_device(o_ptr->tval, o_ptr->sval, SPELL_CAST))
		return TRUE;
	return FALSE;
}

static int _device_power_hack(int pow)
{
	if (magic_eater_hack) return spell_power(pow);
	return device_power(pow);
}

static cptr _do_scroll(int sval, int mode)
{
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool ident = FALSE;

	switch (sval)
	{
	case SV_SCROLL_DARKNESS:
		if (desc) return "It darkens nearby area or current room and blinds you when you read it.";
		if (cast)
		{
			if (!res_save_default(RES_BLIND) && !res_save_default(RES_DARK))
			{
				if (set_blind(p_ptr->blind + 3 + randint1(5), FALSE)) device_noticed = TRUE;
			}
			if (unlite_area(10, 3)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_AGGRAVATE_MONSTER:
		if (desc) return "It aggravates monsters in your vicinity when you read it.";
		if (cast)
		{
			msg_print("There is a high pitched humming noise.");
			aggravate_monsters(0);
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_CURSE_ARMOR:
		if (desc) return "It makes your current armour (Blasted) when you read it.";
		if (cast)
		{
			int slot = equip_random_slot(object_is_armour);
			if (slot && curse_armor(slot)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_CURSE_WEAPON:
		if (desc) return "It makes your wielding weapon (Shattered) when you read it.";
		if (cast)
		{
			int slot = equip_random_slot(object_is_melee_weapon);
			if (slot && curse_weapon(FALSE, slot)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_SUMMON_MONSTER:
		if (desc) return "It summons several monsters as enemies when you read it.";
		if (cast)
		{
			int i;
			for (i = 0; i < randint1(3); i++)
			{
				if (summon_specific(0, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
					device_noticed = TRUE;
			}
		}
		break;
	case SV_SCROLL_SUMMON_UNDEAD:
		if (desc) return "It summons several undead monsters as enemies when you read it.";
		if (cast)
		{
			int i;
			for (i = 0; i < randint1(3); i++)
			{
				if (summon_specific(0, py, px, dun_level, SUMMON_UNDEAD, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
					device_noticed = TRUE;
			}
		}
		break;
	case SV_SCROLL_SUMMON_PET:
		if (desc) return "It summons a monster as your pet when you read it.";
		if (cast)
		{
			if (summon_specific(-1, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_FORCE_PET)))
				device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_SUMMON_KIN:
		if (desc) return "It summons a monster corresponds to your race as your pet when you read it.";
		if (cast)
		{
			if (summon_kin_player(p_ptr->lev, py, px, (PM_FORCE_PET | PM_ALLOW_GROUP)))
				device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_TRAP_CREATION:
		if (desc) return "It creates traps on the squares adjacent to you when you read it.";
		if (cast)
		{
			if (trap_creation(py, px)) 
				device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_PHASE_DOOR:
		if (desc) return "It teleports you a short distance when you read it.";
		if (cast)
		{
			teleport_player(10, 0L);
			if (mut_present(MUT_ASTRAL_GUIDE))
				energy_use = 30;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_TELEPORT:
		if (desc) return "It teleports you a long distance when you read it.";
		if (cast)
		{
			energy_use = 150;
			teleport_player(100, 0L);
			if (mut_present(MUT_ASTRAL_GUIDE))
				energy_use = 75;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_TELEPORT_LEVEL:
		if (desc) return "It teleports you one dungeon level up or down immediately when you read it.";
		if (cast)
		{
			(void)teleport_level(0);
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_WORD_OF_RECALL:
		if (desc) return "It recalls you to the town, or back into the dungeon you have entered when you read it.";
		if (cast)
		{
			if (!word_of_recall()) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_IDENTIFY:
		if (desc) return "It identifies an item when you read it.";
		if (cast)
		{
			if (!ident_spell(NULL)) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_STAR_IDENTIFY:
		if (desc) return "It reveals all information about an item when you read it.";
		if (cast)
		{
			if (!identify_fully(NULL)) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_REMOVE_CURSE:
		if (desc) return "It removes normal curses from equipped items when you read it.";
		if (cast)
		{
			if (remove_curse())
			{
				msg_print("You feel as if someone is watching over you.");
				device_noticed = TRUE;
			}
		}
		break;
	case SV_SCROLL_STAR_REMOVE_CURSE:
		if (desc) return "It removes normal and heavy curses from equipped items when you read it.";
		if (cast)
		{
			if (remove_all_curse())
				msg_print("You feel as if someone is watching over you.");
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_ENCHANT_ARMOR:
		if (desc) return "It increases an armour's to-AC when you read it.";
		if (cast)
		{
			if (!enchant_spell(0, 0, 1)) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
		if (desc) return "It increases a weapon's to-hit when you read it.";
		if (cast)
		{
			if (!enchant_spell(1, 0, 0)) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
		if (desc) return "It increases a weapon's to-dam when you read it.";
		if (cast)
		{
			if (!enchant_spell(0, 1, 0)) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_STAR_ENCHANT_ARMOR:
		if (desc) return "It increases an armour's to-ac powerfully when you read it.";
		if (cast)
		{
			if (!enchant_spell(0, 0, randint1(3) + 2)) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_STAR_ENCHANT_WEAPON:
		if (desc) return "It increases a weapon's to-hit and to-dam when you read it.";
		if (cast)
		{
			if (!enchant_spell(randint1(3), randint1(3), 0)) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_RECHARGING:
		if (desc) return "It recharges wands, staffs or rods when you read it.";
		if (cast)
		{
			if (!recharge(75)) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_MUNDANITY:
		if (desc) return "This removes the ego or artifact status and all enchantment of an item. As a bonus, if you have a stack of them, the extras are destroyed.";
		if (cast)
		{
			if (!mundane_spell(FALSE)) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_LIGHT:
		if (desc) return "It lights up nearby area or the current room permanently when you read it.";
		if (cast)
		{
			if (lite_area(damroll(2, 8), 2)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_MAPPING:
		if (desc) return "It maps your vicinity when you read it.";
		if (cast)
		{
			map_area(DETECT_RAD_MAP);
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_DETECT_GOLD:
		if (desc) return "It detects all treasures in your vicinity when you read it.";
		if (cast)
		{
			if (detect_treasure(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
			if (detect_objects_gold(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_DETECT_ITEM:
		if (desc) return "It detects all items in your vicinity when you read it.";
		if (cast)
		{
			if (detect_objects_normal(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_DETECT_TRAP:
		if (desc) return "It detects all traps in your vicinity when you read it.";
		if (cast)
		{
			if (detect_traps(DETECT_RAD_DEFAULT, device_known)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_DETECT_DOOR:
		if (desc) return "It detects all doors and stairs in your vicinity when you read it.";
		if (cast)
		{
			if (detect_doors(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
			if (detect_stairs(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_DETECT_INVIS:
		if (desc) return "It detects all invisible monsters in your vicinity when you read it.";
		if (cast)
		{
			if (detect_monsters_invis(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_DETECT_MONSTERS:
		if (desc) return "It detects all monsters in your vicinity when you read it.";
		if (cast)
		{
			if (detect_monsters_normal(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_SATISFY_HUNGER:
		if (desc) return "It satisfies hunger when you read it.";
		if (cast)
		{
			if (set_food(PY_FOOD_MAX - 1)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_BLESSING:
		if (desc) return "It blesses you temporarily when you read it.";
		if (cast)
		{
			if (set_blessed(p_ptr->blessed + randint1(12) + 6, FALSE)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_HOLY_CHANT:
		if (desc) return "It blesses you temporarily when you read it.";
		if (cast)
		{
			if (set_blessed(p_ptr->blessed + randint1(24) + 12, FALSE)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_HOLY_PRAYER:
		if (desc) return "It blesses you temporarily when you read it.";
		if (cast)
		{
			if (set_blessed(p_ptr->blessed + randint1(48) + 24, FALSE)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_MONSTER_CONFUSION:
		if (desc) return "You can confuse monster you hit just for once when you read it.";
		if (cast)
		{
			if (!(p_ptr->special_attack & ATTACK_CONFUSE))
			{
				msg_print("Your hands begin to glow.");
				p_ptr->special_attack |= ATTACK_CONFUSE;
				p_ptr->redraw |= (PR_STATUS);
				device_noticed = TRUE;
			}
		}
		break;
	case SV_SCROLL_PROTECTION_FROM_EVIL:
		if (desc) return "It gives temporary protection from lesser evil creatures when you read it.";
		if (cast)
		{
			if (set_protevil(p_ptr->protevil + randint1(25) + 3 * p_ptr->lev, FALSE)) 
				device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_RUNE_OF_PROTECTION:
		if (desc) return "It creates a glyph on the floor you stand when you read it.";
		if (cast)
		{
			warding_glyph();
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
		if (desc) return "It destroys traps on the floors adjacent to you when you read it.";
		if (cast)
		{
			if (destroy_doors_touch()) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_STAR_DESTRUCTION:
		if (desc) return "It destroys everything nearby you when you read it.";
		if (cast)
		{
			if (destroy_area(py, px, 13 + randint0(5), 2000))
				device_noticed = TRUE;
			else
				msg_print("The dungeon trembles...");
		}
		break;
	case SV_SCROLL_DISPEL_UNDEAD:
		if (desc) return "It damages all undead monsters in sight when you read it.";
		if (info) return info_damage(0, 0, 80);
		if (cast)
		{
			if (dispel_undead(80)) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_SPELL:
		if (desc) return "It increases the number you can study spells when you read. If you are the class can't study or don't need to study, it has no effect.";
		if (cast)
		{
			if ((p_ptr->pclass == CLASS_WARRIOR) ||
			    (p_ptr->pclass == CLASS_IMITATOR) || 
				(p_ptr->pclass == CLASS_MINDCRAFTER) || 
				(p_ptr->pclass == CLASS_PSION) || 
				(p_ptr->pclass == CLASS_SORCERER) || 
				(p_ptr->pclass == CLASS_ARCHER) || 
				(p_ptr->pclass == CLASS_MAGIC_EATER) || 
				(p_ptr->pclass == CLASS_RED_MAGE) || 
				(p_ptr->pclass == CLASS_SAMURAI) || 
				(p_ptr->pclass == CLASS_BLUE_MAGE) || 
				(p_ptr->pclass == CLASS_CAVALRY) || 
				(p_ptr->pclass == CLASS_BERSERKER) || 
				(p_ptr->pclass == CLASS_WEAPONSMITH) || 
				(p_ptr->pclass == CLASS_MIRROR_MASTER) || 
				(p_ptr->pclass == CLASS_TIME_LORD) || 
				(p_ptr->pclass == CLASS_BLOOD_KNIGHT) || 
				(p_ptr->pclass == CLASS_WARLOCK) || 
				(p_ptr->pclass == CLASS_ARCHAEOLOGIST) || 
				(p_ptr->pclass == CLASS_DUELIST) || 
				(p_ptr->pclass == CLASS_RUNE_KNIGHT) ||
				(p_ptr->pclass == CLASS_WILD_TALENT) ||
				(p_ptr->pclass == CLASS_NINJA) ||
				p_ptr->pclass == CLASS_SCOUT ||
				p_ptr->pclass == CLASS_MAULER)
			{
				return NULL;
			}
			p_ptr->add_spells++;
			p_ptr->update |= (PU_SPELLS);
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_GENOCIDE:
		if (desc) return "It eliminates an entire class of monster, exhausting you. Powerful or unique monsters may resist.";
		if (cast)
		{
			(void)symbol_genocide(300, TRUE);
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_MASS_GENOCIDE:
		if (desc) return "It eliminates all nearby monsters, exhausting you. Powerful or unique monsters may be able to resist.";
		if (cast)
		{
			(void)mass_genocide(300, TRUE);
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_ACQUIREMENT:
		if (desc) return "It creates one great item when you read it.";
		if (cast)
		{
			acquirement(py, px, 1, TRUE, FALSE);
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_STAR_ACQUIREMENT:
		if (desc) return "It creates some great items when you read it.";
		if (cast)
		{
			acquirement(py, px, randint1(2) + 1, TRUE, FALSE);
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_FOREST_CREATION:
		if (desc) return "";
		if (cast)
		{
			if (tree_creation()) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_WALL_CREATION:
		if (desc) return "";
		if (cast)
		{
			if (wall_stone()) device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_VENGEANCE:
		if (desc) return "";
		if (cast)
		{
			set_tim_eyeeye(randint1(25) + 25, FALSE);
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_RUMOR:
		if (desc) return "A rumor is in it.";
		if (cast)
		{
			char Rumor[1024];
			errr err = 0;

			switch (randint1(20))
			{
			case 1:
				err = get_rnd_line("chainswd.txt", 0, Rumor);
				break;
			case 2:
				err = get_rnd_line("error.txt", 0, Rumor);
				break;
			case 3:
			case 4:
			case 5:
				err = get_rnd_line("death.txt", 0, Rumor);
				break;
			default:
				err = get_rnd_line("rumors.txt", 0, Rumor);
			}

			if (err) strcpy(Rumor, "Some rumors are wrong.");
			msg_print("There is message on the scroll. It says:");
			msg_print(NULL);
			msg_format("%s", Rumor);
			msg_print(NULL);
			msg_print("The scroll disappears in a puff of smoke!");
			device_noticed = TRUE;
		}
		break;
	case SV_SCROLL_ARTIFACT:
		if (desc) return "It creates an artifact from a nameless weapon or armour when you read it. Don't be greedy - you will get only one artifact.";
		if (cast)
		{
			device_noticed = TRUE;
			if (no_artifacts)
			{
				if (!brand_weapon(-1)) return NULL;
			}
			else
			{
				if (!artifact_scroll()) return NULL;
			}
		}
		break;
	case SV_SCROLL_MADNESS:
		if (desc) return "It seems to be the hurried scriblings of a mad wizard on the verge of some great arcane discovery.  You can't make heads or tails of it. Do you read it to see what happens?";
		if (cast)
		{
			int item;
			object_type *o_ptr;
			int n = randint0(100);

			item_tester_hook = item_tester_hook_nameless_weapon_armour;
			if (!get_item(&item, "Use which item? ", "You have nothing to use.", (USE_EQUIP | USE_INVEN | USE_FLOOR))) return NULL;

			if (item >= 0)
				o_ptr = &inventory[item];
			else
				o_ptr = &o_list[0 - item];

			if (o_ptr->number > 1)
			{
				msg_print("Don't be greedy.  Just try it out on a single object at a time.");
				return NULL;
			}
			
			device_noticed = TRUE;

			/* TODO: Add more goodies ... */
			if (n < 10)
			{
				msg_print("Ooops!  That didn't work at all!");
				destroy_area(py, px, 13 + randint0(5), 300);
			}
			else if (n < 15)
			{
				msg_print("You faintly hear crazy laughter for a moment.");
				summon_cyber(-1, py, px);
			}
			else if (n < 25)
			{
				msg_print("The scroll explodes violently!");
				call_chaos();
			}
			else if (n < 65)
			{				
				curse_weapon(FALSE, item);	/* This curses armor too ... */
			}
			else if (n < 90)
			{
				if (object_is_melee_weapon(o_ptr))
				{
					if (!brand_weapon_aux(item)) return NULL;
				}
				else
					msg_print("Funny, nothing happened.");
			}
			else
			{
				if (no_artifacts)
				{
					if (object_is_melee_weapon(o_ptr))
					{
						if (!brand_weapon_aux(item)) return NULL;
					}
				}
				else
					create_artifact(o_ptr, CREATE_ART_SCROLL | CREATE_ART_GOOD);
			}
		}
		break;
	case SV_SCROLL_BRAND_WEAPON:
		if (desc) return "It creates an ego item from a nameless weapon when you read it. ";
		if (cast)
		{
			device_noticed = TRUE;
			if (!brand_weapon(-1)) return NULL;
		}
		break;
	case SV_SCROLL_RESET_RECALL:
		if (desc) return "It resets the dungeon level for recall spell when you read it.";
		if (cast)
		{
			device_noticed = TRUE;
			if (!reset_recall()) return NULL;
		}
		break;
	}
	return "";
}

static cptr _do_staff(int sval, int mode)
{
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool ident = FALSE;

	switch (sval)
	{
	case SV_STAFF_DARKNESS:
		if (desc) return "It darkens nearby area or current room and blinds you when you use it.";
		if (cast)
		{
			if (!res_save_default(RES_BLIND) && !res_save_default(RES_DARK))
			{
				if (set_blind(p_ptr->blind + 3 + randint1(5), FALSE)) device_noticed = TRUE;
			}
			if (unlite_area(10, 3)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_SLOWNESS:
		if (desc) return "It slows you down temporarily when you use it.";
		if (cast)
		{
			if (set_slow(p_ptr->slow + randint1(30) + 15, FALSE)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_HASTE_MONSTERS:
		if (desc) return "It hastes all monsters in sight when you use it.";
		if (cast)
		{
			if (speed_monsters()) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_SUMMONING:
		if (desc) return "It summons several monsters as enemies when you use it.";
		if (cast)
		{
			int i;
			int num = randint1(4);
			for (i = 0; i < num; i++)
			{
				if (summon_specific(0, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
					device_noticed = TRUE;
			}
		}
		break;
	case SV_STAFF_TELEPORTATION:
		if (desc) return "It teleports you a long distance when you use it.";
		if (cast)
		{
			if (mut_present(MUT_ASTRAL_GUIDE))
				energy_use = 30;
			teleport_player(100, 0L);
			device_noticed = TRUE;
		}
		break;
	case SV_STAFF_IDENTIFY:
		if (desc) return "It identifies an item when you use it.";
		if (cast)
		{
			if (!ident_spell(NULL)) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_STAFF_REMOVE_CURSE:
		if (desc) return "It removes normal curses from equipped items when you use it.";
		if (cast && remove_curse())
		{
			if (magic_eater_hack)
				msg_print("You feel as if someone is watching over you.");
			else if (!p_ptr->blind)
				msg_print("The staff glows blue for a moment.");
			device_noticed = TRUE;
		}
		break;
	case SV_STAFF_STARLITE:
		if (desc) return "It fires a line of light directed randomly for multiple times when you use it.";
		if (cast)
		{
			int num = damroll(5, 3);
			int y, x, k;
			int attempts;

			if (!p_ptr->blind && !magic_eater_hack)
				msg_print("The end of the staff glows brightly...");

			for (k = 0; k < num; k++)
			{
				attempts = 1000;
				while (attempts--)
				{
					scatter(&y, &x, py, px, 4, 0);
					if (!cave_have_flag_bold(y, x, FF_PROJECT)) continue;
					if (!player_bold(y, x)) break;
				}
				project(0, 0, y, x, _device_power_hack(damroll(6 + p_ptr->lev / 8, 10)), GF_LITE_WEAK,
						  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_KILL), -1);
			}
			device_noticed = TRUE;
		}
		break;
	case SV_STAFF_LITE:
		if (desc) return "It lights up nearby area or the current room permanently when you use it.";
		if (cast)
		{
			if (lite_area(damroll(2, 8), 2)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_MAPPING:
		if (desc) return "It maps your vicinity when you use it.";
		if (cast)
		{
			map_area(DETECT_RAD_MAP);
			device_noticed = TRUE;
		}
		break;
	case SV_STAFF_DETECT_GOLD:
		if (desc) return "It detects all treasures in your vicinity when you use it.";
		if (cast)
		{
			if (detect_treasure(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
			if (detect_objects_gold(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_DETECT_ITEM:
		if (desc) return "It detects all items in your vicinity when you use it.";
		if (cast)
		{
			if (detect_objects_normal(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_DETECT_TRAP:
		if (desc) return "It detects all traps in your vicinity when you use it.";
		if (cast)
		{
			if (detect_traps(DETECT_RAD_DEFAULT, device_known)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_DETECT_DOOR:
		if (desc) return "It detects all doors and stairs in your vicinity when you use it.";
		if (cast)
		{
			if (detect_doors(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
			if (detect_stairs(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_DETECT_INVIS:
		if (desc) return "It detects all invisible monsters in your vicinity when you use it.";
		if (cast)
		{
			if (detect_monsters_invis(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_DETECT_EVIL:
		if (desc) return "It detects all evil monsters in your vicinity when you use it.";
		if (cast)
		{
			if (detect_monsters_evil(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_CURE_LIGHT:
		if (desc) return "It heals you a bit when you use it.";
		if (info) return info_heal(6, _device_power_hack(8), 0);
		if (cast)
		{
			if (hp_player(damroll(6, 8))) device_noticed = TRUE;
			if (set_blind(0, TRUE)) device_noticed = TRUE;
			if (set_confused(0, TRUE)) device_noticed = TRUE;
			if (set_cut((p_ptr->cut / 2) - 50, TRUE)) device_noticed = TRUE;
			if (set_shero(0,TRUE)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_CURING:
		if (desc) return "It cures blindness, poison, confusion, stunned, cuts, hallucination and berserk when you use it.";
		if (cast)
		{
			if (set_blind(0, TRUE)) device_noticed = TRUE;
			if (set_poisoned(0, TRUE)) device_noticed = TRUE;
			if (set_confused(0, TRUE)) device_noticed = TRUE;
			if (set_stun(0, TRUE)) device_noticed = TRUE;
			if (set_cut(0, TRUE)) device_noticed = TRUE;
			if (set_image(0, TRUE)) device_noticed = TRUE;
			if (set_shero(0,TRUE)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_HEALING:
		if (desc) return "It heals you and cures stunned, cuts and berserk when you use it.";
		if (info) return info_heal(0, 0, _device_power_hack(300));
		if (cast)
		{
			if (hp_player(_device_power_hack(300))) device_noticed = TRUE;
			if (set_stun(0, TRUE)) device_noticed = TRUE;
			if (set_cut(0, TRUE)) device_noticed = TRUE;
			if (set_shero(0,TRUE)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_THE_MAGI:
		if (desc) return "It restores mana to full, restores your intelligence and cures berserk when you use it.";
		if (cast)
		{
			if (do_res_stat(A_INT)) device_noticed = TRUE;
			if (restore_mana()) device_noticed = TRUE;
			if (set_shero(0,TRUE)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_SLEEP_MONSTERS:
		if (desc) return "It puts all monsters in sight to sleep when you use it.";
		if (cast)
		{
			if (sleep_monsters(p_ptr->lev*3)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_SLOW_MONSTERS:
		if (desc) return "It slows all monsters in sight down when you use it.";
		if (cast)
		{
			if (slow_monsters(p_ptr->lev*3)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_SPEED:
		if (desc) return "It hastes you temporarily when you use it.";
		if (info) return info_duration(_device_power_hack(15), _device_power_hack(30));
		if (cast)
		{
			if (set_fast(_device_power_hack(randint1(30) + 15), FALSE)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_PROBING:
		if (desc) return "It probes all monsters' alignment, HP, AC, speed, current experience and true character in sight when you use it.";
		if (cast)
		{
			probing();
			device_noticed = TRUE;
		}
		break;
	case SV_STAFF_DISPEL_EVIL:
		if (desc) return "It damages all evil monsters in sight when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(100));
		if (cast)
		{
			if (dispel_evil(_device_power_hack(100))) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_POWER:
		if (desc) return "It does damage to all monsters in sight when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(150));
		if (cast)
		{
			if (dispel_monsters(_device_power_hack(150))) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_HOLINESS:
		if (desc) return "It does damage to all evil monsters in sight, gives temporary protection from lesser evil creature, cures poison, stuuned, cuts, removes fear and heals you a bit when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(150));
		if (cast)
		{
			int k = 3 * p_ptr->lev;
			if (dispel_evil(_device_power_hack(150))) device_noticed = TRUE;
			if (set_protevil(p_ptr->protevil + randint1(25) + k, FALSE)) device_noticed = TRUE;
			if (set_poisoned(0, TRUE)) device_noticed = TRUE;
			if (hp_player(_device_power_hack(50))) device_noticed = TRUE;
			if (set_stun(0, TRUE)) device_noticed = TRUE;
			if (set_cut(0, TRUE)) device_noticed = TRUE;
		}
		break;
	case SV_STAFF_GENOCIDE:
		if (desc) return "It eliminates an entire class of monster, exhausting you. Powerful or unique monsters may resist.";
		if (cast)
		{
			(void)symbol_genocide((magic_eater_hack ? p_ptr->lev + 50 : device_power(200)), TRUE);
			device_noticed = TRUE;
		}
		break;
	case SV_STAFF_EARTHQUAKES:
		if (desc) return "It causes a earthquake nearby you when you use it.";
		if (cast)
		{
			if (!earthquake(py, px, 10))
				msg_print("The dungeon trembles.");
			device_noticed = TRUE;
		}
		break;
	case SV_STAFF_DESTRUCTION:
		if (desc) return "It destroys everything nearby you when you use it.";
		if (cast)
		{
			if (destroy_area(py, px, 13 + randint0(5), _device_power_hack(4 * p_ptr->lev)))
				device_noticed = TRUE;
		}
		break;
	case SV_STAFF_ANIMATE_DEAD:
		if (desc) return "It raises corpses and skeletons nearby you from dead and makes them your pet when you use it.";
		if (cast)
		{
			if (animate_dead(0, py, px))
				device_noticed = TRUE;
		}
		break;
	case SV_STAFF_MSTORM:
		if (desc) return "It produces a huge mana ball centered on you when you use it. If you are not magically inclined, you take damage as well.";
		if (info) return info_damage(1, _device_power_hack(200), _device_power_hack(300));
		if (cast)
		{
			msg_print("Mighty magics rend your enemies!");
			project(0, 5, py, px,
				_device_power_hack((randint1(200) + 300) * 2), 
				GF_MANA, PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID, -1);
			if ( p_ptr->pclass != CLASS_MAGE
			  && p_ptr->pclass != CLASS_HIGH_MAGE 
			  && p_ptr->pclass != CLASS_SORCERER 
			  && p_ptr->pclass != CLASS_MAGIC_EATER 
			  && p_ptr->pclass != CLASS_BLUE_MAGE 
			  && p_ptr->pclass != CLASS_BLOOD_MAGE )
			{
				(void)take_hit(DAMAGE_NOESCAPE, 50, "unleashing magics too mighty to control", -1);
			}
			device_noticed = TRUE;
		}
		break;
	case SV_STAFF_NOTHING:
		if (desc) return "It does nothing when you use it.";
		if (cast)
		{
			msg_print("Nothing happens.");
			if ( prace_is_(RACE_SKELETON) 
			  || prace_is_(RACE_GOLEM) 
			  || prace_is_(RACE_ZOMBIE) 
			  || prace_is_(RACE_SPECTRE) )
			{
				msg_print("What a waste.  It's your food!");
			}
		}
		break;
	}	
	return "";
}

static cptr _do_wand(int sval, int mode)
{
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool ident = FALSE;
	bool old_target_pet = target_pet;
	int  dir;

	if (cast)
	{
		/* Aim */
		if (sval == SV_WAND_HEAL_MONSTER || sval == SV_WAND_HASTE_MONSTER)
			target_pet = TRUE;

		if (!get_aim_dir(&dir))
		{
			target_pet = old_target_pet;
			return NULL;
		}
		target_pet = old_target_pet;
		/* XXX Hack -- Wand of wonder can do anything before it */
		if (sval == SV_WAND_WONDER)
		{
			int vir = virtue_number(V_CHANCE);
			sval = randint0(SV_WAND_WONDER);

			if (vir)
			{
				if (p_ptr->virtues[vir - 1] > 0)
				{
					while (randint1(300) < p_ptr->virtues[vir - 1]) sval++;
					if (sval > SV_WAND_COLD_BALL) sval = randint0(4) + SV_WAND_ACID_BALL;
				}
				else
				{
					while (randint1(300) < (0-p_ptr->virtues[vir - 1])) sval--;
					if (sval < SV_WAND_HEAL_MONSTER) sval = randint0(3) + SV_WAND_HEAL_MONSTER;
				}
			}
			if (sval < SV_WAND_TELEPORT_AWAY)
				chg_virtue(V_CHANCE, 1);
		}
	}

	/* Fire! */
	switch (sval)
	{
	case SV_WAND_HEAL_MONSTER:
		if (desc) return "It heals a monster when you use it.";
		if (cast)
		{
			if (heal_monster(dir, _device_power_hack(damroll(10, 10)))) device_noticed = TRUE;
		}
		break;
	case SV_WAND_HASTE_MONSTER:
		if (desc) return "It hastes a monster when you use it.";
		if (cast)
		{
			if (speed_monster(dir)) device_noticed = TRUE;
		}
		break;
	case SV_WAND_CLONE_MONSTER:
		if (desc) return "It clones a monster when you use it. Unique monsters are not cloned.";
		if (cast)
		{
			if (clone_monster(dir)) device_noticed = TRUE;
		}
		break;
	case SV_WAND_TELEPORT_AWAY:
		if (desc) return "It fires a beam teleports all monsters on the line when you use it.";
		if (cast)
		{
			if (teleport_monster(dir)) device_noticed = TRUE;
		}
		break;
	case SV_WAND_DISARMING:
		if (desc) return "It fires a beam destroys all traps on the line when you use it.";
		if (cast)
		{
			if (disarm_trap(dir)) device_noticed = TRUE;
		}
		break;
	case SV_WAND_TRAP_DOOR_DEST:
		if (desc) return "It fires a beam destroys all traps and doors on the line when you use it.";
		if (cast)
		{
			if (destroy_door(dir)) device_noticed = TRUE;
		}
		break;
	case SV_WAND_STONE_TO_MUD:
		if (desc) return "It turns a door, rock, wall square to mud when you use it.";
		if (cast)
		{
			if (wall_to_mud(dir)) device_noticed = TRUE;
		}
		break;
	case SV_WAND_LITE:
		if (desc) return "It fires a line of light when you use it.";
		if (cast)
		{
			msg_print("A line of blue shimmering light appears.");
			(void)lite_line(dir);
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_SLEEP_MONSTER:
		if (desc) return "It puts a monster to sleep when you use it.";
		if (cast)
		{
			if (sleep_monster(dir)) device_noticed = TRUE;
		}
		break;
	case SV_WAND_SLOW_MONSTER:
		if (desc) return "It slows a monster down when you use it.";
		if (cast)
		{
			if (slow_monster(dir)) device_noticed = TRUE;
		}
		break;
	case SV_WAND_CONFUSE_MONSTER:
		if (desc) return "It confuses a monster when you use it.";
		if (cast)
		{
			if (confuse_monster(dir, _device_power_hack(p_ptr->lev))) device_noticed = TRUE;
		}
		break;
	case SV_WAND_FEAR_MONSTER:
		if (desc) return "It scares a monster when you use it.";
		if (cast)
		{
			if (fear_monster(dir, _device_power_hack(p_ptr->lev))) device_noticed = TRUE;
		}
		break;
	case SV_WAND_DRAIN_LIFE:
		if (desc) return "It fires a bolt that steals life from a foe when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(50 + p_ptr->lev/2));
		if (cast)
		{
			int dam = _device_power_hack(50 + p_ptr->lev/2);
			if (drain_life(dir, dam)) 
			{
				hp_player(dam);
				device_noticed = TRUE;
			}
		}
		break;
	case SV_WAND_POLYMORPH:
		if (desc) return "It changes a monster into another when you use it.";
		if (cast)
		{
			if (poly_monster(dir)) device_noticed = TRUE;
		}
		break;
	case SV_WAND_STINKING_CLOUD:
		if (desc) return "It fires a ball of poison when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(12 + p_ptr->lev/4));
		if (cast)
		{
			fire_ball(GF_POIS, dir, _device_power_hack(12 + p_ptr->lev/4), 2);
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_MAGIC_MISSILE:
		if (desc) return "It fires a bolt or beam of magic when you use it.";
		if (info) return info_damage(_device_power_hack(2 + p_ptr->lev/10), 6, 0);
		if (cast)
		{
			fire_bolt_or_beam(20, GF_MISSILE, dir, _device_power_hack(damroll(2 + p_ptr->lev/10, 6)));
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_ACID_BOLT:
		if (desc) return "It fires a bolt or beam of acid when you use it.";
		if (info) return info_damage(_device_power_hack(6 + p_ptr->lev/7), 8, 0);
		if (cast)
		{
			fire_bolt_or_beam(20, GF_ACID, dir, _device_power_hack(damroll(6 + p_ptr->lev/7, 8)));
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_CHARM_MONSTER:
		if (desc) return "It charms a monster into your pet when you use it.";
		if (cast)
		{
			if (charm_monster(dir, MAX(20, _device_power_hack(p_ptr->lev))))
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_FIRE_BOLT:
		if (desc) return "It fires a bolt or beam of fire when you use it.";
		if (info) return info_damage(_device_power_hack(7 + p_ptr->lev/6), 8, 0);
		if (cast)
		{
			fire_bolt_or_beam(20, GF_FIRE, dir, _device_power_hack(damroll(7 + p_ptr->lev/6, 8)));
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_COLD_BOLT:
		if (desc) return "It fires a bolt or beam of cold when you use it.";
		if (info) return info_damage(_device_power_hack(5 + p_ptr->lev/8), 8, 0);
		if (cast)
		{
			fire_bolt_or_beam(20, GF_COLD, dir, _device_power_hack(damroll(5 + p_ptr->lev/8, 8)));
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_ACID_BALL:
		if (desc) return "It fires a ball of acid when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(60 + 3*p_ptr->lev/4));
		if (cast)
		{
			fire_ball(GF_ACID, dir, _device_power_hack(60 + 3*p_ptr->lev/4), 2);
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_ELEC_BALL:
		if (desc) return "It fires a ball of lightning when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(40 + 3*p_ptr->lev/4));
		if (cast)
		{
			fire_ball(GF_ELEC, dir, _device_power_hack(40 + 3*p_ptr->lev/4), 2);
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_FIRE_BALL:
		if (desc) return "It fires a ball of fire when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(70 + 3*p_ptr->lev/4));
		if (cast)
		{
			fire_ball(GF_FIRE, dir, _device_power_hack(70 + 3*p_ptr->lev/4), 2);
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_COLD_BALL:
		if (desc) return "It fires a ball of cold when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(50 + 3*p_ptr->lev/4));
		if (cast)
		{
			fire_ball(GF_COLD, dir, _device_power_hack(50 + 3*p_ptr->lev/4), 2);
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_WONDER:
		if (desc) return "It has a random effect when you use it.";
		if (cast) msg_print("Oops.  Wand of wonder activated.");
		break;
	case SV_WAND_DRAGON_FIRE:
		if (desc) return "It breathes fire when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(200));
		if (cast)
		{
			fire_ball(GF_FIRE, dir, _device_power_hack(200), -3);
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_DRAGON_COLD:
		if (desc) return "It breathes cold when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(180));
		if (cast)
		{
			fire_ball(GF_COLD, dir, _device_power_hack(180), -3);
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_DRAGON_BREATH:
		if (desc) return "It breathes acid, lightning, fire, cold or poison when you use it.";
		if (info) return format("dam %d-%d", _device_power_hack(180), _device_power_hack(240));
		if (cast)
		{
			switch (randint1(5))
			{
			case 1: fire_ball(GF_ACID, dir, _device_power_hack(240), -3); break;
			case 2: fire_ball(GF_ELEC, dir, _device_power_hack(210), -3); break;
			case 3: fire_ball(GF_FIRE, dir, _device_power_hack(240), -3); break;
			case 4: fire_ball(GF_COLD, dir, _device_power_hack(210), -3); break;
			case 5: fire_ball(GF_POIS, dir, _device_power_hack(180), -3); break;
			}
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_DISINTEGRATE:
		if (desc) return "It fires a ball of disintegration when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(200 + p_ptr->lev*2));
		if (cast)
		{
			fire_ball(GF_DISINTEGRATE, dir, _device_power_hack(200 + randint1(p_ptr->lev * 2)), 2);
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_ROCKETS:
		if (desc) return "It fires a rocket when you use it.";
		if (info) return info_damage(0, 0, _device_power_hack(250 + p_ptr->lev*3));
		if (cast)
		{
			msg_print("You launch a rocket!");
			fire_rocket(GF_ROCKET, dir, _device_power_hack(250 + p_ptr->lev*3), 2);
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_STRIKING:
		if (desc) return "It fires a bolt of meteor when you use it.";
		if (info) return info_damage(_device_power_hack(15 + p_ptr->lev/3), 13, 0);
		if (cast)
		{
			fire_bolt(GF_METEOR, dir, _device_power_hack(damroll(15 + p_ptr->lev/3, 13)));
			device_noticed = TRUE;
		}
		break;
	case SV_WAND_GENOCIDE:
		if (desc) return "It removes a monster from current dungeon level unless resisted when you use it.";
		if (cast)
		{
			fire_ball_hide(GF_GENOCIDE, dir, magic_eater_hack ? p_ptr->lev + 50 : device_power(250), 0);
			device_noticed = TRUE;
		}
		break;
	}
	return "";
}

static cptr _do_rod(int sval, int mode)
{
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool ident = FALSE;
	int dir;

	if (cast)
	{
		if (!device_known && !get_aim_dir(&dir)) return NULL;
	}

	switch (sval)
	{
	case SV_ROD_DETECT_MONSTERS:
		if (desc) return "It detects all monsters in your vicinity when you zap it.";
		if (cast)
		{
			if (detect_monsters_normal(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_DETECT_TRAP:
		if (desc) return "It detects all traps in your vicinity when you zap it.";
		if (cast)
		{
			if (detect_traps(DETECT_RAD_DEFAULT, device_known)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_DETECT_DOOR:
		if (desc) return "It detects all doors and stairs in your vicinity when you zap it.";
		if (cast)
		{
			if (detect_doors(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
			if (detect_stairs(DETECT_RAD_DEFAULT)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_IDENTIFY:
		if (desc) return "It identifies an item when you zap it.";
		if (cast)
		{
			if (!ident_spell(NULL)) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_RECALL:
		if (desc) return "It recalls you to the town, or back into the dungeon you have entered when you zap it.";
		if (cast)
		{
			if (!word_of_recall()) return NULL;
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_ILLUMINATION:
		if (desc) return "It lights up nearby area or current room permanently when you zap it.";
		if (cast)
		{
			if (lite_area(_device_power_hack(damroll(2, 8)), 2)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_MAPPING:
		if (desc) return "It maps your vicinity when you zap it.";
		if (cast)
		{
			map_area(DETECT_RAD_MAP);
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_DETECTION:
		if (desc) return "It detects all traps, doors, stairs, treasures, items and monsters in the neighborhood when you zap it.";
		if (cast)
		{
			detect_all(DETECT_RAD_DEFAULT);
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_PROBING:
		if (desc) return "It probes all monsters' alignment, HP, AC, speed, current experience and true character in sight when you zap it.";
		if (cast)
		{
			probing();
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_CURING:
		if (desc) return "It cures blindness, poison, confusion, stunned, cuts, hallucination and berserk when you zap it.";
		if (info) return info_heal(0, 0, _device_power_hack(50));
		if (cast)
		{
			if (hp_player(_device_power_hack(50))) device_noticed = TRUE;
			if (set_blind(0, TRUE)) device_noticed = TRUE;
			if (set_poisoned(0, TRUE)) device_noticed = TRUE;
			if (set_confused(0, TRUE)) device_noticed = TRUE;
			if (set_stun(0, TRUE)) device_noticed = TRUE;
			if (set_cut(0, TRUE)) device_noticed = TRUE;
			if (set_image(0, TRUE)) device_noticed = TRUE;
			if (set_shero(0,TRUE)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_HEALING:
		if (desc) return "It heals you and cures stunned, cuts and berserk when you zap it.";
		if (info) return info_heal(0, 0, _device_power_hack(500));
		if (cast)
		{
			if (hp_player(_device_power_hack(500))) device_noticed = TRUE;
			if (set_stun(0, TRUE)) device_noticed = TRUE;
			if (set_cut(0, TRUE)) device_noticed = TRUE;
			if (set_shero(0,TRUE)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_RESTORATION:
		if (desc) return "It restores experience and all your stats when you zap it.";
		if (cast)
		{
			if (restore_level()) device_noticed = TRUE;
			if (do_res_stat(A_STR)) device_noticed = TRUE;
			if (do_res_stat(A_INT)) device_noticed = TRUE;
			if (do_res_stat(A_WIS)) device_noticed = TRUE;
			if (do_res_stat(A_DEX)) device_noticed = TRUE;
			if (do_res_stat(A_CON)) device_noticed = TRUE;
			if (do_res_stat(A_CHR)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_SPEED:
		if (desc) return "It hastes you temporarily when you zap it.";
		if (info) return info_duration(_device_power_hack(15), _device_power_hack(30));
		if (cast)
		{
			if (set_fast(_device_power_hack(randint1(30) + 15), FALSE)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_PESTICIDE:
		if (desc) return "It does slight damage to all monsters in sight when you zap it.";
		if (cast)
		{
			if (dispel_monsters(_device_power_hack(4))) device_noticed = TRUE;
		}
		break;
	case SV_ROD_TELEPORT_AWAY:
		if (desc) return "It fires a beam teleports all monsters on the line when you zap it.";
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			if (teleport_monster(dir)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_DISARMING:
		if (desc) return "It fires a beam that destroys all traps on the line when you zap it.";
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			if (disarm_trap(dir)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_LITE:
		if (desc) return "It fires a line of light when you zap it.";
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			msg_print("A line of blue shimmering light appears.");
			(void)lite_line(dir);
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_SLEEP_MONSTER:
		if (desc) return "It puts a monster to sleep when you zap it.";
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			if (sleep_monster(dir)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_SLOW_MONSTER:
		if (desc) return "It slows a monster down when you zap it.";
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			if (slow_monster(dir)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_DRAIN_LIFE:
		if (desc) return "It fires a bolt that steals life from a foe when you zap it.";
		if (info) return info_damage(0, 0, _device_power_hack(60 + p_ptr->lev/2));
		if (cast)
		{
			int dam = _device_power_hack(60 + p_ptr->lev/2);
			if (device_known && !get_aim_dir(&dir)) return NULL;
			if (drain_life(dir, dam)) 
			{
				hp_player(dam);
				device_noticed = TRUE;
			}
		}
		break;
	case SV_ROD_POLYMORPH:
		if (desc) return "It changes a monster into another when you zap it.";
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			if (poly_monster(dir)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_ACID_BOLT:
		if (desc) return "It fires a bolt or beam of acid when you zap it.";
		if (info) return info_damage(_device_power_hack(6 + p_ptr->lev/7), 8, 0);
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			fire_bolt_or_beam(10, GF_ACID, dir, _device_power_hack(damroll(6 + p_ptr->lev/7, 8)));
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_ELEC_BOLT:
		if (desc) return "It fires a bolt or beam of lightning when you zap it.";
		if (info) return info_damage(_device_power_hack(4 + p_ptr->lev/9), 8, 0);
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			fire_bolt_or_beam(10, GF_ELEC, dir, _device_power_hack(damroll(4 + p_ptr->lev/9, 8)));
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_FIRE_BOLT:
		if (desc) return "It fires a bolt or beam of fire when you zap it.";
		if (info) return info_damage(_device_power_hack(7 + p_ptr->lev/6), 8, 0);
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			fire_bolt_or_beam(10, GF_FIRE, dir, _device_power_hack(damroll(7 + p_ptr->lev/6, 8)));
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_COLD_BOLT:
		if (desc) return "It fires a bolt or beam of cold when you zap it.";
		if (info) return info_damage(_device_power_hack(5 + p_ptr->lev/8), 8, 0);
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			fire_bolt_or_beam(10, GF_COLD, dir, _device_power_hack(damroll(5 + p_ptr->lev/8, 8)));
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_ACID_BALL:
		if (desc) return "It fires a ball of acid when you zap it.";
		if (info) return info_damage(0, 0, _device_power_hack(60 + p_ptr->lev));
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			fire_ball(GF_ACID, dir, _device_power_hack(60 + p_ptr->lev), 2);
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_ELEC_BALL:
		if (desc) return "It fires a ball of lightning when you zap it.";
		if (info) return info_damage(0, 0, _device_power_hack(40 + p_ptr->lev));
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			fire_ball(GF_ELEC, dir, _device_power_hack(40 + p_ptr->lev), 2);
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_FIRE_BALL:
		if (desc) return "It fires a ball of fire when you zap it.";
		if (info) return info_damage(0, 0, _device_power_hack(70 + p_ptr->lev));
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			fire_ball(GF_FIRE, dir, _device_power_hack(70 + p_ptr->lev), 2);
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_COLD_BALL:
		if (desc) return "It fires a ball of cold when you zap it.";
		if (info) return info_damage(0, 0, _device_power_hack(50 + p_ptr->lev));
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			fire_ball(GF_COLD, dir, _device_power_hack(50 + p_ptr->lev), 2);
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_HAVOC:
		if (desc) return "It is capable of firing almost anything, at random.";
		if (cast)
		{
			call_chaos();
			device_noticed = TRUE;
		}
		break;
	case SV_ROD_STONE_TO_MUD:
		if (desc) return "It turns a door, rock and wall square to mud when you zap it.";
		if (cast)
		{
			if (device_known && !get_aim_dir(&dir)) return NULL;
			if (wall_to_mud(dir)) device_noticed = TRUE;
		}
		break;
	case SV_ROD_AGGRAVATE:
		if (desc) return "It aggravates monsters in your vicinity when you zap it.";
		if (cast)
		{
			aggravate_monsters(0);
			device_noticed = TRUE;
		}
		break;
	}
	return "";
}

cptr do_device(int tval, int sval, int mode)
{
	device_noticed = FALSE;
	switch (tval)
	{
	case TV_STAFF: return _do_staff(sval, mode);
	case TV_WAND: return _do_wand(sval, mode);
	case TV_ROD: return _do_rod(sval, mode);
	case TV_SCROLL: return _do_scroll(sval, mode);
	}
	device_known = FALSE;
	return NULL;
}

