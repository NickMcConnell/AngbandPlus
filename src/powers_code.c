#include "spells.h"
#include "target.h"
#include "powers_defines.h"
#include "attack.h"
#include "effects.h"
#include "monster/monster.h"

/* Actually use power */
bool use_power(int power)
{
	int py = p_ptr->py, y;
	int px = p_ptr->px, x;

	int dir, i, count;//, j, item, squelch;
	int plev = p_ptr->lev;
	
	//bool fear;
	
	char m_name[80];//, o_name[100];
	
	monster_race *r_ptr;
	monster_type *m_ptr;
	//object_type object_type_body;
	//object_type *o_ptr;

	/* Hack -- chance of "beam" instead of "bolt" */
	int beam = plev; /* too much? */

	//monster_race *mr_ptr = &r_info[rp_ptr->p_monster_index];

	p_ptr->energy_use = 0;
        
        switch (power)
        {

			case PWR_ABSORB_MANA:
			{
				i = 0;
				for (dir = 0; dir < 8; dir++)
				{
					y = p_ptr->py + ddy_ddd[dir];
					x = p_ptr->px + ddx_ddd[dir];
					if (cave_empty_bold(y, x)) i++;
				}
			
				/* Require at least 4 empty grids */
				if (i < 4)
				{
					msg_print("There is not enough room to absorb mana from.");
					return (FALSE);
				}

				for (; i > 0; i--)
				{
					p_ptr->csp += plev / 2;
				}

				if (p_ptr->csp >= p_ptr->msp)
				{
					p_ptr->csp = p_ptr->msp;
					p_ptr->csp_frac = 0;
				}

				p_ptr->energy_use = 500;
			}
			break;
			
			case PWR_ALTER_REALITY:
			{
				msg_print("The world changes!");
				p_ptr->leaving = TRUE;
			}
			break;
			 
			case PWR_ATTACK_DIST:
			{
			/*
				bool all_polearms = TRUE, anything_wielded = FALSE;

				for (i = INVEN_WIELD; i < INVEN_BOW; i++)
				{
					o_ptr = &inventory[i];
					if (!o_ptr->k_idx) continue;
					else anything_wielded = TRUE;
					if (o_ptr->tval != TV_POLEARM)all_polearms = FALSE;
				}

				if (!anything_wielded)
				{
					msg_print("You must wield something.");
					return(FALSE);
				}

				if (!all_polearms)
				{
					msg_print("You can do that only with a polearm.");
					return (FALSE);
				}
				
				if (!target_set_interactive(TARGET_KILL)) return (FALSE);
				if (!p_ptr->target_who) return (FALSE);
				
				m_ptr = &mon_list[p_ptr->target_who];
				if (m_ptr->cdis > 2)
				{
					msg_print("This monster is too far.");
					return (FALSE);
				}
				
				msg_print("You attack a monster nearby.");
				py_attack(m_ptr->fy, m_ptr->fx);
		*/	}
			break;
			 
			case PWR_ATTACK_SUPER:
			{
				if (!get_rep_dir(&dir)) return (FALSE);
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				
				if (!cave_m_idx[y][x])
				{
					msg_print("There are no monsters here!");
					return (FALSE);
				}
				
				p_ptr->state.to_h += 10;
				p_ptr->state.to_d += plev * 2 / 3;
				msg_print("You hit the monster with full force!");
				py_attack(y, x);
				p_ptr->state.to_h -= 10;
				p_ptr->state.to_d -= plev * 2 / 3;
			}
			break;
			 
			case PWR_ATTACK_WHIRL:
			{
				msg_print("You turn around and hit all nearby enemies at once!");
				count = 0;
				
				for (i = 0; i < 8; i++)
				{
					if (cave_m_idx[p_ptr->py + ddy_ddd[i]][p_ptr->px + ddx_ddd[i]])
					{
						py_attack(p_ptr->py + ddy_ddd[i], p_ptr->px + ddx_ddd[i]);
						count++;
					}
				}
				
				if (!count)
				{
					msg_print("...Unfortunately there are no enemies near you.");
					return (FALSE);
				}
			}
			break;
			 
			case PWR_AWAKE_TREE:
			{
				/* Target */
		/*		if (!target_set_interactive(TARGET_GRID)) return (FALSE);

				msg_print("You awake the trees!");
				project_star(-1, 1 + p_ptr->lev / 20, p_ptr->target_row, p_ptr->target_col, plev, GF_AWAKE_TREE, 0);
		*/	}
			break;
			 
			case PWR_BASILISK_GAZE:
			{
				if (!get_rep_dir(&dir)) return (FALSE);
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				
				if (!cave_m_idx[y][x])
				{
					msg_print("There are no monsters here!");
					return (FALSE);
				}
				
				m_ptr = &mon_list[cave_m_idx[y][x]];
				r_ptr = &r_info[m_ptr->r_idx];
				monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);

				msg_format("You glare at %s with your deadly gaze!", m_name);
			
				/* Don't harm non-living or quester mobs */
				/* deleted (monster_nonliving(r_ptr)) -Simon */
				if (monster_is_unusual(r_ptr) || RF_QUESTOR)
				{
					msg_print("Nothing happens.");
					return (TRUE);  /* Mana is used anyway. */
				}
				
				/* Saving throw. Pretty straightforward */
				/* Hacks -- uniques are very unlikely to be affected */
				if (randint0(plev * 2 + 10) < r_ptr->level * (RF_UNIQUE ? 3 : 1))
				{
					msg_format("%^s resists the effects!", m_name);
				}
				else
				{
					msg_format("%^s is turned to stone.", m_name);
			
					/* Hack -- remember the monster race index */
					i = m_ptr->r_idx;
					
					/* BAMF! The monster disappears in a puff of smoke */
					delete_monster(y, x);

					/* Create a statue */
					//o_ptr = &object_type_body;
					//object_wipe(o_ptr);
					//object_prep(o_ptr, lookup_kind(TV_STATUE, 0));
					//o_ptr->pval = i;
					
					/*
					 * Statue is 5x heavier than corpse. If corpse is undefined,
					 * assume that the statue weighs 250lb
					 */
					//o_ptr->weight = (r_ptr->body.weight ? r_ptr->body.weight * 5 : 250);

					/* ...Here it is! */
					//drop_near(o_ptr, -1, y, x);
				}
			}
			break;
		 
			case PWR_BEAM_OF_LIGHT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("A line of blue shimmering light appears.");
				light_line(dir);
			}
			break;
			 
			case PWR_BERSERK:
			{
				(void)hp_player(30);
				(void)inc_timed(TMD_SHERO, randint1(25) + 25, TRUE);
				(void)clear_timed(TMD_AFRAID, TRUE);
			}
			break;
 
			case PWR_BLINK:
			{
				msg_print("You blink away.");
				teleport_player(10);
			}
			break;
	 
			case PWR_BRAIN_SMASH:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You look deep into monster's eyes.");
				/* XXX XXX XXX This makes three separate bolts - not good... */
				(void)slow_monster(dir);
				(void)confuse_monster(dir, plev, TRUE);
				(void)fear_monster(dir, plev, TRUE);

			}
			break;
		
			case PWR_BR_ACID_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe acid.");
				fire_ball(GF_ACID, dir, plev * 6, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
			 
			case PWR_BR_ACID_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe an acid bolt.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a sizzling stream of acid!");
					fire_beam(GF_ACID, dir, damroll((plev * 2/3), 11));
				}
				else
					fire_bolt(GF_ACID, dir, damroll((plev * 2/3), 11));
			}
			break;
			 
			case PWR_BR_CHAOS_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe Chaos.");
				fire_ball(GF_CHAOS, dir, plev * 4, 1 + plev / 15); //8 + plev / 10, plev > 30 ? 45 : 30); was fire_arc
			}
			break;
			 
			case PWR_BR_CHAOS_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a bolt of Chaos.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of insanity!");
					fire_beam(GF_CHAOS, dir, damroll((plev * 2/3), 7));
				}
				else
					fire_bolt(GF_CHAOS, dir, damroll((plev * 2/3), 7));
			}
			break;
			 
			case PWR_BR_COLD_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe frost.");
				fire_ball(GF_COLD, dir, plev * 6, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
			 
			case PWR_BR_COLD_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a frost bolt.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a chillingly cold beam!");
					fire_beam(GF_COLD, dir, damroll((plev * 2/3), 11));
				}
				else
					fire_bolt(GF_COLD, dir, damroll((plev * 2/3), 11));
			}
			break;
			 
			case PWR_BR_CONFU_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe confusion gas.");
				fire_ball(GF_CONFUSION, dir, plev * 3, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
		 
			case PWR_BR_CRYO_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe freezing air.");
				//fire_ball(GF_CRYO, dir, plev * 2, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
			 
			case PWR_BR_DARK_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe darkness.");
				fire_ball(GF_DARK, dir, plev * 3, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
			 
			case PWR_BR_DARK_BOLT:
			{

				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a bolt of darkness.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of unearthly shadows!");
					fire_beam(GF_DARK, dir,
						  damroll(plev * 2/3, 5));
				}
				else
					fire_bolt(GF_DARK, dir, damroll(plev * 2/3, 5));

			}
			break;
			
			case PWR_BR_DISEN_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe nothingness.");
				fire_ball(GF_DISENCHANT, dir, plev * 3, 1 + plev / 15); //8 + plev / 10, plev > 30 ? 45 : 30); was fire_arc
			}
			break;

			case PWR_BR_DISEN_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a bolt of nothingness.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of the Void!");
					fire_beam(GF_DISENCHANT, dir, damroll(plev * 2/3, 5));
				}
				else
					fire_bolt(GF_DISENCHANT, dir, damroll(plev * 2/3, 5));
			}
			break;
	 
			case PWR_BR_ELEC_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe lighning.");
				fire_ball(GF_ELEC, dir, plev * 6, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
			 
			case PWR_BR_ELEC_BOLT:
			{
				/* Always beams, like mage spell */
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a long spark of lightning.");
				fire_beam(GF_ELEC, dir, damroll((plev * 2/3), 11));
			}
			break;
		 
			case PWR_BR_FEAR_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe pure fear.");
				fire_ball(GF_TURN_ALL, dir, plev * 6, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
	 
			case PWR_BR_FIRE_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe fire.");
				fire_ball(GF_FIRE, dir, plev * 6, 1 + plev / 15); //8 + plev / 10);//, plev > 30 ? 45 : 30);
			}
			break;
			 
			case PWR_BR_FIRE_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a fire bolt.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of hellfire!");
					fire_beam(GF_FIRE, dir, damroll((plev * 2/3), 11));
				}
				else
				fire_bolt(GF_FIRE, dir, damroll((plev * 2/3), 11));
			}
			break;
			 
	 
			case PWR_BR_FORCE_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe force.");
				fire_ball(GF_FORCE, dir, plev * 5/2, 1 + plev / 15); //8 + plev / 10);//, plev > 30 ? 45 : 30);
			}
			break;
			 
			case PWR_BR_FORCE_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a force bolt.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of kinetic energy!");
					fire_beam(GF_FORCE, dir, damroll(plev * 2/3, 4));
				}
				else
				fire_bolt(GF_FORCE, dir, damroll(plev * 2/3, 4));
			}
			break;
			 
			case PWR_BR_GRAVITY_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe gravity.");
				fire_ball(GF_GRAVITY, dir, plev * 5/2, 1 + plev / 15); //8 + plev / 10);//, plev > 30 ? 45 : 30);
			}
			break;
			 
			case PWR_BR_GRAVITY_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a gravity bolt.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of heaviness!");
					fire_beam(GF_GRAVITY, dir, damroll(plev * 2/3, 4));
				}
				else
				fire_bolt(GF_GRAVITY, dir, damroll(plev * 2/3, 4));
			}
			break;
			 
			case PWR_BR_ICE_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe ice.");
				fire_ball(GF_ICE, dir, plev * 7, 1 + plev / 15); //8 + plev / 10);//, plev > 30 ? 45 : 30);
			}
			break;
	 
			case PWR_BR_INERTIA_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe inertia.");
				fire_ball(GF_INERTIA, dir, plev * 3, 1 + plev / 15); //8 + plev / 10);//, plev > 30 ? 45 : 30);
			}
			break;
	 			 
			case PWR_BR_INERTIA_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe an inertia bolt.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of stasis!");
					fire_beam(GF_INERTIA, dir, damroll(plev * 2/3, 5));
				}
				else
				fire_bolt(GF_INERTIA, dir, damroll(plev * 2/3, 5));
			}
			break;
			
			case PWR_BR_LITE_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe light.");
				fire_ball(GF_LIGHT, dir, plev * 3, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
			 
			case PWR_BR_LITE_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a bolt of light.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of blinding light!");
					fire_beam(GF_LIGHT, dir, damroll(plev * 2/3, 5));
				}
				else
					fire_bolt(GF_LIGHT, dir, damroll(plev * 2/3, 5));
			}
			break;
			 
			case PWR_BR_MANA_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe raw mana.");
				fire_ball(GF_MANA, dir, plev * 7/2, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
			
			case PWR_BR_MANA_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a bolt of mana.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of raw magic!");
					fire_beam(GF_MANA, dir, damroll(plev * 2/3, 6));
				}
				else
					fire_bolt(GF_MANA, dir, damroll(plev * 2/3, 6));
			}
			break;		
			 
			case PWR_BR_NETHR_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe nether.");
				fire_ball(GF_NETHER, dir, plev * 5, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
			 
			case PWR_BR_NETHR_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a bolt of nether.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of death!");
					fire_beam(GF_NETHER, dir, damroll(plev * 2/3, 9));
				}
				else
					fire_bolt(GF_NETHER, dir, damroll(plev * 2/3, 9));
			}
			break;
		 
			case PWR_BR_NEXUS_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe nexus.");
				fire_ball(GF_NEXUS, dir, plev * 4, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
	 			 
			case PWR_BR_NEXUS_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a nexus bolt.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of warped space-time!");
					fire_beam(GF_NEXUS, dir, damroll((plev * 2/3), 7));
				}
				else
				fire_bolt(GF_NEXUS, dir, damroll((plev * 2/3), 7));
			}
			break;
			
			case PWR_BR_PLASMA_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe plasma.");
				fire_ball(GF_PLASMA, dir, plev * 5/2, 1 + plev / 15); //8+ plev / 10);//, plev > 30 ? 45 : 30);
			}
			break;
 			
			case PWR_BR_PLASMA_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a plasma bolt.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of charged particles!");
					fire_beam(GF_PLASMA, dir, damroll(plev * 2/3, 4));
				}
				else
				fire_bolt(GF_PLASMA, dir, damroll(plev * 2/3, 4));
			}
			break;
			
			case PWR_BR_POIS_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe gas.");
				fire_ball(GF_POIS, dir, plev * 5, 1 + plev / 15); //8 + plev / 10);//, plev > 30 ? 45 : 30);
			}
			break;
			
			case PWR_BR_POIS_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a poison bolt.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of noxious gases!");
					fire_beam(GF_POIS, dir, damroll(plev * 2/3, 9));
				}
				else
				fire_bolt(GF_POIS, dir, damroll(plev * 2/3, 9));
			}
			break;

			case PWR_BR_SHARD_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe shards.");
				fire_ball(GF_SHARD, dir, plev * 7/2, 1 + plev / 15); //8 + plev / 10);//, plev > 30 ? 45 : 30);
			}
			break;
			
			case PWR_BR_SHARD_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a bolt of shards.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of blades!");
					fire_beam(GF_SHARD, dir, damroll(plev * 2/3, 6));
				}
				else
				fire_bolt(GF_SHARD, dir, damroll(plev * 2/3, 6));
			}
			break;

			case PWR_BR_SOUND_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("Your thunderous roar creates a shockwave.");
				fire_ball(GF_SOUND, dir, plev * 5/2, 1 + plev / 15); //8 + plev / 10);//, plev > 30 ? 45 : 30);
			}
			break;

			case PWR_BR_SOUND_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You roar a bolt of sound.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of deafening noise!");
					fire_beam(GF_SOUND, dir, damroll(plev * 2/3, 4));
				}
				else
				fire_bolt(GF_SOUND, dir, damroll(plev * 2/3, 4));
			}
			break;

			case PWR_BR_TIME_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe time.");
				fire_ball(GF_TIME, dir, plev * 3, 1 + plev / 15); //8 + plev / 10);//, plev > 30 ? 45 : 30);
			}
			break;

			case PWR_BR_TIME_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe a bolt of time.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of aging!");
					fire_beam(GF_TIME, dir, damroll(plev * 2/3, 5));
				}
				else
				fire_bolt(GF_TIME, dir, damroll(plev * 2/3, 5));
			}
			break;

			case PWR_BR_WIND_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You breathe wind.");
				//fire_ball(GF_WIND, dir, plev * 6, 1 + plev / 15);//, plev > 30 ? 45 : 30);
			}
			break;
			 
			case PWR_CALL_LIGHT:
			{
				(void)light_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			}
			break;
			 
			case PWR_CAST_ACID_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a bolt of acid.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of liquid death!");
					fire_beam(GF_ACID, dir, damroll((plev < 10 ? 0 : plev - 10) * 2/3 + 4, 10));
				}
				else
					fire_bolt(GF_ACID, dir, damroll((plev < 10 ? 0 : plev - 10) * 2/3 + 4, 10));
			}
			break;
			 
			case PWR_CAST_COLD_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a ball of frost.");
				fire_ball(GF_COLD, dir, plev * 4, plev > 35 ? 3 : 2);
			}
			break;
		 
			case PWR_CAST_COLD_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a bolt of frost.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a chillingly cold beam!");
					fire_beam(GF_COLD, dir, damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
				}
				else
					fire_bolt(GF_COLD, dir, damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
			}
			break;
			 
			case PWR_CAST_DARK_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a ball of darkness.");
				fire_ball(GF_DARK, dir, plev * 3, plev > 35 ? 3 : 2);
			}
			break;
			 
			case PWR_CAST_DARK_STORM:
			{
				msg_print("You invoke a storm of darkness.");
				fire_ball(GF_DARK, 0, plev * 5, 20);
			}
			break;
			 
			case PWR_CAST_DISENCHANT_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a bolt of nothingness.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of the Void!");
					fire_beam(GF_DISENCHANT, dir,
						  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
				}
				else
					fire_bolt(GF_DISENCHANT, dir,
						  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
			}
			break;
			 
			case PWR_CAST_ELEC_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure an electric spark.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a bright arc!");
					fire_beam(GF_ELEC, dir,
						  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
				}
				else
					fire_bolt(GF_ELEC, dir,
						  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
			}
			break;
			 
			case PWR_CAST_ELEC_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure ball lightning.");
				fire_ball(GF_ELEC, dir, plev * 5, plev > 35 ? 3 : 2);
			}
			break;
			 
			case PWR_CAST_ELEC_BEAM:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You release a blinding lighning bolt!");
				fire_beam(GF_ELEC, dir,
					  damroll((p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3, 12));
			}
			break;
		 
			case PWR_CAST_FIRE_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a ball of fire.");
				fire_ball(GF_FIRE, dir, plev * 4, plev > 35 ? 3 : 2);
			}
			break;
			 
			case PWR_CAST_FIRE_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a bolt of fire.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a searingly hot beam!");
					fire_beam(GF_FIRE, dir,
						  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
				}
				else
					fire_bolt(GF_FIRE, dir, damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
			}
			break;
			 
			case PWR_CAST_FIRE_JUMP:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				
				/* Ensure there is a monster target */
				if (!target_okay())// || !p_ptr->target_who)
				{
					msg_print("You must target a monster!");
					return (FALSE);
				}
				
				msg_print("You conjure a small jumping fireball!");
				//fire_jump_ball(GF_FIRE, plev * 8, 1, 75, 25);
			}
			break;
		 
			case PWR_CAST_ICE_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You invoke a powerful ice storm!");
				fire_ball(GF_ICE, dir, plev * 3, plev > 40 ? 4 : 3);
			}
			break;
			 
			case PWR_CAST_ICE_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a bolt of ice.");
				fire_bolt(GF_ICE, dir, damroll((p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3, 12));
			}
			break;
			 
			case PWR_CAST_LITE_ORB:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You throw a sphere of light.");
				fire_ball(GF_LIGHT, dir, plev * 5, 1);
			}
			break;
		 
			case PWR_CAST_MAGIC_MISSILE:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You cast a magic missile.");
				/* Nevermind beam description... */
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir, damroll(3 + (plev - 1) / 5, 4));
			}
			break;
			 
			case PWR_CAST_MANA_BOLT_1:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You cast a magical bolt.");
				fire_bolt(GF_MISSILE, dir,  damroll((p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) / 5 + 3, 10));
			}
			break;
		 
			case PWR_CAST_MANA_BOLT_2:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You cast a bolt of pure energy.");
				fire_bolt(GF_MISSILE, dir,  damroll((p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) / 4 + 3, 14));
			}
			break;
		 
			case PWR_CAST_MANA_STORM:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You invoke the mana storm!");
				fire_ball(GF_MANA, dir, plev * 7, 5);
			}
			break;
			 
			case PWR_CAST_NETHER_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You blast your enemies with an orb of undeath!");
				fire_ball(GF_NETHER, dir, plev * 9/2, plev > 30 ? 3 : 2);
			}
			break;
			 
			case PWR_CAST_NETHER_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a bolt of nether forces.");
				if (randint0(100) < beam)
				{
					msg_print("It turns into a beam of pure undeath!");
					fire_beam(GF_NETHER, dir, damroll((plev < 11 ? 1 : plev - 9), 8));
				}
				else
					fire_bolt(GF_NETHER, dir, damroll((plev < 11 ? 1 : plev - 9), 8));
			}
			break;
		 
			case PWR_CAST_NEXUS_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You invoke a powerful nexus storm!");
				fire_ball(GF_NEXUS, dir, plev * 4, plev > 40 ? 4 : 3);
			}
			break;
			 
			case PWR_CAST_NEXUS_BURST:
			{
				msg_print("You disrupt space!");
				//project_star(-1, 4, p_ptr->py, p_ptr->px, plev * 7 / 2, GF_NEXUS, PROJECT_PASS);
			}
			break;
			 
			case PWR_CAST_PLASMA_BOLT:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a bolt of burning plasma.");
				fire_bolt(GF_PLASMA, dir, damroll((p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3, 12));
			}
			break;

			case PWR_CAST_PLASMA_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You invoke a powerful plasma storm!");
				fire_ball(GF_PLASMA, dir, plev * 6, plev > 40 ? 4 : 3);
			}
			break;
			 
			case PWR_CAST_STINKING_CLOUD:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a cloud of poisonous gas.");
				fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
			}
			break;
			
			case PWR_CAST_WATER_BALL:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You engulf your foes in whirlpool!");
				fire_ball(GF_WATER, dir, plev * 7/2, plev > 45 ? 3 : 2);
			}
			break;
			 
			case PWR_CAST_WATER_BEAM:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("Water streams through dungeon!");
				fire_beam(GF_WATER, dir, damroll((p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3, 12));
			}
			break;
			 
			case PWR_CAST_WIND_BURST:
			{
				msg_print("You invoke a cyclone!");
				//project_star(-1, 4, p_ptr->py, p_ptr->px, plev * 7 / 2, GF_WIND, PROJECT_PASS);
			}
				break;
			 
			case PWR_CAST_WIND_ORB:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You conjure a powerful blast of wind.");
				//fire_ball(GF_WIND, dir, plev * 4, 1);
			}
			break;
			 
			case PWR_CONFUSE:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You try to confuse the monster.");
				(void)confuse_monster(dir, plev, TRUE);
			}
			break;
			 
			case PWR_CURE_CRITICAL:
			{
				(void)heal_player(25, 30);
				(void)clear_timed(TMD_CUT, TRUE);
				(void)clear_timed(TMD_AMNESIA, TRUE);
				// not possible to use while conf'd -Simon
				//(void)clear_timed(TMD_CONFUSED, TRUE);
				(void)clear_timed(TMD_BLIND, TRUE);
				(void)clear_timed(TMD_POISONED, TRUE);
				(void)clear_timed(TMD_STUN, TRUE);
			}
			break;
			 
			case PWR_CURE_LIGHT:
			{
				msg_print("You cure light wounds on your body.");
				(void)heal_player(15, 15);
				(void)dec_timed(TMD_CUT, 20, TRUE);
				//shouldn't be possible to use while conf'ed -Simon
				//(void)dec_timed(TMD_CONFUSED, 20, TRUE);
				(void)clear_timed(TMD_BLIND, TRUE);
			}
			break;
			 
			case PWR_CURE_SERIOUS:
			{
				msg_print("You cure serious wounds on your body.");
				(void)heal_player(20, 25);
				(void)clear_timed(TMD_CUT, TRUE);
				//shouldn't be possible to use while conf'd -Simon
				//(void)clear_timed(TMD_CONFUSED, TRUE);
				(void)clear_timed(TMD_BLIND, TRUE);
			}
			break;
		/*	 
			case PWR_CURSE_BIG:
			{
		*/		/* Hack -- directly affect monster */
		/*		if (!get_aim_dir(&dir)) return (FALSE);
				if (!p_ptr->target_who) return (FALSE);
				m_ptr = &mon_list[p_ptr->target_who];
				r_ptr = &r_info[m_ptr->r_idx];
				x = m_ptr->fx;
				y = m_ptr->fy;
				monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
				msg_format("You point at %s, incanting terribly.", m_name);
		*/		/* It's hard not to wake up if someone is trying to sacrifice you... */
		//		m_ptr->csleep = 0;
				/* Saving throw. Pretty straightforward */
		/*		if (randint0(plev * 2 + 70) < r_ptr->level)
				{
					msg_format("%^s resists the effects!", m_name);
				}
				else
				{
					if (mon_take_hit(p_ptr->target_who, damroll(15, 20), &fear, " is consumed by the dark flames!", -1))
					{
						project_ball(-1, 1, y, x, y, x, 100, GF_DARK, 0, 0);
					}
				}
			}
			break;
		 
			case PWR_CURSE_MED:
			{
		*/		/* Hack -- directly affect monster */
		/*		if (!get_aim_dir(&dir)) return (FALSE);
				if (!p_ptr->target_who) return (FALSE);
				m_ptr = &mon_list[p_ptr->target_who];
				r_ptr = &r_info[m_ptr->r_idx];
				x = m_ptr->fx;
				y = m_ptr->fy;
				monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
				msg_format("You point at %s and curse horribly.", m_name);
		*/		/* It's hard not to wake up if someone is trying to sacrifice you... */
		//		m_ptr->csleep = 0;
				/* Saving throw. Pretty straightforward */
		/*		if (randint0(plev * 2 + 60) < r_ptr->level)
				{
					msg_format("%^s resists the effects!", m_name);
				}
				else
				{
		*/			/* Additional effect when the monster dies */
		/*			if (mon_take_hit(p_ptr->target_who, damroll(8, 10), &fear, " vanishes in black mist.", -1))
					{
						project_ball(-1, 1, y, x, y, x, 40, GF_NETHER, 0, 0);
					}
				}
			}
			break;
		 
			case PWR_CURSE_MORTAL:
			{

		*/		/* Hack -- directly affect monster */
		/*		if (!get_aim_dir(&dir)) return (FALSE);
				if (!p_ptr->target_who) return (FALSE);
				m_ptr = &mon_list[p_ptr->target_who];
				r_ptr = &r_info[m_ptr->r_idx];
				x = m_ptr->fx;
				y = m_ptr->fy;
				monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
				msg_format("You point at %s and scream the word 'DIE!'", m_name);
		*/		/* It's hard not to wake up if someone is trying to sacrifice you... */
		//		m_ptr->csleep = 0;
				/* Saving throw. Pretty straightforward */
		/*		if (randint0(plev * 2 + 80) < r_ptr->level)
				{
					msg_format("%^s resists the effects!", m_name);
				}
				else
				{
					if (mon_take_hit(p_ptr->target_who, damroll(35, 35), &fear, " is lost in raging thunderstorm!", -1))
					{
						project_ball(-1, 2, y, x, y, x, 150, GF_WIND, 0, 0);
					}
				}
			}
			break;
		 
			case PWR_CURSE_SMALL:
			{

		*/		/* Hack -- directly affect monster */
		/*		if (!get_aim_dir(&dir)) return (FALSE);
				if (!p_ptr->target_who) return (FALSE);
				m_ptr = &mon_list[p_ptr->target_who];
				r_ptr = &r_info[m_ptr->r_idx];
				monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
				msg_format("You point at %s and curse.", m_name);
		*/		/* It's hard not to wake up if someone is trying to sacrifice you... */
		//		m_ptr->csleep = 0;
				/* Saving throw. Pretty straightforward */
		/*		if (randint0(plev * 2 + 50) < r_ptr->level)
				{
					msg_format("%^s resists the effects!", m_name);
				}
				else
				{
					mon_take_hit(p_ptr->target_who, damroll(3, 5), &fear, " is sacrificed to your god.", -1);
				}
			}
			break;
		*/	 
			case PWR_DETECT_ALL:
			{
				(void)detect_all(TRUE);
			}
			break;
			
		/*	 
			case PWR_DETECT_CHAOS:
			{

				(void)detect_monsters_chaotic();

			}
			break;
		*/	 
			case PWR_DETECT_EVIL:
			{
				(void)detect_monsters_evil(TRUE);
			}
			break;
		/*	 
			case PWR_DETECT_LAW:
			{

				(void)detect_monsters_lawful();

			}
			break;
		*/	 
			case PWR_DETECT_MONSTERS:
			{
				(void)detect_monsters_normal(TRUE);
			}
			break;
			
			case PWR_DETECT_TREASURE:
			{
				(void)detect_treasure(TRUE);
			}
			break;
			
			/* to fix this I need to find something equivalent to tgt_pt -Simon
			case PWR_DIMENSION_DOOR:
			{
				if (!tgt_pt(&x, &y)) return (FALSE);
				if (!cave_empty_bold(y, x) || (cave_info[y][x] & CAVE_ICKY) || (distance(y, x, p_ptr->py, p_ptr->px) > plev * 2) || !(cave_info[y][x] & CAVE_MARK))
				{
					msg_print("You fail to enter dimensional portal!");
					teleport_player(plev);
				}
				else
				{
					msg_print("You step through dimensions.");
					teleport_player_to(y, x);
				}
			}
			break;
		*/	 
			case PWR_DISPEL_LIFE:
			{
				/* XXX XXX XXX This should be dispel *life* */
				msg_print("You dispel all creatures!");
				(void)dispel_monsters(plev * 2);
			}
			break;
			 
		//	case PWR_FORCE_CHARM_BY_GHOST:
		//	{

				/* Hack -- directly affect monster */
			/*	if (!get_aim_dir(&dir)) return (FALSE);
				if (!p_ptr->target_who) return (FALSE);
				m_ptr = &mon_list[p_ptr->target_who];
				r_ptr = &r_info[m_ptr->r_idx];
				x = m_ptr->fx;
				y = m_ptr->fy;
				monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
				msg_format("You gaze intently at %s.", m_name);
			*/	/* No undeads */
			/*	if (r_ptr->flags3 & (RF3_UNDEAD))
				{
					msg_format("%^s gazes back at you and bursts in laugh.", m_name);
				}
				else if (monster_nonliving(r_ptr))
				{
					msg_format("%^s does not seem to be impressed.", m_name);
				}
				else
				{
					project(-1, 0, y, x, y, x, plev * 5 / 2, GF_CHARM, PROJECT_KILL, 0, 0);
				}
			}
			break;
			 
			case PWR_ENCHANT_WEAPON:
			{

				enchant_spell(randint0(4) + plev / 20,
								  randint0(4) + plev / 20, 0);

			}
			break;
			 
			case PWR_FORCE_PSEUDOID:
			{

				item_tester_hook = item_tester_nonpseudoid;
				
				if (!get_item(&item, "Concentrate on which item? ", "You have nothing to concentrate on.",
					(USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);
				
				if (item >= 0)
				{
					o_ptr = &inventory[item];
				}
				else
				{
					o_ptr = &o_list[0 - item];
				}
				
				i = value_check_aux1(o_ptr);

			*/	/* Squelch it? */
			/*	if (item < INVEN_EQUIP)
				{
					squelch = squelch_itemp(o_ptr, i, 0);
				}
				
			*/	/* Get an object description */
			//	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

				/* Message (equipment) */
			/*	if (item >= INVEN_EQUIP)
				{
					msg_format("You feel the %s (%c) you are %s %s %s...",
						   o_name, index_to_label(item), describe_use(item),
						   ((o_ptr->number == 1) ? "is" : "are"),
						   inscrip_text[i - INSCRIP_NULL]);
				}

			*/	/* Message (inventory) */
			/*	else if (item >= 0)
				{
					msg_format("You feel the %s (%c) in your pack %s %s...  %s",
						   o_name, index_to_label(item),
						   ((o_ptr->number == 1) ? "is" : "are"),
						   inscrip_text[i - INSCRIP_NULL],
							((squelch == 1) ? "(Squelched)" :
							((squelch == -1) ? "(Squelch Failed)" : "")));
				}
				
			*/	/* Message (floor) */
			/*	else
				{
					msg_format("You feel the %s on the floor %s %s...  %s",
						   o_name,
						   ((o_ptr->number == 1) ? "is" : "are"),
						   inscrip_text[i - INSCRIP_NULL],
							((squelch == 1) ? "(Squelched)" :
							((squelch == -1) ? "(Squelch Failed)" : "")));
				}

			*/	/* Sense the object */
			//	o_ptr->discount = i;

				/* The object has been "sensed" */
			//	o_ptr->ident |= (IDENT_SENSE);

				/* Squelch it if necessary */
			//	do_squelch_item(squelch, item, o_ptr);

				/* Combine / Reorder the pack (later) */
			//	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

				/* Window stuff */
		/*		p_ptr->window |= (PW_INVEN | PW_EQUIP);
			}
			break;
		*/	 
			case PWR_HASTE:
			{
				msg_print("You try to haste yourself magically.");
				if (!p_ptr->timed[TMD_FAST])
					(void)set_timed(TMD_FAST, randint1(20) + plev, TRUE);
				else
					(void)inc_timed(TMD_FAST, randint1(5), TRUE);
			}
			break;
			 
			case PWR_HEAL:
			{
				int amt = (p_ptr->mhp * 35) / 100;
				if (amt < 300) amt = 300;
				
				(void)hp_player(amt);
				(void)clear_timed(TMD_CUT, TRUE);
				(void)clear_timed(TMD_AMNESIA, TRUE);
				//unusable while conf'd -Simon
				//(void)clear_timed(TMD_CONFUSED, TRUE);
				(void)clear_timed(TMD_BLIND, TRUE);
				(void)clear_timed(TMD_POISONED, TRUE);
				(void)clear_timed(TMD_STUN, TRUE);
			}
			break;
			 
		//	case PWR_HEAL_BONES:
		//	{
			/* Get the object */
			/*	item_tester_hook = item_tester_bones;
				
				if (!get_item(&item, "Use which bones? ", "You have no suitable bones.",
					(USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);
				
				if (item >= 0)
				{
					o_ptr = &inventory[item];
				}
				else
				{
					o_ptr = &o_list[0 - item];
				}

			*/	/* Get an object description */
			//	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
				
				/* Hack -- issue the message */
			/*	if (strstr(o_name, "Bones"))
				{
					msg_format("You insert %s into your skeleton.", o_name);
				}
				else
				{
					msg_format("You insert the bones of %s into your skeleton.", o_name);
				}
			*/	
				/* Heal */
			/*	if (o_ptr->tval == TV_CORPSE)
				{
					hp_player(r_info[o_ptr->pval].level * 5);
				}
				
			*/	/* Destroy the object */
			/*	if (item >= 0)
				{
					inven_item_increase(item, -1);
					inven_item_describe(item);
					inven_item_optimize(item);
				}
				else
				{
					floor_item_increase(0 - item, -1);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
				break;


			}
			break;
		 
			case PWR_HEAL_PET:
			{
			*/	/* Target */
			//	if (!target_set_interactive(TARGET_KILL)) return (FALSE);
			//	if (!p_ptr->target_who) return (FALSE);
				
				/* Ensure that monster is a pet */
			/*	m_ptr = &mon_list[p_ptr->target_who];
				if (!(m_ptr->align & AL_PET_MASK))
				{
					msg_print("This monster is not your pet!");
					return (FALSE);
				}
				
				i = damroll(3, 10);
				j = m_ptr->maxhp * (plev * 2 / 3) / 100;
				if (j > i) i = j;
				
				if (m_ptr->hp + i > m_ptr->maxhp) i = m_ptr->maxhp - m_ptr->hp;
				
				msg_print("You use your powers to heal your pet.");
				m_ptr->hp += i;
				p_ptr->redraw |= (PR_HEALTH);
			}
			break;
			 
			case PWR_HEAL_WALL:
			{
				if (!get_rep_dir(&dir)) return (FALSE);
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				if (cave_feat[y][x] < FEAT_WALL_EXTRA || cave_feat[y][x] > FEAT_WALL_SOLID)
				{
					msg_print("You must use the section of granite wall.");
					return (FALSE);
				}
				
				p_ptr->energy_use = 250 + randint0(250);
				cave_set_feat(y, x, FEAT_FLOOR);
				hp_player(100 + plev * 3);
			}
			break;
		 */
			case PWR_HEROISM:
			{

				(void)hp_player(10);
				p_ptr->timed[TMD_HERO] = randint0(25) + 25;
				p_ptr->timed[TMD_AFRAID] = 0;

			}
			break;
			 
			case PWR_HOLY_ORB_SMALL:
			{

				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("The cloud of light smoke engulfs your target.");
				fire_ball(GF_HOLY_ORB, dir, damroll(3, 6), 1);

			}
			break;
 
			case PWR_IDENTIFY:
			{

				//if (p_ptr->al_special == PAL_CHAOS) msg_print("You listen to the voices from the Chaos...");
				(void)ident_spell();

			}
			break;
		/*
			case PWR_LIGHT_HEAL:
			{

				if (!(cave_info[p_ptr->py][p_ptr->px] & (CAVE_GLOW)))
				{
					msg_print("There is no light here to use.");
				}
				else
				{
					unlite_area(plev, 3);
					hp_player(((plev < 20 ? 20 : plev) - 20) * 5 + 100);
				}

			}
			break;
			 
			case PWR_MAKE_ABYSS:
			{

			*/	/* Target */
			/*	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

				msg_print("You change the world!");
				project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col,	1, GF_MAKE_ABYSS, PROJECT_PASS);

			}
			break;
			 
			case PWR_MAKE_FIRE:
			{

			*/	/* Target */
			/*	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

				msg_print("You change the world!");
				project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col, 1, GF_MAKE_FIRE, PROJECT_PASS);

			}
			break;
			 
			case PWR_MAKE_GRASS:
			{

			*/	/* Target */
			/*	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

				msg_print("You change the world!");
				project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col,	1, GF_MAKE_GRASS, PROJECT_PASS);

			}
			break;
		 
			case PWR_MAKE_ICE:
			{

			*/	/* Target */
			/*	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

				msg_print("You change the world!");
				project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col,	1, GF_MAKE_ICE, PROJECT_PASS);

			}
			break;
		 
			case PWR_MAKE_LAVA:
			{
			*/	/* Target */
			/*	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

				msg_print("You change the world!");
				project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col, 1, GF_MAKE_LAVA, PROJECT_PASS);
			}
			break;
			 
			case PWR_MAKE_TREE:
			{
			*/	/* Target */
			/*	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

				msg_print("You change the world!");
				project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col, 1, GF_MAKE_TREE, PROJECT_PASS);
			}
			break;
		 
			case PWR_MAKE_WALL:
			{
			*/	/* Target */
			/*	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

				msg_print("You change the world!");
				project_star(-1, 2 + p_ptr->lev / 20, p_ptr->target_row, p_ptr->target_col, 1, GF_MAKE_WALL, PROJECT_PASS);
			}
			break;
			 
			case PWR_MAKE_WATER:
			{
			*/	/* Target */
			/*	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

				msg_print("You change the world!");
				project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col,	1, GF_MAKE_WATER, PROJECT_PASS);
			}
			break;
	*/		 
			case PWR_OPPOSE_COLD:
			{
				(void)inc_timed(TMD_OPP_COLD, randint0(30) + 30, TRUE);
			}
			break;
			 
			case PWR_OPPOSE_ELEC:
			{
				(void)inc_timed(TMD_OPP_ELEC, randint0(30) + 30, TRUE);
			}
			break;

			case PWR_OPPOSE_FIRE:
			{
				(void)inc_timed(TMD_OPP_FIRE, randint1(20) + 20, TRUE);
			}
			break;
			
			case PWR_PARALYZE:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You stare deep into monster's eyes.");
				(void)sleep_monster(dir, TRUE);
			}
			break;
 
			case PWR_PROJECT_CHAOS:
			{
				msg_print("You invoke raw Chaos upon your enemies!");
				project_los(GF_CHAOS, plev * 2, TRUE);
			}
			break;
	 
			case PWR_PROJECT_CRYO:
			{
				msg_print("You invoke extreme cold upon your enemies!");
				project_los(GF_COLD, plev * 3, TRUE);
			}
			break;
 
			case PWR_PROJECT_DISEN:
			{
				msg_print("You invoke the Void upon your enemies!");
				project_los(GF_DISENCHANT, plev * 3 / 2, TRUE);
			}
			break;
			
			case PWR_PROJECT_SHARD:
			{
				msg_print("You spray shards everywhere in sight!");
				project_los(GF_SHARD, plev * 7/4, TRUE);			
			}
			break;
			
			case PWR_PROJECT_SOUND:
			{
				msg_print("You bellow like a thunderclap!");
				project_los(GF_SOUND, plev * 5/4, TRUE);			
			}
			break;
	 
			case PWR_REMEMBRANCE:
			{
				(void)restore_level();
			}
			break;
			 
			case PWR_RESISTANCE:
			{
				i = randint0(20) + 20;
				(void)inc_timed(TMD_OPP_ACID, i, TRUE);
				(void)inc_timed(TMD_OPP_ELEC, i, TRUE);
				(void)inc_timed(TMD_OPP_COLD, i, TRUE);
				(void)inc_timed(TMD_OPP_FIRE, i, TRUE);
				(void)inc_timed(TMD_OPP_POIS, i, TRUE);
			}
			break;
			 
			case PWR_RESTORATION:
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
			}
			break;
 
			case PWR_SCARE:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You try to scare the monster.");
				(void)fear_monster(dir, plev, TRUE); 
			}
			break;

			case PWR_SCARE_ALL:
			{
				msg_print("You try to scare everything in sight.");
				project_los(GF_TURN_ALL, plev, TRUE);
			}
			break;
			 
			case PWR_SET_PROJECT_ELEC:
			{
				msg_print("You invoke a powerful electric discharge.");
				//p_ptr->project_elec += 25;
			}
			break;
			 
			case PWR_SHIFT_PLANES:
			{
				//(void)set_tim_immaterial(p_ptr->tim_immaterial + randint0(20) + 20);
			}
			break;
			 
			case PWR_SHRIEK:
			{
				msg_print("You make a high pitched shriek.");
				aggravate_monsters(0);
				if (one_in_(2))
				{
					if (one_in_(2))
					{
			/*			if (summon_specific_pet(py, px, p_ptr->depth, 0))
						{
							msg_print("Your shriek attracts a friendly monster!");
						}
			*/		}
					else
					{
						if (summon_specific(py, px, p_ptr->depth, 0, 1))
						{
							msg_print("Your shriek attracts a hostile monster!");
						}
					}        			
				}
			}
			break;
			 
			case PWR_SLOW:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You try to slow down the monster.");
				slow_monster(dir);
			}
			break;
			 
		/*	case PWR_SPAWN:
			{
				int hp;
				r_ptr = &r_info[rp_ptr->p_monster_index];
				if (r_ptr->flags[RF_FORCE_MAXHP])
				{
					hp = r_ptr->hdice * r_ptr->hside;
				}
				else
				{
		*/			/* XXX Not very precise */
		/*			hp = (r_ptr->hdice * r_ptr->hside) / 2;
				}
				if (p_ptr->chp <= hp)
				{
					msg_print("You are too weak to spawn.");
					return (FALSE);
				}
				if (p_ptr->chp - hp <= (p_ptr->mhp * op_ptr->hitpoint_warn / 10))
				{
					if (!get_check("This may critically lower your hit points! Are you sure? ")) return (FALSE);
				}
				
				i = 0;
				summon_pets_hack = TRUE;
				place_monster_group(py, px, rp_ptr->p_monster_index, FALSE, 2);
				summon_pets_hack = FALSE;
				take_hit(hp, "failed attempt to spawn");
				return (TRUE);
			}
			break;
		*/ 
			case PWR_SPIT_ACID:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You spit acid.");
				fire_bolt(GF_ACID, dir, damroll(3 + plev / 4, 5));
			}
			break;
		 
			case PWR_SPIT_POISON:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You spit venom.");
				fire_bolt(GF_POIS, dir, damroll(3 + plev / 3, 5));
			}
			break;
		 
			case PWR_STAR_HEAL:
			{
				msg_print("Your god heals you!");
				(void)hp_player(1000);
				(void)clear_timed(TMD_CUT, TRUE);
				(void)clear_timed(TMD_AMNESIA, TRUE);
				(void)clear_timed(TMD_BLIND, TRUE);
				(void)clear_timed(TMD_POISONED, TRUE);
				(void)clear_timed(TMD_STUN, TRUE);
			}
			break;
			 
		//	case PWR_STEAL:
		//	{

				/* Based on do_cmd_steal() */
			/*	if (!get_aim_dir(&dir)) return (FALSE);
				y = py + ddy[dir];
				x = px + ddx[dir];
				
				if (cave_m_idx[y][x] > 0)
				{
					monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
					
					if (m_ptr->ml)
					{
						py_steal(y, x);
						return (TRUE);
					}
				}

				msg_print("You don't see anything to steal from.");
				return (FALSE);
			}
			break;
			
			case PWR_SUMM_AINU:
			{
				count = 0;
				for (i = 0; i < 3; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_AINU);
				if (count)
				{
					msg_print("You summon the Maiar from Valinor!");
				}
			}
			break; 
		*/	 
			case PWR_SUMM_BALROG:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_BALROG))
				{
					msg_print("Lord of Hell answers to your call!");
				}
			}
			break;
			 
			case PWR_SUMM_DARK_ELF:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_DARK_ELF))
				{
					msg_print("You summon an evil dark elf!");
				}
			}
			break;
			 
			case PWR_SUMM_DEMON:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_DEMON))
				{
					msg_print("You summon a demon from a lower plane!");
				}
			}
			break;
		/*	 
			case PWR_SUMM_DEMON_SUMM:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_DEMON_SUMM))
				{
					msg_print("You summon a demon summoner!");
				}
			}
			break;
		*/ 
			case PWR_SUMM_DRAGON:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_DRAGON))
				{
					msg_print("You summon a dragon!");
				}
			}
			break;
			 
			case PWR_SUMM_DRAGON_ANCIENT:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_HI_DRAGON))
				{
					msg_print("You summon an ancient wyrm!");
				}
			}
			break;
			 
			case PWR_SUMM_DRAGON_MATURE:
			{
				count = 0;
				for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_MATURE_DRAGON);
				if (count)
				{
					msg_print("You summon mighty dragons!");
				}
			}
			break;
		/*	 
			case PWR_SUMM_DRAGON_SUMM:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_DRAGON_SUMM))
				{
					msg_print("You summon a dragon summoner!");
				}
			}
			break;
		*/	 
			case PWR_SUMM_ELEMENTAL:
			{
				count = 0;
				for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_ELEMENTAL);
				if (count)
				{
					msg_print("You conjure some elementals!");
				}
			}
			break;
			 
			case PWR_SUMM_GOLEM:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_GOLEM))
				{
					msg_print("You magically construct a golem!");
				}
			}
			break;
			 
			case PWR_SUMM_HI_DEMON:
			{
				count = 0;
				for (i = 0; i < 8; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_HI_DEMON);
				if (count)
				{
					msg_print("You summon greater demons!");
				}
			}
			break;
			 
			case PWR_SUMM_HI_DRAGON:
			{
				count = 0;
				for (i = 0; i < 8; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_HI_DRAGON);
				if (count)
				{
					msg_print("You summon ancient wyrms!");
				}
			}
			break;
			 
			case PWR_SUMM_HI_UNDEAD:
			{
				count = 0;
				for (i = 0; i < 8; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_HI_UNDEAD);
				if (count)
				{
					msg_print("You summon the shadows of death!");
				}
			}
			break;
			 
			case PWR_SUMM_HYDRA:
			{
				count = 0;
				for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_HYDRA);
				if (count)
				{
					msg_print("You summon some hydras!");
				}
			}
			break;
			 
			case PWR_SUMM_LAWFUL:
			{
				count = 0;
				for (i = 0; i < 3; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_LAWFUL);
				if (count)
				{
					msg_print("You summon the minions of Law!");
				}
			}
			break;
		 
			case PWR_SUMM_LICH:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_LICH))
				{
					msg_print("You summon a sorcerer from his grave!");
				}
			}
			break;
			 
			case PWR_SUMM_MAGMA_ELEM_WALL:
			{
				if (!get_rep_dir(&dir)) return (FALSE);
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				if (cave_feat[y][x] < FEAT_WALL_EXTRA || cave_feat[y][x] > FEAT_WALL_SOLID)
				{
					msg_print("You must use the section of granite wall.");
					return (FALSE);
				}
				
				p_ptr->energy_use = 250 + randint0(250);
				cave_set_feat(y, x, FEAT_FLOOR);
				summon_pets_hack = TRUE;
				/* Hack -- Magma elemental and Greater magma elemental index hardcoded */
				msg_print("You melt the wall to summon the minion!");
				place_monster_aux(y, x, (plev > 40 ? 656 : 397), FALSE, plev > 35 ? 35 : plev);
				summon_pets_hack = FALSE;
			}
			break;
		 
			case PWR_SUMM_OGRE:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_OGRE))
				{
					msg_print("You summon a band of ogres!");
				}
			}
			break;
		 
			case PWR_SUMM_ORC:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_ORC))
				{
					msg_print("You summon a band of orcs!");
				}
			}
			break;
			 
			case PWR_SUMM_SPIDER:
			{
				count = 0;
				for (i = 0; i < 8; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_SPIDER);
				if (count)
				{
					msg_print("You summon spiders from Nan Dungortheb!");
				}
			}
			break;
			 
			case PWR_SUMM_TROLL:
			{
				count = 0;
				for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_TROLL);
				if (count)
				{
					msg_print("You summon some ugly trolls!");
				}
			}
			break;
			 
			case PWR_SUMM_ULTIMATE:
			{
				count = 0;
				for (i = 0; i < 4; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_ULTIMATE);
				if (count)
				{
					msg_print("You summon some *ULTIMATE* allies!");
				}
			}
			break;
			 
			case PWR_SUMM_UNDEAD:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_UNDEAD))
				{
					msg_print("You raise an undead slave from the grave!");
				}
			}
			break;
		 /*
			case PWR_SUMM_UNDEAD_DRAGON:
			{
				count = 0;
				for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_UNDEAD_DRAGON);
				if (count)
				{
					msg_print("You summon the shadows of the Great Wyrms!");
				}
			}
			break;
		
			case PWR_SUMM_UNDEAD_SUMM:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_UNDEAD_SUMM))
				{
					msg_print("You summon an undead summoner!");
				}
			}
			break;
		*/ 
			case PWR_SUMM_VORTEX:
			{
				count = 0;
				for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_VORTEX);
				if (count)
				{
					msg_print("You conjure elemental vortices!");
				}
			}
			break;
			 
			case PWR_SUMM_VROCK:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_VROCK))
				{
					msg_print("Troopers from Hell answer to your call!");
				}
			}
			break;
			 
			case PWR_SUMM_WIGHT_WRAITH:
			{
				if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_WIGHT_WRAITH))
				{
					msg_print("You summon a ghostly figure from the grave!");
				}
			}
			break;
			 
			case PWR_SUMM_YEEK:
			{
				count = 0;
				for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_YEEK);
					if (count)
					{
						msg_print("You summon some wildly screaming yeeks!");
					}
			}
			break;
			
			case PWR_TELEPORT:
			{
				msg_print("You teleport across the level.");
				teleport_player(100);
			}
			break;
	 
			case PWR_TELEPORT_AWAY:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You teleport a monster away.");
				(void)teleport_monster(dir);
			}
			break;
			 
			case PWR_WEB_BALL:
			{
				msg_print("You weave webs around you.");
				fire_bolt_beam_ball_special(GF_WEB, 0, 1, 2,	PROJECT_GRID | PROJECT_THRU);
			}
			break;
			 
			case PWR_WEB_RAY:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				msg_print("You spit webs.");
				fire_bolt_beam_ball_special(GF_WEB, dir, 1, 0, PROJECT_BEAM | PROJECT_GRID | PROJECT_THRU);
			}
			break;
			 
			case PWR_WONDER:
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				//if (p_ptr->al_special == PAL_CHAOS) msg_print("You invoke the force of Chaos...");
				msg_print("You invoke a random spell...");
				(void)effect_wonder(dir, randint1(100) + p_ptr->lev / 5, beam);
			}
			break;
	 
			case PWR_WORD_OF_DESTRUCTION:
			{
				msg_print("You completely wipe out the nearby area!");
				destroy_area(py, px, 15, TRUE);
			}
			break;
			 
			case PWR_WORD_OF_RECALL:
			{
				set_recall();
			}
			break;
		}
	return (TRUE);
}