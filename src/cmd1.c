/* File: cmd1.c */

/* Purpose: Movement commands (part 1) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Determine if the player "hits" a monster (normal combat).
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_fire(s16b chance, s16b ac, s16b vis)
{
   s16b k;

   /* Percentile dice */
   k = rand_int(100);

   /* Hack -- Instant miss or hit */
   if (k < 10) return (k < 5);

   /* Never hit */
   if (chance <= 0) return (FALSE);

   /* Invisible monsters are harder to hit */
   if (!vis) chance = (chance + 1) / 2;

   /* Power competes against armor */
   if (rand_int(chance) < (ac * 3 / 4)) return (FALSE);

   /* Assume hit */
   return (TRUE);
}

/*
 * Determine if the player "hits" a monster (normal combat).
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_norm(s16b chance, s16b ac, s16b vis)
{
   s16b k;

   /* Percentile dice */
   k = rand_int(100);

   /* Hack -- Instant miss or hit */
   if (k < 10) return (k < 5);

   /* Wimpy attack never hits */
   if (chance <= 0) return (FALSE);

   /* Penalize invisible targets */
   if (!vis) chance = (chance + 1) / 2;

   /* Power must defeat armor */
   if (rand_int(chance) < (ac * 3 / 4)) return (FALSE);

   /* Assume hit */
   return (TRUE);
}

/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
/* jwk - msg_show added to surpress messages in do_wiz_rateweapon */
s16b critical_shot(s16b weight, s16b plus, s16b dam,bool msg_show)
{
   s16b i, k;
/* jk - to check for rings of accuracy */
   object_type *i_ptr_left,*i_ptr_right;

   i_ptr_left = &inventory[INVEN_LEFT];
   i_ptr_right = &inventory[INVEN_RIGHT];

   /* Extract "shot" power */
   i = (weight + ((p_ptr->to_h + plus) * 4) + (p_ptr->lev * 2));
   if ((i_ptr_left->tval==TV_RING) && (i_ptr_left->sval==SV_RING_ACCURACY))
   {
      i+=i_ptr_left->to_h*60;
   }
   if ((i_ptr_right->tval==TV_RING) && (i_ptr_right->sval==SV_RING_ACCURACY))
   {
      i+=i_ptr_right->to_h*60;
   }

   /* Critical hit */
   if (randint(5000) <= i)
   {
      k = weight + randint(500);
      if (k < 500)
      {
         if (msg_show) msg_print("It was a good hit!");
         dam = 2 * dam + 5;
      }
      else if (k < 1000)
      {
         if (msg_show) msg_print("It was a great hit!");
         dam = 2 * dam + 10;
      }
      else
      {
         if (msg_show) msg_print("It was a superb hit!");
         dam = 3 * dam + 15;
      }
   }

   return (dam);
}

/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 */
/* jwk - msg_show added to surpress messages in do_wiz_rateweapon */
s16b critical_norm(s16b weight, s16b plus, s16b dam, bool msg_show)
{
   s16b i, k, chance;
/* jk */
   object_type *i_ptr_left,*i_ptr_right;

   /* Extract "blow" power */
   i = (weight + ((p_ptr->to_h + plus) * 5) + (p_ptr->lev * 3));
/* jk - to check for rings of accuracy */

   i_ptr_left = &inventory[INVEN_LEFT];
   i_ptr_right = &inventory[INVEN_RIGHT];

   /* Extract "shot" power */
   i = (weight + ((p_ptr->to_h + plus) * 4) + (p_ptr->lev * 2));
   if ((i_ptr_left->tval==TV_RING) && (i_ptr_left->sval==SV_RING_ACCURACY))
   {
      i+=i_ptr_left->to_h*60;
   }
   if ((i_ptr_right->tval==TV_RING) && (i_ptr_right->sval==SV_RING_ACCURACY))
   {
      i+=i_ptr_right->to_h*60;
   }
   chance = randint(5000);

   /* Chance */
   if (chance <= i)
   {
      k = weight + randint(650);

/* jk - warriors score better critical hits */
/* level 0 - nothing better */
/* level 10 - 150 better */
/* level 50 - 750 better */

      if ( (p_ptr->pclass == CLASS_WARRIOR) || (p_ptr->pclass == CLASS_GLADIATR) )
      {
         k +=p_ptr->lev * 15;
      }

      if (k < 400)
      {
         if (msg_show) msg_print("It was a good hit!");
         dam = 2 * dam + 5;
      }
      else if (k < 700)
      {
         if (msg_show) msg_print("It was a great hit!");
         dam = 2 * dam + 10;
      }
      else if (k < 900)
      {
         if (msg_show) msg_print("It was a superb hit!");
         dam = 3 * dam + 15;
      }
      else if (k < 1300)
      {
         if (msg_show) msg_print("It was a *GREAT* hit!");
         dam = 3 * dam + 20;
      }
      else
      {
         if (msg_show) msg_print("It was a *SUPERB* hit!");
         dam = ((7 * dam) / 2) + 25;
      }
   }

   return (dam);
}

static bool elemental_skin_chance(s16b value)
{
   if (value>400) return(TRUE);
   else if (value>200) return(randint(2)==1);
   else if (value>100) return(randint(3)==1);
   else if (value>50) return(randint(4)==1);
   else if (value>20) return(randint(10)==1);
   else return (FALSE);
}

/*
 * does a ring of flames work?
 * note that the chance is high, but it only works on non-ego, non-artifact weapons!
 */
static bool elemental_ring_chance()
{
   return (randint(6-(p_ptr->lev/10)) == 1);
}

bool test_acid_brand_extra(void)
{
   bool result = FALSE;

   if (inventory[INVEN_LEFT].k_idx)
   {
      object_type *i_ptr = &inventory[INVEN_LEFT];

      if (!cursed_p(i_ptr) &&
          (i_ptr->tval == TV_RING) && (i_ptr->sval == SV_RING_ACID) &&
          elemental_ring_chance())
      {
         result = TRUE;
      }
   }
   if (inventory[INVEN_RIGHT].k_idx)
   {
      object_type *i_ptr = &inventory[INVEN_RIGHT];

      if (!cursed_p(i_ptr) &&
          (i_ptr->tval == TV_RING) && (i_ptr->sval == SV_RING_ACID) &&
          elemental_ring_chance())
      {
         result = TRUE;
      }
   }
   if (result)
      msg_print("Your weapon suddenly drips a an acid rain!");
   if ((p_ptr->acid) && (elemental_skin_chance(p_ptr->acid)))
   {
      cptr who = "acid skin";
      result = TRUE;
      msg_print("Acid splatters from your skin.");
      acid_dam(randint(10)+5, who);
   }
   return (result);
}

bool test_fire_brand_extra(void)
{
   bool result = FALSE;

   if (inventory[INVEN_LEFT].k_idx)
   {
      object_type *i_ptr = &inventory[INVEN_LEFT];

      if (!cursed_p(i_ptr) &&
          (i_ptr->tval == TV_RING) && (i_ptr->sval == SV_RING_FLAMES) &&
          elemental_ring_chance())
      {
         result = TRUE;
      }
   }
   if (inventory[INVEN_RIGHT].k_idx)
   {
      object_type *i_ptr = &inventory[INVEN_RIGHT];

      if (!cursed_p(i_ptr) &&
          (i_ptr->tval == TV_RING) && (i_ptr->sval == SV_RING_FLAMES) &&
          elemental_ring_chance())
      {
         result = TRUE;
      }
   }
   if (result)
      msg_print("Your weapon suddenly glows hot in your hands!");

   if ((p_ptr->fire) && (elemental_skin_chance(p_ptr->fire)))
   {
      cptr who = "fiery skin";
      result = TRUE;
      msg_print("Fire erupts from your fingers.");
      fire_dam(randint(10)+5, who);
   }
   return (result);
}

bool test_cold_brand_extra(void)
{
   bool result = FALSE;

   if (inventory[INVEN_LEFT].k_idx)
   {
      object_type *i_ptr = &inventory[INVEN_LEFT];

      if (!cursed_p(i_ptr) &&
          (i_ptr->tval == TV_RING) && (i_ptr->sval == SV_RING_ICE) &&
          elemental_ring_chance())
      {
         result = TRUE;
      }
   }
   if (inventory[INVEN_RIGHT].k_idx)
   {
      object_type *i_ptr = &inventory[INVEN_RIGHT];

      if (!cursed_p(i_ptr) &&
          (i_ptr->tval == TV_RING) && (i_ptr->sval == SV_RING_ICE) &&
          elemental_ring_chance())
      {
         result = TRUE;
      }
   }
   if (result)
      msg_print("Your weapon suddenly leaves a trail of cold mist!");

   if ((p_ptr->cold) && (elemental_skin_chance(p_ptr->cold)))
   {
      cptr who = "cold skin";
      result = TRUE;
      msg_print("A mist of cold whafts from your hands.");
      cold_dam(randint(10)+5, who);
   }
   return (result);
}

bool test_elec_brand_extra(void)
{
   bool result = FALSE;

   if (inventory[INVEN_LEFT].k_idx)
   {
      object_type *i_ptr = &inventory[INVEN_LEFT];

      if (!cursed_p(i_ptr) &&
          (i_ptr->tval == TV_RING) && (i_ptr->sval == SV_RING_ELEC) &&
          elemental_ring_chance())
      {
         result = TRUE;
      }
   }
   if (inventory[INVEN_RIGHT].k_idx)
   {
      object_type *i_ptr = &inventory[INVEN_RIGHT];

      if (!cursed_p(i_ptr) &&
          (i_ptr->tval == TV_RING) && (i_ptr->sval == SV_RING_ELEC) &&
          elemental_ring_chance())
      {
         result = TRUE;
      }
   }
   if (result)
      msg_print("Your weapon suddenly leaves a trail of sparks!");

   if ((p_ptr->elec) && (elemental_skin_chance(p_ptr->elec)))
   {
      cptr who = "electric skin";
      result = TRUE;
      msg_print("Suddenly sparks sizzle between your fingers.");
      elec_dam(randint(10)+5, who);
   }
   return (result);
}

/*
 * A structure to hold a tval and its description
 */
typedef struct slay_multiplier_type
{
   u64b slay_flag;
   u64b monster_type;
   s16b mult;
} slay_multiplier_type;

static slay_multiplier_type slay_multipliers[] =
{
   { TR1_SLAY_ANIMAL, RF3_ANIMAL, 2 },
   { TR1_KILL_ANIMAL, RF3_ANIMAL, 4 },
   { TR1_SLAY_EVIL,   RF3_EVIL,   2 },
   { TR1_KILL_EVIL,   RF3_EVIL,   4 },
   { TR1_SLAY_UNDEAD, RF3_UNDEAD, 3 },
   { TR1_KILL_UNDEAD, RF3_UNDEAD, 5 },
   { TR1_SLAY_DEMON,  RF3_DEMON,  3 },
   { TR1_KILL_DEMON,  RF3_DEMON,  5 },
   { TR1_SLAY_ORC,    RF3_ORC,    3 },
   { TR1_KILL_ORC,    RF3_ORC,    5 },
   { TR1_SLAY_TROLL,  RF3_TROLL,  3 },
   { TR1_KILL_TROLL,  RF3_TROLL,  5 },
   { TR1_SLAY_GIANT,  RF3_GIANT,  3 },
   { TR1_KILL_GIANT,  RF3_GIANT,  5 },
   { TR1_SLAY_DRAGON, RF3_DRAGON, 3 },
   { TR1_KILL_DRAGON, RF3_DRAGON, 5 },
   { 0LL, 0LL, 0}
};

static s16b get_multiplier(object_type *i_ptr, monster_type *m_ptr)
{
   monster_race *r_ptr = &r_info[m_ptr->r_idx];
   s16b i, mult = 1;
   u64b f1, f2, f3;

   /* Extract the flags */
   object_flags(i_ptr, &f1, &f2, &f3);

/* test if the weapon is of the correct type,  */
/* and the monster is also of the correct type */
   for (i=0; slay_multipliers[i].mult; i++)
   {
      if ((f1 & slay_multipliers[i].slay_flag) &&
          (r_ptr->flags3 & slay_multipliers[i].monster_type))
      {
         if (mult< slay_multipliers[i].mult)
            mult = slay_multipliers[i].mult;
      }
   }
   /* have we found something so far? return now! */
   if (mult>1) return (mult);

   /* Brand (Acid) */
/* jk - a ring of acid once in a while gives you an acid bite, but */
/* only on non-artifact/non-ego weapons */
   if ((f1 & TR1_BRAND_ACID) ||
       (test_acid_brand_extra() && !i_ptr->name2))
   {
      /* Notice immunity */
      if (r_ptr->flags3 & RF3_IM_ACID)
      {
         if (m_ptr->ml) r_ptr->r_flags3 |= RF3_IM_ACID;
      }
      else
      {
         if (mult < 3) mult = 3;
      }
   }
   /* Brand (Elec) */
   if ((f1 & TR1_BRAND_ELEC) ||
       (test_elec_brand_extra() && !i_ptr->name2 && !i_ptr->name1 ))
   {
      /* Notice immunity */
      if (r_ptr->flags3 & RF3_IM_ELEC)
      {
         if (m_ptr->ml) r_ptr->r_flags3 |= RF3_IM_ELEC;
      }
      else
      {
/* jk - this should be 5, really */
         if (mult < 5) mult = 5;
      }
   }

   /* Brand (Fire) */
   if ((f1 & TR1_BRAND_FIRE) ||
       (test_fire_brand_extra() && !i_ptr->name2))
   {
      /* Notice immunity */
      if (r_ptr->flags3 & RF3_IM_FIRE)
      {
         if (m_ptr->ml) r_ptr->r_flags3 |= RF3_IM_FIRE;
      }
      else
      {
         if (mult < 3) mult = 3;
      }
   }

   /* Brand (Cold) */
   if ((f1 & TR1_BRAND_COLD) ||
       (test_cold_brand_extra() && !i_ptr->name2))
   {
      /* Notice immunity */
      if (r_ptr->flags3 & RF3_IM_COLD)
      {
         if (m_ptr->ml) r_ptr->r_flags3 |= RF3_IM_COLD;
      }
      else
      {
         if (mult < 3) mult = 3;
      }
   }
   return (mult);
}

/*
 * Extract the "total damage" from a given object hitting a given monster.
 *
 * Note that "flasks of oil" do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 *
 * Note that most brands and slays are x3, except Slay Animal (x2),
 * Slay Evil (x2), and Kill dragon (x5).
 */
s16b tot_dam_aux(object_type *i_ptr, s16b tdam, monster_type *m_ptr)
{
   s16b mult = 1;

   /* Some "weapons" and "ammo" do extra damage */
   switch (i_ptr->tval)
   {
       case TV_SHOT:
       case TV_ARROW:
       case TV_BOLT:
       case TV_HAFTED:
       case TV_POLEARM:
       case TV_SWORD:
       case TV_DIGGING:
       mult = get_multiplier(i_ptr, m_ptr);
   }

   /* Return the total damage */
   return (tdam * mult);
}

void find_one_trap(s16b x, s16b y)
{
   s16b k;
   trap_item_type *tr_ptr;

   if (!dungeon.level[sublevel][y][x].t_idx)
   {
      /* this shouldn't happen, but some evidence suggests it does - why */
      /* is unknown - but if we find a trap without an trap_item, make   */
      /* sure it is a harmless floor after this! */
dlog(DEBUGTRAPS,"cmd1.c: find_one_trap: called phase 1 without trap at %d,%d @ %d,%d",x,y,px,py);
      (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL, GRID_ADD, 0);
      return;
   }
   tr_ptr = &t_list[dungeon.level[sublevel][y][x].t_idx];

#if (debuglevel & DEBUGTRAPS)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
dlog(DEBUGTRAPS,"cmd1.c: find_one_trap: player @ %d,%d: %d,%d i_idx %d m_idx %d t_idx %d\n", px, py,
           x, y, c_ptr->i_idx, c_ptr->m_idx, c_ptr->t_idx);
dlog(DEBUGTRAPS,"fdat %08lx mtyp %d styp %d\n", c_ptr->fdat,c_ptr->mtyp,c_ptr->styp);
}
#endif

   k=0;
   while (k < MAX_TRAPS_IN_SET)
   {
dlog(DEBUGTRAPS,"cmd1.c: find_one_trap: testing trap entry %d type %d found %d\n",
                k, tr_ptr->type[k], tr_ptr->found[k]);
      if ((tr_ptr->type[k]!=0) && (tr_ptr->found[k]==FALSE)) break;
      k++;
   }

   if (k==MAX_TRAPS_IN_SET)
   {
dlog(DEBUGTRAPS,"cmd1.c: find_one_trap: called phase 2 without trap at %d,%d @ %d,%d",x,y,px,py);
      tr_ptr->inuse = FALSE;
      (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL, GRID_ADD, 0);
      return;
   }
dlog(DEBUGTRAPS,"cmd1.c: find_one_trap at %d,%d: found trap %d type %d\n",
                x, y, k, tr_ptr->type[k]);
   /* then let it be known */
   tr_ptr->found[k]=TRUE;
   msg_print("You have found a trap."); /* Message */
   if (test_grid(x, y, DUNG_FLOOR, DUNG_FLOOR_TRAP))
   {
      (void)set_grid_type(x,y,DUNG_TRAP,
                          DUNG_TRAP_FNDONE, GRID_ADD, 0);
   }
   else if (dungeon.level[sublevel][y][x].mtyp==DUNG_TRAP)
   {
      (void)set_grid_type(x,y,DUNG_TRAP,
                          DUNG_TRAP_FNDMORE, GRID_ADD, 0);
   }
   else if (dungeon.level[sublevel][y][x].mtyp==DUNG_DOOR)
   {
      /* don't do anything - the combinations of doors with traps */
      /* are too many - we'll just have to separate doors with    */
      /* valid t_idx entries in cave.c: map_info                  */
   }
   note_spot(x, y);
   lite_spot(x, y);
   disturb(0, 0); /* Disturb */
}

void find_other_trap(s16b x, s16b y)
{
   s16b num;
   s16b known;
   s16b trap;
dlog(DEBUGTRAPS,"cmd1.c: find_other_trap - about to get known traps\n");
   known = num_traps_xy(x, y, TRAP_FOUND);
dlog(DEBUGTRAPS,"cmd1.c: find_other_trap - about to get all traps\n");
   num   = num_traps_xy(x, y, TRAP_EXISTS);
   if (known<num)
   {
      trap_item_type *tr_ptr = &t_list[dungeon.level[sublevel][y][x].t_idx];
      trap=get_random_trap(tr_ptr, TRAP_NOTFOUND);
dlog(DEBUGTRAPS,"cmd1.c: find_other_trap not found %d known %d\n", num, known);
      msg_print("You have found another trap."); /* Message */
      /* then let it be known */
      set_trap_found_ptr(tr_ptr, trap);
      if (dungeon.level[sublevel][y][x].mtyp==DUNG_TRAP)
      {
         (void)set_grid_type(x,y,DUNG_TRAP,
                             DUNG_TRAP_FNDMORE, GRID_ADD, 0);
      }
      else if (dungeon.level[sublevel][y][x].mtyp==DUNG_DOOR)
      {
        /* don't do anything - the combinations of doors with traps */
        /* are too many - we'll just have to separate doors with    */
        /* valid t_idx entries in cave.c: map_info                  */
      }
      note_spot(x, y);
      lite_spot(x, y);
      disturb(0, 0); /* Disturb */
   }
}

void find_all_other_traps(s16b x, s16b y)
{
   s16b num   = num_traps_xy(x, y, TRAP_NOTFOUND);
   s16b known = num_traps_xy(x, y, TRAP_FOUND);
   s16b k,cnt = 0;
   if (known<num)
   {
      s16b index[MAX_TRAPS_IN_SET];
      s16b trap;
      trap_item_type *tr_ptr = &t_list[dungeon.level[sublevel][y][x].t_idx];
dlog(DEBUGTRAPS,"cmd1.c: find_all_other_traps not found %d known %d\n", num, known);
      msg_format("You have found another %s.", ((num-known)>1)?"set of traps":"trap");
      /* find out which unknown traps exist in tr_ptr */
      for (k=1;k<t_number;k++)
      {
         if (get_trap(tr_ptr,k) && !trap_found_ptr(tr_ptr,k))
            index[cnt++]=k;
      }
      /* choose one */
      for (k=0; k<cnt; k++)
      {
         trap = index[k];
         /* then let it be known */
         set_trap_found_ptr(tr_ptr, trap);
      }
      if (dungeon.level[sublevel][y][x].mtyp==DUNG_TRAP)
      {
         (void)set_grid_type(x,y,DUNG_TRAP,
                             DUNG_TRAP_FNDMORE, GRID_ADD, 0);
      }
      else if (dungeon.level[sublevel][y][x].mtyp==DUNG_DOOR)
      {
         /* don't do anything - the combinations of doors with traps */
         /* are too many - we'll just have to separate doors with    */
         /* valid t_idx entries in cave.c: map_info                  */
      }
      note_spot(x, y);
      lite_spot(x, y);
      disturb(0, 0); /* Disturb */
   }
}

/*
 * handle finding traps on a door
 */
static void search_handle_trapped_door(s16b x, s16b y)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

   trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];
   s16b           trap = get_random_trap(tr_ptr, TRAP_NOTFOUND);
dlog(DEBUGTRAPS,"cmd1.c: search_handle_trapped_door: t_idx %d num_traps NEVER %d ALWAYS %d\n",
             c_ptr->t_idx, num_traps_xy(x, y, TRAP_NOTFOUND), num_traps_xy(x, y, TRAP_FOUND));
dlog(DEBUGTRAPS,"cmd1.c: search_handle_trapped_door: type %016Lx found %016Lx\n", tr_ptr->type, tr_ptr->found);
   /* have we found traps already on this door? */
   if (num_traps_xy(x, y, TRAP_FOUND)==0)
   {
      msg_print("You have found a trapped door."); /* no */
   }
   else
   {
      msg_format("You have found another trap on this door %s.",
                 dirstr[what_dir(px,py,x,y)]);
   }
   set_trap_found_ptr(tr_ptr, trap);
dlog(DEBUGTRAPS,"cmd1.c: search: after finding: with t_idx %d num_traps NEVER %d num_traps ALWAYS %d\n",
             c_ptr->t_idx, num_traps_xy(x, y, TRAP_NOTFOUND), num_traps_xy(x, y, TRAP_FOUND));
dlog(DEBUGTRAPS,"cmd1.c: type %016Lx found %016Lx\n", tr_ptr->type, tr_ptr->found);
   note_spot(x, y); /* Notice */
   lite_spot(x, y); /* Redraw */
   disturb(0, 0); /* Disturb */
}

/*
 * Searches for hidden things.                  -RAK-
 */
void search(void)
{
   s16b               x, y, chance, roll, doorchance;

   cave_cell_type    *c_ptr;
   object_type       *i_ptr;

   /* Start with base search ability */
   chance = p_ptr->skill_srh;

   /* Penalize various conditions */
   if (p_ptr->blind || no_lite()) chance = chance / 10;
   if (p_ptr->confused || p_ptr->image) chance = chance / 10;

   /* Search the nearby grids, which are always in bounds */
   for (y = (py - 1); y <= (py + 1); y++)
   {
      for (x = (px - 1); x <= (px + 1); x++)
      {
         s16b objs;
         /* Sometimes, notice things */
         /* Access the grid */
         c_ptr = &dungeon.level[sublevel][y][x];
         if (rand_int(100) < chance)
         {
            if ( (c_ptr->mtyp == DUNG_FLOOR) &&
                 (c_ptr->styp == DUNG_FLOOR_TRAP)) /* Invisible trap */
            {
               find_one_trap(x, y);
            }
            else if ( (c_ptr->mtyp == DUNG_TRAP) &&
                 (c_ptr->styp == DUNG_TRAP_FNDONE))
            {
               find_other_trap(x, y);
            }           else if ( (c_ptr->mtyp == DUNG_DOOR) &&
                      (c_ptr->styp == DUNG_DOOR_SECRET)) /* Secret door */
            {
               msg_print("You have found a secret door."); /* Message */
               doorchance=randint(150-p_ptr->mdepth);
               if (c_ptr->fdat & CAVE_VAULT) doorchance=doorchance/4;
               if (doorchance<5)
               {
                  (void)set_grid_type(x,y,DUNG_DOOR,
                                      DUNG_DOOR_JAMMED, GRID_ADD, 0);
               }
               else if (doorchance<10)
               {
                  (void)set_grid_type(x,y,DUNG_DOOR,
                                      DUNG_DOOR_LOCKED, GRID_ADD, 0);
               }
               else
               {
                  (void)set_grid_type(x,y,DUNG_DOOR,
                                      DUNG_DOOR_CLOSED, GRID_ADD, 0);
               }
               note_spot(x, y); /* Notice */
               lite_spot(x, y); /* Redraw */
               disturb(0, 0); /* Disturb */
            }
            else if ( (c_ptr->mtyp == DUNG_DOOR) &&
                      (c_ptr->t_idx) && (num_traps_xy(x, y, TRAP_NOTFOUND)>0))
                     /* trapped door which we don't know about yet */
            {
               search_handle_trapped_door(x, y);
            }
            else if (hidden_treasure(x, y))
            {
               switch (wall_art(c_ptr->styp))
               {
                  case DUNG_WALL_ART_GRANITE:
                       (void)set_grid_type(x, y, DUNG_WALL,
                                           DUNG_WALL_GRTREAS, GRID_ADD, 0);
                       break;
                  case DUNG_WALL_ART_CHALK:
                       (void)set_grid_type(x, y, DUNG_WALL,
                                           DUNG_WALL_CHTREAS, GRID_ADD, 0);
                       break;
                  case DUNG_WALL_ART_QUARTZ:
                       (void)set_grid_type(x, y, DUNG_WALL,
                                           DUNG_WALL_QUTREAS, GRID_ADD, 0);
                       break;
                  case DUNG_WALL_ART_MAGMA:
                       (void)set_grid_type(x, y, DUNG_WALL,
                                           DUNG_WALL_MGTREAS, GRID_ADD, 0);
               }
               c_ptr->fdat &= ~CAVE_MIMIC; /* this we do not anymore */
               msg_print("You found a vein in the wall.");
               note_spot(x, y); /* Notice */
               lite_spot(x, y); /* Redraw */
               disturb(0, 0); /* Disturb */
            }
         }

         objs = objects_on_floor(x,y);
         roll = rand_int(100);
/* jk - this is only fair, 4 objects mean a 4 times higher chance */
/* 0 object shouldn't mean 0 chance - it did, which was puzzling */
         if ((objs>0) & (roll < (chance*objs) ) )
         {
/* jk  -  pick a random one from the objects that are there */
            i_ptr = get_item_pointer_floor_xy(rand_int(objs),x,y);

            /* Search chests */
            if ( (i_ptr->tval == TV_CHEST) && (i_ptr->xtra2))
            {
               trap_item_type *tr_ptr = &t_list[i_ptr->xtra2];
dlog(DEBUGTRAPS,"cmd1.c: search: chest, xtra2 %d exist %d found %d notfound %d\n",
                i_ptr->xtra2, num_traps_ptr(tr_ptr, TRAP_EXISTS),
                num_traps_ptr(tr_ptr, TRAP_FOUND),
                num_traps_ptr(tr_ptr, TRAP_NOTFOUND));

               /* Examine chests for traps - */
               /* condition 1 is normal      */
               /* condition 2 should not happen, but it seems to be possible to have  */
               /* a chest with traps that are found, yet the object is not known, so  */
               /* no traps are displayed when looking at it!                          */
               if ( (num_traps_ptr(tr_ptr, TRAP_NOTFOUND)>0) ||
                    (num_traps_ptr(tr_ptr, TRAP_FOUND>0) && !object_known_p(i_ptr) ) )

               {
                  s16b trap = get_random_trap(tr_ptr, TRAP_NOTFOUND);

                  set_trap_found_ptr(tr_ptr, trap);

                  /* Message */
                  msg_print("You have discovered a trap on the chest!");
dlog(DEBUGTRAPS,"cmd1.c: search: trapped chest: xtra2 %ld\n",i_ptr->xtra2);

                  object_known(i_ptr);

                  /* Notice it */
                  disturb(0, 0);
               }
            }
         }
      }
   }
}

bool worthless(object_type *i_ptr)
{
   bool        known, cursed;
   known=object_known_p(i_ptr) || (i_ptr->ident & ID_SENSE);
   cursed = cursed_p(i_ptr);
   if (object_value(i_ptr)<=0)
   {
      cursed=TRUE;
      known=TRUE;
   }

   /* if kill_cursed and we know it's cursed, destroy all */
   return ( (cursed && known) && (!artifact_p(i_ptr)) );
}


/* jk */
void crush_cursed_items_here(s16b x, s16b y)
{
   object_type *i_ptr;
   s16b         j;
   char        i_name[80];

   j = objects_on_floor(x,y)-1;
   for (;j>=0;j--)
   {
      i_ptr = get_item_pointer_floor_xy(j,x,y);
      /* the object_value routines don't like gold */
      if (i_ptr->tval == TV_GOLD) continue;
      /* auto-crushing corpses just isn't right    */
      if (i_ptr->tval == TV_CORPSE) continue;
      /* jk - determine flags */
      if (worthless(i_ptr))
      {
         object_desc(i_name, i_ptr, TRUE, 3);
         msg_format("You immediately destroy %s.",i_name);
         delete_object(px,py,j);
      }
   }
}

/*
 * this adds a log if this item is new 
 */
void test_new_object(object_type *i_ptr)
{
   /* paranoia */
   if (i_ptr->k_idx == 0) return;
   if (i_ptr->log.found_when == 0)
   {
      i_ptr->log.found_when = turn;
   }
   if (k_info[i_ptr->k_idx].log.found_when == 0)
   {
      k_info[i_ptr->k_idx].log.found_when = turn;
      k_info[i_ptr->k_idx].log.whose = i_ptr->log.whose;
      k_info[i_ptr->k_idx].log.where = i_ptr->log.where;
      k_info[i_ptr->k_idx].log.mlevel = i_ptr->log.mlevel;
      k_info[i_ptr->k_idx].log.slevel = i_ptr->log.slevel;
   }
}
            
/*
 * carry an item
 */
void carry_item(s16b item, s16b amt, s16b pickup)
{
   object_type *i_ptr = get_item_pointer_floor(item);
   char        i_name[80];
   s16b        num = i_ptr->number;
   bool        add_to_ammo = FALSE;
   bool        okay = TRUE;

   object_desc(i_name, i_ptr, TRUE, 3);

   disturb(0, 0);                /* Disturb */

   /* Pick up gold */
   if (i_ptr->tval == TV_GOLD)
   {
       /* Message - no check, because here pick_up_gold = FALSE, but we */
       /* already chose to pick up that specific item */
       msg_format("You have found %s.", i_name);
       p_ptr->au += i_ptr->p1val*i_ptr->number;     /* Collect the gold */
       p_ptr->redraw1 |= (PR1_GOLD);               /* Redraw gold */
       delete_object(px, py, item);                /* Delete gold */
   }
/* jk - a locked chest is not something to pick up lightly :-) */
   else if (i_ptr->tval == TV_CHEST)
   {
      if (i_ptr->xtra2)
      {
         trap_item_type *tr_ptr = &t_list[i_ptr->xtra2];
         if (first_trap(tr_ptr)!=-1)
         {
            s16b x = i_ptr->ix;
            s16b y = i_ptr->iy;
            /* Apply chest traps, if any */
            player_execute_trap(tr_ptr, item, x, y, FALSE);
         }
      }
      msg_print("You did not pick up the chest.");
   }
   else                              /* Pick it up */
   {
      if (!pickup)                  /* Describe the object */
      {
         msg_format("You see %s.", i_name);
         return;
      }
      else
      {
         object_type *j_ptr = &inventory[INVEN_AMMO];
dlog(DEBUGITEMS,"cmd1.c: carry_item: pickup_add_to_ammo %d similar %d item %s\n",
                pickup_add_to_ammo, object_similar(j_ptr, i_ptr), i_name);

         if (pickup_add_to_ammo && object_similar(j_ptr, i_ptr))
         {
            msg_format("You can add %s to your ammo.", i_name);
            add_to_ammo = TRUE;
         }
         else
         {
            if (!inven_carry_okay(i_ptr)) /* Note that the pack is too full */
            {
               msg_format("You have no room for %s.", i_name);
               return;
            }
         }
      }
      /* Pick up the item (if requested and allowed) */

      if (!always_pickup && !pickup) /* Hack -- query every item */
      /* if pickup is true, we specifically asked to pick something up - */
      /* don't ask again */
      {
         char out_val[160];
         sprintf(out_val, "Pick up %s? ", i_name);
         okay = get_check(out_val);
      }
      if (okay || pickup)              /* Attempt to pick up an object. */
      {
         if (!add_to_ammo)
         {
            s16b slot;

            test_new_object(i_ptr);
            slot = inven_carry(i_ptr,amt); /* Carry the item */

            if ((amt==1) && (num==1))
            {
               add_msg_format("You pick up %s.", i_name);
            }
            else if ((amt==1) && (num>1))
            {
               add_msg_format("You pick up one of the %s.", i_name);
            }
            else if ((amt>1) && (amt==num))
            {
               add_msg_format("You pick up all of the %s.", i_name);
            }
            else
            {
               add_msg_format("You pick up %d of the %s.", amt, i_name);
            }
            i_ptr = &inventory[slot];  /* Get the item again */
            object_desc(i_name, i_ptr, TRUE, 3);
            msg_format("You have %s (%c).", i_name, index_to_label(slot));
            if (amt>=num)
            {
              delete_object(px, py, item);   /* Delete original */
            }
         }
         else /* ammo */
         {
            object_type *j_ptr = &inventory[INVEN_AMMO];
            msg_format("You add %s to your ammunition.", i_name);
            object_absorb(j_ptr, i_ptr, amt);
            i_ptr->number-=amt;
            /* Increase the weight */
            p_ptr->total_weight += (i_ptr->number * i_ptr->weight * amt);
            if (amt>=num)
            {
              delete_object(px, py, item);   /* Delete original */
            }
         } /* ammo */
      } /* pick-up possible */
   } /* if not gold */
}

bool item_tester_hook_worthless(object_type *i_ptr)
{
   return !worthless(i_ptr);
}

/*
 * Player "wants" to pick up an object or gold.
 * Note that we ONLY handle things that can be picked up.
 * See "move player()" for handling of other things.
 */
/* jk - specific is when the player presses ',' to specifically pick something
 * up, not specific is walking over things.
 */
void carry(s16b pickup, bool specific)
{
/* jk */
   s16b cnt,cnt2,i,j;
   s16b index[MAX_FLOOR_ITEMS];
   s16b item = -1;
   s16b amt = 0;
   object_type *i_ptr;
   char i_name[80];

   /* Hack -- nothing here to pick up */
/* jk */
   cnt=objects_on_floor(px,py);
   if (cnt==0) return;
   if (kill_cursed_floor)
   {
      crush_cursed_items_here(px,py);
      cnt=objects_on_floor(px,py);
   }

   cnt2=0;
   for (i = cnt-1;i>=0;i--)
   {
      object_type *j_ptr = &inventory[INVEN_AMMO];
      i_ptr=get_item_pointer_floor(i);
      /* if it's a worthless item, don't bother */
      if (carry_cursed_flag && (worthless(i_ptr)) ) continue;
dlog(DEBUGITEMS,"cmd1.c: carry_item: pickup_add_to_ammo %d similar %d\n",
                pickup_add_to_ammo, object_similar(j_ptr, i_ptr));
      /* auto pick up similar ammo */
      if (pickup_add_to_ammo && object_similar(j_ptr, i_ptr))
      {
         object_desc(i_name, i_ptr, TRUE, 1);
         msg_format("You add %s to your ammo.", i_name);
         test_new_object(i_ptr);
         object_absorb(j_ptr, i_ptr, i_ptr->number);
         p_ptr->total_weight += (i_ptr->number * i_ptr->weight);
         index[cnt2++]=i;
      }
      else
      {
         for (j = 0; j < INVEN_PACK; j++)
         {
            j_ptr = &inventory[j];
            if (pickup && object_similar(j_ptr, i_ptr))
            {
               /* pity that you can't get object_desc to only say the number of items */
               /* if there are more than 1 */
               object_desc(i_name, i_ptr, FALSE, 0);
               msg_format("You pick up another %s%s.",
                   (i_ptr->number>1)? format("set of %d ",i_ptr->number) : "",
                   i_name);
               test_new_object(i_ptr);
               object_absorb(j_ptr, i_ptr,i_ptr->number);
               p_ptr->total_weight += (i_ptr->number * i_ptr->weight);
               index[cnt2++] = i;
               break;
            }
         } /* auto pick up ammo */
      }
   }
   if (cnt2>=1) /* only remove if there is something to find */
   {
      for (i=cnt2-1;i>=0;i--)
      {                                    /* deleting above screws up */
          delete_object(px,py,index[i]);   /* the indexes, so we do it */
      }                                    /* backwards at the end     */
   }
   cnt=objects_on_floor(px,py);
   if ((pick_up_gold) && (cnt>0))
   {
/* auto-pick up gold if necessary */
      cnt2=0; /* how many gold found */
      for (i = 0;i<cnt;i++)
      {
         i_ptr=get_item_pointer_floor(i);
         if (i_ptr->tval==TV_GOLD) index[cnt2++]=i;
      }
      if (cnt2==1)
      {
         i_ptr=get_item_pointer_floor(index[0]);
         object_desc(i_name, i_ptr, TRUE, 3);
         msg_format("You have found %s.", i_name);
         p_ptr->au += i_ptr->p1val*i_ptr->number;     /* Collect the gold */
         p_ptr->redraw1 |= (PR1_GOLD);   /* Redraw gold */
      }
      else if (cnt2>1)
      {
         s32b total = 0;
         for (i = 0;i<cnt2;i++)
         {
             i_ptr=get_item_pointer_floor(index[i]);
dlog(DEBUGITEMS,"cmd1.c: carry: gold item p1val %ld number %d total now %ld\n", i_ptr->p1val,
          i_ptr->number, total);
             p_ptr->au += i_ptr->p1val*i_ptr->number; /* Collect the gold */
             total += (s32b) i_ptr->p1val*i_ptr->number;
         }
         p_ptr->redraw1 |= (PR1_GOLD);
         msg_format("You found a total of %ld worth of various gems.",total);
      }
      if (cnt2>=1) /* only remove if there is something to find */
      {
         for (i=cnt2-1;i>=0;i--)
         {                                     /* deleting above screws up */
            delete_object(px,py,index[i]);     /* the indexes, so we do it */
         }                                     /* backwards at the end     */
      }
      disturb(0,0);
      cnt=objects_on_floor(px,py);
   } /* pick up gold */
   if (cnt>1) /* if a pile consists of nothing but junk, tell this */
   {
      s16b cnt3 = 0;
      for (i = 0;i<cnt;i++)
      {
         i_ptr=get_item_pointer_floor(i);
         if (carry_cursed_flag && (worthless(i_ptr)) && !specific) cnt3++;
      }
      if (cnt3==cnt)
      {
         msg_print("You see nothing but junk in this pile.");
         return;
      }
   }
   if (cnt==0) return;
   if (cnt==1)
   {
      item = 0;
      i_ptr=get_item_pointer_floor(item);
      /* default: pick up all */
      amt=i_ptr->number;
      object_desc(i_name, i_ptr, FALSE, 3);
      if (carry_cursed_flag && (worthless(i_ptr)) && !specific)
      {
         msg_format("You leave the worthless %s on the floor.",i_name);
         return;
      }
      if (pickup && !inven_carry_okay(i_ptr)) /* pack is too full */
      {
         object_desc(i_name, i_ptr, TRUE, 3);
         msg_format("You have no room for %s.", i_name);
         return;
      }
      if ((i_ptr->number>1) && pickup)
      {
         object_desc(i_name, i_ptr, FALSE, 0);
         amt=get_quantity(format("How many %s of the %d do you want to pick up? ",
                          i_name,i_ptr->number),i_ptr->number,i_ptr->number);
         if (amt==0) /* player pressed escape */
         {
           return;
         }
      }
      else
      {
         amt=1;
      }
      carry_item(item, amt, pickup);
   }
   else
   {
      /* jk - items which can be absorbed have been picked up already */
      /* so we can simply check this! */
      if (inven_cnt == INVEN_PACK)
      {
         char query;
         Term_save();
         prt("Your pack is too full to pick up anything of this pile.  --pause--",
              0, MESSAGE_ROW);
         show_floor(px,py);
         query = inkey();
         Term_load();
         return;
      }
      item_tester_tval=0;     /* no restrictions on type */
      item_tester_hook=NULL;  /* no auxiliary function */
      item_tester_full=FALSE; /* don't show empty slots */
      p_ptr->command_wrk=0;          /* start with showing what's there */
      p_ptr->command_see=TRUE;
      if (carry_cursed_flag && !specific)  /* don't show worthless items */
      {
         item_tester_hook = item_tester_hook_worthless;
      }
      /* FALSE = inven FALSE = equip TRUE = floor */
      if (pickup)
      {
         while (get_item(&item, &amt, "Pick up what item?",FALSE,FALSE,TRUE))
         {
            item -=INVEN_TOTAL;
            carry_item(item, amt, pickup);
            p_ptr->command_wrk=0;           /* these are reset in get_item */
            p_ptr->command_see=TRUE;
            cnt--;                   /* no sense asking when there's not */
            if (cnt==0) break;       /* something left                   */
            if (carry_cursed_flag && !specific)  /* don't show worthless items */
            {
              item_tester_hook = item_tester_hook_worthless;
            }
            if (inven_cnt == INVEN_PACK)
            {
               char query;
               Term_save();
               prt("Your pack is too full to pick up anything more of this pile.  --pause--",
                    0, MESSAGE_ROW);
               show_floor(px,py);
               query = inkey();
               Term_load();
               item_tester_hook = NULL;
               item_tester_tval = 0;
               return;
            }
         }
      }
      else
      {
         char c;
         Term_save();
         show_floor(px,py);
         prt("You see:                  (press any key to continue)",0,0);
         c=inkey();
         Term_load();
      }
      if (carry_cursed_flag && !specific)  /* reset */
      {
         item_tester_hook = NULL;
         item_tester_tval = 0;
      }
   }
}

/*
 * Handle player stumbling into a trap
 */
static void hit_trap(void)
{
   cave_cell_type      *c_ptr;
   trap_item_type      *tr_ptr;

   /* Disturb the player */
   disturb(0, 0);

   /* Get the cave grid */
   c_ptr = &dungeon.level[sublevel][py][px];
dlog(DEBUGTRAPS,"cmd1.c: hit_trap: testing for trap at %d,%d\n", px, py);
   if (!c_ptr->t_idx) return;
dlog(DEBUGTRAPS,"cmd1.c: hit_trap: trap at %d,%d idx %d\n", px, py, c_ptr->t_idx);

   tr_ptr = &t_list[c_ptr->t_idx];

   /* item -1 means it's on floor/door */
dlog(DEBUGTRAPS,"cmd1.c: hit_trap at %d,%d: i_idx %d m_idx %d t_idx %d\n", px, py,
                c_ptr->i_idx, c_ptr->m_idx, c_ptr->t_idx);
dlog(DEBUGTRAPS,"fdat %08lx mtyp %d styp %d\n", c_ptr->fdat,c_ptr->mtyp,c_ptr->styp);
dlog(DEBUGTRAPS,"cmd1.c: hit_trap: now calling player_execute_trap\n");
   player_execute_trap(tr_ptr,-1,px,py, FALSE);
}

/*
 * Player attacks a (poor, defenseless) creature        -RAK-
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
s16b py_attack(s16b x, s16b y)
{
   s16b                 num = 0, k, bonus, chance, tohit, i, vhp;
   s16b                 result = 100;
   u64b                 f1, f2, f3;
   char                 m_name[80], w_name1[80], w_name2[80];
   bool                 fear = FALSE;
   bool                 do_quake = FALSE;
   bool                 was_asleep;
   bool                 rogue_hit = FALSE;
   cave_cell_type      *c_ptr = &dungeon.level[sublevel][y][x];
   monster_type        *m_ptr = &mn_list[c_ptr->m_idx];
   monster_race        *r_ptr = &r_info[m_ptr->r_idx];
   object_type         *i_ptr;
   project_who_type     who;

   /* this array is used from 1, so +1 */
   s16b                 blows_type[p_ptr->num_blow1 + p_ptr->num_blow2 + 1];
   s16b                 have_hit_deep = randint(p_ptr->num_blow1 + p_ptr->num_blow2);

   was_asleep = (m_ptr->csleep>0);

   /* if we wear no rings of damage, no deep hits */
   if (p_ptr->ring_to_d==0) have_hit_deep = 0;

   /* Disturb the player */
   disturb(0, 0);

   /* Disturb the monster */
   m_ptr->csleep = 0;

   /* Extract monster name (or "it") */
   monster_desc(m_name, m_ptr, 0x80);
dlog(DEBUGFIGHT,"cmd1.c: py_attack: attacking %s at %d,%d player %d,%d\n", m_name, m_ptr->fx, m_ptr->fy, px, py);

   monster_desc(m_name, m_ptr, 0);

   /* Auto-Recall if possible and visible */
   if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

   /* Track a new monster */
   if (m_ptr->ml) health_track(c_ptr->m_idx);

   /* Handle player fear */
   if (p_ptr->afraid)
   {
      msg_format("You are too afraid to attack %s!", m_name);
      return (result);
   }
   if (p_ptr->pclass == CLASS_HIGHPRST)
   {
      msg_format("You bump into %s!", m_name);
      return (result);
   }

   for (i=1; i<=p_ptr->num_blow1; i++)
   {
      blows_type[i]=INVEN_WIELD;
   }
/* jk - if it's a gladiator, make sure the weapons are used */
/* in a random sequence */
   if ( (p_ptr->pclass == CLASS_GLADIATR) && inventory[INVEN_ARM].k_idx)
   {
      /* build standard sequence */
      for (i=p_ptr->num_blow1+1; i <= p_ptr->num_blow1 + p_ptr->num_blow2; i++)
      {
         blows_type[i]=INVEN_ARM;
      }

      /* make random permutations */
      for (i=1; i <= p_ptr->num_blow1 + p_ptr->num_blow2; i++)
      {
         s16b temp;
         s16b t1,t2;
         t1 = randint(p_ptr->num_blow1 + p_ptr->num_blow2);
         t2 = randint(p_ptr->num_blow1 + p_ptr->num_blow2);
         if (t1==t2) continue;
         temp = blows_type[t1];
         blows_type[t1]=blows_type[t2];
         blows_type[t2]=temp;
      }
   }

   /* Attack once for each legal blow */
   while (num++ < p_ptr->num_blow1 + p_ptr->num_blow2)
   {
      bool hit_with_first; /* which weapon is used */

      hit_with_first = TRUE;

      /* Access the weapon */
      if (p_ptr->pclass == CLASS_GLADIATR)
      {
         i_ptr = &inventory[blows_type[num]];
         if (blows_type[num] == INVEN_ARM)
         {
            hit_with_first = FALSE;
            object_desc(w_name2, i_ptr, FALSE, 0xc0);
            /* are we fighting with two almost identical weapons? */
            if (inventory[INVEN_WIELD].k_idx)
            {
               object_desc(w_name1, &inventory[INVEN_WIELD], FALSE, 0xc0);
               if (stricmp(w_name1, w_name2)==0)
               {
                  char buffer[80];
                  sprintf(buffer, "second %s", w_name2);
                  strcpy(w_name2, buffer);
               }
            }
         }
         else
         {
            object_desc(w_name1, i_ptr, FALSE, 0xc0);
         }
      }
      else
      {
         i_ptr = &inventory[INVEN_WIELD];
         object_desc(w_name1, i_ptr, FALSE, 0xc0);
      }

      /* if we hit using an untried weapon, chances are we try it in the process */
      if (need_tries(i_ptr) & (!(i_ptr->ident & ID_EQUIPTESTED)))
      {
         test_equipment(i_ptr, TRUE);
      }

      /* jk  - for vampiric weapons */
      /* Extract some flags */
      object_flags(i_ptr, &f1, &f2, &f3);

      /* Calculate the "attack quality" */
      /* jk - two handed */
      bonus = p_ptr->to_h + (p_ptr->twohands ? i_ptr->to_h*3/2 : i_ptr->to_h);
      chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

      if ((p_ptr->pclass==CLASS_ROGUE) && was_asleep )
      {
         chance += p_ptr->lev*8;
         was_asleep=FALSE;
         rogue_hit = TRUE;
      }

      /* Test for hit */
      if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
      {
         /* Sound */
         sound(SOUND_HIT);

/* jk - do vampiric damage only if weapon vampiric and monster warm-blooded */

         /* Message */
/* jk - insert num with it */

         if (number_hit_messages)
         {
            if (!rogue_hit)
            {
               msg_format("%d: You hit %s %s.", num, m_name,
                          format("with your %s",hit_with_first?w_name1:w_name2));
            }
            else
            {
               char m_name2[80];
               monster_desc(m_name2, m_ptr, 0x0100);
               msg_format("%d: You carefully hit the sleeping %s %s.",
                          num, m_name2,
                          format("with your %s",hit_with_first?w_name1:w_name2));
            }
         }
         else
         {
            if (!rogue_hit)
            {
               msg_format("You hit %s %s.", m_name,
                          format("with your %s",hit_with_first?w_name1:w_name2));
            }
            else
            {
               char m_name2[80];
               monster_desc(m_name2, m_ptr, 0x0100);
               msg_format("You carefully hit the sleeping %s %s.", m_name2,
                          format("with your %s",hit_with_first?w_name1:w_name2));
            }
         }

         if ( (f1 & (TR1_VAMPIRIC1| TR1_VAMPIRIC2)) &&
             (!(r_ptr->flags2 & RF2_COLD_BLOOD)) )
         {
            /* increase player HP by p1val % of monster HP, maximum */
            /* to_d value of weapon */
            if (f1 & TR1_VAMPIRIC1)
            {
               vhp=(s16b)((s32b)i_ptr->p1val*(s32b)m_ptr->hp)/(s32b)100;
            }
            else
            {
               vhp=(s16b)((s32b)i_ptr->p2val*(s32b)m_ptr->hp)/(s32b)100;
            }
            if (vhp>i_ptr->to_d) vhp=i_ptr->to_d;
            if (vhp < 5) vhp = 5;
            if (wizard)
            {
                msg_format("You gain %d hp %s.",vhp,
                          format("with your vampiric %s",hit_with_first?w_name1:w_name2));
            }
            hp_player(vhp);
         }

         /* Hack -- bare hands do one damage */
         k = 1;

         /* Handle normal weapon */
         if (i_ptr->k_idx)
         {
            k = damroll(i_ptr->dd, i_ptr->ds);
/* jk */
            if (p_ptr->twohands) k=k*3/2;
            k = tot_dam_aux(i_ptr, k, m_ptr);
            if ((f1 & TR1_IMPACT) && (k > 50)) do_quake = TRUE;
/* jk - rogues have an increased chance of hard-hitting a sleeping monster */
            tohit = i_ptr->to_h;
            if (p_ptr->twohands) tohit=tohit*3/2;

            if (rogue_hit)
            {
               tohit += p_ptr->lev * 8;
            }

            k = critical_norm(i_ptr->weight, tohit, k, TRUE);
            k += i_ptr->to_d;
/* jk - rings of damage only work once per turn on average   */
/* say 8 turns, first randint(8)==1, then randint(7)==1 etc. */
/* rings of damage only work once per round...               */
            if (have_hit_deep==num)
            {
               if ((p_ptr->num_blow1 + p_ptr->num_blow2)>1)
               {
                  if (p_ptr->ring_to_d>0)
                  {
                     msg_format("You hit deeply %s.",
                                format("with your %s",hit_with_first?w_name1:w_name2));
                  }
                  else if (p_ptr->ring_to_d<0)
                  {
                     msg_format("You almost miss %s.",
                                format("with your %s",hit_with_first?w_name1:w_name2));
                  }
               }
               k += p_ptr->ring_to_d;
               have_hit_deep=0;
            }
         }

         /* Apply the player damage bonuses */
         k += p_ptr->to_d;

         /* No negative damage */
         if (k < 0) k = 0;

         /* Complex message */
         if (wizard)
         {
            msg_format("You do %d (out of %d) damage %s.", k, m_ptr->hp,
                       format("with your %s",hit_with_first?w_name1:w_name2));
         }

         /* Damage, check for fear and death */
         who.type = WHO_PLAYER;
         if (mon_take_hit(&who, c_ptr->m_idx, k, &fear, NULL))
         {
            result = (num*100)/(p_ptr->num_blow1 + p_ptr->num_blow2);
            break;
         }

         /* Confusion attack */
         if (p_ptr->confusing)
         {
            /* Cancel glowing hands */
            p_ptr->confusing = FALSE;

            /* Message */
            msg_print("Your hands stop glowing.");

            /* Confuse the monster */
            if (r_ptr->flags3 & RF3_NO_CONF)
            {
               if (m_ptr->ml) r_ptr->r_flags3 |= RF3_NO_CONF;
               msg_format("%^s is unaffected.", m_name);
            }
            else if (rand_int(100) < r_ptr->level)
            {
               msg_format("%^s is unaffected.", m_name);
            }
            else
            {
               msg_format("%^s appears confused.", m_name);
               m_ptr->confused += 10 + rand_int(p_ptr->lev) / 5;
            }
         }
      }

      /* Player misses */
      else
      {
         s16b dir, dir1 = 0, dir2 = 0;

         /* Sound */
         sound(SOUND_MISS);

         /* determine where to look for monsters flanking the current monster */
         switch(what_dir1(px, py, x, y))
         {
            case DIR_N : dir1=DIR_NW; dir2 = DIR_NE; break;
            case DIR_S : dir1=DIR_SW; dir2 = DIR_SE; break;
            case DIR_E : dir1=DIR_SE; dir2 = DIR_NE; break;
            case DIR_W : dir1=DIR_SW; dir2 = DIR_NW; break;
            case DIR_SE: dir1=DIR_S;  dir2 = DIR_E;  break;
            case DIR_SW: dir1=DIR_W;  dir2 = DIR_S;  break;
            case DIR_NE: dir1=DIR_E;  dir2 = DIR_N;  break;
            case DIR_NW: dir1=DIR_W;  dir2 = DIR_N;  break;
         }
dlog(DEBUGFIGHT,"cmd1.c: py_attack: player @ %d,%d attacking to %d,%d: dir %d = %s\n",
                px, py, x, y, what_dir1(px, py, x, y), dirstr[what_dir1(px, py, x, y)]);
dlog(DEBUGFIGHT,"cmd1.c: py_attack: dir1 -> %d,%d dir2 -> %d,%d\n",
                px+ddx[dir1], py+ddy[dir1], px+ddx[dir2], py+ddy[dir2]);

         /* is there a monster in dir1? */
         if (in_bounds(px+ddx[dir1], py+ddy[dir1]))
         {
dlog(DEBUGFIGHT,"cmd1.c: py_attack: looking for dir1 in %d,%d m_idx %d\n",
                px+ddx[dir1], py+ddy[dir1], dungeon.level[sublevel][py+ddy[dir1]][px+ddx[dir1]].m_idx);
            if (dungeon.level[sublevel][py+ddy[dir1]][px+ddx[dir1]].m_idx==0)
            {
               dir1 = -1;
            }
         }

         if (in_bounds(px+ddx[dir2], py+ddy[dir2]))
         {
dlog(DEBUGFIGHT,"cmd1.c: py_attack: looking for dir2 in %d,%d m_idx %d\n",
                px+ddx[dir2], py+ddy[dir2], dungeon.level[sublevel][py+ddy[dir2]][px+ddx[dir2]].m_idx);
            if (dungeon.level[sublevel][py+ddy[dir2]][px+ddx[dir2]].m_idx==0)
            {
               dir2 = -1;
            }
         }

         if ((dir1>0) && (dir2>0))
         {
            if (randint(2)==1)
            {
               dir = dir1;
            }
            else
            {
               dir = dir2;
            }
         }
         /* if both are invalid, we get an invalid dir either way here */
         else
         {
            dir = (dir1>0)?dir1:dir2;
         }
dlog(DEBUGFIGHT,"cmd1.c: py_attack: looking dir %d = %s, found dir1 %d dir2 %d\n",
                dir, (dir>0)?dirstr[dir]:"invalid", dir1, dir2);
         if ((dir>0) && (randint(200) > p_ptr->skill_thn))
         {
            char m_name2[80];
            cave_cell_type *c_ptr2;

dlog(DEBUGFIGHT,"cmd1.c: py_attack: dir %d @ %d,%d m_idx %d\n",
                dir, px+ddx[dir], py+ddy[dir], dungeon.level[sublevel][py+ddy[dir]][px+ddx[dir]].m_idx);

            c_ptr2 = &dungeon.level[sublevel][py+ddy[dir]][px+ddx[dir]];
            monster_desc(m_name2, &mn_list[c_ptr2->m_idx], 0x80);

            if (number_hit_messages)
            {
               msg_format("%d: You miss %s %s and startle %s %s",
                         num, m_name, format("with your %s",hit_with_first?w_name1:w_name2),
                         m_name2, dirstr[dir]);
            }
            else
            {
               msg_format("You miss %s %s and startle %s %s",
                         m_name, format("with your %s",hit_with_first?w_name1:w_name2),
                         m_name2, dirstr[dir]);
            }

            mn_list[c_ptr2->m_idx].csleep = 0;
         }
         else
         {

            /* Message */
            if (number_hit_messages)
            {
               msg_format("%d: You miss %s %s.",
                          num, m_name,
                          format("with your %s",hit_with_first?w_name1:w_name2));
            }
            else
            {
               msg_format("You miss %s %s.",
                          m_name,
                          format("with your %s",hit_with_first?w_name1:w_name2));
            }
         }
      }
   }

   /* Hack -- delay fear messages */
   if (fear && m_ptr->ml)
   {
      /* Sound */
      sound(SOUND_FLEE);

      /* Message */
      msg_format("%^s turns away from you in terror.", m_name);
   }

   /* Mega-Hack -- apply earthquake brand */
   if (do_quake) earthquake(py, px, 10);
   return (result);
}

/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should (probably) always induce energy expenditure.
 *
 * Note that moving will *always* take a turn, and will *always* hit
 * any monster which might be in the destination grid.  Previously,
 * moving into walls was "free" and did NOT hit invisible monsters.
 */
void move_player(s16b dir, s16b do_pickup, bool voluntary)
{
   s16b                 x, y;
   cave_cell_type           *c_ptr;
   monster_type        *m_ptr;
   bool                 can_move = FALSE;

   /* Find the result of moving */
   y = py + ddy[dir];
   x = px + ddx[dir];

   /* Examine the destination */
   c_ptr = &dungeon.level[sublevel][y][x];
dlog(DEBUGMOVES,"cmd1.c: move_player at %d,%d: i_idx %d m_idx %d t_idx %d\n", x, y,
           c_ptr->i_idx, c_ptr->m_idx, c_ptr->t_idx);
dlog(DEBUGMOVES,"fdat %08lx mtyp %d styp %d\n", c_ptr->fdat,c_ptr->mtyp,c_ptr->styp);
   /* Get the monster */
   m_ptr = &mn_list[c_ptr->m_idx];

   /* Hack -- attack monsters */
   if (c_ptr->m_idx)
   {
      /* Attack */
      energy_use = py_attack(x, y);
      p_ptr->sliding_now=FALSE;  /* we just collided on a monster */
   }

   /* Player can not walk through "walls" */
   else if (!floor_grid_bold(x,y))
   {
      /* Disturb the player */
      disturb(0, 0);

      p_ptr->sliding_now=FALSE;  /* we just collided on something else */
      sliding_dir=-1;

      /* Notice things in the dark */
      if (!(c_ptr->fdat & CAVE_MARK) &&
          (p_ptr->blind || !(c_ptr->fdat & CAVE_VIEW)))
      {
         if ( (c_ptr->mtyp == DUNG_WALL) &&
              (c_ptr->styp == DUNG_WALL_RUBBLE))
         {
            msg_print("You feel some rubble blocking your way.");
            c_ptr->fdat |= CAVE_MARK;
            note_spot(x, y);
            lite_spot(x, y);
         }

         /* Closed door */
         else if ( (c_ptr->mtyp == DUNG_DOOR) &&
                   ( (c_ptr->styp == DUNG_DOOR_CLOSED) ||
                     (c_ptr->styp == DUNG_DOOR_LOCKED) ||
                     (c_ptr->styp == DUNG_DOOR_JAMMED) ) )
         {
            /* auto open if we want that and the door is untrapped, or , if it isn't untrapped */
            /* if we asked for it if we need to - nice, isn't it?                              */
            if ( (auto_open) &&
                 ( (num_traps_xy(x, y, TRAP_FOUND) == 0) || (!ask_before_traps) ||
                   (ask_before_traps && get_check("Risk the trap on this door?") ) ) )
            {
               msg_print("Trying to open this door in the dark.");
               /* jk - if we use do_cmd_open, and have chests in our */
               /* inventory, we get questions about what to open */
               (void)open_something(x, y, -1);
            }
            else
            {
               msg_print("You feel a closed door blocking your way.");
            }
            c_ptr->fdat |= CAVE_MARK;
            note_spot(x, y);
            lite_spot(x, y);
         }

         /* chalk wall */
         else if ( (c_ptr->mtyp==DUNG_WALL) &&
                   (wall_art(c_ptr->styp)==DUNG_WALL_ART_CHALK) &&
                   (randint(10)==1) )
         {
            msg_print("You feel a wall crumble under your touch.");
            set_grid_type(x, y, DUNG_WALL, DUNG_WALL_RUBBLE, GRID_REPLACE, 0);
            note_spot(x,y);
            lite_spot(x,y);
         }

         else if (c_ptr->mtyp==DUNG_SHRUB)
         {
            if (c_ptr->styp==DUNG_SHRUB_BRAMBLE)
            {
               msg_print("You feel a thorny bush blocking your way.");
               c_ptr->fdat |= CAVE_MARK;
               note_spot(x,y);
               lite_spot(x, y);
            }
         }

         else if (c_ptr->mtyp == DUNG_SIGN)
         {
            msg_print("You feel some sort of Sign.");
            c_ptr->fdat |= CAVE_MARK;
            note_spot(x,y);
            lite_spot(x, y);
         }

         /* Wall (or secret door) */
         else
         {
            msg_print("You feel a wall blocking your way.");
            c_ptr->fdat |= CAVE_MARK;
            note_spot(x,y);
            lite_spot(x,y);
         }
      }

      /* Notice things */
      else
      {
         cptr name = f_name +
                     f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name;

         if (test_grid_ptr(c_ptr, DUNG_SHRUB, DUNG_SHRUB_BRAMBLE))
         {
            msg_format("There is %s %s blocking your way.",
            is_a_vowel(name[0])?"an":"a",name);
         }

         /* Closed doors */
         else if ( (c_ptr->mtyp == DUNG_DOOR) &&
                   ( (c_ptr->styp == DUNG_DOOR_CLOSED) ||
                     (c_ptr->styp == DUNG_DOOR_LOCKED) ||
                     (c_ptr->styp == DUNG_DOOR_JAMMED) ) )
         {
            /* auto open if we want that and the door is untrapped, or , if it isn't untrapped */
            /* if we asked for it if we need to - nice, isn't it?                              */
            if ( (auto_open) &&
                 ( (num_traps_xy(x, y, TRAP_FOUND) == 0) || (!ask_before_traps) ||
                   (ask_before_traps && get_check("Risk the trap on this door?") ) ) )
            {
               /* jk - if we use do_cmd_open, and have chests in our */
               /* inventory, we get questions about what to open */
               (void)open_something(x, y, -1);
            }
            else
            {
               msg_print("There is a closed door blocking your way.");
               note_spot(x,y);
               lite_spot(x,y);
            }
         }
         /* arena wall */
         else if (test_grid_ptr(c_ptr, DUNG_PERWALL, DUNG_PERWALL_ARDOOR))
         {
            msg_print("Why leave now?");
         }

         /* chalk wall */
         else if ( (c_ptr->mtyp==DUNG_WALL) &&
                   (wall_art(c_ptr->styp)==DUNG_WALL_ART_CHALK) &&
                   (randint(10)==1) )
         {
            msg_print("This wall crumbles under your touch.");
            set_grid_type(x, y, DUNG_WALL, DUNG_WALL_RUBBLE, GRID_REPLACE, 0);
            note_spot(x,y);
            lite_spot(x,y);
         }

         else if (c_ptr->mtyp == DUNG_SIGN)
         {
            msg_print("You can't walk through this Shop's Sign.");
            note_spot(x,y);
            lite_spot(x,y);
         }

         /* Wall (or secret door) */
         else
         {
            msg_print("There is a wall blocking your way.");
            note_spot(x,y);
            lite_spot(x,y);
         }
      }
   }

   /* Normal movement */
   else
   {
      s16b oy, ox;

      /*** Handle traversable terrain. -LM- ***/
      if (test_grid_ptr(c_ptr, DUNG_WALL, DUNG_WALL_RUBBLE))
      {
         /* Players take two turns to enter rubble. */
         if (randint(5)==1) msg_print("You have trouble moving over this rouble!");
         if (p_ptr->crossing)
         {
            can_move = TRUE;
         }
         else
         {
            p_ptr->crossing = TRUE;
         }

         /* Stop any run. */
dlog(DEBUGMOVES,"cmd1.c: move_player: rubble, stopping run\n");
         p_ptr->running = 0;
      }
      else if ((c_ptr->mtyp == DUNG_SHRUB) && (p_ptr->pclass != CLASS_RANGER))
      {
         /* Except for rangers and druids, players take two
          * turns to pass under trees.
          */
         if (randint(5)==1) msg_print("You have trouble moving through these trees!");
         if (p_ptr->crossing)
         {
            can_move = TRUE;
         }
         else
         {
            p_ptr->crossing = TRUE;
         }

         /* Stop any run. */
dlog(DEBUGMOVES,"cmd1.c: move_player: wilderness, stopping run\n");
         p_ptr->running = 0;

      }
      else if (c_ptr->mtyp == DUNG_WATER)
      {
         /* Cannot cross with an over-heavy burden.             */
         /* over-heavy is half of the limit before slowing down */
         s16b limit = adj_str_wgt[p_ptr->stat_ind[A_STR]]*50;

         /* those rings are usefull here */
         if (p_ptr->ffall) limit *= 2;

         can_move = (p_ptr->total_weight <= limit);

         /* Stop any run. */
dlog(DEBUGMOVES,"cmd1.c: move_player: water, stopping run\n");
         p_ptr->running = 0;

         /* Explain why you won't cross the water. */
         if (can_move == FALSE)
         {
            if (!wizard)
            {
               msg_print("You dare not cross carrying so much weight.");
            }
            else
            {
               msg_print("You dare not cross carrying so much weight, but you're a wizard :-).");
               can_move = TRUE;
            }
         }
      }
      else if (test_grid_ptr(c_ptr, DUNG_LAVA, DUNG_LAVA_NORMAL))
      {
         char answer;
         bool cont = TRUE;
         s16b damage;

         /* only bother if we're not already on lava ! */
         if (!test_grid(px, py, DUNG_LAVA, DUNG_LAVA_NORMAL))
         {
            /* Disturb. */
            disturb(0, 0);

            /* Smart enough to stop running. */
            if (p_ptr->running)
            {
               cont = get_check("Lava blocks your path.  Step into it?");
dlog(DEBUGMOVES,"cmd1.c: move_player: lava, stopping run\n");
               p_ptr->running = 0;
            }

            /* Smart enough to sense trouble. */
            else if ((!p_ptr->resist_fire) && (!p_ptr->oppose_fire) &&
               (!p_ptr->immune_fire))
            {
               msg_print("The heat of the lava scalds you!  Really enter?");
               answer = inkey();
               cont = ((answer == 'Y') || (answer == 'y'));
               msg_print(NULL);
            }
         }

         /* Enter if OK or confirmed. */
         if (cont)
         {
            /* Can always cross. */
            can_move = TRUE;

            /* Feather fall makes one lightfooted. */
            if (p_ptr->ffall)
            {
               damage = 25 + randint(50);
               msg_print("Your burn your feet walking lightly over the lava!");
            }
            else
            {
               if (p_ptr->total_weight < (weight_limit() / 3))
               {
                  damage = 100 + randint(100);
                  msg_print("You try to jump over the lava.");
               }
               else if (p_ptr->total_weight < (2 * weight_limit() / 3))
               {
                  damage = 150 + randint(250);
                  msg_print("You quickly run over the lava.");
               }
               else
               {
                  damage = 200 + randint(400);
                  msg_print("You sink deep into the lava!");
               }

               /* sometimes */
               if (randint(40)==1)
               {
                  if (inventory[INVEN_FEET].k_idx && !artifact_p(&inventory[INVEN_FEET]))
                  {
                     object_type *i_ptr = &inventory[INVEN_FEET];
                     char i_name[80];

                     /* Get a description */
                     object_desc(i_name, i_ptr, FALSE, 3);

                     /* Message */
                     msg_format("The %s you are wearing is destroyed!",
                                i_name);

                     /* Destroy "amt" items */
                     item_increase(INVEN_FEET, -1, px, py);
                     item_optimize(INVEN_FEET, px, py);
                  }
               }
            }

            /* Will take serious fire damage. */
            fire_dam(damage, "stepping in molten lava");
         }
      }
      else if ( (c_ptr->mtyp == DUNG_TRAP) && (ask_before_traps) &&
                (voluntary==TRUE) && (!c_ptr->fdat & CAVE_PLAYERTRAP))
      {
         can_move = get_check("Do you want to risk this trap?");
      }

      else
      {
         /* All other terrain can be traversed normally. */
         can_move = TRUE;
      }

      if (can_move)
      {
         /* Save old location */
         oy = py;
         ox = px;

         /* Move the player */
         py = y;
         px = x;

         move_special_location(p_ptr->mdepth, p_ptr->sdepth, ox, oy, px, py);

         /* Redraw new spot */
         lite_spot(px, py);

         /* Redraw old spot */
         lite_spot(ox, oy);

         /* No longer traversing. */
         p_ptr->crossing = FALSE;

         /* Check for new panel (redraw map) */
         verify_panel();

         /* Update stuff */
         p_ptr->update |= (PU_UN_VIEW | PU_VIEW | PU_FLOW);
dlog(DEBUGLIGHT,"cmd1.c: move_player: PU_UN_VIEW en PU_VIEW set, update %016Lx\n", p_ptr->update);

         /* Update the monsters */
         p_ptr->update |= (PU_DISTANCE);

         /* Spontaneous Searching */
         if ((p_ptr->skill_pcp >= 50) ||
             (0 == rand_int(50 - p_ptr->skill_pcp)))
         {
            search();
         }

         /* Continuous Searching */
         if (p_ptr->searching)
         {
            search();
         }

         /* Handle "objects" */

         if (objects_on_floor(x,y)>0) carry(do_pickup, FALSE);

         /* Handle "store doors" */
         if (c_ptr->mtyp == DUNG_ENTR)
         {
            /* Disturb */
            disturb(0, 0);

            /* Hack -- Enter store */
            p_ptr->command_new = '_';
         }

         /* Discover invisible traps */
         else if (test_grid_ptr(c_ptr, DUNG_FLOOR, DUNG_FLOOR_TRAP))
         {
            /* Disturb */
            disturb(0, 0);

            if (!c_ptr->t_idx) /* there are no traps here */
            {
               (void)set_grid_type(x, y, DUNG_FLOOR,
                                   DUNG_FLOOR_NORMAL, GRID_ADD, 0);
dlog(DEBUGTRAPS,"cmd1.c: move_player: silently removing trap with no t_idx at %d,%d\n", x, y);
            }
            else
            {
               /* Hit the trap */
               p_ptr->sliding_now=FALSE;  /* we just collided on a trap */
dlog(DEBUGTRAPS,"cmd1.c: move_player about to call hit_trap at %d,%d: i_idx %d m_idx %d t_idx %d\n", x, y,
              c_ptr->i_idx, c_ptr->m_idx, c_ptr->t_idx);
dlog(DEBUGTRAPS,"fdat %08lx mtyp %d styp %d\n", c_ptr->fdat,c_ptr->mtyp,c_ptr->styp);
               hit_trap();
            }
         }

         /* Set off an visible trap if we didn't set it ourselves */
         else if ((c_ptr->mtyp == DUNG_TRAP) && (! (c_ptr->fdat & CAVE_PLAYERTRAP)))
         {
            /* Disturb */
            disturb(0, 0);
            if (!c_ptr->t_idx) /* there are no traps here */
            {
               (void)set_grid_type(x, y, DUNG_FLOOR,
                                   DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
            }
            else
            {
               /* Hit the trap */
           /* if we set it afterwards, traps of sliding never get it TRUE :-) */
               p_ptr->sliding_now=FALSE;  /* we just collided on a trap */
dlog(DEBUGTRAPS,"cmd1.c: move_player: hitting trap at %d,%d\n", px, py);
               hit_trap();
            }
         }
         else if ((c_ptr->mtyp == DUNG_TRAP) && (c_ptr->fdat & CAVE_PLAYERTRAP))
         {
            msg_print("You lightly step around the trap you made.");
         }
         /* if we should notice this place, or we cannot see what is below */
         /* the objects and it is not a floor, let us know                 */
         else if ( (c_ptr->fdat & CAVE_NOTICE) ||
                 ( (c_ptr->styp != DUNG_FLOOR) && (c_ptr->i_idx != 0) ) )
         {
            cptr name = f_name +
                        f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name;
            cptr desc1 = "are on", desc2 = "";
            bool prep = TRUE;
            switch (c_ptr->mtyp)
            {
               case DUNG_SIGN:  desc1 = "walk under";
                                prep = TRUE;
                                break;
               case DUNG_SHRUB: desc1 = "walk under";
                                desc2 = "";
                                prep = TRUE;
                                break;
               case DUNG_WATER: desc1 = "slosh through";
                                prep = TRUE;
                                break;
               case DUNG_WILDN: desc1 = "walk over";
                                prep = FALSE;
                                break;
            }

            /* don't say this if we keep walking on the same type of square */
            /* as we were on the previous time                              */
            if ((c_ptr->mtyp != dungeon.level[sublevel][oy][ox].mtyp) ||
                (c_ptr->styp != dungeon.level[sublevel][oy][ox].styp))
            {
               msg_format("You %s %s%s%s.\n", desc1,
                          prep?(is_a_vowel(name[0])?"an ":"a "):"",
                          desc2, name);
            }
         }
      }
   }
dlog(DEBUGMOVES,"cmd1.c: move_player: returning\n");
}

/*
 * Hack -- Check for a "motion blocker" (see below)
 */
static s16b see_wall(s16b dir, s16b x, s16b y)
{
   /* Get the new location */
   x += ddx[dir];
   y += ddy[dir];

   /* Illegal grids are blank */
   if (!in_bounds2(x, y)) return (FALSE);

   /* Must actually block motion */
   if (dungeon.level[sublevel][y][x].fdat & CAVE_WALK) return (FALSE);

   /* Must be known to the player */
   if (!(dungeon.level[sublevel][y][x].fdat & CAVE_MARK)) return (FALSE);

   /* Default */
   return (TRUE);
}

/*
 * Hack -- Check for an "unknown corner" (see below)
 */
static s16b see_nothing(s16b dir, s16b y, s16b x)
{
   /* Get the new location */
   x += ddx[dir];
   y += ddy[dir];

   /* Illegal grids are unknown */
   if (!in_bounds2(x,y)) return (TRUE);

   /* Memorized grids are known */
   if (dungeon.level[sublevel][y][x].fdat & CAVE_MARK) return (FALSE);

   /* Non-floor grids are unknown */
   if (!floor_grid_bold(x,y)) return (TRUE);

   /* Viewable grids are known */
   if (player_can_see_bold(x,y)) return (FALSE);

   /* Default */
   return (TRUE);
}

/*
   The running algorithm:                       -CJS-

   In the diagrams below, the player has just arrived in the
   grid marked as '@', and he has just come from a grid marked
   as 'o', and he is about to enter the grid marked as 'x'.

   Of course, if the "requested" move was impossible, then you
   will of course be blocked, and will stop.

   Overview: You keep moving until something interesting happens.
   If you are in an enclosed space, you follow corners. This is
   the usual corridor scheme. If you are in an open space, you go
   straight, but stop before entering enclosed space. This is
   analogous to reaching doorways. If you have enclosed space on
   one side only (that is, running along side a wall) stop if
   your wall opens out, or your open space closes in. Either case
   corresponds to a doorway.

   What happens depends on what you can really SEE. (i.e. if you
   have no light, then running along a dark corridor is JUST like
   running in a dark room.) The algorithm works equally well in
   corridors, rooms, mine tailings, earthquake rubble, etc, etc.

   These conditions are kept in static memory:
        find_openarea    You are in the open on at least one
                         side.
        find_breakleft   You have a wall on the left, and will
                         stop if it opens
        find_breakright  You have a wall on the right, and will
                         stop if it opens

   To initialize these conditions, we examine the grids adjacent
   to the grid marked 'x', two on each side (marked 'L' and 'R').
   If either one of the two grids on a given side is seen to be
   closed, then that side is considered to be closed. If both
   sides are closed, then it is an enclosed (corridor) run.

         LL             L
        @x             LxR
         RR            @R

   Looking at more than just the immediate squares is
   significant. Consider the following case. A run along the
   corridor will stop just before entering the center point,
   because a choice is clearly established. Running in any of
   three available directions will be defined as a corridor run.
   Note that a minor hack is inserted to make the angled corridor
   entry (with one side blocked near and the other side blocked
   further away from the runner) work correctly. The runner moves
   diagonally, but then saves the previous direction as being
   straight into the gap. Otherwise, the tail end of the other
   entry would be perceived as an alternative on the next move.

           #.#
          ##.##
          .@x..
          ##.##
           #.#

   Likewise, a run along a wall, and then into a doorway (two
   runs) will work correctly. A single run rightwards from @ will
   stop at 1. Another run right and down will enter the corridor
   and make the corner, stopping at the 2.

        #@x       1
        ########### ######
        2           #
        #############
        #

   After any move, the function area_affect is called to
   determine the new surroundings, and the direction of
   subsequent moves. It examines the current player location
   (at which the runner has just arrived) and the previous
   direction (from which the runner is considered to have come).

   Moving one square in some direction places you adjacent to
   three or five new squares (for straight and diagonal moves
   respectively) to which you were not previously adjacent,
   marked as '!' in the diagrams below.

       ...!       ...
       .o@!       .o.!
       ...!       ..@!
                   !!!

   You STOP if any of the new squares are interesting in any way:
   for example, if they contain visible monsters or treasure.

   You STOP if any of the newly adjacent squares seem to be open,
   and you are also looking for a break on that side. (that is,
   find_openarea AND find_break).

   You STOP if any of the newly adjacent squares do NOT seem to be
   open and you are in an open area, and that side was previously
   entirely open.

   Corners: If you are not in the open (i.e. you are in a corridor)
   and there is only one way to go in the new squares, then turn in
   that direction. If there are more than two new ways to go, STOP.
   If there are two ways to go, and those ways are separated by a
   square which does not seem to be open, then STOP.

   Otherwise, we have a potential corner. There are two new open
   squares, which are also adjacent. One of the new squares is
   diagonally located, the other is straight on (as in the diagram).
   We consider two more squares further out (marked below as ?).

   We assign "option" to the straight-on grid, and "option2" to the
   diagonal grid, and "check_dir" to the grid marked 's'.

          .s
         @x?
          #?

   If they are both seen to be closed, then it is seen that no
   benefit is gained from moving straight. It is a known corner.
   To cut the corner, go diagonally, otherwise go straight, but
   pretend you stepped diagonally into that next location for a
   full view next time. Conversely, if one of the ? squares is
   not seen to be closed, then there is a potential choice. We check
   to see whether it is a potential corner or an intersection/room entrance.
   If the square two spaces straight ahead, and the space marked with 's'
   are both blank, then it is a potential corner and enter if find_examine
   is set, otherwise must stop because it is not a corner.
*/

/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static byte cycle[] = { 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] = { 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };

/*
 * The direction we are running
 */
static byte find_current;

/*
 * The direction we came from
 */
static byte find_prevdir;

/*
 * We are looking for open area
 */
static bool find_openarea;

/*
 * We are looking for a break
 */
static bool find_breakright;
static bool find_breakleft;

/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(s16b dir)
{
   s16b         row, col, deepleft, deepright;
   s16b         i, shortleft, shortright;

   /* Save the direction */
   find_current = dir;

   /* Assume running straight */
   find_prevdir = dir;

   /* Assume looking for open area */
   find_openarea = TRUE;

   /* Assume not looking for breaks */
   find_breakright = find_breakleft = FALSE;

   /* Assume no nearby walls */
   deepleft = deepright = FALSE;
   shortright = shortleft = FALSE;

   /* Find the destination grid */
   row = py + ddy[dir];
   col = px + ddx[dir];

   /* Extract cycle index */
   i = chome[dir];

   /* Check for walls */
   if (see_wall(cycle[i+1], px, py))
   {
      find_breakleft = TRUE;
      shortleft = TRUE;
   }
   else if (see_wall(cycle[i+1], col, row))
   {
      find_breakleft = TRUE;
      deepleft = TRUE;
   }

   /* Check for walls */
   if (see_wall(cycle[i-1], px, py))
   {
      find_breakright = TRUE;
      shortright = TRUE;
   }
   else if (see_wall(cycle[i-1], col, row))
   {
      find_breakright = TRUE;
      deepright = TRUE;
   }

   /* Looking for a break */
   if (find_breakleft && find_breakright)
   {
      /* Not looking for open area */
      find_openarea = FALSE;

      /* Hack -- allow angled corridor entry */
      if (dir & 0x01)
      {
         if (deepleft && !deepright)
         {
            find_prevdir = cycle[i - 1];
         }
         else if (deepright && !deepleft)
         {
            find_prevdir = cycle[i + 1];
         }
      }

      /* Hack -- allow blunt corridor entry */
      else if (see_wall(cycle[i], col, row))
      {
         if (shortleft && !shortright)
         {
            find_prevdir = cycle[i - 2];
         }
         else if (shortright && !shortleft)
         {
            find_prevdir = cycle[i + 2];
         }
      }
   }
}

/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
   s16b                 prev_dir, new_dir, check_dir = 0;

   s16b                 row = -1, col = -1;
   s16b                 i, max, inv;
   s16b                 option, option2;

   /* jk */
   s16b                 j;

   cave_cell_type           *c_ptr;

   /* No options yet */
   option = 0;
   option2 = 0;

   /* Where we came from */
   prev_dir = find_prevdir;

   /* Range of newly adjacent grids */
   max = (prev_dir & 0x01) + 1;

   /* Look at every newly adjacent square. */
   for (i = -max; i <= max; i++)
   {
       new_dir = cycle[chome[prev_dir] + i];
       col = px + ddx[new_dir];
       row = py + ddy[new_dir];
dlog(DEBUGMOVES,"cmd1.c: run_test: px,py %d,%d looking at %d,%d\n", px, py, col, row);

       c_ptr = &dungeon.level[sublevel][row][col];

       /* Visible monsters abort running */
       if (c_ptr->m_idx)
       {
          monster_type *m_ptr = &mn_list[c_ptr->m_idx];
dlog(DEBUGMOVES,"cmd1.c: run_test: monster visible at %d,%d\n", col, row);

          /* Visible monster */
          if (m_ptr->ml)
          {
             return (TRUE);
          }
       }

       /* Visible objects abort running */
       j = objects_on_floor(col,row)-1;
       for (;j>=0;j--)
       {
          object_type *i_ptr = get_item_pointer_floor_xy(j,col,row);
          if (i_ptr->marked)
          {
dlog(DEBUGMOVES,"cmd1.c: run_test: object visible at %d,%d\n", col, row);
             return (TRUE);
          }
       }

       /* Assume unknown */
       inv = TRUE;

       /* Check memorized grids */
       if (c_ptr->fdat & CAVE_MARK)
       {
          bool notice = TRUE;

          if (is_floor(col,row))
          {
             notice = FALSE;
          }
          if (is_wall(col, row))
          {
             notice = FALSE;
          }

          else if ((c_ptr->mtyp == DUNG_STAIR) && find_ignore_stairs)
          {
             notice = FALSE;
          }
          else if ((c_ptr->mtyp == DUNG_DOOR) &&
                   (c_ptr->styp == DUNG_DOOR_OPEN) &&
                   (!find_ignore_doors))
          {
             notice = FALSE;
          }

          /* Interesting feature */
          if (notice)
          {
dlog(DEBUGMOVES,"cmd1.c: run_test: interesting feature (%d,%d) visible at %d,%d\n",
                c_ptr->mtyp, c_ptr->styp, col, row);
             return (TRUE);
          }

          /* The grid is "visible" */
          inv = FALSE;
       }

       /* Analyze unknown grids and floors */
       if (inv || floor_grid_bold(col, row))
       {
dlog(DEBUGMOVES,"cmd1.c: run_test: analyzing @ %d,%d, new_dir %d=%s\n", col, row, new_dir, dirstr[new_dir]);
          /* Looking for open area */
          if (find_openarea)
          {
             /* Nothing */
          }

          /* The first new direction. */
          else if (!option)
          {
dlog(DEBUGMOVES,"cmd1.c: run_test: setting option @ %d,%d\n", col, row);
             option = new_dir;
          }

          /* Three new directions. Stop running. */
          else if (option2)
          {
dlog(DEBUGMOVES,"cmd1.c: run_test: option2 at %d,%d\n", col, row);
             return (TRUE);
          }

          /* Two non-adjacent new directions.  Stop running. */
          else if (option != cycle[chome[prev_dir] + i - 1])
          {
dlog(DEBUGMOVES,"cmd1.c: run_test: 2 non-adjacent new directions at %d,%d\n", col, row);
             return (TRUE);
          }

          /* Two new (adjacent) directions (case 1) */
          else if (new_dir & 0x01)
          {
             check_dir = cycle[chome[prev_dir] + i - 2];
             option2 = new_dir;
dlog(DEBUGMOVES,"cmd1.c: run_test: 2 new adjacent directions1 at %d,%d - check_dir %d option2 %d\n",
                col, row, check_dir, option2);
          }

          /* Two new (adjacent) directions (case 2) */
          else
          {
             check_dir = cycle[chome[prev_dir] + i + 1];
             option2 = option;
             option = new_dir;
dlog(DEBUGMOVES,"cmd1.c: run_test: 2 new adjacent directions2 at %d,%d - check_dir %d option %d option2 %d\n",
                col, row, check_dir, option, option2);
          }
       }

       /* Obstacle, while looking for open area */
       else
       {
dlog(DEBUGMOVES,"cmd1.c: run_test: obstacle at %d,%d find_openarea %d\n", col, row, find_openarea);
          if (find_openarea)
          {
             if (i < 0)
             {
                /* Break to the right */
                find_breakright = TRUE;
             }

             else if (i > 0)
             {
                /* Break to the left */
                find_breakleft = TRUE;
             }
          }
       }
   }

dlog(DEBUGMOVES,"cmd1.c: run_test: about to look for open area\n");

   /* Looking for open area */
   if (find_openarea)
   {
      /* Hack -- look again */
      for (i = -max; i < 0; i++)
      {
         new_dir = cycle[chome[prev_dir] + i];

         row = py + ddy[new_dir];
         col = px + ddx[new_dir];
dlog(DEBUGMOVES,"cmd1.c: run_test: find_openarea1 @ %d,%d new_dir %d=%s\n",
                col, row, new_dir, dirstr[new_dir]);

         /* Unknown grid or floor */
         if ( (!(dungeon.level[sublevel][row][col].fdat & CAVE_MARK)) || (floor_grid_bold(col, row)))
         {
dlog(DEBUGMOVES,"cmd1.c: run_test: find_openarea1 unknown grid or floor @ %d,%d mark %d bold %d\n",
                col, row, dungeon.level[sublevel][row][col].fdat & CAVE_MARK, floor_grid_bold(col, row));

            /* Looking to break right */
            if (find_breakright)
            {
dlog(DEBUGMOVES,"cmd1.c: run_test: find_openarea1 returning\n");
               return (TRUE);
            }
         }

         /* Obstacle */
         else
         {
dlog(DEBUGMOVES,"cmd1.c: run_test: find_openarea1 obstacle @ %d,%d\n", col, row);
            /* Looking to break left */
            if (find_breakleft)
            {
dlog(DEBUGMOVES,"cmd1.c: run_test: find_openarea1 returning\n");
               return (TRUE);
            }
         }
      }

      /* Hack -- look again */
      for (i = max; i > 0; i--)
      {
         new_dir = cycle[chome[prev_dir] + i];

         row = py + ddy[new_dir];
         col = px + ddx[new_dir];
dlog(DEBUGMOVES,"cmd1.c: run_test: find_openarea2 @ %d,%d new_dir %d=%s\n",
                col, row, new_dir, dirstr[new_dir]);

         /* Unknown grid or floor */
         if ( (!(dungeon.level[sublevel][row][col].fdat & CAVE_MARK)) || (floor_grid_bold(col, row)))
         {
dlog(DEBUGMOVES,"cmd1.c: run_test: find_openarea2 unknown grid or floor @ %d,%d mark %d bold %d\n",
                col, row, dungeon.level[sublevel][row][col].fdat & CAVE_MARK, floor_grid_bold(col, row));
            /* Looking to break left */
            if (find_breakleft)
            {
dlog(DEBUGMOVES,"cmd1.c: run_test: find_openarea2 returning\n");
                return (TRUE);
            }
         }

         /* Obstacle */
         else
         {
dlog(DEBUGMOVES,"cmd1.c: run_test: find_openarea2 obstacle @ %d,%d\n", col, row);
            /* Looking to break right */
            if (find_breakright)
            {
dlog(DEBUGMOVES,"cmd1.c: run_test: find_openarea2 returning\n");
                return (TRUE);
            }
         }
      }
   }

   /* Not looking for open area */
   else
   {
dlog(DEBUGMOVES,"cmd1.c: run_test: no open area @ %d,%d option %d option2 %d\n",
                col, row, option, option2);

      /* No options */
      if (!option)
      {
         return (TRUE);
      }

      /* One option */
      else if (!option2)
      {
         /* Primary option */
         find_current = option;

         /* No other options */
         find_prevdir = option;
      }

      /* Two options, examining corners */
      else if (find_examine && !find_cut)
      {
         /* Primary option */
         find_current = option;

         /* Hack -- allow curving */
         find_prevdir = option2;
      }

      /* Two options, pick one */
      else
      {
         /* Get next location */
         row = py + ddy[option];
         col = px + ddx[option];

         /* Don't see that it is closed off. */
         /* This could be a potential corner or an intersection. */
         if (!see_wall(option, col, row) ||
             !see_wall(check_dir, col, row))
         {
            /* Can not see anything ahead and in the direction we */
            /* are turning, assume that it is a potential corner. */
            if (find_examine &&
                see_nothing(option, row, col) &&
                see_nothing(option2, row, col))
            {
               find_current = option;
               find_prevdir = option2;
            }

            /* STOP: we are next to an intersection or a room */
            else
            {
dlog(DEBUGMOVES,"cmd1.c: run_test: next to intersection @ %d,%d\n", col, row);
               return (TRUE);
            }
         }

         /* This corner is seen to be enclosed; we cut the corner. */
         else if (find_cut)
         {
            find_current = option2;
            find_prevdir = option2;
         }

         /* This corner is seen to be enclosed, and we */
         /* deliberately go the long way. */
         else
         {
            find_current = option;
            find_prevdir = option2;
         }
      }
   }

   /* About to hit a known wall, stop */
   if (see_wall(find_current, px, py))
   {
dlog(DEBUGMOVES,"cmd1.c: run_test: see_wall @ %d,%d\n", col, row);
      return (TRUE);
   }

   /* Failure */
   return (FALSE);
}

/*
 * Take one step along the current "run" path
 */
void run_step(s16b dir)
{
   /* Start running */
dlog(DEBUGMOVES,"cmd1.c: run_step: starting dir %d\n", dir);
   if (dir)
   {
      /* Hack -- do not start silly run */
      if (see_wall(dir, px, py))
      {
dlog(DEBUGMOVES,"cmd1.c: run_step: wall seen\n");
         /* Message */
         msg_print("You cannot run in that direction.");

         /* Disturb */
         disturb(0,0);

         /* Done */
         return;
      }

      /* Calculate torch radius */
      p_ptr->update |= (PU_TORCH);

      /* Initialize */
dlog(DEBUGMOVES,"cmd1.c: run_step: calling run_init, dir %d\n", dir);

      run_init(dir);
   }

   /* Keep running */
   else
   {
dlog(DEBUGMOVES,"cmd1.c: run_step: keep running, dir %d\n", dir);

      /* Update run */
      if (run_test())
      {
dlog(DEBUGMOVES,"cmd1.c: run_step: run_test() returned TRUE, stopping\n");
         /* Disturb */
         disturb(0,0);

         /* Done */
         return;
      }
   }
dlog(DEBUGMOVES,"cmd1.c: run_step: p_ptr->running now %d\n", p_ptr->running);

   /* Decrease the run counter */
   if (--p_ptr->running <= 0) return;

   /* Take time */
   energy_use = 100;

   /* Move the player, using the "pickup" flag */
dlog(DEBUGMOVES,"cmd1.c: run_step: moving player in dir %d=%s\n", find_current, dirstr[find_current]);
   move_player(find_current, always_pickup, TRUE);
dlog(DEBUGMOVES,"cmd1.c: run_step: returning\n");
}

/*
 * Start running.
 */
void do_cmd_run(void)
{
   s16b dir;

dlog(DEBUGMOVES,"cmd1.c: do_cmd_run about to start\n");
   /* Hack -- no running when confused */
   if (p_ptr->confused)
   {
      msg_print("You are too confused!");
      return;
   }

   if (p_ptr->sliding)
   {
      msg_print("You don't dare with these slippery feet.");
   }

   /* Get a "repeated" direction */
   if (get_rep_dir(&dir))
   {
      /* Hack -- Set the run counter */
      p_ptr->running = (p_ptr->command_arg ? p_ptr->command_arg : 1000);
dlog(DEBUGMOVES,"cmd1.c: do_cmd_run: first step dir %d, p_ptr->running %d\n", dir, p_ptr->running);

      /* First step */
      run_step(dir);
   }
dlog(DEBUGMOVES,"cmd1.c: do_cmd_run: returning\n");
}

