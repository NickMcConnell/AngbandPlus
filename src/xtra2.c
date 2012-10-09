/* Purpose: effects of various "objects" */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Set "p_ptr->blind", notice observable changes
 *
 * Note the use of "PU_UN_VIEW", which is needed to
 * memorize any terrain features which suddenly become "visible".
 * Note that blindness is currently the only thing which can affect
 * "player_can_see_bold()".
 */
bool set_blind(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->blind)
      {
         msg_print("You are blind!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->blind)
      {
         msg_print("You can see again.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->blind = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Forget stuff */
   p_ptr->update |= PU_UN_VIEW;

   /* Update stuff */
   p_ptr->update |= PU_VIEW;

   /* Update the monsters */
   p_ptr->update |= (PU_MONSTERS);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);

   /* Redraw the "blind" */
   p_ptr->redraw1 |= (PR1_BLIND);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

/* jk */
bool set_sliding(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   if (v>0)
   {
      if (!p_ptr->sliding)
      {
         msg_print("Your feet slide about!");
         notice = TRUE;
      }
   }
   else
   {
      if (p_ptr->sliding)
      {
         msg_print("Your feet have contact again.");
         notice = TRUE;
         p_ptr->sliding_now = FALSE;
      }
   }
   /* Use the value */
   p_ptr->sliding = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the "blind" */
   p_ptr->redraw1 |= (PR1_SLIDING);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

/* jk */
bool set_reflecting(s16b delta_v)
{
   bool notice = FALSE;
   static s16b time = 0;

   if (delta_v>0)
   {
      time++;
      if (time>5)
      {
         delta_v = 0;
         msg_print("Your skin continues to glow.");
      }
      else if (time>1)
      {
         delta_v /= time;
         msg_print("Your skin feels glows a little more.");
      }
      else
      {
         msg_print("Your skin feels different and softly glows.");
         notice = TRUE;
      }
      p_ptr->reflecting += delta_v;
   }
   else if ((p_ptr->reflecting + delta_v) <= 0)
   {
      time = 0;
      if (p_ptr->reflecting)
      {
         msg_print("Your skin seems to soften.");
         notice = TRUE;
         p_ptr->sliding_now = FALSE;
      }
      p_ptr->reflecting = 0;
   }
   else
      p_ptr->reflecting += delta_v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the "blind" */
   p_ptr->redraw2 |= (PR2_REFLECT);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->confused", notice observable changes
 */
bool set_confused(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->confused)
      {
         msg_print("You are confused!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->confused)
      {
         msg_print("You feel less confused now.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->confused = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the "confused" */
   p_ptr->redraw1 |= (PR1_CONFUSED);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

/*
 * Set "p_ptr->fire", notice observable changes
 */
bool set_fire(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->fire)
      {
         msg_print("Suddenly fire cackles along your skin!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->fire)
      {
         msg_print("Your skin cools down.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->fire = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the "confused" */
   p_ptr->redraw2 |= (PR2_FIRE);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

/*
 * Set "p_ptr->cold", notice observable changes
 */
bool set_cold(s16b v)
{
   bool notice = FALSE;
   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->cold)
      {
         msg_print("Suddenly fire your skin feels cold and brittle");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->cold)
      {
         msg_print("Your skin becomes supple again.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->cold = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the "cold" */
   p_ptr->redraw2 |= (PR2_COLD);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

/*
 * Set "p_ptr->acid", notice observable changes
 */
bool set_acid(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->acid)
      {
         msg_print("Suddenly a sizzling acidic fluid drips from your skin!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->acid)
      {
         msg_print("Your skin is dry again.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->acid = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the "confused" */
   p_ptr->redraw2 |= (PR2_ACID);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

/*
 * Set "p_ptr->elec", notice observable changes
 */
bool set_elec(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->elec)
      {
         msg_print("Suddenly electricity sparks from your skin!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->elec)
      {
         msg_print("Your hairs stand down.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->elec = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the "confused" */
   p_ptr->redraw2 |= (PR2_ELEC);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

/*
 * Set "p_ptr->lift", notice observable changes
 */
bool set_lift(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->lift)
      {
         msg_print("Suddenly you have the legs of a mule!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->lift)
      {
         msg_print("Your legs are normal once again.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->lift = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the "confused" */
   p_ptr->redraw2 |= (PR2_LIFT);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

/*
 * Set "p_ptr->throat", notice observable changes
 */
bool set_throat(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->throat)
      {
         msg_print("Your throat swells. Swallowing is impossible!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->throat)
      {
         msg_print("Your throat feels normal again.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->throat = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the "confused" */
   p_ptr->redraw2 |= (PR1_THROAT);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

/*
 * Set "p_ptr->poisoned", notice observable changes
 */
bool set_poisoned(s16b v)
{
   bool notice = FALSE;
/* jk - some extra code inserted because the Spear of Melkor keeps you */
/* poisoned at all times */
   object_type *i_ptr = &inventory[INVEN_WIELD];

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

/* jk - if you are poisoned, wielding Melkor's Spear and removing poison */
/* then you're in for a surprise..... */
   if ( (v==0) && (i_ptr->name1==ART_MELKOR) && (p_ptr->poisoned))
   {
      msg_print("You feel poison flowing up from your hands.");
      notice=TRUE;
      if (disturb_other) disturb(0,0);
      return (TRUE);
   }

   /* Open */
   if (v)
   {
      if (!p_ptr->poisoned)
      {
         msg_print("You are poisoned!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->poisoned)
      {
         msg_print("You are no longer poisoned.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->poisoned = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the "poisoned" */
   p_ptr->redraw1 |= (PR1_POISONED);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->afraid", notice observable changes
 */
bool set_afraid(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->afraid)
      {
         msg_print("You are terrified!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->afraid)
      {
         msg_print("You feel bolder now.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->afraid = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the "afraid" */
   p_ptr->redraw1 |= (PR1_AFRAID);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

/*
 * Set "p_ptr->paralyzed", notice observable changes
 */
bool set_paralyzed(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->paralyzed)
      {
         msg_print("You are paralyzed!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->paralyzed)
      {
         msg_print("You can move again.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->paralyzed = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw the state */
   p_ptr->redraw1 |= (PR1_STATE);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->image", notice observable changes
 *
 * Note that we must redraw the map when hallucination changes.
 */
bool set_image(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->image)
      {
         msg_print("You feel drugged!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->image)
      {
         msg_print("You can see clearly again.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->image = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);

   /* Update monsters */
   p_ptr->update |= (PU_MONSTERS);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->fast", notice observable changes
 */
bool set_fast(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->fast)
      {
         msg_print("You feel yourself moving faster!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->fast)
      {
         msg_print("You feel yourself slow down.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->fast = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->slow", notice observable changes
 */
bool set_slow(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->slow)
      {
         msg_print("You feel yourself moving slower!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->slow)
      {
         msg_print("You feel yourself speed up.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->slow = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->shield", notice observable changes
 */
bool set_shield(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->shield)
      {
         msg_print("A mystic shield forms around your body!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->shield)
      {
         msg_print("Your mystic shield crumbles away.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->shield = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}



/*
 * Set "p_ptr->blessed", notice observable changes
 */
bool set_blessed(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->blessed)
      {
         msg_print("You feel righteous!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->blessed)
      {
         msg_print("The prayer has expired.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->blessed = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->hero", notice observable changes
 */
bool set_hero(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->hero)
      {
         msg_print("You feel like a hero!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->hero)
      {
         msg_print("The heroism wears off.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->hero = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Recalculate hitpoints */
   p_ptr->update |= (PU_HP);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->shero", notice observable changes
 */
bool set_shero(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->shero)
      {
         msg_print("You feel like a killing machine!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->shero)
      {
         msg_print("You feel less Berserk.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->shero = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Recalculate hitpoints */
   p_ptr->update |= (PU_HP);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->protevil", notice observable changes
 */
bool set_protevil(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->protevil)
      {
         msg_print("You feel safe from evil!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->protevil)
      {
         msg_print("You no longer feel safe from evil.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->protevil = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->invuln", notice observable changes
 */
bool set_invuln(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->invuln)
      {
         msg_print("You feel invulnerable!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->invuln)
      {
         msg_print("You feel vulnerable once more.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->invuln = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->tim_invis", notice observable changes
 */
bool set_tim_invis(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->tim_invis)
      {
         msg_print("Your eyes feel very sensitive!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->tim_invis)
      {
         msg_print("Your eyes feel less sensitive.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->tim_invis = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Update the monsters */
   p_ptr->update |= (PU_MONSTERS);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->tim_infra", notice observable changes
 */
bool set_tim_infra(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->tim_infra)
      {
         msg_print("Your eyes begin to tingle!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->tim_infra)
      {
         msg_print("Your eyes stop tingling.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->tim_infra = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Update the monsters */
   p_ptr->update |= (PU_MONSTERS);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->oppose_acid", notice observable changes
 */
bool set_oppose_acid(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->oppose_acid)
      {
         msg_print("You feel resistant to acid!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->oppose_acid)
      {
         msg_print("You feel less resistant to acid.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->oppose_acid = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->oppose_elec", notice observable changes
 */
bool set_oppose_elec(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->oppose_elec)
      {
         msg_print("You feel resistant to electricity!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->oppose_elec)
      {
         msg_print("You feel less resistant to electricity.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->oppose_elec = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->oppose_fire", notice observable changes
 */
bool set_oppose_fire(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->oppose_fire)
      {
         msg_print("You feel resistant to fire!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->oppose_fire)
      {
         msg_print("You feel less resistant to fire.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->oppose_fire = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->oppose_cold", notice observable changes
 */
bool set_oppose_cold(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->oppose_cold)
      {
         msg_print("You feel resistant to cold!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->oppose_cold)
      {
         msg_print("You feel less resistant to cold.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->oppose_cold = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->oppose_pois", notice observable changes
 */
bool set_oppose_pois(s16b v)
{
   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Open */
   if (v)
   {
      if (!p_ptr->oppose_pois)
      {
         msg_print("You feel resistant to poison!");
         notice = TRUE;
      }
   }

   /* Shut */
   else
   {
      if (p_ptr->oppose_pois)
      {
         msg_print("You feel less resistant to poison.");
         notice = TRUE;
      }
   }

   /* Use the value */
   p_ptr->oppose_pois = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->stun", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool set_stun(s16b v)
{
   s16b old_aux, new_aux;

   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Knocked out */
   if (p_ptr->stun > 100)
   {
      old_aux = 3;
   }

   /* Heavy stun */
   else if (p_ptr->stun > 50)
   {
      old_aux = 2;
   }

   /* Stun */
   else if (p_ptr->stun > 0)
   {
      old_aux = 1;
   }

   /* None */
   else
   {
      old_aux = 0;
   }

   /* Knocked out */
   if (v > 100)
   {
      new_aux = 3;
   }

   /* Heavy stun */
   else if (v > 50)
   {
      new_aux = 2;
   }

   /* Stun */
   else if (v > 0)
   {
      new_aux = 1;
   }

   /* None */
   else
   {
      new_aux = 0;
   }

   /* Increase cut */
   if (new_aux > old_aux)
   {
      /* Describe the state */
      switch (new_aux)
      {
         /* Stun */
         case 1:
            msg_print("You have been stunned.");
            break;

         /* Heavy stun */
         case 2:
            msg_print("You have been heavily stunned.");
            break;

         /* Knocked out */
         case 3:
            msg_print("You have been knocked out.");
            break;
      }

      /* Notice */
      notice = TRUE;
   }

   /* Decrease cut */
   else if (new_aux < old_aux)
   {
      /* Describe the state */
      switch (new_aux)
      {
         /* None */
         case 0:
            msg_print("You are no longer stunned.");
            if (disturb_other) disturb(0, 0);
            break;
      }

      /* Notice */
      notice = TRUE;
   }

   /* Use the value */
   p_ptr->stun = v;

   /* No change */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Redraw the "stun" */
   p_ptr->redraw1 |= (PR1_STUN);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}


/*
 * Set "p_ptr->cut", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool set_cut(s16b v)
{
   s16b old_aux, new_aux;

   bool notice = FALSE;

   /* Hack -- Force good values */
   v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

   /* Mortal wound */
   if (p_ptr->cut > 1000)
   {
      old_aux = 7;
   }

   /* Deep gash */
   else if (p_ptr->cut > 200)
   {
      old_aux = 6;
   }

   /* Severe cut */
   else if (p_ptr->cut > 100)
   {
      old_aux = 5;
   }

   /* Nasty cut */
   else if (p_ptr->cut > 50)
   {
      old_aux = 4;
   }

   /* Bad cut */
   else if (p_ptr->cut > 25)
   {
      old_aux = 3;
   }

   /* Light cut */
   else if (p_ptr->cut > 10)
   {
      old_aux = 2;
   }

   /* Graze */
   else if (p_ptr->cut > 0)
   {
      old_aux = 1;
   }

   /* None */
   else
   {
      old_aux = 0;
   }

   /* Mortal wound */
   if (v > 1000)
   {
      new_aux = 7;
   }

   /* Deep gash */
   else if (v > 200)
   {
      new_aux = 6;
   }

   /* Severe cut */
   else if (v > 100)
   {
      new_aux = 5;
   }

   /* Nasty cut */
   else if (v > 50)
   {
      new_aux = 4;
   }

   /* Bad cut */
   else if (v > 25)
   {
      new_aux = 3;
   }

   /* Light cut */
   else if (v > 10)
   {
      new_aux = 2;
   }

   /* Graze */
   else if (v > 0)
   {
      new_aux = 1;
   }

   /* None */
   else
   {
      new_aux = 0;
   }

   /* Increase cut */
   if (new_aux > old_aux)
   {
      /* Describe the state */
      switch (new_aux)
      {
         /* Graze */
         case 1:
            msg_print("You have been given a graze.");
            break;

         /* Light cut */
         case 2:
            msg_print("You have been given a light cut.");
            break;

         /* Bad cut */
         case 3:
            msg_print("You have been given a bad cut.");
            break;

         /* Nasty cut */
         case 4:
            msg_print("You have been given a nasty cut.");
            break;

         /* Severe cut */
         case 5:
            msg_print("You have been given a severe cut.");
            break;

         /* Deep gash */
         case 6:
            msg_print("You have been given a deep gash.");
            break;

         /* Mortal wound */
         case 7:
            msg_print("You have been given a mortal wound.");
            break;
      }

      /* Notice */
      notice = TRUE;
   }

   /* Decrease cut */
   else if (new_aux < old_aux)
   {
      /* Describe the state */
      switch (new_aux)
      {
         /* None */
         case 0:
            msg_print("You are no longer bleeding.");
            if (disturb_other) disturb(0, 0);
            break;
      }

      /* Notice */
      notice = TRUE;
   }

   /* Use the value */
   p_ptr->cut = v;

   /* No change */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Redraw the "cut" */
   p_ptr->redraw1 |= (PR1_CUT);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

void digest_food(void)
{
   s16b food_amount;

   /* Basic digestion rate based on speed */
   food_amount = extract_energy[p_ptr->pspeed] * 2;

   /* running takes *a lot* of energy */
   if (p_ptr->movement > 4) food_amount *= (p_ptr->movement - 4);

   /* Regeneration takes more food */
   if (p_ptr->regenerate) food_amount += 30;

   /* Slow digestion takes less food */
   if (p_ptr->slow_digest) food_amount -= 10;

   /* lugging a lot of stuff around makes you eat more */
   if (p_ptr->total_weight > weight_limit())
      food_amount += ((p_ptr->total_weight - weight_limit()) / 10);

   if (p_ptr->food>PY_FOOD_MAX)
   {
      /* too much eating is bad for your health... */
      /* p_ptr->food can be from 3*PY_FOOD_MAX to 100 (starving) */
      /* PY_FOOD_MAX = 15000 in defines.h */
      /* so if p_ptr->food = (max) 45000, randint (40 - 30) -> 10% */
      /* if p_ptr->food = 15001 (just bloated) randint(40 - 0) -> 2.5% */
      if (randint(40-(p_ptr->food - PY_FOOD_MAX)/1000)==1)
      {
         s16b chance;
         chance = randint(10);

         if (chance==1)
         {
            msg_print("Your bowels play up and you throw up.");
            p_ptr->food = PY_FOOD_STARVE / 2;
         }
         else if (chance < 3)
         {
            msg_print("Your bowels play up.");
            take_hit((p_ptr->food-PY_FOOD_MAX)/2000+1, "bowel cramps");
         }
         else
         {
            msg_print("Your bowels give you cramp.");
            set_paralyzed(randint(5)+2);
         }
      }
   }

   /* slow moving characters with slow digestions can get below 0! */
   if (food_amount < 1) food_amount = 1;

   /* Digest some food */
   (void)set_food(p_ptr->food - food_amount);

   p_ptr->redraw1 |= PR1_HUNGER;
}

/*
 * Set "p_ptr->food", notice observable changes
 *
 * The "p_ptr->food" variable can get as large as 20000, allowing the
 * addition of the most "filling" item, Elvish Waybread, which adds
 * 7500 food units, without overflowing the 32767 maximum limit.
 *
 * Perhaps we should disturb the player with various messages,
 * especially messages about hunger status changes.  XXX XXX XXX
 *
 * Digestion of food is handled in "dungeon.c", in which, normally,
 * the player digests about 20 food units per 100 game turns, more
 * when "fast", more when "regenerating", less with "slow digestion",
 * but when the player is "gorged", he digests 100 food units per 10
 * game turns, or a full 1000 food units per 100 game turns.
 *
 * Note that the player's speed is reduced by 10 units while gorged,
 * so if the player eats a single food ration (5000 food units) when
 * full (15000 food units), he will be gorged for (5000/100)*10 = 500
 * game turns, or 500/(100/5) = 25 player turns (if nothing else is
 * affecting the player speed).
 */
bool set_food(s32b v)
{
   s16b old_aux, new_aux;

   bool notice = FALSE;

   /* Hack -- Force good values */
   if (v > (3*PY_FOOD_MAX)) v = 3 * PY_FOOD_MAX;
   if (v < 0) v = 0;

   /* Fainting / Starving */
   if (p_ptr->food < PY_FOOD_FAINT)
   {
      old_aux = 0;
   }

   /* Weak */
   else if (p_ptr->food < PY_FOOD_WEAK)
   {
      old_aux = 1;
   }

   /* Hungry */
   else if (p_ptr->food < PY_FOOD_ALERT)
   {
      old_aux = 2;
   }

   /* Normal */
   else if (p_ptr->food < PY_FOOD_FULL)
   {
      old_aux = 3;
   }

   /* Full */
   else if (p_ptr->food < PY_FOOD_MAX)
   {
      old_aux = 4;
   }

   /* Bloated */
   else if (p_ptr->food < 2*PY_FOOD_MAX)
   {
      old_aux = 5;
   }

   /* Gorged */
   else
   {
      old_aux = 6;
   }

   /* Fainting / Starving */
   if (v < PY_FOOD_FAINT)
   {
      new_aux = 0;
   }

   /* Weak */
   else if (v < PY_FOOD_WEAK)
   {
      new_aux = 1;
   }

   /* Hungry */
   else if (v < PY_FOOD_ALERT)
   {
      new_aux = 2;
   }

   /* Normal */
   else if (v < PY_FOOD_FULL)
   {
      new_aux = 3;
   }

   /* Full */
   else if (v < PY_FOOD_MAX)
   {
      new_aux = 4;
   }

   /* Bloated */
   else if (v < 2*PY_FOOD_MAX)
   {
      new_aux = 5;
   }

   /* Gorged */
   else
   {
      new_aux = 6;
   }

   /* Food increase */
   if (new_aux > old_aux)
   {
      /* Describe the state */
      switch (new_aux)
      {
         /* Weak */
         case 1:
            msg_print("You are still weak.");
            break;

         /* Hungry */
         case 2:
            msg_print("You are still hungry.");
            break;

         /* Normal */
         case 3:
            msg_print("You are no longer hungry.");
            break;

         /* Full */
         case 4:
            msg_print("You are full!");
            break;

         /* Bloated */
         case 5:
            msg_print("You have bloated yourself!");
            break;

         /* Gorged */
         case 6:
            msg_print("You have gorged yourself!");
            break;
      }

      /* Change */
      notice = TRUE;
   }

   /* Food decrease */
   else if (new_aux < old_aux)
   {
      /* Describe the state */
      switch (new_aux)
      {
         /* Fainting / Starving */
         case 0:
            msg_print("You are getting faint from hunger!");
            break;

         /* Weak */
         case 1:
            msg_print("You are getting weak from hunger!");
            break;

         /* Hungry */
         case 2:
            msg_print("You are getting hungry.");
            break;

         /* Normal */
         case 3:
            msg_print("You are no longer full.");
            break;

         /* Full */
         case 4:
            msg_print("You are no longer bloated.");
            break;
         /* Gorged */
         case 5:
            msg_print("You are no longer gorged.");
            break;
      }
      if ( (new_aux < 3) && (p_ptr->tactic > 4) )
      {
         msg_print("Perhaps you have been running too long?");
      }

      /* Change */
      notice = TRUE;
   }

   /* Use the value */
   p_ptr->food = v;

   /* Nothing to notice */
   if (!notice) return (FALSE);

   /* Disturb */
   if (disturb_other) disturb(0,0);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Redraw hunger */
   p_ptr->redraw1 |= (PR1_HUNGER);

   /* Handle stuff */
   handle_stuff();

   /* Result */
   return (TRUE);
}

/*
 * Advance experience levels and print experience
 */
void check_experience()
{
   s16b      i;

   /* Note current level */
   i = p_ptr->lev;

   /* Hack -- lower limit */
   if (p_ptr->exp < 0) p_ptr->exp = 0;

   /* Hack -- lower limit */
   if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;

   /* Hack -- upper limit */
   if (p_ptr->exp > PY_MAX_EXP) p_ptr->exp = PY_MAX_EXP;

   /* Hack -- upper limit */
   if (p_ptr->max_exp > PY_MAX_EXP) p_ptr->max_exp = PY_MAX_EXP;

   /* Hack -- maintain "max" experience */
   if (p_ptr->exp > p_ptr->max_exp) p_ptr->max_exp = p_ptr->exp;

   /* Redraw experience */
   p_ptr->redraw1 |= (PR1_EXP);

   /* Handle stuff */
   handle_stuff();

   /* Lose levels while possible */
   while ((p_ptr->lev > 1) &&
         (p_ptr->exp < (player_exp[p_ptr->lev-2] *
                    p_ptr->expfact / 100L)))
   {
      /* Lose a level */
      p_ptr->lev--;

      /* Update some stuff */
      p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

      /* Redraw some stuff */
      p_ptr->redraw1 |= (PR1_LEV | PR1_TITLE);

      /* Handle stuff */
      handle_stuff();
   }

   /* Gain levels while possible */
   while ((p_ptr->lev < PY_MAX_LEVEL) &&
         (p_ptr->exp >= (player_exp[p_ptr->lev-1] *
                     p_ptr->expfact / 100L)))
   {
      /* Gain a level */
      p_ptr->lev++;

      /* Save the highest level */
      if (p_ptr->lev > p_ptr->max_plv)
      {
         p_ptr->max_plv = p_ptr->lev;
         level_reached[p_ptr->lev] = turn;
      }

      /* Sound */
      sound(SOUND_LEVEL);

      /* Message */
      msg_format("Welcome to level %d.", p_ptr->lev);

      /* Update some stuff */
      p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

      /* Redraw some stuff */
      p_ptr->redraw1 |= (PR1_LEV | PR1_TITLE | PR1_EXP);

      /* Handle stuff */
      handle_stuff();
   }
}

/*
 * change arena status
 */
void set_arena_state(byte new_state)
{
   p_ptr->arena_state = new_state;
dlog(DEBUGARENA,"xtra2.c: set_arena_status: @ %d,%d status now %d\n", px, py, p_ptr->arena_state);
}

/*
 * Gain experience
 */
void gain_exp(s32b amount)
{
/* jk - killing inside the arena gains no experience */

   if (dungeon.level[sublevel][py][px].fdat & CAVE_AREN) return;

   /* Gain some experience */
   p_ptr->exp += amount;

   /* Slowly recover from experience drainage */
   if (p_ptr->exp < p_ptr->max_exp)
   {
      /* Gain max experience (10%) */
      p_ptr->max_exp += amount / 10;
   }

   /* Check Experience */
   check_experience();
}

/*
 * Lose experience
 */
void lose_exp(s32b amount)
{
   /* Never drop below zero experience */
   if (amount > p_ptr->exp) amount = p_ptr->exp;

   /* Lose some experience */
   p_ptr->exp -= amount;

   /* Check Experience */
   check_experience();
}

/*
 * Hack -- Return the "automatic coin type" of a monster race
 * Used to allocate proper treasure when "Creeping coins" die
 *
 * XXX XXX XXX Note the use of actual "monster names"
 */
s16b get_coin_type(monster_race *r_ptr)
{
   cptr name = (r_name + r_ptr->name);

   /* Analyze "coin" monsters */
   if (r_ptr->d_char == '$')
   {
      /* Look for textual clues */
      if (strstr(name, "copper")) return (2);
      if (strstr(name, "silver")) return (5);
      if (strstr(name, "gold")) return (10);
      if (strstr(name, "mithril")) return (16);
      if (strstr(name, "adamantite")) return (17);

      /* Look for textual clues */
      if (strstr(name, "Copper")) return (2);
      if (strstr(name, "Silver")) return (5);
      if (strstr(name, "Gold")) return (10);
      if (strstr(name, "Mithril")) return (16);
      if (strstr(name, "Adamantite")) return (17);
   }

   /* Assume nothing */
   return (0);
}

/*
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.
 *
 * Note that only the player can induce "monster_death()" on Uniques.
 * Thus (for now) all Quest monsters should be Uniques.
 *
 * Note that in a few, very rare, circumstances, killing Morgoth
 * may result in the Iron Crown of Morgoth crushing the Lead-Filled
 * Mace "Grond", since the Iron Crown is more important.
 */
void monster_death(project_who_type *who, s16b m_idx)
{
   s16b                i, j, x, y, nx, ny, k_idx;

   s16b                dump_item = 0;
   s16b                dump_gold = 0;

   s16b                number = 0;
   s16b                total = 0;
   s16b                break_number = 0;

   cave_cell_type           *c_ptr;

   monster_type        *m_ptr = &mn_list[m_idx];

   monster_race        *r_ptr = &r_info[m_ptr->r_idx];
   object_type         *i_ptr;
   object_type         corpse;

   bool                visible = (m_ptr->ml || (r_ptr->flags1 & RF1_UNIQUE));

   /* Get the location */
   x = m_ptr->fx;
   y = m_ptr->fy;

   if (dungeon.level[sublevel][y][x].fdat & CAVE_AREN)
   {
     if (arena_monsters_left())
         msg_print("Go on, kill the rest of them, the crowd calls to you");
     else
         msg_print("Congratulations - this is the last one.");
   }

   if (m_ptr->has_drop)
   {
      s16b is_idx = item_set_this_monster(m_idx);
      /* find the correct item_set belonging to this monster */
      item_set_type *is_ptr = &is_list[is_idx];
      number = items_in_set (is_idx);
      for (j = 0; j < number; j++)
      {
         i_ptr = &i_list[is_ptr->index[j]];
         /* more paranoia! */
         if (!i_ptr->k_idx)
         {
            continue;
         }
         dump_gold |= (i_ptr->tval == TV_GOLD);
         dump_item |= (i_ptr->tval != TV_GOLD);
         break_number = breakage_chance(i_ptr, FALSE);
         /* things from Farmer Maggot (the only level 0 unique) don't break */
         if ( (r_ptr->flags1 & RF1_UNIQUE) &&
              (r_ptr->level == 0)) break_number = 0;
         drop_near(i_ptr, break_number, x, y, drop_how(i_ptr), FALSE, FALSE);
         WIPE(&i_list[is_ptr->index[j]], object_type);
      }
      WIPE(&is_list[is_idx],item_set_type); /* item_set now unused */
      m_ptr->has_drop = FALSE;       /* this is paranoia!   */
   }

   /* did we kill a player ghost? */
   if (m_ptr->r_idx >= r_number)
   {
      for (i=0; i<MAX_GHOSTS; i++)
      {
         if (ghost_info[i].r_idx == m_ptr->r_idx)
            break;
      }
      if ((i < MAX_GHOSTS) && (randint(100)<GHOST_RETURN))
      {
         remove_ghost_file(i);
      }
   }

   /* Take note of any dropped treasure */
   if (visible && (dump_item || dump_gold))
   {
       /* Take notes on treasure */
       lore_treasure(m_idx, dump_item, dump_gold);
   }

   /* drop a corpse if the option is on, it's visible and chances are OK */
   if (create_corpses && visible && (randint(100)<r_info[m_ptr->r_idx].corpse_chance))
   {
      /* prepare to drop a corpse */
      k_idx = lookup_kind(TV_CORPSE, 0);
      invcopy(&corpse, k_idx);
      corpse.sval = m_ptr->r_idx;
      corpse.p1val = r_info[m_ptr->r_idx].corpse_nutrition;
      corpse.weight = r_info[m_ptr->r_idx].corpse_weight;
      /* after (turn + corpse_spoiling) turns the corpse disappears */
      corpse.xtra2 = r_info[m_ptr->r_idx].corpse_spoiling;
      corpse.log.mlevel = p_ptr->mdepth;
      corpse.log.slevel = p_ptr->sdepth;
      corpse.log.whose = 0;
      if (who->type == WHO_PLAYER) corpse.log.where = OBJ_FOUND_KILLED;
      else if (who->type == WHO_TRAPBYPLAYER) corpse.log.where = OBJ_FOUND_TRAPZAP;
      else if (who->type == WHO_TRAPBYMONSTER) corpse.log.where = OBJ_FOUND_TRAPZAP;
      else if (who->type == WHO_PLAYERASWIZARD) corpse.log.where = OBJ_FOUND_WIZKILL;
      else if (who->type == WHO_MONSTER)
      {
         corpse.log.where = OBJ_FOUND_MONKILL;
         corpse.log.whose = who->index_race;
      }

      if (!drop_near(&corpse, 0, x, y, 0, FALSE, FALSE))
      {
         /* this happens (unsuccessfull dropping) when it drops in LAVA for example */
         ;
      }
   }

   /* Only process "Quest Monsters" */
   if (!(r_ptr->flags1 & RF1_QUESTOR)) return;

   /* Hack -- Mark quests as complete */
   for (i = 0; i < MAX_Q_IDX; i++)
   {
      /* Hack -- note completed quests */
      if (q_list[i].level == r_ptr->level)
      {
         q_list[i].level = 0;
      }

      /* Count incomplete quests */
      if (q_list[i].level)
      {
         total++;
      }
   }

   /* Need some stairs */
   if (total)
   {
      nx = x;
      ny = y;
      /* Stagger around until we find a legal grid */
      while (!valid_grid_bold(x,y))
      {
         /* Pick a location */
         scatter(&nx, &ny, x, y, 1, 0);

         /* Stagger */
         x = nx;
         y = ny;
      }

      /* Delete any old object XXX XXX XXX */
      delete_object(x, y, -1);

      /* Explain the stairway */
      msg_print("A magical stairway appears...");

      /* Access the grid */
      c_ptr = &dungeon.level[sublevel][y][x];

      /* Create stairs down */
      (void)set_grid_type(x, y, DUNG_STAIR, DUNG_STAIR_DOWN, GRID_KEEP, 0);

      /* Note the spot */
      note_spot(x, y);

      /* Draw the spot */
      lite_spot(x, y);

      /* Remember to update everything */
      p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS);
   }

   /* Nothing left, game over... */
   else
   {
      /* Total winner */
      total_winner = TRUE;

      /* Redraw the "title" */
      p_ptr->redraw1 |= (PR1_TITLE);

      /* Congratulations */
      msg_print("*** CONGRATULATIONS ***");
      msg_print("You have won the game!");
      msg_print("You may retire (commit suicide) when you are ready.");
   }
}

/* add the monster to the list of last kills */
static void add_monster_to_last_kills(s16b r_idx)
{
   s16b i;
   if (last_kills[24].x == r_idx)
   {
      last_kills[24].y++;
      return;
   }

   for (i=1; i <= 24; i++)
   {
      last_kills[i-1] = last_kills[i];
   }
   last_kills[24].x = r_idx;
   last_kills[24].y = 1;
}

/*
 * Decreases monsters hit points, handling monster death.
 *
 * We return TRUE if the monster has been killed (and deleted).
 *
 * We announce monster death (using an optional "death message"
 * if given, and a otherwise a generic killed/destroyed message).
 *
 * Only "physical attacks" can induce the "You have slain" message.
 * Missile and Spell attacks will induce the "dies" message, or
 * various "specialized" messages.  Note that "You have destroyed"
 * and "is destroyed" are synonyms for "You have slain" and "dies".
 *
 * Hack -- unseen monsters yield "You have killed it." message.
 *
 * Added fear (DGK) and check whether to print fear messages -CWS
 *
 * Genericized name, sex, and capitilization -BEN-
 *
 * As always, the "ghost" processing is a total hack.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * XXX XXX XXX Consider decreasing monster experience over time, say,
 * by using "(m_exp * m_lev * (m_lev)) / (p_lev * (m_lev + n_killed))"
 * instead of simply "(m_exp * m_lev) / (p_lev)", to make the first
 * monster worth more than subsequent monsters.  This would also need
 * to induce changes in the monster recall code.
 *
 * who = 0 means player
 * who = -1 means trap
 * who > 0 reserved for other monsters...
 */
bool mon_take_hit(project_who_type *who, s16b m_idx, s16b dam, bool *fear, cptr note)
{
   monster_type        *m_ptr = &mn_list[m_idx];

   monster_race        *r_ptr = &r_info[m_ptr->r_idx];

   s32b                new_exp, new_exp_frac;

   /* Redraw (later) if needed */
   if (health_who == m_idx) p_ptr->redraw1 |= (PR1_HEALTH);

   /* Wake it up */
   m_ptr->csleep = 0;

   /* we were attacked now */
   m_ptr->attacked = 0;

   /* Hurt it */
   m_ptr->hp -= dam;

   /* It is dead now */
   if (m_ptr->hp < 0)
   {
      char m_name[80];

      /* Extract monster name */
      monster_desc(m_name, m_ptr, 0);

      /* Make a sound */
      sound(SOUND_KILL);

      /* Death by Missile/Spell attack */
      if (note)
      {
         msg_format("%^s%s", m_name, note);
      }

      /* Death */
      else if (!m_ptr->ml)
      {
         if (who->type == WHO_PLAYER)
         {
            msg_format("You have killed %s.", m_name);
         }
         else
         {
            msg_format("%^s is killed.", m_name);
         }
      }

      /* Destroying non-physical monster */
      else if ((r_ptr->flags3 & RF3_DEMON) ||
               (r_ptr->flags3 & RF3_UNDEAD) ||
               (r_ptr->flags2 & RF2_STUPID) ||
               (strchr("Evg", r_ptr->d_char)))
      {
         if (who->type == WHO_PLAYER)
         {
            msg_format("You have destroyed %s.", m_name);
         }
         else
         {
            msg_format("%^s is destroyed.", m_name);
         }
      }

      /* Death by Physical attack -- living monster */
      else
      {
         if (who->type == WHO_PLAYER)
         {
            msg_format("You have slain %s.", m_name);
         }
         else
         {
            msg_format("%^s is slain.", m_name);
         }
      }

      /* Give some experience */
      new_exp = ((long)r_ptr->mexp * r_ptr->level) / p_ptr->lev;
      new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % p_ptr->lev)
                      * 0x10000L / p_ptr->lev) + p_ptr->exp_frac;

      /* Keep track of experience */
      if (new_exp_frac >= 0x10000L)
      {
         new_exp++;
         p_ptr->exp_frac = new_exp_frac - 0x10000L;
      }
      else
      {
         p_ptr->exp_frac = new_exp_frac;
      }

      /* Gain experience */
      gain_exp(new_exp);

      /* Generate treasure */

      monster_death(who, m_idx);

      /* When the player kills a Unique, it stays dead */
      if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 0;

      /* Recall even invisible uniques or winners */
      if (m_ptr->ml || (r_ptr->flags1 & RF1_UNIQUE))
      {
         /* Count kills this life */
         if (r_ptr->r_pkills < MAX_SHORT) r_ptr->r_pkills++;

         /* Count kills in all lives */
         if (r_ptr->r_tkills < MAX_SHORT) r_ptr->r_tkills++;
         if (r_ptr->first_kill == 0) r_ptr->first_kill = turn;
         /* Hack -- Auto-recall */
         monster_race_track(m_ptr->r_idx);
      }
      add_monster_to_last_kills(m_ptr->r_idx);
      p_ptr->window |= (PW_LASTKILL);

      /* Delete the monster */
dlog(DEBUGMONST,"xtra2.c: mon_take_hit: m_idx %d (%s) just got killed @ %d,%d\n",
                m_idx, r_name+r_ptr->name, m_ptr->fx, m_ptr->fy);
      delete_monster_idx(m_idx);

      /* Not afraid */
      (*fear) = FALSE;

      /* Monster is dead */
      return (TRUE);
   }

#ifdef ALLOW_FEAR

   /* Mega-Hack -- Pain cancels fear */
   if (m_ptr->afraid && (dam > 0))
   {
      s16b tmp = randint(dam);

      /* Cure a little fear */
      if (tmp < m_ptr->afraid)
      {
         /* Reduce fear */
         m_ptr->afraid -= tmp;
      }

      /* Cure all the fear */
      else
      {
         /* Cure fear */
         m_ptr->afraid = 0;

         /* No more fear */
         (*fear) = FALSE;
      }
   }

   /* Sometimes a monster gets scared by damage */
/* jk - monsters in the arena don't get fear! */
   if (!m_ptr->afraid && !(r_ptr->flags3 & RF3_NO_FEAR) &&
       !(dungeon.level[sublevel][m_ptr->fy][m_ptr->fx].fdat & CAVE_AREN))
   {
      s16b             percentage;

      /* Percentage of fully healthy */
      percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

      /*
       * Run (sometimes) if at 10% or less of max hit points,
       * or (usually) when hit for half its current hit points
       */
      if (((percentage <= 10) && (rand_int(10) < percentage)) ||
          ((dam >= m_ptr->hp) && (rand_int(100) < 80)))
      {
         /* Hack -- note fear */
         (*fear) = TRUE;

         /* XXX XXX XXX Hack -- Add some timed fear */
         m_ptr->afraid = (randint(10) +
                           (((dam >= m_ptr->hp) && (percentage > 7)) ?
                            20 : ((11 - percentage) * 5)));
      }
   }

#endif

   /* Not dead yet */
   return (FALSE);
}

/*
 * Calculates current boundaries
 * Called below and from "do_cmd_locate()".
 */
void panel_bounds(void)
{
   if (!smooth_scroll_panels)
   {
      panel_min_col = p_ptr->wx * (SCREEN_WID / 2);
      panel_max_col = panel_min_col + SCREEN_WID - 1;
      panel_prt_col = panel_min_col - 13;
      panel_min_row = p_ptr->wy * (SCREEN_HGT / 2);
      panel_max_row = panel_min_row + SCREEN_HGT - 1;
      panel_prt_row = panel_min_row - 1;
   }
   else
   {
      /* we take a window, half to the left of px, half to the right */
      panel_min_col = px - (SCREEN_WID / 2);
      panel_max_col = panel_min_col + SCREEN_WID - 1;

      panel_min_row = py - (SCREEN_HGT / 2);
      panel_max_row = panel_min_row + SCREEN_HGT  - 1;
   }

   /* if the window gets out of bounds, adapt it - left */
   if (panel_min_col < 0)
   {
      panel_min_col = 0;
      panel_max_col = SCREEN_WID-1;
   }
   /* idem right */
   if (panel_max_col >= cur_wid)
   {
      panel_max_col = cur_wid-1;
      panel_min_col = panel_max_col - SCREEN_WID + 1;
   }

   /* keep the left 13 columns for info */
   panel_prt_col = panel_min_col - 13;

   if (panel_min_row < MAP_ROW)
   {
      panel_min_row = MAP_ROW;
      panel_max_row = panel_min_row + SCREEN_HGT - 1;
   }
   if (panel_max_row >= cur_hgt)
   {
      panel_max_row = cur_hgt - 1;
      panel_min_row = panel_max_row - SCREEN_HGT +1;
   }
   /* this will break of MAP_ROW is any other row that 0, take care! */
   panel_prt_row = panel_min_row - 1;
}

/*
 * Given an row (y) and col (x), this routine detects when a move
 * off the screen has occurred and figures new borders. -RAK-
 *
 * "Update" forces a "full update" to take place.
 *
 * The map is reprinted if necessary, and "TRUE" is returned.
 */
void verify_panel(void)
{

   s16b x = px;
   s16b y = py;

   s16b pcol = p_ptr->wx;
   s16b prow = p_ptr->wy;

   /* only update the panels once in a while */
   if (!smooth_scroll_panels)
   {
      /* Scroll screen when 2 grids from top/bottom edge */
      if ((y < panel_min_row + 2) || (y > panel_max_row - 2))
      {
         prow = ((y - SCREEN_HGT / 4) / (SCREEN_HGT / 2));
         if (prow > panel_max_rows)
         {
            prow = panel_max_rows;
         }
         else
         {
            if (prow < 0) prow = 0;
         }
      }

      /* Scroll screen when 4 grids from left/right edge */
      if ((x < panel_min_col + 4) || (x > panel_max_col - 4))
      {
         pcol = ((x - SCREEN_WID / 4) / (SCREEN_WID / 2));

         if (pcol > panel_max_cols)
         {
            pcol = panel_max_cols;
         }
         else
         {
            if (pcol < 0) pcol = 0;
         }
      }

      /* Check for "no change" */
      if ((prow == p_ptr->wy) && (pcol == p_ptr->wx)) return;

      /* Hack -- optional disturb on "panel change" */
      if (disturb_panel) disturb(0, 0);
      /* Save the new panel info */
      p_ptr->wy = prow;
      p_ptr->wx = pcol;
   }

   /* Recalculate the boundaries */
   panel_bounds();

   /* Update stuff */
   p_ptr->update |= (PU_MONSTERS);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);
}

/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort_aux(vptr u, vptr v, s16b p, s16b q)
{
   s16b z, a, b;

   /* Done sort */
   if (p >= q) return;

   /* Pivot */
   z = p;

   /* Begin */
   a = p;
   b = q;

   /* Partition */
   while (TRUE)
   {
      /* Slide i2 */
      while (!(*ang_sort_comp)(u, v, b, z)) b--;

      /* Slide i1 */
      while (!(*ang_sort_comp)(u, v, z, a)) a++;

      /* Done partition */
      if (a >= b) break;

      /* Swap */
      (*ang_sort_swap)(u, v, a, b);

      /* Advance */
      a++, b--;
   }

   /* Recurse left side */
   ang_sort_aux(u, v, p, b);

   /* Recurse right side */
   ang_sort_aux(u, v, b+1, q);
}

/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort(vptr u, vptr v, s16b n)
{
   /* Sort the array */
   ang_sort_aux(u, v, 0, n-1);
}

/*
 * Sorting hook -- comp function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by double-distance to the player.
 */
bool ang_sort_comp_distance(vptr u, vptr v, s16b a, s16b b)
{
/* jk - these were bytes */
   s16b *x = (s16b*)(u);
   s16b *y = (s16b*)(v);

   s16b da, db, kx, ky;

   /* Absolute distance components */
   kx = x[a]; kx -= px; kx = ABS(kx);
   ky = y[a]; ky -= py; ky = ABS(ky);

   /* Approximate Double Distance to the first point */
   da = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

   /* Absolute distance components */
   kx = x[b]; kx -= px; kx = ABS(kx);
   ky = y[b]; ky -= py; ky = ABS(ky);

   /* Approximate Double Distance to the first point */
   db = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

   /* Compare the distances */
   return (da <= db);
}

/*
 * Sorting hook -- swap function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by distance to the player.
 */
void ang_sort_swap_distance(vptr u, vptr v, s16b a, s16b b)
{
/* jk - these were bytes */
   s16b *x = (s16b*)(u);
   s16b *y = (s16b*)(v);

   s16b temp;

   /* Swap "x" */
   temp = x[a];
   x[a] = x[b];
   x[b] = temp;

   /* Swap "y" */
   temp = y[a];
   y[a] = y[b];
   y[b] = temp;
}

/*** Targetting Code ***/

/*
 * Determine is a monster makes a reasonable target
 *
 * The concept of "targetting" was stolen from "Morgul" (?)
 *
 * The player can target any location, or any "target-able" monster.
 *
 * Currently, a monster is "target_able" if it is visible, and if
 * the player can hit it with a projection, and the player is not
 * hallucinating.  This allows use of "use closest target" macros.
 *
 * Future versions may restrict the ability to target "trappers"
 * and "mimics", but the semantics is a little bit weird.
 */
bool target_able(s16b m_idx)
{
   monster_type *m_ptr = &mn_list[m_idx];
   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   /* Monster must be visible */
   if (!m_ptr->ml) return (FALSE);

   /* Monster must be projectable */
   if (!projectable(px, py, m_ptr->fx, m_ptr->fy)) return (FALSE);

   /* Hack -- no targeting hallucinations */
   if (p_ptr->image) return (FALSE);

   /* XXX XXX XXX Hack -- Never target trappers */
   if ((r_ptr->flags1 & RF1_ATTR_CLEAR) && (r_ptr->flags1 & RF1_CHAR_CLEAR)) return (FALSE);

   /* Assume okay */
   return (TRUE);
}

/*
 * Update (if necessary) and verify (if possible) the target.
 *
 * We return TRUE if the target is "okay" and FALSE otherwise.
 */
bool target_okay()
{
   /* Accept stationary targets */
   if (target_who < 0) return (TRUE);

   /* Check moving targets */
   if (target_who > 0)
   {
      /* Accept reasonable targets */
      if (target_able(target_who))
      {
         monster_type *m_ptr = &mn_list[target_who];

         /* Acquire monster location */
         target_row = m_ptr->fy;
         target_col = m_ptr->fx;

         /* Good target */
         return (TRUE);
      }
   }

   /* Assume no target */
   return (FALSE);
}

/*
 * Hack -- help "select" a location (see below)
 */
s16b target_pick(s16b x1, s16b y1, s16b dx, s16b dy)
{
   s16b i, v;

   s16b x2, y2, x3, y3, x4, y4;

   s16b b_i = -1, b_v = 9999;

   /* Scan the locations */
   for (i = 0; i < temp_n; i++)
   {
      /* Point 2 */
      x2 = temp_x[i];
      y2 = temp_y[i];

      /* Directed distance */
      x3 = (x2 - x1);
      y3 = (y2 - y1);

      /* Verify quadrant */
      if (dx && (x3 * dx <= 0)) continue;
      if (dy && (y3 * dy <= 0)) continue;

      /* Absolute distance */
      x4 = ABS(x3);
      y4 = ABS(y3);

      /* Verify quadrant */
      if (dy && !dx && (x4 > y4)) continue;
      if (dx && !dy && (y4 > x4)) continue;

      /* Approximate Double Distance */
      v = ((x4 > y4) ? (x4 + x4 + y4) : (y4 + y4 + x4));

      /* XXX XXX XXX Penalize location */

      /* Track best */
      if ((b_i >= 0) && (v >= b_v)) continue;

      /* Track best */
      b_i = i; b_v = v;
   }

   /* Result */
   return (b_i);
}


/*
 * Set a new target.  This code can be called from "get_aim_dir()"
 *
 * The target must be on the current panel.  Consider the use of
 * "panel_bounds()" to allow "off-panel" targets, perhaps by using
 * some form of "scrolling" the map around the cursor.   XXX XXX XXX
 *
 * That is, consider the possibility of "auto-scrolling" the screen
 * while the cursor moves around.  This may require changes in the
 * "update_mon()" code to allow "visibility" even if off panel.
 *
 * Hack -- targetting an "outer border grid" may be dangerous,
 * so this is not currently allowed.
 *
 * You can now use the direction keys to move among legal monsters,
 * just like the new "look" function allows the use of direction
 * keys to move amongst interesting locations.
 */
bool target_set()
{
   s16b          i, d, m;
   s16b          x = px;
   s16b          y = py;

   bool          done = FALSE;
   bool          flag = TRUE;
   char          query;
   char          out_val[160];

   cave_cell_type    *c_ptr;
   monster_type *m_ptr;
   monster_race *r_ptr;

   /* Go ahead and turn off target mode */
   target_who = 0;

   /* Turn off health tracking */
   health_track(0);

   /* Reset "temp" array */
   temp_n = 0;

   /* Collect "target-able" monsters */
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];

      /* Skip "dead" monsters */
      if (!m_ptr->r_idx) continue;

      /* Ignore "unreasonable" monsters */
      if (!target_able(i)) continue;

      /* Save this monster index */
      temp_x[temp_n] = m_ptr->fx;
      temp_y[temp_n] = m_ptr->fy;
      temp_n++;
   }

   /* Set the sort hooks */
   ang_sort_comp = ang_sort_comp_distance;
   ang_sort_swap = ang_sort_swap_distance;

   /* Sort the positions */
   ang_sort(temp_x, temp_y, temp_n);

   /* Start near the player */
   m = 0;

   /* Interact */
   while (!done)
   {
      /* Target monsters */
      if (flag && temp_n)
      {
         x = temp_x[m];
         y = temp_y[m];

         c_ptr = &dungeon.level[sublevel][y][x];

         m_ptr = &mn_list[c_ptr->m_idx];
         r_ptr = &r_info[m_ptr->r_idx];

         /* Hack -- Track that monster race */
         monster_race_track(m_ptr->r_idx);

         /* Hack -- Track that monster */
         health_track(c_ptr->m_idx);

         /* Hack -- handle stuff */
         handle_stuff();

         /* Describe, prompt for recall */
         sprintf(out_val,
               "%s [(t)arget, (o)ffset, (p)osition, (r)ecall, or (q)uit]",
               (r_name + r_ptr->name));
         prt(out_val, 0, MESSAGE_ROW);

         /* Get a command */
         move_cursor_relative(x,y);
         query = inkey();

         /* Optional recall */
         while (query == 'r')
         {
            /* Recall on screen */
            Term_save();
            screen_roff(m_ptr->r_idx);
            Term_addstr(-1, TERM_WHITE, "  --pause--");
            query = inkey();
            Term_load();

            /* Hack -- ask again */
            if (query == ' ')
            {
               /* Get a new command */
               move_cursor_relative(x,y);
               query = inkey();
            }
         }

         /* Hack -- cancel tracking */
         health_track(0);

         /* Assume no "direction" */
         d = 0;

         /* Analyze (non "recall") command */
         switch (query)
         {
            case ESCAPE:
            case 'q':
               done = TRUE;
               break;

            case 't':
            case '.':
            case '5':
            case '0':
               health_track(c_ptr->m_idx);
               target_who = c_ptr->m_idx;
               target_row = y;
               target_col = x;
               done = TRUE;
               break;

            case '*':
            case ' ':
               if (++m == temp_n) m = 0;
               break;

            case '-':
               if (m-- == 0) m = temp_n - 1;
               break;

            case 'p':
               y = py;
               x = px;

            case 'o':
               flag = !flag;
               break;

            case 'm':
               break;

            case '1': case 'b': d = 1; break;
            case '2': case 'j': d = 2; break;
            case '3': case 'n': d = 3; break;
            case '4': case 'h': d = 4; break;
            case '6': case 'l': d = 6; break;
            case '7': case 'y': d = 7; break;
            case '8': case 'k': d = 8; break;
            case '9': case 'u': d = 9; break;

            default:
               bell("Unrecognized key");
         }

         /* Hack -- move around */
         if (d)
         {
            /* Find a new monster */
            i = target_pick(temp_x[m], temp_y[m], ddx[d], ddy[d]);

            /* Use that monster */
            if (i >= 0) m = i;
         }
      }

      /* Target locations */
      else
      {
         /* Now try a location */
         prt("Use cursor to designate target. [(t)arget]", 0, MESSAGE_ROW);

         /* Light up the current location */
         move_cursor_relative(x, y);

#if (debuglevel & DEBUGLOS)
{
   cave_cell_type *c2_ptr = &dungeon.level[sublevel][y][x];
   message_add(format("target: px,py %d,%d x,y %d,%d fdat %08lx mtyp %d styp %d i_idx %d m_idx %d t_idx %d extra %ld",
              px, py, x, y, c2_ptr->fdat, c2_ptr->mtyp, c2_ptr->styp, c2_ptr->i_idx,
              c2_ptr->m_idx, c2_ptr->t_idx, c2_ptr->extra));
   dlog(DEBUGLOS,"target: px,py %d,%d x,y %d,%d fdat %08lx mtyp %d styp %d i_idx %d m_idx %d t_idx %d extra %ld\n",
              px, py, x, y, c2_ptr->fdat, c2_ptr->mtyp, c2_ptr->styp, c2_ptr->i_idx,
              c2_ptr->m_idx, c2_ptr->t_idx, c2_ptr->extra);
}
#endif

         /* Get a command, and convert it to standard form */
         query = inkey();

         /* Assume no direction */
         d = 0;

         /* Analyze the keypress */
         switch (query)
         {
            case ESCAPE:
            case 'q':
               done = TRUE;
               break;

            case '5':
            case '.':
            case 't':
            case '0':
               target_who = -1;
               target_row = y;
               target_col = x;
               done = TRUE;
               break;

            case 'm':
               flag = !flag;
               break;

            case 'p':
               y = py;
               x = px;

            case 'o':
               break;

            case '1': case 'b': d = 1; break;
            case '2': case 'j': d = 2; break;
            case '3': case 'n': d = 3; break;
            case '4': case 'h': d = 4; break;
            case '6': case 'l': d = 6; break;
            case '7': case 'y': d = 7; break;
            case '8': case 'k': d = 8; break;
            case '9': case 'u': d = 9; break;

            default:
               bell("Unrecognized key");
         }

         /* Handle "direction" */
         if (d) x += ddx[d];
         if (d) y += ddy[d];

         /* Hack -- Verify x */
         if ((x>=cur_wid-1) || (x>panel_max_col)) x--;
         else if ((x<=0) || (x<panel_min_col)) x++;

         /* Hack -- Verify y */
         if ((y>=cur_hgt-1) || (y>panel_max_row)) y--;
         else if ((y<=0) || (y<panel_min_row)) y++;
      }
   }

   /* Forget */
   temp_n = 0;

   /* Clear the top line */
   prt("", 0, MESSAGE_ROW);

   /* Failure */
   if (!target_who) return (FALSE);

   /* Success */
   return (TRUE);
}

/*
 * Set a new target. Choose the closest monster
 */
bool target_set_closest()
{
   s16b          i, m;
   s16b          x = px;
   s16b          y = py;

   cave_cell_type    *c_ptr;
   monster_type *m_ptr;
   monster_race *r_ptr;

   /* Go ahead and turn off target mode */
   target_who = 0;

   /* Turn off health tracking */
   health_track(0);

   /* Reset "temp" array */
   temp_n = 0;

   /* Collect "target-able" monsters */
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];

      /* Skip "dead" monsters */
      if (!m_ptr->r_idx) continue;

      /* Ignore "unreasonable" monsters */
      if (!target_able(i)) continue;

      /* Save this monster index */
      temp_x[temp_n] = m_ptr->fx;
      temp_y[temp_n] = m_ptr->fy;
      temp_n++;
   }

   /* we didn't find any monsters */
   if (!temp_n)
   {
      msg_print("No targettable monsters found!");
      return (FALSE);
   }

   /* Set the sort hooks */
   ang_sort_comp = ang_sort_comp_distance;
   ang_sort_swap = ang_sort_swap_distance;

   /* Sort the positions */
   ang_sort(temp_x, temp_y, temp_n);

   /* Start near the player */
   m = 0;

   x = temp_x[m];
   y = temp_y[m];

   c_ptr = &dungeon.level[sublevel][y][x];

   m_ptr = &mn_list[c_ptr->m_idx];
   r_ptr = &r_info[m_ptr->r_idx];

   /* Hack -- Track that monster race */
   monster_race_track(m_ptr->r_idx);

   /* Hack -- Track that monster */
   health_track(c_ptr->m_idx);

   /* Hack -- handle stuff */
   handle_stuff();

   target_who = c_ptr->m_idx;
   target_row = y;
   target_col = x;

   /* Forget */
   temp_n = 0;

   /* Success */
   return (TRUE);
}

/*
 * Extract a direction (or zero) from a character
 */
s16b target_dir(char ch)
{
   s16b dir;

   int mode;

   cptr act, s;
   /* Default direction */
   dir = (isdigit((int)ch) ? D2I(ch) : 0);

   /* Roguelike */
   if (rogue_like_commands)
   {
      mode = KEYMAP_MODE_ROGUE;
   }

   /* Original */
   else
   {
      mode = KEYMAP_MODE_ORIG;
   }

   /* Extract the action (if any) */
   act = keymap_act[mode][(byte)(ch)];

   /* Analyze */
   if (act)
   {
      /* Convert to a direction */
      for (s = act; *s; ++s)
      {
         /* Use any digits in keymap */
         if (isdigit((int)*s)) dir = D2I(*s);
      }
   }

   /* Paranoia */
   if (dir == 5) dir = 0;

   /* Return direction */
   return (dir);
}

/*
 * Get an "aiming direction" from the user.
 *
 * The "dir" is loaded with 1,2,3,4,6,7,8,9 for "actual direction", and
 * "0" for "current target", and "-1" for "entry aborted".
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 *
 * Note that confusion over-rides any (explicit?) user choice.
 */
bool get_aim_dir(s16b *dp)
{
   char                command;
   cptr                p;
   s16b                dir, i, temp_n;
   cave_cell_type     *c_ptr;
   monster_type       *m_ptr;
   monster_race       *r_ptr;

   /* Global direction */
   dir = p_ptr->command_dir;

   /* Hack -- auto-target */
   if (use_old_target && target_okay()) dir = 5;

/* jk - if there is only 1 monster targettable & auto_target_only_monster */
/*      is set, target that monster */
   if (auto_target_only_monster)
   {
      temp_n=0;
      for (i = 1; i < mn_max; i++)
      {
         monster_type *m_ptr = &mn_list[i];
         /* Skip "dead" monsters */
         if (!m_ptr->r_idx) continue;
         /* Ignore "unreasonable" monsters */
         if (!target_able(i)) continue;
         /* Save this monster index */
         temp_x[temp_n] = m_ptr->fx;
         temp_y[temp_n] = m_ptr->fy;
         temp_n++;
      }
      if (temp_n==1)
      {
         c_ptr = &dungeon.level[sublevel][temp_y[0]][temp_x[0]];
         m_ptr = &mn_list[c_ptr->m_idx];
         r_ptr = &r_info[m_ptr->r_idx];
         /* Hack -- Track that monster race */
         monster_race_track(m_ptr->r_idx);
         /* Hack -- Track that monster */
         health_track(c_ptr->m_idx);
         /* Hack -- handle stuff */
         handle_stuff();
         target_who = c_ptr->m_idx;
         target_row = temp_y[0];
         target_col = temp_x[0];
         dir=5;
      }
   }


   /* Ask until satisfied */
   while (!dir)
   {
      /* Choose a prompt */
      if (!target_okay())
      {
          p = "Direction ('*' to choose target,'c' for closest target, Escape to cancel)? ";
      }
      else
      {
          p = "Direction ('5' for target, '*' to re-target, Escape to cancel)? ";
      }

      /* Get a command (or Cancel) */
      if (!get_com(p, &command)) break;

      /* Convert various keys to "standard" keys */
      switch (command)
      {
         /* Various directions */
         case 'B': case 'b': case '1': dir = 1; break;
         case 'J': case 'j': case '2': dir = 2; break;
         case 'N': case 'n': case '3': dir = 3; break;
         case 'H': case 'h': case '4': dir = 4; break;
         case 'L': case 'l': case '6': dir = 6; break;
         case 'Y': case 'y': case '7': dir = 7; break;
         case 'K': case 'k': case '8': dir = 8; break;
         case 'U': case 'u': case '9': dir = 9; break;

         /* Use current target */
         case 'T': case 't': case '.': case '5': dir = 5; break;

         /* Set new target */
         case '*': if (target_set()) dir = 5; break;
         case 'c':
         case 'C': if (target_set_closest()) dir = 5; break;
      }

      /* Verify requested targets */
      if ((dir == 5) && !target_okay()) dir = 0;

      /* Error */
      if (!dir) bell("Unrecognized key");
   }

   /* Save the direction */
   *dp = dir;

   /* No direction */
   if (!dir) return (FALSE);

   /* Save the direction */
   p_ptr->command_dir = dir;

   /* Check for confusion */
   if (p_ptr->confused)
   {
      /* Warn the user */
      msg_print("You are confused.");

      /* Hack -- Random direction */
      *dp = ddd[rand_int(8)];
   }

   /* A "valid" direction was entered */
   return (TRUE);
}



/*
 * Request a "movement" direction (1,2,3,4,6,7,8,9) from the user,
 * and place it into "p_ptr->command_dir", unless we already have one.
 *
 * This function should be used for all "repeatable" commands, such as
 * run, walk, open, close, bash, disarm, spike, tunnel, etc.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", to which "confusion" is applied.
 */
bool get_rep_dir(s16b *dp)
{
   s16b dir;


   /* Global direction */
   dir = p_ptr->command_dir;

   /* Get a direction */
   while (!dir)
   {
      char ch;

      /* Get a command (or Cancel) */
      if (!get_com("Direction (Escape to cancel)? ", &ch)) break;

      /* Convert various keys to "standard" keys */
      switch (ch)
      {
         /* Convert roguelike directions */
         case 'B': case 'b': case '1': dir = 1; break;
         case 'J': case 'j': case '2': dir = 2; break;
         case 'N': case 'n': case '3': dir = 3; break;
         case 'H': case 'h': case '4': dir = 4; break;
         case 'L': case 'l': case '6': dir = 6; break;
         case 'Y': case 'y': case '7': dir = 7; break;
         case 'K': case 'k': case '8': dir = 8; break;
         case 'U': case 'u': case '9': dir = 9; break;

         /* Error */
         default: bell("Unrecognized key");
      }
   }

   /* Keep the given direction */
   *dp = dir;

   /* Aborted */
   if (!dir) return (FALSE);

   /* Save the direction */
   p_ptr->command_dir = dir;

   /* Apply "confusion" */
   if (p_ptr->confused)
   {
      /* Warn the user XXX XXX XXX */
      /* msg_print("You are confused."); */

      /* Standard confusion */
      if (rand_int(100) < 75)
      {
         /* Random direction */
         *dp = ddd[rand_int(8)];
      }
   }

   /* A "valid" direction was entered */
   return (TRUE);
}




