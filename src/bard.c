#include "angband.h"

/*
 * Start singing if the player is a Bard 
 */
void bard_start_singing(int spell, int song)
{
	if (p_ptr->pclass != CLASS_BARD) return;

	/* Remember the song index */
	p_ptr->magic_num1[0] = song;

	/* Remember the index of the spell which activated the song */
	p_ptr->magic_num2[0] = spell;

	/* Now the player is singing */
	set_action(ACTION_SING);


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);
}


/*
 * Stop singing if the player is a Bard 
 */
void bard_stop_singing(void)
{
	if (p_ptr->pclass != CLASS_BARD) return;

 	/* Are there interupted song? */
	if (p_ptr->magic_num1[1])
	{
		/* Forget interupted song */
		p_ptr->magic_num1[1] = 0;
		return;
	}

	/* The player is singing? */
	if (!p_ptr->magic_num1[0]) return;

	/* Hack -- if called from set_action(), avoid recursive loop */
	if (p_ptr->action == ACTION_SING) set_action(ACTION_NONE);

	/* Message text of each song or etc. */
	do_spell(REALM_MUSIC, p_ptr->magic_num2[0], SPELL_STOP);

	p_ptr->magic_num1[0] = MUSIC_NONE;
	p_ptr->magic_num2[0] = 0;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);
}

void bard_stop_singing_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Stop Singing", "²Î¤ò»ß¤á¤ë"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!p_ptr->magic_num1[0] && !p_ptr->magic_num1[1]) return;
		bard_stop_singing();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}