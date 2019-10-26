#include "angband.h"

cptr do_music_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
    bool fail = (mode == SPELL_FAIL) ? TRUE : FALSE;
    bool cont = (mode == SPELL_CONT) ? TRUE : FALSE;
    bool stop = (mode == SPELL_STOP) ? TRUE : FALSE;

    int dir;
    int plev = p_ptr->lev;

    switch (spell)
    {
    case 0:
        if (name) return "Song of Holding";
        if (desc) return "Attempts to slow all monsters in sight.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You start humming a slow, steady melody...");
            bard_start_singing(spell, MUSIC_SLOW);
        }

        {
            int power = plev;

            if (info) return info_power(power);

            if (cont)
            {
                slow_monsters(power);
            }
        }
        break;

    case 1:
        if (name) return "Song of Blessing";
        if (desc) return "Gives bonus to hit and AC for a few turns.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("The holy power of the Music of the Ainur enters you...");
            plr_tim_lock(T_BLESSED);
            bard_start_singing(spell, MUSIC_BLESS);
        }

        if (stop)
            plr_tim_unlock(T_BLESSED);

        break;

    case 2:
        if (name) return "Wrecking Note";
        if (desc) return "Fires a bolt of sound.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        {
            int dice = 4 + (plev - 1) / 5;
            int sides = 4;

            if (info) return info_damage(spell_power(dice), sides, spell_power(p_ptr->to_d_spell));

            if (cast)
            {
                if (!get_fire_dir(&dir)) return NULL;

                fire_bolt(
                    GF_SOUND,
                    dir,
                    spell_power(damroll(dice, sides) + p_ptr->to_d_spell)
                );
            }
        }
        break;

    case 3:
        if (name) return "Stun Pattern";
        if (desc) return "Attempts to stun all monsters in sight.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You weave a pattern of sounds to bewilder and daze...");
            bard_start_singing(spell, MUSIC_STUN);
        }

        {
            int dice = spell_power(plev / 10);
            int sides = 2;

            if (info) return info_power_dice(dice, sides);

            if (cont)
            {
                stun_monsters(damroll(dice, sides));
            }
        }

        break;

    case 4:
        if (name) return "Flow of Life";
        if (desc) return "Heals HP a little.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("Life flows through you as you sing a song of healing...");
            bard_start_singing(spell, MUSIC_L_LIFE);
        }

        {
            int dice = 2;
            int sides = spell_power(6);

            if (info) return info_heal(dice, sides, 0);

            if (cont)
            {
                hp_player(damroll(dice, sides));
            }
        }

        break;

    case 5:
        if (name) return "Song of the Sun";
        if (desc) return "Lights up nearby area and the inside of a room permanently.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        {
            int dice = 2;
            int sides = plev / 2;
            int rad = plev / 10 + 1;

            if (info) return info_damage(dice, sides, 0);

            if (cast)
            {
                msg_print("Your uplifting song brings brightness to dark places...");

                lite_area(damroll(dice, sides), rad);
            }
        }
        break;

    case 6:
        if (name) return "Song of Fear";
        if (desc) return "Attempts to scare all monsters in sight.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You start weaving a fearful pattern...");
            bard_start_singing(spell, MUSIC_FEAR);
        }

        {
            int power = spell_power(plev);

            if (info) return info_power(power);

            if (cont)
            {
                project_los(GF_TURN_ALL, power);
            }
        }

        break;

    case 7:
        if (name) return "Heroic Ballad";
        if (desc) return "Removes fear, and gives bonus to hit and 10 more HP for a while.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You start singing a song of intense fighting...");

            (void)hp_player(10);
            fear_clear_p();

            plr_tim_lock(T_HERO);
            bard_start_singing(spell, MUSIC_HERO);
        }

        if (stop) plr_tim_unlock(T_HERO);
        break;

    case 8:
        if (name) return "Clairaudience";
        if (desc) return "Detects traps, doors and stairs in your vicinity. And detects all monsters at level 15, treasures and items at level 20. Maps nearby area at level 25. Lights and know the whole level at level 40. These effects occurs by turns while this song continues.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("Your quiet music sharpens your sense of hearing...");

            /* Hack -- Initialize the turn count */
            p_ptr->magic_num1[2] = 0;

            bard_start_singing(spell, MUSIC_DETECT);
        }

        {
            int rad = DETECT_RAD_DEFAULT;

            if (info) return info_radius(rad);

            if (cont)
            {
                int count = p_ptr->magic_num1[2];

                if (count >= 19) wiz_lite();
                if (count >= 11)
                {
                    map_area(rad);
                    if (plev > 39 && count < 19)
                        p_ptr->magic_num1[2] = count + 1;
                }
                if (count >= 6)
                {
                    /* There are too many hidden treasure. So... */
                    /* detect_treasure(rad); */
                    detect_objects_gold(rad);
                    detect_objects_normal(rad);

                    if (plev > 24 && count < 11)
                        p_ptr->magic_num1[2] = count + 1;
                }
                if (count >= 3)
                {
                    detect_monsters_invis(rad);
                    detect_monsters_normal(rad);

                    if (plev > 19 && count < 6)
                        p_ptr->magic_num1[2] = count + 1;
                }
                detect_traps(rad, TRUE);
                detect_doors(rad);
                detect_stairs(rad);
                detect_recall(rad);

                if (plev > 14 && count < 3)
                    p_ptr->magic_num1[2] = count + 1;
            }
        }

        break;

    case 9:
        if (name) return "Soul Shriek";
        if (desc) return "Damages all monsters in sight with PSI damages.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You start singing a song of soul in pain...");
            bard_start_singing(spell, MUSIC_PSI);
        }

        {
            int dice = 1;
            int sides = plev * 3 / 2;

            if (info) return info_damage(dice, spell_power(sides), spell_power(p_ptr->to_d_spell));

            if (cont)
            {
                project_los(
                    GF_PSI,
                    spell_power(damroll(dice, sides) + p_ptr->to_d_spell)
                );
            }
        }

        break;

    case 10:
        if (name) return "Song of Lore";
        if (desc) return "Identifies all items which are in the adjacent squares.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You recall the rich lore of the world...");
            bard_start_singing(spell, MUSIC_ID);
        }

        {
            int rad = 1;

            if (info) return info_radius(rad);

            if (cont || cast)
            {
                project(0, rad, p_ptr->pos.y, p_ptr->pos.x, 0, GF_IDENTIFY, PROJECT_ITEM);
            }
        }

        break;

    case 11:
        if (name) return "Hiding Tune";
        if (desc) return "Gives improved stealth.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("Your song carries you beyond the sight of mortal eyes...");
            bard_start_singing(spell, MUSIC_STEALTH);
        }

        if (stop)
        {
            msg_print("You are no longer hidden.");
        }

        break;

    case 12:
        if (name) return "Illusion Pattern";
        if (desc) return "Attempts to confuse all monsters in sight.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You weave a pattern of sounds to beguile and confuse...");
            bard_start_singing(spell, MUSIC_CONF);
        }

        {
            int power = plev * 2;

            if (info) return info_power(power);

            if (cont)
            {
                confuse_monsters(power);
            }
        }

        break;

    case 13:
        if (name) return "Doomcall";
        if (desc) return "Damages all monsters in sight with booming sound.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("The fury of the Downfall of Numenor lashes out...");
            bard_start_singing(spell, MUSIC_SOUND);
        }

        {
            int dice = 10 + plev / 5;
            int sides = 7;

            if (info) return info_damage(spell_power(dice), sides, spell_power(p_ptr->to_d_spell));

            if (cont)
            {
                project_los(
                    GF_SOUND,
                    spell_power(damroll(dice, sides) + p_ptr->to_d_spell)
                );
            }
        }

        break;

    case 14:
        if (name) return "Firiel's Song";
        if (desc) return "Resurrects nearby corpse and skeletons. And makes these your pets.";

        {
            /* Stop singing before start another */
            if (cast || fail) bard_stop_singing();

            if (cast)
            {
                msg_print("The themes of life and revival are woven into your song...");

                animate_dead(0, p_ptr->pos.y, p_ptr->pos.x);
            }
        }
        break;

    case 15:
        if (name) return "Fellowship Chant";
        if (desc) return "Attempts to charm all monsters in sight.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You weave a slow, soothing melody of imploration...");
            bard_start_singing(spell, MUSIC_CHARM);
        }

        {
            int dice = spell_power(10 + plev / 15);
            int sides = 6;

            if (info) return info_power_dice(dice, sides);

            if (cont)
            {
                charm_monsters(damroll(dice, sides));
            }
        }

        break;

    case 16:
        if (name) return "Sound of disintegration";
        if (desc) return "Makes you be able to burrow into walls. Objects under your feet evaporate.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You weave a violent pattern of sounds to break wall.");
            bard_start_singing(spell, MUSIC_WALL);
        }

        {
            if (cont || cast)
            {
                project(0, 0, p_ptr->pos.y, p_ptr->pos.x,
                    0, GF_DISINTEGRATE, PROJECT_KILL | PROJECT_ITEM | PROJECT_HIDE);
            }
        }
        break;

    case 17:
        if (name) return "Finrod's Resistance";
        if (desc) return "Gives resistance to fire, cold, electricity, acid and poison.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You sing a song of perseverance against powers...");
            plr_tim_lock(T_RES_ACID);
            plr_tim_lock(T_RES_ELEC);
            plr_tim_lock(T_RES_FIRE);
            plr_tim_lock(T_RES_COLD);
            plr_tim_lock(T_RES_POIS);
            bard_start_singing(spell, MUSIC_RESIST);
        }

        if (stop)
        {
            plr_tim_unlock(T_RES_ACID);
            plr_tim_unlock(T_RES_ELEC);
            plr_tim_unlock(T_RES_FIRE);
            plr_tim_unlock(T_RES_COLD);
            plr_tim_unlock(T_RES_POIS);
        }

        break;

    case 18:
        if (name) return "Hobbit Melodies";
        if (desc) return "Hastes you.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You start singing joyful pop song...");
            plr_tim_lock(T_FAST);
            bard_start_singing(spell, MUSIC_SPEED);
        }

        if (stop)
            plr_tim_unlock(T_FAST);

        break;

    case 19:
        if (name) return "World Contortion";
        if (desc) return "Teleports all nearby monsters away unless resisted.";

        {
            int rad = spell_power(plev / 15 + 1);
            int power = spell_power(plev * 3 + 1);

            if (info) return info_radius(rad);

            /* Stop singing before start another */
            if (cast || fail) bard_stop_singing();

            if (cast)
            {
                msg_print("Reality whirls wildly as you sing a dizzying melody...");

                project(0, rad, p_ptr->pos.y, p_ptr->pos.x, power, GF_AWAY_ALL, PROJECT_KILL);
            }
        }
        break;

    case 20:
        if (name) return "Dispelling chant";
        if (desc) return "Damages all monsters in sight. Hurts evil monsters greatly.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You cry out in an ear-wracking voice...");
            bard_start_singing(spell, MUSIC_DISPEL);
        }

        {
            int m_sides = plev * 3;
            int e_sides = plev * 3;

            if (info) return info_damage(1, spell_power(m_sides), spell_power(p_ptr->to_d_spell));

            if (cont)
            {
                dispel_monsters(spell_power(randint1(m_sides) + p_ptr->to_d_spell));
                dispel_evil(spell_power(randint1(e_sides) + p_ptr->to_d_spell));
            }
        }
        break;

    case 21:
        if (name) return "The Voice of Saruman";
        if (desc) return "Attempts to slow and sleep all monsters in sight.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You start humming a gentle and attractive song...");
            bard_start_singing(spell, MUSIC_SARUMAN);
        }

        {
            int power = spell_power(plev);

            if (info) return info_power(power);

            if (cont)
            {
                slow_monsters(power);
                sleep_monsters(power);
            }
        }

        break;

    case 22:
        if (name) return "Song of the Tempest";
        if (desc) return "Fires a beam of sound.";

        {
            int dice = 15 + (plev - 1) / 2;
            int sides = 10;

            if (info) return info_damage(spell_power(dice), sides, spell_power(p_ptr->to_d_spell));

            /* Stop singing before start another */
            if (cast || fail) bard_stop_singing();

            if (cast)
            {
                if (!get_fire_dir(&dir)) return NULL;

                fire_beam(
                    GF_SOUND,
                    dir,
                    spell_power(damroll(dice, sides) + p_ptr->to_d_spell)
                );
            }
        }
        break;

    case 23:
        if (name) return "Ambarkanta";
        if (desc) return "Recreates current dungeon level.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You sing of the primeval shaping of Middle-earth...");

            alter_reality();
        }
        break;

    case 24:
        if (name) return "Wrecking Pattern";
        if (desc) return "Shakes dungeon structure, and results in random swapping of floors and walls.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You weave a pattern of sounds to contort and shatter...");
            bard_start_singing(spell, MUSIC_QUAKE);
        }

        {
            int rad = 10;

            if (info) return info_radius(rad);

            if (cont)
            {
                earthquake(p_ptr->pos, 10);
            }
        }

        break;


    case 25:
        if (name) return "Stationary Shriek";
        if (desc) return "Attempts to freeze all monsters in sight.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You weave a very slow pattern which is almost likely to stop...");
            bard_start_singing(spell, MUSIC_STASIS);
        }

        {
            int power = spell_power(plev * 4);

            if (info) return info_power(power);

            if (cont)
            {
                stasis_monsters(power);
            }
        }

        break;

    case 26:
        if (name) return "Endurance";
        if (desc) return "Sets a glyph on the floor beneath you. Monsters cannot attack you if you are on a glyph, but can try to break glyph.";

        {
            /* Stop singing before start another */
            if (cast || fail) bard_stop_singing();

            if (cast)
            {
                msg_print("The holy power of the Music is creating sacred field...");

                warding_glyph();
            }
        }
        break;

    case 27:
        if (name) return "The Hero's Poem";
        if (desc) return "Hastes you. Gives heroism. Damages all monsters in sight.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You chant a powerful, heroic call to arms...");
            (void)hp_player(10);

            plr_tim_lock(T_FAST);
            plr_tim_lock(T_HERO);
            bard_start_singing(spell, MUSIC_SHERO);
        }

        if (stop)
        {
            plr_tim_unlock(T_FAST);
            plr_tim_unlock(T_HERO);
        }

        {
            int dice = 1;
            int sides = plev * 3;

            if (info) return info_damage(dice, sides, spell_power(p_ptr->to_d_spell));

            if (cont)
            {
                dispel_monsters(spell_power(damroll(dice, sides) + p_ptr->to_d_spell));
            }
        }
        break;

    case 28:
        if (name) return "Relief of Yavanna";
        if (desc) return "Powerful healing song. Also heals cut and stun completely.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("Life flows through you as you sing the song...");
            bard_start_singing(spell, MUSIC_H_LIFE);
        }

        {
            int dice = spell_power(15);
            int sides = 10;

            if (info) return info_heal(dice, sides, 0);

            if (cont)
            {
                hp_player(damroll(dice, sides));
                plr_tim_remove(T_STUN);
                plr_tim_remove(T_CUT);
            }
        }

        break;

    case 29:
        if (name) return "Goddess' rebirth";
        if (desc) return "Restores all stats and experience.";

        {
            /* Stop singing before start another */
            if (cast || fail) bard_stop_singing();

            if (cast)
            {
                msg_print("You strewed light and beauty in the dark as you sing. You feel refreshed.");
                (void)do_res_stat(A_STR);
                (void)do_res_stat(A_INT);
                (void)do_res_stat(A_WIS);
                (void)do_res_stat(A_DEX);
                (void)do_res_stat(A_CON);
                (void)do_res_stat(A_CHR);
                (void)restore_level();
                plr_restore_life(1000);
            }
        }
        break;

    case 30:
        if (name) return "Wizardry of Sauron";
        if (desc) return "Fires an extremely powerful tiny ball of sound.";

        {
            int dice = 50 + plev;
            int sides = 10;
            int rad = 0;

            if (info) return info_damage(spell_power(dice), sides, spell_power(p_ptr->to_d_spell));

            /* Stop singing before start another */
            if (cast || fail) bard_stop_singing();

            if (cast)
            {
                if (!get_fire_dir(&dir)) return NULL;

                fire_ball(
                    GF_SOUND,
                    dir,
                    spell_power(damroll(dice, sides) + p_ptr->to_d_spell),
                    rad
                );
            }
        }
        break;

    case 31:
        if (name) return "Fingolfin's Challenge";
        if (desc) return "Generates barrier which completely protect you from almost all damages. Takes a few your turns when the barrier breaks.";

        /* Stop singing before start another */
        if (cast || fail) bard_stop_singing();

        if (cast)
        {
            msg_print("You recall the valor of Fingolfin's challenge to the Dark Lord...");
            plr_tim_lock(T_INVULN);
            bard_start_singing(spell, MUSIC_INVULN);
        }

        if (stop) plr_tim_unlock(T_INVULN);

        break;
    }

    return "";
}
void bard_check_music(void)
{
    magic_type *s_ptr;
    int spell;
    s32b need_mana;
    u32b need_mana_frac;

    if (p_ptr->pclass != CLASS_BARD) return;
    if (!p_ptr->magic_num1[0] && !p_ptr->magic_num1[1]) return;

    if (p_ptr->anti_magic)
    {
        bard_stop_singing();
        return;
    }

    spell = p_ptr->magic_num2[0];
    s_ptr = &technic_info[REALM_MUSIC - MIN_TECHNIC][spell];

    need_mana = mod_need_mana(s_ptr->smana, spell, REALM_MUSIC);
    need_mana_frac = 0;

    /* Divide by 2 */
    s64b_RSHIFT(need_mana, need_mana_frac, 1);

    if (s64b_cmp(p_ptr->csp, p_ptr->csp_frac, need_mana, need_mana_frac) < 0)
    {
        bard_stop_singing();
        return;
    }
    else
    {
        s64b_sub(&(p_ptr->csp), &(p_ptr->csp_frac), need_mana, need_mana_frac);

        p_ptr->redraw |= PR_MANA;
        if (p_ptr->magic_num1[1])
        {
            p_ptr->magic_num1[0] = p_ptr->magic_num1[1];
            p_ptr->magic_num1[1] = 0;
            msg_print("You restart singing.");
            p_ptr->action = ACTION_SING;

            p_ptr->update |= (PU_BONUS | PU_HP);
            p_ptr->redraw |= (PR_MAP | PR_STATUS | PR_STATE);
            p_ptr->update |= (PU_MONSTERS);
            p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
        }
    }
    if (p_ptr->spell_exp[spell] < SPELL_EXP_BEGINNER)
        p_ptr->spell_exp[spell] += 5;
    else if(p_ptr->spell_exp[spell] < SPELL_EXP_SKILLED)
    { if (one_in_(2) && (cave->dun_lvl > 4) && ((cave->dun_lvl + 10) > p_ptr->lev)) p_ptr->spell_exp[spell] += 1; }
    else if(p_ptr->spell_exp[spell] < SPELL_EXP_EXPERT)
    { if (one_in_(5) && ((cave->dun_lvl + 5) > p_ptr->lev) && ((cave->dun_lvl + 5) > s_ptr->slevel)) p_ptr->spell_exp[spell] += 1; }
    else if(p_ptr->spell_exp[spell] < SPELL_EXP_MASTER)
    { if (one_in_(5) && ((cave->dun_lvl + 5) > p_ptr->lev) && (cave->dun_lvl > s_ptr->slevel)) p_ptr->spell_exp[spell] += 1; }

    /* Do any effects of continual song */
    do_spell(REALM_MUSIC, spell, SPELL_CONT);
}

void bard_start_singing(int spell, int song)
{
    if (p_ptr->pclass != CLASS_BARD) return;

    /* Remember the song index */
    p_ptr->magic_num1[0] = song;

    /* Remember the index of the spell which activated the song */
    p_ptr->magic_num2[0] = spell;

    set_action(ACTION_SING);
    p_ptr->update |= (PU_BONUS);
    p_ptr->redraw |= (PR_STATUS);
}

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
    p_ptr->update |= (PU_BONUS);
    p_ptr->redraw |= (PR_STATUS);
}

static void _stop_singing_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stop Singing");
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



static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = _stop_singing_spell;

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "song";
        me.which_stat = A_CHR;
        me.encumbrance.max_wgt = 400;
        me.encumbrance.weapon_pct = 67;
        me.encumbrance.enc_wgt = 800;
        me.realm1_choices = CH_MUSIC;
        init = TRUE;
    }
    return &me;
}

static void _calc_bonuses(void)
{
    res_add(RES_SOUND);
    if (equip_find_art(ART_DAERON) || equip_find_art(ART_MAGLOR))
    {
        p_ptr->dec_mana++;
        p_ptr->easy_spell++;
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_SOUND);
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_SHORT_SWORD, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_spellbooks();
}

plr_class_ptr bard_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  33,  34,  -5,  16,  20,  34,  20};
    skills_t xs = {  8,  13,  11,   0,   0,   0,  10,   8};

        me = plr_class_alloc(CLASS_BARD);
        me->name = "Bard";
        me->desc = "Bards are something like traditional musicians. Their magical "
                    "attacks are sound-based, and last as long as the Bard has mana. "
                    "Although a bard cannot sing two or more songs at the same time, he "
                    "or she does have the advantage that many songs affect all areas in "
                    "sight. A bard's prime statistic is charisma.\n \n"
                    "The songs are found in four songbooks, of which two are sold in "
                    "town. There is a special feature of music; many songs continue to "
                    "be sung until either the Bard chooses to stop, or he runs out of "
                    "the energy required to sing that type of song. Bards have a class "
                    "power - 'Stop Singing'.";

        me->stats[A_STR] = -2;
        me->stats[A_INT] =  1;
        me->stats[A_WIS] =  2;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] =  4;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 140;
        me->pets = 25;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_WEAK |
                   CLASS_SENSE2_MED | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;        
        me->hooks.caster_info = _caster_info;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = spellbook_character_dump;
    }

    return me;
}
