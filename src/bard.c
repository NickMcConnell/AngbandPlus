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

    int rad = DETECT_RAD_DEFAULT;
    dice_t dice = {0};

    dice.scale = spell_power(1000);

    /* Stop singing before starting another ... every spell needs this. */
    if (cast || fail) bard_stop_singing();

    switch (spell)
    {
    case 0:
        if (name) return "Song of Holding";
        if (desc) return "Attempts to slow all monsters in sight.";
        if (cast) {
            msg_print("You start humming a slow, steady melody...");
            bard_start_singing(spell, MUSIC_SLOW);
        }
        dice.base = plr->lev;
        if (info) return dice_info_power(dice);
        if (cont) plr_project_los(GF_SLOW, dice_roll(dice));
        break;
    case 1:
        if (name) return "Song of Blessing";
        if (desc) return "Gives bonus to hit and AC for a few turns.";
        if (cast) {
            msg_print("The holy power of the Music of the Ainur enters you...");
            plr_tim_lock(T_BLESSED);
            bard_start_singing(spell, MUSIC_BLESS);
        }
        if (stop) plr_tim_unlock(T_BLESSED);
        break;
    case 2:
        if (name) return "Wrecking Note";
        if (desc) return "Fires a bolt of sound.";
        dice.dd = 4 + (plr->lev - 1)/5;
        dice.ds = 4;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt(GF_SOUND, dice)) return NULL;
        break;
    case 3:
        if (name) return "Stun Pattern";
        if (desc) return "Attempts to stun all monsters in sight.";
        if (cast) {
            msg_print("You weave a pattern of sounds to bewilder and daze...");
            bard_start_singing(spell, MUSIC_STUN);
        }
        dice.dd = plr->lev/10;
        dice.ds = 2;
        if (info) return dice_info_power(dice);
        if (cont) plr_project_los(GF_STUN, dice_roll(dice));
        break;
    case 4:
        if (name) return "Flow of Life";
        if (desc) return "Heals HP a little.";
        if (cast) {
            msg_print("Life flows through you as you sing a song of healing...");
            bard_start_singing(spell, MUSIC_L_LIFE);
        }
        dice.dd = 2;
        dice.ds = 6;
        if (info) return dice_info_heal(dice);
        if (cont) hp_player(dice_roll(dice));
        break;
    case 5:
        if (name) return "Song of the Sun";
        if (desc) return "Lights up nearby area and the inside of a room permanently.";
        dice.dd = 2;
        dice.ds = plr->lev/2;
        if (info) return dice_info_dam(dice);
        if (cast) {
            msg_print("Your uplifting song brings brightness to dark places...");
            lite_area(dice_roll(dice), 1 + plr->lev/10);
        }
        break;
    case 6:
        if (name) return "Song of Fear";
        if (desc) return "Attempts to scare all monsters in sight.";
        if (cast) {
            msg_print("You start weaving a fearful pattern...");
            bard_start_singing(spell, MUSIC_FEAR);
        }
        dice.base = plr->lev;
        if (info) return dice_info_power(dice);
        if (cont) plr_project_los(GF_FEAR, dice_roll(dice));
        break;
    case 7:
        if (name) return "Heroic Ballad";
        if (desc) return "Removes fear, and gives bonus to hit and 10 more HP for a while.";
        if (cast) {
            msg_print("You start singing a song of intense fighting...");
            hp_player(10);
            fear_clear_p();
            plr_tim_lock(T_HERO);
            bard_start_singing(spell, MUSIC_HERO);
        }
        if (stop) plr_tim_unlock(T_HERO);
        break;
    case 8:
        if (name) return "Clairaudience";
        if (desc) return "Detects traps, doors and stairs in your vicinity. And detects all monsters at level 15, treasures and items at level 20. Maps nearby area at level 25. Lights and know the whole level at level 40. These effects occurs by turns while this song continues.";
        if (cast) {
            msg_print("Your quiet music sharpens your sense of hearing...");
            /* Hack -- Initialize the turn count */
            plr->magic_num1[2] = 0;
            bard_start_singing(spell, MUSIC_DETECT);
        }
        rad = DETECT_RAD_DEFAULT;
        if (cont) {
            int count = plr->magic_num1[2];

            if (count >= 19) wiz_lite();
            if (count >= 11)
            {
                map_area(rad);
                if (plr->lev > 39 && count < 19)
                    plr->magic_num1[2] = count + 1;
            }
            if (count >= 6)
            {
                /* There are too many hidden treasure. So... */
                /* detect_treasure(rad); */
                detect_objects_gold(rad);
                detect_objects_normal(rad);
                if (plr->lev > 24 && count < 11)
                    plr->magic_num1[2] = count + 1;
            }
            if (count >= 3)
            {
                detect_monsters_invis(rad);
                detect_monsters_normal(rad);
                if (plr->lev > 19 && count < 6)
                    plr->magic_num1[2] = count + 1;
            }
            detect_traps(rad, TRUE);
            detect_doors(rad);
            detect_stairs(rad);
            detect_recall(rad);
            if (plr->lev > 14 && count < 3)
                plr->magic_num1[2] = count + 1;
        }
        break;
    case 9:
        if (name) return "Soul Shriek";
        if (desc) return "Damages all monsters in sight with PSI damages.";
        if (cast) {
            msg_print("You start singing a song of soul in pain...");
            bard_start_singing(spell, MUSIC_PSI);
        }
        dice.dd = 1;
        dice.ds = 3*plr->lev/2;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cont) plr_project_los(GF_PSI, dice_roll(dice));
        break;
    case 10:
        if (name) return "Song of Lore";
        if (desc) return "Identifies all items which are in the adjacent squares.";
        if (cast) {
            msg_print("You recall the rich lore of the world...");
            bard_start_singing(spell, MUSIC_ID);
        }
        if (cont || cast) plr_burst(1, GF_IDENTIFY, 0);
        break;
    case 11:
        if (name) return "Hiding Tune";
        if (desc) return "Gives improved stealth.";
        if (cast) {
            msg_print("Your song carries you beyond the sight of mortal eyes...");
            bard_start_singing(spell, MUSIC_STEALTH);
        }
        if (stop) msg_print("You are no longer hidden.");
        break;
    case 12:
        if (name) return "Illusion Pattern";
        if (desc) return "Attempts to confuse all monsters in sight.";
        if (cast) {
            msg_print("You weave a pattern of sounds to beguile and confuse...");
            bard_start_singing(spell, MUSIC_CONF);
        }
        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cont) plr_project_los(GF_OLD_CONF, dice_roll(dice));
        break;
    case 13:
        if (name) return "Doomcall";
        if (desc) return "Damages all monsters in sight with booming sound.";
        if (cast) {
            msg_print("The fury of the Downfall of Numenor lashes out...");
            bard_start_singing(spell, MUSIC_SOUND);
        }
        dice.dd = 10 + plr->lev/5;
        dice.ds = 7;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cont) plr_project_los(GF_SOUND, dice_roll(dice));
        break;
    case 14:
        if (name) return "Firiel's Song";
        if (desc) return "Resurrects nearby corpse and skeletons. And makes these your pets.";
        if (cast) {
            msg_print("The themes of life and revival are woven into your song...");
            plr_animate_dead();
        }
        break;
    case 15:
        if (name) return "Fellowship Chant";
        if (desc) return "Attempts to charm all monsters in sight.";
        if (cast) {
            msg_print("You weave a slow, soothing melody of imploration...");
            bard_start_singing(spell, MUSIC_CHARM);
        }
        dice.dd = 10 + plr->lev/15;
        dice.ds = 6;
        if (info) return dice_info_power(dice);
        if (cont) plr_project_los(GF_CHARM, dice_roll(dice));
        break;
    case 16:
        if (name) return "Sound of disintegration";
        if (desc) return "Makes you be able to burrow into walls. Objects under your feet evaporate.";
        if (cast) {
            msg_print("You weave a violent pattern of sounds to break wall.");
            bard_start_singing(spell, MUSIC_WALL);
        }
        if (cont || cast) {
            gf_affect_f(who_create_plr(), plr->pos, GF_DISINTEGRATE, 0, GF_AFFECT_SPELL);
            gf_affect_o(who_create_plr(), plr->pos, GF_DISINTEGRATE, 0, GF_AFFECT_SPELL);
        }
        break;
    case 17:
        if (name) return "Finrod's Resistance";
        if (desc) return "Gives resistance to fire, cold, electricity, acid and poison.";
        if (cast) {
            msg_print("You sing a song of perseverance against powers...");
            plr_tim_lock(T_RES_ACID);
            plr_tim_lock(T_RES_ELEC);
            plr_tim_lock(T_RES_FIRE);
            plr_tim_lock(T_RES_COLD);
            plr_tim_lock(T_RES_POIS);
            bard_start_singing(spell, MUSIC_RESIST);
        }
        if (stop) {
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
        if (cast) {
            msg_print("You start singing a joyful pop song...");
            plr_tim_lock(T_FAST);
            bard_start_singing(spell, MUSIC_SPEED);
        }
        if (stop) plr_tim_unlock(T_FAST);
        break;
    case 19:
        if (name) return "World Contortion";
        if (desc) return "Teleports all nearby monsters away unless resisted.";
        rad = 1 + plr->lev/15;
        dice.base = 3*plr->lev/2;
        if (info) return dice_info_power(dice);
        if (cast) {
            msg_print("Reality whirls wildly as you sing a dizzying melody...");
            plr_burst(rad, GF_TELEPORT, dice_roll(dice));
        }
        break;
    case 20:
        if (name) return "Dispelling chant";
        if (desc) return "Damages all monsters in sight. Hurts evil monsters greatly.";
        if (cast) {
            msg_print("You cry out in an ear-wracking voice...");
            bard_start_singing(spell, MUSIC_DISPEL);
        }
        dice.dd = 1;
        dice.ds = 3*plr->lev;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cont) {
            plr_project_los(GF_DISP_ALL, dice_roll(dice));
            plr_project_los(GF_DISP_EVIL, dice_roll(dice));
        }
        break;
    case 21:
        if (name) return "The Voice of Saruman";
        if (desc) return "Attempts to slow and sleep all monsters in sight.";
        if (cast) {
            msg_print("You start humming a gentle and attractive song...");
            bard_start_singing(spell, MUSIC_SARUMAN);
        }
        dice.base = plr->lev;
        if (info) return dice_info_power(dice);
        if (cont) {
            plr_project_los(GF_SLOW, dice_roll(dice));
            plr_project_los(GF_SLEEP, dice_roll(dice));
        }
        break;
    case 22:
        if (name) return "Song of the Tempest";
        if (desc) return "Fires a beam of sound.";
        dice.dd = 15 + (plr->lev - 1)/2;
        dice.ds = 10;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_beam(GF_SOUND, dice)) return NULL;
        break;
    case 23:
        if (name) return "Ambarkanta";
        if (desc) return "Recreates current dungeon level.";
        if (cast) {
            msg_print("You sing of the primeval shaping of Middle-earth...");
            alter_reality();
        }
        break;
    case 24:
        if (name) return "Wrecking Pattern";
        if (desc) return "Shakes dungeon structure, and results in random swapping of floors and walls.";
        if (cast) {
            msg_print("You weave a pattern of sounds to contort and shatter...");
            bard_start_singing(spell, MUSIC_QUAKE);
        }
        rad = 10;
        if (info) return info_radius(rad);
        if (cont) earthquake(plr->pos, rad);
        break;
    case 25:
        if (name) return "Stationary Shriek";
        if (desc) return "Attempts to freeze all monsters in sight.";
        if (cast) {
            msg_print("You weave a very slow pattern which is almost likely to stop...");
            bard_start_singing(spell, MUSIC_STASIS);
        }
        dice.base = 4*plr->lev;
        if (info) return dice_info_power(dice);
        if (cont) plr_project_los(GF_STASIS, dice_roll(dice));
        break;
    case 26:
        if (name) return "Endurance";
        if (desc) return "Sets a glyph on the floor beneath you. Monsters cannot attack you if you are on a glyph, but can try to break glyph.";
        if (cast) {
            msg_print("The holy power of the Music is creating a sacred field...");
            warding_glyph();
        }
        break;
    case 27:
        if (name) return "The Hero's Poem";
        if (desc) return "Hastes you. Gives heroism. Damages all monsters in sight.";
        if (cast) {
            msg_print("You chant a powerful, heroic call to arms...");
            hp_player(10);
            plr_tim_lock(T_FAST);
            plr_tim_lock(T_HERO);
            bard_start_singing(spell, MUSIC_SHERO);
        }
        if (stop) {
            plr_tim_unlock(T_FAST);
            plr_tim_unlock(T_HERO);
        }
        dice.dd = 1;
        dice.ds = 3*plr->lev;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cont) plr_project_los(GF_DISP_ALL, dice_roll(dice));
        break;
    case 28:
        if (name) return "Relief of Yavanna";
        if (desc) return "Powerful healing song. Also heals cut and stun completely.";
        if (cast) {
            msg_print("Life flows through you as you sing the song...");
            bard_start_singing(spell, MUSIC_H_LIFE);
        }
        dice.dd = 15;
        dice.ds = 10;
        if (info) return dice_info_heal(dice);
        if (cont) {
            hp_player(dice_roll(dice));
            plr_tim_remove(T_STUN);
            plr_tim_remove(T_CUT);
        }
        break;
    case 29:
        if (name) return "Goddess' rebirth";
        if (desc) return "Restores all stats and experience.";
        if (cast) {
            msg_print("You strewed light and beauty in the dark as you sing. You feel refreshed.");
            do_res_stat(A_STR);
            do_res_stat(A_INT);
            do_res_stat(A_WIS);
            do_res_stat(A_DEX);
            do_res_stat(A_CON);
            do_res_stat(A_CHR);
            restore_level();
            plr_restore_life(1000);
        }
        break;
    case 30:
        if (name) return "Wizardry of Sauron";
        if (desc) return "Fires an extremely powerful tiny ball of sound.";
        dice.dd = 50 + plr->lev;
        dice.ds = 10;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(0, GF_SOUND, dice)) return NULL;
        break;
    case 31:
        if (name) return "Fingolfin's Challenge";
        if (desc) return "Generates barrier which completely protect you from almost all damages. Takes a few your turns when the barrier breaks.";
        if (cast) {
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

    if (plr->pclass != CLASS_BARD) return;
    if (!plr->magic_num1[0] && !plr->magic_num1[1]) return;

    if (plr->anti_magic)
    {
        bard_stop_singing();
        return;
    }

    spell = plr->magic_num2[0];
    s_ptr = &technic_info[REALM_MUSIC - MIN_TECHNIC][spell];

    need_mana = mod_need_mana(s_ptr->smana, spell, REALM_MUSIC);
    need_mana_frac = 0;

    /* Divide by 2 */
    s64b_RSHIFT(need_mana, need_mana_frac, 1);

    if (s64b_cmp(plr->csp, plr->csp_frac, need_mana, need_mana_frac) < 0)
    {
        bard_stop_singing();
        return;
    }
    else
    {
        s64b_sub(&(plr->csp), &(plr->csp_frac), need_mana, need_mana_frac);

        plr->redraw |= PR_MANA;
        if (plr->magic_num1[1])
        {
            plr->magic_num1[0] = plr->magic_num1[1];
            plr->magic_num1[1] = 0;
            msg_print("You restart singing.");
            plr->action = ACTION_SING;

            plr->update |= (PU_BONUS | PU_HP);
            plr->redraw |= (PR_MAP | PR_STATUS | PR_STATE);
            plr->update |= (PU_MONSTERS);
            plr->window |= (PW_OVERHEAD | PW_DUNGEON);
        }
    }
    if (plr->spell_exp[spell] < SPELL_EXP_BEGINNER)
        plr->spell_exp[spell] += 5;
    else if(plr->spell_exp[spell] < SPELL_EXP_SKILLED)
    { if (one_in_(2) && (cave->dun_lvl > 4) && ((cave->dun_lvl + 10) > plr->lev)) plr->spell_exp[spell] += 1; }
    else if(plr->spell_exp[spell] < SPELL_EXP_EXPERT)
    { if (one_in_(5) && ((cave->dun_lvl + 5) > plr->lev) && ((cave->dun_lvl + 5) > s_ptr->slevel)) plr->spell_exp[spell] += 1; }
    else if(plr->spell_exp[spell] < SPELL_EXP_MASTER)
    { if (one_in_(5) && ((cave->dun_lvl + 5) > plr->lev) && (cave->dun_lvl > s_ptr->slevel)) plr->spell_exp[spell] += 1; }

    /* Do any effects of continual song */
    do_spell(REALM_MUSIC, spell, SPELL_CONT);
}

void bard_start_singing(int spell, int song)
{
    if (plr->pclass != CLASS_BARD) return;

    /* Remember the song index */
    plr->magic_num1[0] = song;

    /* Remember the index of the spell which activated the song */
    plr->magic_num2[0] = spell;

    set_action(ACTION_SING);
    plr->update |= (PU_BONUS);
    plr->redraw |= (PR_STATUS);
}

void bard_stop_singing(void)
{
    if (plr->pclass != CLASS_BARD) return;

     /* Are there interupted song? */
    if (plr->magic_num1[1])
    {
        /* Forget interupted song */
        plr->magic_num1[1] = 0;
        return;
    }

    /* The player is singing? */
    if (!plr->magic_num1[0]) return;

    /* Hack -- if called from set_action(), avoid recursive loop */
    if (plr->action == ACTION_SING) set_action(ACTION_NONE);

    /* Message text of each song or etc. */
    do_spell(REALM_MUSIC, plr->magic_num2[0], SPELL_STOP);

    plr->magic_num1[0] = MUSIC_NONE;
    plr->magic_num2[0] = 0;
    plr->update |= (PU_BONUS);
    plr->redraw |= (PR_STATUS);
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
        if (!plr->magic_num1[0] && !plr->magic_num1[1]) return;
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
    res_add(GF_SOUND);
    if (equip_find_art("}.Daeron") || equip_find_art("}.Maglor"))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_SOUND));
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
    skills_t xs = { 40,  65,  55,   0,   0,   0,  50,  40};

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
