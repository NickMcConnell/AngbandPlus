#include "angband.h"

/* TODO: Redo spells ... */

/* "Clean me up, Scotty!"
static void dump_aux_class_special(FILE *fff)
{
    if (p_ptr->pclass == CLASS_BLUE_MAGE)
    {
        int i = 0;
        int j = 0;
        int l1 = 0;
        int l2 = 0;
        int num = 0;
        int spellnum[MAX_MONSPELLS];
        s32b f4 = 0, f5 = 0, f6 = 0;
        char p[60][80];
        int col = 0;
        bool pcol = FALSE;

        for (i=0;i<60;i++) { p[i][0] = '\0'; }

        strcat(p[col], "\n\n  [Learned Blue Magic]\n");


        for (j=1;j<6;j++)
        {
            col++;
            set_rf_masks(&f4, &f5, &f6, j);
            switch(j)
            {
                case MONSPELL_TYPE_BOLT:
                    strcat(p[col], "\n     [Bolt  Type]\n");
                    break;

                case MONSPELL_TYPE_BALL:
                    strcat(p[col], "\n     [Ball  Type]\n");
                    break;

                case MONSPELL_TYPE_BREATH:
                    strcat(p[col], "\n     [  Breath  ]\n");
                    break;

                case MONSPELL_TYPE_SUMMON:
                    strcat(p[col], "\n     [Summonning]\n");
                    break;

                case MONSPELL_TYPE_OTHER:
                    strcat(p[col], "\n     [Other Type]\n");
                    break;
            }

            for (i = 0, num = 0; i < 32; i++)
            {
                if ((0x00000001 << i) & f4) spellnum[num++] = i;
            }
            for (; i < 64; i++)
            {
                if ((0x00000001 << (i - 32)) & f5) spellnum[num++] = i;
            }
            for (; i < 96; i++)
            {
                if ((0x00000001 << (i - 64)) & f6) spellnum[num++] = i;
            }

            col++;
            pcol = FALSE;
            strcat(p[col], "       ");

            for (i = 0; i < num; i++)
            {
                if (p_ptr->magic_num2[spellnum[i]])
                {
                    pcol = TRUE;
                    l1 = strlen(p[col]);
                    l2 = strlen(monster_powers_short[spellnum[i]]);
                    if ((l1 + l2) >= 75)
                    {
                        strcat(p[col], "\n");
                        col++;
                        strcat(p[col], "       ");
                    }
                    strcat(p[col], monster_powers_short[spellnum[i]]);
                    strcat(p[col], ", ");
                }
            }

            if (!pcol)
            {
                strcat(p[col], "None");
            }
            else
            {
                if (p[col][strlen(p[col])-2] == ',')
                {
                    p[col][strlen(p[col])-2] = '\0';
                }
                else
                {
                    p[col][strlen(p[col])-10] = '\0';
                }
            }

            strcat(p[col], "\n");
        }

        for (i=0;i<=col;i++)
        {
            fprintf(fff, "%s", p[i]);
        }
    }
}
*/

/* Spell masks for blue mage */
#define MS_NONE		0
#define MS_BOLT		1
#define MS_BALL		2
#define MS_BREATH	3
#define MS_SUMMON	4
#define MS_OTHER	5

byte blue_mage_mask[] = 
{
	MS_OTHER,	/* MS_SHRIEK */
	MS_NONE,	/* MS_XXX1 */
	MS_OTHER,	/* MS_DISPEL */
	MS_BALL,	/* MS_ROCKET */
	MS_BOLT,	/* MS_SHOOT */
	MS_NONE,	/* MS_XXX2 */
	MS_NONE,	/* MS_XXX3 */
	MS_BREATH,	/* MS_BR_STORM */
	MS_BREATH,	/* MS_BR_ACID */
	MS_BREATH,	/* MS_BR_ELEC */
	MS_BREATH,	/* MS_BR_FIRE */
	MS_BREATH,	/* MS_BR_COLD */
	MS_BREATH,	/* MS_BR_POIS */
	MS_BREATH,	/* MS_BR_NETHER */
	MS_BREATH,	/* MS_BR_LITE */
	MS_BREATH,	/* MS_BR_DARK */
	MS_BREATH,	/* MS_BR_CONF */
	MS_BREATH,	/* MS_BR_SOUND */
	MS_BREATH,	/* MS_BR_CHAOS */
	MS_BREATH,	/* MS_BR_DISEN */
	MS_BREATH,	/* MS_BR_NEXUS */
	MS_BREATH,	/* MS_BR_TIME */
	MS_BREATH,	/* MS_BR_INERTIA */
	MS_BREATH,	/* MS_BR_GRAVITY */
	MS_BREATH,	/* MS_BR_SHARDS */
	MS_BREATH,	/* MS_BR_PLASMA */
	MS_BREATH,	/* MS_BR_FORCE */
	MS_BREATH,	/* MS_BR_MANA */
	MS_BALL,	/* MS_BALL_NUKE */
	MS_BREATH,	/* MS_BR_NUKE */
	MS_BALL,	/* MS_BALL_CHAOS */
	MS_BREATH,	/* MS_BR_DISI */
	MS_BALL,	/* MS_BALL_ACID */
	MS_BALL,	/* MS_BALL_ELEC */
	MS_BALL,	/* MS_BALL_FIRE */
	MS_BALL,	/* MS_BALL_COLD */
	MS_BALL,	/* MS_BALL_POIS */
	MS_BALL,	/* MS_BALL_NETHER */
	MS_BALL,	/* MS_BALL_WATER */
	MS_BALL,	/* MS_BALL_MANA */
	MS_BALL,	/* MS_BALL_DARK */
	MS_OTHER,	/* MS_DRAIN_MANA */
	MS_OTHER,	/* MS_MIND_BLAST */
	MS_OTHER,	/* MS_BRAIN_SMASH */
	MS_OTHER,	/* MS_CAUSE_1 */
	MS_OTHER,	/* MS_CAUSE_2 */
	MS_OTHER,	/* MS_CAUSE_3 */
	MS_OTHER,	/* MS_CAUSE_4 */
	MS_BOLT,	/* MS_BOLT_ACID */
	MS_BOLT,	/* MS_BOLT_ELEC */
	MS_BOLT,	/* MS_BOLT_FIRE */
	MS_BOLT,	/* MS_BOLT_COLD */
	MS_BALL,	/* MS_STARBURST */
	MS_BOLT,	/* MS_BOLT_NETHER */
	MS_BOLT,	/* MS_BOLT_WATER */
	MS_BOLT,	/* MS_BOLT_MANA */
	MS_BOLT,	/* MS_BOLT_PLASMA */
	MS_BOLT,	/* MS_BOLT_ICE */
	MS_BOLT,	/* MS_MAGIC_MISSILE */
	MS_OTHER,	/* MS_SCARE */
	MS_OTHER,	/* MS_BLIND */
	MS_OTHER,	/* MS_CONF */
	MS_OTHER,	/* MS_SLOW */
	MS_OTHER,	/* MS_SLEEP */
	MS_NONE,	/* MS_SPEED */
	MS_OTHER,	/* MS_HAND_DOOM */
	MS_NONE,	/* MS_HEAL */
	MS_NONE,	/* MS_INVULNER */
	MS_NONE,	/* MS_BLINK */
	MS_NONE,	/* MS_TELEPORT */
	MS_NONE,	/* MS_WORLD */
	MS_NONE,	/* MS_SPECIAL */
	MS_OTHER,	/* MS_TELE_TO */
	MS_OTHER,	/* MS_TELE_AWAY */
	MS_OTHER,	/* MS_TELE_LEVEL */
	MS_BOLT,	/* MS_PSY_SPEAR */
	MS_OTHER,	/* MS_DARKNESS */
	MS_OTHER,	/* MS_MAKE_TRAP */
	MS_OTHER,	/* MS_FORGET */
	MS_NONE,	/* MS_RAISE_DEAD */
	MS_SUMMON,	/* MS_S_KIN */
	MS_SUMMON,	/* MS_S_CYBER */
	MS_SUMMON,	/* MS_S_MONSTER */
	MS_SUMMON,	/* MS_S_MONSTERS */
	MS_SUMMON,	/* MS_S_ANT */
	MS_SUMMON,	/* MS_S_SPIDER */
	MS_SUMMON,	/* MS_S_HOUND */
	MS_SUMMON,	/* MS_S_HYDRA */
	MS_SUMMON,	/* MS_S_ANGEL */
	MS_SUMMON,	/* MS_S_DEMON */
	MS_SUMMON,	/* MS_S_UNDEAD */
	MS_SUMMON,	/* MS_S_DRAGON */
	MS_SUMMON,	/* MS_S_HI_UNDEAD */
	MS_SUMMON,	/* MS_S_HI_DRAGON */
	MS_SUMMON,	/* MS_S_AMBERITE */
	MS_SUMMON,	/* MS_S_UNIQUE */
	MS_NONE	/* MS_THROW */
};

void _learning_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Learning");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        if (p_ptr->action == ACTION_LEARN)
            set_action(ACTION_NONE);
        else
            set_action(ACTION_LEARN);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        var_set_int(res, 0);
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
    spell->fn = _learning_spell;

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "power";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 430;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 600;
        me.options = CASTER_ALLOW_DEC_MANA;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    py_birth_obj_aux(TV_DAGGER, SV_DAGGER, 1);
    py_birth_obj_aux(TV_SOFT_ARMOR, SV_ROBE, 1);
    py_birth_obj_aux(TV_WAND, EFFECT_BOLT_MISSILE, 1);

    p_ptr->proficiency[PROF_DAGGER] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_SLING] = WEAPON_EXP_BEGINNER;

    p_ptr->proficiency_cap[PROF_DIGGER] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_BLUNT] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_POLEARM] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_SWORD] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_STAVE] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_AXE] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_DAGGER] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_BOW] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_CROSSBOW] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_SLING] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_MARTIAL_ARTS] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_DUAL_WIELDING] = WEAPON_EXP_UNSKILLED;
    p_ptr->proficiency_cap[PROF_RIDING] = RIDING_EXP_UNSKILLED;
}

class_t *blue_mage_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  40,  36,   3,  20,  16,  40,  25};
    skills_t xs = {  7,  15,  11,   0,   0,   0,   6,   7};

        me.name = "Blue-Mage";
        me.desc = "A Blue-Mage is a spell caster that must live by his wits, as he "
                    "cannot hope to simply hack his way through the dungeon like a "
                    "warrior. A major difference between the Mage and the Blue-Mage is "
                    "the method of learning spells: Blue-Mages may learn spells from "
                    "monsters. A Blue-Mage's prime statistic is Intelligence as this "
                    "determines his spell casting ability.\n \n"
                    "A Blue-Mage can learn and cast monster ranged attacks, spells, or "
                    "summons as their own spells; this technique is called Blue magic. "
                    "Unlike Imitators, Blue-Mages remember their spells permanently, "
                    "but they must get hit by a monster's spell while their class power "
                    "'Learning' is active to learn spells. Because of this "
                    "requirement, they do not learn spells, like healing, that affect "
                    "the monster itself.";

        me.stats[A_STR] = -4;
        me.stats[A_INT] =  4;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] = -2;
        me.stats[A_CHR] = -2;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 100;
        me.base_hp = 4;
        me.exp = 130;
        me.pets = 35;
        me.flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                   CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG;
        
        me.birth = _birth;
        me.caster_info = _caster_info;
        /*TODO: me.get_spells = _get_spells;*/
        me.get_powers = _get_powers;
        init = TRUE;
    }

    return &me;
}
