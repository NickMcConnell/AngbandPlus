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
        me.weight = 430;
        me.options = CASTER_ALLOW_DEC_MANA;
        init = TRUE;
    }
    return &me;
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
        
        me.caster_info = _caster_info;
        /*TODO: me.get_spells = _get_spells;*/
        me.get_powers = _get_powers;
        init = TRUE;
    }

    return &me;
}
