#include "angband.h"

bool spoiler_hack = FALSE;

#ifdef ALLOW_SPOILERS

typedef void(*_file_fn)(FILE*);
static void _text_file(cptr name, _file_fn fn)
{
    FILE    *fff = NULL;
    char    buf[1024];

    path_build(buf, sizeof(buf), ANGBAND_DIR_HELP, name);
    fff = my_fopen(buf, "w");

    if (!fff)
    {
        path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);
        fff = my_fopen(buf, "w");

        if (!fff)
        {
            prt("Failed!", 0, 0);
            (void)inkey();
            return;
        }
    }

    fn(fff);
    fprintf(fff, "\n\n[[[[s|  Automatically generated for PosChengband %d.%d.%d.\n",
            VER_MAJOR, VER_MINOR, VER_PATCH);

    my_fclose(fff);
    msg_format("Created %s", buf);
}

static void _wrap_text(FILE *fff, cptr text, int col, int width)
{
    char buf[5*1024];
    char fmt[128];
    cptr t;

    sprintf(fmt, "%%%ds%%s\n", col);
    
    roff_to_buf(text, width-col, buf, sizeof(buf));
    for (t = buf; t[0]; t += strlen(t) + 1)
        fprintf(fff, fmt, "", t);
}

static void _race_help(FILE *fff, int idx)
{
    race_t *race_ptr = get_race_t_aux(idx, 0);

    fprintf(fff, "***** <%s>\n", race_ptr->name);
    fprintf(fff, "[[[[r|  %s\n\n", race_ptr->name);
    _wrap_text(fff, race_ptr->desc, 2, 80);
    if (idx == RACE_DEMIGOD)
    {
        fprintf(fff, "\n\n");
        _wrap_text(fff, "See [a] for more details on demigod parentage.", 2, 80);
    }
    if (idx == RACE_DRACONIAN)
    {
        fprintf(fff, "\n\n");
        _wrap_text(fff, "See [b] for more details on draconians.", 2, 80);
    }
    if (idx == RACE_MON_RING)
    {
        fprintf(fff, "\n\n");
        _wrap_text(fff, "See [a] for more details on rings.", 2, 80);
    }
    if (idx == RACE_MON_DRAGON)
    {
        fprintf(fff, "\n\n");
        _wrap_text(fff, "See [b] for more details on dragons.", 2, 80);
    }
    fprintf(fff, "\n\n");
}

static void _races_help(FILE* fff)
{
    int i, r;

    fprintf(fff, "[[[[B|  The Races\n\n");
    for (i = 0; i < MAX_RACES; i++)
    {
        race_t *race_ptr = get_race_t_aux(i, 0);
        if (race_ptr->flags & RACE_IS_MONSTER) continue;
        /*if (race_ptr->flags & RACE_IS_DEPRECATED) continue;*/
        _race_help(fff, i);
    }

    fprintf(fff, "***** <Tables>\n");
    fprintf(fff, "[[[[y|  Table 1 - Race Statistic Bonus Table\n\n");

    for (i = 0, r = 0; i < MAX_RACES; i++)
    {
        race_t *race_ptr = get_race_t_aux(i, 0);
        if (race_ptr->flags & RACE_IS_MONSTER) continue;
        /*if (race_ptr->flags & RACE_IS_DEPRECATED) continue;*/

        if (r % 20 == 0)
            fprintf(fff, "[[[[r|                 STR  INT  WIS  DEX  CON  CHR  Life  BHP  Exp\n");    
        r++;

        fprintf(fff, "  [[[[w|%-14s| %+3d  %+3d  %+3d  %+3d  %+3d  %+3d  %3d%%  %+3d  %3d%%\n", 
            race_ptr->name,
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
            race_ptr->life, race_ptr->base_hp, race_ptr->exp
        );
    }
    fprintf(fff, "\n\n");

    fprintf(fff, "[[[[y|  Table 2 - Race Skill Bonus Table\n\n");
    for (i = 0, r = 0; i < MAX_RACES; i++)
    {
        race_t *race_ptr = get_race_t_aux(i, 0);
        if (race_ptr->flags & RACE_IS_MONSTER) continue;
        /*if (race_ptr->flags & RACE_IS_DEPRECATED) continue;*/

        if (r % 20 == 0)
            fprintf(fff, "[[[[r|                 Dsrm  Dvce  Save  Stlh  Srch  Prcp  Melee  Bows  Infra\n");
        r++;

        fprintf(fff, "  [[[[w|%-14s| %+4d  %+4d  %+4d  %+4d  %+4d  %+4d  %+5d  %+4d  %4d'\n", 
            race_ptr->name,
            race_ptr->skills.dis, race_ptr->skills.dev, race_ptr->skills.sav,
            race_ptr->skills.stl, race_ptr->skills.srh, race_ptr->skills.fos,
            race_ptr->skills.thn, race_ptr->skills.thb, race_ptr->infra*10
        );
    }
    fprintf(fff, "\n\n");
    fprintf(fff, "***** [a] Demigods.txt\n");
    fprintf(fff, "***** [b] Draconians.txt\n");
}

static void _monster_races_help(FILE* fff)
{
    int i;
    player_type old = *p_ptr;

    fprintf(fff, "[[[[B|  Monster Races\n\n");
    for (i = 0; i < MAX_RACES; i++)
    {
        int max = 1, j;
        race_t *race_ptr = get_race_t_aux(i, 0);

        if (!(race_ptr->flags & RACE_IS_MONSTER)) continue;

        _race_help(fff, i);

        if (i == RACE_MON_SWORD) continue;
        if (i == RACE_MON_RING) continue;

        switch (i)
        {
        case RACE_MON_SPIDER: max = SPIDER_MAX; break;
        case RACE_MON_DEMON: max = DEMON_MAX; break;
        case RACE_MON_DRAGON: max = DRAGON_MAX; break;
        case RACE_MON_GIANT: max = GIANT_MAX; break;
        case RACE_MON_TROLL: max = TROLL_MAX; break;
        case RACE_MON_ELEMENTAL: max = ELEMENTAL_MAX; break;
        case RACE_MON_GOLEM: max = GOLEM_MAX; break;
        }

        for (j = 0; j < max; j++)
        {
            race_t *race_ptr;
            int     current_r_idx = 0;

            p_ptr->lev = 1;
            p_ptr->exp = 0;
            p_ptr->prace = i;
            p_ptr->psubrace = j;

            race_ptr = get_race_t();

            fprintf(fff, "[[[[r|  %21s STR  INT  WIS  DEX  CON  CHR  Life  BHP  Exp\n", "");    
            if (race_ptr->birth)
                race_ptr->birth();

            for (;;)
            {
                p_ptr->lev++;
                if (race_ptr->gain_level)
                    race_ptr->gain_level(p_ptr->lev);

                if (p_ptr->current_r_idx != current_r_idx)
                {
                    race_ptr = get_race_t();

                    fprintf(fff, "  %-21.21s %+3d  %+3d  %+3d  %+3d  %+3d  %+3d  %3d%%  %+3d  %3d%%\n", 
                        race_ptr->subname,
                        race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
                        race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
                        race_ptr->life, race_ptr->base_hp, race_ptr->exp
                    );

                    current_r_idx = p_ptr->current_r_idx;
                }

                if (p_ptr->lev >= PY_MAX_LEVEL)
                    break;
            }
            fprintf(fff, "\n");

            current_r_idx = 0;
            p_ptr->lev = 1;
            p_ptr->exp = 0;
            p_ptr->prace = i;
            p_ptr->psubrace = j;

            race_ptr = get_race_t();

            fprintf(fff, "[[[[r|  %-21s Dsrm   Dvce   Save   Stlh  Srch  Prcp  Melee  Bows\n", "");
            if (race_ptr->birth)
                race_ptr->birth();

            for (;;)
            {
                p_ptr->lev++;
                if (race_ptr->gain_level)
                    race_ptr->gain_level(p_ptr->lev);

                if (p_ptr->current_r_idx != current_r_idx)
                {
                    race_ptr = get_race_t();

                    fprintf(fff, "  %-21.21s %2d+%-2d  %2d+%-2d  %2d+%-2d  %2d+%-2d %2d+%-2d %2d+%-2d %2d+%-2d  %2d+%-2d\n", 
                        race_ptr->subname,
                        race_ptr->skills.dis, race_ptr->extra_skills.dis, 
                        race_ptr->skills.dev, race_ptr->extra_skills.dev, 
                        race_ptr->skills.sav, race_ptr->extra_skills.sav,
                        race_ptr->skills.stl, race_ptr->extra_skills.stl,
                        race_ptr->skills.srh, race_ptr->extra_skills.srh,
                        race_ptr->skills.fos, race_ptr->extra_skills.fos,
                        race_ptr->skills.thn, race_ptr->extra_skills.thn, 
                        race_ptr->skills.thb, race_ptr->extra_skills.thb
                    );

                    current_r_idx = p_ptr->current_r_idx;
                }

                if (p_ptr->lev >= PY_MAX_LEVEL)
                    break;
            }
            fprintf(fff, "\n");
        }
        if (i == RACE_MON_HOUND)
        {
            _wrap_text(fff, "Evolution in each tier is actually random. All of the possible forms in each tier are not displayed.", 2, 80);
            fprintf(fff, "\n");
        }

        fprintf(fff, "\n");
    }
    fprintf(fff, "\n\n");
    fprintf(fff, "***** [a] rings.txt\n");
    fprintf(fff, "***** [b] DragonRealms.txt\n");

    *p_ptr = old;
}

static void _demigod_help(FILE *fff, int idx)
{
    fprintf(fff, "***** <%s>\n", demigod_info[idx].name);
    fprintf(fff, "[[[[r|  %s\n\n", demigod_info[idx].name);
    _wrap_text(fff, demigod_info[idx].desc, 2, 80);

    fprintf(fff, "\n\n");
}

static void _demigods_help(FILE* fff)
{
    int i;

    fprintf(fff, "[[[[B|  Demigod Parentage\n\n");
    for (i = 0; i < MAX_DEMIGOD_TYPES; i++)
    {
        _demigod_help(fff, i);
    }

    fprintf(fff, "***** <Tables>\n");
    fprintf(fff, "[[[[y|  Table 1 - Race Statistic Bonus Table ---\n\n");
    fprintf(fff, "[[[[r|                 STR  INT  WIS  DEX  CON  CHR  Life  Exp\n");    

    for (i = 0; i < MAX_DEMIGOD_TYPES; i++)
    {
        race_t *race_ptr = get_race_t_aux(RACE_DEMIGOD, i);

        fprintf(fff, "  %-14s %+3d  %+3d  %+3d  %+3d  %+3d  %+3d  %3d%%  %3d%%\n", 
            demigod_info[i].name,
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
            race_ptr->life, race_ptr->exp
        );
    }
    fprintf(fff, "\n\n");

    fprintf(fff, "[[[[y|  Table 2 - Race Skill Bonus Table\n\n");
    fprintf(fff, "[[[[r|                 Dsrm  Dvce  Save  Stlh  Srch  Prcp  Melee  Bows  Infra\n");
    for (i = 0; i < MAX_DEMIGOD_TYPES; i++)
    {
        race_t *race_ptr = get_race_t_aux(RACE_DEMIGOD, i);

        fprintf(fff, "  %-14s %+4d  %+4d  %+4d  %+4d  %+4d  %+4d  %+5d  %+4d  %4d'\n", 
            demigod_info[i].name,
            race_ptr->skills.dis, race_ptr->skills.dev, race_ptr->skills.sav,
            race_ptr->skills.stl, race_ptr->skills.srh, race_ptr->skills.fos,
            race_ptr->skills.thn, race_ptr->skills.thb, race_ptr->infra*10
        );
    }
    fprintf(fff, "\n\n");

    fprintf(fff, "[[[[y|  Table 3 - Demigod Special Powers\n\n");
    _wrap_text(fff, 
                "All demigods have access to special powers. When they reach level 20, they may choose "
                "a single power from the following list. When they reach level, 40, they may choose another. "
                "These powers can never be removed or changed, so you might want to study this list to "
                "decide which powers you will choose for your character.", 
                2, 80);
    fprintf(fff, "\n");
    for (i = 0; i < MAX_MUTATIONS; i++)
    {
        if (mut_demigod_pred(i))
        {
            char b1[255], b2[255];
            mut_name(i, b1);
            mut_help_desc(i, b2);
            fprintf(fff, "  [[[[r|%-19s| %s\n", b1, b2);
        }
    }
    fprintf(fff, "\n\n");
}

static void _draconians_help(FILE* fff)
{
    int i;

    fprintf(fff, "[[[[B|  Draconians\n\n");
    for (i = 0; i < DRACONIAN_MAX; i++)
    {
        /* TODO */
    }

    fprintf(fff, "***** <Tables>\n");
    fprintf(fff, "[[[[y|  Table 1 - Race Statistic Bonus Table ---\n\n");
    fprintf(fff, "[[[[r|                 STR  INT  WIS  DEX  CON  CHR  Life  Exp\n");

    for (i = 0; i < DRACONIAN_MAX; i++)
    {
        race_t *race_ptr = get_race_t_aux(RACE_DRACONIAN, i);

        fprintf(fff, "  %-14s %+3d  %+3d  %+3d  %+3d  %+3d  %+3d  %3d%%  %3d%%\n",
            race_ptr->subname,
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS],
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR],
            race_ptr->life, race_ptr->exp
        );
    }
    fprintf(fff, "\n\n");

    fprintf(fff, "[[[[y|  Table 2 - Race Skill Bonus Table\n\n");
    fprintf(fff, "[[[[r|                 Dsrm  Dvce  Save  Stlh  Srch  Prcp  Melee  Bows  Infra\n");
    for (i = 0; i < DRACONIAN_MAX; i++)
    {
        race_t *race_ptr = get_race_t_aux(RACE_DRACONIAN, i);

        fprintf(fff, "  %-14s %+4d  %+4d  %+4d  %+4d  %+4d  %+4d  %+5d  %+4d  %4d'\n",
            race_ptr->subname,
            race_ptr->skills.dis, race_ptr->skills.dev, race_ptr->skills.sav,
            race_ptr->skills.stl, race_ptr->skills.srh, race_ptr->skills.fos,
            race_ptr->skills.thn, race_ptr->skills.thb, race_ptr->infra*10
        );
    }
    fprintf(fff, "\n\n");

    fprintf(fff, "[[[[y|  Table 3 - Draconian Special Powers\n\n");
    _wrap_text(fff,
                "All draconians have access to special powers. When they reach level 35, they may choose "
                "a single power from the following list. "
                "These powers can never be removed or changed, so you might want to study this list to "
                "decide which power you will choose for your character.",
                2, 80);
    fprintf(fff, "\n");
    for (i = 0; i < MAX_MUTATIONS; i++)
    {
        if (mut_draconian_pred(i))
        {
            char b1[255], b2[255];
            mut_name(i, b1);
            mut_help_desc(i, b2);
            fprintf(fff, "  [[[[r|%-19s| %s\n", b1, b2);
        }
    }
    fprintf(fff, "\n\n");
}

static void _class_help(FILE *fff, int idx)
{
    class_t *class_ptr = get_class_t_aux(idx, 0);
    
    fprintf(fff, "***** <%s>\n", class_ptr->name);
    fprintf(fff, "[[[[r|  %s\n\n", class_ptr->name);
    _wrap_text(fff, class_ptr->desc, 2, 80);
    fprintf(fff, "\n\n");
}

static void _dragon_realms_help(FILE* fff)
{
    int i, j;
    fprintf(fff, "[[[[B|  Dragon Realms\n\n");
    _wrap_text(fff, 
               "Dragons are magical creatures and may choose to learn a particular branch of "
               "dragon magic. Unlike normal spell casters, dragons do not need spell books to "
               "cast or learn powers. Instead, they simply gain spells as they mature. Each "
               "realm of dragon magic has a direct impact on the player's stats and skills, and "
               "each realm also requires a different stat for casting purposes.", 
               2, 80);
    fprintf(fff, "\n\n");
    for (i = 1; i < DRAGON_REALM_MAX; i++)
    {
        dragon_realm_ptr realm = dragon_get_realm(i);
        fprintf(fff, "***** <%s>\n", realm->name);
        fprintf(fff, "[[[[r|  %s\n\n", realm->name);
        _wrap_text(fff, realm->desc, 2, 80);
        fprintf(fff, "\n\n");
    }

    fprintf(fff, "***** <Tables>\n");
    fprintf(fff, "[[[[y|  Table 1 - Realm Statistic Bonus Table ---\n\n");
    fprintf(fff, "[[[[r|                 STR  INT  WIS  DEX  CON  CHR  Life  Exp\n");
    for (i = 1; i < DRAGON_REALM_MAX; i++)
    {
        dragon_realm_ptr realm = dragon_get_realm(i);
        char             line[255];
        char             tmp[255];

        sprintf(line, "  %-14s", realm->name);
        for (j = 0; j < 6; j++)
        {
            if (j == realm->spell_stat)
                sprintf(tmp, "[[[[G| %+3d |", realm->stats[j]);
            else
                sprintf(tmp, " %+3d ", realm->stats[j]);
            strcat(line, tmp);
        }
        sprintf(tmp, " %3d%%  %3d%%", realm->life, realm->exp);
        strcat(line, tmp);
        fprintf(fff, "%s\n", line);
    }
    fprintf(fff, "\n\n");

    fprintf(fff, "[[[[y|  Table 2 - Realm Skill Bonus Table\n\n");
    fprintf(fff, "[[[[r|                 Dsrm  Dvce  Save  Stlh  Srch  Prcp  Melee  Attack  Breath\n");
    for (i = 1; i < DRAGON_REALM_MAX; i++)
    {
        dragon_realm_ptr realm = dragon_get_realm(i);
        fprintf(fff, "  %-14s %+4d  %+4d  %+4d  %+4d  %+4d  %+4d  %+5d  %5d%%  %5d%%\n", 
            realm->name,
            realm->skills.dis, 
            realm->skills.dev,
            realm->skills.sav,
            realm->skills.stl, 
            realm->skills.srh, 
            realm->skills.fos,
            realm->skills.thn,
            realm->attack,
            realm->breath
        );
    }
    fprintf(fff, "\n\n");
}

static void _classes_help(FILE* fff)
{
    int i, j, r;

    fprintf(fff, "[[[[B|  The Classes\n\n");
    for (i = 0; i < MAX_CLASS; i++)
    {
        if (i == CLASS_MONSTER) continue;
        _class_help(fff, i);
    }

    fprintf(fff, "***** <Tables>\n");
    fprintf(fff, "[[[[y|  Table 1 - Class Statistic Bonus Table ---\n\n");

    for (i = 0, r = 0; i < MAX_CLASS; i++)
    {
        class_t     *class_ptr = get_class_t_aux(i, 0);
        caster_info *caster_ptr = 0;
        char         line[255];
        char         tmp[255];

        if (class_ptr->caster_info)
            caster_ptr = class_ptr->caster_info();

        if (i == CLASS_MONSTER) continue;

        if (r % 20 == 0)
            fprintf(fff, "[[[[r|                 STR  INT  WIS  DEX  CON  CHR  Life  BHP  Exp\n");    
        r++;

        sprintf(line, "  %-14s", class_ptr->name);
        for (j = 0; j < 6; j++)
        {
            if (caster_ptr && j == caster_ptr->which_stat && i != CLASS_PSION)
                sprintf(tmp, "[[[[G| %+3d |", class_ptr->stats[j]);
            else
                sprintf(tmp, " %+3d ", class_ptr->stats[j]);
            strcat(line, tmp);
        }
        sprintf(tmp, " %3d%%  %+3d  %3d%%", class_ptr->life, class_ptr->base_hp, class_ptr->exp);
        strcat(line, tmp);
        fprintf(fff, "%s\n", line);
    }
    fprintf(fff, "\n\n");

    fprintf(fff, "[[[[y|  Table 2 - Class Skill Bonus Table\n\n");
    for (i = 0, r = 0; i < MAX_CLASS; i++)
    {
        class_t *class_ptr = get_class_t_aux(i, 0);

        if (i == CLASS_BERSERKER) continue;
        if (i == CLASS_MONSTER) continue;

        if (r % 20 == 0)
            fprintf(fff, "[[[[r|                 Dsrm   Dvce   Save   Stlh  Srch  Prcp  Melee  Bows\n");
        r++;

        fprintf(fff, "  %-14s %2d+%-2d  %2d+%-2d  %2d+%-2d  %4d  %4d  %4d  %2d+%-2d  %2d+%-2d\n", 
            class_ptr->name,
            class_ptr->base_skills.dis, class_ptr->extra_skills.dis, 
            class_ptr->base_skills.dev, class_ptr->extra_skills.dev, 
            class_ptr->base_skills.sav, class_ptr->extra_skills.sav,
            class_ptr->base_skills.stl, 
            class_ptr->base_skills.srh, 
            class_ptr->base_skills.fos,
            class_ptr->base_skills.thn, class_ptr->extra_skills.thn, 
            class_ptr->base_skills.thb, class_ptr->extra_skills.thb
        );
    }
    fprintf(fff, "\n\n");
}

static void _personality_help(FILE *fff, int idx)
{
    player_seikaku *a_ptr = &seikaku_info[idx];
    fprintf(fff, "***** <%s>\n", a_ptr->title);
    fprintf(fff, "[[[[r|  %s\n\n", a_ptr->title);
    _wrap_text(fff, birth_get_personality_desc(idx), 2, 80);
    fprintf(fff, "\n\n");
}

static void _personalities_help(FILE* fff)
{
    int i;

    fprintf(fff, "[[[[B|  The Personalities\n\n");
    for (i = 0; i < MAX_PERSONALITIES; i++)
    {
        _personality_help(fff, i);
    }

    fprintf(fff, "***** <Tables>\n");
    fprintf(fff, "[[[[y|  Table 1 - Personality Statistic Bonus Table ---\n\n");
    fprintf(fff, "[[[[r|                 STR  INT  WIS  DEX  CON  CHR  Life  Exp\n");    

    for (i = 0; i < MAX_PERSONALITIES; i++)
    {
        player_seikaku *a_ptr = &seikaku_info[i];

        fprintf(fff, "  %-14s %+3d  %+3d  %+3d  %+3d  %+3d  %+3d  %3d%%  %3d%%\n", 
            a_ptr->title,
            a_ptr->a_adj[0], a_ptr->a_adj[1], a_ptr->a_adj[2], 
            a_ptr->a_adj[3], a_ptr->a_adj[4], a_ptr->a_adj[5], 
            a_ptr->life, a_ptr->a_exp
        );
    }
    fprintf(fff, "\n\n");

    fprintf(fff, "[[[[y|  Table 2 - Personality Skill Bonus Table ---\n\n");
    fprintf(fff, "[[[[r|                 Dsrm  Dvce  Save  Stlh  Srch  Prcp  Melee  Bows\n");
    for (i = 0; i < MAX_PERSONALITIES; i++)
    {
        player_seikaku *a_ptr = &seikaku_info[i];

        fprintf(fff, "  %-14s %+4d  %+4d  %+4d  %+4d  %+4d  %+4d  %+5d  %+4d\n", 
            a_ptr->title,
            a_ptr->skills.dis, a_ptr->skills.dev, a_ptr->skills.sav,
            a_ptr->skills.stl, a_ptr->skills.srh, a_ptr->skills.fos,
            a_ptr->skills.thn, a_ptr->skills.thb
        );
    }
    fprintf(fff, "\n\n");
}

static void _show_help(cptr helpfile)
{
    screen_save();
    show_file(TRUE, helpfile, NULL, 0, 0);
    screen_load();
}

static void _possessor_stats_help(FILE* fff)
{
    int i;
    fprintf(fff, "Name,Idx,Lvl,Speed,AC,Attacks,Dam,Body,Str,Int,Wis,Dex,Con,Chr,Life,Disarm,Device,Save,Stealth,Search,Perception,Melee,Bows\n");
    for (i = 0; i < max_r_idx; i++)
    {
        monster_race *r_ptr = &r_info[i];

        if (r_ptr->flags9 & RF9_DROP_CORPSE)
        {
            int ac = 0, dam = 0, attacks = 0, j;

            if (r_ptr->flags9 & RF9_POS_GAIN_AC)
                ac = r_ptr->ac;

            for (j = 0; j < 4; j++)
            {
                if (!r_ptr->blow[j].effect) continue;
                if (r_ptr->blow[j].method == RBM_EXPLODE) continue;
                if (r_ptr->blow[j].method == RBM_SHOOT) continue;

                dam += r_ptr->blow[j].d_dice * (r_ptr->blow[j].d_side + 1) / 2;
                attacks++;
            }

            fprintf(fff, "\"%s\",%d,%d,%d,%d,%d,%d,%s,%d,%d,%d,%d,%d,%d,%d,=\"%d+%d\",=\"%d+%d\",=\"%d+%d\",%d,%d,%d,=\"%d+%d\",=\"%d+%d\"\n", 
                r_name + r_ptr->name, i, r_ptr->level, 
                r_ptr->speed - 110, ac, attacks, dam,
                b_name + b_info[r_ptr->body.body_idx].name,
                r_ptr->body.stats[A_STR], r_ptr->body.stats[A_INT], r_ptr->body.stats[A_WIS],
                r_ptr->body.stats[A_DEX], r_ptr->body.stats[A_CON], r_ptr->body.stats[A_CHR],
                r_ptr->body.life,
                r_ptr->body.skills.dis, r_ptr->body.extra_skills.dis, 
                r_ptr->body.skills.dev, r_ptr->body.extra_skills.dev, 
                r_ptr->body.skills.sav, r_ptr->body.extra_skills.sav,
                r_ptr->body.skills.stl,
                r_ptr->body.skills.srh, 
                r_ptr->body.skills.fos,
                r_ptr->body.skills.thn, r_ptr->body.extra_skills.thn, 
                r_ptr->body.skills.thb, r_ptr->body.extra_skills.thb
            );
        }
    }
}

void generate_spoilers(void)
{
    spoiler_hack = TRUE;

    _text_file("Races.txt", _races_help);
    _text_file("MonsterRaces.txt", _monster_races_help);
    _text_file("Demigods.txt", _demigods_help);
    _text_file("Draconians.txt", _draconians_help);
    _text_file("Classes.txt", _classes_help);
    _text_file("Personalities.txt", _personalities_help);
    _text_file("PossessorStats.csv", _possessor_stats_help);
    _text_file("DragonRealms.txt", _dragon_realms_help);

    _show_help("Personalities.txt");
    spoiler_hack = FALSE;
}

#endif
