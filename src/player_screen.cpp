
/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include <src/player_screen.h>
#include <src/help.h>
#include <src/utilities.h>
#include <QDialogButtonBox>
#include <QPixmap>
#include <QPainter>
#include <QPlainTextEdit>
#include <QTextStream>
#include <QList>
#include <QPushButton>
#include <QScrollArea>

// The update code assumes the tables below are no longer than this #define
#define  TABLES_MAX_ENTRIES     20

// The null line is there to prevent crashes as the data is read;
player_flag_record player_resist_table[] =
{
    { "Resist Acid",        2, TR2_RES_ACID,	TR2_IM_ACID, TRUE, FALSE},
    { "Resist Lightning",   2, TR2_RES_ELEC,	TR2_IM_ELEC, TRUE, FALSE},
    { "Resist Fire",        2, TR2_RES_FIRE,	TR2_IM_FIRE, TRUE, FALSE},
    { "Resist Cold",        2, TR2_RES_COLD,	TR2_IM_COLD, TRUE, FALSE},
    { "Resist Poison",      2, TR2_RES_POIS,	TR2_IM_POIS, FALSE, FALSE},
    { "Resist Blindness",   2, TR2_RES_BLIND,	0, TRUE, FALSE},
    { "Resist Confusion",	2, TR2_RES_CONFU,	0, FALSE, FALSE},
    { "Resist Nexus",       2, TR2_RES_NEXUS,	0, FALSE, FALSE},
    { "Resist Nether",      2, TR2_RES_NETHR,	0, FALSE, FALSE},
    { "Resist Chaos",       2, TR2_RES_CHAOS,	0, FALSE, FALSE},
    { "R. Disenchantment",	2, TR2_RES_DISEN,	0, FALSE, FALSE},
    { "Resist Sound",       2, TR2_RES_SOUND,	0, FALSE, FALSE},
    { "Resist Shards",      2, TR2_RES_SHARD,	0, FALSE, FALSE},
    { "Resist Light",       2, TR2_RES_LIGHT,	0, FALSE, FALSE},
    { "Resist Darkness",    2, TR2_RES_DARK,	0, FALSE, FALSE},
    { "Resist Fear",        2, TR2_RES_FEAR,	0, FALSE, FALSE},
    { NULL,                 0, 0,               0, FALSE, FALSE},
};

// The null line is there to prevent crashes as the data is read;
player_flag_record player_abilities_table[] =
{
    { "See Invisible",      3, TR3_SEE_INVIS, 	0, TRUE, FALSE},
    { "Free Action",        3, TR3_FREE_ACT, 	0, TRUE, FALSE},
    { "Telepathy",          3, TR3_TELEPATHY, 	0, TRUE, FALSE},
    { "Hold Life",          3, TR3_HOLD_LIFE, 	0, TRUE, FALSE},
    { "Permanent Light",    3, TR3_LIGHT, 		0, TRUE, FALSE},
    { "Regeneration",       3, TR3_REGEN, 		0, TRUE, FALSE},
    { "Slow Digestion",     3, TR3_SLOW_DIGEST,	0, TRUE, FALSE},
    { "Feather Fall",       3, TR3_FEATHER, 	0, TRUE, FALSE},
    { "Teleportation",      3, TR3_TELEPORT, 	0, TRUE, TRUE},
    { "Aggravate",          3, TR3_AGGRAVATE,	0, TRUE, TRUE},
    { "Cursed",             3, TR3_CURSE_ALL,   0, TRUE, TRUE},
    { "Drain Exp.",         3, TR3_DRAIN_EXP,	0, FALSE, TRUE},
    { NULL,                 0, 0,               0, FALSE, FALSE},
};

// The null line is there to prevent crashes as the data is read;
//The stats need to come first and in order for the stat tooltips to work properly
player_flag_record player_pval_table[] =
{
    { "Strength",           1, TR1_STR,         TR2_SUST_STR, TRUE, FALSE},
    { "Intelligence",       1, TR1_INT,         TR2_SUST_INT, TRUE, FALSE},
    { "Wisdom",             1, TR1_WIS,         TR2_SUST_WIS, TRUE, FALSE},
    { "Dexterity",          1, TR1_DEX,         TR2_SUST_DEX, TRUE, FALSE},
    { "Constitution",       1, TR1_CON,         TR2_SUST_CON, TRUE, FALSE},
    { "Charisma",           1, TR1_CHR,         TR2_SUST_CHR, TRUE, FALSE},
    { "Infravision",        1, TR1_INFRA,		0, TRUE, FALSE},
    { "Stealth",            1, TR1_STEALTH,		0, TRUE, FALSE},
    { "Searching",          1, TR1_SEARCH,		0, TRUE, FALSE},
    { "Speed",              1, TR1_SPEED,		0, TRUE, FALSE},
    { "Tunneling",          1, TR1_TUNNEL,      0, TRUE, FALSE},
    { "Extra attacks",      1, TR1_BLOWS,		0, TRUE, FALSE},
    { "Extra Shots",        1, TR1_SHOTS,		0, TRUE, FALSE},
    { "Shooting Power",     1, TR1_MIGHT,		0, TRUE, FALSE},
    { NULL,                 0, 0,               0, FALSE, FALSE},
};

// The null line is there to prevent crashes as the data is read;
player_flag_record player_nativity_table[]=
{
    { "Lava",   4,  TN1_NATIVE_LAVA,    0, FALSE, FALSE},
    { "Ice",    4,  TN1_NATIVE_ICE,     0, FALSE, FALSE},
    { "Oil",    4,  TN1_NATIVE_OIL,		0, FALSE, FALSE},
    { "Fire",   4,  TN1_NATIVE_FIRE,    0, FALSE, FALSE},
    { "Sand",   4,  TN1_NATIVE_SAND,    0, FALSE, FALSE},
    { "Forest", 4,  TN1_NATIVE_FOREST,  0, FALSE, FALSE},
    { "Water",  4,  TN1_NATIVE_WATER,  0, FALSE, FALSE},
    { "Acid",   4,  TN1_NATIVE_ACID,    0, FALSE, FALSE},
    { "Mud",    4,  TN1_NATIVE_MUD,		0, FALSE, FALSE},
    { "Boiling Water",    4,  TN1_NATIVE_BWATER,		0, FALSE, FALSE},
    { "Boiling Mud",    4,  TN1_NATIVE_BMUD,		0, FALSE, FALSE},
    { NULL,       0,    0,              0, FALSE, FALSE},
};

QString moria_speed_labels(int speed)
{

    // Boundry Control
    if (speed < NPPMORIA_LOWEST_SPEED) speed = NPPMORIA_LOWEST_SPEED;
    else if (speed > NPPMORIA_MAX_SPEED) speed = NPPMORIA_MAX_SPEED;

    switch (speed)
    {
        case NPPMORIA_LOWEST_SPEED:
        {
            return("Very slow");
        }
        case (NPPMORIA_LOWEST_SPEED + 1):
        {
            return("Slow");
        }
        case NPPMORIA_MAX_SPEED:
        {
            return("Max speed");
        }
        case (NPPMORIA_MAX_SPEED - 1):
        {
            return("Very fast");
        }
        case (NPPMORIA_MAX_SPEED - 2):
        {
            return("Fast");
        }
        default: return("Normal");
    }
}

/*
 * Hack - Modify the color based on speed bonuses. -DG-
 */
byte analyze_speed_bonuses(int speed, byte default_attr)
{
    if (game_mode == GAME_NPPMORIA)
    {
        if (speed > NPPMORIA_NORMAL_SPEED)	return (TERM_GREEN);
        if (speed < NPPMORIA_NORMAL_SPEED)	return (TERM_UMBER);
        return (default_attr);
    }

    // GAME_NPPANGBAND
    if (speed > 110) return (TERM_GREEN);
    if (speed < 110) return (TERM_UMBER);
    return (default_attr);
}

// set up a standard label
void make_standard_label(QLabel *this_label, QString title, byte preset_color, QFont this_font)
{
    make_standard_label(this_label, title, preset_color);
    this_label->setFont(this_font);
}

// set up a standard label
void make_standard_label(QLabel *this_label, QString title, byte preset_color)
{
    this_label->clear();
    this_label->setText(QString("<b>%1</b>") .arg(color_string(title, preset_color)));
    this_label->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Minimum);
}

static void make_ability_graph(QLabel *this_label, int min, int max, int value, QFont this_font)
{
    QFontMetrics metrics(this_font);
    QSize this_size = metrics.size(Qt::TextSingleLine, "MMMMMMMMM");
    this_size.setHeight(this_size.height() *2/ 3);
    QPixmap this_img(this_size);
    QPainter paint(&this_img);
    QPen pen;

    this_img.fill(Qt::lightGray);

    // avoid crashes and set values
    if (max <= min) return;
    if (value < 0) return;
    int this_max = max - min;
    int this_value = value - min;
    if (this_value > this_max) this_value = this_max;

    int this_percent = this_value * 100 / this_max;

    //Draw an outside border
    paint.setPen(pen);
    pen.setWidthF(this_img.height() / 5);
    pen.setColor(Qt::black);

    QRect filler(0,0,this_size.width(), this_size.height());
    paint.drawRect(filler);

    // Fill based on % between min and max
    QColor this_color = defined_colors[TERM_RED];
    if (this_percent >= 100) this_color = defined_colors[TERM_GREEN];
    else if (this_percent >= 90) this_color = defined_colors[TERM_L_GREEN];
    else if (this_percent >= 75) this_color = defined_colors[TERM_BLUE];
    else if (this_percent >= 50) this_color = defined_colors[TERM_SKY_BLUE];
    else if (this_percent >= 25) this_color = defined_colors[TERM_ORANGE];
    else if (this_percent > 10) this_color = defined_colors[TERM_YELLOW];

    // Draw progress towards 100%
    filler.setWidth(filler.width() * this_percent / 100);
    paint.fillRect(filler, this_color);

    //draw a box around the progress bar.
    paint.drawRect(filler);

    this_label->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Maximum);

    this_label->setPixmap(this_img);
}

// This should only receive equipment labels created in the function below
// For efficiency's sake, this is not checked
static void update_equippy_labels(QList<QLabel *> this_list)
{
    for (int x = 0; x < this_list.size(); x++)
    {
        QLabel *this_lbl = this_list.at(x);

        QString this_name = this_lbl->objectName();

        this_name.remove("equippy_");
        if (this_name[0].isDigit())
        {
            int this_num = this_name.toInt();
            // We assume this is between INVEN_WIELD and INVEN_TOTAL
            object_type *o_ptr = &inventory[this_num];
            if (!o_ptr->k_idx)
            {
                this_lbl->setText("");
                continue;
            }

            object_kind *k_ptr = &k_info[o_ptr->k_idx];
            this_lbl->setText(QString("<b>%1</b>") .arg(color_string(k_ptr->d_char, k_ptr->color_num)));
            QString obj_text = QString("%1: ") .arg(mention_use(this_num));
            obj_text.append(object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL));
            this_lbl->setToolTip(obj_text);
        }
    }
}

// Draw the equipment labels
static void draw_equippy_labels(QGridLayout *return_layout, int row, int col, bool do_player, bool do_temp, QFont this_font)
{
    // Leave one column for the labels.
    col ++;

    QList<QLabel *> equippy;
    equippy.clear();

    for (int i = INVEN_WIELD; i < INVEN_TOTAL; i++, col++)
    {
        // Set up a tooltip and pixture and add it to the layout.
        QLabel *obj_label = new QLabel;
        obj_label->setObjectName(QString("equippy_%1") .arg(i));
        return_layout->addWidget(obj_label, row, col, Qt::AlignCenter);
        equippy.append(obj_label);
    }

    if (do_player)
    {
        QLabel *person_label = new QLabel;
        make_standard_label(person_label, "@", TERM_DARK, this_font);
        person_label->setObjectName("equippy_p");
        person_label->setToolTip("Innate character traits.");
        return_layout->addWidget(person_label, row, col++);
    }

    if (do_temp)
    {
        QLabel *temp_label = new QLabel("t");
        temp_label->setToolTip("Temporary character traits.");
        temp_label->setObjectName("equippy_t");
        temp_label->setFont(this_font);
        return_layout->addWidget(temp_label, row, col++);
    }

    QLabel *filler = new QLabel(" ");
    filler->setObjectName("equippy_f");
    filler->setFont(this_font);
    return_layout->addWidget(filler, 0, col);

    update_equippy_labels(equippy);
}

QString stat_entry(int stat)
{

    // Paranoia
    if (stat >= A_MAX) return ("invalid stat");
    if (stat < A_STR) return ("invalid stat");

    return(get_help_topic("character_info", stat_names_full[stat]));
}

void PlayerScreenDialog::name_change(void)
{
    QString new_name = get_string("Please enter a new name for your character.", "Enter new name", op_ptr->full_name);

    if (new_name.contains("'"))
    {
        pop_up_message_box("The character name can not contain an apostrophe.");
        new_name.remove("'");
    }

    if (!new_name.length()) return;

    op_ptr->full_name = new_name;

    // Update the new name
    QList<QLabel *> lbl_list = this->findChildren<QLabel *>();
    for (int i = 0; i < lbl_list.size(); i++)
    {
        QLabel *this_lbl = lbl_list.at(i);

        QString this_name = this_lbl->objectName();

        if (this_name.operator ==("PLYR_Name"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(op_ptr->full_name), TERM_BLUE));
            return;
        }
    }
}


// Go through all the labels and update the ones that show data.
void update_char_screen(QWidget *return_widget, QFont this_font)
{
    QList<QLabel *> lbl_list = return_widget->findChildren<QLabel *>();
    for (int i = 0; i < lbl_list.size(); i++)
    {
        QLabel *this_lbl = lbl_list.at(i);

        QString this_name = this_lbl->objectName();

        QString first_num_string;
        QString second_num_string;
        s32b first_num;
        s32b second_num;

        //Not a named label
        if (!this_name.length()) continue;

        // Update all of the stats
        for (int i = 0; i < A_MAX; i++)
        {
            if (this_name.contains(QString("st_base_%1") .arg(i)))
            {
                QString base_num = (QString("<b>%1 </b>") .arg(cnv_stat(p_ptr->stat_base_max[i])));
                base_num = color_string(base_num, TERM_BLUE);
                this_lbl->setText(base_num);

                continue;
            }

            if (this_name.contains(QString("st_race_%1") .arg(i)))
            {
                QString race_num = format_stat(rp_ptr->r_adj[i]);
                race_num.append(" ");
                race_num = color_string(race_num, (rp_ptr->r_adj[i] < 0 ? TERM_RED : TERM_BLUE));
                this_lbl->setText(race_num);
                continue;
            }
            if (this_name.contains(QString("st_class_%1") .arg(i)))
            {
                QString class_num = format_stat(cp_ptr->c_adj[i]);
                class_num.append(" ");
                class_num = color_string(class_num, (cp_ptr->c_adj[i] < 0 ? TERM_RED : TERM_BLUE));
                this_lbl->setText(class_num);
                continue;
            }
            if (this_name.contains(QString("st_equip_%1") .arg(i)))
            {
                int equip = p_ptr->state.stat_equip[i];

                QString equip_num = format_stat(equip);
                equip_num.append(" ");
                equip_num = color_string(equip_num, (equip < 0 ? TERM_RED : TERM_BLUE));
                this_lbl->setText(equip_num);
                continue;
            }
            if (this_name.contains(QString("st_quest_%1") .arg(i)))
            {
                QString quest_num = format_stat(p_ptr->stat_quest_add[i]);
                quest_num.append(" ");
                quest_num = color_string(quest_num, TERM_BLUE);
                this_lbl->setText(quest_num);
                continue;
            }
            if (this_name.contains(QString("st_total_%1") .arg(i)))
            {
                QString stat_final = (QString("<b>%1</b>") .arg(cnv_stat(p_ptr->state.stat_loaded_max[i])));
                stat_final.append(" ");
                stat_final = color_string(stat_final, TERM_BLUE);
                this_lbl->setText(stat_final);
                continue;
            }
            if (this_name.contains(QString("st_reduce_%1") .arg(i)))
            {
                if (p_ptr->state.stat_loaded_cur[i] < p_ptr->state.stat_loaded_max[i])
                {
                    QString lower_stat = cnv_stat(p_ptr->state.stat_loaded_cur[i]);
                    lower_stat = color_string(lower_stat, TERM_RED);
                    this_lbl->setText(lower_stat);
                }
                else this_lbl->setText("");

                continue;
            }
        }

        if (this_name.operator ==("PLYR_Name"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(op_ptr->full_name), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Sex"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(sp_ptr->title), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Race"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(p_info[p_ptr->prace].pr_name), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Class"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(c_info[p_ptr->pclass].cl_name), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Title"))
        {
            QString title = get_player_title();
            if (p_ptr->is_wizard) title = "[=-WIZARD-=]";
            else if (p_ptr->total_winner)  title = "**WINNER**";
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(title), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_HP"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(QString("%1/%2") .arg(p_ptr->chp) .arg(p_ptr->mhp)), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_SP"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(QString("%1/%2") .arg(p_ptr->csp) .arg(p_ptr->msp)), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Fame"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(p_ptr->q_fame), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Gold"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(number_to_formatted_string(p_ptr->au)), TERM_GOLD));
            continue;
        }
        if (this_name.operator ==("PLYR_Age"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(p_ptr->age), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Height"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(p_ptr->ht), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Weight"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(p_ptr->wt), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_SC"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(p_ptr->sc), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("TURN_Game"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(number_to_formatted_string(p_ptr->game_turn)), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("TURN_Player"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(number_to_formatted_string(p_ptr->p_turn)), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("DEPTH_Cur"))
        {
            QString cur_depth = (QString("%1") .arg(p_ptr->depth * 50));
            if (!p_ptr->max_depth) cur_depth = "TOWN";
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(cur_depth), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("DEPTH_Max"))
        {
            QString max_depth = (QString("%1") .arg(p_ptr->max_depth * 50));
            if (!p_ptr->max_depth) max_depth = "TOWN";
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(max_depth), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Infra"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg((QString("%1 feet") .arg(p_ptr->state.see_infra * 10))), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Speed"))
        {
            make_ability_graph(this_lbl, 0, pam_ptr->max_p_speed, calc_energy_gain(p_ptr->state.p_speed), this_font);
            continue;
        }
        if (this_name.operator ==("PLYR_Save"))
        {
            make_ability_graph(this_lbl, 0, 100, p_ptr->state.skills[SKILL_SAVE], this_font);
            continue;
        }
        if (this_name.operator ==("PLYR_Stealth"))
        {
            int stealth = (WAKEUP_MAX - p_ptr->base_wakeup_chance);
            if (p_ptr->state.aggravate) stealth = WAKEUP_MAX;
            make_ability_graph(this_lbl, WAKEUP_MIN, WAKEUP_MAX, stealth, this_font);
            continue;
        }
        if (this_name.operator ==("PLYR_Fight"))
        {
            make_ability_graph(this_lbl, 0, pam_ptr->max_skills[SKILL_TO_HIT_MELEE], p_ptr->state.skills[SKILL_TO_HIT_MELEE], this_font);
            continue;
        }
        if (this_name.operator ==("PLYR_Bow"))
        {
            make_ability_graph(this_lbl, 0, pam_ptr->max_skills[SKILL_TO_HIT_BOW], p_ptr->state.skills[SKILL_TO_HIT_BOW], this_font);
            continue;
        }
        if (this_name.operator ==("PLYR_Throw"))
        {
            make_ability_graph(this_lbl, 0, pam_ptr->max_skills[SKILL_TO_HIT_THROW], p_ptr->state.skills[SKILL_TO_HIT_THROW], this_font);
            continue;
        }
        if (this_name.operator ==("PLYR_Disarm"))
        {
            make_ability_graph(this_lbl, 0, pam_ptr->max_skills[SKILL_DISARM], p_ptr->state.skills[SKILL_DISARM], this_font);
            continue;
        }
        if (this_name.operator ==("PLYR_Magic"))
        {
            make_ability_graph(this_lbl, 0, pam_ptr->max_skills[SKILL_DEVICE], p_ptr->state.skills[SKILL_DEVICE], this_font);
            continue;
        }
        if (this_name.operator ==("PLYR_Level"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(p_ptr->lev), (p_ptr->lev >= p_ptr->max_lev) ? TERM_BLUE : TERM_RED));
            continue;
        }
        if (this_name.operator ==("PLYR_Cur_Exp"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(number_to_formatted_string(p_ptr->exp)), (p_ptr->exp >= p_ptr->max_exp) ? TERM_BLUE : TERM_RED));
            continue;
        }
        if (this_name.operator ==("PLYR_Max_Exp"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(number_to_formatted_string(p_ptr->max_exp)), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Advance"))
        {
            if (p_ptr->lev == z_info->max_level)
            {
                this_lbl->setText(color_string("---------", TERM_BLUE));
                continue;
            }

            s32b advance = (get_experience_by_level(p_ptr->lev-1) * p_ptr->expfact / 100L);
            s32b exp_needed = advance - p_ptr->exp;
            QString exp_output = number_to_formatted_string(exp_needed);
            QString exp_advance = (QString("The total experience needed for the next level is %1") .arg(number_to_formatted_string(advance)));
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(exp_output), TERM_BLUE));
            this_lbl->setToolTip(exp_advance);
            continue;
        }
        if (this_name.operator ==("PLYR_Score"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(number_to_formatted_string(p_ptr->current_score)), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("BURDEN_Cur"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(formatted_weight_string(p_ptr->total_weight)), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("BURDEN_Max"))
        {
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(formatted_weight_string(normal_speed_weight_limit())), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("BURDEN_Percent"))
        {
            int pct = (p_ptr->total_weight * 100) / normal_speed_weight_limit();
            this_lbl->setText(color_string(QString("<b>%1%</b>") .arg(pct), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("BASE_Speed"))
        {
            first_num = p_ptr->state.p_speed;
            // Undo temporary effects
            if (p_ptr->searching) first_num += (game_mode == GAME_NPPMORIA ? 1 : 10);
            if (p_ptr->timed[TMD_FAST]) first_num -= (game_mode == GAME_NPPMORIA ? 1 : 10);
            if (p_ptr->timed[TMD_SLOW]) first_num += (game_mode == GAME_NPPMORIA ? 1 : 10);
            // Speed is different in the different games
            if (game_mode == GAME_NPPMORIA)
            {
                first_num_string = moria_speed_labels(first_num);
                second_num = analyze_speed_bonuses(first_num, TERM_BLUE);

            }
            else
            {
                second_num = analyze_speed_bonuses(first_num, TERM_BLUE);
                if (first_num > 110)
                {
                    first_num_string = (QString("Fast (%1)") .arg(first_num - 110));
                    first_num_string.prepend("+");
                }
                else if (first_num < 110)
                {
                    first_num_string = (QString("Slow (%1)") .arg(110 - first_num));
                }
                else first_num_string = "Normal";
            }

            this_lbl->setText(color_string((QString("<b>%1</b>") .arg(first_num_string)), second_num));
            continue;
        }
        if (this_name.operator ==("PLYR_AC"))
        {
            first_num = p_ptr->state.known_ac;
            second_num = p_ptr->state.known_to_a;
            first_num_string.setNum(first_num);
            second_num_string.setNum(second_num);
            if (second_num > 0) second_num_string.prepend("+");
            this_lbl->setText(color_string((QString("<b>[%1, %2]</b>") .arg(first_num_string) .arg(second_num_string)), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_MELEE"))
        {
            first_num = p_ptr->state.known_to_h;
            second_num = p_ptr->state.known_to_d;

            object_type *o_ptr = &inventory[INVEN_WIELD];
            QString dd; ;
            QString ds;
            dd.setNum(o_ptr->dd);
            ds.setNum(o_ptr->ds);
            // Player is punching monsters
            if (!o_ptr->tval)
            {
                dd.setNum(1);
                ds.setNum(1);
            }
            else if (o_ptr->is_known())
            {
                first_num += o_ptr->to_h;
                second_num += o_ptr->to_d;
            }
            first_num_string.setNum(first_num);
            second_num_string.setNum(second_num);
            if (first_num > 0) first_num_string.prepend("+");
            if (second_num > 0) second_num_string.prepend("+");
            this_lbl->setText(color_string((QString("<b>x(%1) [%2d%3] (%4, %5)</b>") .arg(p_ptr->state.num_blow) .arg(dd) .arg(ds) .arg(first_num_string) .arg(second_num_string)), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("HIT_Critical"))
        {
            object_type *o_ptr = &inventory[INVEN_WIELD];
            first_num = critical_hit_chance(o_ptr, p_ptr->state, TRUE) /  (CRIT_HIT_CHANCE / 100);
            first_num_string.setNum(first_num);
            this_lbl->setText(color_string((QString("<b>%1 %</b>") .arg(first_num_string)), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("SHOOTING_Stats"))
        {
            object_type *o_ptr;
            object_type object_type_body;
            object_type_body.object_wipe();
            //Make sure we are factoring in the bow and not a swap weapon
            if (birth_swap_weapons)
            {
                if (inventory[INVEN_MAIN_WEAPON].tval == TV_BOW) o_ptr = &inventory[INVEN_MAIN_WEAPON];

                /* A bow is not wielded, just set up a "dummy, blank" object and point to that */
                else o_ptr = &object_type_body;
            }
            else o_ptr = &inventory[INVEN_BOW];

            first_num = p_ptr->state.known_to_h;
            second_num = 0;
            s32b mult = p_ptr->state.ammo_mult;
            if (o_ptr->is_known())
            {
                first_num += o_ptr->to_h;
                second_num += o_ptr->to_d;
            }

            // Factor in Rogue and brigand combat bonuses, if applicable
            mult += rogue_shot(o_ptr, &first_num, p_ptr->state);
            mult += brigand_shot(o_ptr, 0L, FALSE, p_ptr->state);

            first_num_string.setNum(first_num);
            second_num_string.setNum(second_num);
            if (first_num > 0) first_num_string.prepend("+");
            if (second_num > 0) second_num_string.prepend("+");
            QString output = (QString("<b>xS(%1) xM(%2) (%3, %4)</b>") .arg(p_ptr->state.num_fire) .arg(mult) .arg(first_num_string) .arg(second_num_string));
            //No bow
            if (!mult) output = "---------";
            this_lbl->setText(color_string(output, TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("SEARCH_Freq"))
        {
            if (p_ptr->state.skills[SKILL_SEARCH_FREQUENCY] > SEARCH_CHANCE_MAX) first_num_string = "1 in 1";
            else first_num_string = (QString("1 in %1") .arg(SEARCH_CHANCE_MAX - p_ptr->state.skills[SKILL_SEARCH_FREQUENCY]));
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(first_num_string), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("SEARCH_Chance"))
        {
            if (p_ptr->state.skills[SKILL_SEARCH_CHANCE] > 100) first_num = 100;
            else first_num = p_ptr->state.skills[SKILL_SEARCH_CHANCE];
            first_num_string = (QString("%1%") .arg(first_num));
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(first_num_string), TERM_BLUE));
            continue;
        }
        if (this_name.operator ==("PLYR_Tunnel"))
        {
            first_num_string.setNum(p_ptr->state.skills[SKILL_DIGGING]);
            this_lbl->setText(color_string(QString("<b>%1</b>") .arg(first_num_string), TERM_BLUE));
            continue;
        }
    }
}

void PlayerScreenDialog::name_change_pushbutton(QGridLayout *return_layout)
{
    QPushButton *label_player_name = new QPushButton("NAME:");
    QPalette pushbutton_palette;
    pushbutton_palette.setColor(QPalette::ButtonText, defined_colors[TERM_DARK]);
    label_player_name->setPalette(pushbutton_palette);
    label_player_name->setToolTip("Press to change player name");
    connect(label_player_name, SIGNAL(clicked()), this, SLOT(name_change()));
    return_layout->addWidget(label_player_name, 0, 0, Qt::AlignLeft);
}


void char_basic_info(QGridLayout *return_layout)
{
    int row = 0;
    int col = 0;

    // Add basic name
    //Hack, the button has already been added and connected inside the class.
    QLabel *player_name = new QLabel;
    make_standard_label(player_name, " ", TERM_BLUE);
    player_name->setToolTip(get_help_topic("character_info", "Character Name"));
    player_name->setObjectName("PLYR_Name");
    return_layout->addWidget(player_name, row++, col+1, Qt::AlignRight);

    // Add gender
    QLabel *label_player_gender = new QLabel;
    make_standard_label(label_player_gender, "GENDER:", TERM_DARK);
    label_player_gender->setObjectName("GENDER_LABEL");
    label_player_gender->setToolTip(get_help_topic("character_info", "Gender"));
    QLabel *player_gender = new QLabel;
    make_standard_label(player_gender, " ", TERM_BLUE);
    player_gender->setObjectName("PLYR_Sex");
    return_layout->addWidget(label_player_gender, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_gender, row++, col+1, Qt::AlignRight);

    // Add race
    QLabel *label_player_race = new QLabel;
    make_standard_label(label_player_race, "RACE:", TERM_DARK);
    label_player_race->setObjectName("RACE_LABEL");
    label_player_race->setToolTip(get_help_topic("race_class_info", "Race"));
    QLabel *player_race = new QLabel;
    make_standard_label(player_race, " ", TERM_BLUE);
    player_race->setObjectName("PLYR_Race");
    return_layout->addWidget(label_player_race, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_race, row++, col+1, Qt::AlignRight);

    // Add class
    QLabel *label_player_class = new QLabel;
    make_standard_label(label_player_class, "CLASS:", TERM_DARK);
    label_player_class->setObjectName("CLASS_LABEL");
    label_player_class->setToolTip(get_help_topic("race_class_info", "Class"));
    QLabel *player_class = new QLabel;
    make_standard_label(player_class, " ", TERM_BLUE);
    player_class->setObjectName("PLYR_Class");
    return_layout->addWidget(label_player_class, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_class, row++, col+1, Qt::AlignRight);

    // Add title
    QLabel *label_player_title = new QLabel;
    make_standard_label(label_player_title, "TITLE:", TERM_DARK);
    label_player_title->setObjectName("TITLE_LABEL");
    label_player_title->setToolTip(get_help_topic("character_info", "Title"));
    QLabel *player_title = new QLabel;
    make_standard_label(player_title, " ", TERM_BLUE);
    player_title->setObjectName("PLYR_Title");
    return_layout->addWidget(label_player_title, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_title, row++, col+1, Qt::AlignRight);

    // Add hit points
    QLabel *label_player_hp = new QLabel;
    make_standard_label(label_player_hp, "HIT POINTS:", TERM_DARK);
    label_player_hp->setObjectName("HP_LABEL");
    label_player_hp->setToolTip(get_help_topic("character_info", "Hit Points"));
    QLabel *player_hp = new QLabel;
    make_standard_label(player_hp, " ", TERM_BLUE);
    player_hp->setObjectName("PLYR_HP");
    return_layout->addWidget(label_player_hp, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_hp, row++, col+1, Qt::AlignRight);

    // Add spell points (if applicable)
    if (cp_ptr->spell_book)
    {
        QLabel *label_player_sp = new QLabel;
        make_standard_label(label_player_sp, "SPELL POINTS:", TERM_DARK);
        label_player_sp->setObjectName("SP_LABEL");
        label_player_sp->setToolTip(get_help_topic("character_info", "Spell Points"));
        QLabel *player_sp = new QLabel;
        make_standard_label(player_sp, " ", TERM_BLUE);
        player_sp->setObjectName("PLYR_SP");
        return_layout->addWidget(label_player_sp, row, col, Qt::AlignLeft);
        return_layout->addWidget(player_sp, row++, col+1, Qt::AlignRight);
    }
    else row++;

    // Add fame or skip a space
    if (!birth_no_quests)
    {
        // Add fame
        QLabel *label_player_fame = new QLabel;
        make_standard_label(label_player_fame, "FAME:", TERM_DARK);
        label_player_fame->setObjectName("FAME_LABEL");
        label_player_fame->setToolTip(get_help_topic("character_info", "Fame"));
        QLabel *player_fame = new QLabel;
        make_standard_label(player_fame, " ", TERM_BLUE);
        player_fame->setObjectName("PLYR_Fame");
        return_layout->addWidget(label_player_fame, row, col, Qt::AlignLeft);
        return_layout->addWidget(player_fame, row++, col+1, Qt::AlignRight);
    }
    else row++;

    // Add Player Gold
    QLabel *label_player_gold = new QLabel;
    make_standard_label(label_player_gold, "GOLD:", TERM_GOLD);
    label_player_gold->setObjectName("GOLD_LABEL");
    label_player_gold->setToolTip(get_help_topic("character_info", "Gold"));
    QLabel *player_gold = new QLabel;
    make_standard_label(player_gold, " ", TERM_BLUE);
    player_gold->setObjectName("PLYR_Gold");
    return_layout->addWidget(label_player_gold, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_gold, row++, col+1, Qt::AlignRight);

    QLabel *filler = new QLabel("  ");
    return_layout->addWidget(filler, 0, col + 2);

}

void char_basic_data(QGridLayout *return_layout)
{
    int row = 0;
    int col = 0;

    // Add age
    QLabel *label_player_age = new QLabel;
    make_standard_label(label_player_age, "AGE:", TERM_DARK);
    label_player_age->setObjectName("AGE_LABEL");
    label_player_age->setToolTip(get_help_topic("character_info", "Player Age"));
    QLabel *player_age = new QLabel;
    make_standard_label(player_age, " ", TERM_BLUE);
    player_age->setObjectName("PLYR_Age");
    return_layout->addWidget(label_player_age, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_age, row++, col+1, Qt::AlignRight);

    // Add Height
    QLabel *label_player_height = new QLabel;
    make_standard_label(label_player_height, "HEIGHT:", TERM_DARK);
    label_player_height->setObjectName("HEIGHT_LABEL");
    label_player_height->setToolTip(get_help_topic("character_info", "Player Height"));
    QLabel *player_height = new QLabel;
    make_standard_label(player_height, " ", TERM_BLUE);
    player_height->setObjectName("PLYR_Height");
    return_layout->addWidget(label_player_height, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_height, row++, col+1, Qt::AlignRight);

    // Add Weight
    QLabel *label_player_weight = new QLabel;
    make_standard_label(label_player_weight, "WEIGHT:", TERM_DARK);
    label_player_weight->setObjectName("WEIGHT_LABEL");
    label_player_weight->setToolTip(get_help_topic("character_info", "Player Weight"));
    QLabel *player_weight = new QLabel;
    make_standard_label(player_weight, " ", TERM_BLUE);
    player_weight->setObjectName("PLYR_Weight");
    return_layout->addWidget(label_player_weight, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_weight, row++, col+1, Qt::AlignRight);

    // Add Social Class
    QLabel *label_player_sc = new QLabel;
    make_standard_label(label_player_sc, "SOCIAL CLASS:", TERM_DARK);
    label_player_sc->setObjectName("SC_LABEL");
    label_player_sc->setToolTip(get_help_topic("character_info", "Social Class"));
    QLabel *player_sc = new QLabel;
    make_standard_label(player_sc, " ", TERM_BLUE);
    player_sc->setObjectName("PLYR_SC");
    return_layout->addWidget(label_player_sc, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_sc, row++, col+1, Qt::AlignRight);

    // Add Game Turn
    QLabel *label_player_gturn = new QLabel;
    make_standard_label(label_player_gturn, "GAME TURN:", TERM_DARK);
    label_player_gturn->setObjectName("GAME_TURN_LABEL");
    label_player_gturn->setToolTip(get_help_topic("character_info", "Game Turn"));
    QLabel *player_gturn = new QLabel;
    make_standard_label(player_gturn, " ", TERM_BLUE);
    player_gturn->setObjectName("TURN_Game");
    return_layout->addWidget(label_player_gturn, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_gturn, row++, col+1, Qt::AlignRight);

    // Add Player Turn
    QLabel *label_player_pturn = new QLabel;
    make_standard_label(label_player_pturn, "PLAYER TURN:", TERM_DARK);
    label_player_pturn->setObjectName("PLAYER_TURN_LABEL");
    label_player_pturn->setToolTip(get_help_topic("character_info", "Player Turn"));
    QLabel *player_pturn = new QLabel;
    make_standard_label(player_pturn, " ", TERM_BLUE);
    player_pturn->setObjectName("TURN_Player");
    return_layout->addWidget(label_player_pturn, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_pturn, row++, col+1, Qt::AlignRight);

    // Add current Depth
    QLabel *label_player_cur_depth = new QLabel;
    make_standard_label(label_player_cur_depth, "CUR DEPTH:", TERM_DARK);
    label_player_cur_depth->setObjectName("DEPTH_CUR_LABEL");
    label_player_cur_depth->setToolTip(get_help_topic("character_info", "Current Depth"));
    QLabel *player_cur_depth = new QLabel;
    make_standard_label(player_cur_depth, " ", TERM_BLUE);
    player_cur_depth->setObjectName("DEPTH_Cur");
    return_layout->addWidget(label_player_cur_depth, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_cur_depth, row++, col+1, Qt::AlignRight);

    // Add Max Depth
    QLabel *label_player_max_depth = new QLabel;
    make_standard_label(label_player_max_depth, "MAX DEPTH:", TERM_DARK);
    label_player_max_depth->setObjectName("DEPTH_MAX_LABEL");
    label_player_max_depth->setToolTip(get_help_topic("character_info", "Max Depth"));
    QLabel *player_max_depth = new QLabel;
    make_standard_label(player_max_depth, " ", TERM_BLUE);
    player_max_depth->setObjectName("DEPTH_Max");
    return_layout->addWidget(label_player_max_depth, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_max_depth, row++, col+1, Qt::AlignRight);

    //Infravision
    QLabel *label_player_infra = new QLabel;
    make_standard_label(label_player_infra, "INFRAVISION:", TERM_DARK);
    label_player_infra->setObjectName("INFRA_LABEL");
    label_player_infra->setToolTip(get_help_topic("character_info", "Infravision"));
    QLabel *player_infra = new QLabel;
    make_standard_label(player_infra, " ", TERM_BLUE);
    player_infra->setObjectName("PLYR_Infra");
    return_layout->addWidget(label_player_infra, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_infra, row++, col+1, Qt::AlignRight);

    QLabel *filler = new QLabel("  ");
    return_layout->addWidget(filler, 0, col + 2);
}

void char_game_info(QGridLayout *return_layout)
{
    int row = 0;
    int col = 0;

    // Add Character Level
    QLabel *label_player_lev = new QLabel;
    make_standard_label(label_player_lev, "CHAR. LEVEL:", TERM_DARK);
    label_player_lev->setObjectName("LEVEL_LABEL");
    label_player_lev->setToolTip(get_help_topic("character_info", "Player Level"));
    QLabel *player_lev = new QLabel;
    make_standard_label(player_lev, " ", TERM_BLUE);
    player_lev->setObjectName("PLYR_Level");
    return_layout->addWidget(label_player_lev, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_lev, row++, col+1, Qt::AlignRight);

    // Add Player Experience
    QLabel *label_player_exp = new QLabel;
    make_standard_label(label_player_exp, "EXPERIENCE:", TERM_DARK);
    label_player_exp->setObjectName("CUR_EXP_LABEL");
    label_player_exp->setToolTip(get_help_topic("character_info", "Player Current Experience"));
    QLabel *player_exp = new QLabel;
    player_exp->setObjectName("PLYR_Cur_Exp");
    make_standard_label(player_exp, " ", TERM_BLUE);
    return_layout->addWidget(label_player_exp, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_exp, row++, col+1, Qt::AlignRight);

    // Add Player Maximum Experience
    QLabel *label_player_max_exp = new QLabel;
    make_standard_label(label_player_max_exp, "MAX EXP:", TERM_DARK);
    label_player_max_exp->setObjectName("MAX_EXP_LABEL");
    label_player_max_exp->setToolTip(get_help_topic("character_info", "Player Maximum Experience"));
    QLabel *player_max_exp = new QLabel;
    player_max_exp->setObjectName("PLYR_Max_Exp");
    make_standard_label(player_max_exp, " ", TERM_BLUE);
    return_layout->addWidget(label_player_max_exp, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_max_exp, row++, col+1, Qt::AlignRight);

    // Add Experience to Advance
    if (p_ptr->lev < z_info->max_level)
    {
        QLabel *label_player_exp_adv = new QLabel;
        make_standard_label(label_player_exp_adv, "ADVANCE EXP:", TERM_DARK);
        label_player_exp_adv->setObjectName("ADVANCE_LABEL");
        label_player_exp_adv->setToolTip(get_help_topic("character_info", "Player Experience Advance"));
        QLabel *player_exp_adv = new QLabel;
        make_standard_label(player_exp_adv, " ", TERM_BLUE);
        player_exp_adv->setObjectName("PLYR_Advance");
        return_layout->addWidget(label_player_exp_adv, row, col, Qt::AlignLeft);
        return_layout->addWidget(player_exp_adv, row++, col+1, Qt::AlignRight);
    }
    else row++;

    // Add player score
    QLabel *label_player_score = new QLabel;
    make_standard_label(label_player_score, "SCORE:", TERM_DARK);
    label_player_score->setObjectName("SCORE_LABEL");
    label_player_score->setToolTip(get_help_topic("character_info", "Player Score"));
    QLabel *player_score = new QLabel;
    make_standard_label(player_score, " ", TERM_BLUE);
    player_score->setObjectName("PLYR_Score");
    return_layout->addWidget(label_player_score, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_score, row++, col+1, Qt::AlignRight);

    // Add Burden
    QLabel *label_player_burden = new QLabel;
    make_standard_label(label_player_burden, "BURDEN:", TERM_DARK);
    label_player_burden->setObjectName("BURDEN_CUR_LABEL");
    label_player_burden->setToolTip(get_help_topic("character_info", "Player Burden"));
    QLabel *player_burden = new QLabel;
    make_standard_label(player_burden, " ", TERM_BLUE);
    player_burden->setObjectName("BURDEN_Cur");
    return_layout->addWidget(label_player_burden, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_burden, row++, col+1, Qt::AlignRight);

    // Add Max Burden
    QLabel *label_player_burden_max = new QLabel;
    make_standard_label(label_player_burden_max, "MAX WEIGHT:", TERM_DARK);
    label_player_burden_max->setObjectName("BURDEN_MAX_LABEL");
    label_player_burden_max->setToolTip(get_help_topic("character_info", "Max Burden"));
    QLabel *player_burden_max = new QLabel;
    make_standard_label(player_burden_max, " ", TERM_BLUE);
    player_burden_max->setObjectName("BURDEN_Max");
    return_layout->addWidget(label_player_burden_max, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_burden_max, row++, col+1, Qt::AlignRight);

    // Add Burden %
    QLabel *label_player_burden_pct = new QLabel;
    make_standard_label(label_player_burden_pct, "% BURDEN:", TERM_DARK);
    label_player_burden_pct->setObjectName("BURDEN_PERCENT_LABEL");
    label_player_burden_pct->setToolTip(get_help_topic("character_info", "Percent Burden"));
    QLabel *player_burden_pct = new QLabel;
    make_standard_label(player_burden_pct, " ", TERM_BLUE);
    player_burden_pct->setObjectName("BURDEN_Percent");
    return_layout->addWidget(label_player_burden_pct, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_burden_pct, row++, col+1, Qt::AlignRight);

    QLabel *filler = new QLabel("  ");
    return_layout->addWidget(filler, 0, col + 2);
}

void char_stat_info(QGridLayout *stat_layout)
{
    // add the headers
    byte row = 0;
    byte col = 0;

    QLabel *stat_header = new QLabel();
    make_standard_label(stat_header, "STAT   ", TERM_DARK);
    stat_header->setObjectName("st_label_X");
    QLabel *self_header = new QLabel();
    make_standard_label(self_header, " SELF ", TERM_DARK);
    self_header->setObjectName("st_base_X");
    QLabel *equip_adj_header = new QLabel();
    make_standard_label(equip_adj_header, "  EA ", TERM_DARK);
    equip_adj_header->setObjectName("st_equip_X");
    equip_adj_header->setToolTip("Stat adjustments due to player equipment");
    QLabel *total_stat_header = new QLabel();
    make_standard_label(total_stat_header, "  TOTAL STAT", TERM_DARK);
    total_stat_header->setObjectName("st_total_X");
    stat_layout->addWidget(stat_header, row, col++, Qt::AlignLeft);
    stat_layout->addWidget(self_header, row, col++, Qt::AlignLeft);
    if (birth_maximize)
    {
        QLabel *race_adj_header = new QLabel();
        make_standard_label(race_adj_header, "  RA ", TERM_DARK);
        race_adj_header->setObjectName("st_race_X");
        race_adj_header->setToolTip("Stat adjustments due to player race");
        QLabel *class_adj_header = new QLabel();
        make_standard_label(class_adj_header, "  CA ", TERM_DARK);
        class_adj_header->setObjectName("st_class_X");
        class_adj_header->setToolTip("Stat adjustments due to player class");
        stat_layout->addWidget(race_adj_header, row, col++, Qt::AlignRight);
        stat_layout->addWidget(class_adj_header, row, col++, Qt::AlignRight);
    }
    stat_layout->addWidget(equip_adj_header, row, col++, Qt::AlignRight);
    if (!birth_no_quests)
    {
        QLabel *reward_adj_header = new QLabel();
        make_standard_label(reward_adj_header, " QA ", TERM_DARK);
        reward_adj_header->setObjectName("st_quest_X");
        reward_adj_header->setToolTip("Stat adjustments due to quest rewards");
        stat_layout->addWidget(reward_adj_header, row, col++, Qt::AlignRight);
    }
    stat_layout->addWidget(total_stat_header, row, col++, Qt::AlignLeft);

    row++;

    for (int i = 0; i < A_MAX; i++)
    {
        col = 0;

        // Stat label
        QLabel *stat_label = new QLabel();
        make_standard_label(stat_label, stat_names[i], TERM_DARK);
        stat_label->setObjectName(QString("st_label_%1") .arg(i));
        stat_label->setToolTip(stat_entry(i));
        stat_layout->addWidget(stat_label, row, col++, Qt::AlignLeft);

        QLabel *self_label = new QLabel();
        make_standard_label(self_label, " ", TERM_BLUE);
        self_label->setObjectName(QString("st_base_%1") .arg(i));
        stat_layout->addWidget(self_label, row, col++, Qt::AlignLeft);

        if (birth_maximize)
        {
            QLabel *race_adj = new QLabel();
            make_standard_label(race_adj, " ", TERM_BLUE);
            race_adj->setObjectName(QString("st_race_%1") .arg(i));
            stat_layout->addWidget(race_adj, row, col++, Qt::AlignRight);

            QLabel *class_adj = new QLabel();
            make_standard_label(class_adj, " ", TERM_BLUE);
            class_adj->setObjectName(QString("st_class_%1") .arg(i));
            stat_layout->addWidget(class_adj, row, col++, Qt::AlignRight);
        }

        QLabel *equip_adj = new QLabel();
        make_standard_label(equip_adj, " ", TERM_BLUE);
        equip_adj->setObjectName(QString("st_equip_%1") .arg(i));
        stat_layout->addWidget(equip_adj, row, col++, Qt::AlignRight);

        if (!birth_no_quests)
        {
            QLabel *quest_adj = new QLabel();
            make_standard_label(quest_adj, " ", TERM_BLUE);
            quest_adj->setObjectName(QString("st_quest_%1") .arg(i));
            stat_layout->addWidget(quest_adj, row, col++, Qt::AlignRight);
        }

        QLabel *stat_total = new QLabel();
        make_standard_label(stat_total, " ", TERM_BLUE);
        stat_total->setObjectName(QString("st_total_%1") .arg(i));
        stat_layout->addWidget(stat_total, row, col++, Qt::AlignLeft);

        //Display reduced stat if necessary
        QLabel *stat_reduce = new QLabel();
        make_standard_label(stat_reduce, " ", TERM_RED);
        stat_reduce->setObjectName(QString("st_reduce_%1") .arg(i));
        stat_layout->addWidget(stat_reduce, row++, col++, Qt::AlignRight);
    }
}

void char_combat_info(QGridLayout *return_layout)
{
    int row = 0;
    int col = 0;

    object_type object_type_body;
    object_type_body.object_wipe();

    // Add Speed
    QLabel *label_player_speed = new QLabel;
    make_standard_label(label_player_speed, "SPEED:", TERM_DARK);
    label_player_speed->setObjectName("SPEED_LABEL");
    label_player_speed->setToolTip(get_help_topic("character_info", "Speed"));
    QLabel *player_speed = new QLabel;
    make_standard_label(player_speed, " ", TERM_BLUE);
    player_speed->setObjectName("BASE_Speed");
    return_layout->addWidget(label_player_speed, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_speed, row++, col+1, Qt::AlignRight);

    // Add armor class
    QLabel *label_player_armor = new QLabel;
    make_standard_label(label_player_armor, "ARMOR:", TERM_DARK);
    label_player_armor->setObjectName("AC_LABEL");
    label_player_armor->setToolTip(get_help_topic("character_info", "Armor Class"));
    QLabel *player_armor = new QLabel;
    make_standard_label(player_armor, " ", TERM_BLUE);
    player_armor->setObjectName("PLYR_AC");
    return_layout->addWidget(label_player_armor, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_armor, row++, col+1, Qt::AlignRight);

    // Melee Weapon Stats
    QLabel *label_player_melee = new QLabel;
    make_standard_label(label_player_melee, "MELEE:", TERM_DARK);
    label_player_melee->setObjectName("MELEE_LABEL");
    label_player_melee->setToolTip(get_help_topic("character_info", "Melee"));
    QLabel *player_melee = new QLabel;
    make_standard_label(player_melee, " ", TERM_BLUE);
    player_melee->setObjectName("PLYR_MELEE");
    return_layout->addWidget(label_player_melee, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_melee, row++, col+1, Qt::AlignRight);

    // Add critical hit %
    QLabel *label_player_crit_hit = new QLabel;
    make_standard_label(label_player_crit_hit, "CRIT. HIT %:", TERM_DARK);
    label_player_crit_hit->setObjectName("CRIT_HIT_LABEL");
    label_player_crit_hit->setToolTip(get_help_topic("character_info", "Critical Hit"));
    QLabel *player_crit_hit = new QLabel;
    make_standard_label(player_crit_hit, " ", TERM_BLUE);
    player_crit_hit->setObjectName("HIT_Critical");
    return_layout->addWidget(label_player_crit_hit, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_crit_hit, row++, col+1, Qt::AlignRight);

    //Shooting weapon stats
    QLabel *label_player_shoot = new QLabel;
    make_standard_label(label_player_shoot, "SHOOT:", TERM_DARK);
    label_player_shoot->setObjectName("SHOOTING_LABEL");
    label_player_shoot->setToolTip(get_help_topic("character_info", "Shooting Stats"));
    QLabel *player_shoot = new QLabel;
    make_standard_label(player_shoot, " ", TERM_BLUE);
    player_shoot->setObjectName("SHOOTING_Stats");
    return_layout->addWidget(label_player_shoot, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_shoot, row++, col+1, Qt::AlignRight);

    // Searching frequency - frequency is inverted
    QLabel *label_player_search_freq = new QLabel;
    make_standard_label(label_player_search_freq, "SEARCH FREQ:", TERM_DARK);
    label_player_search_freq->setObjectName("SRCH_FREQ_LABEL");
    label_player_search_freq->setToolTip(get_help_topic("character_info", "Search Frequency"));
    QLabel *player_search_freq = new QLabel;
    make_standard_label(player_search_freq, " ", TERM_BLUE);
    player_search_freq->setObjectName("SEARCH_Freq");
    return_layout->addWidget(label_player_search_freq, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_search_freq, row++, col+1, Qt::AlignRight);

    // Searching chance
    QLabel *label_player_search_chance = new QLabel;
    make_standard_label(label_player_search_chance, "SEARCH CHANCE:", TERM_DARK);
    label_player_search_chance->setObjectName("SRCH_CHANCE_LABEL");
    label_player_search_chance->setToolTip(get_help_topic("character_info", "Search Chance"));
    QLabel *player_search_chance = new QLabel;
    make_standard_label(player_search_chance," ", TERM_BLUE);
    player_search_chance->setObjectName("SEARCH_Chance");
    return_layout->addWidget(label_player_search_chance, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_search_chance, row++, col+1, Qt::AlignRight);

    //Digging
    QLabel *label_player_dig = new QLabel;
    make_standard_label(label_player_dig, "TUNNEL:", TERM_DARK);
    label_player_dig->setObjectName("TUNNEL_LABEL");
    label_player_dig->setToolTip(get_help_topic("character_info", "Tunneling"));
    QLabel *player_dig = new QLabel;
    make_standard_label(player_dig, " ", TERM_BLUE);
    player_dig->setObjectName("PLYR_Tunnel");
    return_layout->addWidget(label_player_dig, row, col, Qt::AlignLeft);
    return_layout->addWidget(player_dig, row++, col+1, Qt::AlignRight);

    QLabel *filler = new QLabel("  ");
    return_layout->addWidget(filler, 0, col + 2);
}

void char_ability_info(QGridLayout *return_layout)
{
    int row = 0;
    int col = 0;

    // Add Speed
    QLabel *label_player_speed = new QLabel;
    make_standard_label(label_player_speed, "SPEED:", TERM_DARK);
    label_player_speed->setToolTip(get_help_topic("character_info", "Speed"));
    return_layout->addWidget(label_player_speed, row, col, Qt::AlignLeft);
    QLabel *player_speed = new QLabel;
    player_speed->setObjectName("PLYR_Speed");
    return_layout->addWidget(player_speed, row++, col+1);

    // Add Saving Throw
    QLabel *label_player_save = new QLabel;
    make_standard_label(label_player_save, "SAVING THROW:", TERM_DARK);
    label_player_save->setToolTip(get_help_topic("character_info", "Saving Throw"));
    return_layout->addWidget(label_player_save, row, col, Qt::AlignLeft);
    QLabel *player_save = new QLabel;
    player_save->setObjectName("PLYR_Save");
    return_layout->addWidget(player_save, row++, col+1);

    // Add Stealth - note special handling since stealth is inverted
    QLabel *label_player_stealth = new QLabel;
    make_standard_label(label_player_stealth, "STEALTH:", TERM_DARK);
    label_player_stealth->setToolTip(get_help_topic("character_info", "Stealth"));
    return_layout->addWidget(label_player_stealth, row, col, Qt::AlignLeft);
    QLabel *player_stealth = new QLabel;
    player_stealth->setObjectName("PLYR_Stealth");
    return_layout->addWidget(player_stealth, row++, col+1);

    // Add Fighting ability
    QLabel *label_player_fight = new QLabel;
    make_standard_label(label_player_fight, "FIGHTING:", TERM_DARK);
    label_player_fight->setToolTip(get_help_topic("character_info", "Fighting Ability"));
    return_layout->addWidget(label_player_fight, row, col, Qt::AlignLeft);
    QLabel *player_fight = new QLabel;
    player_fight->setObjectName("PLYR_Fight");
    return_layout->addWidget(player_fight, row++, col+1);

    // Add bow ability
    QLabel *label_player_bow = new QLabel;
    make_standard_label(label_player_bow, "SHOOTING:", TERM_DARK);
    label_player_bow->setToolTip(get_help_topic("character_info", "Shooting Ability"));
    return_layout->addWidget(label_player_bow, row, col, Qt::AlignLeft);
    QLabel *player_bow = new QLabel;
    player_bow->setObjectName("PLYR_Bow");
    return_layout->addWidget(player_bow, row++, col+1);

    // Add throwing ability
    QLabel *label_player_throw = new QLabel;
    make_standard_label(label_player_throw, "THROWING:", TERM_DARK);
    label_player_throw->setToolTip(get_help_topic("character_info", "Throwing Ability"));
    return_layout->addWidget(label_player_throw, row, col, Qt::AlignLeft);
    QLabel *player_throw = new QLabel;
    player_throw->setObjectName("PLYR_Disarm");
    return_layout->addWidget(player_throw, row++, col+1);

    // Add disarming ability
    QLabel *label_player_disarm = new QLabel;
    make_standard_label(label_player_disarm, "DISARMING:", TERM_DARK);
    label_player_disarm->setToolTip(get_help_topic("character_info", "Disarming"));
    return_layout->addWidget(label_player_disarm, row, col, Qt::AlignLeft);
    QLabel *player_disarm = new QLabel;
    player_disarm->setObjectName("PLYR_Disarm");
    return_layout->addWidget(player_disarm, row++, col+1);

    // Add magic device
    QLabel *label_player_magic = new QLabel;
    make_standard_label(label_player_magic, "MAGIC DEVICE:", TERM_DARK);
    label_player_magic->setToolTip(get_help_topic("character_info", "Magic Device"));
    return_layout->addWidget(label_player_magic, row, col, Qt::AlignLeft);
    QLabel *player_magic = new QLabel;
    player_magic->setObjectName("PLYR_Magic");
    return_layout->addWidget(player_magic, row++, col+1);

    QLabel *filler = new QLabel(" ");
    return_layout->addWidget(filler, 0, col + 2);
}

void update_equip_flags(QList<QLabel *> equippy_list, QList<QLabel *> flag_list, QList<QLabel *> label_list)
{
    update_equippy_labels(equippy_list);

    u32b f1, f2, f3, fn;

    /* Extract the player flags */
    player_flags(&f1, &f2, &f3, &fn);

    bool did_resist[TABLES_MAX_ENTRIES];
    bool did_temp_resist[TABLES_MAX_ENTRIES];
    bool did_immunity[TABLES_MAX_ENTRIES];

    for (int x = 0; x < TABLES_MAX_ENTRIES; x++)
    {
        did_resist[x] = FALSE;
        did_temp_resist[x] = FALSE;
        did_immunity[x] = FALSE;
    }

    for (int x = 0; x < flag_list.size(); x++)
    {
        QLabel *this_lbl = flag_list.at(x);

        QString this_name = this_lbl->objectName();

        this_name.remove("obj_flag_info_");

        this_name.replace(QString("_"), QString(" "));

        QTextStream line_string (&this_name);

        int flag_set, row, column;

        line_string >> flag_set >> row >> column;

        player_flag_record *pfr_ptr;
        if (flag_set == FLAGS_RESIST) pfr_ptr = &player_resist_table[row];
        else if (flag_set == FLAGS_ABILITY) pfr_ptr = &player_abilities_table[row];
        else pfr_ptr = &player_nativity_table[row];  // (flag_set == FLAGS_NATIVITY)

        if (column < NUM_INVEN_SLOTS)
        {
            int inven_slot = column + INVEN_WIELD;

            object_type *o_ptr = &inventory[inven_slot];

            bool has_flag = FALSE;

            // Special code for Moria
            if (game_mode == GAME_NPPMORIA)
            {
                if (!pfr_ptr->moria_flag) continue;
                if (inven_slot == INVEN_SWAP_WEAPON)
                {
                    this_lbl->setText("");
                    continue;
                }
            }

            if (!o_ptr->k_idx)
            {
                this_lbl->setText(color_string(".", TERM_DARK));
                continue;
            }

            // First, check for immunity
            if (pfr_ptr->extra_flag)
            {
                if (o_ptr->known_obj_flags_2 & (pfr_ptr->extra_flag))
                {
                    this_lbl->setText(color_string("I", TERM_BLUE));
                    did_immunity[row] = TRUE;
                    continue;
                }
            }
            if (pfr_ptr->set == 1)
            {
                if ((o_ptr->known_obj_flags_1 & (pfr_ptr->this_flag)) == pfr_ptr->this_flag) has_flag = TRUE;
            }

            else if (pfr_ptr->set == 2)
            {
                if ((o_ptr->known_obj_flags_2 & (pfr_ptr->this_flag)) == pfr_ptr->this_flag) has_flag = TRUE;
            }

            else if (pfr_ptr->set == 3)
            {
                // Hack - special handling for cursed items
                if (pfr_ptr->set & (TR3_CURSE_ALL))
                {
                    if (o_ptr->ident & (IDENT_CURSED)) has_flag = TRUE;
                }
                else if ((o_ptr->known_obj_flags_3 & (pfr_ptr->this_flag)) == pfr_ptr->this_flag) has_flag = TRUE;
            }
            else if (pfr_ptr->set == 4)
            {
                if ((o_ptr->known_obj_flags_native & (pfr_ptr->this_flag)) == pfr_ptr->this_flag) has_flag = TRUE;
            }
            if (has_flag)
            {
                this_lbl->setText(color_string("+", TERM_GREEN));
                did_resist[row] = TRUE;
            }
            else this_lbl->setText(color_string(".", TERM_DARK));
        }
        // The player inate flags
        else if (column == NUM_INVEN_SLOTS)
        {
            bool player_has_flag = FALSE;
            bool player_has_immunity = FALSE;

            if (pfr_ptr->set == 1)
            {
                if ((f1 & (pfr_ptr->this_flag)) == pfr_ptr->this_flag) player_has_flag = TRUE;
                if ((f1 & (pfr_ptr->extra_flag)))  player_has_immunity = TRUE;
            }
            if (pfr_ptr->set == 2)
            {
                if ((f2 & (pfr_ptr->this_flag)) == pfr_ptr->this_flag) player_has_flag = TRUE;
                if ((f2 & (pfr_ptr->extra_flag)))  player_has_immunity = TRUE;
            }
            if (pfr_ptr->set == 3)
            {
                if ((f3 & (pfr_ptr->this_flag)) == pfr_ptr->this_flag) player_has_flag = TRUE;
                if ((f3 & (pfr_ptr->extra_flag)))  player_has_immunity = TRUE;
            }
            if (pfr_ptr->set == 4)
            {
                if ((fn & (pfr_ptr->this_flag)) == pfr_ptr->this_flag) player_has_flag = TRUE;
                if ((fn & (pfr_ptr->extra_flag)))  player_has_immunity = TRUE;

                // Special hack for boiling mud and boiling water
                if (flag_set == FLAGS_NATIVITY)
                {
                    if (pfr_ptr->this_flag == ELEMENT_BMUD)
                    {
                        if (p_ptr->state.native_boiling_mud) player_has_flag = TRUE;
                        else player_has_flag = FALSE;
                    }
                    else if (pfr_ptr->this_flag == ELEMENT_BWATER)
                    {
                        if (p_ptr->state.native_boiling_water) player_has_flag = TRUE;
                        else player_has_flag = FALSE;
                    }
                }
            }
            if (player_has_immunity)
            {
                this_lbl->setText(color_string("<b>I</b>", TERM_BLUE));
                did_immunity[row] = TRUE;
            }
            else if (player_has_flag)
            {
                this_lbl->setText(color_string("<b>+</b>", TERM_GREEN));
                did_resist[row] = TRUE;
            }
            else this_lbl->setText(color_string(".", TERM_DARK));
        }
        // Temporary resists
        else if (column > NUM_INVEN_SLOTS)
        {
            QString this_tool_tip;
            this_tool_tip.clear();
            bool this_temp_resist = FALSE;
            if (flag_set == FLAGS_RESIST)
            {
                if (pfr_ptr->this_flag == TR2_RES_ACID)
                {
                    if (p_ptr->timed[TMD_OPP_ACID] && !redundant_timed_event(TMD_OPP_ACID))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You temporarily resist fire.";
                    }
                }
                else if (pfr_ptr->this_flag == TR2_RES_ELEC)
                {
                    if (p_ptr->timed[TMD_OPP_ELEC] && !redundant_timed_event(TMD_OPP_ELEC))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You temporarily resist lightning.";
                    }
                }
                else if (pfr_ptr->this_flag == TR2_RES_FIRE)
                {
                    if (p_ptr->timed[TMD_OPP_FIRE] && !redundant_timed_event(TMD_OPP_FIRE))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You temporarily resist fire.";
                    }
                }
                else if (pfr_ptr->this_flag == TR2_RES_COLD)
                {
                    if (p_ptr->timed[TMD_OPP_COLD] && !redundant_timed_event(TMD_OPP_COLD))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You temporarily resist cold.";
                    }
                }
                else if (pfr_ptr->this_flag == TR2_RES_POIS)
                {
                    if (p_ptr->timed[TMD_OPP_POIS] && !redundant_timed_event(TMD_OPP_POIS))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You temporarily resist poison.";
                    }
                }
                else if (pfr_ptr->this_flag == TR2_RES_FEAR)
                {
                    if (p_ptr->timed[TMD_HERO] || p_ptr->timed[TMD_BERSERK])
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You are temporarily immune to fear.";
                    }
                }
            }
            else if (flag_set == FLAGS_ABILITY)
            {
                if (pfr_ptr->this_flag == TR3_SEE_INVIS)
                {
                    if ((p_ptr->timed[TMD_SINVIS])  && !redundant_timed_event(TMD_SINVIS))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You can temporarily see invisible creatures.";
                    }
                }
            }
            else if (flag_set == FLAGS_NATIVITY)
            {
                if (pfr_ptr->this_flag == TN1_NATIVE_LAVA)
                {
                    if ((p_ptr->timed[TMD_NAT_LAVA]) && !redundant_timed_event(TMD_NAT_LAVA))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You are temporarily native to lava terrains.";
                    }
                }
                else if (pfr_ptr->this_flag == TN1_NATIVE_OIL)
                {
                    if ((p_ptr->timed[TMD_NAT_OIL]) && !redundant_timed_event(TMD_NAT_OIL))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You are temporarily native to oily terrains.";
                    }
                }
                else if (pfr_ptr->this_flag == TN1_NATIVE_SAND)
                {
                    if ((p_ptr->timed[TMD_NAT_SAND]) && !redundant_timed_event(TMD_NAT_SAND))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You are temporarily native to sandy terrains.";
                    }
                }
                else if (pfr_ptr->this_flag == TN1_NATIVE_FOREST)
                {
                    if ((p_ptr->timed[TMD_NAT_TREE]) && !redundant_timed_event(TMD_NAT_TREE))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You are temporarily native to forest terrains.";
                    }
                }
                else if (pfr_ptr->this_flag == TN1_NATIVE_WATER)
                {
                    if ((p_ptr->timed[TMD_NAT_WATER]) && !redundant_timed_event(TMD_NAT_WATER))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You are temporarily native to watery terrains.";
                    }
                }
                else if (pfr_ptr->this_flag == TN1_NATIVE_MUD)
                {
                    if ((p_ptr->timed[TMD_NAT_MUD]) && !redundant_timed_event(TMD_NAT_MUD))
                    {
                        this_temp_resist = TRUE;
                        this_tool_tip = "You are temporarily native to muddy terrains.";
                    }
                }
            }
            if (this_temp_resist)
            {
                this_lbl->setText(color_string("<b>+</b>", TERM_PURPLE));
                did_temp_resist[row] = TRUE;
            }
            else this_lbl->setText(color_string(".", TERM_DARK));
            this_lbl->setToolTip(this_tool_tip);
        }
    }

    // Now update the label colors
    for (int x = 0; x < label_list.size(); x++)
    {
        QLabel *this_lbl = label_list.at(x);

        QString this_name = this_lbl->objectName();

        this_name.remove("line_label_");

        this_name.replace(QString("_"), QString(" "));

        QTextStream line_string (&this_name);

        int flag_set, row;

        line_string >> flag_set >> row;

        player_flag_record *pfr_ptr;
        if (flag_set == FLAGS_RESIST) pfr_ptr = &player_resist_table[row];
        else if (flag_set == FLAGS_ABILITY) pfr_ptr = &player_abilities_table[row];
        else pfr_ptr = &player_nativity_table[row];  // (flag_set == FLAGS_NATIVITY)

        QString label_text = html_string_to_plain_text(this_lbl->text());

        int attr = TERM_DARK;
        if (did_immunity[row]) attr = TERM_BLUE;
        else if (did_resist[row] && did_temp_resist[row]) attr = TERM_PURPLE;
        else if (did_resist[row] || did_temp_resist[row]) attr = TERM_GREEN;

        // Hack different color for bad flags
        if (pfr_ptr->bad_flag)
        {
            if (attr != TERM_DARK) attr = TERM_RED;
        }

        this_lbl->setText(QString("<b>%1</b>") .arg(color_string(label_text, attr)));
    }
}

// Prints out the various equipment flags, depending on the specified flag set
void equip_flag_info(QWidget *this_widget, QGridLayout *return_layout, int flag_set, QFont this_font)
{
    int row = 0;

    int x = 0;

    QList<QLabel *> list_flags;
    QList<QLabel *> list_equippy;
    QList<QLabel *> list_labels;

    list_flags.clear();
    list_equippy.clear();
    list_labels.clear();

    if (flag_set == FLAGS_NATIVITY)
    {
        QLabel *nativity_to = new QLabel();
        make_standard_label(nativity_to, "Nativity To:", TERM_BLUE, this_font);
        nativity_to->setObjectName("preserve");
        return_layout->addWidget(nativity_to, row, 0, Qt::AlignLeft);
    }

    draw_equippy_labels(return_layout, row, 0, TRUE, TRUE, this_font);

    // Hack - compile the equippy list
    QList<QLabel *> label_list = this_widget->findChildren<QLabel *>();

    for (int i = 0; i < label_list.size(); i++)
    {
        QLabel *this_lbl = label_list.at(i);

        QString this_name = this_lbl->objectName();

        //Not a named label
        if (!this_name.length()) continue;

        if (!this_name.contains("equippy_")) continue;

        list_equippy.append(this_lbl);
    }

    while (TRUE)
    {
        player_flag_record *pfr_ptr;
        if (flag_set == FLAGS_RESIST) pfr_ptr = &player_resist_table[x++];
        else if (flag_set == FLAGS_ABILITY) pfr_ptr = &player_abilities_table[x++];
        else pfr_ptr = &player_nativity_table[x++];  // (flag_set == FLAGS_NATIVITY)

        // We are done
        if (pfr_ptr->name.isNull()) break;

        // Just make the labels at this point
        row++;
        int col = 0;

        // If in Moria, make sure the flag is used.
        if (game_mode == GAME_NPPMORIA)
        {
            if (!pfr_ptr->moria_flag) continue;
        }

        QLabel *line_label = new QLabel;
        make_standard_label(line_label, pfr_ptr->name, TERM_GREEN, this_font);
        line_label->setObjectName(QString("line_label_%1_%2") .arg(flag_set) .arg(row-1));
        if (pfr_ptr->set == 2)
        {
            // Too messy to include in the charts
            if (pfr_ptr->this_flag == TR2_RES_ACID) line_label->setToolTip(get_help_topic("character_info", "Resist Acid"));
            else if (pfr_ptr->this_flag == TR2_RES_ELEC) line_label->setToolTip(get_help_topic("character_info", "Resist Electricity"));
            else if (pfr_ptr->this_flag == TR2_RES_FIRE) line_label->setToolTip(get_help_topic("character_info", "Resist Fire"));
            else if (pfr_ptr->this_flag == TR2_RES_COLD) line_label->setToolTip(get_help_topic("character_info", "Resist Cold"));
            else if (pfr_ptr->this_flag == TR2_RES_POIS) line_label->setToolTip(get_help_topic("character_info", "Resist Poison"));
            else if (pfr_ptr->this_flag == TR2_RES_BLIND) line_label->setToolTip(get_help_topic("character_info", "Resist Blindness"));
            else if (pfr_ptr->this_flag == TR2_RES_CONFU) line_label->setToolTip(get_help_topic("character_info", "Resist Confusion"));
            else if (pfr_ptr->this_flag == TR2_RES_NEXUS) line_label->setToolTip(get_help_topic("character_info", "Resist Nexus"));
            else if (pfr_ptr->this_flag == TR2_RES_NETHR) line_label->setToolTip(get_help_topic("character_info", "Resist Nether"));
            else if (pfr_ptr->this_flag == TR2_RES_CHAOS) line_label->setToolTip(get_help_topic("character_info", "Resist Chaos"));
            else if (pfr_ptr->this_flag == TR2_RES_DISEN) line_label->setToolTip(get_help_topic("character_info", "Resist Disenchantment"));
            else if (pfr_ptr->this_flag == TR2_RES_SOUND) line_label->setToolTip(get_help_topic("character_info", "Resist Sound"));
            else if (pfr_ptr->this_flag == TR2_RES_SHARD) line_label->setToolTip(get_help_topic("character_info", "Resist Shards"));
            else if (pfr_ptr->this_flag == TR2_RES_LIGHT) line_label->setToolTip(get_help_topic("character_info", "Resist Light"));
            else if (pfr_ptr->this_flag == TR2_RES_DARK) line_label->setToolTip(get_help_topic("character_info", "Resist Darkness"));
            else if (pfr_ptr->this_flag == TR2_RES_FEAR) line_label->setToolTip(get_help_topic("character_info", "Resist Fear"));
        }
        else if (pfr_ptr->set == 3)
        {
            if (pfr_ptr->this_flag == TR3_SEE_INVIS) line_label->setToolTip(get_help_topic("character_info", "See Invisible"));
            else if (pfr_ptr->this_flag == TR3_FREE_ACT) line_label->setToolTip(get_help_topic("character_info", "Free Action"));
            else if (pfr_ptr->this_flag == TR3_TELEPATHY) line_label->setToolTip(get_help_topic("character_info", "Telepathy"));
            else if (pfr_ptr->this_flag == TR3_HOLD_LIFE) line_label->setToolTip(get_help_topic("character_info", "Hold Life"));
            else if (pfr_ptr->this_flag == TR3_LIGHT) line_label->setToolTip(get_help_topic("character_info", "Permanent Light"));
            else if (pfr_ptr->this_flag == TR3_REGEN) line_label->setToolTip(get_help_topic("character_info", "Regeneration"));
            else if (pfr_ptr->this_flag == TR3_SLOW_DIGEST) line_label->setToolTip(get_help_topic("character_info", "Slow Digestion"));
            else if (pfr_ptr->this_flag == TR3_FEATHER) line_label->setToolTip(get_help_topic("character_info", "Feather Falling"));
            else if (pfr_ptr->this_flag == TR3_TELEPORT) line_label->setToolTip(get_help_topic("character_info", "Teleportation"));
            else if (pfr_ptr->this_flag == TR3_AGGRAVATE) line_label->setToolTip(get_help_topic("character_info", "Aggravation"));
            else if (pfr_ptr->this_flag == TR3_CURSE_ALL) line_label->setToolTip(get_help_topic("character_info", "Cursed Items"));
            else if (pfr_ptr->this_flag == TR3_DRAIN_EXP) line_label->setToolTip(get_help_topic("character_info", "Drain Experience"));
        }
        else if (pfr_ptr->set == 4)
        {
            if (pfr_ptr->this_flag == TN1_NATIVE_LAVA) line_label->setToolTip(get_help_topic("character_info", "Native Lava"));
            else if (pfr_ptr->this_flag == TN1_NATIVE_ICE) line_label->setToolTip(get_help_topic("character_info", "Native Ice"));
            else if (pfr_ptr->this_flag == TN1_NATIVE_OIL) line_label->setToolTip(get_help_topic("character_info", "Native Oil"));
            else if (pfr_ptr->this_flag == TN1_NATIVE_FIRE) line_label->setToolTip(get_help_topic("character_info", "Native Fire"));
            else if (pfr_ptr->this_flag == TN1_NATIVE_SAND) line_label->setToolTip(get_help_topic("character_info", "Native Sand"));
            else if (pfr_ptr->this_flag == TN1_NATIVE_FOREST) line_label->setToolTip(get_help_topic("character_info", "Native Forest"));
            else if (pfr_ptr->this_flag == TN1_NATIVE_WATER) line_label->setToolTip(get_help_topic("character_info", "Native Water"));
            else if (pfr_ptr->this_flag == TN1_NATIVE_ACID) line_label->setToolTip(get_help_topic("character_info", "Native Acid"));
            else if (pfr_ptr->this_flag == TN1_NATIVE_MUD) line_label->setToolTip(get_help_topic("character_info", "Native Mud"));
            else if (pfr_ptr->this_flag == TN1_NATIVE_BMUD) line_label->setToolTip(get_help_topic("character_info", "Native Boiling Mud"));
            else if (pfr_ptr->this_flag == TN1_NATIVE_BWATER) line_label->setToolTip(get_help_topic("character_info", "Native Boiling Water"));
        }
        return_layout->addWidget(line_label, row, 0, Qt::AlignLeft);

        list_labels.append(line_label);

        for (int i = INVEN_WIELD; i < (INVEN_TOTAL); i++)
        {
            QLabel *this_label = new QLabel();
            make_standard_label(this_label, ".", TERM_DARK, this_font);
            this_label->setObjectName(QString("obj_flag_info_%1_%2_%3") .arg(flag_set) .arg(row-1) .arg(col++));
            return_layout->addWidget(this_label, row, col, Qt::AlignCenter);
            list_flags.append(this_label);
        }

        // Add player resists

        QLabel *player_label = new QLabel;
        make_standard_label(player_label, ".", TERM_DARK, this_font);
        player_label->setObjectName(QString("obj_flag_info_%1_%2_%3") .arg(flag_set) .arg(row-1) .arg(col++));
        return_layout->addWidget(player_label, row, col, Qt::AlignCenter);
        list_flags.append(player_label);


        // Add corresponding temporary resists
        QLabel *temp_label = new QLabel;
        make_standard_label(temp_label, ".", TERM_DARK, this_font);
        temp_label->setObjectName(QString("obj_flag_info_%1_%2_%3") .arg(flag_set) .arg(row-1) .arg(col++));
        return_layout->addWidget(temp_label, row, col, Qt::AlignCenter);
        list_flags.append(temp_label);
    }

    update_equip_flags(list_equippy, list_flags, list_labels);
}

void update_equip_modifiers(QList<QLabel *> equippy_list, QList<QLabel *> flag_list, QList<QLabel *> label_list)
{
    update_equippy_labels(equippy_list);

    u32b f1, f2, f3, fn;

    /* Extract the player flags */
    player_flags(&f1, &f2, &f3, &fn);

    bool has_modifier[TABLES_MAX_ENTRIES];
    bool has_sustain[TABLES_MAX_ENTRIES];
    int cumulative[TABLES_MAX_ENTRIES];

    for (int x = 0; x < TABLES_MAX_ENTRIES; x++)
    {
        has_modifier[x] = FALSE;
        has_sustain[x] = FALSE;
        cumulative[x] = 0;
    }

    for (int x = 0; x < flag_list.size(); x++)
    {
        QLabel *this_lbl = flag_list.at(x);

        QString this_name = this_lbl->objectName();

        this_name.remove("obj_mod_info_");

        this_name.replace(QString("_"), QString(" "));

        QTextStream line_string (&this_name);

        int row, column;

        line_string >> row >> column;

        player_flag_record *pfr_ptr = &player_pval_table[row];

        int inven_slot = column + INVEN_WIELD;

        object_type *o_ptr = &inventory[inven_slot];

        bool this_sustain = FALSE;
        bool this_modifier = FALSE;

        // Special code for Moria
        if (game_mode == GAME_NPPMORIA)
        {
            if (!pfr_ptr->moria_flag) continue;
            if (inven_slot == INVEN_SWAP_WEAPON)
            {
                this_lbl->setText("");
                continue;
            }
        }

        if (!o_ptr->k_idx)
        {
            this_lbl->setText(color_string(".", TERM_DARK));
            continue;
        }

        // First, check for sustain
        if (pfr_ptr->extra_flag)
        {
            if (o_ptr->known_obj_flags_2 & (pfr_ptr->extra_flag))
            {
                this_sustain = TRUE;
            }
        }

        // CHeck if this equipment has the applicable flag
        if (pfr_ptr->set == 1)
        {
            if (o_ptr->known_obj_flags_1 & (pfr_ptr->this_flag)) this_modifier = TRUE;
        }
        else if (pfr_ptr->set == 2)
        {
            if (o_ptr->known_obj_flags_2 & (pfr_ptr->this_flag)) this_modifier = TRUE;
        }
        else if (pfr_ptr->set == 3)
        {
            if (o_ptr->known_obj_flags_3 & (pfr_ptr->this_flag)) this_modifier = TRUE;
        }
        else if (pfr_ptr->set == 4)
        {
            if (o_ptr->known_obj_flags_native & (pfr_ptr->this_flag)) this_modifier = TRUE;
        }

        if (this_sustain) has_sustain[row] = TRUE;
        else if (this_modifier) has_modifier[row] = TRUE;
        // Nothing to mark.
        else
        {
            this_lbl->setText(color_string(".", TERM_DARK));
            continue;
        }

        int attr = TERM_DARK;
        QString pval_num = (QString("%1") .arg(o_ptr->pval));

        if (o_ptr->pval > 0 && this_modifier)
        {
            attr = TERM_GREEN;
            pval_num.prepend("+");
        }
        else if (o_ptr->pval < 0 && this_modifier) attr = TERM_RED;

        // Sustained stat
        if (this_sustain)
        {
            // Handle cases where the stat is sustained but not modified
            if (!this_modifier)
            {
                pval_num = "s";
            }

            if (o_ptr->pval < 0)    attr = TERM_ORANGE_PEEL;
            else if (o_ptr->pval > 0)      attr = TERM_BLUE;

            pval_num = (QString("<u>%1</u>") .arg(pval_num));

            this_lbl->setToolTip(QString("Your %1 is sustained.") .arg(stat_names_full[row-1]));
        }
        else this_lbl->setToolTip(QString(""));
        pval_num.prepend("<b>");
        pval_num.append("</b>");

        set_html_string_length(pval_num, 3, TRUE);
        this_lbl->setText(color_string(pval_num, attr));

        cumulative[row] += o_ptr->pval;
    }

    // Now update the label colors
    for (int x = 0; x < label_list.size(); x++)
    {
        QLabel *this_lbl = label_list.at(x);

        QString this_name = this_lbl->objectName();

        this_name.remove("line_label_");

        this_name.replace(QString("_"), QString(" "));

        QTextStream line_string (&this_name);

        int row;

        line_string >> row;

        QString label_text = html_string_to_plain_text(this_lbl->text());

        if (!has_modifier[row])
        {
            this_lbl->setText(color_string(label_text, TERM_DARK));
            continue;
        }

        player_flag_record *pfr_ptr = &player_pval_table[row];

        int attr = TERM_DARK;

        if (has_sustain[row])
        {
            if (cumulative[row] < 0) attr = TERM_ORANGE_PEEL;
            else if (cumulative[row] > 0) attr = TERM_BLUE;
        }
        else if (cumulative[row] > 0) attr = TERM_GREEN;
        else if (cumulative[row] < 0) attr = TERM_RED;

        // Hack different color for bad flags
        if (pfr_ptr->bad_flag)
        {
            if (attr != TERM_DARK) attr = TERM_RED;
        }

        this_lbl->setText(QString("<b>%1</b>") .arg(color_string(label_text, attr)));
    }

}

// This grid should be only for fields that are part of the TR1_PVAL_MASK flag
void equip_modifier_info(QWidget *this_widget, QGridLayout *return_layout, QFont this_font)
{
    int row = 0;

    int x = 0;

    QList<QLabel *> list_flags;
    QList<QLabel *> list_equippy;
    QList<QLabel *> list_labels;

    list_flags.clear();
    list_equippy.clear();
    list_labels.clear();

    draw_equippy_labels(return_layout, row, 0, FALSE, FALSE, this_font);

    // Hack - compile the equippy list
    QList<QLabel *> label_list = this_widget->findChildren<QLabel *>();

    for (int i = 0; i < label_list.size(); i++)
    {
        QLabel *this_lbl = label_list.at(i);

        QString this_name = this_lbl->objectName();

        //Not a named label
        if (!this_name.length()) continue;

        if (!this_name.contains("equippy_")) continue;

        list_equippy.append(this_lbl);
    }

    while (TRUE)
    {
        player_flag_record *pfr_ptr = &player_pval_table[x++];;

        // We are done
        if (pfr_ptr->name.isNull()) break;

        row++;

        // If in Moria, make sure the flag is used.
        if (game_mode == GAME_NPPMORIA)
        {
            if (!pfr_ptr->moria_flag) continue;
        }

        QLabel *line_label = new QLabel;
        make_standard_label(line_label, pfr_ptr->name, TERM_WHITE, this_font);
        line_label->setObjectName(QString("line_label_%1") .arg(row-1));

        if (pfr_ptr->set == 1)
        {
            // Too messy to include in the charts
            if (pfr_ptr->this_flag == TR1_INFRA)        line_label->setToolTip(get_help_topic("character_info", "Infravision"));
            else if (pfr_ptr->this_flag == TR1_STEALTH) line_label->setToolTip(get_help_topic("character_info", "Stealth"));
            else if (pfr_ptr->this_flag == TR1_SEARCH)  line_label->setToolTip(get_help_topic("character_info", "Searching"));
            else if (pfr_ptr->this_flag == TR1_SPEED)   line_label->setToolTip(get_help_topic("character_info", "Speed"));
            else if (pfr_ptr->this_flag == TR1_TUNNEL)  line_label->setToolTip(get_help_topic("character_info", "Tunneling"));
            else if (pfr_ptr->this_flag == TR1_BLOWS)   line_label->setToolTip(get_help_topic("character_info", "Extra Blows"));
            else if (pfr_ptr->this_flag == TR1_SHOTS)   line_label->setToolTip(get_help_topic("character_info", "Extra Shots"));
            else if (pfr_ptr->this_flag == TR1_MIGHT)   line_label->setToolTip(get_help_topic("character_info", "Extra Might"));
        }
        return_layout->addWidget(line_label, row, 0, Qt::AlignLeft);
        list_labels.append(line_label);

        int col = 0;

        for (int i = INVEN_WIELD; i < INVEN_TOTAL; i++)
        {
            QLabel *this_label = new QLabel();
            make_standard_label(this_label, ".", TERM_DARK, this_font);
            this_label->setObjectName(QString("obj_mod_info_%1_%2") .arg(row-1) .arg(col++));
            return_layout->addWidget(this_label, row, col, Qt::AlignRight);
            list_flags.append(this_label);
        }
    }

    update_equip_modifiers(list_equippy, list_flags, list_labels);
}

PlayerScreenDialog::PlayerScreenDialog(void)
{
    //Set up the main scroll bar
    QVBoxLayout *top_layout = new QVBoxLayout;
    QVBoxLayout *main_layout = new QVBoxLayout;
    QWidget *top_widget = new QWidget;
    QScrollArea *scroll_box = new QScrollArea;
    top_widget->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding);
    top_widget->setLayout(main_layout);
    scroll_box->setWidget(top_widget);
    scroll_box->setWidgetResizable(TRUE);
    top_layout->addWidget(scroll_box);

    // Title Box
    QVBoxLayout *title_line = new QVBoxLayout;
    main_layout->addLayout(title_line);
    QLabel *main_prompt = new QLabel(QString("<h2>Character Information %1 %2 </h2><br><br>") .arg(VERSION_MODE_NAME) .arg(VERSION_STRING));
    title_line->addWidget(main_prompt, Qt::AlignCenter);

    // Char info line
    QHBoxLayout *char_info = new QHBoxLayout;
    main_layout->addLayout(char_info);

    QVBoxLayout *vlay_basic = new QVBoxLayout;
    char_info->addLayout(vlay_basic);
    QGridLayout *basic_info = new QGridLayout;
    name_change_pushbutton(basic_info);
    char_basic_info(basic_info);
    vlay_basic->addLayout(basic_info);
    vlay_basic->addStretch(1);

    QVBoxLayout *vlay_data = new QVBoxLayout;
    char_info->addLayout(vlay_data);
    QGridLayout *basic_data = new QGridLayout;
    char_basic_data(basic_data);
    vlay_data->addLayout(basic_data);
    vlay_data->addStretch(1);

    QVBoxLayout *vlay_game_info = new QVBoxLayout;
    char_info->addLayout(vlay_game_info);
    QGridLayout *game_info = new QGridLayout;
    char_game_info(game_info);
    vlay_game_info->addLayout(game_info);
    vlay_game_info->addStretch(1);

    QVBoxLayout *vlay_combat_info = new QVBoxLayout;
    char_info->addLayout(vlay_combat_info);
    QGridLayout *combat_info = new QGridLayout;
    char_combat_info(combat_info);
    vlay_combat_info->addLayout(combat_info);
    vlay_combat_info->addStretch(1);

    QVBoxLayout *vlay_ability_info = new QVBoxLayout;
    char_info->addLayout(vlay_ability_info);
    QGridLayout *ability_info = new QGridLayout;
    char_ability_info(ability_info);
    vlay_ability_info->addLayout(ability_info);
    vlay_ability_info->addStretch(1);

    QVBoxLayout *vlay_stat_info = new QVBoxLayout;
    char_info->addLayout(vlay_stat_info);
    QGridLayout *stat_info = new QGridLayout;
    char_stat_info(stat_info);
    vlay_stat_info->addLayout(stat_info);
    vlay_stat_info->addStretch(1);

    // Title Box
    QVBoxLayout *player_hist = new QVBoxLayout;
    main_layout->addLayout(player_hist);
    QString desc = color_string((QString("<br><b>%1</b><br>") .arg(p_ptr->history)), TERM_BLUE);
    int first_space = desc.indexOf(' ', 95, Qt::CaseInsensitive);
    desc.replace(first_space, 1, QString("<br>"));
    QLabel *player_desc = new QLabel(desc);
    player_hist->addWidget(player_desc);

    // Object Info
    QGridLayout *equip_info = new QGridLayout;
    main_layout->addLayout(equip_info);

    QVBoxLayout *resist_vlay = new QVBoxLayout;
    QVBoxLayout *ability_vlay = new QVBoxLayout;
    QVBoxLayout *equip_vlay = new QVBoxLayout;
    QVBoxLayout *nativity_vlay = new QVBoxLayout;
    QGridLayout *resist_flags = new QGridLayout;
    QGridLayout *ability_flags = new QGridLayout;
    QGridLayout *equip_mods = new QGridLayout;
    QGridLayout *nativity_flags = new QGridLayout;
    QWidget *resist_widget = new QWidget;
    QWidget *ability_widget = new QWidget;
    QWidget *equip_widget = new QWidget;
    QWidget *nativity_widget = new QWidget;
    resist_vlay->addWidget(resist_widget);
    ability_vlay->addWidget(ability_widget);
    equip_vlay->addWidget(equip_widget);
    nativity_vlay->addWidget(nativity_widget);
    resist_widget->setLayout(resist_flags);
    ability_widget->setLayout(ability_flags);
    equip_widget->setLayout(equip_mods);
    nativity_widget->setLayout(nativity_flags);
    resist_vlay->addStretch(1);
    ability_vlay->addStretch(1);
    equip_vlay->addStretch(1);
    nativity_vlay->addStretch(1);

    QLabel *resist_label = new QLabel("<h3>Resistance Information</h3>");
    resist_label->setToolTip(QString("Blue represents elemental immunity, green represents resistance, and purple represents double resistance."));
    equip_info->addWidget(resist_label, 0, 0, Qt::AlignCenter);
    equip_flag_info(resist_widget, resist_flags, FLAGS_RESIST, resist_label->font());
    equip_info->addLayout(resist_vlay, 1, 0);

    QLabel *ability_label = new QLabel("<h3>Ability Information</h3>");
    equip_info->addWidget(ability_label, 0, 1, Qt::AlignCenter);
    equip_flag_info(ability_widget, ability_flags, FLAGS_ABILITY, ability_label->font());
    equip_info->addLayout(ability_vlay,  1, 1);

    QLabel *nativity_label = new QLabel("<h3>Nativity Information</h3>");
    equip_info->addWidget(nativity_label, 0, 2, Qt::AlignCenter);
    equip_flag_info(nativity_widget, nativity_flags, FLAGS_NATIVITY, nativity_label->font());
    equip_info->addLayout(nativity_vlay, 1, 2);

    QLabel *modifier_label = new QLabel("<h3>Equipment Modifiers</h3>");
    equip_info->addWidget(modifier_label, 0, 3, Qt::AlignCenter);
    equip_modifier_info(equip_widget, equip_mods, modifier_label->font());
    equip_info->addLayout(equip_vlay, 1, 3);

    QDialogButtonBox *buttons = new QDialogButtonBox(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    top_layout->addWidget(buttons);

    update_char_screen(top_widget, ui_message_window_font());

    setLayout(top_layout);
    setWindowTitle(tr("Player Information"));

    QSize this_size = QSize(width() * 21 / 8, height() * 5 / 3);
    resize(ui_max_widget_size(this_size));
    updateGeometry();
}



void do_cmd_character_screen()
{
    PlayerScreenDialog dlg;

    dlg.exec();


}
