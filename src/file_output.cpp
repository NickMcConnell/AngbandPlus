
/*
 * File: file_output.cpp
 * Purpose: Various file-related activities
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke,
 * Jeff Greene, Diego Gonzalez and Angband developers
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

#include <QFileDialog>
#include <QTextStream>
#include <src/init.h>
#include <src/utilities.h>
#include <src/player_screen.h>

#define RESIST_TABLE_LENGTH 34
#define EQUIPPY_TABLE_LENGTH (INVEN_TOTAL - INVEN_WIELD + 2)
#define RESIST_LABEL_LENGTH (RESIST_TABLE_LENGTH - EQUIPPY_TABLE_LENGTH)

#define ABILITIES_TABLE_LENGTH 30
#define ABILITIES_LABEL_LENGTH (ABILITIES_TABLE_LENGTH - EQUIPPY_TABLE_LENGTH)

#define NATIVITY_TABLE_LENGTH 34
#define NATIVITY_LABEL_LENGTH (NATIVITY_TABLE_LENGTH - EQUIPPY_TABLE_LENGTH)

#define MODIFIERS_TABLE_LENGTH 64
#define EQUIPPY_MODIFIERS_LENGTH ((INVEN_TOTAL - INVEN_WIELD) * 4)
#define MODIFIERS_LABEL_LENGTH (MODIFIERS_TABLE_LENGTH - EQUIPPY_MODIFIERS_LENGTH)

// Helper function to check if the color is white
bool is_white(QColor this_color)
{
    if (this_color.operator ==(defined_colors[TERM_WHITE])) return (TRUE);
    if (this_color.operator ==(defined_colors[TERM_LIGHT_GRAY])) return (TRUE);
    if (this_color.operator ==(defined_colors[TERM_SNOW_WHITE])) return (TRUE);
    if (this_color.operator ==(defined_colors[TERM_IVORY])) return (TRUE);
    if (this_color.operator ==(Qt::white)) return (TRUE);

    return (FALSE);
}

// Helper function to check if the color is black
bool is_black(QColor this_color)
{
    if (this_color.operator ==(defined_colors[TERM_DARK])) return (TRUE);
    if (this_color.operator ==(defined_colors[TERM_L_DARK])) return (TRUE);
    if (this_color.operator ==(Qt::black)) return (TRUE);

    return (FALSE);
}

static QString get_sidebar_string(int which_line)
{
    QString label_string;
    QString output_string;

    // Paranoia
    if (which_line < 0) return ("");
    else if (which_line == 0) label_string = op_ptr->full_name;
    else if (which_line == 1) label_string = p_info[p_ptr->prace].pr_name;
    else if (which_line == 2) label_string = c_info[p_ptr->pclass].cl_name;
    else if (which_line == 3) label_string = get_player_title();

    // Handle names longer than 20 characters, or anyting else unusual
    if (label_string.length() > 20) label_string.truncate(20);

    // These will not be longer than 20 characters
    if (which_line >= 4)
    {
        // Get the label and the text
        which_line -= 4;
        label_string = ui_return_sidebar_text(TRUE, which_line);
        output_string = ui_return_sidebar_text(FALSE, which_line);
    }

    // Need to get the true length without the html tags
    while (TRUE)
    {
        QString simple_1 = html_string_to_plain_text(label_string);
        QString simple_2 = html_string_to_plain_text(output_string);

        if ((simple_1.length() + simple_2.length()) >= 20) break;

        label_string.append(" ");
    }
    label_string.append(output_string);

    label_string.append("  ");

    return (label_string);
}

/* Take an html screenshot */
void save_screenshot(byte do_png)
{
    if (do_png)
    {
        ui_png_screenshot();
        return;
    }

    // Start with the current player name
    QString default_name = "player";
    if (!op_ptr->full_name.isEmpty())default_name = op_ptr->full_name;
    QString default_file = npp_dir_user.path();
    default_file.append("/");
    default_file.append(default_name);
    default_file.append("_npp_scr");

    QString file_name = QFileDialog::getSaveFileName(0, "Select a savefile", default_file, "HTML (*.html)");

    if (file_name.isEmpty())
        return;

    QFile screenshot_file(file_name);

    if (!screenshot_file.open(QIODevice::WriteOnly)) return;

    QString text = (QString("Version: %1 %2") .arg(VERSION_MODE_NAME) .arg(VERSION_STRING));

    QTextStream out(&screenshot_file);


    out << QString("<!DOCTYPE html><html><head><br>");
    out << (QString("  <meta name='variant' value='%1'>") .arg(VERSION_MODE_NAME));
    out << (QString("  <meta name='variant_version' value='%1'>") .arg(VERSION_STRING));
    out << (QString("  <meta name='character_name' value='%1'>") .arg(op_ptr->full_name));
    out << (QString("  <meta name='race' value='%1'><br>") .arg(p_info[p_ptr->prace].pr_name));
    out << (QString("  <meta name='class' value='%1'>") .arg(c_info[p_ptr->pclass].cl_name));
    out << (QString("  <meta name='level' value='%1'>") .arg(p_ptr->lev));
    out << (QString("  <meta name='experience' value='%1'><br>") .arg(p_ptr->exp));
    out << (QString("  <meta name='turncount' value='%1'>") .arg(p_ptr->game_turn));
    out << (QString("  <meta name='max_depth' value='%1'>") .arg(p_ptr->max_depth));
    out << (QString("  <meta name='score' value='%1'>") .arg(p_ptr->current_score));
    out << (QString("  <meta name='fame' value='%1'>") .arg(p_ptr->q_fame));
    out << QString("</head><br><br>");
    out << QString("<body style='color: #fff; background: #000;'><br>");
    out << QString("<pre><br>");

    int sidebar_row = 0;

    /* Dump the screen */
    for (int y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        bool first_x = TRUE;

        for (int x = 0; x < p_ptr->cur_map_wid; x++)
        {
            if (!panel_contains(y, x)) continue;

            //Hack - print out the sidebar info
            if (first_x)
            {
                first_x = FALSE;

                out << get_sidebar_string(sidebar_row++);
            }

            dungeon_type *d_ptr = &dungeon_info[y][x];

            QChar square_char = d_ptr->dun_char;
            QColor square_color = d_ptr->dun_color;

            if (d_ptr->has_visible_monster())
            {
                square_char = d_ptr->monster_char;
                square_color = d_ptr->monster_color;
            }

            else if (d_ptr->has_visible_effect())
            {
                square_char = d_ptr->effect_char;
                square_color = d_ptr->effect_color;
            }

            else if (d_ptr->has_visible_object())
            {
                square_char = d_ptr->object_char;
                square_color = d_ptr->object_color;
            }

            // Can't print white
            if (is_white(square_color)) square_color = defined_colors[TERM_L_DARK];

            out << color_string(square_char, square_color);
        }

        // end the row
        out << QString("<br>");
    }

    // If there are more rows in the sidebar than in the dungeon
    while (sidebar_row < (4+SIDEBAR_LABEL_SIZE))
    {
        out << get_sidebar_string(sidebar_row++);
        out << QString("<br>");
    }

    out << QString("</pre><br>");
    out << QString("</body><br>");
    out << QString("</html><br>");

    screenshot_file.close();
}

#define BASIC_CHAR_NUM 9

QString basic_char_labels[BASIC_CHAR_NUM] =
{
    "<b>NAME:<b/>",  // Hack - the dialog uses a pushbutton
    "GENDER_LABEL",
    "RACE_LABEL",
    "CLASS_LABEL",
    "TITLE_LABEL",
    "HP_LABEL",
    "SP_LABEL",
    "FAME_LABEL",
    "GOLD_LABEL",
};

QString basic_char_data[BASIC_CHAR_NUM] =
{
    "PLYR_Name",
    "PLYR_Sex",
    "PLYR_Race",
    "PLYR_Class",
    "PLYR_Title",
    "PLYR_HP",
    "PLYR_SP",
    "PLYR_Fame",
    "PLYR_Gold",
};

#define BASIC_DATA_NUM 9

QString basic_data_labels[BASIC_DATA_NUM] =
{
    "AGE_LABEL",
    "HEIGHT_LABEL",
    "WEIGHT_LABEL",
    "SC_LABEL",
    "GAME_TURN_LABEL",
    "PLAYER_TURN_LABEL",
    "DEPTH_CUR_LABEL",
    "DEPTH_MAX_LABEL",
    "INFRA_LABEL",
};

QString basic_data_data[BASIC_DATA_NUM] =
{
    "PLYR_Age",
    "PLYR_Height",
    "PLYR_Weight",
    "PLYR_SC",
    "TURN_Game",
    "TURN_Player",
    "DEPTH_Cur",
    "DEPTH_Max",
    "PLYR_Infra",
};

#define GAME_INFO_NUM 8

QString game_info_labels[GAME_INFO_NUM] =
{
    "LEVEL_LABEL",
    "CUR_EXP_LABEL",
    "MAX_EXP_LABEL",
    "ADVANCE_LABEL",
    "SCORE_LABEL",
    "BURDEN_CUR_LABEL",
    "BURDEN_MAX_LABEL",
    "BURDEN_PERCENT_LABEL",
};

QString game_info_data[GAME_INFO_NUM] =
{
    "PLYR_Level",
    "PLYR_Cur_Exp",
    "PLYR_Max_Exp",
    "PLYR_Advance",
    "PLYR_Score",
    "BURDEN_Cur",
    "BURDEN_Max",
    "BURDEN_Percent",
};

#define COMBAT_INFO_NUM 8

QString combat_info_labels[COMBAT_INFO_NUM] =
{
    "SPEED_LABEL",
    "AC_LABEL",
    "MELEE_LABEL",
    "CRIT_HIT_LABEL",
    "SHOOTING_LABEL",
    "SRCH_FREQ_LABEL",
    "SRCH_CHANCE_LABEL",
    "TUNNEL_LABEL",
};

QString combat_info_data[COMBAT_INFO_NUM] =
{
    "BASE_Speed",
    "PLYR_AC",
    "PLYR_MELEE",
    "HIT_Critical",
    "SHOOTING_Stats",
    "SEARCH_Freq",
    "SEARCH_Chance",
    "PLYR_Tunnel",
};

static QString combine_strings(QString string_1, QString string_2, int max_length)
{
    // Need to get the true length without the html tags
    while (TRUE)
    {
        QString simple_1 = html_string_to_plain_text(string_1);
        QString simple_2 = html_string_to_plain_text(string_2);

        if ((simple_1.length() + simple_2.length()) >= max_length) break;

        string_1.append(" ");
    }

    string_1.append(string_2);
    string_1.append("     ");
    return (string_1);
}

// Create the stat line for the character info file
// The truncate statements are because the combine_strings file adds 5 spaces on the end
static QString make_stat_string(QString stat_label, QString stat_base, QString stat_race, QString stat_class,
                                QString stat_equip, QString stat_quest, QString stat_total, QString stat_reduced)
{
    QString return_string = set_html_string_length(stat_label, 6, FALSE);

    // get rid of spaces
    stat_base.remove(" ");
    stat_race.remove(" ");
    stat_class.remove(" ");
    stat_equip.remove(" ");
    stat_quest.remove(" ");
    stat_total.remove(" ");
    stat_reduced.remove(" ");

    stat_base = set_html_string_length(stat_base, 6, TRUE);
    return_string.append(stat_base);

    if (birth_maximize)
    {
        stat_race = set_html_string_length(stat_race, 5, TRUE);
        return_string.append(stat_race);
        stat_class = set_html_string_length(stat_class, 5, TRUE);
        return_string.append(stat_class);
    }

    stat_equip = set_html_string_length(stat_equip, 5, TRUE);
    return_string.append(stat_equip);

    if (!birth_no_quests)
    {
        stat_quest = set_html_string_length(stat_quest, 5, TRUE);
        return_string.append(stat_quest);
    }

    stat_total = set_html_string_length(stat_total, 12, TRUE);
    return_string.append(stat_total);

    if (stat_reduced.length())
    {
        stat_reduced = set_html_string_length(stat_total, 12, TRUE);
        return_string.append(stat_reduced);
    }

    return (return_string);
}

static QString blank_string(byte length)
{
    // Paranoia
    if (length < 1) return (QString(""));

    QString blanks;

    blanks.fill(' ', length);

    return (blanks);
}

static QString make_equippy(bool do_player, bool do_temp, bool modifiers)
{
    QString equippy;
    equippy.clear();

    for (int i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];

        if (!o_ptr->k_idx)
        {
            if (modifiers) equippy.append("    ");
            else equippy.append(" ");
            continue;
        }

        object_kind *k_ptr = &k_info[o_ptr->k_idx];

        if (modifiers) equippy.append(QString("  %1 ") .arg(color_char(k_ptr->d_char, k_ptr->d_color)));
        else equippy.append(color_char(k_ptr->d_char, k_ptr->d_color));
    }

    if (do_player)
    {
        if (modifiers) equippy.append("  @ ");
        else equippy.append("@");
    }
    if (do_temp)
    {
        if (modifiers) equippy.append("  t ");
        else equippy.append("t");
    }

    return (equippy);
}

// Insert page breaks into long lines for html printing.
static QString format_line_breaks(QString this_string)
{
    int findspace = 85;
    int in_bracket = 0;
    int counter = 0;

    this_string.remove("<br>");

    for (int space = 0; space < this_string.length(); space++)
    {
        if (this_string[space] == QChar('<')) in_bracket++;
        else if (this_string[space] == QChar('>')) in_bracket--;

        // Wait until we are outside a bracket and past the counter
        if (in_bracket) continue;
        counter++;
        if (counter < findspace) continue;

        // Looking for the next space
        if (this_string[space] != QChar(' ')) continue;

        //  Before replacing the space,
        // delete spaces as long as we find them.
        while ((space+1) < this_string.length())
        {
            if (this_string[space+1] != QChar(' ')) break;

            this_string.remove(space+1, 1);
        }

        // Paranoia check
        if (space < this_string.length())
        {
            this_string.replace((space), 1, QString("<br>"));

            // Make sure that new bracket space is counted
            space--;
            counter--;
        }

        findspace += 85;
        if (findspace >= this_string.length()) break;
    }

    return (this_string);
}

/* Save a character file
 *
 * For this function to work, it is important that the tables above
 * are consistent with the Qlabel object names in player_screen.
 *
 * This functon creates a dummy characte screen dialog and reads the label values
 */
void save_character_file(void)
{
    // Start with the current player name
    QString default_name = "player";
    if (!op_ptr->full_name.isEmpty())default_name = op_ptr->full_name;
    QString default_file = npp_dir_user.path();
    default_file.append("/");
    default_file.append(default_name);
    default_file.append("_npp_char");

    QString dark_text = "<font color='#000000'>";
    QString white_text = "<font color='#ffffff'>";

    QString file_name = QFileDialog::getSaveFileName(0, "Select a savefile", default_file, "HTML (*.html)");

    if (file_name.isEmpty())
        return;

    QFile char_info_file(file_name);

    if (!char_info_file.open(QIODevice::WriteOnly)) return;

    QTextStream out(&char_info_file);

    QString text = (QString("<h1>[%1 %2 Character Dump]</h1></b><br>") .arg(VERSION_MODE_NAME) .arg(VERSION_STRING));

    //Hack - create the player screen from which all the data will be read
    PlayerScreenDialog dlg;

    QList<QLabel *> lbl_list = dlg.findChildren<QLabel *>();

    out << QString("<!DOCTYPE html><html><head>");
    out << (QString("  <meta name='variant' value='%1'>") .arg(VERSION_MODE_NAME));
    out << (QString("  <meta name='variant_version' value='%1'>") .arg(VERSION_STRING));
    out << (QString("  <meta name='character_name' value='%1'>") .arg(to_ascii(op_ptr->full_name)));
    out << (QString("  <meta name='race' value='%1'><br>") .arg(p_info[p_ptr->prace].pr_name));
    out << (QString("  <meta name='class' value='%1'>") .arg(c_info[p_ptr->pclass].cl_name));
    out << (QString("  <meta name='level' value='%1'>") .arg(p_ptr->lev));
    out << (QString("  <meta name='experience' value='%1'><br>") .arg(p_ptr->exp));
    out << (QString("  <meta name='turncount' value='%1'>") .arg(p_ptr->game_turn));
    out << (QString("  <meta name='max_depth' value='%1'>") .arg(p_ptr->max_depth));
    out << (QString("  <meta name='score' value='%1'>") .arg(p_ptr->current_score));
    out << (QString("  <meta name='fame' value='%1'>") .arg(p_ptr->q_fame));
    out << QString("</head>");
    out << QString("<body style='color: #fff; background: #000;'>");
    out << QString("<pre>");

    out << text;

    int end_loop = MAX(BASIC_DATA_NUM, BASIC_DATA_NUM);
    if (end_loop < A_MAX) end_loop = A_MAX;

    for (int i = 0; i < end_loop; i++)
    {
        QString basic_char_label;
        QString basic_char_info;
        QString basic_data_label;
        QString basic_data_info;
        QString stat_label = "st_label_";
        QString stat_base = "st_base_";
        QString stat_race = "st_race_";
        QString stat_class = "st_class_";
        QString stat_equip = "st_equip_";
        QString stat_quest = "st_quest_";
        QString stat_total = "st_total_";
        QString stat_reduced = "st_reduce_";

        if (i < BASIC_CHAR_NUM)
        {
            basic_char_label = basic_char_labels[i];
            basic_char_info = basic_char_data[i];

            if (basic_char_label.contains("SP_LABEL") && !cp_ptr->spell_book)
            {
                basic_char_label.clear();
                basic_char_info.clear();
            }
            if (basic_char_label.contains("FAME_LABEL") && birth_no_quests)
            {
                basic_char_label.clear();
                basic_char_info.clear();
            }

        }
        else
        {
            basic_char_label.clear();
            basic_char_info.clear();
        }
        if (i < BASIC_DATA_NUM)
        {
            basic_data_label = basic_data_labels[i];
            basic_data_info = basic_data_data[i];
        }
        else
        {
            basic_data_label.clear();
            basic_data_info.clear();
        }

        if (i < (A_MAX+1))
        {
            // Do the labels
            if (!i)
            {
                stat_label.append("X");
                stat_base.append("X");
                stat_race.append("X");
                stat_class.append("X");
                stat_equip.append("X");
                stat_quest.append("X");
                stat_total.append("X");
                stat_reduced.clear();
            }
            // Or fill in the stats
            else
            {
                int k = i - 1;

                QString append;
                append.setNum(k);
                stat_label.append(append);
                stat_base.append(append);
                stat_race.append(append);
                stat_class.append(append);
                stat_equip.append(append);
                stat_quest.append(append);
                stat_total.append(append);
                stat_reduced.append(append);

                // No need to display reduced stat
                if (p_ptr->state.stat_loaded_cur[k] <
                    p_ptr->state.stat_loaded_max[k]) stat_reduced.clear();
            }

            if (!birth_maximize)
            {
                stat_race.clear();
                stat_class.clear();
            }

            if (birth_no_quests)  stat_quest.clear();
        }
        else
        {
            stat_label.clear();
            stat_base.clear();
            stat_race.clear();
            stat_class.clear();
            stat_equip.clear();
            stat_quest.clear();
            stat_total.clear();
            stat_reduced.clear();
        }


        //Find the values
        for (int x = 0; x < lbl_list.size(); x++)
        {
            QLabel *this_lbl = lbl_list.at(x);

            QString this_name = this_lbl->objectName();

            // It is converted to ASCII in case a player picks some unicode characters in their name.
            QString this_text = to_ascii(this_lbl->text());

            // Remove all specified white and dark text
            if (this_text.contains(dark_text) || this_text.contains(white_text))
            {
                this_text.remove("</font>");
                this_text.remove(dark_text);
                this_text.remove(white_text);
            }

            // hack - replace dark text with white text
            this_text.replace(dark_text, white_text, Qt::CaseInsensitive);

            if (strings_match(this_name, basic_char_label)) basic_char_label = this_text;
            if (strings_match(this_name, basic_char_info))  basic_char_info  = this_text;
            if (strings_match(this_name, basic_data_label)) basic_data_label = this_text;
            if (strings_match(this_name, basic_data_info))  basic_data_info  = this_text;
            if (strings_match(this_name, stat_label))       stat_label       = this_text;
            if (strings_match(this_name, stat_base))        stat_base        = this_text;
            if (strings_match(this_name, stat_race))        stat_race        = this_text;
            if (strings_match(this_name, stat_class))       stat_class       = this_text;
            if (strings_match(this_name, stat_equip))       stat_equip       = this_text;
            if (strings_match(this_name, stat_quest))       stat_quest       = this_text;
            if (strings_match(this_name, stat_total))       stat_total       = this_text;
            if (strings_match(this_name, stat_reduced))     stat_reduced     = this_text;
        }

        out << combine_strings(basic_char_label, basic_char_info, 25);
        out << combine_strings(basic_data_label, basic_data_info, 25);
        if (i < (A_MAX+1)) out << make_stat_string(stat_label, stat_base, stat_race, stat_class, stat_equip, stat_quest, stat_total, stat_reduced);
        out << QString("<br>");
    }

    out << QString("<br><br>");

    end_loop = MAX(COMBAT_INFO_NUM, GAME_INFO_NUM);

    for (int i = 0; i < end_loop; i++)
    {
        QString game_info_label;
        QString game_info_info;
        QString combat_info_label;
        QString combat_info_info;

        if (i < GAME_INFO_NUM)
        {
            game_info_label = game_info_labels[i];
            game_info_info = game_info_data[i];
        }
        else
        {
            game_info_label.clear();
            game_info_info.clear();
        }
        if (i < COMBAT_INFO_NUM)
        {
            combat_info_label = combat_info_labels[i];
            combat_info_info = combat_info_data[i];
        }
        else
        {
            combat_info_label.clear();
            combat_info_info.clear();
        }

        //Find the values
        for (int x = 0; x < lbl_list.size(); x++)
        {
            QLabel *this_lbl = lbl_list.at(x);

            QString this_name = this_lbl->objectName();

            QString this_text = this_lbl->text();

            // Remove all specified white and dark text
            if (this_text.contains(dark_text) || this_text.contains(white_text))
            {
                this_text.remove("</font>");
                this_text.remove(dark_text);
                this_text.remove(white_text);
            }

            if (strings_match(this_name, game_info_label))  game_info_label  = this_text;
            if (strings_match(this_name, game_info_info))   game_info_info   = this_text;
            if (strings_match(this_name, combat_info_label))combat_info_label = this_text;
            if (strings_match(this_name, combat_info_info)) combat_info_info = this_text;
        }

        out << combine_strings(game_info_label, game_info_info, 25);
        out << combine_strings(combat_info_label, combat_info_info, 40);
        out << QString("<br>");
    }

    // Print character description
    QString desc = color_string(to_ascii(p_ptr->history), TERM_BLUE);
    int first_space = desc.indexOf(' ', 90, Qt::CaseInsensitive);
    desc.replace(first_space, 1, QString("<br>"));
    out << QString("<br>") << desc << QString("<br><br>");

    // Print character description
    if (guild_quest_active())
    {
        desc = color_string(to_ascii(describe_quest(guild_quest_level())), TERM_RED);
        first_space = desc.indexOf(' ', 60, Qt::CaseInsensitive);
        desc.replace(first_space, 1, QString("<br>"));
        out << QString("<br>") << desc << QString("<br><br>");
    }

    if (p_ptr->is_dead)
    {
        out << to_ascii(output_messages(15));

        QString("<br><br>");
    }

    // Print out the resists and abilities

    //Equippy row
    QString equippy = make_equippy(TRUE, TRUE, FALSE);

    // Title Row
    QString output = "Resistance Information";
    while (output.length() < RESIST_TABLE_LENGTH)
    {
        if (output.length() % 2) output.append(' ');
        else output.prepend(' ');
    }

    out << QString("<br><br>") << QString(output) << blank_string(5);

    output = "Ability Information";
    while (output.length() < ABILITIES_TABLE_LENGTH)
    {
        if (output.length() % 2) output.append(' ');
        else output.prepend(' ');
    }
    out << QString(output) << QString("<br>");

    out << blank_string(RESIST_LABEL_LENGTH) << equippy;
    out << blank_string(ABILITIES_LABEL_LENGTH+6) <<equippy << QString("<br>");

    bool resist_table = TRUE;
    bool ability_table = TRUE;
    int row = 0;

    while (resist_table || ability_table)
    {
        if (resist_table)
        {
            player_flag_record *pfr_ptr = &player_resist_table[row];

            if (pfr_ptr->name.isNull()) resist_table = FALSE;

            // If in Moria, make sure the flag is used.
            if ((game_mode == GAME_NPPMORIA) && resist_table && !pfr_ptr->moria_flag)
            {
                out << blank_string(RESIST_TABLE_LENGTH + 6);
            }

            else if (resist_table)
            {
                QString resist_output = (QString("line_label_%1_%2") .arg(FLAGS_RESIST) .arg(row));
                QString inven_chars[EQUIPPY_TABLE_LENGTH];
                for (int x = 0; x < (EQUIPPY_TABLE_LENGTH); x++)
                {
                    inven_chars[x] = '.';
                }

                QString test_string = (QString("obj_flag_info_%1_%2_") .arg(FLAGS_RESIST) .arg(row));

                //Find the values
                for (int x = 0; x < lbl_list.size(); x++)
                {
                    QLabel *this_lbl = lbl_list.at(x);

                    QString this_name = this_lbl->objectName();

                    QString this_text = this_lbl->text();

                    // Remove all specified white and dark text
                    if (this_text.contains(dark_text) || this_text.contains(white_text))
                    {
                        this_text.remove("</font>");
                        this_text.remove(dark_text);
                        this_text.remove(white_text);
                    }

                    if (!this_name.length()) continue;
                    if (strings_match(this_name, resist_output))
                    {
                        resist_output = this_text;
                        continue;
                    }

                    if (this_name.contains(test_string))
                    {
                        bool ok;
                        this_name.remove(test_string);

                        int column = this_name.toInt(&ok);

                        if(ok) inven_chars[column] = this_text;

                        continue;
                    }
                }

                resist_output = set_html_string_length(resist_output, RESIST_LABEL_LENGTH, FALSE);

                for (int x = 0; x < (EQUIPPY_TABLE_LENGTH); x++)
                {
                    resist_output.append(inven_chars[x]);
                }

                output.append(' ');

                out  << resist_output << blank_string(6);
            }

            else out << blank_string(RESIST_TABLE_LENGTH + 6);
        }
        else out << blank_string(RESIST_TABLE_LENGTH + 6);

        if (ability_table)
        {
            player_flag_record *pfr_ptr = &player_abilities_table[row];

            if (pfr_ptr->name.isNull()) ability_table = FALSE;

            // If in Moria, make sure the flag is used.
            if ((game_mode == GAME_NPPMORIA) && ability_table && !pfr_ptr->moria_flag)
            {
                out << blank_string(ABILITIES_TABLE_LENGTH);
            }

            else if (ability_table)
            {
                QString ability_output = (QString("line_label_%1_%2") .arg(FLAGS_ABILITY) .arg(row));
                QString inven_chars[EQUIPPY_TABLE_LENGTH];
                for (int x = 0; x < EQUIPPY_TABLE_LENGTH; x++)
                {
                    inven_chars[x] = '.';
                }

                QString test_string = (QString("obj_flag_info_%1_%2_") .arg(FLAGS_ABILITY) .arg(row));

                //Find the values
                for (int x = 0; x < lbl_list.size(); x++)
                {
                    QLabel *this_lbl = lbl_list.at(x);

                    QString this_name = this_lbl->objectName();

                    QString this_text = this_lbl->text();

                    // Remove all specified white and dark text
                    if (this_text.contains(dark_text) || this_text.contains(white_text))
                    {
                        this_text.remove("</font>");
                        this_text.remove(dark_text);
                        this_text.remove(white_text);
                    }

                    if (!this_name.length()) continue;
                    if (strings_match(this_name, ability_output))
                    {
                        ability_output = this_text;
                        continue;
                    }

                    if (this_name.contains(test_string))
                    {
                        bool ok;
                        this_name.remove(test_string);

                        int column = this_name.toInt(&ok);

                        if(ok) inven_chars[column] = this_text;

                        continue;
                    }
                }

                ability_output = set_html_string_length(ability_output, ABILITIES_LABEL_LENGTH, FALSE);

                for (int x = 0; x < EQUIPPY_TABLE_LENGTH; x++)
                {
                    ability_output.append(inven_chars[x]);
                }

                output.append(' ');

                out  << ability_output;
            }
        }

        out << QString("<br>");

        row++;
    }

    output = "Nativity Information";
    while (output.length() < NATIVITY_TABLE_LENGTH)
    {
        if (output.length() % 2) output.append(' ');
        else output.prepend(' ');
    }

    out << QString("<br><br>") << QString(output) << blank_string(5);

    output = "Equipment Modification";
    while (output.length() < MODIFIERS_TABLE_LENGTH)
    {
        if (output.length() % 2) output.append(' ');
        else output.prepend(' ');
    }
    out << QString(output) << QString("<br>");

    out << blank_string(NATIVITY_LABEL_LENGTH) << equippy;

    equippy = make_equippy(FALSE, FALSE, TRUE);
    out << blank_string(MODIFIERS_LABEL_LENGTH+7) << equippy << QString("<br>");

    bool nativity_table = TRUE;
    bool modifiers_table = TRUE;
    row = 0;

    while (nativity_table || modifiers_table)
    {
        if (nativity_table)
        {
            player_flag_record *pfr_ptr = &player_nativity_table[row];

            if (pfr_ptr->name.isNull()) nativity_table = FALSE;

            // If in Moria, make sure the flag is used.
            if ((game_mode == GAME_NPPMORIA) && nativity_table && !pfr_ptr->moria_flag)
            {
                out << blank_string(NATIVITY_TABLE_LENGTH + 6);
            }

            else if (nativity_table)
            {
                QString nativity_output = (QString("line_label_%1_%2") .arg(FLAGS_NATIVITY) .arg(row));
                QString inven_chars[EQUIPPY_TABLE_LENGTH];
                for (int x = 0; x < (EQUIPPY_TABLE_LENGTH); x++)
                {
                    inven_chars[x] = '.';
                }

                QString test_string = (QString("obj_flag_info_%1_%2_") .arg(FLAGS_NATIVITY) .arg(row));

                //Find the values
                for (int x = 0; x < lbl_list.size(); x++)
                {
                    QLabel *this_lbl = lbl_list.at(x);

                    QString this_name = this_lbl->objectName();

                    QString this_text = this_lbl->text();

                    // Remove all specified white and dark text
                    if (this_text.contains(dark_text) || this_text.contains(white_text))
                    {
                        this_text.remove("</font>");
                        this_text.remove(dark_text);
                        this_text.remove(white_text);
                    }

                    if (!this_name.length()) continue;
                    if (strings_match(this_name, nativity_output))
                    {
                        nativity_output = this_text;
                        continue;
                    }

                    if (this_name.contains(test_string))
                    {
                        bool ok;
                        this_name.remove(test_string);

                        int column = this_name.toInt(&ok);

                        if(ok) inven_chars[column] = this_text;

                        continue;
                    }
                }

                nativity_output = set_html_string_length(nativity_output, NATIVITY_LABEL_LENGTH, FALSE);

                for (int x = 0; x < (EQUIPPY_TABLE_LENGTH); x++)
                {
                    nativity_output.append(inven_chars[x]);
                }

                output.append(' ');

                out  << nativity_output << blank_string(6);
            }
            else out << blank_string(NATIVITY_TABLE_LENGTH + 6);
        }
        else out << blank_string(NATIVITY_TABLE_LENGTH + 6);

        if (modifiers_table)
        {
            player_flag_record *pfr_ptr = &player_pval_table[row];

            if (pfr_ptr->name.isNull()) modifiers_table = FALSE;

            // If in Moria, make sure the flag is used.
            if ((game_mode == GAME_NPPMORIA) && modifiers_table && !pfr_ptr->moria_flag)
            {
                out << blank_string(MODIFIERS_TABLE_LENGTH);
            }

            else if (modifiers_table)
            {
                QString modifiers_output = (QString("line_label_%1") .arg(row));
                QString inven_chars[EQUIPPY_TABLE_LENGTH-2];
                for (int x = 0; x < (EQUIPPY_TABLE_LENGTH-2); x++)
                {
                    inven_chars[x] = "   .";
                }

                QString test_string = (QString("obj_mod_info_%1") .arg(row));

                //Find the values
                for (int x = 0; x < lbl_list.size(); x++)
                {
                    QLabel *this_lbl = lbl_list.at(x);

                    QString this_name = this_lbl->objectName();

                    QString this_text = this_lbl->text();

                    // Remove all specified white and dark text
                    if (this_text.contains(dark_text) || this_text.contains(white_text))
                    {
                        this_text.remove("</font>");
                        this_text.remove(dark_text);
                        this_text.remove(white_text);
                    }

                    if (!this_name.length()) continue;
                    if (strings_match(this_name, modifiers_output))
                    {
                        modifiers_output = this_text;
                        continue;
                    }

                    if (this_name.contains(test_string))
                    {
                        bool ok;
                        this_name.remove(test_string);

                        int column = this_name.toInt(&ok);

                        QString output_text  = set_html_string_length(this_text, 4, TRUE);

                        inven_chars[column] = output_text;

                        continue;
                    }
                }

                modifiers_output = set_html_string_length(modifiers_output, MODIFIERS_LABEL_LENGTH, FALSE);

                for (int x = 0; x < (EQUIPPY_TABLE_LENGTH-2); x++)
                {
                    modifiers_output.append(inven_chars[x]);
                }
                out  << modifiers_output;
            }
        }
        else out << blank_string(NATIVITY_TABLE_LENGTH + 6);

        out << QString("<br>");

        row++;
    }

    out << QString("<br><b><u>[Character Equipment]</u></b><br><br>");

    for (int i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        QString o_name = to_ascii(object_desc(&inventory[i], ODESC_PREFIX | ODESC_FULL));

        out << (QString("%1) %2<br>") .arg(index_to_label(i)) .arg(o_name));

        /* Describe random object attributes */
        out << format_line_breaks(to_ascii(identify_random_gen(&inventory[i]))) << QString("<br><br>");
    }

    out << QString("<br><br>");

    if (p_ptr->quiver_slots)
    {
        out << QString("<b><u>[Character Equipment -- Quiver]</u></b><br><br>");

        for (int i = QUIVER_START; i < QUIVER_END; i++)
        {
            if (!inventory[i].k_idx) break;

            QString o_name = to_ascii(object_desc(&inventory[i], ODESC_PREFIX | ODESC_FULL));

            out << (QString("%1) %2<br>") .arg(i - QUIVER_START + 1) .arg(o_name));

            /* Describe random object attributes */
            out << format_line_breaks(to_ascii(identify_random_gen(&inventory[i]))) << QString("<br><br>");
        }

        out << QString("<br><br>");
    }

    out << QString("<br><b><u>[Character Inventory]</u></b><br><br>");

    for (int i = 0; i < INVEN_PACK; i++)
    {
        if (!inventory[i].k_idx) break;

        QString o_name = to_ascii(object_desc(&inventory[i], ODESC_PREFIX | ODESC_FULL));

        out << (QString("%1) %2<br>") .arg(index_to_label(i)) .arg(o_name));

        /* Describe random object attributes */
        out << format_line_breaks(to_ascii(identify_random_gen(&inventory[i]))) << QString("<br><br>");
    }

    out << QString("<br><br>");

    // Print the home inventory
    store_type *st_ptr = &store[STORE_HOME];
    if (st_ptr->stock_num)
    {
        out << QString("<b><u>[Home Inventory]</u></b><br><br>");

        for (int i = 0; i < st_ptr->stock_num; i++)
        {
            QString o_name = to_ascii(object_desc(&inventory[i], ODESC_PREFIX | ODESC_FULL));

            out << (QString("%1) %2<br>") .arg(index_to_label(i)) .arg(o_name));

            /* Describe random object attributes */
            out << format_line_breaks(to_ascii(identify_random_gen(&inventory[i]))) << QString("<br>");
        }

        out << QString("<br><br>");
    }

    else out << QString("<b><u>[Your Home Is Empty]</u></b><br><br>");


    // Print the notes file
    out << QString("<b><u>  GAME TURN</u>  <u>DUNGEON DEPTH</u>  <u>PLAYER LEVEL</u>  <u>EVENT</u></b><br>");

    // Print out all the notes
    for (int i = 0; i < notes_log.size(); i++)
    {
       QString depth_note = (QString("Town"));
       notes_type *notes_ptr = &notes_log[i];

       // Print the game turn
       QString game_turn = number_to_formatted_string(notes_ptr->game_turn);
       game_turn = set_html_string_length(game_turn, 10, TRUE);
       out << game_turn;

       // Format the depth, unless the player is in town
       if (notes_ptr->dun_depth) depth_note = number_to_formatted_string(notes_ptr->dun_depth * 50);
       depth_note = set_html_string_length(depth_note, 10, TRUE);
       out << depth_note;

       QString player_level = set_html_string_length((QString("%1") .arg(notes_ptr->player_level)), 16, TRUE);
       out << player_level << blank_string(4) << to_ascii(notes_ptr->recorded_note) << QString("<br>");;
    }

    /* Dump options */
    out << QString("<br><br><br><br><b><u>[Options]</u></b>");
    for (int i = OPT_NOTABLE_HEAD; i < OPT_MAX; i++)
    {
        /* Hack - use game play options */
        if ((i > OPT_NOTABLE_TAIL) && (i < OPT_BIRTH_HEAD))
        {
            if (i != OPT_smart_cheat) continue;
        }

        /* Print the labels */
        if (i == OPT_NOTABLE_HEAD) out <<  QString("<br><br><u>GAME PLAY OPTIONS:</u><br>");
        if (i == OPT_CHEAT_HEAD) out <<  QString("<br><br><u>CHEAT OPTIONS:</u><br>");
        if (i == OPT_BIRTH_HEAD) out <<  QString("<br><br><u>BIRTH OPTIONS:</u><br>");

        if (options[i].name.isNull()) continue;

        QString opt_name = options[i].name;
        opt_name = set_html_string_length(opt_name, 25, FALSE);

        QString opt_desc = options[i].description;
        opt_desc = set_html_string_length(opt_desc, 52, FALSE);

        QString opt_setting = "off";
        if (op_ptr->opt[i]) opt_setting = " on";

        out << opt_name << opt_desc << opt_setting << QString("<br>");
    }

    out << QString("<br></pre></body></html>");

    char_info_file.close();

    char_info_file.open(QIODevice::ReadOnly);

    // Finally, replace all black text with white text
    QByteArray this_text = char_info_file.readAll();
    QString file_text = this_text;

    file_text = file_text.replace(white_text, dark_text);
    char_info_file.close();
    char_info_file.open(QIODevice::WriteOnly);
    out << file_text;
    char_info_file.close();
}

#ifdef USEFUL_UNUSED_CODE
void MainWindow::save_screenshot(void)
{
    QRect dungeon_frame = graphics_view->geometry();

    QRect screen_grab(sidebar_dock->pos(), dungeon_frame.bottomRight());

    QPixmap screenshot = main_window->grab(screen_grab);

    // Start with the current player name
    QString default_name = "player";
    if (!op_ptr->full_name.isEmpty())default_name = op_ptr->full_name;
    QString default_file = npp_dir_user.path();
    default_file.append("/");
    default_file.append(default_name);
    default_file.append("_npp_scr");

    QString file_name = QFileDialog::getSaveFileName(this, tr("Select a savefile"), default_file, tr("PNG (*.png)"));

    if (file_name.isEmpty())
        return;

    QString text = (QString("Version: %1 %2") .arg(VERSION_MODE_NAME) .arg(VERSION_STRING));

    QLabel new_label(text);
    new_label.setPixmap(screenshot);

    /*QPrinter this_printer;
    this_printer.setOutputFileName(file_name);
    this_printer.setOutputFormat(QPrinter::PdfFormat);*/

    screenshot.save(file_name, "PNG", 100);




    /*text.append(QString("Name: %1<br>") .arg(op_ptr->full_name));
    text.append(QString("Race: %1<br>") .arg(p_ptr->prace));
    text.append(QString("Class: %1<br>") .arg(p_ptr->pclass));
    text.append(QString("Level: %1<br>") .arg(p_ptr->lev));*/


}
#endif //USEFUL_UNUSED_CODE
