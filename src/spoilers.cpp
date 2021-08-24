/*
 * Copyright (c) 2015 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/init.h>
#include <src/npp.h>
#include <src/knowledge.h>
#include <QTextStream>
#include <QTextDocument>



void print_monster_spoiler_file(void)
{
    QString spoil_mon_filename = "mon_spoiler.rtf";
    QFile spoil_mon_file;

    if (game_mode == GAME_NPPANGBAND) spoil_mon_filename.prepend("nppangband_");
    else spoil_mon_filename.prepend("nppmoria_"); /* (game_mode == GAME_NPPMORIA) */

    /* Build the filename */
    spoil_mon_file.setFileName(QString("%1/%2" ) .arg(npp_dir_user.path()) .arg(spoil_mon_filename));

    if (!spoil_mon_file.open(QIODevice::WriteOnly | QIODevice::Text)) return;

    QTextStream out(&spoil_mon_file);

    QTextDocument mon_txt_doc;

    QString output = QString("<big># Monster Spoiler File </big>");

    for (int i = 1; i < z_info->r_max; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Unused slot */
        if (r_ptr->r_name_full.isEmpty()) continue;
        if (r_ptr->is_player_ghost()) continue;

        QString race_name_desc = QString("<br><br><big><b>%1</b></big><br>") .arg(monster_desc_race(i));
        race_name_desc.append(get_monster_description(i, TRUE, NULL, FALSE)); 

        output.append(race_name_desc);
    }

    mon_txt_doc.setHtml(output);

    out << mon_txt_doc.toHtml();

    /* Close and save  */
    spoil_mon_file.close();
}


void print_terrain_spoiler_file(void)
{
    QString spoil_terrain_filename = "terrain_spoiler.rtf";
    QFile spoil_terrain_file;

    if (game_mode == GAME_NPPANGBAND) spoil_terrain_filename.prepend("nppangband_");
    else spoil_terrain_filename.prepend("nppmoria_"); /* (game_mode == GAME_NPPMORIA) */

    /* Build the filename */
    spoil_terrain_file.setFileName(QString("%1/%2" ) .arg(npp_dir_user.path()) .arg(spoil_terrain_filename));

    if (!spoil_terrain_file.open(QIODevice::WriteOnly | QIODevice::Text)) return;

    QTextStream out(&spoil_terrain_file);

    QTextDocument terrain_txt_doc;

    QString output = QString("<big># Terrain Spoiler File </big>");

    for (int i = 1; i < z_info->f_max; i++)
    {
        feature_type *f_ptr = &f_info[i];

        /* Unused slot */
        if (f_ptr->f_name.isEmpty()) continue;

        QString race_name_desc = QString("<br><br><big><b>%1</b></big><br>") .arg(feature_desc(i, TRUE, FALSE));
        race_name_desc.append(get_feature_description(i, TRUE, FALSE));

        output.append(race_name_desc);
    }

    terrain_txt_doc.setHtml(output);

    out << terrain_txt_doc.toHtml();

    /* Close and save  */
    spoil_terrain_file.close();
}


void print_object_spoiler_file(void)
{
    QString spoil_object_filename = "object_spoiler.rtf";
    QFile spoil_object_file;

    if (game_mode == GAME_NPPANGBAND) spoil_object_filename.prepend("nppangband_");
    else spoil_object_filename.prepend("nppmoria_"); /* (game_mode == GAME_NPPMORIA) */

    /* Build the filename */
    spoil_object_file.setFileName(QString("%1/%2" ) .arg(npp_dir_user.path()) .arg(spoil_object_filename));

    if (!spoil_object_file.open(QIODevice::WriteOnly | QIODevice::Text)) return;

    QTextStream out(&spoil_object_file);

    QTextDocument object_txt_doc;

    QString output = QString("<big># Object Spoiler File </big>");

    for (int i = 1; i < z_info->k_max; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /* Unused slot, gold, or an artifact template */
        if (!k_ptr->k_name.length()) continue;
        if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;
        if (k_ptr->tval == TV_GOLD) continue;

        object_type object_type_body;
        object_type *o_ptr = &object_type_body;

        make_object_fake(o_ptr, i, 0, FALSE);

        QString o_name = object_desc(o_ptr, ODESC_PREFIX);
        o_name = capitalize_first(o_name);

        QString object_name_desc = QString("<br><br><big><b>%1</b></big><br>") .arg(o_name);
        if (k_ptr->k_text.length())
        {
            object_name_desc.append(k_ptr->k_text);
            object_name_desc.append("<br>");
        }
        object_name_desc.append(object_info_out(o_ptr, FALSE, FALSE));

        output.append(object_name_desc);
    }

    object_txt_doc.setHtml(output);

    out << object_txt_doc.toHtml();

    /* Close and save the ghost_template file */
    spoil_object_file.close();
}

void print_ego_item_spoiler_file(void)
{
    QString spoil_ego_item_filename = "ego_item_spoiler.rtf";
    QFile spoil_ego_item_file;

    if (game_mode == GAME_NPPANGBAND) spoil_ego_item_filename.prepend("nppangband_");
    else spoil_ego_item_filename.prepend("nppmoria_"); /* (game_mode == GAME_NPPMORIA) */

    /* Build the filename */
    spoil_ego_item_file.setFileName(QString("%1/%2" ) .arg(npp_dir_user.path()) .arg(spoil_ego_item_filename));

    if (!spoil_ego_item_file.open(QIODevice::WriteOnly | QIODevice::Text)) return;

    QTextStream out(&spoil_ego_item_file);

    QTextDocument ego_item_txt_doc;

    QString output = QString("<big># Object Spoiler File </big><br><br>");

    for (int i = 1; i < z_info->e_max; i++)
    {
        ego_item_type *e_ptr = &e_info[i];

        /* Unused slot */
        if (!e_ptr->e_name.length()) continue;

        object_type object_type_body;
        object_type *o_ptr = &object_type_body;

        int k_idx = find_first_ego_match(i);
        if (!k_idx) continue;

        make_object_fake(o_ptr, k_idx, 0, FALSE);

        QString o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

        output.append(desc_ego_fake(i, capitalize_first(o_name), FALSE));
        output.append("<br><br>");
    }

    ego_item_txt_doc.setHtml(output);

    out << ego_item_txt_doc.toHtml();

    /* Close and save the ghost_template file */
    spoil_ego_item_file.close();
}

void print_artifact_spoiler_file(void)
{
    QString spoil_artifact_filename = "artifact_spoiler.rtf";
    QFile spoil_artifact_file;

    if (game_mode == GAME_NPPANGBAND) spoil_artifact_filename.prepend("nppangband_");
    else spoil_artifact_filename.prepend("nppmoria_"); /* (game_mode == GAME_NPPMORIA) */

    /* Build the filename */
    spoil_artifact_file.setFileName(QString("%1/%2" ) .arg(npp_dir_user.path()) .arg(spoil_artifact_filename));

    if (!spoil_artifact_file.open(QIODevice::WriteOnly | QIODevice::Text)) return;

    QTextStream out(&spoil_artifact_file);

    QTextDocument artifact_txt_doc;

    QString output = QString("<big># Artifact Spoiler File </big><br><br>");

    for (int i = 1; i < z_info->art_norm_max; i++)
    {
        artifact_type *a_ptr = &a_info[i];

        /* Unused slot */
        if (!a_ptr->a_name.length()) continue;

        object_type object_type_body;
        object_type *o_ptr = &object_type_body;

        make_fake_artifact(o_ptr, i);

        QString o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_BASE | ODESC_SPOIL);
        o_name = capitalize_first(o_name);

        QString object_name_desc = QString("<br><br><big><b>%1</b></big><br>") .arg(o_name);
        if (a_ptr->a_text.length())
        {
            object_name_desc.append(a_ptr->a_text);
            object_name_desc.append("<br>");
        }
        object_name_desc.append(object_info_out(o_ptr, FALSE, FALSE));

        output.append(object_name_desc);

        output.append("<br><br>");
    }

    artifact_txt_doc.setHtml(output);

    out << artifact_txt_doc.toHtml();

    /* Close and save the ghost_template file */
    spoil_artifact_file.close();
}
