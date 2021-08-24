/* File: wizard1.cpp */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
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

#include "src/wizard_mode.h"
#include <QBoxLayout>
#include <QGridLayout>
#include <QLabel>
#include <QSpinBox>
#include <QPushButton>


static bool wiz_alloc_artifact(object_type *o_ptr, int art_num)
{
    artifact_type *a_ptr = a_info + art_num;

    if (a_ptr->tval + a_ptr->sval == 0) return false;

    //if (a_ptr->a_cur_num > 0) return false;

    int k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

    if (!k_idx) return false;

    o_ptr->object_wipe();

    object_prep(o_ptr, k_idx);

    object_into_artifact(o_ptr, a_ptr);

    o_ptr->art_num = art_num;

    object_history(o_ptr, ORIGIN_CHEAT, -1);

    a_ptr->a_cur_num = 1;

    return true;
}


EditObjectDialog::EditObjectDialog(void)
{
    QVBoxLayout *vlay = new QVBoxLayout;

    QGridLayout *edit_info = new QGridLayout;

    object_type *o_ptr;
    int item;

    /* Get an item */
    QString q = "Choose an item to edit. ";
    QString s = "You have no eligible items.";
    /* Only accept legal items. */
    item_tester_hook = item_tester_hook_not_artifact;

    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER))) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    QLabel *obj_label = new QLabel(QString("<b><big>Editing %1</big></b>") .arg(object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL)));
    obj_label->setAlignment(Qt::AlignCenter);

    vlay->addWidget(obj_label);

    // Edit quantity
    QLabel *quant_label = new QLabel("Edit quantity");
    QSpinBox *quant_spinner = new QSpinBox;
    quant_spinner->setRange(1,MAX_STACK_SIZE);
    quant_spinner->setValue(o_ptr->number);
    edit_info->addWidget(quant_label, 1, 0);
    edit_info->addWidget(quant_spinner, 1, 1);

    // Pval
    QLabel *pval_label = new QLabel("Edit pval");
    QSpinBox *pval_spinner = new QSpinBox;
    pval_spinner->setRange(-99,99);
    pval_spinner->setValue(o_ptr->pval);
    if (o_ptr->is_wieldable())
    {
        edit_info->addWidget(pval_label, 2, 0);
        edit_info->addWidget(pval_spinner, 2, 1);
    }

    // To-hit
    QLabel *to_h_label = new QLabel("Edit To-Hit");
    QSpinBox *to_h_spinner = new QSpinBox;
    to_h_spinner->setRange(-99,99);
    to_h_spinner->setValue(o_ptr->to_h);

    if (o_ptr->is_wieldable() || o_ptr->is_ammo())
    {
        edit_info->addWidget(to_h_label, 3, 0);
        edit_info->addWidget(to_h_spinner, 3, 1);
    }

    // To-damage
    QLabel *to_d_label = new QLabel("Edit To-Dam");
    QSpinBox *to_d_spinner = new QSpinBox;
    to_d_spinner->setRange(-99,99);
    to_d_spinner->setValue(o_ptr->to_d);
    if (o_ptr->is_wieldable() || o_ptr->is_ammo())
    {
        edit_info->addWidget(to_d_label, 4, 0);
        edit_info->addWidget(to_d_spinner, 4, 1);
    }

    // Armor Class
    QLabel *to_ac_label = new QLabel("Edit Armor Class");
    QSpinBox *to_ac_spinner = new QSpinBox;
    to_ac_spinner->setRange(-99,99);
    to_ac_spinner->setValue(o_ptr->to_d);
    if (o_ptr->is_wieldable())
    {
        edit_info->addWidget(to_ac_label, 5, 0);
        edit_info->addWidget(to_ac_spinner, 5, 1);
    }


    QPushButton *close_button = new QPushButton(tr("&Close"));
    connect(close_button, SIGNAL(clicked()), this, SLOT(close()));


    vlay->addLayout(edit_info);
    vlay->addStretch();
    vlay->addWidget(close_button);

    setLayout(vlay);
    setWindowTitle("Object Editor");
    this->exec();

    o_ptr->number = quant_spinner->value();
    o_ptr->pval = pval_spinner->value();
    o_ptr->to_h = to_h_spinner->value();
    o_ptr->to_d = to_d_spinner->value();
    o_ptr->to_a = to_ac_spinner->value();

    /* Combine and Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Update stuff */
    p_ptr->update |= (PU_TORCH | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

    handle_stuff();
}


EditCharacterDialog::EditCharacterDialog(void)
{
    QGridLayout *edit_info = new QGridLayout;
    QVBoxLayout *vlay = new QVBoxLayout;

    // First allow the 6 stats to be edited
    // Strength
    QLabel *str_label = new QLabel("Strength");
    QSpinBox *str_spinner = new QSpinBox;
    str_spinner->setRange(3,118);
    str_spinner->setValue(p_ptr->stat_base_cur[A_STR]);
    edit_info->addWidget(str_label, 1, 0);
    edit_info->addWidget(str_spinner, 1, 1);

    // Intelligence
    QLabel *int_label = new QLabel("Intelligence");
    QSpinBox *int_spinner = new QSpinBox;
    int_spinner->setRange(3,118);
    int_spinner->setValue(p_ptr->stat_base_cur[A_INT]);
    edit_info->addWidget(int_label, 2, 0);
    edit_info->addWidget(int_spinner, 2, 1);

    // Wisdom
    QLabel *wis_label = new QLabel("Wisdom");
    QSpinBox *wis_spinner = new QSpinBox;
    wis_spinner->setRange(3,118);
    wis_spinner->setValue(p_ptr->stat_base_cur[A_WIS]);
    edit_info->addWidget(wis_label, 3, 0);
    edit_info->addWidget(wis_spinner, 3, 1);

    // Dexterity
    QLabel *dex_label = new QLabel("Dexterity");
    QSpinBox *dex_spinner = new QSpinBox;
    dex_spinner->setRange(3,118);
    dex_spinner->setValue(p_ptr->stat_base_cur[A_DEX]);
    edit_info->addWidget(dex_label, 4, 0);
    edit_info->addWidget(dex_spinner, 4, 1);

    // Constitution
    QLabel *con_label = new QLabel("Constitution");
    QSpinBox *con_spinner = new QSpinBox;
    con_spinner->setRange(3,118);
    con_spinner->setValue(p_ptr->stat_base_cur[A_CON]);
    edit_info->addWidget(con_label, 5, 0);
    edit_info->addWidget(con_spinner, 5, 1);

    // Charisma
    QLabel *chr_label = new QLabel("Charisma");
    QSpinBox *chr_spinner = new QSpinBox;
    chr_spinner->setRange(3,118);
    chr_spinner->setValue(p_ptr->stat_base_cur[A_CHR]);
    edit_info->addWidget(chr_label, 6, 0);
    edit_info->addWidget(chr_spinner, 6, 1);

    // Gold
    QLabel *gold_label = new QLabel("Gold");
    QSpinBox *gold_spinner = new QSpinBox;
    gold_spinner->setRange(0,500000000);
    gold_spinner->setValue(p_ptr->au);
    gold_spinner->setSingleStep(1000000);
    edit_info->addWidget(gold_label, 7, 0);
    edit_info->addWidget(gold_spinner, 7, 1);

    // Experience
    QLabel *exp_label = new QLabel("Experience");
    QSpinBox *exp_spinner = new QSpinBox;
    exp_spinner->setRange(0,10000000);
    exp_spinner->setValue(p_ptr->max_exp);
    exp_spinner->setSingleStep(10000);
    edit_info->addWidget(exp_label, 8, 0);
    edit_info->addWidget(exp_spinner, 8, 1);

        // Experience
    QLabel *fame_label = new QLabel("Fame");
    QSpinBox *fame_spinner = new QSpinBox;
    fame_spinner->setRange(0,5000);
    fame_spinner->setValue(p_ptr->q_fame);
    fame_spinner->setSingleStep(100);
    edit_info->addWidget(fame_label, 9, 0);
    edit_info->addWidget(fame_spinner, 9, 1);

    QPushButton *close_button = new QPushButton(tr("&Close"));
    connect(close_button, SIGNAL(clicked()), this, SLOT(close()));

    vlay->addLayout(edit_info);
    vlay->addStretch();
    vlay->addWidget(close_button);

    setLayout(vlay);

    setWindowTitle(tr("Character Edit Screen"));
    this->exec();

    p_ptr->stat_base_cur[A_STR] = p_ptr->stat_base_max[A_STR] = str_spinner->value();
    p_ptr->stat_base_cur[A_INT] = p_ptr->stat_base_max[A_INT] = int_spinner->value();
    p_ptr->stat_base_cur[A_WIS] = p_ptr->stat_base_max[A_WIS] = wis_spinner->value();
    p_ptr->stat_base_cur[A_DEX] = p_ptr->stat_base_max[A_DEX] = dex_spinner->value();
    p_ptr->stat_base_cur[A_CON] = p_ptr->stat_base_max[A_CON] = con_spinner->value();
    p_ptr->stat_base_cur[A_CHR] = p_ptr->stat_base_max[A_CHR] = chr_spinner->value();
    p_ptr->au = gold_spinner->value();
    p_ptr->max_exp = p_ptr->exp = exp_spinner->value();
    check_experience();
    p_ptr->q_fame = fame_spinner->value();


        /* Combine and Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Update stuff */
    p_ptr->update |= (PU_TORCH | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

    /* Fully update the visuals */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw everything */
    p_ptr->redraw |= (PR_STATUSBAR | PR_SIDEBAR_PL | PR_MAP | PR_WIN_INVENTORY | PR_WIN_EQUIPMENT |
                      PR_MESSAGES | PR_WIN_MON_RECALL | PR_WIN_OBJ_RECALL | PR_WIN_MESSAGES |
                      PR_WIN_OBJLIST | PR_WIN_FEAT_RECALL);

    handle_stuff();
}


//Helper function to get the complete artifacts name.
QString MakeArtifactDialog::get_artifact_display_name(int a_idx)
{
    object_type object_type_body;
    object_type *o_ptr = &object_type_body;

    /* Make fake artifact */
    o_ptr = &object_type_body;
    o_ptr->object_wipe();
    if (!make_fake_artifact(o_ptr, a_idx)) return ("Error");

    return (object_desc(o_ptr, ODESC_PREFIX | ODESC_BASE | ODESC_SPOIL));
}

// Record the new selected item
void MakeArtifactDialog::update_art_choice(int choice)
{
    art_num = choice;
}

// Select an artifact from a list and place it in the player's inventory
MakeArtifactDialog::MakeArtifactDialog(void)
{
    int i;
    QVBoxLayout *vlay = new QVBoxLayout;
    art_choice = new QComboBox;
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;


    QLabel *obj_label = new QLabel(QString("<b><big>Please select an artifact:</big></b>"));
    obj_label->setAlignment(Qt::AlignCenter);

    vlay->addWidget(obj_label);
    vlay->addStretch();

    connect(art_choice, SIGNAL(currentIndexChanged(int)), this, SLOT(update_art_choice(int)));

    QPushButton *close_button = new QPushButton(tr("&Close"));
    connect(close_button, SIGNAL(clicked()), this, SLOT(close()));

    int count = 0;
    art_num = 0;

    for (i = 1; i < z_info->art_norm_max; i++)
    {
        artifact_type *a_ptr = &a_info[i];

        /* Skip "empty" items */
        if (a_ptr->tval + a_ptr->sval == 0) continue;

        art_choice->addItem(QString("%1") .arg(i));

        art_choice->setItemText(count++, get_artifact_display_name(i));
    }

    vlay->addWidget(art_choice);
    vlay->addStretch();
    vlay->addWidget(close_button);

    setLayout(vlay);

    setWindowTitle(tr("Make Artifact"));
    this->exec();

    // find the artifact
    count = 0;
    for (i = 1; i < z_info->art_norm_max; i++)
    {
        artifact_type *a_ptr = &a_info[i];

        /* Skip "empty" items */
        if (a_ptr->tval + a_ptr->sval == 0) continue;

        // Found the match
        if (count == art_num) break;
        count++;
    }

    i_ptr->object_wipe();
    if (!wiz_alloc_artifact(i_ptr, i))
    {

     return;
    }
    object_history(i_ptr, ORIGIN_CHEAT, 0);
    identify_object(i_ptr, true);
    if(inven_carry(i_ptr) < 0)
    {
        drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
    }
}

//Helper function to get the complete artifacts name.
QString MakeObjectDialog::get_object_display_name(int o_idx)
{
    object_type object_type_body;
    object_type *o_ptr = &object_type_body;

    /* Make fake artifact */
    o_ptr = &object_type_body;
    o_ptr->object_wipe();
    object_prep(o_ptr, o_idx);

    //  This is necessary to keep the game from freezing on dragon armor
    object_level = k_info[o_idx].k_level;

    apply_magic(o_ptr, k_info[o_idx].k_level, FALSE, FALSE, FALSE, FALSE);
    o_ptr->mark_fully_known(TRUE);

    object_level = p_ptr->depth;

    return (object_desc(o_ptr, ODESC_BASE | ODESC_SPOIL));
}

// Record the new selected item
void MakeObjectDialog::update_obj_choice(int choice)
{
    obj_num = choice;
}

// Select an artifact from a list and place it in the player's inventory.
// Note that dragon armor comes out doesn't work too well, since it
// is always make into an ego item.
MakeObjectDialog::MakeObjectDialog(void)
{
    int i;
    QVBoxLayout *vlay = new QVBoxLayout;
    obj_choice = new QComboBox;
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;


    QLabel *obj_label = new QLabel(QString("<b><big>Please select an object:</big></b>"));
    obj_label->setAlignment(Qt::AlignCenter);

    vlay->addWidget(obj_label);
    vlay->addStretch();

    connect(obj_choice, SIGNAL(currentIndexChanged(int)), this, SLOT(update_obj_choice(int)));

    QPushButton *close_button = new QPushButton(tr("&Close"));
    connect(close_button, SIGNAL(clicked()), this, SLOT(close()));

    int count = 0;
    obj_num = 0;

    for (i = 1; i < z_info->k_max; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /* Skip "empty" items */
        if (k_ptr->k_name.isEmpty()) continue;
        // Skip artifact templates
        if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;
        // Skip gold
        if (k_ptr->tval == TV_GOLD) continue;

        obj_choice->addItem(QString("%1") .arg(i));

        obj_choice->setItemText(count++, get_object_display_name(i));
    }

    vlay->addWidget(obj_choice);
    vlay->addStretch();
    vlay->addWidget(close_button);

    setLayout(vlay);

    setWindowTitle(tr("Make Object"));
    this->exec();

    // find the object
    count = 0;
    for (i = 1; i < z_info->k_max; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /* Skip "empty" items */
        if (k_ptr->k_name.isEmpty()) continue;
        // Skip artifact templates
        if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;
        // Skip gold
        if (k_ptr->tval == TV_GOLD) continue;

        // Found the match
        if (count == obj_num) break;
        count++;
    }

    //  This is necessary to keep the game from freezing on dragon armor
    object_level = k_info[i].k_level;

    i_ptr->object_wipe();
    object_prep(i_ptr, i);
    apply_magic(i_ptr, p_ptr->depth, FALSE, FALSE, FALSE, FALSE);
    object_history(i_ptr, ORIGIN_CHEAT, 0);
    identify_object(i_ptr, true);
    if(inven_carry(i_ptr) < 0)
    {
        drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
    }

    object_level = p_ptr->depth;
}

//Helper function to get the complete monster name.
QString MakeMonsterDialog::get_mon_display_name(int r_idx)
{
    return (monster_desc_race(r_idx));
}
// Record the new selected monster
void MakeMonsterDialog::update_mon_choice(int choice)
{
    mon_num = choice;
}

// Select a monster from a list and place it in the dungeon.
MakeMonsterDialog::MakeMonsterDialog(void)
{
    int i;
    QVBoxLayout *vlay = new QVBoxLayout;
    mon_choice = new QComboBox;
    int y, x;

    QLabel *mon_label = new QLabel(QString("<b><big>Please select a monster:</big></b>"));
    mon_label->setAlignment(Qt::AlignCenter);

    vlay->addWidget(mon_label);
    vlay->addStretch();

    connect(mon_choice, SIGNAL(currentIndexChanged(int)), this, SLOT(update_mon_choice(int)));

    QPushButton *close_button = new QPushButton(tr("&Close"));
    connect(close_button, SIGNAL(clicked()), this, SLOT(close()));

    int count = 0;
    mon_num = 0;

    for (i = 1; i < z_info->r_max; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Skip "empty" items */
        if (r_ptr->r_name_full.isEmpty()) continue;
        // Skip player_ghost templates
        if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

        mon_choice->addItem(QString("%1") .arg(i));

        mon_choice->setItemText(count++, get_mon_display_name(i));
    }

    vlay->addWidget(mon_choice);
    vlay->addStretch();
    vlay->addWidget(close_button);

    setLayout(vlay);

    setWindowTitle(tr("Make Monster"));
    this->exec();

    // find the monster
    count = 0;
    for (i = 1; i < z_info->r_max; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Skip "empty" items */
        if (r_ptr->r_name_full.isEmpty()) continue;
        // Skip player_ghost templates
        if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

        // Found the match
        if (count == mon_num) break;
        count++;
    }

    /* Try 10 times */
    for (int k = 0; k < 10; k++)
    {
        int d = 2;

        /* Pick a location */
        scatter(&y, &x, p_ptr->py, p_ptr->px, d, 0);

        /* Require empty grids */
        if (!cave_empty_bold(y, x)) continue;

        /* Place it (allow groups) */
        if (place_monster_aux(y, x, i, (MPLACE_GROUP | MPLACE_OVERRIDE | MPLACE_SLEEP))) break;
    }
}

//Helper function to get the complete monster name.
QString MakeFeatureDialog::get_feat_display_name(int f_idx)
{
    return (feature_desc(f_idx, FALSE, FALSE));
}

// Record the new selected monster
void MakeFeatureDialog::update_feat_choice(int choice)
{
    feat_num = choice;
}

// Select a monster from a list and place it in the dungeon.
MakeFeatureDialog::MakeFeatureDialog(void)
{
    int i;
    QVBoxLayout *vlay = new QVBoxLayout;
    feat_choice = new QComboBox;

    QLabel *feat_label = new QLabel(QString("<b><big>Please select a feature:</big></b>"));
    feat_label->setAlignment(Qt::AlignCenter);

    vlay->addWidget(feat_label);
    vlay->addStretch();

    connect(feat_choice, SIGNAL(currentIndexChanged(int)), this, SLOT(update_feat_choice(int)));

    QPushButton *close_button = new QPushButton(tr("&Close"));
    connect(close_button, SIGNAL(clicked()), this, SLOT(close()));

    int count = 0;
    feat_num = 0;

    for (i = 1; i < z_info->f_max; i++)
    {
        /* Get the feature */
        feature_type *f_ptr = &f_info[i];

        if (f_ptr->f_name.isEmpty()) continue;

        feat_choice->addItem(QString("%1") .arg(i));

        feat_choice->setItemText(count++, get_feat_display_name(i));
    }

    vlay->addWidget(feat_choice);
    vlay->addStretch();
    vlay->addWidget(close_button);

    setLayout(vlay);

    setWindowTitle(tr("Make Feature"));
    this->exec();

    // find the feature
    count = 0;
    for (i = 1; i < z_info->f_max; i++)
    {
        /* Get the feature */
        feature_type *f_ptr = &f_info[i];

        if (f_ptr->f_name.isEmpty()) continue;

        // Found the match
        if (count == feat_num) break;
        count++;
    }

        /* Pick a location */
    if (!target_set_interactive(TARGET_GRID, -1, -1)) return;

    /* Paranoia */
    if (!p_ptr->target_set) return;

    int y = p_ptr->target_row;
    int x = p_ptr->target_col;

    /* Paranoia */
    if (dungeon_info[y][x].has_object())
    {
        pop_up_message_box("Must be an empty grid");
        return;
    }

    if (feat_ff2_match(i, FF2_EFFECT))
    {
        feature_type *f_ptr = &f_info[i];

        int gf_type = f_ptr->x_gf_type;

        if (feat_ff2_match(i, FF2_TRAP_SMART))
        {
            QString dummy_string;
            u16b flags = fire_trap_smart(i, y, x, MODE_FLAGS, &dummy_string);

            set_effect_trap_smart(i, y, x, flags);
        }
        else if (feat_ff2_match(i, FF2_TRAP_PASSIVE)) set_effect_trap_passive(i, y, x);
        else if (feat_ff2_match(i, FF2_TRAP_MON)) set_effect_trap_player(i, y, x);
        else if (i == FEAT_GLYPH_WARDING) set_effect_glyph(y, x);
        else if (i == FEAT_WALL_GLACIER) set_effect_glacier(i, y, x, SOURCE_EFFECT, 0);
        else if (i == FEAT_WALL_INSCRIPTION) set_effect_inscription(i, y, x, SOURCE_EFFECT, 0);
        else if ((i == FEAT_RUBBLE) || (i == FEAT_RUBBLE_HIDDEN_OBJECT) ||
                 (i == FEAT_LOOSE_ROCK)) set_effect_rocks(i, y, x);
        else if (i == FEAT_EFFECT_SMOKE) set_effect_lingering_cloud(FEAT_EFFECT_SMOKE, y, x, 100, SOURCE_OTHER, 0);
         else if (i == FEAT_EFFECT_FOG) set_effect_permanent_cloud(i, y, x, 0, 0);

        // This list should be kept current with the function project_x
        else switch (gf_type)
        {
            case GF_COLD:
            case GF_ACID:
            case GF_ELEC:
            case GF_POIS:
            case GF_BWATER:
            case GF_BMUD:
            case GF_FIRE:
            case GF_LAVA:
            case GF_SPORE:
            case GF_NETHER:
            case GF_CHAOS:
            case GF_DISENCHANT:
            case GF_NEXUS:
            case GF_TIME:
            case GF_CONFUSION:
            case GF_SHARD:
            {
                set_effect_lingering_cloud(i, y, x, 50, SOURCE_OTHER, 0);
                break;
            }
            case GF_GRAVITY:
            case GF_INERTIA_NPP:
            case GF_LIFE_DRAIN:
            case GF_LIGHT:
            case GF_DARK:
            case GF_ELEC_BURST:
            case GF_METEOR:
            {

                set_effect_shimmering_cloud(i, y, x, 50, 50, SOURCE_OTHER, 0);
                break;
            }
            default :break;
        }
    }

    /* Create the feature */
    else cave_set_feat(y, x, i);
}

// Completely cure the player
void WizardModeDialog::wiz_cure_all(void)
{
    /*
     * Cure everything instantly
     */
    /* Remove curses */
    (void)remove_all_curse();

    /* Restore stats */
    (void)res_stat(A_STR);
    (void)res_stat(A_INT);
    (void)res_stat(A_WIS);
    (void)res_stat(A_CON);
    (void)res_stat(A_DEX);
    (void)res_stat(A_CHR);

    /* Restore the level */
    (void)restore_level();

    /* Heal the player */
    p_ptr->chp = p_ptr->mhp;
    p_ptr->chp_frac = 0;

    /* Restore mana */
    p_ptr->csp = p_ptr->msp;
    p_ptr->csp_frac = 0;

    /* Cure stuff */
    (void)clear_timed(TMD_BLIND, TRUE);
    (void)clear_timed(TMD_CONFUSED, TRUE);
    (void)clear_timed(TMD_POISONED, TRUE);
    (void)clear_timed(TMD_AFRAID, TRUE);
    (void)clear_timed(TMD_PARALYZED, TRUE);
    (void)clear_timed(TMD_IMAGE, TRUE);
    (void)clear_timed(TMD_STUN, TRUE);
    (void)clear_timed(TMD_CUT, TRUE);
    (void)clear_timed(TMD_SLOW, TRUE);

    /* No longer hungry */
    (void)set_food(PY_FOOD_MAX - 1);

    this->accept();

    /* Combine and Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Update stuff */
    p_ptr->update |= (PU_TORCH | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

    /* Fully update the visuals */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw everything */
    p_ptr->redraw |= (PR_STATUSBAR | PR_SIDEBAR_PL | PR_MAP | PR_WIN_INVENTORY | PR_WIN_EQUIPMENT |
                      PR_MESSAGES | PR_WIN_MON_RECALL | PR_WIN_OBJ_RECALL |  PR_WIN_MESSAGES |
                      PR_WIN_OBJLIST | PR_WIN_FEAT_RECALL);

    handle_stuff();
}

void WizardModeDialog::wiz_create_artifact()
{
    MakeArtifactDialog();
    this->accept();
}

void WizardModeDialog::wiz_create_object()
{
    MakeObjectDialog();
    this->accept();
}

void WizardModeDialog::wiz_winners_kit(void)
{
    if (!character_dungeon) return;

    if (game_mode == GAME_NPPMORIA)
    {
        // Make 2 rings of speed
        int k_idx = lookup_kind(TV_RING, SV_RING_SPEED);
        object_type object_type_body;
        object_type *i_ptr = &object_type_body;
        i_ptr->object_wipe();
        if (k_idx)
        {
            object_prep(i_ptr, k_idx);
            apply_magic(i_ptr, k_info[k_idx].k_level, FALSE, TRUE, TRUE, TRUE);
            i_ptr->number = 2;
            i_ptr->mark_fully_known(TRUE);
            object_history(i_ptr, ORIGIN_CHEAT, 0);
            if(inven_carry(i_ptr) < 0)
            {
                drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
            }
        }
        //Give an amulet of the magi;
        k_idx = lookup_kind(TV_AMULET, SV_AMULET_THE_MAGI);
        if (k_idx)
        {
            i_ptr->object_wipe();
            object_prep(i_ptr, k_idx);
            apply_magic(i_ptr, k_info[k_idx].k_level, FALSE, TRUE, TRUE, TRUE);
            i_ptr->mark_fully_known(TRUE);
            object_history(i_ptr, ORIGIN_CHEAT, 0);
            if(inven_carry(i_ptr) < 0)
            {
                drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
            }
        }
        //boots of speed
        k_idx = lookup_kind(TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS);
        int ego_num = lookup_ego(TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS, "speed");
        if (k_idx && ego_num)
        {
            i_ptr->object_wipe();
            object_prep(i_ptr, k_idx);
            i_ptr->ego_num = ego_num;
            a_m_aux_2(i_ptr, k_info[k_idx].k_level, 2);
            apply_ego_item_magic(i_ptr, k_info[k_idx].k_level);
            i_ptr->to_a = 25;
            i_ptr->mark_fully_known(TRUE);
            object_history(i_ptr, ORIGIN_CHEAT, 0);
            if(inven_carry(i_ptr) < 0)
            {
                drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
            }
        }
        // Robe of Resistance
        k_idx = lookup_kind(TV_SOFT_ARMOR, SV_ROBE);
        ego_num = lookup_ego(TV_SOFT_ARMOR, SV_ROBE, "resistance");
        if (k_idx && ego_num)
        {
            i_ptr->object_wipe();
            object_prep(i_ptr, k_idx);
            i_ptr->ego_num = ego_num;
            a_m_aux_2(i_ptr, k_info[k_idx].k_level, 2);
            apply_ego_item_magic(i_ptr, k_info[k_idx].k_level);
            i_ptr->to_a = 25;
            i_ptr->mark_fully_known(TRUE);
            object_history(i_ptr, ORIGIN_CHEAT, 0);
            if(inven_carry(i_ptr) < 0)
            {
                drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
            }
        }
        // super-charged holy avenger dagger
        k_idx = lookup_kind(TV_SWORD, SV_DAGGER);
        ego_num = lookup_ego(TV_SWORD, SV_DAGGER, "holy avenger");
        if (k_idx && ego_num)
        {
            i_ptr->object_wipe();
            object_prep(i_ptr, k_idx);
            i_ptr->ego_num = ego_num;
            a_m_aux_1(i_ptr, k_info[k_idx].k_level, 2);
            apply_ego_item_magic(i_ptr, k_info[k_idx].k_level);
            i_ptr->to_a = i_ptr->to_h = i_ptr->to_d = 25;
            i_ptr->dd = i_ptr->ds = 9;
            i_ptr->mark_fully_known(TRUE);
            object_history(i_ptr, ORIGIN_CHEAT, 0);
            if(inven_carry(i_ptr) < 0)
            {
                drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
            }
        }
        // crown of the magi
        k_idx = lookup_kind(TV_CROWN, SV_SILVER_CROWN);
        ego_num = lookup_ego(TV_CROWN, SV_SILVER_CROWN, "magi");
        if (k_idx && ego_num)
        {
            i_ptr->object_wipe();
            object_prep(i_ptr, k_idx);
            i_ptr->ego_num = ego_num;
            a_m_aux_2(i_ptr, k_info[k_idx].k_level, 2);
            apply_ego_item_magic(i_ptr, k_info[k_idx].k_level);
            i_ptr->to_a = 25;
            i_ptr->mark_fully_known(TRUE);
            object_history(i_ptr, ORIGIN_CHEAT, 0);
            if(inven_carry(i_ptr) < 0)
            {
                drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
            }
        }
        // super charged gloves of slaying
        k_idx = lookup_kind(TV_GLOVES, SV_SET_OF_LEATHER_GLOVES);
        ego_num = lookup_ego(TV_GLOVES, SV_SET_OF_LEATHER_GLOVES, "slaying");
        if (k_idx && ego_num)
        {
            i_ptr->object_wipe();
            object_prep(i_ptr, k_idx);
            i_ptr->ego_num = ego_num;
            a_m_aux_2(i_ptr, k_info[k_idx].k_level, 2);
            apply_ego_item_magic(i_ptr, k_info[k_idx].k_level);
            i_ptr->to_a = i_ptr->to_h = i_ptr->to_d = 25;
            i_ptr->mark_fully_known(TRUE);
            object_history(i_ptr, ORIGIN_CHEAT, 0);
            if(inven_carry(i_ptr) < 0)
            {
                drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
            }
        }
        //finally the Phial
        if (TRUE)
        {
            i_ptr->object_wipe();
            if (wiz_alloc_artifact(i_ptr, 1))
            {
                object_history(i_ptr, ORIGIN_CHEAT, 0);
                identify_object(i_ptr, true);
                if(inven_carry(i_ptr) < 0)
                {
                    drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
                }
            }
        }

        handle_stuff();
        this->accept();
        return;

    }

    else if (game_mode != GAME_NPPANGBAND) return;

    int artis[] = {
        47,     // RINGIL
        124,    // CUBRAGOL
        13,     // NARYA
        14,     // NENYA
        10,     // ELESSAR
        12,     // GEM OF AMON SUL
        38,     // BLADETUNDER
        113,    // COLANNON
        33,     // THORIN
        110,    // NUMENOR
        129,    // CAMBELEG
        127,    // FEANOR
        0
    };

    object_type obj;
    object_type *o_ptr = &obj;

    for (int i = 0; artis[i]; i++)
    {
        o_ptr->object_wipe();
        if (!wiz_alloc_artifact(o_ptr, artis[i])) continue;
        object_history(o_ptr, ORIGIN_CHEAT, 0);
        identify_object(o_ptr, true);
        if (inven_carry(o_ptr) < 0)
        {
            drop_near(o_ptr, -1, p_ptr->py, p_ptr->px);
        }
        QString name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);
        message("Allocated " + name);
    }

    //Some amazing ammo;
    int k_idx = lookup_kind(TV_BOLT, SV_AMMO_MITHRIL);
    int ego_num = lookup_ego(TV_BOLT, SV_AMMO_MITHRIL, "holy might");
    if (k_idx && ego_num)
    {
        o_ptr->object_wipe();
        object_prep(o_ptr, k_idx);
        o_ptr->ego_num = ego_num;
        a_m_aux_1(o_ptr, k_info[k_idx].k_level, 2);
        apply_ego_item_magic(o_ptr, k_info[k_idx].k_level);
        o_ptr->to_h = 99;
        o_ptr->to_d = 99;
        o_ptr->dd = 25;
        o_ptr->ds = 25;
        o_ptr->number = 99;
        o_ptr->mark_fully_known(TRUE);
        object_history(o_ptr, ORIGIN_CHEAT, 0);
        if(inven_carry(o_ptr) < 0)
        {
            drop_near(o_ptr, -1, p_ptr->py, p_ptr->px);
        }
    }

    handle_stuff();

    this->accept();
}

// Know every object kind, ego item, feature, and monster
void WizardModeDialog::wiz_know_all(void)
{
    int i, j;

    /* Knowledge of every object */
    for (i = 1; i < z_info->k_max; i++)
    {
        k_info[i].aware = TRUE;
        k_info[i].everseen = TRUE;
        k_info[i].tried = TRUE;

    }
    /* Knowledge of every ego-item */
    for (i = 1; i < z_info->e_max; i++)
    {
        e_info[i].everseen = TRUE;
    }
    /* Full knowledge of every monster */
    for (i = 1; i < z_info->r_max; i++)
    {
        monster_race *r_ptr = &r_info[i];
        monster_lore *l_ptr = &l_list[i];

        /* Know all flags */
        l_ptr->r_l_flags1 = r_ptr->flags1;
        l_ptr->r_l_flags2 = r_ptr->flags2;
        l_ptr->r_l_flags3 = r_ptr->flags3;
        l_ptr->r_l_flags4 = r_ptr->flags4;
        l_ptr->r_l_flags5 = r_ptr->flags5;
        l_ptr->r_l_flags6 = r_ptr->flags6;
        l_ptr->r_l_flags7 = r_ptr->flags7;
        l_ptr->r_l_native = r_ptr->r_native;

        /* Know max sightings, sleeping habits, spellcasting, and combat blows. */
        l_ptr->sights = SHRT_MAX;
        l_ptr->ranged = UCHAR_MAX;
        for (j = 0; j < MONSTER_BLOW_MAX; j++)
        {
            l_ptr->blows[j] = UCHAR_MAX;
        }
        l_ptr->wake = l_ptr->ignore = UCHAR_MAX;

        /* know the treasure drops*/
        l_ptr->drop_gold = l_ptr->drop_item =
        (((r_ptr->flags1 & RF1_DROP_4D2) ? 8 : 0) +
         ((r_ptr->flags1 & RF1_DROP_3D2) ? 6 : 0) +
         ((r_ptr->flags1 & RF1_DROP_2D2) ? 4 : 0) +
         ((r_ptr->flags1 & RF1_DROP_1D2) ? 2 : 0) +
         ((r_ptr->flags1 & RF1_DROP_90)  ? 1 : 0) +
         ((r_ptr->flags1 & RF1_DROP_60)  ? 1 : 0));

        /* But only "valid" treasure drops */
        if (r_ptr->flags1 & RF1_ONLY_GOLD) l_ptr->drop_item = 0;
        if (r_ptr->flags1 & RF1_ONLY_ITEM) l_ptr->drop_gold = 0;
    }

    /* Full knowledge of every feature */
    for (i = 1; i < z_info->f_max; i++)
    {
        feature_type *f_ptr = &f_info[i];
        feature_lore *f_l_ptr = &f_l_list[i];

        /* Know all flags and sites */
        f_l_ptr->f_l_flags1 = f_ptr->f_flags1;
        f_l_ptr->f_l_flags2 = f_ptr->f_flags2;
        f_l_ptr->f_l_flags3 = f_ptr->f_flags3;
        f_l_ptr->f_l_sights = UCHAR_MAX;

        /* Know all transitions */
        f_l_ptr->f_l_defaults = UCHAR_MAX;
        for (j = 0; j < MAX_FEAT_STATES; j++)
        {
            /*There isn't an action here*/
            if (f_ptr->state[j].fs_action == FS_FLAGS_END) continue;

            /* Hack -- we have seen this transition */
            f_l_ptr->f_l_state[j] = UCHAR_MAX;
        }
        /*Know movement, damage to non-native, and stealth.....*/
        f_l_ptr->f_l_dam_non_native = UCHAR_MAX;
        f_l_ptr->f_l_native_moves = UCHAR_MAX;
        f_l_ptr->f_l_non_native_moves = UCHAR_MAX;
        f_l_ptr->f_l_stealth_adj = UCHAR_MAX;
        f_l_ptr->f_l_native_to_hit_adj = UCHAR_MAX;
        f_l_ptr->f_l_non_native_to_hit_adj = UCHAR_MAX;
    }

    this->accept();
}

void WizardModeDialog::wiz_print_spoilers()
{
    print_monster_spoiler_file();
    print_terrain_spoiler_file();
    print_object_spoiler_file();
    print_ego_item_spoiler_file();
    print_artifact_spoiler_file();
    this->accept();
}

void WizardModeDialog::wiz_create_monster()
{
    MakeMonsterDialog();
    this->accept();
}

void WizardModeDialog::wiz_create_feature()
{
    this->hide();
    MakeFeatureDialog();
    this->accept();
}

// Jump to a new dungeon level
void WizardModeDialog::wiz_jump(void)
{
    int new_level;

    /* Prompt */
    QString prompt = QString("Jump to level (0-%1): ") .arg(MAX_DEPTH-1);

    new_level = get_quantity(prompt, MAX_DEPTH - 1, p_ptr->depth, TRUE);

    // Same depth - quit
    if (new_level == p_ptr->depth) return;

    /* Accept request */
    message(QString("You jump to dungeon level %1.") .arg(new_level));

    this->accept();

    /* New depth */
    dungeon_change_level(new_level);

    // Update everything
    change_player_level();
}

void WizardModeDialog::wiz_teleport_to_target(void)
{
    /* Must have a target */
    if (target_okay())
    {
        /* Teleport to the target */
        teleport_player_to(p_ptr->target_row, p_ptr->target_col);

        this->accept();
    }
    else this->reject();
}

void WizardModeDialog::wiz_phase_door(void)
{
    teleport_player(10, FALSE);
    this->accept();
}

void WizardModeDialog::wiz_teleport(void)
{
    teleport_player(100, FALSE);
    this->accept();
}


// Summon one monster
void WizardModeDialog::wiz_summon(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    (void)summon_specific(py, px, p_ptr->depth, 0, 0L);
    this->accept();
}

// Summon one monster
void WizardModeDialog::wiz_banish(void)
{
    int i;

    /* Banish everyone nearby */
    for (i = 1; i < mon_max; i++)
    {
        monster_type *m_ptr = &mon_list[i];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip distant monsters */
        if (m_ptr->cdis > (MAX_SIGHT+10)) continue;

        /* Hack -- Skip unique monsters */
        if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

        /* Quest monsters can only be "killed" by the player */
        if (m_ptr->mflag & (MFLAG_QUEST)) continue;

        /* Delete the monster */
        delete_monster_idx(i);
    }

    /* Update monster list window */
    p_ptr->redraw |= PR_WIN_MONLIST | PR_SIDEBAR_MON;

    handle_stuff();

    this->accept();
}

void WizardModeDialog::wiz_detect_all_monsters(void)
{
    int i;

    this->accept();

    /* Process monsters */
    for (i = 1; i < mon_max; i++)
    {
        monster_type *m_ptr = &mon_list[i];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Optimize -- Repair flags */
        repair_mflag_mark = TRUE;
        repair_mflag_show = TRUE;

        /* Detect the monster */
        m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

        /* Update the monster */
        update_mon(i, FALSE);
    }
}

void WizardModeDialog::wiz_edit_object(void)
{
    EditObjectDialog();
    this->accept();
}

void WizardModeDialog::wiz_edit_character(void)
{
    EditCharacterDialog();
    this->accept();
}

void WizardModeDialog::wiz_detection(void)
{
    //wiz_light();
    this->accept();
    (void)detect(DETECT_RADIUS, DETECT_ALL);  
}

void WizardModeDialog::wiz_magic_mapping(void)
{
    detect(DETECT_RADIUS + 10, DETECT_MAP);    
    this->accept();
}

void WizardModeDialog::wiz_level_light(void)
{
    wiz_light();
    handle_stuff();
    this->accept();
}

void WizardModeDialog::wiz_redraw_dungeon(void)
{
    this->accept();

    // Update everything
    change_player_level();
}

void WizardModeDialog::wiz_mass_create_items(void)
{
    int i;
    object_type object_type_body;

    object_type *i_ptr;

    this->accept();

    for(i=0; i < 25; i++)
    {
        /* Get local object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        i_ptr->object_wipe();

        /* Make a object (if possible) */
        if (!make_object(i_ptr, FALSE, FALSE, DROP_TYPE_UNTHEMED, FALSE)) continue;

        /* Drop the object */
        drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
    }

}


void WizardModeDialog::wiz_create_good_item(void)
{
    acquirement(p_ptr->py, p_ptr->px, 1, FALSE);
    this->accept();
}

void WizardModeDialog::wiz_create_great_item(void)
{
    acquirement(p_ptr->py, p_ptr->px, 1, TRUE);
    this->accept();
}

void WizardModeDialog::wiz_mass_identify_items(void)
{
    this->accept();
    (void)mass_identify(4);

}

WizardModeDialog::WizardModeDialog(void)
{
    int row = 0;

    // Paranoia
    if (!p_ptr->playing) return;

    if (!p_ptr->is_wizard)
    {
        QString prompt = color_string(QString ("<b><big>You are about to use 'Wizard Mode' commands.</big></b><br><br>"), TERM_RED);
        prompt.append("Wizard Mode contains many powerful 'cheat' commands.  ");
        prompt.append("Your savefile will be marked as a wizard mode character and will not be scored.<br><br>");
        prompt.append("Really use Wizard Mode?");
        if (!get_check(prompt)) return;
        p_ptr->is_wizard = TRUE;
    }

    main_prompt = new QLabel(QString("<b><big>Please select a command</big></b><br>"));
    main_prompt->setAlignment(Qt::AlignCenter);

    QVBoxLayout *vlay = new QVBoxLayout;

    vlay->addWidget(main_prompt);

    // Add the player related commands
    QGridLayout *wizard_layout = new QGridLayout;

    player_section = new QLabel("<b>Player commands</b>");
    player_section->setAlignment(Qt::AlignCenter);
    wizard_layout->addWidget(player_section, row, 1);

    row++;

    // Add the "cure all" button
    QPushButton *heal_button = new QPushButton("Heal Player");
    heal_button->setToolTip("Completely heal and restore the player.");
    connect(heal_button, SIGNAL(clicked()), this, SLOT(wiz_cure_all()));
    wizard_layout->addWidget(heal_button, row, 0);

    // Add the "know all" button
    QPushButton *know_button = new QPushButton("Know All");
    know_button->setToolTip("Know everything about every feature, object, and monster in the game.");
    connect(know_button, SIGNAL(clicked()), this, SLOT(wiz_know_all()));
    wizard_layout->addWidget(know_button, row, 1);

    // Add the "jump" button
    QPushButton *jump_button = new QPushButton("Jump To New Level");
    jump_button->setToolTip("Allow the player to instantly jump to any level in the dungeon.");
    connect(jump_button, SIGNAL(clicked()), this, SLOT(wiz_jump()));
    wizard_layout->addWidget(jump_button, row, 2);

    row++;

    // Add the "teleport_to_target" button
    QPushButton *teleport_target_button = new QPushButton("Teleport To Targeted Spot");
    teleport_target_button->setToolTip("Teleports the player to a specified spot on the dungeon level.");
    connect(teleport_target_button, SIGNAL(clicked()), this, SLOT(wiz_teleport_to_target()));
    wizard_layout->addWidget(teleport_target_button, row, 0);

    // Add the "phase door" button
    QPushButton *phase_door = new QPushButton("Phase Door");
    phase_door->setToolTip("Teleports the player to a random spot up to 10 squares away.");
    connect(phase_door, SIGNAL(clicked()), this, SLOT(wiz_phase_door()));
    wizard_layout->addWidget(phase_door, row, 1);

    // Add the "teleport" button
    QPushButton *teleport = new QPushButton("Teleport");
    teleport->setToolTip("Teleports the player to a random spot up to 100 squares away.");
    connect(teleport, SIGNAL(clicked()), this, SLOT(wiz_teleport()));
    wizard_layout->addWidget(teleport, row, 2);

    row++;

    // Add the "edit character" button
    QPushButton *edit_character = new QPushButton("Edit Character");
    edit_character->setToolTip("Edit character statistics, experience, gold, and fame.");
    connect(edit_character, SIGNAL(clicked()), this, SLOT(wiz_edit_character()));
    wizard_layout->addWidget(edit_character, row, 0);

    // Add the "know all" button
    QPushButton *spoil_button = new QPushButton("Print Spoilers");
    spoil_button->setToolTip("Print out Spoilers for all Monsters, objects, artifacts, and terrain.");
    connect(spoil_button, SIGNAL(clicked()), this, SLOT(wiz_print_spoilers()));
    wizard_layout->addWidget(spoil_button, row, 1);

    row++;

    // Add the dungeon commands
    dungeon_section = new QLabel("<b>Dungeon commands</b>");
    dungeon_section->setAlignment(Qt::AlignCenter);
    wizard_layout->addWidget(dungeon_section, row, 1);

    row++;

    // Add the "summon" button
    QPushButton *summon_button = new QPushButton("Summon Monster");
    summon_button->setToolTip("Summon one random monster.");
    connect(summon_button, SIGNAL(clicked()), this, SLOT(wiz_summon()));
    wizard_layout->addWidget(summon_button, row, 0);

    // Add the "banish" button
    QPushButton *banish_button = new QPushButton("Banish Monsters");
    banish_button->setToolTip("Erase all monsters within 30 squares of player, except for uniques and quest monsters.");
    connect(banish_button, SIGNAL(clicked()), this, SLOT(wiz_banish()));
    wizard_layout->addWidget(banish_button, row, 1);

    // Add the "detect all monsters" button
    QPushButton *display_mon_button = new QPushButton("Detect All Monsters");
    display_mon_button->setToolTip("Detect all monsters on the level.");
    connect(display_mon_button, SIGNAL(clicked()), this, SLOT(wiz_detect_all_monsters()));
    wizard_layout->addWidget(display_mon_button, row, 2);

    row++;

    // Add the "detection" button
    QPushButton *detection = new QPushButton("Detection");
    detection->setToolTip("Cast the 'Detection' spell");
    connect(detection, SIGNAL(clicked()), this, SLOT(wiz_detection()));
    wizard_layout->addWidget(detection, row, 0);

    // Add the "magic mapping" button
    QPushButton *magic_mapping = new QPushButton("Magic Mapping");
    magic_mapping->setToolTip("Cast the 'Magic Mapping' spell.");
    connect(magic_mapping, SIGNAL(clicked()), this, SLOT(wiz_magic_mapping()));
    wizard_layout->addWidget(magic_mapping, row, 1);

    // Add the "light dungeon" button
    QPushButton *dungeon_light = new QPushButton("Light Dungeon");
    dungeon_light->setToolTip("Illuminate the entire dungeon level.");
    connect(dungeon_light, SIGNAL(clicked()), this, SLOT(wiz_level_light()));
    wizard_layout->addWidget(dungeon_light, row, 2);

    row++;

    // Add the "redraw dungeon" button
    QPushButton *redraw_dungeon = new QPushButton("Redraw Dungeon");
    redraw_dungeon->setToolTip("Redraw a new dungeon level at the current depth.");
    connect(redraw_dungeon, SIGNAL(clicked()), this, SLOT(wiz_redraw_dungeon()));
    wizard_layout->addWidget(redraw_dungeon, row, 0);

    // Add the "make monster" button
    QPushButton *make_monster = new QPushButton("Make Monster");
    make_monster->setToolTip("Attempt to make a monster of a specified monster race.");
    connect(make_monster, SIGNAL(clicked()), this, SLOT(wiz_create_monster()));
    wizard_layout->addWidget(make_monster, row, 1);

    // Add the "make feature" button
    QPushButton *make_feature = new QPushButton("Make Feature");
    make_feature->setToolTip("Attempt to make a specified feature type.");
    connect(make_feature, SIGNAL(clicked()), this, SLOT(wiz_create_feature()));
    wizard_layout->addWidget(make_feature, row, 2);

    row++;

    // Add the object commands
    object_section = new QLabel("<b>Object commands</b>");
    object_section->setAlignment(Qt::AlignCenter);
    wizard_layout->addWidget(object_section, row, 1);

    row++;

    // Add the "mass create items" button
    QPushButton *mass_create_items_button = new QPushButton("Create 25 Random Items");
    mass_create_items_button->setToolTip("Drop 25 randomly generated objects around the player.");
    connect(mass_create_items_button , SIGNAL(clicked()), this, SLOT(wiz_mass_create_items()));
    wizard_layout->addWidget(mass_create_items_button, row, 0);

    // Add the "create 1 random good item" button
    QPushButton *create_good_item = new QPushButton("Create 1 Random Good Item");
    create_good_item->setToolTip("Drop one randomly created guaranteed good item by the player.");
    connect(create_good_item , SIGNAL(clicked()), this, SLOT(wiz_create_good_item()));
    wizard_layout->addWidget(create_good_item, row, 1);

    // Add the "create 1 random great item" button
    QPushButton *create_great_item = new QPushButton("Create 1 Random Great Item");
    create_great_item->setToolTip("Drop one randomly created guaranteed great item by the player.");
    connect(create_great_item , SIGNAL(clicked()), this, SLOT(wiz_create_great_item()));
    wizard_layout->addWidget(create_great_item, row, 2);

    row++;

    // Add the "mass identify" button
    QPushButton *mass_identify = new QPushButton("Mass Identify");
    mass_identify->setToolTip("Identify all objects the player has, as well as all objects within 5 squares.");
    connect(mass_identify, SIGNAL(clicked()), this, SLOT(wiz_mass_identify_items()));
    wizard_layout->addWidget(mass_identify, row, 0);

    // Add the "winner's kit" button
    QPushButton *winners_kit = new QPushButton("Winner's kit");
    winners_kit->setToolTip("Create a set of artifacts suitable for winning the game.");
    connect(winners_kit, SIGNAL(clicked()), this, SLOT(wiz_winners_kit()));
    wizard_layout->addWidget(winners_kit, row, 1);

    // Add the "edit object" button
    QPushButton *edit_object = new QPushButton("Edit Object");
    edit_object->setToolTip("Edit a non-artifact object.");
    connect(edit_object, SIGNAL(clicked()), this, SLOT(wiz_edit_object()));
    wizard_layout->addWidget(edit_object, row, 2);

    row++;

    // Add the "make artifact" button
    QPushButton *create_artifact = new QPushButton("Create artifact");
    create_artifact->setToolTip("Create an artifact.");
    connect(create_artifact, SIGNAL(clicked()), this, SLOT(wiz_create_artifact()));
    wizard_layout->addWidget(create_artifact, row, 0);

    // Add the "make objecct" button
    QPushButton *create_object = new QPushButton("Create object");
    create_object->setToolTip("Create a normal object.");
    connect(create_object, SIGNAL(clicked()), this, SLOT(wiz_create_object()));
    wizard_layout->addWidget(create_object, row, 1);

    vlay->addLayout(wizard_layout);

    buttons = new QDialogButtonBox(QDialogButtonBox::Cancel);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    vlay->addStretch();
    vlay->addWidget(buttons);

    setLayout(vlay);
    setWindowTitle(tr("Wizard Mode Menu"));

    this->exec();
}



void do_cmd_wizard_mode(void)
{
    WizardModeDialog();
}
