
/* File: knowledge_monsters.cpp */

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


#include <src/npp.h>
#include <src/knowledge.h>
#include <QVBoxLayout>
#include <QPushButton>
#include <QHeaderView>

/*
 * Description of each monster group.
 */
// The null entry at the end is essential for initializing the table of groups.
static struct monster_group monster_group_nppmoria[] =
{
// Unique and "all" group gets special handling
    { NULL,	"All" },//MON_GROUP_ALL
    { NULL,	"Uniques" },//MON_GROUP_UNIQUE
    { "A",		"Ant Lions" },
    { "a",		"Ants" },
    { "B",		"Balrog" },
    { "b",		"Bats" },
    { "C",		"Gelatinous Cube" },
    { "c",		"Centipedes" },
    { "dD",		"Dragons" },
    { "E",		"Elementals/Spirits" },
    { "e",		"Floating Eyes" },
    { "F",		"Flying Insects" },
    { "f",		"Frogs" },
    { "G",		"Ghosts" },
    { "g",		"Golems" },
    { "H",		"Hobgoblin" },
    { "h",		"Harpies" },
    { "i",		"Icky Things" },
    { "J",		"Jellies" },
    { "j",		"Jackals" },
    { "K",		"Killer Beetles" },
    { "k",		"Kobolds" },
    { "L",		"Liches" },
    { "l",		"Louse" },
    { "M",		"Mummies" },
    { "m",		"Molds" },
    { "n",		"Nagas" },
/* Note some special handling of mimics in the code below since there are so many different symbols */
    { "$!?",	"Mimics" },
    { ",",		"Mushroom Patches" },
    { "O",		"Oozes" },
    { "o",		"Orcs" },
    { "p",		"People/Humanoids" },
    { "P",		"Giants" },
    { "q",		"Quasits" },
    { "Q",		"Quylthulgs" },
    { "R",		"Snakes" },
    { "r",		"Rodents" },
    { "S",		"Scorpions" },
    { "s",		"Skeletons" },
    { "U",		"Umber Hulks" },
    { "T",		"Trolls" },
    { "t",		"Ticks" },
    { "V",		"Vampires" },
    { "W",		"Wights/Wraiths" },
    { "w",		"Worms/Worm Masses" },
    { "X",		"Xorns" },
    { "y",		"Yeeks" },
    { "Y",		"Yeti" },
    { "z",		"Zombies" },
// The null entry at the end is essential for initializing the table of groups.
    { NULL,       NULL }
};



static struct monster_group monster_group_nppangband[] =
{
// Unique and "all" group gets special handling
    { NULL,	"All" },//MON_GROUP_ALL
    { NULL,	"Uniques" },//MON_GROUP_UNIQUE{ "A",		"Ainur/Maiar" },
    { "a",		"Ants" },
    { "b",		"Bats" },
    { "B",		"Birds" },
    { "C",		"Canines" },
    { "c",		"Centipedes" },
    { "uU",		"Demons" },
    { "dD",		"Dragons" },
    { "vE",		"Elementals/Vortices" },
    { "e",		"Eyes/Beholders" },
    { "f",		"Felines" },
    { "G",		"Ghosts" },
    { "OP",		"Giants/Ogres" },
    { "g",		"Golems" },
    { "H",		"Harpies/Hybrids" },
    { "h",		"Hominids (Elves, Dwarves)" },
    { "tp",		"Humans" },
    { "M",		"Hydras" },
    { "i",		"Icky Things" },
    { "lFI",	"Insects" },
    { "j",		"Jellies" },
    { "K",		"Killer Beetles" },
    { "k",		"Kobolds" },
    { "L",		"Lichs" },
/* Note some special handling of mimics in the code below since there are so many different symbols */
    { "$!?",	"Mimics" },
    { "m",		"Molds" },
    { ",",		"Mushroom Patches" },
    { "n",		"Nagas" },
    { "o",		"Orcs" },
    { "q",		"Quadrupeds" },
    { "Q",		"Quylthulgs" },
    { "R",		"Reptiles/Amphibians" },
    { "r",		"Rodents" },
    { "S",		"Scorpions/Spiders/Ticks" },
    { "s",		"Skeletons/Drujs" },
    { "J",		"Snakes" },
    { "T",		"Trolls" },
    { "V",		"Vampires" },
    { "W",		"Wights/Wraiths" },
    { "w",		"Worms/Worm Masses" },
    { "X",		"Xorns/Xarens" },
    { "y",		"Yeeks" },
    { "Y",		"Yeti" },
    { "Z",		"Zephyr Hounds" },
    { "z",		"Zombies" },
// The null entry at the end is essential for initializing the table of groups.
    { NULL,       NULL }
};

bool DisplayMonsterKnowledge::mon_matches_mon_group(int r_idx, int group)
{
    monster_group *mon_ptr;
    if (game_mode == GAME_NPPANGBAND) mon_ptr = &monster_group_nppangband[group];
    else mon_ptr = &monster_group_nppmoria[group];

    monster_race *r_ptr = &r_info[r_idx];

    // Special handling for uniques and "all" section
    if (group == MON_GROUP_ALL) return (TRUE);
    if (group == MON_GROUP_UNIQUE)
    {
        return (r_ptr->is_unique());
    }

    QString which_letter = r_ptr->d_char;

    //Not a part of this group
    if (mon_ptr->chars.contains(which_letter, Qt::CaseSensitive))
    {
        return (TRUE);
    }
    else if(r_ptr->is_mimic())
    {
        QString exclamation = QString("!");
        if (mon_ptr->chars.contains(exclamation, Qt::CaseSensitive))
        {
            return (TRUE);
        }
    }

    return (FALSE);
}

// Display the monster info
void DisplayMonsterKnowledge::button_press(int mon_race)
{
    describe_monster(mon_race, p_ptr->is_wizard, NULL);
}

void DisplayMonsterKnowledge::filter_rows(int row, int col, int old_row, int old_col)
{
    int which_group = 0;

    (void)col;
    (void)old_row;
    (void)old_col;
    int i;

    // First find the group we want to filter for
    for (i = 0; i < monster_group_info.size(); i++)
    {
        if (!monster_group_info[i]) continue;
        if (which_group == row) break;
        which_group++;
    }

    //Remember the group
    which_group = i;

    // Go through and hide all the rows where the creature doesn't meet the criteria
    for (i = 0; i < monster_table->rowCount(); i++)
    {
        QString text_idx = this->monster_table->item(i, 5)->text();
        int r_idx = text_idx.toInt();

        if (mon_matches_mon_group(r_idx, which_group))
        {
            monster_table->showRow(i);
        }
        else monster_table->hideRow(i);
    }


}


// Set up the monster knowledge table

DisplayMonsterKnowledge::DisplayMonsterKnowledge(void)
{
    monster_proxy_model = new QSortFilterProxyModel;
    monster_proxy_model->setSortCaseSensitivity(Qt::CaseSensitive);
    QVBoxLayout *main_layout = new QVBoxLayout;
    QHBoxLayout *mon_knowledge_hlay = new QHBoxLayout;
    main_layout->addLayout(mon_knowledge_hlay);

    // To track the monster race info button
    mon_button_group = new QButtonGroup(this);
    mon_button_group->setExclusive(FALSE);

    // Set the table and headers
    mon_group_table = new QTableWidget(0, 1, this);
    mon_group_table->setAlternatingRowColors(FALSE);

    QTableWidgetItem *mon_group_header = new QTableWidgetItem("Monster Groups");
    mon_group_header->setTextAlignment(Qt::AlignLeft);
    mon_group_table->setHorizontalHeaderItem(0, mon_group_header);

    monster_table = new QTableWidget(0, 6, this);
    monster_table->setAlternatingRowColors(FALSE);

    qtablewidget_add_palette(monster_table);
    qtablewidget_add_palette(mon_group_table);

    mon_group_table->verticalHeader()->setVisible(FALSE);
    monster_table->verticalHeader()->setVisible(FALSE);

    int row = 0;
    int col = 0;

    QTableWidgetItem *race_header = new QTableWidgetItem("Monster Race");
    race_header->setTextAlignment(Qt::AlignLeft);
    monster_table->setHorizontalHeaderItem(col++, race_header);
    QTableWidgetItem *symbol_header = new QTableWidgetItem("Symbol");
    symbol_header->setTextAlignment(Qt::AlignCenter);
    monster_table->setHorizontalHeaderItem(col++, symbol_header);
    QTableWidgetItem *depth_header = new QTableWidgetItem("Native Level");
    depth_header->setTextAlignment(Qt::AlignRight);
    monster_table->setHorizontalHeaderItem(col++, depth_header);
    QTableWidgetItem *kills_header = new QTableWidgetItem("Total Kills");
    kills_header->setTextAlignment(Qt::AlignRight);
    monster_table->setHorizontalHeaderItem(col++, kills_header);
    QTableWidgetItem *info_header = new QTableWidgetItem("Info");
    info_header->setTextAlignment(Qt::AlignCenter);
    monster_table->setHorizontalHeaderItem(col++, info_header);
    //This column will be hidden, but is used in filter_rows
    QTableWidgetItem *r_idx_header = new QTableWidgetItem("r_idx");
    r_idx_header->setTextAlignment(Qt::AlignCenter);
    monster_table->setHorizontalHeaderItem(col++, r_idx_header);

    //Gather information to populate the monster groups
    monster_group_info.clear();
    //Automatically add a row for the uniques and all.
    monster_group_info.append(FALSE);
    monster_group_info.append(FALSE);
    int x = MON_GROUP_OTHERS;
    while (TRUE)
    {
        monster_group *mon_ptr;
        if (game_mode == GAME_NPPANGBAND) mon_ptr = &monster_group_nppangband[x];
        else mon_ptr = &monster_group_nppmoria[x];
        //See if we are at end
        if (mon_ptr->chars.isNull()) break;

        //add an entry
        monster_group_info.append(FALSE);

        // Move to the next entry;
        x++;
    }

    //  Populate the table
    for (int i = 1; i < z_info->r_max - 1; i++)
    {
        monster_race *r_ptr = &r_info[i];
        monster_lore *l_ptr = &l_list[i];

        // Don't count non-entries
        if ((!cheat_know) && !l_ptr->sights) continue;
        if (!r_ptr->r_speed) continue;

        monster_table->insertRow(row);
        col = 0;

        // Race
        QString this_mon_race = capitalize_first(r_ptr->r_name_full);
        if (r_ptr->is_player_ghost()) this_mon_race.append(" [G]");
        else if (r_ptr->is_unique()) this_mon_race.append(" [U]");
        QTableWidgetItem *race = new QTableWidgetItem(this_mon_race);
        race->setTextAlignment(Qt::AlignLeft);
        monster_table->setItem(row, col++, race);

        // Symbol (or tile if tiles are used)
        QString mon_symbol = (QString("'%1'") .arg(r_ptr->d_char));
        QTableWidgetItem *mon_ltr = new QTableWidgetItem(mon_symbol);
        if (ui_using_monster_tiles())
        {
            QPixmap pix = ui_get_tile(r_ptr->tile_id, FALSE);
            mon_ltr->setIcon(pix);
        }
        mon_ltr->setData(Qt::ForegroundRole, r_ptr->d_color);
        mon_ltr->setTextAlignment(Qt::AlignCenter);
        monster_table->setItem(row, col++, mon_ltr);

        // dungeon depth
        QTableWidgetItem *mon_lvl = new QTableWidgetItem();
        mon_lvl->setData(Qt::DisplayRole, r_ptr->level);
        mon_lvl->setTextAlignment(Qt::AlignRight);
        monster_table->setItem(row, col++, mon_lvl);

        // Monster Kills
        QTableWidgetItem *total_kills = new QTableWidgetItem();

        if (r_ptr->is_unique())
        {
            if (!l_ptr->pkills) total_kills->setText("Alive");
            else total_kills->setText("Dead");
        }
        else total_kills->setData(Qt::DisplayRole, l_ptr->pkills);
        total_kills->setTextAlignment(Qt::AlignRight);
        monster_table->setItem(row, col++, total_kills);

        // Mon info
        QPushButton *new_button = new QPushButton();
        qpushbutton_dark_background(new_button);
        new_button->setIcon(QIcon(":/icons/lib/icons/help_dark.png"));
        monster_table->setCellWidget(row, col++, new_button);
        mon_button_group->addButton(new_button, i);

        // r_idx
        QTableWidgetItem *mon_r_idx = new QTableWidgetItem();
        mon_r_idx->setData(Qt::DisplayRole, i);
        mon_r_idx->setTextAlignment(Qt::AlignRight);
        monster_table->setItem(row, col++, mon_r_idx);

        row++;

        // Now make sure the group monster is added to the table.
        for(x = 0; x < monster_group_info.size(); x++)
        {
            if (mon_matches_mon_group(i, x)) monster_group_info[x] = TRUE;
        }
    }

    connect(mon_button_group, SIGNAL(buttonClicked(int)), this, SLOT(button_press(int)));

    row = col = 0;

    if (!monster_table->columnCount())
    {
        pop_up_message_box("You are not yet aware of any creatures");
        return;
    }

    //Now populate the monster_group table
    for (int i = 0; i < monster_group_info.size(); i++)
    {
        if (!monster_group_info[i]) continue;
        mon_group_table->insertRow(row);

        monster_group *mon_ptr;
        if (game_mode == GAME_NPPANGBAND) mon_ptr = &monster_group_nppangband[i];
        else mon_ptr = &monster_group_nppmoria[i];

        // Monster Group
        QString group_name = QString(mon_ptr->name);
        QTableWidgetItem *mon_group_label = new QTableWidgetItem(group_name);
        mon_group_label->setTextAlignment(Qt::AlignLeft);
        mon_group_table->setItem(row++, 0, mon_group_label);
    }

    mon_group_table->resizeColumnsToContents();
    mon_group_table->resizeRowsToContents();
    mon_group_table->setSortingEnabled(FALSE);
    mon_group_table->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Expanding);
    mon_group_table->setEditTriggers(QAbstractItemView::NoEditTriggers);
    connect(mon_group_table, SIGNAL(currentCellChanged(int,int,int,int)), this, SLOT(filter_rows(int,int,int,int)));
    mon_knowledge_hlay->addWidget(mon_group_table);

    monster_table->setSortingEnabled(TRUE);
    monster_table->resizeColumnsToContents();
    monster_table->resizeRowsToContents();
    monster_table->sortByColumn(2, Qt::DescendingOrder);
    // Hide the r_idx column
    monster_table->setColumnHidden(5, TRUE);
    monster_table->setEditTriggers(QAbstractItemView::NoEditTriggers);
    monster_table->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Expanding);
    mon_knowledge_hlay->addWidget(monster_table);

    //Add a close button on the right side
    QDialogButtonBox buttons;
    buttons.setStandardButtons(QDialogButtonBox::Close);
    connect(&buttons, SIGNAL(rejected()), this, SLOT(close()));
    main_layout->addWidget(&buttons);

    //Flter for the first monster group.
    filter_rows(0, 0, 0, 0);

    QSize this_size = QSize(width()* 7 / 4, height() * 4 / 3);

    resize(ui_max_widget_size(this_size));
    updateGeometry();

    setLayout(main_layout);
    setWindowTitle(tr("Monster Knowledge"));

    this->exec();
}

void display_monster_knowledge(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    DisplayMonsterKnowledge();
}
