
/* File: knowledge_monsters.cpp */

/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */


#include <src/npp.h>
#include <src/knowledge.h>
#include <QVBoxLayout>
#include <QPushButton>
#include <QHeaderView>

/*
 * The label of the group who collects miscellaneous features.
 */
#define MISC_GRP "Misc"
#define MAX_FEATURE_TYPES	18

/*
 * Description of each feature group.
 */
QString feature_group_text[MAX_FEATURE_TYPES] =
{
    "Floors",
    "Player Traps",
    "Monster Traps",
    "Runes",
    "Doors",
    "Stairs",
    "Walls",
    "Stores",
    "Bridges",
    "Water",
    "Lava",
    "Ice",
    "Acid",
    "Oil",
    "Forest",
    "Sand/Mud",
    "Fire",
    MISC_GRP
};


/*
 * Flags of features in each group.
 * The misc. group must have all flags set to 0.
 */
static const u32b feature_flags[MAX_FEATURE_TYPES][3] =
{
    {FF1_FLOOR, 0, 0},
    {0, FF2_TRAP_PASSIVE, 0},
    {0, FF2_TRAP_MON, 0},
    {0, FF2_TRAP_SMART, 0},
    {FF1_DOOR, 0, 0},
    {FF1_STAIRS, 0, 0},
    {FF1_WALL, 0, 0},
    {FF1_SHOP, 0, 0},
    {0, FF2_BRIDGED, 0},
    {0, 0, FF3_WATER},
    {0, 0, FF3_LAVA},
    {0, 0, FF3_ICE},
    {0, 0, FF3_ACID},
    {0, 0, FF3_OIL},
    {0, 0, FF3_FOREST},
    {0, 0, FF3_SAND | FF3_MUD},
    {0, 0, FF3_FIRE},
    {0, 0, 0}   /*Misc Group*/
};

int DisplayTerrainKnowledge::terrain_matches_group(int f_idx)
{

    feature_type *f_ptr = &f_info[f_idx];

    for (int i = 0; i < MAX_FEATURE_TYPES; i++)
    {
        bool collecting_misc = FALSE;

        if (feature_group_text[i].contains(MISC_GRP)) collecting_misc = TRUE;


        /* Check for any flags matching in the group */
        if ((_feat_ff1_match(f_ptr, feature_flags[i][0])) ||
            (_feat_ff2_match(f_ptr, feature_flags[i][1])) ||
            (_feat_ff3_match(f_ptr, feature_flags[i][2])))
        {
            if (collecting_misc) continue;
        }
        else
        {
            if (!collecting_misc) continue;
        }

        return (i);
    }

    /* No match found, return misc */
    return (MAX_FEATURE_TYPES-1);
}

// Display the feature info
void DisplayTerrainKnowledge::button_press(int f_idx)
{
    describe_feature(f_idx, do_spoiler);
}

void DisplayTerrainKnowledge::filter_rows(int row, int col, int old_row, int old_col)
{
    int which_group = 0;

    (void)col;
    (void)old_row;
    (void)old_col;
    int i;

    // First find the group we want to filter for
    for (i = 0; i < terrain_group_info.size(); i++)
    {

        if (!terrain_group_info[i]) continue;
        if (which_group == row) break;
        which_group++;
    }

    //Remember the group
    which_group = i;

    // Go through and hide all the rows where the feature doesn't meet the criteria
    for (i = 0; i < terrain_table->rowCount(); i++)
    {
        QString text_idx = this->terrain_table->item(i, 3)->text();
        int f_idx = text_idx.toInt();

        if (terrain_matches_group(f_idx) == which_group)
        {
            terrain_table->showRow(i);
        }
        else terrain_table->hideRow(i);
    }


}


// Set up the feature knowledge table

DisplayTerrainKnowledge::DisplayTerrainKnowledge(void)
{
    terrain_proxy_model = new QSortFilterProxyModel;
    terrain_proxy_model->setSortCaseSensitivity(Qt::CaseSensitive);
    QVBoxLayout *main_layout = new QVBoxLayout;
    QHBoxLayout *terrain_knowledge_hlay = new QHBoxLayout;
    main_layout->addLayout(terrain_knowledge_hlay);

    // To track the feature info button
    terrain_button_group = new QButtonGroup(this);
    terrain_button_group->setExclusive(FALSE);

    // Set the table and headers
    terrain_group_table = new QTableWidget(0, 1, this);
    terrain_group_table->setAlternatingRowColors(FALSE);

    QTableWidgetItem *feat_group_header = new QTableWidgetItem("Feature Types");
    feat_group_header->setTextAlignment(Qt::AlignLeft);
    terrain_group_table->setHorizontalHeaderItem(0, feat_group_header);

    terrain_table = new QTableWidget(0, 4, this);
    terrain_table->setAlternatingRowColors(FALSE);

    qtablewidget_add_palette(terrain_group_table);
    qtablewidget_add_palette(terrain_table);

    terrain_group_table->verticalHeader()->setVisible(FALSE);
    terrain_table->verticalHeader()->setVisible(FALSE);

    do_spoiler = FALSE;

    int row = 0;
    int col = 0;

    QTableWidgetItem *feat_header = new QTableWidgetItem("Feature");
    feat_header->setTextAlignment(Qt::AlignLeft);
    terrain_table->setHorizontalHeaderItem(col++, feat_header);
    QTableWidgetItem *symbol_header = new QTableWidgetItem("Symbol");
    symbol_header->setTextAlignment(Qt::AlignCenter);
    terrain_table->setHorizontalHeaderItem(col++, symbol_header);
    QTableWidgetItem *info_header = new QTableWidgetItem("Info");
    info_header->setTextAlignment(Qt::AlignCenter);
    terrain_table->setHorizontalHeaderItem(col++, info_header);
    //This column will be hidden, but is used in filter_rows
    QTableWidgetItem *f_idx_header = new QTableWidgetItem("f_idx");
    f_idx_header->setTextAlignment(Qt::AlignCenter);
    terrain_table->setHorizontalHeaderItem(col++, f_idx_header);

    //Gather information to populate the feature groups
    terrain_group_info.clear();

    for (int x = 0; x < MAX_FEATURE_TYPES; x++) terrain_group_info.append(FALSE);

    //  Populate the table
    for (int i = 1; i < z_info->f_max; i++)
    {
        feature_type *f_ptr = &f_info[i];
        feature_lore *f_l_ptr = &f_l_list[i];

        // Don't count non and unseen entries
        if (f_ptr->f_name.isEmpty()) continue;
        if (!f_l_ptr->f_l_sights && !f_ptr->f_everseen) continue;

        terrain_table->insertRow(row);
        col = 0;

        // feature name
        QString this_feature = capitalize_first(f_ptr->f_name);
        this_feature.remove("~");
        QTableWidgetItem *feat = new QTableWidgetItem(this_feature);
        feat->setTextAlignment(Qt::AlignLeft);
        terrain_table->setItem(row, col++, feat);

        // Symbol (or tile if tiles are used)
        QString feat_symbol = (QString("'%1'") .arg(f_ptr->d_char));
        QTableWidgetItem *feat_ltr = new QTableWidgetItem(feat_symbol);
        if (use_graphics)
        {
            QPixmap pix = ui_get_tile(f_ptr->tile_id, FALSE);
            feat_ltr->setIcon(pix);
        }
        feat_ltr->setData(Qt::ForegroundRole, f_ptr->d_color);
        feat_ltr->setTextAlignment(Qt::AlignCenter);
        terrain_table->setItem(row, col++, feat_ltr);

        // feature info
        QPushButton *new_button = new QPushButton();
        qpushbutton_dark_background(new_button);
        new_button->setIcon(QIcon(":/icons/lib/icons/help_dark.png"));
        terrain_table->setCellWidget(row, col++, new_button);
        terrain_button_group->addButton(new_button, i);

        // f_idx
        QTableWidgetItem *f_idx = new QTableWidgetItem(i);
        f_idx->setData(Qt::DisplayRole, i);
        f_idx->setTextAlignment(Qt::AlignRight);
        terrain_table->setItem(row, col++, f_idx);

        row++;

        // Now make sure the feature type is added to the table.
        terrain_group_info[terrain_matches_group(i)] = TRUE;
    }

    connect(terrain_button_group, SIGNAL(buttonClicked(int)), this, SLOT(button_press(int)));

    row = col = 0;

    //Now populate the mfeature_group table
    for (int i = 0; i < terrain_group_info.size(); i++)
    {
        if (!terrain_group_info[i]) continue;
        terrain_group_table->insertRow(row);

        // Feature Group
        QString group_name = QString(feature_group_text[i]);
        QTableWidgetItem *feat_group_label = new QTableWidgetItem(group_name);
        feat_group_label->setTextAlignment(Qt::AlignLeft);
        terrain_group_table->setItem(row++, 0, feat_group_label);
    }

    terrain_group_table->resizeColumnsToContents();
    terrain_group_table->resizeRowsToContents();
    terrain_group_table->setSortingEnabled(FALSE);
    terrain_group_table->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Expanding);
    terrain_group_table->setEditTriggers(QAbstractItemView::NoEditTriggers);
    connect(terrain_group_table, SIGNAL(currentCellChanged(int,int,int,int)), this, SLOT(filter_rows(int,int,int,int)));
    terrain_knowledge_hlay->addWidget(terrain_group_table);

    terrain_table->setSortingEnabled(TRUE);
    terrain_table->resizeColumnsToContents();
    terrain_table->resizeRowsToContents();
    terrain_table->sortByColumn(2, Qt::DescendingOrder);
    // Hide the f_idx column
    terrain_table->setColumnHidden(3, TRUE);
    terrain_table->setEditTriggers(QAbstractItemView::NoEditTriggers);
    terrain_table->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Expanding);
    terrain_knowledge_hlay->addWidget(terrain_table);

    //Add a close button on the right side
    QDialogButtonBox buttons;
    buttons.setStandardButtons(QDialogButtonBox::Close);
    connect(&buttons, SIGNAL(rejected()), this, SLOT(close()));
    main_layout->addWidget(&buttons);

    //Filter for the first terrain group.
    filter_rows(0,0,0,0);

    QSize this_size = QSize(width()* 9 / 8, height() * 4 / 3);
    resize(ui_max_widget_size(this_size));
    updateGeometry();

    setLayout(main_layout);
    setWindowTitle(tr("Terrain Knowledge"));

    this->exec();
}

void display_terrain_knowledge(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    DisplayTerrainKnowledge();
}
