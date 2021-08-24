/* File: knowledge.cpp */

/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/npp.h>
#include <src/knowledge.h>
#include <src/utilities.h>
#include <src/player_scores.h>
#include <src/store.h>
#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QDialogButtonBox>
#include <QHeaderView>


void qtablewidget_add_palette(QTableWidget *this_tablewidget)
{
    QPalette this_palette;
    this_palette.setColor(QPalette::Base, defined_colors[TERM_DARK]);
    QBrush this_brush(defined_colors[TERM_WHITE]);
    this_palette.setBrush(QPalette::Text, this_brush);

    this_tablewidget->setPalette(this_palette);
}

void qpushbutton_dark_background(QPushButton *this_pushbutton)
{
    this_pushbutton->setStyleSheet("background-color: rgb(0, 0, 0)");
    this_pushbutton->setAutoFillBackground(TRUE);
}


DisplayNotesFile::DisplayNotesFile(void): NPPDialog()
{
    central = new QWidget;
    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    // IMPORTANT: it must be called AFTER setting the layout
    this->setClient(central);

    QPointer<QGridLayout> notes_info = new QGridLayout;

    main_layout->addLayout(notes_info);

    QFont this_font = ui_message_window_font();

    this_font.setPointSize(12);

    QFontMetrics metrics(this_font);
    QSize this_size = metrics.size(Qt::TextSingleLine, "MMMMMMMMMMMMMM");

    int row = 0;
    int col = 0;

    QPointer<QLabel> header_turn = new QLabel("<b><u>GAME TURN</u>  </b>");
    QPointer<QLabel> header_depth = new QLabel("<b>  <u>DUNGEON DEPTH</u>  </b>");
    QPointer<QLabel> header_level = new QLabel("<b>  <u>PLAYER LEVEL</u>  </b>");
    QPointer<QLabel> header_event = new QLabel("<b>  <u>EVENT</u>  </b>");
    notes_info->addWidget(header_turn, row, col++, Qt::AlignRight);
    notes_info->addWidget(header_depth, row, col++, Qt::AlignRight);
    notes_info->addWidget(header_level, row, col++, Qt::AlignRight);
    notes_info->addWidget(header_event, row++, col++, Qt::AlignLeft);

    // Print out all the notes
    for (int i = 0; i < notes_log.size(); i++)
    {
        QString depth_note = (QString("Town"));
        row++;
        col = 0;
        notes_type *notes_ptr = &notes_log[i];
        QPointer<QLabel> game_turn = new QLabel(number_to_formatted_string(notes_ptr->game_turn));
        notes_info->addWidget(game_turn, row, col++, Qt::AlignRight | Qt::AlignTop);
        // Format the depth, handle objects from chests and quest rewards.
        if (notes_ptr->dun_depth)
        {
            if (notes_ptr->dun_depth == CHEST_LEVEL) depth_note = "Chest";
            else if (notes_ptr->dun_depth == QUEST_LEVEL) depth_note = "Quest";
            else depth_note = number_to_formatted_string(notes_ptr->dun_depth * 50);
        }
        QPointer<QLabel> game_depth = new QLabel(depth_note);
        notes_info->addWidget(game_depth, row, col++, Qt::AlignRight | Qt::AlignTop);
        QPointer<QLabel> player_level = new QLabel(QString("%1 ") .arg(notes_ptr->player_level));
        notes_info->addWidget(player_level, row, col++, Qt::AlignRight | Qt::AlignTop);
        QPointer<QLabel> game_event = new QLabel(notes_ptr->recorded_note);
        game_event->setWordWrap(TRUE);
        game_event->setMinimumWidth(this_size.width() * 2);
        notes_info->addWidget(game_event, row, col++, Qt::AlignLeft | Qt::AlignTop);

    }

    main_layout->addStretch(1);

    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    main_layout->addWidget(buttons);

    setWindowTitle(tr("Notes and Accomplishments"));

    this->clientSizeUpdated();

    this->exec();
}

void display_notes_file(void)
{
    // Paranoia
    if (!p_ptr->playing && !p_ptr->in_death_menu) return;

    DisplayNotesFile();
}

DisplayHomeInven::DisplayHomeInven(void): NPPDialog()
{
    // First handle an empty home
    store_type *st_ptr = &store[STORE_HOME];
    if (!st_ptr->stock_num)
    {
        pop_up_message_box("Your Home Is Empty");
        return;
    }

    central = new QWidget;
    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    // IMPORTANT: it must be called AFTER setting the layout
    this->setClient(central);


    /* Display contents of the home */
    for (int i = 0; i < st_ptr->stock_num; i++)
    {
        QChar prefix = number_to_letter(i);
        object_type *o_ptr = &st_ptr->stock[i];
        QString o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);
        QString o_desc = identify_random_gen(o_ptr);

        QPointer<QLabel> name_label = new QLabel(QString("<h3>%1) %2</h3>%4") .arg(prefix) .arg(o_name) .arg(o_desc));
        //QPointer<QLabel> desc_label = new QLabel(o_desc);
        name_label->setWordWrap(TRUE);
        name_label->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

        main_layout->addWidget(name_label);
    }

    //Add a close button on the right side
    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    main_layout->addWidget(buttons);

    setWindowTitle(tr("Home Inventory"));

    this->clientSizeUpdated();

    this->exec();
}

void display_home_inventory(void)
{
    // Paranoia
    if (!p_ptr->playing && !p_ptr->in_death_menu) return;

    DisplayHomeInven();
}



DisplayScores::DisplayScores(void): NPPDialog()
{
    //Copy the vector, add the player and sort it.
    QVector<high_score> score_list;
    for (int i = 0; i < player_scores_list.size(); i++)
    {
        score_list.append(player_scores_list[i]);
    }
    if (!p_ptr->is_wizard && !p_ptr->is_dead)
    {
        high_score player_current = build_score("Still Alive");
        player_current.death_how = QString("nobody (yet)!");
        score_list.append(player_current);
    }
    if (!score_list.size())
    {
        pop_up_message_box("There are no player scores to display.");
        return;
    }

    central = new QWidget;
    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    // IMPORTANT: it must be called AFTER setting the layout
    this->setClient(central);

    scores_proxy_model = new QSortFilterProxyModel;
    scores_proxy_model->setSortCaseSensitivity(Qt::CaseSensitive);

    // Sort the scores
    qSort(player_scores_list.begin(), player_scores_list.end(), scores_sort);

    int col = 0;

    //Set up the headers
    scores_table = new QTableWidget(0, 8, this);
    scores_table->setAlternatingRowColors(TRUE);

    scores_table->verticalHeader()->setVisible(false);

    QTableWidgetItem *score_header = new QTableWidgetItem("Player Score");
    score_header->setTextAlignment(Qt::AlignRight);
    scores_table->setHorizontalHeaderItem(col++, score_header);
    QTableWidgetItem *character_header = new QTableWidgetItem("Character");
    character_header->setTextAlignment(Qt::AlignLeft);
    scores_table->setHorizontalHeaderItem(col++, character_header);
    QTableWidgetItem *killed_by_header = new QTableWidgetItem("Killed By");
    killed_by_header->setTextAlignment(Qt::AlignLeft);
    scores_table->setHorizontalHeaderItem(col++, killed_by_header);
    QTableWidgetItem *level_header = new QTableWidgetItem("Player Level");
    level_header->setTextAlignment(Qt::AlignRight);
    scores_table->setHorizontalHeaderItem(col++, level_header);
    QTableWidgetItem *exp_header = new QTableWidgetItem("Experience");
    exp_header->setTextAlignment(Qt::AlignRight);
    scores_table->setHorizontalHeaderItem(col++, exp_header);
    QTableWidgetItem *turn_header = new QTableWidgetItem("Game Turns");
    turn_header->setTextAlignment(Qt::AlignRight);
    scores_table->setHorizontalHeaderItem(col++, turn_header);
    QTableWidgetItem *fame_header = new QTableWidgetItem("Fame");
    fame_header->setTextAlignment(Qt::AlignRight);
    scores_table->setHorizontalHeaderItem(col++, fame_header);
    QTableWidgetItem *version_header = new QTableWidgetItem("Version");
    version_header->setTextAlignment(Qt::AlignLeft);
    scores_table->setHorizontalHeaderItem(col++, version_header);

    // Add the data
    for (int i = 0; i < score_list.size(); i++)
    {
        high_score *score_ptr = &score_list[i];
        col = 0;
        scores_table->insertRow(i);

        // Score
        QTableWidgetItem *score = new QTableWidgetItem;
        score->setData(Qt::DisplayRole, score_ptr->score);
        score->setTextAlignment(Qt::AlignRight);
        scores_table->setItem(i, col++, score);

        // Player name, race, class, gender
        QString player_name = (QString("%1 the %2 %3 (%4)  ")
                          .arg(score_ptr->p_name) .arg(score_ptr->p_race)
                          .arg(score_ptr->p_class) .arg(score_ptr->p_sex));
        QTableWidgetItem *pl_name = new QTableWidgetItem(player_name);
        pl_name->setTextAlignment(Qt::AlignLeft);
        scores_table->setItem(i, col++, pl_name);


        // Killed by and dungeon depth
        QString died_by = (QString(" %1 ") .arg(score_ptr->death_how));
        if (score_ptr->cur_depth)
        {
            died_by.append(QString("on dungeon level %1 ") .arg(score_ptr->cur_depth));
        }
        else died_by.append("in the town ");

        QTableWidgetItem *killed_by = new QTableWidgetItem(died_by);
        killed_by->setTextAlignment(Qt::AlignLeft);
        scores_table->setItem(i, col++, killed_by);

        // Player level
        QTableWidgetItem *level = new QTableWidgetItem();
        level->setData(Qt::DisplayRole, score_ptr->cur_level);
        level->setTextAlignment(Qt::AlignRight);
        scores_table->setItem(i, col++, level);

        // Player Experience
        QTableWidgetItem *exp = new QTableWidgetItem();
        exp->setData(Qt::DisplayRole, score_ptr->cur_exp);
        exp->setTextAlignment(Qt::AlignRight);
        scores_table->setItem(i, col++, exp);

        // Turns
        QTableWidgetItem *turns = new QTableWidgetItem();
        turns->setData(Qt::DisplayRole, score_ptr->turns);
        turns->setTextAlignment(Qt::AlignRight);
        scores_table->setItem(i, col++, turns);

        // Fame
        QTableWidgetItem *fame = new QTableWidgetItem();
        fame->setData(Qt::DisplayRole, score_ptr->fame);
        fame->setTextAlignment(Qt::AlignRight);
        scores_table->setItem(i, col++, fame);

        // Version
        QTableWidgetItem *version = new QTableWidgetItem(score_ptr->version);
        version->setTextAlignment(Qt::AlignLeft);
        scores_table->setItem(i, col++, version);
    }

    scores_table->setSortingEnabled(TRUE);
    scores_table->resizeColumnsToContents();
    scores_table->sortByColumn(0, Qt::DescendingOrder);
    scores_table->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    main_layout->addWidget(scores_table);
    scores_table->setEditTriggers(QAbstractItemView::NoEditTriggers);

    //Add a close button on the right side
    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    main_layout->addWidget(buttons);

    QSize this_size = QSize(width()* 2, height() * 2);

    resize(ui_max_widget_size(this_size));
    updateGeometry();

    setWindowTitle("Player Scores");

    this->exec();
}

void display_player_scores(void)
{
    // Paranoia
    if (!p_ptr->playing && !p_ptr->in_death_menu) return;

    DisplayScores();
}

// Sort function for the total kills list
// Sort by total kills, then monster level, then name
static bool kill_list_sort(mon_kills first, mon_kills second)
{
    if (first.total_kills > second.total_kills) return (TRUE);
    if (first.total_kills < second.total_kills) return (FALSE);

    monster_race *r1_ptr = &r_info[first.mon_idx];
    monster_race *r2_ptr = &r_info[second.mon_idx];
    if (r1_ptr->level > r2_ptr->level) return (TRUE);
    if (r1_ptr->level < r2_ptr->level) return (FALSE);
    if (r1_ptr->r_name_full < r2_ptr->r_name_full) return (TRUE);

    return (FALSE);
}

DisplayMonKillCount::DisplayMonKillCount(void)
{
    QVector<mon_kills> mon_kill_list;

    /* Collect matching monsters */
    for (int i = 1; i < z_info->r_max - 1; i++)
    {
        monster_race *r_ptr = &r_info[i];
        monster_lore *l_ptr = &l_list[i];

        /* Require non-unique monsters */
        if (r_ptr->flags1 & RF1_UNIQUE) continue;

        /* Collect "appropriate" monsters */
        if (l_ptr->pkills == 0) continue;

        mon_kills mon_body;

        mon_body.mon_idx = i;
        mon_body.total_kills = l_ptr->pkills;
        mon_kill_list.append(mon_body);

    }

    // Make sure they have killed something first
    if (!mon_kill_list.size())
    {
        pop_up_message_box("You have not yet killed any creatures.");
        return;
    }

    // Sort the listTab
    qSort(mon_kill_list.begin(), mon_kill_list.end(), kill_list_sort);

    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;

    int col = 0;

    //Set up the headers
    kill_count_table = new QTableWidget(0, 4, this);
    kill_count_table->setAlternatingRowColors(FALSE);

    qtablewidget_add_palette(kill_count_table);

    QTableWidgetItem *race_header = new QTableWidgetItem("Monster Race");
    race_header->setTextAlignment(Qt::AlignLeft);
    kill_count_table->setHorizontalHeaderItem(col++, race_header);
    QTableWidgetItem *symbol_header = new QTableWidgetItem("Symbol");
    symbol_header->setTextAlignment(Qt::AlignCenter);
    kill_count_table->setHorizontalHeaderItem(col++, symbol_header);
    QTableWidgetItem *depth_header = new QTableWidgetItem("Native Level");
    depth_header->setTextAlignment(Qt::AlignRight);
    kill_count_table->setHorizontalHeaderItem(col++, depth_header);
    QTableWidgetItem *kills_header = new QTableWidgetItem("Total Kills");
    kills_header->setTextAlignment(Qt::AlignRight);
    kill_count_table->setHorizontalHeaderItem(col++, kills_header);

    // Add the data
    for (int i = 0; i < mon_kill_list.size(); i++)
    {
        mon_kills *mk_ptr = &mon_kill_list[i];
        monster_race *r_ptr = &r_info[mk_ptr->mon_idx];
        col = 0;
        kill_count_table->insertRow(i);

        // Race
        QString this_mon_race = r_ptr->r_name_full;
        if (mk_ptr->total_kills > 1) this_mon_race = plural_aux(this_mon_race);
        this_mon_race = capitalize_first(this_mon_race);
        QTableWidgetItem *race = new QTableWidgetItem(this_mon_race);
        race->setTextAlignment(Qt::AlignLeft);
        kill_count_table->setItem(i, col++, race);

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
        kill_count_table->setItem(i, col++, mon_ltr);

        // dungeon depth
        QTableWidgetItem *mon_lvl = new QTableWidgetItem();
        mon_lvl->setData(Qt::DisplayRole, r_ptr->level);
        mon_lvl->setTextAlignment(Qt::AlignRight);
        kill_count_table->setItem(i, col++, mon_lvl);

        // Monster Kills
        QTableWidgetItem *total_kills = new QTableWidgetItem();
        total_kills->setData(Qt::DisplayRole, mk_ptr->total_kills);
        total_kills->setTextAlignment(Qt::AlignRight);
        kill_count_table->setItem(i, col++, total_kills);
    }

    kill_count_table->setSortingEnabled(FALSE);
    kill_count_table->resizeColumnsToContents();
    kill_count_table->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    main_layout->addWidget(kill_count_table);
    kill_count_table->setEditTriggers(QAbstractItemView::NoEditTriggers);


    //Add a close button on the right side
    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    main_layout->addWidget(buttons);

    QSize this_size = QSize(width()* 10 / 9, height() * 2);

    resize(ui_max_widget_size(this_size));
    updateGeometry();

    setLayout(main_layout);
    setWindowTitle(QString("Monster Kill Count"));

    this->exec();
}


void display_mon_kill_count(void)
{
    // Paranoia
    if (!p_ptr->playing) return;

    DisplayMonKillCount();
}

DisplayKnowledgeMenu::DisplayKnowledgeMenu(void): NPPDialog()
{
    central = new QWidget;
    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    // IMPORTANT: it must be called AFTER setting the layout
    this->setClient(central);

    // Add the 9 knowledge screens to the grid
    QPointer<QPushButton>pb_obj_knowledge = new QPushButton("View Object Knowledge");
    connect(pb_obj_knowledge, SIGNAL(clicked()), this, SLOT(slot_object_knowledge()));
    main_layout->addWidget(pb_obj_knowledge, Qt::AlignLeft);

    QPointer<QPushButton>pb_ego_knowledge = new QPushButton("View Ego item Knowledge");
    connect(pb_ego_knowledge, SIGNAL(clicked()), this, SLOT(slot_ego_item_knowledge()));
    main_layout->addWidget(pb_ego_knowledge,  Qt::AlignLeft);

    QPointer<QPushButton>pb_artifact_knowledge = new QPushButton("View Artifact Knowledge");
    connect(pb_artifact_knowledge, SIGNAL(clicked()), this, SLOT(slot_artifact_knowledge()));
    main_layout->addWidget(pb_artifact_knowledge, Qt::AlignLeft);

    QPointer<QPushButton>pb_mon_knowledge = new QPushButton("View Monster Knowledge");
    connect(pb_mon_knowledge, SIGNAL(clicked()), this, SLOT(slot_monster_knowledge()));
    main_layout->addWidget(pb_mon_knowledge,  Qt::AlignLeft);

    QPointer<QPushButton>pb_terrain_knowledge = new QPushButton("View Terrain Knowledge");
    connect(pb_terrain_knowledge, SIGNAL(clicked()), this, SLOT(slot_terrain_knowledge()));
    main_layout->addWidget(pb_terrain_knowledge, Qt::AlignLeft);

    QPointer<QPushButton>pb_notes_file = new QPushButton("View Game Notes");
    connect(pb_notes_file, SIGNAL(clicked()), this, SLOT(slot_notes_file()));
    main_layout->addWidget(pb_notes_file,  Qt::AlignLeft);

    QPointer<QPushButton>pb_home_inven = new QPushButton("View Home Inventory");
    connect(pb_home_inven, SIGNAL(clicked()), this, SLOT(slot_home_inventory()));
    main_layout->addWidget(pb_home_inven, Qt::AlignLeft);

    QPointer<QPushButton>pb_player_scores = new QPushButton("View Player Scores");
    connect(pb_player_scores, SIGNAL(clicked()), this, SLOT(slot_player_scores()));
    main_layout->addWidget(pb_player_scores, Qt::AlignLeft);

    QPointer<QPushButton>pb_mon_kill_count = new QPushButton("View Monster Kill Count");
    connect(pb_mon_kill_count, SIGNAL(clicked()), this, SLOT(slot_mon_kill_count()));
    main_layout->addWidget(pb_mon_kill_count, Qt::AlignCenter);

    //Add a close button on the right side
    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    main_layout->addWidget(buttons);


    setWindowTitle("Knowledge");

    this->clientSizeUpdated();

    this->exec();

}

void do_cmd_knowledge_screens()
{
    DisplayKnowledgeMenu();
}
