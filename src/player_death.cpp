/*
 * File: player_death.cpp
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/player_death.h"
#include <src/knowledge.h>
#include <src/cmds.h>
#include <src/player_scores.h>
#include <src/player_screen.h>
#include <src/object_all_menu.h>
#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <src/messages.h>

void PlayerDeathDialog::death_player_info(void)
{
    do_cmd_character_screen();
}

void PlayerDeathDialog::death_inven_info(void)
{
    do_cmd_all_objects(TAB_INVEN);
}

void PlayerDeathDialog::death_home_inven(void)
{
    display_home_inventory();
}

void PlayerDeathDialog::death_messages(void)
{
    display_message_log();
}

void PlayerDeathDialog::death_file_dump(void)
{
    save_character_file();
}

void PlayerDeathDialog::death_screenshot(void)
{
    bool png_screenshot = get_check("Create a PNG screenshot (YES) or HTML screenshot (NO)");
    save_screenshot(png_screenshot);
}

void PlayerDeathDialog::death_scores(void)
{
    display_player_scores();
}

void PlayerDeathDialog::death_examine(void)
{
    do_cmd_examine();
}

void PlayerDeathDialog::death_notes(void)
{
    display_notes_file();
}

void PlayerDeathDialog::death_spoilers(void)
{
    print_monster_spoiler_file();
    print_terrain_spoiler_file();
    print_object_spoiler_file();
    print_ego_item_spoiler_file();
    print_artifact_spoiler_file();

    pop_up_message_box("Spoilers created.");
}

PlayerDeathDialog::PlayerDeathDialog(void)
{
    QPointer<QVBoxLayout> vlay = new QVBoxLayout;

    QPointer<QLabel> obj_label = new QLabel(QString("<b><big>Press 'Close' when ready:</big></b>"));
    obj_label->setAlignment(Qt::AlignCenter);
    vlay->addWidget(obj_label);

    vlay->addStretch(1);

    // Add the "Character Screen" button
    QPointer<QPushButton> char_screen_button = new QPushButton("Character Information");
    char_screen_button->setToolTip("View player screen.");
    connect(char_screen_button, SIGNAL(clicked()), this, SLOT(death_player_info()));
    vlay->addWidget(char_screen_button);

    // Add the "Information" button
    QPointer<QPushButton> info_button = new QPushButton("  Equipment and Inventory Information  ");
    info_button->setToolTip("View equipment, inventory.");
    connect(info_button, SIGNAL(clicked()), this, SLOT(death_inven_info()));
    vlay->addWidget(info_button);

    // Add the  "Home Inventory" button
    QPointer<QPushButton> home_button = new QPushButton("Home Inventory");
    home_button->setToolTip("View inventory in player's home.");
    connect(home_button, SIGNAL(clicked()), this, SLOT(death_home_inven()));
    vlay->addWidget(home_button);

    // Add the "Messages" button
    QPointer<QPushButton> message_button = new QPushButton("Messages");
    message_button->setToolTip("View recent messages.");
    connect(message_button, SIGNAL(clicked()), this, SLOT(death_messages()));
    vlay->addWidget(message_button);

    // Add the "Character Dump" button
    QPointer<QPushButton> file_dump_button = new QPushButton("Character Dump");
    file_dump_button->setToolTip("Create a final character dump.");
    connect(file_dump_button, SIGNAL(clicked()), this, SLOT(death_file_dump()));
    vlay->addWidget(file_dump_button);

    // Add the "Screenshot" button
    QPointer<QPushButton> screenshot_button = new QPushButton("Screenshot");
    screenshot_button->setToolTip("Create a final screenshot.");
    connect(screenshot_button, SIGNAL(clicked()), this, SLOT(death_screenshot()));
    vlay->addWidget(screenshot_button);

    // Add the "Scores" button
    QPointer<QPushButton> scores_button = new QPushButton("View Scores");
    scores_button->setToolTip("View the scores for all characters.");
    connect(scores_button, SIGNAL(clicked()), this, SLOT(death_scores()));
    vlay->addWidget(scores_button);

    // Add the "Examine" button
    QPointer<QPushButton> examine_button = new QPushButton("Examine Items");
    examine_button->setToolTip("Examine Items in Player Inventory and Backpack.");
    connect(examine_button, SIGNAL(clicked()), this, SLOT(death_examine()));
    vlay->addWidget(examine_button);

    // Add the "View Notes" button
    QPointer<QPushButton> notes_button = new QPushButton("View Notes");
    notes_button->setToolTip("View a log of all notes from the game.");
    connect(notes_button, SIGNAL(clicked()), this, SLOT(death_notes()));
    vlay->addWidget(notes_button);

    // Add the "Spoilers" button
    QPointer<QPushButton> spoilers_button = new QPushButton("Create Spoilers");
    spoilers_button->setToolTip("Create spoiler files.");
    connect(spoilers_button, SIGNAL(clicked()), this, SLOT(death_spoilers()));
    vlay->addWidget(spoilers_button);

    vlay->addStretch(1);

    //Add a close button on the right side
    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(reject()));
    vlay->addWidget(buttons);

    setLayout(vlay);
    setWindowTitle("Press 'Close' when done:");
    this->exec();
}

/*
 * Display the winner crown
 */
static void display_winner(void)
{
    //TODO - make a winner display screen
}

/*
 * Display the tombstone
 */
static void print_tomb(void)
{
    //TODO - make a splash screen
}


/*
 * Know inventory and home items upon death
 */
static void death_knowledge(void)
{
    store_type *st_ptr = &store[STORE_HOME];
    object_type *o_ptr;

    int i;

    /* Know everything in the inven/equip */
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        o_ptr = &inventory[i];
        if (!o_ptr->k_idx) continue;

        o_ptr->mark_fully_known(TRUE);
    }

    /* Know everything in the home */
    for (i = 0; i < st_ptr->stock_num; i++)
    {
        o_ptr = &st_ptr->stock[i];
        if (!o_ptr->k_idx) continue;

        o_ptr->mark_fully_known(TRUE);
    }

    /* Hack -- Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);
    handle_stuff();
}

static void write_death_note(QString long_day)
{
    write_note(QString("<b>%1 the %2 %3 was killed by %4 on %5.<b>")
                   .arg(op_ptr->full_name) .arg(p_info[p_ptr->prace].pr_name) .arg(c_info[p_ptr->pclass].cl_name)
                   .arg(p_ptr->died_from) .arg(long_day), p_ptr->depth);
}

void player_death(void)
{
    /* Try to make a player ghost template */
    add_player_ghost_entry();

    /* Retire in the town in a good state */
    if (p_ptr->total_winner)
    {
        p_ptr->depth = 0;
        p_ptr->died_from = "Ripe Old Age";
        p_ptr->exp = p_ptr->max_exp;
        p_ptr->lev = p_ptr->max_lev;
        p_ptr->au += 10000000L;

        display_winner();
    }

    QDate today = QDate::currentDate();
    QTime right_now = QTime::currentTime();
    QString long_day = QString("%1 at %2") .arg(today.toString()) .arg(right_now.toString());

    write_death_note(long_day);

    print_tomb();
    death_knowledge();
    enter_score(long_day);

    /* Save dead player */
    if (!save_player())
    {
        message(QString("death save failed!"));
    }

    // Hack - update everything onscreen
    p_ptr->do_redraws = TRUE;
    redraw_stuff();
    p_ptr->do_redraws = FALSE;

    // Automatic character dump
    if (death_char_dump)
    {
        save_character_file();
        save_screenshot(FALSE);
    }

    p_ptr->in_death_menu = TRUE;
    PlayerDeathDialog();
    p_ptr->in_death_menu = FALSE;
}

