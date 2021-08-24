
/* File: ommand_list.cpp */

/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/command_list.h>
#include <QDialogButtonBox>
#include <src/cmds.h>
#include <src/player_command.h>
#include <src/object_all_menu.h>
#include <src/messages.h>

static struct command_desc list_commands_targeting[] =
{
    {" ", NULL},
    {"<h3>All Targeting Modes</h3>", NULL},
    {"Cancel targeting", "ESC, or 'x'"},
    {"Targeting Help", "?"},
    {" ", NULL},
    {"<h3>Directional Targeting Modes</h3>", NULL},
    {"Any Mouseclick - Select Direction of selected square relative to the player.", NULL},
    {" ", NULL},
    {"<h3>Aiming, Interactive, and Manual Targeting Modes</h3>", NULL},
    {"Toggle Between Interactive and Manual Targeting Mode", "'m' or '*'"},
    {"Use the Closest Target", "'c' or ','"},
    {"Select Current Target", "'h' , '5' or '.'"},
    {"Mouse Double-click - Select Target", NULL},
    {"Single Mouseclick on Square- Select grid and switch to manual targeting mode", NULL},
    {"Second Single Mouseclick on same square - Select Square As Target", NULL},
    {" ", NULL},
    {"<h3>Interactive Targeting Mode</h3>", NULL},
    {"Select Next Target", "'+' or <space>"},
    {"Select Previous Target", "'-'"},
    {"See Information on Current Square", "'!' or 'l'"},
    {"Use Direction Keys to Move To Next Possible Target In That General Direction", NULL},
    {"Use the Mouse Wheel to Target the Next or Previous Target", NULL},
    {" ", NULL},
    {"<h3>Manual Targeting Mode</h3>", NULL},
    {"Select Next Target", "'+' or <space>"},
    {"Target Player", "'&' or 'p'"},
    {"See Information on Current Square", "'!' or 'l'"},
    {"Use Direction Keys to Move Target One Square In That Direction", NULL},
    {NULL, NULL},
};

static struct command_desc list_commands_new[] =
{
    {"Activate", "a"},
    {"Alter (set direction later)", "z or '+'"},
    {"Bash Door or Monster", "ALT-D"},
    {"Browse Spellbooks", "CTRL-M"},
    {"Cast Magic Spells", "m"},
    {"Center Player", "ESC"},
    {"Character Screen", "C (shift-c)"},
    {"Close Door", "c"},
    {"Destroy Item", "k"},
    {"Disarm Trap", "D (shift-d)"},
    {"Drop Item", "d"},
    {"Equipment (view)", "e"},
    {"Fire Ammunition",	"f"},
    {"Fire At Nearest", "F (shift-f)"},
    {"Fuel Lantern/Torch", "ALT-F"},
    {"Go Down Staircase", "<"},
    {"Do Up Staircase	", ">"},
    {"Inscribe Item", "{"},
    {"Inspect All Objects", "i"},
    {"Inspect Object", "I (shift-i)"},
    {"Jam a Door", "Q (shift-q}"},
    {"Learn Spells, Prayers, or Incantations", "M (shift-m)"},
    {"Look", "l"},
    {"Make/Modify Trap", "O (shift-o)"},
    {"Message Log", "CTRL-L"},
    {"New Game NPPAngband", "CTRL-A"},
    {"New Game NPPMoria", "CTRL-R"},
    {"Open Door", "o"},
    {"Open Savefile", "CTRL-F"},
    {"Option Settings", "="},
    {"Pickup Items", "+"},
    {"Quest Desription", "q"},
    {"Quit NPP Games", "CTRL-Q"},
    {"Redraw Screen", "ALT-E"},
    {"Rest Until Fully Healed	", "r"},
    {"Rest To Recover All HP ", "CTRL-R"},
    {"Rest To Recover All SP", "ALT-R"},
    {"Rest To Recover All HP + SP", "(CTRL + ALT)-R"},
    {"Rest (set rest mode later)", "R (shift-r)"},
    {"Repeat Level Feeling", "CTRL-F"},
    {"Repeat Previous Command", "'p' or '0'"},
    {"Run (set direction later)", "."},
    {"Save Character", "CTRL-S"},
    {"Save Character and Close Game", "CTRL-X"},
    {"Save Character As", "CTRL-W"},
    {"Save Character Dump", "SHIFT-ALT-C"},
    {"Save Screenshot (html file)", ")"},
    {"Save Screenshot (png file", "("},
    {"Search", "s"},
    {"Search (Toggle)", "S (shift-s)"},
    {"Spike A Door", "CTRL-D"},
    {"Swap Weapon", "x"},
    {"Take Off Item", "W (shift-w) or '-'"},
    {"Target Closest", "'*' or ','"},
    {"Terminate Character", "(CTRL + ALT)-Q"},
    {"Throw Item", "L (shift-l)"},
    {"Tunnel (set direction later)", "CTRL-O"},
    {"Uninscribe Item", "}"},
    {"Use Item", "e"},
    {"Wield Item", "w"},
    {"Wizard Mode (cheat)", "ALT-a"},
    {"Write Note", ":"},
    // The null entry at the end is essential for initializing the table of groups.
    {NULL, NULL},
};

static struct command_desc list_commands_angband[] =
{
    {"Activate", "A (shift-a)"},
    {"Aim a Wand", "a"},
    {"Alter", "+"},
    {"Bash Door or Monster", "B (shift-b)"},
    {"Browse Spellbooks", "b"},
    {"Cast Magic Spells", "m"},
    {"Center Player", "ESC, or L (shift-l)"},
    {"Character Screen", "C (shift-c)"},
    {"Close Door", "c"},
    {"Destroy Item", "k"},
    {"Disarm Trap", "D (shift-d)"},
    {"Drop Item", "d"},
    {"Eat Food", "E (shift-e)"},
    {"Equipment (view)", "e"},
    {"Fire Ammunition",	"f"},
    {"Fire At Nearest",	"h"},
    {"Go Down Staircase", "<"},
    {"Go Up Staircase", ">"},
    {"Hold", "'_' or ','"},
    {"Inscribe Item", "{"},
    {"Inspect Inventory", "i"},
    {"Inspect Object", "I (shift-i)"},
    {"Jam a Door", "j"},
    {"Learn Spells, Prayers, or Incantations", "G (shift-g)"},
    {"Look", "l"},
    {"Make/Modify Trap", "O (shift-o)"},
    {"Message Log", "CTRL-P"},
    {"New Game NPPAngband", "CTRL-A"},
    {"New Game NPPMoria", "CTRL-R"},
    {"Open Door", "o"},
    {"Open Savefile", "CTRL-F"},
    {"Option Settings", "="},
    {"Pickup Items", "g"},
    {"Pray a prayer", "p"},
    {"Quaff a Potion", "q"},
    {"Quest Desription", "ALT-Q"},
    {"Quit NPP Games", "CTRL-Q"},
    {"Read a Scroll", "r"},
    {"Redraw Screen", "CTRL-R"},
    {"Repeat Previous Command", "'n' or '0'"},
    {"Rest", "R (shift-r)"},
    {"Repeat Level Feeling", "CTRL-F"},
    {"Run", "."},
    {"Save Character", "CTRL-S"},
    {"Save Character and Close Game", "CTRL-X"},
    {"Save Character As", "CTRL-W"},
    {"Save Character Dump", "SHIFT-ALT-C"},
    {"Save Screenshot (html file)", ")"},
    {"Save Screenshot (png file)", "("},
    {"Search", "s"},
    {"Search (Toggle)", "S (shift-s)"},
    {"Spike A Door", "j"},
    {"Swap Weapon", "x"},
    {"Take Off Item", "t"},
    {"Target CLosest", "'*'' or Apostrophe"},
    {"Terminate Character", "Q (shift-q)"},
    {"Throw Item", "v"},
    {"Tunnel", "T (shift-t)"},
    {"Uninscribe Item", "}"},
    {"Use a Staff", "u"},
    {"Walk (flip pickup)","-"},
    {"Walk (normal pickup)",";"},
    {"Wield Item", "w"},
    {"Wizard Mode (cheat)", "ALT-a"},
    {"Write Note", ":"},
    {"Zap a Rod", "z"},
    // The null entry at the end is essential for initializing the table of groups.
    {NULL, NULL},
};

static struct command_desc list_commands_roguelike[] =
{
    {"Activate", "A (shift-a)"},
    {"Aim a Wand", "z"},
    {"Alter", "+"},
    {"Bash Door or Monster", "f"},
    {"Browse Spellbooks", "P (shift-p)"},
    {"Cast Magic Spells", "m"},
    {"Center Player", "ESC or W (shift-w)"},
    {"Character Screen", "C (shift c)"},
    {"Close Door", "c"},
    {"Destroy Item", "k"},
    {"Disarm Trap", "D (shift-d)"},
    {"Drop Item", "d"},
    {"Eat Food", "E (shift-e)"},
    {"Equipment (view", "e"},
    {"Fire Ammunition",	"f"},
    {"Fire At Nearest",	"t"},
    {"Fuel Lantern/Torch", "F (shift-f)"},
    {"Go Down Staircase", "<"},
    {"Go Up Staircase	", ">"},
    {"Hold", "'_' '.' or 'g'"},
    {"Inscribe Item", "{"},
    {"Inspect Inventory", "i"},
    {"Inspect Object", "I (shift-i)"},
    {"Jam a Door", "S (shift-s)"},
    {"Learn Spells, Prayers, or Incantations", "G (shift-g)"},
    {"Look", "x"},
    {"Make/Modify Trap", "O (shift-o)"},
    {"Message Log", "CTRL-P"},
    {"New Game NPPAngband", "CTRL-A"},
    {"New Game NPPMoria", "CTRL-R"},
    {"Open Door", "o"},
    {"Open Savefile", "CTRL-F"},
    {"Option Settings", "="},
    {"Pickup Items", "]"},
    {"Pray a prayer", "p"},
    {"Quaff a Potion", "q"},
    {"Quest Desription", "ALT-Q"},
    {"Quit NPP Games", "CTRL-Q"},
    {"Read a Scroll", "r"},
    {"Redraw Screen", "CTRL-R"},
    {"Repeat Previous Command", "v"},
    {"Rest", "R (shift-r)"},
    {"Repeat Level Feeling", "CTRL-F"},
    {"Run", ","},
    {"Save Character", "CTRL-S"},
    {"Save Character and Close Game", "CTRL-X"},
    {"Save Character As", "CTRL-W"},
    {"Save Character Dump", "SHIFT-ALT-C"},
    {"Save Screenshot (html file)", ")"},
    {"Save Screenshot (png file", "("},
    {"Search", "s"},
    {"Search (Toggle)", "#"},
    {"Spike A Door", "j"},
    {"Swap Weapon", "X (shift-x)"},
    {"Take Off Item", "T (shift-t)"},
    {"Target Closest", "'*'' or Apostrophe"},
    {"Terminate Character", "Q (shift-q)"},
    {"Throw Item", "v"},
    {"Tunnel", "CTRL-T"},
    {"Uninscribe Item", "}"},
    {"Use a Staff", "Z (shift-z)"},
    {"Walk (flip pickup)","-"},
    {"Walk (normal pickup)",";"},
    {"Wield Item", "w"},
    {"Wizard Mode (cheat)", "ALT-a"},
    {"Write Note", ":"},
    {"Zap a Rod", "a"},
    // The null entry at the end is essential for initializing the table of groups.
    {NULL, NULL},
};


void KeyboardCommandList::add_keyboard_commands(QGridLayout *return_layout)
{

    int x = 0;

    int command_count = 0;

    // Count the number of help commands
    while (TRUE)
    {
        command_desc *cmd_ptr;
        if (which_keyset == KEYSET_NEW) cmd_ptr = &list_commands_new[x++];
        else if (which_keyset == KEYSET_ANGBAND) cmd_ptr = &list_commands_angband[x++];
        else /* KEYSET_ROGUE */ cmd_ptr = &list_commands_roguelike[x++];

        // Null pointer means we are done
        if (!cmd_ptr->command_title.length()) break;

        command_count++;
    }

    command_count = (command_count / 3) + 1;

    x = 0;
    int row = 0;
    int col_count = 0;

    while (TRUE)
    {
        command_desc *cmd_ptr;
        if (which_keyset == KEYSET_NEW) cmd_ptr = &list_commands_new[x++];
        else if (which_keyset == KEYSET_ANGBAND) cmd_ptr = &list_commands_angband[x++];
        else /* KEYSET_ROGUE */ cmd_ptr = &list_commands_roguelike[x++];

        if (row == command_count)
        {
            col_count++;
            row = 0;
        }
        int col = col_count * 4;

        // Null pointer means we are done
        if (!cmd_ptr->command_title.length()) break;

        QPointer<QLabel> this_title = new QLabel();
        make_standard_label(this_title, QString(cmd_ptr->command_title), TERM_BLUE);
        return_layout->addWidget(this_title, row, col++, Qt::AlignLeft);

        QPointer<QLabel> dummy = new QLabel("  ");
        return_layout->addWidget(dummy, row, col++);

        QPointer<QLabel> this_key = new QLabel();
        make_standard_label(this_key, QString(cmd_ptr->command_key), TERM_BLUE);

        // HTML throws off the display of this character
        if (strings_match(QString(cmd_ptr->command_key), QString("<")))
        {
            this_key->setText("<");
            this_key->setStyleSheet("color: blue; font-weight: bold;");
        }
        return_layout->addWidget(this_key, row, col++, Qt::AlignLeft);

        if (col_count < 2)
        {
            QPointer<QLabel> dummy = new QLabel("    ");
            return_layout->addWidget(dummy, row, col);
        }

        row++;
    }

    QPointer<QLabel> dummy = new QLabel("   ");
    return_layout->addWidget(dummy, row, 0);
}



static struct command_desc dir_commands_new[] =
{
    {"Hold", "'h', or '5'"},
    {"Walk", "direction key"},
    {"Jump (Walk With Flip Pickup)", "(CTRL + ALT)direction key"},
    {"Run", "(Shift)direction key"},
    {"Alter", "(ALT)direction key"},
    {"Look (Change Panel)", "(CTRL)direction key"},
    {"Tunnel (Specified Direction)", "(CTRL + SHIFT)direction key"},
    // The null entry at the end is essential for initializing the table of groups.
    {NULL, NULL},
};

static struct command_desc dir_commands_angband[] =
{
    {"Hold", "'h', or '5'"},
    {"Walk", "direction key"},
    {"Jump (Walk With Flip Pickup)", "(CTRL + ALT)direction key"},
    {"Run", "(Shift)direction key"},
    {"Alter", "(ALT)direction key"},
    {"Look (Change Panel)", "(CTRL)direction key"},
    {"Tunnel (Specified Direction)", "(CTRL + SHIFT)direction key"},
    // The null entry at the end is essential for initializing the table of groups.
    {NULL, NULL},
};

static struct command_desc dir_commands_roguelike[] =
{
    {"Walk", "direction key"},
    {"Jump (Walk With Flip Pickup)", "(CTRL + ALT)direction key"},
    {"Run", "(Shift)direction key"},
    {"Alter", "(ALT)direction key"},
    {"Look (Change Panel)", "(CTRL)direction key"},
    {"Tunnel (Specified Direction)", "(CTRL + SHIFT)direction key"},
    // The null entry at the end is essential for initializing the table of groups.
    {NULL, NULL},
};

void KeyboardCommandList::add_dir_commands(QGridLayout *return_layout)
{
    int x = 0;

    while (TRUE)
    {
        command_desc *cmd_ptr;
        if (which_keyset == KEYSET_NEW) cmd_ptr = &dir_commands_new[x];
        else if (which_keyset == KEYSET_ANGBAND) cmd_ptr = &dir_commands_angband[x];
        else /* KEYSET_ROGUE */ cmd_ptr = &dir_commands_roguelike[x];

        int col = 0;

        // Null pointer means we are done
        if (!cmd_ptr->command_title.length()) break;

        QPointer<QLabel> this_title = new QLabel();
        make_standard_label(this_title, QString(cmd_ptr->command_title), TERM_BLUE);
        return_layout->addWidget(this_title, x, col++, Qt::AlignLeft);

        QPointer<QLabel> dummy = new QLabel("   ");
        return_layout->addWidget(dummy, x, col++);

        QPointer<QLabel> this_key = new QLabel();
        make_standard_label(this_key, QString(cmd_ptr->command_key), TERM_BLUE);
        return_layout->addWidget(this_key, x++, col++, Qt::AlignLeft);
    }

    QPointer<QLabel> dummy = new QLabel("   ");
    return_layout->addWidget(dummy, x, 0);
}

void KeyboardCommandList::add_dir_keyboard(QVBoxLayout *return_layout, bool keyboard)
{
    QPointer<QLabel> top_label = new QLabel;

    QString this_title  = "Keypad Dirs";
    if (keyboard) this_title = "Keyboard Dirs";
    make_standard_label(top_label, this_title, TERM_DARK);
    return_layout->addWidget(top_label, Qt::AlignCenter);

    QString letters = "789456123";
    if (keyboard)
    {
        if (which_keyset == KEYSET_ROGUE) letters = "ykuhglbjn";
        else letters = "tyughjvbn";
    }

    QPointer<QGridLayout> dir_keyboard = new QGridLayout;
    return_layout->addLayout(dir_keyboard);

    int which_char = 0;

    for (int i = 0; i < 3; i++)
    {
        for (int j = 0; j < 3; j++)
        {
            QPointer<QLabel> this_label = new QLabel();
            make_standard_label(this_label, QString(letters[which_char++]), TERM_BLUE);
            dir_keyboard->addWidget(this_label, j, i, Qt::AlignCenter);
        }
    }

    return_layout->addStretch(1000);
}



KeyboardCommandList::KeyboardCommandList(void): NPPDialog()
{
    central = new QWidget;
    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    // IMPORTANT: it must be called AFTER setting the layout
    this->setClient(central);

    QPointer<QLabel> main_prompt = new QLabel(QString("<h2>Directional Commands</h2>"));
    main_layout->addWidget(main_prompt, Qt::AlignCenter);

    QPointer<QHBoxLayout> top_across = new QHBoxLayout;
    main_layout->addLayout(top_across);

    QPointer<QVBoxLayout> vlay_key_dirs = new QVBoxLayout;
    add_dir_keyboard(vlay_key_dirs, TRUE);
    top_across->addLayout(vlay_key_dirs);

    if (which_keyset != KEYSET_ROGUE)
    {
        top_across->addStretch(1);
        QPointer<QVBoxLayout> vlay_pad_dirs = new QVBoxLayout;
        add_dir_keyboard(vlay_pad_dirs, FALSE);
        top_across->addLayout(vlay_pad_dirs);
    }

    top_across->addStretch(1);

    QPointer<QVBoxLayout> vlay_dir_commands = new QVBoxLayout;
    QPointer<QGridLayout> glay_dir_commands = new QGridLayout;
    vlay_dir_commands->addLayout(glay_dir_commands);
    add_dir_commands(glay_dir_commands);
    vlay_dir_commands->addStretch(1);
    top_across->addLayout(vlay_dir_commands);
    top_across->addStretch(1);

    QPointer<QLabel> keyboard_prompt = new QLabel(QString("<h2>Keyboard Commands</h2>"));
    main_layout->addWidget(keyboard_prompt, Qt::AlignCenter);

    QPointer<QGridLayout> glay_key_commands = new QGridLayout;
    add_keyboard_commands(glay_key_commands);
    main_layout->addLayout(glay_key_commands);

    QDialogButtonBox buttons;
    buttons.setStandardButtons(QDialogButtonBox::Ok);
    connect(&buttons, SIGNAL(accepted()), this, SLOT(close()));
    main_layout->addWidget(&buttons);

    setWindowTitle(tr("Command List"));

    this->clientSizeUpdated();

    this->exec();
}

static struct command_desc list_commands_mouse[] =
{
    {"<h3>Single Clicks on dungeon square (other than player square)</h3>", NULL},
    {"Left Click on a known dungeon square to run to that spot.", NULL},
    {"Middle Click on any dungeon square to walk one square in that direction.", NULL},
    {"Right Click on a dungeon square to learn about the contents of that square", NULL},
    {"Click Extra Button 1 - TBD", NULL},
    {"Click Extra Button 2 - TBD", NULL},
    {" ", NULL},
    {"<h3>Single Clicks on player</h3>", NULL},
    {"Use Magic (Cast, Pray, Chant)", NULL},
    {"Use an Item", NULL},
    {"Fire Ammunition", NULL},
    {"Click Extra Button 1 to bring up the object handling dialog", NULL},
    {"Click Extra Button 2 to bring up the character screen dialog", NULL},
    {" ", NULL},
    {"<h3>Double Clicks</h3>", NULL},
    {"Left Double-Click on any dungeon square to run in that direction.", NULL},
    {"Middle Double-Click on any dungeon square to alter the square in that direction.", NULL},
    {"Right Double-Click to target that square.", NULL},
    {"Double-Click Extra Button 1 - TBD", NULL},
    {"Double-Click Extra Button 2 - TBD", NULL},
    {" ", NULL},
    {"<h3>Mouse Wheel</h3>", NULL},
    {"Increase or decrease the tile multiplier", NULL},
    {"Use the mouse wheel over the scrollbars to look around the dungeon.", NULL},
    // The null entry at the end is essential for initializing the table of groups.
    {NULL, NULL},
};


void MouseCommandList::add_mouse_commands(QVBoxLayout *return_layout)
{
    int x = 0;

    while (TRUE)
    {
        command_desc *cmd_ptr = &list_commands_mouse[x++];

        // Null pointer means we are done
        if (!cmd_ptr->command_title.length()) break;

        QPointer<QLabel> this_title = new QLabel();
        make_standard_label(this_title, QString(cmd_ptr->command_title), TERM_BLUE);
        return_layout->addWidget(this_title, Qt::AlignLeft);
    }

    QPointer<QLabel> dummy = new QLabel("   ");
    return_layout->addWidget(dummy, x, 0);
}

MouseCommandList::MouseCommandList(void): NPPDialog()
{
    central = new QWidget;
    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    this->setClient(central);  // IMPORTANT: it must be called AFTER setting the layout

    QPointer<QLabel> mouse_prompt = new QLabel(QString("<h2>Mouse Commands</h2>"));
    main_layout->addWidget(mouse_prompt, Qt::AlignCenter);

    QPointer<QVBoxLayout> vlay_mouse_commands = new QVBoxLayout;
    add_mouse_commands(vlay_mouse_commands);
    main_layout->addLayout(vlay_mouse_commands);

    QDialogButtonBox buttons;
    buttons.setStandardButtons(QDialogButtonBox::Ok);
    connect(&buttons, SIGNAL(accepted()), this, SLOT(close()));
    main_layout->addWidget(&buttons);

    main_layout->addStretch(1);

    setWindowTitle(tr("Mouse Command List"));

    this->clientSizeUpdated();

    this->exec();
}

void TargetCommandList::add_dir_targeting(QVBoxLayout *return_layout, bool keyboard)
{
    QPointer<QLabel> top_label = new QLabel;

    QString this_title  = "Keypad Dirs";
    if (keyboard) this_title = "Keyboard Dirs";
    make_standard_label(top_label, this_title, TERM_BLUE);
    return_layout->addWidget(top_label, Qt::AlignCenter);

    QString letters = "7894 6123";
    if (keyboard) letters = "tyug jvbn";

    QPointer<QGridLayout> dir_keyboard = new QGridLayout;
    return_layout->addLayout(dir_keyboard);

    int which_char = 0;

    for (int i = 0; i < 3; i++)
    {
        for (int j = 0; j < 3; j++)
        {
            QPointer<QLabel> this_label = new QLabel();
            make_standard_label(this_label, QString(letters[which_char++]), TERM_BLUE);
            dir_keyboard->addWidget(this_label, j, i, Qt::AlignCenter);
        }
    }

    return_layout->addStretch(1000);
}


void TargetCommandList::add_targeting_commands(QGridLayout *return_layout)
{
    int x = 0;

    while (TRUE)
    {

        int col = 0;
        command_desc *cmd_ptr = &list_commands_targeting[x++];

        // Null pointer means we are done
        if (!cmd_ptr->command_title.length()) break;

        QPointer<QLabel> this_title = new QLabel();
        make_standard_label(this_title, QString(cmd_ptr->command_title), TERM_BLUE);
        return_layout->addWidget(this_title, x, col++, Qt::AlignLeft);

        if (!cmd_ptr->command_key.length()) continue;

        QPointer<QLabel> dummy = new QLabel("   ");
        return_layout->addWidget(dummy, x, col++);

        QPointer<QLabel> this_key = new QLabel();
        make_standard_label(this_key, QString(cmd_ptr->command_key), TERM_BLUE);
        return_layout->addWidget(this_key, x, col++, Qt::AlignLeft);
    }
}

TargetCommandList::TargetCommandList(void): NPPDialog()
{
    central = new QWidget;
    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    this->setClient(central);  // IMPORTANT: it must be called AFTER setting the layout

    QPointer<QLabel> targeting_prompt = new QLabel(color_string(QString("<h2>Targeting Commands</h2>"), TERM_BLUE));
    main_layout->addWidget(targeting_prompt, Qt::AlignCenter);

    QPointer<QHBoxLayout> top_across = new QHBoxLayout;
    main_layout->addLayout(top_across);

    QPointer<QVBoxLayout> vlay_key_dirs = new QVBoxLayout;
    add_dir_targeting(vlay_key_dirs, TRUE);
    top_across->addLayout(vlay_key_dirs);

    top_across->addStretch(1);
    QPointer<QVBoxLayout> vlay_pad_dirs = new QVBoxLayout;
    add_dir_targeting(vlay_pad_dirs, FALSE);
    top_across->addLayout(vlay_pad_dirs);

    top_across->addStretch(1);

    QPointer<QGridLayout> glay_target_commands = new QGridLayout;
    add_targeting_commands(glay_target_commands);
    main_layout->addLayout(glay_target_commands);

    QDialogButtonBox buttons;
    buttons.setStandardButtons(QDialogButtonBox::Ok);
    connect(&buttons, SIGNAL(accepted()), this, SLOT(close()));
    main_layout->addWidget(&buttons);

    main_layout->addStretch(1);

    setWindowTitle(tr("Targeting Command List"));

    this->clientSizeUpdated();

    this->exec();
}

void do_cmd_list_keyboard_commands(void)
{
    KeyboardCommandList();
}

void do_cmd_list_mouse_commands(void)
{
    MouseCommandList();
}
void do_cmd_list_targeting_commands(void)
{
    TargetCommandList();
}

static void process_move_key(int dir, bool shift_key, bool alt_key, bool ctrl_key, bool meta_key)
{
    if (!character_dungeon) return;


    // Flip pickup
    if (ctrl_key && alt_key) do_cmd_walk(dir, TRUE);
    else if (ctrl_key && shift_key) do_cmd_tunnel(dir);
    else if (ctrl_key) ui_change_panel(dir);
    else if (alt_key) do_cmd_alter(dir);
    else if (shift_key)  do_cmd_run(dir);
    else if (meta_key) do_cmd_tunnel(dir);
    else do_cmd_walk(dir, FALSE);
}

void commands_new_keyset(int key_press, bool shift_key, bool alt_key, bool ctrl_key, bool meta_key)
{
    bool using_mods = FALSE;
    if (shift_key || alt_key || ctrl_key || meta_key) using_mods = TRUE;

    // Normal mode
    switch (key_press)
    {
        // ESCAPE
        case Qt::Key_Escape:
        {
            ui_center(p_ptr->py, p_ptr->px);
            break;
        }

        // Move down
        case Qt::Key_2:
        case Qt::Key_Down:
        case Qt::Key_B:
        {
            process_move_key(2, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }

        // Move up
        case Qt::Key_8:
        case Qt::Key_Up:
        case Qt::Key_Y:
        {
            process_move_key(8, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }

        // Move left
        case Qt::Key_4:
        case Qt::Key_Left:
        case Qt::Key_G:
        {
            process_move_key(4, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move right
        case Qt::Key_6:
        case Qt::Key_Right:
        case Qt::Key_J:
        {
            process_move_key(6, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally left and up
        case Qt::Key_7:
        case Qt::Key_T:
        case Qt::Key_Home:
        {
            process_move_key(7, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally right and up
        case Qt::Key_9:
        case Qt::Key_U:
        case Qt::Key_PageUp:
        {
            process_move_key(9, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally left and down
        case Qt::Key_1:
        case Qt::Key_V:
        case Qt::Key_End:
        {
            process_move_key(1, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally right and down
        case Qt::Key_3:
        case Qt::Key_N:
        case Qt::Key_PageDown:
        {
            process_move_key(3, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        case Qt::Key_5:
        case Qt::Key_H:
        case Qt::Key_Clear:
        {
            do_cmd_hold();
            break;
        }
        case Qt::Key_A:
        {
            if (alt_key)           do_cmd_wizard_mode();
            else if (!using_mods)   do_cmd_activate();
            break;
        }
        case Qt::Key_C:
        case Qt::Key_0:
        case Qt::Key_Insert:
        {
            if (shift_key && alt_key) save_character_file();
            else if (shift_key)     do_cmd_character_screen();
            else if (!using_mods)   do_cmd_close(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_D:
        {
            if (ctrl_key)           do_cmd_spike(DIR_UNKNOWN);
            else if (alt_key)       do_cmd_bash(DIR_UNKNOWN);
            else if (shift_key)     do_cmd_disarm(DIR_UNKNOWN);
            else if (!using_mods)   do_cmd_drop();
            break;
        }
        case Qt::Key_E:
        {
            if (alt_key)            ui_redraw_all();
            else if (shift_key)     do_cmd_all_objects(TAB_EQUIP);
            else if (!using_mods)   do_cmd_use_item();
            break;
        }
        case Qt::Key_F:
        {
            if (ctrl_key)           do_cmd_feeling();
            else if (alt_key)       do_cmd_refuel();
            else if (shift_key)     do_cmd_fire_at_nearest();
            else if (!using_mods)   do_cmd_fire();
            break;
        }
        case Qt::Key_I:
        {
            if (shift_key)          do_cmd_examine();
            else if (!using_mods)   do_cmd_all_objects(TAB_INVEN);
            break;
        }
        case Qt::Key_K:
        {
            if (!using_mods)        do_cmd_destroy();
            break;
        }
        case Qt::Key_L:
        {
            if (shift_key)          do_cmd_throw();
            if (!using_mods)        do_cmd_look();
            break;
        }
        case Qt::Key_M:
        {
            if (shift_key)          do_cmd_study(-1);
            else if (ctrl_key)      do_cmd_browse(-1);
            else if (!using_mods)   do_cmd_cast(-1);
            break;
        }
        case Qt::Key_O:
        {
            if (ctrl_key)           do_cmd_tunnel(DIR_UNKNOWN);
            else if (shift_key)     do_cmd_make_trap(DIR_UNKNOWN);
            else if (!using_mods)   do_cmd_open(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_P:
        {
            if (!using_mods)        do_cmd_repeat();
            break;
        }
        case Qt::Key_Q:
        {
            if (ctrl_key && alt_key)do_cmd_suicide();
            else if (!using_mods)   do_cmd_quest_desc();
            break;
        }
        case Qt::Key_R:
        {
            if (alt_key && ctrl_key)do_cmd_rest_specific(REST_BOTH_SP_HP);
            else if (ctrl_key)      do_cmd_rest_specific(REST_HP);
            else if (alt_key)       do_cmd_rest_specific(REST_SP);
            else if (shift_key)     do_cmd_rest();
            else if (!using_mods)   do_cmd_rest_specific(REST_COMPLETE);
            break;
        }
        case Qt::Key_S:
        {
            if (shift_key)          do_cmd_toggle_search();
            else if (!using_mods)   do_cmd_search();
            break;
        }
        case Qt::Key_W:
        {
            if (shift_key)          do_cmd_takeoff();
            else if (!using_mods)   do_cmd_wield();
            break;
        }
        case Qt::Key_X:
        {
            if (!using_mods)        do_cmd_swap_weapon();
            break;
        }
        case Qt::Key_Z:
        {
            if (!using_mods)        do_cmd_alter(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_Asterisk:
        case Qt::Key_Comma:
        {
            target_set_closest(TARGET_KILL);
            break;
        }
        case Qt::Key_BraceLeft:
        {
            do_cmd_inscribe();
            break;
        }
        case Qt::Key_BraceRight:
        {
            do_cmd_uninscribe();
            break;
        }
        case Qt::Key_Greater:
        {
            do_cmd_go_down();
            break;
        }
        case Qt::Key_Less:
        {
            do_cmd_go_up();
            break;
        }
        case Qt::Key_Period:
        {
            do_cmd_run(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_Plus:
        {
            do_cmd_alter(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_Minus:
        {
            do_cmd_takeoff();
            break;
        }
        case Qt::Key_Colon:
        {
            do_cmd_write_note();
            break;
        }
        case Qt::Key_ParenRight:
        {
            save_screenshot(FALSE);
            break;
        }
        case Qt::Key_ParenLeft:
        {
            save_screenshot(TRUE);
            break;
        }
        default:
        {
            break;
        }
    }
}

void commands_angband_keyset(int key_press, bool shift_key, bool alt_key, bool ctrl_key, bool meta_key)
{
    bool using_mods = FALSE;
    if (shift_key || alt_key || ctrl_key || meta_key) using_mods = TRUE;

    // Normal mode
    switch (key_press)
    {
        // ESCAPE
        case Qt::Key_Escape:
        {
            ui_center(p_ptr->py, p_ptr->px);
            break;
        }

        // Move down
        case Qt::Key_2:
        case Qt::Key_Down:
        {
            process_move_key(2, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }

        // Move up
        case Qt::Key_8:
        case Qt::Key_Up:
        {
            process_move_key(8, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }

        // Move left
        case Qt::Key_4:
        case Qt::Key_Left:
        {
            process_move_key(4, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move right
        case Qt::Key_6:
        case Qt::Key_Right:
        {
            process_move_key(6, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally left and up
        case Qt::Key_7:
        case Qt::Key_Home:
        {
            process_move_key(7, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally right and up
        case Qt::Key_9:
        case Qt::Key_PageUp:
        {
            process_move_key(9, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally left and down
        case Qt::Key_1:
        case Qt::Key_End:
        {
            process_move_key(1, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally right and down
        case Qt::Key_3:
        case Qt::Key_PageDown:
        {
            process_move_key(3, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        case Qt::Key_5:
        case Qt::Key_Clear:
        {
            do_cmd_hold();
            break;
        }
        case Qt::Key_A:
        {
            if (alt_key)           do_cmd_wizard_mode();
            else if (shift_key)          do_cmd_activate();
            else if (!using_mods)   do_cmd_aim_wand();
            break;
        }
        case Qt::Key_B:
        {
            if (shift_key)          do_cmd_bash(DIR_UNKNOWN);
            else if (!using_mods)   do_cmd_browse(-1);
            break;
        }
        case Qt::Key_C:
        {
            if (shift_key && alt_key) save_character_file();
            else if (shift_key)     do_cmd_character_screen();
            else if (!using_mods)   do_cmd_close(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_D:
        {
            if (shift_key)          do_cmd_disarm(DIR_UNKNOWN);
            else if (!using_mods)   do_cmd_drop();
            break;
        }
        case Qt::Key_E:
        {
            if (shift_key)          do_cmd_eat_food();
            else if (!using_mods)   do_cmd_all_objects(TAB_EQUIP);
            break;
        }
        case Qt::Key_F:
        {
            if (ctrl_key)           do_cmd_feeling();
            else if (shift_key)     do_cmd_refuel();
            else if (!using_mods)   do_cmd_fire();
            break;
        }
        case Qt::Key_G:
        {
            if (shift_key)          do_cmd_study(-1);
            else if (!using_mods)   do_cmd_pickup_from_pile(FALSE, TRUE);
            break;
        }
        case Qt::Key_H:
        {
            if (!using_mods)        do_cmd_fire_at_nearest();
            break;
        }
        case Qt::Key_I:
        {
            if (shift_key)          do_cmd_examine();
            else if (!using_mods)   do_cmd_all_objects(TAB_INVEN);
            break;
        }
        case Qt::Key_J:
        {
            if (!using_mods)        do_cmd_spike(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_K:
        {
            if (!using_mods)        do_cmd_destroy();
            break;
        }
        case Qt::Key_L:
        {
            if (shift_key)          ui_center(p_ptr->py, p_ptr->px);
            else if (!using_mods)   do_cmd_look();
            break;
        }
        case Qt::Key_M:
        {
            if (shift_key)          break; // TODO - MAP
            else if (!using_mods)   do_cmd_cast(-1);
            break;
        }
        case Qt::Key_N:
        case Qt::Key_0:
        case Qt::Key_Insert:
        {
            if (!using_mods)        do_cmd_repeat();
            break;
        }
        case Qt::Key_O:
        {
            if (shift_key)          do_cmd_make_trap(DIR_UNKNOWN);
            else if (!using_mods)   do_cmd_open(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_P:
        {
            if (ctrl_key)           display_message_log();
            else if (!using_mods)   do_cmd_cast(-1);
            break;
        }
        case Qt::Key_Q:
        {
            if (alt_key)            do_cmd_quest_desc();
            else if (shift_key)     do_cmd_suicide();
            else if (!using_mods)   do_cmd_quaff_potion();
            break;
        }
        case Qt::Key_R:
        {
            if (ctrl_key)           ui_redraw_all();
            else if (shift_key)     do_cmd_rest();
            else if (!using_mods)   do_cmd_read_scroll();
            break;
        }
        case Qt::Key_S:
        {
            if (shift_key)          do_cmd_toggle_search();
            else if (!using_mods)   do_cmd_search();
            break;
        }
        case Qt::Key_T:
        {
            if (shift_key)          do_cmd_tunnel(DIR_UNKNOWN);
            else if (!using_mods)   do_cmd_takeoff();
            break;
        }
        case Qt::Key_U:
        {
            if (!using_mods)        do_cmd_use_staff();
            break;
        }
        case Qt::Key_V:
        {
            if (!using_mods)        do_cmd_throw();
            break;
        }
        case Qt::Key_W:
        {
            if (!using_mods)   do_cmd_wield();
            break;
        }
        case Qt::Key_X:
        {
            if (!using_mods)        do_cmd_swap_weapon();
            break;
        }
        case Qt::Key_Y:
        {
            break;
        }
        case Qt::Key_Z:
        {
            if (!using_mods)        do_cmd_zap_rod();
            break;
        }
        case Qt::Key_Apostrophe:
        case Qt::Key_Asterisk:
        {
            target_set_closest(TARGET_KILL);
            break;
        }
        case Qt::Key_BraceLeft:
        {
            do_cmd_inscribe();
            break;
        }
        case Qt::Key_BraceRight:
        {
            do_cmd_uninscribe();
            break;
        }
        case Qt::Key_Greater:
        {
            do_cmd_go_down();
            break;
        }
        case Qt::Key_Less:
        {
            do_cmd_go_up();
            break;
        }
        case Qt::Key_Period:
        {
            do_cmd_run(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_Plus:
        {
            do_cmd_alter(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_Minus:
        {
            do_cmd_walk(DIR_UNKNOWN, TRUE);
            break;
        }
        case Qt::Key_Underscore:
        case Qt::Key_Comma:
        {
            do_cmd_hold();
            break;
        }
        case Qt::Key_Semicolon:
        {
            do_cmd_walk(DIR_UNKNOWN, FALSE);
            break;
        }
        case Qt::Key_Colon:
        {
            do_cmd_write_note();
            break;
        }
        case Qt::Key_ParenRight:
        {
            save_screenshot(FALSE);
            break;
        }
        case Qt::Key_ParenLeft:
        {
            save_screenshot(TRUE);
            break;
        }
        default:
        {
            break;
        }
    }
}

void commands_roguelike_keyset(int key_press, bool shift_key, bool alt_key, bool ctrl_key, bool meta_key)
{
    bool using_mods = FALSE;
    if (shift_key || alt_key || ctrl_key || meta_key) using_mods = TRUE;

    // Normal mode
    switch (key_press)
    {
        // ESCAPE
        case Qt::Key_Escape:
        {
            ui_center(p_ptr->py, p_ptr->px);
            break;
        }

        // Move down
        case Qt::Key_J:
        case Qt::Key_Down:
        {
            process_move_key(2, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }

        // Move up
        case Qt::Key_K:
        case Qt::Key_Up:
        {
            process_move_key(8, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }

        // Move left
        case Qt::Key_Left:
        case Qt::Key_H:
        {
            process_move_key(4, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move right
        case Qt::Key_L:
        case Qt::Key_Right:
        {
            process_move_key(6, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally left and up
        case Qt::Key_Y:
        case Qt::Key_Home:
        {
            process_move_key(7, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally right and up
        case Qt::Key_U:
        case Qt::Key_PageUp:
        {
            process_move_key(9, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally left and down
        case Qt::Key_B:
        case Qt::Key_End:
        {
            process_move_key(1, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        // Move diagonally right and down
        case Qt::Key_N:
        case Qt::Key_PageDown:
        {
            process_move_key(3, shift_key, alt_key, ctrl_key, meta_key);
            break;
        }
        case Qt::Key_A:
        {
            if (alt_key)           do_cmd_wizard_mode();
            else if (shift_key)    do_cmd_activate();
            else if (!using_mods)  do_cmd_zap_rod();
            break;
        }
        case Qt::Key_C:
        {
            if (shift_key && alt_key) save_character_file();
            else if (shift_key)     do_cmd_character_screen();
            else if (!using_mods)   do_cmd_close(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_D:
        {
            if (ctrl_key)           do_cmd_destroy();
            else if (shift_key)     do_cmd_disarm(DIR_UNKNOWN);
            else if (!using_mods)   do_cmd_drop();
            break;
        }
        case Qt::Key_E:
        {
            if (shift_key)          do_cmd_eat_food();
            else if (!using_mods)   do_cmd_all_objects(TAB_EQUIP);
            break;
        }
        case Qt::Key_F:
        {
            if (ctrl_key)           do_cmd_feeling();
            else if (shift_key)     do_cmd_refuel();
            else if (!using_mods)   do_cmd_bash(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_G:
        {
            if (ctrl_key)           do_cmd_pickup_from_pile(FALSE, TRUE);
            else if (shift_key)     do_cmd_study(-1);
            else if (!using_mods)   do_cmd_hold();
            break;
        }
        case Qt::Key_I:
        {
            if (shift_key)          do_cmd_examine();
            else if (!using_mods)   do_cmd_all_objects(TAB_INVEN);
            break;
        }

        case Qt::Key_M:
        {
            if (shift_key)          break; // TODO - MAP
            else if (!using_mods)   do_cmd_cast(-1);
            break;
        }

        case Qt::Key_O:
        {
            if (shift_key)          do_cmd_make_trap(DIR_UNKNOWN);
            else if (!using_mods)   do_cmd_open(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_P:
        {
            if (ctrl_key)           display_message_log();
            else if (shift_key)     do_cmd_browse(-1);
            else if (!using_mods)   do_cmd_cast(-1);
            break;
        }
        case Qt::Key_Q:
        {
            if (alt_key)           do_cmd_quest_desc();
            else if (shift_key)     do_cmd_suicide();
            else if (!using_mods)   do_cmd_quaff_potion();
            break;
        }
        case Qt::Key_R:
        {
            if (ctrl_key)           ui_redraw_all();
            else if (shift_key)     do_cmd_rest();
            else if (!using_mods)   do_cmd_read_scroll();
            break;
        }
        case Qt::Key_S:
        {
            if (shift_key)          do_cmd_spike(DIR_UNKNOWN);
            else if (!using_mods)   do_cmd_search();
            break;
        }
        case Qt::Key_T:
        {
            if (ctrl_key)           do_cmd_tunnel(DIR_UNKNOWN);
            else if (shift_key)     do_cmd_takeoff();
            else if (!using_mods)   do_cmd_fire_at_nearest();
            break;
        }
        case Qt::Key_V:
        {
            if (ctrl_key)           do_cmd_repeat();
            else if (!using_mods)   do_cmd_throw();
            break;
        }
        case Qt::Key_W:
        {
            if (shift_key)     ui_center(p_ptr->py, p_ptr->px);
            else if (!using_mods)   do_cmd_wield();
            break;
        }
        case Qt::Key_X:
        {
            if (shift_key)          do_cmd_swap_weapon();
            else if (!using_mods)   do_cmd_look();
            break;
        }
        case Qt::Key_Z:
        {
            if (shift_key)          do_cmd_use_staff();
            else if (!using_mods)   do_cmd_zap_rod();
            break;
        }
        case Qt::Key_NumberSign:
        {
            do_cmd_toggle_search();
            break;
        }
        case Qt::Key_Apostrophe:
        case Qt::Key_Asterisk:
        {
            target_set_closest(TARGET_KILL);
            break;
        }
        case Qt::Key_BraceLeft:
        {
            do_cmd_inscribe();
            break;
        }
        case Qt::Key_BraceRight:
        {
            do_cmd_uninscribe();
            break;
        }
        case Qt::Key_Greater:
        {
            do_cmd_go_down();
            break;
        }
        case Qt::Key_Less:
        {
            do_cmd_go_up();
            break;
        }
        case Qt::Key_BracketRight:
        {
            do_cmd_pickup_from_pile(FALSE, TRUE);
            break;
        }
        case Qt::Key_Comma:
        {
            do_cmd_run(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_Plus:
        {
            do_cmd_alter(DIR_UNKNOWN);
            break;
        }
        case Qt::Key_Minus:
        {
            do_cmd_walk(DIR_UNKNOWN, TRUE);
            break;
        }
        case Qt::Key_Underscore:
        case Qt::Key_Period:
        {
            do_cmd_hold();
            break;
        }
        case Qt::Key_Semicolon:
        {
            do_cmd_walk(DIR_UNKNOWN, FALSE);
            break;
        }
        case Qt::Key_Colon:
        {
            do_cmd_write_note();
            break;
        }
        case Qt::Key_ParenRight:
        {
            save_screenshot(FALSE);
            break;
        }
        case Qt::Key_ParenLeft:
        {
            save_screenshot(TRUE);
            break;
        }
        default:
        {
            break;
        }
    }
}
