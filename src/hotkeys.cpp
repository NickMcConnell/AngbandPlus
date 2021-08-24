/* File: hotkeys.cpp */

/*
 * Copyright (c) 2015 Jeff Greene, Diego Gonzalez
 *
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/npp.h>
#include <src/cmds.h>
#include <src/hotkeys.h>
#include <src/store.h>
#include <QPushButton>
#include <QSpinBox>
#include <src/player_command.h>

single_hotkey running_hotkey;
single_hotkey player_hotkeys[NUM_HOTKEYS];

#define STEP_MULT   1000

// The order of this list is assumed in qu_hotkey_toolbar.cpp
hotkey_list list_hotkeys[NUM_HOTKEYS] =
{
    {"F1", FALSE, Qt::Key_F1},
    {"F2", FALSE, Qt::Key_F2},
    {"F3", FALSE, Qt::Key_F3},
    {"F4", FALSE, Qt::Key_F4},
    {"F5", FALSE, Qt::Key_F5},
    {"F6", FALSE, Qt::Key_F6},
    {"F7", FALSE, Qt::Key_F7},
    {"F8", FALSE, Qt::Key_F8},
    {"F9", FALSE, Qt::Key_F9},
    {"F10", FALSE, Qt::Key_F10},
    {"F11", FALSE, Qt::Key_F11},
    {"F12", FALSE, Qt::Key_F12},
    {"Shift-F1", TRUE, Qt::Key_F1},
    {"Shift-F2", TRUE,  Qt::Key_F2},
    {"Shift-F3", TRUE,  Qt::Key_F3},
    {"Shift-F4", TRUE,  Qt::Key_F4},
    {"Shift-F5", TRUE,  Qt::Key_F5},
    {"Shift-F6", TRUE,  Qt::Key_F6},
    {"Shift-F7", TRUE,  Qt::Key_F7},
    {"Shift-F8", TRUE,  Qt::Key_F8},
    {"Shift-F9", TRUE,  Qt::Key_F9},
    {"Shift-F10", TRUE,  Qt::Key_F10},
    {"Shift-F11", TRUE,  Qt::Key_F11},
    {"Shift-F12", TRUE,  Qt::Key_F12},
};

hotkey_type hotkey_actions[] =
{
    //HK_TYPE_EMPTY
    {HK_NEEDS_NOTHING, "None",0},
    //HK_QUAFF_POTION
    {HK_NEEDS_OBJECT_KIND, "Quaff a Potion", TV_POTION},
    //HK_READ_SCROLL
    {HK_NEEDS_OBJECT_KIND, "Read a Scroll",TV_SCROLL},
    //HK_AIM_WAND
    {HK_NEEDS_OBJECT_KIND, "Aim a Wand",TV_WAND},
    //HK_USE_STAFF
    {HK_NEEDS_OBJECT_KIND, "Use a Staff",TV_STAFF},
    //HK_ZAP_ROD
    {HK_NEEDS_OBJECT_KIND, "Zap a Rod",TV_ROD},
    //HK_EAT_FOOD
    {HK_NEEDS_OBJECT_KIND, "Eat Food", TV_FOOD},
    //HK_CAST_SPELL
    {HK_NEEDS_SPELL, "Cast Spell", 0},
    // HK_ACTIVATE
    {HK_NEEDS_ACIVATION, "Activate", 0},
    // HK_FIRE_AMMO
    {HK_NEEDS_SPECIFIC_OBJECT, "Fire", 0},
    // HK_THROW
    {HK_NEEDS_SPECIFIC_OBJECT, "Throw", 0},
    // HK_REST
    {HK_NEEDS_REST, "Rest", 0},
    //HK_TYPE_MOVE
    {HK_NEEDS_DIRECTION, "Walk",0},
    //HK_TYPE_JUMP
    {HK_NEEDS_DIRECTION, "Jump",0},
    //HK_TYPE_RUN
    {HK_NEEDS_DIRECTION, "Run",0},
    //HK_TYPE_ALTER
    {HK_NEEDS_DIRECTION, "Alter",0},
    //HK_TYPE_DISARM
    {HK_NEEDS_DIRECTION, "Disarm",0},
    //HK_TYPE_CLOSE
    {HK_NEEDS_DIRECTION, "Close",0},
    //HK_TYPE_OPEN
    {HK_NEEDS_DIRECTION, "Open",0},
    //HK_TYPE_BASH
    {HK_NEEDS_DIRECTION, "Bash",0},
    //HK_TYPE_TUNNEL
    {HK_NEEDS_DIRECTION, "Tunnel",0},
    //HK_TYPE_MAKE_TRAP
    {HK_NEEDS_DIRECTION, "Make Trap",0},
    //HK_TYPE_SPIKE
    {HK_NEEDS_DIRECTION, "Spike",0},
    //HK_TYPE_HOLD
    {HK_NEEDS_NOTHING, "Hold",0},

};

void single_hotkey::clear_hotkey_steps(void)
{
    hotkey_steps.clear();
    hotkey_step dummy_step;
    dummy_step.step_commmand = HK_TYPE_EMPTY;
    dummy_step.step_args.wipe();
    hotkey_steps.append(dummy_step);
}

void single_hotkey::copy_hotkey(single_hotkey *other_hotkey)
{
    hotkey_name = other_hotkey->hotkey_name;
    hotkey_button = other_hotkey->hotkey_button;
    hotkey_button_name = other_hotkey->hotkey_button_name;

    // Copy each step
    hotkey_steps.clear();
    for (int i = 0; i < other_hotkey->hotkey_steps.size(); i++)
    {
       hotkey_steps.append(other_hotkey->hotkey_steps[i]);
    }
}

bool single_hotkey::has_commands(void)
{
    if (hotkey_steps[0].step_commmand == HK_TYPE_EMPTY) return (FALSE);
    if (!hotkey_steps.size())
    {
        return (FALSE);
    }

    return (TRUE);
}

void clear_all_hotkeys()
{
    for (int i = 0; i < NUM_HOTKEYS; i++)
    {
        single_hotkey *plyr_hk_ptr = &player_hotkeys[i];
        hotkey_list *hk_list_ptr = &list_hotkeys[i];
        plyr_hk_ptr->hotkey_name = hk_list_ptr->hotkey_list_name;
        plyr_hk_ptr->hotkey_button_name = hk_list_ptr->hotkey_list_name;
        plyr_hk_ptr->hotkey_button = hk_list_ptr->listed_hotkey;
        plyr_hk_ptr->clear_hotkey_steps();
    }
}

// Check if the command_step matches the selected object;
static bool activation_matches_selection(cmd_arg *this_arg, object_type *o_ptr)
{
    if (!o_ptr->k_idx) return (FALSE);
    // Match object kind
    if (o_ptr->k_idx != this_arg->k_idx) return (FALSE);
    //Match ego num
    if (o_ptr->ego_num != this_arg->item) return (FALSE);
    // Match artifact num
    if (o_ptr->art_num != this_arg->number) return (FALSE);

    return (TRUE);
}

// Find the first available match to the chosen activation
static object_type *find_object_activation(cmd_arg this_arg)
{
    // Check what the player is holding
    for (int i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];
        if (activation_matches_selection(&this_arg, o_ptr)) return (o_ptr);
    }
    store_type *st_ptr = &store[STORE_HOME];

    // Check the home
    for (int i = 0; i < st_ptr->stock_num; i++)
    {
        object_type *o_ptr = &st_ptr->stock[i];
        if (activation_matches_selection(&this_arg, o_ptr)) return (o_ptr);
    }

    // Whoops!  An extra item not found in the player's inventory = handled later
    object_type object_type_body;
    object_type *o_ptr = &object_type_body;
    o_ptr->object_wipe();
    return (o_ptr);
}



void HotKeyDialog::active_hotkey_name_changed(QString new_name)
{
    dialog_hotkey.hotkey_name = new_name;
}

void HotKeyDialog::save_current_hotkey()
{
    player_hotkeys[current_hotkey_int].copy_hotkey(&dialog_hotkey);
}

void HotKeyDialog::load_new_hotkey(int this_choice)
{
    current_hotkey_int = this_choice;
    dialog_hotkey.copy_hotkey(&player_hotkeys[this_choice]);
}

void HotKeyDialog::active_hotkey_changed(int new_hotkey)
{
    save_current_hotkey();
    load_new_hotkey(new_hotkey);
    current_name->setText(dialog_hotkey.hotkey_name);
    display_hotkey_steps();
}

// Returns the step of the current QObject.
int HotKeyDialog::get_current_step(QString item_id)
{
    item_id.remove("Step_Command_", Qt::CaseInsensitive);

    // Remove anything before the phrase below
    QString this_index = "_step_";
    while (item_id.contains(this_index, Qt::CaseInsensitive))
    {
        int location = item_id.indexOf(this_index, Qt::CaseInsensitive) + this_index.length();
        item_id.remove(0,location);

    }
    return(item_id.toInt());
}

void HotKeyDialog::add_hotkeys_header()
{
    QPointer<QHBoxLayout> hlay_header = new QHBoxLayout;
    main_layout->addLayout(hlay_header);
    current_hotkey_name = new QComboBox;
    for (int i = 0; i < NUM_HOTKEYS; i++)
    {
        QString hotkey_name = list_hotkeys[i].hotkey_list_name;
        current_hotkey_name->addItem(QString("%1") .arg(i));
        current_hotkey_name->setItemText(i, hotkey_name);
    }
    hlay_header->addWidget(current_hotkey_name);
    connect(current_hotkey_name, SIGNAL(currentIndexChanged(int)), this, SLOT(active_hotkey_changed(int)));
    current_name = new QLineEdit;
    current_name->setText(dialog_hotkey.hotkey_name);
    connect(current_name, SIGNAL(textChanged(QString)), this, SLOT(active_hotkey_name_changed(QString)));
    hlay_header->addWidget(current_name);
}

void HotKeyDialog::active_hotkey_command_changed(int this_choice)
{
    QString sender_id = QObject::sender()->objectName();
    int this_step = get_current_step(sender_id);
    dialog_hotkey.hotkey_steps[this_step].step_commmand = this_choice;
    dialog_hotkey.hotkey_steps[this_step].step_args.wipe();
    dialog_hotkey.hotkey_steps[this_step].step_object.object_wipe();
    QString item_id = (QString("hlay_step_%1") .arg(this_step));
    QList<QHBoxLayout *> hlay_list = this->findChildren<QHBoxLayout *>();
    for (int x = 0; x < hlay_list.size(); x++)
    {
        QHBoxLayout *this_hlay = hlay_list.at(x);

        QString this_name = this_hlay->objectName();

        if (this_name.contains(item_id))
        {
            create_one_hotkey_step(this_hlay, this_step);
        }
    }
}



// Find the new object kind based on the combo box
void HotKeyDialog::active_spell_changed(int choice)
{
    QString sender_id = QObject::sender()->objectName();
    int this_step = get_current_step(sender_id);

    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[this_step];

    bool old_spell_dir = spell_needs_aim(cp_ptr->spell_book, hks_ptr->step_args.number);

    // Remember the object choice
    hks_ptr->step_args.number = spell_list.at(choice);

    bool new_spell_dir = spell_needs_aim(cp_ptr->spell_book, hks_ptr->step_args.number);

    // Remove the target box if no longer needed
    if (!new_spell_dir && old_spell_dir)
    {
        delete_targeting_choices(this_step);
        hks_ptr->step_args.direction = DIR_UNKNOWN;
    }

    // Make a new target box
    if (new_spell_dir && !old_spell_dir)
    {
        hks_ptr->step_args.direction = DIR_UNKNOWN;

        // Find the hbox layout
        QString item_id = (QString("hlay_step_%1") .arg(this_step));
        QList<QHBoxLayout *> hlay_list = this->findChildren<QHBoxLayout *>();
        for (int x = 0; x < hlay_list.size(); x++)
        {
            QHBoxLayout *this_hlay = hlay_list.at(x);

            QString this_name = this_hlay->objectName();

            if (this_name.contains(item_id))
            {
                create_targeting_choices(this_hlay, this_step);

                break;
            }
        }
    }
}

// Manuallly turning the buttons true or false is necessary
// because the button group covers more than one step
void HotKeyDialog::hotkey_step_target_changed(int new_target)
{
    int step = new_target / STEP_MULT;
    int new_choice = new_target % STEP_MULT;

    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];

    hks_ptr->step_args.direction = new_choice;

    QString button_name = QString("Target Closest");

    if (new_choice == DIR_TARGET) button_name = QString("Use Current Target");
    else if (new_choice == DIR_UNKNOWN) button_name = QString("Specify During Use");

    // Manually turn the buttons true or false.
    QList<QRadioButton *> radio_list = this->findChildren<QRadioButton *>();

    for (int x = 0; x < radio_list.size(); x++)
    {
        QRadioButton *this_radio = radio_list.at(x);

        // Not the right step.
        QString item_id = this_radio->objectName();
        if (item_id.contains("targeting_step_", Qt::CaseInsensitive))
        {
            item_id.remove("targeting_step_", Qt::CaseInsensitive);
            int which_step = item_id.toInt();
            if (step != which_step) continue;

            // Turn on or off
            QString this_name = this_radio->text();
            if (this_name.contains(button_name)) this_radio->setChecked(TRUE);
            else this_radio->setChecked(FALSE);
        }
    }
}

void HotKeyDialog::delete_targeting_choices(int this_step)
{
    // First, remove the radio buttons from the group
    QList<QRadioButton *> radio_list = this->findChildren<QRadioButton *>();
    QString radio_id = (QString("targeting_step_%1") .arg(this_step));
    for (int x = 0; x < radio_list.size(); x++)
    {
        QRadioButton *this_radio = radio_list.at(x);

        QString this_name = this_radio->objectName();

        if (this_name.contains(radio_id))
        {
            // Remove it from the target group
            group_target_choices->removeButton(this_radio);
            break;
        }
    }

    // Now remove the vbox layout
    QString item_id = (QString("vlay_targeting_step_%1") .arg(this_step));
    QList<QVBoxLayout *> vlay_list = this->findChildren<QVBoxLayout *>();
    for (int x = 0; x < vlay_list.size(); x++)
    {
        QVBoxLayout *this_vlay = vlay_list.at(x);

        QString this_name = this_vlay->objectName();

        if (this_name.contains(item_id))
        {
            clear_layout(this_vlay);
            this_vlay->deleteLater();
            break;
        }
    }
}

void HotKeyDialog::create_targeting_choices(QHBoxLayout *this_layout, int step)
{
    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];

    QPointer<QVBoxLayout> vlay_targets = new QVBoxLayout();
    vlay_targets->setObjectName(QString("vlay_targeting_step_%1") .arg(step));
    this_layout->addLayout(vlay_targets);

    QPointer<QLabel> header_targets = new QLabel("<b>Select Targeting Method</b>");
    vlay_targets->addWidget(header_targets);

    QPointer<QRadioButton> radio_closest = new QRadioButton("Target Closest");
    radio_closest->setObjectName(QString("targeting_step_%1") .arg(step));
    radio_closest->setChecked(FALSE);
    vlay_targets->addWidget(radio_closest);
    group_target_choices->addButton(radio_closest, (step * STEP_MULT + DIR_CLOSEST));

    QPointer<QRadioButton> radio_current = new QRadioButton("Use Current Target");
    radio_current->setObjectName(QString("targeting_step_%1") .arg(step));
    radio_current->setChecked(FALSE);
    vlay_targets->addWidget(radio_current);
    group_target_choices->addButton(radio_current, (step * STEP_MULT + DIR_TARGET));

    QPointer<QRadioButton> radio_specify_use = new QRadioButton("Specify During Use");
    radio_specify_use->setObjectName(QString("targeting_step_%1") .arg(step));
    radio_specify_use->setChecked(FALSE);
    vlay_targets->addWidget(radio_specify_use);
    group_target_choices->addButton(radio_specify_use, (step * STEP_MULT + DIR_UNKNOWN));

    if (hks_ptr->step_args.direction == DIR_CLOSEST) radio_closest->setChecked(TRUE);
    else if (hks_ptr->step_args.direction == DIR_TARGET) radio_current->setChecked(TRUE);
    else radio_specify_use->setChecked(TRUE);

    vlay_targets->addStretch(1);
}

void HotKeyDialog::create_spell_choice_dropbox(QHBoxLayout *this_layout, int step)
{
    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];

    QPointer<QVBoxLayout> vlay_spellbox = new QVBoxLayout;
    this_layout->addLayout(vlay_spellbox);

    //Create the combobox
    QPointer<QComboBox> combobox_spell_choice = new QComboBox;
    int max_spellbooks = (game_mode == GAME_NPPANGBAND ? BOOKS_PER_REALM_ANGBAND : BOOKS_PER_REALM_MORIA);

    spell_list.clear();
    int current_index = -1;

    if (cp_ptr->spell_book) for (int i = 0; i < max_spellbooks; i++)
    {
        for (int j = 0; j < SPELLS_PER_BOOK; j++)
        {
            int spell = get_spell_from_list(i, j);

            if (spell == -1) continue;

            if (!spell_okay(spell, TRUE)) continue;

            combobox_spell_choice->addItem(QString("%1") .arg(spell));
            combobox_spell_choice->setItemText(spell_list.size(), cast_spell(MODE_SPELL_NAME, cp_ptr->spell_book, spell, DIR_UNKNOWN));

            // Try to find the current index.
            if (spell == hks_ptr->step_args.number) current_index = spell_list.size();

            //Assume the first possible spell
            if (!hks_ptr->step_args.number && !spell_list.size()) hks_ptr->step_args.number = spell;

            spell_list.append(spell);
        }
    }

    QString noun = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);
    QString verb = cast_spell(MODE_SPELL_VERB, cp_ptr->spell_book, 1, 0);
    noun.append("s");
    QString label_text = (QString("<b>Select a %1 to %2</b>") .arg(noun) .arg(verb));

    // Add a header
    QPointer<QLabel> header_dir = new QLabel(label_text);
    vlay_spellbox->addWidget(header_dir);
    if (!spell_list.size())
    {
        if (!cp_ptr->spell_book)  header_dir->setText("<b>You cannot cast spells</b>");
        else header_dir->setText(QString("<b>You do not know any %1</b>") .arg(noun));
        delete combobox_spell_choice;
        vlay_spellbox->addStretch(1);
        return;
    }

    combobox_spell_choice->setCurrentIndex(current_index);
    hks_ptr->step_args.verify = TRUE;

    connect(combobox_spell_choice, SIGNAL(currentIndexChanged(int)), this, SLOT(active_spell_changed(int)));
    combobox_spell_choice->setObjectName(QString("Step_Command_%1") .arg(step));
    vlay_spellbox->addWidget(combobox_spell_choice);

    vlay_spellbox->addStretch(1);
}

// Helper function to determine if the current object kind
// should be included in a ComboBox object kinds
bool HotKeyDialog::accept_object_kind(int k_idx, int tval)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Skip "empty" items */
    if (k_ptr->k_name.isEmpty()) return (FALSE);
    if (k_ptr->tval != tval) return (FALSE);
    if (!k_ptr->aware || !k_ptr->everseen) return (FALSE);

    return (TRUE);
}

// Create a dummy of the currently selected object
object_type HotKeyDialog::create_selected_object(cmd_arg args)
{
    object_type object_type_body;
    object_type *o_ptr = &object_type_body;
    o_ptr->object_wipe();

    if (args.number) make_fake_artifact(o_ptr, args.number);
    else make_object_fake(o_ptr, args.k_idx, args.item, FALSE);

    return (object_type_body);
}

// Helper function to find the current k_idx
// selection in a combobox of object kinds
int HotKeyDialog::find_selected_k_idx(int choice, int step)
{
    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];
    int this_tval = hotkey_actions[hks_ptr->step_commmand].tval;

    int current_index = 0;

    for (int i = z_info->k_max-1; i > 0; i--)
    {
        if (!accept_object_kind(i, this_tval)) continue;

        if (choice == current_index) return (i);

        current_index++;
    }

    // Whoops!  Shouldn't happen.
    return (1);
}

// Find the new object kind based on the combo box
void HotKeyDialog::active_k_idx_changed(int choice)
{
    QString sender_id = QObject::sender()->objectName();
    int this_step = get_current_step(sender_id);

    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[this_step];

    bool old_obj_dir = obj_needs_aim(hks_ptr->step_args.k_idx);

    // Remember the object choice
    hks_ptr->step_args.k_idx = find_selected_k_idx(choice, this_step);

    bool new_obj_dir = obj_needs_aim(hks_ptr->step_args.k_idx);

    // Remove the direction box
    if (!new_obj_dir && old_obj_dir)
    {
        delete_targeting_choices(this_step);
        hks_ptr->step_args.direction = DIR_UNKNOWN;
    }

    // Add the direction box if needed
    if (new_obj_dir && !old_obj_dir)
    {
        hks_ptr->step_args.direction = DIR_UNKNOWN;

        // Find the hbox layout
        QString item_id = (QString("hlay_step_%1") .arg(this_step));
        QList<QHBoxLayout *> hlay_list = this->findChildren<QHBoxLayout *>();
        for (int x = 0; x < hlay_list.size(); x++)
        {
            QHBoxLayout *this_hlay = hlay_list.at(x);

            QString this_name = this_hlay->objectName();

            if (this_name.contains(item_id))
            {
                create_targeting_choices(this_hlay, this_step);
                break;
            }
        }
    }
}

static QString get_object_label_text(int tval)
{
    if (tval == TV_POTION) return (QString("<b>Select Potion to Quaff:</b>"));
    if (tval == TV_SCROLL) return (QString("<b>Select Scroll to Read:</b>"));
    if (tval == TV_WAND) return (QString("<b>Select Wand to Aim:</b>"));
    if (tval == TV_STAFF) return (QString("<b>Select Staff to Use:</b>"));
    if (tval == TV_ROD) return (QString("<b>Select Rod to Zap:</b>"));
    if (tval == TV_FOOD) return (QString("<b>Select Food to Eat:</b>"));

    // Should never happen
    return QString("<b>Select Object To Use:</b>");
}

void HotKeyDialog::create_object_kind_dropbox(QHBoxLayout *this_layout, int this_step)
{
    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[this_step];

    int this_tval = hotkey_actions[hks_ptr->step_commmand].tval;

    QPointer<QVBoxLayout> vlay_obj_kind = new QVBoxLayout;
    vlay_obj_kind->setObjectName(QString("vlay_obj_kind_step_%1") .arg(this_step));
    this_layout->addLayout(vlay_obj_kind);

    //Create the combobox
    QPointer<QComboBox> this_combobox = new QComboBox;
    this_combobox->setObjectName(QString("obj_kind_combo_step_%1") .arg(this_step));
    int current_index = 0;
    int count = 0;

    for (int i = z_info->k_max-1; i > 0; i--)
    {

        if (!accept_object_kind(i, this_tval)) continue;

        this_combobox->addItem(QString("%1") .arg(STEP_MULT * this_step + i));
        this_combobox->setItemText(count, capitalize_first(strip_name(i)));

        // Try to find the current index.
        if (i == hks_ptr->step_args.k_idx) current_index = count;

        count++;
    }

    // Add a header
    QPointer<QLabel> header_dir = new QLabel(get_object_label_text(this_tval));
    vlay_obj_kind->addWidget(header_dir);
    if (!count)
    {
        header_dir->setText("<b>No Known Objects Of This Type</b>");
        delete this_combobox;
        vlay_obj_kind->addStretch(1);
        return;
    }

    hks_ptr->step_args.k_idx = find_selected_k_idx(current_index, this_step);

    this_combobox->setCurrentIndex(current_index);
    hks_ptr->step_args.verify = TRUE;

    connect(this_combobox, SIGNAL(currentIndexChanged(int)), this, SLOT(active_k_idx_changed(int)));
    vlay_obj_kind->addWidget(this_combobox);
    vlay_obj_kind->addStretch(1);
}

void HotKeyDialog::delete_specific_object_choices(int this_step)
{
    // First, remove the radio buttons from the group
    QList<QRadioButton *> radio_list = this->findChildren<QRadioButton *>();
    QString radio_id = (QString("specify_obj_step_%1") .arg(this_step));
    for (int x = 0; x < radio_list.size(); x++)
    {
        QRadioButton *this_radio = radio_list.at(x);

        QString this_name = this_radio->objectName();

        if (this_name.contains(radio_id))
        {
            // Remove it from the target group
            group_target_choices->removeButton(this_radio);
            break;
        }
    }

    // Now remove the vbox layout
    QString item_id = (QString("vlay_specific_obj_step_%1") .arg(this_step));
    QList<QVBoxLayout *> vlay_list = this->findChildren<QVBoxLayout *>();
    for (int x = 0; x < vlay_list.size(); x++)
    {
        QVBoxLayout *this_vlay = vlay_list.at(x);

        QString this_name = this_vlay->objectName();

        if (this_name.contains(item_id))
        {
            clear_layout(this_vlay);
            this_vlay->deleteLater();
            break;
        }
    }
}

// Manuallly turning the buttons true or false is necessary
// because the button group covers more than one step
void HotKeyDialog::hotkey_step_obj_selection_changed(int new_target)
{
    int step = new_target / STEP_MULT;
    int new_choice = new_target % STEP_MULT;

    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];

    hks_ptr->step_args.choice = new_choice;

    QString button_name = QString("Specify During Use");

    if (new_choice == OS_FIND_SPECIFIC_INSCRIPTION) button_name = QString("Use Object Inscribed With:");

    // Manually turn the buttons true or false.
    QList<QRadioButton *> radio_list = this->findChildren<QRadioButton *>();

    for (int x = 0; x < radio_list.size(); x++)
    {
        QRadioButton *this_radio = radio_list.at(x);

        // Not the right step.
        QString item_id = this_radio->objectName();
        if (item_id.contains("specify_obj_step_"))
        {
            item_id.remove("specify_obj_step_", Qt::CaseInsensitive);
            int which_step = item_id.toInt();
            if (step != which_step) continue;

            // Turn on or off
            QString this_name = this_radio->text();
            if (this_name.contains(button_name)) this_radio->setChecked(TRUE);
            else this_radio->setChecked(FALSE);
        }
    }

    // Now find the QLineEdit box and enable it or disable it.
    QList<QLineEdit *> line_edit_list = this->findChildren<QLineEdit *>();
    for (int x = 0; x < line_edit_list.size(); x++)
    {
        QLineEdit *this_line_edit = line_edit_list.at(x);

        // Not the right step.
        QString item_id = this_line_edit->objectName();
        if (item_id.contains("specify_obj_step_", Qt::CaseInsensitive))
        {
            item_id.remove("specify_obj_step_", Qt::CaseInsensitive);
            int which_step = item_id.toInt();
            if (step != which_step) continue;

            if (new_choice == OS_SELECT_DURING_USE)
            {
                this_line_edit->setText("");
                this_line_edit->setEnabled(FALSE);
            }
            else  // (new_choice == OS_FIND_SPECIFIC_INSTRIPTION)
            {
                this_line_edit->setEnabled(TRUE);
            }
        }
    }
}

void HotKeyDialog::hotkey_step_obj_select_name_changed(QString inscription)
{
    QString sender_id = QObject::sender()->objectName();
    int step = get_current_step(sender_id);

    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];

    hks_ptr->step_args.string2 = inscription;
}

void HotKeyDialog::create_specific_object_choices(QHBoxLayout *this_layout, int this_step)
{
    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[this_step];

    QPointer<QVBoxLayout> vlay_obj_choose = new QVBoxLayout;
    vlay_obj_choose->setObjectName(QString("vlay_specific_obj_step_%1") .arg(this_step));
    this_layout->addLayout(vlay_obj_choose);

    // Add a header
    QPointer<QLabel> header_dir = new QLabel("<b>Select Object Choice Method:</b>");
    vlay_obj_choose->addWidget(header_dir);

    // Add the buttons and string box
    QPointer<QRadioButton> radio_specify_use = new QRadioButton("Specify During Use");
    radio_specify_use->setObjectName(QString("specify_obj_step_%1") .arg(this_step));
    radio_specify_use->setChecked(FALSE);
    vlay_obj_choose->addWidget(radio_specify_use);
    group_specific_object->addButton(radio_specify_use, (this_step * STEP_MULT + OS_SELECT_DURING_USE));

    QPointer<QRadioButton> radio_specify_inscription = new QRadioButton("Use Object Inscribed With:");
    radio_specify_inscription->setObjectName(QString("specify_obj_step_%1") .arg(this_step));
    radio_specify_inscription->setToolTip("During hotkey execution, the first found object with this inscription will be used");
    radio_specify_inscription->setChecked(FALSE);
    vlay_obj_choose->addWidget(radio_specify_inscription);
    group_specific_object->addButton(radio_specify_inscription, (this_step * STEP_MULT + OS_FIND_SPECIFIC_INSCRIPTION));

    QPointer<QLineEdit> search_inscription = new QLineEdit();
    search_inscription->setText(hks_ptr->step_args.string2);
    search_inscription->setObjectName(QString("specify_obj_step_%1") .arg(this_step));
    search_inscription->setPlaceholderText("Enter Inscription Here");
    connect(search_inscription, SIGNAL(textChanged(QString)), this, SLOT(hotkey_step_obj_select_name_changed(QString)));
    vlay_obj_choose->addWidget(search_inscription);

    // There is already a specified string
    if (hks_ptr->step_args.string2.length())
    {
        radio_specify_inscription->setChecked(TRUE);
        search_inscription->setEnabled(TRUE);
    }
    else
    {
        radio_specify_use->setChecked(TRUE);
        search_inscription->setEnabled(FALSE);
    }

    vlay_obj_choose->addStretch(1);
}

void HotKeyDialog::delete_resting_choices(int this_step)
{
    // First, remove the radio buttons from the group
    QList<QRadioButton *> radio_list = this->findChildren<QRadioButton *>();
    QString radio_id = (QString("specify_rest_step_%1") .arg(this_step));
    for (int x = 0; x < radio_list.size(); x++)
    {
        QRadioButton *this_radio = radio_list.at(x);

        QString this_name = this_radio->objectName();

        if (this_name.contains(radio_id))
        {
            // Remove it from the target group
            group_resting_choices->removeButton(this_radio);
            break;
        }
    }

    // Now remove the vbox layout
    QString item_id = (QString("vlay_resting_step_%1") .arg(this_step));
    QList<QVBoxLayout *> vlay_list = this->findChildren<QVBoxLayout *>();
    for (int x = 0; x < vlay_list.size(); x++)
    {
        QVBoxLayout *this_vlay = vlay_list.at(x);

        QString this_name = this_vlay->objectName();

        if (this_name.contains(item_id))
        {
            clear_layout(this_vlay);
            this_vlay->deleteLater();
            break;
        }
    }
}

// Manuallly turning the buttons true or false is necessary
// because the button group covers more than one step
void HotKeyDialog::hotkey_step_rest_choice_changed(int new_selection)
{
    int step = new_selection/ STEP_MULT;
    int new_choice = new_selection % STEP_MULT;

    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];

    hks_ptr->step_args.choice = new_choice;

    QString button_name = QString("Rest Complete");

    if (new_choice == REST_BOTH_SP_HP) button_name = QString("Rest Both HP and SP");
    else if (new_choice == REST_BOTH_SP_HP) button_name = QString("Rest Both HP and SP");
    else if (new_choice == REST_HP) button_name = QString("Rest Hit Points");
    else if (new_choice == REST_SP) button_name = QString("Rest Spell Points");
    else if (new_choice == REST_TURNCOUNT) button_name = QString("Rest Turncount:");

    // Manually turn the buttons true or false.
    QList<QRadioButton *> radio_list = this->findChildren<QRadioButton *>();

    for (int x = 0; x < radio_list.size(); x++)
    {
        QRadioButton *this_radio = radio_list.at(x);

        // Not the right step.
        QString item_id = this_radio->objectName();
        if (item_id.contains("specify_rest_step_"))
        {
            item_id.remove("specify_rest_step_", Qt::CaseInsensitive);
            int which_step = item_id.toInt();
            if (step != which_step) continue;

            // Turn on or off
            QString this_name = this_radio->text();
            if (this_name.contains(button_name)) this_radio->setChecked(TRUE);
            else this_radio->setChecked(FALSE);
        }
    }

    // Now find the QLineEdit box and enable it or disable it.
    QList<QSpinBox *> spin_box_list = this->findChildren<QSpinBox *>();
    for (int x = 0; x < spin_box_list.size(); x++)
    {
        QSpinBox *this_spin_box = spin_box_list.at(x);

        // Not the right step.
        QString item_id = this_spin_box->objectName();
        if (item_id.contains("specify_rest_step_", Qt::CaseInsensitive))
        {
            item_id.remove("specify_rest_step_", Qt::CaseInsensitive);
            int which_step = item_id.toInt();
            if (step != which_step) continue;

            if (new_choice == REST_TURNCOUNT)
            {
                this_spin_box->setValue(hks_ptr->step_args.number);
                this_spin_box->setEnabled(TRUE);
            }
            else
            {
                this_spin_box->setValue(0);
                this_spin_box->setEnabled(FALSE);
            }
        }
    }
}

void HotKeyDialog::hotkey_rest_turncount_changed(int new_turncount)
{
    QString sender_id = QObject::sender()->objectName();
    int step = get_current_step(sender_id);

    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];

    hks_ptr->step_args.number = new_turncount;
}

void HotKeyDialog::create_resting_choices(QHBoxLayout *this_layout, int this_step)
{
    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[this_step];

    QPointer<QVBoxLayout> vlay_resting_choose = new QVBoxLayout;
    vlay_resting_choose->setObjectName(QString("vlay_resting_step_%1") .arg(this_step));
    this_layout->addLayout(vlay_resting_choose);

    // Add a header
    QPointer<QLabel> header_dir = new QLabel("<b>Select Rest Duration:</b>");
    vlay_resting_choose->addWidget(header_dir);

    // Add the buttons and string box
    QPointer<QRadioButton> radio_rest_complete = new QRadioButton("Rest Complete");
    radio_rest_complete->setObjectName(QString("specify_rest_step_%1") .arg(this_step));
    radio_rest_complete->setToolTip("Rest until the player is fully recovered.");
    radio_rest_complete->setChecked(FALSE);
    vlay_resting_choose->addWidget(radio_rest_complete);
    group_resting_choices->addButton(radio_rest_complete, (this_step * STEP_MULT + REST_COMPLETE));

    QPointer<QRadioButton> radio_rest_hp_sp = new QRadioButton("Rest Both HP and SP");
    radio_rest_hp_sp->setObjectName(QString("specify_rest_step_%1") .arg(this_step));
    radio_rest_hp_sp->setToolTip("Rest until all mana and hit points are fully recovered.");
    radio_rest_hp_sp->setChecked(FALSE);
    vlay_resting_choose->addWidget(radio_rest_hp_sp);
    group_resting_choices->addButton(radio_rest_hp_sp, (this_step * STEP_MULT + REST_BOTH_SP_HP));

    QPointer<QRadioButton> radio_rest_hitpoints = new QRadioButton("Rest Hit Points");
    radio_rest_hitpoints->setObjectName(QString("specify_rest_step_%1") .arg(this_step));
    radio_rest_hitpoints->setToolTip("Rest until all hit points are fully recovered.");
    radio_rest_hitpoints->setChecked(FALSE);
    vlay_resting_choose->addWidget(radio_rest_hitpoints);
    group_resting_choices->addButton(radio_rest_hitpoints, (this_step * STEP_MULT + REST_HP));

    QPointer<QRadioButton> radio_rest_spellpoints = new QRadioButton("Rest Spell Points");
    radio_rest_spellpoints->setObjectName(QString("specify_rest_step_%1") .arg(this_step));
    radio_rest_spellpoints->setToolTip("Rest until all mana is fully recovered.");
    radio_rest_spellpoints->setChecked(FALSE);
    vlay_resting_choose->addWidget(radio_rest_spellpoints);
    group_resting_choices->addButton(radio_rest_spellpoints, (this_step * STEP_MULT + REST_SP));

    QPointer<QRadioButton> radio_rest_turncount = new QRadioButton("Rest Turncount:");
    radio_rest_turncount->setObjectName(QString("specify_rest_step_%1") .arg(this_step));
    radio_rest_turncount->setToolTip("Rest a specified number of turns.");
    radio_rest_turncount->setChecked(FALSE);
    vlay_resting_choose->addWidget(radio_rest_turncount);
    group_resting_choices->addButton(radio_rest_turncount, (this_step * STEP_MULT + REST_TURNCOUNT));

    QPointer<QSpinBox> spin_rest_turncount = new QSpinBox();
    spin_rest_turncount->setRange(0, 9999);
    spin_rest_turncount->setValue(hks_ptr->step_args.number);
    spin_rest_turncount->setObjectName(QString("specify_rest_step_%1") .arg(this_step));
    connect(spin_rest_turncount, SIGNAL(valueChanged(int)), this, SLOT(hotkey_rest_turncount_changed(int)));
    vlay_resting_choose->addWidget(spin_rest_turncount);

    // Start with complete rest, if one is needed.
    if (!hks_ptr->step_args.choice) hks_ptr->step_args.choice = REST_COMPLETE;

    // Set the proper radio button
    if (hks_ptr->step_args.choice == REST_TURNCOUNT)
    {
        radio_rest_turncount->setChecked(TRUE);
        spin_rest_turncount->setEnabled(TRUE);
    }
    else
    {
        if (hks_ptr->step_args.choice == REST_COMPLETE) radio_rest_complete->setChecked(TRUE);
        else if (hks_ptr->step_args.choice == REST_BOTH_SP_HP) radio_rest_hp_sp->setChecked(TRUE);
        else if (hks_ptr->step_args.choice == REST_HP) radio_rest_hitpoints->setChecked(TRUE);
        else if (hks_ptr->step_args.choice == REST_SP) radio_rest_spellpoints->setChecked(TRUE);
        spin_rest_turncount->setEnabled(FALSE);
    }

    vlay_resting_choose->addStretch(1);
}

// Helper function to see if an activatable object
// should be added to the combo box
bool HotKeyDialog::accept_activation_object(object_type *o_ptr)
{
    /* Skip "empty" items */
    if (!o_ptr->k_idx) return (FALSE);
    // Must be activitable and known
    if (!obj_is_activatable(o_ptr)) return (FALSE);
    if (!o_ptr->is_known()) return (FALSE);
    return (TRUE);
}

// Helper function to find the current activation
// selection in a combobox of possible activations
object_type HotKeyDialog::find_selected_activation(int choice, int step)
{
    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];

    int i;
    int current_index = 0;

    // Check what the player is holding
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];
        if (!accept_activation_object(o_ptr)) continue;

        if (choice == current_index) return (inventory[i]);

        current_index++;
    }
    store_type *st_ptr = &store[STORE_HOME];

    // Check the home
    for (i = 0; i < st_ptr->stock_num; i++)
    {
        object_type *o_ptr = &st_ptr->stock[i];
        if (!accept_activation_object(o_ptr)) continue;

        if (choice == current_index) return (st_ptr->stock[i]);

        current_index++;
    }

    // The player no longer has the object, so return a blank object
    return (hks_ptr->step_object);
}

// Find the new object activation based on the combo box
void HotKeyDialog::active_activation_changed(int choice)
{
    QString sender_id = QObject::sender()->objectName();
    int this_step = get_current_step(sender_id);

    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[this_step];

    object_type object_type_body = create_selected_object(hks_ptr->step_args);
    object_type *o_ptr = &object_type_body;

    bool old_obj_dir = obj_needs_aim(o_ptr);

    // Remember the object choice
    object_type_body = find_selected_activation(choice, this_step);

    hks_ptr->step_args.k_idx = o_ptr->k_idx;
    hks_ptr->step_args.item = o_ptr->ego_num;
    hks_ptr->step_args.number = o_ptr->art_num;
    hks_ptr->step_args.string1 = object_desc(o_ptr, ODESC_BASE | ODESC_PREFIX);

    bool new_obj_dir = obj_needs_aim(o_ptr);

    // Remove the direction box
    if (!new_obj_dir && old_obj_dir)
    {
        delete_targeting_choices(this_step);
        hks_ptr->step_args.direction = DIR_UNKNOWN;
    }

    // Add the direction box if needed
    if (new_obj_dir && !old_obj_dir)
    {
        hks_ptr->step_args.direction = DIR_UNKNOWN;

        // Find the hbox layout
        QString item_id = (QString("hlay_step_%1") .arg(this_step));
        QList<QHBoxLayout *> hlay_list = this->findChildren<QHBoxLayout *>();
        for (int x = 0; x < hlay_list.size(); x++)
        {
            QHBoxLayout *this_hlay = hlay_list.at(x);

            QString this_name = this_hlay->objectName();

            if (this_name.contains(item_id))
            {
                create_targeting_choices(this_hlay, this_step);
                break;
            }
        }
    }
}

/*
 * For activation items, k_idx = object num, item = ego_item #, number = artifact number
 * This one is a little more complicated because a simple object kind can be selected,
 * but also ego items and artifacts.
 */
void HotKeyDialog::create_activation_dropbox(QHBoxLayout *this_layout, int this_step)
{
    object_type object_type_body;
    object_type *o_ptr;

    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[this_step];

    QPointer<QVBoxLayout> vlay_obj_kind = new QVBoxLayout;
    vlay_obj_kind->setObjectName(QString("vlay_activation_step_%1") .arg(this_step));
    this_layout->addLayout(vlay_obj_kind);

    //Create the combobox
    QPointer<QComboBox> this_combobox = new QComboBox;
    this_combobox->setObjectName(QString("activation_combo_step_%1") .arg(this_step));
    int current_index = -1;
    int count = 0;

    // Nothing has been selected yet
    if (!hks_ptr->step_args.k_idx) current_index = 0;

    // Check what the player is holding
    for (int i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        o_ptr = &inventory[i];
        if (!accept_activation_object(o_ptr)) continue;

        this_combobox->addItem(QString("%1") .arg(STEP_MULT * this_step + i));
        this_combobox->setItemText(count, object_desc(o_ptr, ODESC_BASE | ODESC_PREFIX));

        // Try to find the current index.
        if (activation_matches_selection(&hks_ptr->step_args, o_ptr)) current_index = count;

        count++;
    }
    store_type *st_ptr = &store[STORE_HOME];

    // Check the home
    for (int i = 0; i < st_ptr->stock_num; i++)
    {
        o_ptr = &st_ptr->stock[i];
        if (!accept_activation_object(o_ptr)) continue;

        this_combobox->addItem(QString("%1") .arg(STEP_MULT * this_step + ALL_INVEN_TOTAL + i));
        this_combobox->setItemText(count, object_desc(o_ptr, ODESC_BASE | ODESC_PREFIX));

        // Try to find the current index.
        if (activation_matches_selection(&hks_ptr->step_args, o_ptr)) current_index = count;

        count++;
    }

    // Add a header
    QPointer<QLabel> header_dir = new QLabel("<b>Select Activatable Object</b>");
    vlay_obj_kind->addWidget(header_dir);
    if (!count)
    {
        header_dir->setText("<b>No Known Activatable Objects</b>");
        delete this_combobox;
        vlay_obj_kind->addStretch(1);
        return;
    }

    object_type_body = find_selected_activation(current_index, this_step);
    o_ptr = &object_type_body;

    // Player no longer has the object that is selected, remember it
    if (current_index == -1)
    {
       object_type_body = create_selected_object(hks_ptr->step_args);

       current_index = STEP_MULT * this_step + ALL_INVEN_TOTAL + st_ptr->stock_num;

       this_combobox->addItem(QString("%1") .arg(current_index));
       this_combobox->setItemText(count++, object_desc(o_ptr, ODESC_BASE | ODESC_PREFIX));

       hks_ptr->step_object.object_copy(o_ptr);
    }

    hks_ptr->step_args.k_idx = o_ptr->k_idx;
    hks_ptr->step_args.item = o_ptr->ego_num;
    hks_ptr->step_args.number = o_ptr->art_num;
    hks_ptr->step_args.string1 = object_desc(o_ptr, ODESC_BASE | ODESC_PREFIX);

    this_combobox->setCurrentIndex(current_index);
    hks_ptr->step_args.verify = TRUE;

    connect(this_combobox, SIGNAL(currentIndexChanged(int)), this, SLOT(active_activation_changed(int)));
    vlay_obj_kind->addWidget(this_combobox);
    vlay_obj_kind->addStretch(1);
}

// Manuallly turning the buttons true or false is necessary
// because the button group covers more than one step
void HotKeyDialog::hotkey_step_direction_changed(int new_dir)
{
    int step = new_dir / STEP_MULT;
    int this_dir = new_dir % STEP_MULT;

    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];

    hks_ptr->step_args.direction = this_dir;

    QString button_dir = QString("Specify Direction during command execution");
    if (this_dir == DIR_NORTHWEST) button_dir = QString("NorthWest");
    else if (this_dir == DIR_NORTH) button_dir = QString("North");
    else if (this_dir == DIR_NORTHEAST) button_dir = QString("NorthEast");
    else if (this_dir == DIR_EAST) button_dir = QString("East");
    else if (this_dir == DIR_WEST) button_dir = QString("West");
    else if (this_dir == DIR_SOUTHEAST) button_dir = QString("SouthEast");
    else if (this_dir == DIR_SOUTHWEST) button_dir = QString("SouthWest");
    else if (this_dir == DIR_SOUTH) button_dir = QString("South");

    // Manually turn the buttons true or false.
    QList<QRadioButton *> radio_list = this->findChildren<QRadioButton *>();

    for (int x = 0; x < radio_list.size(); x++)
    {
        QRadioButton *this_radio = radio_list.at(x);

        // Not the right step.
        QString item_id = this_radio->objectName();
        item_id.remove("direction_step_", Qt::CaseInsensitive);
        int which_step = item_id.toInt();
        if (step != which_step) continue;

        // Turn on or off if it is a match
        QString this_name = this_radio->toolTip();
        if (strings_match(this_name, button_dir))
        {
            this_radio->setChecked(TRUE);
        }
        else this_radio->setChecked(FALSE);
    }
}

void HotKeyDialog::delete_direction_pad(int step)
{
    // First, remove the radio buttons from the group
    QList<QRadioButton *> radio_list = this->findChildren<QRadioButton *>();
    QString radio_id = (QString("direction_step_%1") .arg(step));
    for (int x = 0; x < radio_list.size(); x++)
    {
        QRadioButton *this_radio = radio_list.at(x);

        QString this_name = this_radio->objectName();

        if (this_name.contains(radio_id))
        {
            // Remove it from the target group
            group_directions->removeButton(this_radio);
            break;
        }
    }

    // Now remove the vbox layout
    QString item_id = (QString("vlay_direction_step_%1") .arg(step));
    QList<QVBoxLayout *> vlay_list = this->findChildren<QVBoxLayout *>();
    for (int x = 0; x < vlay_list.size(); x++)
    {
        QVBoxLayout *this_vlay = vlay_list.at(x);

        QString this_name = this_vlay->objectName();

        if (this_name.contains(item_id))
        {
            clear_layout(this_vlay);
            this_vlay->deleteLater();
            break;
        }
    }
}

void HotKeyDialog::create_direction_pad(QHBoxLayout *this_layout, int step)
{
    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];

    QPointer<QVBoxLayout> vlay_direction = new QVBoxLayout;
    this_layout->addLayout(vlay_direction);
    vlay_direction->setObjectName(QString("vlay_direction_step_%1") .arg(step));

    // Add a header
    QPointer<QLabel> header_dir = new QLabel("<b>Direction:</b>");
    vlay_direction->addWidget(header_dir);

    //  Not all classes can set traps
    if (hks_ptr->step_commmand == HK_TYPE_MAKE_TRAP)
    {
        if (!(cp_ptr->flags & (CF_SET_TRAPS)))
        {
            header_dir->setText("<b>You cannot set traps!</b>");
            vlay_direction->addStretch(1);
            return;
        }
    }

    QPointer<QGridLayout> gridlay_direction = new QGridLayout;
    vlay_direction->addLayout(gridlay_direction);

    // Add all the buttons
    QPointer<QRadioButton> north_west = new QRadioButton;
    group_directions->addButton(north_west, step * STEP_MULT + DIR_NORTHWEST);
    north_west->setToolTip("NorthWest");
    north_west->setIcon(QIcon(":/icons/lib/icons/arrow-northwest.png"));
    gridlay_direction->addWidget(north_west, 0, 0);
    if (hks_ptr->step_args.direction == DIR_NORTHWEST) north_west->setChecked(TRUE);
    else north_west->setChecked(FALSE);
    north_west->setObjectName(QString("direction_step_%1") .arg(step));

    QPointer<QRadioButton> north = new QRadioButton;
    group_directions->addButton(north, step * STEP_MULT + DIR_NORTH);
    north->setToolTip("North");
    north->setIcon(QIcon(":/icons/lib/icons/arrow-north.png"));
    gridlay_direction->addWidget(north, 0, 1);
    if (hks_ptr->step_args.direction == DIR_NORTH) north->setChecked(TRUE);
    else north->setChecked(FALSE);
    north->setObjectName(QString("direction_step_%1") .arg(step));

    QPointer<QRadioButton> north_east = new QRadioButton;
    group_directions->addButton(north_east, step * STEP_MULT + DIR_NORTHEAST);
    north_east->setToolTip("NorthEast");
    north_east->setIcon(QIcon(":/icons/lib/icons/arrow-northeast.png"));
    gridlay_direction->addWidget(north_east, 0, 2);
    if (hks_ptr->step_args.direction == DIR_NORTHEAST) north_east->setChecked(TRUE);
    else north_east->setChecked(FALSE);
    north_east->setObjectName(QString("direction_step_%1") .arg(step));

    QPointer<QRadioButton> west = new QRadioButton;
    group_directions->addButton(west, step * STEP_MULT + DIR_WEST);
    west->setToolTip("West");
    west->setIcon(QIcon(":/icons/lib/icons/arrow-west.png"));
    gridlay_direction->addWidget(west, 1, 0);
    if (hks_ptr->step_args.direction == DIR_WEST) west->setChecked(TRUE);
    else west->setChecked(FALSE);
    west->setObjectName(QString("direction_step_%1") .arg(step));

    QPointer<QRadioButton> dir_none = new QRadioButton;
    group_directions->addButton(dir_none, step * STEP_MULT + DIR_UNKNOWN);
    dir_none->setToolTip("Specify Direction during command execution");
    dir_none->setIcon(QIcon(":/icons/lib/icons/target-cancel.png"));
    gridlay_direction->addWidget(dir_none, 1, 1);
    if (hks_ptr->step_args.direction == DIR_UNKNOWN) dir_none->setChecked(TRUE);
    else dir_none->setChecked(FALSE);
    dir_none->setObjectName(QString("direction_step_%1") .arg(step));

    QPointer<QRadioButton> east = new QRadioButton;
    group_directions->addButton(east, step * STEP_MULT + DIR_EAST);
    east->setToolTip("East");
    east->setIcon(QIcon(":/icons/lib/icons/arrow-east.png"));
    gridlay_direction->addWidget(east, 1, 2);
    if (hks_ptr->step_args.direction == DIR_EAST) east->setChecked(TRUE);
    else east->setChecked(FALSE);
    east->setObjectName(QString("direction_step_%1") .arg(step));

    QPointer<QRadioButton> south_west = new QRadioButton;
    group_directions->addButton(south_west, step * STEP_MULT + DIR_SOUTHWEST);
    south_west->setToolTip("SouthWest");
    south_west->setIcon(QIcon(":/icons/lib/icons/arrow-southwest.png"));
    gridlay_direction->addWidget(south_west, 2, 0);
    if (hks_ptr->step_args.direction == DIR_SOUTHWEST) south_west->setChecked(TRUE);
    else south_west->setChecked(FALSE);
    south_west->setObjectName(QString("direction_step_%1") .arg(step));

    QPointer<QRadioButton> south = new QRadioButton;
    group_directions->addButton(south, step * STEP_MULT + DIR_SOUTH);
    south->setToolTip("South");
    south->setIcon(QIcon(":/icons/lib/icons/arrow-south.png"));
    gridlay_direction->addWidget(south, 2, 1);
    if (hks_ptr->step_args.direction == DIR_SOUTH) south->setChecked(TRUE);
    else south->setChecked(FALSE);
    south->setObjectName(QString("direction_step_%1") .arg(step));

    QPointer<QRadioButton> south_east = new QRadioButton;
    group_directions->addButton(south_east, step * STEP_MULT + DIR_SOUTHEAST);
    south_east->setToolTip("SouthEast");
    south_east->setIcon(QIcon(":/icons/lib/icons/arrow-southeast.png"));
    gridlay_direction->addWidget(south_east, 2, 2);
    if (hks_ptr->step_args.direction == DIR_SOUTHEAST) south_east->setChecked(TRUE);
    else south_east->setChecked(FALSE);
    south_east->setObjectName(QString("direction_step_%1") .arg(step));

    vlay_direction->addStretch(1);
}

// Add buttons for inserting and deleting steps
void HotKeyDialog::create_step_buttons(QHBoxLayout *this_layout, int step)
{
    QPointer<QVBoxLayout> vlay_direction = new QVBoxLayout;
    this_layout->addLayout(vlay_direction);
    vlay_direction->setObjectName(QString("vlay_buttons_step_%1") .arg(step));

    QPointer<QPushButton> insert_step_button = new QPushButton;
    insert_step_button->setIcon(QIcon(":/icons/lib/icons/arrow-up-double.png"));
    insert_step_button->setToolTip("Insert a new hotkey step before this step.");
    insert_step_button->setObjectName(QString("Step_Command_%1") .arg(step));
    connect(insert_step_button, SIGNAL(pressed()), this, SLOT(insert_step()));
    vlay_direction->addWidget(insert_step_button);

    // Add an "delete step" button for all steps
    QPointer<QPushButton> delete_step_button = new QPushButton;
    delete_step_button->setIcon(QIcon(":/icons/lib/icons/destroy.png"));
    delete_step_button->setToolTip("Delete this hotkey step.");
    delete_step_button->setObjectName(QString("Step_Command_%1") .arg(step));
    vlay_direction->addWidget(delete_step_button);
    connect(delete_step_button, SIGNAL(pressed()), this, SLOT(delete_step()));

    QPointer<QPushButton> add_step_button = new QPushButton;
    add_step_button->setIcon(QIcon(":/icons/lib/icons/arrow-down-double.png"));
    add_step_button->setToolTip("Add a new hotkey step after this step.");
    add_step_button->setObjectName(QString("Step_Command_%1") .arg(step));
    connect(add_step_button, SIGNAL(pressed()), this, SLOT(add_step()));
    vlay_direction->addWidget(add_step_button);

    vlay_direction->addStretch(1);
}

// Insert a step before the current one
void HotKeyDialog::insert_step()
{
    QString sender_id = QObject::sender()->objectName();
    int this_step = get_current_step(sender_id);

    hotkey_step dummy_step;
    dummy_step.step_commmand = HK_TYPE_EMPTY;
    dummy_step.step_args.wipe();
    dummy_step.step_object.object_wipe();

    dialog_hotkey.hotkey_steps.insert(this_step, dummy_step);

    display_hotkey_steps();
}

// Delete the current step
void HotKeyDialog::delete_step()
{
    QString sender_id = QObject::sender()->objectName();
    int this_step = get_current_step(sender_id);

    if (dialog_hotkey.hotkey_steps.size() == 1)
    {
        dialog_hotkey.clear_hotkey_steps();
    }
    else
    {
        dialog_hotkey.hotkey_steps.remove(this_step);
    }

    display_hotkey_steps();
}

// Add a step after the current one
void HotKeyDialog::add_step()
{
    QString sender_id = QObject::sender()->objectName();
    int this_step = get_current_step(sender_id) + 1;

    hotkey_step dummy_step;
    dummy_step.step_commmand = HK_TYPE_EMPTY;
    dummy_step.step_args.wipe();

    dialog_hotkey.hotkey_steps.insert(this_step, dummy_step);

    display_hotkey_steps();
}

void HotKeyDialog::create_one_hotkey_step(QHBoxLayout *this_layout, int step)
{
    // Make sure any radio buttons are removed form their group
    delete_direction_pad(step);
    delete_targeting_choices(step);
    delete_specific_object_choices(step);
    clear_layout(this_layout);

    hotkey_step *hks_ptr = &dialog_hotkey.hotkey_steps[step];

    hotkey_type *ht_ptr = &hotkey_actions[hks_ptr->step_commmand];

    if (ht_ptr->hotkey_needs == HK_NEEDS_DIRECTION)
    {

        hks_ptr->step_args.k_idx = 0;
        hks_ptr->step_args.number = 0;
        create_direction_pad(this_layout, step);

    }
    else if (ht_ptr->hotkey_needs == HK_NEEDS_OBJECT_KIND)
    {
        create_object_kind_dropbox(this_layout, step);
        hks_ptr->step_args.number = 0;

        if (obj_needs_aim(hks_ptr->step_args.k_idx))
        {
            create_targeting_choices(this_layout, step);
        }
        else hks_ptr->step_args.direction = 0;
    }
    else if (ht_ptr->hotkey_needs == HK_NEEDS_SPELL)
    {
        hks_ptr->step_args.k_idx = 0;
        create_spell_choice_dropbox(this_layout, step);

        if (spell_list.size() && spell_needs_aim(cp_ptr->spell_book, hks_ptr->step_args.number))
        {
            create_targeting_choices(this_layout, step);
        }
        else hks_ptr->step_args.direction = 0;
    }
    else if (ht_ptr->hotkey_needs == HK_NEEDS_ACIVATION)
    {
        create_activation_dropbox(this_layout, step);

        object_type *o_ptr = find_object_activation(hks_ptr->step_args);

        if (obj_needs_aim(o_ptr))
        {
            create_targeting_choices(this_layout, step);
        }
        else hks_ptr->step_args.direction = 0;
    }
    else if (ht_ptr->hotkey_needs == HK_NEEDS_SPECIFIC_OBJECT)
    {
        create_specific_object_choices(this_layout, step);

        if ((hks_ptr->step_commmand == HK_FIRE_AMMO) ||
            (hks_ptr->step_commmand == HK_THROW))
        {
            create_targeting_choices(this_layout, step);
        }
        else hks_ptr->step_args.direction = 0;
    }
    else if (ht_ptr->hotkey_needs == HK_NEEDS_REST)
    {
        create_resting_choices(this_layout, step);
    }

    this_layout->addStretch(1);
}

// Wipe and redraw the hotkey steps
void HotKeyDialog::display_hotkey_steps()
{
    // First, make sure all radio buttons are cleared from each group
    for (int i = 0; i < dialog_hotkey.hotkey_steps.size(); i++)
    {
        delete_direction_pad(i);
        delete_targeting_choices(i);
        delete_specific_object_choices(i);
    }

    clear_layout(vlay_hotkey_steps);

    for (int i = 0; i < dialog_hotkey.hotkey_steps.size(); i++)
    {
        QPointer<QHBoxLayout> hlay_header = new QHBoxLayout;
        vlay_hotkey_steps->addLayout(hlay_header);

        create_step_buttons(hlay_header, i);

        QPointer<QVBoxLayout> this_vlayout = new QVBoxLayout;
        hlay_header->addLayout(this_vlayout);
        QPointer<QLabel> header_step = new QLabel(QString("<b>Step %1:  </b>") .arg(i + 1));
        this_vlayout->addWidget(header_step);
        this_vlayout->addStretch(1);

        QPointer<QComboBox> this_combo_box = new QComboBox;
        this_combo_box->setObjectName(QString("Step_Command_%1") .arg(i));
        for (int x = 0; x < HK_TYPE_END; x++)
        {
            this_combo_box->addItem(QString("%1") .arg(x));

            QString hotkey_name = hotkey_actions[x].name;

            if (x == HK_CAST_SPELL)
            {
                if (cp_ptr->spell_book)
                {
                    QString noun = capitalize_first(cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0));
                    QString verb = capitalize_first(cast_spell(MODE_SPELL_VERB, cp_ptr->spell_book, 1, 0));

                    hotkey_name = (QString("%1 %2") .arg(verb) .arg(noun));
                }
            }
            this_combo_box->setItemText(x, hotkey_name);
        }
        this_combo_box->setCurrentIndex(dialog_hotkey.hotkey_steps[i].step_commmand);
        connect(this_combo_box, SIGNAL(currentIndexChanged(int)), this, SLOT(active_hotkey_command_changed(int)));
        QPointer<QVBoxLayout> combo_vlayout = new QVBoxLayout;
        hlay_header->addLayout(combo_vlayout);
        combo_vlayout->addWidget(this_combo_box);
        combo_vlayout->addStretch(1);

        QPointer<QHBoxLayout> this_layout = new QHBoxLayout;
        this_layout->setObjectName(QString("hlay_step_%1") .arg(i));
        create_one_hotkey_step(this_layout, i);
        hlay_header->addLayout(this_layout);
    }
}


HotKeyDialog::HotKeyDialog(void)
{

    // Start with the first hotkey
    load_new_hotkey(0);

    // Since these radio button groups can cover multiple steps, they are toggled manuallly.
    group_directions = new QButtonGroup(this);
    group_directions->setExclusive(FALSE);
    connect(group_directions, SIGNAL(buttonClicked(int)), this, SLOT(hotkey_step_direction_changed(int)));
    group_target_choices = new QButtonGroup(this);
    group_target_choices->setExclusive(FALSE);
    connect(group_target_choices, SIGNAL(buttonClicked(int)), this, SLOT(hotkey_step_target_changed(int)));
    group_specific_object = new QButtonGroup(this);
    group_specific_object->setExclusive(FALSE);
    connect(group_specific_object, SIGNAL(buttonClicked(int)), this, SLOT(hotkey_step_obj_selection_changed(int)));
    group_resting_choices = new QButtonGroup(this);
    group_resting_choices->setExclusive(FALSE);
    connect(group_resting_choices, SIGNAL(buttonClicked(int)), this, SLOT(hotkey_step_rest_choice_changed(int)));

    //Set up the main scroll bar
    top_layout = new QVBoxLayout;
    main_layout = new QVBoxLayout;
    top_widget = new QWidget;
    scroll_box = new QScrollArea;
    top_widget->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding);
    top_widget->setLayout(main_layout);
    scroll_box->setWidget(top_widget);
    scroll_box->setWidgetResizable(TRUE);
    top_layout->addWidget(scroll_box);

    //Build the header
    QPointer<QLabel> header_main = new QLabel("<b><h2>Hotkey Menu</b></h2>");
    main_layout->addWidget(header_main);

    add_hotkeys_header();

    // Lay Out all of the hotkey steps
    vlay_hotkey_steps = new QVBoxLayout;
    main_layout->addLayout(vlay_hotkey_steps);
    display_hotkey_steps();
    vlay_hotkey_steps->addStretch(1);

    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox(QDialogButtonBox::Ok);
    connect(buttons, SIGNAL(accepted()), this, SLOT(accept()));
    top_layout->addWidget(buttons);

    setLayout(top_layout);
    setWindowTitle(tr("Hotkey Menu"));

    QSize this_size = QSize(width(), height());
    resize(ui_max_widget_size(this_size));
    updateGeometry();

    this->exec();

    save_current_hotkey();
}

void do_hotkey_manage()
{
    HotKeyDialog();

    ui_update_hotkey_toolbar();
}

// Find an item to use
static int find_item(int k_idx, int mode)
{
    s16b this_o_idx, next_o_idx = 0;
    int i;

    bool use_inven = ((mode & USE_INVEN) ? TRUE : FALSE);
    bool use_equip = ((mode & USE_EQUIP) ? TRUE : FALSE);
    bool use_floor = ((mode & USE_FLOOR) ? TRUE : FALSE);
    bool use_quiver = ((mode & USE_QUIVER) ? TRUE : FALSE);

    /* First try to find the objects on the floor */
    if (use_floor) for (this_o_idx = dungeon_info[p_ptr->py][p_ptr->px].object_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        if (o_ptr->k_idx == k_idx) return (-this_o_idx);
    }

    // Now check the backpack
    if (use_inven) for (i = 0; i < INVEN_PACK; i++)
    {
        object_type *o_ptr = &inventory[i];

        if (o_ptr->k_idx == k_idx) return (i);
    }

    if (use_equip) for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];

        if (o_ptr->k_idx == k_idx) return (i);
    }

    if (use_quiver) for (i = QUIVER_START; i < QUIVER_END; i++)
    {
        object_type *o_ptr = &inventory[i];

        if (o_ptr->k_idx == k_idx) return (i);
    }

    // Failed to find anything
    return (MAX_S16B);
}

// Find an item to use
static int find_inscribed_item(QString inscription, int command)
{
    // No inscription
    if (!inscription.length()) return (MAX_S16B);

    // First check the player equipment and inventory
    for (int i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];

        if (!o_ptr->inscription.contains(inscription)) continue;

        if (command == HK_FIRE_AMMO)
        {
            if (!ammo_can_fire(o_ptr, i)) continue;
        }

        // Can't throw wielded items
        if (command == HK_THROW)
        {
            if (i >= INVEN_WIELD && i < QUIVER_START) continue;
        }

        return (i);
    }

    s16b this_o_idx, next_o_idx = 0;

    /* Next try to find the objects on the floor */
    for (this_o_idx = dungeon_info[p_ptr->py][p_ptr->px].object_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        if (command == HK_FIRE_AMMO)
        {
            if (!ammo_can_fire(o_ptr, this_o_idx)) continue;
        }

        if (o_ptr->inscription.contains(inscription)) return (-this_o_idx);
    }

    // Failed to find anything
    return (MAX_S16B);
}

// Find hte item to activate.  Must be wielded.
static int find_item_activation(cmd_arg *this_arg)
{
    for (int i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];

        if (!activation_matches_selection(this_arg, o_ptr)) continue;

        // Found a match
        return (i);
    }
     return (-1);
}

// Get the intended direction for a hotkey that requires one
static int extract_hotkey_dir(int dir, bool trap_spell)
{
    /* Check for confusion */
    if (p_ptr->timed[TMD_CONFUSED])
    {
        message(QString("You are confused."));
        return(ddd[randint0(8)]);
    }

    // First target closest, if there is anything there
    if (dir == DIR_CLOSEST)
    {
        int mode = TARGET_QUIET;

        if (!trap_spell) mode |= TARGET_KILL;
        else mode |= TARGET_TRAP;

        if (target_set_closest(mode))
        {
            return(DIR_CLOSEST);
        }
        else dir = DIR_UNKNOWN;
    }
    if (dir == DIR_TARGET)
    {
        if (target_okay()) return (DIR_TARGET);
    }

    if (!get_aim_dir(&dir, trap_spell)) return (DIR_UNKNOWN);

    return (dir);
}

/*
 * Run the specified active hotkey step
 * First checks if there is a hotkey to run
 */
static void run_hotkey_step(int step)
{
    //First, make sure there are remaining hotkey steps
    if (!running_hotkey.has_commands()) return;

    hotkey_step *this_step = &running_hotkey.hotkey_steps[step];

    int command = this_step->step_commmand;
    hotkey_type *ht_ptr = &hotkey_actions[command];

    cmd_arg *arg_ptr = &this_step->step_args;

    if (command == HK_NEEDS_NOTHING)
    {
        // Do nothing
    }
    else if (command == HK_TYPE_MOVE) do_cmd_walk(arg_ptr->direction, FALSE);
    else if (command == HK_TYPE_JUMP) do_cmd_walk(arg_ptr->direction, TRUE);
    else if (command == HK_TYPE_RUN) do_cmd_run(arg_ptr->direction);
    else if (command == HK_TYPE_ALTER) do_cmd_alter(arg_ptr->direction);
    else if (command == HK_TYPE_DISARM) do_cmd_disarm(arg_ptr->direction);
    else if (command == HK_TYPE_CLOSE) do_cmd_close(arg_ptr->direction);
    else if (command == HK_TYPE_OPEN) do_cmd_open(arg_ptr->direction);
    else if (command == HK_TYPE_BASH) do_cmd_bash(arg_ptr->direction);
    else if (command == HK_TYPE_TUNNEL) do_cmd_tunnel(arg_ptr->direction);
    else if (command == HK_TYPE_MAKE_TRAP) do_cmd_make_trap(arg_ptr->direction);
    else if (command == HK_TYPE_SPIKE) do_cmd_spike(arg_ptr->direction);
    else if (command == HK_TYPE_HOLD) do_cmd_hold();

    else if (ht_ptr->hotkey_needs == HK_NEEDS_OBJECT_KIND)
    {
        arg_ptr->item = find_item(arg_ptr->k_idx, USE_FLOOR | USE_INVEN);

        // Didn't find the item
        if (arg_ptr->item == MAX_S16B)
        {
            message(QString("Unable to locate %1") .arg(object_desc(arg_ptr->k_idx, ODESC_FULL | ODESC_PREFIX | ODESC_SINGULAR)));
            return;
        }
        if (obj_needs_aim(arg_ptr->k_idx))
        {
            bool trap_obj = k_info[arg_ptr->k_idx].is_trap_object_kind();

            arg_ptr->direction = extract_hotkey_dir(arg_ptr->direction, trap_obj);

            if (arg_ptr->direction == DIR_UNKNOWN) return;
        }

        // Use the item
        command_use(this_step->step_args);
    }

    else if (ht_ptr->hotkey_needs == HK_NEEDS_SPELL)
    {
        if (p_ptr->can_cast())
        {
            if (spell_needs_aim(cp_ptr->spell_book, arg_ptr->number))
            {
                bool trap_spell = is_trap_spell(cp_ptr->spell_book, arg_ptr->number);

                arg_ptr->direction = extract_hotkey_dir(arg_ptr->direction, trap_spell);

                if (arg_ptr->direction == DIR_UNKNOWN) return;
            }

            // Use the item
            cast_spell(this_step->step_args);
        }
    }

    // For this one, the cmd_arg needs to be built
    // as many fields from arg_ptr are used to find the item
    else if (ht_ptr->hotkey_needs == HK_NEEDS_ACIVATION)
    {
        cmd_arg command_args;
        command_args.wipe();

        command_args.item = find_item_activation(arg_ptr);

        // Didn't find the item
        if (command_args.item <= -1)
        {
            message(QString("Unable to locate %1") .arg(arg_ptr->string1));
        }
        else
        {
            object_type *o_ptr = &inventory[command_args.item];

            if (o_ptr->timeout)
            {
                message(QString("The %1 is recharging.") .arg(object_desc(o_ptr, ODESC_FULL)));
                return;
            }

            if (obj_needs_aim(o_ptr))
            {
                bool trap_obj = k_info[arg_ptr->k_idx].is_trap_object_kind();

                command_args.direction = extract_hotkey_dir(arg_ptr->direction, trap_obj);

                if (arg_ptr->direction == DIR_UNKNOWN) return;
            }

            command_args.verify = TRUE;

            // Use the item
            command_use(command_args);
        }
    }

    else if (ht_ptr->hotkey_needs == HK_NEEDS_SPECIFIC_OBJECT)
    {
        cmd_arg command_args;
        command_args.wipe();

        if (arg_ptr->choice == OS_SELECT_DURING_USE)
        {
            if (command == HK_FIRE_AMMO)
            {
                do_cmd_fire();
            }
            else // (command == HK_THROW)
            {
                do_cmd_throw();
            }
            return;
        }
        else  // OS_FIND_SPECIFIC_INSCRIPTION
        {
            command_args.item = find_inscribed_item(arg_ptr->string2, command);

            // Didn't find the item
            if (command_args.item == MAX_S16B)
            {
                message(QString("Unable to locate an item inscribed with %1") .arg(arg_ptr->string2));
                return;
            }
        }

        if ((command == HK_FIRE_AMMO) || (command == HK_THROW))
        {
            command_args.direction = extract_hotkey_dir(arg_ptr->direction, FALSE);

            if (arg_ptr->direction == DIR_UNKNOWN) return;
        }

        if (command == HK_FIRE_AMMO) command_fire(command_args);
        else if (command == HK_THROW) command_throw(command_args);
    }
    else if (ht_ptr->hotkey_needs == HK_NEEDS_REST)
    {
        /* Cancel the command */
        p_ptr->player_command_wipe();

        p_ptr->command_current = CMD_RESTING;
        p_ptr->player_args.choice = arg_ptr->choice;

        p_ptr->redraw |= (PR_STATUSBAR | PR_SIDEBAR_PL);

        if (arg_ptr->choice == REST_TURNCOUNT)
        {
            if (!arg_ptr->number) return;
            p_ptr->player_args.repeats = arg_ptr->number;
        }
        // Set up the args for command_rest
        arg_ptr->number = 0;

        command_rest(this_step->step_args);
    }
}

//Loop to run the complete hotkey
static void run_hotkey_steps()
{
    for (int i = 0; i < running_hotkey.hotkey_steps.size(); i++)
    {
        run_hotkey_step(i);
    }

    running_hotkey.clear_hotkey_steps();
}

static bool set_up_hotkey(int which_hotkey)
{
    //paranoia
    if (which_hotkey >= NUM_HOTKEYS)return (FALSE);

    single_hotkey *plyr_hk_ptr = &player_hotkeys[which_hotkey];

    // Make sure the hotkey is set up
    if (!plyr_hk_ptr->has_commands()) return (FALSE);

    running_hotkey.clear_hotkey_steps();
    running_hotkey.copy_hotkey(plyr_hk_ptr);

    run_hotkey_steps();

    return (TRUE);
}

bool check_hotkey_commands(int key_press, bool shift_key, bool alt_key, bool ctrl_key, bool meta_key)
{
    (void)alt_key;
    (void)ctrl_key;
    (void)meta_key;
    for (int i = 0; i < NUM_HOTKEYS; i++)
    {
        hotkey_list *hk_list_ptr = &list_hotkeys[i];

        if (key_press != hk_list_ptr->listed_hotkey) continue;

        if (shift_key != hk_list_ptr->shift) continue;
        return (set_up_hotkey(i));
    }

    return (FALSE);
}
