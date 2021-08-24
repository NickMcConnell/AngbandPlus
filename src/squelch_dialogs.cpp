
/*
 * File: squelch.c
 * Purpose: Item destruction
 *
 * Copyright (c) 2007 David T. Blackston, Iain McFall, DarkGod, Jeff Greene,
 * Diego Gonzalez, David Vestal, Pete Mack, Andrew Sidwell.
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */
#include "src/npp.h"

#include "src/squelch_dialogs.h"
#include "src/knowledge.h"
#include <QRadioButton>
#include <QDialogButtonBox>
#include <QCoreApplication>
#include <QLabel>
#include <QPushButton>
#include <QCheckBox>
#include <QComboBox>

/*
 * This (admittedly hacky) stores the mapping from tval to typeval
 * and is reinitialized every time do_cmd_squelch is called.  This
 * can certainly be done more cleanly.
 */
static int tv_to_type[MAXTV_TO_TYPE];
static bool seen_type[TYPE_MAX];

/* Categories for sval-dependent squelch. */
static tval_desc tvals[SQUELCH_TVALS_MAX] =
{
    {TYPE_AMMO, 	"Missiles"},
    {TYPE_BOW, 		"Missile Launchers"},
    {TYPE_WEAPON1, 	"Weapons (Swords)"},
    {TYPE_WEAPON2, 	"Weapons (Non Swords)"},
    {TYPE_BODY, 	"Body Armor"},
    {TYPE_CLOAK, 	"Cloaks"},
    {TYPE_SHIELD, 	"Shields"},
    {TYPE_HELM, 	"Helmets"},
    {TYPE_GLOVES, 	"Gloves"},
    {TYPE_BOOTS, 	"Boots"},
    {TYPE_AMULET, 	"Amulets"},
    {TYPE_RING, 	"Rings"},
    {TYPE_STAFF, 	"Staves"},
    {TYPE_WAND, 	"Wands"},
    {TYPE_ROD, 		"Rods"},
    {TYPE_SCROLL, 	"Scrolls"},
    {TYPE_POTION, 	"Potions"},
    {TYPE_BOOK, 	"Magic Books"},
    {TYPE_FOOD, 	"Food Items"},
    {TYPE_MISC, 	"Miscellaneous"},
};


/*
 * Hack -- initialize the mapping from tvals to typevals.
 * This is currently called every time the squelch menus are
 * accessed.  This can certainly be improved.
 */

static void init_tv_to_type(void)
{
    int i;


    /* Clear all of the types that have been seen */
    for (i = 0; i < TYPE_MAX; i++)
    {
        seen_type[i] = FALSE;
    }

    /* Make sure all the TV_TY_TYPES are within bounds */
    for (i = 0; i < MAXTV_TO_TYPE; i++)
    {
        tv_to_type[i] = TYPE_MAX;
    }

    tv_to_type[TV_SKELETON]=TYPE_MISC;
    tv_to_type[TV_BOTTLE]=TYPE_MISC;
    tv_to_type[TV_JUNK]=TYPE_MISC;
    tv_to_type[TV_SPIKE]=TYPE_MISC;
    tv_to_type[TV_CHEST]=TYPE_MISC;
    tv_to_type[TV_SHOT]=TYPE_AMMO;
    tv_to_type[TV_ARROW]=TYPE_AMMO;
    tv_to_type[TV_BOLT]=TYPE_AMMO;
    tv_to_type[TV_BOW]=TYPE_BOW;
    tv_to_type[TV_DIGGING]=TYPE_WEAPON2;
    tv_to_type[TV_HAFTED]=TYPE_WEAPON2;
    tv_to_type[TV_POLEARM]=TYPE_WEAPON2;
    tv_to_type[TV_SWORD]=TYPE_WEAPON1;
    tv_to_type[TV_BOOTS]=TYPE_BOOTS;
    tv_to_type[TV_GLOVES]=TYPE_GLOVES;
    tv_to_type[TV_HELM]=TYPE_HELM;
    tv_to_type[TV_CROWN]=TYPE_HELM;
    tv_to_type[TV_SHIELD]=TYPE_SHIELD;
    tv_to_type[TV_CLOAK]=TYPE_CLOAK;
    tv_to_type[TV_SOFT_ARMOR]=TYPE_BODY;
    tv_to_type[TV_HARD_ARMOR]=TYPE_BODY;
    tv_to_type[TV_DRAG_ARMOR]=TYPE_BODY;
    tv_to_type[TV_DRAG_SHIELD]=TYPE_SHIELD;
    tv_to_type[TV_LIGHT]=TYPE_MISC;
    tv_to_type[TV_AMULET]=TYPE_AMULET;
    tv_to_type[TV_RING]=TYPE_RING;
    tv_to_type[TV_STAFF]=TYPE_STAFF;
    tv_to_type[TV_WAND]=TYPE_WAND;
    tv_to_type[TV_ROD]=TYPE_ROD;
    tv_to_type[TV_SCROLL]=TYPE_SCROLL;
    tv_to_type[TV_POTION]=TYPE_POTION;
    tv_to_type[TV_FLASK]=TYPE_MISC;
    tv_to_type[TV_FOOD]=TYPE_FOOD;
    tv_to_type[TV_MAGIC_BOOK]=TYPE_BOOK;
    tv_to_type[TV_PRAYER_BOOK]=TYPE_BOOK;
    tv_to_type[TV_DRUID_BOOK]=TYPE_BOOK;
}

void ObjectSquelchDialog::update_squelch_settings(void)
{
    QString item_id = QObject::sender()->objectName();
    bool dummy;
    int sender_id = item_id.toInt(&dummy);

    // This would only happen if make_quality_squelch_buttons was not set up with a proper object name;
    if (!dummy) return;

    int object_idx = sender_id / 10;
    int squelch_setting = sender_id % 10;
    k_info[object_idx].squelch = squelch_setting;

    // now update all the buttons manually
    QList<QRadioButton *> button_group_list = this->findChildren<QRadioButton *>();

    // Now go through and set all the group buttons to false, except
    // the one just checked.  Ignore the other groups.
    for (int x = 0; x < button_group_list.size(); x++)
    {
        QRadioButton *this_radiobutton = button_group_list.at(x);

        QString this_name = this_radiobutton->objectName();

        int this_id = this_name.toInt(&dummy);

        // This would only happen if squelch_tval_changed was not set up with a proper object name;
        if (!dummy) continue;

        int this_idx = this_id / 10;
        int this_squelch_setting = this_id % 10;

        if (this_idx != object_idx) continue;
        if (this_squelch_setting == squelch_setting) continue;
        else this_radiobutton->setChecked(FALSE);
    }
}

// A new tval has been selected from the combobox.  Change the entries.
void ObjectSquelchDialog::squelch_tval_changed(int new_tval_number)
{
    clear_layout(squelch_glay);

    int row = 0;

    int new_tval = -1;
    int count = 0;

    // find the new TVAL
    for (int i = 0; i < SQUELCH_TVALS_MAX; i++)
    {
        if (!seen_type[tvals[i].tval]) continue;

        if (count++ != new_tval_number) continue;

        // Found it
        new_tval = tvals[i].tval;
        break;
    }

    for (int i = 0; i < squelch_choices.size(); i++)
    {
        int column = 0;
        int k_idx = squelch_choices[i].idx;

        object_kind *k_ptr = &k_info[k_idx];

        if (tv_to_type[k_ptr->tval] != new_tval) continue;

        QPointer<QLabel> object_name = new QLabel(QString("<b>%1</b>") .arg(strip_name(squelch_choices[i].idx)));
        object_name->setAlignment(Qt::AlignLeft);
        squelch_glay->addWidget(object_name, row, column++, Qt::AlignLeft);

        QPointer<QRadioButton> squelch_none = new QRadioButton(squelch_status[SQUELCH_NEVER]);
        squelch_none->setToolTip("Do not hide or destroy object.  Use default pickup options.");
        squelch_none->setObjectName(QString("%1") .arg((k_idx*10) + SQUELCH_NEVER));
        squelch_glay->addWidget(squelch_none, row, column++, Qt::AlignLeft);
        squelch_none->setAutoExclusive(FALSE);
        if (k_ptr->squelch == SQUELCH_NEVER) squelch_none->setChecked(TRUE);
        else squelch_none->setChecked(FALSE);
        connect(squelch_none, SIGNAL(pressed()), this, SLOT(update_squelch_settings()));

        QPointer<QRadioButton> no_pickup = new QRadioButton(squelch_status[NO_SQUELCH_NEVER_PICKUP]);
        no_pickup->setToolTip("Do not hide or destroy object.  Never automatically pickup object.");
        no_pickup->setObjectName(QString("%1") .arg((k_idx*10) + NO_SQUELCH_NEVER_PICKUP));
        squelch_glay->addWidget(no_pickup, row, column++, Qt::AlignLeft);
        no_pickup->setAutoExclusive(FALSE);
        if (k_ptr->squelch == NO_SQUELCH_NEVER_PICKUP) no_pickup->setChecked(TRUE);
        else no_pickup->setChecked(FALSE);
        connect(no_pickup, SIGNAL(pressed()), this, SLOT(update_squelch_settings()));

        QPointer<QRadioButton> yes_pickup = new QRadioButton(squelch_status[NO_SQUELCH_ALWAYS_PICKUP]);
        yes_pickup->setToolTip("Do not hide or destroy object.  Never automatically pickup object.");
        yes_pickup->setObjectName(QString("%1") .arg((k_idx*10) + NO_SQUELCH_ALWAYS_PICKUP));
        squelch_glay->addWidget(yes_pickup, row, column++, Qt::AlignLeft);
        yes_pickup->setAutoExclusive(FALSE);
        if (k_ptr->squelch == NO_SQUELCH_ALWAYS_PICKUP) yes_pickup->setChecked(TRUE);
        else yes_pickup->setChecked(FALSE);
        connect(yes_pickup, SIGNAL(pressed()), this, SLOT(update_squelch_settings()));

        QPointer<QRadioButton> always_squelch = new QRadioButton(squelch_status[SQUELCH_ALWAYS]);
        always_squelch->setToolTip("Automatically destroy object when player walks over it.  Ignore in object lists and messages.");
        always_squelch->setObjectName(QString("%1") .arg((k_idx*10) + SQUELCH_ALWAYS));
        squelch_glay->addWidget(always_squelch, row, column++, Qt::AlignLeft);
        always_squelch->setAutoExclusive(FALSE);
        if (k_ptr->squelch == SQUELCH_ALWAYS) always_squelch->setChecked(TRUE);
        else always_squelch->setChecked(FALSE);
        connect(always_squelch, SIGNAL(pressed()), this, SLOT(update_squelch_settings()));

        row++;
    }

    ui_request_size_update(central);
    QCoreApplication::processEvents();   // IMPORTANT: THE SIZE_HINT UPDATE IS ASYNC, SO WAIT FOR IT

    this->clientSizeUpdated();
}

// First sort by tval slot, then ego name.
static bool obj_kind_choices_sort(squelch_entry first, squelch_entry second)
{
    object_kind *k1_ptr = &k_info[first.idx];
    object_kind *k2_ptr = &k_info[second.idx];

    // Tiebreaker
    if (k1_ptr->tval > k2_ptr->tval) return (TRUE);
    if ((k1_ptr->tval == k2_ptr->tval) && (k1_ptr->cost > k2_ptr->cost))
    {
        return (TRUE);
    }

    return (FALSE);

}

void ObjectSquelchDialog::add_object_squelch_radiobuttons(void)
{
    QPointer<QHBoxLayout> squelch_hlay = new QHBoxLayout;
    main_layout->addLayout(squelch_hlay);

    // Start with a dropbox of object types
    QPointer<QComboBox> object_kind_choice = new QComboBox;

    // Populate the combo box
    int first_tval = -1;

    for (int i = 0; i < SQUELCH_TVALS_MAX; i++)
    {
        if (!seen_type[tvals[i].tval]) continue;

        // Remember the first tval, that is the first set of objects that will appear
        if (first_tval < 0) first_tval = i;

        object_kind_choice->addItem(capitalize_first(tvals[i].desc));
    }

    squelch_hlay->addWidget(object_kind_choice);
    squelch_hlay->addStretch(1);

    connect(object_kind_choice, SIGNAL(currentIndexChanged(int)), this, SLOT(squelch_tval_changed(int)));

    squelch_glay = new QGridLayout;
    main_layout->addLayout(squelch_glay);

    // Initialize the first set of known object kinds
    squelch_tval_changed(first_tval);

}

void ObjectSquelchDialog::gather_object_kinds(void)
{
    squelch_choices.clear();

    init_tv_to_type();

    for (int i = 1; i < z_info->k_max; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /* Skip "empty" items */
        if (k_ptr->k_name.isEmpty()) continue;
        if (!k_ptr->everseen) continue;
        // Skip artifact templates
        if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;
        // Skip gold
        if (k_ptr->tval == TV_GOLD) continue;

        seen_type[tv_to_type[k_ptr->tval]] = TRUE;

        squelch_entry  new_entry;
        new_entry.aware = TRUE;
        new_entry.idx = i;
        squelch_choices.append(new_entry);
    }

    qSort(squelch_choices.begin(), squelch_choices.end(), obj_kind_choices_sort);
}

/*
 *
 *
 * Object Squelch_menu
 *
 *
 */
ObjectSquelchDialog::ObjectSquelchDialog(void)
{
    gather_object_kinds();

    if (!squelch_choices.size())
    {
        pop_up_message_box("No known ego items!");
        return;
    }

    central = new QWidget;
    main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    // IMPORTANT: it must be called AFTER setting the layout
    this->setClient(central);

    QPointer<QLabel> quality_header = new QLabel("<b><h2>Object Squelch Menu</h2></b>");
    quality_header->setToolTip("The settings below allow the player to automatically destroy an object, or specify pickup preferences for that object.");
    main_layout->addWidget(quality_header, Qt::AlignCenter);

    add_object_squelch_radiobuttons();

    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    main_layout->addWidget(buttons);

    setWindowTitle(tr("Object Squelch Menu"));

    this->clientSizeUpdated();

    this->exec();
}

void do_object_squelch_menu()
{
    ObjectSquelchDialog();
}

/*
 *
 *
 * Ego Item Squelch_menu
 *
 *
 */


void EgoItemSquelchDialog::ego_squelch_status_changed()
{
    QString item_id = QObject::sender()->objectName();
    bool dummy;
    int sender_id = item_id.toInt(&dummy);

    // This would only happen if add_ego_item_checkboxes did not set up with a proper object name;
    if (!dummy) return;

    ego_item_type *e_ptr = &e_info[sender_id];

    if (e_ptr->squelch) e_ptr->squelch = FALSE;
    else e_ptr->squelch = TRUE;
}

void EgoItemSquelchDialog::add_ego_item_checkboxes(QGridLayout *return_layout)
{
    int columns_per_row = MIN(((ego_choices.size() / 12) + 1), 4);
    int num_rows = ego_choices.size() / columns_per_row;

    int row = 0;
    int column = 0;

    for (int i = 0; i < ego_choices.size(); i++)
    {
        ego_item_type *e_ptr = &e_info[ego_choices[i].e_idx];

        QPointer<QCheckBox> new_checkbox = new QCheckBox(get_ego_name(e_ptr));
        new_checkbox->setObjectName(QString("%1") .arg(ego_choices[i].e_idx));

        new_checkbox->setChecked(e_ptr->squelch);
        connect(new_checkbox, SIGNAL(pressed()), this, SLOT(ego_squelch_status_changed()));

        return_layout->addWidget(new_checkbox, row, column);

        if (row >= num_rows)
        {
            row = 0;
            column++;
        }
        else row++;
    }
}

// First sort by tval slot, then ego name.
static bool ego_choices_sort(ego_desc first, ego_desc second)
{
    // Tiebreaker
    return (first.long_name < second.long_name);
}

void EgoItemSquelchDialog::gather_ego_types()
{
    ego_choices.clear();

    /* Get the valid ego-items */
    for (int i = 0; i < alloc_ego_table.size(); i++)
    {
        alloc_entry_new *ae_ptr = &alloc_ego_table[i];

        ego_item_type *e_ptr = &e_info[ae_ptr->index];

        /* Skip "empty" and unknown ego-items */
        if (!e_ptr->e_name.length()) continue;
        if (!e_ptr->everseen) continue;

        ego_desc new_entry;

        /* Make the index */
        new_entry.e_idx = ae_ptr->index;
        new_entry.long_name = get_ego_name(e_ptr);

        ego_choices.append(new_entry);
    }

    qSort(ego_choices.begin(), ego_choices.end(), ego_choices_sort);
}

EgoItemSquelchDialog::EgoItemSquelchDialog(void)
{
    //First make sure we have known ego types.

    gather_ego_types();

    if (!ego_choices.size())
    {
        pop_up_message_box("No known ego items!");
        return;
    }

    central = new QWidget;
    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    // IMPORTANT: it must be called AFTER setting the layout
    this->setClient(central);

    QPointer<QLabel> quality_header = new QLabel("<b><h2>Ego Item Squelch Menu</h2></b>");
    quality_header->setToolTip("The settings below allow the player to automatically destroy an ego-item on identification.");
    main_layout->addWidget(quality_header, Qt::AlignCenter);

    QPointer<QGridLayout> checkbox_glay = new QGridLayout;
    main_layout->addLayout(checkbox_glay);

    add_ego_item_checkboxes(checkbox_glay);

    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    main_layout->addWidget(buttons);

    setWindowTitle(tr("Quality Squelch Menu"));

    this->clientSizeUpdated();

    this->exec();
}

void do_ego_item_squelch_menu()
{
    EgoItemSquelchDialog();
}


/*
 *
 *
 * Quality Squelch_menu
 *
 *
 */


// Set all applicable settings to teh new squelch quality
void QualitySquelchDialog::set_all_squelch_quality(int new_quality)
{
    // now update all the buttons manually
    QList<QRadioButton *> button_group_list = this->findChildren<QRadioButton *>();


    // Now go through and set the quality squelch_settings, and update the radio buttons.
    for (int x = 0; x < button_group_list.size(); x++)
    {
        QRadioButton *this_radiobutton = button_group_list.at(x);

        QString this_name = this_radiobutton->objectName();

        bool dummy;

        int this_id = this_name.toInt(&dummy);

        // This would only happen if make_quality_squelch_buttons was not set up with a proper object name;
        if (!dummy) continue;

        int this_group = this_id / 100;
        int this_setting = this_id % 100;

        // Continue for the non-applicable settings.
        if ((this_group == PS_TYPE_AMULET) || (this_group == PS_TYPE_RING))
        {
            if (new_quality > SQUELCH_CURSED) continue;
        }
        else if (this_group == PS_TYPE_LIGHT)
        {
            if (new_quality == SQUELCH_ALL) continue;
        }

        // Not the right quality_setting
        if (this_setting != new_quality)
        {
            this_radiobutton->setChecked(FALSE);
            continue;
        }

        // Change the squelch setting, set the radiobox to true;
        squelch_level[this_group] = new_quality;
        this_radiobutton->setChecked(TRUE);
    }
}

// Make a series of pushbuttons to change settings for all subgroups
void QualitySquelchDialog::make_quality_squelch_pushbuttons(QHBoxLayout *return_layout)
{
    QPointer<QPushButton> squelch_all_none = new QPushButton("Set all to Squelch None");
    connect(squelch_all_none, SIGNAL(pressed()), this, SLOT(set_all_quality_none()));
    return_layout->addWidget(squelch_all_none);

    return_layout->addStretch(1);

    QPointer<QPushButton> squelch_all_cursed = new QPushButton("Set all to Squelch Cursed");
    connect(squelch_all_cursed, SIGNAL(pressed()), this, SLOT(set_all_quality_cursed()));
    return_layout->addWidget(squelch_all_cursed);

    return_layout->addStretch(1);

    QPointer<QPushButton> squelch_all_average = new QPushButton("Set all to Squelch Average");
    connect(squelch_all_average, SIGNAL(pressed()), this, SLOT(set_all_quality_average()));
    return_layout->addWidget(squelch_all_average);

    return_layout->addStretch(1);

    QPointer<QPushButton> squelch_all_good_strong = new QPushButton("Set all to Squelch Good");
    connect(squelch_all_good_strong, SIGNAL(pressed()), this, SLOT(set_all_quality_good_strong()));
    return_layout->addWidget(squelch_all_good_strong);

    return_layout->addStretch(1);

    if (!cp_ptr->pseudo_id_heavy())
    {
        QPointer<QPushButton> squelch_all_good_weak = new QPushButton("Set all to Squelch Good (+Good Pseudo ID)");
        connect(squelch_all_good_weak, SIGNAL(pressed()), this, SLOT(set_all_quality_good_weak()));
        return_layout->addWidget(squelch_all_good_weak);

        return_layout->addStretch(1);
    }

    if (game_mode != GAME_NPPMORIA)
    {
        QPointer<QPushButton> squelch_all_but_artifact = new QPushButton("Set all to Squelch All But Artifact");
        connect(squelch_all_but_artifact, SIGNAL(pressed()), this, SLOT(set_all_quality_all_but_artifact()));
        return_layout->addWidget(squelch_all_but_artifact);
    }
}

void QualitySquelchDialog::update_quality_squelch_settings(void)
{
    QString item_id = QObject::sender()->objectName();
    bool dummy;
    int sender_id = item_id.toInt(&dummy);

    // This would only happen if make_quality_squelch_buttons was not set up with a proper object name;
    if (!dummy) return;

    int quality_group = sender_id / 100;
    int quality_setting = sender_id % 100;
    squelch_level[quality_group] = quality_setting;

    // now update all the buttons manually
    QList<QRadioButton *> button_group_list = this->findChildren<QRadioButton *>();

    // Now go through and set all the group buttons to false, except
    // the one just checked.  Ignore the other groups.
    for (int x = 0; x < button_group_list.size(); x++)
    {
        QRadioButton *this_radiobutton = button_group_list.at(x);

        QString this_name = this_radiobutton->objectName();

        int this_id = this_name.toInt(&dummy);

        // This would only happen if make_quality_squelch_buttons was not set up with a proper object name;
        if (!dummy) continue;

        int this_group = this_id / 100;
        int this_setting = this_id % 100;

        if (this_group != quality_group) continue;
        if (this_setting == quality_setting) continue;
        else this_radiobutton->setChecked(FALSE);
    }
}

// Add the quality buttons depending on the player class and the game type
// The function above parses the buttons as the quality_choice is multiplied by 100, then the quality type added.
void QualitySquelchDialog::make_quality_squelch_radiobuttons(QGridLayout *return_layout)
{
    for (int i = 0; i < PS_TYPE_MAX; i++)
    {
        int column = 0;
        bool limited_types = FALSE;
        if (i == PS_TYPE_AMULET) limited_types = TRUE;
        else if (i == PS_TYPE_RING) limited_types = TRUE;

        QPointer<QLabel> quality_name = new QLabel(QString("<b>%1</b>") .arg(quality_choices[i].name));
        quality_name->setAlignment(Qt::AlignCenter);
        return_layout->addWidget(quality_name, i, column++, Qt::AlignLeft);

        QPointer<QRadioButton> quality_none = new QRadioButton(quality_values[SQUELCH_NONE].name);
        quality_none->setToolTip("Do not automatically destroy items of this type based on the quality of that item.");
        quality_none->setObjectName(QString("%1") .arg((i*100) + SQUELCH_NONE));
        return_layout->addWidget(quality_none, i, column++, Qt::AlignLeft);
        quality_none->setAutoExclusive(FALSE);
        if (squelch_level[i] == SQUELCH_NONE)
        {
            quality_none->setChecked(TRUE);
        }
        else quality_none->setChecked(FALSE);
        connect(quality_none, SIGNAL(pressed()), this, SLOT(update_quality_squelch_settings()));

        QPointer<QRadioButton> quality_cursed = new QRadioButton(quality_values[SQUELCH_CURSED].name);
        quality_cursed->setToolTip("Automatically destroy cursed or broken items upon identification or pseudo-id.");
        quality_cursed->setObjectName(QString("%1") .arg((i*100) + SQUELCH_CURSED));
        return_layout->addWidget(quality_cursed, i, column++, Qt::AlignLeft);
        quality_cursed->setAutoExclusive(FALSE);
        if (squelch_level[i] == SQUELCH_CURSED) quality_cursed->setChecked(TRUE);
        else quality_cursed->setChecked(FALSE);
        connect(quality_cursed, SIGNAL(pressed()), this, SLOT(update_quality_squelch_settings()));

        // Jewlery only needs a couple settings
        if (limited_types) continue;


        QPointer<QRadioButton> quality_average = new QRadioButton(quality_values[SQUELCH_AVERAGE].name);
        quality_average->setToolTip("Automatically destroy cursed or average items of this type  upon identification or pseudo-id.");
        quality_average->setObjectName(QString("%1") .arg((i*100) + SQUELCH_AVERAGE));
        return_layout->addWidget(quality_average, i, column++, Qt::AlignLeft);
        quality_average->setAutoExclusive(FALSE);
        if (squelch_level[i] == SQUELCH_AVERAGE) quality_average->setChecked(TRUE);
        else quality_average->setChecked(FALSE);
        connect(quality_average, SIGNAL(pressed()), this, SLOT(update_quality_squelch_settings()));

        QPointer<QRadioButton> quality_good_strong = new QRadioButton(quality_values[SQUELCH_GOOD_STRONG].name);
        if (cp_ptr->pseudo_id_heavy()) quality_good_strong->setToolTip("Automatically destroy cursed, average, or good items of this type upon identification or pseudo-id.");
        else quality_good_strong->setToolTip("Automatically destroy cursed or average item of this type upon identification or pseudo-id. Destroy good items upon identification, but do not destroy items that psuedo-id as good (to prevent an excellent item that pseudo-ids as good from being destroyed).");
        quality_good_strong->setObjectName(QString("%1") .arg((i*100) + SQUELCH_GOOD_STRONG));
        return_layout->addWidget(quality_good_strong, i, column++, Qt::AlignLeft);
        quality_good_strong->setAutoExclusive(FALSE);
        if (squelch_level[i] == SQUELCH_GOOD_STRONG) quality_good_strong->setChecked(TRUE);
        quality_good_strong->setChecked(FALSE);
        connect(quality_good_strong, SIGNAL(pressed()), this, SLOT(update_quality_squelch_settings()));

        if (!cp_ptr->pseudo_id_heavy())
        {
            QPointer<QRadioButton> quality_good_weak = new QRadioButton(quality_values[SQUELCH_GOOD_WEAK].name);
            quality_good_weak->setToolTip("Automatically destroy cursed, average, or good items of this type upon identification or pseudo-id.  Excellent items that pseudo-id as a good item will be destroyed.");
            quality_good_weak->setObjectName(QString("%1") .arg((i*100) + SQUELCH_GOOD_WEAK));
            return_layout->addWidget(quality_good_weak, i, column++, Qt::AlignLeft);
            quality_good_weak->setAutoExclusive(FALSE);
            if (squelch_level[i] == SQUELCH_GOOD_WEAK) quality_good_weak->setChecked(TRUE);
            quality_good_weak->setChecked(FALSE);
            connect(quality_good_weak, SIGNAL(pressed()), this, SLOT(update_quality_squelch_settings()));
        }

        if (i == PS_TYPE_LIGHT) continue;

        if (game_mode != GAME_NPPMORIA)
        {
            QPointer<QRadioButton> quality_all_but_artifact = new QRadioButton(quality_values[SQUELCH_ALL].name);
            quality_all_but_artifact->setToolTip("Automatically destroy all items of this type, except artifacts, upon identification or pseudo-id.");
            quality_all_but_artifact->setObjectName(QString("%1") .arg((i*100) + SQUELCH_ALL));
            return_layout->addWidget(quality_all_but_artifact, i, column++, Qt::AlignLeft);
            quality_all_but_artifact->setAutoExclusive(FALSE);
            if (squelch_level[i] == SQUELCH_ALL) quality_all_but_artifact->setChecked(TRUE);
            connect(quality_all_but_artifact, SIGNAL(pressed()), this, SLOT(update_quality_squelch_settings()));
        }
    }
}

QualitySquelchDialog::QualitySquelchDialog(void): NPPDialog()
{
    central = new QWidget;
    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    // IMPORTANT: it must be called AFTER setting the layout
    this->setClient(central);

    QPointer<QLabel> quality_header = new QLabel("<b><h2>Quality Squelch Menu</h2></b>");
    quality_header->setToolTip("The settings below allow the player to automatically destroy an item on identification, or pseudo-id, based on the quality of that item.");
    main_layout->addWidget(quality_header, Qt::AlignCenter);

    QPointer<QHBoxLayout> squelch_hlay = new QHBoxLayout;
    main_layout->addLayout(squelch_hlay);
    make_quality_squelch_pushbuttons(squelch_hlay);

    QPointer<QGridLayout> squelch_glay = new QGridLayout;
    main_layout->addLayout(squelch_glay);

    make_quality_squelch_radiobuttons(squelch_glay);

    main_layout->addStretch(1);

    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    main_layout->addWidget(buttons);

    setWindowTitle(tr("Quality Squelch Menu"));

    this->clientSizeUpdated();

    this->exec();
}

void do_quality_squelch_menu()
{
    QualitySquelchDialog();
}
