
/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/npp.h>
#include <src/object_all_menu.h>
#include <src/object_settings.h>
#include <src/messages.h>
#include <src/player_command.h>
#include <QButtonGroup>
#include <QKeyEvent>
#include <QDialogButtonBox>
#include <QPushButton>


/*
 *
 *
 * OBJECTS DIALOG
 *
 *
 */

void AllObjectsDialog::button_click()
{
    QString item_id = QObject::sender()->objectName();

    bool ok;

    int splitter = item_id.indexOf("_");

    QString command_string = item_id;

    command_string.truncate(splitter);
    item_id.remove(0, splitter+1);

    int item_num = item_id.toInt(&ok, 10);
    // Paranoia
    if (!ok) return;
    int command_num = command_string.toInt(&ok, 10);
    // Paranoia
    if (!ok) return;

    // Hack = Special handling for object settings
    if (command_num == CMD_SETTINGS)
    {
        object_settings(item_num);
        return;
    }

    p_ptr->message_append_start();

    // Close this dialog
    if ((command_num == CMD_FIRE) || (command_num == CMD_THROW) ||
        (command_num == CMD_CAST))
    {
        this->close_dialog();
    }

    else if (command_num == CMD_ITEM_USE)
    {
        if (obj_needs_aim(object_from_item_idx(item_num))) this->close_dialog();
    }

    // We aren't repeating the previous command
    p_ptr->player_previous_command_wipe();

    process_command(item_num, command_num);
    p_ptr->message_append_stop();

    update_dialog();
}

void AllObjectsDialog::move_left()
{
    if (current_list == TAB_FLOOR)
    {
        if (allow_equip || allow_quiver) current_list = TAB_EQUIP;
        else if (allow_inven) current_list = TAB_INVEN;

    }
    else if (current_list == TAB_INVEN)
    {
        if (allow_floor) current_list = TAB_FLOOR;
        else if (allow_equip || allow_quiver) current_list = TAB_EQUIP;

    }
    else if (current_list == TAB_EQUIP)
    {
        if (allow_inven) current_list = TAB_INVEN;
        else if (allow_floor) current_list = TAB_FLOOR;

    }
    update_dialog();
}

void AllObjectsDialog::switch_lists(int new_list)
{
    current_list = new_list;
    update_dialog();
}

void AllObjectsDialog::move_right()
{
    if (current_list == TAB_FLOOR)
    {
        if (allow_inven) current_list = TAB_INVEN;
        else if (allow_equip || allow_quiver) current_list = TAB_EQUIP;

    }
    else if (current_list == TAB_INVEN)
    {
        if (allow_equip || allow_quiver) current_list = TAB_EQUIP;
        else if (allow_floor) current_list = TAB_FLOOR;

    }
    else if (current_list == TAB_EQUIP)
    {
        if (allow_floor) current_list = TAB_FLOOR;
        else if (allow_inven) current_list = TAB_INVEN;
    }
    update_dialog();
}


// See if the user selected a button bia a keypress.
void AllObjectsDialog::keyPressEvent(QKeyEvent* which_key)
{
    // Handle escape key
    if (which_key->key() == Qt::Key_Escape)
    {
        this->close();
        return;
    }

    if (which_key->key() == Qt::Key_Less)
    {
        move_left();
        return;
    }

    if (which_key->key() == Qt::Key_Greater)
    {
        move_right();
        return;
    }
}

void AllObjectsDialog::update_header()
{
    //max capactity in ounces
    u16b max_capacity = normal_speed_weight_limit();

    u32b weight_percent = p_ptr->total_weight * 100 / max_capacity;

    header_weight1->setText((QString("<b><big>Burden: %1 lbs (%2% capacity)</big></b>")
                             .arg(formatted_weight_string(p_ptr->total_weight)) .arg(weight_percent)));
    if (p_ptr->total_weight > max_capacity)
    {
        int overweight = p_ptr->total_weight - max_capacity;
        header_weight2->setText(QString("(%1 lbs overweight)")
                        .arg((formatted_weight_string(overweight))));
    }
    else if (p_ptr->total_weight < max_capacity)
    {
        int underweight = max_capacity - p_ptr->total_weight;
        header_weight2->setText(QString("(%1 lbs underweight)")
                        .arg(formatted_weight_string(underweight)));
    }
}


// Confirm which tabs should be displayed.
void AllObjectsDialog::confirm_tabs(bool status_check)
{
    allow_floor = FALSE;
    allow_inven = FALSE;
    allow_equip = FALSE;
    allow_quiver = FALSE;

    // Confirm there is a floor item
    if (dungeon_info[p_ptr->py][p_ptr->px].has_object()) allow_floor = TRUE;

    /* Confirm there are objects in the inventory */
    for (int i = 0; i < (INVEN_WIELD - 1); i++)
    {
        /* Get the object */
        object_type *o_ptr = &inventory[i];

        /* Skip empty slots */
        if (!o_ptr->k_idx) continue;

        allow_inven = TRUE;
        break;
    }

    /* Confirm the player is wielding equipment */
    for (int i = INVEN_WIELD; i < QUIVER_START; i++)
    {
        /* Get the object */
        object_type *o_ptr = &inventory[i];

        /* Skip empty slots */
        if (!o_ptr->k_idx) continue;

        allow_equip = TRUE;
        break;
    }

    /* Scan all objects in the quiver */
    for (int i = QUIVER_START; i < QUIVER_END; i++)
    {
        /* Get the object */
        object_type *o_ptr = &inventory[i];

        /* Skip empty slots */
        if (!o_ptr->k_idx) continue;

        allow_quiver = TRUE;
        break;
    }

    // We are just checking the status, not handling the dialog
    if (status_check) return;

    if (no_objects())
    {
        //Paranoia
        close_dialog();
        return;
    }

    // Update the radiobuttons
    if (allow_floor) floor_items->show();
    else   floor_items->hide();

    if (allow_inven) inven_items->show();
    else   inven_items->hide();

    if (allow_equip || allow_quiver) equip_items->show();
    else   equip_items->hide();

    if ((current_list == TAB_FLOOR) && !allow_floor)
    {
        if (allow_inven) current_list = TAB_INVEN;
        else /* allow_ equip || allow_quiver */ current_list = TAB_EQUIP;
    }
    else if (current_list == TAB_INVEN && !allow_inven)
    {
        if (allow_equip) current_list = TAB_EQUIP;
        else /* allow_floor */ current_list = TAB_FLOOR;
    }

    else if (current_list == TAB_EQUIP && !allow_equip)
    {
        if (allow_inven) current_list = TAB_INVEN;
        else /* allow_floor */ current_list = TAB_FLOOR;
    }

    if (current_list == TAB_FLOOR) floor_items->setChecked(TRUE);
    if (current_list == TAB_INVEN) inven_items->setChecked(TRUE);
    if (current_list == TAB_EQUIP) equip_items->setChecked(TRUE);
}



bool AllObjectsDialog::no_objects()
{
    if (allow_floor) return (FALSE);
    if (allow_inven) return (FALSE);
    if (allow_equip) return (FALSE);
    if (allow_quiver) return (FALSE);
    return (TRUE);
}


void AllObjectsDialog::close_dialog()
{
    p_ptr->message_append_stop();
    this->reject();
}

void AllObjectsDialog::link_pushbuttons()
{
    QList<QPushButton *> pushbutton_list = this->findChildren<QPushButton *>();

    for (int i = 0; i < pushbutton_list.size(); i++)
    {
        QPushButton *this_button = pushbutton_list.at(i);

        // Just the objects
        if (!this_button->objectName().length()) continue;

        connect(this_button, SIGNAL(pressed()), this, SLOT(button_click()));
    }
}

void AllObjectsDialog::update_dialog()
{
    update_header();
    confirm_tabs(FALSE);

    if (no_objects())
    {
        close_dialog();
        return;
    }

    p_ptr->message_append_stop();

    //clear the quiver layout
    clear_layout(quiver_list);

    if (current_list == TAB_FLOOR)
    {
        header_objects->setText("<br><b><h3>Floor Items</b></h3>");
        update_floor_list(object_list, FALSE, TRUE);
        quiver_header->hide();
    }
    else if (current_list == TAB_INVEN)
    {
        header_objects->setText("<br><b><h3>Inventory</b></h3>");
        update_inven_list(object_list, FALSE, TRUE);
        quiver_header->hide();
    }
    else if (current_list == TAB_EQUIP)
    {
        header_objects->setText("<br><b><h3>Equipment</b></h3>");
        update_equip_list(object_list, FALSE, TRUE);
        if (allow_quiver)
        {
            quiver_header->show();
            update_quiver_list(quiver_list, FALSE, TRUE);
        }
        else quiver_header->hide();
    }

    link_pushbuttons();
    update_message_area(message_area, 3);
}





AllObjectsDialog::AllObjectsDialog(bool do_buttons, int start_screen): NPPDialog()
{
    (void)do_buttons;
    confirm_tabs(TRUE);

    object_selection = new QButtonGroup(this);
    object_selection->setExclusive(TRUE);

    current_list = start_screen;

    // Handle no available objects.
    if (no_objects())
    {
        pop_up_message_box("You have no objects to manage");
        return;
    }

    // Set up a scrollable box.  Add widgets to "main_layout"
    central = new QWidget;
    QVBoxLayout *main_layout = new QVBoxLayout;
    central->setLayout(main_layout);
    main_layout->setSpacing(10);
    // IMPORTANT: it must be called AFTER setting the layout
    this->setClient(central);

    //Build the header
    header_main = new QLabel("<b><h2>Object Menu</b></h2>");
    header_weight1 = new QLabel;
    header_weight2 = new QLabel;
    main_layout->addWidget(header_main);
    main_layout->addWidget(header_weight1);
    main_layout->addWidget(header_weight2);
    update_header();

    // add the message area
    message_area = new QLabel;
    message_area->setWordWrap(TRUE);
    message_area->setAutoFillBackground(TRUE);
    QPalette this_palette;
    this_palette.setColor(QPalette::Window, QColor(Qt::black));
    message_area->setPalette(this_palette);
    main_layout->addWidget(message_area);
    update_message_area(message_area, 3);

    QHBoxLayout *radio_across = new QHBoxLayout;
    main_layout->addLayout(radio_across);
    floor_items = new QRadioButton("Floor Items");
    object_selection->addButton(floor_items, TAB_FLOOR);
    inven_items = new QRadioButton("Inventory");
    object_selection->addButton(inven_items, TAB_INVEN);
    equip_items = new QRadioButton("Equipment");
    object_selection->addButton(equip_items, TAB_EQUIP);
    connect(object_selection, SIGNAL(buttonClicked(int)), this, SLOT(switch_lists(int)));
    radio_across->addWidget(floor_items);
    radio_across->addWidget(inven_items);
    radio_across->addWidget(equip_items);
    radio_across->addStretch(1);

    header_objects = new QLabel("<br><b><h3>Inventory</b></h3>");
    main_layout->addWidget(header_objects);

    // Add the list for the items
    object_list = new QGridLayout;
    main_layout->addLayout(object_list);


    // Hack - special layout and for the quiver
    quiver_header = new QLabel("<br><br><b><h3>Quiver</b></h3>");
    main_layout->addWidget(quiver_header);
    quiver_list = new QGridLayout;
    main_layout->addLayout(quiver_list);

    QDialogButtonBox *buttons = new QDialogButtonBox();
    QPushButton *button_left = new QPushButton();
    button_left->setText("<");
    button_left->setToolTip("Pressing '<' also moves the active tab to the left.");
    connect(button_left, SIGNAL(clicked()), this, SLOT(move_left()));
    buttons->addButton(button_left, QDialogButtonBox::ActionRole);
    QPushButton *button_right = new QPushButton();
    button_right->setText(">");
    button_right->setToolTip("Pressing '>' also moves the active tab to the right.");
    connect(button_right, SIGNAL(clicked()), this, SLOT(move_right()));
    buttons->addButton(button_right, QDialogButtonBox::ActionRole);
    buttons->addButton(QDialogButtonBox::Close);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));

    main_layout->addWidget(buttons);

    update_dialog();

    main_layout->addStretch(1);

    setWindowTitle(tr("Object Menu"));

    this->clientSizeUpdated();

    this->exec_saved("AllObjects");
}


void do_cmd_all_objects(int start_screen)
{
    AllObjectsDialog(TRUE, start_screen);
    p_ptr->message_append_stop();
}



