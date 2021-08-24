/*
 * Copyright (c) 2015 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/npp.h>
#include <src/qt_mainwindow.h>
#include <src/player_command.h>
#include <QHeaderView>
#include <QFontDialog>
#include <QPushButton>

void MainWindow::inven_button_click()
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
        p_ptr->message_append_stop();
        return;
    }

    p_ptr->message_append_start();

    // We aren't repeating the previous command
    p_ptr->player_previous_command_wipe();

    process_command(item_num, command_num);
    p_ptr->message_append_stop();

    win_char_inventory_update();
}

void MainWindow::toggle_inven_show_buttons()
{
    if (!inven_show_buttons)
    {
        inven_show_buttons = TRUE;
        char_inventory_buttons->setText("Hide Command Buttons");
    }
    else
    {
        inven_show_buttons = FALSE;
        char_inventory_buttons->setText("Show Command Buttons");
    }
    win_char_inventory_update();
}

void MainWindow::update_label_inventory_font()
{
    QList<QLabel *> lbl_list = window_char_inventory->findChildren<QLabel *>();
    for (int i = 0; i < lbl_list.size(); i++)
    {
        QLabel *this_lbl = lbl_list.at(i);
        this_lbl->setFont(font_char_inventory);
    }

    // Now make the buttons about the same size
    QFontMetrics metrics(font_char_inventory);
    QSize button_size((metrics.width('M') + 2), (metrics.height() + 2));
    QList<QPushButton *> pushbutton_list = window_char_inventory->findChildren<QPushButton *>();
    for (int i = 0; i < pushbutton_list.size(); i++)
    {
        QPushButton *this_button = pushbutton_list.at(i);
        if (!this_button->objectName().length()) continue;
        this_button->setIconSize(button_size);
    }
}

void MainWindow::set_font_char_inventory(QFont newFont)
{
    font_char_inventory = newFont;
    update_label_inventory_font();

}

void MainWindow::win_char_inventory_font()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, font_char_inventory, this);

    if (selected)
    {
        set_font_char_inventory(font);
    }
}

void MainWindow::inven_link_pushbuttons()
{
    QList<QPushButton *> pushbutton_list = window_char_inventory->findChildren<QPushButton *>();

    for (int i = 0; i < pushbutton_list.size(); i++)
    {
        QPushButton *this_button = pushbutton_list.at(i);
        if (!this_button->objectName().length()) continue;
        connect(this_button, SIGNAL(pressed()), this, SLOT(inven_button_click()));
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_char_inventory_wipe()
{
    if (!show_char_inventory) return;
    if (!character_generated) return;
    clear_layout(main_vlay_inventory);
}


void MainWindow::win_char_inventory_update()
{
    if (!character_generated) return;
    if (!show_char_inventory) return;

    update_inven_list(inven_list, TRUE, inven_show_buttons);
    if (inven_show_buttons) inven_link_pushbuttons();
    update_label_inventory_font();
}

void MainWindow::create_win_char_inventory()
{
    if (!character_generated) return;
    if (!show_char_inventory) return;

    // Add the inventory
    QPointer<QLabel> header_inven = new QLabel(QString("<b><h1>Inventory</b></h1>"));
    main_vlay_inventory->addWidget(header_inven, Qt::AlignCenter);
    inven_list = new QGridLayout;
    main_vlay_inventory->addLayout(inven_list);
    // I have no idea why a stretch of 1 doesn't work here.
    main_vlay_inventory->addStretch(1000);

    win_char_inventory_update();
}

void MainWindow::close_win_char_inventory_frame(QObject *this_object)
{
    (void)this_object;
    window_char_inventory = NULL;
    show_char_inventory = FALSE;
    win_char_inventory->setText("Show Character Inventory Screen");
}


/*
 *  Make the inven shell
 *  The game crashes if the labels are drawn before the character is created
 *  So that is filled after a character is created.
 */
void MainWindow::win_char_inventory_create()
{
    window_char_inventory = new QWidget();
    main_vlay_inventory = new QVBoxLayout;
    window_char_inventory->setLayout(main_vlay_inventory);

    char_inventory_menubar = new QMenuBar;
    main_vlay_inventory->setMenuBar(char_inventory_menubar);
    window_char_inventory->setWindowTitle("Character Inventory Screen");
    char_inventory_settings = char_inventory_menubar->addMenu(tr("&Settings"));
    char_inventory_font = new QAction(tr("Set Inventory Screen Font"), this);
    char_inventory_font->setStatusTip(tr("Set the font for the Inventory screen."));
    connect(char_inventory_font, SIGNAL(triggered()), this, SLOT(win_char_inventory_font()));
    char_inventory_settings->addAction(char_inventory_font);
    char_inventory_buttons = new QAction(tr("Show Command Buttons"), this);
    if (inven_show_buttons) char_inventory_buttons->setText("Hide Command Buttons");
    char_inventory_buttons->setStatusTip(tr("Displays or hides the command buttons."));
    connect(char_inventory_buttons, SIGNAL(triggered()), this, SLOT(toggle_inven_show_buttons()));
    char_inventory_settings->addAction(char_inventory_buttons);

    window_char_inventory->setAttribute(Qt::WA_DeleteOnClose);
    connect(window_char_inventory, SIGNAL(destroyed(QObject*)), this, SLOT(close_win_char_inventory_frame(QObject*)));
}

void MainWindow::win_char_inventory_destroy()
{
    if (!show_char_inventory) return;
    if (!window_char_inventory) return;
    delete window_char_inventory;
    window_char_inventory = NULL;
}

void MainWindow::toggle_win_char_inventory_frame()
{
    if (!show_char_inventory)
    {
        win_char_inventory_create();
        show_char_inventory = TRUE;
        create_win_char_inventory();
        win_char_inventory->setText("Hide Character Inventory Screen");
        window_char_inventory->show();
    }
    else

    {
        win_char_inventory_destroy();
        show_char_inventory = FALSE;
        win_char_inventory->setText("Show Character Inventory Screen");
    }
}



