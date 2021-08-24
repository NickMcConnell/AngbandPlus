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

void MainWindow::equip_button_click()
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

    // We aren't repeating the previous command
    p_ptr->player_previous_command_wipe();

    process_command(item_num, command_num);
    p_ptr->message_append_stop();

    win_char_equipment_update();
}

void MainWindow::toggle_equip_show_buttons()
{
    if (!equip_show_buttons)
    {
        equip_show_buttons = TRUE;
        char_equipment_buttons->setText("Hide Command Buttons");
    }
    else
    {
        equip_show_buttons = FALSE;
        char_equipment_buttons->setText("Show Command Buttons");
    }
    win_char_equipment_update();
}

void MainWindow::update_label_equipment_font()
{
    QList<QLabel *> lbl_list = window_char_equipment->findChildren<QLabel *>();
    for (int i = 0; i < lbl_list.size(); i++)
    {
        QLabel *this_lbl = lbl_list.at(i);
        this_lbl->setFont(font_char_equipment);
    }

    // Now make the buttons about the same size
    QFontMetrics metrics(font_char_equipment);
    QSize button_size((metrics.width('M') + 2), (metrics.height() + 2));
    QList<QPushButton *> pushbutton_list = window_char_equipment->findChildren<QPushButton *>();
    for (int i = 0; i < pushbutton_list.size(); i++)
    {
        QPushButton *this_button = pushbutton_list.at(i);
        if (!this_button->objectName().length()) continue;
        this_button->setIconSize(button_size);
    }
}

void MainWindow::set_font_char_equipment(QFont newFont)
{
    font_char_equipment = newFont;
    update_label_equipment_font();

}

void MainWindow::win_char_equipment_font()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, font_char_equipment, this);

    if (selected)
    {
        set_font_char_equipment(font);
    }
}

void MainWindow::equip_link_pushbuttons()
{
    QList<QPushButton *> pushbutton_list = window_char_equipment->findChildren<QPushButton *>();

    for (int i = 0; i < pushbutton_list.size(); i++)
    {
        QPushButton *this_button = pushbutton_list.at(i);
        if (!this_button->objectName().length()) continue;
        connect(this_button, SIGNAL(pressed()), this, SLOT(equip_button_click()));
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_char_equipment_wipe()
{
    if (!show_char_equipment) return;
    if (!character_generated) return;
    clear_layout(main_vlay_equipment);
}


void MainWindow::win_char_equipment_update()
{
    if (!character_generated) return;
    if (!show_char_equipment) return;

    update_equip_list(equip_list, FALSE, equip_show_buttons);
    update_quiver_list(quiver_list, FALSE, equip_show_buttons);
    if (equip_show_buttons) equip_link_pushbuttons();
    update_label_equipment_font();
}

void MainWindow::create_win_char_equipment()
{
    if (!character_generated) return;
    if (!show_char_equipment) return;

    // Add the equipment
    QPointer<QVBoxLayout> equip_vlay = new QVBoxLayout;
    main_vlay_equipment->addLayout(equip_vlay);
    QPointer<QLabel> header_equip = new QLabel(QString("<b><h1>Equipment</b></h1>"));
    equip_vlay->addWidget(header_equip, Qt::AlignCenter);
    equip_list = new QGridLayout;

    equip_vlay->addLayout(equip_list);

    // Add a space
    QPointer<QLabel> empty_space = new QLabel;
    empty_space->setText(" ");
    main_vlay_equipment->addWidget(empty_space);

    // Add the quiver
    QPointer<QVBoxLayout> quiver_vlay = new QVBoxLayout;
    main_vlay_equipment->addLayout(quiver_vlay);
    QPointer<QLabel> header_quiver = new QLabel(QString("<b><h1>Quiver</b></h1>"));
    quiver_vlay->addWidget(header_quiver, Qt::AlignCenter);
    quiver_list = new QGridLayout;
    quiver_vlay->addLayout(quiver_list);

    win_char_equipment_update();
    main_vlay_equipment->addStretch(1);
}

void MainWindow::close_win_char_equipment_frame(QObject *this_object)
{
    (void)this_object;
    window_char_equipment = NULL;
    show_char_equipment = FALSE;
    win_char_equipment->setText("Show Character Equipment Screen");
}


/*
 *  Make the equip shell
 *  The game crashes if the labels are drawn before the character is created
 *  So that is filled after a character is created.
 */
void MainWindow::win_char_equipment_create()
{
    window_char_equipment = new QWidget();
    main_vlay_equipment = new QVBoxLayout;
    window_char_equipment->setLayout(main_vlay_equipment);

    char_equipment_menubar = new QMenuBar;
    main_vlay_equipment->setMenuBar(char_equipment_menubar);
    window_char_equipment->setWindowTitle("Character Equipment Screen");
    char_equipment_settings = char_equipment_menubar->addMenu(tr("&Settings"));
    char_equipment_font = new QAction(tr("Set Equipment Screen Font"), this);
    char_equipment_font->setStatusTip(tr("Set the font for the Equipment screen."));
    connect(char_equipment_font, SIGNAL(triggered()), this, SLOT(win_char_equipment_font()));
    char_equipment_settings->addAction(char_equipment_font);
    char_equipment_buttons = new QAction(tr("Show Command Buttons"), this);
    if (equip_show_buttons) char_equipment_buttons->setText("Hide Command Buttons");
    char_equipment_buttons->setStatusTip(tr("Displays or hides the command buttons."));
    connect(char_equipment_buttons, SIGNAL(triggered()), this, SLOT(toggle_equip_show_buttons()));
    char_equipment_settings->addAction(char_equipment_buttons);
    window_char_equipment->setAttribute(Qt::WA_DeleteOnClose);
    connect(window_char_equipment, SIGNAL(destroyed(QObject*)), this, SLOT(close_win_char_equipment_frame(QObject*)));
}

void MainWindow::win_char_equipment_destroy()
{
    if (!show_char_equipment) return;
    delete window_char_equipment;
    window_char_equipment = NULL;
}

void MainWindow::toggle_win_char_equipment_frame()
{
    if (!show_char_equipment)
    {
        win_char_equipment_create();
        show_char_equipment = TRUE;
        create_win_char_equipment();
        win_char_equipment->setText("Hide Character Equipment Screen");
        window_char_equipment->show();
    }
    else

    {
        win_char_equipment_destroy();
        show_char_equipment = FALSE;
        win_char_equipment->setText("Show Character Equipment Screen");
    }
}


