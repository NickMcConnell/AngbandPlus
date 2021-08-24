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
        char_equipment_buttons_act->setText("Hide Command Buttons");
    }
    else
    {
        equip_show_buttons = FALSE;
        char_equipment_buttons_act->setText("Show Command Buttons");
    }
    win_char_equipment_update();
}

void MainWindow::update_label_equipment_font()
{
    QList<QLabel *> lbl_list = char_equipment_settings.main_widget->findChildren<QLabel *>();
    for (int i = 0; i < lbl_list.size(); i++)
    {
        QLabel *this_lbl = lbl_list.at(i);
        this_lbl->setFont(char_equipment_settings.win_font);
    }

    // Now make the buttons about the same size
    QFontMetrics metrics(char_equipment_settings.win_font);
    QSize button_size((metrics.width('M') + 2), (metrics.height() + 2));
    QList<QPushButton *> pushbutton_list = char_equipment_settings.main_widget->findChildren<QPushButton *>();
    for (int i = 0; i < pushbutton_list.size(); i++)
    {
        QPushButton *this_button = pushbutton_list.at(i);
        if (!this_button->objectName().length()) continue;
        this_button->setIconSize(button_size);
    }
}

void MainWindow::set_font_char_equipment(QFont newFont)
{
    char_equipment_settings.win_font = newFont;
    update_label_equipment_font();

}

void MainWindow::win_char_equipment_font()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, char_equipment_settings.win_font, this);

    if (selected)
    {
        set_font_char_equipment(font);
    }
}

void MainWindow::equip_link_pushbuttons()
{
    QList<QPushButton *> pushbutton_list = char_equipment_settings.main_widget->findChildren<QPushButton *>();

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
    if (!char_equipment_settings.win_show) return;
    if (!character_generated) return;
    clear_layout(char_equipment_settings.main_vlay);
}


void MainWindow::win_char_equipment_update()
{
    if (!character_generated) return;
    if (!char_equipment_settings.win_show) return;

    update_equip_list(equip_list, FALSE, equip_show_buttons);
    update_quiver_list(quiver_list, FALSE, equip_show_buttons);
    if (equip_show_buttons) equip_link_pushbuttons();
    update_label_equipment_font();
}

void MainWindow::create_win_char_equipment()
{
    if (!character_generated) return;
    if (!char_equipment_settings.win_show) return;

    // Add the equipment
    QPointer<QVBoxLayout> equip_vlay = new QVBoxLayout;
    char_equipment_settings.main_vlay->addLayout(equip_vlay);
    QPointer<QLabel> header_equip = new QLabel(QString("<b><h1>Equipment</b></h1>"));
    equip_vlay->addWidget(header_equip, Qt::AlignCenter);
    equip_list = new QGridLayout;

    equip_vlay->addLayout(equip_list);

    // Add a space
    QPointer<QLabel> empty_space = new QLabel;
    empty_space->setText(" ");
    char_equipment_settings.main_vlay->addWidget(empty_space);

    // Add the quiver
    QPointer<QVBoxLayout> quiver_vlay = new QVBoxLayout;
    char_equipment_settings.main_vlay->addLayout(quiver_vlay);
    QPointer<QLabel> header_quiver = new QLabel(QString("<b><h1>Quiver</b></h1>"));
    quiver_vlay->addWidget(header_quiver, Qt::AlignCenter);
    quiver_list = new QGridLayout;
    quiver_vlay->addLayout(quiver_list);

    win_char_equipment_update();
    char_equipment_settings.main_vlay->addStretch(1);
}


/*
 *  Make the equip shell
 *  The game crashes if the labels are drawn before the character is created
 *  So that is filled after a character is created.
 */
void MainWindow::win_char_equipment_create()
{
    char_equipment_settings.make_extra_window();

    char_equipment_settings.main_widget->setWindowTitle("Character Equipment Screen");
    connect(char_equipment_settings.win_font_act, SIGNAL(triggered()), this, SLOT(win_char_equipment_font()));
    char_equipment_buttons_act = new QAction(tr("Show Command Buttons"), this);
    if (equip_show_buttons) char_equipment_buttons_act->setText("Hide Command Buttons");
    char_equipment_buttons_act->setStatusTip(tr("Displays or hides the command buttons."));
    connect(char_equipment_buttons_act, SIGNAL(triggered()), this, SLOT(toggle_equip_show_buttons()));
    char_equipment_settings.win_menu->addAction(char_equipment_buttons_act);

    connect(char_equipment_settings.main_widget, SIGNAL(destroyed(QObject*)), this, SLOT(win_char_equipment_destroy(QObject*)));
}

/*
 * This version should only be used when the game is shutting down.
 * So it is remembered if the window was open or not.
 * For closing the window mid-game use win_char_inventory_destroy directly
 */
void MainWindow::win_char_equipment_destroy(QObject *this_object)
{
    (void)this_object;
    if (!char_equipment_settings.win_show) return;
    if (!char_equipment_settings.main_widget) return;
    char_equipment_settings.get_widget_settings(char_equipment_settings.main_widget);
    char_equipment_settings.main_widget->deleteLater();
    char_equipment_settings.win_show = FALSE;
    win_char_equipment_act->setText("Show Character Equipment Screen");
}

/*
 * This version should only be used when the game is shutting down.
 * So it is remembered if the window was open or not.
 * For closing the window mid-game use win_char_equipment_destroy directly
 */
void MainWindow::win_char_equipment_close()
{
    bool was_open = char_equipment_settings.win_show;
    win_char_equipment_destroy(char_equipment_settings.main_widget);
    char_equipment_settings.win_show = was_open;
}

void MainWindow::toggle_win_char_equipment_frame()
{
    if (!char_equipment_settings.win_show)
    {
        win_char_equipment_create();
        char_equipment_settings.win_show = TRUE;
        create_win_char_equipment();
        char_equipment_settings.main_widget->setGeometry(char_equipment_settings.win_geometry);
        win_char_equipment_act->setText("Hide Character Equipment Screen");
        if (char_equipment_settings.win_maximized) char_equipment_settings.main_widget->showMaximized();
        else char_equipment_settings.main_widget->show();
    }
    else win_char_equipment_destroy(char_equipment_settings.main_widget);
}


