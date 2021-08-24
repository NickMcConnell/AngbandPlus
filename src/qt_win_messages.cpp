/*
 * Copyright (c) 2015 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/npp.h>
#include <src/qt_mainwindow.h>
#include <src/messages.h>
#include <QHeaderView>
#include <QFontDialog>

void MainWindow::set_font_win_messages(QFont newFont)
{
    win_message_settings.win_font = newFont;
    win_messages_update();
}

void MainWindow::win_messages_font()
{
    bool selected;
    QFont font = QFontDialog::getFont(&selected, win_message_settings.win_font, this );

    if (selected)
    {
        set_font_win_messages(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_messages_wipe()
{
    if (!win_message_settings.win_show) return;
    if (!character_generated) return;

    win_messages_area->clear();

}

void MainWindow::win_messages_update()
{
    if (!win_message_settings.win_show) return;
    if (!character_generated) return;
    update_message_window(win_messages_area, win_message_settings.win_font);
}



/*
 *  Show widget is called after this to allow
 * the settings to restore the save geometry.
 */
void MainWindow::win_messages_create()
{
    win_message_settings.make_extra_window();

    win_messages_area = new QTextEdit;
    win_messages_area->setReadOnly(TRUE);
    win_messages_area->setStyleSheet("background-color: black;");
    win_messages_area->setTextInteractionFlags(Qt::TextSelectableByMouse);
    win_message_settings.main_vlay->addWidget(win_messages_area);

    win_message_settings.main_widget->setWindowTitle("Messages Window");

    connect(win_message_settings.win_font_act, SIGNAL(triggered()), this, SLOT(win_messages_font()));

    connect(win_message_settings.main_widget, SIGNAL(destroyed(QObject*)), this, SLOT(win_messages_destroy(QObject*)));

    reset_message_display_marks();
}

void MainWindow::win_messages_destroy(QObject *this_object)
{
    (void)this_object;
    if (!win_message_settings.win_show) return;
    if (!win_message_settings.main_widget) return;
    win_message_settings.get_widget_settings(win_message_settings.main_widget);
    win_message_settings.main_widget->deleteLater();
    win_message_settings.win_show = FALSE;
    win_messages_act->setText("Show Message Display Window");

    reset_message_display_marks();
}

void MainWindow::win_messages_close()
{
    bool was_open = win_message_settings.win_show;
    win_messages_destroy(win_message_settings.main_widget);
    win_message_settings.win_show = was_open;
}

void MainWindow::toggle_win_messages()
{
    if (!win_message_settings.win_show)
    {
        win_messages_create();
        win_message_settings.win_show = TRUE;
        win_message_settings.main_widget->setGeometry(win_message_settings.win_geometry);
        win_messages_act->setText("Hide Message Display Window");
        if (win_message_settings.win_maximized) win_message_settings.main_widget->showMaximized();
        else win_message_settings.main_widget->show();
        win_messages_update();
    }
    else win_messages_destroy(win_message_settings.main_widget);
}

