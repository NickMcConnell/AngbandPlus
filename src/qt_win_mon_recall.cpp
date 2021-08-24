/*
 * Copyright (c) 2015 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/npp.h>
#include <src/qt_mainwindow.h>
#include <QHeaderView>
#include <QFontDialog>

void MainWindow::set_font_win_mon_recall(QFont newFont)
{
    win_mon_recall_settings.win_font = newFont;
    win_mon_recall_update();
}

void MainWindow::win_mon_recall_font()
{
    bool selected;
    QFont font = QFontDialog::getFont(&selected, win_mon_recall_settings.win_font, this );

    if (selected)
    {
        set_font_win_mon_recall(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_mon_recall_wipe()
{
    if (!win_mon_recall_settings.win_show) return;
    if (!character_generated) return;

    mon_recall_area->clear();

}

void MainWindow::win_mon_recall_update()
{
    win_mon_recall_wipe();
    if (!win_mon_recall_settings.win_show) return;
    if (!character_generated) return;
    if (!p_ptr->monster_race_idx) return;


    mon_recall_area->setFont(win_mon_recall_settings.win_font);
    mon_recall_area->moveCursor(QTextCursor::Start);
    QString mon_recall = get_monster_description(p_ptr->monster_race_idx, FALSE, NULL, TRUE);
    mon_recall_area->insertHtml(mon_recall);
}


/*
 *  Show widget is called after this to allow
 * the settings to restore the save geometry.
 */
void MainWindow::win_mon_recall_create()
{
    win_mon_recall_settings.make_extra_window();

    mon_recall_area = new QTextEdit;
    mon_recall_area->setReadOnly(TRUE);
    mon_recall_area->setStyleSheet("background-color: lightGray;");
    mon_recall_area->setTextInteractionFlags(Qt::NoTextInteraction);
    win_mon_recall_settings.main_vlay->addWidget(mon_recall_area);
    win_mon_recall_settings.main_widget->setWindowTitle("Monster Recall Window");
    connect(win_mon_recall_settings.win_font_act, SIGNAL(triggered()), this, SLOT(win_mon_recall_font()));

    connect(win_mon_recall_settings.main_widget, SIGNAL(destroyed(QObject*)), this, SLOT(win_mon_recall_destroy(QObject*)));
}

/*
 * win_mon_recall_close should be used when the game is shutting down.
 * Use this function for closing the window mid-game
 */
void MainWindow::win_mon_recall_destroy(QObject *this_object)
{
    (void)this_object;
    if (!win_mon_recall_settings.win_show) return;
    if (!win_mon_recall_settings.main_widget) return;
    win_mon_recall_settings.get_widget_settings(win_mon_recall_settings.main_widget);
    win_mon_recall_settings.main_widget->deleteLater();
    win_mon_recall_settings.win_show = FALSE;
    win_mon_recall_act->setText("Show Monster Recall Window");
}

/*
 * This version should only be used when the game is shutting down.
 * So it is remembered if the window was open or not.
 * For closing the window mid-game use win_mon_list_destroy directly
 */
void MainWindow::win_mon_recall_close()
{
    bool was_open = win_mon_recall_settings.win_show;
    win_mon_recall_destroy(win_mon_recall_settings.main_widget);
    win_mon_recall_settings.win_show = was_open;
}


void MainWindow::toggle_win_mon_recall()
{
    if (!win_mon_recall_settings.win_show)
    {
        win_mon_recall_create();
        win_mon_recall_settings.win_show = TRUE;
        win_mon_recall_settings.main_widget->setGeometry(win_mon_recall_settings.win_geometry);
        win_mon_recall_act->setText("Hide Monster Recall Window");
        if (win_mon_recall_settings.win_maximized) win_mon_recall_settings.main_widget->showMaximized();
        else win_mon_recall_settings.main_widget->show();
        win_mon_recall_update();
    }
    else win_mon_recall_destroy(win_mon_recall_settings.main_widget);
}
