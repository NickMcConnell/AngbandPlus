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
    font_win_mon_recall = newFont;
    win_mon_recall_update();
}

void MainWindow::win_mon_recall_font()
{
    bool selected;
    QFont font = QFontDialog::getFont(&selected, font_win_mon_recall, this );

    if (selected)
    {
        set_font_win_mon_recall(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_mon_recall_wipe()
{
    if (!show_mon_recall) return;
    if (!character_generated) return;

    mon_recall_area->clear();

}

void MainWindow::win_mon_recall_update()
{
    win_mon_recall_wipe();
    if (!show_mon_recall) return;
    if (!character_generated) return;
    if (!p_ptr->monster_race_idx) return;


    mon_recall_area->setFont(font_win_mon_recall);
    mon_recall_area->moveCursor(QTextCursor::Start);
    QString mon_recall = get_monster_description(p_ptr->monster_race_idx, FALSE, NULL, TRUE);
    mon_recall_area->insertHtml(mon_recall);
}


void MainWindow::close_win_mon_recall(QObject *this_object)
{
    (void)this_object;
    window_mon_recall = NULL;
    show_mon_recall = FALSE;
    win_mon_recall->setText("Show Monster Recall Window");
}

/*
 *  Show widget is called after this to allow
 * the settings to restore the save geometry.
 */
void MainWindow::win_mon_recall_create()
{
    window_mon_recall = new QWidget();
    mon_recall_vlay = new QVBoxLayout;
    window_mon_recall->setLayout(mon_recall_vlay);
    mon_recall_area = new QTextEdit;
    mon_recall_area->setReadOnly(TRUE);
    mon_recall_area->setStyleSheet("background-color: lightGray;");
    mon_recall_area->setTextInteractionFlags(Qt::NoTextInteraction);
    mon_recall_vlay->addWidget(mon_recall_area);
    mon_recall_menubar = new QMenuBar;
    mon_recall_vlay->setMenuBar(mon_recall_menubar);
    window_mon_recall->setWindowTitle("Monster Recall Window");
    mon_recall_win_settings = mon_recall_menubar->addMenu(tr("&Settings"));
    mon_recall_set_font = new QAction(tr("Set Monster Recall Font"), this);
    mon_recall_set_font->setStatusTip(tr("Set the font for the Monster Recall Window."));
    connect(mon_recall_set_font, SIGNAL(triggered()), this, SLOT(win_mon_recall_font()));
    mon_recall_win_settings->addAction(mon_recall_set_font);

    window_mon_recall->setAttribute(Qt::WA_DeleteOnClose);
    connect(window_mon_recall, SIGNAL(destroyed(QObject*)), this, SLOT(close_win_mon_recall(QObject*)));
}


void MainWindow::win_mon_recall_destroy()
{
    if (!show_mon_recall) return;
    delete window_mon_recall;
    window_mon_recall = NULL;
}

void MainWindow::toggle_win_mon_recall()
{
    if (!show_mon_recall)
    {
        win_mon_recall_create();
        show_mon_recall = TRUE;
        win_mon_recall->setText("Hide Monster Recall Window");
        window_mon_recall->show();
        win_mon_recall_update();
    }
    else
    {
        win_mon_recall_destroy();
        show_mon_recall = FALSE;
        win_mon_recall->setText("Show Monster Recall Window");
    }
}
