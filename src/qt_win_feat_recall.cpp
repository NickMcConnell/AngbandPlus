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

void MainWindow::set_font_win_feat_recall(QFont newFont)
{
    win_feat_recall_settings.win_font = newFont;
    win_feat_recall_update();
}

void MainWindow::win_feat_recall_font()
{
    bool selected;
    QFont font = QFontDialog::getFont(&selected, win_feat_recall_settings.win_font, this );

    if (selected)
    {
        set_font_win_feat_recall(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_feat_recall_wipe()
{
    if (!win_feat_recall_settings.win_show) return;
    if (!character_generated) return;

    feat_recall_area->clear();

}

void MainWindow::win_feat_recall_update()
{
    win_feat_recall_wipe();
    if (!win_feat_recall_settings.win_show) return;
    if (!character_generated) return;
    if (!p_ptr->feature_kind_idx) return;

    feat_recall_area->setFont(win_feat_recall_settings.win_font);
    feat_recall_area->moveCursor(QTextCursor::Start);

    QString feat_recall = get_feature_description(p_ptr->feature_kind_idx, FALSE, TRUE);
    feat_recall_area->insertHtml(feat_recall);
}



/*
 *  Show widget is called after this to allow
 * the settings to restore the save geometry.
 */
void MainWindow::win_feat_recall_create()
{
    win_feat_recall_settings.make_extra_window();

    feat_recall_area = new QTextEdit;
    feat_recall_area->setReadOnly(TRUE);
    feat_recall_area->setStyleSheet("background-color: lightGray;");
    feat_recall_area->setTextInteractionFlags(Qt::NoTextInteraction);
    win_feat_recall_settings.main_vlay->addWidget(feat_recall_area);
    win_feat_recall_settings.main_widget->setWindowTitle("Feature Recall Window");

    connect(win_feat_recall_settings.win_font_act, SIGNAL(triggered()), this, SLOT(win_feat_recall_font()));

    connect(win_feat_recall_settings.main_widget, SIGNAL(destroyed(QObject*)), this, SLOT(win_feat_recall_destroy(QObject*)));
}


void MainWindow::win_feat_recall_destroy(QObject *this_object)
{
    (void)this_object;
    if (!win_feat_recall_settings.win_show) return;
    if (!win_feat_recall_settings.main_widget) return;
    win_feat_recall_settings.get_widget_settings(win_feat_recall_settings.main_widget);
    win_feat_recall_settings.main_widget->deleteLater();
    win_feat_recall_settings.win_show = FALSE;
    win_feat_recall_act->setText("Show Feature Recall Window");
}

void MainWindow::win_feat_recall_close()
{
    bool was_open = win_feat_recall_settings.win_show;
    win_feat_recall_destroy(win_feat_recall_settings.main_widget);
    win_feat_recall_settings.win_show = was_open;
}

void MainWindow::toggle_win_feat_recall()
{
    if (!win_feat_recall_settings.win_show)
    {
        win_feat_recall_create();
        win_feat_recall_settings.win_show = TRUE;
        win_feat_recall_settings.main_widget->setGeometry(win_feat_recall_settings.win_geometry);
        win_feat_recall_act->setText("Hide Feature Recall Window");
        if (win_feat_recall_settings.win_maximized) win_feat_recall_settings.main_widget->showMaximized();
        else win_feat_recall_settings.main_widget->show();
        win_feat_recall_update();
    }
    else win_feat_recall_destroy(win_feat_recall_settings.main_widget);
}

