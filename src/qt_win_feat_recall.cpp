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
    font_win_feat_recall = newFont;
    win_feat_recall_update();
}

void MainWindow::win_feat_recall_font()
{
    bool selected;
    QFont font = QFontDialog::getFont(&selected, font_win_feat_recall, this );

    if (selected)
    {
        set_font_win_feat_recall(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_feat_recall_wipe()
{
    if (!show_feat_recall) return;
    if (!character_generated) return;

    feat_recall_area->clear();

}

void MainWindow::win_feat_recall_update()
{
    win_feat_recall_wipe();
    if (!show_feat_recall) return;
    if (!character_generated) return;
    if (!p_ptr->feature_kind_idx) return;

    feat_recall_area->setFont(font_win_feat_recall);
    feat_recall_area->moveCursor(QTextCursor::Start);

    QString feat_recall = get_feature_description(p_ptr->feature_kind_idx, FALSE, TRUE);
    feat_recall_area->insertHtml(feat_recall);
}

void MainWindow::close_win_feat_recall(QObject *this_object)
{
    (void)this_object;
    window_feat_recall = NULL;
    show_feat_recall = FALSE;
    win_feat_recall->setText("Show Feature Recall Window");
}

/*
 *  Show widget is called after this to allow
 * the settings to restore the save geometry.
 */
void MainWindow::win_feat_recall_create()
{
    window_feat_recall = new QWidget();
    feat_recall_vlay = new QVBoxLayout;
    window_feat_recall->setLayout(feat_recall_vlay);
    feat_recall_area = new QTextEdit;
    feat_recall_area->setReadOnly(TRUE);
    feat_recall_area->setStyleSheet("background-color: lightGray;");
    feat_recall_area->setTextInteractionFlags(Qt::NoTextInteraction);
    feat_recall_vlay->addWidget(feat_recall_area);
    feat_recall_menubar = new QMenuBar;
    feat_recall_vlay->setMenuBar(feat_recall_menubar);
    window_feat_recall->setWindowTitle("Feature Recall Window");
    feat_recall_win_settings = feat_recall_menubar->addMenu(tr("&Settings"));
    feat_recall_set_font = new QAction(tr("Set Feature Recall Font"), this);
    feat_recall_set_font->setStatusTip(tr("Set the font for the Feature Recall Window."));
    connect(feat_recall_set_font, SIGNAL(triggered()), this, SLOT(win_feat_recall_font()));
    feat_recall_win_settings->addAction(feat_recall_set_font);

    window_feat_recall->setAttribute(Qt::WA_DeleteOnClose);
    connect(window_feat_recall, SIGNAL(destroyed(QObject*)), this, SLOT(close_win_feat_recall(QObject*)));
}


void MainWindow::win_feat_recall_destroy()
{
    if (!show_feat_recall) return;
    delete window_feat_recall;
    window_feat_recall = NULL;
}

void MainWindow::toggle_win_feat_recall()
{
    if (!show_feat_recall)
    {
        win_feat_recall_create();
        show_feat_recall = TRUE;
        win_feat_recall->setText("Hide Feature Recall Window");
        window_feat_recall->show();
        win_feat_recall_update();
    }
    else
    {
        win_feat_recall_destroy();
        show_feat_recall = FALSE;
        win_feat_recall->setText("Show Feature Recall Window");
    }
}

