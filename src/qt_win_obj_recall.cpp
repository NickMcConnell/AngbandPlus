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


void MainWindow::set_font_win_obj_recall(QFont newFont)
{
    win_obj_recall_settings.win_font = newFont;
    win_obj_recall_update();
}

void MainWindow::win_obj_recall_font()
{
    bool selected;
    QFont font = QFontDialog::getFont(&selected, win_obj_recall_settings.win_font, this );

    if (selected)
    {
        set_font_win_obj_recall(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_obj_recall_wipe()
{
    if (!win_obj_recall_settings.win_show) return;
    if (!character_generated) return;

    obj_recall_area->clear();

}

void MainWindow::win_obj_recall_update()
{
    win_obj_recall_wipe();
    if (!win_obj_recall_settings.win_show) return;
    if (!character_generated) return;
    if (!p_ptr->object_kind_idx && !p_ptr->object_idx) return;

    obj_recall_area->setFont(win_obj_recall_settings.win_font);
    obj_recall_area->moveCursor(QTextCursor::Start);

    QString obj_recall;

    if (p_ptr->object_idx)
    {
        object_type *o_ptr = object_from_item_idx(p_ptr->object_idx);
        obj_recall = get_object_description(o_ptr);
    }
    else // p_ptr->object_kind_idx
    {
        s16b k_idx = p_ptr->object_kind_idx;

        // Initialize and prepare a fake object
        object_type object;
        object_type *o_ptr = &object;
        o_ptr->object_wipe();
        object_prep(o_ptr, k_idx);

        if (k_info[k_idx].aware) o_ptr->ident |= (IDENT_STORE);

        /* draw it */
        obj_recall = get_object_description(o_ptr);
    }


    obj_recall_area->insertHtml(obj_recall);
}


/*
 *  Show widget is called after this to allow
 * the settings to restore the save geometry.
 */
void MainWindow::win_obj_recall_create()
{
    win_obj_recall_settings.make_extra_window();

    obj_recall_area = new QTextEdit;
    obj_recall_area->setReadOnly(TRUE);
    obj_recall_area->setStyleSheet("background-color: lightGray;");
    obj_recall_area->setTextInteractionFlags(Qt::NoTextInteraction);
    win_obj_recall_settings.main_vlay->addWidget(obj_recall_area);
    win_obj_recall_settings.main_widget->setWindowTitle("Object Recall Window");
    connect(win_obj_recall_settings.win_font_act, SIGNAL(triggered()), this, SLOT(win_obj_recall_font()));

    connect(win_obj_recall_settings.main_widget, SIGNAL(destroyed(QObject*)), this, SLOT(win_obj_recall_destroy(QObject*)));
}



/*
 * win_obj_recall_close should be used when the game is shutting down.
 * Use this function for closing the window mid-game
 */
void MainWindow::win_obj_recall_destroy(QObject *this_object)
{
    (void)this_object;
    if (!win_obj_recall_settings.win_show) return;
    if (!win_obj_recall_settings.main_widget) return;
    win_obj_recall_settings.get_widget_settings(win_obj_recall_settings.main_widget);
    win_obj_recall_settings.main_widget->deleteLater();
    win_obj_recall_settings.win_show = FALSE;
    win_obj_recall_act->setText("Show Object Recall Window");
}

/*
 * This version should only be used when the game is shutting down.
 * So it is remembered if the window was open or not.
 * For closing the window mid-game use win_obj_recall_destroy directly
 */
void MainWindow::win_obj_recall_close()
{
    bool was_open = win_obj_recall_settings.win_show;
    win_obj_recall_destroy(win_obj_recall_settings.main_widget);
    win_obj_recall_settings.win_show = was_open;
}

void MainWindow::toggle_win_obj_recall()
{
    if (!win_obj_recall_settings.win_show)
    {
        win_obj_recall_create();
        win_obj_recall_settings.win_show = TRUE;
        win_obj_recall_settings.main_widget->setGeometry(win_obj_recall_settings.win_geometry);
        win_obj_recall_act->setText("Hide Object Recall Window");
        if (win_obj_recall_settings.win_maximized) win_obj_recall_settings.main_widget->showMaximized();
        else win_obj_recall_settings.main_widget->show();

        win_obj_recall_update();
    }
    else win_obj_recall_destroy(win_obj_recall_settings.main_widget);
}

