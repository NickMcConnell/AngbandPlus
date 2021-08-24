
/*
 * Copyright (c) 2015 Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include <src/npp.h>
#include <src/qt_mainwindow.h>
#include <src/knowledge.h>
#include <QHeaderView>
#include <QFontDialog>

/*
 * Monster data for the visible monster list
 */
class monster_vis
{
public:
    u16b count_all;		/* total number of this type visible */
    u16b count_los;		/* number in LOS */
    u16b count_sleeping;	/* total number asleep */
    u16b count_sleeping_los;	/* number asleep and in LOS */
    u16b mon_r_idx; // Monster race
    u16b mon_level;  // Monster level

    u16b count_no_los(void) {return(count_all - count_los);}
    u16b count_sleeping_no_los(void) {return(count_sleeping - count_sleeping_los);}
};

static bool monster_sort(const monster_vis mv1, const monster_vis mv2)
{
    if (mv1.mon_level >= mv2.mon_level) return (TRUE);
    return (FALSE);
}

static QString get_mon_quantity(int r_idx, int quantity)
{
    /* Get monster race and name */
    monster_race *r_ptr = &r_info[r_idx];

    QString output_quantity;

    /* Player ghosts get special markings */
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) output_quantity = ("[G]");

    /* Unique names don't have a number */
    else if (r_ptr->flags1 & (RF1_UNIQUE)) output_quantity = ("[U]");

    /* Get the monster race name for normal races*/
    else output_quantity.setNum(quantity);

    output_quantity.append(" ");

    return (output_quantity);
}

/*
 * Helper function for display monlist.  Prints the number of creatures, followed
 * by either a singular or plural version of the race name as appropriate.
 */
static QString get_mon_name(int r_idx, int los_all, int sleeping)
{
    /* Get monster race and name */
    monster_race *r_ptr = &r_info[r_idx];

    QString output_name;

    /* Player ghosts get special markings */
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
    {
        /* Get the ghost name. */
        output_name =  player_ghost_name;
    }

    /* Unique names don't have a number */
    else if (r_ptr->flags1 & (RF1_UNIQUE))
    {
        output_name = monster_desc_race(r_idx);
    }

    /* Get the monster race name for normal races*/
    else
    {
        output_name = monster_desc_race(r_idx);

        /* Make it plural, if needed. */
        if (los_all > 1)
        {
            output_name = plural_aux(output_name);
        }
    }

    if (sleeping == 1)
    {
        output_name.append(" (sleeping)");
    }

    else if (sleeping > 0)
    {
        if (los_all == sleeping)
        {
            if (los_all == 2) output_name.append(" (both sleeping)");
            else output_name.append(" (all sleeping)");
        }

        else
        {
            output_name.append(QString(" (%1 sleeping) ") .arg(sleeping));
        }
    }

    return (output_name);
}

void MainWindow::set_font_win_mon_list(QFont newFont)
{
    font_win_mon_list = newFont;
    win_mon_list_update();
}

void MainWindow::win_mon_list_font()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, font_message_window, this );

    if (selected)
    {
        set_font_win_mon_list(font);
    }
}

// For when savefiles close but the game doesn't.
void MainWindow::win_mon_list_wipe()
{
    if (!show_mon_list) return;
    if (!character_generated) return;
    while (mon_list_area->rowCount()) mon_list_area->removeRow(mon_list_area->rowCount()-1);
}

void MainWindow::win_mon_list_update()
{
    if (!character_generated) return;
    if (!show_mon_list) return;

    win_mon_list_wipe();

    /* Hallucination is weird */
    if (p_ptr->timed[TMD_IMAGE])
    {
        mon_list_area->insertRow(0);
        mon_list_area->insertRow(1);
        mon_list_area->insertRow(2);

        QTableWidgetItem *header1 = new QTableWidgetItem("You can't believe");
        header1->setTextAlignment(Qt::AlignLeft);
        header1->setFont(font_win_mon_list);
        header1->setTextColor(defined_colors[TERM_ORANGE_PEEL]);
        mon_list_area->setItem(0, 0, header1);

        QTableWidgetItem *header2 = new QTableWidgetItem("what you are seeing!");
        header2->setTextColor(defined_colors[TERM_YELLOW]);
        header2->setFont(font_win_mon_list);
        header2->setTextAlignment(Qt::AlignLeft);
        mon_list_area->setItem(1, 0, header2);

        QTableWidgetItem *header3 = new QTableWidgetItem("It's like a dream!");
        header3->setTextColor(defined_colors[TERM_ORANGE_PEEL]);
        header3->setFont(font_win_mon_list);
        header3->setTextAlignment(Qt::AlignLeft);
        mon_list_area->setItem(2, 0, header3);

        mon_list_area->resizeColumnsToContents();

        return;
    }

    QVector<monster_vis> vis_mon_list;
    vis_mon_list.resize(z_info->r_max);

    monster_type *m_ptr;
    monster_race *r_ptr;

    u16b total_count = 0, disp_count = 0, los_count = 0, hidden = 0;

    /* Scan the list of monsters on the level */
    for (int i = 1; i < mon_max; i++)
    {
        m_ptr = &mon_list[i];
        r_ptr = &r_info[m_ptr->r_idx];

        /* Ignore dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Only consider visible monsters */
        if (!m_ptr->ml) continue;

        /* Hack - ignore lurkers and trappers */
        if (r_ptr->d_char == '.') continue;

        monster_vis *mon_vis_ptr = &vis_mon_list[m_ptr->r_idx];

        total_count++;

        /* Count the hidden ones*/
        if (m_ptr->mflag & (MFLAG_HIDE))
        {
            hidden++;
            continue;
        }

        /* If this is the first one of this type, count the type */
        if (!mon_vis_ptr->count_all)
        {
            mon_vis_ptr->mon_r_idx = m_ptr->r_idx;
            mon_vis_ptr->mon_level = r_ptr->level;
            if (r_ptr->is_unique()) mon_vis_ptr->mon_level += 5;
        }

        /* Check for LOS */
        if (m_ptr->project)
        {
            /* Increment the total number of in-LOS monsters */
            los_count++;

            /* Increment the LOS count for this monster type */
            mon_vis_ptr->count_los++;
        }

        //Sleeping?
        if (m_ptr->m_timed[MON_TMD_SLEEP])
        {
            mon_vis_ptr->count_sleeping++;
            if (m_ptr->project) mon_vis_ptr->count_sleeping_los++;
        }

        mon_vis_ptr->count_all++;
    }

    /* Note no visible monsters at all */
    if (!total_count)
    {
        mon_list_area->insertRow(0);

        if(p_ptr->timed[TMD_BLIND])
        {
            QTableWidgetItem *header1 = new QTableWidgetItem("You are blind!");
            header1->setTextColor(defined_colors[TERM_RED]);
            header1->setTextAlignment(Qt::AlignLeft);
            header1->setFont(font_win_mon_list);
            mon_list_area->setItem(0, 0, header1);
        }
        else
        {
            QTableWidgetItem *header1 = new QTableWidgetItem("You see no monsters.");
            header1->setTextColor(defined_colors[TERM_WHITE]);
            header1->setTextAlignment(Qt::AlignLeft);
            header1->setFont(font_win_mon_list);
            mon_list_area->setItem(0, 0, header1);
        }

        mon_list_area->resizeColumnsToContents();

        return;
    }

    // Trim the list down to only the visible monsters, work backwards
    for (int i = (vis_mon_list.size() - 1); i >= 0; i--)
    {
        monster_vis *mon_vis_ptr = &vis_mon_list[i];

        if (mon_vis_ptr->count_all) continue;

        vis_mon_list.remove(i);
    }

    // sort the remainder
    qSort(vis_mon_list.begin(), vis_mon_list.end(), monster_sort);


    int row = 0;

    mon_list_area->insertRow(row++);

    QTableWidgetItem *in_los = new QTableWidgetItem(QString("%1 MONSTERS IN LOS") .arg(los_count));
    if (los_count == 1) in_los->setText("1 MONSTER IN LOS");
    in_los->setFont(font_win_mon_list);
    in_los->setTextColor(defined_colors[TERM_L_BLUE]);
    in_los->setTextAlignment(Qt::AlignLeft);
    mon_list_area->setItem(0, 2, in_los);

    /* List LOS monsters in descending order of power */
    for (int i = 0; i < vis_mon_list.size(); i++)
    {
        monster_vis *mon_vis_ptr = &vis_mon_list[i];

        // skip if none in LOS
        if (!mon_vis_ptr->count_los) continue;

        int col = 0;

        mon_list_area->insertRow(row);

        r_ptr = &r_info[mon_vis_ptr->mon_r_idx];

        // Symbol (or tile if tiles are used)
        QString mon_symbol = (QString("'%1'") .arg(r_ptr->d_char));
        QTableWidgetItem *mon_ltr = new QTableWidgetItem(mon_symbol);
        if (ui_using_monster_tiles())
        {
            QPixmap pix = ui_get_tile(r_ptr->tile_id, FALSE);
            mon_ltr->setIcon(pix);
        }

        mon_ltr->setData(Qt::ForegroundRole, r_ptr->d_color);
        mon_ltr->setTextAlignment(Qt::AlignCenter);
        mon_ltr->setFont(font_win_mon_list);
        mon_list_area->setItem(row, col++, mon_ltr);

        // Note how many displayed
        disp_count += mon_vis_ptr->count_los;

        QColor this_color;

        /* Uniques and out of depth monsters get a special colour */
        if (r_ptr->flags1 & (RF1_UNIQUE)) this_color = defined_colors[TERM_VIOLET];
        else if (r_ptr->level > p_ptr->depth) this_color = defined_colors[TERM_RED];
        else this_color = defined_colors[TERM_WHITE];

        // Quantity
        QString this_mon_quant = get_mon_quantity(mon_vis_ptr->mon_r_idx, mon_vis_ptr->count_los);
        QTableWidgetItem *quantity = new QTableWidgetItem(this_mon_quant);
        quantity->setFont(font_win_mon_list);
        quantity->setTextColor(this_color);
        quantity->setTextAlignment(Qt::AlignRight);
        mon_list_area->setItem(row, col++, quantity);

        // Race
        QString this_mon_race = get_mon_name(mon_vis_ptr->mon_r_idx, mon_vis_ptr->count_los, mon_vis_ptr->count_sleeping_los);
        QTableWidgetItem *race = new QTableWidgetItem(this_mon_race);
        race->setFont(font_win_mon_list);
        race->setTextColor(this_color);
        race->setTextAlignment(Qt::AlignLeft);
        mon_list_area->setItem(row, col++, race);

        row ++;
    }

    // Handle if we are done
    if (disp_count == total_count)
    {
        mon_list_area->resizeColumnsToContents();
        return;
    }

    // Handle no visible monsters in LOS
    if (!disp_count)
    {
        row = 0;
        mon_list_area->removeRow(row);
    }

    mon_list_area->insertRow(row);
    QTableWidgetItem *out_los = new QTableWidgetItem(QString("%1 MONSTERS OUT OF LOS") .arg(total_count - los_count));
    if ((total_count - los_count) == 1) out_los->setText("1 MONSTER OUT OF LOS");
    out_los->setFont(font_win_mon_list);
    out_los->setTextColor(defined_colors[TERM_L_BLUE]);
    out_los->setTextAlignment(Qt::AlignLeft);
    mon_list_area->setItem(row++, 2, out_los);

    int old_count = disp_count;

    /* List non-LOS monsters in descending order */
    for (int i = 0; i < vis_mon_list.size(); i++)
    {
        monster_vis *mon_vis_ptr = &vis_mon_list[i];

        // skip if none in LOS
        if (!mon_vis_ptr->count_no_los()) continue;

        int col = 0;

        mon_list_area->insertRow(row);

        r_ptr = &r_info[mon_vis_ptr->mon_r_idx];

        // Symbol (or tile if tiles are used)
        QString mon_symbol = (QString("'%1'") .arg(r_ptr->d_char));
        QTableWidgetItem *mon_ltr = new QTableWidgetItem(mon_symbol);
        if (ui_using_monster_tiles())
        {
            QPixmap pix = ui_get_tile(r_ptr->tile_id, FALSE);
            mon_ltr->setIcon(pix);
        }

        mon_ltr->setData(Qt::ForegroundRole, r_ptr->d_color);
        mon_ltr->setTextAlignment(Qt::AlignCenter);
        mon_ltr->setFont(font_win_mon_list);
        mon_list_area->setItem(row, col++, mon_ltr);

        // Note how many displayed
        disp_count += mon_vis_ptr->count_no_los();

        QColor this_color;

        /* Uniques and out of depth monsters get a special colour */
        if (r_ptr->flags1 & (RF1_UNIQUE)) this_color = defined_colors[TERM_VIOLET];
        else if (r_ptr->level > p_ptr->depth) this_color = defined_colors[TERM_RED];
        else this_color = defined_colors[TERM_WHITE];

        // Quantity
        QString this_mon_quant = get_mon_quantity(mon_vis_ptr->mon_r_idx, mon_vis_ptr->count_no_los());
        QTableWidgetItem *quantity = new QTableWidgetItem(this_mon_quant);
        quantity->setFont(font_win_mon_list);
        quantity->setTextColor(this_color);
        quantity->setTextAlignment(Qt::AlignRight);
        mon_list_area->setItem(row, col++, quantity);

        // Race
        QString this_mon_race = get_mon_name(mon_vis_ptr->mon_r_idx, mon_vis_ptr->count_no_los(), mon_vis_ptr->count_sleeping_no_los());
        QTableWidgetItem *race = new QTableWidgetItem(this_mon_race);
        race->setFont(font_win_mon_list);
        race->setTextColor(this_color);
        race->setTextAlignment(Qt::AlignLeft);
        mon_list_area->setItem(row, col++, race);

        row ++;
    }

    // Handle no out of LOS monsters
    if (disp_count == old_count)
    {
        row--;
        mon_list_area->removeRow(row);
    }

    if (hidden)
    {
        int col = 0;

        QString this_string;
        this_string.setNum(hidden);
        this_string.append(" ");

        QTableWidgetItem *quantity = new QTableWidgetItem(this_string);
        quantity->setFont(font_win_mon_list);
        quantity->setTextColor(defined_colors[TERM_WHITE]);
        quantity->setTextAlignment(Qt::AlignRight);
        mon_list_area->setItem(row, col++, quantity);

        QString hidden_mon = "Hidden Monster";
        if (hidden > 1) hidden_mon.append("s");
        QTableWidgetItem *in_los = new QTableWidgetItem(hidden_mon);
        in_los->setFont(font_win_mon_list);
        in_los->setTextColor(defined_colors[TERM_WHITE]);
        in_los->setTextAlignment(Qt::AlignLeft);
        mon_list_area->setItem(row++, col++, in_los);
    }

    mon_list_area->resizeColumnsToContents();
}

void MainWindow::close_win_mon_list(QObject *this_object)
{
    (void)this_object;
    window_mon_list = NULL;
    show_mon_list = FALSE;
    win_mon_list->setText("Show Monster List Window");
}

/*
 *  Show widget is called after this to allow
 * the settings to restore the save geometry.
 */
void MainWindow::win_mon_list_create()
{
    window_mon_list = new QWidget();
    mon_list_vlay = new QVBoxLayout;
    window_mon_list->setLayout(mon_list_vlay);
    mon_list_area = new QTableWidget(0, 3);
    mon_list_area->setAlternatingRowColors(FALSE);
    mon_list_area->verticalHeader()->setVisible(FALSE);
    mon_list_area->horizontalHeader()->setVisible(FALSE);
    mon_list_area->setEditTriggers(QAbstractItemView::NoEditTriggers);
    mon_list_area->setSortingEnabled(FALSE);
    qtablewidget_add_palette(mon_list_area);
    mon_list_vlay->addWidget(mon_list_area);
    mon_list_menubar = new QMenuBar;
    mon_list_vlay->setMenuBar(mon_list_menubar);
    window_mon_list->setWindowTitle("Viewable Monster List");
    mon_win_settings = mon_list_menubar->addMenu(tr("&Settings"));
    mon_list_set_font = new QAction(tr("Set Monster List Font"), this);
    mon_list_set_font->setStatusTip(tr("Set the font for the Monster List."));
    connect(mon_list_set_font, SIGNAL(triggered()), this, SLOT(win_mon_list_font()));
    mon_win_settings->addAction(mon_list_set_font);

    window_mon_list->setAttribute(Qt::WA_DeleteOnClose);
    connect(window_mon_list, SIGNAL(destroyed(QObject*)), this, SLOT(close_win_mon_list(QObject*)));
}

void MainWindow::win_mon_list_destroy()
{
    if (!show_mon_list) return;
    delete window_mon_list;
    window_mon_list = NULL;
}

void MainWindow::toggle_win_mon_list()
{
    if (!show_mon_list)
    {
        win_mon_list_create();
        show_mon_list = TRUE;
        win_mon_list->setText("Hide Monster List Window");
        window_mon_list->show();
        win_mon_list_update();
    }
    else
    {
        win_mon_list_destroy();
        show_mon_list = FALSE;
        win_mon_list->setText("Show Monster List Window");
    }
}
