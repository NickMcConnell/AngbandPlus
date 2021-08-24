/*
 * Copyright (c) 2015 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/npp.h>
#include <src/qt_mainwindow.h>
#include <QFontDialog>
#include <src/emitter.h>
#include <QtCore/qmath.h>

DunMapGrid::DunMapGrid(int _x, int _y)
{
    dm_x = _x;
    dm_y = _y;
    setZValue(0);
}

void DunMapGrid::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
    (void)option;
    (void)widget;

    if (!character_dungeon) return;

    if (!in_bounds(dm_y, dm_x)) return;

    painter->fillRect(QRectF(0, 0, main_window->dun_map_cell_wid, main_window->dun_map_cell_hgt), Qt::black);

    dungeon_type *d_ptr = &dungeon_info[dm_y][dm_x];
    QChar square_char = d_ptr->dun_char;
    QColor square_color = d_ptr->dun_color;
    bool empty = true;
    u16b flags = 0;
    QString key2;
    qreal opacity = 1;
    bool do_shadow = false;

    flags = (d_ptr->ui_flags & (UI_LIGHT_BRIGHT | UI_LIGHT_DIM | UI_LIGHT_TORCH | UI_COSMIC_TORCH));

    bool is_cloud = false;

    // Draw visible monsters
    if (d_ptr->has_visible_monster())
    {
        square_char = d_ptr->monster_char;
        square_color = d_ptr->monster_color;

        empty = false;

        if (!main_window->do_pseudo_ascii) key2 = d_ptr->monster_tile;
        else do_shadow = true;

        flags |= (d_ptr->ui_flags & UI_TRANSPARENT_MONSTER);
        opacity = 0.5;
    }
    // Draw effects
    else if (d_ptr->has_visible_effect())
    {
        square_char = d_ptr->effect_char;
        square_color = d_ptr->effect_color;

        empty = false;

        key2 = d_ptr->effect_tile;

        flags |= (d_ptr->ui_flags & UI_TRANSPARENT_EFFECT);
        opacity = 0.7;

        is_cloud = (flags & UI_TRANSPARENT_EFFECT);
    }
    // Draw objects
    else if (d_ptr->has_visible_object())
    {
        square_char = d_ptr->object_char;
        square_color = d_ptr->object_color;

        empty = false;

        key2 = d_ptr->object_tile;
    }

    bool done_bg = false;
    bool done_fg = false;

    painter->save();

    if (use_graphics && main_window->dun_map_use_graphics)
    {
        // Draw background tile
        QString key1 = d_ptr->dun_tile;

        if (key1.length() > 0)
        {
            QPixmap pix = main_window->get_tile(key1, main_window->dun_map_cell_hgt, main_window->dun_map_cell_wid);

            if (flags & UI_LIGHT_TORCH)
            {
                QColor color = QColor("yellow").darker(150);
                if (flags & UI_COSMIC_TORCH) color = QColor("cyan").darker(150);
                pix = colorize_pix(pix, color);
            }
            else if (flags & UI_LIGHT_BRIGHT)
            {
                pix = main_window->apply_shade(key1, pix, "bright");
            }
            else if (flags & UI_LIGHT_DIM)
            {
                pix = main_window->apply_shade(key1, pix, "dim");
            }

            painter->drawPixmap(pix.rect(), pix, pix.rect());
            done_bg = true;

            // Draw cloud effects (in graphics mode), if not already drawing that
            if (!is_cloud)
            {
                QString tile = find_cloud_tile(dm_y, dm_x);
                if (!tile.isEmpty())
                {
                    painter->setOpacity(0.7);
                    QPixmap pix = main_window->get_tile(tile, main_window->dun_map_cell_hgt, main_window->dun_map_cell_wid);
                    painter->drawPixmap(0, 0, pix);
                    painter->setOpacity(1);
                    done_bg = true;
                }
            }

            // Draw foreground tile
            if (key2.length() > 0) {
               QPixmap pix = main_window->get_tile(key2, main_window->dun_map_cell_hgt, main_window->dun_map_cell_wid);
               if (flags & (UI_TRANSPARENT_EFFECT | UI_TRANSPARENT_MONSTER))
               {
                   painter->setOpacity(opacity);
               }
               painter->drawPixmap(pix.rect(), pix, pix.rect());
               painter->setOpacity(1);
               done_fg = true;
            }
            // draw foreground circle for dtrap edge
            else if (d_ptr->dtrap)
            {
                QPixmap sample = main_window->get_tile(key1, main_window->dun_map_cell_hgt, main_window->dun_map_cell_wid);
                int height = sample.height();
                int width = sample.width();
                QBrush brush(Qt::green);
                painter->setPen(Qt::green);
                painter->setBrush(brush);
                painter->setOpacity(0.7);
                painter->drawEllipse(width/3, height/3, width/3, height/3);
                painter->setOpacity(1);
                painter->setBrush(Qt::NoBrush);
                done_fg = true;
            }

            if (do_shadow) {
                QPixmap pix = pseudo_ascii(square_char, square_color, main_window->font_dun_map,
                                           QSizeF(main_window->dun_map_cell_wid, main_window->dun_map_cell_hgt));
                painter->drawPixmap(pix.rect(), pix, pix.rect());
                done_fg = true;
            }
        }
    }

    // Go ascii?
    if (!done_fg && (!empty || !done_bg))
    {
        painter->setFont(main_window->font_dun_map);
        painter->setPen(square_color);
        painter->drawText(QRectF(0, 0, main_window->dun_map_cell_wid, main_window->dun_map_cell_hgt),
                          Qt::AlignCenter, QString(square_char));
    }

    // Show a red line over a monster with its remaining hp
    if (d_ptr->has_visible_monster())
    {
        int cur = p_ptr->chp;
        int max = p_ptr->mhp;
        if (d_ptr->monster_idx > 0) {
            monster_type *m_ptr = mon_list + d_ptr->monster_idx;
            cur = m_ptr->hp;
            max = m_ptr->maxhp;
        }
        if (max > 0 && cur < max)
        {
            int w = main_window->dun_map_cell_wid * cur / max;
            w = MAX(w, 1);
            int h = 1;
            if (main_window->dun_map_cell_hgt > 16) h = 2;
            QColor color("red");
            if (cur * 100 / max > 50) color = QColor("yellow");
            painter->fillRect(0, 0, w, h, color);
        }
    }

    // Draw a mark for visible artifacts
    if (d_ptr->has_visible_artifact())
    {
        int s = 6;
        QPointF points[] = {
            QPointF(main_window->dun_map_cell_wid - s, main_window->dun_map_cell_hgt),
            QPointF(main_window->dun_map_cell_wid, main_window->dun_map_cell_hgt),
            QPointF(main_window->dun_map_cell_wid, main_window->dun_map_cell_hgt - s)
        };
        painter->setBrush(QColor("violet"));
        painter->setPen(Qt::NoPen);
        painter->drawPolygon(points, 3);
    }

    painter->restore();
}

QPainterPath DunMapGrid::shape() const
{
    QPainterPath p;
    p.addRect(boundingRect());
    return p;
}

QRectF DunMapGrid::boundingRect() const
{
    return QRectF(0, 0, main_window->dun_map_cell_wid, main_window->dun_map_cell_hgt);
}

void DunMapGrid::DunMapCellSizeChanged()
{
    prepareGeometryChange();
}

QRect MainWindow::visible_dun_map()
{
    QGraphicsView *view = dun_map_view;
    QRectF rect1 = view->mapToScene(view->viewport()->geometry()).boundingRect();
    QRect rect2(floor(rect1.x() / dun_map_cell_wid),
                floor(rect1.y() / dun_map_cell_hgt),
                ceil(rect1.width() / dun_map_cell_wid),
                ceil(rect1.height() / dun_map_cell_hgt));
    QRect rect3(0, 0, p_ptr->cur_map_wid, p_ptr->cur_map_hgt);
    rect2 = rect2.intersected(rect3);
    return rect2;
}

void MainWindow::set_dun_map_font(QFont newFont)
{
    font_dun_map = newFont;
    dun_map_calc_cell_size();
}

void MainWindow::win_dun_map_font()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, font_dun_map, this);

    if (selected)
    {
        set_dun_map_font(font);
    }
}

void MainWindow::set_dun_map_graphics()
{
    if (!dun_map_created) return;
    dun_map_use_graphics = dun_map_graphics->isChecked();

    if ((!dun_map_use_graphics) || (use_graphics == GRAPHICS_NONE))
    {
        dun_map_tile_hgt = dun_map_tile_wid = 0;
    }

    else switch (use_graphics)
    {
        case GRAPHICS_RAYMOND_GAUSTADNES:
        {
            dun_map_tile_hgt = dun_map_tile_wid = 64;
            break;
        }
        case GRAPHICS_DAVID_GERVAIS:
        {
            dun_map_tile_hgt = dun_map_tile_wid = 32;
            break;
        }
        case GRAPHICS_ORIGINAL:
        {
            dun_map_tile_hgt = dun_map_tile_wid = 8;
            break;
        }
        default:
        {
            dun_map_tile_hgt = dun_map_tile_wid = 0;
            break;
        }
    }

    dun_map_calc_cell_size();
}

void MainWindow::dun_map_center(int y, int x)
{
    if (!dun_map_created) return;
    dun_map_view->centerOn(x * dun_map_cell_wid, y * dun_map_cell_hgt);
}

void MainWindow::dun_map_calc_cell_size()
{
    if (!dun_map_created) return;

    QFontMetrics metrics(font_dun_map);

    dun_map_font_hgt = metrics.height() + FONT_EXTRA;
    dun_map_font_wid = metrics.width('M') + FONT_EXTRA;

    dun_map_cell_wid = MAX(dun_map_tile_wid, dun_map_font_wid);

    dun_map_cell_hgt = MAX(dun_map_tile_hgt, dun_map_font_hgt);

    for (int y = 0; y < MAX_DUNGEON_HGT; y++)
    {
        for (int x = 0; x < MAX_DUNGEON_WID; x++)
        {
            dun_map_grids[y][x]->DunMapCellSizeChanged();
            dun_map_grids[y][x]->setPos(x * dun_map_cell_wid, y * dun_map_cell_hgt);
        }
    }
    win_dun_map_update();
}

void MainWindow::dun_map_multiplier_clicked(QAction *action)
{
    if (action) dun_map_multiplier = action->objectName();
    QList<QString> parts = dun_map_multiplier.split(":");
    if (parts.size() == 2)
    {
        qreal x = parts.at(1).toFloat();
        qreal y = parts.at(0).toFloat();
        dun_map_view->setTransform(QTransform::fromScale(x, y));
    }
    dun_map_calc_cell_size();
}



// For when savefiles close but the game doesn't.
void MainWindow::win_dun_map_wipe()
{
    if (!show_win_dun_map) return;
    if (!character_generated) return;
    clear_layout(main_vlay_dun_map);
    dun_map_created = FALSE;
}

void MainWindow::dun_map_update_one_grid(int y, int x)
{
    if (!dun_map_created) return;
    DunMapGrid *dmg_ptr = dun_map_grids[y][x];
    dmg_ptr->setVisible(TRUE);
    dmg_ptr->update(dmg_ptr->boundingRect());
}

void MainWindow::win_dun_map_update()
{
    if (!dun_map_created) return;

    // Adjust scrollbars
    dun_map_view->setSceneRect(0, 0, p_ptr->cur_map_wid * dun_map_cell_wid, p_ptr->cur_map_hgt * dun_map_cell_hgt);

    for (int y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (int x = 0; x < p_ptr->cur_map_wid; x++)
        {
            dun_map_update_one_grid(y, x);
        }
    }
    dun_map_center(p_ptr->py, p_ptr->px);
}


void MainWindow::create_win_dun_map()
{
    if (!character_generated) return;
    if (!show_win_dun_map) return;
    dun_map_scene = new QGraphicsScene;
    dun_map_view = new QGraphicsView(dun_map_scene);
    main_vlay_dun_map->addWidget(dun_map_view);

    QBrush brush(QColor("black"));
    dun_map_scene->setBackgroundBrush(brush);

    for (int y = 0; y < MAX_DUNGEON_HGT; y++)
    {
        for (int x = 0; x < MAX_DUNGEON_WID; x++)
        {
            dun_map_grids[y][x] = new DunMapGrid(x, y);
            dun_map_scene->addItem(dun_map_grids[y][x]);
        }
    }

    dun_map_created = TRUE;

    QAction *act = window_dun_map->findChild<QAction *>(dun_map_multiplier);
    if (act)
    {
        dun_map_multiplier_clicked(act);
    }
    set_dun_map_graphics();
    dun_map_calc_cell_size();
}

void MainWindow::close_win_dun_map_frame(QObject *this_object)
{
    (void)this_object;
    window_dun_map = NULL;
    dun_map_created = FALSE;
    show_win_dun_map = FALSE;
    win_dun_map->setText("Show Map Window");
}

/*
 *  Make the small_map shell
 */
void MainWindow::win_dun_map_create()
{
    window_dun_map = new QWidget();
    main_vlay_dun_map = new QVBoxLayout;
    window_dun_map->setLayout(main_vlay_dun_map);

    win_dun_map_menubar = new QMenuBar;
    main_vlay_dun_map->setMenuBar(win_dun_map_menubar);
    window_dun_map->setWindowTitle("Dungeon Map Window");
    win_dun_map_settings = win_dun_map_menubar->addMenu(tr("&Settings"));
    dun_map_font = new QAction(tr("Set Dungeon Map Font"), this);
    dun_map_font->setStatusTip(tr("Set the font for the Dungeon Map Screen."));
    connect(dun_map_font, SIGNAL(triggered()), this, SLOT(win_dun_map_font()));
    win_dun_map_settings->addAction(dun_map_font);
    dun_map_graphics = new QAction(tr("Use Graphics"), this);
    dun_map_graphics->setCheckable(true);
    dun_map_graphics->setChecked(dun_map_use_graphics);
    dun_map_graphics->setStatusTip(tr("If the main window is using graphics, use them in the Dungeon Map Window."));
    connect(dun_map_graphics, SIGNAL(changed()), this, SLOT(set_dun_map_graphics()));
    win_dun_map_settings->addAction(dun_map_graphics);
    QMenu *dun_map_submenu = win_dun_map_settings->addMenu(tr("Tile multiplier"));
    dun_map_multipliers = new QActionGroup(this);

    for (int i = 0; !mult_list[i].isEmpty(); i++)
    {
        QAction *act = dun_map_submenu->addAction(mult_list[i]);
        act->setObjectName(mult_list[i]);
        act->setCheckable(true);
        dun_map_multipliers->addAction(act);
    }
    connect(dun_map_multipliers, SIGNAL(triggered(QAction*)), this, SLOT(dun_map_multiplier_clicked(QAction*)));

    QAction *act = window_dun_map->findChild<QAction *>(dun_map_multiplier);
    if (act)
    {
        act->setChecked(true);
    }

    window_dun_map->setAttribute(Qt::WA_DeleteOnClose);
    connect(window_dun_map, SIGNAL(destroyed(QObject*)), this, SLOT(close_win_dun_map_frame(QObject*)));
}

void MainWindow::win_dun_map_destroy()
{
    if (!show_win_dun_map) return;
    if (!window_dun_map) return;
    delete window_dun_map;
    window_dun_map = NULL;
    dun_map_created = FALSE;
}

void MainWindow::toggle_win_dun_map_frame()
{
    if (!show_win_dun_map)
    {
        win_dun_map_create();
        show_win_dun_map = TRUE;
        create_win_dun_map();
        win_dun_map->setText("Hide Map Window");
        window_dun_map->show();
    }
    else

    {
        win_dun_map_destroy();
        show_win_dun_map = FALSE;
        win_dun_map->setText("Show Map Window");
    }
}
