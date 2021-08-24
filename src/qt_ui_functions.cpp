/* File: qt_ui_functions.cpp */

/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
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

#include <QHeaderView>
#include <QScrollBar>
#include <QTableWidget>
#include <QDesktopWidget>
#include "src/npp.h"
#include "src/qt_mainwindow.h"
#include "emitter.h"
#include "tilebag.h"


QRect visible_dungeon()
{
    QGraphicsView *view = main_window->graphics_view;
    QRectF rect1 = view->mapToScene(view->viewport()->geometry()).boundingRect();
    QRect rect2(floor(rect1.x() / main_window->main_cell_wid),
                floor(rect1.y() / main_window->main_cell_hgt),
                ceil(rect1.width() / main_window->main_cell_wid),
                ceil(rect1.height() / main_window->main_cell_hgt));
    QRect rect3(0, 0, p_ptr->cur_map_wid, p_ptr->cur_map_hgt);
    rect2 = rect2.intersected(rect3);
    return rect2;
}



void ui_request_size_update(QWidget *widget)
{
    QObjectList lst = widget->children();
    for (int i = 0; i < lst.size(); i++)
    {
        QObject *obj = lst.at(i);
        if (obj->isWidgetType())
        {
            ui_request_size_update((QWidget *)obj);
        }
    }

    widget->updateGeometry();

    if (widget->layout())
    {
        widget->layout()->invalidate();
    }
}

QSize ui_estimate_table_size(QTableWidget *table, bool horiz, bool vert, int padding)
{
    QSize final(padding, padding);
    int w = 0, h = 0;

    if (horiz)
    {
        w += table->verticalHeader()->width();
        for (int i = 0; i < table->columnCount(); i++)
        {
            if (table->isColumnHidden(i)) continue;
            w += table->columnWidth(i);
        }
    }

    if (vert)
    {
        h += table->horizontalHeader()->height();
        for (int i = 0; i < table->rowCount(); i++)
        {
            if (table->isRowHidden(i)) continue;
            h += table->rowHeight(i);
        }
    }

    final += QSize(w, h);
    return final;
}

void ui_resize_to_contents(QWidget *widget)
{
    ui_request_size_update(widget);
    QCoreApplication::processEvents();
    widget->resize(widget->sizeHint());
}


bool ui_draw_path(u16b path_n, u16b *path_g, int cur_tar_y, int cur_tar_x)
{
    if (path_n < 1) return (FALSE);

    for (int i = 0; i < path_n; i++)
    {
        int y = GRID_Y(path_g[i]);
        int x = GRID_X(path_g[i]);

        dungeon_type *dun_ptr = &dungeon_info[y][x];

        // Re-draw the cursor
        if (y == cur_tar_y && x == cur_tar_x)
        {
            ui_show_cursor(y, x);
            continue;
        }

        QPen pen(add_preset_color(TERM_WHITE));

        /* Choose a colour. */
        /* Visible monsters are orange. */
        if (dun_ptr->has_monster() && mon_list[dun_ptr->monster_idx].ml)
        {
            pen.setColor(add_preset_color(TERM_ORANGE));
        }

        /* Known objects are yellow. */
        else if (dun_ptr->has_object() && o_list[dun_ptr->object_idx].marked)
        {
            pen.setColor(add_preset_color(TERM_YELLOW));
        }

        /* Effects are green */
        else if (dun_ptr->has_effect() && (dun_ptr->cave_info & (CAVE_SEEN | CAVE_MARK)))
        {
            pen.setColor(add_preset_color(TERM_GREEN));
        }

        /* Known walls are blue. */
        else if (!cave_project_bold(y,x) &&
                ((dun_ptr->cave_info & (CAVE_MARK)) ||	player_can_see_bold(y,x)))
        {
            pen.setColor(add_preset_color(TERM_GREEN));
        }
        /* Unknown squares are grey. */
        else if (!(dun_ptr->cave_info & (CAVE_MARK)) && !player_can_see_bold(y,x))
        {
            pen.setColor(add_preset_color(TERM_L_DARK));
        }

        QGraphicsRectItem *item = main_window->dungeon_scene->addRect(
                    x * main_window->main_cell_wid, y * main_window->main_cell_hgt,
                    main_window->main_cell_wid - 1, main_window->main_cell_hgt - 1, pen, Qt::NoBrush);

        item->setOpacity(1);
        item->setZValue(90);

        main_window->path_items.append(item);
    }

    return true;
}

void ui_destroy_path()
{
    for (int i = 0; i < main_window->path_items.size(); i++)
    {
        QGraphicsItem *item = main_window->path_items.at(i);
        main_window->dungeon_scene->removeItem(item);
        delete item;
    }
    main_window->path_items.clear();
    main_window->force_redraw();
}

void ui_activate_main_window()
{
    QFlags<Qt::WindowState> win_flags = main_window->windowState();

    win_flags |= Qt::WindowActive;

    main_window->setWindowState(win_flags);
}

UserInput ui_get_input()
{
    ui_redraw_all();

    // Avoid reentrant calls
    if (main_window->ev_loop.isRunning())
    {
        UserInput temp;
        temp.mode = INPUT_MODE_NONE;
        temp.key = 0;
        temp.x = temp.y = -1;
        temp.text.clear();

        ui_redraw_all();
        return temp;
    }

    main_window->input.mode = INPUT_MODE_NONE;

    main_window->cursor->update();

    main_window->ev_loop.exec();

    main_window->cursor->update();

    if (main_window->input.mode == INPUT_MODE_KEY)
    {
        main_window->input.x = main_window->input.y = -1;
    }
    else
    {
        if (main_window->input.mode != INPUT_MODE_MOUSE_WHEEL) main_window->input.key = 0;
        main_window->input.text.clear();
    }

    ui_redraw_all();

    return main_window->input;
}

static bool screen_needs_recentering(QRect vis)
{
    int py = p_ptr->py;
    int px = p_ptr->px;
    if (py < (vis.y() + PANEL_CHANGE_OFFSET_Y)) return (TRUE);
    if (py >= (vis.y() + vis.height() - PANEL_CHANGE_OFFSET_Y)) return (TRUE);
    if (px < (vis.x() + PANEL_CHANGE_OFFSET_X)) return (TRUE);
    if (px >= (vis.x() + vis.width() - PANEL_CHANGE_OFFSET_X)) return (TRUE);
    return (FALSE);
}

void ui_player_moved()
{
    if (!character_dungeon) return;
    main_window->update_cursor();

    int py = p_ptr->py;
    int px = p_ptr->px;

    if (center_player && !p_ptr->is_running())
    {
        ui_center(py, px);
        main_window->dun_map_center(py, px);
        main_window->overhead_map_center(py, px);
        return;
    }

    if (screen_needs_recentering(visible_dungeon()))
    {
        ui_center(py, px);
    }

    if (main_window->dun_map_created)
    {
        if (screen_needs_recentering(main_window->visible_dun_map()))
        {
            main_window->dun_map_center(py, px);
        }

    }

    if (main_window->overhead_map_created)
    {
        if (screen_needs_recentering(main_window->visible_overhead_map()))
        {
            main_window->overhead_map_center(py, px);
        }

    }

}

QSize ui_grid_size()
{
    return QSize(main_window->main_cell_wid, main_window->main_cell_hgt);
}

QPixmap ui_get_tile(QString tile_id, bool allow_double_height)
{
    // Build a transparent 1x1 pixmap
    if (tile_id.isEmpty()) return ui_make_blank();

    if (!current_tiles) return ui_make_blank();

    QPixmap pix;

    if (tile_id.startsWith("flav_")) pix = current_flav_tiles->get_tile(tile_id);
    else if (tile_id.startsWith("feat_")) pix = current_feat_tiles->get_tile(tile_id);
    else pix = current_tiles->get_tile(tile_id);

    if (pix.width() == 1) return pix;

    int hgt = 32;
    int wid = 32;

    // Use double_height tiles if appropriate
    if (allow_double_height)
    {
        if (pix.height() >= pix.width()*18/10) hgt = 64;
    }


    if (wid != pix.width() || hgt != pix.height())
    {
        pix.scaled(32, 32);

    }

    return pix;
}


void ui_animate_ball(int y, int x, int radius, int type, u32b flg)
{
    BallAnimation *ball = new BallAnimation(QPointF(x, y), radius, type, flg);
    main_window->dungeon_scene->addItem(ball);
    main_window->wait_animation();
    ball->start();

}

void ui_animate_arc(int y0, int x0, int y1, int x1, int type, int radius, int degrees, u32b flg)
{
    ArcAnimation *arc = new ArcAnimation(QPointF(x0, y0), QPointF(x1, y1), degrees, type, radius, flg);
    main_window->dungeon_scene->addItem(arc);
    main_window->wait_animation();
    arc->start();

}

void ui_animate_beam(int y0, int x0, int y1, int x1, int type)
{
    BeamAnimation *beam = new BeamAnimation(QPointF(x0, y0), QPointF(x1, y1), type);
    main_window->dungeon_scene->addItem(beam);
    main_window->wait_animation();
    beam->start();

}

void ui_animate_bolt(int y0, int x0, int y1, int x1, int type, u32b flg)
{
    BoltAnimation *bolt = new BoltAnimation(QPointF(x0, y0), QPointF(x1, y1), type, flg);
    main_window->dungeon_scene->addItem(bolt);
    main_window->wait_animation();
    bolt->start();

}

void ui_animate_throw(int y0, int x0, int y1, int x1, object_type *o_ptr)
{
    BoltAnimation *bolt = new BoltAnimation(QPointF(x0, y0), QPointF(x1, y1), 0, 0, o_ptr);
    main_window->dungeon_scene->addItem(bolt);
    main_window->wait_animation();
    bolt->start();

}

void ui_animate_star(int y, int x, int radius, int type, int gy[], int gx[], int grids)
{
    StarAnimation *star = new StarAnimation(QPointF(x, y), radius, type, gy, gx, grids);
    main_window->dungeon_scene->addItem(star);
    main_window->wait_animation();
    star->start();

}


QPixmap ui_make_blank()
{
    QImage img(1, 1, QImage::Format_ARGB32);
    img.setPixel(0, 0, qRgba(0, 0, 0, 0));
    return QPixmap::fromImage(img);
}

void ui_animate_accomplishment(int y, int x, int gf_type)
{
    u32b flg = PROJECT_PASS;

    BallAnimation *b1 = new BallAnimation(QPointF(x, y), 3, gf_type, flg);
    main_window->dungeon_scene->addItem(b1);
    b1->setZValue(1000);

    HaloAnimation *h1 = new HaloAnimation(y, x);
    main_window->dungeon_scene->addItem(h1);
    h1->setZValue(900);
    main_window->wait_animation(2);

    b1->start();
    h1->start();


}

QFont ui_main_window_font()
{
    return main_window->font_main_window;
}

QFont ui_message_window_font()
{
    return main_window->font_message_window;
}

QFont ui_sidebar_window_font()
{
    return main_window->font_sidebar_window;
}

void ui_update_sidebar_all()
{
    main_window->update_sidebar_all();
}

void ui_update_sidebar_player()
{
    if (!p_ptr->player_turn) return;
    main_window->update_sidebar_player();
    p_ptr->redraw &= ~(PR_SIDEBAR_PL);
}

void ui_update_sidebar_mon()
{
    if (!p_ptr->player_turn) return;
    main_window->update_sidebar_mon();
    p_ptr->redraw &= ~(PR_SIDEBAR_MON);
}

QString ui_return_sidebar_text(bool label, int row)
{
    return (main_window->return_sidebar_text(label, row));
}

void ui_update_statusbar()
{
    main_window->update_statusbar();
    p_ptr->redraw &= ~(PR_STATUSBAR);
}

void ui_update_titlebar()
{
    main_window->update_titlebar();
    p_ptr->redraw &= ~(PR_TITLEBAR);
}

void ui_update_messages()
{
    main_window->update_messages();
    p_ptr->redraw &= ~(PR_MESSAGES);
}

void ui_update_monlist()
{
    if (!p_ptr->player_turn) return;
    if (p_ptr->is_running()) return;
    if (p_ptr->is_resting()) return;
    main_window->win_mon_list_update();
    p_ptr->redraw &= ~(PR_WIN_MONLIST);
}

void ui_update_objlist()
{
    if (!p_ptr->player_turn) return;
    if (p_ptr->is_running()) return;
    if (p_ptr->is_resting()) return;
    main_window->win_obj_list_update();
    p_ptr->redraw &= ~(PR_WIN_OBJLIST);
}

void ui_update_mon_recall()
{
    if (p_ptr->is_running()) return;
    if (p_ptr->is_resting()) return;
    main_window->win_mon_recall_update();
    p_ptr->redraw &= ~(PR_WIN_MON_RECALL);
}

void ui_update_obj_recall()
{
    if (p_ptr->is_running()) return;
    main_window->win_obj_recall_update();
    p_ptr->redraw &= ~(PR_WIN_OBJ_RECALL);
}

void ui_update_feat_recall()
{
    if (p_ptr->is_running()) return;
    if (p_ptr->is_resting()) return;
    main_window->win_feat_recall_update();
    p_ptr->redraw &= ~(PR_WIN_FEAT_RECALL);
}

void ui_update_message_window()
{
    main_window->win_messages_update();
    p_ptr->redraw &= ~(PR_WIN_MESSAGES);
}

void ui_update_char_basic_window()
{
    if (!p_ptr->player_turn) return;
    if (p_ptr->is_running() || p_ptr->is_resting()) return;
    main_window->win_char_info_basic_update();
    p_ptr->redraw &= ~(PR_WIN_CHAR_BASIC | PR_PLYR_SCORE | PR_TURNCOUNT);
}

void ui_update_char_equip_info_window()
{
    if (!p_ptr->player_turn) return;
    if (p_ptr->is_running() || p_ptr->is_resting()) return;
    main_window->win_char_info_equip_update();
    p_ptr->redraw &= ~(PR_WIN_CHAR_EQUIP_INFO);
}

void ui_update_char_equipment_window()
{
    if (!p_ptr->player_turn) return;
    if (p_ptr->is_running() || p_ptr->is_resting()) return;
    main_window->win_char_equipment_update();
    p_ptr->redraw &= ~(PR_WIN_EQUIPMENT);
}

void ui_update_char_inventory_window()
{
    if (!p_ptr->player_turn) return;
    if (p_ptr->is_running() || p_ptr->is_resting()) return;
    main_window->win_char_inventory_update();
    p_ptr->redraw &= ~(PR_WIN_INVENTORY);
}

void ui_update_char_score()
{
    if (p_ptr->is_running() || p_ptr->is_resting()) return;
    main_window->win_char_info_score();
    p_ptr->redraw &= ~(PR_PLYR_SCORE);
}

void ui_update_char_turncount()
{
    if (p_ptr->is_running() || p_ptr->is_resting()) return;
    main_window->win_char_info_turncount();
    p_ptr->redraw &= ~(PR_TURNCOUNT);
}


int ui_get_dir_from_slope(int y1, int x1, int y2, int x2)
{
    QLineF line(ui_get_center(y1, x1), ui_get_center(y2, x2));
    if (line.length() == 0.0) return 0.0;
    qreal angle = line.angle();
    if (angle < 22.5) return (6);
    if (angle < 67.5) return (9);
    if (angle < 112.5) return (8);
    if (angle < 157.5) return (7);
    if (angle < 202.5) return (4);
    if (angle < 247.5) return (1);
    if (angle < 292.5) return (2);
    if (angle < 337.5) return (3);
    return(6);
}

QPoint ui_get_center(int y, int x)
{
    x *= main_window->main_cell_wid;
    y *= main_window->main_cell_hgt;
    x += main_window->main_cell_wid / 2;
    y += main_window->main_cell_hgt / 2;
    return QPoint(x, y);
}

// determine of a dungeon square is onscreen at present
bool panel_contains(int y, int x)
{
    return main_window->panel_contains(y, x);
}

void ui_ensure(int y, int x)
{
    main_window->graphics_view->ensureVisible(QRectF(x * main_window->main_cell_wid,
                                                     y * main_window->main_cell_hgt,
                                                     main_window->main_cell_wid, main_window->main_cell_hgt));
}

bool ui_modify_panel(int y, int x)
{
    QRect vis = visible_dungeon();

    if (y < 0) y = 0;
    if (y >= p_ptr->cur_map_hgt) y = p_ptr->cur_map_hgt - 1;

    if (x < 0) x = 0;
    if (x >= p_ptr->cur_map_wid) x = p_ptr->cur_map_wid - 1;

    if (y == vis.y() && x == vis.x()) return false;

    main_window->graphics_view->verticalScrollBar()->setValue(y * main_window->main_cell_hgt);
    main_window->graphics_view->horizontalScrollBar()->setValue(x * main_window->main_cell_wid);

    return true;
}

bool ui_adjust_panel(int y, int x)
{
    QRect vis = visible_dungeon();

    int y2 = vis.y();
    int x2 = vis.x();

    while (y < y2) y2 -= vis.height() / 2;
    while (y >= y2 + vis.height()) y2 += vis.height() / 2;

    while (x < x2) x2 -= vis.width() / 2;
    while (x >= x2 + vis.width()) x2 += vis.width() / 2;

    return ui_modify_panel(y2, x2);
}

bool ui_change_panel(int dir)
{
    QRect vis = visible_dungeon();

    int y = vis.y() + ddy[dir] * vis.height() / 2;
    int x = vis.x() + ddx[dir] * vis.width() / 2;

    return ui_modify_panel(y, x);
}

void ui_center(int y, int x)
{
    main_window->graphics_view->centerOn(x * main_window->main_cell_wid, y * main_window->main_cell_hgt);
}

bool ui_use_25d_graphics(void)
{
    if (use_graphics != GRAPHICS_RAYMOND_GAUSTADNES) return (FALSE);
    if (!main_window->do_25d_graphics) return (FALSE);

    return (TRUE);
}

bool ui_using_tiles()
{
    if (use_graphics == GRAPHICS_RAYMOND_GAUSTADNES) return (TRUE);
    if (use_graphics == GRAPHICS_DAVID_GERVAIS) return (TRUE);
    if (use_graphics == GRAPHICS_ORIGINAL) return (TRUE);
    return (FALSE);
}

// pseudo_graphics uses letters for mosnters.
bool ui_using_monster_tiles()
{
    if (main_window->do_pseudo_ascii) return (FALSE);
    return (ui_using_tiles());
}


void ui_redraw_grid(int y, int x)
{
    DungeonGrid *g_ptr = main_window->grids[y][x];
    g_ptr->setVisible(true);
    g_ptr->update(g_ptr->boundingRect());

    main_window->dun_map_update_one_grid(y, x);
    main_window->overhead_map_update_one_grid(y, x);
}

void ui_redraw_all()
{
    p_ptr->redraw &= ~(PR_MAP | PR_DRAW);
    main_window->redraw_all();
    redraw_coords.clear();
}

void player_death_close_game(void)
{
    player_death();
    main_window->close_game_death();
}

void ui_animate_detection(int y, int x, int rad)
{
    DetectionAnimation *anim = new DetectionAnimation(y, x, rad);
    main_window->dungeon_scene->addItem(anim);
    main_window->wait_animation();
    anim->start();

}

void ui_show_cursor(int y, int x)
{
    if (y < 0 || x < 0)
    {
        main_window->update_cursor();
    }
    else
    {
        main_window->cursor->moveTo(y, x);
        main_window->cursor->setVisible(true);
    }
}

// Use the deisred widget screen, unless it is too big for the screen
QSize ui_max_widget_size(QSize this_size)
{
    QDesktopWidget dummy_widget;
    QRect main_screen_size = dummy_widget.screenGeometry(dummy_widget.primaryScreen());

    QSize return_size = main_screen_size.size();

    return_size.setWidth(MIN(this_size.width(), (return_size.width() * 9 / 10)));
    return_size.setHeight(MIN(this_size.height(), (return_size.height() * 17 / 20)));

    return (return_size);
}

void ui_png_screenshot(void)
{
    main_window->save_png_screenshot();
}

void ui_update_message_label(QString message)
{
    main_window->update_message_label(message);
}

void ui_clear_message_label()
{
    main_window->clear_message_label();
}
