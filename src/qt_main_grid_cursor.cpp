/*
 * File: qt_grid_cursor.cpp
 *
 * Copyright (c) 2014  Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/qt_mainwindow.h>
#include <src/npp.h>
#include <src/emitter.h>
#include <src/griddialog.h>
#include <src/player_screen.h>
#include <src/object_all_menu.h>
#include <QGraphicsSceneMouseEvent>
#include <QApplication>



DungeonCursor::DungeonCursor(MainWindow *_parent)
{
    parent = _parent;
    c_x = c_y = 0;
    setZValue(100);
    setVisible(false);
}

QRectF DungeonCursor::boundingRect() const
{
    return QRectF(0, 0, parent->main_cell_wid, parent->main_cell_hgt);
}

void DungeonCursor::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
    (void)option;
    (void)widget;

    if (!character_dungeon) return;

    if (!in_bounds(c_y, c_x)) return;

    painter->save();

    if (parent->targeting_mode)
    {
        painter->setOpacity(0.5);
        painter->setPen(Qt::NoPen);
        painter->setBrush(QColor("red"));
        painter->drawRect(this->boundingRect());
    }

    painter->setOpacity(1);
    painter->setBrush(Qt::NoBrush);
    painter->setPen(QColor("yellow"));
    painter->drawRect(0, 0, parent->main_cell_wid - 1, parent->main_cell_hgt - 1);
    if ((parent->main_cell_wid > 16) && (parent->main_cell_hgt > 16))
    {
        int z = 3;
        painter->drawRect(0, 0, z, z);
        painter->drawRect(parent->main_cell_wid - z - 1, 0, z, z);
        painter->drawRect(0, parent->main_cell_hgt - z - 1, z, z);
        painter->drawRect(parent->main_cell_wid - z - 1, parent->main_cell_hgt - z - 1, z, z);
    }

    painter->restore();
}

void DungeonCursor::moveTo(int _y, int _x)
{
    c_x = _x;
    c_y = _y;
    setPos(c_x * parent->main_cell_wid, c_y * parent->main_cell_hgt);
}

void DungeonCursor::wheelEvent(QGraphicsSceneWheelEvent *event)
{
    event->ignore();
}

void DungeonCursor::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    event->ignore(); // Pass event to the grid
}

void DungeonCursor::mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event)
{
    event->ignore(); // Pass event to the grid
}

void DungeonGrid::cellSizeChanged()
{
    prepareGeometryChange();
}

void DungeonCursor::cellSizeChanged()
{
    prepareGeometryChange();
}

QPainterPath DungeonCursor::shape() const
{
    QPainterPath p;
    p.addRect(boundingRect());
    return p;
}

DungeonGrid::DungeonGrid(int _x, int _y, MainWindow *_parent)
{
    c_x = _x;
    c_y = _y;
    parent = _parent;
    setZValue(0);
}

QRectF DungeonGrid::boundingRect() const
{
    return QRectF(0, 0, parent->main_cell_wid, parent->main_cell_hgt);
}

QString find_cloud_tile(int y, int x)
{
    QString tile;
    tile.clear();

    if (!use_graphics) return tile;

    if (!(dungeon_info[y][x].cave_info & (CAVE_MARK | CAVE_SEEN))) return tile;

    int x_idx = dungeon_info[y][x].effect_idx;
    while (x_idx)
    {
        effect_type *x_ptr = x_list + x_idx;
        x_idx = x_ptr->next_x_idx;

        if (x_ptr->x_flags & EF1_HIDDEN) continue;

        if (x_ptr->x_type == EFFECT_PERMANENT_CLOUD || x_ptr->x_type == EFFECT_LINGERING_CLOUD
                || x_ptr->x_type == EFFECT_SHIMMERING_CLOUD)
        {
            int feat = x_ptr->x_f_idx;
            feat = f_info[feat].f_mimic;
            return f_info[feat].tile_id;
        }
    }

    return tile;
}



// Determine if a tile is intended to be drawn at double-height
static bool is_double_height_tile(int y, int x)
{
    dungeon_type *d_ptr = &dungeon_info[y][x];

    if (use_graphics != GRAPHICS_RAYMOND_GAUSTADNES) return (FALSE);
    if (!d_ptr->double_height_monster) return (FALSE);
    if (main_window->targeting_mode) return (FALSE);
    return (TRUE);
}


/*
 * This function redraws the actual dungeon square onscreen.
 * if 2.5d graphics are on, wall and shop entry sections are drawn
 * from the grid below, to the right, and to the southeast.
 */
void DungeonGrid::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
    (void)option;
    (void)widget;

    if (!character_dungeon) return;

    if (!in_bounds(c_y, c_x)) return;

    painter->fillRect(QRectF(0, 0, parent->main_cell_wid, parent->main_cell_hgt), Qt::black);

    dungeon_type *d_ptr = &dungeon_info[c_y][c_x];
    QChar square_char = d_ptr->dun_char;
    QColor square_color = d_ptr->dun_color;
    bool empty = true;
    u16b flags = 0;
    QString foreground_tile;
    qreal opacity = 1;
    bool do_shadow = false;

    foreground_tile.clear();

    bool double_height_mon = FALSE;
    bool double_height_mon_below = FALSE;

    flags = (d_ptr->ui_flags & (UI_LIGHT_BRIGHT | UI_LIGHT_DIM | UI_LIGHT_TORCH | UI_COSMIC_TORCH));

    bool is_cloud = false;

    // Draw visible monsters
    if (d_ptr->has_visible_monster())
    {
        square_char = d_ptr->monster_char;
        square_color = d_ptr->monster_color;

        empty = false;

        if (!parent->do_pseudo_ascii) foreground_tile = d_ptr->monster_tile;
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

        foreground_tile = d_ptr->effect_tile;

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

        foreground_tile = d_ptr->object_tile;
    }

    bool done_bg = false;
    bool done_fg = false;

    painter->save();

    /// Are we drawing the bottom half of a monster below
    if (is_double_height_tile(c_y, c_x))
    {
        double_height_mon = TRUE;

        // Unless there a visible monster above?
        if (in_bounds(c_y-1, c_x))
        {
            dungeon_type *d2_ptr = &dungeon_info[c_y-1][c_x];
            if (d2_ptr->has_visible_monster()) double_height_mon = FALSE;
        }
    }

    // Are we drawing the top half of a monster below?
    if (in_bounds_fully(c_y+1, c_x) && !d_ptr->has_visible_monster())
    {
        if (is_double_height_tile(c_y+1, c_x)) double_height_mon_below = TRUE;
    }

    //Double check that there isn't something drawn above.  If double height tile and nothing above, use the space.


    // Nothing to draw
    if (empty && !double_height_mon && !double_height_mon_below && !d_ptr->has_visible_terrain())
    {
        painter->restore();
        return;
    }

    if (use_graphics)
    {
        // Draw background tile
        QString background_tile = d_ptr->dun_tile;
        QPixmap pix;

        if (background_tile.length()) pix = parent->get_tile(background_tile, parent->main_cell_hgt, parent->main_cell_wid);

        if (background_tile.length() > 0)
        {
            if (flags & UI_LIGHT_TORCH)
            {
                QColor color = QColor("yellow").darker(150);
                if (flags & UI_COSMIC_TORCH) color = QColor("cyan").darker(150);
                pix = colorize_pix(pix, color);
            }
            else if (flags & UI_LIGHT_BRIGHT)
            {
                pix = parent->apply_shade(background_tile, pix, "bright");
            }
            else if (flags & UI_LIGHT_DIM)
            {
                pix = parent->apply_shade(background_tile, pix, "dim");
            }

            painter->setOpacity(1);
            painter->drawPixmap(pix.rect(), pix, pix.rect());

            done_bg = true;

            // Draw cloud effects (in graphics mode), if not already drawing that
            if (!is_cloud)
            {
                QString tile = find_cloud_tile(c_y, c_x);
                if (!tile.isEmpty())
                {
                    painter->setOpacity(0.7);
                    QPixmap pix = parent->get_tile(tile, parent->main_cell_hgt, parent->main_cell_wid);
                    painter->drawPixmap(0, 0, pix);
                    painter->setOpacity(1);
                    done_bg = true;
                }
            }

            // Draw foreground tile
            if (foreground_tile.length() > 0)
            {
                QPixmap pix3;
                if (double_height_mon)
                {
                    // Use only the bottom half of the tile
                    pix3 = parent->get_tile(foreground_tile, parent->main_cell_hgt*2, parent->main_cell_wid);
                    QRect this_rect(QPoint(0,pix3.height()/2+1), QPoint(pix3.width(), pix3.height()));
                    pix3 = pix3.copy(this_rect);
                }

                else
                {
                    pix3 = parent->get_tile(foreground_tile, parent->main_cell_hgt, parent->main_cell_wid);
                }
                if (flags & (UI_TRANSPARENT_EFFECT | UI_TRANSPARENT_MONSTER))
                {
                   painter->setOpacity(opacity);
                }
                painter->drawPixmap(pix3.rect(), pix3, pix3.rect());
                painter->setOpacity(1);
                done_fg = true;
            }

            // draw foreground circle for dtrap edge
            else if (d_ptr->dtrap && !double_height_mon_below)
            {
                QPixmap sample = parent->get_tile(background_tile, parent->main_cell_hgt, parent->main_cell_wid);
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

            if (double_height_mon_below)
            {
                dungeon_type *d2_ptr = &dungeon_info[c_y+1][c_x];
                // Use only the top half of the tile
                QPixmap pix3 = parent->get_tile(d2_ptr->monster_tile, parent->main_cell_hgt*2, parent->main_cell_wid);
                QRect this_rect(0, 0, pix3.width(), pix3.height()/2);
                pix3 = pix3.copy(this_rect);

                painter->setOpacity(opacity - .3);
                painter->drawPixmap(pix3.rect(), pix3, pix3.rect());
                painter->setOpacity(1);
                done_fg = true;
            }

            if (do_shadow)
            {
                QPixmap pix3 = pseudo_ascii(square_char, square_color, parent->font_main_window,
                                           QSizeF(parent->main_cell_wid, parent->main_cell_hgt));
                painter->drawPixmap(pix3.rect(), pix3, pix.rect());
                done_fg = true;
            }
        }
    }

    // Go ascii?
    if (!done_fg && (!empty || !done_bg))
    {
        // Fill with a solid color for walls if that option is set
        if (main_window->do_wall_block  &&
                ((operator==(square_char, QChar(Qt::Key_NumberSign))) ||
                 (operator==(square_char, QChar(Qt::Key_Percent))) ||
                 (operator==(square_char, QChar(Qt::Key_Asterisk)))))
        {
            // An outside slightly shaded
            QRect outside_shade(QPoint(0, 0), QPoint(parent->main_cell_wid, parent->main_cell_hgt));
            painter->setPen(square_color);
            painter->setOpacity(.85);
            painter->drawRect(outside_shade);

            QRect paste_to(QPoint(1, 1), QPoint(parent->main_cell_wid-1, parent->main_cell_hgt-1));

            painter->setOpacity(1);
            QBrush this_brush;
            if (operator==(square_char, QChar(Qt::Key_Percent)))
            {
                this_brush.setStyle(Qt::Dense5Pattern);
                painter->setBrush(this_brush);
            }
            else if (operator==(square_char, QChar(Qt::Key_Asterisk)))
            {
                this_brush.setStyle(Qt::Dense4Pattern);
                painter->setBrush(this_brush);
            }
            else this_brush.setStyle(Qt::SolidPattern);
            this_brush.setColor(square_color);
            painter->setBrush(this_brush);

            painter->fillRect(paste_to, this_brush);

            painter->restore();
            painter->save();
        }
        else
        {
            painter->setFont(parent->font_main_window);
            painter->setPen(square_color);
            painter->drawText(QRectF(0, 0, parent->main_cell_wid, parent->main_cell_hgt),
                              Qt::AlignCenter, QString(square_char));
        }

    }

    // Show a red line over a monster with its remaining hp
    if (d_ptr->has_visible_monster())
    {
        int cur = p_ptr->chp;
        int max = p_ptr->mhp;
        if (d_ptr->monster_idx > 0)
        {
            monster_type *m_ptr = mon_list + d_ptr->monster_idx;
            cur = m_ptr->hp;
            max = m_ptr->maxhp;
        }
        if (max > 0 && cur < max)
        {
            int w = parent->main_cell_wid * cur / max;
            w = MAX(w, 1);
            int h = 1;
            if (parent->main_cell_hgt > 16) h = 2;
            QColor color("red");
            if (cur * 100 / max > 50) color = QColor("yellow");
            painter->fillRect(0, 0, w, h, color);
        }
    }

    // Draw a mark for visible artifacts
    if (d_ptr->has_visible_artifact())
    {
        int s = 6;
        QPointF points[] =
        {
            QPointF(parent->main_cell_wid - s, parent->main_cell_hgt),
            QPointF(parent->main_cell_wid, parent->main_cell_hgt),
            QPointF(parent->main_cell_wid, parent->main_cell_hgt - s)
        };
        painter->setBrush(QColor("violet"));
        painter->setPen(Qt::NoPen);
        painter->drawPolygon(points, 3);
    }

    painter->restore();
}

QPainterPath DungeonGrid::shape() const
{
    QPainterPath p;
    p.addRect(boundingRect());
    return p;
}

void DungeonGrid::handle_single_click(mouse_click_info mouse_event)
{
    // Already running a command
    if (main_window->executing_command && (!parent->targeting_mode)) return;

    if (parent->targeting_mode)
    {
        parent->input.key = 0;
        parent->input.x = c_x;
        parent->input.y = c_y;
        parent->input.mode = INPUT_MODE_MOUSE_SINGLE_CLICK;
        parent->ev_loop.quit();
    }
    else if (!parent->ev_loop.isRunning())
    {
        int old_x = parent->cursor->c_x;
        int old_y = parent->cursor->c_y;
        parent->grids[old_y][old_x]->update();

        main_window->executing_command = TRUE;

        // The player square has been clicked on.
        if ((p_ptr->py == c_y) && (p_ptr->px == c_x))
        {
            if (mouse_event.right_click)
            {
                do_cmd_cast(-1);
            }
            else if (mouse_event.left_click)
            {
                do_cmd_use_item();
            }
            else if (mouse_event.middle_click)
            {
                do_cmd_fire();
            }
            else if (mouse_event.extra_button_1)
            {
                do_cmd_all_objects(TAB_INVEN);
            }
            else if (mouse_event.extra_button_2)
            {
                do_cmd_character_screen();
            }
        }

        else if (mouse_event.right_click)
        {
            parent->cursor->setVisible(true);
            parent->cursor->moveTo(c_y, c_x);
            GridDialog dlg(c_y, c_x);
        }
        else if (mouse_event.left_click) do_cmd_findpath(c_y, c_x);
        else if (mouse_event.middle_click)
        {
            do_cmd_walk(ui_get_dir_from_slope(p_ptr->py, p_ptr->px, c_y, c_x), !always_pickup);
        }
        else if (mouse_event.extra_button_1)
        {
            //TBD
        }
        else if (mouse_event.extra_button_2)
        {
            //TBD
        }
    }

    notice_stuff();
    handle_stuff();

    main_window->executing_command = FALSE;
    main_window->clear_message_label();
}

// We accept the wheel event here so the main window isn't always scrolled.
void DungeonGrid::wheelEvent(QGraphicsSceneWheelEvent *wheel_event)
{
    // Go to special key handling
    if (main_window->targeting_mode)
    {
      if (wheel_event->delta() > 0) main_window->input.key = Qt::Key_Plus;
      else                    main_window->input.key = Qt::Key_Minus;
      main_window->input.text.clear();
      main_window->input.mode = INPUT_MODE_MOUSE_WHEEL;
      main_window->ev_loop.quit();
      return;
    }

    if (main_window->executing_command) return;

    // Only handling wheel mousewheel rotations.
    if (!wheel_event->delta()) return;

    // Increase or decrease the size of the tile multiplier
    main_window->executing_command = TRUE;

    ui_update_message_label(color_string("Updating Main Window Size", TERM_RED));

    ui_handle_grid_wheelevent((wheel_event->delta() > 0));

    main_window->executing_command = FALSE;

    main_window->clear_message_label();

    handle_stuff();

    // This line prevents the main window from accpeting the event
    // and scrolling the wheel.
    wheel_event->accept();
}

// Use a timer to dintinguish between single and double clicks
void DungeonGrid::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    if (!character_dungeon) return;
    if (p_ptr->in_store) return;
    if (parent->anim_depth > 0) return;

    // Record the mouseclick information
    main_window->single_mouseclick_info.mouse_click_y = c_y;
    main_window->single_mouseclick_info.mouse_click_x = c_x;
    main_window->single_mouseclick_info.left_click = (event->button() & Qt::LeftButton);
    main_window->single_mouseclick_info.right_click = (event->button() & Qt::RightButton);
    main_window->single_mouseclick_info.middle_click = (event->button() & Qt::MiddleButton);
    main_window->single_mouseclick_info.extra_button_1 = (event->button() & Qt::XButton1);
    main_window->single_mouseclick_info.extra_button_2 = (event->button() & Qt::XButton2);
    //  There are many other QGraphicsSceneMouseEvent properties that could be added if needed

    if (!main_window->single_click_timer->isActive())
    {
        bool do_timer = TRUE;

        // First comfirm we should be setting the timer
        if (main_window->executing_command)
        {
            if (!main_window->targeting_mode) do_timer = FALSE;
            else
            {
                if (parent->input.mode == INPUT_MODE_MOUSE_DOUBLE_CLICK) do_timer = FALSE;
            }
        }

        if (do_timer)
        {
            main_window->single_click_timer->start(QApplication::doubleClickInterval());
        }
    }
    main_window->clear_message_label();
    QGraphicsItem::mousePressEvent(event);
}

void DungeonGrid::mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event)
{
    main_window->single_click_timer->stop();

    if (!character_dungeon) return;
    if (p_ptr->in_store) return;
    if (parent->anim_depth > 0) return;

    // Already running a command
    if (main_window->executing_command && (!parent->targeting_mode)) return;

    if (parent->targeting_mode)
    {
        parent->input.key = 0;
        parent->input.x = c_x;
        parent->input.y = c_y;
        parent->input.mode = INPUT_MODE_MOUSE_DOUBLE_CLICK;
        parent->ev_loop.quit();
    }
    else if (!parent->ev_loop.isRunning())
    {
        main_window->executing_command = TRUE;

        bool left_button = (event->button() & Qt::LeftButton);
        bool right_button = (event->button() & Qt::RightButton);
        bool middle_button = (event->button() & Qt::MiddleButton);
        bool extra1 = (event->button() & Qt::XButton1);
        bool extra2 = (event->button() & Qt::XButton2);

        int old_x = parent->cursor->c_x;
        int old_y = parent->cursor->c_y;
        parent->grids[old_y][old_x]->update();

        if (right_button)
        {
            dungeon_type *dun_ptr = &dungeon_info[c_y][c_x];

            if (target_able(dun_ptr->monster_idx, FALSE)) target_set_monster(dun_ptr->monster_idx, FALSE);
            else if (dun_ptr->projectable()) target_set_location(c_y, c_x);
        }
        else if (left_button)
        {
            do_cmd_run(ui_get_dir_from_slope(p_ptr->py, p_ptr->px, c_y, c_x));
        }
        else if (middle_button)
        {
            do_cmd_alter(ui_get_dir_from_slope(p_ptr->py, p_ptr->px, c_y, c_x));
        }
        else if (extra1)
        {

        }
        else if (extra2)
        {

        }
    }

    notice_stuff();
    handle_stuff();

    QGraphicsItem::mouseDoubleClickEvent(event);
    main_window->clear_message_label();
    main_window->executing_command = FALSE;
}

// TO be used in paint
#ifdef PARTIALLY_WORKING_25D_CODE
    bool graphics_25d = ui_use_25d_graphics();


    bool store_or_door_below = FALSE;
    bool store_or_door_right = FALSE;
    bool store_or_door_southeast = FALSE;

    if (graphics_25d)
    {
        if (in_bounds(c_y+1, c_x))
        {
            if (cave_shop_bold(c_y+1, c_x)) store_or_door_below = TRUE;
            if (dungeon_info[c_y+1][c_x].is_door()) store_or_door_below = TRUE;
        }

        if (in_bounds(c_y, c_x+1))
        {
            if (cave_shop_bold(c_y, c_x+1)) store_or_door_right = TRUE;
            if (dungeon_info[c_y][c_x+1].is_door()) store_or_door_right = TRUE;
        }

        if (in_bounds(c_y+1, c_x+1))
        {
            if (cave_shop_bold(c_y+1, c_x+1)) store_or_door_southeast = TRUE;
            if (dungeon_info[c_y+1][c_x+1].is_door()) store_or_door_southeast = TRUE;
        }
    }

    bool is_door = d_ptr->is_door();

        bool is_shop = cave_shop_bold(c_y, c_x);

        //Draw the simple tile
        if (!graphics_25d || (!d_ptr->is_wall(TRUE) && !is_shop && !is_door))
        {
            painter->drawPixmap(pix.rect(), pix, pix.rect());
        }

        // Draw offset walls if needed
        else
        {
            QPixmap pix2 = pix.copy();
            QRect cut_from(QPoint(pix2.width()/3+1, pix2.height()/3+1), QPoint(pix2.width(), pix2.height()));
            QRect paste_to(QPoint(0, 0), QPoint(pix2.width()*2/3, pix2.height()*2/3));
            pix2 = pix2.copy(cut_from);
            painter->setOpacity(1);
            painter->drawPixmap(paste_to, pix2, pix2.rect());

            // Possibly draw the sides of walls if appropriate
            if (!is_shop && !is_door)
            {
                if (!d_ptr->wall_below  && !store_or_door_below)
                {
                    QPixmap pix3 = parent->apply_shade(key1, pix, "dim");
                    QRect paste_offset(QPoint(0, pix3.height()*2/3+1), QPoint(pix3.width()*2/3, pix3.height()));
                    pix3 = pix3.scaledToHeight(pix3.height()/2);
                    painter->drawPixmap(paste_offset, pix3, pix3.rect());
                }
                if (!d_ptr->wall_right && !store_or_door_right)
                {
                    QPixmap pix3 = parent->apply_shade(key1, pix, "dim");
                    QPoint upper_left(pix3.width()*2/3+1, 0);
                    QPoint lower_right(pix3.width(), pix3.height()*2/3);
                    QRect paste_offset(upper_left, lower_right);
                    pix3 = pix3.scaledToWidth(pix3.width()/2);
                    painter->drawPixmap(paste_offset, pix3, pix3.rect());
                }
                if (!d_ptr->wall_southeast  && !store_or_door_southeast)
                {
                    QPixmap pix3 = parent->apply_shade(key1, pix, "dim");

                    QPoint upper_left(pix3.width()*2/3+1, pix3.height()*2/3+1);
                    QPoint lower_right(pix3.width(), pix3.height());
                    QRect paste_offset(upper_left, lower_right);
                    pix3 = pix3.scaledToWidth(pix3.width()/2);
                    pix3 = pix3.scaledToHeight(pix3.height()/2);
                    painter->drawPixmap(paste_offset, pix3, pix3.rect());

                    // If a corner, draw a line
                    if (!d_ptr->wall_right && !d_ptr->wall_below)
                    {
                        QPen this_pen(Qt::black);
                        this_pen.setWidth(2);
                        painter->setPen(this_pen);
                        painter->setOpacity(.25);
                        painter->drawLine(upper_left, lower_right);
                        painter->setOpacity(1);
                    }
                }
            }


        }

            // Cut in the wall from other squares if necessary
            if (graphics_25d)
            {
                bool did_foreground = done_fg;

                if (d_ptr->wall_below || is_shop || is_door)
                {
                    dungeon_type *d2_ptr = &dungeon_info[c_y+1][c_x];

                    QString this_tile = d2_ptr->dun_tile;

                    // Surround shops with permanent walls
                    if (is_shop) this_tile = f_info[FEAT_WALL_PERM].tile_id;

                    // Don't have the outline of another door drawn
                    else if (is_door)
                    {
                        // Use the actual tile
                        int this_feat = f_info[d2_ptr->feature_idx].f_mimic;
                        this_tile = f_info[this_feat].tile_id;
                    }

                    if (this_tile.length())
                    {
                        QPixmap pix3 = parent->get_tile(this_tile, parent->main_cell_hgt, parent->main_cell_wid);
                        if (this_tile.contains(f_info[FEAT_NONE].tile_id)) pix3 = pix.copy();
                        QRect cut_from(QPoint(pix3.width()/3+1, 0), QPoint(pix3.width(), pix3.height()/3));
                        QRect paste_to(QPoint(0, pix3.height()*2/3+1), QPoint(pix3.width()*2/3+1, pix3.height()));
                        pix3 = pix3.copy(cut_from);
                        if (did_foreground || is_shop) painter->setOpacity(.6);
                        else if (d_ptr->is_stairs()) painter->setOpacity(.3);
                        else painter->setOpacity(1);
                        painter->drawPixmap(paste_to, pix3, pix3.rect());
                        painter->setOpacity(1);
                        done_fg = TRUE;
                    }
                }
                else if (store_or_door_below)
                {
                    dungeon_type *d2_ptr = &dungeon_info[c_y+1][c_x];

                    if (d2_ptr->dun_tile.length())
                    {
                        QPixmap pix3 = parent->get_tile(d2_ptr->dun_tile, parent->main_cell_hgt, parent->main_cell_wid);
                        QRect cut_from(QPoint(pix3.width()/3+1, 0), QPoint(pix3.width(), pix3.height()/3));
                        QRect paste_to(QPoint(0, pix3.height()*2/3+1), QPoint(pix3.width()*2/3+1, pix3.height()));
                        pix3 = pix3.copy(cut_from);
                        painter->setOpacity(1);
                        painter->drawPixmap(paste_to, pix3, pix3.rect());
                        done_fg = TRUE;
                    }
                }
                if (d_ptr->wall_right || is_shop || is_door)
                {
                    dungeon_type *d2_ptr = &dungeon_info[c_y][c_x+1];

                    QString this_tile = d2_ptr->dun_tile;

                    // Surround shops with permanent walls
                    if (is_shop) this_tile = f_info[FEAT_WALL_PERM].tile_id;

                    // Don't have the outline of another door drawn
                    else if (is_door)
                    {
                        // Use the actual tile
                        int this_feat = f_info[d2_ptr->feature_idx].f_mimic;
                        this_tile = f_info[this_feat].tile_id;
                    }

                    if (this_tile.length())
                    {
                        QPixmap pix3 = parent->get_tile(this_tile, parent->main_cell_hgt, parent->main_cell_wid);
                        if (this_tile.contains(f_info[FEAT_NONE].tile_id)) pix3 = pix.copy();
                        QRect cut_from(QPoint(0, pix3.height()/3+1), QPoint(pix3.width()/3, pix3.height()));
                        QRect paste_to(QPoint(pix3.width()*2/3+1, 0), QPoint(pix3.width(), pix3.height()*2/3));
                        pix3 = pix3.copy(cut_from);
                        if (did_foreground || is_shop) painter->setOpacity(.6);
                        else if (d_ptr->is_stairs()) painter->setOpacity(.3);
                        else painter->setOpacity(1);
                        painter->drawPixmap(paste_to, pix3, pix3.rect());
                        painter->setOpacity(1);
                        done_fg = TRUE;
                    }
                }
                else if (store_or_door_right)
                {
                    dungeon_type *d2_ptr = &dungeon_info[c_y][c_x+1];
                    if (d2_ptr->dun_tile.length())
                    {
                        QPixmap pix3 = parent->get_tile(d2_ptr->dun_tile, parent->main_cell_hgt, parent->main_cell_wid);
                        QRect cut_from(QPoint(0, pix3.height()/3+1), QPoint(pix3.width()/3, pix3.height()));
                        QRect paste_to(QPoint(pix3.width()*2/3+1, 0), QPoint(pix3.width(), pix3.height()*2/3));
                        pix3 = pix3.copy(cut_from);
                        painter->setOpacity(1);
                        painter->drawPixmap(paste_to, pix3, pix3.rect());
                        done_fg = TRUE;
                    }
                }

                if (d_ptr->wall_southeast || is_shop || is_door)
                {
                    dungeon_type *d2_ptr = &dungeon_info[c_y+1][c_x+1];
                    QString this_tile = d2_ptr->dun_tile;

                    // Surround shops with permanent walls
                    if (is_shop) this_tile = f_info[FEAT_WALL_PERM].tile_id;

                    // Don't have the outline of another door drawn
                    else if (is_door)
                    {
                        // Use the actual tile
                        int this_feat = f_info[d2_ptr->feature_idx].f_mimic;
                        this_tile = f_info[this_feat].tile_id;
                    }

                    if (this_tile.length())
                    {
                        QPixmap pix3 = parent->get_tile(this_tile, parent->main_cell_hgt, parent->main_cell_wid);
                        if (this_tile.contains(f_info[FEAT_NONE].tile_id)) pix3 = pix.copy();
                        QRect cut_from(QPoint(0, 0), QPoint(pix3.width()/3, pix3.height()/3));
                        QRect paste_to(QPoint(pix3.width()*2/3+1, pix3.height()*2/3+1), QPoint(pix3.width(), pix3.height()));
                        pix3 = pix3.copy(cut_from);
                        if (did_foreground || is_shop) painter->setOpacity(.6);
                        else if (d_ptr->is_stairs()) painter->setOpacity(.3);
                        else painter->setOpacity(1);
                        painter->drawPixmap(paste_to, pix3, pix3.rect());
                        painter->setOpacity(1);
                        done_fg = TRUE;
                    }
                }
                else if (store_or_door_southeast)
                {
                    dungeon_type *d2_ptr = &dungeon_info[c_y+1][c_x+1];
                    if (d2_ptr->dun_tile.length())
                    {
                        QPixmap pix3 = parent->get_tile(d2_ptr->dun_tile, parent->main_cell_hgt, parent->main_cell_wid);
                        QRect cut_from(QPoint(0, 0), QPoint(pix3.width()/3, pix3.height()/3));
                        QRect paste_to(QPoint(pix3.width()*2/3+1, pix3.height()*2/3+1), QPoint(pix3.width(), pix3.height()));
                        pix3 = pix3.copy(cut_from);
                        painter->setOpacity(1);
                        painter->drawPixmap(paste_to, pix3, pix3.rect());
                        done_fg = TRUE;
                    }
                }

                // Is there a wall above?
                static bool is_wall_below(int y, int x)
                {
                    if (!ui_use_25d_graphics()) return (FALSE);

                    if (!in_bounds(y+1, x)) return (FALSE);

                    dungeon_type *d_ptr = &dungeon_info[y+1][x];

                    return (d_ptr->is_wall(TRUE));
                }

                // Is there a wall to the right?
                static bool is_wall_right(int y, int x)
                {
                    if (!ui_use_25d_graphics()) return (FALSE);

                    if (!in_bounds(y, x+1)) return (FALSE);

                    dungeon_type *d_ptr = &dungeon_info[y][x+1];

                    return (d_ptr->is_wall(TRUE));
                }

                // Is there a wall (above or below depends on bool above)?
                static bool is_wall_southeast(int y, int x)
                {
                    if (!ui_use_25d_graphics()) return (FALSE);

                    if (!in_bounds(y+1, x+1)) return (FALSE);

                    dungeon_type *d_ptr = &dungeon_info[y+1][x+1];

                    return (d_ptr->is_wall(TRUE));
                }

                static bool coords_sort(coord first, coord second)
                {
                    if (first.y > second.y) return (TRUE);
                    if (first.y < second.y)return (FALSE);

                    // Y coords are equal
                    if (first.x > second.x) return (TRUE);
                    return (FALSE);
                }

#endif
