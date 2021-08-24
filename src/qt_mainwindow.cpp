/* File: qt_mainwindow.cpp */

/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */


#include <QTextStream>
#include <QGraphicsRectItem>
#include <QHeaderView>
#include <QGraphicsScale>
#include <QFileDialog>
#include <QToolButton>
#include <QPushButton>
#include <QSplitter>
#include <QApplication>
#include <QFontDialog>
#include <QMenuBar>
#include <QToolBar>
#include <QStatusBar>
#include <QScrollBar>
#include <QDockWidget>
#include <QMenuBar>

#include "src/npp.h"
#include "src/qt_mainwindow.h"
#include "src/init.h"
#include "src/optionsdialog.h"
#include <src/command_list.h>
#include <src/player_command.h>
#include <src/object_all_menu.h>
#include "src/player_birth.h"
#include "src/utilities.h"
#include "src/knowledge.h"
#include "src/help.h"
#include "src/hotkeys.h"
#include "package.h"
#include "tilebag.h"
#include <src/messages.h>

// Needed to check for keypresses
#ifdef Q_OS_WIN

#include "windows.h"

#endif // Q_OS_WIN


MainWindow *main_window = 0;

QString mult_list[] =
{
  QString("0.25:0.25"),
  QString("0.5:0.5"),
  QString("0.67:0.67"),
  QString("0.75:0.75"),
  QString("1:1"),
  QString("1.25:1.25"),
  QString("1.5:1.5"),
  QString("1.75:1.75"),
  QString("2:1"),
  QString("2:2"),
  QString("2.5:2.5"),
  QString("3:3"),
  QString("4:2"),
  QString("4:4"),
  QString("5:5"),
  QString("6:3"),
  QString("6:6"),
  QString("7:7"),
  QString("8:4"),
  QString("8:8"),
  QString("10:5"),
  QString("10:10"),
  QString("")
};

static QPixmap gray_pix(QPixmap src)
{
    QImage img = src.toImage();
    for (int x = 0; x < img.width(); x++)
    {
        for (int y = 0; y < img.height(); y++)
        {
            QColor col = QColor(img.pixel(x, y)).darker();
            int gray = qGray(col.rgb());
            img.setPixel(x, y, qRgb(gray, gray, gray));
        }
    }
    return QPixmap::fromImage(img);
}


static QPixmap darken_pix(QPixmap src)
{
    QImage img = src.toImage();
    QPainter p(&img);
    p.setCompositionMode(QPainter::CompositionMode_HardLight);
    p.fillRect(img.rect(), QColor("#444"));
    QPixmap pix = QPixmap::fromImage(img);
    return pix;
}


void MainWindow::wait_animation(int n_animations)
{
    anim_depth += n_animations;
}


void MainWindow::animation_done()
{
    anim_depth--;
}

void MainWindow::do_create_package()
{
    PackageDialog dlg("create");
}

void MainWindow::do_extract_from_package()
{
    PackageDialog dlg("extract");
}


void MainWindow::force_redraw()
{
    graphics_view->viewport()->update();
    if (dun_map_created) dun_map_view->viewport()->update();
    if (overhead_map_created) overhead_map_view->viewport()->update();
}

void MainWindow::update_cursor()
{
    if (targeting_mode <= MODE_TARGETING_AIMING) cursor->moveTo(p_ptr->py, p_ptr->px);
    cursor->setVisible(hilight_player || (targeting_mode > MODE_TARGETING_AIMING));
}

QPixmap MainWindow::apply_shade(QString tile_id, QPixmap tile, QString shade_id)
{
    tile_id += ":";
    tile_id += shade_id;    

    if (shade_cache.contains(tile_id)) return shade_cache.value(tile_id);

    QPixmap pix;

    if (shade_id == "dim")
    {
        pix = gray_pix(tile);
    }
    else if (shade_id == "bright")
    {
        pix = darken_pix(tile);
    }
    else {  // It should never happen
        pix = tile;
    }

    shade_cache.insert(tile_id, pix);

    return pix;
}



void MainWindow::destroy_tiles()
{    
    tiles.clear();
    shade_cache.clear();
}

QPixmap MainWindow::get_tile(QString tile_id, int tile_hgt, int tile_wid)
{
    if (!current_tiles) return ui_make_blank();

    QPixmap pix;

    if (tiles.contains(tile_id))
    {
        pix = tiles.value(tile_id);
    }
    else
    {
        if (tile_id.startsWith("flav_")) pix = current_flav_tiles->get_tile(tile_id);
        else if (tile_id.startsWith("feat_")) pix = current_feat_tiles->get_tile(tile_id);
        else pix = current_tiles->get_tile(tile_id);

        tiles.insert(tile_id, pix);
    }

    if (pix.width() == 1) return pix;

    if (tile_wid != pix.width() || tile_hgt != pix.height())
    {
        pix = pix.scaled(tile_wid, tile_hgt);
    }

    return pix;
}

void MainWindow::calculate_cell_size()
{
    main_cell_wid = MAX(main_tile_wid, main_font_wid);

    main_cell_hgt = MAX(main_tile_hgt, main_font_hgt);

    for (int y = 0; y < MAX_DUNGEON_HGT; y++)
    {
        for (int x = 0; x < MAX_DUNGEON_WID; x++)
        {
            grids[y][x]->cellSizeChanged();
            grids[y][x]->setPos(x * main_cell_wid, y * main_cell_hgt);
        }
    }

    cursor->cellSizeChanged();
}

void MainWindow::set_graphic_mode(int mode)
{    


    switch (mode)
    {
        case GRAPHICS_RAYMOND_GAUSTADNES:
        {
            main_tile_hgt = 64;
            main_tile_wid = 64;
            current_tiles = tiles_64x64;
            current_flav_tiles = tiles_flav_64x64;
            current_feat_tiles = tiles_feat_64x64;
            reg_mode_act->setChecked(TRUE);
            break;
        }
        case GRAPHICS_DAVID_GERVAIS:
        {
            main_tile_hgt = 32;
            main_tile_wid = 32;
            current_tiles = tiles_32x32;
            current_flav_tiles = tiles_flav_32x32;
            current_feat_tiles = tiles_feat_32x32;
            dvg_mode_act->setChecked(TRUE);
            break;
        }
        case GRAPHICS_ORIGINAL:
        {
            main_tile_hgt = 8;
            main_tile_wid = 8;
            current_tiles = tiles_8x8;
            current_flav_tiles = tiles_flav_8x8;
            current_feat_tiles = tiles_feat_8x8;
            old_tiles_act->setChecked(TRUE);
            break;
        }
        default: //GRAPHICS_NONE:
        {
            main_tile_hgt = 0;
            main_tile_wid = 0;
            current_tiles = 0;
            current_flav_tiles = 0;
            current_feat_tiles = 0;
            ascii_mode_act->setChecked(TRUE);
            break;
        }
    }

    use_graphics = mode;
    calculate_cell_size();
    set_dun_map_graphics();
    set_overhead_map_graphics();
    destroy_tiles();
    if (character_dungeon) extract_tiles();
    update_sidebar_all();

    // Recenter the view
    if (character_dungeon)
    {
        ui_redraw_all();
        ui_center(p_ptr->py, p_ptr->px);
        dun_map_center(p_ptr->py, p_ptr->px);
        overhead_map_center(p_ptr->py, p_ptr->px);
    }
}

void MainWindow::set_keymap_mode(int mode)
{
    which_keyset = mode;

    if (mode == KEYSET_ANGBAND) keymap_angband->setChecked(TRUE);
    else if (mode == KEYSET_ROGUE) keymap_rogue->setChecked(TRUE);
    // (mode == KEYSET_NEW)
    else  keymap_new->setChecked(TRUE);
}

void MainWindow::set_font_main_window(QFont newFont)
{
    font_main_window = newFont;
    QFontMetrics metrics(font_main_window);
    main_font_hgt = metrics.height() + FONT_EXTRA;
    main_font_wid = metrics.width('M') + FONT_EXTRA;

    calculate_cell_size();

    destroy_tiles();
}

void MainWindow::set_font_message_window(QFont newFont)
{
    font_message_window = newFont;
    message_area->setFont(newFont);
    message_label->setFont(newFont);
    ui_update_messages();
}

void MainWindow::set_font_sidebar_window(QFont newFont)
{
    font_sidebar_window = newFont;
    update_sidebar_font();
}

void MainWindow::toggle_searching()
{
    if (!character_dungeon) return;
    if (executing_command) return;
    executing_command = TRUE;
    do_cmd_toggle_search();
    notice_stuff();
    handle_stuff();
    clear_message_label();
    executing_command = FALSE;
}

void MainWindow::update_message_label(QString message)
{
    message_label->show();
    message = (QString("<b><h2>%1</h2></b>") .arg(message));
    message_label->setText(message);
}

void MainWindow::clear_message_label()
{
    message_label->hide();
    message_label->setText("");
}

void MainWindow::click_study()
{
    if (!character_dungeon) return;
    if (executing_command) return;
    executing_command = TRUE;
    do_cmd_study(-1);
    notice_stuff();
    handle_stuff();
    clear_message_label();
    executing_command = FALSE;
}

void MainWindow::init_scene()
{
    QFontMetrics metrics(font_main_window);

    main_font_hgt = metrics.height() + FONT_EXTRA;
    main_font_wid = metrics.width('M') + FONT_EXTRA;
    main_tile_hgt = main_tile_wid = 0;
    main_cell_hgt = main_cell_wid = 0;

    QBrush brush(QColor("black"));
    dungeon_scene->setBackgroundBrush(brush);    

    for (int y = 0; y < MAX_DUNGEON_HGT; y++)
    {
        for (int x = 0; x < MAX_DUNGEON_WID; x++)
        {
            grids[y][x] = new DungeonGrid(x, y, this);
            dungeon_scene->addItem(grids[y][x]);
        }
    }

    dungeon_scene->addItem(cursor);
}

void MainWindow::redraw_screen()
{
    // Important. No dungeon yet
    if (!character_dungeon)
    {
        if (graphics_view) force_redraw();
        return;
    }

    // Adjust scrollbars
    graphics_view->setSceneRect(0, 0, p_ptr->cur_map_wid * main_cell_wid, p_ptr->cur_map_hgt * main_cell_hgt);
    if (dun_map_created)
    {
        dun_map_view->setSceneRect(0, 0, p_ptr->cur_map_wid * dun_map_cell_wid, p_ptr->cur_map_hgt * dun_map_cell_hgt);
    }
    if (overhead_map_created)
    {
        overhead_map_view->setSceneRect(0, 0, p_ptr->cur_map_wid * overhead_map_cell_wid, p_ptr->cur_map_hgt * overhead_map_cell_hgt);
    }

    for (int y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (int x = 0; x < p_ptr->cur_map_wid; x++)
        {
            map_info(y, x);
        }
    }
    for (int y = 0; y < p_ptr->cur_map_hgt; y++)
    {
        for (int x = 0; x < p_ptr->cur_map_wid; x++)
        {
            grids[y][x]->update(grids[y][x]->boundingRect());
        }
    }
}

void MainWindow::redraw_all()
{    
    redraw_screen();
    update_cursor();
    force_redraw(); // Hack -- Force full redraw

    update_messages();
    update_titlebar();
    update_statusbar();
    win_char_info_basic_update();
    update_sidebar_mon();
    update_sidebar_player();
    win_obj_list_update();
    win_mon_list_update();
    win_char_info_equip_update();
    win_char_equipment_update();


    p_ptr->redraw = 0L;
}

bool MainWindow::panel_contains(int y, int x)
{
    QPolygonF pol = graphics_view->mapToScene(graphics_view->viewport()->geometry());
    // We test top-left and bottom-right corners of the cell
    QPointF point1(x * main_cell_wid, y * main_cell_hgt);
    QPointF point2(x * main_cell_wid + main_cell_wid, y * main_cell_hgt + main_cell_hgt);
    return pol.containsPoint(point1, Qt::OddEvenFill) && pol.containsPoint(point2, Qt::OddEvenFill);
}

QPixmap pseudo_ascii(QChar chr, QColor color, QFont font, QSizeF size)
{
    QImage img(size.width(), size.height(), QImage::Format_ARGB32);
    // Fill with transparent color
    for (int x = 0; x < size.width(); x++)
    {
        for (int y = 0; y < size.height(); y++)
        {
            img.setPixel(x, y, QColor(0, 0, 0, 0).rgba());
        }
    }

    QPainter p(&img);
    p.setPen(color);
    p.setFont(font);
    // Draw the text once to get the shape of the letter plus antialiasing
    p.drawText(img.rect(), Qt::AlignCenter, QString(chr));

    // Mark colored grids
    bool marks[img.width()][img.height()];
    for (int x = 0; x < size.width(); x++) {
        for (int y = 0; y < size.height(); y++) {
            QRgb pixel = img.pixel(x, y);
            if (qAlpha(pixel) > 0) {
                marks[x][y] = true;
            }
            else {
                marks[x][y] = false;
            }
        }
    }

    // Surround with black. Note that all concerning grids are burned, even marked ones
    for (int x = 0; x < size.width(); x++) {
        for (int y = 0; y < size.height(); y++) {
            if (!marks[x][y]) continue;
            for (int y1 = y - 1; y1 <= y + 1; y1++) {
                for (int x1 = x - 1; x1 <= x + 1; x1++) {
                    if (!img.rect().contains(x1, y1, false)) continue;
                    img.setPixel(x1, y1, qRgba(0, 0, 0, 255));
                }
            }
        }
    }

    // Draw the text again so the antialiasing pixels blend with black properly
    p.drawText(img.rect(), Qt::AlignCenter, QString(chr));

    return QPixmap::fromImage(img);
}



// The main function - intitalize the main window and set the menus.
MainWindow::MainWindow()
{
    // Store a reference for public functions (panel_contains and others)
    if (!main_window) main_window = this;

    setAttribute(Qt::WA_DeleteOnClose);

    anim_depth = 0;
    which_keyset = KEYSET_NEW;
    character_dungeon = character_generated = character_loaded = character_xtra = FALSE;
    executing_command = overhead_map_created = dun_map_created = FALSE;
    equip_show_buttons = inven_show_buttons = TRUE;
    dun_map_cell_wid = dun_map_cell_hgt = 0;
    dun_map_use_graphics = dun_map_created = FALSE;
    overhead_map_cell_wid = overhead_map_cell_hgt = 0;
    overhead_map_use_graphics = overhead_map_created = FALSE;
    object_level = monster_level = 0;

    win_mon_list_settings.set_extra_win_default();
    win_obj_list_settings.set_extra_win_default();
    win_mon_recall_settings.set_extra_win_default();
    win_obj_recall_settings.set_extra_win_default();
    win_feat_recall_settings.set_extra_win_default();
    win_message_settings.set_extra_win_default();
    char_info_basic_settings.set_extra_win_default();
    char_info_equip_settings.set_extra_win_default();
    char_equipment_settings.set_extra_win_default();
    char_inventory_settings.set_extra_win_default();
    dun_map_settings.set_extra_win_default();
    overhead_map_settings.set_extra_win_default();

    targeting_mode = MODE_NO_TARGETING;

    cursor = new DungeonCursor(this);
    show_targeting_buttons = show_hotkey_toolbar = TRUE;
    do_25d_graphics = do_pseudo_ascii = do_wall_block = FALSE;

    overhead_map_multiplier = dun_map_multiplier = main_multiplier = "1:1";

    // Set the main area
    main_widget = new QWidget;
    setCentralWidget(main_widget);
    main_widget_hlay = new QHBoxLayout;
    main_widget->setLayout(main_widget_hlay);
    main_sidebar_vlay = new QVBoxLayout;
    main_widget_vlay = new QVBoxLayout;
    main_widget_hlay->addLayout(main_sidebar_vlay);
    main_widget_hlay->addLayout(main_widget_vlay);



    // Set up the sidebar area, make it scrollable in case there are alot of monsters
    sidebar_widget = new QWidget;
    sidebar_scroll = new QScrollArea;
    QPalette this_pal;
    this_pal.setColor(QPalette::Background, Qt::black);
    sidebar_widget->setAutoFillBackground(TRUE);
    sidebar_widget->setPalette(this_pal);
    sidebar_scroll->setPalette(this_pal);
    main_sidebar_vlay->addWidget(sidebar_scroll);
    sidebar_scroll->setWidget(sidebar_widget);
    sidebar_scroll->setWidgetResizable(TRUE);
    sidebar_scroll->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    sidebar_scroll->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

    sidebar_vlay = new QVBoxLayout;
    sidebar_widget->setLayout(sidebar_vlay);

    create_sidebar();


    // Set up the message area
    message_area_hlay = new QHBoxLayout;
    main_widget_vlay->addLayout(message_area_hlay);

    message_area = new QLabel("");
    message_area->setFont(font_message_window);
    message_area->setStyleSheet("background-color: black;");
    message_area->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Preferred);
    message_area_hlay->addWidget(message_area);

    message_label = new QLabel("");
    message_label->setFont(font_message_window);
    message_area_hlay->addWidget(message_label);
    message_label->setStyleSheet("background-color: black;");
    message_label->setWordWrap(TRUE);    
    clear_message_label();

    // Set up the main dungeon area
    dungeon_scene = new QGraphicsScene;
    graphics_view = new QGraphicsView(dungeon_scene);
    graphics_view->installEventFilter(this);

    main_widget_vlay->addWidget(graphics_view);

    // Set up all the folder directories
    create_directories();

    create_actions();

    create_menus();
    create_toolbars();
    select_font();
    create_signals();
    (void)statusBar();
    read_settings();

    init_scene();
    set_graphic_mode(use_graphics);
    set_keymap_mode(which_keyset);

    update_file_menu_game_inactive();

    setWindowFilePath(QString());

    // Deactivate the unused context menu
    setContextMenuPolicy(Qt::NoContextMenu);


}

void MainWindow::setup_nppangband()
{
    game_mode = GAME_NPPANGBAND;

    setWindowTitle(tr("NPPAngband"));

    init_npp_games();
}

void MainWindow::setup_nppmoria()
{
    game_mode = GAME_NPPMORIA;

    setWindowTitle(tr("NPPMoria"));

    init_npp_games();
}


// Prepare to play a game of NPPAngband.
void MainWindow::start_game_nppangband()
{
    setup_nppangband();

    launch_birth(FALSE);
}

// Prepare to play a game of NPPMoria.
void MainWindow::start_game_nppmoria()
{
    setup_nppmoria();

    launch_birth(FALSE);
}

void MainWindow::open_current_savefile()
{
    // Let the user select the savefile
    QString file_name = QFileDialog::getOpenFileName(this, tr("Select a savefile to open"), npp_dir_save.path(), tr("NPP (*.npp)"));
    if (file_name.isEmpty()) return;

    load_file(file_name);
}

void MainWindow::save_character()
{
    if (current_savefile.isEmpty())
        save_character_as();
    else
        save_file(current_savefile);
}

void MainWindow::save_character_as()
{
    // Start with the current player name
    QString default_name = "player";
    if (!op_ptr->full_name.isEmpty())default_name = op_ptr->full_name;
    QString default_file = npp_dir_save.path();
    default_file.append("/");
    default_file.append(default_name);

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save File As"), default_file, tr("NPP (*.npp)"));

    if (fileName.isEmpty())
        return;

    save_file(fileName);
}

void MainWindow::update_messages()
{
    update_message_area(message_area, 3);
}


void MainWindow::close_game_death()
{
    save_and_close();
}

void MainWindow::save_and_close()
{
    if (running_command()) return;

    // Don't need the timer any more
    event_timer->stop();

    save_character();

    set_current_savefile("");

    // Wipe the extra windows
    win_mon_list_wipe();
    win_obj_list_wipe();
    win_mon_recall_wipe();
    win_obj_recall_wipe();
    win_feat_recall_wipe();
    win_messages_wipe();
    win_char_info_basic_wipe();
    win_char_info_equip_wipe();
    win_char_equipment_wipe();
    win_char_inventory_wipe();
    win_dun_map_wipe();
    win_overhead_map_wipe();

    character_loaded = character_dungeon = character_generated = FALSE;

    update_file_menu_game_inactive();

    // close game
    cleanup_npp_games();

    message_area->setText("");
    update_titlebar();

    cursor->setVisible(false);
    destroy_tiles();
    redraw_all();
}

bool MainWindow::eventFilter(QObject *obj, QEvent *event)
{
    if (event->type() == QEvent::KeyPress)
    {
        this->keyPressEvent(dynamic_cast<QKeyEvent *>(event));

        return (TRUE);
    }

    if (event->type() == QEvent::Wheel)
    {
        this->wheelEvent(dynamic_cast<QWheelEvent *>(event));

        return (TRUE);
    }

    return QObject::eventFilter(obj, event);
}

// Use the wheelscroll to increase the tile mltiplier
void MainWindow::handle_grid_wheelevent(bool wheelscroll_increase)
{
    // Go through and find the active multiplier
    QString active_multiplier;
    active_multiplier.clear();
    QString new_multiplier;
    new_multiplier.clear();
    int current_slot = -1;
    QList<QAction *> list_multipliers = multipliers->actions();
    for (int x = 0; x < list_multipliers.size(); x++)
    {
        if (!list_multipliers.at(x)->isChecked()) continue;
        // Found it
        active_multiplier = list_multipliers.at(x)->objectName();
        break;
    }

    // Now find the slot on the list
    for (int i = 0; !mult_list[i].isEmpty(); i++)
    {
        if (!strings_match(active_multiplier, mult_list[i])) continue;
        // Break when we find it
        current_slot = i;
        break;
    }

    // Paranoia
    if (current_slot < 0)
    {
        // Do nothing
    }
    // Increasing wheel click
    else if (wheelscroll_increase)
    {
        if (mult_list[current_slot+1].length())
        {
            new_multiplier = mult_list[current_slot+1];
        }
    }
    //Decreasing wheel click
    else
    {
        // First check if we are not at the bottom of the list
        if (current_slot)
        {
            new_multiplier = mult_list[current_slot-1];
        }
    }

    // Now find the action and select it
    if (new_multiplier.length()) for (int x = 0; x < list_multipliers.size(); x++)
    {
        if (!strings_match(new_multiplier, list_multipliers.at(x)->objectName())) continue;
        // Found it.
        list_multipliers.at(x)->trigger();
        break;
    }


}



void MainWindow::keyPressEvent(QKeyEvent* which_key)
{
    if (!character_dungeon) return;
    if (p_ptr->in_store) return;
    if (anim_depth > 0) return;

    // TODO PLAYTESTING
    debug_rarities();

    QString keystring = which_key->text();

    // Go to special key handling
    if (targeting_mode)
    {
        input.key = which_key->key();
        input.text = keystring;
        input.mode = INPUT_MODE_KEY;
        ev_loop.quit();
        return;
    }

    // Already running a command
    else if (executing_command) return;

    executing_command = TRUE;

    int key_pressed = which_key->key();

    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();

    bool shift_key = modifiers.testFlag(Qt::ShiftModifier);
    bool ctrl_key = modifiers.testFlag(Qt::ControlModifier);
    bool alt_key = modifiers.testFlag(Qt::AltModifier);
    bool meta_key = modifiers.testFlag(Qt::MetaModifier);
    bool keypad_used = modifiers.testFlag(Qt::KeypadModifier);
    bool numlock_on = FALSE;

    if (QApplication::queryKeyboardModifiers() & (Qt::ShiftModifier))    shift_key = TRUE;
    if (QApplication::queryKeyboardModifiers() & (Qt::ControlModifier))  ctrl_key = TRUE;
    if (QApplication::queryKeyboardModifiers() & (Qt::AltModifier))      alt_key = TRUE;
    if (QApplication::queryKeyboardModifiers() & (Qt::MetaModifier))     meta_key = TRUE;
    if (QApplication::queryKeyboardModifiers() & (Qt::KeypadModifier))   keypad_used = TRUE;

    // Check for keypresses
#ifdef Q_OS_WIN
    if (GetKeyState(VK_NUMLOCK) == 1) numlock_on = TRUE;
    if (GetKeyState(VK_CAPITAL) == 1) shift_key = TRUE;
#endif // Q_OS_WIN

    // Numlock interferes with the shift key detection.
    // However the keys below can only be pressed on the keypad if the shift key is pressed.
    if (keypad_used && numlock_on && !shift_key)
    {
        switch (key_pressed)
        {
            case Qt::Key_End:
            case Qt::Key_Down:
            case Qt::Key_PageDown:
            case Qt::Key_Left:
            case Qt::Key_Clear:
            case Qt::Key_Right:
            case Qt::Key_Home:
            case Qt::Key_Up:
            case Qt::Key_PageUp:
            case Qt::Key_Insert:
            {
                shift_key = TRUE;
                break;
            }
            default: break;
        }
    }

    //Hotkeys are checked first
    if (check_hotkey_commands(key_pressed, shift_key, alt_key, ctrl_key, meta_key))
    {
        // Fall through
    }
    else if (which_keyset == KEYSET_NEW)
    {
        commands_new_keyset(key_pressed, shift_key, alt_key, ctrl_key, meta_key);
    }
    else if (which_keyset == KEYSET_ANGBAND)
    {
        commands_angband_keyset(key_pressed, shift_key, alt_key, ctrl_key, meta_key);
    }
    else if (which_keyset == KEYSET_ROGUE)
    {
        commands_roguelike_keyset(key_pressed, shift_key, alt_key, ctrl_key, meta_key);
    }
    else pop_up_message_box("invalid keyset");

    notice_stuff();
    handle_stuff();
    clear_message_label();

    executing_command = FALSE;
}


bool MainWindow::running_command()
{
    if (ev_loop.isRunning())
    {
        pop_up_message_box("You must finish or cancel the current command first to do this");
        return true;
    }
    return false;
}

void MainWindow::closeEvent(QCloseEvent *event)
{
    if (running_command())
    {
        event->ignore();
        return;
    }

    if (!current_savefile.isEmpty() && character_dungeon)
    {
        save_character();
        pop_up_message_box("Game saved");
    }

    /*
     * Take out the additional windows.
     * We do this before the write the settings so the
     * most recent windows geometry is recorded.
     */
    win_mon_list_close();
    win_obj_list_close();
    win_mon_recall_close();
    win_obj_recall_close();
    win_feat_recall_close();
    win_messages_close();
    win_char_info_basic_close();
    win_char_info_equip_close();
    win_char_equipment_close();
    win_char_inventory_close();
    win_dun_map_close();
    win_overhead_map_close();

    write_settings();

    event->accept();
}

void MainWindow::hideEvent(QHideEvent *event)
{
    if (win_mon_list_settings.win_show && win_mon_list_settings.main_widget) win_mon_list_settings.main_widget->hide();
    if (win_obj_list_settings.win_show && win_obj_list_settings.main_widget) win_obj_list_settings.main_widget->hide();
    if (win_mon_recall_settings.win_show && win_mon_recall_settings.main_widget) win_mon_recall_settings.main_widget->hide();
    if (win_obj_recall_settings.win_show && win_obj_recall_settings.main_widget) win_obj_recall_settings.main_widget->hide();
    if (win_feat_recall_settings.win_show && win_feat_recall_settings.main_widget) win_feat_recall_settings.main_widget->hide();
    if (win_message_settings.win_show && win_message_settings.main_widget) win_message_settings.main_widget->hide();
    if (char_info_basic_settings.win_show && char_info_basic_settings.main_widget) char_info_basic_settings.main_widget->hide();
    if (char_info_equip_settings.win_show && char_info_equip_settings.main_widget) char_info_equip_settings.main_widget->hide();
    if (char_equipment_settings.win_show && char_equipment_settings.main_widget) char_equipment_settings.main_widget->hide();
    if (char_inventory_settings.win_show && char_inventory_settings.main_widget) char_inventory_settings.main_widget->hide();
    if (dun_map_settings.win_show && dun_map_settings.main_widget) dun_map_settings.main_widget->hide();
    if (overhead_map_settings.win_show && overhead_map_settings.main_widget) overhead_map_settings.main_widget->hide();

    event->accept();
}

void MainWindow::showEvent(QShowEvent *event)
{
    if (win_mon_list_settings.win_show && win_mon_list_settings.main_widget) win_mon_list_settings.main_widget->show();
    if (win_obj_list_settings.win_show && win_obj_list_settings.main_widget) win_obj_list_settings.main_widget->show();
    if (win_mon_recall_settings.win_show && win_mon_recall_settings.main_widget) win_mon_recall_settings.main_widget->show();
    if (win_obj_recall_settings.win_show && win_obj_recall_settings.main_widget) win_obj_recall_settings.main_widget->show();
    if (win_feat_recall_settings.win_show && win_feat_recall_settings.main_widget) win_feat_recall_settings.main_widget->show();
    if (win_message_settings.win_show && win_message_settings.main_widget) win_message_settings.main_widget->show();
    if (char_info_basic_settings.win_show && char_info_basic_settings.main_widget) char_info_basic_settings.main_widget->show();
    if (char_info_equip_settings.win_show && char_info_equip_settings.main_widget) char_info_equip_settings.main_widget->show();
    if (char_equipment_settings.win_show && char_equipment_settings.main_widget) char_equipment_settings.main_widget->show();
    if (char_inventory_settings.win_show && char_inventory_settings.main_widget) char_inventory_settings.main_widget->show();
    if (dun_map_settings.win_show && dun_map_settings.main_widget) dun_map_settings.main_widget->show();
    if (overhead_map_settings.win_show && overhead_map_settings.main_widget) overhead_map_settings.main_widget->show();

    main_window->activateWindow();

    event->accept();
}

void MainWindow::open_recent_file()
{
    QAction *action = qobject_cast<QAction *>(sender());
    if (action)
    {
        load_file(action->data().toString());
    }
}

void MainWindow::options_dialog()
{
    OptionsDialog *dlg = new OptionsDialog;
    dlg->exec();
    delete dlg;
    redraw_screen();
    handle_stuff();
}

void MainWindow::hp_warning_dialog()
{
    QString prompt = "Please select a hit point warning threshold:";

    op_ptr->hitpoint_warn = (byte)get_quantity_slider(prompt, QString("Percent"), 0, 99, op_ptr->hitpoint_warn);
}

void MainWindow::delay_anim_factor_dialog()
{
    QString prompt = "Please select an animation delay adjustment factor:";

    op_ptr->delay_anim_factor = get_quantity_slider(prompt, QString(" Percent"), 25, 200, op_ptr->delay_anim_factor);
}

void MainWindow::delay_run_factor_dialog()
{
    QString prompt = "Please select a run delay factor:";

    op_ptr->delay_run_factor = get_quantity_slider(prompt, QString("msec"), 0, 250, op_ptr->delay_run_factor);
}


void MainWindow::toggle_show_targeting()
{
    if (show_targeting_buttons)
    {
        show_targeting_buttons = FALSE;
        show_targeting_act->setText("Show Targeting Buttons");
        show_targeting_act->setStatusTip(tr("Display the targeting buttons in the sidebar when sleecting a target."));
    }
    else
    {
        show_targeting_buttons = TRUE;
        show_targeting_act->setText("Hide Targeting Button");
        show_targeting_act->setStatusTip(tr("Do not display the targeting buttons in the sidebar when sleecting a target."));
    }

    if (targeting_mode)show_targeting_sidebar();
}

void MainWindow::toggle_show_hotkey_toolbar()
{
    if (show_hotkey_toolbar)
    {
        show_hotkey_toolbar = FALSE;
        show_hotkey_toolbar_act->setText("Show Hotkey Toolbar");
        show_hotkey_toolbar_act->setStatusTip(tr("Display a toolbar with buttons for each hotkey."));
        hotkey_toolbar_hide();
    }
    else
    {
        show_hotkey_toolbar = TRUE;
        show_hotkey_toolbar_act->setText("Hide Hotkey Toolbar");
        show_hotkey_toolbar_act->setStatusTip(tr("Hide the Hotkey Toolbar."));
        hotkey_toolbar_show();
    }
}

void MainWindow::object_squelch_menu(void)
{
    do_object_squelch_menu();
}

void MainWindow::quality_squelch_menu(void)
{
    do_quality_squelch_menu();
}

void MainWindow::ego_item_squelch_menu(void)
{
    do_ego_item_squelch_menu();
}

void MainWindow::font_dialog_main_window()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, font_main_window, this );

    if (selected)
    {
        set_font_main_window(font);
        redraw_screen();
    }
}

void MainWindow::font_dialog_message_window()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, font_message_window, this );

    if (selected)
    {
        set_font_message_window(font);
    }
}

void MainWindow::font_dialog_sidebar_window()
{
    bool selected;
    QFont font = QFontDialog::getFont( &selected, font_sidebar_window, this );

    if (selected)
    {
        set_font_sidebar_window(font);
    }
}

void MainWindow::manage_hotkeys()
{
    do_hotkey_manage();
}

void MainWindow::export_hotkeys()
{
    QString default_name = "hotkey_";
    QString default_file = npp_dir_user.path();
    default_file.append("/");
    default_file.append(default_name);

    QString file_name = QFileDialog::getSaveFileName(this, tr("Select a hotkey file."), default_file, tr("NPPHK (*.npphk)"));

    if (file_name.isEmpty())
        return;

    do_hotkey_export(file_name);
}

void MainWindow::import_hotkeys()
{
    // Let the user select the savefile
    QString file_name = QFileDialog::getOpenFileName(this, tr("Select a hotkey file."), npp_dir_user.path(), tr("NPPHK (*.npphk)"));
    if (file_name.isEmpty()) return;

    do_hotkey_import(file_name);

    update_hotkey_toolbar();
}


void MainWindow::about()
{
   QMessageBox::about(this, tr("About NPPAngband and NPPMoria"),
            tr("<h2>NPPAngband and NPPMoria"
               "<p>Copyright (c) 2003-2016 Jeff Greene and Diego González.</h2>"

               "<p>For resources and links to places you can talk about the game, please see:"
               "<p>http://forum.nppangband.org/ -- the NPPAngband Forums"

               "<p>Based on Moria: (c) 1985 Robert Alan Koeneke and Umoria (c) 1989 James E. Wilson, David Grabiner,"
               "<p>Angband 2.6.2:   Alex Cutler, Andy Astrand, Sean Marsh, Geoff Hill, Charles Teague, Charles Swiger, "
               "Angband 2.7.0 - 2.8.5:   Ben Harrison 2.9.0 - 3.0.6: Robert Ruehlmann, "
               "Angband 3.0.7 - 3.4.0:  Andrew Sidwell"
               "<p>Oangband 0.6.0 Copyright 1998-2003 Leon Marrick, Bahman Rabii"
               "<p>EYAngband 0.5.2 By Eytan Zweig, UNAngband by Andrew Doull"
               "<p>Sangband 0.9.9 and Demoband by Leon Marrick"
               "<p>FAangband 1.6 by Nick McConnell"
               "<p>Please see copyright.txt for complete copyright and licensing restrictions."));
}

void MainWindow::command_list_keyboard()
{
    do_cmd_list_keyboard_commands();
}

void MainWindow::command_list_mouse()
{
    do_cmd_list_mouse_commands();
}

void MainWindow::command_list_targeting()
{
    do_cmd_list_targeting_commands();
}

// Activates and de-activates certain file_menu commands when a game is started.
// Assumes create_actions has already been called.
void MainWindow::update_file_menu_game_active()
{
    new_game_nppangband->setEnabled(FALSE);
    new_game_nppmoria->setEnabled(FALSE);
    open_savefile->setEnabled(FALSE);
    save_cur_char->setEnabled(TRUE);
    save_cur_char_as->setEnabled(TRUE);
    close_cur_char->setEnabled(TRUE);

    for (int i = 0; i < MAX_RECENT_SAVEFILES; ++i)
    {
        recent_savefile_actions[i]->setEnabled(FALSE);
    }

    options_act->setEnabled(TRUE);
    hitpoint_warning_act->setEnabled(TRUE);
    delay_anim_factor_act->setEnabled(TRUE);
    delay_run_factor_act->setEnabled(TRUE);
    object_squelch_act->setEnabled(TRUE);
    quality_squelch_act->setEnabled(TRUE);
    ego_item_squelch_act->setEnabled(TRUE);
    view_monster_knowledge->setEnabled(TRUE);
    view_object_knowledge->setEnabled(TRUE);
    view_ego_item_knowledge->setEnabled(TRUE);
    view_artifact_knowledge->setEnabled(TRUE);
    view_terrain_knowledge->setEnabled(TRUE);
    view_notes->setEnabled(TRUE);
    view_messages->setEnabled(TRUE);
    view_home_inven->setEnabled(TRUE);
    view_scores->setEnabled(TRUE);
    view_kill_count->setEnabled(TRUE);

    hotkey_manage->setEnabled(TRUE);
    hotkey_export->setEnabled(TRUE);
    hotkey_import->setEnabled(TRUE);

    show_sidebar();
    show_statusbar();

}

// Activates and de-activates certain file_menu commands when a game is ended.
// Assumes create_actions has already been called.
void MainWindow::update_file_menu_game_inactive()
{
    new_game_nppangband->setEnabled(TRUE);
    new_game_nppmoria->setEnabled(TRUE);
    open_savefile->setEnabled(TRUE);
    save_cur_char->setEnabled(FALSE);
    save_cur_char_as->setEnabled(FALSE);
    close_cur_char->setEnabled(FALSE);

    for (int i = 0; i < MAX_RECENT_SAVEFILES; ++i)
    {
        recent_savefile_actions[i]->setEnabled(TRUE);
    }

    options_act->setEnabled(FALSE);
    hitpoint_warning_act->setEnabled(FALSE);
    delay_anim_factor_act->setEnabled(FALSE);
    delay_run_factor_act->setEnabled(FALSE);
    object_squelch_act->setEnabled(FALSE);
    quality_squelch_act->setEnabled(FALSE);
    ego_item_squelch_act->setEnabled(FALSE);
    view_monster_knowledge->setEnabled(FALSE);
    view_object_knowledge->setEnabled(FALSE);
    view_ego_item_knowledge->setEnabled(FALSE);
    view_artifact_knowledge->setEnabled(FALSE);
    view_terrain_knowledge->setEnabled(FALSE);
    view_notes->setEnabled(FALSE);
    view_messages->setEnabled(FALSE);
    view_home_inven->setEnabled(FALSE);
    view_scores->setEnabled(FALSE);
    view_kill_count->setEnabled(FALSE);

    hotkey_manage->setEnabled(FALSE);
    hotkey_export->setEnabled(FALSE);
    hotkey_import->setEnabled(FALSE);

    hide_sidebar();
    hide_statusbar();
    hotkey_toolbar_hide();

}


//  Set's up all the QActions that will be added to the menu bar.  These are later added by create_menus.
void MainWindow::create_actions()
{

    event_timer = new QTimer(this);
    event_timer->setSingleShot(FALSE);
    event_timer->setInterval(100);
    connect(event_timer, SIGNAL(timeout()), this, SLOT(timed_events()));

    single_click_timer = new QTimer(this);
    single_click_timer->setSingleShot(TRUE);
    connect(single_click_timer, SIGNAL(timeout()), this, SLOT(single_click_events()));

    new_game_nppangband = new QAction(tr("New Game - NPPAngband"), this);
    new_game_nppangband->setStatusTip(tr("Start a new game of NPPAngband."));
    new_game_nppangband->setIcon(QIcon(":/icons/lib/icons/New_game_NPPAngband.png"));
    new_game_nppangband->setShortcut(tr("Ctrl+A"));
    connect(new_game_nppangband, SIGNAL(triggered()), this, SLOT(start_game_nppangband()));

    new_game_nppmoria = new QAction(tr("New Game - NPPMoria"), this);
    new_game_nppmoria->setStatusTip(tr("Start a new game of NPPMoria."));
    new_game_nppmoria->setIcon(QIcon(":/icons/lib/icons/New_Game_NPPMoria.png"));
    new_game_nppmoria->setShortcut(tr("Ctrl+R"));
    connect(new_game_nppmoria, SIGNAL(triggered()), this, SLOT(start_game_nppmoria()));

    open_savefile = new QAction(tr("Open Savefile"), this);
    open_savefile->setShortcut(tr("Ctrl+F"));
    open_savefile->setIcon(QIcon(":/icons/lib/icons/open_savefile.png"));
    open_savefile->setStatusTip(tr("Open an existing savefile."));
    connect(open_savefile, SIGNAL(triggered()), this, SLOT(open_current_savefile()));

    save_cur_char = new QAction(tr("Save Character"), this);
    save_cur_char->setShortcut(tr("Ctrl+S"));
    save_cur_char->setIcon(QIcon(":/icons/lib/icons/save.png"));
    save_cur_char->setStatusTip(tr("Save current character."));
    connect(save_cur_char, SIGNAL(triggered()), this, SLOT(save_character()));

    save_cur_char_as = new QAction(tr("Save Character As"), this);
    save_cur_char_as->setShortcut(tr("Ctrl+W"));
    save_cur_char_as->setIcon(QIcon(":/icons/lib/icons/save_as.png"));
    save_cur_char_as->setStatusTip(tr("Save current character to new file."));
    connect(save_cur_char_as, SIGNAL(triggered()), this, SLOT(save_character_as()));

    close_cur_char = new QAction(tr("Save And Close"), this);
    close_cur_char->setShortcut(tr("Ctrl+X"));
    close_cur_char->setIcon(QIcon(":/icons/lib/icons/close_game.png"));
    close_cur_char->setStatusTip(tr("Save and close current character."));
    connect(close_cur_char, SIGNAL(triggered()), this, SLOT(save_and_close()));

    exit_npp = new QAction(tr("Exit Game"), this);
    exit_npp->setShortcut(tr("Ctrl+Q"));
    exit_npp->setIcon(QIcon(":/icons/lib/icons/Exit.png"));
    exit_npp->setStatusTip(tr("Exit the application.  Save any open character."));
    connect(exit_npp, SIGNAL(triggered()), this, SLOT(close()));

    for (int i = 0; i < MAX_RECENT_SAVEFILES; ++i)
    {
        recent_savefile_actions[i] = new QAction(this);
        recent_savefile_actions[i]->setVisible(false);
        connect(recent_savefile_actions[i], SIGNAL(triggered()),
                this, SLOT(open_recent_file()));
    }

    options_act = new QAction(tr("Options"), this);
    options_act->setStatusTip(tr("Change the game options."));
    options_act->setShortcut(Qt::Key_Equal);
    options_act->setIcon(QIcon(":/icons/lib/icons/options.png"));
    connect(options_act, SIGNAL(triggered()), this, SLOT(options_dialog()));

    hitpoint_warning_act = new QAction(tr("Set Hitpoint Warning"), this);
    hitpoint_warning_act->setStatusTip(tr("Give the player a warning when the player's current hitpoints drop below a certain percentage of maximum hitpoints."));
    connect(hitpoint_warning_act, SIGNAL(triggered()), this, SLOT(hp_warning_dialog()));

    delay_anim_factor_act = new QAction(tr("Set Animation Delay Factor"), this);
    delay_anim_factor_act->setStatusTip(tr("Set the adjustment factor for game animations."));
    connect(delay_anim_factor_act, SIGNAL(triggered()), this, SLOT(delay_anim_factor_dialog()));

    delay_run_factor_act = new QAction(tr("Set Run Delay Factor"), this);
    delay_run_factor_act->setStatusTip(tr("Set the minimum increment between run steps."));
    connect(delay_run_factor_act, SIGNAL(triggered()), this, SLOT(delay_run_factor_dialog()));

    show_targeting_act = new QAction(tr("Hide Targeting Buttons"), this);
    show_targeting_act->setStatusTip(tr("Do not display the targeting buttons in the sidebar when sleecting a target."));
    connect(show_targeting_act, SIGNAL(triggered()), this, SLOT(toggle_show_targeting()));

    show_hotkey_toolbar_act = new QAction(tr("Hide Hotkey Toolbar"), this);
    show_hotkey_toolbar_act->setStatusTip(tr("Hide the Hotkey Toolbar."));
    connect(show_hotkey_toolbar_act, SIGNAL(triggered()), this, SLOT(toggle_show_hotkey_toolbar()));

    object_squelch_act = new QAction(tr("Object Squelch Menu"), this);
    object_squelch_act->setStatusTip(tr("Modify squelch and pickup preferencs settings for all known objects."));
    connect(object_squelch_act, SIGNAL(triggered()), this, SLOT(object_squelch_menu()));

    quality_squelch_act = new QAction(tr("Quality Squelch Menu"), this);
    quality_squelch_act->setStatusTip(tr("Modify squelch settings for items upon identification or pseudo-id, based on the quality of the item."));
    connect(quality_squelch_act, SIGNAL(triggered()), this, SLOT(quality_squelch_menu()));

    ego_item_squelch_act = new QAction(tr("Ego-Item Squelch Menu"), this);
    ego_item_squelch_act->setStatusTip(tr("Modify squelch settings for ego-items upon identification."));
    connect(ego_item_squelch_act, SIGNAL(triggered()), this, SLOT(ego_item_squelch_menu()));

    keymap_new = new QAction(tr("Simplified Command Set"), this);
    keymap_new->setStatusTip(tr("Use simplified keyset to enter commands (recommended for players new to Angband and variants"));
    connect(keymap_new, SIGNAL(triggered()), this, SLOT(slot_simplified_keyset()));

    keymap_angband = new QAction(tr("Angband Command Set"), this);
    keymap_angband->setStatusTip(tr("Use the classic Angband keyset to enter commands"));
    connect(keymap_angband, SIGNAL(triggered()), this, SLOT(slot_angband_keyset()));

    keymap_rogue = new QAction(tr("Roguelike Command Set"), this);
    keymap_rogue->setStatusTip(tr("Use the roguelike keyset to enter commands"));
    connect(keymap_rogue, SIGNAL(triggered()), this, SLOT(slot_rogue_keyset()));

    ascii_mode_act = new QAction(tr("Ascii graphics"), this);
    ascii_mode_act->setStatusTip(tr("Set the graphics to ascii mode."));
    connect(ascii_mode_act, SIGNAL(triggered()), this, SLOT(set_ascii()));

    reg_mode_act = new QAction(tr("Raymond Gaustadnes tiles"), this);
    reg_mode_act->setStatusTip(tr("Set the graphics to Raymond Gaustadnes tiles mode."));
    connect(reg_mode_act, SIGNAL(triggered()), this, SLOT(set_reg()));

    dvg_mode_act = new QAction(tr("David Gervais tiles"), this);
    dvg_mode_act->setStatusTip(tr("Set the graphics to David Gervais tiles mode."));
    connect(dvg_mode_act, SIGNAL(triggered()), this, SLOT(set_dvg()));

    old_tiles_act = new QAction(tr("8x8 tiles"), this);
    old_tiles_act->setStatusTip(tr("Set the graphics to 8x8 tiles mode."));
    connect(old_tiles_act, SIGNAL(triggered()), this, SLOT(set_old_tiles()));

    graphics_25d_act = new QAction(tr("Use 2.5D graphics"), this);
    graphics_25d_act->setCheckable(true);
    graphics_25d_act->setChecked(false);
    graphics_25d_act->setStatusTip(tr("Use 2.5D graphics.  Feature development in progress."));
    connect(graphics_25d_act, SIGNAL(changed()), this, SLOT(set_25d_graphics()));
    graphics_25d_act->setDisabled(TRUE);

    pseudo_ascii_act = new QAction(tr("Pseudo-Ascii monsters"), this);
    pseudo_ascii_act->setCheckable(true);
    pseudo_ascii_act->setChecked(false);
    pseudo_ascii_act->setStatusTip(tr("Set the monsters graphics to pseudo-ascii."));
    connect(pseudo_ascii_act, SIGNAL(changed()), this, SLOT(set_pseudo_ascii()));

    wall_block_act = new QAction(tr("Solid Block Walls"), this);
    wall_block_act->setCheckable(true);
    wall_block_act->setChecked(false);
    wall_block_act->setStatusTip(tr("Display walls with a solid block instead of '#'.  (ASCII graphics only)"));
    connect(wall_block_act, SIGNAL(changed()), this, SLOT(set_wall_block()));

    font_main_select_act = new QAction(tr("Main Window Font"), this);
    font_main_select_act->setStatusTip(tr("Change the font or font size for the main window."));
    connect(font_main_select_act, SIGNAL(triggered()), this, SLOT(font_dialog_main_window()));

    font_messages_select_act = new QAction(tr("Message Window Font"), this);
    font_messages_select_act->setStatusTip(tr("Change the font or font size for the message window."));
    connect(font_messages_select_act, SIGNAL(triggered()), this, SLOT(font_dialog_message_window()));

    font_sidebar_select_act = new QAction(tr("Sidebar Window Font"), this);
    font_sidebar_select_act->setStatusTip(tr("Change the font or font size for the sidebar window."));
    connect(font_sidebar_select_act, SIGNAL(triggered()), this, SLOT(font_dialog_sidebar_window()));

    hotkey_manage = new QAction(tr("Manage Hotkeys"), this);
    hotkey_manage->setIcon(QIcon(":/icons/lib/icons/hotkeys.png"));
    hotkey_manage->setShortcut(tr("Ctrl+E"));
    hotkey_manage->setStatusTip(tr("Add, delete, or program hotkeys."));
    connect(hotkey_manage, SIGNAL(triggered()), this, SLOT(manage_hotkeys()));

    hotkey_export = new QAction(tr("Export Hotkeys"), this);
    hotkey_export->setStatusTip(tr("Save a set of hotkeys to a file."));
    connect(hotkey_export, SIGNAL(triggered()), this, SLOT(export_hotkeys()));

    hotkey_import = new QAction(tr("Import Hotkeys"), this);
    hotkey_import->setStatusTip(tr("Load a set of hotkeys from a file."));
    connect(hotkey_import, SIGNAL(triggered()), this, SLOT(import_hotkeys()));

    //Knowledge Menu
    view_monster_knowledge = new QAction(tr("View Monster Knowledge"), this);
    view_monster_knowledge->setStatusTip(tr("View all information the character knows about the monsters."));
    connect(view_monster_knowledge, SIGNAL(triggered()), this, SLOT(display_monster_info()));

    view_object_knowledge = new QAction(tr("View Object Knowledge"), this);
    view_object_knowledge->setStatusTip(tr("View all information the character knows about game objects."));
    connect(view_object_knowledge, SIGNAL(triggered()), this, SLOT(display_object_info()));

    view_ego_item_knowledge = new QAction(tr("View Ego Item Knowledge"), this);
    view_ego_item_knowledge->setStatusTip(tr("View all information the character knows about ego item objects."));
    connect(view_ego_item_knowledge, SIGNAL(triggered()), this, SLOT(display_ego_item_info()));

    view_artifact_knowledge = new QAction(tr("View Artifact Knowledge"), this);
    view_artifact_knowledge->setStatusTip(tr("View all information the character knows about artifacts."));
    connect(view_artifact_knowledge, SIGNAL(triggered()), this, SLOT(display_artifact_info()));

    view_terrain_knowledge = new QAction(tr("View Terrain Knowledge"), this);
    view_terrain_knowledge->setStatusTip(tr("View all information the character knows about terrains and features."));
    connect(view_terrain_knowledge, SIGNAL(triggered()), this, SLOT(display_terrain_info()));

    view_notes = new QAction(tr("View Game Notes"), this);
    view_notes->setStatusTip(tr("View the notes file listing the character's game highlights."));
    connect(view_notes, SIGNAL(triggered()), this, SLOT(display_notes()));

    view_messages = new QAction(tr("View Messages"), this);
    view_messages->setStatusTip(tr("View the message log."));
    view_messages->setShortcut(tr("Ctrl+L"));
    connect(view_messages, SIGNAL(triggered()), this, SLOT(display_messages()));

    view_home_inven = new QAction(tr("View Home Inventory"), this);
    view_home_inven->setStatusTip(tr("View the inventory stored in the character's home."));
    connect(view_home_inven, SIGNAL(triggered()), this, SLOT(display_home()));

    view_scores = new QAction(tr("View Player Scores"), this);
    view_scores->setStatusTip(tr("View the scores for all characters."));
    connect(view_scores, SIGNAL(triggered()), this, SLOT(display_scores()));

    view_kill_count = new QAction(tr("View Monster Kill Count"), this);
    view_kill_count->setStatusTip(tr("View the number of kills sorted by monster race."));
    connect(view_kill_count, SIGNAL(triggered()), this, SLOT(display_kill_count()));

    win_mon_list_act = new QAction(tr("Show Monster List Window"), this);
    win_mon_list_act->setStatusTip(tr("Displays a list of all the visible monsters on the level."));
    connect(win_mon_list_act, SIGNAL(triggered()), this, SLOT(toggle_win_mon_list()));

    win_obj_list_act = new QAction(tr("Show Object List Window"), this);
    win_obj_list_act->setStatusTip(tr("Displays a list of all visible objects on the level."));
    connect(win_obj_list_act, SIGNAL(triggered()), this, SLOT(toggle_win_obj_list()));

    win_mon_recall_act = new QAction(tr("Show Monster Recall Window"), this);
    win_mon_recall_act->setStatusTip(tr("Displays all known information about a given monster race."));
    connect(win_mon_recall_act, SIGNAL(triggered()), this, SLOT(toggle_win_mon_recall()));

    win_obj_recall_act = new QAction(tr("Show Object Recall Window"), this);
    win_obj_recall_act->setStatusTip(tr("Displays all known information about a given object."));
    connect(win_obj_recall_act, SIGNAL(triggered()), this, SLOT(toggle_win_obj_recall()));

    win_feat_recall_act = new QAction(tr("Show Feature Recall Window"), this);
    win_feat_recall_act->setStatusTip(tr("Displays all known information about a given feature."));
    connect(win_feat_recall_act, SIGNAL(triggered()), this, SLOT(toggle_win_feat_recall()));

    win_messages_act = new QAction(tr("Show Message Window"), this);
    win_messages_act->setStatusTip(tr("Displays all recent messages."));
    connect(win_messages_act, SIGNAL(triggered()), this, SLOT(toggle_win_messages()));

    win_char_basic_act = new QAction(tr("Show Basic Character Information"), this);
    win_char_basic_act ->setStatusTip(tr("Display basic character information."));
    connect(win_char_basic_act , SIGNAL(triggered()), this, SLOT(toggle_win_char_basic_frame()));

    win_char_equip_info_act = new QAction(tr("Show Character Equipment Information"), this);
    win_char_equip_info_act->setStatusTip(tr("Display character equipment resistance and stat modifier information."));
    connect(win_char_equip_info_act, SIGNAL(triggered()), this, SLOT(toggle_win_char_equip_frame()));

    win_char_equipment_act = new QAction(tr("Show Character Equipment Screen"), this);
    win_char_equipment_act->setStatusTip(tr("Display character equipment screen."));
    connect(win_char_equipment_act, SIGNAL(triggered()), this, SLOT(toggle_win_char_equipment_frame()));

    win_char_inventory_act = new QAction(tr("Show Character Inventory Screen"), this);
    win_char_inventory_act->setStatusTip(tr("Display character Inventory screen."));
    connect(win_char_inventory_act, SIGNAL(triggered()), this, SLOT(toggle_win_char_inventory_frame()));

    win_dun_map_act = new QAction(tr("Show Map Window"), this);
    win_dun_map_act->setStatusTip(tr("Display map window."));
    connect(win_dun_map_act, SIGNAL(triggered()), this, SLOT(toggle_win_dun_map_frame()));

    win_overhead_map_act = new QAction(tr("Show Overhead Map"), this);
    win_overhead_map_act->setStatusTip(tr("Display overhead map."));
    win_overhead_map_act->setShortcut(tr("Alt+M"));
    connect(win_overhead_map_act, SIGNAL(triggered()), this, SLOT(toggle_win_overhead_map_frame()));

    help_about = new QAction(tr("&About"), this);
    help_about->setStatusTip(tr("Show the application's About box"));
    connect(help_about, SIGNAL(triggered()), this, SLOT(about()));

    help_about_Qt = new QAction(tr("About &Qt"), this);
    help_about_Qt->setStatusTip(tr("Show the Qt library's About box"));
    connect(help_about_Qt, SIGNAL(triggered()), qApp, SLOT(aboutQt()));

    help_command_list = new QAction(tr("&Show Keyboard Commands"), this);
    help_command_list->setShortcut(Qt::Key_Question);
    help_command_list->setStatusTip(tr("Show a list of all keybord commands"));
    connect(help_command_list, SIGNAL(triggered()), this, SLOT(command_list_keyboard()));

    help_mouse_list = new QAction(tr("&Show Mouse Commands"), this);
    help_mouse_list->setShortcut(Qt::Key_Slash);
    help_mouse_list->setStatusTip(tr("Show a list of all keybord commands"));
    connect(help_mouse_list, SIGNAL(triggered()), this, SLOT(command_list_mouse()));

    help_targeting_list = new QAction(tr("&Show Targeting Commands"), this);
    help_targeting_list->setShortcut(Qt::Key_Backslash);
    help_targeting_list->setStatusTip(tr("Show a list of all targeting commands"));
    connect(help_targeting_list, SIGNAL(triggered()), this, SLOT(command_list_targeting()));
}

void MainWindow::set_reg()
{
    set_graphic_mode(GRAPHICS_RAYMOND_GAUSTADNES);
}

void MainWindow::set_dvg()
{
    set_graphic_mode(GRAPHICS_DAVID_GERVAIS);
}

void MainWindow::set_old_tiles()
{
    set_graphic_mode(GRAPHICS_ORIGINAL);
}

void MainWindow::set_ascii()
{
    set_graphic_mode(GRAPHICS_NONE);
}

void MainWindow::set_25d_graphics()
{
    do_25d_graphics = graphics_25d_act->isChecked();
}

void MainWindow::set_wall_block()
{
    do_wall_block = wall_block_act->isChecked();
    ui_redraw_all();
}

void MainWindow::set_pseudo_ascii()
{
    do_pseudo_ascii = pseudo_ascii_act->isChecked();
    ui_redraw_all();
}

void MainWindow::display_monster_info()
{
    display_monster_knowledge();
}

void MainWindow::display_object_info()
{
    display_object_knowledge();
}

void MainWindow::display_ego_item_info()
{
    display_ego_item_knowledge();
}

void MainWindow::display_artifact_info()
{
    display_artifact_knowledge();
}

void MainWindow::display_terrain_info()
{
    display_terrain_knowledge();
}

void MainWindow::display_notes()
{
    display_notes_file();
}

void MainWindow::display_messages()
{
    display_message_log();
}

void MainWindow::display_home()
{
    display_home_inventory();
}

void MainWindow::display_scores()
{
    display_player_scores();
}

void MainWindow::display_kill_count()
{
    display_mon_kill_count();
}

void MainWindow::timed_events()
{
    if (!animate_flicker) return;
    if (!character_dungeon) return;
    if (executing_command) return;
    do_animation();
}

// Call to handle a single slick (so they are not confused with double clicks)
void MainWindow::single_click_events()
{
    grids[single_mouseclick_info.mouse_click_y][single_mouseclick_info.mouse_click_x]->handle_single_click(single_mouseclick_info);
}


//  Set's up many of the keystrokes and commands used during the game.
void MainWindow::create_signals()
{
    // currently empty
}

void MainWindow::slot_multiplier_clicked(QAction *action)
{
    if (action) main_multiplier = action->objectName();
    QList<QString> parts = main_multiplier.split(":");
    if (parts.size() == 2)
    {
        qreal x = parts.at(1).toFloat();
        qreal y = parts.at(0).toFloat();
        graphics_view->setTransform(QTransform::fromScale(x, y));
    }
}


//Actually add the QActions intialized in create_actions to the menu
void MainWindow::create_menus()
{
    //File section of top menu.
    file_menu = menuBar()->addMenu(tr("&File"));
    file_menu->addAction(new_game_nppangband);
    file_menu->addAction(new_game_nppmoria);
    file_menu->addAction(open_savefile);
    separator_act = file_menu->addSeparator();
    file_menu->addAction(save_cur_char);
    file_menu->addAction(save_cur_char_as);
    file_menu->addAction(close_cur_char);
    separator_act = file_menu->addSeparator();
    file_menu->addAction(exit_npp);
    separator_act = file_menu->addSeparator();    
    for (int i = 0; i < MAX_RECENT_SAVEFILES; ++i)
        file_menu->addAction(recent_savefile_actions[i]);
    separator_act = file_menu->addSeparator();

    update_recent_savefiles();

    menuBar()->addSeparator();

    settings = menuBar()->addMenu(tr("&Settings"));
    settings->addAction(options_act);

    settings->addAction(show_targeting_act);
    settings->addAction(show_hotkey_toolbar_act);

    separator_act = settings->addSeparator();

    QMenu *choose_keymap = settings->addMenu("Choose Keyset");
    choose_keymap->addAction(keymap_new);
    choose_keymap->addAction(keymap_angband);
    choose_keymap->addAction(keymap_rogue);
    keymap_choice = new QActionGroup(this);
    keymap_choice->setExclusive(TRUE);
    keymap_choice->addAction(keymap_new);
    keymap_choice->addAction(keymap_angband);
    keymap_choice->addAction(keymap_rogue);
    keymap_new->setCheckable(TRUE);
    keymap_angband->setCheckable(TRUE);
    keymap_rogue->setCheckable(TRUE);
    keymap_new->setChecked(TRUE);

    separator_act = settings->addSeparator();

    QMenu *hotkey_choices = settings->addMenu("Hotkey Settings");
    hotkey_choices->addAction(hotkey_manage);
    hotkey_choices->addAction(hotkey_export);
    hotkey_choices->addAction(hotkey_import);

    separator_act = settings->addSeparator();

    settings->addAction(object_squelch_act);
    settings->addAction(quality_squelch_act);
    settings->addAction(ego_item_squelch_act);

    separator_act = settings->addSeparator();

    settings->addAction(hitpoint_warning_act);
    settings->addAction(delay_anim_factor_act);
    settings->addAction(delay_run_factor_act);


    // Knowledge section of top menu.
    knowledge = menuBar()->addMenu(tr("&Knowledge"));
    knowledge->addAction(view_monster_knowledge);
    knowledge->addAction(view_object_knowledge);
    knowledge->addAction(view_ego_item_knowledge);
    knowledge->addAction(view_artifact_knowledge);
    knowledge->addAction(view_terrain_knowledge);
    knowledge->addAction(view_notes);
    knowledge->addAction(view_messages);
    knowledge->addAction(view_home_inven);
    knowledge->addAction(view_scores);
    knowledge->addAction(view_kill_count);

    //Tileset options
    display = menuBar()->addMenu(tr("&Display"));
    QMenu *choose_fonts = display->addMenu("Choose Fonts");
    choose_fonts->addAction(font_main_select_act);
    choose_fonts->addAction(font_messages_select_act);
    choose_fonts->addAction(font_sidebar_select_act);
    QMenu *choose_tile_set = display->addMenu("Choose Tile Set");
    choose_tile_set->addAction(ascii_mode_act);
    choose_tile_set->addAction(reg_mode_act);
    choose_tile_set->addAction(dvg_mode_act);
    choose_tile_set->addAction(old_tiles_act);
    display->addAction(graphics_25d_act);
    display->addAction(wall_block_act);
    display->addAction(pseudo_ascii_act);
    tiles_choice = new QActionGroup(this);
    tiles_choice->setExclusive(TRUE);
    tiles_choice->addAction(ascii_mode_act);
    tiles_choice->addAction(reg_mode_act);
    tiles_choice->addAction(dvg_mode_act);
    tiles_choice->addAction(old_tiles_act);
    ascii_mode_act->setCheckable(TRUE);
    ascii_mode_act->setChecked(TRUE);
    reg_mode_act->setCheckable(TRUE);
    dvg_mode_act->setCheckable(TRUE);
    old_tiles_act->setCheckable(TRUE);

    QPointer<QMenu> submenu = display->addMenu(tr("Tile multiplier"));
    multipliers = new QActionGroup(this);

    for (int i = 0; !mult_list[i].isEmpty(); i++)
    {
        QAction *act = submenu->addAction(mult_list[i]);
        act->setObjectName(mult_list[i]);
        act->setCheckable(true);
        multipliers->addAction(act);
        if (i == TILE_1x1_MULT) act->setChecked(true);
    }
    connect(multipliers, SIGNAL(triggered(QAction*)), this, SLOT(slot_multiplier_clicked(QAction*)));


    QPointer<QAction> act = display->addAction(tr("Create tile package"));
    connect(act, SIGNAL(triggered()), this, SLOT(do_create_package()));

    act = display->addAction(tr("Extract tiles from package"));
    connect(act, SIGNAL(triggered()), this, SLOT(do_extract_from_package()));

    win_menu = menuBar()->addMenu(tr("&Windows"));
    win_menu->addAction(win_mon_list_act);
    win_menu->addAction(win_obj_list_act);
    win_menu->addAction(win_mon_recall_act);
    win_menu->addAction(win_obj_recall_act);
    win_menu->addAction(win_feat_recall_act);
    win_menu->addAction(win_messages_act);
    win_menu->addAction(win_char_basic_act);
    win_menu->addAction(win_char_equip_info_act);
    win_menu->addAction(win_char_equipment_act);
    win_menu->addAction(win_char_inventory_act);
    win_menu->addAction(win_dun_map_act);
    win_menu->addAction(win_overhead_map_act);

    // Help section of top menu.
    help_menu = menuBar()->addMenu(tr("&Help"));
    help_menu->addAction(help_about);
    help_menu->addAction(help_about_Qt);
    help_menu->addAction(help_command_list);
    help_menu->addAction(help_mouse_list);
    help_menu->addAction(help_targeting_list);
}

// Create the toolbars
void MainWindow::create_toolbars()
{
    file_toolbar = addToolBar(tr("&File"));
    file_toolbar->setObjectName(QString("file_toolbar"));

    file_toolbar->addAction(new_game_nppangband);
    file_toolbar->addAction(new_game_nppmoria);
    file_toolbar->addAction(open_savefile);    
    file_toolbar->addSeparator();
    file_toolbar->addAction(save_cur_char);
    file_toolbar->addAction(save_cur_char_as);
    file_toolbar->addAction(close_cur_char);
    file_toolbar->addAction(options_act);
    file_toolbar->addAction(hotkey_manage);
    file_toolbar->addSeparator();
    file_toolbar->addAction(exit_npp);

    create_statusbar();
    create_hotkey_toolbar();
}

// Just find an initial font to start the game
// User preferences will be saved with the game.
void MainWindow::select_font()
{
    bool have_font = FALSE;

    foreach (QString family, font_database.families())
    {
        if (font_database.isFixedPitch(family))
        {
            font_database.addApplicationFont(family);
            if (have_font) continue;
            font_main_window = QFont(family);
            font_message_window = QFont(family);
            font_sidebar_window = QFont(family);
            win_mon_list_settings.win_font = QFont(family);
            win_obj_list_settings.win_font = QFont(family);
            win_mon_recall_settings.win_font = QFont(family);
            win_obj_recall_settings.win_font = QFont(family);
            win_feat_recall_settings.win_font = QFont(family);
            win_message_settings.win_font = QFont(family);
            char_info_basic_settings.win_font = QFont(family);
            char_info_equip_settings.win_font = QFont(family);
            char_equipment_settings.win_font = QFont(family);
            char_inventory_settings.win_font = QFont(family);
            dun_map_settings.win_font = QFont(family);
            overhead_map_settings.win_font = QFont(family);
            have_font = TRUE;
        }
    }

    font_main_window.setPointSize(12);
    font_message_window.setPointSize(12);
    font_sidebar_window.setPointSize(12);
    win_mon_list_settings.win_font.setPointSize(12);
    win_obj_list_settings.win_font.setPointSize(12);
    win_mon_recall_settings.win_font.setPointSize(12);
    win_obj_recall_settings.win_font.setPointSize(12);
    win_message_settings.win_font.setPointSize(12);
    win_message_settings.win_font.setPointSize(12);
    char_info_basic_settings.win_font.setPointSize(12);
    char_info_equip_settings.win_font.setPointSize(12);
    char_equipment_settings.win_font.setPointSize(12);
    char_inventory_settings.win_font.setPointSize(12);
    dun_map_settings.win_font.setPointSize(10);
    overhead_map_settings.win_font.setPointSize(8);
}


// Read and write the game settings.
// Every entry in write-settings should have a corresponding entry in read_settings.
void MainWindow::read_settings()
{
    QSettings settings("NPPGames", "NPPQT");

    QWidget dummy_widget;

    dummy_widget.restoreGeometry(settings.value("mainWindowGeometry").toByteArray());
    win_geometry = dummy_widget.geometry();
    win_maximized = (settings.value("mainWindowMaximized", false).toBool());

    recent_savefiles = settings.value("recentFiles").toStringList();
    show_targeting_buttons = settings.value("target_buttons", false).toBool();
    if (!show_targeting_buttons)
    {   // First set it to true to it toggles correctly.
        show_targeting_buttons = TRUE;
        toggle_show_targeting();
    }
    show_hotkey_toolbar = settings.value("hotkey_toolbar", false).toBool();
    if (!show_hotkey_toolbar)
    {
        // First set it to true to it toggles correctly.
        show_hotkey_toolbar = TRUE;
        toggle_show_hotkey_toolbar();
    }
    do_25d_graphics = settings.value("graphics_25d", false).toBool();
    graphics_25d_act->setChecked(do_25d_graphics);
    do_pseudo_ascii = settings.value("pseudo_ascii", false).toBool();
    pseudo_ascii_act->setChecked(do_pseudo_ascii);
    do_wall_block = settings.value("solid_block", false).toBool();
    wall_block_act->setChecked(do_wall_block);
    use_graphics = settings.value("use_graphics", 0).toInt();
    which_keyset = settings.value("which_keyset", 0).toInt();
    main_multiplier = settings.value("tile_multiplier", "1:1").toString();
    QAction *act = this->findChild<QAction *>(main_multiplier);
    if (act)
    {
        act->setChecked(true);
        slot_multiplier_clicked(act);
    }

    QString load_font = settings.value("font_window_main", font_main_window ).toString();
    font_main_window.fromString(load_font);
    load_font = settings.value("font_window_messages", font_message_window ).toString();
    font_message_window.fromString(load_font);
    load_font = settings.value("font_window_sidebar", font_sidebar_window ).toString();
    font_sidebar_window.fromString(load_font);


    restoreState(settings.value("window_state").toByteArray());

    /*
     * Before reading the geometry of the windows, add a toolbar to the dummy widget
     * So the widgets appear in the same position upon reloading.
     */
    QVBoxLayout dummy_vlay;
    dummy_widget.setLayout(&dummy_vlay);
    QMenuBar dummy_menubar;
    dummy_vlay.setMenuBar(&dummy_menubar);

    // Monster List window settings
    win_mon_list_settings.win_show = settings.value("show_mon_list_window", false).toBool();
    dummy_widget.restoreGeometry(settings.value("winMonListGeometry").toByteArray());
    win_mon_list_settings.win_geometry = dummy_widget.geometry();
    win_mon_list_settings.win_maximized = settings.value("winMonListMaximized", false).toBool();
    load_font = settings.value("font_mon_list_recall", win_mon_list_settings.win_font ).toString();
    win_mon_list_settings.win_font.fromString(load_font);
    if (win_mon_list_settings.win_show)
    {
        win_mon_list_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_mon_list();
    }


    // Object List window settings
    win_obj_list_settings.win_show = settings.value("show_obj_list_window", false).toBool();
    dummy_widget.restoreGeometry(settings.value("winObjListGeometry").toByteArray());
    win_obj_list_settings.win_geometry = dummy_widget.geometry();
    win_obj_list_settings.win_maximized = settings.value("winObjListMaximized", false).toBool();
    load_font = settings.value("font_obj_list_recall", win_obj_list_settings.win_font ).toString();
    win_obj_list_settings.win_font.fromString(load_font);
    if (win_obj_list_settings.win_show)
    {
        win_obj_list_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_obj_list();
    }


    // Monster recall window settings
    win_mon_recall_settings.win_show = settings.value("show_mon_recall_window", false).toBool();
    dummy_widget.restoreGeometry(settings.value("winMonRecallGeometry").toByteArray());
    win_mon_recall_settings.win_geometry = dummy_widget.geometry();
    win_mon_recall_settings.win_maximized = settings.value("winMonRecallMaximized", false).toBool();
    load_font = settings.value("font_window_mon_recall", win_mon_recall_settings.win_font ).toString();
    win_mon_recall_settings.win_font.fromString(load_font);
    if (win_mon_recall_settings.win_show)
    {
        win_mon_recall_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_mon_recall();
    }


    // Object recall window settings
    win_obj_recall_settings.win_show = settings.value("show_obj_recall_window", false).toBool();
    dummy_widget.restoreGeometry(settings.value("winObjRecallGeometry").toByteArray());
    win_obj_recall_settings.win_geometry = dummy_widget.geometry();
    win_obj_recall_settings.win_maximized = settings.value("winObjRecallMaximized", false).toBool();
    load_font = settings.value("font_window_obj_recall", win_obj_recall_settings.win_font ).toString();
    win_obj_recall_settings.win_font.fromString(load_font);
    if (win_obj_recall_settings.win_show)
    {
        win_obj_recall_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_obj_recall();
    }

    // Feature recall window settings
    win_feat_recall_settings.win_show = settings.value("show_feat_recall_window", false).toBool();
    dummy_widget.restoreGeometry(settings.value("winFeatRecallGeometry").toByteArray());
    win_feat_recall_settings.win_geometry = dummy_widget.geometry();
    win_feat_recall_settings.win_maximized = settings.value("winFeatRecallMaximized", false).toBool();
    load_font = settings.value("font_window_feat_recall", win_feat_recall_settings.win_font ).toString();
    win_feat_recall_settings.win_font.fromString(load_font);
    if (win_feat_recall_settings.win_show)
    {
        win_feat_recall_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_feat_recall();
    }

    // Messages window settings
    win_message_settings.win_show = settings.value("show_messages_window", false).toBool();
    dummy_widget.restoreGeometry(settings.value("winMessagesGeometry").toByteArray());
    win_message_settings.win_geometry = dummy_widget.geometry();
    win_message_settings.win_maximized = settings.value("winMessagesMaximized", false).toBool();
    load_font = settings.value("font_messages_window", win_message_settings.win_font ).toString();
    win_message_settings.win_font.fromString(load_font);
    if (win_message_settings.win_show)
    {
        win_message_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_messages();
    }


    // Character Basic Information window settings
    char_info_basic_settings.win_show = settings.value("show_char_info_basic_window", false).toBool();
    dummy_widget.restoreGeometry(settings.value("winCharBasicGeometry").toByteArray());
    char_info_basic_settings.win_geometry = dummy_widget.geometry();
    char_info_basic_settings.win_maximized = settings.value("winCharBasicMaximized", false).toBool();
    load_font = settings.value("font_char_info_basic", char_info_basic_settings.win_font ).toString();
    char_info_basic_settings.win_font.fromString(load_font);
    if (char_info_basic_settings.win_show)
    {
        char_info_basic_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_char_basic_frame();
    }


    // Character Equipment Information window settings
    char_info_equip_settings.win_show = settings.value("show_char_info_equip_window", false).toBool();
    dummy_widget.restoreGeometry(settings.value("winCharInfoEquipGeometry").toByteArray());
    char_info_equip_settings.win_geometry = dummy_widget.geometry();
    char_info_equip_settings.win_maximized = settings.value("winCharInfoEquipMaximized", false).toBool();
    load_font = settings.value("font_char_info_equip", char_info_equip_settings.win_font ).toString();
    char_info_equip_settings.win_font.fromString(load_font);
    if (char_info_equip_settings.win_show)
    {
        char_info_equip_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_char_equip_frame();
    }

    // Character Equipment window settings
    char_equipment_settings.win_show = settings.value("show_char_equipment_window", false).toBool();
    equip_show_buttons = settings.value("show_equip_window_buttons", false).toBool();
    dummy_widget.restoreGeometry(settings.value("winCharEquipmentGeometry").toByteArray());
    char_equipment_settings.win_geometry = dummy_widget.geometry();
    char_equipment_settings.win_maximized = settings.value("winCharEquipmentMaximized", false).toBool();
    load_font = settings.value("font_char_equipment", char_equipment_settings.win_font ).toString();
    char_equipment_settings.win_font.fromString(load_font);
    if (char_equipment_settings.win_show)
    {
        char_equipment_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_char_equipment_frame();
    }

    // Character Inventory window settings
    char_inventory_settings.win_show = settings.value("show_char_inventory_window", false).toBool();
    inven_show_buttons = settings.value("show_inven_window_buttons", false).toBool();
    dummy_widget.restoreGeometry(settings.value("winCharInventoryGeometry").toByteArray());
    char_inventory_settings.win_geometry = dummy_widget.geometry();
    char_inventory_settings.win_maximized = settings.value("winCharInventoryMaximized", false).toBool();
    load_font = settings.value("font_char_inventory", char_inventory_settings.win_font ).toString();
    char_inventory_settings.win_font.fromString(load_font);
    if (char_inventory_settings.win_show)
    {
        char_inventory_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_char_inventory_frame();
    }


    // Dungeon map window settings
    dun_map_settings.win_show = settings.value("show_dun_map_window", false).toBool();
    dun_map_use_graphics = settings.value("graphics_dun_map", false).toBool();
    dun_map_multiplier = settings.value("dun_map_tile_multiplier", "1:1").toString();
    dummy_widget.restoreGeometry(settings.value("winDunMapGeometry").toByteArray());
    dun_map_settings.win_geometry = dummy_widget.geometry();
    dun_map_settings.win_maximized = settings.value("winDunMapMaximized", false).toBool();
    load_font = settings.value("font_dun_map", dun_map_settings.win_font).toString();
    dun_map_settings.win_font.fromString(load_font);

    if (dun_map_settings.win_show)
    {
        dun_map_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_dun_map_frame();
    }

    // Overhead map window settings
    overhead_map_settings.win_show = settings.value("show_dun_overhead_window", false).toBool();
    overhead_map_use_graphics = settings.value("graphics_overhead_map", false).toBool();
    overhead_map_multiplier = settings.value("dun_overhead_tile_multiplier", "1:1").toString();
    dummy_widget.restoreGeometry(settings.value("winOverheadMapGeometry").toByteArray());
    overhead_map_settings.win_geometry = dummy_widget.geometry();
    overhead_map_settings.win_maximized = settings.value("winOverheadMapMaximized", false).toBool();
    load_font = settings.value("font_overhead_map", overhead_map_settings.win_font ).toString();
    overhead_map_settings.win_font.fromString(load_font);
    if (overhead_map_settings.win_show)
    {
        overhead_map_settings.win_show = FALSE; //hack - so it gets toggled to true
        toggle_win_overhead_map_frame();
    }

    update_recent_savefiles();
}

/*
 *  Note all the extra windows are destroyed before this function is
 *  called so the latest windows settings are recorded.
 */

void MainWindow::write_settings()
{
    QWidget dummy_widget;

    QSettings settings("NPPGames", "NPPQT");
    dummy_widget.setGeometry(geometry());
    settings.setValue("mainWindowGeometry", dummy_widget.saveGeometry());
    settings.setValue("mainWindowMaximized", main_window->isMaximized());
    settings.setValue("recentFiles", recent_savefiles);
    settings.setValue("target_buttons", show_targeting_buttons);
    settings.setValue("hotkey_toolbar", show_hotkey_toolbar);
    settings.setValue("graphics_25d", do_25d_graphics);
    settings.setValue("pseudo_ascii", do_pseudo_ascii);
    settings.setValue("solid_block", do_wall_block);
    settings.setValue("use_graphics", use_graphics);
    settings.setValue("which_keyset", which_keyset);
    settings.setValue("tile_multiplier", main_multiplier);
    settings.setValue("font_window_main", font_main_window.toString());
    settings.setValue("font_window_messages", font_message_window.toString());
    settings.setValue("font_window_sidebar", font_sidebar_window.toString());


    settings.setValue("window_state", saveState());


    /*
     * Before saving the geometry of the windows, add a toolbar to the dummy widget
     * So the widgets appear in the same position upon reloading.
     */
    QVBoxLayout dummy_vlay;
    dummy_widget.setLayout(&dummy_vlay);
    QMenuBar dummy_menubar;
    dummy_vlay.setMenuBar(&dummy_menubar);

    // Monster List window settings
    settings.setValue("show_mon_list_window", win_mon_list_settings.win_show);
    dummy_widget.setGeometry(win_mon_list_settings.win_geometry);
    settings.setValue("winMonListGeometry", dummy_widget.saveGeometry());
    settings.setValue("winMonListMaximized", win_mon_list_settings.win_maximized);
    settings.setValue("font_mon_list_recall", win_mon_list_settings.win_font.toString());


    // Object List window settings
    settings.setValue("show_obj_list_window", win_obj_list_settings.win_show);
    dummy_widget.setGeometry(win_obj_list_settings.win_geometry);
    settings.setValue("winObjListGeometry", dummy_widget.saveGeometry());
    settings.setValue("winObjListMaximized", win_obj_list_settings.win_maximized);
    settings.setValue("font_obj_list_recall", win_obj_list_settings.win_font.toString());


    // Monster recall window settings
    settings.setValue("show_mon_recall_window", win_mon_recall_settings.win_show);
    dummy_widget.setGeometry(win_mon_recall_settings.win_geometry);
    settings.setValue("winMonRecallGeometry", dummy_widget.saveGeometry());
    settings.setValue("winMonRecallMaximized", win_mon_recall_settings.win_maximized);
    settings.setValue("font_window_mon_recall", win_mon_recall_settings.win_font.toString());


    // Object recall window settings
    settings.setValue("show_obj_recall_window", win_obj_recall_settings.win_show);
    dummy_widget.setGeometry(win_obj_recall_settings.win_geometry);
    settings.setValue("winObjRecallGeometry", dummy_widget.saveGeometry());
    settings.setValue("winObjRecallMaximized", win_obj_recall_settings.win_maximized);
    settings.setValue("font_window_obj_recall", win_obj_recall_settings.win_font.toString());


    // Feature recall window settings
    settings.setValue("show_feat_recall_window", win_feat_recall_settings.win_show);
    dummy_widget.setGeometry(win_feat_recall_settings.win_geometry);
    settings.setValue("winFeatRecallGeometry", dummy_widget.saveGeometry());
    settings.setValue("winFeatRecallMaximized", win_feat_recall_settings.win_maximized);
    settings.setValue("font_window_feat_recall", win_feat_recall_settings.win_font.toString());


    // Messages window settings
    settings.setValue("show_messages_window", win_message_settings.win_show);
    dummy_widget.setGeometry(win_message_settings.win_geometry);
    settings.setValue("winMessagesGeometry", dummy_widget.saveGeometry());
    settings.setValue("winMessagesMaximized", win_message_settings.win_maximized);
    settings.setValue("font_messages_window", win_message_settings.win_font.toString());


    // Character Basic Information window settings
    settings.setValue("show_char_info_basic_window", char_info_basic_settings.win_show);
    dummy_widget.setGeometry(char_info_basic_settings.win_geometry);
    settings.setValue("winCharBasicGeometry", dummy_widget.saveGeometry());
    settings.setValue("winCharBasicMaximized", char_info_basic_settings.win_maximized);
    settings.setValue("font_char_info_basic", char_info_basic_settings.win_font.toString());


    // Character Equipment Information window settings
    settings.setValue("show_char_info_equip_window", char_info_equip_settings.win_show);
    dummy_widget.setGeometry(char_info_equip_settings.win_geometry);
    settings.setValue("winCharInfoEquipGeometry", dummy_widget.saveGeometry());
    settings.setValue("winCharInfoEquipMaximized", char_info_equip_settings.win_maximized);
    settings.setValue("font_char_info_equip", char_info_equip_settings.win_font.toString());

    // Character Equipment window settings
    settings.setValue("show_char_equipment_window", char_equipment_settings.win_show);
    settings.setValue("show_equip_window_buttons", equip_show_buttons);
    dummy_widget.setGeometry(char_equipment_settings.win_geometry);
    settings.setValue("winCharEquipmentGeometry", dummy_widget.saveGeometry());
    settings.setValue("winCharEquipmentMaximized", char_equipment_settings.win_maximized);
    settings.setValue("font_char_equipment", char_equipment_settings.win_font.toString());


    // Character Inventory window settings
    settings.setValue("show_char_inventory_window", char_inventory_settings.win_show);
    settings.setValue("show_inven_window_buttons", inven_show_buttons);
    dummy_widget.setGeometry(char_inventory_settings.win_geometry);
    settings.setValue("winCharInventoryGeometry", dummy_widget.saveGeometry());
    settings.setValue("winCharInventoryMaximized", char_inventory_settings.win_maximized);
    settings.setValue("font_char_inventory", char_inventory_settings.win_font.toString());


    // Dungeon map window settings
    settings.setValue("show_dun_map_window", dun_map_settings.win_show);
    settings.setValue("graphics_dun_map", dun_map_use_graphics);
    settings.setValue("dun_map_tile_multiplier", dun_map_multiplier);
    dummy_widget.setGeometry(dun_map_settings.win_geometry);
    settings.setValue("winDunMapGeometry", dummy_widget.saveGeometry());
    settings.setValue("winDunMapMaximized", dun_map_settings.win_maximized);
    settings.setValue("font_dun_map", dun_map_settings.win_font.toString());

    // Overhead map window settings
    settings.setValue("show_dun_overhead_window", overhead_map_settings.win_show);
    settings.setValue("graphics_overhead_map", overhead_map_use_graphics);
    settings.setValue("dun_overhead_tile_multiplier", overhead_map_multiplier);
    dummy_widget.setGeometry(overhead_map_settings.win_geometry);
    settings.setValue("winOverheadMapGeometry", dummy_widget.saveGeometry());
    settings.setValue("winOverheadMapMaximized", overhead_map_settings.win_maximized);
    settings.setValue("font_overhead_map", overhead_map_settings.win_font.toString());
}


void MainWindow::load_file(const QString &file_name)
{    
    if (!file_name.isEmpty())
    {
        set_current_savefile(file_name);

        //make sure we have a valid game_mode
        game_mode = GAME_MODE_UNDEFINED;
        load_gamemode();
        if (game_mode == GAME_MODE_UNDEFINED) return;

        // Initialize game then load savefile
        if (game_mode == GAME_NPPANGBAND) setup_nppangband();
        else if (game_mode == GAME_NPPMORIA) setup_nppmoria();

        if (load_player())
        {
            //update_file_menu_game_active();
            statusBar()->showMessage(tr("File loaded"), 2000);

            if (!character_loaded) message_list.clear();
            update_messages();

            if (!character_loaded)
            {
                save_prev_character();
                launch_birth(TRUE);
            }
            else
            {
                update_file_menu_game_active();
                launch_game();
                graphics_view->setFocus();
                update_sidebar_font();

                // Now that we have a character, fill in the char info window
                if (char_info_basic_settings.win_show) create_win_char_info();
                if (char_info_equip_settings.win_show) create_win_char_equip_info();
                if (char_equipment_settings.win_show) create_win_char_equipment();
                if (char_inventory_settings.win_show) create_win_char_inventory();
                if (dun_map_settings.win_show) create_win_dun_map();
                if (overhead_map_settings.win_show) create_win_overhead_map();
                ui_player_moved();

                //hack - draw everything
                ui_redraw_all();
            }

            event_timer->start();
        }
    }
    else
    {
        QMessageBox::warning(this, tr("Recent Files"), tr("Cannot read file %1").arg(file_name));
        return;
    }
}

void MainWindow::launch_birth(bool quick_start)
{
    PlayerBirth *dlg = new PlayerBirth(quick_start);

    if (p_ptr->game_turn)
    {
        update_file_menu_game_active();
        launch_game();
        save_character();
        graphics_view->setFocus();
        update_sidebar_font();
        if (char_info_basic_settings.win_show) create_win_char_info();
        if (char_info_equip_settings.win_show) create_win_char_equip_info();
        if (char_equipment_settings.win_show) create_win_char_equipment();
        if (char_inventory_settings.win_show) create_win_char_inventory();
        if (dun_map_settings.win_show) create_win_dun_map();
        if (overhead_map_settings.win_show) create_win_overhead_map();
        ui_player_moved();

        // The main purpose of this greeting is to avoid crashes
        // due to the message vector being empty.
        message(QString("Welcome %1") .arg(op_ptr->full_name));

        //hack - draw everything
        ui_redraw_all();

        event_timer->start();
    }
    else
    {
        cleanup_npp_games();
        character_loaded = false;
        current_savefile.clear();
    }
    delete dlg;


}

void MainWindow::save_file(const QString &file_name)
{
    set_current_savefile(file_name);

    if (!save_player())
    {
        QMessageBox::warning(this, tr("Recent Files"), tr("Cannot write file %1").arg(file_name));
        return;
    }

    statusBar()->showMessage(tr("File saved"), 2000);
}

void MainWindow::set_current_savefile(const QString &file_name)
{
    current_savefile = file_name;
    setWindowModified(false);

    QString shownName = "Untitled";
    if (!current_savefile.isEmpty()) {
        shownName = stripped_name(current_savefile);
        recent_savefiles.removeAll(current_savefile);
        recent_savefiles.prepend(current_savefile);
        update_recent_savefiles();
    }
}

// Update the 5 most recently played savefile list.
void MainWindow::update_recent_savefiles()
{
    QMutableStringListIterator i(recent_savefiles);
    while (i.hasNext()) {
        if (!QFile::exists(i.next()))
            i.remove();
    }

    for (int j = 0; j < MAX_RECENT_SAVEFILES; ++j)
    {
        if (j < recent_savefiles.count())
        {
            QString text = tr("&%1 %2")
                           .arg(j + 1)
                           .arg(stripped_name(recent_savefiles[j]));
            recent_savefile_actions[j]->setText(text);
            recent_savefile_actions[j]->setData(recent_savefiles[j]);
            recent_savefile_actions[j]->setVisible(true);
        }
        else
        {
            recent_savefile_actions[j]->setVisible(false);
        }
    }
    separator_act->setVisible(!recent_savefiles.isEmpty());
}

QString MainWindow::stripped_name(const QString &full_file_name)
{
    return QFileInfo(full_file_name).fileName();
}

void MainWindow::save_png_screenshot(void)
{
    QRect dungeon_frame = graphics_view->geometry();

    QRect screen_grab(sidebar_widget->pos(), dungeon_frame.bottomRight());

    QPixmap screenshot = main_window->grab(screen_grab);

    // Start with the current player name
    QString default_name = "player";
    if (!op_ptr->full_name.isEmpty())default_name = op_ptr->full_name;
    QString default_file = npp_dir_user.path();
    default_file.append("/");
    default_file.append(default_name);
    default_file.append("_npp_scr");

    QString file_name = QFileDialog::getSaveFileName(this, tr("Save PNG Screenshot As"), default_file, tr("PNG (*.png)"));

    if (file_name.isEmpty())
        return;

    screenshot.save(file_name, "PNG", 100);

}
