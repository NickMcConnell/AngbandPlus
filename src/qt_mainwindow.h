#ifndef QT_MAINWINDOW_H
#define QT_MAINWINDOW_H

#include <QList>
#include <QMainWindow>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QGraphicsView>
#include <QGraphicsItem>
#include <QGraphicsScene>
#include <QDir>
#include <QPainter>
#include <QImage>
#include <QFontDatabase>
#include <QKeyEvent>
#include <QWheelEvent>
#include <QTableWidget>
#include <QActionGroup>
#include <QEventLoop>
#include <QLineEdit>
#include <QTextEdit>
#include <QMenuBar>
#include <QTimer>
#include <QGraphicsSceneMouseEvent>
#include "defines.h"
#include "structures.h"
#include "src/object_dialog.h"
#include "nppdialog.h"
#include "src/cmds.h"


#define MAX_RECENT_SAVEFILES    5

class QAction;
class QMenu;
class QGraphicsView;
class QGraphicsScene;
class QGraphicsItem;
class DungeonGrid;
class DunMapGrid;
class DunOverheadGrid;
class DungeonCursor;
class QTextEdit;
class QLineEdit;

// Maany other details of the mouseclick could be added if necessary
class mouse_click_info
{
public:
    bool left_click;
    bool right_click;
    bool middle_click;
    bool extra_button_1;
    bool extra_button_2;
    int mouse_click_y;
    int mouse_click_x;
};

class extra_win_settings
{
public:
    QPointer<QWidget> main_widget;
    QPointer<QVBoxLayout> main_vlay;
    QMenuBar *win_menubar;
    QPointer<QMenu> win_menu;
    QPointer<QAction> win_font_act;

    QRect win_geometry;
    bool win_maximized;
    bool win_show;
    QFont win_font;

    void set_extra_win_default();
    void get_widget_settings(QWidget *this_widget);
    void make_extra_window();
};

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    // The editable part of the main window.
    QPointer<QGraphicsView> graphics_view;
    QPointer<QGraphicsScene> dungeon_scene;

    int targeting_mode;
    UserInput input;
    QEventLoop ev_loop;

    int anim_depth;
    QEventLoop anim_loop;

    DungeonGrid *grids[MAX_DUNGEON_HGT][MAX_DUNGEON_WID];

    QFont font_main_window;
    QFont font_message_window;
    QFont font_sidebar_window;
    int main_font_hgt, main_font_wid;
    int main_tile_hgt, main_tile_wid;
    int main_cell_hgt, main_cell_wid;
    QString main_multiplier;
    bool do_25d_graphics;
    bool do_pseudo_ascii;
    bool do_wall_block;
    bool show_targeting_buttons;
    bool show_hotkey_toolbar;
    bool executing_command;
    bool win_maximized;
    QRect win_geometry;

    byte sound_volume;

    // Scaled tiles
    QHash<QString,QPixmap> tiles;

    // For light effects
    QHash<QString,QPixmap> shade_cache;

    QList<QGraphicsItem *> path_items;

    DungeonCursor *cursor;

    QPointer<QWidget> main_widget;
    QPointer<QHBoxLayout> main_widget_hlay;
    // The vlay goes inside the hlay
    QPointer<QVBoxLayout> main_sidebar_vlay;
    QPointer<QVBoxLayout> main_widget_vlay;


    QPointer<QHBoxLayout> message_area_hlay;
    QPointer<QLabel> message_area;
    QPointer<QLabel> message_label;

    QPointer<QToolBar> status_bar;
    QPointer<QToolBar> hotkey_toolbar;

    QPointer<QWidget> sidebar_widget;
    QPointer<QVBoxLayout> sidebar_vlay;
    QScrollArea *sidebar_scroll;

    QList<QLabel *> list_sidebar_labels;
    QList<QLabel *> list_sidebar_mon_direction;
    QList<QLabel *> list_sidebar_mon_pics;
    QList<QLabel *> list_sidebar_mon_name;
    QList<QLabel *> list_sidebar_mon_health;

    QList<QAction *> list_hotkey_toolbar_qactions;

    QPointer<QVBoxLayout> player_info_labels;
    QPointer<QVBoxLayout> player_info_data;
    QPointer<QVBoxLayout> player_info_vlay;
    QPointer<QVBoxLayout> mon_health_vlay;
    QGridLayout *targeting_glay;

    MainWindow();

    void update_messages(void);
    QPoint get_target(u32b flags);
    void init_scene();
    void set_font_main_window(QFont newFont);
    void set_font_message_window(QFont newFont);
    void set_font_sidebar_window(QFont newFont);
    void update_message_label(QString message);
    void clear_message_label(void);
    void calculate_cell_size();
    void destroy_tiles();
    void set_graphic_mode(int mode);
    void set_keymap_mode(int mode);
    void redraw_screen();
    void redraw_all();
    void update_cursor();
    void force_redraw();
    bool panel_contains(int y, int x);    
    bool running_command();
    QPixmap get_tile(QString tile_id, int tile_hgt, int tile_wid);
    QPixmap apply_shade(QString tile_id, QPixmap tile, QString shade_id);
    void wait_animation(int n_animations = 1);
    void animation_done();

    void create_sidebar();
    void update_sidebar_font();
    void update_sidebar_player();
    void update_sidebar_mon();
    void sidebar_display_mon(int index);
    QString return_sidebar_text(bool label, int row);
    void update_sidebar_all() {update_sidebar_mon(); update_sidebar_player();}
    void hide_sidebar();
    void show_sidebar();
    void close_game_death();
    void update_titlebar();
    void create_titlebar();
    void create_statusbar();
    void update_statusbar();
    void hide_statusbar();
    void show_statusbar();
    void create_hotkey_toolbar();
    void update_hotkey_toolbar();
    void hotkey_toolbar_hide();
    void hotkey_toolbar_show();
    void create_targeting_sidebar();
    void hide_targeting_sidebar();
    void show_targeting_sidebar();
    void save_png_screenshot(void);

    void handle_grid_wheelevent(bool wheelscroll_increase);


protected:
    void closeEvent(QCloseEvent *event);
    void keyPressEvent(QKeyEvent* which_key);
    bool eventFilter(QObject *obj, QEvent *event);
    void hideEvent(QHideEvent *event);
    void showEvent(QShowEvent *event);

private slots:

    // Functions called from signals from the top menu.

    void start_game_nppangband();
    void start_game_nppmoria();
    void open_current_savefile();
    void save_character();
    void save_character_as();
    void save_and_close();
    void open_recent_file();
    void about();
    void command_list_keyboard();
    void command_list_mouse();
    void command_list_targeting();
    void options_dialog();
    void hp_warning_dialog();
    void delay_anim_factor_dialog();
    void delay_run_factor_dialog();
    void sound_volume_dialog();
    void toggle_show_targeting();
    void toggle_show_hotkey_toolbar();
    void object_squelch_menu();
    void quality_squelch_menu();
    void ego_item_squelch_menu();
    void font_dialog_main_window();
    void font_dialog_message_window();
    void font_dialog_sidebar_window();

    void manage_hotkeys();
    void export_hotkeys();
    void import_hotkeys();

    void toggle_searching();
    void click_study();

    void do_create_package();
    void do_extract_from_package();

    void slot_simplified_keyset() {set_keymap_mode(KEYSET_NEW);}
    void slot_angband_keyset() {set_keymap_mode(KEYSET_ANGBAND);}
    void slot_rogue_keyset() {set_keymap_mode(KEYSET_ROGUE);}
    void slot_multiplier_clicked(QAction *);
    void hotkey_toolbar_clicked(QAction *);

    // Functions to make sure the available menu commands are appropriate to the situation.
    //  For example, make the save game command unanavailable when no savefile is open.
    void update_file_menu_game_active();
    void update_file_menu_game_inactive();

    // Graphics
    void set_reg();
    void set_dvg();
    void set_old_tiles();
    void set_ascii();
    void set_25d_graphics();
    void set_pseudo_ascii();
    void set_wall_block();
    void display_monster_info();
    void display_object_info();
    void display_ego_item_info();
    void display_artifact_info();
    void display_terrain_info();
    void display_notes();
    void display_messages();
    void display_home();
    void display_scores();
    void display_kill_count();


    void timed_events();
    void single_click_events();
    void set_input_key(int this_key);
    void target_choice();

    void all_extra_windows_open(void);
    void all_extra_windows_close(void);

private:

    void setup_nppangband();
    void setup_nppmoria();
    void launch_birth(bool quick_start);

    // Functions that initialize the file menu of the main window.
    void create_actions();
    void create_menus();
    void create_toolbars();
    void select_font();

    // Set up many of the game commands
    void create_signals();


    // Remember the game settings
    void read_settings();
    void write_settings();

    // Handle timed events
    QPointer<QTimer> event_timer;
public:
// To distinguish single clicks from double clicks
    QPointer<QTimer> single_click_timer;
    mouse_click_info single_mouseclick_info;


private:

    //Functions and variables that handle opening and saving files, as well as maintain the
    //  5 most recent savefile list.
    void load_file(const QString &file_name);
    void save_file(const QString &file_name);
    void set_current_savefile(const QString &file_name);
    void update_recent_savefiles();
    QString stripped_name(const QString &full_file_name);
    QStringList recent_savefiles;
    QPointer<QAction> recent_savefile_actions[MAX_RECENT_SAVEFILES];

    //  Holds the actual commands for the file menu and toolbar.
    QPointer<QMenu> file_menu;
    QPointer<QMenu> recent_files_menu;
    QPointer<QMenu> settings;
    QPointer<QMenu> knowledge;
    QPointer<QMenu> display;
    QPointer<QMenu> win_menu;
    QPointer<QMenu> help_menu;
    QPointer<QToolBar> file_toolbar;
    QPointer<QAction> new_game_nppangband;
    QPointer<QAction> new_game_nppmoria;
    QPointer<QAction> open_savefile;
    QPointer<QAction> save_cur_char;
    QPointer<QAction> save_cur_char_as;
    QPointer<QAction> close_cur_char;
    QPointer<QAction> exit_npp;

    //Command for the settings menu
    QPointer<QAction> options_act;
    QPointer<QAction> font_main_select_act;
    QPointer<QAction> font_messages_select_act;
    QPointer<QAction> font_sidebar_select_act;
    QPointer<QActionGroup> keymap_choice;
    QPointer<QAction> keymap_new;
    QPointer<QAction> keymap_angband;
    QPointer<QAction> keymap_rogue;
    QPointer<QAction> hotkey_manage;
    QPointer<QAction> hotkey_export;
    QPointer<QAction> hotkey_import;
    QPointer<QAction> show_targeting_act;
    QPointer<QAction> show_hotkey_toolbar_act;
    QPointer<QAction> object_squelch_act;
    QPointer<QAction> quality_squelch_act;
    QPointer<QAction> ego_item_squelch_act;
    QPointer<QAction> hitpoint_warning_act;
    QPointer<QAction> delay_anim_factor_act;
    QPointer<QAction> delay_run_factor_act;
    QPointer<QAction> sound_volume_act;


    //Commmands for the knowledge menu
    QPointer<QAction> view_monster_knowledge;
    QPointer<QAction> view_object_knowledge;
    QPointer<QAction> view_ego_item_knowledge;
    QPointer<QAction> view_artifact_knowledge;
    QPointer<QAction> view_terrain_knowledge;
    QPointer<QAction> view_notes;
    QPointer<QAction> view_messages;
    QPointer<QAction> view_home_inven;
    QPointer<QAction> view_scores;
    QPointer<QAction> view_kill_count;

    // Commands for the display menu
    QPointer<QActionGroup> tiles_choice;
    QPointer<QAction> ascii_mode_act;
    QPointer<QAction> reg_mode_act;
    QPointer<QAction> dvg_mode_act;
    QPointer<QAction> old_tiles_act;
    QPointer<QAction> graphics_25d_act;
    QPointer<QAction> pseudo_ascii_act;
    QPointer<QAction> wall_block_act;

    // Commands for the additional windows
    QPointer<QAction> win_mon_list_act;
    QPointer<QAction> win_obj_list_act;
    QPointer<QAction> win_mon_recall_act;
    QPointer<QAction> win_obj_recall_act;
    QPointer<QAction> win_feat_recall_act;
    QPointer<QAction> win_messages_act;
    QPointer<QAction> win_char_basic_act;
    QPointer<QAction> win_char_equip_info_act;
    QPointer<QAction> win_char_equipment_act;
    QPointer<QAction> win_char_inventory_act;
    QPointer<QAction> win_dun_map_act;
    QPointer<QAction> win_overhead_map_act;
    QPointer<QAction> win_open_all_act;
    QPointer<QAction> win_close_all_act;


    // Holds the actual commands for the help menu.
    QPointer<QAction> help_about;
    QPointer<QAction> help_about_Qt;
    QPointer<QAction> help_command_list;
    QPointer<QAction> help_mouse_list;
    QPointer<QAction> help_targeting_list;
    QPointer<QAction> separator_act;


    // information about the main window
    QFontDatabase font_database;

    QPointer<QActionGroup> multipliers;
    QPointer<QActionGroup> hotkey_toolbar_actions;

    // Actions for the statusbar
    // buttons for status bar
    QPointer<QAction> recall;
    QPointer<QAction> searching;
    QPointer<QAction> status_cut;
    QPointer<QAction> status_stun;
    QPointer<QAction> status_hunger;
    QPointer<QAction> study;

    QPointer<QAction> blind;
    QPointer<QAction> paralyzed;
    QPointer<QAction> confused;
    QPointer<QAction> afraid;
    QPointer<QAction> hallucination;
    QPointer<QAction> poisoned;
    QPointer<QAction> protect_evil;
    QPointer<QAction> invulnerability;
    QPointer<QAction> hero;
    QPointer<QAction> berzerk;
    QPointer<QAction> shield;
    QPointer<QAction> blessed;
    QPointer<QAction> see_invisible;
    QPointer<QAction> infravision;

    QPointer<QAction> resist_acid;
    QPointer<QAction> resist_cold;
    QPointer<QAction> resist_fire;
    QPointer<QAction> resist_lightning;
    QPointer<QAction> resist_poison;

    QPointer<QAction> flying;

    QPointer<QAction> native_lava;

    QPointer<QAction> native_oil;
    QPointer<QAction> native_sand;
    QPointer<QAction> native_tree;
    QPointer<QAction> native_water;
    QPointer<QAction> native_mud;

    QPointer<QAction> status_speed;

    QPointer<QAction> elemental_weapon;
    QPointer<QAction> call_hourns;

    QPointer<QAction> nativity;
    QPointer<QAction> status_trap_detect;



// Monster list window
private:
    QTableWidget *mon_list_area;
    void win_mon_list_create();
    void win_mon_list_close();
    void win_mon_list_wipe();
    void set_font_win_mon_list(QFont newFont);
    QPointer<QButtonGroup> mon_button_group;

public:
    void win_mon_list_update();
    extra_win_settings win_mon_list_settings;

private slots:
    void win_mon_list_font();
    void toggle_win_mon_list();
    void win_mon_list_destroy(QObject *this_object);
    void mon_info_press(int mon_race);

// Object list window
private:
    QTableWidget *obj_list_area;
    void win_obj_list_create();
    void win_obj_list_close();
    void win_obj_list_wipe();
    void set_font_win_obj_list(QFont newFont);
    QPointer<QButtonGroup> obj_button_group;

public:
    void win_obj_list_update();
    extra_win_settings win_obj_list_settings;

private slots:
    void win_obj_list_font();
    void toggle_win_obj_list();
    void win_obj_list_destroy(QObject *this_object);
    void obj_info_press(int k_idx);

// Monster Recall window
private:
    QTextEdit *mon_recall_area;
    void win_mon_recall_create();
    void win_mon_recall_close();
    void win_mon_recall_wipe();
    void set_font_win_mon_recall(QFont newFont);

public:
    void win_mon_recall_update();
    extra_win_settings win_mon_recall_settings;

private slots:
    void win_mon_recall_font();
    void toggle_win_mon_recall();
    void win_mon_recall_destroy(QObject *this_object);


// Object Recall window
private:
    QTextEdit *obj_recall_area;
    void win_obj_recall_create();
    void win_obj_recall_close();
    void win_obj_recall_wipe();
    void set_font_win_obj_recall(QFont newFont);

public:
    void win_obj_recall_update();
    extra_win_settings win_obj_recall_settings;

private slots:
    void win_obj_recall_font();
    void toggle_win_obj_recall();
    void win_obj_recall_destroy(QObject *this_object);

// Feature Recall window
private:
    QTextEdit *feat_recall_area;
    void win_feat_recall_create();
    void win_feat_recall_close();
    void win_feat_recall_wipe();
    void set_font_win_feat_recall(QFont newFont);

public:
    void win_feat_recall_update();
    extra_win_settings win_feat_recall_settings;

private slots:
    void win_feat_recall_font();
    void toggle_win_feat_recall();
    void win_feat_recall_destroy(QObject *this_object);

// Messages window
private:
    QTextEdit *win_messages_area;
    QPointer<QMenu> win_messages_win_settings;
    void win_messages_create();
    void win_messages_close();
    void win_messages_wipe();
    void set_font_win_messages(QFont newFont);


public:
    void win_messages_update();
    extra_win_settings win_message_settings;

private slots:
    void win_messages_font();
    void toggle_win_messages();
    void win_messages_destroy(QObject *this_object);


// Character Information window
private:
    void win_char_info_basic_create();
    void win_char_info_basic_close();
    void win_char_info_basic_wipe();
    void update_label_basic_font();
    void set_font_char_info_basic(QFont newFont);
    void name_change_pushbutton(QGridLayout *return_layout);
    void create_win_char_info();

public:
    void win_char_info_basic_update();
    void win_char_info_score();
    void win_char_info_turncount();
    extra_win_settings char_info_basic_settings;

private slots:
    void win_char_info_basic_font();
    void toggle_win_char_basic_frame();
    void name_change(void);
    void win_char_info_basic_destroy(QObject *this_object);

// Character Equipment Information window
private:
    QPointer<QAction> char_info_equip_font_act;
    void win_char_info_equip_create();
    void win_char_info_equip_close();
    void win_char_info_equip_wipe();
    void update_label_equip_info_font();
    void set_font_char_info_equip(QFont newFont);
    void create_win_char_equip_info();
    QPointer<QWidget> resist_widget;
    QPointer<QWidget> ability_widget;
    QPointer<QWidget> equip_widget;
    QPointer<QWidget> nativity_widget;
    QGridLayout *resist_flags;
    QGridLayout *ability_flags;
    QGridLayout *equip_mods;
    QGridLayout *nativity_flags;
    void update_win_char_equip_set_lists();


public:
    void win_char_info_equip_update();
    QList<QLabel *> list_resist_flags;
    QList<QLabel *> list_ability_flags;
    QList<QLabel *> list_equip_flags;
    QList<QLabel *> list_nativity_flags;
    QList<QLabel *> list_resist_labels;
    QList<QLabel *> list_ability_labels;
    QList<QLabel *> list_equip_labels;
    QList<QLabel *> list_nativity_labels;
    QList<QLabel *> list_resist_equippy;
    QList<QLabel *> list_ability_equippy;
    QList<QLabel *> list_equip_equippy;
    QList<QLabel *> list_nativity_equippy;
    extra_win_settings char_info_equip_settings;

private slots:
    void win_char_info_equip_font();
    void toggle_win_char_equip_frame();
    void win_char_info_equip_destroy(QObject *this_object);

// Character Equipment window
private:
    QPointer<QAction> char_equipment_buttons_act;
    void win_char_equipment_create();
    void win_char_equipment_close();
    void win_char_equipment_wipe();
    void update_label_equipment_font();
    void set_font_char_equipment(QFont newFont);
    void create_win_char_equipment();
    void equip_link_pushbuttons();
    bool equip_show_buttons;

    QGridLayout *equip_list;
    QGridLayout *quiver_list;

public:
    void win_char_equipment_update();
    extra_win_settings char_equipment_settings;

private slots:
    void win_char_equipment_font();
    void toggle_equip_show_buttons();
    void toggle_win_char_equipment_frame();
    void equip_button_click();
    void win_char_equipment_destroy(QObject *this_object);

// Character Inventory window
private:
    QPointer<QAction> char_inventory_buttons_act;
    void win_char_inventory_create();
    void win_char_inventory_close();
    void win_char_inventory_wipe();
    void update_label_inventory_font();
    void set_font_char_inventory(QFont newFont);
    void create_win_char_inventory();
    void inven_link_pushbuttons();
    bool inven_show_buttons;

    QGridLayout *inven_list;

public:
    void win_char_inventory_update();
    extra_win_settings char_inventory_settings;

private slots:
    void win_char_inventory_font();
    void toggle_inven_show_buttons();
    void toggle_win_char_inventory_frame();
    void inven_button_click();
    void win_char_inventory_destroy(QObject *this_object);

    // Small map window
private:
    QPointer<QGraphicsScene> dun_map_scene;
    QPointer<QGraphicsView> dun_map_view;
    QPointer<QAction> dun_map_graphics_act;

    void win_dun_map_create();
    void win_dun_map_close();
    void win_dun_map_wipe();
    void create_win_dun_map();
    DunMapGrid *dun_map_grids[MAX_DUNGEON_HGT][MAX_DUNGEON_WID];
    void dun_map_calc_cell_size();
    QPointer<QActionGroup> dun_map_multipliers;
    QString dun_map_multiplier;
    void set_dun_map_font(QFont newFont);

public:
    bool dun_map_use_graphics;
    void win_dun_map_update();
    void dun_map_update_one_grid(int y, int x);
    void dun_map_center(int y, int x);
    QRect visible_dun_map();
    bool dun_map_created;
    int dun_map_font_hgt, dun_map_font_wid;
    int dun_map_tile_hgt, dun_map_tile_wid;
    int dun_map_cell_wid, dun_map_cell_hgt;
    extra_win_settings dun_map_settings;

private slots:
    void win_dun_map_font();
    void toggle_win_dun_map_frame();
    void dun_map_multiplier_clicked(QAction *);
    void set_dun_map_graphics();
    void win_dun_map_destroy(QObject *this_object);

    // Overhead window
private:
    QPointer<QGraphicsScene> overhead_map_scene;
    QPointer<QGraphicsView> overhead_map_view;
    QPointer<QAction> overhead_map_graphics_act;
    void win_overhead_map_create();
    void win_overhead_map_wipe();
    void win_overhead_map_close();
    void create_win_overhead_map();
    DunOverheadGrid *overhead_map_grids[MAX_DUNGEON_HGT/2][MAX_DUNGEON_WID/2];
    void overhead_map_calc_cell_size();
    QPointer<QActionGroup> overhead_map_multipliers;
    QString overhead_map_multiplier;
    void set_overhead_map_font(QFont newFont);


public:
    bool overhead_map_use_graphics;
    void win_overhead_map_update();
    void overhead_map_update_one_grid(int y, int x);
    void overhead_map_center(int y, int x);
    QRect visible_overhead_map();
    bool overhead_map_created;
    int overhead_map_font_hgt, overhead_map_font_wid;
    int overhead_map_tile_hgt, overhead_map_tile_wid;
    int overhead_map_cell_wid, overhead_map_cell_hgt;
    extra_win_settings overhead_map_settings;

private slots:
    void win_overhead_map_font();
    void toggle_win_overhead_map_frame();
    void overhead_map_multiplier_clicked(QAction *);
    void set_overhead_map_graphics();
    void win_overhead_map_destroy(QObject *this_object);

};

extern MainWindow *main_window;

class DunMapGrid: public QGraphicsItem
{
public:
    DunMapGrid(int _x, int _y);

    QRectF boundingRect() const;
    void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
    QPainterPath shape() const;

    void DunMapCellSizeChanged();

    int dm_x, dm_y;
};

class DunOverheadGrid: public QGraphicsItem
{

public:
    DunOverheadGrid(int _x, int _y);

    QRectF boundingRect() const;
    void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
    QPainterPath shape() const;

    void DunMapCellSizeChanged();

    int oh_x, oh_y;
};

class DungeonGrid: public QGraphicsItem
{
public:
    DungeonGrid(int _x, int _y, MainWindow *_parent);

    QRectF boundingRect() const;
    void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
    void wheelEvent(QGraphicsSceneWheelEvent *event);
    void mousePressEvent(QGraphicsSceneMouseEvent *event);
    void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event);
    void mouseSingleClickEvent();
    QPainterPath shape() const;

    void cellSizeChanged();

    MainWindow *parent;
    int c_x, c_y;

    QPixmap shade_tile(QPixmap this_tile, u16b flags, QString tile_name);

    void handle_single_click(mouse_click_info mouse_event);

};

class DungeonCursor: public QGraphicsItem
{
public:
    MainWindow *parent;
    int c_x, c_y;

    QRectF boundingRect() const;
    void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
    void wheelEvent(QGraphicsSceneWheelEvent *event);
    void mousePressEvent(QGraphicsSceneMouseEvent *event);
    void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event);
    QPainterPath shape() const;

    DungeonCursor(MainWindow *_parent);
    void moveTo(int y, int x);

    void cellSizeChanged();
};

#define TILE_1x1_MULT 3
#define FONT_EXTRA 4

extern QString find_cloud_tile(int y, int x);

extern QVector<s16b> sidebar_monsters;
extern QString mult_list[];

#endif
