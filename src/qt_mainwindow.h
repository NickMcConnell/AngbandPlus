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

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    // The editable part of the main window.
    QGraphicsView *graphics_view;
    QGraphicsScene *dungeon_scene;

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
    bool executing_command;

    // Scaled tiles
    QHash<QString,QPixmap> tiles;

    // For light effects
    QHash<QString,QPixmap> shade_cache;

    QList<QGraphicsItem *> path_items;

    DungeonCursor *cursor;

    QWidget *main_widget;
    QHBoxLayout *main_widget_hlay;
    // The vlay goes inside the hlay
    QVBoxLayout *main_sidebar_vlay;
    QVBoxLayout *main_widget_vlay;


    QHBoxLayout *message_area_hlay;
    QLabel *message_area;
    QLabel *message_label;

    QToolBar *status_bar;

    QWidget *sidebar_widget;
    QVBoxLayout *sidebar_vlay;
    QScrollArea *sidebar_scroll;

    QVBoxLayout *player_info_labels;
    QVBoxLayout *player_info_data;
    QVBoxLayout *player_info_vlay;
    QVBoxLayout *mon_health_vlay;
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
    void sidebar_display_mon(int m_idx);
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
    void create_targeting_sidebar();
    void hide_targeting_sidebar();
    void show_targeting_sidebar();
    void save_png_screenshot(void);


protected:
    void closeEvent(QCloseEvent *event);
    void wheelEvent(QWheelEvent* event);
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
    void toggle_show_targeting();
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
    QTimer *event_timer;
public:
// To distinguish single clicks from double clicks
    QTimer *single_click_timer;
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
    QAction *recent_savefile_actions[MAX_RECENT_SAVEFILES];

    //  Holds the actual commands for the file menu and toolbar.
    QMenu *file_menu;
    QMenu *recent_files_menu;
    QMenu *settings;
    QMenu *knowledge;
    QMenu *display;
    QMenu *win_menu;
    QMenu *help_menu;
    QToolBar *file_toolbar;
    QAction *new_game_nppangband;
    QAction *new_game_nppmoria;
    QAction *open_savefile;
    QAction *save_cur_char;
    QAction *save_cur_char_as;
    QAction *close_cur_char;
    QAction *exit_npp;

    //Command for the settings menu
    QAction *options_act;
    QAction *font_main_select_act;
    QAction *font_messages_select_act;
    QAction *font_sidebar_select_act;
    QActionGroup *keymap_choice;
    QAction *keymap_new;
    QAction *keymap_angband;
    QAction *keymap_rogue;
    QAction *hotkey_manage;
    QAction *hotkey_export;
    QAction *hotkey_import;
    QAction *show_targeting_act;


    //Commmands for the knowledge menu
    QAction *view_monster_knowledge;
    QAction *view_object_knowledge;
    QAction *view_ego_item_knowledge;
    QAction *view_artifact_knowledge;
    QAction *view_terrain_knowledge;
    QAction *view_notes;
    QAction *view_messages;
    QAction *view_home_inven;
    QAction *view_scores;
    QAction *view_kill_count;

    // Commands for the display menu
    QActionGroup *tiles_choice;
    QAction *ascii_mode_act;
    QAction *reg_mode_act;
    QAction *dvg_mode_act;
    QAction *old_tiles_act;
    QAction *graphics_25d_act;
    QAction *pseudo_ascii_act;
    QAction *wall_block_act;

    // Commands for the additional windows
    QAction *win_mon_list;
    QAction *win_obj_list;
    QAction *win_mon_recall;
    QAction *win_obj_recall;
    QAction *win_feat_recall;
    QAction *win_messages;
    QAction *win_char_basic;
    QAction *win_char_equip_info;
    QAction *win_char_equipment;
    QAction *win_char_inventory;
    QAction *win_dun_map;
    QAction *win_overhead_map;

    // Holds the actual commands for the help menu.
    QAction *help_about;
    QAction *help_about_Qt;
    QAction *help_command_list;
    QAction *help_mouse_list;
    QAction *help_targeting_list;
    QAction *separator_act;


    // information about the main window
    QFontDatabase font_database;

    QActionGroup *multipliers;

    // Actions for the statusbar
    // buttons for status bar
    QAction *recall;
    QAction *searching;
    QAction *status_cut;
    QAction *status_stun;
    QAction *status_hunger;
    QAction *study;

    QAction *blind;
    QAction *paralyzed;
    QAction *confused;
    QAction *afraid;
    QAction *hallucination;
    QAction *poisoned;
    QAction *protect_evil;
    QAction *invulnerability;
    QAction *hero;
    QAction *berzerk;
    QAction *shield;
    QAction *blessed;
    QAction *see_invisible;
    QAction *infravision;

    QAction *resist_acid;
    QAction *resist_cold;
    QAction *resist_fire;
    QAction *resist_lightning;
    QAction *resist_poison;

    QAction *flying;

    QAction *native_lava;

    QAction *native_oil;
    QAction *native_sand;
    QAction *native_tree;
    QAction *native_water;
    QAction *native_mud;

    QAction *status_speed;

    QAction *elemental_weapon;
    QAction *call_hourns;

    QAction *nativity;
    QAction *status_trap_detect;


// Monster list window
private:
    bool show_mon_list;
    QWidget *window_mon_list;
    QVBoxLayout *mon_list_vlay;
    QTableWidget *mon_list_area;
    QMenuBar *mon_list_menubar;
    QAction *mon_list_set_font;
    QFont font_win_mon_list;
    QMenu *mon_win_settings;
    void win_mon_list_create();
    void win_mon_list_destroy();
    void win_mon_list_wipe();
    void set_font_win_mon_list(QFont newFont);

public:
    void win_mon_list_update();

private slots:
    void win_mon_list_font();
    void toggle_win_mon_list();
    void close_win_mon_list(QObject *this_object);

// Object list window
private:
    bool show_obj_list;
    QWidget *window_obj_list;
    QVBoxLayout *obj_list_vlay;
    QTableWidget *obj_list_area;
    QMenuBar *obj_list_menubar;
    QAction *obj_list_set_font;
    QFont font_win_obj_list;
    QMenu *obj_win_settings;
    void win_obj_list_create();
    void win_obj_list_destroy();
    void win_obj_list_wipe();
    void set_font_win_obj_list(QFont newFont);

public:
    void win_obj_list_update();

private slots:
    void win_obj_list_font();
    void toggle_win_obj_list();
    void close_win_obj_list(QObject *this_object);

// Monster Recall window
private:
    bool show_mon_recall;
    QWidget *window_mon_recall;
    QVBoxLayout *mon_recall_vlay;
    QTextEdit *mon_recall_area;
    QMenuBar *mon_recall_menubar;
    QAction *mon_recall_set_font;
    QFont font_win_mon_recall;
    QMenu *mon_recall_win_settings;
    void win_mon_recall_create();
    void win_mon_recall_destroy();
    void win_mon_recall_wipe();
    void set_font_win_mon_recall(QFont newFont);

public:
    void win_mon_recall_update();

private slots:
    void win_mon_recall_font();
    void toggle_win_mon_recall();
    void close_win_mon_recall(QObject *this_object);


// Object Recall window
private:
    bool show_obj_recall;
    QWidget *window_obj_recall;
    QVBoxLayout *obj_recall_vlay;
    QTextEdit *obj_recall_area;
    QMenuBar *obj_recall_menubar;
    QAction *obj_recall_set_font;
    QFont font_win_obj_recall;
    QMenu *obj_recall_win_settings;
    void win_obj_recall_create();
    void win_obj_recall_destroy();
    void win_obj_recall_wipe();
    void set_font_win_obj_recall(QFont newFont);

public:
    void win_obj_recall_update();

private slots:
    void win_obj_recall_font();
    void toggle_win_obj_recall();
    void close_win_obj_recall(QObject *this_object);

// Feature Recall window
private:
    bool show_feat_recall;
    QWidget *window_feat_recall;
    QVBoxLayout *feat_recall_vlay;
    QTextEdit *feat_recall_area;
    QMenuBar *feat_recall_menubar;
    QAction *feat_recall_set_font;
    QFont font_win_feat_recall;
    QMenu *feat_recall_win_settings;
    void win_feat_recall_create();
    void win_feat_recall_destroy();
    void win_feat_recall_wipe();
    void set_font_win_feat_recall(QFont newFont);

public:
    void win_feat_recall_update();

private slots:
    void win_feat_recall_font();
    void toggle_win_feat_recall();
    void close_win_feat_recall(QObject *this_object);

// Messages window
private:
    bool show_messages_win;
    QWidget *window_messages;
    QVBoxLayout *win_messages_vlay;
    QTextEdit *win_messages_area;
    QMenuBar *win_messages_menubar;
    QAction *win_messages_set_font;
    QFont font_win_messages;
    QMenu *messages_win_settings;
    void win_messages_create();
    void win_messages_destroy();
    void win_messages_wipe();
    void set_font_win_messages(QFont newFont);

public:
    void win_messages_update();

private slots:
    void win_messages_font();
    void toggle_win_messages();
    void close_win_messages(QObject *this_object);



// Character Information window
private:
    bool show_char_info_basic;
    QWidget *window_char_info_basic;
    QVBoxLayout *main_vlay_char_basic;
    QMenuBar *char_info_basic_menubar;
    QAction *char_info_basic_font;
    QFont font_char_basic_info;
    QMenu *char_info_basic_settings;
    void win_char_info_basic_create();
    void win_char_info_basic_destroy();
    void win_char_info_basic_wipe();
    void update_label_basic_font();
    void set_font_char_info_basic(QFont newFont);
    void name_change_pushbutton(QGridLayout *return_layout);
    void create_win_char_info();

public:
    void win_char_info_basic_update();
    void win_char_info_score();
    void win_char_info_turncount();

private slots:
    void win_char_info_basic_font();
    void toggle_win_char_info_frame();
    void name_change(void);
    void close_win_char_info_frame(QObject *this_object);

// Character Equipment Information window
private:
    bool show_char_info_equip;
    QWidget *window_char_info_equip;
    QVBoxLayout *main_vlay_char_equip_info;
    QMenuBar *char_info_equip_menubar;
    QAction *char_info_equip_font;
    QFont font_char_equip_info;
    QMenu *char_info_equip_settings;
    void win_char_info_equip_create();
    void win_char_info_equip_destroy();
    void win_char_info_equip_wipe();
    void update_label_equip_info_font();
    void set_font_char_info_equip(QFont newFont);
    void create_win_char_equip_info();
    QWidget *resist_widget;
    QWidget *ability_widget;
    QWidget *equip_widget;
    QWidget *nativity_widget;
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

private slots:
    void win_char_info_equip_font();
    void toggle_win_char_equip_frame();
    void close_win_char_equip_frame(QObject *this_object);

// Character Equipment window
private:
    bool show_char_equipment;
    QWidget *window_char_equipment;
    QVBoxLayout *main_vlay_equipment;
    QMenuBar *char_equipment_menubar;
    QAction *char_equipment_font;
    QAction *char_equipment_buttons;
    QFont font_char_equipment;
    QMenu *char_equipment_settings;
    void win_char_equipment_create();
    void win_char_equipment_destroy();
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

private slots:
    void win_char_equipment_font();
    void toggle_equip_show_buttons();
    void toggle_win_char_equipment_frame();
    void equip_button_click();
    void close_win_char_equipment_frame(QObject *this_object);

// Character Inventory window
private:
    bool show_char_inventory;
    QWidget *window_char_inventory;
    QVBoxLayout *main_vlay_inventory;
    QMenuBar *char_inventory_menubar;
    QAction *char_inventory_font;
    QAction *char_inventory_buttons;
    QFont font_char_inventory;
    QMenu *char_inventory_settings;
    void win_char_inventory_create();
    void win_char_inventory_destroy();
    void win_char_inventory_wipe();
    void update_label_inventory_font();
    void set_font_char_inventory(QFont newFont);
    void create_win_char_inventory();
    void inven_link_pushbuttons();
    bool inven_show_buttons;

    QGridLayout *inven_list;

public:
    void win_char_inventory_update();

private slots:
    void win_char_inventory_font();
    void toggle_inven_show_buttons();
    void toggle_win_char_inventory_frame();
    void inven_button_click();
    void close_win_char_inventory_frame(QObject *this_object);

    // Small map window
private:
    QWidget *window_dun_map;
    QVBoxLayout *main_vlay_dun_map;
    QGraphicsScene *dun_map_scene;
    QGraphicsView *dun_map_view;
    QMenuBar *win_dun_map_menubar;
    QAction *dun_map_font;
    QAction *dun_map_graphics;
    QMenu *win_dun_map_settings;
    void win_dun_map_create();
    void win_dun_map_destroy();
    void win_dun_map_wipe();
    void create_win_dun_map();
    DunMapGrid *dun_map_grids[MAX_DUNGEON_HGT][MAX_DUNGEON_WID];
    void dun_map_calc_cell_size();
    QActionGroup *dun_map_multipliers;
    QString dun_map_multiplier;
    void set_dun_map_font(QFont newFont);


public:
    bool show_win_dun_map;
    bool dun_map_use_graphics;
    void win_dun_map_update();
    void dun_map_update_one_grid(int y, int x);
    void dun_map_center(int y, int x);
    QRect visible_dun_map();
    bool dun_map_created;
    int dun_map_font_hgt, dun_map_font_wid;
    int dun_map_tile_hgt, dun_map_tile_wid;
    int dun_map_cell_wid, dun_map_cell_hgt;
    QFont font_dun_map;

private slots:
    void win_dun_map_font();
    void toggle_win_dun_map_frame();
    void dun_map_multiplier_clicked(QAction *);
    void set_dun_map_graphics();
    void close_win_dun_map_frame(QObject *this_object);

    // Overhead window
private:
    QWidget *window_overhead_map;
    QVBoxLayout *main_vlay_overhead_map;
    QGraphicsScene *overhead_map_scene;
    QGraphicsView *overhead_map_view;
    QMenuBar *win_overhead_map_menubar;
    QAction *overhead_map_font;
    QAction *overhead_map_graphics;
    QMenu *win_overhead_map_settings;
    void win_overhead_map_create();
    void win_overhead_map_destroy();
    void win_overhead_map_wipe();
    void create_win_overhead_map();
    DunOverheadGrid *overhead_map_grids[MAX_DUNGEON_HGT][MAX_DUNGEON_WID];
    void overhead_map_calc_cell_size();
    QActionGroup *overhead_map_multipliers;
    QString overhead_map_multiplier;
    void set_overhead_map_font(QFont newFont);

public:
    bool show_win_overhead_map;
    bool overhead_map_use_graphics;
    void win_overhead_map_update();
    void overhead_map_update_one_grid(int y, int x);
    void overhead_map_center(int y, int x);
    QRect visible_overhead_map();
    bool overhead_map_created;
    int overhead_map_font_hgt, overhead_map_font_wid;
    int overhead_map_tile_hgt, overhead_map_tile_wid;
    int overhead_map_cell_wid, overhead_map_cell_hgt;
    QFont font_overhead_map;

private slots:
    void win_overhead_map_font();
    void toggle_win_overhead_map_frame();
    void overhead_map_multiplier_clicked(QAction *);
    void set_overhead_map_graphics();
    void close_win_overhead_map_frame(QObject *this_object);

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
