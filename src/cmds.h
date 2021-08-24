#ifndef CMDS_H
#define CMDS_H



#include <src/object_settings.h>
#include "src/player.h"
#include <QKeyEvent>
#include <QTabWidget>
#include <QPointer>

enum
{
    COL_SPELL_TITLE = 0,
    COL_LEVEL,
    COL_MANA,
    COL_FAIL_PCT,
    COL_INFO,
    COL_HELP
};

// Add the headers


class QTabWidget;



class SpellSelectDialog : public QDialog
{
    Q_OBJECT

public:
    explicit SpellSelectDialog(int *spell, int start_sval, QString prompt, int mode, bool *cannot, bool *cancelled);

protected:
    void keyPressEvent(QKeyEvent* which_key);

private slots:
    // Receives the number of the button pressed.
    void button_press(int num);
    void help_press(int num);
    void move_left(void);
    void move_right(void);

private:

    QPointer<QTabWidget> spell_dialog;

    QPointer<QButtonGroup> spell_select_group;
    QPointer<QButtonGroup> spell_help_group;


    // Functions to build the actual tabs
    void build_spellbook_dialog(int mode);

    QString format_button_name(QChar char_index, object_type *o_ptr, byte which_tab, int slot);

    //Functions to track the list of possible items
    void available_spells(int mode);

    QString get_spell_comment(int spell);

    // Variables for keeping track of which item is selected
    int selected_button;

    bool available_books[BOOKS_PER_REALM_ANGBAND];

    bool usable_spells;
    int max_spellbooks;
    int selected_book;
    int activate_tab;

    // Are we a priest studying a book, or do we get to choose ?
    bool choosing_book;

};

class ObjectDestroyDialog : public ObjectSettingsAux
{
    Q_OBJECT

public:
    explicit ObjectDestroyDialog(s16b o_idx);


};

class RestDialog: public QDialog
{
    Q_OBJECT
public:
    int choice;

    RestDialog(int *_choice);

protected:
    void keyPressEvent(QKeyEvent *event);

public slots:
    void on_clicked();
};



// cmd_actions.cpp
extern bool do_cmd_test(int y, int x, int action, bool do_message);
extern void do_cmd_go_up(void);
extern void do_cmd_go_down(void);
extern void command_open(cmd_arg args);
extern void do_cmd_open(int dir);
extern void command_disarm(cmd_arg args);
extern void do_cmd_disarm(int dir);
extern void do_search(void);
extern void do_cmd_toggle_search(void);
extern void command_search(cmd_arg args);
extern void do_cmd_search(void);
extern void command_tunnel(cmd_arg args);
extern void do_cmd_tunnel(int dir);
extern void command_close(cmd_arg args);
extern void do_cmd_close(int dir);
extern void command_alter(cmd_arg args);
extern void do_cmd_alter(int dir);
extern void command_spike(cmd_arg args);
extern void do_cmd_spike(int dir);
extern void command_rest(cmd_arg args);
extern void do_cmd_rest(void);
extern void do_cmd_rest_specific(int choice);
extern void do_cmd_rest_hp(void);
extern void command_run(cmd_arg args);
extern void do_cmd_run(int dir);
extern void command_walk(cmd_arg args);
extern void do_cmd_walk(int dir, bool jumping);
extern void command_bash(cmd_arg args);
extern void do_cmd_bash(int dir);
extern void command_hold(cmd_arg args);
extern void do_cmd_hold();
extern void do_cmd_findpath(int y, int x);


// cmd_traps.cpp
extern bool make_monster_trap(void);
extern void py_set_trap(int y, int x);
extern void command_make_trap(cmd_arg args);
extern void do_cmd_make_trap(int dir);


//cmd_objects
extern cmd_arg obj_wield(object_type *o_ptr, cmd_arg args);
extern cmd_arg obj_uninscribe(object_type *o_ptr, cmd_arg args);
extern bool trap_related_object(object_type *o_ptr);
extern void command_uninscribe(cmd_arg args);
extern void do_cmd_uninscribe(void);
extern void command_inscribe(cmd_arg args);
extern void do_cmd_inscribe(void);
extern cmd_arg obj_examine(object_type *o_ptr, cmd_arg args);
extern void command_examine(cmd_arg args);
extern void do_cmd_examine(void);
extern void command_takeoff(cmd_arg args);
extern void do_cmd_takeoff(void);
extern void command_wield(cmd_arg args);
extern void do_cmd_wield(void);
extern void command_drop(cmd_arg args);
extern void do_cmd_drop(void);
extern void command_refuel(cmd_arg args);
extern void do_cmd_refuel(void);
extern void command_swap(cmd_arg args);
extern void do_cmd_swap_weapon(void);
extern void command_destroy(cmd_arg args);
extern void do_cmd_destroy(void);
extern void do_cmd_activate(void);
extern void do_cmd_use_item(void);
extern void do_cmd_aim_wand(void);
extern void do_cmd_use_staff(void);
extern void do_cmd_zap_rod(void);
extern void do_cmd_eat_food(void);
extern void do_cmd_quaff_potion(void);
extern void do_cmd_read_scroll(void);

// cmd_misc.cpp
extern void do_cmd_feeling(void);
extern void do_cmd_repeat(void);
extern void do_cmd_look(void);
extern void do_cmd_write_note(void);
extern void do_cmd_suicide(void);

// cmd_pickup.cpp
extern bool put_object_in_inventory(object_type *o_ptr);
extern void do_cmd_pickup_from_pile(bool pickup, bool message);
extern void py_pickup_gold(void);
extern void command_pickup(cmd_arg args);
extern void py_pickup(bool pickup);
extern void do_cmd_pickup(void);
extern int move_player(int dir, int jumping);

// cmd_spell.cpp
extern int spell_chance(int spell);
extern bool spell_okay(int spell, bool known);
extern void command_browse(cmd_arg arg);
extern void do_cmd_browse(int book_choice);
extern bool player_can_use_book(const object_type *o_ptr, bool known);
extern void command_study(cmd_arg args);
extern void do_cmd_study(int book_choice);
extern void cast_spell(cmd_arg args);
extern void command_cast(cmd_arg args);
extern void do_cmd_cast(int book_choice);
extern bool is_trap_spell(byte spell_book, int spell);
extern s16b get_spell_from_list(s16b book, s16b spell);
extern bool obj_can_cast(object_type *o_ptr);

// pathfind.cpp
extern bool buildpath(int y, int x);
extern int run_step(int dir);

#endif // CMDS_H
