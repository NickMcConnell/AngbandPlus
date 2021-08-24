#ifndef HOTKEYS_H
#define HOTKEYS_H

#include <src/npp.h>
#include <QLineEdit>
#include <QVector>
#include <QObject>
#include <QScrollArea>
#include <QComboBox>
#include <QVBoxLayout>
#include <QLineEdit>
#include <QButtonGroup>
#include <QPointer>


#define NUM_HOTKEYS 24

enum
{
    HK_TYPE_EMPTY = 0,
    HK_QUAFF_POTION,
    HK_READ_SCROLL,
    HK_AIM_WAND,
    HK_USE_STAFF,
    HK_ZAP_ROD,
    HK_EAT_FOOD,
    HK_CAST_SPELL,
    HK_ACTIVATE,
    HK_FIRE_AMMO,
    HK_THROW,
    HK_REST,
    HK_TYPE_MOVE,
    HK_TYPE_JUMP,
    HK_TYPE_RUN,
    HK_TYPE_ALTER,
    HK_TYPE_DISARM,
    HK_TYPE_CLOSE,
    HK_TYPE_OPEN,
    HK_TYPE_BASH,
    HK_TYPE_TUNNEL,
    HK_TYPE_MAKE_TRAP,
    HK_TYPE_SPIKE,
    HK_TYPE_HOLD,

    HK_TYPE_END,
};

typedef struct hotkey_list hotkey_list;
struct hotkey_list
{
    QString hotkey_list_name;
    bool shift;
    int listed_hotkey;

};

enum
{
    HK_NEEDS_NOTHING = 0,
    HK_NEEDS_DIRECTION,
    HK_NEEDS_OBJECT_KIND,
    HK_NEEDS_TARGET,
    HK_NEEDS_SPELL,
    HK_NEEDS_ACIVATION,
    HK_NEEDS_SPECIFIC_OBJECT,
    HK_NEEDS_REST,
};

enum
{
    OS_SELECT_DURING_USE = 0,
    OS_FIND_SPECIFIC_INSCRIPTION,
};

#define HK_VERIFY_YES   true
#define HK_VERIFY_NO   false

typedef struct hotkey_type hotkey_type;
struct hotkey_type
{
    byte hotkey_needs;
    QString name;
    int tval;
};

typedef struct hotkey_step hotkey_step;
struct hotkey_step
{
    byte step_commmand;
    cmd_arg step_args;

    // This is used to store the selected object while the dialog box is open.
    object_type step_object;
};


class single_hotkey
{
public:
    QString hotkey_name;
    QString hotkey_button_name;
    int hotkey_button;
    QVector<hotkey_step> hotkey_steps;
    void copy_hotkey(single_hotkey *other_hotkey);
    void clear_hotkey_steps(void);
    bool has_commands(void);
};

class HotKeyDialog : public QDialog
{
    Q_OBJECT

public:
    explicit HotKeyDialog(void);
    single_hotkey dialog_hotkey;

private:
    QPointer<QWidget> top_widget;
    QPointer<QVBoxLayout> top_layout;
    QPointer<QVBoxLayout> main_layout;
    QPointer<QVBoxLayout> vlay_hotkey_steps;
    QPointer<QScrollArea> scroll_box;
    QPointer<QLineEdit> hotkey_name;

    QVector<int> spell_list;

    void add_hotkeys_header();
    QPointer<QComboBox> current_hotkey_name;
    QPointer<QLineEdit> current_name;

    void create_one_hotkey_step(QHBoxLayout *this_layout, int step);
    void display_hotkey_steps();
    void delete_direction_pad(int step);
    void create_direction_pad(QHBoxLayout *this_layout, int step);
    void create_object_kind_dropbox(QHBoxLayout *this_layout, int this_step);
    void create_activation_dropbox(QHBoxLayout *this_layout, int this_step);
    void create_specific_object_choices(QHBoxLayout *this_layout, int this_step);
    void create_resting_choices(QHBoxLayout *this_layout, int this_step);
    void delete_resting_choices(int this_step);
    void delete_targeting_choices(int this_step);
    void create_targeting_choices(QHBoxLayout *this_layout, int step);
    void delete_specific_object_choices(int this_step);
    void create_spell_choice_dropbox(QHBoxLayout *this_layout, int step);
    void create_step_buttons(QHBoxLayout *this_layout, int step);
    int get_current_step(QString item_id);
    bool accept_object_kind(int k_idx, int tval);
    bool accept_activation_object(object_type *o_ptr);
    int find_selected_k_idx(int choice, int step);
    object_type find_selected_activation(int choice, int step);
    object_type find_inscribed_object(QString inscription);
    object_type create_selected_object(cmd_arg args);
    QPointer<QButtonGroup> group_directions;
    QPointer<QButtonGroup> group_target_choices;
    QPointer<QButtonGroup> group_specific_object;
    QPointer<QButtonGroup> group_resting_choices;

    int current_hotkey_int;

    void save_current_hotkey();
    void load_new_hotkey(int this_choice);

private slots:

    void active_hotkey_changed(int new_hotkey);
    void active_hotkey_name_changed(QString new_name);
    void active_hotkey_command_changed(int this_choice);
    void hotkey_step_direction_changed(int new_dir);
    void hotkey_step_target_changed(int new_target);
    void active_k_idx_changed(int choice);
    void active_activation_changed(int choice);
    void hotkey_step_obj_selection_changed(int new_selection);
    void hotkey_step_obj_select_name_changed(QString new_inscription);
    void hotkey_step_rest_choice_changed(int new_selection);
    void hotkey_rest_turncount_changed(int new_turncount);
    void active_spell_changed(int choice);
    void insert_step(void);
    void delete_step(void);
    void add_step(void);
};

extern void do_hotkey_manage();

extern single_hotkey running_hotkey;
extern single_hotkey player_hotkeys[NUM_HOTKEYS];
extern hotkey_list list_hotkeys[NUM_HOTKEYS];

#endif // HOTKEYS_H
