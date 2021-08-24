#ifndef COMMAND_LIST_H
#define COMMAND_LIST_H


#include <src/player_screen.h>
#include <src/nppdialog.h>
#include <QPointer>

typedef struct command_desc command_desc;

struct command_desc
{
    QString command_title;
    QString command_key;
};

class KeyboardCommandList : public NPPDialog
{
    Q_OBJECT

public:
    explicit KeyboardCommandList(void);

private:
    void add_dir_keyboard(QVBoxLayout *return_layout, bool keyboard);
    void add_dir_commands(QGridLayout *return_layout);
    void add_keyboard_commands(QGridLayout *return_layout);
    QPointer<QWidget> central;
};

class MouseCommandList : public NPPDialog
{
    Q_OBJECT

public:
    explicit MouseCommandList(void);

private:
    void add_mouse_commands(QVBoxLayout *return_layout);
    QPointer<QWidget> central;

};

class TargetCommandList : public NPPDialog
{
    Q_OBJECT

public:
    explicit TargetCommandList(void);


private:
    void add_dir_targeting(QVBoxLayout *return_layout, bool keyboard);
    void add_targeting_commands(QGridLayout *return_layout);
    QPointer<QWidget> central;
};

extern void do_cmd_list_keyboard_commands(void);
extern void do_cmd_list_mouse_commands(void);
extern void do_cmd_list_targeting_commands(void);
extern void commands_new_keyset(int key_press, bool shift_key, bool alt_key, bool ctrl_key, bool meta_key);
extern void commands_angband_keyset(int key_press, bool shift_key, bool alt_key, bool ctrl_key, bool meta_key);
extern void commands_roguelike_keyset(int key_press, bool shift_key, bool alt_key, bool ctrl_key, bool meta_key);

#endif // COMMAND_LIST_H
