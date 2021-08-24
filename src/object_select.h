#ifndef OBJECT_SELECT_H
#define OBJECT_SELECT_H

#include "src/object_dialog.h"
#include <QTabWidget>
#include <QDialogButtonBox>
#include <QButtonGroup>
#include <QKeyEvent>
#include <QScrollArea>
#include <QDialog>
#include <QLabel>


// The numbered order of the tabs
enum
{
    TAB_FLOOR = 0,
    TAB_INVEN,
    TAB_EQUIP,
    TAB_QUIVER
};

class QTabWidget;



class ObjectSelectDialog : public QDialog
{
    Q_OBJECT

public:
    explicit ObjectSelectDialog(int *item, QString prompt, int mode, bool *success, bool *cancelled, int sq_y, int sq_x);


protected:
    void keyPressEvent(QKeyEvent* which_key);

private slots:
    // For the object select group pushbuttons.
    void object_select_button_press();
    // for the other buttons
    void info_buttons_click();
    void move_left(void);
    void move_right(void);

private:

    QTabWidget *object_tabs;
    QWidget *floor_tab;
    QWidget *inven_tab;
    QWidget *equip_tab;
    QWidget *quiver_tab;
    QLabel *main_prompt;

    QScrollArea *scroll_floor;
    QScrollArea *scroll_inven;
    QScrollArea *scroll_equip;
    QScrollArea *scroll_quiver;

    // Functions to build the actual tabs
    void build_floor_tab();
    void build_inven_tab();
    void build_equip_tab();
    void build_quiver_tab();

    void link_pushbuttons();

    byte find_starting_tab(int mode);

    QString add_equip_use(int slot);
    QString format_button_name(QChar char_index, object_type *o_ptr, bool is_equip, int slot);
    void add_object_button(object_type *o_ptr, s16b item_slot, QChar char_index, bool is_equip, QGridLayout *lay, int row, int col);


    //Functions to track the list of possible items
    void floor_items_count(int mode, int sq_y, int sq_x);
    void inven_items_count(int mode);
    void equip_items_count(int mode);
    void quiver_items_count(int mode);


    //Vectors to store the index numbers of the actual objects
    QVector<int> floor_items;
    QVector<int> inven_items;
    QVector<int> equip_items;
    QVector<int> quiver_items;

    bool allow_floor;
    bool allow_inven;
    bool allow_equip;
    bool allow_quiver;

    // Variables for keeping track of which item is selected
    QVector<byte> tab_order;
    s16b selected_item;

};


#endif // OBJECT_SELECT_H
