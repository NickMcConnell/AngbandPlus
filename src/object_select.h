#ifndef OBJECT_SELECT_H
#define OBJECT_SELECT_H

#include "src/object_dialog.h"
#include <QTabWidget>
#include <QDialogButtonBox>
#include <QButtonGroup>
#include <QKeyEvent>
#include <QDialog>
#include <QLabel>
#include <QScrollArea>
#include <QPOinter>


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
    explicit ObjectSelectDialog(int *item, QString prompt, QString failure_message, int mode, bool *success, int sq_y, int sq_x);


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

    QPointer<QTabWidget> object_tabs;
    QPointer<QWidget> floor_tab;
    QPointer<QWidget> inven_tab;
    QPointer<QWidget> equip_tab;
    QPointer<QWidget> quiver_tab;
    QPointer<QLabel> main_prompt;

    QPointer<QScrollArea> scroll_floor;
    QPointer<QScrollArea> scroll_inven;
    QPointer<QScrollArea> scroll_equip;
    QPointer<QScrollArea> scroll_quiver;

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
