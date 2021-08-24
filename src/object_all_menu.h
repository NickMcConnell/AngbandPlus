#ifndef OBJECT_ALL_MENU_H
#define OBJECT_ALL_MENU_H

// Class to build an interactive dialog
// for all player info.

#include <src/object_dialog.h> //includes npp.h and player_command.h
#include "src/utilities.h"
#include <QLabel>
#include <QScrollArea>
#include <QTabWidget>
#include <QRadioButton>



// The tab order
enum
{
    TAB_FLOOR = 0,
    TAB_INVEN,
    TAB_EQUIP,
    TABS_MAX
};

class AllObjectsDialog : public QDialog
{
    Q_OBJECT
private:

    void update_header();

    // Header area
    QLabel *header_main;
    QLabel *header_weight1;
    QLabel *header_weight2;
    QLabel *header_objects;

    // Message area
    QLabel *message_area;

    QWidget *top_widget;

    QScrollArea *scroll_box;

    bool allow_floor;
    bool allow_inven;
    bool allow_equip;
    bool allow_quiver;

    void confirm_tabs(bool status_check);
    void link_pushbuttons();

    bool no_objects();
    int current_list;

    // Layouts and labels
    QRadioButton *floor_items;
    QRadioButton *inven_items;
    QRadioButton *equip_items;
    QButtonGroup *object_selection;

    QGridLayout *object_list;
    QGridLayout *quiver_list;

    QLabel *quiver_header;

private slots:
    void move_left(void);
    void move_right(void);
    void button_click(void);
    void switch_lists(int new_list);

protected:
    void keyPressEvent(QKeyEvent* which_key);


public:
    explicit AllObjectsDialog(bool do_buttons, int start_screen);
    void update_dialog();
    void close_dialog();


};

#endif // OBJECT_ALL_MENU_H
