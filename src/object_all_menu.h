#ifndef OBJECT_ALL_MENU_H
#define OBJECT_ALL_MENU_H

// Class to build an interactive dialog
// for all player info.

#include <src/object_dialog.h> //includes npp.h and player_command.h
#include "src/utilities.h"
#include <QLabel>
#include <QTabWidget>
#include <QRadioButton>
#include <src/nppdialog.h>
#include <QPointer>


// The tab order
enum
{
    TAB_FLOOR = 0,
    TAB_INVEN,
    TAB_EQUIP,
    TABS_MAX
};

class AllObjectsDialog : public NPPDialog
{
    Q_OBJECT
private:

    void update_header();

    // Header area
    QPointer<QLabel> header_main;
    QPointer<QLabel> header_weight1;
    QPointer<QLabel> header_weight2;
    QPointer<QLabel> header_objects;

    // Message area
    QPointer<QLabel> message_area;

    QPointer<QWidget> central;

    bool allow_floor;
    bool allow_inven;
    bool allow_equip;
    bool allow_quiver;

    void confirm_tabs(bool status_check);
    void link_pushbuttons();

    bool no_objects();
    int current_list;

    // Layouts and labels
    QPointer<QRadioButton> floor_items;
    QPointer<QRadioButton> inven_items;
    QPointer<QRadioButton> equip_items;
    QPointer<QButtonGroup> object_selection;

    QGridLayout *object_list;
    QGridLayout *quiver_list;

    QPointer<QLabel> quiver_header;


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
