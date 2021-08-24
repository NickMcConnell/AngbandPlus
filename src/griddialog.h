#ifndef GRIDDIALOG_H
#define GRIDDIALOG_H

#include "nppdialog.h"

class dungeon_type;

class GridDialog : public NPPDialog
{
    Q_OBJECT
public:
    int y, x;
    dungeon_type *d_ptr;
    QWidget *central;

    GridDialog(int _y, int _x);

public slots:
    void item_click();
};

#endif // GRIDDIALOG_H
