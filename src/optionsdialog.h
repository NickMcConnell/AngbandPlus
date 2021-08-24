#ifndef OPTIONSDIALOG_H
#define OPTIONSDIALOG_H

#include "nppdialog.h"

class OptionsDialog : public NPPDialog
{
    Q_OBJECT

public:
    OptionsDialog();

private slots:
    void on_save();
};

#endif // OPTIONSDIALOG_H
