#ifndef DUN_TRAPS_H
#define DUN_TRAPS_H

#include <QSignalMapper>
#include <QDialogButtonBox>
#include <QDialog>
#include <QPointer>


class TrapSelectDialog : public QDialog
{
    Q_OBJECT

public:
    explicit TrapSelectDialog(void);

    int trap_choice;

private slots:
    void on_dialog_buttons_pressed(QAbstractButton *);
    void trap_choice_sturdy(void);
    void trap_choice_slowing(void);
    void trap_choice_confusion(void);
    void trap_choice_poison(void);
    void trap_choice_life_drain(void);
    void trap_choice_lightning(void);
    void trap_choice_explosive(void);
    void trap_choice_portal(void);
    void trap_choice_dispell(void);



};

#endif // DUN_TRAPS_H
