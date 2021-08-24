#ifndef SPELLS_H
#define SPELLS_H

#include <QDialog>
#include <QDialogButtonBox>
#include <QGridLayout>
#include <QButtonGroup>
#include <QPointer>


typedef struct monster_banish_choices monster_banish_choices;

struct monster_banish_choices
{
    QString mon_symbol;
    QString mon_race;
};

class BanishSelectDialog : public QDialog
{
    Q_OBJECT

public:
    BanishSelectDialog(void);
    bool return_value;

protected:
    void keyPressEvent(QKeyEvent* which_key);


private slots:
    void update_banish_choice(int choice);
    void add_monster_types(QGridLayout *return_layout);

private:
    int chosen_type;
    QPointer<QDialogButtonBox> button_boxes;
    QPointer<QButtonGroup>  banish_choice_group;
};

class DisplaySelfKnowledge : public QDialog
{
    Q_OBJECT

public:
    DisplaySelfKnowledge(void);
};

#endif // SPELLS_H
