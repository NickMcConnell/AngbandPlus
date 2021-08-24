#ifndef UTILITIES_H
#define UTILITIES_H

#include <QVector>
#include <src/defines.h>
#include <QString>
#include <QColor>
#include <QSpinBox>
#include <QDialog>
#include <QPointer>
#include <QSlider>

typedef struct letters_and_numbers letters_and_numbers;

typedef struct notes_type notes_type;

struct letters_and_numbers
{
    QChar let;
    int num;
};



struct notes_type
{
    byte player_level;
    s16b dun_depth;
    s32b game_turn;
    QString recorded_note;
};


class GetQuantityDialog : public QDialog
{
    Q_OBJECT

public:
    explicit GetQuantityDialog(QString prompt, int min, int max, int value);

    s16b current_quantity;

private slots:
    void max_number_button();
    void min_number_button();
    void update_quantity(int new_value);

private:

    QPointer<QSpinBox> this_quantity;
};

class GetQuantitySlider : public QDialog
{
    Q_OBJECT

public:
    explicit GetQuantitySlider(QString prompt, QString unit, int min, int max, int value);

    s16b current_quantity;

private slots:
    void update_quantity(int new_value);

private:
    QPointer<QSpinBox>  spin_quantity;
    QPointer<QSlider>   slider_quantity;
};


extern QVector<notes_type>  notes_log;



#define IS_GRAY(color) ((color).red() == (color).green() && (color).green() == (color).blue())

extern QString likert(int x, int y, byte *attr);
extern QString _num(int n);



#endif // UTILITIES_H
