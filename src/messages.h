#ifndef MESSAGES_H
#define MESSAGES_H

#include <src/npp.h>
#include <QDialog>
#include <QTextEdit>
#include <QLabel>
#include <QPointer>

typedef struct message_type message_type;

struct message_type
{
    QColor msg_color;
    QString message;
    u16b repeats;
    s32b message_turn;
    bool append;
    bool displayed;

};

class DisplayMessages : public QDialog
{
    Q_OBJECT
public:
    DisplayMessages(void);


};

extern QVector<message_type> message_list;
extern QString completed_lines;
extern QString current_line;
extern QString current_repeat;
extern QString current_append;

extern void reset_message_display_marks(void);
extern void update_message_area(QTextEdit *message_area, int max_messages, QFont message_font);
extern void update_message_area(QLabel *message_label, int max_messages);
extern void stop_message_window_append(void);
extern void update_message_window(QTextEdit *message_area, QFont message_font);
extern void display_message_log(void);

#endif // MESSAGES_H
