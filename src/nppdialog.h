#ifndef NPPDIALOG_H
#define NPPDIALOG_H

#include <QDialog>

class QScrollArea;
class QLineEdit;

class NPPDialog : public QDialog
{
    Q_OBJECT
public:
    explicit NPPDialog(QWidget *parent = 0, int _padding = 20, qreal _max_ratio = 0.85);

    int padding;
    qreal max_ratio;
    QScrollArea *scrollArea;
    QWidget *client;

    virtual QSize sizeHint() const;

    void setClient(QWidget *_client);
    void clientSizeUpdated();

    void exec_saved(QString id);

signals:

public slots:

};



#endif // NPPDIALOG_H
