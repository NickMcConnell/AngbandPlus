#include "nppdialog.h"
#include <QScrollArea>
#include <QDesktopWidget>
#include <QVBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QKeyEvent>
#include <QCoreApplication>
#include <QIntValidator>
#include "npp.h"
#include "player_command.h"



NPPDialog::NPPDialog(QWidget *parent, int _padding, qreal _max_ratio) :
    QDialog(parent)
{
    padding = _padding;
    max_ratio = _max_ratio;

    scrollArea = new QScrollArea;
    scrollArea->setWidgetResizable(TRUE);
    scrollArea->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    QVBoxLayout *layout = new QVBoxLayout;
    this->setLayout(layout);

    layout->addWidget(scrollArea);

    layout->setContentsMargins(0, 0, 0, 0);

    client = 0;
}

QSize NPPDialog::sizeHint() const
{
    if (client == 0) return QSize(-1, -1);

    QSize size = client->sizeHint();

    QDesktopWidget dsk;

    QRect geo = dsk.availableGeometry();

    // Check screen size
    int w = geo.width() * max_ratio;
    int h = geo.height() * max_ratio;

    if (size.width() > w) {
        size.setWidth(w);
    }

    if (size.height() > h) {
        size.setHeight(h);
    }

    size += QSize(padding, padding);

    return size;
}

void NPPDialog::clientSizeUpdated()
{
    if (client) {
        client->updateGeometry();
        client->setMinimumSize(client->sizeHint());
    }

    this->resize(this->sizeHint());
}

// Call this function AFTER setting the client layout
void NPPDialog::setClient(QWidget *_client)
{
    client = _client;

    scrollArea->setWidget(client);
}
