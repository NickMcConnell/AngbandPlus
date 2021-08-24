
/*
 * Copyright (c) 2014 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

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
#include <QHash>
#include "npp.h"
#include "player_command.h"

static QHash<QString, QPoint> positions;

void NPPDialog::exec_saved(QString id)
{
    QPoint p;

    if (positions.contains(id)) {
        p = positions.value(id);
        this->move(p);
    }

    this->exec();

    positions.insert(id, this->pos());
}

NPPDialog::NPPDialog(QWidget *parent, int _padding, qreal _max_ratio) :
    QDialog(parent)
{
    padding = _padding;
    max_ratio = _max_ratio;

    scrollArea = new QScrollArea;
    scrollArea->setWidgetResizable(TRUE);
    scrollArea->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    QPointer<QVBoxLayout> layout = new QVBoxLayout;
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
    if (client)
    {
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
