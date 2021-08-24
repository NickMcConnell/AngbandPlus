
/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */


#include "optionsdialog.h"
#include "npp.h"
#include <src/help.h>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QCheckBox>
#include <QLabel>
#include <QGridLayout>
#include <QSpinBox>
#include <QTabWidget>

OptionsDialog::OptionsDialog()
{    
    QPointer<QWidget> central = new QWidget;
    QPointer<QVBoxLayout> lay1 = new QVBoxLayout;
    central->setLayout(lay1);
    this->setClient(central);

    QPointer<QTabWidget> tabs = new QTabWidget;
    lay1->addWidget(tabs);

    QString titles[] = {
        "Gameplay",
        "Display",
        "Disturbance",
        "Birth",
        "Cheat",
        ""
    };

    for (int t = 0; titles[t] != ""; t++)
    {
        QPointer<QWidget> wid2 = new QWidget;
        QPointer<QVBoxLayout> lay2 = new QVBoxLayout;
        wid2->setLayout(lay2);

        tabs->addTab(wid2, titles[t]);

        for (int i = 0; i < OPT_PAGE_PER; i++) {
            byte idx;
            if ((game_mode == GAME_NPPANGBAND) || (game_mode == GAME_MODE_UNDEFINED))
            {
                idx = option_page_nppangband[t][i];
            }
            else idx = option_page_nppmoria[t][i];

            if (idx == OPT_NONE) continue;

            // hack - this option handled on birth dialog
            if (idx == OPT_birth_point_based) continue;

            option_entry *opt = options + idx;
            if (opt->name.isEmpty()) continue;

            QPointer<QCheckBox> chk = new QCheckBox(opt->name + " - " + opt->description);
            chk->setChecked(op_ptr->opt[idx]);
            chk->setProperty("opt_idx", idx);
            chk->setToolTip(get_help_topic(QString("option_info"), opt->name));

            // Only edit birth options during character creation.
            if (p_ptr->game_turn)
            {
                if ((idx >= OPT_BIRTH_HEAD) && (idx <= OPT_BIRTH_TAIL))
                {
                    chk->setEnabled(FALSE);
                }
            }

            lay2->addWidget(chk);
        }

        lay2->addStretch(1);
    }

    QPointer<QWidget> wid3 = new QWidget;
    QPointer<QHBoxLayout> lay3 = new QHBoxLayout;
    wid3->setLayout(lay3);

    lay1->addWidget(wid3);

    lay3->addStretch(1);

    QPointer<QPushButton> btn1 = new QPushButton(tr("Save"));
    lay3->addWidget(btn1);
    connect(btn1, SIGNAL(clicked()), this, SLOT(on_save()));

    QPointer<QPushButton> btn2 = new QPushButton(tr("Cancel"));
    lay3->addWidget(btn2);
    connect(btn2, SIGNAL(clicked()), this, SLOT(reject()));

    this->clientSizeUpdated();
}

void OptionsDialog::on_save()
{
    QList<QCheckBox *> ops = this->findChildren<QCheckBox *>();
    for (int i = 0; i < ops.size(); i++)
    {
        QCheckBox *chk = ops.at(i);
        int idx = chk->property("opt_idx").toInt();
        op_ptr->opt[idx] = chk->isChecked();
    }

    this->accept();
}
