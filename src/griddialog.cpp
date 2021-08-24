#include "griddialog.h"
#include "npp.h"
#include "tilebag.h"
#include <QVBoxLayout>
#include <QGridLayout>
#include <QPushButton>
#include <QSpacerItem>
#include <QLabel>

GridDialog::GridDialog(int _y, int _x): NPPDialog()
{
    y = _y;
    x = _x;
    d_ptr = &dungeon_info[y][x];

    int n = 0;

    bool drugged = (p_ptr->timed[TMD_IMAGE] > 0);

    central = new QWidget;
    QVBoxLayout *lay1 = new QVBoxLayout;
    central->setLayout(lay1);
    this->setClient(central);   // Do this after setting layout

    QWidget *area2 = new QWidget;
    lay1->addWidget(area2);
    QGridLayout *lay2 = new QGridLayout;
    lay2->setContentsMargins(0, 0, 0, 0);
    area2->setLayout(lay2);

    lay2->setColumnStretch(3, 1);

    QFont font = ui_main_window_font();

    int col = 0;
    int row = 0;
    int m_idx = d_ptr->monster_idx;
    if (m_idx > 0 && mon_list[m_idx].ml && !drugged)
    {
        ++n;

        monster_type *m_ptr = mon_list + m_idx;
        monster_race *r_ptr = r_info + m_ptr->r_idx;

        QLabel *lb = new QLabel(QString(" %1 ").arg(r_ptr->d_char));
        lb->setStyleSheet(QString("background-color: black; color: %1;").arg(r_ptr->d_color.name()));
        lb->setFont(font);
        lay2->addWidget(lb, row, col++);

        QPixmap pix = ui_get_tile(r_ptr->tile_id, TRUE);
        QLabel *lb2 = new QLabel;
        lb2->setPixmap(pix);
        lay2->addWidget(lb2, row, col++);

        int gain_m = calc_energy_gain(m_ptr->m_speed);
        int gain_p = calc_energy_gain(p_ptr->state.p_speed);
        QString msg = monster_desc(m_ptr, 0x08);
        if (p_ptr->is_wizard)
        {
            msg.append(QString(" - HP: %4 - Energy: %2 - Player energy: %3")
                        .arg(gain_m).arg(gain_p).arg(m_ptr->hp));
        }
        msg = capitalize_first(msg);
        QPushButton *btn1 = new QPushButton(msg);
        QString item_id = QString("m%1").arg(m_idx);
        btn1->setObjectName(item_id);
        btn1->setStyleSheet("text-align: left;");
        lay2->addWidget(btn1, row, col++);
        connect(btn1, SIGNAL(clicked()), this, SLOT(item_click()));

        monster_race_track(m_ptr->r_idx);

        ++row;
    }

    int o_idx = d_ptr->object_idx;

    bool tracked_item = FALSE;

    while (o_idx && !drugged)
    {
        object_type *o_ptr = o_list + o_idx;
        int cur_o_idx = o_idx;
        o_idx = o_ptr->next_o_idx;

        if (!o_ptr->marked) continue;

        if (!tracked_item)
        {
            track_object(-cur_o_idx);
            tracked_item = TRUE;
        }

        ++n;

        col = 0;

        object_kind *k_ptr = k_info + o_ptr->k_idx;

        QChar chr = k_ptr->get_char();
        QColor color = k_ptr->get_color();
        QString tile = k_ptr->get_tile_id();

        QLabel *lb = new QLabel(QString(" %1 ").arg(chr));
        lb->setStyleSheet(QString("background-color: black; color: %1;").arg(color.name()));
        lb->setFont(font);
        lay2->addWidget(lb, row, col++);

        QPixmap pix = ui_get_tile(tile, TRUE);
        QLabel *lb2 = new QLabel;
        lb2->setPixmap(pix);
        lay2->addWidget(lb2, row, col++);

        QString name = object_desc(o_ptr, ODESC_FULL | ODESC_PREFIX);
        name = capitalize_first(name);
        QPushButton *btn1 = new QPushButton(name);
        QString item_id = QString("o%1").arg(cur_o_idx);
        btn1->setObjectName(item_id);
        btn1->setStyleSheet("text-align: left;");
        lay2->addWidget(btn1, row, col++);
        connect(btn1, SIGNAL(clicked()), this, SLOT(item_click()));

        ++row;
    }

    if (d_ptr->cave_info & (CAVE_MARK | CAVE_SEEN))
    {
        ++n;

        col = 0;

        int feat = d_ptr->feature_idx;
        feat = f_info[feat].f_mimic;
        feature_type *f_ptr = f_info + feat;

        QLabel *lb = new QLabel(QString(" %1 ").arg(f_ptr->d_char));
        lb->setStyleSheet(QString("background-color: black; color: %1;").arg(f_ptr->d_color.name()));
        lb->setFont(font);
        lay2->addWidget(lb, row, col++);

        QPixmap pix = ui_get_tile(f_ptr->tile_id, TRUE);
        QLabel *lb2 = new QLabel;
        lb2->setPixmap(pix);
        lay2->addWidget(lb2, row, col++);

        QString name = feature_desc(feat, true, false);
        name = capitalize_first(name);
        QPushButton *btn1 = new QPushButton(name);
        QString item_id = QString("f%1").arg(feat);
        btn1->setObjectName(item_id);
        btn1->setStyleSheet("text-align: left;");
        lay2->addWidget(btn1, row, col++);
        connect(btn1, SIGNAL(clicked()), this, SLOT(item_click()));

        ++row;

        feature_kind_track(feat);
    }

    int x_idx = d_ptr->effect_idx;
    while (x_idx && (d_ptr->cave_info & (CAVE_MARK | CAVE_SEEN))) {
        effect_type *x_ptr = x_list + x_idx;
        x_idx = x_ptr->next_x_idx;

        if (x_ptr->x_flags & EF1_HIDDEN) continue;

        int feat = x_ptr->x_f_idx;
        if (!feat) continue;
        feat = f_info[feat].f_mimic;
        if (!feat) continue;
        feature_type *f_ptr = f_info + feat;

        col = 0;

        QLabel *lb = new QLabel(QString(" %1 ").arg(f_ptr->d_char));
        lb->setStyleSheet(QString("background-color: black; color: %1;").arg(f_ptr->d_color.name()));
        lb->setFont(font);
        lay2->addWidget(lb, row, col++);

        QPixmap pix = ui_get_tile(f_ptr->tile_id, TRUE);
        QLabel *lb2 = new QLabel;
        lb2->setPixmap(pix);
        lay2->addWidget(lb2, row, col++);

        QString name = feature_desc(feat, true, false);
        name = capitalize_first(name);
        QPushButton *btn1 = new QPushButton(name);
        QString item_id = QString("f%1").arg(feat);
        btn1->setObjectName(item_id);
        btn1->setStyleSheet("text-align: left;");
        lay2->addWidget(btn1, row, col++);
        connect(btn1, SIGNAL(clicked()), this, SLOT(item_click()));

        ++n;
        ++row;
    }

    QSpacerItem *spacer = new QSpacerItem(1, 1, QSizePolicy::Fixed, QSizePolicy::Expanding);
    lay2->addItem(spacer, row, 0);
    ++row;

    QWidget *area3 = new QWidget;
    lay1->addWidget(area3);
    QHBoxLayout *lay3 = new QHBoxLayout;
    lay3->setContentsMargins(0, 0, 0, 0);
    area3->setLayout(lay3);

    spacer = new QSpacerItem(1, 1, QSizePolicy::Expanding, QSizePolicy::Fixed);
    lay3->addItem(spacer);

    QPushButton *btn_close = new QPushButton("Ok");
    lay3->addWidget(btn_close);
    connect(btn_close, SIGNAL(clicked()), this, SLOT(reject()));

    this->clientSizeUpdated();

    handle_stuff();

    if (n > 0) {
        (this->findChildren<QPushButton *>().at(0))->setFocus();
        this->exec();
    }

    else message(tr("There is nothing to see here"));
}

void GridDialog::item_click()
{
    QString item_id = QObject::sender()->objectName();
    QChar kind = item_id.at(0);
    int idx = item_id.mid(1).toInt();
    if (kind == 'm') {
        describe_monster(mon_list[idx].r_idx, false, "");
    }
    else if (kind == 'o') {
        object_info_screen(o_list + idx);
    }
    else {
        describe_feature(idx, false);
    }
}
