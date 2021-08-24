/* File: qt_mainwindow.cpp */

/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "storedialog.h"
#include <QKeyEvent>

#include <QTabWidget>
#include <QPushButton>
#include <QSpacerItem>
#include <QGroupBox>
#include <QSpinBox>
#include <QCoreApplication>
#include <QLabel>
#include <QPixmap>
#include "npp.h"
#include <src/cmds.h>
#include "store.h"
#include <src/help.h>

void launch_store(int store_idx)
{
    p_ptr->in_store = TRUE;
    /* Check if we can enter the store */
    if (birth_no_stores)
    {
        pop_up_message_box("The doors are locked.");
        return;
    }

    /* See if we are holding a quest item */
    if (store_idx == STORE_GUILD)
    {
        /* Check for outstanding rewards */
        quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

        if ((q_ptr->q_type == QUEST_VAULT) && (!guild_quest_complete()))
        {
            /* The artifact has been returned, the quest is a success */
            if (quest_item_slot() > -1) quest_finished(q_ptr);
        }

        if ((quest_type_collection(q_ptr)) && (!guild_quest_complete()))
        {
            if (quest_item_count() >= quest_collection_num(q_ptr)) quest_finished(q_ptr);
        }
    }

    StoreDialog *dlg = new StoreDialog(store_idx);
    dlg->exec_saved("StoreDialog");
    delete dlg;
    p_ptr->in_store = FALSE;
    p_ptr->message_append_stop();
    process_player_energy(BASE_ENERGY_MOVE);
}

void StoreDialog::add_weight_label(QGridLayout *lay, object_type *o_ptr, int row, int col)
{
    // Add the weight
    QString weight_printout = (formatted_weight_string(o_ptr->weight));
    weight_printout.append(" lbs");
    QLabel *weight = new QLabel(weight_printout);
    weight->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    lay->addWidget(weight, row, col);
}

void StoreDialog::add_help_label(QGridLayout *lay, QString id, int row, int col)
{
    QPushButton *help_button = new QPushButton;
    help_button->setIcon(QIcon(":/icons/lib/icons/help.png"));
    help_button->setObjectName(id);
    connect(help_button, SIGNAL(clicked()), this, SLOT(help_click()));
    lay->addWidget(help_button, row, col);
}

void StoreDialog::update_header()
{
    //max capactity in ounces
    u16b max_capacity = normal_speed_weight_limit();

    u32b weight_percent = p_ptr->total_weight * 100 / max_capacity;

    header_weight1->setText((QString("<b><big>Burden: %1 lbs (%2% capacity)</big></b>")
                             .arg(formatted_weight_string(p_ptr->total_weight)) .arg(weight_percent)));
    if (p_ptr->total_weight > max_capacity)
    {
        int overweight = p_ptr->total_weight - max_capacity;
        header_weight2->setText(QString("(%1 lbs overweight)")
                        .arg((formatted_weight_string(overweight))));
    }
    else if (p_ptr->total_weight < max_capacity)
    {
        int underweight = max_capacity - p_ptr->total_weight;
        header_weight2->setText(QString("(%1 lbs underweight)")
                        .arg(formatted_weight_string(underweight)));
    }
}

StoreDialog::StoreDialog(int _store, QWidget *parent): NPPDialog(parent)
{
    store_idx = _store;
    home = (store_idx == STORE_HOME);
    guild = (store_idx == STORE_GUILD);

    central = new QWidget;
    QVBoxLayout *lay1 = new QVBoxLayout;
    central->setLayout(lay1);
    lay1->setSpacing(10);
    this->setClient(central);  // IMPORTANT: it must be called AFTER setting the layout

    message_area = new QLabel;
    message_area->setWordWrap(TRUE);
    message_area->setAutoFillBackground(TRUE);
    QPalette this_palette;
    this_palette.setColor(QPalette::Window, QColor(Qt::black));
    message_area->setPalette(this_palette);
    lay1->addWidget(message_area);
    message_area->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Minimum);
    update_message_area(message_area, 3);

    QWidget *area1 = new QWidget;
    lay1->addWidget(area1);
    QHBoxLayout *lay3 = new QHBoxLayout;
    area1->setLayout(lay3);
    lay3->setContentsMargins(0, 0, 0, 0);

    if (guild)
    {
        QString title = QString("<b>The Adventurer's Guild</b>   - ");
        setWindowTitle(title);
        title.append(get_rep_guild());
        QLabel *guild_intro = new QLabel(title);
        lay3->addWidget(guild_intro);
    }
    else if (!home)
    {
        owner_type *ot_ptr = &b_info[(store_idx * z_info->b_max) + store[store_idx].owner];
        int feat = dungeon_info[p_ptr->py][p_ptr->px].feature_idx;
        QString shop_name = f_info[feat].f_name;
        setWindowTitle(shop_name);
        QString msg = QString("<b>%1</b> - %2 (%3)").arg(shop_name).arg(ot_ptr->owner_name)
                .arg(ot_ptr->max_cost);
        QLabel *store_info = new QLabel(msg);
        lay3->addWidget(store_info);
    }
    else
    {
        lay3->addWidget(new QLabel("<b>Your home</b>"));
    }
    lay3->addStretch(1);

    header_weight1 = new QLabel();
    header_weight2 = new QLabel();
    lay3->addWidget(header_weight1);
    update_header();

    lay3->addStretch(1);

    QPushButton *btn_buy = new QPushButton(home ? "Retrieve/Stash (F2)": "Buy/Sell (F2)");
    lay3->addWidget(btn_buy);
    connect(btn_buy, SIGNAL(clicked()), this, SLOT(buy_sell_click()));

    QPushButton *btn_toggle = new QPushButton("Toggle inven/equip (F4)");
    lay3->addWidget(btn_toggle);
    connect(btn_toggle, SIGNAL(clicked()), this, SLOT(toggle_inven()));

    QWidget *area4 = new QWidget;
    QHBoxLayout *lay6 = new QHBoxLayout;
    area4->setLayout(lay6);
    lay6->setContentsMargins(0, 0, 0, 0);
    lay1->addWidget(area4);

    QLabel *gold_label = new QLabel();
    gold_label->setObjectName("gold_label");
    gold_label->setStyleSheet("font-weight: bold;");
    lay6->addWidget(gold_label);
    reset_gold();

    mode_label = new QLabel("");
    lay6->addWidget(mode_label);
    mode_label->setStyleSheet("font-weight: bold;");


    lay6->addStretch(1);
    lay6->addWidget(header_weight2);

    set_mode(SMODE_DEFAULT);

    lay6->addStretch(1);

    QPushButton *btn_wield = new QPushButton("Wield (F6)");
    lay6->addWidget(btn_wield);
    connect(btn_wield, SIGNAL(clicked()), this, SLOT(wield_click()));

    QPushButton *btn_takeoff = new QPushButton("Take off (F7)");
    lay6->addWidget(btn_takeoff);
    connect(btn_takeoff, SIGNAL(clicked()), this, SLOT(takeoff_click()));

    if (guild || (!home && one_in_(3)))
    {
        QWidget *area_greeting = new QWidget;
        QHBoxLayout *lay_greeting = new QHBoxLayout;
        area_greeting->setLayout(lay_greeting);
        lay_greeting->setContentsMargins(0, 0, 0, 0);
        lay1->addWidget(area_greeting);

        QString greeting;
        if (guild) greeting = get_welcome_guild();
        else greeting = store_welcome(store_idx);

        QString msg = (QString("<big>%1</big>").arg(greeting));
        QLabel *store_greeting = new QLabel(msg);
        lay_greeting->addWidget(store_greeting);
    }

    QWidget *area2 = new QWidget;
    QHBoxLayout *lay2 = new QHBoxLayout;
    area2->setLayout(lay2);
    lay2->setContentsMargins(0, 0, 0, 0);
    lay1->addWidget(area2);

    QGroupBox *box1 = new QGroupBox(home ? "Home items":
            (guild ? "Guild Services and Available Quests" : "Store Inventory and Services"));
    box1->setStyleSheet("font-weight: bold;");
    lay2->addWidget(box1);
    QVBoxLayout *lay4 = new QVBoxLayout;
    box1->setLayout(lay4);

    store_area = new QWidget;
    lay4->addWidget(store_area);

    char_tabs = new QTabWidget;
    lay2->addWidget(char_tabs);

    inven_tab = new QWidget;
    inven_tab->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Expanding);
    equip_tab = new QWidget;
    equip_tab->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Expanding);

    char_tabs->addTab(inven_tab, tr("Inventory"));
    char_tabs->addTab(equip_tab, tr("Equipment"));
    char_tabs->setCurrentIndex(0);

    this->reset_store();
    this->reset_inventory();
    this->reset_equip();

    // Do a quest status for the guild.
    quest_area = new QWidget;
    QGridLayout *quest_layout = new QGridLayout;
    quest_area->setLayout(quest_layout);
    quest_layout->setContentsMargins(0, 0, 0, 0);
    lay1->addWidget(quest_area);
    QLabel *quest_header = new QLabel("<b><big>Your Current Quest:</big></b>");
    quest_header->setAlignment(Qt::AlignLeft);
    quest_status = new QLabel("Quest Desc");
    quest_picture = new QLabel("Quest Picture");
    quest_layout->addWidget(quest_header, 0, 1);
    quest_layout->addWidget(quest_status, 1, 1);
    quest_layout->addWidget(quest_picture, 1, 0);

    QSpacerItem *spacer = new QSpacerItem(1, 1, QSizePolicy::Expanding, QSizePolicy::Fixed);
    quest_layout->addItem(spacer, 1, 2);

    this->reset_quest_status();

    QWidget *area3 = new QWidget;
    QHBoxLayout *lay5 = new QHBoxLayout;
    area3->setLayout(lay5);
    lay5->setContentsMargins(0, 0, 0, 0);
    lay1->addWidget(area3);

    lay5->addStretch(1);

    QPushButton *btn_close = new QPushButton("Close");
    lay5->addWidget(btn_close);
    connect(btn_close, SIGNAL(clicked()), this, SLOT(reject()));

    this->clientSizeUpdated();
}

void StoreDialog::wield_click()
{
    do_cmd_wield();
    reset_all();
}

void StoreDialog::takeoff_click()
{
    do_cmd_takeoff();
    reset_all();
}


void StoreDialog::reset_gold()
{
    QLabel *label = this->findChild<QLabel *>("gold_label");
    label->setText(QString("Gold: %1").arg(number_to_formatted_string(p_ptr->au)));
}

void StoreDialog::reset_quest_status()
{
    if ((!guild) || (!guild_quest_level()))
    {
        quest_area->hide();
        return;
    }
    // First take care of the quest status label
    quest_area->show();
    quest_status->setText(QString("<big>%1</big>")
                 .arg(describe_quest(guild_quest_level())));

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    //Now add the picture if necessary
    if (!quest_single_r_idx(q_ptr))
    {
        quest_picture->hide();
        return;
    }

    quest_picture->show();

    monster_race *r_ptr = &r_info[q_info[GUILD_QUEST_SLOT].mon_idx];
    if (ui_using_monster_tiles())
    {
        QPixmap pix = ui_get_tile(r_ptr->tile_id, TRUE);
        quest_picture->setPixmap(pix);
    }
    else
    {
        quest_picture->setText(QString("<b><big>'%1'</big></b>") .arg(r_ptr->d_char));
        quest_picture->setStyleSheet(QString("color: %1;").arg(r_ptr->d_color.name()));
    }

    quest_picture->adjustSize();
    quest_status->adjustSize();

}

void StoreDialog::buy_sell_click()
{
    set_mode(SMODE_BUY);
}


// Don't display quest reward inventory until the quest is complete.
bool StoreDialog::should_show_inventory(void)
{
    if (store_idx != STORE_GUILD) return (TRUE);

    if (guild_quest_complete()) return (TRUE);

    return (FALSE);
}

//  Determine if a store should offer quests or not.
bool StoreDialog::should_offer_quests(void)
{
    // Only in the guild
    if (store_idx != STORE_GUILD) return (FALSE);

    if (birth_no_quests) return (FALSE);

    /* No quest options if they currently have an active one. */
    if (guild_quest_level()) return (FALSE);

    /*
     * Checks the no quests option, as
     * well as the potential quest level
     */
    if (!can_quest_at_level()) return (FALSE);

    return (TRUE);


}

//  Determine if a store should offer a certain service or not.
bool StoreDialog::should_offer_service(byte service_num)
{
    service_info *service_ptr = &services_info[service_num];
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Services are store-specific */
    if (service_ptr->service_store != store_idx) return (FALSE);

    /*
     * The guild only offers certain services
     * depending on the active quest.
     */

    /* Offer this service only if there is a quest to abandon. */
    if (service_num == SERVICE_ABANDON_QUEST)
    {
        /*We finished the quest, why abandon it?*/
        if (guild_quest_complete()) return (FALSE);

        /* No current guild quest */
        if (!q_ptr->q_type) return (FALSE);

        if (!guild_quest_level()) return (FALSE);

        return (TRUE);
    }

    if ((service_num >= QUEST_REWARD_HEAD) &&
             (service_num <= QUEST_REWARD_TAIL))
    {
        /* Not currently offering a reward */
        if (!guild_quest_complete()) return (FALSE);

        // Certain services only if these rewards are offered.
        if (service_num == SERVICE_QUEST_REWARD_INC_HP)
        {
            if (!(q_ptr->q_reward & (REWARD_INC_HP))) return (FALSE);
        }
        else if (service_num == SERVICE_QUEST_REWARD_RANDART)
        {
            if (!(q_ptr->q_reward & (REWARD_RANDART))) return (FALSE);
        }
        else if (service_num == SERVICE_QUEST_REWARD_INC_STAT)
        {
            if (!(q_ptr->q_reward & (REWARD_INC_STAT))) return (FALSE);
        }
        else if (service_num == SERVICE_QUEST_REWARD_AUGMENTATION)
        {
            if (!(q_ptr->q_reward & (REWARD_AUGMENTATION))) return (FALSE);
        }

        return (TRUE);
    }

    /* Filter out quest-specific services when appropriate. */
    if (service_num == SERVICE_PROBE_QUEST_MON)
    {
        if (!guild_quest_level()) return (FALSE);
        if (guild_quest_complete()) return (FALSE);

        if (q_ptr->q_type == QUEST_VAULT) return (FALSE);
        if (q_ptr->q_type == QUEST_GREATER_VAULT) return (FALSE);
        if (quest_type_collection(q_ptr)) return (FALSE);
        if (quest_multiple_r_idx(q_ptr)) return (FALSE);
    }

    return (TRUE);
}


s32b StoreDialog::price_services(int service_idx)
{
    service_info *service_ptr = &services_info[service_idx];

    /* get the service price*/
    u32b price = service_ptr->service_price;

    /*adjust price, but not for the guild*/
    if (store_idx != STORE_GUILD)
    {
        /* Extract the "minimum" price */
        if (game_mode == GAME_NPPMORIA)
        {
            price = ((price * moria_chr_adj()) / 100L);
        }
        else price = ((price * adj_chr_gold[p_ptr->state.stat_index[A_CHR]]) / 100L);
    }

    /*Guild price factoring*/
    else
    {
        if (p_ptr->q_fame < 1000) price += price * (1000 - p_ptr->q_fame) / 1000;
    }

    return(price);
}


void StoreDialog::reset_store()
{

    QGridLayout *lay = dynamic_cast<QGridLayout *>(store_area->layout());
    if (lay == 0)
    {
        lay = new QGridLayout;
        lay->setColumnStretch(1, 1);
        store_area->setLayout(lay);
    }

    // Remove previous items
    clear_layout(lay);
    int row = 0;
    int col = 2;

    // Add weight label
    QLabel *weight_label = new QLabel("Weight");
    weight_label->setText("Weight");
    weight_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    lay->addWidget(weight_label, row, col++);

    //Add price label
    if (!home)
    {
        QLabel *price_label = new QLabel("Price");
        price_label->setText("Price");
        price_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
        lay->addWidget(price_label, row, col);
    }

    row++;

    store_type *st = &store[store_idx];
    int i;
    int k = 1;

    // Display the services
    for (i = 0; i < STORE_SERVICE_MAX; i++)
    {

        /* Check if the services option is disabled */
        if (birth_no_store_services) break;

        /* Services are store-specific */
        if (!should_offer_service(i)) continue;

        service_info *service_ptr = &services_info[i];

        col = 0;

        // Make an id for the service
        QString id = QString("s%1").arg(i);

        QLabel *lb = new QLabel(QString("%1)").arg(k++));
        lb->setProperty("item_id", QVariant(id));
        lay->addWidget(lb, row, col++);

        QString desc = service_ptr->service_names;
        s32b price = price_services(i);
        QColor service_color = make_color_readable(defined_colors[TERM_GREEN]);
        QString s = QString("color: %1;") .arg(service_color.name());
        QString style = "text-align: left; font-weight: bold;";
        style += s;

        if (price <= p_ptr->au)
        {
            QPushButton *btn = new QPushButton(desc);
            btn->setProperty("item_id", QVariant(id));

            btn->setStyleSheet(style);
            btn->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
            connect(btn, SIGNAL(clicked()), this, SLOT(service_click()));
            lay->addWidget(btn, row, col++);
        }
        else
        {
            QLabel *lb2 = new QLabel(desc);
            lb2->setStyleSheet(style);
            lay->addWidget(lb2, row, col++);
        }

        // Skip the "weight" column.
        col++;

        QString price_text = number_to_formatted_string(price);
        price_text = price_text.rightJustified(15, ' ');

        QLabel *l = new QLabel("price");
        l->setText(price_text);
        l->setAlignment(Qt::AlignRight);
        lay->addWidget(l, row, col++);

        add_help_label(lay, id, row, col++);

        ++row;
    }

    /*get a list of allowable quests*/
    if (should_offer_quests()) for (i = 0;i < QUEST_SLOT_MAX; i++)
    {
        if (!quest_allowed(i)) continue;

        col = 0;

        // Make an id for the quest
        QString id = QString("q%1").arg(i);

        QLabel *lb = new QLabel(QString("%1)").arg(k++));
        lb->setProperty("item_id", QVariant(id));
        lay->addWidget(lb, row, col++);

        QString desc = quests_info[i];
        QColor quest_color = make_color_readable(defined_colors[TERM_GREEN]);
        QString s = QString("color: %1;").arg(quest_color.name());
        QString style = "text-align: left; font-weight: bold;";
        style += s;

        QPushButton *btn = new QPushButton(desc);
        btn->setProperty("item_id", QVariant(id));

        btn->setStyleSheet(style);
        btn->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
        connect(btn, SIGNAL(clicked()), this, SLOT(quest_click()));
        lay->addWidget(btn, row, col++);

        // Skip over the price and weight column.
        col+=2;

        add_help_label(lay, id, row, col++);

        row++;
    }

    if (should_show_inventory()) for (i = 0; i < st->stock_num; i++)
    {
        object_type *o_ptr = &st->stock[i];
        if (o_ptr->k_idx == 0) continue;

        o_ptr->has_been_seen();

        col = 0;

        // Make an id for the item
        QString id = QString("p%1").arg(i);

        QLabel *lb = new QLabel(QString("%1)").arg(number_to_letter(i)));
        lb->setProperty("item_id", QVariant(id));
        lay->addWidget(lb, row, col++);

        QString desc = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);
        s32b price = price_item(store_idx, o_ptr, false);
        QString s = QString("color: %1;").arg(get_object_color(o_ptr).name());
        QString style = "text-align: left; font-weight: bold;";
        style += s;

        if (home || (guild) || price <= p_ptr->au)
        {
            QPushButton *btn = new QPushButton(desc);
            btn->setProperty("item_id", QVariant(id));

            btn->setStyleSheet(style);
            btn->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
            connect(btn, SIGNAL(clicked()), this, SLOT(item_click()));
            lay->addWidget(btn, row, col++);
        }
        else
        {
            QLabel *lb2 = new QLabel(desc);
            lb2->setStyleSheet(style);
            lay->addWidget(lb2, row, col++);
        }

        // Add the weight
        add_weight_label(lay, o_ptr, row, col++);

        // Provice a price, if necessary.
        if (!home && !guild)
        {
            s32b price = price_item(store_idx, o_ptr, FALSE);
            QString price_text = number_to_formatted_string(price);
            price_text = price_text.rightJustified(16, ' ');
            QLabel *price_out = new QLabel("Price");
            price_out->setText(price_text);
            price_out->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
            lay->addWidget(price_out, row, col);
        }
        col++;

        add_help_label(lay, id, row, col++);

        ++row;
    }
    QSpacerItem *spacer = new QSpacerItem(1, 1, QSizePolicy::Expanding, QSizePolicy::Expanding);
    lay->addItem(spacer, row++, 0);
}

void StoreDialog::help_click()
{
    QString id = QObject::sender()->objectName();
    int o_idx = id.mid(1).toInt();
    object_type *o_ptr;

    if (id.at(0) == 'q')
    {
        QString quest_name = quests_info[o_idx];

        QString info = get_help_topic("store_info", quest_name);

        pop_up_message_box(info, QMessageBox::Information);
    }
    else if (id.at(0) == 's')
    {
        QString service_name = services_info[o_idx].service_names;

        QString info = get_help_topic("store_info", service_name);

        pop_up_message_box(info, QMessageBox::Information);
    }
    else if (id.at(0) == 'p')
    {
        o_ptr = &(store[store_idx].stock[o_idx]);
        object_info_screen(o_ptr);
    }
    else
    {
        o_ptr = &(inventory[o_idx]);
        object_info_screen(o_ptr);
    }

}

void StoreDialog::set_mode(int _mode)
{
    mode = _mode;
    QString names[] = {
        QString(""), QString(home ? "Retrieving/Stashing": "Buying/Selling"), QString(home ? "Stashing": "Selling"),
        QString("Examining")
    };
    QString text = names[mode];
    if (!text.isEmpty()) text.append(". Click an item.");
    mode_label->setText(text);
}

void StoreDialog::reset_inventory()
{
    QWidget *tab = inven_tab;
    QGridLayout *lay = dynamic_cast<QGridLayout *>(tab->layout());
    if (lay == 0) {
        lay = new QGridLayout;
        lay->setColumnStretch(1, 1);
        tab->setLayout(lay);
    }
    // Remove previous items
    clear_layout(lay);
    int row = 0;
    lay->addWidget(new QLabel("Weight"), row, 2);
    if (!home)
    {
        QLabel *price_label = new QLabel("Price");
        price_label->setText("price");
        price_label->setAlignment(Qt::AlignRight);
        lay->addWidget(price_label, row, 3);
    }
    row++;

    for (int i = 0; i < INVEN_WIELD - 1; i++)
    {
        object_type *o_ptr = inventory + i;
        if (o_ptr->k_idx == 0) continue;

        o_ptr->has_been_seen();

        // Make an id for the item
        QString id = QString("i%1").arg(i);

        QLabel *lb = new QLabel(QString("%1)").arg(number_to_letter(i).toUpper()));
        lb->setProperty("item_id", QVariant(id));
        lay->addWidget(lb, row, 0);

        QString desc = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);
        QString s = QString("color: %1;").arg(get_object_color(o_ptr).name());
        QString style = "text-align: left; font-weight: bold;";
        style += s;

        if (home || store_will_buy(store_idx, o_ptr))
        {
            QPushButton *btn = new QPushButton(desc);
            btn->setProperty("item_id", QVariant(id));

            btn->setStyleSheet(style);
            btn->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
            connect(btn, SIGNAL(clicked()), this, SLOT(item_click()));
            lay->addWidget(btn, row, 1);
        }
        else
        {
            QLabel *lb2 = new QLabel(desc);
            lb2->setStyleSheet(style);
            lay->addWidget(lb2, row, 1);
        }

        // Add the weight
        add_weight_label(lay, o_ptr, row, 2);

        if (!home && store_will_buy(store_idx, o_ptr))
        {
            s32b price = price_item(store_idx, o_ptr, true);
            QString price_text = number_to_formatted_string(price);
            price_text = price_text.rightJustified(16, ' ');
            QLabel *price_out = new QLabel("Price");
            price_out->setText(price_text);
            price_out->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
            lay->addWidget(price_out, row, 3);
        }

        add_help_label(lay, id, row, 4);

        ++row;
    }
    QSpacerItem *spacer = new QSpacerItem(1, 1, QSizePolicy::Expanding, QSizePolicy::Expanding);
    lay->addItem(spacer, row++, 0);
}

void StoreDialog::reset_equip()
{
    QWidget *tab = equip_tab;
    QGridLayout *lay = dynamic_cast<QGridLayout *>(tab->layout());
    if (lay == 0) {
        lay = new QGridLayout;
        lay->setColumnStretch(1, 1);
        tab->setLayout(lay);
    }
    // Remove previous items
    clear_layout(lay);
    int n = 0;
    int i;
    int row = 0;
    lay->addWidget(new QLabel("Weight"), row, 2);
    if (!home)
    {
        QLabel *price_label = new QLabel("Price");
        price_label->setText("price");
        price_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
        lay->addWidget(price_label, row, 3);
    }

    row++ ;

    for (i = INVEN_WIELD; i < QUIVER_END; i++)
    {
        object_type *o_ptr = inventory + i;
        if (o_ptr->k_idx == 0) continue;

        o_ptr->has_been_seen();

        QString use;
        if (i < QUIVER_START)
        {
            use = QString("%1: ").arg(mention_use(i));
        }

        // Make an id for the item
        QString id = QString("e%1").arg(i);

        QLabel *lb = new QLabel(QString("%1)").arg(number_to_letter(n++).toUpper()));
        lb->setProperty("item_id", QVariant(id));
        lay->addWidget(lb, row, 0);

        QString desc = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);
        desc.prepend(use);
        QString s = QString("color: %1;").arg(get_object_color(o_ptr).name());
        QString style = "text-align: left; font-weight: bold;";
        style += s;

        if (home || store_will_buy(store_idx, o_ptr))
        {
            QPushButton *btn = new QPushButton(desc);
            btn->setProperty("item_id", QVariant(id));

            btn->setStyleSheet(style);
            btn->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
            connect(btn, SIGNAL(clicked()), this, SLOT(item_click()));
            lay->addWidget(btn, row, 1);
        }
        else
        {
            QLabel *lb2 = new QLabel(desc);
            lb2->setStyleSheet(style);
            lay->addWidget(lb2, row, 1);
        }

        // Add the weight
        add_weight_label(lay, o_ptr, row, 2);

        if (!home && store_will_buy(store_idx, o_ptr))
        {
            s32b price = price_item(store_idx, o_ptr, true);
            QString price_text = number_to_formatted_string(price);
            price_text = price_text.rightJustified(16, ' ');
            QLabel *price_out = new QLabel("Price");
            price_out->setText(price_text);
            price_out->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
            lay->addWidget(price_out, row, 3);
        }

        add_help_label(lay, id, row, 4);

        row++;
    }
    QSpacerItem *spacer = new QSpacerItem(1, 1, QSizePolicy::Expanding, QSizePolicy::Expanding);
    lay->addItem(spacer, row++, 0);
}

void StoreDialog::toggle_inven()
{
    char_tabs->setCurrentIndex((char_tabs->currentIndex() + 1) % char_tabs->count());
}

void StoreDialog::keyPressEvent(QKeyEvent *event)
{
    switch (event->key())
    {
        case Qt::Key_F2:
        {
            this->buy_sell_click();
            break;
        }
        case Qt::Key_F4:
        {
            this->toggle_inven();
            break;
        }
        case Qt::Key_F6:
        {
            this->wield_click();
            break;
        }
        case Qt::Key_F7:
        {
            this->takeoff_click();
            break;
        }
        default:
        {
            if (event->text().length() > 0
                    && (event->text().at(0).isLetterOrNumber()))
            {
                QString letter = event->text().at(0);

                letter.append(")");
                QWidget *container = char_tabs->currentWidget();
                if (letter.at(0).isLower() || letter.at(0).isDigit())
                {
                    container = store_area;
                }
                QList<QLabel *> lst = container->findChildren<QLabel *>();
                for (int i = 0; i < lst.size(); i++)
                {
                    QString text = lst.at(i)->text();

                    if (text.startsWith(letter, Qt::CaseSensitive))
                    {

                        QString item_id = lst.at(i)->property("item_id").toString();

                        if (item_id.startsWith('p')) process_item(item_id);
                        else if (item_id.startsWith('s')) process_service(item_id);
                        else if (item_id.startsWith('q')) process_quest(item_id);
                        else if (item_id.startsWith('e')) process_item(item_id);
                        else if (item_id.startsWith('i')) process_item(item_id);
                        return;
                    }
                }
            return;
            }
        }
        NPPDialog::keyPressEvent(event);
    }
}

void StoreDialog::process_item(QString id)
{
    int aux_mode = mode;

    if (id.startsWith("e") || id.startsWith("i")) aux_mode = SMODE_SELL;
    else aux_mode = SMODE_BUY;

    set_mode(aux_mode);

    if (aux_mode == SMODE_SELL && id.startsWith("p")) return;
    if (aux_mode == SMODE_BUY && !id.startsWith("p")) return;

    object_type *o_ptr;
    int item = id.mid(1).toInt();  // Get item index

    if (id.startsWith("p"))
    {
        o_ptr = &(store[store_idx].stock[item]);
    }
    else
    {
        o_ptr = inventory + item;
    }

    int price = price_item(store_idx, o_ptr, false);

    if (aux_mode == SMODE_BUY)
    {
        if (!home && !guild && price > p_ptr->au)
        {
            pop_up_message_box("It's too expensive", QMessageBox::Critical);
            return;
        }
        do_buy(o_ptr, item);
    }
    else if (aux_mode == SMODE_SELL)
    {
        if (!home && !store_will_buy(store_idx, o_ptr))
        {
            if (o_ptr->number == 1) pop_up_message_box("I don't buy that kind of item.", QMessageBox::Critical);
            pop_up_message_box("I don't buy those kind of items.", QMessageBox::Critical);
            return;
        }
        do_sell(o_ptr, item);
    }

    set_mode(SMODE_DEFAULT);

}

void StoreDialog::process_service(QString id)
{
    // Make sure we are buying a service.

    if (!id.startsWith("s")) return;

    // Get quest index
    int service = id.mid(1).toInt();

    set_mode(SMODE_BUY);

    u32b serv_price = price_services(service);

    p_ptr->message_append_start();

    if (!services_info[service].service_function(service, serv_price))
    {
        update_message_area(message_area, 3);
        return;
    }

    reset_all();
}

void StoreDialog::process_quest(QString id)
{
    if (!id.startsWith("q")) return;

    // Get quest index
    int quest_idx = id.mid(1).toInt();

    set_mode(SMODE_BUY);

    p_ptr->message_append_start();

    if (!guild_purchase(quest_idx))
    {
        update_message_area(message_area, 3);
        return;
    }

    reset_all();

}

void StoreDialog::item_click()
{
    QObject *obj = QObject::sender();

    QString id = obj->property("item_id").toString();
    process_item(id);
}

void StoreDialog::service_click()
{
    QObject *service = QObject::sender();

    QString id = service->property("item_id").toString();
    process_service(id);
}

void StoreDialog::quest_click()
{
    QObject *quest = QObject::sender();

    QString id = quest->property("item_id").toString();
    process_quest(id);
}


bool StoreDialog::do_buy(object_type *o_ptr, int item)
{
    p_ptr->message_append_start();

    int amt = request_amt(o_ptr, true);

    if (amt == 0) return false;

    cmd_arg args;
    args.item = item;
    args.number = amt;

    if (home)       do_cmd_retrieve(store_idx, args);
    else if (guild) do_cmd_reward(store_idx, args);
    else            do_cmd_buy(store_idx, args);

    reset_all();

    return true;
}

void StoreDialog::reset_all()
{
    p_ptr->message_append_stop();

    // First, check for pack overflow
    if (inventory[INVEN_MAX_PACK].k_idx)
    {
        if (store_overflow(store_idx))
        {
            this->reject();
        }
    }

    reset_store();
    reset_inventory();
    reset_equip();
    reset_quest_status();
    reset_gold();
    update_message_area(message_area, 3);
    update_header();

    ui_request_size_update(inven_tab);
    ui_request_size_update(equip_tab);
    ui_request_size_update(store_area);
    ui_request_size_update(quest_area);
    ui_request_size_update(message_area);
    QCoreApplication::processEvents();   // IMPORTANT: THE SIZE_HINT UPDATE IS ASYNC, SO WAIT FOR IT
    inven_tab->setMinimumSize(inven_tab->sizeHint());
    equip_tab->setMinimumSize(equip_tab->sizeHint());
    store_area->setMinimumSize(store_area->sizeHint());

    ui_request_size_update(central);
    QCoreApplication::processEvents();   // IMPORTANT: THE SIZE_HINT UPDATE IS ASYNC, SO WAIT FOR IT

    this->clientSizeUpdated();
}

bool StoreDialog::do_sell(object_type *o_ptr, int item)
{
    p_ptr->message_append_start();

    int amt = request_amt(o_ptr, false);

    if (amt == 0) return false;

    cmd_arg args;
    args.item = item;
    args.number = amt;

    if (home)   do_cmd_stash(store_idx, args);
    else        do_cmd_sell(store_idx, args);

    reset_all();

    return true;
}

void QuantityDialog::do_accept()
{
    amt = amt_spin->value();
    accept();
}

// Return the maximum value
void QuantityDialog::max_number_button(void)
{
    amt = amt_spin->maximum();
    this->accept();
}

// return the minimum value
void QuantityDialog::min_number_button(void)
{
    amt = amt_spin->minimum();
    this->accept();
}

QuantityDialog::QuantityDialog(object_type *op, bool buy)
{
    o_ptr = op;

    buying = buy;

    amt = 0;

    int store_idx = f_info[dungeon_info[p_ptr->py][p_ptr->px].feature_idx].f_power;

    price = price_item(store_idx, o_ptr, !buy);

    if (buying && (store_idx != STORE_HOME))
    {
        int money = p_ptr->au;
        max = money / MAX(price, 1);
        max = MIN(max, o_ptr->number);
    }
    else max = o_ptr->number;

    QVBoxLayout *lay1 = new QVBoxLayout;
    this->setLayout(lay1);

    QString verb = tr("sell");
    if (buying) verb = tr("buy");

    if (store_idx == STORE_HOME)
    {
        verb = tr("stash");
        if (buying) verb = tr("retrieve");
    }

    QString desc = object_desc(o_ptr, ODESC_FULL | ODESC_PREFIX);
    QString msg = tr("How many of the %2 do you want to %1?").arg(verb).arg(desc);
    question = new QLabel(msg);
    lay1->addWidget(question);

    amt_spin = new QSpinBox();
    lay1->addWidget(amt_spin);
    amt_spin->setMinimum(1);
    amt_spin->setMaximum(max);
    if (buying) amt_spin->setValue(1);
    else amt_spin->setValue(max);
    amt_spin->selectAll();

    total_label = new QLabel("");
    lay1->addWidget(total_label);
    update_totals(amt_spin->value());

    connect(amt_spin, SIGNAL(valueChanged(int)), this, SLOT(update_totals(int)));

    QHBoxLayout *lay2 = new QHBoxLayout;
    lay2->setContentsMargins(0, 0, 0, 0);
    lay1->addLayout(lay2);

    lay2->addStretch(1);

    // Add buttons for min value, max value, OK, and cancel
    QDialogButtonBox *buttons = new QDialogButtonBox();
    QPushButton *min_button = new QPushButton();
    min_button->setText(QString("Min - %1") .arg(amt_spin->minimum()));
    min_button->setToolTip("Use the minimum possible value");
    connect(min_button, SIGNAL(clicked()), this, SLOT(min_number_button()));
    buttons->addButton(min_button, QDialogButtonBox::ActionRole);
    QPushButton *max_button = new QPushButton();
    max_button->setText(QString("Max - %1") .arg(max));
    max_button->setToolTip("Use the maximum possible value");
    connect(max_button, SIGNAL(clicked()), this, SLOT(max_number_button()));
    buttons->addButton(max_button, QDialogButtonBox::ActionRole);
    buttons->addButton(QDialogButtonBox::Ok);
    buttons->addButton(QDialogButtonBox::Cancel);
    connect(buttons, SIGNAL(accepted()), this, SLOT(do_accept()));
    connect(buttons, SIGNAL(rejected()), this, SLOT(reject()));
    lay1->addWidget(buttons);
}

void QuantityDialog::update_totals(int value)
{
    int n = value;
    int money = p_ptr->au;
    int sign = 1;
    if (buying) sign = -1;
    QString msg = tr("Max. items: %4 - Total price: %1 - Gold: %2 - After: %3")
            .arg(n * price).arg(money).arg(money + n * price * sign).arg(max);
    total_label->setText(msg);
}

int StoreDialog::request_amt(object_type *o_ptr, bool buying)
{
    if (guild)
    {
        return o_ptr->number;
    }

    int amt = o_ptr->number;
    int price = price_item(store_idx, o_ptr, !buying);
    QString desc = object_desc(o_ptr, ODESC_FULL | ODESC_PREFIX);
    QString verb = tr("sell");
    if (buying) verb = tr("buy");

    if (home)
    {
        verb = tr("stash");
        if (buying) verb = tr("retrieve");
    }

    if (amt == 1)
    {
        QString msg = tr("Do you want to %1 %2?") .arg(verb).arg(desc);

        if (!home) msg.append(tr(" Price: %1") .arg(price));
        if (!get_check(msg)) return 0;
        return 1;
    }

    QuantityDialog *dlg = new QuantityDialog(o_ptr, buying);
    dlg->exec();
    amt = dlg->amt;
    delete dlg;

    return amt;
}

bool StatDialog::init_stats_table(int service)
{
    int i;
    bool good_stat = FALSE;

    // Clear the table
    for (i = 0; i < A_MAX; i++) stats[i] = FALSE;

    /* Count up the stats that require the service */
    for (i = 0; i < A_MAX; i++)
    {
        /* Add the stat that need to be restored */
        if ((service == SERVICE_RESTORE_STAT) &&
            (p_ptr->stat_base_cur[i] == p_ptr->stat_base_max[i])) continue;

        if ((service == SERVICE_INCREASE_STAT) &&
            (p_ptr->stat_base_max[i] == 18+100))  continue;

        if ((service == SERVICE_QUEST_REWARD_INC_STAT) &&
            (p_ptr->stat_quest_add[i] >= 3)) continue;

        //this stat is a valid choice
        stats[i] = TRUE;
        good_stat = TRUE;
    }

    return (good_stat);
}

// The first character is the number of the stat collected.
void StatDialog::select_str(void){selected_stat = 0; this->accept();}
void StatDialog::select_int(void){selected_stat = 1; this->accept();}
void StatDialog::select_wis(void){selected_stat = 2; this->accept();}
void StatDialog::select_dex(void){selected_stat = 3; this->accept();}
void StatDialog::select_con(void){selected_stat = 4; this->accept();}
void StatDialog::select_chr(void){selected_stat = 5; this->accept();}

/*
 * Stat selected of A_MAX means no eligible stat.
 * A_Max+1 is cancel.
 */
StatDialog::StatDialog(int service, byte *stat_selected)
{
    if (!init_stats_table(service))
    {
        *stat_selected = A_MAX;
        return;
    }

    QString prompt;

    if (service == SERVICE_RESTORE_STAT)
    {
        prompt = QString("<b><big>Please select a stat to restore.</big></b><br>");
    }
    else if (service == SERVICE_INCREASE_STAT)
    {
        prompt = QString("<b><big>Please select a stat to increase.</big></b><br>");
    }
    /* must be SERVICE_QUEST_REWARD_INC_STAT*/
    else
    {
        prompt = QString("<b><big>Please select a stat to permanently increase.</big></b><br>");
    }

    main_prompt = new QLabel(prompt);
    main_prompt->setAlignment(Qt::AlignCenter);

    QVBoxLayout *vlay = new QVBoxLayout;

    vlay->addWidget(main_prompt);

    // Add the stats
    QGridLayout *stat_layout = new QGridLayout;

    // add the headers
    byte row = 0;
    byte col = 0;
    QLabel *stat_header = new QLabel("Stat");
    QLabel *self_header = new QLabel("Self");
    QLabel *race_adj_header = new QLabel("Race Adj.");
    QLabel *class_adj_header = new QLabel("Class Adj");
    QLabel *equip_adj_header = new QLabel("Equip Adj");
    QLabel *reward_adj_header = new QLabel("Reward_Adj");
    QLabel *total_stat_header = new QLabel("Total Stat");
    stat_header->setAlignment(Qt::AlignLeft);
    self_header->setAlignment(Qt::AlignLeft);
    race_adj_header->setAlignment(Qt::AlignCenter);
    class_adj_header->setAlignment(Qt::AlignCenter);
    equip_adj_header->setAlignment(Qt::AlignCenter);
    reward_adj_header->setAlignment(Qt::AlignCenter);
    total_stat_header->setAlignment(Qt::AlignLeft);
    stat_layout->addWidget(stat_header, row, col++);
    stat_layout->addWidget(self_header, row, col++);
    if (birth_maximize) stat_layout->addWidget(race_adj_header, row, col++);
    if (birth_maximize) stat_layout->addWidget(class_adj_header, row, col++);
    stat_layout->addWidget(equip_adj_header, row, col++);
    if (!birth_no_quests) stat_layout->addWidget(reward_adj_header, row, col++);
    stat_layout->addWidget(total_stat_header, row, col++);

    for (int i = 0; i < A_MAX; i++)
    {
        col = 0;
        row++;

        // Do a button if we can select this stat
        if (stats[i])
        {
            QPushButton *stat_name_button = new QPushButton();
            stat_name_button->setText(stat_names[i]);
            stat_layout->addWidget(stat_name_button, row, col++);
            if (i == 0)connect(stat_name_button, SIGNAL(clicked()), this, SLOT(select_str()));
            else if (i == 1)connect(stat_name_button, SIGNAL(clicked()), this, SLOT(select_int()));
            else if (i == 2)connect(stat_name_button, SIGNAL(clicked()), this, SLOT(select_wis()));
            else if (i == 3)connect(stat_name_button, SIGNAL(clicked()), this, SLOT(select_dex()));
            else if (i == 4)connect(stat_name_button, SIGNAL(clicked()), this, SLOT(select_con()));
            else if (i == 5)connect(stat_name_button, SIGNAL(clicked()), this, SLOT(select_chr()));
        }
        // or make a label
        else
        {
            QLabel *self_label = new QLabel(stat_names[i]);
            stat_layout->addWidget(self_label, row, col++);
        }

        QLabel *stat_player = new QLabel(cnv_stat(p_ptr->stat_base_max[i]));
        stat_player->setAlignment(Qt::AlignCenter);
        stat_layout->addWidget(stat_player, row, col++);

        if (birth_maximize)
        {
            QLabel *race_adj = new QLabel(QString("%1") .arg(rp_ptr->r_adj[i]));
            race_adj->setAlignment(Qt::AlignCenter);
            stat_layout->addWidget(race_adj, row, col++);

            QLabel *class_adj = new QLabel(QString("%1") .arg(cp_ptr->c_adj[i]));
            class_adj->setAlignment(Qt::AlignCenter);
            stat_layout->addWidget(class_adj, row, col++);
        }

        QLabel *equip_adj = new QLabel(QString("%1") .arg(p_ptr->state.stat_equip[i]));
        equip_adj->setAlignment(Qt::AlignCenter);
        stat_layout->addWidget(equip_adj, row, col++);

        if (!birth_no_quests)
        {
            QLabel *quest_adj = new QLabel(QString("%1") .arg(p_ptr->stat_quest_add[i]));
            quest_adj->setAlignment(Qt::AlignCenter);
            stat_layout->addWidget(quest_adj, row, col++);
        }

        QLabel *stat_total = new QLabel(cnv_stat(p_ptr->state.stat_loaded_max[i]));
        stat_total->setAlignment(Qt::AlignLeft);
        stat_layout->addWidget(stat_total, row, col++);

        //Display reduced stat if necessary
        if (p_ptr->state.stat_loaded_cur[i] < p_ptr->state.stat_loaded_max[i])
        {
            QString lower_stat = cnv_stat(p_ptr->state.stat_loaded_cur[i]);
            lower_stat = color_string(lower_stat, TERM_PINK);
            QLabel *stat_reduce = new QLabel(lower_stat);
            stat_reduce->setAlignment(Qt::AlignLeft);
            stat_layout->addWidget(stat_reduce, row, col++);
        }
    }

    buttons = new QDialogButtonBox(QDialogButtonBox::Cancel);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));

    vlay->addLayout(stat_layout);
    vlay->addWidget(buttons);
    setLayout(vlay);
    setWindowTitle(tr("Stat Selection Menu"));

    if (!this->exec())
    {
        *stat_selected = (A_MAX + 1);
    }
    else
    {
        *stat_selected = selected_stat;
    }
}

int launch_stat_dialog(int choice)
{
    byte selection;

    StatDialog(choice, &selection);
    return selection;
}
