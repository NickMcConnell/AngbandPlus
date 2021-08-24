#ifndef STOREDIALOG_H
#define STOREDIALOG_H

#include <qtabwidget.h>
#include <qdialogbuttonbox.h>
#include <QGridLayout>
#include "src/nppdialog.h"
#include "src/npp.h"
#include "src/utilities.h"
#include "store.h"
#include "src/messages.h"

enum
{
    SMODE_DEFAULT = 0,
    SMODE_BUY,
    SMODE_SELL
};



#define QUEST_REWARD_HEAD	SERVICE_QUEST_DEFER_REWARD
#define QUEST_REWARD_TAIL	SERVICE_QUEST_REWARD_AUGMENTATION

class service_info
{
public:

    byte service_store;
    u32b service_price;
    bool (*service_function)(byte choice, u32b price);
    QString service_names;
};

extern service_info services_info[STORE_SERVICE_MAX];



extern  QString quests_info[QUEST_SLOT_MAX];

class object_type;

class StoreDialog : public NPPDialog
{
    Q_OBJECT
private:
    void add_weight_label(QGridLayout *lay, object_type *o_ptr, int row, int col);
    void add_help_label(QGridLayout *lay, QString id, int row, int col);
    void update_header();

public:
    int store_idx;
    int mode;
    QWidget *central;
    QTabWidget *char_tabs;
    QWidget *inven_tab;
    QWidget *equip_tab;
    QLabel *message_area;
    QWidget *store_area;
    QWidget *quest_area;
    QLabel  *quest_status;
    QLabel  *quest_picture;

    QLabel *header_weight1;
    QLabel *header_weight2;


    bool home;
    bool guild;

    QLabel *mode_label;

    StoreDialog(int _store, QWidget *parent = 0);

    bool should_show_inventory(void);
    bool should_offer_quests(void);
    bool should_offer_service(byte service_num);
    s32b price_services(int service_idx);

    void reset_store();
    void reset_messages();
    void reset_inventory();
    void reset_equip();
    void reset_quest_status();
    void reset_all();

    void reset_gold();

    void set_mode(int _mode);

    virtual void keyPressEvent(QKeyEvent *event);

    bool do_buy(object_type *o_ptr, int item);
    bool do_sell(object_type *o_ptr, int item);

    int request_amt(object_type *o_ptr, bool buying);

    void process_item(QString item_id);
    void process_service(QString item_id);
    void process_quest(QString item_id);

public slots:
    void toggle_inven();
    void item_click();
    void service_click();
    void quest_click();
    void buy_sell_click();
    void wield_click();
    void takeoff_click();
    void help_click();
};

class QSpinBox;

class QuantityDialog: public QDialog
{
    Q_OBJECT
public:
    QLabel *question;
    QSpinBox *amt_spin;
    QLabel *total_label;
    object_type *o_ptr;
    bool buying;
    int amt;
    int max;
    int price;

    QuantityDialog(object_type *op, bool buy);

public slots:
    void update_totals(int value);
    void do_accept();

private slots:
    void max_number_button();
    void min_number_button();
};

class StatDialog : public QDialog
{
    Q_OBJECT
public:
    explicit StatDialog(int service, byte *stat_selected);

private:
    QLabel *main_prompt;
    QDialogButtonBox *buttons;

    byte selected_stat;

public slots:
    void select_str(void);
    void select_int(void);
    void select_wis(void);
    void select_dex(void);
    void select_con(void);
    void select_chr(void);

public:
     bool stats[A_MAX];
     bool init_stats_table(int service);
};

extern void launch_store(int store_idx);
extern int launch_stat_dialog(int choice);


#endif // STOREDIALOG_H
