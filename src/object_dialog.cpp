/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/npp.h>
#include <src/cmds.h>
#include <src/object_dialog.h>
#include <src/player_command.h>
#include <src/object_select.h>
#include <src/object_settings.h>
#include <QPushButton>


bool should_add_takeoff(object_type *o_ptr, s16b item_slot)
{
    if (!item_is_available(item_slot, NULL, USE_EQUIP | USE_QUIVER)) return (FALSE);
    if (o_ptr->is_known_cursed()) return (FALSE);
    return (TRUE);
}

bool should_add_wield(object_type *o_ptr, s16b item_slot)
{

    if (!item_is_available(item_slot, NULL, USE_INVEN | USE_FLOOR)) return (FALSE);
    if (!obj_can_wear(o_ptr)) return (FALSE);
    if (o_ptr->is_quest_object()) return (FALSE);

    return (TRUE);
}

bool should_add_swap(object_type *o_ptr, s16b item_slot)
{
    if (!birth_swap_weapons) return (FALSE);
    if (!obj_can_wear(o_ptr)) return (FALSE);

    if (item_slot >= INVEN_WIELD)
    {
        if(o_ptr->is_cursed()) return FALSE;
        if (game_mode == GAME_NPPANGBAND) return (FALSE);
        //GAME_NPPMORIA
        if ((item_slot == INVEN_MAIN_WEAPON) || (item_slot == INVEN_SWAP_WEAPON)) return TRUE;
        return (FALSE);
    }
    if (!o_ptr->use_verify[AUTO_SWAP]) return FALSE;
    return TRUE;
}

bool should_add_use(object_type *o_ptr, s16b item_slot)
{
    (void)item_slot;

    if (!o_ptr->is_usable_item()) return (FALSE);

    // Activation is handled elsewhere.
    else if (obj_can_activate(o_ptr)) return (FALSE);

    // Make sure the wands/rods/staffs can be used.
    if (o_ptr->is_wand() || o_ptr->is_staff())
    {
        if (!o_ptr->could_have_charges()) return (FALSE);
    }
    if (o_ptr->is_rod())
    {
        if (!o_ptr->could_be_zapped())return (FALSE);
    }
    return (TRUE);
}

bool should_add_fire(object_type *o_ptr, s16b item_slot)
{
    // Only add add
    if (ammo_can_fire(o_ptr, item_slot))
    {
        if (!o_ptr->is_known_cursed()) return (TRUE);
    }

    return (FALSE);
}

bool should_add_refill(object_type *o_ptr, s16b item_slot)
{
    if (!item_is_available(item_slot, NULL, USE_INVEN | USE_FLOOR)) return (FALSE);
    return (obj_can_refill(o_ptr));
}

bool should_add_fire_near(object_type *o_ptr, s16b item_slot)
{
    if (!ammo_can_fire(o_ptr, item_slot)) return (FALSE);

    return (monster_target_exists());
}

bool should_add_drop(object_type *o_ptr, s16b item_slot)
{
    // On the floor
    if (!item_is_available(item_slot, NULL, USE_INVEN | USE_EQUIP | USE_QUIVER)) return (FALSE);

    // In the backpack
    if ((item_slot < INVEN_WIELD) && (item_slot >=0)) return (TRUE);

    if (IS_QUIVER_SLOT(item_slot) && p_ptr->state.cursed_quiver) return (FALSE);

    // OK if not known cursed
    return (!o_ptr->is_known_cursed());

}

bool should_add_pickup(object_type *o_ptr, s16b item_slot)
{
    (void)o_ptr;
    // Not the floor
    if (item_slot >= 0) return (FALSE);
    if (pack_is_full())
    {
        if (!inven_stack_okay(o_ptr, INVEN_MAX_PACK)) return (FALSE);
    }
    return (TRUE);
}

bool should_add_browse(object_type *o_ptr, s16b item_slot)
{
    (void)item_slot;
    if (!o_ptr->is_spellbook()) return (FALSE);
    if (o_ptr->tval != cp_ptr->spell_book) return (FALSE);
    if (p_ptr->timed[TMD_BLIND] || no_light()) return (FALSE);
    if (p_ptr->timed[TMD_CONFUSED]) return (FALSE);
    return (TRUE);
}

bool should_add_study(object_type *o_ptr, s16b item_slot)
{
    if (!should_add_browse(o_ptr, item_slot)) return (FALSE);
    return (player_can_use_book(o_ptr, FALSE));
}

bool should_add_cast(object_type *o_ptr, s16b item_slot)
{
    if (!should_add_browse(o_ptr, item_slot)) return (FALSE);
    return (player_can_use_book(o_ptr, TRUE));
}

bool should_add_destroy(object_type *o_ptr, s16b item_slot)
{
    if (!item_is_available(item_slot, NULL, USE_FLOOR | USE_INVEN)) return (FALSE);
    return (!o_ptr->is_known_artifact());
}

bool should_add_uninscribe(object_type *o_ptr, s16b item_slot)
{
    (void)item_slot;
    return (o_ptr->has_inscription());
}

bool should_add_activate(object_type *o_ptr, s16b item_slot)
{
    if (!obj_can_activate(o_ptr)) return (FALSE);
    if (birth_swap_weapons && item_slot == INVEN_SWAP_WEAPON) return (FALSE);
    if (item_slot < INVEN_WIELD) return (FALSE);
    if (item_slot > INVEN_TOTAL) return (FALSE);
    return (TRUE);
}

bool should_add_throw(object_type *o_ptr, s16b item_slot)
{
    // On the floor
    if (!item_is_available(item_slot, NULL, USE_FLOOR | USE_INVEN | USE_QUIVER)) return (FALSE);
    if (IS_QUIVER_SLOT(item_slot) && p_ptr->state.cursed_quiver && !o_ptr->is_cursed())
    {
        return FALSE;
    }
    return (TRUE);
}

void add_settings(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_SETTINGS) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/settings.png"));
    new_button->setObjectName(id);
    new_button->setToolTip("Object Settings");
    lay->addWidget(new_button, row, col);
}

void add_examine(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_EXAMINE) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/look.png"));
    new_button->setObjectName(id);
    new_button->setToolTip("Examine");
    lay->addWidget(new_button, row, col);
}

void add_takeoff(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_TAKEOFF) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/takeoff.png"));
    new_button->setObjectName(id);
    new_button->setToolTip("Take Off");
    lay->addWidget(new_button, row, col);
}


void add_wield(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_WIELD) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/wield.png"));
    new_button->setToolTip("Wield");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_use(QGridLayout *lay, s16b item_slot, int row, int col)
{
    object_type *o_ptr = object_from_item_idx(item_slot);

    QString id = (QString("%1_%2") .arg(CMD_ITEM_USE) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    if (o_ptr->tval == TV_SCROLL)
    {
        new_button->setIcon(QIcon(":/icons/lib/icons/scroll.png"));
        new_button->setToolTip("Read Scroll");
    }
    else if (o_ptr->tval == TV_POTION)
    {
        new_button->setIcon(QIcon(":/icons/lib/icons/potion.png"));
        new_button->setToolTip("Quaff Potion");
    }
    else if (o_ptr->tval == TV_WAND)
    {
        new_button->setIcon(QIcon(":/icons/lib/icons/wand.png"));
        new_button->setToolTip("Aim Wand");
    }
    else if (o_ptr->tval == TV_STAFF)
    {
        new_button->setIcon(QIcon(":/icons/lib/icons/staff.png"));
        new_button->setToolTip("Use Staff");
    }
    else if (o_ptr->tval == TV_ROD)
    {
        new_button->setIcon(QIcon(":/icons/lib/icons/rod.png"));
        new_button->setToolTip("Zap Rod");
    } //TV_FOOD
    else if (o_ptr->is_mushroom())
    {
        new_button->setIcon(QIcon(":/icons/lib/icons/mushroom.png"));
        new_button->setToolTip("Eat Mushroom");
    }
    else if (o_ptr->is_wine())
    {
        new_button->setIcon(QIcon(":/icons/lib/icons/wine.png"));
        new_button->setToolTip("Drink Wine");
    }
    else if (o_ptr->is_ale())
    {
        new_button->setIcon(QIcon(":/icons/lib/icons/ale.png"));
        new_button->setToolTip("Drink Ale");
    }
    else
    {
        new_button->setIcon(QIcon(":/icons/lib/icons/food.png"));
        new_button->setToolTip("Eat Food");
    }
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}


void add_swap(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_SWAP) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/swap.png"));
    new_button->setToolTip("Swap");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_refill(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_REFUEL) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/refill.png"));
    new_button->setToolTip("Refill");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_fire(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_FIRE) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/fire.png"));
    new_button->setToolTip("Fire");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_fire_near(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_FIRE_NEAR) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/fire_near.png"));
    new_button->setToolTip("Fire At Closest Target");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_drop(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_DROP) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/drop.png"));
    new_button->setToolTip("Drop");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_pickup(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_PICKUP) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/pickup.png"));
    new_button->setToolTip("Pick Up");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_browse(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_BROWSE) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/browse.png"));
    new_button->setToolTip("Browse");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_study(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_STUDY) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/study.png"));
    new_button->setToolTip("Study");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_cast(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_CAST) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/cast.png"));
    QString noun = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);
    QString verb = cast_spell(MODE_SPELL_VERB, cp_ptr->spell_book, 1, 0);
    new_button->setToolTip(QString("%1 a %2") .arg(verb) .arg(capitalize_first(noun)));
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_destroy(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_DESTROY) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/destroy.png"));
    new_button->setToolTip("Destroy");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_inscribe(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_INSCRIBE) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/inscribe.png"));
    new_button->setToolTip("Inscribe");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_uninscribe(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_UNINSCRIBE) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/uninscribe.png"));
    new_button->setToolTip("Uninscribe");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_activate(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_ACTIVATE) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/activate.png"));
    new_button->setToolTip("Activate");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void add_throw(QGridLayout *lay, s16b item_slot, int row, int col)
{
    QString id = (QString("%1_%2") .arg(CMD_THROW) .arg(item_slot));
    QPushButton *new_button = new QPushButton;
    new_button->setIcon(QIcon(":/icons/lib/icons/throw.png"));
    new_button->setToolTip("Throw");
    new_button->setObjectName(id);
    lay->addWidget(new_button, row, col);
}

void do_buttons(QGridLayout *lay, object_type *o_ptr, s16b item_slot, s16b row, s16b col)
{
    if (!p_ptr->is_dead) add_settings(lay, item_slot, row, col++);
    add_examine(lay, item_slot, row, col++);
    if (!p_ptr->is_dead)
    {
        if (should_add_takeoff(o_ptr, item_slot)) add_takeoff (lay, item_slot, row, col++);
        if (should_add_wield(o_ptr, item_slot)) add_wield(lay, item_slot, row, col++);
        if (should_add_swap(o_ptr, item_slot))  add_swap(lay, item_slot, row, col++);
        if (should_add_use(o_ptr, item_slot))  add_use(lay, item_slot, row, col++);
        if (should_add_refill(o_ptr, item_slot))  add_refill(lay, item_slot, row, col++);
        if (should_add_fire(o_ptr, item_slot))  add_fire(lay, item_slot, row, col++);
        if (should_add_fire_near(o_ptr, item_slot))  add_fire_near(lay, item_slot, row, col++);
        if (should_add_drop(o_ptr, item_slot))  add_drop(lay, item_slot, row, col++);
        if (should_add_pickup(o_ptr, item_slot))  add_pickup(lay, item_slot, row, col++);
        if (should_add_browse(o_ptr, item_slot))  add_browse(lay, item_slot, row, col++);
        if (should_add_study(o_ptr, item_slot))  add_study(lay, item_slot, row, col++);
        if (should_add_cast(o_ptr, item_slot))  add_cast(lay, item_slot, row, col++);
        if (should_add_destroy(o_ptr, item_slot))  add_destroy(lay, item_slot, row, col++);
        add_inscribe(lay, item_slot, row, col++);
        if (should_add_uninscribe(o_ptr, item_slot))  add_uninscribe(lay, item_slot, row, col++);
        if (should_add_activate(o_ptr, item_slot))  add_activate(lay, item_slot, row, col++);
        if (should_add_throw(o_ptr, item_slot))  add_throw(lay, item_slot, row, col++);
    }
}





void add_plain_label(QGridLayout *lay, QString label, int row, int col)
{
    QLabel *lb = new QLabel(label);
    lb->setAlignment(Qt::AlignLeft);
    lay->addWidget(lb, row, col);
}


void add_letter_label(QGridLayout *lay, QChar location, int label_num, int row, int col)
{
    QString id = (QString("%1%2") .arg(location) .arg(label_num));

    QLabel *lb = new QLabel(QString("%1)").arg(number_to_letter(label_num)));
    lb->setProperty("item_id", QVariant(id));
    lay->addWidget(lb, row, col);
}

void add_object_label(QGridLayout *lay, object_type *o_ptr, QChar location, s16b item_slot, int row, int col)
{
    QString desc = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);
    QString style = QString("color: %1;").arg(get_object_color(o_ptr).name());
    style.append(QString("text-align: left; font-weight: bold;"));

    QString id = (QString("%1%2") .arg(location) .arg(item_slot));
    QLabel *object_label = new QLabel(desc);
    object_label->setStyleSheet(style);
    object_label->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
    lay->addWidget(object_label, row, col);
}

void add_weight_label(QGridLayout *lay, object_type *o_ptr, int row, int col)
{
    // Add the weight
    QString weight_printout = (formatted_weight_string(o_ptr->weight * o_ptr->number));
    weight_printout.append(" lbs");
    QLabel *weight = new QLabel(weight_printout);
    weight->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    lay->addWidget(weight, row, col);
}

/*
 *
 * Floor Layout
 *
 */
void update_floor_list(QGridLayout *lay, bool label, bool buttons)
{
    int row = 0;

    clear_layout(lay);

    s16b this_o_idx, next_o_idx;

    for (this_o_idx = dungeon_info[p_ptr->py][p_ptr->px].object_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        if (!o_ptr->k_idx) continue;

        int col = 0;

        if (label) add_letter_label(lay, QChar('f'), this_o_idx, row, col++);
        add_object_label(lay, o_ptr, QChar('f'), -this_o_idx, row, col++);
        add_weight_label(lay, o_ptr, row, col++);
        if (buttons) do_buttons(lay, o_ptr, -this_o_idx, row, col++);

        ++row;
    }
}

/*
 *
 * INVENTORY TAB
 *
 */
void update_inven_list(QGridLayout *lay, bool label, bool buttons)
{
    int row = 0;

    clear_layout(lay);

    for (int i = 0; i < INVEN_MAX_PACK; i++)
    {
        object_type *o_ptr = &inventory[i];
        if (!o_ptr->k_idx) continue;

        int col = 0;

        if (label) add_letter_label(lay, QChar('i'), i, row, col++);
        add_object_label(lay, o_ptr, QChar('i'), i, row, col++);
        add_weight_label(lay, o_ptr, row, col++);
        if (buttons) do_buttons(lay, o_ptr, i, row, col++);

        ++row;
    }
}

/*
 *
 * EQUIPMENT TAB
 *
 */
void update_equip_list(QGridLayout *lay, bool label, bool buttons)
{
    int row = 0;

    clear_layout(lay);

    for (int i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];

        // Make an id for the item
        QString id = QString("i%1").arg(i);

        int col = 0;

        QString equip_use = (QString("<b><big>%1:</big></b>") .arg(mention_use(i)));

        if (label) add_letter_label(lay, QChar('e'), i, row, col++);
        add_plain_label(lay, equip_use, row, col++);
        if (o_ptr->k_idx)
        {

            add_object_label(lay, o_ptr, QChar('e'), i, row, col++);
            add_weight_label(lay, o_ptr, row, col++);
            if (buttons) do_buttons(lay, o_ptr, i, row, col++);
        }
        else
        {
            QLabel *nothing_label = new QLabel("(nothing)");
            lay->addWidget(nothing_label, row, col++, Qt::AlignLeft);
        }

        ++row;
    }


}


void update_quiver_list(QGridLayout *lay, bool label, bool buttons)
{
    int row = 0;

    clear_layout(lay);

    for (int i = QUIVER_START; i < QUIVER_END; i++)
    {
        object_type *o_ptr = &inventory[i];

        if (!o_ptr->k_idx) continue;

        // Make an id for the item
        QString id = QString("i%1").arg(i);

        int col = 0;

        QString quiver_use = (QString("<b><big>%1:</big></b>") .arg(mention_use(i)));

        if (label) add_letter_label(lay, QChar('e'), i, row, col++);
        add_plain_label(lay, quiver_use, row, col++);

        add_object_label(lay, o_ptr, QChar('e'), i, row, col++);
        add_weight_label(lay, o_ptr, row, col++);
        if (buttons) do_buttons(lay, o_ptr, i, row, col++);

        ++row;
    }
}
