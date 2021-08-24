#ifndef OBJECT_DIALOG_H
#define OBJECT_DIALOG_H

#include <src/object_classes.h>
#include <QGridLayout>


extern void add_plain_label(QGridLayout *lay, QString label, int row, int col);
extern void add_letter_label(QGridLayout *lay, QChar location, int label_num, int row, int col);
extern void add_object_label(QGridLayout *lay, object_type *o_ptr, QChar location, s16b item_slot, int row, int col);
extern void add_weight_label(QGridLayout *lay, object_type *o_ptr, int row, int col);

// Functions to determine which buttons to add for each item.
extern bool should_add_takeoff(object_type *o_ptr, s16b item_slot);
extern bool should_add_wield(object_type *o_ptr, s16b item_slot);
extern bool should_add_swap(object_type *o_ptr, s16b item_slot);
extern bool should_add_use(object_type *o_ptr, s16b item_slot);
extern bool should_add_refill(object_type *o_ptr, s16b item_slot);
extern bool should_add_fire(object_type *o_ptr, s16b item_slot);
extern bool should_add_fire_near(object_type *o_ptr, s16b item_slot);
extern bool should_add_drop(object_type *o_ptr, s16b item_slot);
extern bool should_add_pickup(object_type *o_ptr, s16b item_slot);
extern bool should_add_browse(object_type *o_ptr, s16b item_slot);
extern bool should_add_study(object_type *o_ptr, s16b item_slot);
extern bool should_add_cast(object_type *o_ptr, s16b item_slot);
extern bool should_add_destroy(object_type *o_ptr, s16b item_slot);
extern bool should_add_uninscribe(object_type *o_ptr, s16b item_slot);
extern bool should_add_activate(object_type *o_ptr, s16b item_slot);
extern bool should_add_throw(object_type *o_ptr, s16b item_slot);


// Functions to add the buttons
extern void do_buttons(QGridLayout *lay, object_type *o_ptr, s16b item_slot, s16b row, s16b col);
extern void add_settings(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_examine(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_takeoff(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_wield(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_swap(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_use(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_refill(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_fire(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_fire_near(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_drop(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_pickup(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_browse(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_study(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_cast(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_destroy(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_inscribe(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_uninscribe(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_activate(QGridLayout *lay, s16b item_slot, int row, int col);
extern void add_throw(QGridLayout *lay, s16b item_slot, int row, int col);


extern void update_floor_list(QGridLayout *lay, bool label, bool buttons);
extern void update_inven_list(QGridLayout *lay, bool label, bool buttons);
extern void update_equip_list(QGridLayout *lay, bool label, bool buttons);
extern void update_quiver_list(QGridLayout *lay, bool label, bool buttons);





#endif // OBJECT_DIALOG_H
