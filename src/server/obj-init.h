/*
 * File: obj-init.h
 * Purpose: Object initialization routines
 */

#ifndef OBJECT_INIT_H
#define OBJECT_INIT_H

extern struct object_kind *unknown_item_kind;
extern struct object_kind *unknown_gold_kind;
extern struct object_kind *pile_kind;
extern struct object_kind *curse_object_kind;

extern struct file_parser projection_parser;
extern struct file_parser object_base_parser;
extern struct file_parser slay_parser;
extern struct file_parser brand_parser;
extern struct file_parser curse_parser;
extern struct file_parser act_parser;
extern struct file_parser object_parser;
extern struct file_parser ego_parser;
extern struct file_parser artifact_parser;
extern struct file_parser object_property_parser;
extern struct file_parser object_power_parser;

#endif /* OBJECT_INIT_H */
