/*
 * File: ui-options.h
 * Purpose: Text UI options handling code (everything accessible from '=')
 */


#ifndef INCLUDED_UI_OPTIONS_H
#define INCLUDED_UI_OPTIONS_H

struct ego_desc
{
    s16b e_idx;
    u16b itype;
    const char *short_name;
};

extern int ego_item_name(char *buf, size_t buf_size, struct ego_desc *desc);
extern void do_cmd_options_birth(void);
extern void do_cmd_options(void);
extern void cleanup_options(void);
extern const char *ignore_name_for_type(ignore_type_t type);
extern const char *quality_name_for_value(byte value);
extern bool ignore_tval(int tval);

#endif /* INCLUDED_UI_OPTIONS_H */
