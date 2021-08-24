/*
 * File: obj-inscrip.h
 * Purpose: Object inscription code
 */

#ifndef OBJECT_INSCRIP_H
#define OBJECT_INSCRIP_H

extern bool check_prevent_inscription(struct player *p, int what);
extern bool object_prevent_inscription(struct player *p, const struct object *obj, int what,
    bool is_harmless);
extern bool protected_p(struct player *p, const struct object *obj, int what, bool is_harmless);
extern bool object_match_inscription(struct player *p, const struct object *obj, int what);

#endif /* OBJECT_INSCRIP_H */
