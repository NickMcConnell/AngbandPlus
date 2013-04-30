/*
 * File: pval.h
 * Purpose: Structures and functions for dealing with object pvals
 */

#ifndef INCLUDED_PVAL_H
#define INCLUDED_PVAL_H

/** Functions **/
extern bool object_add_pval(object_type *o_ptr, int pval, int flag);
extern bool object_this_pval_is_visible(const object_type *o_ptr, int pval, bool aware);
extern int which_pval(const object_type *o_ptr, const int flag);
extern void object_pval_flags(const object_type *o_ptr, bitflag flags[MAX_PVALS][OF_SIZE]);
extern void object_pval_flags_known(const object_type *o_ptr,
    bitflag flags[MAX_PVALS][OF_SIZE], bool aware);
extern bool object_dedup_pvals(object_type *o_ptr);

#endif /* INCLUDED_PVAL_H */
