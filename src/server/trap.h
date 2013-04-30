/*
 * File: trap.h
 * Purpose: Trap triggering, selection, and placement
 */

#ifndef TRAP_H
#define TRAP_H

extern int trap_check_hit(struct player *p, int power);
extern void pick_trap(int depth, int y, int x);
extern void place_trap(struct cave *c, int y, int x);
extern void create_trap(struct cave *c, int y, int x);
extern void hit_trap(int Ind);

#endif /* TRAP_H */
