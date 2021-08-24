/*
 * File: sched-win.h
 * Purpose: Windows port of sched.c
 */

#ifndef SCHED_WIN_H
#define SCHED_WIN_H

extern void install_timer_tick(void (*func)(void), int freq);
extern void install_input(void (*func)(int, int), int fd, int arg);
extern void remove_input(int fd);
extern void sched(void);
extern void free_input(void);
extern void remove_timer_tick(void);

#endif /* SCHED_WIN_H */
