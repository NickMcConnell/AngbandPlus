/*
 * File: init.h
 * Purpose: Initialization
 */

#ifndef INCLUDED_INIT_H
#define INCLUDED_INIT_H

extern u16b extract_energy[200];

extern void init_file_paths(const char *configpath, const char *libpath, const char *datapath);
extern void create_needed_dirs(void);
extern void init_angband(void);
extern void cleanup_angband(void);
extern void load_server_cfg(void);
extern int get_energy(int speed);

#endif /* INCLUDED_INIT_H */
