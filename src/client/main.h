/*
 * File: main.h
 * Purpose: Core game initialisation
 */

#ifndef INCLUDED_MAIN_H
#define INCLUDED_MAIN_H

struct module
{
    const char *name;
    errr (*init)(void);
};

#ifdef USE_SDL
extern errr init_sdl(void);
#endif
#ifdef USE_GCU
extern errr init_gcu(void);
#endif

#endif /* INCLUDED_MAIN_H */
