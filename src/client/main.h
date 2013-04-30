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

extern errr init_sound_sdl(void);
extern bool open_audio(void);

extern errr init_sdl(void);
extern errr init_gcu(void);

#endif /* INCLUDED_MAIN_H */
