/*
 * Copyright (c) 2001 Hansjörg Malthaner
 * hansjoerg.malthaner@gmx.de
 *
 * This software may not be sold or used in other projects 
 * than Iso-Angband without written permission by the author
 *
 */

#ifndef hajo_world_view_h
#define hajo_world_view_h

#include "simview.h"

class world_adaptor_t;
struct pair;


class world_view_t : public karte_ansicht_t
{                  
private:
//    static unsigned int world_view_t::transform(int a, int c);
//    static void read_image_table(const char *filename, struct pair *image_tab);

    world_adaptor_t * welt;

protected:

    /**
     * Grund von i,j an Bildschirmkoordinate xpos,ypos zeichnen.
     * @author Hj. Malthaner
     */
    virtual void display_boden(int i, int j, int xpos, int ypos, bool dirty);


    /**
     * Dinge von i,j an Bildschirmkoordinate xpos,ypos zeichnen.
     * @author Hj. Malthaner
     */
    virtual void display_dinge(int i, int j, int xpos, int ypos, bool dirty);


    virtual int gib_anzeige_breite();
    virtual int gib_anzeige_hoehe();
                           
public:                                  

    world_view_t(world_adaptor_t * welt);
};

#endif