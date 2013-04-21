/*
 * Copyright (c) 2001 Hansjörg Malthaner
 * hansjoerg.malthaner@gmx.de
 *
 * This software may not be sold or used in other projects 
 * than Iso-Angband without written permission by the author
 *
 */

/* world_adaptor.c
 *
 * adaption between angband world and simugraph data model
 * Hj. Maltahner, Jan. 2001
 */

#include "world_adaptor.h"
#include "world_view.h"
#include "simview.h"
#include "simgraph.h"

#undef MIN
#undef MAX

extern "C" {
#include "../angband.h"
}

#undef bool

world_adaptor_t::world_adaptor_t()
{
}

/**
 * Ermittelt ij-Koordinate des Blickpunkts auf der Karte.
 * @author Hj. Malthaner
 */
koord world_adaptor_t::gib_ij_off() const
{
    const int p_off = display_get_width() >> 7;

#ifdef USE_HAJO_HACK

    koord pos;

    if(p_ptr) {
	pos = koord(p_ptr->px-p_off, p_ptr->py-p_off);
    }

    return pos;     
#else

    return koord(47-p_off, 10-p_off);


#endif
}


/**
 * Ermittelt x-Offset gescrollter Karte
 * @author Hj. Malthaner
 */
int world_adaptor_t::gib_x_off() const
{
    return 0;
}


/**
 * Ermittelt y-Offset gescrollter Karte
 * @author Hj. Malthaner
 */
int world_adaptor_t::gib_y_off() const
{
    return 0;
}

                                     
/**
 * Holt den Grundwasserlevel der Karte
 *
 * @author Hj. Malthaner
 */
int world_adaptor_t::gib_grundwasser() const
{
    // angbands world has no water
    return -1000;
}



/**
 * Holt Zustand des Planquadrats von Position pos.
 * @param pos Position des abgefragten Planquadrats
 * @return true, wenn das Planquadrat neu gezeichnet werden muss
 * @author Hj. Malthaner
 */
bool world_adaptor_t::ist_dirty(koord pos) const
{
    // currently the world is always dirty
    return true;
}


/**
 * Setzt das dirty Flag eines Planquadrats zurück;
 * @author Hj. Malthaner
 */
void world_adaptor_t::markiere_clean(koord pos)
{          
    // does nothing (yet), currently the world is always dirty
    return;
}

static world_view_t *view = NULL;

extern "C" int init_adaptor()
{

    world_adaptor_t *welt = new world_adaptor_t();
    view = new world_view_t(welt);

    printf("Preparing display ...\n");
    simgraph_init(672, 480, false, false);

    return true;
}


extern "C" int refresh_display()
{
    view->display(true);
    display_flush_buffer();

    return true;
}


extern "C" int close_adaptor()
{
    simgraph_exit();

    return true;
}
