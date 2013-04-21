/*
 * Copyright (c) 2001 Hansjörg Malthaner
 * hansjoerg.malthaner@gmx.de
 *
 * This software may not be sold or used in other projects 
 * than Iso-Angband without written permission by the author
 *
 */


#ifndef hajo_world_adaptor_h
#define hajo_world_adaptor_h

/* world_adapter.h
 *
 * adpater between angband code and simugraph engine
 * Hj. Malthaner, Jan 2001
 */


/* 
 * the next section are the C bindings, 
 * the methods which can be called from C code that is
 */

#ifdef __cplusplus
extern "C" { 
#endif

int init_adaptor();
int close_adaptor();

int refresh_display();

#ifdef __cplusplus
} 
#endif

/* 
 * the next section are the C++ declarations
 */

#ifdef __cplusplus

#include "ifc/karte_modell.h"

/**
 * this class mirrors the world of angband in a way that the 
 * simugraph engine can access and display the data
 * 
 * @author Hj. Malthaner
 */
class world_adaptor_t : public karte_modell_t
{
private:


public:

    world_adaptor_t();

    /**
     * Ermittelt x-Offset gescrollter Karte
     * @author Hj. Malthaner
     */
    virtual int gib_x_off() const;


    /**
     * Ermittelt y-Offset gescrollter Karte
     * @author Hj. Malthaner
     */
    virtual int gib_y_off() const;

                                     
    /**
     * Ermittelt ij-Koordinate des Blickpunkts auf der Karte.
     * @author Hj. Malthaner
     */
    koord gib_ij_off() const;


    /**
     * Holt den Grundwasserlevel der Karte
     *
     * @author Hj. Malthaner
     */
    virtual int gib_grundwasser() const;



    /**
     * Holt Zustand des Planquadrats von Position pos.
     * @param pos Position des abgefragten Planquadrats
     * @return true, wenn das Planquadrat neu gezeichnet werden muss
     * @author Hj. Malthaner
     */
    virtual bool ist_dirty(koord pos) const;


    /**
     * Setzt das dirty Flag eines Planquadrats zurück;
     * @author Hj. Malthaner
     */
    virtual void markiere_clean(koord pos);
};

#endif

#endif  