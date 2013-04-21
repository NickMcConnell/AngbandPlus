/* world_adaptor.h
 *
 * Copyright (c) 2001 Hansjörg Malthaner
 * hansjoerg.malthaner@gmx.de
 *
 * This file is part of the Simugraph<->Angband adaption code.
 *
 *
 * This file may be copied and modified freely so long as the above credits,
 * this paragraph, and the below disclaimer of warranty are retained; no
 * financial profit is derived from said modification or copying; and all
 * licensing rights to any modifications are granted to the original author,
 * Hansjörg Malthaner.
 *
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 * GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
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


/**
 * we need to display images at distinct positions. Images of
 * things which are not part
 * of the world. The Simugraph engine doesn't support reverse position
 * transformation, it assumes everything displayed is part of the world.
 *
 * This function is kind of a hack, it implements the inverse transformation.
 * It will break if the forward transformation is changed (simview.cc)
 *
 * @author Hj. Malthaner
 */

void display_image_ij(int i, int j, int c, int a);


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