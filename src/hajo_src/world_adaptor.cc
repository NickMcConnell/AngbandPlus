/* world_adaptor.cc
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

/* world_adaptor.c
 *
 * adaption between angband world and simugraph data model
 * Hj. Maltahner, Jan. 2001
 */

#include "hackdef.h"

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
    const int mult = display_get_tile_size() == 32 ? 2 : 1;

#ifdef USE_SMALL_ISO_HACK

    koord pos;
/* use this for angband 2.9.1
    if(p_ptr) {
	pos = koord(p_ptr->px-p_off, p_ptr->py-p_off);
    }
*/

    /* this is for Pernangband 4.1.5 */
    pos = koord(px-p_off*mult, py-p_off*mult);

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


/**
 * we need to display images at distinct positions. Images of things 
 * wich are not part
 * of the world. The Simugraph engine doesn't support reverse position
 * transformation, it assumes everything displayed is part of the world.
 *
 * This function is kind of a hack, it implements the inverse transformation.
 * It will break if the forward transformation is changed (simview.cc)
 *
 * @author Hj. Malthaner
 */
extern "C" void display_image_ij(int i, int j, int c, int a)
{
    view->display_image_ij(i, j, c, a);
}


