/* simview.cc
 *
 * Copyright (c) 2001 Hansjörg Malthaner
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


#include "ifc/karte_modell.h"
#include "simview.h"
#include "simgraph.h"


karte_ansicht_t::karte_ansicht_t(karte_modell_t *welt)
{
    this->welt = welt;
}

void
karte_ansicht_t::display(bool dirty)
{
    // zuerst den boden zeichnen
    // denn der Boden kann kein Objekt verdecken 

//    puts("displaying");
                                  
    dirty = dirty || welt->ist_dirty();
    welt->setze_dirty_zurueck();


    const int IMG_SIZE = display_get_tile_size();


    const int const_x_off = gib_anzeige_breite()/2 + welt->gib_x_off();
    const int dpy_width = gib_anzeige_breite()/IMG_SIZE + 2;
    const int dpy_height = (gib_anzeige_hoehe()*4)/IMG_SIZE;


    const int i_off = welt->gib_ij_off().x;
    const int j_off = welt->gib_ij_off().y;

    for(int y=-5; y<dpy_height+10; y++) {
	const int ypos = y*IMG_SIZE/4+16 + welt->gib_y_off();
        
	for(int x=-dpy_width + (y & 1); x<=dpy_width+2; x+=2) {

	    const int i = ((y+x) >> 1) + i_off;
	    const int j = ((y-x) >> 1) + j_off;
	    const int xpos = x*IMG_SIZE/2 + const_x_off;

	    display_boden(i, j, xpos, ypos, dirty);
	}
    }

    // dann die Objekte Zeichnen

    for(int y=-5; y<dpy_height+10; y++) {
	const int ypos = y*IMG_SIZE/4+16 + welt->gib_y_off();
        
	for(int x=-dpy_width + (y & 1); x<=dpy_width+2; x+=2) {

	    const int i = ((y+x) >> 1) + i_off;
	    const int j = ((y-x) >> 1) + j_off;
	    const int xpos = x*IMG_SIZE/2 + const_x_off;


	    display_dinge(i, j, xpos, ypos, dirty);
	}
    }
}

