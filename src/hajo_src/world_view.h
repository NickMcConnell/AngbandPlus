/* world_view.h
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

#ifndef hajo_world_view_h
#define hajo_world_view_h


#ifdef __cplusplus

#include "simview.h"

class world_adaptor_t;

class world_view_t : public karte_ansicht_t
{                  
private:
    world_adaptor_t * welt;


    /* We need to track extra images for spell/breath effects
     * (throwing/shooring too). The following vars. buffer those images
     * properties. - Hajo -
     */
                      

    /** 
     * extra image number
     * @author Hj. Malthaner
     */
    int xtra_image;


    /** 
     * extra image map x position
     * @author Hj. Malthaner
     */
    int xtra_ipos;


    /** 
     * extra image map x position
     * @author Hj. Malthaner
     */
    int xtra_jpos;





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



    world_view_t(world_adaptor_t * welt);
};
#endif /* __cplusplus */

#endif