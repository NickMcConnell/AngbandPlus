/* world_view.cc
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

#include <stdio.h>
#include "simgraph.h" 
#include "world_view.h" 
#include "world_adaptor.h" 

//#include "walls4.h"
#include "walls9.h"

#include "hackdef.h"


#undef MIN
#undef MAX

extern "C" {
#include "../angband.h"


/* from isov-x11.c */
extern unsigned char **iso_ap;
extern unsigned char **iso_cp;
extern unsigned char **iso_atp;
extern unsigned char **iso_ctp;

}

#undef bool


#ifdef USE_SMALL_ISO_HACK

static bool is_valid_position(int x, int y) {
    return x >= 0 && y >= 0 && x<cur_wid && y<cur_hgt;
}                             

static bool is_valid_to_show(int x, int y) {
    return is_valid_position(x, y);
}

static int get_feat_nc(int x, int y)
{
    /* this is for Angabdn 2.9.1
    return cave_feat[y][x];     
    */

    /* Pernangband 4.1.5 */
    cave_type *c_ptr;

    /* Get the cave */
    c_ptr = &cave[y][x];


    /* Feature code */
    return c_ptr->feat;
}


static int get_feat_wc(int x, int y)
{
    if(is_valid_position(x,y)) {
	return get_feat_nc(x,y);
    } else {
	return FEAT_PERM_SOLID;
    }
}


static int get_info_nc(int x, int y)
{
    /* this is for Angabdn 2.9.1
    return cave_info[y][x];     
    */

    /* Pernangband 4.1.5 */
    cave_type *c_ptr;

    /* Get the cave */
    c_ptr = &cave[y][x];


    /* Info code */
    return c_ptr->info;
}


static int get_info_wc(int x, int y)
{
    if(is_valid_position(x,y)) {
	return get_info_nc(x,y);
    } else {
	return 0;
    }
}



static bool is_door(int feat)
{
    return 
    (feat == FEAT_OPEN) ||
    (feat == FEAT_BROKEN) ||
    (feat == FEAT_SECRET) ||
    (feat >= FEAT_DOOR_HEAD &&  feat <= FEAT_DOOR_TAIL);
}

static bool is_wall(int feat)
{
    return 
    (feat >= FEAT_MAGMA &&  feat <= FEAT_PERM_SOLID) || 
    (feat >= FEAT_TREES &&  feat <= FEAT_SANDWALL_K); 
}

static int is_wall_or_door(int x, int y)
{           
    const int feat = get_feat_wc(x, y);

    return is_door(feat) || is_wall(feat);
}

static bool is_lit(int x, int y)
{
    const int info = get_info_wc(x, y);

    return (info & CAVE_GLOW) && (info & CAVE_VIEW) && (info & CAVE_MARK);
}

static bool is_torch_lit(int x, int y)
{
    const int info = get_info_wc(x, y);

    return (info & CAVE_LITE) && (info & CAVE_VIEW) && (info & CAVE_MARK);
}

static bool is_blind()
{
    return p_ptr->blind;
}


#else


static int iso_access(unsigned char **field, int x, int y)
{
    if(x >= 0 && y >= 0 && x<80 && y<24) {
	return field[y][x] & 0x7f;
    } else {
	return 0;
    }                       
}

static int is_wall_or_door(int x, int y)
{           
    const int atp = iso_access(iso_atp, x, y);
    const int ctp = iso_access(iso_ctp, x, y);

    return (atp == 0x01 && (ctp >= 0x70 && ctp <= 0x75)) ||
	   (atp == 0x00  && (ctp =='+' || ctp == '\'')) ;
}

#endif


world_view_t::world_view_t(world_adaptor_t * welt) : karte_ansicht_t(welt)
{
    this->welt = welt;
}


int world_view_t::gib_anzeige_breite()
{
    return display_get_width();
}

int world_view_t::gib_anzeige_hoehe()
{
    return display_get_height();
}


/**
 * Grund von i,j an Bildschirmkoordinate xpos,ypos zeichnen.
 * @author Hj. Malthaner
 */
void world_view_t::display_boden(int x, int y, int xpos, int ypos, bool dirty)
{
    // unused in Iso-Angband
}


static int check_wall(int x, int y) 
{

    int o = 0;

    o |= (is_wall_or_door(x-1, y-1)) << 0;
    o |= (is_wall_or_door(x  , y-1)) << 1;
    o |= (is_wall_or_door(x+1, y-1)) << 2;
    o |= (is_wall_or_door(x-1, y  )) << 3;
    o |= (is_wall_or_door(x+1, y  )) << 4;
    o |= (is_wall_or_door(x-1, y+1)) << 5;
    o |= (is_wall_or_door(x  , y+1)) << 6;
    o |= (is_wall_or_door(x+1, y+1)) << 7;


    return wall_table[o];
}


static int door_direction(int x, int y)
{                 
    if(is_wall_or_door(x-1, y) && is_wall_or_door(x+1, y)) {
	return 1;
    } else {
	return 0;
    }
}

static int calc_nc(int c, int a)
{
    return ((a & 0x7F)  << 7) + (c & 0x7F);
}

/**
 * Dinge von i,j an Bildschirmkoordinate xpos,ypos zeichnen.
 * @author Hj. Malthaner
 */
void world_view_t::display_dinge(int x, int y, int xpos, int ypos, bool dirty)
{

#ifdef USE_SMALL_ISO_HACK
    if(is_valid_to_show(x,y)) {

	byte a, ta;
	char c, tc;

	map_info(y, x, &a, &c, &ta, &tc);

#else
    if(x >= 0 && y >= 1 && x < 80 && y < 23) {
	// floor, walls

	const int c = iso_ctp[y][x];
	const int a = iso_atp[y][x];

	// object/monster
	const int tc = iso_cp[y][x];
	const int ta = iso_ap[y][x];

#endif

	const int feat_nc = calc_nc(tc, ta);
	const int obj_nc = calc_nc(c, a);

	int shade = 1;

	if(is_lit(x, y)) {
	    shade = 2;
	} else if(is_torch_lit(x, y)) {
	    shade = 0;
	}

// check shading
//	printf("%d %d -> %d\n", x, y, shade);


	if(feat_nc >= 240 &&  feat_nc <= 245) {
// old shading
//	    const int shade = (feat_nc-240);
	    const int set_base = 240+shade*9;
	    const int bits = check_wall(x, y);


	    display_img(316+(shade%3), xpos, ypos, dirty);

	    if(bits) {
		for(int i=0; i<4; i++) {
		    if(bits & (1 << i)) {
			display_img(set_base+i, xpos, ypos, dirty);
		    }
		}                         

		display_img(set_base+8, xpos, ypos, dirty);

		for(int i=4; i<8; i++) {
		    if(bits & (1 << i)) {
			display_img(set_base+i, xpos, ypos, dirty);
		    }
		}                         

	    } else {
		// a pillar 
		display_img(set_base+8, xpos, ypos, dirty);
	    }

	    // fields with walls never contain anything else
	    // so we can quit now
	    return;

	} else if(feat_nc == 0x27) {
	    // open doors   
	    display_img(138+door_direction(x,y), xpos, ypos, dirty);
	    

	} else if(feat_nc == 0x2B) {
	    // closed doors
	    display_img(136+door_direction(x,y), xpos, ypos, dirty);

	} else if(feat_nc == 0x8C ||	    // a between gate
	          feat_nc == 0x16E ||	    // brick roof
	          feat_nc == 0x16F ||	    // brick roof top
	          feat_nc == 0x176 ||	    // grass roof
	          feat_nc == 0x177 ||	    // grass roof chimney
	          feat_nc == 0x17E ||	    // grass roof top
	          feat_nc == 0x3E ||	    // down stairs
	          feat_nc == 0x3C	    // up stairs
                  ) {
            // this features should not be shaded
	    

	    display_img(feat_nc, xpos, ypos, dirty);

	} else {             
            // this features should be shaded

	    // floor
	    if(feat_nc == 32) {
		// empty/unknown grid
		// -> no floor shading
		display_img(32, xpos, ypos, dirty);
	    } else {          
		// known grids get shaded floors
		display_img(feat_nc+shade, xpos, ypos, dirty);
	    }
	}

//	printf("%d a=%x c=%x (%c)\n", nc, a, c, c);


        if( obj_nc != feat_nc &&
//           (obj_nc < 276 || obj_nc > 278) &&
            obj_nc != 0x27 &&
	    obj_nc != 0x2B ) {	    
	    display_img(obj_nc, xpos, ypos, dirty);
	}


    } else {

	// outside of dungeon
	display_img(32, xpos, ypos, dirty);
    }
           
    // extra image to show here ?
    if(xtra_image >= 0 && xtra_ipos == x && xtra_jpos == y) {
	display_img(xtra_image, xpos, ypos, dirty);
                        
	// nothing to display next time
	xtra_image = -1;
    }
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
void world_view_t::display_image_ij(int i, int j, int c, int a)
{   
    const int nc = calc_nc(c, a);

/*
    const int i_off = welt->gib_ij_off().x;
    const int j_off = welt->gib_ij_off().y;

    const int x = (i - i_off) - (j - j_off); 
    const int y = (i - i_off) + (j - j_off); 

    const int IMG_SIZE = display_get_tile_size();

    const int ypos = y*IMG_SIZE/4+16 + welt->gib_y_off();
    const int xpos = x*IMG_SIZE/2 + gib_anzeige_breite()/2 + welt->gib_x_off();


    printf("world_view_t::display_image_ij() image=%x xpos=%d ypos=%d\n",
           nc, xpos, ypos); 


    display_img(nc, xpos, ypos, true);
*/


//    printf("world_view_t::display_image_ij() image=%x i=%d j=%d\n",
//           nc, i, j); 


    xtra_ipos = i;
    xtra_jpos = j;

    xtra_image = nc;
}
