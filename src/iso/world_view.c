/* world_view.c
 *
 * Copyright (c) 2001, 2002 Hansjörg Malthaner
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


#include "../angband.h"


/* from isov-x11.c */
extern unsigned char **iso_ap;
extern unsigned char **iso_cp;
extern unsigned char **iso_atp;
extern unsigned char **iso_ctp;

/* Hajo: Constant definitions */
const int OUTSIDE_TOWN = 319;
const int SHADOW = 153;
const int TOWN_FLOOR = 1;
const int TOWN_BUILDINGS = 135;

/** Which part of attr are the color bits? */
const int COLOR_BITS = 0x1F;


/* Hajo: keep code compatible with new and old Angband variants */
#ifndef CAVE_MARK
#define CAVE_MARK PLAY_MARK
#define CAVE_VIEW PLAY_VIEW
#define CAVE_SEEN PLAY_SEEN
#endif

#ifdef USE_SMALL_ISO_HACK

static bool is_valid_position(int x, int y) {

  /* this is for Pernangband 4.1.5 */
  /*
  return x >= 0 && y >= 0 && x<cur_wid && y<cur_hgt;
  */

  /* this is for Angband 2.9.1 */
  return x >= 0 && y >= 0 && x<DUNGEON_WID && y<DUNGEON_HGT;
}                             

static bool is_valid_to_show(int x, int y) {
  return is_valid_position(x, y);
}

static int get_feat_nc(int x, int y)
{
  /* this is for Angband 2.9.1 */
    
  return cave_feat[y][x];     

  
  /* Pernangband 4.1.5 */
  // cave_type *c_ptr;
  
  /* Get the cave */
  // c_ptr = &cave[y][x];
  
  
  /* Feature code */
  // return c_ptr->feat;
}


static int get_feat_wc(int x, int y)
{
  if(is_valid_position(x,y) && cave_feat) {
    return get_feat_nc(x,y);
  } else {
    return FEAT_PERM_SOLID;
  }
}


static int get_info_nc(int x, int y)
{
  /* this is for Angband 2.9.1, UnAngband 0.6.4 */
  return cave_info ? cave_info[y][x] : 0;     

  
  /* Pernangband 4.1.5 */
  // cave_type *c_ptr;
  
  /* Get the cave */
  // c_ptr = &cave[y][x];
  
  
  /* Info code */
  // return c_ptr->info;
}


static int get_info_wc(int x, int y)
{
  if(is_valid_position(x,y)) {
    return get_info_nc(x,y);
  } else {
    return 0;
  }
}

static int get_play_info_wc(int x, int y)
{
  if(is_valid_position(x,y)) {
    return play_info ? play_info[y][x] : 0;
  } else {
    return 0;
  }
}

static int is_player_in_town()
{
    /* this is for Angband 2.9.1 */
    return (p_ptr) && (p_ptr->depth == 0);	
	// return 0;
}

/* 
 * Hajo: check for area outside town 
 */
static int is_location_outside_town(int x, int y)
{
    /* Angband 2.9.3 */
    // const int qy = SCREEN_HGT;
    // const int qx = SCREEN_WID;

    // return x < qx || y < qy ||  x>= qx+ SCREEN_WID || y >= qy+SCREEN_HGT;
    
    /* UnAngband 0.6.4 */

    return x < 0 || y < 0 || x >= TOWN_WID || y >= TOWN_HGT;    
}

static int is_town_wall(const int x, const int y)
{
    /*
    const int qy = SCREEN_HGT;
    const int qx = SCREEN_WID;

    return 
	!is_location_outside_town(x, y) && 
	(x == qx || y == qy ||  x== qx+ SCREEN_WID-1 || y == qy+SCREEN_HGT-1);
    */
    
    return 
	!is_location_outside_town(x, y) && 
	(x == 0 || y == 0 ||  x== TOWN_WID-1 || y == TOWN_HGT-1);
    
}

static bool is_door(int feat)
{
  return 
    (feat == FEAT_OPEN) ||
    (feat == FEAT_BROKEN) ||
    (feat == FEAT_SECRET) ||
    // Old Angband code
    // (feat >= FEAT_DOOR_HEAD &&  feat <= FEAT_DOOR_TAIL);
    // New Angband code
    (feat >= FEAT_DOOR_TRAP_HEAD &&  feat <= FEAT_DOOR_TRAP_TAIL);
}


static bool is_wall(int feat)
{
  /* Pernangband 4.1.5 */
  /*
  return 
    (feat >= FEAT_MAGMA &&  feat <= FEAT_PERM_SOLID) || 
    (feat >= FEAT_TREES &&  feat <= FEAT_SANDWALL_K); 
  */

  /* this is for Angband 2.9.1 */
  return 
    (feat >= FEAT_MAGMA &&  feat <= FEAT_PERM_SOLID);

}


static int unangband_is_wall_or_door(int x, int y)
{
    int wall = 0;
    
    if(!in_bounds_fully(y, x)) 
    {
	/* Assume outside filled with walls */
        return 1;
    }
    
    if(f_info && cave_feat) 
    {
        int feat = cave_feat[y][x];
        feature_type *f_ptr = &f_info[feat];
        
        if((f_ptr->flags1 & (FF1_WALL | FF1_PERMANENT | FF1_INNER |
 	 	             FF1_OUTER | FF1_SOLID | FF1_DOOR)) != 0)
	// if((f_ptr->flags3 & (FF3_ATTR_WALL | FF3_ATTR_DOOR)) != 0)
        {
            wall |= 1;
        }
        
        /* Note we use the mimic'ed value, but always assume this is known */
        if ((f_info[f_info[cave_feat[y][x]].mimic].flags3 & (FF3_ATTR_WALL | FF3_ATTR_DOOR)) != 0)
        {
            wall |= 1;
        }

        /* Note we check the underneath value as well, but always assume this is known */
        else if ((f_info[f_info[f_info[cave_feat[y][x]].mimic].under].flags3 & (FF3_ATTR_WALL | FF3_ATTR_DOOR)) != 0)
        {
            wall |= 1;
        }
    }
    
    return wall;
}    


static int is_known_wall_or_door(int x, int y)
{
    if(is_player_in_town()) 
    {
        if(is_location_outside_town(x, y))
        {
            return 0;
        } else {            
            return is_town_wall(x, y) | unangband_is_wall_or_door(x, y);
        }
    }
    else
    {
        const int info = get_play_info_wc(x, y);

        if(info & (CAVE_MARK | CAVE_SEEN)) 
        {
	    /* Angbad 2.9.3 code */
            // const int feat = get_feat_wc(x, y);
            // return is_door(feat) || is_wall(feat);
		
	    /* Unangband 0.6.4 code */
            return unangband_is_wall_or_door(x, y);
        }
    }
    return 1;
}

static int is_wall_or_door(int x, int y)
{           
  /* Angband 2.9.3 code */
  // const int feat = get_feat_wc(x, y);
  // return is_door(feat) || is_wall(feat);
  
  /* Unangband 0.6.4 code */
  return unangband_is_wall_or_door(x, y);
}

static bool is_lit(int x, int y)
{
  const int info = get_info_wc(x, y);
  
  return (info & CAVE_GLOW) && (info & CAVE_VIEW) && (info & CAVE_MARK);
}


static bool is_torch_lit(int x, int y)
{
  const int info = get_info_wc(x, y);
  
  /* Pernangband 4.1.5 */
  // return (info & CAVE_PLIT) && (info & CAVE_VIEW) && (info & CAVE_MARK);

  /* this is for Angband 2.9.1 */
  return (info & CAVE_GLOW) && (info & CAVE_VIEW) && (info & CAVE_MARK);
}


static bool is_only_torch_lit(int x, int y)
{
  return is_torch_lit(x, y) & !is_lit(x, y);
}


static bool is_blind()
{
        /* Old Angband code */
	// return p_ptr->blind;
	
	/* New Angband code */
	return p_ptr->timed[TMD_BLIND];
}


/**
 * Calculates distance from player to point (x,y).
 * Uses the distance guess from cave.c
 * @author Hj. Malthaner
 */
static int get_distance_to_player(int x, int y)
{                                
  /* PernAngband variant */
  // return distance(y, x, py, px);

  /* this is for Angband 2.9.1 */
  return distance(y, x, p_ptr->py, p_ptr->px);
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


/**
 * Calculate a suitable image for display outside the
 * town walls.
 * @author Hj. Malthaner
 */
static int calc_town_outside(int x, int y)
{
  int image = OUTSIDE_TOWN;
  
  /* Hajo: randomize image by coordinate */
  /* image += (y*3 + x) & 3; */
	
  return image;
}


static int check_wall(int x, int y) 
{
  
    int o = 0;
  
    o |= (is_known_wall_or_door(x-1, y-1)) << 0;
    o |= (is_known_wall_or_door(x  , y-1)) << 1;
    o |= (is_known_wall_or_door(x+1, y-1)) << 2;
    o |= (is_known_wall_or_door(x-1, y  )) << 3;
    o |= (is_known_wall_or_door(x+1, y  )) << 4;
    o |= (is_known_wall_or_door(x-1, y+1)) << 5;
    o |= (is_known_wall_or_door(x  , y+1)) << 6;
    o |= (is_known_wall_or_door(x+1, y+1)) << 7;
  
    // return o;
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

/*
 * Calculate tile number for char (7 bits) and
 * attr (2 or 3 bits).
 */
static int calc_nc_text(const int c, const int a)
{
    /* Hajo: use this for 16 color version */
    // return ((a & 0x70) << 3) + (c & 0x7F);
    
    /* 32 color version */
    return ((a & 0x60) << 2) + (c & 0x7F);
}

static int get_stack_size(const int x, const int y)
{
	int n = 1;
	
	if(is_valid_to_show(x, y))
	{
		/* UnAngband version */
		const int this_m_idx = cave_m_idx[y][x];
		
		if(this_m_idx == 0) 
		{
			const int this_o_idx = cave_o_idx[y][x];

			if(this_o_idx)
			{
				/* Get the object */
				object_type *o_ptr = o_ptr = &o_list[this_o_idx];
				n = o_ptr->number;
			}
		}
	}
	
	return n;
}


static void display_object_stack_line(int xpos, int ypos,
									  int tileno, int color,
									  int count)
{
	const int vx = -3;
	const int vy = 2;
	const int hf = count/2;
	
	int i;
	
	for(i=0; i<count; i++)
	{
		const int n = i - hf;
		const int x = xpos + n*vx;
		const int y = ypos + n*vy;
		
		display_color_img(tileno, x, y, color, TRUE);
	}
}

/* 
 * Display item stacks in layers of 36 (6x6) tiles each 
 */
static void display_object_stack_grid(const int xpos, const int ypos,
									  const int tileno, const int color,
									  int count)
{
    const int layers = count / 36 + 1;
    int layer;
    
    for(layer=0; layer < layers; layer++)
    {
        int count_in_layer = count > 36 ? 36 : count;
    
	    const int vx = 4;
	    const int vy = 2;
	    const int lines = count_in_layer / 6 + 1;
	    const int hf = lines / 2;
	
	    int i;
	
	    for(i=0; i<lines; i++)
	    {
		    const int n = i - hf;
		    const int x = xpos + n*vx + layer;
		    const int y = ypos + n*vy - layer;
		
		    display_object_stack_line(x, y, tileno, color,
                                      count_in_layer > 6 ? 6 : count_in_layer);		
		
		    count -= 6;
            count_in_layer -= 6;
	    }
    }
}

static void paint_9_part_wall(const int set_base, const int bits, 
                                const int xpos, const int ypos,
                                const int color)
{
    int i;
    
	// printf("Set base %d\n", set_base);
	
    if(bits) {
	    for(i=0; i<4; i++) {
	        if(bits & (1 << i)) {
	            display_color_img(set_base+i, xpos, ypos, color, TRUE);
	        }
	    }                         
	
	    display_color_img(set_base+8, xpos, ypos, color, TRUE);
	
	    for(i=4; i<8; i++) {
	        if(bits & (1 << i)) {
	            display_color_img(set_base+i, xpos, ypos, color, TRUE);
	        }
	    }                         
	
    } else {
        // a pillar 
        display_color_img(set_base+8, xpos, ypos, color, TRUE);
    }
}


/**
 * Draw ground of location i,j onto screen position xpos,ypos
 * @author Hj. Malthaner
 */
void display_ground(int x, int y, int xpos, int ypos)
{
  // unused in Iso-Angband
}


static void display_walls(int xpos, int ypos, int x, int y, int feat_nc, int color)
{
    /* display floor/wall */
    const int use_9_part_walls = 1;
    const int info = get_info_wc(x, y);
	  
    if(use_9_part_walls && 
        (feat_nc == 240 || feat_nc == 249 || feat_nc == 267)  &&
        ((1 || info & CAVE_MARK) || (info & CAVE_SEEN) || is_player_in_town())) 
    {        
        const int bits = check_wall(x, y);   
        
        // Floor?
        display_color_img(4, xpos, ypos, color, TRUE);
        
        paint_9_part_wall(feat_nc, bits, xpos, ypos, color);
    } else {
        
        if(feat_nc > 32 && feat_nc < 128) {
            // ASCII need a floor
            display_color_img(4, xpos, ypos, color, TRUE);
        }
	
        // printf("%x\n", color);        
        display_color_img(feat_nc, xpos, ypos, color, TRUE);
    }   
}

static void display_town(int xpos, int ypos, int x, int y, int feat_nc, int ta)
{
    if(is_town_wall(x, y)) 
    {
		const int bits = check_wall(x, y);   
		display_color_img(TOWN_FLOOR, xpos, ypos, ta & COLOR_BITS, TRUE);    
		paint_9_part_wall(267, bits, xpos, ypos, ta & COLOR_BITS);
		paint_9_part_wall(267, bits, xpos, ypos-18, ta & COLOR_BITS);
    }
    else if(is_location_outside_town(x, y)) 
    {
		const int img = calc_town_outside(x, y);
		display_color_img(img, xpos, ypos, ta & COLOR_BITS, TRUE);    
    }
    else if(feat_nc == 0x04) 
	{
		/* special town floors */
		display_color_img(TOWN_FLOOR, xpos, ypos, ta & COLOR_BITS, TRUE);    
    }
    else 
	{
		
		if(feat_nc == 240 || feat_nc == 249 || feat_nc == 267)
		{
			display_color_img(TOWN_BUILDINGS, xpos, ypos, ta & COLOR_BITS, TRUE); 
		}
		else
		{
			display_walls(xpos, ypos, x, y, feat_nc, ta & COLOR_BITS);    
		}
    }
}    


/**
 * Draw objects of location i,j onto screen position xpos,ypos
 * @author Hj. Malthaner
 */
void display_things(int x, int y, int xpos, int ypos)
{
	const int grid = get_grid();
	int feat_nc = -1;
	int obj_nc;	

	/* relative to view position */
	int xoff = x - p_ptr->px + (SCREEN_WID/2+13);
	int yoff = y - p_ptr->py + (SCREEN_HGT/2+1);

	byte a=0x80, ta=0x80;
	char c=0xA0, tc=0xA0;

	/* try to use output of the term package */
	if(xoff >= 13 && yoff >= 1 && xoff < 79 && yoff < 22) {
		/* floor, walls */
    
		c = iso_cp[yoff][xoff];
		a = iso_ap[yoff][xoff];
    
		/* object/monster */
		tc = iso_ctp[yoff][xoff];
		ta = iso_atp[yoff][xoff];

	} else {
		/* outside 80x24 view, try to read map */

		if(is_valid_to_show(x,y)) 
		{      
			if(cave_info) 
			{
				map_info(y, x, &a, &c, &ta, &tc);
			}      
		} 
	}

	/* Text mode code */

	feat_nc = calc_nc_text(tc, ta);
	obj_nc = calc_nc_text(c, a);
  
	/* Hajo: if we are in town, we adjust display */
	if(is_player_in_town())
	{
		display_town(xpos, ypos, x, y, feat_nc, ta);
	}	  
	else if(feat_nc == 0x27) 
	{
		/* open doors */   
		display_color_img(138+door_direction(x,y), xpos, ypos, ta & COLOR_BITS, TRUE);
	} 
	else if(feat_nc == 0x2B) 
	{
		/* closed doors */
		display_color_img(136+door_direction(x,y), xpos, ypos, ta & COLOR_BITS, TRUE);
	} 
	else 
	{
		display_walls(xpos, ypos, x, y, feat_nc, ta & COLOR_BITS);    
	}  


	/* Hajo: display a complete grid if the user wants to */
	if(grid == 2) 
	{
		display_color_img(155, xpos, ypos, ta & COLOR_BITS, TRUE);
	}


	if(feat_nc != obj_nc) 
	{
		const int count = get_stack_size(x, y);
	  
		/* Hajo: display a grid below items/monsters if the user wants to */
		if(grid == 1) 
		{
			display_color_img(152, xpos, ypos, ta & COLOR_BITS, TRUE);
		}

		/* Hajo: display a shadow below items/monsters if the user wants to */
		if(shadow) 
		{
			display_color_img(SHADOW, xpos, ypos, ta & COLOR_BITS, TRUE);
		}

		/* Hajo: colored item/monster */
		// display_color_img(obj_nc, xpos, ypos, a & COLOR_BITS, TRUE);
		display_object_stack_grid(xpos, ypos, obj_nc, a & COLOR_BITS, count);
	}

	/* draw a cursor ? */
	if(xoff == high_x && 
       yoff == high_y &&
	   high_x >= 13 &&
       high_y >= 1 &&
       high_x < 79 &&
       high_y < 23) 
    {
		display_color_img(159, xpos, ypos, 1, TRUE);
	  
		/* undraw cursor */
		highlite_spot(-1,-1);
	}





  /* Graphics mode code

     feat_nc = calc_nc(tc, ta);
     obj_nc = calc_nc(c, a);

  shade = 1;
      
  if(is_lit(x, y)) {
    shade = 2;
  } else if(is_torch_lit(x, y)) {
    shade = 0;
  }

  
  // did we get some data ?
  if(feat_nc != -1) {
    
    
    // check shading
    //	printf("%d %d -> %d\n", x, y, shade);
    
    if(feat_nc >= 240 &&  feat_nc <= 245) {
      // old shading
      //	    const int shade = (feat_nc-240);
      const int set_base = 240+shade*9;
      const int bits = check_wall(x, y);
      
      
      display_img(316+(shade%3), xpos, ypos, TRUE);
      
      if(bits) {
	for(i=0; i<4; i++) {
	  if(bits & (1 << i)) {
	    display_img(set_base+i, xpos, ypos, TRUE);
	  }
	}                         
	
	display_img(set_base+8, xpos, ypos, TRUE);
	
	for(i=4; i<8; i++) {
	  if(bits & (1 << i)) {
	    display_img(set_base+i, xpos, ypos, TRUE);
	  }
	}                         
	
      } else {
	// a pillar 
	display_img(set_base+8, xpos, ypos, TRUE);
      }
      
      // fields with walls never contain anything else
      // so we can quit now
      return;
      
    } else if(feat_nc == 0x27) {
      // open doors   
      display_img(138+door_direction(x,y), xpos, ypos, TRUE);
      
      
    } else if(feat_nc == 0x2B) {
      // closed doors
      display_img(136+door_direction(x,y), xpos, ypos, TRUE);
      
    } else if(feat_nc == 0x8C ||	    // a between gate
	      feat_nc == 0x20 ||	    // empty square
	      feat_nc == 0x16E ||	    // brick roof
	      feat_nc == 0x16F ||	    // brick roof top
	      feat_nc == 0x176 ||	    // grass roof
	      feat_nc == 0x177 ||	    // grass roof chimney
	      feat_nc == 0x17E ||	    // grass roof top
	      feat_nc == 0x3E ||	    // down stairs
	      feat_nc == 0x3C	    // up stairs
	      ) {
      // this features should not be shaded
      
      
      display_img(feat_nc, xpos, ypos, TRUE);
      
    } else if(feat_nc > 2){             
      // this features should be shaded
      
      // floor
      
      // known grids get shaded floors
      if(is_only_torch_lit(x,y)) {
	display_color_img(feat_nc+2, 
			  xpos, ypos, 
			  get_distance_to_player(x, y) << 1,
			  TRUE);
      } else {
	display_img(feat_nc+shade, xpos, ypos, TRUE);
      }               
    }
    
    //	printf("%d a=%x c=%x (%c)\n", nc, a, c, c);
    
    
    if( obj_nc != feat_nc &&
	obj_nc > 2 &&
	obj_nc != 0x27 &&
	obj_nc != 0x2B ) {	    
      display_img(obj_nc, xpos, ypos, TRUE);
    }
    
    
  } else {
    
    // outside of dungeon
    display_img(32, xpos, ypos, TRUE);
  }
  */
}


