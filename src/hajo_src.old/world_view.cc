/*
 * Copyright (c) 2001 Hansjörg Malthaner
 * hansjoerg.malthaner@gmx.de
 *
 * This software may not be sold or used in other projects 
 * than Iso-Angband without written permission by the author
 *
 */

#include <stdio.h>
#include "simgraph.h" 
#include "world_view.h" 
#include "world_adaptor.h" 


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



static bool is_valid_position(int x, int y) {
    return x >= 0 && y >= 0 && x<DUNGEON_WID && y<DUNGEON_HGT;
}                             



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
}


/**
 * Dinge von i,j an Bildschirmkoordinate xpos,ypos zeichnen.
 * @author Hj. Malthaner
 */
#ifdef USE_HAJO_HACK

static bool is_valid_position(int x, int y) {
    return x >= 0 && y >= 0 && x<DUNGEON_WID && y<DUNGEON_HGT;
}                             

static bool is_valid_to_show(int x, int y) {
    return is_valid_position(x, y) && cave_m_idx;
}

static int get_feat_nc(int x, int y)
{
    return cave_feat[y][x];
}

static int get_feat_wc(int x, int y)
{
    if(is_valid_postion(x,y) {
	return get_feat_nc(x,y);
    } else {
	FEAT_NONE;
    }
}

static int get_floor_tile(int x, int y) {

    int result = 32;

    byte a;
    unsigned char c;

    byte feat;
    byte info;

    feature_type *f_ptr;

    s16b m_idx;

    s16b image = p_ptr->image;

    /* Monster/Player */
    m_idx = cave_m_idx[y][x];

    /* Feature */
    feat = get_feat_nc(x, y);

    /* Cave flags */
    info = cave_info[y][x];

    /* Hack -- rare random hallucination on non-outer walls */
    if (image && (!rand_int(256)) && (feat < FEAT_PERM_SOLID))
    {
	int i = image_random();

	a = PICT_A(i);
	c = PICT_C(i);
    }

    /* Boring grids (floors, etc) */
    else if (feat <= FEAT_INVIS)
    {
	/* Memorized (or seen) floor */
	if ((info & (CAVE_MARK)) ||
	    (info & (CAVE_SEEN)))
	{
	    /* Get the floor feature */
	    f_ptr = &f_info[FEAT_FLOOR];

	    /* Normal attr */
	    a = f_ptr->x_attr;

	    /* Normal char */
	    c = f_ptr->x_char;

	    /* Special lighting effects */
	    if (view_special_lite)
	    {
		/* Handle "seen" grids */
		if (info & (CAVE_SEEN))
		{
		    /* Only lit by "torch" lite */
		    if (view_yellow_lite && !(info & (CAVE_GLOW)))
		    {
			/* Use "yellow" */
			a = TERM_YELLOW;
		    }
		}

		/* Handle "blind" */
		else if (p_ptr->blind)
		{
		    /* Use "dark gray" */
		    a = TERM_DARK;
		}
		
		/* Handle "dark" grids */
		else if (!(info & (CAVE_GLOW)))
		{
		    /* Use "dark gray" */
		    a = TERM_DARK;
		}
		
		/* Handle "view_bright_lite" */
		else if (view_bright_lite)
		{
		    /* Use "gray" */
		    a = TERM_L_DARK;
		}
	    }
	}

	/* Unknown */
	else
	{
	    /* Get the darkness feature */
	    f_ptr = &f_info[FEAT_NONE];
	    
	    /* Normal attr */
	    a = f_ptr->x_attr;
	    
	    /* Normal char */
	    c = f_ptr->x_char;
	}
    }

    /* Interesting grids (non-floors) */
    else
    {
	/* Memorized grids */
	if (info & (CAVE_MARK))
	{
	    /* Apply "mimic" field */
	    feat = f_info[feat].mimic;

	    /* Get the feature */
	    f_ptr = &f_info[feat];

	    /* Normal attr */
	    a = f_ptr->x_attr;
			
	    /* Normal char */
	    c = f_ptr->x_char;

	    /* Special lighting effects (walls only) */
	    if (view_granite_lite && feat_supports_lighting(feat)) {
		/* Handle "seen" grids */
		
		/* Only lit by "torch" lite */
		if (view_yellow_lite && !(info & (CAVE_GLOW)))
		{
		    /* Use "yellow" */
		    a = TERM_YELLOW;
		}
				
		if (info & (CAVE_SEEN))
		{
		    /* Use "white" */

		}

		/* Handle "blind" */
		else if (p_ptr->blind)
		{
		    /* Use "dark gray" */
		    a = TERM_L_DARK;
		}

		/* Handle "view_bright_lite" */
		else if (view_bright_lite)
		{
		    /* Use "gray" */
		    a = TERM_SLATE;
		}
		else
		{
		    /* Use "white" */
		    
		}
	    }
	}

	/* Unknown */
	else
	{
	    /* Get the darkness feature */
	    result = 32;
	}
    }


    // Hajo:
    // Check direction of doors (open or closed)
    
    if((info & (CAVE_MARK|CAVE_SEEN)) &&
	((feat >= FEAT_DOOR_HEAD && feat <= FEAT_DOOR_TAIL) ||
	(feat == FEAT_OPEN) || (feat == FEAT_BROKEN))) {
	    
	    if((feat >= FEAT_DOOR_HEAD && feat <= FEAT_DOOR_TAIL)) {
		result = 136;
	    } else {
		result = 138;
	    }


	    if(!((x > 0) && 
                 (cave_feat[y][x-1] <= FEAT_INVIS) &&
	         (x < DUNGEON_WID-1) &&
                 (cave_feat[y][x+1] <= FEAT_INVIS))) {
                result ++;
	    }
	}

    }       


    return result;
}





void world_view_t::display_dinge(int x, int y, int xpos, int ypos, bool dirty)
{
    if(p_ptr == NULL || p_ptr->playing == FALSE) {
	return;
    }


    if(is_valid_to_show(x,y)) {

        const int img = get_floor_tile(x, y);
	display_img(32, xpos, ypos, dirty);

//	printf("%d,%d %d %d %d\n", x, y, c, (a & 15) * 4, dirty);

    } else {
	// outside of dungeon

	display_img(32, xpos, ypos, dirty);
    }


    if(is_valid_to_show(x,y)) {
	byte a = 0;
	char c = 0;

	byte feat;
	byte info;

	s16b this_o_idx, next_o_idx = 0;

	s16b m_idx;

	s16b image = p_ptr->image;

	int floor_num = 0;

	/* Monster/Player */
	m_idx = cave_m_idx[y][x];

	/* Feature */
	feat = cave_feat[y][x];

	/* Cave flags */
	info = cave_info[y][x];

	/* Objects */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorized objects */
		if (o_ptr->marked)
		{
			/* Hack -- object hallucination */
			if (image)
			{
				int i = image_object();

				a = PICT_A(i);
				c = PICT_C(i);

				break;
			}

			/* Normal attr */
			a = object_attr(o_ptr);

//			printf("%x\n", a);

			/* Normal char */
			c = object_char(o_ptr);

//			const unsigned int ac = transform(a, c);    
//			display_color_img(ac & 0xff, xpos+floor_num*2, ypos, (ac >> 8) * 4, dirty);
                        {
			    int nc = (a & 0x10) ? c+128 : c;
			    display_img(nc, xpos+floor_num*2, ypos, dirty);
			}

			floor_num ++;
		}
	}

	c = a = 0;

	/* Monsters */
	if (m_idx > 0)
	{
		monster_type *m_ptr = &m_list[m_idx];

		/* Visible monster */
		if (m_ptr->ml)
		{
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			byte da;
			char dc;

			/* Desired attr */
			da = r_ptr->x_attr;

			/* Desired char */
			dc = r_ptr->x_char;

			/* Hack -- monster hallucination */
			if (image)
			{
				int i = image_monster();

				a = PICT_A(i);
				c = PICT_C(i);
			}

			/* Ignore weird codes */
			else if (avoid_other)
			{
				/* Use attr */
				a = da;

				/* Use char */
				c = dc;
			}

			/* Special attr/char codes */
			else if ((da & 0x80) && (dc & 0x80))
			{
				/* Use attr */
				a = da;

				/* Use char */
				c = dc;
			}

			/* Multi-hued monster */
			else if (r_ptr->flags1 & (RF1_ATTR_MULTI))
			{
				/* Multi-hued attr */
				a = randint(15);

				/* Normal char */
				c = dc;
			}

			/* Normal monster (not "clear" in any way) */
			else if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR | RF1_CHAR_CLEAR)))
			{
				/* Use attr */
				a = da;

				/* Use char */
				c = dc;
			}

			/* Hack -- Bizarre grid under monster */
			else if ((a & 0x80) || (c & 0x80))
			{
				/* Use attr */
				a = da;

				/* Use char */
				c = dc;
			}

			/* Normal char, Clear attr, monster */
			else if (!(r_ptr->flags1 & (RF1_CHAR_CLEAR)))
			{
				/* Normal char */
				c = dc;
			}

			/* Normal attr, Clear char, monster */
			else if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR)))
			{
				/* Normal attr */
				a = da;
			}
		}
	}

	/* Handle "player" */
	else if ((m_idx < 0) && !(p_ptr->running && hidden_player))
	{
		monster_race *r_ptr = &r_info[0];

		/* Get the "player" attr */
		a = r_ptr->x_attr;

		/* Get the "player" char */
		c = r_ptr->x_char;
	}

#ifdef MAP_INFO_MULTIPLE_PLAYERS
	/* Players */
	else if (m_idx < 0)
#else /* MAP_INFO_MULTIPLE_PLAYERS */
	/* Handle "player" */
	else if ((m_idx < 0) && !(p_ptr->running && hidden_player))
#endif /* MAP_INFO_MULTIPLE_PLAYERS */
	{
		monster_race *r_ptr = &r_info[0];

		/* Get the "player" attr */
		a = r_ptr->x_attr;

		/* Get the "player" char */
		c = r_ptr->x_char;
	}
                        
	if(c != 0 && a != 0) {
            int nc = (a & 0x10) ? c+128 : c;
	    display_img(nc, xpos, ypos, dirty);
	}

    } else {
	// outside of dungeon
	// display nothing
    }
}

#else

int iso_access(unsigned char **field, int x, int y)
{
    if(x >= 0 && y >= 0 && x<80 && y<24) {
	return field[y][x] & 0x7f;
    } else {
	return 0;
    }                       
}


//#include "walls4.h"
#include "walls9.h"

int is_wall(int x, int y)
{           
    const int atp = iso_access(iso_atp, x, y);
    const int ctp = iso_access(iso_ctp, x, y);

    return (atp == 0x01 && (ctp >= 0x70 && ctp <= 0x75)) ||
	   (atp == 0x00  && (ctp =='+' || ctp == '\'')) ;
}


int check_wall(int x, int y) 
{

    int o = 0;

    o |= (is_wall(x-1, y-1)) << 0;
    o |= (is_wall(x  , y-1)) << 1;
    o |= (is_wall(x+1, y-1)) << 2;
    o |= (is_wall(x-1, y  )) << 3;
    o |= (is_wall(x+1, y  )) << 4;
    o |= (is_wall(x-1, y+1)) << 5;
    o |= (is_wall(x  , y+1)) << 6;
    o |= (is_wall(x+1, y+1)) << 7;


    return wall_table[o];
}


int door_direction(int x, int y)
{                 
    if(is_wall(x-1, y) && is_wall(x+1, y)) {
	return 1;
    } else {
	return 0;
    }
}


void world_view_t::display_dinge(int x, int y, int xpos, int ypos, bool dirty)
{
    if(x >= 0 && y >= 1 && x < 80 && y < 23) {
	// floor, walls

	int c = (iso_ctp[y][x] & 0x7F);
	int a = (iso_atp[y][x] & 0x7F);

	int nc = (a  << 7) + c;

//	printf("%d a=%x c=%x (%c)\n", nc, a, c, c);


	if(nc >= 240 &&  nc <= 245) {
	    const int shade = (nc-240);
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

	} else if(nc == 0x27) {
	    // open doors   
	    display_img(138+door_direction(x,y), xpos, ypos, dirty);
	    

	} else if(nc == 0x2B) {
	    // closed doors
	    display_img(136+door_direction(x,y), xpos, ypos, dirty);

	} else {             
	    // floor
	    display_img(nc, xpos, ypos, dirty);
	}

	// object/monster
	c = (iso_cp[y][x] & 0x7F);
	a = (iso_ap[y][x] & 0x7F);

	nc = (a << 7) + c;

//	printf("%d a=%x c=%x (%c)\n", nc, a, c, c);

        if((nc < 276 || nc > 278) &&
            nc != 0x27 &&
	    nc != 0x2B ) {	    
	    display_img(nc, xpos, ypos, dirty);
	}

    } else {


	// outside of dungeon
	display_img(32, xpos, ypos, dirty);
    }
}


#endif /* USE_HAJO_HACK */