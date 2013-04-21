#include "langband.h"
#include "lbwindows.h"
#include "lbtools.h"

int lbui_num_predefinedFrames = -1;
int lbui_max_predefinedFrames = -1;
LangbandFrame **lbui_predefinedFrames = NULL;
int lbui_num_activeFrames = -1;
int lbui_max_activeFrames = -1;
LangbandFrame **lbui_activeFrames = NULL;

int
lbui_init_frame_system(int active_size, int predefined_size) {
    int i;

    //DBGPUT("Sizes %d and %d\n", active_size, predefined_size);
    
    if (active_size > 0) {
	lbui_max_activeFrames = active_size;
	lbui_num_activeFrames = 0;
	lbui_activeFrames = malloc(active_size * sizeof(LangbandFrame*));
	for (i=0; i < active_size; i++) {
	    lbui_activeFrames[i] = NULL;
	}
    }
    else {
	ERRORMSG("Illegal size %d for active frame/subwindow-array.\n", active_size);
	return -1;
    }

    if (predefined_size > 0) {
	lbui_max_predefinedFrames = predefined_size;
	lbui_num_predefinedFrames = predefined_size; 
	lbui_predefinedFrames = malloc(predefined_size * sizeof(LangbandFrame*));
	for (i=0; i < predefined_size; i++) {
	    lbui_predefinedFrames[i] = NULL;
	}
    }
    else {
	ERRORMSG("Illegal size %d for predefined frame/subwindow-array.\n", active_size);
	return -2;
    }

    return 0;
}

int
lbui_cleanup_frame_system() {
    int i;

    //DBGPUT("Cleaning frame-system\n");
    
    if (lbui_max_predefinedFrames > 0) {
        for (i=0; i < lbui_max_predefinedFrames; i++) {
	    if (lbui_predefinedFrames[i]) {
		free(lbui_predefinedFrames[i]);
		lbui_predefinedFrames[i] = NULL;
	    }
	}
	free(lbui_predefinedFrames);
	lbui_predefinedFrames = NULL;
	
	lbui_max_predefinedFrames = lbui_num_predefinedFrames = -1;
    }

    if (lbui_max_activeFrames > 0) {
        for (i=0; i < lbui_max_activeFrames; i++) {
	    if (lbui_activeFrames[i]) {
		// just pointers to already cleaned predefined frames
		//free(activeFrames[i]);
		lbui_activeFrames[i] = NULL;
	    }
	}
	free(lbui_activeFrames);
	lbui_activeFrames = NULL;
	
	lbui_max_activeFrames = lbui_num_activeFrames = -1;
    }
    
    return 0;
}

int
lbui_legal_frame_key_p(int key, FrameType ft) {
    int cur_size = -1;
    
    if (ft == ACTIVE) {
	cur_size = lbui_max_activeFrames;
    }
    else if (ft == PREDEFINED) {
	cur_size = lbui_max_predefinedFrames;
    }
    else {
	ERRORMSG("Illegal frametype %d\n", ft);
    }
    
    if (key >= 0 && key < cur_size) {
	return 1;
    }
    else {
	return 0;
    }
}

LangbandFrame *
lbui_make_frame(int key, const char *name) {
    if (!name) {
	ERRORMSG("Empty name passed to make_frame(), please supply a real name.\n");
	return NULL;
    }
    
    if (lbui_legal_frame_key_p(key, PREDEFINED)) {
	LangbandFrame *lf = malloc(sizeof(LangbandFrame));
	memset(lf, 0, sizeof(LangbandFrame));
	lf->key = key;
	lf->name = malloc(strlen(name)+1);
	strcpy(lf->name, name);
	//DEBUGPUT("Made frame '%s' at %d\n", name, key);
	return lf;
    }
    else {
	ERRORMSG("Illegal key %d supplied to make_frame() %s.\n", key, name);
	return NULL;
    }
}

int
lbui_add_frame(int key, const char *name) {
    
    LangbandFrame *lf = lbui_make_frame(key, name);
    
    if (!lf) {
	ERRORMSG("Unable to produce a LangbandFrame [%d,%s], returning.\n",
		 key, name);
	return -1;
    }

    //DBGPUT("Added frame %s at %d.\n", name, key);
    lbui_predefinedFrames[key] = lf;
    
    return 0;
}


int
lbui_add_frame_coords(int key, int x, int y, int w, int h) {

    if (!lbui_legal_frame_key_p(key,PREDEFINED)) {
	ERRORMSG("Illegal key %d for frame.\n", key);
	return -1;
    }
    else {
    	LangbandFrame *lf = lbui_predefinedFrames[key];

	if (!lf) {
	    ERRORMSG("No frame registered for key key %d.\n", key);
	    return -2;   
	}
	
	lf->xoffset = x;
	lf->yoffset = y;
	lf->allowed_width = w;
	lf->allowed_height = h;
	
	return 0;
    }
}

int
lbui_add_frame_fontinfo(int key, const char *font, int ptsize, int style) {

    if (lbui_legal_frame_key_p(key, PREDEFINED)) {
	LangbandFrame *lf = lbui_predefinedFrames[key];

	if (font && strlen(font)>1) {
	    lf->fontname = malloc(strlen(font)+1);
	    strcpy(lf->fontname, font);
	}
	if (ptsize > 0) {
	    lf->wanted_fontsize = ptsize;
	}
	else {
	    lf->wanted_fontsize = 16; // default
	}

	if (style > 0) {
	    lf->wanted_fontstyle = style;
	}
	else {
	    lf->wanted_fontstyle = 0;
	}
	
	return 0;
    }
    else {
	ERRORMSG("Illegal key %d for subwindow.\n", key);
	return -1;
    }
}

int
lbui_add_frame_tileinfo(int key, int tw, int th) {

    if (lbui_legal_frame_key_p(key, PREDEFINED)) {
	LangbandFrame *lf = lbui_predefinedFrames[key];

	lf->tile_width = tw;
	lf->tile_height = th;
		
	return 0;
    }
    else {
	ERRORMSG("Illegal key %d for subwindow.\n", key);
	return -1;
    }
}

int
lbui_add_frame_gfxinfo(int key, int use_tiles) {

    if (lbui_legal_frame_key_p(key, PREDEFINED)) {
	LangbandFrame *lf = lbui_predefinedFrames[key];

	lf->use_gfx_tiles = use_tiles;
		
	return 0;
    }
    else {
	ERRORMSG("Illegal key %d for subwindow.\n", key);
	return -1;
    }
}

int
lbui_add_frame_bg(int key, int img_idx) {
    
    if (lbui_legal_frame_key_p(key, PREDEFINED)) {
	LangbandFrame *lf = lbui_predefinedFrames[key];

	if (!lf) {
	    return -2;
	}
	else {
	    lf->background = img_idx;
	    return 0;
	}
    }
    else {
	ERRORMSG("Illegal key %d for subwindow.\n", key);
	return -1;
    }

}


LangbandFrame *
lbui_get_frame(int key, FrameType ft) {
    
    //DBGPUT("Getting frame %d %d which is %p %p.\n", key, ft, activeFrames[key], predefinedFrames[key]);
    if (lbui_legal_frame_key_p(key, ft)) {
	if (ft == ACTIVE) {
	    return lbui_activeFrames[key];
	}
	else if (ft == PREDEFINED) {
	    return lbui_predefinedFrames[key];
	}
	else {
	    ERRORMSG("Illegal frametype %d for frame %d.\n", ft, key);
	    return NULL;
	}
    }

    ERRORMSG("Illegal key %d for subwindow.\n", key);
    return NULL;
}

int
lbui_has_frame(int key, FrameType ft) {
    LangbandFrame *lf = lbui_get_frame(key, ft);
    if (lf)
	return 1;
    else
	return 0;
}

int
lbui_get_frame_columns(int key, FrameType ft) {
    LangbandFrame *lf = lbui_get_frame(key, ft);
    if (lf)
	return lf->columns;
    else
	return -50;
}

int
lbui_get_frame_rows(int key, FrameType ft) {
    LangbandFrame *lf = lbui_get_frame(key, ft);
    if (lf)
	return lf->rows;
    else
	return -50;
}

int
lbui_get_frame_tile_width(int key, FrameType ft) {
    LangbandFrame *lf = lbui_get_frame(key, ft);
    if (lf)
	return lf->tile_width;
    else
	return -50;
}

int
lbui_get_frame_tile_height(int key, FrameType ft) {
    LangbandFrame *lf = lbui_get_frame(key, ft);
    if (lf)
	return lf->tile_height;
    else
	return -50;
}

int
lbui_get_frame_gfx_tiles(int key, FrameType ft) {
    LangbandFrame *lf = lbui_get_frame(key, ft);
    if (lf)
	return lf->use_gfx_tiles;
    else
	return -50;
}

int
lbui_clean_frame(int key) {
    LangbandFrame *lf = lbui_get_frame(key, ACTIVE);
    if (lf && lf->visible) {
	DEBUGPUT("CLEANING FRAME.. BAD!");
#if 0
	angband_zterm *old = Term;
	Term_activate(lf->azt);
	Term_redraw();
	Term_fresh();
	Term_activate(old);
#endif
	return key;
    }
    else {
	return -1;
    }
}

int
lbui_wipe_frame(int key) {
    LangbandFrame *lf = lbui_get_frame(key, ACTIVE);

    if (lf && lf->visible) {
	DBGPUT("Wiping FRAME %d.\n", key);
#if 0
	angband_zterm *old = Term;
	int which_ui = current_ui();
	

	
	Term_activate(lf->azt);
	
	Term_clear();
	Term_fresh();
	
	if (0) { return -1; }
#ifdef USE_SDL
	else if (which_ui == UITYPE_SDL) {
	    Term_xtra_sdl(TERM_XTRA_CLEAR,5);
	}
#endif
#ifdef USE_GCU
	else if (which_ui == UITYPE_GCU) {
	    Term_xtra_gcu(TERM_XTRA_CLEAR,5);
	}
#endif

	// redraw here?
	Term_redraw();
	Term_fresh();
	Term_activate(old);
	//DBGPUT("Wiped %d.\n", key);
#endif
	return key;
    }
    else {
	return -1;
    }
}

int
lbui_activate_frame(int key) {
    LangbandFrame *lf = NULL;

    if (!lbui_legal_frame_key_p(key, PREDEFINED)) {
        ERRORMSG("Illegal key %d given to activate_frame().\n", key);
        return -1;
    }

    //DBGPUT("Activate frame %d.\n", key);
    
    lf = lbui_predefinedFrames[key];

    if (lf) {
        lf->visible = 1;
        lbui_activeFrames[key] = lf;
        lbui_num_activeFrames++;
        return key;
    }
    else {
        ERRORMSG("Illegal key %d given to activate_frame() (doesn't exist).\n", key);
        return -2;
    }
}

int
lbui_deactivate_frame(int key) {
    LangbandFrame *lf = NULL;
    
    if (!lbui_legal_frame_key_p(key, PREDEFINED)) {
        ERRORMSG("Illegal key %d given to activate_frame().\n", key);
        return -1;
    }
    
    //DBGPUT("Deactivate frame %d.\n", key);
    
    lf = lbui_predefinedFrames[key];

    if (lf) {
        lf->visible = 0;
        lbui_activeFrames[key] = NULL;
        lbui_num_activeFrames--;
        return key;
    }
    else {
        // it's ok to deactivate a frame that's not there
        return -2;
    }
}
