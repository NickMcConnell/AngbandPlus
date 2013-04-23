
#include "angband.h"

#define DLL_version 1

#define Angband_version "2.1.0e"

#define Variant_name "ZAngband"

#define compiled_by "Robert Ruehlmann (rr9@angband.org)"

bool get_use_sound(void)
{
	return use_sound;
}


bool get_use_graphics(void)
{
	return use_graphics;
}


void set_use_sound(bool state)
{
	use_sound = state;
}


void set_use_graphics(bool state)
{
	use_graphics = state;
}

void set_term_init_hook(term *t, vptr init_hook)
{
	t->init_hook = init_hook;
}

void set_term_nuke_hook(term *t, vptr nuke_hook)
{
	t->nuke_hook = nuke_hook;
}

void set_term_user_hook(term *t, vptr user_hook)
{
	t->user_hook = user_hook;
}

void set_term_xtra_hook(term *t, vptr xtra_hook)
{
	t->xtra_hook = xtra_hook;
}

void set_term_curs_hook(term *t, vptr curs_hook)
{
	t->curs_hook = curs_hook;
}

void set_term_wipe_hook(term *t, vptr wipe_hook)
{
	t->wipe_hook = wipe_hook;
}

void set_term_text_hook(term *t, vptr text_hook)
{
	t->text_hook = text_hook;
}

void set_term_pict_hook(term *t, vptr pict_hook)
{
	t->pict_hook = pict_hook;
	t->higher_pict = TRUE;
	t->soft_cursor = TRUE;
}

void set_plog_hook(vptr plog_hook)
{
	plog_aux = plog_hook;
}

void set_quit_hook(vptr quit_hook)
{
	quit_aux = quit_hook;
}

void set_core_hook(vptr core_hook)
{
	core_aux = core_hook;
}

void set_angband_term(int i, term *t)
{
	angband_term[i] = t;
}

term* get_angband_term(int i)
{
	return angband_term[i];
}

void set_term_data(int i, vptr data)
{
	angband_term[i]->data = data;
}

vptr get_term_data(int i)
{
	return (angband_term[i]->data);
}

void set_ANGBAND_SYS(void)
{
	ANGBAND_SYS = "DLL";
}

void set_ANGBAND_GRAF(char *mode)
{
	ANGBAND_GRAF = mode;
}

byte get_angband_color(byte i, byte j)
{
	return (angband_color_table[i][j]);
}

void set_savefile_name(char *name)
{
	strcpy(savefile, name);
}

vptr get_current_term(void)
{
	return (Term->data);
}

char* get_angband_term_name(int i)
{
	return angband_term_name[i];
}

int get_DLL_version(void)
{
	return DLL_version;
}

char* get_Angband_version(void)
{
	return Angband_version;
}

char* get_Variant_name(void)
{
	return Variant_name;
}

char* get_Variant_dir_name(void)
{
	return Variant_name;
}

char* get_compiled_by(void)
{
	return compiled_by;
}
