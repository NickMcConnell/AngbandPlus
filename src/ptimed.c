#include "angband.h"

#include "ptimed.h"
#include "int-map.h"

static int_map_ptr _map(void)
{
	static int_map_ptr m = NULL;
	if (!m)
		m = int_map_alloc(NULL);
	return m;
}

static ptimed_ptr _find(int which)
{
	ptimed_ptr result = p_ptr->effects;
	while (result)
	{
		if (result->type ==  which)
			break;
		result = result->next;
	}
	return result;
}

static int _count(void)
{
	int result = 0;
	ptimed_ptr e = p_ptr->effects;
	while (e)
	{
		result++;
		e = e->next;
	}
	return result;
}

static ptimed_ptr _alloc(void)
{
	ptimed_ptr result = malloc(sizeof(ptimed_t));
	result->type = 0;
	result->dur = 0;
	result->next = 0;
	return result;
}

static void _free(ptimed_ptr e)
{
	free(e);
}

static void _add(ptimed_ptr e)
{
	e->next = p_ptr->effects;
	p_ptr->effects = e;
}

static void _delete(ptimed_ptr e)
{
	ptimed_ptr c = p_ptr->effects;
	ptimed_ptr l = 0;
	while (c)
	{
		if (c == e)
		{
			if (l)
				l->next = c->next;
			else
				p_ptr->effects = c->next;

			_free(e);
			break;
		}
		
		l = c;
		c = c->next;
	}
}

void ptimed_register(int type, ptimed_f effect)
{
	int_map_add(_map(), type, effect);
}

void ptimed_load(FILE *fff)
{
	int ct, i;
	
	fread(&ct, sizeof(int), 1, fff);

	for (i = 0; i < ct; i++)
	{
		ptimed_ptr e = _alloc();

		fread(&e->type, sizeof(int), 1, fff);
		fread(&e->dur, sizeof(int), 1, fff);
	
		_add(e);
	}
}

void ptimed_save(FILE *fff)
{
	int n = _count();
	ptimed_ptr e;

	fwrite(&n, sizeof(int), 1, fff);

	for (e = p_ptr->effects; e; e = e->next);
	{
		fwrite(&e->type, sizeof(int), 1, fff);
		fwrite(&e->dur, sizeof(int), 1, fff);
	}
}

void ptimed_set(int which, int dur)
{
	ptimed_f   f = int_map_find(_map(), which);
	ptimed_ptr e = _find(which);

	if (dur == 0)
	{
		if (e)
		{
		}
	}

	if (!e)
	{
		e = _alloc();
		e->type = which;
		f(PTIMED_BEGIN);
	}
	else if (e->dur > dur)
		f(PTIMED_DECREASE);
	else if (e->dur < dur);
		f(PTIMED_INCREASE);

	e->dur = dur;
}

