/*
 *  Hooks and Callbacks for various classes
 */

#include "angband.h"

class_t *get_class_t(void)
{
class_t *result = NULL;

	switch (p_ptr->pclass)
	{
	case CLASS_ARCHAEOLOGIST:
		result = archaeologist_get_class_t();
		break;
	case CLASS_DUELIST:
		result = duelist_get_class_t();
		break;
	}

	return result;
}