#include "h-basic.h"
#include "z-form.h"
#include "z-term.h"
#include "ui.h"
#include "z-type.h"

const type_union END = { T_END, { 0 } };

const char type_union::printf_mode[T_MAX] = {'d', 'f', 'c', 's'};

void display_panel(const data_panel *panel, int count, bool left_adj, const region *bounds)
{
	int i;
	char buffer[50];
	int col = bounds->col;
	int row = bounds->row;
	int w = bounds->width;
	int offset = 0;

	region_erase(bounds);

	if (left_adj)
	{
		for (i = 0; i < count; i++)
		{
			int len = panel[i].label ? strlen(panel[i].label) : 0;
			if (offset < len) offset = len;
		}
		offset += 2;
	}

	for (i = 0; i < count; i++, row++)
	{
		int len;
		if (!panel[i].label) continue;
		Term_putstr(col, row, strlen(panel[i].label), TERM_WHITE, panel[i].label);

		strnfmt(buffer, sizeof(buffer), panel[i].fmt, panel[i].value[0], panel[i].value[1]);

		len = strlen(buffer);
		len = len < w - offset ? len : w - offset - 1;
		Term_putstr(((left_adj) ? col+offset : col+w-len), row, len, panel[i].color, buffer);
	}
}
