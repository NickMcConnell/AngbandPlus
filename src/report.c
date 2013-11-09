/* File: report.c */

#define _GNU_SOURCE
#include "angband.h"

#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>

#if defined(WINDOWS)
#include <winsock.h>
#elif defined(MACINTOSH)
#include <OpenTransport.h>
#include <OpenTptInternet.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/time.h>

#include <setjmp.h>
#include <signal.h>
#endif

/*
 * simple buffer library
 */

typedef struct {
	size_t max_size;
	size_t size;
	char *data;
} BUF;

#define	BUFSIZE	(65536)

#ifndef HAVE_VASPRINTF
#define vasprintf	Vasprintf

static int Vasprintf(char **buf, const char *fmt, va_list ap)
{
	int ret;

	*buf = malloc(1024);

#if defined(HAVE_VSNPRINTF)
	ret = vsnprintf(*buf, 1024, fmt, ap);
#else
	ret = vsprintf(*buf, fmt, ap);
#endif
	return ret;
}

#endif /* ifndef HAVE_VASPRINTF */ 

static BUF* buf_new(void)
{
	BUF *p;

	if ((p = malloc(sizeof(BUF))) == NULL)
		return NULL;

	p->size = 0;
	p->max_size = BUFSIZE;
	if ((p->data = malloc(BUFSIZE)) == NULL)
	{
		free(p);
		return NULL;
	}
	return p;
}

static void buf_delete(BUF *b)
{
	free(b->data);
	free(b);
}

static int buf_append(BUF *buf, const char *data, size_t size)
{
	while (buf->size + size > buf->max_size)
	{
		char *tmp;
		if ((tmp = malloc(buf->max_size * 2)) == NULL) return -1;

		memcpy(tmp, buf->data, buf->max_size);
		free(buf->data);

		buf->data = tmp;

		buf->max_size *= 2;
	}
	memcpy(buf->data + buf->size, data, size);
	buf->size += size;

	return buf->size;
}

static int buf_sprintf(BUF *buf, const char *fmt, ...)
{
	int		ret;
	char	*tmpbuf;
	va_list	ap;

	va_start(ap, fmt);
	vasprintf(&tmpbuf, fmt, ap);
	va_end(ap);

	if(!tmpbuf) return -1;

#ifdef MAC_MPW
	{
		/* '\n' is 0x0D and '\r' is 0x0A in MPW. Swap back these. */
		char *ptr;
		for (ptr = tmpbuf; *ptr; ptr++)
			if ('\n' == *ptr) *ptr = '\r';
	}
#endif

	ret = buf_append(buf, tmpbuf, strlen(tmpbuf));

	free(tmpbuf);

	return ret;
}


/*
 * Make screen dump to buffer
 */
cptr make_screen_dump(void)
{
	BUF *screen_buf;
	int y, x, i;
	cptr ret;

	byte a = 0, old_a = 0;
	char c = ' ';

	static cptr html_head[] = {
		"<html>\n<body text=\"#ffffff\" bgcolor=\"#000000\">\n",
		"<pre>",
		0,
	};
	static cptr html_foot[] = {
		"</pre>\n",
		"</body>\n</html>\n",
		0,
	};

	int wid, hgt;

	Term_get_size(&wid, &hgt);

	/* Alloc buffer */
	screen_buf = buf_new();
	if (screen_buf == NULL) return (NULL);

	for (i = 0; html_head[i]; i++)
		buf_sprintf(screen_buf, html_head[i]);

	/* Dump the screen */
	for (y = 0; y < hgt; y++)
	{
		/* Start the row */
		if (y != 0)
			buf_sprintf(screen_buf, "\n");

		/* Dump each row */
		for (x = 0; x < wid - 1; x++)
		{
			int rv, gv, bv;
			cptr cc = NULL;
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			switch (c)
			{
			case '&': cc = "&amp;"; break;
			case '<': cc = "&lt;"; break;
			case '>': cc = "&gt;"; break;
#ifdef WINDOWS
			case 0x1f: c = '.'; break;
			case 0x7f: c = (a == 0x09) ? '%' : '#'; break;
#endif
			}

			a = a & 0x0F;
			if ((y == 0 && x == 0) || a != old_a) {
				rv = angband_color_table[a][1];
				gv = angband_color_table[a][2];
				bv = angband_color_table[a][3];
				buf_sprintf(screen_buf, "%s<font color=\"#%02x%02x%02x\">", 
					    ((y == 0 && x == 0) ? "" : "</font>"), rv, gv, bv);
				old_a = a;
			}
			if (cc)
				buf_sprintf(screen_buf, "%s", cc);
			else
				buf_sprintf(screen_buf, "%c", c);
		}
	}
	buf_sprintf(screen_buf, "</font>");

	for (i = 0; html_foot[i]; i++)
		buf_sprintf(screen_buf, html_foot[i]);

	/* Screen dump size is too big ? */
	if (screen_buf->size + 1> SCREEN_BUF_SIZE)
	{
		ret = NULL;
	}
	else
	{
		/* Terminate string */
		buf_append(screen_buf, "", 1);

		ret = string_make(screen_buf->data);
	}

	/* Free buffer */
	buf_delete(screen_buf);

	return ret;
}
