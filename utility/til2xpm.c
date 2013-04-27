/*  til2xpm.c 
 *
 *  compile like this
 *
 *  gcc -o til2xpm til2xpm.c
 *  
 *  written by Marcello Sega & Marco Vecchiocattivi (29.07.2002)
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.     
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.     
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *  
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "palette.h"

int
main (int argc, char **argv)
{
    FILE *cid1, *cid2 = NULL;
    char *name, *str_name, scene_name[128], *ptr;
    int nvw, nsc, nfr, hgt, ndt, i, j, l, byte, tmp, xpalette[256], k, kk,
	pixel;

    if (argc != 2)
	exit (printf ("Usage: til2xpm input_file.til\n"));
    cid1 = fopen (argv[1], "r");

    if (!cid1)
	exit (printf ("Error: file %s doesn't exist\n", argv[1]));

    name = (char *) malloc (sizeof (char) * strlen (argv[1]) + 1024);
    // exmpl: name.til -> name_view_scene_frame.xpm 
    str_name = (char *) malloc (sizeof (char) * strlen (argv[1]) + 1024);

    //  Let's do a sanity check...
    if (fgetc (cid1) != 'T')
	exit(printf ("Error: not a valid tile format\n"));
    if (fgetc (cid1) != 'I')
	exit(printf ("Error: not a valid tile format\n"));
    if (fgetc (cid1) != 'L')
	exit(printf ("Error: not a valid tile format\n"));

    printf ("\n%s\n\t revision number %d\n", argv[1], fgetc (cid1));

    nvw = fgetc (cid1);		// number of views
    nsc = fgetc (cid1);		// number of scenes

    printf ("\t %d view(s)\n\t %d scene(s)\n", nvw, nsc);

    for (i = 0; i < nsc; i++)
    {
	nfr = fgetc (cid1);	// number of frames
	ptr = scene_name;
	do
	{
	    *ptr = fgetc (cid1);
	    ptr++;
	}

	while (*(ptr - 1) != 0);

	printf ("\t scene %s (%d frames)\n", scene_name, nfr);

	for (l = 0; l < nfr * nvw; l++)
	{

	    hgt = fgetc (cid1);	// height 
	    ndt = fgetc (cid1);	// number of data (1)
	    ndt += fgetc (cid1) << 8;	// (2)

	    printf ("\t\t height: %d; %d data\n", hgt, ndt);

	    kk = 0;

	    for (k = 0; k < 256; k++)
		xpalette[k] = -1;

	    for (j = 0; j < ndt; j++)	// scanning every data to extract palette
	    {
		byte = fgetc (cid1);
		for (k = 0; k <= kk; k++)
		{
		    if (byte == xpalette[k])	// color already found
		    {
			byte = -1;
			break;
		    }
		}
		if (byte != -1)	// new color found
		{
		    xpalette[kk] = byte;
		    kk++;
		}
	    }			//now we are at the end of frame

	    fseek (cid1, -ndt, SEEK_CUR);	// start of frame

	    strcpy (name, argv[1]);
	    sprintf (name + strlen (name) - 4, "_%s_%d_%d.xpm", scene_name,
		     l / nfr, l % nfr);
	    strcpy (str_name, name);
	    str_name[strlen (str_name) - 4] = '\0';
	    cid2 = fopen (name, "w");
	    fputs ("/* XPM */\n\n", cid2);
	    fputs ("/* converted from utumno tile format \n", cid2);
	    fputs (" * with til2xpm (Marcello Sega 2002) */\n\n", cid2);
	    fprintf (cid2, "static char *%s [] = {\n", name);
	    fprintf (cid2, "\"%d %d %d %d\",\n", 64, hgt, kk + 1, 3);	/* width, height, n. of colors, chars per pixel.
									 * kk+1 because of the None color (see below)
									 */

	    fprintf (cid2, "\"... c None\",\n");	/* None Color */

	    for (k = 0; k < kk; k++)	/* print palette */
	    {
		fprintf (cid2, "\"%.3d c #%.2x%.2x%.2x\",\n", xpalette[k],
			 palette[3 * xpalette[k]],
			 palette[3 * xpalette[k] + 1],
			 palette[3 * xpalette[k] + 2]);
	    }
	    k = 0;
	    j = 0;
	    pixel = 0;
	    fprintf (cid2, "\"");
	    while (j < ndt)	/* convert  data */
	    {
		if (pixel > 64)
		    exit (printf
			  ("\nerror: no code \\02 at end of line found in .til\n\n"));
		byte = fgetc (cid1);
		j++;
		switch (byte)
		{
		case 0:	//print next tmp pixels
		    tmp = fgetc (cid1);
		    j++;
		    pixel += tmp;
		    for (k = 0; k < tmp; k++)
		    {
			byte = fgetc (cid1);
			j++;
			fprintf (cid2, "%.3d", byte);
		    }
		    break;
		case 1:	//don't print next tmp pixel 
		    tmp = fgetc (cid1);
		    j++;
		    pixel += tmp;
		    for (k = 0; k < tmp; k++)
			fprintf (cid2, "...");
		    break;
		case 2:	//new line !
		    if (pixel < 64)
		    {
			tmp = pixel % 64;
			while (tmp < 64)
			{
			    tmp++;
			    fprintf (cid2, "...");
			}
		    }
		    fprintf (cid2, "\",\n\"");
		    pixel = 0;
		    break;
		case 3:
		    break;	//don't know how to handle ...

		}
	    }
	    fseek (cid2, -3, SEEK_CUR);	//eliminate last  ,\n"
	    fprintf (cid2, "\n};\n");

	}
    }
    fclose (cid1);
    fclose (cid2);
    return 0;
}
