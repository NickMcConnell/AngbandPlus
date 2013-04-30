/* dr_rdppm.c
 *
 * Copyright (c) 2003 Hansjörg Malthaner
 * hansjoerg.malthaner@gmx.de
 *
 * This file is part of the Simugraph graphics engine.
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
#include <stdlib.h>                     
#include <string.h>
#include "dr_rdppm.h"

typedef int INT32;
                               
static void
read_ppm_body_plain(unsigned char *block, FILE *file, const INT32 breite, const INT32 hoehe)
{
    INT32 x,y;

    for(y=0;y<hoehe;y++) {
	for(x=0;x<breite;x++) {
	    *block++ = fgetc(file);
	    *block++ = fgetc(file);
	    *block++ = fgetc(file);
	}
    }
}

int
load_block(unsigned char *block, char *filename)
{                      
    char ID[2];

    FILE *file = fopen(filename, "rb");

    if(file == NULL) {
	perror("Error");
	printf("->%s\n", filename);
	exit(1);
    }

    
    ID[0] = fgetc( file );         /* ID sollte P6 sein */
    ID[1] = fgetc( file );

    


    if( strncmp(ID,"P6",2) == 0 ) {
	char dummy[256];
	INT32 breite,hoehe,tiefe;

	fgetc( file );             /* return schlucken */
	do {
	    fgets(dummy,250,file);
	} while(dummy[0] == '#');

	sscanf(dummy, "%d %d", &breite, &hoehe);
        fgets(dummy, 250,file);
        sscanf(dummy , "%d\n",&tiefe);

	printf("Dateigeometrie ist : %dx%dx%d\n", breite, hoehe, tiefe);

        
	read_ppm_body_plain(block, file, breite, hoehe);

	return 1;
    } else {  
	puts("Keine PPM (P6) Datei !");

	return 0;
    }
}  
