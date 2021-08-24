#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int minoffset;
static int maxlength;
static int lines;
static int vaults;
static char out[256][256];
static int mostlines, mostcols;

void addline(char *line, int offset)
{
	lines++;
	strcpy(out[lines], line);
	if (minoffset > offset) {
		minoffset = offset;
	}
	int length = strlen(line);
	if (maxlength < length) {
		maxlength = length;
	}
	out[lines][length-1] = ' ';
	out[lines][length] = 0;
}

int main(int argc, char **argv)
{
	char buf[256];
	FILE *zv = fopen("Zorbus_Vaults.txt", "r");
	FILE *av = fopen("vaults-out.txt", "w");
	if ((!zv) || (!av)) {
		fprintf(stderr,"Unable to open files\n");
		exit(2);
	}
	do {
		char *left;
		fgets(buf, sizeof(buf), zv);
		buf[sizeof(buf)-1] = 0;
		buf[strlen(buf)-1] = 0;
		if (left = strchr(buf, '#')) {
			// vault
			addline(buf, left-buf);
		} else {
			// outside vault
			maxlength++;
			
			// move to 1 space from the edge (H and V)
			for(int i=1;i<=lines;i++) {
				memmove(&out[i][1], &out[i][minoffset], (maxlength-minoffset)+1);
			}
			
			// fill edge with space
			for(int i=1;i<=lines;i++) {
				out[i][0] = ' ';
				out[i][maxlength] = ' ';
			}
			for(int i=0;i<=maxlength;i++) {
				out[0][i] = ' ';
				out[lines+1][i] = ' ';
			}
			
			// any # touching space is %
			for(int y=1;y<=lines;y++) {
				for(int x=1;x<maxlength;x++) {
					if (out[y][x] == '#') {
						if ((out[y][x-1] == ' ') || (out[y][x+1] == ' ') || 
							(out[y+1][x-1] == ' ') || (out[y+1][x+1] == ' ') || 
							(out[y-1][x-1] == ' ') || (out[y-1][x+1] == ' ') || 
							(out[y+1][x] == ' ') || (out[y-1][x] == ' ')) {
								out[y][x] = '%';
						}
					}
				}
			}
			
			// add trailing space
			for(int i=1;i<=lines;i++) {
				out[i][strlen(out[i]+1)] = 0;
				int length = strlen(out[i]+1);
				memset(&(out[i][length+1]), ' ', maxlength-minoffset);
				out[i][(maxlength-minoffset)-1] = 0;
			}

			// write header and body
			if (lines > 0) {
				vaults++;
				fprintf(av, "\nname:Zorbus %d\n", vaults);
				fprintf(av, "type:Shaped room\n");
				fprintf(av, "rating:0\n");
				fprintf(av, "rows:%d\n", lines);
				fprintf(av, "columns:%d\n", (maxlength-minoffset)-2);
				fprintf(av, "min-depth:0\n");
				fprintf(av, "max-depth:0\n");
				
				if (mostlines < lines) {
					mostlines = lines;
				}
				if (mostcols < (maxlength-minoffset)-2) {
					mostcols = (maxlength-minoffset)-2;
				}
				
				for(int i=1;i<=lines;i++) {
					fprintf(av, "D:%s\n", &out[i][1]);
				}
			}
			
			minoffset = 256;
			maxlength = 0;
			lines = 0;
		}
	} while (!feof(zv));
	fclose(zv);
	fclose(av);
	fprintf(stderr,"Most rows: %d\nMost cols:%d\n", mostlines,mostcols);
}
