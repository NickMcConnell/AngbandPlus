/*
 * compres2.c - data compression program (simple RLE)
 */
#include "angband.h"

u32b rle_compress_total;

#define RLE_MARKER 253

void rle_compress(FILE *fin, FILE *fout)
{
   u32b n = 0;
   s16b prev, cur;
   s16b cnt;
   /* RLE_MARKER <u8b cnt > <u8b byte> */
   cur = (s16b) getc(fin);
   n++;
   prev = -100;
   cnt=0;
   /* write out the marker: Z64, type 2 */
   fputc('Z', fout);
   fputc('6', fout);
   fputc('4', fout);
   fputc('2', fout);
   
   while (cur != EOF)
   {
      if ( ( n % 1024) == 0) compress_progress_indicator(1, n, rle_compress_total);
      /* a new byte? or cnt = 255? */
      if ( (cur != prev) || (cnt == 255) )
      {
         if (prev == -100)
         {
            prev = cur;
            cnt = 1;
         } 
         /* write it out with cnt */
         else if ( (cnt > 1) || (prev == RLE_MARKER) )
         {
            fputc((int)RLE_MARKER, fout);
            fputc((int)cnt, fout);
            fputc((int)prev, fout);
            cnt=1;
            prev=cur;
         }
         else
         /* write it out as a normal character */
         {
            fputc((int)prev, fout);
            cnt=1;
            prev=cur;
         }
      }
      else
      {
         cnt++;
      } 
      cur = getc(fin);
      n++;
   }
   /* now write out the last char */
   if (prev == RLE_MARKER)
   {
      putc((int)RLE_MARKER, fout);
      putc((int)0, fout);
      putc((int)prev, fout);
   }
   else
   /* write it out as a normal character */
   {
      putc((int)prev, fout);
   }
}
    
void rle_decompress(FILE *fin, FILE *fout)
{
   u32b n=0;
   s16b cur, i;
   s16b tmp1, tmp2, tmp3, tmp4;
   s16b cnt;
   /* RLE_MARKER <u8b cnt > <u8b byte> */

   tmp1=(char)fgetc(fin);
   tmp2=(char)fgetc(fin);
   tmp3=(char)fgetc(fin);
   tmp4=(char)fgetc(fin);
   if ( ((char)tmp1 != 'Z') ||
        ((char)tmp2 != '6') ||
        ((char)tmp3 != '4') ||
        ((char)tmp4 != '2'))
   {
dlog(DEBUGCMPRS,"compres2.c: rle_decompress: signature doesn't match (%x %x %x %x = %c%c%c%c)\n",
                tmp1, tmp2, tmp3, tmp4, tmp1, tmp2, tmp3, tmp4);
      return;
   }
   /* RLE_MARKER <u8b cnt > <u8b byte> */
   cur = (s16b) getc(fin);
   n++;
   while (cur != EOF)
   {
      if ( ( n % 1024) == 0) compress_progress_indicator(0, n, rle_compress_total);
      if (cur == RLE_MARKER)
      {
         cnt = (s16b) getc(fin);
         cur = (s16b) getc(fin);
         for (i=0; i < cnt; i++)
         {
            putc((int)cur, fout);
         }
      }
      else
      {
         putc((int)cur, fout);
      }
      cur=(s16b)fgetc(fin);
      n++;
   }

}
   
/* action = 0 compress, action = 1 decompress */
int rle_compress_file(int action, const char *infilename, const char *outfilename)
{
   FILE *fin, *fout, *fff;

dlog(DEBUGCMPRS,"compres2.c.c: rle_compress_file: in %s out %s\n", infilename, outfilename);
   rle_compress_total = 0;
   fff=fopen(infilename, "rb");
   if (!fff) return(-1);
   (void)fseek(fff, 0, SEEK_END);
   rle_compress_total=ftell(fff);
dlog(DEBUGCMPRS,"compres2.c: rle_compress_file: infile %s length %ld\n", infilename, rle_compress_total);
   (void)fclose(fff);

   fin=fopen(infilename, "rb");
   if (!fin) return(-1);
   fout=fopen(outfilename, "wb");
   if (!fout) return(-1);
   if (action == 0)
   {
      rle_compress(fin, fout);
   }
   else
   {
      rle_decompress(fin, fout);
   }
   (void)fseek(fin, 0, SEEK_END);
   rle_compress_total=ftell(fin);
dlog(DEBUGCMPRS,"compres2.c: rle_compress_file: infile %s length %ld\n", infilename, rle_compress_total);
   (void)fclose(fin);
   (void)fseek(fout, 0, SEEK_END);
   rle_compress_total=ftell(fout);
dlog(DEBUGCMPRS,"compres2.c: rle_compress_file: outfile %s length %ld\n", outfilename, rle_compress_total);
   (void)fclose(fout);

dlog(DEBUGCMPRS,"compres2.c: rle_compress_file: returning OK\n");
   return(0);
}
