/*
 * compres1.c - data compression program
 */
#include "angband.h"

# define USERMEM  450000   /* default user memory */
# define BITS  12
# define HSIZE 5003        /* 80% occupancy */

/* Defines for third byte of header */
#define BIT_MASK           0x1f
#define BLOCK_MASK         0x80
/* Masks 0x40 and 0x20 are free.  I think 0x20 should mean that there is
   a fourth header byte (for expansion).
*/
#define INIT_BITS 9        /* initial number of bits/code */

/*
 * compress.c - File compression ala IEEE Computer, June 1984.
 *
 * Authors: Spencer W. Thomas (decvax!harpo!utah-cs!utah-gr!thomas)
 *          Jim McKie         (decvax!mcvax!jim)
 *          Steve Davies      (decvax!vax135!petsd!peora!srd)
 *          Ken Turkowski     (decvax!decwrl!turtlevax!ken)
 *          James A. Woods    (decvax!ihnp4!ames!jaw)
 *          Joe Orost         (decvax!vax135!petsd!joe)
 *
 */

int n_bits;                    /* number of bits/code */
int maxbits = BITS;            /* user settable max # bits/code */
int maxcode;                   /* maximum code, given n_bits */
int maxmaxcode = 1L << BITS;   /* should NEVER generate this code */
# define MAXCODE(n_bits)       ((1L << (n_bits)) - 1)

long int htab [HSIZE];
unsigned short codetab [HSIZE];

#define htabof(i) htab[i]
#define codetabof(i) codetab[i]
int hsize = HSIZE;             /* for dynamic table sizing */
long int compress_total;

/*
 * To save much memory, we overlay the table used by compress() with those
 * used by decompress().  The tab_prefix table is the same size and type
 * as the codetab.  The tab_suffix table needs 2**BITS characters.  We
 * get this from the beginning of htab.  The output stack uses the rest
 * of htab, and contains characters.  There is plenty of room for any
 * possible stack (stack used to be 8000 characters).
 */

#define tab_prefixof(i)    codetabof(i)
# define tab_suffixof(i)   ((unsigned char *)(htab))[i]
# define de_stack          ((unsigned char *)&tab_suffixof(1<<BITS))

int free_ent = 0;          /* first unused entry */
int exit_stat = 0;

int quiet = 1;             /* don't tell me about compression */

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
int block_compress = BLOCK_MASK;
int clear_flg = 0;
long int ratio = 0;
#define CHECK_GAP 10000 /* ratio check interval */
long int checkpoint = CHECK_GAP;
/*
 * the next two codes should not be changed lightly, as they must not
 * lie within the contiguous general code space.
 */
#define FIRST  257   /* first free entry */
#define  CLEAR 256   /* table clear output code */

int do_decomp = 0;
static int offset;
long int in_count = 1;        /* length of input */
long int bytes_out;        /* length of compressed output */
long int out_count = 0;       /* # of codes output (for debugging) */

void cl_hash(long int hsize)    /* reset code table */
{
   long int *htab_p = (long int*)htab+hsize;
   long i;
   long m1 = -1;

   i = hsize - 16;
   /* might use Sys V memset(3) here */
   do
   {
      *(htab_p-16) = m1;
      *(htab_p-15) = m1;
      *(htab_p-14) = m1;
      *(htab_p-13) = m1;
      *(htab_p-12) = m1;
      *(htab_p-11) = m1;
      *(htab_p-10) = m1;
      *(htab_p-9) = m1;
      *(htab_p-8) = m1;
      *(htab_p-7) = m1;
      *(htab_p-6) = m1;
      *(htab_p-5) = m1;
      *(htab_p-4) = m1;
      *(htab_p-3) = m1;
      *(htab_p-2) = m1;
      *(htab_p-1) = m1;
      htab_p -= 16;
   }
   while ((i -= 16) >= 0);
   for ( i += 16; i > 0; i-- )
   {
      *--htab_p = m1;
   }
}

static char buf[BITS];

unsigned char lmask[9] = {0xff, 0xfe, 0xfc, 0xf8, 0xf0, 0xe0, 0xc0, 0x80, 0x00};
unsigned char rmask[9] = {0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff};

/*****************************************************************
 * TAG( output )
 *
 * Output the given code.
 * Inputs:
 *    code: A n_bits-bit integer.  If == -1, then EOF.  This assumes
 *    that n_bits =< (long)wordsize - 1.
 * Outputs:
 *    Outputs code to the file.
 * Assumptions:
 * Chars are 8 bits long.
 * Algorithm:
 *    Maintain a BITS character long buffer (so that 8 codes will
 * fit in it exactly).  Use the VAX insv instruction to insert each
 * code in turn.  When the buffer fills up empty it and start over.
 */

int output( FILE *fout, int code )
{
   int r_off = offset, bits= n_bits;
   char * bp = buf;

   if ( code >= 0 )
   {
      /*
       * Get to the first byte.
       */
      bp += (r_off >> 3);
      r_off &= 7;
      /*
       * Since code is always >= 8 bits, only need to mask the first
       * hunk on the left.
       */
      *bp = (*bp & rmask[r_off]) | ((code << r_off) & lmask[r_off]);
      bp++;
      bits -= (8 - r_off);
      code >>= 8 - r_off;
      /* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
      if ( bits >= 8 )
      {
         *bp++ = code;
         code >>= 8;
         bits -= 8;
      }
      /* Last bits. */
      if(bits)
      {
         *bp = code;
      }
      offset += n_bits;
      if ( offset == (n_bits << 3) )
      {
         bp = buf;
         bits = n_bits;
         bytes_out += bits;
         do
         {
            fputc(*bp++, fout);
         }
         while(--bits);
         offset = 0;
      }

      /*
       * If the next entry is going to be too big for the code size,
       * then increase it, if possible.
       */
      if ( free_ent > maxcode || (clear_flg > 0))
      {
         /*
          * Write the whole buffer, because the input side won't
          * discover the size increase until after it has read it.
          */
         if ( offset > 0 )
         {
            (void)fwrite( buf, 1, n_bits, fout );
            bytes_out += n_bits;
         }
         offset = 0;

         if ( clear_flg )
         {
            maxcode = MAXCODE (n_bits = INIT_BITS);
            clear_flg = 0;
         }
         else
         {
            n_bits++;
            if ( n_bits == maxbits )
            {
               maxcode = maxmaxcode;
            }
            else
            {
               maxcode = MAXCODE(n_bits);
            }
         }
      }
   }
   else
   {
      /*
       * At EOF, write the rest of the buffer.
       */
      if ( offset > 0 )
      {
         fwrite( buf, 1, (offset + 7) / 8, fout );
      }
      bytes_out += (offset + 7) / 8;
      offset = 0;
      fflush( fout );
   }
   return (0);
}

/* table clear for block compress */
void cl_block(FILE *fout)
{
   long int rat;

   checkpoint = in_count + CHECK_GAP;

   /* shift will overflow */
   if(in_count > 0x007fffff)
   {
      rat = bytes_out >> 8;
      /* Don't divide by zero */
      if(rat == 0)
      {
         rat = 0x7fffffff;
      }
      else
      {
         rat = in_count / rat;
      }
   }
   else
   {
      rat = (in_count << 8) / bytes_out;  /* 8 fractional bits */
   }
   if ( rat > ratio )
   {
      ratio = rat;
   }
   else
   {
      ratio = 0;
      cl_hash ( (long int) hsize );
      free_ent = FIRST;
      clear_flg = 1;
      output (fout, (int) CLEAR );
   }
}

/*
 * compress
 *
 * Algorithm:  use open addressing double hashing (no chaining) on the
 * prefix code / next character combination.  We do a variant of Knuth's
 * algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
 * secondary probe.  Here, the modular division first probe is gives way
 * to a faster exclusive-or manipulation.  Also do block compression with
 * an adaptive reset, whereby the code table is cleared when the compression
 * ratio decreases, but after the table fills.  The variable-length output
 * codes are re-sized at this point, and a special CLEAR code is generated
 * for the decompressor.  Late addition:  construct the table according to
 * file size for noticeable speed improvement on small files.  Please direct
 * questions about this implementation to ames!jaw.
 */

int compress(FILE *fin, FILE *fout)
{
   long int fcode;
   int i = 0;
   int c;
   int ent;
   int disp;
   int hsize_reg;
   int hshift;

   offset = 0;
   bytes_out = 3;      /* includes 3-byte header mojo */
   out_count = 0;
   clear_flg = 0;
   ratio = 0;
   in_count = 1;
   checkpoint = CHECK_GAP;
   maxcode = MAXCODE(n_bits = INIT_BITS);
   free_ent = ((block_compress) ? FIRST : 256 );

   /* write special header */
   fputc('Z', fout);
   fputc('6', fout);
   fputc('4', fout);
   fputc('1', fout);

   ent = fgetc(fin);

   hshift = 0;
   for ( fcode = (long) hsize;  fcode < 65536L; fcode *= 2L )
   {
      hshift++;
   }
   hshift = 8 - hshift;      /* set hash code range bound */

   hsize_reg = hsize;
   cl_hash( (long int) hsize_reg);      /* clear hash table */

   while ( (c = fgetc(fin)) != EOF )
   {
      in_count++;
      compress_progress_indicator(1, in_count, compress_total);
      fcode = (long) (((long) c << maxbits) + ent);
      i = (((long)c << hshift) ^ ent); /* xor hashing */

      if ( htabof (i) == fcode )
      {
         ent = codetabof (i);
         continue;
      }
      /* empty slot */
      else if ( (long)htabof (i) < 0 )
      {
         goto nomatch;
      }
      disp = hsize_reg - i;      /* secondary hash (after G. Knott) */
      if ( i == 0 )
      {
         disp = 1;
      }

probe:
      if ( (i -= disp) < 0 )
      {
         i += hsize_reg;
      }

      if ( htabof (i) == fcode )
      {
         ent = codetabof (i);
         continue;
      }
      if ( (long)htabof (i) > 0 )
      {
         goto probe;
      }

nomatch:
      output ( fout, (int) ent );
      out_count++;
      ent = c;
      if ( free_ent < maxmaxcode )
      {
         codetabof (i) = free_ent++;  /* code -> hashtable */
         htabof (i) = fcode;
      }
      else if ( (long int)in_count >= checkpoint && block_compress )
      {
         cl_block (fout);
      }
   }
   /*
    * Put out the final code.
    */
   output( fout, (int)ent );
   out_count++;
   output( fout, (int)-1 );

   if(bytes_out > in_count)  /* exit(2) if no savings */
   {
      exit_stat = 2;
   }
   return (0);
}

/*****************************************************************
 * TAG( getcode )
 *
 * Read one code from the standard input.  If EOF, return -1.
 * Inputs:
 *    FILE *fin
 * Outputs:
 *    code or -1 is returned.
 */

int getcode(FILE *fin)
{
   int code;
   static int offset = 0, size = 0;
   static unsigned char buf[BITS*2];
   int r_off, bits;
   unsigned char *bp = buf;

   if ( clear_flg > 0 || offset >= size || free_ent > maxcode )
   {
      /*
       * If the next entry will be too big for the current code
       * size, then we must increase the size.  This implies reading
       * a new buffer full, too.
       */
      if ( free_ent > maxcode )
      {
         n_bits++;
         if ( n_bits == maxbits )
         {
            maxcode = maxmaxcode;   /* won't get any bigger now */
         }
         else
         {
            maxcode = MAXCODE(n_bits);
         }
      }
      if ( clear_flg > 0)
      {
         maxcode = MAXCODE (n_bits = INIT_BITS);
         clear_flg = 0;
      }
      size = fread( buf, 1, n_bits, fin );
      in_count+=n_bits;
      compress_progress_indicator(0, in_count, compress_total);
      if ( size <= 0 )
      {
         return -1;       /* end of file */
      }
      offset = 0;
      /* Round size down to integral number of codes */
      size = (size << 3) - (n_bits - 1);
   }
   r_off = offset;
   bits = n_bits;
   /*
    * Get to the first byte.
    */
   bp += (r_off >> 3);
   r_off &= 7;
   /* Get first part (low order bits) */
   code = (*bp++ >> r_off);
   bits -= (8 - r_off);
   r_off = 8 - r_off;      /* now, offset into code word */
   /* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
   if ( bits >= 8 )
   {
      code |= *bp++ << r_off;
      r_off += 8;
      bits -= 8;
   }
   /* high order bits. */
   code |= (*bp & rmask[bits]) << r_off;
   offset += n_bits;

   return code;
}

/*
 * Decompress.  This routine adapts to the codes in the
 * file building the "string" table on-the-fly; requiring no table to
 * be stored in the compressed file.  The tables used herein are shared
 * with those of the compress() routine.  See the definitions above.
 */
void decompress(FILE *fin, FILE *fout)
{
   unsigned char *stackp;
   int finchar;
   int code, oldcode, incode;
   char tmp1, tmp2, tmp3, tmp4;

   tmp1=(char)fgetc(fin);
   tmp2=(char)fgetc(fin);
   tmp3=(char)fgetc(fin);
   tmp4=(char)fgetc(fin);
   if ( ((char)tmp1 != 'Z') ||
        ((char)tmp2 != '6') ||
        ((char)tmp3 != '4') ||
        ((char)tmp3 != '1'))
   {
      return;
   }
   /*
    * As above, initialize the first 256 entries in the table.
    */
   maxcode = MAXCODE(n_bits = INIT_BITS);
   for ( code = 255; code >= 0; code-- )
   {
      tab_prefixof(code) = 0;
      tab_suffixof(code) = (unsigned char)code;
   }
   free_ent = ((block_compress) ? FIRST : 256 );


   finchar = oldcode = getcode(fin);
   if(oldcode == -1)   /* EOF already? */
   {
      return;        /* Get out of here */
   }
   fputc( (char)finchar, fout );    /* first code must be 8 bits = char */
   stackp = de_stack;

   in_count=0;
   while ( (code = getcode(fin)) > -1 )
   {
      if ( (code == CLEAR) && block_compress )
      {
         for ( code = 255; code >= 0; code-- )
         {
            tab_prefixof(code) = 0;
         }
         clear_flg = 1;
         free_ent = FIRST - 1;
         /* O, untimely death! */
         if ( (code = getcode (fin)) == -1 )
         {
            break;
         }
      }
      incode = code;
      /*
       * Special case for KwKwK string.
       */
      if ( code >= free_ent )
      {
         *stackp++ = finchar;
         code = oldcode;
      }

      /*
       * Generate output characters in reverse order
       */
      while ( code >= 256 )
      {
         *stackp++ = tab_suffixof(code);
         code = tab_prefixof(code);
      }
      *stackp++ = finchar = tab_suffixof(code);

      /*
       * And put them out in forward order
       */
      do
      {
         fputc ( *--stackp , fout);
      }
      while ( stackp > de_stack );

      /*
       * Generate the new entry.
       */
      if ( (code=free_ent) < maxmaxcode )
      {
         tab_prefixof(code) = (unsigned short)oldcode;
         tab_suffixof(code) = finchar;
         free_ent = code+1;
      }
      /*
       * Remember previous code.
       */
      oldcode = incode;
   }
   fflush( fout );
}

/*****************************************************************
 * TAG( main )
 *
 * Algorithm from "A Technique for High Performance Data Compression",
 * Terry A. Welch, IEEE Computer Vol 17, No 6 (June 1984), pp 8-19.
 *
 * Algorithm:
 *    Modified Lempel-Ziv method (LZW).  Basically finds common
 * substrings and replaces them with a variable size code.  This is
 * deterministic, and can be done on the fly.  Thus, the decompression
 * procedure needs no input table, but tracks the way the table was built.
 */

/* action = 0 compress, action = 1 decompress */
int lzw_compress_file(int action, const char *infilename, const char *outfilename)
{
   char *rindex();
   FILE *fin, *fout, *fff;

   do_decomp = action;

   if(maxbits < INIT_BITS) maxbits = INIT_BITS;
   if (maxbits > BITS) maxbits = BITS;
   maxmaxcode = 1L << maxbits;
   exit_stat = 0;

dlog(DEBUGCMPRS,"compress.c: lzw_compress_file: in %s out %s\n", infilename, outfilename);
   compress_total = 0;
   fff=fopen(infilename, "rb");
   if (!fff) return(-1);
   (void)fseek(fff, 0, SEEK_END);
   compress_total=ftell(fff);
dlog(DEBUGCMPRS,"compress.c: lzw_compress_file: infile length %ld\n", compress_total);
   (void)fclose(fff);

   /*
    * tune hash table size for small files -- ad hoc,
    * but the sizes match earlier #defines, which
    * serve as upper bounds on the number of output codes.
    */
   hsize = HSIZE;
   if ( compress_total < (1 << 12) )
       hsize = min ( 5003, HSIZE );
   else if ( compress_total < (1 << 13) )
       hsize = min ( 9001, HSIZE );
   else if ( compress_total < (1 << 14) )
       hsize = min ( 18013, HSIZE );
   else if ( compress_total < (1 << 15) )
       hsize = min ( 35023, HSIZE );
   else if ( compress_total < 47000 )
       hsize = min ( 50021, HSIZE );

   /* Actually do the compression/decompression */
   fin=fopen(infilename, "rb");
   if (!fin) return(-1);
   fout=fopen(outfilename, "wb");
   if (!fout) return(-1);
   if (do_decomp == 0)
   {
      compress(fin, fout);
   }
   else
   {
      decompress(fin, fout);
   }
   (void)fseek(fin, 0, SEEK_END);
   compress_total=ftell(fin);
dlog(DEBUGCMPRS,"compress.c: lzw_compress_file: infile length %ld\n",
     compress_total);
   (void)fclose(fin);
   (void)fseek(fout, 0, SEEK_END);
   compress_total=ftell(fout);
dlog(DEBUGCMPRS,"compress.c: lzw_compress_file: outfile length %ld\n",
     compress_total);
   (void)fclose(fout);

dlog(DEBUGCMPRS,"compress.c: lzw_compress_file: returning OK\n");
   return(0);
}
