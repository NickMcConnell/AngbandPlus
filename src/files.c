/* File: files.c */

/* Purpose: code dealing with files (and death) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#ifdef DEBUGDUMP
#include "execinfo.h"
/* this is taken from binutils' addr2line from the GNU suite */
/* note that these global variables are needed to let some   */
/* functions communicate with each other via other functions */
#include "bfd.h"
static asymbol    **dump_syms;		/* Symbol table.  */
static bfd_vma      dump_pc;
static const char  *dump_filename;
static const char  *functionname;
bool                dump_found;
unsigned int        dump_line;
void * xmalloc(size_t size);
#endif

/*
 * You may or may not want to use the following "#undef".
 */
/* #undef _POSIX_SAVED_IDS */

/*
 * Hack -- drop permissions
 */
void safe_setuid_drop(void)
{

#ifdef SET_UID
# ifdef SAFE_SETUID
#  ifdef SAFE_SETUID_POSIX

   if (setuid(getuid()) != 0)
   {
      quit("setuid(): cannot set permissions correctly!");
   }
   if (setgid(getgid()) != 0)
   {
      quit("setgid(): cannot set permissions correctly!");
   }

#  else

   if (setreuid(geteuid(), getuid()) != 0)
   {
      quit("setreuid(): cannot set permissions correctly!");
   }
   if (setregid(getegid(), getgid()) != 0)
   {
      quit("setregid(): cannot set permissions correctly!");
   }

#  endif
# endif
#endif
}

/*
 * Hack -- grab permissions
 */
void safe_setuid_grab(void)
{

#ifdef SET_UID

# ifdef SAFE_SETUID

#  ifdef SAFE_SETUID_POSIX

   if (setuid(player_euid) != 0)
   {
      quit("setuid(): cannot set permissions correctly!");
   }
   if (setgid(player_egid) != 0)
   {
      quit("setgid(): cannot set permissions correctly!");
   }

#  else

   if (setreuid(geteuid(), getuid()) != 0)
   {
      quit("setreuid(): cannot set permissions correctly!");
   }
   if (setregid(getegid(), getgid()) != 0)
   {
      quit("setregid(): cannot set permissions correctly!");
   }

#  endif

# endif

#endif

}

/*
 * Extract the first few "tokens" from a buffer
 *
 * This function uses "colon" and "slash" as the delimeter characters.
 *
 * We never extract more than "num" tokens.  The "last" token may include
 * "delimeter" characters, allowing the buffer to include a "string" token.
 *
 * We save pointers to the tokens in "tokens", and return the number found.
 *
 * Hack -- Attempt to handle the 'c' character formalism
 *
 * Hack -- An empty buffer, or a final delimeter, yields an "empty" token.
 *
 * Hack -- We will always extract at least one token
 */
s16b tokenize(char *buf, s16b num, char **tokens)
{
   s16b i = 0;

   char *s = buf;


   /* Process */
   while (i < num - 1)
   {
      char *t;

      /* Scan the string */
      for (t = s; *t; t++)
      {
         /* Found a delimiter */
         if ((*t == ':') || (*t == '/')) break;

         /* Handle single quotes */
         if (*t == '\'')
         {
            /* Advance */
            t++;

            /* Handle backslash */
            if (*t == '\\') t++;

            /* Require a character */
            if (!*t) break;

            /* Advance */
            t++;

            /* Hack -- Require a close quote */
            if (*t != '\'') *t = '\'';
         }

         /* Handle back-slash */
         if (*t == '\\') t++;
      }

      /* Nothing left */
      if (!*t) break;

      /* Nuke and advance */
      *t++ = '\0';

      /* Save the token */
      tokens[i++] = s;

      /* Advance */
      s = t;
   }

   /* Save the token */
   tokens[i++] = s;

   /* Number found */
   return (i);
}

/*
 * Parse a sub-file of the "extra info" (format shown below)
 *
 * Each "action" line has an "action symbol" in the first column,
 * followed by a colon, followed by some command specific info,
 * usually in the form of "tokens" separated by colons or slashes.
 *
 * Blank lines, lines starting with white space, and lines starting
 * with pound signs ("#") are ignored (as comments).
 *
 * Note the use of "tokenize()" to allow the use of both colons and
 * slashes as delimeters, while still allowing final tokens which
 * may contain any characters including "delimiters".
 *
 * Note the use of "strtol()" to allow all "integers" to be encoded
 * in decimal, hexidecimal, or octal form.
 *
 * Note that "monster zero" is used for the "player" attr/char, "object
 * zero" will be used for the "stack" attr/char, and "feature zero" is
 * used for the "nothing" attr/char.
 *
 * Specify the attr/char values for "monsters" by race index
 *   R:<num>:<a>/<c>
 *
 * Specify the attr/char values for "objects" by kind index
 *   K:<num>:<a>/<c>
 *
 * Specify the attr/char values for "features" by feature index
 *   F:mtyp:styp:<a>/<c>
 *
 * Specify the attr/char values for "special" things
 *   S:<num>:<a>/<c>
 *
 * Specify the attr/char values for unaware "objects" by kind tval
 *   U:<tv>:<a>/<c>
 *
 * Specify the attribute values for inventory "objects" by kind tval
 *   E:<tv>:<a>
 *
 * Define a macro action, given an encoded macro action
 *   A:<str>
 *
 * Create a macro, given an encoded macro trigger
 *   P:<str>
 *
 * Create a keymap, given an encoded keymap trigger
 *   C:<num>:<str>
 *
 * Turn an option off, given its name
 *   X:<str>
 *
 * Turn an option on, given its name
 *   Y:<str>
 *
 * Specify visual information, given an index, and some data
 *   V:<num>:<kv>:<rv>:<gv>:<bv>
 */
errr process_pref_file_aux(char *buf)
{
   int i, j, n1, n2;

   char *zz[16];


   /* Skip "empty" lines */
   if (!buf[0]) return (0);

   /* Skip "blank" lines */
   if (isspace((int)buf[0])) return (0);

   /* Skip comments */
   if (buf[0] == '#') return (0);


   /* Paranoia */
   if (strlen(buf) >= 1024) return (1);


   /* Require "?:*" format */
   if (buf[1] != ':') return (1);

   /* Process "R:<num>:<a>/<c>" -- attr/char for monster races */
   if (buf[0] == 'R')
   {
dlog(DEBUGPREF,"files.c: process_pref_file_aux: R: buf %s\n", buf);
      if (tokenize(buf+2, 3, zz) == 3)
      {
         monster_race *r_ptr;
         i = (huge)strtol(zz[0], NULL, 0);
         if ((i < 0) || (i >= r_number_total)) return (1);
         n1 = strtol(zz[1], NULL, 0);
         if ((n1 < 0) || (n1 > 255)) return (1);
         n2 = strtol(zz[2], NULL, 0);
         if ((n2 < 0) || (n2 > 255)) return (1);
         r_ptr = &r_info[i];
         if (n1) r_ptr->x_attr = n1;
         if (n2) r_ptr->x_char = n2;
dlog(DEBUGPREF,"files.c: process_pref_file_aux: R: done\n");
         return (0);
      }
   }


   /* Process "K:<num>:<a>/<c>"  -- attr/char for object kinds */
   else if (buf[0] == 'K')
   {
dlog(DEBUGPREF,"files.c: process_pref_file_aux: K: buf %s\n", buf);
      if (tokenize(buf+2, 3, zz) == 3)
      {
         object_kind *k_ptr;
         i = (huge)strtol(zz[0], NULL, 0);
         if ((i < 0) || (i >= k_number)) return (1);
         n1 = strtol(zz[1], NULL, 0);
         if ((n1 < 0) || (n1 > 255)) return (1);
         n2 = strtol(zz[2], NULL, 0);
         if ((n2 < 0) || (n2 > 255)) return (1);
         k_ptr = &k_info[i];
         if (n1) k_ptr->x_attr = n1;
         if (n2) k_ptr->x_char = n2;
dlog(DEBUGPREF,"files.c: process_pref_file_aux: K item %d tv %d sv %d x_attr %d x_char %d=%c \n",
               i, k_ptr->tval, k_ptr->sval, k_ptr->x_attr, k_ptr->x_char, k_ptr->x_char);
         return (0);
      }
   }


   /* Process "F:<start mtyp>:<end mtyp>:<start styp>:<end styp>:<a>/<c>"
    *  -- attr/char for terrain features
    */
   else if (buf[0] == 'F')
   {
dlog(DEBUGPREF,"files.c: process_pref_file_aux: F: buf %s\n", buf);
      if (tokenize(buf+2, 6, zz) == 6)
      {
         feature_type *f_ptr;
         s16b s_mtyp, e_mtyp, s_styp, e_styp, mtyp, styp;
         s_mtyp = strtol(zz[0], NULL, 0);
         if ((s_mtyp < 0) || (s_mtyp > MAX_F_IDX_MTYP))
         {
            msg_format("s_mtyp %d out of range (0-%d)", s_mtyp, MAX_F_IDX_MTYP);
            return (1);
         }
         e_mtyp = strtol(zz[1], NULL, 0);
         if ((e_mtyp < 0) || (e_mtyp > MAX_F_IDX_MTYP))
         {
            msg_format("e_mtyp %d out of range (0-%d)", e_mtyp, MAX_F_IDX_MTYP);
            return (1);
         }
         s_styp = strtol(zz[2], NULL, 0);
         if ((s_styp < 0) || (s_styp > MAX_F_IDX_STYP))
         {
            msg_format("s_styp %d out of range (0-%d)", s_styp, MAX_F_IDX_MTYP);
            return (1);
         }
         e_styp = strtol(zz[3], NULL, 0);
         if ((e_styp < 0) || (e_styp > MAX_F_IDX_STYP))
         {
            msg_format("e_styp %d out of range (0-%d)", e_styp, MAX_F_IDX_MTYP);
            return (1);
         }
         n1 = strtol(zz[4], NULL, 0);
         /* XXX hack - what's the coded maximum for chars/attrs? */
         if ((n1 < 0) || (n1 > 255))
         {
            msg_format("n1 %d out of range (0-255)", n1);
            return (1);
         }
         n2 = strtol(zz[5], NULL, 0);
         if ((n2 < 0) || (n2 > 255))
         {
            msg_format("n2 %d out of range (0-255)", n2);
            return (1);
         }
         for (mtyp = s_mtyp; mtyp <= e_mtyp; mtyp++)
         {
            for (styp = s_styp; styp <= e_styp; styp++)
            {
               f_ptr = &f_info[get_f_idx(mtyp, styp)];
dlog(DEBUGPREF,"files.c: process_pref_file_aux: setting mtyp %d styp %d to attr %d chr %d\n",
               mtyp, styp, n1, n2);
               if (n1) f_ptr->x_attr = n1;
               if (n2) f_ptr->x_char = n2;
            }
         }
         return (0);
      }
   }

   /* Process "S:<num>:<a>/<c>" -- attr/char for special things */
   /* these are only used in spells1.c: bolt_pict()             */
   else if (buf[0] == 'S')
   {
dlog(DEBUGPREF,"files.c: process_pref_file_aux: S: buf %s\n", buf);
      if (tokenize(buf+2, 3, zz) == 3)
      {
         j = (s16b)strtol(zz[0], NULL, 0);
         if (( j < 0) || (j > MAX_FLAVORS)) return (1);
         n1 = strtol(zz[1], NULL, 0);
         if (( n1 < 0) || (n1 > 255)) return (1);
         n2 = strtol(zz[2], NULL, 0);
         if (( n2 < 0) || (n2 > 255)) return (1);
         misc_to_attr[j] = n1;
         misc_to_char[j] = n2;
dlog(DEBUGPREF,"files.c: process_pref_file_aux: S: done, misc_to_char[%d]=%d, misc_to_attr[%d]=%d\n",
               j, n1, j, n2);
         return (0);
      }
   }


   /* Process "U:<tv>:<a>/<c>" -- attr/char for unaware items */
   else if (buf[0] == 'U')
   {
dlog(DEBUGPREF,"files.c: process_pref_file_aux: U: buf %s\n", buf);
      if (tokenize(buf+2, 3, zz) == 3)
      {
         j = (huge)strtol(zz[0], NULL, 0);
         if (( j < 0) || (j > MAX_TVAL)) return (1);
         n1 = strtol(zz[1], NULL, 0);
         if ((n1 < 0) || (n1 > 255)) return (1);
         n2 = strtol(zz[2], NULL, 0);
         if ((n2 < 0) || (n2 > 255)) return (1);
         for (i = 1; i < k_number; i++)
         {
            object_kind *k_ptr = &k_info[i];
            if (k_ptr->tval == j)
            {
               if (n1) k_ptr->d_attr = n1;
               if (n2) k_ptr->d_char = n2;
dlog(DEBUGPREF,"files.c: process_pref_file_aux: U item %d tv %d sv %d d_attr %d d_char %d=%c\n",
               i, k_ptr->tval, k_ptr->sval, k_ptr->d_attr, k_ptr->d_char, k_ptr->d_char);
            }
         }
dlog(DEBUGPREF,"files.c: process_pref_file_aux: U: done\n");
         return (0);
      }
   }


   /* Process "E:<tv>:<a>" -- attribute for inventory objects */
   else if (buf[0] == 'E')
   {
dlog(DEBUGPREF,"files.c: process_pref_file_aux: E: buf %s\n", buf);
      if (tokenize(buf+2, 2, zz) == 2)
      {
         j = (byte)strtol(zz[0], NULL, 0) % 128;
         if ((j < 0) || (j > 128)) return (1);
         n1 = strtol(zz[1], NULL, 0);
         if ((n1 < 0) || (n1 > 255)) return (1);
         if (n1) tval_to_attr[j] = n1;
dlog(DEBUGPREF,"files.c: process_pref_file_aux: E: done\n");
         return (0);
      }
   }


   /* Process "A:<str>" -- save an "action" for later */
   else if (buf[0] == 'A')
   {
dlog(DEBUGPREF,"files.c: process_pref_file_aux: A: buf %s\n", buf);
      text_to_ascii(macro_buffer, buf+2);
      return (0);
dlog(DEBUGPREF,"files.c: process_pref_file_aux: A: done\n");
   }

   /* Process "P:T/F:<str>" -- create macro */
   else if (buf[0] == 'P')
   {
      char tmp[1024];
dlog(DEBUGPREF,"files.c: process_pref_file_aux: P: buf %s\n", buf);
      text_to_ascii(tmp, buf+2);
      macro_add(tmp, macro_buffer);
dlog(DEBUGPREF,"files.c: process_pref_file_aux: P: done\n");
      return (0);
   }
   /* Process "C:<num>:<str>" -- create keymap */
   else if (buf[0] == 'C')
   {
      int mode;

      char tmp[1024];
dlog((DEBUGPREF|DEBUGKEYS),"files.c: process_pref_file_aux: C: buf %s\n", buf);

      if (tokenize(buf+2, 2, zz) != 2) return (1);

      mode = strtol(zz[0], NULL, 0);
      if ((mode < 0) || (mode >= KEYMAP_MODES)) return (1);

      text_to_ascii(tmp, zz[1]);
      if (!tmp[0] || tmp[1]) return (1);
      i = (byte)(tmp[0]);
      if ((i<0) || (i > 255)) return(1);

      string_free(keymap_act[mode][i]);

      keymap_act[mode][i] = string_make(macro_buffer);
dlog((DEBUGPREF|DEBUGKEYS),"files.c: process_pref_file_aux: C: done\n");

      return (0);
   }

   /* Process "V:<num>:<kv>:<rv>:<gv>:<bv>" -- visual info */
   else if (buf[0] == 'V')
   {
dlog(DEBUGPREF,"files.c: process_pref_file_aux: V: buf %s\n", buf);
      if (tokenize(buf+2, 5, zz) == 5)
      {
         i = (byte)strtol(zz[0], NULL, 0);
         if ((i<0) || (i>255)) return(1);
         angband_color_table[i][0] = (byte)strtol(zz[1], NULL, 0);
         angband_color_table[i][1] = (byte)strtol(zz[2], NULL, 0);
         angband_color_table[i][2] = (byte)strtol(zz[3], NULL, 0);
         angband_color_table[i][3] = (byte)strtol(zz[4], NULL, 0);
dlog(DEBUGPREF,"files.c: process_pref_file_aux: V: done\n");
         return (0);
      }
   }


   /* Process "X:<str>" -- turn option off */
   else if (buf[0] == 'X')
   {
dlog(DEBUGPREF,"files.c: process_pref_file_aux: X: buf %s\n", buf);
      for (i = 0; options[i].variable; i++)
      {
         if (options[i].descr && streq(options[i].descr, buf + 2))
         {
            *options[i].variable = FALSE;
dlog(DEBUGPREF,"files.c: process_pref_file_aux: X: done\n");
            return (0);
         }
      }
   }

   /* Process "Y:<str>" -- turn option on */
   else if (buf[0] == 'Y')
   {
dlog(DEBUGPREF,"files.c: process_pref_file_aux: Y: buf %s\n", buf);
      for (i = 0; options[i].variable; i++)
      {
         if (options[i].descr && streq(options[i].descr, buf + 2))
         {
            *options[i].variable = TRUE;
dlog(DEBUGPREF,"files.c: process_pref_file_aux: Y: done\n");
            return (0);
         }
      }
   }


   /* Failure */
   return (1);
}


/*
 * Helper function for "process_pref_file()"
 *
 * Input:
 *   v: output buffer array
 *   f: final character
 *
 * Output:
 *   result
 */
static cptr process_pref_file_expr(char **sp, char *fp)
{
   cptr v;

   char *b;
   char *s;

   char b1 = '[';
   char b2 = ']';

   char f = ' ';

   /* Initial */
   s = (*sp);

   /* Skip spaces */
   while (isspace((int)*s)) s++;

   /* Save start */
   b = s;

   /* Default */
   v = "?o?o?";

   /* Analyze */
   if (*s == b1)
   {
      const char *p;
      const char *t;

      /* Skip b1 */
      s++;

      /* First */
      t = process_pref_file_expr(&s, &f);

      /* Oops */
      if (!*t)
      {
         /* Nothing */
      }

      /* Function: IOR */
      else if (streq(t, "IOR"))
      {
         v = "0";
         while (*s && (f != b2))
         {
            t = process_pref_file_expr(&s, &f);
            if (*t && !streq(t, "0")) v = "1";
         }
      }

      /* Function: AND */
      else if (streq(t, "AND"))
      {
         v = "1";
         while (*s && (f != b2))
         {
            t = process_pref_file_expr(&s, &f);
            if (*t && streq(t, "0")) v = "0";
         }
      }

      /* Function: NOT */
      else if (streq(t, "NOT"))
      {
         v = "1";
         while (*s && (f != b2))
         {
            t = process_pref_file_expr(&s, &f);
            if (*t && !streq(t, "0")) v = "0";
         }
      }

      /* Function: EQU */
      else if (streq(t, "EQU"))
      {
         v = "1";
         if (*s && (f != b2))
         {
            t = process_pref_file_expr(&s, &f);
         }
         while (*s && (f != b2))
         {
            p = t;
            t = process_pref_file_expr(&s, &f);
            if (*t && !streq(p, t)) v = "0";
         }
      }

      /* Function: LEQ */
      else if (streq(t, "LEQ"))
      {
         v = "1";
         if (*s && (f != b2))
         {
            t = process_pref_file_expr(&s, &f);
         }
         while (*s && (f != b2))
         {
            p = t;
            t = process_pref_file_expr(&s, &f);
            if (*t && (strcmp(p, t) >= 0)) v = "0";
         }
      }

      /* Function: GEQ */
      else if (streq(t, "GEQ"))
      {
         v = "1";
         if (*s && (f != b2))
         {
            t = process_pref_file_expr(&s, &f);
         }
         while (*s && (f != b2))
         {
            p = t;
            t = process_pref_file_expr(&s, &f);
            if (*t && (strcmp(p, t) <= 0)) v = "0";
         }
      }

      /* Oops */
      else
      {
         while (*s && (f != b2))
         {
            t = process_pref_file_expr(&s, &f);
         }
      }

      /* Verify ending */
      if (f != b2) v = "?x?x?";

      /* Extract final and Terminate */
      if ((f = *s) != '\0') *s++ = '\0';
   }

   /* Other */
   else
   {
      /* Accept all printables except spaces and brackets */
      while (isprint((int)*s) && !strchr(" []", *s)) ++s;

      /* Extract final and Terminate */
      if ((f = *s) != '\0') *s++ = '\0';

      /* Variable */
      if (*b == '$')
      {
         /* System */
         if (streq(b+1, "SYS"))
         {
            v = ANGBAND_SYS;
         }

         /* Race */
         else if (streq(b+1, "RACE"))
         {
            v = rp_ptr->title;
         }

         /* Class */
         else if (streq(b+1, "CLASS"))
         {
            v = cp_ptr->title;
         }

         /* Player */
         else if (streq(b+1, "PLAYER"))
         {
            v = player_name;
         }
      }

      /* Constant */
      else
      {
         v = b;
      }
   }

   /* Save */
   (*fp) = f;

   /* Save */
   (*sp) = s;

   /* Result */
   return (v);
}


/*
 * Process the "user pref file" with the given name
 *
 * See the function above for a list of legal "commands".
 *
 * We also accept the special "?" and "%" directives, which
ow conditional evaluation and filename inclusion.
 */
errr process_pref_file(cptr name)
{
   FILE *fp;

   char buf[1024];

   char old[1024];

   int num = -1;

   errr err = 0;

   bool bypass = FALSE;


   /* Build the filename */
   path_build(buf, 1024, ANGBAND_DIR_USER, name);

   /* Open the file */
   fp = my_fopen(buf, "r");
dlog((DEBUGPREF|DEBUGKEYS),"files.c: process_pref_file: trying to open %s\n", buf);
   /* No such file */
   if (!fp) return (-1);
dlog((DEBUGPREF|DEBUGKEYS),"files.c: process_pref_file: opened %s\n", buf);

   /* Process the file */
   while (0 == my_fgets(fp, buf, 1024))
   {
      /* Count lines */
      num++;
dlog(DEBUGPREF,"files.c: process_pref_file: num %d line %s first char %d\n", num, buf, buf[0]);

      /* Skip "empty" lines */
      if (!buf[0]) continue;

      /* Skip "blank" lines */
      if (isspace((int)buf[0])) continue;

      /* Skip comments */
      if (buf[0] == '#') continue;

dlog(DEBUGPREF,"files.c: process_pref_file: normal line len %d\n", (u16b)strlen(buf));

      /* Save a copy */
      strcpy(old, buf);

      /* Process "?:<expr>" */
      if ((buf[0] == '?') && (buf[1] == ':'))
      {
         char f;
         cptr v;
         char *s;

         /* Start */
         s = buf + 2;

         /* Parse the expr */
dlog(DEBUGPREF,"files.c: process_pref_file: about to call process_pref_file_expr\n");
dlog(DEBUGPREF,"files.c: process_pref_file: f %d=%c s %s\n", f, f, s);

         v = process_pref_file_expr(&s, &f);

         /* Set flag */
         bypass = (streq(v, "0") ? TRUE : FALSE);

         /* Continue */
         continue;
      }

      /* Apply conditionals */
      if (bypass) continue;


      /* Process "%:<file>" */
      if (buf[0] == '%')
      {
         /* Process that file if allowed */
dlog((DEBUGPREF|DEBUGKEYS),"files.c: process_pref_file: about to open %s\n", buf+2);
         (void)process_pref_file(buf + 2);
dlog((DEBUGPREF|DEBUGKEYS),"files.c: process_pref_file: processed %s\n", buf+2);

         /* Continue */
         continue;
      }


      /* Process the line */
dlog(DEBUGPREF,"files.c: process_pref_file: about to process line %s\n", buf);
      err = process_pref_file_aux(buf);
dlog(DEBUGPREF,"files.c: process_pref_file: line %s processed\n", buf);

      /* Oops */
      if (err) break;
   }
dlog(DEBUGPREF,"process_pref_file: all lines processed\n");

   /* Error */
   if (err)
   {
      /* Useful error message */
      msg_format("Error %d in line %d of file '%s'.", err, num, name);
      msg_format("Parsing '%s'", old);
      msg_print(NULL);
   }

   /* Close the file */
   my_fclose(fp);

   /* Result */
dlog(DEBUGPREF,"process_pref_file: returning %d\n", err);
   return (err);
}

#ifdef CHECK_TIME

/*
 * Operating hours for ANGBAND (defaults to non-work hours)
 */
static char days[7][29] =
{
    "SUN:XXXXXXXXXXXXXXXXXXXXXXXX",
    "MON:XXXXXXXX.........XXXXXXX",
    "TUE:XXXXXXXX.........XXXXXXX",
    "WED:XXXXXXXX.........XXXXXXX",
    "THU:XXXXXXXX.........XXXXXXX",
    "FRI:XXXXXXXX.........XXXXXXX",
    "SAT:XXXXXXXXXXXXXXXXXXXXXXXX"
};

/*
 * Restict usage (defaults to no restrictions)
 */
static bool check_time_flag = FALSE;

#endif


/*
 * Handle CHECK_TIME
 */
errr check_time(void)
{

#ifdef CHECK_TIME

   time_t              c;
   struct tm           *tp;

   /* No restrictions */
   if (!check_time_flag) return (0);

   /* Check for time violation */
   c = time((time_t *)0);
   tp = localtime(&c);

   /* Violation */
   if (days[tp->tm_wday][tp->tm_hour + 4] != 'X') return (1);

#endif

   /* Success */
   return (0);
}



/*
 * Initialize CHECK_TIME
 */
errr check_time_init(void)
{

#ifdef CHECK_TIME

   FILE        *fp;

   char        buf[1024];


   /* Access the "time" file */
   strcpy(buf, ANGBAND_DIR_FILE);
   strcat(buf, "time.txt");

   /* Open the file */
   fp = my_fopen(buf, "r");

   /* No file, no restrictions */
   if (!fp) return (0);

   /* Assume restrictions */
   check_time_flag = TRUE;

   /* Parse the file */
   while (0 == my_fgets(fp, buf, 80))
   {
      /* Skip comments and blank lines */
      if (!buf[0] || (buf[0] == '#')) continue;

      /* Chop the buffer */
      buf[29] = '\0';

      /* Extract the info */
      if (prefix(buf, "SUN:")) strcpy(days[0], buf);
      if (prefix(buf, "MON:")) strcpy(days[1], buf);
      if (prefix(buf, "TUE:")) strcpy(days[2], buf);
      if (prefix(buf, "WED:")) strcpy(days[3], buf);
      if (prefix(buf, "THU:")) strcpy(days[4], buf);
      if (prefix(buf, "FRI:")) strcpy(days[5], buf);
      if (prefix(buf, "SAT:")) strcpy(days[6], buf);
   }

   /* Close it */
   my_fclose(fp);

#endif

   /* Success */
   return (0);
}



#ifdef CHECK_LOAD

#ifndef MAXHOSTNAMELEN
# define MAXHOSTNAMELEN  64
#endif

typedef struct statstime statstime;

struct statstime
{
   s16b                cp_time[4];
   s16b                dk_xfer[4];
   u16b                v_pgpgin;
   u16b                v_pgpgout;
   u16b                v_pswpin;
   u16b                v_pswpout;
   u16b                v_intr;
   s16b                if_ipackets;
   s16b                if_ierrors;
   s16b                if_opackets;
   s16b                if_oerrors;
   s16b                if_collisions;
   u16b                v_swtch;
   long                avenrun[3];
   struct timeval      boottime;
   struct timeval      curtime;
};

/*
 * Maximal load (if any).
 */
static s16b check_load_value = 0;

#endif

/*
 * Handle CHECK_LOAD
 */
errr check_load(void)
{

#ifdef CHECK_LOAD

   struct statstime    st;

   /* Success if not checking */
   if (!check_load_value) return (0);

   /* Check the load */
   if (0 == rstat("localhost", &st))
   {
      long val1 = (long)(st.avenrun[2]);
      long val2 = (long)(check_load_value) * FSCALE;

      /* Check for violation */
      if (val1 >= val2) return (1);
   }

#endif

   /* Success */
   return (0);
}


/*
 * Initialize CHECK_LOAD
 */
errr check_load_init(void)
{

#ifdef CHECK_LOAD

   FILE        *fp;

   char        buf[1024];

   char        temphost[MAXHOSTNAMELEN+1];
   char        thishost[MAXHOSTNAMELEN+1];


   /* Access the "load" file */
   strcpy(buf, ANGBAND_DIR_FILE);
   strcat(buf, "load.txt");

   /* Open the "load" file */
   fp = my_fopen(buf, "r");

   /* No file, no restrictions */
   if (!fp) return (0);

   /* Default load */
   check_load_value = 100;

   /* Get the host name */
   (void)gethostname(thishost, (sizeof thishost) - 1);

   /* Parse it */
   while (0 == my_fgets(fp, buf, 1024))
   {
      s16b value;

      /* Skip comments and blank lines */
      if (!buf[0] || (buf[0] == '#')) continue;

      /* Parse, or ignore */
      if (sscanf(buf, "%s%d", temphost, &value) != 2) continue;

      /* Skip other hosts */
      if (!streq(temphost,thishost) &&
          !streq(temphost,"localhost")) continue;

      /* Use that value */
      check_load_value = value;

      /* Done */
      break;
   }

   /* Close the file */
   my_fclose(fp);

#endif

   /* Success */
   return (0);
}

/*
 * Prints the following information on the screen.
 *
 * For this to look right, the following should be spaced the
 * same as in the prt_lnum code... -CFT
 */
static void display_player_middle(void)
{
   s16b base_tohit = p_ptr->dis_to_h;
   s16b base_todam = p_ptr->dis_to_d;
   s16b show_tohit1 = base_tohit, show_tohit2 = base_tohit;
   s16b show_todam1 = base_todam, show_todam2 = base_todam;

   s16b i = p_ptr->pspeed;

   s16b attr = TERM_WHITE;
   char buf[80] = "";

   object_type *i_ptr = &inventory[INVEN_WIELD];

   /* Hack -- add in weapon info if known */
   if (object_known_p(i_ptr)) show_tohit1 += i_ptr->to_h;
   if (object_known_p(i_ptr)) show_todam1 += i_ptr->to_d;

   if (p_ptr->twohands)
   {
      show_tohit1 += i_ptr->to_h / 2;
      show_todam1 += i_ptr->to_d / 2;
   }
   if ((p_ptr->pclass == CLASS_GLADIATR) &&
       (inventory[INVEN_ARM].k_idx!=0))
   {
      object_type *i_ptr = &inventory[INVEN_ARM];

      /* Hack -- add in weapon info if known */
      if (object_known_p(i_ptr)) show_tohit2 += i_ptr->to_h;
      if (object_known_p(i_ptr)) show_todam2 += i_ptr->to_d;

   }

   /* Dump the bonuses to hit/dam */
   c_put_str(TERM_WHITE, "To Hit:", 19, 2);
   if ((p_ptr->pclass == CLASS_GLADIATR) &&
       (inventory[INVEN_ARM].k_idx!=0))
   {
      c_put_str(TERM_L_BLUE, format("%7s", format("%d/%d", show_tohit1, show_tohit2)), 26, 2);
   }
   else
   {
      c_put_str(TERM_L_BLUE, format("%7s", format("%d", show_tohit1)), 26, 2);
   }

   c_put_str(TERM_WHITE, "To Dam:", 19, 3);
   if ((p_ptr->pclass == CLASS_GLADIATR) &&
       (inventory[INVEN_ARM].k_idx!=0))
   {
      if (p_ptr->dis_ring_to_d!=0)
      {
         c_put_str(TERM_L_BLUE, format("%7s+%d(=)",
                                       format("%d/%d", show_todam1, show_todam2),
                                       p_ptr->dis_ring_to_d), 26, 3);
      }
      else
      {
         c_put_str(TERM_L_BLUE,
                   format("%7s", format("%d/%d", show_todam1, show_todam2)), 26, 3);
      }
   }
   else
   {
      if (p_ptr->dis_ring_to_d!=0)
      {
         c_put_str(TERM_L_BLUE, format("%7d+%d(=)", show_todam1, p_ptr->dis_ring_to_d), 26, 3);
      }
      else
      {
         c_put_str(TERM_L_BLUE, format("%7d", show_todam1), 26, 3);
      }
   }

   /* Dump the armor class bonus */
   c_put_str(TERM_WHITE,"AC    :", 19, 4);
   c_put_str(TERM_L_BLUE, format("%7s", format("%d+%d", p_ptr->ac, p_ptr->to_a)), 26, 4);
   /* jk */
   /* Visually "undo" the Search Mode Slowdown */
   if (p_ptr->searching) i += 10;

   /* Fast */
   if (i > 110)
   {
      attr = TERM_L_GREEN;
      sprintf(buf, "Fast (+%d)", (i - 110));
   }

   /* Slow */
   else if (i < 110)
   {
      attr = TERM_L_RED;
      sprintf(buf, "Slow (-%d)", (110 - i));
   }
   /* normal */
   else
   {
      attr = TERM_L_BLUE;
      sprintf(buf, "Normal ");
   }
   c_put_str(TERM_WHITE, "Speed     :", 43, 2);
   c_put_str(attr, buf, 54, 2);

   /* Display extras */
   if (p_ptr->lev >= PY_MAX_LEVEL)
   {
      (void)sprintf(buf, "%2d (max)", p_ptr->lev);
   }
   else
   {
      (void)sprintf(buf, "%d (lev %d at %ld)", p_ptr->lev, p_ptr->lev+1,
                   (s32b)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L));
   }
   put_str("Level     :", 43, 6);
   c_put_str(TERM_L_GREEN, buf, 54, 6);

   c_put_str(TERM_WHITE, "Experience:", 43, 7);
   (void)sprintf(buf, "%ld", p_ptr->exp);
   if (p_ptr->exp >= p_ptr->max_exp)
   {
      c_put_str(TERM_L_GREEN, buf, 54, 7);
   }
   else
   {
      c_put_str(TERM_RED, buf, 54, 7);
   }
   i=55+strlen(buf);
   (void)sprintf(buf,"(%ld)", p_ptr->max_exp);
   c_put_str(TERM_L_BLUE, buf, i, 7);

   c_put_str(TERM_WHITE, "Gold      :", 43, 3);
   c_put_str(TERM_L_GREEN, format("%ld", p_ptr->au), 54, 3);

   put_str("HP    :", 19, 5);
   (void)sprintf(buf, "%7d", (int) p_ptr->chp);
   if (p_ptr->chp >= p_ptr->mhp)
   {
      c_put_str(TERM_L_GREEN, buf, 26, 5);
   }
   else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10)
   {
      c_put_str(TERM_YELLOW, buf, 26, 5);
   }
   else
   {
      c_put_str(TERM_RED, buf, 26, 5);
   }
   (void)sprintf(buf," of %d", (int) p_ptr->mhp);
   c_put_str(TERM_L_BLUE, buf, 33, 5);

   put_str("Mana  :", 19, 6);
   (void)sprintf(buf, "%7d", (int) p_ptr->csp);
   if (p_ptr->csp >= p_ptr->msp)
   {
      c_put_str(TERM_L_GREEN, buf, 26, 6);
   }
   else if (p_ptr->chp > (p_ptr->csp) / 3)
   {
      c_put_str(TERM_YELLOW, buf, 26, 6);
   }
   else if (p_ptr->chp > (p_ptr->csp) / 10)
   {
      c_put_str(TERM_ORANGE, buf, 26, 6);
   }
   else
   {
      c_put_str(TERM_RED, buf, 26, 6);
   }
   (void)sprintf(buf," of %d", (int) p_ptr->msp);
   c_put_str(TERM_L_BLUE, buf, 33, 6);
}

/*
 * Returns a "rating" of x depending on y
 */
static cptr likert(s16b x, s16b y, byte *color)
{
   /* Paranoia */
   if (y <= 0) y = 1;

   /* Negative value */
   if (x < 0)
   {
      *color = TERM_RED;
      return ("Very Bad");
   }

   /* Analyze the value */
   switch ((x / y))
   {
      case 0:
          *color = TERM_RED;
          return ("Useless");
      case 1:
          *color = TERM_RED;
          return ("Lacking");
      case 2:
          *color = TERM_RED;
          return ("Wretched");
      case 3:
          *color = TERM_ORANGE;
          return ("Feeble");
      case 4:
          *color = TERM_ORANGE;
          return ("Inept");
      case 5:
          *color = TERM_ORANGE;
          return ("Weak");
      case 6:
          *color = TERM_YELLOW;
          return ("Limited");
      case 7:
          *color = TERM_YELLOW;
          return ("Modest");
      case 8:
          *color = TERM_YELLOW;
          return ("Fair");
      case 9:
          *color = TERM_GREEN;
          return ("Moderate");
      case 10:
          *color = TERM_GREEN;
          return ("Passable");
      case 11:
          *color = TERM_GREEN;
          return ("Adequate");
      case 12:
          *color = TERM_L_BLUE;
          return ("Good");
      case 13:
          *color = TERM_L_BLUE;
          return ("Great");
      case 14:
          *color = TERM_L_BLUE;
          return ("Excellent");
      case 15:
          *color = TERM_BLUE;
          return ("Exemplary");
      case 16:
          *color = TERM_BLUE;
          return ("Master");
      case 17:
          *color = TERM_BLUE;
          return ("Superior");
      case 18:
          *color = TERM_VIOLET;
          return ("Expert");
      case 19:
          *color = TERM_VIOLET;
          return ("Awesome");
      case 20:
          *color = TERM_VIOLET;
          return ("Super");
      case 21:
          *color = TERM_L_WHITE;
          return ("Adept");
      case 22:
          *color = TERM_L_WHITE;
          return ("Matchless");
      default:
          *color = TERM_L_WHITE;
          return ("Sublime");
   }
}

/*
 * this function prints a skill description + value in a certain color
 */
static void print_skill(cptr desc, s16b xdesc, s16b ydesc, s16b skill, s16b divisor, s16b xval, s16b yval)
{
   char    out_val[32];
   cptr    skill_desc;
   byte    color;

   put_str(desc, xdesc, ydesc);
   skill_desc = likert(skill, divisor, &color);
   if (cheat_numeric_skills)
   {
      sprintf(out_val, "%2d/%s", skill/divisor, skill_desc);
   }
   else
   {
      sprintf(out_val, "%s", skill_desc);
   }
   c_put_str(color, out_val, xval, yval);
}


/*
 * Prints ratings on certain abilities
 *
 * This code is "imitated" elsewhere to "dump" a character sheet.
 */
static void display_player_various(bool interactive)
{
   s16b                 tmp, i;
   byte                 color;
   s16b                 xthn, xthb;

   object_type         *i_ptr;

   /* Fighting Skill (with current weapon) */
   i_ptr = &inventory[INVEN_WIELD];
   tmp = p_ptr->to_h + i_ptr->to_h;
   xthn = p_ptr->skill_thn + (tmp * BTH_PLUS_ADJ);

   /* Shooting Skill (with current bow and normal missile) */
   i_ptr = &inventory[INVEN_BOW];
   tmp = p_ptr->to_h + i_ptr->to_h;
   xthb = p_ptr->skill_thb + (tmp * BTH_PLUS_ADJ);

   if (interactive == TRUE)
   {
      put_str("|======== [Bad ", 1, 8);
      for (i=0; i < 24; i++)
      {
         (void)likert(i, 1, &color);
         c_put_str(color, "*", 16+i, 8);
      }
      put_str("Good] =======|", 41, 8);
   }
   print_skill("Fighting    :",  1,  9, xthn, 12, 15, 9);
   print_skill("Bows/Throw  :",  1, 10, xthb, 12, 15, 10);
   print_skill("Saving Throw:",  1, 11, p_ptr->skill_sav,  6,  15, 11);
   print_skill("Stealth     :",  1, 12, p_ptr->skill_stl,  1, 15, 12);
   print_skill("Perception  :", 28,  9, p_ptr->skill_pcp,  6, 42, 9);
   print_skill("Searching   :", 28, 10, p_ptr->skill_srh,  6, 42, 10);
   print_skill("Disarming   :", 28, 11, p_ptr->skill_dis,  8, 42, 11);
   print_skill("Magic Device:", 28, 12, p_ptr->skill_dev, 12, 42, 12);

   put_str("Blows/Round :", 55, 9);
   if ((p_ptr->pclass == CLASS_GLADIATR) &&
       (inventory[INVEN_ARM].k_idx!=0))
   {
      put_str(format("%d/%d", p_ptr->num_blow1, p_ptr->num_blow2), 69, 9);
   }
   else
   {
      if (p_ptr->pclass == CLASS_HIGHPRST)
      {
         put_str("none", 69,9);
      }
      else
      {
         put_str(format("%d", p_ptr->num_blow1), 69, 9);
      }
   }

   put_str("Shots/Round :", 55, 10);
   if ((p_ptr->pclass == CLASS_HIGHPRST) || (p_ptr->num_fire == 0))
   {
      put_str("none", 69, 10);
   }
   else
   {
      put_str(format("%d", p_ptr->num_fire), 69, 10);
   }

   put_str("Infra-Vision:", 55, 11);
   put_str(format("%d feet", p_ptr->see_infra * 10), 69, 11);

   put_str("Light-Range :", 55, 12);
   put_str(format("%d feet", p_ptr->cur_lite*10), 69, 12);

/* jk - add tactic */
   put_str("Tactic    :",43,4);
   c_put_str(TERM_L_BLUE, tactic_info[p_ptr->tactic].name, 54, 4);
/* jk - add movement */
   put_str("Exploring :",43,5);
   c_put_str(TERM_L_BLUE, move_info[p_ptr->movement].name, 54, 5);
}

/*
 * Obtain the "flags" for the player as if he was an item
 */
static void player_flags(u64b *f1, u64b *f2, u64b *f3)
{
   /* Clear */
   (*f1) = (*f2) = (*f3) = 0LL;

   /* Elf */
   if (p_ptr->prace == RACE_ELF) (*f2) |= (TR2_RES_LITE);

   /* Hobbit */
   if (p_ptr->prace == RACE_HOBBIT) (*f2) |= (TR2_SUST_DEX);

   /* Gnome */
   if (p_ptr->prace == RACE_GNOME) (*f2) |= (TR2_FREE_ACT);

   /* Dwarf */
   if (p_ptr->prace == RACE_DWARF) (*f2) |= (TR2_RES_BLIND);

   /* Half-Orc */
   if (p_ptr->prace == RACE_HALF_ORC) (*f2) |= (TR2_RES_DARK);

   /* Half-Troll */
   if (p_ptr->prace == RACE_HALF_TROLL) (*f2) |= (TR2_SUST_STR);

   /* Dunadan */
   if (p_ptr->prace == RACE_DUNADAN) (*f2) |= (TR2_SUST_CON);

   /* Druedain */
   if (p_ptr->prace == RACE_DRUEDAIN) (*f3) |= (TR3_TELEPATHY);
   if (p_ptr->prace == RACE_DRUEDAIN) (*f1) |= (TR1_SPEED1);

   /* High Elf */
   if (p_ptr->prace == RACE_HIGH_ELF) (*f2) |= (TR2_RES_LITE);
   if (p_ptr->prace == RACE_HIGH_ELF) (*f3) |= (TR3_SEE_INVIS);
}

static void display_player_equipment_flags(s16b flagset)
{
   u64b f[3];
   s16b i,j,a;
   char c;

   for (i=0; i<64; i++)
   {
      if (!object_flag_names[flagset][i])
      {
         c_put_str(TERM_RED, "Unused",
                   (i / 22 ) * 24, 1 + (i % 22 ) );
      }
      else
      {
         c_put_str(TERM_BLUE, object_flag_names[flagset][i],
                   (i / 22 ) * 24, 1 + (i % 22 ) );
      }
   }
   c_put_str(TERM_WHITE,"equipment:", 0, 0);
   for (i=0; i< 3; i++)
   {
      for (j=INVEN_WIELD; j<INVEN_AMMO; j++)
      {
         /* skip empty slots */
         if (!inventory[j].k_idx) continue;

         /* if it isn't an easy_know object */
         if (!item_easy_know(&inventory[j]))
         { 
            /* and it isn't *identifyied* */
            if (!(inventory[j].ident & ID_MENTAL))
            {
               /* then only continue if we're cheating */  
               if (!cheat_mode) continue;
            }
         }

         /* Get attr/char for display */
         a = object_attr(&inventory[j]);
         c = object_char(&inventory[j]);
         if (!use_color) a = TERM_WHITE;

         /* Display equippy char */
         Term_putch(10+i * 24+j-INVEN_WIELD, 0, a, c);

      }
   }

   for (j=INVEN_WIELD; j<INVEN_AMMO; j++)
   {
      object_flags(&inventory[j], &f[0], &f[1], &f[2]);
      for (i=0; i<64; i++)
      {
         a=TERM_WHITE;
         c='.';
         if (!inventory[j].k_idx)
         {
            a=TERM_SLATE;
         }
         else if (f[flagset] & (1LL<<i))
         {
            char i_name[80];
            object_desc(i_name, &inventory[j], TRUE, 3);
dlog(DEBUGITEMS,"files.c: d_p_e_f f item %s flag %d %016Lx & %016Lx\n",
           i_name, flagset, f[flagset], (f[flagset] & (1LL<<i)));
            if ((flagset==0) && ((1LL<<i) & TR1_P1VAL_MASK))
            {
               if (inventory[j].p1val<-9)
               {
                  a=TERM_RED;
                  c='*';
               }
               else if (inventory[j].p1val>9)
               {
                  a=TERM_GREEN;
                  c='*';
               }
               else if (inventory[j].p1val<0)
               {
                  a=TERM_RED;
                  c='0'-inventory[j].p1val;
               }
               else if (inventory[j].p1val>0)
               {
                  a=TERM_GREEN;
                  c='0'+inventory[j].p1val;
               }
               else
               {
                  a=TERM_YELLOW;
                  c='0';
               }
            }
            else
            {
               a=TERM_YELLOW;
               c='+';
            } /* flag in TR1_PVAL_MASK */
         } /* flag set */
         Term_putch(10+j-INVEN_WIELD+(i / 22 ) * 24, 1 + (i % 22 ) , a, c);
      } /* for 0-64 */
   }
}

/*
 * Equippy chars
 */
static void display_player_equippy(s16b x, s16b y)
{
   s16b i;

   byte a;
   char c;

   object_type *i_ptr;

   /* Dump equippy chars */
   for (i=INVEN_WIELD; i<INVEN_TOTAL; i++)
   {
      /* Object */
      i_ptr = &inventory[i];

      /* Skip empty objects */
      if (!i_ptr->k_idx) continue;

      /* Get attr/char for display */
      a = object_attr(i_ptr);
      c = object_char(i_ptr);

      /* No color */
/* jk - why do rings have color TERM_DARK? they are simply not visible */
/* now - make them & other such items white */
      if ((!use_color) || (a == TERM_DARK)) a = TERM_WHITE;

      /* Dump */
      Term_putch(x+i-INVEN_WIELD, y, a, c);
   }
}

void print_equippy(void)
{
   display_player_equippy(COL_EQUIPPY, ROW_EQUIPPY);
}

/*
 * Helper function, see below
 */
static void display_player_flag_aux(s16b col, s16b row,
                        char *header, s16b n, u32b flag)
{
   s16b i;

   u64b f[3];

   /* Header */
   c_put_str(TERM_WHITE, header, col, row);

   /* Advance */
   col += strlen(header) + 1;

   /* Check equipment */
   for (i=INVEN_WIELD; i<INVEN_TOTAL; i++)
   {
      object_type *i_ptr;

      /* Object */
      i_ptr = &inventory[i];

      /* Known flags */
      object_flags(i_ptr, &f[0], &f[1], &f[2]);

      /* Default */
      c_put_str(TERM_SLATE, ".", col, row);

      /* Check flags for fully *id*'ed items or when cheating */
      /* if it isn't an easy_know object */
dlog(DEBUGEXTRA,"files.c: display_player_flag_aux: item %s header %s item_easy_know %d\n",
                k_name + k_info[i_ptr->k_idx].name, header, item_easy_know(i_ptr));
      if (!item_easy_know(i_ptr))
      { 
         /* and it isn't *identifyied* */
         if (!(i_ptr->ident & ID_MENTAL))
         {
            /* then only continue if we're cheating */  
            if (!cheat_mode)
            {
               col++;
               continue;
            }
         }
      }
      if (f[n-1] & flag) c_put_str(TERM_WHITE, "+", col, row);

      /* Advance */
      col++;
   }

   /* Player flags */
   player_flags(&f[0], &f[1], &f[2]);

   /* Default */
   c_put_str(TERM_SLATE, ".", col, row);

   /* Check flags */
   /* XXX XXX XXX terrible hack: */
   if (p_ptr->prace == RACE_DRUEDAIN)
   {
      if ((flag == TR1_SPEED1) && (n==1))
      {
         c_put_str(TERM_WHITE, "-", col, row);
      }
      else
      {
         if (f[n-1] & flag) c_put_str(TERM_WHITE, "+", col, row);
      }
   }
}


/*
 * Special display, part 1
 */
static void display_player_flag_info(void)
{
   s16b col;
   s16b row;

   /*** Set 1 ***/

   col = 1;
   row = 13;

   display_player_equippy(col+7, row-2);

   c_put_str(TERM_WHITE, "abcdefghijklm@", col+7, row-1);

   display_player_flag_aux(col, row+0, "Acid :", 2, (TR2_RES_ACID | TR2_IM_ACID));
   display_player_flag_aux(col, row+1, "Elec :", 2, (TR2_RES_ELEC | TR2_IM_ELEC));
   display_player_flag_aux(col, row+2, "Fire :", 2, (TR2_RES_FIRE | TR2_IM_FIRE));
   display_player_flag_aux(col, row+3, "Cold :", 2, (TR2_RES_COLD | TR2_IM_COLD));
   display_player_flag_aux(col, row+4, "Poisn:", 2, TR2_RES_POIS);
   display_player_flag_aux(col, row+5, "Light:", 2, TR2_RES_LITE);
   display_player_flag_aux(col, row+6, "Dark :", 2, TR2_RES_DARK);
   display_player_flag_aux(col, row+7, "Shard:", 2, TR2_RES_SHARDS);
   display_player_flag_aux(col, row+8, "Fear :", 2, TR2_RES_FEAR);

   /*** Set 2 ***/

   col = 24;
   row = 13;

   display_player_equippy(col+8, row-2);

   c_put_str(TERM_WHITE, "abcdefghijklm@", col+8, row-1);

   display_player_flag_aux(col, row+0, "Blind :", 2, TR2_RES_BLIND);
   display_player_flag_aux(col, row+1, "Conf  :", 2, TR2_RES_CONF);
   display_player_flag_aux(col, row+2, "Sound :", 2, TR2_RES_SOUND);
   display_player_flag_aux(col, row+3, "Nether:", 2, TR2_RES_NETHER);
   display_player_flag_aux(col, row+4, "Nexus :", 2, TR2_RES_NEXUS);
   display_player_flag_aux(col, row+5, "Chaos :", 2, TR2_RES_CHAOS);
   display_player_flag_aux(col, row+6, "Disnch:", 2, TR2_RES_DISEN);
   display_player_flag_aux(col, row+7, "Speed :", 1, TR1_SPEED1);

   /*** Set 3 ***/

   col = 48;
   row = 13;

   display_player_equippy(col+15, row-2);

   c_put_str(TERM_WHITE, "abcdefghijklm@", col+15, row-1);

   display_player_flag_aux(col, row+0, "Free Action  :", 2, TR2_FREE_ACT);
   display_player_flag_aux(col, row+1, "See Invisible:", 3, TR3_SEE_INVIS);
   display_player_flag_aux(col, row+2, "Hold Life    :", 2, TR2_HOLD_LIFE);
   display_player_flag_aux(col, row+3, "Telepathy    :", 3, TR3_TELEPATHY);
   display_player_flag_aux(col, row+4, "Slow Digest  :", 3, TR3_SLOW_DIGEST);
   display_player_flag_aux(col, row+5, "Regeneration :", 3, TR3_REGEN);
   display_player_flag_aux(col, row+6, "Feather Fall :", 3, TR3_FEATHER);
   display_player_flag_aux(col, row+7, "Perm Lite    :", 3, TR3_SOMELITE);
}

/*
 * Special display, part 2b
 *
 * How to print out the modifications and sustains.
 * Positive mods with no sustain will be light green.
 * Positive mods with a sustain will be dark green.
 * Sustains (with no modification) will be a dark green 's'.
 * Negative mods (from a curse) will be red.
 * Huge mods (>9), like from MICoMorgoth, will be a '*'
 * No mod, no sustain, will be a slate '.'
 */
static void display_player_stat_info(void)
{
   s16b i, e_adj;
   s16b stat_col, stat;
   s16b row, col;

   object_type *i_ptr;
   u64b f1, f2, f3;
   s16b k_idx;

   byte a;
   char c;

   char buf[80];

   /* Column */
   stat_col = 1;

   /* Row */
   row = 3;

   Term_clear();

   /* Print out the labels for the columns */
   c_put_str(TERM_BLUE,     "Internal",     stat_col+4,  row-1);
   c_put_str(TERM_L_BLUE,   "Race",         stat_col+13, row-1);
   c_put_str(TERM_L_BLUE,   "Class",        stat_col+18, row-1);

   if (p_ptr->teach_birth)
   {
      c_put_str(TERM_L_BLUE,"Teach",        stat_col+24, row-1);
   }
   c_put_str(TERM_L_BLUE,   "Equip",        stat_col+30, row-1);
   c_put_str(TERM_L_GREEN,  "Actual",       stat_col+36, row-1);
   c_put_str(TERM_YELLOW,   "Current",      stat_col+43, row-1);


   /* Display the stats */
   for (i = 0; i < 6; i++)
   {
      s16b k;
      /* Calculate equipment adjustment */
      e_adj = 0;
      for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
      {
         object_type *i_ptr = &inventory[k];
         u64b f1,f2,f3;
         if (!i_ptr->k_idx) continue;
         object_flags(i_ptr, &f1, &f2, &f3);
         if ((i == 0) && (f1 & TR1_STR1)) e_adj += i_ptr->p1val;
         if ((i == 1) && (f1 & TR1_INT1)) e_adj += i_ptr->p1val;
         if ((i == 2) && (f1 & TR1_WIS1)) e_adj += i_ptr->p1val;
         if ((i == 3) && (f1 & TR1_DEX1)) e_adj += i_ptr->p1val;
         if ((i == 4) && (f1 & TR1_CON1)) e_adj += i_ptr->p1val;
         if ((i == 5) && (f1 & TR1_CHR1)) e_adj += i_ptr->p1val;
         if ((i == 0) && (f1 & TR1_STR2)) e_adj += i_ptr->p2val;
         if ((i == 1) && (f1 & TR1_INT2)) e_adj += i_ptr->p2val;
         if ((i == 2) && (f1 & TR1_WIS2)) e_adj += i_ptr->p2val;
         if ((i == 3) && (f1 & TR1_DEX2)) e_adj += i_ptr->p2val;
         if ((i == 4) && (f1 & TR1_CON2)) e_adj += i_ptr->p2val;
         if ((i == 5) && (f1 & TR1_CHR2)) e_adj += i_ptr->p2val;
      }
         
      /* Reduced name of stat */
      c_put_str(TERM_WHITE, stat_names_reduced[i], stat_col, row+i);

      /* Internal "natural" max value.  Maxes at 18/100 */
      /* This is useful to see if you are maxed out     */
      cnv_stat(p_ptr->stat_max[i], buf);
      c_put_str(TERM_BLUE, buf, stat_col+4, row+i);

      /* Race and class modifiers */
      if (!p_ptr->teach_birth)
      {
         (void) sprintf(buf, "%3d", (int) rp_ptr->r_adj[i]);
         c_put_str(TERM_L_BLUE, buf, stat_col+13, row+i);
         (void) sprintf(buf, "%3d", (int) cp_ptr->c_adj[i]);
         c_put_str(TERM_L_BLUE, buf, stat_col+18, row+i);
      }
      else
      {
         /* some difference here: display both bonuses /3 and make sure 
          * that there are no rounding errors
          */
         s16b bonus = (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]) / 3;
         (void) sprintf(buf, "%3d", (int) rp_ptr->r_adj[i] / 3);
         c_put_str(TERM_L_BLUE, buf, stat_col+13, row+i);
         (void) sprintf(buf, "%3d", (int) (bonus - (rp_ptr->r_adj[i] / 3)));
         c_put_str(TERM_L_BLUE, buf, stat_col+18, row+i);
         
         (void) sprintf(buf, "%3d", (int) p_ptr->teach_stat[i]);
         c_put_str(TERM_L_BLUE,buf, stat_col+24, row+i);
      }

      /* equipment modifiers */
      (void) sprintf(buf, "%3d", e_adj);
      c_put_str(TERM_L_BLUE, buf, stat_col+30, row+i);

      /* Actual maximal modified value */
      cnv_stat(p_ptr->stat_top[i], buf);
      c_put_str(TERM_L_GREEN, buf, stat_col+36, row+i);

      /* Only display stat_use if not maximal */
      cnv_stat(p_ptr->stat_use[i], buf);
      if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
      {
         c_put_str(TERM_YELLOW, buf, stat_col+43, row+i);
      }
      else
      {
         c_put_str(TERM_GREEN, buf, stat_col+43, row+i);
      }
   }

   /* Column */
   col = stat_col + 51;

   /* Header and Footer */
   display_player_equippy(col, row-2);
   c_put_str(TERM_WHITE, "abcdefghijklm", col, row-1);
   c_put_str(TERM_L_GREEN, "Modifications", col, row+6);
   c_put_str(TERM_L_GREEN, "  (part I)  ", col, row+7);

   /* Process equipment */
   for (i=INVEN_WIELD; i<INVEN_TOTAL; i++)
   {
      /* Access object */
      i_ptr = &inventory[i];

      /* Object kind */
      k_idx = i_ptr->k_idx;

      /* Acquire "known" flags */
      object_flags(i_ptr, &f1, &f2, &f3);

      /* Initialize color based of sign of p1val. */
      for (stat=0; stat<6; stat++)
      {
         /* Default */
         a = TERM_SLATE;
         c = '.';

         /* Boost */
         if (f1 & TR1_STAT1_START<<stat)
         {
            /* Default */
            c = '*';

            /* Good */
            if (i_ptr->p1val > 0)
            {
               /* Good */
               a = TERM_L_GREEN;

               /* Label boost */
               if (i_ptr->p1val < 10) c = '0' + i_ptr->p1val;
            }

            /* Bad */
            if (i_ptr->p1val < 0)
            {
               /* Bad */
               a = TERM_RED;

               /* Label boost */
               if (i_ptr->p1val < 10) c = '0' - i_ptr->p1val;
            }
         }

         /* Sustain */
         if (f2 & TR2_SUST_STAT_START<<stat)
         {
            /* Dark green "s" */
            a = TERM_GREEN;
            c = 's';
         }

         /* Handle monochrome */
         if (!use_color) a = TERM_WHITE;

         /* Dump proper character */
         Term_putch(col, row+stat, a, c);
      }

      /* Advance */
      col++;
   }

   /* Column */
   col++;

   /* Header and Footer */
   display_player_equippy(col, row-2);
   c_put_str(TERM_WHITE, "abcdefghijklm@", col, row-1);
   c_put_str(TERM_L_GREEN, "Modifications", col, row+6);
   c_put_str(TERM_L_GREEN, "  (part II)  ", col, row+7);

   /* Process equipment */
   for (i=INVEN_WIELD; i<INVEN_TOTAL; i++)
   {
      /* Access object */
      i_ptr = &inventory[i];

      /* Object kind */
      k_idx = i_ptr->k_idx;

      /* Acquire "known" flags */
      object_flags(i_ptr, &f1, &f2, &f3);

      /* Initialize color based of sign of p1val. */
      for (stat=0; stat<6; stat++)
      {
         /* Default */
         a = TERM_SLATE;
         c = '.';

         /* Boost */
         if (f1 & TR1_STAT2_START<<stat)
         {
            /* Default */
            c = '*';

            /* Good */
            if (i_ptr->p2val > 0)
            {
               /* Good */
               a = TERM_L_GREEN;

               /* Label boost */
               if (i_ptr->p2val < 10) c = '0' + i_ptr->p2val;
            }

            /* Bad */
            if (i_ptr->p2val < 0)
            {
               /* Bad */
               a = TERM_RED;

               /* Label boost */
               if (i_ptr->p2val < 10) c = '0' - i_ptr->p2val;
            }
         }

         /* Sustain's are handled in part I */

         /* Handle monochrome */
         if (!use_color) a = TERM_WHITE;

         /* Dump proper character */
         Term_putch(col, row+stat, a, c);
      }

      /* Advance */
      col++;
   }

   /* Player flags */
   player_flags(&f1, &f2, &f3);

   /* Check stats */
   for (stat=0; stat<6; stat++)
   {
      /* Default */
      a = TERM_SLATE;
      c = '.';

      /* Sustain */
      if (f2 & 1<<stat)
      {
         /* Dark green "s" */
         a = TERM_GREEN;
         c = 's';
      }

      /* No color */
      if (!use_color) a = TERM_WHITE;

      /* Dump */
      Term_putch(col, row+stat, a, c);
   }

}

static void display_player_name(void)
{
   char buf[80];

   if ((p_ptr->ht % 12) == 0)
   {
      sprintf(buf, "%d feet", p_ptr->ht / 12);
   }
   else
   {
      sprintf(buf, "%d feet %d", p_ptr->ht / 12, p_ptr->ht % 12);
   }
   c_put_str(TERM_L_BLUE, format("%s the %s %s %s (%d years, %s, %d pounds).",
             player_name, p_ptr->psex==SEX_MALE?"male":"female",rp_ptr->title,
             cp_ptr->title, p_ptr->age, buf, p_ptr->wt), 1, 1);
}


/* standard player display, with or withhout history */

void display_player_standard(bool interactive)
{
   s16b i;
   char buf[80];

   display_player_name();

   /* Display the stats */
   for (i = 0; i < 6; i++)
   {
      /* Special treatment of "injured" stats */
      if (p_ptr->stat_cur[i] < p_ptr->stat_max[i])
      {
         s16b value;
         s16b color;

         if (p_ptr->stat_cnt[i])
            color = TERM_ORANGE;
         else
            color = TERM_RED;

         /* Use lowercase stat name */
         c_put_str(color, stat_names_reduced[i], 1, 2 + i);

         /* Get the current stat */
         value = p_ptr->stat_use[i];

         /* Obtain the current stat (modified) */
         cnv_stat(value, buf);

         /* Display the current stat (modified) */
         c_put_str(TERM_YELLOW, buf, 5, 2 + i);

         /* Acquire the max stat */
         value = p_ptr->stat_top[i];

         /* Obtain the maximum stat (modified) */
         cnv_stat(value, buf);

         /* Display the maximum stat (modified) */
         c_put_str(TERM_L_GREEN, buf, 12, 2 + i);
      }

      /* Normal treatment of "normal" stats */
      else
      {
         /* Assume uppercase stat name */
         put_str(stat_names[i], 1, 2 + i);

         /* Obtain the current stat (modified) */
         cnv_stat(p_ptr->stat_use[i], buf);

         /* Display the current stat (modified) */
         c_put_str(TERM_L_GREEN, buf, 5, 2 + i);
      }
   }

   /* Extra info */
   display_player_middle();

   /* Display "history" info */
   for (i = 0; i < 5; i++)
   {
      put_str(history[i], 5, i + 14);
   }

   if (p_ptr->teach_birth)
   {
      for (i = 0; i < 4; i++)
      {
         put_str(teacher[i], 5, i + 19);
      }
   }

   display_player_various(interactive);
}

/*
 * Display the character on the screen (various modes)
 *
 * The top two and bottom two lines are left blank.
 *
 * Mode 0 = standard display with skills + history
 * Mode 2 = summary of various things
 * Mode 3 = current flags (set 1)
 * Mode 4 = current flags (set 2)
 * Mode 5 = current flags (set 3)
 */

void display_player(s16b mode, bool interactive)
{
   mode = mode % 5;

   /* Erase screen */
   clear_from(0);

   /* Standard */
   if (mode == 0)
   {
     display_player_standard(interactive);
   }
   /* Special */
   else if (mode == 1)
   {
     /* See "http://www.cs.berkeley.edu/~davidb/angband.html" */

     /* Dump the info */
     display_player_stat_info();
     display_player_flag_info();
   }

   /* Special */
   else
   {
      display_player_equipment_flags(mode-2);
   }
}

static void dump_book_info(FILE *fff, object_type *i_ptr)
{
   s16b index[MAX_SPELLS_PER_ITEM], s, count = 0;

   for (s=0; s < (s16b)s_number; s++)
   {
      if (has_spell(i_ptr, s)) index[count++]=s;
   } 
   if (count == 0)
   {
      fprintf(fff, "It holds no spells.\n");
   }
   else
   {
      spell_type *s_ptr;
      bool        cheat_any = FALSE;
      cptr        scalestr[6] = {"small","avg  ","large","super"};
      cptr        categ[MAX_SPELL_TYPES] = { "Nature Forces",
                                             "Dark Forces",
                                             "Escape",
                                             "Heal",
                                             "Sense",
                                             "Change Other",
                                             "Change Item",
                                             "Change Self",
                                             "Change World" };
      fprintf(fff, "It holds:\n"); 
      for (s=0; s < count; s++)
      {
         s_ptr = &s_info[index[s]];
         cheat_any |= (s_ptr->numcast >= (10 * s_ptr->level));
      }
      if (cheat_any)
      {
         fprintf(fff, "Knowledge  Spell                  Lev Mana Fail Scale Category        Extra\n");
      }
      else
      {
         fprintf(fff, "Knowledge  Spell                  Lev Mana Fail\n");
      }
      for (s=0; s < count; s++)
      {
         char extra_info[80];

         s_ptr = &s_info[index[s]];
         if (s_ptr->numcast == 0)
         {
            fprintf(fff, "untried    %-23s%3d  %2d\n",
            s_name + s_ptr->name, s_ptr->level, s_ptr->mana);
         }
         else if (s_ptr->numcast >= (10 * s_ptr->level))
         {
            extra_new_spell_info(extra_info, index[s]);
            fprintf(fff, "well known %-23s%3d  %2d  %2d%%  %5s %-14s %s\n",
            s_name + s_ptr->name, s_ptr->level, s_ptr->mana,
            page_chance(index[s]), scalestr[(s16b)s_ptr->scale],
            categ[s_ptr->type], extra_info);
         }
         else if (s_ptr->numcast > 0)
         {
            fprintf(fff, "known      %-23s%3d  %2d  %2d%%\n",
            s_name + s_ptr->name, s_ptr->level, s_ptr->mana,
            page_chance(index[s]));
         }
      }
   }
}

/*
 * Hack -- Dump a character description file
 *
 * XXX XXX XXX Allow the "full" flag to dump additional info,
 * and trigger its usage from various places in the code.
 */
errr file_character(cptr name, bool full)
{
   s16b           i, x, y;

   byte           a;
   char           c;

   cptr           paren = ")";

   s16b           fd = -1;

   FILE          *fff = NULL;

   store_type    *st_ptr = &store[0];

   char           i_name[80];

   char           buf[1024];
   char           filename[1024];


   /* Drop priv's */
   safe_setuid_drop();

   /* Access the help file */
   strcpy(filename, ANGBAND_DIR_USER);
   strcat(filename, name);

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "text file" */
   _ftype = 'TEXT';
#endif

   /* Check for existing file */
   fd = fd_open(filename, O_RDONLY);

   /* Existing file */
   if (fd >= 0)
   {
      char out_val[160];

      /* Close the file */
      (void)fd_close(fd);

      /* Build query */
      (void)sprintf(out_val, "Replace existing file %s? ", filename);

      /* Ask */
      if (get_check(out_val)) fd = -1;
   }

   /* Open the non-existing file */
   if (fd < 0) fff = my_fopen(filename, "w");

   /* Grab priv's */
   safe_setuid_grab();


   /* Invalid file */
   if (!fff)
   {
      /* Message */
      msg_format("Character dump failed!");
      msg_print(NULL);

      /* Error */
      return (-1);
   }

   /* Begin dump */
   fprintf(fff, " [Angband/64 beta %d release %d (%d.%d.%d) Character Dump]\n\n",
               VERSION_BETA, VERSION_RELEASE, VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);

   /* Save screen */
   Term_save();

   /* Display the player (with various) */
   display_player(0, FALSE);

   /* Dump part of the screen */
   for (y = 0; y < 22; y++)
   {
      /* Dump each row */
      for (x = 0; x < 79; x++)
      {
         /* Get the attr/char */
         (void)(Term_what(x, y, &a, &c));

         /* Dump it */
         buf[x] = c;
      }

      /* Terminate */
      buf[x] = '\0';

      /* End the row */
      fprintf(fff, "%s\n", buf);
   }

   /* Display the player (with history) */
   display_player(1, FALSE);

   /* Dump part of the screen */
   for (y = 0; y < 20; y++)
   {
      /* Dump each row */
      for (x = 0; x < 79; x++)
      {
         /* Get the attr/char */
         (void)(Term_what(x, y, &a, &c));

         /* Dump it */
         buf[x] = c;
      }

      /* Terminate */
      buf[x] = '\0';

      /* End the row */
      fprintf(fff, "%s\n", buf);
   }

   /* Restore screen */
   Term_load();

   /* Skip some lines */
   fprintf(fff, "\n\n");


   /* Dump the equipment */
   if (equip_cnt)
   {
      fprintf(fff, " [Character Equipment]\n\n");
      for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
      {
         if (!inventory[i].k_idx) continue;
         object_desc(i_name, &inventory[i], TRUE, 3);
         fprintf(fff, "%c%s %s\n",
               index_to_label(i), paren, i_name);
         expand_item_log(&inventory[i].log, buf);
         fprintf(fff, "   %s\n", buf);
      }
      fprintf(fff, "\n\n");
   }

   /* Dump the inventory */
   fprintf(fff, " [Character Inventory]\n\n");
   for (i = 0; i < INVEN_PACK; i++)
   {
      if (!inventory[i].k_idx) continue;
      object_desc(i_name, &inventory[i], TRUE, 3);
      fprintf(fff, "%c%s %s\n",
              index_to_label(i), paren, i_name);
      if (inventory[i].tval == TV_BOOK)
      {
         dump_book_info(fff, &inventory[i]);
         fprintf(fff, "\n");
      }
         
      expand_item_log(&inventory[i].log, buf);
      fprintf(fff, "   %s\n", buf);
   }
   fprintf(fff, "\n\n");


   /* Dump the Home (page 1) */
   fprintf(fff, " [Home Inventory (page 1)]\n\n");
   for (i = 0; i < 12; i++)
   {
      if (!st_ptr->stock[i].k_idx) continue;
      object_desc(i_name, &st_ptr->stock[i], TRUE, 3);
      fprintf(fff, "%c%s %s\n", I2A(i%12), paren, i_name);
      if (st_ptr->stock[i].tval == TV_BOOK)
      {
         dump_book_info(fff, &st_ptr->stock[i]);
         fprintf(fff, "\n");
      }
      expand_item_log(&inventory[i].log, buf);
      fprintf(fff, "   %s\n", buf);
   }
   fprintf(fff, "\n\n");

   if (st_ptr->stock_num > 12)
   {
      /* Dump the Home (page 2) */
      fprintf(fff, " [Home Inventory (page 2)]\n\n");
      for (i = 12; i < 24; i++)
      {
         if (!st_ptr->stock[i].k_idx) continue;
         object_desc(i_name, &st_ptr->stock[i], TRUE, 3);
         fprintf(fff, "%c%s %s\n", I2A(i%12), paren, i_name);
         if (st_ptr->stock[i].tval == TV_BOOK)
         {
            dump_book_info(fff, &st_ptr->stock[i]);
            fprintf(fff, "\n");
         }
         expand_item_log(&st_ptr->stock[i].log, buf);
         fprintf(fff, "   %s\n", buf);
      }
      fprintf(fff, "\n\n");
   }

   if (st_ptr->stock_num > 24)
   {
      /* Dump the Home (page 2) */
      fprintf(fff, " [Home Inventory (page 3)]\n\n");
      for (i = 24; i < 36; i++)
      {
         if (!st_ptr->stock[i].k_idx) continue;
         object_desc(i_name, &st_ptr->stock[i], TRUE, 3);
         fprintf(fff, "%c%s %s\n", I2A(i%12), paren, i_name);
         if (st_ptr->stock[i].tval == TV_BOOK)
         {
            dump_book_info(fff, &st_ptr->stock[i]);
            fprintf(fff, "\n");
         }
         expand_item_log(&st_ptr->stock[i].log, buf);
         fprintf(fff, "   %s\n", buf);
      }
      fprintf(fff, "\n\n");
   }

   my_fclose(fff);

   do_cmd_check_intrinsics(filename, FALSE);
   do_cmd_check_artifacts(filename, FALSE);
   do_cmd_check_items(filename, FALSE);
   do_cmd_check_uniques(filename, FALSE);
   do_cmd_check_spells(filename, FALSE);
   do_cmd_check_kills(filename, FALSE);
   do_cmd_check_progress(filename, FALSE);

   /* Message */
   msg_print("Character dump successful.");
   msg_print(NULL);

   /* Success */
   return (0);
}

/*
 * Recursive file perusal.
 *
 * Return FALSE on "ESCAPE", otherwise TRUE.
 *
 * Process various special text in the input file, including
 * the "menu" structures used by the "help file" system.
 *
 * XXX XXX XXX Consider using a temporary file.
 *
 * XXX XXX XXX Allow the user to "save" the current file.
 */
bool browse_file(cptr name, bool start_on_last_line, cptr what)
{
   u32b i, k;
   u32b line = 0;               /* current line, as shown on screen */
   u32b next = 0;               /* Number of "real" lines passed by */
   u32b size = 0;               /* Number of "real" lines in the file */
   u32b back = 0;               /* Backup value for "line" */
   bool menu = FALSE;           /* This screen has sub-screens */
   FILE *fff = NULL;            /* Current help file */
   cptr find = NULL;            /* Find this string (if any) */
   char finder[81];             /* Hold a string to find */
   char shower[81];             /* Hold a string to show */
   char caption[128];           /* Describe this thing */
   char path[1024];             /* Path buffer */
   char buf[1024];              /* General buffer */
   char hook[10][32];           /* Sub-menu information */

   strcpy(finder, "");          /* Wipe finder */
   strcpy(shower, "");          /* Wipe shower */
   strcpy(caption, "");         /* Wipe caption */

   /* Wipe the hooks */
   for (i = 0; i < 10; i++) hook[i][0] = '\0';

   /* change caption if necessary */
   if (what)
   {
      strcpy(caption, what);      /* Caption */
      strcpy(path, name);         /* Access the "file" */
      fff = my_fopen(path, "r");  /* Open */
   }

   /* Look in "help" */
   if (!fff)
   {
      sprintf(caption, "Help file '%s'", name);
      path_build(path, 1024, ANGBAND_DIR_HELP, name);
      fff = my_fopen(path, "r");
   }

   /* Look in "info" */
   if (!fff)
   {
      sprintf(caption, "Info file '%s'", name);
      path_build(path, 1024, ANGBAND_DIR_INFO, name);
      fff = my_fopen(path, "r");
   }

   /* Oops */
   if (!fff)
   {
      msg_format("Cannot open '%s'.", name);
      msg_print(NULL);

      return (TRUE);
   }

   /* Pre-Parse the file */
   while (TRUE)
   {
      /* Read a line or stop */
      if (my_fgets(fff, buf, 1024)) break;

      /* XXX Parse "menu" items */
      if (prefix(buf, "***** "))
      {
         char b1 = '[', b2 = ']';

         /* Notice "menu" requests */
         if ((buf[6] == b1) && isdigit(buf[7]) &&
             (buf[8] == b2) && (buf[9] == ' '))
         {
            /* This is a menu file */
            menu = TRUE;

            /* Extract the menu item */
            k = D2I(buf[7]);

            /* Extract the menu item */
            strcpy(hook[k], buf + 10);
         }

         /* Skip this */
         continue;
      }

      /* Count the "real" lines */
      next++;
   }

   /* Save the number of "real" lines */
   size = next;

   if (start_on_last_line)
   {
      line=size-20;
      start_on_last_line = FALSE; /* don't keep going there! */
   }

   /* Display the file */
   while (TRUE)
   {
      /* Clear screen */
      Term_clear();


      /* Restart when necessary */
      if (line >= size) line = 0;


      /* Re-open the file if needed */
      if (next > line)
      {
         /* Close it */
         my_fclose(fff);

         /* Hack -- Re-Open the file */
         fff = my_fopen(path, "r");

         /* Oops */
         if (!fff) return (FALSE);

         /* File has been restarted */
         next = 0;
      }

      /* Skip lines if needed */
      for (; next < line; next++)
      {
         /* Skip a line */
         if (my_fgets(fff, buf, 1024)) break;
      }


      /* Dump the next 20 lines of the file */
      for (i = 0; i < 20; )
      {
         /* Hack -- track the "first" line */
         if (!i) line = next;

         /* Get a line of the file or stop */
         if (my_fgets(fff, buf, 1024)) break;

         /* Hack -- skip "special" lines */
         if (prefix(buf, "***** ")) continue;

         /* Count the "real" lines */
         next++;

         /* Hack -- keep searching */
         if (find && !i && !strstr(buf, find)) continue;

         /* Hack -- stop searching */
         find = NULL;

         /* Dump the line */
         Term_putstr(0, i+2, -1, TERM_WHITE, buf);

         /* Hilite "shower" */
         if (shower[0])
         {
            cptr str = buf;

            /* Display matches */
            while ((str = istrstr(str, shower)) != NULL)
            {
               int len = strlen(shower);

               /* Display the match */
               Term_putstr(str-buf, i+2, len, TERM_YELLOW, str);

               /* Advance */
               str += len;
            }
         }

         /* Count the printed lines */
         i++;
      }

      /* Hack -- failed search */
      if (find)
      {
         bell("Search string not found!");
         line = back;
         find = NULL;
         continue;
      }


      /* Show a general "title" */
      c_put_str(TERM_L_BLUE, format("[Angband/64 beta %d release %d, %s, Line %d/%d]",
                 VERSION_BETA, VERSION_RELEASE, caption, line, size), 0, 0);
      c_put_str(TERM_L_BLUE, "-------------------------------------------------------------------------------", 0, 1);

      /* Prompt -- menu screen */
      if (menu)
      {
         /* Wait for it */
         c_put_str(TERM_L_BLUE, "Press a number, or ESC to exit.", 0, 23);
      }

      /* Prompt -- small files */
      else if (size <= 20)
      {
         /* Wait for it */
         c_put_str(TERM_L_BLUE, "Press ESC to return to the previous file, or ? to exit.", 0, 23);
      }

      /* Prompt -- large files */
      else
      {
         /* Wait for it */
         c_put_str(TERM_L_BLUE, "Press + or - to scroll, / to search, # to go to a specific line", 0, 22);

         c_put_str(TERM_L_BLUE, "Space to advance, ESC to return to the previous file, or ? to exit.", 0, 23);
      }

      /* Get a keypress */
      k = inkey();

      /* Hack -- return to last screen on escape */
      if (k == ESCAPE) break;

      /* Hack -- try showing */
      if (k == '=')
      {
         /* Get "shower" */
         prt("Show: ", 0, 23);
         (void)askfor_aux(shower, 80);
      }

      /* Hack -- try finding */
      if (k == '/')
      {
         /* Get "finder" */
         prt("Find: ", 0, 23);
         if (askfor_aux(finder, 80))
         {
            /* Find it */
            find = finder;
            back = line;
            line = line + 1;

            /* Show it */
            strcpy(shower, finder);
         }
      }

      /* Hack -- go to a specific line */
      if (k == '#')
      {
         char tmp[81];
         prt("Goto Line: ", 0, 23);
         strcpy(tmp, "0");
         if (askfor_aux(tmp, 80))
         {
            line = atoi(tmp);
         }
      }

      /* Hack -- go to a specific file */
      if (k == '%')
      {
         char tmp[81];
         prt("Goto File: ", 0, 23);
         strcpy(tmp, "help.hlp");
         if (askfor_aux(tmp, 80))
         {
            if (!browse_file(tmp, FALSE, NULL)) k = ESCAPE;
         }
      }

      /* Hack -- Allow backing up */
      if (k == '-')
      {
         line = line - 15;
         if (line < 0) line = 0;
      }

      /* Hack -- Advance a single line */
      if ((k == '\n') || (k == '\r') || (k == '+'))
      {
         line = line + 1;
      }

      /* Advance one page */
      if (k == ' ')
      {
         line = line + 20;
      }

      /* Recurse on numbers */
      if (menu && isdigit(k) && hook[D2I(k)][0])
      {
         /* Recurse on that file */
         if (!browse_file(hook[D2I(k)], FALSE, NULL)) k = ESCAPE;
      }

      /* Exit on '?' */
      if (k == '?') break;
   }

   /* Close the file */
   my_fclose(fff);

   /* Exit on '?' */
   if (k == '?') return (FALSE);

   /* Normal return */
   return (TRUE);
}


/*
 * Peruse the On-Line-Help, starting at the given file.
 */
void do_cmd_help(cptr name)
{
   /* Hack -- default file */
   if (!name) name = "help.hlp";

   /* Enter "icky" mode */
   character_icky = TRUE;

   /* Save the screen */
   Term_save();

   /* Peruse the main help file */
   (void)browse_file(name, FALSE, NULL);

   /* Restore the screen */
   Term_load();

   /* Leave "icky" mode */
   character_icky = FALSE;
}



/*
 * Hack -- display the contents of a file on the screen
 *
 * XXX XXX XXX Use this function for commands such as the
 * "examine object" command.
 */
errr show_file(cptr name, cptr what)
{
   /* Enter "icky" mode */
   character_icky = TRUE;

   /* Save the screen */
   Term_save();

   /* Peruse the requested file */
   (void)browse_file(name, FALSE, what);

   /* Restore the screen */
   Term_load();

   /* Leave "icky" mode */
   character_icky = FALSE;

   /* Success */
   return (0);
}

/*
 * Process the player name.
 * Extract a clean "base name".
 * Build the savefile name if needed.
 */
void process_player_name(bool sf)
{
   s16b i, k = 0;


   /* Cannot be too long */
   if (strlen(player_name) > 15)
   {
      /* Name too long */
      quit_fmt("The name '%s' is too long!", player_name);
   }

   /* Cannot contain "icky" characters */
   for (i = 0; player_name[i]; i++)
   {
      /* No control characters */
      if (iscntrl((int)player_name[i]))
      {
         /* Illegal characters */
         quit_fmt("The name '%s' contains control chars!", player_name);
      }
   }

#ifdef MACINTOSH

   /* Extract "useful" letters */
   for (i = 0; player_name[i]; i++)
   {
      char c = player_name[i];

      /* Convert "dot" to "underscore" */
      if (c == '.') c = '_';

      /* Accept all the letters */
      player_base[k++] = c;
   }

#else

   /* Extract "useful" letters */
   for (i = 0; player_name[i]; i++)
   {
      char c = player_name[i];

      /* Accept some letters */
      if (isalpha((int)c) || isdigit((int)c)) player_base[k++] = c;

      /* Convert space, dot, and underscore to underscore */
      else if (strchr(". _", c)) player_base[k++] = '_';
   }

#endif


#if defined(WINDOWS) || defined(MSDOS)

   /* Hack -- max length */
   if (k > 8) k = 8;

#endif

   /* Terminate */
   player_base[k] = '\0';

   /* Require a "base" name */
   if (!player_base[0]) strcpy(player_base, "PLAYER");

#ifdef SAVEFILE_MUTABLE

   /* Accept */
   sf = TRUE;

#endif

   /* Change the savefile name */
   if (sf)
   {
      char temp[128];

#ifdef SAVEFILE_USE_UID
      /* Rename the savefile, using the player_uid and player_base */
      (void)sprintf(temp, "%d.%s", player_uid, player_base);
#else
      /* Rename the savefile, using the player_base */
      (void)sprintf(temp, "%s", player_base);
#endif

#ifdef VM
      /* Hack -- support "flat directory" usage on VM/ESA */
      (void)sprintf(temp, "%s.sv", player_base);
#endif /* VM */

      /* Build the savefile name */
      strcpy(savefile, ANGBAND_DIR_SAVE);
      strcat(savefile, temp);
      strcpy(levelfile, savefile);
   }
}


/*
 * Gets a name for the character, reacting to name changes.
 *
 * Assumes that "display_player()" has just been called
 * XXX Perhaps we should NOT ask for a name (at "birth()") on Unix?
 */
void get_name()
{
   char tmp[32];

   /* Prompt and ask */
   prt("Enter your player's name:", 2, 23);

   /* Ask until happy */
   while (1)
   {
      /* Go to the "name" field */
      move_cursor(28,23);

      /* Save the player name */
      strcpy(tmp, player_name);

      /* Get an input, ignore "Escape" */
      if (askfor_aux(tmp, 9)) strcpy(player_name, tmp);

      /* Process the player name */
      process_player_name(FALSE);

      /* All done */
      break;
   }

   /* Pad the name (to clear junk) */
   sprintf(tmp, "%-9.9s", player_name);

   display_player_name();

   /* Erase the prompt, etc */
   clear_from(23);
}

/*
 * Hack -- commit suicide
 */
void do_cmd_suicide(void)
{
   s16b i;

   /* Flush input */
   flush();

   /* Verify Retirement */
   if (total_winner)
   {
      /* Verify */
      if (!get_check("Do you want to retire? ")) return;
   }

   /* Verify Suicide */
   else
   {
      /* Verify */
      if (!get_check("Do you really want to quit? ")) return;

      /* Special Verification for suicide */
      prt("Please verify QUITTING by typing the '@' sign: ", 0, MESSAGE_ROW);
      flush();
      i = inkey();
      prt("", 0, MESSAGE_ROW);
      if (i != '@') return;
   }

   /* Stop playing */
   alive = FALSE;

   /* Kill the player */
   death = TRUE;

   /* Create no ghost from this loser */
   suicide = TRUE;

   /* Cause of death */
   (void)strcpy(died_from, "Quitting");
}

/*
 * Save the game
 */
void do_cmd_save_game(bool with_level)
{
   /* Disturb the player */
   disturb(1, 0);

   /* Clear messages */
   msg_print(NULL);

   /* Handle stuff */
   handle_stuff();

   /* Refresh */
   Term_fresh();

   /* The player is not dead */
   (void)strcpy(died_from, "(saved)");

   /* Forbid suspend */
   signals_ignore_tstp();

   /* if we have save_levels, the level is saved as soon as           */
   /* we leave in dungeon.c:play_game(); if p_ptr->mdepth==0 this is  */
   /* also true                                                       */
   if ( (with_level == TRUE) ||
        (!save_levels && (p_ptr->mdepth != 0)) )
   {
      wr_level();
   }

   /* save the player */
   if (!save_player())
   {
      prt("Saving game... failed!", 0, MESSAGE_ROW);
   }
   else
     prt("",0,MESSAGE_ROW);

   /* Allow suspend again */
   signals_handle_tstp();

   /* Refresh */
   Term_fresh();

   /* Note that the player is not dead */
   (void)strcpy(died_from, "(alive and well)");
}

/*
 * Hack -- Calculates the total number of points earned     -JWT-
 */
long total_points(void)
{
   return (p_ptr->max_exp + (100 * p_ptr->max_dlv));
}



/*
 * Centers a string within a 31 character string            -JWT-
 */
static void center_string(char *buf, cptr str)
{
   s16b i, j;

   /* Total length */
   i = strlen(str);

   /* Necessary border */
   j = 15 - i / 2;

   /* Mega-Hack */
   (void)sprintf(buf, "%*s%s%*s", j, "", str, 31 - i - j, "");
}

/*
 * Display a "tomb-stone"
 */
static void print_tomb()
{
   cptr     p;
   char     tmp[160];
   char     buf[1024];
   FILE    *fp;

   time_t   ct = time((time_t)0);

   /* Clear the screen */
   clear_screen();

   /* Access the "dead" file */
   strcpy(buf, ANGBAND_DIR_FILE);
   strcat(buf, "dead.txt");

   /* Open the News file */
   fp = my_fopen(buf, "r");

   /* Dump */
   if (fp)
   {
      s16b i = 0;

      /* Dump the file to the screen */
      while (0 == my_fgets(fp, buf, 1024))
      {
         /* Display and advance */
         put_str(buf, 0, i++);
      }

      /* Close */
      my_fclose(fp);
   }


   /* King or Queen */
   if (total_winner || (p_ptr->lev > PY_MAX_LEVEL))
   {
      p = "Magnificent";
   }

   /* Normal */
   else
   {
      p =  player_title[p_ptr->pclass][(p_ptr->lev-1)/5];
   }
   /* 31 is a bit too wide here and overwrites part of the tomb */
   put_str(player_name, 27-strlen(player_name)/2, 5);

   center_string(buf, "the");
   put_str(buf, 11, 6);

   center_string(buf, p);
   put_str(buf, 11, 7);

   center_string(buf, cp_ptr->title);
   put_str(buf, 11, 8);

   (void)sprintf(tmp, "Level: %d", (int)p_ptr->lev);
   center_string(buf, tmp);
   put_str(buf, 11, 10);

   (void)sprintf(tmp, "Exp: %ld", (long)p_ptr->exp);
   center_string(buf, tmp);
   put_str(buf, 11, 11);

   (void)sprintf(tmp, "AU: %ld", (long)p_ptr->au);
   center_string(buf, tmp);
   put_str(buf, 11, 12);

   if (p_ptr->sdepth==0)
   {
      (void)sprintf(tmp, "Killed on Level %d", p_ptr->mdepth);
   }
   else
   {
      (void)sprintf(tmp, "Killed on Level %d/%d", p_ptr->mdepth,
                        dungeon.level_depth[p_ptr->sdepth]);
   }
   center_string(buf, tmp);

   put_str(buf, 10, 13);

   if (strlen(died_from) > 18)
   {
      char died_space[255];
      char *died_buf = died_space;
      s16b pos, i;

      (void)sprintf(died_space, "Killed by %s.", died_from);
      pos = 18;
      i=14;
      /* now for the rest of it */
      while (strlen(died_buf) > 18)
      {
         pos = 18;
         while (died_buf[pos] != ' ') pos--;
         died_buf[pos]=0;
         (void)sprintf(tmp, "%s", died_buf);
         center_string(buf, tmp);
         put_str(buf, 10, i++);
         died_buf = died_buf + pos + 1;
      }
      /* and the last part */
      (void)sprintf(tmp, "%s", died_buf);
      center_string(buf, tmp);
      put_str(buf, 10, i);
   }
   else
   {
      (void)sprintf(tmp, "Killed by %s.", died_from);
      center_string(buf, tmp);
      put_str(buf, 10, 14);
   }

   (void)sprintf(tmp, "%-.24s", ctime(&ct));
   center_string(buf, tmp);
   put_str(buf, 11, 17);
}


/*
 * Display some character info
 */
static void show_info(void)
{
   s16b            i, j, k;
   char            c;
   object_type    *i_ptr;

   store_type     *st_ptr;

   /* Hack -- Know everything in the inven/equip */
   for (i = 0; i < INVEN_TOTAL; i++)
   {
      i_ptr = &inventory[i];
      if (i_ptr->k_idx)
      {
         object_aware(i_ptr);
         object_known(i_ptr);
      }
   }

   /* Hack -- Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Handle stuff */
   handle_stuff();

   /* Flush all input keys */
   flush();

   /* Flush messages */
   msg_print(NULL);


   /* Describe options */
   prt("Press 'D' to dump this character record to a file.", 0, 21);
   c=inkey();
   c=FORCEUPPER(c);

   /* Dump character records as requested */
   while (c=='D')
   {
      char out_val[160];

      /* Prompt */
      put_str("Filename: ", 0, 23);

      /* Default */
      strcpy(out_val, "");

      /* Ask for filename (or abort) */
      if (!askfor_aux(out_val, 60)) return;

      /* Return means "show on screen" */
      if (!out_val[0]) break;

      /* Dump a character file */
      (void)file_character(out_val, FALSE);
      prt("Press 'D' to dump again to another file, any other key to continue.", 0, 21);
      c=inkey();
      c=FORCEUPPER(c);
   }

   /* Show player on screen */
   display_player(0, TRUE);

   /* Prompt for inventory */
   prt("Hit any key to see more information (ESC to abort): ", 0, 23);

   /* Allow abort at this point */
   if (inkey() == ESCAPE) return;


   /* Show equipment and inventory */

   /* Equipment -- if any */
   if (equip_cnt)
   {
      clear_screen();
      item_tester_full = TRUE;
      show_equip();
      if (death)
      {
         prt("You were using: -more-", 0, MESSAGE_ROW);
      }
      else
      {
         prt("You are using: -more-", 0, MESSAGE_ROW);
      }
      if (inkey() == ESCAPE) return;
   }
   else
   {
      if (death)
      {
         prt("You were using nothing. -more-", 0, MESSAGE_ROW);
      }
      else
      {
         prt("You are using nothing. -more-", 0, MESSAGE_ROW);
      }
      if (inkey() == ESCAPE) return;
   }

   /* Inventory -- if any */
   if (inven_cnt)
   {
      clear_screen();
      item_tester_full = TRUE;
      show_inven();
      if (death)
      {
         prt("You were carrying: -more-", 0, MESSAGE_ROW);
      }
      else
      {
         prt("You are carrying: -more-", 0, MESSAGE_ROW);
      }
      if (inkey() == ESCAPE) return;
   }
   else
   {
      if (death)
      {
         prt("You were carrying nothing. -more-", 0, MESSAGE_ROW);
      }
      else
      {
         prt("You are carrying nothing. -more-", 0, MESSAGE_ROW);
      }
      if (inkey() == ESCAPE) return;
   }

   /* Home is always store[0]. */
   st_ptr=&store[0];

   /* Hack -- Know everything in the home */
   for (i = 0; i < st_ptr->stock_num; i++)
   {
      i_ptr = &st_ptr->stock[i];
      if (i_ptr->k_idx)
      {
         object_aware(i_ptr);
         object_known(i_ptr);
      }
   }

   /* Home -- if anything there */
   if (st_ptr->stock_num)
   {
      /* Display contents of the home */
      for (k = 0, i = 0; i < st_ptr->stock_num; k++)
      {
         /* Clear the screen */
         clear_screen();

         /* Show 12 items */
         for (j = 0; (j < 12) && (i < st_ptr->stock_num); j++, i++)
         {
            char i_name[80];
            char tmp_val[80];

            /* Acquire item */
            i_ptr = &st_ptr->stock[i];

            /* Print header, clear line */
            sprintf(tmp_val, "%c) ", I2A(j));
            prt(tmp_val, 4, j + 2);

            /* Display object description */
            object_desc(i_name, i_ptr, TRUE, 3);
            c_put_str(tval_to_attr[i_ptr->tval], i_name, 7, j+2);
         }

         /* Caption */
         prt(format("Your home contains (page %d): -more-", k+1), 0, MESSAGE_ROW);

         /* Wait for it */
         if (inkey() == ESCAPE) return;
      }
   }
   else
   {
      prt("Your home contains nothing. -more-", 0, MESSAGE_ROW);
      if (inkey() == ESCAPE) return;
   }
}

/*
 * The "highscore" file descriptor, if available.
 */
static s16b highscore_fd = -1;


/*
 * Seek score 'i' in the highscore file
 */
static s16b highscore_seek(s16b i)
{
   /* Seek for the requested record */
   return (fd_seek(highscore_fd, (huge)(i) * sizeof(high_score)));
}


/*
 * Read one score from the highscore file
 */
static errr highscore_read(high_score *score)
{
   /* Read the record, note failure */
   return (fd_read(highscore_fd, (char*)(score), sizeof(high_score)));
}


/*
 * Write one score to the highscore file
 */
static s16b highscore_write(high_score *score)
{
   /* Write the record, note failure */
   return (fd_write(highscore_fd, (char*)(score), sizeof(high_score)));
}




/*
 * Just determine where a new score *would* be placed
 * Return the location (0 is best) or -1 on failure
 */
static s16b highscore_where(high_score *score)
{
   s16b            i;

   high_score        the_score;

   /* Paranoia -- it may not have opened */
   if (highscore_fd < 0) return (-1);

   /* Go to the start of the highscore file */
   if (highscore_seek(0)) return (-1);

   /* Read until we get to a higher score */
   for (i = 0; i < MAX_HISCORES; i++)
   {
      if (highscore_read(&the_score)) return (i);
      if (strcmp(the_score.pts, score->pts) < 0) return (i);
   }

   /* The "last" entry is always usable */
   return (MAX_HISCORES - 1);
}


/*
 * Actually place an entry into the high score file
 * Return the location (0 is best) or -1 on "failure"
 */
static s16b highscore_add(high_score *score)
{
   s16b            i, slot;
   bool           done = FALSE;

   high_score        the_score, tmpscore;


   /* Paranoia -- it may not have opened */
   if (highscore_fd < 0) return (-1);

   /* Determine where the score should go */
   slot = highscore_where(score);

   /* Hack -- Not on the list */
   if (slot < 0) return (-1);

   /* Hack -- prepare to dump the new score */
   the_score = (*score);

   /* Slide all the scores down one */
   for (i = slot; !done && (i < MAX_HISCORES); i++)
   {
      /* Read the old guy, note errors */
      if (highscore_seek(i)) return (-1);
      if (highscore_read(&tmpscore)) done = TRUE;

      /* Back up and dump the score we were holding */
      if (highscore_seek(i)) return (-1);
      if (highscore_write(&the_score)) return (-1);

      /* Hack -- Save the old score, for the next pass */
      the_score = tmpscore;
   }

   /* Return location used */
   return (slot);
}

/*
 * Display the scores in a given range.
 * Assumes the high score list is already open.
 * Only five entries per line, too much info.
 *
 * Mega-Hack -- allow "fake" entry at the given position.
 */
static void display_scores_aux(s16b from, s16b to, s16b note, high_score *score)
{
   s16b      i, j, k, n, attr, place;

   high_score  the_score;

   char     out_val[256];
   char     tmp_val[160];


   /* Paranoia -- it may not have opened */
   if (highscore_fd < 0) return;


   /* Assume we will show the first 10 */
   if (from < 0) from = 0;
   if (to < 0) to = 10;
   if (to > MAX_HISCORES) to = MAX_HISCORES;


   /* Seek to the beginning */
   if (highscore_seek(0)) return;

   /* Hack -- Count the high scores */
   for (i = 0; i < MAX_HISCORES; i++)
   {
      if (highscore_read(&the_score)) break;
   }

   /* Hack -- allow "fake" entry to be last */
   if ((note == i) && score) i++;

   /* Forget about the last entries */
   if (i > to) i = to;


   /* Show 5 per page, until "done" */
   for (k = from, place = k+1; k < i; k += 5)
   {
      /* Clear those */
      clear_screen();

      /* Title */
      put_str("             Angband Hall of Fame", 0, 0);

      /* Indicate non-top scores */
      if (k > 0)
      {
         sprintf(tmp_val, "(from position %d)", k + 1);
         put_str(tmp_val, 40, 0);
      }

      /* Dump 5 entries */
      for (j = k, n = 0; j < i && n < 5; place++, j++, n++)
      {
         s16b pr, pc, clev, mlev, cdun, mdun;

         cptr user, gold, when, aged;


         /* Hack -- indicate death in yellow */
         attr = (j == note) ? TERM_YELLOW : TERM_WHITE;


         /* Mega-Hack -- insert a "fake" record */
         if ((note == j) && score)
         {
            the_score = (*score);
            attr = TERM_L_GREEN;
            score = NULL;
            note = -1;
            j--;
         }

         /* Read a normal record */
         else
         {
            /* Read the proper record */
            if (highscore_seek(j)) break;
            if (highscore_read(&the_score)) break;
         }

         /* Extract the race/class */
         pr = atoi(the_score.p_r);
         pc = atoi(the_score.p_c);

         /* Extract the level info */
         clev = atoi(the_score.cur_lev);
         mlev = atoi(the_score.max_lev);
         cdun = atoi(the_score.cur_dun);
         mdun = atoi(the_score.max_dun);

         /* Hack -- extract the gold and such */
         for (user = the_score.uid; isspace((int)*user); user++) ;
         for (when = the_score.day; isspace((int)*when); when++) ;
         for (gold = the_score.gold; isspace((int)*gold); gold++) ;
         for (aged = the_score.turns; isspace((int)*aged); aged++) ;

         /* Dump some info */
         sprintf(out_val, "%3d.%9s  %s the %s %s, Level %d",
               place, the_score.pts, the_score.who,
               race_info[pr].title, class_info[pc].title,
               clev);

         /* Append a "maximum level" */
         if (mlev > clev) strcat(out_val, format(" (Max %d)", mlev));

         /* Dump the first line */
         c_put_str(attr, out_val, 0, n*4 + 2);

         /* Another line of info */
         sprintf(out_val, "             Killed by %s on %s %d",
               the_score.how, "Dungeon Level", cdun);

         /* Hack -- some people die in the town */
         if (!cdun)
         {
            sprintf(out_val, "             Killed by %s in the Town",
                  the_score.how);
         }

         /* Append a "maximum level" */
         if (mdun > cdun) strcat(out_val, format(" (Max %d)", mdun));

         /* Dump the info */
         c_put_str(attr, out_val, 0, n*4 + 3);

         /* And still another line of info */
         sprintf(out_val,
               "           (User %s, Date %s, Gold %s, Turn %s).",
               user, when, gold, aged);
         c_put_str(attr, out_val, 0, n*4 + 4);
      }


      /* Wait for response. */
      prt("[Press ESC to quit, any other key to continue.]", 17, 23);
      j = inkey();
      prt("", 0, 23);

      /* Hack -- notice Escape */
      if (j == ESCAPE) break;
   }
}


/*
 * Hack -- Display the scores in a given range and quit.
 *
 * This function is only called from "main.c" when the user asks
 * to see the "high scores".
 */
void display_scores(s16b from, s16b to)
{
   char buf[1024];

   /* Access the high score file */
   strcpy(buf, ANGBAND_DIR_APEX);
   strcat(buf, "scores.raw");

   /* Open the binary high score file, for reading */
   highscore_fd = fd_open(buf, O_RDONLY);

   /* Paranoia -- No score file */
   if (highscore_fd < 0) quit("Score file unavailable.");

   /* Clear screen */
   clear_screen();

   /* Display the scores */
   display_scores_aux(from, to, -1, NULL);

   /* Shut the high score file */
   (void)fd_close(highscore_fd);

   /* Forget the high score fd */
   highscore_fd = -1;

   /* Quit */
   quit(NULL);
}



/*
 * Enters a players name on a hi-score table, if "legal", and in any
 * case, displays some relevant portion of the high score list.
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
static errr top_twenty(void)
{
   s16b          j;

   high_score   the_score;

   time_t ct = time((time_t*)0);

   /* Wipe screen */
   clear_screen();

   /* No score file */
   if (highscore_fd < 0)
   {
      msg_print("Score file unavailable.");
      msg_print(NULL);
      return (0);
   }

#ifndef SCORE_WIZARDS
   /* Wizard-mode pre-empts scoring */
   if (noscore & 0x000F)
   {
      msg_print("Score not registered for wizards.");
      msg_print(NULL);
      display_scores_aux(0, 10, -1, NULL);
      return (0);
   }
#endif

#ifndef SCORE_BORGS
   /* Borg-mode pre-empts scoring */
   if (noscore & 0x00F0)
   {
      msg_print("Score not registered for borgs.");
      msg_print(NULL);
      display_scores_aux(0, 10, -1, NULL);
      return (0);
   }
#endif

#ifndef SCORE_CHEATERS
   /* Cheaters are not scored */
   if (noscore & 0xFF00)
   {
      msg_print("Score not registered for cheaters.");
      msg_print(NULL);
      display_scores_aux(0, 10, -1, NULL);
      return (0);
   }
#endif

   /* Interupted */
   if (!total_winner && streq(died_from, "Interrupting"))
   {
      msg_print("Score not registered due to interruption.");
      msg_print(NULL);
      display_scores_aux(0, 10, -1, NULL);
      return (0);
   }

   /* Quitter */
   if (!total_winner && streq(died_from, "Quitting"))
   {
      msg_print("Score not registered due to quitting.");
      msg_print(NULL);
      display_scores_aux(0, 10, -1, NULL);
      return (0);
   }


   /* Clear the record */
   WIPE(&the_score, high_score);

   /* Save the version */
   sprintf(the_score.what, "%u.%u.%u",
           VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);

   /* Calculate and save the points */
   sprintf(the_score.pts, "%9lu", (long)total_points());
   the_score.pts[9] = '\0';

   /* Save the current gold */
   sprintf(the_score.gold, "%9lu", (long)p_ptr->au);
   the_score.gold[9] = '\0';

   /* Save the current turn */
   sprintf(the_score.turns, "%9lu", (long)turn);
   the_score.turns[9] = '\0';

#ifdef HIGHSCORE_DATE_HACK
   /* Save the date in a hacked up form (9 chars) */
   sprintf(the_score.day, "%-.6s %-.2s", ctime(&ct) + 4, ctime(&ct) + 22);
#else
   /* Save the date in standard form (8 chars) */
   strftime(the_score.day, 9, "%m%d%Y", localtime(&ct));
#endif

   /* Save the player name (15 chars) */
   sprintf(the_score.who, "%-.15s", player_name);

   /* Save the player info */
   sprintf(the_score.uid, "%7u", player_uid);
   sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
   sprintf(the_score.p_r, "%2d", p_ptr->prace);
   sprintf(the_score.p_c, "%2d", p_ptr->pclass);

   /* Save the level and such */
   sprintf(the_score.cur_lev, "%3d", p_ptr->lev);
   sprintf(the_score.cur_dun, "%3d", p_ptr->mdepth);
   sprintf(the_score.max_lev, "%3d", p_ptr->max_plv);
   sprintf(the_score.max_dun, "%3d", p_ptr->max_dlv);

   /* Save the cause of death (31 chars) */
   sprintf(the_score.how, "%-.31s", died_from);

   /* Lock (for writing) the highscore file, or fail */
   if (fd_lock(highscore_fd, F_WRLCK)) return (1);

   /* Add a new entry to the score list, see where it went */
   j = highscore_add(&the_score);

   /* Unlock the highscore file, or fail */
   if (fd_lock(highscore_fd, F_UNLCK)) return (1);

   /* Hack -- Display the top fifteen scores */
   if (j < 10)
   {
      display_scores_aux(0, 15, j, NULL);
   }

   /* Display the scores surrounding the player */
   else
   {
      display_scores_aux(0, 5, j, NULL);
      display_scores_aux(j - 2, j + 7, j, NULL);
   }

   /* Success */
   return (0);
}

/*
 * Predict the players location, and display it.
 */
static errr predict_score(void)
{
   s16b          j;

   high_score   the_score;

   /* No score file */
   if (highscore_fd < 0)
   {
      msg_print("Score file unavailable.");
      msg_print(NULL);
      return (0);
   }

   /* Save the version */
   sprintf(the_score.what, "%u.%u.%u",
           VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);

   /* Calculate and save the points */
   sprintf(the_score.pts, "%9lu", (long)total_points());

   /* Save the current gold */
   sprintf(the_score.gold, "%9lu", (long)p_ptr->au);

   /* Save the current turn */
   sprintf(the_score.turns, "%9lu", (long)turn);

   /* Hack -- no time needed */
   strcpy(the_score.day, "TODAY");

   /* Save the player name (15 chars) */
   sprintf(the_score.who, "%-.15s", player_name);

   /* Save the player info */
   sprintf(the_score.uid, "%7u", player_uid);
   sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
   sprintf(the_score.p_r, "%2d", p_ptr->prace);
   sprintf(the_score.p_c, "%2d", p_ptr->pclass);

   /* Save the level and such */
   sprintf(the_score.cur_lev, "%3d", p_ptr->lev);
   sprintf(the_score.cur_dun, "%3d", p_ptr->mdepth);
   sprintf(the_score.max_lev, "%3d", p_ptr->max_plv);
   sprintf(the_score.max_dun, "%3d", p_ptr->mdepth);

   /* Hack -- no cause of death */
   strcpy(the_score.how, "nobody (yet!)");

   /* See where the entry would be placed */
   j = highscore_where(&the_score);

   /* Hack -- Display the top fifteen scores */
   if (j < 10)
   {
      display_scores_aux(0, 15, j, &the_score);
   }

   /* Display some "useful" scores */
   else
   {
      display_scores_aux(0, 5, -1, NULL);
      display_scores_aux(j - 2, j + 7, j, &the_score);
   }

   /* Success */
   return (0);
}

/*
 * Change the player into a King!                       -RAK-
 */
static void kingly()
{
   /* Hack -- retire in town */
   p_ptr->mdepth = 0;
   p_ptr->sdepth = 0;

   /* Fake death */
   (void)strcpy(died_from, "Ripe Old Age");

   /* Restore the experience */
   p_ptr->exp = p_ptr->max_exp;

   /* Restore the level */
   p_ptr->lev = p_ptr->max_plv;

   /* Check the experience */
   /* check_experience(); */

   /* Mega-Hack -- Instant Experience */
   p_ptr->exp += 5000000L;
   p_ptr->max_exp += 5000000L;

   /* Mega-Hack -- Instant Gold */
   p_ptr->au += 250000L;

   /* Automatic level 50 */
   /* p_ptr->lev = 50; */

   /* Display a crown */
   clear_screen();
   put_str("#", 34, 1);
   put_str("#####", 32, 2);
   put_str("#", 34, 3);
   put_str(",,,  $$$  ,,,", 28, 4);
   put_str(",,=$   \"$$$$$\"   $=,,", 24, 5);
   put_str(",$$        $$$        $$,", 22, 6);
   put_str("*>         <*>         <*", 22, 7);
   put_str("$$         $$$         $$", 22, 8);
   put_str("\"$$        $$$        $$\"", 22, 9);
   put_str("\"$$       $$$       $$\"", 23, 10);
   put_str("*#########*#########*", 24, 11);
   put_str("*#########*#########*", 24, 12);

   /* Display a message */
   put_str("Veni, Vidi, Vici!", 26, 15);
   put_str("I came, I saw, I conquered!", 21, 16);
   put_str(format("All Hail the Mighty %s!", sp_ptr->winner), 22, 17);

   /* Flush input */
   flush();

   /* Wait for response */
   pause_line(23);
}

/*
 * Close up the current game (player may or may not be dead)
 *
 * This function is called only from "main.c" and "signals.c".
 */
void close_game(bool with_level)
{
   char buf[1024];

   /* Handle stuff */
   handle_stuff();

   /* Flush the messages */
   msg_print(NULL);

   /* Flush the input */
   flush();

   /* No suspending now */
   signals_ignore_tstp();

   /* Hack -- Character is now "icky" */
   character_icky = TRUE;

   /* Access the high score file */
   strcpy(buf, ANGBAND_DIR_APEX);
   strcat(buf, "scores.raw");

   /* Open the high score file, for reading/writing */
   highscore_fd = fd_open(buf, O_RDWR);

   /* Handle death */
   if (death)
   {
      s16b i;

dlog(DEBUGGHOST,"files.c: close_game: dead (%s), calling make_new_ghost\n", died_from);
      if (!suicide) (void)make_new_ghost();
dlog(DEBUGGHOST,"files.c: close_game: dead, called make_new_ghost\n");

      /* Handle retirement */
      if (total_winner) kingly();

/* jk - who can explain why a dead player is saved? */

      /* You are dead */
      print_tomb();

      /* Show more info */
      show_info();

/* jk - remove current level + all levels that are saved */
      if (remove_levelfiles)
      {
         rm_level(p_ptr->mdepth);
         /* don't remove level 0, the town! */
         for (i = 1; i < MAX_LEVEL; i++)
         {
            if (level_info[i].saved)
            {
               rm_level(i);
            }
         }
      }
      rm_savefile();

      /* Handle score, show Top scores */
      top_twenty();
   }

   /* Still alive */
   else
   {
      /* Save the game */
      do_cmd_save_game(with_level);

      /* Prompt for scores XXX XXX XXX */
      prt("Press Return for score (or Escape to quit).", 0, MESSAGE_ROW);

      /* Predict score (or ESCAPE) */
      if (inkey() != ESCAPE) predict_score();
   }

   /* Shut the high score file */
   (void)fd_close(highscore_fd);

   /* Forget the high score fd */
   highscore_fd = -1;

   /* Allow suspending now */
dlog(DEBUGFLOW,"files.c: close_game: about to end game\n");
   fprintf(stderr,"Angband/64 beta %d rel %d (Angband version %d.%d.%d). Normal exit.\n",
               VERSION_BETA, VERSION_RELEASE, VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
   message_flush();
   signals_handle_tstp();
}

/*
 * Handle abrupt death of the visual system
 *
 * This routine is called only in very rare situations, and only
 * by certain visual systems, when they experience fatal errors.
 *
 * XXX XXX Hack -- clear the death flag when creating a HANGUP
 * save file so that player can see tombstone when restart.
 */
void exit_game_panic(void)
{
   /* If nothing important has happened, just quit */
   if (!character_generated || character_saved) quit("panic");

   /* Mega-Hack -- see "msg_print()" */
   msg_flag = FALSE;

   /* Clear the top line */
   prt("", 0, MESSAGE_ROW);

   /* Hack -- turn off some things */
   disturb(1, 0);

   /* Mega-Hack -- Delay death */
   if (p_ptr->chp < 0) death = FALSE;

   /* Hardcode panic save */
   panic_save = 1;

   /* Forbid suspend */
   signals_ignore_tstp();

   /* Indicate panic save */
   (void)strcpy(died_from, "(panic save)");

   /* Panic save, or get worried */
   if (!wr_level()) quit("panic save of level failed!");
   if (!save_player()) quit("panic save of player failed!");

   message_flush();

   /* Successful panic save */
   quit("panic save succeeded!");
}

#ifdef HANDLE_SIGNALS

#include <signal.h>

/*
 * Handle signals -- suspend
 *
 * Actually suspend the game, and then resume cleanly
 */
static void handle_signal_suspend(int sig)
{
   /* Disable handler */
   (void)signal(sig, SIG_IGN);

#ifdef SIGSTOP

   /* Flush output */
   Term_fresh();

   /* Suspend the "Term" */
   Term_xtra(TERM_XTRA_ALIVE, 0);

   /* Suspend ourself */
   (void)kill(0, SIGSTOP);

   /* Resume the "Term" */
   Term_xtra(TERM_XTRA_ALIVE, 1);

   /* Redraw the term */
   Term_redraw();

   /* Flush the term */
   Term_fresh();

#endif

   /* Restore handler */
   (void)signal(sig, handle_signal_suspend);
}


/*
 * Handle signals -- simple (interrupt and quit)
 *
 * This function was causing a *huge* number of problems, so it has
 * been simplified greatly.  We keep a global variable which counts
 * the number of times the user attempts to kill the process, and
 * we commit suicide if the user does this a certain number of times.
 *
 * We attempt to give "feedback" to the user as he approaches the
 * suicide thresh-hold, but without penalizing accidental keypresses.
 *
 * To prevent messy accidents, we should reset this global variable
 * whenever the user enters a keypress, or something like that.
 */
static void handle_signal_simple(int sig)
{
   /* Disable handler */
   (void)signal(sig, SIG_IGN);

   /* Nothing to save, just quit */
   if (!character_generated || character_saved) quit(NULL);

   wiz_create_crash();

   /* Count the signals */
   signal_count++;

   /* Terminate dead characters */
   if (death)
   {
      /* Mark the savefile */
      (void)strcpy(died_from, "Abortion");

      /* Close stuff */
      close_game(TRUE);

      /* Quit */
      quit("interrupt");
   }

   /* Allow suicide (after 5) */
   else if (signal_count >= 5)
   {
      /* Cause of "death" */
      (void)strcpy(died_from, "Interrupting");

      if (kill_savefile_interrupt)
      {
         /* Stop playing */
         alive = FALSE;

         /* Suicide */
         death = TRUE;
      }

      /* Close stuff */
      close_game(TRUE);

      /* trust Billy not to have implemented sync - a crash should be a real crash :-) */
#ifndef WINDOWS
      /* jk - try to close all files the hard way */
      sync();
#endif

      /* Quit */
      quit("interrupt");
   }

   /* Give warning (after 4) */
   else if (signal_count >= 4)
   {
      /* Make a noise */
      Term_xtra(TERM_XTRA_NOISE, 0);

      /* Clear the top line */
      Term_erase(0, MESSAGE_ROW, 80);

      /* Display the cause */
      Term_putstr(0, MESSAGE_ROW, -1, TERM_WHITE, "Contemplating suicide!");

      /* Flush */
      Term_fresh();
   }

   /* Give warning (after 2) */
   else if (signal_count >= 2)
   {
      /* Make a noise */
      Term_xtra(TERM_XTRA_NOISE, 0);
   }

   /* Restore handler */
   (void)signal(sig, handle_signal_simple);
}

/* can we still print entries on the calling stack or have we finished? */
static bool continue_stack_trace = TRUE;

#define MAX_STACK_ADDR 90

#ifdef WINDOWS
static u32b baseframe = 0L;
#endif

/* __builtin_return_address needs a constant, so this cannot be in a loop */

/* windows (cygwin; mingw32 doesn't do *any* signal stuff!)  */
/* is a bit braindead - __builtin_frame_address doesn't get  */
/* 0 when the stack is empty, and __builtin_return_address   */
/* just crashes - so there no way to know when to stop (!)   */
/* this is solved by saying that we need to stay in the same */
/* frame. This seems to work for ^a C - and I hope this will */
/* work for other instances as well.                         */
#ifdef WINDOWS
#define get_1_address(X) \
   if (baseframe == 0L) \
   { \
      baseframe=(unsigned long)__builtin_frame_address(0); \
      baseframe=((baseframe & 0xffff0000L) >> 16); \
      dlog(DEBUGSAVE,"files.c: handle_signal_abort: baseframe now %08lx\n", \
           baseframe); \
   } \
   if (continue_stack_trace && \
       ((((unsigned long)__builtin_frame_address((X)) & 0xffff0000L)>>16) == baseframe) && \
       ((X) < MAX_STACK_ADDR)) \
   { \
      stack_addr[(X)]= (unsigned long)__builtin_return_address((X)); \
      dlog(DEBUGSAVE,"files.c: handle_signal_abort: stack %d %08lx frame %d %08lx\n", \
                      (X), __builtin_return_address((X)), (X), __builtin_frame_address((X))); \
   } \
   else if (continue_stack_trace) \
   { \
      continue_stack_trace = FALSE; \
   }
#endif

#ifndef WINDOWS
#define get_1_address(X) \
   if (continue_stack_trace && ((unsigned long)__builtin_frame_address((X)) != 0L) && ((X) < MAX_STACK_ADDR)) \
   { \
      stack_addr[(X)]= (unsigned long)__builtin_return_address((X)); \
      dlog(DEBUGSAVE,"files.c: handle_signal_abort: stack %d %08lx frame %d %08lx\n", \
                      (X), __builtin_return_address((X)), (X), __builtin_frame_address((X))); \
   } \
   else if (continue_stack_trace) \
   { \
      continue_stack_trace = FALSE; \
   }
#endif


/*
 * Handle signal -- abort, kill, etc
 */
static void handle_signal_abort(int sig)
{
   bool                 save_ok = FALSE;

   FILE                *fff = NULL;
   char                 filename[1024];
   s16b                 i;
   bool                 dump_ok = FALSE;

   /* Clear the bottom lines */
   Term_erase(0, 20, 80);
   Term_erase(0, 21, 80);
   Term_erase(0, 22, 80);

   /* Give a warning */
   Term_putstr(1, 20, -1, TERM_RED, "You suddenly see a gruesome SOFTWARE BUG leap for your throat!");
   Term_xtra(TERM_XTRA_NOISE, 0);

   /* Access the help file */
   strcpy(filename, ANGBAND_DIR_USER);
   strcat(filename, "crash.txt");

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "text file" */
   _ftype = 'TEXT';
#endif

   /* Drop priv's */
   safe_setuid_drop();

   /* Open the non-existing file */
   fff = my_fopen(filename, "w");

   /* Grab priv's */
   safe_setuid_grab();

   /* Invalid file */
   if (fff)
   {
      fprintf(fff,"Your game has just crashed. Please forward the following\n");
      fprintf(fff,"information to the maintainer (email to thunder7@xs4all.nl)\n\n");
      fprintf(fff,"\nAlso, please add any information you feel is relevant:\n");
      fprintf(fff,"especially, what were you doing at the time this happened?\n\n");
      fprintf(fff,"Angband/64 beta %d release %d (%d.%d.%d)\n\n",
                  VERSION_BETA, VERSION_RELEASE, VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
      fprintf(fff,"STACK TRACE:\n\n");

      dump_stack(fff);

      fprintf(fff,"\nCONFIGURATION:\n\n");
      fprintf(fff, "debuglevel 0x%08lx\n", debuglevel);

#ifdef ALLOW_COMPRESSION
      fprintf(fff,"compression support compiled in\n");
#endif

#ifdef USE_OTHER_RAND
      fprintf(fff,"using other RNG\n");
#endif

#ifdef MONSTER_FLOW
      fprintf(fff,"monster flow support compiled in\n");
#endif

      fprintf(fff,"\nDISPLAY MODULES:\n\n");

#ifdef USE_XAW
      fprintf(fff,"main-xaw compiled in\n");
#endif

#ifdef USE_X11
      fprintf(fff,"main-x11 compiled in\n");
#endif

#ifdef USE_GCU
      fprintf(fff,"main-gcu compiled in\n");
#endif

#ifdef USE_PDC
      fprintf(fff,"main-pdc compiled in\n");
#endif

#ifdef USE_CAP
      fprintf(fff,"main-cap compiled in\n");
#endif

#ifdef USE_IBM
      fprintf(fff,"main-ibm compiled in\n");
#endif

#ifdef USE_DOS
      fprintf(fff,"main-dos compiled in\n");
#endif

#ifdef __EMX__
      fprintf(fff,"main-emx compiled in\n");
#endif

#ifdef USE_SLA
      fprintf(fff,"main-sla compiled in\n");
#endif

#ifdef USE_LSL
      fprintf(fff,"main-lsl compiled in\n");
#endif

#ifdef USE_AMI
      fprintf(fff,"main-ami compiled in\n");
#endif

#ifdef USE_VME
      fprintf(fff,"main-vme compiled in\n");
#endif

      fprintf(fff,"\nOPTIONS SET:\n\n");
      for (i = 0; options[i].descr; i++)
      {
          if (*options[i].variable == TRUE)
          {
             my_fputs(fff, format("%s", options[i].descr), 0);
           }
      }
      fclose(fff);
      Term_putstr(1, 21, -1, TERM_YELLOW, format("Please email %s to THUNDER7@XS4ALL.NL !", filename));
      Term_xtra(TERM_XTRA_NOISE, 0);
      Term_fresh();
      dump_ok = TRUE;
   }
   else
   {
      Term_putstr(1, 21, -1, TERM_YELLOW, "crashdump failed, trying emergency save!");
      Term_xtra(TERM_XTRA_NOISE, 0);
      Term_fresh();
   }

   /* only try panic save if there is anything to save */
   if (character_generated && !character_saved)
   {
      /* Message */
      Term_putstr(1, 22, -1, TERM_RED, "Trying Panic save...");

      /* Flush output */
      Term_fresh();

      /* Panic Save */
      panic_save = 1;

      /* Panic save */
      (void)strcpy(died_from, "(panic save)");

      /* Forbid suspend */
      signals_ignore_tstp();

      /* Attempt to save */
      save_ok=save_player();
      save_ok|=wr_level();
      if (save_ok)
      {
         Term_putstr(24, 22, -1, TERM_RED, "Panic save succeeded!");
         save_ok = TRUE;
      }

      /* Save failed */
      else
      {
         Term_putstr(24, 22, -1, TERM_RED, "Panic save failed!");
      }

      /* Flush output */
      Term_fresh();

      message_flush();
      pause_line(23);
   }
   /* Quit */

   if (dump_ok && save_ok)
   {
      quit("SOFTWARE BUG: savefile written, crashdump saved.");
   }
   else if (dump_ok)
   {
      quit("SOFTWARE BUG: savefile NOT written, crashdump saved.");
   }
   else if (dump_ok)
   {
      quit("SOFTWARE BUG: savefile written, crashdump NOT saved.");
   }
   else
   {
      quit("SOFTWARE BUG: savefile NOT written, crashdump NOT saved.");
   }

}

#ifdef DEBUGDUMP
/* this is a adapted version of addr2line */
/* addr2line.c -- convert addresses to line number and function name
   Copyright 1997, 98, 99, 2000 Free Software Foundation, Inc.
   Contributed by Ulrich Lauther <Ulrich.Lauther@zfe.siemens.de>

   This file is part of GNU Binutils.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* Look for an address in a section.  This is called via bfd_map_over_sections.  */
static void find_address_in_section (bfd *abfd, asection *section, PTR data)
{
  bfd_vma vma;
  bfd_size_type size;

  if (dump_found) return;

  if ((bfd_get_section_flags (abfd, section) & SEC_ALLOC) == 0)
  {
    return;
  }

  vma = bfd_get_section_vma (abfd, section);
  if (dump_pc < vma)
  {
    return;
  }

  size = bfd_get_section_size_before_reloc (section);
  if (dump_pc >= vma + size)
  {
    return;
  }

  dump_found = bfd_find_nearest_line (abfd, section, dump_syms, dump_pc - vma,
				 &dump_filename, &functionname, &dump_line);
}

/* Read hexadecimal addresses from stdin, translate into
   file_name:line_number and optionally function name.  */
/* changed, it takes a single address as argument */
static void translate_address (bfd *abfd, int address,
                               char *function_name, char *source_name, int *source_line)
{
   char addr_hex[100];
   sprintf(addr_hex,"%x", address);

   dump_pc = bfd_scan_vma (addr_hex, NULL, 16);

   dump_found = false;
   bfd_map_over_sections (abfd, find_address_in_section, (PTR) NULL);

   if (! dump_found)
   {
      strcpy(function_name, "??");
      strcpy(source_name, "??");
      *source_line = 0;
   }
   else
   {
      if (functionname == NULL || *functionname == '\0')
      {
         strcpy(function_name, "??");
         if (dump_filename == NULL)
         {
            strcpy(source_name, "??");
         }
         else
         {
            strcpy(source_name, dump_filename);
         }
         *source_line = dump_line;
      }
      else
      {
         strcpy(function_name, functionname);
         if (dump_filename == NULL)
         {
            strcpy(source_name, "??");
         }
         else
         {
            strcpy(source_name, dump_filename);
         }
         *source_line = dump_line;
      }
   }
}

/*
 * Split a line with comma-separated values in tokens
 * This differs from tokenize in the adaptable set of delimiters
 * and the handling of quotes, which is absent here.
 */
s16b split_to_tokens(char *buf, char *delim, s16b max, char **tokens)
{
   s16b  i = 0;
   char *s = buf;

   /* Process */
   while (i < max - 1)
   {
      char *t;

      /* Scan the string */
      for (t = s; *t; t++)
      {
         /* Found a delimiter */
         if ( strchr(delim, *t) != NULL) break;
      }

      /* Nothing left */
      if (!*t) break;

      /* Nuke and advance */
      *t++ = '\0';

      /* Save the token */
      tokens[i++] = s;

      /* Advance */
      s = t;
   }

   /* Save the token */
   tokens[i++] = s;

   /* Number found */
   return (i);
}

/*
 * this function dumps the stack, and interprets the values.
 * note that the output is always written to the log-file, 
 * and fff may be NULL.
 */
void dump_stack(FILE *fff)
{
   s16b i;
   static unsigned long stack_addr[MAX_STACK_ADDR];
   bfd *abfd;
   char **matching;
   const char **targets, **pp;
   long storage = 0;
   long symcount = 0;
   bool bfd_error = FALSE;

   bfd_init();
   dlog(DEBUGDUMP,"files.c: dump_stack: bfd supported targets: ");
   targets = bfd_target_list();
   for (pp = targets; *pp != NULL; pp++)
   {
      dlog(DEBUGDUMP,"%s ", *pp);
   }
   free(targets);
   dlog(DEBUGDUMP,"\n");

   /* clean the stack addresses if necessary */
   for (i=0; i < MAX_STACK_ADDR; i++)
   {
      stack_addr[i] = (unsigned long)0;
   }
   get_1_address(0);  get_1_address(1);  get_1_address(2);  get_1_address(3);  get_1_address(4);
   get_1_address(5);  get_1_address(6);  get_1_address(7);  get_1_address(8);  get_1_address(9);
   get_1_address(10); get_1_address(11); get_1_address(12); get_1_address(13); get_1_address(14);
   get_1_address(15); get_1_address(16); get_1_address(17); get_1_address(18); get_1_address(19);
   get_1_address(20); get_1_address(21); get_1_address(22); get_1_address(23); get_1_address(24);
   get_1_address(25); get_1_address(26); get_1_address(27); get_1_address(28); get_1_address(29);
   get_1_address(30); get_1_address(31); get_1_address(32); get_1_address(33); get_1_address(34);
   get_1_address(35); get_1_address(36); get_1_address(37); get_1_address(38); get_1_address(39);
   get_1_address(40); get_1_address(41); get_1_address(42); get_1_address(43); get_1_address(44);
   get_1_address(45); get_1_address(46); get_1_address(47); get_1_address(48); get_1_address(49);
   get_1_address(50); get_1_address(51); get_1_address(52); get_1_address(53); get_1_address(54);
   get_1_address(55); get_1_address(56); get_1_address(57); get_1_address(58); get_1_address(59);
   get_1_address(60); get_1_address(61); get_1_address(62); get_1_address(63); get_1_address(64);
   get_1_address(65); get_1_address(66); get_1_address(67); get_1_address(68); get_1_address(69);
   get_1_address(70); get_1_address(71); get_1_address(72); get_1_address(73); get_1_address(74);
   get_1_address(75); get_1_address(76); get_1_address(77); get_1_address(78); get_1_address(79);
   get_1_address(80); get_1_address(81); get_1_address(82); get_1_address(83); get_1_address(84);
   get_1_address(85); get_1_address(86); get_1_address(87); get_1_address(88); get_1_address(89);

   /* dump stack frame */
   i = MAX_STACK_ADDR-1;
   while ( (i >=0) && (stack_addr[i] == 0)) i--;

   if (i < 0)
   {
      dlog(DEBUGALWAYS,"files.c: dump_stack: unable to get any addresses off the stack.\n");
      return;
   }
#if defined(linux)
   dlog(DEBUGDUMP,"files.c: dump_stack: bfd_set_default_target elf32-i386\n");
   if (!bfd_set_default_target("elf32-i386"))
   {
      cptr errmsg = bfd_errmsg( bfd_get_error() );
      dlog(DEBUGDUMP,"files.c: dump_stack: bfd_set_default_target return-value != 0; bfd error = %s\n", errmsg);
      bfd_error = TRUE;
   }
#endif
#if defined(dos)
   dlog(DEBUGDUMP,"files.c: dump_stack: bfd_set_default_target coff-go32-exe\n");
   if (!bfd_set_default_target("coff-go32-exe"))
   {
      cptr errmsg = bfd_errmsg( bfd_get_error() );
      dlog(DEBUGDUMP,"files.c: dump_stack: bfd_set_default_target return-value != 0; bfd error = %s\n", errmsg);
      bfd_error = TRUE;
   }
#endif
#if defined(WINDOWS)
   dlog(DEBUGDUMP,"files.c: dump_stack: bfd_set_default_target pei-i386\n");
   if (!bfd_set_default_target("pei-i386"))
   {
      cptr errmsg = bfd_errmsg( bfd_get_error() );
      dlog(DEBUGDUMP,"files.c: dump_stack: bfd_set_default_target return-value != 0; bfd error = %s\n", errmsg);
      bfd_error = TRUE;
   }
   dlog(DEBUGDUMP,"files.c: dump_stack: bfd_set_default_target pei-i386 OK\n");
#endif
#if !defined(linux) && !defined(WINDOWS) && !defined(dos)
 #warning "Please make sure your target is set in files.c, dump_stack!"
#endif

   abfd = bfd_openr (argv0, NULL);

dlog(DEBUGDUMP,"files.c: dump_stack: step 0, i %d argv0 %s\n", i, argv0);
   if (abfd == NULL)
   {
      cptr errmsg = bfd_errmsg( bfd_get_error() );
      dlog(DEBUGDUMP,"files.c: dump_stack: abfd == NULL; bfd error = %s\n", errmsg);
      bfd_error = TRUE;
   }
dlog(DEBUGDUMP,"files.c: dump_stack: step 1, error %d\n", bfd_error);
 
   if ( (bfd_error == FALSE) && (bfd_check_format (abfd, bfd_archive)) )
   {
      cptr errmsg = bfd_errmsg( bfd_get_error() );
      dlog(DEBUGDUMP,"files.c: dump_stack: bfd_check_format return-value != 0; bfd error = %s\n", errmsg);
      bfd_error = TRUE;
   }
dlog(DEBUGDUMP,"files.c: dump_stack: step 2, error %d\n", bfd_error);
   if ( (bfd_error == FALSE) && (! bfd_check_format_matches (abfd, bfd_object, &matching)) )
   {
      cptr errmsg = bfd_errmsg( bfd_get_error() );
      dlog(DEBUGDUMP,"files.c: dump_stack: format doesn't match; bfd error = %s\n", errmsg);
      bfd_error = TRUE;
   }
dlog(DEBUGDUMP,"files.c: dump_stack: step 3, error %d\n", bfd_error);
 
   if ( (bfd_error == FALSE) && ((bfd_get_file_flags (abfd) & HAS_SYMS) == 0) )
   {
      cptr errmsg = bfd_errmsg( bfd_get_error() );
      dlog(DEBUGDUMP,"files.c: dump_stack: no symbols found; bfd error = %s\n", errmsg);
      bfd_error = TRUE;
   }
dlog(DEBUGDUMP,"files.c: dump_stack: step 4, error %d\n", bfd_error);

   if (bfd_error == FALSE) storage = bfd_get_symtab_upper_bound (abfd);

   if ( (bfd_error == FALSE) && (storage < 0) )
   {
      cptr errmsg = bfd_errmsg( bfd_get_error() );
      dlog(DEBUGDUMP,"files.c: dump_stack: storage < 0; bfd error = %s\n", errmsg);
      bfd_error = TRUE;
   }
dlog(DEBUGDUMP,"files.c: dump_stack: step 5, error %d\n", bfd_error);
 
   if (bfd_error == FALSE) dump_syms = (asymbol **) xmalloc (storage);
 
   if (bfd_error == FALSE) symcount = bfd_canonicalize_symtab (abfd, dump_syms);

   if ( (bfd_error == FALSE) && (symcount < 0) )
   {
      cptr errmsg = bfd_errmsg( bfd_get_error() );
      dlog(DEBUGDUMP,"files.c: dump_stack: symcount < 0; bfd error = %s\n", errmsg);
      bfd_error = TRUE;
   }
dlog(DEBUGDUMP,"files.c: dump_stack: step 6, error %d\n", bfd_error);

   for (; i>=0 ; i--)
   {
      {
         char function_name[2048];
         char source_name[2048];
         int source_line;
        
         if (bfd_error == FALSE)
         { 
            translate_address (abfd, stack_addr[i], function_name, source_name, &source_line);
            dlog(DEBUGALWAYS,"files.c: dump_stack: stack frame %2d address %08lx = %s (%s %d) \n",
                             i, stack_addr[i], function_name, source_name, source_line);
            if (fff != NULL) 
            {
               fprintf(fff, "stack frame %2d address %08lx = %s (%s %d) \n",
                            i, stack_addr[i], function_name, source_name, source_line);
            }
         }
         else
         { 
            dlog(DEBUGALWAYS,"files.c: dump_stack: stack frame %2d address %08lx\n",
                             i, stack_addr[i]);
            if (fff != NULL) 
            {
               fprintf(fff, "stack frame %2d address %08lx\n",
                            i, stack_addr[i]);
            }
         }
      }
   }

/* and cleaning up afterwards */
   if (dump_syms != NULL)
   {
      free (dump_syms);
      dump_syms = NULL;
   }

   if (bfd_error == FALSE) bfd_close (abfd);
}
#else
void dump_stack(FILE *fff)
{
   s16b i;
   static unsigned long stack_addr[MAX_STACK_ADDR];

   /* clean the stack addresses if necessary */
   for (i=0; i < MAX_STACK_ADDR; i++)
   {
      stack_addr[i] = (unsigned long)0;
   }
   get_1_address(0);  get_1_address(1);  get_1_address(2);  get_1_address(3);  get_1_address(4);
   get_1_address(5);  get_1_address(6);  get_1_address(7);  get_1_address(8);  get_1_address(9);
   get_1_address(10); get_1_address(11); get_1_address(12); get_1_address(13); get_1_address(14);
   get_1_address(15); get_1_address(16); get_1_address(17); get_1_address(18); get_1_address(19);
   get_1_address(20); get_1_address(21); get_1_address(22); get_1_address(23); get_1_address(24);
   get_1_address(25); get_1_address(26); get_1_address(27); get_1_address(28); get_1_address(29);
   get_1_address(30); get_1_address(31); get_1_address(32); get_1_address(33); get_1_address(34);
   get_1_address(35); get_1_address(36); get_1_address(37); get_1_address(38); get_1_address(39);
   get_1_address(40); get_1_address(41); get_1_address(42); get_1_address(43); get_1_address(44);
   get_1_address(45); get_1_address(46); get_1_address(47); get_1_address(48); get_1_address(49);
   get_1_address(50); get_1_address(51); get_1_address(52); get_1_address(53); get_1_address(54);
   get_1_address(55); get_1_address(56); get_1_address(57); get_1_address(58); get_1_address(59);
   get_1_address(60); get_1_address(61); get_1_address(62); get_1_address(63); get_1_address(64);
   get_1_address(65); get_1_address(66); get_1_address(67); get_1_address(68); get_1_address(69);
   get_1_address(70); get_1_address(71); get_1_address(72); get_1_address(73); get_1_address(74);
   get_1_address(75); get_1_address(76); get_1_address(77); get_1_address(78); get_1_address(79);
   get_1_address(80); get_1_address(81); get_1_address(82); get_1_address(83); get_1_address(84);
   get_1_address(85); get_1_address(86); get_1_address(87); get_1_address(88); get_1_address(89);

   /* dump stack frame */
   i = MAX_STACK_ADDR-1;
   while ( (i >=0) && (stack_addr[i] == 0)) i--;

   if (i < 0)
   {
      dlog(DEBUGALWAYS,"files.c: dump_stack: unable to decode the stack.\n");
      return;
   }

   for (; i>=0 ; i--)
   {
      {
         dlog(DEBUGALWAYS,"files.c: dump_stack: stack frame %2d address %08lx\n", i, stack_addr[i]);
      }
   }
}
#endif /* DEBUGDUMP */

/*
 * Ignore SIGTSTP signals (keyboard suspend)
 */
void signals_ignore_tstp(void)
{

#ifdef SIGTSTP
    (void)signal(SIGTSTP, SIG_IGN);
#endif

}

/*
 * Handle SIGTSTP signals (keyboard suspend)
 */
void signals_handle_tstp(void)
{

#ifdef SIGTSTP
    (void)signal(SIGTSTP, handle_signal_suspend);
#endif

}


/*
 * Prepare to handle the relevant signals
 */
void signals_init()
{

#ifdef SIGHUP
    (void)signal(SIGHUP, SIG_IGN);
#endif

#ifdef SIGTSTP
    (void)signal(SIGTSTP, handle_signal_suspend);
#endif

#ifdef SIGINT
    (void)signal(SIGINT, handle_signal_simple);
#endif

#ifdef SIGQUIT
    (void)signal(SIGQUIT, handle_signal_simple);
#endif

#ifdef SIGFPE
    (void)signal(SIGFPE, handle_signal_abort);
#endif

#ifdef SIGILL
    (void)signal(SIGILL, handle_signal_abort);
#endif

#ifdef SIG
TRAP
    (void)signal(SIGTRAP, handle_signal_abort);
#endif

#ifdef SIGABRT
    (void)signal(SIGABRT, handle_signal_abort);
#endif

#ifdef SIGIOT
    (void)signal(SIGIOT, handle_signal_abort);
#endif

#ifdef SIGKILL
    (void)signal(SIGKILL, handle_signal_abort);
#endif

#ifdef SIGBUS
    (void)signal(SIGBUS, handle_signal_abort);
#endif

#ifdef SIGSEGV
    (void)signal(SIGSEGV, handle_signal_abort);
#endif

#ifdef SIGTERM
    (void)signal(SIGTERM, handle_signal_abort);
#endif

#ifdef SIGPIPE
    (void)signal(SIGPIPE, handle_signal_abort);
#endif

#ifdef SIGEMT
    (void)signal(SIGEMT, handle_signal_abort);
#endif

#ifdef SIGDANGER
    (void)signal(SIGDANGER, handle_signal_abort);
#endif

#ifdef SIGSYS
    (void)signal(SIGSYS, handle_signal_abort);
#endif

#ifdef SIGXCPU
    (void)signal(SIGXCPU, handle_signal_abort);
#endif

#ifdef SIGPWR
    (void)signal(SIGPWR, handle_signal_abort);
#endif

}

#else   /* HANDLE_SIGNALS */

/*
 * Do nothing
 */
void signals_ignore_tstp(void)
{
}

/*
 * Do nothing
 */
void signals_handle_tstp(void)
{
}

/*
 * Do nothing
 */
void signals_init(void)
{
}

#endif  /* HANDLE_SIGNALS */
