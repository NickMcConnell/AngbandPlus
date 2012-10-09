#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* definitions for maze size */
#include "maze.h"

char hedges[SPACE];
char vedges[SPACE];

/* return 1 if the locations were the same.  Make them the same. */
/*
 * Each location in the maze points to some other location connected to it.
 * Because how this is constructed, these interrelations form a tree, and
 * you can check to see if two nodes are connected by going up to the roots,
 * and seeing if the roots are the same.
 *
 * This function adds one tree to the other if they're not connected.
 * As a side effect, it flattens the tree a bit for efficiency.
 */
int do_set(int locations[SPACE], int loc1, int loc2)
{
   int temp;

   /* chase loc1 down to the root */
   while (locations[loc1] != loc1)
   {
      temp = loc1;
      loc1 = locations[loc1];
      locations[temp] = locations[loc1];  /* flatten */
   }
   /* chase loc2 down to the root */
   while (locations[loc2] != loc2)
   {
      temp = loc2;
      loc2 = locations[loc2];
      locations[temp] = locations[loc2];  /* flatten */
   }
   /* are they connected somehow? */
   if (loc1 == loc2)
   {
      /* Yup, they were connected */
      return(1);
   } else {
      /* connect them */
      locations[loc2] = loc1;
      /* and say that they weren't connected before */
      return(0);
   }
}

/* Scramble for a new game */
void randomize(void)
{
   int x, y;
   int squares[SPACE];        /* the maze data */
   int hedgelist[SPACE];         /* h edges left to check */
   int vedgelist[SPACE];         /* v edges left to check */
   int hedgecount;            /* h edges count */
   int vedgecount;            /* v edges count */
   int index, curedge;

   /* set up the variables */
   hedgecount = 0;   /* haven't checked any horizontal edges yet */
   vedgecount = 0;   /* haven't checked any vertical edges yet */
   /* Initialize arrays of all horizontal edges left to check */
   for (x=0; x<XCOUNT; x++)
      for (y=1; y<YCOUNT; y++)
         hedgelist[hedgecount++] = x*YCOUNT+y;
   /* Initialize arrays of all vertical edges left to check */
   for (x=1; x<XCOUNT; x++)
      for (y=0; y<YCOUNT; y++)
         vedgelist[vedgecount++] = x*YCOUNT+y;

   /* label squares by what they're connected to: just self */
   for (x=0; x<XCOUNT; x++)
      for (y=0; y<YCOUNT; y++)
      {
         squares[x*YCOUNT+y] = x*YCOUNT+y;
      }

   /* and the interesting loop -- punch holes as necessary */
   while ((hedgecount > 0) || (vedgecount > 0)) /* all edges checked? */
   {
      /* do a horizontal edge if ... */
      if ((vedgecount == 0) ||   /* that's all that's left */
          ((lrand48() % 2) && /* or 50/50 chance and */
           (hedgecount > 0)))    /* there are some left */
      {
         /* horizontal edge */
         /* pick one at random from the unchecked */
         index = lrand48() % hedgecount;
         curedge = hedgelist[index];
         /* and remove it from the "to check" list */
         hedgelist[index] = hedgelist[--hedgecount];
         /* if the stuff on either side of it is already
          * connected, leave it alone.  If they're not,
          * then do_set connectes them, and we punch a hole
          * in that wall */
         hedges[curedge] =
            do_set(squares,curedge,curedge-1);
      } else {
         /* vertical edge */
         /* pick one at random from the unchecked */
         index = lrand48() % vedgecount;
         curedge = vedgelist[index];
         /* and remove it from the "to check" list */
         vedgelist[index] = vedgelist[--vedgecount];
         /* if the stuff on either side of it is already
          * connected, leave it alone.  If they're not,
          * then do_set connectes them, and we punch a hole
          * in that wall */
         vedges[curedge] =
            do_set(squares,curedge,curedge-YCOUNT);
      }
   }
   /* Finish up the horizontal edges of the maze */
   for (x=0; x<XCOUNT; x++)
   {
      hedges[x*YCOUNT] = 1;
   }
   /* and the vertical edges too */
   for (y=0; y<YCOUNT; y++)
   {
      vedges[y] = 1;
   }
   /* and make one entrance/exit */
   x = lrand48() % XCOUNT;
      hedges[x*YCOUNT] = 0;
}

/* and a function to display it using ascii graphics */
void drawframe(void)
{
   int x, y;

   /* first the arrow pointing in */
   for (x=0; x<XCOUNT; x++)
      if (hedges[x*YCOUNT])
      {
         putchar(' ');
         putchar(' ');
      }
      else
      {
         putchar(' ');
         putchar('|');
      }
   putchar('\n');
   for (x=0; x<XCOUNT; x++)
      if (hedges[x*YCOUNT])
      {
         putchar(' ');
         putchar(' ');
      }
      else
      {
         putchar(' ');
         putchar('v');
      }
   putchar('\n');

   /* and a main loop to draw the maze */
   for (y=0; y<YCOUNT; y++)
   {
      for (x=0; x<XCOUNT; x++)
      {
         putchar('+');
         if (hedges[x*YCOUNT+y])
            putchar('-');
         else
            putchar(' ');
      }
      putchar('+');
      putchar('\n');
      for (x=0; x<XCOUNT; x++)
      {
         if (vedges[x*YCOUNT+y])
            putchar('|');
         else
            putchar(' ');
         putchar(' ');
      }
      putchar('|');
      putchar('\n');
   }
   /* the bottom of the maze */
   for (x=0; x<XCOUNT; x++)
   {
      putchar('+');
      if (hedges[x*YCOUNT])
         putchar('-');
      else
         putchar(' ');
   }
   putchar('+');
   putchar('\n');
   /* and an arrow pointing out */
   for (x=0; x<XCOUNT; x++)
      if (hedges[x*YCOUNT])
      {
         putchar(' ');
         putchar(' ');
      }
      else
      {
         putchar(' ');
         putchar('|');
      }
   putchar('\n');
   for (x=0; x<XCOUNT; x++)
      if (hedges[x*YCOUNT])
      {
         putchar(' ');
         putchar(' ');
      }
      else
      {
         putchar(' ');
         putchar('v');
      }
   putchar('\n');
}

/* The main function just calls the pieces */
int main(int argc, char **argv)
{
   /* make each maze different */
   srand48((long)time((time_t *)NULL));
   /* decide on a maze */
   randomize();
   /* and draw it. */
   drawframe();
   /* done! */
   return 0;
}
