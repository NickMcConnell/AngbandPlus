diff -Nau --exclude=make* --exclude=~* --exclude=*.txt --exclude=*.o --exclude=*.bin --exclude=*~ --exclude=*.zip osrc/cmd2.c src/cmd2.c
--- osrc/cmd2.c	Wed Apr 12 17:37:00 2000
+++ src/cmd2.c	Wed Apr 12 17:54:24 2000
@@ -228,18 +228,19 @@
 		if (tiny && (rand_int(100) < 75))
 		{
 			/* Make some gold */
-			if (!make_gold(i_ptr)) continue;
+			if (make_gold(i_ptr)) 
+			  drop_near(i_ptr, -1, y, x);
 		}
 
 		/* Otherwise drop an item */
 		else
 		{
 			/* Make an object */
-			if (!make_object(i_ptr, FALSE, FALSE)) continue;
+			if (make_object(i_ptr, FALSE, FALSE))
+			  drop_near(i_ptr, -1, y, x);
+		
 		}
 
-		/* Drop it in the dungeon */
-		drop_near(i_ptr, -1, y, x);
 	}
 
 	/* Reset the object level */
diff -Nau --exclude=make* --exclude=~* --exclude=*.txt --exclude=*.o --exclude=*.bin --exclude=*~ --exclude=*.zip osrc/cmd4.c src/cmd4.c
--- osrc/cmd4.c	Wed Apr 12 17:37:00 2000
+++ src/cmd4.c	Wed Apr 12 18:01:32 2000
@@ -854,6 +854,8 @@
 
 		/* Window flags */
 		prt("(W) Window flags", 11, 5);
+		/* Squelch menus */
+		prt("(I) Item Squelch Menus", 12, 5);
 
 		/* Load and Append */
 		prt("(L) Load a user pref file", 13, 5);
@@ -913,7 +915,11 @@
 		{
 			do_cmd_options_win();
 		}
-
+		/* Squelching menus */
+		else if ((ch == 'I') || (ch == 'i'))
+		{
+		        do_cmd_squelch();
+		}
 		/* Load a user pref file */
 		else if ((ch == 'L') || (ch == 'l'))
 		{
diff -Nau --exclude=make* --exclude=~* --exclude=*.txt --exclude=*.o --exclude=*.bin --exclude=*~ --exclude=*.zip osrc/dungeon.c src/dungeon.c
--- osrc/dungeon.c	Wed Apr 12 17:37:02 2000
+++ src/dungeon.c	Wed Apr 12 18:25:38 2000
@@ -186,6 +186,7 @@
 	for (i = 0; i < INVEN_TOTAL; i++)
 	{
 		bool okay = FALSE;
+		int squelch=0;
 
 		o_ptr = &inventory[i];
 
@@ -239,6 +240,11 @@
 		/* Skip non-feelings */
 		if (!feel) continue;
 
+		/* Squelch it? */
+		if (i<INVEN_WIELD) {
+		  squelch = squelch_itemp(o_ptr, feel, 0);
+		}
+		
 		/* Stop everything */
 		if (disturb_minor) disturb(0, 0);
 
@@ -257,10 +263,12 @@
 		/* Message (inventory) */
 		else
 		{
-			msg_format("You feel the %s (%c) in your pack %s %s...",
+			msg_format("You feel the %s (%c) in your pack %s %s...  %s",
 			           o_name, index_to_label(i),
 			           ((o_ptr->number == 1) ? "is" : "are"),
-			           inscrip_text[feel - INSCRIP_NULL]);
+			           inscrip_text[feel - INSCRIP_NULL],
+				   ((squelch==1) ? "(Squelched)" :
+				    ((squelch==-1) ? "(Squelch Failed)" : "")));
 		}
 
 		/* Sense the object */
@@ -269,6 +277,8 @@
 		/* The object has been "sensed" */
 		o_ptr->ident |= (IDENT_SENSE);
 
+		/* Squelch it if necessary */
+		do_squelch_item(squelch, i, o_ptr);
 
 		/* Combine / Reorder the pack (later) */
 		p_ptr->notice |= (PN_COMBINE | PN_REORDER);
diff -Nau --exclude=make* --exclude=~* --exclude=*.txt --exclude=*.o --exclude=*.bin --exclude=*~ --exclude=*.zip osrc/externs.h src/externs.h
--- osrc/externs.h	Wed Apr 12 17:37:16 2000
+++ src/externs.h	Wed Apr 12 17:41:28 2000
@@ -729,6 +729,11 @@
 extern bool get_rep_dir(int *dp);
 extern bool confuse_dir(int *dp);
 
+/* squelch.c */
+extern byte squelch_level[24];
+extern void do_cmd_squelch(void);
+extern int squelch_itemp(object_type *, byte, int);
+extern int do_squelch_item(int, int, object_type *);
 
 /*
  * Hack -- conditional (or "bizarre") externs
diff -Nau --exclude=make* --exclude=~* --exclude=*.txt --exclude=*.o --exclude=*.bin --exclude=*~ --exclude=*.zip osrc/load2.c src/load2.c
--- osrc/load2.c	Wed Apr 12 17:37:02 2000
+++ src/load2.c	Wed Apr 12 17:43:30 2000
@@ -1357,7 +1357,8 @@
 	if (older_than(2, 8, 5)) adult_rand_artifacts = tmp8u;
 
 	/* Future use */
-	for (i = 0; i < 40; i++) rd_byte(&tmp8u);
+	for (i = 0; i < 24; i++) rd_byte(&squelch_level[i]);
+	for (i = 0; i < 16; i++) rd_byte(&tmp8u);
 
 	/* Read the randart version */
 	rd_u32b(&randart_version);
@@ -2641,6 +2642,7 @@
 
 		k_ptr->aware = (tmp8u & 0x01) ? TRUE: FALSE;
 		k_ptr->tried = (tmp8u & 0x02) ? TRUE: FALSE;
+		k_ptr->squelch = (tmp8u & 0x04) ? TRUE: FALSE;
 	}
 	if (arg_fiddle) note("Loaded Object Memory");
 
diff -Nau --exclude=make* --exclude=~* --exclude=*.txt --exclude=*.o --exclude=*.bin --exclude=*~ --exclude=*.zip osrc/object2.c src/object2.c
--- osrc/object2.c	Wed Apr 12 17:37:02 2000
+++ src/object2.c	Wed Apr 12 17:55:22 2000
@@ -3412,7 +3412,7 @@
 bool make_object(object_type *j_ptr, bool good, bool great)
 {
 	int prob, base;
-
+	bool sq_flag = FALSE;
 
 	/* Chance of "special object" */
 	prob = (good ? 10 : 1000);
@@ -3451,6 +3451,9 @@
 
 		/* Handle failure */
 		if (!k_idx) return (FALSE);
+		
+		/* Squelch it? */
+		sq_flag = (k_info[k_idx].squelch & k_info[k_idx].aware);
 
 		/* Prepare the object */
 		object_prep(j_ptr, k_idx);
@@ -3482,8 +3485,8 @@
 		if (cheat_peek) object_mention(j_ptr);
 	}
 
-	/* Success */
-	return (TRUE);
+	/* Do not squelch artifacts */
+	return((sq_flag && !j_ptr->name1)? FALSE : TRUE);
 }
 
 
@@ -3853,10 +3856,8 @@
 		object_wipe(i_ptr);
 
 		/* Make a good (or great) object (if possible) */
-		if (!make_object(i_ptr, TRUE, great)) continue;
-
-		/* Drop the object */
-		drop_near(i_ptr, -1, y1, x1);
+		if (make_object(i_ptr, TRUE, great)) 
+		  drop_near(i_ptr, -1, y1, x1);
 	}
 }
 
diff -Nau --exclude=make* --exclude=~* --exclude=*.txt --exclude=*.o --exclude=*.bin --exclude=*~ --exclude=*.zip osrc/save.c src/save.c
--- osrc/save.c	Wed Apr 12 17:37:02 2000
+++ src/save.c	Wed Apr 12 17:47:00 2000
@@ -827,6 +827,7 @@
 
 	if (k_ptr->aware) tmp8u |= 0x01;
 	if (k_ptr->tried) tmp8u |= 0x02;
+	if (k_ptr->squelch) tmp8u |= 0x04;
 
 	wr_byte(tmp8u);
 }
@@ -1102,8 +1103,8 @@
 	wr_byte(0);
 
 	/* Future use */
-	for (i = 0; i < 10; i++) wr_u32b(0L);
-
+	for (i = 0; i < 24; i++) wr_byte(squelch_level[i]);
+	for (i = 0; i < 16; i++) wr_byte(0);
 
 	/* Random artifact version */
 	wr_u32b(RANDART_VERSION);
diff -Nau --exclude=make* --exclude=~* --exclude=*.txt --exclude=*.o --exclude=*.bin --exclude=*~ --exclude=*.zip osrc/spells2.c src/spells2.c
--- osrc/spells2.c	Wed Apr 12 17:37:02 2000
+++ src/spells2.c	Wed Apr 12 18:24:32 2000
@@ -1835,6 +1835,7 @@
 bool ident_spell(void)
 {
 	int item;
+	int squelch=0;
 
 	object_type *o_ptr;
 
@@ -1868,6 +1869,10 @@
 	/* Recalculate bonuses */
 	p_ptr->update |= (PU_BONUS);
 
+	/* Squelch it? */
+	if (item<INVEN_WIELD) 
+	  squelch=squelch_itemp(o_ptr, 0, 1);
+
 	/* Combine / Reorder the pack (later) */
 	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
 
@@ -1885,15 +1890,23 @@
 	}
 	else if (item >= 0)
 	{
-		msg_format("In your pack: %s (%c).",
-		           o_name, index_to_label(item));
+		msg_format("In your pack: %s (%c).  %s",
+		           o_name, index_to_label(item),
+			   ((squelch==1) ? "(Squelched)" :
+			    ((squelch==-1) ? "(Squelch Failed)" : "")));
 	}
 	else
 	{
-		msg_format("On the ground: %s.",
-		           o_name);
+		msg_format("On the ground: %s.  %s",
+		           o_name,
+			   ((squelch==1) ? "(Squelched)" :
+			    ((squelch==-1) ? "(Squelch Failed)" : "")));
+		
 	}
 
+	/* Now squelch it if needed */
+	do_squelch_item(squelch, item, o_ptr);
+
 	/* Something happened */
 	return (TRUE);
 }
@@ -1908,6 +1921,7 @@
 bool identify_fully(void)
 {
 	int item;
+	int squelch=0;
 
 	object_type *o_ptr;
 
@@ -1938,6 +1952,10 @@
 	object_aware(o_ptr);
 	object_known(o_ptr);
 
+	/* Squelch it? */
+	if (item<INVEN_WIELD) 
+	  squelch=squelch_itemp(o_ptr, 0, 1);
+
 	/* Mark the item as fully known */
 	o_ptr->ident |= (IDENT_MENTAL);
 
@@ -1964,17 +1982,28 @@
 	}
 	else if (item >= 0)
 	{
-		msg_format("In your pack: %s (%c).",
-		           o_name, index_to_label(item));
+		msg_format("In your pack: %s (%c).  %s",
+		           o_name, index_to_label(item),
+			   ((squelch==1) ? "(Squelched)" :
+			    ((squelch==-1) ? "(Squelch Failed)" : "")));
 	}
 	else
 	{
-		msg_format("On the ground: %s.",
-		           o_name);
+		msg_format("On the ground: %s.  %s",
+		           o_name,
+			   ((squelch==1) ? "(Squelched)" :
+			    ((squelch==-1) ? "(Squelch Failed)" : "")));
+		
 	}
 
-	/* Describe it fully */
-	identify_fully_aux(o_ptr);
+	/* Now squelch it if needed */
+	if (squelch==1) {
+	  do_squelch_item(squelch, item, o_ptr);
+	} else {
+	  
+	  /* Describe it fully */
+	  identify_fully_aux(o_ptr);
+	}
 
 	/* Success */
 	return (TRUE);
diff -Nau --exclude=make* --exclude=~* --exclude=*.txt --exclude=*.o --exclude=*.bin --exclude=*~ --exclude=*.zip osrc/squelch.c src/squelch.c
--- osrc/squelch.c	Thu Jan  1 00:00:00 1970
+++ src/squelch.c	Wed Apr 12 18:07:56 2000
@@ -0,0 +1,684 @@
+
+#include "angband.h"
+
+static int do_qual_squelch(void);
+
+
+/* 
+ * This stores the various squelch levels for the secondary squelching.
+ * It is currently hardcoded at 24 bytes, but since there are only 16
+ * applicable tvals there shouldn't be a problem.  
+ */
+
+byte squelch_level[];
+
+/*
+ * These are the various levels of squelching supported by the game.  
+ * Less concisely:
+ * 0 ---> No squelching
+ * 1 ---> Squelch cursed items
+ * 2 ---> Squelch average and worse items
+ * 3 ---> Squelch good and worse items
+ * 4 ---> squelch all but artifacts
+ */
+
+#define SQUELCH_NONE     0
+#define SQUELCH_CURSED   1
+#define SQUELCH_AVERAGE  2
+#define SQUELCH_GOOD     3
+#define SQUELCH_ALL      4
+
+
+
+/*
+ * These are the base types for automatic squelching on creation.
+ * I've combined some of the tvals to make this list a little more
+ * reasonable.
+ */
+
+#define TYPE_AMMO    1
+#define TYPE_BOW     2
+#define TYPE_WEAPON1 3
+#define TYPE_WEAPON2 4
+#define TYPE_BODY    5
+#define TYPE_CLOAK   6
+#define TYPE_SHIELD  7
+#define TYPE_HELM    8
+#define TYPE_GLOVES  9
+#define TYPE_BOOTS   10
+#define TYPE_RING    11
+#define TYPE_STAFF   12
+#define TYPE_WAND    13
+#define TYPE_ROD     14
+#define TYPE_SCROLL  15
+#define TYPE_POTION  16
+#define TYPE_AMULET  17
+#define TYPE_BOOK    18
+#define TYPE_FOOD    19
+#define TYPE_MISC    20
+
+/*
+ * This (admittedly hacky) stores the mapping from tval to typeval
+ * and is reinitialized every time do_cmd_squelch is called.  This 
+ * can certainly be done more cleanly.
+ */
+static int tv_to_type[100];
+
+/*
+ * These structures are lifted from wizard2.c where they were used in 
+ * the 'create item' command.  I have adapted them for my code.
+ */
+
+typedef struct tval_desc
+{
+	int        tval;
+	cptr       desc;
+} tval_desc;
+
+static char head[4] =
+{ 'a', 'A', '0', ':' };
+
+/*
+ * Here are the categories for squelch-on-creation.
+ */
+static tval_desc typevals[] = 
+{
+  {TYPE_AMMO, "Missiles"},    
+  {TYPE_BOW, "Missile Launchers"},
+  {TYPE_WEAPON1, "Weapons (Swords)"},  
+  {TYPE_WEAPON2, "Weapons (Non Swords)"},
+  {TYPE_BODY, "Body Armor"},   
+  {TYPE_CLOAK, "Cloaks"},   
+  {TYPE_SHIELD, "Shields"},   
+  {TYPE_HELM, "Helmets"},   
+  {TYPE_GLOVES, "Gloves"},   
+  {TYPE_BOOTS, "Boots"},   
+  {TYPE_AMULET, "Amulets"},  
+  {TYPE_RING, "Rings"},    
+  {TYPE_STAFF, "Staves"},      
+  {TYPE_WAND, "Wands"},    
+  {TYPE_ROD, "Rods"},     
+  {TYPE_SCROLL, "Scrolls"},  
+  {TYPE_POTION, "Potions"},  
+  {TYPE_BOOK, "Magic Books"},
+  {TYPE_FOOD, "Food Items"},
+  {TYPE_MISC, "Miscellaneous"},    
+  {0, NULL}
+  
+};    
+
+/*
+ * Here are the categories for squelch-on-identification.
+ * This array is lifted (and edited_ from wizard2.c, hence 
+ * the spacy formatting.
+ */
+
+static tval_desc tvals[] =
+{
+	{ TV_SWORD,             "Sword"                },
+	{ TV_POLEARM,           "Polearm"              },
+	{ TV_HAFTED,            "Hafted Weapon"        },
+	{ TV_BOW,               "Bow"                  },
+	{ TV_ARROW,             "Arrows"               },
+	{ TV_BOLT,              "Bolts"                },
+	{ TV_SHOT,              "Shots"                },
+	{ TV_SHIELD,            "Shield"               },
+	{ TV_CROWN,             "Crown"                },
+	{ TV_HELM,              "Helm"                 },
+	{ TV_GLOVES,            "Gloves"               },
+	{ TV_BOOTS,             "Boots"                },
+	{ TV_CLOAK,             "Cloak"                },
+	{ TV_DRAG_ARMOR,        "Dragon Scale Mail"    },
+	{ TV_HARD_ARMOR,        "Hard Armor"           },
+	{ TV_SOFT_ARMOR,        "Soft Armor"           },
+	{ 0,                    NULL }
+};
+
+/*
+ * This code is the heavy pseudoidentify code.  I lifted it 
+ * from dungeon.c.  It is used in the quality squelching code.
+ */
+
+/*
+ * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
+ */
+static int value_check_aux1(object_type *o_ptr)
+{
+	/* Artifacts */
+	if (artifact_p(o_ptr))
+	{
+		/* Cursed/Broken */
+		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_TERRIBLE);
+
+		/* Normal */
+		return (INSCRIP_SPECIAL);
+	}
+
+	/* Ego-Items */
+	if (ego_item_p(o_ptr))
+	{
+		/* Cursed/Broken */
+		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_WORTHLESS);
+
+		/* Normal */
+		return (INSCRIP_EXCELLENT);
+	}
+
+	/* Cursed items */
+	if (cursed_p(o_ptr)) return (INSCRIP_CURSED);
+
+	/* Broken items */
+	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);
+
+	/* Good "armor" bonus */
+	if (o_ptr->to_a > 0) return (INSCRIP_GOOD);
+
+	/* Good "weapon" bonus */
+	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD);
+
+	/* Default to "average" */
+	return (INSCRIP_AVERAGE);
+}
+
+/*
+ * Strip an "object name" into a buffer.  Lifted from wizard2.c.
+ */
+static void strip_name(char *buf, int k_idx)
+{
+	char *t;
+
+	object_kind *k_ptr = &k_info[k_idx];
+
+	cptr str = (k_name + k_ptr->name);
+
+
+	/* Skip past leading characters */
+	while ((*str == ' ') || (*str == '&')) str++;
+
+	/* Copy useful chars */
+	for (t = buf; *str; str++)
+	{
+		if (*str != '~') *t++ = *str;
+	}
+
+	/* Terminate the new name */
+	*t = '\0';
+}
+
+
+/*
+ * This subroutine actually handles the squelching menus.
+ */
+
+static int do_cmd_squelch_aux(void)
+{
+	int i, j, temp, num, max_num;
+	int col, row;
+	int typeval;
+	cptr tval_desc;
+	char ch, sq;
+
+	int choice[60];
+
+	char buf[160];
+	byte color;
+
+	/* Clear screen */
+	Term_clear();
+
+	/* 
+	 * Print all typeval's and their descriptions 
+	 *
+	 * This uses the above arrays.  I combined a few of the 
+	 * tvals into single typevals.
+	 */
+
+	for (num = 0; (num<60) && typevals[num].tval; num++)
+	{
+		row = 3 + (num % 20);
+		col = 30 * (num / 20);
+		ch = head[num/26] +num%26;
+		prt(format("[%c] %s", ch, typevals[num].desc), row, col);
+		
+	}
+
+	/* Me need to know the maximal possible tval_index */
+	max_num = num;
+
+	/* Choose! */
+	if (!get_com("Squelch what type of object? (Q: Go to secondary menu) ", &ch)) return (0);
+
+	if (ch=='Q') 
+	{
+	  /* Switch to secondary squelching menu */
+	  do_qual_squelch();
+	} else {
+
+	  /* Analyze choice */
+	  num = ch-'a';
+
+	  /* Bail out if choice is illegal */
+	  if ((num < 0) || (num >= max_num)) return (0);
+
+	  /* Base object type chosen, fill in tval */
+	  typeval = typevals[num].tval;
+	  tval_desc = typevals[num].desc;
+
+
+	  /*** And now we go for k_idx ***/
+	  
+	  /* Clear screen */
+	  
+	  while (1) {
+	    Term_clear();
+
+	    /* First sort based on value */
+	    /* Step 1: Read into choice array */
+
+	    for (num = 0, i = 1; (num < 60) && (i < MAX_K_IDX); i++)
+	      {
+		object_kind *k_ptr = &k_info[i];
+		
+		if (tv_to_type[k_ptr->tval] == typeval) {
+		  if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;
+		  choice[num++] = i;
+		}
+	      }
+
+	    max_num = num;
+
+	    /* Step 2: Simple bubble sort */
+	    for (i=0; i<max_num; i++) 
+	      for (j=i; j<max_num; j++) {
+		if ((k_info[choice[i]].tval>k_info[choice[j]].tval) ||
+		    ((k_info[choice[i]].tval==k_info[choice[j]].tval) &&
+		     (k_info[choice[i]].cost>k_info[choice[j]].cost))) {
+		  temp = choice[i];
+		  choice[i] = choice[j];
+		  choice[j] = temp;
+		}
+	      }
+	    
+	    for (num = 0; num<max_num; num++)
+	    {
+	      object_kind *k_ptr = &k_info[choice[num]];
+
+	      /* Prepare it */
+	      row = 3 + (num % 20);
+	      col = 30 * (num / 20);
+	      ch = head[num/26] + (num%26);
+
+	      /* Acquire the "name" of object "i" */
+	      strip_name(buf, choice[num]);
+	      
+	      /* Get the squelch character */
+	      sq = (k_ptr->squelch ? (k_ptr->aware ? '*' : '.') : ' ');
+	      
+	      /* Get the color */
+	      color = (k_ptr->squelch ? 
+		       (k_ptr->aware ? TERM_RED : TERM_L_UMBER) :
+		       (k_ptr->aware ? TERM_L_GREEN : TERM_GREEN));
+
+	      /* Print it */
+	      prt(format("[%c%c] ", ch, sq), row, col);
+	      c_put_str(color, buf, row, col+5);
+	    }
+
+	    /* Print the legend */
+	    prt("'*': Squelch (ided)     '.': Squelch (not ided)     ' ': Allow generation", 1, 0);
+
+	    /* Choose! */
+	    if (!get_com(format("%s : Command? (^A: Squelch all   ^U: Unsquelch all)", tval_desc), &ch)) return (1);
+
+	    if (ch==KTRL('A')) {
+	      /* ^A --> Squelch all items */
+	      for (i=0; i<max_num; i++) {
+		k_info[choice[i]].squelch = TRUE;
+	      }
+	    } else if (ch==KTRL('U')) {
+	      /* ^U --> Unsquelch all items */
+	      for (i=0; i<max_num; i++) {
+		k_info[choice[i]].squelch = FALSE;
+	      }
+	    } else {
+	      /* Analyze choice */
+	      num = -1;
+	      if ((ch >= head[0]) && (ch < head[0] + 26)) num = ch - head[0];
+	      if ((ch >= head[1]) && (ch < head[1] + 26)) num = ch - head[1] + 26;
+	      if ((ch >= head[2]) && (ch < head[2] + 17)) num = ch - head[2] + 52;
+	      
+	      /* Bail out if choice is "illegal" */
+	      if ((num < 0) || (num >= max_num)) return (1);
+	      
+	      /* Toggle */
+	      k_info[choice[num]].squelch =
+		(k_info[choice[num]].squelch ? FALSE : TRUE);
+	    }
+	  }
+	}
+	/* And return successful */
+	return (1);
+}
+
+/*
+ * This command handles the secondary squelch menu. 
+ */
+
+static int do_qual_squelch(void)
+{
+	int i, num, max_num, index;
+	int col, row;
+	char ch;
+
+	char squelch_str[5] = "NCVGA";
+
+	index = 0;
+	while (1) {
+	  /* Clear screen */
+	  Term_clear();
+	  
+	  /* Print all tval's and their descriptions */
+	  for (num = 0; (num<60) && tvals[num].tval; num++)
+	    {
+	      row = 2 + (num % 20);
+	      col = 30 * (num / 20);
+	      prt(format("(%c): %s", squelch_str[squelch_level[num]], tvals[num].desc), row, col);
+	      
+	    }
+
+	  /* Print out the rest of the screen */
+	  prt("Legend:", 2, 30);
+	  prt("N     : Squelch Nothing", 4, 30);
+	  prt("C     : Squelch Cursed Items", 5, 30);
+	  prt("V     : Squelch Average and Below", 6, 30);
+	  prt("G     : Squelch Good and Below", 7, 30);
+	  prt("A     : Squelch All but Artifacts", 8, 30);
+
+	  prt("Commands:", 11, 30);
+	  prt("Arrows: Move and adjust settings", 13, 30);
+	  prt("ncvga : Change a single setting", 14, 30);
+	  prt("NCVGA : Change all settings", 15, 30);
+	  prt("ESC   : Exit Secondary Menu", 16, 30);
+	  prt("Secondary Squelching Menu", 0,0);
+
+	  /* Need to know maximum index */
+	  max_num=num;
+
+	  /* Place the cursor */
+	  move_cursor(index+ 2, 1);
+
+
+	  /* Get a key */
+	  ch = inkey();
+
+	  /* Analyze */
+	  switch (ch)
+	    {
+	    case ESCAPE:
+	      {
+		return 0;
+	      }
+
+	    case 'n':
+	      squelch_level[index] = SQUELCH_NONE;
+	      break;
+	    case 'N':
+	      for (i=0; i<24; i++) {
+		squelch_level[i] = SQUELCH_NONE;
+	      }
+	      break;
+
+	    case 'c':
+	      squelch_level[index] = SQUELCH_CURSED;
+	      break;
+	    case 'C':
+	      for (i=0; i<24; i++) {
+		squelch_level[i] = SQUELCH_CURSED;
+	      }
+	      break;
+
+	    case 'v':
+	      squelch_level[index] = SQUELCH_AVERAGE;
+	      break;
+	    case 'V':
+	      for (i=0; i<24; i++) {
+		squelch_level[i] = SQUELCH_AVERAGE;
+	      }
+	      break;
+
+	    case 'g':
+	      squelch_level[index] = SQUELCH_GOOD;
+	      break;
+	    case 'G':
+	      for (i=0; i<24; i++) {
+		squelch_level[i] = SQUELCH_GOOD;
+	      }
+	      break;
+
+	    case 'a':
+	      squelch_level[index] = SQUELCH_ALL;
+	      break;
+	    case 'A':
+	      for (i=0; i<24; i++) {
+		squelch_level[i] = SQUELCH_ALL;
+	      }
+	      break;
+
+
+	    case '-':
+	    case '8':
+	      {
+		index = (max_num + index - 1) % max_num;
+		break;
+	      }
+	    
+	    case ' ':
+	    case '\n':
+	    case '\r':
+	    case '2':
+	      {
+		index = (index + 1) % max_num;
+		break;
+	      }
+	    
+	    case '6':
+	      {
+		squelch_level[index] = (squelch_level[index]+1)%(SQUELCH_ALL+1);
+		break;
+	      }
+	    
+	    case '4':
+	      {
+		squelch_level[index] = (SQUELCH_ALL+squelch_level[index])%(SQUELCH_ALL+1);
+		break;
+	      }
+	    
+	    default:
+	      {
+		bell("");
+		break;
+	      }
+	    }
+
+	}
+
+}
+
+/*
+ * Hack -- initialize the mapping from tvals to typevals.
+ * This is currently called every time the squelch menus are 
+ * accessed.  This can certainly be improved.
+ */
+
+void init_tv_to_type(void) 
+{
+  tv_to_type[TV_SKELETON]=TYPE_MISC;
+  tv_to_type[TV_BOTTLE]=TYPE_MISC;
+  tv_to_type[TV_JUNK]=TYPE_MISC;
+  tv_to_type[TV_SPIKE]=TYPE_MISC;
+  tv_to_type[TV_CHEST]=TYPE_MISC;
+  tv_to_type[TV_SHOT]=TYPE_AMMO;
+  tv_to_type[TV_ARROW]=TYPE_AMMO;
+  tv_to_type[TV_BOLT]=TYPE_AMMO;
+  tv_to_type[TV_BOW]=TYPE_BOW;
+  tv_to_type[TV_DIGGING]=TYPE_WEAPON2;
+  tv_to_type[TV_HAFTED]=TYPE_WEAPON2;
+  tv_to_type[TV_POLEARM]=TYPE_WEAPON2;
+  tv_to_type[TV_SWORD]=TYPE_WEAPON1;
+  tv_to_type[TV_BOOTS]=TYPE_BOOTS;
+  tv_to_type[TV_GLOVES]=TYPE_GLOVES;
+  tv_to_type[TV_HELM]=TYPE_HELM;
+  tv_to_type[TV_CROWN]=TYPE_HELM;
+  tv_to_type[TV_SHIELD]=TYPE_SHIELD;
+  tv_to_type[TV_CLOAK]=TYPE_CLOAK;
+  tv_to_type[TV_SOFT_ARMOR]=TYPE_BODY;
+  tv_to_type[TV_HARD_ARMOR]=TYPE_BODY;
+  tv_to_type[TV_DRAG_ARMOR]=TYPE_BODY;
+  tv_to_type[TV_LITE]=TYPE_MISC;
+  tv_to_type[TV_AMULET]=TYPE_AMULET;
+  tv_to_type[TV_RING]=TYPE_RING;
+  tv_to_type[TV_STAFF]=TYPE_STAFF;
+  tv_to_type[TV_WAND]=TYPE_WAND;
+  tv_to_type[TV_ROD]=TYPE_ROD;
+  tv_to_type[TV_SCROLL]=TYPE_SCROLL;
+  tv_to_type[TV_POTION]=TYPE_POTION;
+  tv_to_type[TV_FLASK]=TYPE_MISC;
+  tv_to_type[TV_FOOD]=TYPE_FOOD;
+  tv_to_type[TV_MAGIC_BOOK]=TYPE_BOOK;
+  tv_to_type[TV_PRAYER_BOOK]=TYPE_BOOK;
+}
+
+void do_cmd_squelch(void)
+{
+
+	int flag;
+	init_tv_to_type();
+
+
+	flag=1;
+	
+	/* Simple loop */
+	while (flag)  {
+	  flag = do_cmd_squelch_aux();
+	}
+
+	/* Restore the screen */
+	Term_load();
+
+	return;
+}
+
+/*
+ * These are the return values of squelch_itemp()
+ */
+
+#define SQUELCH_FAILED -1
+#define SQUELCH_NO      0
+#define SQUELCH_YES     1
+
+/*
+ * Determines if an object is going to be squelched on identification.
+ * Input:
+ *  o_ptr   : This is a pointer to the object type being identified.
+ *  feeling : This is the feeling of the object if it is being 
+ *            pseudoidentified or 0 if the object is being identified.
+ *  fullid  : This is 1 if the object is being identified and 0 otherwise.
+ * 
+ * Output: One of the three above values.
+ */
+
+int squelch_itemp(object_type *o_ptr, byte feeling, int fullid)
+{
+  int i, num, result;
+  byte feel;
+
+  /* default */
+  result = SQUELCH_NO;
+
+
+  /* Check to see if the object is eligible for squelching on id. */
+  num=-1;
+  for (i=0; tvals[i].tval; i++) {
+    if (tvals[i].tval==o_ptr->tval) {
+      num=i;
+    }
+  }
+  if (num==-1) return result;
+ 
+  /* 
+   * Get the "feeling" of the object.  If the object is being identified
+   * get the feeling returned by a heavy pseudoid.
+   */
+  feel = feeling;
+  if (fullid==1) 
+    feel = value_check_aux1(o_ptr);
+
+
+  /* Get result based on the feeling and the squelch_level */
+  switch (squelch_level[num]) 
+    {
+    case SQUELCH_NONE:
+      return result;
+      break;
+    case SQUELCH_CURSED:
+      result = (((feel==INSCRIP_BROKEN) ||
+		 (feel==INSCRIP_TERRIBLE) ||
+		 (feel==INSCRIP_WORTHLESS) ||
+		 (feel==INSCRIP_CURSED)) ? SQUELCH_YES : SQUELCH_NO);
+      break;
+    case SQUELCH_AVERAGE:
+      result = (((feel==INSCRIP_BROKEN) ||
+		 (feel==INSCRIP_TERRIBLE) ||
+		 (feel==INSCRIP_WORTHLESS) ||
+		 (feel==INSCRIP_CURSED) ||
+		 (feel==INSCRIP_AVERAGE)) ? SQUELCH_YES : SQUELCH_NO);
+      break;
+    case SQUELCH_GOOD:
+      result = (((feel==INSCRIP_BROKEN) ||
+		 (feel==INSCRIP_TERRIBLE) ||
+		 (feel==INSCRIP_WORTHLESS) ||
+		 (feel==INSCRIP_CURSED) ||
+		 (feel==INSCRIP_AVERAGE) ||
+		 (feel==INSCRIP_GOOD)) ? SQUELCH_YES : SQUELCH_NO);
+      break;
+    case SQUELCH_ALL:
+      result = SQUELCH_YES;
+      break;
+    }
+
+
+  if (result==SQUELCH_NO) return result;
+
+  /* Squelching will fail on an artifact */
+  if (artifact_p(o_ptr))
+    result = SQUELCH_FAILED;
+  
+  return result;
+
+}
+
+/*
+ * This performs the squelch, actually removing the item from the 
+ * game.  It returns 1 if the item was squelched, and 0 otherwise.  
+ * This return value is never actually used.
+ */
+int do_squelch_item(int squelch, int item, object_type *o_ptr)
+{
+
+  if (squelch != SQUELCH_YES) 
+    return 0;
+
+  if (item>0) {
+    inven_item_increase(item, -o_ptr->number);
+    inven_item_optimize(item);
+  } else {
+    floor_item_increase(0 - item, -o_ptr->number);
+    floor_item_optimize(0 - item);
+  }
+  return 1;
+}
+
+
+
+
+
diff -Nau --exclude=make* --exclude=~* --exclude=*.txt --exclude=*.o --exclude=*.bin --exclude=*~ --exclude=*.zip osrc/types.h src/types.h
--- osrc/types.h	Wed Apr 12 17:37:16 2000
+++ src/types.h	Wed Apr 12 17:48:06 2000
@@ -223,6 +223,7 @@
 	bool aware;			/* The player is "aware" of the item's effects */
 
 	bool tried;			/* The player has "tried" one of the items */
+        bool squelch;
 };
 
 
diff -Nau --exclude=make* --exclude=~* --exclude=*.txt --exclude=*.o --exclude=*.bin --exclude=*~ --exclude=*.zip osrc/xtra2.c src/xtra2.c
--- osrc/xtra2.c	Wed Apr 12 17:37:02 2000
+++ src/xtra2.c	Wed Apr 12 17:56:44 2000
@@ -1949,24 +1949,31 @@
 		if (do_gold && (!do_item || (rand_int(100) < 50)))
 		{
 			/* Make some gold */
-			if (!make_gold(i_ptr)) continue;
+			if (make_gold(i_ptr)) 
+			  {
+			    /* Assume seen XXX XXX XXX */
+			    dump_gold++;
+			    /* Drop it in the dungeon */
+			    drop_near(i_ptr, -1, y, x);
+			  }
 
-			/* Assume seen XXX XXX XXX */
-			dump_gold++;
 		}
 
 		/* Make Object */
 		else
 		{
 			/* Make an object */
-			if (!make_object(i_ptr, good, great)) continue;
+			if (make_object(i_ptr, good, great))
+			  {
+
+			    /* Assume seen XXX XXX XXX */
+			    dump_item++;
+			    /* Drop it in the dungeon */
+			    drop_near(i_ptr, -1, y, x);
+			  }
 
-			/* Assume seen XXX XXX XXX */
-			dump_item++;
 		}
 
-		/* Drop it in the dungeon */
-		drop_near(i_ptr, -1, y, x);
 	}
 
 	/* Reset the object level */
