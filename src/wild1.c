/* File: wild1.c */

/* Purpose: Wilderness generation */

/*
 * Copyright (c) 1989, 1999 James E. Wilson, Robert A. Koeneke,
 * Robert Ruehlmann
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#include "wild.h"



/*
 * This section deals with wilderness generation
 * - both at the start of the game, and sorting
 * out what routines are used to make things as
 * the player moves around.
 *
 * Note that these routines return zero as a fail
 * indicator.  They return a non zero value for
 * a success - the value being the node last added
 * to the decision tree.  (This usually can be
 * ignored.)
 */



/*
 * This function returns a wilderness block type that fits
 * the required parameters.
 *
 * The set of generation types is stored in a "decision tree"
 * - so the required time to get the wilderness type from the
 * three parameters (hgt,pop,law) is proportional to log(n).
 * This speeds up wilderness generation alot.  (Note the
 * "obvious" method of using a linear search to find matching
 * wilderness creation functions is too slow.)
 *
 * The "type" value has two different uses.  One is to specify
 * which axis of the parameter space is being split.  The other
 * is to show whether or not a node is a terminal "leaf" node.
 * If it is a leaf node - the value returned is the number of
 * the type of wilderness generation function.
 */
static u16b get_gen_type(byte hgt, byte pop, byte law)
{
	/* Current node in choice tree - node zero is the "trunk" */
	int node = 0;

	/* branch of tree to go down */
	bool branch = TRUE;

	wild_choice_tree_type *tree_ptr;

	/* Find matching generation type */

	/* The while loop is used instead of the "obvious" recursion */
	while (TRUE)
	{
		/* Access Node */
		tree_ptr = &wild_choice_tree[node];

		/*
		 * If are near end - look at leaves of tree
		 *
		 * (cutoff == 0) is used as a flag since it doesn't
		 * split the possibility tree in any useful way.
		 */
		if (tree_ptr->cutoff == 0)
		{
			/* randomly choose branch */
			if (randint1(tree_ptr->chance1 + tree_ptr->chance2) >
				tree_ptr->chance2)
			{
				/* Chance1 of going "left" */
				branch = TRUE;
			}
			else
			{
				/* Chance2 of going "right" */
				branch = FALSE;
			}
		}
		else
		{
			/*
			 * Get lower two bits of type to decribe which of
			 * (hgt,pop,law) cutoff refers to.
			 */
			switch (tree_ptr->info & 0x03)
			{
				case DT_HGT:
				{
					/* Look at height */
					if (tree_ptr->cutoff >= hgt)
					{
						branch = TRUE;
					}
					else
					{
						branch = FALSE;
					}

					break;
				}
				case DT_POP:
				{
					/* Look at population */
					if (tree_ptr->cutoff >= pop)
					{
						branch = TRUE;
					}
					else
					{
						branch = FALSE;
					}

					break;
				}
				case DT_LAW:
				{
					/* Look at lawfulness */
					if (tree_ptr->cutoff >= law)
					{
						branch = TRUE;
					}
					else
					{
						branch = FALSE;
					}

					break;
				}
				default:
				{
					msgf("Invalid stat chosen!");

					break;
				}
			}
		}


		/* Look at the proper branch of the tree */
		if (branch)
		{
			/* Go "left" */

			/* See if references leaf node */
			if (tree_ptr->info & DT_LEFT)
			{
				/* If the bit is set - leaf */
				return (tree_ptr->ptrnode1);
			}
			else
			{
				/* use the while loop to recurse */
				node = tree_ptr->ptrnode1;
			}
		}
		else
		{
			/* Go "right" */

			/* See if references leaf node */
			if (tree_ptr->info & DT_RIGHT)
			{
				/* If the bit is set - leaf */
				return (tree_ptr->ptrnode2);
			}
			else
			{
				/* use the while loop to recurse */
				node = tree_ptr->ptrnode2;
			}
		}
	}

	/* For some dumb compilers */
	return (0);
}


/*
 * The number of allocated nodes in the decision tree
 */
static u16b d_tree_count;


/*
 * This function creates a new node on the decision tree.
 * It then connects that node to the node referenced by
 * the variable "node".  The process of making the link
 * stomps on any information about the old link.
 *
 * branch == TRUE is "left", FALSE is "right"
 *
 * This function returns the location of the new node in
 * the decision tree array.
 */
static u16b create_node(u16b node, bool branch)
{
	u16b new_node;

	wild_choice_tree_type *tree_ptr;

	if (d_tree_count >= z_info->wn_max)
	{
		/*
		 * Return zero (known as the location of the tree's
		 * "root" - so can act as a flag) if all of the
		 * memory allocated has been used.
		 *
		 * Always check the return value - and report the error
		 *
		 * The number of nodes required is roughly proportional
		 * to nlog(n) for a random input of ranges.  Since the
		 * ranges in w_info.txt are in a "nice" order, the
		 * coefficient for this algorithmic complexity is fairly small.
		 */
		return (0);
	}

	/* Get location of new node */
	new_node = d_tree_count;

	/* Access old node */
	tree_ptr = &wild_choice_tree[node];

	if (branch)
	{
		/* Link new node to left of old */
		tree_ptr->ptrnode1 = new_node;

		/* Link is not to a leaf */
		tree_ptr->info &= ~DT_LEFT;
	}
	else
	{
		/* Link new node to right of old */
		tree_ptr->ptrnode2 = new_node;

		/* Link is not to a leaf */
		tree_ptr->info &= ~DT_RIGHT;
	}

	/* Increase count of allocated nodes */
	d_tree_count++;

	return (new_node);
}


/*
 * This function deletes the last node on the decision tree.
 * It is needed for a major hack when something is being added
 * to a "null" region.
 *
 * This routine, and the above routine are the only ones that can
 * modify the number of nodes in the array.  (This makes checking
 * for the array-full case much easier.)
 */
static void delete_last_node(void)
{
	d_tree_count--;
}


/*
 * This function adds a node to the tree between a "leaf"
 * and the rest of the tree.  As nodes are added to the "leaf"
 * the chance of each wilderness generation type is collated.
 *
 * Note - this function used so that several different wilderness
 * generation types can exist within the same region in parameter
 * space.  Each possibility has a "chance" associated with it.
 * At generation time - the RNG is used to determine which node
 * in the "leaf" of several different generation types is used.
 * The wilderness generation type of that node is then used.
 *
 * This function also takes care of the case when something is
 * being added to a "null" node.  "Null" nodes of type zero
 * describe areas of parameter space that are outside the currently
 * used area.  This is needed because the decision tree starts out
 * empty.  As wilderness generation types are added, the null area
 * is slowly clipped away.  If at the end of creating the decision
 * tree and there is any "null" area left, the types do not fill
 * parameter space.  This will flag an error.
 */
static u16b add_node_chance(u16b type, u16b node, bool branch)
{
	/* Is the "leaf" a tree of nodes - or a single node. */
	bool is_tree;

	/* The node inserted into the decision tree */
	u16b new_node;
	wild_choice_tree_type *tree_ptr;

	/* The old connection */
	u16b old_node;


	tree_ptr = &wild_choice_tree[node];

	if (branch)
	{
		old_node = tree_ptr->ptrnode1;

		/* Check for null case. */
		if (old_node == 0)
		{
			/* Easy - just replace with data */
			tree_ptr->ptrnode1 = type;

			/* Return current node. */
			return (node);
		}

		/* Get left leaf status */
		is_tree = (wild_choice_tree[old_node].info & DT_LEFT);
	}
	else
	{
		old_node = tree_ptr->ptrnode2;

		/* Check for null case. */
		if (old_node == 0)
		{
			/* Easy - just replace with data */
			tree_ptr->ptrnode2 = type;

			/* Return current node. */
			return (node);
		}

		/* Get right leaf status */
		is_tree = (wild_choice_tree[old_node].info & DT_RIGHT);
	}

	/* Insert new node */
	new_node = create_node(node, branch);

	/* Error if array is full */
	if (new_node == 0)
	{
		/* Return zero as error code */
		return (0);
	}

	/* Access node */
	tree_ptr = &wild_choice_tree[new_node];

	/* Cutoff = 0 since is a leaf node */
	tree_ptr->cutoff = 0;

	/* Connect to old leaf */
	tree_ptr->ptrnode1 = old_node;

	/* Connect to new type */
	tree_ptr->ptrnode2 = type;


	if (is_tree)
	{
		/* Set "info" bit-flag */
		/* Only new node is a pointer to gen. type */
		tree_ptr->info = DT_RIGHT;

		/* Calculate the chance fields */
		tree_ptr->chance1 = wild_choice_tree[old_node].chance1 +
			wild_choice_tree[old_node].chance2;

		tree_ptr->chance2 = wild_gen_data[type].chance;
	}
	else
	{
		/* Set "info" bit-flag */
		/* Both links are to wild. gen. types. */
		tree_ptr->info = DT_LEFT | DT_RIGHT;

		/* Calculate the chance fields */
		tree_ptr->chance1 = wild_gen_data[old_node].chance;
		tree_ptr->chance2 = wild_gen_data[type].chance;
	}

	/* Return location of new node if required */
	return (new_node);
}


/*
 * This function copies the contents of one "leaf" (specified by
 * node1 + branch1) to the side of another node.
 *
 * This is needed because as the tree splits the parameter space
 * the leaves occupy regions.  When new wild. gen. types are added
 * to the decision tree - the "leaves" may not match their size.
 * This means that the leaves need to be split - in other words
 * copied.
 */
static u16b copy_branch(u16b node1, bool branch1, u16b node2, bool branch2)
{
	/* This function assumes that the "leaves" are of this form:
	 *
	 *StartNode
	 * /  \
	 *x  Node
	 *    / \
	 * type Node
	 *       / \
	 *    type Node
	 *          / \
	 *       type type
	 *
	 * (Where one pointer connects to a node, and one to a wild. gen. type)
	 */

	/*
	 * The complexity of this function is due to the large number of
	 * possibilities:  both branches can be left of right, and the node
	 * can be terminal or not.  This gives a set of nested "if's" resulting
	 * in eight small sections of code.
	 */
	u16b new_node;
	u16b temp_node;

	wild_choice_tree_type *tree_ptr1;
	wild_choice_tree_type *tree_ptr2;

	/* point to node to be copied from */
	tree_ptr1 = &wild_choice_tree[node1];

	/* work out what has to be copied. */
	if (branch1)
	{
		if (tree_ptr1->info & DT_LEFT)
		{
			/* need to copy tree of nodes */

			/* make new node */
			new_node = create_node(node2, branch2);

			/* Exit on failure */
			if (new_node == 0) return (0);

			/* Point to block to copy */
			temp_node = tree_ptr1->ptrnode1;
			tree_ptr1 = &wild_choice_tree[temp_node];

			/* Point to new block */
			tree_ptr2 = &wild_choice_tree[new_node];

			/* Copy data to new node */
			tree_ptr2->info = tree_ptr1->info;
			tree_ptr2->cutoff = tree_ptr1->cutoff;
			tree_ptr2->chance1 = tree_ptr1->chance1;
			tree_ptr2->chance2 = tree_ptr1->chance2;
			tree_ptr2->ptrnode1 = tree_ptr1->ptrnode1;
			tree_ptr2->ptrnode2 = tree_ptr1->ptrnode2;

			/* Recurse along branches to this node */
			if (!(tree_ptr2->info & DT_LEFT))
			{
				/* Recurse along "left" branch */
				if (copy_branch(temp_node, TRUE, new_node, TRUE) == 0)
					return (0);
			}

			if (!(tree_ptr2->info & DT_RIGHT))
			{
				/* Recurse along "right" branch */
				if (copy_branch(temp_node, TRUE, new_node, TRUE) == 0)
					return (0);
			}

			/* Done */
			return (new_node);
		}
		else
		{
			/* point to node to be copied to */
			tree_ptr2 = &wild_choice_tree[node2];

			/* only need to copy a single wild. gen. type */
			if (branch2)
			{
				/* terminal branch */
				tree_ptr2->info |= DT_LEFT;

				/* Copy information */
				tree_ptr2->ptrnode1 = tree_ptr1->ptrnode1;
				tree_ptr2->chance1 = tree_ptr1->chance1;
			}
			else
			{
				/* terminal branch */
				tree_ptr2->info |= DT_RIGHT;

				/* Copy information */
				tree_ptr2->ptrnode2 = tree_ptr1->ptrnode1;
				tree_ptr2->chance2 = tree_ptr1->chance1;
			}

			/* done */
			return (node2);
		}
	}
	else
	{
		if (tree_ptr1->info & DT_RIGHT)
		{
			/* need to copy tree of nodes */

			/* make new node */
			new_node = create_node(node2, branch2);

			/* Exit on failure */
			if (new_node == 0) return (0);

			/* Point to block to copy */
			temp_node = tree_ptr1->ptrnode2;
			tree_ptr1 = &wild_choice_tree[temp_node];

			/* Point to new block */
			tree_ptr2 = &wild_choice_tree[new_node];

			/* Copy data to new node */
			tree_ptr2->info = tree_ptr1->info;
			tree_ptr2->cutoff = tree_ptr1->cutoff;
			tree_ptr2->chance1 = tree_ptr1->chance1;
			tree_ptr2->chance2 = tree_ptr1->chance2;
			tree_ptr2->ptrnode1 = tree_ptr1->ptrnode1;
			tree_ptr2->ptrnode2 = tree_ptr1->ptrnode2;

			/* Recurse along branches to this node */
			if (!(tree_ptr2->info & DT_LEFT))
			{
				/* Recurse along "left" branch */
				if (copy_branch(temp_node, TRUE, new_node, TRUE) == 0)
					return (0);
			}

			if (!(tree_ptr2->info & DT_RIGHT))
			{
				/* Recurse along "right" branch */
				if (copy_branch(temp_node, TRUE, new_node, TRUE) == 0)
					return (0);
			}

			/* Done */
			return (new_node);
		}
		else
		{
			/* point to node to be copied to */
			tree_ptr2 = &wild_choice_tree[node2];

			/* only need to copy a single wild. gen. type */
			if (branch2)
			{
				/* terminal branch */
				tree_ptr2->info |= DT_LEFT;

				/* Copy information */
				tree_ptr2->ptrnode1 = tree_ptr1->ptrnode2;
				tree_ptr2->chance1 = tree_ptr1->chance2;
			}
			else
			{
				/* terminal branch */
				tree_ptr2->info |= DT_RIGHT;

				/* Copy information */
				tree_ptr2->ptrnode2 = tree_ptr1->ptrnode2;
				tree_ptr2->chance2 = tree_ptr1->chance2;
			}

			/* done */
			return (node2);
		}
	}
}


/*
 * This function is used to add a wilderness generation type within another
 * typed region of parameter space described by the decision tree.  This is
 * the only function that actually extends the decision tree itself.  (The
 * add_node_chance() function increases the size of the "leaves" though.)
 *
 * The bounding box of the bigger region (number 1) is repeatedly clipped onto
 * the sides of the smaller region2.  This can result with up to 6 nodes being
 * used.  Finally, when the two regions are the same size, the add_node_chance()
 * function is called to extend the "leaves" of the decision tree.
 *
 * This function must be called with a new empty node.  The node must be
 * connected to the tree by the calling routine.  (This "feature" is so that
 * this routine can be used to initialise an empty decision tree.)  This means
 * that the calling routine must check for the "null" node + completely filled
 * case. XXX XXX
 */

static u16b add_node_inside(u16b node, u16b type1, wild_bound_box_type *bound1,
                            u16b type2, wild_bound_box_type *bound2)
{
	/* The node inserted into the decision tree */
	u16b new_node;
	wild_choice_tree_type *tree_ptr;

	tree_ptr = &wild_choice_tree[node];

	if (bound1->hgtmin != bound2->hgtmin)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->hgtmin;

		/* Excess is smaller than cutoff */
		tree_ptr->ptrnode1 = type1;

		/* Cutoff = hgt , ptrnode1 = wild. gen. type. */
		tree_ptr->info = DT_HGT | DT_LEFT;

		/* Wipe chance values (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, FALSE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	if (bound1->hgtmax != bound2->hgtmax)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->hgtmax;

		/* Excess is larger than cutoff */
		tree_ptr->ptrnode2 = type1;

		/* Cutoff = hgt , ptrnode2 = wild. gen. type. */
		tree_ptr->info = DT_HGT | DT_RIGHT;

		/* Wipe chance values (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, TRUE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	if (bound1->popmin != bound2->popmin)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->popmin;

		/* Excess is smaller than cutoff */
		tree_ptr->ptrnode1 = type1;

		/* Cutoff = pop , ptrnode1 = wild. gen. type. */
		tree_ptr->info = DT_POP | DT_LEFT;

		/* Wipe chance values (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, FALSE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	if (bound1->popmax != bound2->popmax)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->popmax;

		/* Excess is larger than cutoff */
		tree_ptr->ptrnode2 = type1;

		/* Cutoff = pop , ptrnode2 = wild. gen. type. */
		tree_ptr->info = DT_POP | DT_RIGHT;

		/* Wipe chance values (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, TRUE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	if (bound1->lawmin != bound2->lawmin)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->lawmin;

		/* Excess is smaller than cutoff */
		tree_ptr->ptrnode1 = type1;

		/* Cutoff = law , ptrnode1 = wild. gen. type. */
		tree_ptr->info = DT_LAW | DT_LEFT;

		/* Wipe chance values (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, FALSE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	if (bound1->lawmax != bound2->lawmax)
	{
		/* Split node along face of region */
		tree_ptr->cutoff = bound2->lawmax;

		/* Excess is larger than cutoff */
		tree_ptr->ptrnode2 = type1;

		/* Cutoff = law , ptrnode2 = wild. gen. type. */
		tree_ptr->info = DT_LAW | DT_RIGHT;

		/* Wipe chance values (this probably isn't needed) */
		tree_ptr->chance1 = 0;
		tree_ptr->chance2 = 0;

		/* Add new node to decision tree */
		new_node = create_node(node, TRUE);

		/* Exit if out of space */
		if (new_node == 0) return (0);

		/* reset node to current end of tree */
		node = new_node;
		tree_ptr = &wild_choice_tree[node];
	}

	/*
	 * "null" case - don't need the extra node.
	 * Hack - delete extra node and go up one (should be the last node on the
	 * array.)  XXX XXX XXX
	 * Once there - look for "null" type on _one_ branch.
	 * The other branch was previously a link to the now deleted node.
	 * Replace that link with the wilderness gen. type.
	 *
	 * This only works because we know that at least one node was added
	 * to the bottom of the array.  This is why this routine must never
	 * be called with a "null" region the same size as the region to be
	 * added.
	 */
	if (type1 == 0)
	{
		/* Delete last node on array - and move back one. */
		delete_last_node();
		node--;
		tree_ptr = &wild_choice_tree[node];

		/* look "left" for null */
		if (tree_ptr->ptrnode1 == 0)
		{
			/* Paranoia - check for both branches null */
			if (tree_ptr->ptrnode2 == 0) return (0);

			/* link to wild. gen. type. */
			tree_ptr->ptrnode2 = type2;

			/* right branch is to a wild. gen. type - not a node. */
			tree_ptr->info |= DT_RIGHT;

			/* Done */
			return (node);
		}

		/* look "right" for null */
		if (tree_ptr->ptrnode2 == 0)
		{
			/* Paranoia - check for both branches null */
			if (tree_ptr->ptrnode1 == 0) return (0);

			/* link to wild. gen. type. */
			tree_ptr->ptrnode1 = type2;

			/* left branch is to a wild. gen. type - not a node. */
			tree_ptr->info |= DT_LEFT;

			/* Done */
			return (node);
		}
	}

	/*
	 * Have two wild. gen. types that want to be in the same region of
	 * parameter space.  This is accomedated by using the "chance" fields.
	 * chance1 of going "left", and chance2 of going "right".  This state
	 * is flagged by having cutoff == 0.
	 */

	/* Set flag for existance of "chance" fields. */
	tree_ptr->cutoff = 0;

	/* connect to wild. gen. types */
	tree_ptr->ptrnode1 = type1;
	tree_ptr->ptrnode2 = type2;

	/* Set info flag to show both branches are "leaves" */
	tree_ptr->info = DT_LEFT | DT_RIGHT;

	/* Look up chances and add to node. */
	tree_ptr->chance1 = wild_gen_data[type1].chance;
	tree_ptr->chance2 = wild_gen_data[type2].chance;

	/* Done */
	return (node);
}


/*
 * This routine compares two bounding boxes
 * and returns true if they are the same.
 */
static bool compare_bounds(wild_bound_box_type *bound1,
                           wild_bound_box_type *bound2)
{
	return ((bound2->hgtmin == bound1->hgtmin) &&
			(bound2->hgtmax == bound1->hgtmax) &&
			(bound2->popmin == bound1->popmin) &&
			(bound2->popmax == bound1->popmax) &&
			(bound2->lawmin == bound1->lawmin) &&
			(bound2->lawmax == bound1->lawmax));
}

/*
 * This function adds a type within a leaf that has a bigger bounding box.
 * This means that the nodes containing the leaf are copied several times
 * until the bounding boxes match - and the node can be added to the leaf.
 *
 * This function is similar to the above one - except that the input is
 * node + branch rather than just node. (This is to simplify the copying
 * function.)
 */
static u16b inside_leaf(u16b node, u16b type, wild_bound_box_type *bound1,
                        wild_bound_box_type *bound2, bool branch)
{
	/* The node inserted into the decision tree */
	u16b new_node;
	u16b branch_node;
	wild_choice_tree_type *tree_ptr;

	tree_ptr = &wild_choice_tree[node];


	if (bound1->hgtmin != bound2->hgtmin)
	{
		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;
		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);

		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->hgtmin;

		/* Cutoff = hgt */
		tree_ptr->info = DT_HGT;

		/* work out branch to follow */
		branch = FALSE;
	}

	if (bound1->hgtmax != bound2->hgtmax)
	{
		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;
		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);
		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->hgtmax;

		/* Cutoff = hgt */
		tree_ptr->info = DT_HGT;

		/* work out branch to follow */
		branch = TRUE;
	}

	if (bound1->popmin != bound2->popmin)
	{
		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;
		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);
		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->popmin;

		/* Cutoff = pop */
		tree_ptr->info = DT_POP;

		/* work out branch to follow */
		branch = FALSE;
	}

	if (bound1->popmax != bound2->popmax)
	{
		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;
		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);
		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->popmax;

		/* Cutoff = pop */
		tree_ptr->info = DT_POP;

		/* work out branch to follow */
		branch = TRUE;
	}

	if (bound1->lawmin != bound2->lawmin)
	{
		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;
		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);
		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->lawmin;

		/* Cutoff = law */
		tree_ptr->info = DT_LAW;

		/* work out branch to follow */
		branch = FALSE;
	}

	if (bound1->lawmax != bound2->lawmax)
	{
		/* Record branch node */
		if (branch)
		{
			branch_node = tree_ptr->ptrnode1;
		}
		else
		{
			branch_node = tree_ptr->ptrnode2;
		}

		/* Make empty node connected along branch */
		new_node = create_node(node, branch);
		if (new_node == 0) return (0);

		/* Reconnect to new node */
		tree_ptr = &wild_choice_tree[new_node];
		tree_ptr->ptrnode1 = branch_node;

		/* Copy so that leaf is duplicated */
		if (copy_branch(new_node, TRUE, new_node, FALSE) == 0)
			return (0);

		/* Split node along face of region */
		tree_ptr->cutoff = bound2->lawmax;

		/* Cutoff = law */
		tree_ptr->info = DT_LAW;

		/* work out branch to follow */
		branch = TRUE;
	}

	/* Finally - merge wild. gen. type with leaf of the same size */
	return (add_node_chance(type, node, branch));
}


/*
 * This function copies the parameter bounds from one variable to another.
 */
static void copy_bounds(wild_bound_box_type *bound1,
                        wild_bound_box_type *bound2)
{
	bound2->hgtmin = bound1->hgtmin;
	bound2->hgtmax = bound1->hgtmax;

	bound2->popmin = bound1->popmin;
	bound2->popmax = bound1->popmax;

	bound2->lawmin = bound1->lawmin;
	bound2->lawmax = bound1->lawmax;
}


/*
 * Add a wilderness generation function to the decision tree.
 *
 * There are many special cases to take care of here.  First the
 * current tree is followed until the required region either
 * 1) Is split
 * 2) Is subsumed inside a "leaf" node.
 * 3) Takes over a "null" node.
 *
 * Note: Null nodes exist because no generation routine covers the
 * whole parameter space.  This means that the inital state of the
 * decision tree does not cover every case.  Therefore, as nodes are
 * added - checks are made to see if the region falls ouside of the
 * current "reach" of the tree.
 */
static u16b add_node(wild_bound_box_type *bound,
                     wild_bound_box_type *cur_bound, u16b type, u16b node)
{
	/*
	 * Temp storage of the current bounds and current type bounds
	 * (Used in splitting a region that overlaps a cutoff)
	 */
	wild_bound_box_type temp_bound1;
	wild_bound_box_type temp_bound2;

	u16b oldnode = node;

	bool branch = FALSE;

	wild_choice_tree_type *tree_ptr;


	/* Scan tree until hit a leaf or split required region */

	/* Use a while loop instead of recursion to follow tree */
	while (TRUE)
	{
		/* Access Node */
		tree_ptr = &wild_choice_tree[node];

		/* If are near end - look at leaves of tree
		 *
		 * (cutoff == 0) is used as a flag since it doesn't
		 * split the possibility tree in any useful way.
		 */
		if (tree_ptr->cutoff == 0)
		{
			/* leaf node */
			return (inside_leaf(oldnode, type, bound, cur_bound, branch));
		}
		else
		{
			/*
			 * Get lower two bits of type to decribe which of
			 * (hgt,pop,law) cutoff refers to.
			 */
			switch (tree_ptr->info & 3)
			{
				case DT_HGT:
				{
					/* Look at height */
					if (tree_ptr->cutoff >= bound->hgtmax)
					{
						branch = TRUE;

						cur_bound->hgtmax = tree_ptr->cutoff;
					}
					else if (tree_ptr->cutoff <= bound->hgtmin)
					{
						branch = FALSE;

						cur_bound->hgtmin = tree_ptr->cutoff;
					}
					else
					{
						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* upper bound = cutoff */
						temp_bound1.hgtmax = tree_ptr->cutoff;
						temp_bound2.hgtmax = tree_ptr->cutoff;

						/* rescan with smaller domain */
						if (!add_node(&temp_bound1, &temp_bound2, type, node))
							return (0);

						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* lower bound = cutoff */
						temp_bound1.hgtmin = tree_ptr->cutoff;
						temp_bound2.hgtmin = tree_ptr->cutoff;

						/* rescan with smaller domain */
						return (add_node
								(&temp_bound1, &temp_bound2, type, node));
					}
					break;
				}
				case DT_POP:
				{
					/* Look at population */
					if (tree_ptr->cutoff >= bound->popmax)
					{
						branch = TRUE;

						cur_bound->popmax = tree_ptr->cutoff;
					}
					else if (tree_ptr->cutoff <= bound->popmin)
					{
						branch = FALSE;

						cur_bound->popmin = tree_ptr->cutoff;
					}
					else
					{
						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* upper bound = cutoff */
						temp_bound1.popmax = tree_ptr->cutoff;
						temp_bound2.popmax = tree_ptr->cutoff;

						/* rescan with smaller domain */
						if (!add_node(&temp_bound1, &temp_bound2, type, node))
							return (0);

						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* lower bound = cutoff */
						temp_bound1.popmin = tree_ptr->cutoff;
						temp_bound2.popmin = tree_ptr->cutoff;

						/* rescan with smaller domain */
						return (add_node
								(&temp_bound1, &temp_bound2, type, node));
					}
					break;
				}
				case DT_LAW:
				{
					/* Look at lawfulness */
					if (tree_ptr->cutoff >= bound->lawmax)
					{
						branch = TRUE;

						cur_bound->lawmax = tree_ptr->cutoff;
					}
					else if (tree_ptr->cutoff <= bound->lawmin)
					{
						branch = FALSE;

						cur_bound->lawmin = tree_ptr->cutoff;
					}
					else
					{
						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* upper bound = cutoff */
						temp_bound1.lawmax = tree_ptr->cutoff;
						temp_bound2.lawmax = tree_ptr->cutoff;

						/* rescan with smaller domain */
						if (!add_node(&temp_bound1, &temp_bound2, type, node))
							return (0);

						/* make backups before recursion */
						copy_bounds(bound, &temp_bound1);
						copy_bounds(cur_bound, &temp_bound2);

						/* lower bound = cutoff */
						temp_bound1.lawmin = tree_ptr->cutoff;
						temp_bound2.lawmin = tree_ptr->cutoff;

						/* rescan with smaller domain */
						return (add_node
								(&temp_bound1, &temp_bound2, type, node));
					}
					break;
				}

				default:
				{
					msgf("Info - %d", tree_ptr->info);
					msgf("Invalid stat chosen!");

					break;
				}
			}
		}


		/* Look at the proper branch of the tree */
		if (branch)
		{
			/* Go "left" */

			/* See if references leaf node */
			if (tree_ptr->info & DT_LEFT)
			{
				/* Hit leaf node */

				/* store connection */
				oldnode = tree_ptr->ptrnode1;

				/* Take care of null case */
				if ((oldnode == 0) && compare_bounds(cur_bound, bound))
				{
					/* simply set the branch to point to the wild. gen. type */
					tree_ptr->ptrnode1 = type;

					/*
					 * Done - don't return zero as can happen
					 * with the root node
					 */
					return (1);
				}

				/* Make new node */
				node = create_node(node, TRUE);
				if (node == 0) return (0);

				return (add_node_inside(node, oldnode, cur_bound, type, bound));
			}
			else
			{
				/* use the while loop to recurse */
				oldnode = node;
				node = tree_ptr->ptrnode1;
			}
		}
		else
		{
			/* Go "right" */

			/* See if references leaf node */
			if (tree_ptr->info & DT_RIGHT)
			{
				/* Hit leaf node */

				/* store connection */
				oldnode = tree_ptr->ptrnode2;

				/* Take care of null case */
				if ((oldnode == 0) && compare_bounds(cur_bound, bound))
				{
					/* simply set the branch to point to the wild. gen. type */
					tree_ptr->ptrnode2 = type;

					/* done - don't return zero as can happen with the root node */
					return (1);
				}

				/* Make new node */
				node = create_node(node, FALSE);
				if (node == 0) return (0);

				return (add_node_inside(node, oldnode, cur_bound, type, bound));
			}
			else
			{
				/* use the while loop to recurse */
				oldnode = node;
				node = tree_ptr->ptrnode2;
			}
		}
	}
}


/*
 * Initialise the decision tree with the first wilderness generation type.
 */
u16b init_choice_tree(wild_bound_box_type *bound, u16b type)
{
	wild_bound_box_type start_bounds;

	/* The decision tree has one (empty) node */
	d_tree_count = 1;

	/*
	 * Set the starting bounds of the decision tree - this covers
	 * the whole parameter space used by the wilderness generation
	 * types.
	 */
	start_bounds.hgtmin = 0;
	start_bounds.hgtmax = 255;

	start_bounds.popmin = 0;
	start_bounds.popmax = 255;

	start_bounds.lawmin = 0;
	start_bounds.lawmax = 255;

	/* Assume first node is cleared by C_MAKE */

	/*
	 * Start the tree off by adding the type within a "null" region covering
	 * the whole parameter space.  (Note this routine requires one empty node.
	 * - that is why d_tree_count starts out as one.)
	 */
	return (add_node_inside(0, 0, &start_bounds, type, bound));
}


u16b add_node_tree_root(wild_bound_box_type *bound, u16b type)
{
	/* default bounds */
	wild_bound_box_type start_bounds;

	start_bounds.hgtmin = 0;
	start_bounds.hgtmax = 255;

	start_bounds.popmin = 0;
	start_bounds.popmax = 255;

	start_bounds.lawmin = 0;
	start_bounds.lawmax = 255;

	/* Add to root of tree */
	return (add_node(bound, &start_bounds, type, 0));
}


/*
 * Debug code for the wilderness decision tree.
 */
#ifdef DEBUG
void test_decision_tree(void)
{
	byte hgt, pop, law;

	u16b type;

	/* get parameters */
	msgf("Type in hgt");

	hgt = (byte)get_quantity(NULL, 255);

	msgf("Type in pop");

	pop = (byte)get_quantity(NULL, 255);

	msgf("Type in law");

	law = (byte)get_quantity(NULL, 255);

	/* Get value from decision tree */
	type = get_gen_type(hgt, pop, law);

	msgf("Type returned: %d .", type);
}

#endif /* DEBUG */


#if 0
/*
 * "Testing" function, used to find where the "invisible monster" bug
 * is being caused.
 * This tests the wilderness to see if everything is ok in the monster-
 * wilderness data structures.
 */
void test_mon_wild_integrity(void)
{
	int i, j;
	cave_type *c_ptr;
	monster_type *m_ptr;

	/* Only when in wilderness */
	if (p_ptr->depth) return;

	/* Check the wilderness */
	for (i = min_wid; i < max_wid; i++)
	{
		for (j = min_hgt; j < max_hgt; j++)
		{
			/* Point to location */
			c_ptr = area(i, j);

			/* Want a monster */
			if (!c_ptr->m_idx) continue;

			m_ptr = &m_list[c_ptr->m_idx];

			/* Dead monster? */
			if (!m_ptr->r_idx)
			{
				msgf("Dead Monster");
			}

			if (c_ptr->m_idx > m_max)
			{
				msgf("Monster index inconsistancy.");
			}

			if ((m_ptr->fy != j) || (m_ptr->fx != i))
			{
				msgf("Monster location inconsistancy.");
				msgf("Monster x, cave x,%d,%d", m_ptr->fx, i);
				msgf("Monster y, cave y,%d,%d", m_ptr->fy, j);
			}
		}
	}
}
#endif /* 0 */


/*
 * Test to see that there are no null nodes in the decision tree.
 */
static void test_wild_data(void)
{
	int i;

	for (i = 0; i < d_tree_count; i++)
	{
		if ((wild_choice_tree[i].ptrnode1 == 0) ||
			(wild_choice_tree[i].ptrnode2 == 0))
		{
			msgf("Missing value at %d ", i);
			msgf("Cutoff %d ", wild_choice_tree[i].cutoff);

			/*
			 * The "missing value" will be close to the error in
			 * w_info.txt
			 *
			 * The cutoff provides a hint as to the lawmax value of
			 * the error.  (Note - if this is zero - the error is
			 * in a "leaf" and is a bug in the code.)
			 */
		}
	}
}


/*
 * The rest of the wilderness creation routines
 * These deal with actually making the wilderness,
 * not merely picking which terrain satisfies a
 * set of constraints.
 */


/*
 * Is the specified place able to be connected by roads?
 */
static bool is_road_place(u16b place_num)
{
	place_type *pl_ptr = &place[place_num];

	switch (pl_ptr->type)
	{
		case TOWN_QUEST:
		{
			/* No roads to wilderness quests */
			return (FALSE);
		}
		
		case TOWN_DUNGEON:
		{
			dun_type *d_ptr = pl_ptr->dungeon;
			
			wild_gen2_type *w_ptr = &wild[pl_ptr->y][pl_ptr->x].trans;

			if (w_ptr->law_map + w_ptr->pop_map < 256)
			{
				/* Can we connect a track? */
				if (d_ptr->flags & DF_TRACK) return (TRUE);
			}
			else
			{
				/* Can we connect a road? */
				if (d_ptr->flags & (DF_ROAD)) return (TRUE);
			}
			
			/* No roads here */
			return (FALSE);
		}
		
		default:
		{
			/* Default to true otherwise */
			return (TRUE);
		}
	}
}


/*
 * Link two points in the wilderness with a road
 */
static void road_link(u16b x1, u16b y1, u16b x2, u16b y2)
{
	s16b xn, yn, i;
	s16b dx, dy, changex, changey;

	u16b dist = distance(x1, y1, x2, y2);

	wild_gen2_type *w_ptr;

	if (dist > 6)
	{
		/* Divide path in half and call routine twice. */
		dx = (x2 - x1) / 2;
		dy = (y2 - y1) / 2;

		if (dy != 0)
		{
			/* perturbation perpendicular to path */
			changex = randint1(ABS(dy)) - ABS(dy) / 2;
		}
		else
		{
			changex = 0;
		}

		if (dx != 0)
		{
			/* perturbation perpendicular to path */
			changey = randint1(ABS(dx)) - ABS(dx) / 2;
		}
		else
		{
			changey = 0;
		}

		xn = x1 + dx + changex;
		yn = y1 + dy + changey;

		/* Bounds checking */
		if (xn < 0) xn = 0;
		if (yn < 0) yn = 0;
		if (xn >= max_wild) xn = max_wild - 1;
		if (yn >= max_wild) yn = max_wild - 1;

		/* Link the roads up */
		road_link(x1, y1, xn, yn);
		road_link(xn, yn, x2, y2);

		/* Done */
		return;
	}

	/* Hack - not too small */
	if (dist < 2) return;

	/* Connect the road */
	for (i = 0; i <= dist; i++)
	{
		xn = x1 + i * (x2 - x1) / dist;
		yn = y1 + i * (y2 - y1) / dist;

		w_ptr = &wild[yn][xn].trans;

		/* No bridges over acid or lava */
		if (w_ptr->info & (WILD_INFO_LAVA | WILD_INFO_ACID)) continue;
		
		/* Not over ocean */
		if (w_ptr->hgt_map < 256 / SEA_FRACTION) continue;

		/* Add the road to the wilderness */
		if (w_ptr->law_map + w_ptr->pop_map < 256)
		{
			w_ptr->info |= WILD_INFO_TRACK;
		}
		else
		{
			w_ptr->info |= WILD_INFO_ROAD;
		}
	}
}


/*
 * Try to find a connecting square to a place.
 *
 * x and y point to a square outside the place.
 * A line is drawn from that point to the place.
 * The "gate" closest to the point where this
 * imaginary line crosses the town border is then
 * stored into x, y.  (Wilderness coords)
 */
static void road_connect(u16b *x, u16b *y, u16b place_num)
{
	place_type *pl_ptr = &place[place_num];

	/* Big distance */
	int dist = max_wild * 2;
	int cdist, k;

	u16b x1 = *x, y1 = *y;

	/* Check place type */
	if (pl_ptr->type == TOWN_FRACT)
	{
		for (k = 0; k < MAX_GATES; k++)
		{
			/* Get distance from gate to target square */
			cdist = distance(x1, y1, pl_ptr->x + pl_ptr->gates_x[k] / 2,
							 pl_ptr->y + pl_ptr->gates_y[k] / 2);

			if (cdist < dist)
			{
				/* save minimal path */
				dist = cdist;

				switch (k)
				{
					case 0:
					{
						*x = pl_ptr->x + pl_ptr->gates_x[0] / 2;
						*y = pl_ptr->y + pl_ptr->gates_y[0] / 2;
						break;
					}

					case 1:
					{
						*x = pl_ptr->x + pl_ptr->gates_x[1] / 2;
						*y = pl_ptr->y + pl_ptr->gates_y[1] / 2;
						break;
					}

					case 2:
					{
						*x = pl_ptr->x + pl_ptr->gates_x[2] / 2;
						*y = pl_ptr->y + pl_ptr->gates_y[2] / 2;
						break;
					}

					case 3:
					{
						*x = pl_ptr->x + pl_ptr->gates_x[3] / 2;
						*y = pl_ptr->y + pl_ptr->gates_y[3] / 2;
						break;
					}
				}
			}
		}

		/* Done */
		return;
	}

	/* Dodgy hack = just output median place square */
	*x = pl_ptr->x + pl_ptr->xsize / 2;
	*y = pl_ptr->y + pl_ptr->ysize / 2;
}


/*
 * Create the roads in the wildernes.
 *
 * Link towns that are close together.
 *
 * Look for good places to place "crossroads"
 */
static void create_roads(void)
{
	u16b i, j, places = 0, links = 0;

	u16b x1, x2, x3, y1, y2, y3;

	s16b place1, place2, place3, place4;

	u16b dist, dist2, max_dist;

	u16b **link_list;
	u16b *place_number;


	/* Find number of linkable towns */
	for (i = 1; i < place_count; i++)
	{
		if (is_road_place(i))
		{
			/* Increment number of places */
			places++;
		}
	}

	/* Make places x places array of u16b's */
	C_MAKE(link_list, places, u16b *);

	for (i = 0; i < places; i++)
	{
		C_MAKE(link_list[i], places, u16b);
	}

	/* Place lookup table */
	C_MAKE(place_number, places, u16b);

	/* Fill the lookup table */
	places = 0;

	for (i = 1; i < place_count; i++)
	{
		if (is_road_place(i))
		{
			place_number[places] = i;

			/* Increment number of places */
			places++;
		}
	}

	/* Tabulate distances less than ROAD_DIST */
	for (i = 0; i < places; i++)
	{
		for (j = i + 1; j < places; j++)
		{
			/* Get distance */
			dist = distance(place[place_number[i]].x, place[place_number[i]].y,
							place[place_number[j]].x, place[place_number[j]].y);

			/* Only save it if the distance is smaller than ROAD_DIST */
			if (dist < ROAD_DIST)
			{
				link_list[j][i] = dist;
				link_list[i][j] = dist;

				links += 2;
			}
		}
	}

	/* While there are unconnected links left */
	while (links)
	{
		max_dist = ROAD_DIST;
		place1 = -1;
		place2 = -1;

		/* Find the shortest link */
		for (i = 0; i < places; i++)
		{
			for (j = i + 1; j < places; j++)
			{
				/* Get distance */
				dist = link_list[j][i];

				/* Already linked or no link at all? */
				if (!dist) continue;

				if (dist < max_dist)
				{
					/* This link is better */
					max_dist = dist;
					place1 = i;
					place2 = j;
				}
			}
		}

		/* No third town yet */
		place3 = -1;

		/* Reset max distance so we can find another town */
		max_dist = ROAD_DIST;

		/*
		 * Compare the connections for the two places to see
		 * if they share a connection in common.
		 *
		 * Pick the shortest such dual link.
		 */
		for (i = 0; i < places; i++)
		{
			/* Want a new town */
			if ((i == place1) || (i == place2)) continue;
		
			/* Distance from place1 to the new place */
			dist = link_list[place1][i];

			/* No link? */
			if (!dist) continue;

			/* Distance from place2 to the new place */
			dist2 = link_list[place2][i];

			/* No link? */
			if (!dist2) continue;

			if ((dist2 == ROAD_DIST * 2 + 1) &&
				(link_list[i][place1] != ROAD_DIST * 2 + 1))
			{
				/* Prevent "overlinking" with third place */
				link_list[i][place1] = ROAD_DIST * 2 + 1;
				link_list[place1][i] = ROAD_DIST * 2 + 1;

				links -= 2;
			}

			if ((dist == ROAD_DIST * 2 + 1) &&
				(link_list[i][place2] != ROAD_DIST * 2 + 1))
			{
				/* Prevent "overlinking" with third place */
				link_list[i][place2] = ROAD_DIST * 2 + 1;
				link_list[place2][i] = ROAD_DIST * 2 + 1;

				links -= 2;
			}

			/* There is a link! */
			if (dist + dist2 < max_dist)
			{
				/* Save the possible cross-roads partner */
				place3 = i;

				/* Update distance so we pick the closest three places */
				max_dist = dist + dist2;
			}
		}

		if (place3 != -1)
		{
			/* Mark places as connected to each other */
			link_list[place1][place2] = ROAD_DIST * 2 + 1;
			link_list[place1][place3] = ROAD_DIST * 2 + 1;
			link_list[place2][place1] = ROAD_DIST * 2 + 1;
			link_list[place2][place3] = ROAD_DIST * 2 + 1;
			link_list[place3][place1] = ROAD_DIST * 2 + 1;
			link_list[place3][place2] = ROAD_DIST * 2 + 1;

			/* Decrement link total */
			links -= 6;

			/* Have a triangle of connected places */
			place1 = place_number[place1];
			place2 = place_number[place2];
			place3 = place_number[place3];

			/* Find midpoint */
			x2 = (place[place1].x + place[place2].x + place[place3].x) / 3;
			y2 = (place[place1].y + place[place2].y + place[place3].y) / 3;

			/* Connect the three places to the midpoint */
			x1 = x2;
			y1 = y2;

			/* Get connection square for place1 */
			road_connect(&x1, &y1, place1);

			/* Link place1 with the midpoint */
			road_link(x1, y1, x2, y2);

			x1 = x2;
			y1 = y2;

			/* Get connection square for place2 */
			road_connect(&x1, &y1, place2);

			/* Link place2 with the midpoint */
			road_link(x1, y1, x2, y2);

			x1 = x2;
			y1 = y2;

			/* Get connection square for place3 */
			road_connect(&x1, &y1, place3);

			/* Link place3 with the midpoint */
			road_link(x1, y1, x2, y2);
		}
		else
		{
			dist = link_list[place1][place2];
			max_dist = (dist / 2) + 1;

			/* Mark the two places as connected to each other */
			link_list[place1][place2] = ROAD_DIST * 2 + 1;
			link_list[place2][place1] = ROAD_DIST * 2 + 1;

			/* Decrement link total */
			links -= 2;
;
			/* Hack - save the place number in link_list */
			place3 = place1;
			place4 = place2;

			/* Hack - set j to be zero */
			j = 0;

			place1 = place_number[place1];
			place2 = place_number[place2];

			/* Get first point */
			x1 = place[place2].x;
			y1 = place[place2].y;

			/* Get second point */
			x2 = place[place2].x;
			y2 = place[place2].y;

			/*
			 * In some cases, the road will "run into" other places.
			 * The following code hopefully checks for that.
			 */
			for (i = 0; i < places; i++)
			{
				/* Ignore the places we want to connect */
				if ((i == place3) || (i == place4)) continue;

				/* Get location of the current place */
				x3 = place[place_number[i]].x;
				y3 = place[place_number[i]].y;

				/* See if is close */
				if ((distance(x1, y1, x3, y3) > max_dist) &&
					(distance(x2, y2, x3, y3) > max_dist)) continue;

				/* See if the place is "in the way" */
				if (dist_to_line(x3, y3, x1, y1, x2, y2) > dist / ROAD_MIN)
				{
					continue;
				}

				/* We have a problem - set j to be 1 (a flag) */
				j = 1;

				/* Exit */
				break;
			}

			/* If there are no problems - link the two places */
			if (j == 0)
			{
				x1 = place[place2].x;
				y1 = place[place2].y;

				/* Get connection square for place1 */
				road_connect(&x1, &y1, place1);

				x2 = x1;
				y2 = y1;

				/* Get connection square for place2 */
				road_connect(&x2, &y2, place2);

				/* Link the two places */
				road_link(x1, y1, x2, y2);
			}
		}
	}

	/* Free the array */
	for (i = 0; i < places; i++)
	{
		FREE(link_list[i]);
	}

	FREE(link_list);

	/* Place lookup table */
	FREE(place_number);
}



/*
 * Sorting hook -- comp function -- by "wilderness height"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by the value in wild[y][x].gen.hgt_map
 */
static bool ang_sort_comp_height(vptr u, vptr v, int a, int b)
{
	s16b *x = (s16b *)(u);
	s16b *y = (s16b *)(v);

	int ha, hb;

	/* Get heights */
	ha = wild[y[a]][x[a]].trans.hgt_map;
	hb = wild[y[b]][x[b]].trans.hgt_map;

	/* Compare them */
	return (ha >= hb);
}


/*
 * Sorting hook -- swap function -- by "wilderness height"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by the value in wild[y][x].gen.hgt_map
 */
static void ang_sort_swap_height(vptr u, vptr v, int a, int b)
{
	s16b *x = (s16b *)(u);
	s16b *y = (s16b *)(v);

	s16b temp;

	/* Swap "x" */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;

	/* Swap "y" */
	temp = y[a];
	y[a] = y[b];
	y[b] = temp;
}


/*
 * Make river between two points.
 * Do not change the value of the two points
 */
static void link_river(int x1, int y1, int x2, int y2)
{
	int xn, yn;
	int x, y, dx, dy, changex, changey;
	int length, l;

	length = distance(x1, y1, x2, y2);

	if (length > 6)
	{
		/*
		 * Divide path in half and call routine twice.
		 * There is a small chance of splitting the river
		 */
		dx = (x2 - x1) / 2;
		dy = (y2 - y1) / 2;

		if (dy != 0)
		{
			/* perturbation perpendicular to path */
			changex = randint1(ABS(dy)) - ABS(dy) / 2;
		}
		else
		{
			changex = 0;
		}

		if (dx != 0)
		{
			/* perturbation perpendicular to path */
			changey = randint1(ABS(dx)) - ABS(dx) / 2;
		}
		else
		{
			changey = 0;
		}

		xn = x1 + dx + changex;
		yn = y1 + dy + changey;

		/* Bounds checking */
		if (xn < 0) xn = 0;
		if (yn < 0) yn = 0;
		if (xn >= max_wild) xn = max_wild - 1;
		if (yn >= max_wild) yn = max_wild - 1;

		/* construct river out of two smaller ones */
		link_river(x1, y1, xn, yn);
		link_river(xn, yn, x2, y2);
	}
	else
	{
		/* Actually build the river */
		for (l = 0; l < length; l++)
		{
			x = x1 + l * (x2 - x1) / length;
			y = y1 + l * (y2 - y1) / length;

			/* Set the river flag */
			wild[y][x].trans.info |= WILD_INFO_WATER;
		}
	}
}


/*
 * Make a few rivers in the wilderness.
 *
 * This is done by generating a few random "starting points"
 * The highest closest points are connected by a fractal line.
 * This is repeated until the highest point is below sea level.
 */
static void create_rivers(void)
{
	int i, cur_posn, high_posn, dh, river_start;
	int cx, cy, ch;
	int r1, r2;

	long val, h_val;

	/* Number of river starting points. */
	river_start = RIVER_NUM * RIVER_NUM;

	/* paranoia - bounds checking */
	if (river_start > TEMP_MAX) river_start = TEMP_MAX;

	/* Make some random starting positions */
	for (i = 0; i < river_start; i++)
	{
		/* Evenly spread out the points */
		r1 = ((i % RIVER_NUM) * max_wild) / RIVER_NUM;
		r2 = r1 + (max_wild / RIVER_NUM);

		temp_y[i] = (s16b)rand_range(r1, r2);

		r1 = ((i / RIVER_NUM) * max_wild) / RIVER_NUM;
		r2 = r1 + (max_wild / RIVER_NUM);

		temp_x[i] = (s16b)rand_range(r1, r2);
	}

	temp_n = river_start;


	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_height;
	ang_sort_swap = ang_sort_swap_height;

	/* Sort positions by height of wilderness */
	ang_sort(temp_x, temp_y, temp_n);

	/* Start at highest position */
	cur_posn = 0;

	cx = temp_x[cur_posn];
	cy = temp_y[cur_posn];

	ch = wild[cy][cx].trans.hgt_map;

	/*
	 * Link highest position to closest next highest position.
	 * Stop when all positions above sea level are used, or
	 * (rarely) if there is only one left in the array.
	 */
	while ((ch > (256 / SEA_FRACTION)) && (temp_n > cur_posn + 1))
	{
		/* The highest position is at (0,0) in the array. */

		/* Find the closest next highest one. */
		high_posn = cur_posn + 1;

		/* Large value that should be bigger than anything below. */
		h_val = 10000;

		/* Check the other positions in the array */
		for (i = high_posn; i < temp_n; i++)
		{
			/* Hack - ignore deltas that already have been matched */
			if ((temp_x[i] == -1) || (temp_y[i] == -1)) continue;

			/* Change in Height */
			dh = ch - wild[temp_y[i]][temp_x[i]].trans.hgt_map;

			/* Small val for close high positions */
			/*val = dh + distance(cx, cy, temp_x[i], temp_y[i]); */
			val = distance(cx, cy, temp_x[i], temp_y[i]);

			/* Is this position better than previous best? */
			if (val < h_val)
			{
				h_val = val;
				high_posn = i;
			}
		}

		/* No match */
		if (h_val == 10000) break;

		/* Make river between two points */
		link_river(cx, cy, temp_x[high_posn], temp_y[high_posn]);

		/*
		 * Mega hack - flag below sea level points
		 * to stop "deltas" being made.
		 */
		if (wild[temp_y[high_posn]][temp_x[high_posn]].trans.hgt_map <
			(256 / SEA_FRACTION))
		{
			temp_x[high_posn] = -1;
			temp_y[high_posn] = -1;
		}

		/* Get new highest point */
		cur_posn++;

		cx = temp_x[cur_posn];
		cy = temp_y[cur_posn];

		while (((cx == -1) || (cy == -1)) && (cur_posn < temp_n - 1))
		{
			/* Ignore the point below sea level - already linked */
			cur_posn++;

			cx = temp_x[cur_posn];
			cy = temp_y[cur_posn];
		}

		/* Hack - failure to find a new node */
		if (cur_posn >= temp_n - 1) break;

		ch = wild[cy][cx].trans.hgt_map;
	}

	/* hack - reset viewable grid set. */
	temp_n = 0;
}


/*
 * Create random lakes.
 *
 * This is done by using the frac_block routine
 * to build a 17x17 plasma fractal.  This is interpreted
 * via a cutoff to make the lake.
 *
 * There are several types of lake - (water, lava and acid)
 * The type depends on the HPL of the location.
 *
 * Note the logic used to see that lava and acid lakes do not
 * overlap rivers, and that all lakes are above sea level.
 */
static void create_lakes(void)
{
	int count, i, j, x, y;

	wild_gen2_type *w_ptr;

	bool river, clear;
	byte lake_type;

	/* Try LAKE_NUM times */
	for (count = 0; count < LAKE_NUM; count++)
	{
		/* Make a plasma fractal */

		/* Initialise temporary block */
		clear_temp_block();
		set_temp_corner_val(WILD_BLOCK_SIZE * 256);
		set_temp_mid(WILD_BLOCK_SIZE * 64);

		/* Generate plasma factal */
		frac_block();

		/* Get location */
		x = randint1(max_wild - 16 - 1);
		y = randint1(max_wild - 16 - 1);

		/* Clear river flag */
		river = FALSE;

		/* Is the area clear? */
		clear = TRUE;

		/* Look for free space */
		for (i = x; i < x + 16; i++)
		{
			/* Early exit */
			if (!clear) break;

			for (j = y; j < y + 16; j++)
			{
				w_ptr = &wild[j][i].trans;

				/* If non-lake square */
				if (temp_block[j - y][i - x] > WILD_BLOCK_SIZE * 128) continue;

				/* Below sea level? */
				if (w_ptr->hgt_map <= 256 / SEA_FRACTION)
				{
					clear = FALSE;
					break;
				}

				if (w_ptr->info & WILD_INFO_WATER) river = TRUE;
			}
		}

		/* Try again somewhere else */
		if (!clear) continue;

		/* What type of lake do we want? */
		if (river)
		{
			/* Water */
			lake_type = 1;
		}
		else
		{
			w_ptr = &wild[y][x].trans;

			if ((w_ptr->law_map > 64) || (w_ptr->pop_map > 64))
			{
				/* Water if in lawful or populous region */
				lake_type = 1;
			}
			else
			{
				if (w_ptr->hgt_map > 128)
				{
					/* Lava */
					lake_type = 2;
				}
				else
				{
					/* Acid */
					lake_type = 3;
				}
			}
		}

		/* Make the lake */
		for (i = 0; i < 16; i++)
		{
			for (j = 0; j < 16; j++)
			{
				/* If non-lake square */
				if (temp_block[j][i] > WILD_BLOCK_SIZE * 128) continue;

				w_ptr = &wild[j + y][i + x].trans;

				switch (lake_type)
				{
					case 1:
					{
						w_ptr->info |= WILD_INFO_WATER;
						break;
					}
					case 2:
					{
						w_ptr->info |= WILD_INFO_LAVA;
						break;
					}
					case 3:
					{
						w_ptr->info |= WILD_INFO_ACID;
						break;
					}
				}
			}
		}
	}
}


/*
 * Plasma routines used to build the wilderness.
 * These store the results scaled by a factor of 16
 * (Done for a less "griddy" result.)
 */


/* Value used for sea-level calculation */
static u32b *wild_temp_dist;


/*
 * this routine probably should be an inline function or a macro.
 */
static void store_hgtmap(int x, int y, int val)
{
	/* bounds checking */
	if (val < 0) val = 0;
	if ((val / 16) >= max_wild) val = (max_wild * 16) - 1;

	/* Save distribution information */
	wild_temp_dist[val / 16]++;

	/* store the value in height-map format */
	wild[y][x].gen.hgt_map = val;

	return;
}


/*
 * This function creates the first of the three parameters used to generate
 * the wilderness.  This is done by making a plasma fractal.  The distribution
 * of the values in the height map is stored so that they can be scaled to
 * generate a wilderness with an even distribution of terrain.
 */
static void create_hgt_map(void)
{
	int grd;

	/*
	 * Fixed point variables- these are stored as 16 x normal value
	 * This gives 4 binary places of fractional part + 12 places of normal part
	 */

	int lstep, hstep, i, j, ii, jj, size;

	/*
	 * Size is one bigger than normal blocks
	 * because of speed of algorithm with size = 2^n + 1
	 */
	size = max_wild - 1;

	/* Clear the section */
	for (i = 0; i <= size; i++)
	{
		for (j = 0; j <= size; j++)
		{
			/* MAX_SHORT is a flag for "not done yet" */
			wild[j][i].gen.hgt_map = MAX_SHORT;
		}

		/* Clear distribution information */
		wild_temp_dist[i] = 0;
	}

	/* Set maximum correlation length to be 256 squares */
	grd = 16 * 16;

	/* Set the corner values just in case grd > size. */
	store_hgtmap(0, 0, randint0(size));
	store_hgtmap(size, 0, randint0(size));
	store_hgtmap(0, size, randint0(size));
	store_hgtmap(size, size, randint0(size));

	/* Initialize the step sizes */
	lstep = hstep = size * 16;
	size = size * 16;

	/*
	 * Fill in the square with fractal height data -
	 * like the 'plasma fractal' in fractint.
	 */
	while (hstep > 16)
	{
		/* Halve the step sizes */
		lstep = hstep;
		hstep /= 2;

		/* middle top to bottom. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = 0; j <= size; j += lstep)
			{
				/* cache values of i,j divided by 16 */
				ii = i / 16;
				jj = j / 16;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.hgt_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_hgtmap(ii, jj, randint1(max_wild * 16));
					}
					else
					{
						/* Average of left and right points +random bit */
						store_hgtmap(ii, jj,
									 ((wild[jj][(i - hstep) / 16].gen.hgt_map +
									   wild[jj][(i +
												 hstep) / 16].gen.hgt_map) /
									  2) + ((randint1(lstep) - hstep) / 2));
					}
				}
			}
		}


		/* middle left to right. */
		for (j = hstep; j <= size - hstep; j += lstep)
		{
			for (i = 0; i <= size; i += lstep)
			{
				/* cache values of i,j / 16 */
				ii = i / 16;
				jj = j / 16;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.hgt_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_hgtmap(ii, jj, randint1(max_wild * 16));
					}
					else
					{
						/* Average of up and down points +random bit */
						store_hgtmap(ii, jj,
									 ((wild[(j - hstep) / 16][ii].gen.hgt_map
									   +
									   wild[(j +
											 hstep) / 16][ii].gen.hgt_map) /
									  2) + ((randint1(lstep) - hstep) / 2));
					}
				}
			}
		}

		/* center. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = hstep; j <= size - hstep; j += lstep)
			{
				/* cache values of i,j / 16 */
				ii = i / 16;
				jj = j / 16;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.hgt_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_hgtmap(ii, jj, randint1(max_wild * 16));
					}
					else
					{
						/* average over all four corners + scale by 181 to
						 * reduce the effect of the square grid on the shape of the fractal */
						store_hgtmap(ii, jj,
									 ((wild[(j - hstep) / 16][(i - hstep) / 16].
									   gen.hgt_map +
									   wild[(j + hstep) / 16][(i -
															   hstep) /
															  16].gen.hgt_map +
									   wild[(j - hstep) / 16][(i +
															   hstep) /
															  16].gen.hgt_map +
									   wild[(j + hstep) / 16][(i +
															   hstep) /
															  16].gen.hgt_map) /
									  4) +
									 (((randint1(lstep) - hstep) * 181) / 256));
					}
				}
			}
		}
	}
}


/*
 * this routine probably should be an inline function or a macro.
 */
static void store_popmap(int x, int y, int val, u16b sea)
{
	/* bounds checking */
	if (val < 0) val = 0;
	if ((val / 16) >= max_wild) val = (max_wild * 16) - 1;

	/* Save distribution information (only if not below sea level) */
	if (wild[y][x].gen.hgt_map > sea) wild_temp_dist[val / 16]++;

	/* store the value in height-map format */
	wild[y][x].gen.pop_map = val;

	return;
}


/*
 * This function creates the second of the three parameters used to generate
 * the wilderness.  This is done by making a plasma fractal.
 */
static void create_pop_map(u16b sea)
{
	int grd;

	/*
	 * fixed point variables- these are stored as 16 x normal value
	 * this gives 4 binary places of fractional part + 12 places of normal part
	 */

	int lstep, hstep, i, j, ii, jj, size;

	/* Size is one bigger than normal blocks for speed of algorithm with 2^n + 1 */
	size = max_wild - 1;

	/* Clear the section */
	for (i = 0; i <= size; i++)
	{
		for (j = 0; j <= size; j++)
		{
			/* MAX_SHORT is a flag for "not done yet" */
			wild[j][i].gen.pop_map = MAX_SHORT;
		}

		/* Clear distribution information */
		wild_temp_dist[i] = 0;
	}

	/* Set maximum correlation length to be 256 squares */
	grd = 16 * 16;

	/* Set the corner values just in case grd > size. */
	store_popmap(0, 0, randint0(size), sea);
	store_popmap(size, 0, randint0(size), sea);
	store_popmap(0, size, randint0(size), sea);
	store_popmap(size, size, randint0(size), sea);

	/* Initialize the step sizes */
	lstep = hstep = size * 16;
	size = size * 16;

	/*
	 * Fill in the square with fractal height data -
	 * like the 'plasma fractal' in fractint.
	 */
	while (hstep > 16)
	{
		/* Halve the step sizes */
		lstep = hstep;
		hstep /= 2;

		/* middle top to bottom. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = 0; j <= size; j += lstep)
			{
				/* cache values of i,j divided by 16 */
				ii = i / 16;
				jj = j / 16;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.pop_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_popmap(ii, jj, randint1(max_wild * 16), sea);
					}
					else
					{
						/* Average of left and right points +random bit */
						store_popmap(ii, jj,
									 ((wild[jj][(i - hstep) / 16].gen.pop_map +
									   wild[jj][(i +
												 hstep) / 16].gen.pop_map) /
									  2) + ((randint1(lstep) - hstep) / 2),
									 sea);
					}
				}
			}
		}


		/* middle left to right. */
		for (j = hstep; j <= size - hstep; j += lstep)
		{
			for (i = 0; i <= size; i += lstep)
			{
				/* cache values of i,j / 16 */
				ii = i / 16;
				jj = j / 16;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.pop_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_popmap(ii, jj, randint1(max_wild * 16), sea);
					}
					else
					{
						/* Average of up and down points +random bit */
						store_popmap(ii, jj,
									 ((wild[(j - hstep) / 16][ii].gen.pop_map
									   +
									   wild[(j +
											 hstep) / 16][ii].gen.pop_map) /
									  2) + ((randint1(lstep) - hstep) / 2),
									 sea);
					}
				}
			}
		}

		/* center. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = hstep; j <= size - hstep; j += lstep)
			{
				/* cache values of i,j / 16 */
				ii = i / 16;
				jj = j / 16;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.pop_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_popmap(ii, jj, randint1(max_wild * 16), sea);
					}
					else
					{
						/* average over all four corners + scale by 181 to
						 * reduce the effect of the square grid on the shape of the fractal */
						store_popmap(ii, jj,
									 ((wild[(j - hstep) / 16][(i - hstep) / 16].
									   gen.pop_map +
									   wild[(j + hstep) / 16][(i -
															   hstep) /
															  16].gen.pop_map +
									   wild[(j - hstep) / 16][(i +
															   hstep) /
															  16].gen.pop_map +
									   wild[(j + hstep) / 16][(i +
															   hstep) /
															  16].gen.pop_map) /
									  4) +
									 (((randint1(lstep) - hstep) * 181) / 256),
									 sea);
					}
				}
			}
		}
	}
}


/*
 * this routine probably should be an inline function or a macro.
 */
static void store_lawmap(int x, int y, int val, u16b sea)
{
	/* bounds checking */
	if (val < 0) val = 0;
	if ((val / 16) >= max_wild) val = (max_wild * 16) - 1;

	/* Save distribution information (only if not below sea level) */
	if (wild[y][x].gen.hgt_map > sea) wild_temp_dist[val / 16]++;

	/* store the value in height-map format */
	wild[y][x].gen.law_map = val;

	return;
}


/*
 * This function creates the third of the three parameters used to generate
 * the wilderness.  This is done by making a plasma fractal.
 */
static void create_law_map(u16b sea)
{
	int grd;

	/*
	 * fixed point variables- these are stored as 16 x normal value
	 * this gives 4 binary places of fractional part + 12 places of normal part
	 */

	int lstep, hstep, i, j, ii, jj, size;

	/*
	 * Size is one bigger than normal blocks for speed of
	 * algorithm with 2^n + 1
	 */

	size = max_wild - 1;

	/* Clear the section */
	for (i = 0; i <= size; i++)
	{
		for (j = 0; j <= size; j++)
		{
			/* MAX_SHORT is a flag for "not done yet" */
			wild[j][i].gen.law_map = MAX_SHORT;
		}

		/* Clear distribution information */
		wild_temp_dist[i] = 0;
	}

	/* Set maximum correlation length to be 256 squares */
	grd = 16 * 16;

	/* Set the corner values just in case grd > size. */
	store_lawmap(0, 0, randint0(size), sea);
	store_lawmap(size, 0, randint0(size), sea);
	store_lawmap(0, size, randint0(size), sea);
	store_lawmap(size, size, randint0(size), sea);

	/* Initialize the step sizes */
	lstep = hstep = size * 16;
	size = size * 16;

	/*
	 * Fill in the square with fractal height data -
	 * like the 'plasma fractal' in fractint.
	 */
	while (hstep > 16)
	{
		/* Halve the step sizes */
		lstep = hstep;
		hstep /= 2;

		/* middle top to bottom. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = 0; j <= size; j += lstep)
			{
				/* cache values of i,j divided by 16 */
				ii = i / 16;
				jj = j / 16;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.law_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_lawmap(ii, jj, randint1(max_wild * 16), sea);
					}
					else
					{
						/* Average of left and right points +random bit */
						store_lawmap(ii, jj,
									 ((wild[jj][(i - hstep) / 16].gen.law_map +
									   wild[jj][(i +
												 hstep) / 16].gen.law_map) /
									  2) + ((randint1(lstep) - hstep) / 2),
									 sea);
					}
				}
			}
		}

		/* middle left to right. */
		for (j = hstep; j <= size - hstep; j += lstep)
		{
			for (i = 0; i <= size; i += lstep)
			{
				/* cache values of i,j / 16 */
				ii = i / 16;
				jj = j / 16;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.law_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_lawmap(ii, jj, randint1(max_wild * 16), sea);
					}
					else
					{
						/* Average of up and down points +random bit */
						store_lawmap(ii, jj,
									 ((wild[(j - hstep) / 16][ii].gen.law_map
									   +
									   wild[(j +
											 hstep) / 16][ii].gen.law_map) /
									  2) + ((randint1(lstep) - hstep) / 2),
									 sea);
					}
				}
			}
		}

		/* center. */
		for (i = hstep; i <= size - hstep; i += lstep)
		{
			for (j = hstep; j <= size - hstep; j += lstep)
			{
				/* cache values of i,j / 16 */
				ii = i / 16;
				jj = j / 16;

				/* only write to points that are "blank" */
				if (wild[jj][ii].gen.law_map == MAX_SHORT)
				{
					if (hstep > grd)
					{
						/* If greater than 'grid' level then is random */
						store_lawmap(ii, jj, randint1(max_wild * 16), sea);
					}
					else
					{
						/* average over all four corners + scale by 181 to
						 * reduce the effect of the square grid on the shape of the fractal */
						store_lawmap(ii, jj,
									 ((wild[(j - hstep) / 16][(i - hstep) / 16].
									   gen.law_map +
									   wild[(j + hstep) / 16][(i -
															   hstep) /
															  16].gen.law_map +
									   wild[(j - hstep) / 16][(i +
															   hstep) /
															  16].gen.law_map +
									   wild[(j + hstep) / 16][(i +
															   hstep) /
															  16].gen.law_map) /
									  4) +
									 (((randint1(lstep) - hstep) * 181) / 256),
									 sea);
					}
				}
			}
		}
	}
}


/*
 * Finish making the wilderness - recenter the screen around the player.
 */
static void wild_done(void)
{
	p_ptr->px = (s16b)p_ptr->wilderness_x;
	p_ptr->py = (s16b)p_ptr->wilderness_y;

	/* Refresh random number seed */
	wild_seed = randint0(0x10000000);

	/* Change back to inside wilderness */
	p_ptr->depth = 0;

	/* Change to the wilderness */
	change_level(0);
	
	/* We now are in the wilderness */
	character_dungeon = TRUE;
}


/*
 * Make the vanilla wilderness with only the 'standard' town
 */
static void create_vanilla_wilderness(void)
{
	int i, j;

	wild_type *w_ptr;

	/* Tiny wilderness */
	max_wild = WILD_VIEW + 1;

	/* Mega Hack - make an "empty" wilderness. */
	for (i = 0; i < max_wild; i++)
	{
		for (j = 0; j < max_wild; j++)
		{
			/* Mega Hack - Use the 0 value (normally empty) to denote grass. */
			w_ptr = &wild[j][i];

			w_ptr->done.wild = 0;

			/* Nothing interesting here */
			w_ptr->done.info = 0;

			/* No town yet */
			w_ptr->done.place = 0;

			/* Monsters are easy */
			w_ptr->done.mon_gen = 0;

			/* Monsters are fairly common */
			w_ptr->done.mon_prob = 64;
		}
	}

	/* Make a single vanilla town. */
	init_vanilla_town();

	/* Done */
	wild_done();
}


/*
 * Set wilderness stats depending on town type
 */
static void set_mon_wild_values(byte town_type, wild_done_type *w_ptr)
{
	/* This function is very rudimentary at the moment */

	/* One and only one type of monster distribution */
	switch (town_type)
	{
		case TOWN_MONST_VILLAGER:
		{
			/* Monsters are easy */
			w_ptr->mon_gen = 0;

			/* Monsters are fairly common */
			w_ptr->mon_prob = 64;
			break;
		}

		case TOWN_MONST_ABANDONED:
		{
			/* Monsters are moderately difficult */
			w_ptr->mon_gen = 30;

			/* Monsters are rare */
			w_ptr->mon_prob = 0;
			break;
		}

		case TOWN_MONST_MONST:
		{
			/* Do nothing - keep defaults */
			break;
		}

			/*
			 * Add in other probabilities in here for the
			 * other TOWN_MONST_XXX types
			 */

		default:
		{
			/* Monsters are easy */
			w_ptr->mon_gen = 0;

			/* Monsters are fairly common */
			w_ptr->mon_prob = 64;
		}
	}
}


/*
 * Clear the wilderness
 */
static void wipe_wilderness(void)
{
	int i, j;

	place_type *pl_ptr;
	wild_type *w_ptr;

	/* Erase all places */
	for (i = 1; i < z_info->wp_max; i++)
	{
		pl_ptr = &place[i];

		/* Do we have any stores? */
		if (pl_ptr->numstores)
		{
			/* Free the stores */
			FREE(pl_ptr->store);
		}
		
		/* Free the dungeon data */
		if (pl_ptr->dungeon)
		{
			FREE(pl_ptr->dungeon);
		}

		/* Wipe the place */
		(void)WIPE(pl_ptr, place_type);
	}

	/* Wipe the wild info */
	for (i = 0; i < max_wild; i++)
	{
		for (j = 0; j < max_wild; j++)
		{
			w_ptr = &wild[j][i];

			/* Wipe the block */
			(void)WIPE(w_ptr, wild_type);
		}
	}
}


/*
 * Create the random terrain information and fill in
 * the "gen" info in the wild structure.
 *
 * Using that, fill in the transition wild structure
 * with the height, population an "law" information.
 */
static void create_wild_info(int *bestx, int *besty)
{
	int i, j;
	int x, y;

	byte hgt, pop, law;
	u16b hgt_min, hgt_max, pop_min, pop_max;
	byte sea_level;
	int t;

	long hgt_scale, pop_scale;

	wild_type *w_ptr;

	/* Huge wilderness */
	max_wild = WILD_SIZE;
	C_MAKE(wild_temp_dist, WILD_SIZE, u32b);

	/* Create "height" information of wilderness */
	create_hgt_map();

	/* Work out extremes of height so it can be scaled. */
	hgt_min = hgt_max = pop_min = pop_max = 0;

	/* Minimum height */
	for (i = 0; i < max_wild; i++)
	{
		if (wild_temp_dist[i] != 0)
		{
			hgt_min = i;
			break;
		}
	}

	/* Maximum height */
	for (i = max_wild - 1; i >= 0; i--)
	{
		if (wild_temp_dist[i] != 0)
		{
			hgt_max = i;
			break;
		}
	}

	/* Height scale factor */
	hgt_scale = (hgt_max - hgt_min);

	/*
	 * The sea covers 1/SEA_FRACTION of the wilderness
	 */
	sea_level = (byte)(hgt_scale / SEA_FRACTION);

	hgt_min *= 16;

	/* Create "population density" information */
	create_pop_map((u16b) (sea_level * 16 + hgt_min));

	/* Work out extremes of population so it can be scaled. */

	/* Minimum population */
	for (i = 0; i < max_wild; i++)
	{
		if (wild_temp_dist[i] != 0)
		{
			pop_min = i;
			break;
		}
	}

	/* Maximum population */
	for (i = max_wild - 1; i >= 0; i--)
	{
		if (wild_temp_dist[i] != 0)
		{
			pop_max = i;
			break;
		}
	}

	/* Population scale factor */
	pop_scale = (pop_max - pop_min);

	/* Rescale minimum. */
	pop_min *= 16;

	create_law_map((u16b) (sea_level * 16 + hgt_min));

	/* Work out extremes of "lawfulness" so it can be scaled. */

	/* Calculate lawfulness map */
	for (i = t = 0; i < max_wild; i++)
	{
		t += wild_temp_dist[i];
		wild_temp_dist[i] = t / (max_wild * max_wild / 256);
	}

	/* Best place in wilderness for starting town */
	x = -1;
	y = -1;

	/* Fill wilderness with scaled information */
	for (i = 0; i < max_wild; i++)
	{
		for (j = 0; j < max_wild; j++)
		{
			/* Get wilderness grid */
			w_ptr = &wild[j][i];

			/*
			 * Store parameters before change the information
			 * in the union.  (Want to scale values to be 0 - 255)
			 */

			hgt = (byte)((w_ptr->gen.hgt_map - hgt_min) * 16 / hgt_scale);
			pop = (byte)((w_ptr->gen.pop_map - pop_min) * 16 / pop_scale);
			law = (byte) wild_temp_dist[w_ptr->gen.law_map / 16];

			/*
			 * Go to transition data structure
			 */
			w_ptr->trans.hgt_map = hgt;
			w_ptr->trans.pop_map = pop;
			w_ptr->trans.law_map = law;

			/* No town yet */
			w_ptr->trans.place = 0;

			/* No info flags set yet */
			w_ptr->trans.info = 0;

			/* How good is this spot to put a town? */
			if ((law > 230) && (hgt > 160))
			{
				/* Hack - Only record the first such place */
				if ((x == -1) && (y == -1))
				{
					x = i;
					y = j;
				}
			}
		}
	}

	/* Save best town location */
	*bestx = x;
	*besty = y;
	
	/* Free the temp data */
	FREE(wild_temp_dist);
}


/*
 * Fill the wilderness with terrain
 *
 * Convert from the transition structure
 * to the final "done" structure.
 */
static void create_terrain(void)
{
	int i, j, k;
	int x, y;

	byte hgt, pop, law;

	wild_type *w_ptr;

	/* Fill wilderness with terrain */
	for (i = 0; i < max_wild; i++)
	{
		for (j = 0; j < max_wild; j++)
		{
			byte place_num, info;

			/* Get wilderness grid */
			w_ptr = &wild[j][i];

			/* Save town and info status */
			place_num = w_ptr->trans.place;
			info = w_ptr->trans.info;

			/* Get HPL of grid */
			hgt = w_ptr->trans.hgt_map;
			pop = w_ptr->trans.pop_map;
			law = w_ptr->trans.law_map;

			if (hgt < 256 / SEA_FRACTION)
			{
				/* Ocean */
				wild[j][i].done.wild = 65535 - hgt;

				if (hgt > 128 / SEA_FRACTION)
				{
					/* Set to be water boundary */
					w_ptr->done.info = WILD_INFO_WATER;
				}
				else
				{
					/* No rivers / roads / all unknown */
					w_ptr->done.info = 0;
				}
			}
			else
			{
				/* Rescale the height */
				hgt = hgt - 256 / SEA_FRACTION;
				hgt = (hgt * SEA_FRACTION) / (SEA_FRACTION - 1);

				/* Get wilderness type. */
				w_ptr->done.wild = get_gen_type(hgt, pop, law);
			}

			/* Town */
			w_ptr->done.place = place_num;



			/* Set wilderness monsters to default values */

			/* Toughness (level 0 - 64) */
			w_ptr->done.mon_gen = (256 - law) / 4;
			w_ptr->done.mon_gen = MAX(1, w_ptr->done.mon_gen - 5);

			/* No monsters (probability 0 - 16) */
			w_ptr->done.mon_prob = pop / 16;

			if (place_num)
			{
				/* Set values depending on type of place */
				set_mon_wild_values(place[place_num].monst_type, &w_ptr->done);
			}

			/* Info flags */
			w_ptr->done.info = info;
		}
	}

	/* Create ocean boundaries (This might be very slow) */
	for (i = 0; i < max_wild; i++)
	{
		for (j = 0; j < max_wild; j++)
		{
			if (wild[j][i].done.wild >= WILD_SEA)
			{
				for (k = 0; k < 8; k++)
				{
					x = i + ddx_ddd[k];
					y = j + ddy_ddd[k];

					/* Must be in bounds */
					if ((x < 0) || (x >= max_wild) ||
						(y < 0) || (y >= max_wild))
					{
						continue;
					}

					/* Get wilderness grid */
					w_ptr = &wild[y][x];

					/* Is ocean? */
					if (w_ptr->done.wild < WILD_SEA)
					{
						/* 
						 * Set all squares next to ocean to be "water"
						 * This makes the ocean boundaries look like
						 * those of rivers - rough on the sub-block
						 * level.
						 */
						w_ptr->done.info |= WILD_INFO_WATER;
					}
				}
			}
		}
	}

}

/*
 * Create the wilderness
 *
 * This is done by making three plasma fractals
 * The three values for each 16x16 block are then passed into
 * the decision tree code to get a wilderness type.  (This
 * is done for speed.  The binary tree takes O(log(n)) steps to
 * find a matching type from w_info.txt, a linear search will
 * obviously be a O(n) algorithm.  With hundreds of types, the
 * difference is noticable.
 *
 * The old three values for height, law level, and population level
 * are then merged to work out the monster generation statistics for
 * each 16x16 block.
 *
 * Finally large features like towns, rivers, roads and lakes are placed.
 */
void create_wilderness(void)
{
	int x, y;

	bool done = FALSE;
	
	/* Invalidate the player while we make everything */
	character_dungeon = FALSE;

	/*
	 * XXX XXX Hack  Pretend we have a loaded player.
	 * (Must make sure the object, monsters and fields created
	 * now do not break the savefile, if we are half-way through
	 * loading it.)
	 *
	 * We also must make sure we create the objects, monsters and fields
	 * if we are just starting the game.
	 */
	character_loaded = TRUE;

	/* Delete everything */
	wipe_rg_list();

	/* Test wilderness generation information */
	test_wild_data();

	/* Minimal wilderness */
	if (vanilla_town)
	{
		create_vanilla_wilderness();
		return;
	}

	/*
	 * Try to make the wilderness until we
	 * get one that works.
	 *
	 * Keep track of how long it takes - and
	 * if it takes too long, bail out.
	 */
	while (!done)
	{
		/* Clear the wilderness */
		wipe_wilderness();

		/* Create the height, population, and law info */
		create_wild_info(&x, &y);

		/*
		 * Add in large level features.
		 */

		/* Add in rivers... */
		create_rivers();

		/* Add in lakes... */
		create_lakes();

		/* Add towns + dungeons etc */
		done = init_places(x, y);
	}

	/* Connect the places with roads */
	create_roads();

	/*
	 * Finish everything off
	 */

	/* Convert the wilderness into the final data structure */
	create_terrain();

	/*
	 * We can check the wilderness structures in debug mode - 
	 * So don't delete them in that case...
	 */
#ifndef DEBUG

	/* Free up memory used to create the wilderness */
	FREE(wild_choice_tree);

#endif /* !DEBUG */

	/* Done */
	wild_done();
}
