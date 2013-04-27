#ifndef IH_LIST_H
#define IH_LIST_H

/* File: list.h */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

typedef struct _ihList ihList;
typedef struct _ihNode ihNode;

/* Function prototypes.
 */
void IH_ListInit(ihList *list);
ihNode *IH_ListFirst(ihList *list);
ihNode *IH_ListLast(ihList *list);
int IH_ListCount(ihList *list);
ihNode *IH_ListPrev(ihList *list, ihNode *node);
ihNode *IH_ListNext(ihList *list, ihNode *node);
ihNode *IH_ListFindData(ihList *list, void *data);
ihNode *IH_ListFindNum(ihList *list, int num);
void *IH_ListRemoveNode(ihList *list, ihNode *node);
void *IH_ListRemoveData(ihList *list, void *data);
void *IH_ListRemoveNum(ihList *list, int num);
ihNode *IH_ListPrepend(ihList *list, void *data);
ihNode *IH_ListAppend(ihList *list, void *data);
ihNode *IH_ListInsert(ihList *list, ihNode *prev, void *data);

/* Data structures.
 */
struct _ihList
{
     ihNode *head;
     ihNode *tail;
};

struct _ihNode
{
     ihNode *prev;
     ihNode *next;
     void   *data;
};

#endif /* IH_LIST_H */