
/* $Id: list.c,v 1.4 2003/03/17 22:45:22 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "z-virt.h"
#include "list.h"

static ihNode  *_IH_NodeAlloc(void);
static void     _IH_NodeFree(ihNode * node);

#undef DEBUG

void
IH_ListInit(ihList * list)
{
     if(!list)
          return;

     list->head = NULL;
     list->tail = NULL;
}

ihNode         *
IH_ListFirst(ihList * list)
{
     if(!list)
          return NULL;

     return list->head;
}

ihNode         *
IH_ListLast(ihList * list)
{
     if(!list)
          return NULL;

     return list->tail;
}

int
IH_ListCount(ihList * list)
{
     ihNode         *node;
     int             count = 0;

     if(!list)
          return 0;

     for(node = IH_ListFirst(list); node; node = IH_ListNext(list, node))
          count++;

     return count;
}

ihNode         *
IH_ListPrev(ihList * list,
            ihNode * node)
{
     if(!list)
          return NULL;
     if(!node)
          return NULL;

#ifdef DEBUG
     fprintf(stderr, "IH_ListPrev: node = %p\n", node);
     fprintf(stderr, "IH_ListPrev: node->prev = %p\n", node->prev);
#endif
     return node->prev;
}

ihNode         *
IH_ListNext(ihList * list,
            ihNode * node)
{
     if(!list)
          return NULL;
     if(!node)
          return NULL;

#ifdef DEBUG
     fprintf(stderr, "IH_ListNext: node = %p\n", node);
     fprintf(stderr, "IH_ListNext: node->next = %p\n", node->next);
#endif
     return node->next;
}

ihNode         *
IH_ListFindData(ihList * list,
                void *data)
{
     ihNode         *node = NULL;

     if(!data)
          return NULL;

     node = IH_ListFirst(list);
     while(node)
     {
          if(node->data == data)
               return node;

          node = IH_ListNext(list, node);
     }

     return NULL;
}

ihNode         *
IH_ListFindNum(ihList * list,
               int num)
{
     ihNode         *node = NULL;
     int             count = 0;

     if(num < 0)
          return NULL;

     node = IH_ListFirst(list);
     do
     {
          if(count == num)
               return node;

          node = IH_ListNext(list, node);
          count++;
     } while(node);

     return NULL;
}

void           *
IH_ListRemoveNode(ihList * list,
                  ihNode * node)
{
     ihNode         *prev, *next;
     void           *data = NULL;

     if(!list)
          return NULL;
     if(!node)
          return NULL;

     data = node->data;

     prev = node->prev;
     next = node->next;

     if(prev)
          prev->next = next;
     if(next)
          next->prev = prev;

     _IH_NodeFree(node);

     return data;
}

void           *
IH_ListRemoveData(ihList * list,
                  void *data)
{
     ihNode         *node;

     if(!list)
          return NULL;
     if(!data)
          return NULL;

     node = IH_ListFindData(list, data);
     (void) IH_ListRemoveNode(list, node);

     return data;
}

void           *
IH_ListRemoveNum(ihList * list,
                 int num)
{
     ihNode         *node;
     void           *data = NULL;

     if(!list)
          return NULL;
     if(num < 0)
          return NULL;

     node = IH_ListFindNum(list, num);
     if(node)
     {
          data = node->data;

          (void) IH_ListRemoveNode(list, node);
     }

     return data;
}

ihNode         *
IH_ListPrepend(ihList * list,
               void *data)
{
     ihNode         *node;

     if(!list)
          return NULL;
     if(!data)
          return NULL;

     node = _IH_NodeAlloc();
     if(node)
     {
          ihNode         *head;

          node->data = data;

          head = list->head;
          if(head)
          {
               head->prev = node;
               node->next = head;
          }

          list->head = node;
     }

     return node;
}

ihNode         *
IH_ListAppend(ihList * list,
              void *data)
{
     if(!list)
          return NULL;
     if(!data)
          return NULL;

#ifdef DEBUG
     fprintf(stderr, "IH_ListAppend()\n");
#endif
     return IH_ListInsert(list, list->tail, data);
}

ihNode         *
IH_ListInsert(ihList * list,
              ihNode * prev,
              void *data)
{
     ihNode         *node;

     if(!list)
          return NULL;
     if(!data)
          return NULL;

#ifdef DEBUG
     fprintf(stderr, "IH_ListInsert()\n");
#endif

     node = _IH_NodeAlloc();
     if(node)
     {
#ifdef DEBUG
          fprintf(stderr, "node = %p\n", node);
#endif

          node->data = data;

          if(prev)
          {
               ihNode         *next;

#ifdef DEBUG
               fprintf(stderr, "Appending node after prev node.\n");
#endif
               if(prev == list->tail)
               {
#ifdef DEBUG
                    fprintf(stderr, "Appending to end of list.\n");
#endif
                    list->tail = node;
               }

               next = prev->next;
               if(next)
               {
                    node->next = next;
                    next->prev = node;
               }
               else
               {
                    node->next = NULL;
               }

               prev->next = node;
               node->prev = prev;
          }
          else
          {
#ifdef DEBUG
               fprintf(stderr, "Placing node in empty list.\n");
#endif
               list->head = node;
               list->tail = node;

               node->prev = NULL;
               node->next = NULL;
          }
     }

     return node;
}

static ihNode  *
_IH_NodeAlloc(void)
{
     ihNode         *node = NULL;

     node = ralloc(sizeof(ihNode));
     if(node)
     {
          node->prev = NULL;
          node->next = NULL;
     }

     return node;
}

static void
_IH_NodeFree(ihNode * node)
{
     if(!node)
          return;

     rnfree(node);
}
