/*
 * File: z-queue.h
 * Purpose: Simple circular integer queue.
 */

#ifndef INCLUDED_Z_QUEUE_H
#define INCLUDED_Z_QUEUE_H

struct queue
{
    uintptr_t *data;
    size_t size;
    int head;
    int tail;
};

extern struct queue *q_new(size_t size);
extern int q_len(struct queue *q);

extern void q_push(struct queue *q, uintptr_t item);
extern uintptr_t q_pop(struct queue *q);

extern void q_free(struct queue *q);

#define q_push_ptr(q, ptr) q_push((q), (uintptr_t)(ptr))
#define q_pop_ptr(q) ((void *)q_pop((q)))

#define q_push_int(q, i) q_push((q), (uintptr_t)(i))
#define q_pop_int(q) ((int)q_pop((q)))

#endif /* INCLUDED_Z_QUEUE_H */