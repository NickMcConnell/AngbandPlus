/*
 * File: bit.h
 * Purpose: Bit handling
 */

#ifndef BIT_H
#define BIT_H

#define SET_BIT(w, bit)     ( (w) |= (bit) )
#define CLR_BIT(w, bit)     ( (w) &= ~(bit) )
#define BIT(w, bit)     ( (w) & (bit) )
#define TOGGLE_BIT(w, bit)  ( (w) ^= (bit) )

#define BITV_SIZE   (8 * sizeof(bitv_t))
#define BITV_DECL(X,N)  bitv_t (X)[((N) + BITV_SIZE - 1) / BITV_SIZE]
#define BITV_SET(X,N)   ((X)[(N) / BITV_SIZE] |= 1 << (N) % BITV_SIZE)
#define BITV_CLR(X,N)   ((X)[(N) / BITV_SIZE] &= ~(1 << (N) % BITV_SIZE))
#define BITV_ISSET(X,N) ((X)[(N) / BITV_SIZE] & (1 << (N) % BITV_SIZE))
#define BITV_TOGGLE(X,N)    ((X)[(N) / BITV_SIZE] ^= 1 << (N) % BITV_SIZE)

typedef unsigned char bitv_t;

#endif
