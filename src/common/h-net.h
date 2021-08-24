/*
 * File: h-net.h
 * Purpose: This file includes all the needed networking stuff
 */

/* Include the socket buffer library */
#include "sockbuf.h"

/* Include the socket library for the correct OS */
#include "net-win.h"
#include "net-unix.h"

/* Include the various packet types and error codes */
#include "pack.h"

/* Include some bit-manipulation functions used in the networking code */
#include "bit.h"
