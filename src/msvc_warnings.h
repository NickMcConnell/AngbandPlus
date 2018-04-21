#ifndef INCLUDED_MSVC_WARNINGS_H
#define INCLUDED_MSVC_WARNINGS_H

#ifdef MSVC
#pragma warning (disable:4100) /* unreferenced formal parameter */
#pragma warning (disable:4820) /* X bytes padding added after FOO */

/* Conditional expression is constant
   This is actually an MSVC compiler bug as the following shows:
   if (one_in_(5)) <= Triggers warning
   {
     ...
   }
*/
#pragma warning (disable:4127) 

/* loss of data ... TODO: I should review these later but there
   are a freaking lot of them. For example:
   c = tolower(c);
   where c is "char c". Really, this is just C being poorly specified!
*/
#pragma warning (disable:4242) 

/* TODO Later: An example is 
    warning C4244: '+=' : conversion from 'int' to 's16b', possible loss of data
    o_ptr->to_h += 5 + randint1(10);
*/
#pragma warning (disable:4244) 

/* TODO Later: Nonstandard extension used for initializing structs. Redo menu code */
#pragma warning (disable:4221) 
#pragma warning (disable:4204)

/* C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\include\basetsd.h(114): warning C4668: '__midl' is not defined as a preprocessor macro, replacing with '0' for '#if/#elif' */
/* I'm pretty sure these aren't my fault :) */
#pragma warning (disable:4668)

/* Assignment in conditional ... I'm not sure that warrants a warning as it is 
   somewhat idiomatic for string processing. */
#pragma warning (disable:4706) 

#endif

#endif
