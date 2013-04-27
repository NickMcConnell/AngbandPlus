// static_assert.h
// This file is under the Boost License V1.0.

#ifndef ZAIBAND_STATIC_ASSERT
/* want a static assert */
/* we have two other potential sources...a sufficiently advanced C++ compiler, or Loki */

/* From Boost 1.35 : check for sufficiently advanced C++ compiler */
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ > 2)
/* C++0x features are only enabled when -std=c++0x or -std=gnu++0x are
 * passed on the command line, which in turn defines
 * __GXX_EXPERIMENTAL_CXX0X__. 
 */
#  if defined(__GXX_EXPERIMENTAL_CXX0X__)
#	 define ZAIBAND_STATIC_ASSERT(A) static_assert(A, #A)
#  endif
#elif defined(HAVE_BOOST)
# include <boost/static_assert.hpp>
# define ZAIBAND_STATIC_ASSERT(A) BOOST_STATIC_ASSERT(A)
#else
# define ZAIBAND_STATIC_ASSERT(A)
#endif

#endif
