// POD.hpp

#ifndef POD_HPP
#define POD_HPP 1

#include <cstring>
#ifdef HAVE_BOOST
#include <boost/type_traits.hpp>
#include "static_assert.h"
#endif

// Adapted from the Zaimoni.STL.

// Copyright (c) 2006-2008 Kenneth 'Bessarion' Boyd.  This file is under the Boost License V1.0.

namespace zaiband {

/// POD_pair holds two POD objects of arbitrary type.
template<class T1, class T2>
struct POD_pair
{
#ifdef HAVE_BOOST
	ZAIBAND_STATIC_ASSERT(boost::is_pod<T1>::value);
	ZAIBAND_STATIC_ASSERT(boost::is_pod<T2>::value);
#endif

	typedef T1 first_type;
	typedef T2 second_type;

	T1 first;
	T2 second;

	// POD-struct has no constructors or destructors
};

template<class T1, class T2, class T3, class T4>
inline bool
operator==(const POD_pair<T1, T2>& x, const POD_pair<T3, T4>& y)
{	return x.first==y.first && x.second==y.second;	}

// dictionary ordering
template<class T1, class T2, class T3, class T4>
inline bool
operator<(const POD_pair<T1, T2>& x, const POD_pair<T3, T4>& y)
{
	if (x.first<y.first) return true;
	if (y.first<x.first) return false;
	return x.second<y.second;
}

// \todo: do this right, later (juggling all the const-qualification cases properly)
// basic enumerated function support
template<class T1, class T2>
T2
lookup(const POD_pair<T1, T2>* const reftable, size_t strict_UB, const T1& key)
{
	while(0<strict_UB)
	{
		if (key == reftable[--strict_UB].first) return reftable[strict_UB].second;
	}
	return T2(0);
}

template<class T1, class T2>
T1
inv_lookup(const POD_pair<T1, T2>* const reftable, size_t strict_UB, const T2& value)
{
	while(0<strict_UB)
	{
		if (value == reftable[--strict_UB].second) return reftable[strict_UB].first;
	}
	return T1(0);
}

// strings want strcmp
template<class T2>
T2
lookup(const POD_pair<const char* const, T2>* const reftable, size_t strict_UB, const char* const key)
{
	if (NULL!=key)
	{
		while(0<strict_UB)
		{
			if (0==strcmp(key, reftable[--strict_UB].first)) return reftable[strict_UB].second;
		}
	}
	return T2(0);
}

template<class T1>
T1
inv_lookup(const POD_pair<T1, const char* const>* const reftable, size_t strict_UB, const char* const value)
{
	if (NULL!=value)
	{
		while(0<strict_UB)
		{
			if (0==strcmp(value, reftable[--strict_UB].second)) return reftable[strict_UB].first;
		}
	}
	return T1(0);
}

}	// namespace zaiband

#endif
