/*
 * rational.hpp
 *
 * Copyright 2008, Kenneth Boyd.
 *
 * Very stripped-down rational class for instantiation with fundamental integer types only
 *
 * This file is under the Boost License V1.0.
 */

#ifndef RATIONAL_HPP
#define RATIONAL_HPP 1

#include <cassert>
#include "z-util.h"

namespace zaiband {

template<class T>
class rational
{
public:
	rational() : _numerator(0), _denominator(1) {};
	explicit rational(T n) : _numerator(n), _denominator(1) {};
	explicit rational(T n, T d) : _numerator(n), _denominator(d) {assert(d != 0); normalize();};

	// default copy constructor, assignment, destructor: use

	// Assign in place
   	rational& assign(T n, T d);

	// core comparison operator
	bool operator<(const rational& RHS) const;

	// Access to representation
	T numerator() const { return _numerator; };
	T denominator() const { return _denominator; };
	
private:
	T _numerator;
	T _denominator;

	void normalize();
};

// auxilliary functions
template<class T>
T
GCF(T x, T y)
{
	// Avoid Koenig lookup and library headers for abs
	if (x < 0) x = -x;
	if (y < 0) y = -y;
	if (x == y) return x;
	if (y == 0) return x;
	if (x == 0) return y;
	if (y == 1 || x==1) return 1;

	while(true)
	{
		y %= x;
		if (y == 0) return x;
		if (y == 1) return 1;
		x %= y;
		if (x == 0) return y;
		if (x == 1) return 1;
	}
}

// Assign in place
template<class T>
inline rational<T>& rational<T>::assign(T n, T d)
{
	assert(d != 0);

    _numerator = n;
    _denominator = d;
    normalize();
    return *this;
}

// comparison operators
template<class T>
bool
rational<T>::operator<(const rational<T>& RHS) const
{
	// denominator is normalized-positive, so this short-circuit works
	if (_numerator < 0)
	{
		if (RHS._numerator >= 0) return true;
	}
	else if (_numerator > 0)
	{
		if (RHS._numerator <= 0) return false;
	}
	else	// if (_numerator == 0)
	{
		return 0 < RHS._numerator;
	}

	if (RHS._numerator == 0) return _numerator < 0;
	if (RHS._denominator==_denominator) return _numerator<RHS._numerator;

	{	// evade overflow
	T cross_GCF_numerator = GCF(_numerator,RHS._numerator);
	T cross_GCF_denominator = GCF(_denominator,RHS._denominator);
	return (_numerator/cross_GCF_numerator)*(RHS._denominator/cross_GCF_denominator)<(RHS._numerator/cross_GCF_numerator)*(_denominator/cross_GCF_denominator);
	}
}

// total ordering assumed
template<class T>
inline bool
operator!=(const rational<T>& LHS, const rational<T>& RHS) {return !(LHS==RHS);}

template<class T>
inline bool
operator>(const rational<T>& LHS, const rational<T>& RHS) {return RHS<LHS;}

template<class T>
inline bool
operator<=(const rational<T>& LHS, const rational<T>& RHS) {return !(RHS<LHS);}

template<class T>
inline bool
operator>=(const rational<T>& LHS, const rational<T>& RHS) {return !(LHS<RHS);}



// Normalisation
template<class T>
void
rational<T>::normalize()
{
	assert(_denominator != 0);

	// handle numerator 0 
	if (_numerator == 0) 
	{
		_denominator = 1;
		return;
	}

    // force positive denominator
    if (_denominator < 0) {
        _numerator = -_numerator;
        _denominator = -_denominator;
    }

	{
    T g = GCF(_numerator, _denominator);
	assert(_denominator >= g);

    _numerator /= g;
    _denominator /= g;
	}
}

}	// end namespace zaiband

#endif

