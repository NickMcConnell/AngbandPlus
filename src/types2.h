/*
 * types2.h
 *
 * Some auxilliary types
 *
 * Copyright (c) 2007 Kenneth Boyd.  This file is under the Boost License V1.0.
 */

#ifndef TYPES2_H
#define TYPES2_H 1

typedef bool int_test(int i);

#include "z-probability.h"

/*
 * dungeon grid, and similar
 * instantiate with POD only
 */
template<class T>
struct _grid
{
	T x;	/* constrains dungeon size to 255x255, in practice */
	T y;

	_grid() {};
	_grid(T _x, T _y) : x(_x),y(_y) {};
	template<class U> _grid(const _grid<U>& src) : x(src.x),y(src.y) {}

	bool operator==(const _grid& RHS) const {return x==RHS.x && y==RHS.y;};
	bool operator!=(const _grid& RHS) const {return !(*this==RHS);};

	/* technically wrong metaphor, but I don't use chained assignment operations */
	template<class U> void operator=(const _grid<U>& RHS)
		{
		x = RHS.x;
		y = RHS.y;
		}
	template<class U> void operator+=(const _grid<U>& RHS)
		{
		x += RHS.x;
		y += RHS.y;
		}
	template<class U> void operator-=(const _grid<U>& RHS)
		{
		x -= RHS.x;
		y -= RHS.y;
		}

	template<class U> _grid operator+(const _grid<U>& RHS) const	/* assumes NVRO */
		{
		return _grid(x+RHS.x,y+RHS.y);
		}
	template<class U>_grid operator-(const _grid<U>& RHS) const	/* assumes NVRO */
		{
		return _grid(x-RHS.x,y-RHS.y);
		}

	void clear() {x = 0; y =0;};
};

typedef _grid<unsigned char> coord;
typedef _grid<signed char> coord_delta;
typedef _grid<signed int> coord_scan;

typedef bool coord_action(coord g);

/*
 * typical formalisms for psuedo-random numbers
 */
struct dice_sides
{
	unsigned char dice;		/* dice */
	unsigned char sides;	/* sides */

	bool operator==(dice_sides RHS) const {return dice==RHS.dice && sides==RHS.sides;};
	bool operator!=(dice_sides RHS) const {return !(*this==RHS);};

	int maxroll() const {return (int)dice*(int)sides;};
	/* these two are technically wrong when d!=0, s=0, but that's a degenerate case */
	int minroll() const {return dice;};
	int medianroll() const {return (int)dice*((int)sides+1)/2;};
	int damroll() const {return NdS(dice,sides);};

	void set(unsigned char _sides, unsigned char _dice)
		{
		dice = _dice;
		sides = _sides;
		};

	/* STL interfaces */
	void clear() {dice = 0; sides = 0;};
};

struct dice_large_sides
{
	unsigned short sides;	/* sides */
	unsigned char dice;		/* dice */
	unsigned char _unused;

	bool operator==(dice_sides RHS) const {return dice==RHS.dice && sides==RHS.sides;};
	bool operator!=(dice_sides RHS) const {return !(*this==RHS);};

	int maxroll() const {return (int)dice*(int)sides;};
	/* these two are technically wrong when d!=0, s=0, but that's a degenerate case */
	int minroll() const {return dice;};
	int medianroll() const {return (int)dice*((int)sides+1)/2;};
	int damroll() const {return NdS(dice,sides);};

	void set(unsigned short _sides, unsigned char _dice)
		{
		dice = _dice;
		sides = _sides;
		};

	/* STL interfaces */
	void clear() {dice = 0; sides = 0;};
};

struct range_spec
{
	unsigned short base;
	dice_sides range;

	int maxroll() const {return (int)base + range.maxroll();};
	int minroll() const {return (int)base + range.minroll();};
	int medianroll() const {return (int)base + range.medianroll();};

	void set(unsigned short _base, unsigned char _dice, unsigned char _sides)
		{
		base = _base;
		range.dice = _dice;
		range.sides = _sides;
		};

	int damroll() const {return (int)base + range.damroll();};
};

struct range_large_spec
{
	dice_large_sides range;
	unsigned short base;

	int maxroll() const {return (int)base + range.maxroll();};
	int minroll() const {return (int)base + range.minroll();};
	int medianroll() const {return (int)base + range.medianroll();};

	void set(unsigned short _base, unsigned char _dice, unsigned short _sides)
		{
		base = _base;
		range.dice = _dice;
		range.sides = _sides;
		};

	int damroll() const {return (int)base + range.damroll();};
};

/*
 * Some distance functions based on approximating distance as delta-y + delta-x/2.
 */
inline unsigned int 
_length_delta_increasing_order(unsigned int dx, unsigned int dy)
{	return dy + (dx>>1);	}

inline unsigned int 
_V_length_delta(unsigned int dx, unsigned int dy)
{	return (dx<dy) ? _length_delta_increasing_order(dx,dy) : _length_delta_increasing_order(dy,dx);	}

inline unsigned int 
_XCOM_length_delta(unsigned int dx, unsigned int dy)
{	return (dx<dy) ? _length_delta_increasing_order(dx+1,dy) : _length_delta_increasing_order(dy+1,dx);	}

inline unsigned int
V_distance(signed int x1, signed int y1, signed int x2, signed int y2)
{	
	signed int delta_x = (x2>x1) ? x2-x1 : x1-x2;
	signed int delta_y = (y2>y1) ? y2-y1 : y1-y2;
	return _V_length_delta(delta_x,delta_y);
}

inline unsigned int
XCOM_distance(signed int x1, signed int y1, signed int x2, signed int y2)
{	
	signed int delta_x = (x2>x1) ? x2-x1 : x1-x2;
	signed int delta_y = (y2>y1) ? y2-y1 : y1-y2;
	return _XCOM_length_delta(delta_x,delta_y);
}

template<class T>
inline unsigned int
V_distance(_grid<T> x1, _grid<T> x2)
{
	return V_distance(x1.x,x1.y,x2.x,x2.y);
}

template<class T>
inline unsigned int
XCOM_distance(_grid<T> x1, _grid<T> x2)
{
	return XCOM_distance(x1.x,x1.y,x2.x,x2.y);
}

#endif
