/*
 * types2.h
 *
 * Some auxilliary types
 *
 * This file is under the Boost License V1.0.
 */

#ifndef TYPES2_H
#define TYPES2_H 1

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
