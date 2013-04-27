
#ifndef __cplusplus
typedef struct data_panel data_panel;
typedef struct type_union type_union;
#endif

enum type_val {
	T_END = 0,
	T_INT,
	T_FLOAT,
	T_CHAR,
	T_STRING,
	T_MAX = T_STRING
};

/* this must be a POD-struct; no easy autotest, though */
struct type_union
{
	static const char printf_mode[T_MAX];

	type_val t; 
	union {
		float f;
		int i;
		char c;
		const char *s;
	} u;
};

#define TYPE_FUN(tv, T, v)	\
inline type_union v##2u(T v) { 		\
	type_union r;			\
	r.u.v = v; 				\
	r.t = tv;				\
	return r;				\
}

TYPE_FUN(T_INT, int, i)
TYPE_FUN(T_CHAR, char, c)
TYPE_FUN(T_FLOAT, float, f)
TYPE_FUN(T_STRING, const char *, s)

#undef TYPE_FUN

extern const type_union END;

/* 
 * Helper classes for the display of typed data
*/

struct data_panel
{
	enum {	MAX_FMT = 2 };

	byte color;
	const char *label;
	const char *fmt;	/* printf format argument */
	type_union value[MAX_FMT];	/* (short) arugment list */
};

