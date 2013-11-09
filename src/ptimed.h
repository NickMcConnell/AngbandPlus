#ifndef INCLUDED_PTIMED_H
#define INCLUDED_PTIMED_H

enum ptimed_action {
	PTIMED_BEGIN = 1,
	PTIMED_END,
	PTIMED_INCREASE,
	PTIMED_DECREASE,
	PTIMED_CALCULATE,
	PTIMED_STATUS_TEXT,
	PTIMED_STATUS_ABBREV,
	PTIMED_STATUS_COLOR,
};
typedef void (*ptimed_f)(enum ptimed_action action);

typedef struct ptimed_s ptimed_t;
typedef ptimed_t* ptimed_ptr;

struct ptimed_s {
	int        type;
	int        dur;
	ptimed_ptr next;
};

extern void ptimed_register(int type, ptimed_f effect);

extern void ptimed_load(FILE *fff);
extern void ptimed_save(FILE *fff);

extern void ptimed_set(int which, int dur);
extern void ptimed_clear(int which);
extern void ptimed_clear_all(void);
extern void ptimed_decrement(void);
extern void ptimed_calculate(void);
extern bool ptimed_query(int which);

enum ptimed_type {
	PTIMED_FAST = 1,
};

#endif