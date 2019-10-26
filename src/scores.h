#ifndef INCLUDED_SCORES_H
#define INCLUDED_SCORES_H

/* When keeping player scores, we cannot store ids as these may
 * change. Also, doing get_race() will segfault if r_info is not
 * created, and using certain command line options (-s) will cause
 * this bug. Strings only, please. Do we need uid? */
typedef struct {
    int   id;
    int   uid;
    char *date;
    char *version;
    int   score;
    char *name;
    char *race;
    char *subrace;
    char *class_; /* C++ keyword */
    char *subclass;
    char *sex;
    char *personality;
    int   gold;
    u32b  turns;
    int   clvl;
    int   dlvl;
    char *dungeon;
    char *killer;
    char *status;
    int   exp;   /* for oook html dumps */
    int   max_depth;
    int   fame;
} score_t, *score_ptr;
typedef bool (*score_p)(score_ptr score);

extern vec_ptr   scores_load(score_p filter);
extern void      scores_save(vec_ptr scores);
extern void      scores_display(vec_ptr scores);

extern int       scores_next_id(void);
extern void      scores_update(void);

extern score_ptr score_alloc(void);
extern score_ptr score_current(void);
extern void      score_free(score_ptr score);

#endif
