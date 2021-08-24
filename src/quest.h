#ifndef INCLUDED_QUEST_H
#define INCLUDED_QUEST_H

/* Poschengband Quests
 * [1] Specified in q_info.txt
 * [2] Defined in lib/edit/<quest_file>.txt (q.file)
 * [3] Sequenced in town files
 * [4] Rewards now specified in q_*.txt file rather than town file */

#define QF_GENERATE 0x01 /* quest generates its own level (from q.file) */
#define QF_RETAKE   0x02 /* player can leave quest without failing it (e.g. Oberon) (IN_PROGRESS -> TAKEN rather than FAILED) */
#define QF_TOWN     0x04 /* quest is assigned and rewarded from a town building */
#define QF_RANDOM   0x08 /* KILL(*): quest randomized 'on_birth' */
#define QF_ANYWHERE 0x10 /* reserved ... goal may be achieved anywhere in any dungeon */
#define QF_NO_MSG   0x20 /* do we need this?? */

enum {
    QS_UNTAKEN,
    QS_TAKEN,
    QS_IN_PROGRESS,
    QS_COMPLETED,   /* QF_TOWN: user must return to town for the reward */
    QS_FINISHED,
    QS_FAILED,      /* QF_TOWN: user must return to town for the shame message */
    QS_FAILED_DONE
};
enum {
    QG_CLEAR_LEVEL, /* the default goal need not be specified */
    QG_KILL_MON,    /* KILL(^archlich$, 5) or KILL(^warg$, 16) */
    QG_FIND_ART,    /* FIND(^sting$) */
};
struct quest_s
{
    int  id;
    cptr name;
    cptr file;
    int  level;
    int  dungeon;     /* where to find the quest? 0 -> Wilderness QUEST_ENTER(id) */
    int  flags;

    int  goal;
    sym_t goal_idx;   /* mon_race->id or art->id */
    int  goal_count;  /* Kill 23 Storm wyrms ... */
    int  goal_current;/* You have killed 19 so far ... */

    int  status;
    int  completed_lev;

    u32b seed;        /* For $RANDOM_ in quest files */
};
typedef struct quest_s quest_t, *quest_ptr;
typedef void (*quest_f)(quest_ptr q);
typedef bool (*quest_p)(quest_ptr q);

extern quest_ptr  quest_alloc(cptr name);
extern void       quest_free(quest_ptr q);
extern void       quest_change_file(quest_ptr q, cptr file);

extern void       quest_take(quest_ptr q);
extern void       quest_complete(quest_ptr q, point_t p);
extern void       quest_reward(quest_ptr q);
extern void       quest_fail(quest_ptr q);

extern str_ptr quest_get_description(quest_ptr q); /* read q->file for D: lines */
extern obj_ptr    quest_get_reward(quest_ptr q, int mode); /* read q->file for R: line and create object */
extern room_ptr   quest_get_map(quest_ptr q); /* QF_GENERATE: read q->file for the level map (M: lines) */
                  /* Note: fetching info from a q_*.txt file always allocates new memory, which *you* must delete */

extern bool       quests_init(void); /* parse lib/edit/q_info.txt */
extern void       quests_add(quest_ptr q); /* for the q_info.txt parser on N: line */
extern void       quests_cleanup(void);

extern quest_ptr  quests_get_current(void);
extern quest_ptr  quests_get(int id);
extern quest_ptr  quests_find_quest(int dungeon, int level);
extern quest_ptr  quests_parse(cptr name);
extern cptr       quests_get_name(int id);
extern vec_ptr    quests_get_all(void);
extern vec_ptr    quests_get_active(void);
extern vec_ptr    quests_get_finished(void);
extern vec_ptr    quests_get_failed(void);
extern vec_ptr    quests_get_hidden(void);
extern vec_ptr    quests_get_random(void);
                  /* Note: quest lists are returned sorted. You must vec_free() when finished. */

extern void       quests_on_birth(void); /* assign random quests */
extern void       quests_on_enter(int dungeon, int level); /* dungeon quest */
extern void       quests_on_enter_fixed(int quest_id); /* fixed town quest (FF_QUEST_ENTER) */
extern void       quests_on_kill_mon(mon_ptr mon); /* check for completion */
extern void       quests_on_get_obj(obj_ptr obj); /* check for completion */
extern bool       quests_check_leave(void); /* confirm if !OF_RETAKE and !QS_COMPLETED */
extern void       quests_on_leave(void); /* quest_fail() if !QS_COMPLETED */
extern bool       quests_allow_downstairs(void); /* check for cave_gen '>' */
extern bool       quests_allow_downshaft(void);
extern bool       quests_allow_all_spells(void); /* some quests restrict Destruction et. al. */
extern bool       quests_allow_feeling(void); /* some quests preclude receiving level feelings */

extern void       quests_display(void);
extern void       quests_wizard(void);
extern void       quests_doc(doc_ptr doc);
extern void       quests_load(savefile_ptr file);
extern void       quests_save(savefile_ptr file);

extern doc_ptr    trace_doc;
#endif
