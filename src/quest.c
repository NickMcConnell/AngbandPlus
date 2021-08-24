#include "angband.h"
#include "dun.h"
#include <assert.h>

doc_ptr trace_doc = NULL;

static cptr _strcpy(cptr s)
{
    char *r = malloc(strlen(s)+1);
    strcpy(r, s);
    return r;
}

/************************************************************************
 * Quest
 ***********************************************************************/
quest_ptr quest_alloc(cptr name)
{
    quest_ptr q = malloc(sizeof(quest_t));
    memset(q, 0, sizeof(quest_t));
    assert(name);
    q->name = _strcpy(name);
    return q;
}

void quest_free(quest_ptr q)
{
    if (q)
    {
        free((vptr)q->name);
        if (q->file) free((vptr)q->file);
        free(q);
    }
}

void quest_change_file(quest_ptr q, cptr file)
{
    assert(q);
    if (q->file) free((vptr)q->file);
    if (file) q->file = _strcpy(file);
    else q->file = NULL;
}

/************************************************************************
 * Quest Status
 ***********************************************************************/
void quest_take(quest_ptr q)
{
    str_ptr s;
    assert(q);
    assert(q->status == QS_UNTAKEN);
    q->status = QS_TAKEN;
    q->seed = randint0(0x10000000);
    s = quest_get_description(q);
    msg_format("<color:R>%s</color> (<color:U>Level %d</color>): %s",
        q->name, q->level, str_buffer(s));
    str_free(s);
}

void quest_complete(quest_ptr q, point_t p)
{
    assert(q);
    assert(q->status == QS_IN_PROGRESS);
    q->status = QS_COMPLETED;
    q->completed_lev = plr->lev;

    virtue_add(VIRTUE_VALOUR, 2);
    plr->fame += randint1(2);

    if (!(q->flags & QF_NO_MSG))
        cmsg_print(TERM_L_BLUE, "You just completed your quest!");
    msg_add_tiny_screenshot(50, 24);

    /* create stairs before the reward */
    if (q->dungeon)
        dun_quest_stairs(cave, plr->pos, q->level + 1);

    if (!(q->flags & QF_TOWN)) /* non-town quest get rewarded immediately */
    {
        int i, ct = q->level/25 + 1;
        for (i = 0; i < ct; i++)
        {
            obj_t forge = {0};
            if (make_object(&forge, q->level, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                drop_near(&forge, p, -1);
            else
                msg_print("Software Bug ... you missed out on your reward!");
        }
        q->status = QS_FINISHED;
    }
    plr->redraw |= PR_DEPTH;
}

void quest_reward(quest_ptr q)
{
    str_ptr s;
    obj_ptr reward;

    assert(q);
    assert(q->status == QS_COMPLETED);

    s = quest_get_description(q);
    msg_format("<color:R>%s</color> (<color:U>Level %d</color>): %s",
        q->name, q->level, str_buffer(s));
    str_free(s);

    reward = quest_get_reward(q, 0);
    if (reward)
    {
        /*char name[MAX_NLEN];*/
        obj_identify_fully(reward);
        /*object_desc(name, reward, OD_COLOR_CODED);
        msg_format("You receive %s as a reward.", name);*/
        pack_carry(reward);
        obj_free(reward);
    }

    q->status = QS_FINISHED;
}

void quest_fail(quest_ptr q)
{
    assert(q);
    q->status = QS_FAILED;
    q->completed_lev = plr->lev;
    msg_format("You have <color:v>failed</color> the quest: <color:R>%s</color>.", q->name);
    virtue_add(VIRTUE_VALOUR, -2);
    fame_on_failure();
    if (!(q->flags & QF_TOWN))
        q->status = QS_FAILED_DONE;
}

/************************************************************************
 * Quest Info from q->file
 ***********************************************************************/
static str_ptr _temp_desc;
static errr _parse_desc(char *line, int options)
{
    if (line[0] == 'D' && line[1] == ':')
    {
        if (str_length(_temp_desc) && str_get_last(_temp_desc) != ' ')
            str_append_c(_temp_desc, ' ');
        str_append_s(_temp_desc, line + 2);
    }
    return 0;
}
str_ptr quest_get_description(quest_ptr q)
{
    str_ptr s = str_alloc();
    if (q->file)
    {
        _temp_desc = s;
        parse_edit_file(q->file, _parse_desc, 0);
        _temp_desc = NULL;
    }
    return s;
}

static obj_drop_ptr _temp_reward;
static errr _parse_reward(char *line, int options)
{
    if (line[0] == 'R' && line[1] == ':')
    {
        /* It is common to set a default reward, and then
         * to over-write it later for class specific rewards.
         * The last reward line wins. */
        memset(_temp_reward, 0, sizeof(obj_drop_t));
        return obj_drop_parse(line + 2, _temp_reward, options);
    }
    return 0;
}
obj_ptr quest_get_reward(quest_ptr q, int mode)
{
    obj_ptr reward = NULL;
    if (q->file)
    {
        obj_drop_ptr info = malloc(sizeof(obj_drop_t));
        memset(info, 0, sizeof(obj_drop_t));
        _temp_reward = info;
        if (parse_edit_file(q->file, _parse_reward, 0) == ERROR_SUCCESS)
        {
            reward = obj_drop_make(info, q->level, AM_QUEST | AM_GOOD | mode);
        }
        _temp_reward = NULL;
        free(info);
    }
    return reward;
}

static room_ptr _temp_room;
static errr _parse_room(char *line, int options)
{
    assert(_temp_room);
    return parse_room_line(_temp_room, line, options);
}
room_ptr quest_get_map(quest_ptr q)
{
    if (q->file && (q->flags & QF_GENERATE))
    {
        room_ptr room = room_alloc(q->name);
        _temp_room = room;
        if (parse_edit_file(q->file, _parse_room, 0) != ERROR_SUCCESS)
        {
            room_free(room);
            room = NULL;
        }
        _temp_room = NULL;
        assert(room->type == ROOM_QUEST); /* common mistake ... T:QUEST:NORMAL */
        return room;
    }
    return NULL;
}

/************************************************************************
 * Quest Levels
 ***********************************************************************/
static void _remove_questors(quest_ptr q)
{
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(cave->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        mon->mflag2 &= ~MFLAG2_QUESTOR;
    }
    int_map_iter_free(iter);
}
static int _restore_questors(quest_ptr q)
{
    int ct = 0;
    int ct_remaining = q->goal_count - q->goal_current;
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(cave->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        if (mon->race->id == q->goal_idx && ct < ct_remaining)
        {
            mon->mflag2 |= MFLAG2_QUESTOR;
            ct++;
        }
    }
    int_map_iter_free(iter);
    return ct;
}
static void _place_questors(quest_ptr q)
{
    if (q->goal == QG_KILL_MON)
    {
        monster_race *r_ptr = mon_race_lookup(q->goal_idx);
        int           mode = PM_NO_KAGE | PM_NO_PET | PM_QUESTOR, i, j;
        int           ct = q->goal_count - q->goal_current;

        if (mon_race_is_dead_unique(r_ptr))
        {
            msg_print("It seems this level was guarded by someone before.");
            q->status = QS_FINISHED;
            return;
        }

        if (r_ptr->alloc.flags & RFA_ESCORT)
            mode |= PM_ALLOW_GROUP; /* allow escorts but not friends */
        
        for (i = _restore_questors(q); i < ct; i++)
        {
            for (j = 1000; j > 0; j--)
            {
                point_t pos = dun_random_mon_pos(cave, r_ptr);
                mon_ptr mon;

                if (!dun_pos_interior(cave, pos)) continue;

                /* Handle already allocated uniques by teleporting them to this level */
                if (mon_race_is_unique(r_ptr) && r_ptr->alloc.cur_num)
                {
                    mon = dun_mgr_relocate_unique(r_ptr->id, cave, pos);
                    if (!mon) /* paranoia */
                    {
                        msg_print("It seems this level was guarded by someone before.");
                        q->status = QS_FINISHED;
                        return;
                    }
                    mon->mflag2 |= MFLAG2_QUESTOR;
                    break;
                }

                /* Handle normal allocation */
                mon = place_monster_aux(who_create_null(), pos, r_ptr, mode);
                if (mon)
                {
                    mon->mflag2 |= MFLAG2_QUESTOR;
                    break;
                }
            }

            /* Failed to place */
            if (!j)
            {
                msg_print("It seems this level was guarded by someone before.");
                q->status = QS_FINISHED;
                return;
            }
        }
        if (ct == 1)
            cmsg_format(TERM_VIOLET, "Beware, this level is protected by %s!", r_ptr->name);
        else
        {
            char name[MAX_NLEN];
            strcpy(name, r_ptr->name);
            plural_aux(name);
            cmsg_format(TERM_VIOLET, "Be warned, this level is guarded by %d %s!", ct, name);
        }
    }
}

/************************************************************************
 * Quests (cf q_info.txt)
 ***********************************************************************/
static int_map_ptr _quests = NULL;
static int         _current = 0;

static errr _parse_q_info(char *line, int options)
{
    static quest_ptr quest = NULL;

    /* N:1:5:Thieves' Hideout */
    if (line[0] == 'N' && line[1] == ':')
    {
        char *zz[10];
        int   num = tokenize(line + 2, 10, zz, 0);

        if (num != 3 || !*zz[2])
        {
            msg_print("Error: Invalid N: line. Syntax: N:id:lvl:name.");
            return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        }

        quest = quest_alloc(zz[2]);
        quest->id = atoi(zz[0]);
        quest->level = atoi(zz[1]);
        int_map_add(_quests, quest->id, quest);
    }
    /* T:TOWN | GENERATE */
    else if (line[0] == 'T' && line[1] == ':')
    {
        char *flags[10];
        int   flag_ct = z_string_split(line + 2, flags, 10, "|");
        int   i;

        for (i = 0; i < flag_ct; i++)
        {
            char* flag = flags[i];

            if (streq(flag, "TOWN"))
                quest->flags |= QF_TOWN;
            else if (streq(flag, "GENERATE"))
                quest->flags |= QF_GENERATE;
            else if (streq(flag, "RETAKE"))
                quest->flags |= QF_RETAKE;
            else if (streq(flag, "RANDOM"))
                quest->flags |= QF_RANDOM;
            else if (streq(flag, "ANYWHERE"))
                quest->flags |= QF_ANYWHERE;
            else if (streq(flag, "NO_MSG"))
                quest->flags |= QF_NO_MSG;
            else
            {
                msg_format("Error: Invalid quest flag %s.", flag);
                return PARSE_ERROR_INVALID_FLAG;
            }
        }
    }
    /* F:q_thieves.txt */
    else if (line[0] == 'F' && line[1] == ':')
    {
        quest->file = _strcpy(line + 2);
    }
    /* G:KILL(logrus master)
     * G:KILL(archlich, 5)
     * G:FIND(^sting$) */
    else if (line[0] == 'G' && line[1] == ':')
    {
        char *name;
        char *args[10];
        int   arg_ct = parse_args(line + 2, &name, args, 10);

        if (arg_ct < 1)
        {
            msg_format("Error: Missing arguments to %s quest goal.", name);
            return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        }

        if (streq(name, "KILL"))
        {
            quest->goal = QG_KILL_MON;
            /* KILL(*) for random quests => goal is randomly determined (later) */
            if (strcmp(args[0], "*") == 0 && (quest->flags & QF_RANDOM))
                quest->goal_idx = 0; /* determined later */
            else
            {
                mon_race_ptr race = mon_race_parse(args[0]);
                if (!race)
                {
                    msg_format("Invalid monster: <color:r>%s</color>.", args[0]);
                    return PARSE_ERROR_INVALID_FLAG;
                }
                quest->goal_idx = race->id;
            }
            quest->goal_count = 1;
            if (arg_ct >= 2)
                quest->goal_count = atoi(args[1]);
        }
        else if (streq(name, "FIND"))
        {
            art_ptr art = arts_parse(args[0]);
            if (!art)
            {
                msg_format("Invalid artifact: <color:r>%s</color>.", args[0]);
                return PARSE_ERROR_INVALID_FLAG;
            }
            quest->goal = QG_FIND_ART;
            quest->goal_idx = art->id;
        }
        else
        {
            msg_format("Error: Unkown quest goal <color:r>%s</color>. Try KILL(name[, ct]) or FIND(art).", name);
            return PARSE_ERROR_INVALID_FLAG;
        }
    }
    /* W:Stronghold */
    else if (line[0] == 'W' && line[1] == ':')
    {
        quest->dungeon = dun_types_parse(line + 2);
        if (!quest->dungeon)
        {
            msg_format("Error: Unkown dungeon %s.", line + 2);
            return PARSE_ERROR_INVALID_FLAG;
        }
    }
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;
    return 0;
}

bool quests_init(void)
{
    assert(!_quests);
    _quests = int_map_alloc((int_map_free_f)quest_free);
    _current = 0;
    return !parse_edit_file("q_info.txt", _parse_q_info, 0); /* errr -> bool */
}

void quests_add(quest_ptr q)
{
    assert(q);
    assert(q->id);
    int_map_add(_quests, q->id, q);
}

void quests_cleanup(void)
{
    vec_ptr v;
    int     i;

    assert(_quests);
    _current = 0; /* XXX died in a quest */

    /* remove RFX_QUESTOR from previously assigned random quests
     * This is no longer necessary for normal games, as player_wipe
     * will clear flagsx. However, the quests_wizard() has an option
     * to re-roll the random questors ... */
    v = quests_get_random();
    for (i = 0; i < vec_length(v); i++)
    {
        quest_ptr q = vec_get(v, i);
        if (q->goal == QG_KILL_MON && q->goal_idx)
            mon_race_lookup(q->goal_idx)->flagsx &= ~RFX_QUESTOR;
    }
    vec_free(v);

    int_map_free(_quests);
    _quests = NULL;
    _current = 0;
}

quest_ptr quests_get_current(void)
{
    if (!_current) return NULL;
    return int_map_find(_quests, _current);
}

quest_ptr quests_get(int id)
{
    return int_map_find(_quests, id);
}

quest_ptr quests_parse(cptr name)
{
    quest_ptr result = NULL;
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(_quests);
            int_map_iter_is_valid(iter) && !result;
            int_map_iter_next(iter))
    {
        quest_ptr quest = int_map_iter_current(iter);
        if (strcmp(name, quest->name) == 0) result = quest;
    }
    int_map_iter_free(iter);
    return result;
}

cptr quests_get_name(int id)
{
    quest_ptr q = quests_get(id);
    if (!q) return "";
    return q->name;
}

static int _quest_cmp_level(quest_ptr l, quest_ptr r)
{
    if (l->level < r->level) return -1;
    if (l->level > r->level) return 1;
    if (l->completed_lev < r->completed_lev) return -1;
    if (l->completed_lev > r->completed_lev) return 1;
    if (l->id < r->id) return -1;
    if (l->id > r->id) return 1;
    return 0;
}

static vec_ptr _quests_get(quest_p p)
{
    vec_ptr v = vec_alloc(NULL);
    int_map_iter_ptr i;

    for (i = int_map_iter_alloc(_quests);
            int_map_iter_is_valid(i);
            int_map_iter_next(i))
    {
        quest_ptr q = int_map_iter_current(i);
        if (!p || p(q))
            vec_add(v, q);
    }
    int_map_iter_free(i);

    vec_sort(v, (vec_cmp_f)_quest_cmp_level);
    return v;
}


static bool _is_active(quest_ptr q) { return q->status == QS_TAKEN || q->status == QS_IN_PROGRESS || q->status == QS_COMPLETED; }
static bool _is_finished(quest_ptr q) { return q->status == QS_FINISHED; }
static bool _is_failed(quest_ptr q) { return q->status == QS_FAILED || q->status == QS_FAILED_DONE; }
static bool _is_hidden(quest_ptr q) { return (q->flags & QF_RANDOM) && q->status == QS_UNTAKEN; }
static bool _is_random(quest_ptr q) { return BOOL(q->flags & QF_RANDOM); }

vec_ptr quests_get_all(void) { return _quests_get(NULL); }
vec_ptr quests_get_active(void) { return _quests_get(_is_active); }
vec_ptr quests_get_finished(void) { return _quests_get(_is_finished); }
vec_ptr quests_get_failed(void) { return _quests_get(_is_failed); }
vec_ptr quests_get_hidden(void) { return _quests_get(_is_hidden); }
vec_ptr quests_get_random(void) { return _quests_get(_is_random); }
typedef vec_ptr (*quests_get_f)(void);

/************************************************************************
 * Quests: Randomize on Birth (from birth.c with slight mods)
 ***********************************************************************/
static bool _r_can_quest(mon_race_ptr race)
{
    if (race->alloc.flags & RFA_WILD_ONLY) return FALSE;
    if (mon_race_is_aquatic(race)) return FALSE;
    if (mon_race_can_multiply(race)) return FALSE;
    if (mon_race_is_friendly(race)) return FALSE;
    return TRUE;
}

static bool _r_is_nonunique(mon_race_ptr race)
{
    if (mon_race_is_unique(race)) return FALSE;
    if (mon_race_is_nazgul(race)) return FALSE;
    return TRUE;
}
static void _get_questor(quest_ptr q)
{
    int  attempt;

    /* High Level quests are stacked with uniques. Everything else
       is stacked the other way. So lets make some attempt at balance.
       Of course, users can force all quests to be for uniques, in
       true Hengband spirit. */
    mon_alloc_clear_filters();
    if (quest_unique || one_in_(3))
    {
        mon_alloc_push_filter(_r_can_quest);
        mon_alloc_push_filter(mon_race_is_unique);
    }
    else if (one_in_(2))
    {
        mon_alloc_push_filter(_r_can_quest);
        mon_alloc_push_filter(_r_is_nonunique);
    }
    else
        mon_alloc_push_filter(_r_can_quest);

    for (attempt = 0;; attempt++)
    {
        mon_race_ptr race;
        int min_lev = q->level + 1;
        int max_lev = q->level + 9;
        int mon_lev;
        if (q->level < 10)
            max_lev -= 2;
        else if (q->level < 20)
            max_lev -= 1;
        else if (q->level > 80)
            max_lev += 2;
        else if (q->level > 70)
            max_lev += 1;
        mon_lev = (min_lev + max_lev + 1) / 2;
        mon_lev += randint0(max_lev - mon_lev + 1);

        race = mon_alloc_choose_aux2(mon_alloc_tbl, mon_lev, min_lev, GMN_QUESTOR);

        if (race->flagsx & RFX_QUESTOR) continue;
        if (race->flagsx & RFX_GUARDIAN) continue;
        if (race->alloc.flags & RFA_NO_QUEST) continue;
        if (race->alloc.rarity > 100) continue;
        if (mon_race_is_friendly(race)) continue;
        if (mon_race_is_aquatic(race)) continue;
        if (race->alloc.flags & RFA_WILD_ONLY) continue;
        if (race->alloc.lvl > max_lev) continue;
        if (race->alloc.lvl > min_lev || attempt > 5000)
        {
            q->goal_idx = race->id;
            if (mon_race_is_unique(race))
            {
                race->flagsx |= RFX_QUESTOR;
                q->goal_count = 1;
            }
            else
                q->goal_count = rand_range(10, 20);
            q->goal_current = 0;
            break;
        }
    }
    mon_alloc_clear_filters();
}

void quests_on_birth(void)
{
    vec_ptr v;
    int i, last = 0;

    /* stale data from the last character */
    quests_cleanup();
    quests_init();

    /* assign random quests */
    /* XXX random quests are all in D_AMBER; note: using world_id rather
     * than initial_world_id allows me to do some tricky stuff in wizard mode. */
    if (plr->world_id != W_AMBER) return;

    v = quests_get_random();
    for (i = 0; i < vec_length(v); i++)
    {
        quest_ptr q = vec_get(v, i);
        int       spread = MIN(8, MAX(3, q->level/10));
        int       lvl, attempt = 0;

        if (q->dungeon && !last)
        {
            dun_type_ptr dt = dun_types_lookup(q->dungeon);
            last = dt->min_dun_lvl;
        }

        assert(q->level + spread > last);
        do
        {
            lvl = rand_range(q->level - spread, q->level + spread);
            ++attempt;
        } while ((lvl <= last || lvl > 98)  && attempt < 1000);
        last = lvl;
        q->level = lvl;

        if (q->goal == QG_KILL_MON)
        {
            if (q->goal_idx)
                mon_race_lookup(q->goal_idx)->flagsx &= ~RFX_QUESTOR;
            _get_questor(q);
        }
    }
    vec_free(v);
}

/************************************************************************
 * Quests: Hooks
 ***********************************************************************/
static bool _find_quest_p(quest_ptr q) { return q->dungeon && q->status < QS_COMPLETED; }
quest_ptr quests_find_quest(int dungeon, int level)
{
    int       i;
    vec_ptr   v = _quests_get(_find_quest_p);
    quest_ptr result = NULL;

    for (i = 0; i < vec_length(v); i++)
    {
        quest_ptr q = vec_get(v, i);
        int       d = q->dungeon;

        if (d != dungeon) continue;
        if (q->level != level) continue;
        if ((q->flags & QF_TOWN) && q->status == QS_UNTAKEN) continue;
        if (q->goal == QG_KILL_MON && !q->goal_idx) continue;

        result = q;
        break;
    }
    vec_free(v);
    return result;
}

void quests_on_enter(int dungeon, int level)
{
    quest_ptr q = quests_find_quest(dungeon, level);
    if (!q) return;
    if (q->status == QS_UNTAKEN && q->flags & QF_RANDOM) q->status = QS_TAKEN;
    if (q->status == QS_TAKEN)
    {
        _current = q->id;
        q->status = QS_IN_PROGRESS;
        _place_questors(q);
    }
}
void quests_on_enter_fixed(int quest_id)
{
    quest_ptr quest = quests_get(quest_id);
    assert(quest->status == QS_TAKEN);
    _current = quest->id;
    quest->status = QS_IN_PROGRESS;
}
void quests_on_kill_mon(mon_ptr mon)
{
    quest_ptr q;
    if (!_current) return;
    q = quests_get(_current);
    assert(q);
    assert(mon);
    /* handle monsters summoned *after* the quest was completed ... */
    if (q->status == QS_COMPLETED) return;

    if (q->goal == QG_KILL_MON && mon->race->id == q->goal_idx)
    {
        q->goal_current++;
        if (q->goal_current >= q->goal_count)
            quest_complete(q, mon->pos);
    }
    else if (q->goal == QG_CLEAR_LEVEL)
    {
        int_map_iter_ptr iter;
        bool done = TRUE;
        if (!mon_is_hostile(mon)) return;
        for (iter = int_map_iter_alloc(cave->mon);
                int_map_iter_is_valid(iter) && done;
                int_map_iter_next(iter))
        {
            mon_ptr m = int_map_iter_current(iter);
            if (m == mon) continue;
            if (mon_is_hostile(m)) done = FALSE;
        }
        int_map_iter_free(iter);
        if (done)
            quest_complete(q, mon->pos);
    }
}

void quests_on_get_obj(obj_ptr obj)
{
    quest_ptr q;
    if (!_current) return;
    q = quests_get(_current);
    assert(q);
    assert(obj);
    if ( q->goal == QG_FIND_ART
      && (obj->art_id == q->goal_idx || obj->replacement_art_id == q->goal_idx)
      && q->status == QS_IN_PROGRESS )
    {
        quest_complete(q, plr->pos);
    }
}

bool quests_check_leave(void)
{
    quest_ptr q;
    if (!_current) return TRUE;
    q = quests_get(_current);
    assert(q);
    if (q->status == QS_IN_PROGRESS)
    {
        if (q->flags & QF_RETAKE)
        {
            char       c;
            str_ptr s = str_alloc();

            str_append_s(s, "<color:r>Warning,</color> you are about to leave the quest: <color:R>");
            if ((q->flags & QF_RANDOM) && q->goal == QG_KILL_MON)
                str_printf(s, "Kill %s", mon_race_lookup(q->goal_idx)->name);
            else
                str_append_s(s, q->name);
            str_append_s(s, "</color>. You may return to this quest later though. "
                               "Are you sure you want to leave? <color:y>[Y,n]</color>");
            c = msg_prompt(str_buffer(s), "ny", PROMPT_YES_NO);
            str_free(s);
            if (c == 'n') return FALSE;
        }
        else
        {
            char       c;
            str_ptr s = str_alloc();

            str_append_s(s, "<color:r>Warning,</color> you are about to leave the quest: <color:R>");
            if ((q->flags & QF_RANDOM) && q->goal == QG_KILL_MON)
                str_printf(s, "Kill %s", mon_race_lookup(q->goal_idx)->name);
            else
                str_append_s(s, q->name);
            str_append_s(s, "</color>. <color:v>You will fail this quest if you leave!</color> "
                               "Are you sure you want to leave? <color:y>[Y,n]</color>");
            c = msg_prompt(str_buffer(s), "nY", PROMPT_NEW_LINE | PROMPT_ESCAPE_DEFAULT | PROMPT_CASE_SENSITIVE);
            str_free(s);
            if (c == 'n') return FALSE;
        }
    }
    return TRUE;
}
void quests_on_leave(void)
{
    quest_ptr q;
    if (!_current) return;
    q = quests_get(_current);
    assert(q);
    if (q->status == QS_IN_PROGRESS)
    {
        bool fail = TRUE;
        if (!statistics_hack && (q->flags & QF_RETAKE))
        {
            fail = FALSE;
            if (q->flags & QF_RANDOM)
            {
                cptr p = "If you like, you may choose to intentionally fail this quest. "
                    "Choose this option if you feel that you really cannot handle this opponent or "
                    "would rather not wait until you can. <color:v>Fail this quest?</color> "
                    "<color:y>[Y,n]</color>";
                char c = msg_prompt(p, "nY", PROMPT_NEW_LINE | PROMPT_ESCAPE_DEFAULT | PROMPT_CASE_SENSITIVE);
                if (c == 'Y') fail = TRUE;
            }
        }
        if (fail)
        {
            quest_fail(q);
            if (q->goal == QG_KILL_MON)
                mon_race_lookup(q->goal_idx)->flagsx &= ~RFX_QUESTOR;
        }
        else
        {
            q->status = QS_TAKEN;
            _remove_questors(q);
        }
    }
    _current = 0;
}

bool quests_allow_downstairs(void)
{
    return !_current;
}

bool quests_allow_downshaft(void)
{
    quest_ptr q;
    if (_current) return FALSE;
    q = quests_find_quest(cave->type->id, cave->dun_lvl + 1);
    if (q) return FALSE;
    return TRUE;
}

bool quests_allow_all_spells(void)
{
    quest_ptr q;
    if (!_current) return TRUE;
    q = quests_get(_current);
    assert(q);
    return !(q->flags & QF_GENERATE);
}

bool quests_allow_feeling(void)
{
    quest_ptr q;
    if (!_current) return TRUE;
    q = quests_get(_current);
    assert(q);
    return !(q->flags & QF_GENERATE);
}

/************************************************************************
 * Quests: Display
 ***********************************************************************/
void quests_display(void)
{
    doc_ptr       doc = doc_alloc(80);
    vec_ptr       v = quests_get_active();
    dun_world_ptr world = dun_worlds_current();
    int           i;

    if (world->final_guardian)
    {
        doc_printf(doc, "  <color:R>%s</color>\n", world->name);
        if (world->plr_flags & WFP_COMPLETED)
        {
            dun_type_ptr type = dun_types_lookup(world->final_dungeon);
            if (world->next_world_id)
            {
                doc_printf(doc, "    <indent>You have completed this world. A magical portal "
                                "awaits you on level %d of <color:U>%s</color> when you are "
                                "ready to continue your quest.</indent>\n\n", type->max_dun_lvl, type->name);
            }
            else
            {
                doc_insert(doc, "    <indent>You have won the game! You may retire (commit suicide) when ready.</indent>\n\n");
            }
        }
        else
            doc_printf(doc, "    <indent>%s</indent>\n\n", world->desc);
    }
    if (vec_length(v))
    {
        doc_printf(doc, "  <color:B>Current Quests</color>\n");
        for (i = 0; i < vec_length(v); i++)
        {
            quest_ptr q = vec_get(v, i);

            /* Quest Name and Status */
            doc_printf(doc, "  <color:%c>%s</color> (Lvl <color:U>%d</color>)\n",
                (q->status == QS_IN_PROGRESS) ? 'y' : 'R', q->name, q->level);

            /* Description. However, the QS_COMPLETED description is the 'reward' message */
            if (q->status == QS_COMPLETED)
                doc_insert(doc, "    Quest Completed (Unrewarded)");
            else
            {
                if (q->goal == QG_KILL_MON)
                {
                    monster_race *r_ptr = mon_race_lookup(q->goal_idx);
                    if (q->goal_count > 1)
                    {
                        char name[MAX_NLEN];
                        strcpy(name, r_ptr->name);
                        plural_aux(name);
                        doc_printf(doc, "    <indent>Kill %d %s (%d killed so far)</indent>\n",
                            q->goal_count, name, q->goal_current);
                    }
                    else if (q->flags & QF_RANDOM)
                        doc_printf(doc, "    Kill %s\n", r_ptr->name);
                }
                if (!(q->flags & QF_RANDOM))
                {
                    str_ptr s = quest_get_description(q);
                    doc_printf(doc, "    <indent>%s</indent>", str_buffer(s));
                    str_free(s);
                }
            }
            doc_newline(doc);
        }
        doc_newline(doc);
    }
    vec_free(v);

    quests_doc(doc);

    screen_save();
    doc_display(doc, "Quests", 0);
    screen_load();
    doc_free(doc);
}

static cptr _safe_r_name(int id)
{
    mon_race_ptr r = mon_race_lookup(id);
    if (!r || !r->name)
        return "Monster Removed";
    return r->name;
}
static void quest_doc(quest_ptr q, doc_ptr doc)
{
    if (q->flags & QF_RANDOM)
    {
        doc_insert(doc, "<color:U>");
        if (q->goal == QG_KILL_MON)
        {
            if (q->completed_lev == 0)
            {
                doc_printf(doc, "  Kill %s <tab:60>DL%3d <color:B>Cancelled</color>",
                    _safe_r_name(q->goal_idx), q->level);
            }
            else if (q->goal_count > 1)
            {
                char name[MAX_NLEN];
                strcpy(name, _safe_r_name(q->goal_idx));
                plural_aux(name);
                doc_printf(doc, "  Kill %d %s (%d killed) <tab:60>DL%3d CL%2d",
                    q->goal_count, name, q->goal_current, q->level, q->completed_lev);
            }
            else
            {
                doc_printf(doc, "  Kill %s <tab:60>DL%3d CL%2d",
                    _safe_r_name(q->goal_idx), q->level, q->completed_lev);
            }
        }
        doc_insert(doc, "</color>\n");
    }
    else
    {
        doc_printf(doc, "  %s <tab:60>DL%3d CL%2d\n",
            q->name, q->level, q->completed_lev);
    }
}

void quests_doc(doc_ptr doc)
{
    int     i;
    vec_ptr v = quests_get_finished();

    doc_insert(doc, "<style:table>");
    if (vec_length(v))
    {
        doc_printf(doc, "  <color:G>Completed Quests</color>\n");
        for (i = 0; i < vec_length(v); i++)
        {
            quest_ptr quest = vec_get(v, i);
            quest_doc(quest, doc);
        }
    }
    vec_free(v);

    v = quests_get_failed();
    if (vec_length(v))
    {
        doc_printf(doc, "\n  <color:r>Failed Quests</color>\n");
        for (i = 0; i < vec_length(v); i++)
        {
            quest_ptr quest = vec_get(v, i);
            quest_doc(quest, doc);
        }
    }
    vec_free(v);
    doc_newline(doc);
    doc_insert(doc, "</style>");
}

/************************************************************************
 * Quests: Savefiles
 ***********************************************************************/
void quests_load(savefile_ptr file)
{
    int ct = savefile_read_s16b(file);
    int i;
    for (i = 0; i < ct; i++)
    {
        int       id = savefile_read_s16b(file);
        quest_ptr q = quests_get(id);

        assert(q);
        q->status = savefile_read_byte(file);
        q->completed_lev = savefile_read_byte(file);
        q->goal_current = savefile_read_s16b(file);
        if (q->flags & QF_RANDOM)
        {
            q->level = savefile_read_s16b(file);
            q->goal_idx  = savefile_read_sym(file);
            q->goal_count = savefile_read_s16b(file);
            q->seed  = savefile_read_u32b(file);
        }

        if (q->goal == QG_FIND_ART)
            arts_lookup(q->goal_idx)->gen_flags |= OFG_QUESTITEM;
    }
    _current = savefile_read_s16b(file);
    if (plr->is_dead)
        _current = 0;
}

void quests_save(savefile_ptr file)
{
    int i;
    vec_ptr v = quests_get_all();
    savefile_write_s16b(file, vec_length(v));
    for (i = 0; i < vec_length(v); i++)
    {
        quest_ptr q = vec_get(v, i);
        savefile_write_s16b(file, q->id);
        savefile_write_byte(file, q->status);
        savefile_write_byte(file, q->completed_lev);
        savefile_write_s16b(file, q->goal_current);
        if (q->flags & QF_RANDOM)
        {
            savefile_write_s16b(file, q->level); /* in case I randomize later ... */
            savefile_write_sym(file, q->goal_idx);
            savefile_write_s16b(file, q->goal_count);
            savefile_write_u32b(file, q->seed);
        }
    }
    savefile_write_s16b(file, _current);
    vec_free(v);
}

/************************************************************************
 * Quests: Wizard Utilities (^Aq)
 * This is a lot of work, but it really helps me test things.
 ***********************************************************************/
struct _ui_context_s
{
    vec_ptr quests;
    int     top;
    int     page_size;
    doc_ptr doc;
};
typedef struct _ui_context_s _ui_context_t, *_ui_context_ptr;

static void _display_menu(_ui_context_ptr context);
static void _map_cmd(_ui_context_ptr context);
static void _status_cmd(_ui_context_ptr context, int status);
static void _reward_cmd(_ui_context_ptr context);
static void _analyze_cmd(_ui_context_ptr context);
static void _display_map(room_ptr room);

void quests_wizard(void)
{
    _ui_context_t context = {0};
    quests_get_f  qgf = quests_get_all; /* remember last filter after 'R' command */
    context.quests = qgf();

    character_icky = TRUE;

    msg_line_clear();
    msg_line_init(ui_shop_msg_rect());

    Term_clear();
    context.doc = doc_alloc(MIN(80, ui_shop_rect().cx));
    for (;;)
    {
        int    max = vec_length(context.quests) - 1;
        rect_t r = ui_shop_rect(); /* recalculate in case resize */
        int    cmd;

        context.page_size = MIN(26, r.cy - 7);
        if (context.top % context.page_size != 0) /* resize?? */
            context.top = 0;

        _display_menu(&context);

        cmd = inkey_special(TRUE);
        msg_line_clear();
        msg_boundary();
        if (cmd == ESCAPE || cmd == 'q' || cmd == 'Q') break;
        switch (cmd)
        {
        case 'm': _map_cmd(&context); break;
        case 'c': _status_cmd(&context, QS_COMPLETED); break;
        case 'f': _status_cmd(&context, QS_FAILED); break;
        case 'u': _status_cmd(&context, QS_UNTAKEN); break;
        case 't': _status_cmd(&context, QS_TAKEN); break;
        case '$': _reward_cmd(&context); break;
        case '?': _analyze_cmd(&context); break;
        case 'R':
            quests_on_birth();
            vec_free(context.quests);
            context.quests = qgf();
            break;
        case KTRL('R'):
            vec_free(context.quests);
            qgf = quests_get_random;
            context.quests = qgf();
            context.top = 0;
            break;
        case KTRL('A'):
            vec_free(context.quests);
            qgf = quests_get_active;
            context.quests = qgf();
            context.top = 0;
            break;
        case KTRL('C'):
            vec_free(context.quests);
            qgf = quests_get_finished;
            context.quests = qgf();
            context.top = 0;
            break;
        case KTRL('F'):
            vec_free(context.quests);
            qgf = quests_get_failed;
            context.quests = qgf();
            context.top = 0;
            break;
        case '*':
            vec_free(context.quests);
            qgf = quests_get_all;
            context.quests = qgf();
            context.top = 0;
            break;
        case KTRL('P'):
            do_cmd_messages(dun_mgr()->turn);
            break;
        case SKEY_PGDOWN: case '3':
            if (context.top + context.page_size - 1 < max)
                context.top += context.page_size;
            break;
        case SKEY_PGUP: case '9':
            if (context.top >= context.page_size)
                context.top -= context.page_size;
            break;
        default:
            if (cmd < 256 && isprint(cmd))
            {
                msg_format("Unrecognized command: <color:R>%c</color>. "
                           "Press <color:keypress>?</color> for help.", cmd);
            }
            else if (KTRL('A') <= cmd && cmd <= KTRL('Z'))
            {
                cmd |= 0x40;
                msg_format("Unrecognized command: <color:R>^%c</color>. "
                           "Press <color:keypress>?</color> for help.", cmd);
            }
        }
    }
    character_icky = FALSE;
    msg_line_clear();
    msg_line_init(ui_msg_rect());

    Term_clear();
    do_cmd_redraw();

    vec_free(context.quests);
    doc_free(context.doc);

    if (plr_in_town())
        dun_regen_town(cave); /* XXX we could track when this is required */
}

static cptr _status_name(int status)
{
    switch (status)
    {
    case QS_UNTAKEN: return "Untaken";
    case QS_TAKEN: return "Taken";
    case QS_IN_PROGRESS: return "In Progress";
    case QS_COMPLETED: return "Completed";
    case QS_FINISHED: return "Finished";
    case QS_FAILED: return "Failed";
    case QS_FAILED_DONE: return "FailedDone";
    }
    return "";
}
static char _status_color(int status)
{
    switch (status)
    {
    case QS_UNTAKEN: return 'D';
    case QS_IN_PROGRESS: return 'y';
    case QS_COMPLETED: case QS_FINISHED: return 'G';
    case QS_FAILED: case QS_FAILED_DONE: return 'r';
    }
    return 'w';
}
static char _quest_color(quest_ptr q)
{
    if (q->status < QS_IN_PROGRESS && (q->flags & QF_RANDOM) && q->goal == QG_KILL_MON)
        return 'B';
    return _status_color(q->status);
}
static void _display_menu(_ui_context_ptr context)
{
    rect_t   r = ui_shop_rect();
    doc_ptr  doc = context->doc;
    int      i;

    doc_clear(doc);
    doc_insert(doc, "<style:table>");

    doc_printf(doc, "   <color:U>%-40.40s %-10.10s Lvl</color>\n", "Quest", "Status");
    for (i = 0; i < context->page_size; i++)
    {
        int idx = context->top + i;
        if (idx < vec_length(context->quests))
        {
            quest_ptr quest = vec_get(context->quests, idx);
            doc_printf(doc, "%c) <color:%c>", I2A(i), _quest_color(quest));
            if ((quest->flags & QF_RANDOM) && quest->goal == QG_KILL_MON)
            {
                str_ptr s = str_alloc_format("%-34.34s", _safe_r_name(quest->goal_idx));
                if (quest->goal_count > 1)
                    str_printf(s, " (%d)", quest->goal_count);
                else
                    str_printf(s, " (L%d)", mon_race_lookup(quest->goal_idx)->alloc.lvl);
                doc_printf(doc, "%-40.40s ", str_buffer(s));
                str_free(s);
            }
            else
                doc_printf(doc, "%-40.40s ", quest->name);
            doc_printf(doc, "%-10.10s %3d</color>", _status_name(quest->status), quest->level);
            doc_newline(doc);
        }
        else
            doc_newline(doc);
    }
    doc_newline(doc);
    {
        int max = vec_length(context->quests) - 1;
        int bottom = context->top + context->page_size;

        if (context->top > 0 || bottom < max)
        {
            int page_count = (max - 1) / context->page_size + 1;
            int page_current = context->top / context->page_size + 1;

            doc_printf(doc, "<color:B>(Page %d of %d)</color>\n", page_current, page_count);
        }
    }

    doc_insert(doc,
        "<color:keypress>m</color> to display map. "
        "<color:keypress>R</color> to reset all quests and re-assign all random quests.\n"
        "<color:keypress>$</color> to see reward. "
        "<color:keypress>c</color> to set complete. "
        "<color:keypress>f</color> to set failed. "
        "<color:keypress>u</color> to set untaken.\n"
        "<color:keypress>?</color> to analyze quest file.\n");

    doc_insert(doc, "<color:keypress>Esc</color> to exit.</style>");

    Term_clear_rect(r);
    doc_sync_term(doc,
        doc_range_top_lines(doc, r.cy),
        doc_pos_create(r.x, r.y));
}

static void _map_cmd(_ui_context_ptr context)
{
    for (;;)
    {
        char cmd;
        int  idx;

        if (!msg_command("<color:y>View which quest map <color:w>(<color:keypress>Esc</color> to cancel)</color>?</color>", &cmd)) break;
        if (cmd == ESCAPE) break;
        if (cmd < 'a' || cmd > 'z') continue;
        idx = A2I(cmd);
        idx += context->top;
        if (idx < vec_length(context->quests))
        {
            quest_ptr quest = vec_get(context->quests, idx);
            room_ptr  map;
            if (!(quest->flags & QF_GENERATE) || !quest->file)
            {
                msg_format("The <color:R>%s</color> quest has no map.", quest->name);
                continue;
            }
            map = quest_get_map(quest);
            if (!map)
            {
                msg_format("Unable to load the <color:R>%s</color> quest map.", quest->name);
                continue;
            }
            _display_map(map);
            Term_clear();
            room_free(map);
            break;
        }
    }
}
static void _display_map(room_ptr room)
{
    int which = 0, x, y, cmd;
    int animate = 0;
    bool random = TRUE;

    msg_line_clear();
    for (;;)
    {
        transform_ptr xform;

        if (random)
            xform = transform_alloc_room(room, size_create(MAX_WID, MAX_HGT));
        else
            xform = transform_alloc(which, rect_create(0, 0, room->width, room->height));

        if (xform->dest.cx < Term->wid)
            xform->dest = rect_translate_xy(xform->dest, (Term->wid - xform->dest.cx)/2, 0);

        if (xform->dest.cy < Term->hgt)
            xform->dest = rect_translate_xy(xform->dest, 0, (Term->hgt - xform->dest.cy)/2);

        Term_clear();
        for (y = 0; y < room->height; y++)
        {
            cptr line = vec_get(room->map, y);
            for (x = 0; x < room->width; x++)
            {
                char letter = line[x];
                point_t p = transform_point(xform, point_create(x,y));
                if (0 <= p.x && p.x < Term->wid && 0 <= p.y && p.y < Term->hgt)
                {
                    room_grid_ptr grid = int_map_find(room->letters, letter);
                    int           r_idx = 0, k_idx = 0;
                    byte          a = TERM_WHITE;
                    char          c = letter;

                    if (grid && grid->scramble)
                        grid = int_map_find(room->letters, grid->scramble);

                    if (grid && mon_rule_is_valid(&grid->monster))
                    {
                        if (!(grid->monster.flags & (MON_RULE_CHAR | MON_RULE_RANDOM | MON_RULE_TYPE)))
                            r_idx = grid->monster.which;
                    }
                    if (grid && grid->object.object)
                    {
                        if (!(grid->object.flags & (OBJ_DROP_TYPE | OBJ_DROP_STD_ART | OBJ_DROP_RANDOM)))
                        {
                            k_idx = grid->object.object;
                        }
                    }
                    if (r_idx)
                    {
                        mon_race_ptr race = mon_race_lookup(r_idx);
                        term_char_t  tc = mon_race_visual(race);
                        a = tc.a;
                        c = tc.c;
                    }
                    else if (k_idx)
                    {
                        object_kind *k_ptr = &k_info[k_idx];
                        a = k_ptr->x_attr;
                        c = k_ptr->x_char;
                    }
                    else if (grid)
                    {
                    #if FEAT_BUGZ
                        feature_type *f_ptr = &f_info[grid->cave_feat ? grid->cave_feat : feat_floor];
                        if (f_ptr->mimic) f_ptr = &f_info[f_ptr->mimic];
                        a = f_ptr->x_attr[F_LIT_STANDARD];
                        c = f_ptr->x_char[F_LIT_STANDARD];
                    #endif
                    }
                    Term_putch(p.x, p.y, a, c);
                }
            }
            if (animate)
            {
                Term_fresh();
                Term_xtra(TERM_XTRA_DELAY, animate);
            }
        }
        transform_free(xform);
        cmd = inkey_special(FALSE);
        if (cmd == ESCAPE || cmd == 'q' || cmd == 'Q' || cmd == '\r') break;
        if ('0' <= cmd && cmd < '8') which = cmd - '0';
    }
    do_cmd_redraw();
}
static void _status_cmd(_ui_context_ptr context, int status)
{
    for (;;)
    {
        char cmd;
        int  idx;

        if (!msg_command(
            format("<color:y>Change status to <color:B>%s</color> for which quest <color:w>(<color:keypress>Esc</color> to cancel)</color>?</color>",
                _status_name(status)), &cmd)) break;
        if (cmd == ESCAPE) break;
        if (cmd < 'a' || cmd > 'z') continue;
        idx = A2I(cmd);
        idx += context->top;
        if (idx < vec_length(context->quests))
        {
            quest_ptr quest = vec_get(context->quests, idx);
            quest->status = status;
            if (quest->status >= QS_COMPLETED)
                quest->completed_lev = plr->lev;
            break;
        }
    }
}
static void _reward_cmd(_ui_context_ptr context)
{
    for (;;)
    {
        char cmd;
        int  idx;

        if (!msg_command("<color:y>View which quest reward <color:w>(<color:keypress>Esc</color> when done)</color>?</color>", &cmd)) break;
        if (cmd == ESCAPE) break;
        if (cmd < 'a' || cmd > 'z') continue;
        idx = A2I(cmd);
        idx += context->top;
        if (idx < vec_length(context->quests))
        {
            quest_ptr quest = vec_get(context->quests, idx);
            if (!(quest->flags & QF_TOWN))
            {
                int i, ct = quest->level/25 + 1;
                for (i = 0; i < ct; i++)
                {
                    obj_t forge = {0};
                    if (make_object(&forge, quest->level, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST | AM_NO_DROP))
                    {
                        char name[MAX_NLEN];
                        obj_identify_fully(&forge);
                        object_desc(name, &forge, OD_COLOR_CODED);
                        msg_boundary();
                        msg_format("%s", name);
                    }
                }
            }
            else
            {
                obj_ptr   reward;

                quest->seed = randint0(0x10000000);
                reward = quest_get_reward(quest, AM_NO_DROP);

                if (!reward)
                    msg_format("<color:R>%s</color> has no reward.", quest->name);
                else
                {
                    char name[MAX_NLEN];
                    obj_identify_fully(reward);
                    object_desc(name, reward, OD_COLOR_CODED);
                    msg_format("<color:R>%s</color> gives %s.", quest->name, name);
                    if (reward->art_id)
                        arts_lookup(reward->art_id)->generated = FALSE;
                    if (reward->replacement_art_id)
                        arts_lookup(reward->replacement_art_id)->generated = FALSE;
                    obj_free(reward);
                }
            }
        }
    }
}
static errr _parse_debug(char *line, int options)
{
    if (line[0] == 'R')
        return _parse_reward(line, options);
    return _parse_room(line, options);
}
static void _analyze_cmd(_ui_context_ptr context)
{
    for (;;)
    {
        char cmd;
        int  idx;

        if (!msg_command("<color:y>Analyze which quest file <color:w>(<color:keypress>Esc</color> to cancel)</color>?</color>", &cmd)) break;
        if (cmd == ESCAPE) break;
        if (cmd < 'a' || cmd > 'z') continue;
        idx = A2I(cmd);
        idx += context->top;
        if (idx < vec_length(context->quests))
        {
            quest_ptr quest = vec_get(context->quests, idx);
            if (!quest->file || !strlen(quest->file))
            {
                msg_format("<color:R>%s</color> has no quest file.", quest->name);
                continue;
            }
            /* very hackish ... but very useful */
            _temp_room = room_alloc(quest->name);
            _temp_reward = malloc(sizeof(obj_drop_t));
            memset(_temp_reward, 0, sizeof(obj_drop_t));
            trace_doc = context->doc;
            doc_clear(context->doc);
            parse_edit_file(quest->file, _parse_debug, INIT_DEBUG);
            room_free(_temp_room);
            free(_temp_reward);
            _temp_room = NULL;
            _temp_reward = NULL;
            trace_doc = NULL;
            Term_clear();
            doc_display(context->doc, quest->name, 0);
            Term_clear();
            break;
        }
    }
}

