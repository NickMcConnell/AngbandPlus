#include "angband.h"

#include <assert.h>

/*************************************************************************
 * Discrete Line Generator
 *************************************************************************/
#define _MAX_STRATEGIES 10
#define _MAX_N          21 /* s/b DUN_VIEW_MAX + 1, but see ../lib/edit/n_choose_k.txt */
struct n_choose_k_s {
    int n, k, ct;
    u32b mask[_MAX_STRATEGIES];
};

typedef struct {
    int n;
    n_choose_k_ptr tbl;
} n_choose_t, *n_choose_ptr;

static n_choose_t _n_choose_tbl[_MAX_N + 1];
static void _n_choose_init(void)
{
    int n, k;
    for (n = 1; n <= _MAX_N; n++)
    {
        n_choose_ptr entry = &_n_choose_tbl[n];
        entry->n = n;
        assert(!entry->tbl); /* duplicate initialization */
        entry->tbl = malloc((n+1)*sizeof(n_choose_k_t));
        for (k = 1; k <= n; k++)
        {
            entry->tbl[k].n = n;
            entry->tbl[k].k = k;
            entry->tbl[k].ct = 0;
        }
    }
}
static n_choose_k_ptr _n_choose_k_lookup(int n, int k)
{
    assert(1 <= n && n <= _MAX_N);
    assert(1 <= k && k <= n);
    return &_n_choose_tbl[n].tbl[k];
}
void dun_line_gen_create(dun_line_gen_ptr gen, line_t line)
{
    point_t lv = point_subtract(line.b, line.a);
    point_t a = point_abs(lv);
    point_t s = point_sign(lv);

    assert(line_is_valid(line));
    memset(gen, 0, sizeof(dun_line_gen_t));

    /* rescale the line if our N-choose-k discretization problem is out of range */
    if ( (a.x >= a.y && a.y >= _MAX_N) /* horizontal with too many segments */
      || (a.x < a.y && a.x >= _MAX_N) ) /* vertical with too many segments */
    {
        int n = point_norm(lv);
        lv = point_scale_Q(lv, DUN_VIEW_MAX, n);
        line.b = point_add(line.a, lv);
        /*msg_format("<color:v>(%d,%d)</color>", line.b.x - line.a.x, line.b.y - line.a.y);*/
        a = point_abs(lv);
        s = point_sign(lv);
    }
    gen->line = line;
    gen->step = s;
    if (a.x >= a.y) /* horizontal */
    {
        gen->horizontal = TRUE;
        gen->major_length = a.x;
        gen->minor_length = a.y;
    }
    else /* vertical */
    {
        gen->horizontal = FALSE;
        gen->major_length = a.y;
        gen->minor_length = a.x;
    }
    assert(gen->minor_length <= gen->major_length);

    gen->segment_count = gen->minor_length + 1;
    gen->base_segment_length = (gen->major_length + 1) / gen->segment_count;
    gen->segment_remainder = (gen->major_length + 1) % gen->segment_count;
    if (gen->segment_remainder)
    {
        gen->strategies = _n_choose_k_lookup(gen->segment_count, gen->segment_remainder);
        gen->current_strategy = 0;
        assert(gen->strategies->ct); /* ../lib/edit/n_choose_k.txt */
        gen->current_mask = gen->strategies->mask[0];
    }
    else
    {
        gen->strategies = NULL;
        gen->current_strategy = 0;
        gen->current_mask = 0;
    }
}
void dun_line_gen_destroy(dun_line_gen_ptr gen)
{
    /* cleanup if needed */
}
dun_line_gen_ptr dun_line_gen_alloc(line_t line)
{
    dun_line_gen_ptr gen = malloc(sizeof(dun_line_gen_t));
    dun_line_gen_create(gen, line);
    return gen;
}
void dun_line_gen_free(dun_line_gen_ptr gen)
{
    if (!gen) return;
    dun_line_gen_destroy(gen);
    free(gen);
}
bool dun_line_gen_next_strategy(dun_line_gen_ptr gen)
{
    if (!gen->strategies) return FALSE;
    if (++gen->current_strategy >= gen->strategies->ct) return FALSE;
    gen->current_mask = gen->strategies->mask[gen->current_strategy];
    return TRUE;
}
point_t dun_line_gen_first(dun_line_gen_ptr gen)
{
    gen->current_point = gen->line.a;
    gen->current_segment = 0;
    gen->current_segment_length = gen->base_segment_length;
    if (gen->current_mask & 01)
        gen->current_segment_length++;
    gen->segment_step = 0;
    gen->major_count = 0;
    gen->minor_count = 0;

    return gen->current_point;
}
static void _next_segment(dun_line_gen_ptr gen)
{
    gen->current_segment++;
    if (gen->current_segment == gen->segment_count)
        gen->current_segment = 0; /* wrap */
    gen->current_segment_length = gen->base_segment_length;
    if (gen->current_mask & (1U << gen->current_segment))
        gen->current_segment_length++;
    gen->segment_step = 0;
}
point_t dun_line_gen_next(dun_line_gen_ptr gen)
{
    if (gen->horizontal)
    {
        gen->current_point.x += gen->step.x;
        gen->major_count++;
        gen->segment_step++;
        if (gen->segment_step == gen->current_segment_length)
        {
            if (gen->step.y)
            {
                gen->current_point.y += gen->step.y;
                gen->minor_count++;
            }
            _next_segment(gen);
        }
    }
    else
    {
        gen->current_point.y += gen->step.y;
        gen->major_count++;
        gen->segment_step++;
        if (gen->segment_step == gen->current_segment_length)
        {
            if (gen->step.x)
            {
                gen->current_point.x += gen->step.x;
                gen->minor_count++;
            }
            _next_segment(gen);
        }
    }
    return gen->current_point;
}
int dun_line_gen_distance(dun_line_gen_ptr gen)
{
    return gen->major_count + (gen->minor_count >> 1);
}
point_vec_ptr dun_line_points(line_t l)
{
    point_vec_ptr pv = point_vec_alloc();
    if (line_is_valid(l))
    {
        dun_line_gen_t gen;
        dun_line_gen_create(&gen, l);
        point_vec_push(pv, dun_line_gen_first(&gen));
        for (;;)
        {
            point_t p = dun_line_gen_next(&gen);
            point_vec_push(pv, p);
            /*if (point_equals(p, l.b)) break;*/
            if (dun_line_gen_distance(&gen) >= DUN_VIEW_MAX) break;
        }
        dun_line_gen_destroy(&gen);
    }
    return pv;
}

/*      v----buf
 * k:11: 10110 10101101 01101 | 10110 10110101 01101
 */
static errr _parse_n_choose_k(n_choose_k_ptr entry, char *buf)
{
    char *tokens[10];
    int   token_ct = z_string_split(buf, tokens, 10, "|");
    int   i, n, k;
    for (i = 0; i < token_ct; i++)
    {
        char *cp = tokens[i];
        u32b  mask = 0;
        if (entry->ct >= _MAX_STRATEGIES)
        {
            msg_format("<color:v>Error</color>: Invalid mask token '<color:U>%s</color>'. Only %d strategies are supported.", tokens[i], _MAX_STRATEGIES);
            return PARSE_ERROR_INVALID_FLAG;
        }
        /* masks encode choice functions, not binary numbers. so '01' means choose 2 not binary 0x1
         * while '10' means choose 1 not binary 0x2. Of course, we encode the choice functions as
         * binary numbers, so '01' means choose 2 and gets encoded as 0x00000002. In other words, 
         * read the masks from left to right to infer the choices.
         * n tracks the current choice position and k tracks the number of positive choices made.
         * At the end, they should match entry->n and entry->k. */
        for (n = 0, k = 0; *cp; cp++)
        {
            if (*cp == ' ') continue;
            if (*cp != '1' && *cp != '0')
            {
                msg_format("<color:v>Error</color>: Invalid mask token '<color:U>%s</color>'. Enter binary digits with optional spaces.", tokens[i]);
                return PARSE_ERROR_INVALID_FLAG;
            }
            if (n >= 32)
            {
                msg_format("<color:v>Error</color>: Invalid mask token '<color:U>%s</color>'. Only 32-bits are allowed.", tokens[i]);
                return PARSE_ERROR_INVALID_FLAG;
            }
            if (*cp == '1') 
            { 
                mask |= (1U << n);
                k++; 
            }
            n++;
        }
        if (n != entry->n)
        {
            msg_format("<color:v>Error</color>: Invalid mask token '<color:U>%s</color>'. Need to set %d choices for %d choose %d.", tokens[i], entry->n, entry->n, entry->k);
            return PARSE_ERROR_INVALID_FLAG;
        }
        if (k != entry->k)
        {
            msg_format("<color:v>Error</color>: Invalid mask token '<color:U>%s</color>'. Need to set %d bits.", tokens[i], entry->k);
            return PARSE_ERROR_INVALID_FLAG;
        }
        entry->mask[entry->ct++] = mask; /* support multiple k: lines per entry */
    }
    return 0;
}

static errr _parser(char *buf, int options)
{
    static n_choose_ptr entry = NULL;
    if (buf[0] == 'N')
    {
        char *zz[10];
        int   num = tokenize(buf + 2, 10, zz, 0);
        int   n;

        if (num != 1 || !*zz[0])
        {
            msg_print("Error: Invalid N: line. Syntax: N:<id>.");
            return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        }
        n = atoi(zz[0]);
        if (n < 1 || n > _MAX_N)
        {
            msg_format("<color:v>Error</color>: Invalid N of %d. Must be in 1 to %d, inclusive.", n, _MAX_N);
            return PARSE_ERROR_OUT_OF_BOUNDS;
        }
        entry = &_n_choose_tbl[n];
    }
    else if (!entry)
    {
        msg_print("Error: Missing N: line for new N choose k table.");
        return PARSE_ERROR_MISSING_RECORD_HEADER;
    }
    else if (buf[0] == 'k')
    {
        char *tokens[10];
        int   token_ct = z_string_split(buf + 2, tokens, 10, ":");
        int   k;

        if (token_ct != 2 || !*tokens[0])
        {
            msg_print("Error: Invalid k: line. Syntax: k:<id>: <mask>");
            return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        }
        k = atoi(tokens[0]);
        if (k < 1 || k > entry->n)
        {
            msg_format("<color:v>Error</color>: Invalid k= %d for N=%d. Enter a number between 1 and %d inclusive.", k, entry->n, entry->n);
            return PARSE_ERROR_OUT_OF_BOUNDS;
        }
        return _parse_n_choose_k(&entry->tbl[k], tokens[1]);
    }
    return 0;
}

errr parse_n_choose_k(void)
{
    _n_choose_init(); /* once! cf init_angband */
    return parse_edit_file("n_choose_k.txt", _parser, 0);
}

/*************************************************************************
 * dun_los
 *************************************************************************/
static bool _project_aux(dun_ptr dun, dun_line_gen_ptr gen, u32b flags, int range)
{
    bool    success = FALSE;
    point_t pos;

    dun_line_gen_first(gen); /* skip initial point */
    while (!success)
    {
        pos = dun_line_gen_next(gen);

        if (!dun_pos_interior(dun, pos)) break;

        if (point_equals(pos, gen->line.b))
            success = TRUE;

        if (dun_line_gen_distance(gen) >= range) break;
        if (dun_stop_project(dun, pos, gen->line.b, flags)) break;
    }
    return success;
}
static bool _project(dun_ptr dun, point_t start, point_t stop, u32b flags, int range)
{
    line_t         l = line_create(start, stop);
    dun_line_gen_t gen;
    int            d = point_fast_distance(start, stop);
    bool           b;

    if (!dun_pos_interior(dun, start)) return FALSE;
    if (d > range) return FALSE;
    if (d < 2) return TRUE;

    dun_line_gen_create(&gen, l);
    for (;;)
    {
        b = _project_aux(dun, &gen, flags, range);
        if (b) break;
        if (!dun_line_gen_next_strategy(&gen)) break;
    }
    dun_line_gen_destroy(&gen);
    return b;
}
bool dun_los(dun_ptr dun, point_t p1, point_t p2)
{
    return _project(dun, p1, p2, PROJECT_LOS, DUN_VIEW_MAX);
}
bool dun_in_disintegration_range(dun_ptr dun, point_t p1, point_t p2)
{
    return _project(dun, p1, p2, PROJECT_DISI, DUN_VIEW_MAX);
}
/*************************************************************************
 * dun_update_view
 *
 * plr_view() is the set of currently "viewable" grids and is used for 
 * redrawing, torch lighting and los. This (along with dun_update_light)
 * is one of the major performance bottlenecks.
 *
 * How to define "view"? We could say V = {b|dun_los(plr->pos, b)}, but
 * this has weirdness in the discrete block-world of angband. For instance,
 * dun_los(a,b) is true if a line segment (projection) from a to b actually
 * reaches b, respecting terrain FF_LOS of course. But the weird thing is 
 * that for a point c on the projection from a to b, we would expect
 * dun_los(a,c) to hold: it does not (in general).
 *
 * In a certain sense, dun_update_view *defines* what the player can see.
 * This gives us some lattitude, but we expect the following:
 *
 * [1] View should contain as many dun_los points as possible.
 * [2] View should be a connected set.
 * [3] Any missed dun_los points (and these are rare) should be boundary
 *     points. It is OK for the plr to have a blind spot once in a while.
 * [4] View should contain non-los points for walls. For example, sighting
 *     down a long straight corridor.
 *
 * The RAYCAST algorithm is "official". The NAIVE algorithm is just for 
 * reference and testing. Performance is very good in dungeons (view sets
 * 200 to 300) and acceptable on D_SURFACE (view sets 600-900). The old
 * "update_view" was replaced since it was very poor wrt [1] above, often
 * missing up to 25% of projectable los points when entering rooms.
 *
 * The RAYCAST algorithm defines the view space using a small number of line
 * projections. Each ray cast is an optimal projection, and casts sweep around
 * the plr in a circle, adding each point on the cast to the current view.
 * Easy to understand, though finding the next optimal raycast is hard.
 * In the worst case, each ray is cast to the boundary of the view box
 * (cf _view->rect) giving about 160 raycasts. In practice though, 50 to 60
 * raycasts are required and in corridors, perhaps only 20 to 30. Testing each
 * point in the view box would require over 1600 raycasts.
 *
 *************************************************************************/
/* Debug level 0 - Off
 *             1 - Basic Diagnostics
 *             2 - Verbose XXX Thinking about showing each raycast XXX
 *
 * Note: debugging re-generates "view.html" in your user dir for every dun_update_view */
#define _VIEW_DEBUG 0

/* RAYCAST | NAIVE
 * RAYCAST is faster and gives better wall semantics, but sometimes misses boundary
 *   projectable points. This is *much* faster in normal dungeons. _sweep is rather
 *   complicated.
 * NAIVE never misses projectable points, but is rather slow and has poor wall semantics.
 *   Wall handling could be improved, but it would get much slower.
 *
 * On D_SURFACE with large view-sets (say ~900avg) then performance is comparable.
 * Try the wizard command ^AL (dun_wizard_view) and use 't' to toggle between engines!
 */
enum { _VIEW_RAYCAST, _VIEW_NAIVE };
static int _engine = _VIEW_RAYCAST;

static dun_bmp_ptr _touched;    /* _VIEW_RAYCAST */
static point_vec_ptr _redraw;   /* PU_DELAY_VIS (delayed_visual_update) ... shared with light */

static dun_bmp_ptr _view;       /* singleton: only "plr_dun" ever has a "view" */
static int _view_count;

bool plr_view(point_t pos)
{ 
    if (plr->dun_id != cave->id) return FALSE;
    if (!_view) return FALSE;
    return dun_bmp_test(_view, pos);
}
void plr_view_iter(void (*f)(dun_ptr dun, point_t pos))
{
    if (!_view) return;
    dun_bmp_iter(_view, f);
}
void dun_forget_view(dun_ptr dun)
{
    if (_view)
    {
        dun_bmp_clear(_view);
        _view->dun = NULL;
    }
}
#if _VIEW_DEBUG
static doc_ptr _view_docs[2];
static doc_ptr _debug_doc(int dir)
{
    if (dir == 8 || dir == 6) return _view_docs[0];
    return _view_docs[1];
}
static str_ptr _screen_shot(dun_ptr dun, rect_t rect, bool view)
{
    str_ptr s = str_alloc_size(rect.cx * rect.cy);
    point_t min = rect_top_left(rect);
    point_t max = rect_bottom_right(rect);
    point_t p;
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        int  current_a = -1;
        if (plr->pos.y == p.y)
            str_append_s(s, "<color:b>");
        str_printf(s, "%+3d", p.y - plr->pos.y);
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            dun_cell_ptr cell = dun_cell_at(dun, p);
            term_char_t tc = cell_ascii(cell);

            if (view)
            {
                bool los = dun_bmp_test(_view, p);
                bool verify = (_engine == _VIEW_RAYCAST) ?  dun_bmp_test(_touched, p) : FALSE;

                if (los)
                {
                    if (verify) tc.a = TERM_VIOLET;
                    else tc.a = TERM_YELLOW;
                }
                else
                {
                    if (verify) tc.a = TERM_UMBER;
                    else if (p.y == plr->pos.y) tc.a = TERM_BLUE;
                    else if (p.x == plr->pos.x) tc.a = TERM_BLUE;
                    else tc.a = TERM_L_DARK;
                }
            }
            else if (p.x == plr->pos.x) tc.a = TERM_BLUE;

            if (dun_plr_at(dun, p))
                tc.c = '@';

            if (tc.a != current_a)
            {
                if (current_a >= 0 && current_a != TERM_WHITE)
                    str_append_s(s, "</color>");
                if (tc.a != TERM_WHITE)
                    str_printf(s, "<color:%c>", attr_to_attr_char(tc.a));
                current_a = tc.a;
            }
            str_append_c(s, tc.c);
        }
        if (current_a >= 0 && current_a != TERM_WHITE)
            str_append_s(s, "</color>");
        if (plr->pos.y == p.y)
            str_append_s(s, "</color>");
        str_append_c(s, '\n');
    }
    str_append_s(s, "  -");
    for (p.x = min.x; p.x <= max.x; p.x++)
    {
        int x = ABS(p.x - plr->pos.x);
        if (x == 0) str_append_s(s, "<color:b>");
        str_append_c(s, '0' + x%10);
        if (x == 0) str_append_s(s, "</color>");
    }
    str_append_c(s, '+');
    if (view && _engine == _VIEW_RAYCAST)
    {
        str_append_s(s, "\n<color:y>#</color> LoS Pass I (Raycast)\n");
        str_append_s(s, "<color:v>#</color> LoS Pass II (Verify)\n");
        str_append_s(s, "<color:u>#</color> No LoS Pass II (Verify)\n");
    }
    return s;
}
static void _view_begin(dun_ptr dun)
{
    if (_view_docs[0]) doc_free(_view_docs[0]);
    if (_view_docs[1]) doc_free(_view_docs[1]);
    _view_docs[0] = doc_alloc(50);
    _view_docs[1] = doc_alloc(50);

}
static void _view_end(dun_ptr dun)
{
    rect_t rect = dun_bmp_bounding_rect(_view);
    doc_ptr doc = doc_alloc(100);
    doc_ptr cols[2];
    str_ptr s;
    FILE *fff;
    char  buf[1024];

    cols[0] = doc_alloc(45);
    s = _screen_shot(dun, rect, FALSE);
    doc_insert(cols[0], "<style:screenshot>");
    doc_insert(cols[0], str_buffer(s));
    doc_insert(cols[0], "</style>");
    str_free(s);
    doc_insert_doc(cols[0], _view_docs[0], 0);
    doc_free(_view_docs[0]);
    _view_docs[0] = NULL;

    cols[1] = doc_alloc(45);
    s = _screen_shot(dun, rect, TRUE);
    doc_insert(cols[1], "<style:screenshot>");
    doc_insert(cols[1], str_buffer(s));
    doc_insert(cols[1], "</style>");
    str_free(s);
    doc_insert_doc(cols[1], _view_docs[1], 0);
    doc_free(_view_docs[1]);
    _view_docs[1] = NULL;

    doc_insert_cols(doc, cols, 2, 0);
    doc_free(cols[0]);
    doc_free(cols[1]);

    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "view.html");
    fff = my_fopen(buf, "w");
    if (fff)
    {
        doc_write_file(doc, fff, DOC_FORMAT_HTML);
        my_fclose(fff);
    }
    doc_free(doc);
}
#endif

static bool _allow(dun_ptr dun, point_t pos, u32b flag)
{
    dun_grid_ptr g = dun_grid_at(dun, pos);
    return BOOL(g->flags & flag);
}
static void _set_old_view(dun_ptr dun, point_t pos)
{
    if (!rect_contains_point(dun->rect, pos)) return; /* D_SURFACE teleport */
    point_vec_add(temp_pts, pos);
    dun_grid_at(dun, pos)->flags |= CELL_TEMP;
}
static void _set_new_view(dun_ptr dun, point_t pos)
{
    dun_grid_ptr grid = dun_grid_at(dun, pos);
    _view_count++;
    if (grid->flags & CELL_TEMP) return; /* was in old _view; no change */
    grid->flags |= CELL_NOTE | CELL_REDRAW;
    point_vec_push(_redraw, pos);
}
static void _wall_check(dun_ptr dun, point_t p);
static point_t _cardinal_line(dun_ptr dun, int dir, int adj_dir1, int adj_dir2)
{
    int i;
    point_t p = plr->pos, last = p;
    for (i = 1; i <= DUN_VIEW_MAX; i++)
    {
        p = point_step(p, dir);
        if (!dun_pos_valid(dun, p)) return last;
        dun_bmp_set(_view, p);
        _wall_check(dun, point_step(p, adj_dir1));
        _wall_check(dun, point_step(p, adj_dir2));
        if (!_allow(dun, p, CELL_LOS)) return p;
        last = p;
    }
    return p;
}
static point_t _diagonal_line(dun_ptr dun, int dir, int adj_dir1, int adj_dir2)
{
    int i;
    point_t p = plr->pos, last = p;
    for (i = 1; i <= DUN_VIEW_MAX * 2 / 3; i++)
    {
        p = point_step(p, dir);
        if (!dun_pos_valid(dun, p)) return last;
        dun_bmp_set(_view, p);
        if (!_allow(dun, p, CELL_LOS)) return p;
        _wall_check(dun, point_step(p, adj_dir1));
        _wall_check(dun, point_step(p, adj_dir2));
        last = p;
    }
    return p;
}
static point_t _line_aux(dun_ptr dun, dun_line_gen_ptr gen, int adj_dir)
{
    point_t p;
    dun_line_gen_first(gen);
    for(;;)
    {
        p = dun_line_gen_next(gen);
        dun_bmp_set(_view, p);
        if (!_allow(dun, p, CELL_LOS))
        {
            dun_bmp_set(_touched, point_step(p, adj_dir));
            break;
        }
        _wall_check(dun, point_step(p, adj_dir));
        if (dun_line_gen_distance(gen) >= DUN_VIEW_MAX) break; /* PROJECT_THRU */
    }
    return p;
}
static int _major_len(point_t p, int dir)
{
    switch (dir)
    {
    case 8: return plr->pos.y - p.y;
    case 6: return p.x - plr->pos.x;
    case 2: return p.y - plr->pos.y;
    case 4: return plr->pos.x - p.x;
    }
    assert(FALSE);
    return 0;
}
static int _minor_len(point_t p, int dir)
{
    switch (dir)
    {
    case 8:
    case 2: return ABS(p.x - plr->pos.x);
    case 6:
    case 4: return ABS(p.y - plr->pos.y);
    }
    assert(FALSE);
    return 0;
}
static int _major_max(dun_ptr dun, int dir)
{
    switch (dir)
    {
    case 8: return MIN(plr->pos.y - rect_top(dun->rect), DUN_VIEW_MAX);
    case 2: return MIN(rect_bottom(dun->rect) - plr->pos.y, DUN_VIEW_MAX);
    case 6: return MIN(rect_right(dun->rect) - plr->pos.x, DUN_VIEW_MAX);
    case 4: return MIN(plr->pos.x - rect_left(dun->rect), DUN_VIEW_MAX);
    }
    assert(FALSE);
    return 0;
}
static void _sweep(dun_ptr dun, point_t wall, int dir, int adj_dir)
{
    point_t last_wall, last_b = {0};
    int     max = _major_max(dun, dir);
    int     major, minor, s, nbs, nbr, ns;
    line_t  line;

    line.a = plr->pos;

    #if _VIEW_DEBUG
    doc_printf(_debug_doc(dir), " _sweep([%d,%d], %d)\n", wall.x - plr->pos.x, wall.y - plr->pos.y, adj_dir);
    #endif

    last_wall = wall;
    major = _major_len(wall, dir);
    minor = 0;

    for (;;)
    {
        bool found = FALSE;

        /* calculate projection pos (***very important***) */
        ns = minor + 1;   /* number of segments in next line that should use major grids */
        nbs = major / ns; /* base segment length */
        nbr = major % ns; /* remainder in sub-problem */
        /* The next line will be an N-choose-r problem (N = ns+1). We are
         * seeking the longest allowed line projection that will work. Adding
         * nbs to the next segment is fine, but often we can add nbs+1. This
         * will increase the remainder (r->r+1) and we need the last segment
         * to be an allowed choice for this new N-choose-r+1 problem. This is
         * very important due to PROJECT_THRU ray-casting (think of corners);
         * you'll get poor results without this hack.
         * cf ../lib/edit/n_choose_k.txt */
        switch (ns)
        {
/*2C1*/ case 1: nbs++; break;   /* "01 | 10" in n_choose_k ... "10" will just re-hit wall */
/*3C2*/ case 2: if (nbr == 1) { nbs++; } break;
/*4C3*/ case 3: if (nbr == 2) { nbs++; } break;
/*5C */ case 4: if (nbr == 2 || nbr == 3) { nbs++; } break;
/*6C */ case 5: if (nbr == 2 || nbr == 3 || nbr == 4) { nbs++; } break;
/*7C */ case 6: if (nbr == 3 || nbr == 4 || nbr == 5) { nbs++; } break;
        }
        line.b = point_step(last_wall, adj_dir); /* ideally, new line segment will pass thru this point */
        s = nbs - 1;      /* new segment will add nbs grids to line, but last_wall already counts as 1 */
        if (s + major > max) s = max - major;
        if (s > 0)
            line.b = point_jump(line.b, dir, s);

        if (!dun_pos_valid(dun, line.b)) return; /* *panic* */
        if (_minor_len(line.b, dir) >= _major_len(line.b, dir)) break;

        /* find next wall */
        for (;;)
        {
            if (!point_equals(last_b, line.b))
            {
                dun_line_gen_t gen;
                point_t w;
                assert(dun_pos_valid(dun, line.b));
                dun_line_gen_create(&gen, line);
                last_b = line.b;
                #if _VIEW_DEBUG
                doc_printf(_debug_doc(dir), "   _line([%d,%d])", line.b.x - plr->pos.x, line.b.y - plr->pos.y);
                #endif
                for (;;)
                {
                    w = _line_aux(dun, &gen, adj_dir);
                    if (!point_equals(w, last_wall))
                    {
                        #if _VIEW_DEBUG
                        doc_printf(_debug_doc(dir), " = [%d,%d]\n", w.x - plr->pos.x, w.y - plr->pos.y);
                        #endif
                        last_wall = w;
                        major = _major_len(last_wall, dir);
                        minor = _minor_len(last_wall, dir);
                        found = TRUE;
                        break;
                    }
                    if (!dun_line_gen_next_strategy(&gen)) break;
                }
                dun_line_gen_destroy(&gen);
                if (found) break;
                #if _VIEW_DEBUG
                doc_printf(_debug_doc(dir), " = <color:D>[%d,%d]</color>\n", w.x - plr->pos.x, w.y - plr->pos.y);
                #endif
            }
            line.b = point_step(line.b, adj_dir); /* slide until we clear last wall */
            if (!dun_pos_valid(dun, line.b)) return; /* *panic* */
            if (_minor_len(line.b, dir) >= _major_len(line.b, dir)) break; /* give up */
        }
        if (!found) break;
    }
}
static void _octant(dun_ptr dun, int dir, int adj_dir1, int adj_dir2)
{
    point_t w;
    #if _VIEW_DEBUG
    doc_printf(_debug_doc(dir), "<color:B>_octant(%d):</color>\n", dir);
    #endif
    w = _cardinal_line(dun, dir, adj_dir1, adj_dir2);
    _sweep(dun, w, dir, adj_dir1);
    _sweep(dun, w, dir, adj_dir2);
}

/* optimized _project for dun_update_view (naive) */
static bool _view_los_aux(dun_ptr dun, dun_line_gen_ptr gen)
{
    bool    success = FALSE;
    point_t pos;

    dun_line_gen_first(gen); /* skip initial point */
    while (!success)
    {
        pos = dun_line_gen_next(gen);
        dun_bmp_set(_view, pos);
        if (point_equals(pos, gen->line.b))
            return TRUE;

        if (!_allow(dun, pos, CELL_LOS)) break;
    }
    return success;
}
static bool _view_los(dun_ptr dun, point_t pos)
{
    bool b = FALSE;
    int  d = point_fast_distance(plr->pos, pos);
    if (d > DUN_VIEW_MAX) return FALSE;
    if (d < 2) b = TRUE;
    else
    {
        line_t         l;
        dun_line_gen_t gen;

        l.a = plr->pos;
        l.b = pos;
        dun_line_gen_create(&gen, l);
        for (;;)
        {
            b = _view_los_aux(dun, &gen);
            if (b) break;
            if (!dun_line_gen_next_strategy(&gen)) break;
        }
        dun_line_gen_destroy(&gen);
    }
    if (b) dun_bmp_set(_view, pos);
    return b;
}
static void _wall_check(dun_ptr dun, point_t p)
{
    if (!_allow(dun, p, CELL_PROJECT))
        dun_bmp_set(_view, p);
    else if (_engine == _VIEW_RAYCAST) /* ***very important**** */
        dun_bmp_set(_touched, p);
}
static void _missed(point_t p) /* profile *love* :D */
{
}
static void _verify(dun_ptr dun, point_t p)
{
    if (point_fast_distance(plr->pos, p) > DUN_VIEW_MAX) return;
    if (_view_los(dun, p))
        _missed(p);
}
void dun_update_view(dun_ptr dun)
{
    dun_mgr_ptr dm = dun_mgr();
    rect_t rect;
    int i, ct;

    if (dun->id != plr->dun_id) return;

    if (dm->prof)
        z_timer_resume(&dm->prof->view_timer);

    /***********************************************
     * initialize
     ***********************************************/
    rect = rect_create_centered(plr->pos, DUN_VIEW_MAX, DUN_VIEW_MAX);

    if (!_redraw) _redraw = point_vec_alloc();
    else point_vec_clear(_redraw);
     
    point_vec_clear(temp_pts);

    if (!_touched) _touched = dun_bmp_alloc(dun, rect);
    else
    {
        assert(_touched->rect.cx == rect.cx);
        assert(_touched->rect.cy == rect.cy);
        dun_bmp_clear(_touched);
        _touched->dun = dun;
        _touched->rect = rect;
    }

    if (!_view) _view = dun_bmp_alloc(dun, rect);
    else
    {
        assert(_view->rect.cx == rect.cx);
        assert(_view->rect.cy == rect.cy);
        if (_view->dun == dun)
            dun_bmp_iter(_view, _set_old_view);
        dun_bmp_clear(_view);
        _view->dun = dun;
        _view->rect = rect;
    }
    _view_count = 0;

    /***********************************************
     * calculate new view 
     ***********************************************/
    dun_bmp_set(_view, plr->pos);
    if (_engine == _VIEW_NAIVE)
    {
        rect_t r = rect_intersect(dun->rect, _view->rect);
        point_t min = rect_top_left(r);
        point_t max = rect_bottom_right(r), p;
        #if _VIEW_DEBUG
        _view_begin(dun);
        doc_printf(_view_docs[0], "<color:R>Update View: Naive <color:G>[%d,%d]</color></color>\n", plr->pos.x, plr->pos.y);
        #endif
        /* cardinal lines pick up walls in long corridors */
        _cardinal_line(dun, 8, 4, 6);
        _cardinal_line(dun, 6, 8, 2);
        _cardinal_line(dun, 2, 6, 4);
        _cardinal_line(dun, 4, 2, 8);
        /* process top to plr */
        for (p.y = min.y; p.y < plr->pos.y; p.y++)
        {
            for (p.x = min.x; p.x < plr->pos.x; p.x++)  /* left to plr */
                if (!dun_bmp_test(_view, p)) _view_los(dun, p);
            for (p.x = max.x; p.x > plr->pos.x; p.x--)  /* right to plr */
                if (!dun_bmp_test(_view, p)) _view_los(dun, p);
        }
        /* process bottom to plr */
        for (p.y = max.y; p.y > plr->pos.y; p.y--)
        {
            for (p.x = min.x; p.x < plr->pos.x; p.x++)  /* left to plr */
                if (!dun_bmp_test(_view, p)) _view_los(dun, p);
            for (p.x = max.x; p.x > plr->pos.x; p.x--)  /* right to plr */
                if (!dun_bmp_test(_view, p)) _view_los(dun, p);
        }
    }
    else /* _VIEW_RAYCAST */
    {
        #if _VIEW_DEBUG
        _view_begin(dun);
        doc_printf(_view_docs[0], "<color:R>Update View: Raycaster <color:G>[%d,%d]</color></color>\n", plr->pos.x, plr->pos.y);
        #endif
        _diagonal_line(dun, 7, 8, 4);
        _diagonal_line(dun, 9, 8, 6);
        _diagonal_line(dun, 3, 2, 6);
        _diagonal_line(dun, 1, 2, 4);
        _octant(dun, 8, 4, 6);
        _octant(dun, 6, 8, 2);
        _octant(dun, 2, 6, 4);
        _octant(dun, 4, 2, 8);

        dun_bmp_difference(_touched, _view);
        dun_bmp_iter(_touched, _verify);
    }

    /***********************************************
     * finalize
     ***********************************************/
    /* note and redraw any new los grids */
    dun_bmp_iter(_view, _set_new_view);

    /* redraw any grids that lost los ... clear temp flags */
    ct = point_vec_length(temp_pts);
    for (i = 0; i < ct; i++)
    {
        point_t p = point_vec_get(temp_pts, i);
        dun_grid_ptr g = dun_grid_at(dun, p);

        g->flags &= ~CELL_TEMP;
        if (dun_bmp_test(_view, p)) continue; /* still in _view; no change */

        g->flags |= CELL_REDRAW;
        point_vec_push(_redraw, p);
    }
    point_vec_clear(temp_pts);
    plr->update |= PU_DELAY_VIS;

    #if _VIEW_DEBUG
    _view_end(dun);
    #endif
    if (dm->prof)
    {
        z_timer_pause(&dm->prof->view_timer);
        dm->prof->view_count += _view_count;
    }
}
static void _wizard_view(dun_ptr dun)
{
    point_t uip;
    rect_t  map_rect = ui_map_rect();
    dun_bmp_ptr bmp = dun_bmp_alloc(dun, _view->rect);
    point_t min = rect_top_left(bmp->rect);
    point_t max = rect_bottom_right(bmp->rect);
    point_t p;

    assert(dun->id == plr->dun_id);

    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            if (!dun_pos_valid(dun, p)) continue;
            if (dun_los(dun, plr->pos, p))
                dun_bmp_set(bmp, p);
        }
    }

    min = rect_top_left(map_rect);
    max = rect_bottom_right(map_rect);
    msg_line_clear();
    msg_format("<color:B> Current Line of Sight (<color:R>%d</color>, <color:R>%s</color>)</color>. Hit Any Key.",
        dun_bmp_count(_view),
        _engine == _VIEW_RAYCAST ? "Raycast" : "Naive"
    );
    for (uip.y = min.y; uip.y <= max.y; uip.y++)
    {
        for (uip.x = min.x; uip.x <= max.x; uip.x++)
        {
            point_t cp = ui_pt_to_cave_pt(uip);
            bool view, los, seen;

            if (msg_line_contains(uip.y, uip.x)) continue;
            if (!dun_pos_valid(dun, cp)) continue;

            view = dun_bmp_test(_view, cp);
            los = dun_bmp_test(bmp, cp);
            seen = plr_can_see(cp);

            if (view || los)
            {
                byte attr = TERM_WHITE;
                if (view && los) attr = TERM_L_GREEN;
                else if (view) attr = TERM_L_RED;
                else if (los) attr = TERM_VIOLET;

                if (0 && seen) attr = TERM_YELLOW; /* for testing */

                if (dun_plr_at(dun, cp))
                {
                    Term_queue_bigchar(uip.x, uip.y, attr, '@', 0, 0);
                }
                else if (use_graphics)
                {
                    dun_grid_ptr grid = dun_grid_at(dun, cp);
                    mon_ptr mon = dun_mon_at(dun, cp);
                    char c;
                    if (mon) c = mon_char(mon);
                    else c = cell_ascii(grid).c;
                    Term_queue_bigchar(uip.x, uip.y, attr, c, 0, 0);
                }
                else
                    Term_queue_bigchar(uip.x, uip.y, attr, '*', 0, 0);
            }
        }
    }
    dun_bmp_free(bmp);
}
void dun_wizard_view(dun_ptr dun)
{
    if (!_view) return;
    if (dun->id != plr->dun_id) return;

    for (;;)
    {
        char ch;
        _wizard_view(dun);
        ch = inkey();
        if (ch == 't')
        {
            if (_engine == _VIEW_RAYCAST) _engine = _VIEW_NAIVE;
            else _engine = _VIEW_RAYCAST;
            dun_update_view(dun);
            msg_line_clear();
            prt_map();
            continue;
        }
        break;
    }
    msg_line_clear();
    prt_map();
}
/*************************************************************************
 * dun_path
 *************************************************************************/
#define _HIT_TARGET 0x10000000 /* shared with PROJECT_* flags */
static int _path_aux(dun_path_ptr path, dun_line_gen_ptr gen, int range)
{
    int n = 0;
    path->flags &= ~_HIT_TARGET;
    dun_line_gen_first(gen); /* skip initial point */
    for (;;)
    {
        point_t pos = dun_line_gen_next(gen);

        if (!dun_pos_interior(path->dun, pos)) break;

        path->points[n++] = pos;
        if (point_equals(pos, path->stop))
            path->flags |= _HIT_TARGET;

        if (dun_line_gen_distance(gen) >= range) break;
        if (dun_stop_project(path->dun, pos, path->stop, path->flags)) break;
    }
    return n;
}
static int _path(dun_path_ptr path, int range)
{
    line_t         l = line_create(path->start, path->stop);
    dun_line_gen_t gen;
    int            n = 0;

    if (point_equals(path->start, path->stop)) return 0;
    if (!dun_pos_interior(path->dun, path->start)) return 0;

    dun_line_gen_create(&gen, l);
    for (;;)
    {
        n = _path_aux(path, &gen, range);
        if (path->flags & _HIT_TARGET) break;
        if (!dun_line_gen_next_strategy(&gen)) break;
    }
    dun_line_gen_destroy(&gen);
    return n;
}
dun_path_ptr dun_path_alloc(dun_ptr dun, point_t start, point_t stop, u32b flags)
{
    return dun_path_alloc_aux(dun, start, stop, flags, DUN_PATH_MAX);
}
dun_path_ptr dun_path_alloc_aux(dun_ptr dun, point_t start, point_t stop, u32b flags, int range)
{
    dun_path_ptr path = malloc(sizeof(dun_path_t));
    memset(path, 0, sizeof(dun_path_t));
    path->dun = dun;
    path->start = start;
    path->stop = stop;
    path->flags = flags;
    assert(range <= DUN_VIEW_MAX);
    path->count = _path(path, range);
    if (!path->count)
        path->stop = path->start;
    else
        path->stop = path->points[path->count - 1];

    return path;
}
void dun_path_free(dun_path_ptr path)
{
    if (!path) return;
    free(path);
}

/*************************************************************************
 * dun_project
 *************************************************************************/
bool dun_project(dun_ptr dun, point_t p1, point_t p2)
{
    return dun_project_aux(dun, p1, p2, 0, DUN_PATH_MAX);
}
bool dun_project_aux(dun_ptr dun, point_t p1, point_t p2, u32b flags, int range)
{
    return _project(dun, p1, p2, flags, range);
}
bool dun_stop_project(dun_ptr dun, point_t pos, point_t stop, u32b flags)
{
    assert(dun_pos_interior(dun, pos));
    if (point_equals(pos, stop))
    {
        if (!(flags & PROJECT_THRU)) return TRUE;
    }

    if (flags & PROJECT_DISI)
    {
        if (dun_stop_disintegration_at(dun, pos)) return TRUE;
    }
    else if (flags & PROJECT_LOS)
    {
        dun_grid_ptr g = dun_grid_at(dun, pos);
        bool b;
        if (flags & PROJECT_ILLUSION)
            b = illusion_los(g);
        else
            b = cell_los(g);
        if (!b) return TRUE;
    }
    else if (!(flags & PROJECT_PATH))
    {
        dun_grid_ptr g = dun_grid_at(dun, pos);
        bool b;
        if (flags & PROJECT_ILLUSION)
            b = illusion_project(g);
        else
            b = cell_project(g);
        if (!b) return TRUE;
    }

    if (flags & PROJECT_STOP)
    {
        /* Sometimes stop at non-initial monsters/players */
        if (dun_plr_at(dun, pos) || dun_mon_at(dun, pos)) return TRUE;
    }
    return FALSE;
}

/*************************************************************************
 * dun_blast
 *************************************************************************/
static void _blast_grow(dun_blast_ptr blast, int n)
{
    if (n > blast->allocated)
    {
        dun_blast_point_ptr old_points = blast->points;
        if (!blast->allocated) blast->allocated = 10;
        else blast->allocated *= 2;
        if (blast->allocated < n) blast->allocated = n;
        blast->points = malloc(blast->allocated * sizeof(dun_blast_point_t));
        if (old_points)
            memcpy(blast->points, old_points, blast->count*sizeof(dun_blast_point_t));
        free(old_points);
    }
}
static void _blast_push(dun_blast_ptr blast, dun_blast_point_t pt)
{
    int i = blast->count;
    if (i >= blast->allocated) _blast_grow(blast, i+1);
    blast->count++;
    blast->points[i] = pt;
}
static void _blast_push_aux(dun_blast_ptr blast, point_t pos, int distance)
{
    dun_blast_point_t p;
    p.pos = pos;
    p.distance = distance;
    _blast_push(blast, p);
}
static dun_blast_ptr _blast_alloc(dun_ptr dun, int gf)
{
    dun_blast_ptr blast = malloc(sizeof(dun_blast_t));
    memset(blast, 0, sizeof(dun_blast_t));
    blast->dun = dun;
    blast->gf = gf;
    return blast;
}
static bool _gf_project(dun_ptr dun, point_t src, point_t dest, int gf)
{
    if (gf == GF_LIGHT || gf == GF_LIGHT_WEAK || gf == GF_DARK)
        return dun_los(dun, src, dest);
    else if (gf == GF_DISINTEGRATE)
        return dun_in_disintegration_range(dun, src, dest);
    return dun_project(dun, src, dest);
}
dun_blast_ptr dun_blast_ball(dun_ptr dun, point_t src, int rad, int gf)
{
    dun_blast_ptr blast = _blast_alloc(dun, gf);
    dun_blast_point_t p;
    int r, i; 

    /* determine the blast area; work from inside out */
    assert(rad <= MAX_PRECOMPUTE_DISTANCE);
    assert(rad <= DUN_BLAST_MAX);
    for (r = 0; r <= rad; r++)
    {
        point_vec_ptr pv = distance_offsets(r);

        for (i = 0; i < point_vec_length(pv); i++)
        {
            point_t v = point_vec_get(pv, i);
            p.pos = point_add(src, v);
            if (!dun_pos_valid(dun, p.pos)) continue;
            if (!_gf_project(dun, src, p.pos, gf)) continue;
            p.distance = r;
            _blast_push(blast, p);
        }
        blast->offsets[r + 1] = blast->count;
    }
    blast->radius = rad;
    blast->center = src;
    return blast;
}
dun_blast_ptr dun_blast_beam(dun_path_ptr path, int gf)
{
    dun_blast_ptr blast = _blast_alloc(path->dun, gf);
    int i; 

    /* beams affect the start grid, which is never part of the path.
     * why? consider -UnbarringWays ... we need to gf_affect_o any
     * chests underneath the plr! */
    _blast_push_aux(blast, path->start, 0);

    for (i = 0; i < path->count; i++)
        _blast_push_aux(blast, path->points[i], 0);

    blast->offsets[1] = blast->count;
    blast->radius = 1;
    return blast;
}
/*
 * XXX ported breath_shape
 * Create a conical breath attack
 *
 *         ***
 *     ********
 * D********@**
 *     ********
 *         ***
 */
dun_blast_ptr dun_blast_breath(dun_path_ptr path, int rad, int gf)
{
    dun_blast_ptr blast = _blast_alloc(path->dun, gf);
    point_t bp = path->start;
    line_t l = line_create(path->start, path->stop);
    int i = 0;
    int brad = 0;
    int brev;
    int bdis = 0;
    int cdis;
    int mdis;

    assert(path->count); /* don't call me with an empty path! */
    brev = rad * rad / path->count;
    mdis = point_distance(path->start, path->stop) + rad;

    while (bdis <= mdis)
    {
        if (i < path->count)
        {
            point_t np = path->points[i];
            int nd = point_distance(np, path->start);

            /* Get next base point */
            if (bdis >= nd)
            {
                bp = np;
                i++;
            }
        }

        /* Travel from center outward */
        for (cdis = 0; cdis <= brad; cdis++)
        {
            point_t tp;
            /* Scan the maximal blast area of radius "cdis" */
            for (tp.y = bp.y - cdis; tp.y <= bp.y + cdis; tp.y++)
            {
                for (tp.x = bp.x - cdis; tp.x <= bp.x + cdis; tp.x++)
                {
                    if (!dun_pos_valid(path->dun, tp)) continue;

                    /* Enforce a circular "ripple" */
                    if (point_distance(path->start, tp) != bdis) continue;

                    /* Enforce an arc */
                    if (point_distance(bp, tp) != cdis) continue;

                    /* Enforce projectable */
                    if (!_gf_project(path->dun, bp, tp, gf)) continue;

                    /* Save this grid */
                    _blast_push_aux(blast, tp, point_distance_to_line(tp, l));
                }
            }
        }

        /* Encode some more "radius" info */
        blast->offsets[bdis + 1] = blast->count;

        /* Increase the size */
        brad = rad * (i + brev) / (path->count + brev);

        /* Find the next ripple */
        bdis++;
    }

    /* Store the effect size */
    blast->radius = bdis;
    blast->center = path->stop;
    return blast;
}
/* To test for a point inside a triangle, we need a primitive to test
 * whether or not 2 points lie on opposite sides of a line (segment).
 * A neat trick to do this is to compare the orientation of an obvious
 * cross product:
 *                  C
 *         
 * A-------------B
 *
 *   D
 * Note that CAxCB points out of the screen while DAxDB points into
 * the screen (right hand rule). */
static int _cross_product(point_t v1, point_t v2)
{
    /* for planar vectors, only the z-cofactor of the determinant expansion is non-zero */
    return v1.x*v2.y - v1.y*v2.x;
}
static int _orientation(point_t p, line_t l)
{
    point_t pa = point_subtract(l.a, p);
    point_t pb = point_subtract(l.b, p);
    int     z = _cross_product(pa, pb);
    return SGN(z);
}
static void _bmp_line(dun_ptr dun, dun_bmp_ptr bmp, line_t l)
{
    dun_path_ptr path = dun_path_alloc(dun, l.a, l.b, 0);
    int i;
    for (i = 0; i < path->count; i++)
    {
        point_t p = path->points[i];
        dun_bmp_set(bmp, p);
    }
    dun_path_free(path);
}
dun_blast_ptr dun_blast_triangle(dun_ptr dun, triangle_t t, int gf)
{
    dun_blast_ptr blast = _blast_alloc(dun, gf);
    dun_bmp_ptr   bmp;
    rect_t        rect = triangle_bounding_rect(t);
    point_t       min, max, c, p;
    line_t        sides[3], l;
    int           o, r = 0;

    if (!triangle_is_valid(t)) return blast;
    if (!rect_is_valid(rect)) return blast;

    min = rect_top_left(rect);
    max = rect_bottom_right(rect);

    sides[0] = line_create(t.a, t.b);
    sides[1] = line_create(t.b, t.c);
    sides[2] = line_create(t.c, t.a);

    /* orientation of centerpoint ... avoid rounding issues by scaling the determinant */
    c.x = t.a.x + t.b.x + t.c.x;
    c.y = t.a.y + t.b.y + t.c.y;
    l = line_create_xy(3*t.a.x, 3*t.a.y, 3*t.b.x, 3*t.b.y);
    o = _orientation(c, l);

    if (!o) return blast; /* center lies on side[0] ... probably a degenerate case */

    /* Generally, we get better results by manually projecting along the
     * 3 sides of the triangle. Without this, there are sometimes "dicontinuities"
     * in the field. With this, sometimes the field seems too big. */
    bmp = dun_bmp_alloc(dun, rect);
    _bmp_line(dun, bmp, sides[0]);
    _bmp_line(dun, bmp, sides[1]);
    _bmp_line(dun, bmp, sides[2]);

    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            /* boundary point */
            if (dun_bmp_test(bmp, p))
            {
                _blast_push_aux(blast, p, 0);
                continue;
            }
            /* interior point */
            /* points outside the triangle will have opposite orientations
             * to at least one of the sides. orientation of 0 is on a side, 
             * and should be included in the blast */
            if ( o*_orientation(p, sides[0]) >= 0
              && o*_orientation(p, sides[1]) >= 0
              && o*_orientation(p, sides[2]) >= 0 )
            {
                _blast_push_aux(blast, p, 0);
            }
        }
        blast->offsets[++r] = blast->count; /* animate horizontal "scan lines" */
    }
    dun_bmp_free(bmp);
    blast->radius = r;
    return blast;
}
void dun_blast_free(dun_blast_ptr blast)
{
    if (!blast) return;
    if (blast->points) free(blast->points);
    free(blast);
}
bool dun_blast_iter(dun_blast_ptr blast, dun_blast_f f)
{
    int i;
    bool result = FALSE;
    for (i = 0; i < blast->count; i++)
    {
        if (f(blast, blast->points[i]))
            result = TRUE;
    }
    if (result) blast->notice = TRUE;
    return result;
}
dun_blast_ptr dun_blast_burst(dun_ptr dun, point_t src, int rad, int gf)
{
    /* a "burst" is a "ball" with radius 1 epicenter */
    dun_blast_ptr blast = dun_blast_ball(dun, src, rad, gf);
    int i;
    for (i = 0; i < blast->count; i++)
    {
        dun_blast_point_ptr point = &blast->points[i];
        if (point->distance)
            point->distance--;
    }
    return blast;
}
/*************************************************************************
 * update noctovision (ranged): uses _view (cf update_stuff)
 *************************************************************************/
static dun_bmp_ptr _nocto;
static void _set_old_nocto(dun_ptr dun, point_t pos)
{
    if (!rect_contains_point(dun->rect, pos)) return; /* D_SURFACE teleport */
    point_vec_add(temp_pts, pos);
    dun_grid_at(dun, pos)->flags |= CELL_TEMP;
}
static void _set_new_nocto(dun_ptr dun, point_t pos)
{
    dun_grid_ptr grid = dun_grid_at(dun, pos);
    if (grid->flags & CELL_TEMP) return; /* was in old _nocto; no change */
    grid->flags |= CELL_NOTE | CELL_REDRAW;
    point_vec_push(_redraw, pos);
}
static void dun_update_nocto(dun_ptr dun)
{
    rect_t rect;
    int i, ct;

    if (dun->id != plr->dun_id) return;
    if (!plr->see_nocto || plr->see_nocto >= DUN_VIEW_MAX) return;

    /* initialize */
    point_vec_clear(temp_pts);

    rect = rect_create_centered(plr->pos, DUN_VIEW_MAX, DUN_VIEW_MAX);

    if (!_nocto) _nocto = dun_bmp_alloc(dun, rect);
    else
    {
        assert(_nocto->rect.cx == rect.cx);
        assert(_nocto->rect.cy == rect.cy);
        if (_nocto->dun == dun)
            dun_bmp_iter(_nocto, _set_old_nocto);
        dun_bmp_clear(_nocto);
        _nocto->dun = dun;
        _nocto->rect = rect;
    }

    /* calculate */
    {
        rect_t r = rect_create_centered(plr->pos, plr->see_nocto, plr->see_nocto);
        point_t min = rect_top_left(r);
        point_t max = rect_bottom_right(r), p;
        for (p.y = min.y; p.y <= max.y; p.y++)
        {
            for (p.x = min.x; p.x <= max.x; p.x++)
            {
                if (point_fast_distance(plr->pos, p) > plr->see_nocto) continue;
                if (dun_bmp_test(_view, p))
                    dun_bmp_set(_nocto, p);
            }
        }
    }

    /* finalize */
    dun_bmp_iter(_nocto, _set_new_nocto); /* note points entering nocto */
    ct = point_vec_length(temp_pts); 
    for (i = 0; i < ct; i++)
    {
        point_t p = point_vec_get(temp_pts, i);
        dun_grid_ptr g = dun_grid_at(dun, p);

        g->flags &= ~CELL_TEMP;
        if (dun_bmp_test(_nocto, p)) continue; /* still in _nocto; no change */

        g->flags |= CELL_REDRAW;  /* note points leaving nocto */
        point_vec_push(_redraw, p);
    }
    plr->update |= PU_DELAY_VIS;
    point_vec_clear(temp_pts);
}
/*************************************************************************
 * update light: uses _view (cf update_stuff)
 *************************************************************************/
/* Debug level 0 - Off
 *             1 - Basic Diagnostics (Screenshot for terrain, plr and each mon)
 *             2 - Show Wall Leakage Handling
 * Note: debugging re-generates "light.html" in your user dir for every dun_update_light */
#define _LIGHT_DEBUG 0

static dun_light_ptr _light;     /* singleton: only "plr_dun" ever has "light" */
int plr_light(point_t pos)
{
    int l;
    if (plr->dun_id != cave->id) return 0;
    if (!_light) return 0;
    l = dun_light_get(_light, pos);
    if (plr_view(pos))
        l += cave->ambient_light;
    return l;
}
static void _unlight_mon(int id, mon_ptr mon) { mon->mflag &= ~MFLAG_LIGHT; }
void dun_forget_light(dun_ptr dun)
{
    if (_light)
    {
        dun_light_clear(_light);
        _light->dun = NULL;
        dun_iter_mon(dun, _unlight_mon);
    }
}
#if _LIGHT_DEBUG
static doc_ptr _light_docs[2];
static int _light_col;
static str_ptr _light_screen_shot(dun_ptr dun, rect_t rect, bool show_light)
{
    str_ptr s = str_alloc_size(rect.cx * rect.cy);
    point_t min = rect_top_left(rect);
    point_t max = rect_bottom_right(rect);
    point_t p;
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        int  current_a = -1;
        if (plr->pos.y == p.y)
            str_append_s(s, "<color:b>");
        str_printf(s, "%+3d", p.y - plr->pos.y);
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            dun_cell_ptr cell = dun_cell_at(dun, p);
            term_char_t tc = cell_ascii(cell);
            mon_ptr mon = dun_mon_at(dun, p);

            if (show_light)
            {
                int  light = dun_light_get(_light, p);
                if (light > 0)
                {
                    char colors[6] = {TERM_L_UMBER, TERM_UMBER, TERM_YELLOW,
                                      TERM_L_RED, TERM_RED, TERM_VIOLET};
                    int index = MIN(5, light - 1);
                    tc.a = colors[index];
                }
                else if (light == 0)
                    tc.a = TERM_WHITE;
                else
                {
                    tc.a = TERM_L_DARK;
                    tc.c = ' ';
                }
            }
            else if (p.x == plr->pos.x) tc.a = TERM_BLUE;

            if (dun_plr_at(dun, p))
                tc.c = '@';
            else if (mon)
                tc.c = mon_char(mon);

            if (tc.a != current_a)
            {
                if (current_a >= 0 && current_a != TERM_WHITE)
                    str_append_s(s, "</color>");
                if (tc.a != TERM_WHITE)
                    str_printf(s, "<color:%c>", attr_to_attr_char(tc.a));
                current_a = tc.a;
            }
            str_append_c(s, tc.c);
        }
        if (current_a >= 0 && current_a != TERM_WHITE)
            str_append_s(s, "</color>");
        if (plr->pos.y == p.y)
            str_append_s(s, "</color>");
        str_append_c(s, '\n');
    }
    str_append_s(s, "  -");
    for (p.x = min.x; p.x <= max.x; p.x++)
    {
        int x = ABS(p.x - plr->pos.x);
        if (x == 0) str_append_s(s, "<color:b>");
        str_append_c(s, '0' + x%10);
        if (x == 0) str_append_s(s, "</color>");
    }
    str_append_c(s, '+');
    return s;
}
static void _light_begin(dun_ptr dun)
{
    if (_light_docs[0]) doc_free(_light_docs[0]);
    if (_light_docs[1]) doc_free(_light_docs[1]);
    _light_docs[0] = doc_alloc(50);
    _light_docs[1] = doc_alloc(50);
    _light_col = 0;

}
static void _light_debug(cptr msg)
{
    doc_ptr doc = _light_docs[_light_col % 2];
    rect_t rect = dun_light_bounding_rect(_light);
    str_ptr s = _light_screen_shot(_light->dun, rect, TRUE);
    doc_printf(doc, "<color:U>%s</color>\n", msg);
    doc_insert(doc, "<style:screenshot>");
    doc_insert(doc, str_buffer(s));
    doc_insert(doc, "</style>\n");
    str_free(s);
    _light_col++;
}
static void _light_end(dun_ptr dun)
{
    rect_t rect = dun_light_bounding_rect(_light);
    doc_ptr doc = doc_alloc(100);
    doc_ptr cols[2];
    str_ptr s;
    FILE *fff;
    char  buf[1024];

    cols[0] = doc_alloc(45);
    s = _light_screen_shot(dun, rect, FALSE);
    doc_insert(cols[0], "<style:screenshot>");
    doc_insert(cols[0], str_buffer(s));
    doc_insert(cols[0], "</style>");
    str_free(s);
    doc_insert_doc(cols[0], _light_docs[0], 0);
    doc_free(_light_docs[0]);
    _light_docs[0] = NULL;

    cols[1] = doc_alloc(45);
    s = _light_screen_shot(dun, rect, TRUE);
    doc_insert(cols[1], "<style:screenshot>");
    doc_insert(cols[1], str_buffer(s));
    doc_insert(cols[1], "</style>");
    str_free(s);
    doc_insert_doc(cols[1], _light_docs[1], 0);
    doc_free(_light_docs[1]);
    _light_docs[1] = NULL;

    doc_insert_cols(doc, cols, 2, 0);
    doc_free(cols[0]);
    doc_free(cols[1]);

    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "light.html");
    fff = my_fopen(buf, "w");
    if (fff)
    {
        doc_write_file(doc, fff, DOC_FORMAT_HTML);
        my_fclose(fff);
    }
    doc_free(doc);
}
#endif
int mon_light_radius(mon_ptr mon)
{
    int r = mon->race->light;
    if (!mon_tim_find(mon, MT_SLEEP))
        r += mon->race->lantern;
    return r;
}
#define _NE 0x100
#define _N  0x080
#define _NW 0x040
#define _E  0x020
#define _W  0x008
#define _SE 0x004
#define _S  0x002
#define _SW 0x001
#if _LIGHT_DEBUG > 1
static void _wall_face_doc(doc_ptr doc, u32b face)
{
    if (face & _NE) doc_insert(doc, " NE");
    if (face & _N) doc_insert(doc, " N");
    if (face & _NW) doc_insert(doc, " NW");
    if (face & _E) doc_insert(doc, " E");
    if (face & _W) doc_insert(doc, " W");
    if (face & _SE) doc_insert(doc, " SE");
    if (face & _S) doc_insert(doc, " S");
    if (face & _SW) doc_insert(doc, " SW");
}
#endif
static u32b _wall_face(dun_ptr dun, point_t src, point_t dest)
{
    u32b    face = 0;
    point_t v, a, s;

    if (point_equals(src, dest)) return 0xFFFFFFFF;

    v = point_subtract(dest, src);
    a = point_abs(v);
    s = point_sign(v);

    /* vertical */
    if (a.y > a.x)
    {
        if (s.y < 0) /* up */
        {
            if (s.x == 0) /* straight up */
            {
                face |= _S | _SE | _SW;
            }
            else if (s.x < 0) /* up and left */
            {
                face |= _SE;
                if (dun_allow_los_at(dun, point_step(dest, 2)))
                    face |= _S;
                if (dun_allow_los_at(dun, point_step(dest, 6)))
                    face |= _E;
            }
            else /* up and right */
            {
                face |= _SW;
                if (dun_allow_los_at(dun, point_step(dest, 2)))
                    face |= _S;
                if (dun_allow_los_at(dun, point_step(dest, 4)))
                    face |= _W;
            }
        }
        else /* down */
        {
            if (s.x == 0) /* straight down */
            {
                face |= _N | _NE | _NW;
            }
            else if (s.x < 0) /* down and left */
            {
                face |= _NE;
                if (dun_allow_los_at(dun, point_step(dest, 8)))
                    face |= _N;
                if (dun_allow_los_at(dun, point_step(dest, 6)))
                    face |= _E;
            }
            else /* down and right */
            {
                face |= _NW;
                if (dun_allow_los_at(dun, point_step(dest, 8)))
                    face |= _N;
                if (dun_allow_los_at(dun, point_step(dest, 4)))
                    face |= _W;
            }
        }
    }
    else /* horizontal */
    {
        if (s.x > 0) /* right */
        {
            if (s.y == 0) /* straight right */
            {
                face |= _W | _NW | _SW;
            }
            else if (s.y < 0) /* right and up */
            {
                face |= _SW;
                if (dun_allow_los_at(dun, point_step(dest, 4)))
                    face |= _W;
                if (dun_allow_los_at(dun, point_step(dest, 2)))
                    face |= _S;
            }
            else /* right and down */
            {
                face |= _NW;
                if (dun_allow_los_at(dun, point_step(dest, 4)))
                    face |= _W;
                if (dun_allow_los_at(dun, point_step(dest, 8)))
                    face |= _N;
            }
        }
        else /* left */
        {
            if (s.y == 0) /* straight left */
            {
                face |= _E | _NE | _SE;
            }
            else if (s.y < 0) /* left and up */
            {
                face |= _SE;
                if (dun_allow_los_at(dun, point_step(dest, 6)))
                    face |= _E;
                if (dun_allow_los_at(dun, point_step(dest, 2)))
                    face |= _S;
            }
            else /* left and down */
            {
                face |= _NE;
                if (dun_allow_los_at(dun, point_step(dest, 6)))
                    face |= _E;
                if (dun_allow_los_at(dun, point_step(dest, 8)))
                    face |= _N;
            }
        }
    }
    return face;
}
#define _MAX_MON_LIGHT 6
static void _set_mon_light(int id, mon_ptr mon)
{
    int r, i;
    dun_blast_ptr blast;
    bool dark = FALSE;

    mon->mflag &= ~MFLAG_LIGHT;
    if (mon->cdis > DUN_VIEW_MAX + _MAX_MON_LIGHT) return;

    r = mon_light_radius(mon);
    if (!r) return;

    mon->mflag |= MFLAG_LIGHT;
    if (r < 0)
    {
        dark = TRUE;
        r = -r;
    } 
    blast = dun_blast_burst(mon->dun, mon->pos, r, GF_LIGHT);
    for (i = 0; i < blast->count; i++)
    {
        dun_blast_point_t p = blast->points[i];
        if (dun_bmp_test(_view, p.pos))
        {
            int power = blast->radius - p.distance;
            bool add = dark ? -power : power;
            if (!dun_allow_los_at(_view->dun, p.pos))
            {
                /* Need to handle leakage (marked with % below):
                   Great Storm Wyrm [-4,-1]                     
                    -2##%##########                             
                    -1#D%.........#                             
                    +0#.%..@......#                             
                    +1#.%.........+                             
                    +2............#                             
                    +3#.#.........#                             
                    +4#.###########                             
                     -5432101234567+
                XXX Hack: Figure out which side of wall each can see (including corners) */        
                u32b mon_face = _wall_face(_view->dun, mon->pos, p.pos);
                u32b plr_face = _wall_face(_view->dun, plr->pos, p.pos);
                if (!(mon_face & plr_face)) continue;
                #if _LIGHT_DEBUG > 1
               {doc_ptr doc = _light_docs[_light_col % 2];
                doc_printf(doc, "_mon_face([%d,%d]) =", p.pos.x - mon->pos.x, p.pos.y - mon->pos.y);
                _wall_face_doc(doc, mon_face);
                doc_newline(doc);
                doc_printf(doc, "_plr_face([%d,%d]) =", p.pos.x - plr->pos.x, p.pos.y - plr->pos.y);
                _wall_face_doc(doc, plr_face);
                doc_newline(doc); }
                #endif
            }
            dun_light_add(_light, p.pos, add);
        }
    }
    dun_blast_free(blast);
    #if _LIGHT_DEBUG
    _light_debug(format("%s [%d,%d]", mon->race->name,
        mon->pos.x - plr->pos.x, mon->pos.y - plr->pos.y));
    #endif
}
static bool _check_wall_glow_aux(dun_ptr dun, point_t pos)
{
    dun_grid_ptr g = dun_grid_at(dun, pos);
    return cell_los(g) && (g->flags & CELL_LIT);
}
static bool _check_wall_glow(dun_ptr dun, point_t pos)
{
    point_t v, s;

    if (point_equals(plr->pos, pos))
        return BOOL(dun_grid_at(dun, pos)->flags & CELL_LIT);

    v = point_subtract(plr->pos, pos); /* vector from pos to plr */
    s = point_sign(v);                 /* step from pos to plr */

    if (_check_wall_glow_aux(dun, point_add(pos, s)))
        return TRUE;
    if (s.x && s.y) /* diagonal step should check "cardinal" steps as well */
    {
        if (_check_wall_glow_aux(dun, point_create(pos.x + s.x, pos.y)))
            return TRUE;
        if (_check_wall_glow_aux(dun, point_create(pos.x, pos.y + s.y)))
            return TRUE;
    }
    return FALSE;
}
static void _set_grid_light(point_t pos, dun_grid_ptr grid)
{
    dun_ptr dun = _light->dun;
    int i, r;

    /* handle illuminated grids (only in view) */
    if (dun_bmp_test(_view, pos))
    {
        int glow = 0;
        /* features can override grid illumination: FF_DARK features resist light
         * and can never be illuminated. FF_GLOW features resist dark and are always
         * illuminated */
        if (grid->flags & CELL_LIGHT)
            glow = 1;
        else if (grid->flags & CELL_DARK)
            glow = -1;
        else if (!cell_los(grid)) /* wall */
        {
            /* Fix wall lighting: A lit room will CAVE_GLOW its boundary walls,
             * but these should not count as lit if the plr is outside the room.
             *
             * Steal floor lighting: GF_LIGHT_WEAK a floor tile and adjacent walls
             * s/b illuminated (e.g. Line of Light down a long, dark corridor). */
            if (_check_wall_glow(dun, pos))
            {
                glow = 1;
                grid->flags |= CELL_LIT; /* possibly stolen ... cf map_info for torch light display */
            }
        }
        else if (grid->flags & CELL_LIT)
            glow = 1;
        if (glow)
            dun_light_add(_light, pos, glow);
    }

    /* handle feature light|dark */
    r = cell_light(grid);
    if (r == 1) /* optimize deep lava fields (D_MOUNT_DOOM) (1.9x runtime w/o this) */
    {
        if (dun_bmp_test(_view, pos))
            dun_light_add(_light, pos, 1);
        for (i = 0; i < 8; i++)
        {
            point_t p = point_step(pos, ddd[i]);
            if (dun_bmp_test(_view, p))
            {
                if (!dun_allow_los_at(dun, p)) /* handle wall leakage */
                {
                    u32b feat_face = _wall_face(dun, pos, p);
                    u32b plr_face = _wall_face(dun, plr->pos, p);
                    if (!(feat_face & plr_face)) continue;
                }
                dun_light_add(_light, p, 1);
            }
        }
    }
    else if (r != 0) /* handle arbitrary feat->light */
    {
        dun_blast_ptr blast;
        bool dark = FALSE;
        if (r < 0)
        {
            dark = TRUE;
            r = -r;
        } 
        blast = dun_blast_burst(dun, pos, r, GF_LIGHT);
        for (i = 0; i < blast->count; i++)
        {
            dun_blast_point_t p = blast->points[i];
            if (dun_bmp_test(_view, p.pos))
            {
                int power = blast->radius - p.distance;
                bool add = dark ? -power : power;
                if (!dun_allow_los_at(dun, p.pos)) /* handle wall leakage */
                {
                    u32b feat_face = _wall_face(dun, pos, p.pos);
                    u32b plr_face = _wall_face(dun, plr->pos, p.pos);
                    if (!(feat_face & plr_face)) continue;
                }
                dun_light_add(_light, p.pos, add);
            }
        }
        dun_blast_free(blast);
    }
}
#define _MAX_GRID_LIGHT 3
static void _terrain(dun_ptr dun)
{
    rect_t rect;
    if (_view_count < 150)
        rect = dun_bmp_bounding_rect(_view);
    else
        rect = _view->rect;
    rect = rect_inflate(rect, _MAX_GRID_LIGHT, _MAX_GRID_LIGHT);
    dun_iter_rect(dun, rect, _set_grid_light);
}

#define _LIGHT_OFFSET 100
#define _LIGHT(O,N)   ((((O)+_LIGHT_OFFSET) << 8) | ((N)+_LIGHT_OFFSET))
#define _LIGHT_OLD(L) (((L) >> 8)-_LIGHT_OFFSET)
#define _LIGHT_NEW(L) (((L) & 0xFF)-_LIGHT_OFFSET)

static point_map_ptr _light_map;
static void _set_old_light(dun_ptr dun, point_t pos, int light)
{
    point_map_add_int(_light_map, pos, _LIGHT(light, 0));
}
static void _set_new_light(dun_ptr dun, point_t pos, int light)
{
    u16b l = point_map_find_int(_light_map, pos);
    int  ol = _LIGHT_OLD(l);
    if (ol == 0 - _LIGHT_OFFSET) /* XXX detect failed lookup */
        point_map_add_int(_light_map, pos, _LIGHT(0, light));
    else
        point_map_add_int(_light_map, pos, _LIGHT(ol, light));
}
static void _redraw_light(point_t pos, int l)
{
    int ol, nl;
    if (!dun_pos_valid(_light->dun, pos)) return; /* XXX wilderness scroll */
    ol = _LIGHT_OLD(l);
    nl = _LIGHT_NEW(l);
    if (ol != nl)
    {
        dun_grid_at(_light->dun, pos)->flags |= CELL_NOTE | CELL_REDRAW;
        point_vec_push(_redraw, pos);
    }
}
void dun_update_light(dun_ptr dun)
{
    dun_mgr_ptr dm = dun_mgr();
    rect_t rect;

    if (dun->id != plr->dun_id) return;
    assert(_view); /* dun_update_view s/b called first (cf update_stuff) */
    assert(_redraw);

    if (dm->prof)
        z_timer_resume(&dm->prof->light_timer);

    /* initialize */
    rect = rect_create_centered(plr->pos, DUN_VIEW_MAX, DUN_VIEW_MAX);

    if (!_light_map) _light_map = point_map_alloc(NULL);
    else point_map_clear(_light_map);

    if (!_light)
        _light = dun_light_alloc(dun, rect);
    else
    {
        assert(_light->rect.cx == rect.cx);
        assert(_light->rect.cy == rect.cy);
        if (_light->dun == dun)
            dun_light_iter(_light, _set_old_light);
        dun_light_clear(_light);
        _light->dun = dun;
        _light->rect = rect;
    }

    #if _LIGHT_DEBUG
    _light_begin(dun);
    doc_printf(_light_docs[0], "<color:R>Update Lite <color:G>[%d,%d]</color></color>\n", plr->pos.x, plr->pos.y);
    #endif

    /* terrain */
    _terrain(dun);
    #if _LIGHT_DEBUG
    _light_debug("Terrain");
    #endif

    /* plr */
    if (plr->cur_light)
    {
        /* use "view" semantics for the player's torch, rather than
         * "projection" semantics. this will give better illuminated
         * wall behavior */
        int rad = MIN(MAX_PRECOMPUTE_DISTANCE, ABS(plr->cur_light));
        bool dark = plr->cur_light < 0;
        int i, r;

        for (r = 0; r <= rad; r++)
        {
            point_vec_ptr pv = distance_offsets(r);

            for (i = 0; i < point_vec_length(pv); i++)
            {
                point_t v = point_vec_get(pv, i);
                point_t p = point_add(plr->pos, v);
                int     r2, power, add;
                if (!dun_bmp_test(_view, p)) continue;
                if (r > 0) r2 = r - 1;
                else r2 = r;
                power = rad - r2;
                add = dark ? -power : power;
                dun_light_add(_light, p, add);
            }
        }
    }

    #if _LIGHT_DEBUG
    _light_debug("Player");
    #endif

    /* monsters */
    dun_iter_mon(dun, _set_mon_light);

    /* finalize */
    dun_light_iter(_light, _set_new_light);
    point_map_iter_int(_light_map, _redraw_light);
    plr->update |= PU_DELAY_VIS;

    /* hack for *ranged* noctovision */
    if (0 < plr->see_nocto && plr->see_nocto < DUN_VIEW_MAX)
        dun_update_nocto(dun);

    if (dm->prof)
        z_timer_pause(&dm->prof->light_timer);

    plr_hook_update_light();

    #if _LIGHT_DEBUG
    _light_end(dun);
    #endif
}
void dun_update_mon_light(dun_ptr dun)
{
    bool found = FALSE;
    int_map_iter_ptr iii;
    for (iii = int_map_iter_alloc(dun->mon);
          int_map_iter_is_valid(iii);
          int_map_iter_next(iii))
    {
        mon_ptr mon = int_map_iter_current(iii);
        bool    do_it = FALSE;
        if (mon->cdis <= DUN_VIEW_MAX + _MAX_MON_LIGHT)
        {
            int r = mon_light_radius(mon);
            if (r) do_it = TRUE;
        }
        /* do any monster in view range *and* any monster
         * formerly in view range (e.g. monster goes to sleep,
         * extinguishing lantern ... or monster teleports) */
        if (do_it || (mon->mflag & MFLAG_LIGHT))
        {
            found = TRUE;
            break;
        }
    }
    int_map_iter_free(iii);
    if (found)
        dun_update_light(dun);
}

void delayed_visual_update(dun_ptr dun)
{
    dun_mgr_ptr dm = dun_mgr();
    int i, ct;

    if (!_redraw) return;
    if (dun->id != plr->dun_id) return;

    if (dm->prof)
        z_timer_resume(&dm->prof->redraw_timer);

    ct = point_vec_length(_redraw);
    for (i = 0; i < ct; i++)
    {
        point_t p = point_vec_get(_redraw, i);
        dun_grid_ptr g;
        mon_ptr mon;

        if (!dun_pos_valid(dun, p)) continue; /* paranoia */

        g = dun_grid_at(dun, p);
        if (!(g->flags & CELL_REDRAW)) continue; /* duplicate already processed */

        if (g->flags & CELL_NOTE) note_pos(p);
        draw_pos(p);

        mon = dun_mon_at(dun, p);
        if (mon) update_mon(mon, FALSE);

        g->flags &= ~(CELL_NOTE | CELL_REDRAW);
    }
    point_vec_clear(_redraw);
    if (dm->prof)
        z_timer_pause(&dm->prof->redraw_timer);
}

/*************************************************************************
 * utilities
 *************************************************************************/
static bool _path_visual, _blast_visual;
static vec_ptr _monsters = NULL;
static int _gf;
static int _max_mon_dam(void)
{   /* report max damage sustained by a single monster for device lore */
    int i, d = 0;
    for (i = 0; i < vec_length(_monsters); i++)
    {
        mon_ptr mon = vec_get(_monsters, i);
        if (mon->project_dam > d)
            d = mon->project_dam;
    }
    return d;
}
static bool _mon_notice(void)
{
    int i;
    for (i = 0; i < vec_length(_monsters); i++)
    {
        mon_ptr mon = vec_get(_monsters, i);
        if (mon->project_notice) return TRUE;
    }
    return FALSE;
}
static void _unmark(int id, mon_ptr mon)
{   /* reset mon projection tracking info */
    mon->mflag &= ~MFLAG_TEMP;  /* only hit once per projection (e.g. Storm, Gravity, Pulverise might move mon) */
    mon->project_amt = 0;       /* raw projection amount for debugging */
    mon->project_dam = 0;       /* actual damage sustained for riding and device lore */
    mon->project_notice = FALSE;/* affected by this projection? e.g. GF_TURN_UNDEAD might not apply */
}
static void _begin(dun_ptr dun, int gf)
{
    if (!_monsters) _monsters = vec_alloc(NULL);
    else vec_clear(_monsters);
    _path_visual = FALSE;
    _blast_visual = FALSE;
    _gf = gf;
    int_map_iter(dun->mon, (int_map_iter_f)_unmark);
    /* reset plr projection tracking info */
    plr->pflag &= ~PFLAG_TEMP;/* only hit once per projection (e.g. Kavlax BR_NEXUS) */
    plr->project_dam = 0;     /* actual damage sustained for riding */
}
static void _plr_begin(dun_ptr dun, int gf)
{
    _begin(dun, gf);
    monster_target = plr->pos; /* XXX cf gf_affect_m ... this is for mirror-master */
}
static int _mount_rakuba_dam(mon_ptr mon)
{
    int dam = mon->project_dam;
    if (mon->hp > mon->maxhp/3) dam = (dam + 1) / 2;
    return MIN(200, dam);
}
static int _plr_rakuba_dam(void)
{
    return MIN(200, plr->project_dam);
}
static void _riding_end(dun_ptr dun)
{
    mon_ptr mount;

    if (!plr->riding) return;
    if (plr->dun_id != dun->id) return;

    mount = plr_riding_mon();
    assert(mount);
    if (mount->project_dam && rakuba(_mount_rakuba_dam(mount), FALSE))
    {
        char name[MAX_NLEN_MON];
        monster_desc(name, mount, 0);
        msg_format("%^s has thrown you off!", name);
    }
    else if (plr->project_dam && rakuba(_plr_rakuba_dam(), FALSE))
    {
        char name[MAX_NLEN_MON];
        monster_desc(name, mount, 0);
        msg_format("You have fallen from %s.", name);
    }
}
static void _end(dun_ptr dun)
{
    _riding_end(dun); /* before clearing projection tracking fields */
    vec_clear(_monsters);
    int_map_iter(dun->mon, (int_map_iter_f)_unmark);
    plr->pflag &= ~PFLAG_TEMP;
    plr->project_dam = 0;
}
static void _track(mon_ptr mon)
{
    if (!mon->ml) return;
    if (!plr_tim_find(T_HALLUCINATE)) mon_track(mon);
    health_track(mon);
}
void _virtue(int gf)
{
    switch (gf)
    {
    case GF_TURN_UNDEAD: 
        virtue_add(VIRTUE_UNLIFE, -1);
        break;
    case GF_DISP_UNDEAD: 
        virtue_add(VIRTUE_UNLIFE, -2);
        break;
    case GF_OLD_POLY:
        virtue_add(VIRTUE_CHANCE, 1);
        break;
    }
}
static void _plr_end(dun_ptr dun)
{
    if (_mon_notice())
        _virtue(_gf);
    if (vec_length(_monsters) == 1)
        _track(vec_get(_monsters, 0));
    _end(dun);
}
static void _device_end(dun_ptr dun, int dam)
{
    /* Hack: Handle device lore for offensive effects. We could do this on
     * a case by case basis in do_effect(), but this is easier. Erroneously
     * setting device_lore is harmless. */
    if (_max_mon_dam() >= dam) /* && one_in_(?) */
        device_lore = TRUE;
    _plr_end(dun);
}
/* affect features, objects, monsters and player */
static int _reduce_dam(int dam, int distance)
{
    return (dam + distance) / (distance + 1);
}
static int _reduce_dam_p(int dam, int distance)
{          /* OLD: 100, 50, 33, 25, 20, 16, 14, 12 */
    int pct[8] = { 100, 80, 60, 50, 44, 37, 33, 30 };
    return (dam * pct[MIN(7, distance)] + 50) / 100;
}
static int _gf_affect_flags(who_t who) {
    if (who_is_trap(who)) return GF_AFFECT_TRAP;
    return GF_AFFECT_SPELL | GF_AFFECT_PROJECT;
}
static bool _blast_f(dun_blast_ptr blast, dun_blast_point_t pos)
{
    int dam = blast->dam;
    if (!(blast->flags & PROJECT_FULL_DAM)) /* XXX */
        dam = _reduce_dam(dam, pos.distance);
    return gf_affect_f(blast->who, pos.pos, blast->gf, dam, _gf_affect_flags(blast->who));
}
static bool _blast_o(dun_blast_ptr blast, dun_blast_point_t pos)
{
    int dam = blast->dam;
    if (!(blast->flags & PROJECT_FULL_DAM)) /* XXX */
        dam = _reduce_dam(dam, pos.distance);
    return gf_affect_o(blast->who, pos.pos, blast->gf, dam, _gf_affect_flags(blast->who));
}
static bool _affect_m(mon_ptr mon, who_t who)
{
    if (mon == who_mon(who)) return FALSE;
    if (who_is_plr(who) && mon->id == plr->riding) return FALSE;
    return TRUE;
}
static bool _blast_m(dun_blast_ptr blast, dun_blast_point_t pos)
{
    mon_ptr mon = dun_mon_at(blast->dun, pos.pos);
    int dam = blast->dam;
    if (!mon) return FALSE;
    if (!_affect_m(mon, blast->who)) return FALSE;
    if (mon->mflag & MFLAG_TEMP) return FALSE;
    mon->mflag |= MFLAG_TEMP;
    if (!(blast->flags & PROJECT_FULL_DAM)) /* XXX */
        dam = _reduce_dam(dam, pos.distance);
    assert(_monsters);
    mon->project_amt = dam; /* debugging */
    vec_push(_monsters, mon); /* tracking keeps PW_MONSTER up to date, etc */
    return gf_affect_m(blast->who, mon, blast->gf, dam, _gf_affect_flags(blast->who));
}
static bool _kawarimi(int dam)
{
    if (!(plr->special_defense & NINJA_KAWARIMI)) return FALSE;
    if (!dam) return FALSE;
    if (randint1(55) > plr->lev*3/5 + 20) return FALSE;
    return kawarimi(TRUE);
}
static bool _affect_p(who_t who)
{
    if (who_is_plr(who)) return FALSE;
    if (who_is_mon_id(who, plr->riding)) return FALSE;
    return TRUE;
}
static bool _blast_p(dun_blast_ptr blast, dun_blast_point_t pos)
{
    int dam = blast->dam;
    int ouch;
    mon_ptr caster = who_mon(blast->who);

    /* preamble: check for plr and don't hit him twice */
    if (!dun_plr_at(blast->dun, pos.pos)) return FALSE;
    if (!_affect_p(blast->who)) return FALSE;
    if (plr->pflag & PFLAG_TEMP) return FALSE;
    plr->pflag |= PFLAG_TEMP;

    /* class specific before damage mod */
    if (_kawarimi(dam)) return FALSE;
    if (caster && plr->pclass == CLASS_RAGE_MAGE)
        rage_mage_spell_reaction(caster);
    if (plr->pclass == CLASS_RUNE_KNIGHT && caster)
        dam = rune_knight_absorption(blast->gf, dam);
    if (plr->pclass == CLASS_RAGE_MAGE && mon_spell_current() && caster)
    {
        blast->turned = rage_mage_spell_turning(caster);
        if (blast->turned) return TRUE; /* no effect on plr */
    }

    /* calculate effective damage */
    if (!(blast->flags & PROJECT_FULL_DAM)) /* XXX */
        dam = _reduce_dam_p(dam, pos.distance);

    /* class specific with effective damage before effect */
    if (mon_spell_current())
    {
        if (mon_spell_current()->spell->flags & MSF_INNATE)
        {
            bool evaded = FALSE; /* Demigod Scout with Evasion talent *and* Nimble Dodge cast? */

            if (plr->nimble_dodge)
            {
                int odds = 7 * plr->open_terrain_ct;
                if (randint0(100) < odds)
                {
                    msg_print("You nimbly dodge the attack!");
                    dam = 0;
                    evaded = TRUE;
                }
            }

            if (!evaded && mut_present(MUT_EVASION))
            {
                msg_print("You evade the attack!");
                dam -= dam * (10 + randint1(10))/100;
            }
        }
        else if (plr->pclass != CLASS_RUNE_KNIGHT && plr->magic_resistance)
        {
            dam -= dam * plr->magic_resistance / 100;
        }
    }
    if (plr->pclass == CLASS_DUELIST && who_equals(blast->who, plr->duelist_target))
        dam -= dam/3;
    if (psion_drain())
        dam = psion_do_drain(dam);

    /* actually do the effect */
    ouch = gf_affect_p(blast->who, blast->gf, dam, _gf_affect_flags(blast->who));
    plr->project_dam = ouch;

    /* class specific after the effect */
    hex_on_dam(ouch);
    if (ouch > 0 && !plr->is_dead && caster)
        weaponmaster_do_readied_shot(caster);

    if (plr->revenge && ouch > 0 && !plr->is_dead && caster)
    {
        char m_name[80];
        char m_name_self[80];
        monster_desc(m_name, caster, 0);
        monster_desc(m_name_self, caster, MD_PRON_VISIBLE | MD_POSSESSIVE | MD_OBJECTIVE);
        msg_format("The attack of %s has wounded %s!", m_name, m_name_self);
        gf_affect_m(who_create_plr(), caster, GF_MISSILE, psion_backlash_dam(ouch), 0);
        plr_tim_subtract(T_REVENGE, 5);
    }

    if (caster && ouch && plr->pclass == CLASS_RAGE_MAGE)
        rage_mage_armor_of_fury(caster, ouch);

    if ((plr->special_defense & NINJA_KAWARIMI) && dam && caster && caster->id != plr->riding)
        kawarimi(FALSE);

    /* disturb and notice */
    disturb(1, 0);
    return TRUE;
}
static bool _blast_kill(dun_blast_ptr blast, dun_blast_point_t pos) /* m and/or p; handle riding */
{
    mon_ptr mon = dun_mon_at(blast->dun, pos.pos);
    bool notice = FALSE;

    /* effect both plr and mount. since this effect is not aimed, reduce power somewhat */
    if (mon && mon->id == plr->riding)
        pos.distance++;

    if (_blast_m(blast, pos)) notice = TRUE;
    if (_blast_p(blast, pos)) notice = TRUE;
    return notice;
}

/* animation */
typedef struct {
    char c;
    byte a;
} _ui_char_t, *_ui_char_ptr;
static _ui_char_t _bolt_char(point_t v, int gf)
{
    u16b p = bolt_pict(0, 0, v.y, v.x, gf);
    _ui_char_t result;
    result.c = PICT_C(p);
    result.a = PICT_A(p);
    return result;
}
static _ui_char_t _blast_char(int gf)
{
    return _bolt_char(point_create(0, 0), gf);
}
static void _animate_aux(point_t p, point_t v, int gf, u32b flags)
{
    point_t uip = cave_pt_to_ui_pt(p);
    rect_t  map = ui_map_rect();

    if (rect_contains_point(map, uip) && plr_view(p))
    {
        _ui_char_t c = _bolt_char(v, gf);
        if (!msg_line_contains(uip.y, uip.x))
            Term_queue_bigchar(uip.x, uip.y, c.a, c.c, 0, 0);
        Term_gotoxy(uip.x, uip.y);
        Term_fresh();
        Term_xtra(TERM_XTRA_DELAY, delay_animation);
        draw_pos(p);
        if (flags & PROJECT_BEAM)
        {
            c = _blast_char(gf);
            if (!msg_line_contains(uip.y, uip.x))
                Term_queue_bigchar(uip.x, uip.y, c.a, c.c, 0, 0);
        }
        _path_visual = TRUE;
    }
    else if (_path_visual)
    {
        Term_xtra(TERM_XTRA_DELAY, delay_animation);
    }
}
static bool _hide = FALSE;
static bool _show_animation(dun_ptr dun, int gf)
{
    gf_info_ptr gfi;
    if (dun->id != plr->dun_id) return FALSE;
    if (_hide) return FALSE;
    if (plr_tim_find(T_BLIND)) return FALSE;
    gfi = gf_lookup(gf);
    return !(gfi->flags & GFF_HIDE);
}
static void _animate_path(dun_path_ptr path, int gf)
{
    if (_show_animation(path->dun, gf))
    {
        int i;
        point_t last = path->start;
        _path_visual = FALSE;
        for (i = 0; i < path->count; i++)
        {
            point_t p = path->points[i];
            point_t v = point_subtract(p, last);
            _animate_aux(p, v, gf, path->flags);
            last = p;
        }
    }
}
static void _animate_blast(dun_blast_ptr blast)
{
    if (_show_animation(blast->dun, blast->gf))
    {
        rect_t  map = ui_map_rect();
        int i, r;
        _blast_visual = FALSE;
        for (r = 0; r <= blast->radius; r++)
        {
            point_t uip;
            for (i = blast->offsets[r]; i < blast->offsets[r+1]; i++)
            {
                dun_blast_point_t p = blast->points[i];
                uip = cave_pt_to_ui_pt(p.pos);
                if (rect_contains_point(map, uip) && plr_view(p.pos))
                {
                    _ui_char_t c = _blast_char(blast->gf);
                    if (!msg_line_contains(uip.y, uip.x))
                        Term_queue_bigchar(uip.x, uip.y, c.a, c.c, 0, 0);
                    _blast_visual = TRUE;
                }
            }
            uip = cave_pt_to_ui_pt(blast->center);
            Term_gotoxy(uip.x, uip.y);
            Term_fresh();
            if (_path_visual || _blast_visual)
                Term_xtra(TERM_XTRA_DELAY, delay_animation);
        }
        if (_blast_visual)
        {
            point_t uip;
            for (i = 0; i < blast->count; i++)
            {
                dun_blast_point_t p = blast->points[i];
                uip = cave_pt_to_ui_pt(p.pos);
                if (rect_contains_point(map, uip) && plr_view(p.pos))
                    draw_pos(p.pos);
            }
            uip = cave_pt_to_ui_pt(blast->center);
            Term_gotoxy(uip.x, uip.y);
            Term_fresh();
        }
    }
}
static bool _path_hits_plr(dun_path_ptr path)
{
    int i;
    for (i = 0; i < path->count; i++) /* bolt or beam */
    {
        point_t p = path->points[i];
        if (point_equals(p, plr->pos)) return TRUE;
    }
    return FALSE;
}
static bool _spell_aimed_at_plr(void)
{
    mon_spell_cast_ptr cast = mon_spell_current();
    if (cast && (cast->flags & MSC_DEST_PLAYER))
        return TRUE;
    return FALSE;
}
static bool _spell_aimed_at_mount(void)
{
    mon_spell_cast_ptr cast = mon_spell_current();
    if (cast && (cast->flags & MSC_DEST_MOUNT))
        return TRUE;
    return FALSE;
}
/* a bolt/beam or MST_BALL0 spell (e.g. Brain Smash) is an aimed
 * effect and should hit either the plr or his mount, but not both. */
static void _aim_riding_plr(dun_path_ptr path, who_t who)
{
    if (path->dun->id != plr->dun_id) return;
    if (!plr->riding) return;
    if (who_is_mon_id(who, plr->riding)) return;
    if (!_path_hits_plr(path)) return;

    /* use targetting decision from mon_spell ai if available */
    if (_spell_aimed_at_mount())
        plr->pflag |= PFLAG_TEMP;  /* skip plr */
    else if (_spell_aimed_at_plr())
    {
        mon_ptr mount = plr_riding_mon();
        mount->mflag |= MFLAG_TEMP;  /* skip mount */
    }
    /* no targetting decision: randomly choose plr or mount */
    else if (one_in_(2)) /* aim at plr */
    {
        mon_ptr mount = plr_riding_mon();
        mount->mflag |= MFLAG_TEMP; /* skip mount */
    }
    else /* aim at mount */
        plr->pflag |= PFLAG_TEMP; /* skip plr */
}
/*************************************************************************
 * bolt implementation
 *************************************************************************/
static bool _star_dust_hack;
static bool _reflection(dun_path_ptr path, who_t who, int gf, int dam);
static bool _bolt_aux(dun_ptr dun, who_t who, point_t source, point_t target, int gf, int dam, int range)
{
    dun_path_ptr path;
    dun_blast_ptr blast;
    bool notice = FALSE;

    /* path from source to target, stopping on first hit monster. bolts
     * keep going until they hit something within the range. */
    path = dun_path_alloc_aux(dun, source, target, PROJECT_STOP | PROJECT_THRU, range);
    _aim_riding_plr(path, who);
    if (!_star_dust_hack) _animate_path(path, gf);
    if (_reflection(path, who, gf, dam))
    {
        dun_path_free(path);
        return FALSE;
    }

    /* blast the target */
    blast = dun_blast_ball(dun, path->stop, 0, gf);
    blast->who = who;
    blast->dam = dam;

    _animate_blast(blast);

    dun_blast_iter(blast, _blast_f); /* XXX */
    dun_blast_iter(blast, _blast_m);
    dun_blast_iter(blast, _blast_p);
    if (blast->notice) notice = TRUE;

    if (blast->turned)
        _bolt_aux(path->dun, who_create_null(), path->stop, path->start, gf, dam, range);

    /* cleanup */
    dun_path_free(path);
    dun_blast_free(blast);

    return notice;
}
static bool _bolt(dun_ptr dun, who_t who, point_t source, point_t target, int gf, int dam)
{
    return _bolt_aux(dun, who, source, target, gf, dam, DUN_PATH_MAX);
}
/* reflection */
static point_t _reflection_target(dun_path_ptr path)
{
    point_t p;
    int max_attempts = 10;

    while (max_attempts--)
    {
        p = point_random_jump(path->start, 2);
        if (!dun_pos_valid(path->dun, p)) continue;
        if (!dun_project(path->dun, path->stop, p)) continue;
        break;
    }
    if (max_attempts < 0)
        p = path->start;
    return p;
}
static bool _plr_reflect(void)
{
    if ( (plr->reflect || ((plr->special_defense & KATA_FUUJIN) && !plr_tim_find(T_BLIND)))
      && !one_in_(4))
    {
        return TRUE;
    }
    return FALSE;
}
static bool _can_reflect(int gf)
{
    gf_info_ptr gfi = gf_lookup(gf);
    return !(gfi->flags & GFF_NO_REFLECT);
}
static bool _reflection(dun_path_ptr path, who_t who, int gf, int dam)
{
    mon_ptr mon;
    if (!_can_reflect(gf)) return FALSE;
    mon = dun_mon_at(path->dun, path->stop);
    if (mon && mon_can_reflect(mon) && _affect_m(mon, who))
    {
        bool reflect = TRUE;
        if (path->count <= 1) reflect = FALSE;
        if (one_in_(4)) reflect = FALSE;
        if (reflect)
        {
            point_t p = _reflection_target(path);

            if (mon_show_msg(mon))
                msg_print("The attack bounces!");
            mon_lore_can_reflect(mon);

            if (gf != GF_ATTACK)
                _bolt(path->dun, who_create_null(), path->stop, p, gf, dam);

            return TRUE;
        }
    }
    if (dun_plr_at(path->dun, path->stop) && _plr_reflect() && _affect_p(who))
    {
        point_t p = _reflection_target(path);

        if (plr_tim_find(T_BLIND)) msg_print("Something bounces!");
        else msg_print("The attack bounces!");

        equip_learn_flag(OF_REFLECT);
        mon = dun_mon_at(path->dun, path->start);
        if (mon) mon_smart_learn(mon, SM_REFLECTION);

        if (gf != GF_ATTACK)
            _bolt(path->dun, who_create_null(), path->stop, p, gf, dam);

        return TRUE;
    }
    return FALSE;
}
/*************************************************************************
 * bolt interface
 *************************************************************************/
bool plr_bolt_or_beam(point_t target, int gf, int dam, int beam_chance)
{
    if (randint0(100) < beam_chance)
        return plr_beam(target, gf, dam);
    return plr_bolt(target, gf, dam);
}
bool plr_bolt(point_t target, int gf, int dam)
{
    return plr_bolt_aux(target, gf, dam, DUN_PATH_MAX);
}
bool plr_bolt_aux(point_t target, int gf, int dam, int range)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _bolt_aux(dun, who_create_plr(), plr->pos, target, gf, dam, range);
    _plr_end(dun);
    return notice;
}
bool plr_star_dust(int count, int spread, point_t target, int gf, dice_t dice)
{
    bool notice = FALSE;
    dun_ptr dun = plr_dun();
    int i;
    /* big hack: fire count reflectable gf bolts from plr to scattered
     * points about target. do *not* animate the paths, just the blasts.
     * cf plr_cast_star_dust for how to compute target from "dir" */
    _star_dust_hack = TRUE;
    for (i = 0; i < count; i++)
    {
        point_t p;
        for (;;)
        {
            p = point_random_jump(target, spread);
            if (point_fast_distance(target, p) <= spread) break;
        }
        _plr_begin(dun, gf);
        if (_bolt_aux(dun, who_create_plr(), plr->pos, p, gf, dice_roll(dice), DUN_PATH_MAX))
            notice = TRUE;
        _plr_end(dun);
    }
    _star_dust_hack = FALSE;
    return notice;
}
bool device_bolt(point_t target, int gf, int dam)
{
    return device_bolt_aux(target, gf, dam, DUN_PATH_MAX);
}
bool device_bolt_aux(point_t target, int gf, int dam, int range)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _bolt_aux(dun, who_create_plr(), plr->pos, target, gf, dam, range);
    _device_end(dun, dam);
    return notice;
}
bool mon_bolt(mon_ptr mon, point_t target, int gf, int dam)
{
    return mon_bolt_aux(mon, target, gf, dam, DUN_PATH_MAX);
}
bool mon_bolt_aux(mon_ptr mon, point_t target, int gf, int dam, int range)
{
    dun_ptr dun = mon->dun;
    bool notice;
    _begin(dun, gf);
    notice = _bolt_aux(dun, who_create_mon(mon), mon->pos, target, gf, dam, range);
    _end(dun);
    return notice;
}
/*************************************************************************
 * ball implementation
 *************************************************************************/
/* Note: projection paths include points before testing whether they
 * stop the projection. This allows plr to target ghosts in walls,
 * for example. But we need to shorten the path so that balls explode
 * *before* the point that stops projection. */
static bool _gf_stop_project(dun_ptr dun, point_t pos, int gf)
{
    if (gf == GF_LIGHT || gf == GF_LIGHT_WEAK || gf == GF_DARK)
        return !dun_allow_los_at(dun, pos);
    else if (gf == GF_DISINTEGRATE)
        return dun_stop_disintegration_at(dun, pos);
    return !dun_allow_project_at(dun, pos);
}
void dun_path_fix(dun_path_ptr path, int gf)
{
    if (!path->count) return;
    if (_gf_stop_project(path->dun, path->stop, gf))
    {
        path->count--;
        if (!path->count) path->stop = path->start;
        else path->stop = path->points[path->count - 1];
    }
}
static bool _ball_aux(dun_ptr dun, who_t who, int rad, point_t source, point_t target, int gf, int dam, int range)
{
    dun_path_ptr path;
    dun_blast_ptr blast;
    bool aimed = (rad == 0);
    bool notice = FALSE;

    /* path from source to target ignoring intervening monsters */
    path = dun_path_alloc_aux(dun, source, target, 0, range);
    if (aimed) _aim_riding_plr(path, who); /* MSF_BALL0 ... e.g. Brain Smash */
    else dun_path_fix(path, gf); /* explode before hitting walls */
    _animate_path(path, gf);

    /* blast the target */
    blast = dun_blast_ball(dun, path->stop, rad, gf);
    blast->who = who;
    blast->dam = dam;

    _animate_blast(blast);

    dun_blast_iter(blast, _blast_f);
    dun_blast_iter(blast, _blast_o);
    if (aimed)
    {
        dun_blast_iter(blast, _blast_m);
        dun_blast_iter(blast, _blast_p);
    }
    else
        dun_blast_iter(blast, _blast_kill);

    if (blast->notice) notice = TRUE;
    if (blast->turned)
        _ball_aux(path->dun, who_create_null(), rad, path->stop, path->start, gf, dam, range);

    /* cleanup */
    dun_path_free(path);
    dun_blast_free(blast);

    return notice;
}
/* XXX do a "ball" without the "path" */
static bool _ball_aux2(dun_ptr dun, who_t who, int rad, point_t target, int gf, int dam)
{
    dun_blast_ptr blast;
    bool notice = FALSE;

    blast = dun_blast_ball(dun, target, rad, gf);
    blast->who = who;
    blast->dam = dam;

    _animate_blast(blast);

    dun_blast_iter(blast, _blast_f);
    dun_blast_iter(blast, _blast_o);
    dun_blast_iter(blast, _blast_kill);
    if (blast->notice) notice = TRUE;

    dun_blast_free(blast);
    return notice;
}
/*************************************************************************
 * ball interface
 *************************************************************************/
bool plr_ball(int rad, point_t target, int gf, int dam)
{
    if (gf == GF_ROCKET)
        return plr_rocket(rad, target, gf, dam);
    return plr_ball_aux(rad, target, gf, dam, DUN_PATH_MAX);
}
bool plr_ball_hide(int rad, point_t target, int gf, int dam)
{
    bool notice;
    _hide = TRUE;
    notice = plr_ball(rad, target, gf, dam);
    _hide = FALSE;
    return notice;
}
bool plr_ball_direct(int rad, point_t target, int gf, int dam)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _ball_aux2(dun, who_create_plr(), rad, target, gf, dam);
    _plr_end(dun);
    return notice;
}
bool plr_ball_aux(int rad, point_t target, int gf, int dam, int range)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _ball_aux(dun, who_create_plr(), rad, plr->pos, target, gf, dam, range);
    _plr_end(dun);
    return notice;
}
bool device_ball(int rad, point_t target, int gf, int dam)
{
    if (gf == GF_ROCKET)
        return device_rocket(rad, target, gf, dam);
    return device_ball_aux(rad, target, gf, dam, DUN_PATH_MAX); 
}
bool device_ball_aux(int rad, point_t target, int gf, int dam, int range)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _ball_aux(dun, who_create_plr(), rad, plr->pos, target, gf, dam, range);
    _device_end(dun, dam);
    return notice;
}
bool mon_ball(mon_ptr mon, int rad, point_t target, int gf, int dam)
{
    if (gf == GF_ROCKET)
        return mon_rocket(mon, rad, target, gf, dam);
    return mon_ball_aux(mon, rad, target, gf, dam, DUN_PATH_MAX);
}
bool mon_ball_aux(mon_ptr mon, int rad, point_t target, int gf, int dam, int range)
{
    dun_ptr dun = mon->dun;
    bool notice;
    _begin(dun, gf);
    notice = _ball_aux(dun, who_create_mon(mon), rad, mon->pos, target, gf, dam, range);
    _end(dun);
    return notice;
}
bool plr_meteor_shower(int count, int rad, int gf, dice_t dice)
{
    bool notice = FALSE;
    dun_ptr dun = plr_dun();
    int i, j, d;
    for (i = 0; i < count; i++)
    {
        point_t p;
        /* XXX keeping the number of attempts low prevents casting in tight
         * terrain from doing too much damage. */
        for (j = 0; j < 20; j++)
        {
            p = point_random_jump(plr->pos, 8);
            d = point_fast_distance(plr->pos, p);
            if (!dun_pos_interior(dun, p)) continue;
            if (d > 8) continue;
            if (!plr_project(p)) continue;
            if (!cell_project(dun_grid_at(dun, p))) continue;
            break;
        }
        if (count == 20) continue;
        /* each meteor counts as a separate "projection". this allows a single
         * monster to be hit by, and hurt by, multiple strikes. each meteor simply
         * falls from the sky ... it is not projected from the plr to the target.*/
        _plr_begin(dun, gf);
        if (_ball_aux2(dun, who_create_plr(), rad, p, gf, dice_roll(dice)))
            notice = TRUE;
        _plr_end(dun);
    }
    return notice;
}
/*************************************************************************
 * breath implementation
 *************************************************************************/
static u32b _gf_flags(int gf) /* bolts and beams only */
{
    if (gf == GF_LIGHT || gf == GF_LIGHT_WEAK || gf == GF_DARK)
        return PROJECT_LOS;
    else if (gf == GF_DISINTEGRATE)
        return PROJECT_DISI;
    return 0;
}
static bool _breath_aux(dun_ptr dun, who_t who, int rad, point_t source, point_t target, int gf, int dam, int range)
{
    dun_path_ptr path;
    dun_blast_ptr blast;
    bool notice = FALSE;

    /* path from source to target ignoring intervening monsters. do not animate */
    path = dun_path_alloc_aux(dun, source, target, _gf_flags(gf), range);
    dun_path_fix(path, gf); /* explode before hitting walls */

    /* blast the target */
    if (!path->count) blast = dun_blast_ball(dun, source, rad, gf); /* XXX adjacent mon; plr on mountain (or in wall) */
    else blast = dun_blast_breath(path, rad, gf);
    blast->who = who;
    blast->dam = dam;

    _animate_blast(blast); /* this animates the path as well */

    dun_blast_iter(blast, _blast_f);
    dun_blast_iter(blast, _blast_o);
    dun_blast_iter(blast, _blast_kill);
    if (blast->notice) notice = TRUE;

    if (blast->turned)
        _breath_aux(path->dun, who_create_null(), rad, path->stop, path->start, gf, dam, range);

    /* cleanup */
    dun_path_free(path);
    dun_blast_free(blast);

    return notice;
}
/*************************************************************************
 * breath interface
 *************************************************************************/
bool plr_breath(int rad, point_t target, int gf, int dam)
{
    return plr_breath_aux(rad, target, gf, dam, DUN_PATH_MAX);
}
bool plr_breath_aux(int rad, point_t target, int gf, int dam, int range)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    stop_mouth();
    notice = _breath_aux(dun, who_create_plr(), rad, plr->pos, target, gf, dam, range);
    _plr_end(dun);
    return notice;
}
bool device_breath(int rad, point_t target, int gf, int dam)
{
    return device_breath_aux(rad, target, gf, dam, DUN_PATH_MAX); 
}
bool device_breath_aux(int rad, point_t target, int gf, int dam, int range)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _breath_aux(dun, who_create_plr(), rad, plr->pos, target, gf, dam, range);
    _device_end(dun, dam);
    return notice;
}
bool mon_breath(mon_ptr mon, int rad, point_t target, int gf, int dam)
{
    return mon_breath_aux(mon, rad, target, gf, dam, DUN_PATH_MAX);
}
bool mon_breath_aux(mon_ptr mon, int rad, point_t target, int gf, int dam, int range)
{
    dun_ptr dun = mon->dun;
    bool notice;
    _begin(dun, gf);
    notice = _breath_aux(dun, who_create_mon(mon), rad, mon->pos, target, gf, dam, range);
    _end(dun);
    return notice;
}
/*************************************************************************
 * beam implementation
 *************************************************************************/
static void _erase_path(dun_path_ptr path)
{
    int i;
    point_t uip;
    rect_t  map = ui_map_rect();
    for (i = 0; i < path->count; i++)
    {
        point_t p = path->points[i];
        uip = cave_pt_to_ui_pt(p);
        if (rect_contains_point(map, uip) && plr_view(p))
            draw_pos(p);
    }
    uip = cave_pt_to_ui_pt(path->stop);
    Term_gotoxy(uip.x, uip.y);
    Term_fresh();
}
static bool _beam_aux(dun_ptr dun, who_t who, point_t source, point_t target, int gf, int dam, int range)
{
    dun_path_ptr path;
    dun_blast_ptr blast;
    bool notice = FALSE;

    /* path from source thru target to max range or wall */
    path = dun_path_alloc_aux(dun, source, target, _gf_flags(gf) | PROJECT_BEAM | PROJECT_THRU, range);
    _aim_riding_plr(path, who);
    _animate_path(path, gf); /* this animates the blast as well */

    /* blast the target */
    blast = dun_blast_beam(path, gf);
    blast->who = who;
    blast->dam = dam;

    if (_path_visual) /* cleanup animation */
        _erase_path(path);

    dun_blast_iter(blast, _blast_f);
    dun_blast_iter(blast, _blast_o);
    dun_blast_iter(blast, _blast_m);
    dun_blast_iter(blast, _blast_p);
    if (blast->notice) notice = TRUE;

    if (blast->turned)
        _beam_aux(path->dun, who_create_null(), path->stop, path->start, gf, dam, range);

    /* cleanup */
    dun_path_free(path);
    dun_blast_free(blast);

    return notice;
}
/*************************************************************************
 * beam interface
 *************************************************************************/
bool plr_beam(point_t target, int gf, int dam)
{
    return plr_beam_aux(target, gf, dam, DUN_PATH_MAX);
}
bool plr_beam_aux(point_t target, int gf, int dam, int range)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _beam_aux(dun, who_create_plr(), plr->pos, target, gf, dam, range);
    _plr_end(dun);
    return notice;
}
bool device_beam(point_t target, int gf, int dam)
{
    return device_beam_aux(target, gf, dam, DUN_PATH_MAX);
}
bool device_beam_aux(point_t target, int gf, int dam, int range)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _beam_aux(dun, who_create_plr(), plr->pos, target, gf, dam, range);
    _device_end(dun, dam);
    return notice;
}
bool mon_beam(mon_ptr mon, point_t target, int gf, int dam)
{
    return mon_beam_aux(mon, target, gf, dam, DUN_PATH_MAX);
}
bool mon_beam_aux(mon_ptr mon, point_t target, int gf, int dam, int range)
{
    dun_ptr dun = mon->dun;
    bool notice;
    _begin(dun, gf);
    notice = _beam_aux(dun, who_create_mon(mon), mon->pos, target, gf, dam, range);
    _end(dun);
    return notice;
}
/*************************************************************************
 * rocket implementation
 *************************************************************************/
static bool _rocket_aux(dun_ptr dun, who_t who, int rad, point_t source, point_t target, int gf, int dam)
{
    dun_path_ptr path;
    dun_blast_ptr blast;
    bool notice = FALSE;

    /* non-reflectable "bolt" from src to target */
    path = dun_path_alloc_aux(dun, source, target, PROJECT_STOP | PROJECT_THRU, DUN_PATH_MAX);
    dun_path_fix(path, gf);
    _animate_path(path, gf);

    /* blast the target */
    blast = dun_blast_ball(dun, path->stop, rad, gf);
    blast->who = who;
    blast->dam = dam;

    _animate_blast(blast);

    dun_blast_iter(blast, _blast_f);
    dun_blast_iter(blast, _blast_o);
    dun_blast_iter(blast, _blast_kill);
    if (blast->notice) notice = TRUE;

    if (blast->turned)
        _rocket_aux(path->dun, who_create_null(), rad, path->stop, path->start, gf, dam);

    /* cleanup */
    dun_path_free(path);
    dun_blast_free(blast);

    return notice;
}
/*************************************************************************
 * rocket interface
 *************************************************************************/
bool plr_rocket(int rad, point_t target, int gf, int dam)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, GF_ROCKET);
    notice = _rocket_aux(dun, who_create_plr(), rad, plr->pos, target, gf, dam);
    _plr_end(dun);
    return notice;
}
bool device_rocket(int rad, point_t target, int gf, int dam)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, GF_ROCKET);
    notice = _rocket_aux(dun, who_create_plr(), rad, plr->pos, target, gf, dam);
    _device_end(dun, dam);
    return notice;
}
bool mon_rocket(mon_ptr mon, int rad, point_t target, int gf, int dam)
{
    dun_ptr dun = mon->dun;
    bool notice;
    _begin(dun, GF_ROCKET);
    notice = _rocket_aux(dun, who_create_mon(mon), rad, mon->pos, target, gf, dam);
    _end(dun);
    return notice;
}
bool plr_star_ball(int count, int gf, dice_t dice)
{
    bool notice = FALSE;
    dun_ptr dun = plr_dun();
    int i;
    for (i = 0; i < count; i++)
    {
        int attempts = 100;
        point_t pos;

        /* pick random point around plr */
        while (attempts--)
        {
            pos = dun_scatter(dun, plr->pos, 4);
            /* XXX This makes long corridor casting extremely OP
            if (!dun_allow_project_at(dun, pos)) continue; */
            if (!dun_plr_at(dun, pos)) break;
        }
        if (attempts < 0) continue;

        /* then PROJECT_THRU to find target (PROJECT_STOP) ... "rocket" is doing what we want */
        _plr_begin(dun, gf);
        if (_rocket_aux(dun, who_create_plr(), 3, plr->pos, pos, gf, dice_roll(dice)))
            notice = TRUE;
        _plr_end(dun);
    }
    return notice;
}
bool plr_star_light(int count, int gf, dice_t dice)
{
    bool notice = FALSE;
    dun_ptr dun = plr_dun();
    int i;
    for (i = 0; i < count; i++)
    {
        int attempts = 100;
        point_t pos;

        while (attempts--)
        {
            pos = dun_scatter(dun, plr->pos, 4);
            /* XXX This makes long corridor casting extremely OP
            if (!dun_allow_project_at(dun, pos)) continue; */
            if (!dun_plr_at(dun, pos)) break;
        }
        if (attempts < 0) continue;

        _plr_begin(dun, gf);
        if (_beam_aux(dun, who_create_plr(), plr->pos, pos, gf, dice_roll(dice), DUN_PATH_MAX))
            notice = TRUE;
        _plr_end(dun);
    }
    return notice;
}
/*************************************************************************
 * burst implementation: a "burst" is a ball w/o any path. damage scaling
 * is tweaked so that adjacent squares take full damage (cf _ball_aux2 if
 * you don't want this!). "bursts" usually do not affect the "target".
 * (e.g. _Starburst does a plr_burst(5, GF_LIGHT) that doesn't hurt plr)
 *************************************************************************/
static bool _burst_aux(dun_ptr dun, who_t who, int rad, point_t target, int gf, int dam)
{
    bool notice = FALSE;
    dun_blast_ptr blast = dun_blast_burst(dun, target, rad, gf);
    blast->who = who;
    blast->dam = dam;

    _animate_blast(blast);

    dun_blast_iter(blast, _blast_f);
    dun_blast_iter(blast, _blast_o);
    dun_blast_iter(blast, _blast_kill);
    if (blast->notice) notice = TRUE;

    dun_blast_free(blast);
    return notice;
}
/*************************************************************************
 * burst interface (note the automatic damage scaling in dun_blast_burst)
 *************************************************************************/
bool plr_burst(int rad, int gf, int dam)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _burst_aux(dun, who_create_plr(), rad, plr->pos, gf, dam);
    _plr_end(dun);
    return notice;
}
bool plr_burst_hide(int rad, int gf, int dam)
{
    bool notice;
    _hide = TRUE;
    notice = plr_burst(rad, gf, dam);
    _hide = FALSE;
    return notice;
}
bool device_burst(int rad, int gf, int dam)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _burst_aux(dun, who_create_plr(), rad, plr->pos, gf, dam);
    _device_end(dun, dam);
    return notice;
}
bool mon_burst(mon_ptr mon, int rad, int gf, int dam)
{
    dun_ptr dun = mon->dun;
    bool notice;
    _begin(dun, gf);
    notice = _burst_aux(dun, who_create_mon(mon), rad, mon->pos, gf, dam);
    _end(dun);
    return notice;
}
bool dun_burst(dun_ptr dun, who_t who, int rad, point_t pos, int gf, int dam)
{
    bool notice;
    if (who_is_plr(who)) _plr_begin(dun, gf);
    else _begin(dun, gf);
    notice = _burst_aux(dun, who, rad, pos, gf, dam);
    if (who_is_plr(who)) _plr_end(dun);
    else _end(dun);
    return notice;
}
/*************************************************************************
 * mon ai helpers: pets avoid hurting plr, etc
 *************************************************************************/
bool mon_clean_bolt(mon_ptr mon, point_t target)
{
    return mon_clean_bolt_aux(mon, target, DUN_PATH_MAX);
}
bool mon_clean_bolt_aux(mon_ptr mon, point_t target, int range)
{
    dun_ptr dun = mon->dun;
    dun_path_ptr path;
    bool ok;
    int i;
    bool pet = mon_is_pet(mon);
    bool friend = mon_is_friendly(mon);

    if (point_equals(mon->pos, target)) return FALSE; /* sanity check */

    path = dun_path_alloc_aux(dun, mon->pos, target, PROJECT_STOP, range);
    ok = point_equals(path->stop, target);
    for (i = 0; ok && i < path->count - 1; i++) /* no need to check last point in path */
    {
        point_t p = path->points[i];
        mon_ptr mon2 = dun_mon_at(dun, p);

        if (pet)
        {
            if (dun_plr_at(dun, p)) ok = FALSE; /* pets avoid master */
            else if (mon2 && mon_is_pet(mon2)) ok = FALSE; /* pets avoid other pets */
        }
        else
        {
            if (friend && dun_plr_at(dun, p)) ok = FALSE; /* friends avoid plr (new) */
            else if (mon2 && !are_enemies(mon, mon2)) ok = FALSE; /* non-pets only hit enemies (new) */
        }
    }
    dun_path_free(path);
    return ok;
}
bool mon_clean_beam(mon_ptr mon, point_t target)
{
    return mon_clean_beam_aux(mon, target, GF_MISSILE, DUN_PATH_MAX);
}
bool mon_clean_beam_aux(mon_ptr mon, point_t target, int gf, int range)
{
    dun_ptr dun = mon->dun;
    dun_path_ptr path;
    bool hit_target = FALSE;
    bool ok = TRUE;
    int i;
    bool pet = mon_is_pet(mon);
    bool friend = mon_is_friendly(mon);

    if (point_equals(mon->pos, target)) return FALSE; /* sanity check */

    path = dun_path_alloc_aux(dun, mon->pos, target, _gf_flags(gf) | PROJECT_BEAM | PROJECT_THRU, range);
    for (i = 0; ok && i < path->count; i++)
    {
        point_t p = path->points[i];
        mon_ptr mon2 = dun_mon_at(dun, p);

        if (point_equals(p, target)) hit_target = TRUE;
        if (pet)
        {
            if (dun_plr_at(dun, p)) ok = FALSE; /* pets avoid master */
            else if (mon2 && mon_is_pet(mon2)) ok = FALSE; /* pets avoid other pets */
        }
        else
        {
            if (friend && dun_plr_at(dun, p)) ok = FALSE; /* friends avoid plr (new) */
            /* XXX don't mind collateral damage as long as beam hits target (new) XXX
             * else if (mon2 && !are_enemies(mon, mon2)) ok = FALSE; */
        }
    }
    dun_path_free(path);
    return ok && hit_target;
}
bool mon_clean_breath(mon_ptr mon, point_t target, int rad, int gf)
{
    dun_ptr dun = mon->dun;
    dun_path_ptr path;
    dun_blast_ptr blast;
    bool hit_target = FALSE;
    bool ok = TRUE;
    int i;
    bool pet = mon_is_pet(mon);
    bool friend = mon_is_friendly(mon);

    path = dun_path_alloc(dun, mon->pos, target, _gf_flags(gf));
    dun_path_fix(path, gf);
    if (!path->count) blast = dun_blast_ball(dun, mon->pos, rad, gf); /* XXX adjacent mon; plr on mountain (or in wall) */
    else blast = dun_blast_breath(path, rad, gf);

    /* note: breaths are huge area effects, so we don't mind a bit of collateral damage */
    for (i = 0; ok && i < blast->count; i++)
    {
        dun_blast_point_t p = blast->points[i];
        /*mon_ptr mon2 = dun_mon_at(dun, p);*/

        if (point_equals(p.pos, target)) hit_target = TRUE;
        if (pet)
        {
            if (dun_plr_at(dun, p.pos)) ok = FALSE; /* pets avoid master */
            /* XXX Pets don't mind collateral damage on other pets XXX 
             * else if (mon2 && mon_is_pet(mon2)) ok = FALSE; */
        }
        else
        {
            if (friend && dun_plr_at(dun, p.pos)) ok = FALSE; /* friends avoid plr (new) */
            /* XXX don't mind collateral damage as long as breath hits target XXX
             * else if (mon2 && !are_enemies(mon, mon2)) ok = FALSE; */
        }
    }

    dun_path_free(path);
    dun_blast_free(blast);
    return ok && hit_target;
}
/*************************************************************************
 * los implementation: XXX usage of project vs los is historical XXX
 *************************************************************************/
static who_t _who;
static bool _los_mon_p(mon_ptr mon)
{
    point_t pos;

    if (!_affect_m(mon, _who)) return FALSE;

    if (who_is_mon(_who))
        return mon_project(who_mon(_who), mon->pos);

    if (who_is_plr(_who))
    {
        if (!plr_view(mon->pos)) return FALSE;
        return plr_project_mon(mon);
    }

    pos = who_pos(_who);
    if (dun_pos_interior(mon->dun, pos))
        return dun_project(mon->dun, pos, mon->pos);

    return FALSE; /* XXX */
}
static bool _los_aux(dun_ptr dun, who_t who, int gf, int dam)
{
    vec_ptr v;
    bool    notice = FALSE;
    int     i;

    /* affect monsters */
    _who = who;
    v = dun_filter_mon(dun, _los_mon_p);
    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        if (!mon_is_valid(mon)) continue; /* killed by an exploding monster already processed */
        if (gf_affect_m(who, mon, gf, dam, GF_AFFECT_SPELL))
            notice = TRUE;
    }
    vec_free(v);

    /* affect plr */
    if (_affect_p(who) && who_is_mon(who) && mon_project_plr(who_mon(who)))
    {
        if (gf_affect_p(who, gf, dam, GF_AFFECT_SPELL))
            notice = TRUE;
    }

    return notice;
}

/*************************************************************************
 * los
 *************************************************************************/
bool plr_project_los(int gf, int dam)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _los_aux(dun, who_create_plr(), gf, dam);
    _plr_end(dun);
    return notice;
}
bool device_project_los(int gf, int dam)
{
    dun_ptr dun = plr_dun();
    bool notice;
    _plr_begin(dun, gf);
    notice = _los_aux(dun, who_create_plr(), gf, dam); 
    _device_end(dun, dam);
    return notice;
}
bool mon_project_los(mon_ptr mon, int gf, int dam)
{
    dun_ptr dun = mon->dun;
    bool notice;
    _begin(dun, gf);
    notice = _los_aux(dun, who_create_mon(mon), gf, dam);
    _end(dun);
    return notice;
}
/*************************************************************************
 * wrath of god
 * Each ball has a 23.6% chance of hitting target: 1.1%, 9.0%, 13.5% (R=0,1,2)
 *************************************************************************/
#ifdef DEVELOPER
/* 100 dam per ball vs player (so divide by 100 to get "multipliers"):
   Wrath of God: 248.82 +- 127.66 720
   Wrath of God: 254.48 +- 123.54 780
   Wrath of God: 253.34 +- 122.07 700
   Wrath of God: 253.88 +- 122.05 680
   Wrath of God: 250.66 +- 124.66 740
   Wrath of God: 257.14 +- 125.83 800
   Wrath of God: 248.68 +- 123.17 840
   Wrath of God: 249.00 +- 122.63 740
   Wrath of God: 252.34 +- 123.27 840

   Using the last line:
          1.29x   2.52x   3.76x   8.40x
 ==>50       65     126     188     420
    60       77     151     225     504
    70       90     177     263     588
    80      103     202     300     672
    90      116     227     338     756
    100     129     252     376     840
    110     142     278     413     924
    120     155     303     451    1008

    50 or 60 seems a good choice for monster dam per ball.
    Note that 2 sigma to down side is no damage at all!
    Note the max up to 5 sigma!

   Now for Player (Crusade):
    Wrath of God: 149.78 +- 74.86 404
    Wrath of God: 149.87 +- 75.24 452
    Wrath of God: 149.82 +- 73.54 420
    Wrath of God: 148.23 +- 70.80 402
    Wrath of God: 144.83 +- 74.32 470
    Wrath of God: 150.30 +- 75.33 438
    Wrath of God: 148.59 +- 75.90 454

           0.73x   1.49x   2.24x   4.54x
    150     109     223     337     681
 ==>175     127     260     393     795<== Hengband
    200     145     297     449     908

    cf _reduce_dam and _reduce_dam_p for explanation of
    multiplier differences.
*/
int _wrath_of_god_stats_aux(dun_ptr dun, point_t target)
{
    int count = 10 + _1d(10);
    int dam = 0;
    int i;
    for (i = 0; i < count; i++)
    {
        int attempt = 20, d;
        point_t p;
        while (attempt--)
        {
            p = point_random_jump(target, 5);
            if (point_fast_distance(target, p) <= 5) break;
        }
        if (attempt < 0) continue;
        if (!dun_pos_interior(dun, p)) continue;
        if (_gf_stop_project(dun, p, GF_DISINTEGRATE)) continue;
        if (!_gf_project(dun, target, p, GF_DISINTEGRATE)) continue;
        d = point_fast_distance(p, target);
        if (d <= 2)
            dam += _reduce_dam_p(100, d);
    }
    return dam;
}
int_stat_t wrath_of_god_stats(dun_ptr dun, point_t target)
{
    vec_ptr v = vec_alloc(NULL);
    int_stat_t stats = {0};
    int i, ct = 1000;
    for (i = 0; i < ct; i++)
        vec_add_int(v, _wrath_of_god_stats_aux(dun, target));
    stats = int_calc_stats(v);
    vec_free(v);
    return stats;
}
#endif
static bool _wrath_of_god_aux(dun_ptr dun, who_t who, point_t source, point_t target, int gf, dice_t dice)
{
    bool notice = FALSE;
    int i;
    int count = 10 + _1d(10);
    int spread = 5;
    int rad = 2;

    if (point_equals(source, target))
    {
        /* XXX e.g. Azriel stuck in a web ... I like allowing this
         * case for a dramatic escape! cf _ai_stuck in mon_spell.c XXX */
    }
    /* fix target (cf dun_path_fix) */
    else if (!dun_allow_project_at(dun, target)) /* e.g. plr in wall|web|tree */
    {
        dun_path_ptr path = dun_path_alloc(dun, source, target, PROJECT_STOP);
        assert(path->count); /* target == source*/
        path->count--;
        if (!path->count) target = path->start;
        else target = path->points[path->count - 1];
        dun_path_free(path);
    }
    /* blast balls around target: each blast is a separate projection event */
    for (i = 0; i < count; i++)
    {
        int attempt = 20;
        point_t p;
        while (attempt--)
        {
            p = point_random_jump(target, spread);
            if (point_fast_distance(target, p) <= spread) break;
        }
        if (attempt < 0) continue;
        if (!dun_pos_interior(dun, p)) continue;
        if (_gf_stop_project(dun, p, gf)) continue; /* note: _gf_project=TRUE if last point stops the projection (cf dun_path_fix) */
        if (!_gf_project(dun, target, p, gf)) continue;
        if (who_is_plr(who)) _plr_begin(dun, gf);
        else _begin(dun, gf);
        if (_ball_aux2(dun, who, rad, p, gf, dice_roll(dice)))
            notice = TRUE;
        if (who_is_plr(who)) _plr_end(dun);
        else _end(dun);
    }
    return notice;
}
bool mon_wrath_of_god(mon_ptr mon, point_t target, int gf, dice_t dice)
{
    return _wrath_of_god_aux(mon->dun, who_create_mon(mon), mon->pos, target, gf, dice);
}
bool plr_wrath_of_god(point_t target, int gf, dice_t dice)
{
    return _wrath_of_god_aux(plr_dun(), who_create_plr(), plr->pos, target, gf, dice);
}
/*************************************************************************
 * mirror master
 *************************************************************************/
static bool dun_grid_is_mirror(dun_grid_ptr grid)
{
    return floor_has_mirror(grid);
}
static point_t _current_mirror;
static bool _next_mirror_p(point_t pos, dun_grid_ptr grid)
{
    if (point_equals(pos, _current_mirror)) return FALSE;
    if (!floor_has_mirror(grid)) return FALSE;
    return point_project(pos, _current_mirror);
}
static point_t dun_next_mirror(dun_ptr dun, point_t current)
{
    point_vec_ptr pts;
    point_t pos = {0};
    
    _current_mirror = current;
    pts = dun_filter_grids(dun, _next_mirror_p);

    if (point_vec_length(pts))
        pos = point_vec_get(pts, randint0(point_vec_length(pts)));
    else
    {
        for (;;)
        {
            pos = point_random_jump(current, 2);
            if (dun_pos_interior(dun, pos)) break;
        }
    }
    point_vec_free(pts);
    return pos;
}
/* mirror master "beams" stop on first hit mirror at which point they are reflected or split */
static dun_path_ptr _mirror_path(dun_ptr dun, point_t source, point_t target)
{
    dun_path_ptr path = dun_path_alloc(dun, source, target, PROJECT_BEAM | PROJECT_THRU);
    int i;
    for (i = 0; i < path->count; i++)
    {
        point_t p = path->points[i];
        dun_grid_ptr g = dun_grid_at(dun, p);
        if (dun_grid_is_mirror(g))
        {
            path->stop = p;
            path->count = i + 1;
            break;
        }
    }
    return path;
}
static bool _seeker_aux(dun_ptr dun, point_t source, point_t target, int dam)
{
    dun_path_ptr path = _mirror_path(dun, source, target);
    dun_blast_ptr blast;
    bool notice = FALSE;

    _begin(dun, GF_SEEKER);
    monster_target = source;
    _animate_path(path, GF_SEEKER); /* this animates the blast as well */
    if (_path_visual) /* cleanup animation */
        _erase_path(path);

    blast = dun_blast_beam(path, GF_SEEKER);
    blast->who = who_create_plr();
    blast->dam = dam;

    dun_blast_iter(blast, _blast_f);
    dun_blast_iter(blast, _blast_o);
    dun_blast_iter(blast, _blast_m);
    _plr_end(dun);
    if (blast->notice) notice = TRUE;

    /* seek out another mirror */
    if (dun_grid_is_mirror(dun_grid_at(dun, path->stop)))
    {
        point_t next;
        dun_remove_mirror(dun, path->stop); /* remove first to prevent cycles */
        next = dun_next_mirror(dun, path->stop);
        if (dun_pos_interior(dun, next))
        {
            if (_seeker_aux(dun, path->stop, next, dam)) /* recurse */
                notice = TRUE;
        }
    }

    /* cleanup */
    dun_path_free(path);
    dun_blast_free(blast);

    return notice;
}
bool plr_seeker_ray(point_t target, int dam)
{
    dun_ptr dun = plr_dun();
    return _seeker_aux(dun, plr->pos, target, dam);
}
static bool _super_aux(dun_ptr dun, point_t source, point_t target, int dam)
{
    dun_path_ptr path = _mirror_path(dun, source, target);
    dun_blast_ptr blast;
    bool notice = FALSE;

    _plr_begin(dun, GF_SUPER_RAY); 
    _animate_path(path, GF_SUPER_RAY); /* this animates the blast as well */
    if (_path_visual) /* cleanup animation */
        _erase_path(path);

    blast = dun_blast_beam(path, GF_SUPER_RAY);
    blast->who = who_create_plr();
    blast->dam = dam;

    dun_blast_iter(blast, _blast_f);
    dun_blast_iter(blast, _blast_o);
    dun_blast_iter(blast, _blast_m);
    _plr_end(dun);
    if (blast->notice) notice = TRUE;

    if (dun_grid_is_mirror(dun_grid_at(dun, path->stop)))
    {
        int i;
        dun_remove_mirror(dun, path->stop);
        for (i = 0; i < 8; i++)
        {
            point_t p = point_step(path->stop, cdd[i]);
            /* each _ray is a separate projection event */
            _begin(dun, GF_SUPER_RAY);
            monster_target = path->stop;
            if (_beam_aux(dun, who_create_plr(), path->stop, p, GF_SUPER_RAY, dam, DUN_PATH_MAX))
                notice = TRUE;
            _plr_end(dun);
        }
    }

    /* cleanup */
    dun_path_free(path);
    dun_blast_free(blast);

    return notice;
}
bool plr_super_ray(point_t target, int dam)
{
    dun_ptr dun = plr_dun();
    return _super_aux(dun, plr->pos, target, dam);
}
static triangle_t _field_triangle(dun_ptr dun)
{
    point_vec_ptr pts;
    triangle_t    t = {{0}};
    
    _current_mirror = plr->pos;
    pts = dun_filter_grids(dun, _next_mirror_p);

    if (point_vec_length(pts) >= 2)
    {
        point_vec_shuffle(pts);
        t.a = plr->pos;
        t.b = point_vec_get(pts, 0);
        t.c = point_vec_get(pts, 1);
    }
    point_vec_free(pts);
    return t;
}
bool plr_binding_field(int dam)
{
    bool    notice = FALSE;
    dun_ptr dun = plr_dun();
    triangle_t t = _field_triangle(dun);

    _plr_begin(dun, GF_MANA);
    if (triangle_is_valid(t))
    {
        dun_blast_ptr blast = dun_blast_triangle(dun, t, GF_MANA);
        blast->who = who_create_plr();
        blast->dam = dam;

        _animate_blast(blast);
        dun_blast_iter(blast, _blast_f);
        dun_blast_iter(blast, _blast_o);
        dun_blast_iter(blast, _blast_m);
        /*if (blast->notice) notice = TRUE;*/
        if (blast->count)
        {
            notice = TRUE;
            if (one_in_(7) && !plr->wizard)
            {
                msg_print("The field broke a mirror");
                dun_remove_mirror(dun, t.c);
            }
        }
        dun_blast_free(blast);
    }
    _plr_end(dun);
    return notice;
}
