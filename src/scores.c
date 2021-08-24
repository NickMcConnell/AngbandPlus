#include "angband.h"

#include <assert.h>

/************************************************************************
 * Utilities
 ************************************************************************/
char *_str_copy_aux(cptr s)
{
    char *copy = malloc(strlen(s) + 1);
    strcpy(copy, s);
    return copy;
}
char *_str_copy(cptr s)
{
    if (!s) return NULL;
    return _str_copy_aux(s);
}
char *_timestamp(void)
{
    time_t  now = time(NULL);
    char    buf[20];
    strftime(buf, 20, "%Y-%m-%d", localtime(&now));
    return _str_copy(buf);
}
char *_version(void)
{
    char buf[20];
    sprintf(buf, "%d.%d.%d", VER_MAJOR, VER_MINOR, VER_PATCH);
    return _str_copy(buf);
}
static char *_status(void)
{
    cptr status = 
        plr->total_winner ? "Winner" : (plr->is_dead ? "Dead" : "Alive");
    return _str_copy(status);
}
static char *_killer(void)
{
    if (plr->is_dead)
        return _str_copy(plr->died_from);
    return NULL;
}

/************************************************************************
 * Score Entries
 ************************************************************************/
score_ptr score_alloc(void)
{
    score_ptr score = malloc(sizeof(score_t));
    memset(score, 0, sizeof(score_t));
    return score;
}

score_ptr score_current(void)
{
    score_ptr       score = score_alloc();
    race_t         *race = get_true_race();
    class_t        *class_ = get_class();
    personality_ptr personality = get_personality();

    score->id = plr->id;
    score->uid = player_uid;
    score->date = _timestamp();
    score->version = _version();
    score->score = plr->max_max_exp;  /* XXX */

    score->name = _str_copy(player_name);
    score->race = _str_copy(race->name);
    score->subrace = _str_copy(race->subname);
    score->class_ = _str_copy(class_->name);
    score->subclass = _str_copy(class_->subname);

    score->sex = _str_copy(plr->psex == SEX_MALE ? "Male" : "Female");
    score->personality = _str_copy(personality->name);

    score->gold = plr->au;
    score->turns = plr->turn;
    score->clvl = plr->lev;
    score->dlvl = cave->dun_lvl;
    score->dungeon = _str_copy(map_name());
    score->killer = _killer();
    score->status = _status();

    score->exp = plr->exp;
    score->max_depth = plr_max_dun_lvl();
    score->fame = plr->fame;

    return score;
}

static int _parse_int(vec_ptr v, int i)
{
    if (i < vec_length(v))
    {
        str_ptr s = vec_get(v, i);
        return atoi(str_buffer(s));
    }
    return 0;
}
static char *_parse_string(vec_ptr v, int i)
{
    if (i < vec_length(v))
    {
        str_ptr s = vec_get(v, i);
        return _str_copy(str_buffer(s));
    }
    return NULL;
}
#define _FIELD_COUNT 22
static score_ptr score_read(FILE *fp)
{
    score_ptr  score = NULL;
    str_ptr line = str_alloc();
    vec_ptr    fields;

    str_read_line(line, fp);
    fields = str_split(line, '|');
    str_free(line);
    if (vec_length(fields))
    {
        score = score_alloc();
        score->id = _parse_int(fields, 0);
        score->uid = _parse_int(fields, 1);
        score->date = _parse_string(fields, 2);
        score->version = _parse_string(fields, 3);
        score->score = _parse_int(fields, 4);

        score->name = _parse_string(fields, 5);
        score->race = _parse_string(fields, 6);
        score->subrace = _parse_string(fields, 7);
        score->class_ = _parse_string(fields, 8);
        score->subclass = _parse_string(fields, 9);

        score->sex = _parse_string(fields, 10);
        score->personality = _parse_string(fields, 11);

        score->gold = _parse_int(fields, 12);
        score->turns = _parse_int(fields, 13);
        score->clvl = _parse_int(fields, 14);
        score->dlvl = _parse_int(fields, 15);
        score->dungeon = _parse_string(fields, 16);
        score->killer = _parse_string(fields, 17);
        score->status = _parse_string(fields, 18);

        score->exp = _parse_int(fields, 19);
        score->max_depth = _parse_int(fields, 20);
        score->fame = _parse_int(fields, 21);
    }
    vec_free(fields);
    return score;
}

static cptr _opt(cptr s) { return s ? s : ""; }
static void score_write(score_ptr score, FILE* fp)
{
    str_ptr line = str_alloc();

    str_printf(line, "%d|%d|%s|%s|%d|",
        score->id, score->uid, score->date, score->version, score->score);

    str_printf(line, "%s|%s|%s|%s|%s|",
        score->name, score->race, _opt(score->subrace), score->class_, _opt(score->subclass));

    str_printf(line, "%s|%s|%d|%d|%d|%d|%s|%s|%s|",
        score->sex, score->personality, score->gold, score->turns,
        score->clvl, score->dlvl, _opt(score->dungeon), _opt(score->killer),
        _opt(score->status));

    str_printf(line, "%d|%d|%d\n", score->exp, score->max_depth, score->fame);

    fputs(str_buffer(line), fp);
    str_free(line);
}

void score_free(score_ptr score)
{
    if (score)
    {
        if (score->date) free(score->date);
        if (score->version) free(score->version);
        if (score->name) free(score->name);
        if (score->race) free(score->race);
        if (score->subrace) free(score->subrace);
        if (score->class_) free(score->class_);
        if (score->subclass) free(score->subclass);
        if (score->sex) free(score->sex);
        if (score->personality) free(score->personality);
        if (score->dungeon) free(score->dungeon);
        if (score->killer) free(score->killer);
        if (score->status) free(score->status);
        free(score);
    }
}

static int score_cmp_id(score_ptr l, score_ptr r)
{
    if (l->id < r->id) return 1;
    if (l->id > r->id) return -1;
    return 0;
}
static int score_cmp_score(score_ptr l, score_ptr r)
{
    if (l->score < r->score) return 1;
    if (l->score > r->score) return -1;
    return score_cmp_id(l, r);
}
static int score_cmp_date(score_ptr l, score_ptr r)
{
    int result = -strcmp(l->date, r->date);
    if (!result)
        result = score_cmp_id(l, r); /* XXX multiple deaths on same day XXX */
    return result;
}

static int score_cmp_race(score_ptr l, score_ptr r)
{
    int result = strcmp(l->race, r->race);
    if (!result)
        result = score_cmp_score(l, r);
    return result;
}

static int score_cmp_class(score_ptr l, score_ptr r)
{
    int result = strcmp(l->class_, r->class_);
    if (!result)
        result = score_cmp_score(l, r);
    return result;
}

static int score_cmp_name(score_ptr l, score_ptr r)
{
    int result = strcmp(l->name, r->name);
    if (!result)
        result = score_cmp_score(l, r);
    return result;
}

static bool score_is_winner(score_ptr score)
{
    return score->status && strcmp(score->status, "Winner") == 0;
}
static bool score_is_dead(score_ptr score)
{
    return score->status && strcmp(score->status, "Dead") == 0;
}

/************************************************************************
 * Scores File (scores.txt)
 ************************************************************************/
static FILE *_scores_fopen(cptr name, cptr mode)
{
    char buf[1024];
    path_build(buf, sizeof(buf), ANGBAND_DIR_SCORES, name);
    return my_fopen(buf, mode);
}

static FILE *_scores_html_fopen(cptr name, cptr mode)
{
    char buf1[1024], buf2[1024];
    path_build(buf1, sizeof(buf1), ANGBAND_DIR_SCORES, "html");
    path_build(buf2, sizeof(buf2), buf1, name);
    return my_fopen(buf2, mode);
}

vec_ptr scores_load(score_p filter)
{
    FILE   *fp = _scores_fopen("scores.txt", "r");
    vec_ptr v = vec_alloc((vec_free_f)score_free);

    if (fp)
    {
        for (;;)
        {
            score_ptr score = score_read(fp);
            if (!score) break;
            if (filter && !filter(score))
            {
                score_free(score);
                continue;
            }
            vec_add(v, score);
        }
        fclose(fp);
    }
    vec_sort(v, (vec_cmp_f)score_cmp_score);
    return v;
}

void scores_save(vec_ptr scores)
{
    int i;
    FILE *fp = _scores_fopen("scores.txt", "w");

    if (!fp)
    {
        msg_print("<color:v>Error:</color> Unable to open scores.txt");
        return;
    }
    vec_sort(scores, (vec_cmp_f)score_cmp_score);
    for (i = 0; i < vec_length(scores); i++)
    {
        score_ptr score = vec_get(scores, i);
        score_write(score, fp);
    }
    fclose(fp);
}

int scores_next_id(void)
{
    FILE *fp = _scores_fopen("next", "r");
    int   next;

    if (!fp)
    {
        fp = _scores_fopen("next", "w");
        if (!fp)
        {
            msg_print("<color:v>Error:</color> Unable to open next file in scores directory.");
            return 1;
        }
        fputc('2', fp);
        fclose(fp);
        return 1;
    }
   
    if (1 != fscanf(fp, "%d", &next)) next = 1;
    fclose(fp);
    fp = _scores_fopen("next", "w");
    fprintf(fp, "%d", next + 1);
    fclose(fp);
    return next;
}

void scores_update(void)
{
    int       i;
    vec_ptr   scores = scores_load(NULL);
    score_ptr current = score_current();
    FILE     *fp;
    char      name[100];

    for (i = 0; i < vec_length(scores); i++)
    {
        score_ptr score = vec_get(scores, i);
        if (score->id == current->id)
        {
            vec_set(scores, i, current);
            break;
        }
    }
    if (i == vec_length(scores))
        vec_add(scores, current);
    scores_save(scores);
    vec_free(scores); /* current is now in scores[] and need not be freed */

    sprintf(name, "dump%d.doc", plr->id);
    fp = _scores_fopen(name, "w");
    if (fp)
    {
        doc_ptr doc = doc_alloc(80);
        plr_display_character_sheet(doc);
        doc_write_file(doc, fp, DOC_FORMAT_DOC);
        doc_free(doc);
        fclose(fp);
    } 
}

/************************************************************************
 * User Interface
 ************************************************************************/
static void _display(doc_ptr doc, vec_ptr scores, int top, int page_size)
{
    int i, j;
    doc_clear(doc);
    doc_insert(doc, "<style:table>");
    #ifdef NO_SCORES
    doc_insert(doc, "<tab:32><color:R>High Score Listing (CLOSED)</color>\n");
    #else
    doc_insert(doc, "<tab:32><color:R>High Score Listing</color>\n");
    #endif
    doc_insert(doc, "<color:G>    <color:keypress>N</color>ame            "
        "CL <color:keypress>R</color>ace         "
        "<color:keypress>C</color>lass            "
        "<color:keypress>S</color>core Rank <color:keypress>D</color>ate       "
        "Status</color>\n");
    for (i = 0; i < page_size; i++)
    {
        score_ptr score;
        j = top + i;
        if (j >= vec_length(scores))
        {
            doc_newline(doc); /* for scrolling */
            continue;
        }
        score = vec_get(scores, j);
        if (score->id == plr->id)
            doc_insert(doc, "<color:B>");
        doc_printf(doc, " <color:y>%c</color>) %-15.15s", I2A(i), score->name);
        doc_printf(doc, " %2d %-12.12s %-13.13s", score->clvl, score->race, score->class_);
        doc_printf(doc, " %8d %4d", score->score, j + 1);
        doc_printf(doc, " %s", score->date);
        if (score_is_winner(score))
            doc_insert(doc, " <color:v>Winner</color>");
        else if (score_is_dead(score))
            doc_insert(doc, " <color:r>Dead</color>");
        else
            doc_insert(doc, " Alive");
        if (score->id == plr->id)
            doc_insert(doc, "</color>");
        doc_newline(doc);
    }
    doc_insert(doc, "</style>");
    doc_insert(doc, "\n <color:U>Press corresponding letter to view last character sheet.</color>\n");
    doc_insert(doc, " <color:U>Press <color:keypress>|</color> to export to html. Press <color:keypress>^N</color> to sort by Name, etc.</color>\n");
    if (page_size < vec_length(scores))
        doc_insert(doc, " <color:U>Press <color:keypress>PageUp</color>, <color:keypress>PageDown</color>, <color:keypress>Up</color> and <color:keypress>Down</color> to scroll.</color>\n");
    doc_sync_menu(doc);
}
/* Generating html dumps from the scores directory will omit the html header
 * attributes necessary for posting to angband.cz.oook. I don't want to store
 * html dumps in scores for space considerations and I insist on being able to view
 * the dumps from within the game (so we store .doc formats). Try to patch things
 * up to keep oook happy, but this is untested. */
static void _add_html_header(score_ptr score, doc_ptr doc)
{
    str_ptr header = str_alloc();

    str_append_s(header, "<head>\n");
    str_append_s(header, " <meta name='filetype' value='character dump'>\n");
    str_printf(header,  " <meta name='variant' value='%s'>\n", VERSION_NAME); /* never changes */
    str_printf(header,  " <meta name='variant_version' value='%s'>\n", score->version);
    str_printf(header,  " <meta name='character_name' value='%s'>\n", score->name);
    str_printf(header,  " <meta name='race' value='%s'>\n", score->race);
    str_printf(header,  " <meta name='class' value='%s'>\n", score->class_);
    str_printf(header,  " <meta name='level' value='%d'>\n", score->clvl);
    str_printf(header,  " <meta name='experience' value='%d'>\n", score->exp);
    str_printf(header,  " <meta name='turncount' value='%d'>\n", score->turns);
    str_printf(header,  " <meta name='max_depth' value='%d'>\n", score->max_depth);
    str_printf(header,  " <meta name='score' value='%d'>\n", score->score);
    str_printf(header,  " <meta name='fame' value='%d'>\n", score->fame);

    { /* XXX Is oook case sensitive? */
        char status[100];
        sprintf(status, "%s", score->status);
        status[0] = tolower(status[0]);
        str_printf(header,  " <meta name='status' value='%s'>\n", status);
    }

    /* XXX drop winner, dead and retired boolean fields. Hopefully oook relies on
     * the status field instead */

    if (score_is_dead(score))
        str_printf(header,  " <meta name='killer' value='%s'>\n", score->killer);
    str_append_s(header, "</head>");

    doc_change_html_header(doc, str_buffer(header));

    str_free(header);
}

static void _show_dump(score_ptr score)
{
    char  name[30];
    FILE *fp;

    sprintf(name, "dump%d.doc", score->id);
    fp = _scores_fopen(name, "r");
    if (fp)
    {
        doc_ptr doc = doc_alloc(80);
        doc_read_file(doc, fp);
        fclose(fp);
        _add_html_header(score, doc); /* in case the user pipes to html then posts to oook */
        doc_display(doc, name, 0);
        doc_free(doc);
        Term_clear();
    }
}

static void _export_dump(score_ptr score)
{
    char  name[30];
    FILE *fp;

    /* copy scores/dump<id>.doc to scores/html/dump<id>.html */
    sprintf(name, "dump%d.doc", score->id);
    fp = _scores_fopen(name, "r");
    if (fp)
    {
        doc_ptr doc = doc_alloc(80);
        doc_read_file(doc, fp);
        fclose(fp);

        _add_html_header(score, doc);
        sprintf(name, "dump%d.html", score->id);
        doc_change_name(doc, name);

        fp = _scores_html_fopen(name, "w");
        if (fp)
        {
            doc_write_file(doc, fp, DOC_FORMAT_HTML);
            fclose(fp);
        }

        doc_free(doc);
    }
}

static void _html_color(FILE *fp, byte a)
{
    fprintf(fp,
        "<font color=\"#%02x%02x%02x\">",
        angband_color_table[a][1],
        angband_color_table[a][2],
        angband_color_table[a][3]
    );
}
static void _export(vec_ptr scores)
{
    int i;
    FILE *fp = _scores_html_fopen("scores.html", "w");

    if (!fp) return;
    fprintf(fp, "<!DOCTYPE html>\n<html>\n");
    fprintf(fp, "<body text=\"#ffffff\" bgcolor=\"#000000\"><pre>\n");
    _html_color(fp, TERM_L_RED);
    fprintf(fp, "%-32.32sHigh Score Listing</font>\n", "");
    _html_color(fp, TERM_L_GREEN);
    fprintf(fp, "    Name            CL Race         Class            Score Rank Date       Status</font>\n");

    for (i = 0; i < vec_length(scores); i++)
    {
        score_ptr score = vec_get(scores, i);
        _export_dump(score);

        fprintf(fp, "    <a href=\"dump%d.html\">", score->id);
        _html_color(fp, TERM_L_UMBER);
        if (strlen(score->name) < 15)
        {
            int j, ct = 15 - strlen(score->name);
            fprintf(fp, "%s</font></a>", score->name);
            for (j = 0; j < ct; j++)
                fputc(' ', fp);
        }
        else
            fprintf(fp, "%-15.15s</font></a>", score->name);
        fprintf(fp, " %2d %-12.12s %-13.13s", score->clvl, score->race, score->class_);
        fprintf(fp, " %8d %4d", score->score, i + 1);
        fprintf(fp, " %s", score->date);
        if (score_is_winner(score))
        {
            _html_color(fp, TERM_VIOLET);
            fprintf(fp, " Winner</font>");
        }
        else if (score_is_dead(score))
        {
            _html_color(fp, TERM_RED);
            fprintf(fp, " Dead</font>");
        }
        else
            fprintf(fp, " Alive");
        fputc('\n', fp);
    }

    fprintf(fp, "</pre></body></html>\n");
    fclose(fp);
}

void scores_display(vec_ptr scores)
{
    doc_ptr   doc = doc_alloc(100);
    int       top = 0, cmd;
    int       page_size = ui_screen_rect().cy - 6;
    bool      done = FALSE;

    if (page_size > 26)
        page_size = 26;

    Term_clear();
    while (!done)
    {
        _display(doc, scores, top, page_size);
        cmd = inkey_special(TRUE);
        if (cmd == ESCAPE || cmd == 'Q') break;
        switch (cmd)
        {
        case ESCAPE: case 'Q':
            done = TRUE;
            break;
        case SKEY_PGDOWN: case '3': case ' ':
            if (top + page_size < vec_length(scores))
                top += page_size;
            break;
        case SKEY_PGUP: case '9': case '-':
            if (top >= page_size)
                top -= page_size;
            break;
        case SKEY_UP: case '8':
            if (top > 0)
                top--;
            break;
        case SKEY_DOWN: case '2':
            if (top + 1 < vec_length(scores))
                top++;
            break;
        case KTRL('S'):
            vec_sort(scores, (vec_cmp_f)score_cmp_score);
            top = 0;
            break;
        case KTRL('D'):
            vec_sort(scores, (vec_cmp_f)score_cmp_date);
            top = 0;
            break;
        case KTRL('R'):
            vec_sort(scores, (vec_cmp_f)score_cmp_race);
            top = 0;
            break;
        case KTRL('C'):
            vec_sort(scores, (vec_cmp_f)score_cmp_class);
            top = 0;
            break;
        case KTRL('N'):
            vec_sort(scores, (vec_cmp_f)score_cmp_name);
            top = 0;
            break;
        case '|':
            _export(scores);
            break;
        default:
            if (islower(cmd))
            {
                int j = top + A2I(cmd);
                if (0 <= j && j < vec_length(scores))
                    _show_dump(vec_get(scores, j));
            }
        }
    }
    Term_clear();
    doc_free(doc);
}

