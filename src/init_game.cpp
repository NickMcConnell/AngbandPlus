#include <src/init.h>
#include <src/npp.h>

#include <ctime>
#include <QMainWindow>
#include <QLabel>
#include <src/qt_mainwindow.h>
#include <src/utilities.h>
#include <src/player_scores.h>
#include <src/messages.h>
#include <QTime>
#include <QApplication>

// was init.2

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

/*
 * This file is used to initialize various variables and arrays for the
 * NPPAngband or NPPMoria game.
 *
 * Most of the arrays for Angband are built from "template" files in
 * the "lib/edit" directory.
 *
 * Warning -- the "ascii" file parsers use a minor hack to collectthe
 * name and text information in a single pass.  Thus, the game will not
 * be able to load any template file with more than 20K of names or 60K
 * of text, even though technically, up to 64K should be legal.
 */

// Functional pointer and variables used only in the init_xxx.files
// Specify which information to be parsed
int (*parse_which_info)(QString line_info);

/*
 * Error tracking
 */
int last_idx;
int prev_lev;
int shop_idx;
int owner_idx;
int cur_title;
int cur_equip;

// The file directories
QDir npp_dir_base;
QDir npp_dir_lib;
QDir npp_dir_bone;
QDir npp_dir_edit;
QDir npp_dir_help;
QDir npp_dir_icon;
QDir npp_dir_save;
QDir npp_dir_user;
QDir npp_dir_graf;


/*
 * Standard error message text
 */
static QString err_str[PARSE_ERROR_MAX] =
{
    NULL,
    "parse error",
    "invalid flag specification",
    "invalid number of items (0-99)",
    "invalid spell frequency",
    "invalid random value",
    "missing colon",
    "missing field",
    "missing record header",
    "non-sequential records",
    "value not a number",
    "unrecognized feature transition"
    "obsolete file",
    "value out of bounds",
    "out of memory",
    "too few entries",
    "too many entries",
    "undefined directive",
    "unrecognised blow",
    "unrecognised tval name",
    "unrecognised sval name",
    "vault too big",
    "too many allocations",
    "too many arguments",
    "too many non sequential quests",
    "name too long",
};

/* We are going to play NPPAngband*/
static void init_nppangband(void)
{
    z_info->max_level = 50;
    z_info->max_titles = z_info->max_level  / 5;
}

/* We are going to play NPPMoria*/
static void init_nppmoria(void)
{
    z_info->max_level = z_info->max_titles = PY_MAX_LEVEL_MORIA;
}

void quit_npp_games(QString message)
{
    QMessageBox msg_box;
    msg_box.setText(message);
    msg_box.setInformativeText(QString(QObject::tr("NPPGames will close now.")));
    msg_box.exec();

    exit(1);
}

/*
 * Display a parser error message.
 */
static void display_parse_error(QString filename, int err, QString buf, int error_line)
{

    QString add_on;


    //  Make a message box, display it, and then quit.
    add_on = (((err > 0) && (err < PARSE_ERROR_MAX)) ? err_str[err] : "unknown");

    pop_up_message_box(QString(QObject::tr("Error at line %1 of '%2.txt'. %3 ")) .arg(error_line) .arg(filename) .arg(buf));

    /* Force Quit */
    quit_npp_games(QObject::tr("exiting NPP Games"));
}

//Initialize the various directories used by NPP
void create_directories()
{

    npp_dir_base.setPath(QDir::currentPath());
    npp_dir_lib.setPath(QString(npp_dir_base.path() .append("/lib/")));

    //Things are arranged a little differently on OSX
#ifdef Q_OS_OSX
    {
        // Use this when creating a distribution bundle
        QString this_path = (QApplication::applicationDirPath());
        int cut_index = this_path.indexOf(QString("/MacOS"), 1, Qt::CaseSensitive);
        if (cut_index >=0) this_path.resize(cut_index);
        npp_dir_lib.setPath(QString(this_path .append("/Resources/lib/")));

        /* Use this on OSX when just coding and using the source downloaded from Github
        int cut_index = this_path.indexOf(QString("/NPPG"), 1, Qt::CaseSensitive);
        if (cut_index >=0) this_path.resize(cut_index);
        npp_dir_base.setPath(this_path);
        */
    }
#endif // Q_OS_OSX

    npp_dir_bone.setPath(QString(npp_dir_lib.path() .append("/bone/")));
    npp_dir_edit.setPath(QString(npp_dir_lib.path() .append("/edit/")));
    npp_dir_help.setPath(QString(npp_dir_lib.path() .append("/help/")));
    npp_dir_icon.setPath(QString(npp_dir_lib.path() .append("/icons/")));
    npp_dir_save.setPath(QString(npp_dir_lib.path() .append("/save/")));
    npp_dir_user.setPath(QString(npp_dir_lib.path() .append("/user/")));
    npp_dir_graf.setPath(QString(npp_dir_lib.path() .append("/graf/")));

    /*
     * Make sure the save and user directories exist.
     * mkdir does nothing and returns FALSE if the directories already exist.
     */
    (void)npp_dir_save.mkdir(npp_dir_save.path());
    (void)npp_dir_user.mkdir(npp_dir_user.path());
}


static int read_edit_file(QString file_name)
{
    QFile edit_file;

    int err = 0;

    int error_line = 0;

    // Keep track of the last idx we have read so we can point to the correct entry.
    last_idx = -1;

    file_name.append(".txt");

    /* Build the filename */
    edit_file.setFileName(QString("%1/%2" ) .arg(npp_dir_edit.path()) .arg(file_name));

    if (!edit_file.exists())
    {
        quit_npp_games(QString(QObject::tr("Cannot find '%1' file.")) .arg(file_name));
        return (1);
    }

    if (!edit_file.open(QIODevice::ReadOnly))
    {
        quit_npp_games(QString(QObject::tr("Cannot open '%1' file.")) .arg(file_name));
        return(1);
    }

    QTextStream reading(&edit_file);

    /* Parse */
    while (!reading.atEnd())
    {
        error_line++;

        QString file_line = reading.readLine();

        err = parse_which_info(file_line);

        if (err)
        {
             display_parse_error(file_name, err, file_line, error_line);
             //An error has occurrred.

             break;
        }
    }

    // close the file
    edit_file.close();

    return (err);
}



/*
 * Initialize the "z_info" array
 */
static int init_z_info(void)
{
    int err;

    QString file_name;

    z_info = C_ZNEW(1, maxima);

    if (game_mode == GAME_NPPANGBAND) file_name = "limits";
    else file_name = "m_limits";  /* game_mode == NPPMORIA */

    parse_which_info = parse_z_info;

    err = read_edit_file(file_name);

    return (err);
}

/*
 * Initialize the "preset" color array
 * This function should never fail, unless the table preset colors has
 * been set up incorrectly (which would cause a crash.
 *
 */
static int init_color_info(void)
{
    for (int i = 0; i < MAX_COLORS; i++)
    {
        // Load each color into the color array.  255 is the alpha consistency.
        defined_colors[i].setRgb(preset_colors[i].red, preset_colors[i].green, preset_colors[i].blue, 255);
    }

    for (u16b i = 0; i < N_ELEMENTS(tval_to_attr); i++) {
        tval_to_attr[i] = TERM_WHITE;
    }

    // Taken from pref file
    tval_to_attr[1] = TERM_WHITE;
    tval_to_attr[2] = TERM_WHITE;
    tval_to_attr[3] = TERM_WHITE;
    tval_to_attr[5] = TERM_SLATE;
    tval_to_attr[7] = TERM_SLATE;
    tval_to_attr[16] = TERM_L_UMBER;
    tval_to_attr[17] = TERM_L_UMBER;
    tval_to_attr[18] = TERM_L_UMBER;
    tval_to_attr[19] = TERM_UMBER;
    tval_to_attr[20] = TERM_SLATE;
    tval_to_attr[21] = TERM_WHITE;
    tval_to_attr[22] = TERM_WHITE;
    tval_to_attr[23] = TERM_WHITE;
    tval_to_attr[30] = TERM_L_UMBER;
    tval_to_attr[31] = TERM_L_UMBER;
    tval_to_attr[32] = TERM_L_UMBER;
    tval_to_attr[33] = TERM_L_UMBER;
    tval_to_attr[34] = TERM_L_UMBER;
    tval_to_attr[35] = TERM_L_UMBER;
    tval_to_attr[36] = TERM_SLATE;
    tval_to_attr[37] = TERM_SLATE;
    tval_to_attr[38] = TERM_SLATE;
    tval_to_attr[39] = TERM_YELLOW;
    tval_to_attr[40] = TERM_ORANGE;
    tval_to_attr[45] = TERM_RED;
    tval_to_attr[55] = TERM_L_UMBER;
    tval_to_attr[65] = TERM_GREEN;
    tval_to_attr[66] = TERM_VIOLET;
    tval_to_attr[70] = TERM_WHITE;
    tval_to_attr[75] = TERM_L_BLUE;
    tval_to_attr[77] = TERM_YELLOW;
    tval_to_attr[80] = TERM_L_UMBER;
    tval_to_attr[90] = TERM_L_RED;
    tval_to_attr[91] = TERM_BLUE;
    tval_to_attr[92] = TERM_L_GREEN;

    return (0);
}



/*
 * Initialize the "f_info" array
 */
static int init_f_info(void)
{
    int err;

    QString file_name;

    f_info = C_ZNEW(z_info->f_max, feature_type);

    file_name = "terrain";

    parse_which_info = parse_f_info;

    err = read_edit_file(file_name);

    return (err);
}


/*
 * Initialize the "k_info" array
 */
static int init_k_info(void)
{
    int err;

    QString file_name;

    k_info = C_ZNEW(z_info->k_max, object_kind);

    if (game_mode == GAME_NPPANGBAND) file_name = "object";
    else file_name = "m_object";  /* game_mode == NPPMORIA */

    parse_which_info = parse_k_info;

    err = read_edit_file(file_name);

    return (err);
}

/*
 * Initialize the "t_info" array
 */
static int init_t_info(void)
{
    int err;

    QString file_name;

    t_info = C_ZNEW(z_info->ghost_template_max, ghost_template);

    file_name = "player_ghost";

    parse_which_info = parse_t_info;

    err = read_edit_file(file_name);

    return (err);
}




/*
 * Initialize the "a_info" array
 */
static int init_a_info(void)
{
    int err;

    QString file_name;

    a_info = C_ZNEW(z_info->art_max, artifact_type);

    if (game_mode == GAME_NPPANGBAND) file_name = "artifact";
    else file_name = "m_artifact";  /* game_mode == NPPMORIA */

    parse_which_info = parse_a_info;

    err = read_edit_file(file_name);

    return (err);
}



/*
 * Initialize the "e_info" array
 */
static int init_e_info(void)
{
    int err;

    QString file_name;

    e_info = C_ZNEW(z_info->e_max, ego_item_type);

    if (game_mode == GAME_NPPANGBAND) file_name = "ego_item";
    else file_name = "m_ego_item";  /* game_mode == NPPMORIA */

    parse_which_info = parse_e_info;

    err = read_edit_file(file_name);

    return (err);
}


/*
 * Initialize the "r_info" array
 */
static int init_r_info(void)
{
    int err;

    QString file_name;

    r_info = C_ZNEW(z_info->r_max, monster_race);

    if (game_mode == GAME_NPPANGBAND) file_name = "monster";
    else file_name = "m_monster";  /* game_mode == NPPMORIA */

    parse_which_info = parse_r_info;

    err = read_edit_file(file_name);

    return (err);
}



/*
 * Initialize the "v_info" array
 */
static int init_v_info(void)
{
    int err;

    QString file_name;

    v_info = C_ZNEW(z_info->v_max, vault_type);

    file_name = "vault";

    parse_which_info = parse_v_info;

    err = read_edit_file(file_name);

    return (err);
}



/*
 * Initialize the "p_info" array
 */
static int init_p_info(void)
{
    int err;

    QString file_name;

    p_info = C_ZNEW(z_info->p_max, player_race);

    if (game_mode == GAME_NPPANGBAND) file_name = "p_race";
    else file_name = "m_p_race";  /* game_mode == NPPMORIA */

    parse_which_info = parse_p_info;

    err = read_edit_file(file_name);

    return (err);
}


/*
 * Initialize the "c_info" array
 */
static int init_c_info(void)
{
    int err;

    cur_title = 0;
    cur_equip = 0;

    QString file_name;

    c_info = C_ZNEW(z_info->c_max, player_class);

    if (game_mode == GAME_NPPANGBAND) file_name = "p_class";
    else file_name = "m_p_class";  /* game_mode == NPPMORIA */

    parse_which_info = parse_c_info;

    err = read_edit_file(file_name);

    return (err);
}



/*
 * Initialize the "h_info" array
 */
static int init_h_info(void)
{
    int err;

    QString file_name;

    h_info = C_ZNEW(z_info->h_max, hist_type);

    if (game_mode == GAME_NPPANGBAND) file_name = "p_hist";
    else file_name = "m_p_hist";  /* game_mode == NPPMORIA */

    parse_which_info = parse_h_info;

    err = read_edit_file(file_name);

    return (err);
}



/*
 * Initialize the "b_info" array
 */
static int init_b_info(void)
{
    int err;

    shop_idx = 0;
    owner_idx = 0;

    QString file_name;

    b_info = C_ZNEW((z_info->b_max * MAX_STORES), owner_type);

    if (game_mode == GAME_NPPANGBAND) file_name = "shop_own";
    else file_name = "m_shop_own";  /* game_mode == NPPMORIA */

    parse_which_info = parse_b_info;

    err = read_edit_file(file_name);

    return (err);
}


/*
 * Initialize the "q_info" array
 */
static int init_q_info(void)
{
    int err;

    QString file_name;

    q_info = C_ZNEW(z_info->q_max, quest_type);

    if (game_mode == GAME_NPPANGBAND) file_name = "quest";
    else file_name = "m_quest";  /* game_mode == NPPMORIA */

    parse_which_info = parse_q_info;

    err = read_edit_file(file_name);

    // Make sure the quests are appearing in the right order.
    prev_lev = 0;

    return (err);
}

/*
 * Initialize the "n_info" structure
 */
static int init_n_info(void)
{
  int err;

  QString file_name;

  n_info = C_ZNEW(1, names_type);

  file_name = "names";

  parse_which_info = parse_n_info;

  err = read_edit_file(file_name);

  return (err);
}



/*
 * Initialize the "flavor_info" array
 */
static int init_flavor_info(void)
{
    int err;

    QString file_name;

    flavor_info = C_ZNEW(z_info->flavor_max, flavor_type);

    file_name = "flavor";

    parse_which_info = parse_flavor_info;

    err = read_edit_file(file_name);

    return (err);
}



/*
 * Initialize some other arrays
 */
static int init_other(void)
{
    int i;

    reset_dungeon_info();

    /* Arrays of grids */
    view_grids.clear();
    fire_grids.clear();
    project_grids.clear();
    room_grids.clear();
    target_grids.clear();

    /* Array of dynamic grids */
    dyna_grids.clear();

    /* Array of stacked monster messages */
    mon_msg.clear();
    mon_message_hist.clear();

    /* Prepare monster movement array*/
    mon_moment_info.clear();

    /*** Prepare dungeon arrays ***/


    /*start with cost at center 0*/
    for (i = 0; i < MAX_FLOWS; i++)
    {
        cost_at_center[i] = 0;
    }


    /*** Prepare "vinfo" array ***/

    /* Used by "update_view()" */
    (void)vinfo_init();


    /*** Prepare entity arrays ***/

    /* Objects */
    o_list = C_ZNEW(z_info->o_max, object_type);
    o_max = 1;
    o_cnt = 0;

    /* Monsters */
    mon_list = C_ZNEW(z_info->m_max, monster_type);
    mon_max = 1;
    mon_cnt = 0;

    /* Effects */
    x_list = C_ZNEW(z_info->x_max, effect_type);
    x_max = 1;
    x_cnt = 0;


    /*** Prepare mosnter lore array ***/

    /* Lore */
    l_list = C_ZNEW(z_info->r_max, monster_lore);

    /*** Prepare terrain lore array ***/

    /* Lore */
    f_l_list = C_ZNEW(z_info->f_max, feature_lore);

    /*** Prepare artifact lore array ***/

    /* Lore */
    a_l_list = C_ZNEW(z_info->art_max, artifact_lore);

    /*** Prepare the inventory ***/

    /* Allocate it */
    inventory = C_ZNEW(ALL_INVEN_TOTAL, object_type);

    /*** Prepare the stores ***/

    /* Allocate the stores */
    store = C_ZNEW(MAX_STORES, store_type);

    /* Fill in each store */
    for (i = 0; i < MAX_STORES; i++)
    {

        /* Get the store */
        store_type *st_ptr = &store[i];

        /* Assume full stock */
        st_ptr->stock_size = STORE_INVEN_MAX;

        /* Allocate the stock */
        st_ptr->stock = C_ZNEW(st_ptr->stock_size, object_type);

    }

    /*** Prepare the options ***/

    p_ptr->player_type_wipe();
    op_ptr->player_other_wipe();
    op_ptr->delay_anim_factor = 100;

    /*Clear the update flags*/
    p_ptr->notice = 0L;
    p_ptr->update = 0L;
    p_ptr->redraw = 0L;

    //Clear some vectors
    notes_log.clear();
    message_list.clear();
    player_scores_list.clear();

    /* Success */
    return (0);
}



/*
 * Initialize some other arrays
 */
static int init_alloc(void)
{
    int i, j;

    object_kind *k_ptr;

    feature_type *f_ptr;

    monster_race *r_ptr;

    ego_item_type *e_ptr;

    alloc_entry *table;

    s16b num[MAX_DEPTH_ALL];

    s16b aux[MAX_DEPTH_ALL];


    /*** Analyze object allocation info ***/

    /* Clear the "aux" array */
    (void)C_WIPE(aux, MAX_DEPTH_ALL, s16b);

    /* Clear the "num" array */
    (void)C_WIPE(num, MAX_DEPTH_ALL, s16b);

    /* Size of "alloc_kind_table" */
    alloc_kind_size = 0;

    /* Scan the objects */
    for (i = 1; i < z_info->k_max; i++)
    {
        k_ptr = &k_info[i];

        /* Scan allocation pairs */
        for (j = 0; j < 4; j++)
        {
            /* Count the "legal" entries */
            if (k_ptr->chance[j])
            {
                /* Count the entries */
                alloc_kind_size++;

                /* Group by level */
                num[k_ptr->locale[j]]++;
            }
        }
    }

    /* Collect the level indexes */
    for (i = 1; i < MAX_DEPTH_ALL; i++)
    {
        /* Group by level */
        num[i] += num[i-1];
    }

    /* Paranoia */
    if (!num[0]) quit_npp_games(QObject::tr("No town objects!"));


    /*** Initialize object allocation info ***/

    /* Allocate the alloc_kind_table */
    alloc_kind_table = C_ZNEW(alloc_kind_size, alloc_entry);

    /* Get the table entry */
    table = alloc_kind_table;

    /* Scan the objects */
    for (i = 1; i < z_info->k_max; i++)
    {
        k_ptr = &k_info[i];

        /* Scan allocation pairs */
        for (j = 0; j < 4; j++)
        {
            /* Count the "legal" entries */
            if (k_ptr->chance[j])
            {
                int p, x, y, z;

                /* Extract the base level */
                x = k_ptr->locale[j];

                /* Extract the base probability */
                p = (100 / k_ptr->chance[j]);

                /* Skip entries preceding our locale */
                y = (x > 0) ? num[x-1] : 0;

                /* Skip previous entries at this locale */
                z = y + aux[x];

                /* Load the entry */
                table[z].index = i;
                table[z].level = x;
                table[z].prob1 = p;
                table[z].prob2 = p;
                table[z].prob3 = p;

                /* Another entry complete for this locale */
                aux[x]++;
            }
        }
    }

    /*** Analyze feature allocation info ***/

    /* Clear the "aux" array */
    (void)C_WIPE(&aux, MAX_DEPTH_ALL, s16b);

    /* Clear the "num" array */
    (void)C_WIPE(&num, MAX_DEPTH_ALL, s16b);

    /* Size of "alloc_feat_table" */
    alloc_feat_size = 0;

    /* Scan the features */
    for (i = 1; i < z_info->f_max; i++)
    {
        /* Get the i'th race */
        f_ptr = &f_info[i];

        /* Legal features */
        if (f_ptr->f_rarity)
        {
            /* Count the entries */
            alloc_feat_size++;

            /* Group by level */
            num[f_ptr->f_level]++;
        }
    }

    /* Collect the level indexes */
    for (i = 1; i < MAX_DEPTH_ALL; i++)
    {
        /* Group by level */
        num[i] += num[i-1];
    }

    /* Paranoia - not really necessary */
    if (!num[0]) quit_npp_games(QObject::tr("No town features!"));

    /*** Initialize feature allocation info ***/

    /* Allocate the alloc_feat_table */
    alloc_feat_table = C_ZNEW(alloc_feat_size, alloc_entry);

    /* Get the table entry */
    table = alloc_feat_table;

    /* Scan the features */
    for (i = 1; i < z_info->f_max; i++)
    {
        /* Get the i'th feature */
        f_ptr = &f_info[i];

        /* Count valid pairs */
        if (f_ptr->f_rarity)
        {
            int p, x, y, z;

            /* Extract the base level */
            x = f_ptr->f_level;

            /* Extract the base probability */
            p = (100 / f_ptr->f_rarity);

            /* Skip entries preceding our locale */
            y = (x > 0) ? num[x-1] : 0;

            /* Skip previous entries at this locale */
            z = y + aux[x];

            /* Load the entry */
            table[z].index = i;
            table[z].level = x;
            table[z].prob1 = p;
            table[z].prob2 = p;
            table[z].prob3 = p;

            /* Another entry complete for this locale */
            aux[x]++;
        }
    }

    /*** Analyze monster allocation info ***/

    /* Clear the "aux" array */
    (void)C_WIPE(aux, MAX_DEPTH_ALL, s16b);

    /* Clear the "num" array */
    (void)C_WIPE(num, MAX_DEPTH_ALL, s16b);

    /* Size of "alloc_race_table" */
    alloc_race_size = 0;

    /* Scan the monsters*/
    for (i = 1; i < z_info->r_max; i++)
    {
        /* Get the i'th race */
        r_ptr = &r_info[i];

        /* Legal monsters */
        if (r_ptr->rarity)
        {
            /* Count the entries */
            alloc_race_size++;

            /* Group by level */
            num[r_ptr->level]++;
        }
    }

    /* Collect the level indexes */
    for (i = 1; i < MAX_DEPTH_ALL; i++)
    {
        /* Group by level */
        num[i] += num[i-1];
    }

    /* Paranoia */
    if (!num[0]) quit_npp_games(QObject::tr("No town monsters!"));


    /*** Initialize monster allocation info ***/

    /* Allocate the alloc_race_table */
    alloc_race_table = C_ZNEW(alloc_race_size, alloc_entry);

    /* Get the table entry */
    table = alloc_race_table;

    /* Scan the monsters*/
    for (i = 1; i < z_info->r_max; i++)
    {
        /* Get the i'th race */
        r_ptr = &r_info[i];

        /* Count valid pairs */
        if (r_ptr->rarity)
        {
            int p, x, y, z;

            /* Extract the base level */
            x = r_ptr->level;

            /* Extract the base probability */
            p = (100 / r_ptr->rarity);

            /* Skip entries preceding our locale */
            y = (x > 0) ? num[x-1] : 0;

            /* Skip previous entries at this locale */
            z = y + aux[x];

            /* Load the entry */
            table[z].index = i;
            table[z].level = x;
            table[z].prob1 = p;
            table[z].prob2 = p;
            table[z].prob3 = p;

            /* Another entry complete for this locale */
            aux[x]++;
        }
    }

    /*** Analyze ego_item allocation info ***/

    /* Clear the "aux" array */
    (void)C_WIPE(aux, MAX_DEPTH_ALL, s16b);

    /* Clear the "num" array */
    (void)C_WIPE(num, MAX_DEPTH_ALL, s16b);

    /* Size of "alloc_ego_table" */
    alloc_ego_size = 0;

    /* Scan the ego items */
    for (i = 1; i < z_info->e_max; i++)
    {
        /* Get the i'th ego item */
        e_ptr = &e_info[i];

        /* Legal items */
        if (e_ptr->rarity)
        {
            /* Count the entries */
            alloc_ego_size++;

            /* Group by level */
            num[e_ptr->level]++;
        }
    }

    /* Collect the level indexes */
    for (i = 1; i < MAX_DEPTH_ALL; i++)
    {
        /* Group by level */
        num[i] += num[i-1];
    }

    /*** Initialize ego-item allocation info ***/

    /* Allocate the alloc_ego_table */
    alloc_ego_table = C_ZNEW(alloc_ego_size, alloc_entry);

    /* Get the table entry */
    table = alloc_ego_table;

    /* Scan the ego-items */
    for (i = 1; i < z_info->e_max; i++)
    {
        /* Get the i'th ego item */
        e_ptr = &e_info[i];

        /* Count valid pairs */
        if (e_ptr->rarity)
        {
            int p, x, y, z;

            /* Extract the base level */
            x = e_ptr->level;

            /* Extract the base probability */
            p = (100 / e_ptr->rarity);

            /* Skip entries preceding our locale */
            y = (x > 0) ? num[x-1] : 0;

            /* Skip previous entries at this locale */
            z = y + aux[x];

            /* Load the entry */
            table[z].index = i;
            table[z].level = x;
            table[z].prob1 = p;
            table[z].prob2 = p;
            table[z].prob3 = p;

            /* Another entry complete for this locale */
            aux[x]++;
        }
    }

    /* Success */
    return (0);
}

static void init_rng()
{
    /* Init RNG */
    if (Rand_quick)
    {
        u32b seed;

        /* Basic seed */
        seed = (u32b)(time(NULL));

        /* Use the complex RNG */
        Rand_quick = FALSE;

        /* Seed the "complex" RNG */
        Rand_state_init(seed);
    }
}

/*
 * Hack -- main Angband initialization entry point
 *
 * Verify some files, display the "news.txt" file, create
 * the high score file, initialize all internal arrays, and
 * load the basic "user pref files".
 *
 * Be very careful to keep track of the order in which things
 * are initialized, in particular, the only thing *known* to
 * be available when this function is called is the "z-term.c"
 * package, and that may not be fully initialized until the
 * end of this function, when the default "user pref files"
 * are loaded and "Term_xtra(TERM_XTRA_REACT,0)" is called.
 *
 * Note that this function attempts to verify the "news" file,
 * and the game aborts (cleanly) on failure, since without the
 * "news" file, it is likely that the "lib" folder has not been
 * correctly located.  Otherwise, the news file is displayed for
 * the user.
 *
 * Note that this function attempts to verify (or create) the
 * "high score" file, and the game aborts (cleanly) on failure,
 * since one of the most common "extraction" failures involves
 * failing to extract all sub-directories (even empty ones), such
 * as by failing to use the "-d" option of "pkunzip", or failing
 * to use the "save empty directories" option with "Compact Pro".
 * This error will often be caught by the "high score" creation
 * code below, since the "lib/apex" directory, being empty in the
 * standard distributions, is most likely to be "lost", making it
 * impossible to create the high score file.
 *
 * Note that various things are initialized by this function,
 * including everything that was once done by "init_some_arrays".
 *
 * This initialization involves the parsing of special files
 * in the "lib/data" and sometimes the "lib/edit" directories.
 *
 * Note that the "template" files are initialized first, since they
 * often contain errors.  This means that macros and message recall
 * and things like that are not available until after they are done.
 *
 * We load the default "user pref files" here in case any "color"
 * changes are needed before character creation.
 *
 * Note that the "graf-xxx.prf" file must be loaded separately,
 * if needed, in the first (?) pass through "TERM_XTRA_REACT".
 */
void init_npp_games(void)
{
    Rand_quick = true;

    init_rng();

    character_dungeon = character_generated = character_loaded = FALSE;

    QLabel status_update;
    status_update.setText (QString("Starting game"));
    //status_update.show();

    /*** Initialize some arrays ***/

    /* Initialize size info */
    status_update.setText (QString(QObject::tr("Initializing array sizes...")));
    if (init_z_info()) quit_npp_games(QObject::tr("Cannot initialize sizes"));

    if (game_mode == GAME_NPPANGBAND) init_nppangband();
    else if (game_mode == GAME_NPPMORIA) init_nppmoria();
    else return;  //Something would be broken with the setup for this to happen.

    /* Initialize color info */
    status_update.setText (QString(QObject::tr("Initializing colors...")));
    if (init_color_info()) quit_npp_games(QObject::tr("Cannot initialize colors"));

    /* Initialize feature info */
    status_update.setText (QString(QObject::tr("Initializing arrays... (features)")));
    if (init_f_info()) quit_npp_games(QObject::tr("Cannot initialize features"));

    /* Initialize object info */
    status_update.setText (QString(QObject::tr("Initializing arrays... (objects)")));
    if (init_k_info()) quit_npp_games(QObject::tr("Cannot initialize objects"));

    /* Initialize object info */
    status_update.setText (QString(QObject::tr("Initializing arrays... (ghosts)")));
    if (init_t_info()) quit_npp_games(QObject::tr("Cannot initialize ghosts"));

    /* Initialize artifact info */
    status_update.setText (QString(QObject::tr("Initializing arrays... (artifacts)")));
    if (init_a_info()) quit_npp_games(QObject::tr("Cannot initialize artifacts"));

    /* Initialize ego-item info */
    status_update.setText (QString(QObject::tr("Initializing arrays... (ego-items)")));
    if (init_e_info()) quit_npp_games(QObject::tr("Cannot initialize ego-items"));

    /* Initialize monster info */
    status_update.setText (QString(QObject::tr("Initializing arrays... (monsters)")));
    if (init_r_info()) quit_npp_games(QObject::tr("Cannot initialize monsters"));

    /* Initialize feature info */
    status_update.setText (QString(QObject::tr("Initializing arrays... vaults)")));
    if (init_v_info()) quit_npp_games(QObject::tr("Cannot initialize vaults"));

    /* Initialize history info */
    status_update.setText (QString(QObject::tr("Initializing arrays... histories)")));
    if (init_h_info()) quit_npp_games(QObject::tr("Cannot initialize histories"));

    /* Initialize race info */
    status_update.setText (QString(QObject::tr("Initializing arrays... (races)")));
    if (init_p_info()) quit_npp_games(QObject::tr("Cannot initialize races"));

    /* Initialize class info */
    status_update.setText (QString(QObject::tr("Initializing arrays... (classes)")));
    if (init_c_info()) quit_npp_games(QObject::tr("Cannot initialize classes"));

    /* Initialize owner info */
    status_update.setText (QString(QObject::tr("Initializing arrays... (owners)")));
    if (init_b_info()) quit_npp_games(QObject::tr("Cannot initialize owners"));

    /* Initialize flavor info */
    status_update.setText (QString(QObject::tr("Initializing arrays... (flavors)")));
    if (init_flavor_info()) quit_npp_games(QObject::tr("Cannot initialize flavors"));

    /* Initialize quests info */
    status_update.setText (QString(QObject::tr("Initializing arrays... (quests)")));
    if (init_q_info()) quit_npp_games(QObject::tr("Cannot initialize quests"));

    /* Initialize some other arrays */
    status_update.setText (QString(QObject::tr("Initializing arrays... (other)")));
    if (init_other()) quit_npp_games(QObject::tr("Cannot initialize other stuff"));

    /* Initialize some other arrays */
    status_update.setText (QString(QObject::tr("Initializing arrays... (alloc)")));
    if (init_alloc()) quit_npp_games(QObject::tr("Cannot initialize alloc stuff"));

    /*** Load default user pref files ***/


    /* Initialize randart tables info */
    status_update.setText (QString(QObject::tr("Initializing Random Artifact Tables...]")));
    if (init_n_info()) quit_npp_games(QObject::tr("Cannot initialize random name generator list"));

    /*Build the randart probability tables based on the standard Artifact Set*/
    build_randart_tables();

    /* Done */
    status_update.setText (QString(QObject::tr("Initialization complete")));
    status_update.hide();

    // Calculate the maximum possible abilities for each race/class combo.
    pam_ptr->calculate_maximums();

    // These are loaded and saved with the player
    clear_all_hotkeys();

    qsrand(QTime::currentTime().msec());
}


void cleanup_npp_games(void)
{
    //clear_graphics();

    /* Free the allocation tables */
    FREE_ARRAY(alloc_ego_table);
    FREE_ARRAY(alloc_feat_table);
    FREE_ARRAY(alloc_race_table);
    FREE_ARRAY(alloc_kind_table);

    if (store)
    {
        /* Free the store inventories */
        for (int i = 0; i < MAX_STORES; i++)
        {
            /* Get the store */
            store_type *st_ptr = &store[i];

            /* Free the store inventory */
            if (st_ptr->stock) FREE_ARRAY(st_ptr->stock);
        }
    }

    /* Free the stores */
    FREE_ARRAY(store);

    /* Free the player inventory */
    FREE_ARRAY(inventory);

    // Free the various edit file arrays
    FREE_ARRAY(z_info);
    FREE_ARRAY(f_info);
    FREE_ARRAY(k_info);
    FREE_ARRAY(t_info);
    FREE_ARRAY(a_info);
    FREE_ARRAY(e_info);
    FREE_ARRAY(r_info);
    FREE_ARRAY(v_info);
    FREE_ARRAY(p_info);
    FREE_ARRAY(c_info);
    FREE_ARRAY(h_info);
    FREE_ARRAY(b_info);
    FREE_ARRAY(q_info);
    FREE_ARRAY(n_info);
    FREE_ARRAY(flavor_info);


    /* Free the lore, monster, effects, and object lists */
    FREE_ARRAY(l_list);
    FREE_ARRAY(f_l_list);
    FREE_ARRAY(a_l_list);
    FREE_ARRAY(mon_list);
    FREE_ARRAY(o_list);
    FREE_ARRAY(x_list);


    /* CLear the monster movement array*/
    mon_moment_info.clear();

    /* Free the "update_view()" array */
    view_grids.clear();
    fire_grids.clear();
    project_grids.clear();
    room_grids.clear();
    target_grids.clear();

    /* CLear dynamic features */
    dyna_grids.clear();

    /* Free the stacked monster messages */
    mon_msg.clear();
    mon_message_hist.clear();

    /*free the randart arrays*/
    free_randart_tables();

    // CLear the hotkeys
    clear_all_hotkeys();
}


/*
 * Hold the titles of scrolls, 6 to 14 characters each.
 */
QString scroll_adj[MAX_TITLES];


/*
 * Assign flavors that don't change
 */
static void flavor_assign_fixed(void)
{
    for (int i = 1; i < z_info->flavor_max; i++)
    {
        flavor_type *flavor_ptr = &flavor_info[i];

        /* Skip random flavors */
        if (flavor_ptr->sval == SV_UNKNOWN) continue;

        for (int j = 0; j < z_info->k_max; j++)
        {
            /* Skip other objects */
            if ((k_info[j].tval == flavor_ptr->tval) &&
                (k_info[j].sval == flavor_ptr->sval))
            {
                /* Store the flavor index */
                k_info[j].flavor = i;
            }
        }
    }
}


/*
 * Assign random flavors
 */
static void flavor_assign_random(byte tval)
{
    u16b i, choice;
    u16b flavor_count = 0;
    u16b *flavor;

    /* Allocate the "who" array */
    flavor = C_ZNEW(z_info->flavor_max, u16b);

    /* Count the random flavors for the given tval */
    for (i = 0; i < z_info->flavor_max; i++)
    {
        if ((flavor_info[i].tval == tval) &&
            (flavor_info[i].sval == SV_UNKNOWN))
        {
            flavor[flavor_count] = i;
            flavor_count++;
        }
    }

    for (i = 0; i < z_info->k_max; i++)
    {
        u16b slot;

        /* Skip other object types */
        if (k_info[i].tval != tval) continue;

        /* Skip objects that already are flavored */
        if (k_info[i].flavor != 0) continue;

            /* HACK - Ordinary food is "boring" */
        if ((tval == TV_FOOD) && (k_info[i].sval > SV_FOOD_MIN_FOOD))
            continue;

        if (!flavor_count)
        {
            FREE_ARRAY(flavor);
            quit_npp_games(QString("Not enough flavors for tval %1.")  .arg(tval));
        }

        /* Select a flavor */
        slot = randint0(flavor_count);
        choice = flavor[slot];

        /* Store the flavor index */
        k_info[i].flavor = choice;

        /* Mark the flavor as used */
        flavor_info[choice].sval = k_info[i].sval;

        /* One less flavor to choose from */
        flavor_count--;
        flavor[slot] = flavor[flavor_count];
    }

    /* Free the array */
    FREE_ARRAY(flavor);
}


/*
 * Prepare the "variable" part of the "k_info" array.
 *
 * The "color"/"metal"/"type" of an item is its "flavor".
 * For the most part, flavors are assigned randomly each game.
 *
 * Initialize descriptions for the "colored" objects, including:
 * Rings, Amulets, Staffs, Wands, Rods, Food, Potions, Scrolls.
 *
 * The first 4 entries for potions are fixed (Water, Apple Juice,
 * Slime Mold Juice, Unused Potion).
 *
 * Scroll titles are always between 6 and 14 letters long.  This is
 * ensured because every title is composed of whole words, where every
 * word is from 1 to 8 letters long (one or two syllables of 1 to 4
 * letters each), and that no scroll is finished until it attempts to
 * grow beyond 15 letters.  The first time this can happen is when the
 * current title has 6 letters and the new word has 8 letters, which
 * would result in a 6 letter scroll title.
 *
 * Duplicate titles are avoided by requiring that no two scrolls share
 * the same first four letters (not the most efficient method, and not
 * the least efficient method, but it will always work).
 *
 * Hack -- make sure everything stays the same for each saved game
 * This is accomplished by the use of a saved "random seed", as in
 * "town_gen()".  Since no other functions are called while the special
 * seed is in effect, so this function is pretty "safe".
 */
void flavor_init(void)
{
    int i, j;

    /* Hack -- Use the "simple" RNG */
    Rand_quick = TRUE;

    /* Hack -- Induce consistent flavors */
    Rand_value = seed_flavor;

    flavor_assign_fixed();

    flavor_assign_random(TV_RING);
    flavor_assign_random(TV_AMULET);
    flavor_assign_random(TV_STAFF);
    flavor_assign_random(TV_WAND);
    flavor_assign_random(TV_ROD);
    flavor_assign_random(TV_FOOD);
    flavor_assign_random(TV_POTION);
    flavor_assign_random(TV_SCROLL);

    /* Scrolls (random titles, always white) */
    for (i = 0; i < MAX_TITLES; i++)
    {
        /* Get a new title */
        while (TRUE)
        {
            QString buf;

            bool okay;

            /* Start a new title */
            buf.clear();

            /* Collect words until done */
            while (TRUE)
            {
                int q, s;

                QString tmp;

                /* Start a new word */
                tmp.clear();

                /* Choose one or two syllables */
                s = ((rand_int(100) < 30) ? 1 : 2);

                /* Add a one or two syllable word */
                for (q = 0; q < s; q++)
                {
                    QString syllable;
                    byte min;
                    byte max;

                    /* Different syllable lengths for 1 and two syllable words */
                    if (s == 1)
                    {
                        min = 4;
                        max = 10;
                    }
                    else /* (s == 2) */
                    {
                        min = 2;
                        max = 6;
                    }

                    /* Make random_syllable */
                    syllable = make_random_name(min, max);
                    syllable.remove(" ");

                    /* Make second syllable lowercase */
                    if (q) syllable = syllable.toLower();

                    /* Add the syllable */
                    tmp.append (syllable);
                }

                /* Stop before getting too long */
                if ((buf.length() + 1 + tmp.length()) > 15) break;

                /* Add a space */
                if (buf.length()) buf.append(" ");

                /* Add the word */
                buf.append (tmp);
            }

            /* Save the title */
           scroll_adj[i] = buf;

            /* Assume okay */
            okay = TRUE;

            /* Check for "duplicate" scroll titles */
            for (j = 0; j < i; j++)
            {
                /* Compare first four characters */
                QString hack1 = scroll_adj[j];
                QString hack2 = scroll_adj[i];
                hack1.truncate(4);
                hack2.truncate(4);
                if (QString::compare(hack1, hack2, Qt::CaseInsensitive)) continue;

                /* Not okay */
                okay = FALSE;

                /* Stop looking */
                break;
            }

            /* Break when done */
            if (okay) break;
        }
    }


    /* Hack -- Use the "complex" RNG */
    Rand_quick = FALSE;

    /* Analyze every object */
    for (i = 1; i < z_info->k_max; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /*Skip "empty" objects*/
        if (!k_ptr->k_name.length()) continue;

        /*No flavor yields aware*/
        if (!k_ptr->flavor) k_ptr->aware = TRUE;
    }
}

