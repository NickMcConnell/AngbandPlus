
/* File: was cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/player_command.h>
#include <src/cmds.h>
#include "src/object_settings.h"
#include <src/npp.h>
#include <QPushButton>
#include <QVBoxLayout>

void SpellSelectDialog::move_left()
{
    int which_tab = spell_dialog->currentIndex();
    spell_dialog->setCurrentIndex(which_tab - 1);
}

void SpellSelectDialog::move_right()
{
    int which_tab = spell_dialog->currentIndex();
    spell_dialog->setCurrentIndex(which_tab + 1);
}

// Receives the number of the button pressed.
void SpellSelectDialog::button_press(int num)
{
    selected_button = num;

    this->accept();
}

// See if the user selected a button bia a keypress.
void SpellSelectDialog::keyPressEvent(QKeyEvent* which_key)
{
    // Handle escape key
    if (which_key->key() == Qt::Key_Escape)
    {
        this->reject();
        return;
    }

    if (which_key->key() == Qt::Key_Less)
    {
        move_left();
        return;
    }

    if (which_key->key() == Qt::Key_Greater)
    {
        move_right();
        return;
    }

    QString key_pressed = which_key->text();

    //  Make sure we are dealing with a letter
    if (key_pressed.length() != 1 || !key_pressed.at(0).isLetter()) return;

    // Make it lowercase
    key_pressed = key_pressed.toLower();

    // Make sure we are dealing with the item name buttons and not the info buttons
    // or other future buttons
    key_pressed.append(") ");

    QWidget *tab = this->spell_dialog->currentWidget(); // Search in the current tab
    QList<QPushButton *> buttons = tab->findChildren<QPushButton *>();
    for (int i = 0; i < buttons.size(); i++)
    {
        QString this_text = buttons.at(i)->text();

        if (this_text.startsWith(key_pressed))
        {
            buttons.at(i)->click();
            break;
        }
    }
}


// Receives the number of the button pressed.
void SpellSelectDialog::help_press(int num)
{

    QString spell_desc = (QString("<b><big>%1</big></b><br><br>")
                        .arg(cast_spell(MODE_SPELL_NAME, cp_ptr->spell_book, num, 0)));

    spell_desc.append(cast_spell(MODE_SPELL_DESC, cp_ptr->spell_book, num, 0));

    /* Display the spell */
    QMessageBox::information(0, "Press OK to continue.", spell_desc, QMessageBox::Ok);
}



void SpellSelectDialog::available_spells(int mode)
{
    for (int i = 0; i < max_spellbooks; i++)
    {
        available_books[i] = FALSE;

        int idx = lookup_kind(cp_ptr->spell_book, i);
        if (mode == BOOK_BROWSE)
        {
            if (!k_info[idx].everseen) continue;
        }
        else if (!object_kind_is_available(idx, USE_FLOOR | USE_INVEN | USE_STORE)) continue;

        for (int j = 0; j < SPELLS_PER_BOOK; j++)
        {
            int spell = get_spell_from_list(i, j);

            if (spell == -1) continue;

            if (mode != BOOK_BROWSE)
            {
                if (!spell_okay(spell, (mode == BOOK_CAST))) continue;
            }

            // Make sure we know we are using this book
            available_books[i] = TRUE;

            usable_spells = TRUE;

            break;
        }
    }
}


QString SpellSelectDialog::get_spell_comment(int spell)
{
    const magic_type *s_ptr = &mp_ptr->info[spell];
    QString comment;

    // Illegible spell
    if (s_ptr->slevel >= 99) comment = "(illegible)";

    // Is it an ironman spell?
    else if (p_ptr->spell_flags[spell] & PY_SPELL_IRONMAN)      comment = "Ironman Spell";

    // Forgotten spell
    else if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)    comment = "forgotten";

    // Not learned yet
    else if (!(p_ptr->spell_flags[spell] & PY_SPELL_LEARNED))
    {
        if (s_ptr->slevel <= p_ptr->lev) comment = "unknown";
        else comment = "difficult";
    }
    else if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED)) comment = "untried";

    // Get a spell description
    else comment = cast_spell(MODE_SPELL_DESC_SHORT, cp_ptr->spell_book, spell, 0);

    return (comment);
}


void SpellSelectDialog::build_spellbook_dialog(int mode)
{
    for (int i = 0; i < max_spellbooks; i++)
    {
        // Track to which line we are adding widgets
        int row_num = 0;

        // Nothing to select in this book.
        if (!available_books[i]) continue;

        int idx = lookup_kind(cp_ptr->spell_book, i);

        // Get the book name and remove the brackets
        QString book_name = k_info[idx].k_name;
        book_name.remove("[");
        book_name.remove("]");

        QPointer<QWidget> spell_tab = new QWidget;

        QPointer<QVBoxLayout> vlay = new QVBoxLayout;

        QPointer<QWidget> aux = new QWidget;
        vlay->addWidget(aux);

        vlay->addStretch(1);

        QPointer<QGridLayout> spell_layout = new QGridLayout;
        aux->setLayout(spell_layout);

        // Add a button to select the spellbook
        if (choosing_book)
        {
            QString noun = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);
            QString button_text = (QString("a) Learn a %1 from %2") .arg(noun) .arg(book_name));
            QPointer<QPushButton> button = new QPushButton(button_text);
            spell_layout->addWidget(button, row_num, COL_SPELL_TITLE, Qt::AlignLeft);
            spell_select_group->addButton(button, i);
            row_num++;
        }

        // Give each one titles
        QPointer<QLabel> spell_title_header = new QLabel("Spell Name");
        QPointer<QLabel> level_header = new QLabel("  Level  ");
        QPointer<QLabel> mana_header = new QLabel("  Mana  ");
        QPointer<QLabel> fail_header = new QLabel("  % Fail  ");
        QPointer<QLabel> info_header = new QLabel(" Info");
        QPointer< QLabel> help_header = new QLabel("  Help  ");

        // Add the headers
        spell_layout->addWidget(spell_title_header, row_num, COL_SPELL_TITLE, Qt::AlignLeft);
        spell_layout->addWidget(level_header, row_num, COL_LEVEL, Qt::AlignCenter);
        spell_layout->addWidget(mana_header, row_num, COL_MANA, Qt::AlignCenter);
        spell_layout->addWidget(fail_header, row_num, COL_FAIL_PCT, Qt::AlignCenter);
        spell_layout->addWidget(info_header, row_num, COL_INFO, Qt::AlignLeft);
        spell_layout->addWidget(help_header, row_num, COL_HELP, Qt::AlignCenter);

        row_num++;

        for (int j = 0; j < SPELLS_PER_BOOK; j++)
        {
            // First decide if thise must be a text button or a push button
            int spell = get_spell_from_list(i, j);

            // Not a spell
            if (spell == -1) continue;

            /* Get the spell info */
            const magic_type *s_ptr = &mp_ptr->info[spell];

            bool do_text = FALSE;
            if (mode == BOOK_BROWSE) do_text = TRUE;
            else if (choosing_book) do_text = TRUE;
            else if (!spell_okay(spell, (mode == BOOK_CAST))) do_text = TRUE;

            QString spell_name;
            //  No prefix when selecting a book to learn prayers.
            if (choosing_book) spell_name.clear();
            else spell_name = (QString("%1) ") .arg(number_to_letter(j)));

            spell_name.append(cast_spell(MODE_SPELL_NAME, cp_ptr->spell_book, spell, 0));
            QString spell_comment = get_spell_comment(spell);

            QString text_num = QString::number(spell);
            // Either do a pushbutton or a text label
            if (do_text)
            {
                QPointer<QLabel> spell_label = new QLabel(spell_name);
                spell_label->setAlignment(Qt::AlignLeft);
                spell_label->setToolTip(cast_spell(MODE_SPELL_DESC, cp_ptr->spell_book, spell, 0));
                spell_layout->addWidget(spell_label, row_num, COL_SPELL_TITLE);
            }
            // Create a push button and link it
            else
            {
                QPointer<QPushButton> button = new QPushButton(text_num);
                button->setText(spell_name);
                button->setStyleSheet("Text-align:left");
                button->setToolTip(cast_spell(MODE_SPELL_DESC, cp_ptr->spell_book, spell, 0));
                spell_select_group->addButton(button, spell);
                spell_layout->addWidget(button, row_num, COL_SPELL_TITLE);
            }

            // Add level info
            QPointer<QLabel> level_value = new QLabel(QString("%1") .arg(s_ptr->slevel));
            spell_layout->addWidget(level_value, row_num, COL_LEVEL, Qt::AlignCenter);
            // Add mana info
            QPointer<QLabel> mana_value = new QLabel(QString("%1") .arg(s_ptr->smana));
            spell_layout->addWidget(mana_value, row_num, COL_MANA, Qt::AlignCenter);
            // Add spell % info
            QPointer<QLabel> spell_value = new QLabel(QString("%1%") .arg(spell_chance(spell)));
            spell_layout->addWidget(spell_value, row_num, COL_FAIL_PCT, Qt::AlignCenter);
            // Add spell_desc info
            QPointer<QLabel> info_desc = new QLabel(QString("%1") .arg(spell_comment));
            spell_layout->addWidget(info_desc, row_num, COL_INFO, Qt::AlignLeft);

            // Add a help button to put up a detailed spell description.
            QPointer<QPushButton> help_button = new QPushButton();
            help_button->setIcon(QIcon(":/icons/lib/icons/help.png"));
            spell_help_group->addButton(help_button, spell);
            spell_layout->addWidget(help_button, row_num, COL_HELP);

            row_num++;

        }

        vlay->addStretch(1);

        spell_tab->setLayout(vlay);

        int tab_index = spell_dialog->addTab(spell_tab, book_name);

        // Prepare to activate the right tab, if specified
        if (k_info[idx].sval == selected_book) activate_tab = tab_index;
    }

    connect(spell_select_group, SIGNAL(buttonClicked(int)), this, SLOT(button_press(int)));
    connect(spell_help_group, SIGNAL(buttonClicked(int)), this, SLOT(help_press(int)));
}


// This assumes the check that the player can cast has already been done.
SpellSelectDialog::SpellSelectDialog(int *spell, int start_sval, QString prompt, int mode, bool *cannot, bool *cancelled)
{
    // Start with a clean slate
    usable_spells = FALSE;
    max_spellbooks = (game_mode == GAME_NPPANGBAND ? BOOKS_PER_REALM_ANGBAND : BOOKS_PER_REALM_MORIA);
    choosing_book = FALSE;
    *cancelled = FALSE;
    *cannot = TRUE;
    activate_tab = -1;
    selected_book = start_sval;

    // First, find the eligible spells
    available_spells(mode);

    // Make sure we can actually do the command asked for
    if ((mode == BOOK_STUDY) && !p_ptr->can_study())
    {
        return;
    }
    else if (!p_ptr->can_cast())
    {
        return;
    }

    // Handle no available objects.
    if (!usable_spells)
    {
        /* Report failure */
         return;
    }

    spell_dialog = new QTabWidget(this);

    QPointer<QLabel> main_prompt = new QLabel(QString("<b><big>%1</big></b>") .arg(prompt), this);
    main_prompt->setAlignment(Qt::AlignCenter);

    // Passed all the pre-dialog checks.
    *cannot = FALSE;

    // We are selecting a book instead of a specific spell.
    if ((mode == BOOK_STUDY) && !p_ptr->chooses_spells()) choosing_book = TRUE;

    // Set up the button groups
    spell_select_group = new QButtonGroup(this);
    spell_help_group = new QButtonGroup(this);

    build_spellbook_dialog(mode);

    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox();
    QPointer<QPushButton> button_left = new QPushButton();
    button_left->setText("<");
    button_left->setToolTip("Pressing '<' also moves the active tab to the left.");
    connect(button_left, SIGNAL(clicked()), this, SLOT(move_left()));
    buttons->addButton(button_left, QDialogButtonBox::ActionRole);
    QPointer<QPushButton> button_right = new QPushButton();
    button_right->setText(">");
    button_right->setToolTip("Pressing '>' also moves the active tab to the right.");
    connect(button_right, SIGNAL(clicked()), this, SLOT(move_right()));
    buttons->addButton(button_right, QDialogButtonBox::ActionRole);

    if (mode == BOOK_BROWSE)
    {
        buttons->addButton(QDialogButtonBox::Close);
    }
    else buttons->addButton(QDialogButtonBox::Cancel);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));

    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;

    main_layout->addWidget(main_prompt);
    main_layout->addWidget(spell_dialog);
    main_layout->addWidget(buttons);

    setLayout(main_layout);
    setWindowTitle(tr("Spell Selection Menu"));

    if (activate_tab >=0) spell_dialog->setCurrentIndex(activate_tab);

    if (!this->exec())
    {
        *cancelled = TRUE;
    }
    else
    {
        *spell = selected_button;
    }
}


/*
 * Check if the given spell is in the given book.
 */
static bool spell_in_book(int spell, int sval)
{
    for (int i = 0; i < SPELLS_PER_BOOK; i++)
    {
        if (spell == get_spell_from_list(sval, i)) return TRUE;
    }

    return FALSE;
}

// Make sure the player has access to a spellbook to actually cast the spell.
static bool spell_is_available(int spell)
{
    int max_books = BOOKS_PER_REALM_ANGBAND;
    if (game_mode == GAME_NPPMORIA) max_books = BOOKS_PER_REALM_MORIA;

    // Go through all the books to find the spell
    for (int i = 0; i < max_books; i++)
    {
        int k_idx = lookup_kind(cp_ptr->spell_book, i);

        // Make sure we have the spell and book
        if (!spell_in_book(spell, i)) continue;

        if (!object_kind_is_available(k_idx, USE_INVEN | USE_FLOOR)) continue;

        // Success
        return (TRUE);
    }

    return (FALSE);
}

/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
bool spell_okay(int spell, bool known)
{
    const magic_type *s_ptr;

    /* Get the spell */
    s_ptr = &mp_ptr->info[spell];

    /* Spell is illegal */
    if (s_ptr->slevel > p_ptr->lev) return (FALSE);

    /* Spell is forgotten */
    if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
    {
        /* Never okay */
        return (FALSE);
    }

    /* Spell is ironman */
    if (p_ptr->spell_flags[spell] & PY_SPELL_IRONMAN)
    {
        /* Never okay */
        return (FALSE);
    }

    /* Spell is learned */
    if (p_ptr->spell_flags[spell] & PY_SPELL_LEARNED)
    {

        /* Okay to cast, not to study */
        return (known);
    }

    /* Okay to study, not to cast */
    return (!known);
}


/*
 * Learn the specified spell.
 */
static void spell_learn(int spell)
{
    int i;
   QString noun = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);

    /* Learn the spell */
    p_ptr->spell_flags[spell] |= PY_SPELL_LEARNED;

    /* Find the next open entry in "spell_order[]" */
    for (i = 0; i < PY_MAX_SPELLS; i++)
    {
        /* Stop at the first empty space */
        if (p_ptr->spell_order[i] == 99) break;
    }

    /* Add the spell to the known list */
    p_ptr->spell_order[i] = spell;

    /* Mention the result */
    message(QString("You have learned the %1 of %2.") .arg(noun) .arg(get_spell_name(cp_ptr->spell_book, spell)));

    /* One less spell available */
    p_ptr->new_spells--;

    /* Message if needed */
    if (p_ptr->new_spells)
    {
        if (p_ptr->new_spells > 1) noun.append("s");

        /* Message */
        message(QString("You can learn %1 more %2.") .arg(p_ptr->new_spells) .arg(noun));
    }

    /* Redraw Study Status */
    p_ptr->redraw |= (PR_STATUSBAR);
}

/*
 * Gain a random spell from the given book (for priests)
 */
static void study_book(int book)
{

    int spell = -1;
    int i, k = 0;

    /* Extract spells */
    for (i = 0; i < SPELLS_PER_BOOK; i++)
    {
        int s = get_spell_from_list(book, i);

        /* Skip non-OK spells */
        if (s == -1) continue;
        if (!spell_okay(s, FALSE)) continue;

        /* Apply the randomizer */
        if ((++k > 1) && (randint0(k) != 0)) continue;

        /* Track it */
        spell = s;
    }

    /* Remember we have used this book */
    object_kind *k_ptr = &k_info[lookup_kind(cp_ptr->spell_book, book)];
    k_ptr->tried = TRUE;

    spell_learn(spell);
}

s16b get_spell_from_list(s16b book, s16b spell)
{
    int realm = get_player_spell_realm();

    if (game_mode == GAME_NPPMORIA)
    {
        /* Check bounds */
        if ((spell < 0) || (spell >= SPELLS_PER_BOOK)) return (-1);
        if ((book < 0) || (book >= BOOKS_PER_REALM_MORIA)) return (-1);

        if (realm == MAGE_REALM) return (spell_list_nppmoria_mage[book][spell]);
        if (realm == PRIEST_REALM) return (spell_list_nppmoria_priest[book][spell]);
    }
    else
    {
        /* Check bounds */
        if ((spell < 0) || (spell >= SPELLS_PER_BOOK)) return (-1);
        if ((book < 0) || (book >= BOOKS_PER_REALM_ANGBAND)) return (-1);

        if (realm == MAGE_REALM) return (spell_list_nppangband_mage[book][spell]);
        if (realm == PRIEST_REALM) return (spell_list_nppangband_priest[book][spell]);
        if (realm == DRUID_REALM) return (spell_list_nppangband_druid[book][spell]);
    }


    /* Whoops! */
    return (-1);
}

/*
 * Helper function to help spells that target traps (disarming, etc...)
 */
bool is_trap_spell(byte spell_book, int spell)
{
    if (spell_book == TV_MAGIC_BOOK)
    {
        if (spell == SPELL_TRAP_DOOR_DESTRUCTION) return (TRUE);
    }
    else if (spell_book == TV_PRAYER_BOOK)
    {
        if (spell == PRAYER_UNBARRING_WAYS) return (TRUE);
    }
    else if (spell_book == TV_DRUID_BOOK)
    {
        if (spell == DRUID_TRAP_DOOR_DESTRUCTION) return (TRUE);

    }
    return (FALSE);
}

/* Adjustment to minimum failure rates for wisdom/intelligence in moria */
static int spell_failure_min_moria(int stat)
{
    int value = p_ptr->state.stat_loaded_cur[stat];

    if (value > 117) 		return(0);
    else if (value > 107)	return(1);
    else if (value > 87)	return(2);
    else if (value > 67)	return(3);
    else if (value > 17)	return(4);
    else if (value > 14)	return(7);
    else if (value > 7)		return(10);
    else	return(25);
}

/*
 * Returns chance of failure for a spell
 */
int spell_chance(int spell)
{
    int chance, minfail;

    const magic_type *s_ptr;

    /* Paranoia -- must be literate */
    if (!cp_ptr->spell_book) return (100);

    /* Get the spell */
    s_ptr = &mp_ptr->info[spell];

    /* Extract the base spell failure rate */
    chance = s_ptr->sfail;

    /* Reduce failure rate by "effective" level adjustment */
    chance -= 3 * (p_ptr->lev - s_ptr->slevel);

    /* Reduce failure rate by INT/WIS adjustment */
    /* Extract the minimum failure rate */
    if (game_mode == GAME_NPPMORIA)
    {
        chance -= 3 * (stat_adj_moria(MORIA_SPELL_STAT)-1);
    }
    else chance -= adj_mag_stat[SPELL_STAT_SLOT];

    /* Not enough mana to cast */
    if (s_ptr->smana > p_ptr->csp)
    {
        chance += 5 * (s_ptr->smana - p_ptr->csp);
    }

    /* Extract the minimum failure rate */
    if (game_mode == GAME_NPPMORIA) minfail = spell_failure_min_moria(MORIA_SPELL_STAT);
    else minfail = adj_mag_fail[SPELL_STAT_SLOT];

    /* Non mage/priest characters never get better than 5 percent */
    if (!(cp_ptr->flags & CF_ZERO_FAIL))
    {
        if (minfail < 5) minfail = 5;
    }

    /* Priest prayer penalty for "edged" weapons (before minfail) */
    if (p_ptr->state.icky_wield)
    {
        chance += 25;
    }

    /* Minimum failure rate */
    if (chance < minfail) chance = minfail;

    /* Stunning makes spells harder (after minfail) */
    if (p_ptr->timed[TMD_STUN] > STUN_HEAVY) chance += 25;
    else if (p_ptr->timed[TMD_STUN]) chance += 15;

    /* Always a 5 percent chance of working */
    if (chance > 95) chance = 95;

    /* Return the chance */
    return (chance);
}

void cast_spell(cmd_arg args)
{
    if (!p_ptr->can_cast()) return;
    if (!p_ptr->has_learned_spells()) return;

    int spell = args.number;
    int dir = args.direction;
    QString noun = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);
    QString verb = cast_spell(MODE_SPELL_VERB, cp_ptr->spell_book, 1, 0);
    bool trap_spell = is_trap_spell(cp_ptr->spell_book, spell);

    // Verify we have access to the spell book;
    if (!spell_okay(spell, TRUE) || !spell_is_available(spell))
    {
        pop_up_message_box(QString("You cannot %1 this %2.") .arg(verb) .arg(noun));
        return;
    }

    // Check for direction if necessary
    if (dir == DIR_UNKNOWN)
    {
        if (spell_needs_aim(cp_ptr->spell_book, spell) && !get_aim_dir(&dir, trap_spell)) return;
        args.direction = dir;
    }

    /* Get the spell */
    const magic_type *s_ptr;s_ptr = &mp_ptr->info[spell];

    /* Verify insufficient mana */
    if (s_ptr->smana > p_ptr->csp)
    {
        /* Warning */
        QString prompt = (QString("You do not have enough mana to %1 this %2.<br>Attempt it anyway?") .arg(verb) .arg(noun));

        /* Verify */
        if (!get_check(prompt)) return;
    }

    //Find the book, and verify its use if necessary


    /* Spell failure chance */
    int chance = spell_chance(spell);

    p_ptr->player_previous_command_update(CMD_CAST, args);

    /* Failed spell */
    if (rand_int(100) < chance)
    {
        message(QString("You failed to get the spell off!"));
    }

    else
    {

        //Actually cast the spell.
        sound(MSG_SPELL);

        p_ptr->message_append_start();

        if (cast_spell(MODE_SPELL_CAST, cp_ptr->spell_book, spell, dir) == NULL) return;

        /* The spell was cast */
        if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
        {
            int e = s_ptr->sexp;

            /* The spell worked */
            p_ptr->spell_flags[spell] |= PY_SPELL_WORKED;

            /* Gain experience */
            gain_exp(e * s_ptr->slevel);
        }
    }

    /* Sufficient mana */
    if (s_ptr->smana <= p_ptr->csp)
    {
        /* Use some mana */
        p_ptr->csp -= s_ptr->smana;
    }

    /* Over-exert the player */
    else
    {
        int oops = s_ptr->smana - p_ptr->csp;

        /* No mana left */
        p_ptr->csp = 0;
        p_ptr->csp_frac = 0;

        /* Message */
        message(QString("You faint from the effort!"));

        /* Hack -- Bypass free action */
        (void)inc_timed(TMD_PARALYZED, randint1(5 * oops + 1), TRUE);

        /* Damage CON (possibly permanently) */
        if (rand_int(100) < 50)
        {
            bool perm = (rand_int(100) < 25);

            /* Message */
            message(QString("You have damaged your health!"));

            /* Reduce constitution */
            (void)dec_stat(A_CON, 15 + randint(10), perm);
        }
    }

    /* Redraw mana */
    p_ptr->redraw |= (PR_SIDEBAR_PL);

    process_player_energy(BASE_ENERGY_MOVE);
}

// Placeholder for use in the player_command menu
void command_cast(cmd_arg arg)
{
    // Are we repeating a spell, or casting for the first time?
    if (p_ptr->command_previous == CMD_CAST) cast_spell(arg);

    // Not repeating a spell
    else do_cmd_cast(arg.item);

}

/* Cast a spell if book choice is -1,
 * need to select book, otherwise it is the book sval
 */
void do_cmd_cast(int book_choice)
{
    if (!character_dungeon) return;
    if (!p_ptr->can_cast()) return;
    if (!p_ptr->has_learned_spells()) return;

    int spell;
    int dir = DIR_UNKNOWN;
    QString noun = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);
    QString verb = cast_spell(MODE_SPELL_VERB, cp_ptr->spell_book, 1, 0);

    //Is a spellbook selected?
    if (book_choice < 0)
    {
        QString q = "Use which book?";
        QString s = "You have no books that you can cast from.";

        /* Restrict the choices */
        item_tester_hook = obj_can_cast;

        int item;

        if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
        {
            /*clear the restriction*/
            item_tester_hook = NULL;
            return;
        }

        /*clear the restriction*/
        item_tester_hook = NULL;

        object_type *o_ptr = object_from_item_idx(item);

        book_choice = o_ptr->sval;
    }

    QString prompt = (QString("Please select a %1 to %2.") .arg(noun) .arg(verb));

    int mode = BOOK_CAST;
    bool cancelled;
    bool cannot;
    SpellSelectDialog(&spell, book_choice, prompt, mode, &cannot, &cancelled);

    // Handle not having a spell to cast
    if (cannot)
    {
        message(QString("You have no %1s that you can %3 right now.") .arg(noun) .arg(verb));
        return;
    }
    else if (cancelled) return;

    bool trap_spell = is_trap_spell(cp_ptr->spell_book, spell);

    if (spell_needs_aim(cp_ptr->spell_book, spell) && !get_aim_dir(&dir, trap_spell)) return;

    cmd_arg args;
    args.wipe();

    args.direction = dir;
    args.number = spell;

    // Set up the spell for repeating
    p_ptr->player_previous_command_update(CMD_CAST, args);

    cast_spell(args);
}

/*
 * See if we can cast or study from a book
 */
bool player_can_use_book(const object_type *o_ptr, bool known)
{
    int i;

    /* Check the player can study at all, and the book is the right type */
    if (!cp_ptr->spell_book) return FALSE;
    if (p_ptr->timed[TMD_BLIND] || no_light()) return FALSE;
    if (p_ptr->timed[TMD_CONFUSED]) return FALSE;
    if (o_ptr->tval != cp_ptr->spell_book) return (FALSE);

    /* Extract spells */
    for (i = 0; i < SPELLS_PER_BOOK; i++)
    {
        int s = get_spell_from_list(o_ptr->sval, i);

        /* Skip non-OK spells */
        if (s == -1) continue;
        if (!spell_okay(s, known)) continue;

        /* We found a spell to study/cast */
        return (TRUE);
    }

    /* No suitable spells */
    return (FALSE);
}

// Study with a specific spellbook in mind
void command_study(cmd_arg arg)
{
    object_type *o_ptr = object_from_item_idx(arg.item);

    do_cmd_study(o_ptr->sval);
}

// Learn a spell
void do_cmd_study(int book_choice)
{
    if (!p_ptr->can_study()) return;

    int spell;
    QString noun = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);

    if (!p_ptr->chooses_spells()) noun = QString("prayer book");

    QString prompt = (QString("Please select a %1 to study.") .arg(noun));

    int mode = BOOK_STUDY;
    bool cancelled;
    bool cannot;
    SpellSelectDialog(&spell, book_choice, prompt, mode, &cannot, &cancelled);

    // Handle not having a spell to learn
    if (cannot)
    {
        message(QString("You have no %1s that you can study right now.") .arg(noun));

        return;
    }
    else if (cancelled) return;

    p_ptr->message_append_start();

    //Actually learn the spell.
    if (p_ptr->chooses_spells()) spell_learn(spell);
    else study_book(spell);
    process_player_energy(BASE_ENERGY_MOVE);
}

// Browse with a specific spellbook in mind.
void command_browse(cmd_arg arg)
{
    object_type *o_ptr = object_from_item_idx(arg.item);

    do_cmd_browse(o_ptr->sval);
}

// Browse the available spellbooks
void do_cmd_browse(int book_choice)
{
    if (!p_ptr->can_cast()) return;

    int spell;
    QString prompt = QString("Press OK when done browsing.");
    int mode = BOOK_BROWSE;
    bool cannot;
    bool cancelled;
    SpellSelectDialog(&spell, book_choice, prompt, mode, &cannot, &cancelled);

    if (cannot) message(QString("You have no books that you can read."));
}

/*
 * Determine if an object is a spellbook with spells that can be cast
 */
bool obj_can_cast(object_type *o_ptr)
{
    int i;
    if (o_ptr->tval != cp_ptr->spell_book) return FALSE;

    for (i = 0;  i < SPELLS_PER_BOOK; i++)
    {
        int spell = get_spell_from_list(o_ptr->sval, i);

        /* Not a spell */
        if (spell == -1) continue;

        /* Is there a spell we can learn? */
        if (spell_okay(spell, TRUE)) return (TRUE);
    }
    return (FALSE);
}
