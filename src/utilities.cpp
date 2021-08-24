/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Leon Marrick, Bahman Rabbi, Diego Gonzalez, Jeff Greene
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include "src/utilities.h"
#include <QInputDialog>
#include <QLineEdit>
#include <QVBoxLayout>
#include <QDialogButtonBox>
#include <QPushButton>
#include <QLabel>

static letters_and_numbers lowercase_and_numbers[26] =
{
    { 'a', 0},
    { 'b', 1},
    { 'c', 2},
    { 'd', 3},
    { 'e', 4},
    { 'f', 5},
    { 'g', 6},
    { 'h', 7},
    { 'i', 8},
    { 'j', 9},
    { 'k', 10},
    { 'l', 11},
    { 'm', 12},
    { 'n', 13},
    { 'o', 14},
    { 'p', 15},
    { 'q', 16},
    { 'r', 17},
    { 's', 18},
    { 't', 19},
    { 'u', 20},
    { 'v', 21},
    { 'w', 22},
    { 'x', 23},
    { 'y', 24},
    { 'z', 25}
};


QString _num(int n)
{
    return QString::number(n);
}

/*
 * Returns a "rating" of x depending on y, and sets "attr" to the
 * corresponding "attribute".
 */
QString likert(int x, int y, byte *attr)
{
    /* Paranoia */
    if (y <= 0) y = 1;

    /* Negative value */
    if (x < 0)
    {
        *attr = TERM_RED;
        return ("Very Bad");
    }

    /* Analyze the value */
    switch ((x / y))
    {
        case 0:
        case 1:
        {
            *attr = TERM_RED;
            return ("Bad");
        }
        case 2:
        {
            *attr = TERM_RED;
            return ("Poor");
        }
        case 3:
        case 4:
        {
            *attr = TERM_YELLOW;
            return ("Fair");
        }
        case 5:
        {
            *attr = TERM_YELLOW;
            return ("Good");
        }
        case 6:
        {
            *attr = TERM_YELLOW;
            return ("Very Good");
        }
        case 7:
        case 8:
        {
            *attr = TERM_L_GREEN;
            return ("Excellent");
        }
        case 9:
        case 10:
        case 11:
        case 12:
        case 13:
        {
            *attr = TERM_L_GREEN;
            return ("Superb");
        }
        case 14:
        case 15:
        case 16:
        case 17:
        {
            *attr = TERM_L_GREEN;
            return ("Heroic");
        }
        default:
        {
            *attr = TERM_L_GREEN;
            return ("Legendary");
        }
    }
}

// There is probably a better way with QChar, but I can't find it.
int letter_to_number (QChar let)
{
   // Make sure we are dealing with lowercase letters.
    let = let.toLower();
    if (!let.isLower()) return 0;

    for (int i = 0; i < 26; i++)
    {
        letters_and_numbers *ln_ptr =& lowercase_and_numbers[i];
        if (ln_ptr->let == let) return (ln_ptr->num);
    }

    /* all else - just return zero*/
    return 0;
}

// There is probably a better way with QChar, but I can't find it.
QChar number_to_letter (int num)
{
    if (num >=26) return ('a');

    for (int i = 0; i < 26; i++)
    {
        letters_and_numbers *ln_ptr =& lowercase_and_numbers[i];
        if (ln_ptr->num == num) return (ln_ptr->let);
    }

    /* all else - Paranoia*/
    return ('a');
}

//Convert a long number to string number formatted with commas
QString number_to_formatted_string(s32b number)
{
    bool is_negative = TRUE;
    if (number >= 0) is_negative = FALSE;

    if (!number) return (QString("0"));

    QString formatted_num;
    formatted_num.clear();

    //preserve the original number
    s32b working_num = number;
    if (working_num < 0) working_num = working_num * -1;

    while (working_num >=1000)
    {
        s32b remainder = (working_num % 1000);
        QString temp_string = (QString("%1") .arg(remainder));
        if (remainder < 100) temp_string.prepend('0');
        if (remainder < 10)  temp_string.prepend('0');
        temp_string.prepend(",");
        working_num = working_num / 1000;

        formatted_num.prepend(temp_string);
    }

    formatted_num.prepend(QString("%1") .arg(working_num));
    if (is_negative) formatted_num.prepend('-');

    return(formatted_num);
}

bool is_a_vowel(QChar single_letter)
{
    // Make sure we are dealing with lowercase letters.
    single_letter = single_letter.toLower();
    if (!single_letter.isLower()) return FALSE;

    if (single_letter == 'a') return TRUE;
    if (single_letter == 'e') return TRUE;
    if (single_letter == 'i') return TRUE;
    if (single_letter == 'o') return TRUE;
    if (single_letter == 'u') return TRUE;

    return (FALSE);
}

bool begins_with_vowel(QString line)
{
    // Paranoia
    if (line.isEmpty()) return (FALSE);
    QChar first = line[0];
    return (is_a_vowel(first));
}

// Capitilize the first character in a string.
QString capitalize_first(QString line)
{
    // Paranoia
    if (line.isEmpty()) return (line);
    QChar first = line[0];
    first = first.toTitleCase();
    line[0] = first;
    return (line);
}

QString format_stat(s16b value)
{
    QString text;
    if (value > 0) text.append('+');
    return QString("<b>%1%2</b>").arg(text).arg(value);
}

void pop_up_message_box(QString message, QMessageBox::Icon the_icon)
{
    QMessageBox msg_box;
    msg_box.setModal(true);
    msg_box.setIcon(the_icon);
    msg_box.setText(message);
    msg_box.exec();
}

void pop_up_message_box(QString message, QPixmap into_icon)
{
    QMessageBox msg_box;
    msg_box.setModal(true);
    msg_box.setIconPixmap(into_icon);
    msg_box.setText(message);
    msg_box.exec();
}

// Post a question to the player and wait for a yes/no response.
// return TRUE/FALSE;
bool get_check(QString question)
{
    int answer = QMessageBox::question(0, "Please respond yes or no", question, QMessageBox::Yes, QMessageBox::No);
    if (answer == QMessageBox::Yes) return (TRUE);
    return (FALSE);
}

// Post a question to the player and wait for player to enter a string.
// returns the string;
QString get_string(QString question, QString description, QString answer)
{

    bool ok;
    QString text = QInputDialog::getText(0, question, description, QLineEdit::Normal, answer, &ok, Qt::Dialog, 0);

    if (!ok) return NULL;

    return (text);
}

// Return the maximum value
void GetQuantityDialog::max_number_button(void)
{
    current_quantity = this_quantity->maximum();
    this->accept();
}

// return the minimum value
void GetQuantityDialog::min_number_button(void)
{
    current_quantity = this_quantity->minimum();
    this->accept();
}

void GetQuantityDialog::update_quantity(int new_value)
{
    current_quantity = new_value;
}

GetQuantityDialog::GetQuantityDialog(QString prompt, int min, int max, int value)
{
    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;

    QPointer<QLabel> header_main = new QLabel(QString("<b><h2>%1</h2></b>") .arg(prompt));
    header_main->setAlignment(Qt::AlignCenter);
    main_layout->addWidget(header_main);

    setLayout(main_layout);
    setWindowTitle(tr("Please select a quantity:"));

    this_quantity = new QSpinBox;
    this_quantity->setRange(min, max);
    this_quantity->setValue(value);

    current_quantity = value;

    main_layout->addWidget(this_quantity);

    connect(this_quantity, SIGNAL(valueChanged(int)), this, SLOT(update_quantity(int)));

    // Add buttons for min value, max value, OK, and cancel
    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox();
    QPointer<QPushButton> min_button = new QPushButton();
    min_button->setText(QString("Min - %1") .arg(min));
    min_button->setToolTip("Use the minimum possible value");
    connect(min_button, SIGNAL(clicked()), this, SLOT(min_number_button()));
    buttons->addButton(min_button, QDialogButtonBox::ActionRole);
    QPointer<QPushButton> max_button = new QPushButton();
    max_button->setText(QString("Max - %1") .arg(max));
    max_button->setToolTip("Use the maximum possible value");
    connect(max_button, SIGNAL(clicked()), this, SLOT(max_number_button()));
    buttons->addButton(max_button, QDialogButtonBox::ActionRole);
    buttons->addButton(QDialogButtonBox::Ok);
    buttons->addButton(QDialogButtonBox::Cancel);
    connect(buttons, SIGNAL(accepted()), this, SLOT(accept()));
    connect(buttons, SIGNAL(rejected()), this, SLOT(reject()));
    main_layout->addWidget(buttons);

    setLayout(main_layout);
}



/*
 * Request a "quantity" from the user
 */
s16b get_quantity(QString prompt, int max, int amt, bool allow_zero)
{
    if (!amt) amt = 1;
    int min = 1;
    if (allow_zero) min = 0;
    if (max == 1) return (1);

    /* Build a prompt if needed */
    if (prompt.isEmpty())
    {
        /* Build a prompt */
        prompt = (QString("Please enter a quantity "));
    }
    prompt.append(QString(" (%1-%2)") .arg(min) .arg(max));

    GetQuantityDialog dlg(prompt, min, max, amt);

    if (!dlg.exec()) amt = 0;
    else amt = dlg.current_quantity;

    /* Return the result */
    return (amt);
}


void GetQuantitySlider::update_quantity(int new_value)
{
    current_quantity = new_value;
    if (slider_quantity->value() != new_value)
    {
        slider_quantity->setValue(new_value);
    }
    if (spin_quantity->value() != new_value)
    {
        spin_quantity->setValue(new_value);
    }
}

// Select a qualtity using a slider as well as combo box.
GetQuantitySlider::GetQuantitySlider(QString prompt, QString unit, int min, int max, int value)
{
    // Paranoia
    if (value < min) value = min;
    if (value > max) value = max;

    QPointer<QVBoxLayout> main_layout = new QVBoxLayout;

    QPointer<QLabel> header_main = new QLabel(QString("<b>%1 (%2-%3)</b>") .arg(prompt) .arg(min) .arg(max));
    header_main->setAlignment(Qt::AlignCenter);
    main_layout->addWidget(header_main);

    setLayout(main_layout);
    setWindowTitle(tr("Please select a value:"));
    QPointer<QHBoxLayout> main_hlay = new QHBoxLayout;
    main_layout->addLayout(main_hlay);

    spin_quantity = new QSpinBox;
    spin_quantity->setRange(min, max);
    spin_quantity->setValue(value);

    slider_quantity = new QSlider(Qt::Horizontal);
    slider_quantity->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
    slider_quantity->setRange(min, max);
    slider_quantity->setValue(value);

    current_quantity = value;

    main_hlay->addWidget(spin_quantity);
    main_hlay->addWidget(slider_quantity);

    connect(spin_quantity, SIGNAL(valueChanged(int)), this, SLOT(update_quantity(int)));
    connect(slider_quantity, SIGNAL(valueChanged(int)), this, SLOT(update_quantity(int)));

    if (unit.length())
    {
        QPointer<QLabel> this_label = new QLabel(QString("<b>%1</b>") .arg(unit));
        main_hlay->addWidget(this_label);
    }

    // Add buttons for min value, max value, OK, and cancel
    QPointer<QDialogButtonBox> buttons = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
    connect(buttons, SIGNAL(accepted()), this, SLOT(accept()));
    connect(buttons, SIGNAL(rejected()), this, SLOT(reject()));
    main_layout->addWidget(buttons);

    setLayout(main_layout);
}

s16b get_quantity_slider(QString prompt, QString unit, int min, int max, int value)
{
    GetQuantitySlider dlg(prompt, unit, min, max, value);

    if (!dlg.exec()) return (value);
    else return (dlg.current_quantity);
}


QColor add_preset_color(int which_color)
{
    QColor color;

    color.setRgb(preset_colors[which_color].red, preset_colors[which_color].green, preset_colors[which_color].blue, 255);

    return (color);
}

QString html_string_to_plain_text(QString text)
{
    text.remove(QRegExp("<[^>]*>"));
    return (text);
}

// Get an html string to a certain length, not factoring in the html tags
// This can only make a string longer, not shorter
QString set_html_string_length(QString html_string, int length, bool prepend)
{
    QString plain_string = html_string_to_plain_text(html_string);

    while (plain_string.length() < length)
    {
        if (prepend)    html_string.prepend(" ");
        else            html_string.append(" ");

        if (prepend)    plain_string.prepend(" ");
        else            plain_string.append(" ");
    }

    return (html_string);
}


// Returns a QString in any 16 bit color, in HTML format
QString color_string(QString msg, QColor which_color)
{
    return (QString("<font color='%1'>%2</font>").arg(which_color.name()).arg(msg));
}

/*
 *  returns a QString with a preset color
 *  Colors are in 24-bit hex RGB format (#000000 - #FFFFFF)
 *  Keep this current with preset_colors table in tables.c.
 */
QString color_string(QString msg, byte color_num)
{
    // Paranoia
    if (color_num >= MAX_COLORS) color_num = TERM_WHITE;

    QColor msg_color = defined_colors[color_num];

    return (color_string(msg, msg_color));
}

// Returns a character in any 16 bit color, in HTML format
QString color_char(QChar which_char, QColor which_color)
{
    return (QString("<font color='%1'>%2</font>").arg(which_color.name()).arg(which_char));
}

/*
 *  returns a character with a preset color
 *  Colors are in 24-bit hex RGB format (#000000 - #FFFFFF)
 *  Keep this current with preset_colors table in tables.c.
 */
QString color_char(QChar which_char, byte color_num)
{
    // Paranoia
    if (color_num >= MAX_COLORS) color_num = TERM_WHITE;

    QColor msg_color = defined_colors[color_num];

    return (color_char(which_char, msg_color));
}


static bool repeat_prev_allowed;

void cmd_enable_repeat(void)
{
    repeat_prev_allowed = TRUE;
}

void cmd_disable_repeat(void)
{
    repeat_prev_allowed = FALSE;
}

/*
 * Return object weight in a "XX.X" format.
 * The output is automatically formatted to be 6 characters long.
 */
QString format_object_weight(object_type *o_ptr)
{
    int object_weight = o_ptr->weight * o_ptr->number;

    QString formatted_weight = (QString("%1.%2") .arg(object_weight/10) .arg(object_weight % 10));

    while (formatted_weight.length() < 6) formatted_weight.prepend(" ");
    if (object_weight == 10) formatted_weight.append(" lb");
    else formatted_weight.append(" lbs");

    return (formatted_weight);
}

QString formatted_weight_string(s32b weight)
{
    return (QString("%1.%2") .arg(weight / 10) .arg(weight % 10));
}

/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
static void ang_sort_aux(void *u, void *v, int p, int q)
{
    int z, a, b;

    /* Done sort */
    if (p >= q) return;

    /* Pivot */
    z = p;

    /* Begin */
    a = p;
    b = q;

    /* Partition */
    while (TRUE)
    {
        /* Slide i2 */
        while (!(*ang_sort_comp)(u, v, b, z)) b--;

        /* Slide i1 */
        while (!(*ang_sort_comp)(u, v, z, a)) a++;

        /* Done partition */
        if (a >= b) break;

        /* Swap */
        (*ang_sort_swap)(u, v, a, b);

        /* Advance */
        a++, b--;
    }

    /* Recurse left side */
    ang_sort_aux(u, v, p, b);

    /* Recurse right side */
    ang_sort_aux(u, v, b+1, q);
}


/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort(void *u, void *v, int n)
{
    /* Sort the array */
    ang_sort_aux(u, v, 0, n-1);
}



QString get_player_title(void)
{
    if(p_ptr->is_wizard) return ("--WIZARD--");

    if (p_ptr->total_winner) return(sp_ptr->winner);

    if (game_mode == GAME_NPPMORIA)
    {
        return (cp_ptr->cl_title[p_ptr->lev-1]);
    }

    return (cp_ptr->cl_title[(p_ptr->lev - 1) / 5]);
}

void debug_rarities()
{
    if (!character_dungeon) return;

    /* TODO PLAYTESTING
    int n = 0;
    for (int y = 0; y < p_ptr->cur_map_hgt; y++) {
        for (int x = 0; x < p_ptr->cur_map_wid; x++) {
            if (dungeon_info[y][x].feat == 0) {
                ++n;
                color_message(QString("NONE terrain %1x%2").arg(y).arg(x), TERM_RED);
            }
        }
    }*/

    /*
    n = 0;
    for (int i = 1; i < z_info->f_max; i++) {
        feature_type *f_ptr = f_info + i;
        if ((f_ptr->f_flags1 & FF1_DOOR) && (f_ptr->f_flags3 & FF3_DOOR_LOCKED)) {
            ++n;
        }
    }
    if (n == 0) color_message("There is not locked doors", TERM_RED);

    for (int i = 0; i < z_info->art_max; i++) {
        artifact_type *a_ptr = a_info + i;
        if (a_ptr->tval == 0 and a_ptr->sval) {
            popup1(QString("Invalid artifact: %1 %2").arg(a_ptr->a_name).arg(a_ptr->sval), 53);
            break;
        }
    }

    n = 0;
    for (int i = 0; i < mon_max; i++) {
        monster_type *m_ptr = mon_list + i;
        if (m_ptr->r_idx == 0) continue;
        monster_race *r_ptr = r_info + m_ptr->r_idx;
        if (r_ptr->level == 0) ++n;
    }
    if (n > 0) message(QString("monsters: %1").arg(n));

    if (monster_level == 0 && p_ptr->depth > 0) message("?????");
    */
}

// Make a color darker if it's too bright
QColor make_color_readable(QColor clr)
{
    // Gray
    if (clr.red() == clr.green() && clr.green() == clr.blue()) {
        return QColor("black");
    }
    clr = clr.toHsv();
    int value = MIN(clr.value(), 150);
    int saturation = 255;
    clr.setHsv(clr.hue(), saturation, value, 255);
    return clr.toRgb();
}

// For object listings
QColor get_object_color(object_type *o_ptr)
{
    int idx = tval_to_attr[o_ptr->tval];
    QColor clr = defined_colors[idx];
    return make_color_readable(clr);
}








// Display an actual window with the information, sybmol, and tile
void display_info_window(byte mode, int index, QString info)
{
    QString tile_id;
    QMessageBox message_box;
    message_box.setText(info);
    message_box.setStandardButtons(QMessageBox::Ok);
    message_box.setDefaultButton(QMessageBox::Ok);
    message_box.setInformativeText("Press 'OK' to continue.");
    message_box.setStyleSheet("background-color: lightGray;");

    //Get the pixmap, depending on if we are displaying an object, terrain, or monster.
    if (mode == DISPLAY_INFO_FEATURE)
    {
        index = f_info[index].f_mimic;
        feature_type *f_ptr = &f_info[index];
        tile_id = f_ptr->tile_id;
    }
    else if (mode == DISPLAY_INFO_MONSTER)
    {
        monster_race *r_ptr = &r_info[index];
        tile_id = r_ptr->tile_id;
    }
    else if (mode == DISPLAY_INFO_OBJECT)
    {
        object_kind *k_ptr = &k_info[index];
        tile_id = k_ptr->get_tile_id();
    }
    // Whoops!
    else return;
    message_box.setIconPixmap(ui_get_tile(tile_id, TRUE));

    message_box.show();
    message_box.exec();

}

class Repl {
public:
    QString what;
    QString with;
    Repl(QString a, QString b);
};

Repl::Repl(QString a, QString b)
{
    what = a;
    with = b;
}

// A list of unicode characters to convert to ASCII.
//  Clearly not an inclusive list, but it should work to convert the game's monster, artifact, and ego-item names.
QString to_ascii(QString src)
{
    QString original        = "àáâãäåĀÁÂÃÄÅçćċčÇĆĊČďđÐĎèéêëÈÉÊËìíîïÌÍÎÏñÑòóôõöøðÓÔÕÖØÒùúûüÙÚÛÜýÿÝ";
    QString replacements    = "aaaaaaAAAAAAccccCCCCddDDeeeeEEEEiiiiIIIInNooooooOOOOOOOuuuuUUUUyyY";

    for (int i = 0; i < original.length(); i++)
    {
        src = src.replace(original[i], replacements[i], Qt::CaseSensitive);
    }

    return src;
}

// The QT version doesn't work all that great
bool strings_match(QString string1, QString string2)
{
    if (string1.isEmpty() || string2.isEmpty()) return (FALSE);
    if (string1.length() != string2.length()) return (FALSE);
    if (!string1.contains(string2)) return (FALSE);
    if (!string2.contains(string1)) return (FALSE);
    return (TRUE);
}

/*
 * Converts stat num into a six-char (right justified) string
 */
QString cnv_stat(int val)
{
    QString str;

    /* Above 18 */
    if (val > 18)
    {
        int bonus = (val - 18);

        if (game_mode == GAME_NPPMORIA)
        {
            if (bonus > 99) bonus = 100;
        }

        if (bonus >= 220)
            str = "18/***";
        else if (bonus >= 100)
            str = QString("18/%1").arg(_num(bonus), 3, '0');
        else
            str = QString(" 18/%1").arg(_num(bonus), 2, '0');
    }

    /* From 3 to 18 */
    else
    {
        str = QString("%1").arg(_num(val), 6, ' ');
    }

    return str;
}


// Write a note to the notes file
void write_note(QString note, s16b depth)
{
    // Hack - no blank notes
    if (!note.length()) return;

    notes_type note_body;
    notes_type *notes_ptr = &note_body;

    notes_ptr->game_turn = p_ptr->game_turn;
    notes_ptr->dun_depth = depth;
    notes_ptr->player_level = p_ptr->lev;
    notes_ptr->recorded_note = note;

    notes_log.append(note_body);
}

static bool is_roman_numeral_char(QChar letter)
{
    QString numerals = "IVXLCDM";

    if (numerals.contains(letter, Qt::CaseSensitive)) return (TRUE);
    return (FALSE);
}

// Return the roman number segment of a string
QString find_roman_numeral(QString full_name)
{
    QString roman_numeral = NULL;
    QString name = full_name;
    QString space = QString(" ");

    // Require a space
    if (!name.contains(space)) return NULL;

    //Start analyzing after the first space
    name.remove(0, name.indexOf(' '));

    while (name.length())
    {
        QChar first = name[0];

        // If we find a roman numeral char, start recording
        if (is_roman_numeral_char(first))
        {
            roman_numeral.append(first);
        }
        // If the roman numeral stops, quit.
        else if (roman_numeral.length()) break;

        // Break when a space if found.
        if (operator==(first, space) && roman_numeral.length()) break;

        // Cut off the first letter
        name.remove(0, 1);
    }
    return (roman_numeral);
}

/*----- Roman numeral functions  ------*/

/*
 * Converts an arabic numeral (int) to a roman numeral (char *).
 *
 * An arabic numeral is accepted in parameter `n`, and the corresponding
 * upper-case roman numeral is placed in the parameter `roman`.  The
 * length of the buffer must be passed in the `bufsize` parameter.  When
 * there is insufficient room in the buffer, or a roman numeral does not
 * exist (e.g. non-positive integers) a value of 0 is returned and the
 * `roman` buffer will be the empty string.  On success, a value of 1 is
 * returned and the zero-terminated roman numeral is placed in the
 * parameter `roman`.
 */
QString int_to_roman(int n)
{
    QString roman;
    /* Roman symbols */
    QString roman_symbol_labels[13] =
        {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX",
         "V", "IV", "I"};
    int  roman_symbol_values[13] =
        {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};

    /* Clear the roman numeral buffer */
    roman.clear();

    /* Roman numerals have no zero or negative numbers */
    if (n < 1)
        return NULL;

    /* Build the roman numeral in the buffer */
    while (n > 0)
    {
        int i = 0;

        /* Find the largest possible roman symbol */
        while (n < roman_symbol_values[i]) i++;

        roman.append(roman_symbol_labels[i]);

        /* Decrease the value of the arabic numeral */
        n -= roman_symbol_values[i];
    }

    return (roman);
}




/*
 * Converts a roman numeral to an arabic numeral (int).
 *
 * The null-terminated roman numeral is accepted in the `roman`
 * parameter and the corresponding integer arabic numeral is returned.
 * Only upper-case values are considered. When the `roman` parameter
 * is empty or does not resemble a roman numeral, a value of -1 is
 * returned.
 *
 * XXX This function will parse certain non-sense strings as roman
 *     numerals, such as IVXCCCVIII
 */
int roman_to_int(QString roman)
{

    int n = 0;

    QString numerals = "IVXLCDM";
    QString roman_symbol_labels[13] =
        {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX",
         "V", "IV", "I"};
    int  roman_symbol_values[13] =
        {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};

    if (!roman.length()) return -1;

    while (roman.length())
    {
        bool found = FALSE;
        for (int i = 0; i < 13; i++)
        {
            if (roman.indexOf(roman_symbol_labels[i]) != 0) continue;

            // We have a match
            n += roman_symbol_values[i];
            roman.remove(0, roman_symbol_labels[i].length());
            found = TRUE;
            break;
        }
        if (!found) roman.remove(0,1);
    }

    return n;
}

// Completely clear out a layout.
// Use recursion to clear out layouts inside of layouts
void clear_layout(QLayout* layout)
{
    // Paranoia - Layout doesn't exist.
    if (!layout) return;

    while (QLayoutItem* item = layout->takeAt(0))
    {
        if (QWidget* widget = item->widget()) delete widget;

        if (QLayout* childLayout = item->layout())
        {
            clear_layout(childLayout);
        }
        delete item;
    }
}

