// File: util.cpp
// Purpose: Utumno utilities -BEN-


#include "utumno.h"


/*
 * Log something to debug.log
 */
void debug_log(char *str)
{
    FILE *f = fopen("debug.log", "at");
    fprintf(f, "%s", str);
    fclose(f);
}


/*
 * Write out options to a pref file
 */
void write_options(void)
{
    // Open
    FILE *fff = fopen("dat/utumno.prf", "wt");

    // Write
    for (int i = 0; options[i].o_desc; i++) {
        fprintf(fff, "%s %d\n", options[i].o_text, *(options[i].o_var));
    }

    // Close
    fclose(fff);
}


/*
 * Read options from a pref file
 */
void read_options(void)
{
    char buf[128], o[128];
    int val;

    // Open
    FILE *fff = fopen("dat/utumno.prf", "rt");

    // Read
    for (;;) {
        // Get a line
        fgets(buf, 127, fff);

        // End of file?
        if (feof(fff)) break;

        // Scan it for info
        sscanf(buf, "%s %d", o, &val);

        // Search
        for (int i = 0; options[i].o_desc; i++) {
            if (streq(options[i].o_text, o)) {
                *(options[i].o_var) = val;
            }
        }
    }

    // Close
    fclose(fff);
}


// Formatted text
// DO NOT USE IF STRING IS >2048 CHARS
void format_text(int x1, int x2, int y, char *str, byte color, byte font)
{
    char c, *buf, *p, *q, temp[128];
    int words, cur_x, width, i;
    byte word_now;
    
    buf = new char[2048];

    // Read words in
    p = buf;
    q = str;
    word_now = FALSE;
    words = 0;
    for (;;) {
        c = *q++;
        if (!c) break;
        if (isspace(c)) {
            if (word_now) {
                *p++ = ' ';
                word_now = FALSE;
            }
        }
        else {
            if (word_now) {
                *p++ = c;
            }
            else {
                words++;
                *p++ = c;
                word_now = TRUE;
            }
        }
    }
    *p++ = ' ';
    *p = 0;

    // Display
    p = buf;
    cur_x = x1;
    for (i = 0; i < words; i++) {
        // Get the word into temp
        q = temp;
        while (*p != ' ') *q++ = *p++;
        *q = 0;
        p++;

        // Add a space
        strcat(temp, " ");
                
        // Get width
        width = string_width(temp, font);

        // Does it fit?
        if (cur_x+width < x2) {
            put_text(cur_x, y, temp, color, font);
            cur_x += width;
        }
        else {
            cur_x = x1;
            y += 16;
            put_text(cur_x, y, temp, color, font);
            cur_x += width;
        }
    }

    delete[] buf;
}


/*
 * Determine if string "t" is equal to string "t"
 */
bool streq(char *a, char *b)
{
    return !strcmp(a, b);
}


/*
 * Determine if string "t" is a suffix of string "s"
 */
bool suffix(char *s, char *t)
{
    int tlen = strlen(t);
    int slen = strlen(s);

    /* Check for incompatible lengths */
    if (tlen > slen) return (FALSE);

    /* Compare "t" to the end of "s" */
    return streq(s + slen - tlen, t);
}


/*
 * Determine if string "t" is a prefix of string "s"
 */
bool prefix(char *s, char *t)
{
    /* Scan "t" */
    while (*t) {
        /* Compare content and length */
        if (*t++ != *s++) return FALSE;
    }

    /* Matched, we have a prefix */
    return TRUE;
}


/*
 * Extract a "parsed" path from an initial filename
 *
 * This requires no special processing on simple machines,
 * except for verifying the size of the filename.
 */
errr path_parse(char *buf, int max, char *file)
{
    /* Accept the filename */
    strnfmt(buf, max, "%s", file);

    /* Success */
    return (0);
}


/*
 * Hack -- acquire a "temporary" file name if possible
 */
errr path_temp(char *buf, int max)
{
    /* Extract a path */

// -- MV
// We prefer the use of mkstemp instead of tmpnam

	static char template_buffer[20];
	strcpy(template_buffer,"utumno.tmp.XXXXXX");
#ifdef LINUX
	mkstemp(template_buffer);
#else
	tmpnam(template_buffer);
#endif
        return (path_parse(buf, max, template_buffer));
}


/*
 * Hack -- replacement for "fopen()"
 */
FILE *my_fopen(char *file, char *mode)
{
    char buf[1024];

    /* Hack -- Try to parse the path */
    if (path_parse(buf, 1024, file)) return (NULL);

    /* Attempt to fopen the file anyway */
    return (fopen(buf, mode));
}


/*
 * Hack -- replacement for "fgets()"
 *
 * Read a string, without a newline, from a file
 *
 * Process tabs, strip internal non-printables
 */
errr my_fgets(FILE *fff, char *buf, huge n)
{
    int i = 0;

    char *s;

    char tmp[1024];

    /* Read a line */
    if (fgets(tmp, 1024, fff)) {
        /* Convert weirdness */
        for (s = tmp; *s; s++) {
            /* Handle newline */
            if (*s == '\n') {
                /* Terminate */
                buf[i] = '\0';

                /* Success */
                return (0);
            }

            /* Handle tabs */
            else if (*s == '\t') {
                /* Hack -- require room */
	// -- MV : Inserting a cast to (huge) in order to avoid compile warning
				if ( (huge) i + 8 >= n) break;

                /* Append a space */
                buf[i++] = ' ';

                /* Append some more spaces */
                while (!(i % 8)) buf[i++] = ' ';
            }

            /* Handle printables */
            else if (isprint(*s)) {
                /* Copy */
                buf[i++] = *s;

                /* Check length */
	// -- MV : Inserting a cast to (huge) in order to avoid compile warning
                if ( (huge) i >= n) break;
            }
        }
    }

    /* Nothing */
    buf[0] = '\0';

    /* Failure */
    return 1;
}


/*
 * Hack -- attempt to delete a file
 */
errr fd_kill(char *file)
{
    char                buf[1024];

    /* Hack -- Try to parse the path */
    if (path_parse(buf, 1024, file)) return (-1);

    /* Remove */
    (void)remove(buf);

    /* XXX XXX XXX */
    return (0);
}


/*
 * Hack -- attempt to move a file
 */
errr fd_move(char *file, char *what)
{
    char buf[1024];
    char aux[1024];

    /* Hack -- Try to parse the path */
    if (path_parse(buf, 1024, file)) return (-1);

    /* Hack -- Try to parse the path */
    if (path_parse(aux, 1024, what)) return (-1);

    /* Rename */
    (void)rename(buf, aux);

    /* XXX XXX XXX */
    return (0);
}


/*
 * Hack -- attempt to copy a file
 */
errr fd_copy(char *file, char *what)
{
    char buf[1024];
    char aux[1024];

    /* Hack -- Try to parse the path */
    if (path_parse(buf, 1024, file)) return (-1);

    /* Hack -- Try to parse the path */
    if (path_parse(aux, 1024, what)) return (-1);

    /* Copy XXX XXX XXX */
    /* (void)rename(buf, aux); */

    /* XXX XXX XXX */
    return (1);
}


/*
 * Hack -- attempt to open a file descriptor (create file)
 */
int fd_make(char *file, int mode)
{
    char buf[1024];

    /* Hack -- Try to parse the path */
    if (path_parse(buf, 1024, file)) return (-1);

    /* Attempt to open the file */
    return (creat(buf, mode));

    /* We could probably simply use the following call XXX XXX XXX */
    /* return (open(buf, O_WRONLY | O_CREAT | O_TRUNC | O_EXCL, mode)); */
}


/*
 * Hack -- attempt to open a file descriptor (existing file)
 */
int fd_open(char *file, int flags)
{
    char buf[1024];

    /* Hack -- Try to parse the path */
    if (path_parse(buf, 1024, file)) return (-1);

    /* Attempt to open the file */
    return (open(buf, flags, 0));
}


/*
 * Hack -- attempt to lock a file descriptor
 *
 * Legal lock types -- F_UNLCK, F_RDLCK, F_WRLCK
 */
errr fd_lock(int fd, int what)
{
    /* XXX XXX */
    what = what ? what : 0;

    /* Verify the fd */
    if (fd < 0) return (-1);

    /* Success */
    return (0);
}


/*
 * Hack -- attempt to seek on a file descriptor
 */
errr fd_seek(int fd, huge n)
{
    long p;

    /* Verify fd */
    if (fd < 0) return (-1);

    /* Seek to the given position */
    p = lseek(fd, n, SEEK_SET);

    /* Failure */
    if (p < 0) return (1);

    /* Failure */
// -- MV : Inserting a cast to (huge) in order to avoid compile warning
    if ( (huge) p != n) return (1);

    /* Success */
    return (0);
}


/*
 * Hack -- attempt to read data from a file descriptor
 */
errr fd_read(int fd, char *buf, huge n)
{
    // Verify the fd
    if (fd < 0) return (-1);

    // Read pieces
    while (n >= 16384) {
        // Read a piece
        if (read(fd, buf, 16384) != 16384) return (1);

        // Shorten the task
        buf += 16384;

        // Shorten the task
        n -= 16384;
    }

    // Read the final piece
// -- MV : Inserting a cast to (huge) in order to avoid compile warning
    if ( (huge) read(fd, buf, n) != n) return (1);

    // Success
    return (0);
}


/*
 * Hack -- Attempt to write data to a file descriptor
 */
errr fd_write(int fd, char *buf, huge n)
{
    /* Verify the fd */
    if (fd < 0) return (-1);

    /* Write pieces */
    while (n >= 16384) {
        /* Write a piece */
        if (write(fd, buf, 16384) != 16384) return (1);

        /* Shorten the task */
        buf += 16384;

        /* Shorten the task */
        n -= 16384;
    }

    /* Write the final piece */
// -- MV : Inserting a cast to (huge) in order to avoid compile warning
    if ( (huge) write(fd, buf, n) != n) return (1);

    /* Success */
    return (0);
}


/*
 * Hack -- attempt to close a file descriptor
 */
errr fd_close(int fd)
{
    /* Verify the fd */
    if (fd < 0) return (-1);

    /* Close */
    close(fd);

    /* XXX XXX XXX */
    return (0);
}




static char *messages[MESSAGE_LIMIT];
static s32b message_when[MESSAGE_LIMIT];

/*
 * Put a message into the message buffer so it gets shown to the player
 * for a while.
 *
 * Messages go away after 200 turns (see get_old_message).
 */
void msg_print(char *msg)
{
    if (msg) {
        if (messages[0]) delete[] messages[0];
        for (int i = 0; i < MESSAGE_LIMIT-1; i++) {
            messages[i] = messages[i+1];
            message_when[i] = message_when[i+1];
        }
        messages[MESSAGE_LIMIT-1] = new char[strlen(msg)+1];
        strcpy(messages[MESSAGE_LIMIT-1], msg);
        message_when[MESSAGE_LIMIT-1] = game_turn;
        console_print(msg);
    }
}


/*
 * Display a formatted message, using "vstrnfmt()" and "msg_print()".
 */
void msg_format(char *fmt, ...)
{
    va_list vp;
    char buf[1024];

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Format the args, save the length */
    vstrnfmt(buf, 1024, fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Display */
    msg_print(buf);
}


/*
 * Get an old message
 */
char *get_old_message(int i)
{
    if (i < 0) return NULL;
    if (i >= MESSAGE_LIMIT) return NULL;
    if (game_turn >= message_when[i]+20) return NULL;
    return messages[i];
}



/*
 * Get some input at the cursor location.
 * Assume the buffer is initialized to a default string.
 * Note that this string is often "empty" (see below).
 * The default buffer is displayed in yellow until cleared.
 * Pressing RETURN right away accepts the default entry.
 * Normal chars clear the default and append the char.
 * Backspace clears the default or deletes the final char.
 * ESCAPE clears the buffer and the window and returns FALSE.
 * RETURN accepts the current buffer contents and returns TRUE.
 */
bool askfor_aux(char *buf, int len, int x, int y)
{
    int i = 0, k = 0;
    bool done = FALSE;
    char c;

    /* Paranoia -- check len */
    if (len < 1) len = 1;

    /* Paranoia -- check column */
    if ((x < 0) || (x >= 80)) x = 0;

    /* Restrict the length */
    if (x + len > 80) len = 80 - x;


    /* Paranoia -- Clip the default entry */
    buf[len] = '\0';


    /* Display the default answer */
    box(x*8, y*16, 639, y*16+15, COLOR_BLACK);
    put_string(x*8, y*16, buf, COLOR_YELLOW);


    /* Process input */
    while (!done) {
        // Refresh
        screen_refresh();

        /* Get a key */
        i = scan_inkey();
        c = convert(i, get_shift(), get_capslock());

        /* Analyze the key */
        switch (i) {
            case KEY_ESCAPE:
                k = 0;
                done = TRUE;
                break;

            case KEY_ENTER:
                k = strlen(buf);
                done = TRUE;
                break;

            case KEY_BACKSPACE:
                if (k > 0) k--;
                break;

            default:
                if ((k < len) && isprint(c)) {
                    buf[k++] = c;
                }
                else {
                    bell();
                }
                break;
        }

        /* Terminate */
        buf[k] = '\0';

        /* Update the entry */
        box(x*8, y*16, 639, y*16+15, COLOR_BLACK);
        put_string(x*8, y*16, buf, COLOR_WHITE);
    }

    /* Aborted */
    if (i == KEY_ESCAPE) return FALSE;

    /* Success */
    return TRUE;
}


/*
 * Get a string from the user
 *
 * The "prompt" should take the form "Prompt: "
 *
 * Note that the initial contents of the string is used as
 * the default response, so be sure to "clear" it if needed.
 *
 * We clear the input, and return FALSE, on "ESCAPE".
 */
bool get_string(char *prompt, char *buf, int len)
{
    bool res;

    /* Display prompt */
    box(0, 0, 639, 15, COLOR_BLACK);
    put_string(0, 0, (char *) prompt, COLOR_WHITE);
    screen_refresh();

    /* Ask the user for a string */
    res = askfor_aux(buf, len, strlen(prompt), 0);

    /* Clear prompt */
    box(0, 0, 639, 15, COLOR_BLACK);

    /* Result */
    return (res);
}


/*
 * Verify something with the user
 *
 * The "prompt" should take the form "Query? "
 *
 * Note that "[y/n]" is appended to the prompt.
 */
bool get_check(char *prompt)
{
    char c;
    int mx, my;

    CWindow window(180, 130, 280, 112, "Verification");
    CButton *yes = new CButton(230, 210, 60, 20, "Yes"); window.Attach(yes);
    CButton *no = new CButton(350, 210, 60, 20, "No"); window.Attach(no);
    window.Attach(new CFormatText(195, 155, 250, prompt, FONT_BOLD));
    
    for (;;) {
        gui_draw(&window);

        c = scan_inkey_scan();
        if ((c == KEY_Y) || (c == KEY_ENTER)) return TRUE;
        if ((c == KEY_N) || (c == KEY_ESCAPE)) return FALSE;

        if (get_last_left_button_release(&mx, &my)) {
            if (yes->inClientArea(mx, my)) return TRUE;
            if (no->inClientArea(mx, my)) return FALSE;
        }
    }
}

bool get_check_old(char *prompt)
{
    int i;
    char buf[80];

    /* Hack -- Build a "useful" prompt */
    strnfmt(buf, 78, "%.70s[y/n] ", prompt);

    /* Prompt for it */
    box(0, 0, 639, 15, COLOR_BLACK);
    put_string(0, 0, buf, COLOR_WHITE);
    screen_refresh();

    /* Get an acceptable answer */
    while (TRUE) {
        i = scan_inkey();
        if (i == KEY_ESCAPE) break;
        if (i == KEY_Y) break;
        if (i == KEY_N) break;
        bell();
    }

    /* Erase the prompt */
    box(0, 0, 639, 15, COLOR_BLACK);

    /* Normal negation */
    if (i != KEY_Y) return FALSE;

    /* Success */
    return TRUE;
}


/*
 * Prompts for a keypress
 *
 * The "prompt" should take the form "Command: "
 *
 * Returns TRUE unless the character is "Escape"
 */
bool get_com(char *prompt, char *command)
{
    /* Display a prompt */
    box(0, 0, 639, 15, COLOR_BLACK);
    put_text(0, 0, prompt, COLOR_WHITE, FONT_BOLD);
    screen_refresh();

    /* Get a key */
    *command = scan_inkey();
    *command = convert(*command, get_shift(), get_capslock());

    /* Clear the prompt */
    box(0, 0, 639, 15, COLOR_BLACK);

    /* Handle "cancel" */
    if (*command == ESCAPE) return FALSE;

    /* Success */
    return TRUE;
}


/*
 * Request a "quantity" from the user
 */
s16b get_quantity(char *prompt, int max)
{
    int amt;
    char tmp[80];
    char buf[80];


    /* Build a prompt if needed */
    if (!prompt) {
        /* Build a prompt */
        sprintf(tmp, "Quantity (1-%d): ", max);

        /* Use that prompt */
        prompt = tmp;
    }


    /* Default to one */
    amt = 1;

    /* Build the default */
    sprintf(buf, "%d", amt);

    /* Ask for a quantity */
    if (!get_string(prompt, buf, 6)) return (0);

    /* Extract a number */
    amt = atoi(buf);

    /* A letter means "all" */
    if (isalpha(buf[0])) amt = max;

    /* Enforce the maximum */
    if (amt > max) amt = max;

    /* Enforce the minimum */
    if (amt < 0) amt = 0;

    /* Return the result */
    return (amt);
}


/*
 * Check a char for "vowel-hood"
 */
bool is_a_vowel(int ch)
{
    switch (ch) {
        case 'a':
        case 'e':
        case 'i':
        case 'o':
        case 'u':
        case 'A':
        case 'E':
        case 'I':
        case 'O':
        case 'U':
            return TRUE;
    }

    return FALSE;
}


/*
 * Get a color from red to green to represent something at cur/max
 */
byte get_pct_color(int cur, int max)
{
    int pct;

    if (max == 0) return COLOR_LT_GREEN;
    
    pct = (cur*100)/max;

    if (pct < 30) return 228;
    else if (pct < 50) return 229;
    else if (pct < 70) return 230;
    else if (pct < 100) return 231;
    else return 232;
}


/*
 * Do a screen_refresh, but also draw and undraw the mouse
 * Note: in general, this function is only a temporary means to deal with more generic
 * problems!
 */
void mouse_refresh(void)
{
    int mx, my;
    bool left;

    get_mouse_status(&mx, &my, &left);
    virt_draw_mouse(mx, my);
    screen_refresh();
    virt_kill_mouse(mx, my);
}


/*** Utility Functions for Displaying Text ***/

/*
 * Draws text to the screen.  Can be right, left, or center-justified to
 * the x value given.
 */
void put_text_format(int x, int y, char *c, byte color, int font, int justify)
{
    int width = string_width(c, font);

    if (justify == JUST_CENTER) x -= width/2;
    else if (justify == JUST_RIGHT) x -= width;
    put_text(x, y, c, color, font);
}

// XXX other utilities for text display


/*** Keyboard Functions ***/

/*
 * Wait for a keypress
 */
void wait_for_key(void)
{
    scan_inkey();
}

/*
 * Wait for a keypress; return its scancode
 */
int scan_inkey(void)
{
    int c;
    for (;;) {
        c = scan_inkey_scan();
        if (c) return c;
    }
}
