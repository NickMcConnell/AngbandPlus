// File: gui.cpp
// Purpose: functions for the Utumno GUI

/*
 * Copyright (c) 1997 Matt Craighead
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"

void draw_window_border(int x1, int y1, int x2, int y2, bool depressed)
{
    if (!depressed) {
        horiz_line(x1, x2, y1, COLOR_LT_GREY);
        vert_line(x1, y1, y2, COLOR_LT_GREY);
        vert_line(x2, y1, y2, COLOR_BLACK);
        horiz_line(x1, x2, y2, COLOR_BLACK);
        horiz_line(x1+1, x2-1, y1+1, COLOR_WHITE);
        vert_line(x1+1, y1+1, y2-1, COLOR_WHITE);
        vert_line(x2-1, y1+1, y2-1, 7);
        horiz_line(x1+1, x2-1, y2-1, 7);
    }
    else {
        rectangle(x1, y1, x2, y2, COLOR_BLACK);
        rectangle(x1+1, y1+1, x2-1, y2-1, 7);
    }
}

void draw_pane(int x1, int y1, int x2, int y2, byte color)
{
    box(x1+2, y1+2, x2-2, y2-2, color);
    horiz_line(x1, x2, y1, 7);
    vert_line(x1, y1, y2, 7);
    vert_line(x2, y1, y2, COLOR_WHITE);
    horiz_line(x1, x2, y2, COLOR_WHITE);
    horiz_line(x1+1, x2-1, y1+1, COLOR_BLACK);
    vert_line(x1+1, y1+1, y2-1, COLOR_BLACK);
    vert_line(x2-1, y1+1, y2-1, COLOR_LT_GREY);
    horiz_line(x1+1, x2-1, y2-1, COLOR_LT_GREY);
}

void draw_window(int x1, int y1, int x2, int y2, char *title)
{
    box(x1+2, y1+2, x2-2, y1+19, 200);
    box(x1+2, y1+20, x2-2, y2-2, COLOR_GREY);
    draw_window_border(x1, y1, x2, y2, FALSE);
    put_text_format((x1+x2)/2, y1+4, title, COLOR_WHITE, FONT_BOLD, JUST_CENTER);
}


CComponent::CComponent(int xx, int yy, int wid, int hgt)
{
    x = xx; y = yy; width = wid; height = hgt;
    child = sibling = NULL;
    enabled = TRUE;
}

CComponent::~CComponent()
{
    if (child) delete child;
    if (sibling) delete sibling;
}

void CComponent::Attach(CComponent *c)
{
    if (!child) {
        child = c;
    }
    else {
        c->sibling = child->sibling;
        child->sibling = c;
    }
}

void CComponent::Detach(CComponent *c)
{
    if (child) {
        if (child == c) {
            child = c->sibling;
            delete c;
            return;
        }
        child->Detach(c);
    }
    if (sibling) {
        if (sibling == c) {
            sibling = c->sibling;
            delete c;
            return;
        }
        sibling->Detach(c);
    }
}

bool CComponent::inClientArea(int xx, int yy)
{
    return ((xx >= x) && (xx <= x+width) && (yy >= y) && (yy <= y+height));
}

void CComponent::Draw(int mx, int my, bool left)
{
    DrawMe(mx, my, left);
    if (child) child->Draw(mx, my, left);
    if (sibling) sibling->Draw(mx, my, left);
}

void CComponent::Click(int mx, int my)
{
    if (inClientArea(mx, my)) WasClicked();
    if (child) child->Click(mx, my);
    if (sibling) sibling->Click(mx, my);
}

void CComponent::DrawMe(int mx, int my, bool left) {}


CText::CText(int xx, int yy, char *str, int f, int a) : CComponent(xx, yy, 0, 0)
{
    text = new char[strlen(str)+1];
    strcpy(text, str);
    font = f; align = a;
}

void CText::DrawMe(int mx, int my, bool left)
{
    put_text_format(x, y, text, enabled ? COLOR_BLACK : 8, font, align);
}


CFormatText::CFormatText(int xx, int yy, int wid, char *str, int f) :
    CComponent(xx, yy, wid, 0)
{
    text = new char[strlen(str)+1];
    strcpy(text, str);
    font = f;
}

void CFormatText::DrawMe(int mx, int my, bool left)
{
    format_text(x, x+width, y, text, enabled ? COLOR_BLACK : 8, font);
}


CButton::CButton(int xx, int yy, int wid, int hgt, char *str) : CComponent(xx, yy, wid, hgt)
{
    text = new char[strlen(str)+1];
    strcpy(text, str);
}

void CButton::DrawMe(int mx, int my, bool left)
{
    bool depressed = isDepressed(mx, my, left);
    box(x+2, y+2, x+width-2, y+height-2, COLOR_GREY);
    draw_window_border(x, y, x+width, y+height, depressed); 
    put_text_format(x+width/2+depressed, y+3+depressed, text, COLOR_BLACK, FONT_BOLD,
        JUST_CENTER);
}


CWindow::CWindow(int xx, int yy, int wid, int hgt, char *str) : CComponent(xx, yy, wid, hgt)
{
    title = new char[strlen(str)+1];
    strcpy(title, str);
}

void CWindow::DrawMe(int mx, int my, bool left)
{
    draw_window(x, y, x+width, y+height, title);
}


CCheckBox::CCheckBox(int xx, int yy, bool *dptr) : CComponent(xx, yy, 13, 13)
{
    data = dptr;
}

static byte checkmark[10][10] = {
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0, 0, 0, 0, 1, 0},
    {0, 0, 0, 0, 0, 0, 0, 1, 1, 0},
    {0, 0, 0, 0, 0, 0, 1, 1, 1, 0},
    {0, 1, 0, 0, 0, 1, 1, 1, 0, 0},
    {0, 1, 1, 0, 1, 1, 1, 0, 0, 0},
    {0, 1, 1, 1, 1, 1, 0, 0, 0, 0},
    {0, 0, 1, 1, 1, 0, 0, 0, 0, 0},
    {0, 0, 0, 1, 0, 0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
};

void CCheckBox::DrawMe(int mx, int my, bool left)
{
    bool checked, depressed;
    checked = isChecked();
    depressed = isDepressed(mx, my, left);
    box(x+2, y+2, x+width-2, y+height-2, depressed ? COLOR_GREY : COLOR_WHITE);
    horiz_line(x, x+width, y, 7);
    vert_line(x, y, y+height, 7);
    vert_line(x+width, y, y+height, COLOR_WHITE);
    horiz_line(x, x+width, y+height, COLOR_WHITE);
    horiz_line(x+1, x+width-1, y+1, COLOR_BLACK);
    vert_line(x+1, y+1, y+height-1, COLOR_BLACK);
    vert_line(x+width-1, y+1, y+height-1, COLOR_LT_GREY);
    horiz_line(x+1, x+width-1, y+height-1, COLOR_LT_GREY);
    if (checked) {
        start_pixel_draw();
        for (int yy = 0; yy < 10; yy++) {
            for (int xx = 0; xx < 10; xx++) {
                if (checkmark[yy][xx]) {
                    draw_pixel(xx+x+2, yy+y+2, COLOR_BLACK);
                }
            }
        }
        end_pixel_draw();
    }
}


CRadioButton::CRadioButton(int xx, int yy, int *dptr, int v) : CComponent(xx, yy, 13, 13)
{
    data = dptr;
    value = v;
}

static byte radio[12][12] = {
    {0, 0, 0, 0, 6, 6, 6, 6, 0, 0, 0, 0},
    {0, 0, 6, 6, 5, 5, 5, 5, 6, 6, 0, 0},
    {0, 6, 5, 5, 2, 2, 2, 2, 5, 5, 4, 0},
    {0, 6, 5, 2, 2, 2, 2, 2, 2, 3, 4, 0},
    {6, 5, 2, 2, 2, 1, 1, 2, 2, 2, 3, 4},
    {6, 5, 2, 2, 1, 1, 1, 1, 2, 2, 3, 4},
    {6, 5, 2, 2, 1, 1, 1, 1, 2, 2, 3, 4},
    {6, 5, 2, 2, 2, 1, 1, 2, 2, 2, 3, 4},
    {0, 6, 5, 2, 2, 2, 2, 2, 2, 3, 4, 0},
    {0, 6, 3, 3, 2, 2, 2, 2, 3, 3, 4, 0},
    {0, 0, 4, 4, 3, 3, 3, 3, 4, 4, 0, 0},
    {0, 0, 0, 0, 4, 4, 4, 4, 0, 0, 0, 0},
};

void CRadioButton::DrawMe(int mx, int my, bool left)
{
    bool selected, depressed;
    int px, py;

    selected = isSelected();
    depressed = isDepressed(mx, my, left);
    start_pixel_draw();
    for (int yy = 0; yy < 12; yy++) {
        for (int xx = 0; xx < 12; xx++) {
            px = xx+x+1;
            py = yy+y+1;
            switch (radio[yy][xx]) {
                case 1:
                    if (selected) draw_pixel(px, py, COLOR_BLACK);
                    else if (depressed || !enabled) draw_pixel(px, py, COLOR_GREY);
                    else draw_pixel(px, py, COLOR_WHITE);
                    break;
                case 2:
                    if (depressed || !enabled) draw_pixel(px, py, COLOR_GREY);
                    else draw_pixel(px, py, COLOR_WHITE);
                    break;
                case 3:
                    draw_pixel(px, py, COLOR_LT_GREY);
                    break;
                case 4:
                    draw_pixel(px, py, COLOR_WHITE);
                    break;
                case 5:
                    draw_pixel(px, py, 11);
                    break;
                case 6:
                    draw_pixel(px, py, 7);
                    break;
            }
        }
    }
    end_pixel_draw();
}


CTextField::CTextField(int xx, int yy, int wid, char *dptr, int l) :
CComponent(xx, yy, wid, 18)
{
    data = dptr;
    len = l-1;
}

void CTextField::DrawMe(int mx, int my, bool left)
{
    int xx;
    box(x+2, y+2, x+width-2, y+height-2, COLOR_WHITE);
    horiz_line(x, x+width, y, 7);
    vert_line(x, y, y+height, 7);
    vert_line(x+width, y, y+height, COLOR_WHITE);
    horiz_line(x, x+width, y+height, COLOR_WHITE);
    horiz_line(x+1, x+width-1, y+1, COLOR_BLACK);
    vert_line(x+1, y+1, y+height-1, COLOR_BLACK);
    vert_line(x+width-1, y+1, y+height-1, COLOR_LT_GREY);
    horiz_line(x+1, x+width-1, y+height-1, COLOR_LT_GREY);

    set_clip_rect(x+2, y+2, x+width-2, y+height-2);
    xx = x+3+string_width(data, FONT_BOLD);
    if (xx > x+width-2) xx = x+width-2;
    put_text_format(xx, y+2, data, COLOR_BLACK, FONT_BOLD, JUST_RIGHT);
    vert_line(xx, y+3, y+14, COLOR_BLACK);
    set_clip_rect(0, 0, 639, 479);
    //: Revert to old cliprect
}

void CTextField::ProcessChar(char c)
{
    int slen = strlen(data);
    char ascii = convert(c, get_shift(), get_capslock());

    if (isprint(ascii)) {
        if (slen == len) bell();
        else {
            data[slen] = ascii;
            data[slen+1] = 0;
        }
    }
    else {
        switch (c) {
            case KEY_BACKSPACE:
                if (slen > 0) data[slen-1] = 0;
                else bell();
                break;
        }
    }
}


void gui_draw(CComponent *base)
{
    int mx, my;
    bool left;

    get_mouse_status(&mx, &my, &left);
    base->Draw(mx, my, left);
    virt_draw_mouse(mx, my);
    screen_refresh();
    virt_kill_mouse(mx, my);
}

void mini_message_box(char *title, char *msg)
{
    int mx, my, width;
    char c;

    width = string_width(msg, FONT_BOLD)/2 + 20;
    if (width < 80) width = 80;

    CWindow *window = new CWindow(320-width, 146, width*2, 81, title);
    window->Attach(new CText(320, 176, msg, FONT_BOLD, JUST_CENTER));
    CButton *button = new CButton(290, 200, 60, 20, "OK");
    window->Attach(button);

    for (;;) {
        // Draw
        gui_draw(window);

        // Keyboard events
        c = scan_inkey_scan();
        if ((c == KEY_ENTER) || (c == KEY_ESCAPE)) break;

        // Mouse events
        if (get_last_left_button_release(&mx, &my)) {
            window->Click(mx, my);
            if (button->inClientArea(mx, my)) break;
        }
    }
    delete window;
}
