// File: gui.h
// Purpose: declarations for the Utumno GUI

/*
 * Copyright (c) 1997 Matt Craighead
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

class CComponent {
private:
    CComponent *child, *sibling;
    
    virtual void DrawMe(int mx, int my, bool left);
    virtual void WasClicked() {}

protected:
    int x, y, width, height;
    bool enabled;

public:
    CComponent(int xx, int yy, int wid, int hgt);
    virtual ~CComponent();
    void Attach(CComponent *c);
    void Detach(CComponent *c);
    void Enable() { enabled = TRUE; }
    void Disable() { enabled = FALSE; }
    bool isEnabled() { return enabled; }

    bool inClientArea(int xx, int yy);
    bool isDepressed(int mx, int my, bool left) { return (left && inClientArea(mx, my)); }

    void Click(int mx, int my);
    void Draw(int mx, int my, bool left);
};

class CText: public CComponent {
private:
    char *text;
    int align, font;

    void DrawMe(int mx, int my, bool left);

public:
    CText(int xx, int yy, char *str, int f, int a);
    virtual ~CText() { delete[] text; }
};

class CFormatText: public CComponent {
private:
    char *text;
    int font;

    void DrawMe(int mx, int my, bool left);

public:
    CFormatText(int xx, int yy, int wid, char *str, int f);
    virtual ~CFormatText() { delete[] text; }
};

class CButton: public CComponent {
private:
    char *text;

    void DrawMe(int mx, int my, bool left);

public:
    CButton(int xx, int yy, int wid, int hgt, char *str);
    virtual ~CButton() { delete[] text; }
};

class CWindow: public CComponent {
private:
    char *title;

    void DrawMe(int mx, int my, bool left);

public:
    CWindow(int xx, int yy, int wid, int hgt, char *str);
    virtual ~CWindow() { delete[] title; }
};

class CCheckBox: public CComponent {
private:
    bool *data;

    void DrawMe(int mx, int my, bool left);
    void WasClicked() { if (enabled) *data = !*data; }

public:
    CCheckBox(int xx, int yy, bool *dptr);
    bool isChecked() { return *data; }
};

class CRadioButton: public CComponent {
private:
    int *data;
    int value;

    void DrawMe(int mx, int my, bool left);
    void WasClicked() { if (enabled) *data = value; }

public:
    CRadioButton(int xx, int yy, int *dptr, int v);
    bool isSelected() { return (*data == value); }
};

class CTextField: public CComponent {
private:
    char *data;
    int len;

    void DrawMe(int mx, int my, bool left);

public:
    CTextField(int xx, int yy, int wid, char *dptr, int l);
    void ProcessChar(char c);
};

