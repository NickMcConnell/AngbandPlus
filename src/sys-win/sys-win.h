// File: sys-win/sys-win.h
// Purpose: Prototypes and definitions for sys-win/*

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <ddraw.h>
#include "../utumno.h"

class CDirectX {
private:
    LPDIRECTDRAW m_lpDD;
    LPDIRECTDRAWSURFACE m_lpDDSPrimary;
    LPDIRECTDRAWSURFACE m_lpDDSBack;
    LPDIRECTDRAWPALETTE m_lpDDPalette;
    HWND m_hWnd;

    void RestoreSurfaces(void);

public:
    CDirectX(HWND hWnd);
    ~CDirectX();

    void FillRect(int x1, int y1, int x2, int y2, byte color);
    void ScreenRefresh(void);
    void Lock(byte **vs, LONG *pitch);
    void Unlock(void);
    void SetPaletteEntry(int c, int r, int g, int b);
};