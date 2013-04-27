// File: sys-win/dxwrap.cpp
// Purpose: Hide the DX interfaces behind my own class


/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "sys-win.h"

static void DXError(HRESULT hRes, char *param_str)
{
    char error_type[80];

    // Get a string for the error
    switch (hRes) {
        case DD_OK: strcpy(error_type, "DD_OK"); break; // heh, oops
        case DDERR_ALREADYINITIALIZED: strcpy(error_type, "DDERR_ALREADYINITIALIZED"); break;
        case DDERR_BLTFASTCANTCLIP: strcpy(error_type, "DDERR_BLTFASTCANTCLIP"); break;
        case DDERR_CANNOTATTACHSURFACE: strcpy(error_type, "DDERR_CANNOTATTACHSURFACE"); break;
        case DDERR_CANNOTDETACHSURFACE: strcpy(error_type, "DDERR_CANNOTDETACHSURFACE"); break;
        case DDERR_CANTCREATEDC: strcpy(error_type, "DDERR_CANTCREATEDC"); break;
        case DDERR_CANTDUPLICATE: strcpy(error_type, "DDERR_CANTDUPICATE"); break;
        case DDERR_CANTLOCKSURFACE: strcpy(error_type, "DDERR_CANTLOCKSURFACE"); break;
        case DDERR_CANTPAGELOCK: strcpy(error_type, "DDERR_CANTPAGELOCK"); break;
        case DDERR_CANTPAGEUNLOCK: strcpy(error_type, "DDERR_CANTPAGEUNLOCK"); break;
        case DDERR_CLIPPERISUSINGHWND: strcpy(error_type, "DDERR_CLIPPERISUSINGHWND"); break;
        case DDERR_COLORKEYNOTSET: strcpy(error_type, "DDERR_COLORKEYNOTSET"); break;
        case DDERR_CURRENTLYNOTAVAIL: strcpy(error_type, "DDERR_CURRENTLYNOTAVAIL"); break;
        case DDERR_DCALREADYCREATED: strcpy(error_type, "DDERR_DCALREADYCREATED"); break;
        case DDERR_DIRECTDRAWALREADYCREATED: strcpy(error_type, "DDERR_DIRECTDRAWALREADYCREATED"); break;
        case DDERR_EXCEPTION: strcpy(error_type, "DDERR_EXCEPTION"); break;
        case DDERR_EXCLUSIVEMODEALREADYSET: strcpy(error_type, "DDERR_EXCLUSIVEMODEALREADYSET"); break;
        case DDERR_GENERIC: strcpy(error_type, "DDERR_GENERIC"); break;
        case DDERR_HEIGHTALIGN: strcpy(error_type, "DDERR_HEIGHTALIGN"); break;
        case DDERR_HWNDALREADYSET: strcpy(error_type, "DDERR_HWNDALREADYSET"); break;
        case DDERR_HWNDSUBCLASSED: strcpy(error_type, "DDERR_HWNDSUBCLASSED"); break;
        case DDERR_IMPLICITLYCREATED: strcpy(error_type, "DDERR_IMPLICITLYCREATED"); break;
        case DDERR_INCOMPATIBLEPRIMARY: strcpy(error_type, "DDERR_INCOMPATIBLEPRIMARY"); break;
        case DDERR_INVALIDCAPS: strcpy(error_type, "DDERR_INVALIDCAPS"); break;
        case DDERR_INVALIDCLIPLIST: strcpy(error_type, "DDERR_INVALIDCLIPLIST"); break;
        case DDERR_INVALIDDIRECTDRAWGUID: strcpy(error_type, "DDERR_INVALIDDIRECTDRAWGUID"); break;
        case DDERR_INVALIDMODE: strcpy(error_type, "DDERR_INVALIDMODE"); break;
        case DDERR_INVALIDOBJECT: strcpy(error_type, "DDERR_INVALIDOBJECT"); break;
        case DDERR_INVALIDPARAMS: strcpy(error_type, "DDERR_INVALIDPARAMS"); break;
        case DDERR_INVALIDPIXELFORMAT: strcpy(error_type, "DDERR_INVALIDPIXELFORMAT"); break;
        case DDERR_INVALIDPOSITION: strcpy(error_type, "DDERR_INVALIDPOSITION"); break;
        case DDERR_INVALIDRECT: strcpy(error_type, "DDERR_INVALIDRECT"); break;
        case DDERR_INVALIDSURFACETYPE: strcpy(error_type, "DDERR_INVALIDSURFACETYPE"); break;
        case DDERR_LOCKEDSURFACES: strcpy(error_type, "DDERR_LOCKEDSURFACES"); break;
        case DDERR_NO3D: strcpy(error_type, "DDERR_NO3D"); break;
        case DDERR_NOALPHAHW: strcpy(error_type, "DDERR_NOALPHAHW"); break;
        case DDERR_NOBLTHW: strcpy(error_type, "DDERR_NOBLTHW"); break;
        case DDERR_NOCLIPLIST: strcpy(error_type, "DDERR_NOCLIPLIST"); break;
        case DDERR_NOCLIPPERATTACHED: strcpy(error_type, "DDERR_NOCLIPPERATTACHED"); break;
        case DDERR_NOCOLORCONVHW: strcpy(error_type, "DDERR_NOCOLORCONVHW"); break;
        case DDERR_NOCOLORKEY: strcpy(error_type, "DDERR_NOCOLORKEY"); break;
        case DDERR_NOCOLORKEYHW: strcpy(error_type, "DDERR_NOCOLORKEYHW"); break;
        case DDERR_NOCOOPERATIVELEVELSET: strcpy(error_type, "DDERR_NOCOOPERATIVELEVELSET"); break;
        case DDERR_NODC: strcpy(error_type, "DDERR_NODC"); break;
        case DDERR_NODDROPSHW: strcpy(error_type, "DDERR_NODDROPSHW"); break;
        case DDERR_NODIRECTDRAWHW: strcpy(error_type, "DDERR_NODIRECTDRAWHW"); break;
        case DDERR_NODIRECTDRAWSUPPORT: strcpy(error_type, "DDERR_NODIRECTDRAWSUPPORT"); break;
        case DDERR_NOEMULATION: strcpy(error_type, "DDERR_NOEMULATION"); break;
        case DDERR_NOEXCLUSIVEMODE: strcpy(error_type, "DDERR_NOEXCLUSIVEMODE"); break;
        case DDERR_NOFLIPHW: strcpy(error_type, "DDERR_NOFLIPHW"); break;
        case DDERR_NOGDI: strcpy(error_type, "DDERR_NOGDI"); break;
        case DDERR_NOHWND: strcpy(error_type, "DDERR_NOHWND"); break;
        case DDERR_NOMIPMAPHW: strcpy(error_type, "DDERR_NOMIPMAPHW"); break;
        case DDERR_NOMIRRORHW: strcpy(error_type, "DDERR_NOMIRRORHW"); break;
        case DDERR_NOOVERLAYDEST: strcpy(error_type, "DDERR_NOOVERLAYDEST"); break;
        case DDERR_NOOVERLAYHW: strcpy(error_type, "DDERR_NOOVERLAYHW"); break;
        case DDERR_NOPALETTEATTACHED: strcpy(error_type, "DDERR_NOPALETTEATTACHED"); break;
        case DDERR_NOPALETTEHW: strcpy(error_type, "DDERR_NOPALETTEHW"); break;
        case DDERR_NORASTEROPHW: strcpy(error_type, "DDERR_NORASTEROPHW"); break;
        case DDERR_NOROTATIONHW: strcpy(error_type, "DDERR_NOROTATIONHW"); break;
        case DDERR_NOSTRETCHHW: strcpy(error_type, "DDERR_NOSTRETCHHW"); break;
        case DDERR_NOT4BITCOLOR: strcpy(error_type, "DDERR_NOT4BITCOLOR"); break;
        case DDERR_NOT4BITCOLORINDEX: strcpy(error_type, "DDERR_NOT4BITCOLORINDEX"); break;
        case DDERR_NOT8BITCOLOR: strcpy(error_type, "DDERR_NOT8BITCOLOR"); break;
        case DDERR_NOTAOVERLAYSURFACE: strcpy(error_type, "DDERR_NOTAOVERLAYSURFACE"); break;
        case DDERR_NOTEXTUREHW: strcpy(error_type, "DDERR_NOTEXTUREHW"); break;
        case DDERR_NOTFLIPPABLE: strcpy(error_type, "DDERR_NOTFLIPPABLE"); break;
        case DDERR_NOTFOUND: strcpy(error_type, "DDERR_NOTFOUND"); break;
        case DDERR_NOTINITIALIZED: strcpy(error_type, "DDERR_NOTINITIALIZED"); break;
        case DDERR_NOTLOCKED: strcpy(error_type, "DDERR_NOTLOCKED"); break;
        case DDERR_NOTPAGELOCKED: strcpy(error_type, "DDERR_NOTPAGELOCKED"); break;
        case DDERR_NOTPALETTIZED: strcpy(error_type, "DDERR_NOTPALETTIZED"); break;
        case DDERR_NOVSYNCHW: strcpy(error_type, "DDERR_NOVSYNCHW"); break;
        case DDERR_NOZBUFFERHW: strcpy(error_type, "DDERR_NOZBUFFERHW"); break;
        case DDERR_NOZOVERLAYHW: strcpy(error_type, "DDERR_NOZOVERLAYHW"); break;
        case DDERR_OUTOFCAPS: strcpy(error_type, "DDERR_OUTOFCAPS"); break;
        case DDERR_OUTOFMEMORY: strcpy(error_type, "DDERR_OUTOFMEMORY"); break;
        case DDERR_OUTOFVIDEOMEMORY: strcpy(error_type, "DDERR_OUTOFVIDEOMEMORY"); break;
        case DDERR_OVERLAYCANTCLIP: strcpy(error_type, "DDERR_OVERLAYCANTCLIP"); break;
        case DDERR_OVERLAYCOLORKEYONLYONEACTIVE: strcpy(error_type, "DDERR_OVERLAYCOLORKEYONLYONEACTIVE"); break;
        case DDERR_OVERLAYNOTVISIBLE: strcpy(error_type, "DDERR_OVERLAYNOTVISIBLE"); break;
        case DDERR_PALETTEBUSY: strcpy(error_type, "DDERR_PALETTEBUSY"); break;
        case DDERR_PRIMARYSURFACEALREADYEXISTS: strcpy(error_type, "DDERR_PRIMARYSURFACEALREADYEXISTS"); break;
        case DDERR_REGIONTOOSMALL: strcpy(error_type, "DDERR_REGIONTOOSMALL"); break;
        case DDERR_SURFACEALREADYATTACHED: strcpy(error_type, "DDERR_SURFACEALREADYATTACHED"); break;
        case DDERR_SURFACEALREADYDEPENDENT: strcpy(error_type, "DDERR_SURFACEALREADYDEPENDENT"); break;
        case DDERR_SURFACEBUSY: strcpy(error_type, "DDERR_SURFACEBUSY"); break;
        case DDERR_SURFACEISOBSCURED: strcpy(error_type, "DDERR_SURFACEISOBSCURED"); break;
        case DDERR_SURFACELOST: strcpy(error_type, "DDERR_SURFACELOST"); break;
        case DDERR_SURFACENOTATTACHED: strcpy(error_type, "DDERR_SURFACENOTATTACHED"); break;
        case DDERR_TOOBIGHEIGHT: strcpy(error_type, "DDERR_TOOBIGHEIGHT"); break;
        case DDERR_TOOBIGSIZE: strcpy(error_type, "DDERR_TOOBIGSIZE"); break;
        case DDERR_TOOBIGWIDTH: strcpy(error_type, "DDERR_TOOBIGWIDTH"); break;
        case DDERR_UNSUPPORTED: strcpy(error_type, "DDERR_UNSUPPORTED"); break;
        case DDERR_UNSUPPORTEDFORMAT: strcpy(error_type, "DDERR_UNSUPPORTEDFORMAT"); break;
        case DDERR_UNSUPPORTEDMASK: strcpy(error_type, "DDERR_UNSUPPORTEDMASK"); break;
        case DDERR_UNSUPPORTEDMODE: strcpy(error_type, "DDERR_UNSUPPORTEDMODE"); break;
        case DDERR_VERTICALBLANKINPROGRESS: strcpy(error_type, "DDERR_VERTICALBLANKINPROGRESS"); break;
        case DDERR_WASSTILLDRAWING: strcpy(error_type, "DDERR_WASSTILLDRAWING"); break;
        case DDERR_WRONGMODE: strcpy(error_type, "DDERR_WRONGMODE"); break;
        case DDERR_XALIGN: strcpy(error_type, "DDERR_XALIGN"); break;
        default: strcpy(error_type, "unknown");
    }

    // Quit with error
    quit(format("%s: error code %s", param_str, error_type));
}

void CDirectX::RestoreSurfaces(void)
{
    HRESULT ddrval;

    // Primary surface
    if ((ddrval = m_lpDDSPrimary->IsLost()) != DD_OK) {
        if (ddrval == DDERR_SURFACELOST) {
            ddrval = m_lpDDSPrimary->Restore();
            if (ddrval != DD_OK) DXError(ddrval, "Could not restore primary surface");
        }
        else {
            DXError(ddrval, "Invalid IsLost check on primary surface");
        }
    }
}

CDirectX::CDirectX(HWND hWnd)
{
    HRESULT ddrval;
    DDSURFACEDESC ddsd;
    DDSCAPS ddscaps;
    PALETTEENTRY pe[256];

    // Set the hWnd internally
    m_hWnd = hWnd;

    // Set all the interface pointers to NULL
    m_lpDD = NULL;
    m_lpDDSPrimary = NULL;
    m_lpDDSBack = NULL;
    m_lpDDPalette = NULL;

    // Fill the palette entries with zeros
    for (int i = 0; i < 256; i++) {
        pe[i].peRed = 0;
        pe[i].peGreen = 0;
        pe[i].peBlue = 0;
        pe[i].peFlags = NULL;
    }

    // Create the DirectDraw object
    ddrval = DirectDrawCreate(NULL, &m_lpDD, NULL);
    if (ddrval != DD_OK) DXError(ddrval, "Could not create DirectDraw object");

    // Set exclusive mode
    ddrval = m_lpDD->SetCooperativeLevel(m_hWnd, DDSCL_ALLOWREBOOT | DDSCL_EXCLUSIVE |
        DDSCL_FULLSCREEN);
    if (ddrval != DD_OK) DXError(ddrval, "Could not set cooperative level");

    // Set video mode
    ddrval = m_lpDD->SetDisplayMode(640, 480, 8);
    if (ddrval != DD_OK) DXError(ddrval, "Could not set display mode");

    // Create the primary surface with a back buffer
    ZeroMemory(&ddsd, sizeof(ddsd));
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_CAPS | DDSD_BACKBUFFERCOUNT;
    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE | DDSCAPS_FLIP | DDSCAPS_COMPLEX;
    ddsd.dwBackBufferCount = 1;
    ddrval = m_lpDD->CreateSurface(&ddsd, &m_lpDDSPrimary, NULL);
    if (ddrval != DD_OK) DXError(ddrval, "Could not create primary surface");

    // Acquire the back buffer
    ddscaps.dwCaps = DDSCAPS_BACKBUFFER;
    ddrval = m_lpDDSPrimary->GetAttachedSurface(&ddscaps, &m_lpDDSBack);
    if (ddrval != DD_OK) DXError(ddrval, "Could not acquire back buffer");

    // Create a palette object
    ddrval = m_lpDD->CreatePalette(DDPCAPS_8BIT | DDPCAPS_ALLOW256, pe,
        &m_lpDDPalette, NULL);
    if (ddrval != DD_OK) DXError(ddrval, "Could not create palette object");

    // Attach the palette to the primary surface
    ddrval = m_lpDDSPrimary->SetPalette(m_lpDDPalette);
    if (ddrval != DD_OK) DXError(ddrval, "Could not attach palette object");
}

CDirectX::~CDirectX()
{
    if (m_lpDD) {
        m_lpDD->RestoreDisplayMode();
        m_lpDD->SetCooperativeLevel(m_hWnd, DDSCL_NORMAL);
        if (m_lpDDSPrimary) {
            m_lpDDSPrimary->Release();
            m_lpDDSPrimary = NULL;
        }
        if (m_lpDDPalette) {
            m_lpDDPalette->Release();
            m_lpDDPalette = NULL;
        }
        m_lpDD->Release();
        m_lpDD = NULL;
    }
}

void CDirectX::FillRect(int x1, int y1, int x2, int y2, byte color)
{
    RECT rc;
    HRESULT ddrval;
    DDBLTFX ddbltfx;

    // Set up ddbltfx
    ZeroMemory(&ddbltfx, sizeof(ddbltfx));
    ddbltfx.dwSize = sizeof(ddbltfx);
    ddbltfx.dwFillColor = color;

    // Set up rc
    rc.left = x1; rc.top = y1;
    rc.right = x2+1; rc.bottom = y2+1;

    // Repeat until it works
    for (;;) {
        // Try the blit
        ddrval = m_lpDDSBack->Blt(&rc, NULL, NULL, DDBLT_COLORFILL, &ddbltfx);

        // If OK, get out
        if (ddrval == DD_OK) break;

        // Correctable errors
        if (ddrval == DDERR_SURFACELOST) {
            RestoreSurfaces();
            continue;
        }
        if (ddrval == DDERR_WASSTILLDRAWING) continue;

        // Bad error
        DXError(ddrval, "FillRect failed");
    }
}

void CDirectX::ScreenRefresh(void)
{
    HRESULT ddrval;
    DDBLTFX ddbltfx;

    // Set up ddbltfx
    ZeroMemory(&ddbltfx, sizeof(ddbltfx));
    ddbltfx.dwSize = sizeof(ddbltfx);

    // Repeat until it works
    for (;;) {
        // Try the blit
        ddrval = m_lpDDSPrimary->Blt(NULL, m_lpDDSBack, NULL, 0, &ddbltfx);

        // If OK, get out
        if (ddrval == DD_OK) break;

        // Correctable errors
        if (ddrval == DDERR_SURFACELOST) {
            RestoreSurfaces();
            continue;
        }
        if (ddrval == DDERR_WASSTILLDRAWING) continue;

        // Bad error
        DXError(ddrval, "ScreenRefresh failed");
    }
}


void CDirectX::Lock(byte **vs, LONG *pitch)
{
    HRESULT ddrval;
    DDSURFACEDESC ddsd;

    // Set up ddsd
    ZeroMemory(&ddsd, sizeof(ddsd));
    ddsd.dwSize = sizeof(ddsd);

    // Repeat until it works
    for (;;) {
        // Try the lock
        ddrval = m_lpDDSBack->Lock(NULL, &ddsd, DDLOCK_SURFACEMEMORYPTR, NULL);

        // If OK, get out
        if (ddrval == DD_OK) break;

        // Correctable errors
        if (ddrval == DDERR_SURFACELOST) {
            RestoreSurfaces();
            continue;
        }
        if (ddrval == DDERR_WASSTILLDRAWING) continue;

        // Bad error
        DXError(ddrval, "Lock failed");
    }

    // Copy the data from the lock
    *vs = (byte *)ddsd.lpSurface;
    *pitch = ddsd.lPitch;
}


void CDirectX::Unlock(void)
{
    HRESULT ddrval;

    // Repeat until it works
    for (;;) {
        // Try the unlock
        ddrval = m_lpDDSBack->Unlock(NULL);

        // If OK or not locked, get out
        if (ddrval == DD_OK) break;
        if (ddrval == DDERR_NOTLOCKED) break;

        // Correctable errors
        if (ddrval == DDERR_SURFACELOST) {
            RestoreSurfaces();
            continue;
        }

        // Bad error
        DXError(ddrval, "Unlock failed");
    }
}


void CDirectX::SetPaletteEntry(int c, int r, int g, int b)
{
    HRESULT ddrval;
    PALETTEENTRY pe;

    // Set up pe
    pe.peRed = r;
    pe.peGreen = g;
    pe.peBlue = b;
    pe.peFlags = NULL;

    // Set the palette
    ddrval = m_lpDDPalette->SetEntries(0, c, 1, &pe);

    // If OK, return
    if (ddrval == DD_OK) return;

    // Error
    DXError(ddrval, "SetPaletteEntry failed");
}