/**
 * Copyright (C) 2002-2005
 * W3L GmbH
 * Technologiezentrum Ruhr
 * Universitätsstraße 142
 * D-44799 Bochum
 * 
 * Author: Dipl.Ing. Doga Arinir
 * E-Mail: doga.arinir@w3l.de
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the author or the company be held liable 
 * for any damages arising from the use of this software. EXPECT BUGS!
 * 
 * You may use this software in compiled form in any way you desire PROVIDING it is
 * not sold for profit without the authors written permission, and providing that this
 * notice and the authors name is included. If the source code in this file is used in 
 * any commercial application then acknowledgement must be made to the author of this file.
 */

#include "stdafx.h"
#include "stdafx.h"
#include "Hyperlink.h"
#include "shellapi.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CHyperlink::CHyperlink() : wnd(NULL)
{

}

CHyperlink::~CHyperlink()
{
	if (wnd)
		::DestroyWindow(wnd);
}


bool CHyperlink::create(int resourceid, HWND parent, HINSTANCE ins)
{
	CHyperlink::register_class(ins);

	HWND old = ::GetDlgItem(parent, resourceid);
	if (old != NULL)
	{
		RECT rect;

		::GetWindowText(old, url, sizeof(url));
		::GetWindowRect(old, &rect);

		//GetWindowRect return bounding box in screen coordinates.
		POINT pos;pos.x = rect.left;pos.y = rect.top;
		//calculate them down to client coordinates of the according dialog box...
		ScreenToClient(parent, &pos);

		wnd = ::CreateWindow( "STATIC_HYPER", url, WS_CHILD | WS_VISIBLE, 
										pos.x, pos.y, rect.right - rect.left, rect.bottom - rect.top, 
										parent, NULL, ins, NULL);

		::SetWindowLong( wnd, GWL_USERDATA, (LONG)this);
		//finally, destroy the old label
		::DestroyWindow(old);
	}
	return false;
}


bool CHyperlink::register_class(HINSTANCE ins)
{
	WNDCLASS hc;

	hc.style = 0;
	hc.lpfnWndProc = (WNDPROC)CHyperlink::WndProc;
	hc.cbClsExtra = 0;
	hc.cbWndExtra = sizeof(CHyperlink*);
	hc.hInstance = ins;
	hc.hIcon = NULL;
	hc.hCursor = NULL;
	hc.hbrBackground = NULL;
	hc.lpszMenuName = NULL;
	hc.lpszClassName = "STATIC_HYPER";
	return RegisterClass(&hc) != 0;
}

int CHyperlink::WndProc(HWND hwnd,WORD wMsg,WPARAM wParam,LPARAM lParam)
{
	CHyperlink *hl = (CHyperlink*)GetWindowLong(hwnd, GWL_USERDATA);

	switch (wMsg)  
	{
	case WM_LBUTTONDOWN:
		if (((UINT)::ShellExecute(NULL, _T("open"), hl->url, NULL, NULL, SW_SHOWNORMAL)) <= 32)
			MessageBeep(0);
		break;
	case WM_PAINT:
		{
			HDC hDC; PAINTSTRUCT ps;
			hDC = ::BeginPaint(hwnd, &ps);
			if (hl == NULL)
				return 0;

			RECT rect;
			::GetClientRect(hwnd, &rect);

			HFONT font = ::CreateFont( 13, //height
										5, //average char width
										0, //angle of escapement
										0, //base-line orientation angle
										FW_NORMAL,	//font weight
										FALSE,		//italic
										TRUE,		//underline
										FALSE,		//strikeout
										ANSI_CHARSET,			//charset identifier
										OUT_DEFAULT_PRECIS,		//ouput precision
										CLIP_DEFAULT_PRECIS,	//clipping precision
										DEFAULT_QUALITY,	//output quality
										DEFAULT_PITCH,			//pitch and family
										"Arial");
				
			::SelectObject(hDC, font);
			::SetTextColor(hDC, RGB(0,0,200));
			::SetBkMode(hDC, TRANSPARENT);
			::DrawText(hDC, hl->url, strlen(hl->url), &rect, DT_VCENTER | DT_CENTER);
			::DeleteObject(font);

			::EndPaint(hwnd, &ps);
						
			return TRUE;
		}
		case WM_SETCURSOR:
		{
			static HCURSOR cursor = NULL;
			if (cursor == NULL)
			{
				TCHAR szWindowsDir[MAX_PATH];
				GetWindowsDirectory(szWindowsDir ,MAX_PATH);
				strcat_s(szWindowsDir,MAX_PATH,"\\Winhlp32.exe");
				HMODULE hModule = LoadLibrary(szWindowsDir);		
				if (hModule)
					cursor = ::LoadCursor(hModule, MAKEINTRESOURCE(106));
			}
			::SetCursor(cursor);
			break;
		}
		default:
			DefWindowProc(hwnd, wMsg, wParam, lParam);
	}

	return TRUE;
}
