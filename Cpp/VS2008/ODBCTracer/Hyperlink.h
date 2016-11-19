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

#if !defined(AFX_HYPERLINK_H__7544C8BB_6031_422B_BA0E_820CCBD5DC25__INCLUDED_)
#define AFX_HYPERLINK_H__7544C8BB_6031_422B_BA0E_820CCBD5DC25__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CHyperlink  
{
public:
	CHyperlink();
	virtual ~CHyperlink();

	bool create(int resourceid, HWND parent, HINSTANCE ins);

protected:
	char url[256];
	HWND wnd;
	static bool register_class(HINSTANCE ins);
	static int WndProc(HWND hwnd,WORD wMsg,WPARAM wParam,LPARAM lParam);
};

#endif // !defined(AFX_HYPERLINK_H__7544C8BB_6031_422B_BA0E_820CCBD5DC25__INCLUDED_)
