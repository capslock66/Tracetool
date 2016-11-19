// ClipboardHistory.h : main header file for the ClipboardHistory DLL
//

#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "resource.h"		// main symbols


// CClipboardHistoryApp
// See ClipboardHistory.cpp for the implementation of this class
//

class CClipboardHistoryApp : public CWinApp
{
public:
	CClipboardHistoryApp();

// Overrides
public:
	virtual BOOL InitInstance();

	DECLARE_MESSAGE_MAP()
   virtual int ExitInstance();
};
