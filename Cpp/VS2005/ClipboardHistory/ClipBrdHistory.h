// ClipBrdHistory.h : main header file for the ClipBrdHistory DLL
//

#pragma once

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols


// CClipBrdHistoryApp
// See ClipBrdHistory.cpp for the implementation of this class
//

class CClipBrdHistoryApp : public CWinApp
{
public:
	CClipBrdHistoryApp();

// Overrides
public:
	virtual BOOL InitInstance();

	DECLARE_MESSAGE_MAP()
   virtual int ExitInstance();
};
