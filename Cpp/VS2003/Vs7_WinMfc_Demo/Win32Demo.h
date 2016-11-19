// Win32Demo.h : main header file for the PROJECT_NAME application
//

#pragma once

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols


// CWin32DemoApp:
// See Win32Demo.cpp for the implementation of this class
//

class CWin32DemoApp : public CWinApp
{
public:
	CWin32DemoApp();

// Overrides
	public:
	virtual BOOL InitInstance();

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern CWin32DemoApp theApp;