// CppWin32Plugin.h : main header file for the SamplePlugin DLL
//

#pragma once

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols


// CSamplePluginApp
// See CppWin32Plugin.cpp for the implementation of this class
//

class CSamplePluginApp : public CWinApp
{
public:
	CSamplePluginApp();

// Overrides
public:
	virtual BOOL InitInstance();

	DECLARE_MESSAGE_MAP()
   virtual int ExitInstance();
};
