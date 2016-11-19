// VS8_PPC2003_MFC.h : main header file for the VS8_PPC2003_MFC application
//
#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "resourceppc.h"

// CVS8_PPC2003_MFCApp:
// See VS8_PPC2003_MFC.cpp for the implementation of this class
//

class CVS8_PPC2003_MFCApp : public CWinApp
{
public:
	CVS8_PPC2003_MFCApp();

// Overrides
public:
	virtual BOOL InitInstance();

// Implementation
public:
	afx_msg void OnAppAbout();

	DECLARE_MESSAGE_MAP()
};

extern CVS8_PPC2003_MFCApp theApp;
