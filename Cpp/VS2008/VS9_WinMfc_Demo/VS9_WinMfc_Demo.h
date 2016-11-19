// VS9_Mfc_Demo.h : main header file for the VS9_Mfc_Demo application
//
#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "resource.h"       // main symbols


// CVS9_Mfc_DemoApp:
// See VS9_Mfc_Demo.cpp for the implementation of this class
//

class CVS9_Mfc_DemoApp : public CWinApp
{
public:
	CVS9_Mfc_DemoApp();


// Overrides
public:
	virtual BOOL InitInstance();

// Implementation

public:
	afx_msg void OnAppAbout();
	DECLARE_MESSAGE_MAP()
};

extern CVS9_Mfc_DemoApp theApp;