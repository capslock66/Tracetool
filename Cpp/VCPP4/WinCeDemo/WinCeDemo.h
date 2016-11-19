// WinCeDemo.h : main header file for the WINCEDEMO application
//

#if !defined(AFX_WINCEDEMO_H__F9C601A7_C2B7_4105_80A2_D954B4C529F6__INCLUDED_)
#define AFX_WINCEDEMO_H__F9C601A7_C2B7_4105_80A2_D954B4C529F6__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CWinCeDemoApp:
// See WinCeDemo.cpp for the implementation of this class
//

class CWinCeDemoApp : public CWinApp
{
public:
	CWinCeDemoApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CWinCeDemoApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CWinCeDemoApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft eMbedded Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_WINCEDEMO_H__F9C601A7_C2B7_4105_80A2_D954B4C529F6__INCLUDED_)
