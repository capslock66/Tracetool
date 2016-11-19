#if !defined(AFX_MULTIPAGES_H__F0C7E269_A283_4054_A08C_DCBE235A64C0__INCLUDED_)
#define AFX_MULTIPAGES_H__F0C7E269_A283_4054_A08C_DCBE235A64C0__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// MultiPages.h : header file
//

#include "tracetool.h"


/////////////////////////////////////////////////////////////////////////////
// MultiPages dialog

class MultiPages : public CPropertyPage
{
	DECLARE_DYNCREATE(MultiPages)

// Construction
public:
	MultiPages();
	~MultiPages();

   WinTrace * MyWinTrace ;
   WinTrace * MulticolWintrace ;

// Dialog Data
	//{{AFX_DATA(MultiPages)
	enum { IDD = IDD_MULTIPAGES };
		// NOTE - ClassWizard will add data members here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(MultiPages)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(MultiPages)
	afx_msg void OnDisplaywin();
	afx_msg void OnLoadxml();
	afx_msg void OnNewwin();
	afx_msg void OnSavetext();
	afx_msg void OnSavetoxml();
	afx_msg void OnSayhello();
	afx_msg void OnMulticol();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MULTIPAGES_H__F0C7E269_A283_4054_A08C_DCBE235A64C0__INCLUDED_)
