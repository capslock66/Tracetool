#if !defined(AFX_BASIC_H__0C918302_9380_4E3C_ACD7_8ADFCBF08238__INCLUDED_)
#define AFX_BASIC_H__0C918302_9380_4E3C_ACD7_8ADFCBF08238__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Basic.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// Basic dialog

class Basic : public CPropertyPage
{
	DECLARE_DYNCREATE(Basic)

// Construction
public:
	Basic();
	~Basic();

// Dialog Data
	//{{AFX_DATA(Basic)
	enum { IDD = IDD_BASIC };
		// NOTE - ClassWizard will add data members here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(Basic)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(Basic)
	afx_msg void OnSample1();
	afx_msg void OnMainloadxml();
	afx_msg void OnIndent();
	afx_msg void OnMainclear();
	afx_msg void OnMainsavetext();
	afx_msg void OnMainsavetoxml();
	afx_msg void OnShowviewer();
	afx_msg void OnTest();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_BASIC_H__0C918302_9380_4E3C_ACD7_8ADFCBF08238__INCLUDED_)
