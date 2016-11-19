#if !defined(AFX_WATCHES_H__CEB7E386_ED35_4E53_A874_D77D87851FFA__INCLUDED_)
#define AFX_WATCHES_H__CEB7E386_ED35_4E53_A874_D77D87851FFA__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Watches.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// Watches dialog

class Watches : public CPropertyPage
{
	DECLARE_DYNCREATE(Watches)

// Construction
public:
	Watches();
	~Watches();

// Dialog Data
	//{{AFX_DATA(Watches)
	enum { IDD = IDD_WATCH };
		// NOTE - ClassWizard will add data members here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(Watches)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(Watches)
	afx_msg void OnClear();
	afx_msg void OnDisp();
	afx_msg void OnSend();
	afx_msg void OnWinwatchClear();
	afx_msg void OnWinwatchDisplay();
	afx_msg void OnWinwatchSend();
	afx_msg void OnWinwatchCreate();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_WATCHES_H__CEB7E386_ED35_4E53_A874_D77D87851FFA__INCLUDED_)
