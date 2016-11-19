// WinCeDemoDlg.h : header file
//

#if !defined(AFX_WINCEDEMODLG_H__07BA22C6_122E_4B7F_B5A6_9D66D760DE96__INCLUDED_)
#define AFX_WINCEDEMODLG_H__07BA22C6_122E_4B7F_B5A6_9D66D760DE96__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

/////////////////////////////////////////////////////////////////////////////
// CWinCeDemoDlg dialog

class CWinCeDemoDlg : public CDialog
{
// Construction
public:
	CWinCeDemoDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CWinCeDemoDlg)
	enum { IDD = IDD_WINCEDEMO_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CWinCeDemoDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CWinCeDemoDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft eMbedded Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_WINCEDEMODLG_H__07BA22C6_122E_4B7F_B5A6_9D66D760DE96__INCLUDED_)
