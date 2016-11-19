#if !defined(AFX_SHEET_H__0FA50E66_D76F_418F_98D1_DED43B93FA05__INCLUDED_)
#define AFX_SHEET_H__0FA50E66_D76F_418F_98D1_DED43B93FA05__INCLUDED_

#include "Basic.h"
#include "NodeOperations.h"
#include "MultiPages.h"
#include "Watches.h"


#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Sheet.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// Sheet

class Sheet : public CPropertySheet
{
	DECLARE_DYNAMIC(Sheet)

// Construction
public:
	Sheet(UINT nIDCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);
	Sheet(LPCTSTR pszCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);

// Attributes
public:
    Basic basic ;
    NodeOperations nodeOperations ;
    MultiPages multiPages ;
    Watches watches ;
    CMenu menu ;

    CCeCommandBar * m_pCommandBar ;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(Sheet)
	public:
	virtual BOOL OnInitDialog();
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~Sheet();

	// Generated message map functions
protected:
	//{{AFX_MSG(Sheet)
	afx_msg void OnFileExit();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SHEET_H__0FA50E66_D76F_418F_98D1_DED43B93FA05__INCLUDED_)
