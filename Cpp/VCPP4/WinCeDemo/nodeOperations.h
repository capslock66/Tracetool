#if !defined(AFX_NODEOPERATIONS_H__91DDBC45_51EA_4584_8E50_55410C35B272__INCLUDED_)
#define AFX_NODEOPERATIONS_H__91DDBC45_51EA_4584_8E50_55410C35B272__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// NodeOperations.h : header file
//

#include "tracetool.h"

/////////////////////////////////////////////////////////////////////////////
// nodeOperations dialog

class NodeOperations : public CPropertyPage
{
	DECLARE_DYNCREATE(NodeOperations)

// Construction
public:
	NodeOperations();
	~NodeOperations();

        TraceNodeEx * start1 ;
        TraceNodeEx * start2 ;

// Dialog Data
	//{{AFX_DATA(NodeOperations)
	enum { IDD = IDD_NODEEX };
		// NOTE - ClassWizard will add data members here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(NodeOperations)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(NodeOperations)
	afx_msg void OnAppend();
	afx_msg void OnResend();
	afx_msg void OnSetselected();
	afx_msg void OnShownode();
	afx_msg void OnStart1();
	afx_msg void OnStart2();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NODEOPERATIONS_H__91DDBC45_51EA_4584_8E50_55410C35B272__INCLUDED_)
