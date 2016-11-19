// AllControlsSheet.cpp : implementation file
//

// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (c) Microsoft Corporation.  All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#include "stdafx.h"
#include "resource.h"		// main symbols
#include "propsht.h"
#include "propsht.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif



/////////////////////////////////////////////////////////////////////////////
// CAllControlsSheet

IMPLEMENT_DYNAMIC(CAllControlsSheet, CPropertySheet)

CAllControlsSheet::CAllControlsSheet(UINT nIDCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(nIDCaption, pParentWnd, iSelectPage)
{
	AddControlPages();
}

CAllControlsSheet::CAllControlsSheet(LPCTSTR pszCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(pszCaption, pParentWnd, iSelectPage)
{
	AddControlPages();
}

CAllControlsSheet::~CAllControlsSheet()
{
}

void CAllControlsSheet::AddControlPages()
{
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	m_psh.dwFlags |= PSP_USEHICON;
	m_psh.dwFlags &= ~PSH_HASHELP;  // Lose the Help button
	m_psh.hIcon = m_hIcon;

	AddPage(&m_Basic);
	AddPage(&m_NodeOperations);
	AddPage(&m_MultiPages);
    AddPage(&m_Watches) ;
}

BEGIN_MESSAGE_MAP(CAllControlsSheet, CPropertySheet)
	//{{AFX_MSG_MAP(CAllControlsSheet)
	ON_WM_QUERYDRAGICON()
	ON_WM_SYSCOMMAND()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CAllControlsSheet message handlers

BOOL CAllControlsSheet::OnInitDialog()
{
	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	SetIcon(m_hIcon, TRUE);
	SetIcon(m_hIcon, FALSE);

   // Hide "Cancel", "Help" and "Apply" buttons
   GetDlgItem (IDCANCEL)->EnableWindow (FALSE);
   GetDlgItem (IDCANCEL)->ShowWindow (FALSE);

   GetDlgItem (IDHELP)->EnableWindow (FALSE);
   GetDlgItem (IDHELP)->ShowWindow (FALSE);

   GetDlgItem (ID_APPLY_NOW)->EnableWindow (FALSE);
   GetDlgItem (ID_APPLY_NOW)->ShowWindow (FALSE);

	return CPropertySheet::OnInitDialog();
}
HCURSOR CAllControlsSheet::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

void CAllControlsSheet::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		//CAboutDlg dlgAbout;
		//dlgAbout.DoModal();
	}
	else
	{
		CPropertySheet::OnSysCommand(nID, lParam);
	}
}


