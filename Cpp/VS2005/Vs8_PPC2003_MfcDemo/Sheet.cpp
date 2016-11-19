// Sheet.cpp : implementation file
//

#include "stdafx.h"
#include "VS8_PPC2003Mfc_demo.h"
#include "Sheet.h"
#include "resourcePPC.h"


// Sheet

IMPLEMENT_DYNAMIC(Sheet, CPropertySheet)

Sheet::Sheet(UINT nIDCaption, CWnd* pParentWnd, UINT iSelectPage)
:CPropertySheet(nIDCaption, pParentWnd, iSelectPage)
{
    AddControlPages() ;
}

Sheet::Sheet(LPCTSTR pszCaption, CWnd* pParentWnd, UINT iSelectPage)
:CPropertySheet(pszCaption, pParentWnd, iSelectPage)
{
    AddControlPages() ;
}

Sheet::~Sheet()
{
}


BEGIN_MESSAGE_MAP(Sheet, CPropertySheet)
END_MESSAGE_MAP()



void Sheet::AddControlPages()
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


// Sheet message handlers

BOOL Sheet::OnInitDialog()
{
    // Add "About..." menu item to system menu.

    // IDM_ABOUTBOX must be in the system command range.

    /*
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
    */

    return CPropertySheet::OnInitDialog();
}
