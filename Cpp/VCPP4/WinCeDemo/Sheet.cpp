// Sheet.cpp : implementation file
//

#include "stdafx.h"
#include "WinCeDemo.h"
#include "resource.h"

#include "Sheet.h"              // tab sheet
#include "Basic.h"              // page 1
#include "NodeOperations.h"     // Page 2
#include "MultiPages.h"         // Page 3
#include "Watches.h"            // Page 4
#include "tracetool.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Sheet

IMPLEMENT_DYNAMIC(Sheet, CPropertySheet)

Sheet::Sheet(UINT nIDCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(nIDCaption, pParentWnd, iSelectPage)
{
}

Sheet::Sheet(LPCTSTR pszCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(pszCaption, pParentWnd, iSelectPage)
{
   AddPage(&basic); 
   AddPage(&nodeOperations) ;
   AddPage(&multiPages) ;
   AddPage(&watches) ;  
}

Sheet::~Sheet()
{
}


BEGIN_MESSAGE_MAP(Sheet, CPropertySheet)
	//{{AFX_MSG_MAP(Sheet)
	ON_COMMAND(IDM_FILE_EXIT, OnFileExit)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Sheet message handlers

BOOL Sheet::OnInitDialog() 
{
	
   
   //ModifyStyle(0, WS_SYSMENU | WS_MINIMIZEBOX); 
   //CMenu   *pSysMenu=GetSystemMenu(FALSE); 
   //pSysMenu->InsertMenu(1, MF_BYPOSITION, SC_RESTORE, "Restore"); 
   //pSysMenu->InsertMenu(3, MF_BYPOSITION, SC_MINIMIZE, "Minimize"); 
      

   // Building the command bar. 
   //m_pCommandBar = new CCeCommandBar(); 
   //m_pCommandBar->Create(this); 

   //CMenu* pMenu = m_pCommandBar->InsertMenuBar(IDM_FILEINFO); 
   //m_pCommandBar->LoadToolBar(IDR_MENUBAR); // IDR_TOOLBAR
   
   ::DrawMenuBar(GetSafeHwnd()); 
   return CPropertySheet::OnInitDialog();
}

void Sheet::OnFileExit() 
{
   //PressButton(PSBTN_OK);    // or 
   EndDialog(IDOK);
	
}
