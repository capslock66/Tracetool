// WinCeDemo.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "WinCeDemo.h"

#include "Sheet.h"
//#include "WinCeDemoDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CWinCeDemoApp

BEGIN_MESSAGE_MAP(CWinCeDemoApp, CWinApp)
//{{AFX_MSG_MAP(CWinCeDemoApp)
// NOTE - the ClassWizard will add and remove mapping macros here.
//    DO NOT EDIT what you see in these blocks of generated code!
//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CWinCeDemoApp construction

CWinCeDemoApp::CWinCeDemoApp()
: CWinApp()
{
   // TODO: add construction code here,
   // Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CWinCeDemoApp object

CWinCeDemoApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CWinCeDemoApp initialization

static HWND	hwndCB=NULL;			// The command bar handle

BOOL CWinCeDemoApp::InitInstance()
{
   if (!AfxSocketInit())
   {
      AfxMessageBox(IDP_SOCKETS_INIT_FAILED);
      return FALSE;
   }
    

   Sheet sheet (_T("TraceTool demo")); 
   m_pMainWnd = &sheet;
   int nResponse = sheet.DoModal ();
   
   
   // Since the dialog has been closed, return FALSE so that we exit the
   //  application, rather than start the application's message pump.
   return FALSE;
}
