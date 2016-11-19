// Win32Demo.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "Win32Demo.h"
#include "Win32DemoDlg.h"
#include "propsht.h"
#include "tracetool.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CWin32DemoApp

BEGIN_MESSAGE_MAP(CWin32DemoApp, CWinApp)
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()


// CWin32DemoApp construction

CWin32DemoApp::CWin32DemoApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}


// The one and only CWin32DemoApp object

CWin32DemoApp theApp;


// CWin32DemoApp initialization

BOOL CWin32DemoApp::InitInstance()
{
	// InitCommonControls() is required on Windows XP if an application
	// manifest specifies use of ComCtl32.dll version 6 or later to enable
	// visual styles.  Otherwise, any window creation will fail.
	
   //InitCommonControls();

	//CWinApp::InitInstance();

	AfxEnableControlContainer();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	// of your final executable, you should remove from the following
	// the specific initialization routines you do not need
	// Change the registry key under which our settings are stored
	// TODO: You should modify this string to be something appropriate
	// such as the name of your company or organization
	//SetRegistryKey(_T("Local AppWizard-Generated Applications"));

   CAllControlsSheet sheet (_T("TraceTool demo")); 
	m_pMainWnd = &sheet;
   INT_PTR nResponse = sheet.DoModal ();

   //TTrace::Stop() ;

	return FALSE;
}
