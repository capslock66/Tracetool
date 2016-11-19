// VS9_Mfc_Demo.cpp : Defines the class behaviors for the application.
//
#include "stdafx.h"
#include "VS9_WinMfc_Demo.h"
#include "MainFrm.h"
#include "propsht.h"
#include "..\..\Source\tracetool.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CVS9_Mfc_DemoApp

BEGIN_MESSAGE_MAP(CVS9_Mfc_DemoApp, CWinApp)
	ON_COMMAND(ID_APP_ABOUT, &CVS9_Mfc_DemoApp::OnAppAbout)
END_MESSAGE_MAP()


// CVS9_Mfc_DemoApp construction

CVS9_Mfc_DemoApp::CVS9_Mfc_DemoApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}


// The one and only CVS9_Mfc_DemoApp object

CVS9_Mfc_DemoApp theApp;


// CVS9_Mfc_DemoApp initialization

BOOL CVS9_Mfc_DemoApp::InitInstance()
{
    // InitCommonControlsEx() is required on Windows XP if an application
    // manifest specifies use of ComCtl32.dll version 6 or later to enable
    // visual styles.  Otherwise, any window creation will fail.
    INITCOMMONCONTROLSEX InitCtrls;
    InitCtrls.dwSize = sizeof(InitCtrls);
    // Set this to include all the common control classes you want to use
    // in your application.
    InitCtrls.dwICC = ICC_WIN95_CLASSES;
    InitCommonControlsEx(&InitCtrls);

    CWinApp::InitInstance();

    CAllControlsSheet sheet (_T("TraceTool demo"));
    m_pMainWnd = &sheet;
    INT_PTR nResponse = sheet.DoModal ();

    //TTrace::Stop() ;

    return FALSE;
}


// CVS9_Mfc_DemoApp message handlers




// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	enum { IDD = IDD_ABOUTBOX };

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

// Implementation
protected:
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
END_MESSAGE_MAP()

// App command to run the dialog
void CVS9_Mfc_DemoApp::OnAppAbout()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}


// CVS9_Mfc_DemoApp message handlers

