// VS8_PPC2003_MFC.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "VS8_PPC2003Mfc_demo.h"
#include "MainFrm.h"
#include "Sheet.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CVS8_PPC2003_MFCApp

BEGIN_MESSAGE_MAP(CVS8_PPC2003_MFCApp, CWinApp)
	ON_COMMAND(ID_APP_ABOUT, &CVS8_PPC2003_MFCApp::OnAppAbout)
END_MESSAGE_MAP()



// CVS8_PPC2003_MFCApp construction
CVS8_PPC2003_MFCApp::CVS8_PPC2003_MFCApp()
	: CWinApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

// Basic           m_Basic;
// MultiPages      m_MultiPages;
// NodeOperations  m_NodeOperations;
// Watches         m_Watches ;

// The one and only CVS8_PPC2003_MFCApp object
CVS8_PPC2003_MFCApp theApp;

// CVS8_PPC2003_MFCApp initialization

BOOL CVS8_PPC2003_MFCApp::InitInstance()
{
    // SHInitExtraControls should be called once during your application's initialization to initialize any
    // of the Windows Mobile specific controls such as CAPEDIT and SIPPREF.
    SHInitExtraControls();

    Sheet sheet (_T("TraceTool demo")); 
    m_pMainWnd = &sheet;
    sheet.DoModal (); // ShowWindow(SW_SHOW) ; // 

	return TRUE;
}

// CVS8_PPC2003_MFCApp message handlers


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
#ifdef _DEVICE_RESOLUTION_AWARE
	afx_msg void OnSize(UINT /*nType*/, int /*cx*/, int /*cy*/);
#endif
	virtual BOOL OnInitDialog();
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
}

BOOL CAboutDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	return TRUE;	// return TRUE unless you set the focus to a control
			// EXCEPTION: OCX Property Pages should return FALSE
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
#ifdef _DEVICE_RESOLUTION_AWARE
	ON_WM_SIZE()
#endif
END_MESSAGE_MAP()

#ifdef _DEVICE_RESOLUTION_AWARE
void CAboutDlg::OnSize(UINT /*nType*/, int /*cx*/, int /*cy*/)
{
	DRA::RelayoutDialog(
		AfxGetInstanceHandle(), 
		this->m_hWnd, 
		DRA::GetDisplayMode() != DRA::Portrait ? MAKEINTRESOURCE(IDD_ABOUTBOX_WIDE) : MAKEINTRESOURCE(IDD_ABOUTBOX));
}
#endif

// App command to run the dialog
void CVS8_PPC2003_MFCApp::OnAppAbout()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}
