// Watches.cpp : implementation file
//

#include "stdafx.h"
#include "WinCeDemo.h"
#include "Watches.h"
#include "tracetool.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Watches property page

IMPLEMENT_DYNCREATE(Watches, CPropertyPage)

Watches::Watches() : CPropertyPage(Watches::IDD)
{
	//{{AFX_DATA_INIT(Watches)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}

Watches::~Watches()
{
}

void Watches::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(Watches)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(Watches, CPropertyPage)
	//{{AFX_MSG_MAP(Watches)
	ON_BN_CLICKED(IDC_CLEAR, OnClear)
	ON_BN_CLICKED(IDC_DISP, OnDisp)
	ON_BN_CLICKED(IDC_SEND, OnSend)
	ON_BN_CLICKED(IDC_WINWATCH_CLEAR, OnWinwatchClear)
	ON_BN_CLICKED(IDC_WINWATCH_DISPLAY, OnWinwatchDisplay)
	ON_BN_CLICKED(IDC_WINWATCH_SEND, OnWinwatchSend)
	ON_BN_CLICKED(IDC_WINWATCH_CREATE, OnWinwatchCreate)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Watches message handlers

//----------------------------------------------------------------------

// send watch to main watch window
void Watches::OnSend() 
{
   TTrace::Watches()->Send ("test2" , TTrace::CreateTraceID()) ;	
}

//----------------------------------------------------------------------

// clear main watch window
void Watches::OnClear() 
{
   TTrace::Watches()->ClearAll() ;          	
}

//----------------------------------------------------------------------

// display main watch window
void Watches::OnDisp() 
{
   TTrace::Watches()->DisplayWin() ;          
}

//----------------------------------------------------------------------

// create a new watch window
WinWatch * MyWinWatch  = NULL ;

void Watches::OnWinwatchCreate() 
{
   if (MyWinWatch == NULL)
      MyWinWatch = new WinWatch ("MyWinWatchID" , "My watches") ;	
}

//----------------------------------------------------------------------

// send watch to the new wiwatch window
void Watches::OnWinwatchSend() 
{
   if (MyWinWatch == NULL)
      return ;

   SYSTEMTIME Time;
   char buffer [MAX_PATH] ;

   GetLocalTime(&Time);
   sprintf(buffer, "%02d:%02d:%02d:%03d", Time.wHour, Time.wMinute, Time.wSecond, Time.wMilliseconds); 

   MyWinWatch->Send ("Now" , buffer) ;
}

//----------------------------------------------------------------------

// clear the new watch window
void Watches::OnWinwatchClear() 
{
   if (MyWinWatch != NULL)
      MyWinWatch->ClearAll() ;          
}

//----------------------------------------------------------------------

// display the new watch window
void Watches::OnWinwatchDisplay() 
{
   if (MyWinWatch != NULL)
      MyWinWatch->DisplayWin() ;          	
}

//----------------------------------------------------------------------
