// MultiPages.cpp : implementation file
//

#include "stdafx.h"
#include "WinCeDemo.h"
#include "MultiPages.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// MultiPages property page

IMPLEMENT_DYNCREATE(MultiPages, CPropertyPage)

MultiPages::MultiPages() : CPropertyPage(MultiPages::IDD)
{
   MyWinTrace = NULL ;
   MulticolWintrace = NULL ;
   //{{AFX_DATA_INIT(MultiPages)
   // NOTE: the ClassWizard will add member initialization here
   //}}AFX_DATA_INIT
}

MultiPages::~MultiPages()
{
   if (MyWinTrace != NULL)
      delete MyWinTrace ;
   if (MulticolWintrace != NULL)
      delete MulticolWintrace ;
}

void MultiPages::DoDataExchange(CDataExchange* pDX)
{
   CPropertyPage::DoDataExchange(pDX);
   //{{AFX_DATA_MAP(MultiPages)
   // NOTE: the ClassWizard will add DDX and DDV calls here
   //}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(MultiPages, CPropertyPage)
//{{AFX_MSG_MAP(MultiPages)
ON_BN_CLICKED(IDC_DISPLAYWIN, OnDisplaywin)
ON_BN_CLICKED(IDC_LOADXML, OnLoadxml)
ON_BN_CLICKED(IDC_NEWWIN, OnNewwin)
ON_BN_CLICKED(IDC_SAVETEXT, OnSavetext)
ON_BN_CLICKED(IDC_SAVETOXML, OnSavetoxml)
ON_BN_CLICKED(IDC_SAYHELLO, OnSayhello)
	ON_BN_CLICKED(IDC_MULTICOL, OnMulticol)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// MultiPages message handlers

WinTrace * MyWinTrace = NULL ;
WinTrace * MulticolWintrace = NULL ;

//----------------------------------------------------------------------

// create a new trace window
void MultiPages::OnNewwin() 
{
   // only one times
   if (MyWinTrace != NULL)
      return ;

   // multiple windows trace
   MyWinTrace = new WinTrace("MyId" , "MyTitle") ;  
}

//----------------------------------------------------------------------

// the viewer switch to the specified window
void MultiPages::OnDisplaywin() 
{
   if (MyWinTrace == NULL)
      return ;
   MyWinTrace->DisplayWin() ;                        
}

//----------------------------------------------------------------------

// send a message to the trace window
void MultiPages::OnSayhello() 
{
   if (MyWinTrace == NULL)
      return ;
   MyWinTrace->Debug()->Send ("Hello", "World") ;	
}

//----------------------------------------------------------------------

// save windows messages to a text file
void MultiPages::OnSavetext() 
{
   if (MyWinTrace == NULL)
      return ;
   MyWinTrace->SaveToTextfile("c:\\log2.txt");	
}

//----------------------------------------------------------------------

// save traces to xml file
void MultiPages::OnSavetoxml() 
{
   if (MyWinTrace == NULL)
      return ;
   MyWinTrace->SaveToXml("c:\\log2.xml");	
}

//----------------------------------------------------------------------

// load the xml trace into the viewer
void MultiPages::OnLoadxml() 
{
   MyWinTrace->LoadXml("c:\\log2.xml");	
}

//----------------------------------------------------------------------

// multi column test
void MultiPages::OnMulticol() 
{
   char buffer [100] ;
   if (MulticolWintrace == NULL) 
   {
      MulticolWintrace = new WinTrace("MCOLID" , "MultiCol trace window") ;
      MulticolWintrace->SetMultiColumn (1) ;  // must be called before calling setColumnsTitle
      MulticolWintrace->SetColumnsTitle("Column A \t Column B \t Column C ");
      MulticolWintrace->SetColumnsWidth("100:20:80 \t 200:50 \t 100");
      MulticolWintrace->DisplayWin() ;
   }

   int col1 , col2 , col3 ;
   col1 = rand() ;
   col2 = rand() ;
   col3 = rand() ;
   sprintf (buffer,"%d\t%d\t%d", col1 ,col2 ,col3) ;

   MulticolWintrace->Debug()->Send (buffer) ;	
}

//----------------------------------------------------------------------
