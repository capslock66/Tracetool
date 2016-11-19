// MultiPages.cpp : implementation file
//

#include "stdafx.h"
#include "Win32Demo.h"
#include "MultiPages.h"
#include ".\multipages.h"


// MultiPages dialog

IMPLEMENT_DYNAMIC(MultiPages, CPropertyPage)
MultiPages::MultiPages()
	: CPropertyPage(IDD_MULTIPAGES)  // MultiPages::IDD
{
   MyWinTrace = NULL ;
   MulticolWintrace = NULL ;
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
}


BEGIN_MESSAGE_MAP(MultiPages, CPropertyPage)
   ON_BN_CLICKED(IDC_NEWWIN, OnBnClickedNewwin)
   ON_BN_CLICKED(IDC_DISPLAYWIN, OnBnClickedDisplaywin)
   ON_BN_CLICKED(IDC_SAYHELLO, OnBnClickedSayhello)
   ON_BN_CLICKED(IDC_SAVETEXT, OnBnClickedSavetext)
   ON_BN_CLICKED(IDC_SAVETOXML, OnBnClickedSavetoxml)
   ON_BN_CLICKED(IDC_LOADXML, OnBnClickedLoadxml)
   ON_BN_CLICKED(IDC_MULTICOL, OnBnClickedMulticol)
END_MESSAGE_MAP()


// MultiPages message handlers

//----------------------------------------------------------------------

// create a new trace window
void MultiPages::OnBnClickedNewwin()
{
   // only one times
   if (MyWinTrace != NULL)
      return ;

   // multiple windows trace
   MyWinTrace = new WinTrace("MyId" , "MyTitle") ;  
}

//----------------------------------------------------------------------

// the viewer switch to the specified window
void MultiPages::OnBnClickedDisplaywin()
{
   if (MyWinTrace == NULL)
      return ;
   MyWinTrace->DisplayWin() ;                        
}

//----------------------------------------------------------------------

// send a message to the trace window
void MultiPages::OnBnClickedSayhello()
{
   if (MyWinTrace == NULL)
      return ;
   MyWinTrace->Debug()->Send ("Hello", "World") ;	
}

//----------------------------------------------------------------------

// save windows messages to a text file
void MultiPages::OnBnClickedSavetext()
{
   if (MyWinTrace == NULL)
      return ;
   MyWinTrace->SaveToTextfile("c:\\log2.txt");	
}

//----------------------------------------------------------------------

// save traces to xml file
void MultiPages::OnBnClickedSavetoxml()
{
   if (MyWinTrace == NULL)
      return ;
   MyWinTrace->SaveToXml("c:\\log2.xml");	
}

//----------------------------------------------------------------------

// load the xml trace into the viewer
void MultiPages::OnBnClickedLoadxml()
{
   MyWinTrace->LoadXml("c:\\log2.xml");	
}

//----------------------------------------------------------------------

// multi column test
void MultiPages::OnBnClickedMulticol()
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
