// Basic.cpp : implementation file
//

#include "stdafx.h"
#include "WinCeDemo.h"
#include "Basic.h"
#include "tracetool.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Basic property page

IMPLEMENT_DYNCREATE(Basic, CPropertyPage)

Basic::Basic() : CPropertyPage(Basic::IDD)
{
	//{{AFX_DATA_INIT(Basic)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}

Basic::~Basic()
{
}

void Basic::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(Basic)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(Basic, CPropertyPage)
	//{{AFX_MSG_MAP(Basic)
	ON_BN_CLICKED(IDSAMPLE1, OnSample1)
	ON_BN_CLICKED(IDC_MAINLOADXML, OnMainloadxml)
	ON_BN_CLICKED(IDC_INDENT, OnIndent)
	ON_BN_CLICKED(IDC_MAINCLEAR, OnMainclear)
	ON_BN_CLICKED(IDC_MAINSAVETEXT, OnMainsavetext)
	ON_BN_CLICKED(IDC_MAINSAVETOXML, OnMainsavetoxml)
	ON_BN_CLICKED(IDC_SHOWVIEWER, OnShowviewer)
	ON_BN_CLICKED(IDC_SAMPLE1, OnSample1)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Basic message handlers

// send samples traces
void Basic::OnSample1() 
{

   char buffer [100] ;
   DWORD thid ; 

    // send the first trace (process name is not send)
   TTrace::Debug()->Send ("Hello");   // single byte string
   TTrace::Debug()->Send (L"C++");    // wide string

   WCHAR wFileName [MAX_PATH+1] ;      
   wFileName[GetModuleFileNameW (0 /* hInstance */ ,wFileName,MAX_PATH)] = 0; 
   TTrace::Debug()->Send (L"Process name", wFileName);    // don't mix char * and wchar_t * in the send() functions

   // force the process name to be send for further traces
   TTrace::Options()->SendProcessName = true ;

   // send some traces : current thread Id, Process ID 
   thid = GetCurrentThreadId();
   sprintf (buffer, "Thread id : %u" , thid) ;
   TTrace::Debug()->Send (buffer);                    // 1 column trace
    
   thid = GetCurrentProcessId() ;
   sprintf (buffer, "%u" , thid) ;
   TTrace::Debug()->Send ("process id", buffer);      // 2 columns traces

   // extended Node with members
   TraceNodeEx * node = TTrace::Debug()->CreateChildEx("MyNode") ;
   node->AddFontDetail (3,true) ;                       // change the "MyNode" trace caption to bold
   node->Members()
      ->Add ("a1" , "b1" , "c1")                        // add member              //   a1        | b1   |  c1
      ->SetFontDetail (0,true)                          // a1 bold
      ->SetFontDetail (1,false,true)                    // a2 Italic 
      ->SetFontDetail (2,false,false,-1,15) ;           // a3 Size 15
   node->Members()
      ->Add (NULL , "b2" )                              // add member              //             | b2   |
      ->SetFontDetail (1,false,false,-1,15,"Symbol") ;  // b2 font Symbol
   node->Members()
      ->Add ("a3") ;                                    // add sub member          //   a3        |      |
   node->Members()
      ->Add ("a4" , "b4" ) ;                            // add sub member          //   a4        | b4   |
   node->Members()         
      ->Add ("a5")                                      // add member              //   a5        |      |
         ->Add ("a6")                                   // add sub member          //      a6     |      |
            ->Add ("a7", "b7") ;                        // add sub sub member      //         a7  | b7   |
   // finally send the node
   node->Send() ;
   delete node ;

   // Dump test
   TTrace::Debug()->SendDump ("Dump test", NULL , "Dump" , buffer , 30) ; 	
}

//----------------------------------------------------------------------

// Indent and Unindent demo
void Basic::OnIndent() 
{
   // Indent and UnIndent 
   TTrace::Debug()->Indent ("Before", "some work");
   TTrace::Debug()->Indent ("Level1") ;
   TTrace::Debug()->Send ("Level2") ;
   TTrace::Debug()->Send ("More level2") ;
   TTrace::Debug()->UnIndent ("Done level 1") ;
   TTrace::Debug()->UnIndent ("Work is done") ;	
}

//----------------------------------------------------------------------

// show the viewer
void Basic::OnShowviewer() 
{
    TTrace::Show(true) ;	
}

//----------------------------------------------------------------------

// clear the main trace window
void Basic::OnMainclear() 
{
    TTrace::ClearAll() ;	
}

//----------------------------------------------------------------------

// save traces to text file
void Basic::OnMainsavetext() 
{
    TTrace::WindowTrace()->SaveToTextfile("c:\\log.txt");	
}

//----------------------------------------------------------------------

// save traces to xml file
void Basic::OnMainsavetoxml() 
{
    TTrace::WindowTrace()->SaveToXml("c:\\log.xml");	
}

//----------------------------------------------------------------------

// load the xml trace into the viewer
void Basic::OnMainloadxml() 
{
    TTrace::WindowTrace()->LoadXml("c:\\log.xml");	
}

//----------------------------------------------------------------------

