// Basic.cpp : implementation file
//

#include "stdafx.h"
#include "Win32Demo.h"
#include "Basic.h"
#include ".\basic.h"
#include "tracetool.h"



// Basic dialog

IMPLEMENT_DYNAMIC(Basic, CPropertyPage)
Basic::Basic()
: CPropertyPage(Basic::IDD)
{
}

Basic::~Basic()
{
}

void Basic::DoDataExchange(CDataExchange* pDX)
{
   CPropertyPage::DoDataExchange(pDX);
}


BEGIN_MESSAGE_MAP(Basic, CPropertyPage)
   ON_BN_CLICKED(IDC_INDENT, OnBnClickedIndent)
   ON_BN_CLICKED(IDC_SAMPLE, OnBnClickedSample)
   ON_BN_CLICKED(IDC_MAINSAVETEXT, OnBnClickedMainsavetext)
   ON_BN_CLICKED(IDC_MAINSAVETOXML, OnBnClickedMainsavetoxml)
   ON_BN_CLICKED(IDC_MAINLOADXML, OnBnClickedMainloadxml)
   ON_BN_CLICKED(IDC_SHOWVIEWER, OnBnClickedShowviewer)
   ON_BN_CLICKED(IDC_MAINCLEAR, OnBnClickedMainclear)
END_MESSAGE_MAP()


// Basic message handlers

//----------------------------------------------------------------------

// send samples traces
void Basic::OnBnClickedSample()
{
   char buffer [100] ;
   DWORD thid ; 

   // send the first trace (process name is not send)
   TTrace::Debug()->Send ("Hello C++");


   // force the process name to be send for further traces
   TTrace::Options()->SendProcessName = true ;

   // send some traces : current thread Id, Process ID 
   thid = GetCurrentThreadId();
   sprintf (buffer, "Thread id : %u" , thid) ;
   TTrace::Debug()->Send (buffer);                      // 1 column trace

   thid = GetCurrentProcessId() ;
   sprintf (buffer, "%u" , thid) ;
   TTrace::Warning()->Send ("process id", buffer);      // 2 columns traces

   // single separator
   TTrace::Debug()->Send("---");

   // extended Node with members
   TraceNodeEx * node = TTrace::Debug()->CreateChildEx("MyNode") ;
   node->AddFontDetail (3,true,false,255) ;                       // change the "MyNode" trace caption to bold and red
   node->AddBackgroundColor(12639424,-1) ;                        // background color : light green
   node->Members()
      ->Add ("a1" , "b1" , "c1")                        // add member              //   a1        | b1   |  c1
      ->SetFontDetail (0,true,false,255)                // a1 bold, red
      ->SetFontDetail (1,false,true)                    // b1 Italic 
      ->SetFontDetail (2,false,false,-1,15) ;           // c1 Size 15
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
   TTrace::Options()->SendDate = true ;
   TTrace::Debug()->SendDump ("Dump test", NULL , "Dump" , buffer , 30) ; 	
   TTrace::Options()->SendDate = false ;
}


//----------------------------------------------------------------------

// Indent and Unindent demo
void Basic::OnBnClickedIndent()
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

// save traces to text file
void Basic::OnBnClickedMainsavetext()
{
   TTrace::WindowTrace()->SaveToTextfile("c:\\log.txt");
}

//----------------------------------------------------------------------

// save traces to xml file
void Basic::OnBnClickedMainsavetoxml()
{
   TTrace::WindowTrace()->SaveToXml("c:\\log.xml");
}

//----------------------------------------------------------------------

// load the xml trace into the viewer
void Basic::OnBnClickedMainloadxml()
{
   TTrace::WindowTrace()->LoadXml("c:\\log.xml");
}

//----------------------------------------------------------------------

// show the viewer
void Basic::OnBnClickedShowviewer()
{
   TTrace::Show(true) ;
}

//----------------------------------------------------------------------

// clear the main trace window
void Basic::OnBnClickedMainclear()
{
   TTrace::ClearAll() ;
}
