// Basic.cpp : implementation file
//

#include "stdafx.h"
#include "Basic.h"
#include "..\..\Source\tracetool.h"

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
   ON_BN_CLICKED(IDC_LOCALFILE, &Basic::OnBnClickedLocalfile)
   ON_BN_CLICKED(IDC_WINDOW, &Basic::OnBnClickedWindow)
   ON_BN_CLICKED(IDC_SOCKET, &Basic::OnBnClickedSocket)
   ON_BN_CLICKED(IDC_NONE, &Basic::OnBnClickedNone)
   ON_BN_CLICKED(IDC_MANYNODES, &Basic::OnBnClickedManynodes)
   ON_BN_CLICKED(IDC_SEARCH, &Basic::OnBnClickedSearch)
   ON_BN_CLICKED(IDC_FILTER, &Basic::OnBnClickedFilter)
   ON_BN_CLICKED(IDC_RESETFILTER, &Basic::OnBnClickedResetfilter)
END_MESSAGE_MAP()


// Basic message handlers

//----------------------------------------------------------------------

// send samples traces
void Basic::OnBnClickedSample()
{

   char buffer [100] ;
   DWORD thid ; 

   // send the first traces (process name is not send)
   TTrace::Debug()->Send ("Hello");   // single byte string
   TTrace::Debug()->Send (L"C++");    // wide string
   TTrace::Debug()->Send ("special string chars" , "€ é $ ç à ù");    // wide string

   WCHAR wFileName [MAX_PATH+1] ;      
   wFileName[GetModuleFileNameW (0 /* hInstance */ ,wFileName,MAX_PATH)] = 0; 
   TTrace::Debug()->Send (L"Process name", wFileName);    // don't mix char * and wchar_t * in the send() functions

   // force the process name to be send for further traces
   TTrace::Options()->SendProcessName = true ;

   // send some traces : current thread Id, Process ID 
   thid = GetCurrentThreadId();
   sprintf_s (buffer, 100,"Thread id : %u" , thid) ;
   TTrace::Debug()->Send (buffer);                      // 1 column trace

   thid = GetCurrentProcessId() ;
   sprintf_s (buffer,100, "%u" , thid) ;
   TTrace::Warning()->Send ("process id", buffer);      // 2 columns traces

   // send also the date close to the time
   //  TTrace::Options()->SendDate = true ;
   //  TTrace::Debug()->Send ("trace with date") ;
   //  TTrace::Options()->SendDate = false ;

   // don't send the thread id
   //  TTrace::Options()->SendThreadId = false ;
   //  TTrace::Debug()->Send ("trace without thread id") ;
   //  TTrace::Options()->SendThreadId = true ;

   // single separator
   TTrace::Debug()->Send("---");

   // extended Node with members
   TraceNodeEx * nodeEx = TTrace::Debug()->CreateChildEx("MyNode") ;
   nodeEx->AddFontDetail (3,true,false,255) ;                       // change the "MyNode" trace caption to bold and red
   nodeEx->AddBackgroundColor(12639424,-1) ;                        // background color : light green
   nodeEx->Members()
      ->Add ("a1" , "b1" , "c1")                        // add member              //   a1        | b1   |  c1
      ->SetFontDetail (0,true,false,255)                // a1 bold, red
      ->SetFontDetail (1,false,true)                    // b1 Italic 
      ->SetFontDetail (2,false,false,-1,15) ;           // c1 Size 15
   nodeEx->Members()
      ->Add (NULL , "b2" )                              // add member              //             | b2   |
      ->SetFontDetail (1,false,false,-1,15,"Symbol") ;  // b2 font Symbol
   nodeEx->Members()
      ->Add ("a3") ;                                    // add sub member          //   a3        |      |
   nodeEx->Members()
      ->Add ("a4" , "b4" ) ;                            // add sub member          //   a4        | b4   |
   nodeEx->Members()         
      ->Add ("a5")                                      // add member              //   a5        |      |
         ->Add ("a6")                                   // add sub member          //      a6     |      |
            ->Add ("a7", "b7") ;                        // add sub sub member      //         a7  | b7   |
   nodeEx->AddBackgroundColor(16711935 /*Fuchsia*/,4 );               // change background color col 4 (right msg)
   // finally send the node
   nodeEx->Send() ;   
   
   // once send, the TraceNodeEx::traceNode property represent the node on the viewer

   nodeEx->traceNode->ResendIconIndex(CST_ICO_CONTROL) ;  // change icon after the is send.
   nodeEx->traceNode->Send ("sub node") ;                 // Send sub nodes

   // delete node to prevent memory leak
   delete nodeEx ;       

   // Dump test
   TTrace::Debug()->SendDump ("Dump test", NULL , "Dump" , buffer , 30) ; 	

   // xml test
   TTrace::Debug()->SendXml ("Xml test", "<?xml version='1.0' ?><Data> Hello XML </Data>") ; 	


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

   TTrace::Debug()->EnterMethod("OnBnClickedSample", "",65280) ;  // Lime color   $00FF00
   TTrace::Debug()->Send ("inside method") ;
   TTrace::Debug()->ExitMethod("OnBnClickedSample", "", 65280) ;   // Lime color   $00FF00

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
   TTrace::WindowTrace()->SaveToXml("c:\\logWithStyleSheet.xml", "tracetool.xsl");  
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

void Basic::OnBnClickedLocalfile()
{
   // set viewer log
   // 0, Viewer Log is disabled.
   // 1, Viewer log enabled. 
   // 2, Viewer log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
   TTrace::WindowTrace()->SetLogFile("c:\\logFromViewer.xml", 1,-1);  // -1 : unlimited file size
   // set local log
   // 3, Local log is disabled
   // 4, Local log enabled.
   // 5, Local log enabled. A new file is create each day (CCYYMMDD is appended to the filename)
   TTrace::WindowTrace()->SetLogFile("c:\\logFromCppApi.xml",5,-1);  // -1 : unlimited file size

   TTrace::Debug()->Send("Some traces...");
   TTrace::Flush() ;
   TTrace::Debug()->Send("saved in", TTrace::WindowTrace()->GetLocalLogFile().c_str());
}

void Basic::OnBnClickedWindow()
{
   TTrace::Options()->sendMode = WinMsg ;
}

void Basic::OnBnClickedSocket()
{
   TTrace::Options()->sendMode = Socket ;
}

void Basic::OnBnClickedNone()
{
   TTrace::Options()->sendMode = None ;
}

void Basic::OnBnClickedManynodes()
{
   TTrace::Debug()->Indent ("begin") ;
   for (int c = 1; c <= 50 ; c++)
   {
      TTrace::Debug()->Indent ("level c ") ;
      for (int d = 1; d <= 10 ; d++)
      {
         TTrace::Debug()->Indent ("level d ") ;
         for (int e = 1; e <= 10 ; e++)
            TTrace::Debug()->Send ("level e ") ;
         TTrace::Debug()->UnIndent() ;
      }
      TTrace::Debug()->UnIndent() ;
   }
   TTrace::Debug()->UnIndent() ;
   TTrace::Debug()->Send ("Last message") ;
   // wait until all previous messages (from all threads) are send. 
   // 50 * 10 * 10 messages can be send in 3 to 4 seconds, depending of the processor speed
   TTrace::Flush(10000) ;   //  Wait max 10 seconds (10000 ms). Default wait is 5 seconds
   TTrace::Debug()->Send ("flush done") ;  
   // now compare in the viewer the "Last message" trace time with the "flush done" message time (3 to 4 seconds)
}

void Basic::OnBnClickedSearch()
{
   // ttrace.WinTrace.GotoBookmark(1); // second bookmark, noted [1]
   // ttrace.WinTrace.clearBookmark() ;
   // ttrace.WinTrace.GotoFirstNode() ;
   // ttrace.WinTrace.GotoLastNode() ;

   // TTrace.Find just set the criterias and hightlight if asked, but don't move to the next matching node.
   TTrace::Find("StRinG", false,  true ,  true,  true);// {Sensitive}{WholeWord}{Highlight}{SearchInAllPages}

   // from the current node : go to the next item matching criteria. Call ttrace.WinTrace.GotoFirstNode() before FindNext to start search from first node
   TTrace::WindowTrace()->FindNext( true) ; //{SearForward}
}

void Basic::OnBnClickedFilter()
{
   TTrace::WindowTrace()->ClearFilter();

   // 5 kinds of filters
   // -------------------
   //Equal = 0
   //Not equal = 1
   //contains = 2
   //Don't contains = 3
   //(Ignore this filter) = 4 or -1

   // filters can be applied on all columns. Note that the "icone" column is not zero but 999. The members are identified by the column 998
   // On multicolumn mode. Column 0 can be used normally.

   TTrace::WindowTrace()->AddFilter(/*col icone*/   999,/*Equal*/                 0, "24");           // 999 : Icon column . Filter on "info" (index is 24)
   TTrace::WindowTrace()->AddFilter(/*col time */     1,/*Not equal*/             1, "string");
   TTrace::WindowTrace()->AddFilter(/*col thread*/    2,/*contains*/              2, "0x");
   TTrace::WindowTrace()->AddFilter(/*col traces*/    3,/*Don't contains*/        3, "nothing");
   TTrace::WindowTrace()->AddFilter(/*col Comment*/   4,/*(Ignore this filter)*/ -1, "string");       // -1 or 4 can be used to disable this filter (not very usefull...)
   //TTrace::WindowTrace()->AddFilter(/*col members*/ 998,/*contains*/              2, "string");       // members info : 998

   TTrace::WindowTrace()->ApplyFilter(/*{ConditionAnd*/ true, /*ShowMatch*/ true, /*IncludeChildren*/ true);
}

void Basic::OnBnClickedResetfilter()
{
   TTrace::WindowTrace()->ClearFilter();
}
