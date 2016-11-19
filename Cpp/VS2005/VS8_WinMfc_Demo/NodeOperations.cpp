// NodeOperations.cpp : implementation file
//

#include "stdafx.h"
#include "resource.h"		// main symbols
#include "NodeOperations.h"
#include ".\nodeoperations.h"

// NodeOperations dialog

IMPLEMENT_DYNAMIC(NodeOperations, CPropertyPage)
NodeOperations::NodeOperations()
	: CPropertyPage(IDD_NODEEX)    // NodeOperations::IDD
{
   start1 = NULL ;
   start2 = NULL ;
}

NodeOperations::~NodeOperations()
{
   if (start1 != NULL)
      delete start1 ;

   if (start2 != NULL)
      delete start2 ;
}

void NodeOperations::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
}


BEGIN_MESSAGE_MAP(NodeOperations, CPropertyPage)
   ON_BN_CLICKED(IDC_START1, OnBnClickedStart1)
   ON_BN_CLICKED(IDC_RESEND, OnBnClickedResend)
   ON_BN_CLICKED(IDC_SETSELECTED, OnBnClickedSetselected)
   ON_BN_CLICKED(IDC_START2, OnBnClickedStart2)
   ON_BN_CLICKED(IDC_APPEND, OnBnClickedAppend)
   ON_BN_CLICKED(IDC_SHOWNODE, OnBnClickedShownode)
END_MESSAGE_MAP()


// NodeOperations message handlers

//----------------------------------------------------------------------

// create a TraceNodeEx for the Resend and SetSelected demo
void NodeOperations::OnBnClickedStart1()
{
   if (start1 == NULL)
   {
      start1 = TTrace::Debug()->CreateChildEx("Start 1 ..") ;	
      start1->Send() ;
   }
}

//----------------------------------------------------------------------

// Resend the text part of a node
void NodeOperations::OnBnClickedResend()
{
   if (start1 == NULL)
       return ;
   
   char buffer [100] ;
   int col1  ;

   col1 = rand() ;
   sprintf_s (buffer,100,"Done %d", col1) ;
   start1->traceNode->Resend (NULL,buffer ) ;   // resend right part
}

//----------------------------------------------------------------------

// set the node a selected in the viewer
void NodeOperations::OnBnClickedSetselected()
{
    if (start1 == NULL)
       return ;

    start1->traceNode->SetSelected() ;   
}

//----------------------------------------------------------------------

// create a TraceNodeEx for the Append() and Show() demo
void NodeOperations::OnBnClickedStart2()
{
   if (start2 == NULL)
   {
      start2 = new TraceNodeEx () ;
      start2->iconIndex = CST_ICO_CONTROL ;
      start2->leftMsg = "Start 2 .." ;
      start2->Send() ;
   }
}

//----------------------------------------------------------------------

// append text to a node
void NodeOperations::OnBnClickedAppend()
{
   if (start2 == NULL)
       return ;

   start2->traceNode->Append ("..Done") ;   // Append left part
}

//----------------------------------------------------------------------

// ensure the node is visible in the viewer
void NodeOperations::OnBnClickedShownode()
{
   if (start2 == NULL)
       return ;

   start2->traceNode->Show() ;   
}

