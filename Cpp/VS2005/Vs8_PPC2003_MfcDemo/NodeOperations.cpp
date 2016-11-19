// NodeOperations.cpp : implementation file
//

#include "stdafx.h"
#include "VS8_PPC2003Mfc_demo.h"
#include "NodeOperations.h"


// NodeOperations dialog

IMPLEMENT_DYNAMIC(NodeOperations, CPropertyPage)

NodeOperations::NodeOperations()
	: CPropertyPage(NodeOperations::IDD)
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
	CDialog::DoDataExchange(pDX);
}


BEGIN_MESSAGE_MAP(NodeOperations, CPropertyPage)
    ON_BN_CLICKED(IDC_START1, &NodeOperations::OnBnClickedStart1)
    ON_BN_CLICKED(IDC_RESEND, &NodeOperations::OnBnClickedResend)
    ON_BN_CLICKED(IDC_SETSELECTED, &NodeOperations::OnBnClickedSetselected)
    ON_BN_CLICKED(IDC_INDENT2, &NodeOperations::OnBnClickedIndent2)
    ON_BN_CLICKED(IDC_START2, &NodeOperations::OnBnClickedStart2)
    ON_BN_CLICKED(IDC_APPEND, &NodeOperations::OnBnClickedAppend)
    ON_BN_CLICKED(IDC_SHOWNODE, &NodeOperations::OnBnClickedShownode)
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
   sprintf (buffer,"Done %d", col1) ;
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

void NodeOperations::OnBnClickedIndent2()
{
    if (start1 == NULL)
       return ;

    start1->traceNode->Send ("before indent") ;    // send text under the start1 node
    start1->traceNode->Indent ("ident 1") ;        // send text under the start1 node and keep it this trace as the new target for further sub traces
    start1->traceNode->Send ("Level2") ;           // send text under the "indent 1" node
    start1->traceNode->UnIndent ("done") ;         // unindent and send text under the start1 node. Text is optional
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
