// NodeOperations.cpp : implementation file
//

#include "stdafx.h"
#include "WinCeDemo.h"
#include "NodeOperations.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// NodeOperations property page

IMPLEMENT_DYNCREATE(NodeOperations, CPropertyPage)

NodeOperations::NodeOperations() : CPropertyPage(NodeOperations::IDD)
{
   start1 = NULL ;
   start2 = NULL ;
   //{{AFX_DATA_INIT(NodeOperations)
   // NOTE: the ClassWizard will add member initialization here
   //}}AFX_DATA_INIT
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
   //{{AFX_DATA_MAP(NodeOperations)
   // NOTE: the ClassWizard will add DDX and DDV calls here
   //}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(NodeOperations, CPropertyPage)
//{{AFX_MSG_MAP(NodeOperations)
ON_BN_CLICKED(IDC_APPEND, OnAppend)
ON_BN_CLICKED(IDC_RESEND, OnResend)
ON_BN_CLICKED(IDC_SETSELECTED, OnSetselected)
ON_BN_CLICKED(IDC_SHOWNODE, OnShownode)
ON_BN_CLICKED(IDC_START1, OnStart1)
ON_BN_CLICKED(IDC_START2, OnStart2)
//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// NodeOperations message handlers


//----------------------------------------------------------------------


// create a TraceNodeEx for the Resend and SetSelected demo
void NodeOperations::OnStart1() 
{
   if (start1 == NULL)
   {
      start1 = TTrace::Debug()->CreateChildEx("Start 1 ..") ;	
      start1->Send() ;
   }
}

//----------------------------------------------------------------------

// Resend the text part of a node
void NodeOperations::OnResend() 
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
void NodeOperations::OnSetselected() 
{
    if (start1 == NULL)
       return ;

    start1->traceNode->SetSelected() ;   
}

//----------------------------------------------------------------------

// create a TraceNodeEx for the Append and Show demo
void NodeOperations::OnStart2() 
{
   if (start2 == NULL)
   {
      start2 = TTrace::Debug()->CreateChildEx ("Start 2 ..") ;
      start2->Send() ;
   }
}

//----------------------------------------------------------------------

// append text to a node
void NodeOperations::OnAppend() 
{
   if (start2 == NULL)
       return ;

   start2->traceNode->Append ("..Done") ;   // Append left part
}


//----------------------------------------------------------------------

// ensure the node is visible in the viewer
void NodeOperations::OnShownode() 
{
   if (start2 == NULL)
       return ;

   start2->traceNode->Show() ;   
}

