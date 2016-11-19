// SamplePlugin.cpp : Defines the initialization routines for the DLL.
//

// General : use of MFC : Use MFC in a Static Library
// C++ : Additional Include directories : tracetool folder : ../Source
// C++ : Basic Runtime check : both
// C++ : runtime library : Multi-threaded Debug (/MTd)

#include "stdafx.h"
#include "winuser.h"
#include "SamplePlugin.h"
#include "tracetool.h"
#include ".\sampleplugin.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#endif

// CSamplePluginApp

BEGIN_MESSAGE_MAP(CSamplePluginApp, CWinApp)
END_MESSAGE_MAP()


// CSamplePluginApp construction

CSamplePluginApp::CSamplePluginApp()
{
}


//-------------------------------------------------------------------------

// Application variables

CSamplePluginApp theApp;
char PlugName[] = "CppPlugin" ;
WinTrace * PlugTraces  ;
TraceNodeEx * TimerNode ;
TraceNodeEx * ActionNode ;
TraceNodeEx * BeforeDeleteNodes ;

//-------------------------------------------------------------------------

// CSamplePluginApp initialization

BOOL CSamplePluginApp::InitInstance()
{
   CWinApp::InitInstance();
   return TRUE;
}

//-------------------------------------------------------------------------

int CSamplePluginApp::ExitInstance()
{
   return CWinApp::ExitInstance();
}

//-------------------------------------------------------------------------


extern "C"
{

   //-------------------------------------------------------------------------

   // Get the plugin name 
   __declspec(dllexport) void  __cdecl  GetPlugName (char * lpPlugName)  
   {
      TTrace::Debug()->Send ("GetPlugName") ;
      strcpy (lpPlugName, PlugName) ;
   }

   //-------------------------------------------------------------------------

   //Initialise the plugin. Called after TraceTool load the plugin
   __declspec(dllexport) void __cdecl Start() 
   {

      PlugTraces = new WinTrace("CPPWIN" , "C++ Plugin") ;
      //PlugTraces->DisplayWin() ;

      // create a node for the timer event
      TimerNode = new TraceNodeEx (PlugTraces->Debug(), false) ;  // Parent 
      TimerNode->leftMsg = "Timer" ; 
      TimerNode->id = "TimerId" ;       // id is changed after constructor
      TimerNode->Send () ;   

      // create a node for the action event
      ActionNode = new TraceNodeEx (PlugTraces->Debug(), false) ;
      ActionNode->leftMsg = "Action" ;
      ActionNode->id = "ActionId" ; 
      ActionNode->Send () ;   

      // create a node for the BeforeDeleteNodes event
      BeforeDeleteNodes = new TraceNodeEx (PlugTraces->Debug(), false) ;
      BeforeDeleteNodes->leftMsg = "BeforeDelete" ; 
      BeforeDeleteNodes->id = "BeforeDeleteId" ; 
      BeforeDeleteNodes->Send () ;   

      // add a button on right (100 pixels)
      PlugTraces->CreateResource (103,CST_RES_BUT_RIGHT,100,"Press me");

      // attach the window to the plugin (itself)
      PlugTraces->LinkToPlugin (PlugName, CST_PLUG_ONACTION + CST_PLUG_ONBEFOREDELETE + CST_PLUG_ONTIMER) ; 

      PlugTraces->Debug()->Send ("Started") ;


   }

   //-------------------------------------------------------------------------     

   //Stop the plugin (free any resources before unloading)
   __declspec(dllexport) void __cdecl Stop() 
   {
      if (PlugTraces == NULL)  // plugin stopped
         return ;

      PlugTraces->Debug()->Send ("Stopped") ;

      delete TimerNode ;
      delete ActionNode ;
      delete BeforeDeleteNodes ;
      delete PlugTraces ;

      TimerNode         = NULL ;
      ActionNode        = NULL ;
      BeforeDeleteNodes = NULL ;
      PlugTraces        = NULL;
   }

   //-------------------------------------------------------------------------

   // Called when the user click on a button, label or menu on a WinTrace. 
   // The plugin must call WinTrace.LinkToPlugin with CST_PLUG_ONACTION  in order to receive this event.
   // Parameters
   //    WinId  is the Wintrace Id
   //    ResourceId is the User created resource or a tracetool resource.See the Tracetool resources table above
   //    NodeId is the node id of the current selected trace (can be empty)
   // Return value : 
   //    when true : tracetool perform the default action 
   //    when false : tracetool don't perform any action

   __declspec(dllexport) BOOL __cdecl OnAction (char * WinId , int ResourceId ,char *  NodeId) 
   {
      if (PlugTraces == NULL)  // plugin stopped
         return true ;
      switch ( ResourceId )
      {
      case CST_ACTION_CUT         : ActionNode->traceNode->Send ("cut") ;                break ;     // cut
      case CST_ACTION_COPY        : ActionNode->traceNode->Send ("copy") ;               break ;     // copy
      case CST_ACTION_SELECT_ALL  : ActionNode->traceNode->Send ("select all") ;         break ;     // select all
      case CST_ACTION_RESIZE_COLS : ActionNode->traceNode->Send ("resize columns") ;     break ;     // resize columns
      case CST_ACTION_VIEW_INFO   : ActionNode->traceNode->Send ("view trace info") ;    break ;     // view trace info
      case CST_ACTION_VIEW_PROP   : ActionNode->traceNode->Send ("view properties") ;    break ;     // view properties
      case CST_ACTION_PAUSE       : ActionNode->traceNode->Send ("Pause on") ;           break ;     // Pause on
      case CST_ACTION_SAVE        : ActionNode->traceNode->Send ("SaveToFile") ;         break ;     // SaveToFile
      case CST_ACTION_CLEAR_ALL   : ActionNode->traceNode->Send ("clear all") ;          break ;     // clear all
      case CST_ACTION_RESUME      : ActionNode->traceNode->Send ("resume from Pause ") ; break ;     // resume from Pause 
      
      case CST_ACTION_CLOSE_WIN   :   // Close win
            ActionNode->traceNode->Send ("Close win") ;    // demo : disable close button      
            return false ;
            break ;     
      
      case 103 :                      // user defined resource
            ActionNode->traceNode->Send ("You press me !") ;
            break ;

      case CST_ACTION_DELETE : 
            if (strcmp (NodeId, "BeforeDeleteId") == 0 || // BeforeDeleteNodes->id.c_str()
                strcmp (NodeId, "ActionId") == 0 ||       // ActionNode->id.c_str()
                strcmp (NodeId, "TimerId") == 0)          // TimerNode->id.c_str()
            {
               ActionNode->traceNode->Send ("this node cannot be deleted :" , NodeId) ; 
               return false ;
            } else {
               ActionNode->traceNode->Send ("delete selected") ;    
               BeforeDeleteNodes->traceNode->Resend (NULL, NodeId) ;
            }
            break ;

      default : 
         char buffer [50] ;
         sprintf(buffer, "On action. Resource Id : %d ", ResourceId); 
         ActionNode->traceNode->Send (buffer ) ; 
      }

      return true ;
   }

   //-------------------------------------------------------------------------

   //Called when a node is to be deleted on a WinTrace 
   //The plugin must call WinTrace.LinkToPlugin with CST_PLUG_ONBEFOREDELETE in order to receive this event.
   //WinId  is the WinTrace Id
   //NodeId is the node id of the current selected trace
   //Return value : 
   //when true : TraceTool delete the node 
   //when false : TraceTool don't delete the node
   __declspec(dllexport) BOOL __cdecl  OnBeforeDelete (char * WinId , char *  NodeId) 
   {
      //if (PlugTraces == NULL)  // plugin stopped
      //   return true ;

      //BeforeDeleteNodes->traceNode->Resend (NULL, NodeId) ;
      
      //if (strcmp (NodeId, "BeforeDeleteId") == 0 || // BeforeDeleteNodes->id.c_str()
      //    strcmp (NodeId, "ActionId") == 0 ||       // ActionNode->id.c_str()
      //    strcmp (NodeId, "TimerId") == 0)          // TimerNode->id.c_str()
      //{
      //   ActionNode->traceNode->Send ("this node cannot be deleted") ; 
      //   return false ;
      //}
      return true ;
   }

   //-------------------------------------------------------------------------

   //Called every 500 ms. Can be used for example to refresh labels 
   //The plugin must call LinkToPlugin with CST_PLUG_ONTIMER in order to receive this event 
   __declspec(dllexport) void __cdecl OnTimer() 
   {
      if (PlugTraces == NULL)  // plugin stopped
         return ;
      char buffer [20] ;
      SYSTEMTIME Time;
      GetLocalTime(&Time);
      sprintf(buffer, "%02d:%02d:%02d", Time.wHour, Time.wMinute, Time.wSecond); 
      TimerNode->traceNode->Resend (NULL,buffer) ; 
   }

   //-------------------------------------------------------------------------



}

