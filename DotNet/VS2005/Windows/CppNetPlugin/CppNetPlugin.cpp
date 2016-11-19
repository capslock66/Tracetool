// This is the main DLL file.

#include "stdafx.h" 
#include "CppNetPlugin.h"
#include <string.h>

using namespace TraceTool ;

char PlugName[] = "CppNetPlugin" ;

int main() 
{
   TTrace::Debug->Send ("main") ;
}

extern "C"
{

   //-------------------------------------------------------------------------

   // Get the plugin name 
   __declspec(dllexport) void  __cdecl  GetPlugName (char * lpPlugName)  
   {
      TTrace::Debug->Send (PlugName, "GetPlugName") ;
      strcpy (lpPlugName, PlugName) ;
      try
      {
         //
      } catch (...) {
      } 
   }

   //-------------------------------------------------------------------------     
   //Initialise the plugin. Called after TraceTool load the plugin
   __declspec(dllexport) void __cdecl Start() 
   {
      TTrace::Debug->Send (PlugName, "Start()") ;
   }

   //-------------------------------------------------------------------------     

   //Stop the plugin (free any resources before unloading)
   __declspec(dllexport) void __cdecl Stop() 
   {
      TTrace::Debug->Send (PlugName, "Stop()") ;
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

   __declspec(dllexport) int __cdecl OnAction (char * WinId , int ResourceId ,char *  NodeId) 
   {
      TTrace::Debug->Send (PlugName, "OnAction()") ;
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
   __declspec(dllexport) int __cdecl  OnBeforeDelete (char * WinId , char *  NodeId) 
   {
      TTrace::Debug->Send (PlugName, "OnBeforeDelete()") ;
      return true ;
   }

   //-------------------------------------------------------------------------

   //Called every 500 ms. Can be used for example to refresh labels 
   //The plugin must call LinkToPlugin with CST_PLUG_ONTIMER in order to receive this event 
   __declspec(dllexport) void __cdecl OnTimer() 
   {
      TTrace::Debug->Send (PlugName, "OnTimer()") ;
   }

   //-------------------------------------------------------------------------

} 

