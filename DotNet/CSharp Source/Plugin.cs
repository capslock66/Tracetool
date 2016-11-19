// Plugin.cs
//
// Provide classes and interfaces for plugins
//
// Author : Thierry Parent
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information
//
// Change the tracetool project option ("conditional compilation constant") to specify the target dot net version :
// NETF1  (dot net framework 1)          , NETF2 ((dot net framework 2) ,
// NETCF1 (dot net compact framework 1)  , NETCF2 (dot net compact framework 2) , NETCF3 (dot net compact framework 3)

using System;
using System.Reflection;


namespace TraceTool
{
   /// <summary>
   /// Plugin interface. Inherit from this interface to create a TraceTool plugin
   /// </summary>
   public interface ITracePLugin
   {
      /// <summary>
      /// Get the plugin name
      /// </summary>
      /// <returns>
      /// plugin name 
      /// </returns>
      string GetPlugName();
      /// <summary>
      /// Called when the user click on a button, label or menu on a WinTrace.
      /// The plugin must call WinTrace.LinkToPlugin in order to receive this event
      /// </summary>
      /// <param name="WinId">Wintrace Id</param>
      /// <param name="ResourceId">Resource Id</param>
      /// <param name="NodeId">Node id of the current selected trace (can be empty)</param>
      /// <returns>
      ///  when true  : tracetool perform the default action
      ///  when false : tracetool don't perform any action
      /// </returns>
      bool OnAction (string WinId , int ResourceId , string NodeId) ;
      /// <summary>
      /// Called when a node is to be deleted on a WinTrace
      /// The plugin must call WinTrace.LinkToPlugin in order to receive this event
      /// </summary>
      /// <param name="WinId">Wintrace Id</param>
      /// <param name="NodeId">Node Id</param>
      /// <returns>
      ///  when true  : tracetool delete the node
      ///  when false : tracetool don't delete the node
      /// </returns>
      bool OnBeforeDelete(string WinId , string NodeId) ;
      /// <summary>
      /// Called every 500 ms. Can be used for example to refresh labels
      /// The plugin must call LinkToPlugin in order to receive this event
      /// </summary>
      void OnTimer () ;
      /// <summary>
      /// Initialise the plugin
      /// </summary>
      void Start () ;
      /// <summary>
      /// Stop the plugin
      /// </summary>
      void Stop () ;
   }   // ITracePLugin
}      // namespace TraceTool