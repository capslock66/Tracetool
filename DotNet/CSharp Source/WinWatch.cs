// WinWatch.cs
//
// WinWatch represent a windows tree where you put watches
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
using System.Collections;                 // ArrayList, queue

namespace TraceTool
{
   /// <summary>
   /// WinWatch represent a windows tree where you put watches
   /// </summary>
   public class WinWatch
   {
      //------------------------------------------------------------------------------

      /// <summary>
      /// The "Required" Id of the window tree, can be any string, or a guid
      /// The Main window trace Id is empty
      /// </summary>
      public string Id ;

      //------------------------------------------------------------------------------

      /// <summary>
      /// When Enabled is false, all traces are disabled. Default is true.
      /// </summary>
      public bool Enabled ;

      //------------------------------------------------------------------------------

      /// <summary>
      /// User variable, provided for the convenience of developers
      /// </summary>
      public object Tag ;

      //------------------------------------------------------------------------------

      /// <summary>
      /// WinWatch constructor : you can map a WinWatch to an existing window
      /// Nothing Is send to the viewer
      /// </summary>
      public WinWatch()
      {
         Enabled = true ;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// WinWatch constructor. The Window Watch is create on the viewer (if not already done)
      /// </summary>
      /// <param name="WinWatchID">Required window trace Id. If empty, a guid will be generated</param>
      /// <param name="WinWatchText">The Window Title on the viewer.If empty, a default name will be used</param>
      public WinWatch (string WinWatchID , string WinWatchText) : this ()
      {
         if (WinWatchID == null || WinWatchID == "")
            WinWatchID = Helper.NewGuid ().ToString() ;

         Id = WinWatchID ;

         if (WinWatchText == null || WinWatchText == "")
            WinWatchText = "Watches " + Id ;

         // create the trace window
         StringList CommandList = new StringList();
         CommandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_WINWATCH_NAME, WinWatchText));
         TTrace.SendToWinWatchClient (CommandList,this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Put the window in foreground
      /// </summary>
      public void DisplayWin ()
      {
         StringList CommandList = new StringList();
         CommandList.Insert(0, String.Format("{0,5}", TraceConst.CST_DISPLAY_TREE));
         TTrace.SendToWinWatchClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Clear all the watches
      /// </summary>
      public void ClearAll ()
      {
         StringList CommandList = new StringList();
         CommandList.Insert(0, String.Format("{0,5}", TraceConst.CST_CLEAR_ALL));
         TTrace.SendToWinWatchClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Clear all the watches
      /// </summary>
      public void Close()
      {
         StringList CommandList = new StringList();
         CommandList.Insert(0, String.Format("{0,5}", TraceConst.CST_CLOSE_WIN));
         TTrace.SendToWinWatchClient(CommandList, this.Id);
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Send a watch
      /// </summary>
      /// <param name="WatchName">Watch name</param>
      /// <param name="WatchValue">Watch value</param>
      public void Send (string WatchName , object WatchValue)
      {
         if (Enabled == false)
            return ;

         StringList CommandList = new StringList();
         CommandList.Insert(0, String.Format("{0,5}{1}", TraceConst.CST_WATCH_NAME, WatchName));

         // create a node with same properties as "self" with new ID
         TraceNodeEx node = new TraceNodeEx (null, false) ;  // no parent, don't generate node id

         node.AddValue (WatchValue  ,  TTrace.Options.SendPrivate , TTrace.Options.ObjectTreeDepth ,"");    // sendPrivate true , max 3 levels, no title
         node.Members.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

         TTrace.SendToWinWatchClient(CommandList, this.Id);
      }

   }
}
