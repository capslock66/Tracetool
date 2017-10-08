// TraceTable.CS
//
// construct a trace table
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

//using System;
//using System.Collections;  // ArrayList, queue
//using System.Diagnostics;  // Process
//using System.Reflection;
//using System.Text;

#if (!NETCF1 && !SILVERLIGHT)
//using System.Xml.XPath;
#endif


//using System.Runtime.InteropServices;     // for DDL import
//using Microsoft.Win32 ;                   // registry
//using System.IO ;                         // file exist
//using System.Threading ;                  // thread pool, ResetEvent
//using System.Net;
//using System.Net.Sockets;


namespace TraceTool
{
   /// <summary>
   /// TraceTable class : construct a table of row to display in the viewer on a node.
   /// The table must be associated with a node. see TraceNodeEx.AddTable() and TraceToSend.SendTable()
   /// </summary>
   public class TraceTable
   {
      private TMemberNode _members;
      private TMemberNode _currentRow;

      //----------------------------------------------------------------------

      /// <summary>
      /// create a table
      /// </summary>
      public TraceTable()
      {
         _members = new TMemberNode();
         _members.ViewerKind = TraceConst.CST_VIEWER_TABLE;
         _currentRow = null;
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Add columns title : one or more columns titles separated by tabs
      /// </summary>
      /// <param name="colTitle">one or more columns titles separated by tabs. Can also be called several times to add titles</param>
      public void AddColumnTitle(string colTitle)
      {
         if (_members.Col1 == "")
            _members.Col1 = colTitle;
         else
            _members.Col1 = _members.Col1 + "\t" + colTitle;
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Add an empty row
      /// </summary>
      public void AddRow()
      {
         _currentRow = _members.Add("");
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Add data to current row
      /// </summary>
      /// <param name="cell">one or more columns data separated by tabs. Can also be called several times to add cells</param>
      public void AddRowData(string cell)
      {
         if (_currentRow == null)
            AddRow();

         if (_currentRow.Col1 == "")
            _currentRow.Col1 = cell;
         else
            _currentRow.Col1 = _currentRow.Col1 + "\t" + cell;
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// convert to members
      /// </summary>
      /// <param name="nodeMembers">target</param>
      internal void CopyToNodeMembers(TMemberNode nodeMembers)
      {
         TMemberNode TableMembers;
         TableMembers = nodeMembers.Add(_members.Col1);
         TableMembers.ViewerKind = TraceConst.CST_VIEWER_TABLE;
         for (int c = 0; c < _members.Members.Count; c++)
            TableMembers.Add(_members.Members[c].Col1);
      }
   }

   //----------------------------------------------------------------------


}       // namespace TraceTool
