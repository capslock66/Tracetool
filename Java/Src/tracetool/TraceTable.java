/*
 * TraceTable.java
 *
 * HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
 * Download :  http://sourceforge.net/projects/tracetool/
 * See License.txt for license information
 *
 * Author : Thierry Parent
 * Version : 12.4.1
 */
package tracetool;

/*
 * * TraceTable class : construct a table of row to display in the viewer on a node. The table must be associated with a node. see TraceNodeEx.AddTable() and TraceSend.SendTable()
 * @author tpa
 */
/*
 * *
 * @author Thierry Parent
 */
public class TraceTable
{

   private TMemberNode fMembers;
   private TMemberNode fCurrentRow;

   //----------------------------------------------------------------------

   /**
    * create a table
    */
   public TraceTable()
   {
      this.fMembers = new TMemberNode();
      this.fMembers.viewerKind = TraceConst.CST_VIEWER_TABLE;
      this.fCurrentRow = null;
   }

   //----------------------------------------------------------------------

   /**
    * Add columns title : one or more columns titles separated by tabs
    * @param colTitle one or more columns titles separated by tabs. Can also be called several times to add title
    */
   public void addColumnTitle(final String colTitle)
   {
      if (this.fMembers.col1.length() == 0) 
         this.fMembers.col1 = colTitle;
      else
         this.fMembers.col1 = this.fMembers.col1 + "\t" + colTitle; //$NON-NLS-1$
   }

   //----------------------------------------------------------------------

   /**
    * Add an empty row
    */
   public void addRow()
   {
      this.fCurrentRow = this.fMembers.add(""); //$NON-NLS-1$
   }

   //----------------------------------------------------------------------

   /**
    * Add data to current row
    * @param cell one or more columns data separated by tabs. Can also be called several times to add cells
    */

   public void addRowData(String cell)
   {
      if (this.fCurrentRow == null)
         addRow();

      if (this.fCurrentRow.col1.length() == 0) 
         this.fCurrentRow.col1 = cell;
      else
         this.fCurrentRow.col1 = this.fCurrentRow.col1 + "\t" + cell; //$NON-NLS-1$
   }

   //----------------------------------------------------------------------
   /**
    * convert to members
    * @param nodeMembers target
    */
   protected void copyToNodeMembers(TMemberNode nodeMembers)
   {
      TMemberNode TableMembers;
      TableMembers = nodeMembers.add(this.fMembers.col1);
      TableMembers.viewerKind = TraceConst.CST_VIEWER_TABLE;
      for (int c = 0; c < this.fMembers.members.size(); c++)
         TableMembers.add(((TMemberNode)this.fMembers.members.get(c)).col1);
   }

   }
