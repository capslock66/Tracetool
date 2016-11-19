/*
 * TMemberNode.java
 *
 * HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
 * Download :  http://sourceforge.net/projects/tracetool/
 * See License.txt for license information
 *
 * Author : Thierry Parent
 * Version : 12.4.1
 */

package tracetool;

import java.util.ArrayList;
import java.awt.Color;

/**
 * TMemberNode represent a sub node information in the "info" trace tree.<p>See TraceNode class for sample use.
 * @author tpa
 */
public class TMemberNode
{
   /** The 3 columns to display */
   /** First column */
   public String col1;
   /** Second column */
   public String col2;
   /** Third column */
   public String col3;

   /** Array of sub members (TMemberNode) */
   public ArrayList members;

   /** User defined tag, NOT SEND to the viewer */
   public int tag ;

   /**  Viewer kind. determine how the node will display members */
   public int viewerKind ;

   //----------------------------------------------------------------------

   /** Create a TMemberNode with no text in the 3 columns */
   public TMemberNode()
   {
      this("", "", "") ; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
   }

   //----------------------------------------------------------------------

   /**
    * Create a TMemberNode with a text for the first column
    * @param col1 text of first col
    */
   public TMemberNode (final String col1)
   {
      this(col1, "", ""); //$NON-NLS-1$ //$NON-NLS-2$
   }

   //----------------------------------------------------------------------

   /**
    * Create a TMemberNode with text for the first 2 columns
    * @param col1 text of first col
    * @param col2 text of second col
    */
   public TMemberNode(final String col1, final String col2)
   {
      this(col1, col2, ""); //$NON-NLS-1$
   }

   //----------------------------------------------------------------------

   /**
    * Create a TMemberNode with text for the 3 columns
    * @param col1 text of first col
    * @param col2 text of second col
    * @param col3 text of third col
    */
   public TMemberNode(final String col1, final String col2, final String col3)
   {
      this.col1 = col1;
      this.col2 = col2;
      this.col3 = col3;
      this.members = new ArrayList();
   }

   //----------------------------------------------------------------------

   /**
    * Add a member to the members list
    * @param member the TMember node to add
    * @return the TMember node to add
    */
   public TMemberNode add(final TMemberNode member)
   {
      this.members.add(member);
      return member;
   }

   //----------------------------------------------------------------------

   /**
    * Add a TMemberNode with a text for the first column
    * @param strCol1 text of first col
    * @return the created TMember node
    */
   public TMemberNode add(final String strCol1)
   {
      return add(strCol1, "", ""); //$NON-NLS-1$ //$NON-NLS-2$
   }

   //----------------------------------------------------------------------

   /**
    * Add a TMemberNode with a text for the first 2 columns
    * @param strCol1 text of first col
    * @param strCol2 text of second col
    * @return the created TMember node
    */
   public TMemberNode add(final String strCol1, final String strCol2)
   {
      return add(strCol1, strCol2, ""); //$NON-NLS-1$
   }

   //----------------------------------------------------------------------

   /**
    * Add a TMemberNode with a text for the 3 columns
    * @param strCol1 text of first col
    * @param strCol2 text of second col
    * @param strCol3 text of third col
    * @return the created TMember node
    */
   public TMemberNode add(final String strCol1, final String strCol2, final String strCol3)
   {
      TMemberNode member;
      member = new TMemberNode(strCol1, strCol2, strCol3);
      this.members.add(member);
      return member;
   }

   //----------------------------------------------------------------------

   private ArrayList fontDetails ;

   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Col1=0, Col2=1, Col3=2
    * @param bold Change font to bold
    * @return the TMember node
    */
   public TMemberNode setFontDetail(final int colId, final boolean bold)
   {
      return setFontDetail (colId , bold , false , null , 0 , "") ; //$NON-NLS-1$
    }

   //----------------------------------------------------------------------
   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Col1=0, Col2=1, Col3=2
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @return the TMember node
    */
   public TMemberNode setFontDetail(final int colId, final boolean bold, final boolean italic)
   {
      //java.awt.color.BLUE ;
      return setFontDetail (colId , bold , italic , null , 0 , "") ; //$NON-NLS-1$
   }

   //----------------------------------------------------------------------
   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Col1=0, Col2=1, Col3=2
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @param color Change Color.
    * @return the TMember node
    */
   public TMemberNode setFontDetail(final int colId, final boolean bold, final boolean italic, final Color color)
   {
      return setFontDetail (colId , bold , italic , color , 0 , "") ; //$NON-NLS-1$
   }

   //----------------------------------------------------------------------
   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Col1=0, Col2=1, Col3=2
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @param color Change Color.
    * @param size Change font size, use zero to keep normal size
    * @return the TMember node
    */
   public TMemberNode setFontDetail(final int colId, final boolean bold, final boolean italic, final Color color, final int size)
   {
      return setFontDetail (colId , bold , italic , color , size , "") ;  //$NON-NLS-1$
   }

   //----------------------------------------------------------------------
   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Col1=0, Col2=1, Col3=2
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @param color Change Color.
    * @param size Change font size, use zero to keep normal size
    * @param fontName Change font name
    * @return the TMember node
    */
   public TMemberNode setFontDetail(final int colId, final boolean bold, final boolean italic, final Color color, final int size, final String fontName)
   {
      FontDetail fontDetail = new FontDetail();
      fontDetail.colId    = colId;
      fontDetail.bold     = bold;
      fontDetail.italic   = italic;
      fontDetail.color    = color;    // store color object. Will be converted to "BGR" before sending
      fontDetail.size     = size;
      fontDetail.fontName = fontName;

      if (this.fontDetails == null)
         this.fontDetails = new ArrayList();

      this.fontDetails.add(fontDetail);
      return this;

   }

   //----------------------------------------------------------------------
   /**
    * recursively add members to the node commandList
    * @param commandList Where to store members
    */
   public void addToStringList(final ArrayList commandList)
   {
      // the root node node itself is not send for now.
      // Later we can send the 3 columns text to specify the header, if specfied.
      // the text should be like that : "Myfirstcol:150" where 150 is the column with
      // sub nodes, if any
      for (int c = 0; c < this.members.size(); c++)
      {
         TMemberNode node;
         node = (TMemberNode) this.members.get(c);
         node.internalAddToStringList(commandList);
      }

      // once copied to Commandlist, clear the array
      this.members.clear();
   }

   /**
    * internal use. Recursively add sub members to the array
    * @param commandList Where to store members
    */
   protected void internalAddToStringList(final ArrayList commandList)
   {
      // create the member and set col1
      Utility.addCommand(commandList, TraceConst.CST_CREATE_MEMBER, this.col1);
      // add command if col2 and/or col3 exist
      if (this.col2 != "") //$NON-NLS-1$
         Utility.addCommand(commandList, TraceConst.CST_MEMBER_COL2, this.col2);

      if (this.col3 != "") //$NON-NLS-1$
         Utility.addCommand(commandList, TraceConst.CST_MEMBER_COL3, this.col3);

      // add viewer kind
      if (this.viewerKind != 0)
         Utility.addCommand(commandList, TraceConst.CST_MEMBER_VIEWER_KIND, this.viewerKind);

      // add font detail
      if (this.fontDetails != null)
      {
         for (int c = 0; c < this.fontDetails.size(); c++)
         {
            FontDetail fontDetail = (FontDetail) this.fontDetails.get(c);

            StringBuffer tempStr = new StringBuffer() ;

            tempStr.append(Utility.intToStr5(TraceConst.CST_MEMBER_FONT_DETAIL))
                   .append(Utility.intToStr3(fontDetail.colId)) ;

            if (fontDetail.bold)
               tempStr.append("1"); //$NON-NLS-1$
            else
               tempStr.append("0"); //$NON-NLS-1$

            if (fontDetail.italic)
               tempStr.append("1"); //$NON-NLS-1$
            else
               tempStr.append("0"); //$NON-NLS-1$

            // Color is coded as RGB. convert to BGR
            int colorValue ;
            if (fontDetail.color == null)
               colorValue = -1 ;
            else
               colorValue = (fontDetail.color.getBlue()   << 16)
                          | (fontDetail.color.getGreen()  << 8)
                          | (fontDetail.color.getRed()   << 0);

            tempStr.append(Utility.intToStr11(colorValue))
                   .append(Utility.intToStr11(fontDetail.size))
                   .append(fontDetail.fontName);
            commandList.add(tempStr.toString());  // don't use Utility.addCommand(commandList,...)
         }
         this.fontDetails.clear() ;
         this.fontDetails = null ;  // once copied to commandlist, clear the array
     }

      // recursive add sub nodes, if any
      for (int c = 0; c < this.members.size(); c++)
      {
         TMemberNode node;
         node = (TMemberNode) this.members.get(c);
         node.internalAddToStringList(commandList);
      }

      // close the member group
      Utility.addCommand(commandList, TraceConst.CST_ADD_MEMBER);
   }
}

/**
 * Internal Use.<p>Used to store font detail
 * @author tpa
 */

class FontDetail
{
   int       colId  ;
   boolean   bold;
   boolean   italic;
   Color     color;
   int       size;
   String    fontName;

}
