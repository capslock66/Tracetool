// member.cs
//
// Describe a member in the right panel of the trace tool
//
// Author : Thierry Parent
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information
//

using System;
using System.Text;         // StringBuilder
using System.Collections.Generic;

// ReSharper disable ClassNeverInstantiated.Global
// ReSharper disable ConvertIfStatementToNullCoalescingExpression
// ReSharper disable ConvertIfStatementToConditionalTernaryExpression
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable IntroduceOptionalParameters.Global
// ReSharper disable FieldCanBeMadeReadOnly.Global
// ReSharper disable UnusedMethodReturnValue.Global
// ReSharper disable UnusedMember.Global
// ReSharper disable InlineOutVariableDeclaration
// ReSharper disable UseStringInterpolation
// ReSharper disable UseObjectOrCollectionInitializer
// ReSharper disable UseNullPropagation
// ReSharper disable MergeCastWithTypeCheck
// ReSharper disable UsePatternMatching
// ReSharper disable ArrangeAccessorOwnerBody

namespace TraceTool
{
    /// <summary>
    /// TMemberNode represent a node inside the right object tree
    /// </summary>

    // ReSharper disable once InconsistentNaming
    public class TMemberNode
    {
        /// <summary>
        /// The 3 columns to display
        /// </summary>
        public string Col1, Col2, Col3, DefaultCol2;
        /// <summary>
        /// an array of sub members (TMemberNode)
        /// </summary>
        public List<TMemberNode> Members;
        /// <summary>
        ///  Viewer kind. determine how the node will display members
        /// </summary>
        public int ViewerKind;
        /// <summary>
        ///  User defined tag, NOT SEND to the viewer
        /// </summary>
        public int Tag;
        /// <summary>
        /// Create a TMemberNode with no text in the 3 columns
        /// </summary>
        public TMemberNode() : this("", "", "") { }
        /// <summary>
        /// Create a TMemberNode with a text for the first column
        /// </summary>
        /// <param name="col1">text of col1</param>
        public TMemberNode(string col1) : this(col1, "", "") { }
        /// <summary>
        /// Create a TMemberNode with text for the first 2 columns
        /// </summary>
        /// <param name="col1">text of col1</param>
        /// <param name="col2">text of col2</param>
        public TMemberNode(string col1, string col2) : this(col1, col2, "") { }
        /// <summary>
        /// Create a TMemberNode with text for the 3 columns
        /// </summary>
        /// <param name="col1">text of col1</param>
        /// <param name="col2">text of col2</param>
        /// <param name="col3">text of col3</param>
        public TMemberNode(string col1, string col2, string col3)
        {
            Col1 = col1;
            Col2 = col2;
            Col3 = col3;
            ViewerKind = 0;
            Members = new List<TMemberNode>();
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Add a member to the members list
        /// </summary>
        /// <param name="member"></param>
        /// <returns>The TMember node to add</returns>
        public TMemberNode Add(TMemberNode member)
        {
            Members.Add(member);
            return member;
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Create a TMemberNode with a text for the first column
        /// </summary>
        /// <param name="col1">text of first col</param>
        /// <returns>The TMember node to add</returns>
        public TMemberNode Add(string col1)
        {
            return Add(new TMemberNode(col1, "", ""));
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Create a TMemberNode with text for the first 2 columns
        /// </summary>
        /// <param name="col1">text of first col</param>
        /// <param name="col2">text of second col</param>
        /// <returns>The TMember node to add</returns>
        public TMemberNode Add(string col1, string col2)
        {
            return Add(new TMemberNode(col1, col2, ""));
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Create a TMemberNode with text for the 3 columns
        /// </summary>
        /// <param name="col1">text of first col</param>
        /// <param name="col2">text of second col</param>
        /// <param name="col3">text of third col</param>
        /// <returns>The TMember node to add</returns>
        public TMemberNode Add(string col1, string col2, string col3)
        {
            return Add(new TMemberNode(col1, col2, col3));
        }

        //----------------------------------------------------------------------
        internal List<FontDetail> FontDetails;

        /// <summary>
        /// Change font detail for an item in the trace
        /// </summary>
        /// <param name="colId">Column index : All columns=-1, Col1=0, Col2=1, Col3=2</param>
        /// <param name="bold">Change font to bold</param>
        /// <returns>The TMember node</returns>
        public TMemberNode SetFontDetail(int colId, bool bold)
        {
            return SetFontDetail(colId, bold, false, -1, 0, "");
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Change font detail for an item in the trace
        /// </summary>
        /// <param name="colId">Column index : All columns=-1, Col1=0, Col2=1, Col3=2</param>
        /// <param name="bold">Change font to bold</param>
        /// <param name="italic">Change font to Italic</param>
        /// <returns>The TMember node</returns>
        public TMemberNode SetFontDetail(int colId, bool bold, bool italic)
        {
            return SetFontDetail(colId, bold, italic, -1, 0, "");
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Change font detail for an item in the trace
        /// </summary>
        /// <param name="colId">Column index : All columns=-1, Col1=0, Col2=1, Col3=2</param>
        /// <param name="bold">Change font to bold</param>
        /// <param name="italic">Change font to Italic</param>
        /// <param name="color">Change Color. To reduce the number assembly reference, the Color structure is not used. Use YourColor.ToArgb() instead. </param>
        /// <returns>The TMember node</returns>
        public TMemberNode SetFontDetail(int colId, bool bold, bool italic, int color)
        {
            return SetFontDetail(colId, bold, italic, color, 0, "");
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Change font detail for an item in the trace
        /// </summary>
        /// <param name="colId">Column index : All columns=-1, Col1=0, Col2=1, Col3=2</param>
        /// <param name="bold">Change font to bold</param>
        /// <param name="italic">Change font to Italic</param>
        /// <param name="color">Change Color. To reduce the number assembly reference, the Color structure is not used. Use YourColor.ToArgb() instead. Use -1 to keep default color</param>
        /// <param name="size">Change font size</param>
        /// <returns>The TMember node</returns>
        public TMemberNode SetFontDetail(int colId, bool bold, bool italic, int color, int size)
        {
            return SetFontDetail(colId, bold, italic, color, size, "");
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Change font detail for an item in the trace
        /// </summary>
        /// <param name="colId">Column index : All columns=-1, Col1=0, Col2=1, Col3=2</param>
        /// <param name="bold">Change font to bold</param>
        /// <param name="italic">Change font to Italic</param>
        /// <param name="color">Change Color. To reduce the number assembly reference, the Color structure is not used. Use YourColor.ToArgb() instead. Use -1 to keep default color</param>
        /// <param name="size">Change font size, use zero to keep normal size</param>
        /// <param name="fontName">Change font name</param>
        /// <returns>The TMember node</returns>
        public TMemberNode SetFontDetail(int colId, bool bold, bool italic, int color, int size, string fontName)
        {
            FontDetail fontDetail = new FontDetail();
            fontDetail.ColId = colId;
            fontDetail.Bold = bold;
            fontDetail.Italic = italic;
            fontDetail.Color = color;
            fontDetail.Size = size;
            fontDetail.FontName = fontName;

            if (FontDetails == null)
                FontDetails = new List<FontDetail>();

            FontDetails.Add(fontDetail);
            return this;
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// recursively add members to the node CommandList
        /// </summary>
        /// <param name="commandList">target command list</param>
        public void AddToStringList(List<string> commandList)
        {
            // the root node node itself is not send for now.
            // Later we can send the 3 columns text to specify the header, if specfied.
            // the text should be like that : "Myfirstcol:150" where 150 is the column with
            // sub nodes, if any
            foreach (TMemberNode node in Members)
            {
                if (node != null)
                    node._AddToStringList(commandList);
            }

            // once copied to Commandlist, clear the array
            Members.Clear();
        }

        //----------------------------------------------------------------------
        // internal use only, used to recursively add sub members to the array
        internal void _AddToStringList(List<string> commandList)
        {
            // create the member and set col1
            Helper.AddCommand(commandList, TraceConst.CST_CREATE_MEMBER, Col1);
            // add command if col2 and/or col3 exist
            if (Col2 != "")
                Helper.AddCommand(commandList, TraceConst.CST_MEMBER_COL2, Col2);

            if (Col3 != "")
                Helper.AddCommand(commandList, TraceConst.CST_MEMBER_COL3, Col3);

            // add viewer kind
            if (ViewerKind != 0)
                Helper.AddCommand(commandList, TraceConst.CST_MEMBER_VIEWER_KIND, ViewerKind);

            // add font detail
            if (FontDetails != null)
            {
                foreach (FontDetail fontDetail in FontDetails)
                {
                    StringBuilder tempStr = new StringBuilder();

                    tempStr.Append(String.Format("{0,5}{1,3}", TraceConst.CST_MEMBER_FONT_DETAIL, fontDetail.ColId));


                    if (fontDetail.Bold)
                        tempStr.Append("1");
                    else
                        tempStr.Append("0");

                    if (fontDetail.Italic)
                        tempStr.Append("1");
                    else
                        tempStr.Append("0");

                    int colorValue;
                    if (fontDetail.Color == -1)
                    {
                        colorValue = -1;
                    }
                    else
                    {
                        // remove Alpha blending
                        colorValue = fontDetail.Color & 0xFFFFFF;
                        // Color is coded as RGB. convert to BGR
                        int b = colorValue & 0xff;
                        int g = (colorValue >> 8) & 0xff;
                        int r = (colorValue >> 0x10) & 0xff;
                        colorValue = (b << 0x10) + (g << 8) + r;
                    }

                    tempStr.Append(String.Format("{0,11}{1,11}", colorValue, fontDetail.Size)).Append(fontDetail.FontName);
                    commandList.Add(tempStr.ToString());

                }
                FontDetails.Clear();
                FontDetails = null;  // once copied to Commandlist, clear the array
            }

            // recursive add sub nodes, if any
            foreach (TMemberNode node in Members)
                if (node != null)
                    node._AddToStringList(commandList);

            // close the member group
            Helper.AddCommand(commandList, TraceConst.CST_ADD_MEMBER);

        }
    }       // TMemberNode class
}
