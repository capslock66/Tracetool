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
// Change the tracetool project option ("conditional compilation constant") to specify the target dot net version :
// NETF1  (dot net framework 1)          , NETF2 ((dot net framework 2) ,
// NETCF1 (dot net compact framework 1)  , NETCF2 (dot net compact framework 2) , NETCF3 (dot net compact framework 3)

using System;
using System.Collections;  // ArrayList, queue
using System.Text;         // StringBuilder

// generic start in F2
#if (!NETCF1 && !NETF1)
using System.Collections.Generic;
#endif

namespace TraceTool
{
   /// <summary>
   /// TMemberNode represent a node inside the right object tree
   /// </summary>

   public class TMemberNode
   {
      /// <summary>
      /// The 3 columns to display
      /// </summary>
      public string Col1, Col2, Col3 ;
      /// <summary>
      /// an array of sub members (TMemberNode)
      /// </summary>
      public MemberList Members;
      /// <summary>
      ///  Viewer kind. determine how the node will display members
      /// </summary>
      public int ViewerKind ;
      /// <summary>
      ///  User defined tag, NOT SEND to the viewer
      /// </summary>
      public int Tag ;
      /// <summary>
      /// Create a TMemberNode with no text in the 3 columns
      /// </summary>
      public TMemberNode () : this ("" , "" , "") {}
      /// <summary>
      /// Create a TMemberNode with a text for the first column
      /// </summary>
      /// <param name="col1">text of col1</param>
      public TMemberNode (string col1 ) : this (col1 , "" , "") {}
      /// <summary>
      /// Create a TMemberNode with text for the first 2 columns
      /// </summary>
      /// <param name="col1">text of col1</param>
      /// <param name="col2">text of col2</param>
      public TMemberNode (string col1 , string col2) : this (col1 , col2 , "") {}
      /// <summary>
      /// Create a TMemberNode with text for the 3 columns
      /// </summary>
      /// <param name="col1">text of col1</param>
      /// <param name="col2">text of col2</param>
      /// <param name="col3">text of col3</param>
      public TMemberNode (string col1 , string col2 , string col3)
      {
         Col1 = col1 ;
         Col2 = col2 ;
         Col3 = col3 ;
         ViewerKind = 0;
         Members = new MemberList();
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Add a member to the members list
      /// </summary>
      /// <param name="member"></param>
      /// <returns>The TMember node to add</returns>
      public TMemberNode Add (TMemberNode member)
      {
         Members.Add (member) ;
         return member ;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Create a TMemberNode with a text for the first column
      /// </summary>
      /// <param name="col1">text of first col</param>
      /// <returns>The TMember node to add</returns>
      public TMemberNode Add (string col1)
      {
         return Add (new TMemberNode(col1 , "" , ""));
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Create a TMemberNode with text for the first 2 columns
      /// </summary>
      /// <param name="col1">text of first col</param>
      /// <param name="col2">text of second col</param>
      /// <returns>The TMember node to add</returns>
      public TMemberNode Add (string col1 , string col2)
      {
         return Add (new TMemberNode(col1 , col2 , "")) ;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Create a TMemberNode with text for the 3 columns
      /// </summary>
      /// <param name="col1">text of first col</param>
      /// <param name="col2">text of second col</param>
      /// <param name="col3">text of third col</param>
      /// <returns>The TMember node to add</returns>
      public TMemberNode Add (string col1 , string col2 , string col3)
      {
         return Add (new TMemberNode (col1,col2,col3)) ;
      }

      //----------------------------------------------------------------------
      internal FontDetailList FontDetails;

      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="ColId">Column index : All columns=-1, Col1=0, Col2=1, Col3=2</param>
      /// <param name="Bold">Change font to bold</param>
      /// <returns>The TMember node</returns>
      public TMemberNode SetFontDetail(int ColId, bool Bold)
      {
         return SetFontDetail(ColId, Bold, false, -1, 0, "");
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="ColId">Column index : All columns=-1, Col1=0, Col2=1, Col3=2</param>
      /// <param name="Bold">Change font to bold</param>
      /// <param name="Italic">Change font to Italic</param>
      /// <returns>The TMember node</returns>
      public TMemberNode SetFontDetail(int ColId, bool Bold, bool Italic)
      {
         return SetFontDetail(ColId, Bold, Italic, -1, 0, "");
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="ColId">Column index : All columns=-1, Col1=0, Col2=1, Col3=2</param>
      /// <param name="Bold">Change font to bold</param>
      /// <param name="Italic">Change font to Italic</param>
      /// <param name="Color">Change Color. To reduce the number assembly reference, the Color structure is not used. Use YourColor.ToArgb() instead. </param>
      /// <returns>The TMember node</returns>
      public TMemberNode SetFontDetail(int ColId, bool Bold, bool Italic, int Color)
      {
         return SetFontDetail(ColId, Bold, Italic, Color, 0, "");
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="ColId">Column index : All columns=-1, Col1=0, Col2=1, Col3=2</param>
      /// <param name="Bold">Change font to bold</param>
      /// <param name="Italic">Change font to Italic</param>
      /// <param name="Color">Change Color. To reduce the number assembly reference, the Color structure is not used. Use YourColor.ToArgb() instead. Use -1 to keep default color</param>
      /// <param name="Size">Change font size</param>
      /// <returns>The TMember node</returns>
      public TMemberNode SetFontDetail(int ColId, bool Bold, bool Italic, int Color, int Size)
      {
         return SetFontDetail(ColId, Bold, Italic, Color, Size, "");
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="ColId">Column index : All columns=-1, Col1=0, Col2=1, Col3=2</param>
      /// <param name="Bold">Change font to bold</param>
      /// <param name="Italic">Change font to Italic</param>
      /// <param name="Color">Change Color. To reduce the number assembly reference, the Color structure is not used. Use YourColor.ToArgb() instead. Use -1 to keep default color</param>
      /// <param name="Size">Change font size, use zero to keep normal size</param>
      /// <param name="FontName">Change font name</param>
      /// <returns>The TMember node</returns>
      public TMemberNode SetFontDetail(int ColId, bool Bold, bool Italic, int Color, int Size, string FontName)
      {
         FontDetail fontDetail = new FontDetail();
         fontDetail.ColId    = ColId;
         fontDetail.Bold     = Bold;
         fontDetail.Italic   = Italic;
         fontDetail.Color    = Color;
         fontDetail.Size     = Size;
         fontDetail.FontName = FontName;

         if (FontDetails == null)
            FontDetails = new FontDetailList();

         FontDetails.Add(fontDetail);
         return this;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// recursively add members to the node CommandList
      /// </summary>
      /// <param name="CommandList">target command list</param>
      public void AddToStringList(StringList CommandList)
      {
         // the root node node itself is not send for now.
         // Later we can send the 3 columns text to specify the header, if specfied.
         // the text should be like that : "Myfirstcol:150" where 150 is the column with
         // sub nodes, if any
         foreach (TMemberNode node in Members)
         {
            if (node != null)
               node._AddToStringList (CommandList) ;
         }

         // once copied to Commandlist, clear the array
         Members.Clear ();
      }

      //----------------------------------------------------------------------
      // internal use only, used to recursively add sub members to the array
      internal void _AddToStringList(StringList CommandList)
      {
         // create the member and set col1
         Helper.addCommand (CommandList, TraceConst.CST_CREATE_MEMBER, Col1);
         // add command if col2 and/or col3 exist
         if (Col2 != "")
            Helper.addCommand(CommandList, TraceConst.CST_MEMBER_COL2, Col2);

         if (Col3 != "")
            Helper.addCommand(CommandList, TraceConst.CST_MEMBER_COL3, Col3);

         // add viewer kind
         if (ViewerKind != 0)
            Helper.addCommand(CommandList, TraceConst.CST_MEMBER_VIEWER_KIND, ViewerKind);

         // add font detail
         if (FontDetails != null)
         {
            foreach (FontDetail fontDetail in FontDetails)
            {
               StringBuilder TempStr = new StringBuilder() ;

               TempStr.Append(String.Format("{0,5}{1,3}", TraceConst.CST_MEMBER_FONT_DETAIL , fontDetail.ColId)) ;


               if (fontDetail.Bold)
                  TempStr.Append("1");
               else
                  TempStr.Append("0");

               if (fontDetail.Italic)
                  TempStr.Append("1");
               else
                  TempStr.Append("0");

               int colorValue ;
               if (fontDetail.Color == -1)
               {
                  colorValue = -1 ;
               }
               else
               {
                  // remove Alpha blending
                  colorValue = fontDetail.Color & 0xFFFFFF;
                  // Color is coded as RGB. convert to BGR
                  int B = colorValue & 0xff;
                  int G = (colorValue >> 8) & 0xff;
                  int R = (colorValue >> 0x10) & 0xff;
                  colorValue = (B << 0x10) + (G << 8) + R;
               }

               TempStr.Append(String.Format("{0,11}{1,11}", colorValue, fontDetail.Size)).Append(fontDetail.FontName);
               CommandList.Add(TempStr.ToString());

            }
            FontDetails.Clear() ;
            FontDetails = null ;  // once copied to Commandlist, clear the array
         }

         // recursive add sub nodes, if any
         foreach (TMemberNode node in Members)
            if (node != null)
               node._AddToStringList (CommandList) ;

         // close the member group
         Helper.addCommand(CommandList, TraceConst.CST_ADD_MEMBER);

      }
   }       // TMemberNode class
}
