// TraceNodeEx.cs
//
// construct the trace node
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
using System.Text;
using System.Reflection;
using System.Diagnostics;  // Process
using System.Collections;  // ArrayList, queue
using System.Drawing.Imaging;
//using System.Drawing;      // images
using System.IO;           // streams

#if (!NETCF1 && !SILVERLIGHT)
using System.Xml.XPath;
#endif

// generic start in F2
#if (!NETCF1 && !NETF1)
using System.Collections.Generic;
#endif

#if (NETF3)
using System.Windows.Markup.Primitives;   // dependency properties (don't work with Silverlight).  Assembly : PresentationFramework
#endif

#if (NETF3 || SILVERLIGHT)
using System.Windows.Media.Imaging;
// ReSharper disable InconsistentNaming
#endif

namespace TraceTool
{
   /// <summary> Alternate way to send traces : prepare a TraceNode with all properties then send it.
   /// </summary>
   public class TraceNodeEx : TraceNodeBase
   {
      /// <summary>
      /// The Id of the parent node
      /// </summary>
      public string ParentNodeId ;
      /// <summary>
      /// The left part of the tree message
      /// </summary>
      public string LeftMsg ;
      /// <summary>
      /// The right part of the tree message
      /// </summary>
      public string RightMsg ;
      /// <summary>
      /// time
      /// </summary>
      public string Time ;
      /// <summary>
      /// thread name
      /// </summary>
      public string ThreadName ;

      /// <summary>
      /// the root for the Member tree
      /// </summary>
      public TMemberNode Members = new TMemberNode();

      internal FontDetailList FontDetails;

      //----------------------------------------------------------------------
      /// <summary>
      /// create a Node with an unique ID
      /// </summary>

      public TraceNodeEx ()
      {
         this.Id = Helper.NewGuid().ToString();
         this.IconIndex = TraceConst.CST_ICO_DEFAULT;
         this.Enabled = true;
         this.WinTraceId = null;
         this.ParentNodeId = "" ;
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// create a Node with an unique ID (true)
      /// </summary>
      /// <param name="ParentNode">The parent node where to place that trace.
      /// The IconIndex and the enabled properties are also recopied
      /// Can be null : the root tree become the parent node, enabled is true and the default icon is used
      /// </param>

      public TraceNodeEx (TraceToSend ParentNode)
      {
         this.Id = Helper.NewGuid().ToString();

         if (ParentNode == null)
         {
            this.IconIndex    = TraceConst.CST_ICO_DEFAULT;
            this.Enabled      = true;
            this.WinTraceId   = null;
            this.ParentNodeId = "";
         }
         else
         {
            this.IconIndex    = ParentNode.IconIndex;
            this.Enabled      = ParentNode.Enabled;
            this.WinTraceId   = ParentNode.WinTraceId;
            this.ParentNodeId = ParentNode.GetLastContextId();
         }
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Create a Node.
      /// </summary>
      /// <param name="ParentNode">The parent node where to place that trace.
      /// The IconIndex and the enabled properties are also recopied
      /// Can be null : the root tree become the parent node, enabled is true and the default icon is used
      /// </param>
      /// <param name="generateUniqueId">if true, the id is generated automatically, else set the empty string
      /// </param>

      public TraceNodeEx(TraceToSend ParentNode, bool generateUniqueId)
      {
         if (generateUniqueId)
            Id = Helper.NewGuid().ToString();

         if (ParentNode == null)
         {
            this.IconIndex = TraceConst.CST_ICO_DEFAULT;
            this.Enabled = true;
            this.WinTraceId = null;
            this.ParentNodeId = "";
         }
         else
         {
            this.IconIndex    = ParentNode.IconIndex;
            this.Enabled      = ParentNode.Enabled;
            this.WinTraceId   = ParentNode.WinTraceId;
            this.ParentNodeId = ParentNode.Id;
         }
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Call AddObject to fill the "member" tree with the object description
      /// </summary>
      /// <param name="ObjToSend"></param>
      public void AddObject (object ObjToSend)
      {
         AddObject (ObjToSend, TTrace.Options.GetDefault()) ;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Call AddObject to fill the "member" tree with the object description
      /// </summary>
      /// <param name="ObjToSend">Object to send</param>
      /// <param name="flags">determine what information to send</param>
      public void AddObject (object ObjToSend, TraceDisplayFlags flags)
      {
         if (Enabled == false)
            return ;
         Type oType ;
         if (ObjToSend == null)
            oType = null ;
         else
            oType = ObjToSend.GetType();

         AddTypeObject (ObjToSend  , oType, flags );        // add info to this.Members
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// Call AddType to fill the "member" tree with the object type
      /// </summary>
      /// <param name="typeToSend">Object type to send</param>
      public void AddType (Type typeToSend)
      {
         AddType (typeToSend, TTrace.Options.GetDefault()) ;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Call AddType to fill the "member" tree with the object type
      /// </summary>
      /// <param name="typeToSend">Object type to send</param>
      /// <param name="flags">determine what infrmation to send</param>
      public void AddType (Type typeToSend, TraceDisplayFlags flags)
      {
         if (Enabled == false)
            return ;
         AddTypeObject (null  , typeToSend, flags);        // add info to this.Members
      }

      //----------------------------------------------------------------------

#if (NETCF1 || NETCF2 || NETCF3)
      internal static Int32 SendObjectRecursiveStatus = 0;
#else
      /// <summary>
      /// SendObjectRecursiveStatus is used to block recursive call to AddTypeObject
      /// since AddTypeObject will try to evaluate field GET method.
      /// If the GET method call AddObject or similar function,
      /// we have a possible recursive call (with stack overflow).
      /// </summary>
      [ThreadStatic] internal static Int32 SendObjectRecursiveStatus = 0 ;
#endif

      /// fill the Members member with a type description and optional values of that type
      /// caller : AddType, AddObject
      internal void AddTypeObject(Object ObjToSend, Type oType, TraceDisplayFlags flags)
      {
         if (Enabled == false)
            return;

         // detect null type
         if (oType == null)
         {
            Members.Add("Null Type");
            return;
         }

         string str;
         string strModifier = "";
         string strName = "";
         string strValue = "";

         ReflectionHelper.Type2String(oType, ref strModifier, ref strName);

         if ((flags & TraceDisplayFlags.ShowModifiers) != 0)
         {
            if (strModifier != "")
               strModifier = strModifier + " ";
         }
         else
         {
            strModifier = "";
         }

         // only 2 parts are not optional : quick info and properties

         // quick info on the root (not optional)
         // --------------------------------------
         TMemberNode memberClass ;
         if (ObjToSend != null)
         {
            try
            {
               strValue = ObjToSend.ToString();
            }
            catch (Exception e)
            {
               strValue = "AddTypeObject : ObjToSend.ToString() threw an exception of type " + e.GetType();
            }            // don't display the value if it's the same as the type name
            if (strValue == oType.ToString())
               memberClass = new TMemberNode(strModifier + strName);
            else
               memberClass = new TMemberNode(strModifier + strName, strValue);
         }
         else
            memberClass = new TMemberNode(strModifier + strName);
         memberClass.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
         Members.Add(memberClass);

         // detect recursive call to SendObject when DisplayFields try to get fields values
         if (SendObjectRecursiveStatus > 0)
         {
            TMemberNode memberError = new TMemberNode("Recursive call detected");
            memberError.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
            Members.Add(memberError);
            memberError.Add("Error : A Field Read method has called SendObject/AddObject.");
            memberError.Add("->Possible recursive call is stopped now");
            SendObjectRecursiveStatus = 2; // tell the calling SendObject that a recursive call was stopped
            return;
         }
         // reset recursive call flag
         SendObjectRecursiveStatus = 1;

         //XPathNavigator DocumentationNav = null;
         Object DocumentationNav = null;

#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)
         string XmlDocFileName = ReflectionHelper.AssemblyDocumentationFileName(oType);

         if ((flags & TraceDisplayFlags.ShowDoc) != 0)
         {
            // exception can occur here if XML file is incorrect.
            try
            {
               XPathDocument XMLDoc = null;
               XMLDoc = new XPathDocument(XmlDocFileName);
               DocumentationNav = XMLDoc.CreateNavigator();
            }
            catch
            {
               DocumentationNav = null;
            }
         }

#endif

         try
         {
            // Class info
            //---------------
            if ((flags & TraceDisplayFlags.ShowClassInfo) != 0)
            {
               TMemberNode classGroup = new TMemberNode("Class information");
               classGroup.SetFontDetail(0, true);
               classGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
               Members.Add(classGroup);

               // in case of ReflectionHelper.Type2String is different of OBJ.ToString() :
               if (ObjToSend != null)
               {
                   try
                   {
                       str = ObjToSend.ToString();
                   }
                   catch (Exception e)
                   {
                       str = "AddTypeObject : ObjToSend.ToString() threw an exception of type " + e.GetType();
                   }
                   if (str != strName)
                     classGroup.Add("OBJ.ToString", str);
               }
               else
                  classGroup.Add("Null Object");

               // in case of ReflectionHelper.Type2String is different of oType.ToString() :
               str = oType.ToString();
               if (str != strName)
                  classGroup.Add("Type.ToString", str);

               // display boolean flags if true
               if (oType.IsByRef)
                  classGroup.Add("IsByRef");
               if (oType.IsCOMObject)
                  classGroup.Add("IsCOMObject");
               if (oType.IsImport)
                  classGroup.Add("IsImport");
               if (oType.IsNotPublic)
                  classGroup.Add("IsNotPublic");
               if (oType.IsSpecialName)
                  classGroup.Add("IsSpecialName");


#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)
               if (oType.IsContextful)
                  classGroup.Add("IsContextful");
               if (oType.IsMarshalByRef)
                  classGroup.Add("IsMarshalByRef");
               if (oType.IsSerializable)
                  classGroup.Add("IsSerializable");
               if (oType.IsExplicitLayout)// Class layout
                  classGroup.Add("Class layout", "Explicit");
               if (oType.IsLayoutSequential)// Class layout
                  classGroup.Add("Class layout", "Sequential");
#endif

               if (oType.IsAutoLayout)// Class layout
                  classGroup.Add("Class layout", "Auto");


               // boolean and corresponding value
               if (oType.IsPrimitive)
                  classGroup.Add("IsPrimitive", oType.UnderlyingSystemType.ToString());

               //if (oType.HasElementType)
               //   ClassGroup.Add("HasElementType" , oType.GetElementType().ToString());

               // Array .See Type2ShortString for array
               if (oType.IsArray)
               {
                  Array arr = (Array)ObjToSend;
                  StringBuilder result = new StringBuilder();

                  result.Append("[");
                  result.Append(arr.GetLowerBound(0)).Append("..").Append(arr.GetUpperBound(0));
#if ((!NETCF1) || (NETCF2) || (NETCF3))  // GetArrayRank start from CF 2
                  for (int i = 1; i < oType.GetArrayRank(); i++)
                     result.Append(",").Append(arr.GetLowerBound(i)).Append("..").Append(arr.GetUpperBound(i));
                  result.Append("]");
#endif
                  classGroup.Add("IsArray", oType.GetElementType().ToString(), result.ToString());
               }

               // String Format
               if (oType.IsAnsiClass)
                  classGroup.Add("String Format", "Ansi");
               if (oType.IsAutoClass)
                  classGroup.Add("String Format", "Auto");
               if (oType.IsUnicodeClass)
                  classGroup.Add("String Format", "Unicode");

               // show all other infos giving strings
#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)
               classGroup.Add("GUID", oType.GUID.ToString());
               if (oType.TypeInitializer != null)
                  classGroup.Add("TypeInitializer", oType.TypeInitializer.ToString());
#endif
               if (oType.Namespace != null)
                  classGroup.Add("Namespace", oType.Namespace.ToString());
               classGroup.Add("TypeHandle", oType.TypeHandle.ToString());
               if (oType.ReflectedType != null)
                  classGroup.Add("ReflectedType", oType.ReflectedType.ToString());


               if (ObjToSend != null)
                  classGroup.Add("OBJ.HashCode", ObjToSend.GetHashCode().ToString("X2"));
               classGroup.Add("Type.HashCode", oType.GetHashCode().ToString("X2"));

               TypeCode myTypeCode = Type.GetTypeCode(oType);
               classGroup.Add("TypeCode", myTypeCode.ToString());

               // Module, Assembly name, location and XML documentation file name
               classGroup.Add("Module", oType.Module.ToString());
                try
                {
                    classGroup.Add("Assembly", oType.Assembly.ToString());
                }
                catch (Exception e)
                {
                    classGroup.Add("Assembly", e.GetType().ToString());
                }
#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)
                try
                {
                    classGroup.Add("Assembly location", oType.Assembly.Location);
                }
                catch (Exception e)
                {
                    classGroup.Add("Assembly location", e.GetType().ToString());
                }               // documentation
               if (XmlDocFileName != "")
               {
                  classGroup.Add("XML Documentation", XmlDocFileName);
                  if (DocumentationNav == null)
                     classGroup.Add("XML Documentation", "Unable to open file");
               }

               AddDocumentation(DocumentationNav, classGroup, oType, null);      // null MemberInfo
#endif

               // Custom attributes.
               if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
               {
                  // an other way to do it is to call : Object [] CustomAttribs = oType.GetCustomAttributes (true) ;
                  Attribute[] CusAttib = Attribute.GetCustomAttributes(oType, true);  // true : inherit
                  foreach (Attribute attr in CusAttib)
                     classGroup.Add("Custom attribute", attr.ToString());
               }
            }

            // Describe members, base type and nested type
            // -------------------------------------------

            // FIELDS
            if ((flags & TraceDisplayFlags.ShowFields) != 0)
               DisplayFields(ObjToSend, oType,DocumentationNav,flags);

            // PROPERTIES (not optional)
            DisplayProperties(ObjToSend, oType, DocumentationNav, flags);

            // Dependency properties
            DisplayDependencyProperties(ObjToSend);

            // METHODS and CONSTRUCTORS
            if ((flags & TraceDisplayFlags.ShowMethods) != 0)
            {
               DisplayConstructors(ObjToSend, oType, DocumentationNav, flags);
               DisplayMethods(ObjToSend, oType, DocumentationNav, flags);
            }

            // EVENTS
            if ((flags & TraceDisplayFlags.ShowEvents) != 0)
               DisplayEvents(ObjToSend, oType, DocumentationNav, flags);

            // BASES CLASSES and SUB CLASSES
            if ((flags & TraceDisplayFlags.ShowClassInfo) != 0)
            {
               DisplayBases(oType, flags);
               DisplayNestedTypes(oType, DocumentationNav, flags);
            }
         }
         catch (Exception e)
         {
             TMemberNode memberError = new TMemberNode("Recursive call detected");
             memberError.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
             Members.Add(memberError);
             memberError.Add("AddTypeObject threw an exception of type " + e.GetType());
         }
         finally
         {
            SendObjectRecursiveStatus = 0;
         }

      }

      //----------------------------------------------------------------------
#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)

      /// <summary>
      /// add documentation for a type or a member
      /// </summary>
      ///

      /*
       * Typical member documentation
       * -----------------------------
       *
       * <member name="M:System.Data.InternalDataCollectionBase.CopyTo(System.Array,System.Int32)">
       *    <summary>
       *       <para>Copies all the elements of the current
       *          <see cref="T:System.Data.InternalDataCollectionBase" />
       *          to a one-dimensional <see cref="T:System.Array" />, starting at
       *          the specified <see cref="T:System.Data.InternalDataCollectionBase" /> index.
       *       </para>
       *    </summary>
       *    <param name="ar">The one-dimensional <see cref="T:System.Array" /> to copy the current <see cref="T:System.Data.InternalDataCollectionBase" /> object's elements into.</param>
       *    <param name=" index">The destination <see cref="T:System.Array" /> index to start copying into.</param>
       * </member>

       * Note that summary tag is not mandatory :
       * ----------------------------------------
       *
       * <member name="F:TraceTool.SendMode.WinMsg">
       *     Windows message
       * </member>
       *
       * Sample case of a method with "op_Explicit"
       * ------------------------------------------
       *
       * <member name="M:System.Drawing.SizeF.op_Explicit(System.Drawing.SizeF)~System.Drawing.PointF"> ...
       * <member name="M:System.Decimal.op_Implicit(System.Byte)~System.Decimal"> ...
       * <member name="M:System.Decimal.op_Explicit(System.Decimal)~System.Single"> ...
      */

      // to do : move that function to reflection.cs file and return a string list
      // to do : if mi is not a member of the xml file, open the appropriate one. All XPathNavigator must be stored into an array
      // to do : XML iteration must be changed to use sub tags (like <summary> <para> and <see>)

      internal void AddDocumentation (Object Documentation, TMemberNode Group,Type oType,MemberInfo mi )
      {
          try
          {
              // for compact framework compatibility, the first argument is not passed as an XPathNavigator object
              XPathNavigator DocumentationNav = (XPathNavigator) Documentation ;
              if (DocumentationNav != null)
              {
                  string typeName = oType.FullName.Replace ("+",".") ;

                  string XpathString = "" ;
                  if (mi == null)
                  {
                      XpathString = "//member[@name='T:" + typeName + "']";
                  }
                  else
                  {
                      switch (mi.MemberType)
                      {
                          case MemberTypes.Constructor :
                              XpathString = "//member[@name='M:" + typeName + ".#ctor" +
                                            ReflectionHelper.MethodParamsType2String ((MethodBase) mi) +
                                            "']";
                              break ;
                          case MemberTypes.Method :
                              XpathString = "//member[@name='M:" + typeName + "." + mi.Name +
                                            ReflectionHelper.MethodParamsType2String ((MethodBase) mi) ;

                              if (mi.Name == "op_Implicit" || mi.Name == "op_Explicit")
                                  XpathString += "~" + ((MethodInfo)mi).ReturnType.FullName ;

                              XpathString += "']";
                              break ;
                          case MemberTypes.Property :
                              XpathString = "//member[@name='P:" + typeName + "." + mi.Name +
                                            ReflectionHelper.PropertyParamsType2String ((PropertyInfo) mi) +
                                            "']";
                              break ;
                          case MemberTypes.Field :
                              XpathString = "//member[@name='F:" + typeName + "." + mi.Name + "']";
                              break ;
                          case MemberTypes.Event :
                              XpathString = "//member[@name='E:" + typeName + "." + mi.Name + "']";
                              break ;

                      }
                  }

                  if (XpathString == "")
                      return ;

                  //Group.Add("","",XpathString) ;
                  XPathNodeIterator DocumentationIterator = DocumentationNav.Select (XpathString) ;
                  // XML iteration must be changed to display sub tags (like <summary> <para> and <see>)
                  while (DocumentationIterator.MoveNext() == true)
                  {
                      // get value
                      string DocStr = DocumentationIterator.Current.Value ;
                      string [] split = DocStr.Split(("\n").ToCharArray()) ;
                      foreach (string s in split)
                      {
                          string s2 = s.Trim(("\r ").ToCharArray()) ;
                          if (s2.Length != 0)
                              if (mi == null)
                                  Group.Add("Documentation", s2) ;
                              else // putting Documentation on the column 3 is to far, use space indentation in place.
                                  Group.Add("Documentation", "            " + s2) ;
                      }
                  }
              }

          }
          catch (Exception)
          {
              // ignored
          }
      }
#endif

      //----------------------------------------------------------------------
      /// add the attributes stored in the given parameter to the submembers
      internal void displayCustomAttrib (TMemberNode MemberNode, Object [] attribs)
      {
          try
          {
              if (attribs.Length > 0)
                  foreach (Object obj in attribs)
                  {
                      // putting attribute name on the column 3 is to far, use space indentation in place.
                      MemberNode.Add ("Custom Attrib" , "            " + obj.ToString()) ;
                  }
          }
          catch (Exception e)
          {
              MemberNode.Add("displayCustomAttrib threw an exception of type " + e.GetType()); 
          }
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Call AddValue to fill the "member" tree with the object value.
      /// </summary>
      /// <param name="ObjToSend">Object to display</param>
      public void AddValue(object ObjToSend)
      {
         AddValue (ObjToSend, TTrace.Options.SendPrivate) ;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Call AddValue to fill the "member" tree with the object value.
      /// </summary>
      /// <param name="ObjToSend">Object to display</param>
      /// <param name="SendPrivate">Display private fields</param>
      public void AddValue(object ObjToSend, bool SendPrivate)
      {
         AddValue (ObjToSend, SendPrivate , TTrace.Options.ObjectTreeDepth) ;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Call AddValue to fill the "member" tree with the object value.
      /// </summary>
      /// <param name="ObjToSend">Object to display</param>
      /// <param name="sendPrivate">Display private fields</param>
      /// <param name="maxLevel">Number of sub component to display in tree</param>
      public void AddValue(object ObjToSend, bool sendPrivate, int maxLevel)
      {

         string strModifier = "" ;
         string strName = "" ;
         try
         {
            if (ObjToSend == null)
            {
               strModifier = "" ;
            }
            else
            {
               Type oType = ObjToSend.GetType() ;
               ReflectionHelper.Type2String (oType,ref strModifier, ref strName) ;
               if (strModifier != "")
                  strModifier = strModifier + " " ;
            }
         }
         catch
         {
            // no error
         }
         AddValue (ObjToSend, sendPrivate , maxLevel , strModifier + strName) ;
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Call AddValue to fill the "member" tree with the object value.
      /// </summary>
      /// <param name="ObjToSend">Object to display</param>
      /// <param name="sendPrivate">Display private fields</param>
      /// <param name="maxLevel">Number of sub component to display in tree</param>
      /// <param name="objTitle">Title to display for the object</param>
      public void AddValue(object ObjToSend, bool sendPrivate, int maxLevel, string objTitle)
      {
         AddValue(ObjToSend, sendPrivate, maxLevel, objTitle, new ParsedObjectList());
      }

      //----------------------------------------------------------------------
      // Call AddValue to fill the "member" tree with the object value. Useful for Variant and array
      internal void AddValue(object ObjToSend, bool sendPrivate, int maxLevel, string objTitle, ParsedObjectList alreadyParsedObjects)
      {
         if (Enabled == false)
            return ;

         try
         {
            //string strValue = "" ;
            //strValue = ObjToSend.ToString() ;

            // create the top node using only title.
            // Value (col2) and Type (col3) will be added by inner_addValue
            TMemberNode result = new TMemberNode(objTitle) ; //  strValue
            result.ViewerKind = TraceConst.CST_VIEWER_VALUE;

            // add top node to trace
            Members.Add (result) ;

            // recursive fill members
            inner_addValue (ObjToSend, result, sendPrivate, maxLevel, alreadyParsedObjects) ;
         }
         catch (Exception ex)
         {
             Members.Add("AddValue threw an exception of type " + ex.GetType());
         }
      }

      //----------------------------------------------------------------------

      internal void inner_addValue(Object objTosend, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects)
      {
          try
          {
              if (objTosend == null)
              {
                  upperNode.Col2 = "Null" ;
                  return ;
              }

              Type oType = objTosend.GetType();
              // display the type name in upper node (col 3). Old col3 content is kept
              if (TTrace.Options.SendTypeWithValue == true)
              {
                  string strTypeName = ReflectionHelper.Type2ShortString(oType);
                  upperNode.Col3 = new StringBuilder().Append(upperNode.Col3).Append(strTypeName).ToString();
              }
              // display value in upper node (col2)

              // check primitive and well know type
              // TO DO : use a MAP to store class to ignore
              if (oType.IsPrimitive || oType.IsEnum ||
                  objTosend is string || objTosend is StringBuilder || objTosend is DateTime || objTosend is Decimal)  // 2014/10/23 : added Decimal 
              {
                  upperNode.Col2 = objTosend.ToString() ;
                  return ;
              }

              // check if the object is already parsed
              string hashCode = new StringBuilder ().Append(oType.Name).Append("@").
                  Append(objTosend.GetHashCode().ToString("X2")).ToString() ;

              if (alreadyParsedObjects.ContainsKey(hashCode) || alreadyParsedObjects.Contains(objTosend))  //2014/09/21 : added second test
              {
                  upperNode.Col2 = "see " + hashCode ;
                  return ;
              }

              // by default, display the hash code as the value
              upperNode.Col2 = hashCode  ;

              // max level reached : display the hashCode, since ToString don't tell what object is
              if (maxLevel <= 1)
              {
                  return ;
              }

              // no more display this object content (array or fields)
#if (NETCF1 || NETF1)
         alreadyParsedObjects.Add (hashCode, objTosend) ;
         #else
              alreadyParsedObjects.Add(objTosend);
#endif

              // display IDictionary arrays (like hashTables)
              if (objTosend is IDictionary)
              {
                  addDictionary ((IDictionary)objTosend,upperNode,sendPrivate,maxLevel,alreadyParsedObjects) ;
                  return ;   // don't display fields or values if it's an array.
              }

              // display Array content (special case of IList with multidimensional bounds)
              if (objTosend is Array)
              {
                  addArray ((Array)objTosend,upperNode,sendPrivate,maxLevel,alreadyParsedObjects) ;
                  return ;   // don't display fields or values if it's an array.
              }

              // display IEnumerable arrays. I's maybe also a ICollection that give the count property
              if (objTosend is IEnumerable)
              {
                  addEnumerable ((IEnumerable)objTosend,upperNode,sendPrivate,maxLevel,alreadyParsedObjects) ;
                  return ;   // don't display fields or values if it's an array.
              }

              // display fields
              addAllFieldsValue (objTosend, oType, upperNode,sendPrivate,maxLevel,alreadyParsedObjects) ; 
              addProperties (objTosend, oType, upperNode,sendPrivate,maxLevel,alreadyParsedObjects) ;
              addDependencyPropertiesValues(objTosend,upperNode);
          }
          catch (Exception e)
          {
              upperNode.Add("inner_addValue threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------

      internal void addEnumerable(IEnumerable array, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects)
      {
          try
          {
              // Count is introduce in ICollection interface. IEnumerable has just getEnumerator()
              if (array is ICollection)
                  upperNode.Col3 = new StringBuilder().Append(upperNode.Col3).Append(" (").Append(((ICollection) array).Count).Append(" element(s))").ToString() ;

              int c = 0 ;
              foreach (Object itemObject in array)
              {
                  // display the position in col1 : [n]
                  TMemberNode itemNode = new TMemberNode(new StringBuilder().Append("[").Append(c).Append("]").ToString());
                  upperNode.Add(itemNode) ;
                  // recursive call to display the value in col2 and type in col 3
                  inner_addValue (itemObject, itemNode, sendPrivate,maxLevel-1,alreadyParsedObjects);
                  c++ ;
              }
          }
          catch (Exception e)
          {
              upperNode.Add("addEnumerable threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------

      internal void addDictionary(IDictionary array, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects)
      {
          try
          {
              upperNode.Col3 = new StringBuilder().Append(upperNode.Col3).Append(" (").Append(array.Count).Append(" element(s))").ToString() ;
              foreach (DictionaryEntry itemDic in array)
              {
                  // display the key.ToString() as indice in col1 : [MyKey]
                  TMemberNode itemNode = new TMemberNode(new StringBuilder().Append("[").Append(itemDic.Key).Append("]").ToString());
                  upperNode.Add(itemNode) ;
                  // recursive call to display the value in col2 and type in col 3
                  inner_addValue (itemDic.Value, itemNode, sendPrivate,maxLevel-1,alreadyParsedObjects);
              }
          }
          catch (Exception e)
          {
              upperNode.Add("addDictionary threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------

      internal void addArray(Array array, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects)
      {
          try
          {
              // construct the array Title (col3)
              StringBuilder arrTitle = new StringBuilder() ;
              arrTitle.Append (upperNode.Col3) ;   // get original accessor and type

              // empty array
              if (array.GetLength(0) == 0)
              {
                  arrTitle.Append (" Empty") ;
                  upperNode.Col3 = arrTitle.ToString() ;
                  return ;
              }

              int DimCount = array.Rank ;
              int[] BoundsArray  = new int[DimCount] ;

              arrTitle.Append (" (") ;             // and append bounds

              arrTitle.Append (array.GetLowerBound(0)).Append ("..").Append (array.GetUpperBound(0)) ;
              BoundsArray[0] = array.GetLowerBound(0) ;
              for (int i = 1 ; i < DimCount ; i++)
              {
                  arrTitle.Append(",").Append (array.GetLowerBound(i)).Append ("..").Append (array.GetUpperBound(i)) ;
                  BoundsArray[i] = array.GetLowerBound(i) ;   // set bounds array to low index
              }
              arrTitle.Append (")") ;
              //arrTitle.Append(array.GetType().GetElementType()) ;
              upperNode.Col3 = arrTitle.ToString() ;


              // display the array
              int currentDim = DimCount -1 ;
              while (currentDim >= 0)
              {    // loop 1.  currentDim is changed in Loop2
                  // construct a string with current indexes in col1 : [n][m]...
                  StringBuilder itemTitle = new StringBuilder() ;
                  for (int c = 0 ; c < DimCount ; c++)
                      itemTitle.Append ("[").Append (BoundsArray[c]).Append ("]") ;

                  // create the node with just array indice title
                  TMemberNode itemNode = new TMemberNode(itemTitle.ToString());
                  upperNode.Add(itemNode) ;

                  // get the element value
                  Object Arrelement ;
                  try
                  {
                      Arrelement = array.GetValue (BoundsArray) ;
                  }
                  catch (Exception ex)
                  {
                      Arrelement = ex.GetType().ToString();
                  }
                  // recursive call to display the value in col2 and type in col 3
                  inner_addValue (Arrelement, itemNode, sendPrivate,maxLevel-1,alreadyParsedObjects);

                  currentDim = DimCount -1 ;
                  while (currentDim >= 0)
                  {    // loop 2
                      BoundsArray[currentDim] ++ ;
                      if (BoundsArray[currentDim] > array.GetUpperBound(currentDim))
                      {
                          // out of bound. Reset current dimension and try to increment upper dimension
                          BoundsArray[currentDim] = array.GetLowerBound(currentDim);
                          currentDim-- ;
                          // if the currentDim is less than 0 then loop 1 finish
                      }
                      else
                      {
                          // current dimension is not out of bound, quit loop 2
                          break ;
                      }
                  }  // loop 2 end
              }     // loop 1 end
          }
          catch (Exception e)
          {
              upperNode.Add("addArray threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------


      /// Display all fields (with corresponding value) of the type
      /// Called by AddValue(), not by AddObject()
      internal void addAllFieldsValue(Object ObjToSend, Type oType, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects)
      {
          try
          {
              FieldInfo [] fi = oType.GetFields(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  ) ;

              if (fi.Length <= 0)
                  return ;

              object memberValue ;
              string MemberModifier ;
              string MemberName  ;
              FieldInfo member ;

              for (int iprop = 0 ; iprop < fi.Length ; iprop++)
              {
                  member = fi[iprop] ;

                  if (member.IsPublic == false && sendPrivate == false)
                      continue ;

                  if (member.IsStatic)
                      memberValue = member.GetValue(null) ;
                  else
                      memberValue = member.GetValue(ObjToSend) ;

                  MemberModifier = "" ;

                  // Omit modifier if type is not send
                  if (TTrace.Options.SendTypeWithValue == true)
                  {
                      if (ReflectionHelper.IsDefaultMember(oType, member))
                          MemberModifier = "[default] ";

                      MemberModifier += ReflectionHelper.getFieldModifier(member);  // already contain a space

                      // if the member value is null, add the declared type to MemberModifier
                      if (memberValue == null)
                          MemberModifier += ReflectionHelper.Type2ShortString(member.FieldType);
                  }

                  MemberName = "" ;
                  // add the declaring type if not the actual type
                  if  (member.DeclaringType != member.ReflectedType)
                      MemberName += ReflectionHelper.Type2ShortString(member.DeclaringType) + "::"   ;

                  MemberName += member.Name;

                  TMemberNode FieldNode = new TMemberNode (MemberName ,"" , MemberModifier) ;
                  upperNode.Add (FieldNode) ;

                  inner_addValue (memberValue,FieldNode,sendPrivate,maxLevel-1,alreadyParsedObjects) ;
              }
          }
          catch (Exception e)
          {
              upperNode.Add("addAllFieldsValue threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------
      // caller : inner_addValue
      internal void addDependencyPropertiesValues(Object ObjToSend, TMemberNode upperNode)
      {
          try
          {
#if (NETF3 )   // silverlight don't support MarkupWriter to retreive Dependency Properties 
              MarkupObject markupObject = MarkupWriter.GetMarkupObjectFor(ObjToSend);
              //TTrace.Debug.SendObject("markupObject", markupObject);
              //TTrace.Debug.SendValue("markupObject", markupObject,true);
              if (markupObject != null)
              {
                  foreach (MarkupProperty mp in markupObject.Properties)
                  {
                      TMemberNode FieldNode ;
                      string strTypeName = "" ;
                      if (TTrace.Options.SendTypeWithValue == true)
                      {
                          strTypeName = ReflectionHelper.Type2ShortString(mp.PropertyType);
                          if (mp.IsAttached)
                              strTypeName += " [attached]";
                      } 
                      string mpName ;
                      string mpStringValue ;

                      try {mpName = mp.Name;} catch (Exception){mpName="?";}
                      try {mpStringValue = mp.StringValue;} catch (Exception){mpStringValue="?";}

                      FieldNode = new TMemberNode(mpName, mpStringValue, strTypeName);
                      upperNode.Add(FieldNode);
                  }
              }
#endif
          }
          catch (Exception e)
          {
              upperNode.Add("addDependencyPropertiesValues threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------

      /// Display all properties (with corresponding value, if any) of the type
      /// Called by AddValue(), not by AddObject()
      internal void addProperties(Object ObjToSend, Type oType, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects)
      {
          try
          {
              PropertyInfo field ;
              PropertyInfo [] pi = oType.GetProperties(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  // | BindingFlags.FlattenHierarchy
                  ) ;

              if (pi.Length <= 0)
                  return ;

              int iprop ;
              object oValue ;
              string MemberModifier ;
              string MemberType ;
              string MemberName ;
              Boolean HasPublic ;

              for (iprop = 0 ; iprop < pi.Length ; iprop++)
              {
                  field = pi [iprop] ;

                  //if (sendPrivate == false && (field.DeclaringType != field.ReflectedType))
                  //   continue ;


                  // Try to retrieve instance property value
                  try
                  {
                      oValue = field.GetValue(ObjToSend, null);
                  }
                  catch
                  {
                      // if exception try to retrieve static property value
                      try
                      {
                          oValue = field.GetValue(null, null);
                      }
                      catch (Exception ex)
                      {
                          oValue = ex.Message ;
                      }
                  }

                  // get only properties with "get"
                  if (field.CanRead == false)
                      continue ;

                  MethodInfo getMethod = field.GetGetMethod (true);
                  if (getMethod == null)
                      continue ;

                  // if the get or set method is public, check if the GET method has public access
                  if (getMethod.IsPublic == false && sendPrivate == false)
                      continue ;

                  MemberModifier = "" ;
                  MemberName = "" ;
                  MemberType = "" ;
                  HasPublic = false ;

                  ReflectionHelper.Property2String(field, ref MemberModifier, ref MemberType, ref MemberName, ref HasPublic);

                  // Omit modifier if type is not send
                  if (TTrace.Options.SendTypeWithValue == true)
                  {
                      if (ReflectionHelper.IsDefaultMember(oType, field))
                          MemberModifier = "[default] " + MemberModifier;
                      if (oValue == null)
                          MemberModifier += MemberType;        // add the declared type if null field
                      else if (MemberModifier != "")
                          MemberModifier += " ";
                  } else {
                      MemberModifier = ""; // reset modifier
                  }
                  TMemberNode FieldNode = new TMemberNode (MemberName, "", MemberModifier ) ;
                  upperNode.Add (FieldNode) ;

                  inner_addValue (oValue,FieldNode,sendPrivate,maxLevel-1,alreadyParsedObjects) ;
              }
          }
          catch (Exception e)
          {
              upperNode.Add("addProperties threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------

      /// show the type with its interfaces, and base classes.
      internal void DisplayBases (Type oType, TraceDisplayFlags flags)
      {
          try
          {
              TMemberNode BasesGroup = null ;

              while (oType != null)
              {
                  if (oType != typeof(object) )
                  {
                      string typeName = ReflectionHelper.Type2ShortString(oType) ;
                      string interfacesNames = "" ;

                      // return ALL interfaces, not only interfaces for the current type
                      Type[] typeIntfs = oType.GetInterfaces();

                      foreach (Type intf in typeIntfs)
                      {
                          if (interfacesNames == "")
                              interfacesNames = intf.Name ;
                          else
                              interfacesNames += "," + intf.Name ;
                      }

                      if (BasesGroup == null)
                      {
                          BasesGroup = new TMemberNode("Classes and interfaces").SetFontDetail(0, true);
                          BasesGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                          Members.Add (BasesGroup) ;
                      }

                      BasesGroup.Add(typeName, interfacesNames) ;
                  }
                  oType = oType.BaseType ;
              }
          }
          catch (Exception e)
          {
              Members.Add("DisplayBases threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------
      /// show nested type names (not the content)
      internal void DisplayNestedTypes (Type oType, Object DocumentationNav, TraceDisplayFlags flags)
      {
          try
          {
              Type[] nestedTypes = oType.GetNestedTypes(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  );

              if (nestedTypes.Length <= 0)
                  return ;

              TMemberNode NestedTypeGroup = new TMemberNode("Nested Types").SetFontDetail(0, true);
              NestedTypeGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
              Members.Add(NestedTypeGroup);

              Type subType ;
              for (int iprop = 0 ; iprop < nestedTypes.Length ; iprop++)
              {
                  subType = nestedTypes [iprop] ;

                  // don't check (yet) if the subtype is non public...

                  string strModifier = "" , strName = "" ;
                  ReflectionHelper.Type2String(subType,ref strModifier, ref strName) ;
                  NestedTypeGroup.Add (strModifier, strName ) ;
              }
          }
          catch (Exception e)
          {
              Members.Add("DisplayNestedTypes threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------

      /// Display all fields (with corresponding value) of the type
      /// Called by AddObject(), not by AddValue()
      internal void DisplayFields (Object ObjToSend , Type oType, Object DocumentationNav, TraceDisplayFlags flags)
      {
          try
          {
              FieldInfo [] fi = oType.GetFields(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  ) ;

              if (fi.Length <= 0)
                  return ;

              int iprop ;
              Object [] CustomAttribs  ;
              TMemberNode fieldsGroup = null ;

              object memberValue ;
              string MemberModifier ;
              string MemberName  ;
              FieldInfo member ;

              for (iprop = 0 ; iprop < fi.Length ; iprop++)
              {
                  member = fi[iprop] ;

                  if (((flags & TraceDisplayFlags.ShowInheritedMembers) != TraceDisplayFlags.ShowInheritedMembers)
                      && (member.DeclaringType != member.ReflectedType))
                      continue ;

                  if (member.IsPublic == false && (flags & TraceDisplayFlags.ShowNonPublic) != TraceDisplayFlags.ShowNonPublic)
                      continue ;

                  try
                  {
                      if (member.IsStatic)
                          memberValue = member.GetValue(null) ;
                      else if (ObjToSend != null)
                          memberValue = member.GetValue(ObjToSend) ;
                      else
                          memberValue = "" ;
                  }
                  catch (Exception ex)
                  {
                      memberValue = ex.Message ;
                  }

                  // if the member value is null, convert it to a "null" string
                  if (memberValue == null)
                      memberValue = "null" ;
                  else if (memberValue.ToString() == member.FieldType.FullName)
                      memberValue = "" ;


                  MemberModifier = "" ;
                  MemberName = "" ;
                  if (ReflectionHelper.IsDefaultMember (oType,member))
                      MemberModifier = "[default] " ;

                  ReflectionHelper.Field2String (member,ref MemberModifier, ref MemberName) ;

                  if (fieldsGroup == null)
                  {
                      fieldsGroup = new TMemberNode("Fields").SetFontDetail(0, true);
                      fieldsGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                      Members.Add(fieldsGroup);
                  }

                  TMemberNode MemberNode = new TMemberNode (MemberName , memberValue.ToString(),MemberModifier) ;
                  fieldsGroup.Add (MemberNode) ;

                  // add doc and custom attrib
#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)
                  AddDocumentation (DocumentationNav, MemberNode,oType,member ) ;
#endif

                  if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
                  {
                      CustomAttribs = Attribute.GetCustomAttributes (member , true) ;
                      displayCustomAttrib (MemberNode,CustomAttribs) ;
                  }
              }

          }
          catch (Exception e)
          {
              Members.Add("DisplayFields threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------

      /// caller : AddTypeObject (caller : AddType, AddObject)
      internal void DisplayDependencyProperties(Object ObjToSend)
      {
          try
          {
#if (NETF3)
              if (ObjToSend == null)
                  return;
              MarkupObject markupObject = MarkupWriter.GetMarkupObjectFor(ObjToSend);
              //TTrace.Debug.SendObject("markupObject", markupObject);
              //TTrace.Debug.SendValue("markupObject", markupObject,true);
              if (markupObject != null)
              {
                  TMemberNode PropertiesGroup = null;
                  foreach (MarkupProperty mp in markupObject.Properties)
                  {
                      TMemberNode FieldNode;

                      if (PropertiesGroup == null)
                      {
                          PropertiesGroup = new TMemberNode("Dependency Properties").SetFontDetail(0, true);
                          PropertiesGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                          Members.Add(PropertiesGroup);
                      }

                      string mpName ;
                      string mpStringValue ;
                      string strTypeName = ReflectionHelper.Type2ShortString(mp.PropertyType);

                      try {mpName = mp.Name;} catch (Exception){mpName="?";}
                      try {mpStringValue = mp.StringValue;} catch (Exception){mpStringValue="?";}


                      if (mp.IsAttached)
                          FieldNode = new TMemberNode(mpName, mpStringValue, strTypeName + "[attached]");
                      else
                          FieldNode = new TMemberNode(mpName, mpStringValue, strTypeName);
                      PropertiesGroup.Add(FieldNode);
                  }
              }
#endif
          }
          catch (Exception e)
          {
              Members.Add("DisplayDependencyProperties threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------

      /// Display all properties (with corresponding value, if any) of the type
      /// Called by AddObject(), not by AddValue()
      internal void DisplayProperties (Object ObjToSend ,Type oType, Object DocumentationNav, TraceDisplayFlags flags)
      {
          try
          {
              PropertyInfo member ;
              PropertyInfo [] pi = oType.GetProperties(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  // | BindingFlags.FlattenHierarchy
                  ) ;

              if (pi.Length <= 0)
                  return ;

              int iprop ;
              object [] CustomAttribs  ;
              object oValue ;
              string MemberModifier ;
              string MemberName ;
              string MemberValue ;
              string MemberType ;
              bool   HasPublic ;

              TMemberNode PropertiesGroup = null ;

              for (iprop = 0 ; iprop < pi.Length ; iprop++)
              {
                  member = pi [iprop] ;

                  if (((flags & TraceDisplayFlags.ShowInheritedMembers) != TraceDisplayFlags.ShowInheritedMembers)
                      && (member.DeclaringType != member.ReflectedType))
                      continue ;


                  // Try to retrieve instance property value
                  try
                  {
                      oValue = member.GetValue(ObjToSend, null);
                  }
                  catch
                  {
                      // if exception try to retrieve static property value
                      try   { oValue = member.GetValue(null, null); }
                      catch { oValue = "" ; }
                  }


                  MemberModifier = "" ;
                  MemberName = "" ;
                  MemberType = "" ;
                  HasPublic = false ;

                  if (ReflectionHelper.IsDefaultMember (oType,member))
                      MemberModifier = "[default] " ;
                  ReflectionHelper.Property2String (member,ref MemberModifier, ref MemberType , ref MemberName, ref HasPublic) ;

                  // check if the GET or the SET method has public access
                  if (HasPublic == false && (flags & TraceDisplayFlags.ShowNonPublic) != TraceDisplayFlags.ShowNonPublic)
                      continue ;

                  if (oValue == null)
                      MemberValue = "null" ;
                  else
                      MemberValue = oValue.ToString() ;

                  if (PropertiesGroup == null)
                  {
                      PropertiesGroup = new TMemberNode("Properties").SetFontDetail(0, true);
                      PropertiesGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                      Members.Add(PropertiesGroup);
                  }

                  TMemberNode MemberNode = new TMemberNode ( MemberName, MemberValue ,MemberModifier + MemberType) ;
                  PropertiesGroup.Add (MemberNode) ;
                  // add doc and custom attrib
#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)
                  AddDocumentation (DocumentationNav, MemberNode,oType,member ) ;
#endif
                  if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
                  {
                      CustomAttribs = Attribute.GetCustomAttributes (member , true) ;
                      displayCustomAttrib (MemberNode,CustomAttribs) ;
                  }
              }
          }
          catch (Exception e)
          {
              Members.Add("DisplayProperties threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------

      /// display all Constructors of a type
      internal void DisplayConstructors (Object ObjToSend ,Type oType, Object DocumentationNav, TraceDisplayFlags flags)
      {
          try
          {
              ConstructorInfo [] ci = oType.GetConstructors (
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  ) ;
              // no way to see inherited constructor directly from one type :-(
              if (ci.Length <= 0)
                  return ;

              int iprop ;
              Object [] CustomAttribs  ;
              string MemberModifier ;
              string MemberName ;

              TMemberNode ConstructorGroup = null ;
              for (iprop = 0 ; iprop < ci.Length ; iprop++)
              {

                  MemberModifier = "" ;
                  MemberName = "" ;

                  ConstructorInfo member = ci[iprop] ;

                  if (member.IsPublic == false && (flags & TraceDisplayFlags.ShowNonPublic) != TraceDisplayFlags.ShowNonPublic)
                      continue ;

                  ReflectionHelper.Constructor2String (member,ref MemberModifier, ref MemberName) ;

                  if (ConstructorGroup == null)
                  {
                      ConstructorGroup = new TMemberNode("Constructors").SetFontDetail(0, true);
                      ConstructorGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                      Members.Add(ConstructorGroup);
                  }

                  TMemberNode MemberNode = new TMemberNode (MemberModifier, MemberName) ; // ci[iprop].ToString()) ;
                  ConstructorGroup.Add (MemberNode) ;

                  // add doc and custom attrib
#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)
                  AddDocumentation (DocumentationNav, MemberNode,oType,member ) ;
#endif
                  if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
                  {
                      CustomAttribs = Attribute.GetCustomAttributes (member , true) ;
                      displayCustomAttrib (MemberNode,CustomAttribs) ;
                  }
              }
          }
          catch (Exception e)
          {
              Members.Add("DisplayConstructors threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------

      /// display all Methods of a type
      /// Note that Operators are method but will be displayed in a separate group
      internal void DisplayMethods (Object ObjToSend , Type oType, Object DocumentationNav, TraceDisplayFlags flags)
      {
          try
          {
              MethodInfo [] mi_raw = oType.GetMethods(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  )  ;
              if (mi_raw.Length <= 0)
                  return ;

              int iprop ;
              Object [] CustomAttribs  ;
              string MemberModifier ;
              string MemberName ;

              TMemberNode MethodsGroup = null;
              TMemberNode OperatorGroup = null;
              TMemberNode GroupToUse = null;
              MethodInfo member = null;
         
              // get method list, but discard method used by properties
              for(iprop=0 ; iprop < mi_raw.Length; ++iprop)
              {
                  member = (MethodInfo)mi_raw[iprop];
                  string methname = mi_raw[iprop].Name;

                  if (methname.IndexOf("add_") != 0 &&
                      methname.IndexOf("remove_") != 0 &&
                      methname.IndexOf("get_") != 0 &&
                      methname.IndexOf("set_") != 0 )
                  {

                      if (((flags & TraceDisplayFlags.ShowInheritedMembers) != TraceDisplayFlags.ShowInheritedMembers)
                          && (member.DeclaringType != member.ReflectedType))
                          continue;

                      if (member.IsPublic == false && (flags & TraceDisplayFlags.ShowNonPublic) != TraceDisplayFlags.ShowNonPublic)
                          continue;

                      MemberModifier = "";
                      MemberName = "";

                      if (ReflectionHelper.IsDefaultMember(oType, member))
                          MemberModifier = "[default] ";

                      // to do : Method2String will return true if it's an operator
                      bool IsOperator = ReflectionHelper.Method2String(member, ref MemberModifier, ref MemberName);
                      if (IsOperator)
                      {
                          if (OperatorGroup == null)
                          {
                              OperatorGroup = new TMemberNode("Operators").SetFontDetail(0, true);
                              OperatorGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                              Members.Add(OperatorGroup);
                          }
                          GroupToUse = OperatorGroup;
                      }
                      else
                      {
                          if (MethodsGroup == null)
                          {
                              MethodsGroup = new TMemberNode("Methods").SetFontDetail(0, true);
                              MethodsGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                              Members.Add(MethodsGroup);
                          }
                          GroupToUse = MethodsGroup;
                      }

                      TMemberNode MemberNode = new TMemberNode(MemberModifier, MemberName);  //  mi[iprop].ToString()
                      GroupToUse.Add(MemberNode);

                      // add doc and custom attrib
#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)
                      AddDocumentation (DocumentationNav, MemberNode,oType,member ) ;
#endif
                      if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
                      {
                          CustomAttribs = Attribute.GetCustomAttributes(member, true);
                          displayCustomAttrib(MemberNode, CustomAttribs);
                      }
                  }     // discard method used by properties
              }        // for looop
          }
          catch (Exception e)
          {
              Members.Add("DisplayMethods threw an exception of type " + e.GetType());
          }
      }           // DisplayMethods()

      //----------------------------------------------------------------------

      /// display all Events of a type
      internal void DisplayEvents (Object ObjToSend , Type oType, Object DocumentationNav, TraceDisplayFlags flags)
      {
          try
          {
              int iprop ;
              Object [] CustomAttribs  ;
              EventInfo [] ei = oType.GetEvents(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  ) ;

              if (ei.Length <= 0)
                  return ;

              string MemberModifier ;
              string MemberName ;
              EventInfo member ;

              TMemberNode EventsGroup = null ;

              for (iprop = 0 ; iprop < ei.Length ; iprop++)
              {
                  member = ei[iprop] ;

                  if (((flags & TraceDisplayFlags.ShowInheritedMembers) != TraceDisplayFlags.ShowInheritedMembers)
                      && (member.DeclaringType != member.ReflectedType))
                      continue ;

                  // events are public...
                  //if (member.IsPublic == false && (flags & TraceDisplayFlags.ShowNonPublic) != TraceDisplayFlags.ShowNonPublic)
                  //   continue ;

                  MemberModifier = "" ;
                  MemberName = "" ;

                  if (ReflectionHelper.IsDefaultMember (oType,member))
                      MemberModifier = "[default] " ;

                  ReflectionHelper.event2String (member,ref MemberModifier, ref MemberName) ;

                  if (EventsGroup == null)
                  {
                      EventsGroup = new TMemberNode("Events").SetFontDetail(0, true);
                      EventsGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                      Members.Add(EventsGroup);
                  }

                  TMemberNode MemberNode = new TMemberNode (MemberModifier, MemberName) ;
                  EventsGroup.Add (MemberNode) ;

                  // add doc and custom attrib
#if (!NETCF1 && !NETCF2 && !NETCF3 && !SILVERLIGHT)
                  AddDocumentation (DocumentationNav, MemberNode,oType,member ) ;
#endif
                  if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
                  {
                      CustomAttribs = Attribute.GetCustomAttributes (member , true) ;
                      displayCustomAttrib (MemberNode,CustomAttribs) ;
                  }
              }
          }
          catch (Exception e)
          {
              Members.Add("DisplayEvents threw an exception of type " + e.GetType());
          }
      }

      //----------------------------------------------------------------------
#if (!NETCF1 && !NETCF2 && !NETCF3)
      /// <summary>
      /// Add the stack frame to the Members.
      /// </summary>
      public void AddStackTrace()
      {
         AddStackTrace(1);    // skip itself
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Add the stack frame to the Members.
      /// </summary>
      /// <param name="level">start level (default 1)</param>
      public void AddStackTrace(int level)
      {
         if (Enabled == false)
            return ;
          
          try
          {
              TMemberNode Group = new TMemberNode("Call stack").SetFontDetail(0, true);
              Group.ViewerKind = TraceConst.CST_VIEWER_STACK;
              Members.Add(Group);

              StackTrace stackTrace = new StackTrace();
              for (int i = level + 1 ; i< stackTrace.FrameCount; i++ )
              {
                  StackFrame sf = stackTrace.GetFrame(i);
                  //MethodInfo OneMethod = (MethodInfo) sf.GetMethod() ;
                  MethodBase OneMethod = sf.GetMethod() ;

                  string MemberModifier = "" ;
                  string MemberName = "" ;
                  string AssemblyName;
                  try
                  {
                      if (OneMethod.DeclaringType == null)   // lambda methods are not in type
                         AssemblyName = "" ;
                      else
                         AssemblyName = OneMethod.DeclaringType.Assembly.GetName().Name;   // Silverlight Assembly.GetName() is [SecurityCritical]
                  }
                  catch (MethodAccessException)
                  {
                      AssemblyName = "Assemby name is [SecurityCritical]";
                  }
                  catch (Exception)
                  {
                      AssemblyName = "???";
                  }

                  // discard methods from this assembly
                  if (OneMethod.DeclaringType == null || OneMethod.DeclaringType.Assembly != this.GetType().Assembly)
                  {
                      if (OneMethod is MethodInfo)
                          ReflectionHelper.Method2String ((MethodInfo)OneMethod,ref MemberModifier, ref MemberName) ;
                      else
                          ReflectionHelper.Constructor2String ((ConstructorInfo)OneMethod,ref MemberModifier, ref MemberName) ;

                      if (OneMethod.DeclaringType != null)
                          MemberName = OneMethod.DeclaringType.FullName + '.' + MemberName ;

                      int lineNumber = sf.GetFileLineNumber() ;
                      if (lineNumber != 0)
                          MemberName = MemberName + " Line " + lineNumber ;

                      TMemberNode MemberNode = new TMemberNode (AssemblyName, MemberName) ;  // don't display MemberModifier. Not needed for stack trace
                      Group.Add (MemberNode);
                  }
              }
          }
          catch (Exception e)
          {
              Members.Add("AddStackTrace threw an exception of type " + e.GetType() );
          }
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Add the caller frame to the Members. Level 0 is self
      /// </summary>
      public void AddCaller()
      {
         AddCaller(1);    // skip itself
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Add the caller stack information. It's like the call stack, but display only 1 line
      /// </summary>
      /// <param name="level">Level 0 is self</param>
      public void AddCaller(int level)
      {
         if (Enabled == false)
            return ;

          try
          {
              TMemberNode Group = new TMemberNode("Caller information").SetFontDetail(0, true);
              Group.ViewerKind = TraceConst.CST_VIEWER_STACK;
              Members.Add(Group);


              StackTrace stackTrace = new StackTrace();

              StackFrame sf = stackTrace.GetFrame(level + 1);
              //MethodInfo OneMethod = (MethodInfo) sf.GetMethod() ;
              MethodBase OneMethod = sf.GetMethod() ;

              string MemberModifier = "" ;
              string MemberName = "" ;

              if (OneMethod is MethodInfo)
                  ReflectionHelper.Method2String ((MethodInfo)OneMethod,ref MemberModifier, ref MemberName) ;
              else
                  ReflectionHelper.Constructor2String ((ConstructorInfo)OneMethod,ref MemberModifier, ref MemberName) ;

              string AssemblyName;
              try
              {
                  AssemblyName = OneMethod.DeclaringType.Assembly.GetName().Name;   // Silverlight Assembly.GetName() is [SecurityCritical]
              }
              catch (MethodAccessException)
              {
                  AssemblyName = "Assemby name is [SecurityCritical]";
              }
            
              TMemberNode MemberNode = new TMemberNode (MemberModifier, MemberName,AssemblyName) ;
              Group.Add (MemberNode);
          }
          catch (Exception e)
          {
              Members.Add("AddCaller threw an exception of type " + e.GetType());
          }
      }

      //------------------------------------------------------------------------------

#if (!SILVERLIGHT)
      /// <summary>
      /// Add a bitmap
      /// </summary>
      /// <param name="image">The Image</param>
      public void AddBitmap(System.Drawing.Image image)
      {
         if (Enabled == false)
            return;

          try
          {
              // 1) put the image into a stream
              MemoryStream imgStream = new MemoryStream();
              image.Save(imgStream, ImageFormat.Bmp);
              imgStream.Position = 0;

              // 2) create a byte array from the stream
              int sourceLength = (int)imgStream.Length;
              byte[] SourceData = new byte[sourceLength];
              imgStream.Read(SourceData, 0, sourceLength);
              imgStream.Close();

              // 3) encode (base 64) source array into another array
              char[] base64data = new char[(int)(Math.Ceiling((double)sourceLength / 3) * 4)];
              Convert.ToBase64CharArray(SourceData, 0, sourceLength, base64data, 0);

              // 4) attach the encoded array to a new member. the member vierwer kind specify a bitmap viewer
              TMemberNode member = Members.Add(new String(base64data));
              member.ViewerKind = TraceConst.CST_VIEWER_BITMAP;
          }
          catch (Exception e)
          {
              Members.Add("AddBitmap threw an exception of type " + e.GetType());
          }

      }
#endif  // SILVERLIGHT

      //------------------------------------------------------------------------------

#endif  // (!NETCF1 && !NETCF2 && !NETCF3)

// currently not possibe in silverlight 2 : 
// - No way to read the Image content
// - BmpBitmapEncoder is not supported
#if (NETF3)  
      /// <summary>
      /// Add a bitmap
      /// </summary>
      /// <param name="image">The Image</param>
      public void AddBitmap(System.Windows.Controls.Image image)
      {
         if (Enabled == false)
            return;

          try
          {
              BmpBitmapEncoder encoder = new BmpBitmapEncoder();
              BitmapFrame bitmapFrame = BitmapFrame.Create(image.Source as BitmapSource);
              encoder.Frames.Add(bitmapFrame);

              // 1) put the image into a stream
              MemoryStream imgStream = new MemoryStream();

              encoder.Save(imgStream);

              ////////image.Save(imgStream, System.Drawing.Imaging.ImageFormat.Bmp);
              imgStream.Position = 0;

              // 2) create a byte array from the stream
              int sourceLength = (int)imgStream.Length;
              byte[] SourceData = new byte[sourceLength];
              imgStream.Read(SourceData, 0, sourceLength);
              imgStream.Close();

              // 3) encode (base 64) source array into another array
              char[] base64data = new char[(int)(Math.Ceiling((double)sourceLength / 3) * 4)];
              Convert.ToBase64CharArray(SourceData, 0, sourceLength, base64data, 0);

              // 4) attach the encoded array to a new member. the member vierwer kind specify a bitmap viewer
              TMemberNode member = Members.Add(new String(base64data));
              member.ViewerKind = TraceConst.CST_VIEWER_BITMAP;
          }
          catch (Exception e)
          {
              Members.Add("AddBitmap threw an exception of type " + e.GetType());
          }

      }
#endif



      /// <summary>
      /// Add byte dump to the Members
      /// </summary>
      /// <param name="shortTitle">Tite to display in the first col</param>
      /// <param name="bytes">Pointer to the buffer to dump</param>
      /// <param name="count">Number of bytes to dump</param>
      public void AddDump(string shortTitle, byte[] bytes, int count)
      {
         if (Enabled == false)
            return;
         AddDump(shortTitle, bytes, 0, count);                     // add info to this.Members
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Add byte dump to the Members
      /// </summary>
      /// <param name="shortTitle">Tite to display in the first col</param>
      /// <param name="bytes">Pointer to the buffer to dump</param>
      /// <param name="index">start offset</param>
      /// <param name="count">Number of byte to dump</param>
      public void AddDump(string shortTitle, byte[] bytes, int index, int count)
      {
         if (Enabled == false)
            return ;

          try
          {
              int c = index ;
              int byteDumped = 0 ;

              TMemberNode DumpGroup = new TMemberNode(shortTitle).SetFontDetail(0, true);
              DumpGroup.ViewerKind = TraceConst.CST_VIEWER_DUMP;
              Members.Add(DumpGroup);

              while (byteDumped < count && (c < bytes.Length))
              {
                  int d = 0 ;           // inner loop. From 0 to 15 max
                  int beginLine = c ;   // used to print the offset
                  StringBuilder hexa_representation = new StringBuilder();
                  //StringBuilder Str_representation = new StringBuilder();

                  while ((byteDumped < count) && (d < 16) && (c < bytes.Length))
                  {
                      byte OneByte = bytes [c] ;
                      hexa_representation.Append(((Int32)OneByte).ToString("X2")).Append(" ") ;

                      // only the zero cannot be copied to the stream
                      //if (OneByte == 0)
                      //   Str_representation.Append('.') ;
                      //else
                      //   Str_representation.Append((char) OneByte) ;

                      byteDumped++ ;
                      d++ ;
                      c++ ;
                  }
                  DumpGroup.Add(beginLine.ToString("X6"), hexa_representation.ToString()) ; // , Str_representation.ToString());
                  //.SetFontDetail(1,false,false,-1,0,"Lucida console") ;
              }
              DumpGroup.Col2 = byteDumped.ToString() + " byte(s) dumped" ;
          }
          catch (Exception e)
          {
              Members.Add("AddDump threw an exception of type " + e.GetType());
          }
      }


      //------------------------------------------------------------------------------

      /// <summary>
      /// Add xml text
      /// </summary>
      /// <param name="xml">xml text to send</param>
      public void AddXML(string xml)
      {
         if (Enabled == false)
            return;

         TMemberNode member = Members.Add(xml);
         member.ViewerKind = TraceConst.CST_VIEWER_XML;
      }


      //------------------------------------------------------------------------------

      /// <summary>
      /// Add table to node
      /// </summary>
      /// <param name="table">table to send</param>
      public void AddTable(TraceTable table)
      {
         if (Enabled == false)
            return;

          try
          {
              table.CopyToNodeMembers(Members); // copy member to node. Member viewer kind is already set
          }
          catch (Exception e)
          {
              Members.Add("AddTable threw an exception of type " + e.GetType());
          }
      }

      //------------------------------------------------------------------------------

      internal bool inner_AddTable(TMemberNode TableMembers, Object itemObject, bool isFirstRow, string FirstcolValue)
      {
         FieldInfo member;
         object memberValue;
         string strMemberValue;
         bool isFirstCol = true ; 

         TMemberNode fCurrentRow = TableMembers.Add("");
         Type oType = itemObject.GetType();

         // set first col if gived. First col title is set by caller.
         if (FirstcolValue != null)
            fCurrentRow.Col1 = FirstcolValue;

         // special case for Primitive object
         if (oType.IsPrimitive || oType.IsEnum || itemObject is string || itemObject is StringBuilder || itemObject is DateTime)
         {
            // Add Column Title if first line
            if (isFirstRow)
               if (TableMembers.Col1 == "")
                  TableMembers.Col1 = "Values";
               else 
                  TableMembers.Col1 += "\tValues";


            //if (isFirstCol) // bug fix 12.3
               fCurrentRow.Col1 = itemObject.ToString();
            //else 
            //   fCurrentRow.Col1 += "\t" + itemObject.ToString();
            //isFirstCol = false ;
            return false;
         }

         //add Fields Value
         //----------------
         FieldInfo[] fi = itemObject.GetType().GetFields(BindingFlags.Public | BindingFlags.Static | BindingFlags.Instance);

         for (int iprop = 0; iprop < fi.Length; iprop++)
         {
            member = fi[iprop];
            // Add Column Title if first line
            if (isFirstRow)
            {
               if (TableMembers.Col1 == "")
                  TableMembers.Col1 = member.Name;
               else
                  TableMembers.Col1 = TableMembers.Col1 + "\t" + member.Name;
            }
            // add data
            memberValue = member.GetValue(itemObject);
            if (memberValue == null)
               strMemberValue = "null";
            else
               strMemberValue = memberValue.ToString();

            if (isFirstCol) // bug fix 12.3
               fCurrentRow.Col1 = strMemberValue;
            else
               fCurrentRow.Col1 = fCurrentRow.Col1 + "\t" + strMemberValue;
            isFirstCol = false ;
         }
         // add Properties
         //-----------------
         PropertyInfo field;
         PropertyInfo[] pi = itemObject.GetType().GetProperties(
            BindingFlags.Public | BindingFlags.Static | BindingFlags.Instance
            // | BindingFlags.FlattenHierarchy
            );
         for (int iprop = 0; iprop < pi.Length; iprop++)
         {
            field = pi[iprop];

            // Add Column Title if first line
            if (isFirstRow)
            {
               if (TableMembers.Col1 == "")
                  TableMembers.Col1 = field.Name;
               else
                  TableMembers.Col1 = TableMembers.Col1 + "\t" + field.Name;
            }

            try
            {
               memberValue = field.GetValue(itemObject, null);
            }
            catch
            {
               // if exception try to retrieve static property value
               try
               {
                  memberValue = field.GetValue(null, null);
               }
               catch (Exception ex)
               {
                  memberValue = ex.Message;
               }
            }
            if (memberValue == null)
               strMemberValue = "null";
            else
               strMemberValue = memberValue.ToString();

            if (isFirstCol) // bug fix 12.3
               fCurrentRow.Col1 = strMemberValue;
            else
               fCurrentRow.Col1 = fCurrentRow.Col1 + "\t" + strMemberValue;
            isFirstCol = false ;
         }

         // add Dependency Properties
         //--------------------------
         bool HasDependencyproperties = false;

#if (NETF3)
         MarkupObject markupObject = MarkupWriter.GetMarkupObjectFor(itemObject);
         if (markupObject != null)
         {
            string allProperties = "";
            foreach (MarkupProperty mp in markupObject.Properties)
            {
               string NameAndValue = mp.Name + "=" + mp.StringValue;
               if (allProperties == "")
                  allProperties = NameAndValue;
               else
                  allProperties += ", " + NameAndValue;
               HasDependencyproperties = true;
            }
            if (isFirstCol) // bug fix 12.3
               fCurrentRow.Col1 = allProperties;
            else
               fCurrentRow.Col1 = fCurrentRow.Col1 + "\t" + allProperties;
            isFirstCol = false ;
         }
#endif
         return HasDependencyproperties;
      }

      //------------------------------------------------------------------------------

      /// <summary>
      /// Add table to node
      /// </summary>
      /// <param name="list">Object table to send. Must be an Array or IEnumerable or IDictionary</param>
      public void AddTable(Object list)
      {
          try
          {
              // create table
              TMemberNode TableMembers;
              TableMembers = Members.Add("");
              TableMembers.ViewerKind = TraceConst.CST_VIEWER_TABLE;

              // fill table
              bool isFirst = true;
              bool hasDependencyProperties = false ;

              // check for specialised object before using the IEnumerable
              if (list is Array) {
                  // Special case for Array : display the index on first column
                  // Array implement : IList, ICollection, IEnumerable
                  int c = 0;
                  TableMembers.Col1 = "Index"; // set first col title
                  foreach (Object itemObject in (Array)list)
                  {
                      if (inner_AddTable(TableMembers, itemObject, isFirst, "[" + c + "]") == true)
                          hasDependencyProperties = true;

                      isFirst = false;
                      c++;
                  }
              } else if (list is IDictionary) {
                  // Special case for IDictionary : display the Key on first column
                  // IDictionary must be check before IEnumerable because IDictionary inherit from IEnumerable
                  TableMembers.Col1 = "Key" ; // set first col title
                  foreach (DictionaryEntry itemDic in (IDictionary)list)
                  {
                      if (inner_AddTable(TableMembers, itemDic.Value, isFirst, "[" + itemDic.Key + "]") == true)  // key cannot be null
                          hasDependencyProperties = true;
                      isFirst = false;
                  }
              } else if (list is IEnumerable) {
                  // IEnumerable is the base classe for ICollection ,IList, IDictionary 
                  // Error may occur here if your LINQ query contains errors
                  foreach (Object itemObject in (IEnumerable)list)
                  {
                      if(inner_AddTable(TableMembers, itemObject, isFirst, null) == true) // no first column
                          hasDependencyProperties = true;
                      isFirst = false;
                  }
           
              } else {
                  // not a collection : print single object  
                  if (inner_AddTable(TableMembers, list, isFirst, null) == true) // no first column
                      hasDependencyProperties = true;
              }
              if (hasDependencyProperties == true)
                  TableMembers.Col1 += "\t" + "Dependencies"; // set first col title
          }
          catch (Exception e)
          {
              Members.Add("AddTable threw an exception of type " + e.GetType());
          }
      }

      //------------------------------------------------------------------------------
      /* The code is not necessary, because the "AddTable(Object table)" perform the same job : IEnumerable<T> inherit from IEnumerable
      #if (!NETCF1 && !NETF1)
      /// <summary>
      /// Add table to node
      /// </summary>
      /// <param name="list">Generic IEnumerable table to send. </param>
      public void AddTable<T>(IEnumerable<T> list) //where T : 
      {
         // create table
         TMemberNode TableMembers;
         TableMembers = Members.Add("");
         TableMembers.ViewerKind = TraceConst.CST_VIEWER_TABLE;

         bool isFirst = true;
         foreach (T itemObject in list)
         {
            inner_AddTable(TableMembers, itemObject, isFirst, null);
            isFirst = false;
         }
      }
      #endif
      */

      //----------------------------------------------------------------------

      /// <summary>
      /// Change background font color
      /// </summary>
      /// <param name="color">RGB background color (see Color.ToArgb function)</param>
      public void AddBackgroundColor(int color)
      {
         if (Enabled == false)
            return ;
         AddBackgroundColor(color,-1);
      }

      //------------------------------------------------------------------------------
      /// <summary>
      /// Change background font color
      /// </summary>
      /// <param name="color">RGB background color (see Color.ToArgb function)</param>
      /// <param name="colId">Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      public void AddBackgroundColor(int color, int colId)
      {
         if (Enabled == false)
            return ;
         FontDetail fontDetail = new FontDetail();
         fontDetail.ColId = colId;
         fontDetail.Color = color;
         fontDetail.FontName = "BackgroundColor";  // special name. Indicate that color is for background, not font itsef
         FontDetails.Add(fontDetail);
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="colId">Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <returns>The TraceNodeEx</returns>
      public TraceNodeEx AddFontDetail(int colId, bool bold)
      {
         return AddFontDetail(colId, bold, false, -1, 0, "");
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="colId">Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <param name="italic">Change font to italic</param>
      /// <returns>The TraceNodeEx</returns>
      public TraceNodeEx AddFontDetail(int colId, bool bold, bool italic)
      {
         return AddFontDetail(colId, bold, italic, -1, 0, "");
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="colId">Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <param name="italic">Change font to italic</param>
      /// <param name="color">RGB color (see Color.ToArgb function). Use -1 to keep default color</param>
      /// <returns>The TraceNodeEx</returns>
      public TraceNodeEx AddFontDetail(int colId, bool bold, bool italic, int color)
      {
         return AddFontDetail(colId, bold, italic, color, 0, "");
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="colId">Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <param name="italic">Change font to italic</param>
      /// <param name="color">RGB color (see Color.ToArgb function). Use -1 to keep default color</param>
      /// <param name="size">Change font size, use zero to keep normal size</param>
      /// <returns>The TraceNodeEx</returns>
      public TraceNodeEx AddFontDetail(int colId, bool bold, bool italic, int color, int size)
      {
         return AddFontDetail(colId, bold, italic, color, size, "");
      }

      //----------------------------------------------------------------------
      /// <summary>
      /// Change font detail for an item in the trace
      /// </summary>
      /// <param name="colId">Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column</param>
      /// <param name="bold">Change font to bold</param>
      /// <param name="italic">Change font to italic</param>
      /// <param name="color">RGB color (see Color.ToArgb function). Use -1 to keep default color</param>
      /// <param name="size">Change font size, use zero to keep normal size</param>
      /// <param name="fontName">Change font name</param>
      /// <returns>The TraceNodeEx</returns>
      public TraceNodeEx AddFontDetail(int colId, bool bold, bool italic, int color, int size, string fontName)
      {
         if (Enabled == false)
            return this ;

         FontDetail fontDetail = new FontDetail();
         fontDetail.ColId = colId;
         fontDetail.Bold = bold;
         fontDetail.Italic = italic;
         fontDetail.Color = color;         // color is stored in ARGB. Converted to BGR before sending the trace
         fontDetail.Size = size;
         fontDetail.FontName = fontName;

         if (FontDetails == null)
            FontDetails = new FontDetailList();

         FontDetails.Add(fontDetail);
         return this;
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Send the trace to the server (left + right + members)
      /// </summary>
      /// <returns>a TraceNode for furthers changes</returns>
      public TraceNode Send()
      {
         TraceNode result = new TraceNode (this) ; // create a copy
         if (Enabled == false)
            return result;

         StringList CommandList = new StringList();

         Helper.addCommand (CommandList, TraceConst.CST_NEW_NODE  , ParentNodeId) ; // param : parent Node id
         Helper.addCommand(CommandList, TraceConst.CST_TRACE_ID, Id);               // param : guid
         if (LeftMsg != null)
            Helper.addCommand(CommandList, TraceConst.CST_LEFT_MSG, LeftMsg);       // param : left string
         if (RightMsg != null)
            Helper.addCommand(CommandList, TraceConst.CST_RIGHT_MSG, RightMsg);     // param : right string
         Helper.addCommand(CommandList, TraceConst.CST_ICO_INDEX, IconIndex);       // param : the icon index

         // add font detail
         if (FontDetails != null)
         {
            foreach (FontDetail fontDetail in FontDetails)
            {
               int colorValue ;

               if (fontDetail.Color == -1)
                  colorValue = -1 ;
               else
                  colorValue = Helper.ARGB_to_BGR(fontDetail.Color) ;

               StringBuilder TempStr = new StringBuilder();

               if (fontDetail.FontName == "BackgroundColor")
               {
                  //special color : background color
                  Helper.addCommand(CommandList, TraceConst.CST_BACKGROUND_COLOR, colorValue, fontDetail.ColId.ToString());      // param : color, colId

               }
               else
               {
                  TempStr.Append(String.Format("{0,5}{1,3}", TraceConst.CST_FONT_DETAIL, fontDetail.ColId));

                  if (fontDetail.Bold)
                     TempStr.Append("1");
                  else
                     TempStr.Append("0");

                  if (fontDetail.Italic)
                     TempStr.Append("1");
                  else
                     TempStr.Append("0");
                  TempStr.Append(String.Format("{0,11}{1,11}", colorValue, fontDetail.Size)).Append(fontDetail.FontName);
                  CommandList.Add(TempStr.ToString());
               }
            }
            FontDetails.Clear();
            FontDetails = null;  // once copied to Commandlist, clear the array
         }

         Members.AddToStringList (CommandList) ;   // convert all groups and nested items/group to strings

         TTrace.SendToWinTraceClient(CommandList,this.WinTraceId, this.Time,this.ThreadName);
         return result;
      }

      //----------------------------------------------------------------------

      /// <summary>
      /// Resend the trace to the server (only left and right message)
      /// </summary>
      public void Resend ()
      {
         if (Enabled == false)
            return ;

         StringList CommandList = new StringList();

         Helper.addCommand(CommandList, TraceConst.CST_USE_NODE, Id);           // param : guid
         Helper.addCommand(CommandList, TraceConst.CST_LEFT_MSG, LeftMsg);       // param : left string
         Helper.addCommand(CommandList, TraceConst.CST_RIGHT_MSG, RightMsg);      // param : right string

         // don't resend members and icon
         TTrace.SendToWinTraceClient (CommandList, WinTraceId);
      }  // Resend
   }     // TraceNodeEx
}        // namespace TraceTool
