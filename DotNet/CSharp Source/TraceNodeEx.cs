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
using System;
using System.Text;
using System.Reflection;
using System.Diagnostics;  // Process
using System.Collections;  // ArrayList, queue

#if NETFULL
using System.Drawing.Imaging;
using System.IO;           // streams
using System.Windows.Markup.Primitives;   // dependency properties.  Assembly : PresentationFramework
using System.Windows.Media.Imaging;
#endif

using System.Xml.XPath;
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
    /// <summary> Alternate way to send traces : prepare a TraceNode with all properties then send it.
    /// </summary>
    public class TraceNodeEx : TraceNodeBase
    {
        /// <summary>
        /// The Id of the parent node
        /// </summary>
        public string ParentNodeId;
        /// <summary>
        /// The left part of the tree message
        /// </summary>
        public string LeftMsg = null;
        /// <summary>
        /// The right part of the tree message
        /// </summary>
        public string RightMsg = null;
        /// <summary>
        /// time
        /// </summary>
        public string Time = null;
        /// <summary>
        /// thread name
        /// </summary>
        public string ThreadName = null;

        /// <summary>
        /// the root for the Member tree
        /// </summary>
        public TMemberNode Members = new TMemberNode();

        internal List<FontDetail> FontDetails;

        //----------------------------------------------------------------------
        /// <summary>
        /// create a Node with an unique ID
        /// </summary>

        public TraceNodeEx()
        {
            Id = Helper.NewGuid().ToString();
            IconIndex = TraceConst.CST_ICO_DEFAULT;
            Enabled = true;
            WinTraceId = null;
            ParentNodeId = "";
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// create a Node with an unique ID (true)
        /// </summary>
        /// <param name="parentNode">The parent node where to place that trace.
        /// The IconIndex and the enabled properties are also recopied
        /// Can be null : the root tree become the parent node, enabled is true and the default icon is used
        /// </param>

        public TraceNodeEx(TraceToSend parentNode)
        {
            Id = Helper.NewGuid().ToString();

            if (parentNode == null)
            {
                IconIndex = TraceConst.CST_ICO_DEFAULT;
                Enabled = true;
                WinTraceId = null;
                ParentNodeId = "";
            }
            else
            {
                IconIndex = parentNode.IconIndex;
                Enabled = parentNode.Enabled;
                WinTraceId = parentNode.WinTraceId;
                ParentNodeId = parentNode.GetLastContextId();
            }
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Create a Node.
        /// </summary>
        /// <param name="parentNode">The parent node where to place that trace.
        /// The IconIndex and the enabled properties are also recopied
        /// Can be null : the root tree become the parent node, enabled is true and the default icon is used
        /// </param>
        /// <param name="generateUniqueId">if true, the id is generated automatically, else set the empty string
        /// </param>

        public TraceNodeEx(TraceToSend parentNode, bool generateUniqueId)
        {
            if (generateUniqueId)
                Id = Helper.NewGuid().ToString();

            if (parentNode == null)
            {
                IconIndex = TraceConst.CST_ICO_DEFAULT;
                Enabled = true;
                WinTraceId = null;
                ParentNodeId = "";
            }
            else
            {
                IconIndex = parentNode.IconIndex;
                Enabled = parentNode.Enabled;
                WinTraceId = parentNode.WinTraceId;
                ParentNodeId = parentNode.Id;
            }
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Call AddObject to fill the "member" tree with the object description
        /// </summary>
        /// <param name="objToSend"></param>
        public void AddObject(object objToSend)
        {
            AddObject(objToSend, TTrace.Options.GetDefault());
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Call AddObject to fill the "member" tree with the object description
        /// </summary>
        /// <param name="objToSend">Object to send</param>
        /// <param name="flags">determine what information to send</param>
        public void AddObject(object objToSend, TraceDisplayFlags flags)
        {
            if (Enabled == false)
                return;
            var oType = objToSend?.GetType();
            AddTypeObject(objToSend, oType, flags);        // add info to this.Members
        }

        //------------------------------------------------------------------------------
        /// <summary>
        /// Call AddType to fill the "member" tree with the object type
        /// </summary>
        /// <param name="typeToSend">Object type to send</param>
        public void AddType(Type typeToSend)
        {
            AddType(typeToSend, TTrace.Options.GetDefault());
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Call AddType to fill the "member" tree with the object type
        /// </summary>
        /// <param name="typeToSend">Object type to send</param>
        /// <param name="flags">determine what information to send</param>
        public void AddType(Type typeToSend, TraceDisplayFlags flags)
        {
            if (Enabled == false)
                return;
            AddTypeObject(null, typeToSend, flags);        // add info to this.Members
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// SendObjectRecursiveStatus is used to block recursive call to AddTypeObject
        /// since AddTypeObject will try to evaluate field GET method.
        /// If the GET method call AddObject or similar function,
        /// we have a possible recursive call (with stack overflow).
        /// </summary>
        [ThreadStatic] internal static Int32 SendObjectRecursiveStatus;

        /// fill the Members member with a type description and optional values of that type
        /// caller : AddType, AddObject
        internal void AddTypeObject(Object objToSend, Type oType, TraceDisplayFlags flags)
        {
            if (Enabled == false)
                return;

            // detect null type
            if (oType == null)
            {
                Members.Add("Null Type");
                return;
            }

            string strModifier = "";
            string strName = "";

            ReflectionHelper.Type2String(oType, ref strModifier, ref strName);

            if ((flags & TraceDisplayFlags.ShowModifiers) != 0)
            {
                if (strModifier != "")
                    strModifier += " ";
            }
            else
            {
                strModifier = "";
            }

            // only 2 parts are not optional : quick info and properties

            // quick info on the root (not optional)
            // --------------------------------------
            TMemberNode memberClass;
            if (objToSend != null)
            {
                string strValue;
                try
                {
                    strValue = objToSend.ToString();
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
                TMemberNode memberError = new TMemberNode("Recursive call detected")
                {
                    ViewerKind = TraceConst.CST_VIEWER_OBJECT
                };
                Members.Add(memberError);
                memberError.Add("Error : A Field Read method has called SendObject/AddObject.");
                memberError.Add("->Possible recursive call is stopped now");
                SendObjectRecursiveStatus = 2; // tell the calling SendObject that a recursive call was stopped
                return;
            }
            // reset recursive call flag
            SendObjectRecursiveStatus = 1;

            //XPathNavigator DocumentationNav = null;
            Object documentationNav = null;

            string xmlDocFileName = ReflectionHelper.AssemblyDocumentationFileName(oType);

            if ((flags & TraceDisplayFlags.ShowDoc) != 0)
            {
                // exception can occur here if XML file is incorrect.
                try
                {
                    var xmlDoc = new XPathDocument(xmlDocFileName);
                    documentationNav = xmlDoc.CreateNavigator();
                }
                catch
                {
                    documentationNav = null;
                }
            }

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
                    string str;
                    if (objToSend != null)
                    {
                        try
                        {
                            str = objToSend.ToString();
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

#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsCOMObject)
#else
                    if (oType.IsCOMObject)
#endif
                        classGroup.Add("IsCOMObject");

#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsImport)
#else
                    if (oType.IsImport)
#endif
                        classGroup.Add("IsImport");

#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsNotPublic)
#else
                    if (oType.IsNotPublic)
#endif
                        classGroup.Add("IsNotPublic");

#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsSpecialName)
#else
                    if (oType.IsSpecialName)
#endif
                        classGroup.Add("IsSpecialName");

#if !NETSTANDARD1_6
                    if (oType.IsContextful)
                        classGroup.Add("IsContextful");
#endif

#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsMarshalByRef)
#else
                    if (oType.IsMarshalByRef)
#endif
                        classGroup.Add("IsMarshalByRef");
#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsSerializable)
#else
                    if (oType.IsSerializable)
#endif
                        classGroup.Add("IsSerializable");
#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsExplicitLayout)// Class layout
#else
                    if (oType.IsExplicitLayout)// Class layout
#endif
                        classGroup.Add("Class layout", "Explicit");
#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsLayoutSequential)// Class layout
#else
                    if (oType.IsLayoutSequential)// Class layout
#endif
                        classGroup.Add("Class layout", "Sequential");

#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsAutoLayout)// Class layout
#else
                    if (oType.IsAutoLayout)// Class layout
#endif
                        classGroup.Add("Class layout", "Auto");

                    // boolean and corresponding value
#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsPrimitive)
                  classGroup.Add("IsPrimitive", oType.GetTypeInfo().UnderlyingSystemType.ToString());
#else
                    if (oType.IsPrimitive)
                        classGroup.Add("IsPrimitive", oType.UnderlyingSystemType.ToString());
#endif

                    //if (oType.HasElementType)
                    //   ClassGroup.Add("HasElementType" , oType.GetElementType().ToString());

                    // Array .See Type2ShortString for array
                    if (oType.IsArray)
                    {
                        StringBuilder result = new StringBuilder();
                        Array arr = (Array)objToSend;
                        if (arr != null)
                        {
                            result.Append("[");
                            result.Append(arr.GetLowerBound(0)).Append("..").Append(arr.GetUpperBound(0));
                            for (int i = 1; i < oType.GetArrayRank(); i++)
                                result.Append(",").Append(arr.GetLowerBound(i)).Append("..").Append(arr.GetUpperBound(i));
                            result.Append("]");
                        }
                        var elementType = oType.GetElementType();
                        if (elementType != null)
                            classGroup.Add("IsArray", elementType.ToString(), result.ToString());
                    }

                    // String Format
#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsAnsiClass)
#else
                    if (oType.IsAnsiClass)
#endif
                        classGroup.Add("String Format", "Ansi");

#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsAutoClass)
#else
                    if (oType.IsAutoClass)
#endif
                        classGroup.Add("String Format", "Auto");

#if NETSTANDARD1_6
               if (oType.GetTypeInfo().IsUnicodeClass)
#else
                    if (oType.IsUnicodeClass)
#endif
                        classGroup.Add("String Format", "Unicode");

                    // show all other infos giving strings
#if NETSTANDARD1_6
                classGroup.Add("GUID", oType.GetTypeInfo().GUID.ToString());
                if (oType.GetTypeInfo().TypeInitializer != null)
                    classGroup.Add("TypeInitializer", oType.GetTypeInfo().TypeInitializer.ToString());
#else
                    classGroup.Add("GUID", oType.GUID.ToString());
                    if (oType.TypeInitializer != null)
                        classGroup.Add("TypeInitializer", oType.TypeInitializer.ToString());

#endif
                    if (oType.Namespace != null)
                        classGroup.Add("Namespace", oType.Namespace);
                    classGroup.Add("TypeHandle", oType.TypeHandle.ToString());

#if !NETSTANDARD1_6
                    if (oType.ReflectedType != null)
                        classGroup.Add("ReflectedType", oType.ReflectedType.ToString());
#endif

                    if (objToSend != null)
                        classGroup.Add("OBJ.HashCode", objToSend.GetHashCode().ToString("X2"));
                    classGroup.Add("Type.HashCode", oType.GetHashCode().ToString("X2"));

                    TypeCode myTypeCode = Type.GetTypeCode(oType);
                    classGroup.Add("TypeCode", myTypeCode.ToString());

                    // Module, Assembly name, location and XML documentation file name
#if NETSTANDARD1_6
               classGroup.Add("Module", oType.GetTypeInfo().Module.ToString());
#else
                    classGroup.Add("Module", oType.Module.ToString());
#endif
                    try
                    {
#if NETSTANDARD1_6
                   classGroup.Add("Assembly", oType.GetTypeInfo().Assembly.ToString());
#else
                        classGroup.Add("Assembly", oType.Assembly.ToString());
#endif
                    }
                    catch (Exception e)
                    {
                        classGroup.Add("Assembly", e.GetType().ToString());
                    }

                    try
                    {
#if NETSTANDARD1_6
                   classGroup.Add("Assembly location", oType.GetTypeInfo().Assembly.Location);
#else
                        classGroup.Add("Assembly location", oType.Assembly.Location);
#endif
                    }
                    catch (Exception e)
                    {
                        classGroup.Add("Assembly location", e.GetType().ToString());
                    }               // documentation
                    if (xmlDocFileName != "")
                    {
                        classGroup.Add("XML Documentation", xmlDocFileName);
                        if (documentationNav == null)
                            classGroup.Add("XML Documentation", "Unable to open file");
                    }

                    AddDocumentation(documentationNav, classGroup, oType, null);      // null MemberInfo

                    // Custom attributes.
                    if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
                    {
#if NETSTANDARD1_6
                  //IEnumerable<Attribute> CusAttribList = oType.GetTypeInfo().GetCustomAttributes(inherit:true) ;
                  //foreach (Attribute attr in CusAttribList)
                  //   classGroup.Add("Custom attribute", attr.ToString());
#else
                        Attribute[] custAttribList = Attribute.GetCustomAttributes(oType, true);  // true : inherit
                        foreach (Attribute attr in custAttribList)
                            classGroup.Add("Custom attribute", attr.ToString());
#endif
                    }
                }

                // Describe members, base type and nested type
                // -------------------------------------------

                // FIELDS
                if ((flags & TraceDisplayFlags.ShowFields) != 0)
                    DisplayFields(objToSend, oType, documentationNav, flags);

                // PROPERTIES (not optional)
                DisplayProperties(objToSend, oType, documentationNav, flags);

                // Dependency properties
                DisplayDependencyProperties(objToSend);

                // METHODS and CONSTRUCTORS
                if ((flags & TraceDisplayFlags.ShowMethods) != 0)
                {
                    DisplayConstructors(oType, documentationNav, flags);
                    DisplayMethods(oType, documentationNav, flags);
                }

                // EVENTS
                if ((flags & TraceDisplayFlags.ShowEvents) != 0)
                    DisplayEvents(oType, documentationNav, flags);

                // BASES CLASSES and SUB CLASSES
                if ((flags & TraceDisplayFlags.ShowClassInfo) != 0)
                {
                    DisplayBases(oType);
                    DisplayNestedTypes(oType);
                }
            }
            catch (Exception e)
            {
                TMemberNode memberError = new TMemberNode("Recursive call detected")
                {
                    ViewerKind = TraceConst.CST_VIEWER_OBJECT
                };
                Members.Add(memberError);
                memberError.Add("AddTypeObject threw an exception of type " + e.GetType());
            }
            finally
            {
                SendObjectRecursiveStatus = 0;
            }

        }

        //----------------------------------------------------------------------

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

        internal void AddDocumentation(Object documentation, TMemberNode group, Type oType, MemberInfo mi)
        {
            try
            {
                XPathNavigator documentationNav = (XPathNavigator)documentation;
                if (documentationNav != null)
                {
                    if (oType.FullName == null)
                        return;
                    string typeName = oType.FullName.Replace("+", ".");

                    string xpathString = "";
                    if (mi == null)
                    {
                        xpathString = "//member[@name='T:" + typeName + "']";
                    }
                    else
                    {
                        switch (mi.MemberType)
                        {
                            case MemberTypes.Constructor:
                                xpathString = "//member[@name='M:" + typeName + ".#ctor" +
                                              ReflectionHelper.MethodParamsType2String((MethodBase)mi) +
                                              "']";
                                break;
                            case MemberTypes.Method:
                                xpathString = "//member[@name='M:" + typeName + "." + mi.Name +
                                              ReflectionHelper.MethodParamsType2String((MethodBase)mi);

                                if (mi.Name == "op_Implicit" || mi.Name == "op_Explicit")
                                    xpathString += "~" + ((MethodInfo)mi).ReturnType.FullName;

                                xpathString += "']";
                                break;
                            case MemberTypes.Property:
                                xpathString = "//member[@name='P:" + typeName + "." + mi.Name +
                                              ReflectionHelper.PropertyParamsType2String((PropertyInfo)mi) +
                                              "']";
                                break;
                            case MemberTypes.Field:
                                xpathString = "//member[@name='F:" + typeName + "." + mi.Name + "']";
                                break;
                            case MemberTypes.Event:
                                xpathString = "//member[@name='E:" + typeName + "." + mi.Name + "']";
                                break;
                        }
                    }

                    if (xpathString == "")
                        return;

                    //Group.Add("","",XpathString) ;
                    XPathNodeIterator documentationIterator = documentationNav.Select(xpathString);
                    // XML iteration must be changed to display sub tags (like <summary> <para> and <see>)
                    while (documentationIterator.MoveNext())
                    {
                        // get value
                        if (documentationIterator.Current == null)
                            continue;
                        string docStr = documentationIterator.Current.Value;
                        string[] split = docStr.Split("\n".ToCharArray());
                        foreach (string s in split)
                        {
                            string s2 = s.Trim("\r ".ToCharArray());
                            if (s2.Length != 0)
                                if (mi == null)
                                    group.Add("Documentation", s2);
                                else // putting Documentation on the column 3 is to far, use space indentation in place.
                                    group.Add("Documentation", "            " + s2);
                        }
                    }
                }

            }
            catch (Exception)
            {
                // ignored
            }
        }

        //----------------------------------------------------------------------
        /// add the attributes stored in the given parameter to the sub members
        internal void DisplayCustomAttribute(TMemberNode memberNode, Attribute[] attributeList)
        {
            try
            {
                if (attributeList.Length > 0)
                    foreach (Attribute attribute in attributeList)
                    {
                        // putting attribute name on the column 3 is to far, use space indentation in place.
                        memberNode.Add("Custom Attribute", "            " + attribute);
                    }
            }
            catch (Exception e)
            {
                memberNode.Add("DisplayCustomAttribute threw an exception of type " + e.GetType());
            }
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Call AddValue to fill the "member" tree with the object value.
        /// </summary>
        /// <param name="objToSend">Object to display</param>
        public void AddValue(object objToSend)
        {
            AddValue(objToSend, TTrace.Options.SendPrivate);
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Call AddValue to fill the "member" tree with the object value.
        /// </summary>
        /// <param name="objToSend">Object to display</param>
        /// <param name="sendPrivate">Display private fields</param>
        public void AddValue(object objToSend, bool sendPrivate)
        {
            AddValue(objToSend, sendPrivate, TTrace.Options.ObjectTreeDepth);
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Call AddValue to fill the "member" tree with the object value.
        /// </summary>
        /// <param name="objToSend">Object to display</param>
        /// <param name="sendPrivate">Display private fields</param>
        /// <param name="maxLevel">Number of sub component to display in tree</param>
        public void AddValue(object objToSend, bool sendPrivate, int maxLevel)
        {
            string strModifier = "";
            string strName = "";
            try
            {
                if (objToSend == null)
                {
                    strModifier = "";
                }
                else
                {
                    Type oType = objToSend.GetType();
                    ReflectionHelper.Type2String(oType, ref strModifier, ref strName);
                    if (strModifier != "")
                        strModifier += " ";
                }
            }
            catch
            {
                // no error
            }
            AddValue(objToSend, sendPrivate, maxLevel, strModifier + strName);
        }

        //----------------------------------------------------------------------
        /// <summary>
        /// Call AddValue to fill the "member" tree with the object value.
        /// </summary>
        /// <param name="objToSend">Object to display</param>
        /// <param name="sendPrivate">Display private fields</param>
        /// <param name="maxLevel">Number of sub component to display in tree</param>
        /// <param name="objTitle">Title to display for the object</param>
        public void AddValue(object objToSend, bool sendPrivate, int maxLevel, string objTitle)
        {
            AddValue(objToSend, sendPrivate, maxLevel, objTitle, new ParsedObjectList());
        }

        //----------------------------------------------------------------------
        // Call AddValue to fill the "member" tree with the object value. Useful for Variant and array
        internal void AddValue(object objToSend, bool sendPrivate, int maxLevel, string objTitle, ParsedObjectList alreadyParsedObjects)
        {
            if (Enabled == false)
                return;

            try
            {
                //string strValue = "" ;
                //strValue = ObjToSend.ToString() ;

                // create the top node using only title.
                // Value (col2) and Type (col3) will be added by inner_addValue
                TMemberNode result = new TMemberNode(objTitle)
                {
                    ViewerKind = TraceConst.CST_VIEWER_VALUE
                }; //  strValue

                // add top node to trace
                Members.Add(result);

                // recursive fill members
                Inner_addValue(objToSend, result, sendPrivate, maxLevel, alreadyParsedObjects);
            }
            catch (Exception ex)
            {
                Members.Add("AddValue threw an exception of type " + ex.GetType());
            }
        }

        //----------------------------------------------------------------------

        internal void Inner_addValue(object objToSend, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects)
        {
            try
            {
                if (objToSend == null)
                {
                    upperNode.Col2 = "Null";
                    if (upperNode.DefaultCol2 != null)
                        if (upperNode.DefaultCol2 != upperNode.Col2)
                            upperNode.Add("{dp}", upperNode.DefaultCol2);
                        else
                            upperNode.Col1 += "{dp}";
                    return;
                }

                Type oType = objToSend.GetType();
                // display the type name in upper node (col 3). Old col3 content is kept
                if (TTrace.Options.SendTypeWithValue)
                {
                    string strTypeName = ReflectionHelper.Type2ShortString(oType);
                    upperNode.Col3 = new StringBuilder().Append(upperNode.Col3).Append(strTypeName).ToString();
                }
                // display value in upper node (col2)

                // check primitive and well know type
                // TO DO : use a MAP to store class to ignore
                if (
#if NETSTANDARD1_6
                    oType.GetTypeInfo().IsPrimitive || oType.GetTypeInfo().IsEnum ||
#else
                    oType.IsPrimitive || oType.IsEnum ||
#endif
                    objToSend is string || objToSend is StringBuilder || objToSend is DateTime || objToSend is Decimal)  // 2014/10/23 : added Decimal 
                {
                    upperNode.Col2 = objToSend.ToString();
                    if (upperNode.DefaultCol2 != null)
                        if (upperNode.DefaultCol2 != upperNode.Col2)
                            upperNode.Add("{dp}", upperNode.DefaultCol2);
                        else
                            upperNode.Col1 += "{dp}";
                    return;
                }

                // check if the object is already parsed
                string hashCode = new StringBuilder().Append(oType.Name).Append("@").
                    Append(objToSend.GetHashCode().ToString("X2")).ToString();

                if (alreadyParsedObjects.ContainsKey(hashCode) || alreadyParsedObjects.Contains(objToSend))  //2014/09/21 : added second test
                {
                    upperNode.Col2 = "see " + hashCode;
                    if (upperNode.DefaultCol2 != null)
                        if (upperNode.DefaultCol2 != upperNode.Col2)
                            upperNode.Add("{dp}", upperNode.DefaultCol2);
                        else
                            upperNode.Col1 += "{dp}";
                    return;
                }

                // by default, display the hash code as the value
                upperNode.Col2 = hashCode;
                if (upperNode.DefaultCol2 != null)
                    if (upperNode.DefaultCol2 != upperNode.Col2)
                        upperNode.Add("{dp}", upperNode.DefaultCol2);
                    else
                        upperNode.Col1 += "{dp}";

                // max level reached : display the hashCode, since ToString don't tell what object is
                if (maxLevel <= 1)
                    return;

                // no more display this object content (array or fields)
                alreadyParsedObjects.Add(objToSend);

                // display IDictionary arrays (like hashTables)
                if (objToSend is IDictionary)
                {
                    AddDictionary((IDictionary)objToSend, upperNode, sendPrivate, maxLevel, alreadyParsedObjects);
                    return;   // don't display fields or values if it's an array.
                }

                // display Array content (special case of IList with multidimensional bounds)
                if (objToSend is Array)
                {
                    AddArray((Array)objToSend, upperNode, sendPrivate, maxLevel, alreadyParsedObjects);
                    return;   // don't display fields or values if it's an array.
                }

                // display IEnumerable arrays. I's maybe also a ICollection that give the count property
                if (objToSend is IEnumerable)
                {
                    AddEnumerable((IEnumerable)objToSend, upperNode, sendPrivate, maxLevel, alreadyParsedObjects);
                    return;   // don't display fields or values if it's an array.
                }

                // display fields
                AddAllFieldsValue(objToSend, oType, upperNode, sendPrivate, maxLevel, alreadyParsedObjects);
                Dictionary<string, TMemberNode> dependencyProperties = GetDependencyPropertiesValues(objToSend);
                AddProperties(objToSend, oType, upperNode, sendPrivate, maxLevel, alreadyParsedObjects, dependencyProperties);
                AddDependencyPropertiesValues(upperNode, dependencyProperties);
            }
            catch (Exception e)
            {
                upperNode.Add("inner_addValue threw an exception of type " + e.GetType());
            }
        }

        //----------------------------------------------------------------------

        internal void AddEnumerable(IEnumerable array, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects)
        {
            try
            {
                // Count is introduce in ICollection interface. IEnumerable has just getEnumerator()
                if (array is ICollection)
                    upperNode.Col3 = new StringBuilder().Append(upperNode.Col3).Append(" (").Append(((ICollection)array).Count).Append(" element(s))").ToString();

                int c = 0;
                foreach (Object itemObject in array)
                {
                    // display the position in col1 : [n]
                    TMemberNode itemNode = new TMemberNode(new StringBuilder().Append("[").Append(c).Append("]").ToString());
                    upperNode.Add(itemNode);
                    // recursive call to display the value in col2 and type in col 3
                    Inner_addValue(itemObject, itemNode, sendPrivate, maxLevel - 1, alreadyParsedObjects);
                    c++;
                }
            }
            catch (Exception e)
            {
                upperNode.Add("addEnumerable threw an exception of type " + e.GetType());
            }
        }

        //----------------------------------------------------------------------

        internal void AddDictionary(IDictionary array, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects)
        {
            try
            {
                upperNode.Col3 = new StringBuilder().Append(upperNode.Col3).Append(" (").Append(array.Count).Append(" element(s))").ToString();
                foreach (DictionaryEntry itemDic in array)
                {
                    // display the key.ToString() as index in col1 : [MyKey]
                    TMemberNode itemNode = new TMemberNode(new StringBuilder().Append("[").Append(itemDic.Key).Append("]").ToString());
                    upperNode.Add(itemNode);
                    // recursive call to display the value in col2 and type in col 3
                    Inner_addValue(itemDic.Value, itemNode, sendPrivate, maxLevel - 1, alreadyParsedObjects);
                }
            }
            catch (Exception e)
            {
                upperNode.Add("addDictionary threw an exception of type " + e.GetType());
            }
        }

        //----------------------------------------------------------------------

        internal void AddArray(Array array, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects)
        {
            try
            {
                // construct the array Title (col3)
                StringBuilder arrTitle = new StringBuilder();
                arrTitle.Append(upperNode.Col3);   // get original accessor and type

                // empty array
                if (array.GetLength(0) == 0)
                {
                    arrTitle.Append(" Empty");
                    upperNode.Col3 = arrTitle.ToString();
                    return;
                }

                int dimCount = array.Rank;
                int[] boundsArray = new int[dimCount];

                arrTitle.Append(" (");             // and append bounds

                arrTitle.Append(array.GetLowerBound(0)).Append("..").Append(array.GetUpperBound(0));
                boundsArray[0] = array.GetLowerBound(0);
                for (int i = 1; i < dimCount; i++)
                {
                    arrTitle.Append(",").Append(array.GetLowerBound(i)).Append("..").Append(array.GetUpperBound(i));
                    boundsArray[i] = array.GetLowerBound(i);   // set bounds array to low index
                }
                arrTitle.Append(")");
                //arrTitle.Append(array.GetType().GetElementType()) ;
                upperNode.Col3 = arrTitle.ToString();


                // display the array
                int currentDim = dimCount - 1;
                while (currentDim >= 0)
                {    // loop 1.  currentDim is changed in Loop2
                     // construct a string with current indexes in col1 : [n][m]...
                    StringBuilder itemTitle = new StringBuilder();
                    for (int c = 0; c < dimCount; c++)
                        itemTitle.Append("[").Append(boundsArray[c]).Append("]");

                    // create the node with just array index title
                    TMemberNode itemNode = new TMemberNode(itemTitle.ToString());
                    upperNode.Add(itemNode);

                    // get the element value
                    object arrElement;
                    try
                    {
                        arrElement = array.GetValue(boundsArray);
                    }
                    catch (Exception ex)
                    {
                        arrElement = ex.GetType().ToString();
                    }
                    // recursive call to display the value in col2 and type in col 3
                    Inner_addValue(arrElement, itemNode, sendPrivate, maxLevel - 1, alreadyParsedObjects);

                    currentDim = dimCount - 1;
                    while (currentDim >= 0)
                    {    // loop 2
                        boundsArray[currentDim]++;
                        if (boundsArray[currentDim] > array.GetUpperBound(currentDim))
                        {
                            // out of bound. Reset current dimension and try to increment upper dimension
                            boundsArray[currentDim] = array.GetLowerBound(currentDim);
                            currentDim--;
                            // if the currentDim is less than 0 then loop 1 finish
                        }
                        else
                        {
                            // current dimension is not out of bound, quit loop 2
                            break;
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
        internal void AddAllFieldsValue(Object objToSend, Type oType, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects)
        {
            try
            {
#if NETSTANDARD1_6
                FieldInfo [] fi = oType.GetTypeInfo().GetFields(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  ) ;
#else              
                FieldInfo[] fi = oType.GetFields(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  );
#endif


                if (fi.Length <= 0)
                    return;

                foreach (var member in fi)
                {
                    if (member.IsPublic == false && sendPrivate == false)
                        continue;

                    object memberValue;
                    if (member.IsStatic)
                        memberValue = member.GetValue(null);
                    else
                        memberValue = member.GetValue(objToSend);

                    var memberModifier = "";

                    // Omit modifier if type is not send
                    if (TTrace.Options.SendTypeWithValue)
                    {
                        if (ReflectionHelper.IsDefaultMember(oType, member))
                            memberModifier = "[default] ";

                        memberModifier += ReflectionHelper.GetFieldModifier(member);  // already contain a space

                        // if the member value is null, add the declared type to MemberModifier
                        if (memberValue == null)
                            memberModifier += ReflectionHelper.Type2ShortString(member.FieldType);
                    }

                    var memberName = "";
                    // add the declaring type if not the actual type
#if !NETSTANDARD1_6
                    if (member.DeclaringType != member.ReflectedType)
                        memberName += ReflectionHelper.Type2ShortString(member.DeclaringType) + "::";
#endif

                    memberName += member.Name;

                    TMemberNode fieldNode = new TMemberNode(memberName, "", memberModifier);
                    upperNode.Add(fieldNode);

                    Inner_addValue(memberValue, fieldNode, sendPrivate, maxLevel - 1, alreadyParsedObjects);
                }
            }
            catch (Exception e)
            {
                upperNode.Add("addAllFieldsValue threw an exception of type " + e.GetType());
            }
        }

        // ReSharper disable once UnusedParameter.Global
        internal Dictionary<string, TMemberNode> GetDependencyPropertiesValues(Object objToSend)
        {
            Dictionary<string, TMemberNode> result = new Dictionary<string, TMemberNode>();
#if NETFULL && !NETSTANDARD1_6 && !NETSTANDARD2_0
            try
            {
                MarkupObject markupObject = MarkupWriter.GetMarkupObjectFor(objToSend);
                int nameErrorCount = 0;
                foreach (MarkupProperty mp in markupObject.Properties)
                {
                    if (mp.DependencyProperty == null)
                        continue;
                    string strTypeName = "";
                    if (TTrace.Options.SendTypeWithValue)
                    {
                        strTypeName = ReflectionHelper.Type2ShortString(mp.PropertyType);
                        if (mp.IsAttached)
                            strTypeName += " [attached]";
                    }
                    string mpName;
                    string mpStringValue;

                    try { mpName = mp.DependencyProperty.Name; } catch (Exception) { mpName = "?" + nameErrorCount++; }
                    try { mpStringValue = mp.StringValue; } catch (Exception) { mpStringValue = "?"; }

                    TMemberNode fieldNode = new TMemberNode(mpName, mpStringValue, strTypeName);
                    result.Add(mpName, fieldNode);
                }
            }
            catch (Exception)
            {
                // eat exception
            }
#endif
            return result;
        }


        //----------------------------------------------------------------------
        // caller : inner_addValue
        // ReSharper disable once UnusedParameter.Global
        internal void AddDependencyPropertiesValues(TMemberNode upperNode, Dictionary<string, TMemberNode> markupObjectProperties)
        {
            try
            {
                foreach (KeyValuePair<string, TMemberNode> markupObjectProperty in markupObjectProperties)
                {
                    TMemberNode markupNode = markupObjectProperty.Value;
                    markupNode.Col1 += " {dp}";
                    upperNode.Add(markupNode);
                }
            }
            catch (Exception e)
            {
                upperNode.Add("addDependencyPropertiesValues threw an exception of type " + e.GetType());
            }
        }

        //----------------------------------------------------------------------

        /// Display all properties (with corresponding value, if any) of the type
        /// Called by AddValue(), not by AddObject()
        internal void AddProperties(Object objToSend, Type oType, TMemberNode upperNode, bool sendPrivate, int maxLevel, ParsedObjectList alreadyParsedObjects, Dictionary<string, TMemberNode> markupObjectProperties)
        {
            try
            {
#if NETSTANDARD1_6
              PropertyInfo [] pi = oType.GetTypeInfo().GetProperties(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  // | BindingFlags.FlattenHierarchy
                  ) ;
#else
                PropertyInfo[] pi = oType.GetProperties(
                    BindingFlags.Public | BindingFlags.NonPublic |
                    BindingFlags.Static | BindingFlags.Instance
                    // | BindingFlags.FlattenHierarchy
                    );
#endif

                if (pi.Length <= 0)
                    return;

                int iProp;

                for (iProp = 0; iProp < pi.Length; iProp++)
                {
                    var field = pi[iProp];

                    //if (sendPrivate == false && (field.DeclaringType != field.ReflectedType))
                    //   continue ;

                    // Try to retrieve instance property value
                    object oValue;
                    try
                    {
                        oValue = field.GetValue(objToSend, null);
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
                            oValue = ex.Message;
                        }
                    }

                    string fieldNameKey = field.Name;
                    bool dependencyExist = markupObjectProperties.TryGetValue(fieldNameKey, out var markupNode);

                    // get only properties with "get"
                    if (field.CanRead == false)
                        continue;

                    MethodInfo getMethod = field.GetGetMethod(true);
                    if (getMethod == null)
                        continue;

                    // if the get or set method is public, check if the GET method has public access
                    if (getMethod.IsPublic == false && sendPrivate == false)
                        continue;

                    var memberName = "";
                    var memberModifier = "";
                    var memberType = "";
                    var hasPublic = false;

                    ReflectionHelper.Property2String(field, ref memberModifier, ref memberType, ref memberName, ref hasPublic);

                    // Omit modifier if type is not send
                    if (TTrace.Options.SendTypeWithValue)
                    {
                        if (ReflectionHelper.IsDefaultMember(oType, field))
                            memberModifier = "[default] " + memberModifier;
                        if (oValue == null)
                            memberModifier += memberType;        // add the declared type if null field
                        else if (memberModifier != "")
                            memberModifier += " ";
                    }
                    else
                    {
                        memberModifier = ""; // reset modifier
                    }
                    TMemberNode fieldNode = new TMemberNode(memberName, "", memberModifier);    // the col2 (property value) is not calculated here. Done by a recursive call to inner_addValue
                    if (dependencyExist)
                    {
                        fieldNode.DefaultCol2 = markupNode.Col2;  // save dependency value
                        markupObjectProperties.Remove(fieldNameKey);
                    }
                    upperNode.Add(fieldNode);

                    Inner_addValue(oValue, fieldNode, sendPrivate, maxLevel - 1, alreadyParsedObjects);
                }
            }
            catch (Exception e)
            {
                upperNode.Add("addProperties threw an exception of type " + e.GetType());
            }
        }

        //----------------------------------------------------------------------

        /// show the type with its interfaces, and base classes.
        internal void DisplayBases(Type oType)
        {
            try
            {
                TMemberNode basesGroup = null;

                while (oType != null)
                {
                    if (oType != typeof(object))
                    {
                        string typeName = ReflectionHelper.Type2ShortString(oType);
                        string interfacesNames = "";

                        // return ALL interfaces, not only interfaces for the current type
#if NETSTANDARD1_6
                      Type[] typeInterfaceList = oType.GetTypeInfo().GetInterfaces();
#else
                        Type[] typeInterfaceList = oType.GetInterfaces();
#endif

                        foreach (Type intf in typeInterfaceList)
                        {
                            if (interfacesNames == "")
                                interfacesNames = intf.Name;
                            else
                                interfacesNames += "," + intf.Name;
                        }

                        if (basesGroup == null)
                        {
                            basesGroup = new TMemberNode("Classes and interfaces").SetFontDetail(0, true);
                            basesGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                            Members.Add(basesGroup);
                        }

                        basesGroup.Add(typeName, interfacesNames);
                    }
#if NETSTANDARD1_6
                  oType = oType.GetTypeInfo().BaseType ;
#else
                    oType = oType.BaseType;
#endif
                }
            }
            catch (Exception e)
            {
                Members.Add("DisplayBases threw an exception of type " + e.GetType());
            }
        }

        //----------------------------------------------------------------------
        /// show nested type names (not the content)
        internal void DisplayNestedTypes(Type oType)
        {
            try
            {
#if NETSTANDARD1_6
              Type[] nestedTypes = oType.GetTypeInfo().GetNestedTypes(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
              );
#else
                Type[] nestedTypes = oType.GetNestedTypes(
                    BindingFlags.Public | BindingFlags.NonPublic |
                    BindingFlags.Static | BindingFlags.Instance
                    );
#endif
                if (nestedTypes.Length <= 0)
                    return;

                TMemberNode nestedTypeGroup = new TMemberNode("Nested Types").SetFontDetail(0, true);
                nestedTypeGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                Members.Add(nestedTypeGroup);

                foreach (var subType in nestedTypes)
                {
                    // don't check (yet) if the subtype is non public...

                    string strModifier = "", strName = "";
                    ReflectionHelper.Type2String(subType, ref strModifier, ref strName);
                    nestedTypeGroup.Add(strModifier, strName);
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
        internal void DisplayFields(Object objToSend, Type oType, Object documentationNav, TraceDisplayFlags flags)
        {
            try
            {
#if NETSTANDARD1_6
              FieldInfo [] fi = oType.GetTypeInfo().GetFields(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
              ) ;
#else
                FieldInfo[] fi = oType.GetFields(
                    BindingFlags.Public | BindingFlags.NonPublic |
                    BindingFlags.Static | BindingFlags.Instance
                    );
#endif

                if (fi.Length <= 0)
                    return;

                TMemberNode fieldsGroup = null;
                int iProp;

                for (iProp = 0; iProp < fi.Length; iProp++)
                {
                    var member = fi[iProp];

                    if ((flags & TraceDisplayFlags.ShowInheritedMembers) != TraceDisplayFlags.ShowInheritedMembers
#if !NETSTANDARD1_6
                      && (member.DeclaringType != member.ReflectedType)
#endif
                  )
                        continue;

                    if (member.IsPublic == false && (flags & TraceDisplayFlags.ShowNonPublic) != TraceDisplayFlags.ShowNonPublic)
                        continue;

                    object memberValue;
                    try
                    {
                        if (member.IsStatic)
                            memberValue = member.GetValue(null);
                        else if (objToSend != null)
                            memberValue = member.GetValue(objToSend);
                        else
                            memberValue = "";
                    }
                    catch (Exception ex)
                    {
                        memberValue = ex.Message;
                    }

                    // if the member value is null, convert it to a "null" string
                    if (memberValue == null)
                        memberValue = "null";
                    else if (memberValue.ToString() == member.FieldType.FullName)
                        memberValue = "";

                    var memberModifier = "";
                    var memberName = "";
                    if (ReflectionHelper.IsDefaultMember(oType, member))
                        memberModifier = "[default] ";

                    ReflectionHelper.Field2String(member, ref memberModifier, ref memberName);

                    if (fieldsGroup == null)
                    {
                        fieldsGroup = new TMemberNode("Fields").SetFontDetail(0, true);
                        fieldsGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                        Members.Add(fieldsGroup);
                    }

                    TMemberNode memberNode = new TMemberNode(memberName, memberValue.ToString(), memberModifier);
                    fieldsGroup.Add(memberNode);

                    // add doc and custom attribute
                    AddDocumentation(documentationNav, memberNode, oType, member);

                    if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
                    {
#if NETSTANDARD1_6
                      //Attribute [] CustomAttribs = Attribute.GetCustomAttributes (member , true);
                      //displayCustomAttrib (MemberNode,CustomAttribs) ;
#else
                        Attribute[] customAttribs = Attribute.GetCustomAttributes(member, true);
                        DisplayCustomAttribute(memberNode, customAttribs);
#endif
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
        // ReSharper disable once UnusedParameter.Global
        internal void DisplayDependencyProperties(Object objToSend)
        {
            try
            {
#if NETFULL && !NETSTANDARD1_6 && !NETSTANDARD2_0
                if (objToSend == null)
                    return;

                MarkupObject markupObject = MarkupWriter.GetMarkupObjectFor(objToSend);

                TMemberNode propertiesGroup = null;
                foreach (MarkupProperty mp in markupObject.Properties)
                {
                    if (mp.DependencyProperty == null)
                        continue;
                    TMemberNode fieldNode;

                    if (propertiesGroup == null)
                    {
                        propertiesGroup = new TMemberNode("Dependency Properties").SetFontDetail(0, true);
                        propertiesGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                        Members.Add(propertiesGroup);
                    }

                    string mpName;
                    string mpStringValue;
                    string strTypeName = ReflectionHelper.Type2ShortString(mp.PropertyType);

                    try { mpName = mp.Name; } catch (Exception) { mpName = "?"; }
                    try { mpStringValue = mp.StringValue; } catch (Exception) { mpStringValue = "?"; }


                    if (mp.IsAttached)
                        fieldNode = new TMemberNode(mpName, mpStringValue, strTypeName + "[attached]");
                    else
                        fieldNode = new TMemberNode(mpName, mpStringValue, strTypeName);
                    propertiesGroup.Add(fieldNode);
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
        internal void DisplayProperties(Object objToSend, Type oType, Object documentationNav, TraceDisplayFlags flags)
        {
            try
            {
#if NETSTANDARD1_6
              PropertyInfo [] pi = oType.GetTypeInfo().GetProperties(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  // | BindingFlags.FlattenHierarchy
                  ) ;
#else
                PropertyInfo[] pi = oType.GetProperties(
                    BindingFlags.Public | BindingFlags.NonPublic |
                    BindingFlags.Static | BindingFlags.Instance
                    // | BindingFlags.FlattenHierarchy
                    );
#endif

                if (pi.Length <= 0)
                    return;

                int iProp;

                TMemberNode propertiesGroup = null;

                for (iProp = 0; iProp < pi.Length; iProp++)
                {
                    var member = pi[iProp];

                    if ((flags & TraceDisplayFlags.ShowInheritedMembers) != TraceDisplayFlags.ShowInheritedMembers
#if !NETSTANDARD1_6
                        && (member.DeclaringType != member.ReflectedType)
#endif
                  )
                        continue;

                    // Try to retrieve instance property value
                    object oValue;
                    try
                    {
                        oValue = member.GetValue(objToSend, null);
                    }
                    catch
                    {
                        // if exception try to retrieve static property value
                        try { oValue = member.GetValue(null, null); }
                        catch { oValue = ""; }
                    }

                    var memberModifier = "";
                    var memberName = "";
                    var memberType = "";
                    var hasPublic = false;

                    if (ReflectionHelper.IsDefaultMember(oType, member))
                        memberModifier = "[default] ";
                    ReflectionHelper.Property2String(member, ref memberModifier, ref memberType, ref memberName, ref hasPublic);

                    // check if the GET or the SET method has public access
                    if (hasPublic == false && (flags & TraceDisplayFlags.ShowNonPublic) != TraceDisplayFlags.ShowNonPublic)
                        continue;

                    string memberValue;
                    if (oValue == null)
                        memberValue = "null";
                    else
                        memberValue = oValue.ToString();

                    if (propertiesGroup == null)
                    {
                        propertiesGroup = new TMemberNode("Properties").SetFontDetail(0, true);
                        propertiesGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                        Members.Add(propertiesGroup);
                    }

                    TMemberNode memberNode = new TMemberNode(memberName, memberValue, memberModifier + memberType);
                    propertiesGroup.Add(memberNode);
                    // add doc and custom attribute
                    AddDocumentation(documentationNav, memberNode, oType, member);
                    if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
                    {
#if NETSTANDARD1_6
                       //Attribute [] CustomAttribs = Attribute.GetCustomAttributes (member , true);
                       //displayCustomAttrib (MemberNode,CustomAttribs) ;
#else
                        Attribute[] customAttribs = Attribute.GetCustomAttributes(member, true);
                        DisplayCustomAttribute(memberNode, customAttribs);
#endif
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
        internal void DisplayConstructors(Type oType, Object documentationNav, TraceDisplayFlags flags)
        {
            try
            {
#if NETSTANDARD1_6
              ConstructorInfo [] ci = oType.GetTypeInfo().GetConstructors (
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  ) ;
#else
                ConstructorInfo[] ci = oType.GetConstructors(
                    BindingFlags.Public | BindingFlags.NonPublic |
                    BindingFlags.Static | BindingFlags.Instance
                    );
#endif
                // no way to see inherited constructor directly from one type :-(
                if (ci.Length <= 0)
                    return;

                int iProp;

                TMemberNode constructorGroup = null;
                for (iProp = 0; iProp < ci.Length; iProp++)
                {
                    var memberModifier = "";
                    var memberName = "";

                    ConstructorInfo member = ci[iProp];

                    if (member.IsPublic == false && (flags & TraceDisplayFlags.ShowNonPublic) != TraceDisplayFlags.ShowNonPublic)
                        continue;

                    ReflectionHelper.Constructor2String(member, ref memberModifier, ref memberName);

                    if (constructorGroup == null)
                    {
                        constructorGroup = new TMemberNode("Constructors").SetFontDetail(0, true);
                        constructorGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                        Members.Add(constructorGroup);
                    }

                    TMemberNode memberNode = new TMemberNode(memberModifier, memberName); // ci[iprop].ToString()) ;
                    constructorGroup.Add(memberNode);

                    // add doc and custom attribute
                    AddDocumentation(documentationNav, memberNode, oType, member);
                    if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
                    {
#if NETSTANDARD1_6
                      //Attribute [] CustomAttribs = Attribute.GetCustomAttributes (member , true);
                      //displayCustomAttrib (MemberNode,CustomAttribs) ;
#else
                        Attribute[] customAttribs = Attribute.GetCustomAttributes(member, true);
                        DisplayCustomAttribute(memberNode, customAttribs);
#endif
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
        internal void DisplayMethods(Type oType, Object documentationNav, TraceDisplayFlags flags)
        {
            try
            {
#if NETSTANDARD1_6
              MethodInfo [] miRaw = oType.GetTypeInfo().GetMethods(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  )  ;
#else
                MethodInfo[] miRaw = oType.GetMethods(
                    BindingFlags.Public | BindingFlags.NonPublic |
                    BindingFlags.Static | BindingFlags.Instance
                    );
#endif
                if (miRaw.Length <= 0)
                    return;

                int iProp;

                TMemberNode methodsGroup = null;
                TMemberNode operatorGroup = null;

                // get method list, but discard method used by properties
                for (iProp = 0; iProp < miRaw.Length; ++iProp)
                {
                    var member = miRaw[iProp];
                    string methodName = miRaw[iProp].Name;

                    // ReSharper disable StringIndexOfIsCultureSpecific.1
                    if (methodName.IndexOf("add_") != 0 &&
                        methodName.IndexOf("remove_") != 0 &&
                        methodName.IndexOf("get_") != 0 &&
                        methodName.IndexOf("set_") != 0)
                    // ReSharper restore StringIndexOfIsCultureSpecific.1
                    {

                        if (((flags & TraceDisplayFlags.ShowInheritedMembers) != TraceDisplayFlags.ShowInheritedMembers)
#if !NETSTANDARD1_6
                            && (member.DeclaringType != member.ReflectedType)
#endif
                            )
                            continue;

                        if (member.IsPublic == false && (flags & TraceDisplayFlags.ShowNonPublic) != TraceDisplayFlags.ShowNonPublic)
                            continue;

                        var memberModifier = "";
                        var memberName = "";

                        if (ReflectionHelper.IsDefaultMember(oType, member))
                            memberModifier = "[default] ";

                        // to do : Method2String will return true if it's an operator
                        bool isOperator = ReflectionHelper.Method2String(member, ref memberModifier, ref memberName);
                        TMemberNode groupToUse;
                        if (isOperator)
                        {
                            if (operatorGroup == null)
                            {
                                operatorGroup = new TMemberNode("Operators").SetFontDetail(0, true);
                                operatorGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                                Members.Add(operatorGroup);
                            }
                            groupToUse = operatorGroup;
                        }
                        else
                        {
                            if (methodsGroup == null)
                            {
                                methodsGroup = new TMemberNode("Methods").SetFontDetail(0, true);
                                methodsGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                                Members.Add(methodsGroup);
                            }
                            groupToUse = methodsGroup;
                        }

                        TMemberNode memberNode = new TMemberNode(memberModifier, memberName);  //  mi[iprop].ToString()
                        groupToUse.Add(memberNode);

                        // add doc and custom attribute
                        AddDocumentation(documentationNav, memberNode, oType, member);

                        if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
                        {
#if NETSTANDARD1_6
                          //Attribute [] CustomAttribs = Attribute.GetCustomAttributes(member, true);
                          //displayCustomAttrib(MemberNode, CustomAttribs);
#else
                            Attribute[] customAttribs = Attribute.GetCustomAttributes(member, true);
                            DisplayCustomAttribute(memberNode, customAttribs);
#endif
                        }
                    }     // discard method used by properties
                }        // for loop
            }
            catch (Exception e)
            {
                Members.Add("DisplayMethods threw an exception of type " + e.GetType());
            }
        }           // DisplayMethods()

        //----------------------------------------------------------------------

        /// display all Events of a type
        internal void DisplayEvents(Type oType, Object documentationNav, TraceDisplayFlags flags)
        {
            try
            {
                int iProp;
#if NETSTANDARD1_6
              EventInfo [] ei = oType.GetTypeInfo().GetEvents(
                  BindingFlags.Public | BindingFlags.NonPublic |
                  BindingFlags.Static | BindingFlags.Instance
                  ) ;
#else
                EventInfo[] ei = oType.GetEvents(
                    BindingFlags.Public | BindingFlags.NonPublic |
                    BindingFlags.Static | BindingFlags.Instance
                    );
#endif

                if (ei.Length <= 0)
                    return;

                TMemberNode eventsGroup = null;

                for (iProp = 0; iProp < ei.Length; iProp++)
                {
                    var member = ei[iProp];

                    if ((flags & TraceDisplayFlags.ShowInheritedMembers) != TraceDisplayFlags.ShowInheritedMembers
#if !NETSTANDARD1_6
                        && (member.DeclaringType != member.ReflectedType)
#endif
                      )
                        continue;

                    // events are public...
                    //if (member.IsPublic == false && (flags & TraceDisplayFlags.ShowNonPublic) != TraceDisplayFlags.ShowNonPublic)
                    //   continue ;

                    var memberModifier = "";
                    var memberName = "";

                    if (ReflectionHelper.IsDefaultMember(oType, member))
                        memberModifier = "[default] ";

                    ReflectionHelper.Event2String(member, ref memberModifier, ref memberName);

                    if (eventsGroup == null)
                    {
                        eventsGroup = new TMemberNode("Events").SetFontDetail(0, true);
                        eventsGroup.ViewerKind = TraceConst.CST_VIEWER_OBJECT;
                        Members.Add(eventsGroup);
                    }

                    TMemberNode memberNode = new TMemberNode(memberModifier, memberName);
                    eventsGroup.Add(memberNode);

                    // add doc and custom attrib
                    AddDocumentation(documentationNav, memberNode, oType, member);

                    if ((flags & TraceDisplayFlags.ShowCustomAttributes) != 0)
                    {
#if NETSTANDARD1_6
                      //Attribute [] CustomAttribs = Attribute.GetCustomAttributes (member , true);
                      //displayCustomAttrib (MemberNode,CustomAttribs) ;
#else
                        Attribute[] customAttribs = Attribute.GetCustomAttributes(member, true);
                        DisplayCustomAttribute(memberNode, customAttribs);
#endif
                    }
                }
            }
            catch (Exception e)
            {
                Members.Add("DisplayEvents threw an exception of type " + e.GetType());
            }
        }

        //----------------------------------------------------------------------
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
                return;

            try
            {
                TMemberNode group = new TMemberNode("Call stack").SetFontDetail(0, true);
                group.ViewerKind = TraceConst.CST_VIEWER_STACK;
                Members.Add(group);

#if NETSTANDARD1_6
              Exception ex;
              try
              {
                  throw new Exception();
              }
              catch (Exception e)
              {
                  ex = e ;
              }
              StackTrace stackTrace = new StackTrace(ex,true);
#else
                StackTrace stackTrace = new StackTrace();
#endif
                StackFrame[] frames = stackTrace.GetFrames();
                int i = 0;
                if (frames == null)
                    return;
                foreach (var sf in frames)
                {
                    i++;
                    if (i < level)
                        continue;
                    MethodBase oneMethod = sf.GetMethod();

                    string memberModifier = "";
                    string memberName = "";
                    string assemblyName;
                    try
                    {
                        if (oneMethod.DeclaringType == null)   // lambda methods are not in type
                            assemblyName = "";
                        else
#if NETSTANDARD1_6
                         assemblyName = oneMethod.DeclaringType.GetTypeInfo().Assembly.GetName().Name;  
#else
                            assemblyName = oneMethod.DeclaringType.Assembly.GetName().Name;
#endif
                    }
                    catch (MethodAccessException)
                    {
                        assemblyName = "Assembly name is [SecurityCritical]";
                    }
                    catch (Exception)
                    {
                        assemblyName = "???";
                    }

                    // discard methods from this assembly
#if NETSTANDARD1_6
                  if (oneMethod.DeclaringType == null || !Equals(oneMethod.DeclaringType.GetTypeInfo().Assembly, GetType().GetTypeInfo().Assembly))
#else
                    if (oneMethod.DeclaringType == null || oneMethod.DeclaringType.Assembly != GetType().Assembly)
#endif
                    {
                        if (oneMethod is MethodInfo)
                            ReflectionHelper.Method2String((MethodInfo)oneMethod, ref memberModifier, ref memberName);
                        else
                            ReflectionHelper.Constructor2String((ConstructorInfo)oneMethod, ref memberModifier, ref memberName);

                        if (oneMethod.DeclaringType != null)
                            memberName = oneMethod.DeclaringType.FullName + '.' + memberName;

                        int lineNumber = sf.GetFileLineNumber();
                        if (lineNumber != 0)
                            memberName = memberName + " Line " + lineNumber;

                        TMemberNode memberNode = new TMemberNode(assemblyName, memberName);  // don't display MemberModifier. Not needed for stack trace
                        group.Add(memberNode);
                    }
                }
            }
            catch (Exception e)
            {
                Members.Add("AddStackTrace threw an exception of type " + e.GetType());
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
                return;

            try
            {
                TMemberNode group = new TMemberNode("Caller information").SetFontDetail(0, true);
                group.ViewerKind = TraceConst.CST_VIEWER_STACK;
                Members.Add(group);


#if NETSTANDARD1_6
              Exception ex ;
              try
              {
                  throw new Exception();
              }
              catch (Exception e)
              {
                  ex = e ;
              }
              StackTrace stackTrace = new StackTrace(ex,true);
#else
                StackTrace stackTrace = new StackTrace();
#endif
                StackFrame[] frames = stackTrace.GetFrames();

                int i = 0;
                if (frames == null)
                    return;
                foreach (var sf in frames)
                {
                    i++;
                    if (i < level)
                        continue;
                    MethodBase oneMethod = sf.GetMethod();

                    string memberModifier = "";
                    string memberName = "";

                    if (oneMethod is MethodInfo methodInfo)
                        ReflectionHelper.Method2String(methodInfo, ref memberModifier, ref memberName);
                    else
                        ReflectionHelper.Constructor2String((ConstructorInfo)oneMethod, ref memberModifier, ref memberName);

                    string assemblyName = "AssemblyName unknown";
                    try
                    {
                        if (oneMethod.DeclaringType != null)
#if NETSTANDARD1_6
                         assemblyName = oneMethod.DeclaringType.GetTypeInfo().Assembly.GetName().Name; 
#else
                            assemblyName = oneMethod.DeclaringType.Assembly.GetName().Name;
#endif
                    }
                    catch (MethodAccessException)
                    {
                        assemblyName = "Assembly name is [SecurityCritical]";
                    }

                    TMemberNode memberNode = new TMemberNode(memberModifier, memberName, assemblyName);
                    group.Add(memberNode);
                }
            }
            catch (Exception e)
            {
                Members.Add("AddCaller threw an exception of type " + e.GetType());
            }
        }

        //------------------------------------------------------------------------------

#if !NETSTANDARD1_6 && !NETSTANDARD2_0
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
                byte[] sourceData = new byte[sourceLength];
                imgStream.Read(sourceData, 0, sourceLength);
                imgStream.Close();

                // 3) encode (base 64) source array into another array
                char[] base64Data = new char[(int)(Math.Ceiling((double)sourceLength / 3) * 4)];
                Convert.ToBase64CharArray(sourceData, 0, sourceLength, base64Data, 0);

                // 4) attach the encoded array to a new member. the member viewer kind specify a bitmap viewer
                TMemberNode member = Members.Add(new String(base64Data));
                member.ViewerKind = TraceConst.CST_VIEWER_BITMAP;
            }
            catch (Exception e)
            {
                Members.Add("AddBitmap threw an exception of type " + e.GetType());
            }

        }
#endif

        //------------------------------------------------------------------------------

#if NETFULL && !NETSTANDARD1_6 && !NETSTANDARD2_0
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
                if (image.Source is BitmapSource source)
                {
                    BitmapFrame bitmapFrame = BitmapFrame.Create(source);
                    encoder.Frames.Add(bitmapFrame);
                }

                // 1) put the image into a stream
                MemoryStream imgStream = new MemoryStream();

                encoder.Save(imgStream);

                ////////image.Save(imgStream, System.Drawing.Imaging.ImageFormat.Bmp);
                imgStream.Position = 0;

                // 2) create a byte array from the stream
                int sourceLength = (int)imgStream.Length;
                byte[] sourceData = new byte[sourceLength];
                imgStream.Read(sourceData, 0, sourceLength);
                imgStream.Close();

                // 3) encode (base 64) source array into another array
                char[] base64Data = new char[(int)(Math.Ceiling((double)sourceLength / 3) * 4)];
                Convert.ToBase64CharArray(sourceData, 0, sourceLength, base64Data, 0);

                // 4) attach the encoded array to a new member. the member viewer kind specify a bitmap viewer
                TMemberNode member = Members.Add(new String(base64Data));
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
        /// <param name="shortTitle">Title to display in the first col</param>
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
        /// <param name="shortTitle">Title to display in the first col</param>
        /// <param name="bytes">Pointer to the buffer to dump</param>
        /// <param name="index">start offset</param>
        /// <param name="count">Number of byte to dump</param>
        public void AddDump(string shortTitle, byte[] bytes, int index, int count)
        {
            if (Enabled == false)
                return;

            try
            {
                int c = index;
                int byteDumped = 0;

                TMemberNode dumpGroup = new TMemberNode(shortTitle).SetFontDetail(0, true);
                dumpGroup.ViewerKind = TraceConst.CST_VIEWER_DUMP;
                Members.Add(dumpGroup);

                while (byteDumped < count && (c < bytes.Length))
                {
                    int d = 0;           // inner loop. From 0 to 15 max
                    int beginLine = c;   // used to print the offset
                    StringBuilder hexaRepresentation = new StringBuilder();
                    //StringBuilder Str_representation = new StringBuilder();

                    while ((byteDumped < count) && (d < 16) && (c < bytes.Length))
                    {
                        byte oneByte = bytes[c];
                        hexaRepresentation.Append(((Int32)oneByte).ToString("X2")).Append(" ");

                        // only the zero cannot be copied to the stream
                        //if (OneByte == 0)
                        //   Str_representation.Append('.') ;
                        //else
                        //   Str_representation.Append((char) OneByte) ;

                        byteDumped++;
                        d++;
                        c++;
                    }
                    dumpGroup.Add(beginLine.ToString("X6"), hexaRepresentation.ToString()); // , Str_representation.ToString());
                    //.SetFontDetail(1,false,false,-1,0,"Lucida console") ;
                }
                dumpGroup.Col2 = byteDumped.ToString() + " byte(s) dumped";
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
        // ReSharper disable once InconsistentNaming
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

        internal bool Inner_AddTable(TMemberNode tableMembers, Object itemObject, bool isFirstRow, string firstColValue)
        {
            object memberValue;
            string strMemberValue;
            bool isFirstCol = true;

            TMemberNode fCurrentRow = tableMembers.Add("");
            Type oType = itemObject.GetType();

            // set first col if give. First col title is set by caller.
            if (firstColValue != null)
                fCurrentRow.Col1 = firstColValue;

            // special case for Primitive object
#if NETSTANDARD1_6
         if (oType.GetTypeInfo().IsPrimitive || oType.GetTypeInfo().IsEnum || itemObject is string || itemObject is StringBuilder || itemObject is DateTime)
#else
            if (oType.IsPrimitive || oType.IsEnum || itemObject is string || itemObject is StringBuilder || itemObject is DateTime)
#endif
            {
                // Add Column Title if first line
                if (isFirstRow)
                    if (tableMembers.Col1 == "")
                        tableMembers.Col1 = "Values";
                    else
                        tableMembers.Col1 += "\tValues";


                //if (isFirstCol) 
                fCurrentRow.Col1 = itemObject.ToString();
                //else 
                //   fCurrentRow.Col1 += "\t" + itemObject.ToString();
                //isFirstCol = false ;
                return false;
            }

            //add Fields Value
            //----------------
#if NETSTANDARD1_6
         FieldInfo[] fi = itemObject.GetType().GetTypeInfo().GetFields(BindingFlags.Public | BindingFlags.Static | BindingFlags.Instance);
#else
            FieldInfo[] fi = itemObject.GetType().GetFields(BindingFlags.Public | BindingFlags.Static | BindingFlags.Instance);
#endif

            foreach (var member in fi)
            {
                // Add Column Title if first line
                if (isFirstRow)
                {
                    if (tableMembers.Col1 == "")
                        tableMembers.Col1 = member.Name;
                    else
                        tableMembers.Col1 = tableMembers.Col1 + "\t" + member.Name;
                }
                // add data
                memberValue = member.GetValue(itemObject);
                if (memberValue == null)
                    strMemberValue = "null";
                else
                    strMemberValue = memberValue.ToString();

                if (isFirstCol)
                    fCurrentRow.Col1 = strMemberValue;
                else
                    fCurrentRow.Col1 = fCurrentRow.Col1 + "\t" + strMemberValue;
                isFirstCol = false;
            }
            // add Properties
            //-----------------
#if NETSTANDARD1_6
         PropertyInfo[] pi = itemObject.GetType().GetTypeInfo().GetProperties(
            BindingFlags.Public | BindingFlags.Static | BindingFlags.Instance
            // | BindingFlags.FlattenHierarchy
            );
#else
            PropertyInfo[] pi = itemObject.GetType().GetProperties(
               BindingFlags.Public | BindingFlags.Static | BindingFlags.Instance
               // | BindingFlags.FlattenHierarchy
               );
#endif
            foreach (var field in pi)
            {
                // Add Column Title if first line
                if (isFirstRow)
                {
                    if (tableMembers.Col1 == "")
                        tableMembers.Col1 = field.Name;
                    else
                        tableMembers.Col1 = tableMembers.Col1 + "\t" + field.Name;
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

                if (isFirstCol)
                    fCurrentRow.Col1 = strMemberValue;
                else
                    fCurrentRow.Col1 = fCurrentRow.Col1 + "\t" + strMemberValue;
                isFirstCol = false;
            }

            // add Dependency Properties
            //--------------------------

#if NETFULL && !NETSTANDARD1_6 && !NETSTANDARD2_0
            bool hasDependencyProperties = false;
            MarkupObject markupObject = MarkupWriter.GetMarkupObjectFor(itemObject);

            string allProperties = "";
            foreach (MarkupProperty mp in markupObject.Properties)
            {
                if (mp.DependencyProperty == null)
                    continue;
                string nameAndValue = mp.Name + "=" + mp.StringValue;
                if (allProperties == "")
                    allProperties = nameAndValue;
                else
                    allProperties += ", " + nameAndValue;
                hasDependencyProperties = true;
            }
            if (isFirstCol)
                fCurrentRow.Col1 = allProperties;
            else
                fCurrentRow.Col1 = fCurrentRow.Col1 + "\t" + allProperties;
            //isFirstCol = false ;
            return hasDependencyProperties;
#else
            return false;
#endif
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
                var tableMembers = Members.Add("");
                tableMembers.ViewerKind = TraceConst.CST_VIEWER_TABLE;

                // fill table
                bool isFirst = true;
                bool hasDependencyProperties = false;

                // check for specialized object before using the IEnumerable
                if (list is Array)
                {
                    // Special case for Array : display the index on first column
                    // Array implement : IList, ICollection, IEnumerable
                    int c = 0;
                    tableMembers.Col1 = "Index"; // set first col title
                    foreach (Object itemObject in (Array)list)
                    {
                        if (Inner_AddTable(tableMembers, itemObject, isFirst, "[" + c + "]"))
                            hasDependencyProperties = true;

                        isFirst = false;
                        c++;
                    }
                }
                else if (list is IDictionary)
                {
                    // Special case for IDictionary : display the Key on first column
                    // IDictionary must be check before IEnumerable because IDictionary inherit from IEnumerable
                    tableMembers.Col1 = "Key"; // set first col title
                    foreach (DictionaryEntry itemDic in (IDictionary)list)
                    {
                        if (Inner_AddTable(tableMembers, itemDic.Value, isFirst, "[" + itemDic.Key + "]"))  // key cannot be null
                            hasDependencyProperties = true;
                        isFirst = false;
                    }
                }
                else if (list is IEnumerable)
                {
                    // IEnumerable is the base class for ICollection ,IList, IDictionary 
                    // Error may occur here if your LINQ query contains errors
                    foreach (Object itemObject in (IEnumerable)list)
                    {
                        if (Inner_AddTable(tableMembers, itemObject, isFirst, null)) // no first column
                            hasDependencyProperties = true;
                        isFirst = false;
                    }
                }
                else
                {
                    // not a collection : print single object  
                    if (Inner_AddTable(tableMembers, list, /*isFirst*/ true, null)) // no first column
                        hasDependencyProperties = true;
                }
                if (hasDependencyProperties)
                    tableMembers.Col1 += "\t" + "Dependencies"; // set first col title
            }
            catch (Exception e)
            {
                Members.Add("AddTable threw an exception of type " + e.GetType());
            }
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// Change background font color
        /// </summary>
        /// <param name="color">RGB background color (see Color.ToArgb function)</param>
        public void AddBackgroundColor(int color)
        {
            if (Enabled == false)
                return;
            AddBackgroundColor(color, -1);
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
                return;
            FontDetail fontDetail = new FontDetail
            {
                ColId = colId,
                Color = color,
                FontName = "BackgroundColor"  // special name. Indicate that color is for background, not font itself
            };
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
                return this;

            FontDetail fontDetail = new FontDetail
            {
                ColId = colId,
                Bold = bold,
                Italic = italic,
                Color = color,         // color is stored in ARGB. Converted to BGR before sending the trace
                Size = size,
                FontName = fontName
            };

            if (FontDetails == null)
                FontDetails = new List<FontDetail>();

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
            TraceNode result = new TraceNode(this); // create a copy
            if (Enabled == false)
                return result;

            List<string> commandList = new List<string>();

            Helper.AddCommand(commandList, TraceConst.CST_NEW_NODE, ParentNodeId); // param : parent Node id
            Helper.AddCommand(commandList, TraceConst.CST_TRACE_ID, Id);               // param : guid
            if (LeftMsg != null)
                Helper.AddCommand(commandList, TraceConst.CST_LEFT_MSG, LeftMsg);       // param : left string
            if (RightMsg != null)
                Helper.AddCommand(commandList, TraceConst.CST_RIGHT_MSG, RightMsg);     // param : right string
            Helper.AddCommand(commandList, TraceConst.CST_ICO_INDEX, IconIndex);       // param : the icon index

            // add font detail
            if (FontDetails != null)
            {
                foreach (FontDetail fontDetail in FontDetails)
                {
                    int colorValue;

                    if (fontDetail.Color == -1)
                        colorValue = -1;
                    else
                        colorValue = Helper.ARGB_to_BGR(fontDetail.Color);

                    StringBuilder tempStr = new StringBuilder();

                    if (fontDetail.FontName == "BackgroundColor")
                    {
                        //special color : background color
                        Helper.AddCommand(commandList, TraceConst.CST_BACKGROUND_COLOR, colorValue, fontDetail.ColId.ToString());      // param : color, colId
                    }
                    else
                    {
                        tempStr.Append(String.Format("{0,5}{1,3}", TraceConst.CST_FONT_DETAIL, fontDetail.ColId));

                        if (fontDetail.Bold)
                            tempStr.Append("1");
                        else
                            tempStr.Append("0");

                        if (fontDetail.Italic)
                            tempStr.Append("1");
                        else
                            tempStr.Append("0");
                        tempStr.Append(String.Format("{0,11}{1,11}", colorValue, fontDetail.Size)).Append(fontDetail.FontName);
                        commandList.Add(tempStr.ToString());
                    }
                }
                FontDetails.Clear();
                FontDetails = null;  // once copied to CommandList, clear the array
            }

            Members.AddToStringList(commandList);   // convert all groups and nested items/group to strings

            TTrace.SendToWinTraceClient(commandList, WinTraceId, Time, ThreadName);
            return result;
        }

        //----------------------------------------------------------------------

        /// <summary>
        /// Resend the trace to the server (only left and right message)
        /// </summary>
        public void Resend()
        {
            if (Enabled == false)
                return;

            List<string> commandList = new List<string>();

            Helper.AddCommand(commandList, TraceConst.CST_USE_NODE, Id);           // param : guid
            Helper.AddCommand(commandList, TraceConst.CST_LEFT_MSG, LeftMsg);       // param : left string
            Helper.AddCommand(commandList, TraceConst.CST_RIGHT_MSG, RightMsg);      // param : right string

            // don't resend members and icon
            TTrace.SendToWinTraceClient(commandList, WinTraceId);
        }  // Resend
    }     // TraceNodeEx
}        // namespace TraceTool
