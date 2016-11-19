/*
 * TraceNodeEx.java
 *
 * HomePage : http://www.codeproject.com/csharp/TraceTool.asp
 * Download : http://sourceforge.net/projects/tracetool/
 * See License.txt for license information
 *
 * Author : Thierry Parent
 * Version : 12.4.1
 */

package tracetool;

import java.awt.Color;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

//----------------------------------------------------------------------
//----------------------------------------------------------------------
//----------------------------------------------------------------------

/*
 * * Alternate way to send traces : prepare a TraceNode with all properties then send it
 * @author tpa
 */
/*
 * *
 * @author Thierry Parent
 */
public class TraceNodeEx extends TraceNodeBase
{
   /** Parent Node Id */
   public String parentNodeId;

   /** Left message */
   public String leftMsg;

   /** right message (comment) */
   public String rightMsg;

   /** time */
   public String time;

   /** thread name */
   public String threadName;

   /*
    * * the root for the Member tree
    */

   public TMemberNode members = new TMemberNode();

   private ArrayList fontDetails;

   // ----------------------------------------------------------------------

   /**
    * create a Node with an unique ID (true)
    * @param parentNode The parent node where to place that trace. The IconIndex and the enabled properties are also recopied Parameters can be null : the root tree become the parent node, enabled is true and the default icon is used
    */

   public TraceNodeEx(final TraceToSend parentNode)
   {
      this.id = new java.rmi.server.UID().toString();

      if (parentNode == null)
      {
         this.iconIndex = TraceConst.CST_ICO_DEFAULT;
         this.enabled = true;
         this.winTraceId = null;
         this.parentNodeId = ""; //$NON-NLS-1$
      } else
      {
         this.iconIndex = parentNode.iconIndex;
         this.enabled = parentNode.enabled;
         this.winTraceId = parentNode.winTraceId;
         this.parentNodeId = parentNode.getLastContextId();
      }
   }

   // ----------------------------------------------------------------------
   /**
    * Create a Node
    * @param parentNode The parent node where to place that trace. The IconIndex and the enabled properties are also recopied Parameters can be null : the root tree become the parent node, enabled is true and the default icon is used
    * @param generateUniqueId if true, the id is generated automatically, else set the empty string
    */

   public TraceNodeEx(final TraceToSend parentNode, final boolean generateUniqueId)
   {
      if (generateUniqueId)
         this.id = new java.rmi.server.UID().toString();

      if (parentNode == null)
      {
         this.iconIndex    = TraceConst.CST_ICO_DEFAULT;
         this.enabled      = true;
         this.winTraceId   = null;
         this.parentNodeId = ""; //$NON-NLS-1$
      } else
      {
         this.iconIndex    = parentNode.iconIndex;
         this.enabled      = parentNode.enabled;
         this.winTraceId   = parentNode.winTraceId;
         this.parentNodeId = parentNode.getLastContextId();
      }
   }

   // ----------------------------------------------------------------------

   /**
    * create a Node with an unique ID
    */

   public TraceNodeEx()
   {
      this.id = new java.rmi.server.UID().toString();
      this.iconIndex = TraceConst.CST_ICO_DEFAULT;
      this.enabled = true;
      this.winTraceId = null;
      this.parentNodeId = "";//$NON-NLS-1$
   }

   // ----------------------------------------------------------------------

   /**
    * Send Private and public values of an object.<p>
    * addValue is quite different from addObject : less verbal (no class info) but can show many level.
    * @param objToSend the object to examine
    * @since 4.0.0
    *
    */
   public void addValue(final Object objToSend)
   {
      addValue(objToSend, TTrace.options.sendPrivate);
   }

   // ----------------------------------------------------------------------

   /**
    * Send Private and public values of an object.<p>
    * addValue is quite different from addObject : less verbal (no class info) but can show many level.
    * @param objToSend the object to examine
    * @param sendPrivate flag to send private field
    * @since 4.0.0
    *
    */
   public void addValue(final Object objToSend, final boolean sendPrivate)
   {
      addValue(objToSend, sendPrivate, TTrace.options.objectTreeDepth);
   }

   // ----------------------------------------------------------------------
   /**
    * Send Private and public values of an object.<p>
    * addValue is quite different from addObject : less verbal (no class info) but can show many level.
    * @param objToSend the object to examine
    * @param sendPrivate flag to send private field
    * @param maxLevel The number of sub element to display. Default is 3
    * @since 4.0.0
    *
    */
   public void addValue(final Object objToSend, final boolean sendPrivate, final int maxLevel)
   {
      String title;
      if (objToSend == null)
         title = ""; //$NON-NLS-1$
      else
      {
         Class objClass = objToSend.getClass();
         if (objClass.getModifiers() == 0)
            title = objClass.getName();
         else
            title = Modifier.toString(objClass.getModifiers()) + " " + objClass.getName(); //$NON-NLS-1$
      }
      addValue(objToSend, sendPrivate, maxLevel, title);
   }

   // ----------------------------------------------------------------------

   /**
    * Send Private and public values of an object.<p>
    * addValue is quite different from addObject : less verbal (no class info) but can show many level.
    * @param objToSend the object to examine
    * @param sendPrivate flag to send private field
    * @param maxLevel The number of sub element to display. Default is 3
    * @param title Object title
    * @since 4.0.0
    *
    */
   public void addValue(final Object objToSend, final boolean sendPrivate, final int maxLevel, final String title)
   {
      addValue(objToSend, sendPrivate, maxLevel, title, new HashMap());
   }

   // ----------------------------------------------------------------------
   /**
    * Send Private and public values of an object.<p>
    * addValue is quite different from addObject : less verbal (no class info) but can show many level.
    * @param objToSend The object to examine
    * @param sendPrivate flag to send private field
    * @param maxLevel The number of sub element to display. Default is 3
    * @param title Object title
    * @param alreadyParsedObject List of already parsed objects
    * @since 4.0.0
    *
    */
   protected void addValue(final Object objToSend, final boolean sendPrivate, final int maxLevel, final String title, final HashMap alreadyParsedObject)
   {
      if (!this.enabled)
         return;

      if (objToSend == null)
      {
         this.members.add("null"); //$NON-NLS-1$
         return;
      }

      try
      {
         TMemberNode result;

         // create the top node using only title.
         // Value (col2) and Type (col3) will be added by inner_addValue
         result = new TMemberNode(title, Utility.getObjectHashCode(objToSend));
         result.viewerKind = TraceConst.CST_VIEWER_VALUE ;

         // add top node to trace
         this.members.add(result);

         // recursive fill members
         innerAddValue(objToSend, result, maxLevel, sendPrivate, alreadyParsedObject);
      } catch (Exception e)
      {
         // eat exception
      }
   }

   // ----------------------------------------------------------------------

   /**
    * internal AddValue (recursive)
    * @param objToSend The object to examine
    * @param upperNode The upper node (recursive add sub objects)
    * @param maxLevel The number of sub element to display.
    * @param sendPrivate flag to send private field
    * @param alreadyParsedObject List of already parsed objects
    */
   private void innerAddValue(final Object objToSend, final TMemberNode upperNode, final int maxLevel, final boolean sendPrivate, final HashMap alreadyParsedObject)
   {
      try
      {

         if (objToSend == null)
         {
            upperNode.col2 = "Null"; //$NON-NLS-1$
            return;
         }

         Class objClass = objToSend.getClass();

         // display the modifier and type name in upper node (col 3). Old col3 content is keept
         upperNode.col3 = new StringBuffer().append(upperNode.col3).append(objClass.toString()).toString();

         // display value in upper node (col2)

         // check primitive and well know type
         // String strObjClass = objClass.getName() ;
         if (Utility.isPrimitive(objClass))
         {
            upperNode.col2 = Utility.getObjectString(objToSend);
            return;
         }

         // check class stored in the tracetool.property file,
         // like java.lang.Date or your business class having a nice toString()
         for (Iterator iter = TTrace.nativeClasses.iterator(); iter.hasNext();)
         {
            Class nativeClass = (Class) iter.next();
            if (nativeClass.isAssignableFrom(objClass))
            {
               upperNode.col2 = Utility.getObjectString(objToSend);
               return;
            }
         }

         String hashCode = Utility.getObjectHashCode(objToSend);

         // check if the object is already parsed
         // if (Utility.containObject(alreadyParsedObject,objToSend))
         if (alreadyParsedObject.containsKey(hashCode))
         {
            upperNode.col2 = "see " + hashCode; //$NON-NLS-1$
            return;
         }

         // by default, display the hascode as the value
         upperNode.col2 = hashCode;

         // max level reached : display the hashCode, since ToString don't tell what object is
         if (maxLevel <= 1)
         {
            return;
         }

         // no more display this object content (array or fields)
         try
         {
            // this is the only place where object is added to alreadyParsedObject list
            // adding object to hashtable use the object hash code.
            // detect if the object hash code function generate exception
            alreadyParsedObject.put(hashCode, null); // only the key is needed
         } catch (Exception e)
         {
            // HasMap.put() can raise exception if the objToSend.getHash() raise exception
         }

         // send array
         if (objClass.isArray())
         {
            addArray(objToSend, upperNode, maxLevel, sendPrivate, alreadyParsedObject);
            return;
         }

         // send collections
         if (objToSend instanceof Collection)
         {
            addCollection((Collection) objToSend, upperNode, maxLevel, sendPrivate, alreadyParsedObject);
            return;
         }

         // send Map
         if (objToSend instanceof Map)
         {
            addMap((Map) objToSend, upperNode, maxLevel, sendPrivate, alreadyParsedObject);
            return;
         }

         addAllFieldsValue(objToSend, upperNode, maxLevel, sendPrivate, alreadyParsedObject);

      } catch (Exception e)
      {
         // eat exception
      }

   }

   // ----------------------------------------------------------------------

   private void innerAddTable(TMemberNode tableMembers, Object itemObject, boolean isFirstRow)
   {
      try
      {

         TMemberNode fCurrentRow = tableMembers.add(""); //$NON-NLS-1$
         
         if (itemObject == null)
         {
            fCurrentRow.col1 = "Null"; //$NON-NLS-1$
            return;
         }

         Class objClass = itemObject.getClass();

         // check primitive and well know type
         if (Utility.isPrimitive(objClass))
         {
            // Add Column Title if first line
            if (isFirstRow)
               tableMembers.col1 = "Values"; //$NON-NLS-1$
            fCurrentRow.col1 = Utility.getObjectString(itemObject); 
            return;
         }

         for (Iterator iter = TTrace.nativeClasses.iterator(); iter.hasNext();)
         {
            Class nativeClass = (Class) iter.next();
            if (nativeClass.isAssignableFrom(objClass))
            {
               fCurrentRow.col1 = Utility.getObjectString(itemObject);
               return;
            }
         }

         //String hashCode = Utility.getObjectHashCode(objToSend);

         // public + private members , no inherited members
         boolean isFirstCol = true ;  // bug fix 12.3
         Field[] declaredFieldsArray = objClass.getDeclaredFields();
         for (int c = 0; c <= declaredFieldsArray.length - 1; c++)
         {
            Field field = declaredFieldsArray[c];

            String memberName;
            Object memberValue;
            String strMemberValue ;

            try
            {
               memberName = field.getName();
               if (memberName.startsWith("this$")) //$NON-NLS-1$
                  continue;
               // Add Column Title if first line
               if (isFirstRow)
               {
                  if (tableMembers.col1 == "") //$NON-NLS-1$
                     tableMembers.col1 = memberName;
                  else
                     tableMembers.col1 = tableMembers.col1 + "\t" + memberName; //$NON-NLS-1$
               }
               // bypass the this property for inner class
               //if (memberName.startsWith("this$")) //$NON-NLS-1$
               //   return;

               
               // add data
               memberValue = Utility.getFieldValue(itemObject, field);
               
               if (memberValue == null)
                  strMemberValue = "null"; //$NON-NLS-1$
               else
                  strMemberValue = Utility.getObjectString(memberValue); 

               if (isFirstCol) // bug fix 12.3
                  fCurrentRow.col1 = strMemberValue;
               else
                  fCurrentRow.col1 = fCurrentRow.col1 + "\t" + strMemberValue; //$NON-NLS-1$               

               isFirstCol = false ;
            } catch (Exception e)
            {
               // eat exception
            }

         }


      } catch (Exception e)
      {
         // eat exception
      }

 }
   
   // ----------------------------------------------------------------------
  
   /**
    * Add table to node
    * @param list Object collection(Array / Collection / Map) to send
    */

   public void addTable(final Object list)
   {
      if (!this.enabled)
         return;

      // create table
      TMemberNode TableMembers;
      TableMembers = this.members.add(""); //$NON-NLS-1$
      TableMembers.viewerKind = TraceConst.CST_VIEWER_TABLE;

      // fill table
      boolean isFirstRow = true;
      if (list == null)
      {
         innerAddTable( TableMembers,  list,  isFirstRow) ;
         return;
      }       

      // send array
      Class objClass = list.getClass();
      if (objClass.isArray())
      {
         int arrLength = java.lang.reflect.Array.getLength(list);
         for (int c = 0; c < arrLength; c++)
         {
            Object itemObj = java.lang.reflect.Array.get(list, c);
            innerAddTable( TableMembers,  itemObj,  isFirstRow) ;  // no need to display indices, since it's a logical order
            isFirstRow = false;
         }
      } else if (list instanceof Collection) { // send collections      
         try
         {
            //TableMembers.col1 = "Values"; //$NON-NLS-1$ 
            Iterator it = ((Collection)list).iterator();
            
            while (it.hasNext())
            {
               Object itemObj = it.next();
               innerAddTable( TableMembers,  itemObj,  isFirstRow) ;  // no need to display indices, since it's a logical order
               isFirstRow = false;
            }
         } catch (Exception e)
         {
            // eat exception
         }
      } else if (list instanceof Map) { // send Map
         try
         {
            TableMembers.col1 = "Key" + "\t" + "Value"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            Set set = ((Map) list).entrySet();
            Iterator it = set.iterator();
            while (it.hasNext())
            {
               Map.Entry entry = (Map.Entry) it.next();
               String title;
               Object itemObj = entry.getKey();
               if (itemObj == null)
                  title = "[null]"; //$NON-NLS-1$
               else
                  title = "[" + Utility.getObjectString(itemObj) + "]"; //$NON-NLS-1$ //$NON-NLS-2$

               itemObj = entry.getValue();

               TableMembers.add(title + "\t" + Utility.getObjectString(itemObj)); //$NON-NLS-1$
 
            }
         } catch (Exception e)
         {
            // eat exception
         }
      } else {
         // not a collection : print object  
         innerAddTable( TableMembers,  list,  isFirstRow) ;
      }
   }

   // ----------------------------------------------------------------------
   /**
    * Add table to node
    * @param table xml table to send
    */

   public void addTable(TraceTable table)
   {
      if (!this.enabled)
         return;

      table.copyToNodeMembers(this.members); // copy member to node. Member viewer kind is already set
   }

   // ----------------------------------------------------------------------


   /**
    * Add all fields value to upper node
    * @param objToSend An array object to send
    * @param upperNode The upper node that display receive collection
    * @param maxLevel Max Level to display
    * @param sendPrivate flag to send private field
    * @param alreadyParsedObject List of already parsed objects
    */
   protected void addAllFieldsValue(final Object objToSend, final TMemberNode upperNode, final int maxLevel, final boolean sendPrivate, final HashMap alreadyParsedObject)
   {
      // recursive call
      addAllClassFieldsValue(objToSend, objToSend.getClass(), upperNode, maxLevel, sendPrivate, alreadyParsedObject);
   }

   // ----------------------------------------------------------------------

   /**
    * Add all private and public fields value to upper node and super Class (recursive function)
    * @param objToSend An array object to send
    * @param objClass Get fields for that class , not the objToSend class
    * @param upperNode The upper node that display receive collection
    * @param maxLevel Max Level to display
    * @param sendPrivate flag to send private field
    * @param alreadyParsedObject List of already parsed objects
    */
   private void addAllClassFieldsValue(final Object objToSend, final Class objClass, final TMemberNode upperNode, final int maxLevel, final boolean sendPrivate, final HashMap alreadyParsedObject)
   {
      // getDeclaredFields() don't return inherited private fields, we must then find them recursively.
      if (!objClass.getName().equals("java.lang.Object")) //$NON-NLS-1$
         addAllClassFieldsValue(objToSend, objClass.getSuperclass(), upperNode, maxLevel, sendPrivate, alreadyParsedObject);

      // public + private members , no inherited members
      Field[] declaredFieldsArray = objClass.getDeclaredFields();
      for (int c = 0; c <= declaredFieldsArray.length - 1; c++)
      {
         Field field = declaredFieldsArray[c];

         // bypass static fields
         //if (Modifier.isStatic(field.getModifiers()))
         //   continue;

         if (!Modifier.isPublic(field.getModifiers()) && !sendPrivate)
            continue;

         addFieldValue(objToSend, upperNode, maxLevel, sendPrivate, alreadyParsedObject, field);
      }
   }

   // ----------------------------------------------------------------------

   /**
    * Add a Field to a TMemberNode Node
    * @param objToSend The object to examine
    * @param upperNode The upper node (recursive add sub objects)
    * @param maxLevel The number of sub element to display.
    * @param sendPrivate flag to send private field
    * @param alreadyParsedObject List of already parsed objects
    * @param field The field to Add
    */
   private void addFieldValue(final Object objToSend, final TMemberNode upperNode, final int maxLevel, final boolean sendPrivate, final HashMap alreadyParsedObject, final Field field)
   {
      String memberModifier;
      String memberName;
      Object memberValue;

      try
      {
         memberName = field.getName();
         // bypass the this property for inner class
         if (memberName.startsWith("this$")) //$NON-NLS-1$
            return;

         // add class name if the field is not declared in the same class as objToSend
         if (field.getDeclaringClass() != objToSend.getClass())
            memberName = field.getDeclaringClass().getName() + "::" + memberName; //$NON-NLS-1$

         memberModifier = Utility.getFieldModifier(field);
         memberValue = Utility.getFieldValue(objToSend, field);

         if (memberModifier != "") //$NON-NLS-1$
            memberModifier += " "; //$NON-NLS-1$

         TMemberNode node = new TMemberNode(memberName, "", memberModifier); //$NON-NLS-1$
         upperNode.add(node);
         innerAddValue(memberValue, node, maxLevel - 1, sendPrivate, alreadyParsedObject);

      } catch (Exception e)
      {
         // eat exception
      }
   }

   // ----------------------------------------------------------------------

   /**
    * Add array object to a TMemberNode
    * @param objToSend An array object to send
    * @param upperNode The upper node that display receive collection
    * @param maxLevel Max Level to display
    * @param sendPrivate flag to send private field
    * @param alreadyParsedObject List of already parsed objects
    */
   protected void addArray(final Object objToSend, final TMemberNode upperNode, final int maxLevel, final boolean sendPrivate, final HashMap alreadyParsedObject)
   {
      int arrLength = java.lang.reflect.Array.getLength(objToSend);

      // no need to get component type. innerAddValue will display real object type

      // Class tempClass = objToSend.getClass().getComponentType();
      // String componentType ;
      // if (tempClass != null)
      // componentType = tempClass.getName(); // toString() ;
      // else
      // componentType = "unknow" ; ////$NON-NLS-1$

      for (int c = 0; c < arrLength; c++)
      {
         Object itemObj = java.lang.reflect.Array.get(objToSend, c);

         String title = "[" + Integer.toString(c) + "]"; //$NON-NLS-1$ //$NON-NLS-2$
         // create the node with just array indice title
         TMemberNode node = new TMemberNode(title);
         upperNode.add(node);
         innerAddValue(itemObj, node, maxLevel - 1, sendPrivate, alreadyParsedObject);

      }
   }

   // ----------------------------------------------------------------------

   /**
    * Add collection object to a TMemberNode
    * @param objToSend A Collection object to send
    * @param upperNode The upper node that display receive collection
    * @param maxLevel Max Level to display
    * @param sendPrivate flag to send private field
    * @param alreadyParsedObject List of already parsed objects
    */
   protected void addCollection(final Collection objToSend, final TMemberNode upperNode, final int maxLevel, final boolean sendPrivate, final HashMap alreadyParsedObject)
   {
      try
      {
         Iterator it = objToSend.iterator();
         int c = 0;
         while (it.hasNext())
         {
            Object itemObj = it.next();
            String title = "[" + Integer.toString(c) + "]"; //$NON-NLS-1$ //$NON-NLS-2$

            TMemberNode node = new TMemberNode(title);
            upperNode.add(node);
            innerAddValue(itemObj, node, maxLevel - 1, sendPrivate, alreadyParsedObject);

            c++;
         }
      } catch (Exception e)
      {
         // eat exception
      }
   }

   // ----------------------------------------------------------------------

   /**
    * Add collection object to a TMemberNode
    * @param map A Map object to send
    * @param upperNode The upper node that display receive collection
    * @param maxLevel Max Level to display
    * @param sendPrivate flag to send private field
    * @param alreadyParsedObject List of already parsed objects
    */
   protected void addMap(final Map map, final TMemberNode upperNode, final int maxLevel, final boolean sendPrivate, final HashMap alreadyParsedObject)
   {
      try
      {
         Set set = map.entrySet();
         Iterator it = set.iterator();
         while (it.hasNext())
         {
            Map.Entry entry = (Map.Entry) it.next();
            String title;
            Object itemObj = entry.getKey();
            if (itemObj == null)
               title = "[null]"; //$NON-NLS-1$
            else
               title = "[" + Utility.getObjectString(itemObj) + "]"; //$NON-NLS-1$ //$NON-NLS-2$

            itemObj = entry.getValue();

            TMemberNode node = new TMemberNode(title);
            upperNode.add(node);
            innerAddValue(itemObj, node, maxLevel - 1, sendPrivate, alreadyParsedObject);

         }
      } catch (Exception e)
      {
         // eat exception
      }
   }

   // ----------------------------------------------------------------------

   /**
    * Send an object or a Type
    * @param objToSend The object to send (can be null)
    * @param objClass The Class to examine (not null)
    */
   protected void addTypeObject(final Object objToSend, final Class objClass)
   {
      addTypeObject(objToSend, objClass, TTrace.options.getDefault());
   }

   // ----------------------------------------------------------------------

   /**
    * Send an object or a Type
    * @param objToSend The object to send (can be null)
    * @param objClass The Class to examine (not null)
    * @param flags Let you specify what to send
    */

   protected void addTypeObject(final Object objToSend, final Class objClass, final int flags)
   {
      if (!this.enabled)
         return;

      try
      {
         // detect null type
         if (objClass == null)
         {
            this.members.add("Null class"); //$NON-NLS-1$
            return;
         }

         addQuickInfo(objToSend, objClass, flags); // quick info
         addClassInfo(objClass, flags); // Class definition
         addFieldsInfo(objToSend, objClass, flags); // FIELDS
         addMethods(objClass, flags); // Constructors and methods
         addClassInfo2(objClass, flags); // declared classes, super classes and interfaces

      } catch (Exception e)
      {
         // eat exception
      }
   }

   // ----------------------------------------------------------------------

   /**
    * Display quick class information
    * @param objToSend an object to analyse
    * @param objClass the class type of the object
    * @param flags options Let you specify what to send
    * @return the quick info node
    */

   protected TMemberNode addQuickInfo(final Object objToSend, final Class objClass, final int flags)
   {
      // quick info
      TMemberNode result = null;
      String strModifier;

      try
      {
         if ((flags & TraceDisplayFlags.showModifiers) != 0)
         {
            // default modifier (no modifier specified) cannot be used outside
            // the package,but can be read using reflection !!!!!!
            if (objClass.getModifiers() == 0)
               strModifier = ""; //$NON-NLS-1$
            else
               strModifier = Modifier.toString(objClass.getModifiers()) + " "; //$NON-NLS-1$
         } else
         {
            strModifier = ""; //$NON-NLS-1$
         }

         if (objToSend == null)
            result = new TMemberNode(objClass.getName(), "null"); // ObjectGroup //$NON-NLS-1$
         else
         {
            if (objClass.isArray())
            {
               int arrLength = java.lang.reflect.Array.getLength(objToSend);

               String componentType;
               Class tempClass = objClass.getComponentType();

               if (tempClass != null)
                  componentType = tempClass.getName(); // toString() ;
               else
                  componentType = "unknow"; // //$NON-NLS-1$

               result = new TMemberNode(componentType + " [" + Integer.toString(arrLength) + "] "); //$NON-NLS-1$ //$NON-NLS-2$
            } else
            {
               String strName = objClass.getName();
               String strValue = Utility.getObjectString(objToSend);

               if (strValue == strName)
                  result = new TMemberNode(strModifier + strName);
               else
                  result = new TMemberNode(strModifier + strName, strValue);
            }
         }
         result.viewerKind = TraceConst.CST_VIEWER_OBJECT ;
         this.members.add(result);
      } catch (Exception e)
      {
         // eat exception
      }
      return result;
   }

   // ----------------------------------------------------------------------

   /**
    * Display full class info
    * @param objClass The class to display
    * @param flags Let you specify what to send
    */

   protected void addClassInfo(final Class objClass, final int flags)
   {
      // Class definition
      // ----------------

      if ((flags & TraceDisplayFlags.showClassInfo) == 0)
         return;

      try
      {
         Class tempClass;

         TMemberNode classGroup = new TMemberNode("Class information").setFontDetail(0, true); //$NON-NLS-1$
         classGroup.viewerKind = TraceConst.CST_VIEWER_OBJECT ;

         this.members.add(classGroup);
         classGroup.add(new TMemberNode("getName()", objClass.getName())); //$NON-NLS-1$

         // default modifier (no modifier specified) cannot be used outside the package,
         // but can be read using reflection !!!!!!
         if (objClass.getModifiers() == 0)
            classGroup.add(new TMemberNode("getModifiers()", "[default]")); //$NON-NLS-1$ //$NON-NLS-2$
         else
            classGroup.add(new TMemberNode("getModifiers()", Modifier.toString(objClass.getModifiers()))); //$NON-NLS-1$

         Package pkg = objClass.getPackage();
         if (pkg != null)
            classGroup.add(new TMemberNode("getPackage()", pkg.getName())); //$NON-NLS-1$

         if (objClass.isArray())
         {
            tempClass = objClass.getComponentType();

            String componentType;
            if (tempClass != null)
               componentType = tempClass.getName();
            else
               componentType = "unknow"; // //$NON-NLS-1$

            classGroup.add("Array of " + componentType); //$NON-NLS-1$
         }

         ClassLoader loader = objClass.getClassLoader();
         if (loader != null)
            classGroup.add(new TMemberNode("Class Loader", loader.toString())); //$NON-NLS-1$

         tempClass = objClass.getDeclaringClass();
         if (tempClass != null)
            classGroup.add(new TMemberNode("Declaring Class", tempClass.toString())); //$NON-NLS-1$

         java.security.ProtectionDomain domain = objClass.getProtectionDomain();
         if (domain != null)
            classGroup.add(new TMemberNode("Protection Domain", domain.toString())); //$NON-NLS-1$

         Object[] signers = objClass.getSigners();
         if (signers != null)
            for (int c = 0; c < signers.length - 1; c++)
               classGroup.add(new TMemberNode("Signer", signers[c].toString())); //$NON-NLS-1$
         if (objClass.isInterface())
            classGroup.add("isInterface", "true"); //$NON-NLS-1$ //$NON-NLS-2$
         if (objClass.isPrimitive())
            classGroup.add("isPrimitive", "true"); //$NON-NLS-1$ //$NON-NLS-2$

      } catch (Exception e)
      {
         // eat exception
      }
   }

   // ----------------------------------------------------------------------

   /**
    * Display Nested Types, super class and interfaces
    * @param objClass the class to display
    * @param flags Let you specify what to send
    */

   protected void addClassInfo2(final Class objClass, final int flags)
   {
      // Class information : declared classes, super classes and interfaces
      // nested type and super classes are visible when ShowClassInfo is set
      if ((flags & TraceDisplayFlags.showClassInfo) == 0)
         return;

      try
      {
         // Nested Types
         // ---------------

         TMemberNode nestedTypesGroup = new TMemberNode("Nested Types").setFontDetail(0, true); //$NON-NLS-1$
         nestedTypesGroup.viewerKind = TraceConst.CST_VIEWER_OBJECT ;

         Class[] classes;

         classes = objClass.getClasses();
         for (int c = 0; c <= classes.length - 1; c++)
            nestedTypesGroup.add("getClasses() [" + c + "]", classes[c].toString()); //$NON-NLS-1$ //$NON-NLS-2$

         classes = objClass.getDeclaredClasses();
         for (int c = 0; c <= classes.length - 1; c++)
            nestedTypesGroup.add("getDeclaredClasses() [" + c + "]", classes[c].toString()); //$NON-NLS-1$ //$NON-NLS-2$

         // show the group only if not empty
         if (nestedTypesGroup.members.size() != 0)
            this.members.add(nestedTypesGroup);

         // Super classes and interfaces
         // -----------------------------

         Class currentClass = objClass;
         TMemberNode group;

         TMemberNode inheritGroup = new TMemberNode("Super classes and interfaces").setFontDetail(0, true); //$NON-NLS-1$
         inheritGroup.viewerKind = TraceConst.CST_VIEWER_OBJECT ;

         this.members.add(inheritGroup);

         while (currentClass != Object.class)
         {
            if (currentClass == objClass)
               group = new TMemberNode("this : " + currentClass.getName()); //$NON-NLS-1$
            else
               group = new TMemberNode(currentClass.getName());

            inheritGroup.add(group);

            classes = currentClass.getInterfaces();
            for (int c = 0; c <= classes.length - 1; c++)
               group.add(classes[c].toString());

            currentClass = currentClass.getSuperclass();
         }
      } catch (Exception e)
      {
         // eat exception
      }
   }

   // ----------------------------------------------------------------------

   /**
    * Show class methods
    * @param objClass The class to display
    * @param flags Let you specify what to send
    */

   protected void addMethods(final Class objClass, final int flags)
   {
      String memberName;
      // Constructors and methods
      // ------------------------
      if ((flags & TraceDisplayFlags.showMethods) == 0)
         return;

      try
      {
         TMemberNode methodsGroup = new TMemberNode("Constructors and methods").setFontDetail(0, true); //$NON-NLS-1$
         methodsGroup.viewerKind = TraceConst.CST_VIEWER_OBJECT ;

         // non inherited (private and public) constructors
         // ------------------------
         Constructor[] declaredConstructors = objClass.getDeclaredConstructors();
         for (int c = 0; c <= declaredConstructors.length - 1; c++)
         {
            Constructor constructor = declaredConstructors[c];
            if (!Modifier.isPublic(constructor.getModifiers()) && (flags & TraceDisplayFlags.showNonPublic) != TraceDisplayFlags.showNonPublic)
               continue;
            memberName = constructor.toString();
            methodsGroup.add(new TMemberNode(memberName));
         }

         // inherited public constructor
         // ------------------------
         if ((flags & TraceDisplayFlags.showInheritedMembers) != 0)
         {
            Constructor[] constructors = objClass.getConstructors();
            for (int c = 0; c <= constructors.length - 1; c++)
            {
               Constructor constructor = constructors[c];
               if (constructor.getDeclaringClass() == objClass)
                  continue;
               memberName = constructor.getDeclaringClass().getName() + "::" + constructor.toString(); //$NON-NLS-1$
               methodsGroup.add(new TMemberNode(memberName));
            }
         }

         // non inherited (private and public) Methods
         // ------------------------
         Method[] declaredMethods = objClass.getDeclaredMethods();
         for (int c = 0; c <= declaredMethods.length - 1; c++)
         {
            Method method = declaredMethods[c];
            if (!Modifier.isPublic(method.getModifiers()) && (flags & TraceDisplayFlags.showNonPublic) != TraceDisplayFlags.showNonPublic)
               continue;
            memberName = method.toString();
            methodsGroup.add(new TMemberNode(memberName));
         }

         // inherited public methods
         // ------------------------
         if ((flags & TraceDisplayFlags.showInheritedMembers) != 0)
         {
            Method[] methods = objClass.getMethods();
            for (int c = 0; c <= methods.length - 1; c++)
            {
               Method method = methods[c];
               if (method.getDeclaringClass() == objClass)
                  continue;
               memberName = method.getDeclaringClass().getName() + "::" + method.toString(); //$NON-NLS-1$
               methodsGroup.add(new TMemberNode(memberName));
            }
         }

         // show the group only if not empty
         if (methodsGroup.members.size() != 0)
            this.members.add(methodsGroup);
      } catch (Exception e)
      {
         // eat exception
      }

   }

   // ----------------------------------------------------------------------

   /**
    * display fields information
    * @param objToSend The Object to display
    * @param objClass The object type to display
    * @param flags Let you specify what to send
    */

   protected void addFieldsInfo(final Object objToSend, final Class objClass, final int flags)
   {

      if ((flags & TraceDisplayFlags.showFields) == 0)
         return;

      try
      {
         String memberType;
         String memberModifier;
         String memberName;
         String memberValue;
         TMemberNode fieldsGroup = new TMemberNode("Fields").setFontDetail(0, true); //$NON-NLS-1$
         fieldsGroup.viewerKind = TraceConst.CST_VIEWER_OBJECT ;

         // public + private members , no inherited members
         Field[] declaredFieldsArray = objClass.getDeclaredFields();

         for (int c = 0; c <= declaredFieldsArray.length - 1; c++)
         {
            Field field = declaredFieldsArray[c];

            if (!Modifier.isPublic(field.getModifiers()) && ((flags & TraceDisplayFlags.showNonPublic) != TraceDisplayFlags.showNonPublic))
               continue;

            memberModifier = Utility.getFieldModifier(field);
            memberType = Utility.getFieldType(field);
            memberName = field.getName();

            if ((objToSend != null) || (Modifier.isStatic(field.getModifiers())))
               memberValue = Utility.getObjectString(Utility.getFieldValueEx(objToSend, field));
            else
               memberValue = "[null object, non static field]"; //$NON-NLS-1$

            fieldsGroup.add(new TMemberNode(memberName, memberValue, memberModifier + " " + memberType)); //$NON-NLS-1$

         }

         // public only members, with inherited members
         if ((flags & TraceDisplayFlags.showInheritedMembers) != 0)
         {
            Field[] fields = objClass.getFields();
            for (int c = 0; c <= fields.length - 1; c++)
            {
               Field field = fields[c];
               // discard already displayed members
               if (field.getDeclaringClass() == objClass)
                  continue;

               memberModifier = Modifier.toString(field.getModifiers());
               memberName = field.getDeclaringClass().getName() + "::" + field.getName(); //$NON-NLS-1$
               memberType = Utility.getFieldType(field);
               memberValue = Utility.getObjectString(Utility.getFieldValueEx(objToSend, field));

               fieldsGroup.add(new TMemberNode(memberName, memberValue, memberModifier + " " + memberType)); //$NON-NLS-1$
            }
         }
         // show the group only if not empty
         if (fieldsGroup.members.size() != 0)
            this.members.add(fieldsGroup);
      } catch (Exception e)
      {
         // eat exception
      }
   }

   // ----------------------------------------------------------------------
   /**
    * Fill the "member" tree with the object description
    * @param objToSend the object to send
    */
   public void addObject(final Object objToSend)
   {
      if (!this.enabled)
         return;
      addObject(objToSend, TTrace.options.getDefault());
   }

   // ----------------------------------------------------------------------

   /**
    * Call addObject to fill the "member" tree with the object value
    * @param objToSend the object to send
    * @param flags Let you specify what to send
    */
   public void addObject(final Object objToSend, final int flags)
   {
      if (!this.enabled)
         return;
      Class oType;
      if (objToSend == null)
         oType = null;
      else
         oType = objToSend.getClass();

      addTypeObject(objToSend, oType, flags); // add info to this.Members
   }

   // ----------------------------------------------------------------------

   /**
    * Add type information
    * @param typeToSend the type to inspect
    */
   public void addType(final Class typeToSend)
   {
      if (!this.enabled)
         return;
      addType(typeToSend, TTrace.options.getDefault());
   }

   // ----------------------------------------------------------------------

   /**
    * Add type information
    * @param typeToSend the type to inspect
    * @param flags Let you specify what to send
    */
   public void addType(final Class typeToSend, final int flags)
   {
      if (!this.enabled)
         return;
      addTypeObject(null, typeToSend, flags); // add info to this.Members
   }

   // ----------------------------------------------------------------------

   /**
    * Display dump
    * @param shortTitle A short title displayed on top of the dump
    * @param bytes The byte buffer to dump
    * @param count Number of byte to dump
    */
   public void addDump(final String shortTitle, final byte[] bytes, final int count)
   {
      addDump(shortTitle, bytes, 0, count);
   }

   // ----------------------------------------------------------------------

   /**
    * Display dump
    * @param shortTitle A short title displayed on top of the dump
    * @param bytes The byte buffer to dump
    * @param index first byte
    * @param count Number of byte to dump
    */
   public void addDump(final String shortTitle, final byte[] bytes, final int index, final int count)
   {
      if (!this.enabled)
         return;

      int c = index;
      int byteDumped = 0;

      try
      {
         TMemberNode dumpGroup = new TMemberNode(shortTitle).setFontDetail(0, true);
         dumpGroup.viewerKind = TraceConst.CST_VIEWER_DUMP ;
         this.members.add(dumpGroup);

         while (byteDumped < count && (c < bytes.length))
         {
            int d = 0; // inner loop. From 0 to 15 max
            int beginLine = c; // used to print the offset
            StringBuffer hexaRepresentation = new StringBuffer(""); //$NON-NLS-1$
            //StringBuffer strRepresentation = new StringBuffer(""); //$NON-NLS-1$
            StringBuffer adr = new StringBuffer(""); //$NON-NLS-1$

            while ((byteDumped < count) && (d < 16) && (c < bytes.length))
            {
               byte oneByte = bytes[c];
               int oneInt = oneByte; // (int)

               String str = Integer.toHexString(oneInt).toUpperCase();
               if (str.length() < 2)
                  hexaRepresentation.append('0');
               if (str.length() > 2)
                  str = str.substring(str.length()-2, str.length()) ;

               hexaRepresentation.append(str);
               hexaRepresentation.append(" "); //$NON-NLS-1$

               // since 12.4 : string representation is no more send to viewer. The viewer will calculate hitself the string
               //char oneChar = (char) bytes[c];

               // only the zero cannot be copied to the stream
               //if (oneChar == 0)
               //   strRepresentation.append('.');
               //else
               //   strRepresentation.append(oneChar); // (char)

               byteDumped++;
               d++;
               c++;
            }
            adr.append(Integer.toHexString(beginLine).toUpperCase());

            Utility.leftPadding(adr, 6, '0');
            dumpGroup.add(adr.toString(), hexaRepresentation.toString()) ; //, strRepresentation.toString());
         }
         dumpGroup.col2 = Integer.toString(byteDumped) + " byte(s) dumped"; //$NON-NLS-1$
      } catch (Exception e)
      {
         // eat exception
      }
   }

   // ----------------------------------------------------------------------

   /**
    * add stack to the Members
    */
   public void addStackTrace()
   {
      if (!this.enabled)
         return;
      addStackTrace(1);
   }

   // ----------------------------------------------------------------------

   // / display the stack
   /**
    * add stack trace
    * @param level Level to use (0 is the current function)
    */
   public void addStackTrace(final int level)
   {
      if (!this.enabled)
         return;

      try
      {
         TMemberNode group = new TMemberNode("Call stack").setFontDetail(0, true); //$NON-NLS-1$
         group.viewerKind = TraceConst.CST_VIEWER_STACK ;
         this.members.add(group);

         // first line is "java.lang.Throwable"
         // second line is "TraceNode.addStackTrace"
         // third line is "TraceNodeEx.addStackTrace"

         new Throwable().printStackTrace(new StackWriter(group, level + 3, false)); // false : all call stack line

      } catch (Exception e)
      {
         // eat exception
      }
   }

   // ----------------------------------------------------------------------

   /**
    * show caller information. <p>
    * level 0 correspond at the first line of StackTrace(0) <p>
    * It's like the call stack, but display only 1 line
    * @param level starting level
    */
   public void addCaller(final int level)
   {
      if (!this.enabled)
         return;
      try
      {
         TMemberNode group = new TMemberNode("Caller information").setFontDetail(0, true); //$NON-NLS-1$
         group.viewerKind = TraceConst.CST_VIEWER_STACK ;
         this.members.add(group);

         new Throwable().printStackTrace(new StackWriter(group, level + 3, true)); // true : singleline

      } catch (Exception e)
      {
         // eat exception
      }
   }

   // ----------------------------------------------------------------------

   /**
    * show caller information. (Level is 1)
    */
   public void addCaller()
   {
      addCaller(1); // skip itself
   }


   // ----------------------------------------------------------------------

   /**
    * Add xml text
    * @param xml xml text to send
    */
   public void addXML(final String xml)
   {
      if (!this.enabled)
         return;

      TMemberNode member = this.members.add(xml);
      member.viewerKind = TraceConst.CST_VIEWER_XML;
   }


   //------------------------------------------------------------------------------

   /**
    * Change background font color
    * @param color xml background color
    */

   public void addBackgroundColor(final Color color)
   {
      if (!this.enabled)
         return;
      addBackgroundColor(color,-1);
   }

   //------------------------------------------------------------------------------
   /**
    * Change background font color
    * @param color xml background color
    * @param colId Column index : All columns= -1,Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    */

   public void addBackgroundColor(final Color color, final int colId)
   {
      if (!this.enabled)
         return;
      FontDetail fontDetail = new FontDetail();
      fontDetail.colId = colId;
      fontDetail.color = color;    // store the color and convert it to BGR when the node is send
      fontDetail.fontName = "BackgroundColor";  // special name. Indicate that color is for background, not font itself //$NON-NLS-1$
      this.fontDetails.add(fontDetail);
   }

   //----------------------------------------------------------------------

   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    * @param bold Change font to bold
    * @return The trace node
    */
   public TraceNodeEx addFontDetail(final int colId, final boolean bold)
   {
      return addFontDetail(colId, bold, false, null, 0, ""); //$NON-NLS-1$
   }

   // ----------------------------------------------------------------------
   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @return The trace node
    */
   public TraceNodeEx addFontDetail(final int colId, final boolean bold, final boolean italic)
   {
      return addFontDetail(colId, bold, italic, null, 0, ""); //$NON-NLS-1$
   }

   // ----------------------------------------------------------------------
   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @param color Change Color
    * @return The trace node
    */
   public TraceNodeEx addFontDetail(final int colId, final boolean bold, final boolean italic, final Color color)
   {
      return addFontDetail(colId, bold, italic, color, 0, ""); //$NON-NLS-1$
   }

   // ----------------------------------------------------------------------
   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @param color Change Color
    * @param size Change font size, use zero to keep normal size
    * @return The trace node
    */
   public TraceNodeEx addFontDetail(final int colId, final boolean bold, final boolean italic, final Color color, final int size)
   {
      return addFontDetail(colId, bold, italic, color, size, ""); //$NON-NLS-1$
   }

   // ----------------------------------------------------------------------
   /**
    * Change font detail for an item in the trace
    * @param colId Column index : All columns=-1, Icon=0, Time=1, thread=2, left msg=3, right msg =4 or user defined column
    * @param bold Change font to bold
    * @param italic Change font to Italic
    * @param color Change Color
    * @param size Change font size, use zero to keep normal size
    * @param fontName Change font name
    * @return The trace node
    */
   public TraceNodeEx addFontDetail(final int colId, final boolean bold, final boolean italic, final Color color, final int size, final String fontName)
   {
      if (!this.enabled)
         return this ;

      FontDetail fontDetail = new FontDetail();
      fontDetail.colId = colId;
      fontDetail.bold = bold;
      fontDetail.italic = italic;
      fontDetail.color = color;        // store the color and convert it to BGR when the node is send
      fontDetail.size = size;
      fontDetail.fontName = fontName;

      if (this.fontDetails == null)
         this.fontDetails = new ArrayList();

      this.fontDetails.add(fontDetail);
      return this;
   }

   // ----------------------------------------------------------------------

   /**
    * Send the trace to the server (left + right + members)
    * @return a traceNode
    */
   public TraceNode send()
   {
      TraceNode result = new TraceNode(this); // create a copy
      if (!this.enabled)
         return result;
      ArrayList commandList = new ArrayList();

      Utility.addCommand(commandList, TraceConst.CST_NEW_NODE, this.parentNodeId); // param : parent Node id (this)
      Utility.addCommand(commandList, TraceConst.CST_TRACE_ID, this.id); // param : Node id
      if (this.leftMsg != null)
         Utility.addCommand(commandList, TraceConst.CST_LEFT_MSG, this.leftMsg); // param : left string
      if (this.rightMsg != null)
         Utility.addCommand(commandList, TraceConst.CST_RIGHT_MSG, this.rightMsg); // param : right string

      Utility.addCommand(commandList, TraceConst.CST_ICO_INDEX, this.iconIndex); // param : Icon index

      // add font detail
      if (this.fontDetails != null)
      {
         for (int c = 0; c < this.fontDetails.size(); c++)
         {
            FontDetail fontDetail = (FontDetail) this.fontDetails.get(c);

            // Color is coded as RGB. convert to BGR
            int colorValue;
            if (fontDetail.color == null)
               colorValue = -1;
            else
               colorValue = (fontDetail.color.getBlue() << 16) | (fontDetail.color.getGreen() << 8) | (fontDetail.color.getRed() << 0);

            StringBuffer tempStr = new StringBuffer();

            if (fontDetail.fontName.compareTo("BackgroundColor") == 0) //$NON-NLS-1$
            {
               //special color : background color
               Utility.addCommand(commandList, TraceConst.CST_BACKGROUND_COLOR, colorValue, Utility.intToString(fontDetail.colId));      // param : color, colId
            } else {
               tempStr.append(Utility.intToStr5(TraceConst.CST_FONT_DETAIL)).append(Utility.intToStr3(fontDetail.colId));

               if (fontDetail.bold)
                  tempStr.append("1"); //$NON-NLS-1$
               else
                  tempStr.append("0"); //$NON-NLS-1$

               if (fontDetail.italic)
                  tempStr.append("1"); //$NON-NLS-1$
               else
                  tempStr.append("0"); //$NON-NLS-1$

               tempStr.append(Utility.intToStr11(colorValue)).append(Utility.intToStr11(fontDetail.size)).append(fontDetail.fontName);
               commandList.add(tempStr.toString());   // don't use Utility.addCommand(commandList,...)
            }
         }
         this.fontDetails.clear();
         this.fontDetails = null; // once copied to commandlist, clear the array
      }

      this.members.addToStringList(commandList); // convert all groups and nested items/group to strings

      TTrace.sendToWinTraceClient(commandList, this.winTraceId, this.time, this.threadName);
      return result;
   }

   //----------------------------------------------------------------------

   /**
    * Re-send the trace to the viewer (only left and right message)
    */
   public void resend()
   {
      if (!this.enabled)
         return;

      ArrayList commandList = new ArrayList();

      Utility.addCommand(commandList, TraceConst.CST_USE_NODE, this.id); // param : guid
      Utility.addCommand(commandList, TraceConst.CST_LEFT_MSG, this.leftMsg); // param : left string
      Utility.addCommand(commandList, TraceConst.CST_RIGHT_MSG, this.rightMsg); // param : right string

      // don't re-send members and icon
      TTrace.sendToWinTraceClient(commandList, this.winTraceId);
   }
}
