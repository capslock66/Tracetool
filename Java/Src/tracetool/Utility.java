/*
 * Utility.java
 *
 * HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
 * Download :  http://sourceforge.net/projects/tracetool/
 * See License.txt for license information
 *
 * Author : Thierry Parent
 * Version : 12.4.1
 *
 * Provide some utility functions
 */

package tracetool;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;

/**
 * Internal Use.<p>
 * That class is useful for you if you want to learn java reflection
 * @author tpa
 *
 * Utility class 
 */
public final class Utility
{
   /**  no public constructor */
   private Utility()
   {
      // no public constructor
   }

   //----------------------------------------------------------------------

   /**
    * A string representation of field type
    * @param field Field
    * @return a string
    */
   public static String getFieldType(final Field field)
   {
      Class fieldType;
      fieldType = field.getType();
      if (fieldType.isArray())
         return fieldType.getComponentType().getName() + " []"; //$NON-NLS-1$

      return fieldType.getName();
   }

   //----------------------------------------------------------------------

   /**
    * check if a class is a primitve
    * @param fieldType the class to check
    * @return true if a primitive or date
    */
   public static boolean isPrimitive (final Class fieldType)
   {
      try
      {
         // to discard checkstyle warning (too much tests), tests are grouped by type

         // strings
         if (fieldType == byte.class
          || fieldType == String.class
          || fieldType == StringBuffer.class
          || fieldType == char.class)
            return true ;

         // floating point
         if (fieldType == float.class
          || fieldType == double.class)
            return true ;

         // integer
         if (fieldType == short.class
          || fieldType == Integer.class  // final class
          || fieldType == int.class
          || fieldType == java.math.BigInteger.class
          || fieldType == java.math.BigDecimal.class
          || fieldType == long.class)
            return true ;

         // other
         if (fieldType == void.class
          || fieldType == boolean.class
          || fieldType == Boolean.class
          || fieldType == java.lang.Class.class)
            return true ;
      } catch (Exception e) {
         // eat exception
      }
      return false;
   }

   //----------------------------------------------------------------------

   /**
    * Value of a field with string representation if native or date type
    * @param objToSend Object to read
    * @param field Field to read
    * @return An object
    */
   public static Object getFieldValueEx(final Object objToSend, final Field field)
   {
      Object oMemberValue = null ;
      try
      {
         // bypass 'Private' limitation.
         // Thanks to Guy De Tremerie for the tips :-)
         field.setAccessible(true);

         // detect primitive or Java.Lang.class
         Class fieldType;
         fieldType = field.getType();
         if (isPrimitive(fieldType))
            return field.get(objToSend).toString();

         oMemberValue = field.get(objToSend);

         // handle other kind of well know object
         if (oMemberValue instanceof java.util.Date)
            oMemberValue = oMemberValue.toString();

      } catch (IllegalAccessException e) {
         oMemberValue = "[Illegal Access Exception]"; //$NON-NLS-1$
      } catch (NullPointerException e) {
         oMemberValue = ""; //$NON-NLS-1$   // [Null Pointer Exception]
      }

      if (oMemberValue == null)
         return "null"; //$NON-NLS-1$

      return oMemberValue;
   }

   //----------------------------------------------------------------------

   /**
    * Value of a field
    * @param objToSend Object to read
    * @param field Field to read
    * @return An object
    */
   public static Object getFieldValue(final Object objToSend, final Field field)
   {
      Object oMemberValue = null ;
      try
      {
         // bypass 'Private' limitation.
         // Thanks to Guy De Tremerie for the tips :-)
         field.setAccessible(true);

         oMemberValue = field.get(objToSend);

      } catch (IllegalAccessException e) {
         oMemberValue = "[Illegal Access Exception]"; //$NON-NLS-1$
      } catch (NullPointerException e) {
         oMemberValue = ""; //$NON-NLS-1$   // [Null Pointer Exception]
      }

      if (oMemberValue == null)
         return "null"; //$NON-NLS-1$

      return oMemberValue;
   }

   //----------------------------------------------------------------------

   /**
    * return Object toString() value. This function is used to bypass bad hashCode implementations
    * @param obj Suspected Object
    * @return toString() value
    */
   public static String getObjectString (final Object obj)
   {
      try
      {
         if (obj == null)
            return "null" ; //$NON-NLS-1$
         return obj.toString() ;
      } catch (Exception e) {
         return "toString() exception" ; //$NON-NLS-1$
      }
   }

   //----------------------------------------------------------------------

   //   /**
   //    * This function is used to bypass bad hashCode implementations
   //    * @param alreadyParsedObject HashMap list to test suspect object
   //    * @param obj Suspected Object
   //    * @return HashMap.containsKey()
   //    */
   //   public static boolean containObject(final HashMap alreadyParsedObject, final Object obj)
   //   {
   //      try
   //      {
   //         // HashMap.containsKey on a Set object call his hashCode() method.
   //         // Set.hashCode() call all sub elements hashCode()
   //         // if one element return exception on the hashCode(), we can't add this object to alreadyParsedObject.
   //
   //         return alreadyParsedObject.containsKey(obj) ;
   //      } catch (Exception e) {
   //         return true ;  // bad object are considered as already in the list
   //      }
   //   }

   //----------------------------------------------------------------------

   /**
    * return Hexa hashcode. This function is used to bypass bad hashCode implementations
    * @param obj Suspected Object
    * @return hexa hashCode
    */
   public static String getObjectHashCode (final Object obj)
   {
      Class objType = obj.getClass() ;
      String typeName = objType.getName() ;
      int packageLenght = objType.getPackage().getName().length() ;
      // remove package name
      if (packageLenght != 0)
         typeName = typeName.substring(packageLenght + 1) ;

      try
      {
         return new StringBuffer().append(typeName).append("@").append(Integer.toHexString(obj.hashCode())).toString() ;   //$NON-NLS-1$
      } catch (Exception e) {  // obj.hashCode() exception
         return new StringBuffer().append(typeName).append("@Error").toString()  ;   //$NON-NLS-1$
      }
   }

   //----------------------------------------------------------------------
   /**
    * A string reprenstation of a field
    * @param field Field
    * @return a string
    */
   public static String getFieldModifier(final Field field)
   {
      if (field.getModifiers() == 0)
         return "[default]"; //$NON-NLS-1$

      return Modifier.toString(field.getModifiers());
   }

   //----------------------------------------------------------------------

   /**
    * convert an integer to a string.
    * @param param An integer to convert
    * @return a string representation
    * @see #intToString(int, int) for formatted string
    */
   public static String intToString (final int param)
   {
      return Integer.toString (param) ;
   }

   //----------------------------------------------------------------------

   /**
    * convert an integer to a string (left padding).
    * @param param  An integer to convert
    * @param len The string length
    * @return a string representation
    */
   public static String intToString (final int param , final int len)
   {
      return intToStringBuffer (param , len).toString () ;
   }

   //----------------------------------------------------------------------

   /**
    * convert an integer to 5 chars
    * @param param An integer to convert
    * @return a String of 5 chars
    */

   public static String intToStr5 (final int param)
   {
      StringBuffer sb = new StringBuffer (9) ;   // 4 + possible 5 chars
      sb.append("    ").append(Integer.toString (param)) ; //$NON-NLS-1$
      return sb.substring(sb.length() - 5) ;
   }

   //----------------------------------------------------------------------

   /**
    * convert an integer to 3 chars
    * @param param An integer to convert
    * @return a String of 3 chars
    */

   public static String intToStr3 (final int param)
   {
      StringBuffer sb = new StringBuffer (5) ;   // 2 + possible 3 chars
      sb.append("  ").append(Integer.toString (param)) ; //$NON-NLS-1$
      return sb.substring(sb.length() - 3) ;
   }

   //----------------------------------------------------------------------
   /**
    * convert an integer to 11 chars
    * @param param An integer to convert
    * @return a String of 11 chars
    */

   public static String intToStr11 (final int param)
   {
      StringBuffer sb = new StringBuffer (21) ;   // 10 + possible 11 chars
      sb.append("          ").append(Integer.toString (param)) ; //$NON-NLS-1$
      return sb.substring(sb.length() - 11) ;
   }

   //----------------------------------------------------------------------

   /**
    * convert an integer to a StringBuffer (left padding).
    * @param param  An integer to convert
    * @param len The string length
    * @return a string representation
    */
   public static StringBuffer intToStringBuffer (final int param , final int len)
   {
      StringBuffer temp = new StringBuffer (Integer.toString (param)) ;
      leftPadding (temp, len , ' ') ;
      return temp ;
   }

   //----------------------------------------------------------------------

   /**
    * Left Pad Stringbuffer with special char
    * @param strBuf Target buffer
    * @param bufLen Buffer length
    * @param fill Char to fill
    * @return a padded StringBuffer
    */
   public static StringBuffer leftPadding (final StringBuffer strBuf, final int bufLen , final char fill)
   {
      while (strBuf.length () < bufLen)
         strBuf.insert (0, fill) ;
      return strBuf ;
   }

   //----------------------------------------------------------------------
   private static long   previousTime;
   private static char[] previousTimeWithoutMillis = new char[19]; // "YYYYMMDD HH:mm:ss."
   private static int    previousTimeLength = 0 ;
   private static boolean previousDateAdded = false ;

   /**
    * return the current time in the format "HH:mm:ss:SSS" for example, "15:49:37:459" <p>
    * @return formatted time
    */
   public static String currentTime()
   {
      return formatTime(0) ;
   }

   //------------------------------------------------------------------------------

   /**
    * return the time in the format "HH:mm:ss:SSS" for example, "15:49:37:459" <p>
    * This code was taken from Log4J source code :-)
    * @param time the time to format
    * @return formatted time
    */
   public static String formatTime (long time)
   {
      StringBuffer sbuf = new StringBuffer() ;

      boolean addDate = TTrace.options.sendDate ;

      // detect that the date is added or deleted
      if (addDate != previousDateAdded )
         previousTime = 0 ;

      if (time == 0)
         time = new Date ().getTime () ;

      int millis = (int) (time % 1000);

      if ((time - millis) != previousTime)
      {
         // We reach this point at most once per second
         // across all threads instead of each time format()
         // is called. This saves considerable CPU time.

         Calendar calendar = Calendar.getInstance() ;
         calendar.setTime(new Date(time)) ;
         //calendar.setTimeInMillis(time) ;  // setTimeInMillis work only in 1.4 and later

         int temp ;
         if (addDate)
         {
            sbuf.append (calendar.get (Calendar.YEAR));
            temp = calendar.get (Calendar.MONTH);
            if (temp < 10)
               sbuf.append ('0');
            sbuf.append (temp);
            temp = calendar.get (Calendar.DAY_OF_MONTH);
            if (temp < 10)
               sbuf.append ('0');
            sbuf.append (temp);
            sbuf.append (' ');
            previousTimeLength = 18 ;
         } else {
            previousTimeLength = 9 ;
         }
         previousDateAdded = addDate ;

         temp = calendar.get (Calendar.HOUR_OF_DAY);
         if (temp < 10)
            sbuf.append ('0');
         sbuf.append (temp);
         sbuf.append (':');

         temp = calendar.get (Calendar.MINUTE);
         if (temp < 10)
            sbuf.append ('0');
         sbuf.append (temp);
         sbuf.append (':');

         temp = calendar.get (Calendar.SECOND);
         if (temp < 10)
            sbuf.append ('0');
         sbuf.append (temp);
         sbuf.append (':');

         int len = sbuf.length () ;
         // store the time string for next time to avoid recomputation
         sbuf.getChars (0, len, previousTimeWithoutMillis, 0);

         previousTime = time - millis;
      }
      else
      {
         sbuf.append (previousTimeWithoutMillis, 0, previousTimeLength);
      }

      if (millis < 100)
         sbuf.append ('0');
      if (millis < 10)
         sbuf.append ('0');

      sbuf.append (millis);
      return sbuf.toString ();
   }

   //------------------------------------------------------------------------------

   /**
    * Add command code
    * @param commandList The target array
    * @param code Command ID
    */
   public static void addCommand(final ArrayList commandList, final int code)
   {
      String str = Utility.intToStr5(code);
      commandList.add(str);
   }

   //------------------------------------------------------------------------------
   /// code + int
   /**
    * Add command code + int value
    * @param commandList The target array
    * @param code Command ID
    * @param intvalue Int parameter
    */
   public static void addCommand(final ArrayList commandList, final int code, final int intvalue)
   {
      String str = Utility.intToStr5(code) + Utility.intToStr11(intvalue);
      commandList.add(str);
   }

   //------------------------------------------------------------------------------
   /// code + int
   /**
    * Add command code + int value
    * @param commandList The target array
    * @param code Command ID
    * @param boolvalue boolean parameter
    */
   public static void addCommand(final ArrayList commandList, final int code, final boolean boolvalue)
   {
      String str ;
      if (boolvalue == true)
         str = Utility.intToStr5(code) + Utility.intToStr11(1);
      else
         str = Utility.intToStr5(code) + Utility.intToStr11(0);
      commandList.add(str);
   }

   //------------------------------------------------------------------------------
   /// code + string
   /**
    * Add command code + string value
    * @param commandList The target array
    * @param code Command ID
    * @param strvalue String parameter
    */
   public static void addCommand(final ArrayList commandList, final int code, final String strvalue)
   {
      if (strvalue == null )
         commandList.add(Utility.intToStr5(code));
      else
         commandList.add(Utility.intToStr5(code) + strvalue);
   }

   //------------------------------------------------------------------------------
   /// code + int + string
   /**
    * Add command code + int and string value
    * @param commandList The target array
    * @param code Command ID
    * @param intvalue Int parameter
    * @param strvalue String parameter
    */
   public static void addCommand(final ArrayList commandList, final int code, final int intvalue, final String strvalue)
   {
      if (strvalue == null )
         commandList.add(Utility.intToStr5(code) + Utility.intToStr11(intvalue));
      else
         commandList.add(Utility.intToStr5(code) + Utility.intToStr11(intvalue) + strvalue);
   }

   //------------------------------------------------------------------------------
   /// code + int + int + string
   /**
    * Add command code + int and string value
    * @param commandList The target array
    * @param code Command ID
    * @param intvalue1 Int parameter 1
    * @param intvalue2 Int parameter 2
    * @param strvalue String parameter 3
    */
   public static void addCommand(final ArrayList commandList, final int code, final int intvalue1, final int intvalue2, final String strvalue)
   {
      if (strvalue == null )
         commandList.add(Utility.intToStr5(code) + Utility.intToStr11(intvalue1) + Utility.intToStr11(intvalue2));
      else
         commandList.add(Utility.intToStr5(code) + Utility.intToStr11(intvalue1) + Utility.intToStr11(intvalue2) + strvalue);
   }

   //------------------------------------------------------------------------------
   /// code + int + int + int + string
   /**
    * Add command code + int and string value
    * @param commandList The target array
    * @param code Command ID
    * @param intvalue1 Int parameter 1
    * @param intvalue2 Int parameter 2
    * @param intvalue3 Int parameter 3
    * @param strvalue String parameter 4
    */
   public static void addCommand(final ArrayList commandList, final int code, final int intvalue1, final int intvalue2, final int intvalue3 , final String strvalue)
   {
      if (strvalue == null )
         commandList.add(Utility.intToStr5(code) + Utility.intToStr11(intvalue1) + Utility.intToStr11(intvalue2) + Utility.intToStr11(intvalue3));
      else
         commandList.add(Utility.intToStr5(code) + Utility.intToStr11(intvalue1) + Utility.intToStr11(intvalue2)  + Utility.intToStr11(intvalue3) + strvalue);
   }

   //------------------------------------------------------------------------------
   /**
    * Html encode, with StringBuffer as target (faster)
     * @param source
     * @param target
    */
    public static void htmlEncode(final String source, StringBuffer target)
    {
       if (source == null || source.length() == 0)  
          return ;

       int length = source.length();

       char[] charArray = new char[length] ;
       source.getChars(0, length, charArray, 0) ;
       
       int startIndex = 0;
       int currentIndex = 0;
       while (currentIndex < length)
       {

          char ch = charArray[currentIndex];
          int oneInt = ch;
          switch (ch)
          {
             case '<':
                if (startIndex < currentIndex)
                   target.append(charArray, startIndex, currentIndex - startIndex);
                target.append("&lt;");       //$NON-NLS-1$
                startIndex = currentIndex + 1;
                break;

              case '>':
                if (startIndex < currentIndex)
                   target.append(charArray, startIndex, currentIndex - startIndex);
                target.append("&gt;");       //$NON-NLS-1$
                startIndex = currentIndex + 1;
                break;

             case '"':
                if (startIndex < currentIndex)
                   target.append(charArray, startIndex, currentIndex - startIndex);
                target.append("&quot;");     //$NON-NLS-1$
                startIndex = currentIndex + 1;
                break;

             case '&':
                if (startIndex < currentIndex)
                   target.append(charArray, startIndex, currentIndex - startIndex);
                target.append("&amp;");      //$NON-NLS-1$
                startIndex = currentIndex + 1;
                break;

             default :
                if (oneInt > 65533) // char > 65533 cannot be converted to &#nnn; Use a formal text ?nnnn 
                   target.append("?").append(intToString(ch)); //$NON-NLS-1$
                else if ((ch < ' ') || ((oneInt >= 160) ))  // 0 to 31 and 160 to 256   // && (ch < 'A')
                {
                   if (startIndex < currentIndex)
                      target.append(charArray, startIndex, currentIndex - startIndex);
                   target.append("&#");                  //$NON-NLS-1$
                   target.append(intToString(ch));
                   target.append(';');
                   startIndex = currentIndex + 1;
                } // else no encoding
                break;
          } // switch
          currentIndex++ ;
       }
       target.append(charArray, startIndex, length - startIndex);
    }

}
