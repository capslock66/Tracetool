/*
 * Log4JAppender.java
 *
 * Note : No more linked by default in tracetool.jar
 *
 * HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
 * Download :  http://sourceforge.net/projects/tracetool/
 * See License.txt for license information
 *
 * Author : Thierry Parent
 * Version : 12.4.1
 */

package tracetool;

// Appender variable :
// -------------------
//   ImmediateFlush
//   SendLocationInfo
//   RemoteHost,RemotePort
//   SendPrivateObjectInfo

// you must install Log4J to compile this file.
// remove Log4JAppender.java if you want to build only the framework
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Layout;
import org.apache.log4j.spi.LocationInfo;
import org.apache.log4j.spi.LoggingEvent;

/*
 * * Log4J Appender to send Log4J traces to the viewer. <PRE> <b>Sample log4J.properties property file :</b> log4j.rootLogger=INFO, JTRACETOOL1 log4j.appender.JTRACETOOL1=tracetool.Log4JAppender
 * log4j.appender.JTRACETOOL1.layout=org.apache.log4j.PatternLayout log4j.appender.JTRACETOOL1.layout.ConversionPattern=[%x]\t%r\t%d{yyyy/MM/dd HH:mm:ss:SSS}\t%c{2}\t%p\t[%t]\t%m%n # optional properties : log4j.appender.JTRACETOOL1.TitleLayout=[NDC]
 * \t Time \t Date Time \t logger \t Level \t [Thread] \t Message log4j.appender.JTRACETOOL1.ImmediateFlush=true log4j.appender.JTRACETOOL1.SendPrivateObjectInfo=true log4j.appender.JTRACETOOL1.SendLocationInfo=false
 * log4j.appender.JTRACETOOL1.RemoteHost=LocalHost log4j.appender.JTRACETOOL1.RemotePort=8090 log4j.appender.JTRACETOOL1.WinTraceId=Hello log4j.appender.JTRACETOOL1.WinTraceTitle=Hello log4j.appender.JTRACETOOL1.LogFile=1,log.xml
 * log4j.appender.JTRACETOOL1.MaxLevel=3 </PRE>
 * @author tpa
 */
/*
 * *
 * @author Thierry Parent
 */
public class Log4JAppender extends AppenderSkeleton
{
   /*
    * * target tabsheet
    */
   private WinTrace log4WinTrace ;

   /** Wintrace ID  */
   private String winTraceId ;

   /** Wintrace title  */
   private String winTraceTitle ;

   /** Immediate flush means that the traces will be flushed at the end of each append operation.  */
   private boolean immediateFlush = false;

   /** send log4j Location Information (filename,method and line number).   */
   private boolean sendLocationInfo = false;

   /** multicolumn Title to display  */
   private String titleLayout ;

   /** When Object value is send, Private fields can be filtered */
   private boolean sendPrivateObjectInfo = false ;   // TTrace.options.sendPrivate

   /** max level to display into View Trace Info */
   private int maxLevel = 3;   // TTrace.options.objectTreeDepth

   /** Log file name */
   private String logFileName ;

   /** Log file type */
   private int logMode ;

   //--------------------------------------------------------------------------------
   /** Creates a new instance of Log4JAppender */
   public Log4JAppender ()
   {
      //TTrace.debug().send("Log4JAppender constructor") ;
      this.immediateFlush = false ;
      this.logMode = -1 ;
      this.maxLevel = TTrace.options.objectTreeDepth ;
      this.sendPrivateObjectInfo = TTrace.options.sendPrivate ;
   }

   //--------------------------------------------------------------------------------

   /**
    * Append an event.
    * @param event The event to send
    * @see org.apache.log4j.AppenderSkeleton
    */
   public void append (final LoggingEvent event)
   {
      try
      {
         // if null then first append.
         if (this.log4WinTrace == null)
         {
            if (this.winTraceId != null || this.winTraceTitle != null)
            {
               this.log4WinTrace = new WinTrace (this.winTraceId , this.winTraceTitle) ;
               if (this.layout != null)
                  this.log4WinTrace.setMultiColumn () ;
            } else {
               // no wintrace specified

               if (this.layout != null)
               {
                  // Layout on main trace window. create a brother main wintrace
                  this.log4WinTrace = new WinTrace ("_" , "_") ; //$NON-NLS-1$ //$NON-NLS-2$
                  this.log4WinTrace.setMultiColumn () ;
               } else {  // no layout and no wintrace specified, use main wintrace
                  this.log4WinTrace = TTrace.winTrace () ;
               }
            }

            if (this.titleLayout != null && this.log4WinTrace != TTrace.winTrace ())
               this.log4WinTrace.setColumnsTitle (this.titleLayout) ;

            if (this.logMode >= 0)
               this.log4WinTrace.setLogFile(this.logFileName, this.logMode) ;
         }

         TraceNodeEx node = new TraceNodeEx (this.log4WinTrace.debug ()) ;

         // if layout is used, fill only the leftMsg.
         if (this.layout != null)
         {
            node.leftMsg =  this.layout.format (event) ;
            node.time = "" ;         // blank time //$NON-NLS-1$
            node.threadName = "" ;   // blank thread name //$NON-NLS-1$
         }
         else
         {
            // no layout. Use tracetool columns

            node.leftMsg =  event.getLoggerName () ;
            node.rightMsg =  event.getRenderedMessage () ;
            node.threadName = event.getThreadName ();
            node.time = Utility.formatTime (event.timeStamp) ;  // TTrace.formatTime is faster than SimpleDateFormat

            // to do : change icon
            //int level = event.getLevel ().toInt () ;
            //String levelstr = event.getLevel ().toString () ;
            //node.iconIndex = 8 ;
         }

         // add the message object if not a primitive
         Object msg = event.getMessage () ;
         if (!Utility.isPrimitive(msg.getClass()))
            node.addValue(msg, this.sendPrivateObjectInfo, this.maxLevel, "Trace Object") ;  //$NON-NLS-1$

         // add throwable info, if any
         if (event.getThrowableInformation () != null)
         {
            //event.getThrowableInformation ().getThrowableStrRep ()
            String[] s = event.getThrowableStrRep ();
            if (s != null)
            {
               TMemberNode localInfo = node.members.add ("Throwable informations") ; //$NON-NLS-1$
               int len = s.length;
               if (len == 0)
                  return;
               for (int i = 1; i < len; i++)
                  localInfo.add (s[i]);
            }
         }

         // send Local information.
         if (this.sendLocationInfo)
         {
            TMemberNode localInfo = node.members.add ("LocalInfo") ; //$NON-NLS-1$
            LocationInfo locInfo = event.getLocationInformation () ;
            localInfo.add (locInfo.getFileName () , locInfo.getMethodName () , locInfo.getLineNumber ()) ;
         }

         // finally send the node
         node.send () ;

         if (this.immediateFlush)
            TTrace.flush ();
      } catch (Exception e) {
         return ;
         // eat exception
      }
   }

   //--------------------------------------------------------------------------------
   /** Override requiresLayout.
    * @see org.apache.log4j.Appender#requiresLayout()
    * @return true
    */
   public boolean requiresLayout()
   {
      return true;
   }

   //--------------------------------------------------------------------------------

   /** Override setLayout.
    * @param layout Log4j layout
    * @see org.apache.log4j.Appender#setLayout(org.apache.log4j.Layout)
    */
   public void setLayout (final Layout layout)
   {
      this.layout = layout;
   }

   //--------------------------------------------------------------------------------
   /**
    * Close this appender instance.
    *
    * <p>Closed appenders cannot be reused.
    *
    */
   public synchronized void close ()
   {
      if (this.closed)
         return ;
      this.closed = true ;
   }

   //--------------------------------------------------------------------------------
   /*
    * * Immediate flush means that the traces will be flushed at the end of each append operation. Immediate flush is slower but ensures that each append request is actually written. If <code>immediateFlush</code> is set to <code>false</code>, then
    * there is a good chance that the last few logs events are not actually written to persistent media if and when the application crashes.
    * @param value Flush value
    */
   public void setImmediateFlush (final boolean value)
   {
      this.immediateFlush = value;
   }

   /*
    * * Immediate flush means that the traces will be flushed at the end of each append operation. Immediate flush is slower but ensures that each append request is actually written. If <code>immediateFlush</code> is set to <code>false</code>, then
    * there is a good chance that the last few logs events are not actually written to persistent media if and when the application crashes.
    * @return Flush value
    */
   /*
    * *
    * @return
    */
   public boolean getImmediateFlush ()
   {
      return this.immediateFlush;
   }

   //--------------------------------------------------------------------------------

   /*
    * * send log4j Location Information (filename,method and line number). Default is false.
    * @param value flag
    */
   public void setSendLocationInfo (final boolean value)
   {
      this.sendLocationInfo = value;
   }

   /*
    * * send log4j Location Information (filename,method and line number). Default is false.
    * @return flag
    */
   public boolean getSendLocationInfo ()
   {
      return this.sendLocationInfo;
   }

   //--------------------------------------------------------------------------------

   /*
    * * When Object value is send, Private fields can be filtered
    * @param value When True : send private fields
    */
   public void setSendPrivateObjectInfo (final boolean value)
   {
      this.sendPrivateObjectInfo = value;
   }

   /*
    * * When Object value is send, Private fields can be filtered
    * @return When True : send private fields
    */
   public boolean getSendPrivateObjectInfo ()
   {
      return this.sendPrivateObjectInfo;
   }
   //--------------------------------------------------------------------------------

   /**
    * Define the target viewer IP. Default is 127.0.0.1
    * @param value IP adress
    */
   public void setRemoteHost (final String value)
   {
      TTrace.options.socketHost = value;
   }

   /**
    * Define the target viewer IP. Default is 127.0.0.1
    * @return Current IP adress
    */
   public String getRemoteHost ()
   {
      return TTrace.options.socketHost ;
   }
   //--------------------------------------------------------------------------------

   /**
    * Define the target viewer port. Default is 8090
    * @param value Port to use.
    */
   public void setRemotePort (final int value)
   {
      TTrace.options.socketPort = value ;
   }

   /**
    * Define the target viewer port. Default is 8090
    * @return current Port.
    */
   public int getRemotePort ()
   {
      return TTrace.options.socketPort ;
   }

   //--------------------------------------------------------------------------------

   /*
    * * multicolumn Title to display
    * @param value Tab separated strings
    */
   public void setTitleLayout (final String value)
   {
      this.titleLayout = value ;
   }

   /*
    * * multicolumn Title to display
    * @return Tab separated strings
    */
   public String getTitleLayout ()
   {
      return this.titleLayout ;
   }

   //--------------------------------------------------------------------------------

   /*
    * * The WinTrace ID to use. Usually a GUID, but can be any other unique string like 'MyTraceID'
    * @return current WinTrace ID
    */
   public String getWinTraceId ()
   {
      return this.winTraceId ;
   }

   /*
    * * The WinTrace ID to use. Usually a GUID, but can be any other unique string like 'MyTraceID'
    * @param value WinTrace ID to use
    */
   public void setWinTraceId (final String value)
   {
      this.winTraceId = value ;
   }

   //--------------------------------------------------------------------------------

   /*
    * * The WinTrace "Tabsheet" title corresponding to the WinTraceID
    * @return Current title
    */
   public String getWinTraceTitle ()
   {
      return this.winTraceTitle ;
   }

   /*
    * * The WinTrace "Tabsheet" title corresponding to the WinTraceID
    * @param value Title to display
    */
   public void setWinTraceTitle (final String value)
   {
      this.winTraceTitle = value ;
   }


   /*
    * * max tree depth to display into view Trace info
    * @return the level
    */
   public int getMaxLevel()
   {
      return this.maxLevel;
   }

   /*
    * * max tree depth to display into view Trace info
    * @param level Max level
    */
   public void setMaxLevel(final int level) {
      this.maxLevel = level;
   }

   /**
    * Log file name
    * @param log mode and filename, separated by a comma : 1,log.xml
    */
   public void setLogFile (final String log) {
      int pos = log.indexOf(',') ;
      try {
         this.logFileName = log.substring(pos + 1) ;
         this.logMode = Integer.parseInt(log.substring(0, pos));
      } catch (Exception e) {
         // no error
         return ;
      }
   }
}
