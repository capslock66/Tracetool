<!--
https://code.visualstudio.com/docs/languages/markdown

Tip: You can also right-click on the editor Tab and select Open Preview (Ctrl+Shift+V) or use the Command Palette (Ctrl+Shift+P) to run the Markdown: Open Preview to the Side command (Ctrl+K V).

https://guides.github.com/features/mastering-markdown/
https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet

https://www.codeproject.com/Articles/5498/TraceTool-The-Swiss-Army-Knife-of-Trace

<a name="What-is-TraceTool"></a>
<a name="Installation"></a>
<a name="ClientAPI"></a>
-->

# Tracetool

![Viewer](/GithubFiles/Server1.jpg)

* [What is TraceTool](#What-is-TraceTool "What is TraceTool")
* [Viewer Installation](#Installation "Installation")
* [Client Api](#ClientAPI "Client Api")
  * [DotNet](#DotNet)
  * [Blazor Client / Server](#Blazor-client--server)
  * [Java, Android](#Java)
  * [Javascript, TypeScript, Node](#Javascript)
  * [C++](#C++)
  * [Python](#Python)
  * [Delphi](#Delphi)
  * [ActiveX](#ActiveX)

* [Samples](#Samples "Samples")
  * [Basic traces](#Basic-traces)
  * [Indent](#Indent)
  * [Resendxxx and Appendxxx](#Resendxxx-and-Appendxxx "Resendxxx and Appendxxx")
  * [Multiple Window Tab](#Multiple-Window-Tab "Multiple Window Tab")
  * [Multiple Column Traces](#Multiple-Column-Traces "Multiple Column Traces")
  * [SendObject, SendType, SendValue](#SendObject-SendType-SendValue "SendObject, *endType, SendValue")
  * [SendDump , SendStack and SendCaller](#SendDump-SendStack-and-SendCaller "SendDump , *endStack and SendCaller")
  * [SendBitmap](#SendBitmap "SendBitmap")
  * [XML](#XML "XML")
  * [Table](#Table "Table")
  * [Watches](#Watches "Watches")
  * [TraceNodeEx](#TraceNodeEx "TraceNodeEx")

* [Working with the viewer](#Working-with-the-viewer "Working with the viewer")
  * [Info Pane](#Info-Pane "Info Pane")
  * [System.Trace (.NET) Support](#System-Trace-NET) Support "System.Trace (.NET) support")
  * [Microsoft Enterprise Framework (.NET) Support #Microsoft-Enterprise-Framework-NET-Support "Microsoft Enterprise Framework (.NET) *upport")
  * [Log4Net (.NET) Support](#Log4Net-NET-Support "Log4Net (.NET) Support")
  * [Log4J (Java) Support](#Log4J-Java-Support "Log4J (Java) Support")
  * [Gdebug (Delphi) Support](#Gdebug-Delphi-Support "Gdebug (Delphi) Support")
  * [Plug-ins](#Plug-ins "Plug-ins")
  * [System OutputDebugString](#System-OutputDebugString "System OutputDebugString")
  * [Tail files](#Tail-files "Tail files")
  * [Event Log Traces](#Event-Log-Traces "Event Log Traces")
  * [Save/Load to XML File](#Save-Load-to-XML-File "Save/Load to XML File")

# What is TraceTool ?

* A viewer that displays multiple kinds of sources (from the tracetool framework, log file, event log, or OutputDebugString)
* A native language client framework (Dotnet, Java, Javasvript, C++ , Python, Delphi) to send simple traces, class and object viewer, dump, and call stack to the viewer. See [Working with the viewer](#Working-with-the-viewer "Working with the viewer") for user interface

# Viewer Installation

Download the [Viewer](/GithubFiles/Viewer.zip "Viewer.zip") and unpack the file into a folder of your chose(windows only).

If you plan to use the "windows message" mode, you must start once the viewer to self register his location into the registry. For socket mode, the viewer must always be started manually

When the viewer is started, he appears on the tray icon. Closing the viewer just reduce it to systray. \
Hit ctrl - Alt - X to close it.

# Client Api

You can chose how to send trace to the viewer.

* Windows message (same computer). Don't work with Java, javascript / TypeScript, node and python
* Socket connection (localhost or remote). Don't work with javascript / TypeSCript, node
* Web socket connection. Work only on dotnet
* http connection. Work only javascript / Typescript

On dotnet, you can use Asynchronous or worker Thread. Blazor support only Synchronous communi
Demo is provided for each client api

Note for Web socket connection: The viewer use a plugin to receive traces and display on the viewer. \
The plugin is configured, but not activated by default. \
On the view menu, select options... item, click on "WebsockPlugin.dll" item, click "Load at startup",
click "Load and start" button then "Ok" button.

![Plugin Option](/GithubFiles/WebSockedPlugin.png)

The "web socket" label is displayed in the trace window. Clicking on this label show some statistics informations

![Websock label](/GithubFiles/websocketLabel.png)

## DotNet

In visual studio, reference the client Api nuget using "manage nuget packages","Manage Nuget Packages for solution" context menus or via the nuget console
>Install-Package Tracetool.DotNet.Api

The nuget contains DotNet4.7, Standard 1.6 and Standard 2.0 libraries

Sample code

``` C#
using TraceTool ;
...
TTrace.Error.Send("Hello world") ;          // one columns
TTrace.Warning.Send("Hello","world") ;      // two columns
TTrace.Debug.Send("Hello").Send("world") ;  // 2 nodes (master detail)
```

Here is the result:

![Short sample](/GithubFiles/DotNetShortSample.png)

If you chose socket mode (web development, service,...), add this code to your startup function

``` C#
using TraceTool ;
...

TTrace.Options.SendMode = SendMode.Socket;
TTrace.Options.SocketHost = "127.0.0.1";
TTrace.Options.SocketPort = 8090;
```

See the [Samples](#Samples "Samples") section for more examples

## Blazor client / server

You can use tracetool on client site. Add a reference to Tracetool.DotNet.Api, specify websocket mode and async communication.
Don't forget to enable plugin ! \
Here is a simple razor page

``` c#
@page "/"

@using TraceTool

<h1>tracetool demo</h1>

<button class="btn btn-primary" @onclick="SimpleTraces">trace tool demo</button>

@code {

    protected override void OnInitialized()
    {
        TTrace.Options.SendMode = SendMode.WebSocket;
        TTrace.Options.UseWorkerThread = false; // Async communication for blazor client
        TTrace.Options.SocketHost = "127.0.0.1";
        TTrace.Options.SocketPort = 8091;
    }

    private async void SimpleTraces()
    {

        TTrace.Debug.Send($"trace from blazor client");
        TTrace.Debug.SendValue("Value", TTrace.Debug);
        TTrace.Debug.SendObject("Object", TTrace.Debug);

        // not necessary to flush. If you don't need to wait for traces, just remove the "async" before SimpleTraces()
        await TTrace.FlushAsync();   // wait for all message send
    }
```

Here is the result:

![blazor](/GithubFiles/Blazor.png)

See the [Samples](#Samples "Samples") section for more examples

## Java

Download the [Tracetool.jar](/java/lib/Tracetool.jar "Tracetool.jar") from github

Sample code

``` Java
import TraceTool.*;
...
TTrace.Warning().Send("hello" , "world");
```

See the [Samples](#Samples "Samples") section for more examples

## Java Android

This is a special version of the Java library with some implementation differences (GUID, color, connection). The library can send traces to the viewer through Wi-Fi, 3G, or USB. Both real device and Android Virtual Device (AVD in short) are supported.

Open the 8090 port on your firewall.
Run your application, and set TTrace.options.socketHost to your development computer IP address
Send traces.

## Javascript

The TraceTool API is a cross browser (tested under Internet Explorer ,chrome, Firefox) and cross domain tracing solution. The viewer can be installed on any PC (localhost, for example) while you are displaying a page from another server. Just configure the TraceTool JavaScript client to use the viewer on the network.
you can use tracetool in html (angular for example)

Javascript files ([tracetool.js](/viewer/tracetool.js "tracetool.js"))
and ([tracetool.jmin.js](/viewer/tracetool.jmin.js "tracetool.jmin.js")) are included in the viewer folder. \
You can also install it from Npm command line:
>npm install -s tracetool

The node package contains typings tracetool.d.ts for typescript

**Html** sample code

``` html
...
<script type="text/javascript" src="tracetool.js" no-cache></script>
...
<script>
ttrace.host="localHost:85";

function butSample()
{
   ttrace.debug.send('hello world') ;
   ttrace.debug.send('hello','world') ;
   var hello = ttrace.debug.send('hello') ;
   hello.send('world') ;  // send traces under another node
} ;
</script>
...
<input type="button" value="Sample traces" onclick="butSample()"/>
```

Another way to load the library is to ask the viewer to send the script. Add this line on the body tag:

``` html
<script type="text/JavaScript"
   src="http://localHost:81/tracetool.js?Compressed=true" no-cache>
</script>
```

**Angular** sample code

package.json -> "dependencies" -> "@tracetool/webpack": "^1.0.1"

app.component.ts

``` Typescript
import '@tracetool/webpack';   // import once. ttrace is saved in global

ttrace.host = "127.0.0.1:85";  // Must be done once

...

ngOnInit(): void {
   ttrace.debug.send("Hello", "world");
}
```

**Node js** sample code

package.json -> "dependencies" -> "tracetool": ">12.10.2"

example.js

``` javascript
"use strict";

const ttrace = require('tracetool');    // default host is 127.0.0.1:81
ttrace.host = "127.0.0.1:85";

ttrace.debug.send("Hello", "world");
```

See the [Samples](#Samples "Samples") section for more examples

## C++

For managed code, use the dotnet library

Unmanaged C++ files ([tracetool.cpp](/Cpp/Source/tracetool.cpp "tracetool.cpp"))
and ([tracetool.h](/Cpp/Source/tracetool.h "tracetool.h")) are included in the Cpp/Source folder.

Unmanaged code sample:

``` C++
#include "tracetool.h"

...
TTrace::Debug()->Send ("Hello");
```

See the [Samples](#Samples "Samples") section for more examples

## Python

Python file ([tracetool.py](/Python/Src/tracetool.py "tracetool.py"))
is included in the Python/Src folder.

The library is compatible with Python 2, Python 3, and Iron Python (.NET). \
The library was also tested under Blender

Sample code

``` Python
>>> from tracetool import ttrace
>>> ttrace.debug.send ("hello Python")
```

## Delphi

Delphi files are included in the [Delphi/Delphi Library](/Delphi/Delphi%20Library "libs") folder

Sample code

``` Delphi
uses TraceTool,
...
TTrace.Warning.Send('hello' , 'world') ;
```

## ActiveX

ActiveX files ([ActiveX_Lib.zip](/ActiveX/Lib/ActiveX_Lib.zip "ActiveX_Lib.zip")) are included in the [ActiveX/Lib](/ActiveX/Lib "libs") folder. The zip file contains batch file to register the library on the registry

Sample code (JScript):

``` JScript
var TTrace = new ActiveXObject("TraceToolCom.XTrace");
...
TTrace.Debug.Send("hello from jScript") ;
```

# Samples

For facility, all samples uses the DOTNET syntax.
Note that not all client framework support all functionalities like sending image to viewer with Python

## Basic traces

``` C#
// 90 % of your traces will be just sending a single string or 2 strings on a single line
TTrace.Debug.Send("simple node");
TTrace.Debug.Send("my string", "my value");

// Master detail node
TTrace.Debug.Send("master").Send("Detail");

var masterNode = TTrace.Debug.Send("master");
masterNode.Send("Detail 1");
masterNode.Send("Detail 2");
```

## Indent

Another way to have master detail is to use indent and UnIndent methods

``` C#
TTrace.Debug.Indent ("Begin of procedure") ;
TTrace.Debug.Send ("inside procedure" ) ;
TTrace.Debug.Send ("Do some work" ) ;
TTrace.Debug.UnIndent ("end of procedure") ;
```

![Indent](/GithubFiles/Server3.jpg)

Note : ensure the UnIndent is called (use try finally), else you will see unexpected indentation

## Resendxxx and Appendxxx

Once a node is sent, you have the possibility to change the text. For example, send the "Start.." text, and when an operation is completed, append the "Done" text, or replace one of the two columns with a new text:

``` C#
TraceNode start1 = TTrace.Debug.Send ("Start 1 ..") ;
start1.ResendRight ("Done") ;

TraceNode start2 = TTrace.Debug.Send ("Start 2 ..") ;
start2.AppendLeft ("Done") ;
```

![Resendxxx](/GithubFiles/traceT13.gif)

Note that the time is not changed.

## Multiple Window Tab

You can send traces in a separate window tab:

``` C#
WinTrace myWinTrace ;
myWinTrace = new WinTrace ("MyWINID", "My trace window");
myWinTrace.Debug.Send ("Hello", "Can be used to store exceptions, for examples");
```

You can access the main WinTrace objects from the TTrace.WinTrace attribute.

The WinTrace API lets you save to a text file or an XML file, load XML files, and clear content. You can separate exceptions or SQL traces from classic traces, then save it at any time.

Each window can automatically save traces in an XML file at real time. To set the file name, you can use the WinTrace.SetLogFile function, or do it directly in the viewer. You can create a daily file (the date is appended to the file name), or disable the log file.

![multitab](/GithubFiles/multitab.jpg)

## Multiple Column Traces

The original framework allows you to send two columns of information (including thread name, icon, and date). It's now possible to send as many columns as you want, but with a small restriction: multi-column is not supported in the main trace window, since many applications (and in different languages) cannot share the main window in "classic" mode and in "multi-column" mode. You must then create a window trace and set the column titles (with just a few lines). In that mode, of course, you lose the automatic thread name and date. You must send them yourself if you need to. Sending many columns of text is performed with the same API as for simple trace. Just add a tabulation ("\t" or char 9) between the different columns.

Here is a C# example:

``` C#
WinTrace MultiColTrace ;
public  void initTrace()
{
     // create the window
     MultiColTrace = new WinTrace ("MCOL",
                       "MultiCol trace window") ;

     // set the window to multi column mode.

     // That automatically remove all columns
     MultiColTrace.SetMultiColumn () ;

     // must be called before calling setColumnsTitle
     // add columns title
     MultiColTrace.SetColumnsTitle("col1 \t col2 \t col3");

     // columns titles must separated by tabulations

     // change the active window (optional)
     MultiColTrace.DisplayWin() ;
}
```

Once initialized, use this new WinTrace instance to send data:

``` C#
// columns data must be separated by tabulations

MultiColTrace.Debug.send("1 \t 2 \t 3") ;
```

![multicol](/GithubFiles/multicol.jpg)

## SendObject, SendType, SendValue

SendObject displays the object class information with the property values. SendType displays class information with static property values:

``` C#
TTrace.Debug.SendType ("this type",  this.GetType() );
TTrace.Debug.SendObject ("this object", this );
```

If SendObject displays the complete structure of the object, with a lot of details, SendValue displays the object value (with multi-dimensional arrays and collections) on many levels. Primitives, and some classes like Date, are displayed as strings:

``` C#
object obj;
obj = null;
TTrace.Debug.SendValue ("null Object", obj);

obj = new Object();
TTrace.Debug.SendValue ("Object instance", obj);

obj = 123;
TTrace.Debug.SendValue ("integer Object", obj);

obj = "str";
TTrace.Debug.SendValue ("string", obj);

// simple array of Integer
int[] VtArr = new int[10]{7,1,5,2,0,3,4,8,6,9};
TTrace.Debug.SendValue ("simple array", VtArr);
```

![tracet8](/GithubFiles/traceT8.gif)

![tracet9](/GithubFiles/traceT9.gif)

As you can see in the previous example, SendValue is not limited to a simple type. Object properties can point to another object. Recursive object display is limited, by default, to three levels. Object reference is displayed (class@code) on the second column. Objects already displayed are replaced by a "see @" reference. Presently, the Delphi SendValue is limited to variant values (no properties) and arrays.

Here is another screenshot for a complex multidimensional array with user defined bounds and types:

![tracet10](/GithubFiles/traceT10.jpg)

## SendDump , SendStack and SendCaller

SenDump displays the buffer dump. Sample code:

``` C#
TTrace.Debug.SendDump ("Dump test", "Unicode",  System.Text.Encoding.Unicode.GetBytes(str) ,50) ;
```

![tracet11](/GithubFiles/traceT11.gif)

Sendstack and SendCaller let you display the stack information:

``` C#
TTrace.Debug.SendStack  ("Stack test" , 0) ;
TTrace.Debug.SendCaller ("Caller test" , 0) ;
```

![tracet12](/GithubFiles/traceT12.gif)

## SendBitmap

``` C#
TTrace.Debug.SendBitmap("Bitmap", pictureBox1.Image);
```

![sendbitmap](/GithubFiles/sendBitmap.jpg)

## XML

``` C#
nodeEx.AddXML("<data> Hello XML </data>");
```

![sendxml](/GithubFiles/sendXml.jpg)

## Table

SendTable and AddTable send a multicolumn table in the info panel:

``` C#
// create the table
TraceTable table = new TraceTable();

// add titles. Individual or multiple columns titles
// can be added, separated by tabs
table.AddColumnTitle("colA");          // first column title

table.AddColumnTitle("colB");          // second column title
table.AddColumnTitle("title column C\tcolD");  // other columns title

// add first line. Individual or multiple columns data

// can be added columns, separated by tabs
table.AddRow();
// add first col
table.AddRowData("a");
// then add other columns (tab separated)
table.AddRowData("b" + "\t" + "c" + "\t" + "d" + "\t" + "e");

// add second line
table.AddRow();
// add all columns data in a single step (tab separated)
table.AddRowData("aa" + "\t" + "data second column" +
                 "\t" + "cc" + "\t" + "dd" + "\t" + "ee");

// finally send the table
TTrace.Debug.SendTable("Mytable", table);
```

You can send collections (Array, IEnumerable, IDictionary, ICollection) using the same syntax. This can be useful to display LINQ results.

Tracetool will get the first object in the collection to get the titles. It's important that all items should then be of the same kind.

``` C#
// array of FileInfo[]
string strTempPath = System.IO.Path.GetTempPath();
DirectoryInfo TempPath = new DirectoryInfo(strTempPath);
TTrace.Debug.SendTable("Files in temp path", TempPath.GetFiles());

// Linq to object of FileInfo[]

var LinqToObjectQuery =
   from file in TempPath.GetFiles()
   where file.Length > 100
   orderby file.Length descending
   select new
   {
      file.Name,
      file.Length,
      file.LastWriteTime,
      file
   };
TTrace.Debug.SendTable("Files having length>100 in temp path (Linq)",
                       LinqToObjectQuery);
```

![sendtable](/GithubFiles/SendTable.jpg)

## Watches

If you want to follow the values of a variable, you can send it using the classic TTrace.Debug.Send method, but that can produce a lot of traces. In its place, you can use the TTrace.Watches.Send method that shows only the last value. Watches are displayed on a separate window. You can create different watch windows.

![watches](/GithubFiles/watches.jpg)

## TraceNodeEx

In place of overloading the Send method with different parameters, you can use the TraceNodeEx class (that inherits from TraceNode). The advantage is that you can have, for example, many dumps for the same trace, on the "Info" pane. The SendObject, SendType, SendValue, SendDump, SendStack, and the SendCaller functions have the Addxxx counterparts.

You must specify the parent node trace in the constructor parameter. The parent IconIndex, Enabled, and Id properties are used for the newly created trace (a null parent is also accepted).

You can specify the text of the columns separately, specify the icon to use, add object and type, or add Dump to the associated 'Detail' tree. When the node is completed, just call its Send method:

``` C#
// Create an unicode string with special
// char in front and at the end.
string str =  '\u2250' + "azertyuiop qsdfghjklm" + '\u9999' ;

// Create a trace node with the same icon and
// Enabled properties as TTrace.Debug
TraceNodeEx node = new TraceNodeEx (TTrace.Debug) ;
// Fill it
node.LeftMsg = "hello" ;
node.RightMsg = "world" ;
node.IconIndex = 8 ;
node.AddDump (
   System.Text.Encoding.Unicode.GetBytes(str) ,50) ;
node.AddDump (
   System.Text.Encoding.ASCII.GetBytes(str) ,50) ;
node.AddObject (str) ;

// and finally send it
node.Send () ;
```

The TracenodeEx.Resend function lets you resend the two texts (not members) at any time.

# Working with the Viewer

## Info Pane

The info panel displays additional information for a specific trace. When available, a small blue circle is visible on the gutter. This can be the object view, dump, stack, or any data that can be displayed in a maximum of three columns. The "Info" button on the toolbar shows or hides this panel.

## System.Trace (.NET) Support

The classic Microsoft Trace can be redirected to the viewer using the TTraceListener bridge. Here is a sample:

``` C#
Trace.Listeners.Clear() ;
Trace.Listeners.Add (new TTraceListener ()) ;

int[] myArray = new int[3] { 3, 5 , 5};
Trace.WriteLine ("TraceListener demo") ;
Trace.Write ("myArray : ") ;
Trace.WriteLine (myArray) ;
```

![tracet14](/GithubFiles/traceT14.gif)

## Microsoft Enterprise Framework (.NET) Support

Microsoft Enterprise Framework (EIF) traces can be redirected to the viewer using the TraceToolEventSink library.

Sample EIF demo:

``` C#
private static EventSource myEventSource =
                           new EventSource("EIFDemo");

[STAThread]

static void Main(string[] args) {
   // sample 1 : use static ErrorMessageEvent
   // msg, severity, error code
   ErrorMessageEvent.Raise("Error Message Event", 1, "CODE");

   // sample 2 : create a Trace Message Event instance,

   // fill it then raise from the event source
   TraceMessageEvent messageEvent1  = new TraceMessageEvent();
   messageEvent1.Message = "Costly Message";
   myEventSource.Raise(messageEvent1 );

   // sample 3 : static one line (which wraps the
   // above code sequence)

   TraceMessageEvent.Raise(myEventSource,
                        "Static One Line Message");

   // sample 4 : static one line which is raised through
   // the EventSource.Application EventSource.
   TraceMessageEvent.Raise("Static One Line Message " +
                   "through the Application event source");
}
```

EIF uses a specific configuration file to link traces to the target library (event log, SQL, ...). See the demo configuration file for more details on TraceToolEventSink. Note that this library must be signed (not the case of Log4Net support).

![eif](/GithubFiles/eif.jpg)

## Log4Net (.NET) Support

Log4Net traces can be redirected to the viewer using the TraceTool4Log4Net library (TraceTool for Log4Net). Column formatting is done by Log4Net, but tabulations must be used as column separators. Column title must be specified in the CONFIG file.

Sample Log4Net demo:

``` C#
// Create a logger for use in this class
// (Log4NetDemo.Form1)
private static ILog log = LogManager.GetLogger(
         MethodBase.GetCurrentMethod().DeclaringType);

private void butLog_Click(object sender, EventArgs e)
{
   // simple test

   log.Info("Hello world");
   // use an object as message (the object is
   // displayed in the info panel)
   log.Debug(log);
   // exception test (the exception is displayed

   // in the info panel)
   Exception MyException = new Exception("my exception") ;
   log.Error("Received exception",MyException);
}
```

Like for EIF, the Log4Net engine, the configuration file is used to link traces to the library.

![log4net](/GithubFiles/log4net.jpg)

## Log4J (Java) Support

As for Log4NET, Log4J traces can be redirected to the viewer using the TraceTool library. Column formatting is done by Log4J, but tabulations must be used as column separators. Column title must be specified in the CONFIG file.

Sample Log4J demo:

``` java
Logger logger ;    // Log4J logger
Category cat ;     // Log4J category

// Initialize Log4J
PropertyConfigurator.configure("log4J.properties");
logger = Logger.getLogger("hello.world");
cat    = Category.getInstance(JavaDemo.class);

// simple logger test. Log4J will prepare the trace
// and call the TraceTool appender.
logger.info("Hello world");

// Logger test with attached exception (displayed in info pane)
Exception  e = new Exception("my exception") ;
logger.debug("Received exception",e);

// Category test with object value (displayed in info pane)
cat.debug(this);
```

PropertyConfigurator.configure opens the configuration file to know how to link traces to the library. Unlike the .NET support, the TraceTool Log4JAppender class doesn't need to be implemented in another library (Jar).

![log4j](/GithubFiles/log4j.jpg)

## Gdebug (Delphi) Support

Since the original idea of TraceTool came from the Gdebug tool, I also provided a DbugIntf.pas file with the same functions. That helps those users switching from Gdebug to TraceTool.

Sample Delphi code:

``` Delphi
SendDebug ('Hello world') ;
SendDebugEx ('Hello world', mtWarning);
```

## Plug-ins

You can extend the TraceTool functionality with plug-ins. Plug-ins can be written in .NET, Delphi, or C++. With plug-ins, you can add, for example, a key logger, Registry logger, or a protocol analyzer to TraceTool without having to create a separate application. Plug-ins receive user actions like deleting a node, pushing the clear button, or trying to close a trace window. Plug-ins can add actions on the viewer menu, or labels and buttons on a trace window. A functional clipboard history is included in the main distribution. The plug-ins must be added in the viewer using the View / Options menu.

## System OutputDebugString

To display the OutputDebugString window, activate it in the window menu (show OutputDebugString). The OutputDebugString windows can be paused or cleared using the toolbar. You can also copy selected lines to the Clipboard and save the content to an XML file (the same format as the TraceTool export).

![ods](/GithubFiles/ODS.jpg)

## Tail files

To add a new "Tail" tab, select the Windows/Open Tail File... menu, then select the file you want to watch.

Clipboard operations are allowed, but not exported, since traces are already stored in a file. The number of displayed lines is also specified in the Options menu.

![tail](/GithubFiles/Tail.jpg)

## Event Log Traces

To add a new "Event log" tab, select the Windows/Open Event Log... menu, then select the log you want to watch. New events are automatically added.

Clipboard operations and XML export are allowed (the same format as the TraceTool export).

![evn](/GithubFiles/Evn.jpg)

## Save/Load to XML File

The viewer can append traces into an XML file (single filename or daily filename). You can specify the maximum number of traces stored in a file. New files appended with _n are created if needed. These files can also be created by the client API. The same Wintrace.setLogFile function is used to specify the client and the viewer log file. Due to security reasons, local log file is not implemented in JavaScript and the Silverlight 2 client API.

When used with the TTrace.Options.SendMode parameter set to 'None', you can write traces in XML files without having to run the viewer. SendMode is not implemented in the JavaScript API (forced to HTTP).
