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

![plugin](/GithubFiles/Server1.jpg)

* [What is TraceTool](#What-is-TraceTool "What is TraceTool")
* [Viewer Installation](#Installation "Installation")
* [Client Api](#ClientAPI "Client Api")
  * [DotNet](#DotNet)
  * [Blazor](#Blazor)
  * [Java, Android](#Java)
  * [Javascript, TypeScript, Node](#Javascript)
  * [C++](#C++)
  * [Python](#Python)
  * [Delphi](#Delphi)
  * [ActiveX](#ActiveX)

* [Samples](#Samples "Samples")
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
  * [Log File](#Log-File "Log File")
  * [System.Trace (.NET) Support](#System-Trace-NET) Support "System.Trace (.NET) *upport")
  * [Microsoft Enterprise Framework (.NET) Support #Microsoft-Enterprise-Framework-NET-Support "Microsoft Enterprise Framework (.NET) *upport")
  * [Log4Net (.NET) Support](#Log4Net-NET-Support "Log4Net (.NET) Support")
  * [Log4J (Java) Support](#Log4J-Java-Support "Log4J (Java) Support")
  * [Gdebug (Delphi) Support](#Gdebug-Delphi-Support "Gdebug (Delphi) Support")
  * [Plug-ins](#Plug-ins "Plug-ins")
  * [System OutputDebugString](#System-OutputDebugString "System OutputDebugString")
  * [Tail files](#Tail-files "Tail files")
  * [Event Log Traces](#Event-Log-Traces "Event Log Traces")
  * [Save/Load to XML File](#Save-Load-to-XML-File "Save/Load to XML File")

# What is TraceTool

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

![plugin](/GithubFiles/WebSockedPlugin.png)

The "web socket" label is displayed in the trace window. Clicking on this label show some statistics informations

![plugin](/GithubFiles/websocketLabel.png)

## DotNet

In visual studio install the client Api using "manage nuget packages","Manage Nuget Packages for solution" context menus or via the nuget console
>Install-Package Tracetool.DotNet.Api

The nuget contains DotNet4.7, Standard 1.6 and Standard 2.0 libraries

Sample code

``` DotNet
using TraceTool ;
...
TTrace.Error.Send("Hello world") ;          // one columns
TTrace.Warning.Send("Hello","world") ;      // two columns
TTrace.Debug.Send("Hello").Send("world") ;  // 2 nodes (master detail)
```

Here is the result:

![plugin](/GithubFiles/DotNetShortSample.png)

If you chose socket mode, add this code to your startup function

``` DotNet
using TraceTool ;
...

TTrace.Options.SendMode = SendMode.Socket;
TTrace.Options.SocketHost = "127.0.0.1";
TTrace.Options.SocketPort = 8090;
```

See the [Samples](#Samples "Samples") section for more examples

## Blazor (client / server)

You can use tracetool on client site. Add a reference to Tracetool.DotNet.Api, specify websocket mode and async communication.
Don't forget to enable plugin ! \
Here is a simple razor page

``` DotNet
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

![plugin](/GithubFiles/Blazor.png)

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

## Javascript

you can use tracetool in html (angular for example) and in Node js

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

Sample code

``` Python
>>> from tracetool import ttrace
>>> ttrace.debug.send ("hello Python")
```

## Delphi

<!-- https://github.com/capslock66/Tracetool/tree/Develop/Delphi/Delphi%20Library -->
Delphi files are included in the [Delphi/Delphi Library](/Delphi/Delphi%20Library "libs") folder

Sample code

``` Delphi
uses TraceTool,
...
TTrace.Warning.Send('hello' , 'world') ;
```

## ActiveX

# Samples

## Indent

## Resendxxx and Appendxxx

## Multiple Window Tab

## Multiple Column Traces

## SendObject, SendType, SendValue

## SendDump , SendStack and SendCaller

## SendBitmap

## XML

## Table

## Watches

## TraceNodeEx

# Working with the Viewer

## Info Pane

## Log File

## System.Trace (.NET) Support

## Microsoft Enterprise Framework (.NET) Support

## Log4Net (.NET) Support

## Log4J (Java) Support

## Gdebug (Delphi) Support

## Plug-ins

## System OutputDebugString

## Tail files

## Event Log Traces

## Save/Load to XML File

..... work in progress ....
