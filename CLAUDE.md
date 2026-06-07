# TraceTool — CLAUDE.md

## What is this project

TraceTool is a **Windows-only trace viewer** with native client libraries for multiple languages. It displays structured traces sent from applications via socket, HTTP, WebSocket, or Windows messages.

**Project structure:**

| Folder | Content |
|---|---|
| `Viewer/` | Pre-built viewer executable (`TraceTool.exe`) |
| `Viewer source/` | Delphi source code of the viewer |
| `DotNet/` | C# / .NET client library + demos |
| `NodeJs/` | JavaScript/TypeScript/Node.js client library + MCP server |
| `Java/` | Java client library |
| `Python/` | Python client library |
| `Cpp/` | Unmanaged C++ client library |
| `Delphi/` | Delphi client library |
| `ActiveX/` | ActiveX/COM client library |
| `Plugins/` | Viewer plug-in examples |

## Viewer

- **Location:** `Viewer/TraceTool.exe`
- **Default port:** 8090 (socket), 81 (HTTP / JavaScript)
- Start the viewer before running any client code
- Closing the window minimises to systray; use **Ctrl+Alt+X** to quit
- Config file: `Viewer/TracetoolConfig.xml`

## Communication protocols

| Protocol | Who uses it |
|---|---|
| Socket (TCP port 8090) | .NET, Java, C++, Python, Delphi — preferred |
| Windows message | .NET, C++, Delphi — same machine only |
| HTTP (port 81) | JavaScript, Node.js — only option for browser/node |
| WebSocket (port 8091) | Blazor WebAssembly only |

## Language APIs — quick reference

### .NET (C#)
```csharp
using TraceTool;
TTrace.Debug.Send("message");
TTrace.Debug.Send("left", "right");   // two columns
TTrace.Debug.SendValue("obj", myObj);
TTrace.Debug.Indent("scope"); ... TTrace.Debug.UnIndent();
```
NuGet: `Install-Package Tracetool.DotNet.Api`  
Socket mode: `TTrace.Options.SendMode = SendMode.Socket; TTrace.Options.SocketHost = "127.0.0.1"; TTrace.Options.SocketPort = 8090;`

### Node.js / JavaScript
```javascript
const ttrace = require('tracetool'); // npm install -s tracetool
ttrace.host = "127.0.0.1:81";
ttrace.debug.send("message");
ttrace.debug.send("left", "right");
ttrace.clearAll();
```
Library: `NodeJs/Source/lib/tracetool.js`  
Sample: `NodeJs/Sample/example.js`

### Python
```python
from tracetool import ttrace
ttrace.debug.send("hello Python")
```
Install: `pip install --upgrade tracetool`  
Source: `Python/Src/tracetool.py`

### Java
```java
import TraceTool.*;
TTrace.Warning().Send("hello", "world");
```
Jar: `Java/lib/Tracetool.jar`

### C++ (unmanaged)
```cpp
#include "tracetool.h"
TTrace::Debug()->Send("Hello");
```
Source: `Cpp/Source/tracetool.cpp` + `tracetool.h`

### Delphi
```delphi
uses TraceTool;
TTrace.Warning.Send('hello', 'world');
```
Source: `Delphi/Delphi Library/`

## MCP server (AI / Claude Code integration)

An MCP server wraps the Node.js library so Claude can send traces to the viewer.

- **Server:** `NodeJs/mcp-server/server.js`
- **Config:** `~/.claude/mcp.json`
- **Skill:** `.claude/skills/tracetool.md`

Start: `node NodeJs/mcp-server/server.js`  
Env override: `TRACETOOL_HOST=127.0.0.1:81`

Available MCP tools: `tracetool_send`, `tracetool_clear_all`, `tracetool_indent`, `tracetool_unindent`, `tracetool_enter_method`, `tracetool_exit_method`, `tracetool_send_value`, `tracetool_send_xml`, `tracetool_send_table`, `tracetool_set_host`.

## Common trace patterns

```javascript
// Simple message
ttrace.debug.send("Processing started");

// Two-column (label | value)
ttrace.debug.send("Status", "200 OK");

// Indented scope
ttrace.debug.indent("ProcessOrder", undefined, undefined, true);
ttrace.debug.send("Validate items");
ttrace.debug.send("Charge card");
ttrace.debug.unIndent("ProcessOrder done", undefined, undefined, true);

// Send an object tree
ttrace.debug.sendValue("Response", responseObj, 3);

// Send XML
ttrace.debug.sendXml("Payload", xmlString);

// Separate window tab
var win = new ttrace.classes.WinTrace("MYWIN", "My Window");
win.debug.send("trace in separate tab");

// Watches (shows only last value)
ttrace.watches.send("counter", counter);

// Clear viewer
ttrace.clearAll();
```

## Key API methods (all languages follow the same pattern)

| Method | Description |
|---|---|
| `Send(left [, right])` | Send 1 or 2 column trace; returns a node for sub-traces |
| `Indent(msg)` / `UnIndent(msg)` | Open / close an indented scope |
| `EnterMethod(name)` / `ExitMethod(name)` | Same as Indent/UnIndent with Enter/Exit icon |
| `SendValue(label, obj [, depth])` | Expandable object tree (max 3 levels by default) |
| `SendObject(label, obj)` | Full class info + properties |
| `SendDump(label, title, buffer, count)` | Hex dump |
| `SendStack(label)` | Call stack |
| `SendCaller(label)` | Caller function name only |
| `SendXml(label, xml)` | XML with syntax highlighting |
| `SendTable(label, table)` | Multi-column table |
| `node.ResendLeft/Right(text)` | Update a previously sent node |
| `node.AppendLeft/Right(text)` | Append text to a previously sent node |

## Viewer features

- **Multiple tabs:** main window + custom `WinTrace` windows per application/module
- **Info pane:** shows detail (object tree, dump, stack) for a selected trace
- **Watches window:** displays only the latest value for each key
- **Multi-column mode:** custom column titles with tab-separated data
- **Plug-ins:** extend via .NET / Delphi / C++ plug-ins (View → Options)
- **External sources:** OutputDebugString, Tail files, Event Log, Log4Net, Log4J, System.Trace
- **Save/Load:** XML export per window; daily log files supported
- **Search:** Ctrl+F with highlight, case-sensitive, whole-word options

## Development notes

- The viewer is written in **Delphi** (source in `Viewer source/`)
- The .NET library targets both .NET 4.8 and .NET Standard 2.0
- The Node.js library requires `request`, `stack-trace`, `uuid` — install in `NodeJs/Source/` before use
- The MCP server dependencies are in `NodeJs/mcp-server/node_modules/`
- Current viewer version: **15** (see `Viewer/TracetoolConfig.xml`)
- Git branch: `Develop` (main branch: `master`)
