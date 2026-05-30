# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

TraceTool is a Windows desktop trace viewer built in Delphi (Object Pascal) using VCL. It receives debug/trace messages from client applications (.NET, JavaScript, Java, Delphi, C++, Python) via multiple protocols and displays them in a hierarchical tree view.

## Build

- **IDE:** Embarcadero RAD Studio / Delphi
- **Project file:** `TraceTool.dproj`
- **Output directory:** `..\Viewer\`
- **Configurations:** Release (optimized, no debug symbols) and Debug (full symbols)
- **Targets:** Win32 and Win64
- **Post-build:** `afterBuild.bat` sets file permissions on the compiled executable

Build from the command line using the provided script (defaults to Release Win64):

```powershell
.\build.ps1                          # Release Win64 (default)
.\build.ps1 -Platform Win32          # Release Win32
.\build.ps1 -Config Debug            # Debug Win64
.\build.ps1 -Config Debug -Platform Win32
```

The script reads IDE library search paths from the registry (`HKCU:\Software\Embarcadero\BDS\<ver>\Library\<Platform>`) so third-party units (madExcept, JCL, VirtualTrees, etc.) are found without manual path configuration. Alternatively, build directly from the Delphi IDE.

## Architecture

### Main Form
`TFrm_Tool` (`Unt_Tool.pas`) is the single application-wide instance. It owns all server listeners and holds static references used across the whole app: `FormTraceList` (list of all module instances) and `ContainerList` (list of docking containers).

Server listeners hosted by `TFrm_Tool`:
- TCP servers (two `TIdTCPServer` instances)
- UDP servers (two instances)
- HTTP server (`TIdHTTPServer`)
- WM_COPYDATA handler (`Unt_receiver.pas`)
- Socket Policy Server for Flash/AIR clients

Incoming messages are queued and dispatched via a timer-based processing loop. `unt_Parse.pas` parses raw data and XML payloads into `TTreeRec` records. `unt_Decode.pas` handles lower-level decoding.

### Modules
The four module types all inherit from `TFrmBase` (`unt_base.pas`) and share a three-level display structure:

1. **Primary grid** — `TVirtualStringTree` (first-level message list)
2. **Detail grid** — second `TVirtualStringTree` (selected message detail)
3. **Sub-detail** — `TFrameMemo` (`unt_FrameMemo.pas`) for raw text content

| Module | Class | Unit | Instances | Notes |
|---|---|---|---|---|
| Trace viewer | `TFrm_Trace` | `unt_TraceWin` | Multiple | 3 modes: normal, multicolumn (`IsMultiColTree`), watch (`isWatch`); internal traces shown in a dedicated instance; XML export via `Application6` |
| OutputDebugString | `TFrm_ODS` | `unt_ODS` | One | Receives Win32 `OutputDebugString` messages |
| Event viewer | `TFrmEventLog` | `unt_eventLog` | Multiple | User picks a source (e.g. "Application") via `TFrmSelectEvent` (`unt_selectEvent`) |
| File tail | `TFrmTail` | `unt_Tail` | Multiple | Like `tail -f`; user picks a file via `TFrmSelectTail` (`unt_selectEvent`) |

### Docking & Layout
Module forms are hosted inside `TFrmPageContainer` instances (`unt_PageContainer.pas`). Containers are managed by `TDockingPageControl` (also in `unt_PageContainer.pas`). The full container list is stored in `TFrm_Tool.ContainerList`.

### Configuration
`TfrmDebugOptions` (`DebugOptions.pas`) is the configuration UI. `Config.pas` is auto-generated XML data binding from `Config.xsd`; it stores ports, UI layout, favorites, and plugin settings. `unt_TraceConfig.pas` wraps trace-specific config.

### Plugins
`unt_plugin.pas` defines `TPlugin` (abstract), `TWin32Plugin` (Delphi DLL), and `TDotNetPlugin`/`TDotNetManager` (.NET managed). The C# plugin side lives in `CSharpPlugin\CSharpPlugin.csproj`. Plugins are loaded at runtime and managed through the configuration form.

### Key Dependencies
- **VirtualTrees** (v4.0.17) — tree view component
- **Indy 10** — TCP/UDP/HTTP servers
- **madExcept / madKernel / madBasic** — exception handling and utilities
- **JCL** (JEDI Code Library) — general utilities
- **FastMM4/FastMM5** — memory manager
- **MSXML2 / XMLDoc** — XML parsing
- **PSC / FCL** — Pascal Script scripting support

## Key Files

| File | Role |
|---|---|
| `Unt_Tool.pas` | Main form; all server/protocol management |
| `unt_TraceWin.pas` | Primary trace tree UI |
| `Unt_receiver.pas` | WM_COPYDATA receiver |
| `unt_Parse.pas` | Core message parsing → tree nodes |
| `unt_plugin.pas` | Plugin architecture |
| `unt_filter.pas` | Filtering engine |
| `unt_search.pas` | Search |
| `unt_utility.pas` | Shared helpers (drawing, string, process utils) |
| `unt_linkedList.pas` | Message queue data structure |
| `Config.pas` | XML config data binding (generated) |
| `Application6.pas` | XML app data binding (generated) |
