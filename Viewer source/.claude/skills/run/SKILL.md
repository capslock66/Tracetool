---
name: run
description: Build and launch TraceTool. Use when asked to build the application, detect build errors, run the app, or verify a change compiles and starts correctly.
---

## Build

Run from the project root (`Viewer source\`):

```powershell
.\build.ps1                          # Release Win64 (default)
.\build.ps1 -Platform Win32          # Release Win32
.\build.ps1 -Config Debug            # Debug Win64
.\build.ps1 -Config Debug -Platform Win32
```

The script auto-detects the RAD Studio installation via `$env:BDS` or known install paths, then reads IDE library search paths from the registry so third-party units (madExcept, JCL, VirtualTrees, FastMM5, etc.) resolve without manual configuration.

A successful build produces `..\Viewer\TraceTool.exe`.

## Launch

```powershell
Start-Process "..\Viewer\TraceTool.exe"
```

TraceTool is a Windows desktop GUI (VCL). It opens its main window immediately on launch — no arguments required. Internal traces appear in a dedicated trace window on startup.

## Verify the build succeeded

Check exit code from `build.ps1` — it exits non-zero on any compiler error and prints the failing unit and error code, e.g.:

```
TraceTool.dpr(60): error F2613: Unit 'madExcept' not found.
Build FAILED (exit code 1)
```
