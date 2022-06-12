// ***************************************************************
//  madKernel.pas             version:  1.3l  ·  date: 2009-07-14
//  -------------------------------------------------------------
//  processes, threads, modules, tray icons, handles, ...
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2009 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2009-07-14 1.3l fixed: some methods failed "not all replace pointers found"
// 2009-02-09 1.3k Delphi 2009 support
// 2007-05-28 1.3j another Vista bugfix
// 2007-01-14 1.3i some IHandle/IKernelObj bugs fixed
// 2006-11-28 1.3h (1) GetHandleTableNt doesn't work in 64bit OSs
//                 (2) IProcess.ExeFile improved (when exe file is renamed)
//                 (3) ITrayIcon.Rect improved
//                 (4) Vista crashes fixed
// 2006-01-05 1.3g leak fixed which only occurred when using BDS2006
// 2005-11-13 1.3f (1) IProcess.AllocMem now allocates at >= $5f000000
//                 (2) bug in IProcess.AllocMem fixed
// 2005-08-12 1.3e (1) compile without madDisAsm/Remote is now possible
//                     some undocumented functions won't work this way, though
//                 (2) IProcess.SetPriorityClass fix fixed
// 2005-02-03 1.3d two little bugs fixed in IProcess.Get/SetPriorityClass
// 2004-10-10 1.3c (1) IProcess.Session added
//                 (2) NewProcess(16bitProcessWithParams) had a bug
//                 (3) ITrayIcon.MouseClick supports double clicks now
//                 (4) IThread.IsSuspended added
// 2004-06-26 1.3b (1) IProcess.PriorityClass: pcBelowNormal/AboveNormal added
//                 (2) (invalid IProcess).Windows_ enumerated system wide
// 2004-04-19 1.3a little leak in IProcesses.RefreshItems fixed
// 2004-03-06 1.3  (1) ProtectMemory added where UnprotectMemory was used
//                 (2) TrayIcon.Rect improved
//                 (3) Process('c:\progra~1\shorty\shorty~1.exe') didn't work
// 2003-11-18 1.2z (1) critical bug in TIThread.GetHandle2 fixed
// 2003-11-13 1.2y (1) support for Windows 2003 added
//                 (2) THREAD_XXX flags renamed to Thread_XXX -> BCB support
//                 (3) IProcess.ZeroMemory renamed to zeroMemory -> BCB support
//                 (4) IProcess.GetServiceProcess now works in NT, too
//                 (5) error handling improved a bit
//                 (6) module file name logic improved a bit (winNT)
// 2003-06-09 1.2x (1) GetImageProcAddress bug in handling forwarded APIs
//                 (2) TrayIcons didn't work in ME
//                 (3) IProcess.ParentProcess didn't work in the winNT family
//                 (4) some minor stability/reliability improvements
//                 (5) ITrayIcon.Delete always returned false
//                 (6) SendMessage(WM_GETTEXT(LEN)) -> SendMessageTimeout
// 2002-12-05 1.2w (1) StartButton and DesktopListView didn't work in XP
//                 (2) IWindow.Text was a read only property, now it's r/w
//                 (3) CurrentModule bug fixed
// 2002-11-07 1.2v added "timeOut" parameter to IProcess.LoadModule
// 2002-09-30 1.2u (1) tray icon stuff now also works in XP
//                 (2) Fixed little bug in "QueryObjNtThread"
//                 (3) got rid of TIMiniStream, this reduces compiler warnings
// 2002-06-29 1.2t IWindow.BringToForeground(noBlink) now also works in XP
// 2002-03-08 1.2s Module names in NT had line feed characters at the end
// 2002-02-23 1.2r NewMutex did not enter, when the mutex already existed
// 2002-01-22 1.2q kernel32.OpenThread is now used when available
// 2001-12-05 1.2p the changes of the last version didn't work in NT4
// 2001-10-13 1.2o (1) TIThreads.RefreshItem and TIProcesses.RefreshItem failed
//                     in NT if too many many threads/processes were running
//                 (2) the whole Handle stuff now supports winXP
// 2001-10-01 1.2n TIMiniStream redefined, old design crashed sometimes
// 2001-07-08 1.2m (1) madDisAsm.CreateRemoteThread used where it makes sense
//                 (2) IThread./IProcess.IsStillRunning logic changed
//                 (3) some win9x internal hack structures corrected/extended
// 2001-05-22 1.2l little bug in TIProcess.Suspend/Resume fixed
// 2001-01-24 1.2k bug in GetHandleTableNt fixed (buffer too small)
// 2000-12-21 1.2j "IsMultiThread := true" added in NewThread
// 2000-12-07 1.2i bug in GetWindowPlacement calls
// 2000-11-23 1.2h little bug in [IProcess.]Module(memory) fixed
// 2000-10-25 1.2g found a new 9x hack for IProcess.CommandLine/.StartupInfo

unit madKernel;

{$I mad.inc}

interface

uses Windows, SysUtils, madBasic, madTypes, madTools, WinSock;

// ***************************************************************

const
  // constants for IThread.GetHandle access
  Thread_TERMINATE         = $0001;
  Thread_SUSPEND_RESUME    = $0002;
  Thread_GET_CONTEXT       = $0008;
  Thread_SET_CONTEXT       = $0010;
  Thread_SET_INFORMATION   = $0020;
  Thread_QUERY_INFORMATION = $0040;
  Thread_ALL_ACCESS        = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $3FF;

type
  // type for IKernelObj.ObjType
  TKernelObjType = (otUnknown,
                    otSemaphore,
                    otEvent,
                    otMutex,
                    otProcess,
                    otThread,
                    otTimer,
                    otFileMapping,
                    otFile,
                    otSnapshot,         // in winNT -> otFileMapping
                    otNotification,     // in winNT -> otFile
                    otComPort,          // in winNT -> otFile
                    otPipe,             // in winNT -> otFile
                    otMailslot,         // in winNT -> otFile
                    otSocket,           // in winNT -> otFile
                    otVxd,              // only win9x
                    otConsoleInput,     // only win9x
                    otConsoleOutput,    // only win9x
                    otDirectory,        // only winNT
                    otSymbolicLink,     // only winNT
                    otToken,            // only winNT
                    otWindowStation,    // only winNT
                    otDesktop,          // only winNT
                    otKey,              // only winNT
                    otPort,             // only winNT
                    otIoCompletion,     // only winNT
                    otKeyedEvent);      // only winXP

  // *******************************************************************

  // forward
  IHandle  = interface;
  IHandles = interface;

  // *******************************************************************

  // base interface for all kind of kernel objects
  IKernelObj = interface (IBasic) ['{72518460-8D63-11D3-A52E-00005A180D69}']
    // object type
    function GetObjType    : TKernelObjType;
    function GetObjTypeStr : AnsiString;
    property ObjType       : TKernelObjType read GetObjType;
    property ObjTypeStr    : AnsiString read GetObjTypeStr;

    // object address
    function GetObjAddr : pointer;
    property ObjAddr    : pointer read GetObjAddr;

    // object name  (only winNT)
    function GetObjName : AnsiString;
    property ObjName    : AnsiString read GetObjName;

    // object ID, only valid for a process or thread handle, otherwise 0
    function GetID : cardinal;
    property ID    : cardinal read GetID;

    // is this a auto event?
    // only valid for an event handle, otherwise false
    function IsAutoEvent : boolean;

    // if no handle is available yet, a new one is created (e.g. by OpenProcess)
    function GetHandle : IHandle;
    property Handle    : IHandle read GetHandle;

    // access to all already available handles for this kernel object
    function GetHandles : IHandles;
    property Handles    : IHandles read GetHandles;
  end;

  // *******************************************************************

  // forward
  IProcess = interface;

  // *******************************************************************

  // a kernel handle simply contains a kernel object with a specific access mask
  IHandle = interface (IBasic) ['{86522220-8323-11D3-A52D-00005A180D69}']
    // check whether this handle is still valid
    function IsStillValid : boolean;

    // to which process does this handle belong?
    function GetOwnerProcess : IProcess;
    property OwnerProcess    : IProcess read GetOwnerProcess;

    // handle value
    function GetHandle : cardinal;
    property Handle    : cardinal read GetHandle;

    // handle access
    function GetAccess : cardinal;
    property Access    : cardinal read GetAccess;

    // get the type of the kernel object, that is represented by this handle
    function GetObjType : TKernelObjType;
    property ObjType    : TKernelObjType read GetObjType;

    // get the kernel object, that is represented by this handle
    function GetKernelObj : IKernelObj;
    property KernelObj    : IKernelObj read GetKernelObj;

    // will this handle be automatically closed, when the interface gets destroyed?
    function  GetAutoClose : boolean;
    procedure SetAutoClose (value: boolean);
    property  AutoClose    : boolean read GetAutoClose write SetAutoClose;

    // tests, whether we would have to wait for this object right now
    function WouldWait : boolean;

    // wait, until this object enters "signaled state"
    // e.g. a process is in signaled state, if it is not running anymore
    function WaitFor (milliseconds   : cardinal = INFINITE;
                      handleMessages : boolean  = true    ) : boolean;

    // continue with our program right now, but send a "msg" to
    // the "window" when this object enters "signaled state"
    function Notify (window : cardinal;
                     msg    : cardinal) : boolean;

    // DuplicateHandle API
    // "targetProcess" can be: "IProcess", "IHandle", "handle" or "ID"
    // "targetProcess = 0" stands for the current process
    function Duplicate (autoClose           : boolean;
                        access              : cardinal;
                        inheritHandles      : boolean;
                        const targetProcess : IProcess       ) : IHandle; overload;
    function Duplicate (autoClose           : boolean;
                        access              : cardinal;
                        inheritHandles      : boolean;
                        const targetProcess : IHandle        ) : IHandle; overload;
    function Duplicate (autoClose           : boolean  = true;
                        access              : cardinal = 0;
                        inheritHandles      : boolean  = true;
                        targetProcess       : cardinal = 0   ) : IHandle; overload;
  end;

  // list of some specific handles
  IHandles = interface (ICustomBasicList) ['{B3017220-8338-11D3-A52D-00005A180D69}']
    // to which process (if only one) do all the handles in this list belong?
    function GetOwnerProcess : IProcess;
    property OwnerProcess    : IProcess read GetOwnerProcess;

    // to which kernel object (if only one) do all the handles in this list belong?
    function GetKernelObj : IKernelObj;
    property KernelObj    : IKernelObj read GetKernelObj;

    // read access to the items of this list
    function GetItem (index: integer) : IHandle;
    property Items   [index: integer] : IHandle read GetItem; default;

    // refresh the list, look for new/deleted/changed handles
    function RefreshItems : boolean;

    function WouldWait (waitAll: boolean) : boolean;

    // if "waitAll = false" and the wait was successful then
    // the index of the signaled object is returned in "index"
    function WaitFor (waitAll        : boolean   = false;
                      milliseconds   : cardinal  = INFINITE;
                      handleMessages : boolean   = true;
                      index          : TPInteger = nil     ) : boolean;

    function Notify (window  : cardinal;
                     msg     : cardinal;
                     waitAll : boolean = false) : boolean;
  end;

  // *******************************************************************

  // base interface for all kernel objects that can be used in wait functions
  IWaitableObj = interface (IKernelObj) ['{72518461-8D63-11D3-A52E-00005A180D69}']
    // tests, whether we would have to wait for this object right now
    function WouldWait : boolean;

    // wait, until this object enters "signaled state"
    // e.g. a process is in signaled state, if it is not running anymore
    function WaitFor (milliseconds   : cardinal = INFINITE;
                      handleMessages : boolean  = true    ) : boolean;

    // continue with our program right now, but send a "msg" to
    // the "window" when this object enters "signaled state"
    function Notify (window : cardinal;
                     msg    : cardinal) : boolean;
  end;

  // implement wait/notify functions for all IWaitableObj lists
  IWaitableObjs = interface (ICustomBasicList) ['{A1DB3222-8EB8-11D3-A52E-00005A180D69}']
    function WouldWait (waitAll: boolean) : boolean;

    // if "waitAll = false" and the wait was successful then
    // the index of the signaled object is returned in "index"
    function WaitFor (waitAll        : boolean   = false;
                      milliseconds   : cardinal  = INFINITE;
                      handleMessages : boolean   = true;
                      index          : TPInteger = nil     ) : boolean;

    function Notify (window  : cardinal;
                     msg     : cardinal;
                     waitAll : boolean = false) : boolean;
  end;

  // *******************************************************************

  // forward
  IThread = interface;

  // *******************************************************************

  // types for IWindow.GetChildWindow
  TSkipWindows  = (swInvisible, swDisabled, swTransparent);
  TSSkipWindows = set of TSkipWindows;

  // type for IWindow.WndProc
  TWndProc = function (window : cardinal;
                       msg    : cardinal;
                       wParam : integer;
                       lParam : integer ) : integer; stdcall;

  // type for IWindow.HookWndProc
//  TWndHookProc = function (var window  : cardinal;
//                           var msg     : cardinal;
//                           var wParam  : integer;
//                           var lParam  : integer;
//                           var handled : boolean ) : integer;

  // type for IWindow.SendMessage
  TSendAsyncProc = procedure (window       : cardinal;
                              msg          : cardinal;
                              callbackInfo : cardinal;
                              result       : integer ); stdcall;

  // encapsulation of the window APIs, some hidden undocumented functions
  IWindow = interface (IBasic) ['{F70A8420-C1D6-11D3-A530-00005A180D69}']
    // check whether this window is still valid
    function IsStillValid : boolean;

    // window handle, this is NO kernel handle!!
    function GetHandle : cardinal;
    property Handle    : cardinal read GetHandle;

    // get the text of the window
    // this is e.g. the caption of a main window or the content of an edit box
    function  GetText : string;
    procedure SetText (value: string);
    property  Text    : string read GetText write SetText;

    // is this window visible?
    function  GetVisible   : boolean;
    procedure SetVisible   (value    : boolean);
    property  Visible      : boolean read GetVisible write SetVisible;
    procedure Hide;
    procedure Show         (activate : boolean = true);

    // is this window enabled?
    function  GetEnabled : boolean;
    procedure SetEnabled (value: boolean);
    property  Enabled    : boolean read GetEnabled write SetEnabled;

    // is this window listed in the taskbar?
    function GetInTaskbar : boolean;
    property InTaskbar    : boolean read GetInTaskbar;

    // get the owner / parent window
    function  GetOwnerWindow  : IWindow;
    function  GetParentWindow : IWindow;
    procedure SetParentWindow (const value: IWindow);
    property  OwnerWindow     : IWindow read GetOwnerWindow;
    property  ParentWindow    : IWindow read GetParentWindow write SetParentWindow;

    // get the owner thread / process
    function GetOwnerThread  : IThread;
    function GetOwnerProcess : IProcess;
    property OwnerThread     : IThread  read GetOwnerThread;
    property OwnerProcess    : IProcess read GetOwnerProcess;

    // size / position of the window
    function  GetRect         : TRect;
    function  GetClientRect   : TRect;
    procedure SetRect         (value: TRect);
    procedure SetClientRect   (value: TRect);
    property  Rect            : TRect read GetRect       write SetRect;
    property  ClientRect      : TRect read GetClientRect write SetClientRect;
    function  SetRectEx       (rect     : TRect;
                               copyBits : boolean = true;
                               redraw   : boolean = true;
                               activate : boolean = true) : boolean;
    function  SetClientRectEx (rect     : TRect;
                               copyBits : boolean = true;
                               redraw   : boolean = true;
                               activate : boolean = true) : boolean;

    // minimize / maximize / restore
    function  GetMinimized : boolean;
    function  GetMaximized : boolean;
    function  GetRestored  : boolean;
    procedure SetMinimized (value: boolean);
    procedure SetMaximized (value: boolean);
    procedure SetRestored  (value: boolean);
    property  Minimized    : boolean read GetMinimized write SetMinimized;
    property  Maximized    : boolean read GetMaximized write SetMaximized;
    property  Restored     : boolean read GetRestored  write SetRestored;
    procedure Minimize     (activate: boolean = false);
    procedure Maximize     (activate: boolean = true );
    procedure Restore      (activate: boolean = true );

    // minimal / maximal / normal window size
    function  GetMinimizePos : TPoint;
    function  GetMaximizePos : TPoint;
    function  GetNormalPos   : TRect;
    procedure SetMinimizePos (value: TPoint);
    procedure SetMaximizePos (value: TPoint);
    procedure SetNormalPos   (value: TRect );
    property  MinimizePos    : TPoint read GetMinimizePos write SetMinimizePos;
    property  MaximizePos    : TPoint read GetMaximizePos write SetMaximizePos;
    property  NormalPos      : TRect  read GetNormalPos   write SetNormalPos;

    // only application wide, extended BringWindowToTop API
    function BringToTop (copyBits : boolean = true;
                         redraw   : boolean = true;
                         activate : boolean = true) : boolean;

    // brings the window to the foreground
    // "noBlick" makes sense only for new shell versions (e.g. win98, win2000)
    function BringToForeground (noBlink: boolean = true) : boolean;

    // does this window always stay on top?
    function  GetStayOnTop   : boolean;
    procedure SetStayOnTop   (value: boolean);
    property  StayOnTop      : boolean read GetStayOnTop write SetStayOnTop;
    function  SetStayOnTopEx (onTop    : boolean;
                              copyBits : boolean = true;
                              redraw   : boolean = true;
                              activate : boolean = true) : boolean;

    // parameters of the window (GetWindowLong / SetWindowLong API)
    function  GetParam (index: integer) : integer;
    procedure SetParam (index: integer; value: integer);
    property  Param    [index: integer] : integer read GetParam write SetParam;

    // style / extended style of the window
    function  GetStyle   : integer;
    function  GetExStyle : integer;
    procedure SetStyle   (value: integer);
    procedure SetExStyle (value: integer);
    property  Style      : integer read GetStyle   write SetStyle;
    property  ExStyle    : integer read GetExStyle write SetExStyle;

    // ShowOwnedPopup API
    function ShowOwnedPopups (show: boolean = true) : boolean;

    // find a child window
    function ChildWindow (pos       : TPoint;
                          skip      : TSSkipWindows = [] ) : IWindow; overload;
    function ChildWindow (className : AnsiString;
                          caption   : AnsiString    = '*') : IWindow; overload;

    // get the current top child window
    function GetTopChildWindow : IWindow;
    property TopChildWindow    : IWindow read GetTopChildWindow;

    // ask / set / change / delete a window property
    function  GetProp    (name: AnsiString) : cardinal;
    procedure SetProp    (name: AnsiString; value: cardinal);
    property  Prop       [name: AnsiString] : cardinal read GetProp write SetProp;
    function  DeleteProp (name: AnsiString) : boolean;

    // get a list of the window properties
//    function GetProps : IWindowProps;
//    property Props    : IWindowProps read GetProps;

    // is this a native Unicode window?
    function IsUnicode : boolean;

    // only valid if this window was created by a Delphi TWinControl object
    function SelfAsTWinControl : TObject;

    // if you set the window procedure of a window that belongs to another process
    // the code of this new window procedure MUST be accessible by the other process
    function  GetWndProc : TWndProc;
    procedure SetWndProc (value: TWndProc);
    property  WndProc    : TWndProc read GetWndProc write SetWndProc;

    // hook the window procedure without using any dlls (!)
//    function Hook   (hookProc : TWndHookProc  ) : boolean; overload;
//    function Hook   (window   : cardinal;
//                     viaSend  : boolean = true) : boolean; overload;
//    function Unhook : boolean;

    // post /send a message to the window
    function PostMessage (msg                  : cardinal;
                          wParam               : integer  = 0;
                          lParam               : integer  = 0       ) : boolean;
    function SendMessage (msg                  : cardinal;
                          wParam               : integer  = 0;
                          lParam               : integer  = 0;
                          timeOut              : cardinal = INFINITE;
                          abortIfHung          : boolean  = false;
                          blockThread          : boolean  = false;
                          noTimeOutWhenNotHung : boolean  = false   ) : integer; overload;
    function SendMessage (msg                  : cardinal;
                          wParam               : integer;
                          lParam               : integer;
                          callbackProc         : TSendAsyncProc;
                          callbackInfo         : cardinal = 0       ) : boolean; overload;

    // close the window
    function Close : boolean;

    // destroy the window
    function Destroy : boolean;

    // get the name of the window class
    function GetClassName : AnsiString;
    property ClassName    : AnsiString read GetClassName;

    // get the background brush of the window class
    function  GetBackgroundBrush : cardinal;
    procedure SetBackgroundBrush (value: cardinal);
    property  BackgroundBrush    : cardinal read GetBackgroundBrush write SetBackgroundBrush;

    // get the cursor of the window class
    function  GetCursor : cardinal;
    procedure SetCursor (value: cardinal);
    property  Cursor    : cardinal read GetCursor write SetCursor;

    // get the icon / small icon of the window class
    function  GetIcon      : cardinal;
    function  GetSmallIcon : cardinal;
    procedure SetIcon      (value: cardinal);
    procedure SetSmallIcon (value: cardinal);
    property  Icon         : cardinal read GetIcon      write SetIcon;
    property  SmallIcon    : cardinal read GetSmallIcon write SetSmallIcon;
  end;

  // contains a list of specific IWindow objects
  IWindows = interface (ICustomBasicList) ['{58831900-C282-11D3-A530-00005A180D69}']
    // valid if this list contains only windows of a specific thread / process
    function GetOwnerThread  : IThread;
    function GetOwnerProcess : IProcess;
    property OwnerThread     : IThread  read GetOwnerThread;
    property OwnerProcess    : IProcess read GetOwnerProcess;

    // valid if this list contains only windows that are listed in the taskbar
    function GetInTaskbar : boolean;
    property InTaskbar    : boolean read GetInTaskbar;

    procedure Hide;
    procedure Show (activate: boolean = true);

    procedure SetEnabled (value: boolean);

    procedure Minimize (activate: boolean = false);
    procedure Maximize (activate: boolean = true );
    procedure Restore  (activate: boolean = true );

    function SetStayOnTop (onTop    : boolean;
                           copyBits : boolean = true;
                           redraw   : boolean = true;
                           activate : boolean = true) : boolean;

    function ShowOwnedPopups (show: boolean = true) : boolean;

    function PostMessage (msg                  : cardinal;
                          wParam               : integer  = 0;
                          lParam               : integer  = 0       ) : boolean;

    function Close : boolean;

    function Destroy : boolean;

    // access to the items of this list
    function GetItem (index: integer) : IWindow;
    property Items   [index: integer] : IWindow read GetItem; default;

    // refresh the list, look for new/deleted/changed windows
    function RefreshItems : boolean;
  end;

  // *******************************************************************

  // encapsulation of the tray icon APIs (Shell_NotifyIcon) and some extensions
  ITrayIcon = interface (IBasic) ['{4BFE6700-C522-11D3-A530-00005A180D69}']
    // application defined identifier for the tray icon
    function ID : cardinal;

    // window that receives the messages for the tray icon and
    // application defined message which is used for the notification messages
    function  GetWindow  : IWindow;
    function  GetMessage : cardinal;
    procedure SetWindow  (const value: IWindow );
    procedure SetMessage (      value: cardinal);
    property  Window     : IWindow  read GetWindow  write SetWindow;
    property  Message    : cardinal read GetMessage write SetMessage;

    // which hint is shown for this tray icon?
    function  GetHint : AnsiString;
    procedure SetHint (value: AnsiString);
    property  Hint    : AnsiString read GetHint write SetHint;

    // which index in the imageList does the icon of the tray icon has?
    // makes only sense in connection with the "imageList" property of ITrayIcons
    function GetImageIndex : integer;
    property ImageIndex    : integer read GetImageIndex;

    // ask / change the icon
    function  GetIcon : cardinal;
    procedure SetIcon (value: cardinal);
    property  Icon    : cardinal read GetIcon write SetIcon;

    // ask the position / size of the tray icon
    function GetRect : TRect;
    property Rect    : TRect read GetRect;

    // simulate mouse events
    procedure MouseMove;
    procedure MouseClick (right: boolean = true; Double: boolean = false);

    // temporarily hide the tray icon
    // when the interface is destroyed the icon is automatically shown again
    function  GetVisible : boolean;
    procedure SetVisible (value: boolean);
    property  Visible    : boolean read GetVisible write SetVisible;
    function  Hide       : boolean;
    function  Show       : boolean;

    // delete this tray icon completely
    function Delete : boolean;
  end;

  // contains a list of specific ITrayIcons objects
  ITrayIcons = interface (ICustomBasicList) ['{4BFE6701-C522-11D3-A530-00005A180D69}']
    // valid if this list contains only tray icons that belong to a specific process
    function GetOwnerProcess : IProcess;
    property OwnerProcess    : IProcess read GetOwnerProcess;

    // image list, in which all the icons of the tray icons in this list are contained
    // makes only sense in connection with the "imageIndex" property of ITrayIcon
    function GetImageList : cardinal;
    property ImageList    : cardinal read GetImageList;

    function Hide : boolean;
    function Show : boolean;

    function Delete : boolean;

    // access to the items of this list
    function GetItem (index: integer) : ITrayIcon;
    property Items   [index: integer] : ITrayIcon read GetItem; default;

    // refresh the list, look for new/deleted/changed tray icons
    function RefreshItems : boolean;
  end;

  // *******************************************************************

  // forward
  IModule = interface;

  // module import/export list
  IExportEntry = interface (IBasic) ['{64B5B8D1-B4EA-4A13-9D48-CBACF2265882}']
    // to which process does this module belong
    function GetExportModule : IModule;
    property ExportModule    : IModule read GetExportModule;

    // import/export ordinal of this entry
    function GetOrdinal : cardinal;
    property Ordinal    : cardinal read GetOrdinal;

    // import/export name of this entry
    function GetName : AnsiString;
    property Name    : AnsiString read GetName;

    // address of the function
    function GetAddress : pointer;
    property Address    : pointer read GetAddress;
  end;
  IXxportList = interface (ICustomBasicList) ['{AF45CC02-1296-47C2-B6AE-727B3C82B820}']
    // read access to the import entries
    function GetItem (index: integer) : IExportEntry;
    property Items   [index: integer] : IExportEntry read GetItem; default;

    // looks through all items and returns the found item
    function FindItem (func: pointer) : IExportEntry; overload;
  end;

  // information about a module (DLL or executable or something similar)
  IModule = interface (IBasic) ['{F84BE460-C13F-11D3-A530-00005A180D69}']
    // check whether this module is still valid
    function IsStillValid : boolean;

    // to which process does this module belong
    function GetOwnerProcess : IProcess;
    property OwnerProcess    : IProcess read GetOwnerProcess;

    // get the module handle, this is NO kernel handle!!
    function GetHandle : cardinal;
    property Handle    : cardinal read GetHandle;

    // get the HInstance value of this module
    function GetHInstance : cardinal;
    property HInstance    : cardinal read GetHInstance;

    // get the memory position the module is loaded to
    function GetMemory : pointer;
    property Memory    : pointer read GetMemory;

    // get the file name of the module (under NT with full path)
    function GetFileName : AnsiString;
    property FileName    : AnsiString read GetFileName;

    // is this the module from that this process was created?
    function IsMainModule : boolean;

    // will this module be automatically closed, when the interface gets destroyed?
    function  GetAutoClose : boolean;
    procedure SetAutoClose (value: boolean);
    property  AutoClose    : boolean read GetAutoClose write SetAutoClose;

    // get the address of the exported functions
    function GetProcAddress (name  : AnsiString) : pointer; overload;
    function GetProcAddress (index : integer   ) : pointer; overload;

    // manipulate the address of the exported functions (caution!!)
    function SetProcAddress (name  : AnsiString; newAddr: pointer) : boolean; overload;
    function SetProcAddress (index : integer;    newAddr: pointer) : boolean; overload;

    // get the complete import/export list
    function GetImportList : IXxportList;
    function GetExportList : IXxportList;
    property ImportList    : IXxportList read GetImportList;
    property ExportList    : IXxportList read GetExportList;

    // get the module's entry point
    function GetEntryPoint : pointer;
    property EntryPoint    : pointer read GetEntryPoint;

    // get the image nt headers
    function GetImageNtHeaders : PImageNtHeaders;
    property ImageNtHeaders    : PImageNtHeaders read GetImageNtHeaders;

    // get the image import/export directory
    function GetImageImportDirectory : PImageImportDirectory;
    function GetImageExportDirectory : PImageExportDirectory;
    property ImageImportDirectory    : PImageImportDirectory read GetImageImportDirectory;
    property ImageExportDirectory    : PImageExportDirectory read GetImageExportDirectory;
  end;

  // contains a list of specific IModule objects
  IModules = interface (ICustomBasicList) ['{F84BE461-C13F-11D3-A530-00005A180D69}']
    // valid if this list contains only modules of a specific process
    function GetOwnerProcess : IProcess;
    property OwnerProcess    : IProcess read GetOwnerProcess;

    // valid if this list contains only of the same module
    function GetFileName : AnsiString;
    property FileName    : AnsiString read GetFileName;

    // access to the items of this list
    function GetItem (index: integer) : IModule;
    property Items   [index: integer] : IModule read GetItem; default;

    // refresh the list, look for new/deleted/changed modules
    function RefreshItems : boolean;
  end;

  // *******************************************************************

  // encapsulation of the thread APIs, some hidden undocumented functions
  IThread = interface (IWaitableObj) ['{A1DB3220-8EB8-11D3-A52E-00005A180D69}']
    // check whether this thread is still valid
    function IsStillValid : boolean;

    // get a handle with minimum access
    // the returned handle may have more access (due to handle storing)
    function GetHandle (access: cardinal = THREAD_ALL_ACCESS) : IHandle;

    // store a thread handle to avoid constantly opening/closing  (only win9x)
    // uses reference counting, call ReleaseHandle 1x for every StoreHandle
    // winNT has no handle restriction, so we always store our handles there
    procedure StoreHandle;
    procedure ReleaseHandle;

    // to which process does this thread belong?
    function GetOwnerProcess : IProcess;
    property OwnerProcess    : IProcess read GetOwnerProcess;

    // get a list of all windows that belong to this thread
    function GetWindows : IWindows;
    property Windows_   : IWindows read GetWindows;

    // get a list of all windows that belong to this thread and are visible in the taskbar
    function GetTaskbarWindows : IWindows;
    property TaskbarWindows    : IWindows read GetTaskbarWindows;

    // GetThreadPriority / SetThreadPriority
    function  GetPriority : integer;
    procedure SetPriority (priority: integer);
    property  Priority    : integer read GetPriority write SetPriority;

    // GetThreadPriorityBoost / SetThreadPriorityBoost
    function  GetPriorityBoost : boolean;
    procedure SetPriorityBoost (priorityBoost: boolean);
    property  PriorityBoost    : boolean read GetPriorityBoost write SetPriorityBoost;

    // SetThreadAffinityMask / SetThreadIdealProcessor
    function SetAffinityMask   (affinityMask    : cardinal;
                                oldAffinityMask : TPCardinal = nil) : boolean;
    function SetIdealProcessor (processorNo     : cardinal;
                                oldProcessorNo  : TPCardinal = nil) : boolean;

    // PostThreadMessage
    function PostMessage (msg    : cardinal;
                          wParam : integer = 0;
                          lParam : integer = 0) : boolean;

    // SuspendThread / ResumeThread
    function IsSuspended : boolean;
    function Suspend (oldSuspendCount: TPCardinal = nil) : boolean;
    function Resume  (oldSuspendCount: TPCardinal = nil) : boolean;

    // GetThreadContext / SetThreadContext
    function  GetContext : TContext;
    procedure SetContext (const context: TContext);
    property  Context    : TContext read GetContext write SetContext;

    // AttachThreadInput
    // "thread" can be: "IThread", "IHandle", "handle" or "ID"
    // "thread = 0" stands for the current thread
    function AttachInput (const thread: IThread     ) : boolean; overload;
    function AttachInput (const thread: IHandle     ) : boolean; overload;
    function AttachInput (      thread: cardinal = 0) : boolean; overload;
    function DetachInput (const thread: IThread     ) : boolean; overload;
    function DetachInput (const thread: IHandle     ) : boolean; overload;
    function DetachInput (      thread: cardinal = 0) : boolean; overload;

    // PostThreadMessage(WM_QUIT, exitCode, 0)
    function PostQuitMessage (exitCode: cardinal = 0) : boolean;

    // not WouldWait
    function IsStillRunning : boolean;

    // TerminateThread
    function Terminate (exitCode: cardinal = 0) : boolean;

    // GetExitCodeThread
    function GetExitCode : cardinal;
    property ExitCode    : cardinal read GetExitCode;

    // GetThreadTimes
    function GetTimes (var creation, exit, kernel, user: int64) : boolean;
  end;

  // contains a list of specific IThread objects
  IThreads = interface (IWaitableObjs) ['{A1DB3221-8EB8-11D3-A52E-00005A180D69}']
    // valid if this list contains only threads of a specific process
    function GetOwnerProcess : IProcess;
    property OwnerProcess    : IProcess read GetOwnerProcess;

    // access to the items of this list
    function GetItem (index: integer) : IThread;
    property Items   [index: integer] : IThread read GetItem; default;

    // refresh the list, look for new/deleted/changed threads
    function RefreshItems : boolean;

    procedure SetPriority (priority: integer);
    property  Priority    : integer write SetPriority;

    procedure SetPriorityBoost (priorityBoost: boolean);
    property  PriorityBoost    : boolean write SetPriorityBoost;

    function SetAffinityMask   (affinityMask : cardinal) : boolean;
    function SetIdealProcessor (processorNo  : cardinal) : boolean;

    function PostMessage (msg    : cardinal;
                          wParam : integer = 0;
                          lParam : integer = 0) : boolean;

    function Suspend : boolean;
    function Resume  : boolean;

    function PostQuitMessage (exitCode: cardinal = 0) : boolean;

    function IsStillRunning (all: boolean = false) : boolean;

    function Terminate (exitCode: cardinal = 0) : boolean;
  end;

  // *******************************************************************

  // type for IProcess.OsVersion
  TOsVersion = packed record
    minor : word;
    major : word;
  end;

  // type for IProcess.ExeType
  TExeType = (etUnknown, etDos, etWin16, etConsole, etWin32);

  // type for IProcess.PriorityClass
  TPriorityClass = (pcUnknown, pcIdle, pcBelowNormal, pcNormal, pcAboveNormal, pcHigh, pcRealTime);

  TExecuteFunctionProc = procedure (var params);

  // encapsulation of the process APIs, some hidden undocumented functions
  IProcess = interface (IWaitableObj) ['{D2097382-9AB1-11D3-A530-00005A180D69}']
    // check whether this process is still valid
    function IsStillValid : boolean;

    // get the exe file name (including the path) of this process
    function GetExeFile : AnsiString;
    property ExeFile    : AnsiString read GetExeFile;

    function GetSession : dword;
    property Session    : dword read GetSession;

    // get a handle with minimum access
    // the returned handle may have more access (due to handle storing/caching)
    function GetHandle (access: cardinal = PROCESS_ALL_ACCESS) : IHandle;

    // store a process handle to avoid constantly opening/closing  (only win9x)
    // uses reference counting, call ReleaseHandle 1x for every StoreHandle
    // winNT has no handle restriction, so we always store our handles there
    procedure StoreHandle;
    procedure ReleaseHandle;

    // who is the creator of this process?
    function GetParentProcess : IProcess;
    property ParentProcess    : IProcess read GetParentProcess;

    // get the version of the OS for that this process is built
    function GetOsVersion : TOsVersion;
    property OsVersion    : TOsVersion read GetOsVersion;

    // get the exe type of this process
    function GetExeType : TExeType;
    property ExeType    : TExeType read GetExeType;

    // get the icon that belongs to the process
    function Icon (smallIcon             : boolean = true;
                   linkOverlayIfLnkOrPif : boolean = false) : cardinal;

    // get the HInstance value of this process (= pointer to the exe image)
    function GetHInstance : cardinal;
    property HInstance    : cardinal read GetHInstance;

    // get the module which was used to create this process
    function GetMainModule : IModule;
    property MainModule    : IModule read GetMainModule;

    // is this process a system process or a normal application?
    // in the winNT family this is a read only property
    function  GetServiceProcess : boolean;
    procedure SetServiceProcess (serviceProcess: boolean = true);
    property  ServiceProcess    : boolean read GetServiceProcess write SetServiceProcess;

    // GetPriorityClass / SetPriorityClass
    function  GetPriorityClass : TPriorityClass;
    procedure SetPriorityClass (priorityClass: TPriorityClass);
    property  PriorityClass    : TPriorityClass read GetPriorityClass write SetPriorityClass;

    // GetProcessPriorityBoost / SetProcessPriorityBoost
    function  GetPriorityBoost : boolean;
    procedure SetPriorityBoost (priorityBoost: boolean);
    property  PriorityBoost    : boolean read GetPriorityBoost write SetPriorityBoost;

    // GetProcessAffinityMask / SetProcessAffinityMask
    function  GetAffinityMask : cardinal;
    procedure SetAffinityMask (affinityMask: cardinal);
    property  AffinityMask    : cardinal read GetAffinityMask write SetAffinityMask;

    // GetProcessWorkingSetSize / SetProcessWorkingSetSize
    procedure GetWorkingSetSize (var minimum, maximum: cardinal);
    function  SetWorkingSetSize (    minimum, maximum: cardinal) : boolean;

    // OpenProcessToken + GetTokenInformation(tokenLevel)
    // the resulting string is a data buffer containing the requested record
    function GetTokenInformation (tokenLevel: dword) : AnsiString;
    property TokenInformation [tokenLevel: dword] : AnsiString read GetTokenInformation;

    // LookupAccountSid(GetTokenInformation(TokenUser))
    function GetUserName : AnsiString;
    property UserName : AnsiString read GetUserName;

    // GetCommandLine
    function GetCommandLine : AnsiString;
    property CommandLine    : AnsiString read GetCommandLine;

    // GetStartupInfo
    function GetStartupInfo : TStartupInfo;
    property StartupInfo    : TStartupInfo read GetStartupInfo;

    // winNT: VirtualAllocEx/VirtualFreeEx
    // win9x: (1) undocumented shared memory allocation functions or
    //        (2) LocalAlloc/LocalFree in the context of this process
    //        depending on the "mayUseSharedArea" parameter
    //        Windows automatically deallocates shared memory when our program ends
    function AllocMem (size             : integer;
                       mayUseSharedArea : boolean = false) : pointer;
    function FreeMem  (var ptr          : pointer        ) : boolean;

    // ReadProcessMemory / WriteProcessMemory
    function ReadMemory  (const source; var dest; count: integer) : boolean;
    function WriteMemory (const source; var dest; count: integer) : boolean;
    function zeroMemory  (              var dest; count: integer) : boolean;

    // get a list of all threads that belong to this process
    function GetThreads : IThreads;
    property Threads    : IThreads read GetThreads;

    // madRemote.CreateRemoteThreadEx
    function CreateThread (startAddr     : pointer;
                           parameter     : pointer             =   nil;
                           creationFlags : cardinal            =     0;
                           stackSize     : cardinal            =     0;
                           threadAttr    : PSecurityAttributes =   nil) : IThread;

    // handle -> IModule / memory -> IModule / fileName -> IModule
    function Module (handle    : cardinal;
                     autoClose : boolean = false) : IModule; overload;
    function Module (memory    : pointer;
                     autoClose : boolean = false) : IModule; overload;
    function Module (fileName  : AnsiString;
                     autoClose : boolean = false) : IModule; overload;

    // loads a module into this process (API LoadLibrary(Ex))
    function LoadModule (fileName          : AnsiString;
                         autoClose         : boolean = true;
                         withoutReferences : boolean = false;
                         onlyAsDataFile    : boolean = false;
                         alteredSearchPath : boolean = false;
                         timeOut           : integer = 3000 ) : IModule;

    // get a list of all modules that were loaded in this process
    function GetModules : IModules;
    property Modules    : IModules read GetModules;

    // get a list of all exported functions of all modules loaded in this process
    function GetExportList : IXxportList;
    property ExportList    : IXxportList read GetExportList;

    // get a list of all tray icons that belong to this process
    function GetTrayIcons : ITrayIcons;
    property TrayIcons    : ITrayIcons read GetTrayIcons;

    // get a list of all windows that belong to this process
    function GetWindows : IWindows;
    property Windows_   : IWindows read GetWindows;

    // get a list of all windows that belong to this process and are visible in the taskbar
    function GetTaskbarWindows : IWindows;
    property TaskbarWindows    : IWindows read GetTaskbarWindows;

    // get a list of all handles that are opened from this process
    function GetHandles : IHandles;
    property Handles    : IHandles read GetHandles;

    // minimize/maximize/restore this process
    function Minimize (activate: boolean = false) : boolean;
    function Maximize (activate: boolean = true ) : boolean;
    function Restore  (activate: boolean = true ) : boolean;

    // copies the function "func" from our process to the other process and executes it
    // all parameters must be valid in OUR process
    // they're automatically mapped to the context of the other process  
    function ExecuteFunction (func                 : TExecuteFunctionProc;
                              timeOut              : cardinal = 1000;
                              params               : pointer  = nil;
                              paramsSize           : cardinal = 0;
                              acceptUnknownTargets : boolean  = false) : boolean;

    // suspend/resume this process
    function Suspend : boolean;
    function Resume  : boolean;

    // win32 WaitForInputIdle API
    function WaitForInputIdle (timeOut: cardinal = INFINITE) : boolean;

    // not WouldWait
    function IsStillRunning : boolean;

    // close this process  (WM_CLOSE to the main window)
    function Close : boolean;

    // quit this process  (WM_QUIT to the main thread)
    function Quit  : boolean;

    // ExitProcess (in the context of the process!!)
    function Exit_ (exitCode: cardinal = 0) : boolean;

    // TerminateProcess
    function Terminate (exitCode: cardinal = 0) : boolean;

    // GetExitCodeProcess
    function GetExitCode : cardinal;
    property ExitCode    : cardinal read GetExitCode;

    // GetProcessTimes
    function GetTimes (var creation, exit, kernel, user: int64) : boolean;
  end;

  // contains a list of specific IProcess objects
  IProcesses = interface (IWaitableObjs) ['{D2097381-9AB1-11D3-A530-00005A180D69}']
    // valid if this list contains only the running instances of one application
    function GetExeFile : AnsiString;
    property ExeFile    : AnsiString read GetExeFile;

    // access to the items of this list
    function GetItem (index: integer) : IProcess;
    property Items   [index: integer] : IProcess read GetItem; default;

    // refresh the list, look for new/deleted/changed processes
    function RefreshItems : boolean;

    procedure SetServiceProcess (serviceProcess: boolean = true);

    procedure SetPriorityClass (priorityClass: TPriorityClass);

    procedure SetPriorityBoost (priorityBoost: boolean);

    procedure SetAffinityMask (affinityMask: cardinal);

    function SetWorkingSetSize (minimum, maximum: cardinal) : boolean;

    function Minimize (activate: boolean = false) : boolean;
    function Maximize (activate: boolean = true ) : boolean;
    function Restore  (activate: boolean = true ) : boolean;

    function Suspend : boolean;
    function Resume  : boolean;

    function IsStillRunning (all: boolean = false) : boolean;

    function Close : boolean;

    function Quit  : boolean;

    function Exit_ (exitCode: cardinal = 0) : boolean;

    function Terminate (exitCode: cardinal = 0) : boolean;
  end;

// ***************************************************************

// handle -> IHandle
function Handle (handle    : cardinal;
                 autoClose : boolean = true) : IHandle; overload;

// get a list of all handles (system wide)
function Handles (systemWide: boolean = false) : IHandles;

// pick all IHandle interfaces
function PickHandles (const objects: array of IBasic ) : IHandles; overload;
function PickHandles (const objects: ICustomBasicList) : IHandles; overload;

// ***************************************************************

// types and constants for the Wait and Notify functions
type
  TWaitForMessage   = (wfKey, wfMouseMove, wfMouseButton, wfPostMessage,
                       wfTimer, wfPaint, wfSendMessage, wfHotKey, wfAllPostMessage);
  TWaitForMessages  = set of TWaitForMessage;
const
  CWaitForMouse     : TWaitForMessages = [       wfMouseMove, wfMouseButton];
  CWaitForInput     : TWaitForMessages = [wfKey, wfMouseMove, wfMouseButton];
  CWaitForAllEvents : TWaitForMessages = [wfKey, wfMouseMove, wfMouseButton,
                                          wfPostMessage, wfTimer, wfPaint,
                                                         wfHotKey];
  CWaitForAllInput  : TWaitForMessages = [wfKey, wfMouseMove, wfMouseButton,
                                          wfPostMessage, wfTimer, wfPaint,
                                          wfSendMessage, wfHotKey];

// would we have to wait for these objects?
function WouldWait (const objects : array of IBasic;
                    waitAll       : boolean = false ) : boolean; overload;
function WouldWait (const objects : ICustomBasicList;
                    waitAll       : boolean = false ) : boolean; overload;

// wait, until one or all of the objects are in signaled state
function WaitFor (const objects  : array of IBasic;
                  waitAll        : boolean          = false;
                  milliseconds   : cardinal         = INFINITE;
                  handleMessages : boolean          = true;
                  msgs           : TWaitForMessages = [];
                  alertable      : boolean          = false   ) : integer; overload;
function WaitFor (const objects  : ICustomBasicList;
                  waitAll        : boolean          = false;
                  milliseconds   : cardinal         = INFINITE;
                  handleMessages : boolean          = true;
                  msgs           : TWaitForMessages = [];
                  alertable      : boolean          = false   ) : integer; overload;

// continue with our program right now, but send a "msg" to the "window"
// when one or all of the objects are in signaled state
function Notify (window        : cardinal;
                 msg           : cardinal;
                 const objects : array of IBasic;
                 waitAll       : boolean          {= false};
                 msgs          : TWaitForMessages {= []};
                 alertable     : boolean          {= false}) : boolean; overload;
function Notify (window        : cardinal;
                 msg           : cardinal;
                 const objects : ICustomBasicList;
                 waitAll       : boolean          = false;
                 msgs          : TWaitForMessages = [];
                 alertable     : boolean          = false) : boolean; overload;

// ***************************************************************

// handle -> IWindow / caption/className -> IWindow / pos -> IWindow
function Window (handle    : cardinal        ) : IWindow; overload;
function Window (className : AnsiString;
                 caption   : AnsiString = '*') : IWindow; overload;
function Window (pos       : TPoint          ) : IWindow; overload;

// get the foreground window
function ForegroundWindow : IWindow;

// get the window that is focused in the current thread
function FocusedWindow : IWindow;

// get the desktop window
function DesktopWindow : IWindow;

// get the desktop's list view
function DesktopListView : IWindow;

// get special windows from the taskbar
function Taskbar          : IWindow;
function StartButton      : IWindow;
function TaskButtonWindow : IWindow;
function TrayWindow       : IWindow;
function ClockWindow      : IWindow;

// get a list of all Windows process wide or system wide
function Windows_ (systemWide: boolean = false) : IWindows;

// get a list of all Windows that are visible in the taskbar process wide or system wide
function TaskbarWindows (systemWide: boolean = true) : IWindows;

// pick all IWindow interfaces
function PickWindows (const objects: array of IBasic ) : IWindows; overload;
function PickWindows (const objects: ICustomBasicList) : IWindows; overload;

// ***************************************************************

// get a list of all TrayIcons process wide or system wide
function TrayIcons (systemWide: boolean = true) : ITrayIcons;

// pick all ITrayIcon interfaces
function PickTrayIcons (const objects: array of IBasic ) : ITrayIcons; overload;
function PickTrayIcons (const objects: ICustomBasicList) : ITrayIcons; overload;

// ***************************************************************

// handle -> IModule / memory -> IModule / fileName -> IModule
function Module (handle        : cardinal;
                 autoClose     : boolean = false) : IModule; overload;
function Module (memory        : pointer;
                 autoClose     : boolean = false) : IModule; overload;
function Module (fileName      : AnsiString;
                 autoClose     : boolean = false) : IModule; overload;

// get the current module
function CurrentModule : IModule;

// get the module which was used to create this process
function MainModule : IModule;

// loads a module into the current process (API LoadLibrary(Ex))
function LoadModule (fileName          : AnsiString;
                     autoClose         : boolean = true;
                     withoutReferences : boolean = false;
                     onlyAsDataFile    : boolean = false;
                     alteredSearchPath : boolean = false) : IModule;

// get a list of all modules process wide or system wide
function Modules (systemWide: boolean = false) : IModules; overload;

// get a list in which applications the module "fileName" is loaded
function Modules (fileName: AnsiString) : IModules; overload;

// pick all IModule interfaces
function PickModules (const objects: array of IBasic ) : IModules; overload;
function PickModules (const objects: ICustomBasicList) : IModules; overload;

// ***************************************************************

type
  // type for NewThread
  TThreadFunc = function (param: pointer) : cardinal; stdcall;

// IHandle -> IThread / handle/id -> IThread
function Thread (const thread : IHandle        ) : IThread; overload;
function Thread (thread       : cardinal;
                 autoClose    : boolean  = true) : IThread; overload;

// get the current thread
function CurrentThread : IThread;

// get the main thread
function MainThread : IThread;

// create a new thread in our process
function NewThread (threadFunc    : TThreadFunc;
                    parameter     : pointer             = nil;
                    creationFlags : cardinal            = 0;
                    stackSize     : cardinal            = 0;
                    threadAttr    : PSecurityAttributes = nil) : IThread;

// get a list of all threads process wide or system wide
function Threads (systemWide: boolean = false) : IThreads;

// pick all IThread interfaces
function PickThreads (const objects: array of IBasic ) : IThreads; overload;
function PickThreads (const objects: ICustomBasicList) : IThreads; overload;

// ***************************************************************

// IHandle -> IProcess / handle/id -> IProcess / exeFile -> IProcess
// "process" can be: "IHandle", "handle" or "ID"
function Process (const process : IHandle       ) : IProcess; overload;
function Process (process       : cardinal;
                  autoClose     : boolean = true) : IProcess; overload;
function Process (exeFile       : AnsiString    ) : IProcess; overload;

// get the current process
function CurrentProcess : IProcess;

// get the process that created the current process
function ParentProcess : IProcess;

// create a new process
function NewProcess (exeFile        : AnsiString;
                     params         : AnsiString          = '';
                     workingDir     : AnsiString          = '';
                     showCmd        : cardinal            = SW_SHOWDEFAULT;
                     creationFlags  : cardinal            = 0;
                     processAttr    : PSecurityAttributes = nil;
                     threadAttr     : PSecurityAttributes = nil;
                     inheritHandles : boolean             = true;
                     environment    : pointer             = nil           ) : IProcess;

// create a new process under a different user account                     
function NewProcessAsUser (userName       : UnicodeString;
                           password       : UnicodeString;
                           domain         : UnicodeString;
                           exeFile        : AnsiString;
                           params         : AnsiString          = '';
                           workingDir     : AnsiString          = '';
                           showCmd        : cardinal            = SW_SHOWDEFAULT;
                           creationFlags  : cardinal            = 0;
                           processAttr    : PSecurityAttributes = nil;
                           threadAttr     : PSecurityAttributes = nil;
                           inheritHandles : boolean             = true;
                           environment    : pointer             = nil           ) : IProcess;

// prints a file
function PrintFile (file_      : AnsiString;
                    workingDir : AnsiString = '';
                    showCmd    : cardinal   = SW_SHOWNORMAL) : IProcess;

// get a list of all running processes or a list of all instances of one application
function Processes (exeFile: AnsiString = '*') : IProcesses; overload;

// pick all IProcess interfaces
function PickProcesses(const objects: array of IBasic ) : IProcesses; overload;
function PickProcesses(const objects: ICustomBasicList) : IProcesses; overload;

// ***************************************************************

type
  // interface for all kernel objects that have no own interface (yet)
  IOtherKernelObj = interface (IKernelObj) ['{4910FAC0-B61E-11D3-A530-00005A180D69}']
  end;

// ***************************************************************

type
  // encapsulation of the event APIs
  IEvent = interface (IWaitableObj) ['{53F8CE41-2C8A-11D3-A52D-00005A180D69}']
    // name of the event
    function GetName : AnsiString;
    property Name    : AnsiString read GetName;

    // is this an auto event?
    function IsAuto : boolean;

    // ResetEvent / SetEvent / PulseEvent
    function Lock   : boolean;
    function Unlock : boolean;
    function Pulse  : boolean;
  end;

// IHandle -> IEvent / handle -> IEvent
function Event (const event : IHandle       ) : IEvent; overload;
function Event (event       : cardinal;
                autoClose   : boolean = true) : IEvent; overload;

// name -> IEvent
function OpenEvent (name           : AnsiString;
                    access         : cardinal    = EVENT_ALL_ACCESS;
                    inheritHandles : boolean     = true            ) : IEvent; overload;

// create a new event
function NewEvent (auto      : boolean             = false;
                   locked    : boolean             = true;
                   name      : AnsiString          = '';
                   eventAttr : PSecurityAttributes = nil ) : IEvent;

// ***************************************************************

type
  // encapsulation of the mutex APIs
  IMutex = interface (IWaitableObj) ['{7B7FE820-8E34-11D3-A52E-00005A180D69}']
    // name of the mutex
    function GetName : AnsiString;
    property Name    : AnsiString read GetName;

    // same as WaitFor, except that the "handleMessages" parameter makes no sense here
    // cause handleMessages is realized internally by the use of a helper thread
    // so with handleMessages the helper thread would gain ownership of the mutex
    function Enter (milliseconds: cardinal = INFINITE) : boolean;

    // same as Enter, but returns with no delay
    // locks the Mutex only, if the try is successful
    function TryEnter : boolean;

    // ReleaseMutex
    function Leave : boolean;
  end;

// IHandle -> IMutex / handle -> IMutex
function Mutex (const mutex : IHandle       ) : IMutex; overload;
function Mutex (mutex       : cardinal;
                autoClose   : boolean = true) : IMutex; overload;

// name -> IMutex
function OpenMutex (name           : AnsiString;
                    access         : cardinal   = MUTEX_ALL_ACCESS;
                    inheritHandles : boolean    = true            ) : IMutex; overload;

// create a new mutex
function NewMutex (name      : AnsiString          = '';
                   enter     : boolean             = true;
                   mutexAttr : PSecurityAttributes = nil ) : IMutex;

// ***************************************************************

type
  // named buffer that can be shared by several processes
  // realized by CreateFileMapping(-1, ...) and MapViewOfFile APIs
  INamedBuffer = interface (IKernelObj) ['{AA4C5EE0-C417-11D3-A530-00005A180D69}']
    // name of the buffer
    function GetName : AnsiString;
    property Name    : AnsiString read GetName;

    // size of the buffer
    function GetSize : integer;
    property Size    : integer read GetSize;

    // buffer location in memory
    function GetMemory : pointer;
    property Memory    : pointer read GetMemory;

    // does this object have write access to the buffer?
    function IsWriteAccess : boolean;
  end;

// open an existing named buffer
function OpenNamedBuffer (name        : AnsiString;
                          writeAccess : boolean = true) : INamedBuffer;

// create a new named buffer or open an existing one
// (in the latter case the size parameter is ignored)
function NewNamedBuffer (name : AnsiString;
                         size : integer   ) : INamedBuffer;

// ***************************************************************

// ***************************************************************

// get the exe type of the "exeFile"
function ExeType(exeFile: AnsiString) : TExeType;

// get the icon that belongs to "file_"
function Icon (file_                 : AnsiString;
               smallIcon             : boolean = true;
               linkOverlayIfLnkOrPif : boolean = false) : cardinal;

// ***************************************************************

// error codes
const CErrorNo_RefreshListError       = CErrorBase_Kernel + 0;
      CErrorNo_ProcessStartedButNoID  = CErrorBase_Kernel + 1;
      CErrorNo_NoTopLevelWindow       = CErrorBase_Kernel + 2;
      CErrorNo_NoWin32Process         = CErrorBase_Kernel + 3;
      CErrorNo_InvalidReplaceArray    = CErrorBase_Kernel + 4;
      CErrorNo_CantCopyFunction       = CErrorBase_Kernel + 5;
      CErrorStr_WaitAbandoned         : PAnsiChar = 'Wait abandoned.';
      CErrorStr_WaitTimeout           : PAnsiChar = 'Wait timeout.';
      CErrorStr_WaitFailed            : PAnsiChar = 'Wait failed.';
      CErrorStr_RefreshListError      : PAnsiChar = 'Can''t refresh this kind of list.';
      CErrorStr_ProcessStartedButNoID : PAnsiChar = 'Process was started, but Windows returned invalid PID.';
      CErrorStr_NoTopLevelWindow      : PAnsiChar = 'This function works only on top-level windows.';
      CErrorStr_NoWin32Process        : PAnsiChar = 'This function works only on win32 processes.';
      CErrorStr_InvalidReplaceArray   : PAnsiChar = 'Invalid replace array.';
      CErrorStr_CantCopyFunction      : PAnsiChar = 'This function cannot be copied.';

// ***************************************************************

implementation

{$I madKernelUses.inc}

uses Messages, ShellAPI, ShlObj, ActiveX, CommCtrl,
     {$ifdef madDisAsm}madDisAsm,{$endif}
     {$ifdef madRemote}madRemote,{$endif}
     madStrings;

// ***************************************************************

var
  InFinalization : boolean = false;

// ***************************************************************

{$ifndef madDisAsm}

  var FMagic      : cardinal = 0;
      FMagic95    : boolean  = false;
      FMagicReady : boolean  = false;

  function Magic : cardinal;

    function TryRead4(addr: pointer; var value: dword) : boolean;
    begin
      result := not IsBadReadPtr2(addr, 4);
      if result then
        value := dword(addr^);
    end;

    function Fs(index: cardinal) : cardinal;
    asm
      mov eax, fs:[eax]
    end;

  var c1 : dword;
  begin
    if not FMagicReady then begin
      FMagicReady := true;
      FMagic := GetCurrentThreadID xor (Fs($18) - $10);
      if (not TryRead4(@TPACardinal(GetCurrentThreadID xor FMagic)^[2], c1)) or
         (c1 <> GetCurrentProcessID xor FMagic) then begin
        FMagic := GetCurrentProcessID xor Fs($30);
        if (not TryRead4(@TPACardinal(GetCurrentThreadID xor FMagic)^[14], c1)) or
           (c1 <> GetCurrentProcessID xor FMagic) then
          FMagic := 0;
      end else
        FMagic95 := true;
    end;
    result := FMagic;
  end;

  function Magic95 : boolean;
  begin
    if not FMagicReady then Magic;
    result := FMagic95;
  end;

{$endif}

// ***************************************************************

{$ifndef madRemote}

  var VirtualAllocEx    : function (process: dword; address: pointer; size, allocType, flags: dword) : pointer stdcall = nil;
      VirtualFreeEx     : function (process: dword; address: pointer; size, freeType        : dword) : bool    stdcall = nil;
      SharedMem9x_Alloc : function (size : dword  ) : pointer stdcall = nil;
      SharedMem9x_Free  : function (ptr  : pointer) : bool    stdcall = nil;

  procedure InitMemEx;
  var comctl : dword;
  begin
    if GetVersion and $80000000 <> 0 then begin
      if @SharedMem9x_Alloc = nil then begin
        comctl := LoadLibrary('comctl32.dll');
        SharedMem9x_Alloc := GetImageProcAddress(comctl, 71);
        SharedMem9x_Free  := GetImageProcAddress(comctl, 73);
      end;
    end else
      if @VirtualAllocEx = nil then begin
        VirtualAllocEx := GetProcAddress(GetModuleHandle(kernel32), 'VirtualAllocEx');
        VirtualFreeEx  := GetProcAddress(GetModuleHandle(kernel32), 'VirtualFreeEx' );
      end;
  end;

  function AllocMemEx(size: dword; processHandle: dword = 0) : pointer;
  begin
    if size > 0 then begin
      InitMemEx;
      if processHandle = 0 then
        processHandle := GetCurrentProcess;
      if GetVersion and $80000000 = 0 then
           result := VirtualAllocEx(processHandle, nil, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE)
      else result := SharedMem9x_Alloc(size);
    end else
      result := nil;
  end;

  function FreeMemEx(mem: pointer; processHandle: dword = 0) : bool;
  begin
    InitMemEx;
    if processHandle = 0 then
      processHandle := GetCurrentProcess;
    if GetVersion and $80000000 = 0 then
         result := VirtualFreeEx(processHandle, mem, 0, MEM_RELEASE)
    else result := (dword(mem) >= $80000000) and SharedMem9x_Free(mem);
  end;

  function CreateRemoteThreadEx(processHandle : dword;
                                threadAttr    : PSecurityAttributes;
                                stackSize     : integer;
                                startAddr     : pointer;
                                params        : pointer;
                                creationFlags : dword;
                                var threadId  : dword  ) : dword; stdcall;
  begin
    result := CreateRemoteThread(processHandle, threadAttr, stackSize, startAddr, params, creationFlags, threadId);
  end;

  procedure InitUnprotectMemory; begin end;

  function IsMemoryProtected(addr: pointer) : boolean;
  var mbi : TMemoryBasicInformation;
  begin
    result := (VirtualQuery(addr, mbi, sizeOf(mbi)) <> sizeOf(mbi)) or
              ( (mbi.Protect and PAGE_EXECUTE_READWRITE = 0) and
                (mbi.Protect and PAGE_EXECUTE_WRITECOPY = 0) and
                (mbi.Protect and PAGE_READWRITE         = 0) and
                (mbi.Protect and PAGE_WRITECOPY         = 0)     );
  end;

  function UnprotectMemory(address: pointer; size: integer) : boolean;
  var c1 : dword;
  begin
    result := VirtualProtect(address, size, PAGE_EXECUTE_READWRITE, @c1);
  end;

  function ProtectMemory(address: pointer; size: integer) : boolean;
  var c1 : dword;
  begin
    result := VirtualProtect(address, size, PAGE_EXECUTE_READ, @c1);
  end;

{$else}

 {$ifdef win64}
 
   procedure InitUnprotectMemory; begin end;

   function IsMemoryProtected(addr: pointer) : boolean;
   var mbi : TMemoryBasicInformation;
   begin
     result := (VirtualQuery(addr, mbi, sizeOf(mbi)) <> sizeOf(mbi)) or
               ( (mbi.Protect and PAGE_EXECUTE_READWRITE = 0) and
                 (mbi.Protect and PAGE_EXECUTE_WRITECOPY = 0) and
                 (mbi.Protect and PAGE_READWRITE         = 0) and
                 (mbi.Protect and PAGE_WRITECOPY         = 0)     );
   end;

   function UnprotectMemory(address: pointer; size: integer) : boolean;
   var c1 : dword;
   begin
     result := VirtualProtect(address, size, PAGE_EXECUTE_READWRITE, @c1);
   end;

   function ProtectMemory(address: pointer; size: integer) : boolean;
   var c1 : dword;
   begin
     result := VirtualProtect(address, size, PAGE_EXECUTE_READ, @c1);
   end;

  {$endif}

{$endif}

// ***************************************************************

type
  // types for TP9xPid.handleTable
  T9xHandleItem = packed record
    access  : cardinal;
    objAddr : pointer;
  end;
  TP9xHandleItem = ^T9xHandleItem;
  T9xHandleTable = packed record
    itemCount : integer;
    items     : array [0..maxInt shr 3 - 1] of T9xHandleItem;
  end;
  TP9xHandleTable = ^T9xHandleTable;

  // forward
  TP9xTid = ^T9xTid;

  // types for TP9xPid.threadList
  TP9xThreadListItem = ^T9xThreadListItem;
  T9xThreadListItem = record
    next   : TP9xThreadListItem;
    prev   : TP9xThreadListItem;
    thread : TP9xTid;
  end;
  TP9xThreadList = ^T9xThreadList;
  T9xThreadList = record
    first : TP9xThreadListItem;
    prev  : TP9xThreadListItem;
    d     : TP9xThreadListItem;
  end;

  // type for T9xPid.flags
  T9xPidFlags = set of (
                  pfDebugSingle,            // 00000001  set if process is being debugged
                  pfCreateProcessEvent,     // 00000002  set in debugged process after starting
                  pfExitProcessEvent,       // 00000004  might be set in debugged process at exit time
                  pfWin16Process,           // 00000008  a 16 bit process
                  pfDosProcess,             // 00000010  a dos process
                  pfConsoleProcess,         // 00000020  a 32 bit console process
                  pfFileAPIsAreOEM,         // 00000040  SetFileAPIsToOEM
                  pfNukeProcess,            // 00000080
                  pfServiceProcess,         // 00000100  RegisterServiceProcess
                  pf0, pf1,
                  pfLoginScriptHack,        // 00000800  might be a novell network login process
                  pf2, pf3, pf4, pf5,
                  pf6, pf7, pf8, pf9,
                  pfA,
                  pfSendDllNotifications,   // 00200000
                  pfDebugEventPending,      // 00400000  e.g. stopped in debugger
                  pfNearlyTermination,      // 00800000
                  pfB, pfC, pfD,
                  pfFaulted,                // 08000000
                  pfTerminating,            // 10000000
                  pfTerminated,             // 20000000
                  pfInitError,              // 40000000
                  pfSignaled                // 80000000
                );

  // types for T9xPid.memoryMappedFiles
  TP9xMemoryMappedFile = ^T9xMemoryMappedFile;
  T9xMemoryMappedFile = packed record
    baseAddress : pointer;
    next        : TP9xMemoryMappedFile;
  end;

  // types for T9xPid.edb
  TP9xEDB = ^T9xEDB;
  T9xEDB = packed record
    environment    : PAnsiChar;     // GetEnvironmentStrings
    d0             : cardinal;
    cmdLine        : PAnsiChar;     // GetCommandLine
    currentDir     : PAnsiChar;     // GetCurrentDirectory
    startupInfo    : PStartupInfo;  // GetStartupInfo
    stdIn          : cardinal;      // Get/SetStdHandle(STD_INPUT_HANDLE)
    stdOut         : cardinal;      // Get/SetStdHandle(STD_OUTPUT_HANDLE)
    stdErr         : cardinal;      // Get/SetStdHandle(STD_ERROR_HANDLE)
    d1             : cardinal;
    inheritConsole : cardinal;      // inherit console from parent process?
    breakType      : cardinal;      // console break type (Ctrl-C)
    breakSemaphore : cardinal;      // semaphore for SetConsoleCtrlHandler
    breakEvent     : cardinal;      // event     for SetConsoleCtrlHandler
    breakThreadID  : cardinal;      // which thread called SetConsoleCtrlHandler?
    breakHandlers  : pointer;       // list of installed console control handlers
  end;

  // forward
  TP9xPid = ^T9xPid;

  // types for T9xPid.moduleList and T9xPid.exeModule
  TP9xModule = ^T9xModule;
  T9xModule = packed record
    next         : TP9xModule;  // next module structure or nil (end of list)
    d1           : dword;
    d2           : dword;
    d3           : dword;
    gmlIndex     : dword;       // index into the GlobalModuleList
    d4           : dword;
    d5           : dword;
    ownerProcess : TP9xPid;     // owner process
    d6           : dword;
    d7           : dword;
    d8           : dword;
  end;

  // structure at position (processID xor Magic)^
  T9xPid = packed record
    objectType        : byte;                  // $00 kernel object type
    d0                : array [1..3] of byte;  // $01
    referenceCount    : cardinal;              // $04
    d1                : cardinal;              // $08
    event             : cardinal;              // $0C event for process waiting
    terminationStatus : cardinal;              // $10 GetExitCodeProcess
    d2                : cardinal;              // $14 may be used for private purposes
    defaultHeap       : cardinal;              // $18 GetProcessHeap
    memoryContext     : cardinal;              // $1C
    flags             : T9xPidFlags;           // $20
    psp               : cardinal;              // $24 linear address of dos PSP
    pspSelector       : word;                  // $28 selector to dos PSP
    mteIndex          : word;                  // $2A index into global module table
    threads           : word;                  // $2C Threads.ItemCount
    notTermThreads    : word;                  // $2E Threads.ItemCount
    d3                : word;                  // $30
    ring0Threads      : word;                  // $32 normally Threads.ItemCount (except kernel32)
    heapHandle        : cardinal;              // $34 kernel32 shared heap
    w16tdbSelector    : cardinal;              // $38 win16 task database (tdb) selector
    memoryMappedFiles : TP9xMemoryMappedFile;  // $3C list of memory mapped files
    edb               : TP9xEDB;               // $40 environment database
    handleTable       : TP9xHandleTable;       // $44
    parentProcess     : TP9xPid;               // $48 TP9xPid structure of the parent process
    moduleList        : TP9xModule;            // $4C list of modules
    threadList        : TP9xThreadList;        // $50 list of threads
    debuggeeCB        : cardinal;              // $54 debuggee context block
    localHeapFreeHead : pointer;               // $58 free list for process default heap
    initialRing0ID    : cardinal;              // $5C meaning unknown
    criticalSection   : TRtlCriticalSection;   // $60 for synchronizing threads
    d4                : cardinal;              // $78
    d5                : cardinal;              // $7C
    console           : cardinal;              // $80 output console
    tlsInUseBits1     : cardinal;              // $84 status of TLS indexes  0 - 31
    tlsInUseBits2     : cardinal;              // $88 status of TLS indexes 32 - 63
    processDword      : cardinal;              // $8C undocumented API GetProcessDword, meaning unknown
    processGroup      : TP9xPid;               // $90 master process (in debugging)
    exeModule         : TP9xModule;            // $94 points to exe's module structure
    topExcFilter      : cardinal;              // $98 SetUnhandledExceptionFilter
    priorityClass     : cardinal;              // $9C SetPriorityClass
    heapList          : pointer;               // $A0 list of heaps
    heapHandleList    : pointer;               // $A4 list of moveable memory blocks
    heapPointer       : pointer;               // $A8 pointer to one moveable memory block, meaning unknown
    consoleProvider   : cardinal;              // $AC console for DOS apps
    edbSelector       : word;                  // $B0 environment database selector
    errorMode         : word;                  // $B2 SetErrorMode MUST BE $B2 !!!
    eventLoadFinished : cardinal;              // $B4 signaled when the process has finished loading
    utState           : word;                  // $B8 universal thunking, meaning unknown
  end;

  // type for T9xTdbx.waitExFlags
  T9xTdbxFlags = set of (
                   tdfWaitExBit,              // 00000001
                   tdfWaitAckBit,             // 00000002
                   tdfSuspendApcPending,      // 00000004
                   tdfSuspendTerminated,      // 00000008
                   tdfBlockedForTermination,  // 00000010
                   tdfEmulateNpx,             // 00000020
                   tdfWin32Npx,               // 00000040
                   tdfExtendedHandles,        // 00000080
                   tdfFrozen,                 // 00000100
                   tdfDontFreeze,             // 00000200
                   tdfDontUnfreeze,           // 00000400
                   tdfDontTrace,              // 00000800
                   tdfStopTracing,            // 00001000
                   tdfWaitingForCrstSafe,     // 00002000
                   tdfCrstSafe,               // 00004000
                   tdf00008000,               // 00008000
                   tdf00010000,               // 00010000
                   tdf00020000,               // 00020000
                   tdfBlockTerminateApc);     // 00040000

  // VVIN32 thread structure (T9xTid.tdbx)
  // CAUTION!!! This structure is valid for win95 only. Win98 has changed a lot!!
  T9xTdbx = packed record
    thread            : TP9xTid;               // $00
    ownerProcess      : TP9xPid;               // $04
    memoryContext     : dword;                 // $08
    d0                : dword;                 // $0C
    timeOutHandle     : dword;                 // $10
    wakeParam         : dword;                 // $14
    blockHandle       : dword;                 // $18
    blockState        : dword;                 // $1C
    suspendCount      : dword;                 // $20 how often was SuspendThread called?
    suspendHandle     : dword;                 // $24
    mustCompleteCount : dword;                 // $28 the thread can't be interrupted when > 0
    waitExFlags       : T9xTdbxFlags;          // $2C
    syncWaitCursor    : dword;                 // $30
    queuedSyncFuncs   : dword;                 // $34
    apc0              : dword;                 // $38
    apc1              : dword;                 // $3C
    pmPspSelector     : TPWord;                // $40 pointer to protected mode psp selector
    blockedOnID       : dword;                 // $44
    d1                : array [0..6] of dword; // $48
    d2                : dword;                 // $64
    traceCallback     : dword;                 // $68
    traceEventHandle  : dword;                 // $6C
    d3                : word;                  // $70
    k16tdb            : word;                  // $72 this process' win16 task database selector
    k16pdb            : word;                  // $74 this process' win16 program segment prefix selector
    dosPdbSeg         : word;                  // $76 this process' real mode segment value of the psp
    exceptionCount    : word;                  // $78
  end;
  TP9xTdbx = ^T9xTdbx;

  // type for T9xTid.flags
  T9xTidFlags = set of (
                  tfCreateThreadEvent,       // 00000001  set if the thread is being debugged
                  tfCancelExceptionAbort,    // 00000002
                  tfOnTempStack,             // 00000004
                  tfGrowaUeStack,            // 00000008
                  tfDelaySingleStep,         // 00000010
                  tfOpenExeAsImmovableFile,  // 00000020
                  tfCreateSuspended,         // 00000040
                  tfStackOverflow,           // 00000080
                  tfNestedCleanAPCs,         // 00000100
                  tfWasOemNowAnsi,           // 00000200
                  tfOkToSetThreadOem,        // 00000400
                  tf00000800,
                  tf00001000,
                  tf00002000,
                  tf00004000,
                  tf00008000,
                  tf00010000);

  // structure at position (threadID xor Magic)^
  // CAUTION!!! This structure is valid for win95 only. Win98 has changed a lot!!
  T9xTid = packed record
    objectType        : byte;                      // $00 kernel object type
    d0                : array [1..3] of byte;      // $01
    referenceCount    : cardinal;                  // $04
    ownerProcess95    : TP9xPid;                   // $08
    event             : cardinal;                  // $0C event for thread waiting
    excChain          : cardinal;                  // $10 structured exception handling chain
    topOfStack        : pointer;                   // $14
    bottomOfStack     : pointer;                   // $18
    w16tdbSelector    : word;                      // $1C win16 task database (tdb) selector
    stackSelector16   : word;                      // $1E for thunking
    selManList        : cardinal;                  // $20 selector manager list
    userPointer       : cardinal;                  // $24
    tib               : pointer;                   // $28 thread information block = @excChain
    tibFlags          : word;                      // $2C 1 = 32-bit thread
    win16MutexCount   : word;                      // $2E
    debugContext      : cardinal;                  // $30
    priorityLevel     : ^cardinal;                 // $34 Get/SetThreadPriority
    ownerProcess98    : cardinal;                  // $38 messageQueue under win95???
    pTlsArray         : pointer;                   // $3C @tlsArray
    ownerProcess95_2  : TP9xPid;                   // $40
    flags             : T9xTidFlags;               // $44
    terminationStatus : cardinal;                  // $48 GetExitCodeThread
    tibSelector       : word;                      // $4C -> FS register
    emulatorSelector  : word;                      // $4E
    handles           : cardinal;                  // $50
    waitNodeList      : pointer;                   // $54 list of events this thread is waiting for
    d1                : cardinal;                  // $58
    ring0Thread       : pointer;                   // $5C ring 0 thread control block (THCB)
    tdbx              : TP9xTdbx;                  // $60 vwin32's tdbx structure for this thread
    stackBase         : pointer;                   // $64 = bottomOfStack?
    terminationStack  : pointer;                   // $68 = topOfStack
    emulatorData      : cardinal;                  // $6C
    lastErrorCode     : cardinal;                  // $70 Get/SetLastError
    debuggerCB        : cardinal;                  // $74
    debuggerThread    : cardinal;                  // $78
    threadContext     : PContext;                  // $7C
    except16List      : cardinal;                  // $80
    thunkConnect      : cardinal;                  // $84
    negStackBase      : cardinal;                  // $88
    currentSS         : cardinal;                  // $8C current stack selector
    ssTable           : cardinal;                  // $90
    thunkSS16         : cardinal;                  // $94
    tlsArray          : array [0..63] of cardinal; // $98
    deltaPriority     : integer;                   // $198 difference between thread's and process' priority
    d2                : array [0..6] of cardinal;  // $19C
    createData16      : pointer;                   // $1B8
    suspendCount      : cardinal;                  // $1BC thread suspend count, see Suspend/ResumeThread
    d3                : cardinal;                  // $1C0
    wowChain          : cardinal;                  // $1C4
    ssBig             : word;                      // $1C8
    d4                : word;                      // $1CA
    ip16SwitchRec     : cardinal;                  // $1CC
    d5                : array [0..4] of cardinal;  // $1D0
    criticalSection1  : PRtlCriticalSection;       // $1E4
    win16Mutex        : pointer;                   // $1E8
    win32Mutex        : pointer;                   // $1EC
    criticalSection2  : PRtlCriticalSection;       // $1F0
    d6                : cardinal;                  // $1F4
    ripString         : cardinal;                  // $1F8
    d7                : cardinal;                  // $1FC
    lastTlsEip        : array [0..63] of cardinal; // $200
  end;

{
  // structure at position (threadID xor Magic)^
  T9xTid = packed record
    d1, d2 : cardinal;
    pid95  : TP9xPid;
    d3     : array [0..10] of cardinal;
    pid98  : TP9xPid;
    d4     : array [0..21] of cardinal;
  end;   }

  // types for the win9x only global module list
  TGlobalModule = packed record
    d1           : dword;
    ntHeaders    : PImageNtHeaders;   // copy of the module's header (in shared memory)
    d2           : dword;
    path         : PAnsiChar;         // name (with    path) of the module
    name         : PAnsiChar;         // name (without path) of the module
    pathLen      : word;              // length of path
    nameLen      : word;              // length of name
    d3           : dword;
    sectionCnt   : dword;             // number of sections in the module's header
    d4           : dword;
    baseAddress  : dword;             // base address of the module
    hModule16    : word;              // module 16bit handle
    usageCnt     : word;              // GetModuleUsage
    d5           : dword;
    path2        : PAnsiChar;         // same as path
    path2Len     : word;              // same as pathLen
    name2        : PAnsiChar;         // same as name
    name2Len     : word;              // same as nameLen
  end;
  TPGlobalModule = ^TGlobalModule;
  TGlobalModuleList = record
    ItemCount : integer;
    Items     : array of TPGlobalModule;
  end;

  // types for GetHandleTableNt
  TNtHandleItem  = packed record
    pid     : cardinal;
    objType : word;
    handle  : word;
    objAddr : pointer;
    access  : cardinal;
  end;
  TPNtHandleItem = ^TNtHandleItem;
  TNtHandleTable = record
    ItemCount : integer;
    Items     : array of TNtHandleItem;
  end;

  // types for NT module functions
  TPNtModuleInfo = ^TNtModuleInfo;
  TNtModuleInfo  = record
                     next    : TPNtModuleInfo;
                     d1      : array [0..2] of pointer;
                     handle  : cardinal;
                     d2      : array [0..1] of pointer;
                     d3      : word;
                     nameLen : word;
                     name    : pointer;
                   end;

  // types for NtQueryProcessInformation
  TNtProcessInfoClass = (ProcessBasicInformation,
                         ProcessQuotaLimits,
                         ProcessIoCounters,
                         ProcessVmCounters,
                         ProcessTimes,
                         ProcessBasePriority,
                         ProcessRaisePriority,
                         ProcessDebugPort,
                         ProcessExceptionPort,
                         ProcessAccessToken,
                         ProcessLdtInformation,
                         ProcessLdtSize,
                         ProcessDefaultHardErrorMode,
                         ProcessIoPortHandlers,
                         ProcessPooledUsageAndLimits,
                         ProcessWorkingSetWatch,
                         ProcessUserModeIOPL,
                         ProcessEnableAlignmentFaultFixup,
                         ProcessPriorityClass,
                         ProcessWx86Information,
                         ProcessHandleCount,
                         ProcessAffinityMask,
                         ProcessPriorityBoost,
                         ProcessDeviceMap,
                         ProcessSessionInformation,
                         ProcessForegroundInformation,
                         ProcessWow64Information,
                         ProcessImageFileName,
                         ProcessLUIDDeviceMapsEnabled,
                         ProcessBreakOnTermination,
                         ProcessDebugObjectHandle,
                         ProcessDebugFlags,
                         ProcessHandleTracing,
                         ProcessIoPriority,
                         ProcessExecuteFlags,
                         ProcessResourceManagement,
                         ProcessCookie,
                         ProcessImageInformation,
                         MaxProcessInfoClass);

  TNtProcessBasicInfo = record
    exitStatus     : cardinal;
    pebBaseAddress : cardinal;
    affinityMask   : cardinal;
    basePriority   : cardinal;
    pid            : cardinal;
    parentPid      : cardinal;
  end;

  // type for NtQuerySystemInformation(5, ...)
  TNtProcessInfo = record
    offset      : cardinal;
    numThreads  : cardinal;
    d1          : array [2..14] of cardinal;
    name        : PWideChar;
    d2          : cardinal;
    pid         : cardinal;
    parentPid   : cardinal;
    handleCount : cardinal;
    sessionId   : cardinal;
    d4          : array [21..42] of cardinal;
    threads     : array [0..maxInt shr 7 - 1] of record
                    tidNt4 : cardinal;
                    d5     : array [44..54] of cardinal;
                    tidNt5 : cardinal;
                    d6     : array [56..58] of cardinal;
                  end;
  end;

var
  NtQuerySystemInformation  : function (infoClass     : cardinal;
                                        buffer        : pointer;
                                        bufSize       : cardinal;
                                        returnSize    : TPCardinal) : cardinal stdcall = nil;
  NtQueryInformationProcess : function (processHandle : cardinal;
                                        infoClass     : TNtProcessInfoClass;
                                        buffer        : pointer;
                                        bufSize       : cardinal;
                                        returnSize    : TPCardinal) : cardinal stdcall = nil;
  CreateToolhelp32Snapshot  : function (flags         : cardinal;
                                        processID     : cardinal  ) : cardinal stdcall = nil;

const
  // dll for Nt* functions
  ntdll = 'ntdll.dll';

function FirstModuleInfoNt(ph: cardinal; var mi: TNtModuleInfo; var loopEnd: pointer) : boolean;
var pbi : TNtProcessBasicInfo;
    c1  : NativeUInt;
begin
  if @NtQueryInformationProcess = nil then
    NtQueryInformationProcess := GetProcAddress(GetModuleHandle(ntdll), 'NtQueryInformationProcess');
  result := (NtQueryInformationProcess(ph, ProcessBasicInformation, @pbi,
                                       sizeOf(TNtProcessBasicInfo), nil) = 0) and
            ReadProcessMemory(ph, pointer(cardinal(pbi.pebBaseAddress) + $C), @loopEnd, 4, c1) and
            (c1 = 4);
  if result then begin
    loopEnd := pointer(cardinal(loopEnd) + $14);
    mi.next := loopEnd;
    result := ReadProcessMemory(ph, loopEnd, @mi, sizeOf(TNtModuleInfo), c1) and
              (c1 = sizeOf(TNtModuleInfo));
  end;
end;

function NextModuleInfoNt(ph: cardinal; var mi: TNtModuleInfo) : boolean;
var c1 : NativeUInt;
begin
  result := ReadProcessMemory(ph, mi.next, @mi, sizeOf(TNtModuleInfo), c1) and
            (c1 = sizeOf(TNtModuleInfo));
end;

function ModuleInfoNameNt(ph: cardinal; const mi: TNtModuleInfo; var ws: UnicodeString) : boolean;
var c1     : NativeUInt;
    arrChW : array [0..MAX_PATH] of WideChar;
begin
  result := false;
  if (mi.handle <> 0) and (mi.nameLen >= 2) then begin
    SetLength(ws, (mi.nameLen - 2) div 2);
    result := ReadProcessMemory(ph, mi.name, pointer(ws), mi.nameLen - 2, c1) and
              (c1 + 2 = mi.nameLen);
    if result then begin
      ws := PWideChar(ws);
      if PosStrIs1(string('\??\'), ws) then begin
        Delete(ws, 1, 4);
      end else if PosTextIs1(string('\SystemRoot\System32\'), ws) then begin
        GetSystemDirectoryW(arrChW, MAX_PATH);
        ws := arrChW + Copy(ws, 21, maxInt);
      end else
        result := GetFileAttributesW(PWideChar(ws)) <> dword(-1);
    end;
  end;
end;

function ExeType(exeFile: AnsiString) : TExeType;
var c1  : cardinal;
    sfi : TSHFileInfoA;
    s1  : AnsiString;
begin
  c1 := SHGetFileInfoA(PAnsiChar(exeFile), 0, sfi, sizeOf(sfi), SHGFI_EXETYPE);
  s1 := AnsiChar(c1 and $ff) + AnsiChar((c1 and $ff00) shr 8);
  if       s1 = 'MZ'                                                                    then result := etDos
  else if  s1 = 'NE'                                                                    then result := etWin16
  else if (s1 = 'PE') and (hiWord(c1) = 0)                                              then result := etConsole
  else if (s1 = 'PE') and (hiWord(c1) > 0)                                              then result := etWin32
  else if IsTextEqual(string(ExtractFileName(string(exeFile))), string('winoa386.mod')) then result := etDos
  else                                                                                       result := etUnknown;
end;

function Icon(file_: AnsiString; smallIcon, linkOverlayIfLnkOrPif: boolean) : cardinal;
var sfi : TSHFileInfoA;
    s1  : AnsiString;
    c1  : cardinal;
begin
  c1 := SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES;
  if smallIcon then c1 := c1 or SHGFI_SMALLICON
  else              c1 := c1 or SHGFI_LARGEICON;
  if linkOverlayIfLnkOrPif then begin
    s1 := AnsiString(ExtractFileExt(string(file_)));
    linkOverlayIfLnkOrPif := IsTextEqual(s1, AnsiString('.lnk')) or IsTextEqual(s1, AnsiString('.pif'));
    if linkOverlayIfLnkOrPif then c1 := c1 or SHGFI_ICON or SHGFI_LINKOVERLAY;
  end;
  c1 := SHGetFileInfoA(PAnsiChar(file_), GetFileAttributesA(PAnsiChar(file_)), sfi, sizeOf(sfi), c1);
  if c1 = 0 then
    result := 0
  else
    if linkOverlayIfLnkOrPif then result := sfi.hIcon
    else                          result := ImageList_GetIcon(c1, sfi.iIcon, ILD_NORMAL);
end;

function PickInterfaces(const objects: array of IBasic; class_: TClass) : TDABasic;
var len : integer;

  procedure InternalPickInterfaces(const objects: array of IBasic);
  var i1  : integer;
      obj : TIBasic;
  begin
    for i1 := 0 to high(objects) do
      if (objects[i1] <> nil) and objects[i1].IsValid then begin
        obj := TIBasic(objects[i1].SelfAsTObject);
        if obj is class_ then begin
          SetLength(result, len + 1);
          result[len] := obj.GetMaxInterface;
          inc(len);
        end else if obj is TICustomBasicList then
          InternalPickInterfaces(TICustomBasicList(obj).FItems);
      end;
  end;

begin
  result := nil;
  len := 0;
  InternalPickInterfaces(objects);
  if len = 0 then SetLength(result, 1);
end;

// ***************************************************************

type
  // implements a part of IKernelObj
  TIKernelObj = class (TIBasic, IKernelObj)
  public
    FType         : TKernelObjType;
    FTypeStrReady : boolean;
    FTypeStr      : AnsiString;
    FAddr         : pointer;
    FNameReady    : boolean;
    FName         : AnsiString;
    FIDReady      : boolean;
    FID           : cardinal;
    FEventReady   : boolean;
    FAutoEvent    : boolean;
    FHandle       : IHandle;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        const handle: IHandle); overload;
    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        type_: TKernelObjType; id: cardinal); overload;
    destructor Destroy; override;

    function GetObjType    : TKernelObjType;
    function GetObjTypeStr : AnsiString;

    function GetObjAddr : pointer;

    function GetObjName : AnsiString;

    function GetID : cardinal;

    function IsAutoEvent : boolean;

    function GetHandle : IHandle; virtual;

    function GetHandles : IHandles;
  end;

  // *******************************************************************

  // implements IHandle
  TIHandle = class (TIBasic, IHandle)
  public
    FProcess   : cardinal;
    FHandle    : cardinal;
    FAccess    : cardinal;
    FType      : TKernelObjType;
    FKernelObj : IKernelObj;
    FAutoClose : boolean;
    FAddr      : pointer;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        processID, handle: cardinal; autoClose: boolean;
                        hi95: TP9xHandleItem; hiNt: TPNtHandleItem;
                        typ: TKernelObjType);
    destructor Destroy; override;

    function IsStillValid : boolean;

    function GetOwnerProcess : IProcess;

    function GetHandle : cardinal;

    function GetAccess : cardinal;

    function GetObjType : TKernelObjType;

    function GetKernelObj : IKernelObj;

    function  GetAutoClose : boolean;
    procedure SetAutoClose (value: boolean);

    function WouldWait : boolean;

    function WaitFor (milliseconds   : cardinal = INFINITE;
                      handleMessages : boolean  = true    ) : boolean;

    function Notify (window : cardinal;
                     msg    : cardinal) : boolean;

    function Duplicate (autoClose           : boolean;
                        access              : cardinal;
                        inheritHandles      : boolean;
                        const targetProcess : IProcess       ) : IHandle; overload;
    function Duplicate (autoClose           : boolean;
                        access              : cardinal;
                        inheritHandles      : boolean;
                        const targetProcess : IHandle        ) : IHandle; overload;
    function Duplicate (autoClose           : boolean  = true;
                        access              : cardinal = 0;
                        inheritHandles      : boolean  = true;
                        targetProcess       : cardinal = 0   ) : IHandle; overload;

    function GetMaxInterface : IBasic; override;
  end;

  // implements IHandles
  TIHandles = class (TICustomBasicList, IHandles)
  public
    FAll     : boolean;
    FProcess : IProcess;
    FObj     : IKernelObj;
    FList    : boolean;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        const process: IProcess; const kernelObj: IKernelObj; const objects: array of IBasic);

    function GetOwnerProcess : IProcess;

    function GetKernelObj : IKernelObj;

    function GetItem (index: integer) : IHandle;

    function RefreshItems : boolean;

    function WouldWait (waitAll: boolean) : boolean;

    function WaitFor (waitAll        : boolean   = false;
                      milliseconds   : cardinal  = INFINITE;
                      handleMessages : boolean   = true;
                      index          : TPInteger = nil     ) : boolean;

    function Notify (window  : cardinal;
                     msg     : cardinal;
                     waitAll : boolean = false) : boolean;

    function GetMaxInterface : IBasic; override;
  end;

  // *******************************************************************

  // implements IWaitable
  TIWaitableObj = class (TIKernelObj, IWaitableObj)
  public
    function WouldWait : boolean;

    function WaitFor (milliseconds   : cardinal = INFINITE;
                      handleMessages : boolean  = true    ) : boolean;

    function Notify (window : cardinal;
                     msg    : cardinal) : boolean;
  end;

  // implements IWaitableObjs
  TIWaitableObjs = class (TICustomBasicList, IWaitableObjs)
  public
    function WouldWait (waitAll: boolean) : boolean;

    function WaitFor (waitAll        : boolean   = false;
                      milliseconds   : cardinal  = INFINITE;
                      handleMessages : boolean   = true;
                      index          : TPInteger = nil     ) : boolean;

    function Notify (window  : cardinal;
                     msg     : cardinal;
                     waitAll : boolean = false) : boolean;
  end;

  // *******************************************************************

  // implements IThread
  TIThread = class (TIWaitableObj, IThread)
  public
    FStore   : integer;
    FProcess : IProcess;

    function IsStillValid : boolean;

    function GetHandle : IHandle; override;
    function GetHandle2 (access: cardinal = THREAD_ALL_ACCESS) : IHandle;
    function IThread.GetHandle = GetHandle2;

    procedure StoreHandle;
    procedure ReleaseHandle;

    function GetOwnerProcess : IProcess;

    function GetWindows : IWindows;

    function GetTaskbarWindows : IWindows;

    function  GetPriority : integer;
    procedure SetPriority (priority: integer);

    function  GetPriorityBoost : boolean;
    procedure SetPriorityBoost (priorityBoost: boolean);

    function SetAffinityMask   (affinityMask    : cardinal;
                                oldAffinityMask : TPCardinal = nil) : boolean;
    function SetIdealProcessor (processorNo     : cardinal;
                                oldProcessorNo  : TPCardinal = nil) : boolean;

    function PostMessage (msg    : cardinal;
                          wParam : integer = 0;
                          lParam : integer = 0) : boolean;

    function IsSuspended : boolean;
    function Suspend (oldSuspendCount: TPCardinal = nil) : boolean;
    function Resume  (oldSuspendCount: TPCardinal = nil) : boolean;

    function  GetContext : TContext;
    procedure SetContext (const context: TContext);

    function AttachInput (const thread: IThread     ) : boolean; overload;
    function AttachInput (const thread: IHandle     ) : boolean; overload;
    function AttachInput (      thread: cardinal = 0) : boolean; overload;
    function DetachInput (const thread: IThread     ) : boolean; overload;
    function DetachInput (const thread: IHandle     ) : boolean; overload;
    function DetachInput (      thread: cardinal = 0) : boolean; overload;

    function PostQuitMessage (exitCode: cardinal = 0) : boolean;

    function IsStillRunning : boolean;

    function Terminate (exitCode: cardinal = 0) : boolean;

    function GetExitCode : cardinal;

    function GetTimes (var creation, exit, kernel, user: int64) : boolean;

    function GetMaxInterface : IBasic; override;
  end;
  
  // contains a list of specific IThread objects
  TIThreads = class (TIWaitableObjs, IThreads)
  public
    FAll     : boolean;
    FProcess : IProcess;
    FList    : boolean;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        const process: IProcess; const objects: array of IBasic);

    function GetOwnerProcess : IProcess;

    function GetItem (index: integer) : IThread;

    function RefreshItems : boolean;

    procedure SetPriority (priority: integer);

    procedure SetPriorityBoost (priorityBoost: boolean);

    function SetAffinityMask   (affinityMask : cardinal) : boolean;
    function SetIdealProcessor (processorNo  : cardinal) : boolean;

    function PostMessage (msg    : cardinal;
                          wParam : integer = 0;
                          lParam : integer = 0) : boolean;

    function Suspend : boolean;
    function Resume  : boolean;

    function PostQuitMessage (exitCode: cardinal = 0) : boolean;

    function IsStillRunning (all: boolean = false) : boolean;

    function Terminate (exitCode: cardinal = 0) : boolean;

    function GetMaxInterface : IBasic; override;
  end;

  // *******************************************************************

  // type for alien thread injection
  TInjectCommand = (icUnknown,
                    icExitProcess, icSetWindowLong,
                    icAllocMem, icFreeMem, icCloseHandle,
                    icLoadLibrary, icFreeLibrary,
                    icGetCommandLine, icGetStartupInfo,
                    icCallFunc);

  // type for CopyFunction
  TCopyFunctionRec = record
    IsValid       : boolean;
    Buffer        : pointer;
    UnloadModules : array of IModule;
    EntryPoint    : pointer;
    Params_       : pointer;
  end;

  // implements IProcess
  TIProcess = class (TIWaitableObj, IProcess)
  public
    FExeFile        : AnsiString;
    FSession        : dword;
    FSessionDone    : boolean;
    FStore          : integer;
    FParentProcess  : IProcess;
    FOsVersionReady : boolean;
    FOsVersion      : TOsVersion;
    FExeTypeReady   : boolean;
    FExeType        : TExeType;
    FHInstance      : cardinal;
    FCommandLine    : AnsiString;
    FStartupInfo    : TStartupInfo;
    FInjectMutex    : IMutex;
    FInjectNB       : INamedBuffer;
    FInjectMem      : pointer;
    FInjectEvent1   : IEvent;
    FInjectEvent2   : IEvent;

    function IsStillValid : boolean;

    function GetExeFile : AnsiString;

    function GetSession : dword;

    function GetHandle : IHandle; override;
    function GetHandle2 (access: cardinal = PROCESS_ALL_ACCESS) : IHandle;
    function IProcess.GetHandle = GetHandle2;

    procedure StoreHandle;
    procedure ReleaseHandle;

    function GetParentProcess : IProcess;

    function GetOsVersion : TOsVersion;

    function GetExeType : TExeType;

    function Icon (smallIcon             : boolean = true;
                   linkOverlayIfLnkOrPif : boolean = false) : cardinal;

    function GetHInstance : cardinal;

    function GetMainModule : IModule;

    function  GetServiceProcess : boolean;
    procedure SetServiceProcess (serviceProcess: boolean = true);

    function  GetPriorityClass : TPriorityClass;
    procedure SetPriorityClass (priorityClass: TPriorityClass);

    function  GetPriorityBoost : boolean;
    procedure SetPriorityBoost (priorityBoost: boolean);

    function  GetAffinityMask : cardinal;
    procedure SetAffinityMask (affinityMask: cardinal);

    procedure GetWorkingSetSize (var minimum, maximum: cardinal);
    function  SetWorkingSetSize (    minimum, maximum: cardinal) : boolean;

    function GetTokenInformation (tokenLevel: dword) : AnsiString;

    function GetUserName : AnsiString;

    function GetCommandLine : AnsiString;

    function GetStartupInfo : TStartupInfo;

    function AllocMem (size             : integer;
                       mayUseSharedArea : boolean = false) : pointer;
    function FreeMem  (var ptr          : pointer        ) : boolean;

    function ReadMemory  (const source; var dest; count: integer) : boolean;
    function WriteMemory (const source; var dest; count: integer) : boolean;
    function ZeroMemory  (              var dest; count: integer) : boolean;

    function GetThreads : IThreads;

    function CreateThread (startAddr     : pointer;
                           parameter     : pointer             = nil;
                           creationFlags : cardinal            =   0;
                           stackSize     : cardinal            =   0;
                           threadAttr    : PSecurityAttributes = nil) : IThread;

    function Module (handle    : cardinal;
                     autoClose : boolean = false) : IModule; overload;
    function Module (memory    : pointer;
                     autoClose : boolean = false) : IModule; overload;
    function Module (fileName  : AnsiString;
                     autoClose : boolean = false) : IModule; overload;

    function LoadModule (fileName          : AnsiString;
                         autoClose         : boolean = true;
                         withoutReferences : boolean = false;
                         onlyAsDataFile    : boolean = false;
                         alteredSearchPath : boolean = false;
                         timeOut           : integer = 3000 ) : IModule;

    // internal: is only called from IModule.Destroy
    function FreeModule (moduleHandle: cardinal) : boolean;

    function GetModules : IModules;

    function GetExportList : IXxportList;

    function GetTrayIcons : ITrayIcons;

    function GetWindows : IWindows;

    function GetTaskbarWindows : IWindows;

    //internal: is only called from IWindow.SetParam
    function SetWindowLong (window: cardinal; index, value: integer) : boolean;

    function GetHandles2 : IHandles;
    function IProcess.GetHandles = GetHandles2;

    // internal: is only called from IHandle.Destroy
    function FreeHandle (handle: cardinal) : boolean;

    function Minimize (activate: boolean = false) : boolean;
    function Maximize (activate: boolean = true ) : boolean;
    function Restore  (activate: boolean = true ) : boolean;

    {$ifdef madDisAsm}
      // internal: checks the function code that begins at "func"
      function CheckFunction (func                 : pointer;
                              paramsSize           : integer;
                              loadModules          : boolean;
                              out cfr              : TCopyFunctionRec;
                              out fi               : TFunctionInfo;
                              out bufferSize       : cardinal;
                              acceptUnknownTargets : boolean = false) : boolean;

      // internal: relocates the function code that begins at "func" for this process
      function RelocateFunction (var   cfr : TCopyFunctionRec;
                                 const fi  : TFunctionInfo   ) : boolean;

      // internal: copies the function code that begins at "func" to this process
      function CopyFunction (func                 : pointer;
                             paramsSize           : integer = 0;
                             acceptUnknownTargets : boolean = false) : TCopyFunctionRec;
    {$endif}

    // internal: copies the code between "beginCode" and "endCode" from our process
    // to the destination in the other process and replaces all pointers in "replace"
    function CopyCode (beginCode   : pointer;
                       endCode     : TProcedure;
                       destination : pointer;
                       replace     : array of pointer) : boolean;

    // internal: injects an alien thread into this process and executes some code in it
    function InjectCommand (command : TInjectCommand;
                            params  : array of cardinal;
                            result_ : pointer    = nil;
                            str     : AnsiString = '';
                            timeOut : cardinal   = 1000  ) : boolean;

    function ExecuteFunction (func                 : TExecuteFunctionProc;
                              timeOut              : cardinal = 1000;
                              params               : pointer  = nil;
                              paramsSize           : cardinal = 0;
                              acceptUnknownTargets : boolean  = false) : boolean;

    function Suspend : boolean;
    function Resume  : boolean;

    function WaitForInputIdle (timeOut: cardinal = INFINITE) : boolean;

    function IsStillRunning : boolean;

    function Close : boolean;

    function Quit  : boolean;

    function Exit_ (exitCode: cardinal = 0) : boolean;

    function Terminate (exitCode: cardinal = 0) : boolean;

    function GetExitCode : cardinal;

    function GetTimes (var creation, exit, kernel, user: int64) : boolean;

    function GetMaxInterface : IBasic; override;
  end;

  // contains a list of specific IProcess objects
  TIProcesses = class (TIWaitableObjs, IProcesses)
  public
    FAll     : boolean;
    FExeFile : AnsiString;
    FList    : boolean;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        exeFile: AnsiString; const objects: array of IBasic);

    function GetExeFile : AnsiString;

    function GetItem (index: integer) : IProcess;

    function RefreshItems : boolean;

    procedure SetServiceProcess (serviceProcess: boolean = true);

    procedure SetPriorityClass (priorityClass: TPriorityClass);

    procedure SetPriorityBoost (priorityBoost: boolean);

    procedure SetAffinityMask (affinityMask: cardinal);

    function SetWorkingSetSize (minimum, maximum: cardinal) : boolean;

    function Minimize (activate: boolean = false) : boolean;
    function Maximize (activate: boolean = true ) : boolean;
    function Restore  (activate: boolean = true ) : boolean;

    function Suspend : boolean;
    function Resume  : boolean;

    function IsStillRunning (all: boolean = false) : boolean;

    function Close : boolean;

    function Quit  : boolean;

    function Exit_ (exitCode: cardinal = 0) : boolean;

    function Terminate (exitCode: cardinal = 0) : boolean;

    function GetMaxInterface : IBasic; override;
  end;

  // *******************************************************************

  // implements IOtherKernelObj
  TIOtherKernelObj = class (TIKernelObj, IOtherKernelObj)
  public
    function GetMaxInterface : IBasic; override;
  end;

  // *******************************************************************

  // implements IEvent
  TIEvent = class (TIWaitableObj, IEvent)
  public
    FName : AnsiString;

    function GetName : AnsiString;

    function IEvent.IsAuto = IsAutoEvent;

    function Lock   : boolean;
    function Unlock : boolean;
    function Pulse  : boolean;

    function GetMaxInterface : IBasic; override;
  end;

  // *******************************************************************

  // implements IMutex
  TIMutex = class (TIWaitableObj, IMutex)
  public
    FName : AnsiString;

    function GetName : AnsiString;

    function Enter (milliseconds: cardinal = INFINITE) : boolean;

    function TryEnter : boolean;

    function Leave : boolean;

    function GetMaxInterface : IBasic; override;
  end;

// ***************************************************************

type
  // types for QueryObjNtThread
  TQueryObjNtThreadRec  = record
    handle : cardinal;
    str    : AnsiString;
  end;
  TPQueryObjNtThreadRec = ^TQueryObjNtThreadRec;

  // type for NtQueryInformationThread
  TNtThreadInfoClass = (ThreadBasicInformation,
                        ThreadTimes,
                        ThreadPriority,
                        ThreadBasePriority,
                        ThreadAffinityMask,
                        ThreadImpersonationToken,
                        ThreadDescriptorTableEntry,
                        ThreadEnableAlignmentFaultFixup,
                        ThreadEventPair,
                        ThreadQuerySetWin32StartAddress,
                        ThreadZeroTlsCell,
                        ThreadPerformanceCount,
                        ThreadAmILastThread,
                        MaxThreadInfoClass);

var
  NtQueryInformationFile   : function (fileHandle    : cardinal;
                                       ioStatusBlock : pointer;
                                       buffer        : pointer;
                                       bufSize       : cardinal;
                                       infoClass     : cardinal  ) : cardinal stdcall = nil;
  NtQueryObject            : function (handle        : cardinal;
                                       funcNum       : cardinal;
                                       buffer        : pointer;
                                       bufSize       : cardinal;
                                       returnSize    : TPCardinal) : cardinal stdcall = nil;
  NtQueryInformationThread : function (threadHandle  : cardinal;
                                       infoClass     : TNtThreadInfoClass;
                                       buffer        : pointer;
                                       bufSize       : cardinal;
                                       returnSize    : TPCardinal) : cardinal stdcall = nil;
  NtQueryEvent             : function (eventHandle   : cardinal;
                                       infoClass     : cardinal;
                                       buffer        : pointer;
                                       bufSize       : cardinal;
                                       returnSize    : TPCardinal) : cardinal stdcall = nil;

const
  CConvertObjType95  : array [0..$11] of byte =
    ($00, $01, $02, $03, $00, $04, $05, $08, $0A, $10, $11, $07, $0B, $0F, $0C, $0D, $09, $0E);
  CConvertObjType98  : array [0..$13] of byte =
    ($00, $01, $02, $03, $00, $06, $04, $05, $08, $0A, $10, $00, $11, $07, $0B, $0F, $0C, $0D, $09, $0E);
  CObjNames : array [TKernelObjType] of PAnsiChar =
    ('Unknown', 'Semaphore', 'Event', 'Mutex|Mutant', 'Process', 'Thread', 'Timer', 'FileMapping|Section',
     'File', 'Snapshot', 'Notification', 'ComPort', 'Pipe', 'Mailslot', 'Socket', 'Vxd',
     'ConsoleInput', 'ConsoleOutput', 'Directory', 'SymbolicLink', 'Token', 'WindowStation',
     'Desktop', 'Key', 'Port', 'IoCompletion', 'KeyedEvent');

function GetHandleTableNt : TNtHandleTable;
var c1 : cardinal;
    p1 : pointer;
begin
  result.ItemCount := 0;
  result.Items := nil;
  if @NtQuerySystemInformation = nil then
    NtQuerySystemInformation := GetProcAddress(GetModuleHandle(ntdll), 'NtQuerySystemInformation');
  dword(p1) := LocalAlloc(LPTR, 20);
  try
    c1 := 0;
    NtQuerySystemInformation(16, p1, 20, @c1);
    LocalFree(dword(p1));
    dword(p1) := LocalAlloc(LPTR, c1 * 2);
    if NtQuerySystemInformation(16, p1, c1 * 2, nil) = 0 then begin
      result.ItemCount := TPInteger(p1)^;
      SetLength(result.Items, result.ItemCount);
      Move(pointer(integer(p1) + 4)^, pointer(result.Items)^, result.ItemCount * sizeOf(TNtHandleItem));
    end;
  finally LocalFree(dword(p1)) end;
end;

var KernelObjs : array of TIKernelObj = nil;
    KOSection  : ICriticalSection     = nil;

function AddKernelObj(type_: TKernelObjType; const handle: IHandle) : TIKernelObj; overload;
var i1, i2 : integer;
    addr   : pointer;
begin
  result := nil;
  if KOSection = nil then KOSection := NewCriticalSection;
  KOSection.Enter;
  try
    with TIHandle(handle.SelfAsTObject) do
      if FValid then begin
        type_ := FType;
        addr  := FAddr;
      end else
        addr := nil;
    i2 := Length(KernelObjs);
    if addr <> nil then begin
      for i1 := 0 to i2 - 1 do
        with KernelObjs[i1] do
          if (FType = type_) and (FAddr = addr) then begin
            result := KernelObjs[i1];
            break;
          end;
    end;
    if result = nil then begin
      case type_ of
        otEvent   : result := TIEvent         .Create(addr <> nil, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), handle);
        otMutex   : result := TIMutex         .Create(addr <> nil, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), handle);
        otThread  : result := TIThread        .Create(addr <> nil, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), handle);
        otProcess : result := TIProcess       .Create(addr <> nil, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), handle);
        else        result := TIOtherKernelObj.Create(addr <> nil, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), handle);
      end;
      SetLength(KernelObjs, i2 + 1);
      KernelObjs[i2] := result;
    end;
  finally KOSection.Leave end;
end;

function AddKernelObj(type_: TKernelObjType; id: cardinal) : TIKernelObj; overload;
var i1, i2 : integer;
begin
  result := nil;
  if KOSection = nil then KOSection := NewCriticalSection;
  KOSection.Enter;
  try
    i2 := Length(KernelObjs);
    for i1 := 0 to i2 - 1 do
      with KernelObjs[i1] do
        if (FType = type_) and (GetID = id) then begin
          result := KernelObjs[i1];
          break;
        end;
    if result = nil then begin
      case type_ of
        otThread  : result := TIThread        .Create(true, 0, '', type_, id);
        otProcess : result := TIProcess       .Create(true, 0, '', type_, id);
        else        result := TIOtherKernelObj.Create(true, 0, '', type_, id);
      end;
      SetLength(KernelObjs, i2 + 1);
      KernelObjs[i2] := result;
    end;
  finally KOSection.Leave end;
end;

procedure DelKernelObj(obj: TIKernelObj);
var i1, i2 : integer;
begin
  if (KOSection = nil) and (not InFinalization) then KOSection := NewCriticalSection;
  if KOSection <> nil then
    KOSection.Enter;
  try
    i2 := High(KernelObjs);
    for i1 := 0 to i2 do
      if KernelObjs[i1] = obj then begin
        KernelObjs[i1] := KernelObjs[i2];
        SetLength(KernelObjs, i2);
        break;
      end;
  finally
    if KOSection <> nil then
      KOSection.Leave;
  end;
end;

constructor TIKernelObj.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                               const handle: IHandle);
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FValid := (handle <> nil) and handle.IsValid;
    if FValid then begin
      if (not handle.AutoClose) or (handle.OwnerProcess.ID <> GetCurrentProcessID) then begin
        FHandle := handle.Duplicate;
        TIHandle(handle.SelfAsTObject).FKernelObj := IKernelObj(self.GetMaxInterface);
      end else FHandle := handle;
      FValid := FHandle.IsValid;
      if FValid then begin
// hierrr        FStore := 1;
        with TIHandle(FHandle.SelfAsTObject) do begin
          self.FType := FType;
          self.FAddr := FAddr;
        end;
        if OS.win9x then begin
          FTypeStrReady := true;
          FTypeStr      := SubStr(CObjNames[FType], 1);
          FIDReady      := true;
          FID           := cardinal(FAddr) xor Magic;
        end;
      end else SetLastError(FHandle.LastErrorNo, FHandle.LastErrorStr);
    end else SetLastError(ERROR_INVALID_PARAMETER);
  end;
end;

constructor TIKernelObj.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                               type_: TKernelObjType; id: cardinal);
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FType    := type_;
    FIDReady := true;
    FID      := id;
    if OS.win9x then begin
      FAddr         := pointer(FID xor Magic);
      FTypeStrReady := true;
      FTypeStr      := SubStr(CObjNames[FType], 1);
    end;
  end;
end;

destructor TIKernelObj.Destroy;
begin
  DelKernelObj(self);
  inherited;
end;

function TIKernelObj.GetObjType : TKernelObjType;
begin
  result := FType;
end;

function TIKernelObj.GetObjTypeStr : AnsiString;
var c1  : cardinal;
    buf : TPAPointer;
begin
  if not FTypeStrReady then begin
    FTypeStrReady := true;
    if CheckValid then begin
      with GetHandle do
        if IsValid then begin
          if @NtQueryObject = nil then
            NtQueryObject := GetProcAddress(GetModuleHandle(ntdll), 'NtQueryObject');
          c1 := 0;
          NtQueryObject(Handle, 2, nil, 0, @c1);
          if (c1 > 0) and (c1 and $80000000 = 0) then begin
            buf := AllocMem(c1);
            try
              if NtQueryObject(Handle, 2, buf, c1, nil) = 0 then
                FTypeStr := AnsiString(UnicodeString(PWideChar(buf^[1])));
            finally FreeMem(buf) end;
          end;
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else FTypeStr := SubStr(CObjNames[otUnknown], 1);
  end;
  result := FTypeStr;
end;

function TIKernelObj.GetObjAddr : pointer;
begin
  if (FAddr = nil) and CheckValid and OS.winNT then
    if FHandle <> nil then
         FAddr := TIHandle(  FHandle.SelfAsTObject).FAddr
    else FAddr := TIHandle(GetHandle.SelfAsTObject).FAddr;
  result := FAddr;
end;

function QueryObjNtThread(tr: TPQueryObjNtThreadRec) : cardinal; stdcall;
var buf : record len: cardinal; str: array [0..MAX_PATH] of wideChar; end;
    iob : record v1, v2: cardinal; end;
begin
  result := 0;
  try
    if @NtQueryInformationFile = nil then
      NtQueryInformationFile := GetProcAddress(GetModuleHandle(ntdll), 'NtQueryInformationFile');
    if NtQueryInformationFile(tr^.handle, @iob, @buf, sizeOf(buf), 9) = 0 then begin
      if buf.len div 2 > MAX_PATH then
           buf.str[MAX_PATH     ] := #0
      else buf.str[buf.len div 2] := #0;
      tr^.str := AnsiString(UnicodeString(buf.str));
    end;
  except sysUtils.ShowException(ExceptObject, ExceptAddr) end;
end;

function TIKernelObj.GetObjName : AnsiString;
var c1  : cardinal;
    buf : TPAPointer;
    th  : cardinal;
    tr  : TQueryObjNtThreadRec;
begin
  if OS.winNT then begin
    if (not FNameReady) and CheckValid then begin
      FNameReady := true;
      with GetHandle do
        if IsValid then begin
          if @NtQueryObject = nil then
            NtQueryObject := GetProcAddress(GetModuleHandle(ntdll), 'NtQueryObject');
          c1 := 0;
          NtQueryObject(Handle, 1, nil, 0, @c1);
          if (c1 = 0) or (c1 and $80000000 <> 0) then begin
            tr.handle := Handle;
            tr.str := '';
            th := CreateThread(nil, 0, @QueryObjNtThread, @tr, 0, c1);
            if th <> 0 then
              try
                if WaitForSingleObject(th, 500) = WAIT_OBJECT_0 then begin
                  FName := tr.str;
                  c1 := $5000;
                end else begin
                  TerminateThread(th, 0);
                  c1 := 0;
                end;
              finally CloseHandle(th) end;
          end;
          if (c1 > 0) and (c1 and $80000000 = 0) then begin
            if c1 < $5000 then
              c1 := $5000;
            buf := VirtualAlloc(nil, c1, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
            try
              c1 := NtQueryObject(Handle, 1, buf, c1, nil);
              if c1 = 0 then
                   FName := AnsiString(UnicodeString(PWideChar(buf^[1])))
              else self.SetLastError(c1);
            finally VirtualFree(buf, 0, MEM_RELEASE) end;
          end;
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end;
  end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  result := FName;
end;

function TIKernelObj.GetID : cardinal;
var c1  : cardinal;
    buf : array [0..6] of cardinal;
    pbi : TNtProcessBasicInfo;
begin
  if (not FIDReady) and (FType in [otThread, otProcess]) and CheckValid then begin
    FIDReady := true;
    with GetHandle do
      if IsValid then begin
        if FType = otThread then begin
          ZeroMemory(@buf, sizeOf(buf));
          if @NtQueryInformationThread = nil then
            NtQueryInformationThread :=
              GetProcAddress(GetModuleHandle(ntdll), 'NtQueryInformationThread');
          if @NtQueryInformationThread <> nil then begin
            c1 := NtQueryInformationThread(Handle, ThreadBasicInformation, @buf,
                                           sizeOf(buf), nil);
            if c1 = 0 then FID := buf[3]
            else           self.SetLastError(c1);
          end else self.SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
        end else begin
          ZeroMemory(@pbi, sizeOf(TNtProcessBasicInfo));
          if @NtQueryInformationProcess = nil then
            NtQueryInformationProcess :=
              GetProcAddress(GetModuleHandle(ntdll), 'NtQueryInformationProcess');
          if @NtQueryInformationProcess <> nil then begin
            c1 := NtQueryInformationProcess(Handle, ProcessBasicInformation, @pbi,
                                            sizeOf(TNtProcessBasicInfo), nil);
            if c1 = 0 then FID := pbi.pid
            else           self.SetLastError(c1);
          end else self.SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
        end;
      end else self.SetLastError(LastErrorNo, LastErrorStr);
  end;
  result := FID;
end;

function TIKernelObj.IsAutoEvent : boolean;
var buf : record
            flags : cardinal;
            dummy : cardinal;
          end;
    c1  : cardinal;
begin
  if (not FEventReady) and CheckValid then begin
    FEventReady := true;
    if OS.winNT then begin
      with GetHandle do
        if IsValid then begin
          if @NtQueryEvent = nil then
            NtQueryEvent := GetProcAddress(GetModuleHandle(ntdll), 'NtQueryEvent');
          if @NtQueryEvent <> nil then begin
            buf.flags := 0;
            c1 := NtQueryEvent(Handle, 0, @buf, 8, @c1);
            if c1 = 0 then FAutoEvent := buf.flags and $01 <> 0
            else           self.SetLastError(c1);
          end else self.SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else
      try
        if Magic95 then
             FAutoEvent := TPAByte(FAddr)^[24] and $01 = 0
        else FAutoEvent := TPAByte(FAddr)^[01] and $80 = 0;
      except end;
  end;
  result := FAutoEvent;
end;

function TIKernelObj.GetHandle : IHandle;
begin
  if CheckValid and (FHandle.OwnerProcess.ID <> GetCurrentProcessID) then begin
    FHandle := FHandle.Duplicate;
    if not FHandle.IsValid then
      SetLastError(FHandle.LastErrorNo, FHandle.LastErrorStr);
  end;
  if FHandle = nil then
    FHandle := TIHandle.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), 0, 0, false, nil, nil, otUnknown);
  result := FHandle;
end;

function TIKernelObj.GetHandles : IHandles;
begin
  if CheckValid then
       result := TIHandles.Create(true, 0, '', CurrentProcess, self, [])
  else result := TIHandles.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), nil, nil, []);
end;

// ***************************************************************

function KernelObjTypeStrToKernelObjType(typeStr: AnsiString) : TKernelObjType;
var i1 : integer;
    ot : TKernelObjType;
begin
  result := otUnknown;
  for ot := low(TKernelObjType) to high(TKernelObjType) do
    for i1 := 1 to SubStrCount(CObjNames[ot]) do
      if IsTextEqual(SubStr(CObjNames[ot], i1), typeStr) then begin
        result := ot;
        exit;
      end;
end;

var ConvertObjType : array of TKernelObjType;

function KernelObjTypeNoToKernelObjType(typeNo: word) : TKernelObjType;
var ht : TNtHandleTable;

  procedure AddItem(objType: TKernelObjType; handle: dword; close: boolean = true);
  var i1 : integer;
  begin
    if (handle <> 0) and (handle <> INVALID_HANDLE_VALUE) then begin
      with ht do
        for i1 := 0 to ItemCount - 1 do
          if (Items[i1].pid = GetCurrentProcessId) and (Items[i1].handle = handle) then begin
            if byte(Items[i1].objType) > high(ConvertObjType) then
              SetLength(ConvertObjType, byte(Items[i1].objType) + 1);
            ConvertObjType[byte(Items[i1].objType)] := objType;
            break;
          end;
      if close then
        CloseHandle(handle);
    end;
  end;

var semaphore, event, mutex, process, thread, file_, section : dword;
    token  : THandle;
    arrChW : array [0..MAX_PATH] of wideChar;
    i1     : integer;
    c1     : dword;
    buf    : TPAPointer;
begin
  result := otUnknown;
  if ConvertObjType = nil then begin
    SetLength(ConvertObjType, 1);
    
    GetModuleFileNameW(0, arrChW, MAX_PATH);
    semaphore := CreateSemaphore(nil, 0, 10, nil);
    event     := CreateEvent(nil, true, false, nil);
    mutex     := CreateMutex(nil, false, nil);
    process   := 0;
    thread    := 0;
    file_     := CreateFileW(arrChW, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    section   := CreateFileMapping(maxCard, nil, PAGE_READWRITE, 0, 100, nil);
    token     := 0;
    DuplicateHandle(windows.GetCurrentProcess, windows.GetCurrentProcess, windows.GetCurrentProcess, @process, 0, true, DUPLICATE_SAME_ACCESS);
    DuplicateHandle(windows.GetCurrentProcess, windows.GetCurrentThread, windows.GetCurrentProcess, @thread, 0, true, DUPLICATE_SAME_ACCESS);
    OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, token);

    ht := GetHandleTableNt;

    AddItem(otSemaphore,   semaphore);
    AddItem(otEvent,       event    );
    AddItem(otMutex,       mutex    );
    AddItem(otProcess,     process  );
    AddItem(otThread,      thread   );
    AddItem(otFile,        file_    );
    AddItem(otFileMapping, section  );
    AddItem(otToken,       token    );

    for i1 := 0 to ht.ItemCount - 1 do
      if (ht.Items[i1].pid = GetCurrentProcessId) and
         ( (byte(ht.Items[i1].objType) > high(ConvertObjType)) or
           (ConvertObjType[byte(ht.Items[i1].objType)] = otUnknown) ) then begin
        if @NtQueryObject = nil then
          NtQueryObject := GetProcAddress(GetModuleHandle(ntdll), 'NtQueryObject');
        c1 := 0;
        NtQueryObject(ht.Items[i1].handle, 2, nil, 0, @c1);
        if (c1 > 0) and (c1 and $80000000 = 0) then begin
          buf := AllocMem(c1);
          try
            if NtQueryObject(ht.Items[i1].handle, 2, buf, c1, nil) = 0 then
              AddItem(KernelObjTypeStrToKernelObjType(AnsiString(UnicodeString(PWideChar(buf^[1])))), ht.Items[i1].handle, false);
          finally FreeMem(buf) end;
        end;
      end;

    // still missing:
    // otDirectory
    // otSymbolicLink
    // otWindowStation
    // otDesktop
    // otKey
    // otPort
    // otIoCompletion
    // otKeyedEvent
  end;
  if typeNo <= high(ConvertObjType) then
    result := ConvertObjType[typeNo];
end;

constructor TIHandle.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                            processID, handle: cardinal; autoClose: boolean;
                            hi95: TP9xHandleItem; hiNt: TPNtHandleItem;
                            typ: TKernelObjType);
var c1  : cardinal;
    ht  : TNtHandleTable;
    i1  : integer;
    obi : array [0..13] of dword;  // object basic information
    s1  : AnsiString;
    buf : TPAPointer;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    if (handle <> maxCard) and (handle <> 0) then begin
      FProcess   := processID;
      FHandle    := handle;
      FAutoClose := autoClose;
      FType      := typ;
      if OS.win9x then begin
        try
          if hi95 = nil then begin
            if Magic95 then c1 := FHandle
            else            c1 := FHandle div 4;
            with TP9xPid(FProcess xor Magic)^.handleTable^ do
              if integer(c1) < itemCount then
                hi95 := @items[c1];
          end;
          if hi95 <> nil then begin
            FAddr := hi95^.objAddr;
            FAccess := hi95^.access;
            if FType = otUnknown then begin
              i1 := TP9xPid(cardinal(hi95^.objAddr))^.objectType;
              if Magic95 then begin
                if i1 > ord(high(CConvertObjType95)) then
                     FType := otUnknown
                else FType := TKernelObjType(CConvertObjType95[i1]);
              end else
                if i1 > ord(high(CConvertObjType98)) then
                     FType := otUnknown
                else FType := TKernelObjType(CConvertObjType98[i1]);
            end;
          end else FHandle := maxCard;
        except FHandle := maxCard end;
      end else begin
        if @NtQueryObject = nil then
          NtQueryObject := GetProcAddress(GetModuleHandle(ntdll), 'NtQueryObject');
        if FType = otUnknown then begin
          if hiNt <> nil then
            FType := KernelObjTypeNoToKernelObjType(byte(hiNt.objType));
          if (FType = otUnknown) and (FProcess = GetCurrentProcessId) then begin
            c1 := 0;
            NtQueryObject(FHandle, 2, nil, 0, @c1);
            s1 := '';
            if (c1 > 0) and (c1 and $80000000 = 0) then begin
              buf := AllocMem(c1);
              try
                if NtQueryObject(FHandle, 2, buf, c1, nil) = 0 then
                  s1 := AnsiString(UnicodeString(PWideChar(buf^[1])));
              finally FreeMem(buf) end;
              FType := KernelObjTypeStrToKernelObjType(s1);
            end;
          end;
        end;
        if FType <> otUnknown then begin
          if hiNt = nil then begin
            ht := GetHandleTableNt;
            with ht do
              for i1 := 0 to ItemCount - 1 do
                if (Items[i1].pid = FProcess) and (Items[i1].handle = FHandle) then begin
                  hiNt := @Items[i1];
                  break;
                end;
          end;
          if hiNt <> nil then begin
            FAccess := hiNt^.access;
            FAddr   := hiNt^.objAddr;
          end else begin
            if (processID = GetCurrentProcessId) and (NtQueryObject(FHandle, 0, @obi, sizeOf(obi), nil) = 0) then
              FAccess := obi[1];
            FAddr := pointer(FHandle);
          end;
        end else
          FHandle := maxCard;
      end;
      if FHandle <> maxCard then
           FSuccess := true
      else SetLastError(ERROR_INVALID_PARAMETER);
    end else SetLastError(ERROR_INVALID_PARAMETER);
    FValid := (FHandle <> 0) and (FHandle <> maxCard);
  end else FHandle := maxCard;
end;

destructor TIHandle.Destroy;
begin
  if FValid and FAutoClose then
    if FProcess = GetCurrentProcessID then
         CloseHandle(FHandle)
    else TIProcess(Process(FProcess).SelfAsTObject).FreeHandle(FHandle);
  inherited Destroy;
end;

function TIHandle.IsStillValid : boolean;
begin
  if FValid then
    with TIHandle.Create(true, 0, '', FProcess, FHandle, false, nil, nil, FType) do begin
      self.FValid := FValid and (FType = self.FType) and (FAddr = self.FAddr);
      Destroy;
    end;
  result := FValid;
end;

function TIHandle.GetOwnerProcess : IProcess;
begin
  if CheckValid then
       result := TIProcess(AddKernelObj(otProcess, FProcess))
  else result := TIProcess.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), otProcess, 0);
end;

function TIHandle.GetHandle : cardinal;
begin
  result := FHandle;
end;

function TIHandle.GetAccess : cardinal;
begin
  result := FAccess;
end;

function TIHandle.GetObjType : TKernelObjType;
begin
  result := FType;
end;

function TIHandle.GetKernelObj : IKernelObj;
begin
  if FKernelObj = nil then begin
    if CheckValid then begin
      result := AddKernelObj(FType, self);
      if not FAutoClose then
        FKernelObj := result;
    end else begin
      result := TIOtherKernelObj.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), otUnknown, 0);
      FKernelObj := result;
    end;
  end else result := FKernelObj;
end;

function TIHandle.GetAutoClose : boolean;
begin
  result := FAutoClose;
end;

procedure TIHandle.SetAutoClose(value: boolean);
begin
  if CheckValid and (value <> FAutoClose) then begin
    FAutoClose := value;
    if FAutoClose then
      FKernelObj := nil;
  end;
end;

function TIHandle.WouldWait : boolean;
begin
  result := false;
  if CheckValid then
    if FProcess = GetCurrentProcessID then begin
      case WaitForSingleObject(FHandle, 0) of
        WAIT_TIMEOUT   : begin
                           result := true;
                           SetLastError(WAIT_TIMEOUT, UnicodeString(AnsiString(CErrorStr_WaitTimeout)));
                         end;
        WAIT_ABANDONED : SetLastError(WAIT_ABANDONED, UnicodeString(AnsiString(CErrorStr_WaitAbandoned)));
        WAIT_OBJECT_0  : begin
                           case FType of
                             otMutex : ReleaseMutex(FHandle);
                             otEvent : if GetKernelObj.IsAutoEvent then SetEvent(FHandle);
                           end;
                           SetLastError(0);
                         end;
        else             SetLastError(GetLastError);
      end;
    end else result := Duplicate.WouldWait;
end;

function TIHandle.WaitFor(milliseconds   : cardinal = INFINITE;
                          handleMessages : boolean  = true    ) : boolean;
begin
  result := false;
  if CheckValid then
    if FProcess = GetCurrentProcessID then begin
      case madKernel.WaitFor([IHandle(self)], false, milliseconds, handleMessages) of
        WAIT_TIMEOUT   : SetLastError(WAIT_TIMEOUT,   UnicodeString(AnsiString(CErrorStr_WaitTimeout  )));
        WAIT_ABANDONED : SetLastError(WAIT_ABANDONED, UnicodeString(AnsiString(CErrorStr_WaitAbandoned)));
        WAIT_OBJECT_0  : result := true;
        else             SetLastError(GetLastError);
      end;
    end else result := Duplicate.WaitFor(milliseconds, handleMessages);
end;

function TIHandle.Notify(window, msg: cardinal) : boolean;
begin
  result := false;
  if CheckValid then
    if FProcess = GetCurrentProcessID then begin
      result := madKernel.Notify(window, msg, [IHandle(self)], false, [], false);
      if not result then SetLastError(WAIT_FAILED, UnicodeString(AnsiString(CErrorStr_WaitFailed)));
    end else result := Duplicate.Notify(window, msg);
end;

function TIHandle.Duplicate(autoClose           : boolean;
                            access              : cardinal;
                            inheritHandles      : boolean;
                            const targetProcess : IProcess) : IHandle;
var handle   : cardinal;
    c1       : cardinal;
    ih1, ih2 : IHandle;
begin
  if (targetProcess <> nil) and targetProcess.IsValid then begin
    result := nil;
    if CheckValid then begin
      ih1 := GetOwnerProcess.GetHandle;
      if ih1.IsValid then begin
        ih2 := targetProcess.GetHandle(PROCESS_DUP_HANDLE);
        if ih2.IsValid then begin
          if access = 0 then c1 := DUPLICATE_SAME_ACCESS
          else               c1 := 0;
          if DuplicateHandle(ih1.Handle, FHandle, ih2.Handle, @handle, access, inheritHandles, c1) then
               result := TIHandle.Create(true, 0, '', targetProcess.ID, handle, autoClose, nil, nil, FType)
          else SetLastError(GetLastError);
        end else SetLastError(ih2.LastErrorNo, ih2.LastErrorStr);
      end else SetLastError(ih1.LastErrorNo, ih1.LastErrorStr);
    end;
    if result = nil then
      result := TIHandle.Create(false, FLastErrorNo, GetLastErrorStr, 0, 0, false, nil, nil, otUnknown);
  end else result := TIHandle.Create(false, ERROR_INVALID_PARAMETER, '', 0, 0, false, nil, nil, otUnknown);
end;

function TIHandle.Duplicate(autoClose           : boolean;
                            access              : cardinal;
                            inheritHandles      : boolean;
                            const targetProcess : IHandle ) : IHandle;
begin
  result := Duplicate(autoClose, access, inheritHandles, Process(targetProcess));
end;

function TIHandle.Duplicate(autoClose      : boolean  = true;
                            access         : cardinal = 0;
                            inheritHandles : boolean  = true;
                            targetProcess  : cardinal = 0   ) : IHandle;
begin
  if targetProcess = 0 then
       result := Duplicate(autoClose, access, inheritHandles, CurrentProcess        )
  else result := Duplicate(autoClose, access, inheritHandles, Process(targetProcess));
end;

function TIHandle.GetMaxInterface : IBasic;
begin
  result := IHandle(self);
end;

function Handle(handle    : cardinal;
                autoClose : boolean = true) : IHandle;
begin
  result := TIHandle.Create(true, 0, '', GetCurrentProcessID, handle, autoClose, nil, nil, otUnknown);
end;

// ***************************************************************

constructor TIHandles.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                             const process: IProcess; const kernelObj: IKernelObj;
                             const objects: array of IBasic);
var i1 : integer;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FAll := process = nil;
    FProcess := process;
    FObj     := kernelObj;
    FList    := Length(objects) > 0;
    if FList then
      for i1 := 0 to high(objects) do
        if objects[i1] <> nil then
          AddItem(objects[i1]);
    if not FList then RefreshItems;
  end;
end;

function TIHandles.GetOwnerProcess : IProcess;
begin
  if FProcess = nil then
    FProcess := TIProcess.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), otProcess, 0);
  result := FProcess;
end;

function TIHandles.GetKernelObj : IKernelObj;
begin
  if FObj = nil then
    FObj := TIOtherKernelObj.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), otUnknown, 0);
  result := FObj;
end;

function TIHandles.GetItem(index: integer) : IHandle;
begin
  if (index < 0) or (index >= FCount) then
       result := TIHandle.Create(false, CErrorNo_IndexOutOfRange, UnicodeString(AnsiString(CErrorStr_IndexOutOfRange)), 0, 0, false, nil, nil, otUnknown)
  else result := IHandle(FItems[index]);
end;

function TIHandles.RefreshItems : boolean;
var oldCount : integer;

  procedure RefreshItem(handle, access, process: dword;
                        addr: pointer; hi95: TP9xHandleItem; hiNt: TPNtHandleItem);
  var i1  : integer;
      ih1 : TIHandle;
  begin
    if (FObj = nil) or (FObj.ObjAddr = addr) then begin
      if oldCount > 0 then
        for i1 := 0 to FCount - 1 do
          with TIHandle(FItems[i1].SelfAsTObject) do
            if (FProcess = process) and (FHandle = handle) and (FAddr = addr) then begin
              if FAccess <> access then begin
                Change(FItems[i1], true, lctChanged, i1, i1);
                FAccess := access;
                self.FItemInfos[i1].LastChangeType := lctChanged;
                Change(FItems[i1], false, lctChanged, i1, SortItem(i1));
              end else
                self.FItemInfos[i1].LastChangeType := lctUnChanged;
              exit;
            end;
      ih1 := TIHandle.Create(true, 0, '', process, handle, false, hi95, hiNt, otUnknown);
      if AddItem(IHandle(ih1)) = -1 then
        ih1.Destroy;
    end;
  end;

var i1, i2, i3 : integer;
begin
  result := false;
  if CheckValid then begin
    BeginRefresh;
    try
      oldCount := FCount;
      if OS.win9x then begin
        if (FProcess = nil) or (not FProcess.IsValid) then begin
          with Processes do
            for i3 := 0 to ItemCount - 1 do
              with TP9xPid(Items[i3].ObjAddr)^.handleTable^ do
                for i1 := 0 to itemCount - 1 do
                  if (items[i1].objAddr <> nil) and (cardinal(items[i1].objAddr) <> Magic) then begin
                    if Magic95 then i2 := i1
                    else            i2 := i1 * 4;
                    RefreshItem(i2, items[i1].access, GetItem(i3).ID, items[i1].objAddr, @items[i1], nil);
                  end;
        end else
          with TP9xPid(FProcess.ObjAddr)^.handleTable^ do
            for i1 := 0 to itemCount - 1 do
              if (items[i1].objAddr <> nil) and (cardinal(items[i1].objAddr) <> Magic) then begin
                if Magic95 then i2 := i1
                else            i2 := i1 * 4;
                RefreshItem(i2, items[i1].access, FProcess.ID, items[i1].objAddr, @items[i1], nil);
              end;
      end else
        with GetHandleTableNt do
          for i1 := 0 to ItemCount - 1 do
            if (FProcess = nil) or (not FProcess.IsValid) or (Items[i1].pid = FProcess.ID) then
              RefreshItem(Items[i1].handle, Items[i1].access, Items[i1].pid, Items[i1].objAddr, nil, @Items[i1]);
      result := true;
    except end;
    EndRefresh;
  end;
end;

// Delphi4's overload doesn't work perfectly, so we need to help a bit
{$ifdef ver120}
  var WaitForArr : function (const objects  : array of IBasic;
                             waitAll        : boolean          = false;
                             milliseconds   : cardinal         = INFINITE;
                             handleMessages : boolean          = true;
                             msgs           : TWaitForMessages = [];
                             alertable      : boolean          = false   ) : integer
                 = WaitFor;
{$endif}

function TIHandles.WouldWait(waitAll: boolean) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    {$ifdef ver120} case           WaitForArr(FItems, waitAll, 0, false) of
    {$else}         case madKernel.WaitFor   (FItems, waitAll, 0, false) of {$endif}
      WAIT_TIMEOUT:
        begin
          result := true;
          SetLastError(WAIT_TIMEOUT, UnicodeString(AnsiString(CErrorStr_WaitTimeout)));
        end;
      WAIT_ABANDONED .. (WAIT_ABANDONED + MAXIMUM_WAIT_OBJECTS - 1):
        SetLastError(WAIT_ABANDONED, UnicodeString(AnsiString(CErrorStr_WaitAbandoned)));
      WAIT_OBJECT_0 .. (WAIT_OBJECT_0 + MAXIMUM_WAIT_OBJECTS - 1):
        begin
          for i1 := 0 to FCount - 1 do
            with IHandle(FItems[i1]) do
              case ObjType of
                otMutex : ReleaseMutex(Handle);
                otEvent : if KernelObj.IsAutoEvent then SetEvent(Handle);
              end;
          SetLastError(0);
        end;
      else
        SetLastError(GetLastError);
    end;
end;

function TIHandles.WaitFor(waitAll        : boolean   = false;
                           milliseconds   : cardinal  = INFINITE;
                           handleMessages : boolean   = true;
                           index          : TPInteger = nil     ) : boolean;
var i1 : integer;
begin
  result := false;
  if index <> nil then index^ := -1;
  if CheckValid then begin
    {$ifdef ver120} i1 :=           WaitForArr(FItems, waitAll, milliseconds, handleMessages);
    {$else}         i1 := madKernel.WaitFor   (FItems, waitAll, milliseconds, handleMessages); {$endif}
    case i1 of
      WAIT_TIMEOUT:
        SetLastError(WAIT_TIMEOUT, UnicodeString(AnsiString(CErrorStr_WaitTimeout)));
      WAIT_ABANDONED .. (WAIT_ABANDONED + MAXIMUM_WAIT_OBJECTS - 1):
        begin
          SetLastError(WAIT_ABANDONED, UnicodeString(AnsiString(CErrorStr_WaitAbandoned)));
          if index <> nil then index^ := i1 - WAIT_ABANDONED;
        end;
      WAIT_OBJECT_0 .. (WAIT_OBJECT_0 + MAXIMUM_WAIT_OBJECTS - 1):
        begin
          result := true;
          if index <> nil then index^ := i1 - WAIT_ABANDONED;
        end;
      else
        SetLastError(GetLastError);
    end;
  end;
end;

// Delphi4's overload doesn't work perfectly, so we need to help a bit
{$ifdef ver120}
  var NotifyArr : function (window        : cardinal;
                            msg           : cardinal;
                            const objects : array of IBasic;
                            waitAll       : boolean          = false;
                            msgs          : TWaitForMessages = [];
                            alertable     : boolean          = false) : boolean
                = Notify;
{$endif}

function TIHandles.Notify(window  : cardinal;
                          msg     : cardinal;
                          waitAll : boolean = false) : boolean;
begin
  if CheckValid then begin
    {$ifdef ver120} result :=           NotifyArr(window, msg, FItems, false, [], false);
    {$else}         result := madKernel.Notify   (window, msg, FItems, false, [], false); {$endif}
    if not result then SetLastError(WAIT_FAILED, UnicodeString(AnsiString(CErrorStr_WaitFailed)));
  end else result := false;
end;

function TIHandles.GetMaxInterface : IBasic;
begin
  result := IHandles(self);
end;

function Handles(systemWide: boolean = false) : IHandles;
begin
  if systemWide then
       result := TIHandles.Create(true, 0, '', nil,            nil, [])
  else result := TIHandles.Create(true, 0, '', CurrentProcess, nil, []);
end;

function PickHandles(const objects: array of IBasic) : IHandles;
begin
  result := TIHandles.Create(true, 0, '', nil, nil, PickInterfaces(objects, TIHandle));
end;

function PickHandles(const objects: ICustomBasicList) : IHandles;
begin
  if (objects <> nil) and objects.IsValid then begin
    result := TIHandles.Create(true, 0, '', nil, nil,
                PickInterfaces(TICustomBasicList(objects.SelfAsTObject).FItems, TIHandle));
  end else
    result := TIHandles.Create(false, ERROR_INVALID_PARAMETER, '', nil, nil, []);
end;

// ***************************************************************

function TIWaitableObj.WouldWait : boolean;
begin
  result := false;
  if CheckValid then
    with GetHandle do begin
      result := IsValid and WouldWait;
      self.SetLastError(LastErrorNo, LastErrorStr);
    end;
end;

function TIWaitableObj.WaitFor(milliseconds   : cardinal = INFINITE;
                               handleMessages : boolean  = true    ) : boolean;
begin
  result := false;
  if CheckValid then
    with GetHandle do begin
      result := IsValid and WaitFor(milliseconds, handleMessages);
      self.SetLastError(LastErrorNo, LastErrorStr);
    end;
end;

function TIWaitableObj.Notify(window, msg: cardinal) : boolean;
begin
  result := false;
  if CheckValid then
    with GetHandle do begin
      result := IsValid and Notify(window, msg);
      self.SetLastError(LastErrorNo, LastErrorStr);
    end;
end;

// ***************************************************************

function TIWaitableObjs.WouldWait(waitAll: boolean) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    {$ifdef ver120} case           WaitForArr(FItems, waitAll, 0, false) of
    {$else}         case madKernel.WaitFor   (FItems, waitAll, 0, false) of {$endif}
      WAIT_TIMEOUT:
        begin
          result := true;
          SetLastError(WAIT_TIMEOUT, UnicodeString(AnsiString(CErrorStr_WaitTimeout)));
        end;
      WAIT_ABANDONED .. (WAIT_ABANDONED + MAXIMUM_WAIT_OBJECTS - 1):
        SetLastError(WAIT_ABANDONED, UnicodeString(AnsiString(CErrorStr_WaitAbandoned)));
      WAIT_OBJECT_0 .. (WAIT_OBJECT_0 + MAXIMUM_WAIT_OBJECTS - 1):
        begin
          for i1 := 0 to FCount - 1 do
            with IWaitableObj(FItems[i1]) do
              case ObjType of
                otMutex : ReleaseMutex(Handle.Handle);
                otEvent : if IsAutoEvent then SetEvent(Handle.Handle);
              end;
          SetLastError(0);
        end;
      else
        SetLastError(GetLastError);
    end;
end;

function TIWaitableObjs.WaitFor(waitAll        : boolean   = false;
                                milliseconds   : cardinal  = INFINITE;
                                handleMessages : boolean   = true;
                                index          : TPInteger = nil     ) : boolean;
var i1 : integer;
begin
  result := false;
  if index <> nil then index^ := -1;
  if CheckValid then begin
    {$ifdef ver120} i1 :=           WaitForArr(FItems, waitAll, milliseconds, handleMessages);
    {$else}         i1 := madKernel.WaitFor   (FItems, waitAll, milliseconds, handleMessages); {$endif}
    case i1 of
      WAIT_TIMEOUT:
        SetLastError(WAIT_TIMEOUT, UnicodeString(AnsiString(CErrorStr_WaitTimeout)));
      WAIT_ABANDONED .. (WAIT_ABANDONED + MAXIMUM_WAIT_OBJECTS - 1):
        begin
          SetLastError(WAIT_ABANDONED, UnicodeString(AnsiString(CErrorStr_WaitAbandoned)));
          if index <> nil then index^ := i1 - WAIT_ABANDONED;
        end;
      WAIT_OBJECT_0 .. (WAIT_OBJECT_0 + MAXIMUM_WAIT_OBJECTS - 1):
        begin
          result := true;
          if index <> nil then index^ := i1 - WAIT_ABANDONED;
        end;
      else
        SetLastError(GetLastError);
    end;
  end;
end;

function TIWaitableObjs.Notify(window  : cardinal;
                               msg     : cardinal;
                               waitAll : boolean = false) : boolean;
begin
  if CheckValid then begin
    {$ifdef ver120} result :=           NotifyArr(window, msg, FItems, false, [], false);
    {$else}         result := madKernel.Notify   (window, msg, FItems, false, [], false); {$endif}
    if not result then SetLastError(WAIT_FAILED, UnicodeString(AnsiString(CErrorStr_WaitFailed)));
  end else result := false;
end;

// ***************************************************************

type
  // record for the wait thread
  TWaitForRec = record
    waitingThreadID : cardinal;
    timeOut         : cardinal;
    msgs_           : cardinal;
    flags           : cardinal;
    alertable_      : boolean;
    waitAll_        : boolean;
    handleCnt       : integer;
    handleArr       : array [0..MAXIMUM_WAIT_OBJECTS] of cardinal;
    ready           : boolean;
    result_         : integer;
    lastErrorNo     : cardinal;
  end;

  // record for the notify thread
  TNotifyRec = record
    window_    : cardinal;
    msg_       : cardinal;
    msgs_      : cardinal;
    flags      : cardinal;
    alertable_ : boolean;
    waitAll_   : boolean;
    handles    : array of IHandle;
  end;
  TPNotifyRec = ^TNotifyRec;

var
  // this functions in only available under winNT
  MsgWaitForMultipleObjectsEx : function (nCount         : cardinal;
                                          var pHandles;
                                          dwMilliseconds : cardinal;
                                          dwWakeMask     : cardinal;
                                          dwFlags        : cardinal) : cardinal stdcall = nil;
  MsgWaitExReady : boolean = false;

function WouldWait(const objects : array of IBasic;
                   waitAll       : boolean = false) : boolean;
var ah        : array of IHandle;
    i1        : integer;
    obj       : TIBasic;
    handleCnt : integer;
    handleArr : array [0..MAXIMUM_WAIT_OBJECTS] of cardinal;
begin
  result := false;
  handleCnt := 0;
  SetLength(ah, Length(objects));
  for i1 := 0 to high(objects) do
    if (objects[i1] <> nil) and objects[i1].IsValid then begin
      obj := TIBasic(objects[i1].SelfAsTObject);
      if not (obj is TIHandle) then
        if obj is TIWaitableObj then begin
          obj := TIBasic(TIWaitableObj(obj).GetHandle.SelfAsTObject);
          if not obj.FValid then obj := nil;
        end else obj := nil;
      if obj <> nil then begin
        ah[handleCnt] := IHandle(TIHandle(obj));
        handleArr[handleCnt] := ah[handleCnt].Handle;
        inc(handleCnt);
        if handleCnt = MAXIMUM_WAIT_OBJECTS - 1 then break;
      end;
    end;
  if handleCnt > 0 then begin
    SetLength(ah, handleCnt);
    case integer(WaitForMultipleObjects(handleCnt, @handleArr, waitAll, 0)) of
      WAIT_TIMEOUT:
        result := true;
      WAIT_OBJECT_0 .. (WAIT_OBJECT_0 + MAXIMUM_WAIT_OBJECTS - 1):
        for i1 := 0 to high(ah) do
          case ah[i1].ObjType of
            otMutex : ReleaseMutex(ah[i1].Handle);
            otEvent : if ah[i1].KernelObj.IsAutoEvent then SetEvent(ah[i1].Handle);
          end;
    end;
  end;
end;

// Delphi4's overload doesn't work perfectly, so we need to help a bit
{$ifdef ver120}
  var WouldWaitArr : function (const objects : array of IBasic;
                               waitAll       : boolean = false) : boolean
                   = WouldWait;
{$endif}                   

function WouldWait(const objects : ICustomBasicList;
                   waitAll       : boolean = false ) : boolean;
begin
  if (objects <> nil) and objects.IsValid and (objects.ItemCount > 0) then begin
    with TICustomBasicList(objects.SelfAsTObject) do begin
      SetCapacity(FCount);
      {$ifdef ver120} result := WouldWaitArr(FItems, waitAll);
      {$else}         result := WouldWait   (FItems, waitAll); {$endif}
    end;
  end else result := false;
end;

function WaitForThreadFunc(var waitForRec: TWaitForRec) : cardinal; stdcall;
begin
  result := 0;
  try
    with waitForRec do begin
      if msgs_ <> 0 then begin
        if @MsgWaitForMultipleObjectsEx = nil then
               result_ := integer(MsgWaitForMultipleObjects  (handleCnt,  handleArr, waitAll_, timeout, msgs_       ))
        else   result_ := integer(MsgWaitForMultipleObjectsEx(handleCnt,  handleArr,           timeout, msgs_, flags));
      end else result_ := integer(   WaitForMultipleObjectsEx(handleCnt, @handleArr, waitAll_, timeout, alertable_  ));
      lastErrorNo := GetLastError;
      ready := true;
      PostThreadMessageA(waitingThreadID, WM_NULL, 0, 0);
    end;
  except sysUtils.ShowException(ExceptObject, ExceptAddr) end;
end;

function WaitFor(const objects  : array of IBasic;
                 waitAll        : boolean          = false;
                 milliseconds   : cardinal         = INFINITE;
                 handleMessages : boolean          = true;
                 msgs           : TWaitForMessages = [];
                 alertable      : boolean          = false   ) : integer;
var ah      : array of IHandle;
    i1      : integer;
    obj     : TIBasic;
    wfr     : TWaitForRec;
    it1     : IThread;
    msg     : TMsg;
begin
  result := -1;
  if not MsgWaitExReady then begin
    MsgWaitExReady := true;
    MsgWaitForMultipleObjectsEx :=
      GetProcAddress(GetModuleHandle(user32), 'MsgWaitForMultipleObjectsEx');
  end;
  with wfr do begin
    if (msgs <> []) and alertable and (@MsgWaitForMultipleObjectsEx = nil) then
      alertable := false;
    handleCnt := 0;
    SetLength(ah, Length(objects));
    for i1 := 0 to high(objects) do
      if (objects[i1] <> nil) and objects[i1].IsValid then begin
        obj := TIBasic(objects[i1].SelfAsTObject);
        if not (obj is TIHandle) then
          if not (obj is TIWaitableObj) then begin
            obj := TIBasic(TIWaitableObj(obj).GetHandle.SelfAsTObject);
            if not obj.FValid then obj := nil;
          end else obj := nil;
        if obj <> nil then begin
          ah[handleCnt] := IHandle(TIHandle(obj));
          handleArr[handleCnt] := ah[handleCnt].Handle;
          inc(handleCnt);
          if handleCnt = MAXIMUM_WAIT_OBJECTS - 1 then break;
        end;
      end;
    if handleCnt > 0 then begin
      SetLength(ah, handleCnt);
      waitingThreadID := GetCurrentThreadID;
      timeout         := milliseconds;
      msgs_           := word(msgs);
      waitAll_        := waitAll;
      alertable_      := alertable;
      if waitAll then flags := MWMO_WAITALL else flags := 0;
      if alertable then flags := flags or MWMO_ALERTABLE;
      if handleMessages then begin
        ready := false;
        it1 := NewThread(@WaitForThreadFunc, @wfr);
        if it1.IsValid then begin
          while not ready do begin
            if dword(integer(GetMessage(msg, 0, 0, 0)) + 1) < 2 then begin
              it1.Terminate;
              exit;
            end;
            TranslateMessage(msg);
            DispatchMessage(msg);
          end;
          result := result_;
          SetLastError(lastErrorNo);
        end;
      end else
        if msgs_ <> 0 then begin
          if @MsgWaitForMultipleObjectsEx = nil then
                 result := integer(MsgWaitForMultipleObjects  (handleCnt,  handleArr, waitAll_, timeout, msgs_       ))
          else   result := integer(MsgWaitForMultipleObjectsEx(handleCnt,  handleArr,           timeout, msgs_, flags));
        end else result := integer(   WaitForMultipleObjectsEx(handleCnt, @handleArr, waitAll_, timeout, alertable_  ));
    end;
  end;
end;

function WaitFor(const objects  : ICustomBasicList;
                 waitAll        : boolean          = false;
                 milliseconds   : cardinal         = INFINITE;
                 handleMessages : boolean          = true;
                 msgs           : TWaitForMessages = [];
                 alertable      : boolean          = false   ) : integer;
begin
  if (objects <> nil) and objects.IsValid and (objects.ItemCount > 0) then begin
    with TICustomBasicList(objects.SelfAsTObject) do begin
      SetCapacity(FCount);
      {$ifdef ver120}
              result := WaitForArr(FItems, waitAll, milliseconds, handleMessages, msgs, alertable);
      {$else} result := WaitFor   (FItems, waitAll, milliseconds, handleMessages, msgs, alertable); {$endif}
    end;
  end else result := -1;
end;

function NotifyThreadFunc(notifyRec: TPNotifyRec) : cardinal; stdcall;
var handleArr : array [0..MAXIMUM_WAIT_OBJECTS] of cardinal;
    i1        : integer;
    result_   : integer;
begin
  result := 0;
  try
    try
      with notifyRec^ do begin
        for i1 := 0 to high(handles) do
          handleArr[i1] := handles[i1].Handle;
        i1 := Length(handles);
        repeat
          if @MsgWaitForMultipleObjectsEx = nil then begin
            if alertable_ then
                   result_ := integer(   WaitForMultipleObjectsEx(i1, @handleArr, waitAll_, INFINITE, true        ))
            else   result_ := integer(MsgWaitForMultipleObjects  (i1,  handleArr, waitAll_, INFINITE, msgs_       ));
          end else result_ := integer(MsgWaitForMultipleObjectsEx(i1,  handleArr,           INFINITE, msgs_, flags));
        until InFinalization or (result_ = integer(WAIT_FAILED)) or
              (SendMessageA(window_, msg_, result_, integer(handles)) = 0);
      end;
    finally Dispose(notifyRec) end;
  except sysUtils.ShowException(ExceptObject, ExceptAddr) end;
end;

function Notify(window        : cardinal;
                msg           : cardinal;
                const objects : array of IBasic;
                waitAll       : boolean          {= false};
                msgs          : TWaitForMessages {= []};
                alertable     : boolean          {= false}) : boolean;
var pnr     : TPNotifyRec;
    i1, i2  : integer;
    obj     : TIBasic;
    th, tid : cardinal;
begin
  result := false;
  if not MsgWaitExReady then begin
    MsgWaitExReady := true;
    MsgWaitForMultipleObjectsEx :=
      GetProcAddress(GetModuleHandle(user32), 'MsgWaitForMultipleObjectsEx');
  end;
  New(pnr);
  try
    with pnr^ do begin
      i2 := 0;
      SetLength(handles, Length(objects));
      for i1 := 0 to high(objects) do
        if (objects[i1] <> nil) and objects[i1].IsValid then begin
          obj := TIBasic(objects[i1].SelfAsTObject);
          if not (obj is TIHandle) then
            if obj is TIWaitableObj then begin
              obj := TIBasic(TIWaitableObj(obj).GetHandle.SelfAsTObject);
              if not obj.FValid then obj := nil;
            end else obj := nil;
          if obj <> nil then begin
            handles[i2] := IHandle(TIHandle(obj));
            inc(i2);
            if i2 = MAXIMUM_WAIT_OBJECTS - 1 then break;
          end;
        end;
      if i2 > 0 then begin
        SetLength(handles, i2);
        if (msgs <> []) and alertable and (@MsgWaitForMultipleObjectsEx = nil) then
          alertable := false;
        window_    := window;
        msg_       := msg;
        msgs_      := word(msgs);
        waitAll_   := waitAll;
        alertable_ := alertable;
        if waitAll then flags := MWMO_WAITALL else flags := 0;
        if alertable then flags := flags or MWMO_ALERTABLE;
        th := CreateThread(nil, 0, @NotifyThreadFunc, pnr, 0, tid);
        result := th <> 0;
        if result then CloseHandle(th);
      end;
    end;
  finally
    if not result then Dispose(pnr);
  end;
end;

function Notify(window        : cardinal;
                msg           : cardinal;
                const objects : ICustomBasicList;
                waitAll       : boolean          = false;
                msgs          : TWaitForMessages = [];
                alertable     : boolean          = false) : boolean;
begin
  if (objects <> nil) and objects.IsValid and (objects.ItemCount > 0) then begin
    with TICustomBasicList(objects.SelfAsTObject) do begin
      SetCapacity(FCount);
      {$ifdef ver120} result := NotifyArr(window, msg, FItems, waitAll, msgs, alertable);
      {$else}         result := Notify   (window, msg, FItems, waitAll, msgs, alertable); {$endif}
    end;
  end else result := false;
end;

// ***************************************************************

type
  // types for window procedure hooking
  TWndProcRec = record
    eventName1   : array [0..39] of AnsiChar;
    eventHandle1 : cardinal;
    eventName2   : array [0..39] of AnsiChar;
    eventHandle2 : cardinal;
    timeOut_     : cardinal;
    window_      : cardinal;
    msg_         : cardinal;
    wParam_      : integer;
    lParam_      : integer;
    result_      : integer;
    handled_     : boolean;
  end;
  TWndProcRecEx = record
    wndProcRec  : TWndProcRec;
    wndProcCode : byte;
  end;
  TPWndProcRecEx = ^TWndProcRecEx;

  // implements IWindow
  TIWindow = class (TIBasic, IWindow)
  public
    FHandle        : cardinal;
    FThreadID      : cardinal;
    FProcessID     : cardinal;
    FThread        : IThread;
    FProcess       : IProcess;
    FClassName     : AnsiString;
    FWndProcMem    : TPWndProcRecEx;
    FOldWndProc    : TWndProc;
    FWndProcEvent1 : IEvent;
    FWndProcEvent2 : IEvent;
    FWndProcThread : IThread;
//    FWndProcHook   : TWndHookProc;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        handle, tid, pid: cardinal);
    destructor Destroy; override;

    function IsStillValid : boolean;

    function GetHandle : cardinal;

    function  GetText : string;
    procedure SetText (value: string);

    function  GetVisible   : boolean;
    procedure SetVisible   (value    : boolean);
    procedure Hide;
    procedure Show         (activate : boolean = true);

    function  GetEnabled : boolean;
    procedure SetEnabled (value: boolean);

    function GetInTaskbar : boolean;

    function  GetOwnerWindow  : IWindow;
    function  GetParentWindow : IWindow;
    procedure SetParentWindow (const value: IWindow);

    function  GetOwnerThread  : IThread;
    function  GetOwnerProcess : IProcess;

    function  GetRect         : TRect;
    function  GetClientRect   : TRect;
    procedure SetRect         (value: TRect);
    procedure SetClientRect   (value: TRect);
    function  SetRectEx       (rect     : TRect;
                               copyBits : boolean = true;
                               redraw   : boolean = true;
                               activate : boolean = true) : boolean;
    function  SetClientRectEx (rect     : TRect;
                               copyBits : boolean = true;
                               redraw   : boolean = true;
                               activate : boolean = true) : boolean;

    function  GetMinimized : boolean;
    function  GetMaximized : boolean;
    function  GetRestored  : boolean;
    procedure SetMinimized (value: boolean);
    procedure SetMaximized (value: boolean);
    procedure SetRestored  (value: boolean);
    procedure Minimize     (activate: boolean = false);
    procedure Maximize     (activate: boolean = true );
    procedure Restore      (activate: boolean = true );

    function  GetMinimizePos : TPoint;
    function  GetMaximizePos : TPoint;
    function  GetNormalPos   : TRect;
    procedure SetMinimizePos (value: TPoint);
    procedure SetMaximizePos (value: TPoint);
    procedure SetNormalPos   (value: TRect );
    property  MinimizePos    : TPoint read GetMinimizePos write SetMinimizePos;
    property  MaximizePos    : TPoint read GetMaximizePos write SetMaximizePos;
    property  NormalPos      : TRect  read GetNormalPos   write SetNormalPos;

    function BringToTop (copyBits : boolean = true;
                         redraw   : boolean = true;
                         activate : boolean = true) : boolean;

    function BringToForeground (noBlink: boolean = true) : boolean;

    function  GetStayOnTop   : boolean;
    procedure SetStayOnTop   (value: boolean);
    function  SetStayOnTopEx (onTop    : boolean;
                              copyBits : boolean = true;
                              redraw   : boolean = true;
                              activate : boolean = true) : boolean;

    function  GetParam (index: integer) : integer;
    procedure SetParam (index: integer; value: integer);

    function  GetStyle   : integer;
    function  GetExStyle : integer;
    procedure SetStyle   (value: integer);
    procedure SetExStyle (value: integer);

    function ShowOwnedPopups (show: boolean = true) : boolean;

    function ChildWindow (pos       : TPoint;
                          skip      : TSSkipWindows = [] ) : IWindow; overload;
    function ChildWindow (className : AnsiString;
                          caption   : AnsiString    = '*') : IWindow; overload;

    function GetTopChildWindow : IWindow;

    function  GetProp    (name: AnsiString) : cardinal;
    procedure SetProp    (name: AnsiString; value: cardinal);
    function  DeleteProp (name: AnsiString) : boolean;

//    function GetProps : IWindowProps;

    function IsUnicode : boolean;

    function SelfAsTWinControl : TObject;

    function  GetWndProc : TWndProc;
    procedure SetWndProc (value: TWndProc);

//    function Hook   (hookProc : TWndHookProc  ) : boolean; overload;
//    function Hook   (window   : cardinal;
//                     viaSend  : boolean = true) : boolean; overload;
//    function Unhook : boolean;

    function PostMessage (msg                  : cardinal;
                          wParam               : integer  = 0;
                          lParam               : integer  = 0       ) : boolean;
    function SendMessage (msg                  : cardinal;
                          wParam               : integer  = 0;
                          lParam               : integer  = 0;
                          timeOut              : cardinal = INFINITE;
                          abortIfHung          : boolean  = false;
                          blockThread          : boolean  = false;
                          noTimeOutWhenNotHung : boolean  = false   ) : integer; overload;
    function SendMessage (msg                  : cardinal;
                          wParam               : integer;
                          lParam               : integer;
                          callbackProc         : TSendAsyncProc;
                          callbackInfo         : cardinal = 0       ) : boolean; overload;

    function Close : boolean;

    function Destroy_ : boolean;
    function IWindow.Destroy = Destroy_;

    function GetClassName : AnsiString;

    function  GetBackgroundBrush : cardinal;
    procedure SetBackgroundBrush (value: cardinal);

    function  GetCursor : cardinal;
    procedure SetCursor (value: cardinal);

    function  GetIcon      : cardinal;
    function  GetSmallIcon : cardinal;
    procedure SetIcon      (value: cardinal);
    procedure SetSmallIcon (value: cardinal);

    function GetMaxInterface : IBasic; override;
  end;

var WindowsArr : array of TIWindow = nil;
    WSection   : ICriticalSection  = nil;

function AddWindow(handle: cardinal) : TIWindow;
var i1,  i2  : integer;
    tid, pid : cardinal;
begin
  result := nil;
  if WSection = nil then WSection := NewCriticalSection;
  WSection.Enter;
  try
    if IsWindow(handle) then begin
      tid := GetWindowThreadProcessID(handle, @pid);
      i2 := Length(WindowsArr);
      for i1 := 0 to i2 - 1 do
        with WindowsArr[i1] do
          if (FHandle = handle) and (FProcessID = pid) and (FThreadID = tid) then begin
            result := WindowsArr[i1];
            break;
          end;
      if result = nil then begin
        result := TIWindow.Create(true, 0, '', handle, tid, pid);
        SetLength(WindowsArr, i2 + 1);
        WindowsArr[i2] := result;
      end;
    end else result := TIWindow.Create(false, ERROR_INVALID_PARAMETER, '', 0, 0, 0);
  finally WSection.Leave end;
end;

procedure DelWindow(window: TIWindow);
var i1, i2 : integer;
begin
  if WSection = nil then WSection := NewCriticalSection;
  WSection.Enter;
  try
    i2 := High(WindowsArr);
    for i1 := 0 to i2 do
      if WindowsArr[i1] = window then begin
        WindowsArr[i1] := WindowsArr[i2];
        SetLength(WindowsArr, i2);
        break;
      end;
  finally WSection.Leave end;
end;

constructor TIWindow.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                            handle, tid, pid: cardinal);
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FHandle    := handle;
    FThreadID  := tid;
    FProcessID := pid;
  end;
end;

destructor TIWindow.Destroy;
begin
  DelWindow(self);
  inherited;
end;

function TIWindow.IsStillValid : boolean;
var tid, pid : cardinal;
begin
  if FValid then begin
    FValid := IsWindow(FHandle);
    if FValid then begin
      tid := GetWindowThreadProcessID(FHandle, @pid);
      FValid := (FThreadID = tid) and (FProcessID = pid);
    end;
    if FValid then FSuccess := true
    else           SetLastError(CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)));
  end;
  result := FValid;
end;

function TIWindow.GetHandle : cardinal;
begin
  result := FHandle;
end;

function TIWindow.GetText : string;
var i1 : integer;
begin
  if CheckValid and
     (SendMessageTimeout(FHandle, WM_GETTEXTLENGTH, 0, 0, SMTO_ABORTIFHUNG, 2000, dword(i1)) <> 0) then begin
    SetLength(result, i1 + 1);
    if SendMessageTimeout(FHandle, WM_GETTEXT, i1 + 1, integer(result), SMTO_ABORTIFHUNG, 2000, dword(i1)) <> 0 then
         SetLength(result, i1)
    else result := '';
  end else
    result := '';
end;

procedure TIWindow.SetText(value: string);
var c1 : dword;
begin
  if CheckValid then
    SendMessageTimeout(FHandle, WM_SETTEXT, 0, integer(value), SMTO_ABORTIFHUNG, 2000, c1);
end;

function TIWindow.GetVisible : boolean;
begin
  result := CheckValid and IsWindowVisible(FHandle);
end;

procedure TIWindow.SetVisible(value: boolean);
begin
  if CheckValid then
    if value then ShowWindow(FHandle, SW_SHOW)
    else          ShowWindow(FHandle, SW_HIDE);
end;

procedure TIWindow.Hide;
begin
  if CheckValid then
    ShowWindow(FHandle, SW_HIDE);
end;

procedure TIWindow.Show(activate: boolean = true);
begin
  if CheckValid then
    if activate then ShowWindow(FHandle, SW_SHOW  )
    else             ShowWindow(FHandle, SW_SHOWNA);
end;

function TIWindow.GetEnabled : boolean;
begin
  result := CheckValid and IsWindowEnabled(FHandle);
end;

procedure TIWindow.SetEnabled(value: boolean);
begin
  if CheckValid then
    if not EnableWindow(FHandle, value) then
      SetLastError(GetLastError);
end;

function TIWindow.GetInTaskbar : boolean;
begin
  result := CheckValid and IsWindowVisible(FHandle) and
            (GetWindow(FHandle, GW_OWNER) = 0) and (GetParent(FHandle) = 0) and
            (GetWindowLong(FHandle, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = 0);
end;

function TIWindow.GetOwnerWindow : IWindow;
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := GetWindow(FHandle, GW_OWNER);
    if c1 <> 0 then result := AddWindow(c1)
    else            result := TIWindow.Create(false, GetLastError, '', 0, 0, 0);
  end else result := TIWindow.Create(false, FLastErrorNo, FLastErrorStr, 0, 0, 0);
end;

function TIWindow.GetParentWindow : IWindow;
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := GetParent(FHandle);
    if c1 <> 0 then result := AddWindow(c1)
    else            result := TIWindow.Create(false, GetLastError, '', 0, 0, 0);
  end else result := TIWindow.Create(false, FLastErrorNo, FLastErrorStr, 0, 0, 0);
end;

procedure TIWindow.SetParentWindow(const value: IWindow);
begin
  if CheckValid then
    if value <> nil then begin
      if value.IsStillValid then begin
        if SetParent(FHandle, value.Handle) = 0 then
          SetLastError(GetLastError);
      end else SetLastError(ERROR_INVALID_PARAMETER);
    end else
      if SetParent(FHandle, 0) = 0 then
        SetLastError(GetLastError);
end;

function TIWindow.GetOwnerThread : IThread;
begin
  if FThread = nil then
    if CheckValid then
         FThread := TIThread(AddKernelObj(otThread, FThreadID))
    else FThread := TIThread.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), otUnknown, 0);
  result := FThread;
end;

function TIWindow.GetOwnerProcess : IProcess;
begin
  if FProcess = nil then
    if CheckValid then
         FProcess := TIProcess(AddKernelObj(otProcess, FProcessID))
    else FProcess := TIProcess.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), otUnknown, 0);
  result := FProcess;
end;

function TIWindow.GetRect : TRect;
begin
  if CheckValid then
    if not GetWindowRect(FHandle, result) then
      SetLastError(GetLastError);
  if not FSuccess then
    ZeroMemory(@result, sizeOf(TRect));
end;

function TIWindow.GetClientRect : TRect;
begin
  if CheckValid then
    if not windows.GetClientRect(FHandle, result) then
      SetLastError(GetLastError);
  if not FSuccess then
    ZeroMemory(@result, sizeOf(TRect));
end;

procedure TIWindow.SetRect(value: TRect);
begin
  if CheckValid and
     (not SetWindowPos(FHandle, 0, value.Left, value.Top, value.Right - value.Left,
                       value.Bottom - value.Top, SWP_NOZORDER)) then
    SetLastError(GetLastError);
end;

procedure TIWindow.SetClientRect(value: TRect);
begin
  if CheckValid then begin
    AdjustWindowRectEx(value, cardinal(GetWindowLong(FHandle, GWL_STYLE)),
                       GetMenu(FHandle) <> 0,
                       cardinal(GetWindowLong(FHandle, GWL_EXSTYLE)));
    if not SetWindowPos(FHandle, 0, value.Left, value.Top, value.Right - value.Left,
                        value.Bottom - value.Top, SWP_NOZORDER) then
      SetLastError(GetLastError);
  end;
end;

function TIWindow.SetRectEx(rect     : TRect;
                            copyBits : boolean = true;
                            redraw   : boolean = true;
                            activate : boolean = true) : boolean;
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := SWP_NOZORDER;
    if not copyBits then c1 := c1 or SWP_NOCOPYBITS;
    if not redraw   then c1 := c1 or SWP_NOREDRAW;
    if not activate then c1 := c1 or SWP_NOACTIVATE;
    result := SetWindowPos(FHandle, 0, rect.Left, rect.Top,
                           rect.Right - rect.Left, rect.Bottom - rect.Top, c1);
    if not result then SetLastError(GetLastError);
  end else result := false;
end;

function TIWindow.SetClientRectEx(rect     : TRect;
                                  copyBits : boolean = true;
                                  redraw   : boolean = true;
                                  activate : boolean = true) : boolean;
var c1 : cardinal;
begin
  if CheckValid then begin
    AdjustWindowRectEx(rect, cardinal(GetWindowLong(FHandle, GWL_STYLE)),
                       GetMenu(FHandle) <> 0,
                       cardinal(GetWindowLong(FHandle, GWL_EXSTYLE)));
    c1 := SWP_NOZORDER;
    if not copyBits then c1 := c1 or SWP_NOCOPYBITS;
    if not redraw   then c1 := c1 or SWP_NOREDRAW;
    if not activate then c1 := c1 or SWP_NOACTIVATE;
    result := SetWindowPos(FHandle, 0, rect.Left, rect.Top,
                           rect.Right - rect.Left, rect.Bottom - rect.Top, c1);
    if not result then SetLastError(GetLastError);
  end else result := false;
end;

function TIWindow.GetMinimized : boolean;
begin
  result := CheckValid and IsIconic(FHandle);
end;

function TIWindow.GetMaximized : boolean;
begin
  result := CheckValid and IsZoomed(FHandle);
end;

function TIWindow.GetRestored : boolean;
begin
  result := IsStillValid and (not (IsIconic(FHandle) or IsZoomed(FHandle)));
end;

procedure TIWindow.SetMinimized(value: boolean);
begin
  if CheckValid then
    if value then ShowWindow(FHandle, SW_SHOWMINNOACTIVE)
    else          ShowWindow(FHandle, SW_RESTORE        );
end;

procedure TIWindow.SetMaximized(value: boolean);
begin
  if CheckValid then
    if value then ShowWindow(FHandle, SW_SHOWMAXIMIZED)
    else          ShowWindow(FHandle, SW_RESTORE      );
end;

procedure TIWindow.SetRestored(value: boolean);
begin
  if CheckValid then
    if value then ShowWindow(FHandle, SW_RESTORE      )
    else          ShowWindow(FHandle, SW_SHOWMAXIMIZED);
end;

procedure TIWindow.Minimize(activate: boolean = false);
begin
  if CheckValid then
    if activate then ShowWindow(FHandle, SW_SHOWMINIMIZED  )
    else             ShowWindow(FHandle, SW_SHOWMINNOACTIVE);
end;

procedure TIWindow.Maximize(activate: boolean = true);
begin
  if CheckValid then
    if activate then ShowWindow(FHandle, SW_SHOWMAXIMIZED)
    else             ShowWindow(FHandle, SW_MAXIMIZE     );
end;

procedure TIWindow.Restore(activate: boolean = true);
begin
  if CheckValid then
    if activate then ShowWindow(FHandle, SW_RESTORE       )
    else             ShowWindow(FHandle, SW_SHOWNOACTIVATE);
end;

function TIWindow.GetMinimizePos : TPoint;
var wp : TWindowPlacement;
begin
  if CheckValid then begin
    wp.length := sizeOf(TWindowPlacement);
    if not GetWindowPlacement(FHandle, @wp) then
      SetLastError(GetLastError);
  end;
  if not FSuccess then ZeroMemory(@result, sizeOf(TPoint))
  else                 result := wp.ptMinPosition;
end;

function TIWindow.GetMaximizePos : TPoint;
var wp : TWindowPlacement;
begin
  if CheckValid then begin
    wp.length := sizeOf(TWindowPlacement);
    if not GetWindowPlacement(FHandle, @wp) then
      SetLastError(GetLastError);
  end;
  if not FSuccess then ZeroMemory(@result, sizeOf(TPoint))
  else                 result := wp.ptMaxPosition;
end;

function TIWindow.GetNormalPos : TRect;
var wp : TWindowPlacement;
begin
  if CheckValid then begin
    wp.length := sizeOf(TWindowPlacement);
    if not GetWindowPlacement(FHandle, @wp) then
      SetLastError(GetLastError);
  end;
  if not FSuccess then ZeroMemory(@result, sizeOf(TRect))
  else                 result := wp.rcNormalPosition;
end;

procedure TIWindow.SetMinimizePos(value: TPoint);
var wp : TWindowPlacement;
begin
  if CheckValid then begin
    wp.length := sizeOf(TWindowPlacement);
    if GetWindowPlacement(FHandle, @wp) then begin
      wp.ptMinPosition := value;
      if not SetWindowPlacement(FHandle, @wp) then
        SetLastError(GetLastError);
    end else SetLastError(GetLastError);
  end;  
end;

procedure TIWindow.SetMaximizePos(value: TPoint);
var wp : TWindowPlacement;
begin
  if CheckValid then begin
    wp.length := sizeOf(TWindowPlacement);
    if GetWindowPlacement(FHandle, @wp) then begin
      wp.ptMaxPosition := value;
      if not SetWindowPlacement(FHandle, @wp) then
        SetLastError(GetLastError);
    end else SetLastError(GetLastError);
  end;  
end;

procedure TIWindow.SetNormalPos(value: TRect);
var wp : TWindowPlacement;
begin
  if CheckValid then begin
    wp.length := sizeOf(TWindowPlacement);
    if GetWindowPlacement(FHandle, @wp) then begin
      wp.rcNormalPosition := value;
      if not SetWindowPlacement(FHandle, @wp) then
        SetLastError(GetLastError);
    end else SetLastError(GetLastError);
  end;
end;

function TIWindow.BringToTop(copyBits : boolean = true;
                             redraw   : boolean = true;
                             activate : boolean = true) : boolean;
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := SWP_NOMOVE or SWP_NOSIZE;
    if not copyBits then c1 := c1 or SWP_NOCOPYBITS;
    if not redraw   then c1 := c1 or SWP_NOREDRAW;
    if not activate then c1 := c1 or SWP_NOACTIVATE;
    result := SetWindowPos(FHandle, HWND_TOP, 0, 0, 0, 0, c1);
    if not result then
      SetLastError(GetLastError);
  end else result := false;
end;

function TIWindow.BringToForeground(noBlink: boolean = true) : boolean;
const SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
      SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var   timeout : cardinal;
      tid     : dword;
begin
  if CheckValid then begin
    if noBlink and ((OS.win9xEnum >= osWin98) or (OS.winNtEnum >= osWin2k)) then begin
      if OS.win9x then begin
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, nil,      0);
        result := SetForegroundWindow(FHandle);
        if not result then
          SetLastError(GetLastError);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, pointer(timeout), 0);
      end else begin
        tid := GetWindowThreadProcessID(GetForegroundWindow, nil);
        if tid <> 0 then
          AttachThreadInput(GetCurrentThreadId, tid, true);
        result := SetForegroundWindow(FHandle);
        if tid <> 0 then
          AttachThreadInput(GetCurrentThreadId, tid, false);
      end;
    end else begin
      result := SetForegroundWindow(FHandle);
      if not result then
        SetLastError(GetLastError);
    end;
  end else result := false;
end;

function TIWindow.GetStayOnTop : boolean;
begin
  result := CheckValid and (GetWindowLong(FHandle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0);
end;

procedure TIWindow.SetStayOnTop(value: boolean);
var c1 : cardinal;
begin
  if CheckValid then begin
    if value then c1 := HWND_TOPMOST
    else          c1 := HWND_NOTOPMOST;
    if not SetWindowPos(FHandle, c1, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE) then
      SetLastError(GetLastError);
  end;
end;

function TIWindow.SetStayOnTopEx(onTop    : boolean;
                                 copyBits : boolean = true;
                                 redraw   : boolean = true;
                                 activate : boolean = true) : boolean;
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := SWP_NOMOVE or SWP_NOSIZE;
    if not copyBits then c1 := c1 or SWP_NOCOPYBITS;
    if not redraw   then c1 := c1 or SWP_NOREDRAW;
    if not activate then c1 := c1 or SWP_NOACTIVATE;
    if onTop then
         result := SetWindowPos(FHandle, HWND_TOPMOST,   0, 0, 0, 0, c1)
    else result := SetWindowPos(FHandle, HWND_NOTOPMOST, 0, 0, 0, 0, c1);
    if not result then
      SetLastError(GetLastError);
  end else result := false;
end;

function TIWindow.GetParam(index: integer) : integer;
begin
  if CheckValid then begin
    windows.SetLastError(0);
    result := GetWindowLong(FHandle, index);
    if (result = 0) and (GetLastError <> 0) then
      SetLastError(GetLastError);
  end else result := 0;
end;

procedure TIWindow.SetParam(index: integer; value: integer);
begin
  if IsStillValid or CheckValid then begin
    if FProcessID = GetCurrentProcessID then begin
      windows.SetLastError(0);
      if (SetWindowLong(FHandle, index, value) = 0) and (GetLastError <> 0) then
        SetLastError(GetLastError);
    end else
      if not TIProcess(GetOwnerProcess.SelfAsTObject).SetWindowLong(self.FHandle, index, value) then
        SetLastError(FProcess.LastErrorNo, FProcess.LastErrorStr);
  end;
end;

function TIWindow.GetStyle : integer;
begin
  result := GetParam(GWL_STYLE);
end;

function TIWindow.GetExStyle : integer;
begin
  result := GetParam(GWL_EXSTYLE);
end;

procedure TIWindow.SetStyle(value: integer);
begin
  SetParam(GWL_STYLE, value);
end;

procedure TIWindow.SetExStyle(value: integer);
begin
  SetParam(GWL_EXSTYLE, value);
end;

function TIWindow.ShowOwnedPopups(show: boolean = true) : boolean;
begin
  if CheckValid then begin
    result := windows.ShowOwnedPopups(FHandle, show);
    if not result then
      SetLastError(GetLastError);
  end else result := false;
end;

function TIWindow.ChildWindow(pos  : TPoint;
                              skip : TSSkipWindows = []) : IWindow;
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := 0;
    if skip <> [] then begin
      if swInvisible   in skip then c1 := c1 or CWP_SKIPINVISIBLE;
      if swDisabled    in skip then c1 := c1 or CWP_SKIPDISABLED;
      if swTransparent in skip then c1 := c1 or CWP_SKIPTRANSPARENT;
    end;
    c1 := ChildWindowFromPointEx(FHandle, pos, c1);
    if c1 <> 0 then result := AddWindow(c1)
    else            result := TIWindow.Create(false, ERROR_FILE_NOT_FOUND, '', 0, 0, 0);
  end else result := TIWindow.Create(false, FLastErrorNo, FLastErrorStr, 0, 0, 0);
end;

function TIWindow.ChildWindow(className : AnsiString;
                              caption   : AnsiString = '*') : IWindow;
var pc1, pc2 : PAnsiChar;
    c1       : cardinal;
begin
  if CheckValid then begin
    if className <> '*' then pc1 := PAnsiChar(className) else pc1 := nil;
    if caption   <> '*' then pc2 := PAnsiChar(caption  ) else pc2 := nil;
    c1 := FindWindowExA(FHandle, 0, pc1, pc2);
    if c1 <> 0 then result := AddWindow(c1)
    else            result := TIWindow.Create(false, GetLastError, '', 0, 0, 0);
  end else result := TIWindow.Create(false, FLastErrorNo, FLastErrorStr, 0, 0, 0);
end;

function TIWindow.GetTopChildWindow : IWindow;
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := GetTopWindow(FHandle);
    if c1 <> 0 then result := AddWindow(c1)
    else            result := TIWindow.Create(false, GetLastError, '', 0, 0, 0);
  end else result := TIWindow.Create(false, FLastErrorNo, FLastErrorStr, 0, 0, 0);
end;

function TIWindow.GetProp(name: AnsiString) : cardinal;
begin
  if CheckValid then begin
    result := windows.GetPropA(FHandle, PAnsiChar(name));
    if result = 0 then
      SetLastError(ERROR_FILE_NOT_FOUND);
  end else result := 0;
end;

procedure TIWindow.SetProp(name: AnsiString; value: cardinal);
begin
  if CheckValid then begin
    if not windows.SetPropA(FHandle, PAnsiChar(name), value) then
      SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
  end;
end;

function TIWindow.DeleteProp(name: AnsiString) : boolean;
begin
  if CheckValid then begin
    result := RemovePropA(FHandle, PAnsiChar(name)) = 0;
    if not result then
      SetLastError(ERROR_FILE_NOT_FOUND);
  end else result := false;
end;

//    function TIWindow.GetProps : IWindowProps;

function TIWindow.IsUnicode : boolean;
begin
  result := CheckValid and IsWindowUnicode(FHandle);
end;

function TIWindow.SelfAsTWinControl : TObject;
begin
  if CheckValid then begin
    result := pointer(windows.GetPropA(FHandle, PAnsiChar('Delphi' + AnsiString(IntToHex(FProcessID, 8)))));
    if result = nil then
      SetLastError(ERROR_FILE_NOT_FOUND);
  end else result := nil;
end;

function TIWindow.GetWndProc : TWndProc;
begin
  result := pointer(GetParam(GWL_WNDPROC));
end;

procedure TIWindow.SetWndProc(value: TWndProc);
begin
  SetParam(GWL_WNDPROC, integer(@value));
end;

{function InjectedWndProc1(window, msg: cardinal; wParam, lParam: integer) : integer; stdcall;
begin
  with TWndProcRec(pointer($11111111)^) do begin
    if eventHandle1 = 0 then begin
      eventHandle1 := windows.OpenEvent(EVENT_ALL_ACCESS, false, eventName1);
      eventHandle2 := windows.OpenEvent(EVENT_ALL_ACCESS, false, eventName2);
      if eventHandle1 = 0 then eventHandle1 := maxCard;
    end;
    if eventHandle1 <> maxCard then begin
      if (msg = WM_NULL) and (wParam = 777) and (lParam = 777) then begin
        CloseHandle(eventHandle1);
        CloseHandle(eventHandle2);
        eventHandle1 := maxCard;
        result := 777;
        exit;
      end;
      window_  := window;
      msg_     := msg;
      wParam_  := wParam;
      lParam_  := lParam;
      handled_ := false;
      SetEvent(eventHandle1);
      if WaitForSingleObject(eventHandle2, timeout_) = WAIT_OBJECT_0 then
        if handled_ then begin
          result := result_;
          exit;
        end else begin
          window := window_;
          msg    := msg_;
          wParam := wParam_;
          lParam := lParam_;
        end;
    end;
  end;
  result := CallWindowProc(pointer($22222222), window, msg, wParam, lParam);
end;

procedure InjectedWndProc1_End; begin end;

function WndProcThreadFunc(param: pointer) : cardinal; stdcall;
var self : TIWindow absolute param;
    wpr  : TWndProcRec;
begin
  result := 0;
  with self do
    while FWndProcEvent1.WaitFor(INFINITE, false) and (FWndProcThread <> nil) do
      if ParentProcess.ReadMemory(FWndProcMem^, wpr, sizeOf(TWndProcRec)) then begin
        wpr.result_ := FWndProcHook(wpr.window_, wpr.msg_, wpr.wParam_, wpr.lParam_, wpr.handled_);
        ParentProcess.WriteMemory(wpr, FWndProcMem^, sizeOf(TWndProcRec));
        FWndProcEvent2.UnLock;
      end;
end; }

{function TIWindow.Hook(hookProc : TWndHookProc) : boolean;
begin
  result := false;
end;

function TIWindow.Hook(window   : cardinal;
                       viaSend  : boolean = true) : boolean;
begin
  result := false;
end;

function TIWindow.Unhook : boolean;
begin
  result := false;
end; }

function TIWindow.PostMessage(msg    : cardinal;
                              wParam : integer = 0;
                              lParam : integer = 0) : boolean;
begin
  if CheckValid then begin
    result := windows.PostMessageA(FHandle, msg, wParam, lParam);
    if not result then
      SetLastError(GetLastError);
  end else result := false;
end;

function TIWindow.SendMessage(msg                  : cardinal;
                              wParam               : integer  = 0;
                              lParam               : integer  = 0;
                              timeOut              : cardinal = INFINITE;
                              abortIfHung          : boolean  = false;
                              blockThread          : boolean  = false;
                              noTimeOutWhenNotHung : boolean  = false   ) : integer;
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := 0;
    if abortIfHung          then c1 := c1 or SMTO_ABORTIFHUNG;
    if blockThread          then c1 := c1 or SMTO_BLOCK;
    if noTimeOutWhenNotHung then c1 := c1 or SMTO_NOTIMEOUTIFNOTHUNG;
    if timeOut > cardinal(maxInt) then timeOut := maxInt;
    windows.SetLastError(0);
    if SendMessageTimeoutA(FHandle, msg, wParam, lParam, c1, timeOut, cardinal(result)) = 0 then begin
      result := 0;
      if GetLastError = 0 then SetLastError(WAIT_TIMEOUT, UnicodeString(AnsiString(CErrorStr_WaitTimeout)))
      else                     SetLastError(GetLastError);
    end;
  end else result := 0;
end;

function TIWindow.SendMessage(msg          : cardinal;
                              wParam       : integer;
                              lParam       : integer;
                              callbackProc : TSendAsyncProc;
                              callbackInfo : cardinal = 0  ) : boolean;
begin
  if CheckValid then begin
    result := SendMessageCallbackA(FHandle, msg, wParam, lParam, @callbackProc, callbackInfo);
    if not result then
      SetLastError(GetLastError);
  end else result := false;
end;

function TIWindow.Close : boolean;
begin
  if CheckValid then begin
    result := windows.PostMessageA(FHandle, WM_CLOSE, 0, 0);
    if not result then
      SetLastError(GetLastError);
  end else result := false;
end;

function TIWindow.Destroy_ : boolean;
begin
  if CheckValid then begin
    result := DestroyWindow(FHandle);
    if result then IsStillValid
    else           SetLastError(GetLastError);
  end else result := false;
end;

function TIWindow.GetClassName : AnsiString;
begin
  if (FClassName = '') and CheckValid then begin
    SetLength(FClassName, MAX_PATH + 1);
    SetLength(FClassName, windows.GetClassNameA(FHandle, PAnsiChar(FClassName), MAX_PATH));
  end;
  result := FClassName;
end;

function TIWindow.GetBackgroundBrush : cardinal;
begin
  if CheckValid then begin
    windows.SetLastError(0);
    result := GetClassLong(FHandle, GCL_HBRBACKGROUND);
    if (result = 0) and (GetLastError <> 0) then
      SetLastError(GetLastError);
  end else result := 0;
end;

procedure TIWindow.SetBackgroundBrush(value: cardinal);
begin
  if CheckValid then begin
    windows.SetLastError(0);
    if (SetClassLong(FHandle, GCL_HBRBACKGROUND, integer(value)) = 0) and (GetLastError <> 0) then
      SetLastError(GetLastError);
  end;
end;

function TIWindow.GetCursor : cardinal;
begin
  if CheckValid then begin
    windows.SetLastError(0);
    result := GetClassLong(FHandle, GCL_HCURSOR);
    if (result = 0) and (GetLastError <> 0) then
      SetLastError(GetLastError);
  end else result := 0;
end;

procedure TIWindow.SetCursor(value: cardinal);
begin
  if CheckValid then begin
    windows.SetLastError(0);
    if (SetClassLong(FHandle, GCL_HCURSOR, integer(value)) = 0) and (GetLastError <> 0) then
      SetLastError(GetLastError);
  end;
end;

function TIWindow.GetIcon : cardinal;
begin
  if CheckValid then begin
    windows.SetLastError(0);
    result := GetClassLong(FHandle, GCL_HICON);
    if (result = 0) and (GetLastError <> 0) then
      SetLastError(GetLastError);
  end else result := 0;
end;

function TIWindow.GetSmallIcon : cardinal;
begin
  if CheckValid then begin
    windows.SetLastError(0);
    result := GetClassLong(FHandle, GCL_HICONSM);
    if (result = 0) and (GetLastError <> 0) then
      SetLastError(GetLastError);
  end else result := 0;
end;

procedure TIWindow.SetIcon(value: cardinal);
begin
  if CheckValid then begin
    windows.SetLastError(0);
    if (SetClassLong(FHandle, GCL_HICON, integer(value)) = 0) and (GetLastError <> 0) then
      SetLastError(GetLastError);
  end;
end;

procedure TIWindow.SetSmallIcon(value: cardinal);
begin
  if CheckValid then begin
    windows.SetLastError(0);
    if (SetClassLong(FHandle, GCL_HICONSM, integer(value)) = 0) and (GetLastError <> 0) then
      SetLastError(GetLastError);
  end;
end;

function TIWindow.GetMaxInterface : IBasic;
begin
  result := IWindow(self);
end;

function Window(handle: cardinal) : IWindow;
begin
  result := AddWindow(handle);
end;

function Window(className : AnsiString;
                caption   : AnsiString = '*') : IWindow;
var pc1, pc2 : PAnsiChar;
    c1       : cardinal;
begin
  if className <> '*' then pc1 := PAnsiChar(className) else pc1 := nil;
  if caption   <> '*' then pc2 := PAnsiChar(caption  ) else pc2 := nil;
  c1 := windows.FindWindowA(pc1, pc2);
  if c1 <> 0 then result := AddWindow(c1)
  else            result := TIWindow.Create(false, GetLastError, '', 0, 0, 0);
end;

function Window(pos: TPoint) : IWindow;
var c1 : cardinal;
begin
  c1 := WindowFromPoint(pos);
  if c1 <> 0 then result := AddWindow(c1)
  else            result := TIWindow.Create(false, ERROR_FILE_NOT_FOUND, '', 0, 0, 0);
end;

function ForegroundWindow : IWindow;
var c1 : cardinal;
begin
  c1 := windows.GetForegroundWindow;
  if c1 <> 0 then result := AddWindow(c1)
  else            result := TIWindow.Create(false, ERROR_FILE_NOT_FOUND, '', 0, 0, 0);
end;

function FocusedWindow : IWindow;
var c1 : cardinal;
begin
  c1 := GetFocus;
  if c1 <> 0 then result := AddWindow(c1)
  else            result := TIWindow.Create(false, ERROR_FILE_NOT_FOUND, '', 0, 0, 0);
end;

function DesktopWindow : IWindow;
var c1 : cardinal;
begin
  c1 := windows.GetDesktopWindow;
  if c1 <> 0 then result := AddWindow(c1)
  else            result := TIWindow.Create(false, ERROR_FILE_NOT_FOUND, '', 0, 0, 0);
end;

function DesktopListView : IWindow;
var c1 : cardinal;
begin
  c1 := FindWindowEx(FindWindowEx(windows.FindWindow('Progman', 'Program Manager'), 0, 'SHELLDLL_DefView', nil), 0, 'SysListView32', nil);
  if c1 <> 0 then result := AddWindow(c1)
  else            result := TIWindow.Create(false, ERROR_FILE_NOT_FOUND, '', 0, 0, 0);
end;

function Taskbar : IWindow;
var c1 : cardinal;
begin
  c1 := windows.FindWindow('Shell_TrayWnd', '');
  if c1 <> 0 then result := AddWindow(c1)
  else            result := TIWindow.Create(false, GetLastError, '', 0, 0, 0);
end;

function StartButton : IWindow;
var c1 : cardinal;
begin
  c1 := FindWindowEx(windows.FindWindow('Shell_TrayWnd', ''), 0, 'Button', nil);
  if c1 <> 0 then result := AddWindow(c1)
  else            result := TIWindow.Create(false, GetLastError, '', 0, 0, 0);
end;

function TaskButtonWindow : IWindow;
var c1, c2 : cardinal;
begin
  c1 := windows.FindWindow('Shell_TrayWnd', '');
  if c1 <> 0 then begin
    c2 := FindWindowEx(c1, 0, 'ReBarWindow32', '');
    if c2 <> 0 then c1 := FindWindowEx(c2, 0, 'MSTaskSwWClass', '')
    else            c1 := FindWindowEx(c1, 0, 'MSTaskSwWClass', '');
  end;
  if c1 <> 0 then result := AddWindow(c1)
  else            result := TIWindow.Create(false, GetLastError, '', 0, 0, 0);
end;

function TrayWindow : IWindow;
var c1 : cardinal;
begin
  c1 := FindWindowEx(windows.FindWindow('Shell_TrayWnd', ''), 0, 'TrayNotifyWnd', '');
  if c1 <> 0 then result := AddWindow(c1)
  else            result := TIWindow.Create(false, GetLastError, '', 0, 0, 0);
end;

function ClockWindow : IWindow;
var c1 : cardinal;
begin
  c1 := FindWindowEx(FindWindowEx(windows.FindWindow('Shell_TrayWnd', ''), 0, 'TrayNotifyWnd', ''),
                     0, 'TrayClockWClass', nil);
  if c1 <> 0 then result := AddWindow(c1)
  else            result := TIWindow.Create(false, GetLastError, '', 0, 0, 0);
end;

// ***************************************************************

type
  // implements IWindows
  TIWindows = class (TICustomBasicList, IWindows)
  public
    FThread    : IThread;
    FProcess   : IProcess;
    FInTaskbar : boolean;
    FList      : boolean;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        const thread: IThread; const process: IProcess; inTaskbar: boolean;
                        const objects: array of IBasic);

    function GetOwnerThread  : IThread;
    function GetOwnerProcess : IProcess;

    function GetInTaskbar : boolean;

    procedure Hide;
    procedure Show (activate: boolean = true);

    procedure SetEnabled (value: boolean);

    procedure Minimize (activate: boolean = false);
    procedure Maximize (activate: boolean = true );
    procedure Restore  (activate: boolean = true );

    function SetStayOnTop (onTop    : boolean;
                           copyBits : boolean = true;
                           redraw   : boolean = true;
                           activate : boolean = true) : boolean;

    function ShowOwnedPopups (show: boolean = true) : boolean;

    function PostMessage (msg                  : cardinal;
                          wParam               : integer  = 0;
                          lParam               : integer  = 0       ) : boolean;

    function Close : boolean;

    function Destroy_ : boolean;
    function IWindows.Destroy = Destroy_;

    function GetItem (index: integer) : IWindow;

    function RefreshItems : boolean;

    function GetMaxInterface : IBasic; override;
  end;

constructor TIWindows.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                             const thread: IThread; const process: IProcess; inTaskbar: boolean;
                             const objects: array of IBasic);
var i1 : integer;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FThread    := thread;
    FProcess   := process;
    FInTaskbar := inTaskbar;
    FList := Length(objects) > 0;
    if FList then
      for i1 := 0 to high(objects) do
        if objects[i1] <> nil then
          AddItem(objects[i1]);
    if not FList then RefreshItems;
  end;
end;

function TIWindows.GetOwnerThread : IThread;
begin
  result := FThread;
end;

function TIWindows.GetOwnerProcess : IProcess;
begin
  result := FProcess;
end;

function TIWindows.GetInTaskbar : boolean;
begin
  result := FInTaskbar;
end;

procedure TIWindows.Hide;
var i1 : integer;
begin
  for i1 := 0 to FCount - 1 do
    IWindow(FItems[i1]).Hide;
end;

procedure TIWindows.Show(activate: boolean = true);
var i1 : integer;
begin
  for i1 := 0 to FCount - 1 do
    IWindow(FItems[i1]).Show(activate);
end;

procedure TIWindows.SetEnabled(value: boolean);
var i1 : integer;
begin
  for i1 := 0 to FCount - 1 do
    IWindow(FItems[i1]).SetEnabled(value);
end;

procedure TIWindows.Minimize(activate: boolean = false);
var i1 : integer;
begin
  for i1 := 0 to FCount - 1 do
    IWindow(FItems[i1]).Minimize(activate);
end;

procedure TIWindows.Maximize(activate: boolean = true);
var i1 : integer;
begin
  for i1 := 0 to FCount - 1 do
    IWindow(FItems[i1]).Maximize(activate);
end;

procedure TIWindows.Restore(activate: boolean = true);
var i1 : integer;
begin
  for i1 := 0 to FCount - 1 do
    IWindow(FItems[i1]).Restore(activate);
end;

function TIWindows.SetStayOnTop(onTop    : boolean;
                                copyBits : boolean = true;
                                redraw   : boolean = true;
                                activate : boolean = true) : boolean;
var i1 : integer;
begin
  result := false;
  for i1 := 0 to FCount - 1 do
    result := IWindow(FItems[i1]).SetStayOnTopEx(onTop, copyBits, redraw, activate) or result;
end;

function TIWindows.ShowOwnedPopups(show: boolean = true) : boolean;
var i1 : integer;
begin
  result := false;
  for i1 := 0 to FCount - 1 do
    result := IWindow(FItems[i1]).ShowOwnedPopups(show) or result;
end;

function TIWindows.PostMessage(msg    : cardinal;
                               wParam : integer = 0;
                               lParam : integer = 0) : boolean;
var i1 : integer;
begin
  result := false;
  for i1 := 0 to FCount - 1 do
    result := IWindow(FItems[i1]).PostMessage(msg, wParam, lParam) or result;
end;

function TIWindows.Close : boolean;
var i1 : integer;
begin
  result := false;
  for i1 := 0 to FCount - 1 do
    result := IWindow(FItems[i1]).Close or result;
end;

function TIWindows.Destroy_ : boolean;
var i1 : integer;
begin
  result := false;
  for i1 := 0 to FCount - 1 do
    result := IWindow(FItems[i1]).Destroy or result;
end;

function TIWindows.GetItem(index: integer) : IWindow;
begin
  if (index < 0) or (index >= FCount) then
       result := TIWindow.Create(false, CErrorNo_IndexOutOfRange, UnicodeString(AnsiString(CErrorStr_IndexOutOfRange)), 0, 0, 0)
  else result := IWindow(FItems[index]);
end;

function TIWindows_RefreshItem(wnd: cardinal; iws: TIWindows) : LongBool; stdcall;
var tid, pid : cardinal;
    i1       : integer;
    iw1      : TIWindow;
begin
  result := true;
  with iws do begin
    tid := GetWindowThreadProcessID(wnd, @pid);
    if ( (FProcess = nil) or (not FProcess.IsValid) or (FProcess.ID = pid) ) and
       ( (not FInTaskbar) or
         (IsWindowVisible(wnd) and (GetWindow(wnd, GW_OWNER) = 0) and
          (GetParent(wnd) = 0) and
          (GetWindowLong(wnd, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = 0)  ) ) then begin
      for i1 := 0 to FCount - 1 do
        with TIWindow(FItems[i1].SelfAsTObject) do
          if (FHandle = wnd) and (FProcessID = pid) and (FThreadID = tid) then begin
            iws.FItemInfos[i1].LastChangeType := lctUnChanged;
            exit;
          end;
      iw1 := AddWindow(wnd);
      if AddItem(IWindow(iw1)) = -1 then
        iw1.Destroy;
    end;
  end;
end;

function TIWindows.RefreshItems : boolean;
begin
  result := false;
  if CheckValid then
    if not FList then begin
      BeginRefresh;
      try
        if (FThread <> nil) and FThread.IsValid then begin
          if FThread.ID <> 0 then
            EnumThreadWindows(FThread.ID, @TIWindows_RefreshItem, integer(self));
        end else EnumWindows(@TIWindows_RefreshItem, integer(self));
      finally EndRefresh end;
    end else SetLastError(CErrorNo_RefreshListError, UnicodeString(AnsiString(CErrorStr_RefreshListError)));
end;

function TIWindows.GetMaxInterface : IBasic;
begin
  result := IWindows(self);
end;

function Windows_(systemWide: boolean = false) : IWindows;
begin
  if systemWide then result := TIWindows.Create(true, 0, '', nil, nil,            false, [])
  else               result := TIWindows.Create(true, 0, '', nil, CurrentProcess, false, []);
end;

function TaskbarWindows(systemWide: boolean = true) : IWindows;
begin
  if systemWide then result := TIWindows.Create(true, 0, '', nil, nil,            true, [])
  else               result := TIWindows.Create(true, 0, '', nil, CurrentProcess, true, []);
end;

function PickWindows(const objects: array of IBasic) : IWindows;
begin
  result := TIWindows.Create(true, 0, '', nil, nil, false, PickInterfaces(objects, TIWindow));
end;

function PickWindows(const objects: ICustomBasicList) : IWindows;
begin
  if (objects <> nil) and objects.IsValid then begin
    result := TIWindows.Create(true, 0, '', nil, nil, false,
                PickInterfaces(TICustomBasicList(objects.SelfAsTObject).FItems, TIWindow));
  end else
    result := TIWindows.Create(false, ERROR_INVALID_PARAMETER, '', nil, nil, false, []);
end;

// ***************************************************************

type
  // forward
  TITrayIcons = class;

  // implements ITrayIcon
  TITrayIcon = class (TIBasic, ITrayIcon)
  public
    FNID        : TNotifyIconDataA;
    FWindow     : IWindow;
    FImageIndex : integer;
    FVisible    : boolean;
    FRect       : TRect;
    FTrayIcons  : TITrayIcons;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        trayIcons: TITrayIcons; const nid: TNotifyIconDataA; imageIndex: integer; rect: PRect);
    destructor Destroy; override;

    function ID : cardinal;

    function  GetWindow  : IWindow;
    function  GetMessage : cardinal;
    procedure SetWindow  (const value: IWindow );
    procedure SetMessage (      value: cardinal);

    function  GetHint : AnsiString;
    procedure SetHint (value: AnsiString);

    function GetImageIndex : integer;

    function  GetIcon : cardinal;
    procedure SetIcon (value: cardinal);

    function GetRect : TRect;

    procedure MouseMove;
    procedure MouseClick (right: boolean = true; Double: boolean = false);

    function  GetVisible : boolean;
    procedure SetVisible (value: boolean);
    function  Hide       : boolean;
    function  Show       : boolean;

    function Delete : boolean;

    function GetMaxInterface : IBasic; override;
  end;

  // implements ITrayIcons
  TITrayIcons = class (TICustomBasicList, ITrayIcons)
  public
    FAll       : boolean;
    FList      : boolean;
    FProcess   : IProcess;
    FImageList : cardinal;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        const process: IProcess; const objects: array of IBasic);
    destructor Destroy; override;

    function GetOwnerProcess : IProcess;

    function GetImageList : cardinal;

    function Hide : boolean;
    function Show : boolean;

    function Delete : boolean;

    function GetItem (index: integer) : ITrayIcon;

    function RefreshItems : boolean;

    function GetMaxInterface : IBasic; override;
  end;

constructor TITrayIcon.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                              trayIcons: TITrayIcons; const nid: TNotifyIconDataA; imageIndex: integer; rect: PRect);
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FTrayIcons  := trayIcons;
    FNID        := nid;
    FNID.cbSize := sizeOf(TNotifyIconDataA);
    FImageIndex := imageIndex;
    if rect <> nil then
      FRect := rect^
    else
      ZeroMemory(@FRect, sizeOf(FRect));
    FVisible    := true;
  end;
end;

destructor TITrayIcon.Destroy;
begin
  if FValid and (not FVisible) then
    Show;
  if FNID.hIcon <> 0 then
    DestroyIcon(FNID.hIcon);
  inherited;
end;

function TITrayIcon.ID : cardinal;
begin
  result := FNID.uID;
end;

function TITrayIcon.GetWindow : IWindow;
begin
  if FWindow = nil then
    if CheckValid then
         FWindow := AddWindow(FNID.Wnd)
    else FWindow := TIWindow.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), 0, 0, 0);
  result := FWindow;
end;

function TITrayIcon.GetMessage : cardinal;
begin
  result := FNID.uCallbackMessage;
end;

procedure TITrayIcon.SetWindow(const value: IWindow);
var c1 : cardinal;
    b1 : boolean;
    s1 : UnicodeString;
begin
  if CheckValid then
    if (value <> nil) and value.IsStillValid then begin
      c1 := FNID.Wnd;
      b1 := FVisible;
      if (not b1) or Hide then begin
        FNID.Wnd := value.Handle;
        if not Show then FNID.Wnd := c1;
        if b1 <> FSuccess then begin
          c1 := FLastErrorNo;
          s1 := FLastErrorStr;
          SetVisible(b1);
          SetLastError(c1, s1);
        end;
      end;
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

procedure TITrayIcon.SetMessage(value: cardinal);
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := FNID.uCallbackMessage;
    FNID.uCallbackMessage := value;
    if FVisible then begin
      FNID.uFlags := NIF_MESSAGE;
      if not Shell_NotifyIcon(NIM_MODIFY, @FNID) then begin
        FNID.uCallbackMessage := c1;
        SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
      end;
      FNID.uFlags := 0;
    end;
  end;
end;

function TITrayIcon.GetHint : AnsiString;
begin
  result := FNID.szTip;
end;

procedure TITrayIcon.SetHint(value: AnsiString);
var s1 : AnsiString;
begin
  if CheckValid then begin
    s1 := FNID.szTip;
    StrPLCopy(FNID.szTip, value, 63);
    if FVisible then begin
      FNID.uFlags := NIF_TIP;
      if not Shell_NotifyIcon(NIM_MODIFY, @FNID) then begin
        StrPCopy(FNID.szTip, s1);
        SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
      end;
      FNID.uFlags := 0;
    end;
  end;
end;

function TITrayIcon.GetImageIndex : integer;
begin
  result := FImageIndex;
end;

function TITrayIcon.GetIcon : cardinal;
var i1 : integer;
begin
  result := 0;
  if CheckValid then
    if FVisible then begin
      if FTrayIcons = nil then begin
        with TrayIcons do
          for i1 := 0 to ItemCount - 1 do
            if (Items[i1].ID = FNID.uID) and (Items[i1].Window.Handle = FNID.Wnd) then begin
              result := ImageList_GetIcon(ImageList, Items[i1].ImageIndex, ILD_NORMAL);
              break;
            end;
      end else result := ImageList_GetIcon(FTrayIcons.FImageList, FImageIndex, ILD_NORMAL);
    end else result := FNID.hIcon;
end;

procedure TITrayIcon.SetIcon(value: cardinal);
begin
  if CheckValid then begin
    FNID.hIcon  := value;
    if FVisible then begin
      FNID.uFlags := NIF_ICON;
      if not Shell_NotifyIcon(NIM_MODIFY, @FNID) then
        SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
      FNID.hIcon  := 0;
      FNID.uFlags := 0;
    end;
  end;
end;

function TITrayIcon.GetRect : TRect;
begin
  result := FRect;
end;

procedure TITrayIcon.MouseMove;
begin
  if CheckValid then
    PostMessageA(FNID.Wnd, FNID.uCallbackMessage, FNID.uID, WM_MOUSEMOVE);
end;

procedure TITrayIcon.MouseClick(right: boolean = true; Double: boolean = false);
begin
  if CheckValid then
    if double then begin
      if right then
           PostMessageA(FNID.Wnd, FNID.uCallbackMessage, FNID.uID, WM_RBUTTONDBLCLK)
      else PostMessageA(FNID.Wnd, FNID.uCallbackMessage, FNID.uID, WM_LBUTTONDBLCLK);
    end else
      if right then begin
        PostMessageA(FNID.Wnd, FNID.uCallbackMessage, FNID.uID, WM_RBUTTONDOWN);
        PostMessageA(FNID.Wnd, FNID.uCallbackMessage, FNID.uID, WM_RBUTTONUP  );
      end else begin
        PostMessageA(FNID.Wnd, FNID.uCallbackMessage, FNID.uID, WM_LBUTTONDOWN);
        PostMessageA(FNID.Wnd, FNID.uCallbackMessage, FNID.uID, WM_LBUTTONUP  );
      end;
end;

function TITrayIcon.GetVisible : boolean;
begin
  result := FVisible;
end;

procedure TITrayIcon.SetVisible(value: boolean);
begin
  if value <> FVisible then
    if value then Show
    else          Hide;
end;

function TITrayIcon.Hide : boolean;
begin
  result := not FVisible;
  if (not result) and CheckValid then begin
    FNID.hIcon := GetIcon;
    if FNID.hIcon <> 0 then begin
      result := Shell_NotifyIcon(NIM_DELETE, @FNID) or (not TrayWindow.IsValid);
      if not result then begin
        DestroyIcon(FNID.hIcon);
        FNID.hIcon := 0;
        SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
      end else FVisible := false;
    end;
  end;
end;

function TITrayIcon.Show : boolean;
begin
  result := FVisible;
  if (not result) and CheckValid then begin
    FNID.uFlags := NIF_MESSAGE or NIF_TIP or NIF_ICON;
    result := TrayWindow.IsValid and Shell_NotifyIcon(NIM_ADD, @FNID);
    if result then begin
      FNID.uFlags := 0;
      DestroyIcon(FNID.hIcon);
      FNID.hIcon := 0;
      FVisible := true;
    end else
      SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
  end;
end;

function TITrayIcon.Delete : boolean;
begin
  result := false;
  if CheckValid then
    if (not FVisible) or (not TrayWindow.IsValid) or
       Shell_NotifyIcon(NIM_DELETE, @FNID) then begin
      FValid := false;
      result := true;
      if not FVisible then
        DestroyIcon(FNID.hIcon);
      if FTrayIcons <> nil then
        FTrayIcons.DeleteItem(GetIndex(FTrayIcons));
    end else
      SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
end;

function TITrayIcon.GetMaxInterface : IBasic;
begin
  result := ITrayIcon(self);
end;

// ***************************************************************

type
  // types for TITrayIcons.RefreshItems.CopyImageList
  TCopyImageListRec = record
    imageList : cardinal;
    process   : cardinal;
    buffer    : pointer;
    size      : cardinal;
    convert   : boolean;
    result    : boolean;
    lastError : cardinal;
  end;

const ole32 = 'ole32.dll';

function CreateMiniStreamOnHGlobal (hglobal         : cardinal;
                                    deleteOnRelease : bool;
                                    out stream      : cardinal) : cardinal; stdcall;
         external ole32 name 'CreateStreamOnHGlobal';
function GetHGlobalFromMiniStream (stream      : cardinal;
                                   out hglobal : cardinal) : cardinal; stdcall;
         external ole32 name 'GetHGlobalFromStream';
function ImageList_WriteToMiniStream (imageList : cardinal;
                                      stream    : cardinal) : bool; stdcall;
         external 'comctl32.dll' name 'ImageList_Write';

constructor TITrayIcons.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                               const process: IProcess; const objects: array of IBasic);
var i1 : integer;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FAll      := process = nil;
    FProcess  := process;
    FList     := Length(objects) > 0;
    if FList then
      for i1 := 0 to high(objects) do
        if objects[i1] <> nil then
          AddItem(objects[i1]);
    if not FList then RefreshItems;
  end;
end;

destructor TITrayIcons.Destroy;
begin
  if FValid and (FImageList <> 0) then ImageList_Destroy(FImageList);
  inherited;
end;

function TITrayIcons.GetOwnerProcess : IProcess;
begin
  if FProcess = nil then
    FProcess := TIProcess.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), otUnknown, 0);
  result := FProcess;
end;

function TITrayIcons.GetImageList : cardinal;
begin
  result := FImageList;
end;

function TITrayIcons.Hide : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := ITrayIcon(FItems[i1]).Hide or result;
end;

function TITrayIcons.Show : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := ITrayIcon(FItems[i1]).Show or result;
end;

function TITrayIcons.Delete : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := FCount - 1 downto 0 do
      result := ITrayIcon(FItems[i1]).Delete or result;
end;

function TITrayIcons.GetItem(index: integer) : ITrayIcon;
var nid : TNotifyIconDataA;
begin
  if (index < 0) or (index >= FCount) then
       result := TITrayIcon.Create(false, CErrorNo_IndexOutOfRange, UnicodeString(AnsiString(CErrorStr_IndexOutOfRange)), nil, nid, 0, nil)
  else result := ITrayIcon(FItems[index]);
end;

var ImageListSize : cardinal = 0;
function TITrayIcons.RefreshItems : boolean;
type
  TTrayIconInfo = record
    imageIndex : cardinal;
    case boolean of
      false : (notifyIconDataA: TNotifyIconDataA);
      true  : (notifyIconDataW: TNotifyIconDataW);
  end;
  TTrayIconsInfo = record
    iconCount     : integer;
    trayIconInfos : TPAPointer;
  end;
  TTrayWindowInfo = record
    dummy     : array [0..6] of cardinal;
    iconsInfo : ^TTrayIconsInfo;
    iconList  : cardinal;
  end;
  TTrayWindowInfo2000 = record
    dummy      : cardinal;
    window     : cardinal;
    dummy2     : cardinal;
    iconList   : cardinal;
    dummy3     : cardinal;
    xpWindow   : cardinal;
    dummy4     : array [0..135] of dword;
    xpIconList : cardinal;
  end;
  TTrayIconInfo2000 = record
    window      : cardinal;
    id          : cardinal;
    callbackMsg : cardinal;
    flags       : cardinal;
  end;
  TTrayIconCommand2000 = record
    size       : cardinal;
    flags      : cardinal;
    id         : cardinal;
    imageIndex : cardinal;
    state      : cardinal;
    iconInfo   : ^TTrayIconInfo2000;
    tip        : PWideChar;
    tipLen     : cardinal;
  end;
  TTrayIconCommand2000Ex = record
    main : TTrayIconCommand2000;
    str  : array [0..MAX_PATH] of wideChar;
  end;

  procedure RefreshItem(nid: TNotifyIconDataA; imageIndex: integer; const rect: TRect);
  var i1   : integer;
      iti1 : TITrayIcon;
      pid  : cardinal;
  begin
    GetWindowThreadProcessID(nid.Wnd, @pid);
    if FAll or (pid = FProcess.ID) then begin
      for i1 := 0 to FCount - 1 do
        with TITrayIcon(FItems[i1].SelfAsTObject) do
          if (FNID.uID = nid.uID) and (FNID.Wnd = nid.Wnd) then begin
            if (FNID.uFlags            <> nid.uFlags           ) or
               (FNID.uCallbackMessage  <> nid.uCallbackMessage ) or
               (AnsiString(FNID.szTip) <> AnsiString(nid.szTip)) or
               (FImageIndex            <> imageIndex           ) then begin
              Change(FItems[i1], true, lctChanged, i1, i1);
              FNID.uFlags           := nid.uFlags;
              FNID.uCallbackMessage := nid.uCallbackMessage;
              Move(nid.szTip, FNID.szTip, sizeOf(nid.szTip));
              FImageIndex := imageIndex;
              self.FItemInfos[i1].LastChangeType := lctChanged;
              Change(FItems[i1], false, lctChanged, i1, SortItem(i1));
            end else
              self.FItemInfos[i1].LastChangeType := lctUnChanged;
            exit;
          end;
      iti1 := TITrayIcon.Create(true, 0, '', self, nid, imageIndex, @rect);
      if AddItem(ITrayIcon(iti1)) = -1 then
        iti1.Destroy;
    end;
  end;

  function CopyImageList(const process: IProcess; imageList: cardinal) : cardinal;

    procedure CopyImageListNT(var params);
    var c1     : cardinal;
        c2     : NativeUInt;
        is1    : cardinal;
        p1     : pointer;
        il     : dword;
        i1, i2 : integer;
    begin
      with TCopyImageListRec(params) do begin
        result := false;
        lastError := CreateMiniStreamOnHGlobal(0, true, is1);
        if convert then begin
          ImageList_GetIconSize(imageList, i1, i2);
          il := ImageList_Create(i1, i2, ILC_COLOR32 or ILC_MASK, 1, 1);
          for i1 := 0 to ImageList_GetImageCount(imageList) - 1 do begin
            c1 := ImageList_GetIcon(imageList, i1, ILD_NORMAL);
            ImageList_ReplaceIcon(il, -1, c1);
            DestroyIcon(c1);
          end;
          imageList := il;
        end;
        if lastError = 0 then begin
          if ImageList_WriteToMiniStream(imageList, is1) then begin
            lastError := GetHGlobalFromMiniStream(is1, c1);
            if lastError = 0 then begin
              size := GlobalSize(c1);
              p1 := GlobalLock(c1);
              if p1 <> nil then begin
                buffer := VirtualAlloc(nil, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
                result := WriteProcessMemory(GetCurrentProcess, buffer, p1, size, c2);
                if not result then
                  lastError := GetLastError;
                GlobalUnlock(c1);
              end else lastError := GetLastError;
            end;
          end else lastError := cardinal(-1);
          IStream(is1)._Release;
        end;
        if convert then
          ImageList_Destroy(imageList);
        CloseHandle(process);
      end;
    end;

  var params : TCopyImageListRec;
      is1    : IStream;
      mem    : cardinal;
      p1     : pointer;
      c1     : cardinal;
      i64    : {$ifdef xe8} LargeUInt {$else} int64 {$endif};
      arrCh  : array [0..MAX_PATH] of AnsiChar;
  begin
    result := 0;
    params.imageList := imageList;
    if OS.winNT then begin
      params.process := CurrentProcess.Handle.Duplicate(false, 0, false, process).Handle;
      params.size    := 0;
      params.buffer  := nil;
      try
        params.result    := false;
        params.lastError := ERROR_ACCESS_DENIED;
        GetModuleFileNameA(GetModuleHandle('comctl32.dll'), arrCh, MAX_PATH);
        params.convert   := GetFileVersion(arrCh) shr 48 < 6;
        if process.ExecuteFunction(@CopyImageListNT, 5000, @params, sizeOf(TCopyImageListRec), true) then
          if params.result then begin
            mem := GlobalAlloc(GMEM_NODISCARD or GMEM_MOVEABLE, params.size);
            if mem <> 0 then
              try
                p1 := GlobalLock(mem);
                if p1 <> nil then begin
                  process.ReadMemory(params.buffer^, p1^, params.size);
                  GlobalUnlock(mem);
                  c1 := CreateStreamOnHGlobal(mem, false, is1);
                  if c1 = 0 then begin
                    result := ImageList_Read(is1);
                    if result = 0 then
                      SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
                  end else SetLastError(c1);
                end;
              finally GlobalFree(mem) end;
          end else SetLastError(params.lastError);
      finally process.FreeMem(params.buffer) end;
    end else begin
      if ImageListSize = 0 then begin
        c1 := ImageList_Create(16, 16, ILC_COLOR, 1, 1);
        ImageListSize := GlobalSize(c1);
        ImageList_Destroy(c1);
      end;
      if ImageListSize <> 0 then begin
        params.buffer := AllocMem(ImageListSize);
        try
          if process.ReadMemory(pointer(imageList)^, params.buffer^, ImageListSize) then begin
            c1 := CreateStreamOnHGlobal(0, true, is1);
            if c1 = 0 then begin
              if ImageList_Write(cardinal(params.buffer), is1) and
                 (is1.Seek(0, STREAM_SEEK_SET, i64) = 0) then
                result := ImageList_Read(is1);
              if result = 0 then
                SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
            end else SetLastError(c1);
          end;
        finally FreeMem(params.buffer) end;
      end else SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
    end;
  end;

var wnd    : IWindow;
    p1     : pointer;
    twi    : TTrayWindowInfo;
    tisi   : TTrayIconsInfo;
    tii    : TTrayIconInfo;
    i1     : integer;
    tip    : AnsiString;
    twi2   : TTrayWindowInfo2000;
    tic    : TTrayIconCommand2000Ex;
    ptic   : ^TTrayIconCommand2000Ex;
    tii2   : TTrayIconInfo2000;
    c1     : dword;
    r1, r2 : TRect;
    ih     : integer;
begin
  result := false;
  if CheckValid then begin
    BeginRefresh;
    try
      wnd := TrayWindow;
      p1 := pointer(wnd.Param[0]);
      if p1 <> nil then
        with wnd.OwnerProcess do
          if IsValid then begin
            StoreHandle;
            try
              with GetHandle(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or
                             PROCESS_VM_WRITE or PROCESS_VM_OPERATION) do
                if IsValid then begin
                  try
                    if (OS.winNtEnum >= osWin2k) or (OS.enum = osWinME) then begin
                      if ReadMemory(p1^, twi2, sizeOf(TTrayWindowInfo2000)) then begin
                        if FImageList <> 0 then begin
                          ImageList_Destroy(FImageList);
                          FImageList := 0;
                        end;
                        try
                          if OS.winNtEnum <= osWin2k then
                               c1 := twi2.iconList
                          else c1 := twi2.xpIconList;
                          FImageList := CopyImageList(wnd.OwnerProcess, c1);
                        except end;
                        ptic := AllocMem(sizeOf(TTrayIconCommand2000Ex));
                        try
                          if OS.winNtEnum <= osWin2k then
                               c1 := twi2.window
                          else c1 := twi2.xpWindow;
                          if OS.win9x then
                               i1 := SendMessageA(c1, $418, 0, 0)
                          else i1 := SendMessageW(c1, $418, 0, 0);
                          GetWindowRect(c1, r2);
                          for i1 := 0 to i1 - 1 do begin
                            windows.ZeroMemory(@tic, sizeOf(TTrayIconCommand2000Ex));
                            tic.main.size := sizeOf(TTrayIconCommand2000);
                            tic.main.flags := $80000037;
                            integer(tic.main.tip) := integer(ptic) + sizeOf(TTrayIconCommand2000);
                            tic.main.tipLen := MAX_PATH;
                            if WriteMemory(tic, ptic^, sizeOf(TTrayIconCommand2000Ex)) then begin
                              if OS.win9x then
                                   SendMessageA(c1, $43F, i1, integer(ptic))
                              else SendMessageW(c1, $43F, i1, integer(ptic));
                              if ReadMemory(ptic^, tic, sizeOf(TTrayIconCommand2000Ex)) and
                                 (tic.main.iconInfo <> nil) and
                                 ReadMemory(tic.main.iconInfo^, tii2, sizeOf(TTrayIconInfo2000)) then begin
                                if tic.main.state and 8 = 0 then begin  // not hidden
                                  tii.notifyIconDataA.Wnd := tii2.window;
                                  tii.notifyIconDataA.uID := tii2.id;
                                  tii.notifyIconDataA.uFlags := tii2.flags;
                                  tii.notifyIconDataA.uCallbackMessage := tii2.callbackMsg;
                                  StrPLCopy(tii.notifyIconDataA.szTip, AnsiString(UnicodeString(tic.str)), 64);
                                  tii.notifyIconDataA.cbSize := sizeOf(TNotifyIconDataA);
                                  if ( (OS.win9x and (SendMessageA(c1, TB_GETRECT, integer(tic.main.id), integer(ptic)) <> 0)) or
                                       (OS.winNt and (SendMessageW(c1, TB_GETRECT, integer(tic.main.id), integer(ptic)) <> 0))    ) and
                                     ReadMemory(ptic^, r1, sizeOf(TRect)) then
                                    OffsetRect(r1, r2.Left, r2.Top)
                                  else
                                    windows.ZeroMemory(@r1, sizeOf(r1));
                                  RefreshItem(tii.notifyIconDataA, tic.main.imageIndex, r1);
                                end;
                              end;
                            end;
                          end;
                          result := true;
                        finally FreeMem(pointer(ptic)) end;
                      end;
                    end else
                      if ReadMemory(p1^, twi, sizeOf(TTrayWindowInfo)) then begin
                        if FImageList <> 0 then begin
                          ImageList_Destroy(FImageList);
                          FImageList := 0;
                        end;
                        try
                          FImageList := CopyImageList(wnd.OwnerProcess, twi.iconList);
                        except end;
                        if ReadMemory(twi.iconsInfo^, tisi, sizeOf(TTrayIconsInfo)) and
                           (tisi.IconCount < 50) then begin
                          for i1 := 0 to tisi.IconCount - 1 do
                            if ReadMemory(pointer(integer(tisi.trayIconInfos) + i1 * 4)^, p1, 4) and
                               ReadMemory(p1^, tii, sizeOf(TTrayIconInfo)) then begin
                              if OS.winNt then begin
                                tip := AnsiString(tii.notifyIconDataW.szTip);
                                StrPCopy(tii.notifyIconDataA.szTip, tip);
                              end;
                              tii.notifyIconDataA.cbSize := sizeOf(TNotifyIconDataA);
                              with wnd do
                                if IsValid then begin
                                  with ClientRect do
                                    ih := Bottom - Top - 3;
                                  with Rect do begin
                                    r1.Left   := Left + 1 + ih * FCount;
                                    r1.Right  := r1.Left + ih;
                                    r1.Top    := Top + 2;
                                    r1.Bottom := Top + 2 + ih;
                                  end;
                                end else
                                  windows.ZeroMemory(@r1, sizeOf(r1));
                              RefreshItem(tii.notifyIconDataA, tii.imageIndex, r1);
                            end;
                          result := true;
                        end;
                      end;
                  except end;
                end else self.SetLastError(LastErrorNo, LastErrorStr);
            finally ReleaseHandle end;
          end else self.SetLastError(LastErrorNo, LastErrorStr);
    finally EndRefresh end;
    for i1 := FCount - 1 downto 0 do
      if not ITrayIcon(FItems[i1]).Window.IsStillValid then
        ITrayIcon(FItems[i1]).Delete;
  end;
end;

function TITrayIcons.GetMaxInterface : IBasic;
begin
  result := ITrayIcons(self);
end;

function TrayIcons(systemWide: boolean = true) : ITrayIcons;
begin
  if systemWide then
       result := TITrayIcons.Create(true, 0, '', nil,            [])
  else result := TITrayIcons.Create(true, 0, '', CurrentProcess, []);
end;

function PickTrayIcons(const objects: array of IBasic) : ITrayIcons;
begin
  result := TITrayIcons.Create(true, 0, '', nil, PickInterfaces(objects, TITrayIcon));
end;

function PickTrayIcons(const objects: ICustomBasicList) : ITrayIcons;
begin
  if (objects <> nil) and objects.IsValid then begin
    result := TITrayIcons.Create(true, 0, '', nil,
                PickInterfaces(TICustomBasicList(objects.SelfAsTObject).FItems, TITrayIcon));
  end else
    result := TITrayIcons.Create(false, ERROR_INVALID_PARAMETER, '', nil, []);
end;

// ***************************************************************

type
  // forward
  TIModule = class;

  // implements IExportEntry
  TIExportEntry = class (TIBasic, IExportEntry)
  public
    FPid          : cardinal;
    FModuleName   : AnsiString;
    FModuleHandle : cardinal;
    FOrdinal      : cardinal;
    FName         : AnsiString;
    FAddress      : pointer;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        pid: cardinal; moduleName: AnsiString; moduleHandle: cardinal;
                        ordinal: cardinal; name: AnsiString; address: pointer);

    function GetExportModule : IModule;

    function GetOrdinal : cardinal;

    function GetName : AnsiString;

    function GetAddress : pointer;

    function GetMaxInterface : IBasic; override;
  end;
  // implements IXxportList
  TIXxportList = class (TICustomBasicList, IXxportList)
  public
    function GetItem (index: integer) : IExportEntry;

    function FindItem (func: pointer) : IExportEntry; overload;

    function NewItem (pid          : cardinal;
                      moduleName   : AnsiString;
                      moduleHandle : cardinal;
                      ordinal      : cardinal;
                      name         : AnsiString;
                      address      : pointer ) : integer;

    function GetMaxInterface : IBasic; override;
  end;

  // implements IModule
  TIModule = class (TIBasic, IModule)
  public
    FProcess      : IProcess;
    FHandle       : cardinal;
    FFileName     : AnsiString;
    FIsMain       : TExtBool;
    FAutoClose    : boolean;
    FImportList   : IXxportList;
    FExportList   : IXxportList;
    FEntryPoint   : pointer;
    FImageHeaders : PImageNtHeaders;
    FImageImport  : PImageImportDirectory;
    FImageExport  : PImageExportDirectory;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        const process: IProcess;
                        handle: cardinal; fileName_: AnsiString; autoClose: boolean);
    destructor Destroy; override;

    function IsStillValid : boolean;

    function GetOwnerProcess : IProcess;

    function GetHandle : cardinal;

    function GetHInstance : cardinal;

    function GetMemory : pointer;

    function GetFileName : AnsiString;

    function IsMainModule : boolean;

    function  GetAutoClose : boolean;
    procedure SetAutoClose (value: boolean);

    function GetProcAddress (name  : AnsiString) : pointer; overload;
    function GetProcAddress (index : integer   ) : pointer; overload;

    function SetProcAddress (name  : AnsiString; newAddr: pointer) : boolean; overload;
    function SetProcAddress (index : integer;    newAddr: pointer) : boolean; overload;

    function GetImportList : IXxportList;
    function GetExportList : IXxportList;

    function GetEntryPoint : pointer;

    function GetImageNtHeaders : PImageNtHeaders;

    function GetImageImportDirectory : PImageImportDirectory;
    function GetImageExportDirectory : PImageExportDirectory;

    function GetMaxInterface : IBasic; override;
  end;

var
  XxportListCache : array of record
    ModuleName   : AnsiString;
    ModuleHandle : cardinal;
    ImportList   : IXxportList;
    ExportList   : IXxportList;
  end;
  XxportSection : ICriticalSection;

{procedure GetGlobalModuleListPtr(module: dword); stdcall; external kernel32 index 23;

function GetGlobalModuleList(var globalModuleList: TGlobalModuleList) : boolean;

  function InternalGetGlobalModuleListPtr : pointer;
  asm
    push 0
    call GetModuleHandle;
    push eax
    call GetGlobalModuleListPtr;
    mov eax, ecx
  end;

var pap : TPAPointer;
    i1  : integer;
begin
  try
    pap := InternalGetGlobalModuleListPtr;
    result := pap <> nil;
    if result then
      with globalModuleList do begin
        i1 := HeapSize(TP9xPid(GetCurrentProcessID xor Magic)^.HeapHandle, 0, pap) div 4;
        SetLength(Items, i1);
        ItemCount := 0;
        for i1 := 0 to i1 - 1 do
          if pap^[i1] <> nil then begin
            Items[ItemCount] := pap^[i1];
            inc(ItemCount);
          end;
        SetLength(Items, ItemCount);
      end;
  except result := false end;
end; }

function FindXxportList(import: boolean; moduleName: AnsiString; moduleHandle: cardinal) : IXxportList;
var i1 : integer;
begin
  result := nil;
  if XxportSection <> nil then begin
    XxportSection.Enter;
    try
      for i1 := 0 to high(XxportListCache) do
        if (XxportListCache[i1].ModuleHandle = moduleHandle) and
           IsTextEqual(XxportListCache[i1].ModuleName, moduleName) then begin
          if import then result := XxportListCache[i1].ImportList
          else           result := XxportListCache[i1].ExportList;
          break;
        end;
    finally XxportSection.Leave end;
  end else
    XxportSection := NewCriticalSection;
end;

procedure AddXxportList(const list: IXxportList; import: boolean;
                        moduleName: AnsiString; moduleHandle: cardinal);
var i1 : integer;
    b1 : boolean;
begin
  if XxportSection = nil then XxportSection := NewCriticalSection;
  XxportSection.Enter;
  try
    b1 := true;
    for i1 := 0 to high(XxportListCache) do
      if (XxportListCache[i1].ModuleHandle = moduleHandle) and
         IsTextEqual(XxportListCache[i1].ModuleName, moduleName) then begin
        b1 := false;
        break;
      end;
    if b1 then begin
      i1 := Length(XxportListCache);
      SetLength(XxportListCache, i1 + 1);
      XxportListCache[i1].ModuleName   := moduleName;
      XxportListCache[i1].ModuleHandle := moduleHandle;
    end;
    if import then XxportListCache[i1].ImportList := list
    else           XxportListCache[i1].ExportList := list;
  finally XxportSection.Leave end;
end;

constructor TIExportEntry.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                                 pid: cardinal; moduleName: AnsiString; moduleHandle: cardinal;
                                 ordinal: cardinal; name: AnsiString; address: pointer);
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FPid          := pid;
    FModuleName   := moduleName;
    FModuleHandle := moduleHandle;
    FOrdinal      := ordinal;
    FName         := name;
    FAddress      := address;
  end;
end;

function TIExportEntry.GetExportModule : IModule;
begin
  if CheckValid then
       result := TIModule.Create(true, 0, '', Process(FPid), FModuleHandle, FModuleName, false)
  else result := TIModule.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), nil, 0, '', false);
end;

function TIExportEntry.GetOrdinal : cardinal;
begin
  result := FOrdinal;
end;

function TIExportEntry.GetName : AnsiString;
begin
  result := FName;
end;

function TIExportEntry.GetAddress : pointer;
begin
  result := FAddress;
end;

function TIExportEntry.GetMaxInterface : IBasic;
begin
  result := IExportEntry(self);
end;

function TIXxportList.GetItem(index: integer) : IExportEntry;
begin
  if (index < 0) or (index >= FCount) then
       result := TIExportEntry.Create(false, CErrorNo_IndexOutOfRange, UnicodeString(AnsiString(CErrorStr_IndexOutOfRange)), 0, '', 0, 0, '', nil)
  else result := IExportEntry(FItems[index]);
end;

function SortExportListByAddress(const list: IList; const item1, item2: IBasic; info: integer) : integer;
begin
  if cardinal(IExportEntry(item1).Address) < cardinal(IExportEntry(item2).Address) then
    result := -1
  else if cardinal(IExportEntry(item1).Address) > cardinal(IExportEntry(item2).Address) then
    result := +1
  else
    result := 0;
end;

function TIXxportList.FindItem(func: pointer) : IExportEntry;
var i1, i2 : integer;
    c1     : cardinal;
    b1     : boolean;
begin
  if FCount > 0 then begin
    SetSortParams(SortExportListByAddress);
    i1 := FCount div 2;
    i2 := (i1 + 2) div 2;
    b1 := false;
    while i2 > 0 do begin
      c1 := cardinal(IExportEntry(FItems[i1]).Address);
      if cardinal(func) = c1 then begin
        result := IExportEntry(FItems[i1]);
        exit;
      end;
      if b1 then break;
      if cardinal(func) < c1 then begin
        dec(i1, i2);
        if i1 < 0 then i1 := 0;
      end else begin
        inc(i1, i2);
        if i1 >= FCount then i1 := FCount - 1;
      end;
      if i2 = 1 then b1 := true;
      i2 := (i2 + 1) div 2;
    end;
  end;
  result := TIExportEntry.Create(false, ERROR_FILE_NOT_FOUND, '', 0, '', 0, 0, '', nil);
end;

function TIXxportList.NewItem(pid          : cardinal;
                              moduleName   : AnsiString;
                              moduleHandle : cardinal;
                              ordinal      : cardinal;
                              name         : AnsiString;
                              address      : pointer   ) : integer;
var xe : TIExportEntry;
begin
  if CheckValid then begin
    xe := TIExportEntry.Create(true, 0, '', pid, moduleName, moduleHandle, ordinal, name, address);
    result := AddItem(IExportEntry(xe));
    if result = -1 then xe.Destroy;
  end else result := -1;
end;

function TIXxportList.GetMaxInterface : IBasic;
begin
  result := IXxportList(self);
end;

constructor TIModule.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                            const process: IProcess;
                            handle: cardinal; fileName_: AnsiString; autoClose: boolean);
var i1  : integer;
    fno : AnsiString;  // file name only
    b1  : boolean;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FValid := (handle <> 0) or (fileName_ <> '') or (not process.IsValid);
    if FValid then begin
      FProcess   := process;
      FHandle    := handle;
      FFileName  := fileName_;
      FIsMain    := other;
      FAutoClose := autoClose;
      if FHandle = 0 then
        if FProcess.ID = GetCurrentProcessID then begin
          FHandle   := GetModuleHandleA(PAnsiChar(FFileName));
          FFileName := '';
          if FHandle = 0 then SetLastError(GetLastError);
        end else begin
          with process.Modules do
            if ExtractFilePath(string(fileName_)) <> '' then begin
              fno := AnsiString(ExtractFileName(string(fileName_)));
              for i1 := 0 to ItemCount - 1 do begin
                b1 := Pos(AnsiString('\'), Items[i1].FileName) = 0;
                if (     b1  and IsTextEqual(fno,       Items[i1].FileName)) or
                   ((not b1) and IsTextEqual(fileName_, Items[i1].FileName)) then begin
                  FHandle := Items[i1].Handle;
                  break;
                end;
              end;
            end else
              for i1 := 0 to ItemCount - 1 do
                if IsTextEqual(fileName_, AnsiString(ExtractFileName(string(Items[i1].FileName)))) then begin
                  FHandle := Items[i1].Handle;
                  break;
                end;
          if FHandle = 0 then SetLastError(ERROR_FILE_NOT_FOUND);
        end;
      if FValid and (FFileName = '') then
        if FProcess.ID = GetCurrentProcessID then begin
          SetLength(FFileName, MAX_PATH + 1);
          if GetModuleFileNameA(FHandle, PAnsiChar(FFileName), MAX_PATH) = 0 then begin
            FFileName := '';
            SetLastError(GetLastError);
          end else FFileName := PAnsiChar(FFileName);
        end else begin
          with process.Modules do
            for i1 := 0 to ItemCount - 1 do
              if FHandle = Items[i1].Handle then begin
                FFileName := Items[i1].FileName;
                break;
              end;
          if FFileName = '' then SetLastError(ERROR_FILE_NOT_FOUND);
        end;
      FValid := FSuccess;
    end else SetLastError(ERROR_INVALID_PARAMETER);
  end;
end;

destructor TIModule.Destroy;
begin
  if FValid and FAutoClose then
    if FProcess.ID = GetCurrentProcessID then
         FreeLibrary(FHandle)
    else TIProcess(FProcess.SelfAsTObject).FreeModule(FHandle);
  inherited;
end;

function TIModule.IsStillValid : boolean;
var i1 : integer;
begin
  if FValid then
    if FProcess.ID = GetCurrentProcessID then begin
      FValid := GetModuleHandleA(PAnsiChar(FFileName)) = FHandle;
    end else begin
      FValid := false;
      with GetOwnerProcess do
        if IsValid then
          with Modules do
            for i1 := 0 to ItemCount - 1 do
              if (Items[i1].Handle = FHandle) and IsTextEqual(Items[i1].FileName, FFileName) then begin
                FValid := true;
                break;
              end;
    end;
  result := FValid;
end;

function TIModule.GetOwnerProcess : IProcess;
begin
  if FProcess = nil then
    FProcess := TIProcess.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), otUnknown, 0);
  result := FProcess;
end;

function TIModule.GetHandle : cardinal;
begin
  result := FHandle;
end;

function TIModule.GetHInstance : cardinal;
begin
  result := FHandle;
end;

function TIModule.GetMemory : pointer;
begin
  result := pointer(FHandle);
end;

function TIModule.GetFileName : AnsiString;
begin
  result := FFileName;
end;

function TIModule.IsMainModule : boolean;
begin
  if FIsMain = other then
    if CheckValid then begin
      if FProcess.ID = GetCurrentProcessID then begin
        FIsMain := TExtBool(FHandle = GetModuleHandle(nil));
      end else
        with GetOwnerProcess do
          FIsMain := TExtBool(IsValid and (FHandle = HInstance));
    end else FIsMain := no;
  result := FIsMain = yes;
end;

function TIModule.GetAutoClose : boolean;
begin
  result := FAutoClose;
end;

procedure TIModule.SetAutoClose(value: boolean);
begin
  FAutoClose := value;
end;

function TIModule.GetProcAddress(name: AnsiString) : pointer;
var pexp     : PImageExportDirectory;
    exp      : TImageExportDirectory;
    i1       : integer;
    aon      : array of cardinal;
    aof      : array of cardinal;
    aono     : array of word;
    c1       : dword;
    pinh     : PImageNtHeaders;
    inh      : TImageNtHeaders;
    pc1, pc2 : PAnsiChar;
    arrCh    : array [0..MAX_PATH] of AnsiChar;
begin
  result := nil;
  if CheckValid then begin
    pexp := GetImageExportDirectory;
    if pexp <> nil then
      if (FProcess.ID = GetCurrentProcessID) or (OS.win9x and (FHandle >= $80000000)) then begin
        result := GetImageProcAddress(FHandle, name);
        if result = nil then
          SetLastError(GetLastError);
      end else
        with FProcess, GetHandle(PROCESS_VM_READ) do
          if IsValid then begin
            pinh := GetImageNtHeaders;
            if ReadMemory(pinh^, inh, sizeOf(TImageNtHeaders)) and
               ReadMemory(pexp^, exp, sizeOf(TImageExportDirectory)) then
              with exp do begin
                SetLength(aon,  NumberOfNames    );
                SetLength(aono, NumberOfNames    );
                SetLength(aof,  NumberOfFunctions);
                if ReadMemory(pointer(FHandle + AddressOfNames       )^, pointer(aon )^, NumberOfNames     * 4) and
                   ReadMemory(pointer(FHandle + AddressOfNameOrdinals)^, pointer(aono)^, NumberOfNames     * 2) and
                   ReadMemory(pointer(FHandle + AddressOfFunctions   )^, pointer(aof )^, NumberOfFunctions * 4) then
                  for i1 := 0 to NumberOfNames - 1 do
                    if ReadMemory(PAnsiChar(FHandle + aon[i1])^, arrCh, MAX_PATH) and (arrCh = name) then begin
                      c1 := aof[aono[i1]];
                      if c1 > 0 then
                        with inh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT] do
                          if (c1 >= VirtualAddress) and (c1 < VirtualAddress + Size) then begin
                            if ReadMemory(PAnsiChar(FHandle + c1)^, arrCh, MAX_PATH) then begin
                              pc1 := arrCh;
                              pc2 := pc1;
                              repeat inc(pc2) until pc2^ = '.';
                              pc2^ := #0;
                              inc(pc2);
                              result := Module(AnsiString(pc1) + '.dll').GetProcAddress(pc2);
                            end;
                          end else
                            result := pointer(FHandle + c1);
                      if result = nil then
                        self.SetLastError(ERROR_FILE_NOT_FOUND);
                      break;
                    end;
              end;
          end else self.SetLastError(LastErrorNo, LastErrorStr);
  end;
end;

function TIModule.GetProcAddress(index: integer) : pointer;
var pexp     : PImageExportDirectory;
    exp      : TImageExportDirectory;
    aof      : array of cardinal;
    c1       : cardinal;
    idx      : integer absolute index;
    pinh     : PImageNtHeaders;
    inh      : TImageNtHeaders;
    pc1, pc2 : PAnsiChar;
    arrCh    : array [0..MAX_PATH] of AnsiChar;
begin
  result := nil;
  if CheckValid then begin
    pexp := GetImageExportDirectory;
    if pexp <> nil then
      if (FProcess.ID = GetCurrentProcessID) or (OS.win9x and (FHandle >= $80000000)) then begin
        try
          with pexp^ do begin
            dec(index, Base);
            if (index >= 0) and (index < NumberOfFunctions) then begin
              c1 := TPACardinal(FHandle + AddressOfFunctions)^[index];
              if c1 > 0 then
                result := pointer(FHandle + c1);
            end;
          end;
        except end;
        if result = nil then
          result := windows.GetProcAddress(FHandle, PAnsiChar(index));
        if result = nil then
          SetLastError(GetLastError);
      end else
        with FProcess, GetHandle(PROCESS_VM_READ) do
          if IsValid then begin
            pinh := GetImageNtHeaders;
            if ReadMemory(pinh^, inh, sizeOf(TImageNtHeaders)) and
               ReadMemory(pexp^, exp, sizeOf(TImageExportDirectory)) then
              with exp do begin
                dec(idx, Base);
                if (idx >= 0) and (idx < NumberOfFunctions) then begin
                  SetLength(aof, NumberOfFunctions);
                  if ReadMemory(pointer(FHandle + AddressOfFunctions)^, pointer(aof)^, NumberOfFunctions * 4) then begin
                    c1 := aof[idx];
                    if c1 > 0 then
                      with inh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT] do
                        if (c1 >= VirtualAddress) and (c1 < VirtualAddress + Size) then begin
                          if ReadMemory(PAnsiChar(FHandle + c1)^, arrCh, MAX_PATH) then begin
                            pc1 := arrCh;
                            pc2 := pc1;
                            repeat inc(pc2) until pc2^ = '.';
                            pc2^ := #0;
                            inc(pc2);
                            result := Module(AnsiString(pc1) + '.dll').GetProcAddress(pc2);
                          end;
                        end else
                          result := pointer(FHandle + c1);
                    if result = nil then
                      self.SetLastError(ERROR_FILE_NOT_FOUND);
                  end;
                end;
              end;
          end else self.SetLastError(LastErrorNo, LastErrorStr);
  end;
end;

function TIModule.SetProcAddress(name: AnsiString; newAddr: pointer) : boolean;
var pexp : PImageExportDirectory;
    exp  : TImageExportDirectory;
    i1   : integer;
    s1   : AnsiString;
    aon  : array of cardinal;
    aof  : array of cardinal;
    aono : array of word;
    pi1  : TPInteger;
    c1   : cardinal;
    b1   : boolean;
begin
  result := false;
  if CheckValid then begin
    InitUnprotectMemory;
    pexp := GetImageExportDirectory;
    if pexp <> nil then
      if (FProcess.ID = GetCurrentProcessID) or (OS.win9x and (FHandle >= $80000000)) then begin
        try
          with pexp^ do
            for i1 := 0 to NumberOfNames - 1 do
              if PAnsiChar(FHandle + TPACardinal(FHandle + AddressOfNames)^[i1]) = name then begin
                pi1 := @TPAInteger(FHandle + AddressOfFunctions)^[TPAWord(FHandle + cardinal(exp.AddressOfNameOrdinals))^[i1]];
                b1 := IsMemoryProtected(pi1);
                if UnprotectMemory(pi1, 4) then begin
                  pi1^ := integer(newAddr) - integer(FHandle);
                  result := true;
                  if b1 then
                    ProtectMemory(pi1, 4);
                end;
                break;
              end;
        except end;
      end else
        with FProcess, GetHandle(PROCESS_VM_READ) do
          if IsValid then begin
            if ReadMemory(pexp^, exp, sizeOf(TImageExportDirectory)) then
              with exp do begin
                SetLength(aon,  NumberOfNames    );
                SetLength(aono, NumberOfNames    );
                SetLength(aof,  NumberOfFunctions);
                if ReadMemory(pointer(FHandle + AddressOfNames       )^, pointer(aon )^, NumberOfNames     * 4) and
                   ReadMemory(pointer(FHandle + AddressOfNameOrdinals)^, pointer(aono)^, NumberOfNames     * 2) and
                   ReadMemory(pointer(FHandle + AddressOfFunctions   )^, pointer(aof )^, NumberOfFunctions * 4) then begin
                  SetLength(s1, MAX_PATH + 1);
                  for i1 := 0 to NumberOfNames - 1 do
                    if ReadMemory(PAnsiChar(FHandle + aon[i1])^, pointer(s1)^, MAX_PATH) and
                       (PAnsiChar(s1) = name) then begin
                      pi1 := @TPAInteger(FHandle + AddressOfFunctions)^[aono[i1]];
                      newAddr := pointer(integer(newAddr) - integer(FHandle));
                      result := VirtualProtectEx(Handle, pi1, 4, PAGE_EXECUTE_READWRITE, @c1) and
                                WriteMemory(newAddr, pi1, 4);
                      break;
                    end;
                end;
              end;
          end else self.SetLastError(LastErrorNo, LastErrorStr);
  end;
end;

function TIModule.SetProcAddress(index: integer; newAddr: pointer) : boolean;
var pexp : PImageExportDirectory;
    exp  : TImageExportDirectory;
    pi1  : TPInteger;
    c1   : cardinal;
    idx  : integer absolute index;
    b1   : boolean;
begin
  result := false;
  if CheckValid then begin
    InitUnprotectMemory;
    pexp := GetImageExportDirectory;
    if pexp <> nil then
      if (FProcess.ID = GetCurrentProcessID) or (OS.win9x and (FHandle >= $80000000)) then begin
        try
          with pexp^ do begin
            dec(index, Base);
            if (index >= 0) and (index < NumberOfFunctions) then begin
              pi1 := @TPAInteger(FHandle + AddressOfFunctions)^[index];
              b1 := IsMemoryProtected(pi1);
              if UnprotectMemory(pi1, 4) then begin
                pi1^ := integer(newAddr) - integer(FHandle);
                result := true;
                if b1 then
                  ProtectMemory(pi1, 4);
              end;
            end;
          end;
        except end;
      end else
        with FProcess, GetHandle(PROCESS_VM_READ) do
          if IsValid then begin
            if ReadMemory(pexp^, exp, sizeOf(TImageExportDirectory)) then
              with exp do begin
                dec(idx, Base);
                if (idx >= 0) and (idx < NumberOfFunctions) then begin
                  pi1 := @TPAInteger(FHandle + AddressOfFunctions)^[idx];
                  newAddr := pointer(integer(newAddr) - integer(FHandle));
                  result := VirtualProtectEx(Handle, pi1, 4, PAGE_EXECUTE_READWRITE, @c1) and
                            WriteMemory(newAddr, pi1, 4);
                end;
              end;
          end else self.SetLastError(LastErrorNo, LastErrorStr);
  end;
end;

function TIModule.GetImportList : IXxportList;
const IMAGE_ORDINAL_FLAG = $80000000;
var pimp : PImageImportDirectory;
    imp  : TImageImportDirectory;
    pp1  : TPPointer;
    p1   : pointer;
    xl   : IXxportList;
    il   : TIXxportList;
    xe   : IExportEntry;
    b1   : boolean;
begin
  if FImportList = nil then
    if CheckValid then begin
      FImportList := FindXxportList(true, FFileName, FHandle);
      if FImportList = nil then begin
        pimp := GetImageImportDirectory;
        if pimp <> nil then begin
          xl := FProcess.ExportList;
          try
            il := TIXxportList.Create(true, 0, '');
            FImportList := il;
            b1 := (FProcess.ID = GetCurrentProcessID) or (OS.win9x and (FHandle >= $80000000));
            while (pimp <> nil) and
                  FProcess.ReadMemory(pimp^, imp, sizeOf(TImageImportDirectory)) and
                  (imp.Name_ <> 0) do begin
              pp1 := pointer(FHandle + imp.ThunkArray);
              if (pp1 <> nil) and (imp.ThunkArray <> 0) then
                while true do begin
                  if b1 then p1 := pp1^
                  else       FProcess.ReadMemory(pp1^, p1, 4);
                  if p1 = nil then break;
                  xe := xl.FindItem(p1);
                  if xe.IsValid then il.AddItem(xe)
                  else               il.NewItem(FProcess.ID, '?', 0, 0, '?', p1);
                  inc(pp1);
                end;
              inc(pimp);
            end;
          except FImportList := nil end;
        end;
        if FImportList <> nil then
             AddXxportList(FImportList, true, FFileName, FHandle)
        else FImportList := TIXxportList.Create(false, ERROR_FILE_NOT_FOUND, '');
      end;
    end else FImportList := TIXxportList.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)));
  result := FImportList;
end;

function TIModule.GetExportList : IXxportList;
var pexp   : PImageExportDirectory;
    exp    : TImageExportDirectory;
    aon    : array of cardinal;
    aof    : array of cardinal;
    aono   : array of word;
    i1, i2 : integer;
    b1     : boolean;
    xl     : TIXxportList;
    c1     : cardinal;
    s1     : AnsiString;
begin
  if FExportList = nil then
    if CheckValid then begin
      FExportList := FindXxportList(false, FFileName, FHandle);
      if FExportList = nil then begin
        pexp := GetImageExportDirectory;
        if pexp <> nil then
          if (FProcess.ID = GetCurrentProcessID) or (OS.win9x and (FHandle >= $80000000)) then begin
            try
              with pexp^ do begin
                xl := TIXxportList.Create(true, 0, '');
                FExportList := xl;
                for i1 := 0 to NumberOfFunctions - 1 do begin
                  c1 := TPACardinal(FHandle + AddressOfFunctions)^[i1];
                  if c1 > 0 then begin
                    b1 := true;
                    for i2 := 0 to NumberOfNames - 1 do
                      if TPAWord(FHandle + AddressOfNameOrdinals)^[i2] = i1 then begin
                        b1 := false;
                        xl.NewItem(FProcess.ID, FFileName, FHandle, i1 + integer(Base),
                                   PAnsiChar(FHandle + TPACardinal(FHandle + AddressOfNames)^[i2]),
                                   pointer(FHandle + c1));
                        break;
                      end;
                    if b1 then
                      xl.NewItem(FProcess.ID, FFileName, FHandle,
                                 i1 + integer(Base), '', pointer(FHandle + c1));
                  end;
                end;
              end;
            except FExportList := nil end;
          end else
            with FProcess, GetHandle(PROCESS_VM_READ) do
              if IsValid then begin
                if ReadMemory(pexp^, exp, sizeOf(TImageExportDirectory)) then
                  with exp do begin
                    SetLength(aon,  NumberOfNames    );
                    SetLength(aono, NumberOfNames    );
                    SetLength(aof,  NumberOfFunctions);
                    if ReadMemory(pointer(FHandle + AddressOfNames       )^, pointer(aon )^, NumberOfNames     * 4) and
                       ReadMemory(pointer(FHandle + AddressOfNameOrdinals)^, pointer(aono)^, NumberOfNames     * 2) and
                       ReadMemory(pointer(FHandle + AddressOfFunctions   )^, pointer(aof )^, NumberOfFunctions * 4) then begin
                      SetLength(s1, MAX_PATH + 1);
                      xl := TIXxportList.Create(true, 0, '');
                      FExportList := xl;
                      for i1 := 0 to NumberOfFunctions - 1 do begin
                        c1 := aof[i1];
                        if c1 > 0 then begin
                          b1 := true;
                          for i2 := 0 to NumberOfNames - 1 do
                            if (aono[i2] = i1) and
                               ReadMemory(PAnsiChar(FHandle + aon[i2])^, pointer(s1)^, MAX_PATH) then begin
                              b1 := false;
                              xl.NewItem(FProcess.ID, FFileName, FHandle,
                                         i1 + integer(Base), PAnsiChar(s1), pointer(FHandle + c1));
                              break;
                            end;
                          if b1 then
                            xl.NewItem(FProcess.ID, FFileName, FHandle,
                                       i1 + integer(Base), '', pointer(FHandle + c1));
                        end;
                      end;
                    end;
                  end;
              end else self.SetLastError(LastErrorNo, LastErrorStr);
        if FExportList <> nil then
             AddXxportList(FExportList, false, FFileName, FHandle)
        else FExportList := TIXxportList.Create(false, ERROR_FILE_NOT_FOUND, '');
      end;
    end else FExportList := TIXxportList.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)));
  result := FExportList;
end;

function TIModule.GetEntryPoint : pointer;
var pinh : PImageNtHeaders;
    inh  : TImageNtHeaders;
begin
  if FEntryPoint = nil then
    if CheckValid then begin
      pinh := GetImageNtHeaders;
      if pinh <> nil then begin
        if (FProcess.ID = GetCurrentProcessID) or (OS.win9x and (FHandle >= $80000000)) then begin
          try
            FEntryPoint := pointer(FHandle + pinh^.OptionalHeader.AddressOfEntryPoint);
          except end;
        end else
          with FProcess, GetHandle(PROCESS_VM_READ) do
            if IsValid then begin
              if ReadMemory(pinh^, inh, sizeOf(TImageNtHeaders)) then
                FEntryPoint := pointer(FHandle + inh.OptionalHeader.AddressOfEntryPoint);
            end else self.SetLastError(LastErrorNo, LastErrorStr);
        if FEntryPoint = nil then
          SetLastError(ERROR_FILE_NOT_FOUND);
      end;
    end;
  result := FEntryPoint;
end;

function TIModule.GetImageNtHeaders : PImageNtHeaders;
var w1 : word;
    c1 : cardinal;
begin
  if FImageHeaders = nil then
    if CheckValid then begin
      if (FProcess.ID = GetCurrentProcessID) or (OS.win9x and (FHandle >= $80000000)) then begin
        FImageHeaders := madTools.GetImageNtHeaders(FHandle);
      end else
        with FProcess, GetHandle(PROCESS_VM_READ) do
          if IsValid then begin
            if ReadMemory(pointer(FHandle)^, w1, 2) and (w1 = CEMAGIC) and
               ReadMemory(pointer(FHandle + CENEWHDR)^, c1, 4) then begin
              FImageHeaders := pointer(FHandle + c1);
              if (not ReadMemory(FImageHeaders^.signature, c1, 4)) or
                 (c1 <> CPEMAGIC) then
                FImageHeaders := nil;
            end;
          end else self.SetLastError(LastErrorNo, LastErrorStr);
      if FImageHeaders = nil then
        SetLastError(ERROR_FILE_NOT_FOUND);
    end;
  result := FImageHeaders;
end;

function TIModule.GetImageImportDirectory : PImageImportDirectory;
var pinh : PImageNtHeaders;
    inh  : TImageNtHeaders;
begin
  if FImageImport = nil then
    if CheckValid then begin
      pinh := GetImageNtHeaders;
      if pinh <> nil then begin
        if (FProcess.ID = GetCurrentProcessID) or (OS.win9x and (FHandle >= $80000000)) then begin
          try
            FImageImport := pointer(FHandle +
              pinh^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress);
          except end;
        end else
          with FProcess, GetHandle(PROCESS_VM_READ) do
            if IsValid then begin
              if ReadMemory(pinh^, inh, sizeOf(TImageNtHeaders)) then
                FImageImport := pointer(FHandle +
                  inh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress);
            end else self.SetLastError(LastErrorNo, LastErrorStr);
        if FImageImport = nil then
          SetLastError(ERROR_FILE_NOT_FOUND);
      end;
    end;
  result := FImageImport;
end;

function TIModule.GetImageExportDirectory : PImageExportDirectory;
var pinh : PImageNtHeaders;
    inh  : TImageNtHeaders;
begin
  if FImageExport = nil then
    if CheckValid then begin
      pinh := GetImageNtHeaders;
      if pinh <> nil then begin
        if (FProcess.ID = GetCurrentProcessID) or (OS.win9x and (FHandle >= $80000000)) then begin
          try
            FImageExport := pointer(FHandle +
              pinh^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress);
          except end;
        end else
          with FProcess, GetHandle(PROCESS_VM_READ) do
            if IsValid then begin
              if ReadMemory(pinh^, inh, sizeOf(TImageNtHeaders)) then
                FImageExport := pointer(FHandle +
                  inh.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress);
            end else self.SetLastError(LastErrorNo, LastErrorStr);
        if FImageExport = nil then
          SetLastError(ERROR_FILE_NOT_FOUND);
      end;
    end;
  result := FImageExport;
end;

function TIModule.GetMaxInterface : IBasic;
begin
  result := IModule(self);
end;

function Module(handle    : cardinal;
                autoClose : boolean = false) : IModule;
begin
  result := TIModule.Create(true, 0, '', CurrentProcess, handle, '', autoClose);
end;

function Module(memory    : pointer;
                autoClose : boolean = false) : IModule;
var mbi : TMemoryBasicInformation;
begin
  if (VirtualQueryEx(windows.GetCurrentProcess, memory, mbi, sizeOf(mbi)) = sizeOf(mbi)) and
     (mbi.State = MEM_COMMIT) and (mbi.AllocationBase <> nil) then 
    memory := mbi.AllocationBase;
  result := TIModule.Create(true, 0, '', CurrentProcess, cardinal(memory), '', autoClose);
end;

function Module(fileName  : AnsiString;
                autoClose : boolean = false) : IModule;
begin
  result := TIModule.Create(true, 0, '', CurrentProcess, 0, fileName, autoClose);
end;

function CurrentModule : IModule;
begin
  result := Module(SysInit.HInstance);
end;

function MainModule : IModule;
begin
  result := Module(GetModuleHandle(nil));
end;

function LoadModule(fileName          : AnsiString;
                    autoClose         : boolean = true;
                    withoutReferences : boolean = false;
                    onlyAsDataFile    : boolean = false;
                    alteredSearchPath : boolean = false) : IModule;
var c1 : cardinal;
begin
  c1 := 0;
  if withoutReferences then c1 := c1 or DONT_RESOLVE_DLL_REFERENCES;
  if onlyAsDataFile    then c1 := c1 or LOAD_LIBRARY_AS_DATAFILE;
  if alteredSearchPath then c1 := c1 or LOAD_WITH_ALTERED_SEARCH_PATH;
  c1 := LoadLibraryExA(PAnsiChar(fileName), 0, c1);
  if c1 = 0 then
       result := TIModule.Create(false, c1, '', nil, 0, '', false)
  else result := TIModule.Create(true, 0, '', CurrentProcess, c1, '', autoClose);
end;

// ***************************************************************

type
  // contains a list of specific IModule objects
  TIModules = class (TICustomBasicList, IModules)
  public
    FAll      : boolean;
    FProcess  : IProcess;
    FFileName : AnsiString;
    FOnlyName : boolean;
    FList     : boolean;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        const process: IProcess; fileName: AnsiString; const objects: array of IBasic);

    function GetOwnerProcess : IProcess;

    function GetFileName : AnsiString;

    function GetItem (index: integer) : IModule;

    function RefreshItems : boolean;

    function GetMaxInterface : IBasic; override;
  end;

type
  // types for Module32First/Next
  TModuleEntry32 = record
    size         : cardinal;
    module       : cardinal;  // this module (ID)
    ownerProcess : cardinal;  // owning process (ID)
    GlobalUsage  : cardinal;  // global usage count on the module
    ProcessUsage : cardinal;  // module usage count in ownerProcess' context
    baseAddress  : pointer;   // base address of module in ownerProcess' context
    baseSize     : cardinal;  // size in bytes of module starting at baseAddress
    handle       : cardinal;  // the handle of this module in ownerProcess' context
    fileName     : array [0..255         ] of AnsiChar;
    exePath      : array [0..MAX_PATH - 1] of AnsiChar;
  end;

var
  // enumeration of modules in win9x
  Module32First : function (snap: cardinal; var me: TModuleEntry32) : LongBool stdcall = nil;
  Module32Next  : function (snap: cardinal; var me: TModuleEntry32) : LongBool stdcall = nil;

const
  // constants for CreateToolhelp32Snapshot
  TH32CS_SnapModule = 8;

constructor TIModules.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                             const process: IProcess; fileName: AnsiString; const objects: array of IBasic);
var i1 : integer;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FAll      := process = nil;
    FProcess  := process;
    FFileName := fileName;
    FOnlyName := ExtractFilePath(string(FFileName)) = '';
    FList     := Length(objects) > 0;
    if FList then
      for i1 := 0 to high(objects) do
        if objects[i1] <> nil then
          AddItem(objects[i1]);
    if not FList then RefreshItems;
  end;
end;

function TIModules.GetOwnerProcess : IProcess;
begin
  if FProcess = nil then
    FProcess := TIProcess.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), otProcess, 0);
  result := FProcess;
end;

function TIModules.GetFileName : AnsiString;
begin
  result := FFileName;
end;

function TIModules.GetItem(index: integer) : IModule;
begin
  if (index < 0) or (index >= FCount) then
       result := TIModule.Create(false, CErrorNo_IndexOutOfRange, UnicodeString(AnsiString(CErrorStr_IndexOutOfRange)), nil, 0, '', false)
  else result := IModule(FItems[index]);
end;

function TIModules.RefreshItems : boolean;

  procedure RefreshItem(const ownerProcess: IProcess; handle: cardinal; fileName: AnsiString);
  var s1, s2 : AnsiString;
      i1     : integer;
      im1    : TIModule;
  begin
    if FFileName <> '' then begin
      s1 := FFileName;
      s2 := fileName;
      if      FOnlyName                        then s2 := AnsiString(ExtractFileName(string(s2)))
      else if ExtractFilePath(string(s2)) = '' then s1 := AnsiString(ExtractFileName(string(s1)));
    end;
    if (FFileName = '') or IsTextEqual(s1, s2) then begin
      for i1 := 0 to FCount - 1 do
        with TIModule(FItems[i1].SelfAsTObject) do
          if (FProcess.ID = ownerProcess.ID) and (FHandle = handle) and IsTextEqual(FFileName, fileName) then begin
            self.FItemInfos[i1].LastChangeType := lctUnChanged;
            exit;
          end;
      im1 := TIModule.Create(true, 0, '', ownerProcess, handle, fileName, false);
      if AddItem(IModule(im1)) = -1 then
        im1.Destroy;
    end;
  end;

var i1      : integer;
    mi      : TNtModuleInfo;
    loopEnd : pointer;
    ws      : UnicodeString;
    c1      : cardinal;
    me      : TModuleEntry32;
begin
  result := false;
  if CheckValid then begin
    BeginRefresh;
    try
      if OS.win9x then begin
        if @Module32First = nil then begin
          c1 := GetModuleHandle(kernel32);
          CreateToolhelp32Snapshot := GetProcAddress(c1, 'CreateToolhelp32Snapshot');
          Module32First            := GetProcAddress(c1, 'Module32First'           );
          Module32Next             := GetProcAddress(c1, 'Module32Next'            );
        end;
        if FAll then begin
          with Processes do
            for i1 := 0 to ItemCount - 1 do begin
              c1 := CreateToolHelp32Snapshot(TH32CS_SnapModule, Items[i1].ID);
              if c1 <> INVALID_HANDLE_VALUE then begin
                try
                  me.size := sizeOf(TModuleEntry32);
                  if Module32First(c1, me) then
                    repeat
                      RefreshItem(Items[i1], me.handle, me.fileName);
                    until not Module32Next(c1, me);
                finally CloseHandle(c1) end;
              end else SetLastError(GetLastError);
            end;
        end else begin
          c1 := CreateToolHelp32Snapshot(TH32CS_SnapModule, FProcess.ID);
          if c1 <> INVALID_HANDLE_VALUE then begin
            try
              me.size := sizeOf(TModuleEntry32);
              if Module32First(c1, me) then
                repeat
                  RefreshItem(FProcess, me.handle, me.fileName);
                until not Module32Next(c1, me);
            finally CloseHandle(c1) end;
          end else SetLastError(GetLastError);
        end;
      end else
        if FAll then begin
          with Processes do
            for i1 := 0 to ItemCount - 1 do
              with Items[i1].GetHandle(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ) do
                if IsValid then begin
                  if FirstModuleInfoNt(Handle, mi, loopEnd) then
                    repeat
                      if ModuleInfoNameNt(Handle, mi, ws) then
                        RefreshItem(Items[i1], mi.handle, AnsiString(ws));
                    until (mi.next = loopEnd) or (not NextModuleInfoNt(Handle, mi));
                end;
        end else
          with FProcess.GetHandle(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ) do
            if IsValid then begin
              if FirstModuleInfoNt(Handle, mi, loopEnd) then
                repeat
                  if ModuleInfoNameNt(Handle, mi, ws) then
                    RefreshItem(FProcess, mi.handle, AnsiString(ws));
                until (mi.next = loopEnd) or (not NextModuleInfoNt(Handle, mi));
            end else self.SetLastError(LastErrorNo, LastErrorStr);
      result := true;
    except end;
    EndRefresh;
  end;
end;

function TIModules.GetMaxInterface : IBasic;
begin
  result := IModules(self);
end;

function Modules(systemWide: boolean = false) : IModules;
begin
  if systemWide then
       result := TIModules.Create(true, 0, '', nil,            '', [])
  else result := TIModules.Create(true, 0, '', CurrentProcess, '', []);
end;

function Modules(fileName: AnsiString) : IModules;
begin
  result := TIModules.Create(true, 0, '', nil, fileName, []);
end;

function PickModules(const objects: array of IBasic) : IModules;
begin
  result := TIModules.Create(true, 0, '', nil, '', PickInterfaces(objects, TIModule));
end;

function PickModules(const objects: ICustomBasicList) : IModules;
begin
  if (objects <> nil) and objects.IsValid then begin
    result := TIModules.Create(true, 0, '', nil, '',
                PickInterfaces(TICustomBasicList(objects.SelfAsTObject).FItems, TIModule));
  end else
    result := TIModules.Create(false, ERROR_INVALID_PARAMETER, '', nil, '', []);
end;

// ***************************************************************

type
  // types for Thread32First/Next
  TThreadEntry32 = record
    size          : cardinal;
    usage         : cardinal;
    thread        : cardinal;  // this thread (ID)
    ownerProcess  : cardinal;  // process (ID) this thread is associated with
    basePriority  : integer;
    deltaPriority : integer;
    flags         : cardinal;
  end;

  // types for NewThreadThreadProc
  TNewThreadRec = record
    threadFunc : TThreadFunc;
    parameter  : pointer;
  end;
  TPNewThreadRec = ^TNewThreadRec;

var
  // not available in all systems
  GetThreadPriorityBoost  : function (th             : cardinal;
                                      var disable    : longBool ) : longBool stdcall = nil;
  SetThreadPriorityBoost  : function (th             : cardinal;
                                      disable        : longBool ) : longBool stdcall = nil;
  SetThreadIdealProcessor : function (th             : cardinal;
                                      processor      : cardinal ) : cardinal stdcall = nil;
  GetThreadTimes          : function (th             : cardinal;
                                      var creation   : int64;
                                      var exitTime   : int64;
                                      var kernelTime : int64;
                                      var userTime   : int64    ) : longBool stdcall = nil;
  OpenThread              : function (access         : cardinal;
                                      inheritHandle  : bool;
                                      threadID       : cardinal ) : cardinal stdcall = nil;
  NtOpenThread            : function (var hThread    : cardinal;
                                      access         : cardinal;
                                      objAttr        : pointer;
                                      clientID       : pointer  ) : cardinal stdcall = nil;

  // enumeration of threads in win9x
  Thread32First : function (snap: cardinal; var te: TThreadEntry32) : LongBool stdcall = nil;
  Thread32Next  : function (snap: cardinal; var te: TThreadEntry32) : LongBool stdcall = nil;

const
  // constants for CreateToolhelp32Snapshot
  TH32CS_SnapThread = 4;

function TIThread.IsStillValid : boolean;
var i1 : integer;
begin
  if FValid and ((FHandle = nil) or (not FHandle.IsValid)) then begin
    FValid := false;
    with GetOwnerProcess do
      if IsValid then
        with Threads do
          for i1 := 0 to ItemCount - 1 do
            if Items[i1].ID = FID then begin
              FValid := true;
              break;
            end;
    if not FValid then DelKernelObj(self);
  end;
  result := FValid;
end;

function TIThread.GetHandle : IHandle;
begin
  if CheckValid then begin
    result := GetHandle2;
    if not result.IsValid then
      result := GetHandle2(THREAD_QUERY_INFORMATION or SYNCHRONIZE);
  end else begin
    if FHandle = nil then
      FHandle := TIHandle.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)),
                                 0, 0, false, nil, nil, otUnknown);
    result := FHandle;
  end;
end;

function TIThread.GetHandle2(access: cardinal = THREAD_ALL_ACCESS) : IHandle;

  function IsThreadValid(thread: cardinal) : boolean;
  var c1 : cardinal;
      te : TThreadEntry32;
  begin
    result := false;
    if @Thread32First = nil then begin
      c1 := GetModuleHandle(kernel32);
      CreateToolhelp32Snapshot := GetProcAddress(c1, 'CreateToolhelp32Snapshot');
      Thread32First            := GetProcAddress(c1, 'Thread32First'           );
      Thread32Next             := GetProcAddress(c1, 'Thread32Next'            );
    end;
    c1 := CreateToolHelp32Snapshot(TH32CS_SnapThread, 0);
    if c1 <> INVALID_HANDLE_VALUE then
      try
        te.size := sizeOf(TThreadEntry32);
        if Thread32First(c1, te) then
          repeat
            if te.thread = thread then begin
              result := true;
              break;
            end;
          until not Thread32Next(c1, te);
      finally CloseHandle(c1) end;
  end;

const CNtObjAttr : array [0..5] of cardinal = ($18, 0, 0, 0, 0, 0);
var c1, c2, c3, c4 : cardinal;
    oldObjAddr     : pointer;
    clientID       : array [0..1] of cardinal;
begin
  if CheckValid then begin
    if (FHandle = nil) or (FHandle.Access and access <> access) or
       (FHandle.OwnerProcess.ID <> GetCurrentProcessID) then begin
      c4 := 0;
      if @OpenThread = nil then begin
        OpenThread := GetProcAddress(GetModuleHandle(kernel32), 'OpenThread');
        if @OpenThread = nil then
          OpenThread := pointer(1);
      end;
      if dword(@OpenThread) > 1 then begin
        c4 := OpenThread(access, true, GetID);
        if c4 = 0 then
          SetLastError(GetLastError);
      end else
        if OS.win9x then begin
          if (FHandle <> nil) or IsThreadValid(GetID) then begin
            try
              c1 := windows.GetCurrentProcess;
              if DuplicateHandle(c1, c1, c1, @c2, 0, false, DUPLICATE_SAME_ACCESS) then begin
                try
                  if Magic95 then c3 := c2
                  else            c3 := c2 div 4;
                  with TP9xPid(GetCurrentProcessID xor Magic)^.handleTable^.items[c3] do begin
                    oldObjAddr := objAddr;
                    objAddr := FAddr;
                  end;
                  if not DuplicateHandle(c1, c2, c1, @c4, access, true, 0) then begin
                    c4 := 0;
                    SetLastError(GetLastError);
                  end;
                  TP9xPid(GetCurrentProcessID xor Magic)^.handleTable^.items[c3].objAddr := oldObjAddr;
                finally CloseHandle(c2) end;
              end else SetLastError(GetLastError);
            except end;
          end else SetLastError(ERROR_INVALID_PARAMETER);
        end else begin
          if @NtOpenThread = nil then
            NtOpenThread := GetProcAddress(GetModuleHandle(ntdll), 'NtOpenThread');
          if @NtOpenThread <> nil then begin
            clientID[0] := 0;
            clientID[1] := GetID;
            c1 := NtOpenThread(c4, access, @CNtObjAttr, @clientID);
            if c1 <> 0 then begin
              c4 := 0;
              SetLastError(c1 and $7FFFFFFF);
            end;
          end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
        end;
      result := TIHandle.Create(c4 <> 0, FLastErrorNo, GetLastErrorStr,
                                GetCurrentProcessID, c4, true, nil, nil, otThread);
      if result.IsValid and (OS.winNT or (FStore > 0)) then
        FHandle := result;
    end else result := FHandle;
  end else begin
    if FHandle = nil then
      FHandle := TIHandle.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)),
                                 0, 0, false, nil, nil, otUnknown);
    result := FHandle;
  end;
end;

procedure TIThread.StoreHandle;
begin
  inc(FStore);
end;

procedure TIThread.ReleaseHandle;
begin
  if FStore > 0 then dec(FStore);
  if FStore = 0 then FHandle := nil;
end;

function TIThread.GetOwnerProcess : IProcess;
var buf : array [0..6] of cardinal;
    c1  : cardinal;
begin
  if FProcess = nil then begin
    c1 := 0;
    if CheckValid then
      if OS.winNT then begin
        with GetHandle2(THREAD_QUERY_INFORMATION) do
          if IsValid then begin
            ZeroMemory(@buf, sizeOf(buf));
            if @NtQueryInformationThread = nil then
              NtQueryInformationThread := GetProcAddress(GetModuleHandle(ntdll), 'NtQueryInformationThread');
            if @NtQueryInformationThread <> nil then begin
              c1 := NtQueryInformationThread(Handle, ThreadBasicInformation, @buf, sizeOf(buf), nil);
              if c1 = 0 then c1 := buf[2]
              else           self.SetLastError(c1);
            end else self.SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
          end else self.SetLastError(LastErrorNo, LastErrorStr);
      end else
        try
          if Magic95 then
               c1 := cardinal(TP9xTid(FAddr)^.ownerProcess95) xor Magic
          else c1 := cardinal(TP9xTid(FAddr)^.ownerProcess98) xor Magic;
        except SetLastError(ERROR_INVALID_PARAMETER) end;
    if FSuccess then
         FProcess := TIProcess(AddKernelObj(otProcess, c1))
    else FProcess := TIProcess.Create(false, FLastErrorNo, GetLastErrorStr, otUnknown, 0);
  end;
  result := FProcess;
end;

function TIThread.GetWindows : IWindows;
begin
  result := TIWindows.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), self, FProcess, false, []);
end;

function TIThread.GetTaskbarWindows : IWindows;
begin
  result := TIWindows.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), self, FProcess, true, []);
end;

function TIThread.GetPriority : integer;
begin
  result := THREAD_PRIORITY_ERROR_RETURN;
  if CheckValid then
    with GetHandle2(THREAD_QUERY_INFORMATION) do
      if IsValid then begin
        result := GetThreadPriority(Handle);
        if result = THREAD_PRIORITY_ERROR_RETURN then
          self.SetLastError(GetLastError);
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

procedure TIThread.SetPriority(priority: integer);
begin
  if CheckValid then
    with GetHandle2(THREAD_SET_INFORMATION) do
      if IsValid then begin
        if not SetThreadPriority(Handle, priority) then
          self.SetLastError(GetLastError);
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIThread.GetPriorityBoost : boolean;
var b1 : longBool;
begin
  result := true;
  if CheckValid then begin
    if @GetThreadPriorityBoost = nil then
      GetThreadPriorityBoost :=
        GetProcAddress(GetModuleHandle(kernel32), 'GetThreadPriorityBoost');
    if @GetThreadPriorityBoost <> nil then begin
      with GetHandle2(THREAD_QUERY_INFORMATION) do
        if IsValid then begin
          if GetThreadPriorityBoost(Handle, b1) then
               result := b1
          else self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else self.SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
end;

procedure TIThread.SetPriorityBoost(priorityBoost: boolean);
begin
  if CheckValid then begin
    if @SetThreadPriorityBoost = nil then
      SetThreadPriorityBoost :=
        GetProcAddress(GetModuleHandle(kernel32), 'SetThreadPriorityBoost');
    if @SetThreadPriorityBoost <> nil then begin
      with GetHandle2(THREAD_SET_INFORMATION) do
        if IsValid then begin
          if not SetThreadPriorityBoost(Handle, priorityBoost) then
            self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
end;

function TIThread.SetAffinityMask(affinityMask    : cardinal;
                                  oldAffinityMask : TPCardinal = nil) : boolean;
var c1 : cardinal;
begin
  result := false;
  if oldAffinityMask <> nil then oldAffinityMask^ := 0;
  if CheckValid then
    with GetHandle2(THREAD_SET_INFORMATION) do
      if IsValid then begin
        c1 := SetThreadAffinityMask(Handle, affinityMask);
        result := c1 > 0;
        if result then begin
          if oldAffinityMask <> nil then oldAffinityMask^ := c1;
        end else self.SetLastError(GetLastError);
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIThread.SetIdealProcessor(processorNo     : cardinal;
                                    oldProcessorNo  : TPCardinal = nil) : boolean;
var c1 : cardinal;
begin
  result := false;
  if oldProcessorNo <> nil then oldProcessorNo^ := 0;
  if CheckValid then begin
    if @SetThreadIdealProcessor = nil then
      SetThreadIdealProcessor :=
        GetProcAddress(GetModuleHandle(kernel32), 'SetThreadIdealProcessor');
    if @SetThreadIdealProcessor <> nil then begin
      with GetHandle2(THREAD_SET_INFORMATION) do
        if IsValid then begin
          c1 := SetThreadIdealProcessor(Handle, processorNo);
          result := c1 <> maxCard;
          if result then begin
            if oldProcessorNo <> nil then oldProcessorNo^ := c1;
          end else self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
end;

function TIThread.PostMessage(msg    : cardinal;
                              wParam : integer = 0;
                              lParam : integer = 0) : boolean;
begin
  if CheckValid then begin
    result := PostThreadMessageA(GetID, msg, wParam, lParam);
    if not result then SetLastError(GetLastError);
  end else result := false;
end;

function TIThread.IsSuspended : boolean;
var c1 : cardinal;
begin
  result := false;
  if CheckValid then
    with GetHandle2(THREAD_SUSPEND_RESUME) do
      if IsValid then begin
        c1 := SuspendThread(Handle);
        if c1 <> maxCard then begin
          ResumeThread(Handle);
          result := c1 > 0;
        end else self.SetLastError(GetLastError);
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIThread.Suspend(oldSuspendCount: TPCardinal = nil) : boolean;
var c1 : cardinal;
begin
  result := false;
  if oldSuspendCount <> nil then oldSuspendCount^ := 0;
  if CheckValid then
    with GetHandle2(THREAD_SUSPEND_RESUME) do
      if IsValid then begin
        c1 := SuspendThread(Handle);
        result := c1 <> maxCard;
        if result then begin
          if oldSuspendCount <> nil then oldSuspendCount^ := c1;
        end else self.SetLastError(GetLastError);
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIThread.Resume(oldSuspendCount: TPCardinal = nil) : boolean;
var c1 : cardinal;
begin
  result := false;
  if oldSuspendCount <> nil then oldSuspendCount^ := 0;
  if CheckValid then
    with GetHandle2(THREAD_SUSPEND_RESUME) do
      if IsValid then begin
        c1 := ResumeThread(Handle);
        result := c1 <> maxCard;
        if result then begin
          if oldSuspendCount <> nil then oldSuspendCount^ := c1;
        end else self.SetLastError(GetLastError);
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIThread.GetContext : TContext;
begin
  ZeroMemory(@result, sizeOf(TContext));
  if CheckValid then
    with GetHandle2(THREAD_GET_CONTEXT) do
      if IsValid then begin
        result.ContextFlags := CONTEXT_CONTROL or CONTEXT_INTEGER or CONTEXT_SEGMENTS or
                               CONTEXT_FLOATING_POINT or CONTEXT_DEBUG_REGISTERS;
        if not GetThreadContext(Handle, PContext(@result)^) then
          self.SetLastError(GetLastError);
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

procedure TIThread.SetContext(const context: TContext);
begin
  if CheckValid then
    with GetHandle2(THREAD_SET_CONTEXT) do
      if IsValid then begin
        if not SetThreadContext(Handle, PContext(@context)^) then 
          self.SetLastError(GetLastError);
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIThread.AttachInput(thread: cardinal = 0) : boolean;
begin
  result := false;
  if CheckValid then begin
    if thread <> 0 then begin
      with Handle(thread, false) do
        if IsValid and (ObjType = otThread) then
          thread := KernelObj.ID;
    end else thread := GetCurrentThreadID;
    result := AttachThreadInput(GetID, thread, true);
    if not result then SetLastError(0);  // no error info available...
  end;
end;

function TIThread.AttachInput(const thread: IHandle) : boolean;
begin
  result := false;
  if CheckValid then
    if (thread <> nil) and thread.IsValid and (thread.ObjType = otThread) then begin
      result := AttachThreadInput(GetID, thread.KernelObj.ID, true);
      if not result then SetLastError(0);  // no error info available...
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIThread.AttachInput(const thread: IThread) : boolean;
begin
  result := false;
  if CheckValid then
    if (thread <> nil) and thread.IsValid then begin
      result := AttachThreadInput(GetID, thread.ID, true);
      if not result then SetLastError(0);  // no error info available...
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIThread.DetachInput(thread: cardinal = 0) : boolean;
begin
  result := false;
  if CheckValid then begin
    if thread <> 0 then begin
      with Handle(thread, false) do
        if IsValid and (ObjType = otThread) then
          thread := KernelObj.ID;
    end else thread := GetCurrentThreadID;
    result := AttachThreadInput(GetID, thread, false);
    if not result then SetLastError(0);  // no error info available...
  end;
end;

function TIThread.DetachInput(const thread: IHandle) : boolean;
begin
  result := false;
  if CheckValid then
    if (thread <> nil) and thread.IsValid and (thread.ObjType = otThread) then begin
      result := AttachThreadInput(GetID, thread.KernelObj.ID, false);
      if not result then SetLastError(0);  // no error info available...
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIThread.DetachInput(const thread: IThread) : boolean;
begin
  result := false;
  if CheckValid then
    if (thread <> nil) and thread.IsValid then begin
      result := AttachThreadInput(GetID, thread.ID, false);
      if not result then SetLastError(0);  // no error info available...
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIThread.PostQuitMessage(exitCode: cardinal = 0) : boolean;
begin
  if CheckValid then begin
    result := PostThreadMessageA(GetID, WM_QUIT, exitCode, 0);
    if not result then SetLastError(GetLastError);
  end else result := false;
end;

function TIThread.IsStillRunning : boolean;
begin
  result := CheckValid and (WaitForSingleObject(GetHandle2(SYNCHRONIZE).Handle, 0) = WAIT_TIMEOUT);
end;

function TIThread.Terminate(exitCode: cardinal = 0) : boolean;
begin
  result := false;
  if CheckValid then
    with GetHandle2(THREAD_TERMINATE) do
      if IsValid then begin
        result := TerminateThread(Handle, exitCode);
        if not result then self.SetLastError(GetLastError);
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIThread.GetExitCode : cardinal;
begin
  result := maxCard;
  if CheckValid then
    with GetHandle2(THREAD_QUERY_INFORMATION) do
      if IsValid then begin
        if not GetExitCodeThread(Handle, result) then begin
          result := maxCard;
          self.SetLastError(GetLastError);
        end;
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIThread.GetTimes(var creation, exit, kernel, user: int64) : boolean;
begin
  result := false;
  if CheckValid then begin
    if @GetThreadTimes = nil then
      GetThreadTimes :=
        GetProcAddress(GetModuleHandle(kernel32), 'GetThreadTimes');
    if @GetThreadTimes <> nil then begin
      with GetHandle2(THREAD_QUERY_INFORMATION) do
        if IsValid then begin
          result := GetThreadTimes(Handle, creation, exit, kernel, user);
          if not result then
            self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
  if not result then begin
    creation := 0;
    exit     := 0;
    kernel   := 0;
    user     := 0;
  end;
end;

function TIThread.GetMaxInterface : IBasic;
begin
  result := IThread(self);
end;

function Thread(const thread: IHandle) : IThread;
begin
  if (thread <> nil) and thread.IsValid and (thread.ObjType = otThread) then
       result := TIThread(AddKernelObj(otThread, thread))
  else result := TIThread.Create(false, ERROR_INVALID_PARAMETER, '', otUnknown, 0);
end;

function Thread(thread    : cardinal;
                autoClose : boolean  = true) : IThread;
begin
  result := madKernel.Thread(Handle(thread, autoClose));
  if not result.IsValid then
    result := TIThread(AddKernelObj(otThread, thread));
end;

function CurrentThread : IThread;
begin
  result := TIThread(AddKernelObj(otThread, GetCurrentThreadID));
end;

function MainThread : IThread;
begin
  result := TIThread(AddKernelObj(otThread, MainThreadID));
end;

function NewThreadThreadProc(pntr: TPNewThreadRec) : cardinal; stdcall;
begin
  try
    result := maxCard;
    try
      result := pntr^.threadFunc(pntr^.parameter);
    except sysUtils.ShowException(ExceptObject, ExceptAddr) end;
  finally Dispose(pntr) end;
end;

function NewThread(threadFunc    : TThreadFunc;
                   parameter     : pointer             = nil;
                   creationFlags : cardinal            = 0;
                   stackSize     : cardinal            = 0;
                   threadAttr    : PSecurityAttributes = nil) : IThread;
var th, tid : cardinal;
    pntr    : TPNewThreadRec;
    it1     : TIThread;
begin
  IsMultiThread := true;  // make Delphi's memory manager is thread safe
  New(pntr);
  pntr^.threadFunc := threadFunc;
  pntr^.parameter  := parameter;
  th := CreateThread(threadAttr, stackSize, @NewThreadThreadProc, pntr, creationFlags, tid);
  if th = 0 then begin
    Dispose(pntr);
    result := TIThread.Create(false, GetLastError, '', otUnknown, 0);
  end else begin
    it1 := TIThread(AddKernelObj(otThread, Handle(th)));
    it1.FIDReady  := true;
    it1.FID       := tid;
    it1.FProcess  := CurrentProcess;
    result := it1;
  end;
end;

// ***************************************************************

constructor TIThreads.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                             const process: IProcess; const objects: array of IBasic);
var i1 : integer;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FAll := process = nil;
    FProcess := process;
    FList := Length(objects) > 0;
    if FList then
      for i1 := 0 to high(objects) do
        if objects[i1] <> nil then
          AddItem(objects[i1]);
    if not FList then RefreshItems;
  end;
end;

function TIThreads.GetOwnerProcess : IProcess;
begin
  result := FProcess;
end;

function TIThreads.GetItem(index: integer) : IThread;
begin
  if (index < 0) or (index >= FCount) then
       result := TIThread.Create(false, CErrorNo_IndexOutOfRange, UnicodeString(AnsiString(CErrorStr_IndexOutOfRange)), otThread, 0)
  else result := IThread(FItems[index]);
end;

function TIThreads.RefreshItems : boolean;

  procedure RefreshItem(tid: cardinal; const prc: IProcess);
  var i1  : integer;
      it1 : TIThread;
  begin
    for i1 := 0 to FCount - 1 do
      with TIThread(FItems[i1].SelfAsTObject) do
        if (GetID = tid) and ((FProcess = nil) or (FProcess.ID = prc.ID)) then begin
          if FProcess = nil then begin
            Change(FItems[i1], true, lctChanged, i1, i1);
            FProcess := prc;
            self.FItemInfos[i1].LastChangeType := lctChanged;
            Change(FItems[i1], false, lctChanged, i1, SortItem(i1));
          end else
            self.FItemInfos[i1].LastChangeType := lctUnChanged;
          exit;
        end;
    it1 := TIThread(AddKernelObj(otThread, tid));
    it1.FProcess := prc;
    if AddItem(IThread(it1)) = -1 then
      it1.Destroy;
  end;

var c1, c2 : cardinal;
    te     : TThreadEntry32;
    p1     : pointer;
    npi    : ^TNtProcessInfo;
    i1     : integer;
    ip1    : IProcess;
begin
  result := false;
  if CheckValid then
    if not FList then begin
      BeginRefresh;
      try
        if OS.win9x then begin
          if @Thread32First = nil then begin
            c1 := GetModuleHandle(kernel32);
            CreateToolhelp32Snapshot := GetProcAddress(c1, 'CreateToolhelp32Snapshot');
            Thread32First            := GetProcAddress(c1, 'Thread32First'           );
            Thread32Next             := GetProcAddress(c1, 'Thread32Next'            );
          end;
          c1 := CreateToolHelp32Snapshot(TH32CS_SnapThread, 0);
          if c1 <> INVALID_HANDLE_VALUE then begin
            try
              te.size := sizeOf(TThreadEntry32);
              if Thread32First(c1, te) then
                repeat
                  if FAll then begin
                    if (ip1 = nil) or (ip1.ID <> te.ownerProcess) then
                      ip1 := TIProcess(AddKernelObj(otProcess, te.ownerProcess));
                    RefreshItem(te.thread, ip1);
                  end else
                    if (FProcess.ID = te.ownerProcess) then
                      RefreshItem(te.thread, FProcess);
                until not Thread32Next(c1, te);
            finally CloseHandle(c1) end;
          end else SetLastError(GetLastError);
        end else begin
          if @NtQuerySystemInformation = nil then
            NtQuerySystemInformation := GetProcAddress(GetModuleHandle(ntdll), 'NtQuerySystemInformation');
          c1 := 0;
          NtQuerySystemInformation(5, nil, 0, @c1);
          p1 := nil;
          try
            if c1 = 0 then begin
              c1 := $10000;
              repeat
                c1 := c1 * 2;
                LocalFree(dword(p1));
                dword(p1) := LocalAlloc(LPTR, c1);
                c2 := NtQuerySystemInformation(5, p1, c1, nil);
              until (c2 = 0) or (c1 = $400000);
            end else begin
              c1 := c1 * 2;
              dword(p1) := LocalAlloc(LPTR, c1);
              c2 := NtQuerySystemInformation(5, p1, c1, nil);
            end;
            if c2 = 0 then begin
              npi := p1;
              while true do begin
                if FAll or (FProcess.ID = npi^.pid) then begin
                  if FAll then ip1 := TIProcess(AddKernelObj(otProcess, npi^.pid))
                  else         ip1 := FProcess;
                  for i1 := 0 to npi^.numThreads - 1 do begin
                    if OS.winNtEnum > osWinNt4 then c1 := npi^.threads[i1].tidNt5
                    else                            c1 := npi^.threads[i1].tidNt4;
                    RefreshItem(c1, ip1);
                  end;
                  if not FAll then break;
                end;
                if npi^.offset = 0 then break;
                npi := pointer(cardinal(npi) + npi^.offset);
              end;
            end else SetLastError(c2);
          finally LocalFree(dword(p1)) end;
        end;
      finally EndRefresh end;
    end else SetLastError(CErrorNo_RefreshListError, UnicodeString(AnsiString(CErrorStr_RefreshListError)));
end;

procedure TIThreads.SetPriority(priority: integer);
var i1 : integer;
begin
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      with IThread(FItems[i1]) do begin
        SetPriority(priority);
        FSuccess := FSuccess or Success;
      end;
end;

procedure TIThreads.SetPriorityBoost(priorityBoost: boolean);
var i1 : integer;
begin
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      with IThread(FItems[i1]) do begin
        SetPriorityBoost(priorityBoost);
        FSuccess := FSuccess or Success;
      end;
end;

function TIThreads.SetAffinityMask(affinityMask: cardinal) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IThread(FItems[i1]).SetAffinityMask(affinityMask) or result;
end;

function TIThreads.SetIdealProcessor(processorNo: cardinal) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IThread(FItems[i1]).SetIdealProcessor(processorNo) or result;
end;

function TIThreads.PostMessage(msg    : cardinal;
                               wParam : integer = 0;
                               lParam : integer = 0) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := PostThreadMessageA(IThread(FItems[i1]).ID, msg, wParam, lParam) or result;
end;

function TIThreads.Suspend : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IThread(FItems[i1]).Suspend or result;
end;

function TIThreads.Resume : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IThread(FItems[i1]).Resume or result;
end;

function TIThreads.PostQuitMessage(exitCode: cardinal = 0) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IThread(FItems[i1]).PostQuitMessage(exitCode) or result;
end;

function TIThreads.IsStillRunning(all: boolean = false) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do begin
      result := IThread(FItems[i1]).IsStillRunning;
      if result <> all then break;
    end;
end;

function TIThreads.Terminate(exitCode: cardinal = 0) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IThread(FItems[i1]).Terminate(exitCode) or result;
end;

function TIThreads.GetMaxInterface : IBasic;
begin
  result := IThreads(self);
end;

function Threads(systemWide: boolean = false) : IThreads;
begin
  if systemWide then result := TIThreads.Create(true, 0, '', nil,            [])
  else               result := TIThreads.Create(true, 0, '', CurrentProcess, []);
end;

function PickThreads(const objects: array of IBasic) : IThreads;
begin
  result := TIThreads.Create(true, 0, '', nil, PickInterfaces(objects, TIThread));
end;

function PickThreads(const objects: ICustomBasicList) : IThreads;
begin
  if (objects <> nil) and objects.IsValid then begin
    result := TIThreads.Create(true, 0, '', nil,
                PickInterfaces(TICustomBasicList(objects.SelfAsTObject).FItems, TIThread));
  end else
    result := TIThreads.Create(false, ERROR_INVALID_PARAMETER, '', nil, []);
end;

// ***************************************************************

const
  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;
  BELOW_NORMAL_PRIORITY_CLASS = $00004000;

type
  // types for alien thread injection
  TInjectRec = record
    event1      : cardinal;
    event2      : cardinal;
    command     : TInjectCommand;
    result      : boolean;
    lastError   : cardinal;
    params      : array [0..3] of cardinal;
    name        : array [0..MAX_PATH] of AnsiChar;
    cmdLine     : array [0..MAX_PATH] of AnsiChar;
    startupInfo : TStartupInfo;
  end;
  TInjectRecEx = record
    injectRec  : TInjectRec;
    injectCode : byte;
  end;
  TPInjectRecEx = ^TInjectRecEx;

  THijackThreadRec = record
    memSize : cardinal;  // in / memory size needed for the new thread
    memPtr  : pointer;   // out / memory location of the newly created thread code memory
    tid     : cardinal;  // out / thread id of the new thread
    ready   : boolean;   // out / am ready
  end;

var
  // not available in all systems
  GetProcessPriorityBoost   : function (ph           : cardinal;
                                        var disable  : longBool) : longBool stdcall = nil;
  SetProcessPriorityBoost   : function (ph           : cardinal;
                                        disable      : longBool) : longBool stdcall = nil;
  GetProcessWorkingSetSize  : function (ph           : cardinal;
                                        var minimum  : cardinal;
                                        var maximum  : cardinal) : longBool stdcall = nil;
  SetProcessWorkingSetSize  : function (ph           : cardinal;
                                        minimum      : cardinal;
                                        maximum      : cardinal) : longBool stdcall = nil;
  GetProcessTimes           : function (ph           : cardinal;
                                        var creation : int64;
                                        var exit     : int64;
                                        var kernel   : int64;
                                        var userTime : int64   ) : longBool stdcall = nil;
  SetProcessAffinityMask    : function (ph           : cardinal;
                                        mask         : cardinal) : longBool stdcall = nil;
  QueryFullProcessImageName : function (process      : THandle;
                                        flags        : dword;
                                        exeName      : PWideChar;
                                        var size     : dword   ) : longBool; stdcall = nil;


function GetProcessFileNameNt(handle: cardinal) : AnsiString;
var pbi  : TNtProcessBasicInfo;
    c1   : NativeUInt;
    dw1  : dword;
    pp   : pointer;
    ws   : UnicodeString;
    ustr : packed record
             len, maxLen : word;
             str         : pointer;
           end;
begin
  result := '?';
  if @QueryFullProcessImageName = nil then
    QueryFullProcessImageName := GetProcAddress(GetModuleHandle(kernel32), 'QueryFullProcessImageNameW');
  SetLength(ws, MAX_PATH);
  dw1 := MAX_PATH;
  if (@QueryFullProcessImageName <> nil) and QueryFullProcessImageName(handle, 0, PWideChar(ws), dw1) then
    result := AnsiString(UnicodeString(PWideChar(ws)))
  else begin
    if @NtQueryInformationProcess = nil then
      NtQueryInformationProcess := GetProcAddress(GetModuleHandle(ntdll), 'NtQueryInformationProcess');
    if (NtQueryInformationProcess(handle, ProcessBasicInformation, @pbi, sizeOf(TNtProcessBasicInfo), nil) = 0) and
       ReadProcessMemory(handle, pointer(dword(pbi.pebBaseAddress) + $10), @pp, 4, c1) and (c1 = 4) and
       ReadProcessMemory(handle, pointer(dword(pp) + 14 * 4), @ustr, 8, c1) and (c1 = 8) then begin
      SetLength(ws, ustr.len div 2);
      if ReadProcessMemory(handle, ustr.str, pointer(ws), ustr.len, c1) and (c1 = ustr.len) then
        result := AnsiString(ws);
    end;
  end;
end;

function TIProcess.IsStillValid : boolean;
var i1 : integer;
begin
  if FValid and ((FHandle = nil) or (not FHandle.IsValid)) then begin
    FValid := false;
    with Processes do
      for i1 := 0 to ItemCount - 1 do
        if Items[i1].ID = FID then begin
          FValid := true;
          break;
        end;
    if not FValid then DelKernelObj(self);
  end;
  result := FValid;
end;

function TIProcess.GetExeFile : AnsiString;
var i1 : integer;
begin
  if CheckValid and (FExeFile = '') then begin
    FExeFile := '?';
    if OS.win9x then begin
      with Processes do
        for i1 := 0 to ItemCount - 1 do
          with Items[i1] do
            if FID = GetID then begin
              FExeFile := ExeFile;
              break;
            end;
    end else
      with GetHandle2(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ) do
        if IsValid then
             FExeFile := GetProcessFileNameNt(Handle)
        else self.SetLastError(LastErrorNo, LastErrorStr);
    if FExeFile = '?' then SetLastError(ERROR_FILE_NOT_FOUND);
  end;
  result := FExeFile;
end;

function TIProcess.GetSession : dword;
var c1, c2 : dword;
begin
  if CheckValid and (not FSessionDone) and OS.winNt then
    with GetHandle2(PROCESS_QUERY_INFORMATION) do
      if IsValid then begin
        if @NtQueryInformationProcess = nil then
          NtQueryInformationProcess :=
            GetProcAddress(GetModuleHandle(ntdll), 'NtQueryInformationProcess');
        c2 := 0;
        c1 := NtQueryInformationProcess(Handle, ProcessSessionInformation, @c2, 4, nil);
        if c1 = 0 then begin
          FSessionDone := true;
          FSession := c2;
        end else
          self.SetLastError(c1);
      end else
        self.SetLastError(LastErrorNo, LastErrorStr);
  result := FSession;
end;

function TIProcess.GetHandle : IHandle;
begin
  if CheckValid then begin
    result := GetHandle2;
    if not result.IsValid then
      result := GetHandle2(PROCESS_QUERY_INFORMATION or SYNCHRONIZE);
  end else begin
    if FHandle = nil then
      FHandle := TIHandle.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)),
                                 0, 0, false, nil, nil, otUnknown);
    result := FHandle;
  end;
end;

function TIProcess.GetHandle2(access: cardinal = PROCESS_ALL_ACCESS) : IHandle;
var c1 : cardinal;
begin
  if CheckValid then begin
    if (FHandle = nil) or (FHandle.Access and access <> access) or
       (FHandle.OwnerProcess.ID <> GetCurrentProcessID) then begin
      c1 := OpenProcess(access, true, GetID);
      if c1 = 0 then begin
        SetLastError(GetLastError);
        result := TIHandle.Create(false, GetLastError, '', 0, 0, false, nil, nil, otUnknown);
      end else
        result := TIHandle.Create(true, 0, '', GetCurrentProcessID, c1, true, nil, nil, otProcess);
      if result.IsValid and (OS.winNT or (FStore > 0)) then
        FHandle := result;
    end else result := FHandle;
  end else begin
    if FHandle = nil then
      FHandle := TIHandle.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)),
                                 0, 0, false, nil, nil, otUnknown);
    result := FHandle;
  end;
end;

procedure TIProcess.StoreHandle;
begin
  inc(FStore);
end;

procedure TIProcess.ReleaseHandle;
begin
  if FStore > 0 then dec(FStore);
  if FStore = 0 then FHandle := nil;
end;

function TIProcess.GetParentProcess : IProcess;
var pbi : TNtProcessBasicInfo;
    c1  : cardinal;
begin
  if CheckValid and (FParentProcess = nil) then begin
    if OS.win9x then begin
      c1 := 0;
      try
        c1 := cardinal(TP9xPid(FAddr)^.parentProcess);
      except end;
      if c1 <> 0 then
           FParentProcess := TIProcess(AddKernelObj(otProcess, c1 xor Magic))
      else FParentProcess := TIProcess.Create(false, ERROR_FILE_NOT_FOUND, '', otUnknown, 0);
    end else
      with GetHandle2(PROCESS_QUERY_INFORMATION) do
        if IsValid then begin
          if @NtQueryInformationProcess = nil then
            NtQueryInformationProcess :=
              GetProcAddress(GetModuleHandle(ntdll), 'NtQueryInformationProcess');
          c1 := NtQueryInformationProcess(Handle, ProcessBasicInformation, @pbi,
                                          sizeOf(TNtProcessBasicInfo), nil);
          if c1 = 0 then
               FParentProcess := TIProcess(AddKernelObj(otProcess, pbi.ParentPid))
          else self.SetLastError(c1);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
  end;
  result := FParentProcess;
end;

function TIProcess.GetOsVersion : TOsVersion;
begin
  if CheckValid and (not FOsVersionReady) then begin
    FOsVersionReady := true;
    FOsVersion := TOsVersion(GetProcessVersion(GetID));
    if cardinal(FOsVersion) = 0 then
      SetLastError(GetLastError);
  end;
  result := FOsVersion;
end;

function TIProcess.GetExeType : TExeType;
begin
  if CheckValid and (not FExeTypeReady) then begin
    FExeTypeReady := true;
    FExeType := ExeType(GetExeFile);
  end;
  result := FExeType;
end;

function TIProcess.Icon(smallIcon             : boolean = true;
                        linkOverlayIfLnkOrPif : boolean = false) : cardinal;
begin
  if CheckValid then
       result := madKernel.Icon(GetExeFile, smallIcon, linkOverlayIfLnkOrPif)
  else result := 0;
end;

function TIProcess.GetHInstance : cardinal;
begin
  if CheckValid and (FHInstance = 0) then
    FHInstance := GetModules[0].Handle;
  result := FHInstance;
end;

function TIProcess.GetMainModule : IModule;
begin
  result := TIModule.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), self, GetHInstance, '', false);
end;

function TIProcess.GetServiceProcess : boolean;

  function IsSystemProcess(processHandle: dword) : boolean;
  var saa : AnsiString;
  begin
    result := true;
    saa := GetTokenInformation(dword(TokenUser));
    if (saa <> '') and (PSidAndAttributes(saa).Sid <> nil) and (TPAByte(PSidAndAttributes(saa).Sid)[1] > 1) then
      result := false;
  end;

var arrCh : array [0..MAX_PATH] of AnsiChar;
    pf    : T9xPidFlags;
begin
  result := false;
  if CheckValid then
    if OS.win9x then begin
      try
        pf := TP9xPid(FAddr)^.flags;
        result := pfServiceProcess in pf;
        FExeTypeReady := true;
        if      pfWin16Process   in pf then FExeType := etWin16
        else if pfDosProcess     in pf then FExeType := etDos
        else if pfConsoleProcess in pf then FExeType := etConsole
        else                                FExeType := etWin32;
        if not result then begin
          GetSystemDirectoryA(arrCh, MAX_PATH);
          if IsTextEqual(AnsiString(arrCh) + '\' + kernel32, GetExeFile) then
            result := true;
        end;
      except end;
    end else
      with GetHandle2(PROCESS_QUERY_INFORMATION) do
        if IsValid then
          result := IsSystemProcess(Handle)
        else
          self.SetLastError(LastErrorNo, LastErrorStr);
end;

procedure TIProcess.SetServiceProcess(serviceProcess: boolean = true);
begin
  if CheckValid then
    if OS.win9x then begin
      try
        with TP9xPid(FAddr)^ do
          if serviceProcess then Include(flags, pfServiceProcess)
          else                   Exclude(flags, pfServiceProcess);
      except end;
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TIProcess.GetPriorityClass : TPriorityClass;
begin
  result := pcNormal;
  if CheckValid then
    with GetHandle2(PROCESS_QUERY_INFORMATION) do
      if IsValid then begin
        case windows.GetPriorityClass(Handle) of
          0                           : begin
                                          result := pcUnknown;
                                          self.SetLastError(GetLastError);
                                        end;
          IDLE_PRIORITY_CLASS         : result := pcIdle;
          BELOW_NORMAL_PRIORITY_CLASS : result := pcBelowNormal;
          NORMAL_PRIORITY_CLASS       : result := pcNormal;
          ABOVE_NORMAL_PRIORITY_CLASS : result := pcAboveNormal;
          HIGH_PRIORITY_CLASS         : result := pcHigh;
          REALTIME_PRIORITY_CLASS     : result := pcRealTime;
          else                          begin
                                          result := pcUnknown;
                                          self.SetLastError(0);
                                        end;
        end;
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

procedure TIProcess.SetPriorityClass(priorityClass: TPriorityClass);
var b1 : boolean;
begin
  if CheckValid then
    if priorityClass in ([low(TPriorityClass)..high(TPriorityClass)] - [pcUnknown]) then begin
      with GetHandle2(PROCESS_SET_INFORMATION) do
        if IsValid then begin
          case priorityClass of
            pcIdle        : b1 := windows.SetPriorityClass(Handle, IDLE_PRIORITY_CLASS        );
            pcBelowNormal : b1 := windows.SetPriorityClass(Handle, BELOW_NORMAL_PRIORITY_CLASS);
            pcAboveNormal : b1 := windows.SetPriorityClass(Handle, ABOVE_NORMAL_PRIORITY_CLASS);
            pcHigh        : b1 := windows.SetPriorityClass(Handle, HIGH_PRIORITY_CLASS        );
            pcRealTime    : b1 := windows.SetPriorityClass(Handle, REALTIME_PRIORITY_CLASS    );
            else            b1 := windows.SetPriorityClass(Handle, NORMAL_PRIORITY_CLASS      );
          end;
          if not b1 then self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIProcess.GetPriorityBoost : boolean;
var b1 : longBool;
begin
  result := false;
  if CheckValid then begin
    if @GetProcessPriorityBoost = nil then
      GetProcessPriorityBoost :=
        GetProcAddress(GetModuleHandle(kernel32), 'GetProcessPriorityBoost');
    if @GetProcessPriorityBoost <> nil then begin
      with GetHandle2(PROCESS_QUERY_INFORMATION) do
        if IsValid then begin
          if GetProcessPriorityBoost(Handle, b1) then
               result := b1
          else self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
end;

procedure TIProcess.SetPriorityBoost(priorityBoost: boolean);
begin
  if CheckValid then begin
    if @SetProcessPriorityBoost = nil then
      SetProcessPriorityBoost :=
        GetProcAddress(GetModuleHandle(kernel32), 'SetProcessPriorityBoost');
    if @SetProcessPriorityBoost <> nil then begin
      with GetHandle2(PROCESS_SET_INFORMATION) do
        if IsValid then begin
          if not SetProcessPriorityBoost(Handle, priorityBoost) then
            self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
end;

function TIProcess.GetAffinityMask : cardinal;
var c1  : NativeUInt;
    res : NativeUInt;
begin
  result := 1;
  if CheckValid then
    with GetHandle2(PROCESS_QUERY_INFORMATION) do
      if IsValid then begin
        if not GetProcessAffinityMask(Handle, res, c1) then begin
          result := c1;
          self.SetLastError(GetLastError);
        end else
          result := res;
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

procedure TIProcess.SetAffinityMask(affinityMask: cardinal);
begin
  if CheckValid then begin
    if @SetProcessAffinityMask = nil then
      SetProcessAffinityMask :=
        GetProcAddress(GetModuleHandle(kernel32), 'SetProcessAffinityMask');
    if @SetProcessAffinityMask <> nil then begin
      with GetHandle2(PROCESS_SET_INFORMATION) do
        if IsValid then begin
          if not SetProcessAffinityMask(Handle, affinityMask) then
            self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
end;

procedure TIProcess.GetWorkingSetSize(var minimum, maximum: cardinal);
begin
  if CheckValid then begin
    if @GetProcessWorkingSetSize = nil then
      GetProcessWorkingSetSize :=
        GetProcAddress(GetModuleHandle(kernel32), 'GetProcessWorkingSetSize');
    if @GetProcessWorkingSetSize <> nil then begin
      with GetHandle2(PROCESS_QUERY_INFORMATION) do
        if IsValid then begin
          if not GetProcessWorkingSetSize(Handle, minimum, maximum) then
            self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
  if not FSuccess then begin
    minimum := 0;
    maximum := 0;
  end;
end;

function TIProcess.SetWorkingSetSize(minimum, maximum: cardinal) : boolean;
begin
  result := false;
  if CheckValid then begin
    if @SetProcessWorkingSetSize = nil then
      SetProcessWorkingSetSize :=
        GetProcAddress(GetModuleHandle(kernel32), 'SetProcessWorkingSetSize');
    if @SetProcessWorkingSetSize <> nil then begin
      with GetHandle2(PROCESS_SET_INFORMATION) do
        if IsValid then begin
          if SetProcessWorkingSetSize(Handle, minimum, maximum) then
               result := true
          else self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
end;

function TIProcess.GetTokenInformation(tokenLevel: dword) : AnsiString;
var token : THandle;
    size  : dword;
begin
  with GetHandle2(PROCESS_QUERY_INFORMATION) do
    if IsValid then begin
      if OpenProcessToken(Handle, TOKEN_QUERY, token) then begin
        size := 0;
        Windows.GetTokenInformation(token, TTokenInformationClass(tokenLevel), nil, 0, size);
        if size > 0 then begin
          SetLength(result, size * 2 + 16);
          if Windows.GetTokenInformation(token, TTokenInformationClass(tokenLevel), pointer(result), size * 2 + 16, size) then
            SetLength(result, size)
          else begin
            self.SetLastError(GetLastError);
            result := '';
          end;
        end else self.SetLastError(GetLastError);
        CloseHandle(token);
      end else self.SetLastError(GetLastError);
    end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIProcess.GetUserName : AnsiString;
var c1, c2, c3 : dword;
    s1, s2     : AnsiString;
begin
  result := GetTokenInformation(dword(TokenUser));
  if result <> '' then begin
    c1 := 0;
    c2 := 0;
    LookupAccountSid(nil, PSidAndAttributes(result).Sid, nil, c1, nil, c2, c3);
    SetLength(s1, c1);
    SetLength(s2, c2);
    if LookupAccountSidA(nil, PSidAndAttributes(result).Sid, PAnsiChar(s1), c1, PAnsiChar(s2), c2, c3) then
      result := PAnsiChar(s1)
    else
      self.SetLastError(GetLastError);
  end;
end;

function TIProcess.GetCommandLine : AnsiString;
var b1 : boolean;
begin
  if (FCommandLine = '') and CheckValid then begin
    b1 := true;
    if OS.win9x then
      try
        FCommandLine := TP9xPid(FID xor Magic)^.edb^.cmdLine;
        b1 := false;
      except end;
    if b1 then
      if FID = GetCurrentProcessID then
           FCommandLine := AnsiString(string(CmdLine))
      else InjectCommand(icGetCommandLine, [], @FCommandLine);
    if FCommandLine = '' then FCommandLine := '"' + GetExeFile + '"';
  end;
  result := FCommandLine;
end;

function TIProcess.GetStartupInfo : TStartupInfo;
var b1 : boolean;
begin
  if (FStartupInfo.cb = 0) and CheckValid then begin
    b1 := true;
    if OS.win9x then
      try
        FStartupInfo := TP9xPid(FID xor Magic)^.edb^.startupInfo^;
        b1 := false;
      except end;
    if b1 then
      if FID = GetCurrentProcessID then begin
        FStartupInfo.cb := sizeOf(TStartupInfo);
        windows.GetStartupInfo(FStartupInfo);
      end else
        InjectCommand(icGetStartupInfo, [], @FStartupInfo);
    if FStartupInfo.cb = 0 then begin
      FStartupInfo.cb := sizeOf(TStartupInfo);
      FStartupInfo.wShowWindow := SW_SHOWNORMAL;
    end;
  end;
  result := FStartupInfo;
end;

function TIProcess.AllocMem(size             : integer;
                            mayUseSharedArea : boolean = false) : pointer;
begin
  result := nil;
  if CheckValid then
    if OS.winNT then begin
      with GetHandle2(PROCESS_VM_OPERATION or PROCESS_QUERY_INFORMATION) do
        if IsValid then begin
          result := AllocMemEx(size, Handle);
          if result = nil then self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else
      if FID <> GetCurrentProcessID then begin
        if not mayUseSharedArea then
             InjectCommand(icAllocMem, [cardinal(size)], @result)
        else result := AllocMemEx(size);
      end else result := pointer(LocalAlloc(LPTR, size));
end;

function TIProcess.FreeMem(var ptr: pointer) : boolean;
begin
  result := false;
  if CheckValid then begin
    if OS.winNT then begin
      with GetHandle2(PROCESS_VM_OPERATION) do
        if IsValid then begin
          result := FreeMemEx(ptr, Handle);
          if not result then self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else
      if cardinal(ptr) < $80000000 then begin
        if FID <> GetCurrentProcessID then
             result := InjectCommand(icFreeMem, [cardinal(ptr)])
        else result := LocalFree(cardinal(ptr)) = 0;
      end else result := FreeMemEx(ptr);
    if result then ptr := nil;
  end;
end;

function TIProcess.ReadMemory(const source; var dest; count: integer) : boolean;
var c1 : NativeUInt;
begin
  result := false;
  if CheckValid then
    if count >= 0 then begin
      if count > 0 then begin
        with GetHandle2(PROCESS_VM_READ) do
          if IsValid then begin
            result := ReadProcessMemory(Handle, @source, @dest, count, c1) and
                      (integer(c1) = count);
            if not result then self.SetLastError(GetLastError);
          end else self.SetLastError(LastErrorNo, LastErrorStr);
      end else result := true;
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIProcess.WriteMemory(const source; var dest; count: integer) : boolean;
var c1 : NativeUInt;
begin
  result := false;
  if CheckValid then
    if count >= 0 then begin
      if count > 0 then begin
        with GetHandle2(PROCESS_VM_WRITE or PROCESS_VM_OPERATION) do
          if IsValid then begin
            result := WriteProcessMemory(Handle, @dest, @source, count, c1) and
                      (integer(c1) = count);
            if not result then self.SetLastError(GetLastError);
          end else self.SetLastError(LastErrorNo, LastErrorStr);
      end else result := true;
    end else SetLastError(ERROR_INVALID_PARAMETER)
end;

function TIProcess.ZeroMemory(var dest; count: integer) : boolean;
const CBufSize = 1024 * 10;
var c1 : NativeUInt;
    p1 : pointer;
    i1 : integer;
begin
  result := false;
  if CheckValid then
    if count >= 0 then begin
      if count > 0 then begin
        with GetHandle2(PROCESS_VM_WRITE or PROCESS_VM_OPERATION) do
          if IsValid then begin
            p1 := AllocMem(CBufSize);
            if p1 <> nil then
              try
                result := true;
                while result and (count > 0) do begin
                  if count > CBufSize then i1 := CBufSize
                  else                     i1 := count;
                  result := WriteProcessMemory(Handle, @dest, p1, i1, c1) and
                            (integer(c1) = i1);
                  dec(count, i1);
                end;
              finally FreeMem(p1) end;
          end else self.SetLastError(LastErrorNo, LastErrorStr);
      end else result := true;
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIProcess.GetThreads : IThreads;
begin
  result := TIThreads.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), self, []);
end;

function TIProcess.CreateThread(startAddr     : pointer;
                                parameter     : pointer             = nil;
                                creationFlags : cardinal            =   0;
                                stackSize     : cardinal            =   0;
                                threadAttr    : PSecurityAttributes = nil) : IThread;
var c1, c2 : cardinal;
begin
  result := nil;
  if CheckValid then
    if FID = GetCurrentProcessID then begin
      c1 := windows.CreateThread(threadAttr, stackSize, startAddr, parameter, creationFlags, c2);
      if c1 <> 0 then result := TIThread(AddKernelObj(otThread, madKernel.Handle(c1)))
      else            SetLastError(GetLastError);
    end else
      with GetHandle2(PROCESS_CREATE_THREAD) do
        if IsValid then begin
          c1 := CreateRemoteThreadEx(Handle, threadAttr, stackSize, startAddr, parameter, creationFlags, c2);
          if c1 <> 0 then result := TIThread(AddKernelObj(otThread, madKernel.Handle(c1)))
          else            self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
  if result = nil then
    result := TIThread.Create(false, FLastErrorNo, GetLastErrorStr, otUnknown, 0);
end;

function TIProcess.Module(handle    : cardinal;
                          autoClose : boolean = false) : IModule;
begin
  result := TIModule.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), self, handle, '', autoClose);
end;

function TIProcess.Module(memory    : pointer;
                          autoClose : boolean = false) : IModule;
var mbi : TMemoryBasicInformation;
begin
  with GetHandle2(PROCESS_QUERY_INFORMATION) do
    if IsValid and (VirtualQueryEx(Handle, memory, mbi, sizeOf(mbi)) = sizeOf(mbi)) and
       (mbi.State = MEM_COMMIT) and (mbi.AllocationBase <> nil) then
      memory := mbi.AllocationBase;
  result := TIModule.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), self, cardinal(memory), '', autoClose);
end;

function TIProcess.Module(fileName  : AnsiString;
                          autoClose : boolean = false) : IModule;
begin
  result := TIModule.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), self, 0, fileName, autoClose);
end;

function TIProcess.LoadModule(fileName          : AnsiString;
                              autoClose         : boolean = true;
                              withoutReferences : boolean = false;
                              onlyAsDataFile    : boolean = false;
                              alteredSearchPath : boolean = false;
                              timeOut           : integer = 3000 ) : IModule;
var c1 : cardinal;
begin
  result := nil;
  if CheckValid then begin
    if FID <> GetCurrentProcessID then begin
      c1 := 0;
      if withoutReferences then c1 := c1 or DONT_RESOLVE_DLL_REFERENCES;
      if onlyAsDataFile    then c1 := c1 or LOAD_LIBRARY_AS_DATAFILE;
      if alteredSearchPath then c1 := c1 or LOAD_WITH_ALTERED_SEARCH_PATH;
      if InjectCommand(icLoadLibrary, [c1], @c1, fileName, timeOut) then
        result := TIModule.Create(true, 0, '', self, c1, '', autoClose);
    end else begin
      result := madKernel.LoadModule(fileName, autoClose, withoutReferences,
                                     onlyAsDataFile, alteredSearchPath);
      if not result.IsValid then
        SetLastError(result.LastErrorNo, result.LastErrorStr);
    end;
  end;
  if result = nil then
    result := TIModule.Create(false, FLastErrorNo, GetLastErrorStr, nil, 0, '', false);
end;

function TIProcess.FreeModule(moduleHandle: cardinal) : boolean;
begin
  result := false;
  if CheckValid then
    if FID <> GetCurrentProcessID then
         result := InjectCommand(icFreeLibrary, [moduleHandle])
    else result := FreeLibrary(moduleHandle);
end;

function TIProcess.GetExportList : IXxportList;
var xl     : TIXxportList;
    i1, i2 : integer;
begin
  if CheckValid then begin
    xl := TIXxportList.Create(true, 0, '');
    result := xl;
    with GetModules do
      for i1 := 0 to ItemCount - 1 do
        with Items[i1].ExportList do
          for i2 := 0 to ItemCount - 1 do
            xl.AddItem(Items[i2]); 
  end else result := TIXxportList.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)));
end;

function TIProcess.GetModules : IModules;
begin
  result := TIModules.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), self, '', []);
end;

function TIProcess.GetTrayIcons : ITrayIcons;
begin
  result := TITrayIcons.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), self, []);
end;

function TIProcess.GetWindows : IWindows;
begin
  result := TIWindows.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), nil, self, false, []);
end;

function TIProcess.GetTaskbarWindows : IWindows;
begin
  result := TIWindows.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), nil, self, true, []);
end;

function TIProcess.SetWindowLong(window: cardinal; index, value: integer) : boolean;
begin
  result := false;
  if CheckValid then
    if FID = GetCurrentProcessID then begin
      windows.SetLastError(0);
      result := (windows.SetWindowLong(window, index, value) <> 0) or (GetLastError = 0);
      if not result then
        SetLastError(GetLastError);
    end else result := InjectCommand(icSetWindowLong, [window, cardinal(index), cardinal(value)])
end;

function TIProcess.GetHandles2 : IHandles;
begin
  result := TIHandles.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), self, nil, []);
end;

function TIProcess.FreeHandle(handle: cardinal) : boolean;
begin
  result := false;
  if CheckValid then
    if FID <> GetCurrentProcessID then
         result := (handle = 0) or (handle = INVALID_HANDLE_VALUE) or InjectCommand(icCloseHandle, [handle])
    else result := (handle = 0) or (handle = INVALID_HANDLE_VALUE) or CloseHandle(handle);
end;

function TIProcess.Minimize(activate: boolean = false) : boolean;
begin
  result := false;
  if CheckValid then
    with GetTaskbarWindows do
      if ItemCount > 0 then begin
        result := true;
        Minimize(activate);
      end else SetLastError(ERROR_FILE_NOT_FOUND);
end;

function TIProcess.Maximize(activate: boolean = true) : boolean;
begin
  result := false;
  if CheckValid then
    with GetTaskbarWindows do
      if ItemCount > 0 then begin
        result := true;
        Maximize(activate);
      end else SetLastError(ERROR_FILE_NOT_FOUND);
end;

function TIProcess.Restore(activate: boolean = true) : boolean;
begin
  result := false;
  if CheckValid then
    with GetTaskbarWindows do
      if ItemCount > 0 then begin
        result := true;
        Restore(activate);
      end else SetLastError(ERROR_FILE_NOT_FOUND);
end;

{$ifdef madDisAsm}

  function TIProcess.CheckFunction(func                 : pointer;
                                   paramsSize           : integer;
                                   loadModules          : boolean;
                                   out cfr              : TCopyFunctionRec;
                                   out fi               : TFunctionInfo;
                                   out bufferSize       : cardinal;
                                   acceptUnknownTargets : boolean = false) : boolean;
  var checkModules : array of IModule;
      resultRec    : TCopyFunctionRec absolute cfr;

    function AddModule(const module: IModule; codeAddr: pointer) : IModule;
    var i1, i2 : integer;
    begin
      result := nil;
      if loadModules then begin
        i2 := length(checkModules);
        for i1 := 0 to i2 - 1 do
          if module = checkModules[i1] then begin
            result := resultRec.UnloadModules[i1];
            exit;
          end;
        SetLength(checkModules, i2 + 1);
        checkModules[i2] := module;
        SetLength(resultRec.UnloadModules, i2 + 1);
        result := LoadModule(module.FileName);
        resultRec.UnloadModules[i2] := result;
        if not resultRec.UnloadModules[i2].IsValid then begin
          resultRec.UnloadModules := nil;
          raise Exception.Create('Could not load module for this target.') at codeAddr;
        end;
      end else begin
        result := self.Module(module.FileName);
        if not result.IsValid then
          raise Exception.Create('Could not find module for this target.') at codeAddr;
      end;
    end;

  var i1, i2 : integer;
      im     : IModule;
  begin
    Finalize(cfr);
    windows.ZeroMemory(@cfr, sizeOf(TCopyFunctionRec));
    checkModules := nil;
    fi := ParseFunction(func);
    if fi.IsValid then begin
      if fi.Copy.IsValid then begin
        if acceptUnknownTargets or (fi.UnknownTargets = nil) then begin
          i2 := 0;
          if fi.FarCalls <> nil then
            for i1 := 0 to high(fi.FarCalls) do
              with fi.FarCalls[i1] do begin
                im := madKernel.Module(Target);
                with im.ExportList.FindItem(Target) do
                  if IsValid then begin
                    Target := AddModule(im, CodeAddr2).GetProcAddress(Ordinal);
                    if Target = nil then begin
                      cfr.UnloadModules := nil;
                      raise Exception.Create('Could not find this target ($' + IntToHex(cardinal(Target), 1) +
                                             ') in the module!') at CodeAddr2;
                    end;
                    if PPTarget <> nil then
                      inc(i2);
                  end else begin
                    cfr.UnloadModules := nil;
                    raise Exception.Create('Invalid target ($' + IntToHex(cardinal(Target), 1) + ')') at CodeAddr2;
                  end;
              end;
          result := true;
          bufferSize := fi.CodeLen + i2 * 4 + paramsSize;
        end else
          raise Exception.Create(string(CErrorStr_UnknownTarget)) at fi.UnknownTargets[0].CodeAddr2;
      end else
        raise Exception.Create(string(fi.Copy.LastErrorStr)) at fi.Copy.LastErrorAddr;
    end else
      raise Exception.Create(string(fi.LastErrorStr)) at fi.LastErrorAddr;
  end;

  function TIProcess.RelocateFunction(var cfr: TCopyFunctionRec; const fi: TFunctionInfo) : boolean;
  var i1, i2 : integer;
      i64    : int64;
      pp     : TPPointer;
  begin
    cardinal(pp) := cardinal(cfr.Buffer) + cardinal(fi.CodeLen);
    i64 := int64(cardinal(fi.CodeBegin)) - int64(cardinal(cfr.Buffer));
    result := WriteMemory(fi.CodeBegin^, cfr.Buffer^, fi.CodeLen);
    if result then begin
      for i1 := 0 to high(fi.FarCalls) do
        with fi.FarCalls[i1] do
          if RelTarget then begin
            i2 := int64(cardinal(Target)) - (int64(cardinal(PTarget)) - i64) - 4;
            WriteMemory(i2, pointer(int64(cardinal(PTarget)) - i64)^, 4);
          end else
            if PPTarget <> nil then begin
              WriteMemory(Target, pp^, 4);
              WriteMemory(pp, pointer(int64(cardinal(PPTarget)) - i64)^, 4);
              inc(pp);
            end;
      cfr.IsValid := true;
      cfr.EntryPoint := pointer(int64(cardinal(fi.EntryPoint)) - i64);
      cardinal(cfr.Params_) := cardinal(pp);
    end;
  end;

  function TIProcess.CopyFunction(func                 : pointer;
                                  paramsSize           : integer = 0;
                                  acceptUnknownTargets : boolean = false) : TCopyFunctionRec;
  var fi : TFunctionInfo;
      c1 : cardinal;
  begin
    if CheckFunction(func, paramsSize, true, result, fi, c1, acceptUnknownTargets) then begin
      result.Buffer := AllocMem(c1);
      if not RelocateFunction(result, fi) then
        FreeMem(result.Buffer);
    end;
  end;

{$endif}

function TIProcess.CopyCode(beginCode   : pointer;
                            endCode     : TProcedure;
                            destination : pointer;
                            replace     : array of pointer) : boolean;
var i1, i2, i3 : integer;
    s1         : AnsiString;
    raHigh     : integer;
    p1         : pointer;
    pp1        : TPPointer;
    ab         : array of boolean;
begin
  result := false;
  raHigh := high(replace);
  for i1 := 0 to raHigh do
    if replace[i1] = nil then begin
      self.SetLastError(CErrorNo_InvalidReplaceArray, UnicodeString(AnsiString(CErrorStr_InvalidReplaceArray)));
      exit;
    end;
  with GetHandle2(PROCESS_VM_WRITE or PROCESS_VM_OPERATION) do
    if IsValid then begin
      i1 := integer(@endCode) - integer(beginCode);
      SetLength(s1, i1);
      Move(beginCode^, pointer(s1)^, i1);
      SetLength(ab, (raHigh + 1) div 2);
      for i2 := 1 to i1 - 3 do begin
        pp1 := @s1[i2];
        p1  := pp1^;
        i3  := 0;
        while i3 < raHigh do begin
          if p1 <> replace[i3] then begin
            if (PAnsiChar(pp1)^ = #$E8) and
               (TPInteger(@s1[i2 + 1])^ = integer(replace[i3]) - integer(beginCode) - i2 + 1 - 5) then begin
              TPInteger(@s1[i2 + 1])^ := integer(replace[i3 + 1]) - integer(destination) - i2 + 1 - 5;
              ab[i3 div 2] := true;
            end;
          end else begin
            pp1^ := replace[i3 + 1];
            ab[i3 div 2] := true;
          end;
          inc(i3, 2);
        end;
      end;
      for i2 := 0 to high(ab) do
        if not ab[i2] then
          raise Exception.Create('Not all replace pointers found!');
      result := WriteMemory(pointer(s1)^, destination^, i1);
    end;
end;

function InjectThread(parameter: pointer) : cardinal; stdcall;
var pc1, pc2 : PAnsiChar;
begin
  result := 0;
  with TInjectRec(parameter^) do
    while WaitForSingleObject(event1, INFINITE) = WAIT_OBJECT_0 do begin
      if command = icExitProcess then begin
        result := true;
        SetEvent(event2);
        ExitProcess(params[0]);
      end else if command = icAllocMem then begin
        params[0] := LocalAlloc(LPTR, params[0]);
        result := params[0] <> 0;
      end else if command = icFreeMem then begin
        result := LocalFree(params[0]) = 0;
      end else if command = icCloseHandle then begin
        result := CloseHandle(params[0]);
      end else if command = icSetWindowLong then begin
        windows.SetLastError(0);
        result := (SetWindowLong(params[0], integer(params[1]), integer(params[2])) <> 0) or
                  (GetLastError = 0);
      end else if command = icLoadLibrary then begin
        params[0] := LoadLibraryExA(name, 0, params[0]);
        result := params[0] <> 0;
      end else if command = icFreeLibrary then begin
        result := FreeLibrary(params[0]);
      end else if command = icGetCommandLine then begin
        pc1 := GetCommandLineA;
        pc2 := cmdLine;
        while (pc1^ <> #0) and (pc2 - PAnsiChar(@cmdLine) < MAX_PATH) do begin
          pc2^ := pc1^;
          inc(pc1);
          inc(pc2);
        end;
        result := true;
      end else if command = icGetStartupInfo then begin
        GetStartupInfo(startupInfo);
        result := true;
      end else if command = icCallFunc then begin
        TExecuteFunctionProc(params[0])(pointer(params[1])^);
        result := true;
      end;
      if not result then lastError := GetLastError;
      SetEvent(event2);
    end;
end;

procedure InjectThread_End; begin end;

function TIProcess.InjectCommand(command : TInjectCommand;
                                 params  : array of cardinal;
                                 result_ : pointer    = nil;
                                 str     : AnsiString = '';
                                 timeOut : cardinal   = 1000  ) : boolean;
var kdll, udll : IModule;
    ir         : TInjectRec;
    c1, c2     : cardinal;
    it         : IThread;
    b1, b2     : boolean;
    i1         : integer;
begin
  result := false;
  if CheckValid then
    if GetExeType in [etWin32, etConsole] then begin
      if FInjectMutex = nil then
        FInjectMutex := NewMutex('InjectMutex ' + IntToHexEx(FID, 1), false);
      if FInjectMutex.Enter(3000) then begin
        StoreHandle;
        try
          with GetHandle2(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_CREATE_THREAD or
                          PROCESS_VM_WRITE or PROCESS_VM_OPERATION or PROCESS_DUP_HANDLE) do
            if IsValid then begin
              if FInjectNB <> nil then begin
                b1 := true;
                b2 := true;
              end else begin
                FInjectNB := OpenNamedBuffer('InjectNB ' + IntToHexEx(FID, 1), false);
                if FInjectNB.IsValid then begin
                  FInjectMem := TPPointer(FInjectNB.Memory)^;
                  FInjectEvent1 := OpenEvent('InjectEvent1 ' + IntToHexEx(FID, 1));
                  FInjectEvent2 := OpenEvent('InjectEvent2 ' + IntToHexEx(FID, 1));
                  b1 := true;
                  b2 := true;
                end else begin
                  b1 := false;
                  b2 := false;
                  FInjectNB := nil;
                  kdll := madKernel.Module(kernel32);
                  udll := madKernel.LoadModule(user32);
                  with ir do begin
                    FInjectMem := AllocMem(sizeOf(TInjectRec) + integer(@InjectThread_End) - integer(@InjectThread), true);
                    c1 := CreateRemoteThreadEx(Handle, nil, 0,
                                               @TPInjectRecEx(FInjectMem)^.injectCode,
                                               @TPInjectRecEx(FInjectMem)^.injectRec,
                                               CREATE_SUSPENDED, c2);
                    if c1 = 0 then begin
                      FreeMem(FInjectMem);
                      self.SetLastError(GetLastError);
                    end else it := Thread(c1);
                  end;
                  if FInjectMem <> nil then begin
                    if CopyCode(@InjectThread, @InjectThread_End, @TPInjectRecEx(FInjectMem)^.injectCode,
                                [@WaitForSingleObject,     kdll.GetProcAddress('WaitForSingleObject'),
                                 @ExitProcess,             kdll.GetProcAddress('ExitProcess'        ),
                                 @CloseHandle,             kdll.GetProcAddress('CloseHandle'        ),
                                 @SetEvent,                kdll.GetProcAddress('SetEvent'           ),
                                 @windows.SetWindowLong,   udll.GetProcAddress('SetWindowLongA'     ),
                                 @LocalAlloc,              kdll.GetProcAddress('LocalAlloc'         ),
                                 @LocalFree,               kdll.GetProcAddress('LocalFree'          ),
                                 @LoadLibraryExA,          kdll.GetProcAddress('LoadLibraryExA'     ),
                                 @FreeLibrary,             kdll.GetProcAddress('FreeLibrary'        ),
                                 @windows.GetCommandLineA, kdll.GetProcAddress('GetCommandLineA'    ),
                                 @windows.GetStartupInfo,  kdll.GetProcAddress('GetStartupInfoA'    ),
                                 @windows.SetLastError,    kdll.GetProcAddress('SetLastError'       ),
                                 @GetLastError,            kdll.GetProcAddress('GetLastError'       )]) then begin
                      FInjectEvent1 := NewEvent(true, true, 'InjectEvent1 ' + IntToHexEx(FID, 1));
                      FInjectEvent2 := NewEvent(true, true, 'InjectEvent2 ' + IntToHexEx(FID, 1));
                      windows.ZeroMemory(@ir, sizeOf(TInjectRec));
                      ir.event1 := FInjectEvent1.Handle.Duplicate(false, 0, false, self).Handle;
                      ir.event2 := FInjectEvent2.Handle.Duplicate(false, 0, false, self).Handle;
                      ir.command := command;
                      for i1 := 0 to high(params) do ir.params[i1] := params[i1];
                      StrPLCopy(ir.name, str, sizeOf(ir.name) - 1);
                      if WriteMemory(ir, FInjectMem^, sizeOf(TInjectRec)) then
                        if it.Resume then begin
                          FInjectNB := NewNamedBuffer('InjectNB ' + IntToHexEx(FID, 1), 4);
                          TPPointer(FInjectNB.Memory)^ := FInjectMem;
                          FInjectNB.Handle.Duplicate(false, 0, false, self);
                          b1 := true;
                        end else self.SetLastError(it.LastErrorNo, it.LastErrorStr);
                    end;
                  end;
                end;
              end;
              if b2 then
                if ReadMemory(FInjectMem^, ir, sizeOf(TInjectRec)) then begin
                  ir.command := command;
                  for i1 := 0 to high(params) do ir.params[i1] := params[i1];
                  StrPLCopy(ir.name, str, sizeOf(ir.name) - 1);
                  b1 := WriteMemory(ir, FInjectMem^, sizeOf(TInjectRec));
                end else b1 := false;
              if b1 and FInjectEvent1.UnLock and FInjectEvent2.WaitFor(timeOut, false) and
                 ReadMemory(FInjectMem^, ir, sizeOf(TInjectRec)) then 
                if command = icGetCommandLine then begin
                  result := true;
                  if result_ <> nil then TPAnsiString(result_)^ := ir.cmdLine;
                end else if command = icGetStartupInfo then begin
                  result := true;
                  if result_ <> nil then PStartupInfo(result_)^ := ir.startupInfo;
                end else
                  if ir.result then begin
                    if result_ <> nil then TPCardinal(result_)^ := ir.params[0];
                    result := true;
                  end else begin
                    if result_ <> nil then TPCardinal(result_)^ := 0;
                    self.SetLastError(ir.lastError);
                  end;
            end else self.SetLastError(LastErrorNo, LastErrorStr);
        finally
          ReleaseHandle;
          FInjectMutex.Leave;
        end;
      end else SetLastError(FInjectMutex.LastErrorNo, FInjectMutex.LastErrorStr);
    end else SetLastError(CErrorNo_NoWin32Process, UnicodeString(AnsiString(CErrorStr_NoWin32Process)));
end;

function TIProcess.ExecuteFunction(func                 : TExecuteFunctionProc;
                                   timeOut              : cardinal = 1000;
                                   params               : pointer  = nil;
                                   paramsSize           : cardinal = 0;
                                   acceptUnknownTargets : boolean  = false) : boolean;

{$ifdef madDisAsm}
  var cfr : TCopyFunctionRec;
{$endif}
begin
  result := false;
  if CheckValid then
    if (params = nil) = (paramsSize = 0) then begin
      if FID <> GetCurrentProcessID then begin
        {$ifdef madDisAsm}
          with GetHandle2(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_CREATE_THREAD or
                          PROCESS_VM_WRITE or PROCESS_VM_OPERATION or PROCESS_DUP_HANDLE) do
            if IsValid then begin
              cfr := CopyFunction(@func, paramsSize, acceptUnknownTargets);
              with cfr do
                if IsValid then
                  try
                    if WriteMemory(params^, Params_^, paramsSize) then
                      result := InjectCommand(icCallFunc,
                                              [cardinal(EntryPoint), cardinal(Params_)],
                                              nil, '', timeOut) and
                                ReadMemory(Params_^, params^, paramsSize);
                  finally
                    cfr.UnloadModules := nil;
                    FreeMem(Buffer);
                  end;
            end else self.SetLastError(LastErrorNo, LastErrorStr);
        {$else}
          result := false;
          SetLastError(ERROR_NOT_SUPPORTED);
        {$endif}
      end else begin
        result := true;
        func(params^);
      end;
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIProcess.Suspend : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    with GetThreads do begin
      result := true;
      for i1 := 0 to ItemCount - 1 do
        result := Items[i1].Suspend and result;
    end;
end;

function TIProcess.Resume : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    with GetThreads do begin
      result := true;
      for i1 := 0 to ItemCount - 1 do
        result := Items[i1].Resume and result;
    end;
end;

function TIProcess.WaitForInputIdle(timeOut: cardinal = INFINITE) : boolean;
begin
  result := false;
  if CheckValid then
    with GetHandle2(PROCESS_QUERY_INFORMATION) do
      if IsValid then begin
        case windows.WaitForInputIdle(Handle, timeOut) of
          0            : result := true;
          WAIT_TIMEOUT : self.SetLastError(WAIT_TIMEOUT, UnicodeString(AnsiString(CErrorStr_WaitTimeout)));
          else           self.SetLastError(GetLastError);
        end;
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIProcess.IsStillRunning : boolean;
begin
  result := CheckValid and (WaitForSingleObject(GetHandle2(SYNCHRONIZE).Handle, 0) = WAIT_TIMEOUT);
end;

function TIProcess.Close : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    with GetTaskbarWindows do
      if ItemCount > 0 then begin
        for i1 := ItemCount - 1 downto 0 do
          result := Items[i1].Close or result;
      end else result := Quit;
end;

function TIProcess.Quit : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    with GetThreads do begin
      for i1 := ItemCount - 1 downto 0 do
        if Items[i1].GetTaskbarWindows.ItemCount > 0 then begin
          result := true;
          Items[i1].PostQuitMessage(0);
        end;
      if not result then
        for i1 := ItemCount - 1 downto 0 do
          if Items[i1].GetWindows.ItemCount > 0 then begin
            result := true;
            Items[i1].PostQuitMessage(0);
          end;
      if not result then
        for i1 := ItemCount - 1 downto 0 do
          result := Items[i1].PostQuitMessage(0) or result;
    end;
end;

function TIProcess.Exit_(exitCode: cardinal = 0) : boolean;
begin
  result := false;
  if CheckValid then begin
    if FID <> GetCurrentProcessID then
         result := InjectCommand(icExitProcess, [exitCode]) and WaitFor(1000, false)
    else ExitProcess(exitCode);
  end;
end;

function TIProcess.Terminate(exitCode: cardinal = 0) : boolean;
begin
  result := false;
  if CheckValid then
    with GetHandle2(PROCESS_TERMINATE) do
      if IsValid then begin
        result := TerminateProcess(Handle, exitCode);
        if not result then self.SetLastError(GetLastError);
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIProcess.GetExitCode : cardinal;
begin
  result := maxCard;
  if CheckValid then
    with GetHandle2(PROCESS_QUERY_INFORMATION) do
      if IsValid then begin
        if not GetExitCodeProcess(Handle, result) then begin
          result := maxCard;
          self.SetLastError(GetLastError);
        end;
      end else self.SetLastError(LastErrorNo, LastErrorStr);
end;

function TIProcess.GetTimes(var creation, exit, kernel, user: int64) : boolean;
begin
  result := false;
  if CheckValid then begin
    if @GetProcessTimes = nil then
      GetProcessTimes :=
        GetProcAddress(GetModuleHandle(kernel32), 'GetProcessTimes');
    if @GetProcessTimes <> nil then begin
      with GetHandle2(PROCESS_QUERY_INFORMATION) do
        if IsValid then begin
          result := GetProcessTimes(Handle, creation, exit, kernel, user);
          if not result then
            self.SetLastError(GetLastError);
        end else self.SetLastError(LastErrorNo, LastErrorStr);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
  if not result then begin
    creation := 0;
    exit     := 0;
    kernel   := 0;
    user     := 0;
  end;
end;

function TIProcess.GetMaxInterface : IBasic;
begin
  result := IProcess(self);
end;

function Process(const process: IHandle) : IProcess;
begin
  if (process <> nil) and process.IsValid and (process.ObjType = otProcess) then
       result := TIProcess(AddKernelObj(otProcess, process))
  else result := TIProcess.Create(false, ERROR_INVALID_PARAMETER, '', otUnknown, 0);
end;

function Process(process   : cardinal;
                 autoClose : boolean = true) : IProcess;
begin
  result := madKernel.Process(Handle(process, autoClose));
  if not result.IsValid then
    result := TIProcess(AddKernelObj(otProcess, process));
end;

function Process(exeFile: AnsiString) : IProcess;
begin
  with Processes(exeFile) do
    if ItemCount > 0 then
         result := Items[0]
    else result := TIProcess.Create(false, ERROR_FILE_NOT_FOUND, '', otUnknown, 0);
end;

var FCurrentProcess : IProcess;
function CurrentProcess : IProcess;
begin
  if FCurrentProcess = nil then
    FCurrentProcess := TIProcess(AddKernelObj(otProcess, GetCurrentProcessID));
  result := FCurrentProcess;
end;

function ParentProcess : IProcess;
begin
  result := CurrentProcess.ParentProcess;
end;

var CreateProcessWithLogonW : function (user, domain, password: PWideChar;
                                        logonFlags: dword;
                                        applicationName, commandLine: PWideChar;
                                        creationFlags: dword;
                                        environment: pointer;
                                        currentDirectory: PWideChar;
                                        var startupInfo: {$ifdef UNICODE} TStartupInfoA; {$else} TStartupInfo; {$endif}
                                        var processInfo: TProcessInformation) : bool; stdcall = nil;

function NewProcessEx(userName       : UnicodeString;
                      password       : UnicodeString;
                      domain         : UnicodeString;
                      exeFile        : AnsiString;
                      params         : AnsiString;
                      workingDir     : AnsiString;
                      showCmd        : cardinal;
                      creationFlags  : cardinal;
                      processAttr    : PSecurityAttributes;
                      threadAttr     : PSecurityAttributes;
                      inheritHandles : boolean;
                      environment    : pointer            ) : IProcess;

  function GetParamStr(pc: PAnsiChar; var param: AnsiString) : PAnsiChar;
  var len : integer;
      buf : array [0..4095] of AnsiChar;
  begin
    while true do begin
      while (pc[0] <> #0) and (pc[0] <= ' ') do inc(pc);
      if (pc[0] = '"') and (pc[1] = '"') then inc(pc, 2) else break;
    end;
    len := 0;
    while (pc[0] > ' ') and (len < sizeOf(buf)) do
      if pc[0] = '"' then begin
        inc(pc);
        while (pc[0] <> #0) and (pc[0] <> '"') do begin
          buf[len] := pc[0];
          inc(len);
          inc(pc);
        end;
        if pc[0] <> #0 then inc(pc);
      end else begin
        buf[len] := pc[0];
        inc(len);
        inc(pc);
      end;
    SetString(param, buf, len);
    result := pc;
  end;

  function ParamCount(cmdLine: PAnsiChar = nil) : integer;
  var s1 : AnsiString;
  begin
    if cmdLine = nil then cmdLine := GetCommandLineA;
    cmdLine := GetParamStr(cmdLine, s1);
    result := 0;
    while true do begin
      cmdLine := GetParamStr(cmdLine, s1);
      if s1 = '' then break;
      inc(result);
    end;
  end;

  function ParamStr(index: integer; cmdLine: PAnsiChar = nil): AnsiString;
  var buf : array [0..260] of AnsiChar;
  begin
    if cmdLine = nil then cmdLine := GetCommandLineA;
    if index > 0 then begin
      while true do begin
        cmdLine := GetParamStr(cmdLine, result);
        if (index = 0) or (result = '') then break;
        dec(index);
      end;
    end else SetString(result, buf, GetModuleFileNameA(0, buf, sizeOf(buf)));
  end;

const LOGON_WITH_PROFILE        = $00000001;
      LOGON_NETCREDENTIALS_ONLY = $00000002;
var si             : {$ifdef UNICODE} TStartupInfoA; {$else} TStartupInfo; {$endif}
    pi             : TProcessInformation;
    pc1            : PAnsiChar;
    s1, s2, s3, s4 : AnsiString;
    i1             : integer;
    sei            : TShellExecuteInfoA;
    sl             : IShellLinkA;
    pf             : IPersistFile;
    arrCh          : array [0..MAX_PATH] of AnsiChar;
    wfd            : TWin32FindDataA;
    ip1            : TIProcess;
    wdw            : UnicodeString;
    pw1            : PWideChar;
    b1             : boolean;
begin
  KillChar(exeFile, '"');
  exeFile := GetLongFileName(exeFile);
  s1 := AnsiString(ExtractFileExt(string(exeFile)));
  if IsTextEqual(s1, AnsiString('.lnk')) or IsTextEqual(s1, AnsiString('.pif')) then begin
    // with a shellLink CreateProcess wouldn't work at all
    // ShellExecuteEx would work, but would not give us a valid process ID
    CoInitialize(nil);
    if (CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IShellLink, sl) = 0) and
       (sl.QueryInterface(IPersistFile, pf) = 0) and
       (pf.Load(PWideChar(UnicodeString(exeFile)), 0) = 0) and
       (sl.GetPath(arrCh, MAX_PATH, wfd, 0) = 0) then begin
      exeFile := arrCh;
      KillChar(exeFile, '"');
      exeFile := GetLongFileName(exeFile);
      if (params = '') and (sl.GetArguments(arrCh, MAX_PATH) = 0) then
        params := arrCh;
      if (sl.GetWorkingDirectory(arrCh, MAX_PATH) = 0) and (arrCh[0] <> #0) then
        workingDir := arrCh;
      if (showCmd = SW_SHOWDEFAULT) and (sl.GetShowCmd(i1) = 0) then
        showCmd := i1;
    end;
  end;
  if showCmd = SW_SHOWDEFAULT then showCmd := SW_SHOWNORMAL;
  if workingDir = '' then pc1 := nil
  else                    pc1 := PAnsiChar(workingDir);
  zeroMemory(@si, sizeOf(TStartupInfo));
  si.cb := sizeOf(TStartupInfo);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := showCmd;
  if ExeType(exeFile) in [etDos, etWin16] then
    if params <> '' then begin
      s1 := '"x" ' + params;
      params := '';
      for i1 := 1 to ParamCount(PAnsiChar(s1)) do begin
        s2 := ParamStr(i1, PAnsiChar(s1));
        s3 := s2;
        KillChar(s3, '"');
        if GetFileAttributesA(PAnsiChar(s3)) <> dword(-1) then begin
          s4 := GetShortFileName(s3);
          if (s4 <> s3) and (Pos(AnsiString(' '), s4) = 0) then
               params := params + ' ' + s4
          else params := params + ' ' + s3;
        end else
          params := params + ' ' + s3;
      end;
      Delete(params, 1, 1);
    end;
  s1 := '"' + exeFile + '"';
  if params <> '' then
    s1 := s1 + ' ' + params;
  if @CreateProcessWithLogonW = nil then
    CreateProcessWithLogonW := GetProcAddress(LoadLibrary(advapi32), 'CreateProcessWithLogonW');
  if (userName <> '') and (@CreateProcessWithLogonW <> nil) then begin
    if workingDir <> '' then begin
      wdw := UnicodeString(workingDir);
      pw1 := PWideChar(wdw);
    end else
      pw1 := nil;
    b1 := CreateProcessWithLogonW(PWideChar(userName), PWideChar(domain), PWideChar(password), LOGON_NETCREDENTIALS_ONLY,//LOGON_WITH_PROFILE,
                                  nil, PWideChar(UnicodeString(s1)), creationFlags, environment, pw1, si, pi);
  end else
    b1 := CreateProcessA(nil, PAnsiChar(s1), processAttr, threadAttr, false, creationFlags, environment, pc1, si, pi);
  if b1 then begin
    CloseHandle(pi.hThread);
    if pi.dwProcessID <> 0 then begin
      ip1 := TIProcess(AddKernelObj(otProcess, Handle(pi.hProcess)));
      ip1.FIDReady       := true;
      ip1.FID            := pi.dwProcessID;
      ip1.FParentProcess := CurrentProcess;
      result := ip1;
    end else begin
      CloseHandle(pi.hProcess);
      result := TIProcess.Create(false, CErrorNo_ProcessStartedButNoID,
                                 UnicodeString(AnsiString(CErrorStr_ProcessStartedButNoID)), otUnknown, 0);
    end;
  end else
    if (creationFlags = 0) and (processAttr = nil) and (threadAttr = nil) and
       (inheritHandles = true) and (environment = nil) then begin
      ZeroMemory(@sei, sizeOf(TShellExecuteInfo));
      sei.cbSize       := sizeOf(TShellExecuteInfo);
      sei.fMask        := SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
      sei.lpFile       := PAnsiChar(exeFile);
      sei.lpParameters := PAnsiChar(params);
      sei.lpDirectory  := pc1;
      sei.nShow        := integer(showCmd);
      if ShellExecuteEx(@sei) then begin
        if sei.hProcess <> 0 then begin
          ip1 := TIProcess(AddKernelObj(otProcess, Handle(sei.hProcess)));
          ip1.FParentProcess := CurrentProcess;
          result := ip1;
        end else
          result := TIProcess.Create(false, CErrorNo_ProcessStartedButNoID,
                                     UnicodeString(AnsiString(CErrorStr_ProcessStartedButNoID)), otUnknown, 0);
      end else result := TIProcess.Create(false, GetLastError, '', otUnknown, 0);
    end else result := TIProcess.Create(false, GetLastError, '', otUnknown, 0);
end;

function NewProcess(exeFile        : AnsiString;
                    params         : AnsiString          = '';
                    workingDir     : AnsiString          = '';
                    showCmd        : cardinal            = SW_SHOWDEFAULT;
                    creationFlags  : cardinal            = 0;
                    processAttr    : PSecurityAttributes = nil;
                    threadAttr     : PSecurityAttributes = nil;
                    inheritHandles : boolean             = true;
                    environment    : pointer             = nil           ) : IProcess;
begin
  result := NewProcessEx('', '', '', exeFile, params, workingDir, showCmd, creationFlags, processAttr, threadAttr, inheritHandles, environment);
end;

function NewProcessAsUser(userName       : UnicodeString;
                          password       : UnicodeString;
                          domain         : UnicodeString;
                          exeFile        : AnsiString;
                          params         : AnsiString          = '';
                          workingDir     : AnsiString          = '';
                          showCmd        : cardinal            = SW_SHOWDEFAULT;
                          creationFlags  : cardinal            = 0;
                          processAttr    : PSecurityAttributes = nil;
                          threadAttr     : PSecurityAttributes = nil;
                          inheritHandles : boolean             = true;
                          environment    : pointer             = nil           ) : IProcess;
begin
  result := NewProcessEx(userName, password, domain, exeFile, params, workingDir, showCmd, creationFlags, processAttr, threadAttr, inheritHandles, environment);
end;

function PrintFile(file_      : AnsiString;
                   workingDir : AnsiString = '';
                   showCmd    : cardinal   = SW_SHOWNORMAL) : IProcess;
var pc1  : PAnsiChar;
    verb : AnsiString;
    sei  : TShellExecuteInfoA;
    ip1  : TIProcess;
begin
  KillChar(file_, '"');
  file_ := GetLongFileName(file_);
  if workingDir = '' then pc1 := nil
  else                    pc1 := PAnsiChar(workingDir);
  verb := 'print';
  ZeroMemory(@sei, sizeOf(TShellExecuteInfo));
  sei.cbSize       := sizeOf(TShellExecuteInfo);
  sei.fMask        := SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  sei.lpFile       := PAnsiChar(file_);
  sei.lpDirectory  := pc1;
  sei.nShow        := integer(showCmd);
  sei.lpVerb       := PAnsiChar(verb);
  if ShellExecuteEx(@sei) then begin
    if sei.hProcess <> 0 then begin
      ip1 := TIProcess(AddKernelObj(otProcess, Handle(sei.hProcess)));
      ip1.FParentProcess := CurrentProcess;
      result := ip1;
    end else
      result := TIProcess.Create(false, CErrorNo_ProcessStartedButNoID,
                                 UnicodeString(AnsiString(CErrorStr_ProcessStartedButNoID)), otUnknown, 0);
  end else result := TIProcess.Create(false, GetLastError, '', otUnknown, 0);
end;

// ***************************************************************

type
  // types for Process32First/Next
  TProcessEntry32 = record
    size          : cardinal;
    usage         : cardinal;
    process       : cardinal;  // this process (ID)
    defaultHeap   : cardinal;  // default heap (ID)
    module        : cardinal;  
    threadCount   : cardinal;
    parentProcess : cardinal;  // this process's parent process (ID)
    basePriority  : integer;   // Base priority of process's threads
    flags         : cardinal;
    exeFile       : array [0..MAX_PATH - 1] of AnsiChar;
  end;

var
  // enumeration of processes in win9x
  Process32First : function (snap: cardinal; var pe: TProcessEntry32) : LongBool stdcall = nil;
  Process32Next  : function (snap: cardinal; var pe: TProcessEntry32) : LongBool stdcall = nil;

const
  // constants for CreateToolhelp32Snapshot
  TH32CS_SnapProcess = 2;

constructor TIProcesses.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                               exeFile: AnsiString; const objects: array of IBasic);
var i1 : integer;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FAll := (exeFile = '') or (exeFile = '*') or (exeFile = '*.*');
    if not FAll then begin
      FExeFile := GetLongFileName(exeFile);
      FList := Length(objects) > 0;
      if FList then
        for i1 := 0 to high(objects) do
          if objects[i1] <> nil then
            AddItem(objects[i1]);
    end;
    if not FList then RefreshItems;
  end;
end;

function TIProcesses.GetExeFile : AnsiString;
begin
  result := FExeFile;
end;

function TIProcesses.GetItem(index: integer) : IProcess;
begin
  if (index < 0) or (index >= FCount) then
       result := TIProcess.Create(false, CErrorNo_IndexOutOfRange, UnicodeString(AnsiString(CErrorStr_IndexOutOfRange)), otProcess, 0)
  else result := IProcess(FItems[index]);
end;

function TIProcesses.RefreshItems : boolean;

  procedure RefreshItem(pid, parentPid, session: cardinal; exeFile: AnsiString);
  var i1     : integer;
      s1, s2 : AnsiString;
      b1     : boolean;
      ip1    : TIProcess;
  begin
    if not FAll then begin
      s1 := FExeFile;
      s2 := exeFile;
      if      ExtractFilePath(string(s1)) = '' then s2 := AnsiString(ExtractFileName(string(s2)))
      else if ExtractFilePath(string(s2)) = '' then s1 := AnsiString(ExtractFileName(string(s1)));
      b1 := IsTextEqual(s1, s2);
    end else b1 := true;
    if b1 then begin
      for i1 := 0 to FCount - 1 do
        with TIProcess(FItems[i1].SelfAsTObject) do
          if GetID = pid then begin
//             ((FParentProcess = nil) or (parentPid = 0) or (FParentProcess.ID = parentPid)) and
//             (FExeFile <> exeFile) then begin
            if ((FParentProcess = nil) and (parentPid <> 0)) or (FExeFile <> exeFile) or (FSession <> session) then begin
              Change(FItems[i1], true, lctChanged, i1, i1);
              if parentPid <> 0 then
                FParentProcess := TIProcess(AddKernelObj(otProcess, parentPid));
              FExeFile := exeFile;
              FSession := session;
              self.FItemInfos[i1].LastChangeType := lctChanged;
              Change(FItems[i1], false, lctChanged, i1, SortItem(i1));
            end else
              self.FItemInfos[i1].LastChangeType := lctUnChanged;
            exit;
          end;
      ip1 := TIProcess(AddKernelObj(otProcess, pid));
      if parentPid <> 0 then
        ip1.FParentProcess := TIProcess(AddKernelObj(otProcess, parentPid));
      ip1.FExeFile := exeFile;
      ip1.FSession := session;
      ip1.FSessionDone := true;
      if AddItem(IProcess(ip1)) = -1 then
        ip1.Destroy;
    end;
  end;

var c1, c2 : cardinal;
    pe     : TProcessEntry32;
    p1     : pointer;
    npi    : ^TNtProcessInfo;
    s1     : AnsiString;
begin
  result := false;
  if CheckValid then
    if not FList then begin
      BeginRefresh;
      try
        if OS.win9x then begin
          if @Process32First = nil then begin
            c1 := GetModuleHandle(kernel32);
            CreateToolhelp32Snapshot := GetProcAddress(c1, 'CreateToolhelp32Snapshot');
            Process32First           := GetProcAddress(c1, 'Process32First'          );
            Process32Next            := GetProcAddress(c1, 'Process32Next'           );
          end;
          c1 := CreateToolHelp32Snapshot(TH32CS_SnapProcess, 0);
          if c1 <> INVALID_HANDLE_VALUE then begin
            try
              pe.size := sizeOf(TProcessEntry32);
              if Process32First(c1, pe) then
                repeat
                  RefreshItem(pe.process, pe.parentProcess, 0, GetLongFileName(pe.exeFile));
                until not Process32Next(c1, pe);
            finally CloseHandle(c1) end;
          end else SetLastError(GetLastError);
        end else begin
          if @NtQuerySystemInformation = nil then
            NtQuerySystemInformation := GetProcAddress(GetModuleHandle(ntdll), 'NtQuerySystemInformation');
          c1 := 0;
          NtQuerySystemInformation(5, nil, 0, @c1);
          p1 := nil;
          try
            if c1 = 0 then begin
              c1 := $10000;
              repeat
                c1 := c1 * 2;
                LocalFree(dword(p1));
                dword(p1) := LocalAlloc(LPTR, c1);
                c2 := NtQuerySystemInformation(5, p1, c1, nil);
              until (c2 = 0) or (c1 = $400000);
            end else begin
              c1 := c1 * 2;
              dword(p1) := LocalAlloc(LPTR, c1);
              c2 := NtQuerySystemInformation(5, p1, c1, nil);
            end;
            if c2 = 0 then begin
              npi := p1;
              while true do begin
                if npi^.name <> nil then begin
                  s1 := '?';
                  c2 := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, npi^.pid);
                  if c2 <> 0 then
                    try
                      s1 := GetProcessFileNameNt(c2);
                    finally CloseHandle(c2) end;
                  if s1 = '?' then
                    s1 := AnsiString(UnicodeString(npi^.name));
                end else s1 := '[System Process]';
                RefreshItem(npi^.pid, npi^.parentPid, npi^.sessionId, s1);
                if npi^.offset = 0 then break;
                npi := pointer(cardinal(npi) + npi^.offset);
              end;
            end else SetLastError(c2);
          finally LocalFree(dword(p1)) end;
        end;
      finally EndRefresh end;
    end else SetLastError(CErrorNo_RefreshListError, UnicodeString(AnsiString(CErrorStr_RefreshListError)));
end;

procedure TIProcesses.SetServiceProcess(serviceProcess: boolean = true);
var i1 : integer;
begin
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      with IProcess(FItems[i1]) do begin
        SetServiceProcess(serviceProcess);
        FSuccess := FSuccess or Success;
      end;
end;

procedure TIProcesses.SetPriorityClass(priorityClass: TPriorityClass);
var i1 : integer;
begin
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      with IProcess(FItems[i1]) do begin
        SetPriorityClass(priorityClass);
        FSuccess := FSuccess or Success;
      end;
end;

procedure TIProcesses.SetPriorityBoost(priorityBoost: boolean);
var i1 : integer;
begin
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      with IProcess(FItems[i1]) do begin
        SetPriorityBoost(priorityBoost);
        FSuccess := FSuccess or Success;
      end;
end;

procedure TIProcesses.SetAffinityMask(affinityMask: cardinal);
var i1 : integer;
begin
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      with IProcess(FItems[i1]) do begin
        SetAffinityMask(affinityMask);
        FSuccess := FSuccess or Success;
      end;
end;

function TIProcesses.SetWorkingSetSize(minimum, maximum: cardinal) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IProcess(FItems[i1]).SetWorkingSetSize(minimum, maximum) or result;
end;

function TIProcesses.Minimize(activate: boolean = false) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IProcess(FItems[i1]).Minimize(activate) or result;
end;

function TIProcesses.Maximize(activate: boolean = true) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IProcess(FItems[i1]).Maximize(activate) or result;
end;

function TIProcesses.Restore(activate: boolean = true) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IProcess(FItems[i1]).Restore(activate) or result;
end;

function TIProcesses.Suspend : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IProcess(FItems[i1]).Suspend or result;
end;

function TIProcesses.Resume : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IProcess(FItems[i1]).Resume or result;
end;

function TIProcesses.IsStillRunning(all: boolean = false) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do begin
      result := IProcess(FItems[i1]).IsStillRunning;
      if result <> all then break;
    end;
end;

function TIProcesses.Close : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IProcess(FItems[i1]).Close or result;
end;

function TIProcesses.Quit : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IProcess(FItems[i1]).Quit or result;
end;

function TIProcesses.Exit_(exitCode: cardinal = 0) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IProcess(FItems[i1]).Exit_(exitCode) or result;
end;

function TIProcesses.Terminate(exitCode: cardinal = 0) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    for i1 := 0 to FCount - 1 do
      result := IProcess(FItems[i1]).Terminate(exitCode) or result;
end;

function TIProcesses.GetMaxInterface : IBasic;
begin
  result := IProcesses(self);
end;

function Processes(exeFile: AnsiString = '*') : IProcesses;
begin
  if exeFile = '*' then
       result := TIProcesses.Create(true, 0, '', '',      [])
  else result := TIProcesses.Create(true, 0, '', exeFile, []);
end;

function PickProcesses(const objects: array of IBasic) : IProcesses;
begin
  result := TIProcesses.Create(true, 0, '', '', PickInterfaces(objects, TIProcess));
end;

function PickProcesses(const objects: ICustomBasicList) : IProcesses;
begin
  if (objects <> nil) and objects.IsValid then begin
    result := TIProcesses.Create(true, 0, '', '',
                PickInterfaces(TICustomBasicList(objects.SelfAsTObject).FItems, TIProcess));
  end else result := TIProcesses.Create(false, ERROR_INVALID_PARAMETER, '', '', []);
end;

// ***************************************************************

function TIOtherKernelObj.GetMaxInterface : IBasic;
begin
  result := IOtherKernelObj(self);
end;

// ***************************************************************

function TIEvent.GetName : AnsiString;
begin
  result := FName;
end;

function TIEvent.Lock : boolean;
begin
  result := CheckValid and ResetEvent(FHandle.Handle);
  if FValid and (not result) then SetLastError(GetLastError);
end;

function TIEvent.Unlock : boolean;
begin
  result := CheckValid and SetEvent(FHandle.Handle);
  if FValid and (not result) then SetLastError(GetLastError);
end;

function TIEvent.Pulse : boolean;
begin
  result := CheckValid and PulseEvent(FHandle.Handle);
  if FValid and (not result) then SetLastError(GetLastError);
end;

function TIEvent.GetMaxInterface : IBasic;
begin
  result := IEvent(self);
end;

function Event(const event: IHandle) : IEvent;
begin
  if (event <> nil) and event.IsValid and (event.ObjType = otEvent) then
       result := TIEvent(AddKernelObj(otEvent, event))
  else result := TIEvent.Create(false, ERROR_INVALID_PARAMETER, '', otUnknown, 0);
end;

function Event(event     : cardinal;
               autoClose : boolean = true) : IEvent;
begin
  result := madKernel.Event(Handle(event, autoClose));
end;

function OpenEvent(name           : AnsiString;
                   access         : cardinal = EVENT_ALL_ACCESS;
                   inheritHandles : boolean  = true            ) : IEvent;
var c1  : cardinal;
    pc1 : PAnsiChar;
    ie1 : TIEvent;
begin
  if name = '' then pc1 := nil else pc1 := PAnsiChar(name);
  c1 := windows.OpenEventA(EVENT_ALL_ACCESS, true, pc1);
  ie1 := TIEvent(AddKernelObj(otEvent, Handle(c1)));
  ie1.FName := name;
  result := ie1;
end;

function NewEvent(auto      : boolean             = false;
                  locked    : boolean             = true;
                  name      : AnsiString          = '';
                  eventAttr : PSecurityAttributes = nil ) : IEvent;
var c1  : cardinal;
    pc1 : PAnsiChar;
    ie1 : TIEvent;
begin
  if name = '' then pc1 := nil else pc1 := PAnsiChar(name);
  c1 := CreateEventA(eventAttr, not auto, not locked, pc1);
  ie1 := TIEvent(AddKernelObj(otEvent, Handle(c1)));
  ie1.FName       := name;
  ie1.FEventReady := true;
  ie1.FAutoEvent  := auto;
  result := ie1;
end;

// ***************************************************************

function TIMutex.GetName : AnsiString;
begin
  result := FName;
end;

function TIMutex.Enter(milliseconds: cardinal = INFINITE) : boolean;
begin
  result := WaitFor(milliseconds, false);
end;

function TIMutex.TryEnter : boolean;
begin
  result := false;
  if CheckValid then
    case WaitForSingleObject(FHandle.Handle, 0) of
      WAIT_TIMEOUT   : SetLastError(WAIT_TIMEOUT,   UnicodeString(AnsiString(CErrorStr_WaitTimeout  )));
      WAIT_ABANDONED : SetLastError(WAIT_ABANDONED, UnicodeString(AnsiString(CErrorStr_WaitAbandoned)));
      WAIT_OBJECT_0  : result := true;
      else             SetLastError(GetLastError);
    end;
end;

function TIMutex.Leave : boolean;
begin
  result := CheckValid and ReleaseMutex(FHandle.Handle);
  if FValid and (not result) then SetLastError(GetLastError);
end;

function TIMutex.GetMaxInterface : IBasic;
begin
  result := IMutex(self);
end;

function Mutex(const mutex: IHandle) : IMutex;
begin
  if (mutex <> nil) and mutex.IsValid and (mutex.ObjType = otMutex) then
       result := TIMutex(AddKernelObj(otMutex, mutex))
  else result := TIMutex.Create(false, ERROR_INVALID_PARAMETER, '', otUnknown, 0);
end;

function Mutex(mutex     : cardinal;
               autoClose : boolean = true) : IMutex;
begin
  result := madKernel.Mutex(Handle(mutex, autoClose));
end;

function OpenMutex(name           : AnsiString;
                   access         : cardinal = MUTEX_ALL_ACCESS;
                   inheritHandles : boolean  = true            ) : IMutex;
var c1  : cardinal;
    pc1 : PAnsiChar;
    im1 : TIMutex;
begin
  if name = '' then pc1 := nil else pc1 := PAnsiChar(name);
  c1 := windows.OpenMutexA(MUTEX_ALL_ACCESS, true, pc1);
  im1 := TIMutex(AddKernelObj(otMutex, Handle(c1)));
  im1.FName := name;
  result := im1;
end;

function NewMutex(name      : AnsiSTring = '';
                  enter     : boolean = true;
                  mutexAttr : PSecurityAttributes = nil) : IMutex;
var c1  : cardinal;
    pc1 : PAnsiChar;
    im1 : TIMutex;
begin
  if name = '' then pc1 := nil else pc1 := PAnsiChar(name);
  c1 := CreateMutexA(mutexAttr, bool(integer(enter)), pc1);
  if (c1 <> 0) and (GetLastError = ERROR_ALREADY_EXISTS) and enter then
    WaitForSingleObject(c1, INFINITE);
  im1 := TIMutex(AddKernelObj(otMutex, Handle(c1)));
  im1.FName := name;
  result := im1;
end;

// ***************************************************************

type
  // implements INamedBuffer
  TINamedBuffer = class (TIKernelObj, INamedBuffer)
  public
    FName        : AnsiString;
    FSize        : integer;
    FBuffer      : pointer;
    FWriteAccess : boolean;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        name: AnsiString; size: integer; writeAccess: boolean;
                        buffer: pointer; handle: cardinal);
    destructor Destroy; override;

    function GetName : AnsiString;

    function GetSize : integer;

    function GetMemory : pointer;

    function IsWriteAccess : boolean;

    function GetMaxInterface : IBasic; override;
  end;

constructor TINamedBuffer.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                                 name: AnsiString; size: integer; writeAccess: boolean;
                                 buffer: pointer; handle: cardinal);
begin
  if valid then
       inherited Create(valid, lastErrorNo, lastErrorStr, madKernel.Handle(handle))
  else inherited Create(valid, lastErrorNo, lastErrorStr, otUnknown, 0);
  if FValid then begin
    FName        := name;
    FSize        := size;
    FWriteAccess := writeAccess;
    FBuffer      := buffer;
  end;
end;

destructor TINamedBuffer.Destroy;
begin
  if FBuffer <> nil then
    UnMapViewOfFile(pointer(integer(FBuffer) - 4));
  inherited;
end;

function TINamedBuffer.GetName : AnsiString;
begin
  result := FName;
end;

function TINamedBuffer.GetSize : integer;
begin
  result := FSize;
end;

function TINamedBuffer.GetMemory : pointer;
begin
  result := FBuffer;
end;

function TINamedBuffer.IsWriteAccess : boolean;
begin
  result := FWriteAccess;
end;

function TINamedBuffer.GetMaxInterface : IBasic;
begin
  result := INamedBuffer(self);
end;

function OpenNamedBuffer(name        : AnsiString;
                         writeAccess : boolean = true) : INamedBuffer;
var c1, c2 : cardinal;
    p1     : pointer;
begin
  if name <> '' then begin
    if writeAccess then c1 := FILE_MAP_ALL_ACCESS
    else                c1 := FILE_MAP_READ;
    c2 := OpenFileMappingA(c1, true, PAnsiChar(name));
    if c2 <> 0 then begin
      p1 := MapViewOfFile(c2, c1, 0, 0, 0);
      if p1 <> nil then begin
        result := TINamedBuffer.Create(true, 0, '', name, TPInteger(p1)^, true,
                                       pointer(integer(p1) + 4), c2);
      end else begin
        result := TINamedBuffer.Create(false, GetLastError, '', '', 0, false, nil, 0);
        CloseHandle(c2);
      end;
    end else result := TINamedBuffer.Create(false, GetLastError, '', '', 0, false, nil, 0);
  end else result := TINamedBuffer.Create(false, ERROR_INVALID_PARAMETER, '', '', 0, false, nil, 0);
end;

function NewNamedBuffer(name : AnsiString;
                        size : integer) : INamedBuffer;
var sa : TSecurityAttributes;
    c1 : cardinal;
    p1 : pointer;
begin
  if name <> '' then begin
    c1 := OpenFileMappingA(FILE_MAP_ALL_ACCESS, true, PAnsiChar(name));
    if c1 <> 0 then begin
      p1 := MapViewOfFile(c1, FILE_MAP_ALL_ACCESS, 0, 0, 0);
      if p1 <> nil then begin
        result := TINamedBuffer.Create(true, 0, '', name, TPInteger(p1)^, true,
                                       pointer(integer(p1) + 4), c1);
      end else begin
        result := TINamedBuffer.Create(false, GetLastError, '', '', 0, false, nil, 0);
        CloseHandle(c1);
      end;
    end else
      if size > 0 then begin
        sa.nLength              := sizeOf(TSecurityAttributes);
        sa.lpSecurityDescriptor := nil;
        sa.bInheritHandle       := true;
        c1 := CreateFileMappingA(cardinal(-1), @sa, PAGE_READWRITE, 0, size + 4, PAnsiChar(name));
        if c1 <> 0 then begin
          p1 := MapViewOfFile(c1, FILE_MAP_ALL_ACCESS, 0, 0, 0);
          if p1 <> nil then begin
            TPInteger(p1)^ := size;
            result := TINamedBuffer.Create(true, 0, '', name, size, true,
                                           pointer(integer(p1) + 4), c1);
          end else begin
            result := TINamedBuffer.Create(false, GetLastError, '', '', 0, false, nil, 0);
            CloseHandle(c1);
          end;
        end else
          result := TINamedBuffer.Create(false, GetLastError, '', '', 0, false, nil, 0);
      end else result := TINamedBuffer.Create(false, ERROR_INVALID_PARAMETER, '', '', 0, false, nil, 0);
  end else result := TINamedBuffer.Create(false, ERROR_INVALID_PARAMETER, '', '', 0, false, nil, 0);
end;

// ***************************************************************

// ***************************************************************

{procedure testHandles;
var s1, s2 : AnsiString;
    i1     : integer;

  procedure FillClipboard(str: AnsiString);
  // fill the clipboard with a specific AnsiString
  var mem : dword;
      buf : pointer;
  begin
    if OpenClipboard(0) then begin
      // w2k needs this, the other OSs don't...  :-(
      EmptyClipboard;
      CloseClipboard;
    end;
    if OpenClipboard(0) then begin
      mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Length(str) + 1);
      buf := GlobalLock(mem);
      if buf <> nil then begin
        Move(PAnsiChar(str)^, buf^, Length(str) + 1);
        GlobalUnlock(mem);
        SetClipboardData(CF_TEXT, mem);
      end;
      CloseClipboard;
    end;
  end;

  procedure Add(name: AnsiString; hnd: cardinal);
  begin
    s1 := s1 + #$D#$A + name + ': ';
    if (hnd <> 0) and (hnd <> INVALID_HANDLE_VALUE) then begin
      with Handle(hnd, false) do
        s1 := s1 + ' (' + KernelObj.ObjTypeStr + ') ' + IntToHex(hnd, 1) + '/' +
              IntToHex(Handle, 1) + ':' + (* + ' ' + IntToStr(TIHandle(SelfAsTObject).FTypeNo) +*) ' "' + KernelObj.ObjName + '"';
    end else s1 := s1 + '-';
  end;

var c1    : cardinal;
    arrCh : array [0..MAX_PATH] of AnsiChar;
    wd    : TWSAData;
//    i2    : integer;
begin
  @CreateToolhelp32Snapshot := GetProcAddress(GetModuleHandle(kernel32), 'CreateToolhelp32Snapshot');
  if @CreateToolhelp32Snapshot <> nil then
    Add('Toolhelp snapshot', CreateToolHelp32Snapshot(TH32CS_SnapProcess, 0));
  Add('Waitable Timer', CreateWaitableTimer(nil, true, 'testTimer'));
  Add('Semaphore', CreateSemaphore(nil, 0, 10, 'testSemaphore'));  // 1 1
  Add('Event', CreateEvent(nil, true, false, 'testEvent'));  // 2 2
  Add('Mutex', CreateMutex(nil, false, 'testMutex'));  // 3 3
  DuplicateHandle(windows.GetCurrentProcess, windows.GetCurrentProcess, windows.GetCurrentProcess, @c1, 0, true, DUPLICATE_SAME_ACCESS);
  Add('Process', c1);  // 5 6
  DuplicateHandle(windows.GetCurrentProcess, windows.GetCurrentThread, windows.GetCurrentProcess, @c1, 0, true, DUPLICATE_SAME_ACCESS);
  Add('Thread', c1);  // 6 7
  GetWindowsDirectory(arrCh, MAX_PATH);
  Add('File', CreateFile(PAnsiChar(arrCh + '\regedit.exe'), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0));  // 7 8
  Add('Notification', FindFirstChangeNotification('C:\', false, FILE_NOTIFY_CHANGE_FILE_NAME));  // 8 9
  Add('FileMapping', CreateFileMapping(maxCard, nil, PAGE_READWRITE, 0, 100, nil));  // 11 13
  Add('ComPort', CreateFile('COM2', GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0));  // 12 14
  CreatePipe(c1, c1, nil, 100); Add('Pipe', c1);  // 14 16
  Add('vxd', CreateFile('\\.\vwin32', 0, 0, nil, 0, 0, 0));  // 13 15
  Add('Mailslot', CreateMailslot('\\.\mailslot\mailtest', 100, 100, nil));  // 15 17
  Add('ConsoleInput', GetStdHandle(STD_INPUT_HANDLE));  // 9 10
  Add('ConsoleOutput', GetStdHandle(STD_OUTPUT_HANDLE));  // 10 12
  Add('Directory', CreateFile(arrCh, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0));
  Add('Drive', CreateFile('\\.\C:', GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0));
  c1 := 0;
  OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, c1);
  Add('Token', c1);

  WSAStartup(makeword(2, 0), wd);
  Add('Socket', Socket(PF_INET, SOCK_RAW, IPPROTO_ICMP));  // ?? 19
  Delete(s1, 1, 2);
//  if MessageBox(0, PAnsiChar(s1), 'info', MB_YESNO) <> ID_YES then ExitProcess(0);

  with Processes do
//    for i2 := 0 to ItemCount - 1 do
      with CurrentProcess.(*Items[i2],*) Handles do begin
        s2 := ExtractFileName(ExeFile) + ' has ' + IntToStr(ItemCount) + ' Handles' + #$D#$A;
        i1 := ItemCount - 1;
//        if i1 > 35 then i1 := 35;
        for i1 := 0 to i1 do
          with TIHandle(Items[i1].SelfAsTObject) do
            s2 := s2 + #$D#$A + 'Handle: ' + IntToHex(FHandle, 1) + '; ' +
                                'Type: ' + GetKernelObj.ObjTypeStr + '/' + CObjNames[FType] + (*' (' + IntToStr(FTypeNo) + ')' +*) '; ' +
                                'Access: ' + IntToHex(FAccess, 1);
//        if MessageBox(0, PAnsiChar(s2), 'info', MB_YESNO) <> ID_YES then ExitProcess(0);
      end;
  FillClipboard(s1 + #$D#$A + #$D#$A + s2 + #$D#$A + #$D#$A + 'IsAutoEvent: ' + booleanToChar(Event(CreateEvent(nil, false, false, nil)).IsAutoEvent and (not Event(CreateEvent(nil, true,  false, nil)).IsAutoEvent)));
//  MessageBox(0, PAnsiChar(AnsiString(booleanToChar(Event(CreateEvent(nil, false, false, nil)).IsAutoEvent)) +
//                             booleanToChar(Event(CreateEvent(nil, true,  false, nil)).IsAutoEvent)    ), '+-', 0);
  ExitProcess(0);
end; }

//********************************************

initialization
//  testHandles;
finalization
  InFinalization := true;
end.
