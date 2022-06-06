// ***************************************************************
//  madShell.pas              version:  1.3s  ·  date: 2012-04-05
//  -------------------------------------------------------------
//  shell namespace, short cuts, desktop items, item ID lists...
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2012 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2012-04-05 1.3s fixed bug in PathToIDList
// 2010-03-25 1.3r added support for desktop icon positions for newer OSs
// 2009-02-09 1.3q Delphi 2009 support
// 2006-09-07 1.3p (1) IShellObj.GetIcon improved
//                 (2) winFolder improved when run on TerminalServer
//                 (3) UnregisterShellEvent always returned false
// 2006-01-05 1.3o "watchSubtree = false" didn't work correctly (winNT)
// 2005-06-26 1.3n bug in IShellObj.RefreshItems fixed
// 2005-01-02 1.3m file change notifications were surpressed, sometimes
// 2004-08-06 1.3l (1) IShellObj.SetName added
//                 (2) IShellObj.GetIcon: workaround for Windows bug
// 2004-03-14 1.3k IShellObj.GetIcon added
// 2004-01-10 1.3j (1) file notifications use ReadDirectoryChangesW now (winNT)
//                 (2) seOrderChanged/MsiChanged/MsiUninstalled bugs fixed
// 2003-10-05 1.3i dynamic linking of undocumented APIs -> BCB support
// 2003-06-09 1.3h (1) DisplayMode now accepts a specific refresh rate
//                 (2) IShortCut.SetPath added
//                 (3) IShellObj enumeration missed some items
//                 (4) IShortCut.Save optionally skips resolving
//                 (5) IShortCut.FDirty flag was not always set correctly
// 2002-12-05 1.3g Desktop icon positions didn't work in XP
// 2002-06-04 1.3f little NT4 bug workaround, see TIShellObj.ShowContextMenu
// 2002-03-27 1.3e reorganized special folders, some renamed & several new items
// 2001-04-26 1.3d special folders "sfDialupNetwork" and "sfTempFolder" added
// 2001-02-22 1.3c little parsing error in PathToIDList fixed
// 2001-01-28 1.3b SystemFolder renamed to SysFolder
// 2001-01-26 1.3a bug fixed in TIShellObj.NeedName
// 2000-11-22 1.3  RegisterShellEvent (and related) functionality added
// 2000-08-09 1.2b bug fixed in TIShellObj.ShowContextMenu (TrackPopupMenuEx)

unit madShell;

{$I mad.inc}

interface

uses Windows, ShlObj, madBasic, madTypes;

// ***************************************************************

type
  TSpecialFolder = (
    // objects, no file system path available
    sfDesktopObj,          // desktop object
    sfMyComputer,          // my computer
    sfNetworkObj,          // My Network Places object
    sfRecycleBin,          // recycle bin
    sfControlPanel,        // control panel
    sfPrintersObj,         // printers
    sfDialupNetwork,       // network and dialup connections
    sfMyDocumentsObj,      // my documents object
    sfIEObj,               // internet explorer object
    sfComputersNearMe,     // computers near me (workgroup)

    // system directories, user independent
    sfWindows,             // windows folder
    sfSystem,              // system(32) folder
    sfSystemX86,           // RISC: x86 system directory
    sfFonts,               // fonts
    sfResources,           // resource directory
    sfLocalizedResources,  // localized resource directory
    sfProgramFiles,        // program files
    sfProgramFilesX86,     // RISC: x86 program files directory
    sfCommonFiles,         // program files \ common
    sfCommonFilesX86,      // RISC: x86 program files \ common folder
    sfTemp,                // windows temp folder
    sfProfiles,            // root directory of all user profiles

    // directories of the current user
    sfMyProfile,           // <user> \ .
    sfDesktopDir,          // <user> \ desktop
    sfStartMenu,           // <user> \ start menu
    sfPrograms,            // <user> \ start menu \ programs
    sfStartup,             // <user> \ start menu \ programs \ startup
    sfAltStartup,          // <user> \ non localized startup
    sfMyDocumentsDir,      // <user> \ my documents
    sfMyMusic,             // <user> \ my music
    sfMyPictures,          // <user> \ my pictures
    sfMyVideo,             // <user> \ my video
    sfTemplates,           // <user> \ templates
    sfFavorites,           // <user> \ favorites
    sfIECache,             // <user> \ ie cache folder
    sfIECookies,           // <user> \ ie cookie folder
    sfIEHistory,           // <user> \ ie history folder
    sfRecent,              // <user> \ recent
    sfSendTo,              // <user> \ sendto
    sfAppData,             // <user> \ application data
    sfLocalSettings,       // <user> \ local settings \ .
    sfLocalAppData,        // <user> \ local settings \ application data
    sfCdBurning,           // <user> \ local settings \ application data \ microsoft \ cd burning
    sfPrintersDir,         // <user> \ printhood
    sfNetworkDir,          // <user> \ nethood
    sfAdminTools,          // <user> \ administrative tools

    // directories of all users
    sfAllUsersProfile,     // all users \ .
    sfAllUsersDesktopDir,  // all users \ desktop
    sfAllUsersStartMenu,   // all users \ start menu
    sfAllUsersPrograms,    // all users \ start menu \ programs
    sfAllUsersStartup,     // all users \ start menu \ programs \ startup
    sfAllUsersAltStartup,  // all users \ non localized startup
    sfAllUsersDocuments,   // all users \ documents
    sfAllUsersMusic,       // all users \ music
    sfAllUsersPictures,    // all users \ pictures
    sfAllUsersVideo,       // all users \ video
    sfAllUsersTemplates,   // all users \ templates
    sfAllUsersFavorites,   // all users \ favorites
    sfAllUsersAppData,     // all users \ application data
    sfAllUsersAdminTools,  // all users \ administrative tools
    sfAllUsersOemLinks     // all users \ links to OEM specific apps
  );

// get the path of a special folder
function GetSpecialFolder (sf: TSpecialFolder; out path: AnsiString) : boolean;

// get the path of the windows folder or system folder
function WinFolder : AnsiString;
function SysFolder : AnsiString;

// ***************************************************************

type
  // functions for managing item ID lists
  IIDList = interface (IBasic) ['{00BED960-C78D-11D3-A530-00005A180D69}']
    // get a pointer to Windows' PItemIDList type
    function GetPIdl : PItemIDList;
    property PIdl    : PItemIDList read GetPIdl;

    // get the size of this IDList
    function GetSize : integer;
    property Size    : integer read GetSize;

    // extract the path or the name of the IDList, this IDList remains unchanged
    function GetName : IIDList;
    function GetPath : IIDList;
    property Name    : IIDList read GetName;
    property Path    : IIDList read GetPath;

    // is this IDList equal to the other one?
    function IsEqual (const otherIDList: IIDList) : boolean;

    // build a new IDList which consists of this one and the "idList" parameter
    // this IDList remains unchanged
    function Concat (const idList: IIDList) : IIDList;

    // get a copy of this IDList, this IDList remains unchanged
    function Clone : IIDList;

    // may the user change this IDList?
    // caution: if you set this property to "true", you can't undo that
    function  GetReadOnly : boolean;
    procedure SetReadOnly (value: boolean);
    property  ReadOnly    : boolean read GetReadOnly write SetReadOnly;

    // cut and return the name from this IDList (fails in read only mode)
    function SplitName : IIDList;

    // appends the "idList" parameter to the current IDList (fails in read only mode)
    procedure Append (const idList: IIDList);
  end;

// PIdl -> IIDList
function IDList (pidl: PItemIDList; copy: boolean = false) : IIDList;

// ***************************************************************

type
  // encapsulation of the IShellLink COM object
  IShortCut = interface (IBasic) ['{00BED963-C78D-11D3-A530-00005A180D69}']
    // get the IShellLink instance of this short cut
    function GetIShellLink : IShellLinkA;
    property IShellLink_   : IShellLinkA read GetIShellLink;

    // get the file name (& path) of the short cut file
    function  GetShortCutFileName : AnsiString;
    procedure SetShortCutFileName (value: AnsiString);
    property  ShortCutFileName    : AnsiString read GetShortCutFileName write SetShortCutFileName;

    // get the ID list of the linked object
    function GetIDList : IIDList;
    property IDList    : IIDList read GetIDList;

    // get the path of the linked object  (valid only if it belongs to the file system)
    function  GetPath : AnsiString;
    procedure SetPath (value: AnsiString);
    property  Path    : AnsiString read GetPath write SetPath;

    // get a complete TWin32FindData structure for the linked object
    // valid only if it belongs to the file system
    function GetFindData : TWin32FindDataA;
    property FindData    : TWin32FindDataA read GetFindData;

    // get / set several parameters of the short cut
    function  GetDescription : AnsiString;
    function  GetParams      : AnsiString;
    function  GetWorkingDir  : AnsiString;
    function  GetHotKey      : AnsiString;
    function  GetShowCmd     : integer;
    function  GetIconPath    : AnsiString;
    function  GetIconIndex   : integer;
    procedure SetDescription (value: AnsiString);
    procedure SetParams      (value: AnsiString);
    procedure SetWorkingDir  (value: AnsiString);
    procedure SetHotKey      (value: AnsiString);
    procedure SetShowCmd     (value: integer   );
    procedure SetIconPath    (value: AnsiString);
    procedure SetIconIndex   (value: integer   );
    property  Description    : AnsiString read GetDescription write SetDescription;
    property  Params         : AnsiString read GetParams      write SetParams;
    property  WorkingDir     : AnsiString read GetWorkingDir  write SetWorkingDir;
    property  HotKey         : AnsiString read GetHotKey      write SetHotKey;
    property  ShowCmd        : integer    read GetShowCmd     write SetShowCmd;
    property  IconPath       : AnsiString read GetIconPath    write SetIconPath;
    property  IconIndex      : integer    read GetIconIndex   write SetIconIndex;
    function  SetIcon        (iconPath  : AnsiString;
                              iconIndex : integer) : boolean;

    // resolve the short cut
    function Resolve (parentWnd : cardinal = INVALID_HANDLE_VALUE;
                      timeOut   : word     = 3000                ) : boolean;

    // are there any changes made that were not saved yet?
    // changes are automatically saved when this IShortCut object gets destroyed
    function IsDirty : boolean;

    // save the short cut file
    function Save (shortCutFileName : AnsiString = '';
                   tryToResolve     : boolean    = true) : boolean;
  end;

// IShellLink -> IShortCut
function ShortCut (const sli: IShellLinkA) : IShortCut;

// load a short cut file
function LoadShortCut (shortCutFile: AnsiString) : IShortCut;

// create a new short cut to a specific object or file
function NewShortCut (path         : AnsiString    ) : IShortCut; overload;
function NewShortCut (sf           : TSpecialFolder) : IShortCut; overload;
function NewShortCut (const idList : IIDList       ) : IShortCut; overload;
function NewShortCut (const sfi    : IShellFolder  ) : IShortCut; overload;

// ***************************************************************

type
  // type for IShellObj.ShowContextMenu
  TContextAlign = (caLeftTop,   caLeftCenter,  caLeftBottom,
                   caCenterTop, caCenter,      caCenterBottom,
                   caRightTop,  caRightCenter, caRightBottom);

  TIconFlags = set of (ifForShell,       // the icon is for shell browsing
                       ifForShortcut,    // the icon is for a shortcut
                       ifOpenState  );   // get the "open" state of the icon

  // shell namespace functionality and a lot more
  IShellObj = interface (ICustomBasicList) ['{00BED961-C78D-11D3-A530-00005A180D69}']
    // get the ID list that characterizes this shell object
    function GetIDList : IIDList;
    property IDList    : IIDList read GetIDList;

    // get the description (or another display variant) of this shell object
    function GetDescription   : AnsiString;
    function GetDescrInFolder : AnsiString;
    property Description      : AnsiString read GetDescription;
    property DescrInFolder    : AnsiString read GetDescrInFolder;

    // get the name / path of this shell object
    // if the shell object doesn't belong to the file system, the path consists of Guids
    function  GetName : AnsiString;
    function  GetPath : AnsiString;
    procedure SetName (value: AnsiString);
    property  Name    : AnsiString read GetName write SetName;
    property  Path    : AnsiString read GetPath;

    // get the attributes of the shell object
    function GetAttributes : cardinal;
    property Attributes    : cardinal read GetAttributes;

    // is this a folder shell object?
    function IsFolder : boolean;

    // get the Guid, if this shell object has no Guid, it is "{00000000-0000-0000-0000-000000000000}"
    function GetGuid : TGuid;
    property Guid    : TGuid read GetGuid;

    // get the IShellFolder instance of this shell object
    function GetIShellFolder : IShellFolder;
    property IShellFolder_   : IShellFolder read GetIShellFolder;

    // show the context menu of this shell object
    function ShowContextMenu (parentWnd : cardinal;
                              x         : integer       = -1;
                              y         : integer       = -1;
                              align     : TContextAlign = caLeftTop;
                              explore   : boolean       = false    ) : boolean;

    // execute an item of the context menu of this shell object
    // item = '' stands for the default item
    function ExecContextItem (parentWnd : cardinal;
                              item      : AnsiString = '') : boolean;

    // create a new short cut to this shell object
    // give in the file name (&path) now or later when saving the short cut
    function NewShortCut (shortCutFileName: AnsiString = '') : IShortCut;

    // if this shell object is a short cut file: load it
    function LoadShortCut : IShortCut;

    // position on the desktop  (valid only for desktop items)
    function  GetPosition : TPoint;
    procedure SetPosition (pos: TPoint);
    property  Position    : TPoint read GetPosition write SetPosition;

    // state of the desktop item  (valid only for desktop items)
    function GetState : cardinal;
    property State    : cardinal read GetState;

    // get an icon for this shell object - usual size is 16 or 32
    // askAgain =  nil -> correct icon will be returned even if it takes a while
    // askAgain <> nil -> GetIcon will fill the parameter "askAgain" with
    //                    false : correct icon was available quite fast
    //                    true  : simplified icon is returned, ask again later
    //                            without specifiying "askAgain" to get the
    //                            correct icon -> this will be time consuming
    function GetIcon (size     : cardinal   = 32;
                      flags    : TIconFlags = [];
                      askAgain : TPBoolean  = nil) : cardinal;

    // read access to the items of this list
    function GetItem (index: integer) : IShellObj;
    property Items   [index: integer] : IShellObj read GetItem; default;

    // refresh the list, look for new/deleted/changed shell objects
    function RefreshItems : boolean;

    // find a specific item
    function Item (name       : AnsiString) : IShellObj; overload;
    function Item (const name : TGuid     ) : IShellObj; overload;
    function Item (const name : IIDList   ) : IShellObj; overload;

    // get the parent shell object (if any)
    function GetParent : IShellObj;
    property Parent    : IShellObj read GetParent;
  end;

// path/TSpecialFolder/IIDList/IShellFolder -> IShellObj
function ShellObj (path         : AnsiString    ) : IShellObj; overload;
function ShellObj (sf           : TSpecialFolder) : IShellObj; overload;
function ShellObj (const idList : IIDList       ) : IShellObj; overload;
function ShellObj (const sfi    : IShellFolder  ) : IShellObj; overload;

// returns the desktop root shell object
// the items of this object have valid "position" properties
function Desktop : IShellObj;

// ***************************************************************

type
  TShellEventType = (seItemCreated, seItemRenamed, seItemChanged, seItemDeleted,     // item
                     seDirCreated,  seDirRenamed,  seDirChanged,  seDirDeleted,      // dir
                     seAttributesChanged,                                            // item & dir
                     seFreespaceChanged,                                             // drive
                     seDriveAdded, seDriveAddedGui, seDriveRemoved,                  // drive
                     seMediaInserted, seMediaRemoved,                                // medium
                     seShareAdded, seShareRemoved, seServerDisconnect,               // network
                     seImageChanged, seAssociationChanged,                           // etc.
                     seThemeChanged, seOrderChanged, seMsiChanged, seMsiUninstalled  // extended
                    );
  TShellEventTypes = set of TShellEventType;

const
  CShellEvents_Disk  = [seItemCreated..seAttributesChanged];
  CShellEvents_Share = [seShareAdded..seShareRemoved];
  CShellEvents_All   = [low(TShellEventType)..high(TShellEventType)];

type
  TShellEvent   = procedure (event: TShellEventType; const obj1, obj2: IShellObj; drive: AnsiChar; value: cardinal);
  TShellEventOO = procedure (event: TShellEventType; const obj1, obj2: IShellObj; drive: AnsiChar; value: cardinal) of object;

function RegisterShellEvent (eventProc    : TShellEvent;
                             root         : AnsiString       = '*';
                             watchSubtree : boolean          = true;
                             eventTypes   : TShellEventTypes = CShellEvents_All) : boolean; overload;
function RegisterShellEvent (eventProc    : TShellEventOO;
                             root         : AnsiString       = '*';
                             watchSubtree : boolean          = true;
                             eventTypes   : TShellEventTypes = CShellEvents_All) : boolean; overload;

function UnregisterShellEvent (event: TShellEvent  ) : boolean; overload;
function UnregisterShellEvent (event: TShellEventOO) : boolean; overload;

// ***************************************************************

type
  // type for IDisplayMode.Install
  TDisplayModeInstallResult = (dmOk, dmRestart, dmError);

  // functionality for a display mode that your current graphics card/driver supports
  IDisplayMode = interface (IBasic) ['{00BED964-C78D-11D3-A530-00005A180D69}']
    // basic information about the display mode
    function GetWidth        : integer;
    function GetHeight       : integer;
    function GetBitsPerPixel : integer;
    function GetNoOfColors   : integer;
    function GetRefreshRate  : integer;
    property Width           : integer read GetWidth;
    property Height          : integer read GetHeight;
    property BitsPerPixel    : integer read GetBitsPerPixel;
    property NoOfColors      : integer read GetNoOfColors;
    property RefreshRate     : integer read GetRefreshRate;

    // extended mode information
    function IsInterlaced : boolean;
    function IsGrayScale  : boolean;

    // could we install this display mode without needing to restart?
    // this call does NOT install the display mode
    function IsRestartNecessary : boolean;

    // install this display mode
    function Install (failIfRestartNecessary : boolean = true;
                      fullScreen             : boolean = true;
                      updateRegistry         : boolean = false;
                      forAllUsers            : boolean = false) : TDisplayModeInstallResult;
  end;

  // list of all display modes the current graphics card/driver supports
  IDisplayModes = interface (ICustomBasicList) ['{00BED965-C78D-11D3-A530-00005A180D69}']
    // read access to the items of this list
    function GetItem (index: integer) : IDisplayMode;
    property Items   [index: integer] : IDisplayMode read GetItem; default;

    // find a specific display mode
    function Item (width        : integer;
                   height       : integer;
                   bitsPerPixel : integer) : IDisplayMode;
  end;

// look for a specific display mode
function DisplayMode (width        : integer;
                      height       : integer;
                      bitsPerPixel : integer;
                      refreshRate  : integer = 0) : IDisplayMode;

// information about the current display mode
function CurrentDisplayMode : IDisplayMode;

// restore the display mode that is stored in the registry
function RestoreDisplayMode (dontResizeOtherWindows : boolean = false) : TDisplayModeInstallResult;

// get a list of all display modes, that the current graphics card/driver supports
function DisplayModes : IDisplayModes;

// ***************************************************************

// error codes
const CErrorNo_CantUndoReadOnly  = CErrorBase_Shell + 0;
      CErrorNo_ProtectedIDList   = CErrorBase_Shell + 1;
      CErrorNo_NoDesktopItem     = CErrorBase_Shell + 2;
      CErrorStr_CantUndoReadOnly : PAnsiChar = 'You can''t unprotect an ID list.';
      CErrorStr_ProtectedIDList  : PAnsiChar = 'This ID list is protected!';
      CErrorStr_NoDesktopItem    : PAnsiChar = 'This is no desktop item!';

// ***************************************************************

// internal stuff, please ignore
type TWatchType = (wtAdded, wtRemoved, wtModified, wtRenamed);
     TWatchDirCallback   = procedure (watchType: TWatchType; old, new_: UnicodeString);
     TWatchDirCallbackOO = procedure (watchType: TWatchType; old, new_: UnicodeString) of object;
function WatchDir (dir: UnicodeString; watchSubtree: boolean; events: TShellEventTypes;
                   callback: TWatchDirCallback; callbackOO: TWatchDirCallbackOO) : dword;
function UnwatchDir (var wdh: dword) : boolean;

// ***************************************************************

implementation

uses Messages, SysUtils, ShellAPI, Classes, Consts, ActiveX, ComObj, CommCtrl,
     madStrings, madTools, Menus;

// ***************************************************************

function GetSpecialFolder(sf: TSpecialFolder; out path: AnsiString) : boolean;
begin
  case sf of
    sfWindows : path := WinFolder;
    sfSystem  : path := SysFolder;
    else        path := ShellObj(sf).path;
  end;
  result := path <> '';
end;

var FWinFolder : AnsiString = '';
function WinFolder : AnsiString;
var i1 : integer;
begin
  if FWinFolder = '' then begin
    FWinFolder := SysFolder;
    for i1 := Length(FWinFolder) downto 1 do
      if FWinFolder[i1] = '\' then begin
        Delete(FWinFolder, i1, maxInt);
        break;
      end;
  end;
  result := FWinFolder;
end;

var FSysFolder : AnsiString = '';
function SysFolder : AnsiString;
begin
  if FSysFolder = '' then begin
    SetLength(FSysFolder, MAX_PATH + 1);
    SetLength(FSysFolder, GetSystemDirectoryA(PAnsiChar(FSysFolder), MAX_PATH));
  end;
  result := FSysFolder;
end;

// ***************************************************************

type
  // implements IIDList
  TIIDList = class (TIBasic, IIDList)
  public
    FPidl         : PItemIDList;
    FReadOnly     : boolean;
    FSizeReady    : boolean;
    FSize         : integer;
    FPNameReady   : boolean;
    FPName        : PItemIDList;
    FExtractReady : boolean;
    FPath         : IIDList;
    FName         : IIDList;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        pidl: PItemIDList; copy: boolean = false);
    destructor Destroy; override;

    function GetPIdl : PItemIDList;

    function GetSize : integer;

    function GetName : IIDList;
    function GetPath : IIDList;

    function IsEqual (const otherIDList: IIDList) : boolean;

    function Concat (const idList: IIDList) : IIDList;

    function Clone : IIDList;

    function  GetReadOnly : boolean;
    procedure SetReadOnly (value: boolean);

    function SplitName : IIDList;

    procedure Append (const idList: IIDList);

    // internal functions
    procedure NeedExtract;
    procedure NeedInfo;

    function GetMaxInterface : IBasic; override;
  end;

var FMalloc : IMalloc;

constructor TIIDList.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                            pidl: PItemIDList; copy: boolean = false);
var c1 : cardinal;                            
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FValid := pidl <> nil;
    if FValid then begin
      if FMalloc = nil then
           c1 := cardinal(SHGetMalloc(FMalloc))
      else c1 := 0;
      FValid := c1 = 0;
      if FValid then begin
        FPidl := pidl;
        if copy then begin
          FPidl := FMalloc.Alloc(GetSize);
          Move(pidl^, FPidl^, FSize);
        end;
      end else SetLastError(c1);
    end else SetLastError(ERROR_INVALID_PARAMETER);
  end;
end;

destructor TIIDList.Destroy;
begin
  if FPidl <> nil then FMalloc.Free(FPidl);
  inherited;
end;

function TIIDList.GetPIdl : PItemIDList;
begin
  result := FPidl;
end;

function TIIDList.GetSize : integer;
begin
  if (not FSizeReady) and CheckValid then NeedInfo;
  result := FSize;
end;

function TIIDList.GetName : IIDList;
begin
  if (not FExtractReady) and CheckValid then NeedExtract;
  result := FName;
end;

function TIIDList.GetPath : IIDList;
begin
  if (not FExtractReady) and CheckValid then NeedExtract;
  result := FPath;
end;

function TIIDList.IsEqual(const otherIDList: IIDList) : boolean;
begin
  result := false;
  if CheckValid then
    if (otherIDList <> nil) and otherIDList.IsValid then begin
      result := (GetSize = otherIDList.Size) and
                CompareMem(FPidl, otherIDList.pidl, GetSize);
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIIDList.Concat(const idList: IIDList) : IIDList;
var c1, c2 : cardinal;
    pidl1  : PItemIDList;
begin
  result := nil;
  if CheckValid then
    if (idList <> nil) and idList.IsValid then begin
      c1 := GetSize - 2;
      c2 := idList.Size;
      pidl1 := FMalloc.Alloc(c1 + c2);
      if pidl1 <> nil then begin
        Move(      FPidl^,            pidl1^,        c1);
        Move(idList.pidl^, (PAnsiChar(pidl1) + c1)^, c2);
      end;
      result := TIIDList.Create(true, 0, '', pidl1);
    end else SetLastError(ERROR_INVALID_PARAMETER);
  if result = nil then
    result := TIIDList.Create(false, FLastErrorNo, GetLastErrorStr, nil);
end;

function TIIDList.Clone : IIDList;
var idl1  : TIIDList;
    pidl1 : PItemIDList;
begin
  if CheckValid then begin
    pidl1 := FMalloc.Alloc(GetSize);
    Move(FPidl^, pidl1^, FSize);
    idl1 := TIIDList.Create(true, 0, '', pidl1);
    idl1.FSizeReady := true;
    idl1.FSize      := FSize;
    if FPNameReady then begin
      idl1.FPNameReady := true;
      integer(idl1.FPName) := integer(FPName) - integer(FPidl) + integer(idl1.FPidl);
    end;
    if FExtractReady then begin
      idl1.FExtractReady  := true;
      idl1.FPath          := FPath.Clone;
      idl1.FPath.ReadOnly := true;
      idl1.FName          := FName.Clone;
      idl1.FName.ReadOnly := true;
    end;
    result := idl1;
  end else
    result := TIIDList.Create(false, FLastErrorNo, FLastErrorStr, nil);
end;

function TIIDList.GetReadOnly : boolean;
begin
  result := FReadOnly;
end;

procedure TIIDList.SetReadOnly(value: boolean);
begin
  if CheckValid then
    if FReadOnly <> value then
      if value then
           FReadOnly := true
      else SetLastError(CErrorNo_CantUndoReadOnly, UnicodeString(AnsiString(CErrorStr_CantUndoReadOnly)));
end;

function TIIDList.SplitName: IIDList;
var pidl1  : PItemIDList;
    i1, i2 : integer;
    idl1   : TIIDList;
begin
  result := nil;
  if CheckValid then
    if not FReadOnly then begin
      if FExtractReady then begin
        result := FName;
        with TIIDList(FPath.SelfAsTObject) do begin
          self.FValid        := FValid;
          self.FPidl         := FPidl;
          FPidl := nil;
          self.FReadOnly     := FReadOnly;
          self.FSizeReady    := FSizeReady;
          self.FSize         := FSize;
          self.FPNameReady   := FPNameReady;
          self.FPName        := FPName;
          self.FExtractReady := FExtractReady;
          self.FName         := FName;
          self.FPath         := FPath;
        end;
      end else begin
        if (not FSizeReady) or (not FPNameReady) then NeedInfo;
        i1 := FPName^.mkid.cb;
        i2 := i1 + 2;
        pidl1 := FMalloc.Alloc(i2);
        Move(FPName^, pidl1^, i2);
        if FSize > 2 then begin
          FPidl := FMalloc.ReAlloc(FPidl, FSize - i1);
          TPWord(NativeInt(FPidl) + FSize - i2)^ := 0;
          dec(FSize, i1);
          FPNameReady := false;
        end else begin
          FMalloc.Free(FPidl);
          FValid      := false;
          FPidl       := nil;
          FSizeReady  := true;
          FSize       := 0;
          FPNameReady := true;
          FPName      := nil;
        end;
        idl1 := TIIDList.Create(true, 0, '', pidl1);
        idl1.FSize       := i2;
        idl1.FSizeReady  := true;
        idl1.FPName      := idl1.FPidl;
        idl1.FPNameReady := true;
        idl1.FReadOnly   := true;
        result := idl1;
      end;
    end else SetLastError(CErrorNo_ProtectedIDList, UnicodeString(AnsiString(CErrorStr_ProtectedIDList)));
  if result = nil then
    result := TIIDList.Create(false, FLastErrorNo, GetLastErrorStr, nil);
end;

procedure TIIDList.Append(const idList: IIDList);
var c1, c2 : cardinal;
begin
  if CheckValid then
    if not FReadOnly then begin
      if (idList <> nil) and idList.IsValid then begin
        c1 := GetSize - 2;
        c2 := idList.Size;
        FPidl := FMalloc.ReAlloc(FPidl, c1 + c2);
        Move(idList.pidl^, (PAnsiChar(FPidl) + c1)^, c2);
        inc(FSize, c2 - 2);
        FPNameReady   := true;
        FPName        := pointer(NativeUInt(FPidl) + c1);
        FExtractReady := false;
        FPath         := nil;
        FName         := nil;
      end else SetLastError(ERROR_INVALID_PARAMETER);
    end else SetLastError(CErrorNo_ProtectedIDList, UnicodeString(AnsiString(CErrorStr_ProtectedIDList)));
end;

procedure TIIDList.NeedInfo;
var pidl1 : PItemIDList;
begin
  FSize := 2;
  pidl1 := FPidl;
  FPName := pidl1;
  while pidl1^.mkid.cb > 0 do begin
    FPName := pidl1;
    inc(FSize, pidl1^.mkid.cb);
    NativeUInt(pidl1) := NativeUInt(pidl1) + pidl1^.mkid.cb;
  end;
  FSizeReady  := true;
  FPNameReady := true;
end;

procedure TIIDList.NeedExtract;
var pidl1, pidl2 : PItemIDList;
    idl1         : TIIDList;
    i1, i2       : integer;
begin
  if (not FSizeReady) or (not FPNameReady) then NeedInfo;
  i1 := FPName^.mkid.cb;
  i2 := i1 + 2;
  pidl2 := FMalloc.Alloc(i2);
  Move(FPName^, pidl2^, i2);
  idl1 := TIIDList.Create(true, 0, '', pidl2);
  idl1.FSize       := i2;
  idl1.FSizeReady  := true;
  idl1.FPName      := idl1.FPidl;
  idl1.FPNameReady := true;
  idl1.FReadOnly   := true;
  FName := idl1;
  if FSize > 2 then begin
    pidl1 := FMalloc.Alloc(FSize - i1);
    Move(FPidl^, pidl1^, FSize - i2);
    TPWord(NativeInt(pidl1) + FSize - i2)^ := 0;
    idl1 := TIIDList.Create(true, 0, '', pidl1);
    idl1.FSize      := FSize - i1;
    idl1.FSizeReady := true;
    idl1.FReadOnly  := true;
    FPath := idl1;
  end else
    FPath := TIIDList.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), nil);
  FExtractReady := true;
end;

function TIIDList.GetMaxInterface : IBasic;
begin
  result := IIDList(self);
end;

function IDList(pidl: PItemIDList; copy: boolean = false) : IIDList;
begin
  result := TIIDList.Create(true, 0, '', pidl, copy);
end;

// ***************************************************************

type
  // implements IShortCut
  TIShortCut = class (TIBasic, IShortCut)
  public
    FIDList    : IIDList;
    FFile      : AnsiString;
    FDirty     : boolean;
    FShellLink : IShellLinkA;
    FIconPath  : AnsiString;
    FIconIndex : integer;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        const idList: IIDList; fileName: AnsiString);
    destructor Destroy; override;

    function GetIShellLink : IShellLinkA;

    function  GetShortCutFileName : AnsiString;
    procedure SetShortCutFileName (value: AnsiString);

    function GetIDList : IIDList;

    function  GetPath : AnsiString;
    procedure SetPath (value: AnsiString);

    function GetFindData : TWin32FindDataA;

    function  GetDescription : AnsiString;
    function  GetParams      : AnsiString;
    function  GetWorkingDir  : AnsiString;
    function  GetHotKey      : AnsiString;
    function  GetShowCmd     : integer;
    function  GetIconPath    : AnsiString;
    function  GetIconIndex   : integer;
    procedure SetDescription (value: AnsiString);
    procedure SetParams      (value: AnsiString);
    procedure SetWorkingDir  (value: AnsiString);
    procedure SetHotKey      (value: AnsiString);
    procedure SetShowCmd     (value: integer   );
    procedure SetIconPath    (value: AnsiString);
    procedure SetIconIndex   (value: integer   );
    function  SetIcon        (iconPath  : AnsiString;
                              iconIndex : integer) : boolean;

    function Resolve (parentWnd : cardinal = INVALID_HANDLE_VALUE;
                      timeOut   : word     = 3000                ) : boolean;

    function IsDirty : boolean;

    function Save (shortCutFileName : AnsiString = '';
                   tryToResolve     : boolean    = true) : boolean;

    function GetMaxInterface : IBasic; override;
  end;

constructor TIShortCut.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                              const idList: IIDList; fileName: AnsiString);
var c1    : cardinal;
    pidl1 : PItemIDList;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FValid := (idList = nil) or idList.IsValid;
    if FValid then begin
      CoInitialize(nil);
      c1 := cardinal(CoCreateInstance(CLSID_ShellLink, nil,
                                      CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,
                                      IShellLinkA, FShellLink));
      if c1 = 0 then
        if idList = nil then begin
          c1 := cardinal((FShellLink as IPersistFile).Load(PWideChar(UnicodeString(fileName)), 0));
          if c1 = 0 then begin
            c1 := cardinal(FShellLink.GetIDList(pidl1));
            if c1 = 0 then
              FIDList := TIIDList.Create(true, 0, '', pidl1);
          end;
        end else begin
          c1 := cardinal(FShellLink.SetIDList(idList.pidl));
          if c1 = 0 then begin
            FDirty  := true;
            FIDList := idList;
          end;
        end;
      FValid := c1 = 0;
      if FValid then begin
        FIDList.ReadOnly := true;
        FFile      := fileName;
        FIconIndex := -1;
      end else SetLastError(c1);
    end else SetLastError(ERROR_INVALID_PARAMETER);
  end;
end;

destructor TIShortCut.Destroy;
begin
  if FDirty then Save;
  inherited;
end;

function TIShortCut.GetIShellLink : IShellLinkA;
begin
  result := FShellLink;
end;

function TIShortCut.GetShortCutFileName : AnsiString;
begin
  result := FFile;
end;

procedure TIShortCut.SetShortCutFileName(value: AnsiString);
begin
  if CheckValid then
    if not IsTextEqual(FFile, value) then begin
      FFile  := value;
      FDirty := true;
    end;
end;

function TIShortCut.GetIDList : IIDList;
begin
  if FIDList = nil then
    FIDList := TIIDList.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), nil);
  result := FIDList;
end;

function TIShortCut.GetPath : AnsiString;
var wfd   : TWin32FindDataA;
    arrCh : array [0..MAX_PATH] of AnsiChar;
    c1    : cardinal;
begin
  result := '';
  if CheckValid then begin
    c1 := cardinal(FShellLink.GetPath(arrCh, MAX_PATH, wfd, 0));
    if c1 = 0 then result := arrCh
    else           SetLastError(c1);
  end;
end;

function TIShortCut.GetFindData : TWin32FindDataA;
var arrCh : array [0..MAX_PATH] of AnsiChar;
    c1    : cardinal;
begin
  if CheckValid then begin
    c1 := cardinal(FShellLink.GetPath(arrCh, MAX_PATH, result, 0));
    if c1 <> 0 then SetLastError(c1);
  end;
  if not FSuccess then
    ZeroMemory(@result, sizeOf(TWin32FindData));
end;

function TIShortCut.GetDescription : AnsiString;
var arrCh : array [0..MAX_PATH] of AnsiChar;
    c1    : cardinal;
begin
  result := '';
  if CheckValid then begin
    c1 := cardinal(FShellLink.GetDescription(arrCh, MAX_PATH));
    if c1 = 0 then result := arrCh
    else           SetLastError(c1);
  end;
end;

function TIShortCut.GetParams : AnsiString;
var arrCh : array [0..MAX_PATH] of AnsiChar;
    c1    : cardinal;
begin
  result := '';
  if CheckValid then begin
    c1 := cardinal(FShellLink.GetArguments(arrCh, MAX_PATH));
    if c1 = 0 then result := arrCh
    else           SetLastError(c1);
  end;
end;

function TIShortCut.GetWorkingDir : AnsiString;
var arrCh : array [0..MAX_PATH] of AnsiChar;
    c1    : cardinal;
begin
  result := '';
  if CheckValid then begin
    c1 := cardinal(FShellLink.GetWorkingDirectory(arrCh, MAX_PATH));
    if c1 = 0 then result := arrCh
    else           SetLastError(c1);
  end;
end;

function TIShortCut.GetHotKey : AnsiString;
var w1 : word;
    c1 : cardinal;
begin
  result := '';
  if CheckValid then begin
    c1 := cardinal(FShellLink.GetHotKey(w1));
    if c1 = 0 then begin
      if w1 and (HOTKEYF_ALT     shl 8) <> 0 then w1 := (w1 and (not (HOTKEYF_ALT     shl 8))) or scAlt;
      if w1 and (HOTKEYF_CONTROL shl 8) <> 0 then w1 := (w1 and (not (HOTKEYF_CONTROL shl 8))) or scCtrl;
      if w1 and (HOTKEYF_SHIFT   shl 8) <> 0 then w1 := (w1 and (not (HOTKEYF_SHIFT   shl 8))) or scShift;
      result := AnsiString(ShortCutToText(w1));
    end else SetLastError(c1);
  end;
end;

function TIShortCut.GetShowCmd : integer;
var i1 : integer;
    c1 : cardinal;
begin
  result := 0;
  if CheckValid then begin
    c1 := cardinal(FShellLink.GetShowCmd(i1));
    if c1 = 0 then result := i1
    else           SetLastError(c1);
  end;
end;

function TIShortCut.GetIconPath : AnsiString;
var arrCh : array [0..MAX_PATH] of AnsiChar;
    i1    : integer;
    c1    : cardinal;
begin
  result := '';
  if CheckValid then begin
    c1 := cardinal(FShellLink.GetIconLocation(arrCh, MAX_PATH, i1));
    if c1 = 0 then begin
      result := arrCh;
      FIconPath  := result;
      FIconIndex := i1;
    end else SetLastError(c1);
  end;
end;

function TIShortCut.GetIconIndex : integer;
var arrCh : array [0..MAX_PATH] of AnsiChar;
    i1    : integer;
    c1    : cardinal;
begin
  result := -1;
  if CheckValid then begin
    c1 := cardinal(FShellLink.GetIconLocation(arrCh, MAX_PATH, i1));
    if c1 = 0 then begin
      result := i1;
      FIconPath  := arrCh;
      FIconIndex := result;
    end else SetLastError(c1);
  end;
end;

procedure TIShortCut.SetPath(value: AnsiString);
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := cardinal(FShellLink.SetPath(PAnsiChar(value)));
    if c1 = 0 then FDirty := true
    else           SetLastError(c1);
  end;
end;

procedure TIShortCut.SetDescription(value: AnsiString);
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := cardinal(FShellLink.SetDescription(PAnsiChar(value)));
    if c1 = 0 then FDirty := true
    else           SetLastError(c1);
  end;
end;

procedure TIShortCut.SetParams(value: AnsiString);
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := cardinal(FShellLink.SetArguments(PAnsiChar(value)));
    if c1 = 0 then FDirty := true
    else           SetLastError(c1);
  end;
end;

procedure TIShortCut.SetWorkingDir(value: AnsiString);
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := cardinal(FShellLink.SetWorkingDirectory(PAnsiChar(value)));
    if c1 = 0 then FDirty := true
    else           SetLastError(c1);
  end;
end;

procedure TIShortCut.SetHotKey(value: AnsiString);
var w1 : word;
    c1 : cardinal;
begin
  if CheckValid then begin
    w1 := TextToShortCut(string(value));
    if w1 and scAlt   <> 0 then w1 := (w1 and (not scAlt  )) or (HOTKEYF_ALT     shl 8);
    if w1 and scCtrl  <> 0 then w1 := (w1 and (not scCtrl )) or (HOTKEYF_CONTROL shl 8);
    if w1 and scShift <> 0 then w1 := (w1 and (not scShift)) or (HOTKEYF_SHIFT   shl 8);
    c1 := cardinal(FShellLink.SetHotKey(w1));
    if c1 = 0 then FDirty := true
    else           SetLastError(c1);
  end;
end;

procedure TIShortCut.SetShowCmd(value: integer);
var c1 : cardinal;
begin
  if CheckValid then begin
    c1 := cardinal(FShellLink.SetShowCmd(value));
    if c1 = 0 then FDirty := true
    else           SetLastError(c1);
  end;
end;

procedure TIShortCut.SetIconPath(value: AnsiString);
var c1 : cardinal;
begin
  if CheckValid then begin
    FIconPath := value;
    if FIconIndex = -1 then begin
      FIconIndex := GetIconIndex;
      c1 := cardinal(FShellLink.SetIconLocation(PAnsiChar(FIconPath), FIconIndex));
      if c1 <> 0 then begin
        FIconIndex := 0;
        c1 := cardinal(FShellLink.SetIconLocation(PAnsiChar(FIconPath), FIconIndex));
      end;
    end else
      c1 := cardinal(FShellLink.SetIconLocation(PAnsiChar(FIconPath), FIconIndex));
    if c1 = 0 then FDirty := true
    else           SetLastError(c1);
  end;
end;

procedure TIShortCut.SetIconIndex(value: integer);
var c1 : cardinal;
begin
  if CheckValid then begin
    FIconIndex := value;
    if FIconPath = '' then FIconIndex := GetIconIndex;
    c1 := cardinal(FShellLink.SetIconLocation(PAnsiChar(FIconPath), FIconIndex));
    if c1 = 0 then FDirty := true
    else           SetLastError(c1);
  end;
end;

function TIShortCut.SetIcon(iconPath: AnsiString; iconIndex: integer) : boolean;
var c1 : cardinal;
begin
  result := false;
  if CheckValid then begin
    FIconPath  := iconPath;
    FIconIndex := iconIndex;
    c1 := cardinal(FShellLink.SetIconLocation(PAnsiChar(iconPath), iconIndex));
    if c1 = 0 then begin
      FDirty := true;
      result := true;
    end else
      SetLastError(c1);
  end;
end;

function TIShortCut.Resolve(parentWnd : cardinal = INVALID_HANDLE_VALUE;
                            timeOut   : word     = 3000                ) : boolean;
var c1 : cardinal;
begin
  result := false;
  if CheckValid then begin
    if parentWnd <> INVALID_HANDLE_VALUE then
         c1 := SLR_ANY_MATCH
    else c1 := SLR_NO_UI;
    if timeOut = 0 then timeOut := 1;
    c1 := cardinal(FShellLink.Resolve(parentWnd, c1 or (cardinal(timeOut) shl 16)));
    if c1 = 0 then result := true
    else           SetLastError(c1);
  end;
end;

function TIShortCut.IsDirty : boolean;
begin
  result := FDirty;
end;

function TIShortCut.Save(shortCutFileName: AnsiString = ''; tryToResolve: boolean = true) : boolean;
var s1 : AnsiString;
    c1 : cardinal;
begin
  result := false;
  if CheckValid then
    if (FFile <> '') or (shortCutFileName <> '') then begin
      try
        if FDirty or ((shortCutFileName <> '') and (not IsTextEqual(shortCutFileName, FFile))) then begin
          if shortCutFileName = '' then shortCutFileName := FFile;
          s1 := AnsiString(ExtractFileExtW(string(shortCutFileName)));
          if IsTextEqual(s1, AnsiString('.pif')) or IsTextEqual(s1, AnsiString('.lnk')) then
            DeleteR(shortCutFileName, 4);
          if (not tryToResolve) or Resolve(INVALID_HANDLE_VALUE, 0) then begin
            c1 := cardinal((FShellLink as IPersistFile).Save(PWideChar(UnicodeString(shortCutFileName + '.lnk')), true));
            if c1 = 0 then result := true
            else           SetLastError(c1);
          end;
        end else result := true;
      except
        result := false;
        SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
      end;
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIShortCut.GetMaxInterface : IBasic;
begin
  result := IShortCut(self);
end;

function ShortCut(const sli: IShellLinkA) : IShortCut;
var c1    : cardinal;
    pidl1 : PItemIDList;
begin
  if sli <> nil then begin
    c1 := cardinal(sli.GetIDList(pidl1));
    if c1 = 0 then
         result := TIShortCut.Create(true, 0, '', IDList(pidl1), '')
    else result := TIShortCut.Create(false, c1, '', nil, '');
  end else result := TIShortCut.Create(false, ERROR_INVALID_PARAMETER, '', nil, '');
end;

function LoadShortCut(shortCutFile: AnsiString) : IShortCut;
begin
  result := TIShortCut.Create(true, 0, '', nil, shortCutFile);
end;

function NewShortCut(path: AnsiString) : IShortCut;
begin
  result := ShellObj(path).NewShortCut('');
end;

function NewShortCut(sf: TSpecialFolder) : IShortCut;
begin
  result := ShellObj(sf).NewShortCut('');
end;

function NewShortCut(const idList: IIDList) : IShortCut;
begin
  result := ShellObj(idList).NewShortCut('');
end;

function NewShortCut(const sfi: IShellFolder) : IShortCut;
begin
  result := ShellObj(sfi).NewShortCut('');
end;

// ***************************************************************

type
  // implements IShellObj
  TIShellObj = class (TICustomBasicList, IShellObj)
  public
    FRefreshReady           : boolean;
    FAttribReady            : boolean;
    FSplittedIDListsReady   : boolean;
    FHasSplittedIDLists     : boolean;
    FShellFolderReady       : boolean;
    FHasShellFolder         : boolean;
    FParentShellFolderReady : boolean;
    FHasParentShellFolder   : boolean;
    FDescriptionReady       : boolean;
    FDescrInFolderReady     : boolean;
    FNameReady              : boolean;
    FGuidReady              : boolean;
    FIDList                 : IIDList;
    FChildIDList            : IIDList;
    FParentIDList           : IIDList;
    FShellFolder            : IShellFolder;
    FParentShellFolder      : IShellFolder;
    FAttrib                 : cardinal;
    FFolder                 : TExtBool;
    FDescription            : AnsiString;
    FDescrInFolder          : AnsiString;
    FName                   : AnsiString;
    FPath                   : AnsiString;
    FGuid                   : TGuid;
    FDesktopItem            : boolean;
    FPos                    : TPoint;
    FState                  : cardinal;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        const parentShellFolder: IShellFolder;
                        const idList, childIdList: IIDList);

    function GetIDList : IIDList;

    function GetDescription   : AnsiString;
    function GetDescrInFolder : AnsiString;

    function  GetName : AnsiString;
    function  GetPath : AnsiString;
    procedure SetName (value: AnsiString);

    function GetAttributes : cardinal;

    function IsFolder : boolean;

    function GetGuid : TGuid;

    function GetIShellFolder : IShellFolder;

    function ShowContextMenu (parentWnd : cardinal;
                              x         : integer       = -1;
                              y         : integer       = -1;
                              align     : TContextAlign = caLeftTop;
                              explore   : boolean       = false    ) : boolean;

    function ExecContextItem (parentWnd : cardinal;
                              item      : AnsiString = '') : boolean;

    function NewShortCut (shortCutFileName: AnsiString = '') : IShortCut;

    function LoadShortCut : IShortCut;

    function  GetPosition : TPoint;
    procedure SetPosition (pos: TPoint);

    function GetState : cardinal;

    function GetIcon (size     : cardinal   = 32;
                      flags    : TIconFlags = [];
                      askAgain : TPBoolean  = nil) : cardinal;

    function GetItemCount : integer; override;

    function GetItem (index: integer) : IShellObj;

    function RefreshItems : boolean;

    function Item (name       : AnsiString) : IShellObj; overload;
    function Item (const name : TGuid     ) : IShellObj; overload;
    function Item (const name : IIDList   ) : IShellObj; overload;

    function GetParent : IShellObj;

    // internal functions
    function  NeedShellFolder       : boolean;
    function  NeedStr               (shgdn : cardinal; out str: AnsiString) : boolean;
    function  NeedSplittedIDLists   : boolean;
    function  NeedParentShellFolder : boolean;
    function  NeedGuid              : boolean;
    procedure NeedName;

    function GetMaxInterface : IBasic; override;
  end;

var
  FDesktopReady  : boolean = false;
  FDesktop       : IShellFolder;

  // documented functions for shared memory allocation  (only winNt)
  VirtualAllocEx : function (ph        : cardinal;
                             addr      : pointer;
                             size      : cardinal;
                             allocType : cardinal;
                             protect   : cardinal) : pointer stdcall = nil;
  VirtualFreeEx  : function (ph        : cardinal;
                             addr      : pointer;
                             size      : cardinal;
                             freeType  : cardinal) : pointer stdcall = nil;

  // undocumented functions for shared memory allocation  (only win9x)
  SharedMem9x_Alloc : function (size : cardinal) : pointer  stdcall = nil;
  SharedMem9x_Free  : function (ptr  : pointer ) : longBool stdcall = nil;

function AllocMemEx(processHandle, size: dword) : pointer;
begin
  result := nil;
  if OS.winNt then begin
    if @VirtualAllocEx = nil then
      VirtualAllocEx := GetProcAddress(GetModuleHandle(kernel32), 'VirtualAllocEx');
    if @VirtualAllocEx <> nil then
      result := VirtualAllocEx(processHandle, nil, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  end else begin
    if @SharedMem9x_Alloc = nil then
      SharedMem9x_Alloc := GetProcAddress(LoadLibraryA('comctl32.dll'), PAnsiChar(71));
    if @SharedMem9x_Alloc <> nil then
      result := SharedMem9x_Alloc(size);
  end;
end;

procedure FreeMemEx(processHandle: dword; buf: pointer);
begin
  if OS.winNt then begin
    if @VirtualFreeEx = nil then
      VirtualFreeEx := GetProcAddress(GetModuleHandle(kernel32), 'VirtualFreeEx');
    if @VirtualFreeEx <> nil then
      VirtualFreeEx(processHandle, buf, 0, MEM_RELEASE);
  end else begin
    if @SharedMem9x_Free = nil then
      SharedMem9x_Free := GetProcAddress(LoadLibraryA('comctl32.dll'), PAnsiChar(73));
    if @SharedMem9x_Free <> nil then
      SharedMem9x_Free(buf);
  end;
end;

constructor TIShellObj.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                              const parentShellFolder: IShellFolder;
                              const idList, childIdList: IIDList);
var c1 : cardinal;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  FPos.x := -1;
  FPos.y := -1;
  FFolder := other;
  if FValid then begin
    CoInitialize(nil);
    if not FDesktopReady then begin
      FDesktopReady := true;
      c1 := cardinal(SHGetDesktopFolder(FDesktop));
    end else c1 := 0;
    FValid := (c1 = 0) and (FDesktop <> nil);
    if FValid then begin
      FValid := (idList <> nil) and (idList.IsValid);
      if FValid then begin
        if childIdList <> nil then begin
          if parentShellFolder <> nil then begin
            FParentShellFolderReady := true;
            FHasParentShellFolder   := true;
            FParentShellFolder      := parentShellFolder;
          end;
          FParentIDList := idList;
          FChildIDList  := childIdList;
          FParentIDList.ReadOnly := true;
          FChildIDList .ReadOnly := true;
          FIDList := FParentIDList.Clone;
          FIDList.Append(FChildIdList);
        end else FIDList := idList;
        FIDList.ReadOnly := true;
      end else SetLastError(ERROR_INVALID_PARAMETER);
    end else SetLastError(c1);
  end;
end;

function TIShellObj.GetIDList : IIDList;
begin
  if FIDList = nil then
    FIDList := TIIDList.Create(false, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)), nil);
  result := FIDList;
end;

function TIShellObj.GetDescription : AnsiString;
begin
  if (not FDescriptionReady) and CheckValid then begin
    FDescriptionReady := true;
    NeedStr(SHGDN_NORMAL, FDescription);
  end;
  result := FDescription;
end;

function TIShellObj.GetDescrInFolder : AnsiString;
begin
  if (not FDescrInFolderReady) and CheckValid then begin
    FDescrInFolderReady := true;
    NeedStr(SHGDN_INFOLDER, FDescrInFolder);
  end;
  result := FDescrInFolder;
end;

function TIShellObj.GetName : AnsiString;
begin
  if (not FNameReady) and CheckValid then NeedName;
  result := FName;
end;

function TIShellObj.GetPath : AnsiString;
begin
  if (not FNameReady) and CheckValid then NeedName;
  result := FPath;
end;

procedure TIShellObj.SetName(value: AnsiString);
var c1    : dword;
    pidl1 : PItemIDList;
begin
  if CheckValid and NeedParentShellFolder and NeedSplittedIDLists then begin
    c1 := FParentShellFolder.SetNameOf(0, FChildIDList.pidl, PWideChar(UnicodeString(value)), 0, pidl1);
    if c1 = 0 then
      FMalloc.Free(pidl1)
    else
      SetLastError(c1);
  end;
end;

function TIShellObj.GetAttributes : cardinal;
var c1, c2 : cardinal;
    pidl1  : PItemIDList;
begin
  if (not FAttribReady) and CheckValid and NeedParentShellFolder and NeedSplittedIDLists then begin
    FAttribReady := true;
    pidl1 := FChildIDList.pidl;
    c2 := $FFFFFFFF;
    c1 := cardinal(FParentShellFolder.GetAttributesOf(1, pidl1, c2));
    if c1 and $80000000 = 0 then
         FAttrib := c2
    else SetLastError(c1);
  end;
  result := FAttrib;
end;

function TIShellObj.IsFolder : boolean;
begin
  if (FFolder = other) and CheckValid then
    FFolder := TExtBool(GetAttributes and (SFGAO_FOLDER or SFGAO_HASSUBFOLDER) <> 0);
  result := FFolder = yes;
end;

function TIShellObj.GetGuid : TGuid;
begin
  if (not FGuidReady) and CheckValid then NeedGuid;
  result := FGuid;
end;

function TIShellObj.GetIShellFolder : IShellFolder;
begin
  if (not FShellFolderReady) and CheckValid then NeedShellFolder;
  result := FShellFolder;
end;

function ShellObjContextMenuWndProc(wnd, msg: cardinal; wParam, lParam: integer) : integer; stdcall;
var cm  : IContextMenu;
    cm3 : IContextMenu3;
    c1  : LRESULT;
begin
  case Msg of
    WM_CREATE:
      begin
        SetWindowLong(wnd, GWL_USERDATA, integer(PCreateStruct(lParam).lpCreateParams));
        if IsWindowUnicode(wnd) then
          result := DefWindowProcA(wnd, msg, wParam, lParam)
        else
          result := DefWindowProcW(wnd, msg, wParam, lParam);
      end;
    WM_DRAWITEM, WM_MEASUREITEM, WM_INITMENUPOPUP:
      begin
        cm := IContextMenu(GetWindowLong(wnd, GWL_USERDATA));
        if cm.QueryInterface(IContextMenu3, cm3) <> NO_ERROR then begin
          (cm as IContextMenu2).HandleMenuMsg(msg, wParam, lParam);
          if msg = WM_INITMENUPOPUP then result := 0
          else                           result := 1;
        end else begin
          cm3.HandleMenuMsg2(msg, wParam, lParam, c1);
          result := c1;
        end;
      end;
    else
      if IsWindowUnicode(wnd) then
        result := DefWindowProcA(wnd, msg, wParam, lParam)
      else
        result := DefWindowProcW(wnd, msg, wParam, lParam);
  end;
end;

function TIShellObj.ShowContextMenu(parentWnd : cardinal;
                                    x         : integer       = -1;
                                    y         : integer       = -1;
                                    align     : TContextAlign = caLeftTop;
                                    explore   : boolean       = false    ) : boolean;
var cm         : IContextMenu;
    cm2        : IContextMenu2;
    cm3        : IContextMenu3;
    ici        : TCMInvokeCommandInfo;
    c1, c2, c3 : cardinal;
    wnd        : cardinal;
    wndCl      : TWndClassA;
    p          : TPoint;
    pidl1      : PItemIDList;
    i1         : integer;
begin
  result := false;
  if CheckValid and NeedParentShellFolder and NeedSplittedIDLists then begin
    pidl1 := FChildIDList.pidl;
    c1 := cardinal(FParentShellFolder.GetUIObjectOf(INVALID_HANDLE_VALUE, 1, pidl1, IContextMenu, nil, pointer(cm)));
    if c1 = 0 then begin
      c2  := CreatePopupMenu;
      wnd := 0;
      try
        if explore then c3 := CMF_EXPLORE
        else            c3 := CMF_NORMAL;
        c1 := cardinal(cm.QueryContextMenu(c2,0,1,$7FFF,c3));
        if c1 and $80000000 = 0 then begin
          if GetASyncKeyState(VK_RBUTTON) and $8000 <> 0 then c3 := TPM_RIGHTBUTTON
          else                                                c3 := TPM_LEFTBUTTON;
          if      align in [caLeftTop,    caLeftCenter,   caLeftBottom ] then c3:=c3 or TPM_LEFTALIGN
          else if align in [caRightTop,   caRightCenter,  caRightBottom] then c3:=c3 or TPM_RIGHTALIGN
          else                                                                c3:=c3 or TPM_CENTERALIGN;
          if      align in [caLeftTop,    caCenterTop,    caRightTop   ] then c3:=c3 or TPM_TOPALIGN
          else if align in [caLeftBottom, caCenterBottom, caRightBottom] then c3:=c3 or TPM_BOTTOMALIGN
          else                                                                c3:=c3 or TPM_VCENTERALIGN;
          if (cm.QueryInterface(IContextMenu2, cm2) = NO_ERROR) or
             (cm.QueryInterface(IContextMenu3, cm3) = NO_ERROR) then begin
            ZeroMemory(@wndCl, sizeOf(wndCl));
            wndCl.hInstance := HInstance;
            with wndCl do begin
              style         := CS_PARENTDC;
              lpszClassName := 'ShellObjContextSubMenuHelperClass';
              lpfnWndProc   := @ShellObjContextMenuWndProc;
            end;
            windows.RegisterClassA(wndCl);
            // in NT4 you sometimes have to give exactly the same *pointer*
            // which you used in RegisterClass, the same *string* sometimes fails
            wnd := CreateWindowA(wndCl.lpszClassName, 'ShellObjContextSubMenuHelper',
                                 WS_POPUPWINDOW, 0, 0, 0, 0, 0, 0, HInstance, pointer(cm));
            if wnd <> 0 then parentWnd := wnd;
          end;
          if (x = -1) and (y = -1) then begin
            GetCursorPos(p);
            x := p.x;
            y := p.y;
          end;
          longBool(i1) := TrackPopupMenuEx(c2, c3 or TPM_RETURNCMD, x, y, parentWnd, nil);
          if i1 <> 0 then begin
            ZeroMemory(@ici, sizeOf(ici));
            with ici do begin
              cbSize := sizeOf(TCMInvokeCommandInfo);
              fMask  := CMIC_MASK_FLAG_NO_UI;
              hwnd   := parentWnd;
              lpVerb := MakeIntResourceA(i1 - 1);
              nShow  := SW_SHOW;
            end;
            cm.InvokeCommand(ici);
          end else SetLastError(GetLastError);
        end else SetLastError(c1 and $FFFF);
      finally
        DestroyMenu(c2);
        if wnd <> 0 then DestroyWindow(wnd);
      end;
    end else SetLastError(c1);
  end;
end;

function TIShellObj.ExecContextItem(parentWnd : cardinal;
                                    item      : AnsiString = '') : boolean;

  function FindMenuItem(menu: cardinal; item: AnsiString) : cardinal;
  var i1, i2, i3 : integer;
      s1, s2, s3 : AnsiString;
      mii        : TMenuItemInfoA;
      arrCh      : array [0..MAX_PATH] of AnsiChar;
  begin
    result := 0;
    i1 := GetMenuItemCount(menu);
    for i2 := 0 to i1 - 1 do begin
      ZeroMemory(@mii, sizeOf(mii));
      mii.cbSize     := sizeOf(mii) - 4;
      mii.fMask      := MIIM_STATE or MIIM_TYPE or MIIM_SUBMENU;
      mii.cch        := MAX_PATH;
      mii.dwTypeData := arrCh;
      if GetMenuItemInfoA(menu, i2, true, mii) and
         (mii.fType and MFT_STRING = MFT_STRING) and (mii.cch > 0) then begin
        s1 := mii.dwTypeData;
        s2 := s1;
        KillChar(s2, '&');
        if mii.hSubMenu <> 0 then begin
          i3 := Pos(AnsiString('|'), item);
          if i3 > 0 then begin
            s3 := Copy(item, 1, i3 - 1);
            if IsTextEqual(s3, s1) or IsTextEqual(s3, s2) then begin
              result := FindMenuItem(GetSubMenu(menu, i2), Copy(item, i3 + 1, maxInt));
              if result > 0 then break;
            end;
          end;
        end else
          if IsTextEqual(item, s1) or IsTextEqual(item, s2) then begin
            result := GetMenuItemID(menu, i2);
            break;
          end;
      end;
    end;
  end;

var cm         : IContextMenu;
    ici        : TCMInvokeCommandInfo;
    c1, c2, c3 : cardinal;
    pidl1      : PItemIDList;
begin
  result := false;
  if CheckValid and NeedParentShellFolder and NeedSplittedIDLists then begin
    pidl1 := FChildIDList.pidl;
    c1 := cardinal(FParentShellFolder.GetUIObjectOf(INVALID_HANDLE_VALUE, 1, pidl1, IContextMenu, nil, pointer(cm)));
    if c1 = 0 then begin
      zeroMemory(@ici,sizeOf(ici));
      with ici do begin
        cbSize := sizeOf(TCMInvokeCommandInfo);
        fMask  := CMIC_MASK_FLAG_NO_UI;
        hwnd   := parentWnd;
        lpVerb := PAnsiChar(item);
        nShow  := SW_SHOW;
      end;
      result := (item <> '') and (cm.InvokeCommand(ici) = 0);
      if not result then begin
        c2 := CreatePopupMenu;
        try
          if item = '' then c3 := CMF_DEFAULTONLY
          else              c3 := CMF_NORMAL;
          c1 := cardinal(cm.QueryContextMenu(c2, 0, 1, $7FFF, c3));
          if c1 and $80000000 = 0 then begin
            if item <> '' then c3 := FindMenuItem(c2, item)
            else               c3 := GetMenuDefaultItem(c2, 0, 0);
            if c3 <> 0 then begin
              ici.lpVerb := PAnsiChar(c3 - 1);
              c1 := cardinal(cm.InvokeCommand(ici));
              if c1 = 0 then result := true
              else           SetLastError(c1);
            end else SetLastError(ERROR_FILE_NOT_FOUND);
          end else SetLastError(c1 and $FFFF);
        finally DestroyMenu(c2) end;
      end;
    end else SetLastError(c1);
  end;
end;

function TIShellObj.NewShortCut(shortCutFileName: AnsiString) : IShortCut;
begin
  result := TIShortCut.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)),
                              FIDList, shortCutFileName);
end;

function TIShellObj.LoadShortCut : IShortCut;
begin
  result := TIShortCut.Create(FValid, CErrorNo_AmInvalid, UnicodeString(AnsiString(CErrorStr_AmInvalid)),
                              nil, GetPath);
end;

function TIShellObj.GetPosition : TPoint;
begin
  result := FPos;
end;

function GetDesktopListView : HWND;

  function EnumFunc(AParent: HWND; out AWindow: HWND) : BOOL; stdcall;
  var ClassName : array [Byte] of Char;
  begin
    AWindow := AParent;
    ClassName[0] := #0;
    GetClassName(AParent, ClassName, Length(ClassName));
    Result := lstrcmp(ClassName, 'SHELLDLL_DefView') <> 0;
  end;

begin
  Result := FindWindow('Progman', nil);
  if Result = 0 then
    Exit;
  Result := FindWindowEx(Result, 0, 'SHELLDLL_DefView', nil);
  if Result = 0 then
    Exit;
  if FindWindowEx(Result, 0, 'HTML_Internet Explorer', nil) <> 0 then
    EnumChildWindows(Result, TFNWndEnumProc(@EnumFunc), LPARAM(@Result));
  Result := FindWindowEx(Result, 0, 'SysListView32', nil);
end;

var
  Done64Init     : boolean = false;
  IsWow64Process : function (processHandle: dword; var wow64Process: bool) : bool; stdcall = nil;
  Am64OS         : boolean = false;

function Is64bitProcess(processHandle: dword) : bool; stdcall;
const PROCESSOR_ARCHITECTURE_AMD64 = 9;
var gnsi : procedure (var si: TSystemInfo) stdcall;
    si   : TSystemInfo;
    b1   : bool;
begin
  if not Done64Init then begin
    gnsi := GetProcAddress(GetModuleHandle(kernel32), 'GetNativeSystemInfo');
    if @gnsi <> nil then begin
      ZeroMemory(@si, sizeOf(si));
      gnsi(si);
      Am64OS := si.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64;
      if Am64OS then
        IsWow64Process := GetProcAddress(GetModuleHandle(kernel32), 'IsWow64Process');
    end;
  end;
  result := Am64OS and ((@IsWow64Process = nil) or (not IsWow64Process(processHandle, b1)) or (not b1));
end;

type
  TLvFindInfo32 = packed record
    flags       : dword;
    psz         : dword;
    lParam      : dword;
    pt          : TPoint;
    vkDirection : dword;
  end;
  TLvFindInfo64 = packed record
    flags       : dword;
    alignment   : dword;
    psz         : int64;
    lParam      : int64;
    pt          : TPoint;
    vkDirection : dword;
  end;
  TLvFindInfoBuffer = packed record
    case Integer of
      0: (
        // Shared members
        flags : dword;
      );
      32: (LVItem32 : TLvFindInfo32);
      64: (LVItem64 : TLvFindInfo64);
  end;

procedure TIShellObj.SetPosition(pos: TPoint);
type TFindRec = record
       findItem : TLvFindInfoBuffer;
       arrCh    : array [0..MAX_PATH] of AnsiChar;
     end;
var p1  : ^TFindRec;
    buf : TFindRec;
    i1  : integer;
    lv  : cardinal;
    pid : cardinal;
    ph  : cardinal;
    c1  : NativeUInt;
begin
  if CheckValid then
    if FDesktopItem then begin
      lv := GetDesktopListView;
      if lv <> 0 then begin
        GetWindowThreadProcessID(lv, @pid);
        ph := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_READ or PROCESS_VM_WRITE, false, pid);
        if ph <> 0 then
          try
            p1 := AllocMemEx(ph, sizeOf(buf));
            if p1 <> nil then
              try   
                ZeroMemory(@buf, sizeOf(buf));
                StrPLCopy(buf.arrCh, GetDescription, MAX_PATH);
                buf.findItem.flags := LVFI_STRING;
                if Is64bitProcess(ph) then
                  buf.findItem.LVItem64.psz := int64(@p1^.arrCh)
                else
                  buf.findItem.LVItem32.psz := dword(@p1^.arrCh);
                if WriteProcessMemory(ph, p1, @buf, sizeOf(buf), c1) and (c1 = sizeOf(buf)) then begin
                  i1 := SendMessageA(lv, LVM_FINDITEMA, -1, NativeInt(@p1^.findItem));
                  if i1 <> -1 then begin
                    if SendMessageA(lv, LVM_SETITEMPOSITION, i1, pos.x or (pos.y shl 16)) = 0 then
                      SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
                  end else
                    self.SetLastError(ERROR_FILE_NOT_FOUND);
                end;
              finally FreeMemEx(ph, p1) end;
          finally CloseHandle(ph) end;
      end;
    end else SetLastError(CErrorNo_NoDesktopItem, UnicodeString(AnsiString(CErrorStr_NoDesktopItem)));
end;

function TIShellObj.GetState : cardinal;
begin
  result := FState;
end;

var PrivateExtractIconsW : function (file_: pwidechar; index, cx, cy : integer; var icon: HICON; var iconId: dword; num, flags: dword) : dword; stdcall = nil;
    ExtractIconExW       : function (file_: pwidechar; index: integer; var iconLarge, iconSmall: HICON; num: dword) : dword; stdcall = nil;

function TIShellObj.GetIcon(size: cardinal; flags: TIconFlags; askAgain: TPBoolean) : cardinal;
const GIL_FORSHORTCUT = $80;
      GIL_DEFAULTICON = $40;
var eiA    : IExtractIconA;
    eiW    : IExtractIconW;
    c1, c2 : dword;
    i1, i2 : integer;
    arrChA : array [0..MAX_PATH] of AnsiChar;
    arrChW : array [0..MAX_PATH] of WideChar;
    pidl1  : PItemIDList;
begin
  result := 0;
  if askAgain <> nil then
    askAgain^ := false;
  if CheckValid and NeedParentShellFolder and NeedSplittedIDLists then begin
    pidl1 := FChildIDList.pidl;
    if OS.winNt then
         i1 := FParentShellFolder.GetUIObjectOf(dword(-1), 1, pidl1, IExtractIconW, nil, pointer(eiW))
    else i1 := FParentShellFolder.GetUIObjectOf(dword(-1), 1, pidl1, IExtractIconA, nil, pointer(eiA));
    if i1 = 0 then begin
      if askAgain <> nil then
           c1 := GIL_ASYNC
      else c1 := 0;
      if ifForShell    in flags then c1 := c1 or GIL_FORSHELL;
      if ifForShortcut in flags then c1 := c1 or GIL_FORSHORTCUT;
      if ifOpenState   in flags then c1 := c1 or GIL_OPENICON;
      if OS.winNt then
           i2 := integer(eiW.GetIconLocation(c1, arrChW, MAX_PATH, i1, c1))
      else i2 := integer(eiA.GetIconLocation(c1, arrChA, MAX_PATH, i1, c1));
      if (i2 = integer(E_PENDING)) and (askAgain <> nil) then begin
        if OS.winNt then
             i2 := integer(eiW.GetIconLocation(c1 or GIL_DEFAULTICON, arrChW, MAX_PATH, i1, c1))
        else i2 := integer(eiA.GetIconLocation(c1 or GIL_DEFAULTICON, arrChA, MAX_PATH, i1, c1));
        if i2 = 0 then
          askAgain^ := true;
      end;
      if i2 = 0 then begin
        if size < 32 then begin
          if OS.winNt then
               i2 := integer(eiW.Extract(arrChW, i1, HICON(c2), HICON(c1), size shl 16 + size))
          else i2 := integer(eiA.Extract(arrChA, i1, HICON(c2), HICON(c1), size shl 16 + size));
        end else
          if OS.winNt then
               i2 := integer(eiW.Extract(arrChW, i1, HICON(c1), HICON(c2), size shl 16 + size))
          else i2 := integer(eiA.Extract(arrChA, i1, HICON(c1), HICON(c2), size shl 16 + size));
        if i2 = 0 then
          if c1 = 0 then
            c1 := c2
          else
            DestroyIcon(c2);
        if i2 = S_FALSE then begin
          if OS.winNt then
            ExtractIconExW := GetProcAddress(GetModuleHandle(user32), 'ExtractIconExW');
          if OS.winNt and (@PrivateExtractIconsW = nil) then
            PrivateExtractIconsW := GetProcAddress(GetModuleHandle(user32), 'PrivateExtractIconsW');
          if @PrivateExtractIconsW <> nil then
            i2 := ord(PrivateExtractIconsW(arrChW, i1, size, size, HICON(c1), c2, 1, 0) <> 1)
          else
            if size < 32 then begin
              if OS.winNt then
                   i2 := ord(ExtractIconExW(arrChW, i1, HICON(nil^), HICON(c1), 1) <> 1)
              else i2 := ord(ExtractIconExA(arrChA, i1, HICON(nil^), HICON(c1), 1) <> 1);
            end else
              if OS.winNt then
                   i2 := ord(ExtractIconExW(arrChW, i1, HICON(c1), HICON(nil^), 1) <> 1)
              else i2 := ord(ExtractIconExA(arrChA, i1, HICON(c1), HICON(nil^), 1) <> 1);
        end;
        if i2 = 0 then
          result := c1;
      end;
    end;
  end;
end;

function TIShellObj.GetItemCount : integer;
begin
  if (not FRefreshReady) and CheckValid then RefreshItems;
  result := FCount;
end;

function TIShellObj.GetItem(index: integer) : IShellObj;
begin
  if (not FRefreshReady) and CheckValid then RefreshItems;
  if (index < 0) or (index >= FCount) then
       result := TIShellObj.Create(false, CErrorNo_IndexOutOfRange, UnicodeString(AnsiString(CErrorStr_IndexOutOfRange)), nil, nil, nil)
  else result := IShellObj(FItems[index]);
end;

type
  TLVItem32 = packed record
    mask       : dword;    // 00
    iItem      : integer;  // 04
    iSubItem   : integer;  // 08
    state      : dword;    // 0C
    stateMask  : dword;    // 10
    pszText    : dword;    // 14
    cchTextMax : integer;  // 18
    iImage     : integer;  // 1C
    lParam     : dword;    // 20
    iIndent    : integer;  // 24
    iGroupId   : integer;  // 28
    cColumns   : dword;    // 2C
    puColumns  : dword;    // 30
    piColFmt   : dword;    // 34
    iGroup     : integer;  // 38
  end;                     //(3C)
  TLVItem64 = packed record
    mask       : dword;    // 00
    iItem      : integer;  // 04
    iSubItem   : integer;  // 08
    state      : dword;    // 0C
    stateMask  : dword;    // 10
    alignment1 : dword;    // 14
    pszText    : int64;    // 18
    cchTextMax : integer;  // 20
    iImage     : integer;  // 24
    lParam     : int64;    // 28
    iIndent    : integer;  // 30
    iGroupId   : integer;  // 34
    cColumns   : dword;    // 38
    alignment2 : dword;    // 3C
    puColumns  : int64;    // 40
    piColFmt   : int64;    // 48
    iGroup     : integer;  // 50
    alignment3 : dword;    // 54
  end;                     //(58)
  TLVItemBuffer = packed record
    case Integer of
      0: (
        // Shared members
        mask      : dword;    // 00
        iItem     : integer;  // 04
        iSubItem  : integer;  // 08
        state     : dword;    // 0C
        stateMask : dword;    // 10
      );
      32: (LVItem32 : TLVItem32);
      64: (LVItem64 : TLVItem64);
  end;

function TIShellObj.RefreshItems : boolean;
var desktopItems  : array of record
                      caption : AnsiString;
                      pos     : TPoint;
                      state   : cardinal;
                    end;

  procedure FindDesktopItem(caption: AnsiString; var pos: TPoint; var state: cardinal);
  var i1 : integer;
  begin
    for i1 := 0 to high(desktopItems) do
      if caption = desktopItems[i1].caption then begin
        pos   := desktopItems[i1].pos;
        state := desktopItems[i1].state;
        exit;
      end;
    pos.x := -1;
    pos.y := -1;
    state := 0;
  end;

  function RefreshItem(pidl: PItemIDList) : boolean;
  var i1    : integer;
      pos   : TPoint;
      state : cardinal;
      iso1  : TIShellObj;
  begin
    result := true;
    pos.x := -1;
    pos.y := -1;
    state := 0;
    for i1 := 0 to FCount - 1 do begin
      iso1 := TIShellObj(FItems[i1].SelfAsTObject);
      if (iso1.FIDList.PIdl^.mkid.cb = pidl^.mkid.cb) and
         CompareMem(@iso1.FIDList.PIdl^.mkid.abID, @pidl^.mkid.abID, iso1.FIDList.PIdl^.mkid.cb) then begin
        if desktopItems <> nil then begin
          iso1.FDesktopItem := true;
          FindDesktopItem(iso1.GetDescription, pos, state);
        end;
        if (not CompareMem(@pos, @iso1.FPos, sizeOf(TPoint))) or (state <> iso1.FState) then begin
          Change(FItems[i1], true, lctChanged, i1, i1);
          iso1.FPos   := pos;
          iso1.FState := state;
          FItemInfos[i1].LastChangeType := lctChanged;
          Change(FItems[i1], false, lctChanged, i1, SortItem(i1));
        end else
          FItemInfos[i1].LastChangeType := lctUnChanged;
        exit;
      end;
    end;
    result := false;
    iso1 := TIShellObj.Create(true, 0, '', FShellFolder, FIDList, IDList(pidl));
    if desktopItems <> nil then begin
      iso1.FDesktopItem := true;
      FindDesktopItem(iso1.GetDescription, iso1.FPos, iso1.FState);
    end;
    if (not iso1.FValid) or (AddItem(IShellObj(iso1)) = -1) then
      iso1.Destroy;
  end;

const SHCONTF_STORAGE = $800;
type TDesktopItemBuf = record
       caption : array [0..MAX_PATH] of AnsiChar;
       pos     : TPoint;
       lvItem  : TLVItemBuffer;
     end;
var el    : IEnumIDList;
    pidl1 : PItemIDList;
    c1    : cardinal;
    size  : NativeUInt;
    p1    : ^TDesktopItemBuf;
    buf   : TDesktopItemBuf;
    i1    : integer;
    lv    : cardinal;
    pid   : cardinal;
    ph    : cardinal;
    is64b : boolean;
begin
  result := false;
  if CheckValid then begin
    FRefreshReady := true;
    if NeedShellFolder then begin
      pidl1 := nil;
      c1 := SFGAO_VALIDATE;
      FShellFolder.GetAttributesOf(0, pidl1, c1);  // delete cache
      c1 := cardinal(FShellFolder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_STORAGE, el));
      if c1 = 0 then begin
        BeginRefresh;
        try
          desktopItems := nil;
          if FIDList.Size = 2 then begin
            lv := GetDesktopListView;
            if lv <> 0 then begin
              GetWindowThreadProcessID(lv, @pid);
              ph := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_READ or PROCESS_VM_WRITE, false, pid);
              if ph <> 0 then
                try
                  p1 := AllocMemEx(ph, sizeOf(buf));
                  if p1 <> nil then
                    try
                      is64b := Is64bitProcess(ph);
                      ZeroMemory(@buf, sizeOf(buf));
                      SetLength(desktopItems, SendMessageA(lv, LVM_GETITEMCOUNT, 0, 0));
                      for i1 := 0 to high(desktopItems) do begin
                        with buf.lvItem do begin
                          mask := LVIF_TEXT or LVIF_STATE;
                          iItem := i1;
                          iSubItem := 0;
                          if is64b then begin
                            with buf.lvItem.LVItem64 do begin
                              pszText := int64(@p1^.caption);
                              cchTextMax := MAX_PATH;
                            end;
                          end else 
                            with buf.lvItem.LVItem32 do begin
                              pszText := dword(@p1^.caption);
                              cchTextMax := MAX_PATH;
                            end;
                        end;
                        if WriteProcessMemory(ph, p1, @buf, sizeOf(buf), size) and (size = sizeOf(buf)) and
                           (SendMessageA(lv, LVM_GETITEMPOSITION, i1, NativeInt(@p1^.pos)) <> 0) and
                           (SendMessageA(lv, LVM_GETITEM, 0, NativeInt(@p1^.lvItem)) <> 0) and
                           ReadProcessMemory(ph, p1, @buf, sizeOf(buf), size) and (size = sizeOf(buf)) then begin
                          desktopItems[i1].caption := buf.caption;
                          desktopItems[i1].pos     := buf.pos;
                          desktopItems[i1].state   := buf.lvItem.state;
                        end;
                      end;
                    finally FreeMemEx(ph, p1) end;
                finally CloseHandle(ph) end;
            end;
          end;
          el.Reset;
          while el.Next(1, pidl1, c1) = NOERROR do
            if RefreshItem(pidl1) then
              FMalloc.Free(pidl1);
          result := true;
        finally EndRefresh end;
      end else SetLastError(c1);
    end;
  end;
end;

function TIShellObj.Item(name: AnsiString) : IShellObj;
var i1 : integer;
begin
  if CheckValid then begin
    if not FRefreshReady then RefreshItems;
    if posStrIs1(AnsiString('::{'), name) then begin
      if (length(name) >= 40) and (name[40] = '}') then
        try
          result := Item(StringToGuid(string(Copy(name, 3, 38))));
          exit;
        except end;
    end else begin
      if posStrIs1(AnsiString('{'), name) and (length(name) = 38) and (name[38] = '}') then
        try
          result := Item(StringToGuid(string(name)));
          if result.IsValid then exit;
        except end;
      for i1 := 0 to FCount - 1 do
        if (IsTextEqual(IShellObj(FItems[i1]).Name,          name) or
            IsTextEqual(IShellObj(FItems[i1]).Description,   name) or
            IsTextEqual(IShellObj(FItems[i1]).DescrInFolder, name)) then begin
          result := IShellObj(FItems[i1]);
          exit;
        end;
    end;
    result := TIShellObj.Create(false, ERROR_FILE_NOT_FOUND, '', nil, nil, nil);
  end else
    result := TIShellObj.Create(false, FLastErrorNo, FLastErrorStr, nil, nil, nil);
end;

function TIShellObj.Item(const name: TGuid) : IShellObj;
var i1  : integer;
    so1 : TIShellObj;
begin
  if CheckValid then begin
    if not FRefreshReady then RefreshItems;
    for i1 := 0 to FCount - 1 do begin
      so1 := TIShellObj(FItems[i1].SelfAsTObject);
      if (so1.FGuidReady or so1.NeedGuid) and
         CompareMem(@so1.FGuid, @name, sizeOf(TGuid)) then begin
        result := IShellObj(FItems[i1]);
        exit;
      end;
    end;
    result := TIShellObj.Create(false, ERROR_FILE_NOT_FOUND, '', nil, nil, nil);
  end else
    result := TIShellObj.Create(false, FLastErrorNo, FLastErrorStr, nil, nil, nil);
end;

function TIShellObj.Item(const name: IIDList) : IShellObj;
var i1 : integer;
begin
  if CheckValid then begin
    if not FRefreshReady then RefreshItems;
    for i1 := 0 to FCount - 1 do
      if IShellObj(FItems[i1]).IDList.IsEqual(name) then begin
        result := IShellObj(FItems[i1]);
        exit;
      end;
    result := TIShellObj.Create(false, ERROR_FILE_NOT_FOUND, '', nil, nil, nil);
  end else
    result := TIShellObj.Create(false, FLastErrorNo, FLastErrorStr, nil, nil, nil);
end;

function TIShellObj.GetParent : IShellObj;
var so1 : TIShellObj;
begin
  if NeedSplittedIDLists then begin
    so1 := TIShellObj.Create(FValid, 0, '', nil, FParentIDList, nil);
    result := so1;
    so1.AddItem(IShellObj(self));
  end else
    result := TIShellObj.Create(false, FLastErrorNo, GetLastErrorStr, nil, nil, nil);
end;

function TIShellObj.NeedShellFolder : boolean;
var c1 : cardinal;
begin
  if not FShellFolderReady then begin
    FShellFolderReady := true;
    CoInitialize(nil);
    if FIDList.pidl^.mkid.cb > 0 then begin
      if FHasParentShellFolder and (FChildIDList <> nil) and FChildIDList.IsValid then
           c1 := cardinal(FParentShellFolder.BindToObject(FChildIDList.pidl, nil, IShellFolder, pointer(FShellFolder)))
      else c1 := cardinal(FDesktop          .BindToObject(     FIDList.pidl, nil, IShellFolder, pointer(FShellFolder)));
      if c1 <> 0 then
        SetLastError(c1);
    end else FShellFolder := FDesktop;
  end;
  result := FShellFolder <> nil;
end;

function TIShellObj.NeedStr(shgdn: cardinal; out str: AnsiString) : boolean;
var sr         : TStrRet;
    sf1,sf2    : IShellFolder;
    pf         : IPersistFolder;
    guid       : TGuid;
    idl1, idl2 : IIDList;
    s1         : AnsiString;
    c1         : cardinal;
begin
  result := false;
  str := '';
  if shgdn = SHGDN_FORPARSING then begin
    SetLength(s1, MAX_PATH + 1);
    if SHGetPathFromIDListA(FIDList.pidl, PAnsiChar(s1)) then begin
      str := PAnsiChar(s1);
      result := true;
    end else begin
      NeedShellFolder;
      NeedSplittedIDLists;
      NeedParentShellFolder;
      idl1 := FParentIDList.Clone;
      idl2 := FChildIDList.Clone;
      sf1  := FShellFolder;
      sf2  := FParentShellFolder;
      repeat
        if (sf1 <> nil) and (sf1.QueryInterface(IPersistFolder, pf) = 0) and
           (pf.GetClassID(guid) = 0) then begin
          str := '::' + AnsiString(GuidToString(guid)) + '\' + str;
        end else if (sf2 <> nil) and (sf2.GetDisplayNameOf(idl2.pidl, SHGDN_FORPARSING, sr) = 0) then begin
          case sr.uType of
            STRRET_CSTR   : str := AnsiString(sr.cStr)                           + '\' + str;
            STRRET_OFFSET : str := PAnsiChar(NativeUInt(idl2.pidl) + sr.uOffset) + '\' + str;
            STRRET_WSTR   : str := AnsiString(WideString(sr.pOleStr))            + '\' + str;
            else            str := '?'                                           + '\' + str;
          end;
        end else break;
        idl2 := idl1.SplitName;
        sf1  := sf2;
        sf2  := nil;
        if idl1.IsValid and (FDesktop.BindToObject(idl1.pidl, nil, IShellFolder, pointer(sf2)) <> 0) then
          sf2 := nil;
      until idl2.Size <= 2;
      if str <> '' then Delete(str, length(str), 1);
      result := str <> '';
      if not result then SetLastError(ERROR_FILE_NOT_FOUND);
    end;
  end else if NeedParentShellFolder and NeedSplittedIDLists then begin
    sr.uType := STRRET_CSTR;
    c1 := cardinal(FParentShellFolder.GetDisplayNameOf(FChildIDList.pidl, shgdn, sr));
    result := c1 and $80000000 = 0;
    if result then begin
      case sr.uType of
        STRRET_CSTR   : str := AnsiString(sr.cStr);
        STRRET_OFFSET : str := PAnsiChar(NativeUInt(FChildIDList.pidl) + sr.uOffset);
        STRRET_WSTR   : str := AnsiString(WideString(sr.pOleStr));
        else            str := '?';
      end;
    end else SetLastError(c1 and $FFFF);
  end;
end;

function TIShellObj.NeedSplittedIDLists : boolean;
begin
  if not FSplittedIDListsReady then begin
    FSplittedIDListsReady := true;
    FParentIDList := FIDList.GetPath;
    FChildIDList  := FIDList.GetName;
    FHasSplittedIDLists := FParentIDList.IsValid;
    if not FHasSplittedIDLists then
      SetLastError(CErrorNo_Unknown, UnicodeString(AnsiString(CErrorStr_Unknown)));
  end;
  result := FHasSplittedIDLists;
end;

function TIShellObj.NeedParentShellFolder : boolean;
var c1 : cardinal;
begin
  if not FParentShellFolderReady then begin
    FParentShellFolderReady := true;
    if NeedSplittedIDLists then begin
      if FParentIDList.pidl^.mkid.cb = 0 then begin
        FHasParentShellFolder := true;
        FParentShellFolder    := FDesktop;
      end else begin
        CoInitialize(nil);
        c1 := cardinal(FDesktop.BindToObject(FParentIDList.pidl, nil, IShellFolder, pointer(FParentShellFolder)));
        FHasParentShellFolder := c1 and $80000000 = 0;
        if not FHasParentShellFolder then
          SetLastError(c1 and $FFFF);
      end;
    end;
  end;
  result := FHasParentShellFolder;
end;

function TIShellObj.NeedGuid : boolean;
var pf : IPersistFolder;
    c1 : cardinal;
begin
  FGuidReady := true;
  if NeedShellFolder then begin
    c1 := cardinal(FShellFolder.QueryInterface(IPersistFolder, pf));
    if c1 = 0 then
      c1 := cardinal(pf.GetClassID(FGuid));
    result := c1 and $80000000 = 0;
    if not result then
      SetLastError(c1 and $FFFF);
  end else result := false;
  if not result then
    ZeroMemory(@FGuid,sizeOf(TGuid));
end;

procedure TIShellObj.NeedName;
var i1 : integer;
begin
  FNameReady := true;
  if NeedStr(SHGDN_FORPARSING, FPath) then begin
    FName := FPath;
    i1 := PosStr(AnsiString('\'), FName, maxInt, 1);
    if (i1 > 0) and (i1 < Length(FName)) then Delete(FName, 1, i1);
  end;
end;

function TIShellObj.GetMaxInterface : IBasic;
begin
  result := IShellObj(self);
end;

function PathToIDList(path: UnicodeString) : IIDList;
var sf1, sf2 : IShellFolder;
    pidl2    : PItemIDList;
    idl1     : IIDList;
    c1, c2   : cardinal;
    so1      : IShellObj;
    i1, i2   : integer;
    ws1      : UnicodeString;
    b1, b2   : boolean;
begin
  if not FDesktopReady then begin
    FDesktopReady := true;
    c1 := cardinal(SHGetDesktopFolder(FDesktop));
  end else c1 := 0;
  if (c1 = 0) and (FDesktop <> nil) then begin
    if (path <> '') and (path[length(path)] = '\') and (length(path) > length(ExtractFileDir(path))) then
      Delete(path, length(path), 1);
    if path <> '' then begin
      idl1 := nil;
      c1 := 0;
      c1 := cardinal(FDesktop.ParseDisplayName(0, nil, pwideChar(path), c1, pidl2, TPCardinal(nil)^));
      if c1 <> 0 then begin
        CoInitialize(nil);
        b1 := true;
        b2 := false;
        sf1 := FDesktop;
        repeat
          i1 := Pos('\', path);
          i2 := i1;
          if i1 = 0 then begin
            i1 := maxInt;
            i2 := maxInt;
          end else
            if (i1 = 3) and (path[2] = ':') then
              inc(i1);
          ws1 := Copy(path, 1, i1 - 1);
          Delete(path, 1, i2);
          b1 := b1 and (sf1 <> nil);
          if b1 then begin
            c2 := 0;
            b1 := sf1.ParseDisplayName(0, nil, PWideChar(ws1), c1, pidl2, c2) = 0;
            if b1 then begin
              if sf1.BindToObject(pidl2, nil, IShellFolder, pointer(sf2)) = 0 then
                   sf1 := sf2
              else sf1 := nil;
              if idl1 = nil then idl1 := TIIDList.Create(true, 0, '', pidl2)
              else               idl1.Append(madShell.IDList(pidl2));
            end;
          end;
          if not b1 then begin
            if so1 = nil then
              if b2 then so1 := ShellObj(idl1)
              else       so1 := ShellObj(sfDesktopObj);
            so1 := so1.Item(AnsiString(ws1));
            if not so1.IsValid then begin
              result := TIIDList.Create(false, ERROR_FILE_NOT_FOUND, '', nil);
              exit;
            end;
          end else b2 := true;
        until path = '';
        if so1 <> nil then result := so1.IDList
        else               result := idl1;
      end else result := TIIDList.Create(true, 0, '', pidl2);
    end else result := TIIDList.Create(false, ERROR_FILE_NOT_FOUND, '', nil);
  end else result := TIIDList.Create(false, c1, '', nil);
end;

function ShellObj(path: AnsiString) : IShellObj;
begin
  result := TIShellObj.Create(true, 0, '', nil, pathToIDList(UnicodeString(path)), nil);
end;

function ShellObj(sf: TSpecialFolder) : IShellObj;

  function RegRdStr(key: HKEY; path, name: AnsiString; var str: AnsiString; var error: cardinal) : boolean;
  var hk     : HKEY;
      c1, c2 : dword;
      s1     : AnsiString;
  begin
    result := false;
    error := cardinal(RegOpenKeyExA(key, PAnsiChar(path), 0, KEY_QUERY_VALUE, hk));
    if error = 0 then
      try
        SetLength(str, MAX_PATH + 1);
        c1 := MAX_PATH;
        error := cardinal(RegQueryValueExA(hk, PAnsiChar(name), nil, @c2, pointer(str), @c1));
        if error = 0 then begin
          str := PAnsiChar(str);
          if c2 = REG_EXPAND_SZ then begin
            SetLength(s1, MAX_PATH + 1);
            SetLength(str, c1 - 1);
            if ExpandEnvironmentStringsA(PAnsiChar(str), PAnsiChar(s1), MAX_PATH) > 0 then
              str := PAnsiChar(s1);
          end;
          result := true;
        end;
      finally RegCloseKey(hk) end;
  end;

const
//CSIDL_DESKTOP                       = $0000;  // desktop object
//CSIDL_INTERNET                      = $0001;  // internet explorer (desktop icon)
//CSIDL_PROGRAMS                      = $0002;  // <user> \ start menu \ programs
//CSIDL_CONTROLS                      = $0003;  // control panel
//CSIDL_PRINTERS                      = $0004;  // printers
//CSIDL_PERSONAL                      = $0005;  // <user> \ my documents
//CSIDL_FAVORITES                     = $0006;  // <user> \ favorites
//CSIDL_STARTUP                       = $0007;  // <user> \ start menu \ programs \ startup
//CSIDL_RECENT                        = $0008;  // <user> \ recent
//CSIDL_SENDTO                        = $0009;  // <user> \ sendto
//CSIDL_BITBUCKET                     = $000a;  // recycle bin
//CSIDL_STARTMENU                     = $000b;  // <user> \ start menu
  CSIDL_MYDOCUMENTS                   = $000c;  // my documents (desktop icon)
  CSIDL_MYMUSIC                       = $000d;  // <user> \ my music
  CSIDL_MYVIDEO                       = $000e;  // <user> \ my video
//CSIDL_DESKTOPDIRECTORY              = $0010;  // desktop directory
//CSIDL_DRIVES                        = $0011;  // my computer
//CSIDL_NETWORK                       = $0012;  // My Network Places object
//CSIDL_NETHOOD                       = $0013;  // <user> \ nethood
//CSIDL_FONTS                         = $0014;  // fonts
//CSIDL_TEMPLATES                     = $0015;  // templates
//CSIDL_COMMON_STARTMENU              = $0016;  // all users \ start menu
//CSIDL_COMMON_PROGRAMS               = $0017;  // all users \ start menu \ programs
//CSIDL_COMMON_STARTUP                = $0018;  // all users \ start menu \ programs \ startup
//CSIDL_COMMON_DESKTOPDIRECTORY       = $0019;  // all users \ desktop
//CSIDL_APPDATA                       = $001a;  // <user> \ application data
//CSIDL_PRINTHOOD                     = $001b;  // <user> \ printhood
  CSIDL_LOCAL_APPDATA                 = $001c;  // <user> \ local settings \ application data
//CSIDL_ALTSTARTUP                    = $001d;  // <user> \ non localized startup
//CSIDL_COMMON_ALTSTARTUP             = $001e;  // all users \ non localized startup
//CSIDL_COMMON_FAVORITES              = $001f;  // all users \ favorites
//CSIDL_INTERNET_CACHE                = $0020;  // ie cache folder
//CSIDL_COOKIES                       = $0021;  // ie cookie folder
//CSIDL_HISTORY                       = $0022;  // ie history folder
  CSIDL_COMMON_APPDATA                = $0023;  // all users \ application data
  CSIDL_WINDOWS                       = $0024;  // windows folder
  CSIDL_SYSTEM                        = $0025;  // system(32) folder
  CSIDL_PROGRAM_FILES                 = $0026;  // program files
  CSIDL_MYPICTURES                    = $0027;  // <user> \ my pictures
  CSIDL_PROFILE                       = $0028;  // <user> \ .
  CSIDL_SYSTEMX86                     = $0029;  // RISC: x86 system directory
  CSIDL_PROGRAM_FILESX86              = $002a;  // RISC: x86 program files directory
  CSIDL_PROGRAM_FILES_COMMON          = $002b;  // program files \ common
  CSIDL_PROGRAM_FILES_COMMONX86       = $002c;  // RISC: x86 program files \ common folder
  CSIDL_COMMON_TEMPLATES              = $002d;  // all users \ templates
  CSIDL_COMMON_DOCUMENTS              = $002e;  // all users \ documents
  CSIDL_COMMON_ADMINTOOLS             = $002f;  // all users \ administrative tools
  CSIDL_ADMINTOOLS                    = $0030;  // <user> \ administrative tools
  CSIDL_DIALUPNETWORK                 = $0031;  // network and dialup connections
  CSIDL_COMMON_MUSIC                  = $0035;  // all users \ my music
  CSIDL_COMMON_PICTURES               = $0036;  // all users \ my pictures
  CSIDL_COMMON_VIDEO                  = $0037;  // all users \ my video
  CSIDL_RESOURCES                     = $0038;  // resource directory
  CSIDL_RESOURCES_LOCALIZED           = $0039;  // localized resource directory
  CSIDL_COMMON_OEM_LINKS              = $003a;  // links to all users \ OEM specific apps
  CSIDL_CDBURN_AREA                   = $003b;  // <user> \ local settings \ application data \ microsoft \ cd burning
  CSIDL_COMPUTERSNEARME               = $003d;  // computers near me (workgroup)

  CSIDL_FLAG_PER_USER_INIT            = $0800;  // indicate per-user init (e.g. upgrade)
  CSIDL_FLAG_NO_ALIAS                 = $1000;  // don't return alias versions of the pidl
  CSIDL_FLAG_DONT_VERIFY              = $4000;  // return folder path without verifying
  CSIDL_FLAG_CREATE                   = $8000;  // force folder creation

  CSIDLs : array [TSpecialFolder] of integer = (
    CSIDL_DESKTOP, CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_BITBUCKET,
      CSIDL_CONTROLS, CSIDL_PRINTERS, CSIDL_DIALUPNETWORK,
      CSIDL_MYDOCUMENTS, CSIDL_INTERNET, CSIDL_COMPUTERSNEARME,
    CSIDL_WINDOWS, CSIDL_SYSTEM, CSIDL_SYSTEMX86, CSIDL_FONTS,
      CSIDL_RESOURCES, CSIDL_RESOURCES_LOCALIZED,
      CSIDL_PROGRAM_FILES, CSIDL_PROGRAM_FILESX86,
      CSIDL_PROGRAM_FILES_COMMON, CSIDL_PROGRAM_FILES_COMMONX86, -1, -1,
    CSIDL_PROFILE, CSIDL_DESKTOPDIRECTORY,
      CSIDL_STARTMENU, CSIDL_PROGRAMS, CSIDL_STARTUP, CSIDL_ALTSTARTUP,
      CSIDL_PERSONAL, CSIDL_MYMUSIC, CSIDL_MYPICTURES, CSIDL_MYVIDEO,
      CSIDL_TEMPLATES, CSIDL_FAVORITES,
      CSIDL_INTERNET_CACHE, CSIDL_COOKIES, CSIDL_HISTORY,
      CSIDL_RECENT, CSIDL_SENDTO,
      CSIDL_APPDATA, -1, CSIDL_LOCAL_APPDATA, CSIDL_CDBURN_AREA,
      CSIDL_PRINTHOOD, CSIDL_NETHOOD, CSIDL_ADMINTOOLS,
    -1, CSIDL_COMMON_DESKTOPDIRECTORY,
      CSIDL_COMMON_STARTMENU, CSIDL_COMMON_PROGRAMS, CSIDL_COMMON_STARTUP, CSIDL_COMMON_ALTSTARTUP,
      CSIDL_COMMON_DOCUMENTS, CSIDL_COMMON_MUSIC, CSIDL_COMMON_PICTURES, CSIDL_COMMON_VIDEO,
      CSIDL_COMMON_TEMPLATES, CSIDL_COMMON_FAVORITES,
      CSIDL_COMMON_APPDATA,
      CSIDL_COMMON_ADMINTOOLS,
      CSIDL_COMMON_OEM_LINKS
    );
  CRegNames : array [TSpecialFolder] of PAnsiChar = (
    '', '', '', '', '', '', '', '', '', '',
      '', '', '', 'Fonts', '', '', '', '', '', '', '', '',
    '', 'Desktop', 'Start Menu', 'Programs', 'Startup', 'AltStartup',
      'Personal', 'My Music', 'My Pictures', 'My Video', 'Templates',
      'Favorites', 'Cache', 'Cookies', 'History', 'Recent', 'SendTo',
      'AppData', 'Local Settings', 'Local AppData', 'CD Burning',
      'PrintHood', 'NetHood', 'Administrative Tools',
    '', 'Common Desktop', 'Common Start Menu', 'Common Programs',
      'Common Startup', 'Common AltStartup',
      'Common Documents', 'CommonMusic', 'CommonPictures', 'CommonVideo',
      'Common Templates', 'Common Favorites', 'Common AppData',
      'Common Administrative Tools', 'Common OEM Links');
  CRegCurrentVersion   : PAnsiChar = 'Software\Microsoft\Windows\CurrentVersion';
  CRegUserShellFolders : PAnsiChar = '\Explorer\User Shell Folders';
  CRegShellFolders     : PAnsiChar = '\Explorer\Shell Folders';
var pidl1 : PItemIDList;
    s1    : AnsiString;
    c1    : cardinal;
    idl1  : IIDList;
begin
  idl1 := nil;
  if CSIDLs[sf] >= 0 then begin
    c1 := cardinal(SHGetSpecialFolderLocation(INVALID_HANDLE_VALUE, CSIDLs[sf], pidl1));
    if c1 = 0 then
      idl1 := IDList(pidl1);
  end;
  if idl1 = nil then
    if (CRegNames[sf] <> '') and
       ( RegRdStr(HKEY_CURRENT_USER,  AnsiString(CRegCurrentVersion) + AnsiString(CRegUserShellFolders), CRegNames[sf], s1, c1) or
         RegRdStr(HKEY_CURRENT_USER,  AnsiString(CRegCurrentVersion) + AnsiString(CRegShellFolders    ), CRegNames[sf], s1, c1) or
         RegRdStr(HKEY_LOCAL_MACHINE, AnsiString(CRegCurrentVersion) + AnsiString(CRegUserShellFolders), CRegNames[sf], s1, c1) or
         RegRdStr(HKEY_LOCAL_MACHINE, AnsiString(CRegCurrentVersion) + AnsiString(CRegShellFolders    ), CRegNames[sf], s1, c1)    ) then
      idl1 := pathToIDList(UnicodeString(s1))
    else
      case sf of
        sfDialupNetwork :
          if OS.win9x then
               idl1 := pathToIDList('::{20D04FE0-3AEA-1069-A2D8-08002B30309D}\::{992CFFA0-F557-101A-88EC-00DD010CCC48}')
          else idl1 := pathToIDList('::{20D04FE0-3AEA-1069-A2D8-08002B30309D}\::{A4D92740-67CD-11CF-96F2-00AA00A11DD9}');
        sfWindows :
          idl1 := pathToIDList(UnicodeString(winFolder));
        sfSystem :
          idl1 := pathToIDList(UnicodeString(sysFolder));
        sfTemp :
          begin
            SetLength(s1, MAX_PATH + 1);
            if GetTempPathA(MAX_PATH, PAnsiChar(s1)) <> 0 then
                 idl1 := pathToIDList(UnicodeString(AnsiString(PAnsiChar(s1))))
            else c1 := GetLastError;
          end;
        sfProgramFiles :
          if RegRdStr(HKEY_LOCAL_MACHINE, CRegCurrentVersion, 'ProgramFilesDir',  s1, c1) or
             RegRdStr(HKEY_LOCAL_MACHINE, CRegCurrentVersion, 'ProgramFilesPath', s1, c1) then
            idl1 := pathToIDList(UnicodeString(s1));
        sfCommonFiles :
          if RegRdStr(HKEY_LOCAL_MACHINE, CRegCurrentVersion, 'CommonFilesDir',  s1, c1) then
            idl1 := pathToIDList(UnicodeString(s1));
        sfProfiles :
          if RegRdStr(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows NT\CurrentVersion\ProfileList', 'ProfilesDirectory', s1, c1) then
            idl1 := pathToIDList(UnicodeString(s1));
        sfAllUsersProfile :
          begin
            SetLength(s1, MAX_PATH + 1);
            if ExpandEnvironmentStringsA('%ALLUSERSPROFILE%', PAnsiChar(s1), MAX_PATH) > 0 then begin
              s1 := PAnsiChar(s1);
              if (s1 <> '') and (s1[1] <> '%') then
                   idl1 := pathToIDList(UnicodeString(AnsiString(PAnsiChar(s1))))
              else c1 := ERROR_FILE_NOT_FOUND;
            end else
              c1 := GetLastError;
          end;
      end;
  if idl1 <> nil then
       result := TIShellObj.Create(idl1.IsValid, idl1.LastErrorNo, idl1.LastErrorStr, nil, idl1, nil)
  else result := TIShellObj.Create(false, c1, '', nil, nil, nil);
end;

function ShellObj(const idList: IIDList) : IShellObj;
begin
  result := TIShellObj.Create(true, 0, '', nil, idList, nil);
end;

function ShellObj(const sfi: IShellFolder) : IShellObj;
var pidl1 : PItemIDList;
    pf2   : IPersistFolder2;
    so1   : TIShellObj;
    c1    : cardinal;
begin
  c1 := cardinal(sfi.QueryInterface(IPersistFolder2, pf2));
  if c1 = 0 then
    c1 := cardinal(pf2.GetCurFolder(pidl1));
  if c1 = 0 then begin
    so1 := TIShellObj.Create(true, 0, '', nil, IDList(pidl1), nil);
    so1.FShellFolder      := sfi;
    so1.FShellFolderReady := true;
    so1.FHasShellFolder   := true;
    result := so1;
  end else result := TIShellObj.Create(false, c1, '', nil, nil, nil);
end;

function Desktop : IShellObj;
begin
  result := ShellObj(sfDesktopObj);
end;

// ***************************************************************

type
  TWatchDir = record
    buf     : array [1..32 * 1024] of byte;
    dir     : dword;
    event   : dword;
    thread  : dword;
    proc    : TWatchDirCallback;
    procOO  : TWatchDirCallbackOO;
    root    : array [0..MAX_PATH] of wideChar;
    flags   : dword;
    subtree : boolean;
  end;
  TPWatchDir = ^TWatchDir;

procedure WatchDirComplete(error, bytesTransfered: dword; var ovlp: TOverlapped); stdcall;
type TFileNotifyInformation = packed record
       NextEntryOffset : dword;
       Action          : dword;
       FileNameLength  : dword;
       FileName        : wideChar;
     end;
var fni    : ^TFileNotifyInformation;
    ws1    : UnicodeString;
    pwd    : ^TWatchDir;
    store  : UnicodeString;
    c1, c2 : dword;
begin
  pwd := pointer(ovlp.hEvent);
  store := '';
  if bytesTransfered > 0 then begin
    fni := @pwd.buf;
    while true do begin
//      if fni.Action <> FILE_ACTION_MODIFIED then begin
        ws1 := pwd^.root;
        fni.FileNameLength := fni.FileNameLength div 2;
        c1 := Length(ws1);
        SetLength(ws1, c1 + fni.FileNameLength);
        for c2 := c1 + 1 to c1 + fni.FileNameLength do
          ws1[c2] := PWideChar(@fni.FileName)[c2 - c1 - 1];
        case fni.Action of
          FILE_ACTION_ADDED            : if @pwd^.proc <> nil then
                                              pwd.proc  (wtAdded,    '', ws1)
                                         else pwd.procOO(wtAdded,    '', ws1);
          FILE_ACTION_REMOVED          : if @pwd^.proc <> nil then
                                              pwd.proc  (wtRemoved,  ws1, '')
                                         else pwd.procOO(wtRemoved,  ws1, '');
          FILE_ACTION_MODIFIED         : if @pwd^.proc <> nil then
                                              pwd.proc  (wtModified, ws1, ws1)
                                         else pwd.procOO(wtModified, ws1, ws1);
          FILE_ACTION_RENAMED_OLD_NAME : store := ws1;
          FILE_ACTION_RENAMED_NEW_NAME : if @pwd^.proc <> nil then
                                              pwd.proc  (wtRenamed,  store, ws1)
                                         else pwd.procOO(wtRenamed,  store, ws1);
        end;
//      end;
      if fni.NextEntryOffset = 0 then
        break;
      dword(fni) := dword(fni) + fni.NextEntryOffset;
    end;
  end;
end;

function WatchDirThread(params: dword) : integer; stdcall;
var ovlp : TOverlapped;
    c1   : dword;
    pwd  : ^TWatchDir absolute params;
begin
  result := 0;
  ZeroMemory(@ovlp, sizeOf(ovlp));
  ovlp.hEvent := params;
  while ReadDirectoryChangesW(pwd^.dir, @pwd^.buf, 32 * 1024, pwd^.subtree,
                              pwd^.flags, @c1, @ovlp, @WatchDirComplete) and
        (WaitForSingleObjectEx(pwd^.event, INFINITE, true) = WAIT_IO_COMPLETION) do
    ;
end;

function WatchDir(dir: UnicodeString; watchSubtree: boolean; events: TShellEventTypes;
                  callback: TWatchDirCallback; callbackOO: TWatchDirCallbackOO) : dword;
const FILE_LIST_DIRECTORY = $0001;
var pwd : ^TWatchDir absolute result;
    tid : dword;
begin
  if (dir <> '') and (dir[length(dir)] <> '\') then
    dir := dir + '\';
  pwd := VirtualAlloc(nil, sizeOf(pwd^), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if pwd <> nil then begin
    pwd^.dir := CreateFileW(PWideChar(dir), FILE_LIST_DIRECTORY,
                            FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                            nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
    if pwd^.dir <> INVALID_HANDLE_VALUE then begin
      pwd^.flags := 0;
      if [seItemCreated, seItemRenamed, seItemDeleted] * events <> [] then
        pwd^.flags := pwd^.flags or FILE_NOTIFY_CHANGE_FILE_NAME;
      if [seDirCreated, seDirRenamed, seDirDeleted] * events <> [] then
        pwd^.flags := pwd^.flags or FILE_NOTIFY_CHANGE_DIR_NAME;
      if [seItemChanged, seDirChanged] * events <> [] then
        pwd^.flags := pwd^.flags or FILE_NOTIFY_CHANGE_SIZE or
                                    FILE_NOTIFY_CHANGE_LAST_WRITE or
                                    FILE_NOTIFY_CHANGE_CREATION or
                                    FILE_NOTIFY_CHANGE_SECURITY;
      if seAttributesChanged in events then
        pwd^.flags := pwd^.flags + FILE_NOTIFY_CHANGE_ATTRIBUTES;
      lstrcpyW(pwd^.root, PWideChar(dir));
      pwd^.subtree := watchSubtree;
      pwd^.proc    := callback;
      pwd^.procOO  := callbackOO;
      pwd^.event   := CreateEvent(nil, true, false, nil);
      pwd^.thread  := CreateThread(nil, 0, @WatchDirThread, pwd, 0, tid);
    end else begin
      VirtualFree(pwd, 0, MEM_RELEASE);
      pwd := nil;
    end;
  end;
end;

function UnwatchDirThread(pwd: TPWatchDir) : integer; stdcall;
begin
  SetEvent(pwd^.event);
  WaitForSingleObject(pwd^.thread, INFINITE);
  CloseHandle(pwd^.event );
  CloseHandle(pwd^.thread);
  CloseHandle(pwd^.dir   );
  VirtualFree(pwd, 0, MEM_RELEASE);
  result := 0;
end;

function UnwatchDir(var wdh: dword) : boolean;
var pwd : ^TWatchDir absolute wdh;
    tid : dword;
begin
  result := pwd <> nil;
  if result then begin
    CloseHandle(CreateThread(nil, 0, @UnwatchDirThread, pwd, 0, tid));
    pwd := nil;
  end;
end;

// ***************************************************************

type
  TInternalShellEventType = (
    iseItemRenamed, iseItemCreated, iseItemDeleted, iseDirCreated, iseDirDeleted,
    iseMediaInserted, iseMediaRemoved, iseDriveRemoved, iseDriveAdded, iseShareAdded, iseShareRemoved,
    iseAttributesChanged, iseDirChanged, iseItemChanged, iseServerDisconnect, iseImageChanged,
    iseDriveAddedGui, iseDirRenamed, iseFreespaceChanged, iseExtendedEventPreIe4,
    ise00100000, ise00200000, ise00400000, ise00800000, ise01000000, ise02000000,
    iseExtendedEvent, iseAssociationChanged, ise10000000, ise20000000, ise40000000, iseInterrupt);
  TInternalShellEventTypes = set of TInternalShellEventType;

  TShellEventContainer = class
    FEventTypes    : TShellEventTypes;
    FWatchHandle   : dword;
    FWatchMsg      : cardinal;
    FThreadID      : cardinal;
    FEventProc     : TShellEvent;
    FEventProcOO   : TShellEventOO;
    FShMsg         : cardinal;
    FLockProc      : function (memoryMap, processID: cardinal; var pidls: pointer; var eventID: TInternalShellEventTypes) : cardinal; stdcall;
    FUnlockProc    : function (notifyLock: cardinal) : longBool; stdcall;
    FNotifyHandle  : cardinal;
    constructor Create (eventProc: TShellEvent; eventProcOO: TShellEventOO;
                        root: AnsiString; watchSubtree: boolean; eventTypes: TShellEventTypes);
    destructor Destroy; override;
    procedure ShMsgHandlerProc (window: HWND; msg: dword; wParam, lParam: NativeInt; var result: NativeInt);
    procedure WatchMsgHandlerProc (window: HWND; msg: dword; wParam, lParam: NativeInt; var result: NativeInt);
    procedure WatchDirCallback (watchType: TWatchType; old, new_: UnicodeString);
  end;

const
  CShellEventTypeToInternal : array [TShellEventType] of TInternalShellEventType =
    (iseItemCreated, iseItemRenamed, iseItemChanged, iseItemDeleted,           // item
     iseDirCreated,  iseDirRenamed,  iseDirChanged,  iseDirDeleted,            // dir
     iseAttributesChanged,                                                     // item & dir
     iseFreespaceChanged,                                                      // drive
     iseDriveAdded, iseDriveAddedGui, iseDriveRemoved,                         // drive
     iseMediaInserted, iseMediaRemoved,                                        // medium
     iseShareAdded, iseShareRemoved, iseServerDisconnect,                      // network
     iseImageChanged, iseAssociationChanged,                                   // etc.
     iseExtendedEvent, iseExtendedEvent, iseExtendedEvent, iseExtendedEvent);  // extended

var
  ShellEventSection : ICriticalSection;
  ShellEventList    : array of TShellEventContainer;

  SHChangeNotifyRegister   : function  (window, flags: cardinal; ie: TInternalShellEventTypes;
                                        msg, itemCount: cardinal; items: pointer) : cardinal stdcall = nil;
  SHChangeNotifyDeregister : function  (notify: cardinal) : longBool stdcall = nil;
  ILFree                   : procedure (pidl: pointer) stdcall = nil;
  SHSimpleIDListFromPath   : function  (path: pointer) : pointer stdcall = nil;

constructor TShellEventContainer.Create(eventProc: TShellEvent; eventProcOO: TShellEventOO;
                                        root: AnsiString; watchSubtree: boolean; eventTypes: TShellEventTypes);

  function ShellEventTypesToInternal(eventTypes: TShellEventTypes) : TInternalShellEventTypes;
  var e1 : TShellEventType;
  begin
    result := [];
    for e1 := low(TShellEventType) to high(TShellEventType) do
      if e1 in eventTypes then
        Include(result, CShellEventTypeToInternal[e1]);
  end;

const SHCNF_InterruptLevel        = $0001;
      SHCNF_ShellLevel            = $0002;
      SHCNF_RecursiveInterrupt    = $0000;  // unknown!
      SHCNF_NewDelivery           = $8000;
var notifyInfo : packed record
                   pidl         : pointer;
                   watchSubtree : longBool;
                 end;
    flags : dword;
    dll   : dword;
begin
  inherited Create;
  if (GetVersion and $80000000 = 0)         and         // NT-Familie
     (eventTypes * CShellEvents_Disk <> []) and         // Disk-Events gewünscht
     (root <> '') and (root <> '*')         then begin  // Pfad angegeben
    FWatchHandle := WatchDir(UnicodeString(root), watchSubtree, eventTypes, nil, WatchDirCallback);
    if FWatchHandle <> 0 then
      FWatchMsg := AddMsgHandler(WatchMsgHandlerProc);
    eventTypes := eventTypes - CShellEvents_Disk;
  end else
    FWatchHandle := 0;
  FEventTypes   := eventTypes;
  FThreadID     := GetCurrentThreadID;
  FEventProc    := eventProc;
  FEventProcOO  := eventProcOO;
  if @SHChangeNotifyRegister = nil then begin
    dll := LoadLibrary(shell32);
    SHChangeNotifyRegister   := GetProcAddress(dll, PAnsiChar(  2));
    SHChangeNotifyDeregister := GetProcAddress(dll, PAnsiChar(  4));
    ILFree                   := GetProcAddress(dll, PAnsiChar(155));
    SHSimpleIDListFromPath   := GetProcAddress(dll, PAnsiChar(162));//157));
  end;
  if eventTypes <> [] then begin
    FShMsg        := AddMsgHandler(ShMsgHandlerProc);
    FLockProc     := GetProcAddress(GetModuleHandleA(shell32), PAnsiChar(644));
    FUnlockProc   := GetProcAddress(GetModuleHandleA(shell32), PAnsiChar(645));
    if (root <> '') and (root <> '*') then begin
      if GetVersion and $80000000 = 0 then
           notifyInfo.pidl := SHSimpleIDListFromPath(PWideChar(UnicodeString(root)))
      else notifyInfo.pidl := SHSimpleIDListFromPath(PAnsiChar(              root ));
    end else notifyInfo.pidl := nil;
    notifyInfo.watchSubtree := watchSubtree;
    flags := SHCNF_InterruptLevel or SHCNF_ShellLevel or SHCNF_NewDelivery;
    if watchSubtree then
      flags := flags or SHCNF_RecursiveInterrupt;
    if (@SHChangeNotifyRegister <> nil) and (@SHChangeNotifyDeregister <> nil) and
       (@ILFree <> nil) and (@SHSimpleIDListFromPath <> nil) then begin
      FNotifyHandle := SHChangeNotifyRegister(MsgHandlerWindow, flags,
                                              ShellEventTypesToInternal(FEventTypes),
                                              FShMsg, 1, @notifyInfo);
      if notifyInfo.pidl <> nil then ILFree(notifyInfo.pidl);
    end;
  end;
end;

destructor TShellEventContainer.Destroy;
begin
  if FEventTypes <> [] then begin
    if FNotifyHandle <> 0 then begin
      SHChangeNotifyDeregister(FNotifyHandle);
      FNotifyHandle := 0;
    end;
    DelMsgHandler(ShMsgHandlerProc, FShMsg);
  end;
  if FWatchHandle <> 0 then begin
    UnwatchDir(FWatchHandle);
    DelMsgHandler(WatchMsgHandlerProc, FWatchMsg);
  end;
end;

procedure TShellEventContainer.ShMsgHandlerProc(window: HWND; msg: dword; wParam, lParam: NativeInt; var result: NativeInt);
var pidls  : pointer;
    events : TInternalShellEventTypes;
    lock   : cardinal;

  procedure DoCallback(event: TShellEventType; const obj1, obj2: IShellObj; drive: AnsiChar; value: cardinal);
  begin
    if event in FEventTypes then
      if @FEventProc <> nil then FEventProc  (event, obj1, obj2, drive, value)
      else                       FEventProcOO(event, obj1, obj2, drive, value);
  end;

  procedure ProcessEvent;
  var e1           : TInternalShellEventType;
      pidl1, pidl2 : pointer;
      driveMap     : cardinal;
      i1           : integer;
  begin
    pidl1 := pointer(pointer(NativeUInt(pidls) + 0)^);
    pidl2 := pointer(pointer(NativeUInt(pidls) + 4)^);
    for e1 := low(TInternalShellEventType) to high(TInternalShellEventType) do
      if e1 in events then begin
        case e1 of
          iseItemRenamed         : DoCallback(seItemRenamed,       ShellObj(IDList(pidl1, true)), ShellObj(IDList(pidl2, true)), #0, 0);
          iseItemCreated         : DoCallback(seItemCreated,       ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseItemDeleted         : DoCallback(seItemDeleted,       ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseDirCreated          : DoCallback(seDirCreated,        ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseDirDeleted          : DoCallback(seDirDeleted,        ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseMediaInserted       : DoCallback(seMediaInserted,     ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseMediaRemoved        : DoCallback(seMediaRemoved,      ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseDriveRemoved        : DoCallback(seDriveRemoved,      ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseDriveAdded          : DoCallback(seDriveAdded,        ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseShareAdded          : DoCallback(seShareAdded,        ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseShareRemoved        : DoCallback(seShareRemoved,      ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseAttributesChanged   : DoCallback(seAttributesChanged, ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseDirChanged          : DoCallback(seDirChanged,        ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseItemChanged         : DoCallback(seItemChanged,       ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseServerDisconnect    : DoCallback(seServerDisconnect,  ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseImageChanged        : DoCallback(seImageChanged,      nil, nil, #0, cardinal(pointer(NativeUInt(pidl1) + 2)^));
          iseDriveAddedGui       : DoCallback(seDriveAddedGui,     ShellObj(IDList(pidl1, true)), nil, #0, 0);
          iseDirRenamed          : DoCallback(seDirRenamed,        ShellObj(IDList(pidl1, true)), ShellObj(IDList(pidl2, true)), #0, 0);
          iseFreespaceChanged    : begin
                                     driveMap := cardinal(pointer(NativeUInt(pidl1) + 2)^);
                                     for i1 := 0 to 25 do
                                       if (1 shl i1) and driveMap <> 0 then
                                         DoCallback(seFreespaceChanged, nil, nil, AnsiChar(ord('A') + i1), 0);
                                   end;
          iseExtendedEventPreIe4,
          iseExtendedEvent       : case cardinal(pointer(NativeUInt(pidl1) + 2)^) of
                                     1 : DoCallback(seThemeChanged,   nil, nil, #0, 0);
                                     2 : DoCallback(seOrderChanged,   ShellObj(IDList(pidl2, true)), nil, #0, 0);
                                     4 : DoCallback(seMsiChanged,     nil, nil, #0, TPCardinal(pidl2)^);
                                     5 : DoCallback(seMsiUninstalled, nil, nil, #0, TPCardinal(pidl2)^);
                                   end;
          iseAssociationChanged  : DoCallback(seAssociationChanged, nil, nil, #0, 0);
        end;
      end;
  end;

begin
  if (@FLockProc <> nil) and (@FUnlockProc <> nil) then begin
    lock := FLockProc(cardinal(wParam), cardinal(lParam), pidls, events);
    if lock <> 0 then
      try
        ProcessEvent;
      finally FUnlockProc(lock) end;
  end else begin
    pidls  := pointer                 (wParam);
    events := TInternalShellEventTypes(lParam);
    ProcessEvent;
  end;
end;

type
  TWatchDirInfo = record
    watchType : TWatchType;
    old       : UnicodeString;
    new       : UnicodeString;
  end;

procedure TShellEventContainer.WatchMsgHandlerProc(window: HWND; msg: dword; wParam, lParam: NativeInt; var result: NativeInt);

  procedure DoCallback(event: TShellEventType; old, new: UnicodeString);
  var pidl     : PItemIdList;
      so1, so2 : IShellObj;
  begin
    so1 := nil;
    so2 := nil;
    pidl := SHSimpleIDListFromPath(PWideChar(old));
    if pidl <> nil then
      so1 := ShellObj(IDList(pidl));
    if new <> '' then begin
      pidl := SHSimpleIDListFromPath(PWideChar(new));
      if pidl <> nil then
        so2 := ShellObj(IDList(pidl));
    end;
    if @FEventProc <> nil then FEventProc  (event, so1, so2, #0, 0)
    else                       FEventProcOO(event, so1, so2, #0, 0);
  end;

begin
  if wParam = 789 then
    with TWatchDirInfo(pointer(lParam)^) do
      case watchType of
        wtAdded    : DoCallback(seItemCreated, new, '');
        wtRemoved  : DoCallback(seItemDeleted, old, '');
        wtRenamed  : DoCallback(seItemRenamed, old, new);
        wtModified : DoCallback(seItemChanged, old, '');
      end;
end;

procedure TShellEventContainer.WatchDirCallback(watchType: TWatchType; old, new_: UnicodeString);
var infoRec: TWatchDirInfo;
begin
  infoRec.watchType := watchType;
  infoRec.old       := old;
  infoRec.new       := new_;
  SendMessageA(MsgHandlerWindow(FThreadID), FWatchMsg, 789, NativeInt(@infoRec));
end;

function NewShellEvent(eventProc: TShellEvent; eventProcOO: TShellEventOO;
                       root: AnsiString; watchSubtree: boolean; eventTypes: TShellEventTypes) : boolean;
var i1 : integer;
    sn : TShellEventContainer;
begin
  sn := TShellEventContainer.Create(eventProc, eventProcOO, root, watchSubtree, eventTypes);
  result := (sn.FNotifyHandle <> 0) or (sn.FWatchHandle <> 0);
  if result then begin
    if ShellEventSection = nil then ShellEventSection := NewCriticalSection;
    ShellEventSection.Enter;
    try
      i1 := Length(ShellEventList);
      SetLength(ShellEventList, i1 + 1);
      ShellEventList[i1] := sn;
    finally ShellEventSection.Leave end;
  end else sn.Free;
end;

function RegisterShellEvent(eventProc    : TShellEvent;
                            root         : AnsiString       = '*';
                            watchSubtree : boolean          = true;
                            eventTypes   : TShellEventTypes = CShellEvents_All) : boolean;
begin
//  UnregisterShellEvent(eventProc);
  result := NewShellEvent(eventProc, nil, root, watchSubtree, eventTypes);
end;

function RegisterShellEvent(eventProc    : TShellEventOO;
                            root         : AnsiString       = '*';
                            watchSubtree : boolean          = true;
                            eventTypes   : TShellEventTypes = CShellEvents_All) : boolean;
begin
//  UnregisterShellEvent(eventProc);
  result := NewShellEvent(nil, eventProc, root, watchSubtree, eventTypes);
end;

function UnregisterShellEvent(event: TShellEvent) : boolean;
var i1, i2 : integer;
begin
  result := false;
  if ShellEventSection <> nil then begin
    ShellEventSection.Enter;
    try
      i2 := high(ShellEventList);
      for i1 := i2 downto 0 do
        with ShellEventList[i1] do
          if (FThreadID = GetCurrentThreadID) and (@FEventProc = @event) then begin
            Free;
            ShellEventList[i1] := ShellEventList[i2];
            SetLength(ShellEventList, i2);
            dec(i2);
            result := true;
          end;
    finally ShellEventSection.Leave end;
  end;
end;

function UnregisterShellEvent(event: TShellEventOO) : boolean;
var i1, i2 : integer;
begin
  result := false;
  if ShellEventSection <> nil then begin
    ShellEventSection.Enter;
    try
      i2 := high(ShellEventList);
      for i1 := i2 downto 0 do
        with ShellEventList[i1] do
          if (FThreadID = GetCurrentThreadID) and (int64(TMethod(FEventProcOO)) = int64(TMethod(event))) then begin
            Free;
            ShellEventList[i1] := ShellEventList[i2];
            SetLength(ShellEventList, i2);
            dec(i2);
            result := true;
          end;
    finally ShellEventSection.Leave end;
  end;
end;

// ***************************************************************

type
  // implements IDisplayMode
  TIDisplayMode = class (TIBasic, IDisplayMode)
  public
    FDevMode : TDevMode;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                        const devMode: TDevMode);

    function GetWidth        : integer;
    function GetHeight       : integer;
    function GetBitsPerPixel : integer;
    function GetNoOfColors   : integer;
    function GetRefreshRate  : integer;

    function IsInterlaced : boolean;
    function IsGrayScale  : boolean;

    function IsRestartNecessary : boolean;

    function Install (failIfRestartNecessary : boolean = true;
                      fullScreen             : boolean = true;
                      updateRegistry         : boolean = false;
                      forAllUsers            : boolean = false) : TDisplayModeInstallResult;

    function GetMaxInterface : IBasic; override;
  end;

constructor TIDisplayMode.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString;
                                 const devMode: TDevMode);
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then
    FDevMode := devMode;
end;

function TIDisplayMode.GetWidth : integer;
begin
  result := integer(FDevMode.dmPelsWidth);
end;

function TIDisplayMode.GetHeight : integer;
begin
  result := integer(FDevMode.dmPelsHeight);
end;

function TIDisplayMode.GetBitsPerPixel : integer;
begin
  result := integer(FDevMode.dmBitsPerPel);
end;

function TIDisplayMode.GetNoOfColors : integer;
begin
  if FDevMode.dmBitsPerPel < 32 then
       result := 1 shl integer(FDevMode.dmBitsPerPel)
  else result := 1 shl 24;
end;

function TIDisplayMode.GetRefreshRate : integer;
begin
  result := FDevMode.dmDisplayFrequency;
end;

function TIDisplayMode.IsInterlaced : boolean;
begin
  result := FDevMode.dmDisplayFlags and DM_INTERLACED <> 0;
end;

function TIDisplayMode.IsGrayScale : boolean;
begin
  result := FDevMode.dmDisplayFlags and DM_GRAYSCALE <> 0;
end;

function TIDisplayMode.IsRestartNecessary : boolean;
begin
  result := CheckValid and
            (ChangeDisplaySettings(FDevMode, CDS_TEST) = DISP_CHANGE_RESTART);
end;

function TIDisplayMode.Install(failIfRestartNecessary : boolean = true;
                               fullScreen             : boolean = true;
                               updateRegistry         : boolean = false;
                               forAllUsers            : boolean = false) : TDisplayModeInstallResult;
var c1 : cardinal;
begin
  if CheckValid then begin
    if not (failIfRestartNecessary and IsRestartNecessary) then begin
      c1 := 0;
      if fullScreen     then c1 := c1 or CDS_FULLSCREEN;
      if updateRegistry then c1 := c1 or CDS_UPDATEREGISTRY;
      if forAllUsers    then c1 := c1 or CDS_UPDATEREGISTRY or CDS_GLOBAL;
      c1 := ChangeDisplaySettings(FDevMode, c1);
      case c1 of
        DISP_CHANGE_SUCCESSFUL : result := dmOk;
        DISP_CHANGE_RESTART    : result := dmRestart;
        else                     begin
                                   SetLastError(c1);
                                   result := dmError;
                                 end;
      end;
    end else result := dmRestart;
  end else result := dmError;
end;

function TIDisplayMode.GetMaxInterface : IBasic;
begin
  result := IDisplayMode(self);
end;

// ***************************************************************

type
  // implements IDisplayModes
  TIDisplayModes = class (TICustomBasicList, IDisplayModes)
  public
    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString);

    function GetItem (index: integer) : IDisplayMode;

    function Item (width        : integer;
                   height       : integer;
                   bitsPerPixel : integer) : IDisplayMode;

    function GetMaxInterface : IBasic; override;
  end;

constructor TIDisplayModes.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: UnicodeString);

  procedure RefreshItem(dm: IDisplayMode);
  var i1 : integer;
  begin
    for i1 := 0 to FCount - 1 do
      with IDisplayMode(FItems[i1]) do
        if (dm.Width < Width) or
           ((dm.Width = Width) and (dm.Height < Height)) or
           ((dm.Width = Width) and (dm.Height = Height) and (dm.BitsPerPixel < BitsPerPixel)) or
           ((dm.Width = Width) and (dm.Height = Height) and (dm.BitsPerPixel = BitsPerPixel) and (dm.RefreshRate < RefreshRate)) then begin
          InsertItem(dm, i1);
          exit;
        end;
    AddItem(dm);
  end;

const CRes : array [0..12] of record x, y : cardinal; end =
             ((x: 320; y: 200), (x: 320; y: 240), (x: 320; y: 350), (x: 320; y: 400), (x: 320; y: 480),
              (x: 360; y: 200), (x: 360; y: 240), (x: 360; y: 350), (x: 360; y: 400), (x: 360; y: 480),
              (x: 400; y: 300), (x: 512; y: 384), (x: 640; y: 400));
      CBpp : array [0..4] of cardinal = (8, 15, 16, 24, 32);
var dm         : TDevMode;
    i1, i2, i3 : integer;
    b1         : boolean;
    c1         : cardinal;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    zeroMemory(@dm, sizeOf(TDevMode));
    dm.dmSize := sizeOf(TDevMode);
    while EnumDisplaySettings(nil, FCount, dm) do
      RefreshItem(TIDisplayMode.Create(true, 0, '', dm));
    zeroMemory(@dm, sizeOf(TDevMode));
    dm.dmSize       := sizeOf(TDevMode);
    dm.dmFields     := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL;
    dm.dmPelsWidth  := 777;
    dm.dmPelsHeight := 777;
    dm.dmBitsPerPel := 32;
    c1 := cardinal(ChangeDisplaySettings(dm, CDS_TEST));
    if (c1 <> DISP_CHANGE_RESTART) and (c1 <> DISP_CHANGE_SUCCESSFUL) then
      for i1 := 0 to high(CRes) do
        for i2 := 0 to high(CBpp) do begin
          b1 := true;
          for i3 := 0 to FCount - 1 do
            with TIDisplayMode(FItems[i3].SelfAsTObject).FDevMode do
              if (CRes[i1].x = dmPelsWidth) and (CRes[i1].y = dmPelsHeight) and
                 (CBpp[i2] = dmBitsPerPel) then begin
                b1 := false;
                break;
              end;
          if b1 then begin
            dm.dmPelsWidth  := CRes[i1].x;
            dm.dmPelsHeight := CRes[i1].y;
            dm.dmBitsPerPel := CBpp[i2];
            c1 := cardinal(ChangeDisplaySettings(dm, CDS_TEST));
            if (c1 = DISP_CHANGE_RESTART) or (c1 = DISP_CHANGE_SUCCESSFUL) then
              RefreshItem(TIDisplayMode.Create(true, 0, '', dm));
          end;
        end;
  end;
end;

function TIDisplayModes.GetItem(index: integer) : IDisplayMode;
var dm : TDevMode;
begin
  if (index < 0) or (index >= FCount) then
       result := TIDisplayMode.Create(false, CErrorNo_IndexOutOfRange, UnicodeString(AnsiString(CErrorStr_IndexOutOfRange)), dm)
  else result := IDisplayMode(FItems[index]);
end;

function TIDisplayModes.Item(width        : integer;
                             height       : integer;
                             bitsPerPixel : integer) : IDisplayMode;
var dm : TDevMode;
    i1 : integer;
begin
  for i1 := FCount - 1 downto 0 do
    with TIDisplayMode(FItems[i1].SelfAsTObject).FDevMode do
      if (width = integer(dmPelsWidth)) and (height = integer(dmPelsHeight)) and
         (bitsPerPixel = integer(dmBitsPerPel)) then begin
        result := IDisplayMode(FItems[i1]);
        exit;
      end;
  result := TIDisplayMode.Create(false, ERROR_FILE_NOT_FOUND, '', dm);
end;

function TIDisplayModes.GetMaxInterface : IBasic;
begin
  result := IDisplayModes(self);
end;

function DisplayMode(width        : integer;
                     height       : integer;
                     bitsPerPixel : integer;
                     refreshRate  : integer = 0) : IDisplayMode;
var dm : TDevMode;
    c1 : cardinal;
begin
  zeroMemory(@dm, sizeOf(TDevMode));
  dm.dmSize       := sizeOf(TDevMode);
  dm.dmFields     := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL;
  dm.dmPelsWidth  := cardinal(width);
  dm.dmPelsHeight := cardinal(height);
  dm.dmBitsPerPel := cardinal(bitsPerPixel);
  if refreshRate <> 0 then begin
    dm.dmDisplayFrequency := refreshRate;
    dm.dmFields := dm.dmFields or DM_DISPLAYFREQUENCY;
  end;
  c1 := cardinal(ChangeDisplaySettings(dm, CDS_TEST));
  result := TIDisplayMode.Create((c1 = DISP_CHANGE_RESTART) or (c1 = DISP_CHANGE_SUCCESSFUL),
                                 c1, '', dm);
end;

function CurrentDisplayMode : IDisplayMode;
var dm : TDevMode;
begin
  zeroMemory(@dm, sizeOf(TDevMode));
  dm.dmSize := sizeOf(TDevMode);
  EnumDisplaySettings(nil, dword(-1), dm);
  result := TIDisplayMode.Create(true, 0, '', dm);
end;

function RestoreDisplayMode(dontResizeOtherWindows : boolean = false) : TDisplayModeInstallResult;
var c1 : cardinal;
begin
  if dontResizeOtherWindows then c1 := CDS_FULLSCREEN
  else                           c1 := 0;
  c1 := cardinal(ChangeDisplaySettings(PDevMode(nil)^, c1));
  case c1 of
    DISP_CHANGE_SUCCESSFUL : result := dmOk;
    DISP_CHANGE_RESTART    : result := dmRestart;
    else                     begin
                               SetLastError(c1);
                               result := dmError;
                             end;
  end;
end;

function DisplayModes : IDisplayModes;
begin
  result := TIDisplayModes.Create(true, 0, '');
end;

// ***************************************************************

end.
