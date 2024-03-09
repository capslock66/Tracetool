// ***************************************************************
//  madTools.pas              version: 1.3.7  ·  date: 2023-03-08
//  -------------------------------------------------------------
//  several basic tool functions
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2023 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2023-03-08 1.3.7 added support for detecting Windows 11
// 2021-05-12 1.3.6 added support for detecting Windows 2019
// 2018-05-17 1.3.5 added "nil" detection in ExportToFunc helper function
// 2017-03-13 1.3.4 fixed crash with Windows XP Black editions
// 2015-04-20 1.3.3 (1) added detection for Windows 8.1, Windows 10 etc
//                  (2) fixed MsgHandlerWindow atom leak
// 2012-09-05 1.3.2 added unmangle support for XE3 x64 package export names
// 2012-07-03 1.3.1 added UIPI workaround to MsgHandler functionality
// 2012-04-03 1.3.0 (1) added x64 support
//                  (2) most functions are now available as xxx, xxxA and xxxW
// 2011-03-27 1.2w  (1) added GetFileVersionStr
//                  (2) added special handling for "wine"
// 2009-07-14 1.2v  added osWin2008, osWin7, osWin2008r2
// 2009-02-09 1.2u  (1) Delphi 2009 support
//                  (2) added some unicode function overloads
// 2006-11-28 1.2t  (1) limited support for 64bit modules added
//                  (2) "OS" detects Vista, Media Center, x64, etc
// 2005-06-11 1.2s  "ResToStr" returns a resource as binary data in a string
// 2005-02-05 1.2r  GetImageNtHeaders avoids inconvenient debugger exceptions
// 2004-03-12 1.2q  AMD64 NX: New -> VirtualAlloc (in MethodToProcedure)
// 2003-10-05 1.2p  (1) support for Windows 2003 added
//                  (2) OS.enum renamed to OS.Enum -> BCB support
// 2003-06-09 1.2o  (1) GetImageProcAddress now handles forwarded APIs correctly
//                  (2) minor bug in GetImageNtHeaders fixed
//                  (3) some other minor bug fixes / improvements
// 2002-12-27 1.2m  FindModule + GetImageProcName added
// 2002-11-20 1.2l  make OS.description work even after madTools.finalization
// 2002-10-25 1.2k  some low level module image parsing functions added
// 2002-09-05 1.2j  GetFreeSystemResources now simply returns 0 in NT family
// 2002-06-04 1.2i  little NT4 bug workaround, see MsgHandlerWindow
// 2002-04-24 1.2h  mutex and window class name now process+module dependent
// 2002-02-24 1.2g  MsgHandler is now using mutex instead of critical section
// 2002-02-16 1.2f  MsgHandler stuff rewritten, thread safe etc.
// 2001-07-23 1.2e  Add/DelMsgHandler now can also work with other threads
// 2001-07-22 1.2d  fix for wrong OS information ("setup.exe" on ME)
// 2001-07-15 1.2c  new functions added (1) GetFreeSystemResources
//                                      (2) GetFileVersion / FileVersionToStr
// 2001-05-19 1.2b  osWinXP added
// 2001-04-10 1.2a  TOS.description added
// 2000-11-22 1.2   MsgHandlerWindow (and related) functionality added
// 2000-07-25 1.1a  minor changes in order to get rid of SysUtils

unit madTools;

{$I mad.inc}

interface

uses Windows, madTypes;

// ***************************************************************

type
  // types for the "OS" function
  TOsEnum = (osNone, osWin95, osWin95osr2, osWin98, osWin98se, osWinME, osWin9xNew,
                     osWinNtOld, osWinNt4, osWin2k, osWinXP, osWin2003, osWinVista, osWin2008, osWin7, osWin2008r2, osWin8, osWin2012, osWin81, osWin2012r2, osWin10, osWin2016, osWin2019, osWin11, osWinNtNew);
  TOS = record
    major       : cardinal;
    minor       : cardinal;
    build       : cardinal;
    spStr       : UnicodeString;
    win9x       : boolean;
    win9xEnum   : TOsEnum;//osNone..osWin9xNew;   BCB doesn't like this
    winNt       : boolean;
    winNtEnum   : TOsEnum;
    Enum        : TOsEnum;
    x64         : boolean;
    spNo        : cardinal;
    description : UnicodeString;
  end;

const
  // operating system strings
  COsDescr : array [TOsEnum] of PAnsiChar =
             ('None', 'Windows 95', 'Windows 95 OSR-2', 'Windows 98', 'Windows 98 SE',
                         'Windows ME', 'Windows 9x New',
                      'Windows NT 3', 'Windows NT 4', 'Windows 2000', 'Windows XP',
                         'Windows 2003', 'Windows Vista', 'Windows 2008', 'Windows 7', 'Windows 2008 R2',
                         'Windows 8', 'Windows 2012', 'Windows 8.1', 'Windows 2012 R2', 'Windows 10', 'Windows 2016', 'Windows 2019', 'Windows 11', 'Windows NT New');

// Tests which system is running...
function OS : TOS;

// ***************************************************************

{$ifndef win64}

  // returns the 9x resource usage; 0: System; 1: GDI; 2: User
  function GetFreeSystemResources (resource: word) : word;

{$endif}

// ***************************************************************

// returns the short respectively the long variant of the filename
function GetShortFileName (fileName:    AnsiString) :    AnsiString; {$ifdef UnicodeOverloads} overload;
function GetShortFileName (fileName: UnicodeString) : UnicodeString; overload; {$endif}
function GetLongFileName  (fileName:    AnsiString) :    AnsiString; {$ifdef UnicodeOverloads} overload;
function GetLongFileName  (fileName: UnicodeString) : UnicodeString; overload; {$endif}

var
  GetShortFileNameA : function (fileName: AnsiString) : AnsiString = GetShortFileName;
  GetLongFileNameA  : function (fileName: AnsiString) : AnsiString = GetLongFileName;

{$ifdef UnicodeOverloads}
  var
    GetShortFileNameW : function (fileName: UnicodeString) : UnicodeString = GetShortFileName;
    GetLongFileNameW  : function (fileName: UnicodeString) : UnicodeString = GetLongFileName;
{$else}
  function GetShortFileNameW (fileName: UnicodeString) : UnicodeString;
  function GetLongFileNameW  (fileName: UnicodeString) : UnicodeString;
{$endif}

// ***************************************************************

// returns the version number of a file, can be used on e.g. system dlls
function GetFileVersion    (const file_:    AnsiString) : int64; {$ifdef UnicodeOverloads} overload;
function GetFileVersion    (const file_: UnicodeString) : int64; overload; {$endif}
function GetFileVersionStr (const file_:    AnsiString) : AnsiString; {$ifdef UnicodeOverloads} overload;
function GetFileVersionStr (const file_: UnicodeString) : UnicodeString; overload; {$endif}
function FileVersionToStr  (version : int64) : AnsiString;
function FileVersionToStrW (version : int64) : UnicodeString;

var
  GetFileVersionA    : function (const file_: AnsiString) : int64 = GetFileVersion;
  GetFileVersionStrA : function (const file_: AnsiString) : AnsiString = GetFileVersionStr;
  FileVersionToStrA  : function (version : int64) : AnsiString = FileVersionToStr;

{$ifdef UnicodeOverloads}
  var
    GetFileVersionW    : function (const file_: UnicodeString) : int64 = GetFileVersion;
    GetFileVersionStrW : function (const file_: UnicodeString) : UnicodeString = GetFileVersionStr;
{$else}
  function GetFileVersionW    (const file_: UnicodeString) : int64;
  function GetFileVersionStrW (const file_: UnicodeString) : UnicodeString;
{$endif}

// ***************************************************************

// converts a procedure/function to a method
function ProcedureToMethod (self: TObject; procAddr: pointer) : TMethod;

// converts a method to a procedure/function
// CAUTION: this works only for stdcall methods!!
// you should free the procedure pointer (FreeMem), when you don't need it anymore
function MethodToProcedure (self: TObject; methodAddr: pointer; maxParamCount: integer = 32) : pointer; overload;
function MethodToProcedure (method: TMethod; maxParamCount: integer = 32) : pointer; overload;

// ***************************************************************

type
  // types for AddMsgHandler/DelMsgHandler
  TMsgHandler   = procedure (window: HWND; msg: cardinal; wParam, lParam: NativeInt; var result: NativeInt);
  TMsgHandlerOO = procedure (window: HWND; msg: cardinal; wParam, lParam: NativeInt; var result: NativeInt) of object;

// returns the message handler window handle of the specified thread
// if no such window exists yet (and if threadID = 0) then the window is created
function MsgHandlerWindow (threadID: cardinal = 0) : cardinal;

// add/delete a message handler for the message handler window of the specified thread
function AddMsgHandler (handler: TMsgHandler;   msg: cardinal = 0; threadID: cardinal = 0) : cardinal; overload;
function AddMsgHandler (handler: TMsgHandlerOO; msg: cardinal = 0; threadID: cardinal = 0) : cardinal; overload;
function DelMsgHandler (handler: TMsgHandler;   msg: cardinal = 0; threadID: cardinal = 0) : boolean;  overload;
function DelMsgHandler (handler: TMsgHandlerOO; msg: cardinal = 0; threadID: cardinal = 0) : boolean;  overload;

// ***************************************************************

const
  // PE header constants
  CENEWHDR = $003C;  // offset of new EXE header
  CEMAGIC  = $5A4D;  // old EXE magic id:  'MZ'
  CPEMAGIC = $4550;  // NT portable executable
  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $10b;  // 32bit PE file
  {$EXTERNALSYM IMAGE_NT_OPTIONAL_HDR32_MAGIC}
  IMAGE_NT_OPTIONAL_HDR64_MAGIC = $20b;  // 64bit PE file
  {$EXTERNALSYM IMAGE_NT_OPTIONAL_HDR64_MAGIC}

type
  // PE header types
  TImageImportDirectory = packed record
    HintNameArray  : dword;
    TimeDateStamp  : dword;
    ForwarderChain : dword;
    Name_          : dword;
    ThunkArray     : dword;
  end;
  PImageImportDirectory = ^TImageImportDirectory;
  TImageExportDirectory = packed record
    Characteristics       : dword;
    TimeDateStamp         : dword;
    MajorVersion          : word;
    MinorVersion          : word;
    Name_                 : dword;
    Base                  : dword;
    NumberOfFunctions     : integer;
    NumberOfNames         : integer;
    AddressOfFunctions    : dword;
    AddressOfNames        : dword;
    AddressOfNameOrdinals : dword;
  end;
  PImageExportDirectory = ^TImageExportDirectory;

  {$ifndef xe2}
    TImageOptionalHeader64 = packed record
      Magic                       : word;
      MajorLinkerVersion          : byte;
      MinorLinkerVersion          : byte;
      SizeOfCode                  : dword;
      SizeOfInitializedData       : dword;
      SizeOfUninitializedData     : dword;
      AddressOfEntryPoint         : dword;
      BaseOfCode                  : dword;
      ImageBase                   : int64;
      SectionAlignment            : dword;
      FileAlignment               : dword;
      MajorOperatingSystemVersion : word;
      MinorOperatingSystemVersion : word;
      MajorImageVersion           : word;
      MinorImageVersion           : word;
      MajorSubsystemVersion       : word;
      MinorSubsystemVersion       : word;
      Win32VersionValue           : dword;
      SizeOfImage                 : dword;
      SizeOfHeaders               : dword;
      CheckSum                    : dword;
      Subsystem                   : word;
      DllCharacteristics          : word;
      SizeOfStackReserve          : int64;
      SizeOfStackCommit           : int64;
      SizeOfHeapReserve           : int64;
      SizeOfHeapCommit            : int64;
      LoaderFlags                 : dword;
      NumberOfRvaAndSizes         : dword;
      DataDirectory               : array [0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of IMAGE_DATA_DIRECTORY;
    end;
    PImageOptionalHeader64 = ^TImageOptionalHeader64;

    TImageNtHeaders64 = packed record
      Signature      : dword;
      FileHeader     : TImageFileHeader;
      OptionalHeader : TImageOptionalHeader64;
    end;
    PImageNtHeaders64 = ^TImageNtHeaders64;

    TImageOptionalHeader32 = TImageOptionalHeader;
    TImageNtHeaders32 = TImageNtHeaders;
    PImageNtHeaders32 = PImageNtHeaders;
  {$endif}

// find out and return whether the dll file is a 64bit dll
function Is64bitModule (fileName: PWideChar) : bool; stdcall;

// does the DLL export an API under the ordinal of 1?
function DoesModuleExportOrdinal1 (fileName: PWideChar) : boolean; stdcall;

// find into which module the specified address belongs (if any)
function FindModule (addr: pointer; var moduleHandle: HMODULE; var moduleName:    AnsiString) : boolean; {$ifdef UnicodeOverloads} overload;
function FindModule (addr: pointer; var moduleHandle: HMODULE; var moduleName: UnicodeString) : boolean; overload; {$endif}

var FindModuleA : function (addr: pointer; var moduleHandle: HMODULE; var moduleName: AnsiString) : boolean = FindModule;
{$ifdef UnicodeOverloads}
  var FindModuleW : function (addr: pointer; var moduleHandle: HMODULE; var moduleName: UnicodeString) : boolean = FindModule;
{$else}
  function FindModuleW (addr: pointer; var moduleHandle: HMODULE; var moduleName: UnicodeString) : boolean;
{$endif}

// some low level module image parsing functions
function GetImageNtHeaders       (module: HMODULE) : PImageNtHeaders32;
function GetImageImportDirectory (module: HMODULE) : PImageImportDirectory;
function GetImageExportDirectory (module: HMODULE) : PImageExportDirectory;

// most of the time GetImageProcAddress is equal to GetProcAddress, except:
// (1) IAT hooking often hooks GetProcAddress, too, and fakes the result
// (2) in win9x GetProcAddress refuses to work for ordinal kernel32 APIs
function GetImageProcAddress (module: HMODULE; const name : AnsiString; doubleCheck: boolean = false) : pointer; overload;
function GetImageProcAddress (module: HMODULE; index      : integer                                 ) : pointer; overload;

// this is the opposite of Get(Image)ProcAddress
function GetImageProcName  (module: HMODULE; proc: pointer; unmangle: boolean) : AnsiString;
function GetImageProcNameW (module: HMODULE; proc: pointer; unmangle: boolean) : UnicodeString;
var GetImageProcNameA : function (module: HMODULE; proc: pointer; unmangle: boolean) : AnsiString = GetImageProcName;

// returns a resource as binary data in a string
function ResToStr (module: HMODULE; resType: PWideChar; const resName: UnicodeString) : AnsiString;

// ***************************************************************

// try..except/finally works only if you have SysUtils in your uses clause
// call this function and it works without SysUtils, too
procedure InitTryExceptFinally;

// ***************************************************************

// internal functions, please ignore
const INVALID_FILE_ATTRIBUTES = DWORD($FFFFFFFF);
{$EXTERNALSYM INVALID_FILE_ATTRIBUTES}
function NeedModuleFileMap(module: HMODULE) : pointer;
function UnmangleA(var publicName, unitName: AnsiString) : boolean;
function UnmangleW(var publicName, unitName: UnicodeString) : boolean;
function VirtualToRaw(nh: PImageNtHeaders32; addr: dword) : dword;
function GetSizeOfImage(nh: PImageNtHeaders32) : dword;
function TickDif(tick: dword) : dword;
function IsBadReadPtr2(src: pointer; count: dword) : boolean;
function IsBadWritePtr2(dst: pointer; count: dword) : boolean;
var CheckProcAddress : function (var addr: pointer) : boolean = nil;
    NeedModuleFileMapEx : function (module: HMODULE) : pointer = nil;
    HideLeak_ : function (handle: THandle) : boolean = nil;
    IsWine : boolean = false;

implementation

uses Messages, madStrings;

// ***************************************************************

var os_     : TOS;
    osReady : boolean = false;
function OS : TOS;
const PROCESSOR_ARCHITECTURE_AMD64 = 9;
      VER_NT_WORKSTATION           = 1;
      SM_TABLEPC                   = 86;
      SM_MEDIACENTER               = 87;
      SM_STARTER                   = 88;
      SM_SERVERR2                  = 89;
type TOsVersionInfoExW = packed record
                           dwOSVersionInfoSize : dword;
                           dwMajorVersion      : dword;
                           dwMinorVersion      : dword;
                           dwBuildNumber       : dword;
                           dwPlatformId        : dword;
                           szCSDVersion        : array [0..127] of WideChar;
                           wServicePackMajor   : word;
                           wServicePackMinor   : word;
                           wSuiteMask          : word;
                           wProductType        : byte;
                           wReserved           : byte;
                         end;
var viA  : TOsVersionInfoA;
    viW  : TOsVersionInfoExW;
    i1   : integer;
    gnsi : procedure (var si: TSystemInfo) stdcall;
    si   : TSystemInfo;
    rgv  : function (var viW: TOsVersionInfoExW) : HRESULT; stdcall;
begin
  if (not osReady) or (os_.description = '') then begin
    osReady := true;
    if GetVersion and $80000000 = 0 then begin
      ZeroMemory(@viW, sizeOf(viW));
      viW.dwOSVersionInfoSize := sizeOf(TOsVersionInfoExW);
      rgv := GetProcAddress(GetModuleHandle('ntdll.dll'), 'RtlGetVersion');
      if (@rgv = nil) or (rgv(viW) <> 0) then
        if not GetVersionExW(POsVersionInfo(@viW)^) then begin
          viW.dwOSVersionInfoSize := sizeOf(TOsVersionInfoW);
          GetVersionExW(POsVersionInfo(@viW)^);
        end;
    end else begin
      ZeroMemory(@viA, sizeOf(viA));
      viA.dwOSVersionInfoSize := sizeOf(viA);
      GetVersionExA(viA);
      Move(viA, viW, sizeOf(viA));
      for i1 := low(viA.szCSDVersion) to high(viA.szCSDVersion) do
        viW.szCSDVersion[i1] := WideChar(viA.szCSDVersion[i1]);
    end;
    with os_ do begin
      major := viW.dwMajorVersion;
      minor := viW.dwMinorVersion;
      spStr := viW.szCSDVersion;
      win9x := viW.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS;
      winNt := viW.dwPlatformId = VER_PLATFORM_WIN32_NT;
      if win9x then build := word(viW.dwBuildNumber)
      else          build := viW.dwBuildNumber;
      enum := osNone;
      spNo := 0;
      if win9x then begin
        case major of
          0..3 : ;
          4    : case minor of
                   00..09 : if      build > 1000 then enum := osWin95osr2
                            else                      enum := osWin95;
                   10     : if      build > 2700 then enum := osWinME
                            else if build > 2000 then enum := osWin98se
                            else                      enum := osWin98;
                   11..90 : enum := osWinME;
                   else     enum := osWin9xNew;
                 end;
          else   enum := osWin9xNew;
        end;
        win9xEnum := enum;
        winNtEnum := osNone;
      end else if winNt then begin
        case major of
          0..3 : enum := osWinNtOld;
          4    : enum := osWinNt4;
          5    : case minor of
                   0  : enum := osWin2k;
                   1  : enum := osWinXP;
                   else begin
                          if viW.wProductType = VER_NT_WORKSTATION then
                               enum := osWinXP
                          else enum := osWin2003;
                        end;
                 end;
          6    : case minor of
                   0  : if viW.wProductType = VER_NT_WORKSTATION then
                             enum := osWinVista
                        else enum := osWin2008;
                   1  : if viW.wProductType = VER_NT_WORKSTATION then
                             enum := osWin7
                        else enum := osWin2008r2;
                   2  : if viW.wProductType = VER_NT_WORKSTATION then
                             enum := osWin8
                        else enum := osWin2012;
                   3  : if viW.wProductType = VER_NT_WORKSTATION then
                             enum := osWin81
                        else enum := osWin2012r2;
                   else enum := osWinNtNew;
                 end;
          10   : if viW.wProductType = VER_NT_WORKSTATION then begin
                   if build >= 22000 then
                     enum := osWin11
                   else
                     enum := osWin10;
                 end else
                   if build >= 17763 then
                     enum := osWin2019
                   else
                     enum := osWin2016;
          else   enum := osWinNtNew;
        end;
        win9xEnum := osNone;
        winNtEnum := enum;
        if viW.dwOSVersionInfoSize >= sizeOf(TOsVersionInfoExW) then
          spNo := viW.wServicePackMajor
        else
          if Length(spStr) >= 14 then
            spNo := StrToIntExW(false, @spStr[14], Length(spStr) - 13);
        gnsi := GetProcAddress(GetModuleHandle(kernel32), 'GetNativeSystemInfo');
        if @gnsi <> nil then begin
          ZeroMemory(@si, sizeOf(si));
          gnsi(si);
          x64 := si.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64;
        end;
      end;
      description := UnicodeString(AnsiString(COsDescr[enum]));
      if winNt then begin
        if GetSystemMetrics(SM_SERVERR2) <> 0 then
          description := description + ' R2';
        if GetSystemMetrics(SM_STARTER) <> 0 then
          description := description + ' Starter';
        if (enum < osWinVista) and (GetSystemMetrics(SM_MEDIACENTER) <> 0) then
          description := description + ' Media Center';
        if x64 then
          description := description + ' x64';
        if spStr <> '' then
          description := description + ' ' + spStr;
      end;
    end;
  end;
  result := os_;
end;

// ***************************************************************

{$ifndef win64}

//this was nice, but unfortunately doesn't work in BCB:

//function LoadLibrary16    (libraryName : PAnsiChar                 ) : dword;   stdcall; external kernel32 index 35;
//function FreeLibrary16    (hInstance   : dword                     ) : integer; stdcall; external kernel32 index 36;
//function GetProcAddress16 (hinstance   : dword; procName: PAnsiChar) : pointer; stdcall; external kernel32 index 37;

// so we have to do it the hard way:

var LoadLibrary16    : function (libraryName : PAnsiChar                 ) : dword   stdcall = nil;
    FreeLibrary16    : function (hInstance   : dword                     ) : integer stdcall = nil;
    GetProcAddress16 : function (hinstance   : dword; procName: PAnsiChar) : pointer stdcall = nil;

function GetFreeSystemResources(resource: word) : word;
var thunkTrash : array [0..$3f] of byte;
    user16     : dword;
    gfsr       : pointer;
    qtt        : pointer;
    dll        : dword;
begin
  result := 0;
  if GetVersion and $80000000 <> 0 then begin
    if @LoadLibrary16 = nil then begin
      dll := GetModuleHandle(kernel32);
      LoadLibrary16    := GetImageProcAddress(dll, 35);
      FreeLibrary16    := GetImageProcAddress(dll, 36);
      GetProcAddress16 := GetImageProcAddress(dll, 37);
    end;
    if @LoadLibrary16 <> nil then begin
      user16 := LoadLibrary16('user.exe');
      if user16 <> 0 then begin
        thunkTrash[0] := 0;
        gfsr := GetProcAddress16(user16, 'GetFreeSystemResources');
        qtt  := GetProcAddress(GetModuleHandle(kernel32), 'QT_Thunk');
        if (gfsr <> nil) and (qtt <> nil) then
          asm
            push resource
            mov edx, gfsr
            call qtt
            mov result, ax
          end;
        FreeLibrary16(user16);
      end;
    end;
  end;
end;

{$endif}

// ***************************************************************

function ExtractFileDriveA(const fileName: AnsiString) : AnsiString;
var i1 : integer;
begin
  result := '';
  if Length(fileName) >= 2 then
    if (fileName[1] = '\') and (fileName[2] = '\') then begin
      i1 := PosStrA('\', fileName, 3);
      if (i1 > 0) and (i1 < Length(fileName)) then begin
        i1 := PosStrA('\', fileName, i1 + 1);
        if i1 > 0 then
          result := Copy(fileName, 1, i1)
        else
          result := fileName;
      end;
    end else
      if fileName[2] = ':' then
        result := Copy(fileName, 1, 3);
end;

function GetShortFileName(fileName: AnsiString) : AnsiString;
var c1, c2 : dword;
    wfd    : TWin32FindDataA;
    fh     : THandle;
begin
  result := '';
  if (fileName <> '') and (fileName[Length(fileName)] = '\') then
    Delete(fileName, Length(fileName), 1);
  c2 := Length(ExtractFileDriveA(fileName));
  repeat
    c1 := PosStrA('\', fileName, maxInt, 1);
    if (c2 = 0) or (c1 < c2) then begin
      result := fileName + '\' + result;
      break;
    end;
    {$ifdef d6}
      if (PosStrA('*', fileName, c1 + 1) = 0) and (PosStrA('?', fileName, c1 + 1) = 0) then begin
    {$else}
      // Delphi 4 compiler needs it this way, or else we get internal error C11567 
      if (PosStr('*', fileName, c1 + 1) = 0) and (PosStr('?', fileName, c1 + 1) = 0) then begin
    {$endif}
      fh := FindFirstFileA(PAnsiChar(fileName), wfd);
      if fh <> INVALID_HANDLE_VALUE then begin
        windows.FindClose(fh);
        if wfd.cAlternateFileName[0] <> #0 then
          result := AnsiString(wfd.cAlternateFileName) + '\' + result
        else
          result := AnsiString(wfd.cFileName         ) + '\' + result;
      end else
        result := Copy(fileName, c1 + 1, maxInt) + '\' + result;
    end else
      result := Copy(fileName, c1 + 1, maxInt) + '\' + result;
    Delete(fileName, c1, maxInt);
  until (c1 = 0) or (fileName = '');
  if (result <> '') and (result[Length(result)] = '\') then
    Delete(result, Length(result), 1);
end;

{$ifdef UnicodeOverloads}
function GetShortFileName(fileName: UnicodeString) : UnicodeString;
{$else}
function GetShortFileNameW(fileName: UnicodeString) : UnicodeString;
{$endif}
var c1, c2 : cardinal;
    wfd    : TWin32FindDataW;
    fh     : THandle;
begin
  result := '';
  if GetVersion and $80000000 <> 0 then begin
    result := UnicodeString(GetShortFileName(AnsiString(fileName)));
    exit;
  end;
  if (fileName <> '') and (fileName[Length(fileName)] = '\') then
    Delete(fileName, Length(fileName), 1);
  c2 := Length(ExtractFileDriveW(fileName));
  repeat
    c1 := PosStrW('\', fileName, maxInt, 1);
    if (c2 = 0) or (c1 < c2) then begin
      result := fileName + '\' + result;
      break;
    end;
    if (PosStrW('*', fileName, c1 + 1) = 0) and (PosStrW('?', fileName, c1 + 1) = 0) then begin
      fh := FindFirstFileW(PWideChar(fileName), wfd);
      if fh <> INVALID_HANDLE_VALUE then begin
        windows.FindClose(fh);
        if wfd.cAlternateFileName[0] <> #0 then
          result := UnicodeString(wfd.cAlternateFileName) + '\' + result
        else
          result := UnicodeString(wfd.cFileName         ) + '\' + result;
      end else
        result := Copy(fileName, c1 + 1, maxInt) + '\' + result;
    end else
      result := Copy(fileName, c1 + 1, maxInt) + '\' + result;
    Delete(fileName, c1, maxInt);
  until (c1 = 0) or (fileName = '');
  if (result <> '') and (result[Length(result)] = '\') then
    Delete(result, Length(result), 1);
end;

function GetLongFileName(fileName: AnsiString) : AnsiString;
var c1, c2 : cardinal;
    wfd    : TWin32FindDataA;
    fh     : THandle;
begin
  result := '';
  if (fileName <> '') and (fileName[Length(fileName)] = '\') then
    Delete(fileName, Length(fileName), 1);
  c2 := Length(ExtractFileDriveA(fileName));
  repeat
    c1 := PosStrA('\', fileName, maxInt, 1);
    if (c2 = 0) or (c1 < c2) then begin
      result := fileName + '\' + result;
      break;
    end;
    {$ifdef d6}
      if (PosStrA('~', fileName, c1 + 1) > 0) and (PosStrA('*', fileName, c1 + 1) = 0) and (PosStrA('?', fileName, c1 + 1) = 0) then begin
    {$else}
      // Delphi 4 compiler needs it this way, or else we get internal error C11567
      if (PosStr('~', fileName, c1 + 1) > 0) and (PosStr('*', fileName, c1 + 1) = 0) and (PosStr('?', fileName, c1 + 1) = 0) then begin
    {$endif}
      fh := FindFirstFileA(PAnsiChar(fileName), wfd);
      if fh <> INVALID_HANDLE_VALUE then begin
        windows.FindClose(fh);
        result := AnsiString(wfd.cfileName) + '\' + result;
      end else
        result := Copy(fileName, c1 + 1, maxInt) + '\' + result;
    end else
      result := Copy(fileName, c1 + 1, maxInt) + '\' + result;
    Delete(fileName, c1, maxInt);
  until (c1 = 0) or (fileName = '');
  if (result <> '') and (result[Length(result)] = '\') then
    Delete(result, Length(result), 1);
end;

{$ifdef UnicodeOverloads}
function GetLongFileName(fileName: UnicodeString) : UnicodeString;
{$else}
function GetLongFileNameW(fileName: UnicodeString) : UnicodeString;
{$endif}
var c1, c2 : cardinal;
    wfd    : TWin32FindDataW;
    fh     : THandle;
begin
  result := '';
  if GetVersion and $80000000 <> 0 then begin
    result := UnicodeString(GetLongFileNameA(AnsiString(fileName)));
    exit;
  end;
  if (fileName <> '') and (fileName[Length(fileName)] = '\') then
    Delete(fileName, Length(fileName), 1);
  c2 := Length(ExtractFileDriveW(fileName));
  repeat
    c1 := PosStrW('\', fileName, maxInt, 1);
    if (c2 = 0) or (c1 < c2) then begin
      result := fileName + '\' + result;
      break;
    end;
    if (PosStrW('~', fileName, c1 + 1) > 0) and (PosStrW('*', fileName, c1 + 1) = 0) and (PosStrW('?', fileName, c1 + 1) = 0) then begin
      fh := FindFirstFileW(PWideChar(fileName), wfd);
      if fh <> INVALID_HANDLE_VALUE then begin
        windows.FindClose(fh);
        result := UnicodeString(wfd.cfileName) + '\' + result;
      end else
        result := Copy(fileName, c1 + 1, maxInt) + '\' + result;
    end else
      result := Copy(fileName, c1 + 1, maxInt) + '\' + result;
    Delete(fileName, c1, maxInt);
  until (c1 = 0) or (fileName = '');
  if (result <> '') and (result[Length(result)] = '\') then
    Delete(result, Length(result), 1);
end;

// ***************************************************************

function GetFileVersion(const file_: AnsiString) : int64;
var len, hnd : dword;
    buf      : AnsiString;
    pfi      : PVsFixedFileInfo;
begin
  result := 0;
  len := GetFileVersionInfoSizeA(PAnsiChar(file_), hnd);
  if len <> 0 then begin
    SetLength(buf, len);
    if GetFileVersionInfoA(PAnsiChar(file_), hnd, len, pointer(buf)) and
       VerQueryValueA(pointer(buf), '\', pointer(pfi), len) then
      result := int64(pfi^.dwFileVersionMS) shl 32 + int64(pfi^.dwFileVersionLS);
  end;
end;

{$ifdef UnicodeOverloads}
function GetFileVersion(const file_: UnicodeString) : int64;
{$else}
function GetFileVersionW(const file_: UnicodeString) : int64;
{$endif}
var len, hnd : dword;
    buf      : AnsiString;
    pfi      : PVsFixedFileInfo;
begin
  result := 0;
  if GetVersion and $80000000 <> 0 then begin
    result := GetFileVersionA(AnsiString(file_));
    exit;
  end;
  len := GetFileVersionInfoSizeW(PWideChar(file_), hnd);
  if len <> 0 then begin
    SetLength(buf, len);
    if GetFileVersionInfoW(PWideChar(file_), hnd, len, pointer(buf)) and
       VerQueryValueW(pointer(buf), '\', pointer(pfi), len) then
      result := int64(pfi^.dwFileVersionMS) shl 32 + int64(pfi^.dwFileVersionLS);
  end;
end;

function FileVersionToStr(version: int64) : AnsiString;
begin
  result := IntToStrExA( version shr 48           ) + '.' +
            IntToStrExA((version shr 32) and $FFFF) + '.' +
            IntToStrExA((version shr 16) and $FFFF) + '.' +
            IntToStrExA( version         and $FFFF);
end;

function FileVersionToStrW(version: int64) : UnicodeString;
begin
  result := IntToStrExW( version shr 48           ) + '.' +
            IntToStrExW((version shr 32) and $FFFF) + '.' +
            IntToStrExW((version shr 16) and $FFFF) + '.' +
            IntToStrExW( version         and $FFFF);
end;

function GetFileVersionStr(const file_: AnsiString) : AnsiString;
var len, hnd : dword;
    buf      : AnsiString;
    pfi      : PVsFixedFileInfo;
    trans    : ^integer;
    value    : PAnsiChar;
begin
  result := '';
  len := GetFileVersionInfoSizeA(PAnsiChar(file_), hnd);
  if len <> 0 then begin
    SetLength(buf, len);
    if GetFileVersionInfoA(PAnsiChar(file_), hnd, len, pointer(buf)) then
      if VerQueryValueA(pointer(buf), '\', pointer(pfi), len) and ((pfi^.dwFileVersionMS <> 0) or (pfi^.dwFileVersionLS <> 0)) then
        result := FileVersionToStrA(int64(pfi^.dwFileVersionMS) shl 32 + int64(pfi^.dwFileVersionLS))
      else
        if VerQueryValueA(pointer(buf), '\VarFileInfo\Translation', pointer(trans), len) and (trans <> nil) and
           VerQueryValueA(Pointer(buf), PAnsiChar('\StringFileInfo\' + Copy(IntToHexExA(MakeLong(HiWord(trans^), LoWord(trans^)), 8), 2, maxInt) + '\FileVersion'), pointer(value), len) then
          result := RetTrimStrA(value);
  end;
end;

{$ifdef UnicodeOverloads}
function GetFileVersionStr(const file_: UnicodeString) : UnicodeString;
{$else}
function GetFileVersionStrW(const file_: UnicodeString) : UnicodeString;
{$endif}
var len, hnd : dword;
    buf      : UnicodeString;
    pfi      : PVsFixedFileInfo;
    trans    : ^integer;
    value    : PWideChar;
begin
  result := '';
  if GetVersion and $80000000 <> 0 then begin
    result := UnicodeString(GetFileVersionStrA(AnsiString(file_)));
    exit;
  end;
  len := GetFileVersionInfoSizeW(PWideChar(file_), hnd);
  if len <> 0 then begin
    SetLength(buf, len);
    if GetFileVersionInfoW(PWideChar(file_), hnd, len, pointer(buf)) then
      if VerQueryValueW(pointer(buf), '\', pointer(pfi), len) and ((pfi^.dwFileVersionMS <> 0) or (pfi^.dwFileVersionLS <> 0)) then
        result := UnicodeString(FileVersionToStrW(int64(pfi^.dwFileVersionMS) shl 32 + int64(pfi^.dwFileVersionLS)))
      else
        if VerQueryValueW(pointer(buf), '\VarFileInfo\Translation', pointer(trans), len) and (trans <> nil) and
           VerQueryValueW(pointer(buf), PWideChar(WideString('\StringFileInfo\' + Copy(IntToHexExW(MakeLong(HiWord(trans^), LoWord(trans^)), 8), 2, maxInt) + '\FileVersion')), pointer(value), len) then
          result := RetTrimStrW(value);
  end;
end;

// ***************************************************************

{$ifdef win64}

  function MethodToProcedure(self: TObject; methodAddr: pointer; maxParamCount: integer = 32) : pointer;
  const
    AsmArr1 : array [0..79] of byte =
      ($48, $81, $ec,      $00, $00, $00, $00,             // sub     rsp, $118
       $48, $89, $84, $24, $00, $00, $00, $00,             // mov     [rsp+$110], rax
       $48, $8b, $84, $24, $00, $00, $00, $00,             // mov     rax, [rsp+$120]    // read 1st original stack parameter
       $48, $89, $44, $24, $08,                            // mov     [rsp+8], rax       // store as 2nd new stack parameter
       $48, $8b, $84, $24, $00, $00, $00, $00,             // mov     rax, [rsp+$128]    // read 2nd original stack parameter
       $48, $89, $44, $24, $10,                            // mov     [rsp+$10], rax     // store as 3rd new stack parameter
       $48, $8b, $84, $24, $00, $00, $00, $00,             // mov     rax, [rsp+$130]    // read 3rd original stack parameter
       $48, $89, $44, $24, $18,                            // mov     [rsp+$18], rax     // store as 4th new stack parameter
       $4c, $89, $4c, $24, $20,                            // mov     [rsp+$20], r9      // store 4th original register parameter as 5th new stack parameter
       $4d, $89, $c1,                                      // mov     r9, r8             // cycle the register parameters (rcx -> rdx -> r8 -> r9)
       $49, $89, $d0,                                      // mov     r8, rdx
       $48, $89, $ca,                                      // mov     rdx, rcx
       $66, $0f, $6f, $da,                                 // movdqa  xmm3, xmm2         // cycle the register parameters (xmm0 -> xmm1 -> xmm2 -> xmm3)
       $66, $0f, $6f, $d1,                                 // movdqa  xmm2, xmm1
       $66, $0f, $6f, $c8);                                // movdqa  xmm1, xmm0
    AsmArr2 : array [0..15] of byte =
      ($48, $8b, $84, $24, $00, $00, $00, $00,             // mov     rax, [rsp+$140]
       $48, $89, $84, $24, $00, $00, $00, $00);            // mov     [rsp+$28], rax
    AsmArr3 : array [0..54] of byte =
      ($48, $8b, $84, $24, $00, $00, $00, $00,             // mov     rax, [rsp+$110]
       $48, $b9, $00, $00, $00, $00, $00, $00, $00, $00,   // mov     rcx, methodAddr
       $48, $89, $8c, $24, $00, $00, $00, $00,             // mov     [rsp+$110], rcx
       $48, $b9, $00, $00, $00, $00, $00, $00, $00, $00,   // mov     rcx, self
       $48, $89, $0c, $24,                                 // mov     [rsp], rcx         // store "self" as 1st new stack parameter
       $ff, $94, $24, $00, $00, $00, $00,                  // call    [rsp+$110]
       $48, $81, $c4, $00, $00, $00, $00,                  // add     rsp, $118
       $c3);                                               // ret

  var stackSpace : integer;
      s1, s2     : AnsiString;
      pos        : integer;
      i1         : integer;
      op         : dword;
  begin
    if maxParamCount < 4 then
      maxParamCount := 4;
    if odd(maxParamCount) then
      stackSpace := (maxParamCount + 2) * 8   // parameters + self + localVar
    else
      stackSpace := (maxParamCount + 3) * 8;  // parameters + self + localVar + alignment
    SetString(s1, PAnsiChar(@(AsmArr1[0])), 80);
    integer(pointer(@s1[ 4])^) := stackSpace;
    integer(pointer(@s1[12])^) := stackSpace -  $8;
    integer(pointer(@s1[20])^) := stackSpace +  $8;
    integer(pointer(@s1[33])^) := stackSpace + $10;
    integer(pointer(@s1[46])^) := stackSpace + $18;
    pos := Length(s1) + 1;
    SetLength(s1, Length(s1) + (maxParamCount - 4) * 16);
    SetString(s2, PAnsiChar(@(AsmArr2[0])), 16);
    for i1 := 1 to maxParamCount - 4 do begin
      integer(pointer(@s2[ 5])^) := $20 + i1 * 8 + stackSpace;
      integer(pointer(@s2[13])^) := $20 + i1 * 8;
      Move(s2[1], s1[pos], Length(s2));
      inc(pos, Length(s2));
    end;
    SetString(s2, PAnsiChar(@(AsmArr3[0])), 55);
    integer(pointer(@s2[ 5])^) := stackSpace - $8;
    pointer(pointer(@s2[11])^) := methodAddr;
    integer(pointer(@s2[23])^) := stackSpace - $8;
    pointer(pointer(@s2[29])^) := self;
    integer(pointer(@s2[44])^) := stackSpace - $8;
    integer(pointer(@s2[51])^) := stackSpace;
    s1 := s1 + s2;
    result := VirtualAlloc(nil, Length(s1), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    Move(s1[1], result^, Length(s1));
    VirtualProtect(nil, Length(s1), PAGE_EXECUTE_READ, @op);
  end;

{$else}

  function MethodToProcedure(self: TObject; methodAddr: pointer; maxParamCount: integer = 32) : pointer;
  type
    TMethodToProc = packed record
      popEax   : byte;                  // $58      pop EAX
      pushSelf : record                 //          push self
                   opcode  : byte;      // $B8
                   self    : pointer;   // self
                 end;
      pushEax  : byte;                  // $50      push EAX
      jump     : record                 //          jmp [target]
                   opcode  : byte;      // $FF
                   modRm   : byte;      // $25
                   pTarget : ^pointer;  // @target
                   target  : pointer;   //          @MethodAddr
                 end;
    end;
  var mtp : ^TMethodToProc absolute result;
      op  : dword;
  begin
    mtp := VirtualAlloc(nil, sizeOf(mtp^), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    with mtp^ do begin
      popEax          := $58;
      pushSelf.opcode := $68;
      pushSelf.self   := self;
      pushEax         := $50;
      jump.opcode     := $FF;
      jump.modRm      := $25;
      jump.pTarget    := @jump.target;
      jump.target     := methodAddr;
    end;
    VirtualProtect(nil, sizeOf(mtp^), PAGE_EXECUTE_READ, @op);
  end;

{$endif}

function MethodToProcedure(method: TMethod; maxParamCount: integer = 32) : pointer;
begin
  result := MethodToProcedure(TObject(method.data), method.code);
end;

function ProcedureToMethod(self: TObject; procAddr: pointer) : TMethod;
begin
  result.Data := self;
  result.Code := procAddr;
end;

// ***************************************************************

type
  TMsgHandlers = array of record
    message   : cardinal;
    handler   : TMsgHandler;
    handlerOO : TMsgHandlerOO;
  end;

var
  MsgHandlerMutex : THandle = 0;
  MsgHandlerWindows : array of record
    threadID  : cardinal;
    window    : HWND;
    handlers  : TMsgHandlers;
    classAtom : word;
  end;

function MsgHandlerWindowProc(window: HWND; msg: cardinal; wParam, lParam: NativeInt) : NativeInt; stdcall;
var i1 : integer;
    mh : TMsgHandlers;
begin
  mh := nil;
  if IsWindowUnicode(window) then
    result := DefWindowProcW(window, msg, wParam, lParam)
  else
    result := DefWindowProcA(window, msg, wParam, lParam);
  if (msg in [WM_QUERYENDSESSION, WM_QUIT, WM_SYSCOLORCHANGE, WM_ENDSESSION, WM_SYSTEMERROR,
              WM_WININICHANGE, WM_DEVMODECHANGE, WM_ACTIVATEAPP, WM_FONTCHANGE, WM_TIMECHANGE,
              WM_SPOOLERSTATUS, WM_COMPACTING, WM_POWER, WM_INPUTLANGCHANGEREQUEST, WM_INPUTLANGCHANGE,
              WM_USERCHANGED, WM_DISPLAYCHANGE, WM_COPYDATA]) or (msg >= WM_POWERBROADCAST) then begin
    WaitForSingleObject(MsgHandlerMutex, INFINITE);
    try
      for i1 := 0 to high(MsgHandlerWindows) do
        if MsgHandlerWindows[i1].window = window then begin
          mh := Copy(MsgHandlerWindows[i1].handlers);
          break;
        end;
    finally ReleaseMutex(MsgHandlerMutex) end;
    for i1 := 0 to high(mh) do
      if mh[i1].message = msg then
        if @mh[i1].handler <> nil then
             mh[i1].handler  (window, msg, wParam, lParam, result)
        else mh[i1].handlerOO(window, msg, wParam, lParam, result);
  end;
end;

function MsgHandlerWindow(threadID: cardinal = 0) : cardinal;
const CMadToolsMsgHandlerWindow : PAnsiChar = 'madToolsMsgHandlerWindow';
var wndClass : TWndClassA;
    mutex    : THandle;
    i1       : integer;
    s1       : AnsiString;
    atom     : word;
begin
  result := 0;
  if threadID = 0 then
    threadID := GetCurrentThreadID;
  s1 := IntToHexExA(GetCurrentThreadID) + IntToHexExA(NativeUInt(@MsgHandlerWindow));
  if MsgHandlerMutex = 0 then begin
    mutex := CreateMutexA(nil, false, nil);
    if mutex <> 0 then begin
      if @HideLeak_ <> nil then
        HideLeak_(mutex);
      WaitForSingleObject(mutex, INFINITE);
      if MsgHandlerMutex = 0 then
        MsgHandlerMutex := mutex
      else
        CloseHandle(mutex);
    end;
  end else
    WaitForSingleObject(MsgHandlerMutex, INFINITE);
  try
    for i1 := 0 to high(MsgHandlerWindows) do
      if MsgHandlerWindows[i1].threadID = threadID then begin
        result := MsgHandlerWindows[i1].window;
        break;
      end;
  finally ReleaseMutex(MsgHandlerMutex) end;
  if (result = 0) and (threadID = GetCurrentThreadID) then begin
    ZeroMemory(@wndClass, sizeOf(wndClass));
    wndClass.lpfnWndProc   := @MsgHandlerWindowProc;
    wndClass.hInstance     := GetModuleHandle(nil);
    wndClass.lpszClassName := PAnsiChar(CMadToolsMsgHandlerWindow + s1);
    atom := windows.RegisterClassA(wndClass);
    result := CreateWindowExA(WS_EX_TOOLWINDOW, PAnsiChar(atom), '', WS_POPUP, 0, 0, 0, 0, 0, 0, wndClass.hInstance, nil);
    if result <> 0 then begin
      WaitForSingleObject(MsgHandlerMutex, INFINITE);
      try
        i1 := Length(MsgHandlerWindows);
        SetLength(MsgHandlerWindows, i1 + 1);
        MsgHandlerWindows[i1].threadID  := threadID;
        MsgHandlerWindows[i1].window    := result;
        MsgHandlerWindows[i1].classAtom := atom;
      finally ReleaseMutex(MsgHandlerMutex) end;
    end;
  end;
end;

procedure AllowWindowMessage(wnd: HWND; msg: dword);
// tell User Interface Privilege Isolation that we want to receive this message
var cwmf   : function (msg, flag: dword) : bool; stdcall;
    cwmfex : function (wnd: HWND; msg, action: dword; extendedResult: pointer) : bool; stdcall;
begin
  cwmfex := GetProcAddress(GetModuleHandle(user32), 'ChangeWindowMessageFilterEx');
  if @cwmfex <> nil then
    cwmfex(wnd, msg, 1, nil)
  else begin
    cwmf := GetProcAddress(GetModuleHandle(user32), 'ChangeWindowMessageFilter');
    if @cwmf <> nil then
      cwmf(msg, 1);
  end;
end;

function AddMsgHandler_(handler: TMsgHandler; handlerOO: TMsgHandlerOO; msg, threadID: cardinal) : cardinal;
var i1, i2 : integer;
    b1     : boolean;
    window : HWND;
begin
  result := 0;
  window := MsgHandlerWindow(threadID);
  if window <> 0 then begin
    WaitForSingleObject(MsgHandlerMutex, INFINITE);
    try
      for i1 := 0 to high(MsgHandlerWindows) do
        if MsgHandlerWindows[i1].window = window then
          with MsgHandlerWindows[i1] do begin
            if msg = 0 then begin
              msg := WM_USER;
              repeat
                b1 := true;
                for i2 := 0 to high(handlers) do
                  if handlers[i2].message = msg then begin
                    b1 := false;
                    inc(msg);
                    break;
                  end;
              until b1;
            end else
              for i2 := 0 to high(handlers) do
                if (handlers[i2].message = msg) and
                   (       @handlers[i2].handler         =        @handler        ) and
                   (TMethod(handlers[i2].handlerOO).code = TMethod(handlerOO).code) and
                   (TMethod(handlers[i2].handlerOO).data = TMethod(handlerOO).data) then
                  exit;
            i2 := Length(handlers);
            SetLength(handlers, i2 + 1);
            handlers[i2].message   := msg;
            handlers[i2].handler   := handler;
            handlers[i2].handlerOO := handlerOO;
            result := msg;
            AllowWindowMessage(window, msg);
            break;
          end;
    finally ReleaseMutex(MsgHandlerMutex) end;
  end;
end;

function AddMsgHandler(handler: TMsgHandler; msg: cardinal = 0; threadID: cardinal = 0) : cardinal;
begin
  result := AddMsgHandler_(handler, nil, msg, threadID);
end;

function AddMsgHandler(handler: TMsgHandlerOO; msg: cardinal = 0; threadID: cardinal = 0) : cardinal;
begin
  result := AddMsgHandler_(nil, handler, msg, threadID);
end;

function DelMsgHandler_(handler: TMsgHandler; handlerOO: TMsgHandlerOO; msg, threadID: cardinal) : boolean;
var i1, i2 : integer;
    w1     : HWND;
    a1     : word;
begin
  result := false;
  if MsgHandlerMutex <> 0 then begin
    if threadID = 0 then
      threadID := GetCurrentThreadID;
    w1 := 0;
    a1 := 0;
    WaitForSingleObject(MsgHandlerMutex, INFINITE);
    try
      for i1 := 0 to high(MsgHandlerWindows) do
        if MsgHandlerWindows[i1].threadID = threadID then
          with MsgHandlerWindows[i1] do begin
            for i2 := high(handlers) downto 0 do
              if ( (msg = 0) or (handlers[i2].message = msg) ) and
                 (       @handlers[i2].handler         =        @handler        ) and
                 (TMethod(handlers[i2].handlerOO).code = TMethod(handlerOO).code) and
                 (TMethod(handlers[i2].handlerOO).data = TMethod(handlerOO).data) then begin
                handlers[i2] := handlers[high(handlers)];
                SetLength(handlers, high(handlers));
              end;
            if (handlers = nil) and IsWindow(window) then begin
              w1 := window;
              a1 := MsgHandlerWindows[i1].classAtom;
              MsgHandlerWindows[i1] := MsgHandlerWindows[high(MsgHandlerWindows)];
              SetLength(MsgHandlerWindows, high(MsgHandlerWindows));
            end;
            break;
          end;
    finally ReleaseMutex(MsgHandlerMutex) end;
    if w1 <> 0 then
      if threadID = GetCurrentThreadID then begin
        DestroyWindow(w1);
        UnregisterClassA(PAnsiChar(a1), GetModuleHandle(nil));
      end else
        PostMessage(w1, WM_CLOSE, 0, 0);
  end;
end;

function DelMsgHandler(handler: TMsgHandler; msg: cardinal = 0; threadID: cardinal = 0) : boolean;
begin
  result := DelMsgHandler_(handler, nil, msg, threadID);
end;

function DelMsgHandler(handler: TMsgHandlerOO; msg: cardinal = 0; threadID: cardinal = 0) : boolean;
begin
  result := DelMsgHandler_(nil, handler, msg, threadID);
end;

procedure FinalMsgHandler;
var mutex : THandle;
begin
  mutex := MsgHandlerMutex;
  MsgHandlerMutex := 0;
  if mutex <> 0 then
    CloseHandle(mutex);
end;

// ***************************************************************

function FindModule(addr: pointer; var moduleHandle: HMODULE; var moduleName: AnsiString) : boolean;
var mbi   : TMemoryBasicInformation;
    arrCh : array [0..MAX_PATH] of WideChar;
begin
  result := (VirtualQuery(addr, mbi, sizeOf(mbi)) = sizeOf(mbi)) and
            (mbi.State = MEM_COMMIT) and (mbi.AllocationBase <> nil);
  if result then begin
    if GetVersion and $80000000 = 0 then
      result := GetModuleFileNameW(HMODULE(mbi.AllocationBase), arrCh, MAX_PATH) <> 0
    else
      result := GetModuleFileNameA(HMODULE(mbi.AllocationBase), pointer(@arrCh), MAX_PATH) <> 0;
    if result then begin
      moduleHandle := HMODULE(mbi.AllocationBase);
      if GetVersion and $80000000 = 0 then
        moduleName := WideToAnsiEx(arrCh)
      else
        moduleName := PAnsiChar(@arrCh);
    end;
  end;
end;

{$ifdef UnicodeOverloads}
function FindModule(addr: pointer; var moduleHandle: HMODULE; var moduleName: UnicodeString) : boolean;
{$else}
function FindModuleW(addr: pointer; var moduleHandle: HMODULE; var moduleName: UnicodeString) : boolean;
{$endif}
var mbi   : TMemoryBasicInformation;
    arrCh : array [0..MAX_PATH] of WideChar;
begin
  result := (VirtualQuery(addr, mbi, sizeOf(mbi)) = sizeOf(mbi)) and
            (mbi.State = MEM_COMMIT) and (mbi.AllocationBase <> nil);
  if result then begin
    if GetVersion and $80000000 = 0 then
      result := GetModuleFileNameW(HMODULE(mbi.AllocationBase), arrCh, MAX_PATH) <> 0
    else
      result := GetModuleFileNameA(HMODULE(mbi.AllocationBase), pointer(@arrCh), MAX_PATH) <> 0;
    if result then begin
      moduleHandle := HMODULE(mbi.AllocationBase);
      if GetVersion and $80000000 = 0 then
        moduleName := arrCh
      else
        moduleName := UnicodeString(AnsiString(PAnsiChar(@arrCh)));
    end;
  end;
end;

function GetImageNtHeaders(module: HMODULE) : PImageNtHeaders32;
begin
  result := nil;
  try
    if (not IsBadReadPtr2(pointer(module), 2)) and (TPWord(module)^ = CEMAGIC) then begin
      result := pointer(module + dword(pointer(module + CENEWHDR)^));
      if result^.signature <> CPEMAGIC then
        result := nil;
    end;
  except result := nil end;
end;

function GetImageDataDirectory(module: HMODULE; directory: dword) : pointer;
var nh : PImageNtHeaders32;
begin
  nh := GetImageNtHeaders(module);
  if nh <> nil then begin
    if nh^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC then
         result := pointer(module + PImageOptionalHeader64(@nh^.OptionalHeader).DataDirectory[directory].VirtualAddress)
    else result := pointer(module +                         nh^.OptionalHeader .DataDirectory[directory].VirtualAddress);
  end else
    result := nil;
end;

function GetImageImportDirectory(module: HMODULE) : PImageImportDirectory;
begin
  result := GetImageDataDirectory(module, IMAGE_DIRECTORY_ENTRY_IMPORT);
end;

function GetImageExportDirectory(module: HMODULE) : PImageExportDirectory;
begin
  result := GetImageDataDirectory(module, IMAGE_DIRECTORY_ENTRY_EXPORT);
end;

function ExportToFunc(module: HMODULE; addr: dword) : pointer;
var nh       : PImageNtHeaders32;
    ed       : TImageDataDirectory;
    s1       : AnsiString;
    pc1, pc2 : PAnsiChar;
    dll      : HMODULE;
begin
  nh := GetImageNtHeaders(module);
  if nh <> nil then begin
    if nh^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC then
         ed := PImageOptionalHeader64(@nh^.OptionalHeader).DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT]
    else ed :=                         nh^.OptionalHeader .DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
    if (addr >= ed.VirtualAddress) and (addr < ed.VirtualAddress + ed.Size) then begin
      s1 := PAnsiChar(module + addr);
      pc1 := PAnsiChar(s1);
      pc2 := pc1;
      repeat
        inc(pc2);
      until pc2^ = '.';
      pc2^ := #0;
      inc(pc2);
      dll := GetModuleHandleA(pc1);
      if (dll <> 0) and (dll <> module) then
        result := GetImageProcAddress(dll, pc2)
      else
        result := nil;
    end else
      result := pointer(module + addr);
  end else
    result := nil;
end;

function VirtualToRaw(nh: PImageNtHeaders32; addr: dword) : dword;
type TAImageSectionHeader = packed array [0..maxInt shr 6] of TImageSectionHeader;
var i1 : integer;
    sh : ^TAImageSectionHeader;
begin
  result := addr;
  if nh^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC then
       sh := pointer(NativeUInt(@nh^.OptionalHeader) + sizeOf(TImageOptionalHeader64))
  else sh := pointer(NativeUInt(@nh^.OptionalHeader) + sizeOf(TImageOptionalHeader32));
  for i1 := 0 to nh^.FileHeader.NumberOfSections - 1 do
    if (addr >= sh[i1].VirtualAddress) and
       ((i1 = nh^.FileHeader.NumberOfSections - 1) or (addr < sh[i1 + 1].VirtualAddress)) then begin
      result := addr - sh[i1].VirtualAddress + sh[i1].PointerToRawData;
      break;
    end;
end;

function GetSizeOfImage(nh: PImageNtHeaders32) : dword;
begin
  if nh^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC then
       result := PImageOptionalHeader64(@nh.OptionalHeader).SizeOfImage
  else result :=                         nh.OptionalHeader .SizeOfImage;
end;

function NeedModuleFileMap(module: HMODULE) : pointer;
var arrCh : pointer;
    fh    : THandle;
    map   : THandle;
begin
  result := nil;
  if not IsWine then begin
    if GetVersion and $80000000 = 0 then begin
      arrCh := pointer(LocalAlloc(LPTR, 32 * 1024 * 2));
      GetModuleFileNameW(module, arrCh, 32 * 1024);
      fh := CreateFileW(arrCh, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    end else begin
      arrCh := pointer(LocalAlloc(LPTR, MAX_PATH + 1));
      GetModuleFileNameA(module, arrCh, MAX_PATH);
      fh := CreateFileA(arrCh, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    end;
    LocalFree(HLOCAL(arrCh));
    if fh <> INVALID_HANDLE_VALUE then begin
      if GetVersion and $80000000 = 0 then
           map := CreateFileMappingW(fh, nil, PAGE_READONLY, 0, 0, nil)
      else map := CreateFileMappingA(fh, nil, PAGE_READONLY, 0, 0, nil);
      if map <> 0 then begin
        result := MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);
        CloseHandle(map);
      end;
      CloseHandle(fh);
    end;
  end;
end;

function GetImageProcAddress(module: HMODULE; const name: AnsiString; doubleCheck: boolean = false) : pointer;
var ed             : PImageExportDirectory;
    nh, nh2        : PImageNtHeaders32;
    i1             : integer;
    c1, c2, c3, c4 : dword;
    w1             : word;
    va, ra         : dword;
    buf            : pointer;
    p1             : pointer;
    freeBuf        : boolean;
    soi            : dword;
begin
  result := nil;
  if module <> 0 then begin
    nh := GetImageNtHeaders(module);
    if nh <> nil then begin
      soi := GetSizeOfImage(nh);
      if nh^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC then begin
        va := PImageOptionalHeader64(@nh^.OptionalHeader).DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
        ra := PImageOptionalHeader64(@nh^.OptionalHeader).DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress;
      end else begin
        va :=                         nh^.OptionalHeader .DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
        ra :=                         nh^.OptionalHeader .DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress;
      end;
      ed := pointer(module + va);
      if ed <> nil then
        for i1 := 0 to ed^.NumberOfNames - 1 do
          if lstrcmpA(pointer(module + TPACardinal(module + ed^.AddressOfNames)^[i1]), pointer(name)) = 0 then begin
            w1 := TPAWord(module + ed^.AddressOfNameOrdinals)^[i1];
            c1 := TPACardinal(module + ed^.AddressOfFunctions)^[w1];
            if doubleCheck or (c1 > soi) then begin
              NativeUInt(p1) := module + c1;
              if (@CheckProcAddress <> nil) and CheckProcAddress(p1) then begin
                result := p1;
                break;
              end;
              if @NeedModuleFileMapEx <> nil then
                buf := NeedModuleFileMapEx(module)
              else
                buf := nil;
              freeBuf := buf = nil;
              if buf = nil then
                buf := NeedModuleFileMap(module);
              if buf <> nil then begin
                try
                  nh2 := GetImageNtHeaders(HMODULE(buf));
                  if nh2 <> nil then begin
                    c2 := ed^.AddressOfNames;
                    c3 := ed^.AddressOfFunctions;
                    if nh2^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC then
                      c4 := PImageOptionalHeader64(@nh2^.OptionalHeader).DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress
                    else
                      c4 :=                         nh2^.OptionalHeader .DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress;
                    NativeUInt(ed) := NativeUInt(buf) + VirtualToRaw(nh, va);
                    if (c2 = ed^.AddressOfNames) and (c3 = ed^.AddressOfFunctions) and (c4 = ra) and (soi = GetSizeOfImage(nh2)) then
                      c1 := TPACardinal(NativeUInt(buf) + VirtualToRaw(nh, ed^.AddressOfFunctions))^[w1];
                  end;
                except end;
                if freeBuf then
                  UnmapViewOfFile(buf);
              end;
            end;
            result := ExportToFunc(module, c1);
            break;
          end;
    end;
    if (result = nil) and (module <> 0) then begin
//      nh := GetImageNtHeaders(module);
//      if (nh <> nil) and (nh^.OptionalHeader.Magic = {$ifdef win64} IMAGE_NT_OPTIONAL_HDR64_MAGIC {$else} IMAGE_NT_OPTIONAL_HDR32_MAGIC {$endif}) then
        result := GetProcAddress(module, PAnsiChar(name))
    end;
  end;
end;

function Is64bitModule(fileName: PWideChar) : bool; stdcall;
// find out and return whether the dll file is a 64bit dll
var fh, map : THandle;
    buf     : pointer;
    nh      : PImageNtHeaders32;
begin
  result := false;
  if GetVersion and $80000000 = 0 then
    fh := CreateFileW(fileName, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0)
  else
    fh := CreateFileA(PAnsiChar(AnsiString(UnicodeString(fileName))), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if fh <> INVALID_HANDLE_VALUE then begin
    map := CreateFileMapping(fh, nil, PAGE_READONLY, 0, 0, nil);
    if map <> 0 then begin
      buf := MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);
      if buf <> nil then begin
        nh := GetImageNtHeaders(HMODULE(buf));
        result := (nh <> nil) and (nh.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC);
        UnmapViewOfFile(buf);
      end;
      CloseHandle(map);
    end;
    CloseHandle(fh);
  end;
end;

function DoesModuleExportOrdinal1(fileName: PWideChar) : boolean; stdcall;
// does the DLL export an API under the ordinal of 1?
var fh, map : THandle;
    buf     : pointer;
    nh      : PImageNtHeaders32;
    va      : dword;
    ed      : PImageExportDirectory;
begin
  result := false;
  fh := CreateFileW(fileName, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if fh <> INVALID_HANDLE_VALUE then begin
    map := CreateFileMapping(fh, nil, PAGE_READONLY, 0, 0, nil);
    if map <> 0 then begin
      buf := MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);
      if buf <> nil then begin
        nh := GetImageNtHeaders(HMODULE(buf));
        if nh <> nil then begin
          if nh.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC then
            va := PImageOptionalHeader64(@nh.OptionalHeader).DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress
          else
            va :=                         nh.OptionalHeader .DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
          if va <> 0 then begin
            ed := pointer(NativeUInt(buf) + VirtualToRaw(nh, va));
            result := ((ed.Base = 0) and (ed.NumberOfFunctions > 1)) or
                      ((ed.Base = 1) and (ed.NumberOfFunctions > 0));
          end;
        end;
        UnmapViewOfFile(buf);
      end;
      CloseHandle(map);
    end;
    CloseHandle(fh);
  end;
end;

function GetImageProcAddress(module: HMODULE; index: integer) : pointer; overload;
var oi : integer;
//    nh : PImageNtHeaders32;
    ed : PImageExportDirectory;
    c1 : dword;
begin
  result := nil;
  oi := index;
  ed := GetImageExportDirectory(module);
  if ed <> nil then
    with ed^ do begin
      dec(index, Base);
      if (index >= 0) and (index < NumberOfFunctions) then begin
        c1 := TPACardinal(module + AddressOfFunctions)^[index];
        if c1 > 0 then
          result := ExportToFunc(module, c1);
      end;
    end;
  if (result = nil) and (module <> 0) then begin
//    nh := GetImageNtHeaders(module);
//    if (nh <> nil) and (nh^.OptionalHeader.Magic = {$ifdef win64} IMAGE_NT_OPTIONAL_HDR64_MAGIC {$else} IMAGE_NT_OPTIONAL_HDR32_MAGIC {$endif}) then
      result := GetProcAddress(module, PAnsiChar(oi));
  end;
end;

function GetImageProcName(module: HMODULE; proc: pointer; unmangle: boolean) : AnsiString;
var ed     : PImageExportDirectory;
    i1, i2 : integer;
    as1    : AnsiString;
begin
  if GetVersion and $80000000 <> 0 then begin
    // no idea why we have to do this, but stability suffers in win9x otherwise
    result := AnsiString(GetImageProcNameW(module, proc, unmangle));
    exit;
  end;
  result := '';
  ed := GetImageExportDirectory(module);
  if ed <> nil then
    with ed^ do
      for i1 := 0 to NumberOfFunctions - 1 do
        if module + TPACardinal(module + AddressOfFunctions)^[i1] = NativeUInt(proc) then begin
          for i2 := 0 to NumberOfNames - 1 do
            if TPAWord(module + AddressOfNameOrdinals)^[i2] = i1 then begin
              result := PAnsiChar(module + TPACardinal(module + AddressOfNames)^[i2]);
              break;
            end;
          if result = '' then
            result := '#' + IntToStrExA(Base + dword(i1));
          break;
        end;
  if unmangle and madTools.UnmangleA(result, as1) then
    result := as1 + '.' + result;
end;

function GetImageProcNameW(module: HMODULE; proc: pointer; unmangle: boolean) : UnicodeString;
var ed     : PImageExportDirectory;
    i1, i2 : integer;
    us1    : UnicodeString;
begin
  result := '';
  ed := GetImageExportDirectory(module);
  if ed <> nil then
    with ed^ do
      for i1 := 0 to NumberOfFunctions - 1 do
        if module + TPACardinal(module + AddressOfFunctions)^[i1] = NativeUInt(proc) then begin
          for i2 := 0 to NumberOfNames - 1 do
            if TPAWord(module + AddressOfNameOrdinals)^[i2] = i1 then begin
              result := UnicodeString(AnsiString(PAnsiChar(module + TPACardinal(module + AddressOfNames)^[i2])));
              break;
            end;
          if result = '' then
            result := '#' + IntToStrExW(Base + dword(i1));
          break;
        end;
  if unmangle and madTools.UnmangleW(result, us1) then
    result := us1 + '.' + result;
end;

function ResToStr(module: HMODULE; resType: PWideChar; const resName: UnicodeString) : AnsiString;
var res : HRSRC;
    mem : HGLOBAL;
begin
  result := '';
  if GetVersion and $80000000 = 0 then
    res := FindResourceW(module, PWideChar(resName), resType)
  else
    if PWideChar(byte(resType)) = resType then
      res := FindResourceA(module, PAnsiChar(AnsiString(resName)), pointer(resType))
    else
      res := FindResourceA(module, PAnsiChar(AnsiString(resName)), PAnsiChar(AnsiString(UnicodeString(resType))));
  if res <> 0 then begin
    mem := LoadResource(module, res);
    if mem <> 0 then begin
      SetString(result, PAnsiChar(LockResource(mem)), SizeOfResource(module, res));
      UnlockResource(mem);
      FreeResource(mem);
    end;
  end;
end;

function UnmangleA(var publicName, unitName: AnsiString) : boolean;
var i1, i2 : integer;
    b1, b2 : boolean;
    {$ifdef win64}{$ifdef xe3}
      len    : integer;
    {$endif}{$endif}
    s2     : AnsiString;
begin
  result := false;
  unitName := '';
  ReplaceStrA(publicName, '::', '.');
  if (publicName <> '') and (publicName[1] = '@') then begin
    // might be a mangled bpl export, so let's try to unmangle it
    s2 := '';
    if (Length(publicName) > 1) and (publicName[2] = '%') then begin
      for i1 := Length(publicName) - 1 downto 3 do
        if (publicName[i1] = '%') and (publicName[i1 + 1] = '@') then begin
          s2 := '.' + Copy(publicName, i1 + 2, maxInt);
          break;
        end;
    end;
    i2 := 3;
    if (Length(publicName) > 6) and (publicName[2] = '_') and (publicName[3] = '$') then
      for i1 := 5 to Length(publicName) - 1 do
        if publicName[i1] = '$' then begin
          if publicName[i1 + 1] = '@' then
            i2 := i1 + 1;
          break;
        end;
    b1 := false;
    b2 := false;
    for i1 := i2 to Length(publicName) do
      case publicName[i1] of
        '$' : begin
                b1 := (Length(publicName) > i1 + 1) and (publicName[i1 + 2] = 'd');
                Delete(publicName, i1, maxInt);
                publicName := publicName + s2;
                b2 := true;
                break;
              end;
        '@' : begin
                publicName[i1] := '.';
                if unitName = '' then
                  unitName := Copy(publicName, 2, i1 - 2)
              end;
      end;
    if unitName <> '' then
      if length(unitName) + 2 < length(publicName) then begin
        Delete(publicName, 1, Length(unitName) + 2);
        b2 := false;
      end else
        unitName := '';
    if b2 then
      Delete(publicName, 1, 1);
    if publicName <> '' then
      if      publicName[1] = '%' then Delete(publicName, 1, 1)
      else if publicName[1] = '.' then publicName[1] := '@';
    if (publicName <> '') and (publicName[Length(publicName)] = '.') then
      if b1 then
           publicName := publicName + 'Destroy'
      else publicName := publicName + 'Create';
    TrimStrA(publicName);
    if (publicName <> '') and (publicName[Length(publicName)] = '.') then
      publicName := publicName + '?';
    result := true;
  end {$ifdef win64}{$ifdef xe3} else
    if PosStrIs1A('_ZN', publicName) then begin
      s2 := '';
      i1 := 4;
      while (i1 < Length(publicName)) and (publicName[i1] in ['0'..'9']) do begin
        i2 := i1 + 1;
        while publicName[i2] in ['0'..'9'] do
          inc(i2);
        len := StrToIntExA(false, @publicName[i1], i2 - i1);
        s2 := s2 + '.' + Copy(publicName, i2, len);
        i1 := i2 + len;
      end;
      if (i1 <= Length(publicName)) and (publicName[i1] = 'E') then begin
        Delete(s2, 1, 1);
        if PosStrA('.', s2) > 0 then begin
          unitName := SubStrA(s2, 1, '.');
          publicName := Copy(s2, Length(unitName) + 2, maxInt);
        end else
          publicName := s2;
        result := true;
      end;
    end; {$endif}{$endif}
end;

function UnmangleW(var publicName, unitName: UnicodeString) : boolean;
var i1, i2 : integer;
    b1, b2 : boolean;
    {$ifdef win64}{$ifdef xe3}
      len    : integer;
    {$endif}{$endif}
    s2     : UnicodeString;
begin
  result := false;
  unitName := '';
  ReplaceStrW(publicName, '::', '.');
  if (publicName <> '') and (publicName[1] = '@') then begin
    // might be a mangled bpl export, so let's try to unmangle it
    s2 := '';
    if (Length(publicName) > 1) and (publicName[2] = '%') then begin
      for i1 := Length(publicName) - 1 downto 3 do
        if (publicName[i1] = '%') and (publicName[i1 + 1] = '@') then begin
          s2 := '.' + Copy(publicName, i1 + 2, maxInt);
          break;
        end;
    end;
    i2 := 3;
    if (Length(publicName) > 6) and (publicName[2] = '_') and (publicName[3] = '$') then
      for i1 := 5 to Length(publicName) - 1 do
        if publicName[i1] = '$' then begin
          if publicName[i1 + 1] = '@' then
            i2 := i1 + 1;
          break;
        end;
    b1 := false;
    b2 := false;
    for i1 := i2 to Length(publicName) do
      case publicName[i1] of
        '$' : begin
                b1 := (Length(publicName) > i1 + 1) and (publicName[i1 + 2] = 'd');
                Delete(publicName, i1, maxInt);
                publicName := publicName + s2;
                b2 := true;
                break;
              end;
        '@' : begin
                publicName[i1] := '.';
                if unitName = '' then
                  unitName := Copy(publicName, 2, i1 - 2)
              end;
      end;
    if unitName <> '' then
      if length(unitName) + 2 < length(publicName) then begin
        Delete(publicName, 1, Length(unitName) + 2);
        b2 := false;
      end else
        unitName := '';
    if b2 then
      Delete(publicName, 1, 1);
    if publicName <> '' then
      if      publicName[1] = '%' then Delete(publicName, 1, 1)
      else if publicName[1] = '.' then publicName[1] := '@';
    if (publicName <> '') and (publicName[Length(publicName)] = '.') then
      if b1 then
           publicName := publicName + 'Destroy'
      else publicName := publicName + 'Create';
    TrimStrW(publicName);
    if (publicName <> '') and (publicName[Length(publicName)] = '.') then
      publicName := publicName + '?';
    result := true;
  end {$ifdef win64}{$ifdef xe3} else
    if PosStrIs1W('_ZN', publicName) then begin
      s2 := '';
      i1 := 4;
      while (i1 < Length(publicName)) and (WideChar(AnsiChar(publicName[i1])) = publicName[i1]) and (AnsiChar(publicName[i1]) in ['0'..'9']) do begin
        i2 := i1 + 1;
        while (WideChar(AnsiChar(publicName[i2])) = publicName[i2]) and (AnsiChar(publicName[i2]) in ['0'..'9']) do
          inc(i2);
        len := StrToIntExW(false, @publicName[i1], i2 - i1);
        s2 := s2 + '.' + Copy(publicName, i2, len);
        i1 := i2 + len;
      end;
      if (i1 <= Length(publicName)) and (publicName[i1] = 'E') then begin
        Delete(s2, 1, 1);
        if PosStrW('.', s2) > 0 then begin
          unitName := SubStrW(s2, 1, '.');
          publicName := Copy(s2, Length(unitName) + 2, maxInt);
        end else
          publicName := s2;
        result := true;
      end;
    end; {$endif}{$endif}
end;

// ***************************************************************

function IsBadReadPtr2(src: pointer; count: dword) : boolean;
var mbi : TMemoryBasicInformation;
begin
  result := (VirtualQuery(src, mbi, sizeOf(mbi)) <> sizeOf(mbi)) or (mbi.State <> MEM_COMMIT) or (mbi.Protect and (PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_READONLY or PAGE_READWRITE or PAGE_WRITECOPY) = 0) or (mbi.Protect and PAGE_GUARD <> 0) or
            (NativeUInt(src) + count > NativeUInt(mbi.BaseAddress) + mbi.RegionSize);
end;

function IsBadWritePtr2(dst: pointer; count: dword) : boolean;
var mbi : TMemoryBasicInformation;
begin
  result := (VirtualQuery(dst, mbi, sizeOf(mbi)) <> sizeOf(mbi)) or (mbi.State <> MEM_COMMIT) or (mbi.Protect and (PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY or PAGE_READWRITE or PAGE_WRITECOPY) = 0) or (mbi.Protect and PAGE_GUARD <> 0) or
            (NativeUInt(dst) + count > NativeUInt(mbi.BaseAddress) + mbi.RegionSize);
end;

// ***************************************************************

function TickDif(tick: dword) : dword;
var dw : dword;
begin
  dw := GetTickCount;
  if dw >= tick then
       result := dw - tick
  else result := high(dword) - tick + dw;
end;

// ***************************************************************

function GetExceptionObject(er: pointer) : MadException;
begin
  result := MadException.Create('Unknown exception. If you want to know more, you have to add SysUtils to your project.');
end;

procedure InitTryExceptFinally;
begin
  if ExceptionClass = nil then begin
    ExceptionClass := MadException;
    ExceptObjProc := @GetExceptionObject;
  end;
end;

// ***************************************************************

initialization
finalization
  FinalMsgHandler;
end.