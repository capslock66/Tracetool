//==============================================================================
// Modul - Name   : DirMon
// Version        : 1.0
// Description    : Directory Changes Monitoring Component.
//                  The component notifies by changes in a diretory such as:
//                  File/Directory: Create/Delete/Modify/Rename
//                  We have 4 Events:
//                    OnCreated: TFileChangedEvent
//                    OnDeleted: TFileChangedEvent
//                    OnModified: TFileChangedEvent
//                    OnRenamed: TFileRenamedEvent
//                  We are using here only the Idea's from Damien Thouvenin - but it is
//                  is easyer for us to use a Component instead the source of his Demo.
//                  if you find it good so send your thanks also to damien@thouvenin.net
//                  but your BUGS you can sent to support@phaeteon.de
//------------------------------------------------------------------------------
// Compatible: Delphi 4 and Delphi 5
//------------------------------------------------------------------------------
// Tested Platforms : Windows NT 4.0, Windows 2000 Prof/Server
//                    not Compatible to Windows'98
//------------------------------------------------------------------------------
// Histoy:
// When        Who   What
// 06.11.2000  UR    Created by phaeteon(www.phaeteon.de) and LoLa (www.lolasoft.de)
//
//==============================================================================


// This is the "readme" from Damien Thouvenin - great thanks for this routines!
// His original files are in "DirMon.zip" you can found at torry or DSP
{ ========================== DIR EVENTS MONITORING =============================

  == Overview ==

  This form demonstrates the use of ReadDirectoryChangesW API function

  The function can be used either in synchronous or asynchronous mode.
  Here I implemented the asynchronous mode (ie: function returns immediatly
  and you have to watch for events). There are 3 ways to get results:
  a. call the function with a callback proc that the system calls when an event occurs
  b. associate the directory with an IOCompletionPort that "traps" IO events (that's what I did)
  c. create an Event, wait for it and call GetOverlapped Result

  For more information on synchronous calls or on the 2 other asynchronous implementations refer to MSDN

  == Implementation notes ==

  I assume anyone willing to use this code will have sufficient knowledge of
  basic API Calls so I won't comment on threads, API Structures etc...

  I implemented a very basic call to SHBrowseForFolder. If you're interested
  refer to MSDN or download Brad Stower's components at www.delphifreestuff.com

  OK, Now we get to the bottom of things.
  Like much of the APIs, Monitoring is quite simple once you know how to get it to work !

  First you have to open the directory you want to monitor. Use CreateFile in
  FILE_LIST_DIRECTORY mode and with FILE_FLAG_BACKUP_SEMANTICS privilege.
  Note that you have to add FILE_FLAG_OVERLAPPED for asynchronous operations.

  Then create an IOCompletionPort with the directory handle.
  If you open multiple directories, you can reuse the same port, simply
  specify a different cookie for each dir.

  Third Call ReadDirectoryChangesW with an empty Overlapped struct and no
  callback proc (asynchronous b method, see overview)

  Then wait for events using GetQueuedCompletionStatus. Upon event fire ReadDirectoryChangesW
  again and loop.

  Here you have mulmtiple implementation choices. Either you give a TimeOut to GetQueuedCompletionStatus
  and check whether it returned sth or (what I did) you call it in a thread with INFINITE wait time
  In this alternative, post an empty completion status to stop the thread; see PostQueuedCompletionStatus
  call in bStopClick method

  When you are finished, release all dir handles and IOCompletionPort with CloseHandle

  Events are written as continous TFileNotifyInformation records in a buffer you provide.

  >>Important Note<<
  FBytesWritten is not updated by asynchronous calls to ReadDirectoryChangesW
  Thus, don't rely on it for buffer parsing. Rather use the NextEntryOffset which
  is set to 0 for the last record.

  == Release Notes ==

  This code has been tested with delphi 3.02 running on Windows NT4 SP6
  It should work on all Windows NT platforms, though I haven't tested it under
  Windows NT 3.51 or Windows 2000.

  I don't known whether it works under Win9x or not.
  Eventually, it may be kind of you to let me know if you run some tests

  You shouldn't have much trouble compiling it with Delphi 2/4/5+ and C++ port is quite easy

  Damien Thouvenin  (mailto:damien@thouvenin.net)
}


unit DirMon;

//==============================================================================
interface
//==============================================================================

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  // Exception's
  EDirMonError = class(Exception);

  // Changes in Files/Directories
  TFileChangedEvent = procedure( Sender: TObject; FileName: String) of Object;
  // Files/Directory - Renamed
  TFileRenamedEvent = procedure( Sender: TObject; fromFileName: String; toFileName: String) of Object;

  // watch filters
  TWatchFilter = (nfFILE_NAME,
                  nfDIR_NAME,
                  nfATTRIBUTES,
                  nfSIZE,
                  nfLAST_WRITE,
                  nfLAST_ACCESS,
                  nfCREATION,
                  nfSECURITY);
  TWatchFilters = set of TWatchFilter;

  // The Directory Monitor
  TDirMon = class(TComponent)
  private
    { Private-Deklarationen }
    FDirectoryHandle: THandle;
    FNotificationBuffer: array[0..4096] of Byte;
    FWatchThread: TThread;
    FWatchFilters: TWatchFilters;
    FNotifyFilter: DWord;
    FOverlapped: TOverlapped;
    FPOverlapped: POverlapped;
    FBytesWritten: DWORD;
    FCompletionPort: THandle;
    FPath: String;
    FActive: Boolean;
    FOnCreated: TFileChangedEvent;
    FOnDeleted: TFileChangedEvent;
    FOnModified: TFileChangedEvent;
    FOnRenamed: TFileRenamedEvent;
    FWatchSubTree: Boolean;

    procedure SetActive( AActive: Boolean);
    procedure SetPath(aPath: String);
    procedure cmdCreated( Sender: TObject; FileName: String);
    procedure cmdDeleted( Sender: TObject; FileName: String);
    procedure cmdModified( Sender: TObject; FileName: String);
    procedure cmdRenamed( Sender: TObject; fromFileName: String; toFileName: String);
  protected
    procedure Start ;
    procedure Stop;
  public
    error : string ;
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }

    property Active: Boolean read FActive write SetActive;
    property Path: String read FPath write SetPath;
    property OnCreated: TFileChangedEvent read FOnCreated write FOnCreated;
    property OnDeleted: TFileChangedEvent read FOnDeleted write FOnDeleted;
    property OnModified: TFileChangedEvent read FOnModified write FOnModified;
    property OnRenamed: TFileRenamedEvent read FOnRenamed write FOnRenamed;
    property WatchSubtree: Boolean read FWatchSubTree write FWatchSubtree;
    property WatchFilters: TWatchfilters read FWatchFilters write FWatchFilters;

  end;

procedure Register;

//==============================================================================
implementation
//==============================================================================

uses
  ShlObj, ActiveX; //, FileCtrl;

type
  // see windows API help
  PFileNotifyInformation = ^TFileNotifyInformation;
  TFileNotifyInformation = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array[0..0] of WideChar;
  end;

const
  FILE_LIST_DIRECTORY   = $0001;

type
  TWaitThread = class(TThread)
  private
    FParent: TDirMon;
    FRenamedFrom: String;
    procedure HandleEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AParent: TDirMon);
  end;

//------------------------------------------------------------------------------
// TWatchThread
//------------------------------------------------------------------------------
constructor TWaitThread.Create(AParent: TDirMon);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FParent := AParent;
end;

procedure TWaitThread.HandleEvent;
var
  FileOpNotification: PFileNotifyInformation;
  Offset: Longint;
begin
  with FParent do
  begin
    Pointer(FileOpNotification) := @FNotificationBuffer[0];
    repeat
      Offset := FileOpNotification^.NextEntryOffset;
      Case FileOpNotification^.Action of
        1: cmdCreated( FParent, WideCharToString(@(FileOpNotification^.FileName)));
        2: cmdDeleted( FParent, WideCharToString(@(FileOpNotification^.FileName)));
        3: cmdModified( FParent, WideCharToString(@(FileOpNotification^.FileName)));
        4: FRenamedFrom := WideCharToString(@(FileOpNotification^.FileName)); // Ausnahme
        5: cmdRenamed( FParent, FRenamedFrom, WideCharToString(@(FileOpNotification^.FileName)));
      end;
      PChar(FileOpNotification) := PChar(FileOpNotification)+Offset;
    until Offset=0;
  end;
end;

procedure TWaitThread.Execute;
var
  numBytes: DWORD;
  CompletionKey: ULONG_PTR;
begin
  while not Terminated do
  begin

    GetQueuedCompletionStatus(    //function GetQueuedCompletionStatus(
       FParent.FCompletionPort,   //   CompletionPort: THandle;
       numBytes,                  //   var lpNumberOfBytesTransferred: DWORD;
       CompletionKey,             //   var lpCompletionKey: ULONG_PTR;
       FParent.FPOverlapped,      //   var lpOverlapped: POverlapped;
       INFINITE);                 //   dwMilliseconds: DWORD): BOOL; stdcall;

    if CompletionKey <> 0 then
    begin
      Synchronize(HandleEvent);
      with FParent do
      begin
        FBytesWritten := 0;
        ZeroMemory(@FNotificationBuffer, SizeOf(FNotificationBuffer));
        ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer, SizeOf(FNotificationBuffer), FParent.WatchSubtree , FNotifyFilter, @FBytesWritten, @FOverlapped, nil);
      end;
    end
    else
      Terminate;
  end;
end;

//------------------------------------------------------------------------------
// TDirMon
//------------------------------------------------------------------------------
constructor TDirMon.Create( AOwner: TComponent);
begin
  inherited;
  FCompletionPort := 0;
  FDirectoryHandle := 0;
  FPOverlapped := @FOverlapped;
  ZeroMemory(@FOverlapped, SizeOf(FOverlapped));
  FWatchFilters:=[nfFILE_NAME,nfDIR_NAME,nfLAST_WRITE,nfCREATION];
end;

destructor TDirMon.destroy;
begin
  if FActive then
     Stop;
  inherited;
end;

procedure TDirMon.SetActive( AActive: Boolean);
begin
  if csDesigning in ComponentState then
     Exit;  // Don't start it in DesignerMode
  If AActive Then
    Start
  else
    Stop;
end;

procedure TDirMon.Start ;
begin
  error := '' ;  // no error
  if FActive then
     Exit; // Don't start it again
  FNotifyFilter := 0;   // Set MyFilterArray->DWord-Filter in ReadDirectoryChanges
  if (nfFILE_NAME in FWatchFilters)
     then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_FILE_NAME;
  if (nfDIR_NAME in FWatchFilters)
     then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_DIR_NAME;
  if (nfATTRIBUTES in FWatchFilters)
     then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if (nfSIZE in FWatchFilters)
     then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_SIZE;
  if (nfLAST_WRITE in FWatchFilters)
     then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if (nfLAST_ACCESS in FWatchFilters)
     then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if (nfCREATION in FWatchFilters)
     then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_CREATION;
  if (nfSECURITY in FWatchFilters)
     then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_SECURITY;
  if FNotifyFilter = 0 then
     exit;
  FDirectoryHandle := CreateFile(PChar(FPath),
     FILE_LIST_DIRECTORY,
     FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
     nil,
     OPEN_EXISTING,
     FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,
     0);
  if FDirectoryHandle = INVALID_HANDLE_VALUE then
  begin
     FDirectoryHandle := 0;
     //raise EDirMonError.Create(SysErrorMessage(GetLastError));
     error := SysErrorMessage(GetLastError) ;
     exit;
  end;
  FCompletionPort := CreateIoCompletionPort(FDirectoryHandle, 0, Longint(pointer(self)), 0);
  ZeroMemory(@FNotificationBuffer, SizeOf(FNotificationBuffer));
  FBytesWritten := 0;
  if not ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer, SizeOf(FNotificationBuffer), FWatchSubTree, FNotifyFilter, @FBytesWritten, @FOverlapped, nil) then
  begin
     CloseHandle(FDirectoryHandle);
     FDirectoryHandle := 0;
     CloseHandle(FCompletionPort);
     FCompletionPort := 0;
     //raise EDirMonError.Create(SysErrorMessage(GetLastError));
     error := SysErrorMessage(GetLastError) ;
     exit;
  end;
  FWatchThread := TWaitThread.Create(self); // The Thread is the Monitorig Thred
  TWaitThread(FWatchThread).Start;
  FActive := True;
end;

procedure TDirMon.Stop;
begin
  if not FActive then Exit;
  if FCompletionPort = 0 then
    exit;
  PostQueuedCompletionStatus(FCompletionPort, 0, 0, nil);
  FWatchThread.WaitFor;
  FWatchThread.Free;
  CloseHandle(FDirectoryHandle);
  FDirectoryHandle := 0;
  CloseHandle(FCompletionPort);
  FCompletionPort := 0;
  FActive := False;
end;

procedure TDirMon.cmdCreated( Sender: TObject; FileName: String);
begin
  if Assigned(FOnCreated) then FOnCreated(Sender,FileName);
end;

procedure TDirMon.cmdDeleted( Sender: TObject; FileName: String);
begin
  if Assigned(FOnDeleted) then FOnDeleted(Sender,FileName);
end;

procedure TDirMon.cmdModified( Sender: TObject; FileName: String);
begin
  if Assigned(FOnModified) then FOnModified(Sender,FileName);
end;

procedure TDirMon.cmdRenamed( Sender: TObject; fromFileName: String; toFileName: String);
begin
  if Assigned(FOnRenamed) then FOnRenamed(Sender,fromFileName,toFileName);
end;

procedure TDirMon.SetPath(aPath: String);
{$IFNDEF VER130}
function IncludeTrailingBackslash(const S: string): string;
begin
  if S[length(S)]='\' then result:=S else result:=S+'\';
end;
{$ENDIF}
begin
  if DirectoryExists(aPath) then
    FPath:=IncludeTrailingBackslash(aPath);
  if FActive then // When You do this at RunTime - We stop and start the Monitoring Process
  begin
    Stop;
    start;
  end;
end;

//------------------------------------------------------------------------------
// registering the component TDirMon
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('Win32', [TDirMon]);
end;

end.
