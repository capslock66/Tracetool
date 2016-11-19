{

  Give eventLog entries and send event when new event arrive
  ===========================================================

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information   

}
unit Eventlog;

interface

uses
   Windows, SysUtils, Classes, ConTnrs, forms, Messages;

const

   EVENTLOG_SEQUENTIAL_READ        = $0001;
   EVENTLOG_SEEK_READ              = $0002;
   EVENTLOG_FORWARDS_READ          = $0004;
   EVENTLOG_BACKWARDS_READ         = $0008;

   WM_EVENTLOGCHANGED = WM_USER + 1 ;

type
   TEventLog = class ;

   TEventLogRecord = packed record
      Length : DWORD;               // Length of full record
      Reserved : DWORD;             // Used by the service                      none
      RecordNumber : DWORD;         // Absolute record number                   GetRecordNumber
      TimeGenerated : DWORD;        // Seconds since 1-1-1970                   GetEventTime
      TimeWritten : DWORD;          // Seconds since 1-1-1970                   none
      EventID : DWORD;              //                                          GetEventID
      EventType : WORD;             //                                          GetEventType
      NumStrings : WORD;            //                                          GetEventStringCount
      EventCategory : WORD;         //                                          GetEventCategory
      ReservedFlags : WORD;         // For use with paired events (auditing)    none
      ClosingRecordNumber : DWORD;  // For use with paired events (auditing)    none
      StringOffset : DWORD;         // Offset from beginning of record          none
      UserSidLength : DWORD;        //                                          GetEventSID
      UserSidOffset : DWORD;        //                                          GetEventSID
      DataLength : DWORD;           //                                          GetEventData
      DataOffset : DWORD;           // Offset from beginning of record          GetEventData
      //
      // Then follow:
      //
      // WCHAR SourceName[]
      // WCHAR Computername[]
      // SID   UserSid
      // WCHAR Strings[]
      // BYTE  Data[]
      // CHAR  Pad[]
      // DWORD Length;
      //
   end;
   PEventLogRecord = ^TEventLogRecord;

   //--------------------------------------------------------------------------------------------------

   // the thread that send to the server
   TEventLogThread = class(TThread)
   private
      hEventHandle : THandle;       // the handle to the "file"
      hComponentHandle : THandle ;  // component handle (where to end message)
      hChangedEvent : THandle ;     // signal that new event is available.
   public
      hCloseEvent : THandle ;       // Handle to close the thread.
   protected
      constructor create (fFileHandle : THandle; comp : THandle);
      // master thread routine
      procedure Execute; override;
   end ;

  //--------------------------------------------------------------------------------------------------

  TEventLogNotify = procedure(Sender: TEventLog) of object;

  TEventLog = class(TComponent)
  private
    fFileHandle : THandle;
    fLog : string;
    fServer : string;
    fSource : string;
    fOpen : boolean;

    fCurrentRecord : pointer;         // the pointer to the buffer that receive event log trace
    fCurrentRecordLen : DWORD;
    fMessagePath : string ;

    FOnChangeEventLog : TEventLogNotify;
    fWindowHandle : HWND ;

    dllModule : THandle;
    lastDLLName : string;

    fComputerName : string;

    procedure WndProc (var msg : TMessage) ;
    procedure SetServer (const value : string);
    procedure SetSource (const value : string);
    procedure SetLog (const value : string);
    function GetEventCount : Integer;

    function GetEventSource : string;
    function GetEventComputer : string;
    function GetEventID : DWORD;
    function GetEventStringCount : DWORD;
    function GetEventSID : PSID;
    function GetEventString (index : Integer) : string;
    function GetEventMessageText : string;
    function GetEventTime : TDateTime;

    function GetEventCategory: Integer;
    function GetEventType: Integer;
    function GetEventUser: string;
    function GetRecordNumber: DWORD;

  protected
  public
    EventLogThread : TEventLogThread ;

    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;

    function GetEventData (out len: DWORD): PAnsiChar;

    function GetNewerRecID: Integer;
    function GetOldestRecID: Integer;
    procedure ReadEvent (n : Integer);

    property NewerRecID : Integer read GetNewerRecID;
    property OldestRecID : Integer read GetOldestRecID;
    property EventCount : Integer read GetEventCount;
    property EventSource : string read GetEventSource;
    property EventComputer : string read GetEventComputer;
    property EventID : DWORD read GetEventID;
    property EventStringCount : DWORD read GetEventStringCount;
    property EventSID : PSID read GetEventSID;
    property EventUser : string read GetEventUser;
    property EventString [index : Integer] : string read GetEventString;
    property EventMessageText : string read GetEventMessageText;
    property EventTime : TDateTime read GetEventTime;
    property EventCategory : Integer read GetEventCategory;
    property EventType : Integer read GetEventType;
    property EventMessageHandler : string read fMessagePath ;
    property EventRecordNumber : DWORD read GetRecordNumber ;
  published
    property Server : string read fServer write SetServer;
    property Source : string read fSource write SetSource;
    property Log : string read fLog write SetLog;
    property OnChangeEventLog: TEventLogNotify read FOnChangeEventLog write FOnChangeEventLog;
  end;

   //--------------------------------------------------------------------------------------------------


implementation

uses Registry, unt_TraceWin;

//-------------------------------------------------------------------------------

function TimeTToDateTime (t : Integer) : TDateTime;
var
   sysTime, sysTime1 : TSystemTime;
begin
   DateTimeToSystemTime (EncodeDate(1970, 1, 1) + (t / 86400), sysTime);
   SystemTimeToTzSpecificLocalTime (Nil, sysTime, sysTime1);
   Result := SystemTimeToDateTime (sysTime1);
end;

//-------------------------------------------------------------------------------

constructor TEventLog.Create (AOwner : TComponent);
var
  cnLen : DWORD;
begin
   inherited Create (AOwner);

   fWindowHandle := Classes.AllocateHWnd (WndProc) ;

   fLog := 'Application';
   fSource := '';

   cnLen := MAX_COMPUTERNAME_LENGTH + 1;
   SetLength (fComputerName, cnLen + 1);
   GetComputerName (PChar (fComputerName), cnLen);
   fComputerName := PChar (fComputerName);
end;

//-------------------------------------------------------------------------------

destructor TEventLog.Destroy;
begin
   if dllModule <> 0 then
      FreeLibrary (dllModule);
   Close;
   Classes.DeallocateHWnd (fWindowHandle) ;
   inherited;
end;

//-------------------------------------------------------------------------------

procedure TEventLog.Open;
begin
   if not fOpen then
   begin
      fFileHandle := OpenEventLog (PChar (Server), PChar (Log));
      if fFileHandle = 0 then
         RaiseLastOSError ;
      fOpen := True ;
      //fCurrentRecordPos := -2 ;
      EventLogThread := TEventLogThread.create(fFileHandle, fWindowHandle);
   end
end;

//-------------------------------------------------------------------------------

procedure TEventLog.Close;
begin
   if fOpen then
   begin
      setevent (EventLogThread.hCloseEvent) ;
      if fFileHandle <> 0 then
      begin
         CloseEventLog (fFileHandle);
         fFileHandle := 0
      end;
      ReallocMem (fCurrentRecord, 0);
      fOpen := False
   end
end;

//-------------------------------------------------------------------------------

procedure TEventLog.SetServer (const value : string);
var
   oldOpen : boolean;
begin
   if fServer <> value then
   begin
      oldOpen := fOpen;
      Close;
      fServer := value;
      if oldOpen then
         Open
   end
end;

//-------------------------------------------------------------------------------

procedure TEventLog.SetSource (const value : string);
var
   oldOpen : boolean;
begin
   if fSource <> value then
   begin
      oldOpen := fOpen;
      Close;
      fSource := value;
      if oldOpen then Open
   end
end;

//-------------------------------------------------------------------------------

procedure TEventLog.SetLog (const value : string);
var
   oldOpen : boolean;
begin
   if fLog <> value then
   begin
      oldOpen := fOpen;
      Close;
      fLog := value;
      if oldOpen then Open
   end
end;

//-------------------------------------------------------------------------------

function TEventLog.GetEventCount : Integer;
var
   count : DWORD;
begin
   if not fOpen then begin
      result := 0 ;
      exit ;
   end ;
   GetNumberOfEventLogRecords (fFileHandle, count) ;
   result := Integer (count)
end;

//-------------------------------------------------------------------------------
function TEventLog.GetOldestRecID : Integer;
var
   offset : DWORD;
begin
   if not fOpen then begin
      result := 0 ;
      exit ;
   end ;
   GetOldestEventLogRecord (fFileHandle, offset) ;
   result := Integer (offset)
end;

//-------------------------------------------------------------------------------
function TEventLog.GetNewerRecID : Integer;
var
   offset : DWORD;
   count : DWORD;
begin
   if not fOpen then begin
      result := 0 ;
      exit ;
   end ;

   GetOldestEventLogRecord (fFileHandle, offset);
   GetNumberOfEventLogRecords (fFileHandle, count) ;

   result := Integer (offset+count-1) ;
end;

//-------------------------------------------------------------------------------

procedure TEventLog.ReadEvent (n : Integer);
var
   flags : DWORD;
   bytesRead, bytesNeeded : DWORD;
   dummy : char;
begin

   flags := EVENTLOG_SEEK_READ or EVENTLOG_FORWARDS_READ;

   ReadEventLogW (fFileHandle, flags, n, @dummy, 0, bytesRead, bytesNeeded);
   if GetLastError = ERROR_INSUFFICIENT_BUFFER then
   begin
      ReallocMem (fCurrentRecord, bytesNeeded);
      if not ReadEventLogW (fFileHandle, flags, n, fCurrentRecord, bytesNeeded, bytesRead, bytesNeeded) then
         RaiseLastOSError;
   end else begin
      RaiseLastOSError;
   end ;
   fCurrentRecordLen := bytesRead;
end;

//-------------------------------------------------------------------------------

function TEventLog.GetEventMessageText : string;
var
   //messagePath : string;
   count, i : Integer;
   p1 : PAnsiChar ;
   p : PChar;
   args, pArgs : ^PCHAR;
   st : string;

   function FormatMessageFrom (const dllName : string) : boolean;
   var
      buffer : PChar;
      fullDLLName : array [0..MAX_PATH] of char;
   begin
      result := False;
      ExpandEnvironmentStrings (PChar (dllName), fullDllName, MAX_PATH);
      if lastdllName <> fullDLLName then
      begin
         if dllModule <> 0 then
            FreeLibrary (dllModule);
         dllModule := LoadLibraryEx (fullDLLName, 0, LOAD_LIBRARY_AS_DATAFILE);
         lastdllName := fullDLLName
      end;

      if dllModule <> 0 then
      try
         if FormatMessage (
            FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_ARGUMENT_ARRAY,
            pointer (dllModule),
            EventID,
            0,
            PChar (@buffer),
            0,
            args) > 0 then
          begin
             st := buffer;
             LocalFree (THandle (buffer));
             result := True
          end ;
      finally
      end
   end;

   procedure getStrings() ;
   var
      c : integer ;
      substr : string ;
      added : boolean ;
   begin
      added := false ;
      st := '' ; // Message not found.  Insertion strings:';
      for c := 0 to Count - 1 do
      begin
         substr := EventString [c] ;
         if substr <> '' then begin
             if added then
                st := st + ', ' + substr
             else
                st := st + substr ;
            added := true ;
         end ;
      end ;
   end ;

begin
  st := '';
  count := EventStringCount;
  GetMem (args, count * sizeof (PChar));
  try

    // create a pointer array on each strings. Used by FormatMessage method.
    pArgs := args;
    p1 := PAnsiChar (fCurrentRecord) + PEventLogRecord (fCurrentRecord)^.StringOffset ;  // p1 must be declared as pAnsiChar for correct incrementation
    p := pChar(p1) ;  // cast Ansi pointer to wide pointer
    for i := 0 to count - 1 do
    begin
      pArgs^ := p;
      Inc (p, lstrlenW (p) + 1);
      Inc (pArgs)
    end;

    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey (Format ('SYSTEM\CurrentControlSet\Services\EventLog\%s\%s', [Log, EventSource]), False);
      fmessagePath := ReadString ('EventMessageFile');

      // C:\WINDOWS\Microsoft.NET\Framework\v1.0.3705\EventLogMessages.dll
      // C:\WINDOWS\Microsoft.NET\Framework\v1.1.4322\EventLogMessages.dll
      // %SystemRoot%\System32\userenv.dll;%SystemRoot%\System32\xpsp1res.dll
      // %SystemRoot%\System32\IoLogMsg.dll;%SystemRoot%\System32\Drivers\acpiec.sys
      if fMessagePath = '' then begin
         getStrings() ;   // construct 'st' from strings
      end else begin
         repeat
           i := Pos (';', fMessagePath);
           if i <> 0 then
           begin
             if FormatMessageFrom (Copy (fMessagePath, 1, i-1)) then
               break;
             fMessagePath := Copy (fMessagePath, i + 1, MaxInt);
           end
           else
             FormatMessageFrom (fMessagePath)
         until i = 0 ;
         if trim(st) = '' then
            getStrings() ;   // construct 'st' from strings
      end ;
    finally
      Free
    end
  finally
    FreeMem (args)
  end;
  result := st ;
end;

//-------------------------------------------------------------------------------

function TEventLog.GetEventSource : string;
var
   pAnsiStr : PAnsiChar;
begin
   pAnsiStr := PAnsiChar(fCurrentRecord);
   Inc(pAnsiStr,sizeof(TEventLogRecord));  // To increment correctly pstr, he must be a PAnsiChar
   setlength (result,lstrlenw(pwidechar(pAnsiStr))) ;
   StrLCopy (PWideChar(result), PWideChar(pAnsiStr), lstrlenw(PWideChar(pAnsiStr))) ;
end;

//-------------------------------------------------------------------------------

function TEventLog.GetEventComputer : string;
var
   pAnsiStr : PAnsiChar;

begin
   pAnsiStr := PAnsiChar(fCurrentRecord);
   Inc(pAnsiStr,sizeof(TEventLogRecord));  // To increment correctly pstr, he must be a PAnsiChar

   Inc(pAnsiStr,(lstrlenw(pwidechar(pAnsiStr))+1) * sizeof(char));  // bypass the "source" string
   setlength (result,lstrlenw(pwidechar(pAnsiStr))) ;
   StrLCopy (PWideChar(result), PWideChar(pAnsiStr), lstrlenw(PWideChar(pAnsiStr))) ;
end;

//-------------------------------------------------------------------------------

function TEventLog.GetEventID : DWORD;
begin
  result := PEventLogRecord (fCurrentRecord)^.EventID;
end;

//-------------------------------------------------------------------------------

function TEventLog.GetRecordNumber : DWORD;
begin
  result := PEventLogRecord (fCurrentRecord)^.RecordNumber;
end;


//-------------------------------------------------------------------------------

function TEventLog.GetEventStringCount : DWORD;
begin
  result := PEventLogRecord (fCurrentRecord)^.NumStrings;
end;

//-------------------------------------------------------------------------------

function TEventLog.GetEventSID : PSID;
var
  rec : PEventLogRecord;
begin
  rec := PEventLogRecord (fCurrentRecord);
  if rec^.UserSidLength <> 0 then
    result := PSID (PChar (fCurrentRecord) + rec^.userSIDOffset)
  else
    result := Nil;
end;

//-------------------------------------------------------------------------------

function TEventLog.GetEventData (out len : DWORD) : PAnsiChar ;
begin
   result := PAnsiChar (fCurrentRecord) + PEventLogRecord (fCurrentRecord)^.DataOffset;
   len := PEventLogRecord (fCurrentRecord)^.DataLength ;
end ;

//-------------------------------------------------------------------------------

function TEventLog.GetEventUser: string;
var
  accountNameLen : DWORD;
  domainNameLen : DWORD;
  domain, account : string;
  use : SID_NAME_USE;
  sid : PSID;
  res_lookup : boolean ;
begin
  sid := EventSID;

  if not Assigned (sid) then begin
    result := '-';
    exit ;
  end ;

  accountNameLen := 256;
  domainNameLen := 256;

  SetLength (Account, accountNameLen);
  SetLength (Domain, domainNameLen);

  res_lookup := LookupAccountSID ('', Sid, PChar (Account), accountNameLen, PChar (Domain), domainNameLen, use);

  if res_lookup then
  begin
    Account := PChar (Account);
    Domain := PChar (Domain);
    case use of
    SidTypeUser           : result := 'User';
    SidTypeGroup          : result := 'Group';
    SidTypeDomain         : result := 'Domain';
    SidTypeAlias          : result := 'Alias';
    SidTypeWellKnownGroup : result := 'WellKnownGroup';
    SidTypeDeletedAccount : result := 'DeletedAccount';
    SidTypeInvalid        : result := 'Invalid';
    SidTypeUnknown        : result := 'Unknown';
    end ;

    if ((use = sidTypeUser) or (use = sidTypeGroup)) and (domain <> fComputerName) then
      result := result + ' : ' + domain + '\' + account
    else
      result := result + ' : ' + account;
  end else begin
    result := 'n/a'
  end ;
end;

//-------------------------------------------------------------------------------

function TEventLog.GetEventString (index : Integer) : string;
var
  p : PChar;
begin
  if index < Integer (EventStringCount) then
  begin
    p := PChar (fCurrentRecord) + PEventLogRecord (fCurrentRecord)^.StringOffset;
    while index > 0 do
    begin
      Inc (p, lstrlen (p) + 1);
      Dec (index);
    end;
    result := p
  end
  else result := ''
end;

//-------------------------------------------------------------------------------

function TEventLog.GetEventTime : TDateTime;
begin
  result := TimeTToDateTime (PEventLogRecord (fCurrentRecord)^.TimeGenerated);
end;

//-------------------------------------------------------------------------------

function TEventLog.GetEventCategory: Integer;
begin
  result := PEventLogRecord (fCurrentRecord)^.EventCategory
end;

//-------------------------------------------------------------------------------

function TEventLog.GetEventType: Integer;
begin
  result := PEventLogRecord (fCurrentRecord)^.EventType;
end;

//-------------------------------------------------------------------------------

// receive message from TEventLogThread. Only WM_EVENTLOGCHANGED is send
procedure TEventLog.WndProc(var msg: TMessage);
begin
   with msg do begin
      result := 0 ;
      if Msg = WM_EVENTLOGCHANGED then begin
         if assigned (FOnChangeEventLog) then
            FOnChangeEventLog (self) ;
      end else begin // Handle all messages with the default handler
         Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
      end ;
   end ;
end;

//-------------------------------------------------------------------------------
//-------------------------------------------------------------------------------
//-------------------------------------------------------------------------------

{ TEventLogThread }

constructor TEventLogThread.create(fFileHandle: THandle; comp : THandle);
begin
   FreeOnTerminate := true ;
   hCloseEvent    := CreateEvent( nil, True, False, nil );   // Manualreset = true, initial = false
   hChangedEvent := CreateEvent( nil, false, False, nil );  // Manualreset = false, initial = false
   hEventHandle  := fFileHandle ;
   hComponentHandle := comp ;
   inherited create (false) ;  // CreateSuspended : false
end;

//-------------------------------------------------------------------------------

procedure TEventLogThread.Execute;
var
   dwHandleSignaled:   DWORD;
   HandlesToWaitFor: array[0..1] of THandle;
   OldOffset , OldCount, NewOffset, NewCount  : DWORD;
begin
   OldCount  := 0 ;
   OldOffset := 0 ;

   // ask to receive events
   NotifyChangeEventLog (hEventHandle, hChangedEvent) ;
   GetNumberOfEventLogRecords (hEventHandle, OldCount) ;

   // We will be waiting on these objects.
   HandlesToWaitFor[0] := hCloseEvent;
   HandlesToWaitFor[1] := hChangedEvent ;

   while True do begin
       // wait for a message or the CloseEvent. The Update will wait max 300 msec
       dwHandleSignaled := WaitForMultipleObjects({count} 2, {handelList} @HandlesToWaitFor, {bwaitall} False, {millisec} 300);
       case dwHandleSignaled of
          WAIT_OBJECT_0:     // CloseEvent signaled
             break ;
          WAIT_OBJECT_0 + 1: // ChangedEvent signaled : New message was received.
             begin
                // send to TEventLog.WndProc
                GetOldestEventLogRecord (hEventHandle, NewOffset);
                GetNumberOfEventLogRecords (hEventHandle, NewCount) ;

                //Frm_Trace.InternalTrace ('ChangedEvent signaled') ;
                //Frm_Trace.InternalTrace ('Thread : before Oldest : ' + inttostr (OldOffset) ) ;
                //Frm_Trace.InternalTrace ('Thread : new    Oldest : ' + inttostr (NewOffset)) ;
                //Frm_Trace.InternalTrace ('Thread : Before NumberOfEvent : ' + inttostr (OldCount) ) ;
                //Frm_Trace.InternalTrace ('Thread : new    NumberOfEvent : ' + inttostr (NewCount)) ;

                OldCount := NewCount ;
                OldOffset := NewOffset ; 
                PostMessage (hComponentHandle,WM_EVENTLOGCHANGED,0,0) ;
             end ;
          WAIT_TIMEOUT :
             begin
                GetOldestEventLogRecord (hEventHandle, NewOffset);
                GetNumberOfEventLogRecords (hEventHandle, NewCount) ;
                if (NewCount <> OldCount) or (OldOffset <> NewOffset) then begin
                   //Frm_Trace.InternalTrace ('WAIT_TIMEOUT') ;
                   //Frm_Trace.InternalTrace ('Thread : before Oldest : ' + inttostr (OldOffset) ) ;
                   //Frm_Trace.InternalTrace ('Thread : new    Oldest : ' + inttostr (NewOffset)) ;
                   //Frm_Trace.InternalTrace ('Thread : Before NumberOfEvent : ' + inttostr (OldCount) ) ;
                   //Frm_Trace.InternalTrace ('Thread : new    NumberOfEvent : ' + inttostr (NewCount)) ;
                   PostMessage (hComponentHandle,WM_EVENTLOGCHANGED,0,0) ;
                end ;
                OldCount := NewCount ;
                OldOffset := NewOffset ;
             end ;
       end ;
   end ;
end;


end.
