// Provide access to the warning, eror and debug node and
// send the trace using socket or windows messages
//
// Author : Thierry Parent
// Version : 12.4
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information


unit unt_XTrace;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, TraceToolCom_TLB, StdVcl, sysutils, tracetool , SocketTrace ;

type
  TXTrace = class(TAutoObject, IXTrace)
  protected
    procedure ClearAll; safecall;
    procedure Flush(FlushTimeOut: SYSINT); safecall;
    procedure Show(IsVisible: WordBool); safecall;
    function Get_Debug: IXTraceToSend; safecall;
    function Get_Warning: IXTraceToSend; safecall;
    function Get_Error: IXTraceToSend; safecall;
    function Get_Options: IXTraceOptions; safecall;
    function Get_Watches: IXWinWatch; safecall;
    function Get_WinTrace: IXWinTrace; safecall;
    function CreateWinTrace(const WinTraceId, WinTraceText: WideString): IXWinTrace; safecall;
    function CreateWinWatch(const WinWatchId, WinWatchText: WideString): IXWinWatch; safecall;
    function CreateTraceTable: IXTraceTable; safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    procedure CloseViewer; safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  private
     fWintrace : IXWinTrace ;
     fOptions: IXTraceOptions ;
     fWatches: IXWinWatch ;
  end;

implementation

uses ComServ,
unt_XWinTrace,
unt_XWinWatch ,
unt_XTraceNodeEx,
unt_XTraceOptions,
unt_XTraceTable;

//------------------------------------------------------------------------------

procedure TXTrace.Initialize;
begin
   // don't send thread id : it's marchaling thread
   TTrace.Options.SendMode := tmWinMsg ;   // force the use of windows mode (default. Can be changed by the client)
   ttrace.Options.SendThreadId := false ;
   ttrace.Options.SendProcessName := false ;  // don't need to show the OLe wrapper : TraceToolCom.dll
   globalColorKind := $00000000 ; // BGR
   inherited;
end;

//------------------------------------------------------------------------------

destructor TXTrace.Destroy;
begin
   //ttrace.error.send ('TXTrace.destroy', integer(pointer(self))) ;

   if fWintrace <> nil then
      fWintrace := nil ;

   if fWatches <> nil then
      fWatches := nil ;

   ttrace.stop ;
   inherited;
end;

//------------------------------------------------------------------------------

function TXTrace.Get_Debug: IXTraceToSend;
begin
   result := Get_WinTrace().Debug ;
end;

//------------------------------------------------------------------------------

function TXTrace.Get_Error: IXTraceToSend;
begin
   result := Get_WinTrace().Error ;
end;

//------------------------------------------------------------------------------

function TXTrace.Get_Warning: IXTraceToSend;
begin
   result := Get_WinTrace().Warning ;
end;

//------------------------------------------------------------------------------

function TXTrace.Get_WinTrace: IXWinTrace;
begin
   if fWintrace = nil then begin
      fWintrace :=  TXWinTrace.create ;
      fWintrace.Id := '' ;                 // force create warning, debug and error
   end ;
   result := fWinTrace ;
end;

//------------------------------------------------------------------------------

procedure TXTrace.ClearAll;
begin
   TTrace.ClearAll() ;
end;

//------------------------------------------------------------------------------

procedure TXTrace.Flush(FlushTimeOut: SYSINT);
begin
   TTrace.Flush(FlushTimeOut);
end;

//------------------------------------------------------------------------------

procedure TXTrace.Show(IsVisible: WordBool);
begin
   TTrace.Show(IsVisible);
end;

//------------------------------------------------------------------------------

function TXTrace.Get_Options: IXTraceOptions;
begin
   if fOptions = nil then
      fOptions := TXTraceOptions.create() ;     // no link is necessary
   result := fOptions ;
end;

//------------------------------------------------------------------------------

function TXTrace.Get_Watches: IXWinWatch;
begin
   if fWatches = nil then
      fWatches := TXWinWatch.create ();         // link is created by constructor and set to ttrace.watches
   result := fWatches ;
end;

//------------------------------------------------------------------------------

function TXTrace.CreateWinTrace(const WinTraceId, WinTraceText: WideString): IXWinTrace;
begin
   result := TXWinTrace.Create() ;
   result.WinTraceText := WinTraceText ;
   result.Id := WinTraceId ;                   // link is created when setting ID
end;

//------------------------------------------------------------------------------

function TXTrace.CreateWinWatch(const WinWatchId, WinWatchText: WideString): IXWinWatch;
begin
   result := TXWinWatch.create(WinWatchId,WinWatchText) ;  // link is created by constructor
end;

//------------------------------------------------------------------------------

function TXTrace.CreateTraceTable: IXTraceTable;
begin
   result := TXTraceTable.create() ; // link is created by constructor
end;

//------------------------------------------------------------------------------

procedure TXTrace.Start;
begin
   TTrace.start() ;
end;

//------------------------------------------------------------------------------

procedure TXTrace.Stop;
begin
   TTrace.stop() ;
end;

//------------------------------------------------------------------------------

procedure TXTrace.CloseViewer;
begin
   TTrace.CloseViewer() ;
end;

//------------------------------------------------------------------------------

initialization
  TAutoObjectFactory.Create(ComServer, TXTrace, Class_XTrace,ciMultiInstance, tmFree);     // (ciInternal, ciSingleInstance, ciMultiInstance)(tmSingle, tmApartment, tmFree, tmBoth, tmNeutral);
end.
