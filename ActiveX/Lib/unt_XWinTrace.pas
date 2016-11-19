// Author : Thierry Parent
// Version : 12.4
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information unit unt_XWinTrace;

unit unt_XWinTrace ;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, TraceToolCom_TLB, StdVcl, windows,
  tracetool, unt_XTraceSend;

type
  TXWinTrace = class(TXTraceToSend, IXWinTrace)
  protected
    procedure Set_Id(const Value: WideString); safecall;
    procedure ClearAll; safecall;
    procedure DisplayWin; safecall;
    function Get_Debug: IXTraceToSend; safecall;
    function Get_Error: IXTraceToSend; safecall;
    function Get_Warning: IXTraceToSend; safecall;
    function Get_Link: IUnknown; safecall;
    function Get_WinTraceText: WideString; safecall;
    procedure Set_WinTraceText(const Value: WideString); safecall;
    procedure LoadXml(const Filename: WideString); safecall;
    procedure SaveToTextfile(const Filename: WideString); safecall;
    procedure SaveToXml(const Filename, StyleSheet: WideString); safecall;
    procedure setColumnsWidth(const Widths: WideString); safecall;
    procedure setLogFile(const Filename: WideString; Mode: LogMode; MaxLines: Integer);
          safecall;
    procedure setMultiColumn(MainColIndex: SYSINT); safecall;
    procedure setColumnsTitle(const Titles: WideString); safecall;
    function CreateNodeEx: IXTraceNodeEx; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_IconIndex: SYSINT; safecall;
    function Get_Id: WideString; safecall;
    function Get_Tag: SYSINT; safecall;
    function Get_WinTraceId: WideString; safecall;
    function Send(const LeftMsg, RightMsg: WideString): IXTraceNode; safecall;
    function SendDump(const LeftMsg, ShortTitle: WideString; Adress: PChar;
      Count: SYSINT): IXTraceNode; safecall;
    function SendObject(const LeftMsg: WideString;
      Obj: OleVariant): IXTraceNode; safecall;
    function SendValue(const LeftMsg: WideString; Obj: OleVariant;
      const ObjTitle: WideString): IXTraceNode; safecall;
    function SendValueIDisptach(const LeftMsg: WideString;
      const Obj: IDispatch; const ObjTitle: WideString): IXTraceNode;
      safecall;
    procedure EnterMethod(const LeftMsg, RightMsg: WideString); safecall;
    procedure ExitMethod(const LeftMsg, RightMsg: WideString); safecall;
    procedure Indent(const LeftMsg, RightMsg: WideString); safecall;
    procedure SendBackgroundColor(const LeftMsg: WideString; Color,
      ColId: SYSINT); safecall;
    procedure SendTable(const LeftMsg: WideString; var Table: IXTraceTable);
      safecall;
    procedure SendXml(const LeftMsg, Xml: WideString); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_IconIndex(Value: SYSINT); safecall;
    procedure Set_Tag(Value: SYSINT); safecall;
    procedure Set_WinTraceId(const Value: WideString); safecall;
    procedure UnIndent(const LeftMsg, RightMsg: WideString); safecall;
    procedure Close; safecall;
  public
    // link is inherited from TXTraceToSend but point to a IWinTrace in place of ITraceToSend
    constructor create;
    destructor Destroy; override;
  private
    fDebug, fWarning, fError : IXTraceToSend ;
    fWinTraceText : string ;
  end;

implementation

uses ComServ, unt_XTraceNode, unt_XTraceNodeEx , SysUtils ;

//------------------------------------------------------------------------------

// no public constructor. Use a SendXXX method from IXTraceToSend to create this object
constructor TXWinTrace.create();
begin
   inherited create();
   // link is created by calling a SendXXX method from IXTraceToSend
end;

//------------------------------------------------------------------------------
// act like a constructor. Window text must be specified before setting ID
procedure TXWinTrace.Set_Id(const Value: WideString);
begin
   if value = '' then
      Link := TTrace.createWinTrace('_')                      // master WinTrace
   else
      Link := TTrace.createWinTrace(Value,fWinTraceText) ;    // Id is saved in IWinTrace
end;

//------------------------------------------------------------------------------

//procedure TXWinTrace.Initialize;
//begin
//   //ttrace.error.send ('TXWinTrace.Initialize', integer(pointer(self))) ;
//   inherited;
//end;

//------------------------------------------------------------------------------

destructor TXWinTrace.Destroy;
begin
   //ttrace.error.send ('TXWinTrace.Destroy', integer(pointer(self))) ;
   fDebug := nil ;     // free ITraceNode
   fWarning := nil ;
   fError := nil ;
   link := nil ;       // free IWintrace
   inherited;
end;

//------------------------------------------------------------------------------

// read only link
function TXWinTrace.Get_Link: IUnknown;
begin
   result := Link ;
end;

//------------------------------------------------------------------------------

function TXWinTrace.Get_Debug: IXTraceToSend;
var
   node : TXTraceToSend ;
begin
   result := nil ;
   if Link = nil then
      exit ;

   if fDebug = nil then begin
      // create public wrapper node
      node := TXTraceToSend.Create() ;
      // link public node to internal ITraceNode nodes
      node.link {ITraceToSend}  := IWinTrace(Link).Debug() ;   // debug return a {ITraceToSend}
      fDebug := node ;
   end ;
   result := fDebug ;
end;

//------------------------------------------------------------------------------

function TXWinTrace.Get_Error: IXTraceToSend;
var
   node : TXTraceToSend ;
begin
   result := nil ;
   if Link = nil then
      exit ;

   if fError = nil then begin
      node := TXTraceToSend.Create() ;
      node.link := IWinTrace(Link).Error() ;
      fError := node ;
   end ;
   result := fError ;
end;

//------------------------------------------------------------------------------

function TXWinTrace.Get_Warning: IXTraceToSend;
var
   node : TXTraceToSend ;
begin
   result := nil ;
   if Link = nil then
      exit ;

   if fWarning = nil then begin
      node := TXTraceToSend.Create() ;
      node.link := IWinTrace(Link).Warning() ;
      fWarning := node ;
   end ;
   result := fWarning ;
end;

//------------------------------------------------------------------------------


function TXWinTrace.Get_WinTraceText: WideString;
begin
   result := fWinTraceText ;
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.Set_WinTraceText(const Value: WideString);
begin
   fWinTraceText := value ;
end;

//------------------------------------------------------------------------------


procedure TXWinTrace.ClearAll;
begin
   if link <> nil then
      IWinTrace(Link).ClearAll ;
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.DisplayWin;
begin
   if link <> nil then
      IWinTrace(Link).DisplayWin ;
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.LoadXml(const Filename: WideString);
begin
   if link <> nil then
      IWinTrace(Link).LoadXml(Filename) ;
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.SaveToTextfile(const Filename: WideString);
begin
   if link <> nil then
      IWinTrace(Link).SaveToTextfile(Filename) ;
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.SaveToXml(const Filename, StyleSheet: WideString);
begin
   if link <> nil then
      IWinTrace(Link).SaveToXml(Filename,StyleSheet) ;
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.setColumnsWidth(const Widths: WideString);
begin
   if link <> nil then
      IWinTrace(Link).setColumnsWidth(Widths) ;
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.setLogFile(const Filename: WideString; Mode: LogMode; MaxLines: Integer);

begin
   if link <> nil then
      IWinTrace(Link).setLogFile(Filename,Mode,MaxLines) ;
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.setMultiColumn(MainColIndex: SYSINT);
begin
   if link <> nil then
      IWinTrace(Link).setMultiColumn(MainColIndex) ;
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.setColumnsTitle(const Titles: WideString);
begin
   if link <> nil then
      IWinTrace(Link).setColumnsTitle(Titles) ;
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.Close;
begin
   if link <> nil then
      IWinTrace(Link).Close() ;
end;

//------------------------------------------------------------------------------
// From here : Inherit from TXTraceToSend
// The type library don't understand that TXTraceNode inherit from TXTraceToSend and always recreate all functions
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------

function TXWinTrace.CreateNodeEx: IXTraceNodeEx;
begin
   result := inherited CreateNodeEx;
end;

//------------------------------------------------------------------------------

function TXWinTrace.Get_Enabled: WordBool;
begin
   result := inherited Get_Enabled;
end;

//------------------------------------------------------------------------------

function TXWinTrace.Get_IconIndex: SYSINT;
begin
   result := inherited Get_IconIndex;
end;

//------------------------------------------------------------------------------

function TXWinTrace.Get_Id: WideString;
begin
   result := inherited Get_Id;
end;

//------------------------------------------------------------------------------

function TXWinTrace.Get_Tag: SYSINT;
begin
   result := inherited Get_Tag;
end;

//------------------------------------------------------------------------------

function TXWinTrace.Get_WinTraceId: WideString;
begin
   result := inherited Get_WinTraceId;
end;

//------------------------------------------------------------------------------

function TXWinTrace.Send(const LeftMsg, RightMsg: WideString): IXTraceNode;
begin
   result := inherited Send(LeftMsg, RightMsg);
end;

//------------------------------------------------------------------------------

function TXWinTrace.SendDump(const LeftMsg, ShortTitle: WideString;Adress: PChar; Count: SYSINT): IXTraceNode;
begin
   result := inherited SendDump(LeftMsg, ShortTitle,Adress,Count);
end;

//------------------------------------------------------------------------------

function TXWinTrace.SendObject(const LeftMsg: WideString;Obj: OleVariant): IXTraceNode;
begin
   result := inherited SendObject(LeftMsg,Obj);
end;

//------------------------------------------------------------------------------

function TXWinTrace.SendValue(const LeftMsg: WideString; Obj: OleVariant;const ObjTitle: WideString): IXTraceNode;
begin
   result := inherited SendValue(LeftMsg, Obj, ObjTitle);
end;

//------------------------------------------------------------------------------

function TXWinTrace.SendValueIDisptach(const LeftMsg: WideString;const Obj: IDispatch; const ObjTitle: WideString): IXTraceNode;
begin
   result := inherited SendValueIDisptach(LeftMsg,Obj,ObjTitle);
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.EnterMethod(const LeftMsg, RightMsg: WideString);
begin
   inherited EnterMethod(LeftMsg, RightMsg);
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.ExitMethod(const LeftMsg, RightMsg: WideString);
begin
   inherited ExitMethod(LeftMsg, RightMsg);
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.Indent(const LeftMsg, RightMsg: WideString);
begin
   inherited Indent(LeftMsg, RightMsg);
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.SendBackgroundColor(const LeftMsg: WideString; Color,ColId: SYSINT);
begin
   inherited SendBackgroundColor(LeftMsg,Color,ColId);
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.SendTable(const LeftMsg: WideString;var Table: IXTraceTable);
begin
   inherited SendTable(LeftMsg,Table);
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.SendXml(const LeftMsg, Xml: WideString);
begin
   inherited SendXml(LeftMsg, Xml);
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.Set_Enabled(Value: WordBool);
begin
   inherited Set_Enabled(Value);
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.Set_IconIndex(Value: SYSINT);
begin
   inherited Set_IconIndex(Value);
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.Set_Tag(Value: SYSINT);
begin
   inherited Set_Tag(Value) ;
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.Set_WinTraceId(const Value: WideString);
begin
   inherited Set_WinTraceId(Value);
end;

//------------------------------------------------------------------------------

procedure TXWinTrace.UnIndent(const LeftMsg, RightMsg: WideString);
begin
   inherited UnIndent(LeftMsg, RightMsg);
end;

//------------------------------------------------------------------------------


initialization
  TAutoObjectFactory.Create(ComServer, TXWinTrace, Class_XWinTrace, ciSingleInstance, tmFree);
end.
