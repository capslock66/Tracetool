// Author : Thierry Parent
// Version : 12.4
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information

unit unt_XTraceNode;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, TraceToolCom_TLB, StdVcl, SysUtils ,
  tracetool, unt_XTraceSend;

type

  TXTraceNode = class(TXTraceToSend, IXTraceNode)
  protected
    function ResendIconIndex(Index: SYSINT): IXTraceNode; safecall;
    function SetBackgroundColor(Color, ColId: SYSINT): IXTraceNode; safecall;
    function Append(const NewLeftMsg, NewRightMsg: WideString): IXTraceNode; safecall;
    function AppendLeft(const NewLeftMsg: WideString): IXTraceNode; safecall;
    function AppendRight(const NewRightMsg: WideString): IXTraceNode; safecall;
    function Delete: IXTraceNode; safecall;
    function DeleteChildren: IXTraceNode; safecall;
    function Resend(const LeftMsg, RightMsg: WideString): IXTraceNode; safecall;
    function ResendLeft(const LeftMsg: WideString): IXTraceNode; safecall;
    function ResendRight(const RightMsg: WideString): IXTraceNode; safecall;
    function SetFontDetail(ColId: SYSINT; Bold, Italic: WordBool; Color, Size: SYSINT; const FontName: WideString): IXTraceNode; safecall;
    function SetSelected: IXTraceNode; safecall;
    function Show: IXTraceNode; safecall;
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
    procedure Set_Id(const Value: WideString); safecall;
    procedure Set_Tag(Value: SYSINT); safecall;
    procedure Set_WinTraceId(const Value: WideString); safecall;
    procedure UnIndent(const LeftMsg, RightMsg: WideString); safecall;
  public
    // link is inherited from TXTraceToSend but point to a ITraceNode in place of ITraceToSend
    constructor create;
    destructor Destroy; override;
  private
  end;

implementation

uses ComServ, unt_XWinTrace, unt_XTraceNodeEx , unt_XTraceOptions ;

//------------------------------------------------------------------------------

// no public constructor. Use a SendXXX method from IXTraceToSend to create this object
constructor TXTraceNode.create();
begin
   //TTrace.warning.send ('TXTraceNode.create', integer(pointer(self))) ;
   inherited create();
   // link is created by calling a SendXXX method from IXTraceToSend
end;

//------------------------------------------------------------------------------

destructor TXTraceNode.Destroy;
begin
   //TTrace.warning.send ('TXTraceNode.Destroy', integer(pointer(self))) ;
   inherited;  // will call TXTraceToSend.Destroy() -> reset link
end;

//------------------------------------------------------------------------------

function TXTraceNode.Append(const NewLeftMsg, NewRightMsg: WideString): IXTraceNode;
begin
    if Link = nil then
      exit ;
   ITraceNode(Link).Append(NewLeftMsg,NewRightMsg) ;
   result := self ;
end;

//------------------------------------------------------------------------------

function TXTraceNode.AppendLeft(const NewLeftMsg: WideString): IXTraceNode;
begin
   if Link = nil then
      exit ;

   ITraceNode(Link).AppendLeft(NewLeftMsg) ;
   result := self ;
end;

//------------------------------------------------------------------------------

function TXTraceNode.AppendRight(const NewRightMsg: WideString): IXTraceNode;
begin
   if Link = nil then
      exit ;

   ITraceNode(Link).AppendRight(NewRightMsg) ;
   result := self ;
end;

//------------------------------------------------------------------------------

function TXTraceNode.Delete: IXTraceNode;
begin
   if Link = nil then
      exit ;

   ITraceNode(Link).Delete() ;
   result := self ;
end;

//------------------------------------------------------------------------------

function TXTraceNode.DeleteChildren: IXTraceNode;
begin
   if Link = nil then
      exit ;

   ITraceNode(Link).DeleteChildren() ;
   result := self ;
end;

//------------------------------------------------------------------------------

function TXTraceNode.Resend(const LeftMsg,RightMsg: WideString): IXTraceNode;
begin
   if Link = nil then
      exit ;
   ITraceNode(Link).Resend(LeftMsg,RightMsg) ;
   result := self ;
end;

//------------------------------------------------------------------------------

function TXTraceNode.ResendLeft(const LeftMsg: WideString): IXTraceNode;
begin
   if Link = nil then
      exit ;
   ITraceNode(Link).ResendLeft(LeftMsg) ;
   result := self ;
end;

//------------------------------------------------------------------------------

function TXTraceNode.ResendRight(const RightMsg: WideString): IXTraceNode;
begin
   if Link = nil then
      exit ;
   ITraceNode(Link).ResendRight(RightMsg) ;
   result := self ;
end;

//------------------------------------------------------------------------------

function TXTraceNode.SetFontDetail(ColId: SYSINT; Bold, Italic: WordBool; Color, Size: SYSINT; const FontName: WideString): IXTraceNode;
begin
   result := self ;
   if Link <> nil then
      ITraceNode(Link).SetFontDetail(ColId, Bold, Italic, ConvertColor(color), Size, FontName) ;
end;

//------------------------------------------------------------------------------

function TXTraceNode.SetSelected: IXTraceNode;
begin
   result := self ;
   if Link <> nil then
      ITraceNode(Link).SetSelected() ;
end;

//------------------------------------------------------------------------------

function TXTraceNode.Show: IXTraceNode;
begin
   result := self ;
   if Link <> nil then
      ITraceNode(Link).Show ;
end;

//------------------------------------------------------------------------------

function TXTraceNode.ResendIconIndex(Index: SYSINT): IXTraceNode ;
begin
   result := self ;
   if Link <> nil then
      ITraceNode(Link).ResendIconIndex (index);
end;

//------------------------------------------------------------------------------

function TXTraceNode.SetBackgroundColor(Color, ColId: SYSINT): IXTraceNode;
begin
   result := self ;
   if Link <> nil then
      ITraceNode(Link).SetBackgroundColor(ConvertColor(color), ColId) ;
end;

//------------------------------------------------------------------------------
// From here : Inherit from TXTraceToSend
// The type library don't understand that TXTraceNode inherit from TXTraceToSend and always recreate all functions
//------------------------------------------------------------------------------

function TXTraceNode.CreateNodeEx: IXTraceNodeEx;
begin
   result := inherited CreateNodeEx;
end;

//------------------------------------------------------------------------------

function TXTraceNode.Get_Enabled: WordBool;
begin
   result := inherited Get_Enabled;
end;

//------------------------------------------------------------------------------

function TXTraceNode.Get_IconIndex: SYSINT;
begin
   result := inherited Get_IconIndex;
end;

//------------------------------------------------------------------------------

function TXTraceNode.Get_Id: WideString;
begin
   result := inherited Get_Id;
end;

//------------------------------------------------------------------------------

function TXTraceNode.Get_Tag: SYSINT;
begin
   result := inherited Get_Tag;
end;

//------------------------------------------------------------------------------

function TXTraceNode.Get_WinTraceId: WideString;
begin
   result := inherited Get_WinTraceId;
end;

//------------------------------------------------------------------------------

function TXTraceNode.Send(const LeftMsg, RightMsg: WideString): IXTraceNode;
begin
   result := inherited Send(LeftMsg, RightMsg);
end;

//------------------------------------------------------------------------------

function TXTraceNode.SendDump(const LeftMsg, ShortTitle: WideString;Adress: PChar; Count: SYSINT): IXTraceNode;
begin
   result := inherited SendDump(LeftMsg, ShortTitle,Adress,Count);
end;

//------------------------------------------------------------------------------

function TXTraceNode.SendObject(const LeftMsg: WideString;Obj: OleVariant): IXTraceNode;
begin
   result := inherited SendObject(LeftMsg,Obj);
end;

//------------------------------------------------------------------------------

function TXTraceNode.SendValue(const LeftMsg: WideString; Obj: OleVariant;const ObjTitle: WideString): IXTraceNode;
begin
   result := inherited SendValue(LeftMsg, Obj, ObjTitle);
end;

//------------------------------------------------------------------------------

function TXTraceNode.SendValueIDisptach(const LeftMsg: WideString;const Obj: IDispatch; const ObjTitle: WideString): IXTraceNode;
begin
   result := inherited SendValueIDisptach(LeftMsg,Obj,ObjTitle);
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.EnterMethod(const LeftMsg, RightMsg: WideString);
begin
   inherited EnterMethod(LeftMsg, RightMsg);
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.ExitMethod(const LeftMsg, RightMsg: WideString);
begin
   inherited ExitMethod(LeftMsg, RightMsg);
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.Indent(const LeftMsg, RightMsg: WideString);
begin
   inherited Indent(LeftMsg, RightMsg);
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.SendBackgroundColor(const LeftMsg: WideString; Color,ColId: SYSINT);
begin
   inherited SendBackgroundColor(LeftMsg,color,ColId);
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.SendTable(const LeftMsg: WideString;var Table: IXTraceTable);
begin
   inherited SendTable(LeftMsg,Table);
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.SendXml(const LeftMsg, Xml: WideString);
begin
   inherited SendXml(LeftMsg, Xml);
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.Set_Enabled(Value: WordBool);
begin
   inherited Set_Enabled(Value);
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.Set_IconIndex(Value: SYSINT);
begin
   inherited Set_IconIndex(Value);
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.Set_Tag(Value: SYSINT);
begin
   inherited Set_Tag(Value) ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.Set_WinTraceId(const Value: WideString);
begin
   inherited Set_WinTraceId(Value);
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.UnIndent(const LeftMsg, RightMsg: WideString);
begin
   inherited UnIndent(LeftMsg, RightMsg);
end;

//------------------------------------------------------------------------------

procedure TXTraceNode.Set_Id(const Value: WideString);
begin
   inherited Set_Id(Value);
end;

//------------------------------------------------------------------------------

initialization
  TAutoObjectFactory.Create(ComServer, TXTraceNode, Class_XTraceNode, ciSingleInstance, tmFree);
end.
