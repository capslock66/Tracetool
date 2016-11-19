// Author : Thierry Parent
// Version : 12.4
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license informationunit unt_XTraceOptions;

unit unt_XTraceSend;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, StdVcl, sysutils ,
  TraceToolCom_TLB,
  tracetool;

type
  TXTraceToSend = class(TAutoObject, IXTraceToSend)
  protected
    function  CreateNodeEx: IXTraceNodeEx; safecall;
    function  Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function  Get_IconIndex: SYSINT; safecall;
    procedure Set_IconIndex(Value: SYSINT); safecall;
    function  Get_Id: WideString; safecall;
    procedure Set_Id(const Value: WideString); safecall;
    function  Get_Tag: SYSINT; safecall;
    procedure Set_Tag(Value: SYSINT); safecall;
    function  Get_WinTraceId: WideString; safecall;
    procedure Set_WinTraceId(const Value: WideString); safecall;
    function  Send(const LeftMsg, RightMsg: WideString): IXTraceNode; safecall;
    function  SendDump(const LeftMsg, ShortTitle: WideString; Adress: PChar; Count: SYSINT): IXTraceNode; safecall;
    function  SendObject(const LeftMsg: WideString; Obj: OleVariant): IXTraceNode; safecall;
    function  SendValue(const LeftMsg: WideString; Obj: OleVariant; const ObjTitle: WideString): IXTraceNode; safecall;
    function  SendValueIDisptach(const LeftMsg: WideString; const Obj: IDispatch; const ObjTitle: WideString): IXTraceNode; safecall;
    procedure Indent(const LeftMsg, RightMsg: WideString); safecall;
    procedure UnIndent(const LeftMsg, RightMsg: WideString); safecall;
    procedure SendBackgroundColor(const LeftMsg: WideString; Color, ColId: SYSINT); safecall;
    procedure SendTable(const LeftMsg: WideString; var Table: IXTraceTable); safecall;
    procedure SendXml(const LeftMsg, Xml: WideString); safecall;
    procedure EnterMethod(const LeftMsg, RightMsg: WideString); safecall;
    procedure ExitMethod(const LeftMsg, RightMsg: WideString); safecall;
  public
    Link : ITraceToSend ;
    constructor create;
    destructor Destroy; override;
  end;

implementation

uses ComServ  ,
  unt_XTraceNodeEx,
  unt_XTraceNode,
  unt_XTraceOptions,
  unt_XTraceTable;

//------------------------------------------------------------------------------

constructor TXTraceToSend.create();
begin
   //TTrace.warning.send ('TXTraceToSend.create', integer(pointer(self))) ;
   inherited create();
end;

//------------------------------------------------------------------------------

destructor TXTraceToSend.Destroy;
begin
   //ttrace.error.send ('TXTraceToSend.Destroy', integer(pointer(self))) ;
   link := nil ;  // release internal link (ITraceNode)
   inherited;
end;

//------------------------------------------------------------------------------

function TXTraceToSend.CreateNodeEx: IXTraceNodeEx;
var
   _ITraceNodeEx : ITraceNodeEx ;
   XTraceNodeEx : TXTraceNodeEx ;
begin
   result := nil ;

   if Link = nil then
      exit ;

   _ITraceNodeEx :=  Link.CreateNodeEx()  ;           // create internal nodeEx  : call TTraceNode.create (self,true);
   XTraceNodeEx :=  TXTraceNodeEx.Create() ;          // create public nodeEx wrapper
   XTraceNodeEx.Link := _ITraceNodeEx  ;              // link public to internal nodeEx
   result := XTraceNodeEx ;                           // increment ref count and return it
   //result.ParentNodeId := _ITraceNodeEx.id ;
   //result.IconIndex    := _ITraceNodeEx.IconIndex ;
   //result.Enabled      := _ITraceNodeEx.Enabled ;
end;

//------------------------------------------------------------------------------

function TXTraceToSend.Get_Enabled: WordBool;
begin
   result := false ;
   if Link = nil then
      exit ;
   result := Link.Enabled
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.Set_Enabled(Value: WordBool);
begin
   if Link = nil then
      exit ;
   Link.Enabled := value ;
end;

//------------------------------------------------------------------------------

function TXTraceToSend.Get_IconIndex: SYSINT;
begin
   result := -1 ;
   if Link = nil then
      exit ;
   result := link.IconIndex ;
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.Set_IconIndex(Value: SYSINT);
begin
   if Link = nil then
      exit ;
   Link.IconIndex := Value ;
end;

//------------------------------------------------------------------------------

function TXTraceToSend.Get_Id: WideString;
begin
   Result := '' ;
   if Link = nil then
      exit ;
   result := Link.id ;
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.Set_Id(const Value: WideString);
begin
   if Link = nil then
      exit ;
   Link.id := value ;
end;

//------------------------------------------------------------------------------

function TXTraceToSend.Get_Tag: SYSINT;
begin
   result := 0 ;
   if Link = nil then
      exit ;
   result := Link.Tag ;
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.Set_Tag(Value: SYSINT);
begin
   if Link = nil then
      exit ;
   Link.Tag := value ;
end;

//------------------------------------------------------------------------------

function TXTraceToSend.Get_WinTraceId: WideString;
begin
   result := '' ;
   if Link = nil then
      exit ;

   result := Link.WinTraceId ;
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.Set_WinTraceId(const Value: WideString);
begin
   if Link = nil then
      exit ;
   Link.WinTraceId := value ;
end;

//------------------------------------------------------------------------------

function TXTraceToSend.Send(const LeftMsg,RightMsg: WideString): IXTraceNode;
var
   _ITraceNode : ITraceNode ;
   XTraceNode : TXTraceNode ;
begin
   //TTrace.warning.indent ('TXTraceToSend.Send', inttostr(integer(pointer(self)))) ;
   try
      result := nil ;
      if Link = nil then
         exit ;

      _ITraceNode := Link.send (LeftMsg, RightMsg) ;    // return ITraceNode
      XTraceNode := TXTraceNode.create() ;              // create a TXTraceNode Wrapper
      XTraceNode.Link := _ITraceNode ;                  // linked to the native ITraceNode
      result := XTraceNode ;
   finally
      //TTrace.warning.send ('TXTraceToSend.Send done', inttostr(integer(pointer(self)))) ;
      //TTrace.warning.unindent() ;
   end;
end;

//------------------------------------------------------------------------------

function TXTraceToSend.SendDump(const LeftMsg, ShortTitle: WideString; Adress: PChar; Count: SYSINT): IXTraceNode;
var
   XTraceNode : TXTraceNode ;
begin
   if Link = nil then
      exit ;

   XTraceNode := TXTraceNode.Create() ;                                              // create a TXTraceNode Wrapper
   XTraceNode.Link := Link.SendDump (LeftMsg, ShortTitle, Pointer(Adress), Count) ;  // linked to an ITraceNode
   result := XTraceNode ;
end;

//------------------------------------------------------------------------------

function TXTraceToSend.SendObject(const LeftMsg: WideString; Obj: OleVariant): IXTraceNode;
var
   XTraceNode : TXTraceNode ;
begin
   if Link = nil then
      exit ;

   XTraceNode := TXTraceNode.Create() ;                  // create a TXTraceNode Wrapper
   XTraceNode.Link := Link.SendObject (LeftMsg, Obj) ;   // linked to an ITraceNode
   result := XTraceNode ;
end;

//------------------------------------------------------------------------------

function TXTraceToSend.SendValue(const LeftMsg: WideString; Obj: OleVariant; const ObjTitle: WideString): IXTraceNode;
var
   XTraceNode : TXTraceNode ;
begin
   if Link = nil then
      exit ;

   XTraceNode := TXTraceNode.Create() ;                          // create a TXTraceNode Wrapper
   XTraceNode.Link := Link.SendValue(LeftMsg,Obj,ObjTitle) ;     // linked to an ITraceNode
   result := XTraceNode ;
end;

//------------------------------------------------------------------------------

function TXTraceToSend.SendValueIDisptach(const LeftMsg: WideString;  const Obj: IDispatch; const ObjTitle: WideString): IXTraceNode;
var
   XTraceNode : TXTraceNode ;
begin
   if Link = nil then
      exit ;

   XTraceNode := TXTraceNode.Create() ;                          // create a TXTraceNode Wrapper
   XTraceNode.Link := Link.SendValue(LeftMsg,obj,ObjTitle) ;     // linked to an ITraceNode
   result := XTraceNode ;
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.Indent(const LeftMsg, RightMsg: WideString);
begin
   if Link = nil then
      exit ;

   Link.Indent(LeftMsg,RightMsg);
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.UnIndent(const LeftMsg, RightMsg: WideString);
begin
   if Link = nil then
      exit ;

   Link.UnIndent(LeftMsg,RightMsg);
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.SendBackgroundColor(const LeftMsg: WideString; Color, ColId: SYSINT);
begin
   if Link = nil then
      exit ;
   link.SendBackgroundColor(LeftMsg,ConvertColor(color),colId) ;
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.SendTable(const LeftMsg: WideString; var Table: IXTraceTable);
var
   _ITraceNodeEx : ITraceNodeEx ;
   c , RowCount : integer ;
   TableMembers : TMemberNode ;
begin
   if Link = nil then
      exit ;

   _ITraceNodeEx :=  Link.CreateNodeEx()  ;           // create internal nodeEx  : call TTraceNode.create (self,true);
   _ITraceNodeEx.LeftMsg := LeftMsg ;

   TableMembers := _ITraceNodeEx.Members.Add(table.GetTitle()) ;
   TableMembers.ViewerKind := CST_VIEWER_TABLE ;


   RowCount := table.RowCount ;
   for c := 0 to RowCount-1 do
      TableMembers.Add(table.GetRowData(c)) ;

   _ITraceNodeEx.Send() ;
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.SendXml(const LeftMsg, Xml: WideString);
begin
   if Link = nil then
      exit ;
   link.SendXml(LeftMsg,xml) ;
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.EnterMethod(const LeftMsg, RightMsg: WideString);
begin
   if Link = nil then
      exit ;
   link.EnterMethod(LeftMsg,RightMsg);
end;

//------------------------------------------------------------------------------

procedure TXTraceToSend.ExitMethod(const LeftMsg, RightMsg: WideString);
begin
   if Link = nil then
      exit ;
   link.ExitMethod(LeftMsg,RightMsg);
end;

//------------------------------------------------------------------------------

initialization
  TAutoObjectFactory.Create(ComServer, TXTraceToSend, Class_XTraceToSend, ciSingleInstance, tmFree);
end.
