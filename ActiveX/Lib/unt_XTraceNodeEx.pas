// Author : Thierry Parent
// Version : 12.4
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/unit unt_XTraceNodeEx;

unit unt_XTraceNodeEx ;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, TraceToolCom_TLB, StdVcl, tracetool, sysutils;

type
  TXTraceNodeEx = class(TAutoObject, IXTraceNodeEx)  //   TInterfacedObject
  protected
    function  Get_Enabled: WordBool; safecall;
    function  Get_IconIndex: SYSINT; safecall;
    function  Get_Id: WideString; safecall;
    function  Get_Members: IXMemberNode; safecall;
    function  Get_Tag: SYSINT; safecall;
    function  Resend: IXTraceNode; safecall;
    function  Send: IXTraceNode; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_IconIndex(Value: SYSINT); safecall;
    procedure Set_Id(const Value: WideString); safecall;
    procedure Set_Tag(Value: SYSINT); safecall;
    function  Get_leftMsg: WideString; safecall;
    procedure Set_leftMsg(const Value: WideString); safecall;
    function  Get_ParentNodeId: WideString; safecall;
    function  Get_RightMsg: WideString; safecall;
    procedure AddDump(const ShortTitle: WideString; Adr: PChar; Count: SYSINT); safecall;
    procedure AddFontDetail(ColId: SYSINT; Bold, Italic: WordBool; Color, Size: SYSINT; const FontName: WideString); safecall;
    procedure AddObject(Obj: OleVariant); safecall;
    procedure Set_ParentNodeId(const Value: WideString); safecall;
    procedure Set_RightMsg(const Value: WideString); safecall;
    procedure AddValue(Obj: OleVariant; const ObjTitle: WideString;MaxLevel: Integer); safecall;
    function CreateNodeEx: IXTraceNodeEx; safecall;
    function Get_WinTraceId: WideString; safecall;
    procedure Set_WinTraceId(const Value: WideString); safecall;
    procedure AddBackgroundColor(ColId, Color: SYSINT); safecall;
    procedure AddTable(var Table: IXTraceTable); safecall;
    procedure AddXml(const Xml: WideString); safecall;
  public
    Link : ITraceNodeEx ;
    destructor Destroy; override;
//    procedure Initialize; override;
  private
    fMembers : IXMemberNode ;
    SentXTraceNode : IXTraceNode ;

  end;

implementation

uses ComServ, unt_XTraceNode, unt_XMemberNode, unt_XTraceTable , unt_XTraceOptions;

//------------------------------------------------------------------------------

//procedure TXTraceNodeEx.Initialize;
//begin
//   //ttrace.error.send ('TXTraceNodeEx.Initialize', integer(pointer(self))) ;
//   inherited;
//end;

//------------------------------------------------------------------------------

destructor TXTraceNodeEx.Destroy;
begin
   //ttrace.error.send ('TXTraceNodeEx.Destroy', integer(pointer(self))) ;
   // internal members are freed when the link is freed
   Link := nil ; // release internal link (ITraceNodeEx)
   inherited;
end;

//------------------------------------------------------------------------------

function TXTraceNodeEx.Send: IXTraceNode;
var
   node : TXTraceNode ;
begin
   node := TXTraceNode.create ;
   node.Link := Link.Send() ;
   result := node ;
   SentXTraceNode := result ;          // remain the sent node for the resend funtion
end;

//------------------------------------------------------------------------------

function TXTraceNodeEx.Resend: IXTraceNode;
begin
   // SentXTraceNode can be null if resend is called without calling Send
   if SentXTraceNode = nil then
      result := Send()
   else begin
      result := SentXTraceNode ;
      Link.Resend() ;
   end ;
end;

//------------------------------------------------------------------------------

function TXTraceNodeEx.Get_WinTraceId: WideString;
begin
   result := link.WinTraceId ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.Set_WinTraceId(const Value: WideString);
begin
   link.WinTraceId := Value ;
end;

//------------------------------------------------------------------------------

function TXTraceNodeEx.Get_Enabled: WordBool;
begin
   result := Link.Enabled ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.Set_Enabled(Value: WordBool);
begin
   Link.Enabled := value ;
end;

//------------------------------------------------------------------------------

function TXTraceNodeEx.Get_leftMsg: WideString;
begin
   result := Link.LeftMsg ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.Set_leftMsg(const Value: WideString);
begin
   Link.LeftMsg := value ;
end;

//------------------------------------------------------------------------------

function TXTraceNodeEx.Get_RightMsg: WideString;
begin
   result := Link.RightMsg ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.Set_RightMsg(const Value: WideString);
begin
   Link.RightMsg := value ;
end;

//------------------------------------------------------------------------------

function TXTraceNodeEx.Get_ParentNodeId: WideString;
begin
   result := Link.ParentNodeId ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.Set_ParentNodeId(const Value: WideString);
begin
   Link.ParentNodeId := value ;
end;

//------------------------------------------------------------------------------

function TXTraceNodeEx.Get_IconIndex: SYSINT;
begin
   result := Link.IconIndex ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.Set_IconIndex(Value: SYSINT);
begin
   Link.IconIndex := value ;
end;

//------------------------------------------------------------------------------

function TXTraceNodeEx.Get_Id: WideString;
begin
   if Link <> nil then
      result := Link.id
   else
      Result := '' ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.Set_Id(const Value: WideString);
begin
   if Link <> nil then
      Link.id := value ;
end;

//------------------------------------------------------------------------------

function TXTraceNodeEx.Get_Tag: SYSINT;
begin
   result := Link.Tag ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.Set_Tag(Value: SYSINT);
begin
   Link.Tag := value ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.AddDump(const ShortTitle: WideString; Adr: PChar; Count: SYSINT);
begin
   Link.AddDump(ShortTitle,  Pointer(Adr), Count)  ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.AddFontDetail(ColId: SYSINT; Bold, Italic: WordBool; Color, Size: SYSINT; const FontName: WideString);
begin
   Link.AddFontDetail(ColId, Bold,Italic,ConvertColor(color),size,fontname)  ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.AddObject(Obj: OleVariant);
begin
   Link.AddObject(Obj)  ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.AddValue(Obj: OleVariant; const ObjTitle: WideString; MaxLevel: Integer);
begin
   Link.AddValue(Obj,ObjTitle) ;
end;

//------------------------------------------------------------------------------

function TXTraceNodeEx.Get_Members: IXMemberNode;
var
   MemberNode : TXMemberNode ;
begin
   if fMembers = nil then begin
      MemberNode := TXMemberNode.Create ;  // create public member wrapper
      MemberNode.Link := Link.Members  ;  // link existing internal members (TMemberNode) to public wrapper
      fMembers := MemberNode ;
   end ;
   result := fMembers ;
end;


//------------------------------------------------------------------------------

function TXTraceNodeEx.CreateNodeEx: IXTraceNodeEx;
var
   _ITraceNodeEx : ITraceNodeEx ;
   XTraceNodeEx : TXTraceNodeEx ;
begin
   result := nil ;

   if Link = nil then
      exit ;

   _ITraceNodeEx :=  Link.CreateNodeEx()  ;           // create internal nodeEx
   XTraceNodeEx :=  TXTraceNodeEx.Create() ;          // create public nodex wrapper
   XTraceNodeEx.Link := _ITraceNodeEx  ;              // link public to internal nodeEx
   result := XTraceNodeEx ;                           // increment ref count and return it
   //result.ParentNodeId := _ITraceNodeEx.id ;
   //result.IconIndex    := _ITraceNodeEx.IconIndex ;
   //result.Enabled      := _ITraceNodeEx.Enabled ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.AddBackgroundColor(ColId, Color: SYSINT);
begin
   link.AddBackgroundColor(ColId,ConvertColor(color));
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.AddTable(var Table: IXTraceTable);
var
   c , RowCount : integer ;
   TableMembers : TMemberNode ;
begin
//   LinkTable := TXTraceTable(Table).link ;   // not sure that will work
//   link.AddTable(LinkTable);  // will call CopyToNodeMembers

   TableMembers := link.Members.Add(table.GetTitle()) ;
   TableMembers.ViewerKind := CST_VIEWER_TABLE ;

   RowCount := table.RowCount ;
   for c := 0 to RowCount-1 do
      TableMembers.Add(table.GetRowData(c)) ;
end;

//------------------------------------------------------------------------------

procedure TXTraceNodeEx.AddXml(const Xml: WideString);
begin
   link.AddXml (Xml) ;
end;

//------------------------------------------------------------------------------

initialization
  TAutoObjectFactory.Create(ComServer, TXTraceNodeEx, Class_XTraceNodeEx, ciSingleInstance, tmFree);
end.
