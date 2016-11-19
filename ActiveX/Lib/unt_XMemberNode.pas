// Author : Thierry Parent
// Version : 12.4
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license information

unit unt_XMemberNode;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, TraceToolCom_TLB, StdVcl, tracetool;

type
  TXMemberNode = class(TAutoObject, IXMemberNode) //  TInterfacedObject
  protected
    function Get_Col1: WideString; safecall;
    procedure Set_Col1(const Value: WideString); safecall;
    function Get_Col2: WideString; safecall;
    function Get_Col3: WideString; safecall;
    procedure Set_Col2(const Value: WideString); safecall;
    procedure Set_Col3(const Value: WideString); safecall;
    function SetFontDetail(ColId: SYSINT; Bold, Italic: WordBool; Color,
      Size: SYSINT; const FontName: WideString): IXMemberNode; safecall;
    function Add(const Col1, Col2, Col3: WideString): IXMemberNode; safecall;
    function Get_Tag: SYSINT; safecall;
    procedure Set_Tag(Value: SYSINT); safecall;
  public
    Link : TMemberNode ;   // class, not interface : not garbage collected.
    destructor Destroy; override;
  end;

implementation

uses ComServ, unt_XtraceOptions;

//------------------------------------------------------------------------------

//procedure TXMemberNode.Initialize;
//begin
//   //ttrace.error.send ('TXMemberNode.Initialize', integer(pointer(self))) ;
//
//   // the link (TMemberNode) is created by traceNodeEx and owned by traceNodeEx
//   inherited;
//end;

//------------------------------------------------------------------------------

//todo : destructor   -> link and submembers
destructor TXMemberNode.Destroy;
begin
   //ttrace.error.send ('TXMemberNode.Destroy', integer(pointer(self))) ;

   // internal link (TMemberNode) is automatically freed by the owner (TraceNodeEx)
   inherited;
end;

//------------------------------------------------------------------------------

function TXMemberNode.Get_Col1: WideString;
begin
   result := Link.Col1 ;
end;

//------------------------------------------------------------------------------

procedure TXMemberNode.Set_Col1(const Value: WideString);
begin
   Link.Col1 := Value ;
end;

//------------------------------------------------------------------------------

function TXMemberNode.Get_Col2: WideString;
begin
   result := Link.Col2 ;
end;

//------------------------------------------------------------------------------

procedure TXMemberNode.Set_Col2(const Value: WideString);
begin
   Link.Col2 := Value ;
end;

//------------------------------------------------------------------------------

function TXMemberNode.Get_Col3: WideString;
begin
   result := Link.Col3 ;
end;

//------------------------------------------------------------------------------

procedure TXMemberNode.Set_Col3(const Value: WideString);
begin
   Link.Col3 := Value ;
end;

//------------------------------------------------------------------------------

function TXMemberNode.SetFontDetail(ColId: SYSINT; Bold, Italic: WordBool;Color, Size: SYSINT; const FontName: WideString): IXMemberNode;
begin
  Link.SetFontDetail(ColId, Bold, Italic, ConvertColor(color), Size,FontName) ;
end;

//------------------------------------------------------------------------------

function TXMemberNode.Add(const Col1, Col2, Col3: WideString): IXMemberNode;
var
   subMemberWrapper : TXMemberNode ;
   subMemberInternalNode : TMemberNode ;
begin
   subMemberWrapper := TXMemberNode.Create ;            // create the wrapper.
   subMemberInternalNode := link.Add(col1,col2,col3) ;  // create internal member (garbage collected by the TMemberNode link)
   subMemberWrapper.Link := subMemberInternalNode ;     // link the internal member to the wrapper
   result := subMemberWrapper ;
end ;

//------------------------------------------------------------------------------

function TXMemberNode.Get_Tag: SYSINT;
begin
   result := Link.Tag ;
end;

//------------------------------------------------------------------------------

procedure TXMemberNode.Set_Tag(Value: SYSINT);
begin
   Link.Tag := value ;
end;

//------------------------------------------------------------------------------

initialization
  TAutoObjectFactory.Create(ComServer, TXMemberNode, Class_XMemberNode, ciMultiInstance, tmApartment);
end.
