// Author : Thierry Parent
// Version : 12.4
//
// HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
// Download :  http://sourceforge.net/projects/tracetool/
// See License.txt for license informationunit unt_XTraceOptions;

unit unt_XTraceOptions ;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, TraceToolCom_TLB, StdVcl, tracetool;

type
  TXTraceOptions = class(TAutoObject, IXTraceOptions)     //   TInterfacedObject
  protected
    function Get_ColorKind: ColorKind; safecall;
    procedure Set_ColorKind(Value: ColorKind); safecall;
    function Get_SocketHost: WideString; safecall;
    procedure Set_SocketHost(const Value: WideString); safecall;
    function Get_SendMode: SendMode; safecall;
    procedure Set_SendMode(Value: SendMode); safecall;
    function Get_SendDate: WordBool; safecall;
    procedure Set_SendDate(Value: WordBool); safecall;
    function Get_SendFunctions: WordBool; safecall;
    procedure Set_SendFunctions(Value: WordBool); safecall;
    function Get_SendProcessName: WordBool; safecall;
    function Get_SendThreadId: WordBool; safecall;
    procedure Set_SendProcessName(Value: WordBool); safecall;
    procedure Set_SendThreadId(Value: WordBool); safecall;
    function Get_SocketPort: SYSINT; safecall;
    procedure Set_SocketPort(Value: SYSINT); safecall;
    function Get_SocketUdp: WordBool; safecall;
    procedure Set_SocketUdp(Value: WordBool); safecall;
  public
    // no link is necessary
    destructor Destroy; override;
  end;

  function ConvertColor(color : integer) : integer;

var
  globalColorKind : integer ;   // BGR = $00000000;     RGB = $00000001;

implementation

uses ComServ;

//------------------------------------------------------------------------------

// return the color converted to BGR
function ConvertColor(color : integer) : integer;
var
  b,g,r : integer ;
begin
  if globalColorKind = BGR then   // like for delphi
     result := Color
  else begin // RGB                     // vb, dot net
     B := Color and $FF;
     G := Color shr 8 and $FF;
     R := Color shr 16 and $FF;
     // convert to BGR
     result := (B shl 16) + (G shl 8) + R;
  end;

end;

//------------------------------------------------------------------------------

destructor TXTraceOptions.Destroy;
begin
   //ttrace.error.send ('TXTraceOptions.Destroy', integer(pointer(self))) ;
   inherited;
end;

//------------------------------------------------------------------------------

function TXTraceOptions.Get_SocketHost: WideString;
begin
   result := TTrace.Options.SocketHost ;
end;

//------------------------------------------------------------------------------

procedure TXTraceOptions.Set_SocketHost(const Value: WideString);
begin
   TTrace.Options.SocketHost := Value ;
end;

//------------------------------------------------------------------------------

function TXTraceOptions.Get_SendMode: SendMode;
begin
   if TTrace.Options.SendMode = tmWinMsg then
      result := WinMsg   // TOleEnum
   else
      Result := Socket ;
end;

//------------------------------------------------------------------------------

procedure TXTraceOptions.Set_SendMode(Value: SendMode);
begin
   if value = WinMsg then    // TOleEnum
      TTrace.Options.SendMode := tmWinMsg
   else
      TTrace.Options.SendMode := tmAlternate ;
end;

//------------------------------------------------------------------------------

function TXTraceOptions.Get_SendDate: WordBool;
begin
   result := ttrace.Options.SendDate ;
end;

//------------------------------------------------------------------------------

procedure TXTraceOptions.Set_SendDate(Value: WordBool);
begin
   ttrace.Options.SendDate := Value ;
end;

//------------------------------------------------------------------------------

function TXTraceOptions.Get_SendFunctions: WordBool;
begin
   result := ttrace.Options.SendFunctions ;
end;

//------------------------------------------------------------------------------

procedure TXTraceOptions.Set_SendFunctions(Value: WordBool);
begin
   ttrace.Options.SendFunctions := value ;
end;

//------------------------------------------------------------------------------

function TXTraceOptions.Get_SendProcessName: WordBool;
begin
   result := ttrace.Options.SendProcessName ;
end;

//------------------------------------------------------------------------------

procedure TXTraceOptions.Set_SendProcessName(Value: WordBool);
begin
   ttrace.Options.SendProcessName := value ;
end;

//------------------------------------------------------------------------------

function TXTraceOptions.Get_SendThreadId: WordBool;
begin
   result := ttrace.Options.SendThreadId ;
end;

//------------------------------------------------------------------------------

procedure TXTraceOptions.Set_SendThreadId(Value: WordBool);
begin
   ttrace.Options.SendThreadId := value ;
end;

//------------------------------------------------------------------------------

function TXTraceOptions.Get_SocketUdp: WordBool;
begin
   result := ttrace.Options.SocketUdp ;
end;

//------------------------------------------------------------------------------

procedure TXTraceOptions.Set_SocketUdp(Value: WordBool);
begin
   ttrace.Options.SocketUdp := Value ;
end;

//------------------------------------------------------------------------------

function TXTraceOptions.Get_SocketPort: SYSINT;
begin
   result := ttrace.Options.SocketPort ;
end;

//------------------------------------------------------------------------------

procedure TXTraceOptions.Set_SocketPort(Value: SYSINT);
begin
   ttrace.Options.SocketPort := value ;
end;

//------------------------------------------------------------------------------

function TXTraceOptions.Get_ColorKind: ColorKind;
begin
   result := globalColorKind ;
end;

//------------------------------------------------------------------------------

procedure TXTraceOptions.Set_ColorKind(Value: ColorKind);
begin
   globalColorKind := Value ;
end;


initialization
  TAutoObjectFactory.Create(ComServer, TXTraceOptions, Class_XTraceOptions,  ciMultiInstance, tmApartment);
end.
