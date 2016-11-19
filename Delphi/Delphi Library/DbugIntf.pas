// GDebug compatibility : senddebug, SendDebugEx, SendDebugClear

unit DbugIntf;

interface

uses Dialogs , Windows , sysutils, TraceTool ;

   // gdebug compatibility
   procedure SendDebug (const msg : string) ;
   procedure SendDebugEx (const Msg: string; MType: TMsgDlgType);
   procedure SendDebugClear;

procedure SendBoolean(const Identifier: string; const Value: Boolean);
procedure SendDateTime(const Identifier: string; const Value: TDateTime);
procedure SendInteger(const Identifier: string; const Value: Integer);
procedure SendMethodEnter(const MethodName: string);
procedure SendMethodExit(const MethodName: string);
procedure SendSeparator;
procedure SendDebugFmt(const Msg: string; const Args: array of const);
procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TMsgDlgType);
function  StartDebugWin: hWnd;

implementation

threadvar
  MsgPrefix: AnsiString;

const
  Indentation = '    ';

function StartDebugWin: hWnd;
begin
   result := TraceTool.StartDebugWin ;
end ;

//------------------------------------------------------------------------------

procedure senddebug (const msg : string) ;
begin
   TTrace.debug.Send(msg) ;
end ;

//------------------------------------------------------------------------------

procedure SendDebugEx(const Msg: string; MType: TMsgDlgType);
begin
   case MType of
   mtWarning     : TTrace.Warning.Send(msg) ;
   mtError       : TTrace.Error.Send(msg) ;
   mtInformation : TTrace.debug.Send(msg) ;
   end ;
end ;

//------------------------------------------------------------------------------

procedure SendDebugClear;
begin
   TTrace.ClearAll ;
end ;

//------------------------------------------------------------------------------

procedure SendDebugFmt(const Msg: string; const Args: array of const);
begin
  SendDebugEx(Format(Msg, Args), mtInformation);
end;

//------------------------------------------------------------------------------

procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TMsgDlgType);
begin
  SendDebugEx(Format(Msg, Args), MType);
end;

//------------------------------------------------------------------------------

procedure SendMethodEnter(const MethodName: string);
begin
  MsgPrefix := MsgPrefix + Indentation;
  SendDebugEx('Entering ' + MethodName, mtInformation);
end;

//------------------------------------------------------------------------------

procedure SendMethodExit(const MethodName: string);
begin
  SendDebugEx('Exiting ' + MethodName, mtInformation);

  Delete(MsgPrefix, 1, Length(Indentation));
end;

//------------------------------------------------------------------------------

procedure SendSeparator;
const
  SeparatorString = '------------------------------';
begin
  SendDebugEx(SeparatorString, mtInformation);
end;

//------------------------------------------------------------------------------

procedure SendBoolean(const Identifier: string; const Value: Boolean);
begin
  // Note: We deliberately leave "True" and "False" as
  // hard-coded string constants, since these are
  // technical terminology which should not be localised.
  if Value then
    SendDebugEx(Identifier + '= True', mtInformation)
  else
    SendDebugEx(Identifier + '= False', mtInformation);
end;

//------------------------------------------------------------------------------

procedure SendInteger(const Identifier: string; const Value: Integer);
begin
  SendDebugEx(Format('%s = %d', [Identifier, Value]), mtInformation);
end;

//------------------------------------------------------------------------------

procedure SendDateTime(const Identifier: string; const Value: TDateTime);
begin
  SendDebugEx(Identifier + '=' + DateTimeToStr(Value), mtInformation);
end;

end.
