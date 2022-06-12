unit ConfForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFStackTraceConfuser = class(TForm)
    ActionBtn: TButton;
    ConfuseRawTracerCheck: TCheckBox;
    ConfuseStackFrameTracerCheck: TCheckBox;
    procedure ActionBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure RaiseException;
  end;

var FStackTraceConfuser: TFStackTraceConfuser;

implementation

{$R *.dfm}

procedure TFStackTraceConfuser.FormKeyPress(Sender: TObject; var Key: Char);
// escape -> close
begin
  if Key = #27 then begin
    Key := #0;
    Close;
  end;
end;

procedure FillArrayWithAfterCalls(var dwordArr: array of dword);
// fill the specified array with valid *looking* call stack items

  function CouldBeAfterCall(ai: pointer) : boolean;
  // heuristic test whether the specified "afterInstr" might be
  // an instruction after an asm "call" instruction
  var pc4, pc8 : ^cardinal;
      c4, c8   : cardinal;
  begin
    pc4 := pointer(dword(ai) - 4);
    pc8 := pointer(dword(ai) - 8);
    c8 := pc8^;
    c4 := pc4^;
    result := ((c8 and $FF000000) = $E8000000) or
              ((c4 and $30FF0000) = $10FF0000) or
              ((c4 and $0030FF00) = $0010FF00) or
              ((c4 and $000030FF) = $000010FF) or
              ((c8 and $30FF0000) = $10FF0000) or
              ((c8 and $0030FF00) = $0010FF00) or
              ((c8 and $0000FF00) = $00009A00);
  end;

var c1, c2 : dword;
begin
  c1 := 0;
  c2 := high(dwordArr);
  repeat
    if CouldBeAfterCall(pointer(dword(@System.TextStart) + c1)) then begin
      dwordArr[c2] := dword(@System.TextStart) + c1;
      dec(c2);
    end;
    inc(c1);
  until c2 = 0;
end;

procedure TFStackTraceConfuser.RaiseException;
var dwordArr : array [1..10000] of dword;
begin
  ZeroMemory(@dwordArr, sizeOf(dwordArr));
  if ConfuseRawTracerCheck.Checked then
    // we want to confuse raw stackers, so we fill our local array
    // if it zeroed, it won't confuse raw tracers
    FillArrayWithAfterCalls(dwordArr);
  raise Exception.Create('Just an exception.');
end;

procedure TFStackTraceConfuser.ActionBtnClick(Sender: TObject);
begin
  if ConfuseStackFrameTracerCheck.Checked then begin
    {$ifdef WIN64}
      RaiseException;
    {$else}
      // to confuse stack frame tracers, we simulate the usage of the
      // ebp register for other purposes than ebp stack framing
      // such a thing breaks the stack frame chain, yes, it can happen
      // e.g. EnumWindows in win9x does that
      asm
        mov eax, self
        push ebp
        mov ebp, 0
        call RaiseException
        pop ebp
      end;
    {$endif}
  end else
    RaiseException;
end;

end.
