unit unt_Decode;

interface

Uses
  Classes, SysUtils, Graphics;

function Decode(data: pointer; len: integer) : AnsiString; overload;
function Decode(data: AnsiString) : AnsiString; overload;
function HTMLToDelphiColor(S: String): TColor;
Function SubStrCount(Substr, MainStr: String): Integer;


implementation

Uses
  Windows;

type
  TAByte         = array [0..maxInt      -1] of byte;
  TPAByte        = ^TAByte;

Function SubStrCount(Substr, MainStr: String): Integer;
Var
  Count, P: Integer;
Begin
  Count := 0;
  While Pos(SubStr, MainStr) <> 0 Do Begin
    P := Pos(SubStr, MainStr);
    Delete(MainStr, 1, P + Length(SubStr)-1);
    Inc(Count);
  End;
  Result := Count;
End;

// decode functions from MadCrypt
function Decode(data: pointer; len: integer) : AnsiString; overload;
var i1, i2 : integer;
    pi, po : TPAByte;
    ch1    : AnsiChar;
    c1     : dword;
begin
  if (len > 0) and (len mod 4 = 0) then begin
    len := len shr 2;
    SetLength(result, len * 3);
    pi := pointer(data);
    po := pointer(result);
    for i1 := 1 to len do begin
      c1 := 0;
      i2 := 0;
      while true do begin
        ch1 := AnsiChar(pi^[i2]);
        case ch1 of
          'A'..'Z' : c1 := c1 or (dword(ch1) - byte('A')     );
          'a'..'z' : c1 := c1 or (dword(ch1) - byte('a') + 26);
          '0'..'9' : c1 := c1 or (dword(ch1) - byte('0') + 52);
          '+'      : c1 := c1 or 62;
          '/'      : c1 := c1 or 63;
          else       begin
                       if i2 = 3 then begin
                         po^[0] := c1 shr 16;
                         po^[1] := byte(c1 shr 8);
                         SetLength(result, Length(result) - 1);
                       end else begin
                         po^[0] := c1 shr 10;
                         SetLength(result, Length(result) - 2);
                       end;
                       exit;
                     end;
        end;
        if i2 = 3 then
          break;
        inc(i2);
        c1 := c1 shl 6;
      end;
      po^[0] := c1 shr 16;
      po^[1] := byte(c1 shr 8);
      po^[2] := byte(c1);
      inc(NativeUInt(pi), 4);
      inc(NativeUInt(po), 3);
    end;
  end else
    result := '';
end;

//------------------------------------------------------------------------------

function Decode(data: AnsiString) : AnsiString;
begin
  result := Decode(pAnsiChar(data), Length(data));
end;

//------------------------------------------------------------------------------
function HTMLToDelphiColor(S: String): TColor;
var
  Red, Green, Blue: LongInt;
begin
  Red := StrToInt('$' + Copy(S, 1, 2));
  Green := StrToInt('$' + Copy(S, 3, 2));
  Blue := StrToInt('$' + Copy(S, 5, 2));
  Result := (Blue shl 16) + (Green shl 8) + Red;
end;

end.
