unit DesktopPos;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFDesktopSaver = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FDesktopSaver: TFDesktopSaver;

implementation

{$R *.dfm}

uses Registry, madShell, madStrings, madTypes;

procedure SaveDesktopPositions;
var s1  : string;
    i1  : integer;
    p1  : TPoint;
    hk1 : HKEY;
begin
  with Desktop do
    if IsValid then begin
      s1 := '';
      for i1 := 0 to ItemCount - 1 do begin
        p1 := Items[i1].Position;
        if p1.x <> -1 then
          s1 := s1 + Items[i1].Description + ' ' + IntToStrEx(p1.x, 4) +
                                             '/' + IntToStrEx(p1.y, 4) + #$D#$A;
      end;
      if RegCreateKeyExA(HKEY_CURRENT_USER, 'Software\madshi\madShell', 0, nil, 0, KEY_ALL_ACCESS, nil, hk1, nil) = 0 then begin
        RegSetValueExA(hk1, 'Desktop Positions', 0, REG_BINARY, PAnsiChar(s1), Length(s1) + 1);
        RegCloseKey(hk1);
      end;
    end;
end;

function RestoreDesktopPositions : boolean;

  function GetFreePos : TPoint;
  var i1  : integer;
      wnd : HWND;
  begin
    CloseHandle(CreateFileA(PAnsiChar(ShellObj(sfDesktopDir).Path + '\%dummy%'), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0));
    wnd := FindWindowEx(FindWindowEx(windows.FindWindow('Progman','Program Manager'), 0, 'SHELLDLL_DefView', ''), 0, 'SysListView32', nil);
    SendMessage(wnd, WM_NULL, 0, 0);
    i1 := 0;
    repeat
      if i1 > 0 then begin
        Sleep(50);
        Application.ProcessMessages;
      end;
      inc(i1);
      result := Desktop.Item('%dummy%').Position;
    until (result.X <> -1) or (i1 = 100);
    DeleteFileA(PAnsiChar(ShellObj(sfDesktopDir).Path + '\%dummy%'));
    SendMessage(wnd, WM_NULL, 0, 0);
    i1 := 0;
    repeat
      if i1 > 0 then begin
        Sleep(50);
        Application.ProcessMessages;
      end;
      inc(i1);
    until (Desktop.Item('%dummy%').Position.X = -1) or (i1 = 100);
    for i1 := 0 to 20 do begin
      Sleep(50);
      Application.ProcessMessages;
      SendMessage(wnd, WM_NULL, 0, 0);
    end;
  end;

  procedure AddPos(var poss: TDAInteger; pos: integer);
  var b1 : boolean;
      i1 : integer;
  begin
    if pos >= 0 then begin
      b1 := true;
      for i1 := 0 to high(poss) do
        if poss[i1] = pos then begin
          b1 := false;
          break;
        end;
      if b1 then begin
        SetLength(poss, Length(poss) + 1);
        for i1 := high(poss) downto 1 do
          if poss[i1 - 1] < pos then begin
            poss[i1] := pos;
            b1 := false;
            break;
          end else
            poss[i1] := poss[i1 - 1];
        if b1 then
          poss[0] := pos;
      end;
    end;
  end;

  procedure InterpretPos(const poss: TDAInteger; out offset, dif: integer);
  var i1 : integer;
  begin
    dif := maxInt;
    for i1 := 1 to high(poss) do
      if poss[i1] - poss[i1 - 1] < dif then
        dif := poss[i1] - poss[i1 - 1];
    offset := poss[0];
    while offset > dif do
      offset := offset - dif;
  end;

var s1, s2     : string;
    i1, i2, i3 : integer;
    targetPos  : TPoint;
    currentPos : TPoint;
    freePos    : TPoint;
    first      : boolean;
    hk1        : HKEY;
    size       : integer;
    wnd        : HWND;
    srcPosX,    srcPosY    : TDAInteger;
    dstPosX,    dstPosY    : TDAInteger;
    srcXOffset, srcYOffset : integer;
    dstXOffset, dstYOffset : integer;
    srcXDif,    srcYDif    : integer;
    dstXDif,    dstYDif    : integer;
begin
  srcPosX := nil;
  srcPosY := nil;
  dstPosX := nil;
  dstPosY := nil;
  result := false;
  first := true;
  wnd := FindWindowEx(FindWindowEx(windows.FindWindow('Progman','Program Manager'), 0, 'SHELLDLL_DefView', ''), 0, 'SysListView32', nil);
  if RegOpenKeyExA(HKEY_CURRENT_USER, 'Software\madshi\madShell', 0, KEY_QUERY_VALUE, hk1) = 0 then begin
    size := 0;
    RegQueryValueExA(hk1, 'Desktop Positions', nil, nil, nil, @size);
    if size > 0 then begin
      inc(size);
      SetLength(s1, size);
      if RegQueryValueExA(hk1, 'Desktop Positions', nil, nil, pointer(s1), @size) = 0 then begin
        with Desktop do
          for i1 := 0 to ItemCount - 1 do begin
            targetPos := Items[i1].Position;
            AddPos(srcPosX, targetPos.x);
            AddPos(srcPosY, targetPos.y);
          end;
        i1 := PosStr(#$D#$A, s1);
        i2 := PosStr('/', s1, 1, i1);
        while i1 <> 0 do begin
          if i2 <> 0 then begin
            s2 := Copy(s1, 1, i2 - 6);
            AddPos(dstPosX, StrToIntEx(false, PAnsiChar(@s1[i2 - 4]), 4));
            AddPos(dstPosY, StrToIntEx(false, PAnsiChar(@s1[i2 + 1]), 4));
          end;
          Delete(s1, 1, i1 + 1);
          i1 := PosStr(#$D#$A, s1);
          i2 := PosStr('/', s1, 1, i1);
        end;
        InterpretPos(srcPosX, srcXOffset, srcXDif);
        InterpretPos(srcPosY, srcYOffset, srcYDif);
        InterpretPos(dstPosX, dstXOffset, dstXDif);
        InterpretPos(dstPosY, dstYOffset, dstYDif);
        SetLength(s1, size);
        RegQueryValueExA(hk1, 'Desktop Positions', nil, nil, pointer(s1), @size);
        i1 := PosStr(#$D#$A, s1);
        i2 := PosStr('/', s1, 1, i1);
        while i1 <> 0 do begin
          if i2 <> 0 then begin
            s2 := Copy(s1, 1, i2 - 6);
            targetPos.x := StrToIntEx(false, PAnsiChar(@s1[i2 - 4]), 4);
            targetPos.y := StrToIntEx(false, PAnsiChar(@s1[i2 + 1]), 4);
            if (srcXDif > 0) and (dstXDif > 0) and (abs(srcXDif - dstXDif) < srcXDif div 5) then
              targetPos.x := ((targetPos.x - dstXOffset) div dstXDif) * srcXDif + srcXOffset;
            if (srcYDif > 0) and (dstYDif > 0) and (abs(srcYDif - dstYDif) < srcYDif div 5) then
              targetPos.y := ((targetPos.y - dstYOffset) div dstYDif) * srcYDif + srcYOffset;
            with Desktop do
              for i2 := 0 to ItemCount - 1 do
                if Items[i2].Description = s2 then begin
                  currentPos := Items[i2].Position;
                  if int64(targetPos) <> int64(currentPos) then begin
                    result := true;
                    if first then begin
                      first := false;
                      freePos := GetFreePos;
                    end;
                    if freePos.X > -1 then begin
                      for i3 := 0 to ItemCount - 1 do
                        if (i3 <> i2) and (int64(Items[i3].Position) = int64(targetPos)) then begin
                          Items[i3].SetPosition(freePos);
                          SendMessage(wnd, WM_NULL, 0, 0);
                          break;
                        end;
                      Items[i2].SetPosition(targetPos);
                      SendMessage(wnd, WM_NULL, 0, 0);
                      freePos := currentPos;
                    end else begin
                      Items[i2].SetPosition(targetPos);
                      SendMessage(wnd, WM_NULL, 0, 0);
                    end;
                  end;
                  break;
                end;
          end;
          Delete(s1, 1, i1 + 1);
          i1 := PosStr(#$D#$A, s1);
          i2 := PosStr('/', s1, 1, i1);
        end;
      end;
    end;
    RegCloseKey(hk1);
  end;
end;

procedure TFDesktopSaver.Button1Click(Sender: TObject);
begin
  SaveDesktopPositions;
end;

procedure TFDesktopSaver.Button2Click(Sender: TObject);
begin
  RestoreDesktopPositions;
end;

end.
