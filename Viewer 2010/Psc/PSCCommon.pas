{*****************************************************************************
  Name           : PSCCommon
  Author         : POL OFFICE COMMON
  Description    : Common functions for PSCControls library
  History        :

  Date         By                   Description
  ----         --                   -----------
  05-10-2005   POL OFFICE COMMON     Initial creation of the Unit.
 *****************************************************************************}

{$I PSCControls.inc}
{$R-}

unit PSCCommon;

interface
uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Graphics,
  TypInfo,
  ImgList,
  ExtCtrls,
  Commctrl,
  ShellAPI,
  SHLObj,
  ActiveX;


const
  CS_DROPSHADOW = $00020000;
  
const
 AnsiCrLf = #13#10;
 AnsiTab = #9;

 ROP_DSPDxax = $00E20746;


//  Epoch is the earliest date calendar supports
//  Armageddon is the latest date calendar supports
//
//  Epoch is 1-jan-1753
//  Armageddon is 31-dec-9998
//

const
  Epoch : TSystemTime = (wYear : 1753;  wMonth : 1; wDayOfWeek : 0; wDay : 1; wHour: 0; wMinute: 0; wSecond : 0; wMilliseconds :0 );
  Armageddon : TSystemTime = ( wYear : 9998; wMonth :12; wDayOfWeek : 4; wDay : 31; wHour:23; wMinute: 59; wSecond:59; wMilliseconds: 999 );
  EmptyDateTime : TSystemTime = (wYear : 0;  wMonth : 0; wDayOfWeek : 0; wDay : 0; wHour: 0; wMinute: 0; wSecond : 0; wMilliseconds :0 );


type
  PLeadByteSet = ^TLeadByteSet;
  TLeadByteSet = set of Char;

  NotEmplementedException = class(Exception)
  public
    constructor Create;
  end;

  {$IFDEF VERSION5ONLY}
  TLabelPosition = (lpAbove, lpBelow, lpLeft, lpRight);
  {$ENDIF}

  IBoundLabel = interface(IUnknown)
  ['{1B272C12-8D0F-42E0-9E64-C12258C543B2}']
  procedure SetLabelPosition(const Value: TLabelPosition);
  function GetLabelPosition: TLabelPosition;
  function GetTranslationID : integer;
  procedure SetTranslationID(const Value: integer);
  function GetTranslationSupport : IUnknown;
  end;

  TPSCTimeInc = (tiYear, tiMonth, tiWeek, tiDay, tiHour, tiMinute, tiSecond);

  //About property type for all PSC components
  TPSCAboutInfo = (PSCComponent);

  IPSCControlCenter = interface(IUnknown)
  ['{AF5A3B1F-E914-4296-9386-9ACBE0D17C12}']
  end;

  IPSCControlCenterCallback = interface(IUnknown)
  ['{2638D735-7BAC-4ECB-9246-6A5D58288F06}']
  procedure UpdateLookFeel;
  end;

  TPSCLanguage = (lgNONE, lgDUTCH, lgFRENCH, lgGERMAN, lgENGLISH);
  

type
 TPSCCharSet = set of char;

const
  PSCBrackets = ['(', ')', '[', ']', '{', '}'];
  PSCStdWordDelims = [#0..' ', ',', '.', ';', '/', '\', ':', '''', '"', '`'] + PSCBrackets;
  PSCstdWordBegins = PSCstdWordDelims + ['&'];

const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 75;
  mcButtonHeight = 25;
  mcFormSpacing=6;
  mcButtonSpacing = 4;
  mcSpaceBeforeLabel=4;
  mcSpaceAfterLabel=4;
  mcMinWidth=150;
  mcMinHeight=75;
  mcMinCmbBxWidth=120;
  // mcLblHeight=Height of the blue label plcaed on the top panel
  mcLblHeight=23;
  // Type kinds for use with tiGetPropertyNames
  // All string type properties
  ctkString = [ tkChar, tkString, tkWChar, tkLString, tkWString ] ;
  // Integer type properties
  ctkInt    = [ tkInteger, tkInt64 ] ;
  // Float type properties
  ctkFloat  = [ tkFloat ] ;
  // Numeric type properties
  ctkNumeric = [tkInteger, tkInt64, tkFloat];
  // All simple types (string, int, float)
  ctkSimple = ctkString + ctkInt + ctkFloat ;


function  GrayColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
procedure GrayBitmap(ABitmap: TBitmap; Value: integer);
function  NewColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
procedure FillRectColor(HDC: THandle; R: TRect; clr: TColor);
procedure DimBitmap(ABitmap: TBitmap; Value: integer);
function  GetShadeColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
procedure DrawBitmapShadow(B: TBitmap; ACanvas: TCanvas; X, Y: integer;
  ShadowColor: TColor);
procedure GetSystemMenuFont(Font: TFont);
procedure DrawArrow(DC : THandle; X, Y: Integer; Vertical: Boolean);

procedure KillMessage(Wnd: HWnd; Msg: Integer);

function  PadString(const AValue : string; ANumberOfDigits : integer) : string;
function  PadNumber(const AValue : integer; ANumberOfDigits : integer) : string;
function  BooleanToStr(B: Boolean): AnsiString;
function  StringToBoolean(AStr : string) : boolean;

function  GetPropertyValue(Instance: TPersistent; const PropName: string): TPersistent;
procedure GetPropertyNames(Instance: TPersistentClass; StrgLst:TStringList;
                                 pPropFilter:TTypeKinds=ctkSimple);overload;
procedure GetPropertyNames(Instance:TPersistent;StrgLst:TStringList;
                                 pPropFilter:TTypeKinds=ctkSimple);overload;


//PSCMessenger support
function MakeLangID(PrimaryLangID, SubLangID: Word): Word;
function PSCAnsiUpperCase(sSource : String) : string;
function AnsiProperCase(const S: string; const WordDelims: TPSCCharSet): string;
function CapitalizeAllWords(S : string) : string;

// procedures for ControlCenter Support
procedure AddControlToList(AControl : TComponent);
procedure RemoveControlFromList(AControl : TComponent);

function GetNextPointSeparatedToken(var Path: String; const Separator : char = '.'): String;
function FindParentForm(Control: TControl): TCustomForm;

procedure DivMod(Dividend: Integer; Divisor: Word;  var Result, Remainder: Word);

procedure InternalDecodeDate(Date: TDateTime; var Year, Month, Day, DOW: Word);

function GetDaysForMonth(yr : word; mo : word) : integer;
function IsValidDate(st : TSystemTime) : boolean;
function IsValidTime(st : TSystemTime) : boolean;
function IsValidSystemTime(st : TSystemTime) : boolean;
function GetStartDowForMonth(const yr : word;  const mo : word) : integer;
function GetWeekNumber(pst : TSystemTime; dowFirst : integer) : integer;
function DaysBetweenDates(StartDate, EndDate : TSystemTime) : DWORD;
function CmpDate(Date1, Date2 : TSystemTime) : integer;
function CmpSystemTime(Time1, Time2 : TSystemTime) : integer;
function IncSystemTime(const AValue : TSystemTime; ADelta : longint;  Flag : TPSCTimeInc) : TSystemTime;
function IncDateTime(const AValue : TDateTime; ADelta : longint; Flag : TPSCTimeInc) : TDateTime;
function DowFromDate(pst : TSystemTime) : integer;
function IsZeroDate(AValue  : TDate) : boolean;
function IsZeroDateTime(AValue : TDate) : boolean;

function IncHour(const AValue: TDateTime; const ANumberOfHours: integer = 1 ): TDateTime;
function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: integer = 1 ): TDateTime;
function IncSecond(const AValue: TDateTime;  const ANumberOfSeconds: integer = 1): TDateTime;

function IncDay(const AValue : TDateTime; const ANumberOfDays : integer = 1) : TDateTime;
function IncMonth(const AValue : TDateTime; const ANumberOfMonths : integer = 1) : TDateTime;
function IncYear(const AValue : TDateTime; const ANumberOfYears : integer = 1) : TDateTime;

function ReplaceSystemDate(const Source : TSystemTime; NewValue : TSystemTime) : TSystemTime;
function ReplaceSystemTime(const Source : TSystemTime; NewValue : TSystemTime) : TSystemTime;

procedure ImageListDrawDisabled(Images: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; HighlightColor, GrayColor: TColor; DrawHighlight: Boolean);


//strings & stream
procedure WriteString(Stream : TStream; const St : string);

procedure WriteStrings(Stream : TStream; const StA : array of string);

procedure Swap(var X : integer; var Y : integer);
procedure FlipRect(var R : TRect);
procedure VertDrawEdge(HDC : THandle; qrc : TRect; EdgeType : integer;  grfFlags : integer; fVert : boolean);

procedure DrawTransparentBitmap(DC: HDC; hBmp : HBITMAP ;
          xStart: integer; yStart : integer; cTransparentColor : COLORREF);

function GetAveCharSize(Canvas: TCanvas): TPoint;
function Max3(I, J,K: Integer): Integer;
procedure CenterWindow(Wnd: HWnd);
procedure PlaceForm(Frm:TForm; X,Y:integer);

procedure InternalError(S : string);
function ExpandConst(const S: String): String;
function ExpandConstEx(const S: String; const CustomConsts: array of String): String;

function DirectoryExists(const Directory: string): Boolean;

function SelectDirectory(const Caption: string; const Root: WideString;
  var Directory: string): Boolean;

function PathIsURL(pszPath: string): BOOL;

function LanguageToString(ALanguage : TPSCLanguage) : string;

procedure RunFile(const FName: string; Handle: THandle = 0;
  const Param: string = '');

procedure OpenUrl(const Url: string);

function RectWidth(R : TRect) : integer;
function RectHeight(R : TRect) : integer;

var
 ControlCenterList : TList = nil;
var
  ConstLeadBytes: PLeadByteSet = nil;
  LeadBytes : TLeadByteSet;

implementation

const
  Accum : array[0..12] of integer =
 ( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 );


function LIncWord(var pn : word; ADelta : longint;  AMin,  AMax : integer) : longint;
var
 lNew, lIncr : longint;
begin
  lIncr := 0;
  lNew  := pn + ADelta;

  while (lNew >= AMax) do
   begin
     lNew := lNew - (AMax - AMin);
     inc(lIncr);
   end;

   if (lIncr = 0 ) then
    begin
      while (lNew < AMin) do
       begin
         lNew := lNew + (AMax - AMin);
         dec(lIncr)
       end;
     end;

    pn := WORD(lNew);

    Result := lIncr;
end;

function ZeroString(N: Integer): string;
begin
  if N < 1 then
    Result := ''
      else
       begin
         SetLength(Result, N);
         FillChar(Result[1], Length(Result), ord('0'));
       end;
end;


function GetPropertyValue(Instance: TPersistent; const PropName: string): TPersistent;
var
  PropInfo: PPropInfo;
begin
  Result := nil;
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, PropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
    Result := TObject(GetOrdProp(Instance, PropInfo)) as TPersistent;
end;

function GrayColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
var
  r, g, b, avg: integer;
  clrRef : COLORREF;
begin
  if Value > 100 then
    Value := 100;
  clrRef := ColorToRGB(clr);

  r := ClrRef and $000000FF;
  g := (ClrRef and $0000FF00) shr 8;
  b := (ClrRef and $00FF0000) shr 16;

  Avg := (r + g + b) div 3;
  Avg := Avg + Value;

  if Avg > 240 then Avg := 240;

  Result := Windows.GetNearestColor (ACanvas.Handle,RGB(Avg, avg, avg));
end;

procedure GrayBitmap(ABitmap: TBitmap; Value: integer);
var
  x, y: integer;
  LastColor1, LastColor2, Color: TColor;
begin
  LastColor1 := 0;
  LastColor2 := 0;

  for y := 0 to ABitmap.Height do
    for x := 0 to ABitmap.Width do
    begin
      Color := ABitmap.Canvas.Pixels[x, y];
      if Color = LastColor1 then
        ABitmap.Canvas.Pixels[x, y] := LastColor2
      else
      begin
        LastColor2 := GrayColor(ABitmap.Canvas , Color, Value);
        ABitmap.Canvas.Pixels[x, y] := LastColor2;
        LastColor1 := Color;
      end;
    end;
end;


procedure KillMessage(Wnd: HWnd; Msg: Integer);
// Delete the requested message from the queue, but throw back
// any WM_QUIT msgs that PeekMessage may also return
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;


function PadString(const AValue : string; ANumberOfDigits: integer): string;
begin
  if Length(AValue) < ANumberOfDigits then
   Result :=  ZeroString(ANumberOfDigits - Length(AValue)) + AValue
    else
      Result := AValue;
end;

function PadNumber(const AValue : integer;  ANumberOfDigits: integer): string;
begin
  if AValue < 0 then
   Result := '-' + PadString( IntToStr(Abs(AValue)), ANumberOfDigits)
    else
       Result := PadString(IntToStr(AValue), ANumberOfDigits);
end;


Function PSCAnsiUpperCase(sSource : String) : String;
var
 i, iOrd, L : integer ;
 sChar : Char;
begin
  L := length(sSource);
  if L = 0 then
   Exit;

  SetLength(Result, L);
  for i := 1 to L do
   begin
     sChar := sSource[i] ;
     iOrd  := Ord(sChar);
     if      (iOrd in [224..229]) then sChar := 'A'
     else if (iOrd in [232..235]) then sChar := 'E'
     else if (iOrd in [236..239]) then sChar := 'I'
     else if (iOrd in [242..246]) then sChar := 'O'
     else if (iOrd in [249..252]) then sChar := 'U'
     else if (iOrd = 39)          then sChar := Chr(180)
     else if (iOrd = 231)         then schar := Chr(67)
     else sChar := UpCase(sChar);
     Result[i] := sChar;
  end;
end;



function AnsiProperCase(const S: string; const WordDelims: TPSCCharSet): string;
var
  SLen, I: Cardinal;
begin
  Result := AnsiLowerCase(S);
  I := 1;
  SLen := Length(Result);
  while I <= SLen do
  begin
    while (I <= SLen) and (Result[I] in PSCstdWordBegins) do
      Inc(I);
    if I <= SLen then
      Result[I] := AnsiUpperCase(Result[I])[1];
    while (I <= SLen) and not (Result[I] in WordDelims) do
      Inc(I);
  end;
end;


function CapitalizeAllWords(S : string) : string;
begin
  Result := AnsiProperCase(S, PSCStdWordDelims);
end;


function MakeLangID(PrimaryLangID, SubLangID: Word): Word;
begin
  Result := (SubLangID shl 10) or PrimaryLangID;
end;

procedure AddControlToList(AControl : TComponent);
begin
  if (csDesigning in AControl.ComponentState) then
   Exit;

  if AControl = nil then
   Exit;

  if not Assigned(ControlCenterList) then
   ControlCenterList := TList.Create;

  if (ControlCenterList.IndexOf(AControl) < 0) then
   begin
     ControlCenterList.Add(AControl);
   end;
end;

procedure RemoveControlFromList(AControl : TComponent);
begin
  if (csDesigning in AControl.ComponentState) then
   Exit;

   if ControlCenterList <> nil then
    begin
       if ControlCenterList.IndexOf(AControl) > -1 then
        begin
          ControlCenterList.Remove(AControl);
        end;
    end;
end;


function GetNextPointSeparatedToken(var Path: String; const Separator : char = '.'): String;
var
  PPos: Integer;
begin
  PPos := Pos(Separator, Path);
  if PPos > 0 then
    begin
     Result := Copy(Path, 1, PPos-1);
     Delete(Path, 1, PPos);
    end
    else
      begin
        Result := Path;
        Path := '';
      end;
end;


function FindParentForm(Control: TControl): TCustomForm;
begin
 if Control.Parent is TCustomForm then
  Result := TCustomForm(Control.parent) else
    begin
     while Control.Parent <> nil do
      begin
        Control := Control.Parent;
        if Control is TCustomForm then
         break;
      end;
     if Control is TCustomForm then
       Result := TCustomForm(Control) else
         Result := nil;
     end;
end;


//from sysutils

procedure DivMod(Dividend: Integer; Divisor: Word;
  var Result, Remainder: Word);
asm
        PUSH    EBX
        MOV     EBX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        DIV     BX
        MOV     EBX,Remainder
        MOV     [ECX],AX
        MOV     [EBX],DX
        POP     EBX
end;

// from sysutils
procedure InternalDecodeDate(Date: TDateTime; var Year, Month, Day, DOW: Word);
const
  D1 = 365;
  D4 = D1 * 4 + 1;
  D100 = D4 * 25 - 1;
  D400 = D100 * 4 + 1;
var
  Y, M, D, I: Word;
  T: Integer;
  DayTable: PDayTable;
begin
  T := DateTimeToTimeStamp(Date).Date;
  if T <= 0 then
  begin
    Year := 0;
    Month := 0;
    Day := 0;
    DOW := 0;
  end else
  begin
    DOW := T mod 7;
    Dec(T);
    Y := 1;
    while T >= D400 do
    begin
      Dec(T, D400);
      Inc(Y, 400);
    end;
    DivMod(T, D100, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D100);
    end;
    Inc(Y, I * 100);
    DivMod(D, D4, I, D);
    Inc(Y, I * 4);
    DivMod(D, D1, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D1);
    end;
    Inc(Y, I);
    DayTable := @MonthDays[IsLeapYear(Y)];
    M := 1;
    while True do
    begin
      I := DayTable^[M];
      if D < I then Break;
      Dec(D, I);
      Inc(M);
    end;
    Year := Y;
    Month := M;
    Day := D + 1;
  end;
end;


function GetDaysForMonth(yr : word; mo : word) : integer;
var
  cdy : integer;
begin
  Result := 0;
  if mo < 1 then
   Exit;
  if mo > 12 then
   Exit;


  if ((yr = 1752) and (mo = 9)) then
   begin
     Result := 19;
     Exit;
   end;

  cdy := Accum[mo] - Accum[mo - 1];
  if ( (mo = 2) and ((yr and 03) = 0) and ( (yr <= 1750) or (yr mod 100 <> 0) or (yr mod 400 = 0))) then
   inc(cdy);

 Result := cdy;

end;


function IsValidDate(st : TSystemTime) : boolean;
var
 cDay : integer;
begin
  Result := false;
  if (st.wMonth >= 1) and (st.wMonth <= 12) then
   begin
     cDay := GetDaysForMonth(st.wYear, st.wMonth);
     if (st.wDay >= 1) and (st.wDay <= cDay) then
       Result := true;
   end;
end;

function IsValidTime(st : TSystemTime) : boolean;
begin
  Result := (st.wHour <= 23) and (st.wMinute <= 59) and (st.wSecond <= 59) and (st.wMilliseconds < 1000);
end;

function IsValidSystemTime(st : TSystemTime) : boolean;
var
 cDay : integer;
begin
  Result := false;
  if (st.wMonth >= 1) and (st.wMonth <= 12) then
   begin
     cDay := GetDaysForMonth(st.wYear, st.wMonth);
    if ( (st.wDay >= 1)  and
         (st.wDay <= cDay) and
         (st.wHour <= 23) and
         (st.wMinute <= 59) and
         (st.wSecond <= 59) and
         (st.wMilliseconds < 1000)) then
           Result := true
   end;
end;

function GetStartDowForMonth(const yr : word;  const mo : word) : integer;
var
 dow : integer;
begin
  dow := 5 + (yr - 1) + ((yr - 1) shr 2);
    if (yr > 1752) then
        dow := dow + round(((yr - 1) - 1600) / 400 - ((yr - 1) - 1700) / 100 - 11)
    else if ( (yr = 1752) and (mo > 9)) then
        dow := dow - 11;
    dow := dow + Accum[mo - 1];
    if ( (mo > 2) and ( (yr and 03) = 0) and
       ( (yr <= 1750) or ( yr mod 100 <> 0) or ( yr mod 400 = 0) ) ) then
        inc(dow);
    dow := dow mod 7;

    Result := dow;
end;

function GetWeekNumber(pst : TSystemTime; dowFirst : integer) : integer;
var
 day, ddow, ddowT, nweek : integer;
 st : TSystemTime;
begin
  st.wYear := pst.wYear;
  st.wMonth := 1;
  st.wDay := 1;

  ddow := GetStartDowForMonth(st.wYear, st.wMonth) - dowFirst;
  if (ddow < 0) then
    ddow  := ddow + 7;

  if ( (pst.wMonth = 1) and (pst.wDay < 8 - ddow) ) then
   begin
     nweek := 0;
   end
    else
        begin
          if (ddow <> 0 ) then
            st.wDay := 8 - ddow;
           nweek := (DaysBetweenDates(st, pst) div 7) + 1;
        end;

    if ( (ddow <> 0) and (ddow <= 3)) then
        inc(nweek);

    // adjust if necessary for calendar
    if (nweek = 0) then
        begin
        if (ddow = 0) then
         begin
            Result := 1;
            Exit;
         end;

        // check what week Dec 31 is on
        Dec(st.wYear);
        st.wMonth := 12;
        st.wDay := 31;
        Result := GetWeekNumber(st, dowFirst);
        Exit;
        end
    else if (nweek >= 52) then
        begin
        ddowT := (GetStartDowForMonth(pst.wYear, pst.wMonth) +
                    pst.wDay - 1 + 7 -    dowFirst) mod 7;
        day := pst.wDay + (7 - ddowT);
        if (day > 31 + 4) then
            nweek := 1;
        end;

    Result := nWeek;
end;

function DaysBetweenDates(StartDate, EndDate : TSystemTime) : DWORD;
var
 cday : longint;
 yr   : longint;
 tmpDate : TSystemTime;
begin
  if cmpDate(StartDate, EndDate) > 0 then
   begin
     tmpDate := StartDate;
     StartDate := EndDate;
     EndDate := tmpDate;
   end;

    cday := Accum[EndDate.wMonth - 1] - Accum[StartDate.wMonth - 1] +
             EndDate.wDay - StartDate.wDay;
    yr := StartDate.wYear;

    if ( ((yr < EndDate.wYear) or (StartDate.wMonth <= 2)) and
         (EndDate.wMonth > 2) and
         ((EndDate.wYear and 03) = 0) and
         ((EndDate.wYear <= 1750) or (EndDate.wYear mod 100 <> 0) or (EndDate.wYear mod 400 = 0))) then
         inc(cday);

    if (yr < EndDate.wYear) then
        begin
        // If the start date is before march and the start year is
        // a leap year then add an extra day to account for Feb. 29.
        if ( (StartDate.wMonth <= 2) and
            ((yr and 03) = 0) and
            ((yr <= 1750) or ( yr mod 100 <> 0) or (yr mod 400 = 0))) then
             inc(cDay);

        // Account for the days in each year (disregarding leap years).
        cday := cDay + 365;
        inc(yr);

        // Keep on accounting for the days in each year including leap
        // years until we reach the end year.
        while (yr < EndDate.wYear) do
            begin
              cday := cDay + 365;
              if (((yr and 03) = 0) and ( (yr <= 1750) or (yr mod 100 <> 0) or (yr mod 400 = 0))) then
                inc(cday);
              inc(yr);
            end;
        end;

    Result := cDay;
end;

function CmpDate(Date1, Date2 : TSystemTime) : integer;
begin
  if (Date1.wYear < Date2.wYear) then
   Result := -1
     else
       if (Date1.wYear > Date2.wYear) then
         Result := 1
           else
             if (Date1.wMonth < Date2.wMonth) then
               Result := -1
                 else
                   if (Date1.wMonth > Date2.wMonth) then
                     Result := 1
                       else
                         if (Date1.wDay < Date2.wDay) then
                           Result := -1
                             else if (Date1.wDay > Date2.wDay)  then
                               Result := 1
                                 else
                                   Result := 0;
end;

function CmpSystemTime(Time1, Time2 : TSystemTime) : integer;
begin
    if (Time1.wYear < Time2.wYear) then
        Result := -1
    else if (Time1.wYear > Time2.wYear) then
        Result := 1
    else if (Time1.wMonth < Time2.wMonth)then
        Result := -1
    else if (Time1.wMonth > Time2.wMonth) then
        Result := 1
    else if (Time1.wDay < Time2.wDay) then
        Result := -1
    else if (Time1.wDay > Time2.wDay) then
        Result := 1
    else if (Time1.wHour < Time2.wHour) then
        Result := -1
    else if (Time1.wHour > Time2.wHour) then
        Result := 1
    else if (Time1.wMinute < Time2.wMinute) then
        Result := -1
    else if (Time1.wMinute > Time2.wMinute) then
        Result := 1
    else if (Time1.wSecond < Time2.wSecond) then
        Result := -1
    else if (Time1.wSecond > Time2.wSecond) then
        Result := 1
    else
        Result := 0;
end;

function IncSystemTime(const AValue : TSystemTime; ADelta : longint;  flag : TPSCTimeInc) : TSystemTime;
label
 LDTday;
var
 cdyMon : integer;
begin
  Result := AValue;

  if (flag = tiSecond) then
   begin
    ADelta := LIncWord(Result.wSecond, ADelta, 0, 60);
    if ADelta = 0 then
     Exit;
   end;

   if ((flag = tiSecond) or (flag = tiMinute)) then
     begin
       ADelta := LIncWord(Result.wMinute, ADelta, 0, 60);
       if ADelta = 0 then
        Exit;
     end;

   if ((flag = tiSecond) or (flag = tiMinute) or (flag = tiHour)) then
    begin
      ADelta := LIncWord(Result.wHour, ADelta, 0, 24);
      if ADelta = 0 then
       Exit;
    end;

   if ((flag = tiSecond) or (flag = tiMinute) or (flag = tiHour) or (flag = tiDay)) then
    begin
      LDTday:
            if (ADelta >= 0) then
                begin
                cdyMon := GetDaysForMonth(Result.wYear, Result.wMonth);
                while (Result.wDay + ADelta > cdyMon) do
                    begin
                      ADelta := ADelta - (cdyMon + 1 - Result.wDay);
                      Result.wDay := 1;
                      Result := IncSystemTime(Result, 1, tiMonth);
                      cdyMon := GetDaysForMonth(Result.wYear, Result.wMonth);
                    end;
                end
            else
                begin
                while (Result.wDay <= -ADelta) do
                    begin
                      ADelta := ADelta + Result.wDay;
                      Result := IncSystemTime(Result,  -1, tiMonth);
                      cdyMon := GetDaysForMonth(Result.wYear, Result.wMonth);
                      Result.wDay := cdyMon;
                    end;
                end;

            Result.wDay := Result.wDay + ADelta;
            Exit;
      end;

   if ((flag = tiSecond) or (flag = tiMinute) or (flag = tiHour) or (flag = tiDay) or (flag = tiMonth) ) then
    begin
       ADelta := LIncWord(Result.wMonth, ADelta, 1, 13);
       if ADelta = 0 then
        begin
          cdyMon := GetDaysForMonth(Result.wYear, Result.wMonth);
          if (Result.wDay > cdyMon) then
            Result.wDay := cdyMon;
          Exit;
        end;
     end;

   if ((flag = tiSecond) or (flag = tiMinute) or (flag = tiHour) or (flag = tiDay) or (flag = tiMonth) or (flag = tiYear)) then
     begin
       Result.wYear := Result.wYear + ADelta;
       cdyMon := GetDaysForMonth(Result.wYear, Result.wMonth);
       if (Result.wDay > cdyMon) then
         Result.wDay := cdyMon;
       Exit;
     end;

   if ((flag = tiSecond) or (flag = tiMinute) or (flag = tiHour) or (flag = tiDay) or (flag = tiMonth) or (flag = tiYear) or (flag = tiWeek)) then
    begin
      ADelta := ADelta * 7;
      goto LDTday;
    end;
end;


function IncDateTime(const AValue : TDateTime; ADelta : longint; Flag : TPSCTimeInc) : TDateTime;
var
 FT : TSystemTime;
begin
  DateTimeToSystemTime(AValue, FT);
  FT := IncSystemTime(FT, ADelta, Flag);
  Result := SystemTimeToDateTime(FT);
end;

function DowFromDate(pst : TSystemTime) : integer;
begin
  Result := GetStartDowForMonth(pst.wYear, pst.wMonth);
  Result := (Result + pst.wDay - 1) mod 7;
end;


function IncHour( const AValue: TDateTime; const ANumberOfHours: integer = 1 ): TDateTime;
begin
  Result := IncDateTime(AValue, ANumberOfHours, tiHour);
end;


function IncMinute( const AValue: TDateTime; const ANumberOfMinutes: integer = 1 ): TDateTime;
begin
  Result := IncDateTime(AValue, ANumberOfMinutes, tiMinute);
end;

function IncSecond(const AValue: TDateTime;  const ANumberOfSeconds: integer = 1): TDateTime;
begin
  Result := IncDateTime(AValue, ANumberOfSeconds, tiSecond);
end;


function IncDay(const AValue : TDateTime; const ANumberOfDays : integer = 1) : TDateTime;
begin
  Result := IncDateTime(AValue, ANumberOfDays, tiDay);
end;

function IncMonth(const AValue : TDateTime; const ANumberOfMonths : integer = 1) : TDateTime;
begin
  Result := IncDateTime(AValue, ANumberOfMonths, tiMonth);
end;

function IncYear(const AValue : TDateTime; const ANumberOfYears : integer = 1) : TDateTime;
begin
  Result := IncDateTime(AValue, ANumberOfYears, tiYear);
end;


function NewColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
var
  r, g, b: integer;
begin
  if Value > 100 then Value := 100;
  clr := ColorToRGB(clr);
  r := Clr and $000000FF;
  g := (Clr and $0000FF00) shr 8;
  b := (Clr and $00FF0000) shr 16;


  r := r + Round((255 - r) * (value / 100));
  g := g + Round((255 - g) * (value / 100));
  b := b + Round((255 - b) * (value / 100));

  Result := Windows.GetNearestColor(ACanvas.Handle, RGB(r, g, b));

end;

procedure FillRectColor(HDC: THandle; R: TRect; clr: TColor);
var
 clrRef : COLORREF;
 clrSave: COLORREF;
begin
  clrRef := ColorToRGB(clr);
  clrSave := SetBkColor(HDC, clrRef);
  ExtTextOut(hdc,0,0,ETO_OPAQUE,@R, nil,0, nil);
  SetBkColor(hdc, clrSave);
end;

procedure DimBitmap(ABitmap: TBitmap; Value: integer);
var
  x, y: integer;
  LastColor1, LastColor2, Color: TColor;
begin
  if Value > 100 then Value := 100;
  LastColor1 := -1;
  LastColor2 := -1;

  for y := 0 to ABitmap.Height - 1 do
    for x := 0 to ABitmap.Width - 1 do
    begin
      Color := ABitmap.Canvas.Pixels[x, y];
      if Color = LastColor1 then
        ABitmap.Canvas.Pixels[x, y] := LastColor2
      else
      begin
        LastColor2 := NewColor(ABitmap.Canvas, Color, Value);
        ABitmap.Canvas.Pixels[x, y] := LastColor2;
        LastColor1 := Color;
      end;
    end;
end;


function GetShadeColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
var
  r, g, b: integer;
begin
  clr := ColorToRGB(clr);
  r := Clr and $000000FF;
  g := (Clr and $0000FF00) shr 8;
  b := (Clr and $00FF0000) shr 16;

  r := (r - value);
  if r < 0 then r := 0;
  if r > 255 then r := 255;

  g := (g - value) + 2;
  if g < 0 then g := 0;
  if g > 255 then g := 255;

  b := (b - value);
  if b < 0 then b := 0;
  if b > 255 then b := 255;

  Result := RGB(r, g, b);
end;




procedure DrawBitmapShadow(B: TBitmap; ACanvas: TCanvas; X, Y: integer;
  ShadowColor: TColor);
var
  BX, BY: integer;
  TransparentColor: TColor;
begin
  TransparentColor := B.Canvas.Pixels[0, B.Height - 1];
  for BY := 0 to B.Height - 1 do
    for BX := 0 to B.Width - 1 do
    begin
      if B.Canvas.Pixels[BX, BY] <> TransparentColor then
        ACanvas.Pixels[X + BX, Y + BY] := ShadowColor;
    end;
end;


 procedure DrawArrow(DC : THandle; X, Y: Integer; Vertical: Boolean);
  var
    P: array[1..3] of TPoint;
    Pen: HPEN;
    Brush: HBRUSH;
  begin
    if Vertical then
    begin
      P[1] := Point(X, Y);
      P[2] := Point(X, Y + 4);
      P[3] := Point(X - 2, Y + 2);
    end
    else
    begin
      P[1] := Point(X, Y);
      P[2] := Point(X + 4, Y);
      P[3] := Point(X + 2, Y + 2);
    end;
    Pen := SelectObject(DC, CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNTEXT)));
    Brush := SelectObject(DC, GetSysColorBrush(COLOR_BTNTEXT));
    Polygon(DC, P, 3);
    SelectObject(DC, Brush);
    DeleteObject(SelectObject(DC, Pen));
  end;

function ReplaceSystemDate(const Source : TSystemTime; NewValue : TSystemTime) : TSystemTime;
begin
  Result := Source;
  Result.wYear := NewValue.wYear;
  Result.wMonth := NewValue.wMonth;
  Result.wDayOfWeek := NewValue.wDayOfWeek;
  Result.wDay := NewValue.wDay;
end;

function ReplaceSystemTime(const Source : TSystemTime; NewValue : TSystemTime) : TSystemTime;
begin
  Result := Source;
  Result.wHour := NewValue.wHour;
  Result.wMinute := NewValue.wMinute;
  Result.wSecond := NewValue.wSecond;
  Result.wMilliseconds := NewValue.wMilliseconds;
end;

function BooleanToStr(B: Boolean): AnsiString;
const
  Bools: array [Boolean] of PChar = ('False', 'True');
begin
  Result := Bools[B];
end;


function StringToBoolean(AStr : string) : boolean;
begin
  Result := SameText(AStr, 'True');
end;

// procedure ImageListDrawDisabled was copied from RX library VCLUtils unit
//
procedure ImageListDrawDisabled(Images: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; HighlightColor, GrayColor: TColor; DrawHighlight: Boolean);
var
  Bmp: TBitmap;
  SaveColor: TColor;
begin
  SaveColor := Canvas.Brush.Color;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Images.Width;
    Bmp.Height := Images.Height;
    with Bmp.Canvas do begin
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, Images.Width, Images.Height));
      ImageList_Draw(Images.Handle, Index, Handle, 0, 0, ILD_MASK);
    end;
    Bmp.Monochrome := True;
    if DrawHighlight then begin
      Canvas.Brush.Color := HighlightColor;
      SetTextColor(Canvas.Handle, clWhite);
      SetBkColor(Canvas.Handle, clBlack);
      BitBlt(Canvas.Handle, X + 1, Y + 1, Images.Width,
        Images.Height, Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    end;
    Canvas.Brush.Color := GrayColor;
    SetTextColor(Canvas.Handle, clWhite);
    SetBkColor(Canvas.Handle, clBlack);
    BitBlt(Canvas.Handle, X, Y, Images.Width,
      Images.Height, Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
  finally
    Bmp.Free;
    Canvas.Brush.Color := SaveColor;
  end;
end;

procedure WriteString(Stream : TStream; const St : string);
begin
  Stream.WriteBuffer(pointer(st)^, Length(st));
end;

procedure WriteStrings(Stream : TStream; const StA : array of string);
var
 cnt : integer;
begin
  for cnt := 0 to high(StA) do
   Stream.WriteBuffer(pointer(StA[cnt])^, Length(StA[cnt]));
end;


procedure Swap(var X, Y: integer);
var
 I : integer;
begin
  I := X;
  X := Y;
  Y := I;
end;

procedure FlipRect(var R: TRect);
begin
  Swap(R.Left, R.Top);
  Swap(R.Right, R.Bottom);
end;


procedure VertDrawEdge(HDC : THandle; qrc : TRect; EdgeType : integer;  grfFlags : integer; fVert : boolean);
var
  temprc : TRect;
  uFlags : integer;
begin
  uFlags := grfFlags;
  temprc := qrc;
    if (fVert) then
     begin
       FlipRect(temprc);

       if ((uFlags and BF_DIAGONAL) <> BF_DIAGONAL) then
        begin
          if (grfFlags and BF_TOP = BF_TOP) then uFlags := uFlags or BF_LEFT
            else
              uFlags := uFlags and not BF_LEFT;

          if (grfFlags and BF_LEFT = BF_LEFT) then uFlags := uFlags or BF_TOP
             else
               uFlags := uFlags and not BF_TOP;

          if (grfFlags and BF_BOTTOM = BF_BOTTOM) then uFlags := uFlags or BF_RIGHT
            else
              uFlags := uFlags and not BF_RIGHT;

          if (grfFlags and BF_RIGHT = BF_RIGHT) then uFlags := uFlags or BF_BOTTOM
            else
              uFlags := uFlags and not BF_BOTTOM;
        end
          else
             begin
                if ((grfFlags and  (BF_BOTTOM or BF_RIGHT)) = (BF_BOTTOM or BF_RIGHT)) then
                  begin
                    uFlags := BF_TOP or BF_LEFT;

                    if (edgeType = EDGE_RAISED) then
                     edgeType := EDGE_SUNKEN
                       else
                         edgeType := EDGE_RAISED;



                    uFlags := uFlags or (grfFlags and not BF_RECT);
                    uFlags := uFlags or BF_SOFT;
                end;
            end;
    end;
    DrawEdge(hdc, temprc, edgeType, uFlags);
end;


function IsZeroDate(AValue : TDate) : boolean;
begin
  Result := (AValue = -DateDelta);
end;

function IsZeroDateTime(AValue : TDate) : boolean;
begin
  Result := (AValue = -DateDelta);
end;


procedure DrawTransparentBitmap(DC: HDC; hBmp : HBITMAP ;
          xStart: integer; yStart : integer; cTransparentColor : COLORREF);
var
      bm:                                                  BITMAP;
      cColor:                                              COLORREF;
      bmAndBack, bmAndObject, bmAndMem, bmSave:            HBITMAP;
      bmBackOld, bmObjectOld, bmMemOld, bmSaveOld:         HBITMAP;
      hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave:        HDC;
      ptSize:                                              TPOINT;

begin
   hdcTemp := CreateCompatibleDC(dc);
   SelectObject(hdcTemp, hBmp);   // Select the bitmap

   GetObject(hBmp, sizeof(BITMAP), @bm);
   ptSize.x := bm.bmWidth;            // Get width of bitmap
   ptSize.y := bm.bmHeight;           // Get height of bitmap
   DPtoLP(hdcTemp, ptSize, 1);      // Convert from device
                                     // to logical points

   // Create some DCs to hold temporary data.
   hdcBack   := CreateCompatibleDC(dc);
   hdcObject := CreateCompatibleDC(dc);
   hdcMem    := CreateCompatibleDC(dc);
   hdcSave   := CreateCompatibleDC(dc);

   // Create a bitmap for each DC. DCs are required for a number of
   // GDI functions.

   // Monochrome DC
   bmAndBack   := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

   // Monochrome DC
   bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

   bmAndMem    := CreateCompatibleBitmap(dc, ptSize.x, ptSize.y);
   bmSave      := CreateCompatibleBitmap(dc, ptSize.x, ptSize.y);

   // Each DC must select a bitmap object to store pixel data.
   bmBackOld   := SelectObject(hdcBack, bmAndBack);
   bmObjectOld := SelectObject(hdcObject, bmAndObject);
   bmMemOld    := SelectObject(hdcMem, bmAndMem);
   bmSaveOld   := SelectObject(hdcSave, bmSave);

   // Set proper mapping mode.
   SetMapMode(hdcTemp, GetMapMode(dc));

   // Save the bitmap sent here, because it will be overwritten.
   BitBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCCOPY);

   // Set the background color of the source DC to the color.
   // contained in the parts of the bitmap that should be transparent
   cColor := SetBkColor(hdcTemp, cTransparentColor);

   // Create the object mask for the bitmap by performing a BitBlt
   // from the source bitmap to a monochrome bitmap.
   BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0,
          SRCCOPY);

   // Set the background color of the source DC back to the original
   // color.
   SetBkColor(hdcTemp, cColor);

   // Create the inverse of the object mask.
   BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0,
          NOTSRCCOPY);

   // Copy the background of the main DC to the destination.
   BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, dc, xStart, yStart,
          SRCCOPY);

   // Mask out the places where the bitmap will be placed.
   BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);

   // Mask out the transparent colored pixels on the bitmap.
   BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcBack, 0, 0, SRCAND);

   // XOR the bitmap with the background on the destination DC.
   BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCPAINT);

   // Copy the destination to the screen.
   BitBlt(dc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0,
          SRCCOPY);

   // Place the original bitmap back into the bitmap sent here.
   BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcSave, 0, 0, SRCCOPY);

   // Delete the memory bitmaps.
   DeleteObject(SelectObject(hdcBack, bmBackOld));
   DeleteObject(SelectObject(hdcObject, bmObjectOld));
   DeleteObject(SelectObject(hdcMem, bmMemOld));
   DeleteObject(SelectObject(hdcSave, bmSaveOld));

   // Delete the memory DCs.
   DeleteDC(hdcMem);
   DeleteDC(hdcBack);
   DeleteDC(hdcObject);
   DeleteDC(hdcSave);
   DeleteDC(hdcTemp);
end;


{ NotEmplementedException }

constructor NotEmplementedException.Create;
begin
  inherited Create('The method or operation is not implemented');
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function Max3(I, J,K: Integer): Integer;
begin
  if I > J then Result := I else Result := J;
  if K>Result then result:=K;
end;


procedure CenterWindow(Wnd: HWnd);
var
  Rect: TRect;
  Monitor: TMonitor;
begin
  GetWindowRect(Wnd, Rect);
  if Application.MainForm <> nil then
    Monitor := Application.MainForm.Monitor
  else
    Monitor := Screen.Monitors[0];
    SetWindowPos(Wnd, 0,
    Monitor.Left + ((Monitor.Width - Rect.Right + Rect.Left) div 2),
    Monitor.Top + ((Monitor.Height - Rect.Bottom + Rect.Top) div 2),
    0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;


procedure PlaceForm(Frm: TForm; X,Y: integer);
begin
  if not Assigned(Frm) then
   Exit;
  CenterWindow(Frm.Handle);
  if X>=0 then Frm.Left:=X;
  if Y>=0 then Frm.Top:=Y;
  if (X<0) and (Y<0) then Frm.Position:=poScreenCenter;
end;


procedure GetPropertyNames(Instance:TPersistent;StrgLst:TStringList;
                             pPropFilter:TTypeKinds=ctkSimple);
begin
  if Instance = nil then
   Exit;
  GetPropertyNames(TPersistentClass(Instance.ClassType),
                      StrgLst,
                      pPropFilter);
end ;

procedure GetPropertyNames(Instance:TPersistentClass;
                             StrgLst:TStringList;
                             pPropFilter:TTypeKinds=ctkSimple);
var
  nCount:integer;
  nSize:integer;
  PrpLst:PPropList;
  nI:integer;
  lPropFilter:TTypeKinds;
begin
  if (Instance = nil) or (StrgLst = nil) then
   Exit;
  lPropFilter:=pPropFilter;
  StrgLst.Clear;
  nCount:=GetPropList(Instance.ClassInfo,lPropFilter,nil);
  nSize:=nCount*SizeOf(Pointer);
  GetMem(PrpLst,nSize);
  try
    GetPropList(Instance.ClassInfo,lPropFilter,PrpLst);
    for nI:=0 to ncount-1 do
      StrgLst.add(PrpLst[nI].Name);
  finally
    FreeMem(PrpLst,nSize);
  end;
end;

procedure InternalError(S : string);
begin
  raise Exception.Create(S);
end;


function SkipPastConst(const S: String; const Start: Integer): Integer;
{ Returns the character index following the Inno Setup constant embedded
  into the string S at index Start.
  If the constant is not closed (missing a closing brace), it returns zero. }
var
  L, BraceLevel, LastOpenBrace: Integer;
begin
  Result := Start;
  L := Length(S);
  if Result < L then begin
    Inc(Result);
    if S[Result] = '{' then begin
      Inc(Result);
      Exit;
    end
    else begin
      BraceLevel := 1;
      LastOpenBrace := -1;
      while Result <= L do begin
        case S[Result] of
          '{': begin
                   if LastOpenBrace <> Result-1 then begin
                     Inc(BraceLevel);
                     LastOpenBrace := Result;
                   end
                   else
                     { Skip over '{{' when in an embedded constant }
                     Dec(BraceLevel);
                 end;
          '}': begin
                 Dec(BraceLevel);
                 if BraceLevel = 0 then begin
                   Inc(Result);
                   Exit;
                 end;
               end;
        else
          if S[Result] in ConstLeadBytes^ then
            Inc(Result);
        end;
        Inc(Result);
      end;
    end;
  end;
  Result := 0;
end;

function ExpandIndividualConst(const Cnst: String;
  const CustomConsts: array of String): String;
var
  K: Integer;
begin
    if Cnst <> '' then begin
      K := 0;
      while K < High(CustomConsts) do begin
        if Cnst = CustomConsts[K] then begin
          Result := CustomConsts[K+1];
          Exit;
        end;
        Inc(K, 2);
      end;
    end;
    { Unknown constant }
    InternalError(Format('Unknown constant "%s"', [Cnst]));
end;

function ExpandConst(const S: String): String;
begin
  Result := ExpandConstEx(S, ['']);
end;

function ExpandConstEx(const S: String; const CustomConsts: array of String): String;
var
  I, Start: Integer;
  Cnst, ReplaceWith: String;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do begin
    if Result[I] = '{' then begin
      if (I < Length(Result)) and (Result[I+1] = '{') then begin
        { Change '{{' to '{' if not in an embedded constant }
        Inc(I);
        Delete(Result, I, 1);
      end
      else begin
        Start := I;
        { Find the closing brace, skipping over any embedded constants }
        I := SkipPastConst(Result, I);
        if I = 0 then  { unclosed constant? }
          InternalError('Unclosed constant');
        Dec(I);  { 'I' now points to the closing brace }

        { Now translate the constant }
        Cnst := Copy(Result, Start+1, I-(Start+1));
        ReplaceWith := ExpandIndividualConst(Cnst, CustomConsts);
        Delete(Result, Start, (I+1)-Start);
        Insert(ReplaceWith, Result, Start);
        I := Start + Length(ReplaceWith);
      end;
    end
    else begin
      if Result[I] in ConstLeadBytes^ then
        Inc(I);
      Inc(I);
    end;
  end;
end;

procedure GetLeadBytes(var ALeadBytes: TLeadByteSet);
var
  AnsiCPInfo: TCPInfo;
  I: Integer;
  J: Byte;
begin
  ALeadBytes := [];
  if GetCPInfo(CP_ACP, AnsiCPInfo) then
    with AnsiCPInfo do begin
      I := 0;
      while (I < MAX_LEADBYTES) and ((LeadByte[I] or LeadByte[I+1]) <> 0) do begin
        for J := LeadByte[I] to LeadByte[I+1] do
          Include(ALeadBytes, Char(J));
        Inc(I, 2);
      end;
    end;
end;


function DirectoryExists(const Directory: string): Boolean;
var
  Code: integer;
begin
  Code := GetFileAttributes(PChar(Directory));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
    SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), lpdata);
  result := 0;
end;

function SelectDirectory(const Caption: string; const Root: WideString;
  var Directory: string): Boolean;
var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  OldErrorMode: Cardinal;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
begin
  Result := False;
  if not DirectoryExists(Directory) then
    Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(Application.Handle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      with BrowseInfo do
      begin
        hwndOwner := Application.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
        if Directory <> '' then
        begin
          lpfn := SelectDirCB;
          lParam := Integer(PChar(Directory));
        end;
      end;
      WindowList := DisableTaskWindows(0);
      OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        SetErrorMode(OldErrorMode);
        EnableTaskWindows(WindowList);
      end;
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

function HasText(AWord, AText : string) : boolean;
begin
  Result := Pos(AText, LowerCase(AWord)) = 1;
end;

function PathIsURL(pszPath: string): BOOL;
begin
  if ( HasText(pszPath,'http:') or HasText(pszPath,'ftp:') or
       HasText(pszPath,'news:') or HasText(pszPath,'mailto:')) or
     ( HasText(pszPath,'www.')  or HasText(pszPath,'ftp.'))  then
   Result := true
    else
      Result := false;
end;

function LanguageToString(ALanguage : TPSCLanguage) : string;
begin
  case Alanguage of
   lgNONE : Result := 'Undefined';
   lgDUTCH : Result := 'Dutch';
   lgFRENCH : Result := 'French';
   lgGERMAN : Result := 'German';
   lgENGLISH : Result := 'English';
  end; 
end;

procedure GetSystemMenuFont(Font: TFont);
var
  FNonCLientMetrics: TNonCLientMetrics;
begin
  FNonCLientMetrics.cbSize := Sizeof(TNonCLientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @FNonCLientMetrics,0) then
  begin
    Font.Handle := CreateFontIndirect(FNonCLientMetrics.lfMenuFont);
    Font.Color := clMenuText;
  end;
end;

procedure RunFile(const FName: string; Handle: THandle;
  const Param: string);
begin
  ShellExecute(Handle, nil, PChar(FName), PChar(Param), nil, SW_SHOWNORMAL);
end;

procedure OpenUrl(const Url: string);
const
  csPrefix = 'http://';
var
  AUrl: string;
begin
  if Pos(csPrefix, Url) < 1 then
    AUrl := csPrefix + Url
  else
    AUrl := Url;

  RunFile(AUrl);
end;

function RectWidth(R : TRect) : integer;
begin
  Result := R.Right - R.Left;
end;

function RectHeight(R : TRect) : integer;
begin
  Result := R.Bottom - R.Top;
end;

initialization

ControlCenterList := TList.Create;
GetLeadBytes(LeadBytes);
ConstLeadBytes := @LeadBytes;

finalization

LeadBytes := [];
ControlCenterList.Free;
ControlCenterList := nil;

end.
