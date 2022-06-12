{

  Give ExeName from various sources, and some other helper functions
  =================================================================

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information   

}

unit unt_utility;


interface


uses
  system.SyncObjs, system.UITypes, Windows, SysUtils, Classes, Types, PSAPI, Tlhelp32, VirtualTrees, graphics , messages, printers, unt_tool;

{$Include TraceConst.Inc}



  // some helper functions to retreive Process name from its ID
  Function GetExenameForProcessUsingToolhelp32( pid: DWORD ): String;
  Function GetExenameForProcessUsingPSAPI( pid: DWORD ): String;
  Function GetExenameForProcess( pid: DWORD ): String;
  Function GetExenameForWindow( wnd: HWND ): String;

  function CharIsAlpha(const C: AnsiChar): Boolean;
  function CharIsAlphaNum(const C: AnsiChar): Boolean;
  function CRtoCRLF (strIn : string) : string ;
  //function BinaryEncode(const AStr: String): String;
  function BinaryDecode(const AStr: String): String;

  procedure AutosizeAll (Sender: TVirtualStringTree) ;
  function  StrRepeat(const S: AnsiString; Count: Integer): AnsiString; overload ;
  function  StrRepeat(const S: String; Count: Integer): String; overload ;
  function  getStrings(source: pchar; minchar:integer): TStringList;
  function  getDelimStrings(source: pchar; delim : char): TStringList;
  function  getTabStrings(source: pchar): TStringList;
  function  StringsToTab(source : TStringList) : string;
  function  RemoveLastCRLF (str : string) : string ;
  function  BufToString (PtrBeg,PtrEnd: PAnsiChar) : string ;
  procedure CorrectString (str: string);
  function  BufToAnsiString (PtrBeg : PAnsiChar ; maxLen : integer) : AnsiString ;
  procedure LowTrace(msg: string);
  procedure ResetLowTrace() ;
  procedure DrawHighlight (TargetCanvas: TCanvas; CellRect: TRect; IsBookmark : boolean);
  function  IsSeparator (str : string) : boolean ;
  function  ChangeFontDetail(const TargetCanvas: TCanvas; Column: TColumnIndex; const FontDetails : TFontDetailArray) : boolean;
  procedure GetSystemMenuFont(Font: TFont);

  function  NewColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
  function  GetShadeColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
  procedure DimBitmap(ABitmap: TBitmap; Value: integer);
  procedure GrayBitmap(ABitmap: TBitmap; Value: integer);
  function  GrayColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
  procedure DrawBitmapShadow(B: TBitmap; ACanvas: TCanvas; X, Y: integer;  ShadowColor: TColor);
  procedure FillRectColor(HDC: THandle; R: TRect; clr: TColor);
  procedure DrawArrow(DC : THandle; X, Y: Integer; Vertical: Boolean);
  procedure manyInstances() ;
  procedure parseParameters(var EnterDebugMode : boolean; var LeaveDebugMode : boolean ; var XmlTraceFile : boolean ; var filename : string) ;

var
   AnsiCharTypes: array [AnsiChar] of Word;

implementation

uses
unt_TraceConfig ;

//------------------------------------------------------------------------------

procedure parseParameters(var EnterDebugMode : boolean; var LeaveDebugMode : boolean ; var XmlTraceFile : boolean ; var filename : string) ;
var
   c : integer ;
begin
   // compareText is not case sensitive
   EnterDebugMode := false ;
   LeaveDebugMode := false ;
   XmlTraceFile   := false ;
   filename := '' ;
   for c := 1 to ParamCount do begin
      if (CompareText(ParamStr(c), '/debug') = 0) or (CompareText(ParamStr(c), 'debug') = 0) then begin
         EnterDebugMode := true ;
      end else if (CompareText(ParamStr(c), '/NoDebug') = 0) or (CompareText(ParamStr(c), 'NoDebug') = 0) then begin
         LeaveDebugMode := true ;
      end else if (CompareText(ParamStr(c), '/XmlTrace') = 0) or (CompareText(ParamStr(c), 'XmlTrace') = 0) then begin
         XmlTraceFile := true ;
      end else begin
         FileName := FileName + ParamStr(c) + ' ' ;
      end ;
   end ;
end;

//------------------------------------------------------------------------------

// called by the tracetool.dpr if it's not possible to create the mutex
procedure manyInstances() ;
var
   DebugWin: hWnd ;
   MessageString: string;
   CDS: TCopyDataStruct;
   EnterDebugMode : boolean;
   LeaveDebugMode : boolean ;
   XmlTraceFile : boolean ;
   filename : string ;
begin
   LowTrace('Many instances') ;

   DebugWin := FindWindow('TFormReceiver', 'FormReceiver');
   if DebugWin <> 0 then begin
      parseParameters(EnterDebugMode,LeaveDebugMode,XmlTraceFile,filename) ;

      if EnterDebugMode then begin
         MessageString := Format('%5d%11d', [CST_ENTER_DEBUG_MODE,1])  + #0 + #0 ;
         CDS.cbData := 18 ; // 5 + 11 + 1 + 1
      end else if LeaveDebugMode then begin
         MessageString := Format('%5d%11d', [CST_LEAVE_DEBUG_MODE,1])  + #0 + #0 ;
         CDS.cbData := 18 ; // 5 + 11 + 1 + 1
      end else if (XmlTraceFile) and (FileName <> '')then begin
         MessageString := Format('%5d%s', [CST_OPEN_XML,filename]) + #0 + #0 ;
         CDS.cbData := length(MessageString) ; // 5 + len + 1 + 1
      end else if (filename <> '') then begin
         MessageString := Format('%5d%s', [CST_OPEN_TAIL,filename]) + #0 + #0 ;
         CDS.cbData := length(MessageString) ; // 5 + len + 1 + 1
      end ;

      if MessageString <> '' then begin

         // send mesage to other instance
         CDS.dwData := WMD ;   // identification code "123"
         CDS.lpData := pAnsiString (AnsiString(MessageString)) ; // no need to add #0, because String are null terminated
         SendMessage(DebugWin, WM_COPYDATA, 0, LParam(@CDS));  //WParam(Application.Handle)

      end;
      // finish with the CST_SHOW message
      MessageString := Format('%5d%11d', [CST_SHOW,1])  + #0 + #0 ;
      CDS.cbData := 18 ; // 5 + 11 + 1 + 1

      // send mesage to other instance
      CDS.dwData := WMD ;   // identification code "123"
      CDS.lpData := pAnsiString (AnsiString(MessageString)) ; // no need to add #0, because String are null terminated
      SendMessage(DebugWin, WM_COPYDATA, 0, LParam(@CDS));  //WParam(Application.Handle)
   end ;
end ;

//------------------------------------------------------------------------------

// some helper functions to retreive Process name from its ID

Function GetExenameForProcessUsingPSAPI( pid: DWORD ): String;
Var
  i, cbNeeded: DWORD;
  modules: Array [1..1024] of HINST;
  ProcHandle: THandle;
  filename: Array [0..512] of Char;
Begin
  SetLastError(0);
  result := '';
  prochandle := OpenProcess(
                  PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                  False, pid );
  If prochandle <> 0 Then
  Try
    If EnumProcessModules( prochandle, @modules[1],
                           sizeof( modules ), cbNeeded )
    Then
      For i:= 1 To cbNeeded Div sizeof( HINST ) Do Begin
        If GetModuleFilenameEx( prochandle, modules[i], filename,
                                sizeof( filename )
                               ) > 0
        Then
          If CompareText( ExtractFileExt( filename ), '.EXE' ) = 0
          Then Begin
            Result := filename;
            Break;
          End;
      End; { For }
  Finally
    CloseHandle( prochandle );
  End;
End;

//------------------------------------------------------------------------------

Function GetExenameForProcessUsingToolhelp32( pid: DWORD ): String;
Var
  snapshot: THandle;
  procentry: TProcessEntry32;
  ret: BOOL;
Begin
  SetLastError(0);  
  Result := '';
  snapshot:= CreateToolhelp32Snapshot( TH32CS_SNAPPROCESS, 0 );
  If snapshot <> INVALID_HANDLE_VALUE Then
  Try
    procentry.dwSize := Sizeof( procentry );
    ret := Process32FIRST( snapshot, procentry );
    While ret Do Begin
      If procentry.th32ProcessID = pid Then Begin
        Result := procentry.szExeFile;
        Break;
      End
      Else
        ret:= Process32Next( snapshot, procentry );
    End;
  Finally
    CloseHandle( snapshot );
  End;
End;

//------------------------------------------------------------------------------

Function GetExenameForProcess( pid: DWORD ): String;
Begin
  If (Win32Platform = VER_PLATFORM_WIN32_NT) and
     (Win32MajorVersion <= 4)
  Then
    Result := GetExenameForProcessUsingPSAPI( pid )
  Else
    Result := GetExenameForProcessUsingToolhelp32( pid );

  result := ExtractFileName(Result)
End;

//------------------------------------------------------------------------------

Function GetExenameForWindow( wnd: HWND ): String;
Var
  pid: DWORD;
Begin
  Result := '';
  If IsWindow( wnd ) Then Begin
    GetWindowThreadProcessID( wnd, pid );
    If pid <> 0 Then
      Result := GetExenameForProcess( pid );
  End;
End;

//------------------------------------------------------------------------------

function CharIsAlphaNum(const C: AnsiChar): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_ALPHA) <> 0) or ((AnsiCharTypes[C] and C1_DIGIT) <> 0);
end;

//------------------------------------------------------------------------------

function CharIsAlpha(const C: AnsiChar): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_ALPHA) <> 0;
end;

//------------------------------------------------------------------------------

procedure LoadCharTypes;
{$IFDEF MSWINDOWS}
var
  CurrChar: AnsiChar;
  CurrType: Word;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  for CurrChar := Low(AnsiChar) to High(AnsiChar) do
  begin
    GetStringTypeExA(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, SizeOf(AnsiChar), CurrType);
    AnsiCharTypes[CurrChar] := CurrType;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

//function BinaryEncode(const AStr: String): String;
//const
//  NoConversion = ['A'..'Z','a'..'z','*','@','.','_','-',' ',
//                  '0'..'9','$','!','''','(',')'];
//var
//  Sp, Rp: PChar;
//begin
//  SetLength(Result, Length(AStr) * 6);    // each bytes can be converted to 6 chars
//  Sp := PChar(AStr);
//  Rp := PChar(Result);
//  while Sp^ <> #0 do
//  begin
//    if Sp^ in NoConversion then
//      Rp^ := Sp^
//    else begin
//      FormatBuf(Rp^, 6, '&#%3.3d;', 8, [Ord(Sp^)]);    // 6 chars
//      Inc(Rp,5);    // 5 + 1 in the next statement
//    end;
//    Inc(Rp);
//    Inc(Sp);
//  end;
//  SetLength(Result, Rp - PChar(Result));
//end;

//------------------------------------------------------------------------------
// decode only the &#nnn; format

function BinaryDecode(const AStr: String): String;
var
  Sp, Rp, Tp1,Tp2: PChar;
  S: String;
  charCode : cardinal ;
  I, Code: Integer;
  goodSentence : boolean ;
begin
  SetLength(Result, Length(AStr));
  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do
  begin
    if (Sp^ = '&') and ((Sp+1)^ = '#') then begin
       Tp1 := Sp+2 ;
       Tp2 := Sp+2 ;
       goodSentence := true ;
       while (Tp2^ <> ';') and (Tp2^ <> #0) do begin
          charCode := Ord(Tp2^) ;
          // if not numeric : goodSentence become false
          if (charCode < Ord ('0')) or (charCode > Ord ('9')) then begin
             goodSentence := false ;
             break ;
          end ;
          Inc(Tp2);
       end ;
       if (Tp2^ <> ';') then
          goodSentence := false ;

       if goodSentence then begin  // correct sentence
          SetString(S, Tp1, Tp2-Tp1);
          Val(S, I, Code);
          Rp^ := Chr((I));
          Sp := Tp2 + 1 ;
       end else begin
          // bad sentence, just copy it
          Rp^ := Sp^;
          Inc(Sp);
       end ;

    end else begin
       Rp^ := Sp^;
       Inc(Sp);
    end ;
    Inc(Rp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

//------------------------------------------------------------------------------

function CRtoCRLF (strIn : string) : string ;
begin
   result := StringReplace(strIn, #10, #13#10, [rfReplaceAll]);
end ;

//------------------------------------------------------------------------------

procedure AutosizeAll (Sender: TVirtualStringTree) ;
var
   ColIdx : TColumnIndex ;
   bitmap : Tbitmap ;
   HeaderSize: TSize;
   MaxColWidth : integer ;
   col : TVirtualTreeColumn ;
begin
   bitmap := TBitmap.Create ;
   bitmap.Canvas.Font.Assign(sender.Header.Font);
   ColIdx := Sender.Header.Columns.GetFirstVisibleColumn ;
   while ColIdx <> InvalidColumn do begin
      col := Sender.header.Columns[ColIdx] ;
      if (coResizable in col.Options) then begin
         GetTextExtentPoint32W(bitmap.Canvas.Handle,PWideChar(col.text), Length (col.text),HeaderSize) ;
         inc (HeaderSize.cx,12) ;
         MaxColWidth := sender.GetMaxColumnWidth(ColIdx) ;
         //MaxColWidth := 200 ;
         if MaxColWidth > HeaderSize.cx then
            col.Width := MaxColWidth
         else
            col.Width := HeaderSize.cx ;
      end ;
      ColIdx := Sender.Header.Columns.GetNextVisibleColumn(ColIdx) ;
   end ;
   bitmap.free ;
end ;

//------------------------------------------------------------------------------

function getTabStrings(source: pchar): TStringList;
begin
   result := getDelimStrings (source, #9) ;
end ;

//------------------------------------------------------------------------------
// same as TStrings.GetDelimitedText but without QuoteChar
function StringsToTab(source : TStringList) : string;
var
  S: string;
  I, Count: Integer;
begin
  Count := source.Count;
    Result := '' ;
  if Count = 0 then
     exit ;

   Result := '';
   for I := 0 to Count - 2 do
   begin
      S := source[I];
      Result := Result + S + #9;
   end;
   Result := Result + source[Count - 1];
end;

//------------------------------------------------------------------------------

function getDelimStrings(source: pchar; delim : char): TStringList;
var
   Ptr1,ptr2 : pchar ;
begin
   result := TStringList.create ;
   ptr2 := source ;
   ptr1 := ptr2 ;

   // loop until we found a #0
   while (true ) do begin
      if (ptr2^ = #0) then begin
         result.Add(ptr1) ;
         exit ;
      end else if (ptr2^ = delim) then begin       // separator
         ptr2^ := #0 ;
         result.Add(ptr1) ;
         ptr2^ := delim ;    // keep source unchanged
         inc (ptr2) ;
         ptr1 := ptr2 ;
      end else begin
         inc (ptr2);
      end ;
   end ;
end ;

//------------------------------------------------------------------------------

function getStrings(source: pchar;minchar:integer): TStringList;
var
   Ptr1,ptr2 : pchar ;
   readchar : integer ;

   // create a string with all the chars between the 2 pointers (inclusive) and add to stringlist
   procedure addToList (PtrBeg, PtrEnd : pchar) ;
   var
      LineToAdd : string ;
   begin
      LineToAdd := copy (ptrBeg,1,PtrEnd-PtrBeg+1) ;
      result.Add(LineToAdd) ;
   end ;
begin
   result := TStringList.create ;
   ptr2 := source ;
   ptr1 := ptr2 ;
   readchar := 0 ;

   // loop until we found a #0
   while (true ) do begin
      if minchar <> -1 then begin
         // min char is specified : the 0 is not considered as end of buffer
         if readchar > minchar then
            break ;
         if (ptr2^ = #0) and ((ptr2+1)^ = #10)  then begin   // 00 + LF
            addToList (ptr1, ptr2-1) ;
            inc (ptr2,2) ;
            inc (readchar,2) ;
            ptr1 := ptr2 ;
         end ;

      end else begin // no min specified : stop on first 0
         if (ptr2^ = #0) then
            break ;
      end ;

      if (ptr2^ = #13) and ((ptr2+1)^ = #10) then begin   // CR + LF
         //ptr2^ := #0 ;
         addToList (ptr1, ptr2-1) ;
         inc (ptr2,2) ;
         inc (readchar,2) ;
         ptr1 := ptr2 ;
      end else if (ptr2^ = #13) then begin                // CR
         //ptr2^ := #0 ;
         addToList (ptr1, ptr2-1) ;
         inc (ptr2) ;
         inc (readchar) ;
         ptr1 := ptr2 ;
      end else if (ptr2^ = #10) then begin                // LF
         //ptr2^ := #0 ;
         addToList (ptr1, ptr2-1) ;
         inc (ptr2) ;
         inc (readchar,2) ;
         ptr1 := ptr2 ;
      end else begin
         inc (ptr2);
         inc (readchar) ;
      end ;
   end ;
end ;

//------------------------------------------------------------------------------

function RemoveLastCRLF (str : string) : string ;
var
   Ptr : pchar ;
   le : integer ;
begin
   ptr := pchar(str) ;
   le := length (str) ;
   if le = 0 then begin
      result := str ;
      exit ;
   end ;

   ptr := ptr + le - 1 ;  // go to last char

   if (ptr^ = #13) and ((ptr-1)^ = #10) then    // CR + LF
      result := copy (str,1,le-2)
   else if (ptr^ = #10) and ((ptr-1)^ = #13) then    // LF + CR
      result := copy (str,1,le-2)
   else if (ptr^ = #10) then    // LF
      result := copy (str,1,le-1)
   else if (ptr^ = #13) then    // CR
      result := copy (str,1,le-1) 
   else
      result := str ;
end ;


//------------------------------------------------------------------------------

// generate a string removing zero / CR / LF chars
function BufToString (PtrBeg,PtrEnd: PAnsiChar) : string ;
var
   Src : PAnsiChar ;
   Dest : PChar ;
begin
   //result := 'BufToString (PtrBeg,PtrEnd: PAnsiChar)' ;
   //exit ;

   SetLength (result, PtrEnd-PtrBeg+1) ;
   Dest := PChar(result) ;
   Src := PtrBeg ;
   // loop until we found end of line
   while (Src <= PtrEnd ) do begin
      if (Src^ <> #0) and (Src^ <> #13) and (Src^ <> #10) then begin
         //result := result + Src^ ;       // VERY SLOW : create MANY strings
         Dest^ := char (Src^) ;  // convert the Ansichar to char
         inc(Dest) ;
      end ;
      inc (Src);
   end ;
   //Dest^ := #0 ;
   dec(dest) ;
   if PtrEnd-PtrBeg <> Dest-PChar(result) then
      SetLength (result, Dest-PChar(result)+1) ;
end ;

//------------------------------------------------------------------------------

// remove bad chars ( zero / -1 / -2)
procedure CorrectString (str: string)  ;
var
   Src : PChar ;
begin
   Src := pchar(str) ;
   // loop until we found end of line
   while (Src^ <> #0 ) do begin
      if (Src^ >= #$FFFE) then begin
         //result := result + Src^ ;       // VERY SLOW : create MANY strings
         Src^ := '?' ;
      end ;
      inc (Src);
   end ;

end ;

//------------------------------------------------------------------------------

function BufToAnsiString (PtrBeg : PAnsiChar ; maxLen : integer) : AnsiString ;
var
   Src : PAnsiChar ;
   Dest : PAnsiChar ;
   CopiedByte : integer ;
begin
   //result := PtrBeg ; exit ;
   SetLength (result, maxLen+1) ;
   Dest := PAnsiChar(result) ;
   Src := PtrBeg ;
   CopiedByte := 0 ;
   // loop until we found #0
   while (Src^ <> #0) do begin
      //result := result + Src^ ;       // VERY SLOW : create MANY strings
      Dest^ := Src^ ;
      inc (Dest) ;
      inc (Src);
      inc (CopiedByte) ;
   end ;

   if CopiedByte <> maxLen then
      SetLength (result, CopiedByte) ;
end ;

//------------------------------------------------------------------------------

function StrRepeat(const S: AnsiString; Count: Integer): AnsiString;
var
  L: Integer;
  P: PAnsiChar;
begin
  L := Length(S);
  SetLength(Result, Count * L);
  P := Pointer(Result);
  while Count > 0 do
  begin
    Move(Pointer(S)^, P^, L);
    P := P + L;
    Dec(Count);
  end;
end;

//------------------------------------------------------------------------------

function StrRepeat(const S: String; Count: Integer): String;
var
  L: Integer;
  P: PChar;
begin
  L := Length(S);
  SetLength(Result, Count * L);
  P := Pointer(Result);
  while Count > 0 do
  begin
    Move(Pointer(S)^, P^, L* SizeOf(Char));
    P := P + L;
    Dec(Count);
  end;
end;

//------------------------------------------------------------------------------

procedure ResetLowTrace() ;
var
   f:textfile;
begin
   if FileCriticalSection <> nil then
      FileCriticalSection.enter ;
   try try
     assignfile(f,TraceConfig.General_InternalLog);
     rewrite(f);
     writeln(f, DateTimeToStr(now) );
     closefile(f);
   except end finally
      if FileCriticalSection <> nil then
         FileCriticalSection.Release
   end ;
end ;

//------------------------------------------------------------------------------

procedure LowTrace(msg: string);
var
   f:textfile;
begin
   if (TraceConfig.DebugMode = false) and (TraceConfig.AppDisplay_DisableInternalLog = true) then
      exit ;
   OutputDebugString (PWideChar(msg));
   if FileCriticalSection <> nil then
      FileCriticalSection.enter ;
   try try
     assignfile(f,TraceConfig.General_InternalLog);
     if FileExists(TraceConfig.General_InternalLog)
     then
       append(f)
     else
       rewrite(f);
     writeln(f, Format('%5d ', [CST_THREAD_ID,getCurrentThreadID()]) + FormatDateTime('yyyymmdd hh:mm:ss:zzz',now) + ' ' + msg );
     closefile(f);
   except end finally
      if FileCriticalSection <> nil then
         FileCriticalSection.Release
   end ;
end;

//------------------------------------------------------------------------------

procedure DrawHighlight (TargetCanvas: TCanvas; CellRect: TRect; IsBookmark : boolean);
begin
   // the TBaseVirtualTree.PrepareCell use the column color and fill the cell
   // We must refill the cell with the "Search" color

   {
   // draw 2 lines
   TargetCanvas.Pen.Color := TColor($00EFCB8F);
   TargetCanvas.MoveTo(0, CellRect.Bottom - 4);
   TargetCanvas.LineTo(CellRect.Right, CellRect.Bottom - 4 );

   TargetCanvas.Pen.Color := clGreen;
   TargetCanvas.MoveTo(0, CellRect.Bottom - 3);
   TargetCanvas.LineTo(CellRect.Right, CellRect.Bottom - 3 );
   }

   // fill line with a light blue color
   if IsBookmark then
      TargetCanvas.Brush.Color :=  clTeal  //    TColor($BDFEB4)  -> light green
   else
      TargetCanvas.Brush.Color := TColor($FED0B9) ;
   TargetCanvas.FillRect(CellRect);

end ;

//------------------------------------------------------------------------------

// check if ALL chars are - or =
// return true if at least 3 chars are detected
// spaces are not allowed. the strings '-=-' and '=-==' return true 
function IsSeparator (str : string) : boolean ;
var
   ptr : pchar ;
   sep : char ;
   nbSep : integer ;
begin
   result := false ;
   ptr := pchar(trim(str)) ;
   if ptr^ = #0 then
      exit ;

   sep := ptr^ ;
   if (sep <> '-') and (sep <> '=') then
      exit ;

   nbSep := 0 ;
   while ptr^ <> #0 do begin
      if ptr^ <> sep then
         exit ;
      inc (ptr) ;
      inc (nbSep) ;
   end ;
   if nbSep >= 3 then
      result := true ;
end ;

//------------------------------------------------------------------------------

// apply font change and return true if at least one font change is detected
function ChangeFontDetail(const TargetCanvas: TCanvas; Column: TColumnIndex; const FontDetails : TFontDetailArray) : boolean;
var
   FontDetail : TFontDetail ;
   c : integer ;
begin
   //TargetCanvas.font.Style := [fsBold] ;
   //TargetCanvas.font.Name := 'Roman' ;
   //TargetCanvas.font.size := 24 ;

   result := false ;
   for c := 0 to length(FontDetails) -1 do begin
      FontDetail := FontDetails[c] ;
      if FontDetail.ColId = Column then begin
         result := true ;     // at least one font change is detected
         if FontDetail.Bold then
            TargetCanvas.font.Style := [fsBold] ;

         if FontDetail.Italic then
            TargetCanvas.font.Style := TargetCanvas.font.Style + [fsItalic] ;

         if FontDetail.Name <> '' then
            TargetCanvas.font.Name := FontDetail.Name ;

         if FontDetail.Size <> 0  then
            TargetCanvas.font.size := FontDetail.Size ;

         if FontDetail.Color <> -1  then
            TargetCanvas.font.Color := FontDetail.Color ;
      end ;
   end ;
end ;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
   LoadCharTypes;          // this table first

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
