// ***************************************************************
//  madStrings.pas            version: 1.7.1  ·  date: 2012-07-03
//  -------------------------------------------------------------
//  string routines
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2012 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2012-07-03 1.7.1 fixed: IntToStrEx(cardinal) was output as integer
// 2012-04-03 1.7.0 (1) added x64 support
//                  (2) most functions are now available as xxx, xxxA and xxxW
// 2009-07-13 1.6a  fixed bug in unicode PosPChar function
// 2009-02-09 1.6   (1) Delphi 2009 support
//                  (2) made most functions available for unicode strings
// 2003-11-02 1.5k  (1) booleanToChar parameter name changed -> BCB support
//                  (2) ErrorCodeToStr unknown errors are shown in hex now
//                  (3) SizeToStr/MsToStr: language dependent decimal seperator
// 2003-06-09 1.5j  (1) IntToHex now returns low characters, looks nicer
//                  (2) ErrorCodeToStr understands nt status errors ($Cxxxxxxx)
// 2002-05-07 1.5i  SubStrExists/SubTextExists speed up
// 2001-05-17 1.5h  ReplaceText added
// 2001-02-01 1.5g  FileMatch rewritten
// 2001-01-31 1.5f  bug in PosPChar fixed
// 2001-01-03 1.5e  bug in recursive ReplaceStr fixed
// 2000-11-13 1.5d  little bug in PosPChar fixed
// 2000-08-18 1.5c  bugs in SubStrExists + SubTextExists + SubStr fixed
// 2000-07-25 1.5b  minor changes in order to get rid of SysUtils

unit madStrings;

{$I mad.inc}

interface

uses madTypes;

// deletes all control and space characters at the end and the beginning of "str"
procedure    TrimStr (  var str:    AnsiString); {$ifdef UnicodeOverloads} overload;
procedure    TrimStr (  var str: UnicodeString); overload; {$endif}
function  RetTrimStr (const str:    AnsiString) :    AnsiString; {$ifdef UnicodeOverloads} overload;
function  RetTrimStr (const str: UnicodeString) : UnicodeString; overload; {$endif}

// deletes all "killChr(s)" characters from "str"
function KillChar  (var str:    AnsiString; killChr : AnsiChar) : boolean; {$ifdef UnicodeOverloads} overload;
function KillChar  (var str: UnicodeString; killChr : WideChar) : boolean; overload; {$endif}
function KillChars (var str:    AnsiString; killChrs: TSChar  ) : boolean; {$ifdef UnicodeOverloads} overload;
function KillChars (var str: UnicodeString; killChrs: TSChar  ) : boolean; overload; {$endif}

// deletes all occurences of "killStr" from "str"
function KillStr (var str:    AnsiString; const killStr:    AnsiString) : boolean; {$ifdef UnicodeOverloads} overload;
function KillStr (var str: UnicodeString; const killStr: UnicodeString) : boolean; overload; {$endif}

// replaces all occurences of "replaceThis" in "str" with "withThis"
function ReplaceStr  (var str:    AnsiString; const replaceThis, withThis:    AnsiString; replaceSelf: boolean = false) : boolean; {$ifdef UnicodeOverloads} overload;
function ReplaceStr  (var str: UnicodeString; const replaceThis, withThis: UnicodeString; replaceSelf: boolean = false) : boolean; overload; {$endif}
function ReplaceText (var str:    AnsiString; const replaceThis, withThis:    AnsiString; replaceSelf: boolean = false) : boolean; {$ifdef UnicodeOverloads} overload;
function ReplaceText (var str: UnicodeString; const replaceThis, withThis: UnicodeString; replaceSelf: boolean = false) : boolean; overload; {$endif}

// same as System.Delete, but returns the result instead of changing the "str" parameter
function RetDelete (const str:    AnsiString; index: cardinal; count: cardinal = maxInt) : AnsiString; {$ifdef UnicodeOverloads} overload;
function RetDelete (const str: UnicodeString; index: cardinal; count: cardinal = maxInt) : UnicodeString; overload; {$endif}

// deletes "count" characters from the end of the string "str"
procedure    DeleteR (  var str:    AnsiString; count: cardinal); {$ifdef UnicodeOverloads} overload;
procedure    DeleteR (  var str: UnicodeString; count: cardinal); overload; {$endif}
function  RetDeleteR (const str:    AnsiString; count: cardinal) :    AnsiString; {$ifdef UnicodeOverloads} overload;
function  RetDeleteR (const str: UnicodeString; count: cardinal) : UnicodeString; overload; {$endif}

// same as System.Copy, but changes the parameter instead of returning the result string
procedure Keep (var str:    AnsiString; index: cardinal; count: cardinal = maxInt); {$ifdef UnicodeOverloads} overload;
procedure Keep (var str: UnicodeString; index: cardinal; count: cardinal = maxInt); overload; {$endif}

// copies/keeps the last "count" characters
procedure KeepR (  var str:    AnsiString; count: cardinal); {$ifdef UnicodeOverloads} overload;
procedure KeepR (  var str: UnicodeString; count: cardinal); overload; {$endif}
function  CopyR (const str:    AnsiString; count: cardinal) :    AnsiString; {$ifdef UnicodeOverloads} overload;
function  CopyR (const str: UnicodeString; count: cardinal) : UnicodeString; overload; {$endif}

// same as AnsiUpperCase/AnsiLowerCase, but much faster
function UpChar  (const c:    AnsiChar  ) :    AnsiChar;   {$ifdef UnicodeOverloads} overload;
function UpChar  (const c:    WideChar  ) :    WideChar;   overload; {$endif}
function UpStr   (const s:    AnsiString) :    AnsiString; {$ifdef UnicodeOverloads} overload;
function UpStr   (const s: UnicodeString) : UnicodeString; overload; {$endif}
function LowChar (const c:    AnsiChar  ) :    AnsiChar;   {$ifdef UnicodeOverloads} overload;
function LowChar (const c:    WideChar  ) :    WideChar;   overload; {$endif}
function LowStr  (const s:    AnsiString) :    AnsiString; {$ifdef UnicodeOverloads} overload;
function LowStr  (const s: UnicodeString) : UnicodeString; overload; {$endif}

// boolean -> char
function BooleanToChar (value: boolean) : AnsiString;

// tests (case insensitivly) if "s1" and "s2" are identical
function IsTextEqual (const s1, s2:    AnsiString) : boolean; {$ifdef UnicodeOverloads} overload;
function IsTextEqual (const s1, s2: UnicodeString) : boolean; overload; {$endif}

// same as SysUtils.CompareStr/CompareText, but supports ['ä', 'é', ...]
function CompareStr  (const s1, s2:    AnsiString) : integer; {$ifdef UnicodeOverloads} overload;
function CompareStr  (const s1, s2: UnicodeString) : integer; overload; {$endif}
function CompareText (const s1, s2:    AnsiString) : integer; {$ifdef UnicodeOverloads} overload;
function CompareText (const s1, s2: UnicodeString) : integer; overload; {$endif}

// SysUtils.Pos with extended functionality
// searches only the sub areas "fromPos..toPos"
// is "fromPos > toPos", the search works backwards
function PosStr   (const subStr, str:    AnsiString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer; {$ifdef UnicodeOverloads} overload;
function PosStr   (const subStr, str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer; overload; {$endif}
function PosText  (const subStr, str:    AnsiString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer; {$ifdef UnicodeOverloads} overload;
function PosText  (const subStr, str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer; overload; {$endif}

// similar to PosStr/Text, but with PAnsi/WideChar instead of PAnsi/UnicodeString
function PosPChar (subStr       : PAnsiChar;
                   str          : PAnsiChar;
                   subStrLen    : cardinal = 0;   // 0 -> StrLen is called internally
                   strLen       : cardinal = 0;
                   ignoreCase   : boolean  = false;
                   fromPos      : cardinal = 0;
                   toPos        : cardinal = maxInt) : integer; {$ifdef UnicodeOverloads} overload;
function PosPChar (subStr       : PWideChar;
                   str          : PWideChar;
                   subStrLen    : cardinal = 0;   // 0 -> StrLen is called internally
                   strLen       : cardinal = 0;
                   ignoreCase   : boolean  = false;
                   fromPos      : cardinal = 0;
                   toPos        : cardinal = maxInt) : integer; overload; {$endif}

// same as "PosStr(...) = 1" and "PosText(...) = 1", but much faster
function PosStrIs1  (const subStr, str:    AnsiString) : boolean; {$ifdef UnicodeOverloads} overload;
function PosStrIs1  (const subStr, str: UnicodeString) : boolean; overload; {$endif}
function PosTextIs1 (const subStr, str:    AnsiString) : boolean; {$ifdef UnicodeOverloads} overload;
function PosTextIs1 (const subStr, str: UnicodeString) : boolean; overload; {$endif}

// returns the first occurence of one of the characters in "str"
function PosChars (const ch: TSChar; const str:    AnsiString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer; {$ifdef UnicodeOverloads} overload;
function PosChars (const ch: TSChar; const str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer; overload; {$endif}

// tests, if the string "str" matches the "mask"
// examples:
// StrMatch  ('test123abc', 'test???abc') = true
// StrMatch  ('test123abc', 'test?abc')   = false
// StrMatch  ('test123abc', 'test*abc')   = true
// StrMatch  ('test123abc', 'TEST*abc')   = false
// TextMatch ('test123abc', 'TEST*abc')   = true
// TextMatch ('test123abc', '*.*')        = false
// FileMatch ('test123abc', '*.*')        = true
function  StrMatch (const str,   mask:    AnsiString) : boolean; {$ifdef UnicodeOverloads} overload;
function  StrMatch (const str,   mask: UnicodeString) : boolean; overload; {$endif}
function TextMatch (const str,   mask:    AnsiString) : boolean; {$ifdef UnicodeOverloads} overload;
function TextMatch (const str,   mask: UnicodeString) : boolean; overload; {$endif}
function FileMatch (const file_, mask:    AnsiString) : boolean; {$ifdef UnicodeOverloads} overload;
function FileMatch (const file_, mask: UnicodeString) : boolean; overload; {$endif}

// same as StrMatch/TextMatch...
// extended sytax capabilities:    [!Length:0,1,2,4..7]
// examples:
// StrMatchEx  ('test123abc', 'test[3:0..9]abc')       = true
// TextMatchEx ('test123abc', 'test123[3:A,b,C]')      = true
// StrMatchEx  ('test123abc', 'test[!3:a..z,A..Z]abc') = true
function  StrMatchEx (const str, mask:    AnsiString) : boolean;
function TextMatchEx (const str, mask:    AnsiString) : boolean;

// returns "str", but with a minimal Length of "minLen" characters
// if nessecary, the string is filled up with "fillChar"
// if minLen is negative, the string is filled at the end, otherwise at the beginning
function FillStr (const str:    AnsiString; minLen: integer; fillChar: AnsiChar = ' ') : AnsiString; {$ifdef UnicodeOverloads} overload;
function FillStr (const str: UnicodeString; minLen: integer; fillChar: WideChar = ' ') : UnicodeString; overload; {$endif}

// same as SysUtils.IntToStr/IntToHex, but with extended functionality
function IntToStrEx (value: integer;  minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;
function IntToStrEx (value: cardinal; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;
function IntToStrEx (value: int64;    minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;
function IntToHexEx (value: integer;  minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;
function IntToHexEx (value: cardinal; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;
function IntToHexEx (value: int64;    minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;

// same as SysUtils.StrToInt, but with different parameters
// no exceptions are raised, an invalid string results in an invalid result
// this function is *very* fast...
function StrToIntEx (hex: boolean; str: PAnsiChar; len: integer) : integer; {$ifdef UnicodeOverloads} overload;
function StrToIntEx (hex: boolean; str: PWideChar; len: integer) : integer; overload; {$endif}
{$EXTERNALSYM StrToIntEx}

// handles strings like this: "*.txt|c:\dokumente\*.doc|test.bat"
// the first subString has the index "1"
procedure FormatSubStrs (var   str:    AnsiString;                               delimiter: AnsiChar = '|'); {$ifdef UnicodeOverloads} overload;
procedure FormatSubStrs (var   str: UnicodeString;                               delimiter: AnsiChar = '|'); overload; {$endif}
function  SubStrCount   (const str:    AnsiString;                               delimiter: AnsiChar = '|') : integer; {$ifdef UnicodeOverloads} overload;
function  SubStrCount   (const str: UnicodeString;                               delimiter: AnsiChar = '|') : integer; overload; {$endif}
function  SubStr        (const str:    AnsiString;       index  :      cardinal; delimiter: AnsiChar = '|') : AnsiString; {$ifdef UnicodeOverloads} overload;
function  SubStr        (const str: UnicodeString;       index  :      cardinal; delimiter: AnsiChar = '|') : UnicodeString; overload; {$endif}
function  SubStrExists  (const str:    AnsiString; const subStr :    AnsiString; delimiter: AnsiChar = '|') : boolean; {$ifdef UnicodeOverloads} overload;
function  SubStrExists  (const str: UnicodeString; const subStr : UnicodeString; delimiter: AnsiChar = '|') : boolean; overload; {$endif}
function  SubTextExists (const str:    AnsiString; const subText:    AnsiString; delimiter: AnsiChar = '|') : boolean; {$ifdef UnicodeOverloads} overload;
function  SubTextExists (const str: UnicodeString; const subText: UnicodeString; delimiter: AnsiChar = '|') : boolean; overload; {$endif}

// converts "fileSize" to a string
// examples:
//  500       ->  '500 Bytes'
// 1024       ->  '1 KB'
// 1024*1024  ->  '1 MB'
function SizeToStr (size: int64) : AnsiString;

// converts "time" to a string
// examples:
// 15          ->  '15 ms'
// 1000        ->  '1 s'
// 60*1000     ->  '1 min'
// 60*60*1000  ->  '1 h'
function MsToStr (time: cardinal) : AnsiString;

// converts the "error" code to a string
// 5  ->  'Access denied'
function ErrorCodeToStr (error: cardinal) : AnsiString;

// ***************************************************************

var
  TrimStrA        : procedure (var   str: AnsiString) = TrimStr;
  RetTrimStrA     : function  (const str: AnsiString) : AnsiString = RetTrimStr;
  KillCharA       : function  (var   str: AnsiString; killChr : AnsiChar) : boolean = KillChar;
  KillCharsA      : function  (var   str: AnsiString; killChrs: TSChar  ) : boolean = KillChars;
  KillStrA        : function  (var   str: AnsiString; const killStr: AnsiString) : boolean = KillStr;
  ReplaceStrA     : function  (var   str: AnsiString; const replaceThis, withThis: AnsiString; replaceSelf: boolean = false) : boolean = ReplaceStr;
  ReplaceTextA    : function  (var   str: AnsiString; const replaceThis, withThis: AnsiString; replaceSelf: boolean = false) : boolean = ReplaceText;
  {$EXTERNALSYM ReplaceTextA}
  RetDeleteA      : function  (const str: AnsiString; index: cardinal; count: cardinal = maxInt) : AnsiString = RetDelete;
  DeleteRA        : procedure (var   str: AnsiString; count: cardinal) = DeleteR;
  RetDeleteRA     : function  (const str: AnsiString; count: cardinal) : AnsiString = RetDeleteR;
  KeepA           : procedure (var   str: AnsiString; index: cardinal; count: cardinal = maxInt) = Keep;
  KeepRA          : procedure (var   str: AnsiString; count: cardinal) = KeepR;
  CopyRA          : function  (const str: AnsiString; count: cardinal) : AnsiString = CopyR;
  UpCharA         : function  (const c: AnsiChar) : AnsiChar = UpChar;
  UpStrA          : function  (const s: AnsiString) : AnsiString = UpStr;
  LowCharA        : function  (const c: AnsiChar) : AnsiChar = LowChar;
  LowStrA         : function  (const s: AnsiString) : AnsiString = LowStr;
  BooleanToCharA  : function  (value: boolean) : AnsiString = BooleanToChar;
  IsTextEqualA    : function  (const s1, s2: AnsiString) : boolean = IsTextEqual;
  CompareStrA     : function  (const s1, s2: AnsiString) : integer = CompareStr;
  CompareTextA    : function  (const s1, s2: AnsiString) : integer = CompareText;
  PosStrA         : function  (const subStr, str: AnsiString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer = PosStr;
  PosTextA        : function  (const subStr, str: AnsiString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer = PosText;
  PosPCharA       : function  (subStr, str: PAnsiChar; subStrLen: cardinal = 0; strLen: cardinal = 0; ignoreCase: boolean = false; fromPos: cardinal = 0; toPos: cardinal = maxInt) : integer = PosPChar;
  PosStrIs1A      : function  (const subStr, str: AnsiString) : boolean = PosStrIs1;
  PosTextIs1A     : function  (const subStr, str: AnsiString) : boolean = PosTextIs1;
  PosCharsA       : function  (const ch: TSChar; const str: AnsiString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer = PosChars;
  StrMatchA       : function  (const str,   mask: AnsiString) : boolean = StrMatch;
  TextMatchA      : function  (const str,   mask: AnsiString) : boolean = TextMatch;
  FileMatchA      : function  (const file_, mask: AnsiString) : boolean = FileMatch;
  StrMatchExA     : function  (const str, mask: AnsiString) : boolean = StrMatchEx;
  TextMatchExA    : function  (const str, mask: AnsiString) : boolean = TextMatchEx;
  FillStrA        : function  (const str: AnsiString; minLen: integer; fillChar: AnsiChar = ' ') : AnsiString = FillStr;
  StrToIntExA     : function  (hex: boolean; str: PAnsiChar; len: integer) : integer = StrToIntEx;
  {$EXTERNALSYM StrToIntExA}
  FormatSubStrsA  : procedure (var   str: AnsiString;                            delimiter: AnsiChar = '|') = FormatSubStrs;
  SubStrCountA    : function  (const str: AnsiString;                            delimiter: AnsiChar = '|') : integer = SubStrCount;
  SubStrA         : function  (const str: AnsiString;       index  :   cardinal; delimiter: AnsiChar = '|') : AnsiString = SubStr;
  SubStrExistsA   : function  (const str: AnsiString; const subStr : AnsiString; delimiter: AnsiChar = '|') : boolean = SubStrExists;
  SubTextExistsA  : function  (const str: AnsiString; const subText: AnsiString; delimiter: AnsiChar = '|') : boolean = SubTextExists;
  SizeToStrA      : function  (size: int64) : AnsiString = SizeToStr;
  MsToStrA        : function  (time: cardinal) : AnsiString = MsToStr;
  ErrorCodeToStrA : function  (error: cardinal) : AnsiString = ErrorCodeToStr;

// ***************************************************************

{$ifdef UnicodeOverloads}
  var
    TrimStrW       : procedure (var   str: UnicodeString) = TrimStr;
    RetTrimStrW    : function  (const str: UnicodeString) : UnicodeString = RetTrimStr;
    KillCharW      : function  (var   str: UnicodeString; killChr : WideChar) : boolean = KillChar;
    KillCharsW     : function  (var   str: UnicodeString; killChrs: TSChar  ) : boolean = KillChars;
    KillStrW       : function  (var   str: UnicodeString; const killStr: UnicodeString) : boolean = KillStr;
    ReplaceStrW    : function  (var   str: UnicodeString; const replaceThis, withThis: UnicodeString; replaceSelf: boolean = false) : boolean = ReplaceStr;
    ReplaceTextW   : function  (var   str: UnicodeString; const replaceThis, withThis: UnicodeString; replaceSelf: boolean = false) : boolean = ReplaceText;
    {$EXTERNALSYM ReplaceTextW}
    RetDeleteW     : function  (const str: UnicodeString; index: cardinal; count: cardinal = maxInt) : UnicodeString = RetDelete;
    DeleteRW       : procedure (  var str: UnicodeString; count: cardinal) = DeleteR;
    RetDeleteRW    : function  (const str: UnicodeString; count: cardinal) : UnicodeString = RetDeleteR;
    KeepW          : procedure (var   str: UnicodeString; index: cardinal; count: cardinal = maxInt) = Keep;
    KeepRW         : procedure (  var str: UnicodeString; count: cardinal) = KeepR;
    CopyRW         : function  (const str: UnicodeString; count: cardinal) : UnicodeString = CopyR;
    UpCharW        : function  (const c: WideChar) : WideChar = UpChar;
    UpStrW         : function  (const s: UnicodeString) : UnicodeString = UpStr;
    LowCharW       : function  (const c: WideChar) : WideChar = LowChar;
    LowStrW        : function  (const s: UnicodeString) : UnicodeString = LowStr;
    IsTextEqualW   : function  (const s1, s2: UnicodeString) : boolean = IsTextEqual;
    CompareStrW    : function  (const s1, s2: UnicodeString) : integer = CompareStr;
    CompareTextW   : function  (const s1, s2: UnicodeString) : integer = CompareText;
    PosStrW        : function  (const subStr, str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer = PosStr;
    PosTextW       : function  (const subStr, str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer = PosText;
    PosPCharW      : function  (subStr, str: PWideChar; subStrLen: cardinal = 0; strLen: cardinal = 0; ignoreCase: boolean = false; fromPos: cardinal = 0; toPos: cardinal = maxInt) : integer = PosPChar;
    PosStrIs1W     : function  (const subStr, str: UnicodeString) : boolean = PosStrIs1;
    PosTextIs1W    : function  (const subStr, str: UnicodeString) : boolean = PosTextIs1;
    PosCharsW      : function  (const ch: TSChar; const str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer = PosChars;
    StrMatchW      : function  (const str,   mask: UnicodeString) : boolean = StrMatch;
    TextMatchW     : function  (const str,   mask: UnicodeString) : boolean = TextMatch;
    FileMatchW     : function  (const file_, mask: UnicodeString) : boolean = FileMatch;
    FillStrW       : function  (const str: UnicodeString; minLen: integer; fillChar: WideChar = ' ') : UnicodeString = FillStr;
    StrToIntExW    : function  (hex: boolean; str: PWideChar; len: integer) : integer = StrToIntEx;
    {$EXTERNALSYM StrToIntExW}
    FormatSubStrsW : procedure (var   str: UnicodeString;                               delimiter: AnsiChar = '|') = FormatSubStrs;
    SubStrCountW   : function  (const str: UnicodeString;                               delimiter: AnsiChar = '|') : integer = SubStrCount;
    SubStrW        : function  (const str: UnicodeString;       index  :      cardinal; delimiter: AnsiChar = '|') : UnicodeString = SubStr;
    SubStrExistsW  : function  (const str: UnicodeString; const subStr : UnicodeString; delimiter: AnsiChar = '|') : boolean = SubStrExists;
    SubTextExistsW : function  (const str: UnicodeString; const subText: UnicodeString; delimiter: AnsiChar = '|') : boolean = SubTextExists;
{$else}
  procedure TrimStrW       (var   str: UnicodeString);
  function  RetTrimStrW    (const str: UnicodeString) : UnicodeString;
  function  KillCharW      (var   str: UnicodeString; killChr : WideChar) : boolean;
  function  KillCharsW     (var   str: UnicodeString; killChrs: TSChar  ) : boolean;
  function  KillStrW       (var   str: UnicodeString; const killStr: UnicodeString) : boolean;
  function  ReplaceStrW    (var   str: UnicodeString; const replaceThis, withThis: UnicodeString; replaceSelf: boolean = false) : boolean;
  function  ReplaceTextW   (var   str: UnicodeString; const replaceThis, withThis: UnicodeString; replaceSelf: boolean = false) : boolean;
  function  RetDeleteW     (const str: UnicodeString; index: cardinal; count: cardinal = maxInt) : UnicodeString;
  procedure DeleteRW       (  var str: UnicodeString; count: cardinal);
  function  RetDeleteRW    (const str: UnicodeString; count: cardinal) : UnicodeString;
  procedure KeepW          (var   str: UnicodeString; index: cardinal; count: cardinal = maxInt);
  procedure KeepRW         (  var str: UnicodeString; count: cardinal);
  function  CopyRW         (const str: UnicodeString; count: cardinal) : UnicodeString;
  function  UpCharW        (const c: WideChar) : WideChar;
  function  UpStrW         (const s: UnicodeString) : UnicodeString;
  function  LowCharW       (const c: WideChar) : WideChar;
  function  LowStrW        (const s: UnicodeString) : UnicodeString;
  function  IsTextEqualW   (const s1, s2: UnicodeString) : boolean;
  function  CompareStrW    (const s1, s2: UnicodeString) : integer;
  function  CompareTextW   (const s1, s2: UnicodeString) : integer;
  function  PosStrW        (const subStr, str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer;
  function  PosTextW       (const subStr, str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer;
  function  PosPCharW      (subStr, str: PWideChar; subStrLen: cardinal = 0; strLen: cardinal = 0; ignoreCase: boolean = false; fromPos: cardinal = 0; toPos: cardinal = maxInt) : integer;
  function  PosStrIs1W     (const subStr, str: UnicodeString) : boolean;
  function  PosTextIs1W    (const subStr, str: UnicodeString) : boolean;
  function  PosCharsW      (const ch: TSChar; const str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer;
  function  StrMatchW      (const str,   mask: UnicodeString) : boolean;
  function  TextMatchW     (const str,   mask: UnicodeString) : boolean;
  function  FileMatchW     (const file_, mask: UnicodeString) : boolean;
  function  FillStrW       (const str: UnicodeString; minLen: integer; fillChar: WideChar = ' ') : UnicodeString;
  function  StrToIntExW    (hex: boolean; str: PWideChar; len: integer) : integer;
  procedure FormatSubStrsW (var   str: UnicodeString;                               delimiter: AnsiChar = '|');
  function  SubStrCountW   (const str: UnicodeString;                               delimiter: AnsiChar = '|') : integer;
  function  SubStrW        (const str: UnicodeString;       index  :      cardinal; delimiter: AnsiChar = '|') : UnicodeString;
  function  SubStrExistsW  (const str: UnicodeString; const subStr : UnicodeString; delimiter: AnsiChar = '|') : boolean;
  function  SubTextExistsW (const str: UnicodeString; const subText: UnicodeString; delimiter: AnsiChar = '|') : boolean;
{$endif}

// ***************************************************************

function BooleanToCharW  (value: boolean) : UnicodeString;
function IntToStrExW     (value: integer;  minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString; overload;
function IntToStrExW     (value: cardinal; minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString; overload;
function IntToStrExW     (value: int64;    minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString; overload;
function IntToHexExW     (value: integer;  minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString; overload;
function IntToHexExW     (value: cardinal; minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString; overload;
function IntToHexExW     (value: int64;    minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString; overload;
function SizeToStrW      (size : int64   ) : UnicodeString;
function MsToStrW        (time : cardinal) : UnicodeString;
function ErrorCodeToStrW (error: cardinal) : UnicodeString;
function PosNotCharsW    (const ch: TSChar; const str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer;

// ***************************************************************

function IntToStrExA (value: integer;  minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;
function IntToStrExA (value: cardinal; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;
function IntToStrExA (value: int64;    minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;
function IntToHexExA (value: integer;  minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;
function IntToHexExA (value: cardinal; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;
function IntToHexExA (value: int64;    minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString; overload;

// ***************************************************************

{$ifndef ver120}{$ifndef ver130}{$define d6}{$ifndef ver140}{$define d7}{$endif}{$endif}{$endif}

// internal stuff, please ignore
function DecryptStr(const str: AnsiString) : AnsiString;
function AnsiToWideEx(const ansi: AnsiString; addTerminatingZero: boolean = true) : AnsiString;
function WideToAnsiEx(wide: PWideChar) : AnsiString;
function InternalStrMatchW(const str, mask: UnicodeString; fileMode: boolean) : boolean;
const CNtDll : AnsiString = (* ntdll.dll *)  #$3B#$21#$31#$39#$39#$7B#$31#$39#$39;
function DecodeUtf8(const s: AnsiString) : UnicodeString;
function EncodeUtf8(const s: UnicodeString) : AnsiString;
function IsValidIdentW(const ident: UnicodeString; allowDots: boolean = false) : boolean;
function ExtractFileNameW(const str: UnicodeString) : UnicodeString;
function ExtractFilePathW(const str: UnicodeString) : UnicodeString;
function ExtractFileDriveW(const str: UnicodeString) : UnicodeString;
function ExtractFileExtW(const str: UnicodeString) : UnicodeString;

// ***************************************************************

implementation

uses Windows;

// ***************************************************************

procedure TrimStr(var str: AnsiString);
var c1, c2 : cardinal;
begin
  c1 := PosCharsA([#33..#255], str);
  if c1 <> 0 then begin
    c2 := PosCharsA([#33..#255], str, maxInt, 1);
    KeepA(str, c1, c2 - c1 + 1);
  end else
    str := '';
end;

{$ifdef UnicodeOverloads}
procedure TrimStr(var str: UnicodeString);
{$else}
procedure TrimStrW(var str: UnicodeString);
{$endif}
var c1, c2 : cardinal;
begin
  c1 := PosNotCharsW([#0..#32], str);
  if c1 <> 0 then begin
    c2 := PosNotCharsW([#0..#32], str, maxInt, 1);
    KeepW(str, c1, c2 - c1 + 1);
  end else
    str := '';
end;

function RetTrimStr(const str: AnsiString) : AnsiString;
var c1, c2 : cardinal;
begin
  c1 := PosCharsA([#33..#255], str);
  if c1 <> 0 then begin
    c2 := PosCharsA([#33..#255], str, maxInt, 1);
    result := Copy(str, c1, c2 - c1 + 1);
  end else
    result := '';
end;

{$ifdef UnicodeOverloads}
function RetTrimStr(const str: UnicodeString) : UnicodeString;
{$else}
function RetTrimStrW(const str: UnicodeString) : UnicodeString;
{$endif}
var c1, c2 : cardinal;
begin
  c1 := PosNotCharsW([#0..#32], str);
  if c1 <> 0 then begin
    c2 := PosNotCharsW([#0..#32], str, maxInt, 1);
    result := Copy(str, c1, c2 - c1 + 1);
  end else
    result := '';
end;

function KillChar(var str: AnsiString; killChr: AnsiChar) : boolean;
var cursor1, cursor2, lastChar : PAnsiChar;
    ch1 : AnsiChar;
begin
  UniqueString(str);
  cursor1 := PAnsiChar(str);
  cursor2 := cursor1;
  lastChar := cursor1 + Length(str) - 1;
  while cursor2 <= lastChar do begin
    ch1 := cursor2^;
    if ch1 <> killChr then begin
      cursor1^ := ch1;
      inc(cursor1);
    end;
    inc(cursor2);
  end;
  result := cursor1 <> cursor2;
  if result then
    SetLength(str, cursor1 - PAnsiChar(str));
end;

{$ifdef UnicodeOverloads}
function KillChar(var str: UnicodeString; killChr: WideChar) : boolean;
{$else}
function KillCharW(var str: UnicodeString; killChr: WideChar) : boolean;
{$endif}
var cursor1, cursor2, lastChar : PWideChar;
    ch1 : WideChar;
begin
  {$ifdef d6}
    UniqueString(str);
  {$endif}
  cursor1 := PWideChar(str);
  cursor2 := cursor1;
  lastChar := cursor1 + Length(str) - 1;
  while cursor2 <= lastChar do begin
    ch1 := cursor2^;
    if ch1 <> killChr then begin
      cursor1^ := ch1;
      inc(cursor1);
    end;
    inc(cursor2);
  end;
  result := cursor1 <> cursor2;
  if result then
    SetLength(str, cursor1 - PWideChar(str));
end;

function KillChars(var str: AnsiString; killChrs: TSChar) : boolean;
var cursor1, cursor2, lastChar : PAnsiChar;
    ch1 : AnsiChar;
begin
  UniqueString(str);
  cursor1 := PAnsiChar(str);
  cursor2 := cursor1;
  lastChar := cursor1 + Length(str) - 1;
  while cursor2 <= lastChar do begin
    ch1 := cursor2^;
    if not (ch1 in killChrs) then begin
      cursor1^ := ch1;
      inc(cursor1);
    end;
    inc(cursor2);
  end;
  result := cursor1 <> cursor2;
  if result then
    SetLength(str, cursor1 - PAnsiChar(str));
end;

{$ifdef UnicodeOverloads}
function KillChars(var str: UnicodeString; killChrs: TSChar) : boolean;
{$else}
function KillCharsW(var str: UnicodeString; killChrs: TSChar) : boolean;
{$endif}
var cursor1, cursor2, lastChar : PWideChar;
    ch1 : WideChar;
begin
  {$ifdef d6}
    UniqueString(str);
  {$endif}
  cursor1 := PWideChar(str);
  cursor2 := cursor1;
  lastChar := cursor1 + Length(str) - 1;
  while cursor2 <= lastChar do begin
    ch1 := cursor2^;
    if (word(ch1) and $ff00 <> 0) or (not (AnsiChar(ch1) in killChrs)) then begin
      cursor1^ := ch1;
      inc(cursor1);
    end;
    inc(cursor2);
  end;
  result := cursor1 <> cursor2;
  if result then
    SetLength(str, cursor1 - PWideChar(str));
end;

function KillStr(var str: AnsiString; const killStr: AnsiString) : boolean;
var cursor1, cursor2 : PAnsiChar;
    ps  : PAnsiChar;   // PAnsiChar(str);
    pks : PAnsiChar;   // PAnsiChar(killStr)
    ls  : integer;     // length(str)
    lks : integer;     // length(killStr)
    i1  : integer;
begin
  lks := length(killStr);
  UniqueString(str);
  ps := PAnsiChar(str);
  ls := Length(str);
  pks := PAnsiChar(killStr);
  i1 := PosPCharA(pks, ps, lks, ls);
  result := i1 <> -1;
  if result then begin
    cursor1 := PAnsiChar(str);
    cursor2 := cursor1;
    repeat
      ls := ls - i1 - lks;
      inc(cursor1, i1);
      inc(cursor2, i1 + lks);
      i1 := PosPCharA(pks, cursor2, lks, ls);
      if i1 > 0 then
        Move(cursor2^, cursor1^, i1);
    until i1 = -1;
    if ls > 0 then
      Move(cursor2^, cursor1^, ls);
    SetLength(str, cursor1 - ps + ls);
  end;
end;

{$ifdef UnicodeOverloads}
function KillStr(var str: UnicodeString; const killStr: UnicodeString) : boolean;
{$else}
function KillStrW(var str: UnicodeString; const killStr: UnicodeString) : boolean;
{$endif}
var cursor1, cursor2 : PWideChar;
    ps  : PWideChar;   // PWideChar(str);
    pks : PWideChar;   // PWideChar(killStr)
    ls  : integer;     // length(str)
    lks : integer;     // length(killStr)
    i1  : integer;
begin
  lks := length(killStr);
  {$ifdef d6}
    UniqueString(str);
  {$endif}
  ps := PWideChar(str);
  ls := Length(str);
  pks := PWideChar(killStr);
  i1 := PosPCharW(pks, ps, lks, ls);
  result := i1 <> -1;
  if result then begin
    cursor1 := PWideChar(str);
    cursor2 := cursor1;
    repeat
      ls := ls - i1 - lks;
      inc(cursor1, i1);
      inc(cursor2, i1 + lks);
      i1 := PosPCharW(pks, cursor2, lks, ls);
      if i1 > 0 then
        Move(cursor2^, cursor1^, i1 * 2);
    until i1 = -1;
    if ls > 0 then
      Move(cursor2^, cursor1^, ls * 2);
    SetLength(str, dword(cursor1 - ps) + dword(ls));
  end;
end;

function ReplaceA(var str: AnsiString; const replaceThis, withThis: AnsiString; replaceSelf, ignoreCase: boolean) : boolean;
var cursor1, cursor2 : PAnsiChar;
    ps  : PAnsiChar;   // PAnsiChar(str);
    prt : PAnsiChar;   // PAnsiChar(replaceThis)
    pwt : PAnsiChar;   // PAnsiChar(withThis)
    ls  : integer;     // length(str)
    lrt : integer;     // length(replaceThis)
    lwt : integer;     // length(withThis)
    ld  : integer;     // length difference
    id  : integer;     // inc difference
    i1  : integer;
begin
  lrt := length(replaceThis);
  lwt := length(withThis);
  ld := lwt - lrt;
  if ld <= 0 then begin
    UniqueString(str);
    ps := PAnsiChar(str);
    ls := Length(str);
    prt := PAnsiChar(replaceThis);
    i1 := PosPCharA(prt, ps, lrt, ls, ignoreCase);
    result := i1 <> -1;
    if result then begin
      cursor1 := PAnsiChar(str);
      pwt := PAnsiChar(withThis);
      if replaceSelf then begin
        repeat
          ls := ls - i1;
          inc(cursor1, i1);
          Move(pwt^, cursor1^, lwt);
          if ld <> 0 then begin
            Move(pointer(NativeUInt(cursor1) + dword(lrt))^, pointer(NativeUInt(cursor1) + dword(lwt))^, ls - lrt);
            ls := ls + ld;
          end;
          i1 := PosPCharA(prt, cursor1, lrt, ls, ignoreCase);
        until i1 = -1;
        SetLength(str, NativeUInt(cursor1 - ps) + dword(ls));
      end else begin
        cursor2 := cursor1;
        repeat
          ls := ls - i1 - lrt;
          inc(cursor1, i1);
          inc(cursor2, i1);
          Move(pwt^, cursor1^, lwt);
          inc(cursor1, lwt);
          inc(cursor2, lrt);
          i1 := PosPCharA(prt, cursor2, lrt, ls, ignoreCase);
          if i1 > 0 then
            Move(cursor2^, cursor1^, i1);
        until i1 = -1;
        if ls > 0 then
          Move(cursor2^, cursor1^, ls);
        SetLength(str, NativeUInt(cursor1 - ps) + dword(ls));
      end;
    end;
  end else begin
    i1 := PosPCharA(PAnsiChar(replaceThis), PAnsiChar(str), Length(replaceThis), Length(str), ignoreCase) + 1;
    result := i1 <> 0;
    if result then begin
      if replaceSelf then
        id := 1
      else
        id := lwt;
      ls := length(str);
      repeat
        inc(ls, ld);
        SetLength(str, ls);
        Move(str[i1], str[i1 + ld], ls - i1 + 1 - ld);
        Move(withThis[1], str[i1], lwt);
        inc(i1, id);
        i1 := PosPCharA(PAnsiChar(replaceThis), PAnsiChar(str), Length(replaceThis), Length(str), ignoreCase, i1 - 1, maxInt) + 1;
      until i1 = 0;
    end;
  end;
end;

function ReplaceW(var str: UnicodeString; const replaceThis, withThis: UnicodeString; replaceSelf, ignoreCase: boolean) : boolean;
var cursor1, cursor2 : PWideChar;
    ps  : PWideChar;   // PWideChar(str);
    prt : PWideChar;   // PWideChar(replaceThis)
    pwt : PWideChar;   // PWideChar(withThis)
    ls  : integer;     // length(str)
    lrt : integer;     // length(replaceThis)
    lwt : integer;     // length(withThis)
    ld  : integer;     // length difference
    id  : integer;     // inc difference
    i1  : integer;
begin
  lrt := length(replaceThis);
  lwt := length(withThis);
  ld := lwt - lrt;
  if ld <= 0 then begin
    {$ifdef d6}
      UniqueString(str);
    {$endif}
    ps := PWideChar(str);
    ls := Length(str);
    prt := PWideChar(replaceThis);
    i1 := PosPCharW(prt, ps, lrt, ls, ignoreCase);
    result := i1 <> -1;
    if result then begin
      cursor1 := PWideChar(str);
      pwt := PWideChar(withThis);
      if replaceSelf then begin
        repeat
          ls := ls - i1;
          inc(cursor1, i1);
          Move(pwt^, cursor1^, lwt * 2);
          if ld <> 0 then begin
            Move(pointer(NativeUInt(cursor1) + dword(lrt) * 2)^, pointer(NativeUInt(cursor1) + dword(lwt) * 2)^, (ls - lrt) * 2);
            ls := ls + ld;
          end;
          i1 := PosPCharW(prt, cursor1, lrt, ls, ignoreCase);
        until i1 = -1;
        SetLength(str, NativeUInt(cursor1 - ps) + dword(ls));
      end else begin
        cursor2 := cursor1;
        repeat
          ls := ls - i1 - lrt;
          inc(cursor1, i1);
          inc(cursor2, i1);
          Move(pwt^, cursor1^, lwt * 2);
          inc(cursor1, lwt);
          inc(cursor2, lrt);
          i1 := PosPCharW(prt, cursor2, lrt, ls, ignoreCase);
          if i1 > 0 then
            Move(cursor2^, cursor1^, i1 * 2);
        until i1 = -1;
        if ls > 0 then
          Move(cursor2^, cursor1^, ls * 2);
        SetLength(str, NativeUInt(cursor1 - ps) + dword(ls));
      end;
    end;
  end else begin
    i1 := PosPCharW(PWideChar(replaceThis), PWideChar(str), Length(replaceThis), Length(str), ignoreCase) + 1;
    result := i1 <> 0;
    if result then begin
      if replaceSelf then
        id := 1
      else
        id := lwt;
      ls := length(str);
      repeat
        inc(ls, ld);
        SetLength(str, ls);
        Move(str[i1], str[i1 + ld], (ls - i1 + 1 - ld) * 2);
        Move(withThis[1], str[i1], lwt * 2);
        inc(i1, id);
        i1 := PosPCharW(PWideChar(replaceThis), PWideChar(str), Length(replaceThis), Length(str), ignoreCase, i1 - 1, maxInt) + 1;
      until i1 = 0;
    end;
  end;
end;

function ReplaceStr(var str: AnsiString; const replaceThis, withThis: AnsiString; replaceSelf: boolean = false) : boolean;
begin
  result := ReplaceA(str, replaceThis, withThis, replaceSelf, false);
end;

{$ifdef UnicodeOverloads}
function ReplaceStr(var str: UnicodeString; const replaceThis, withThis: UnicodeString; replaceSelf: boolean = false) : boolean;
{$else}
function ReplaceStrW(var str: UnicodeString; const replaceThis, withThis: UnicodeString; replaceSelf: boolean = false) : boolean;
{$endif}
begin
  result := ReplaceW(str, replaceThis, withThis, replaceSelf, false);
end;

function ReplaceText(var str: AnsiString; const replaceThis, withThis: AnsiString; replaceSelf: boolean = false) : boolean;
begin
  result := ReplaceA(str, replaceThis, withThis, replaceSelf, true);
end;

{$ifdef UnicodeOverloads}
function ReplaceText(var str: UnicodeString; const replaceThis, withThis: UnicodeString; replaceSelf: boolean = false) : boolean;
{$else}
function ReplaceTextW(var str: UnicodeString; const replaceThis, withThis: UnicodeString; replaceSelf: boolean = false) : boolean;
{$endif}
begin
  result := ReplaceW(str, replaceThis, withThis, replaceSelf, true);
end;

function RetDelete(const str: AnsiString; index: cardinal; count: cardinal = maxInt) : AnsiString;
begin
  result := str;
  Delete(result, index, count);
end;

{$ifdef UnicodeOverloads}
function RetDelete(const str: UnicodeString; index: cardinal; count: cardinal = maxInt) : UnicodeString;
{$else}
function RetDeleteW(const str: UnicodeString; index: cardinal; count: cardinal = maxInt) : UnicodeString;
{$endif}
begin
  result := str;
  Delete(result, index, count);
end;

procedure DeleteR(var str: AnsiString; count: cardinal);
begin
  Delete(str, Length(str) - integer(count) + 1, maxInt);
end;

{$ifdef UnicodeOverloads}
procedure DeleteR(var str: UnicodeString; count: cardinal);
{$else}
procedure DeleteRW(var str: UnicodeString; count: cardinal);
{$endif}
begin
  Delete(str, Length(str) - integer(count) + 1, maxInt);
end;

function RetDeleteR(const str: AnsiString; count: cardinal) : AnsiString;
begin
  result := Copy(str, 1, Length(str) - integer(count));
end;

{$ifdef UnicodeOverloads}
function RetDeleteR(const str: UnicodeString; count: cardinal) : UnicodeString;
{$else}
function RetDeleteRW(const str: UnicodeString; count: cardinal) : UnicodeString;
{$endif}
begin
  result := Copy(str, 1, Length(str) - integer(count));
end;

procedure Keep(var str: AnsiString; index: cardinal; count: cardinal = maxInt);
begin
  str := Copy(str, index, count);
end;

{$ifdef UnicodeOverloads}
procedure Keep(var str: UnicodeString; index: cardinal; count: cardinal = maxInt);
{$else}
procedure KeepW(var str: UnicodeString; index: cardinal; count: cardinal = maxInt);
{$endif}
begin
  str := Copy(str, index, count);
end;

procedure KeepR(var str: AnsiString; count: cardinal);
begin
  Delete(str, 1, Length(str) - integer(count));
end;

{$ifdef UnicodeOverloads}
procedure KeepR(var str: UnicodeString; count: cardinal);
{$else}
procedure KeepRW(var str: UnicodeString; count: cardinal);
{$endif}
begin
  Delete(str, 1, Length(str) - integer(count));
end;

function CopyR(const str: AnsiString; count: cardinal) : AnsiString;
begin
  result := Copy(str, Length(str) - integer(count) + 1, maxInt);
end;

{$ifdef UnicodeOverloads}
function CopyR(const str: UnicodeString; count: cardinal) : UnicodeString;
{$else}
function CopyRW(const str: UnicodeString; count: cardinal) : UnicodeString;
{$endif}
begin
  result := Copy(str, Length(str) - integer(count) + 1, maxInt);
end;

var lowCharTable : array [AnsiChar] of AnsiChar =
  (#$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0A,#$0B,#$0C,#$0D,#$0E,#$0F,
   #$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1A,#$1B,#$1C,#$1D,#$1E,#$1F,
   #$20,#$21,#$22,#$23,#$24,#$25,#$26,#$27,#$28,#$29,#$2A,#$2B,#$2C,#$2D,#$2E,#$2F,
   #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3A,#$3B,#$3C,#$3D,#$3E,#$3F,
   #$40,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6A,#$6B,#$6C,#$6D,#$6E,#$6F,
   #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7A,#$5B,#$5C,#$5D,#$5E,#$5F,
   #$60,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6A,#$6B,#$6C,#$6D,#$6E,#$6F,
   #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7A,#$7B,#$7C,#$7D,#$7E,#$7F,
   #$80,#$81,#$82,#$83,#$84,#$85,#$86,#$87,#$88,#$89,#$9A,#$8B,#$9C,#$8D,#$9E,#$8F,
   #$90,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$9A,#$9B,#$9C,#$9D,#$9E,#$FF,
   #$A0,#$A1,#$A2,#$A3,#$A4,#$A5,#$A6,#$A7,#$A8,#$A9,#$AA,#$AB,#$AC,#$AD,#$AE,#$AF,
   #$B0,#$B1,#$B2,#$B3,#$B4,#$B5,#$B6,#$B7,#$B8,#$B9,#$BA,#$BB,#$BC,#$BD,#$BE,#$BF,
   #$E0,#$E1,#$E2,#$E3,#$E4,#$E5,#$E6,#$E7,#$E8,#$E9,#$EA,#$EB,#$EC,#$ED,#$EE,#$EF,
   #$F0,#$F1,#$F2,#$F3,#$F4,#$F5,#$F6,#$D7,#$F8,#$F9,#$FA,#$FB,#$FC,#$FD,#$FE,#$DF,
   #$E0,#$E1,#$E2,#$E3,#$E4,#$E5,#$E6,#$E7,#$E8,#$E9,#$EA,#$EB,#$EC,#$ED,#$EE,#$EF,
   #$F0,#$F1,#$F2,#$F3,#$F4,#$F5,#$F6,#$F7,#$F8,#$F9,#$FA,#$FB,#$FC,#$FD,#$FE,#$FF);
{
procedure SaveLowCharTable(fileName: AnsiString);
var s1, s2 : AnsiString;
    i1     : integer;
begin
  SetLength(s1, 255);
  for i1 := 1 to 255 do s1[i1] := AnsiChar(i1);
  s1 := AnsiLowerCase(s1);
  with TFileStream.Create(fileName, fmCreate) do
    try
      s2 := '#$00,';
      for i1 := 1 to 255 do begin
        if i1 mod 16 = 0 then s2 := s2 + #$D#$A;
        s2 := s2 + '#$' + IntToHexA(cardinal(s1[i1]), 2) + ',';
      end;
      WriteBuffer(pointer(s2)^, Length(s2));
    finally Free end;
end;
}

function UpChar(const c: AnsiChar) : AnsiChar;
begin
  if c in ['a'..'z', 'ö', 'ä', 'ü'] then
    result := AnsiChar(ord(c) - 32)
  else
    result := c;
end;

{$ifdef UnicodeOverloads}
function UpChar(const c: WideChar) : WideChar;
{$else}
function UpCharW(const c: WideChar) : WideChar;
{$endif}
begin
  if (word(c) and $ff00 = 0) and (AnsiChar(c) in ['a'..'z', 'ö', 'ä', 'ü']) then
    result := WideChar(ord(c) - 32)
  else
    result := c;
end;

function UpStr(const s: AnsiString) : AnsiString;
var i1, i2 : integer;
begin
  result := s;
  i2 := Length(s);
  for i1 := 1 to i2 do
    if s[i1] in ['a'..'z','ö','ä','ü'] then
      result[i1] := AnsiChar(ord(s[i1]) - 32);
end;

{$ifdef UnicodeOverloads}
function UpStr(const s: UnicodeString) : UnicodeString;
{$else}
function UpStrW(const s: UnicodeString) : UnicodeString;
{$endif}
var i1, i2 : integer;
begin
  result := s;
  i2 := Length(s);
  for i1 := 1 to i2 do
    if (word(s[i1]) and $ff00 = 0) and (AnsiChar(s[i1]) in ['a'..'z','ö','ä','ü']) then
      result[i1] := WideChar(ord(s[i1]) - 32);
end;

function LowChar(const c: AnsiChar) : AnsiChar;
begin
  result := lowCharTable[c];
end;

{$ifdef UnicodeOverloads}
function LowChar(const c: WideChar) : WideChar;
{$else}
function LowCharW(const c: WideChar) : WideChar;
{$endif}
begin
  if word(c) and $ff00 <> 0 then
    result := c
  else
    result := WideChar(lowCharTable[AnsiChar(c)]);
end;

function LowStr(const s: AnsiString) : AnsiString;
var i1, i2 : integer;
begin
  result := s;
  i2 := Length(s);
  for i1 := 1 to i2 do
    if s[i1] in ['A'..'Z','Ö','Ä','Ü'] then
      result[i1] := AnsiChar(ord(s[i1]) + 32);
end;

{$ifdef UnicodeOverloads}
function LowStr(const s: UnicodeString) : UnicodeString;
{$else}
function LowStrW(const s: UnicodeString) : UnicodeString;
{$endif}
var i1, i2 : integer;
begin
  result := s;
  i2 := Length(s);
  for i1 := 1 to i2 do
    if (word(s[i1]) and $ff00 = 0) and (AnsiChar(s[i1]) in ['A'..'Z','Ö','Ä','Ü']) then
      result[i1] := WideChar(ord(s[i1]) + 32);
end;

function BooleanToChar(value: boolean) : AnsiString;
begin
  if value then result := '+' else result := '-';
end;

function BooleanToCharW(value: boolean) : UnicodeString;
begin
  if value then result := '+' else result := '-';
end;

function IsTextEqual(const s1, s2: AnsiString) : boolean;
var c1, c2 : cardinal;
begin
  c1 := Length(s1);
  result := cardinal(Length(s2)) = c1;
  if result then
    for c2 := 1 to c1 do
      if lowCharTable[s1[c2]] <> lowCharTable[s2[c2]] then begin
        result := false;
        break;
      end;
end;

{$ifdef UnicodeOverloads}
function IsTextEqual(const s1, s2: UnicodeString) : boolean;
{$else}
function IsTextEqualW(const s1, s2: UnicodeString) : boolean;
{$endif}
var c1, c2 : cardinal;
    b1, b2 : boolean;
begin
  c1 := Length(s1);
  result := cardinal(Length(s2)) = c1;
  if result then
    for c2 := 1 to c1 do begin
      b1 := word(s1[c2]) and $ff00 = 0;
      b2 := word(s2[c2]) and $ff00 = 0;
      if b1 <> b2 then begin
        result := false;
        break;
      end;
      if b1 then begin
        if lowCharTable[AnsiChar(s1[c2])] <> lowCharTable[AnsiChar(s2[c2])] then begin
          result := false;
          break;
        end;
      end else
        if s1[c2] <> s2[c2] then begin
          result := false;
          break;
        end;
    end;
end;

function CompareStr(const s1, s2: AnsiString) : integer;
{$ifdef win64}
  var c1, c2, c3, c4 : cardinal;
      ch1, ch2       : AnsiChar;
  begin
    c1 := Length(s1);
    c2 := Length(s2);
    if c1 < c2 then
      c3 := c1
    else
      c3 := c2;
    for c4 := 1 to c3 do begin
      ch1 := s1[c4];
      ch2 := s2[c4];
      if ch1 <> ch2 then begin
        result := integer(ch1) - integer(ch2);
        exit;
      end;
    end;
    result := integer(c1) - integer(c2);
{$else}
  asm  // s1 = EAX; s2 = EDX; result = EAX;
          CMP     EAX,EDX
          JNE     @@doIt
          XOR     EAX,EAX
          JMP     @@noWork
  @@doIt:
          PUSH    ESI
          PUSH    EDI
          MOV     ESI,EAX
          MOV     EDI,EDX
          OR      EAX,EAX
          JE      @@s1Nil
          MOV     EAX,[EAX-4]
  @@s1Nil:
          OR      EDX,EDX
          JE      @@s2Nil
          MOV     EDX,[EDX-4]
  @@s2Nil:
          MOV     ECX,EAX
          CMP     ECX,EDX
          JBE     @@s1Shorter
          MOV     ECX,EDX
  @@s1Shorter:
          CMP     ECX,ECX
          REPE    CMPSB
          JE      @@firstCharsEqual
          MOVZX   EAX,BYTE PTR [ESI-1]
          MOVZX   EDX,BYTE PTR [EDI-1]
  @@firstCharsEqual:
          SUB     EAX,EDX
          POP     EDI
          POP     ESI
  @@noWork:
{$endif}
end;

{$ifdef UnicodeOverloads}
function CompareStr(const s1, s2: UnicodeString) : integer;
{$else}
function CompareStrW(const s1, s2: UnicodeString) : integer;
{$endif}
{$ifdef win64}
  var c1, c2, c3, c4 : cardinal;
      ch1, ch2       : WideChar;
  begin
    c1 := Length(s1);
    c2 := Length(s2);
    if c1 < c2 then
      c3 := c1
    else
      c3 := c2;
    for c4 := 1 to c3 do begin
      ch1 := s1[c4];
      ch2 := s2[c4];
      if ch1 <> ch2 then begin
        result := integer(ch1) - integer(ch2);
        exit;
      end;
    end;
    result := integer(c1) - integer(c2);
{$else}
  asm  // s1 = EAX; s2 = EDX; result = EAX;
          CMP     EAX,EDX
          JNE     @@doIt
          XOR     EAX,EAX
          JMP     @@noWork
  @@doIt:
          PUSH    ESI
          PUSH    EDI
          MOV     ESI,EAX
          MOV     EDI,EDX
          OR      EAX,EAX
          JE      @@s1Nil
          MOV     EAX,[EAX-4]
          {$ifndef UNICODE}
            SHR     EAX,1
          {$ENDIF}
  @@s1Nil:
          OR      EDX,EDX
          JE      @@s2Nil
          MOV     EDX,[EDX-4]
          {$ifndef UNICODE}
            SHR     EDX,1
          {$ENDIF}
  @@s2Nil:
          MOV     ECX,EAX
          CMP     ECX,EDX
          JBE     @@s1Shorter
          MOV     ECX,EDX
  @@s1Shorter:
          CMP     ECX,ECX
          REPE    CMPSW
          JE      @@firstCharsEqual
          MOVZX   EAX,WORD PTR [ESI-2]
          MOVZX   EDX,WORD PTR [EDI-2]
  @@firstCharsEqual:
          SUB     EAX,EDX
          POP     EDI
          POP     ESI
  @@noWork:
{$endif}
end;

function CompareText(const s1, s2: AnsiString) : integer;
var c1, c2, c3, c4 : cardinal;
    ch1, ch2       : AnsiChar;
begin
  c1 := Length(s1);
  c2 := Length(s2);
  if c1 < c2 then
    c3 := c1
  else
    c3 := c2;
  for c4 := 1 to c3 do begin
    ch1 := lowCharTable[s1[c4]];
    ch2 := lowCharTable[s2[c4]];
    if ch1 <> ch2 then begin
      result := integer(ch1) - integer(ch2);
      exit;
    end;
  end;
  result := integer(c1) - integer(c2);
end;

{$ifdef UnicodeOverloads}
function CompareText(const s1, s2: UnicodeString) : integer;
{$else}
function CompareTextW(const s1, s2: UnicodeString) : integer;
{$endif}
var c1, c2, c3, c4 : cardinal;
    ch1, ch2       : WideChar;
begin
  c1 := Length(s1);
  c2 := Length(s2);
  if c1 < c2 then
    c3 := c1
  else
    c3 := c2;
  for c4 := 1 to c3 do begin
    ch1 := s1[c4];
    if word(ch1) and $ff00 = 0 then
      ch1 := WideChar(lowCharTable[AnsiChar(ch1)]);
    ch2 := s2[c4];
    if word(ch2) and $ff00 = 0 then
      ch2 := WideChar(lowCharTable[AnsiChar(ch2)]);
    if ch1 <> ch2 then begin
      result := integer(ch1) - integer(ch2);
      exit;
    end;
  end;
  result := integer(c1) - integer(c2);
end;

function PosStr(const subStr, str: AnsiString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer;
begin
  if (fromPos > 0) and (toPos > 0) then
       result := PosPCharA(PAnsiChar(subStr), PAnsiChar(str), Length(subStr), Length(str), false, fromPos - 1, toPos - 1) + 1
  else result := 0;
end;

{$ifdef UnicodeOverloads}
function PosStr(const subStr, str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer;
{$else}
function PosStrW(const subStr, str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer;
{$endif}
begin
  if (fromPos > 0) and (toPos > 0) then
       result := PosPCharW(PWideChar(subStr), PWideChar(str), Length(subStr), Length(str), false, fromPos - 1, toPos - 1) + 1
  else result := 0;
end;

function PosText(const subStr, str: AnsiString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer;
begin
  if (fromPos > 0) and (toPos > 0) then
       result := PosPCharA(PAnsiChar(subStr), PAnsiChar(str), Length(subStr), Length(str), true, fromPos - 1, toPos - 1) + 1
  else result := 0;
end;

{$ifdef UnicodeOverloads}
function PosText(const subStr, str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer;
{$else}
function PosTextW(const subStr, str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer;
{$endif}
begin
  if (fromPos > 0) and (toPos > 0) then
       result := PosPCharW(PWideChar(subStr), PWideChar(str), Length(subStr), Length(str), true, fromPos - 1, toPos - 1) + 1
  else result := 0;
end;

function PosPChar(subStr       : PAnsiChar;
                  str          : PAnsiChar;
                  subStrLen    : cardinal = 0;   // 0 -> StrLen is called internally
                  strLen       : cardinal = 0;
                  ignoreCase   : boolean  = false;
                  fromPos      : cardinal = 0;
                  toPos        : cardinal = maxInt) : integer;

  function GetPCharLen(const pc: PAnsiChar) : cardinal;
  {$ifdef win64}
    begin
      if pc <> nil then
        result := lstrlenA(pc)
      else
        result := 0;
  {$else}
    asm
      MOV     EDX,EDI
      MOV     EDI,EAX
      MOV     ECX,0FFFFFFFFH
      XOR     AL,AL
      REPNE   SCASB
      MOV     EAX,0FFFFFFFEH
      SUB     EAX,ECX
      MOV     EDI,EDX
  {$endif}
  end;

var pc1, pc2, pc3, pc4, pc5, pc6 : PAnsiChar;
    c1                           : cardinal;
    ch1                          : AnsiChar;
begin
  result := -1;
  if (subStr <> nil) and ((subStrLen <> 0) or (subStr^ <> #0)) and
     (   str <> nil) and ((   strLen <> 0) or (   str^ <> #0)) then begin
    if subStrLen = 0 then subStrLen := GetPCharLen(subStr);
    if    strLen = 0 then    strLen := GetPCharLen(   str);
    if strLen >= subStrLen then begin
      c1 := strLen - subStrLen;
      if ignoreCase then
        ch1 := lowCharTable[subStr^]
      else
        ch1 := subStr^;
      if fromPos > toPos then begin
        if toPos <= c1 then begin
          if fromPos > c1 then
            fromPos := c1;
          pc1 := str + fromPos;
          pc2 := str + toPos;
          pc3 := subStr + 1;
          pc4 := subStr + subStrLen - 1;
          pc6 := pc3;
          if ignoreCase then begin
            while pc1 >= pc2 do
              if lowCharTable[pc1^] = ch1 then begin
                inc(pc1);
                pc5 := pc1;
                while (pc3 <= pc4) and (lowCharTable[pc1^] = lowCharTable[pc3^]) do begin
                  inc(pc1);
                  inc(pc3);
                end;
                if pc3 > pc4 then begin
                  result := pc5 - PAnsiChar(str) - 1;
                  break;
                end;
                pc3 := pc6;
                pc1 := pc5 - 2;
              end else
                dec(pc1);
          end else
            while pc1 >= pc2 do
              if pc1^ = ch1 then begin
                inc(pc1);
                pc5 := pc1;
                while (pc3 <= pc4) and (pc1^ = pc3^) do begin
                  inc(pc1); inc(pc3);
                end;
                if pc3 > pc4 then begin
                  result := pc5 - PAnsiChar(str) - 1;
                  break;
                end;
                pc3 := pc6;
                pc1 := pc5 - 2;
              end else
                dec(pc1);
        end;
      end else
        if fromPos <= c1 then begin
          if toPos > c1 then
            toPos := c1;
          pc1 := str + fromPos;
          pc2 := str + toPos;
          pc3 := subStr + 1;
          pc4 := subStr + subStrLen - 1;
          pc6 := pc3;
          if ignoreCase then begin
            while pc1 <= pc2 do
              if lowCharTable[pc1^] = ch1 then begin
                inc(pc1);
                pc5 := pc1;
                while (pc3 <= pc4) and (lowCharTable[pc1^] = lowCharTable[pc3^]) do begin
                  inc(pc1);
                  inc(pc3);
                end;
                if pc3 > pc4 then begin
                  result := pc5 - PAnsiChar(str) - 1;
                  break;
                end;
                pc3 := pc6;
                pc1 := pc5;
              end else
                inc(pc1);
          end else
            while pc1 <= pc2 do
              if pc1^ = ch1 then begin
                inc(pc1);
                pc5 := pc1;
                while (pc3 <= pc4) and (pc1^ = pc3^) do begin
                  inc(pc1); inc(pc3);
                end;
                if pc3 > pc4 then begin
                  result := pc5 - PAnsiChar(str) - 1;
                  break;
                end;
                pc3 := pc6;
                pc1 := pc5;
              end else
                inc(pc1);
        end;
    end;
  end;
end;

{$ifdef UnicodeOverloads}
function PosPChar(subStr: PWideChar; str: PWideChar; subStrLen, strLen: cardinal; ignoreCase: boolean; fromPos, toPos: cardinal) : integer;
{$else}
function PosPCharW(subStr: PWideChar; str: PWideChar; subStrLen, strLen: cardinal; ignoreCase: boolean; fromPos, toPos: cardinal) : integer;
{$endif}

  function GetPCharLen(const pc: PWideChar) : cardinal;
  {$ifdef win64}
    begin
      if pc <> nil then
        result := lstrlenW(pc)
      else
        result := 0;
  {$else}
    asm
      MOV     EDX,EDI
      MOV     EDI,EAX
      MOV     ECX,0FFFFFFFFH
      XOR     AX,AX
      REPNE   SCASW
      MOV     EAX,0FFFFFFFEH
      SUB     EAX,ECX
      MOV     EDI,EDX
    {$endif}
  end;

var pc1, pc2, pc3, pc4, pc5, pc6 : PWideChar;
    c1                           : cardinal;
    ch1, ch2, ch3                : WideChar;
begin
  result := -1;
  if (subStr <> nil) and ((subStrLen <> 0) or (subStr^ <> #0)) and
     (   str <> nil) and ((   strLen <> 0) or (   str^ <> #0)) then begin
    if subStrLen = 0 then subStrLen := GetPCharLen(subStr);
    if    strLen = 0 then    strLen := GetPCharLen(   str);
    if strLen >= subStrLen then begin
      c1 := strLen - subStrLen;
      ch1 := subStr^;
      if ignoreCase and (word(ch1) and $ff00 = 0) then
        ch1 := WideChar(lowCharTable[AnsiChar(ch1)]);
      if fromPos > toPos then begin
        if toPos <= c1 then begin
          if fromPos > c1 then
            fromPos := c1;
          pc1 := str + fromPos;
          pc2 := str + toPos;
          pc3 := subStr + 1;
          pc4 := subStr + subStrLen - 1;
          pc6 := pc3;
          if ignoreCase then begin
            while pc1 >= pc2 do begin
              ch3 := pc1^;
              if word(ch3) and $ff00 = 0 then
                ch3 := WideChar(lowCharTable[AnsiChar(ch3)]);
              if ch1 = ch3 then begin
                inc(pc1);
                pc5 := pc1;
                while pc3 <= pc4 do begin
                  ch2 := pc1^;
                  if word(ch2) and $ff00 = 0 then
                    ch2 := WideChar(lowCharTable[AnsiChar(ch2)]);
                  ch3 := pc3^;
                  if word(ch3) and $ff00 = 0 then
                    ch3 := WideChar(lowCharTable[AnsiChar(ch3)]);
                  if ch2 <> ch3 then
                    break;
                  inc(pc1);
                  inc(pc3);
                end;
                if pc3 > pc4 then begin
                  result := integer(pc5 - str) - 1;
                  break;
                end;
                pc3 := pc6;
                pc1 := pc5;
                dec(pc1, 2);
              end else
                dec(pc1);
            end;
          end else
            while pc1 >= pc2 do
              if pc1^ = ch1 then begin
                inc(pc1);
                pc5 := pc1;
                while (pc3 <= pc4) and (pc1^ = pc3^) do begin
                  inc(pc1);
                  inc(pc3);
                end;
                if pc3 > pc4 then begin
                  result := integer(pc5 - str) - 1;
                  break;
                end;
                pc3 := pc6;
                pc1 := pc5;
                dec(pc1, 2);
              end else
                dec(pc1);
        end;
      end else
        if fromPos <= c1 then begin
          if toPos > c1 then
            toPos := c1;
          pc1 := str + fromPos;
          pc2 := str + toPos;
          pc3 := subStr + 1;
          pc4 := subStr + subStrLen - 1;
          pc6 := pc3;
          if ignoreCase then begin
            while pc1 <= pc2 do begin
              ch3 := pc1^;
              if word(ch3) and $ff00 = 0 then
                ch3 := WideChar(lowCharTable[AnsiChar(ch3)]);
              if ch1 = ch3 then begin
                inc(pc1);
                pc5 := pc1;
                while pc3 <= pc4 do begin
                  ch2 := pc1^;
                  if word(ch2) and $ff00 = 0 then
                    ch2 := WideChar(lowCharTable[AnsiChar(ch2)]);
                  ch3 := pc3^;
                  if word(ch3) and $ff00 = 0 then
                    ch3 := WideChar(lowCharTable[AnsiChar(ch3)]);
                  if ch2 <> ch3 then
                    break;
                  inc(pc1);
                  inc(pc3);
                end;
                if pc3 > pc4 then begin
                  result := integer(pc5 - str) - 1;
                  break;
                end;
                pc3 := pc6;
                pc1 := pc5;
              end else
                inc(pc1);
            end;
          end else
            while pc1 <= pc2 do
              if pc1^ = ch1 then begin
                inc(pc1);
                pc5 := pc1;
                while (pc3 <= pc4) and (pc1^ = pc3^) do begin
                  inc(pc1);
                  inc(pc3);
                end;
                if pc3 > pc4 then begin
                  result := integer(pc5 - str) - 1;
                  break;
                end;
                pc3 := pc6;
                pc1 := pc5;
              end else
                inc(pc1);
        end;
    end;
  end;
end;

function PosStrIs1(const subStr, str: AnsiString) : boolean;
var c1, c2 : cardinal;
begin
  c1 := Length(   str);
  c2 := Length(subStr);
  if      c1 < c2 then result := false
  else if c2 = 0  then result := true
  else if c1 = c2 then result := subStr = str
  else                 result := PosPCharA(PAnsiChar(subStr), PAnsiChar(str), Length(subStr), Length(str), false, 0, 0) = 0;
end;

{$ifdef UnicodeOverloads}
function PosStrIs1(const subStr, str: UnicodeString) : boolean;
{$else}
function PosStrIs1W(const subStr, str: UnicodeString) : boolean;
{$endif}
var c1, c2 : cardinal;
begin
  c1 := Length(   str);
  c2 := Length(subStr);
  if      c1 < c2 then result := false
  else if c2 = 0  then result := true
  else if c1 = c2 then result := subStr = str
  else                 result := PosPCharW(PWideChar(subStr), PWideChar(str), Length(subStr), Length(str), false, 0, 0) = 0;
end;

function PosTextIs1(const subStr, str: AnsiString) : boolean;
var c1, c2 : cardinal;
begin
  c1 := Length(   str);
  c2 := Length(subStr);
  if      c1 < c2 then result := false
  else if c2 = 0  then result := true
  else if c1 = c2 then result := IsTextEqualA(subStr, str)
  else                 result := PosPCharA(PAnsiChar(subStr), PAnsiChar(str), Length(subStr), Length(str), true, 0, 0) = 0;
end;

{$ifdef UnicodeOverloads}
function PosTextIs1(const subStr, str: UnicodeString) : boolean;
{$else}
function PosTextIs1W(const subStr, str: UnicodeString) : boolean;
{$endif}
var c1, c2 : cardinal;
begin
  c1 := Length(   str);
  c2 := Length(subStr);
  if      c1 < c2 then result := false
  else if c2 = 0  then result := true
  else if c1 = c2 then result := IsTextEqualW(subStr, str)
  else                 result := PosPCharW(PWideChar(subStr), PWideChar(str), Length(subStr), Length(str), true, 0, 0) = 0;
end;

function PosChars(const ch: TSChar; const str: AnsiString; fromPos, toPos: cardinal) : integer;
var c1 : cardinal;
begin
  result := 0;
  if str <> '' then begin
    c1 := Length(str);
    if fromPos > toPos then begin
      if toPos <= c1 then begin
        if fromPos > c1 then fromPos := c1;
        for c1 := fromPos downto toPos do
          if str[c1] in ch then begin
            result := c1;
            break;
          end;
      end;
    end else
      if fromPos <= c1 then begin
        if toPos > c1 then toPos := c1;
        for c1 := fromPos to toPos do
          if str[c1] in ch then begin
            result := c1;
            break;
          end;
      end;
  end;
end;

{$ifdef UnicodeOverloads}
function PosChars(const ch: TSChar; const str: UnicodeString; fromPos, toPos: cardinal) : integer;
{$else}
function PosCharsW(const ch: TSChar; const str: UnicodeString; fromPos, toPos: cardinal) : integer;
{$endif}
var c1  : cardinal;
    ch1 : WideChar;
begin
  result := 0;
  if str <> '' then begin
    c1 := Length(str);
    if fromPos > toPos then begin
      if toPos <= c1 then begin
        if fromPos > c1 then
          fromPos := c1;
        for c1 := fromPos downto toPos do begin
          ch1 := str[c1];
          if (word(ch1) and $ff00 = 0) and (AnsiChar(ch1) in ch) then begin
            result := c1;
            break;
          end;
        end;
      end;
    end else
      if fromPos <= c1 then begin
        if toPos > c1 then
          toPos := c1;
        for c1 := fromPos to toPos do begin
          ch1 := str[c1];
          if (word(ch1) and $ff00 = 0) and (AnsiChar(ch1) in ch) then begin
            result := c1;
            break;
          end;
        end;
      end;
  end;
end;

function PosNotCharsW(const ch: TSChar; const str: UnicodeString; fromPos: cardinal = 1; toPos: cardinal = maxInt) : integer;
var c1  : cardinal;
    ch1 : WideChar;
begin
  result := 0;
  if str <> '' then begin
    c1 := Length(str);
    if fromPos > toPos then begin
      if toPos <= c1 then begin
        if fromPos > c1 then
          fromPos := c1;
        for c1 := fromPos downto toPos do begin
          ch1 := str[c1];
          if (word(ch1) and $ff00 <> 0) or (not (AnsiChar(ch1) in ch)) then begin
            result := c1;
            break;
          end;
        end;
      end;
    end else
      if fromPos <= c1 then begin
        if toPos > c1 then
          toPos := c1;
        for c1 := fromPos to toPos do begin
          ch1 := str[c1];
          if (word(ch1) and $ff00 <> 0) or (not (AnsiChar(ch1) in ch)) then begin
            result := c1;
            break;
          end;
        end;
      end;
  end;
end;

function StrMatch(const str, mask: AnsiString) : boolean;
var cs, cm : PAnsiChar;
    ms, mm : PAnsiChar;
    b1     : boolean;
begin
  cm := PAnsiChar(mask);
  result := (cm[0] = '*') and (cm[1] = #0);
  if not result then begin
    cs := PAnsiChar(str);
    ms := cs;
    mm := cm;
    b1 := false;
    while (cm^ <> #0) or (cs^ <> #0) do begin
      if cm^ = #0 then
        exit;
      if cs^ = #0 then begin
        while cm^ <> #0 do begin
          if cm^ <> '*' then
            exit;
          inc(cm);
        end;
        break;
      end;
      case cm^ of
        '*': if cm[1] <> #0 then begin
               b1 := true;
               inc(cm);
               ms := cs + 1;
               mm := cm;
             end else
               break;
        '?': begin
               inc(cs);
               inc(cm);
             end;
        else if cm^ <> cs^ then begin
               if b1 then begin
                 cm := mm;
                 cs := ms;
                 inc(ms);
               end else
                 exit;
             end else begin
               inc(cs);
               inc(cm);
             end;
      end;
    end;
    result := true;
  end;
end;

function InternalStrMatchW(const str, mask: UnicodeString; fileMode: boolean) : boolean;
var cs, cm : PWideChar;
    ms, mm : PWideChar;
    b1     : boolean;
begin
  cm := PWideChar(mask);
  result := (cm[0] = '*') and (cm[1] = #0);
  if not result then begin
    cs := PWideChar(str);
    ms := cs;
    mm := cm;
    b1 := false;
    while (cm^ <> #0) or (cs^ <> #0) do begin
      if cm^ = #0 then
        exit;
      if cs^ = #0 then begin
        if fileMode and (cm^ = '.') and (cs > PWideChar(str)) and ((cs - 1)^ <> '.') then
          inc(cm);
        while cm^ = '*' do
          inc(cm);
        result := cm^ = #0;
        exit;
      end;
      case cm^ of
        '*': if cm[1] <> #0 then begin
               b1 := true;
               inc(cm);
               ms := cs + 1;
               mm := cm;
             end else
               break;
        '?': begin
               inc(cs);
               inc(cm);
             end;
        else if cm^ <> cs^ then begin
               if b1 then begin
                 cm := mm;
                 cs := ms;
                 inc(ms);
               end else
                 exit;
             end else begin
               inc(cs);
               inc(cm);
             end;
      end;
    end;
    result := true;
  end;
end;

{$ifdef UnicodeOverloads}
function StrMatch(const str, mask: UnicodeString) : boolean;
{$else}
function StrMatchW(const str, mask: UnicodeString) : boolean;
{$endif}
begin
  result := InternalStrMatchW(str, mask, false);
end;

function TextMatch(const str, mask: AnsiString) : boolean;
var cs, cm : PAnsiChar;
    ms, mm : PAnsiChar;
    b1     : boolean;
begin
  cm := PAnsiChar(mask);
  result := (cm[0] = '*') and (cm[1] = #0);
  if not result then begin
    cs := PAnsiChar(str);
    ms := cs;
    mm := cm;
    b1 := false;
    while (cm^ <> #0) or (cs^ <> #0) do begin
      if cm^ = #0 then
        exit;
      if cs^ = #0 then begin
        while cm^ <> #0 do begin
          if cm^ <> '*' then
            exit;
          inc(cm);
        end;
        break;
      end;
      case cm^ of
        '*': if cm[1] <> #0 then begin
               b1 := true;
               inc(cm);
               ms := cs + 1;
               mm := cm;
             end else
               break;
        '?': begin
               inc(cs);
               inc(cm);
             end;
        else if lowCharTable[cm^] <> lowCharTable[cs^] then begin
               if b1 then begin
                 cm := mm;
                 cs := ms;
                 inc(ms);
               end else
                 exit;
             end else begin
               inc(cs);
               inc(cm);
             end;
      end;
    end;
    result := true;
  end;
end;

{$ifdef UnicodeOverloads}
function TextMatch(const str, mask: UnicodeString) : boolean;
{$else}
function TextMatchW(const str, mask: UnicodeString) : boolean;
{$endif}
var cs, cm   : PWideChar;
    ms, mm   : PWideChar;
    b1       : boolean;
    ch1, ch2 : WideChar;
begin
  cm := PWideChar(mask);
  result := (cm[0] = '*') and (cm[1] = #0);
  if not result then begin
    cs := PWideChar(str);
    ms := cs;
    mm := cm;
    b1 := false;
    while (cm^ <> #0) or (cs^ <> #0) do begin
      if cm^ = #0 then
        exit;
      if cs^ = #0 then begin
        while cm^ <> #0 do begin
          if cm^ <> '*' then
            exit;
          inc(cm);
        end;
        break;
      end;
      case cm^ of
        '*': if cm[1] <> #0 then begin
               b1 := true;
               inc(cm);
               ms := cs + 1;
               mm := cm;
             end else
               break;
        '?': begin
               inc(cs);
               inc(cm);
             end;
        else begin
               ch1 := cm^;
               if word(ch1) and $ff00 = 0 then
                 ch1 := WideChar(lowCharTable[AnsiChar(ch1)]);
               ch2 := cs^;
               if word(ch2) and $ff00 = 0 then
                 ch2 := WideChar(lowCharTable[AnsiChar(ch2)]);
               if ch1 <> ch2 then begin
                 if b1 then begin
                   cm := mm;
                   cs := ms;
                   inc(ms);
                 end else
                   exit;
               end else begin
                 inc(cs);
                 inc(cm);
               end;
             end;
      end;
    end;
    result := true;
  end;
end;

const CSpecialCharBegin = '[';
      CSpecialCharEnd   = ']';

function ParseSpecialString(ignoreCase: boolean; var cm: PAnsiChar; var cset: TSChar; var b2: boolean; var sc: integer) : boolean;
const CControlChars : TSChar = [#0, CSpecialCharEnd, ':', ',', '.'];
var pc1 : PAnsiChar;  // PAnsiChar memory
    ch1 : AnsiChar;   // char counter variable
begin
  result := false;
  cset := []; sc := 1;   // inizialize return values to default value
  inc(cm);               // skip CSpecialCharBegin char
  b2 := cm^ = '!';
  if b2 then inc(cm);    // (match) or (not match) ?
  while true do begin
    pc1 := cm;           // remember current char
    while not (cm^ in CControlChars) do inc(cm);        // search first control char
    case cm^ of
      #0                  : exit;                       // CSpecialCharEnd char missing!!
      ':'                 : begin                       // Length value found
                              sc := StrToIntExA(false, pc1, cm - pc1);
                              if sc = 0 then exit;      // if Length is 0 or no integer value -> error!!
                            end;
      ',',CSpecialCharEnd : if cm - pc1 = 1 then begin  // ",x," single char, must be 1 byte long
                              if ignoreCase then
                                   Include(cset, lowCharTable[pc1^])      // fill char set
                              else Include(cset, pc1^);
                              if cm^ = CSpecialCharEnd then begin           // we are ready with parsing...
                                inc(cm);
                                break;
                              end;
                            end else exit;
      '.'                 : if cm - pc1 = 1 then begin  // ",x..y," multiple chars, both x and y must be 1 byte long
                              inc(cm);
                              while cm^ = '.' do inc(cm);                 // skip all '.' chars
                              if cm^ in CControlChars then exit;          // y must not be a control char
                              if cm^ < pc1^ then exit;                    // ",y..x," is not valid
                              for ch1 := pc1^ to cm^ do                   // fill char set
                                if ignoreCase then
                                     Include(cset, lowCharTable[ch1])
                                else Include(cset, ch1);
                              inc(cm);
                              if cm^ = CSpecialCharEnd then begin         // we are ready with parsing...
                                inc(cm);
                                break;
                              end;
                              if cm^ <> ',' then exit;                    // otherwise a ',' MUST follow...
                            end else exit;
    end;
    inc(cm);
  end;
  result := true;
end;

function StrMatchEx(const str, mask: AnsiString) : boolean;
var cs, cm : PAnsiChar;    // cursorString, cursorMask
    ms, mm : PAnsiChar;    // memoryString, memoryMask
    b1     : boolean;      // found "*" in mask ?
    cset   : TSChar;       // for special purposes...
    b2     : boolean;      // special or not special, that's here the question...   :-)
    sc     : integer;      // special char count
    i1     : integer;      // integer counter variable
begin
  cm := PAnsiChar(mask);
  result := (cm^ = '*') and ((cm + 1)^ = #0);  // mask = '*' ?
  if not result then begin
    cs := PAnsiChar(str);
    ms := cs;
    mm := cm;
    b1 := false;
    while (cm^ <> #0) or (cs^ <> #0) do begin
      if cm^ = #0 then exit;   // if mask is empty before string is empty -> no match
      if cs^ = #0 then begin   // if string is empty before mask is empty -> match only if rest of mask '*****'...
        while cm^ <> #0 do begin
          if cm^ <> '*' then exit;
          inc(cm);
        end;
        break;
      end;
      case cm^ of
        '*': if (cm + 1)^ <> #0 then begin        // '*' found -> match if '*' is last char of mask
               b1 := true;                        // else continue testing...
               inc(cm);                           // memoryMask   -> first char after '*'
               ms := cs + 1;                      // memoryString -> currentPos + 1
               mm := cm;
             end else break;
        '?': begin
               inc(cs); inc(cm);                  // '?' simply means, we can skip one char in both string and mask
             end;
        CSpecialCharBegin:                        // ooops... now it gets more difficult...  :-)
             begin
               if not ParseSpecialString(false, cm, cset, b2, sc) then exit;  // wrong special character syntax !!
               for i1 := 1 to sc do
                 if (cs^ in cset) = b2 then begin // current char does match ?
                   if b1 then begin               // it does not, but we have already found a '*' some time ago...
                     cm := mm;                    // continue with memoryMask /
                     cs := ms;                    //                            memoryString /
                     inc(ms);                     //                                           inc(memoryString)
                     break;                       // break the special loop...
                   end else exit;                 // it does not match, and we had no '*' yet, so -> no match
                 end else inc(cs);                // current char matches, so move string cursors + 1
             end;
        else if cm^ <> cs^ then begin             // current char does match ?
               if b1 then begin                   // it does not, but we have already found a '*' some time ago...
                 cm := mm;                        // continue with memoryMask /
                 cs := ms;                        //                            memoryString /
                 inc(ms);                         //                                           inc(memoryString)
               end else exit;                     // it does not match, and we had no '*' yet, so -> no match
             end else begin
               inc(cs); inc(cm);                  // current char matches, so move both cursors + 1
             end;
      end;
    end;
    result := true;
  end;
end;

function TextMatchEx(const str, mask: AnsiString) : boolean;
var cs, cm : PAnsiChar;
    ms, mm : PAnsiChar;
    b1     : boolean;
    cset   : TSChar;
    b2     : boolean;
    sc     : integer;
    i1     : integer;
begin
  cm := PAnsiChar(mask);
  result := (cm^ = '*') and ((cm + 1)^ = #0);
  if not result then begin
    cs := PAnsiChar(str);
    ms := cs;
    mm := cm;
    b1 := false;
    while (cm^ <> #0) or (cs^ <> #0) do begin
      if cm^ = #0 then exit;
      if cs^ = #0 then begin
        while cm^ <> #0 do begin
          if cm^ <> '*' then exit;
          inc(cm);
        end;
        break;
      end;
      case cm^ of
        '*': if (cm + 1)^ <> #0 then begin
               b1 := true;
               inc(cm);
               ms := cs + 1;
               mm := cm;
             end else break;
        '?': begin
               inc(cs); inc(cm);
             end;
        CSpecialCharBegin:
             begin
               if not ParseSpecialString(true, cm, cset, b2, sc) then exit;
               for i1 := 1 to sc do
                 if (lowCharTable[cs^] in cset) = b2 then begin
                   if b1 then begin
                     cm := mm;
                     cs := ms;
                     inc(ms);
                     break;
                   end else exit;
                 end else inc(cs);
             end;
        else if lowCharTable[cm^] <> lowCharTable[cs^] then begin
               if b1 then begin
                 cm := mm;
                 cs := ms;
                 inc(ms);
               end else exit;
             end else begin
               inc(cs); inc(cm);
             end;
      end;
    end;
    result := true;
  end;
end;

function FileMatch(const file_, mask: AnsiString) : boolean;
var f1, f2, m1, m2 : AnsiString;
    i1             : integer;
begin
  if mask <> '*.*' then begin
    i1 := PosStrA('.', file_, maxInt, 1);
    if i1 > 0 then begin
      f1 := Copy(file_, 1,      i1 - 1);
      f2 := Copy(file_, i1 + 1, maxInt);
    end else begin
      f1 := file_;
      f2 := '';
    end;
    i1 := PosStrA('.', mask, maxInt, 1);
    if i1 > 0 then begin
      m1 := Copy(mask, 1,      i1 - 1);
      m2 := Copy(mask, i1 + 1, maxInt);
    end else begin
      m1 := mask;
      m2 := '';
    end;
    result := TextMatchA(file_, mask) or (TextMatchA(f2, m2) and TextMatchA(f1, m1));
  end else
    result := true;
end;

{$ifdef UnicodeOverloads}
function FileMatch(const file_, mask: UnicodeString) : boolean;
{$else}
function FileMatchW(const file_, mask: UnicodeString) : boolean;
{$endif}
var f1, f2, m1, m2 : UnicodeString;
    i1             : integer;
begin
  if mask <> '*.*' then begin
    i1 := PosStrW('.', file_, maxInt, 1);
    if i1 > 0 then begin
      f1 := Copy(file_, 1,      i1 - 1);
      f2 := Copy(file_, i1 + 1, maxInt);
    end else begin
      f1 := file_;
      f2 := '';
    end;
    i1 := PosStrW('.', mask, maxInt, 1);
    if i1 > 0 then begin
      m1 := Copy(mask, 1,      i1 - 1);
      m2 := Copy(mask, i1 + 1, maxInt);
    end else begin
      m1 := mask;
      m2 := '';
    end;
    result := TextMatchW(file_, mask) or (TextMatchW(f2, m2) and TextMatchW(f1, m1));
  end else
    result := true;
end;

procedure _FillStrA(var str: AnsiString; fillLen: integer; addLeft: boolean; fillChar: AnsiChar); {$ifdef UnicodeOverloads} overload; {$endif}
var s1 : AnsiString;
begin
  if fillLen > 0 then begin
    SetLength(s1, fillLen);
    system.FillChar(pointer(s1)^, fillLen, byte(fillChar));
    if addLeft then begin
      if (fillChar in ['0'..'9']) and (str <> '') and (str[1] = '-') then
        str := '-' + s1 + RetDeleteA(str, 1, 1)
      else
        str := s1 + str;
    end else
      str := str + s1;
  end;
end;

procedure _FillStrW(var str: UnicodeString; fillLen: integer; addLeft: boolean; fillChar: WideChar); overload;
var s1 : UnicodeString;
    i1 : integer;
begin
  if fillLen > 0 then begin
    SetLength(s1, fillLen);
    for i1 := 1 to fillLen do
      s1[i1] := fillChar;
    if addLeft then begin
      if (word(fillChar) and $ff00 = 0) and (AnsiChar(fillChar) in ['0'..'9']) and (str <> '') and (str[1] = '-') then
        str := '-' + s1 + RetDeleteW(str, 1, 1)
      else
        str := s1 + str;
    end else
      str := str + s1;
  end;
end;

function FillStr(const str: AnsiString; minLen: integer; fillChar: AnsiChar = ' ') : AnsiString;
begin
  result := str;
  _FillStrA(result, abs(minLen) - Length(result), minLen > 0, fillChar);
end;

{$ifdef UnicodeOverloads}
function FillStr(const str: UnicodeString; minLen: integer; fillChar: WideChar = ' ') : UnicodeString;
{$else}
function FillStrW(const str: UnicodeString; minLen: integer; fillChar: WideChar = ' ') : UnicodeString;
{$endif}
begin
  result := str;
  _FillStrW(result, abs(minLen) - Length(result), minLen > 0, fillChar);
end;

function IntToStr32A(value: integer) : AnsiString;
var i1, i2 : integer;
    b1     : boolean;
begin
  if value <> 0 then begin
    b1 := value < 0;
    SetLength(result, 11);
    i1 := 11;
    repeat
      i2    := abs(value mod 10);
      value :=     value div 10;
      result[i1] := AnsiChar(ord('0') + i2);
      dec(i1);
    until value = 0;
    if b1 then begin
      result[i1] := '-';
      dec(i1);
    end;
    if i1 > 0 then begin
      Move(result[i1 + 1], result[1], 11 - i1);
      SetLength(result, 11 - i1);
    end;
  end else
    result := '0';
end;

function IntToStr32W(value: integer) : UnicodeString;
var i1, i2 : integer;
    b1     : boolean;
begin
  if value <> 0 then begin
    b1 := value < 0;
    SetLength(result, 11);
    i1 := 11;
    repeat
      i2    := abs(value mod 10);
      value :=     value div 10;
      result[i1] := WideChar(ord('0') + i2);
      dec(i1);
    until value = 0;
    if b1 then begin
      result[i1] := '-';
      dec(i1);
    end;
    if i1 > 0 then begin
      Move(result[i1 + 1], result[1], (11 - i1) * 2);
      SetLength(result, 11 - i1);
    end;
  end else
    result := '0';
end;

function IntToStr64A(value: int64) : AnsiString; overload;
var i1, i2 : integer;
    b1     : boolean;
begin
  if value <> 0 then begin
    b1 := value < 0;
    SetLength(result, 20);
    i1 := 20;
    repeat
      i2    := abs(value mod 10);
      value :=     value div 10;
      result[i1] := AnsiChar(ord('0') + i2);
      dec(i1);
    until value = 0;
    if b1 then begin
      result[i1] := '-';
      dec(i1);
    end;
    if i1 > 0 then begin
      Move(result[i1 + 1], result[1], 20 - i1);
      SetLength(result, 20 - i1);
    end;
  end else
    result := '0';
end;

function IntToStr64W(value: int64) : UnicodeString; overload;
var i1, i2 : integer;
    b1     : boolean;
begin
  if value <> 0 then begin
    b1 := value < 0;
    SetLength(result, 20);
    i1 := 20;
    repeat
      i2    := abs(value mod 10);
      value :=     value div 10;
      result[i1] := WideChar(ord('0') + i2);
      dec(i1);
    until value = 0;
    if b1 then begin
      result[i1] := '-';
      dec(i1);
    end;
    if i1 > 0 then begin
      Move(result[i1 + 1], result[1], (20 - i1) * 2);
      SetLength(result, 20 - i1);
    end;
  end else
    result := '0';
end;

function IntToHex32A(value: integer) : AnsiString; overload;
var c1, c2, c3 : cardinal;
begin
  if value <> 0 then begin
    c3 := cardinal(value);
    SetLength(result, 8);
    c1 := 8;
    repeat
      c2 := c3 mod $10;
      c3 := c3 div $10;
      if c2 > 9 then result[c1] := AnsiChar(ord('a') + c2 - $A)
      else           result[c1] := AnsiChar(ord('0') + c2 -  0);
      dec(c1);
    until c3 = 0;
    if c1 > 0 then begin
      Move(result[c1 + 1], result[1], 8 - c1);
      SetLength(result, 8 - c1);
    end;
  end else
    result := '0';
end;

function IntToHex32W(value: integer) : UnicodeString; overload;
var c1, c2, c3 : cardinal;
begin
  if value <> 0 then begin
    c3 := cardinal(value);
    SetLength(result, 8);
    c1 := 8;
    repeat
      c2 := c3 mod $10;
      c3 := c3 div $10;
      if c2 > 9 then result[c1] := WideChar(ord('a') + c2 - $A)
      else           result[c1] := WideChar(ord('0') + c2 -  0);
      dec(c1);
    until c3 = 0;
    if c1 > 0 then begin
      Move(result[c1 + 1], result[1], (8 - c1) * 2);
      SetLength(result, 8 - c1);
    end;
  end else
    result := '0';
end;

function IntToHex64A(value: int64) : AnsiString; overload;
var splitInt64 : packed record
                   loCard, hiCard : cardinal;
                 end absolute value;
begin
  if value <> 0 then begin
    result := IntToHex32A(integer(splitInt64.loCard));
    if splitInt64.hiCard <> 0 then
      result := IntToHex32A(integer(splitInt64.hiCard)) + FillStrA(result, 8, '0');
  end else
    result := '0';
end;

function IntToHex64W(value: int64) : UnicodeString; overload;
var splitInt64 : packed record
                   loCard, hiCard : cardinal;
                 end absolute value;
begin
  if value <> 0 then begin
    result := IntToHex32W(integer(splitInt64.loCard));
    if splitInt64.hiCard <> 0 then
      result := IntToHex32W(integer(splitInt64.hiCard)) + FillStrW(result, 8, '0');
  end else
    result := '0';
end;

function IntToStrEx(value: integer; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToStr32A(value);
  _FillStrA(result, abs(minLen) - Length(result), minLen > 0, fillChar);
end;

function IntToHexEx(value: integer; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToHex32A(value);
  if (minLen < 0) or (fillChar in ['0'..'9','A'..'F','a'..'f']) then begin
    _FillStrA(result, abs(minLen) - Length(result), minLen > 0, fillChar);
    result := '$' + result;
  end else begin
    result := '$' + result;
    _FillStrA(result, abs(minLen) - Length(result) + 1, true, fillChar);
  end;
end;

function IntToStrEx(value: cardinal; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToStr64A(value);
  _FillStrA(result, abs(minLen) - Length(result), minLen > 0, fillChar);
end;

function IntToStrEx(value: int64; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToStr64A(value);
  _FillStrA(result, abs(minLen) - Length(result), minLen > 0, fillChar);
end;

function IntToHexEx(value: cardinal; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToHex32A(integer(value));
  if (minLen < 0) or (fillChar in ['0'..'9','A'..'F','a'..'f']) then begin
    _FillStrA(result, abs(minLen) - Length(result), minLen > 0, fillChar);
    result := '$' + result;
  end else begin
    result := '$' + result;
    _FillStrA(result, abs(minLen) - Length(result) + 1, true, fillChar);
  end;
end;

function IntToHexEx(value: int64; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToHex64A(value);
  if (minLen < 0) or (fillChar in ['0'..'9','A'..'F','a'..'f']) then begin
    _FillStrA(result, abs(minLen) - Length(result), minLen > 0, fillChar);
    result := '$' + result;
  end else begin
    result := '$' + result;
    _FillStrA(result, abs(minLen) - Length(result) + 1, true, fillChar);
  end;
end;

function IntToStrExA(value: integer; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToStrEx(value, minLen, fillChar);
end;

function IntToHexExA(value: integer; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToHexEx(value, minLen, fillChar);
end;

function IntToStrExA(value: cardinal; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToStrEx(value, minLen, fillChar);
end;

function IntToStrExA(value: int64; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToStrEx(value, minLen, fillChar);
end;

function IntToHexExA(value: cardinal; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToHexEx(value, minLen, fillChar);
end;

function IntToHexExA(value: int64; minLen: integer = 1; fillChar: AnsiChar = '0') : AnsiString;
begin
  result := IntToHexEx(value, minLen, fillChar);
end;

function IntToStrExW(value: integer; minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString;
begin
  result := IntToStr32W(value);
  _FillStrW(result, abs(minLen) - Length(result), minLen > 0, fillChar);
end;

function IntToHexExW(value: integer; minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString;
begin
  result := IntToHex32W(value);
  if (minLen < 0) or ((WideChar(AnsiChar(fillChar)) = fillChar) and (AnsiChar(fillChar) in ['0'..'9','A'..'F','a'..'f'])) then begin
    _FillStrW(result, abs(minLen) - Length(result), minLen > 0, fillChar);
    result := '$' + result;
  end else begin
    result := '$' + result;
    _FillStrW(result, abs(minLen) - Length(result) + 1, true, fillChar);
  end;
end;

function IntToStrExW(value: cardinal; minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString;
begin
  result := IntToStr64W(value);
  _FillStrW(result, abs(minLen) - Length(result), minLen > 0, fillChar);
end;

function IntToStrExW(value: int64; minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString;
begin
  result := IntToStr64W(value);
  _FillStrW(result, abs(minLen) - Length(result), minLen > 0, fillChar);
end;

function IntToHexExW(value: cardinal; minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString;
begin
  result := IntToHex32W(integer(value));
  if (minLen < 0) or ((WideChar(AnsiChar(fillChar)) = fillChar) and (AnsiChar(fillChar) in ['0'..'9','A'..'F','a'..'f'])) then begin
    _FillStrW(result, abs(minLen) - Length(result), minLen > 0, fillChar);
    result := '$' + result;
  end else begin
    result := '$' + result;
    _FillStrW(result, abs(minLen) - Length(result) + 1, true, fillChar);
  end;
end;

function IntToHexExW(value: int64; minLen: integer = 1; fillChar: WideChar = '0') : UnicodeString;
begin
  result := IntToHex64W(value);
  if (minLen < 0) or ((WideChar(AnsiChar(fillChar)) = fillChar) and (AnsiChar(fillChar) in ['0'..'9','A'..'F','a'..'f'])) then begin
    _FillStrW(result, abs(minLen) - Length(result), minLen > 0, fillChar);
    result := '$' + result;
  end else begin
    result := '$' + result;
    _FillStrW(result, abs(minLen) - Length(result) + 1, true, fillChar);
  end;
end;

var valTable : array [AnsiChar] of byte =
  ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$00,$00,$00,$00,$00,$00,
   $00,$0A,$0B,$0C,$0D,$0E,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$0A,$0B,$0C,$0D,$0E,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);

function StrToIntEx(hex: boolean; str: PAnsiChar; len: integer) : integer;
var b1 : boolean;
    i1 : integer;
begin
  result := 0;
  if str <> nil then begin
    b1 := str^ = '-';
    if not b1 then
      result := valTable[str^];
    inc(str);
    if hex then begin
      for i1 := 1 to len - 1 do begin
        result := (result shl 4) + valTable[str^];
        inc(str);
      end;
    end else
      for i1 := 1 to len - 1 do begin
        result := (result * 10) + valTable[str^];
        inc(str);
      end;
    if b1 then
      result := -result;
  end;
end;

{$ifdef UnicodeOverloads}
function StrToIntEx(hex: boolean; str: PWideChar; len: integer) : integer;
{$else}
function StrToIntExW(hex: boolean; str: PWideChar; len: integer) : integer;
{$endif}
var b1 : boolean;
    i1 : integer;
begin
  result := 0;
  if str <> nil then begin
    b1 := str^ = '-';
    if not b1 then
      result := valTable[AnsiChar(str^)];
    inc(str);
    if hex then begin
      for i1 := 1 to len - 1 do begin
        result := (result shl 4) + valTable[AnsiChar(str^)];
        inc(str);
      end;
    end else
      for i1 := 1 to len - 1 do begin
        result := (result * 10) + valTable[AnsiChar(str^)];
        inc(str);
      end;
    if b1 then
      result := -result;
  end;
end;

procedure FormatSubStrs(var str: AnsiString; delimiter: AnsiChar = '|');
var c1, c2 : cardinal;
    chs    : TSChar;
begin
  chs := [#32..#255] - [' ',delimiter];
  c1 := PosCharsA(chs, str);
  if c1 <> 0 then begin
    c2 := PosCharsA(chs, str, maxInt, 1);
    KeepA(str, c1, c2 - c1 + 1);
    c1 := PosStrA(delimiter, str);
    while c1 > 0 do begin
      c2 := PosCharsA(chs, str, c1 + 1);
      if c2 - c1 > 1 then
        Delete(str, c1 + 1, c2 - c1 - 1);
      c2 := PosCharsA(chs, str, c1 - 1, 1);
      if c1 - c2 > 1 then begin
        Delete(str, c2 + 1, c1 - c2 - 1);
        c1 := c2 + 1;
      end;
      c1 := PosStrA(delimiter, str, c1 + 2);
    end;
  end else
    str := '';
end;

{$ifdef UnicodeOverloads}
procedure FormatSubStrs(var str: UnicodeString; delimiter: AnsiChar = '|');
{$else}
procedure FormatSubStrsW(var str: UnicodeString; delimiter: AnsiChar = '|');
{$endif}
var c1, c2 : cardinal;
    chs    : TSChar;
begin
  chs := [#0..#31] + [' ', delimiter];
  c1 := PosNotCharsW(chs, str);
  if c1 <> 0 then begin
    c2 := PosNotCharsW(chs, str, maxInt, 1);
    KeepW(str, c1, c2 - c1 + 1);
    c1 := PosStrW(WideChar(delimiter), str);
    while c1 > 0 do begin
      c2 := PosNotCharsW(chs, str, c1 + 1);
      if c2 - c1 > 1 then
        Delete(str, c1 + 1, c2 - c1 - 1);
      c2 := PosNotCharsW(chs, str, c1 - 1, 1);
      if c1 - c2 > 1 then begin
        Delete(str, c2 + 1, c1 - c2 - 1);
        c1 := c2 + 1;
      end;
      c1 := PosStrW(WideChar(delimiter), str, c1 + 2);
    end;
  end else
    str := '';
end;

function SubStrCount(const str: AnsiString; delimiter: AnsiChar = '|') : integer;
var i1 : integer;
begin
  if str <> '' then begin
    result := 1;
    for i1 := 1 to length(str) do
      if str[i1] = delimiter then
        inc(result);
  end else
    result := 0;
end;

{$ifdef UnicodeOverloads}
function SubStrCount(const str: UnicodeString; delimiter: AnsiChar = '|') : integer;
{$else}
function SubStrCountW(const str: UnicodeString; delimiter: AnsiChar = '|') : integer;
{$endif}
var i1  : integer;
    ch1 : WideChar;
begin
  if str <> '' then begin
    result := 1;
    ch1 := WideChar(delimiter);
    for i1 := 1 to length(str) do
      if str[i1] = ch1 then
        inc(result);
  end else
    result := 0;
end;

function SubStr(const str: AnsiString; index: cardinal; delimiter: AnsiChar = '|') : AnsiString;
var c1, c2 : cardinal;
begin
  result := '';
  if (str <> '') and (index >= 1) then begin
    c2 := 0;
    repeat
      dec(index);
      c1 := c2 + 1;
      c2 := PosStrA(delimiter, str, c1);
    until (index = 0) or (c2 = 0);
    if index = 0 then
      if c2 = 0 then
        result := Copy(str, c1, maxInt )
      else
        result := Copy(str, c1, c2 - c1);
  end;
end;

{$ifdef UnicodeOverloads}
function SubStr(const str: UnicodeString; index: cardinal; delimiter: AnsiChar = '|') : UnicodeString;
{$else}
function SubStrW(const str: UnicodeString; index: cardinal; delimiter: AnsiChar = '|') : UnicodeString;
{$endif}
var c1, c2 : cardinal;
begin
  result := '';
  if (str <> '') and (index >= 1) then begin
    c2 := 0;
    repeat
      dec(index);
      c1 := c2 + 1;
      c2 := PosStrW(WideChar(delimiter), str, c1);
    until (index = 0) or (c2 = 0);
    if index = 0 then
      if c2 = 0 then
        result := Copy(str, c1, maxInt )
      else
        result := Copy(str, c1, c2 - c1);
  end;
end;

function SubStrExists(const str: AnsiString; const subStr: AnsiString; delimiter: AnsiChar = '|') : boolean;
var i1, lstr, lsub : integer;
begin
  if subStr <> '' then begin
    lstr := Length(str);
    lsub := Length(subStr);
    i1 := -lsub;
    repeat
      inc(i1, lsub);
      i1 := PosStrA(subStr, str, i1 + 1);
      result := (i1 > 0) and
                ( (i1 =               1) or (str[i1 -    1] = delimiter) ) and
                ( (i1 = lstr - lsub + 1) or (str[i1 + lsub] = delimiter) );
    until result or (i1 = 0);
  end else
    result := false;
end;

{$ifdef UnicodeOverloads}
function SubStrExists(const str: UnicodeString; const subStr: UnicodeString; delimiter: AnsiChar = '|') : boolean;
{$else}
function SubStrExistsW(const str: UnicodeString; const subStr: UnicodeString; delimiter: AnsiChar = '|') : boolean;
{$endif}
var i1, lstr, lsub : integer;
begin
  if subStr <> '' then begin
    lstr := Length(str);
    lsub := Length(subStr);
    i1 := -lsub;
    repeat
      inc(i1, lsub);
      i1 := PosStrW(subStr, str, i1 + 1);
      result := (i1 > 0) and
                ( (i1 =               1) or (str[i1 -    1] = WideChar(delimiter)) ) and
                ( (i1 = lstr - lsub + 1) or (str[i1 + lsub] = WideChar(delimiter)) );
    until result or (i1 = 0);
  end else
    result := false;
end;

function SubTextExists(const str: AnsiString; const subText: AnsiString; delimiter: AnsiChar = '|') : boolean;
var i1, lstr, lsub : integer;
begin
  if subText <> '' then begin
    lstr := Length(str);
    lsub := Length(subText);
    i1 := -lsub;
    repeat
      inc(i1, lsub);
      i1 := PosTextA(subText, str, i1 + 1);
      result := (i1 > 0) and
                ( (i1 =               1) or (str[i1 -    1] = delimiter) ) and
                ( (i1 = lstr - lsub + 1) or (str[i1 + lsub] = delimiter) );
    until result or (i1 = 0);
  end else
    result := false;
end;

{$ifdef UnicodeOverloads}
function SubTextExists(const str: UnicodeString; const subText: UnicodeString; delimiter: AnsiChar = '|') : boolean;
{$else}
function SubTextExistsW(const str: UnicodeString; const subText: UnicodeString; delimiter: AnsiChar = '|') : boolean;
{$endif}
var i1, lstr, lsub : integer;
begin
  if subText <> '' then begin
    lstr := Length(str);
    lsub := Length(subText);
    i1 := -lsub;
    repeat
      inc(i1, lsub);
      i1 := PosTextW(subText, str, i1 + 1);
      result := (i1 > 0) and
                ( (i1 =               1) or (str[i1 -    1] = WideChar(delimiter)) ) and
                ( (i1 = lstr - lsub + 1) or (str[i1 + lsub] = WideChar(delimiter)) );
    until result or (i1 = 0);
  end else
    result := false;
end;

var FDecSepA : AnsiChar = #0;
function DecSepA : AnsiChar;
var buf : array [0..1] of AnsiChar;
begin
  if FDecSepA = #0 then
    if GetLocaleInfoA(GetThreadLocale, LOCALE_SDECIMAL, buf, 2) > 0 then
      FDecSepA := buf[0]
    else
      FDecSepA := ',';
  result := FDecSepA;
end;

var FDecSepW : WideChar = #0;
function DecSepW : WideChar;
var buf : array [0..1] of WideChar;
begin
  if FDecSepW = #0 then
    if GetLocaleInfoW(GetThreadLocale, LOCALE_SDECIMAL, buf, 2) > 0 then
      FDecSepW := buf[0]
    else
      FDecSepW := ',';
  result := FDecSepW;
end;

function SizeToStr(size: int64) : AnsiString;
begin
  if abs(size) >= 1024 then begin
    if abs(size) >= 1024 * 1024 then begin
      if abs(size) >= 1024 * 1024 * 1024 then begin
        result := IntToStrEx(abs(size div 1024 div 1024 * 100 div 1024)) + ' GB';
        Insert(AnsiString(DecSepA), result, Length(result) - 4);
      end else begin
        result := IntToStrEx(abs(size div 1024 * 100 div 1024)) + ' MB';
        Insert(AnsiString(DecSepA), result, Length(result) - 4);
      end;
    end else begin
      result := IntToStrEx(abs(size * 100 div 1024)) + ' KB';
      Insert(AnsiString(DecSepA), result, Length(result) - 4);
    end;
  end else
    result := IntToStrExA(abs(size)) + ' Bytes';
end;

function MsToStr(time: cardinal) : AnsiString;
begin
  if time >= 1000 then begin
    if time >= 1000 * 60 then begin
      if time >= 1000 * 60 * 60 then begin
        time := time div (1000 * 60);
        result := IntToStrEx(time mod 60);
        if Length(result) = 1 then result := '0' + result;
        result := IntToStrEx(time div 60) + ':' + result + ' h';
      end else begin
        time := time div 1000;
        result := IntToStrEx(time mod 60);
        if Length(result) = 1 then result := '0' + result;
        result := IntToStrEx(time div 60) + ':' + result + ' min';
      end;
    end else begin
      result := IntToStrEx(time mod 1000 div 10);
      if Length(result) = 1 then result := '0' + result;
      result := IntToStrEx(time div 1000) + DecSepA + result + ' s';
    end;
  end else
    result := IntToStrEx(time) + ' ms';
end;

function ErrorCodeToStr(error: cardinal) : AnsiString;
const
  NERR_BASE              = 2100;
  MAX_NERR               = NERR_BASE + 899;
  WINHTTP_ERROR_BASE     = 12000;
  WINHTTP_ERROR_LAST     = WINHTTP_ERROR_BASE + 186;
  CNetMsg                : AnsiString = (* netmsg.dll            *)  #$3B#$30#$21#$38#$26#$32#$7B#$31#$39#$39;
  CWinHttp               : AnsiString = (* winhttp.dll           *)  #$22#$3C#$3B#$3D#$21#$21#$25#$7B#$31#$39#$39;
  CRtlNtStatusToDosError : AnsiString = (* RtlNtStatusToDosError *)  #$07#$21#$39#$1B#$21#$06#$21#$34#$21#$20#$26#$01#$3A#$11#$3A#$26#$10#$27#$27#$3A#$27;
  CWinErrNr              : AnsiString = (* Windows error number  *)  #$02#$3C#$3B#$31#$3A#$22#$26#$75#$30#$27#$27#$3A#$27#$75#$3B#$20#$38#$37#$30#$27;
var pc1   : PAnsiChar;
    c1    : cardinal;
    dll   : HMODULE;
    flags : cardinal;
    ns2de : function (ntstatus: dword) : dword; stdcall;
begin
  if error and $c0000000 <> 0 then begin
    ns2de := GetProcAddress(GetModuleHandleA(PAnsiChar(DecryptStr(CNtDll))), PAnsiChar(DecryptStr(CRtlNtStatusToDosError)));
    if @ns2de <> nil then
      error := ns2de(error);
  end;
  flags := FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_FROM_SYSTEM;
  if (error >= NERR_BASE) and (error <= MAX_NERR) then begin
    // network error codes are in this dll...
    dll := LoadLibraryExA(PAnsiChar(DecryptStr(CNetMsg)), 0, LOAD_LIBRARY_AS_DATAFILE);
    if dll <> 0 then
      flags := flags or FORMAT_MESSAGE_FROM_HMODULE;
  end else
    if (error >= WINHTTP_ERROR_BASE) and (error <= WINHTTP_ERROR_LAST) then begin
      // winhttp error codes are in this dll...
      dll := LoadLibraryExA(PAnsiChar(DecryptStr(CWinHttp)), 0, LOAD_LIBRARY_AS_DATAFILE);
      if dll <> 0 then
        flags := flags or FORMAT_MESSAGE_FROM_HMODULE;
    end else
      dll := 0;
  try
    if FormatMessageA(flags, pointer(dll), error, SUBLANG_DEFAULT shl 10 or LANG_NEUTRAL, @pc1, 0, nil) <> 0 then begin
      try
        result := pc1;
        c1 := Length(result);
        while c1 > 0 do begin
          if      result[c1] = #$D then result[c1] := ' '
          else if result[c1] = #$A then Delete(result, c1, 1);
          dec(c1);
        end;
      finally LocalFree(NativeUInt(pc1)) end;
    end else
      result := DecryptStr(CWinErrNr) + ' ' + IntToHexExA(error);
  finally
    if dll <> 0 then
      FreeLibrary(dll);
  end;
end;

function SizeToStrW(size: int64) : UnicodeString;
begin
  if abs(size) >= 1024 then begin
    if abs(size) >= 1024 * 1024 then begin
      if abs(size) >= 1024 * 1024 * 1024 then begin
        result := IntToStrExW(abs(size div 1024 div 1024 * 100 div 1024)) + ' GB';
        Insert(UnicodeString(DecSepW), result, Length(result) - 4);
      end else begin
        result := IntToStrExW(abs(size div 1024 * 100 div 1024)) + ' MB';
        Insert(UnicodeString(DecSepW), result, Length(result) - 4);
      end;
    end else begin
      result := IntToStrExW(abs(size * 100 div 1024)) + ' KB';
      Insert(UnicodeString(DecSepW), result, Length(result) - 4);
    end;
  end else
    result := IntToStrExW(abs(size)) + ' Bytes';
end;

function MsToStrW(time: cardinal) : UnicodeString;
begin
  if time >= 1000 then begin
    if time >= 1000 * 60 then begin
      if time >= 1000 * 60 * 60 then begin
        time := time div (1000 * 60);
        result := IntToStrExW(time mod 60);
        if Length(result) = 1 then result := '0' + result;
        result := IntToStrExW(time div 60) + ':' + result + ' h';
      end else begin
        time := time div 1000;
        result := IntToStrExW(time mod 60);
        if Length(result) = 1 then result := '0' + result;
        result := IntToStrExW(time div 60) + ':' + result + ' min';
      end;
    end else begin
      result := IntToStrExW(time mod 1000 div 10);
      if Length(result) = 1 then result := '0' + result;
      result := IntToStrExW(time div 1000) + DecSepW + result + ' s';
    end;
  end else
    result := IntToStrExW(time) + ' ms';
end;

function ErrorCodeToStrW(error: cardinal) : UnicodeString;
const
  NERR_BASE              = 2100;
  MAX_NERR               = NERR_BASE + 899;
  CNetMsg                : AnsiString = (* netmsg.dll            *)  #$3B#$30#$21#$38#$26#$32#$7B#$31#$39#$39;
  CRtlNtStatusToDosError : AnsiString = (* RtlNtStatusToDosError *)  #$07#$21#$39#$1B#$21#$06#$21#$34#$21#$20#$26#$01#$3A#$11#$3A#$26#$10#$27#$27#$3A#$27;
  CWinErrNr              : AnsiString = (* Windows error number  *)  #$02#$3C#$3B#$31#$3A#$22#$26#$75#$30#$27#$27#$3A#$27#$75#$3B#$20#$38#$37#$30#$27;
var pc1   : PWideChar;
    c1    : cardinal;
    dll   : HMODULE;
    flags : cardinal;
    ns2de : function (ntstatus: dword) : dword; stdcall;
begin
  if GetVersion and $80000000 <> 0 then begin
    result := UnicodeString(ErrorCodeToStrA(error));
    exit;
  end;
  if error and $c0000000 <> 0 then begin
    ns2de := GetProcAddress(GetModuleHandleA(PAnsiChar(DecryptStr(CNtDll))), PAnsiChar(DecryptStr(CRtlNtStatusToDosError)));
    if @ns2de <> nil then
      error := ns2de(error);
  end;
  flags := FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_FROM_SYSTEM;
  if (error >= NERR_BASE) and (error <= MAX_NERR) then begin
    // network error codes are in this dll...
    dll := LoadLibraryExA(PAnsiChar(DecryptStr(CNetMsg)), 0, LOAD_LIBRARY_AS_DATAFILE);
    if dll <> 0 then
      flags := flags or FORMAT_MESSAGE_FROM_HMODULE;
  end else
    dll := 0;
  try
    if FormatMessageW(flags, pointer(dll), error, SUBLANG_DEFAULT shl 10 or LANG_NEUTRAL, @pc1, 0, nil) <> 0 then begin
      try
        result := pc1;
        c1 := Length(result);
        while c1 > 0 do begin
          if      result[c1] = #$D then result[c1] := ' '
          else if result[c1] = #$A then Delete(result, c1, 1);
          dec(c1);
        end;
      finally LocalFree(NativeUInt(pc1)) end;
    end else
      result := UnicodeString(DecryptStr(CWinErrNr)) + ' ' + IntToHexExW(error);
  finally
    if dll <> 0 then
      FreeLibrary(dll);
  end;
end;

// ***************************************************************

function DecryptStr(const str: AnsiString) : AnsiString;
var i1 : integer;
begin
  result := str;
  UniqueString(result);
  for i1 := 1 to Length(result) do
    byte(result[i1]) := byte(result[i1]) xor $55;
end;

function AnsiToWideEx(const ansi: AnsiString; addTerminatingZero: boolean = true) : AnsiString;
var pwc : PWideChar;
    i1  : integer;
begin
  SetLength(result, Length(ansi) * 2);
  pwc := pointer(result);
  for i1 := 1 to Length(ansi) do begin
    pwc^ := WideChar(ansi[i1]);
    inc(pwc);
  end;
  if addTerminatingZero then
    result := result + #0#0;
end;

function WideToAnsiEx(wide: PWideChar) : AnsiString;
var i1 : integer;
begin
  SetLength(result, lstrlenW(wide));
  for i1 := 1 to Length(result) do
    result[i1] := AnsiChar(wide[i1 - 1]);
end;

var
  WideCharToMultiByte : function (codePage, flags: dword; src: PWideChar; srcLen: integer; dst: PAnsiChar; dstLen: integer; default: PAnsiChar; defaultUsed: PBOOL) : integer; stdcall;
  MultiByteToWideChar : function (codePage, flags: dword; src: PAnsiChar; srcLen: integer; dst: PWideChar; dstLen: integer) : integer; stdcall;

function DecodeUtf8(const s: AnsiString) : UnicodeString;

  function Utf8ToUnicode_Legacy(dst: PWideChar; dstLen: dword; src: PAnsiChar; srcLen: dword) : dword;
  var i1, wc : dword;
  begin
    result := 0;
    i1 := 0;
    while (i1 < srcLen) and (result < dstLen) do begin
      wc := dword(src[i1]);
      inc(i1);
      if  (wc and $80 <> 0) and  (i1     < srcLen) and (byte(src[i1    ]) and $c0 = $80) and
         ((wc and $20 =  0) or  ((i1 + 1 < srcLen) and (byte(src[i1 + 1]) and $c0 = $80))) then begin
        wc := wc and $3f;
        if wc and $20 <> 0 then begin
          wc := (wc shl 6) or (byte(src[i1]) and $3f);
          inc(i1);
        end;
        dst[result] := WideChar((wc shl 6) or (byte(src[i1]) and $3f));
        inc(i1);
      end else
        dst[result] := WideChar(wc);
      inc(result);
    end;
    if result >= dstLen then
      result := dstLen - 1;
    dst[result] := #0;
    inc(result);
  end;

var len : integer;
begin
  if s <> '' then begin
    SetLength(result, Length(s));
    if (@@MultiByteToWideChar = nil) and (GetVersion and $80000000 = 0) then
      MultiByteToWideChar := GetProcAddress(GetModuleHandle(kernel32), 'MultiByteToWideChar');
    if @MultiByteToWideChar <> nil then begin
      len := MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(s), Length(s) + 1, PWideChar(result), Length(s) + 1);
      if (len > 0) and (len <= Length(s) + 1) and (PWideChar(result)[len - 1] <> #0) then begin
        if len = Length(s) + 1 then begin
          if (len > 1) and (word(PWideChar(result)[len - 1]) >= $DC00) and (word(PWideChar(result)[len - 1]) <= $DFFF) then
            dec(len);
        end else
          inc(len);
        PWideChar(result)[len - 1] := #0;
      end;
    end else
      len := Utf8ToUnicode_Legacy(PWideChar(result), Length(result) + 1, PAnsiChar(s), length(s));
    if len > 1 then
      SetLength(result, len - 1)
    else
      result := '';
  end else
    result := '';
end;

function EncodeUtf8(const s: UnicodeString) : AnsiString;

  function UnicodeToUtf8_Legacy(dst: PAnsiChar; dstLen: Cardinal; src: PWideChar; srcLen: Cardinal): Cardinal;
  var i1, wc : dword;
  begin
    result := 0;
    i1 := 0;
    while (i1 < srcLen) and (result < dstLen) do begin
      wc := dword(src[i1]);
      inc(i1);
      if wc <= $7f then begin
        dst[result] := AnsiChar(wc);
        inc(result);
      end else
        if wc <= $7ff then begin
          dst[result    ] := AnsiChar($c0 or (wc shr 6));
          dst[result + 1] := AnsiChar($80 or (wc and $3f));
          inc(result, 2);
        end else begin
          dst[result    ] := AnsiChar($e0 or (wc shr 12));
          dst[result + 1] := AnsiChar($80 or ((wc shr 6) and $3f));
          dst[result + 2] := AnsiChar($80 or (wc and $3f));
          inc(result, 3);
        end;
    end;
    if result >= dstLen then
      result := dstLen - 1;
    dst[result] := #0;
    inc(result);
  end;

var len : integer;
begin
  if s <> '' then begin
    SetLength(result, Length(s) * 3);
    if (@WideCharToMultiByte = nil) and (GetVersion and $80000000 = 0) then
      WideCharToMultiByte := GetProcAddress(GetModuleHandle(kernel32), 'WideCharToMultiByte');
    if @WideCharToMultiByte <> nil then begin
      len := WideCharToMultiByte(CP_UTF8, 0, PWideChar(s), Length(s) + 1, PAnsiChar(result), Length(result) + 1, nil, nil);
      if (len > 0) and (len <= Length(result) + 1) and (PAnsiChar(result)[len - 1] <> #0) then begin
        if len = Length(result) + 1 then begin
          while (len > 1) and (byte(PAnsiChar(result)[len - 1]) > $7F) and (byte(PAnsiChar(result)[len - 1]) and $80 <> 0) and (byte(PAnsiChar(result)[len - 1]) and $C0 <> $C0) do
            Dec(len);
        end else
          inc(len);
        PAnsiChar(result)[len - 1] := #0;
      end;
    end else
      len := UnicodeToUtf8_Legacy(PAnsiChar(result), Length(result) + 1, PWideChar(s), Length(s));
    if len > 1 then
      SetLength(result, len - 1)
    else
      result := '';
  end else
    result := '';
end;

function IsValidIdentW(const ident: UnicodeString; allowDots: boolean = false) : boolean;
var i1 : integer;
begin
  result := (ident <> '') and ((word(ident[1]) and $ff00 <> 0) or (AnsiChar(ident[1]) in ['A'..'Z', 'a'..'z', '_']));
  if result then
    for i1 := 2 to Length(ident) do
      if (word(ident[i1]) and $ff00 = 0) and (not (AnsiChar(ident[i1]) in ['A'..'Z', 'a'..'z', '_', '0'..'9'])) and ((ident[i1] <> '.') or (not allowDots)) then begin
        result := false;
        break;
      end;
end;

function ExtractFileNameW(const str: UnicodeString) : UnicodeString;
var i1 : integer;
begin
  result := str;
  for i1 := Length(result) downto 1 do
    if result[i1] = '\' then begin
      Delete(result, 1, i1);
      break;
    end;
end;

function ExtractFilePathW(const str: UnicodeString) : UnicodeString;
var i1 : integer;
begin
  result := str;
  for i1 := Length(result) downto 1 do
    if result[i1] = '\' then begin
      Delete(result, i1 + 1, maxInt);
      exit;
    end;
  result := '';
end;

function ExtractFileDriveW(const str: UnicodeString) : UnicodeString;
var i1, i2 : integer;
begin
  result := str;
  if Length(result) >= 2 then
    if (result[1] = '\') and (result[2] = '\') then begin
      i2 := 0;
      for i1 := 4 to Length(result) do
        if result[i1] = '\' then begin
          inc(i2);
          if i2 = 2 then begin
            Delete(result, i1, maxInt);
            break;
          end;
        end;
      result := result + '\';
    end else
      if result[2] = ':' then
        result := result[1] + ':\';
end;

function ExtractFileExtW(const str: UnicodeString) : UnicodeString;
var i1 : integer;
begin
  result := '';
  for i1 := Length(str) downto 1 do
    if str[i1] = '.' then begin
      result := Copy(str, i1, maxInt);
      break;
    end;
end;

end.
