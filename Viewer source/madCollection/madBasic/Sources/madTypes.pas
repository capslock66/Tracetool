// ***************************************************************
//  madTypes.pas              version: 1.5.1  ·  date: 2012-06-12
//  -------------------------------------------------------------
//  general purpose types
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2012 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2012-06-12 1.5.1 improved (Mad)Exception class logic
// 2012-04-03 1.5.0 added x64 support
// 2009-02-09 1.4d  (1) Delphi 2009 support
//                  (2) char/string -> changed to Ansi/Wide
// 2000-07-25 1.4c  MadException/TMethod added (to get rid of SysUtils)

unit madTypes;

{$I mad.inc}

interface

uses Windows;

// ***************************************************************

{$ifndef UNICODE}
  type UnicodeString = WideString;
{$endif}

// ***************************************************************

{$ifndef xe2}
  type
    NativeUInt = cardinal;
    NativeInt  = integer;
{$endif}

// ***************************************************************

const
  maxCard         = high(cardinal);

type
  TExtBool        = (no, yes, other);

  // TSxxx = set of xxx
  TSByte          = set of byte;
  TSChar          = set of AnsiChar;
  TSBoolean       = set of boolean;
  TSExtBool       = set of TExtBool;

  // TPSxxx = ^(set of xxx)
  TPSByte         = ^TSByte;
  TPSChar         = ^TSChar;
  TPSBoolean      = ^TSBoolean;
  TPSExtBool      = ^TSExtBool;

  // TPxxx = ^xxx
  TPByte          = ^byte;
  TPShortInt      = ^shortInt;
  TPChar          = ^Char;
  TPAnsiChar      = ^AnsiChar;
  TPWideChar      = ^WideChar;
  TPBoolean       = ^boolean;
  TPExtBool       = ^TExtBool;
  TPWord          = ^word;
  TPSmallInt      = ^smallInt;
  TPCardinal      = ^cardinal;
  TPInteger       = ^integer;
  TPNativeUInt    = ^NativeUInt;
  TPNativeInt     = ^NativeInt;
  TPPointer       = ^pointer;
  TPString        = ^string;
  TPAnsiString    = ^AnsiString;
  TPWideString    = ^WideString;
  TPUnicodeString = ^UnicodeString;
  TPIUnknown      = ^IUnknown;
  TPInt64         = ^int64;

  // TAxxx = array [0..maxPossible] of xxx
  TAByte          = array [0..maxInt      -1] of byte;
  TAShortInt      = array [0..maxInt      -1] of shortInt;
  TAChar          = array [0..maxInt div sizeOf(Char)-1] of Char;
  TAAnsiChar      = array [0..maxInt      -1] of AnsiChar;
  TAWideChar      = array [0..maxInt shr 1-1] of WideChar;
  TABoolean       = array [0..maxInt      -1] of boolean;
  TAExtBool       = array [0..maxInt      -1] of TExtBool;
  TAWord          = array [0..maxInt shr 1-1] of word;
  TASmallInt      = array [0..maxInt shr 1-1] of smallInt;
  TACardinal      = array [0..maxInt shr 2-1] of cardinal;
  TAInteger       = array [0..maxInt shr 2-1] of integer;
  {$ifdef win64}
    TANativeUInt    = array [0..maxInt shr 3-1] of NativeUInt;
    TANativeInt     = array [0..maxInt shr 3-1] of NativeInt;
    TAPointer       = array [0..maxInt shr 3-1] of pointer;
    TAString        = array [0..maxInt shr 3-1] of string;
    TAAnsiString    = array [0..maxInt shr 3-1] of AnsiString;
    TAWideString    = array [0..maxInt shr 3-1] of WideString;
    TAUnicodeString = array [0..maxInt shr 3-1] of UnicodeString;
    TAIUnknown      = array [0..maxInt shr 3-1] of IUnknown;
  {$else}
    TANativeUInt    = array [0..maxInt shr 2-1] of NativeUInt;
    TANativeInt     = array [0..maxInt shr 2-1] of NativeInt;
    TAPointer       = array [0..maxInt shr 2-1] of pointer;
    TAString        = array [0..maxInt shr 2-1] of string;
    TAAnsiString    = array [0..maxInt shr 2-1] of AnsiString;
    TAWideString    = array [0..maxInt shr 2-1] of WideString;
    TAUnicodeString = array [0..maxInt shr 2-1] of UnicodeString;
    TAIUnknown      = array [0..maxInt shr 2-1] of IUnknown;
  {$endif}
  TAInt64         = array [0..maxInt shr 3-1] of int64;

  // TPAxxx = ^(array [0..maxPossible] of xxx)
  TPAByte          = ^TAByte;
  TPAShortInt      = ^TAShortInt;
  TPAChar          = ^TAChar;
  TPAAnsiChar      = ^TAAnsiChar;
  TPAWideChar      = ^TAWideChar;
  TPABoolean       = ^TABoolean;
  TPAExtBool       = ^TAExtBool;
  TPAWord          = ^TAWord;
  TPASmallInt      = ^TASmallInt;
  TPACardinal      = ^TACardinal;
  TPAInteger       = ^TAInteger;
  TPANativeUInt    = ^TANativeUInt;
  TPANativeInt     = ^TANativeInt;
  TPAPointer       = ^TAPointer;
  TPAString        = ^TAString;
  TPAAnsiString    = ^TAAnsiString;
  TPAWideString    = ^TAWideString;
  TPAUnicodeString = ^TAUnicodeString;
  TPAIUnknown      = ^TAIUnknown;
  TPAInt64         = ^TAInt64;

  // TDAxxx = array of xxx
  TDAByte          = array of byte;
  TDAShortInt      = array of shortInt;
  TDAChar          = array of Char;
  TDAAnsiChar      = array of AnsiChar;
  TDAWideChar      = array of WideChar;
  TDABoolean       = array of boolean;
  TDAExtBool       = array of TExtBool;
  TDAWord          = array of word;
  TDASmallInt      = array of smallInt;
  TDACardinal      = array of cardinal;
  TDAInteger       = array of integer;
  TDANativeUInt    = array of NativeUInt;
  TDANativeInt     = array of NativeInt;
  TDAPointer       = array of pointer;
  TDAString        = array of string;
  TDAAnsiString    = array of AnsiString;
  TDAWideString    = array of WideString;
  TDAUnicodeString = array of UnicodeString;
  TDAIUnknown      = array of IUnknown;
  TDAInt64         = array of int64;

  // TPDAxxx = ^(array of xxx)
  TPDAByte          = ^TDAByte;
  TPDAShortInt      = ^TDAShortInt;
  TPDAChar          = ^TDAChar;
  TPDAAnsiChar      = ^TDAAnsiChar;
  TPDAWideChar      = ^TDAWideChar;
  TPDABoolean       = ^TDABoolean;
  TPDAExtBool       = ^TDAExtBool;
  TPDAWord          = ^TDAWord;
  TPDASmallInt      = ^TDASmallInt;
  TPDACardinal      = ^TDACardinal;
  TPDAInteger       = ^TDAInteger;
  TPDANativeUInt    = ^TDANativeUInt;
  TPDANativeInt     = ^TDANativeInt;
  TPDAPointer       = ^TDAPointer;
  TPDAString        = ^TDAString;
  TPDAAnsiString    = ^TDAAnsiString;
  TPDAWideString    = ^TDAWideString;
  TPDAUnicodeString = ^TDAUnicodeString;
  TPDAIUnknown      = ^TDAIUnknown;
  TPDAInt64         = ^TDAInt64;

// ***************************************************************

type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode    : dword;
    ExceptionFlags   : dword;
    ExceptionRecord  : PExceptionRecord;
    ExceptionAddress : pointer;
    NumberParameters : dword;
    case {IsOsException:} boolean of
      true:  (ExceptionInformation : array [0..14] of NativeUInt);
      false: (ExceptAddr, ExceptObject: pointer);
  end;
  MadException = class
  public
    Message : string;
    HelpContext : integer;
    {$ifdef d2009}
      InnerException : MadException;
      StackInfo : pointer;
      AcquireInnerException : boolean;
    {$endif}
    class function Create(const msg: string) : MadException;
    destructor Destroy; override;
    {$ifdef d2009}
      class var
        CleanUpStackInfoProc: procedure (Self: TObject);
    {$endif}
  end;

type
  TMethod = record code, data: pointer; end;

// ***************************************************************

implementation

// ***************************************************************

class function MadException.Create(const msg: string) : MadException;
begin
  if ExceptionClass <> nil then
    result := MadException(ExceptionClass.Create)
  else
    result := MadException(TClass(MadException).Create);
  result.Message := msg;
end;

destructor MadException.Destroy;
{$ifdef d2009}
  var obj : TObject;
{$endif}
begin
  {$ifdef d2009}
    obj := InnerException;
    InnerException := nil;
    if obj <> nil then
      obj.Free;
    if Assigned(CleanupStackInfoProc) then
      CleanupStackInfoProc(Self);
  {$endif}
  inherited Destroy;
end;

// ***************************************************************

end.
