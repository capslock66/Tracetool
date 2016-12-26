/// common functions used by most Synopse projects
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCommons;

(*
    This file is part of Synopse framework.
*)


{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
{$ifndef LVCL}
{$ifndef FPC}
{$ifndef HASFASTMM4}
  FastMM4,
{$endif}
{$endif}
{$endif}
{$ifdef MSWINDOWS}
  Windows,
  Messages,
  {$ifndef LVCL}
  Registry,
  {$endif}
{$else MSWINDOWS}
{$ifdef KYLIX3}
  Types,
  LibC,
  SynKylix,
{$endif}
{$ifdef FPC}
  BaseUnix,
{$endif}
{$endif MSWINDOWS}
  Classes,
{$ifndef LVCL}
  SyncObjs, // for TEvent and TCriticalSection
  Contnrs,  // for TObjectList
{$ifdef HASINLINE}
  Types,
{$endif}
{$endif}
{$ifndef NOVARIANTS}
  Variants,
{$endif}
  SysUtils;



{ ************ common types used for compatibility between compilers and CPU }

const
  /// internal Code Page for UTF-16 Unicode encoding
  // - used e.g. for Delphi 2009+ UnicodeString=String type
  CP_UTF16 = 1200;

  /// internal Code Page for UTF-8 Unicode encoding
  CP_UTF8 = 65001;

{$ifdef FPC} { make cross-compiler and cross-CPU types available to Delphi }

type
  PBoolean = ^Boolean;
  {$ifdef BSD}
  TThreadID = Cardinal;
  {$endif}
{$else FPC}

type
  /// a CPU-dependent unsigned integer type cast of a pointer / register
  // - used for 64 bits compatibility, native under Free Pascal Compiler
{$ifdef ISDELPHI2009}
  PtrUInt = cardinal; { see http://synopse.info/forum/viewtopic.php?id=136 }
{$else}
  {$ifdef UNICODE}
  PtrUInt = NativeUInt;
  {$else}
  PtrUInt = cardinal;
  {$endif}
{$endif}
  /// a CPU-dependent unsigned integer type cast of a pointer of pointer
  // - used for 64 bits compatibility, native under Free Pascal Compiler
  PPtrUInt = ^PtrUInt;

  /// a CPU-dependent signed integer type cast of a pointer / register
  // - used for 64 bits compatibility, native under Free Pascal Compiler
{$ifdef ISDELPHI2009}
  PtrInt = integer;
{$else}
  {$ifdef UNICODE}
  PtrInt = NativeInt;
  {$else}
  PtrInt = integer;
  {$endif}
{$endif}
  /// a CPU-dependent signed integer type cast of a pointer of pointer
  // - used for 64 bits compatibility, native under Free Pascal Compiler
  PPtrInt = ^PtrInt;

  /// unsigned Int64 doesn't exist under older Delphi, but is defined in FPC
  // - and UInt64 is buggy as hell under Delphi 2007 when inlining functions
  {$ifdef FPC_OR_UNICODE}
  QWord = UInt64;
  {$else}
  QWord = type Int64;
  {$endif}
  /// points to an unsigned Int64
  PQWord = ^QWord;

  {$ifndef ISDELPHIXE2}
  /// used to store the handle of a system Thread
  TThreadID = cardinal;
  {$endif}

{$endif FPC}

{$ifdef DELPHI6OROLDER}

// some definitions not available prior to Delphi 7
type
  UInt64 = Int64;

{$endif}

{$ifdef DELPHI5OROLDER}
  // Delphi 5 doesn't have those basic types defined :(
const
  varShortInt = $0010;
  varInt64 = $0014; { vt_i8 }
  soBeginning = soFromBeginning;
  soCurrent = soFromCurrent;
  reInvalidPtr = 2;
  PathDelim  = '\';
  sLineBreak = #13#10;

type
  PPointer = ^Pointer;
  PPAnsiChar = ^PAnsiChar;
  PInteger = ^Integer;
  PCardinal = ^Cardinal;
  PWord = ^Word;
  PByte = ^Byte;
  PBoolean = ^Boolean;
  PComp = ^Comp;
  THandle = LongWord;
  PVarData = ^TVarData;
  TVarData = packed record
    // mostly used for varNull, varInt64, varDouble, varString and varAny
    VType: word;
    case Integer of
      0: (Reserved1: Word;
          case Integer of
            0: (Reserved2, Reserved3: Word;
                case Integer of
                  varSmallInt: (VSmallInt: SmallInt);
                  varInteger:  (VInteger: Integer);
                  varSingle:   (VSingle: Single);
                  varDouble:   (VDouble: Double);     // DOUBLE
                  varCurrency: (VCurrency: Currency);
                  varDate:     (VDate: TDateTime);
                  varOleStr:   (VOleStr: PWideChar);
                  varDispatch: (VDispatch: Pointer);
                  varError:    (VError: HRESULT);
                  varBoolean:  (VBoolean: WordBool);
                  varUnknown:  (VUnknown: Pointer);
                  varByte:     (VByte: Byte);
                  varInt64:    (VInt64: Int64);      // INTEGER
                  varString:   (VString: Pointer);   // TEXT
                  varAny:      (VAny: Pointer);
                  varArray:    (VArray: PVarArray);
                  varByRef:    (VPointer: Pointer);
               );
            1: (VLongs: array[0..2] of LongInt); );
  end;
{$endif}

type
  /// a pointer to a PtrUInt array
  TPtrUIntArray = array[0..MaxInt div SizeOf(PtrUInt)-1] of PtrUInt;
  PPtrUIntArray = ^TPtrUIntArray;

  /// a dynamic array of PtrUInt values
  TPtrUIntDynArray = array of PtrUInt;

{$ifndef NOVARIANTS}
  /// a variant values array
  TVariantArray = array[0..MaxInt div SizeOf(Variant)-1] of Variant;
  /// a pointer to a variant array
  PVariantArray = ^TVariantArray;

  /// a dynamic array of variant values
  TVariantDynArray = array of variant;
{$endif}

  /// RawUnicode is an Unicode String stored in an AnsiString
  // - faster than WideString, which are allocated in Global heap (for COM)
  // - an AnsiChar(#0) is added at the end, for having a true WideChar(#0) at ending
  // - length(RawUnicode) returns memory bytes count: use (length(RawUnicode) shr 1)
  // for WideChar count (that's why the definition of this type since Delphi 2009
  // is AnsiString(1200) and not UnicodeString)
  // - pointer(RawUnicode) is compatible with Win32 'Wide' API call
  // - mimic Delphi 2009 UnicodeString, without the WideString or Ansi conversion overhead
  // - all conversion to/from AnsiString or RawUTF8 must be explicit: the
  // compiler is not able to make valid implicit conversion on CP_UTF16
  {$ifdef HASCODEPAGE}
  RawUnicode = type AnsiString(CP_UTF16); // Codepage for an UnicodeString
  {$else}
  RawUnicode = type AnsiString;
  {$endif}

  /// RawUTF8 is an UTF-8 String stored in an AnsiString
  // - use this type instead of System.UTF8String, which behavior changed
  // between Delphi 2009 compiler and previous versions: our implementation
  // is consistent and compatible with all versions of Delphi compiler
  // - mimic Delphi 2009 UTF8String, without the charset conversion overhead
  // - all conversion to/from AnsiString or RawUnicode must be explicit
  RawUTF8 = type AnsiString;

  /// WinAnsiString is a WinAnsi-encoded AnsiString (code page 1252)
  // - use this type instead of System.String, which behavior changed
  // between Delphi 2009 compiler and previous versions: our implementation
  // is consistent and compatible with all versions of Delphi compiler
  // - all conversion to/from RawUTF8 or RawUnicode must be explicit
  WinAnsiString = type AnsiString;

  {$ifdef HASCODEPAGE}
  {$ifdef FPC}
  // missing declaration
  PRawByteString = ^RawByteString;
  {$endif}
  {$else}
  /// define RawByteString, as it does exist in Delphi 2009+
  // - to be used for byte storage into an AnsiString
  // - use this type if you don't want the Delphi compiler not to do any
  // code page conversions when you assign a typed AnsiString to a RawByteString,
  // i.e. a RawUTF8 or a WinAnsiString
  RawByteString = type AnsiString;
  /// pointer to a RawByteString
  PRawByteString = ^RawByteString;
  {$endif}

  /// SynUnicode is the fastest available Unicode native string type, depending
  //  on the compiler used
  // - this type is native to the compiler, so you can use Length() Copy() and
  //   such functions with it (this is not possible with RawUnicodeString type)
  // - before Delphi 2009+, it uses slow OLE compatible WideString
  //   (with our Enhanced RTL, WideString allocation can be made faster by using
  //   an internal caching mechanism of allocation buffers - WideString allocation
  //   has been made much faster since Windows Vista/Seven)
  // - starting with Delphi 2009, it uses fastest UnicodeString type, which
  //   allow Copy On Write, Reference Counting and fast heap memory allocation
  {$ifdef UNICODE}
  SynUnicode = UnicodeString;
  {$else}
  SynUnicode = WideString;
  {$endif}

  PRawUnicode = ^RawUnicode;
  PRawUTF8 = ^RawUTF8;
  PWinAnsiString = ^WinAnsiString;
  PWinAnsiChar = type PAnsiChar;
  PSynUnicode = ^SynUnicode;

  /// a simple wrapper to UTF-8 encoded zero-terminated PAnsiChar
  // - PAnsiChar is used only for Win-Ansi encoded text
  // - the Synopse mORMot framework uses mostly this PUTF8Char type,
  // because all data is internaly stored and expected to be UTF-8 encoded
  PUTF8Char = type PAnsiChar;
  PPUTF8Char = ^PUTF8Char;

  PObject = ^TObject;

  TInt64Array = array[0..MaxInt div SizeOf(Int64)-1] of Int64;
  PInt64Array = ^TInt64Array;

  TPtrIntArray = array[0..MaxInt div SizeOf(PtrInt)-1] of PtrInt;
  PPtrIntArray = ^TPtrIntArray;

  TObjectArray = array [0..MaxInt div SizeOf(TObject)-1] of TObject;
  PObjectArray = ^TObjectArray;

  TPointerDynArray = array of pointer;
  PPointerDynArray = ^TPointerDynArray;

  TObjectDynArray = array of TObject;
  PObjectDynArray = ^TObjectDynArray;

  TCardinalArray = array[0..MaxInt div SizeOf(cardinal)-1] of cardinal;
  PCardinalArray = ^TCardinalArray;

  {$ifndef DELPHI5OROLDER}
  /// a dynamic array of interface values
  TInterfaceDynArray = array of IInterface;
  PInterfaceDynArray = ^TInterfaceDynArray;
  {$endif}

{ note: those VariantToInteger*() functions are expected to be there }

/// convert any numerical Variant into a 32 bit integer
// - it will expect true numerical Variant and won't convert any string nor
// floating-pointer Variant, which will return FALSE and won't change the
// Value variable content
function VariantToInteger(const V: Variant; var Value: integer): boolean;

/// convert any numerical Variant into a 64 bit integer
// - it will expect true numerical Variant and won't convert any string nor
// floating-pointer Variant, which will return FALSE and won't change the
// Value variable content
function VariantToInt64(const V: Variant; var Value: Int64): boolean;

/// convert any numerical Variant into a 64 bit integer
// - it will expect true numerical Variant and won't convert any string nor
// floating-pointer Variant, which will return the supplied DefaultValue
function VariantToInt64Def(const V: Variant; DefaultValue: Int64): Int64;

/// convert any numerical Variant into a floating point value
function VariantToDouble(const V: Variant; var Value: double): boolean;

/// convert any numerical Variant into a fixed decimals floating point value
function VariantToCurrency(const V: Variant; var Value: currency): boolean;

/// convert any numerical Variant into a boolean value
function VariantToBoolean(const V: Variant; var Value: Boolean): boolean;

/// convert any numerical Variant into an integer
// - it will expect true numerical Variant and won't convert any string nor
// floating-pointer Variant, which will return the supplied DefaultValue
function VariantToIntegerDef(const V: Variant; DefaultValue: integer): integer; overload;


type
  /// function prototype used internally for UTF-8 buffer comparaison
  // - used in mORMot.pas unit during TSQLTable rows sort and by TSQLQuery
  TUTF8Compare = function(P1,P2: PUTF8Char): PtrInt;

/// convert the endianness of a given unsigned 32 bit integer into BigEndian
function bswap32(a: cardinal): cardinal;

{$ifndef ISDELPHI2007ANDUP}
type
  TBytes = array of byte;
{$endif}

/// fast concatenation of several AnsiStrings
function RawByteStringArrayConcat(const Values: array of RawByteString): RawByteString;

/// creates a TBytes from a RawByteString memory buffer
procedure RawByteStringToBytes(const buf: RawByteString; out bytes: TBytes);

/// creates a RawByteString memory buffer from a TBytes content
procedure BytesToRawByteString(const bytes: TBytes; out buf: RawByteString);
  {$ifdef HASINLINE}inline;{$endif}

/// creates a RawByteString memory buffer from an embedded resource
// - returns '' if the resource is not found
// - warning: resources size may be rounded up to alignment
procedure ResourceToRawByteString(const ResName: string; ResType: PChar;
  out buf: RawByteString);


var
  /// best possible precision when rendering a "single" kind of float
  // - can be used as parameter for ExtendedToString/ExtendedToStr
  // - is defined as a var, so that you may be able to override the default
  // settings, for the whole process
  SINGLE_PRECISION: integer = 8;
  /// best possible precision when rendering a "double" kind of float
  // - can be used as parameter for ExtendedToString/ExtendedToStr
  // - is defined as a var, so that you may be able to override the default
  // settings, for the whole process
  DOUBLE_PRECISION: integer = 15;
  /// best possible precision when rendering a "extended" kind of float
  // - can be used as parameter for ExtendedToString/ExtendedToStr
  // - is defined as a var, so that you may be able to override the default
  // settings, for the whole process
  EXTENDED_PRECISION: integer = 18;

type
  {$ifdef CPUARM}
  // ARM does not support 80bit extended -> 64bit double is enough for us
  TSynExtended = double;
  {$else}
  {$ifdef CPU64}
  TSynExtended = double;
  {$else}
  /// the floating-point type to be used for best precision and speed
  // - will allow to fallback to double e.g. on x64 and ARM CPUs
  TSynExtended = extended;
  {$endif}
  {$endif}
  /// the non-number values potentially stored in an IEEE floating point 
  TSynExtendedNan = (seNumber, seNan, seInf, seNegInf);

/// convert a floating-point value to its numerical text equivalency
// - returns the count of chars stored into S (S[0] is not set)
function ExtendedToString(var S: ShortString; Value: TSynExtended; Precision: integer): integer;



{$ifdef PUREPASCAL}
/// inlined StrComp(), to be used with PUTF8Char/PAnsiChar
function StrComp(Str1, Str2: pointer): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// buffer-safe version of StrComp(), to be used with PUTF8Char/PAnsiChar
// - pure pascal StrComp() won't access the memory beyond the string, but this
// function is defined for compatibility with SSE 4.2 expectations
function StrCompFast(Str1, Str2: pointer): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}
{$else}

/// x86 asm version of StrComp(), to be used with PUTF8Char/PAnsiChar
// - this version won't access the memory beyond the string, so may be
// preferred to StrCompSSE42 or StrComp, when using e.g. mapped files
function StrCompFast(Str1, Str2: pointer): PtrInt;

/// SSE 4.2 version of StrComp(), to be used with PUTF8Char/PAnsiChar
// - please note that this optimized version may read up to 15 bytes
// beyond the string; this is rarely a problem but it can in principle
// generate a protection violation (e.g. when used over mapped files) - in this
// case, you can use the slightly slower StrCompFast() function instead
function StrCompSSE42(Str1, Str2: pointer): PtrInt;

/// fastest available version of StrComp(), to be used with PUTF8Char/PAnsiChar
// - will use SSE4.2 instructions on supported CPUs - and potentiall read up
// to 15 bytes beyond the string: use StrCompFast() for a safe memory read
var StrComp: function (Str1, Str2: pointer): PtrInt = StrCompFast;

{$endif}

/// use our fast version of StrIComp2(), to be used with PUTF8Char/PAnsiChar
function StrIComp2(Str1, Str2: pointer): PtrInt;
  {$ifdef PUREPASCAL} {$ifdef HASINLINE}inline;{$endif} {$endif}

/// slower version of StrLen(), but which will never read beyond the string
// - this version won't access the memory beyond the string, so may be
// preferred to StrLen(), when using e.g. mapped files or any memory
// protected buffer
function StrLenPas(S: pointer): PtrInt;




/// our fast version of move()
// - this version will use fast SSE2 instructions (if available), on both Win32
// and Win64 platforms, or an optimized X86 revision on older CPUs
//var MoveFast: procedure(const Source; var Dest; Count: PtrInt);

/// our fast version of StrLen(), to be used with PWideChar
function StrLenW(S: PWideChar): PtrInt;

/// use our fast version of StrComp(), to be used with PWideChar
function StrCompW(Str1, Str2: PWideChar): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}


/// get the signed 32-bit integer value stored in P^
// - we use the PtrInt result type, even if expected to be 32-bit, to use
// native CPU register size (don't want any 32-bit overflow here)
// - it will stop the parsing when P^ does not contain numbers any more
function GetInteger(P: PUTF8Char): PtrInt; overload;

/// get the signed 32-bit integer value stored in P^
// - if P if nil or not start with a valid numerical value, returns Default
function GetIntegerDef(P: PUTF8Char; Default: PtrInt): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// get the signed 32-bit integer value stored in P^
// - this version return 0 in err if no error occured, and 1 if an invalid
// character was found, not its exact index as for the val() function
function GetInteger(P: PUTF8Char; var err: integer): PtrInt; overload;

/// get the unsigned 32-bit integer value stored in P^
// - we use the PtrUInt result type, even if expected to be 32-bit, to use
// native CPU register size (don't want any 32-bit overflow here)
function GetCardinal(P: PUTF8Char): PtrUInt;

/// get the unsigned 32-bit integer value stored in P^
// - if P if nil or not start with a valid numerical value, returns Default
function GetCardinalDef(P: PUTF8Char; Default: PtrUInt): PtrUInt;

/// get the unsigned 32-bit integer value stored as Unicode string in P^
function GetCardinalW(P: PWideChar): PtrUInt;


/// get the 64-bit integer value stored in P^
function GetInt64(P: PUTF8Char): Int64; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// get the 64-bit integer value stored in P^
// - if P if nil or not start with a valid numerical value, returns Default
function GetInt64Def(P: PUTF8Char; const Default: Int64): Int64;

/// get the 64-bit integer value stored in P^
procedure SetInt64(P: PUTF8Char; var result: Int64);
  {$ifdef CPU64}inline;{$endif}

/// get the 64-bit integer value stored in P^
// - set the err content to the index of any faulty character, 0 if conversion
// was successful (same as the standard val function)
function GetInt64(P: PUTF8Char; var err: integer): Int64; overload;
  {$ifdef CPU64}inline;{$endif}



/// get the unsigned 32-bit cardinal value stored in a RawUTF8 string
// - returns TRUE if the supplied text was successfully converted into a cardinal
function ToCardinal(const text: RawUTF8; out value: cardinal; minimal: cardinal=0): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// get the signed 64-bit integer value stored in a RawUTF8 string
// - returns TRUE if the supplied text was successfully converted into an Int64
function ToInt64(const text: RawUTF8; out value: Int64): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// returns TRUE if the given text buffer contains A..Z,0..9,_ characters
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
// - i.e. can be tested via IdemPropName*() functions, and the MongoDB-like
// extended JSON syntax as generated by dvoSerializeAsExtendedJson
// - first char must be alphabetical or '_', following chars can be
// alphanumerical or '_'
function PropNameValid(P: PUTF8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}


/// case unsensitive test of P1 and P2 content
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
function IdemPropName(const P1,P2: shortstring): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// case unsensitive test of P1 and P2 content
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
// - this version expects P2 to be a PAnsiChar with a specified length
function IdemPropName(const P1: shortstring; P2: PUTF8Char; P2Len: integer): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// case unsensitive test of P1 and P2 content
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
// - this version expects P1 and P2 to be a PAnsiChar with specified lengths
function IdemPropName(P1,P2: PUTF8Char; P1Len,P2Len: integer): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// case unsensitive test of P1 and P2 content
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
// - this version expects P2 to be a PAnsiChar with specified length
function IdemPropNameU(const P1: RawUTF8; P2: PUTF8Char; P2Len: integer): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// case unsensitive test of P1 and P2 content of same length
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
// - this version expects P1 and P2 to be a PAnsiChar with an already checked
// identical length, so may be used for a faster process, e.g. in a loop
// - if P1 and P2 are RawUTF8, you should better call overloaded function
// IdemPropNameU(const P1,P2: RawUTF8), which would be slightly faster by
// using the length stored before the actual text buffer of each RawUTF8
function IdemPropNameUSameLen(P1,P2: PUTF8Char; P1P2Len: integer): boolean;
  {$ifdef PUREPASCAL}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// case unsensitive test of P1 and P2 content
// - use it with property names values (i.e. only including A..Z,0..9,_ chars)
function IdemPropNameU(const P1,P2: RawUTF8): boolean; overload;
  {$ifdef PUREPASCAL}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// return the index of Value in Values[], -1 if not found
// - here name search would use fast IdemPropNameU() function
function FindPropName(const Names: array of RawUTF8; const Name: RawUTF8): integer;

/// fill already allocated Reversed[] so that Reversed[Values[i]]=i
procedure Reverse(const Values: TIntegerDynArray; ValuesCount: integer;
  Reversed: PIntegerArray);

/// fill some values with i,i+1,i+2...i+Count-1
procedure FillIncreasing(Values: PIntegerArray; StartValue, Count: integer);
/// quick helper to initialize a dynamic array of integer from some constants
// - can be used e.g. as:
// ! MyArray := TIntegerDynArrayFrom([1,2,3]);
function TIntegerDynArrayFrom(const Values: array of integer): TIntegerDynArray; 

/// quick helper to initialize a dynamic array of 64-bit integers from 32-bit values
function TInt64DynArrayFrom(const Values: TIntegerDynArray): TInt64DynArray;


{ ************ low-level RTTI types and conversion routines ***************** }

type

  /// internal enumeration used to specify some standard Delphi arrays
  // - will be used e.g. to match JSON serialization or TDynArray search
  // (see TDynArray and TDynArrayHash InitSpecific method)
  // - djBoolean would generate an array of JSON boolean values
  // - djByte .. djTimeLog match numerical JSON values
  // - djDateTime .. djSynUnicode match textual JSON values
  // - djVariant will match standard variant JSON serialization (including
  // TDocVariant or other custom types, if any)
  // - djCustom will be used for registered JSON serializer (invalid for
  // InitSpecific methods call)
  // - see also djPointer and djObject constant aliases for a pointer or
  // TObject field hashing / comparison
  // - is used also by TDynArray.InitSpecific() to define the main field type
  TDynArrayKind = (
    djNone,
    djBoolean, djByte, djWord, djInteger, djCardinal, djSingle,
    djInt64, djDouble, djCurrency,
    djTimeLog, djDateTime, djRawUTF8, djWinAnsi, djString, djRawByteString,
    djWideString, djSynUnicode, djInterface,
    {$ifndef NOVARIANTS}djVariant,{$endif}
    djCustom);

  /// internal set to specify some standard Delphi arrays
  TDynArrayKinds = set of TDynArrayKind;

const
  /// TDynArrayKind alias for a pointer field hashing / comparison
  djPointer = {$ifdef CPU64}djInt64{$else}djCardinal{$endif};

  /// TDynArrayKind alias for a TObject field hashing / comparison
  djObject = djPointer;

type
  /// a wrapper around a dynamic array with one dimension
  // - provide TList-like methods using fast RTTI information
  // - can be used to fast save/retrieve all memory content to a TStream
  // - note that the "const Elem" is not checked at compile time nor runtime:
  // you must ensure that Elem matchs the element type of the dynamic array
  // - can use external Count storage to make Add() and Delete() much faster
  // (avoid most reallocation of the memory buffer)
  // - Note that TDynArray is just a wrapper around an existing dynamic array:
  // methods can modify the content of the associated variable but the TDynArray
  // doesn't contain any data by itself. It is therefore aimed to initialize
  // a TDynArray wrapper on need, to access any existing dynamic array.
  // - is defined either as an object either as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  {$ifdef UNDIRECTDYNARRAY}
  TDynArray = record
  private
  {$else}
  TDynArray = object
  protected
  {$endif}
    fValue: PPointer;
    fTypeInfo: pointer;
    fElemSize: PtrUInt;
    fElemType: pointer;
    fCountP: PInteger;
    fKnownSize: integer;
    fKnownType: TDynArrayKind;
    fIsObjArray: (oaUnknown, oaTrue, oaFalse);
    function GetCount: integer; {$ifdef HASINLINE}inline;{$endif}
    procedure SetCount(aCount: integer);
    function GetCapacity: integer;
    procedure SetCapacity(aCapacity: integer);
    function GetArrayTypeName: RawUTF8;
    function GetIsObjArray: boolean; {$ifdef HASINLINE}inline;{$endif}
    procedure SetIsObjArray(aValue: boolean); {$ifdef HASINLINE}inline;{$endif}
    /// will set fKnownType and fKnownOffset/fKnownSize fields
    function ToKnownType(exactType: boolean=false): TDynArrayKind;
    /// faster than System.DynArraySetLength() function + handle T*ObjArray
    procedure InternalSetLength(NewLength: PtrUInt);
  public
    /// initialize the wrapper with a one-dimension dynamic array
    // - the dynamic array must have been defined with its own type
    // (e.g. TIntegerDynArray = array of Integer)
    // - if aCountPointer is set, it will be used instead of length() to store
    // the dynamic array items count - it will be much faster when adding
    // elements to the array, because the dynamic array won't need to be
    // resized each time - but in this case, you should use the Count property
    // instead of length(array) or high(array) when accessing the data: in fact
    // length(array) will store the memory size reserved, not the items count
    // - if aCountPointer is set, its content will be set to 0, whatever the
    // array length is, or the current aCountPointer^ value is
    // - a sample usage may be:
    // !var DA: TDynArray;
    // !    A: TIntegerDynArray;
    // !begin
    // !  DA.Init(TypeInfo(TIntegerDynArray),A);
    // ! (...)
    // - a sample usage may be (using a count variable):
    // !var DA: TDynArray;
    // !    A: TIntegerDynArray;
    // !    ACount: integer;
    // !    i: integer;
    // !begin
    // !  DA.Init(TypeInfo(TIntegerDynArray),A,@ACount);
    // !  for i := 1 to 100000 do
    // !    DA.Add(i); // MUCH faster using the ACount variable
    // ! (...)   // now you should use DA.Count or Count instead of length(A)
    procedure Init(aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil);
    /// initialize the wrapper with a one-dimension dynamic array
    // - this version accepts to specify how comparison should occur, using
    // TDynArrayKind  kind of first field
    // - djNone and djCustom are too vague, and will raise an exception
    // - no RTTI check is made over the corresponding array layout: you shall
    // ensure that the aKind parameter matches the dynamic array element definition
    // - aCaseInsensitive will be used for djRawUTF8..djSynUnicode comparison
    procedure InitSpecific(aTypeInfo: pointer; var aValue; aKind: TDynArrayKind;
      aCountPointer: PInteger=nil; aCaseInsensitive: boolean=false);
    /// define the reference to an external count integer variable
    // - Init and InitSpecific methods will reset the aCountPointer to 0: you
    // can use this method to set the external count variable without overriding
    // the current value
    procedure UseExternalCount(var aCountPointer: Integer);
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize the wrapper to point to no dynamic array
    procedure Void;
    /// check if the wrapper points to a dynamic array
    function IsVoid: boolean;
    /// add an element to the dynamic array
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Add(i+10) e.g.)
    // - returns the index of the added element in the dynamic array
    // - note that because of dynamic array internal memory managment, adding
    // will be a bit slower than e.g. with a TList: the list is reallocated
    // every time a record is added - but in practice, with FastMM4 or
    // SynScaleMM, there is no big speed penalty - for even better speed, you
    // can also specify an external count variable in Init(...,@Count) method
    function Add(const Elem): integer;
    /// add an element to the dynamic array
    // - this version add a void element to the array, and returns its index
    function New: integer;
    /// add an element to the dynamic array at the position specified by Index
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Insert(10,i+10) e.g.)
    procedure Insert(Index: Integer; const Elem);
    /// delete the whole dynamic array content
    // - this method will recognize T*ObjArray types and free all instances
    procedure Clear;
      {$ifdef HASINLINE}inline;{$endif}
    /// delete the whole dynamic array content, ignoring exceptions
    // - returns true if no exception occured when calling Clear, false otherwise
    // - you should better not call this method, which will catch and ignore
    // all exceptions - but it may somewhat make sense in a destructor
    // - this method will recognize T*ObjArray types and free all instances
    function ClearSafe: boolean;
    /// delete one item inside the dynamic array
    // - the deleted element is finalized if necessary
    // - this method will recognize T*ObjArray types and free all instances
    procedure Delete(aIndex: Integer);
    /// returns a pointer to an element of the array
    // - returns nil if aIndex is out of range
    // - since TDynArray is just a wrapper around an existing array, you should
    // better use direct access to its wrapped variable, and not using this
    // slower and more error prone method (such pointer access lacks of strong
    // typing abilities), which was designed for TDynArray internal use
    function ElemPtr(aIndex: integer): pointer;
    /// search for an element value inside the dynamic array
    // - return the index found (0..Count-1), or -1 if Elem was not found
    // - will search for all properties content of the eLement: TList.IndexOf()
    // searches by address, this method searches by content using the RTTI
    // element description (and not the Compare property function)
    // - use the Find() method if you want the search via the Compare property
    // function, or e.g. to search only with some part of the element content
    // - will work with simple types: binaries (byte, word, integer, Int64,
    // Currency, array[0..255] of byte, packed records with no reference-counted
    // type within...), string types (e.g. array of string), and packed records
    // with binary and string types within (like TFileVersion)
    // - won't work with not packed types (like a shorstring, or a record
    // with byte or word fields with {$A+}): in this case, the padding data
    // (i.e. the bytes between the aligned feeds can be filled as random, and
    // there is no way with standard RTTI do know which they are)
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write IndexOf(i+10) e.g.)
    function IndexOf(const Elem): integer;

    ///  select a sub-section (slice) of a dynamic array content
    procedure Slice(var Dest; aCount: Cardinal; aFirstIndex: cardinal=0);
    /// add elements from a given dynamic array variable
    // - the supplied source DynArray MUST be of the same exact type as the
    // current used for this TDynArray - warning: pass here a reference to
    // a "array of ..." variable, not another TDynArray instance; if you
    // want to add another TDynArray, use AddDynArray() method
    // - you can specify the start index and the number of items to take from
    // the source dynamic array (leave as -1 to add till the end)
    procedure AddArray(const DynArrayVar; aStartIndex: integer=0; aCount: integer=-1);
    {$ifndef DELPHI5OROLDER}
    /// add elements from a given TDynArray
    // - the supplied source TDynArray MUST be of the same exact type as the
    // current used for this TDynArray, otherwise it won't do anything
    // - you can specify the start index and the number of items to take from
    // the source dynamic array (leave as -1 to add till the end)
    procedure AddDynArray(const aSource: TDynArray; aStartIndex: integer=0; aCount: integer=-1);

    {$endif}
    /// compare the content of two elements, returning TRUE if both values equal
    // - this method compares first using any supplied Compare property,
    // then by content using the RTTI element description of the whole record
    function ElemEquals(const A,B): boolean;
    /// will reset the element content
    procedure ElemClear(var Elem);
    /// will copy one element content
    procedure ElemCopy(const A; var B);
    /// load an array element as saved by the ElemSave method
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    procedure ElemLoad(Source: PAnsiChar; var Elem); overload;
    /// load an array element as saved by the ElemSave method
    // - this overloaded method will retrieve the element as a memory buffer
    // and caller MUST call ElemLoadClear() method to finalize its content
    function ElemLoad(Source: PAnsiChar): RawByteString; overload;
    /// release memory allocated by ElemLoad(): RawByteString
    procedure ElemLoadClear(var ElemLoaded: RawByteString);
    /// search for an array element as saved by the ElemSave method
    // - same as ElemLoad() + Find()/IndexOf() + ElemLoadClear()
    // - will call Find() method if Compare property is set
    // - will call generic IndexOf() method if no Compare property is set
    function ElemLoadFind(Source: PAnsiChar): integer;

    /// retrieve or set the number of elements of the dynamic array
    // - same as length(DynArray) or SetLenght(DynArray)
    // - this property will recognize T*ObjArray types, so will free any stored
    // instance if the array is sized down
    property Count: integer read GetCount write SetCount;
    /// the internal buffer capacity
    // - if no external Count pointer was set with Init, is the same as Count
    // - if an external Count pointer is set, you can set a value to this
    // property before a massive use of the Add() method e.g.
    // - if no external Count pointer is set, set a value to this property
    // will affect the Count value, i.e. Add() will append after this count
    // - this property will recognize T*ObjArray types, so will free any stored
    // instance if the array is sized down
    property Capacity: integer read GetCapacity write SetCapacity;
    /// low-level direct access to the storage variable
    property Value: PPointer read fValue;
    /// the known type, possibly retrieved from dynamic array RTTI
    property KnownType: TDynArrayKind read fKnownType;
    /// the known RTTI information of the whole array
    property ArrayType: pointer read fTypeInfo;
    /// the known type name of the whole array
    property ArrayTypeName: RawUTF8 read GetArrayTypeName;
    /// the internal in-memory size of one element, as retrieved from RTTI
    property ElemSize: PtrUInt read fElemSize;
    /// the internal type information of one element, as retrieved from RTTI
    property ElemType: pointer read fElemType;
    /// if this dynamic aray is a T*ObjArray
    property IsObjArray: boolean read GetIsObjArray write SetIsObjArray;
  end;

  /// function prototype to be used for hashing of an element
  // - it must return a cardinal hash, with as less collision as possible
  // - a good candidate is our crc32() function in optimized asm in SynZip unit
  // - TDynArrayHashed.Init will use crc32c() if no custom function is supplied,
  // which will run either as software or SSE4.2 hardware
  THasher = function(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;

  /// function prototype to be used for hashing of a dynamic array element
  // - this function must use the supplied hasher on the Elem data
  TDynArrayHashOne = function(const Elem; Hasher: THasher): cardinal;

  /// event handler to be used for hashing of a dynamic array element
  // - can be set as an alternative to TDynArrayHashOne
  TEventDynArrayHashOne = function(const Elem): cardinal of object;

  /// internal structure used to store one item hash
  // - used e.g. by TDynArrayHashed or TObjectHash via TSynHashDynArray
  TSynHash = record
    /// unsigned integer hash of the item
    Hash: cardinal;
    /// index of the item in the main storage array
    Index: cardinal;
  end;


  /// store one Name/Value pair, as used by TSynNameValue class
  TSynNameValueItem = record
    /// the name of the Name/Value pair
    // - this property is hashed by TSynNameValue for fast retrieval
    Name: RawUTF8;
    /// the value of the Name/Value pair
    Value: RawUTF8;
    /// any associated Pointer or numerical value
    Tag: PtrInt;
  end;

  /// Name/Value pairs storage, as used by TSynNameValue class
  TSynNameValueItemDynArray = array of TSynNameValueItem;

  /// event handler used to convert on the fly some UTF-8 text content
  TConvertRawUTF8 = function(const text: RawUTF8): RawUTF8 of object;

  /// callback event used by TSynNameValue
  TSynNameValueNotify = procedure(const Item: TSynNameValueItem; Index: PtrInt) of object;


/// wrapper to add an item to a array of pointer dynamic array storage
function PtrArrayAdd(var aPtrArray; aItem: pointer): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// wrapper to delete an item from a array of pointer dynamic array storage
function PtrArrayDelete(var aPtrArray; aItem: pointer): integer;

/// wrapper to add an item to a T*ObjArray dynamic array storage
// - as expected by TJSONSerializer.RegisterObjArrayForJSON()
// - could be used as such (note the T*ObjArray type naming convention):
// ! TUserObjArray = array of TUser;
// ! ...
// ! var arr: TUserObjArray;
// !     user: TUser;
// ! ..
// ! try
// !   user := TUser.Create;
// !   user.Name := 'Name';
// !   index := ObjArrayAdd(arr,user);
// ! ...
// ! finally
// !   ObjArrayClear(arr); // release all items
// ! end;
// - return the index of the item in the dynamic array
function ObjArrayAdd(var aObjArray; aItem: TObject): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// wrapper to add once an item to a T*ObjArray dynamic array storage
// - as expected by TJSONSerializer.RegisterObjArrayForJSON()
// - if the object is already in the array (searching by address/reference,
// not by content), return its current index in the dynamic array
// - if the object does not appear in the array, add it at the end
procedure ObjArrayAddOnce(var aObjArray; aItem: TObject);

/// wrapper to set the length of a T*ObjArray dynamic array storage
// - could be used as an alternative to SetLength() when you do not
// know the exact T*ObjArray type
procedure ObjArraySetLength(var aObjArray; aLength: integer);
  {$ifdef HASINLINE}inline;{$endif}

/// wrapper to search an item in a T*ObjArray dynamic array storage
// - as expected by TJSONSerializer.RegisterObjArrayForJSON()
// - search is performed by address/reference, not by content
// - returns -1 if the item is not found in the dynamic array
function ObjArrayFind(const aObjArray; aItem: TObject): integer;


/// wrapper to delete an item in a T*ObjArray dynamic array storage
// - as expected by TJSONSerializer.RegisterObjArrayForJSON()
// - do nothing if the index is out of range in the dynamic array
procedure ObjArrayDelete(var aObjArray; aItemIndex: integer;
  aContinueOnException: boolean=false); overload;

/// wrapper to delete an item in a T*ObjArray dynamic array storage
// - as expected by TJSONSerializer.RegisterObjArrayForJSON()
// - search is performed by address/reference, not by content
// - do nothing if the item is not found in the dynamic array
function ObjArrayDelete(var aObjArray; aItem: TObject): integer; overload;

/// wrapper to release all items stored in a T*ObjArray dynamic array
// - as expected by TJSONSerializer.RegisterObjArrayForJSON()
// - you should always use ObjArrayClear() before the array storage is released,
// e.g. in the owner class destructor
// - will also set the dynamic array length to 0, so could be used to re-use
// an existing T*ObjArray
procedure ObjArrayClear(var aObjArray; aContinueOnException: boolean=false);

/// wrapper to release all items stored in an array of T*ObjArray dynamic array
// - e.g. aObjArray may be defined as "array of array of TSynFilter"
procedure ObjArrayObjArrayClear(var aObjArray);

/// wrapper to release all items stored in several T*ObjArray dynamic arrays
// - as expected by TJSONSerializer.RegisterObjArrayForJSON()
procedure ObjArraysClear(const aObjArray: array of pointer);

{$ifndef DELPHI5OROLDER}

/// wrapper to add an item to a T*InterfaceArray dynamic array storage
function InterfaceArrayAdd(var aInterfaceArray; const aItem: IUnknown): integer;

/// wrapper to add once an item to a T*InterfaceArray dynamic array storage
procedure InterfaceArrayAddOnce(var aInterfaceArray; const aItem: IUnknown);

/// wrapper to search an item in a T*InterfaceArray dynamic array storage
// - search is performed by address/reference, not by content
// - return -1 if the item is not found in the dynamic array, or the index of
// the matching entry otherwise
function InterfaceArrayFind(const aInterfaceArray; const aItem: IUnknown): integer;

/// wrapper to delete an item in a T*InterfaceArray dynamic array storage
// - search is performed by address/reference, not by content
// - do nothing if the item is not found in the dynamic array
function InterfaceArrayDelete(var aInterfaceArray; const aItem: IUnknown): integer; overload;

/// wrapper to delete an item in a T*InterfaceArray dynamic array storage
// - do nothing if the item is not found in the dynamic array
procedure InterfaceArrayDelete(var aInterfaceArray; aItemIndex: integer); overload;

{$endif DELPHI5OROLDER}


/// fast search of an exact case-insensitive match of a RTTI's PShortString array
function FindShortStringListExact(List: PShortString; MaxValue: integer;
  aValue: PUTF8Char; aValueLen: integer): integer;

/// fast search of an left-trimmed lowercase match of a RTTI's PShortString array
function FindShortStringListTrimLowerCase(List: PShortString; MaxValue: integer;
  aValue: PUTF8Char; aValueLen: integer): integer;

/// retrieve the type name from its low-level RTTI
function TypeInfoToName(aTypeInfo: pointer): RawUTF8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the type name from its low-level RTTI
procedure TypeInfoToName(aTypeInfo: pointer; var result: RawUTF8;
  const default: RawUTF8=''); overload;

/// retrieve the unit name and type name from its low-level RTTI
procedure TypeInfoToQualifiedName(aTypeInfo: pointer; var result: RawUTF8;
  const default: RawUTF8='');

/// retrieve the record size from its low-level RTTI
function RecordTypeInfoSize(aRecordTypeInfo: pointer): integer;

/// compute a dynamic array element information
// - will raise an exception if the supplied RTTI is not a dynamic array
// - will return the element type name and set ElemTypeInfo otherwise
// - if there is no element type information, an approximative element type name
// will be returned (e.g. 'byte' for an array of 1 byte items), and ElemTypeInfo
// will be set to nil
// - this low-level function is used e.g. by mORMotWrappers unit
function DynArrayElementTypeName(TypeInfo: pointer; ElemTypeInfo: PPointer=nil): RawUTF8;



/// initialize the structure with a one-dimension dynamic array
// - the dynamic array must have been defined with its own type
// (e.g. TIntegerDynArray = array of Integer)
// - if aCountPointer is set, it will be used instead of length() to store
// the dynamic array items count - it will be much faster when adding
// elements to the array, because the dynamic array won't need to be
// resized each time - but in this case, you should use the Count property
// instead of length(array) or high(array) when accessing the data: in fact
// length(array) will store the memory size reserved, not the items count
// - if aCountPointer is set, its content will be set to 0, whatever the
// array length is, or the current aCountPointer^ value is
// - a typical usage could be:
// !var IntArray: TIntegerDynArray;
// !begin
// !  with DynArray(TypeInfo(TIntegerDynArray),IntArray) do
// !  begin
// !    (...)
// !  end;
// ! (...)
// ! DynArray(TypeInfo(TIntegerDynArray),IntArrayA).SaveTo
function DynArray(aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil): TDynArray;
  {$ifdef HASINLINE}inline;{$endif}

var
  /// mORMot.pas will registry here its T*ObjArray serialization process
  DynArrayIsObjArray: function(aDynArrayTypeInfo: Pointer): boolean;

{ ****************** text buffer and JSON functions and classes ************ }

type

  /// the available logging events, as handled by TSynLog
  // - defined in SynCommons so that it may be used with TTextWriter.AddEndOfLine
  // - sllInfo will log general information events
  // - sllDebug will log detailed debugging information
  // - sllTrace will log low-level step by step debugging information
  // - sllWarning will log unexpected values (not an error)
  // - sllError will log errors
  // - sllEnter will log every method start
  // - sllLeave will log every method exit
  // - sllLastError will log the GetLastError OS message
  // - sllException will log all exception raised - available since Windows XP
  // - sllExceptionOS will log all OS low-level exceptions (EDivByZero,
  // ERangeError, EAccessViolation...)
  // - sllMemory will log memory statistics
  // - sllStackTrace will log caller's stack trace (it's by default part of
  // TSynLogFamily.LevelStackTrace like sllError, sllException, sllExceptionOS,
  // sllLastError and sllFail)
  // - sllFail was defined for TSynTestsLogged.Failed method, and can be used
  // to log some customer-side assertions (may be notifications, not errors)
  // - sllSQL is dedicated to trace the SQL statements
  // - sllCache should be used to trace the internal caching mechanism
  // - sllResult could trace the SQL results, JSON encoded
  // - sllDB is dedicated to trace low-level database engine features
  // - sllHTTP could be used to trace HTTP process
  // - sllClient/sllServer could be used to trace some Client or Server process
  // - sllServiceCall/sllServiceReturn to trace some remote service or library
  // - sllUserAuth to trace user authentication (e.g. for individual requests)
  // - sllCustom* items can be used for any purpose
  // - sllNewRun will be written when a process opens a rotated log
  // - sllDDDError will log any DDD-related low-level error information
  // - sllDDDInfo will log any DDD-related low-level debugging information
  // - sllMonitoring will log the statistics information (if available),
  // or may be used for real-time chat among connected people to ToolsAdmin
  TSynLogInfo = (
    sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError,
    sllEnter, sllLeave,
    sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
    sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
    sllServiceCall, sllServiceReturn, sllUserAuth,
    sllCustom1, sllCustom2, sllCustom3, sllCustom4, sllNewRun,
    sllDDDError, sllDDDInfo, sllMonitoring);

  /// used to define a set of logging level abilities
  // - i.e. a combination of none or several logging event
  // - e.g. use LOG_VERBOSE constant to log all events, or LOG_STACKTRACE
  // to log all errors and exceptions
  TSynLogInfos = set of TSynLogInfo;

  /// a dynamic array of logging event levels
  TSynLogInfoDynArray = array of TSynLogInfo;


type


  /// event signature to locate a service for a given string key
  // - used e.g. by TRawUTF8ObjectCacheList.OnKeyResolve property
  TOnKeyResolve = function(const aInterface: TGUID; const Key: RawUTF8; out Obj): boolean of object;
  /// event signature to notify a given string key
  TOnKeyNotify = procedure(Sender: TObject; const Key: RawUTF8) of object;

type


  /// a TStream using a RawByteString as internal storage
  // - default TStringStream uses WideChars since Delphi 2009, so it is
  // not compatible with previous versions, and it does make sense to
  // work with RawByteString in our UTF-8 oriented framework
  TRawByteStringStream = class(TStream)
  protected
    fDataString: RawByteString;
    fPosition: Integer;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const aString: RawByteString=''); overload;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property DataString: RawByteString read fDataString write fDataString;
  end;

  /// a TStream pointing to some in-memory data, for instance UTF-8 text
  // - warning: there is no local copy of the supplied content: the
  // source data must be available during all the TSynMemoryStream usage
  TSynMemoryStream = class(TCustomMemoryStream)
  public
    /// create a TStream with the supplied text data
    // - warning: there is no local copy of the supplied content: the aText
    // variable must be available during all the TSynMemoryStream usage:
    // don't release aText before calling TSynMemoryStream.Free
    // - aText can be on any AnsiString format, e.g. RawUTF8 or RawByteString
    constructor Create(const aText: RawByteString); overload;
    /// create a TStream with the supplied data buffer
    // - warning: there is no local copy of the supplied content: the
    // Data/DataLen buffer must be available during all the TSynMemoryStream usage:
    // don't release the source Data before calling TSynMemoryStream.Free
    constructor Create(Data: pointer; DataLen: integer); overload;
    /// this TStream is read-only: calling this method will raise an exception
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

type
  /// store one name/value pair of raw UTF-8 content, from a JSON buffer
  // - used e.g. by JSONDecode() overloaded function to returns names/values
  TNameValuePUTF8Char = record
    Name: PUTF8Char;
    Value: PUTF8Char;
    NameLen: integer;
    ValueLen: integer;
  end;
  /// used e.g. by JSONDecode() overloaded function to returns name/value pairs
  TNameValuePUTF8CharDynArray = array of TNameValuePUTF8Char;


const
  /// standard header for an UTF-8 encoded XML file
  XMLUTF8_HEADER = '<?xml version="1.0" encoding="UTF-8"?>'#13#10;

  /// standard namespace for a generic XML File
  XMLUTF8_NAMESPACE = '<contents xmlns="http://www.w3.org/2001/XMLSchema-instance">';

  POINTERSHR = {$ifdef CPU64}3{$else}2{$endif};



/// compare to floating point values, with IEEE 754 double precision
// - use this function instead of raw = operator
// - the precision is calculated from the A and B value range
// - faster equivalent than SameValue() in Math unit
// - if you know the precision range of A and B, it's faster to check abs(A-B)<range
function SameValue(const A, B: Double; DoublePrec: double = 1E-12): Boolean;

/// compare to floating point values, with IEEE 754 double precision
// - use this function instead of raw = operator
// - the precision is calculated from the A and B value range
// - faster equivalent than SameValue() in Math unit
// - if you know the precision range of A and B, it's faster to check abs(A-B)<range
function SameValueFloat(const A, B: TSynExtended; DoublePrec: TSynExtended = 1E-12): Boolean;



{$ifndef ENHANCEDRTL}
{$ifndef LVCL} { don't define these twice }

var
  /// these procedure type must be defined if a default system.pas is used
  // - mORMoti18n.pas unit will hack default LoadResString() procedure
  // - already defined in our Extended system.pas unit
  // - needed with FPC, Delphi 2009 and up, i.e. when ENHANCEDRTL is not defined
  // - expect generic "string" type, i.e. UnicodeString for Delphi 2009+
  // - not needed with the LVCL framework (we should be on server side)
  LoadResStringTranslate: procedure(var Text: string) = nil;

  /// current LoadResString() cached entries count
  // - i.e. resourcestring caching for faster use
  // - used only if a default system.pas is used, not our Extended version
  // - defined here, but resourcestring caching itself is implemented in the
  // mORMoti18n.pas unit, if the ENHANCEDRTL conditional is not defined
  CacheResCount: integer = -1;

{$endif}
{$endif}

type
  /// a generic callback, which can be used to translate some text on the fly
  // - maps procedure TLanguageFile.Translate(var English: string) signature
  // as defined in mORMoti18n.pas
  // - can be used e.g. for TSynMustache's {{"English text}} callback
  TOnStringTranslate = procedure (var English: string) of object;


{ ************ fast low-level lookup types used by internal conversion routines }

{$ifndef ENHANCEDRTL}
{$ifndef LVCL} { don't define these const twice }

const
  /// fast lookup table for converting any decimal number from
  // 0 to 99 into their ASCII equivalence
  // - our enhanced SysUtils.pas (normal and LVCL) contains the same array
  TwoDigitLookup: packed array[0..99] of array[1..2] of AnsiChar =
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

{$endif}
{$endif}

var
  /// fast lookup table for converting any decimal number from
  // 0 to 99 into their ASCII equivalence
  TwoDigitLookupW: packed array[0..99] of word absolute TwoDigitLookup;

const
  /// used internaly for fast word recognition (32 bytes const)
  IsWord: set of byte =
    [ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z')];

  /// used internaly for fast identifier recognition (32 bytes const)
  // - can be used e.g. for field or table name
  // - this char set matches the classical pascal definition of identifiers
  // - see also PropNameValid()
  IsIdentifier: set of byte =
    [ord('_'),ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z')];

  /// used internaly for fast URI "unreserved" characters identifier
  // - defined as unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
  // in @http://tools.ietf.org/html/rfc3986#section-2.3
  IsURIUnreserved: set of byte =
    [ord('a')..ord('z'),ord('A')..ord('Z'),ord('0')..ord('9'),
     ord('-'),ord('.'),ord('_'),ord('~')];

  /// used internaly for fast extended JSON property name recognition (32 bytes const)
  // - can be used e.g. for extended JSON object field
  // - follow JsonPropNameValid, GetJSONPropName and GotoNextJSONObjectOrArray
  IsJsonIdentifierFirstChar: set of byte =
    [ord('_'),ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z'),ord('$')];

  /// used internaly for fast extended JSON property name recognition (32 bytes const)
  // - can be used e.g. for extended JSON object field
  // - follow JsonPropNameValid, GetJSONPropName and GotoNextJSONObjectOrArray
  IsJsonIdentifier: set of byte =
    [ord('_'),ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z'),
     ord('.'),ord('['),ord(']')];

{$ifdef DELPHI6OROLDER}

// define some common constants not available prior to Delphi 7
const
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;
  UnixDateDelta = 25569;

/// GetFileVersion returns the most significant 32 bits of a file's binary
// version number
// - typically, this includes the major and minor version placed
// together in one 32-bit integer
// - generally does not include the release or build numbers
// - returns Cardinal(-1) in case of failure
function GetFileVersion(const FileName: TFileName): cardinal;

{$endif}

{$ifdef MSWINDOWS}

type
  /// the recognized Windows versions
  TWindowsVersion = (
    wUnknown, w2000, wXP, wXP_64, wServer2003, wServer2003_R2,
    wVista, wVista_64, wServer2008, wServer2008_64,
    wSeven, wSeven_64, wServer2008_R2, wServer2008_R2_64,
    wEight, wEight_64, wServer2012, wServer2012_64,
    wEightOne, wEightOne_64, wServer2012R2, wServer2012R2_64,
    wTen, wTen_64, wServer2016, wServer2016_64);
  {$ifndef UNICODE}
  /// not defined in older Delphi versions
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of char;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
  {$endif}

const
  /// the recognized Windows versions, as plain text
  WINDOWS_NAME: array[TWindowsVersion] of RawUTF8 = (
    '', '2000', 'XP', 'XP 64bit', 'Server 2003', 'Server 2003 R2',
    'Vista', 'Vista 64bit', 'Server 2008', 'Server 2008 64bit',
    '7', '7 64bit', 'Server 2008 R2', 'Server 2008 R2 64bit',
    '8', '8 64bit', 'Server 2012', 'Server 2012 64bit',
    '8.1', '8.1 64bit', 'Server 2012 R2', 'Server 2012 R2 64bit',
    '10', '10 64bit', 'Server 2016', 'Server 2016 64bit');

var
  /// is set to TRUE if the current process is a 32 bit image running under WOW64
  // - WOW64 is the x86 emulator that allows 32-bit Windows-based applications
  // to run seamlessly on 64-bit Windows
  // - equals always FALSE if the current executable is a 64 bit image
  IsWow64: boolean;
  /// the current System information, as retrieved for the current process
  // - under a WOW64 process, it will use the GetNativeSystemInfo() new API
  // to retrieve the real top-most system information
  // - note that the lpMinimumApplicationAddress field is replaced by a
  // more optimistic/realistic value ($100000 instead of default $10000)
  SystemInfo: TSystemInfo;
  /// the current Operating System information, as retrieved for the current process
  OSVersionInfo: TOSVersionInfoEx;
  /// the current Operating System version, as retrieved for the current process
  OSVersion: TWindowsVersion;
  /// the current Operating System version, as retrieved for the current process
  // - contains e.g. 'Windows Seven 64 SP1 (6.1.7601)'
  OSVersionText: RawUTF8;

{$endif MSWINDOWS}


{ ************ TSynTable generic types and classes ************************** }

{$define SORTCOMPAREMETHOD}
{ if defined, the field content comparison will use a method instead of fixed
  functions - could be mandatory for tftArray field kind }



const
  /// can be used e.g. in logs
  BOOL_STR: array[boolean] of string[7] = ('false','true');

  /// used by TSynTableStatement.WhereField for "SELECT .. FROM TableName WHERE ID=?"
  SYNTABLESTATEMENTWHEREID = 0;

  /// can be used to append to most English nouns to form a plural
  PLURAL_FORM: array[boolean] of RawUTF8 = ('','s');


{ ************ variant-based process, including JSON/BSON document content }

const
  /// this variant type is not defined in older versions of Delphi
  varWord64 = 21;

  /// this variant type will map the current SynUnicode type
  // - depending on the compiler version
  varSynUnicode = {$ifdef HASVARUSTRING}varUString{$else}varOleStr{$endif};

  /// this variant type will map the current string type
  // - depending on the compiler version
  varNativeString = {$ifdef UNICODE}varUString{$else}varString{$endif};

  /// those TVarData.VType values are un-managed and do not need to be cleared
  // - used mainly in low-level code similar to the folllowing:
  // !  if TVarData(aVariant).VType and VTYPE_STATIC<>0 then
  // !    VarClear(aVariant);
  // - equals private constant varDeepData in Variants.pas 
  VTYPE_STATIC = $BFE8;

/// same as Dest := TVarData(Source) for simple values
// - will return TRUE for all simple values after varByRef unreference, and
// copying the unreferenced Source value into Dest raw storage
// - will return FALSE for not varByRef values, or complex values (e.g. string)
function SetVariantUnRefSimpleValue(const Source: variant; var Dest: TVarData): boolean;
  {$ifdef HASINLINE}inline;{$endif}

{$ifndef LVCL}

/// convert a raw binary buffer into a variant RawByteString varString
// - you can then use VariantToRawByteString() to retrieve the binary content
procedure RawByteStringToVariant(Data: PByte; DataLen: Integer; var Value: variant); overload;

/// convert a RawByteString content into a variant varString
// - you can then use VariantToRawByteString() to retrieve the binary content
procedure RawByteStringToVariant(const Data: RawByteString; var Value: variant); overload;

/// convert back a RawByteString from a variant
// - the supplied variant should have been created via a RawByteStringToVariant()
// function call
procedure VariantToRawByteString(const Value: variant; var Dest: RawByteString);

/// same as Value := Null, but slightly faster
procedure SetVariantNull(var Value: variant);
  {$ifdef HASINLINE}inline;{$endif}

const
  NullVarData: TVarData = (VType: varNull);
var
  /// a slightly faster alternative to Variants.Null function
  Null: variant absolute NullVarData;

{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - we also discovered some issues with FPC's Variants unit, so this function
// may be used even in end-user cross-compiler code
function VarIsEmptyOrNull(const V: Variant): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(PVariant(V)^) or VarIsEmpty(PVariant(V)^), but faster
// - we also discovered some issues with FPC's Variants unit, so this function
// may be used even in end-user cross-compiler code
function VarDataIsEmptyOrNull(VarData: pointer): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// fastcheck if a variant hold a value
// - varEmpty, varNull or a '' string would be considered as void
// - varBoolean=false or varDate=0 would be considered as void
// - a TDocVariantData with Count=0 would be considered as void 
// - any other value (e.g. integer) would be considered as not void  
function VarIsVoid(const V: Variant): boolean;

type
  TVarDataTypes = set of 0..255;

/// allow to check for a specific set of TVarData.VType
function VarIs(const V: Variant; const VTypes: TVarDataTypes): Boolean;
  {$ifdef HASINLINE}inline;{$endif}



{ ******************* cross-cutting classes and functions ***************** }

/// return the Delphi Compiler Version
// - returns 'Delphi 2007' or 'Delphi 2010' e.g.
function GetDelphiCompilerVersion: RawUTF8;


resourcestring

  sInvalidPattern = '"%s" does not match the expected pattern';
  sCharacter01n = 'character,character,characters';
  sInvalidTextLengthMin = 'Expect at least %d %s';
  sInvalidTextLengthMax = 'Expect up to %d %s';
  sInvalidTextChar = 'Expect at least %d %s %s,Expect up to %d %s %s,'+
    'alphabetical,digital,punctuation,lowercase,uppercase,space,'+
    'Too much spaces on the left,Too much spaces on the right';
  sValidationFailed = '"%s" rule failed';
  sValidationFieldVoid = 'An unique key field must not be void';
  sValidationFieldDuplicate = 'Value already used for this unique key field';


implementation


{$ifdef FPC}

type
  /// available type families for Free Pascal RTTI values
  // - values differs from Delphi, and are taken from FPC typinfo.pp unit
  // - here below, we defined tkLString instead of FPC tkAString to match
  // Delphi - see http://lists.freepascal.org/fpc-devel/2013-June/032233.html
  TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,tkFloat,
    tkSet,tkMethod,tkSString,tkLStringOld,tkLString,
    tkWString,tkVariant,tkArray,tkRecord,tkInterface,
    tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord,
    tkDynArray,tkInterfaceRaw,tkProcVar,tkUString,tkUChar,
    tkHelper,tkFile,tkClassRef,tkPointer);

const
   // all potentially managed types
   tkManagedTypes = [tkLStringOld,tkLString,tkWstring,tkUstring,tkArray,
                     tkObject,tkRecord,tkDynArray,tkInterface,tkVariant];
   // maps record or object types
   tkRecordTypes = [tkObject,tkRecord];
   tkRecordTypeOrSet = [tkObject,tkRecord];

type
  TDelphiTypeKind = (dkUnknown, dkInteger, dkChar, dkEnumeration, dkFloat,
    dkString, dkSet, dkClass, dkMethod, dkWChar, dkLString, dkWString,
    dkVariant, dkArray, dkRecord, dkInterface, dkInt64, dkDynArray,
    dkUString, dkClassRef, dkPointer, dkProcedure);

const
  FPCTODELPHI: array[TTypeKind] of TDelphiTypeKind = (
    dkUnknown,dkInteger,dkChar,dkEnumeration,dkFloat,
    dkSet,dkMethod,dkString,dkLString,dkLString,
    dkWString,dkVariant,dkArray,dkRecord,dkInterface,
    dkClass,dkRecord,dkWChar,dkEnumeration,dkInt64,dkInt64,
    dkDynArray,dkInterface,dkProcedure,dkUString,dkWChar,
    dkPointer,dkPointer,dkClassRef,dkPointer);

  DELPHITOFPC: array[TDelphiTypeKind] of TTypeKind = (
    tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkSString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray,
    tkUString, tkClassRef, tkPointer, tkProcVar);

{$else}

type
  /// available type families for Delphi 6 and up, similar to typinfo.pas
  TTypeKind = (tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
    {$ifdef UNICODE}, tkUString, tkClassRef, tkPointer, tkProcedure{$endif});

const
  // maps record or object types
  tkRecordTypes = [tkRecord];
  tkRecordTypeOrSet = tkRecord;

{$endif}

type
  TOrdType = (otSByte,otUByte,otSWord,otUWord,otSLong,otULong);
  TFloatType = (ftSingle,ftDoub,ftExtended,ftComp,ftCurr);
  TTypeKinds = set of TTypeKind;
  PTypeKind = ^TTypeKind;

  PStrRec = ^TStrRec;
  /// map the Delphi/FPC string header, as defined in System.pas
  TStrRec =
    {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed
    {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    record
{$ifdef FPC}
  {$ifdef ISFPC27}
    codePage: Word;
    elemSize: Word;
  {$endif}
  {$ifdef CPU64}
    _Padding: LongInt;
  {$endif}
    refCnt: SizeInt;
    length: SizeInt;
{$else FPC}
  {$ifdef UNICODE}
    {$ifdef CPU64}
    /// padding bytes for 16 byte alignment of the header
    _Padding: LongInt;
    {$endif}
    /// the associated code page used for this string
    // - exist only since Delphi/FPC 2009
    // - 0 or 65535 for RawByteString
    // - 1200=CP_UTF16 for UnicodeString
    // - 65001=CP_UTF8 for RawUTF8
    // - the current code page for AnsiString
    codePage: Word;
    /// either 1 (for AnsiString) or 2 (for UnicodeString)
    // - exist only since Delphi/FPC 2009
    elemSize: Word;
  {$endif UNICODE}
    /// COW string reference count (basic garbage memory mechanism)
    refCnt: Longint;
    /// length in characters
    // - size in bytes = length*elemSize
    length: Longint;
{$endif FPC}
  end;

  /// map the Delphi/FPC dynamic array header (stored before each instance)
  TDynArrayRec = packed record
    /// dynamic array reference count (basic garbage memory mechanism)
    {$ifdef FPC}
    refCnt: PtrInt;
    high: tdynarrayindex;
    function GetLength: sizeint; inline;
    procedure SetLength(len: sizeint); inline;
    property length: sizeint read GetLength write SetLength;
    {$else}
    {$ifdef CPUX64}
    _Padding: LongInt; // Delphi/FPC XE2+ expects 16 byte alignment
    {$endif}
    refCnt: Longint;
    /// length in element count
    // - size in bytes = length*ElemSize
    length: PtrInt;
    {$endif}
  end;
  PDynArrayRec = ^TDynArrayRec;

  {$ifdef FPC}
    {$PACKRECORDS C}
  {$endif FPC}

  PTypeInfo = ^TTypeInfo;
  {$ifdef HASDIRECTTYPEINFO}
  PTypeInfoStored = PTypeInfo;
  {$else}
  PTypeInfoStored = ^PTypeInfo;
  {$endif}

  /// map the Delphi/FPC record field RTTI
  TFieldInfo =
    {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed
    {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    record
    TypeInfo: PTypeInfoStored;
    {$ifdef FPC}
    Offset: sizeint;
    {$else}
    Offset: PtrUInt;
    {$endif FPC}
  end;
  {$ifdef ISDELPHI2010}
  /// map the Delphi record field enhanced RTTI (available since Delphi 2010)
  TEnhancedFieldInfo = packed record
    TypeInfo: PTypeInfoStored;
    Offset: PtrUInt;
    Flags: Byte;
    NameLen: byte; // = Name[0] = length(Name)
  end;
  PEnhancedFieldInfo = ^TEnhancedFieldInfo;
  {$endif}

  /// map the Delphi/FPC RTTI content
  {$ifdef FPC_HAS_MANAGEMENT_OPERATORS}
  PPRecordInitTable = ^PRecordInitTable;
  PRecordInitTable = ^TRecordInitTable;
  TRecordInitTable =
    {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed
    {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    record
      recSize: longint;
      Terminator: Pointer;
      recManagementOperators: Pointer;
      ManagedCount: longint;
    end;
  {$endif FPC_HAS_MANAGEMENT_OPERATORS}

  TTypeInfo =
    {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed
    {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    record
    kind: TTypeKind;
    NameLen: byte;
    case TTypeKind of
    tkUnknown: (
      NameFirst: AnsiChar;
    );
    tkDynArray: (
      {$ifdef FPC}
      elSize: SizeUInt;
      elType2: PTypeInfoStored;
      varType: LongInt;
      elType: PTypeInfoStored;
      //DynUnitName: ShortStringBase;
      {$else}
      // storage byte count for this field
      elSize: Longint;
      // nil for unmanaged field
      elType: PTypeInfoStored;
      // OleAuto compatible type
      varType: Integer;
      // also unmanaged field
      elType2: PTypeInfoStored;
      {$endif}
    );
    tkArray: (
      {$ifdef FPC}
      // and $7FFFFFFF needed
      arraySize: SizeInt;
      // product of lengths of all dimensions
      elCount: SizeInt;
      {$else}
      arraySize: Integer;
      // product of lengths of all dimensions
      elCount: Integer;
      {$endif}
      arrayType: PTypeInfoStored;
      dimCount: Byte;
      dims: array[0..255 {DimCount-1}] of PTypeInfoStored;
    );
    {$ifdef FPC}
    tkRecord, tkObject:(
      recSize: longint;
      {$ifdef FPC_HAS_MANAGEMENT_OPERATORS}
      recInitTable: PPRecordInitTable;
      {$endif FPC_HAS_MANAGEMENT_OPERATORS}
      ManagedCount: longint;
    {$else}
    tkRecord: (
      recSize: cardinal;
      ManagedCount: integer;
    {$endif FPC}
      ManagedFields: array[0..0] of TFieldInfo;
      {$ifdef ISDELPHI2010} // enhanced RTTI containing info about all fields
      NumOps: Byte;
      //RecOps: array[0..0] of Pointer;
      AllCount: Integer; // !!!! may need $RTTI EXPLICIT FIELDS([vcPublic])
      AllFields: array[0..0] of TEnhancedFieldInfo;
      {$endif ISDELPHI2010}
    );
    tkEnumeration: (
      EnumType: TOrdType;
      {$ifdef FPC_ENUMHASINNER}
      inner:
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif}
      record
      {$endif}
      MinValue: longint;
      MaxValue: longint;
      EnumBaseType: PTypeInfoStored;
      {$ifdef FPC_ENUMHASINNER}
      end;
      {$endif}
      NameList: string[255];
    );
    tkInteger: (
      IntegerType: TOrdType;
    );
    tkSet: (
      SetType: TOrdType;
      SetBaseType: PTypeInfoStored;
    );
    tkFloat: (
      FloatType: TFloatType;
    );
    tkClass: (
      ClassType: PAnsiChar; // TClass;
      ParentInfo: PTypeInfoStored;
      PropCount: SmallInt;
      UnitNameLen: byte;
    );
  end;
  TPropInfo = packed record
    PropType: PTypeInfoStored;
    GetProc: PtrInt;
    SetProc: PtrInt;
    StoredProc: PtrInt;
    Index: Integer;
    Default: Longint;
    NameIndex: SmallInt;
    {$ifdef FPC}
    PropProcs : Byte;
    {$endif}
    NameLen: byte;
  end;
  PPropInfo = ^TPropInfo;

const
  /// codePage offset = string header size
  // - used to calc the beginning of memory allocation of a string
  STRRECSIZE = SizeOf(TStrRec);


{$ifdef HASDIRECTTYPEINFO}
type
  Deref = PTypeInfo;
{$else}
function Deref(Info: PTypeInfoStored): PTypeInfo;
{$ifdef HASINLINE} inline;
begin
  if Info=nil then
    result := pointer(Info) else
    result := Info^;
end;
{$else}
asm // Delphi is so bad at compiling above code...
        or      eax, eax
        jz      @z
        mov     eax, [eax]
        ret
@z:     db      $f3 // rep ret
end;
{$endif HASINLINE}
{$endif HASDIRECTTYPEINFO}


var
  KnownTypeInfo: array of PTypeInfo;

function DynArrayLength(Value: Pointer): integer;
  {$ifdef HASINLINE}inline;{$endif}
begin
  if Value=nil then
    result := PtrInt(Value) else begin
    {$ifdef FPC}
    result := PDynArrayRec(PtrUInt(Value)-SizeOf(TDynArrayRec))^.length;
    {$else}
    result := PInteger(PtrUInt(Value)-sizeof(PtrInt))^;
    {$endif}
  end;
end;

function GetTypeInfo(aTypeInfo: pointer; aExpectedKind: TTypeKind): PTypeInfo; overload;
{$ifdef HASINLINE} inline;
begin
  if (aTypeInfo<>nil) and (PTypeKind(aTypeInfo)^=aExpectedKind) then begin
    {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    result := GetFPCAlignPtr(aTypeInfo);
    {$else}
    result := aTypeInfo;
    inc(PtrUInt(result),result^.NameLen);
    {$endif}
  end else
    result := nil;
end;
{$else}
asm
        test    eax, eax
        jz      @n
        cmp     dl, [eax]
        movzx   ecx, byte ptr[eax + TTypeInfo.NameLen]
        jne     @n
        add     eax, ecx
        ret
@n:     xor     eax, eax
end;
{$endif}

function GetTypeInfo(aTypeInfo: pointer; const aExpectedKind: TTypeKinds): PTypeInfo; overload;
{$ifdef HASINLINE} inline;
begin
  result := aTypeInfo;
  if (result<>nil) and (result^.Kind in aExpectedKind) then
    {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    result := GetFPCAlignPtr(result)
    {$else}
    inc(PtrUInt(result),result^.NameLen)
    {$endif}
  else
    result := nil;
end;
{$else}
asm // eax=aTypeInfo edx=aExpectedKind
        test    eax, eax
        jz      @n
        movzx   ecx, byte ptr[eax]
        bt      edx, ecx
        movzx   ecx, byte ptr[eax + TTypeInfo.NameLen]
        jnb     @n
        add     eax, ecx
        ret
@n:     xor     eax, eax
end;
{$endif}


procedure SetRawUTF8(var Dest: RawUTF8; text: pointer; len: integer);
var P: PStrRec;
begin
  if (len>128) or (len=0) or (PtrInt(Dest)=0) or     // Dest=''
    (PStrRec(PtrInt(Dest)-STRRECSIZE)^.refCnt<>1) then
    SetString(Dest,PAnsiChar(text),len) else begin
    if PStrRec(Pointer(PtrInt(Dest)-STRRECSIZE))^.length<>len then begin
      P := Pointer(PtrInt(Dest)-STRRECSIZE);
      ReallocMem(P,len+(STRRECSIZE+1));
      P^.length := len;
      pointer(Dest) := pointer(PAnsiChar(P)+STRRECSIZE);
      PByteArray(Dest)[len] := 0;
    end;
    System.Move(pointer(text)^,pointer(Dest)^,len);
  end;
end;

procedure TypeInfoToName(aTypeInfo: pointer; var result: RawUTF8;
  const default: RawUTF8='');
begin
  if aTypeInfo<>nil then
    SetRawUTF8(result,PAnsiChar(@PTypeInfo(aTypeInfo)^.NameLen)+1,
      PTypeInfo(aTypeInfo)^.NameLen) else
    result := default;
end;

procedure TypeInfoToQualifiedName(aTypeInfo: pointer; var result: RawUTF8;
  const default: RawUTF8='');
var unitname: RawUTF8;
begin
  if aTypeInfo<>nil then begin
    SetRawUTF8(result,PAnsiChar(@PTypeInfo(aTypeInfo)^.NameLen)+1,
      PTypeInfo(aTypeInfo)^.NameLen);
    if PTypeInfo(aTypeInfo)^.Kind=tkClass then begin
      with GetTypeInfo(aTypeInfo,PTypeKind(aTypeInfo)^)^ do
        SetRawUTF8(unitname,PAnsiChar(@UnitNameLen)+1,UnitNameLen);
      result := unitname+'.'+result;
    end;
  end else result := default;
end;

function TypeInfoToName(aTypeInfo: pointer): RawUTF8;
begin
  TypeInfoToName(aTypeInfo,Result,'');
end;

function RecordTypeInfoSize(aRecordTypeInfo: Pointer): integer;
var info: PTypeInfo;
begin
  info := GetTypeInfo(aRecordTypeInfo,tkRecordTypeOrSet);
  if info=nil then
    result := 0 else
    result := info^.recSize;
end;


function FindShortStringListExact(List: PShortString; MaxValue: integer;
  aValue: PUTF8Char; aValueLen: integer): integer;
var PLen: integer;
begin
  for result := 0 to MaxValue do begin
    PLen := ord(List^[0]);
    if (PLen=aValuelen) and IdemPropNameUSameLen(@List^[1],aValue,aValueLen) then
      exit else
      inc(PByte(List),PLen+1); // next short string
  end;
  result := -1;
end;

function FindShortStringListTrimLowerCase(List: PShortString; MaxValue: integer;
  aValue: PUTF8Char; aValueLen: integer): integer;
var PLen: integer;
begin
  for result := 0 to MaxValue do begin
    PLen := ord(List^[0]);
    inc(PUTF8Char(List));
    repeat
      if not(PUTF8Char(List)^ in ['a'..'z']) then
        break;
      inc(PUTF8Char(List));
      dec(PLen);
    until PLen=0;
    if (PLen=aValueLen) and IdemPropNameUSameLen(aValue,PUTF8Char(List),PLen) then
      exit else
      inc(PUTF8Char(List),PLen);
  end;
  result := -1;
end;

{ note: those low-level VariantTo*() functions are expected to be there
        even if NOVARIANTS conditional is defined (used e.g. by SynDB.TQuery) }

function VariantToInteger(const V: Variant; var Value: integer): boolean;
var tmp: TVarData;
begin
  with TVarData(V) do
  case VType of
  varNull,
  varEmpty:    Value := 0;
  varBoolean:  Value := ord(VBoolean);
  varSmallint: Value := VSmallInt;
  {$ifndef DELPHI5OROLDER}
  varShortInt: Value := VShortInt;
  varWord:     Value := VWord;
  varLongWord:
    if (VLongWord>=cardinal(Low(integer))) and (VLongWord<=cardinal(High(integer))) then
      Value := VLongWord else begin
      result := false;
      exit;
    end;
  {$endif}
  varByte:     Value := VByte;
  varInteger:  Value := VInteger;
  varWord64:
    if (VInt64>=0) and (VInt64<=High(integer)) then
      Value := VInt64 else begin
      result := False;
      exit;
    end;
  varInt64:
    if (VInt64>=Low(integer)) and (VInt64<=High(integer)) then
      Value := VInt64 else begin
      result := False;
      exit;
    end;
  else
    if SetVariantUnRefSimpleValue(V,tmp) then begin
      result := VariantToInteger(variant(tmp),Value);
      exit;
    end else begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

function VariantToDouble(const V: Variant; var Value: double): boolean;
var tmp: TVarData;
begin
  with TVarData(V) do
  if VType=varVariant or varByRef then
    result := VariantToDouble(PVariant(VPointer)^,Value) else
  if VariantToInt64(V,tmp.VInt64) then begin // also handle varEmpty,varNull
    Value := tmp.VInt64;
    result := true;
  end else
  case VType of
  varDouble,varDate: begin
    Value := VDouble;
    result := true;
  end;
  varSingle: begin
    Value := VSingle;
    result := true;
  end;
  varCurrency: begin
    Value := VCurrency;
    result := true;
  end else
    if SetVariantUnRefSimpleValue(V,tmp) then
      result := VariantToDouble(variant(tmp),Value) else
      result := false;
  end;
end;

function VariantToCurrency(const V: Variant; var Value: currency): boolean;
var tmp: TVarData;
begin
  with TVarData(V) do
  if VType=varVariant or varByRef then
    result := VariantToCurrency(PVariant(VPointer)^,Value) else
  if VariantToInt64(V,tmp.VInt64) then begin
    Value := tmp.VInt64;
    result := true;
  end else
  case VType of
  varDouble,varDate: begin
    Value := VDouble;
    result := true;
  end;
  varSingle: begin
    Value := VSingle;
    result := true;
  end;
  varCurrency: begin
    Value := VCurrency;
    result := true;
  end else
    if SetVariantUnRefSimpleValue(V,tmp) then
      result := VariantToCurrency(variant(tmp),Value) else
      result := false;
  end;
end;

function VariantToBoolean(const V: Variant; var Value: Boolean): boolean;
var tmp: TVarData;
begin
  case TVarData(V).VType of
  varBoolean:
    Value := TVarData(V).VBoolean;
  varInteger: // coming e.g. from GetJsonField() 
    Value := TVarData(V).VInteger=1;
  else
    if SetVariantUnRefSimpleValue(V,tmp) then
      if tmp.VType=varBoolean then
        Value := tmp.VBoolean else begin
        result := false;
        exit;
      end else begin
        result := false;
        exit;
      end;
  end;
  result := true;
end;

function VariantToInt64(const V: Variant; var Value: Int64): boolean;
var tmp: TVarData;
begin
  with TVarData(V) do
  case VType of
  varNull,
  varEmpty:    Value := 0;
  varBoolean:  Value := ord(VBoolean);
  varSmallint: Value := VSmallInt;
  {$ifndef DELPHI5OROLDER}
  varShortInt: Value := VShortInt;
  varWord:     Value := VWord;
  varLongWord: Value := VLongWord;
  {$endif}
  varByte:     Value := VByte;
  varInteger:  Value := VInteger;
  varWord64,
  varInt64:    Value := VInt64;
  else
    if SetVariantUnRefSimpleValue(V,tmp) then begin
      result := VariantToInt64(variant(tmp),Value);
      exit;
    end else begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

function VariantToInt64Def(const V: Variant; DefaultValue: Int64): Int64;
begin
  if not VariantToInt64(V,result) then
    result := DefaultValue;
end;

function VariantToIntegerDef(const V: Variant; DefaultValue: integer): integer;
begin
  if not VariantToInteger(V,result) then
    result := DefaultValue;
end;

function DoubleToString(Value: Double): string;
var tmp: ShortString;
begin
  if Value=0 then
    result := '0' else
    SetString(result,PAnsiChar(@tmp[1]),ExtendedToString(tmp,Value,DOUBLE_PRECISION));
end;


{$ifdef PUREPASCAL}
function bswap32(a: cardinal): cardinal; {$ifdef HASINLINE}inline;{$endif}
begin
  result := ((a and $ff)shl 24)or((a and $ff00)shl 8)or
            ((a and $ff0000)shr 8)or((a and $ff000000)shr 24);
end;
{$else}
{$ifdef CPUX64}
function bswap32(a: cardinal): cardinal;
{$ifdef FPC}nostackframe; assembler;
asm
{$else}
asm
  .NOFRAME // ecx=a (Linux: edi)
{$endif FPC}
  {$ifdef win64}
  mov eax,ecx
  {$else}
  mov eax,edi
  {$endif win64} 
  bswap eax
end;
{$endif CPUX64}
{$ifdef CPUX86}
function bswap32(a: cardinal): cardinal;
asm
  bswap eax
end;
{$endif CPUX86}
{$endif PUREPASCAL}

{$ifndef PUREPASCAL} { these functions are implemented in asm }
{$ifndef LVCL}       { don't define these functions twice }

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
asm     // eax=P1 edx=P2 ecx=Length
        cmp     eax, edx
        je      @0                 // P1=P2
        sub     ecx, 8
        jl      @small
        push    ebx
        mov     ebx, [eax]         // Compare First 4 Bytes
        cmp     ebx, [edx]
        jne     @setbig
        lea     ebx, [eax + ecx]   // Compare Last 8 Bytes
        add     edx, ecx
        mov     eax, [ebx]
        cmp     eax, [edx]
        jne     @setbig
        mov     eax, [ebx + 4]
        cmp     eax, [edx + 4]
        jne     @setbig
        sub     ecx, 4
        jle     @true              // All Bytes already Compared
        neg     ecx                // ecx=-(Length-12)
        add     ecx, ebx           // DWORD Align Reads
        and     ecx, -4
        sub     ecx, ebx
@loop:  mov     eax, [ebx + ecx]   // Compare 8 Bytes per Loop
        cmp     eax, [edx + ecx]
        jne     @setbig
        mov     eax, [ebx + ecx + 4]
        cmp     eax, [edx + ecx + 4]
        jne     @setbig
        add     ecx, 8
        jl      @loop
@true:  pop     ebx
@0:     mov     al, 1
        ret
@setbig:pop     ebx
        setz    al
        ret
@small: add     ecx, 8             // ecx=0..7
        jle     @0                 // Length <= 0
        neg     ecx                // ecx=-1..-7
        lea     ecx, [@1 + ecx * 8 + 8]   // each @#: block below = 8 bytes
        jmp     ecx
@7:     mov     cl, [eax + 6]
        cmp     cl, [edx + 6]
        jne     @setsml
@6:     mov     ch, [eax + 5]
        cmp     ch, [edx + 5]
        jne     @setsml
@5:     mov     cl, [eax + 4]
        cmp     cl, [edx + 4]
        jne     @setsml
@4:     mov     ch, [eax + 3]
        cmp     ch, [edx + 3]
        jne     @setsml
@3:     mov     cl, [eax + 2]
        cmp     cl, [edx + 2]
        jne     @setsml
@2:     mov     ch, [eax + 1]
        cmp     ch, [edx + 1]
        jne     @setsml
@1:     mov     al, [eax]
        cmp     al, [edx]
@setsml:setz    al
end;

{$ifndef ISDELPHI2007ANDUP}
{$endif ISDELPHI2007ANDUP}

{$endif LVCL}
{$endif PUREPASCAL}

{$ifdef PUREPASCAL} // from Aleksandr Sharahov's PosEx_Sha_Pas_2()
function PosEx(const SubStr, S: RawUTF8; Offset: PtrUInt = 1): Integer;
var len, lenSub: PtrInt;
    ch: AnsiChar;
    p, pSub, pStart, pStop: PUTF8Char;
label Loop0, Loop4, TestT, Test0, Test1, Test2, Test3, Test4,
      AfterTestT, AfterTest0, Ret, Exit;
begin;
  pSub := pointer(SubStr);
  p := pointer(S);
  if (p=nil) or (pSub=nil) or (Offset<1) then begin
    Result := 0;
    goto Exit;
  end;
  {$ifdef FPC}
  len := PStrRec(Pointer(PtrInt(p)-STRRECSIZE))^.length;
  lenSub := PStrRec(Pointer(PtrInt(pSub)-STRRECSIZE))^.length-1;
  {$else}
  len := PInteger(p-4)^;
  lenSub := PInteger(pSub-4)^-1;
  {$endif}
  if (len<lenSub+PtrInt(Offset)) or (lenSub<0) then begin
    Result := 0;
    goto Exit;
  end;
  pStop := p+len;
  p := p+lenSub;
  pSub := pSub+lenSub;
  pStart := p;
  p := p+Offset+3;
  ch := pSub[0];
  lenSub := -lenSub;
  if p<pStop then goto Loop4;
  p := p-4;
  goto Loop0;
Loop4:
  if ch=p[-4] then goto Test4;
  if ch=p[-3] then goto Test3;
  if ch=p[-2] then goto Test2;
  if ch=p[-1] then goto Test1;
Loop0:
  if ch=p[0] then goto Test0;
AfterTest0:
  if ch=p[1] then goto TestT;
AfterTestT:
  p := p+6;
  if p<pStop then goto Loop4;
  p := p-4;
  if p<pStop then goto Loop0;
  Result := 0;
  goto Exit;
Test3: p := p-2;
Test1: p := p-2;
TestT: len := lenSub;
  if lenSub<>0 then
  repeat
    if (psub[len]<>p[len+1]) or (psub[len+1]<>p[len+2]) then
      goto AfterTestT;
    len := len+2;
  until len>=0;
  p := p+2;
  if p<=pStop then goto Ret;
  Result := 0;
  goto Exit;
Test4: p := p-2;
Test2: p := p-2;
Test0: len := lenSub;
  if lenSub<>0 then
  repeat
    if (psub[len]<>p[len]) or (psub[len+1]<>p[len+1]) then
      goto AfterTest0;
    len := len+2;
  until len>=0;
  inc(p);
Ret:
  Result := p-pStart;
Exit:
end;
{$else}
function PosEx(const SubStr, S: RawUTF8; Offset: PtrUInt = 1): Integer;
asm     // eax=SubStr, edx=S, ecx=Offset
        push    ebx
        push    esi
        push    edx
        test    eax, eax
        jz      @notfnd            // exit if SubStr=''
        test    edx, edx
        jz      @notfnd            // exit if S=''
        mov     esi, ecx
        mov     ecx, [edx - 4]     // length(S)
        mov     ebx, [eax - 4]     // length(SubStr)
        add     ecx, edx
        sub     ecx, ebx              // ecx = max start pos for full match
        lea     edx, [edx + esi - 1]  // edx = start position
        cmp     edx, ecx
        jg      @notfnd            // startpos > max start pos
        cmp     ebx, 1
        jle     @onec              // optimized loop for length(SubStr)<=1
        push    edi
        push    ebp
        lea     edi, [ebx - 2]     // edi = length(SubStr)-2
        mov     esi, eax           // esi = SubStr
        movzx   ebx, byte ptr[eax] // bl = search character
@l:     cmp     bl, [edx]          // compare 2 characters per @l
        je      @c1fnd
@notc1: cmp     bl, [edx + 1]
        je      @c2fnd
@notc2: lea     edx, [edx + 2]
        cmp     edx, ecx            // next start position <= max start position
        jle     @l
        pop     ebp
        pop     edi
@notfnd:xor     eax, eax            // returns 0 if not fnd
        pop     edx
        pop     esi
        pop     ebx
        ret
@c1fnd: mov     ebp, edi            // ebp = length(SubStr)-2
@c1l:   movzx   eax, word ptr[esi + ebp]
        cmp     ax, [edx + ebp]     // compare 2 chars per @c1l (may include #0)
        jne     @notc1
        sub     ebp, 2
        jnc     @c1l
        pop     ebp
        pop     edi
        jmp     @setres
@c2fnd: mov     ebp, edi            // ebp = length(SubStr)-2
@c2l:   movzx   eax, word ptr[esi + ebp]
        cmp     ax, [edx + ebp + 1] // compare 2 chars per @c2l (may include #0)
        jne     @notc2
        sub     ebp, 2
        jnc     @c2l
        pop     ebp
        pop     edi
        jmp     @chkres
@onec:  jl      @notfnd             // needed for zero-length non-nil strings
        movzx   eax, byte ptr[eax]  // search character
@charl: cmp     al, [edx]
        je      @setres
        cmp     al, [edx + 1]
        je      @chkres
        lea     edx, [edx + 2]
        cmp     edx, ecx
        jle     @charl
        jmp     @notfnd
@chkres:cmp     edx, ecx           // check within ansistring
        jge     @notfnd
        add     edx, 1
@setres:pop     ecx                // ecx = S
        pop     esi
        pop     ebx
        neg     ecx
        lea     eax, [edx + ecx + 1]
end;
{$endif PUREPASCAL}

function Split(const Str, SepStr: RawUTF8; StartPos: integer): RawUTF8;
var i: integer;
begin
  i := PosEx(SepStr,Str,StartPos);
  if i>0 then
    result := Copy(Str,StartPos,i-StartPos) else
    if StartPos=1 then
      result := Str else
      result := Copy(Str,StartPos,maxInt);
end;



{$ifndef EXTENDEDTOSTRING_USESTR}
var // standard FormatSettings (US)
    SettingsUS: TFormatSettings;
{$endif}

function ExtendedToString(var S: ShortString; Value: TSynExtended;
  Precision: integer): integer;
{$ifdef EXTENDEDTOSTRING_USESTR}
var i,prec: integer;
begin
  str(Value:0:Precision,S); // not str(Value:0,S) -> '  0.0E+0000'
  // using str() here avoid FloatToStrF() usage -> LVCL is enough
  result := length(S);
  prec := result; // if no decimal
  if S[1]='-' then
    dec(prec);
  for i := 2 to result do // test if scientific format -> return as this
    case S[i] of
    'E': exit;  // pos('E',S)>0; which Delphi 2009+ don't like
    '.': dec(prec);
    end;
  if (prec>=Precision) and (prec<>result) then begin
    dec(result,prec-Precision);
    if S[result+1]>'5' then begin // manual rounding
      prec := result;
      repeat
        case S[prec] of
        '.': ; // just ignore decimal separator
        '0'..'8': begin
          inc(S[prec]);
          break;
        end;
        '9': begin
          S[prec] := '0';
          if ((prec=2) and (S[1]='-')) or (prec=1) then begin
            System.Move(S[prec],S[prec+1],result);
            S[prec] := '1';
            break;
          end;
        end;
        else break;
        end;
        dec(prec);
      until prec=0;
    end; // note: this fixes http://stackoverflow.com/questions/2335162
  end;
  while S[result]='0' do begin
    dec(result); // trunc any trimming 0
    if S[result]='.' then begin
      dec(result);
      if (result=2) and (S[1]='-') and (S[2]='0') then begin
        result := 1;
        S[1] := '0'; // '-0.000' -> '0'
      end;
      break; // decimal were all '0' -> return only integer part
    end;
  end;
{$else}
{$ifdef UNICODE}
var i: integer;
{$endif}
begin
  // use ffGeneral: see http://synopse.info/forum/viewtopic.php?pid=442#p442
  result := FloatToText(PChar(@S[1]), Value, fvExtended, ffGeneral,
    Precision, 0, SettingsUS);
  {$ifdef UNICODE} // FloatToText(PWideChar) is faster than FloatToText(PAnsiChar)
  for i := 1 to result do
    PByteArray(@S)[i] := PWordArray(PtrInt(@S)-1)[i];
  {$endif}
{$endif EXTENDEDTOSTRING_USESTR}
end;


function RawByteStringArrayConcat(const Values: array of RawByteString): RawByteString;
var i, L: integer;
    P: PAnsiChar;
begin
  L := 0;
  for i := 0 to high(Values) do
    inc(L,length(Values[i]));
  SetString(Result,nil,L);
  P := pointer(Result);
  for i := 0 to high(Values) do begin
    L := length(Values[i]);
    System.Move(pointer(Values[i])^,P^,L);
    inc(P,L);
  end;
end;

procedure RawByteStringToBytes(const buf: RawByteString; out bytes: TBytes);
var L: Integer;
begin
  L := Length(buf);
  if L<>0 then begin
    SetLength(bytes,L);
    System.Move(pointer(buf)^,pointer(bytes)^,L);
  end;
end;

procedure BytesToRawByteString(const bytes: TBytes; out buf: RawByteString);
begin
  SetString(buf,PAnsiChar(pointer(bytes)),Length(bytes));
end;

procedure ResourceToRawByteString(const ResName: string; ResType: PChar;
  out buf: RawByteString);
var HResInfo: THandle;
    HGlobal: THandle;
begin
  HResInfo := FindResource(HInstance,PChar(ResName),ResType);
  if HResInfo=0 then
    exit;
  HGlobal := LoadResource(HInstance,HResInfo);
  if HGlobal<>0 then
    SetString(buf,PAnsiChar(LockResource(HGlobal)),SizeofResource(HInstance,HResInfo));
end;

function StrIComp2(Str1, Str2: pointer): PtrInt;
{$ifdef PUREPASCAL}
var C1, C2: AnsiChar;
begin
  if Str1<>Str2 then
  if Str1<>nil then
  if Str2<>nil then begin
    repeat
      C1 := PAnsiChar(Str1)^;
      C2 := PAnsiChar(Str2)^;
      if C1 in ['a'..'z'] then dec(C1,32);
      if C2 in ['a'..'z'] then dec(C2,32);
      if (C1<>C2) or (C1=#0) then
        break;
      Inc(PtrUInt(Str1));
      Inc(PtrUInt(Str2));
    until false;
    Result := Ord(C1) - Ord(C2);
  end else
  result := 1 else  // Str2=''
  result := -1 else // Str1=''
  result := 0;      // Str1=Str2
end;
{$else}
asm // faster version by AB, from Agner Fog's original
        mov     ecx, eax
        test    eax, edx
        jz      @n
@ok:    sub     edx, eax
        jz      @0
@10:    mov     al, [ecx]
        cmp     al, [ecx + edx]
        jne     @20
        inc     ecx
        test    al, al
        jnz     @10                    // continue with next byte
        // terminating zero found. Strings are equal
@0:     xor     eax, eax
        ret
@20:    // bytes are different. check case
        xor     al, 20H                // toggle case
        cmp     al, [ecx + edx]
        jne     @30
        // possibly differing only by case. Check if a-z
        or      al, 20H                // upper case
        sub     al, 'a'
        cmp     al, 'z' - 'a'
        ja      @30                    // not a-z
        // a-z and differing only by case
        inc     ecx
        jmp     @10                    // continue with next byte
@30:    // bytes are different,even after changing case
        movzx   eax, byte[ecx]        // get original value again
        sub     eax, 'A'
        cmp     eax, 'Z' - 'A'
        ja      @40
        add     eax, 20H
@40:    movzx   edx, byte[ecx + edx]
        sub     edx, 'A'
        cmp     edx, 'Z' - 'A'
        ja      @50
        add     edx, 20H
@50:    sub     eax, edx                 // subtract to get result
        ret
@n:     cmp     eax, edx
        je      @0
        test    eax, eax  // Str1='' ?
        jz      @max
        test    edx, edx  // Str2='' ?
        jnz     @ok
        mov     eax, 1
        ret
@max:   dec     eax
end;
{$endif}

function StrLenW(S: PWideChar): PtrInt;
begin
  result := 0;
  if S<>nil then
  while true do
    if S[result+0]<>#0 then
    if S[result+1]<>#0 then
    if S[result+2]<>#0 then
    if S[result+3]<>#0 then
      inc(result,4) else begin
      inc(result,3);
      exit;
    end else begin
      inc(result,2);
      exit;
    end else begin
      inc(result);
      exit;
    end else
      exit;
end;

function StrCompW(Str1, Str2: PWideChar): PtrInt;
begin
  if Str1<>Str2 then
  if Str1<>nil then
  if Str2<>nil then begin
    if Str1^=Str2^ then
    repeat
      if (Str1^=#0) or (Str2^=#0) then break;
      inc(Str1);
      inc(Str2);
    until Str1^<>Str2^;
    result := PWord(Str1)^-PWord(Str2)^;
    exit;
  end else
  result := 1 else  // Str2=''
  result := -1 else // Str1=''
  result := 0;      // Str1=Str2
end;

{$ifdef PUREPASCAL}

function StrLenPas(S: pointer): PtrInt;
begin
  result := 0;
  if S<>nil then
  while true do
    if PAnsiChar(S)[result+0]<>#0 then
    if PAnsiChar(S)[result+1]<>#0 then
    if PAnsiChar(S)[result+2]<>#0 then
    if PAnsiChar(S)[result+3]<>#0 then
      inc(result,4) else begin
      inc(result,3);
      exit;
    end else begin
      inc(result,2);
      exit;
    end else begin
      inc(result);
      exit;
    end else
      exit;
end;

function StrComp(Str1, Str2: pointer): PtrInt;
begin
  if Str1<>Str2 then
  if Str1<>nil then
  if Str2<>nil then begin
    if PByte(Str1)^=PByte(Str2)^ then
      repeat
        if PByte(Str1)^=0 then break;
        inc(PByte(Str1));
        inc(PByte(Str2));
      until PByte(Str1)^<>PByte(Str2)^;
    result := PByte(Str1)^-PByte(Str2)^;
    exit;
  end else
  result := 1 else  // Str2=''
  result := -1 else // Str1=''
  result := 0;      // Str1=Str2
end;

function StrCompFast(Str1, Str2: pointer): PtrInt;
begin
  if Str1<>Str2 then
  if Str1<>nil then
  if Str2<>nil then begin
    if PByte(Str1)^=PByte(Str2)^ then
      repeat
        if PByte(Str1)^=0 then break;
        inc(PByte(Str1));
        inc(PByte(Str2));
      until PByte(Str1)^<>PByte(Str2)^;
    result := PByte(Str1)^-PByte(Str2)^;
    exit;
  end else
  result := 1 else  // Str2=''
  result := -1 else // Str1=''
  result := 0;      // Str1=Str2
end;

{$else}

function StrLenPas(S: pointer): PtrInt;
asm // slower than x86/SSE* StrLen(), but won't read any byte beyond the string
        test    eax, eax
        mov     edx, eax
        jz      @0
        xor     eax, eax
@s:     cmp     byte ptr[eax + edx + 0], 0
        je      @0
        cmp     byte ptr[eax + edx + 1], 0
        je      @1
        cmp     byte ptr[eax + edx + 2], 0
        je      @2
        cmp     byte ptr[eax + edx + 3], 0
        je      @3
        add     eax, 4
        jmp     @s
@1:     inc     eax
        ret
@0:     rep     ret
@2:     add     eax, 2
        ret
@3:     add     eax, 3
end;

function StrCompFast(Str1, Str2: pointer): PtrInt;
asm // no branch taken in case of not equal first char
        cmp     eax, edx
        je      @zero  // same string or both nil
        test    eax, edx
        jz      @maynil
@1:     mov     cl, [eax]
        mov     ch, [edx]
        test    cl, cl
        lea     eax, [eax + 1]
        lea     edx, [edx + 1]
        jz      @exit
        cmp     cl, ch
        je      @1
@exit:  movzx   eax, cl
        movzx   edx, ch
        sub     eax, edx
        ret
@maynil:test    eax, eax  // Str1='' ?
        jz      @max
        test    edx, edx  // Str2='' ?
        jnz     @1
        mov     eax, 1
        ret
@max:   dec     eax
        ret
@zero:  xor     eax, eax
end;

const
  EQUAL_EACH = 8;   // see https://msdn.microsoft.com/en-us/library/bb531463
  NEGATIVE_POLARITY = 16;

function StrCompSSE42(Str1, Str2: pointer): PtrInt;
asm // warning: may read up to 15 bytes beyond the string itself
      test      eax,edx
      jz        @n
@ok:  sub       eax,edx
      jz        @0
      {$ifdef HASAESNI}
      movdqu    xmm0,dqword [edx]
      pcmpistri xmm0,dqword [edx+eax],EQUAL_EACH+NEGATIVE_POLARITY // result in ecx
      {$else}
      db $F3,$0F,$6F,$02
      db $66,$0F,$3A,$63,$04,$10,EQUAL_EACH+NEGATIVE_POLARITY
      {$endif}
      ja        @1
      jc        @2
      xor       eax,eax
      ret
@1:   add       edx,16
      {$ifdef HASAESNI}
      movdqu    xmm0,dqword [edx]
      pcmpistri xmm0,dqword [edx+eax],EQUAL_EACH+NEGATIVE_POLARITY // result in ecx
      {$else}
      db $F3,$0F,$6F,$02
      db $66,$0F,$3A,$63,$04,$10,EQUAL_EACH+NEGATIVE_POLARITY
      {$endif}
      ja        @1
      jc        @2
@0:   xor       eax,eax // Str1=Str2
      ret
@n:   cmp       eax,edx
      je        @0
      test      eax,eax  // Str1='' ?
      jz        @max
      test      edx,edx  // Str2='' ?
      jnz       @ok
      mov       eax,1
      ret
@max: dec       eax
      ret
@2:   add       eax,edx
      movzx     eax,byte ptr [eax+ecx]
      movzx     edx,byte ptr [edx+ecx]
      sub       eax,edx
end;

function SortDynArrayAnsiStringSSE42(const A,B): integer;
asm // warning: may read up to 15 bytes beyond the string itself
      mov       eax,[eax]
      mov       edx,[edx]
      test      eax,edx
      jz        @n
@ok:  sub       eax,edx
      jz        @0
      {$ifdef HASAESNI}
      movdqu    xmm0,dqword [edx]
      pcmpistri xmm0,dqword [edx+eax],EQUAL_EACH+NEGATIVE_POLARITY // result in ecx
      {$else}
      db $F3,$0F,$6F,$02
      db $66,$0F,$3A,$63,$04,$10,EQUAL_EACH+NEGATIVE_POLARITY
      {$endif}
      ja        @1
      jc        @2
      xor       eax,eax
      ret
@1:   add       edx,16
      {$ifdef HASAESNI}
      movdqu    xmm0,dqword [edx]
      pcmpistri xmm0,dqword [edx+eax],EQUAL_EACH+NEGATIVE_POLARITY // result in ecx
      {$else}
      db $F3,$0F,$6F,$02
      db $66,$0F,$3A,$63,$04,$10,EQUAL_EACH+NEGATIVE_POLARITY
      {$endif}
      ja        @1
      jc        @2
@0:   xor       eax,eax // Str1=Str2
      ret
@n:   cmp       eax,edx
      je        @0
      test      eax,eax  // Str1='' ?
      jz        @max
      test      edx,edx  // Str2='' ?
      jnz       @ok
      or        eax,-1
      ret
@max: inc       eax
      ret
@2:   add       eax,edx
      movzx     eax,byte ptr [eax+ecx]
      movzx     edx,byte ptr [edx+ecx]
      sub       eax,edx
end;

{$endif PUREPASCAL}

function IdemPropNameU(const P1,P2: RawUTF8): boolean;
{$ifdef PUREPASCAL}
var i,j,L: integer;
begin
  result := false;
  L := length(P1);
  if L<>length(P2) then
    exit;
  j := 1;
  for i := 1 to L shr 2 do
    if (PCardinal(@P1[j])^ xor PCardinal(@P2[j])^) and $dfdfdfdf<>0 then
      exit else
      inc(j,4);
  for i := j to L do
    if (ord(P1[i]) xor ord(P2[i])) and $df<>0 then
      exit;
  result := true;
end;
{$else}
asm // eax=p1, edx=p2
        cmp     eax, edx
        je      @out1
        test    eax, edx
        jz      @maybenil
@notnil:mov     ecx, [eax - 4] // compare lengths
        cmp     ecx, [edx - 4]
        jne     @out1
        push    ebx
        lea     edx, [edx + ecx - 4]  // may include the length for shortest strings
        lea     ebx, [eax + ecx - 4]
        neg     ecx
        mov     eax, [ebx]     // compare last 4 chars
        xor     eax, [edx]
        and     eax, $dfdfdfdf // case insensitive
        jne     @out2
@by4:   add     ecx, 4
        jns     @match
        mov     eax, [ebx + ecx]
        xor     eax, [edx + ecx]
        and     eax, $dfdfdfdf // case insensitive
        je      @by4
@out2:  pop     ebx
@out1:  setz    al
        ret
@match: mov     al, 1
        pop     ebx
        ret
@maybenil: // here we know that eax<>edx
        test    eax, eax
        jz      @nil0     // eax=nil and eax<>edx -> edx<>nil -> false
        test    edx, edx
        jnz     @notnil
        mov     al, dl    // eax<>nil and edx=nil -> false
@nil0:
end;
{$endif}

function IdemPropName(const P1,P2: shortstring): boolean; overload;
begin
  if P1[0]=P2[0] then
    result := IdemPropNameUSameLen(@P1[1],@P2[1],ord(P2[0])) else
    result := false;
end;

function IdemPropName(const P1: shortstring; P2: PUTF8Char; P2Len: integer): boolean; overload;
begin
  if ord(P1[0])=P2Len then
    result := IdemPropNameUSameLen(@P1[1],P2,P2Len) else
    result := false;
end;

function IdemPropName(P1,P2: PUTF8Char; P1Len,P2Len: integer): boolean; overload;
begin
  if P1Len=P2Len then
    result := IdemPropNameUSameLen(P1,P2,P2Len) else
    result := false;
end;

function IdemPropNameU(const P1: RawUTF8; P2: PUTF8Char; P2Len: integer): boolean;
begin
  if length(P1)=P2Len then
    result := IdemPropNameUSameLen(pointer(P1),P2,P2Len) else
    result := false;
end;

function IdemPropNameUSameLen(P1,P2: PUTF8Char; P1P2Len: integer): boolean;
var i,j: integer;
begin
  result := false;
  j := 0;
  for i := 1 to P1P2Len shr 2 do
    if (PCardinal(PtrInt(P1)+j)^ xor PCardinal(@P2[j])^) and $dfdfdfdf<>0 then
      exit else
      inc(j,4);
  for i := j to P1P2Len-1 do
    if (PByteArray(P1)^[i] xor ord(P2[i])) and $df<>0 then
      exit;
  result := true;
end;



procedure YearToPChar(Y: cardinal; P: PUTF8Char);
{$ifdef PUREPASCAL}
var d100: cardinal;
begin
  if Y<=9999 then begin
    d100 := Y div 100;
    PWordArray(P)[0] := TwoDigitLookupW[d100];
    PWordArray(P)[1] := TwoDigitLookupW[Y-(d100*100)];
  end else
    PCardinal(P)^ := $39393939; // '9999'
end;
{$else}
asm // eax=Y, edx=P
        cmp     eax, 9999
        push    edx
        mov     ecx, eax
        ja      @big
        mov     edx, 1374389535 // use power of two reciprocal to avoid division
        mul     edx
        shr     edx, 5          // now edx=Y div 100
        movzx   eax, word ptr[TwoDigitLookup + edx * 2]
        imul    edx, -200
        movzx   edx, word ptr[TwoDigitLookup + ecx * 2 + edx]
        pop     ecx
        shl     edx, 16
        or      eax, edx
        mov     [ecx], eax
        ret
@big:   pop     eax
        mov     dword ptr [edx], $39393939 // '9999'
end;
{$endif}

function UInt3DigitsToUTF8(Value: Cardinal): RawUTF8;
begin
  SetString(result,nil,3);
  PWordArray(result)[0] := TwoDigitLookupW[Value div 10];
  PByteArray(result)[2] := (Value mod 10)+48;
end;

function UInt4DigitsToUTF8(Value: Cardinal): RawUTF8;
begin
  SetString(result,nil,4);
  YearToPChar(Value,pointer(result));
end;

function SameValue(const A, B: Double; DoublePrec: double): Boolean;
var AbsA,AbsB: double;
begin // faster than the Math unit version
  AbsA := Abs(A);
  AbsB := Abs(B);
  if AbsA<AbsB then
    AbsA := AbsA*DoublePrec else
    AbsA := AbsB*DoublePrec; // AbsA := Min(Abs(A),Abs(B))*DoublePrec
  // AbsA is the allowed Epsilon value
  if AbsA<DoublePrec then
    Result := Abs(A-B)<=DoublePrec else
    Result := Abs(A-B)<=AbsA;
end;

function SameValueFloat(const A, B: TSynExtended; DoublePrec: TSynExtended): Boolean;
var AbsA,AbsB: TSynExtended;
begin // faster than the Math unit version
  AbsA := Abs(A);
  AbsB := Abs(B);
  if AbsA<AbsB then
    AbsA := AbsA*DoublePrec else
    AbsA := AbsB*DoublePrec; // AbsA := Min(Abs(A),Abs(B))*DoublePrec
  // AbsA is the allowed Epsilon value
  if AbsA<DoublePrec then
    Result := Abs(A-B)<=DoublePrec else
    Result := Abs(A-B)<=AbsA;
end;


function FindPropName(const Names: array of RawUTF8; const Name: RawUTF8): integer;
{$ifdef HASINLINE}
var NameLen: integer;
begin
  NameLen := Length(Name);
  for result := 0 to high(Names) do
    if (Length(Names[result])=NameLen) and
       IdemPropNameUSameLen(pointer(Names[result]),pointer(Name),NameLen) then
      exit;
  result := -1;
end;
{$else}
begin
  for result := 0 to high(Names) do
    if IdemPropNameU(Names[result],Name) then
      exit;
  result := -1;
end;
{$endif}


procedure Reverse(const Values: TIntegerDynArray; ValuesCount: integer;
  Reversed: PIntegerArray);
var i: integer;
begin
  i := 0;
  if ValuesCount>=4 then begin
    dec(ValuesCount,4);
    while i<ValuesCount do begin // faster pipelined version
      Reversed[Values[i]] := i;
      Reversed[Values[i+1]] := i+1;
      Reversed[Values[i+2]] := i+2;
      Reversed[Values[i+3]] := i+3;
      inc(i,4);
    end;
    inc(ValuesCount,4);
  end;
  while i<ValuesCount do begin
    Reversed[Values[i]] := i;
    inc(i);
  end;
  //for i := 0 to Count-1 do Assert(Reverse[Orig[i]]=i);
end;

procedure FillIncreasing(Values: PIntegerArray; StartValue, Count: integer);
var i: integer;
begin
  if StartValue=0 then
    for i := 0 to Count-1 do
      Values[i] := i else
    for i := 0 to Count-1 do
      Values[i] := StartValue+i;
end;



function TIntegerDynArrayFrom(const Values: array of integer): TIntegerDynArray;
var i: integer;
begin
  SetLength(result,length(Values));
  for i := 0 to high(Values) do
    result[i] := Values[i];
end;



function TInt64DynArrayFrom(const Values: TIntegerDynArray): TInt64DynArray;
var i: integer;
begin
  SetLength(result,length(Values));
  for i := 0 to high(Values) do
    result[i] := Values[i];
end;

function GetInteger(P: PUTF8Char): PtrInt;
var c: PtrUInt;
    minus: boolean;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  if P^='-' then begin
    minus := true;
    repeat inc(P) until P^<>' ';
  end else begin
    minus := false;
    if P^='+' then
      repeat inc(P) until P^<>' ';
  end;
  c := byte(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+PtrInt(c);
      inc(P);
    until false;
  end;
  if minus then
    result := -result;
end;

function GetInteger(P: PUTF8Char; var err: integer): PtrInt;
var c: PtrUInt;
    minus: boolean;
begin
  if P=nil then begin
    result := 0;
    err := 1;
    exit;
  end else
    err := 0;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  if P^='-' then begin
    minus := true;
    repeat inc(P) until P^<>' ';
  end else begin
    minus := false;
    if P^='+' then
      repeat inc(P) until P^<>' ';
  end;
  c := byte(P^)-48;
  if c>9 then begin
    err := 1;
    result := 0;
    exit;
  end else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then begin
        if byte(P^)<>0 then
          err := 1; // always return 1 as err code -> don't care about char index
        break;
      end else
        result := result*10+PtrInt(c);
      inc(P);
    until false;
  end;
  if minus then
    result := -result;
end;

function GetIntegerDef(P: PUTF8Char; Default: PtrInt): PtrInt;
var err: integer;
begin
  result := GetInteger(P,err);
  if err<>0 then
    result := Default;
end;

function UTF8ToInteger(const value: RawUTF8; Default: PtrInt=0): PtrInt;
var err: integer;
begin
  result := GetInteger(pointer(value),err);
  if err<>0 then
    result := Default;
end;

function ToCardinal(const text: RawUTF8; out value: cardinal; minimal: cardinal): boolean;
begin
  value := GetCardinalDef(pointer(text),cardinal(-1));
  result := (value<>cardinal(-1)) and (value>=minimal);
end;

function ToInt64(const text: RawUTF8; out value: Int64): boolean;
var err: integer;
begin
  value := GetInt64(pointer(text),err);
  result := err=0;
end;

//function GetBoolean(P: PUTF8Char): boolean;
//begin
//  if P<>nil then
//    case PInteger(P)^ of
//      TRUE_LOW:  result := true;
//      FALSE_LOW: result := false;
//      else result := PWord(P)^<>ord('0');
//    end else
//    result := false;
//end;

function GetCardinalDef(P: PUTF8Char; Default: PtrUInt): PtrUInt;
var c: PtrUInt;
begin
  if P=nil then begin
    result := Default;
    exit;
  end;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  c := byte(P^)-48;
  if c>9 then
    result := Default else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+PtrUInt(c);
      inc(P);
    until false;
  end;
end;

function GetCardinal(P: PUTF8Char): PtrUInt;
var c: PtrUInt;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  c := byte(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+PtrUInt(c);
      inc(P);
    until false;
  end;
end;

function GetCardinalW(P: PWideChar): PtrUInt;
var c: PtrUInt;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  if ord(P^) in [1..32] then repeat inc(P) until not(ord(P^) in [1..32]);
  c := word(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := word(P^)-48;
      if c>9 then
        break else
        result := result*10+c;
      inc(P);
    until false;
  end;
end;

{$ifdef CPU64}
procedure SetInt64(P: PUTF8Char; var result: Int64);
begin // PtrInt is already int64 -> call PtrInt version
  result := GetInteger(P);
end;
{$else}
procedure SetInt64(P: PUTF8Char; var result: Int64);
var c: cardinal;
    minus: boolean;
begin
  result := 0;
  if P=nil then
    exit;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  if P^='-' then begin
    minus := true;
    repeat inc(P) until P^<>' ';
  end else begin
    minus := false;
    if P^='+' then
      repeat inc(P) until P^<>' ';
  end;
  c := byte(P^)-48;
  if c>9 then
    exit;
  Int64Rec(result).Lo := c;
  inc(P);
  repeat // fast 32 bit loop
    c := byte(P^)-48;
    if c>9 then
      break else
      Int64Rec(result).Lo := Int64Rec(result).Lo*10+c;
    inc(P);
    if Int64Rec(result).Lo>=high(cardinal)div 10 then begin
      repeat // 64 bit loop
        c := byte(P^)-48;
        if c>9 then
          break;
        result := result shl 3+result+result; // fast result := result*10
        inc(result,c);
        inc(P);
      until false;
      break;
    end;
  until false;
  if minus then
    result := -result;
end;
{$endif}

{$ifdef CPU64}
function GetInt64(P: PUTF8Char): Int64;
begin // PtrInt is already int64 -> call previous version
  result := GetInteger(P);
end;
{$else}
function GetInt64(P: PUTF8Char): Int64;
begin
  SetInt64(P,result);
end;
{$endif}

function GetInt64Def(P: PUTF8Char; const Default: Int64): Int64;
var err: integer;
begin
  result := GetInt64(P,err);
  if err>0 then
    result := Default;
end;

{$ifdef CPU64}
function GetInt64(P: PUTF8Char; var err: integer): Int64;
begin // PtrInt is already int64 -> call previous version
  result := GetInteger(P,err);
end;
{$else}
function GetInt64(P: PUTF8Char; var err: integer): Int64;
var c: cardinal;
    minus: boolean;
begin
  err := 0;
  result := 0;
  if P=nil then
    exit;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  if P^='-' then begin
    minus := true;
    repeat inc(P) until P^<>' ';
  end else begin
    minus := false;
    if P^='+' then
      repeat inc(P) until P^<>' ';
  end;
  inc(err);
  c := byte(P^)-48;
  if c>9 then
    exit;
  Int64Rec(result).Lo := c;
  inc(P);
  repeat // fast 32 bit loop
    c := byte(P^);
    if c<>0 then begin
      dec(c,48);
      inc(err);
      if c>9 then
        exit;
      Int64Rec(result).Lo := Int64Rec(result).Lo*10+c;
      inc(P);
      if Int64Rec(result).Lo>=high(cardinal)div 10 then begin
        repeat // 64 bit loop
          c := byte(P^);
          if c=0 then begin
            err := 0; // conversion success without error
            break;
          end;
          dec(c,48);
          inc(err);
          if c>9 then
            exit else
            result := result shl 3+result+result; // fast result := result*10
          inc(result,c);
          if result<0 then
            exit; // overflow (>$7FFFFFFFFFFFFFFF)
          inc(P);
        until false;
        break;
      end;
    end else begin
      err := 0; // reached P^=#0 -> conversion success without error
      break;
    end;
  until false;
  if minus then
    result := -result;
end;
{$endif}


const
  NULCHAR: AnsiChar = #0;


{ ************ variant-based process, including JSON/BSON document content }

function SetVariantUnRefSimpleValue(const Source: variant; var Dest: TVarData): boolean;
var typ: word;
begin
  if TVarData(Source).VType and varByRef<>0 then begin
    typ := TVarData(Source).VType and not varByRef;
    case typ of
    varVariant:
      if PVarData(TVarData(Source).VPointer)^.VType in
          [varEmpty..varDate,varBoolean,varShortInt..varWord64] then begin
        Dest := PVarData(TVarData(Source).VPointer)^;
        result := true;
      end else
        result := false;
    varEmpty..varDate,varBoolean,varShortInt..varWord64: begin
      Dest.VType := typ;
      Dest.VInt64 :=  PInt64(TVarData(Source).VAny)^;
      result := true;
    end;
    else
      result := false;
    end;
  end else
    result := false;
end;

{$ifndef LVCL}

procedure RawByteStringToVariant(Data: PByte; DataLen: Integer; var Value: variant);
begin
  with TVarData(Value) do begin
    if VType and VTYPE_STATIC<>0 then
      VarClear(Value);
    if (Data=nil) or (DataLen<=0) then
      VType := varNull else begin
      VType := varString;
      VAny := nil; // avoid GPF below when assigning a string variable to VAny
      SetString(RawByteString(VAny),PAnsiChar(Data),DataLen);
    end;
  end;
end;

procedure RawByteStringToVariant(const Data: RawByteString; var Value: variant);
begin
  with TVarData(Value) do begin
    if VType and VTYPE_STATIC<>0 then
      VarClear(Value);
    if Data='' then
      VType := varNull else begin
      VType := varString;
      VAny := nil; // avoid GPF below when assigning a string variable to VAny
      RawByteString(VAny) := Data;
    end;
  end;
end;

procedure VariantToRawByteString(const Value: variant; var Dest: RawByteString);
begin
  case TVarData(Value).VType of
  varEmpty, varNull:
    Dest := '';
  varString:
    Dest := RawByteString(TVarData(Value).VAny);
  else // not from RawByteStringToVariant() -> conversion to string
    Dest := {$ifdef UNICODE}RawByteString{$else}string{$endif}(Value);
  end;
end;

procedure SetVariantNull(var Value: variant);
begin // slightly faster than Value := Null
  VarClear(Value);
  TVarData(Value).VType := varNull;
end;

{$endif LVCL}

function VarIsEmptyOrNull(const V: Variant): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function VarDataIsEmptyOrNull(VarData: pointer): Boolean;
begin
  repeat
    if PVarData(VarData)^.VType<>varVariant or varByRef then
      break;
    VarData := PVarData(VarData)^.VPointer;
    if VarData=nil then begin
      result := true;
      exit;
    end;
  until false;
  result := (PVarData(VarData)^.VType<=varNull) or
            (PVarData(VarData)^.VType=varNull or varByRef);
end;

function VarIs(const V: Variant; const VTypes: TVarDataTypes): Boolean;
var VD: PVarData;
begin
  VD := @V;
  repeat
    if VD^.VType<>varVariant or varByRef then
      break;
    VD := VD^.VPointer;
    if VD=nil then begin
      result := false;
      exit;
    end;
  until false;
  result := VD^.VType in VTypes;
end;

function VarIsVoid(const V: Variant): boolean;
begin
  with TVarData(V) do
    case VType of
    varEmpty,varNull:
      result := true;
    varBoolean:
      result := not VBoolean;
    varString,varOleStr{$ifdef HASVARUSTRING},varUString{$endif}:
      result := VAny=nil;
    varDate:
      result := VInt64=0;
    else
      if VType=varVariant or varByRef then
        result := VarIsVoid(PVariant(VPointer)^) else
      if (VType=varByRef or varString) or (VType=varByRef or varOleStr)
         {$ifdef HASVARUSTRING} or (VType=varByRef or varUString) {$endif} then
        result := PPointer(VAny)^=nil else
      ///{$ifndef NOVARIANTS}
      ///if VType=word(DocVariantVType) then
      ///  result := TDocVariantData(V).Count=0 else
      ///{$endif}
        result := false;
    end;
end;


{$ifndef NOVARIANTS}



{$ifndef FPC} // better not try it with FPC - rely on the current implementation

function VariantsDispInvokeAddress: pointer;
asm
  {$ifdef CPU64}
  mov rax,offset Variants.@DispInvoke
  {$else}
  mov eax,offset Variants.@DispInvoke
  {$endif}
end;

{$ifdef DOPATCHTRTL}
  {$define DOPATCHDISPINVOKE} // much faster late-binding process for our types
{$endif}
{$ifdef CPU64}
  {$define DOPATCHDISPINVOKE}
  // we NEED our patched DispInvoke to circumvent some Delphi bugs on Win64
{$endif}
{$ifdef DELPHI6OROLDER}
  {$define DOPATCHDISPINVOKE}
  // to circumvent LIdent := Uppercase() in TInvokeableVariantType.DispInvoke()
{$endif}

{$endif FPC}


{$endif NOVARIANTS}


{ ****************** TDynArray wrapper }

function DynArrayElementTypeName(TypeInfo: pointer; ElemTypeInfo: PPointer): RawUTF8;
var DynArray: TDynArray;
    VoidArray: pointer;
const KNOWNTYPE_ITEMNAME: array[TDynArrayKind] of RawUTF8 = ('',
  'boolean','byte','word','integer','cardinal','single','Int64','double','currency',
  'TTimeLog','TDateTime','RawUTF8','WinAnsiString','string','RawByteString',
  'WideString','SynUnicode','IInterface',{$ifndef NOVARIANTS}'variant',{$endif}'');
begin
  VoidArray := nil;
  DynArray.Init(TypeInfo,VoidArray);
  result := '';
  if ElemTypeInfo<>nil then
    ElemTypeInfo^ := DynArray.ElemType;
  if DynArray.ElemType<>nil then
    TypeInfoToName(ElemTypeInfo,result) else
    result := KNOWNTYPE_ITEMNAME[DynArray.ToKnownType];
end;



function TDynArray.Add(const Elem): integer;
begin
  result := Count;
  if fValue=nil then
    exit; // avoid GPF if void
  SetCount(result+1);
  ElemCopy(Elem,pointer(PtrUInt(fValue^)+PtrUInt(result)*ElemSize)^);
end;

function TDynArray.New: integer;
begin
  result := Count;
  if fValue=nil then
    exit; // avoid GPF if void
  SetCount(result+1);
end;

procedure TDynArray.Insert(Index: Integer; const Elem);
var n: integer;
    P: PByteArray;
begin
  if fValue=nil then
    exit; // avoid GPF if void
  n := Count;
  SetCount(n+1);
  if cardinal(Index)<cardinal(n) then begin
    P := pointer(PtrUInt(fValue^)+PtrUInt(Index)*ElemSize);
    System.Move(P[0],P[ElemSize],cardinal(n-Index)*ElemSize);
    if ElemType<>nil then
      System.FillChar(P[0],ElemSize,0); // avoid GPF in ElemCopy() below
  end else
    // Index>=Count -> add at the end
    P := pointer(PtrUInt(fValue^)+PtrUInt(n)*ElemSize);
  ElemCopy(Elem,P^);
end;

procedure TDynArray.Clear;
begin
  SetCount(0);
end;

function TDynArray.ClearSafe: boolean;
begin
  try
    SetCount(0);
    result := true;
  except // weak code, but may be a good idea in a destructor
    result := false;
  end;
end;

procedure TDynArray.Delete(aIndex: Integer);
var n, len: integer;
    P: PAnsiChar;
    zerolast: boolean;
begin
  if fValue=nil then
    exit; // avoid GPF if void
  n := Count;
  if cardinal(aIndex)>=cardinal(n) then
    exit; // out of range
  dec(n);
  P := pointer(PtrUInt(fValue^)+PtrUInt(aIndex)*ElemSize);
  if ElemType<>nil then begin
    ///////////_Finalize(P,ElemType);
    zerolast := true;
  end else
    if GetIsObjArray then begin
      FreeAndNil(PObject(P)^);
      zerolast := true;
    end else
    zerolast := false;
  if n>aIndex then begin
    len := cardinal(n-aIndex)*ElemSize;
    System.Move(P[ElemSize],P[0],len);
    if zerolast then // avoid GPF
      System.FillChar(P[len],ElemSize,0);
  end;
  SetCount(n);
end;

function TDynArray.ElemPtr(aIndex: integer): pointer;
begin
  result := nil;
  if (fValue=nil) or (fValue^=nil) then
    exit;
  if fCountP<>nil then begin
    if cardinal(aIndex)>=PCardinal(fCountP)^ then
      exit;
  end else
    {$ifdef FPC}
    if cardinal(aIndex)>=cardinal(PDynArrayRec(PtrUInt(fValue^)-SizeOf(TDynArrayRec))^.length) then
    {$else}
    if cardinal(aIndex)>=PCardinal(PtrUInt(fValue^)-sizeof(PtrInt))^ then
    {$endif}
      exit;
  result := pointer(PtrUInt(fValue^)+PtrUInt(aIndex)*ElemSize);
end;

function TDynArray.GetCount: integer;
begin
  if fValue<>nil then
    if fCountP=nil then
      if PtrInt(fValue^)<>0 then begin
        {$ifdef FPC}
        result := PDynArrayRec(PtrUInt(fValue^)-SizeOf(TDynArrayRec))^.length;
        {$else}
        result := PInteger(PtrUInt(fValue^)-sizeof(PtrInt))^;
        {$endif}
        exit;
      end else begin
        result := 0;
        exit;
      end else begin
      result := fCountP^;
      exit;
    end else begin
    result := 0; // avoid GPF if void
    exit;
  end;
end;

//procedure TDynArray.Reverse;
//var i, siz, n, tmp: integer;
//    P1, P2: PAnsiChar;
//    c: AnsiChar;
//    i64: Int64;
//begin
//  n := Count-1;
//  if n>0 then begin
//    siz := ElemSize;
//    P1 := fValue^;
//    case siz of
//    1: begin
//      // optimized version for TByteDynArray and such
//      P2 := P1+n;
//      for i := 1 to n shr 1 do begin
//        c := P1^;
//        P1^ := P2^;
//        P2^ := c;
//        inc(P1);
//        dec(P2);
//      end;
//    end;
//    4: begin
//      // optimized version for TIntegerDynArray + TRawUTF8DynArray and such
//      P2 := P1+n*sizeof(Integer);
//      for i := 1 to n shr 1 do begin
//        tmp := PInteger(P1)^;
//        PInteger(P1)^ := PInteger(P2)^;
//        PInteger(P2)^ := tmp;
//        inc(P1,4);
//        dec(P2,4);
//      end;
//    end;
//    8: begin
//      // optimized version for TInt64DynArray + TDoubleDynArray and such
//      P2 := P1+n*sizeof(Int64);
//      for i := 1 to n shr 1 do begin
//        i64 := PInt64(P1)^;
//        PInt64(P1)^ := PInt64(P2)^;
//        PInt64(P2)^ := i64;
//        inc(P1,8);
//        dec(P2,8);
//      end;
//    end;
//    16: begin
//      // optimized version for TVariantDynArray and such
//      P2 := P1+n*16;
//      for i := 1 to n shr 1 do begin
//        Exchg16(Pointer(P1),Pointer(P2));
//        inc(P1,16);
//        dec(P2,16);
//      end;
//    end;
//    else begin
//      // generic version
//      P2 := P1+n*siz;
//      for i := 1 to n shr 1 do begin
//        Exchg(P1,P2,siz);
//        inc(P1,siz);
//        dec(P2,siz);
//      end;
//    end;
//    end;
//  end;
//end;

const
  PTRSIZ = sizeof(Pointer);
  KNOWNTYPE_SIZE: array[TDynArrayKind] of byte = (
    0, 1,1, 2, 4,4,4, 8,8,8,8,8, PTRSIZ,PTRSIZ,PTRSIZ,PTRSIZ,PTRSIZ,PTRSIZ,PTRSIZ,
    {$ifndef NOVARIANTS}sizeof(Variant),{$endif} 0);

function TDynArray.GetArrayTypeName: RawUTF8;
begin
  TypeInfoToName(fTypeInfo,result);
end;

function TDynArray.ToKnownType(exactType: boolean): TDynArrayKind;
var nested: PTypeInfo;
label Bin, Rec;
begin
  if fKnownType<>djNone then begin
    result := fKnownType;
    exit;
  end;

  if (fKnownType=djNone) and not exactType then begin
    fKnownSize := 0;
    if ElemType=nil then
Bin:  case ElemSize of
      1: fKnownType := djByte;
      2: fKnownType := djWord;
      4: fKnownType := djInteger;
      8: fKnownType := djInt64;
      else fKnownSize := ElemSize;
      end else
    case PTypeKind(ElemType)^ of
      tkLString{$ifdef FPC},tkLStringOld{$endif}: fKnownType := djRawUTF8;
      tkWString: fKnownType := djWideString;
      {$ifdef UNICODE}
      tkUString: fKnownType := djString;
      {$endif}
      {$ifndef NOVARIANTS}
      tkVariant: fKnownType := djVariant;
      {$endif}
      tkInterface: fKnownType := djInterface;
      tkRecord{$ifdef FPC},tkObject{$endif}: begin
        nested := ElemType; // inlined GetTypeInfo()
        {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
rec:    nested := GetFPCAlignPtr(nested);
        {$else}
rec:    inc(PtrUInt(nested),(nested^.NameLen));
        {$endif}
        if nested^.ManagedCount=0 then // only binary content -> full content
          goto Bin;
        with nested^.ManagedFields[0] do
        case Offset of
        0: case TypeInfo^.Kind of
            tkLString{$ifdef FPC},tkLStringOld{$endif}: fKnownType := djRawUTF8;
            tkWString: fKnownType := djWideString;
            {$ifdef UNICODE}
            tkUString: fKnownType := djString;
            {$endif}
            tkRecord{$ifdef FPC},tkObject{$endif}: begin
              nested := Deref(TypeInfo);
              goto Rec;
            end;
            {$ifndef NOVARIANTS}
            tkVariant: fKnownType := djVariant;
            {$endif}
            else begin
              {$ifdef FPC} // unmanaged fields have RTTI in newest FPC! :)
              if (nested^.ManagedCount<>1) and // emulate Delphi behavior
                 (nested^.ManagedFields[1].TypeInfo^.Kind in tkManagedTypes) then
              case nested^.ManagedFields[1].Offset of
                1: fKnownType := djByte;
                2: fKnownType := djWord;
                4: fKnownType := djInteger;
                8: fKnownType := djInt64;
                else fKnownSize := nested^.ManagedFields[1].Offset;
              end else
              {$endif}
              goto bin;
            end;
           end;
        1: fKnownType := djByte;
        2: fKnownType := djWord;
        4: fKnownType := djInteger;
        8: fKnownType := djInt64;
        else fKnownSize := Offset;
        end;
      end;
    end;
  end;
  if KNOWNTYPE_SIZE[fKnownType]<>0 then
    fKnownSize := KNOWNTYPE_SIZE[fKnownType];
  result := fKnownType;
end;

function TDynArray.ElemEquals(const A,B): boolean;
begin
    if ElemType=nil then
      case ElemSize of // optimized versions for arrays of common types
        1: result := byte(A)=byte(B);
        2: result := word(A)=word(B);
        4: result := cardinal(A)=cardinal(B);
        8: result := Int64(A)=Int64(B);
      else result := CompareMem(@A,@B,ElemSize); // generic comparison
      end else
    case PTypeKind(ElemType)^ of
    tkLString{$ifdef FPC},tkLStringOld{$endif}:
      result := AnsiString(A)=AnsiString(B);
    tkWString:
      result := WideString(A)=WideString(B);
    {$ifdef HASVARUSTRING}
    tkUString:
      result := UnicodeString(A)=UnicodeString(B);
    {$endif}
    tkInterface:
      result := pointer(A)=pointer(B);
    {$ifndef NOVARIANTS}
    tkVariant:
      result := Variant(A)=Variant(B);
    {$endif}
    else result := false;
    end;
end;

function TDynArray.IndexOf(const Elem): integer;
var P: pointer;
    max: integer;
begin
  if fValue=nil then begin
    result := -1;
    exit; // avoid GPF if void
  end;
  max := Count-1;
  P := fValue^;
  if @Elem<>nil then
  if ElemType=nil then
  case ElemSize of
    // optimized versions for arrays of byte,word,integer,Int64,Currency,Double
    1: for result := 0 to max do
         if PByteArray(P)^[result]=byte(Elem) then exit;
    2: for result := 0 to max do
         if PWordArray(P)^[result]=word(Elem) then exit;
    4: for result := 0 to max do // integer,single,32bitPointer
         if PIntegerArray(P)^[result]=integer(Elem) then exit;
    8: for result := 0 to max do // Int64,Currency,Double,64bitPointer
         if PInt64Array(P)^[result]=Int64(Elem) then exit;
  else // generic binary comparison (fast with our overloaded CompareMem)
    for result := 0 to max do
      if CompareMem(P,@Elem,ElemSize) then
        exit else
        inc(PtrUInt(P),ElemSize);
  end else
  case PTypeKind(ElemType)^ of
  tkLString{$ifdef FPC},tkLStringOld{$endif}:
    for result := 0 to max do
      if AnsiString(PPtrIntArray(P)^[result])=AnsiString(Elem) then exit;
  tkWString:
    for result := 0 to max do
      if WideString(PPtrIntArray(P)^[result])=WideString(Elem) then exit;
  {$ifdef HASVARUSTRING}
  tkUString:
    for result := 0 to max do
      if UnicodeString(PPtrIntArray(P)^[result])=UnicodeString(Elem) then exit;
  {$endif}
  {$ifndef NOVARIANTS}
  tkVariant:
    for result := 0 to max do
      if PVariantArray(P)^[result]=variant(Elem) then exit;
  {$endif}
  tkInterface:
    for result := 0 to max do
      if PPtrIntArray(P)^[result]=PtrInt(Elem) then exit;
  end;
  result := -1;
end;

procedure TDynArray.Init(aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil);
begin
  fValue := @aValue;
  fTypeInfo := aTypeInfo;
  if PTypeKind(aTypeInfo)^<>tkDynArray then // inlined GetTypeInfo()
    raise Exception.Create('TDynArray.Init('+String( PShortString(@PTypeInfo(aTypeInfo)^.NameLen)^)+'): not a dynamic array');
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  aTypeInfo := GetFPCAlignPtr(aTypeInfo);
  {$else}
  inc(PtrUInt(aTypeInfo),PTypeInfo(aTypeInfo)^.NameLen);
  {$endif}
  fElemSize := PTypeInfo(aTypeInfo)^.elSize {$ifdef FPC}and $7FFFFFFF{$endif};
  fElemType := PTypeInfo(aTypeInfo)^.elType;
  if fElemType<>nil then begin
    {$ifndef HASDIRECTTYPEINFO}
    // FPC compatibility: if you have a GPF here at startup, your 3.1 trunk
    // revision seems older than June 2016
    // -> enable HASDIRECTTYPEINFO conditional below $ifdef VER3_1 in Synopse.inc
    // or in your project's options
    fElemType := PPointer(fElemType)^;
    {$endif}
    {$ifdef FPC}
    if not (PTypeKind(fElemType)^ in tkManagedTypes) then
      fElemType := nil; // as with Delphi
    {$endif}
  end;
  fCountP := aCountPointer;
  if fCountP<>nil then
    fCountP^ := 0;
  fKnownSize := 0;
  fKnownType := djNone;
  fIsObjArray := oaUnknown;
end;                

procedure TDynArray.InitSpecific(aTypeInfo: pointer; var aValue; aKind: TDynArrayKind;
  aCountPointer: PInteger=nil; aCaseInsensitive: boolean=false);
begin
  Init(aTypeInfo,aValue,aCountPointer);
  fKnownType := aKind;
  fKnownSize := KNOWNTYPE_SIZE[aKind];
end;

procedure TDynArray.UseExternalCount(var aCountPointer: Integer);
begin
  fCountP := @aCountPointer;
end;

procedure TDynArray.Void;
begin
  fValue := nil;
end;

function TDynArray.IsVoid: boolean;
begin
  result := fValue=nil;
end;

procedure _DynArrayClear(var a: Pointer; typeInfo: Pointer);
{$ifdef FPC}
  [external name 'FPC_DYNARRAY_CLEAR'];
{$else}
asm
{$ifdef CPU64}
  .NOFRAME
{$endif}
  jmp System.@DynArrayClear
end;
{$endif}

procedure _FinalizeArray(p: Pointer; typeInfo: Pointer; elemCount: PtrUInt);
{$ifdef FPC}
  [external name 'FPC_FINALIZE_ARRAY'];
{$else}
asm
{$ifdef CPU64}
  .NOFRAME
{$endif}
  jmp System.@FinalizeArray
end;
{$endif}

function TDynArray.GetIsObjArray: boolean;
begin
  if fIsObjArray=oaUnknown then
    if (fElemSize=sizeof(pointer)) and (fElemType=nil) and
       Assigned(DynArrayIsObjArray) and DynArrayIsObjArray(fTypeInfo) then
      fIsObjArray := oaTrue else
      fIsObjArray := oaFalse;
  result := fIsObjArray=oaTrue;
end;

procedure TDynArray.SetIsObjArray(aValue: boolean);
begin
  if aValue then
    fIsObjArray := oaTrue else
    fIsObjArray := oaFalse;
end;

procedure TDynArray.InternalSetLength(NewLength: PtrUInt);
var p: PDynArrayRec;
    pa: PAnsiChar absolute p;
    OldLength, NeededSize, minLength: PtrUInt;
    pp: pointer;
    i: integer;
begin // this method is faster than default System.DynArraySetLength() function
  // check that new array length is not just a hidden finalize
  if NewLength=0 then begin
    {$ifndef NOVARIANTS} // faster clear of custom variant uniformous array
    if ArrayType=TypeInfo(TVariantDynArray) then begin
      ///VariantDynArrayClear(TVariantDynArray(fValue^));
      exit;
    end;
    {$endif}
    if GetIsObjArray then
      for i := 0 to Count-1 do
        PObjectArray(fValue^)^[i].Free;
    _DynArrayClear(fValue^,ArrayType);
    exit;
  end;
  // retrieve old length
  p := fValue^;
  if p<>nil then begin
    dec(PtrUInt(p),Sizeof(TDynArrayRec)); // p^ = start of heap object
    OldLength := p^.length;
  end else
    OldLength := 0;
  // calculate the needed size of the resulting memory structure on heap
  NeededSize := NewLength*ElemSize+Sizeof(TDynArrayRec);
  {$ifndef CPU64}
  if NeededSize>1024*1024*1024 then // max workable memory block is 1 GB
    raise ERangeError.CreateFmt('TDynArray SetLength(%s,%d) size concern',
      [PShortString(@PTypeInfo(ArrayType).NameLen)^,NewLength]);
  {$endif}
  // if not shared (refCnt=1), resize; if shared, create copy (not thread safe)
  if (p=nil) or (p^.refCnt=1) then begin
    if NewLength<OldLength then
      if ElemType<>nil then
        _FinalizeArray(pa+NeededSize,ElemType,OldLength-NewLength) else
        if GetIsObjArray then
          for i := NewLength to OldLength-1 do
            PObjectArray(fValue^)^[i].Free;
    ReallocMem(p,neededSize);
  end else begin
    InterlockedDecrement(PInteger(@p^.refCnt)^); // FPC has refCnt: PtrInt
    GetMem(p,neededSize);
    minLength := oldLength;
    if minLength>newLength then
      minLength := newLength;
    if ElemType<>nil then begin
      pp := pa+Sizeof(TDynArrayRec);
      System.FillChar(pp^,minLength*elemSize,0);
      CopyArray(pp,fValue^,ElemType,minLength)
    end else
      System.Move(fValue^,pa[Sizeof(TDynArrayRec)],minLength*elemSize);
  end;
  // set refCnt=1 and new length to the heap memory structure
  with p^ do begin
    refCnt := 1;
    {$ifdef FPC}
    high := newLength-1;
    {$else}
    length := newLength;
    {$endif}
  end;
  inc(PtrUInt(p),Sizeof(p^));
  // reset new allocated elements content to zero
  if NewLength>OldLength then begin
    OldLength := OldLength*elemSize;
    System.FillChar(pa[OldLength],neededSize-OldLength-Sizeof(TDynArrayRec),0);
  end;
  fValue^ := p;
end;

procedure TDynArray.SetCount(aCount: integer);
const MINIMUM_SIZE = 64;
var capa, delta: integer;
begin
  if fValue=nil then
    exit; // avoid GPF if void
  if fCountP<>nil then begin
    delta := aCount-fCountP^;
    if delta=0 then
      exit;
    fCountP^ := aCount;
    if PtrInt(fValue^)=0 then begin
      // no capa yet
      if (delta>0) and (aCount<MINIMUM_SIZE) then
        aCount := MINIMUM_SIZE; // reserve some minimal space for Add()
    end else begin
      {$ifdef FPC}
      capa := PDynArrayRec(PtrUInt(fValue^)-SizeOf(TDynArrayRec))^.length;
      {$else}
      capa := PInteger(PtrInt(fValue^)-sizeof(PtrInt))^;
      {$endif}
      if delta>0 then begin
        // size-up -> grow by chunks
        if capa>=fCountP^ then
          exit; // no need to grow
        inc(capa,capa shr 2);
        if capa<fCountP^ then
          aCount := fCountP^ else
          aCount := capa;
      end else
      if aCount>0 then // aCount=0 should release memory (e.g. TDynArray.Clear)
        // size-down -> only if worth it (for faster Delete)
        if (capa<=MINIMUM_SIZE) or (capa-aCount<capa shr 3) then
          exit;
    end;
  end;
  // no external Count, array size-down or array up-grow -> realloc
  InternalSetLength(aCount);
end;

function TDynArray.GetCapacity: integer;
begin // capacity := length(DynArray)
  if (fValue<>nil) and (PtrInt(fValue^)<>0) then
    result := PDynArrayRec(PtrUInt(fValue^)-SizeOf(TDynArrayRec))^.length else
    result := 0;
end;

procedure TDynArray.SetCapacity(aCapacity: integer);
begin
  if fValue=nil then
    exit; // avoid GPF if void
  if fCountP<>nil then
    if fCountP^>aCapacity then
      fCountP^ := aCapacity;
  InternalSetLength(aCapacity);
end;

procedure TDynArray.Slice(var Dest; aCount: Cardinal; aFirstIndex: cardinal=0);
var n: Cardinal;
    D: PPointer;
    P: PAnsiChar;
begin
  if fValue=nil then
    exit; // avoid GPF if void
  n := Count;
  if aFirstIndex>=n then
    aCount := 0 else
  if aCount>=n-aFirstIndex then
    aCount := n-aFirstIndex;
  DynArray(ArrayType,Dest).InternalSetLength(aCount);
  D := @Dest;
  if aCount>0 then begin
    P := PAnsiChar(fValue^)+aFirstIndex*ElemSize;
    if ElemType=nil then
      System.Move(P^,D^^,aCount*ElemSize) else
      CopyArray(D^,P,ElemType,aCount);
  end;
end;

procedure TDynArray.AddArray(const DynArrayVar; aStartIndex,aCount: integer);
var DynArrayCount, n: integer;
    PS,PD: pointer;
begin
  if fValue=nil then
    exit; // avoid GPF if void
  DynArrayCount := DynArrayLength(pointer(DynArrayVar));
  if aStartIndex>=DynArrayCount then
    exit; // nothing to copy
  if (aCount<0) or (cardinal(aStartIndex+aCount)>cardinal(DynArrayCount)) then
    aCount := DynArrayCount-aStartIndex;
  if aCount<=0 then
    exit;
  n := Count;
  SetCount(n+aCount);
  PS := pointer(PtrUInt(DynArrayVar)+cardinal(aStartIndex)*ElemSize);
  PD := pointer(PtrUInt(fValue^)+cardinal(n)*ElemSize);
  if ElemType=nil then
    System.Move(PS^,PD^,cardinal(aCount)*ElemSize) else
    CopyArray(PD,PS,ElemType,aCount);
end;

{$ifndef DELPHI5OROLDER} // don't know why Delphi 5 does not like this signature
procedure TDynArray.AddDynArray(const aSource: TDynArray; aStartIndex,aCount: integer);
var SourceCount: integer;
begin
  if (aSource.fValue<>nil) and (ArrayType=aSource.ArrayType) then begin
    SourceCount := aSource.Count;
    if (aCount<0) or (aCount>SourceCount) then
      aCount := SourceCount; // force use of external Source.Count, if any
    AddArray(aSource.fValue^,aStartIndex,aCount);
  end;
end;
{$endif DELPHI5OROLDER}

procedure TDynArray.ElemClear(var Elem);
begin
  if ElemType<>nil then
    case PTypeKind(ElemType)^ of // release reference counted
      tkLString{$ifdef FPC},tkLStringOld{$endif}:
        RawByteString(Elem) := '';
      tkWString:
        WideString(Elem) := '';
      tkInterface:
        IUnknown(Elem) := nil;
      {$ifdef HASVARUSTRING}
      tkUString:
        UnicodeString(Elem) := '';
      {$endif}
      //tkRecord{$ifdef FPC},tkObject{$endif}:
      //  RecordClear(Elem,ElemType);
      tkDynArray:
        _DynArrayClear(pointer(Elem),ElemType);
      {$ifndef NOVARIANTS}
      tkVariant:
        VarClear(Variant(Elem));
      {$endif}
      else exit;
    end;
  System.FillChar(Elem,ElemSize,0); // always fill with zero binary content
end;

procedure TDynArray.ElemCopy(const A; var B);
begin
  if ElemType=nil then begin
    System.Move(A,B,ElemSize);
    exit;
  end else begin
    case PTypeKind(ElemType)^ of
      tkLString{$ifdef FPC},tkLStringOld{$endif}: begin
        RawByteString(B) := RawByteString(A);
        exit;
      end;
      tkWString: begin
        WideString(B) := WideString(A);
        exit;
      end;
      tkInterface: begin
        IUnknown(B) := IUnknown(A);
        exit;
      end;
      {$ifdef HASVARUSTRING}
      tkUString: begin
        UnicodeString(B) := UnicodeString(A);
        exit;
      end;
      {$endif}
//      tkRecord{$ifdef FPC},tkObject{$endif}: begin
//        RecordCopy(B,A,ElemType);
//        exit;
//      end;
      {$ifndef NOVARIANTS}
      tkVariant: begin
        variant(B) := variant(A);
        exit;
      end;
      {$endif}
      else begin
        {$ifdef FPC}
        RecordClear(B,ElemType); // inlined CopyArray()
        System.Move(A,B,RTTIManagedSize(ElemType));
        RecordAddRef(B,ElemType);
        {$else}
        CopyArray(@B,@A,ElemType,1);
        {$endif}
        exit;
      end;
    end;
  end;
end;

function TDynArray.ElemLoad(Source: PAnsiChar): RawByteString;
begin
  if (Source<>nil) and (ElemType=nil) then
    SetString(result,Source,ElemSize) else begin
    SetString(result,nil,ElemSize);
    System.FillChar(pointer(result)^,ElemSize,0);
    ElemLoad(Source,pointer(result)^);
  end;
end;

procedure TDynArray.ElemLoad(Source: PAnsiChar; var Elem);
begin
  if Source=nil then
    exit; // avoid GPF
  if ElemType=nil then
    System.Move(Source^,Elem,ElemSize) else
    case PTypeKind(ElemType)^ of
    tkLString{$ifdef FPC},tkLStringOld{$endif}: begin
      SetString(RawByteString(Elem),Source+4,PInteger(Source)^);
      {$ifdef HASCODEPAGE}
      { Delphi 2009+: set Code page for this AnsiString }
      if PPtrUInt(@Elem)^<>0 then
        SetCodePage(RawByteString(Elem),PWord(PtrUInt(ElemType)+
          PTypeInfo(ElemType)^.NameLen+2)^,false);
      {$endif}
    end;
    tkWString: // WideString internal length is in bytes
      SetString(WideString(Elem),PWideChar(Source+4),PInteger(Source)^ shr 1);
    {$ifdef HASVARUSTRING}
    tkUString:
      SetString(UnicodeString(Elem),PWideChar(Source+4),PInteger(Source)^);
    {$endif}
    {$ifndef NOVARIANTS}
    ///tkVariant:
    ///  VariantLoad(variant(Elem),Source,@JSON_OPTIONS[true]);
    {$endif}
    ///tkRecord{$ifdef FPC},tkObject{$endif}:
    ///  RecordLoad(Elem,Source,ElemType);
    end;
end;

procedure TDynArray.ElemLoadClear(var ElemLoaded: RawByteString);
begin
  if (ElemType<>nil) and (length(ElemLoaded)=integer(ElemSize)) then
  case PTypeKind(ElemType)^ of
    tkLString{$ifdef FPC},tkLStringOld{$endif}:
      PRawByteString(pointer(ElemLoaded))^ := '';
    tkWString:
      PWideString(pointer(ElemLoaded))^ := '';
    {$ifdef HASVARUSTRING}
    tkUString:
      PUnicodeString(pointer(ElemLoaded))^ := '';
    {$endif}
    {$ifndef NOVARIANTS}
    tkVariant:
      VarClear(PVariant(pointer(ElemLoaded))^);
    {$endif}
    //tkRecord{$ifdef FPC},tkObject{$endif}:
    //  RecordClear(pointer(ElemLoaded)^,ElemType);
  end;
  ElemLoaded := '';
end;

function TDynArray.ElemLoadFind(Source: PAnsiChar): integer;
var tmp: RawByteString;
begin
  tmp := ElemLoad(Source);
  if tmp='' then
    result := -1 else
    try
        result := IndexOf(pointer(tmp)^)
    finally
      ElemLoadClear(tmp);
    end;
end;

function DynArray(aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil): TDynArray;
begin
  result.Init(aTypeInfo,aValue,aCountPointer);
end;

function IntegerScanExists(P: PCardinalArray; Count: PtrInt; Value: cardinal): boolean;
var i: PtrInt; // very optimized code for speed
begin
  if P<>nil then begin
    result := true;
    for i := 1 to (Count shr 2) do   // 4 DWORD by loop - aligned read
      if (P^[0]=Value) or (P^[1]=Value) or
         (P^[2]=Value) or (P^[3]=Value) then
        exit else
        inc(PtrUInt(P),sizeof(P^[0])*4);
    for i := 0 to (Count and 3)-1 do // last 0..3 DWORD
      if P^[i]=Value then
        exit;
  end;
  result := false;
end;

function PtrUIntScanExists(P: PPtrUIntArray; Count: PtrInt; Value: PtrUInt): boolean;
begin
  {$ifdef CPU64}
  result := Int64ScanExists(pointer(P),Count,Value);
  {$else}
  result := IntegerScanExists(pointer(P),Count,Value);
  {$endif}
end;



function PtrUIntScanIndex(P: PPtrUIntArray; Count: PtrInt; Value: PtrUInt): PtrInt;
var i: PtrInt; // optimized code for speed
begin
  if P<>nil then begin
    result := 0;
    for i := 1 to Count shr 2 do // 4 PtrUInt by loop - aligned read
      if P^[0]<>Value then
      if P^[1]<>Value then
      if P^[2]<>Value then
      if P^[3]<>Value then begin
        inc(PtrUInt(P),sizeof(P^[0])*4);
        inc(result,4);
      end else begin
        inc(result,3);
        exit;
      end else begin
        inc(result,2);
        exit;
      end else begin
        inc(result,1);
        exit;
      end else
        exit;
    for i := 0 to (Count and 3)-1 do // last 0..3 PtrUInt
      if P^[i]=Value then
        exit else
        inc(result);
  end;
  result := -1;
end;

function PtrArrayAdd(var aPtrArray; aItem: pointer): integer;
var a: TPointerDynArray absolute aPtrArray;
begin
  result := length(a);
  SetLength(a,result+1);
  a[result] := aItem;
end;

function PtrArrayDelete(var aPtrArray; aItem: pointer): integer;
var a: TPointerDynArray absolute aPtrArray;
    n: integer;
begin
  n := length(a);
  result := PtrUIntScanIndex(pointer(a),n,PtrUInt(aItem));
  if result<0 then
    exit;
  dec(n);
  if n>result then
    System.Move(a[result+1],a[result],(n-result)*sizeof(pointer));
  SetLength(a,n);
end;

{ wrapper functions to T*ObjArr types }

function ObjArrayAdd(var aObjArray; aItem: TObject): integer;
var a: TObjectDynArray absolute aObjArray;
begin
  result := length(a);
  SetLength(a,result+1);
  a[result] := aItem;
end;

procedure ObjArrayAddOnce(var aObjArray; aItem: TObject);
begin
  if not PtrUIntScanExists(pointer(aObjArray),
     length(TObjectDynArray(aObjArray)),PtrUInt(aItem)) then
    ObjArrayAdd(aObjArray,aItem);
end;

procedure ObjArraySetLength(var aObjArray; aLength: integer);
begin
  SetLength(TObjectDynArray(aObjArray),aLength);
end;

function ObjArrayFind(const aObjArray; aItem: TObject): integer;
begin
  result := PtrUIntScanIndex(pointer(aObjArray),
    length(TObjectDynArray(aObjArray)),PtrUInt(aItem));
end;

procedure ObjArrayDelete(var aObjArray; aItemIndex: integer;
  aContinueOnException: boolean);
var n: integer;
    a: TObjectDynArray absolute aObjArray;
begin
  n := length(a);
  if cardinal(aItemIndex)>=cardinal(n) then
    exit; // out of range
  if aContinueOnException then
    try
      a[aItemIndex].Free;
    except
    end else
    a[aItemIndex].Free;
  dec(n);
  if n>aItemIndex then
    System.Move(a[aItemIndex+1],a[aItemIndex],(n-aItemIndex)*sizeof(TObject));
  SetLength(a,n);
end;

function ObjArrayDelete(var aObjArray; aItem: TObject): integer;
begin
  result := ObjArrayFind(aObjArray,aItem);
  if result>=0 then
    ObjArrayDelete(aObjArray,result);
end;

//procedure ObjArraySort(var aObjArray; Compare: TDynArraySortCompare);
//var QuickSort: TDynArrayQuickSort;
//    n: integer;
//begin
//  n := length(TObjectDynArray(aObjArray));
//  if (@Compare<>nil) and (n>0) then begin
//    Quicksort.Compare := @Compare;
//    Quicksort.Value := pointer(aObjArray);
//    Quicksort.ElemSize := sizeof(pointer);
//    Quicksort.QuickSort(0,n-1);
//  end;
//end;

procedure ObjArrayClear(var aObjArray; aContinueOnException: boolean);
var i: integer;
    a: TObjectDynArray absolute aObjArray;
begin
  if a<>nil then begin
    if aContinueOnException then
      for i := 0 to length(a)-1 do
      try
        a[i].Free
      except
      end
    else
      for i := 0 to length(a)-1 do
        a[i].Free;
    a := nil;
  end;
end;

procedure ObjArrayObjArrayClear(var aObjArray);
var i: integer;
    a: TPointerDynArray absolute aObjArray;
begin
  if a<>nil then begin
    for i := 0 to length(a)-1 do
      ObjArrayClear(a[i]);
    a := nil;
  end;
end;

procedure ObjArraysClear(const aObjArray: array of pointer);
var i: integer;
begin
  for i := 0 to high(aObjArray) do
    if aObjArray[i]<>nil then
      ObjArrayClear(aObjArray[i]^);
end;

{$ifndef DELPHI5OROLDER}

function InterfaceArrayAdd(var aInterfaceArray; const aItem: IUnknown): integer;
var a: TInterfaceDynArray absolute aInterfaceArray;
begin
  result := length(a);
  SetLength(a,result+1);
  a[result] := aItem;
end;

procedure InterfaceArrayAddOnce(var aInterfaceArray; const aItem: IUnknown);
var a: TInterfaceDynArray absolute aInterfaceArray;
    n: integer;
begin
  if PtrUIntScanExists(pointer(aInterfaceArray),
     length(TInterfaceDynArray(aInterfaceArray)),PtrUInt(aItem)) then
    exit;
  n := length(a);
  SetLength(a,n+1);
  a[n] := aItem;
end;

function InterfaceArrayFind(const aInterfaceArray; const aItem: IUnknown): integer;
begin
  result := PtrUIntScanIndex(pointer(aInterfaceArray),
    length(TInterfaceDynArray(aInterfaceArray)),PtrUInt(aItem));
end;

procedure InterfaceArrayDelete(var aInterfaceArray; aItemIndex: integer);
var n: integer;
    a: TInterfaceDynArray absolute aInterfaceArray;
begin
  n := length(a);
  if cardinal(aItemIndex)>=cardinal(n) then
    exit; // out of range
  a[aItemIndex] := nil;
  dec(n);
  if n>aItemIndex then
    System.Move(a[aItemIndex+1],a[aItemIndex],(n-aItemIndex)*sizeof(IInterface));
  TPointerDynArray(aInterfaceArray)[n] := nil; // avoid GPF in SetLength()
  SetLength(a,n);
end;

function InterfaceArrayDelete(var aInterfaceArray; const aItem: IUnknown): integer;
begin
  result := InterfaceArrayFind(aInterfaceArray,aItem);
  if result>=0 then
    InterfaceArrayDelete(aInterfaceArray,result);
end;

{$endif DELPHI5OROLDER}




function GetDelphiCompilerVersion: RawUTF8;
begin
  result :=
{$ifdef FPC}
  'Free Pascal'
  {$ifdef VER2_6_4}+' 2.6.4'{$endif}
  {$ifdef VER3_0_0}+' 3.0.0'{$endif}
  {$ifdef VER3_0_1}+' 3.0.1'{$endif}
  {$ifdef VER3_0_2}+' 3.0.2'{$endif}
  {$ifdef VER3_1_1}+' 3.1.1'{$endif}
    {$ifdef FPC_HAS_EXTENDEDINTERFACERTTI}+' ERTTI'{$endif}
    {$ifdef FPC_HAS_MANAGEMENT_OPERATORS}+' MOP'{$endif}
{$else}
  {$ifdef VER130} 'Delphi 5'{$endif}
  {$ifdef CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
    {$if     defined(KYLIX3)}'Kylix 3'
    {$elseif defined(VER140)}'Delphi 6'
    {$elseif defined(VER150)}'Delphi 7'
    {$elseif defined(VER160)}'Delphi 8'
    {$elseif defined(VER170)}'Delphi 2005'
    {$elseif defined(VER185)}'Delphi 2007'
    {$elseif defined(VER180)}'Delphi 2006'
    {$elseif defined(VER200)}'Delphi 2009'
    {$elseif defined(VER210)}'Delphi 2010'
    {$elseif defined(VER220)}'Delphi XE'
    {$elseif defined(VER230)}'Delphi XE2'
    {$elseif defined(VER240)}'Delphi XE3'
    {$elseif defined(VER250)}'Delphi XE4'
    {$elseif defined(VER260)}'Delphi XE5'
    {$elseif defined(VER265)}'AppMethod 1'
    {$elseif defined(VER270)}'Delphi XE6'
    {$elseif defined(VER280)}'Delphi XE7'
    {$elseif defined(VER290)}'Delphi XE8'
    {$elseif defined(VER300)}'Delphi 10 Seattle'
    {$elseif defined(VER310)}'Delphi 10.1 Berlin'
    {$elseif defined(VER320)}'Delphi 10.2 Tokyo'
    {$elseif defined(VER320)}'Delphi 10.3 Carnival'
    {$ifend}
  {$endif CONDITIONALEXPRESSIONS}
{$endif}
{$ifdef CPU64}
  +' 64 bit'
{$endif}
end;





{ TSynMemoryStream }

constructor TSynMemoryStream.Create(const aText: RawByteString);
begin
  inherited Create;
  SetPointer(pointer(aText),length(aText));
end;

constructor TSynMemoryStream.Create(Data: pointer; DataLen: integer);
begin
  inherited Create;
  SetPointer(Data,DataLen);
end;

function TSynMemoryStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise EStreamError.Create('TSynMemoryStream.Write');
end;




{ TSynTable }


{$ifndef SORTCOMPAREMETHOD}

function SortU8(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PByte(P1)^-PByte(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortU16(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PWord(P1)^-PWord(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortI32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PInteger(P1)^-PInteger(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortI64(P1,P2: PUTF8Char): PtrInt;
var V: Int64;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V := PInt64(P1)^-PInt64(P2)^;
        if V<0 then
          result := -1 else
        if V>0 then
          result := 1 else
          result := 0;
       end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortDouble(P1,P2: PUTF8Char): PtrInt;
var V: Double;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V := PDouble(P1)^-PDouble(P2)^;
        if V<0 then
          result := -1 else
        if V=0 then
          result := 0 else
          result := 1;
       end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortU24(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PtrInt(PWord(P1)^)+PtrInt(P1[2])shl 16
          -PtrInt(PWord(P2)^)-PtrInt(P2[2]) shl 16;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := FromVarUInt32(PByte(P1))-FromVarUInt32(PByte(P2));
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := FromVarInt32(PByte(P1))-FromVarInt32(PByte(P2));
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := FromVarUInt64(PByte(P1))-FromVarUInt64(PByte(P2)) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := FromVarInt64(PByte(P1))-FromVarInt64(PByte(P2)) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortStr(P1,P2: PUTF8Char): PtrInt;
var L1, L2, L, i: PtrInt;
    PB1, PB2: PByte;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        if PtrInt(P1^)<=$7F then begin
          L1 := PtrInt(P1^);
          inc(P1);
        end else begin
          PB1 := pointer(P1);
          L1 := FromVarUInt32High(PB1);
          P1 := pointer(PB1);
        end;
        if PtrInt(P2^)<=$7F then begin
          L2 := PtrInt(P2^);
          inc(P2);
        end else begin
          PB2 := pointer(P2);
          L2 := FromVarUInt32High(PB2);
          P2 := pointer(PB2);
        end;
        L := L1;
        if L2>L then
          L := L2;
        for i := 0 to L-1 do begin
          result := PtrInt(P1[i])-PtrInt(P2[i]);
          if Result<>0 then
            exit;
        end;
        result := L1-L2;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortIStr(P1,P2: PUTF8Char): PtrInt;
var L1, L2, L, i: PtrInt;
    PB1, PB2: PByte;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        if PtrInt(P1^)<=$7F then begin
          L1 := PtrInt(P1^);
          inc(P1);
        end else begin
          PB1 := pointer(P1);
          L1 := FromVarUInt32High(PB1);
          P1 := pointer(PB1);
        end;
        if PtrInt(P2^)<=$7F then begin
          L2 := PtrInt(P2^);
          inc(P2);
        end else begin
          PB2 := pointer(P2);
          L2 := FromVarUInt32High(PB2);
          P2 := pointer(PB2);
        end;
        if L2>L1 then
          L := L2 else
          L := L1;
        for i := 0 to L-1 do // NormToUpperAnsi7 works for both WinAnsi & UTF-8
          if NormToUpperAnsi7[P1[i]]<>NormToUpperAnsi7[P2[i]] then begin
            result := PtrInt(P1[i])-PtrInt(P2[i]);
            exit;
          end;
        result := L1-L2;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

const
  FIELD_SORT: array[TSynTableFieldType] of TUTF8Compare = (
    nil, // tftUnknown,
    SortU8,    SortU8,  SortU16,  SortU24,  SortI32, SortI64,
 // tftBoolean,tftUInt8,tftUInt16,tftUInt24,tftInt32,tftInt64,
    SortI64,  SortDouble, SortVarUInt32,SortVarInt32,SortVarUInt64,
 // tftCurrency,tftDouble, tftVarUInt32, tftVarInt32,tftVarUInt64,
    SortStr,   SortStr, SortStr,        nil,           SortVarInt64);
 // tftWinAnsi,tftUTF8, tftBlobInternal,tftBlobExternal,tftVarInt64);

{$endif SORTCOMPAREMETHOD}


function PropNameValid(P: PUTF8Char): boolean;
begin
  result := false;
  if (P=nil) or not (P^ in ['a'..'z','A'..'Z','_']) then
    exit; // first char must be alphabetical
  inc(P);
  while P^<>#0 do
    if not (ord(P^) in IsIdentifier) then
      exit else // following chars can be alphanumerical
      inc(P);
  result := true;
end;



{$ifdef FPC}
function BooleanNormalize(value: boolean): integer; inline;
begin
  if value then
    result := 1 else
    result := 0;
end;
{$endif}

{ TRawByteStringStream }

constructor TRawByteStringStream.Create(const aString: RawByteString);
begin
  fDataString := aString;
end;

function TRawByteStringStream.Read(var Buffer; Count: Integer): Longint;
begin
  if Count<=0 then
    Result := 0 else begin
    Result := Length(fDataString)-fPosition;
    if Result>Count then
      Result := Count;
    System.Move(PByteArray(fDataString)[fPosition],Buffer,Result);
    inc(fPosition, Result);
  end;
end;

function TRawByteStringStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: fPosition := Offset;
    soFromCurrent:   fPosition := fPosition+Offset;
    soFromEnd:       fPosition := Length(fDataString)-Offset;
  end;
  if fPosition>Length(fDataString) then
    fPosition := Length(fDataString) else
    if fPosition<0 then
      fPosition := 0;
  result := fPosition;
end;

procedure TRawByteStringStream.SetSize(NewSize: Integer);
begin
  SetLength(fDataString, NewSize);
  if fPosition>NewSize then
    fPosition := NewSize;
end;

function TRawByteStringStream.Write(const Buffer; Count: Integer): Longint;
begin
  if Count<=0 then
    Result := 0 else begin
    Result := Count;
    SetLength(fDataString,(fPosition+Result));
    System.Move(Buffer,PByteArray(fDataString)[fPosition],Result);
    inc(FPosition,Result);
  end;
end;



end.
