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

{$ifndef ISDELPHI2007ANDUP}
type
  TBytes = array of byte;
{$endif}

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
    function GetCapacity: integer;
    function GetArrayTypeName: RawUTF8;
    /// will set fKnownType and fKnownOffset/fKnownSize fields
    function ToKnownType(exactType: boolean=false): TDynArrayKind;
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

    /// returns a pointer to an element of the array
    // - returns nil if aIndex is out of range
    // - since TDynArray is just a wrapper around an existing array, you should
    // better use direct access to its wrapped variable, and not using this
    // slower and more error prone method (such pointer access lacks of strong
    // typing abilities), which was designed for TDynArray internal use
    function ElemPtr(aIndex: integer): pointer;

    /// retrieve or set the number of elements of the dynamic array
    // - same as length(DynArray) or SetLenght(DynArray)
    // - this property will recognize T*ObjArray types, so will free any stored
    // instance if the array is sized down
    property Count: integer read GetCount ;
    /// the internal buffer capacity
    // - if no external Count pointer was set with Init, is the same as Count
    // - if an external Count pointer is set, you can set a value to this
    // property before a massive use of the Add() method e.g.
    // - if no external Count pointer is set, set a value to this property
    // will affect the Count value, i.e. Add() will append after this count
    // - this property will recognize T*ObjArray types, so will free any stored
    // instance if the array is sized down
    property Capacity: integer read GetCapacity ;
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



function TDynArray.GetCapacity: integer;
begin // capacity := length(DynArray)
  if (fValue<>nil) and (PtrInt(fValue^)<>0) then
    result := PDynArrayRec(PtrUInt(fValue^)-SizeOf(TDynArrayRec))^.length else
    result := 0;
end;

function DynArray(aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil): TDynArray;
begin
  result.Init(aTypeInfo,aValue,aCountPointer);
end;







end.
