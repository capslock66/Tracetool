/// common functions used by most Synopse projects
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCommons;

{$I Synopse.inc}

interface

uses
  Windows,
  Messages,
  Classes,
  Types,
  Variants,
  SysUtils;


{ ************ common types used for compatibility between compilers and CPU }

const
  CP_UTF16 = 1200;
  CP_UTF8 = 65001;

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

  /// unsigned Int64 doesn't exist under older Delphi
  // - and UInt64 is buggy as hell under Delphi 2007 when inlining functions

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

  /// a variant values array
  TVariantArray = array[0..MaxInt div SizeOf(Variant)-1] of Variant;
  /// a pointer to a variant array
  PVariantArray = ^TVariantArray;

  /// a dynamic array of variant values
  TVariantDynArray = array of variant;


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

  RawUnicode = type AnsiString(CP_UTF16); // Codepage for an UnicodeString

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
  // - is used also by TDynArray.InitSpecific() to define the main field type
  TDynArrayKind = (
    djNone,
    djBoolean, djByte, djWord, djInteger, djCardinal, djSingle,
    djInt64, djDouble, djCurrency,
    djTimeLog, djDateTime, djRawUTF8, djWinAnsi, djString, djRawByteString,
    djWideString, djSynUnicode, djInterface,
    djVariant,
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
  TDynArray = record
  private
    fValue: PPointer;
    fTypeInfo: pointer;
    fElemSize: PtrUInt;
    fElemType: pointer;
    fCountP: PInteger;
    fKnownSize: integer;
    fKnownType: TDynArrayKind;
    function GetCount: integer; inline;
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
    procedure InitSpecific(aTypeInfo: pointer; var aValue; aKind: TDynArrayKind; aCountPointer: PInteger=nil; aCaseInsensitive: boolean=false);
    /// define the reference to an external count integer variable
    // - Init and InitSpecific methods will reset the aCountPointer to 0: you
    // can use this method to set the external count variable without overriding
    // the current value
    procedure UseExternalCount(var aCountPointer: Integer);  inline;

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



/// retrieve the type name from its low-level RTTI
function TypeInfoToName(aTypeInfo: pointer): RawUTF8; overload; inline;

/// retrieve the type name from its low-level RTTI
procedure TypeInfoToName(aTypeInfo: pointer; var result: RawUTF8;  const default: RawUTF8=''); overload;

/// retrieve the unit name and type name from its low-level RTTI
procedure TypeInfoToQualifiedName(aTypeInfo: pointer; var result: RawUTF8; const default: RawUTF8='');

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
function DynArray(aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil): TDynArray; inline;

implementation

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


type
  TOrdType = (otSByte,otUByte,otSWord,otUWord,otSLong,otULong);
  TFloatType = (ftSingle,ftDoub,ftExtended,ftComp,ftCurr);
  TTypeKinds = set of TTypeKind;
  PTypeKind = ^TTypeKind;

  PStrRec = ^TStrRec;
  /// map the Delphi string header, as defined in System.pas
  TStrRec =
    record
  {$ifdef UNICODE}
    {$ifdef CPU64}
    /// padding bytes for 16 byte alignment of the header
    _Padding: LongInt;
    {$endif}
    /// the associated code page used for this string
    // - exist only since Delphi 2009
    // - 0 or 65535 for RawByteString
    // - 1200=CP_UTF16 for UnicodeString
    // - 65001=CP_UTF8 for RawUTF8
    // - the current code page for AnsiString
    codePage: Word;
    /// either 1 (for AnsiString) or 2 (for UnicodeString)
    // - exist only since Delphi 2009
    elemSize: Word;
  {$endif UNICODE}
    /// COW string reference count (basic garbage memory mechanism)
    refCnt: Longint;
    /// length in characters
    // - size in bytes = length*elemSize
    length: Longint;
  end;

  /// map the Delphi dynamic array header (stored before each instance)
  TDynArrayRec = packed record
    /// dynamic array reference count (basic garbage memory mechanism)
    {$ifdef CPUX64}
    _Padding: LongInt; // Delphi XE2+ expects 16 byte alignment
    {$endif}
    refCnt: Longint;
    /// length in element count
    // - size in bytes = length*ElemSize
    length: PtrInt;
  end;
  PDynArrayRec = ^TDynArrayRec;

  PTypeInfo = ^TTypeInfo;
  {$ifdef HASDIRECTTYPEINFO}
  PTypeInfoStored = PTypeInfo;
  {$else}
  PTypeInfoStored = ^PTypeInfo;
  {$endif}

  /// map the Delphi record field RTTI
  TFieldInfo =
    record
    TypeInfo: PTypeInfoStored;
    Offset: PtrUInt;
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

  TTypeInfo =
    packed
    record
    kind: TTypeKind;
    NameLen: byte;
    case TTypeKind of
    tkUnknown: (
      NameFirst: AnsiChar;
    );
    tkDynArray: (
      // storage byte count for this field
      elSize: Longint;
      // nil for unmanaged field
      elType: PTypeInfoStored;
      // OleAuto compatible type
      varType: Integer;
      // also unmanaged field
      elType2: PTypeInfoStored;
    );
    tkArray: (
      arraySize: Integer;
      // product of lengths of all dimensions
      elCount: Integer;
      arrayType: PTypeInfoStored;
      dimCount: Byte;
      dims: array[0..255 {DimCount-1}] of PTypeInfoStored;
    );
    tkRecord: (
      recSize: cardinal;
      ManagedCount: integer;
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
      MinValue: longint;
      MaxValue: longint;
      EnumBaseType: PTypeInfoStored;
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
    NameLen: byte;
  end;
  PPropInfo = ^TPropInfo;

const
  /// codePage offset = string header size
  // - used to calc the beginning of memory allocation of a string
  STRRECSIZE = SizeOf(TStrRec);

function Deref(Info: PTypeInfoStored): PTypeInfo; inline;
begin
  if Info=nil then
    result := pointer(Info) else
    result := Info^;
end;

var
  KnownTypeInfo: array of PTypeInfo;

function DynArrayLength(Value: Pointer): integer; inline;
begin
  if Value=nil then
    result := PtrInt(Value) else begin
    result := PInteger(PtrUInt(Value)-sizeof(PtrInt))^;
  end;
end;

function GetTypeInfo(aTypeInfo: pointer; aExpectedKind: TTypeKind): PTypeInfo; overload; inline;
begin
  if (aTypeInfo<>nil) and (PTypeKind(aTypeInfo)^=aExpectedKind) then begin
    result := aTypeInfo;
    inc(PtrUInt(result),result^.NameLen);
  end else
    result := nil;
end;


function GetTypeInfo(aTypeInfo: pointer; const aExpectedKind: TTypeKinds): PTypeInfo; overload; inline;
begin
  result := aTypeInfo;
  if (result<>nil) and (result^.Kind in aExpectedKind) then
    inc(PtrUInt(result),result^.NameLen)
  else
    result := nil;
end;


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

procedure TypeInfoToName(aTypeInfo: pointer; var result: RawUTF8;  const default: RawUTF8='');
begin
  if aTypeInfo<>nil then
    SetRawUTF8(result,PAnsiChar(@PTypeInfo(aTypeInfo)^.NameLen)+1,  PTypeInfo(aTypeInfo)^.NameLen) 
  else
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

{ ****************** TDynArray wrapper }

function DynArrayElementTypeName(TypeInfo: pointer; ElemTypeInfo: PPointer): RawUTF8;
var DynArray: TDynArray;
    VoidArray: pointer;
const KNOWNTYPE_ITEMNAME: array[TDynArrayKind] of RawUTF8 = ('',
  'boolean','byte','word','integer','cardinal','single','Int64','double','currency',
  'TTimeLog','TDateTime','RawUTF8','WinAnsiString','string','RawByteString',
  'WideString','SynUnicode','IInterface','variant','');
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
    if cardinal(aIndex)>=PCardinal(PtrUInt(fValue^)-sizeof(PtrInt))^ then
      exit;
  result := pointer(PtrUInt(fValue^)+PtrUInt(aIndex)*ElemSize);
end;

function TDynArray.GetCount: integer;
begin
  if fValue<>nil then
    if fCountP=nil then
      if PtrInt(fValue^)<>0 then begin
        result := PInteger(PtrUInt(fValue^)-sizeof(PtrInt))^;
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
    sizeof(Variant), 0);

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
      tkLString: fKnownType := djRawUTF8;
      tkWString: fKnownType := djWideString;
      {$ifdef UNICODE}
      tkUString: fKnownType := djString;
      {$endif}
      tkVariant: fKnownType := djVariant;
      tkInterface: fKnownType := djInterface;
      tkRecord: begin
        nested := ElemType; // inlined GetTypeInfo()
rec:    inc(PtrUInt(nested),(nested^.NameLen));
        if nested^.ManagedCount=0 then // only binary content -> full content
          goto Bin;
        with nested^.ManagedFields[0] do
        case Offset of
        0: case TypeInfo^.Kind of
            tkLString: fKnownType := djRawUTF8;
            tkWString: fKnownType := djWideString;
            {$ifdef UNICODE}
            tkUString: fKnownType := djString;
            {$endif}
            tkRecord: begin
              nested := Deref(TypeInfo);
              goto Rec;
            end;
            tkVariant: fKnownType := djVariant;
            else begin
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
  inc(PtrUInt(aTypeInfo),PTypeInfo(aTypeInfo)^.NameLen);
  fElemSize := PTypeInfo(aTypeInfo)^.elSize ;
  fElemType := PTypeInfo(aTypeInfo)^.elType;
  if fElemType<>nil then begin
     fElemType := PPointer(fElemType)^;
  end;
  fCountP := aCountPointer;
  if fCountP<>nil then
    fCountP^ := 0;
  fKnownSize := 0;
  fKnownType := djNone;
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

asm
{$ifdef CPU64}
  .NOFRAME
{$endif}
  jmp System.@DynArrayClear
end;


procedure _FinalizeArray(p: Pointer; typeInfo: Pointer; elemCount: PtrUInt);

asm
{$ifdef CPU64}
  .NOFRAME
{$endif}
  jmp System.@FinalizeArray
end;

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
