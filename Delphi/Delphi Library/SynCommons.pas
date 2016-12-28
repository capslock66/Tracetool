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
{$if CompilerVersion = 20} // DELPHI 2009
  PtrUInt = cardinal; { see http://synopse.info/forum/viewtopic.php?id=136 }
{$else}
  {$ifdef UNICODE}
  PtrUInt = NativeUInt;
  {$else}
  PtrUInt = cardinal;
  {$endif}
{$ifend}

/// a CPU-dependent unsigned integer type cast of a pointer of pointer
// - used for 64 bits compatibility, native under Free Pascal Compiler
PPtrUInt = ^PtrUInt;

/// a CPU-dependent signed integer type cast of a pointer / register
// - used for 64 bits compatibility, native under Free Pascal Compiler
{$if CompilerVersion = 20} // DELPHI 2009
  PtrInt = integer;
{$else}
  {$ifdef UNICODE}
  PtrInt = NativeInt;
  {$else}
  PtrInt = integer;
  {$endif}
{$ifend}

/// a CPU-dependent signed integer type cast of a pointer of pointer
// - used for 64 bits compatibility, native under Free Pascal Compiler
PPtrInt = ^PtrInt;

  /// unsigned Int64 doesn't exist under older Delphi
  // - and UInt64 is buggy as hell under Delphi 2007 when inlining functions

{$ifndef CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
// some definitions not available prior to Delphi 7
type
  UInt64 = Int64;

const
  varShortInt = $0010;
  varInt64 = $0014; { vt_i8 }

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




{ ************ low-level RTTI types and conversion routines ***************** }

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
    function GetCount: integer; inline;
    function GetCapacity: integer;
    function GetArrayTypeName: AnsiString;
    /// will set fKnownType and fKnownOffset/fKnownSize fields
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
    /// the known RTTI information of the whole array
    property ArrayType: pointer read fTypeInfo;
    /// the known type name of the whole array
    property ArrayTypeName: AnsiString read GetArrayTypeName;
    /// the internal in-memory size of one element, as retrieved from RTTI
    property ElemSize: PtrUInt read fElemSize;
    /// the internal type information of one element, as retrieved from RTTI
    property ElemType: pointer read fElemType;
  end;

/// retrieve the type name from its low-level RTTI
procedure TypeInfoToName(aTypeInfo: pointer; var result: AnsiString;  const default: AnsiString=''); overload;

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
  {$if CompilerVersion >= 21.0}  // DELPHI 2010
  /// map the Delphi record field enhanced RTTI (available since Delphi 2010)
  TEnhancedFieldInfo = packed record
    TypeInfo: PTypeInfoStored;
    Offset: PtrUInt;
    Flags: Byte;
    NameLen: byte; // = Name[0] = length(Name)
  end;
  PEnhancedFieldInfo = ^TEnhancedFieldInfo;
  {$ifend}

  TTypeInfo = packed record
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
      {$if CompilerVersion >= 21.0} // DELPHI 2010 // enhanced RTTI containing info about all fields
      NumOps: Byte;
      //RecOps: array[0..0] of Pointer;
      AllCount: Integer; // !!!! may need $RTTI EXPLICIT FIELDS([vcPublic])
      AllFields: array[0..0] of TEnhancedFieldInfo;
      {$ifend}
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

procedure SetRawUTF8(var Dest: AnsiString; text: pointer; len: integer);
var P: PStrRec;
begin
  if (len>128) or (len=0) or (PtrInt(Dest)=0) or
    (PStrRec(PtrInt(Dest)-STRRECSIZE)^.refCnt<>1) then
    SetString(Dest,PAnsiChar(text),len)
  else begin
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


procedure TypeInfoToName(aTypeInfo: pointer; var result: AnsiString;  const default: AnsiString='');
begin
  if aTypeInfo<>nil then
    SetRawUTF8(result,PAnsiChar(@PTypeInfo(aTypeInfo)^.NameLen)+1,  PTypeInfo(aTypeInfo)^.NameLen)
  else
    result := default;
end;

{ ****************** TDynArray wrapper }

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
end;

function TDynArray.GetArrayTypeName: AnsiString;
begin
  TypeInfoToName(fTypeInfo,result);
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

function TDynArray.GetCapacity: integer;
begin // capacity := length(DynArray)
  if (fValue<>nil) and (PtrInt(fValue^)<>0) then
    result := PDynArrayRec(PtrUInt(fValue^)-SizeOf(TDynArrayRec))^.length else
    result := 0;
end;



end.
