unit ecCrashParam;

// the application decides whether the dll should crash or not
// the initialization crash is initiated through a simple command line parameter
// however, the dll finalization crash can't be initiated that way
// so we use a memory mapped file to share a common buffer between dll and exe

interface

// which crash param do we have?
var CrashParam : pchar = nil;

// set the current crash param
procedure SetCrashParam (value: pchar);

implementation

uses Windows, madStrings;

procedure SetCrashParam(value: pchar);
// set the crash param
begin
  Move(value^, CrashParam^, (lstrlen(value) + 1) * sizeOf(char));
end;

procedure InitCrashParam;
var map : THandle;
    new : boolean;
begin
  SetLastError(0);
  // first create/open a named memory mapped file buffer for the crash param
  map := CreateFileMappingA(THandle(-1), nil, PAGE_READWRITE, 0, 4096,
                            PAnsiChar('madCrashParam' + IntToHexExA(GetCurrentProcessID)));
  // did the named buffer already exist or is it new?
  new := GetLastError = 0;
  // map the buffer into our memory context
  CrashParam := MapViewOfFile(map, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if new then
    // the buffer was new, so let's fill it with the command line parameters
    SetCrashParam(pchar(ParamStr(1)));
end;

initialization
  InitCrashParam;
end.
