unit LeakTestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TTestLeakForm = class(TForm)
    LeakDelphiMemButton: TButton;
    LeakOsMemButton: TButton;
    LeakKernelHandlesButton: TButton;
    LeakGdiHandlesButton: TButton;
    LeakUserHandlesButton: TButton;
    LeakOtherStuffButton: TButton;
    procedure LeakDelphiMemButtonClick(Sender: TObject);
    procedure LeakOsMemButtonClick(Sender: TObject);
    procedure LeakKernelHandlesButtonClick(Sender: TObject);
    procedure LeakGdiHandlesButtonClick(Sender: TObject);
    procedure LeakUserHandlesButtonClick(Sender: TObject);
    procedure LeakOtherStuffButtonClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  TestLeakForm: TTestLeakForm;

implementation

{$R *.dfm}

uses ActiveX, madTypes, CommCtrl, WinSpool;

type
  TSomeClass = class (TInterfacedObject, IUnknown)
  private
    FAnsiStr : AnsiString;
    FWideStr : WideString;
    constructor Create;
  end;

constructor TSomeClass.Create;
begin
  inherited;
  FAnsiStr := 'someAnsi';
  FWideStr := 'someWide';
end;

procedure TTestLeakForm.LeakDelphiMemButtonClick(Sender: TObject);
var FAnsiStr : AnsiString;
    FWideStr : WideString;
    FDynArr  : TDAInteger;
begin
  inherited;
  AllocMem(777);
  FAnsiStr := 'someAnsi';
  UniqueString(FAnsiStr);
  FWideStr := 'someWide';
  SetLength(FDynArr, 5);
  pointer(pointer(@FAnsiStr)^) := nil;
  pointer(pointer(@FWideStr)^) := nil;
  pointer(pointer(@FDynArr )^) := nil;
  TSomeClass.Create._AddRef;
  Exception.Create('someException');
end;

procedure TTestLeakForm.LeakOsMemButtonClick(Sender: TObject);
var malloc : IMalloc;
begin
  HeapCreate(0, 0, 0);
  CoGetMalloc(1, malloc);
  malloc.Alloc(77);
  VirtualAlloc(nil, 100, MEM_COMMIT, PAGE_EXECUTE_READ);
  VirtualAlloc(nil, 100, MEM_RESERVE, PAGE_NOACCESS);
  GlobalAlloc(LPTR, 100);
  GlobalAlloc(LMEM_MOVEABLE or LMEM_ZEROINIT, 100);
  CoTaskMemAlloc(24);
end;

procedure TTestLeakForm.LeakKernelHandlesButtonClick(Sender: TObject);
var arrCh : array [0..MAX_PATH] of WideChar;
    h1    : THandle;
    hk1   : HKEY;
    c1    : cardinal;
begin
  OpenProcess(PROCESS_ALL_ACCESS, false, GetCurrentProcessId);
  DuplicateHandle(GetCurrentProcess, GetCurrentThread, GetCurrentProcess, @h1, 0, false, DUPLICATE_SAME_ACCESS);
  OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, h1);
  CreateFile(pchar(ParamStr(0)), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  CreateFileMapping(THandle(-1), nil, PAGE_READWRITE, 0, 100, 'someSection');
  RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\Microsoft', 0, KEY_READ, hk1);
  CreateEvent(nil, true, true, 'someEvent');
  CreateMutex(nil, true, 'someMutex');
  CreateSemaphore(nil, 1, 10, 'someSemaphore');
  CreateWaitableTimer(nil, true, 'someTimer');
  CreateNamedPipe('\\.\pipe\somePipe', PIPE_ACCESS_INBOUND, 0, 1, 100, 100, 1000, nil);
  OpenInputDesktop(0, false, GENERIC_READ);
  if GetUserObjectInformationW(GetProcessWindowStation, 2, @arrCh, MAX_PATH * 2, c1) then
    OpenWindowStationW(arrCh, false, GENERIC_READ);
end;

procedure TTestLeakForm.LeakGdiHandlesButtonClick(Sender: TObject);
begin
  CreateFontW(-11, 0, 0, 0, 400, 0, 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH or FF_DONTCARE, 'Tahoma');
  CreatePen(PS_SOLID, 1, $203040);
  CreateSolidBrush($102030);
  CreateEllipticRgn(100, 100, 200, 200);
  CreateBitmap(100, 100, 1, 32, nil);
  GetDC(GetDesktopWindow);
end;

procedure TTestLeakForm.LeakUserHandlesButtonClick(Sender: TObject);
begin
  CreateIcon(0, 16, 16, 1, 24, Self, Self);
  CreateWindow('static', 'someWindow', 0, 100, 100, 200, 200, 0, 0, HInstance, nil);
  CreateMenu;
end;

function GetDefaultPrinterW(buffer: PWideChar; var bufLen: dword) : bool; stdcall; external 'winspool.drv';

procedure TTestLeakForm.LeakOtherStuffButtonClick(Sender: TObject);
var h1    : THandle;
    wfd   : TWin32FindData;
    arrCh : array [0..MAX_PATH] of WideChar;
    len   : dword;
begin
  ImageList_Create(16, 16, 0, 1, 1);
  len := MAX_PATH;
  GetDefaultPrinterW(arrCh, len);
  OpenPrinterW(arrCh, h1, nil);
  FindFirstFile('c:\*.*', wfd);
  WNetOpenEnum(RESOURCE_GLOBALNET, RESOURCETYPE_DISK, 0, nil, h1);
end;

end.
