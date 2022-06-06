unit ecMain;

// handles the GUI stuff, also raises most of the exception types

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls;

type
  TFMainForm = class(TForm)
    EarlyUnitInitializationBtn: TSpeedButton;
    VclUnitInitializationBtn: TSpeedButton;
    ProjectInitializationBtn: TSpeedButton;
    Shape2: TShape;
    Bevel6: TBevel;
    Label6: TLabel;
    CreateThreadBtn: TSpeedButton;
    BeginThreadBtn: TSpeedButton;
    TThreadBtn: TSpeedButton;
    Shape1: TShape;
    Bevel1: TBevel;
    Label1: TLabel;
    SysUtilsShowExceptionBtn: TSpeedButton;
    ApplicationShowExceptionBtn: TSpeedButton;
    ApplicationHandleExceptionBtn: TSpeedButton;
    Shape3: TShape;
    Bevel2: TBevel;
    Label2: TLabel;
    DllInitializationStaticBtn: TSpeedButton;
    DllExportedFunctionBtn: TSpeedButton;
    Shape4: TShape;
    Bevel3: TBevel;
    DuringRunningApplicationBtn: TSpeedButton;
    VclUnitFinalizationBtn: TSpeedButton;
    LateUnitFinalizationBtn: TSpeedButton;
    DllFinalizationStaticBtn: TSpeedButton;
    ExceptionFloodingBtn: TSpeedButton;
    InfiniteLoopBtn: TSpeedButton;
    Shape6: TShape;
    Bevel5: TBevel;
    Label5: TLabel;
    BackShape1: TShape;
    BackShape2: TShape;
    BackShape3: TShape;
    BackShape4: TShape;
    BackShape6: TShape;
    DeadLockBtn: TSpeedButton;
    ProjectFinalizationBtn: TSpeedButton;
    Label3: TLabel;
    SourceCode: TRichEdit;
    Explanation: TRichEdit;
    TitleLabel: TLabel;
    Shape5: TShape;
    ActionBtn: TButton;
    Shape7: TShape;
    Bevel4: TBevel;
    Shape9: TShape;
    Shape8: TShape;
    procedure ChooseDemo(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
    procedure FlooderTimer (var Message: TMessage); message WM_TIMER;
    function  FindDownButton : TSpeedButton;
  public
    { Public-Deklarationen }
    procedure DemoEarlyUnitInitialization;
    procedure DemoVclUnitInitialization;
    procedure DemoProjectInitialization;
    procedure DemoDuringRunningApplication;
    procedure DemoProjectFinalization;
    procedure DemoVclUnitFinalization;
    procedure DemoLateUnitFinalization;
    procedure DemoCreateThread;
    procedure DemoBeginThread;
    procedure DemoTThread;
    procedure DemoDllInitialization;
    procedure DemoDllExportedFunction;
    procedure DemoDllFinalization;
    procedure DemoSysUtilsShowException;
    procedure DemoApplicationShowException;
    procedure DemoApplicationHandleException;
    procedure DemoExceptionFlooding;
    procedure DemoInfiniteLoop;
    procedure DemoDeadlock;
  end;

var
  FMainForm: TFMainForm;

implementation

{$R *.dfm}

uses ecCrashParam, ecStrings, madExcept;

procedure TFMainForm.DemoEarlyUnitInitialization;
// can only be demoed by restarting the program with a command line parameter
begin
  Hide;
  WinExec(PAnsiChar(AnsiString(ParamStr(0)) + ' /EarlyUnitInitialization'), SW_SHOWNORMAL);
  Close;
end;

procedure TFMainForm.DemoVclUnitInitialization;
// can only be demoed by restarting the program with a command line parameter
begin
  Hide;
  WinExec(PAnsiChar(AnsiString(ParamStr(0)) + ' /VclUnitInitialization'), SW_SHOWNORMAL);
  Close;
end;

procedure TFMainForm.DemoProjectInitialization;
// can only be demoed by restarting the program with a command line parameter
begin
  Hide;
  WinExec(PAnsiChar(AnsiString(ParamStr(0)) + ' /ProjectInitialization'), SW_SHOWNORMAL);
  Close;
end;

procedure TFMainForm.DemoDuringRunningApplication;
// the easiest exception of all
begin
  raise Exception.Create('Demo "during running application".');
end;

procedure TFMainForm.DemoProjectFinalization;
// set the CrashParam value, then close the program down
begin
  SetCrashParam('/ProjectFinalization');
  Close;
end;

procedure TFMainForm.DemoVclUnitFinalization;
// set the CrashParam value, then close the program down
begin
  SetCrashParam('/VclUnitFinalization');
  Close;
end;

procedure TFMainForm.DemoLateUnitFinalization;
// set the CrashParam value, then close the program down
begin
  SetCrashParam('/LateUnitFinalization');
  Close;
end;

procedure TFMainForm.DemoDllInitialization;
// can only be demoed by restarting the program with a command line parameter
begin
  Hide;
  WinExec(PAnsiChar(AnsiString(ParamStr(0)) + ' /DllInitialization'), SW_SHOWNORMAL);
  Close;
end;

procedure DllExportedFunction; external 'ec.dll';

procedure TFMainForm.DemoDllExportedFunction;
// quite easy exception, just call the function which is exported by our dll
begin
  DllExportedFunction;
end;

procedure TFMainForm.DemoDllFinalization;
// set the CrashParam value, then close the program down
begin
  SetCrashParam('/DllFinalization');
  Close;
end;

function CreateThreadFunc(parameter: pointer) : integer; stdcall;
// looks easy, but hey, this is inside of a thread
begin
  raise Exception.Create('Demo "CreateThread".');
end;

procedure TFMainForm.DemoCreateThread;
// create a "CreateThread" thread, which will then raise an exception for us
var tid : dword;
begin
  CloseHandle(CreateThread(nil, 0, @CreateThreadFunc, nil, 0, tid));
end;

function BeginThreadFunc(parameter: pointer) : integer;
// looks easy again, but again, this is inside of a thread
begin
  raise Exception.Create('Demo "BeginThread".');
end;

procedure TFMainForm.DemoBeginThread;
// create a "BeginThread" thread, which will then raise an exception for us
var tid : dword;
begin
  CloseHandle(THandle(BeginThread(nil, 0, BeginThreadFunc, nil, 0, tid)));
end;

type
  // as easy as a TThread can be
  TSimpleThread = class (TThread)
    procedure Execute; override;
  end;

procedure TSimpleThread.Execute;
// sigh, not as easy as it looks like
begin
  FreeOnTerminate := true;
  raise Exception.Create('Demo "TThread".');
end;

procedure TFMainForm.DemoTThread;
// create a "TThread" based thread, which will then raise an exception for us
begin
  TSimpleThread.Create(false);
end;

procedure TFMainForm.DemoSysUtilsShowException;
// just test whether "SysUtils.ShowException" hooking works
begin
  try
    raise Exception.Create('Demo "SysUtils.ShowException".');
  except
    SysUtils.ShowException(ExceptObject, ExceptAddr);
  end;
end;

procedure TFMainForm.DemoApplicationShowException;
// does "Application.ShowException" hooking work?
begin
  try
    raise Exception.Create('Demo "Application.ShowException".');
  except
    Application.ShowException(Exception(ExceptObject));
  end;
end;

procedure TFMainForm.DemoApplicationHandleException;
// and how about "Application.HandleException"?
begin
  try
    raise Exception.Create('Demo "Application.HandleException".');
  except
    Application.HandleException(Exception(ExceptObject));
  end;
end;

procedure TFMainForm.FlooderTimer(var Message: TMessage);
// gets called by a timer every 500 ms to simulate exception flooding
begin
  raise Exception.Create('Demo "exception flooding".');
end;

procedure TFMainForm.DemoExceptionFlooding;
// start the exception flooding timer
begin
  SetTimer(Handle, 777, 500, nil);
end;

procedure TFMainForm.DemoInfiniteLoop;
// a simple infinite loop
begin
  // set madExcept's "AutoRestart" feature on just for this test
  MESettings.AutoRestart := 1;
  while true do ;
end;

type
  // now it gets a bit more complicated
  TDeadlockThread = class (TThread)
    FDeadlockSection : TRTLCriticalSection;  // will help in deadlocking
    procedure Execute; override;
    procedure Deadlock;  // this method will be synchronized
  end;

procedure TDeadlockThread.Execute;
// first enter the critical section, then pass control to the main thread
begin
  FreeOnTerminate := true;
  InitializeCriticalSection(FDeadlockSection);
  EnterCriticalSection(FDeadlockSection);
  Synchronize(Deadlock);
  LeaveCriticalSection(FDeadlockSection);
end;

procedure TDeadlockThread.Deadlock;
// this method gets executed in the context of the main thread
begin
  EnterCriticalSection(FDeadlockSection);
  // can't reach this, because the section is entered by 2 threads -> deadlock
  ShowMessage('Can''t reach this!');
  LeaveCriticalSection(FDeadlockSection);
end;

procedure TFMainForm.DemoDeadlock;
// start the deadlock thread
begin
  TDeadlockThread.Create(false);
end;

procedure TFMainForm.FormCreate(Sender: TObject);
// set up the edits and colors
begin
  Color := GetSysColor(COLOR_BTNFACE) - $181818;
  Explanation.DefAttributes.Color := clBlack;
  FillRichEdit(Explanation, string(AnsiString(CWelcomeExplanation)));
  FillRichEdit(SourceCode, string(AnsiString(CWelcomeSourceCode)));
end;

function TFMainForm.FindDownButton : TSpeedButton;
// which exception type was chosen?
var i1 : integer;
begin
  result := nil;
  for i1 := 0 to ComponentCount - 1 do
    if (Components[i1] is TSpeedButton) and TSpeedButton(Components[i1]).Down then begin
      result := TSpeedButton(Components[i1]);
      break;
    end;
end;

procedure TFMainForm.ChooseDemo(Sender: TObject);
// another exception type was chosen, update the edits
var btn : TSpeedButton;
begin
  btn := FindDownButton;
  if btn <> nil then begin
    TitleLabel.Caption := btn.Caption;
    Explanation.Text := string(AnsiString(CExplanations[btn.Tag]));
    FillRichEdit(SourceCode, string(AnsiString(CSourceCodes[btn.Tag])));
  end;
end;

procedure TFMainForm.ActionBtnClick(Sender: TObject);
// action was pressed, so let's do action!
var btn : TSpeedButton;
begin
  btn := FindDownButton;
  if btn <> nil then begin
    case btn.Tag of
       0 : DemoEarlyUnitInitialization;
       1 : DemoVclUnitInitialization;
       2 : DemoProjectInitialization;
       3 : DemoDuringRunningApplication;
       4 : DemoProjectFinalization;
       5 : DemoVclUnitFinalization;
       6 : DemoLateUnitFinalization;
       7 : DemoDllInitialization;
       8 : DemoDllExportedFunction;
       9 : DemoDllFinalization;
      10 : DemoCreateThread;
      11 : DemoBeginThread;
      12 : DemoTThread;
      13 : DemoSysUtilsShowException;
      14 : DemoApplicationShowException;
      15 : DemoApplicationHandleException;
      16 : DemoExceptionFlooding;
      17 : DemoInfiniteLoop;
      18 : DemoDeadlock;
    end;
  end else
    MessageBox(0, 'Please choose an action type in the left area.', 'Information...', MB_ICONINFORMATION);
end;

procedure TFMainForm.FormKeyPress(Sender: TObject; var Key: Char);
// escape key -> close
begin
  if Key = #27 then begin
    Key := #0;
    Close;
  end;
end;

// does the crash parameter say that our VCL unit should raise an exception?
initialization
  if CrashParam = '/VclUnitInitialization' then
    raise Exception.Create('Demo "VCL unit initialization".');
finalization
  if CrashParam = '/VclUnitFinalization' then
    raise Exception.Create('Demo "VCL unit finalization".');
end.
