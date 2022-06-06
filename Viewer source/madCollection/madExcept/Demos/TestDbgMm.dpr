program TestDbgMm;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  TestDbgMmForm in 'TestDbgMmForm.pas' {TestDebugMemForm};

{$R madExcept.res}

begin
  Application.Initialize;
  Application.CreateForm(TTestDebugMemForm, TestDebugMemForm);
  Application.Run;
end.
