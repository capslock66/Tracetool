program LeakTest;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  LeakTestForm in 'LeakTestForm.pas' {TestLeakForm};

{$R madExcept.res}

begin
  Application.Initialize;
  Application.CreateForm(TTestLeakForm, TestLeakForm);
  Application.Run;
end.
