program ExcCatch;

// our project doesn't do much, just the uses clause and 2 exception types

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  ecMini in 'ecMini.pas',
  ecMain in 'ecMain.pas' {FMainForm},
  ecStrings in 'ecStrings.pas',
  ecCrashParam in 'ecCrashParam.pas',
  Forms,
  SysUtils;

{$R madExcept.res}

begin
  // shall our project raise an exception during initialization?
  if CrashParam = '/ProjectInitialization' then
    raise Exception.Create('Demo "project initialization".');
  // initialize and start our main form
  Application.Initialize;
  Application.CreateForm(TFMainForm, FMainForm);
  Application.Run;
  // shall our project raise an exception during finalization?
  if CrashParam = '/ProjectFinalization' then
    raise Exception.Create('Demo "project finalization".');
end.
