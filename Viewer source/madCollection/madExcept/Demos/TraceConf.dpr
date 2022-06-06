program TraceConf;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  ConfForm in 'ConfForm.pas' {FStackTraceConfuser};

{$R madExcept.res}

begin
  Application.Initialize;
  Application.CreateForm(TFStackTraceConfuser, FStackTraceConfuser);
  Application.Run;
end.
