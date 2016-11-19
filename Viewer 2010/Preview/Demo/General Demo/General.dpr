program General;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  untPrintPreview in '..\Viewer\untPrintPreview.pas' {MainForm};

{$R *.RES}

begin
  Application.Title := 'TPrintPreview Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
