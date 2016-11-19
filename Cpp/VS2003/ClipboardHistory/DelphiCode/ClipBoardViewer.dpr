program ClipBoardViewer;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  MiscTools in 'MiscTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
