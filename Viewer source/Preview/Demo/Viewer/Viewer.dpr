program Viewer;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.RES}

begin
  Application.Title := 'Print Preview Viewer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
