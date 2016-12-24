program RichPrint;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.RES}

begin
  Application.Title := 'TPrintPreview Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
