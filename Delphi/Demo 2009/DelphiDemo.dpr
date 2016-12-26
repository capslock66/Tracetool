program DelphiDemo2009;

uses
  Forms,
  unitTest1 in 'unitTest1.pas' {form1},
  TraceTool in '..\Delphi Library\TraceTool.pas',
  SynCommons in '..\Delphi Library\SynCommons.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(Tform1, form1);
  Application.Run;
end.
