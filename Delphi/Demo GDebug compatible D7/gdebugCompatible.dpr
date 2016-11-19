program gdebugCompatible;

uses
  Forms,
  UnitTest2 in 'UnitTest2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
