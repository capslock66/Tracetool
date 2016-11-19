program DelphiDemo2009;

uses
  //fastmm4,
  //madExcept,
  //madLinkDisAsm,
  //madListHardware,
  //madListProcesses,
  //madListModules,
  Forms,
  unitTest1 in 'unitTest1.pas' {form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(Tform1, form1);
  Application.Run;
end.
