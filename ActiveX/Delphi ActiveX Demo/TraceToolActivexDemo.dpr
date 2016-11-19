program TraceToolActivexDemo;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  Unit1 in 'Unit1.pas' {Form4},
  TraceToolCom_TLB in '..\Lib\TraceToolCom_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
