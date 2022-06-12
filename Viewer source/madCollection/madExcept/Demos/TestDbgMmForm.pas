unit TestDbgMmForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TTestDebugMemForm = class(TForm)
    OverrunButton: TButton;
    UnderrunButton: TButton;
    AccessFreedMemButton: TButton;
    procedure OverrunButtonClick(Sender: TObject);
    procedure UnderrunButtonClick(Sender: TObject);
    procedure AccessFreedMemButtonClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  TestDebugMemForm: TTestDebugMemForm;

implementation

{$R *.dfm}

procedure TTestDebugMemForm.OverrunButtonClick(Sender: TObject);
var pc1 : PAnsiChar;
begin
  pc1 := AllocMem(4);
  pc1[0] := '1';
  pc1[1] := '2';
  pc1[2] := '3';
  pc1[3] := '4';
  strlen(pc1);  // <- buffer overrun (read access)
  FreeMem(pc1);
end;

procedure TTestDebugMemForm.UnderrunButtonClick(Sender: TObject);
var pc1 : PAnsiChar;
    i1  : integer;
begin
  pc1 := AllocMem(4);
  i1 := -1;
  pc1[0] := pc1[i1];  // <- buffer underrun (read access)
  FreeMem(pc1);
end;

procedure TTestDebugMemForm.AccessFreedMemButtonClick(Sender: TObject);
var pc1 : PAnsiChar;
begin
  pc1 := AllocMem(4);
  pc1[0] := #0;
  strlen(pc1);
  FreeMem(pc1);
  strlen(pc1);  // <- accessing freed memory (read access)
end;

end.
