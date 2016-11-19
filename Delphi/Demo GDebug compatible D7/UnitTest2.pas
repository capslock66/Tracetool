unit UnitTest2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TypInfo, ExtCtrls, CheckLst, Grids, comobj , activex, Contnrs,
  ComCtrls, Menus,
  dbugintf ; 

type
  TForm2 = class(TForm)
    butTest: TButton;
    procedure butTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.butTestClick(Sender: TObject);
begin
   senddebug ('hello world') ;
   SendDebugEx('warning',mtWarning);
   SendDebugEx('Error',mtError);
   SendDebugEx('Information',mtInformation);
end;

end.
