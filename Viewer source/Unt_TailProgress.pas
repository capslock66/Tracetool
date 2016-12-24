{

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information  

}

unit Unt_TailProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TFrmTailProgress = class(TForm)
    ProgressBar: TProgressBar;
    butCancel: TButton;
    LabelCaptionLinesRead: TLabel;
    LabelLinesRead: TLabel;
    procedure butCancelClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure CreateParams(var Params : TCreateParams) ; override ;
  public
    { Public declarations }
    IsStoped : boolean ;
  end;

var
  FrmTailProgress: TFrmTailProgress;

implementation

{$R *.dfm}

procedure TFrmTailProgress.butCancelClick(Sender: TObject);
begin
   IsStoped := true ;
end;

procedure TFrmTailProgress.CreateParams(var Params: TCreateParams);
begin
   inherited ;
   //Params.style := Params.style or WS_CAPTION ;
   //Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW ;
   Params.WndParent := 0 ;   // window is independant of the main window (for example minimize or show)
end;

end.
