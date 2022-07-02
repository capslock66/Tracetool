unit unt_AddLine;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TFrm_AddLine = class(TForm)
    butAddLine: TButton;
    butCancel: TButton;
    PanelUnderline: TPanel;
    InsertWhere: TRadioGroup;
    GroupBoxTextToAdd: TGroupBox;
    LabelTime: TLabel;
    editTime: TEdit;
    LabelThId: TLabel;
    EditThId: TEdit;
    LabelTraces: TLabel;
    EditTrace: TEdit;
    LabelComment: TLabel;
    EditComment: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetTraceWinMode;
  end;

var
  Frm_AddLine: TFrm_AddLine;

implementation

{$R *.dfm}

procedure TFrm_AddLine.FormCreate(Sender: TObject);
begin
   InsertWhere.ItemIndex := 1 ; // Before selected line
end;

procedure TFrm_AddLine.FormShow(Sender: TObject);
begin
   EditTrace.SetFocus;
end;

procedure TFrm_AddLine.SetTraceWinMode;
begin
   // show or hide , update labels
end;

end.
