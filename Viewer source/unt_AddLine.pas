unit unt_AddLine;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TFrm_AddLine = class(TForm)
    butAddLine: TButton;
    butCancel: TButton;
    InsertWhere: TRadioGroup;
    GroupBoxTextToAdd: TGroupBox;
    PanelTime: TPanel;
    LabelTime: TLabel;
    editTime: TEdit;
    PanelThId: TPanel;
    LabelThId: TLabel;
    EditThId: TEdit;
    PanelLines: TPanel;
    PanelComment: TPanel;
    LabelTraces: TLabel;
    EditTrace: TEdit;
    LabelComment: TLabel;
    EditComment: TEdit;
    PanelButtons: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetTraceWinMode;
    procedure SetTailMode;
    procedure SetOdsMode;
    procedure SetEventLogMode;
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
   // PanelTime, LabelTime, editTime,
   // PanelThId, LabelThId, EditThId,
   // PanelLines, LabelTraces, EditTrace,
   // PanelComment, LabelComment, EditComment

   // time, thId, Traces, comment
   PanelTime.Visible := true;
   PanelThId.Visible := true;
   LabelThId.Caption := 'Thread Id';
   PanelLines.Visible := true;
   LabelTraces.Caption := 'Trace';
   PanelComment.Visible := true;
end;

procedure TFrm_AddLine.SetTailMode;
begin
   // PanelTime, LabelTime, editTime,
   // PanelThId, LabelThId, EditThId,
   // PanelLines, LabelTraces, EditTrace,
   // PanelComment, LabelComment, EditComment

   // time, Lines
   PanelTime.Visible := true;
   PanelThId.Visible := false;
   PanelLines.Visible := true;
   LabelTraces.Caption := 'Line';
   PanelComment.Visible := false;
end;

procedure TFrm_AddLine.SetOdsMode;
begin
   // PanelTime, LabelTime, editTime,
   // PanelThId, LabelThId, EditThId,
   // PanelLines, LabelTraces, EditTrace,
   // PanelComment, LabelComment, EditComment

   // time, Process, Lines
   PanelTime.Visible := true;
   PanelThId.Visible := true;  // Process
   LabelThId.Caption := 'Process name';
   PanelLines.Visible := true;
   LabelTraces.Caption := 'Line';
   PanelComment.Visible := false;
end;

procedure TFrm_AddLine.SetEventLogMode;
begin
   // PanelTime, LabelTime, editTime,
   // PanelThId, LabelThId, EditThId,
   // PanelLines, LabelTraces, EditTrace,
   // PanelComment, LabelComment, EditComment

   // Time, Source, Lines
   PanelTime.Visible := true;
   PanelThId.Visible := true;  // Source
   LabelThId.Caption := 'Source';
   PanelLines.Visible := true;
   LabelTraces.Caption := 'Line';
   PanelComment.Visible := false;
end;



end.
