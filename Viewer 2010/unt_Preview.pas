unit unt_Preview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, NicePreview, ComCtrls, Series, TeEngine,
  TeeProcs, Chart;

type
  TFrmNicePreview = class(TForm)
    NicePreview1: TNicePreview;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    StatusBar1: TStatusBar;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button10: TButton;
    butRenderGrids: TButton;
    procedure NicePreview1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure butRenderGridsClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure RenderTexts;
    procedure RenderGrids;
  end;

var
  FrmNicePreview: TFrmNicePreview;

implementation

{$R *.dfm}

procedure TFrmNicePreview.NicePreview1Change(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'Page ' + IntToStr(NicePreview1.PageIndex + 1) +
    ' of ' + IntToStr(NicePreview1.PageCount);
  StatusBar1.Panels[1].Text := 'Magnification ' + IntToStr(Round(NicePreview1.Magnification * 100)) + '%';
end;

procedure TFrmNicePreview.Button1Click(Sender: TObject);
begin
  NicePreview1.PreviewMode := pmNormal;
end;

procedure TFrmNicePreview.Button2Click(Sender: TObject);
begin
  NicePreview1.PreviewMode := pmZoomIn;
end;

procedure TFrmNicePreview.Button3Click(Sender: TObject);
begin
  NicePreview1.PreviewMode := pmZoomOut;
end;

procedure TFrmNicePreview.Button10Click(Sender: TObject);
begin
  NicePreview1.PreviewMode := pmDrag;
end;

procedure TFrmNicePreview.Button8Click(Sender: TObject);
begin
  NicePreview1.ViewActualSize;
end;

procedure TFrmNicePreview.Button4Click(Sender: TObject);
begin
  NicePreview1.ViewFitToWidth;
end;

procedure TFrmNicePreview.Button5Click(Sender: TObject);
begin
  NicePreview1.ViewWholePage;
end;

procedure TFrmNicePreview.Button6Click(Sender: TObject);
begin
  NicePreview1.ViewTwoPage;
end;

procedure TFrmNicePreview.Button7Click(Sender: TObject);
begin
  NicePreview1.ViewFourPage;
end;

procedure TFrmNicePreview.FormCreate(Sender: TObject);
begin
   RenderTexts ;
end;

procedure TFrmNicePreview.RenderTexts;
var
  ACanvas: TCanvas;
  h: Integer;
  th: Integer;
  i, j: Integer;
  t: TStringList;

begin

  NicePreview1.Clear;

  t := TStringList.Create;
  try
    t.LoadFromFile('NicePreview\License.txt');
  except
    ShowMessage('File ..\License.txt doesn''t exist. Unable to render preview.'#13 +
      'If you run this file from ZIP extractor program, extract all files first and rerun.');
  end;

  if (t.Count = 0) then
  begin
    t.Free;
    Exit;
  end;

  with NicePreview1 do
  begin
    th := PageHeight - MarginTop - MarginBottom;
  end;

  i := 0;
  j := -1;
  h := 0;
  ACanvas := nil;

  repeat

    if (j < h) then
    begin
      if (ACanvas <> nil)
        then NicePreview1.EndPage;
      ACanvas := NicePreview1.BeginPage;
      with ACanvas do
      begin
        Font.Name := 'Lucida Console';
        Font.Size := 8;
        h := TextHeight('Ag');
      end;
      j := th;
    end;

    ACanvas.TextOut(NicePreview1.MarginLeft, NicePreview1.MarginTop + (th - j), t[i]);

    Inc(i);
    Dec(j, h);

  until (i >= t.Count);

  NicePreview1.EndPage;

  t.Free;

end;


procedure TFrmNicePreview.RenderGrids;
const
  Str1 = 'This is a grid test.';
  Str2 = 'All like drawing on ordinary screen canvas.';
var
  x, y, w, h: Integer;
  R: TRect;
  ax, ay: Integer;
  DeltaX: Integer;
  c : integer ;
  PreviewCanvas : TCanvas ;
begin

  NicePreview1.Clear;

  for c := 0 to 2 do begin
    PreviewCanvas := NicePreview1.BeginPage ;

    PreviewCanvas.Font.Name := 'Arial';
    PreviewCanvas.Font.Size := 24;
    PreviewCanvas.Font.Style := [fsBold];
    PreviewCanvas.Font.Color := clBlack;
    w := PreviewCanvas.TextWidth(Str1);
    h := PreviewCanvas.TextHeight(Str1);
    x := (NicePreview1.PageWidth - w) div 2;
    y := NicePreview1.MarginTop;
    PreviewCanvas.TextOut(x, y, Str1);
    y := y + h + 5;

    PreviewCanvas.Font.Name := 'Courier New';
    PreviewCanvas.Font.Size := 16;
    PreviewCanvas.Font.Style := [];
    PreviewCanvas.Font.Color := clRed;
    w := PreviewCanvas.TextWidth(Str2);
    h := PreviewCanvas.TextHeight(Str2);
    x := (NicePreview1.PageWidth - w) div 2;
    PreviewCanvas.TextOut(x, y, Str2);
    y := y + h + 30;

    PreviewCanvas.Font.Name := 'Arial';
    PreviewCanvas.Font.Size := 8;
    PreviewCanvas.Font.Style := [];
    PreviewCanvas.Font.Color := clBlack;
    w := (NicePreview1.PageWidth - NicePreview1.MarginLeft - nicePreview1.MarginRight) div 10;
    h := PreviewCanvas.TextHeight('Ag') + 3;
    DeltaX := (NicePreview1.PageWidth - (w * 10)) div 2;
    PreviewCanvas.Brush.Style := bsClear;

    for ax := 0 to 9 do
    begin
      for ay := 0 to 29 do
      begin
        R := Rect(ax * w, ay * h, ((ax + 1) * w) + 1, ((ay + 1) * h) + 1);
        OffsetRect(R, DeltaX, y);
        PreviewCanvas.Brush.Style := bsSolid;
        PreviewCanvas.Rectangle(R);
        PreviewCanvas.Brush.Style := bsClear;
        PreviewCanvas.TextRect(R, R.Left + 5, R.Top + 3, IntToStr(ax) + ',' + IntToStr(ay));
      end;
    end;  
    NicePreview1.EndPage;
  end ;


end;

procedure TFrmNicePreview.Button9Click(Sender: TObject);
begin
  NicePreview1.PrintAll;
end;

procedure TFrmNicePreview.butRenderGridsClick(Sender: TObject);
begin
RenderGrids
end;

end.
