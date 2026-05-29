unit unt_detailPopup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit, Vcl.ComCtrls, Vcl.ToolWin,
  Xml.xmldom, Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc,
  SynHighlighterXML, SynEditHighlighter, SynEditCodeFolding, SynHighlighterJSON,
  System.JSON, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, unt_FrameMemo,
  System.Generics.Collections;


type
  TDetailPopupForm = class(TForm)
    FrameMemo: TFrameMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    class var Instances: TList<TDetailPopupForm>;
    procedure SetMemoText(text : string);
  end;

var
  DetailPopupForm: TDetailPopupForm;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TDetailPopupForm.FormCreate(Sender: TObject);
begin
   TDetailPopupForm.Instances.Add(Self);
   frameMemo.WordWrapButton.Left := frameMemo.ShowPopupButton.left;
   frameMemo.ShowPopupButton.visible := false;
   frameMemo.LabelSelect.visible := false;
   frameMemo.ApplySynMemoTheme();
end;

//------------------------------------------------------------------------------

procedure TDetailPopupForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   TDetailPopupForm.Instances.Remove(Self);
   Action := caFree;
end;

//------------------------------------------------------------------------------

procedure TDetailPopupForm.SetMemoText(text: string);
var
   isXml,isJson : boolean;
begin
   isXml := false;
   isJson := false;
   if ( text.StartsWith('<')) then
      isXml := true
   else if ( text.StartsWith('{')) then
      isJson := true;
   frameMemo.SetMemoText(text,isXml,isJson);
end;


initialization
   TDetailPopupForm.Instances := TList<TDetailPopupForm>.Create;

finalization
   TDetailPopupForm.Instances.Free;
   TDetailPopupForm.Instances := nil;

end.
