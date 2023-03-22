unit unt_detailPopup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit, Vcl.ComCtrls, Vcl.ToolWin,
  Xml.xmldom, Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc,
  SynHighlighterXML, SynEditHighlighter, SynEditCodeFolding, SynHighlighterJSON,
  System.JSON, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, unt_FrameMemo;


type
  TDetailPopupForm = class(TForm)
    FrameMemo: TFrameMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetMemoText(text : string);
  end;

var
  DetailPopupForm: TDetailPopupForm;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TDetailPopupForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

//------------------------------------------------------------------------------

procedure TDetailPopupForm.FormCreate(Sender: TObject);
begin
   frameMemo.ShowPopupButton.visible := false;
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


end.
