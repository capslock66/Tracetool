unit unt_FrameMemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit, Vcl.ComCtrls,
  Vcl.ToolWin, Xml.xmldom, Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc,
  SynHighlighterXML, SynEditHighlighter, SynEditCodeFolding, SynHighlighterJSON,
  System.JSON, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TFrameMemo = class(TFrame)
    SynMemo: TSynEdit;
    SynJSONSyn: TSynJSONSyn;
    SynXMLSyn: TSynXMLSyn;
    XMLDocument: TXMLDocument;
    PanelTop: TPanel;
    ShowAsTextButton: TBitBtn;
    ShowAsXmlButton: TBitBtn;
    ShowAsJSonButton: TBitBtn;
    FormatButton: TBitBtn;
    ShowPopupButton: TBitBtn;
    procedure ShowAsTextButtonClick(Sender: TObject);
    procedure ShowAsXmlButtonClick(Sender: TObject);
    procedure ShowAsJSonButtonClick(Sender: TObject);
    procedure FormatButtonClick(Sender: TObject);
    procedure ShowPopupButtonClick(Sender: TObject);
    procedure PanelTopResize(Sender: TObject);
  private
  public
    procedure SetMemoText(text: string; isXml, isJson: boolean);
  end;

implementation

uses unt_detailPopup;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TFrameMemo.PanelTopResize(Sender: TObject);
begin
   if width >= 290 then begin
      ShowAsTextButton.left :=   0 ; ShowAsTextButton.width := 45 ; ShowAsTextButton.caption := 'Text' ;
      ShowAsXmlButton .left :=  48 ; ShowAsXmlButton .width := 45 ; ShowAsXmlButton .caption := 'Xml' ;
      ShowAsJSonButton.left :=  99 ; ShowAsJSonButton.width := 45 ; ShowAsJSonButton.caption := 'Json' ;
      FormatButton    .left := 150 ; FormatButton    .width := 50 ; FormatButton    .caption := 'Format' ;
      ShowPopupButton .left := 203 ; ShowPopupButton .width := 85 ; ShowPopupButton .caption := 'Show in popup' ;
   end else if width >= 156 then begin
      ShowAsTextButton.left :=   0 ; ShowAsTextButton.width := 25 ; ShowAsTextButton.caption := 'Txt' ;
      ShowAsXmlButton .left :=  28 ; ShowAsXmlButton .width := 25 ; ShowAsXmlButton .caption := 'Xml' ;
      ShowAsJSonButton.left :=  56 ; ShowAsJSonButton.width := 28 ; ShowAsJSonButton.caption := 'Json' ;
      FormatButton    .left :=  87 ; FormatButton    .width := 25 ; FormatButton    .caption := 'Fmt' ;
      ShowPopupButton .left := 115 ; ShowPopupButton .width := 38 ; ShowPopupButton .caption := 'Popup' ;
   end else begin
      ShowAsTextButton.left :=   0 ; ShowAsTextButton.width := 14 ; ShowAsTextButton.caption := 'T' ;
      ShowAsXmlButton .left :=  15 ; ShowAsXmlButton .width := 14 ; ShowAsXmlButton .caption := 'X' ;
      ShowAsJSonButton.left :=  30 ; ShowAsJSonButton.width := 14 ; ShowAsJSonButton.caption := 'J' ;
      FormatButton    .left :=  45 ; FormatButton    .width := 19 ; FormatButton    .caption := 'F.' ;
      ShowPopupButton .left :=  65 ; ShowPopupButton .width := 19 ; ShowPopupButton .caption := 'P.' ;
   end;

end;

procedure TFrameMemo.SetMemoText(text: string; isXml:boolean; isJson : boolean);
begin
   SynMemo.Text := text;
   if (isXml) then
      SynMemo.Highlighter := SynXMLSyn
   else if (isJson) then
      SynMemo.Highlighter := SynJSONSyn
   else
      SynMemo.Highlighter := nil;
end;

//------------------------------------------------------------------------------

procedure TFrameMemo.ShowAsTextButtonClick(Sender: TObject);
begin
   SynMemo.Highlighter := nil;
end;

//------------------------------------------------------------------------------

procedure TFrameMemo.ShowAsXmlButtonClick(Sender: TObject);
begin
   SynMemo.Highlighter := SynXMLSyn;
end;

//------------------------------------------------------------------------------

procedure TFrameMemo.ShowAsJSonButtonClick(Sender: TObject);
begin
   SynMemo.Highlighter := SynJSONSyn;
end;

//------------------------------------------------------------------------------

procedure TFrameMemo.FormatButtonClick(Sender: TObject);
  var
     CurrentLine : string ;
     tmpJson: TJsonValue;
     tmpXml:string;

  Procedure FormatNode(SourceNode: IXMLNode; level : integer);
  Var
    I: Integer;
    NodeName : string ;
    indent : string ;
    AttribName : string ;
    AttribValue : OleVariant ;
  Begin
    indent := '' ;
    for i := 0 to level-1 do
       indent := indent + '   ';
    NodeName := SourceNode.NodeName ;

    if SourceNode.NodeType = ntText Then Begin
       SynMemo.Lines.Add(indent + trim(SourceNode.Text)) ;
    end else begin
       CurrentLine := indent + '<' + NodeName;
       // add attributes
       For I := 0 to SourceNode.AttributeNodes.Count - 1 do begin
          AttribName := SourceNode.AttributeNodes[I].NodeName ;
          AttribValue := SourceNode.AttributeNodes[I].NodeValue ;
          if AttribValue = null then
             AttribValue := '' ;
         CurrentLine := CurrentLine + ' ' + AttribName + '="' + AttribValue + '"' ;
         //NewNode.SetAttribute(SourceNode.AttributeNodes[I].NodeName, SourceNode.AttributeNodes[I].NodeValue);
       end ;

       if SourceNode.ChildNodes.Count = 0 then begin
          SynMemo.Lines.Add(CurrentLine + '/>');
       end else if (SourceNode.ChildNodes.Count = 1) and (SourceNode.ChildNodes[0].NodeType = ntText) then begin
          // single text sub node : add to the same line
          SynMemo.Lines.Add(CurrentLine + '>' + trim(SourceNode.ChildNodes[0].Text) + '</' + NodeName + '>') ;
       end else begin
          SynMemo.Lines.Add(CurrentLine + '>') ;
          For I := 0 to SourceNode.ChildNodes.Count - 1 do
            FormatNode(SourceNode.ChildNodes[I]{, NewNode},level+1);
          SynMemo.Lines.Add(indent +'</' + NodeName + '>') ;
       end ;
    end ;
  end;

begin
   if (trim(SynMemo.text).StartsWith('<')) then begin
      tmpXml := SynMemo.text;
      SynMemo.Highlighter := SynXMLSyn;
      XMLDocument.Active := False;
      XMLDocument.XML.Text := SynMemo.text;
      SynMemo.text := '';
      try
         XMLDocument.Active := True;
         FormatNode(XMLDocument.DocumentElement,0);    // , XMLDoc2.DocumentElement
      except
         on e : exception do begin
            SynMemo.text := tmpXml;
            Application.MessageBox (pchar('Invalid xml:' + e.Message),'Format xml', MB_OK);
         end ;
      end ;
   end else if (trim(SynMemo.text).StartsWith('{')) then begin
      SynMemo.Highlighter := SynJSONSyn;
      tmpJson := TJSONObject.ParseJSONValue(SynMemo.text);
      if tmpJson = nil then begin
         Application.MessageBox (pchar('Invalid json'),'Format Json', MB_OK);
      end else begin
         SynMemo.text := tmpJson.Format(3);
         FreeAndNil(tmpJson);
      end;
   end else
      SynMemo.Highlighter := nil;
end;

//------------------------------------------------------------------------------

procedure TFrameMemo.ShowPopupButtonClick(Sender: TObject);
var
   popup : TDetailPopupForm;
begin
   popup := TDetailPopupForm.create(Application);
   popup.SetMemoText(SynMemo.Text);
   popup.show();
end;

end.
