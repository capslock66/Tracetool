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
  private
  public
    procedure SetMemoText(text: string; isXml, isJson: boolean);
  end;

implementation

uses unt_detailPopup;

{$R *.dfm}

//------------------------------------------------------------------------------

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
