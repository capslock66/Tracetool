unit unt_detailPopup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit, Vcl.ComCtrls, Vcl.ToolWin,
  Xml.xmldom, Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc,
  SynHighlighterXML, SynEditHighlighter, SynEditCodeFolding, SynHighlighterJSON,
  System.JSON, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;


type
  TDetailPopupForm = class(TForm)
    SynMemo: TSynEdit;
    SynJSONSyn: TSynJSONSyn;
    SynXMLSyn: TSynXMLSyn;
    XMLDocument: TXMLDocument;
    PanelTop: TPanel;
    ShowAsTextButton: TBitBtn;
    ShowAsXmlButton: TBitBtn;
    ShowAsJSonButton: TBitBtn;
    FormatButton: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ShowAsTextButtonClick(Sender: TObject);
    procedure ShowAsXmlButtonClick(Sender: TObject);
    procedure ShowAsJSonButtonClick(Sender: TObject);
    procedure FormatButtonClick(Sender: TObject);
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

procedure TDetailPopupForm.SetMemoText(text: string);
begin

   if ( text.StartsWith('<')) then
      SynMemo.Highlighter := SynXMLSyn
   else if ( text.StartsWith('{')) then
      SynMemo.Highlighter := SynJSONSyn
   else
      SynMemo.Highlighter := nil;
   SynMemo.Text := text;
end;

//------------------------------------------------------------------------------

procedure TDetailPopupForm.FormatButtonClick(Sender: TObject);
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

procedure TDetailPopupForm.ShowAsJSonButtonClick(Sender: TObject);
begin
   SynMemo.Highlighter := SynJSONSyn;
end;

//------------------------------------------------------------------------------

procedure TDetailPopupForm.ShowAsTextButtonClick(Sender: TObject);
begin
   SynMemo.Highlighter := nil;
end;

//------------------------------------------------------------------------------

procedure TDetailPopupForm.ShowAsXmlButtonClick(Sender: TObject);
begin
   SynMemo.Highlighter := SynXMLSyn;
end;

end.
