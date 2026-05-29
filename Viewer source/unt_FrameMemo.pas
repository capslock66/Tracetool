unit unt_FrameMemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit, Vcl.ComCtrls,
  Vcl.ToolWin, Xml.xmldom, Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc,
  SynHighlighterXML, SynEditHighlighter, SynEditCodeFolding, SynHighlighterJSON,
  System.JSON, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Menus;

type
  TFrameMemo = class(TFrame)
    SynMemo: TSynEdit;
    SynJSONSyn: TSynJSONSyn;
    SynXMLSyn: TSynXMLSyn;
    XMLDocument: TXMLDocument;
    PanelTop: TPanel;
    FormatButton: TBitBtn;
    ShowPopupButton: TBitBtn;
    PopupShowAs: TPopupMenu;
    ShowAsText: TMenuItem;
    ShowAsXml: TMenuItem;
    ShowAsJson: TMenuItem;
    ShowAsButton: TButton;
    LabelSelect: TLabel;
    WordWrapButton: TSpeedButton;
    procedure FormatButtonClick(Sender: TObject);
    procedure ShowPopupButtonClick(Sender: TObject);
    procedure PanelTopResize(Sender: TObject);
    procedure ShowAsButtonClick(Sender: TObject);
    procedure ShowAsTextClick(Sender: TObject);
    procedure ShowAsXmlClick(Sender: TObject);
    procedure ShowAsJsonClick(Sender: TObject);
    procedure WordWrapButtonClick(Sender: TObject);
  private
  public
    procedure SetMemoText(text: string; isXml, isJson: boolean);
    //procedure ApplyTheme;
    procedure ApplySynMemoTheme ();
  end;

implementation

uses unt_detailPopup, unt_TraceWin, unt_TraceConfig;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TFrameMemo.PanelTopResize(Sender: TObject);
begin
   if width >= 290 then begin
      if (SynMemo.Highlighter = nil) then
         ShowAsButton.Caption := 'Text'
      else if (SynMemo.Highlighter = SynXMLSyn) then
         ShowAsButton.Caption := 'Xml'
      else if (SynMemo.Highlighter = SynJSONSyn) then
         ShowAsButton.Caption := 'JSon';
      ShowAsButton    .left :=   0; ShowAsButton.width := 50 ;                                                    //   0 + 50 + 6 = 56
      FormatButton    .left :=  56; FormatButton    .width := 50 ; FormatButton    .caption := 'Format' ;         //  56 + 50 + 6 = 112
      ShowPopupButton .left := 112; ShowPopupButton .width := 85 ; ShowPopupButton .caption := 'Show in popup' ;  // 112 + 85 + 6 = 203
      WordWrapButton  .Left := 203;                                                                               // 203 + 25 + 6 = 234
      LabelSelect     .Left := 234;

   end else if width >= 156 then begin
      if (SynMemo.Highlighter = nil) then
         ShowAsButton.Caption := 'Text'
      else if (SynMemo.Highlighter = SynXMLSyn) then
         ShowAsButton.Caption := 'Xml'
      else if (SynMemo.Highlighter = SynJSONSyn) then
         ShowAsButton.Caption := 'JSon';
      ShowAsButton    .left :=   0; ShowAsButton.width := 45 ;                                                    //   0 + 45 + 3 = 48
      FormatButton    .left :=  48; FormatButton    .width := 38 ; FormatButton    .caption := 'Format' ;         //  48 + 38 + 3 = 89
      ShowPopupButton .left :=  89; ShowPopupButton .width := 38 ; ShowPopupButton .caption := 'Popup' ;          //  89 + 38 + 3 = 130
      WordWrapButton  .Left := 130;                                                                               // 130 + 25 + 3 = 158
      LabelSelect     .Left := 158;

   end else begin
      if (SynMemo.Highlighter = nil) then
         ShowAsButton.Caption := 'T'
      else if (SynMemo.Highlighter = SynXMLSyn) then
         ShowAsButton.Caption := 'X'
      else if (SynMemo.Highlighter = SynJSONSyn) then
         ShowAsButton.Caption := 'J';
      ShowAsButton    .left :=   0; ShowAsButton.width := 30 ;                                                    //  0 + 30 + 1 = 31
      FormatButton    .left :=  31; FormatButton    .width := 19 ; FormatButton    .caption := 'F.' ;             // 31 + 19 + 1 = 51
      ShowPopupButton .left :=  51; ShowPopupButton .width := 19 ; ShowPopupButton .caption := 'P.' ;             // 51 + 19 + 1 = 71
      WordWrapButton  .Left :=  71;                                                                               // 71 + 25 + 1 = 97
      LabelSelect     .Left :=  97;
   end;

   if ShowPopupButton.Visible = false then
      WordWrapButton.Left := ShowPopupButton.Left;

end;

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

procedure TFrameMemo.ShowAsTextClick(Sender: TObject);
begin
   if width >= 290 then
      ShowAsButton.Caption := 'Text'
   else if width >= 156 then
      ShowAsButton.Caption := 'Txt'
   else
      ShowAsButton.Caption := 'T';
   SynMemo.Highlighter := nil;
end;

procedure TFrameMemo.ShowAsXmlClick(Sender: TObject);
begin
   if width >= 290 then
      ShowAsButton.Caption := 'Xml'
   else if width >= 156 then
      ShowAsButton.Caption := 'Xml'
   else
      ShowAsButton.Caption := 'X';
   SynMemo.Highlighter := SynXMLSyn;
end;

procedure TFrameMemo.ShowAsJsonClick(Sender: TObject);
begin
   if width >= 290 then
      ShowAsButton.Caption := 'JSon'
   else if width >= 156 then
      ShowAsButton.Caption := 'Jsn'
   else
      ShowAsButton.Caption := 'J';
   SynMemo.Highlighter := SynJSONSyn;
end;

procedure TFrameMemo.ShowAsButtonClick(Sender: TObject);
begin
    FormatButtonClick(sender);
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

//------------------------------------------------------------------------------

procedure TFrameMemo.WordWrapButtonClick(Sender: TObject);
begin
   SynMemo.WordWrap := WordWrapButton.Down;
end;

procedure TFrameMemo.ApplySynMemoTheme();
begin
   if TraceConfig.Dark_Enabled then
   begin
      SynMemo.Color                        := TColor($001E1E1E);  // dark background
      SynMemo.Font.Color                   := TColor($00D4D4D4);  // light gray text
      SynMemo.Gutter.Color                 := TColor($00252526);  // slightly lighter than editor bg
      SynMemo.Gutter.Font.Color            := TColor($00858585);  // muted gray line numbers
      SynMemo.ActiveLineColor              := TColor($00282828);  // subtle current-line highlight
      SynMemo.SelectedColor.Background     := TraceConfig.Dark_SelectedBackgroundColor;
      SynMemo.SelectedColor.Foreground     := TraceConfig.Dark_SelectedTextColor;
   end else begin
      SynMemo.Color                        := clWindow;
      SynMemo.Font.Color                   := clWindowText;
      SynMemo.Gutter.Color                 := TColor($00F0F0F0);
      SynMemo.Gutter.Font.Color            := clGrayText;
      SynMemo.ActiveLineColor              := clNone;
      SynMemo.SelectedColor.Background     := clHighlight;
      SynMemo.SelectedColor.Foreground     := clHighlightText;
   end;
end;

//------------------------------------------------------------------------------


end.
