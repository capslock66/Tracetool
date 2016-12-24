unit unt_Details_xml;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, unt_Details_base, unt_TraceWin, ExtCtrls, StdCtrls, unt_tool,
  xmldom, XMLIntf, msxmldom, XMLDoc, SynEditHighlighter, SynHighlighterXML,
  SynEdit, SynMemo;


type
  TFrame_XML = class(Tframe_BaseDetails)
    SynMemo: TSynMemo;
    SynXMLSyn1: TSynXMLSyn;
    XMLDoc: TXMLDocument;
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure AddDetails(TreeRec: PTreeRec; RootMember : TMember); override;
    function HasFocus : boolean ; override;
    procedure SelectAll() ; override;
    procedure copySelected() ; override;
  end;

var
  Frame_XML: TFrame_XML;

implementation

{$R *.dfm}

{ TFrame_XML }

procedure TFrame_XML.AddDetails(TreeRec: PTreeRec; RootMember: TMember);
  var
     //target : TStringList ;
     CurrentLine : string ;

  Procedure AddNode(SourceNode: IXMLNode{; DestNode: IXMLNode}; level : integer);
  Var
    //NewNode: IXMLNode;
    I: Integer;
    NodeName : string ;
    indent : string ;
    AttribName : string ;
    AttribValue : OleVariant ;
  Begin

    indent := '' ;
    for i := 0 to level-1 do
       indent := indent + '   ' ;

    NodeName := SourceNode.NodeName ;

    if SourceNode.NodeType = ntText Then Begin
      //If DestNode <> nil Then
      //  DestNode.Text := SourceNode.Text ;

      SynMemo.Lines.Add(indent + SourceNode.Text) ;
    end else begin
       //If DestNode = nil Then
       //  NewNode := XMLDoc2.AddChild(NodeName)
       //Else
       //  NewNode := DestNode.AddChild(NodeName);

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
          SynMemo.Lines.Add(CurrentLine + '/>') ;

       end else if (SourceNode.ChildNodes.Count = 1) and (SourceNode.ChildNodes[0].NodeType = ntText) then begin
          // single text sub node : add to the same line
          SynMemo.Lines.Add(CurrentLine + '>' + SourceNode.ChildNodes[0].Text + '</' + NodeName + '>') ;

       end else begin
          SynMemo.Lines.Add(CurrentLine + '>') ;
          For I := 0 to SourceNode.ChildNodes.Count - 1 do
            AddNode(SourceNode.ChildNodes[I]{, NewNode},level+1);
          SynMemo.Lines.Add(indent +'</' + NodeName + '>') ;
       end ;

    end ;

  End;
begin
   inherited;

   TFrm_Trace(Owner).CurrentViewers.add(self) ;

   //TFrm_Trace(Owner).XmlVisible := true ;   // viewer will be visible
   //inc (TFrm_Trace(Owner).ViewerCount) ;    // need to know the number of viewer to display

   SynMemo.Lines.Clear ;
   XMLDoc.Active   := False;
   XMLDoc.XML.Text := RootMember.col1;
   try
      XMLDoc.Active   := True;
      AddNode(XMLDoc.DocumentElement{, XMLDoc2.DocumentElement},0);
   except
      on e : exception do
         SynMemo.Lines.Add(e.Message) ;
   end ;

end;

//------------------------------------------------------------------------------

procedure TFrame_XML.copySelected;
begin
  SynMemo.CopyToClipboard ;
end;

//------------------------------------------------------------------------------

function TFrame_XML.HasFocus: boolean;
begin
  result := Focused or SynMemo.Focused ; 
end;

//------------------------------------------------------------------------------

// CTRL-A : select all
procedure TFrame_XML.SelectAll;
begin
   SynMemo.SetFocus ;
   SynMemo.SelStart := 0;
   SynMemo.SelLength := Length(SynMemo.lines.Text);
end;

end.
 